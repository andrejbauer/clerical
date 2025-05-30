open Util
module Syntax = Typing.Syntax
module Type = Typing.Type
module Dyadic = Reals.Dyadic
module Real = Reals.Real

(** Make the stack read-only by pushing a new empty top frame onto it, and
    converting the read-write entries to read-only entries. *)
let make_ro Run.{ frame; frames; funs } =
  Run.{ frame = []; frames = frame :: frames; funs }

(** Push a read-write value onto the top frame. *)
let push_rw x v st = Run.{ st with frame = (x, RW (ref v)) :: st.frame }

(** Push a read-only value onto the top frame. *)
let push_ro x v st = Run.{ st with frame = (x, RO v) :: st.frame }

(** Define a new function. *)
let push_fun f stack = Run.{ stack with funs = f :: stack.funs }

(** Push many read-write values *)
let push_rws xvs st = List.fold_left (fun st (x, v) -> push_rw x v st) st xvs

(** Push many read-only values *)
let push_ros xvs st = List.fold_left (fun st (x, v) -> push_ro x v st) st xvs

(** Push many read-only values *)
let push_ros' xs vs st = List.fold_left2 (fun st x v -> push_ro x v st) st xs vs

(** Pop values from stack *)
let pop stack k =
  let rec remove k lst =
    match (k, lst) with
    | 0, lst -> lst
    | k, _ :: lst -> remove (k - 1) lst
    | _, [] -> Run.error ~loc:Location.nowhere (Run.InternalError "pop")
  in
  Run.{ stack with frame = remove k stack.frame }

(** Lookup a value on the stack *)
let lookup_val k Run.{ frame; frames; _ } =
  let rec lookup k vs vss =
    match (k, vs, vss) with
    | 0, (_, Run.RO v) :: _, _ -> Some v
    | 0, (_, Run.RW r) :: _, _ -> Some !r
    | k, [], vs :: vss -> lookup k vs vss
    | k, [], [] -> None
    | k, _ :: vs, vss -> lookup (k - 1) vs vss
  in
  lookup k frame frames

(** Lookup a reference to a read-write value on the stack *)
let lookup_ref k Run.{ frame; _ } =
  let rec lookup k vs =
    match (k, vs) with
    | 0, (_, Run.RO v) :: _ -> None
    | 0, (_, Run.RW r) :: _ -> Some r
    | k, [] -> None
    | k, _ :: vs -> lookup (k - 1) vs
  in
  lookup k frame

(** Lookup a function definition *)
let lookup_fun k Run.{ funs; _ } =
  let rec lookup k fs =
    match (k, fs) with
    | _, [] -> None
    | 0, f :: _ -> Some f
    | k, _ :: fs -> lookup (k - 1) fs
  in
  lookup k funs

(** Print trace *)
let print_trace ~loc ~prec Run.{ frame; frames; _ } =
  let xvs = frame @ List.flatten frames in
  Print.message ~loc "Trace" "\tprecision: %t@\n\t%t@." (Run.print_prec prec)
    (fun ppf ->
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "@\n\t")
        (fun ppf (x, entry) ->
          let v = match entry with Run.RO v -> v | Run.RW r -> !r in
          Format.fprintf ppf "%s:\t%t" x (Value.print_value v))
        ppf xvs)

(** Extraction of values with expected type *)

let as_integer ~loc v =
  match Value.ro_as_integer v with
  | None -> Run.error ~loc Run.IntegerExpected
  | Some k -> k

let as_boolean ~loc v =
  match Value.ro_as_boolean v with
  | None -> Run.error ~loc Run.BooleanExpected
  | Some b -> b

let as_real ~loc v =
  match Value.ro_as_real v with
  | None -> Run.error ~loc Run.RealExpected
  | Some r -> r

let as_unit ~loc (Value.RW r) =
  match r with
  | Value.VUnit -> ()
  | Value.(VInteger _ | VBoolean _ | VReal _) -> Run.error ~loc Run.UnitExpected

let as_value ~loc v = Value.ro_as_value v

(** [comp ~prec n stack c] evaluates computation [c] in the given [stack] at
    precision level [n], and returns the new stack and the computed value. *)
let rec comp ~prec stack { Location.data = c; Location.loc } :
    Run.stack * Value.result =
  if !Config.trace then print_trace ~loc ~prec stack;
  let { Run.prec_mpfr; _ } = prec in
  match c with
  | Syntax.Var k -> (
      match lookup_val k stack with
      | None -> Run.error ~loc Run.OutOfStack
      | Some v -> (stack, Value.return v))
  | Syntax.Boolean b -> (stack, Value.(return (VBoolean b)))
  | Syntax.Integer k -> (stack, Value.(return (VInteger k)))
  | Syntax.Float s ->
      let rl = Dyadic.of_string ~prec:prec_mpfr ~round:Dyadic.down s in
      let ru = Dyadic.of_string ~prec:prec_mpfr ~round:Dyadic.up s in
      let r = Real.make rl ru in
      (stack, Value.(return (VReal r)))
  | Syntax.Apply (k, es) -> (
      match lookup_fun k stack with
      | None -> Run.error ~loc Run.InvalidFunction
      | Some f ->
          let vs = List.map (fun e -> comp_ro_value ~prec stack e) es in
          let p = f ~loc ~prec vs in
          let r = Value.ro_as_rw p in
          (stack, r))
  | Syntax.Skip -> (stack, Value.(return VUnit))
  | Syntax.Trace ->
      print_trace ~loc ~prec stack;
      (stack, Value.(return VUnit))
  | Syntax.Sequence (c1, c2) ->
      let stack, r1 = comp ~prec stack c1 in
      let () = as_unit ~loc:c1.Location.loc r1 in
      comp ~prec stack c2
  | Syntax.If (b, c1, c2) -> (
      match comp_ro_boolean ~loc ~prec stack b with
      | true -> comp ~prec stack c1
      | false -> comp ~prec stack c2)
  | Syntax.Case cases -> comp_case ~loc ~prec stack cases
  | Syntax.While (b, c) ->
      let granularity = 1000 in
      let rec loop k stack =
        if k = 0 then Parallel.yield ();
        match comp_ro_boolean ~loc:b.Location.loc ~prec stack b with
        | false -> (stack, Value.(return VUnit))
        | true ->
            let stack, v = comp ~prec stack c in
            let () = as_unit ~loc:c.Location.loc v in
            loop ((k + 1) mod granularity) stack
      in
      loop 1 stack
  | Syntax.Newvar (xes, c) ->
      let xvs = List.map (fun (x, e) -> (x, comp_ro_value ~prec stack e)) xes in
      let stack' = push_rws xvs stack in
      let _, v = comp ~prec stack' c in
      (stack, v)
  | Syntax.Let (xes, c) ->
      let xvs = List.map (fun (x, e) -> (x, comp_ro_value ~prec stack e)) xes in
      let stack' = push_ros xvs stack in
      let _, v = comp ~prec stack' c in
      (stack, v)
  | Syntax.PNewvar (xes, c) ->
      let xvs =
        Parallel.map (fun (x, e) -> (x, comp_ro_value ~prec stack e)) xes
      in
      let stack' = push_rws xvs stack in
      let _, v = comp ~prec stack' c in
      (stack, v)
  | Syntax.PLet (xes, c) ->
      let xvs =
        Parallel.map (fun (x, e) -> (x, comp_ro_value ~prec stack e)) xes
      in
      let stack' = push_ros xvs stack in
      let _, v = comp ~prec stack' c in
      (stack, v)
  | Syntax.Assign (k, e) -> (
      match lookup_ref k stack with
      | None -> Run.error ~loc Run.CannotWrite
      | Some r ->
          let v = comp_ro_value ~prec stack e in
          r := v;
          (stack, Value.(return VUnit)))
  | Syntax.Lim (x, e) -> (
      (* when computing at precision n we first try to compute the n-th term
        of the limit, and use that as the approximate result. If the computation
        fails we fall back to computing successively the 1st, 2nd, ... term of
        the limit, and take the last one that doesn't fail.
     *)
      let try_lim n =
        let stack' = push_ro x (Value.VInteger (Mpzf.of_int n)) stack in
        let r = comp_ro_real ~loc:e.Location.loc ~prec stack' e in
        let err =
          Dyadic.shift ~prec:prec_mpfr ~round:Dyadic.up Dyadic.one (-n)
        in
        let rl =
          Dyadic.sub ~prec:prec_mpfr ~round:Dyadic.down (Real.lower r) err
        and ru =
          Dyadic.add ~prec:prec_mpfr ~round:Dyadic.up (Real.upper r) err
        in
        let r = Real.make rl ru in
        (stack, Value.(return (VReal r)))
      in
      try
        (* If we succeed with current precision then we return *)
        try_lim prec_mpfr
      with
      (* If current precision fails, then successively try n = 1, 2, 4, ... up to mpfr_prec, until we fail.
        We return the last result that succeeded. This strategy was inspired by iRRAM. *)
      | Run.NoPrecision ->
        let rec loop n previous_result =
          try
            if n >= prec_mpfr then previous_result
            else
              let current_result = try_lim (2 * n) in
              loop (2 * n) current_result
          with Run.NoPrecision -> previous_result
        in
        let poorest = try_lim 1 in
        loop 1 poorest)

(** Compute a read-only computation. *)
and comp_ro ~prec stack c : Value.result_ro =
  let _, Value.RW v = comp ~prec (make_ro stack) c in
  Value.RO v

(** Compute a read-only computation and extract its value. *)
and comp_ro_value ~prec stack c =
  as_value ~loc:c.Location.loc (comp_ro ~prec stack c)

(** Compute a read-only computation and extract its value as a boolean. *)
and comp_ro_boolean ~loc ~prec stack c =
  as_boolean ~loc:c.Location.loc (comp_ro ~prec stack c)

(** Compute a read-only computation and extract its value as a real. *)
and comp_ro_real ~loc ~prec stack c =
  as_real ~loc:c.Location.loc (comp_ro ~prec stack c)

(* Evaluate a case statement using parallel threads. *)
and comp_case ~loc ~prec stack cases =
  let rec make_guard ~prec b =
    let loc = b.Location.loc in
    fun () ->
      try as_boolean ~loc (comp_ro ~prec stack b)
      with Run.NoPrecision ->
        Parallel.yield ();
        let prec = Run.next_prec ~loc prec in
        make_guard ~prec b ()
  in
  let c =
    Parallel.run_guards (List.map (fun (b, c) -> (make_guard ~prec b, c)) cases)
  in
  comp ~prec stack c

let topcomp ~max_prec stack ({ Location.loc; _ } as c) =
  let require k r =
    let err =
      Dyadic.sub ~prec:12 ~round:Dyadic.up (Real.upper r) (Real.lower r)
    in
    let req = Dyadic.shift ~prec:12 ~round:Dyadic.down Dyadic.one (-k) in
    if not (Dyadic.lt err req) then raise Run.NoPrecision
  in
  let rec loop prec =
    try
      match comp_ro_value ~prec stack c with
      | Value.VReal r as v ->
          require !Config.out_prec r;
          Value.return v
      | (Value.VUnit | Value.VBoolean _ | Value.VInteger _) as v ->
          Value.return v
    with Run.NoPrecision ->
      if !Config.verbose then
        Print.message ~loc "Runtime" "Loss of precision at %t"
          (Run.print_prec prec);
      let prec = Run.next_prec ~loc prec in
      loop prec
  in
  let prec = Run.initial_prec () in
  try Parallel.toplevel ?domains:!Config.domains @@ fun () -> loop prec
  with Parallel.InvalidCase -> Run.(error ~loc InvalidCase)

let topfun stack xs c =
  let g ~loc ~prec vs =
    let stack = push_ros' xs vs stack in
    try comp_ro ~prec stack c
    with Run.Error Location.{ data = err; loc = loc' } ->
      raise (Run.Error (Location.locate ~loc:loc' (Run.CallTrace (loc, err))))
  in
  push_fun g stack

let topexternal ~loc stack s =
  match External.lookup s with
  | None -> Run.(error ~loc (UnknownExternal s))
  | Some g ->
      let h ~loc ~prec vs =
        try g ~prec vs
        with Run.Error { Location.data = err; loc = loc' } ->
          raise
            (Run.Error (Location.locate ~loc:loc' (Run.CallTrace (loc, err))))
      in
      push_fun h stack

let rec toplevel ~quiet runtime { Location.data = c; Location.loc } =
  match c with
  | Syntax.TyTopDo (c, Type.Cmd dt) ->
      let v = topcomp ~max_prec:!Config.max_prec runtime c in
      if not quiet then
        Format.printf "%t : %t@." (Value.print_result v) (Type.print_valty dt);
      runtime
  | Syntax.TyTopTime (c, Type.Cmd dt) ->
      let t0 = Unix.gettimeofday () in
      let v = topcomp ~max_prec:!Config.max_prec runtime c in
      let t1 = Unix.gettimeofday () in
      if not quiet then
        Format.printf "%t : %t@." (Value.print_result v) (Type.print_valty dt);
      Format.printf "Execution time: %f s@." (t1 -. t0);
      runtime
  | Syntax.TyTopFunction (f, xts, c, t) ->
      let runtime = topfun runtime (List.map fst xts) c in
      if not quiet then
        Format.printf "function %s : %t@." f
          (Type.print_funty (List.map snd xts, t));
      runtime
  | Syntax.TyTopExternal (f, s, ft) ->
      let runtime = topexternal ~loc runtime s in
      if not quiet then
        Format.printf "external %s : %t@." f (Type.print_funty ft);
      runtime
  | Syntax.TyTopFile cmds -> topfile ~quiet runtime cmds
  | Syntax.TyTopPrecision p ->
      Config.out_prec := p;
      if not quiet then Format.printf "Output precision set to %d@." p;
      runtime
  | Syntax.TyTopDomains d ->
      Config.domains := Some d;
      if not quiet then Format.printf "Number of domains set to %d@." d;
      runtime

and topfile ~quiet runtime = function
  | [] -> runtime
  | cmd :: cmds ->
      let runtime = toplevel ~quiet runtime cmd in
      topfile ~quiet runtime cmds

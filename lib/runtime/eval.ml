open Util
module Syntax = Typing.Syntax
module Type = Typing.Type
module Dyadic = Reals.Dyadic
module Real = Reals.Real

(** Increase working precision *)
let next_prec ~loc (Run.{ topenv = { prec; _ } as tenv; _ } as env) =
  let prec = Run.next_prec ~loc prec in
  Run.{ env with topenv = { tenv with prec } }

(** Make the stack read-only by pushing a new empty top frame onto it, and
    converting the read-write entries to read-only entries. *)
let make_ro env =
  let Run.{ stack = { frame; frames }; _ } = env in
  Run.{ env with stack = { frame = []; frames = frame :: frames } }

(** Push a read-write value onto the top frame. *)
let push_rw x v (Run.{ stack = st; _ } as env) =
  Run.{ env with stack = { st with frame = (x, RW (ref v)) :: st.frame } }

(** Push a read-only value onto the top frame. *)
let push_ro x v (Run.{ stack = st; _ } as env) =
  Run.{ env with stack = { st with frame = (x, RO v) :: st.frame } }

(** Define a new function. *)
let push_fun f (Run.{ topenv; _ } as env) =
  Run.{ env with topenv = { topenv with funs = topenv.funs @ [ f ] } }

(** Push many read-write values *)
let push_rws xvs env =
  List.fold_left (fun env (x, v) -> push_rw x v env) env xvs

(** Push many read-only values *)
let push_ros xvs env =
  List.fold_left (fun env (x, v) -> push_ro x v env) env xvs

(** Push many read-only values *)
let push_ros' xs vs env =
  List.fold_left2 (fun env x v -> push_ro x v env) env xs vs

(** Lookup a value on the stack *)
let lookup_val k Run.{ stack = { frame; frames }; _ } =
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
let lookup_ref k Run.{ stack = { frame; _ }; _ } =
  let rec lookup k vs =
    match (k, vs) with
    | 0, (_, Run.RO v) :: _ -> None
    | 0, (_, Run.RW r) :: _ -> Some r
    | k, [] -> None
    | k, _ :: vs -> lookup (k - 1) vs
  in
  lookup k frame

(** Lookup a function definition *)
let lookup_fun k Run.{ topenv = { funs; _ }; _ } =
  let rec lookup k fs =
    match (k, fs) with
    | _, [] -> None
    | 0, f :: _ -> Some f
    | k, _ :: fs -> lookup (k - 1) fs
  in
  lookup k funs

(** Print trace *)
let print_trace ~loc Run.{ stack = { frame; frames }; topenv = { prec; _ } } =
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
  | Value.(VInteger _ | VBoolean _ | VReal _ | VArray _) ->
      Run.error ~loc Run.UnitExpected

let as_array ~loc v =
  match Value.ro_as_array v with
  | Some vs -> vs
  | None -> Run.error ~loc Run.ArrayExpected

let as_value ~loc v = Value.ro_as_value v

(** [comp ~prec n stack c] evaluates computation [c] in the given [stack] at
    precision level [n], and returns the new stack and the computed value. *)
let rec comp env { Location.data = c; Location.loc } :
    Run.runtime * Value.result =
  if !Config.trace then print_trace ~loc env;
  match c with
  | Syntax.Var k -> (
      match lookup_val k env with
      | None -> Run.error ~loc Run.OutOfStack
      | Some v -> (env, Value.return v))
  | Syntax.Boolean b -> (env, Value.(return (VBoolean b)))
  | Syntax.Integer k -> (env, Value.(return (VInteger k)))
  | Syntax.Float s ->
      let Run.{ topenv = { prec = { prec_mpfr; _ }; _ }; _ } = env in
      let rl = Dyadic.of_string ~prec:prec_mpfr ~round:Dyadic.down s in
      let ru = Dyadic.of_string ~prec:prec_mpfr ~round:Dyadic.up s in
      let r = Real.make rl ru in
      (env, Value.(return (VReal r)))
  | Syntax.And (e1, e2) -> (
      match comp_ro_boolean ~loc env e1 with
      | false -> (env, Value.(return (VBoolean false)))
      | true ->
          let b = comp_ro_boolean ~loc env e2 in
          (env, Value.(return (VBoolean b))))
  | Syntax.Or (e1, e2) -> (
      match comp_ro_boolean ~loc env e1 with
      | true -> (env, Value.(return (VBoolean true)))
      | false ->
          let b = comp_ro_boolean ~loc env e2 in
          (env, Value.(return (VBoolean b))))
  | Syntax.Apply (k, es) -> (
      match lookup_fun k env with
      | None -> Run.error ~loc Run.InvalidFunction
      | Some f ->
          let vs = List.map (fun e -> comp_ro_value env e) es in
          let p = f ~loc env.Run.topenv vs in
          let r = Value.ro_as_rw p in
          (env, r))
  | Syntax.Skip -> (env, Value.(return VUnit))
  | Syntax.Trace ->
      print_trace ~loc env;
      (env, Value.(return VUnit))
  | Syntax.Sequence (c1, c2) ->
      let stack, r1 = comp env c1 in
      let () = as_unit ~loc:c1.Location.loc r1 in
      comp env c2
  | Syntax.If (b, c1, c2) -> (
      match comp_ro_boolean ~loc env b with
      | true -> comp env c1
      | false -> comp env c2)
  | Syntax.Case cases -> comp_case ~loc env cases
  | Syntax.While (b, c) ->
      let granularity = 1000 in
      let rec loop k env =
        if k = 0 then Parallel.yield ();
        match comp_ro_boolean ~loc:b.Location.loc env b with
        | false -> (env, Value.(return VUnit))
        | true ->
            let env, v = comp env c in
            let () = as_unit ~loc:c.Location.loc v in
            loop ((k + 1) mod granularity) env
      in
      loop 1 env
  | Syntax.Newvar (xes, c) ->
      let xvs = List.map (fun (x, e) -> (x, comp_ro_value env e)) xes in
      let env' = push_rws xvs env in
      let _, v = comp env' c in
      (env, v)
  | Syntax.Let (xes, c) ->
      let xvs = List.map (fun (x, e) -> (x, comp_ro_value env e)) xes in
      let env' = push_ros xvs env in
      let _, v = comp env' c in
      (env, v)
  | Syntax.PNewvar (xes, c) ->
      let xvs = Parallel.map (fun (x, e) -> (x, comp_ro_value env e)) xes in
      let env' = push_rws xvs env in
      let _, v = comp env' c in
      (env, v)
  | Syntax.PLet (xes, c) ->
      let xvs = Parallel.map (fun (x, e) -> (x, comp_ro_value env e)) xes in
      let env' = push_ros xvs env in
      let _, v = comp env' c in
      (env, v)
  | Syntax.Assign (k, e) -> (
      match lookup_ref k env with
      | None -> Run.error ~loc Run.CannotWrite
      | Some r ->
          let v = comp_ro_value env e in
          r := v;
          (env, Value.(return VUnit)))
  | Syntax.Lim (x, e) ->
      (* when computing at precision n we first try to compute the n-th term
        of the limit, and use that as the approximate result. If the computation
        fails we fall back to computing successively the 1st, 2nd, ... term of
        the limit, and take the last one that doesn't fail.
     *)
      let Run.{ topenv = { prec = { prec_mpfr = n; _ }; _ }; _ } = env in
      (* We will compute the n-th term of the limit and make sure the output interval contains the true
         limit and has width at most 2^(-n+1). *)
      let env' = push_ro x (Value.VInteger (Mpzf.of_int n)) env in
      let width = Dyadic.shift ~prec:16 ~round:Dyadic.down Dyadic.one (-n) in
      let r = comp_ro_real_width ~loc:e.Location.loc width env' e in
      let err = Dyadic.shift ~prec:n ~round:Dyadic.up Dyadic.one (-n) in
      let rl = Dyadic.sub ~prec:n ~round:Dyadic.down (Real.lower r) err
      and ru = Dyadic.add ~prec:n ~round:Dyadic.up (Real.upper r) err in
      let r = Real.make rl ru in
      (env, Value.(return (VReal r)))
  | Syntax.ArrayEnum es ->
      let vs =
        Array.of_list
        @@ List.mapi
             (fun i e ->
               let env = push_ro "_i" (Value.VInteger (Mpzf.of_int i)) env in
               comp_ro_value env e)
             es
      in
      (env, Value.(return (VArray vs)))
  | Syntax.ArrayInit (e1, x, e2) ->
      let n = comp_ro_int ~loc env e1 in
      let vs =
        Parallel.array_map (fun i ->
            let env = push_ro x (Value.VInteger (Mpzf.of_int i)) env in
            comp_ro_value env e2)
        @@ Array.init n (fun i -> i)
      in
      (env, Value.(return (VArray vs)))
  | Syntax.ArrayIndex (e1, e2) -> (
      let vs = comp_ro_array ~loc env e1 in
      (* Index could be out of bounds, report error *)
      try
        let i = comp_ro_int ~loc env e2 in
        let v = vs.(i) in
        (env, Value.(return v))
      with Invalid_argument e -> Run.error ~loc @@ Run.ArrayError e)
  | Syntax.ArrayLen e ->
      let vs = comp_ro_array ~loc env e in
      let n = Array.length vs in
      (env, Value.(return (Value.VInteger (Mpzf.of_int n))))

(** Compute a read-only computation. *)
and comp_ro env c : Value.result_ro =
  let _, Value.RW v = comp (make_ro env) c in
  Value.RO v

(** Compute a read-only computation and extract its value. *)
and comp_ro_value env c = as_value ~loc:c.Location.loc (comp_ro env c)

(** Compute a read-only computation and extract its value as a boolean. *)
and comp_ro_boolean ~loc env c = as_boolean ~loc:c.Location.loc (comp_ro env c)

(** Compute a read-only computation and extract its value as a real. *)
and comp_ro_real ~loc env c = as_real ~loc:c.Location.loc (comp_ro env c)

(** Compute a read-only computation and extract is value as a real. If the
    output interval has width more than [width], repeat at a higher working
    precision. The output is guaranteed to have widh at most [width]. *)
and comp_ro_real_width ~loc width env c =
  let r = comp_ro_real ~loc env c in
  let r_width = Real.width ~prec:16 ~round:Real.down r in
  if Dyadic.leq r_width width then r
  else (
    Format.printf "comp_ro_real_width increasing prec@\n@.";
    comp_ro_real_width ~loc width (next_prec ~loc env) c)

(** Compute a read-only computation and extract its value as an array. *)
and comp_ro_array ~loc env c = as_array ~loc:c.Location.loc (comp_ro env c)

(** Compute a read-only computation and extract its value as an int. *)
and comp_ro_int ~loc env c =
  let v = as_integer ~loc:c.Location.loc (comp_ro env c) in
  if Mpz.fits_int_p @@ v then Mpz.get_int v
  else Run.error ~loc Run.IntegerOverflow

(* Evaluate a case statement using parallel threads. *)
and comp_case ~loc env cases =
  let rec make_guard env b =
    let loc = b.Location.loc in
    fun () ->
      try as_boolean ~loc (comp_ro env b)
      with Run.NoPrecision ->
        Parallel.yield ();
        let env = next_prec ~loc env in
        make_guard env b ()
  in
  let c =
    Parallel.run_guards (List.map (fun (b, c) -> (make_guard env b, c)) cases)
  in
  comp env c

let topcomp ~max_prec env ({ Location.loc; _ } as c) =
  let require k r =
    let err =
      Dyadic.sub ~prec:12 ~round:Dyadic.up (Real.upper r) (Real.lower r)
    in
    let req = Dyadic.shift ~prec:12 ~round:Dyadic.down Dyadic.one (-k) in
    if not (Dyadic.lt err req) then raise Run.NoPrecision
  in
  let rec require_real_array = function
    | Value.VReal r as v ->
        require !Config.out_prec r;
        v
    | Value.VArray rs -> Value.VArray (Array.map require_real_array rs)
    | v -> v
  in
  let rec loop env =
    try
      match comp_ro_value env c with
      | Value.VReal r as v ->
          require !Config.out_prec r;
          Value.return v
      | Value.VArray rs as vs -> Value.return @@ require_real_array vs
      | (Value.VUnit | Value.VBoolean _ | Value.VInteger _) as v ->
          Value.return v
    with Run.NoPrecision ->
      if !Config.verbose then
        Print.message ~loc "Runtime" "Loss of precision at %t"
          (Run.print_prec (Run.get_prec env));
      let env = next_prec ~loc env in
      loop env
  in
  try Parallel.toplevel ?domains:!Config.domains @@ fun () -> loop env
  with Parallel.InvalidCase -> Run.(error ~loc InvalidCase)

let topfun env xs c =
  let g ~loc topenv vs =
    let env = Run.{ topenv; stack = initial_stack } in
    let env = push_ros' xs vs env in
    try comp_ro env c
    with Run.Error Location.{ data = err; loc = loc' } ->
      raise (Run.Error (Location.locate ~loc:loc' (Run.CallTrace (loc, err))))
  in
  push_fun g env

let rec topfuns ~quiet env = function
  | [] -> env
  | Location.{ data = f, xts, c, t; loc } :: lst ->
      let env = topfun env (List.map fst xts) c in
      if not quiet then
        Format.printf "function %s : %t@." f
          (Type.print_funty (List.map snd xts, t));
      topfuns ~quiet env lst

let topexternal ~loc env s =
  match External.lookup s with
  | None -> Run.(error ~loc (UnknownExternal s))
  | Some g ->
      let h ~loc topenv vs =
        let Run.{ prec; _ } = topenv in
        try g ~prec vs
        with Run.Error { Location.data = err; loc = loc' } ->
          raise
            (Run.Error (Location.locate ~loc:loc' (Run.CallTrace (loc, err))))
      in
      push_fun h env

let rec toplevel ~quiet env Location.{ data = c; loc } =
  match c with
  | Syntax.TyTopDo (c, Type.Cmd dt) ->
      let v = topcomp ~max_prec:!Config.max_prec env c in
      if not quiet then
        Format.printf "%t : %t@." (Value.print_result v) (Type.print_valty dt);
      env
  | Syntax.TyTopTime (c, Type.Cmd dt) ->
      let t0 = Unix.gettimeofday () in
      let v = topcomp ~max_prec:!Config.max_prec env c in
      let t1 = Unix.gettimeofday () in
      if not quiet then
        Format.printf "%t : %t@." (Value.print_result v) (Type.print_valty dt);
      Format.printf "Execution time: %f s@." (t1 -. t0);
      env
  | Syntax.TyTopFunctions lst -> topfuns ~quiet env lst
  | Syntax.TyTopExternal (f, s, ft) ->
      let env = topexternal ~loc env s in
      if not quiet then
        Format.printf "external %s : %t@." f (Type.print_funty ft);
      env
  | Syntax.TyTopFile cmds -> topfile ~quiet env cmds
  | Syntax.TyTopPrecision p ->
      Config.out_prec := p;
      if not quiet then Format.printf "Output precision set to %d@." p;
      env
  | Syntax.TyTopDomains d ->
      Config.domains := Some d;
      if not quiet then Format.printf "Number of domains set to %d@." d;
      env

and topfile ~quiet env = function
  | [] -> env
  | cmd :: cmds ->
      let env = toplevel ~quiet env cmd in
      topfile ~quiet env cmds

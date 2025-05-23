(** Clerical types. *)

open Util

(* Typing context *)
type entry = RO of Type.valty | RW of Type.valty

type context = {
  frame : entry list;
  frames : entry list list;
  funs : Type.funty list;
}

let initial = { frame = []; frames = []; funs = [] }

(** Make the contexr read-only by pushing a new empty top frame onto it. *)
let make_ro { frame; frames; funs } =
  { frame = []; frames = frame :: frames; funs }

(** Push a read-write datatype onto the top frame. *)
let push_rw dt ctx = { ctx with frame = RW dt :: ctx.frame }

(** Push a read-only datatype onto the top frame. *)
let push_ro dt ctx = { ctx with frame = RO dt :: ctx.frame }

(** Define a new function. *)
let push_fun ft ctx = { ctx with funs = ctx.funs @ [ ft ] }

let push_args xts ctx =
  List.fold_left (fun ctx (_, dt) -> push_ro dt ctx) (make_ro ctx) xts

(** Type errors *)
type type_error =
  | InternalError of string
  | TypeMismatch of Type.valty * Type.valty
  | InvalidApplication of int * int
  | InvalidAssign
  | ValueExpected
  | InvalidDomains
  | EmptyArrayEnum
  | ArrayExpected

exception Error of type_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let print_error err ppf =
  match err with
  | InternalError s -> Format.fprintf ppf "internal error %s, please report" s
  | TypeMismatch (t_expected, t_actual) ->
      Format.fprintf ppf "expected %t but found %t"
        (Type.print_valty t_expected)
        (Type.print_valty t_actual)
  | InvalidApplication (i, j) ->
      Format.fprintf ppf "%d arguments expected but %d given" i j
  | InvalidAssign -> Format.fprintf ppf "cannot write to readonly variable"
  | ValueExpected -> Format.fprintf ppf "expected a value but got a command"
  | InvalidDomains ->
      Format.fprintf ppf "the number of domains must be between 1 and 128"
  | EmptyArrayEnum ->
      Format.fprintf ppf
        "an empty array [] is not allowed, use (array[0] i => ...) instead"
  | ArrayExpected ->
      Format.fprintf ppf
        "an empty array [] is not allowed, use (array[0] i => ...) instead"

(** Lookup the type of an identifier *)
let lookup_val k { frame; frames; _ } =
  let rec lookup k vs vss =
    match (k, vs, vss) with
    | 0, RO t :: _, _ -> t
    | 0, RW t :: _, _ -> t
    | k, [], vs :: vss -> lookup k vs vss
    | k, [], [] ->
        error ~loc:Location.nowhere (InternalError "Typecheck.lookup_val")
    | k, _ :: vs, vss -> lookup (k - 1) vs vss
  in
  lookup k frame frames

(** Lookup the type of a writable identifier *)
let lookup_ref k { frame; frames; _ } =
  let rec lookup k vs vss =
    match (k, vs, vss) with
    | 0, RO t :: _, _ -> None
    | 0, RW t :: _, _ -> Some t
    | k, [], vs :: vss -> lookup k vs vss
    | k, [], [] ->
        error ~loc:Location.nowhere (InternalError "Typecheck.lookup_ref")
    | k, _ :: vs, vss -> lookup (k - 1) vs vss
  in
  lookup k frame frames

(** Lookup a function type *)
let lookup_fun j { funs; _ } =
  let rec lookup k ts =
    match (k, ts) with
    | _, [] ->
        error ~loc:Location.nowhere (InternalError "Typecheck.lookup_fun")
    | 0, t :: _ -> t
    | k, _ :: ts -> lookup (k - 1) ts
  in
  lookup j funs

(** Infer the type of a read-write computation. *)
let rec comp ctx { Location.data = c; loc } =
  match c with
  | Syntax.Var k -> Type.Cmd (lookup_val k ctx)
  | Syntax.Boolean _ -> Type.(Cmd Boolean)
  | Syntax.Integer _ -> Type.(Cmd Integer)
  | Syntax.Float _ -> Type.(Cmd Real)
  | Syntax.And (c1, c2) ->
      check_expr ctx Type.Boolean c1;
      check_expr ctx Type.Boolean c2;
      Type.(Cmd Boolean)
  | Syntax.Or (c1, c2) ->
      check_expr ctx Type.Boolean c1;
      check_expr ctx Type.Boolean c2;
      Type.(Cmd Boolean)
  | Syntax.Apply (k, args) ->
      let t_args, t_ret = lookup_fun k ctx in
      check_args ~loc ctx t_args args;
      t_ret
  | Syntax.Skip -> Type.(Cmd Unit)
  | Syntax.Trace -> Type.(Cmd Unit)
  | Syntax.Sequence (c1, c2) ->
      check_comp ctx Type.(Cmd Unit) c1;
      comp ctx c2
  | Syntax.Case [] -> error ~loc (InternalError "Typecheck.comp")
  | Syntax.Case ((b, c) :: lst) ->
      check_expr ctx Type.Boolean b;
      let t = comp ctx c in
      let rec fold = function
        | [] -> t
        | (b, c) :: lst ->
            check_expr ctx Type.Boolean b;
            check_comp ctx t c;
            fold lst
      in
      fold lst
  | Syntax.If (b, c1, c2) ->
      check_expr ctx Type.Boolean b;
      let t = comp ctx c1 in
      check_comp ctx t c2;
      t
  | Syntax.While (b, c) ->
      check_expr ctx Type.Boolean b;
      check_comp ctx Type.(Cmd Unit) c;
      Type.(Cmd Unit)
  | Syntax.Let (lst, c) ->
      let ctx = let_clauses ctx lst in
      comp ctx c
  | Syntax.Newvar (lst, c) ->
      let ctx = newvar_clauses ctx lst in
      comp ctx c
  | Syntax.PLet (lst, c) ->
      let ctx = let_clauses ctx lst in
      comp ctx c
  | Syntax.PNewvar (lst, c) ->
      let ctx = newvar_clauses ctx lst in
      comp ctx c
  | Syntax.Assign (k, e) -> (
      match lookup_ref k ctx with
      | None -> error ~loc InvalidAssign
      | Some dt ->
          check_expr ctx dt e;
          Type.(Cmd Unit))
  | Syntax.Lim (_, e) ->
      let ctx = push_ro Type.Integer ctx in
      check_expr ctx Type.Real e;
      Type.(Cmd Real)
  | Syntax.ArrayEnum [] -> error ~loc EmptyArrayEnum
  | Syntax.ArrayEnum (e :: es) ->
      let dt = expr ctx e in
      List.iter (check_expr ctx dt) es;
      Type.(Cmd (Array dt))
  | Syntax.ArrayInit (e1, _, e2) ->
      check_expr ctx Type.Integer e1;
      let ctx = push_ro Type.Integer ctx in
      let dt = expr ctx e2 in
      Type.(Cmd (Array dt))
  | Syntax.ArrayIndex (e1, e2) ->
      let dt = check_array ctx e1 in
      check_expr ctx Type.Integer e2;
      Type.Cmd dt
  | Syntax.ArrayLen e ->
      let _ = check_array ctx e in
      Type.(Cmd Integer)

(** Infer the type of an expression (read-only computation). *)
and expr ctx c =
  let (Type.Cmd dt) = comp (make_ro ctx) c in
  dt

(** Check that a read-write computation has the given type. *)
and check_comp ctx (Type.Cmd t) c =
  let (Type.Cmd t') = comp ctx c in
  if t <> t' then error ~loc:c.Location.loc (TypeMismatch (t, t'))

(** Check that an expression (read-only computation) has the given type. *)
and check_expr ctx dt c = check_comp ctx (Type.Cmd dt) c

(** Check that a read-only computation is an array and return the type
    of the array elements. *)
and check_array ctx e =
  match expr ctx e with
  | Type.Array dt -> dt
  | Type.(Boolean | Integer | Real | Unit) ->
      error ~loc:e.Location.loc ArrayExpected

(** Check that the arguments of a function have the given types. *)
and check_args ~loc ctx dts cs =
  let rec fold dts' cs' =
    match (dts', cs') with
    | [], [] -> ()
    | dt :: dts', c :: cs' ->
        check_expr ctx dt c;
        fold dts' cs'
    | [], _ :: _ | _ :: _, [] ->
        error ~loc (InvalidApplication (List.length dts, List.length cs))
  in
  fold dts cs

and let_clauses ctx lst =
  let rec fold ctx' = function
    | [] -> ctx'
    | (_, c) :: cs ->
        let t = expr ctx c in
        let ctx' = push_ro t ctx' in
        fold ctx' cs
  in
  fold ctx lst

and newvar_clauses ctx lst =
  let rec fold ctx' = function
    | [] -> ctx'
    | (_, c) :: cs ->
        let t = expr ctx c in
        let ctx' = push_rw t ctx' in
        fold ctx' cs
  in
  fold ctx lst

let check_fundefs ctx lst =
  let ctx_fs =
    List.fold_left
      (fun ctx Location.{ data = _, xts, _, t; _ } ->
        push_fun (List.map snd xts, t) ctx)
      ctx lst
  in
  List.iter
    (fun Location.{ data = _, xts, c, t; _ } ->
      check_comp (push_args xts ctx_fs) t c)
    lst;
  ctx_fs

(** Typecheck a toplevel directive. *)
let rec toplevel ctx { Location.data = tc; loc } =
  let ctx, tc =
    match tc with
    | Syntax.TopDo c ->
        let t = comp ctx c in
        (ctx, Syntax.TyTopDo (c, t))
    | Syntax.TopTime c ->
        let t = comp ctx c in
        (ctx, Syntax.TyTopTime (c, t))
    | Syntax.TopFunctions lst ->
        let ctx = check_fundefs ctx lst in
        (ctx, Syntax.TyTopFunctions lst)
    | Syntax.TopExternal (f, s, ft) ->
        let ctx = push_fun ft ctx in
        (ctx, Syntax.TyTopExternal (f, s, ft))
    | Syntax.TopFile lst ->
        let ctx, cmds = topfile ctx lst in
        (ctx, Syntax.TyTopFile cmds)
    | Syntax.TopPrecision p -> (ctx, Syntax.TyTopPrecision p)
    | Syntax.TopDomains d ->
        if 1 <= d && d <= 128 then (ctx, Syntax.TyTopDomains d)
        else error ~loc InvalidDomains
  in
  (ctx, Location.locate ~loc tc)

and topfile ctx lst =
  let rec fold ctx lst' = function
    | [] -> (ctx, List.rev lst')
    | c :: lst ->
        let ctx, c = toplevel ctx c in
        fold ctx (c :: lst') lst
  in
  fold ctx [] lst

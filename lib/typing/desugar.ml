(** Conversion from concrete syntax to abstract syntax. Here we also load all
    required files, which may not be optimal but is systematic. *)

open Util
module Input = Parsing.Input

(** Conversion errors *)
type desugar_error = UnknownIdentifier of string | UnknownFunction of string

exception Error of desugar_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let print_error err ppf =
  match err with
  | UnknownIdentifier x -> Format.fprintf ppf "unknown identifier %s" x
  | UnknownFunction x -> Format.fprintf ppf "unknown function %s" x

type context = { idents : Name.ident list; funs : Name.ident list }
(** A desugaring context is a list of known identifiers, which is used to
    compute de Bruijn indices. *)

(** Initial context *)
let initial = { idents = []; funs = [] }

(** Add a new identifier to the context. *)
let add_ident x { idents; funs } = { idents = x :: idents; funs }

(** Add many identifiers to the context. *)
let add_args xts ctx =
  List.fold_left (fun ctx (x, _) -> add_ident x ctx) ctx xts

(** Add a new function name to the context. *)
let add_fun f { idents; funs } = { idents; funs = funs @ [f] }

(** Find the de Bruijn index of [x] in the context [ctx]. *)
let index x { idents; _ } =
  let rec search k = function
    | [] -> None
    | y :: ys -> if x = y then Some k else search (k + 1) ys
  in
  search 0 idents

let index_fun f { funs; _ } =
  let rec search k = function
    | [] -> None
    | g :: gs -> if f = g then Some k else search (k + 1) gs
  in
  search 0 funs

(** Desugar a value type *)
let valty = function
  | Input.TBoolean -> Type.Boolean
  | Input.TInteger -> Type.Integer
  | Input.TReal -> Type.Real
  | Input.TUnit -> Type.Unit

(** Desugar a computation type *)
let cmpty t =
  let dt = valty t in
  Type.Cmd dt

(** Desugar a function type *)
let funty (dts, t) = (List.map valty dts, cmpty t)

(** Desugar a computation *)
let rec comp ctx { Location.data = c; Location.loc } =
  let comp' ctx = function
    | Input.Var x -> (
        match index x ctx with
        | None -> error ~loc (UnknownIdentifier x)
        | Some k -> Syntax.Var k)

    | Input.Boolean b -> Syntax.Boolean b

    | Input.Integer k -> Syntax.Integer k

    | Input.Float x -> Syntax.Float x

    | Input.And (c1, c2) ->
      let c1 = comp ctx c1
      and c2 = comp ctx c2 in
      Syntax.And (c1, c2)

    | Input.Or (c1, c2) ->
      let c1 = comp ctx c1
      and c2 = comp ctx c2 in
      Syntax.Or (c1, c2)

    | Input.Apply (f, es) -> (
        match index_fun f ctx with
        | None -> error ~loc (UnknownFunction f)
        | Some k ->
            let es = List.map (comp ctx) es in
            Syntax.Apply (k, es))
    | Input.Skip -> Syntax.Skip
    | Input.Trace -> Syntax.Trace
    | Input.Sequence (c1, c2) ->
        let c1 = comp ctx c1 and c2 = comp ctx c2 in
        Syntax.Sequence (c1, c2)
    | Input.Case lst ->
        let rec fold = function
          | [] -> []
          | (b, c) :: lst ->
              let b = comp ctx b and c = comp ctx c and lst = fold lst in
              (b, c) :: lst
        in
        Syntax.Case (fold lst)
    | Input.If (b, c1, c2) ->
        let b = comp ctx b and c1 = comp ctx c1 and c2 = comp ctx c2 in
        Syntax.If (b, c1, c2)
    | Input.While (b, c) ->
        let b = comp ctx b and c = comp ctx c in
        Syntax.While (b, c)
    | Input.Let (lst, c) ->
        let rec fold ctx' lst' = function
          | [] -> (ctx', List.rev lst')
          | (x, c) :: lst ->
              let c = comp ctx c in
              let ctx' = add_ident x ctx' and lst' = (x, c) :: lst' in
              fold ctx' lst' lst
        in
        let ctx, lst = fold ctx [] lst in
        let c = comp ctx c in
        Syntax.Let (lst, c)
    | Input.Newvar (lst, c) ->
        let rec fold ctx' lst' = function
          | [] -> (ctx', List.rev lst')
          | (x, c) :: lst ->
              let c = comp ctx c in
              let ctx' = add_ident x ctx' and lst' = (x, c) :: lst' in
              fold ctx' lst' lst
        in
        let ctx, lst = fold ctx [] lst in
        let c = comp ctx c in
        Syntax.Newvar (lst, c)
    | Input.PLet (lst, c) ->
        let rec fold ctx' lst' = function
          | [] -> (ctx', List.rev lst')
          | (x, c) :: lst ->
              let c = comp ctx c in
              let ctx' = add_ident x ctx' and lst' = (x, c) :: lst' in
              fold ctx' lst' lst
        in
        let ctx, lst = fold ctx [] lst in
        let c = comp ctx c in
        Syntax.PLet (lst, c)
    | Input.PNewvar (lst, c) ->
        let rec fold ctx' lst' = function
          | [] -> (ctx', List.rev lst')
          | (x, c) :: lst ->
              let c = comp ctx c in
              let ctx' = add_ident x ctx' and lst' = (x, c) :: lst' in
              fold ctx' lst' lst
        in
        let ctx, lst = fold ctx [] lst in
        let c = comp ctx c in
        Syntax.PNewvar (lst, c)
    | Input.Assign (x, e) -> (
        match index x ctx with
        | None -> error ~loc (UnknownIdentifier x)
        | Some k ->
            let e = comp ctx e in
            Syntax.Assign (k, e))
    | Input.Lim (x, e) ->
        let ctx = add_ident x ctx in
        let e = comp ctx e in
        Syntax.Lim (x, e)
  in
  let c = comp' ctx c in
  Location.locate ~loc c

and fundefs ctx lst =
  let ctx_fs = List.fold_left (fun ctx Location.{data=(f, _, _, _); _} -> add_fun f ctx) ctx lst in
  let lst = List.map
    (function Location.{loc; data=(f, xts, c, t)} ->
       let t = cmpty t in
       let ctx_c = add_args xts ctx_fs in
       let c = comp ctx_c c
       and xts = List.map (fun (x, t) -> (x, valty t)) xts in
       Location.locate ~loc (f, xts, c, t)
    )
    lst
  in
  (ctx_fs, Syntax.TopFunctions lst)


let rec toplevel ctx { Location.data = c; Location.loc } =
  let toplevel' ctx = function
    | Input.TopDo c ->
        let c = comp ctx c in
        (ctx, Syntax.TopDo c)
    | Input.TopTime c ->
        let c = comp ctx c in
        (ctx, Syntax.TopTime c)
    | Input.TopFunctions fs ->
        fundefs ctx fs
    | Input.TopExternal (f, s, ft) ->
        let ctx = add_fun f ctx in
        let ft = funty ft in
        (ctx, Syntax.TopExternal (f, s, ft))
    | Input.TopLoad fn ->
        let ctx, cmds = load ctx fn in
        (ctx, Syntax.TopFile cmds)
    | Input.TopPrecision p -> (ctx, Syntax.TopPrecision (Mpz.get_int p))
    | Input.TopDomains d -> (ctx, Syntax.TopDomains (Mpz.get_int d))
  in
  let ctx, c = toplevel' ctx c in
  (ctx, Location.locate ~loc c)

and load ctx fn =
  let cmds = Parsing.Lexer.read_file Parsing.Parser.file fn in
  let ctx, cmds =
    List.fold_left
      (fun (ctx, cmds) cmd ->
        let ctx, cmd = toplevel ctx cmd in
        (ctx, cmd :: cmds))
      (ctx, []) cmds
  in
  let cmds = List.rev cmds in
  (ctx, cmds)

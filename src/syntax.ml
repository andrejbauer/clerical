(** Abstract syntax. *)

(* We use de Bruijn indices for variables *)
type index = int

type operator = string

(* TODO add reals *)
type comp = comp' Location.located
and comp' =
  | Var of index
  | Boolean of bool
  | Integer of Mpzf.t
  | Float of string
  | CastReal of comp
  | Apply of index * comp list
  | Skip
  | Sequence of comp * comp
  | Case of (comp * comp) list
  | If of comp * comp * comp
  | While of comp * comp
  | Let of comp list * comp
  | Newvar of comp list * comp
  | Assign of index * comp
  | Lim of comp

type toplevel = toplevel' Location.located
and toplevel' =
  | TopDo of comp
  | TopLet of (Name.ident * comp) list
  | TopFunction of Name.ident * (Name.ident * Type.valty) list * comp
  | TopExternal of Name.ident * string * Type.funty
  | TopFile of toplevel list
  | TopPrecision of int

(** Toplevel command annotated with types *)
type tytoplevel = tytoplevel' Location.located
and tytoplevel' =
  | TyTopDo of comp * Type.cmdty
  | TyTopLet of (Name.ident * comp * Type.valty) list
  | TyTopFunction of Name.ident * (Name.ident * Type.valty) list * comp * Type.cmdty
  | TyTopExternal of Name.ident * string * Type.funty
  | TyTopFile of tytoplevel list
  | TyTopPrecision of int

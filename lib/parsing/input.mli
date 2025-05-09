open Util

type operator = string

type ty = TBoolean | TInteger | TReal | TUnit

type funty = ty list * ty

type comp = comp' Location.located

and comp' =
  | Var of Name.ident
  | Boolean of bool
  | Integer of Mpzf.t
  | Float of string (* store the string so we can correctly round later *)
  | Apply of operator * comp list
  | Skip
  | Sequence of comp * comp
  | Case of (comp * comp) list
  | If of comp * comp * comp
  | While of comp * comp
  | Let of (Name.ident * comp) list * comp
  | Newvar of (Name.ident * comp) list * comp
  | Assign of Name.ident * comp
  | Lim of Name.ident * comp
  | Trace

type toplevel = toplevel' Location.located

and toplevel' =
  | TopDo of comp
  | TopFunction of Name.ident * (Name.ident * ty) list * comp
  | TopExternal of Name.ident * string * funty
  | TopLoad of string
  | TopPrecision of Mpzf.t
  | TopDomains of Mpzf.t

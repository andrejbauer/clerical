(** Abstract syntax. *)

open Util

(* We use de Bruijn indices for variables *)
type index = int
type operator = string

type comp = comp' Location.located

and comp' =
  | Var of index
  | Boolean of bool
  | Integer of Mpzf.t
  | Float of string
  | And of comp * comp
  | Or of comp * comp
  | Apply of index * comp list
  | Skip
  | Sequence of comp * comp
  | Case of (comp * comp) list
  | If of comp * comp * comp
  | While of comp * comp
  | Let of (Name.ident * comp) list * comp
  | Newvar of (Name.ident * comp) list * comp
  | PLet of (Name.ident * comp) list * comp
  | PNewvar of (Name.ident * comp) list * comp
  | Assign of index * comp
  | Lim of Name.ident * comp
  | Trace

type toplevel = toplevel' Location.located

and toplevel' =
  | TopDo of comp
  | TopTime of comp
  | TopFunction of Name.ident * (Name.ident * Type.valty) list * comp
  | TopExternal of Name.ident * string * Type.funty
  | TopFile of toplevel list
  | TopPrecision of int
  | TopDomains of int

type tytoplevel = tytoplevel' Location.located
(** Toplevel command annotated with types *)

and tytoplevel' =
  | TyTopDo of comp * Type.cmdty
  | TyTopTime of comp * Type.cmdty
  | TyTopFunction of
      Name.ident * (Name.ident * Type.valty) list * comp * Type.cmdty
  | TyTopExternal of Name.ident * string * Type.funty
  | TyTopFile of tytoplevel list
  | TyTopPrecision of int
  | TyTopDomains of int

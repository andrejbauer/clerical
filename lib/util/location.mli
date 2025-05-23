(** Source code locations. *)
type t =
  | Location of Lexing.position * Lexing.position  (** delimited location *)
  | Nowhere  (** no location *)

type 'a located = private { data : 'a; loc : t }
(** A datum tagged with a source code location *)

val locate : ?loc:t -> 'a -> 'a located
(** Tag a datum with an (optional) location. *)

val nowhere : t
(** An unknown location, use with care. *)

val of_lex : Lexing.lexbuf -> t
(** Convert a [Lexing.lexbuf] location to a [location] *)

val make : Lexing.position -> Lexing.position -> t
(** [make p1 p2] creates a location which starts at [p1] and ends at [p2]. *)

val print : t -> Format.formatter -> unit
(** Print a location *)

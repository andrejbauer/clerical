(* The runtime environment.

   Programs are excuted by a stack machine. A stack consists of
   a top frame, followed by a stack of frames. Each frame is a
   stack of read-only and read-write values. Only the read-write
   values in the top frame can be written into.

   In addition to the stack there is also a global list of function
   definitions.
*)

open Util

(** Runtime errors *)
type runtime_error =
  | CallTrace of Location.t * runtime_error
  | OutOfStack
  | UnitExpected
  | BooleanExpected
  | IntegerExpected
  | RealExpected
  | ArrayExpected
  | NonnegativeRealExpected
  | PrecisionLoss
  | CannotWrite
  | InvalidFunction
  | InvalidCase
  | IntegerOverflow
  | ArrayError of string
  | InvalidExternal of string
  | UnknownExternal of string
  | InternalError of string

exception Error of runtime_error Location.located

exception NoPrecision
(** Exception that signals loss of precision *)

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let rec print_error err ppf =
  match err with
  | CallTrace (loc, err) ->
      Format.fprintf ppf "(call at %t)@\n%t" (Location.print loc)
        (print_error err)
  | OutOfStack -> Format.fprintf ppf "invalid stack position"
  | UnitExpected -> Format.fprintf ppf "did not expect a return value here"
  | BooleanExpected -> Format.fprintf ppf "boolean expected"
  | IntegerExpected -> Format.fprintf ppf "integer expected"
  | RealExpected -> Format.fprintf ppf "real expected"
  | ArrayExpected -> Format.fprintf ppf "array expected"
  | NonnegativeRealExpected -> Format.fprintf ppf "non-negative real expected"
  | PrecisionLoss ->
      Format.fprintf ppf "loss of precision, try increasing --max-prec"
  | CannotWrite -> Format.fprintf ppf "cannot write into a read-only position"
  | InvalidFunction -> Format.fprintf ppf "invalid function application"
  | InvalidCase -> Format.fprintf ppf "invalid case statement"
  | IntegerOverflow -> Format.fprintf ppf "integer overflow"
  | ArrayError s -> Format.fprintf ppf "array error: (%s)" s
  | InvalidExternal s -> Format.fprintf ppf "invalid application of %s" s
  | UnknownExternal s -> Format.fprintf ppf "unknown external function %s" s
  | InternalError s -> Format.fprintf ppf "internal error (%s)" s

(** A stack entry *)
type entry =
  | RO of Value.value  (** read-only stack value *)
  | RW of Value.value ref  (** read-write stack value *)

type precision = { prec_mpfr_min : int; prec_lim_min : int; prec_mpfr : int }
(** Precision describes at what precision we run MPFR (the field [prec_mpfr])
    and which elements of limits we are computing [prec_lim]. We also record the
    starting values. The field prec_while gives the maximum number of iterations
    allowed in a while loop. *)

(** In absence of any knowledge, we scan for each value of [prec_mpfr] all
    values of [prec_lim] up to [prec_mpfr]. *)
let next_prec ~loc
    ({ prec_mpfr_min = _k0; prec_lim_min = _n0; prec_mpfr = k } as prec) =
  (* if 2 * n < k then { prec with prec_lim = n + 1} *)
  (* else  *)
  if k >= !Config.max_prec then error ~loc PrecisionLoss
  else { prec with prec_mpfr = 1 + (3 * k / 2) }

let initial_prec () =
  let k0 = max 2 !Config.init_prec and n0 = 0 in
  { prec_mpfr_min = k0; prec_lim_min = n0; prec_mpfr = k0 }

let print_prec { prec_mpfr = k; _ } ppf = Format.fprintf ppf "(mpfr=%d)" k

(** A Clerical function *)
type func = loc:Location.t -> topenv -> Value.value list -> Value.result_ro

(** The top-level environment, currently it holds just function definitions *)
and topenv = {
  prec : precision ;
  funs : func list ;
}

(** The top frame is the one that we can write into, all the other frames are read-only. *)
type stack = {
  frame : (Name.ident * entry) list; (* read-write *)
  frames : (Name.ident * entry) list list; (* read-only *)
}

type runtime = {
  topenv : topenv ;
  stack : stack
}

(** Initial toplevel *)
let initial_topenv = { prec = initial_prec () ; funs = [] }

(** Initial stack *)
let initial_stack = { frame = []; frames = [] }

let initial_runtime = { topenv = initial_topenv ; stack = initial_stack }

let get_prec {topenv={prec;_};_} = prec
let get_stack {stack;_} = stack

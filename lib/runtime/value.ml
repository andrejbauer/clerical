(* Runtime values and computations. *)

(** Value *)
type value =
  | VBoolean of bool
  | VInteger of Mpzf.t
  | VReal of Reals.Real.t
  | VUnit

(** Result of a read-only computation *)
type result_ro = RO of value

(** Result of a read-write computation *)
type result = RW of value

(** Embed a value into a read-write computation *)
let return v = RW v

(** Embed a value into a read-only computation *)
let return_ro v = RO v

(** Extract an integers from a value *)
let value_as_integer = function
  | VInteger k -> Some k
  | VBoolean _ | VReal _ | VUnit -> None

(** Extract a boolean from a value *)
let value_as_boolean = function
  | VBoolean b -> Some b
  | VInteger _ | VReal _ | VUnit -> None

(** Extract a real from a value *)
let value_as_real = function
  | VReal r -> Some r
  | VInteger _ | VBoolean _ | VUnit -> None

(** Extract a unit from value *)
let value_as_unit = function
  | VUnit -> Some ()
  | VBoolean _ | VInteger _ | VReal _ -> None

(** Extract a boolean from a read-only computation *)
let ro_as_boolean (RO v) = value_as_boolean v

(** Extract an integer from a read-only computation *)
let ro_as_integer (RO v) = value_as_integer v

(** Extract an integer from a read-only computation *)
let ro_as_real (RO v) = value_as_real v

(** Extract an unit from a read-only computation *)
let ro_as_unit (RO v) = value_as_unit v

(** Extract a value from a read-only computation *)
let ro_as_value (RO v) = v

(** Convert the result of a read-only computation to the result of a read-write
    computation.*)
let ro_as_rw (RO v) = RW v

(** Print a value *)
let print_value v ppf =
  match v with
  | VBoolean b -> Format.fprintf ppf "%b" b
  | VInteger k -> Format.fprintf ppf "%t" (fun ppf -> Mpz.print ppf k)
  | VReal r -> Format.fprintf ppf "%s" (Reals.Real.to_string r)
  | VUnit -> Format.fprintf ppf "()"

(** Print the result of a read-write computation *)
let print_result (RW v) ppf = print_value v ppf

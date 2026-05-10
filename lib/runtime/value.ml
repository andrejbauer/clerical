(* Runtime values and computations. *)

(** Value *)
type value =
  | VBoolean of bool
  | VInteger of Mpzf.t
  | VReal of Reals.Real.t
  | VUnit
  | VArray of value array

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
  | VBoolean _ | VReal _ | VUnit | VArray _ -> None

(** Extract a boolean from a value *)
let value_as_boolean = function
  | VBoolean b -> Some b
  | VInteger _ | VReal _ | VUnit | VArray _ -> None

(** Extract a real from a value *)
let value_as_real = function
  | VReal r -> Some r
  | VInteger _ | VBoolean _ | VUnit | VArray _ -> None

(** Extract a unit from value *)
let value_as_unit = function
  | VUnit -> Some ()
  | VBoolean _ | VInteger _ | VReal _ | VArray _ -> None

(** Extract a unit from value *)
let value_as_array = function
  | VArray a -> Some a
  | VBoolean _ | VInteger _ | VReal _ | VUnit -> None

(** Extract a boolean from a read-only computation *)
let ro_as_boolean (RO v) = value_as_boolean v

(** Extract an integer from a read-only computation *)
let ro_as_integer (RO v) = value_as_integer v

(** Extract an integer from a read-only computation *)
let ro_as_real (RO v) = value_as_real v

(** Extract an integer from a read-only computation *)
let ro_as_array (RO v) = value_as_array v

(** Extract an unit from a read-only computation *)
let ro_as_unit (RO v) = value_as_unit v

(** Extract a value from a read-only computation *)
let ro_as_value (RO v) = v

(** Convert the result of a read-only computation to the result of a read-write
    computation.*)
let ro_as_rw (RO v) = RW v

(** Print a value *)
let rec print_value v ppf =
  match v with
  | VBoolean b -> Format.fprintf ppf "%b" b
  | VInteger k -> Format.fprintf ppf "%t" (fun ppf -> Mpz.print ppf k)
  | VReal r -> Format.fprintf ppf "%s" (Reals.Real.to_string r)
  | VUnit -> Format.fprintf ppf "()"
  | VArray vs -> Format.fprintf ppf "[%t]" (print_array vs)

and print_array vs ppf =
  let last = Array.length vs - 1 in
  (* TODO: use formatting boxes to control line breaks better globally *)
  Format.pp_open_box ppf 120;
  Array.iteri
    (fun i v ->
      if i < last then Format.fprintf ppf "%t,@ " (print_value v)
      else Format.fprintf ppf "%t" (print_value v))
    vs

(** Print the result of a read-write computation *)
let print_result (RW v) ppf = print_value v ppf

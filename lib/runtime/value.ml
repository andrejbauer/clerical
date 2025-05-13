(** Runtime values. *)

(** Value *)
type value =
  | VBoolean of bool
  | VInteger of Mpzf.t
  | VReal of Reals.Real.t
  | VUnit

(** A value that is computed in parallel. *)
type value_promise = value Parallel.promise

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

(** [awaiting f p] awaits the promise [p] to obtain a value, then applies [f] to the value *)
let awaiting f p =
  let v = Parallel.await p in
  f v

(** Print a value *)
let print_value v ppf =
  match v with
  | VBoolean b -> Format.fprintf ppf "%b" b
  | VInteger k -> Format.fprintf ppf "%t" (fun ppf -> Mpz.print ppf k)
  | VReal r -> Format.fprintf ppf "%s" (Reals.Real.to_string r)
  | VUnit -> Format.fprintf ppf "()"

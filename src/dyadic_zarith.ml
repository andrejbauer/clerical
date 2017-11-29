(** Dyadic numbers with [Z] *)

(* Dyadic numbers with the Ocaml [Z] package. This is a lot slower than
   [Dyadic_mpfr] but is independent of any third-party libraries.

   The [Dyadic_mpfr] package measures output preceision with bits of mantissa [prec], and
   uses rounding modes. Here dyadics are presented as a pair (m,e) = m*2^e. Precision is
   implicitly hidden in the number of bits of m. Not to be confused with the more usual
   representation with 1.m * 2^e. When no precision is given the calculations are "exact"
   (i.e. based on previous precision).
*)
type t =
  | NegInf
  | Dyadic of Z.t * int
  | PosInf
  | NaN

(** Rounding modes *)

type rounding_mode = Down | Up

let down = Down

let up = Up

(** Invert the rounding mode.*)
let anti = function
  | Down -> Up
  | Up -> Down

(** Rounding *)

let get_prec x = Z.numbits x

let rounddir = function
  | Some Down | None -> fun x->x
  | Some Up -> Z.neg

let round_dyadic ?prec ~round (m,e) =
  match prec with
    | None -> m,e
    | Some p0 ->
      let p1 = get_prec m in
      (if p1 <= p0 then Z.shift_left m (p0-p1)
      else
	match round with
	| Down -> Z.shift_right m (p1-p0)
	| Up -> Z.neg (Z.shift_right (Z.neg m) (p1-p0))
      ), e-(p0-p1)

(** Constructors *)

let dyadic (m,e) = Dyadic (m,e)

let of_int ?prec ~round k = dyadic (round_dyadic ?prec ~round (Z.of_int k, 0))

(** Big integer of type [Z] to dyadic. *)
let of_integer ?prec ~round k = dyadic (round_dyadic ?prec ~round (k, 0))

let pow2 = function
 | Dyadic(m,e) -> Dyadic (m,e+1)
 | a -> a

let make ?prec ~round m e =
  dyadic (round_dyadic ?prec ~round (m,e))

let make_int ?prec ~round m e =
  make ?prec ~round (Z.of_int m) e

let get_exp = function
  | Dyadic (_, e) -> e
  | NaN | PosInf | NegInf -> 0

(** Constants *)

let zero = of_int ~round:Down 0

let one = of_int ~round:Down 1

let negative_one = of_int ~round:Down (-1)

let two = of_int ~round:Down 2

let half ?prec ~round = make_int ?prec ~round 1 (-1)

(** Order *)

let nan_exc = Invalid_argument "Dyadic_num: nan encountered"

let common_base (m1, e1) (m2, e2) =
  let c = e1 - e2 in
    if c < 0 then (m1, Z.shift_left m2 (-c), e1)
    else if c > 0 then (Z.shift_left m1 c, m2, e2)
    else (m1, m2, e1)

let cmp a b =
  match a, b with
    | NaN, _ | _, NaN -> raise nan_exc
    | PosInf, PosInf | NegInf, NegInf -> `equal
    | NegInf, _ | _, PosInf -> `less
    | PosInf, _ | _, NegInf -> `greater
    | Dyadic (m1,e1), Dyadic (m2,e2) ->
       let (m1, m2, _) = common_base (m1,e1) (m2,e2) in  (*optimize*)
         let c = Z.compare m1 m2 in
            if c<0 then `less
            else if c>0 then `greater
            else `equal

let max x y =
  if cmp x y = `greater then x else y

let min x y =
  if cmp x y = `less then x else y


let sgn = function
  | NaN -> raise nan_exc
  | NegInf -> `negative
  | PosInf -> `positive
  | Dyadic (m,_) ->
     let s = Z.sign m in
	if s < 0 then `negative
	else if s > 0 then `positive
	else `zero

(** strictly less *)
let lt a b = (cmp a b = `less)

(** strictly greater *)
let gt a b = (cmp a b = `greater)

(** equal *)
let eq a b = (cmp a b = `equal)

(** less or equal *)
let leq a b = (cmp a b <> `greater)

(** greater or equal *)
let geq a b = (cmp a b <> `less)

let is_zero a = (sgn a = `zero)

let negative a = (sgn a = `negative)

let positive a = (sgn a = `positive)

let nonpositive a = (sgn a <> `positive)

let nonnegative a = (sgn a <> `negative)

(** Special values *)

let positive_infinity = PosInf

let negative_infinity = NegInf

(** Depending on the rounding mode, return negative or positive infinity *)
let infinity = function
  | Down -> negative_infinity
  | Up -> positive_infinity

let is_infinity = function
  | NegInf | PosInf -> true
  | Dyadic _ -> false
  | NaN -> false

let is_positive_infinity a = (a = PosInf)

let is_negative_infinity a = (a = NegInf)

let is_number = function
  | Dyadic _ -> true
  | PosInf | NegInf | NaN -> false

let is_nan a = (a = NaN)

let classify = function
  | NegInf -> `negative_infinity
  | Dyadic _ -> `number
  | PosInf -> `positive_infinity
  | NaN -> `nan

(** Arithmetic *)

(** Arithmetical operations need to take care of infinite operands when
   the result would be [nan] (not a number). *)

(** Addition. *)
let add ?prec ~round a b =
  match a, b with
    | (NaN, _) | (_, NaN)
    | NegInf, PosInf | PosInf, NegInf -> infinity round
    | NegInf, _ | _, NegInf -> NegInf
    | _, PosInf | PosInf, _ -> PosInf
    | Dyadic (m1,e1), Dyadic (m2,e2) ->
      let (m1, m2, e) = common_base (m1,e1) (m2,e2) in
	dyadic (round_dyadic ?prec ~round (Z.add m1 m2, e))

(** Subtraction. *)
let sub ?prec ~round a b =
  match a, b with
    | NaN, _ | _, NaN | NegInf, NegInf | PosInf, PosInf -> infinity round
    | NegInf, _ | _, PosInf -> NegInf
    | PosInf, _ | _, NegInf -> PosInf
    | Dyadic (m1,e1), Dyadic (m2,e2) ->
      let (m1, m2, e) = common_base (m1,e1) (m2,e2) in
	dyadic (round_dyadic ?prec ~round (Z.sub m1 m2, e))

(** Negation. *)
let neg ?prec ~round = function
  | NaN -> infinity round
  | NegInf -> PosInf
  | PosInf -> NegInf
  | Dyadic (m,e) ->
      dyadic (round_dyadic ?prec ~round (Z.neg m, e))

(** Multiplication. Special cases: ±∞ * 0 and 0 * ±∞ *)
let mul ?prec ~round a b =
  match a, b with
    | NaN, _ | _, NaN -> infinity round
    | NegInf, NegInf -> PosInf
    | NegInf, Dyadic (m,e)
    | Dyadic (m,e), NegInf ->
	(let s = Z.sign m in
	   if s < 0 then PosInf
	   else if s > 0 then NegInf
	   else infinity round)
    | NegInf, PosInf | PosInf, NegInf -> NegInf
    | Dyadic (m1,e1), Dyadic (m2,e2) ->
	dyadic (round_dyadic ?prec ~round (Z.mul m1 m2, e1+e2))
    | Dyadic (m,e), PosInf
    | PosInf, Dyadic (m,e) ->
	(let s = Z.sign m in
	   if s < 0 then NegInf
	   else if s > 0 then PosInf
	   else infinity round)
    | PosInf, PosInf -> PosInf

(** Powers with non-negative exponents. *)
let pow ?prec ~round a k =
  match a with
    | NaN -> infinity round
    | NegInf ->
	if k mod 2 = 0 then PosInf else NegInf
    | Dyadic (m,e) -> dyadic (round_dyadic ?prec ~round
                                (Z.pow m k, e*k))
    | PosInf -> PosInf

(** Division. *)
let div ~prec ~round a b =
  match a, b with
    | (NaN,_) | (_, NaN)
    | _, (NegInf | PosInf) -> infinity round
    | NegInf, Dyadic (m,e) ->
	(let s = Z.sign m in
	   if s < 0 then PosInf
	   else if s > 0 then NegInf
	   else infinity round)
    | Dyadic (m1,e1), Dyadic (m2,e2) ->
	if Z.sign m2 = 0 then
	  infinity round
	else
	    let (m1,m2, _) = common_base (m1,e1) (m2,e2) in
	    let k = Pervasives.max (prec - (get_prec m1) + (get_prec m2)) 0 in
	    let m1 = Z.shift_left m1 k in
	    let (q,r) = Z.div_rem m1 m2 in
	    let p1 = (match round with
		| Up -> Z.sign r * Z.sign m2
		| Down -> 0
	    ) in
	      dyadic (Z.add (Z.of_int p1) q, -k)
	      (*dyadic (round_dyadic ~prec ~round (add_int p1 q, -k))*)
    | PosInf, Dyadic (m,e) ->
	(let s = Z.sign m in
	   if s < 0 then NegInf
	   else if s > 0 then PosInf
	   else infinity round)

(** Inverse. *)
let inv ~prec ~round = function
  | NaN
  | NegInf
  | PosInf -> infinity round
  | Dyadic (m,e) ->
      if Z.sign m = 0 then
	infinity round
      else
	div ~prec ~round one (Dyadic(m,e))

(** Shift by a power of two. *)
let shift ?prec ~round a k =
  match a with
    | Dyadic (m,e) -> dyadic (round_dyadic ?prec ~round (m,e+k))
    | a -> a

let halve ?prec ~round a = shift ?prec ~round a (-1)

let double ?prec ~round a = shift ?prec ~round a 1

(** [average a b] returns a dyadic which is guaranteed to be strictly between [a] and [b],
   close to the average. This only works for finite [a] and [b]. *)
let average a b =
  match a, b with
    | NaN, _ | _, NaN  | NegInf, PosInf | PosInf, NegInf -> NaN
    | PosInf, _ | _, PosInf -> PosInf
    | NegInf, _ | _, NegInf -> NegInf
    | Dyadic (m1,e1), Dyadic(m2,e2) ->
	 add ~round:Down a (halve ~round:Down (sub ~round:Down b a))

(** String conversions *)

let starts_with sub s =
  let lsub = String.length sub in
  let l = String.length s in
  if lsub > l || String.compare (String.sub s 0 lsub) sub != 0  then (false,s)
  else (true, String.sub s lsub (l-lsub))

let split s c =
   if (String.contains s c) then
      let i = String.index s c in
      let l = String.length s in
          (i+1-String.length s, String.sub s 0 i, String.sub s (i+1) (l-i-1))
   else (0,s,"")

let from_base10 ~prec ~round m10 e10 =
  let p10 = dyadic (Z.pow (Z.of_int 10) (abs e10), 0) in
  if e10 >= 0 then
    mul ~round:round ~prec:prec (dyadic (m10, 0)) p10
  else
    div ~round:round ~prec:prec (dyadic (m10, 0)) p10

let to_base10 ~round m e =
    let e10 = int_of_float (floor ((float_of_int e) *. (log 2.) /. (log 10.))) - 1 in
    let p10 = Z.pow (Z.of_int 10) (abs e10) in
    let m10 = m in
    let m10 = if e > 0 then Z.shift_left m10 e else m10 in
    let m10 = if e10 < 0 then Z.mul m10 p10 else m10 in
    let m10 = if e < 0 then Z.shift_right m10 (-e) else m10 in
    let m10 = if e10 > 0 then Z.div m10 p10 else m10 in
    match round with
    | Down -> (m10, e10)
    | Up -> (Z.add m10 Z.one, e10)

let of_string ~prec ~round str =
   let str = String.lowercase_ascii str in
   let (hex, str) = starts_with "0x" str in
   let (_, m, e) = split str 'e' in
   let e = if String.length e = 0 then 0 else int_of_string e in
   let (em, m1, m2) = split m '.' in
   let m10 = Z.of_string (m1 ^ m2)
   and e10 = em + e in
   from_base10 ~round ~prec m10 e10

let to_string2 = function
  | NaN -> "nan"
  | PosInf -> "inf"
  | NegInf -> "-inf"
  | Dyadic (m,e) ->
      (Z.to_string m) ^ "p" ^ (string_of_int e)

let string_insert a pos b =
  (String.sub a 0 pos) ^ b ^ (String.sub a pos (String.length a - pos))

let to_string ~round x =
  (* let exp_notation = 4 in *)
  match x with
  | NaN -> "nan"
  | PosInf -> "inf"
  | NegInf -> "-inf"
  | Dyadic (m,e) ->
     let (m10, e10) = to_base10 ~round m  e in
     Z.to_string m10 ^ "E" ^ string_of_int e10
     (* let s = Z.to_string (Z.abs m10) in
      * let e10 = e10 + String.length s10 in
      * let sign = if Z.sign m < 0 then "-" else "" in
      * if e > String.length s || e < - exp_notation then
      *   sign ^ string_insert s 1 "." ^ "e" ^ string_of_int (e - 1)
      * else if e > 0 then
      *   sign ^ string_insert s e "."
      * else
      *   sign ^ "0." ^ String.make (-e) '0' ^ s *)

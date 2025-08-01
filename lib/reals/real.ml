(** Interval arithmetic *)

(** An interval is a pair [(l,u)] of [Dyadic] numbers. There is no restriction
    that [l] should be smaller than [u], i.e., the library also works with
    back-to-front intervals. It uses Kaucher multiplication for back-to-front
    intervals. Infinity and negative infinity are allowed as endpoints.

    Note: we broke back-to-front intervals in the following functions:
    abs
*)


let down = Dyadic.down
let up = Dyadic.up
let anti = Dyadic.anti

type t = { lower : Dyadic.t; upper : Dyadic.t }
(** An interval is a record with fields [lower] and [upper]. *)

(** Basic mainpulation *)

(** [make l u] constructs an interal from two given dyadics. *)
let make l u = { lower = l; upper = u }

(** [lower i] returns the lower endpoint. *)
let lower i = i.lower

(** [upper i] returns the upper endpoint. *)
let upper i = i.upper

(** [flip i] exchanges the lower and upper endpoints. *)
let flip i = { lower = i.upper; upper = i.lower }

(** Compute the width of the interval *)
let width ~prec ~round i = Dyadic.sub ~prec ~round (upper i) (lower i)

let of_dyadic q = make q q
let bottom = make Dyadic.negative_infinity Dyadic.positive_infinity
let top = make Dyadic.positive_infinity Dyadic.negative_infinity

(** Testing for infinite endpoints and back-to-front. *)

(** [forward i] returns [true] if [i] is a front-to-back interval *)
let forward i = Dyadic.leq (lower i) (upper i)

(** [backward i] returns [true] if [i] is a back-to-front interval *)
let backward i = Dyadic.gt (lower i) (upper i)

(** [proper i] returns [true] if [i] is a front-to-back interval with finite
    endpoints. *)
let proper i =
  forward i && Dyadic.is_number (lower i) && Dyadic.is_number (upper i)

(** String conversion *)

let to_string i =
  "["
  ^ Dyadic.to_string ~round:Dyadic.down (lower i)
  ^ ","
  ^ Dyadic.to_string ~round:Dyadic.up (upper i)
  ^ "]"

(** Arithmetic *)

(** Results are computed to precision [prec] and rounded according to [round].
    If [round] is [Dyadic.down] then the result approximates the true value from
    below. If [round] is [Dyadic.up] then the true value is approximated from
    above. It is perhaps more customary to always approximate the true value
    from below, but we need the other approximant for Kaucher intervals. *)

let add ~prec ~round i j =
  let dnuor = Dyadic.anti round in
  {
    lower = Dyadic.add ~prec ~round (lower i) (lower j);
    upper = Dyadic.add ~prec ~round:dnuor (upper i) (upper j);
  }

let sub ~prec ~round i j =
  let dnuor = Dyadic.anti round in
  {
    lower = Dyadic.sub ~prec ~round (lower i) (upper j);
    upper = Dyadic.sub ~prec ~round:dnuor (upper i) (lower j);
  }

let neg ~prec ~round i =
  let dnuor = Dyadic.anti round in
  {
    lower = Dyadic.neg ~prec ~round (upper i);
    upper = Dyadic.neg ~prec ~round:dnuor (lower i);
  }

(* TODO: We should fix this one to work with Kaucher intervals *)
let abs ~prec ~round i =
  if Mpfr.sgn (lower i) >= 0 then
    i
  else if Mpfr.sgn (upper i) <= 0 then
    neg ~prec ~round i
  else
    (* TODO: We are relying on MFPR not changing the sign when rounding to a lower precision. *)
    let l = Dyadic.neg ~prec ~round (lower i) in
    {
      lower = Dyadic.zero;
      upper = Dyadic.max l (upper i)
    }

(** Kaucher multiplication of intervals is given by the following table.

     \begin{center}
     \begin{tabular}{|c|c|c|c|c|}
     \hline
     $[a,b] \times [c,d]$
     & $a, b \leq 0$ & $a \leq 0 \leq b$  & $b \leq 0 \leq a$  & $0 \leq a,b$ \\ \hline
     $ 0 \leq c, d$ & $[ad,bc]$ &  $[ad,bd]$ &  $[ac,bc]$ &  $[ac,bd]$ \\ \hline
     $ d \leq 0 \leq c$ & $[bd,bc]$ &   $[0,0]$ &    $[q,p]$  &  $[ac,ad]$ \\ \hline
     $ c \leq 0 \leq d$ & $[ad,ac]$ &   $[p,q]$ &    $[0,0]$  &  $[bc,bd]$ \\ \hline
     $ c, d \leq 0$ & $[bd,ac]$ &  $[bc,ac]$ &  $[bd,ad]$ &  $[bc,ad]$ \\ \hline
     \end{tabular}
     \end{center}

     Where $p = \min(ad,bc) \leq 0$ and $q = \max(ac,bd) \geq 0$.
 *)

let mul ~prec ~round i j =
  let negative = Dyadic.negative in
  {
    lower =
      (let lmul = Dyadic.mul ~prec ~round in
       let a = lower i in
       let b = upper i in
       let c = lower j in
       let d = upper j in
       if negative a then
         if negative b then if negative d then lmul b d else lmul a d
         else if
           (* positive [b] *)
           negative c
         then if negative d then lmul b c else Dyadic.min (lmul a d) (lmul b c)
         else if
           (* positive [c] *)
           negative d
         then Dyadic.zero
         else lmul a d
       else if
         (* positive [a] *)
         negative b
       then
         if negative c then if negative d then lmul b d else Dyadic.zero
         else if
           (* positive [c] *)
           negative d
         then Dyadic.max (lmul a c) (lmul b d)
         else lmul a c
       else if
         (* positive [b] *)
         negative c
       then lmul b c
       else lmul a c);
    upper =
      (let umul = Dyadic.mul ~prec ~round:(Dyadic.anti round) in
       let a = lower i in
       let b = upper i in
       let c = lower j in
       let d = upper j in
       if negative a then
         if negative b then if negative c then umul a c else umul b c
         else if
           (* positive [b] *)
           negative c
         then if negative d then umul a c else Dyadic.max (umul a c) (umul b d)
         else if
           (* positive [c] *)
           negative d
         then Dyadic.zero
         else umul b d
       else if
         (* positive [a] *)
         negative b
       then
         if negative c then if negative d then umul a d else Dyadic.zero
         else if
           (* positive [c] *)
           negative d
         then Dyadic.min (umul a d) (umul b c)
         else umul b c
       else if
         (* positive [b] *)
         negative d
       then umul a d
       else umul b d);
  }

(** Power by non-negative exponent. *)

let pow ~prec ~round i k =
  let dnuor = Dyadic.anti round in
  if k mod 2 = 1 then
    {
      lower = Dyadic.pow ~prec ~round (lower i) k;
      upper = Dyadic.pow ~prec ~round:dnuor (upper i) k;
    }
  else
    let a = lower i in
    let b = upper i in
    {
      lower =
        (let lpow = Dyadic.pow ~prec ~round in
         if Dyadic.negative a then
           if Dyadic.negative b then lpow b k
           else (* non-negative [b] *)
             Dyadic.zero
         else if
           (* non-negative [a] *)
           Dyadic.negative b
         then Dyadic.max (lpow a k) (lpow b k)
         else (* non-negative [b] *)
           lpow a k);
      upper =
        (let upow = Dyadic.pow ~prec ~round in
         if Dyadic.negative a then
           if Dyadic.negative b then upow a k
           else (* non-negative [b] *)
             Dyadic.max (upow a k) (upow b k)
         else if
           (* non-negative [a] *)
           Dyadic.negative b
         then Dyadic.zero
         else (* non-negative [b] *)
           upow b k);
    }

let inv ~prec ~round i =
  let a = lower i in
  let b = upper i in
  {
    lower =
      (let linv = Dyadic.inv ~prec ~round in
       match (Dyadic.sgn a, Dyadic.sgn b) with
       | `negative, `negative -> linv b
       | `zero, `negative -> linv b
       | `positive, `negative -> Dyadic.positive_infinity
       | `negative, `zero -> Dyadic.negative_infinity
       | `zero, `zero -> Dyadic.negative_infinity
       | `positive, `zero -> Dyadic.positive_infinity
       | `negative, `positive -> Dyadic.negative_infinity
       | `zero, `positive -> Dyadic.negative_infinity
       | `positive, `positive -> linv b);
    upper =
      (let uinv = Dyadic.inv ~prec ~round:(Dyadic.anti round) in
       match (Dyadic.sgn a, Dyadic.sgn b) with
       | `negative, `negative -> uinv a
       | `zero, `negative -> Dyadic.negative_infinity
       | `positive, `negative -> Dyadic.negative_infinity
       | `negative, `zero -> Dyadic.positive_infinity
       | `zero, `zero -> Dyadic.positive_infinity
       | `positive, `zero -> uinv a
       | `negative, `positive -> Dyadic.positive_infinity
       | `zero, `positive -> Dyadic.positive_infinity
       | `positive, `positive -> uinv a);
  }

let div ~prec ~round i j = mul ~prec ~round i (inv ~prec ~round j)

(*let exp ~prec ~round i =
    let dnuor = Dyadic.anti round in
    { lower = lazy (Dyadic.exp prec round (lower i)) ;
    upper = lazy (Dyadic.exp prec dnuor (upper i)) }*)

(** Interval splitting *)

(** [midpoint prec i] computes the midpoint of an interval [i]. It guarantees
    that the point is actually inside the interval (which means that it will use
    precision higher than [prec] if necessary. It works correctly for infinite
    and back-to-front intervals. For infinite intervals it goes closer to
    infinity as [prec] increases. *)

let midpoint ~prec k i =
  let a = lower i in
  let b = upper i in
  match (Dyadic.classify a, Dyadic.classify b) with
  | `number, `number -> Dyadic.average a b
  | `negative_infinity, `positive_infinity
  | `positive_infinity, `negative_infinity ->
      Dyadic.zero
  | `negative_infinity, `number ->
      if Dyadic.leq b Dyadic.negative_one then
        Dyadic.shift ~prec ~round:Dyadic.down b k
      else Dyadic.negative_one
  | `positive_infinity, `number ->
      if Dyadic.geq b Dyadic.one then Dyadic.shift ~prec ~round:Dyadic.up b k
      else Dyadic.one
  | `number, `positive_infinity ->
      if Dyadic.geq a Dyadic.one then Dyadic.shift ~prec ~round:Dyadic.up a k
      else Dyadic.one
  | `number, `negative_infinity ->
      if Dyadic.leq a Dyadic.negative_one then
        Dyadic.shift ~prec ~round:Dyadic.down a k
      else Dyadic.negative_one
  | _ -> raise (Invalid_argument ("Interval.midpoint: " ^ to_string i))

(** Split an interval into two smaller ones. *)

let split ~prec k i =
  let m = midpoint ~prec k i in
  ({ lower = i.lower; upper = m }, { lower = m; upper = i.upper })

(** [thirds prec i] computes points [m1] and [m2] which divide [i] into three
    roughly equal parts. If [i] is infinite it does a reasonable thing. *)

let thirds ~prec k i =
  let i1, i2 = split ~prec k i in
  (midpoint ~prec k i1, midpoint ~prec k i2)

(** Definitions of external function. *)

type entry =
    prec:Runtime.precision -> Value.value list -> Value.result

(** Helper functions for making external functions.
    The suffix X..ZT means that the inputs are of types
    X, ..., Z and the ouyput of type T, where I stands for
    integers and B for booleans. *)

let make_BB s f =
  s,
  begin
    fun ~prec ->
    function
    | [v] ->
       begin match Value.value_as_boolean v with
       | Some b -> Value.CBoolean (f b)
       | None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
       end
    | [] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_IR s f =
  s,
  begin
    fun ~prec ->
    function
    | [v] ->
       begin match Value.value_as_integer v with
       | Some k -> Value.CReal (f ~prec k)
       | None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
       end
    | [] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end


let make_RR s f =
  s,
  begin
    fun ~prec ->
    function
      | [v] ->
         begin match Value.value_as_real v  with
         | Some r -> Value.CReal (f ~prec r)
         | None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
         end
      | [] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_III s f =
  s,
  begin
    fun ~prec ->
    function
    | [v] ->
       begin match Value.value_as_integer v with
       | Some k -> Value.CReal (f ~prec k)
       | None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
       end
    | [] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_III s f =
  s,
  begin
    fun ~prec ->
    function
    | [v1; v2] ->
         begin match Value.value_as_integer v1, Value.value_as_integer v2 with
         | Some k1, Some k2 -> Value.CInteger (f k1 k2)
         | None, _ | _, None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
         end
      | [] | [_] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_IIB s f =
  s,
  begin
    fun ~prec ->
    function
      | [v1; v2] ->
         begin match Value.value_as_integer v1, Value.value_as_integer v2 with
         | Some k1, Some k2 -> Value.CBoolean (f k1 k2)
         | None, _ | _, None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
         end
      | [] | [_] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_RRR s f =
  s,
  begin
    fun ~prec ->
    function
      | [v1; v2] ->
         begin match Value.value_as_real v1, Value.value_as_real v2 with
         | Some r1, Some r2 -> Value.CReal (f ~prec r1 r2)
         | None, _ | _, None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
         end
      | [] | [_] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_RRB s f =
  s,
  begin
    fun ~prec ->
    function
      | [v1; v2] ->
         begin match Value.value_as_real v1, Value.value_as_real v2 with
         | Some r1, Some r2 -> Value.CBoolean (f ~prec r1 r2)
         | None, _ | _, None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
         end
      | [] | [_] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let make_BBB s f =
  s,
  begin
    fun ~prec ->
    function
      | [v1; v2] ->
         begin match Value.value_as_boolean v1, Value.value_as_integer v2 with
         | Some b1, Some b2 -> Value.CBoolean (f b1 b2)
         | None, _ | _, None -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
         end
      | [] | [_] | _ :: _ :: _ -> Runtime.error ~loc:Location.nowhere (Runtime.InvalidExternal s)
  end

let real_cmp r1 r2 =
  if Dyadic.lt (Real.upper r1) (Real.lower r2)
  then (-1)
  else if Dyadic.lt (Real.upper r2) (Real.lower r1)
  then 1
  else raise Runtime.Abort

let two : Mpfr.t = snd (Mpfr.init_set_d 2.0 Mpfr.Near)
let half : Mpfr.t = snd (Mpfr.init_set_d 0.5 Mpfr.Near)

(* Shift [k] by [j] places to the left (or right if [j] is negative. *)
let shift k j =
  let m = Mpz.init () in
  let j = Mpz.get_int j in
  Mpz.mul_2exp m k j ;
  Mpzf._mpzf m

let externals : (string * entry) list = [
  make_III "+" (fun k1 k2 -> Mpzf.add k1 k2) ;
  make_III "-" (fun k1 k2 -> Mpzf.sub k1 k2) ;
  make_III "*" (fun k1 k2 -> Mpzf.mul k1 k2) ;
  make_IIB "<" (fun k1 k2 -> Mpzf.cmp k1 k2 < 0) ;
  make_IIB ">" (fun k1 k2 -> Mpzf.cmp k1 k2 > 0) ;
  make_IIB "<=" (fun k1 k2 -> Mpzf.cmp k1 k2 <= 0) ;
  make_IIB ">=" (fun k1 k2 -> Mpzf.cmp k1 k2 >= 0) ;
  make_IIB "<>" (fun k1 k2 -> Mpzf.cmp k1 k2 <> 0) ;
  make_IIB "==" (fun k1 k2 -> Mpzf.cmp k1 k2 = 0) ;
  make_BB  "not" (fun b -> not b) ;
  make_RRR "+." (fun ~prec r1 r2 -> Real.add ~prec ~round:Real.down r1 r2);
  make_RRR "-." (fun ~prec r1 r2 -> Real.sub ~prec ~round:Real.down r1 r2);
  make_RRR "*." (fun ~prec r1 r2 -> Real.mul ~prec ~round:Real.down r1 r2);
  make_RRR "/." (fun ~prec r1 r2 -> Real.div ~prec ~round:Real.down r1 r2);
  make_RRB "<." (fun ~prec r1 r2 -> real_cmp r1 r2 < 0);
  make_RRB ">." (fun ~prec r1 r2 -> real_cmp r1 r2 > 0);
  make_III "shift" shift
]

let lookup s =
  try
    Some (List.assoc s externals)
  with
  | Not_found -> None

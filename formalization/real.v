Require Import ZArith.
Open Scope Z_scope.

CoInductive Real : Type :=
  | Better : Z -> Real -> Real.


Fixpoint pwer (p : nat) : nat :=
  match p with 
  | O => 1
  | S n' => pwer (n')*2
  end.
Definition inj (z : Z) (n : nat) := ((Z.of_nat (pwer n)) * z).
CoFixpoint injR (z : Z) (n : nat) : Real := Better (inj z n) (injR z (S n)).
Definition ZtoR (z : Z) := injR z O.


Definition roundupdown_4 (z : Z) : Z :=
  if (Z.geb (Zmod z 4) 2) then (Zdiv z 4) + 1 else (Zdiv z 4).

CoFixpoint plus (x y : Real) : Real :=
  match x, y with
  | Better z1 t1, Better z2 t2 => 
    match t1, t2 with 
    | Better z1' t1', Better z2' t2' =>
      match t1', t2' with 
      | Better z1'' t1'', Better z2'' t2'' => Better (roundupdown_4 (z1'' + z2'')) (plus t1 t2)
      end
    end
  end.

Require Import List.
Fixpoint get_first_n (r:Real) (n : nat) : list Z :=
  match n with 
  | O => nil
  | S n => match r with 
            | Better z r => (z::get_first_n r n)
            end
  end.


(* Delay monad. *)
CoInductive lift (A : Type) :=
| Now : A -> lift A
| Later : lift A -> lift A.


Arguments Later {_}.
Arguments Now {_}.

CoFixpoint less (x y : Real) : lift bool :=
  match x, y with
  | Better z1 t1, Better z2 t2 => 
    if (Z.gtb (z1 - 1)  (z2 + 1)) then (Now false) else
      (if (Z.gtb (z2 - 1) (z1 + 1)) then (Now true) else (Later (less t1 t2)))
  end.



Definition x : Real := ZtoR 3.
Definition y : Real := ZtoR 6.
Definition lazybool : lift bool := less x y.

Fixpoint booleval (b : lift bool) (n : nat) : option bool := 
  match b with 
  | Now v => Some v
  | Later b' =>
    match n with 
    | O => None
    | S n => booleval b' n
    end
  end.

Fixpoint chooseEval (b1 b2 : lift bool) (n : nat) : option bool :=
  match b1, b2 with
  | Now v1, Now v2 => match v1, v2 with 
                      | false, false => None
                      | false, true => Some true
                      | true, false => Some false
                      | true, true => Some true
                      end
  | Now v1, Later b2 => match v1 with
                        | true => Some false
                        | false => match n with 
                                    | O => None
                                    | S n => chooseEval b1 b2 n
                                    end
                        end
  | Later b1, Now v2 => match v2 with
                        | true => Some true
                        | false => match n with
                                    | O => None
                                    | S n => chooseEval b1 b2 n
                                    end
                        end
  | Later b1, Later b2 => match n with
                          | O => None
                          | S n => chooseEval b1 b2 n
                          end
  end.

Eval compute in (chooseEval (less x x) (less y x) 30).

  

Eval compute in (booleval (less (plus y y) (plus x (plus x x))) 10).



CoFixpoint bind {A B : Type} (x : lift A) (f : A -> lift B) :=
  match x with
  | Now a => f a
  | Later x' => Later (bind x' f)
  end.

(* Unfold n steps of x, stop if you get a Now *)
Fixpoint fuel {A : Type} (x : lift A) (n : nat) : option A :=
  match x with
  | Now v => Some v
  | Later x' =>
    match n with
    | 0 => None
    | S n => fuel x' n
    end
  end.

(* Use the step function to evaluate command c in environment \u03b7, give a delayed result. *)
CoFixpoint run (\u03b7 : env) (c : comm) : lift result :=
  match step \u03b7 c with
  | inl r => Now r
  | inr (\u03b8, c') => Later (run \u03b8 c')
  end.
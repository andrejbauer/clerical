(* Delay monad. *)
CoInductive lift (A : Type) :=
  | Now : A -> lift A
  | Error : lift A
  | Later : lift A -> lift A.

Arguments Later {_}.
Arguments Error {_}.
Arguments Now {_}.

CoFixpoint bottom {A : Type} := @Later A bottom.

Definition raise {A B : Type} (f : A -> B) : A -> lift B :=
  fun x => Now (f x).

Definition raise2 {A B C : Type} (f : A -> B -> C) : A -> B -> lift C :=
  fun x y => Now (f x y).

CoFixpoint bind {A B : Type} (x : lift A) (f : A -> lift B) :=
  match x with
  | Now a => f a
  | Error => Error
  | Later x' => Later (bind x' f)
  end.

CoFixpoint bind2 {A B C : Type} (x : lift A) (y : lift B) (f : A -> B -> lift C) :=
  match x, y with
  | Now a, Now b => f a b
  | Error, _ => Error
  | _, Error => Error
  | Now a, Later y' => bind y' (fun y => f a y)
  | Later x', Now b => bind x' (fun x => f x b)
  | Later x', Later y' => Later (bind2 x' y' f)
  end.

CoFixpoint take_while {A B : Type} (x : lift (A + B)) (f : A -> lift (A + B)) : lift B :=
  match x with
  | Now (inl a) => Later (take_while (f a) f)
  | Now (inr b) => Now b
  | Error => Error
  | Later x => Later (take_while x f)
  end.

(** [check b x] checks that [b] is true, then does [x], or it runs forever. *)
CoFixpoint check {A : Type} (b : lift bool) (x : lift A) : lift A :=
  match b with
  | Later b => Later (check b x)
  | Now true => x
  | Now false => bottom
  | Error => Error
  end.

CoFixpoint join {A : Type} (x y : lift A) : lift A :=
  match x, y with
  | Later x, Later y => Later (join x y)
  | Now a, _ => Now a
  | _, Now b => Now b
  | Error, y => y
  | x, Error => x
  end.


(** Multi-valued [case b1 x1 b2 x2] does [x1] or [x2] depending on which guard [b1] or [b2] is true. *)
CoFixpoint mcase {A : Type} (b1 : lift bool) (x1 : lift A) (b2 : lift bool) (x2 : lift A) : lift A :=
  join (check b1 x1) (check b2 x2).

(* Unfold n steps of x, stop if you get a Now *)
Fixpoint fuel {A : Type} (n : nat) (x : lift A) : lift A :=
  match x with
  | Now v => Now v
  | Error => Error
  | Later x' =>
    match n with
    | 0 => x'
    | S n => fuel n x'
    end
  end.

(* Section Examples. *)
(*   Eval compute in *)
(*     fuel 20 (take_while (Now (inl 0)) *)
(*                (fun n => match n with 0 | 1 | 2 | 3 => Later (Now (inl (S n))) | _ => Now (inr (n, false)) end)). *)
(* End Examples. *)


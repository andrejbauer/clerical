(* Just enough of domain theory and Plotkin's powerdomain to get us through. *)

(* Given a set X we formalize Plotkin's powerdomain over the lifting of X.
   This is an ω-CPO G(X) whose elements are inhabited countable subsets
   of X⊥, such that an infinite set necessarily contains ⊥ as well. We need
   a constructive definition that will be workable with Coq.
*)

(* (* The usual "delay" monad. *) *)
(* CoInductive Lift (A : Type) := *)
(*   | Later : Lift A -> Lift A *)
(*   | Now : A -> Lift A. *)

(* Arguments Later {_} _. *)
(* Arguments Now {_} _. *)

CoInductive G (A : Type) :=
  | Join : G A -> G A -> G A
  | Skip : G A -> G A
  | Stop : A -> G A.

Arguments Join {_} _ _.
Arguments Skip {_}.
Arguments Stop {_}.

(* The monad structure. *)

(* The unit of the monad is the constructor [Stop]. *)

(* The monad bind. *)
CoFixpoint bindG {A B : Type} (u : G A) (f : A -> G B) : G B :=
  match u with
  | Join v w => Join (bindG v f) (bindG v f)
  | Skip u => Skip (bindG u f)
  | Stop a => f a
  end.

(* Modal operators. *)

Inductive totally {A : Type} (φ : A -> Prop) : G A -> Prop :=
  | totally_Join : forall S T, totally φ S -> totally φ T -> totally φ (Join S T)
  | totally_Skip : forall S, totally φ S -> totally φ (Skip S)
  | totally_Stop : forall a, φ a -> totally φ (Stop a).

CoInductive partially {A : Type} (φ : A -> Prop) : G A -> Prop :=
  | partially_Join : forall S T, partially φ S -> partially φ T -> partially φ (Join S T)
  | partially_Skip : forall S, partially φ S -> partially φ (Skip S)
  | partially_Stop : forall a, φ a -> partially φ (Stop a).

CoInductive maybe {A : Type} (φ : A -> Prop) : G A -> Prop :=
  | maybe_Join_left : forall S T, maybe φ S -> maybe φ (Join S T)
  | maybe_Join_right : forall S T, maybe φ T -> maybe φ (Join S T)
  | maybe_Skip : forall S, maybe φ S -> maybe φ (Skip S)
  | maybe_Stop : forall a, φ a -> maybe φ (Stop a).

(* The fact that a computation terminates is expressed like this. *)
Definition is_terminating {A : Type} := totally (fun (_ : A) => True).

(* The fact that a computation necessarily diverges is expressed like this. *)
Definition is_diverging {A : Type} := partially (fun (_ : A) => False).

(* The fact that divergence is a possible result (the tree is infinite). *)
Definition maybe_diverging {A : Type} := maybe (fun (_ : A) => False).

(* The non-terminating computation. *)
CoFixpoint bottom {A : Type} : G A := Skip bottom.

(* Total correctness implies partial correctness. *)
Fixpoint total_to_partial {A : Type} (φ : A -> Prop) (u : G A) (D : totally φ u) :
  partially φ u.
Proof.
  induction D ; constructor ; tauto.
Defined.

(* Partial correctness and termination together imply total correctness. *)
Fixpoint partial_to_total {A : Type} (φ : A -> Prop) (u : G A)
         (D : partially φ u) (T : is_terminating u) :
   totally φ u.
Proof.
  induction T ; inversion D ; constructor ; tauto.
Defined.

(* The Egli-Milner order. *)

Definition leq {A : Type} (u v : G A) : Prop :=
  (forall φ : A -> Prop, totally φ u -> totally φ v) /\
  (forall ψ : A -> Prop, partially ψ v -> partially ψ u).

Notation "u ≤ v" := (leq u v) (at level 50, no associativity).

Lemma leq_refl {A : Type} (u : G A) : u ≤ u.
Proof.
  firstorder.
Qed.

Lemma leq_tran {A : Type} (u v w : G A) : u ≤ v -> v ≤ w -> u ≤ v.
Proof.
  firstorder.
Qed.

(* Termination computations are maximal. *)
Lemma total_maximal {A : Type} (u v : G A) :
  is_terminating u -> u ≤ v -> v ≤ u.
Proof.
  intros T [Ltot Lpar].
  split.
  - intros φ H.
    apply partial_to_total.
    + now apply Lpar, total_to_partial.
    + assumption.
  - intros ψ H.
    now apply total_to_partial, Ltot, partial_to_total.
Qed.

Definition is_upper {A : Type} (c : nat -> G A) (v : G A) :=
  forall n, c n ≤ v.

Definition is_sup {A : Type} (c : nat -> G A) (v : G A) :=
  is_upper c v /\ forall w, is_upper c w -> v ≤ w.


(* The following construction is used to give the while loop a meaning. *)
CoFixpoint iterate {A B : Type} (f : A -> G (A + B)) (u : G (A + B)) : G B :=
  match u with
  | Join v w => Join (iterate f v) (iterate f w)
  | Skip v => Skip (iterate f v)
  | Stop (inl a) => Skip (iterate f (f a))
  | Stop (inr b) => Stop b
  end.

(* Support for nondeterministic case.

   We need to arrange the non-deterministic choice in such a way
   that if anywhere in the computation [(true, a)] appears,
   then the result is a terminating computation. It seems necessary
   to have some sort of synchronized parallel search.

   We shall achieve this by serializing the search.
*)
(* CoInductive Enumeration (A : Type) : Type := *)
(*   | Say : A -> Enumeration A -> Enumeration A *)
(*   | Mumble : Enumeration A -> Enumeration A (* Rick Statman taught me to call this one "mumble" *) *)
(*   | Period : Enumeration A. *)

(* Arguments Say {_} _ _. *)
(* Arguments Mumble {_} _. *)
(* Arguments Period {_}. *)

(* CoFixpoint zip {A : Type} (e f : Enumeration A) : Enumeration A := *)
(*   match e, f with *)
(*   | Say a e, f => Say a (zip f e) *)
(*   | Mumble e, f => Mumble (zip f e) *)
(*   | Period, f => f *)
(*   end. *)

(* (* The possible results of a computation are enumerable. *) *)
(* CoFixpoint enumerate {A : Type} (u : G A) : Enumeration A := *)
(*   match u with *)
(*   | Join v w => Mumble (zip (enumerate v) (enumerate w)) *)
(*   | Skip v => Mumble (enumerate v) *)
(*   | Stop a => Say a Period *)
(*   end. *)
(* Just enough of domain theory and Plotkin's powerdomain to get us through. *)

(* Given a set X we formalize Plotkin's powerdomain over the lifting of X.
   This is an ω-CPO G(X) whose elements are inhabited countable subsets
   of X⊥, such that an infinite set necessarily contains ⊥ as well. We need
   a constructive definition that will be workable with Coq.
*)

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

Inductive maybe {A : Type} (φ : A -> Prop) : G A -> Prop :=
  | maybe_Join_left : forall S T, maybe φ S -> maybe φ (Join S T)
  | maybe_Join_right : forall S T, maybe φ T -> maybe φ (Join S T)
  | maybe_Skip : forall S, maybe φ S -> maybe φ (Skip S)
  | maybe_Stop : forall a, φ a -> maybe φ (Stop a).

(* The fact that a computation terminates is expressed like this. *)
Definition is_terminating {A : Type} := totally (fun (_ : A) => True).

(* The fact that a computation necessarily diverges is expressed like this. *)
Definition is_diverging {A : Type} := partially (fun (_ : A) => False).

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

(* Terminating computations are maximal. *)
Lemma terminating_maximal {A : Type} (u v : G A) :
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

(* The following construction is used to give the while loop a meaning. We start with a
   computation [u] which may end with values [inl a] or [inr b]. We replace [inr b] with
   [b], and expand the ones of the form [inl a] into [f a] to get further subcomputations,
   and we keep doing this.

   In the semantics of [while b do c done] we use [inr γ₁] to indicate that the while loop
   is done and [γ₁] is the state of read-write variables it left us with. We use [inl δ]
   to indicate that the loop is not done yet, and [δ] is the current state. The map [f]
   does one iteration of the loop, i.e., [if b then c ; .. else skip]. *)
CoFixpoint iterate {A B : Type} (f : A -> G (A + B)) (u : G (A + B)) : G B :=
  match u with
  | Join v w => Join (iterate f v) (iterate f w)
  | Skip v => Skip (iterate f v)
  | Stop (inl a) => Skip (iterate f (f a))
  | Stop (inr b) => Stop b
  end.

(* The semantics of guarded case is not quite satisfying, but we will deal with this
   later. For now we just define what essentially corresponds to [if b then c else ⊥]. *)
CoFixpoint check {A : Type} (b : G bool) (u : G A) : G A :=
  match b with
  | Join b1 b2 => Join (check b1 u) (check b2 u)
  | Skip b => Skip (check b u)
  | Stop false => bottom
  | Stop true => u
  end.
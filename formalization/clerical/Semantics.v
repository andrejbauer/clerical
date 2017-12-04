Require Import ZArith.
Require Import Clerical.
Require Import Typing.
Require Import Nondeterminism.

Definition sem_datatype (τ : datatype) : Type :=
  match τ with
  | DInteger => Z
  | DBoolean => bool
  end.

Definition sem_result_type (ρ : result_type) :=
  match ρ with
  | RData τ => sem_datatype τ
  | RCommand => unit
  end.

Fixpoint sem_list_datatype (lst : list datatype) : Type :=
  match lst with
  | nil => unit
  | cons t lst => sem_datatype t * sem_list_datatype lst
  end.

Fixpoint update
  {τ : datatype} {Θ : list datatype} (k : nat) (v : sem_datatype τ) (γ : sem_list_datatype Θ)
  (i : is_writable Θ k τ) {struct i} : sem_list_datatype Θ.
Proof.
  induction i.

  (* is_writable_0 *)
  {
    exact (v, snd γ).
  }

  (* is_writable_S *)
  {
    split.
    - exact (fst γ).
    - apply (IHi v (snd γ)).
  }
Defined.

Fixpoint sem_list_list_datatype (lst : list (list datatype)) : Type :=
  match lst with
  | nil => unit
  | cons l lst => sem_list_datatype l * sem_list_list_datatype lst
  end.

Definition sem_ctx (Γ : ctx) : Type :=
  (sem_list_datatype (ctx_rw Γ)) * (sem_list_list_datatype (ctx_ro Γ)).

Definition sem_rw (Γ : ctx) := sem_list_datatype (ctx_rw Γ).

(* Cheap trick to get the a large inductive proof organized. Eventually
   we want to remove this. *)
Axiom magic_axiom : forall A : Type, A. (* every type is inhabited, use with care *)
Ltac unfinished := now apply magic_axiom.

(* The meaning of a well-typed program in relational form. *)
Fixpoint sem_comp (Γ : ctx) (c : comp) (ρ : result_type) (D : has_type Γ c ρ):
  sem_ctx Γ -> G (sem_rw Γ * sem_result_type ρ).

Proof.
  intro γ.
  induction D.

  (* has_type_Var_0 *)
  {
    apply Stop.
    split.
    - exact (fst γ).
    - exact (fst (fst γ)).
  }

  (* has_type_Var_S *)
  {
    pose (u := IHD ((snd (fst γ)), snd γ)).
    apply (bindG u).
    intros [δ s].
    apply Stop.
    split.
    - split.
      + exact (fst (fst γ)).
      + exact (snd (fst γ)).
    - exact s.
  }

  (* has_type_Var_empty_rw *)
  {
    unfold sem_rw, readonly ; simpl.
    unfold sem_rw, readonly in IHD ; simpl in IHD.
    apply (bindG (IHD (snd γ))).
    intros [γ1 t].
    apply Stop.
    exact (tt, t).
  }

  (* has_type_True *)
  {
    apply Stop.
    exact (fst γ, true).
  }

  (* has_type_False *)
  {
    apply Stop.
    exact (fst γ, false).
  }

  (* has_type_Integer *)
  {
    apply Stop.
    exact (fst γ, k).
  }

  (* has_type_Skip *)
  {
    apply Stop.
    exact (fst γ, tt).
  }

  (* has_type_Sequence *)
  {
    simple refine (bindG _ IHD2).
    apply (bindG (IHD1 γ)).
    intros [γ1 []].
    apply Stop.
    exact (γ1, snd γ).
  }

  (* has_type_while *)
  {
    apply (@bindG (sem_rw Γ)).
    - apply (@iterate (sem_ctx Γ) (sem_rw Γ)).
      + intro δ.
        apply (bindG (sem_comp (readonly Γ) b RBoolean D1 (tt, δ))).
        intros [[] [|]].
        * (* condition was true *)
          exact (Stop (inl δ)).
        * (* condition was false *)
          exact (Stop (inr (fst δ))).
      + exact (Stop (inl γ)). 
    - intro γ1.
      exact (Stop (γ1, tt)).
  }

  (* has_type_Case *)
  {
    unfinished.
    (* apply Join. *)
    (* - apply check. *)
    (*   + apply (bindG (IHD1 (tt, γ))). *)
    (*     intros [_ b]. *)
    (*     exact (Stop b). *)
    (*   + apply (bindG (IHD2 γ)). *)
    (*     apply Stop. *)
    (* - apply check. *)
    (*   + apply (bindG (IHD3 (tt, γ))). *)
    (*     intros [_ b]. *)
    (*     exact (Stop b). *)
    (*   + apply (bindG (IHD4 γ)). *)
    (*     apply Stop. *)
  }

  (* has_type_newvar *)
  {
    apply (bindG (IHD1 (tt, γ))).
    intros [[] x].
    apply (bindG (IHD2 ((x, fst γ), snd γ))).
    intros [[_ γ1] y].
    apply Stop.
    exact (γ1, y).
  }

  (* has_type_assign *)
  {
    apply (bindG (sem_comp _ _ _ D (tt, γ))).
    intros [[] val_e].
    apply Stop.
    simple refine (_, tt).
    apply (update k val_e (fst γ) i).
  }

Defined.

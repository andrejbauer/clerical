(** Clerical typing judgments *)

Require Import Clerical.

(* A typing context. *)
Structure ctx := {
  ctx_rw : list datatype ;
  ctx_ro : list (list datatype)
}.

Definition empty_ctx := {| ctx_rw := nil; ctx_ro := nil |}.

Definition extend t Γ :=
  {| ctx_rw := cons t (ctx_rw Γ) ;
     ctx_ro := ctx_ro Γ
  |}.

Definition readonly Γ :=
  {|
    ctx_rw := nil ;
    ctx_ro := cons (ctx_rw Γ) (ctx_ro Γ)
  |}.

Open Scope clerical_scope.

Inductive is_writable : list datatype -> nat -> datatype -> Type :=
  | is_writable_0 :
      forall Γ τ,
        is_writable (cons τ Γ) 0 τ

  | is_writable_S :
      forall Γ τ σ k,
        is_writable Γ k τ ->
        is_writable (cons σ Γ) (S k) τ
.

Inductive has_type : ctx -> comp -> result_type -> Type :=
  | has_type_Var_0 :
      forall Γ τ,
        has_type (extend τ Γ) (VAR 0) (RData τ)

  | has_type_Var_S :
      forall Γ σ τ k,
        has_type Γ (VAR k) (RData τ) ->
        has_type (extend σ Γ) (VAR (S k)) (RData τ)

  | has_type_Var_empty_rw :
      forall Γ τ k,
        has_type Γ (VAR k) (RData τ) ->
        has_type (readonly Γ) (VAR k) (RData τ)

  | has_type_True :
      forall Γ,
        has_type Γ TRUE RBoolean

  | has_type_False :
      forall Γ,
        has_type Γ FALSE RBoolean

  | has_type_Integer :
      forall Γ k,
        has_type Γ (INT k) RInteger

  | has_type_Skip :
      forall Γ,
        has_type Γ SKIP RCommand

  | has_type_Sequence :
      forall Γ c1 c2 ρ,
        has_type Γ c1 RCommand ->
        has_type Γ c2 ρ ->
        has_type Γ (c1 ;; c2)  ρ

  | has_type_while :
      forall Γ b c,
        has_type (readonly Γ) b RBoolean ->
        has_type Γ c RCommand ->
        has_type Γ (WHILE b DO c END) RCommand

  | has_type_Case :
      forall Γ b1 c1 b2 c2 ρ,
        has_type (readonly Γ) b1 RBoolean ->
        has_type Γ c1 ρ ->
        has_type (readonly Γ) b2 RBoolean ->
        has_type Γ c2 ρ ->
        has_type Γ (MCASE b1 ==> c1 OR b2 ==> c2 END) ρ

  | has_type_newvar :
      forall Γ ρ τ e c,
        has_type (readonly Γ) e (RData τ) ->
        has_type (extend τ Γ) c ρ ->
        has_type Γ (NEWVAR e IN c) ρ

  | has_type_assign :
      forall Γ τ k e,
        is_writable (ctx_rw Γ) k τ ->
        has_type (readonly Γ) e (RData τ) ->
        has_type Γ (SET k := e) RCommand
.

(** TODO: a function which takes Γ and c and optionally gives the judgment [has_type Γ c t]
    for some datatype [t]. *)

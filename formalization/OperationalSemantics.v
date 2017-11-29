(* An initial attempt at defining a language for real number comptuations, based on
   converstations with Sewon Park and Alex Simspon *)

Require Import ZArith.
Require Import List.
Require Import Clerical.

Require Import Delay.

(* These are the things that can be stored in a variable. *)
Inductive value :=
  | VBoolean : bool -> value
  | VInteger : Z -> value
  | VUnit : value.

(* Auxiliary functions to extract a value of a given type. *)
Definition get_boolean (x : value) : lift bool :=
  match x with
  | VBoolean b => Now b
  | _ => Error
  end.

Definition get_integer (x : value) : lift Z :=
  match x with
  | VInteger k => Now k
  | _ => Error
  end.

Definition get_unit (x : value) : lift unit :=
  match x with
  | VUnit => Now tt
  | _ => Error
  end.

(* Notation for [Delay.bind]. *)
Notation "x >>= f" := (bind x f) (at level 90, left associativity).

(* Builtin operators. *)

(* First we define auxiliary wrappers, one per type. *)

Definition wrapIII (f : Z -> Z -> Z) :=
  fun u v =>
    match u, v with
    | VInteger k1, VInteger k2 => Now (VInteger (f k1 k2))
    | _, _ => Error
    end.

Definition wrapIIB (f : Z -> Z -> bool) :=
  fun u v =>
    match u, v with
    | VInteger k1, VInteger k2 => Now (VBoolean (f k1 k2))
    | _, _ => Error
    end.

Definition wrapBBB (f : bool -> bool -> bool) :=
  fun u v =>
    match u, v with
    | VBoolean b1, VBoolean b2 => Now (VBoolean (f b1 b2))
    | _, _ => Error
    end.

Definition wrapBB (f : bool -> bool) :=
  fun u =>
    match u with
    | VBoolean b => Now (VBoolean (f b))
    | _ => Error
    end.

(* Definitions of operators. *)
Definition eval_binary_op op :=
  match op with
  | OpPlus => wrapIII Z.add
  | OpLess => wrapIIB Z.ltb
  | OpAnd => wrapBBB andb
  end.

Definition eval_unary_op op :=
  match op with
  | OpNot => wrapBB negb
  end.

Definition op_not := wrapBB negb.

(* There are two kinds of state: readwrite and readonly. *)
Definition readwrite := list value.
Definition readonly := list (list value).

(* Combined state. *)
Structure state := {
  state_rw : readwrite ;
  state_ro : readonly
}.

Definition empty_state := {| state_rw := nil ; state_ro := nil |}.

(* Push the readwrite state onto the readonly state, return only the readonly state. *)
Definition make_ro (σ : state) : state :=
  {| state_rw := nil ; state_ro := cons (state_rw σ) (state_ro σ) |}.

(* Extend state by a new read-write variable. *)
Definition newvar (a : value) (σ : state) :=
  {| state_rw := cons a (state_rw σ) ; state_ro := state_ro σ |}.

(** [put n v η] sets the [n]-th element of list [η] to [v]. *)
Fixpoint put {A : Type} (n : nat) (v : A) (η : list A) : option (list A) :=
  match n, η with
  | _, nil => None
  | 0, (cons _ θ) => Some (cons v θ)
  | S m, (cons u θ) =>
    match put m v θ with
    | None => None
    | Some ρ => Some (cons u ρ)
    end
  end.

(* Return the n-th element of a list, or the left-over index if the list is
   too short. *)
Fixpoint lookup {A : Type} (n : nat) (α : list A) : A + nat :=
  match n, α with
  | n, nil => inr n
  | 0, (cons x _) => inl x
  | S m, (cons _ β) => lookup m β
  end.

(* Helper function for [get], see below. *)
Fixpoint decompose {A : Type} (xss : list (list A)) : option (A * list (list A)) :=
  match xss with
  | nil => None
  | cons nil xss => decompose xss
  | cons (cons x xs) xss => Some (x, cons xs xss)
  end.

(* Get the value of the [n]-th variable in a readonly state. *)
Fixpoint get (n : nat) (ρ : readonly) : option value :=
  match decompose ρ with
  | None => None
  | Some (x, ρ) =>
    match n with
    | 0 => Some x
    | S n => get n ρ
    end
  end.

(* Evaluate a computation.

   We have two modes of evaluation:

   - [eval_comp σ c] evaluates computation [c] in state [σ] and eventually returns a pair
     [(ω, v)] where [ω] is the new readwrite portion of the state and [v] is the return value.

   - [eval_expr σ e] evaluates expression [e] in state [σ] converted to readonly, and
     eventually returns a value [v].
*)
Fixpoint eval_comp (σ : state) (c : comp) {struct c} : lift (readwrite * value) :=
  (* [ret v] converts a result given by [eval_expr] into a result for [eval_comp] *)
  let ret := (fun (x : lift value) => bind x (fun v => Now (state_rw σ, v))) in
  let eval_expr := (fun (σ : state) (e : comp) => bind (eval_comp (make_ro σ) e) (fun u => Now (snd u))) in
  match c with

  | Var n =>
    match get n (state_ro (make_ro σ)) with
    | None => Error
    | Some v => ret (Now v)
    end

  | Boolean b => ret (Now (VBoolean b))

  | Integer i => ret (Now (VInteger i))

  | BinOp op e1 e2 =>
    ret (bind2 (eval_expr σ e1) (eval_expr σ e2) (eval_binary_op op))

  | UniOp op e =>
    ret (bind (eval_expr σ e) (eval_unary_op op))

  | Skip => ret (Now VUnit)

  | Sequence c1 c2 =>
    bind (eval_comp σ c1)
         (fun u =>
            match u with
            | (ω, VUnit) => eval_comp {| state_rw := ω; state_ro := state_ro σ |} c2
            | (_, _) => Error
            end)

  | Case b1 c1 b2 c2 =>
    mcase
      (eval_expr σ b1 >>= get_boolean)
      (eval_comp σ c1)
      (eval_expr σ b2 >>= get_boolean)
      (eval_comp σ c2)

  | While b c =>
    bind
      (take_while (Now (inl σ))
                  (fun σ =>
                     bind (eval_expr σ b)
                          (fun u =>
                             match u with
                             | VBoolean false => Now (inr σ)
                             | VBoolean true =>
                               bind (eval_comp σ c)
                                    (fun v =>
                                       match v with
                                       | (ω, VUnit) => Now (inl {| state_rw := ω; state_ro := state_ro σ |})
                                       | (_, _) => Error
                                       end)
                             | _ => Error
                             end)))
      (fun ω => Now (state_rw ω, VUnit))

  | Newvar e c =>
    bind (eval_expr σ e)
         (fun a =>
            bind (eval_comp (newvar a σ) c)
                 (fun v =>
                    match v with
                    | (cons _ ω, b) => Now (ω, b)
                    | (nil, _) => Error
                    end))

  | Assign n e =>
    bind (eval_expr σ e)
         (fun a =>
            match put n a (state_rw σ) with
            | None => Error
            | Some ω => Now (ω, VUnit)
            end)
  end.

Definition run (n : nat) (c : comp) := fuel n (eval_comp empty_state c).

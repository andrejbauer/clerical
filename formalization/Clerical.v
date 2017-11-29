(* An initial attempt at defining a language for real number comptuations, based on
   converstations with Sewon Park and Alex Simspon *)

(* This version is just a simple command language, to get us started. *)

Require Import ZArith.
Require Import List.

Require Import Delay.

Inductive binary_op :=
  | OpPlus | OpLess | OpAnd.

Inductive unary_op :=
  | OpNot.

(* Computations *)
Inductive comp :=
  | Var : nat -> comp
  | Boolean : bool -> comp
  | Integer : Z -> comp
  | BinOp : binary_op -> comp -> comp -> comp
  | UniOp : unary_op -> comp -> comp
  | Skip : comp
  | Sequence : comp -> comp -> comp
  | Case : comp -> comp -> comp -> comp -> comp
  | While : comp -> comp -> comp
  | Newvar : comp -> comp -> comp
  | Assign : nat -> comp -> comp.

(* Datatypes *)
Inductive datatype :=
  | DBoolean
  | DInteger.

(* Results of computations *)
Inductive result_type :=
  | RData : datatype -> result_type
  | RCommand.

Definition RBoolean := RData DBoolean.
Definition RInteger := RData DInteger.

(* Notations for writing clerical programs. *)

Notation "'VAR' k" := (Var k) (at level 30) : clerical_scope.

Notation "'TRUE'" := (Boolean true) : clerical_scope.

Notation "'FALSE'" := (Boolean false) : clerical_scope.

Notation "'INT' k" := (Integer k) (at level 30) : clerical_scope.

Notation "e1 ':+:' e2" := (BinOp OpPlus e1 e2) (at level 60, right associativity) : clerical_scope.

Notation "e1 ':<:' e2" := (BinOp OpLess e1 e2) (at level 70, right associativity) : clerical_scope.

Notation "e1 'AND' e2" := (BinOp OpAnd e1 e2) (at level 75, right associativity) : clerical_scope.

Notation "'NOT' e" := (UniOp OpNot e) (at level 30) : clerical_scope.

Notation "'SKIP'" := (Skip) : clerical_scope.

Notation "c1 ;; c2" := (Sequence c1 c2) (at level 80, right associativity) : clerical_scope.

Notation "'MCASE' b1 '==>' c1 'OR' b2 '==>' c2 'END'" := (Case b1 c1 b2 c2) (at level 89)  : clerical_scope.

Notation "'WHEN' b 'THEN' c1 'ELSE' c2 'END'" :=
  (Newvar b (Case (Var 0) c1 (UniOp OpNot (Var 0)) c2)) (at level 85) : clerical_scope.

Notation "'WHILE' b 'DO' c 'END'" := (While b c) (at level 85) : clerical_scope.

Notation "'NEWVAR' e 'IN' c" := (Newvar e c) (at level 85) : clerical_scope.

Notation "'SET' n ':=' e" := (Assign n e) (at level 78) : clerical_scope.

Open Scope clerical_scope.

Delimit Scope clerical_scope with clerical.

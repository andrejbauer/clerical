Require Import ZArith.
Require Import Clerical.
Require Import OperationalSemantics.

Open Scope clerical_scope.

Definition Example1 :=
  (NEWVAR (INT 1 :+: INT 2 :+: INT 3) IN
          SET 0 := (VAR 0 :+: VAR 0) ;;
          VAR 0 :+: INT 10).

Eval compute in run 10 Example1.

Eval compute in run 10 (SKIP ;; INT 10).

Definition Example2 :=
  (SKIP ;;
   WHEN (INT 5 :<: INT 3) THEN
     INT 42
   ELSE
     INT 10 :<: INT 22
   END
  ).

Eval compute in run 10 Example2.

Eval compute in
    run 10 (
      WHILE FALSE DO SKIP END
    ).

Eval compute in
    run 10 (
      NEWVAR (INT 5) IN
        SET 0 := INT 17 ;;
        VAR 0 :<: INT 10
    ).

Eval compute in
    run 10 (
      NEWVAR (INT 5) IN
      NEWVAR (INT 3) IN
        SET 0 := INT 17 ;;
        SET 1 := INT 1000 ;;
        VAR 0 :+: VAR 1
    ).

Eval compute in
   run 100 (
      NEWVAR (INT 0) IN
      WHILE (VAR 0 :<: INT 20) DO
        SET 0 := (VAR 0 :+: INT 8)
      END ;;
      VAR 0
   ).

(* Compute the sum 1 + 2 + ... + 10. *)
Definition Example3 :=
  (
    NEWVAR (INT 0) IN (* the accumulator is VAR 1 *)
    NEWVAR (INT 1) IN (* the counter is VAR 0 *)
      WHILE (VAR 0 :<: INT 101) DO
        (SET 1 := (VAR 1 :+: VAR 0) ;;
        SET 0 := VAR 0 :+: INT 1)
      END ;;
      VAR 1
  ).

Eval compute in run 800 Example3.

Eval compute in
    run 10 (
          MCASE
            TRUE ==> INT 10
          OR
            FALSE ==> INT 20
          END
        ).

Definition wait (n : Z) :=
  NEWVAR INT 0 IN
    WHILE (VAR 0 :<: INT n) DO
      SET 0 := (VAR 0 :+: INT 1)
    END.

Eval compute in
    run 100 (
          MCASE
            (wait 21 ;; TRUE) ==> INT 10
          OR
            (wait 20 ;; TRUE) ==> INT 20
          END
        ).


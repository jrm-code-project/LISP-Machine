;;; -*- Mode:LISP; Package:NC; Base:10 -*-


(defun create-n (n)
  (do ((n n (1- n))
       (a () (push () a)))
      ((= n 0) a)))

Template: P_9
  (MOVE A1 A0)
  (MOVE A2 (QUOTE NIL))
  (JUMP ALWAYS DO3996_15)
DO3996_15
  (SUB GARBAGE A1 (QUOTE 0))
  (JUMP NOT-EQUAL C_19)
C_18
  (MOVE RETURN A2)
  (RETURN)
C_19
  (OPEN A2)
  (MOVE O0 (QUOTE NIL))
  (MOVE O1 A2)
  (CALL CONS (CONSTANT 2))
Template: C_24
  (L+R-1 A1 A1 (CONSTANT (QUOTE 0)))
  (JUMP ALWAYS DO3996_15)


(defun div2 (l)
       (do ((l l (cddr l))
            (a () (push (car l) a)))
           ((null l) a)))

Template: P_9
  (MOVE A1 A0)
  (MOVE A2 (QUOTE NIL))
  (JUMP ALWAYS DO3997_15)
DO3997_15
  (SUB GARBAGE A1 (QUOTE NIL))
  (JUMP NOT-EQUAL C_19)
C_18
  (MOVE RETURN A2)
  (RETURN)
C_19
  (OPEN A2)
  (OPEN O0)
  (MOVE O0 A1)
  (CALL CAR (CONSTANT 1))
Template: C_26
  (MOVE O1 A2)
  (CALL CONS (CONSTANT 2))
Template: C_29
  (OPEN A1)
  (MOVE O0 A1)
  (CALL CDDR (CONSTANT 1))
Template: C_31
  (JUMP ALWAYS DO3997_15)


(defun dv2 (l)
       (cond ((null l) ())
             (t (cons (car l) (dv2 (cddr l))))))

Template: P_5
  (SUB GARBAGE A0 (QUOTE NIL))
  (JUMP NOT-EQUAL C_9)
C_8
  (MOVE RETURN (QUOTE NIL))
  (RETURN)
C_9
  (TAIL-OPEN)
  (OPEN O1)
  (OPEN O0)
  (MOVE O0 A0)
  (CALL CDDR (CONSTANT 1))
Template: C_16
  (CALL DV2 (CONSTANT 1))
Template: C_21
  (OPEN O0)
  (MOVE O0 A0)
  (CALL CAR (CONSTANT 1))
Template: C_19
  (TAIL-CALL CONS (CONSTANT 2))


(defun test1 (l)
       (do ((i 300. (1- i)))
           ((= i 0))
           (div2 l)
           (div2 l)
           (div2 l)
           (div2 l)))

Template: P_8
  (MOVE A1 (QUOTE 300))
  (JUMP ALWAYS DO3998_14)
DO3998_14
  (SUB GARBAGE A1 (QUOTE 0))
  (JUMP NOT-EQUAL C_18)
C_17
  (MOVE RETURN (QUOTE NIL))
  (RETURN)
C_18
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DIV2 (CONSTANT 1))
Template: B_22
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DIV2 (CONSTANT 1))
Template: B_25
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DIV2 (CONSTANT 1))
Template: B_28
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DIV2 (CONSTANT 1))
Template: B_34
  (L+R-1 A1 A1 (CONSTANT (QUOTE 0)))
  (JUMP ALWAYS DO3998_14)


(defun test2 (l)
       (do ((i 300. (1- i)))
           ((= i 0))
           (dv2 l)
           (dv2 l)
           (dv2 l)
           (dv2 l)))

Template: P_8
  (MOVE A1 (QUOTE 300))
  (JUMP ALWAYS DO3999_14)
DO3999_14
  (SUB GARBAGE A1 (QUOTE 0))
  (JUMP NOT-EQUAL C_18)
C_17
  (MOVE RETURN (QUOTE NIL))
  (RETURN)
C_18
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DV2 (CONSTANT 1))
Template: B_22
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DV2 (CONSTANT 1))
Template: B_25
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DV2 (CONSTANT 1))
Template: B_28
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL DV2 (CONSTANT 1))
Template: B_34
  (L+R-1 A1 A1 (CONSTANT (QUOTE 0)))
  (JUMP ALWAYS DO3999_14)

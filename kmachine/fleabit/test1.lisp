;;; -*- Mode:LISP; Package:NC; Readtable:CL; Base:10 -*-

(defun foo (pred x)
  (if pred
      (setq x (1+ x))
    (setq x (1- x)))
  (bar x))

Template: P_6
  (SUB GARBAGE A0 (CONSTANT NIL))
  (JUMP EQUAL C_13)
C_9
  (L+R+1 A1 A1 (CONSTANT (QUOTE 0)))
  (JUMP ALWAYS B_20)
C_13
  (L+R-1 A1 A1 (CONSTANT (QUOTE 0)))
  (JUMP ALWAYS B_20)
Template: B_20
  (TAIL-OPEN)
  (MOVE O0 A1)
  (TAIL-CALL BAR (CONSTANT 1))


(defun foo (x)
  (tagbody
      (setq x 0)
    l (print x)
      (setq x (1+ x))
      (go l)))

Template: P_7
  (MOVE A0 (QUOTE 0))
  (JUMP ALWAYS L_13)
L_13
  (OPEN IGNORE)
  (MOVE O0 A0)
  (CALL PRINT (CONSTANT 1))
Template: B_20
  (L+R+1 A0 A0 (CONSTANT (QUOTE 0)))
  (JUMP ALWAYS L_13)

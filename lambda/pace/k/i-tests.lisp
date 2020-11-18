;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:CL -*-

(defkfun tst1 (x)
  (ALU L+R+1 RETURN A0 '0 CH-RETURN))

(defkfun utst4 (x)
  (MOVE O0 A0 CH-OPEN)
  (KCALL TST1 '1 A1)
  (MOVE RETURN A1 CH-RETURN))

(defkfun t-tst (x)
  (MOVE O0 A0 CH-TAIL-OPEN)
  (TAIL-CALL TST1 '1))


(defkfun t-noop ()
  (MOVE RETURN 'NIL CH-RETURN))

(defkfun t-foo1 ()
  (OPEN-CALL T-NOOP '0 A0)
  (MOVE RETURN A0 CH-RETURN))

(defkfun t-zerop (arg)
  (ALU L-R GARBAGE A0 '0)
  (TEST BR-NOT-EQUAL)
  (BRANCH TISNT)
  (MOVE RETURN 'T CH-RETURN)
 TISNT
  (MOVE RETURN 'NIL CH-RETURN))

(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))


(DEFKFUN TAK (X Y Z)
  TAG::P_7
    (ALU L-R GARBAGE A1 A0)
    (TEST BR-NOT-LESS-THAN)
    (BRANCH TAG::C_10)
  TAG::C_11
    (TAIL-OPEN)
    (ALU L+R-1 O0 A0 (QUOTE 0) CH-OPEN)
    (MOVE O1 A1)
    (KCALL TAK (QUOTE 3) O0 (O2 A2))
  TAG::C_29
    (ALU L+R-1 O0 A1 (QUOTE 0) CH-OPEN)
    (MOVE O1 A2)
    (KCALL TAK (QUOTE 3) O1 (O2 A0))
  TAG::C_27
    (ALU L+R-1 O0 A2 (QUOTE 0) CH-OPEN)
    (MOVE O1 A0)
    (KCALL TAK (QUOTE 3) O2 (O2 A1))
  TAG::C_25
    (TAIL-CALL TAK (QUOTE 3))
  TAG::C_10
    (MOVE RETURN A2 CH-RETURN))



;(defun create-n (n)
;  (do ((n n (1- n))
;       (a () (push () a)))
;      ((= n 0) a)))

(defkfun create-n (n)
  TAG::P_9
    (MOVE A1 A0)
    (MOVE A2 (QUOTE NIL))
  TAG::DO5369_15
    (ALU L-R GARBAGE A1 (QUOTE 0))
    (TEST BR-NOT-EQUAL)
    (BRANCH TAG::C_19)
  TAG::C_18
    (MOVE RETURN A2 CH-RETURN)
  TAG::C_19
    (MOVE O0 (QUOTE NIL) CH-OPEN)
    (KCALL KCONS (QUOTE 2) A2 (O1 A2))
   TAG::C_24
    (ALU L+R-1 A1 A1 (QUOTE 0))
    (JUMP TAG::DO5369_15))


;(defun div2 (l)
;  (do ((l l (cddr l))
;       (a () (push (car l) a)))
;      ((null l) a)))


(DEFKFUN DIV2 (L)
  TAG::P_9
    (MOVE A1 A0)
    (MOVE A2 (QUOTE NIL))
  TAG::DO4211_15
    (ALU L-R GARBAGE A1 (QUOTE NIL))
    (TEST BR-NOT-EQUAL)
    (BRANCH TAG::C_19)
  TAG::C_18
    (MOVE RETURN A2 CH-RETURN)
  TAG::C_19
    (KOPEN)
    (OPEN-CALL KCAR (QUOTE 1) O0 (O0 A1))
  TAG::C_26
    (KCALL KCONS (QUOTE 2) A2 (O1 A2))
  TAG::C_29
    (OPEN-CALL KCDDR (QUOTE 1) A1 (O0 A1))
  TAG::C_31
    (JUMP TAG::DO4211_15))


;(defun tid (l)
;       (do ((i 300. (1- i)))
;          ((= i 0))
;          (div2 l)
;          (div2 l)
;          (div2 l)
;          (div2 l)))


(DEFKFUN TID (L)
  TAG::P_8
    (MOVE A1 (QUOTE 300))
  TAG::DO4317_14
    (ALU L-R GARBAGE A1 (QUOTE 0))
    (TEST BR-NOT-EQUAL)
    (BRANCH TAG::C_18)
  TAG::C_17
    (MOVE RETURN (QUOTE NIL) CH-RETURN)
  TAG::C_18
    (OPEN-CALL NC::DIV2 (QUOTE 1) IGNORE (O0 A0))
  TAG::B_22
    (OPEN-CALL NC::DIV2 (QUOTE 1) IGNORE (O0 A0))
  TAG::B_25
    (OPEN-CALL NC::DIV2 (QUOTE 1) IGNORE (O0 A0))
  TAG::B_28
    (OPEN-CALL NC::DIV2 (QUOTE 1) IGNORE (O0 A0))
  TAG::B_34
    (ALU L+R-1 A1 A1 (QUOTE 0))
    (JUMP TAG::DO4317_14))

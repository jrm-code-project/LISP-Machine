;;; -*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-


(defun create-n (n)
  (do ((n n (1- n))
       (a () (push () a)))
      ((= n 0) a)))


(DEFKFUN CREATE-N (N)
  TAG::P_9
     (MOVE A2 A0)
     (MOVE A1 'NIL)
  TAG::DO4077_15
     (ALU L-R GARBAGE A2 '0)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_19)
  TAG::C_18
     (MOVE RETURN A1 CH-RETURN)
  TAG::C_19
     (MOVE O0 'NIL CH-OPEN)
     (KCALL KWRAP::CONS '2 A1 (O1 A1))
  TAG::C_27
     (ALU L+R-1 A2 A2 '0)
     (JUMP TAG::DO4077_15))



(defun iterative-div2 (l)
       (do ((l l (cddr l))
            (a () (push (car l) a)))
           ((null l) a)))


(DEFKFUN ITERATIVE-DIV2 (L)
  TAG::P_9
     (MOVE A1 A0)
     (MOVE A2 'NIL)
  TAG::DO4094_15
     (ALU L-R GARBAGE A1 'NIL)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_19)
  TAG::C_18
     (MOVE RETURN A2 CH-RETURN)
  TAG::C_19
     (KOPEN)
     (OPEN-CALL KWRAP::CAR '1 O0 (O0 A1))
  TAG::C_29
     (KCALL KWRAP::CONS '2 A2 (O1 A2))
  TAG::C_32
     (OPEN-CALL KWRAP::CDDR '1 A1 (O0 A1))
  TAG::C_34
     (JUMP TAG::DO4094_15))


(defun recursive-div2 (l)
       (cond ((null l) ())
             (t (cons (car l) (recursive-div2 (cddr l))))))


(DEFKFUN RECURSIVE-DIV2 (L)
  TAG::P_5
     (ALU L-R GARBAGE A0 'NIL)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_9)
  TAG::C_8
     (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_9
     (TAIL-OPEN)
     (KOPEN)
     (OPEN-CALL KWRAP::CDDR '1 O0 (O0 A0))
  TAG::C_16
     (KCALL RECURSIVE-DIV2 '1 O1 NIL)
  TAG::C_21
     (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_19
     (TAIL-CALL KWRAP::CONS '2))






(defun test-1 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))


(DEFKFUN TEST-1 (L)
  TAG::P_8
     (MOVE A1 '300)
  TAG::DO4140_14
     (ALU L-R GARBAGE A1 '0)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_18)
  TAG::C_17
     (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_18
     (OPEN-CALL ITERATIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_25
     (OPEN-CALL ITERATIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_28
     (OPEN-CALL ITERATIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_31
     (OPEN-CALL ITERATIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_37
     (ALU L+R-1 A1 A1 '0)
     (JUMP TAG::DO4140_14))



(defun test-2 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))


(DEFKFUN TEST-2 (L)
  TAG::P_8
     (MOVE A1 '300)
  TAG::DO4141_14
     (ALU L-R GARBAGE A1 '0)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_18)
  TAG::C_17
     (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_18
     (OPEN-CALL RECURSIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_25
     (OPEN-CALL RECURSIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_28
     (OPEN-CALL RECURSIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_31
     (OPEN-CALL RECURSIVE-DIV2 '1 IGNORE (O0 A0))
  TAG::B_37
     (ALU L+R-1 A1 A1 '0)
     (JUMP TAG::DO4141_14))

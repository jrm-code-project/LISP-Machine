;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:ZL -*-

(defun listn (n)
  (if (not (= 0 n))
      (cons n (listn (1- n)))))

(defvar 18l (listn 18.))
(defvar 12l (listn 12.))
(defvar  6l (listn 6.))

(defun mas (x y z)
  (if (not (shorterp y x))
      z
    (mas (mas (cdr x)
              y z)
         (mas (cdr y)
              z x)
         (mas (cdr z)
              x y))))


(DEFKFUN MAS (X Y Z)
  TAG::MAS_4
         (MOVE O0 A1 CH-OPEN)
         (KCALL SHORTERP '2 A3 (O1 A0))
  TAG::C_37
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_7)
  TAG::C_8
         (TAIL-OPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_13
         (MOVE O1 A1)
         (KCALL MAS '3 O0 (O2 A2))
  TAG::C_28
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A1))
  TAG::C_17
         (MOVE O1 A2)
         (KCALL MAS '3 O1 (O2 A0))
  TAG::C_26
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A2))
  TAG::C_21
         (MOVE O1 A0)
         (KCALL MAS '3 O2 (O2 A1))
  TAG::C_24
         (TAIL-CALL MAS '3)
  TAG::C_7
         (MOVE RETURN A2 CH-RETURN))

(defun shorterp (x y)
  (and y (or (null x)
             (shorterp (cdr x)
                       (cdr y)))))

(DEFKFUN SHORTERP (X Y)
  TAG::SHORTERP_5
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_27)
  TAG::C_8
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A0 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_13)
  TAG::C_30
         (MOVE RETURN 'T CH-RETURN)
  TAG::C_13
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_21
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_19
         (TAIL-CALL SHORTERP '2)
  TAG::C_27
         (MOVE RETURN 'NIL CH-RETURN))

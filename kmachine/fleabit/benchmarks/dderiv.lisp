;;;-*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-


;;; DDERIV -- The Common Lisp version of a
;;; symbolic derivative benchmark, written by Vaughan Pratt.
;;;
;;; This benchmark is a variaint of the simple symbolic
;;; derivative program (DERIV). The main change is that it is
;;; `table-driven.'  Instead of using a large COND that branches
;;; on the CAR of the expression, this program finds the code
;;; that will take the derivative on the property list of the
;;; atom in the CAR position. So, when the expression is (+
;;; <rest>), the code stored under the atom '+ with indicator
;;; DERIV will take <rest> and return the derivative for '+.

(defun dderiv-aux (a)
  (list '/ (dderiv a) a))


(DEFKFUN DDERIV-AUX (A)
  TAG::P_5
         (TAIL-OPEN)
         (OPEN-CALL DDERIV '1 O1 (O0 A0))
  TAG::C_10
         (MOVE O0 '/)
         (TAIL-CALL KWRAP::LIST3 '3 (O2 A0)))

(defun +dderiv (a)
  (cons '+ (mapcar 'dderiv A)))


(DEFKFUN +DDERIV (A)
  TAG::P_13
         (MOVE A1 'NIL CH-TAIL-OPEN)
         (MOVE O1 'NIL)
         (MOVE A2 A0)
  TAG::MAP5957_21
         (ALU L-R GARBAGE A2 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_24)
  TAG::C_54
  TAG::B_66
         (TAIL-CALL KWRAP::CONS '2 (O0 '+))
  TAG::C_24
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O1 (O0 A2))
  TAG::C_39
         (KCALL FUNCALL '2 O0 (O0 'DDERIV))
  TAG::C_42
         (KCALL KWRAP::NCONS '1 A3 NIL)
  TAG::C_45
         (ALU L-R GARBAGE A1 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_31)
  TAG::C_28
         (MOVE O0 A1 CH-OPEN)
         (KCALL KWRAP::RPLACD '2 IGNORE (O1 A3))
  TAG::B_33
         (MOVE A1 A3)
         (OPEN-CALL KWRAP::CDR '1 A2 (O0 A2))
  TAG::C_50
         (JUMP TAG::MAP5957_21)
  TAG::C_31
         (MOVE O1 A3)
         (JUMP TAG::B_33))

(defun -dderiv (a)
  (cons '- (mapcar 'deriv a)))

(defun *dderiv (a)
  (list '* (cons '* a)
        (cons '+ (mapcar 'dderiv-aux a))))

(defun *dderiv (a)
  (list '-
        (list '/
              (dderiv (car a))
              (cadr a))
        (list '/
              (car a)
              (list '*
                    (cadr a)
                    (cadr a)
                    (dderiv (cadr a))))))


(DEFKFUN *DDERIV (A)
  TAG::P_5
         (TAIL-OPEN)
         (KOPEN)
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_26
         (KCALL DDERIV '1 O3 NIL)
  TAG::C_33
         (OPEN-CALL KWRAP::CADR '1 O1 (O0 A0))
  TAG::C_31
         (OPEN-CALL KWRAP::CADR '1 O2 (O0 A0))
  TAG::C_29
         (KCALL KWRAP::LIST4 '4 O2 (O0 '*))
  TAG::C_38
         (OPEN-CALL KWRAP::CAR '1 O1 (O0 A0))
  TAG::C_36
         (KCALL KWRAP::LIST3 '3 O2 (O0 '/))
  TAG::C_43
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_11
         (KCALL DDERIV '1 O1 NIL)
  TAG::C_18
         (OPEN-CALL KWRAP::CADR '1 O2 (O0 A0))
  TAG::C_16
         (KCALL KWRAP::LIST3 '3 O1 (O0 '/))
  TAG::C_41
         (TAIL-CALL KWRAP::LIST3 '3 (O0 '-)))

(mapc
  #'(lambda (op fun)
      (setf
        (get op 'dderiv)
        (symbol-function fun)))
      '((+ +dderiv)(- -dderiv)(* *dderiv)(/ /dderiv)))

(defun dderiv (a)
  (cond
    ((atom a)
     (cond ((eq a 'x) 1) (t 0)))
    (t (let ((dderiv (get (car a) 'dderiv)))
         (cond (dderiv (funcall dderiv (cdr a)))
               (t 'error))))))


(DEFKFUN DDERIV (A)
  TAG::P_7
         (OPEN-CALL ATOM '1 A1 (O0 A0))
  TAG::C_42
         (ALU L-R GARBAGE A1 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_18)
  TAG::C_10
         (ALU L-R GARBAGE A0 'X)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_14)
  TAG::C_13
         (MOVE RETURN '1 CH-RETURN)
  TAG::C_14
         (MOVE RETURN '0 CH-RETURN)
  TAG::C_18
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_34
         (KCALL GET '2 A2 (O1 'DDERIV))
  TAG::C_37
         (ALU L-R GARBAGE A2 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_29)
  TAG::C_22
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A0))
  TAG::C_27
         (TAIL-CALL FUNCALL '2 (O0 A2))
  TAG::C_29
         (MOVE RETURN 'ERROR CH-RETURN))

(defun run ()
 (declare (fixnum i))
 (do ((i 0 (1+ i)))
     ((= i 1000.))
     (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
     (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
     (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
     (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))
     (dderiv '(+ (* 3 x x) (* a x x) (* b x) 5))))

;;; call:  (run)

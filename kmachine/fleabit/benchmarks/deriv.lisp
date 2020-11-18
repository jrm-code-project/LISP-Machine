;;; -*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-


;;; DERIV -- This is a Common Lisp version of a symbolic
;;; derivative benchmark written by Vaughan Pratt.
;;; It uses a simple subset of Lisp and does a lot of
;;; CONSing.

(defun deriv-aux (a)
  (list '/ (deriv a) a))


(DEFKFUN DERIV-AUX (A)
  TAG::P_5
     (TAIL-OPEN)
     (OPEN-CALL DERIV '1 O1 (O0 A0))
  TAG::C_10
     (MOVE O0 '/)
     (TAIL-CALL KWRAP::LIST3 '3 (O2 A0)))



(defun deriv (a)
  (cond
    ((atom a)
     (cond ((eq a 'x) 1) (t 0)))
    ((eq (car a) '+)
     (cons '+ (mapcar #'deriv (cdr a))))
    ((eq (car a) '-)
     (cons '- (mapcar #'deriv
                      (cdr a))))
    ((eq (car a) '*)
     (list '*
           a
           (cons '+ (mapcar #'deriv-aux (cdr a)))))
    ((eq (car a) '/)
     (list '-
           (list '/
                 (deriv (cadr a))
                 (caddr a))
           (list '/
                 (cadr a)
                 (list '*
                       (caddr a)
                       (caddr a)
                       (deriv (caddr a))))))
     (t 'error)))


(DEFKFUN DERIV (A)
  TAG::P_29
         (OPEN-CALL KWRAP::ATOM '1 A1 (O0 A0))
  TAG::C_286
         (ALU L-R GARBAGE A1 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_40)
  TAG::C_32
         (ALU L-R GARBAGE A0 'X)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_36)
  TAG::C_35
         (MOVE RETURN '1 CH-RETURN)
  TAG::C_36
         (MOVE RETURN '0 CH-RETURN)
  TAG::C_40
         (OPEN-CALL KWRAP::CAR '1 A2 (O0 A0))
  TAG::C_279
         (ALU L-R GARBAGE A2 '+)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_101)
  TAG::C_43
         (MOVE A3 'NIL CH-TAIL-OPEN)
         (MOVE O1 'NIL)
         (OPEN-CALL KWRAP::CDR '1 A4 (O0 A0))
  TAG::C_93
  TAG::MAP4680_51
         (ALU L-R GARBAGE A4 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_54)
  TAG::C_81
  TAG::B_96
         (TAIL-CALL KWRAP::CONS '2 (O0 '+))
  TAG::C_54
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A4))
  TAG::C_67
         (KCALL DERIV '1 O0 NIL)
  TAG::C_70
         (KCALL KWRAP::NCONS '1 A5 NIL)
  TAG::C_73
         (ALU L-R GARBAGE A3 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_61)
  TAG::C_58
         (MOVE O0 A3 CH-OPEN)
         (KCALL KWRAP::RPLACD '2 IGNORE (O1 A5))
  TAG::B_62
         (MOVE A3 A5)
         (OPEN-CALL KWRAP::CDR '1 A4 (O0 A4))
  TAG::C_78
         (JUMP TAG::MAP4680_51)
  TAG::C_61
         (MOVE O1 A5)
         (JUMP TAG::B_62)
  TAG::C_101
         (OPEN-CALL KWRAP::CAR '1 A3 (O0 A0))
  TAG::C_273
         (ALU L-R GARBAGE A3 '-)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_157)
  TAG::C_104
         (MOVE A4 'NIL CH-TAIL-OPEN)
         (MOVE O1 'NIL)
         (OPEN-CALL KWRAP::CDR '1 A5 (O0 A0))
  TAG::C_149
  TAG::MAP4685_111
         (ALU L-R GARBAGE A5 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_114)
  TAG::C_137
  TAG::B_152
         (TAIL-CALL KWRAP::CONS '2 (O0 '-))
  TAG::C_114
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A5))
  TAG::C_124
         (KCALL DERIV '1 O0 NIL)
  TAG::C_127
         (KCALL KWRAP::NCONS '1 A6 NIL)
  TAG::C_130
         (ALU L-R GARBAGE A4 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_120)
  TAG::C_118
         (MOVE O0 A4 CH-OPEN)
         (KCALL KWRAP::RPLACD '2 IGNORE (O1 A6))
  TAG::B_121
         (MOVE A4 A6)
         (OPEN-CALL KWRAP::CDR '1 A5 (O0 A5))
  TAG::C_134
         (JUMP TAG::MAP4685_111)
  TAG::C_120
         (MOVE O1 A6)
         (JUMP TAG::B_121)
  TAG::C_157
         (OPEN-CALL KWRAP::CAR '1 A4 (O0 A0))
  TAG::C_267
         (ALU L-R GARBAGE A4 '*)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_217)
  TAG::C_160
         (TAIL-OPEN)
         (MOVE A5 'NIL CH-OPEN)
         (MOVE O1 'NIL)
         (OPEN-CALL KWRAP::CDR '1 A6 (O0 A0))
  TAG::C_206
  TAG::MAP4690_168
         (ALU L-R GARBAGE A6 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_171)
  TAG::C_194
  TAG::B_209
         (KCALL KWRAP::CONS '2 O2 (O0 '+))
  TAG::C_215
         (MOVE O0 '*)
         (TAIL-CALL KWRAP::LIST3 '3 (O1 A0))
  TAG::C_171
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A6))
  TAG::C_181
         (KCALL DERIV-AUX '1 O0 NIL)
  TAG::C_184
         (KCALL KWRAP::NCONS '1 A7 NIL)
  TAG::C_187
         (ALU L-R GARBAGE A5 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_177)
  TAG::C_175
         (MOVE O0 A5 CH-OPEN)
         (KCALL KWRAP::RPLACD '2 IGNORE (O1 A7))
  TAG::B_178
         (MOVE A5 A7)
         (OPEN-CALL KWRAP::CDR '1 A6 (O0 A6))
  TAG::C_191
         (JUMP TAG::MAP4690_168)
  TAG::C_177
         (MOVE O1 A7)
         (JUMP TAG::B_178)
  TAG::C_217
         (OPEN-CALL KWRAP::CAR '1 A5 (O0 A0))
  TAG::C_261
         (ALU L-R GARBAGE A5 '/)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_258)
  TAG::C_220
         (TAIL-OPEN)
         (KOPEN)
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CADDR '1 O0 (O0 A0))
  TAG::C_239
         (KCALL DERIV '1 O3 NIL)
  TAG::C_246
         (OPEN-CALL KWRAP::CADDR '1 O1 (O0 A0))
  TAG::C_244
         (OPEN-CALL KWRAP::CADDR '1 O2 (O0 A0))
  TAG::C_242
         (KCALL KWRAP::LIST4 '4 O2 (O0 '*))
  TAG::C_251
         (OPEN-CALL KWRAP::CADR '1 O1 (O0 A0))
  TAG::C_249
         (KCALL KWRAP::LIST3 '3 O2 (O0 '/))
  TAG::C_256
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_224
         (KCALL DERIV '1 O1 NIL)
  TAG::C_231
         (OPEN-CALL KWRAP::CADDR '1 O2 (O0 A0))
  TAG::C_229
         (KCALL KWRAP::LIST3 '3 O1 (O0 '/))
  TAG::C_254
         (TAIL-CALL KWRAP::LIST3 '3 (O0 '-))
  TAG::C_258
         (MOVE RETURN 'ERROR CH-RETURN))


(defun run ()
 (declare (fixnum i))
 (do ((i 0 (1+ i)))
     ((= i 1000.))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))


(DEFKFUN RUN NIL
  TAG::P_7
         (MOVE A0 '0)
  TAG::DO4805_14
         (ALU L-R GARBAGE A0 '1000)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_18)
  TAG::C_17
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_18
         (OPEN-CALL DERIV '1 IGNORE (O0 '(+ (* 3 X X) (* A X X) (* B X) 5)))
  TAG::B_24
         (OPEN-CALL DERIV '1 IGNORE (O0 '(+ (* 3 X X) (* A X X) (* B X) 5)))
  TAG::B_26
         (OPEN-CALL DERIV '1 IGNORE (O0 '(+ (* 3 X X) (* A X X) (* B X) 5)))
  TAG::B_28
         (OPEN-CALL DERIV '1 IGNORE (O0 '(+ (* 3 X X) (* A X X) (* B X) 5)))
  TAG::B_30
         (OPEN-CALL DERIV '1 IGNORE (O0 '(+ (* 3 X X) (* A X X) (* B X) 5)))
  TAG::B_35
         (ALU L+R+1 A0 A0 '0)
         (JUMP TAG::DO4805_14))

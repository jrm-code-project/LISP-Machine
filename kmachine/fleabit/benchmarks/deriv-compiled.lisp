;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-


;;; DERIV -- This is a Common Lisp version of a symbolic
;;; derivative benchmark written by Vaughan Pratt.
;;; It uses a simple subset of Lisp and does a lot of
;;; CONSing.

(defun deriv-aux (a)
  (list '/ (deriv a) a))

DERIV-AUX_4
   (OPEN-CALL (DERIV 1) (NEW-TAIL-OPEN 1) (O0 A0))
   (MOVEI O0 (QUOTE /) BOXED)
   (TAIL-CALL (LIST3 3) (O2 A0))


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

DERIV_55
   (ALU-FIELD FIELD-PASS R2 A0 (REGISTER *ZERO* 4 0) (BYTE 6 -26) PW-II DT-NONE BOXED-RIGHT)
   (ALU L-R NOP R2 (REGISTER *TWO* 4 3) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_65 NIL)
C_58
   (MOVEI R1 (QUOTE X) BOXED)
   (ALU L-R NOP A0 R1 BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_62 NIL)
C_61
   (RETURNI (QUOTE 1))
C_62
   (RETURNI (QUOTE 0))
C_65
   (OPEN-CALL (CAR 1) R2 (O0 A0))
   (MOVEI R1 (QUOTE +) BOXED)
   (ALU L-R NOP R2 R1 BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_130 NIL)
C_68
   (TAIL-OPEN)
BLOCK_69
   (OPEN-CALL (CDR 1) A9 (O0 A0))
   (MOVEI O1 (QUOTE NIL) BOXED)
   (MOVEI A8 (QUOTE NIL) BOXED)
P_70
DO1029_77
   (ALU L-R NOP A9 (REGISTER *NIL* 4 5) BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_83 NIL)
C_80
C_128
   (MOVEI O0 (QUOTE +) BOXED)
   (TAIL-CALL (CONS 2) NIL)
C_83
   (OPEN-CALL (CAR 1) (NEW-OPEN 0) (O0 A9))
   (CALL (DERIV 1) (NEW-OPEN 0) NIL)
   (CALL (NCONS 1) A7 NIL)
P_88
   (MOVE NOP A8)
   (TEST BR-ZERO)
   (BRANCH C_91 NIL)
C_89
   (MOVE O0 A8 CH-OPEN)
   (CALL (RPLACD 2) IGNORE (O1 A7))
B_93
   (MOVE A8 A7)
   (OPEN-CALL (CDR 1) A9 (O0 A9))
P_106
   (UNCONDITIONAL-BRANCH DO1029_77 NIL)
C_91
   (UNCONDITIONAL-BRANCH B_93 (MOVE O1 A7))
C_130
   (OPEN-CALL (CAR 1) R2 (O0 A0))
   (MOVEI R1 (QUOTE -) BOXED)
   (ALU L-R NOP R2 R1 BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_195 NIL)
C_133
   (TAIL-OPEN)
BLOCK_134
   (OPEN-CALL (CDR 1) A6 (O0 A0))
   (MOVEI O1 (QUOTE NIL) BOXED)
   (MOVEI A5 (QUOTE NIL) BOXED)
P_135
DO1037_142
   (ALU L-R NOP A6 (REGISTER *NIL* 4 5) BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_148 NIL)
C_145
C_193
   (MOVEI O0 (QUOTE -) BOXED)
   (TAIL-CALL (CONS 2) NIL)
C_148
   (OPEN-CALL (CAR 1) (NEW-OPEN 0) (O0 A6))
   (CALL (DERIV 1) (NEW-OPEN 0) NIL)
   (CALL (NCONS 1) A4 NIL)
P_153
   (MOVE NOP A5)
   (TEST BR-ZERO)
   (BRANCH C_156 NIL)
C_154
   (MOVE O0 A5 CH-OPEN)
   (CALL (RPLACD 2) IGNORE (O1 A4))
B_158
   (MOVE A5 A4)
   (OPEN-CALL (CDR 1) A6 (O0 A6))
P_171
   (UNCONDITIONAL-BRANCH DO1037_142 NIL)
C_156
   (UNCONDITIONAL-BRANCH B_158 (MOVE O1 A4))
C_195
   (OPEN-CALL (CAR 1) R2 (O0 A0))
   (MOVEI R1 (QUOTE *) BOXED)
   (ALU L-R NOP R2 R1 BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_263 NIL)
C_198
   (OPEN)
BLOCK_199
   (OPEN-CALL (CDR 1) A3 (O0 A0))
   (MOVEI O1 (QUOTE NIL) BOXED)
   (MOVEI A2 (QUOTE NIL) BOXED)
P_200
DO1045_207
   (ALU L-R NOP A3 (REGISTER *NIL* 4 5) BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_213 NIL)
C_210
C_258
   (MOVEI O0 (QUOTE +) BOXED)
   (CALL (CONS 2) (NEW-TAIL-OPEN 2) NIL)
   (MOVEI O0 (QUOTE *) BOXED)
   (TAIL-CALL (LIST3 3) (O1 A0))
C_213
   (OPEN-CALL (CAR 1) (NEW-OPEN 0) (O0 A3))
   (CALL (DERIV-AUX 1) (NEW-OPEN 0) NIL)
   (CALL (NCONS 1) A1 NIL)
P_218
   (MOVE NOP A2)
   (TEST BR-ZERO)
   (BRANCH C_221 NIL)
C_219
   (MOVE O0 A2 CH-OPEN)
   (CALL (RPLACD 2) IGNORE (O1 A1))
B_223
   (MOVE A2 A1)
   (OPEN-CALL (CDR 1) A3 (O0 A3))
P_236
   (UNCONDITIONAL-BRANCH DO1045_207 NIL)
C_221
   (UNCONDITIONAL-BRANCH B_223 (MOVE O1 A1))
C_263
   (OPEN-CALL (CAR 1) R2 (O0 A0))
   (MOVEI R1 (QUOTE /) BOXED)
   (ALU L-R NOP R2 R1 BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_301 NIL)
C_266
   (OPEN-CALL (CADR 1) (NEW-OPEN 0) (O0 A0))
   (CALL (DERIV 1) (NEW-OPEN 1) NIL)
   (OPEN-CALL (CADDR 1) O2 (O0 A0))
   (MOVEI O0 (QUOTE /) BOXED)
   (CALL (LIST3 3) (NEW-TAIL-OPEN 1) NIL)
   (OPEN-CALL (CADR 1) (NEW-OPEN 1) (O0 A0))
   (OPEN-CALL (CADDR 1) (NEW-OPEN 1) (O0 A0))
   (OPEN-CALL (CADDR 1) O2 (O0 A0))
   (OPEN-CALL (CADDR 1) (NEW-OPEN 0) (O0 A0))
   (CALL (DERIV 1) O3 NIL)
   (MOVEI O0 (QUOTE *) BOXED)
   (CALL (LIST4 4) O2 NIL)
   (MOVEI O0 (QUOTE /) BOXED)
   (CALL (LIST3 3) O2 NIL)
   (MOVEI O0 (QUOTE -) BOXED)
   (TAIL-CALL (LIST3 3) NIL)
C_301
   (RETURNI (QUOTE ERROR))




(defun run ()
 (do ((i 0 (1+ i)))
     ((= i 1000.))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))

RUN_10
   (MOVEI A0 (QUOTE 0) BOXED)
P_13
DO1049_20
   (MOVEI R1 (QUOTE 1000) BOXED)
   (ALU L-R NOP A0 R1 BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_26 NIL)
C_23
   (RETURNI (QUOTE NIL))
C_26
   (MOVEI O0 (QUOTE (+ (* 3 X X) (* A X X) (* B X) 5)) BOXED CH-OPEN)
   (CALL (DERIV 1) IGNORE NIL)
   (MOVEI O0 (QUOTE (+ (* 3 X X) (* A X X) (* B X) 5)) BOXED CH-OPEN)
   (CALL (DERIV 1) IGNORE NIL)
   (MOVEI O0 (QUOTE (+ (* 3 X X) (* A X X) (* B X) 5)) BOXED CH-OPEN)
   (CALL (DERIV 1) IGNORE NIL)
   (MOVEI O0 (QUOTE (+ (* 3 X X) (* A X X) (* B X) 5)) BOXED CH-OPEN)
   (CALL (DERIV 1) IGNORE NIL)
   (MOVEI O0 (QUOTE (+ (* 3 X X) (* A X X) (* B X) 5)) BOXED CH-OPEN)
   (CALL (DERIV 1) IGNORE NIL)
   (UNCONDITIONAL-BRANCH DO1049_20 (ALU R+1 A0 IGNORE A0 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW))

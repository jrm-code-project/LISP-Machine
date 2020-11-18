l;;; Div2

(defun create-n (n)
  (do ((n n (1- n))
       (a () (push () a)))
      ((= n 0) a)))

CREATE-N_13
   (MOVE A2 A0)
   (MOVEI A1 (QUOTE NIL) BOXED)
P_15
DO0903_22
   (ALU L-R NOP A2 (REGISTER *ZERO* 4 0) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_28 NIL)
C_25
   (RETURN A1)
C_28
   (ALU R-1 A2 IGNORE A2 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW)
   (MOVEI O0 (QUOTE NIL) BOXED CH-OPEN)
   (CALL (CONS 2) A1 (O1 A1))
   (UNCONDITIONAL-BRANCH DO0903_22 NIL)



(defun iterative-div2 (l)
       (do ((l l (cddr l))
            (a () (push (car l) a)))
           ((null l) a)))

ITERATIVE-DIV2_15
   (MOVE A3 A0)
   (MOVEI A2 (QUOTE NIL) BOXED)
P_17
DO0906_24
   (ALU L-R NOP A3 (REGISTER *NIL* 4 5) BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_30 NIL)
C_27
   (RETURN A2)
C_30
   (OPEN-CALL (CDDR 1) A1 (O0 A3))
   (OPEN-CALL (CAR 1) (NEW-OPEN 0) (O0 A3))
   (CALL (CONS 2) A2 (O1 A2))
   (UNCONDITIONAL-BRANCH DO0906_24 (MOVE A3 A1))



(defun recursive-div2 (l)
       (cond ((null l) ())
             (t (cons (car l) (recursive-div2 (cddr l))))))

RECURSIVE-DIV2_6
   (ALU L-R NOP A0 (REGISTER *NIL* 4 5) BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_10 NIL)
C_9
   (RETURNI (QUOTE NIL))
C_10
   (OPEN-CALL (CAR 1) (NEW-TAIL-OPEN 0) (O0 A0))
   (OPEN-CALL (CDDR 1) (NEW-OPEN 0) (O0 A0))
   (CALL (RECURSIVE-DIV2 1) O1 NIL)
   (TAIL-CALL (CONS 2) NIL)



(defun test-1 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)))

TEST-1_11
   (MOVEI A1 (QUOTE 300) BOXED)
P_13
DO0909_20
   (ALU L-R NOP A1 (REGISTER *ZERO* 4 0) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_26 NIL)
C_23
   (RETURNI (QUOTE NIL))
C_26
   (OPEN-CALL (ITERATIVE-DIV2 1) IGNORE (O0 A0))
   (OPEN-CALL (ITERATIVE-DIV2 1) IGNORE (O0 A0))
   (OPEN-CALL (ITERATIVE-DIV2 1) IGNORE (O0 A0))
   (OPEN-CALL (ITERATIVE-DIV2 1) IGNORE (O0 A0))
   (UNCONDITIONAL-BRANCH DO0909_20 (ALU R-1 A1 IGNORE A1 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW))


(defun test-2 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

TEST-2_11
   (MOVEI A1 (QUOTE 300) BOXED)
P_13
DO0911_20
   (ALU L-R NOP A1 (REGISTER *ZERO* 4 0) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_26 NIL)
C_23
   (RETURNI (QUOTE NIL))
C_26
   (OPEN-CALL (RECURSIVE-DIV2 1) IGNORE (O0 A0))
   (OPEN-CALL (RECURSIVE-DIV2 1) IGNORE (O0 A0))
   (OPEN-CALL (RECURSIVE-DIV2 1) IGNORE (O0 A0))
   (OPEN-CALL (RECURSIVE-DIV2 1) IGNORE (O0 A0))
   (UNCONDITIONAL-BRANCH DO0911_20 (ALU R-1 A1 IGNORE A1 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW))

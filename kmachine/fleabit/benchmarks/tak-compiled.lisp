

(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))

TAK_5
   (ALU L-R NOP A1 A0 BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-LESS-THAN)
   (BRANCH C_8 NIL)
C_9
   (ALU R-1 O0 IGNORE A0 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW CH-OPEN)
   (MOVE O1 A1)
   (CALL (TAK 3) (NEW-TAIL-OPEN 0) (O2 A2))
   (ALU R-1 O0 IGNORE A1 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW CH-OPEN)
   (MOVE O1 A2)
   (CALL (TAK 3) O1 (O2 A0))
   (ALU R-1 O0 IGNORE A2 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW CH-OPEN)
   (MOVE O1 A0)
   (CALL (TAK 3) O2 (O2 A1))
   (TAIL-CALL (TAK 3) NIL)
C_8
   (RETURN A2)

;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:ZL -*-


(defun add (a b)
  (if (zerop a)
      b
    (add (1- a) (1+ b))))


(defafun add (a b)
ADD_3
   (MOVEI R0 (QUOTE 0))
   (ALU L-R NOP-NO-OVERFLOW-TRAP A0 R0 BW-24)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_7 NIL)
C_6
   (RETURN A1)
C_7
   (ALU R-1 O0 A0 A0 BW-24 CH-TAIL-OPEN)
   (ALU R+1 O1 A0 A1 BW-24)
   (TAIL-CALL ADD1 (QUOTE 2) NIL))

(defafun add1 (a b)
ADD_3
   (MOVEI R0 (QUOTE 0))
   (ALU L-R NOP-NO-OVERFLOW-TRAP A0 R0 BW-24)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_7 NIL)
C_6
   (RETURN A1)
C_7
   (ALU R-1 O0 A0 A0 BW-24 CH-OPEN)
   (ALU R+1 O1 A0 A1 BW-24)
   (KCALL ADD (QUOTE 2) a3 NIL)
   (nop)
   (return a3))


(defafun test-add ()
  loop
   (alu r+1 (%register g0 0 0) (%register g0 0 0) (%register g0 0 0))
  (alu-field field-not memory-control (%register g0 0 0) memory-control hw:%%memory-control-leds)
   (MOVEI O0 '11 CH-OPEN)
   (MOVEI O1 '13)
   (MOVEI O2 '4)
   (KCALL add 3 a0 NIL)
 here
  (unconditional-branch here ()))

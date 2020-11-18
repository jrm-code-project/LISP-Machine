;;; -*- Mode:LISP; Package:(TEST :use (K LISP)); Base:10; Readtable:ZL -*-


(import '(nc:defafun nc:ndisassemble nc:prins nc:link))

(do-symbols (s 'k)
  (export s 'k))

(defafun foo ()
  (return a0))

;;; Count in processor lights
(defafun lights ()
  loop
   (alu r+1 (%register g0 0 0) (%register g0 0 0) (%register g0 0 0))
   (alu-field field-pass (%register g1 0 1) (%register g0 0 0) (%register g1 0 1) (byte 3 -19))
   (alu-field field-not memory-control (%register g1 0 1) memory-control hw:%%memory-control-leds)
   (nop)
   (unconditional-branch loop ()))



;----------------------------------------------------------------


(defafun count-calls ()
  loop
   (alu r+1 (%register g0 0 0) (%register g0 0 0) (%register g0 0 0))
  (alu-field field-not memory-control (%register g0 0 0) memory-control hw:%%memory-control-leds)
   (MOVEI O0 '20 CH-OPEN)
   (MOVEI O1 '12)
   (MOVEI O2 '6)
   (KCALL fib 3 r0 NIL)
  (jump #x100 ()))



;----------------------------------------------------------------

(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (1- n))
       (fib (- n 2)))))


(defafun fib (n)
FIB_2
   (MOVEI R0 (QUOTE 2))
   (ALU L-R NOP-NO-OVERFLOW-TRAP A0 R0 BW-24)
   (TEST BR-NOT-LESS-THAN)
   (BRANCH C_6 NIL)
C_5
   (RETURN A0)
C_6
   (ALU R-1 O0 A0 A0 BW-24 CH-OPEN)
   (KCALL FIB (QUOTE 1) A2 NIL)
C_19
   (MOVEI R0 (QUOTE 2) CH-OPEN)
   (ALU L-R O0 A0 R0 BW-24)
   (KCALL FIB (QUOTE 1) A1 NIL)
C_17
   (nop)
   (ALU L+R RETURN A2 A1 BW-24 CH-RETURN NEXT-PC-RETURN))


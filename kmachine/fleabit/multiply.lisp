;;; -*- Mode:LISP; Package:TEST; Base:10; Readtable:ZL -*-


(defafun test-mult ()
  loop
   (alu r+1 (%register g0 0 0) (%register g0 0 0) (%register g0 0 0))
  (alu-field field-not memory-control (%register g0 0 0) memory-control hw:%%memory-control-leds)
   (MOVEI O0 '3 CH-OPEN)
   (MOVEI O1 '4)
   (KCALL imult24 2 a0 NIL)
 here
  (unconditional-branch here ()))

(defafun imult24 (x y)                          ;;; 21 instructions max
  (alu load-q-r nop-no-overflow-trap a0 a0 bw-32)
  (movei a4 '0) ;use global ZERO register eventually
  (alu smul-first a2 a1 a4 bw-24)

  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)

  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)

  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)
  (alu smul-step  a2 a1 a2 bw-24)

 (alu pass-q a3 a1 a1 bw-24)                    ;; get low half
 (move nop-no-overflow-trap a3 bw-24)           ;; check sign
 (move nop-no-overflow-trap a2 bw-24 br-less-than)
 (branch neg (alu not-r nop-no-overflow-trap r0 a2 bw-24) br-not-equal)
pos
 (branch overflow ())
done
 (return a3)
neg
 (test br-equal)
 (branch done ())
overflow
 (unconditional-branch overflow ()))




(defafun test-div ()
  loop
   (MOVEI O0 '24 CH-OPEN)
   (MOVEI O1 '3)
   (KCALL idiv24 2 a0 NIL)
 here
  (unconditional-branch here ()))


(defafun idiv24 (x y)                           ;;; 33 instructions max
  (alu load-q-r a2 r0 a2 bw-24)                 ;q <- dividend
  (alu sign a2 r0 r0 bw-24)                     ;sign extend initial remainder
  (alu sdiv-first a2 a1 a2 bw-24)               ;step 1

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-last1 a2 a1 a2 bw-24)               ;first fixup
  (alu pass-q a3 a1 a1 br-equal bw-24)          ;no fixup2 if zero, save quotient maybe
  (branch done ())

  (alu sdiv-last2 nop-no-overflow-trap a1 a2 bw-24)             ;second fixup
  (alu pass-q a3 a1 a1 bw-24)                   ;save fixed quotient
  (alu quo-corr a3 a3 a3 bw-24)                 ;final fixup
done
  (return a3))

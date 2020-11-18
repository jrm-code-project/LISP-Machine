;-*-Mode:LISP; base: 8; readtable: ZL-*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

(DEFCONST UC-ARITH '(
;;; NON-DESTINATION GROUP 1
;   E IN VMA, C(E) IN M-T, MOSTLY EXIT BY PUTTING RESULT ON STACK

;GET TWO PDL ARGUMENTS, FIRST TO M-1, SECOND TO M-2

FXGTPP    (ERROR-TABLE RESTART FXGTPP)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)           ;GET PDL ARG
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
          (ERROR-TABLE ARGTYP FIXNUM PP 1 FXGTPP)
        ((M-T) C-PDL-BUFFER-POINTER-POP)
          (ERROR-TABLE RESTART FXGTP0)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
          (ERROR-TABLE ARGTYP FIXNUM PP 0 FXGTP0)
          (ERROR-TABLE ARG-POPPED 0 PP M-T)
        (POPJ-AFTER-NEXT
          (M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
       ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)

;FIXGET, FIXGET-1 not used on LAMBDA.


;;; MULTIPLY SUBROUTINE
;M-1 TIMES Q-R, RESULT TO Q-R, LEAVES CORRECT HIGH HALF IN M-2.
;CALLER MUST CHECK FOR OVERFLOW, IF SHE CARES.

MPY     ((M-2) MULTIPLY-STEP A-1 M-ZERO)
(REPEAT 30. ((M-2) MULTIPLY-STEP M-2 A-1))
        (POPJ-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 0) Q-R)
       ((M-2) MULTIPLY-STEP M-2 A-1)
        (POPJ-AFTER-NEXT
         (M-2) M-2 SUB A-1)             ;FINAGLE IF NEGATIVE VALUE INITIALLY IN Q-R
       (NO-OP)

;;; DIVIDE SUBROUTINE
;   DIVIDEND IN M-1, DIVISOR IN M-2
;   QUOTIENT IN Q-R, REMAINDER IN M-1, CLOBBERS A-TEM1
#-lambda(begin-comment)
DIV     (declare (args a-1 a-2) (values a-1) (clobbers a-tem1))
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-1 A-ZERO DIV1)
       ((M-TEM1 Q-R) M-1)       ;Q GETS MAGNITUDE OF DIVIDEND, A-TEM1 SAVES ORIGINAL
        ((Q-R) SUB M-ZERO A-TEM1)
DIV1    ((M-1) DIVIDE-FIRST-STEP M-ZERO A-2)
DIV1A   (CALL-IF-BIT-SET (BYTE-FIELD 1 0) Q-R TRAP)     ;DIVIDE OVERFLOW
  (ERROR-TABLE DIVIDE-BY-ZERO)
(REPEAT 31. ((M-1) DIVIDE-STEP M-1 A-2))
        ((M-1) DIVIDE-LAST-STEP M-1 A-2)
        (JUMP-LESS-OR-EQUAL-XCT-NEXT M-ZERO A-TEM1 DIV2) ;JUMP IF POSITIVE DIVIDEND
       ((M-1) DIVIDE-REMAINDER-CORRECTION-STEP M-1 A-2) ;M-1 GETS MAGNITUDE OF REMAINDER
        ((M-1) SUB M-ZERO A-1)          ;NEGATIVE DIVIDEND => NEGATIVE REMAINDER
DIV2    ((M-TEM1) XOR M-2 A-TEM1)       ;IF SIGNS OF DIVIDEND AND DIVISOR ARE DIFFERENT,
        (POPJ-LESS-OR-EQUAL M-ZERO A-TEM1)
        (POPJ-AFTER-NEXT
         (M-TEM1) Q-R)
       ((Q-R) SUB M-ZERO A-TEM1)        ;THEN QUOTIENT IS NEGATIVE
#-lambda(end-comment)

#-exp(begin-comment)
DIV
        (Jump-greater-or-equal-xct-next M-1 A-Zero DIV1)
       ((M-TEM1 Q-R) M-1)
        ((Q-R) SUB M-Zero A-tem1)
DIV1
#-exp(begin-comment)
        (Jump-greater-or-equal-xct-next M-2 A-Zero DIV-Not-Neg-2)
       ((m-tem2) M-2)
        ((M-2) SUB M-Zero A-2)
DIV-Not-Neg-2
#-exp(end-comment)
        ((M-1) Divide-First-Step M-Zero A-2)
DIV1A   (Call-If-Bit-Set (Byte-Field 1 0) Q-R TRAP)
  (ERROR-TABLE DIVIDE-BY-ZERO)
       (repeat 31. ((M-1) Divide-Step M-1 A-2))
        ((M-1) Divide-Last-Step M-1 A-2)
        (Jump-Less-Or-Equal-Xct-Next M-Zero a-tem1 DIV2)
       ((M-1) Divide-Remainder-Correction-Step M-1 A-2)
        ((M-1) SUB M-Zero A-1)
DIV2
#+exp   ((M-2) m-tem2)
        ((m-tem1) xor m-2 a-tem1)
        (popj-less-or-equal m-zero a-tem1)
        (popj-after-next (m-tem1) Q-R)
       ((Q-R) SUB M-zero a-tem1)
#-exp(end-comment)

;FIXNUM EXPONENTIATION ROUTINE.
;M-3 HOLDS THE EXPONENT, AND GETS SHIFTED AND TESTED.
;M-1 HOLDS THE FIRST ARG, SQUARED N TIMES.
;M-T HOLDS THE PARTIAL PRODUCTS

(ERROR-TABLE DEFAULT-ARG-LOCATIONS ^ PP PP)

XUPARROW (MISC-INST-ENTRY ^)
        ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER (A-CONSTANT 1))      ;POINT TO FIRST ARG
        (jump-data-type-not-equal c-pdl-buffer-index
                 (a-constant (byte-value q-data-type dtp-fix)) xupout)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                 (a-constant (byte-value q-data-type dtp-fix)) xupout)
        ((m-2) output-selector-extend-25 c-pdl-buffer-pointer)
        ((m-1) output-selector-extend-25 c-pdl-buffer-index)
        (JUMP-LESS-THAN M-2 A-ZERO XUP6)                ;FIXNUM ^ -<FIXNUM> = 0 USUALLY
XUP5    ((M-TEM) (A-CONSTANT 1))                        ;INITIALIZE RESULT
        (JUMP-EQUAL M-2 A-ZERO XUP4)                    ;ANYTHING ^ 0 = 1
        ((M-3) M-2)                                     ;SAVE THE EXPONENT
XUP1    (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) M-3 XUP2)
        (CALL-XCT-NEXT MPY)                             ;M-1 TIMES M-TEM TO Q-R
       ((Q-R) M-TEM)
        ((M-2) SELECTIVE-DEPOSIT Q-R
         (BYTE-FIELD (DIFFERENCE 33. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 1))
         A-2)   ;DISCARDED BITS AND SIGN
                                                        ;M-TEM IS 32 BITS, BUT FIXED BIN(23,0)
        (JUMP-EQUAL-XCT-NEXT M-2 A-ZERO XUP2)           ;JUMP IF POSITIVE NO OVERFLOW
       ((M-TEM) Q-R)                                    ;PRODUCT BACK TO M-TEM
        (JUMP-NOT-EQUAL M-2 (A-CONSTANT -1) XUPOUT)     ;DROP THROUGH IF OK NEG, ELSE OVFL
XUP2    ((M-3) M-3 OUTPUT-SELECTOR-RIGHTSHIFT-1)        ;(SETQ M-3 (ASH M-3 -1))
        (JUMP-EQUAL M-3 A-ZERO XUP4)                    ;IF ZERO, RESULT IS IN M-TEM
        (CALL-XCT-NEXT MPY)                             ;OTHERWISE COMPUTE NEXT POWER
       ((Q-R) M-1)                                      ;I.E. Q-R GETS M-1 TIMES M-1
        ((M-2) SELECTIVE-DEPOSIT Q-R
         (BYTE-FIELD (DIFFERENCE 33. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 1))
         A-2)   ;DISCARDED BITS AND SIGN
        (JUMP-NOT-EQUAL M-2 A-ZERO XUPOUT)              ;OVERFLOW (RESULT IS ALWAYS POSITIVE)
        (JUMP-XCT-NEXT XUP1)
       ((M-1) Q-R)                                      ;(SETQ M-1 (* M-1 M-1))

;;; Here if exponent is a negative integer.
;Result is a rational unless base (M-1) is 0, -1, or 1.
XUP6    (JUMP-GREATER-THAN M-1 (A-CONSTANT 1) XUP3)
        (JUMP-LESS-THAN M-1 (A-CONSTANT -1) XUP3)
        (CALL-EQUAL M-1 (A-CONSTANT 0) TRAP)            ;0 ^ negative power is an error
            (ERROR-TABLE DIVIDE-BY-ZERO)
        ((M-TEM) M-1)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) M-2 XUP4)     ;-1 ^ odd negative power is -1
        ((M-TEM) (A-CONSTANT 1))                        ;-1 ^ even negative power is 1
        ;drop into XUP4
;;; RETURN VALUE IN M-TEM AND POP OFF ARGUMENTS
XUP4    (POPJ-AFTER-NEXT (PDL-BUFFER-POINTER) SUB PDL-BUFFER-POINTER (A-CONSTANT 2))
       ((M-T) DPB M-TEM Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XUP3    ((M-2) SUB M-ZERO A-2)  ;Make the exponent positive.
        ;Make exponent on stack positive as well -- if we overflow, will call-out to macrocode with original arguments
        ((c-pdl-buffer-pointer) dpb m-2 q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (CALL XUP5)             ;Exponentiate.
        ((PDL-PUSH) M-T)        ;This becomes the denominator.  The numerator is 1.
        (JUMP-IF-BIT-SET BOXED-SIGN-BIT M-T XUP3A)      ;But may need to factor out -1.
        ((PDL-PUSH) DPB M-MINUS-ONE (BYTE-FIELD 1 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP MAKE-RATIONAL)

XUP3A   (CALL XABS)             ;Here for reciprocal of negative integer: make it positive
        ((PDL-PUSH) M-T)        ;and make rational with numerator -1.
        ((PDL-PUSH) DPB M-MINUS-ONE Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP MAKE-RATIONAL)

;;; HERE CALL OUT TO MACRO CODE
XUPOUT  ((M-A) C-PDL-BUFFER-POINTER-POP)        ;The exponent
        ((M-B) C-PDL-BUFFER-POINTER-POP)        ;The base
        (CALL P3ZERO)                           ;Open micro-to-macro call
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-EXPT))        ;Get fctn cell of EXPT-HELP
        (DISPATCH TRANSPORT MD)
        ((C-PDL-BUFFER-POINTER-PUSH) READ-MEMORY-DATA)  ;Push function
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the base
                Q-TYPED-POINTER M-B (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the exponent
                Q-TYPED-POINTER M-A (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-JUMP MMJCALL) (I-ARG 2))          ;Call it tail-recursively


XGCD (MISC-INST-ENTRY INTERNAL-\\)              ;GCD, STEIN'S ALGORITHM.
        (CALL-XCT-NEXT GET-FIX-OR-BIGNUM)       ; SET UP FOR GCD'S BY GETTING 2 ARGS
       ((M-A) M-ZERO)                           ;THIS IS MAGIC INDEX ON TYPES OF ARGUMENTS
        (DISPATCH (BYTE-FIELD 2 0) M-A GCD-DISPATCH)

(LOCALITY D-MEM)
(START-DISPATCH 2)
GCD-DISPATCH
        (P-BIT R-BIT)           ;FIXNUM-FIXNUM CASE (DROPS THROUGH)
        (INHIBIT-XCT-NEXT-BIT GCD-FIX-BIG)      ;FIXNUM-BIGNUM CASE
        (INHIBIT-XCT-NEXT-BIT GCD-BIG-FIX)      ;BIGNUM-FIXNUM CASE
        (INHIBIT-XCT-NEXT-BIT GCD-BIG-BIG)      ;BIGNUM-BIGNUM CASE
(END-DISPATCH)
(LOCALITY I-MEM)

;;; DROP THROUGH ON FIX-FIX CASE (ARGUMENTS IN M-1 M-2)
;;; Clobbers M-1, M-2, M-A, Q-R, M-TEM, A-TEM1.
GCD-FIX-FIX
#+lambda((M-A Q-R) (A-CONSTANT (LOGAND 7777  ;ASSURE SAVE FOR TYPED REGISTER
                                       (OA-LOW-CONTEXT ((BYTE-FIELD 32. 0))))))
#+exp   ((m-a q-r) (a-constant 0)) ;byte-length & rot field for all bits, no shift
        (JUMP-GREATER-OR-EQUAL M-1 A-ZERO XGCD0)        ;TAKE ABS OF ARGS
        ((M-1) SUB M-ZERO A-1)
XGCD0   (JUMP-GREATER-OR-EQUAL M-2 A-ZERO XGCDL)
        ((M-2) SUB M-ZERO A-2)
XGCDL   (JUMP-EQUAL M-2 A-ZERO XGCD5)
        (JUMP-GREATER-THAN M-1 A-2 XGCD1)
        ((M-TEM) M-1)           ;EXCHANGE ARGS SO M-1 IS THE BIGGER
        ((M-1) M-2)
        ((M-2) M-TEM)
XGCD1   (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) M-1 XGCD2)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) M-2 XGCD3)
        ((M-A) SUB M-A (A-CONSTANT 37))                 ;BOTH EVEN
                                                        ;ADD1 TO ROTATE FIELD, SUB1 FROM LENGTH
        ((M-2) M-2 OUTPUT-SELECTOR-RIGHTSHIFT-1)
XGCD3   (JUMP-XCT-NEXT XGCDL)                           ;M-1 EVEN
       ((M-1) M-1 OUTPUT-SELECTOR-RIGHTSHIFT-1)

XGCD2   (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) M-2 XGCD4)
        (JUMP-XCT-NEXT XGCDL)                           ;M-2 EVEN
       ((M-2) M-2 OUTPUT-SELECTOR-RIGHTSHIFT-1)

XGCD4   ((M-TEM) M-2)           ;BOTH ODD
        ((M-2) SUB M-1 A-2)
        (JUMP-XCT-NEXT XGCDL)
       ((M-1) M-TEM)

XGCD5
#+exp   ((m-tem3) add m-a (a-constant 1_5))
                ;Final shifting step
#+exp   ((OA-REG-LOW) (Byte-Field 10. 0) M-A)
#+lambda((oa-reg-low) m-a)
        ((M-1) DPB M-1 (BYTE-FIELD 0 0) A-ZERO)
        (JUMP RETURN-M-1)

;BIGNUM GCD MOVED TO UC-HACKS FILE.

;;;This takes a stream of 31. bit words and right justifies it
;;; into the bignum in M-S. You hand words in in M-2.
;;; After each call M-E "points" to the location about to be stored into or 0
;;; if no ones have been found, M-3 is the number of 31. bit words of zeros skipped,
;;; M-4 is the number of bits skipped mod 31. M-A and M-B are used for internal
;;; constants for ldbing and dpbing. Temporary things are kept in M-1 as well
;;; Inits: ((M-E) A-ZERO)       ;flags that no 1s have been found.
;;;        ((M-3) A-MINUS-ONE)  ;Actually init to anything you want, it will be
;;;                             ; incremented N+1 times.
BIGNUM-RIGHT-JUST
        (JUMP-EQUAL M-E A-ZERO BIGNUM-RIGHT-JUST-FFO)
        (JUMP-EQUAL M-4 A-ZERO BIGNUM-RIGHT-JUST-PUNT)
#+exp   ((m-tem3) add m-a (a-constant 1_5))
#+exp   ((oa-reg-low) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-A)
        ((MD) DPB M-2 (BYTE-FIELD 0 0) A-1)
        ((VMA-START-WRITE) ADD M-S A-E)
        (CHECK-PAGE-WRITE-UNBOXED)
#+exp   ((m-tem3) add m-b (a-constant 1_5))
#+exp   ((oa-reg-low) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-B)
        (POPJ-AFTER-NEXT (M-1) (BYTE-FIELD 0 0) M-2 A-ZERO)
       ((M-E) ADD M-E (A-CONSTANT 1))

BIGNUM-RIGHT-JUST-PUNT
        ((MD) M-1)
        ((VMA-START-WRITE) ADD M-S A-E)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ-AFTER-NEXT (M-1) M-2)
       ((M-E) ADD M-E (A-CONSTANT 1))

BIGNUM-RIGHT-JUST-FFO
        (POPJ-EQUAL-XCT-NEXT M-2 A-ZERO)
       ((M-3) ADD M-3 (A-CONSTANT 1))
        ((M-E) (A-CONSTANT 1))
        ((M-4) A-MINUS-ONE)
        ((M-2) DPB M-2 (BYTE-FIELD 31. 1) A-ZERO)
BIGNUM-RIGHT-JUST-FFO-1
        ((M-2) (BYTE-FIELD 31. 1) M-2 A-ZERO)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 0) M-2 BIGNUM-RIGHT-JUST-FFO-1)
       ((M-4) ADD M-4 (A-CONSTANT 1))
        ((M-1) M-2)
        ;;Now for DPB (M-A) we need BYTL-1 = M-4 - 1   and MROT = 31. - M-4
        ;;and for LDB (M-B) we need BYTL-1 = 30. - M-4 and MROT = 32. - M-4
        ((M-TEM) (A-CONSTANT 30.))
        ((M-TEM) SUB M-TEM A-4)                 ; 30. - M-4
        ((M-A) ADD M-TEM (A-CONSTANT 1))
        ((M-B) ADD M-TEM (A-CONSTANT 2))
#+exp   ((M-TEM) M+1 M-TEM)
        ((M-B) DPB M-TEM OAL-BYTL-1 A-B)
        (POPJ-AFTER-NEXT (M-TEM) SUB M-4 #+lambda (A-CONSTANT 1) #+exp a-zero)
       ((M-A) DPB M-TEM OAL-BYTL-1 A-A)

XREM (MISC-INST-ENTRY \)
        (CALL-XCT-NEXT XFLOOR-2-A)
       ((M-1) (A-CONSTANT 2))                           ;TRUNCATE opcode.
        (POPJ-AFTER-NEXT
          (M-T) Q-TYPED-POINTER PDL-POP)
       ((M-GARBAGE) PDL-POP)

;       (CALL-XCT-NEXT GET-FIX-OR-BIGNUM)
;       ((M-A) M-ZERO)
;       (DISPATCH (BYTE-FIELD 2 0) M-A REMAINDER-DISPATCH)
;;;; DROP THROUGH ON FIX-FIX CASE
;REMAINDER-FIX-FIX
;       (CALL DIV)
;XREM1  (POPJ-AFTER-NEXT
;        (M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       (NO-OP)

;(LOCALITY D-MEM)
;(START-DISPATCH 2)
;REMAINDER-DISPATCH
;       (P-BIT R-BIT)           ;FIXNUM-FIXNUM CASE (DROPS THROUGH)
;       (INHIBIT-XCT-NEXT-BIT REMAINDER-FIX-BIG)        ;FIXNUM-BIGNUM CASE
;       (INHIBIT-XCT-NEXT-BIT REMAINDER-BIG-FIX)        ;BIGNUM-FIXNUM CASE
;       (INHIBIT-XCT-NEXT-BIT REMAINDER-BIG-BIG)        ;BIGNUM-BIGNUM CASE
;(END-DISPATCH)
;(LOCALITY I-MEM)

;;;; THE VALUE IS ALWAYS THE FIXNUM EXCEPT WHEN THE FIXNUM IS "SETZ" AND THE BIGNUM IS
;;;; POSITIVE "SETZ", IN WHICH CASE THE ANSWER IS 0 (THIS DEPENDS ON THE HEADER FOR THE
;;;; BIGNUM BEING IN MD)
;REMAINDER-FIX-BIG
;       (POPJ-NOT-EQUAL-XCT-NEXT M-2 (A-CONSTANT NEGATIVE-SETZ))
;       ((M-T) M-C)                     ;RESULT IS THE FIXNUM, USUALLY
;       ((M-1) BIGNUM-HEADER-LENGTH MD) ;GET THE LENGTH OF THE BIGNUM
;       (POPJ-NOT-EQUAL M-1 (A-CONSTANT 1))
;       ((VMA-START-READ) ADD M-B (A-CONSTANT 1))       ;READ THE BIGNUM
;       (CHECK-PAGE-READ)
;       (POPJ-AFTER-NEXT
;         POPJ-NOT-EQUAL MD (A-CONSTANT POSITIVE-SETZ))
;       ((M-T) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))   ;RESULT IS 0

;;; HERE THE BIGNUM IS IN M-B, FIXNUM IN M-2
;;; M-1 IS USED FOR ACCUMULATOR M-B IS THE POINTER TO THE
;;; BIGNUM, M-C IS THE LOOP COUNTER (INITED WITH THE LENGTH OF THE BIGNUM) (ALSO OFFSET)
;;; M-A IS THE SIGN BIT OF THE BIGNUM
;;; REMAINDER MUST BE LEFT IN M-1 FOR THE SAKE OF GCD-BIG-FIX AND REMAINDER-BIG-BIG
;;;People will want to call REMAINDER-BIG-FIX-1 with a fixnum in M-2 a bignum in M-B
;;; its length in M-C and sign bit in the low bit of M-A. REMAINDER-BIG-FIX-1 Doesn't
;;; work if the fixnum is 0! (You must deal with that yourself.)
REMAINDER-BIG-FIX
        (JUMP-EQUAL M-2 A-ZERO RETURN-M-B)
        ((M-C) BIGNUM-HEADER-LENGTH MD)
        ((M-A) BIGNUM-HEADER-SIGN MD)
REMAINDER-BIG-FIX-1
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-2 A-ZERO REM-BIG-FIX-LOOP)
       ((M-1) SETZ)
        ((M-2) SUB M-ZERO A-2)          ;TAKE ABS OF DIVISOR
REM-BIG-FIX-LOOP
        ((VMA-START-READ) ADD M-B A-C)
        (CHECK-PAGE-READ)
        ((M-TEM1) SETZ)                 ;IMPLICIT ARGUMENT TO DIV1A
        ((M-TEM) DPB M-1 (BYTE-FIELD 1 31.) A-ZERO)
        ((Q-R) ADD MD A-TEM)
        (JUMP-IF-BIT-SET-XCT-NEXT (BYTE-FIELD 1 31.) M-TEM REM-BIG-FIX-OVFL)
       ((M-1) (BYTE-FIELD 31. 1) M-1)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 31.) Q-R REM-BIG-FIX-OVFL)
        ((M-1) ADD M-1 (A-CONSTANT 1))
;;; HERE M-1,,Q-R HAVE (M-1)*1_31.+MD
REM-BIG-FIX-OVFL
#-exp(begin-comment)
        (Jump-greater-or-equal-xct-next M-2 A-Zero REM-BIG-FIX-Not-Neg-2)
       ((m-tem2) M-2)
        ((M-2) SUB M-Zero A-2)
REM-BIG-FIX-Not-Neg-2
#-exp(end-comment)
        (CALL-XCT-NEXT DIV1A)
       ((M-1) DIVIDE-FIRST-STEP M-1 A-2)
        (JUMP-NOT-EQUAL-XCT-NEXT M-C (A-CONSTANT 1) REM-BIG-FIX-LOOP)
       ((M-C) SUB M-C (A-CONSTANT 1))
        (POPJ-EQUAL-XCT-NEXT M-A A-ZERO)        ;POPJ IF DIVIDEND POSITIVE
       ((M-T) DPB Q-POINTER M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (POPJ-AFTER-NEXT (M-1) SUB M-ZERO A-1)
       ((M-T) DPB Q-POINTER M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

RETURN-M-B
        (POPJ-AFTER-NEXT (M-T) M-B)
       (NO-OP)

;;; RETURNS IN M-A BITS SAYING WHAT THE TWO ARGUMENTS ARE (FOR \ AND \\)
;;; IN FIXNUM-FIXNUM CASE RETURNS M-A UNCHANGED (0) AND FIXNUMS IN M-1 AND M-2 (SECOND)
;;; IN THE BIGNUM-FIXNUM AND FIXNUM-BIGNUM CASE, IT RETURNS THE FIXNUM IN M-2 AND THE
;;; BIGNUM IN M-B. IN THE BIGNUM-BIGNUM CASE, IT RETURNS THE BIGNUMS IN M-B AND M-C
;;; IN ANY CASE M-T IS THE SECOND ARGUMENT, M-C IS THE FIRST
GET-FIX-OR-BIGNUM
        ((M-T) C-PDL-BUFFER-POINTER-POP)
        ((M-1) SELECTIVE-DEPOSIT M-T Q-DATA-TYPE A-ZERO)
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                        GET-ANY-BIG)
       ((M-C) C-PDL-BUFFER-POINTER-POP)
GET-ANY-CHAR
#+lambda((OA-REG-HIGH) BOXED-SIGN-BIT M-T)              ;SIGN EXTEND (MUNG M SOURCE)
#+exp   ((m-tem3) boxed-sign-bit m-t)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-2) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-T)
        ((M-1) SELECTIVE-DEPOSIT M-C Q-DATA-TYPE A-ZERO)
        (JUMP-NOT-EQUAL M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) GET-BIG-FIX)
GET-CHAR-FIX
#+lambda(POPJ-AFTER-NEXT
         (OA-REG-HIGH) BOXED-SIGN-BIT M-C)              ;SIGN EXTEND (MUNG M SOURCE)
#+exp   ((m-tem3) boxed-sign-bit m-c)
#+exp   (popj-after-next (oa-reg-high) dpb m-tem3 oah-m-src a-zero)
       ((M-1) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-C)
GET-FIX-ANY
#+lambda(POPJ-AFTER-NEXT
         (OA-REG-HIGH) BOXED-SIGN-BIT M-C)              ;SIGN EXTEND (MUNG M SOURCE)
#+exp   ((m-tem3) boxed-sign-bit m-c)
#+exp   (popj-after-next (oa-reg-high) dpb m-tem3 oah-m-src a-zero)
       ((M-2) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-C)

GET-BIG-FIX
        (JUMP-EQUAL M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)) GET-CHAR-FIX)
        ((M-A) (A-CONSTANT 2))
        (CALL-XCT-NEXT ASSURE-BIGNUM)
       ((M-I) M-C)
    (ERROR-TABLE ARG-POPPED 0 M-C M-T)
        (POPJ-AFTER-NEXT
         (M-C) M-I)
       ((M-B) M-I)

GET-ANY-BIG
        (JUMP-EQUAL M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER))
                    GET-ANY-CHAR)
        ((M-A) (A-CONSTANT 1))
        (CALL-XCT-NEXT ASSURE-BIGNUM)
       ((M-I) M-T)
    (ERROR-TABLE ARG-POPPED 0 M-C M-T)
        ((M-T) M-I)
        ((M-1) SELECTIVE-DEPOSIT M-C Q-DATA-TYPE A-ZERO)
        (JUMP-EQUAL-XCT-NEXT M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                        GET-FIX-ANY)
       ((M-B) M-T)              ;THIS IS THE SECOND ARGUMENT BIGNUM
        (JUMP-EQUAL M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER))
                        GET-FIX-ANY)
;;; HERE THEY ARE BIG-BIG
        ((M-A) (A-CONSTANT 3))
        (CALL-XCT-NEXT ASSURE-BIGNUM)
       ((M-I) M-C)
    (ERROR-TABLE ARG-POPPED 0 M-C M-T)
        (POPJ-AFTER-NEXT
         (M-C) M-I)
        (NO-OP)

;;;   ASSURES THAT THE HEADER NOW BEING READ INTO MD POINTS TO A LEGAL BIGNUM HEADER
;;;   VMA AND M-I CONTAIN POINTER TO THE BIGNUM
;;; NOTE: This scheme didn't check the pointer data-type until after the memory cycle
;;; started.  Since other data-types (ie: small-flonum) occasionally elide previous
;;; type-checking, this marginality was not deemed acceptable.  The code now takes the
;;; argument in M-I, which is type-checked before it is referenced.  KHS 10/13/84.
ASSURE-BIGNUM
        (CALL-DATA-TYPE-NOT-EQUAL M-I
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)) TRAP)
                (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) M-I NIL)
        ((VMA-START-READ) M-I)
        (CHECK-PAGE-READ)                       ;CHECK FOR PAGE FAULTS
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-I) VMA)             ;get transported number address
        ((M-TEM) SELECTIVE-DEPOSIT MD HEADER-TYPE-FIELD A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM)))
        (CALL TRAP)
                (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) M-I NIL)

;;; Takes a fixnum or bignum argument on the stack and returns the low-order 32
;;; bits of it in M-1.  Bashes M-I, M-J only.
GET-32-BITS  (declare (values a-1) (clobbers a-i a-j))
        (JUMP-DATA-TYPE-EQUAL C-PDL-BUFFER-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                FXUNPK-P-1)
        (JUMP-DATA-TYPE-EQUAL C-PDL-BUFFER-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER))
                FXUNPK-P-1)
        (CALL-XCT-NEXT ASSURE-BIGNUM)
       ((M-I) C-PDL-BUFFER-POINTER-POP)
        ((M-I) BIGNUM-HEADER-LENGTH MD)
        ((M-J) BIGNUM-HEADER-SIGN MD)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;Low-order word
        (CHECK-PAGE-READ)
        (JUMP-LESS-THAN-XCT-NEXT M-I (A-CONSTANT 2) GET-32-BITS-1)
       ((M-1) READ-MEMORY-DATA)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-1) DPB READ-MEMORY-DATA (BYTE-FIELD 1 31.) A-1)
GET-32-BITS-1
        (POPJ-AFTER-NEXT POPJ-EQUAL M-J A-ZERO)
       ((M-1) SUB M-ZERO A-1)                           ;Negative

;;; Operations on unsigned quantities.

;TEMPORARY DOUBLE PRECISION KLUDGE.  DOESN'T CHECK FOR OVERFLOW (PRESUMABLY CAN'T ANYWAY!)
XMUL-FRACTIONS (MISC-INST-ENTRY %MULTIPLY-FRACTIONS)
        (CALL FXGTPP)
   (ERROR-TABLE CALLS-SUB %MULTIPLY-FRACTIONS)
        (CALL-XCT-NEXT MPY)
       ((Q-R) M-2)
        (POPJ-AFTER-NEXT
         (M-T)
         (BYTE-FIELD (DIFFERENCE 32. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 0))
         Q-R (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((M-T) DPB M-2 (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH
                                              (DIFFERENCE 32. Q-POINTER-WIDTH))
                                  (DIFFERENCE 32. Q-POINTER-WIDTH))
                  A-T)

XPOINTER-TIMES (MISC-INST-ENTRY %POINTER-TIMES)
        (CALL FXGTPP)
    (ERROR-TABLE CALLS-SUB %24-BIT-TIMES)
        (CALL-XCT-NEXT MPY)
       ((Q-R) M-2)
        (POPJ-AFTER-NEXT (M-T) DPB Q-R Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       (NO-OP)

;SPECIAL NON-OVERFLOW-CHECKING FUNCTIONS FOR WEIRD HACKS
X24ADD (MISC-INST-ENTRY %24-BIT-PLUS)
        (CALL FXGTPP)
   (ERROR-TABLE CALLS-SUB %24-BIT-PLUS)
        (POPJ-AFTER-NEXT (M-1) ADD M-1 A-2)
       ((M-T) DPB M-1 (BYTE-FIELD 24. 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

X24SUB (MISC-INST-ENTRY %24-BIT-DIFFERENCE)
        (CALL FXGTPP)
   (ERROR-TABLE CALLS-SUB %24-BIT-DIFFERENCE)
        (POPJ-AFTER-NEXT (M-1) SUB M-1 A-2)
       ((M-T) DPB M-1 (BYTE-FIELD 24. 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

X24MUL (MISC-INST-ENTRY %24-BIT-TIMES)
        (CALL FXGTPP)
    (ERROR-TABLE CALLS-SUB %24-BIT-TIMES)
        (CALL-XCT-NEXT MPY)
       ((Q-R) M-2)
        (POPJ-AFTER-NEXT (M-T) DPB Q-R (BYTE-FIELD 24. 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       (NO-OP)

XDIV-DOUBLE (MISC-INST-ENTRY %DIVIDE-DOUBLE)
        (CALL XDIVD1)                                   ;CALL DOUBLE PRECISION DIVIDE
        (POPJ-AFTER-NEXT                ;DIVIDE CAN'T OVERFLOW
         (M-T) DPB Q-R Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       (NO-OP)

XREM-DOUBLE (MISC-INST-ENTRY %REMAINDER-DOUBLE)
        (call xdivd1)
        (popj-after-next
          (m-t) dpb m-1 q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (no-op)

;       (JUMP-XCT-NEXT XREM1)
;       (CALL XDIVD1)                                   ;CALL DOUBLE PRECISION DIVIDE
;XREM1  (POPJ-AFTER-NEXT
;        (M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       (NO-OP)

;DOUBLE PRECISION DIVIDE.  ARGS ON PDL ARE DIVIDEND HIGH, DIVIDEND LOW, DIVISOR
XDIVD1  (CALL FXGTPP)                                   ;M-1 GETS DIVIDEND LOW, M-2 DIVISOR
        ((M-A) M-1)                                     ;SAVE DIVIDEND LOW
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)            ;GET DIVIDEND HIGH.
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
    (ERROR-TABLE ARG-POPPED 0 PP M-A M-2)
        ((m-1) output-selector-extend-25 c-pdl-buffer-pointer-pop)
        ((M-TEM) DPB M-1
         (BYTE-FIELD (DIFFERENCE 32. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 0))
         A-A)   ;LOW WORD HAS 32 BITS
        ((M-A)
         (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 0)
                     (DIFFERENCE 32. Q-POINTER-WIDTH))
         M-1 A-1)               ;ARITH SHIFT M-1 RIGHT 8 FOR HIGH WORD
        (JUMP-GREATER-OR-EQUAL M-1 A-ZERO XDIVD3)       ;MAKE DIVIDEND POSITIVE
        (JUMP-NOT-EQUAL-XCT-NEXT M-TEM A-ZERO XDIVD2)   ;DOUBLE PRECISION NEGATE M-A,,M-TEM
       ((M-TEM) SUB M-ZERO A-TEM)
        ((M-A) SUB M-A (A-CONSTANT 1))                  ;BORROW IF LOW WORD IS ZERO
XDIVD2  ((M-A) SETCM M-A)                               ;ONES COMPLEMENT HIGH WORD
XDIVD3  ;DIVIDEND IS IN M-A (HIGH), M-TEM (LOW), DIVISOR IS IN M-2
        ((M-TEM1) M-1)  ;ORIGINAL SIGN OF DIVIDEND IS IN SIGN(M-TEM1) FOR DIVIDE
        ((Q-R) M-TEM)                                   ;LOW DIVIDEND TO Q-R FOR DIVIDE
#-exp(begin-comment)
        (Jump-greater-or-equal-xct-next M-2 A-Zero XDIVD3-Not-Neg-2)
       ((m-tem2) M-2)
        ((M-2) SUB M-Zero A-2)
XDIVD3-Not-Neg-2
#-exp(end-comment)
        (JUMP-XCT-NEXT DIV1A)                           ;JOIN NORMAL DIVIDE ROUTINE
       ((M-1) DIVIDE-FIRST-STEP M-A A-2)                ;BUT WITH DIFFERENT FIRST STEP

;;; ARITHMETIC MICROCODE.

;Generic operations save away one of these codes to indicate the operation to
;be performed, and then jump to routines that think about types and unpacking.
(ASSIGN ARITH-1ARG-ABS 0)
(ASSIGN ARITH-1ARG-MINUS 1)
(ASSIGN ARITH-1ARG-ZEROP 2)
(ASSIGN ARITH-1ARG-PLUSP 3)
(ASSIGN ARITH-1ARG-MINUSP 4)
(ASSIGN ARITH-1ARG-ADD1 5)
(ASSIGN ARITH-1ARG-SUB1 6)
(ASSIGN ARITH-1ARG-FIX 7)
(ASSIGN ARITH-1ARG-FLOAT 10)
(ASSIGN ARITH-1ARG-SMALL-FLOAT 11)
(ASSIGN ARITH-1ARG-HAULONG 12)
(ASSIGN ARITH-1ARG-LDB 13)      ;DEALS WITH 2ND ARG ONLY.
(ASSIGN ARITH-1ARG-DPB 14)      ;DEALS WITH 3RD ARG ONLY.
(ASSIGN ARITH-1ARG-ASH 15)
(ASSIGN ARITH-1ARG-ODDP 16)
(ASSIGN ARITH-1ARG-EVENP 17)
;HAIPART?
(ASSIGN NUM-UNUSED-ARITH-1ARGS 0)

;These go through the dispatches just like FIX,
;but the FIX routines do a subdispatch to decide how to round.
(ASSIGN ARITH-1ARG-FLOOR 7)
(ASSIGN ARITH-1ARG-CEIL 107)
(ASSIGN ARITH-1ARG-TRUNC 207)
(ASSIGN ARITH-1ARG-ROUND 307)
(DEF-DATA-FIELD ARITH-FIX-ROUNDING-MODE-FIELD 2 6)  ;these bits come from the destination field
                ;of the INTERNAL-FLOOR-1 and INTERNAL-FLOOR-2 macroinstructions.

(ASSIGN ARITH-2ARG-ADD 0)
(ASSIGN ARITH-2ARG-SUB 1)
(ASSIGN ARITH-2ARG-MUL 2)
(ASSIGN ARITH-2ARG-IDIV 3)
(ASSIGN ARITH-2ARG-EQUAL 4)             ;=
(ASSIGN ARITH-2ARG-GREATERP 5)
(ASSIGN ARITH-2ARG-LESSP 6)
(ASSIGN ARITH-2ARG-MIN 7)
(ASSIGN ARITH-2ARG-MAX 10)
(ASSIGN ARITH-2ARG-BOOLE 11)
(ASSIGN ARITH-2ARG-DIV 12)
(ASSIGN ARITH-2ARG-EQL 13)      ;EQL.  Differs from = on complex numbers.
(ASSIGN NUM-UNUSED-ARITH-2ARGS 4)
;REMAINDER, EXPT?

;These codes are used to save the type of the first numeric argument in dyadic
;operations, so that the routines for handling various types of second arguments
;can dispatch on them.
(ASSIGN NUMBER-CODE-FIXNUM 0)
(ASSIGN NUMBER-CODE-SMALL-FLONUM 1)
(ASSIGN NUMBER-CODE-FLONUM 2)
(ASSIGN NUMBER-CODE-BIGNUM 3)
(ASSIGN NUM-UNUSED-NUMBER-CODES 4)

;This is the format of all DTP-HEADER words.
(DEF-DATA-FIELD HEADER-TYPE-FIELD 5 19.)
(DEF-DATA-FIELD HEADER-REST-FIELD 19. 0)
(ASSIGN-EVAL NUM-UNUSED-HEADER-TYPES (EVAL (- 32. (LENGTH Q-HEADER-TYPES))))

;This is how flonums are stored in a header, and how to convert from internal
;form (see below) back into flonum form.
(DEF-DATA-FIELD HEADER-FLONUM-EXPONENT 11. 8.)
(DEF-DATA-FIELD HEADER-FLONUM-HIGH-MANTISSA 8. 0)
(DEF-DATA-FIELD FLONUM-HEADER-HIGH-MANTISSA 8. 24.)
(DEF-DATA-FIELD FLONUM-HEADER-LOW-MANTISSA 24. 0)

;Small-flonum definitions.  These are inums, with a DTP-SMALL-FLONUM data type,
;an 8-bit excess-200 exponent (10^-38 to 10^+38 approximately), and a
;17-bit 2's complement normalized mantissa (5 digits approximately).  The
;sign bit is elided since it is always the complement of the high bit of
;the mantissa, except for zero, which is represented as an all-zero exponent
;and mantissa.

(DEF-DATA-FIELD SMALL-FLONUM-EXPONENT 8 17.)    ;The exponent in a small-flonum
(ASSIGN SMALL-FLONUM-EXPONENT-OFFSET 1600)      ;To convert from excess-200 to excess-2000
(ASSIGN SMALL-FLONUM-MAX-EXPONENT 377)          ;Largest value that fits in exponent field
(DEF-DATA-FIELD FLONUM-SMALL-MANTISSA-FIELD 17. 14.) ;DPB here to put into low-level form
(DEF-DATA-FIELD FLONUM-SMALL-USELESS-BITS 14. 0) ;Low-order discarded bits of mantissa
(DEF-DATA-FIELD FLONUM-SMALL-ROUND-BIT 1 13.)   ;Highest discarded bit
(DEF-DATA-FIELD FLONUM-SMALL-GUARD-BITS 13. 0)  ;The remaining discarded bits
(DEF-DATA-FIELD FLONUM-SMALL-MANTISSA-LOW-BIT 1 14.)
(DEF-DATA-FIELD SMALL-FLONUM-MANTISSA-HIGH-BIT 1 16.)

;Both flonums and small flonums are converted to an internal
;format, on which the subrouines FADD, FSUB, FMPY, FDIV, etc. work.
;Those routines are also intended to be useful for hairier functions
;such as series expansions when written in microcode.

;These routines operate on numbers which consist of a 32-bit
;normalized 2's complement mantissa in M-1 or M-2 and an excess-2000
;exponent in M-I or M-J.  The binary point is just to the right
;of the sign (bit 31).  The range of mantissas is
;1/2 <= f < 1, -1/2 > f >= -1, except for zero which has a zero
;mantissa and a zero exponent.  All results are normalized and
;properly rounded, and returned in M-1 and M-I.  Overflow and underflow
;are not detected at this level, which is a feature.  Fuzz is not
;hacked.  Rounding is towards even if the discarded bits = exactly 1/2 lsb.

;Definitions for low-level form

(DEF-DATA-FIELD FLONUM-SIGN-BIT 1 31.)
(DEF-DATA-FIELD MANTISSA-HIGH-BIT 1 30.)
(ASSIGN FLONUM-EXPONENT-EXCESS 2000)    ;The exponent is excess-2000.
(DEF-DATA-FIELD SIGN-BIT-AND-MANTISSA-HIGH-THREE 4 28.)
(DEF-DATA-FIELD SIGN-BIT-AND-MANTISSA-HIGH-TWO 3 29.)
(DEF-DATA-FIELD SIGN-BIT-AND-MANTISSA-HIGH-BIT 2 30.)

;;; Packing and unpacking fixnums.

FXUNPK-P-1
        (popj-after-next (M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (no-op)
SIGN-EXTEND-M-1
#+lambda(POPJ-AFTER-NEXT
                (OA-REG-HIGH) BOXED-SIGN-BIT M-1)
#+exp   ((m-tem3) boxed-sign-bit m-1)
#+exp   (popj-after-next (oa-reg-high) dpb m-tem3 oah-m-src a-zero)
       ((M-1) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-1)

FXUNPK-T-2
#+lambda(POPJ-AFTER-NEXT
                (OA-REG-HIGH) BOXED-SIGN-BIT M-T)
#+exp   ((m-tem3) boxed-sign-bit m-t)
#+exp   (popj-after-next (oa-reg-high) dpb m-tem3 oah-m-src a-zero)
       ((M-2) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-T)

;;; Come to one of these to return a fixnum in M-1.
;;; Checks for fixnum overflow, and adds data type DTP-FIX.
;;; Result goes to M-T, and FIXPACK-P also pushes it on the PDL.

;;; Return the number in M-1 as either a fixnum or a bignum depending on its magnitude
;;; Returns it via M-T
RETURN-M-1
        (JUMP-LESS-THAN M-1 (A-CONSTANT NEGATIVE-SETZ) FIX-OVERFLOW-1)
        (JUMP-GREATER-OR-EQUAL M-1 (A-CONSTANT POSITIVE-SETZ) FIX-OVERFLOW-1)
        ;drop into FIXPACK-T

;;; Return it via M-T checking only for single-bit overflow.  This is also open coded.
FIXPACK-T
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;;; Return it via pdl checking only for single-bit overflow
FIXPACK-P
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))
(LOCALITY D-MEM)
;DISPATCH TABLE FOR CHECKING FOR SINGLE-BIT ADD/SUBTRACT-TYPE FIXNUM OVERFLOW
;ON VALUE WHICH IS UNBOXED IN M-1.  DISPATCH ON SIGN BIT AND LOW DATA TYPE BIT.
;I-ARG SHOULD BE 0 IF RESULT ONLY TO M-T, OR 1 IF ALSO TO PDL.
;IN ANY CASE, DOES ESSENTIALLY POPJ-XCT-NEXT.
;NEXT SHOULD BE INSTRUCTION TO BOX M-1 AS A FIXNUM.
(START-DISPATCH 2 0)
D-FXOVCK
        (R-BIT)                                  ;BITS AGREE NO OVERFLOW
        (FIX-OVERFLOW INHIBIT-XCT-NEXT-BIT)      ;DISAGREE => OVERFLOW
        (FIX-OVERFLOW INHIBIT-XCT-NEXT-BIT)      ;DISAGREE => OVERFLOW
        (R-BIT)                                  ;BITS AGREE NO OVERFLOW
(END-DISPATCH)

(begin-comment) ;this is part of the scheme that unfortunately doesnt work out.
;This one used after doing OUTPUT-SELECTOR-MASK-25 ADD or SUB.  Q has full result,
;M-T and C-PDL-BUFFER-POINTER have DTP-FIX and low 25 bits of result. Before the operation,
;the data types of both operands were known to be DTP-FIX.
;  Consider add:  There are two cases, either the operands were the same sign or not.
;If the original operands were the same sign, it is possible to have a real overflow, otherwise
;not.  Note that Q.25 is a true sum bit
;(not interfered with by the data-types).  (On an ADD the tag field has been shifted
;left one.  On a SUB it has been cancelled out).  On overflow then, Q.25 should be replicated
;in bits 31. thru 25. of M-T, and the result made into a bignum.  I-ARG should be 1
;as result is desired both on PDL and in M-T.  Note, however, that a semi-garbage result
;is ALREADY on PDL at this time.  (In case of D-FXOVCK, the INHIBIT-XCT-NEXT-BIT stops
;the semi-garbage result from getting to the PDL, but that doesnt work out here.)
;If the original operands were of opposite signs, there is no possibility of a real overflow.
;However, since we are adding 25 bit non-sign-extended quantities, our test to detect
;overflow that boxed sign bit and the bit above it are different may go off.

(start-dispatch 2 0)
d-lambda-fast-overflow-check
        (r-bit)
        (lambda-fast-fix-overflow inhibit-xct-next-bit)
        (lambda-fast-fix-overflow inhibit-xct-next-bit)
        (r-bit)
(end-dispatch)
(end-comment)
(LOCALITY I-MEM)

;;; This is called from the fixnum packing routines. M-1 contains a unboxed number
;;; IARG is 0 if the result is to go only to M-T, and 1 if it should also go to the
;;; PDL
FIX-OVERFLOW
        (JUMP-EQUAL READ-I-ARG A-ZERO FIX-OVERFLOW-1)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
FIX-OVERFLOW-1  ;Enter directly here with unboxed number in M-1.  Returns bignum in M-T.
        ((M-C) M-ZERO)                          ;sign bit
        (JUMP-GREATER-THAN-XCT-NEXT M-1 A-ZERO OVERFLOW-BIGNUM-CREATE)
       ((M-2) M-ZERO)
        (JUMP-XCT-NEXT OVERFLOW-BIGNUM-CREATE-NEGATIVE)
       ((M-1) SUB M-ZERO A-1)

RETURN-M-1-UNSIGNED
        (JUMP-LESS-THAN M-1 A-ZERO FIX-OVERFLOW-1-UNSIGNED)     ;ANY NEGATIVE NUMBER
        (JUMP-GREATER-OR-EQUAL M-1 (A-CONSTANT POSITIVE-SETZ) FIX-OVERFLOW-1-UNSIGNED)
        (POPJ-AFTER-NEXT
         (M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       (NO-OP)

FIX-OVERFLOW-1-UNSIGNED ;Enter directly here with unboxed number in M-1.  Returns bignum in M-T.
        ((M-C) M-ZERO)                          ;sign bit
        (JUMP-XCT-NEXT M-1 A-ZERO OVERFLOW-BIGNUM-CREATE)
       ((M-2) M-ZERO)

(begin-comment) ;this is part of the scheme that unfortunately doesnt win.
lambda-fast-fix-overflow
        ((m-2) xor m-1 a-t)             ;xor signs of original operands.
        (popj-if-bit-set-xct-next boxed-sign-bit m-2)  ;return on false overflow, all ok.
       ((m-t) q-typed-pointer c-pdl-buffer-pointer)
        ((MICRO-STACK-DATA-PUSH) SETA C-PDL-BUFFER-POINTER-POP  ;FLUSH SEMI-GARBAGE.
                (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
  ;Q.25 has true sum bit, replicate it.
#+lambda((OA-REG-HIGH) BIT-ABOVE-BOXED-SIGN-BIT Q-R)            ;SIGN EXTEND (MUNG M SOURCE)
#+exp   ((m-tem3) bit-above-boxed-sign-bit q-r)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-1) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-T)
        ((M-C) M-ZERO)                          ;sign bit
        (JUMP-GREATER-THAN-XCT-NEXT M-1 A-ZERO OVERFLOW-BIGNUM-CREATE)
       ((M-2) M-ZERO)
        (JUMP-XCT-NEXT OVERFLOW-BIGNUM-CREATE-NEGATIVE)
       ((M-1) SUB M-ZERO A-1)
(end-comment)

;;; These return here before returning a value. This puts value from M-T
;;; also on stack for those that need it
M-T-TO-CPDL
        (POPJ-AFTER-NEXT
         (C-PDL-BUFFER-POINTER-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
                                         (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
       (NO-OP)

;;; This is called from the fixnum multiply. M-2 contains the high product
;;; and M-1 the low product. Result is to go to the PDL and M-T.
FIX-2-WORD-OVERFLOW
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
FIX-2-WORD-OVERFLOW-TO-M-T
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-2 A-ZERO OVERFLOW-BIGNUM-CREATE)
       ((M-C) M-ZERO)                           ;sign bit
        ((M-1) SUB M-ZERO A-1)
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-ZERO OVERFLOW-BIGNUM-CREATE-NEGATIVE)
       ((M-2) M-A-1 M-ZERO A-2)         ;ONE'S COMPLEMENT
        ((M-2) ADD M-2 (A-CONSTANT 1))  ;CARRY FROM LOW TO HIGH WORD
                                        ;DROPS THROUGH
;;; M-2,,M-1 HAS A 64 BIT POSITIVE NUMBER THAT IS A MAX OF 47 BITS OF PRECISION
;;; M-C GETS THE SIGN BIT
;;; M-J GETS LENGTH OF BIGNUM
OVERFLOW-BIGNUM-CREATE-NEGATIVE
        ((M-C) SELECTIVE-DEPOSIT M-MINUS-ONE BIGNUM-HEADER-SIGN A-ZERO)
OVERFLOW-BIGNUM-CREATE
        ;; We need a 2-word bignum if non-zero bits above the low 31.
        (JUMP-NOT-EQUAL-XCT-NEXT M-2 A-ZERO OVERFLOW-BIGNUM-CREATE-1)
       ((M-J) (A-CONSTANT 2))
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 31.) M-1 OVERFLOW-BIGNUM-CREATE-1)
        ((M-J) (A-CONSTANT 1))
OVERFLOW-BIGNUM-CREATE-1
        (CALL-XCT-NEXT BNCONS)                  ;Cons up a bignum
       ((M-B) ADD M-J (A-CONSTANT 1))
        ((VMA) ADD M-T (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) (BYTE-FIELD 31. 0) M-1)  ;Low 31. bits
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ-EQUAL M-J (A-CONSTANT 1))
        ((M-TEM) (BYTE-FIELD 1 31.) M-1)
        ((VMA) ADD VMA (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) DPB M-2 (BYTE-FIELD 31. 1) A-TEM)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ) ;NO POPJ-AFTER-NEXT, MAY BE RETURNING TO MAIN-LOOP

;;; Packing and unpacking small flonums.

;Unpack from C-PDL-BUFFER-POINTER-POP into M-1 and M-I.
SFLUNPK-P-1
        ((M-I) SMALL-FLONUM-EXPONENT C-PDL-BUFFER-POINTER)
        (POPJ-EQUAL-XCT-NEXT M-I A-ZERO FLZERO)         ;zero exponent => this is 0.0
       ((M-1) DPB C-PDL-BUFFER-POINTER-POP FLONUM-SMALL-MANTISSA-FIELD A-ZERO)
        ((M-I) ADD M-I (A-CONSTANT SMALL-FLONUM-EXPONENT-OFFSET))
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET MANTISSA-HIGH-BIT M-1)
       ((M-1) DPB (M-CONSTANT -1) FLONUM-SIGN-BIT A-1)  ;negative => set sign bit

;Unpack from M-T into M-2 and M-J.
SFLUNPK-T-2
        ((M-J) SMALL-FLONUM-EXPONENT M-T)
        (POPJ-EQUAL-XCT-NEXT M-J A-ZERO)        ;zero exponent => this is 0.0
       ((M-2) DPB M-T FLONUM-SMALL-MANTISSA-FIELD A-ZERO)
        ((M-J) ADD M-J (A-CONSTANT SMALL-FLONUM-EXPONENT-OFFSET))
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET SMALL-FLONUM-MANTISSA-HIGH-BIT M-T)
       ((M-2) DPB (M-CONSTANT -1) FLONUM-SIGN-BIT A-2)

;Pack from M-1 and M-I into C-PDL-BUFFER-POINTER-PUSH and M-T, rounding.
SFLPACK-P
        (JUMP-IF-BIT-CLEAR FLONUM-SMALL-ROUND-BIT M-1 SFLPCK1)  ;Jump if no rounding required
        ((M-T) FLONUM-SMALL-GUARD-BITS M-1)     ;Discarded fraction exactly 1/2 lsb?
        (JUMP-NOT-EQUAL M-T A-ZERO SFLPCK0)     ;No, round.
        (JUMP-IF-BIT-CLEAR FLONUM-SMALL-MANTISSA-LOW-BIT M-1 SFLPCK1) ;Yes, round towards even.
SFLPCK0 (CALL-XCT-NEXT FRND1)                   ;Round and renormalize (may bring in two
       ((M-1) ADD M-1 (A-CONSTANT (BYTE-MASK FLONUM-SMALL-ROUND-BIT))   ; garbage bits from Q)
                OUTPUT-SELECTOR-RIGHTSHIFT-1)
SFLPCK1 ((M-1) DPB M-ZERO FLONUM-SMALL-USELESS-BITS A-1) ;clear low-order bits so can test zero
        (POPJ-EQUAL-XCT-NEXT M-1 A-ZERO)        ;Special case 0.0, which has 0 in exponent
       ((M-T C-PDL-BUFFER-POINTER-PUSH)         ;Store mantissa and data-type fields
                FLONUM-SMALL-MANTISSA-FIELD M-1
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-SMALL-FLONUM)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))
        ((M-I) SUB M-I (A-CONSTANT SMALL-FLONUM-EXPONENT-OFFSET))
        (JUMP-LESS-OR-EQUAL M-I A-ZERO SFL-E-UND)       ;Underflow.  ZUNDERFLOW?
        (POPJ-AFTER-NEXT
         (M-T C-PDL-BUFFER-POINTER) DPB M-I SMALL-FLONUM-EXPONENT A-T)
       (CALL-GREATER-THAN M-I (A-CONSTANT SMALL-FLONUM-MAX-EXPONENT) SFL-E-OV) ;Overflow

SFL-E-UND
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-ZUNDERFLOW)
        (CALL-EQUAL M-TEM A-V-NIL TRAP)
            (ERROR-TABLE FLOATING-EXPONENT-UNDERFLOW SFL)
        (POPJ-AFTER-NEXT                                ;Return 0.0s0 instead or if continued
         (M-T C-PDL-BUFFER-POINTER)
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-SMALL-FLONUM)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))
       (NO-OP)

SFL-E-OV (CALL TRAP)
   (ERROR-TABLE FLOATING-EXPONENT-OVERFLOW SFL)

;Pack from M-1 and M-I into M-T, rounding.
SFLPACK-T
        (CALL SFLPACK-P)
        (POPJ-AFTER-NEXT (M-GARBAGE) C-PDL-BUFFER-POINTER-POP)
       (NO-OP)

;;; Packing flonums.

;;; Note: the code to unpack flonums only exists at ARITH-FLO-ANY
;;; and ARITH-ANY-FLO, and is written there.  There is also GET-FLONUM,
;;; a general routine which is not used by the normal arithmetic path.

;;; Take a flonum in M-1/M-I, and return a DTP-EXTENDED-NUMBER to it.
FLOPACK-T
        (CALL FLOPACK)
        (POPJ)  ;May be returning to main loop, can't popj and start-write together

FLOPACK-P
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))
FLOPACK (CALL-LESS-OR-EQUAL M-I A-ZERO FLOPACK-UNDERFLOW)
        (CALL-GREATER-OR-EQUAL M-I (A-CONSTANT 4000) TRAP)
            (ERROR-TABLE FLOATING-EXPONENT-OVERFLOW FLO)

     ;; Allocate two boxed words of structure storage in the number-cons area.
        ((m-b) seta (a-constant 2))
        (call-xct-next allocate-extended-number-storage)
       ((m-a) m-b)

        ((VMA) ADD M-T (A-CONSTANT 1))          ;Write the second word
        ((MD-START-WRITE)
                FLONUM-HEADER-LOW-MANTISSA M-1
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NIL))))
        (CHECK-PAGE-WRITE)
        ((M-TEM) FLONUM-HEADER-HIGH-MANTISSA M-1
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                  (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-FLONUM))))
        ((VMA M-T) Q-POINTER M-T
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
        (POPJ-AFTER-NEXT (MD-START-WRITE) DPB M-I HEADER-FLONUM-EXPONENT A-TEM)
       (CHECK-PAGE-WRITE)

FLOPACK-UNDERFLOW
        (POPJ-EQUAL M-1 A-ZERO)         ;0.0 case: M-I has zero, don't trap
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-ZUNDERFLOW)
        (CALL-EQUAL M-TEM A-V-NIL TRAP)
            (ERROR-TABLE FLOATING-EXPONENT-UNDERFLOW FLO)
        (POPJ-AFTER-NEXT (M-I) A-ZERO)  ;Return 0.0 instead
       ((M-1) A-ZERO)

;;; Given something on stack, return a flonum unpacked into M-I and M-1, doing coercions.
;;; Clobbers only M-T, M-TEM, M-4, M-3 (inside FLOAT-A-BIGNUM)
GET-FLONUM  (ERROR-TABLE RESTART GET-FLONUM)
        (JUMP-DATA-TYPE-NOT-EQUAL C-PDL-BUFFER-POINTER
                 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)) GET-FLONUM-1)
        ((VMA-START-READ) C-PDL-BUFFER-POINTER-POP)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-TEM) HEADER-TYPE-FIELD READ-MEMORY-DATA)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-FLONUM)) GET-FLONUM-2)
        ((M-I) HEADER-FLONUM-EXPONENT READ-MEMORY-DATA)
        ((M-1) DPB READ-MEMORY-DATA FLONUM-HEADER-HIGH-MANTISSA A-ZERO)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT NO-OP)
       ((M-1) SELECTIVE-DEPOSIT READ-MEMORY-DATA FLONUM-HEADER-LOW-MANTISSA A-1)

GET-FLONUM-1
        (JUMP-DATA-TYPE-EQUAL C-PDL-BUFFER-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SMALL-FLONUM)) SFLUNPK-P-1)
        (JUMP-DATA-TYPE-EQUAL C-PDL-BUFFER-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)) GET-FLONUM-1-CHAR)
        (CALL-DATA-TYPE-NOT-EQUAL C-PDL-BUFFER-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) TRAP)
            (ERROR-TABLE ARGTYP NUMBER PP T GET-FLONUM)
GET-FLONUM-1-CHAR
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
       ((M-I) (A-CONSTANT 2036))
        (JUMP-XCT-NEXT FNORM)
       ((Q-R) M-ZERO)

GET-FLONUM-2
        (CALL-NOT-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-BIGNUM)) ILLOP) ;unknown type?
        ((C-PDL-BUFFER-POINTER-PUSH) M-Q)
        ((C-PDL-BUFFER-POINTER-PUSH) M-C)
        ((C-PDL-BUFFER-POINTER-PUSH) M-K)
        ((M-Q) VMA)
        ((M-C) Q-POINTER READ-MEMORY-DATA)
        ((M-I) BIGNUM-HEADER-LENGTH M-C)
        (CALL FLOAT-A-BIGNUM)
        ((M-K) C-PDL-BUFFER-POINTER-POP)
        (POPJ-AFTER-NEXT (M-C) C-PDL-BUFFER-POINTER-POP)
       ((M-Q) C-PDL-BUFFER-POINTER-POP)

XFLOAT-FRACTION (MISC-INST-ENTRY FLOAT-FRACTION)
        (CALL GET-FLONUM)
        (JUMP-XCT-NEXT FLOPACK-T)
       ((M-I) (A-CONSTANT 2000))

XFLOAT-EXPONENT (MISC-INST-ENTRY FLOAT-EXPONENT)
        (CALL GET-FLONUM)
        ((M-1) SUB M-I (A-CONSTANT 2000))
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XSCALE-FLOAT (MISC-INST-ENTRY SCALE-FLOAT)
        (DISPATCH Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM M-T 2)
        (CALL-XCT-NEXT GET-FLONUM)
       ((M-A) Q-TYPED-POINTER PDL-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-A)
        ((M-I) ADD M-I A-2)
                ;make result small float if arg was.
        (JUMP-DATA-TYPE-EQUAL M-A
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SMALL-FLONUM)) SFLPACK-T)
        (JUMP FLOPACK-T)                ;pack M-1/M-I

;;; Simple one-argument operations.

XABS (MISC-INST-ENTRY ABS) (ERROR-TABLE RESTART XABS)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XABS)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-ABS))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (JUMP-GREATER-OR-EQUAL M-1 A-ZERO FIXPACK-T)
        ((M-1) SUB M-ZERO A-1)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XMINUS (MISC-INST-ENTRY MINUS) (ERROR-TABLE RESTART XMINUS)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XMINUS)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-MINUS))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
       ((M-1) SUB M-ZERO A-1)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XZEROP (MISC-INST-ENTRY ZEROP) (ERROR-TABLE RESTART XZEROP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XZEROP zerop)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-ZEROP))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
FLONUM-ZEROP
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-EQUAL M-1 A-ZERO)
       ((M-T) A-V-NIL)

XPLUSP (MISC-INST-ENTRY PLUSP) (ERROR-TABLE RESTART XPLUSP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XPLUSP)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-PLUSP))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
FLONUM-PLUSP
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-GREATER-THAN M-1 A-ZERO)
       ((M-T) A-V-NIL)

XMINUSP (MISC-INST-ENTRY MINUSP) (ERROR-TABLE RESTART XMINUSP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XMINUSP)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-MINUSP))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
FLONUM-MINUSP
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-LESS-THAN M-1 A-ZERO)
       ((M-T) A-V-NIL)

XODDP (MISC-INST-ENTRY ODDP) (ERROR-TABLE RESTART XODDP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XODDP)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-ODDP))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET (BYTE-FIELD 1 0) M-1)
       ((M-T) A-V-NIL)

XEVENP (MISC-INST-ENTRY EVENP) (ERROR-TABLE RESTART XEVENP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XEVENP)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-EVENP))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-IF-BIT-CLEAR (BYTE-FIELD 1 0) M-1)
       ((M-T) A-V-NIL)

X1PLS (MISC-INST-ENTRY 1+) ;ADD1 GETS FSET TO THIS
            (ERROR-TABLE RESTART X1PLS)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T X1PLS)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-ADD1))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-1) ADD M-1 (A-CONSTANT 1))
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

X1MNS (MISC-INST-ENTRY 1-) ;SUB1 GETS FSET TO THIS
            (ERROR-TABLE RESTART X1MNS)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T X1MNS)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-SUB1))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-1) SUB M-1 (A-CONSTANT 1))
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XFIX (MISC-INST-ENTRY FIX) (ERROR-TABLE RESTART XFIX)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XFIX)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-FIX))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XINTERNAL-FLOAT (MISC-INST-ENTRY INTERNAL-FLOAT)
XFLOAT (MISC-INST-ENTRY FLOAT) (ERROR-TABLE RESTART XFLOAT)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XFLOAT)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-FLOAT))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-I) (A-CONSTANT 2036))
        (CALL-XCT-NEXT FNORM)
       ((Q-R) M-ZERO)
        (JUMP FLOPACK-T)

XSMALL-FLOAT (MISC-INST-ENTRY SMALL-FLOAT) (ERROR-TABLE RESTART XSMALL-FLOAT)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XSMALL-FLOAT)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-SMALL-FLOAT))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-I) (A-CONSTANT 2036))
        (CALL-XCT-NEXT FNORM)
       ((Q-R) M-ZERO)
        (JUMP SFLPACK-T)

XHAUL (MISC-INST-ENTRY HAULONG) ;TAKES ONE ARG, RETURNS # SIGNIFICANT BITS
            (ERROR-TABLE RESTART XHAUL)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XHAUL)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-HAULONG))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-1 A-ZERO XHAUL1)
       ((M-T) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-1) SUB M-ZERO A-1)
XHAUL1  (POPJ-EQUAL M-1 A-ZERO)
        ((M-T) ADD M-T (A-CONSTANT 1))
        (JUMP-XCT-NEXT XHAUL1)
       ((M-1) (BYTE-FIELD 31. 1) M-1)           ;SHIFT RIGHT

XHAULFLO (CALL FLOPACK-P)       ;HAULONG or LDB of a flonum.  Argument is unpacked.
        (CALL TRAP)             ;Repack and hope don't mind if SFL became FLO in the process.
            (ERROR-TABLE ARGTYP INTEGER PP T)

;;; Simple two-argument operations.

;;; Generic addition.
XMADD (MISC-INST-ENTRY M-+)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
XTCADD  ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC POPTJ)))        ;MC-LINKAGE
QIADD           (ERROR-TABLE RESTART QIADD)
        (jump-data-type-not-equal c-pdl-buffer-pointer a-t qiadd-hard)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qiadd-hard)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) ADD M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qiadd-hard
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QIADD)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-ADD))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
                (ERROR-TABLE RESTART QIADD0)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QIADD0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (JUMP-XCT-NEXT FIXPACK-P)
       ((M-1) ADD M-1 A-2)

;;; Generic subtraction.
XMSUB (MISC-INST-ENTRY M--)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
XTCSUB  ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC POPTJ)))        ;MC-LINKAGE
QISUB           (ERROR-TABLE RESTART QISUB)
        (jump-data-type-not-equal c-pdl-buffer-pointer a-t qisub-hard)
        (jump-data-type-not-equal m-t
                 (a-constant (byte-value q-data-type dtp-fix)) qisub-hard)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))
qisub-hard
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QISUB)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-SUB))
                (ERROR-TABLE RESTART QISUB0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QISUB0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

;;; Generic multiplication.
XMMUL (MISC-INST-ENTRY M-*)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
XTCMUL  ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC POPTJ)))        ;MC-LINKAGE
QIMUL           (ERROR-TABLE RESTART QIMUL)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QIMUL)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-MUL))
                (ERROR-TABLE RESTART QIMUL0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QIMUL0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)  ;CAN NOT BUM THIS BECAUSE Q LOADS DIRECTLY FROM ALU
        (CALL-XCT-NEXT MPY)     ;LOW PRODUCT TO Q-R, HIGH TO M-2
       ((Q-R) M-2)
        ((M-TEM) SELECTIVE-DEPOSIT Q-R
         (BYTE-FIELD (DIFFERENCE 33. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 1))
         A-2)   ;DISCARDED BITS AND SIGN
        (JUMP-EQUAL-XCT-NEXT M-TEM A-ZERO FIXPACK-P)   ;JUMP IF NON-OVERFLOWING POSITIVE RESULT
       ((M-1) Q-POINTER Q-R A-TEM)                   ;SIGN EXTEND (IF NON-OVERFLOWING)
        (JUMP-EQUAL M-TEM (A-CONSTANT -1) FIXPACK-P)   ;JUMP IF NON-OVERFLOWING NEGATIVE
        (JUMP-XCT-NEXT FIX-2-WORD-OVERFLOW)
       ((M-1) Q-R)

;;; Generic division, producing integer result with integer arguments.
XMDIV (MISC-INST-ENTRY M-//)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
XTCDIV  ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC POPTJ)))        ;MC-LINKAGE
;Division, with args on stack and in M-T, value on stack.
QIDIV           (ERROR-TABLE RESTART QIDIV)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QIDIV)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-IDIV))
                (ERROR-TABLE RESTART QIDIV0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QIDIV0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (CALL DIV)
        (JUMP-XCT-NEXT FIXPACK-P)       ;DIVIDE CAN'T OVERFLOW EXCEPT FOR SETZ/-1
       ((M-1) Q-R)

;args on PDL, single result ALWAYS to PDL.  Destination field used to decode rounding mode:
; 0 -> FLOOR, 1 -> CEIL, 2 -> TRUNC, 3 -> ROUND
XFLOOR-1 (MISC-INST-ENTRY INTERNAL-FLOOR-1)
        ;this must be only place it extracts destination.
        ((M-1) MACRO-IR-DEST)
        (JUMP-EQUAL M-1 A-ZERO XFLOOR-1-C)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;Don't store in our destination.
XFLOOR-1-C
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
xfloor-1-uc-entry               ;microcompiled code enters here.
        ((M-T) Q-TYPED-POINTER PDL-POP)
                                ;MC-LINKAGE
;Given args on stack and M-T, return first value of FLOOR, in M-T.
;M-1 has rounding code.
XFLOOR-1-INTERNAL
        (JUMP-EQUAL M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1))
                XFLOOR-1-A)
        (DISPATCH (BYTE-FIELD 2 0) M-1 D-XFLOOR-1)
    (ERROR-TABLE ILLEGAL-INSTRUCTION)
XFLOOR-1-ROUND  ;m-1 must have "rounding mode"
        ((micro-stack-data-push) m-1)
        (CALL QDIV)  ;Clobbers M-INST-DEST, and all M-registers, if calls out to macrocode.
        ((M-1) MICRO-STACK-DATA-POP)
;       (NO-OP)   ;QDIV returns while pushing; avoid screw from missing pass-around path.
;This code is copied from FIX, but we put the DEST field into part of M-A
;to tell the fix routine how to round.
;The DEST field should be in M-1 on arrival here.
XFLOOR-1-A
        (JUMP-DATA-TYPE-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                XFLOOR-1-B)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)
    (ERROR-TABLE ARGTYP NUMBER PP T XFIX)
    (ERROR-TABLE ARG-POPPED 0 PP)
       ((M-A) DPB M-1 ARITH-FIX-ROUNDING-MODE-FIELD (A-CONSTANT ARITH-1ARG-FIX))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XFLOOR-1-B
        (POPJ-XCT-NEXT)
       ((M-T) Q-TYPED-POINTER PDL-POP)

;; Decide how to divide, if the second arg is not 1, based on type of rounding desired.
;; We could simply use QDIV, but in the case of two fixnum args
;; it is much faster to do some rounding in the division process itself
;; and avoid creating a rationalnum.
(LOCALITY D-MEM)
(START-DISPATCH 2 0)
D-XFLOOR-1
        (INHIBIT-XCT-NEXT-BIT XFLOOR-1-FLOOR)
        (INHIBIT-XCT-NEXT-BIT XFLOOR-1-CEIL)
        (INHIBIT-XCT-NEXT-BIT XFLOOR-1-TRUNC)
        (P-BIT R-BIT)
(END-DISPATCH)
(LOCALITY I-MEM)

;;Return nonzero in M-1 unless both M-T and PDL-TOP are fixnums.
;XFLOOR-BOTH-ARGS-FIXNUMS
;#+CADR ((M-2) Q-DATA-TYPE M-T)
;#+CADR (POPJ-NOT-EQUAL-XCT-NEXT M-2 (A-CONSTANT (EVAL DTP-FIX)))
;#+LAMBDA(POPJ-DATA-TYPE-NOT-EQUAL-XCT-NEXT M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       ((M-1) A-MINUS-ONE)
;#+CADR ((M-2) Q-DATA-TYPE PDL-TOP)
;#+CADR (POPJ-NOT-EQUAL M-2 (A-CONSTANT (EVAL DTP-FIX)))
;#+LAMBDA(POPJ-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       (POPJ-XCT-NEXT)
;       ((M-1) A-ZERO)

XFLOOR-1-CEIL
 ;      (CALL XFLOOR-BOTH-ARGS-FIXNUMS)  -- loses clobbers code in M-1.
 ;      (JUMP-NOT-EQUAL M-1 A-ZERO XFLOOR-1-ROUND)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix))
                xfloor-1-round)
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-fix))
                xfloor-1-round)
        ;; CEIL and TRUNC are the same if the arg signs don't match.
        ((M-1) XOR PDL-TOP A-T)
        (JUMP-IF-BIT-SET BOXED-SIGN-BIT M-1 XFLOOR-1-TRUNC)
;Replace the dividend with dividend+divisor-1.  (This cannot change the sign).
;(Actually, it's hairier than that.  We change dividend to dividend+divisor
;and then move back by 1 toward the original dividend.
;The amount of change is thus |divisor|-1).
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-1) ADD M-1 A-2)
        ((M-1) SUB M-1 (A-CONSTANT 1))
        (JUMP-GREATER-THAN M-2 A-ZERO XFLOOR-1-CEIL-POSITIVE)
        ((M-1) ADD M-1 (A-CONSTANT 2))
XFLOOR-1-CEIL-POSITIVE
        (CALL DIV)
                ;DIVIDE CAN'T OVERFLOW EXCEPT FOR SETZ/-1
        ((M-1) Q-R)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XFLOOR-1-TRUNC
        ((micro-stack-data-push) m-1)
        (CALL QIDIV)
        ((M-1) MICRO-STACK-DATA-POP)
        (JUMP XFLOOR-1-A)

XFLOOR-1-FLOOR
;       (CALL XFLOOR-BOTH-ARGS-FIXNUMS)
;       (JUMP-NOT-EQUAL M-1 A-ZERO XFLOOR-1-ROUND)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix))
                xfloor-1-round)
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-fix))
                xfloor-1-round)
        ;; FLOOR and TRUNC are the same if the arg signs match.
        ((M-1) XOR PDL-TOP A-T)
        (JUMP-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 XFLOOR-1-TRUNC)
;Replace the dividend with dividend-divisor+1.  (This cannot change the sign).
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-1) SUB M-1 A-2)
        ((M-1) ADD M-1 (A-CONSTANT 1))
        (JUMP-GREATER-THAN M-2 A-ZERO XFLOOR-1-CEIL-POSITIVE)
        ((M-1) SUB M-1 (A-CONSTANT 2))
        (JUMP XFLOOR-1-CEIL-POSITIVE)           ;ok not to have rounding mode in M-1

;Given two args on the stack, return two values on the stack.
;The destination field only says how to round (floor vs ceil vs trunc vs round).
XFLOOR-2 (MISC-INST-ENTRY INTERNAL-FLOOR-2)
                ;this should be only place destination is extracted.
        ((M-1) MACRO-IR-DEST)
        (JUMP-EQUAL M-1 A-ZERO XFLOOR-2-A)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;Don't store in our destination.
XFLOOR-2-A
xfloor-2-uc-entry                               ;microcompiled code enters here.
        ((M-T) Q-TYPED-POINTER PDL-POP)
                                ;MC-LINKAGE
        ((M-A) PDL-TOP)
        ((PDL-PUSH) M-A)
        ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-A)
        (CALL XFLOOR-1-INTERNAL)
;Stack has empty slot, dividend, divisor.
;M-T has the quotient -- the correct first value of FLOOR.
;Put it in the empty slot.
        ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 2))
        ((C-PDL-BUFFER-INDEX) M-T)
;Stack has 1st value, dividend, divisor.  M-T has 1st value (quotient).
;Multiply divisor and quotient, result on stack and in M-T.
        (CALL QIMUL)
        (PDL-POP)
;Subtract product from dividend, result on stack and in M-T.
        (JUMP QISUB)

;;; Generic division, producing rational result with integer arguments.
XDIV (MISC-INST-ENTRY %DIV)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC POPTJ)))        ;MC-LINKAGE
;Division, with args on stack and in M-T, value on stack.
QDIV            (ERROR-TABLE RESTART QDIV)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QDIV)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-DIV))
                (ERROR-TABLE RESTART QDIV0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QDIV0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
;The args are two fixnums, now unpacked in M-1 and M-2.  Second arg packed is in M-T.
        (CALL-EQUAL M-2 A-ZERO TRAP)            ;denominator of zero causes divide-by-zero trap
    (ERROR-TABLE DIVIDE-BY-ZERO)
        (JUMP-EQUAL M-1 A-ZERO QDIV-ZERO)
        (JUMP-EQUAL M-2 (A-CONSTANT 1) FIXPACK-P)
        (JUMP-GREATER-OR-EQUAL M-2 A-ZERO QDIV-SIGNS-RIGHT)
        ((M-1) SUB M-ZERO A-1)
        ((M-2) SUB M-ZERO A-2)
QDIV-SIGNS-RIGHT
        ((M-B) M-1)
        ((M-C) M-2)
;First just check to see if the result comes out even.
        (CALL DIV)
;(Remainder is in M-1, quotient in Q-R.)
        (JUMP-EQUAL-XCT-NEXT M-1 A-ZERO QDIV-EVEN)
;It doesn't, but save a step in the GCD by using this remainder instead of the first arg.
        ((M-2) M-C)
        (CALL GCD-FIX-FIX)
        (JUMP-EQUAL-XCT-NEXT M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1))
                QDIV-REL-PRIME)
       ((M-1) M-C)
;Get the denominator and the GCD in M-1 and M-2.  Save the GCD in M-D.
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-D) M-2)
        (CALL DIV)
;Save denominator/gcd in M-E, and divide numerator by the gcd.
        ((M-E) Q-R)
        ((M-2) M-D)
        ((M-1) M-B)
        (CALL DIV)
        ((M-1) M-E)
;Now make a rational from numerator in Q-R and denominator in M-1.
QDIV-REL-PRIME-1
        (CALL FIXPACK-P)
        ((M-1) Q-R)
        (CALL FIXPACK-P)
        (CALL MAKE-RATIONAL)
        (POPJ-AFTER-NEXT
         (PDL-PUSH) M-T)
       (NO-OP)

QDIV-REL-PRIME
        (JUMP-XCT-NEXT QDIV-REL-PRIME-1)
       ((Q-R) M-B)

QDIV-ZERO
        (JUMP-XCT-NEXT FIXPACK-P)
       ((M-1) A-ZERO)

;Here if the division comes out even: return the quotient, as an integer.
QDIV-EVEN
        (JUMP-XCT-NEXT FIXPACK-P)       ;DIVIDE CAN'T OVERFLOW EXCEPT FOR SETZ/-1
       ((M-1) Q-R)

XRATIO-CONS (MISC-INST-ENTRY %RATIO-CONS)
        ((M-A) PDL-POP)
        ((M-B) PDL-POP)
        ((PDL-PUSH) M-A)
        ((PDL-PUSH) M-B)

;Construct a rational from DENOMINATOR and NUMERATOR on the stack,
;and return it in M-T.  Notice that the args are not in the order you would expect!
MAKE-RATIONAL
     ;; Allocate 3 boxed words of structure storage in the number-cons area.
        ((m-b) (a-constant 3))
        (call-xct-next allocate-extended-number-storage)
       ((m-a) m-b)

        ((VMA) ADD M-T (A-CONSTANT 1))          ;Write the numerator.
        ((MD-START-WRITE) PDL-POP)
        (CHECK-PAGE-WRITE)
        (gc-write-test)
        ((WRITE-MEMORY-DATA) PDL-POP)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE)
        (gc-write-test)
;Write the header word.
        ((VMA M-T) Q-POINTER M-T
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
        (POPJ-AFTER-NEXT
         (MD-START-WRITE)
         (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                           (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-RATIONAL)
                           0)))
       (CHECK-PAGE-WRITE)

;EQL is EQ, except = for numbers of matching type.
XEQL (MISC-INST-ENTRY EQL)
        ((M-T) Q-TYPED-POINTER PDL-POP)
;Args in M-T and on stack.
XEQL1
        (JUMP-DATA-TYPE-NOT-EQUAL PDL-TOP A-T POP-THEN-XFALSE)
        (JUMP-DATA-TYPE-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER))
                QMEQ)
;; Both args are extended numbers.
;; Are they the same kind?
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((M-1) Q-TYPED-POINTER MD)
        ((VMA-START-READ) PDL-TOP)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((M-2) Q-TYPED-POINTER MD)
        (JUMP-NOT-EQUAL M-1 A-2 POP-THEN-XFALSE)
;; Yes.  Do numeric comparison.
;; Not quite the same as =, because for complex numbers
;; EQL is T only if the types of the components match.
    (ERROR-TABLE RESTART XEQL)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 XEQL)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-EQL))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (JUMP XEQL2)

;;; Generic numeric equality (the "=" function).
XMEQL (MISC-INST-ENTRY M-=)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
QMEQL                                       ;MC-LINKAGE
QIEQL           (ERROR-TABLE RESTART QIEQL)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QIEQL)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-EQUAL))
                (ERROR-TABLE RESTART QIEQL0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
XEQL2 ;; Enter from EQL, with ARITH-2ARG-EQL in M-A.
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QIEQL0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

;;; Generic numeric GREATERP
XMGRTH (MISC-INST-ENTRY M->)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
QMGRP                                        ;MC-LINKAGE
QIGRP           (ERROR-TABLE RESTART QIGRP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QIGRP)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-GREATERP))
                (ERROR-TABLE RESTART QIGRP0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QIGRP0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

;;; Generic numeric LESSP
XMLESS (MISC-INST-ENTRY M-<)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
QMLSP                                         ;MC-LINKAGE
QILSP           (ERROR-TABLE RESTART QILSP)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 QILSP)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-LESSP))
                (ERROR-TABLE RESTART QILSP0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 QILSP0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

XMAX (MISC-INST-ENTRY *MAX)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
                (ERROR-TABLE RESTART XMAX)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 XMAX)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-MAX))
                (ERROR-TABLE RESTART XMAX0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 XMAX0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (JUMP-GREATER-OR-EQUAL M-1 A-2 FIXPACK-T)
        ((M-1) A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XMIN (MISC-INST-ENTRY *MIN)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
                (ERROR-TABLE RESTART XMIN)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP NUMBER PP 0 XMIN)
    (ERROR-TABLE ARG-POPPED 0 PP M-T)
       ((M-A) (A-CONSTANT ARITH-2ARG-MIN))
                (ERROR-TABLE RESTART XMIN0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 XMIN0)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (JUMP-LESS-OR-EQUAL M-1 A-2 FIXPACK-T)
        ((M-1) A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))


;;; Data-type dispatches for arithmetic.

;;; Dispatch on the type of a one-argument numeric function.
;;; DTP-FIX unpacks and then drops through; eveything else jumps.
;;; On LAMBDA, DTP-FIX does nothing and drops thru.
(LOCALITY D-MEM)
(START-DISPATCH 5 0)
D-NUMARG
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SYMBOL HEADER
#+cadr  (P-BIT FXUNPK-P-1)                      ;FIX
#+lambda(P-BIT R-BIT)
#+exp   (P-BIT R-BIT)
        (ARITH-XNM)                             ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LOCATIVE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LIST
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;U CODE ENTRY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ARRAY-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;STACK-GROUP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELECT-METHOD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ENTITY
        (P-BIT INHIBIT-XCT-NEXT-BIT illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
#+cadr  (P-BIT FXUNPK-P-1)                      ;CHARACTER
#+lambda(P-BIT R-BIT)
#+exp   (P-BIT R-BIT)
        (p-bit n-bit illop)     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;spare
        (ARITH-SFL)                             ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

;;; Dispatch on the type of the first numeric arg.
;;; DTP-FIX unpacks and then drops through; eveything else jumps.
;;; On LAMBDA, DTP-FIX just drops thru.
(START-DISPATCH 5 0)
D-NUMARG1
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SYMBOL HEADER
#+cadr  (P-BIT FXUNPK-P-1)                      ;FIX
#+lambda(P-BIT R-BIT)
#+exp   (P-BIT R-BIT)
        (ARITH-XNM-ANY)                         ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LOCATIVE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LIST
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;U CODE ENTRY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ARRAY-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;STACK-GROUP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELECT-METHOD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ENTITY
        (P-BIT INHIBIT-XCT-NEXT-BIT illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
#+CADR  (P-BIT FXUNPK-P-1)                      ;CHARACTER
#+LAMBDA(P-BIT R-BIT)
#+exp   (P-BIT R-BIT)
        (p-bit n-bit illop)     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;spare
        (ARITH-SFL-ANY)                         ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

;;; Data type dispatch on second numeric arg, when first one was a DTP-FIXNUM.
;;; DTP-FIXNUM unpacks and drops through; everything else jumps.  First arg
;;; is unpacked into M-1.  Second arg in M-T.
;;; On LAMBDA, DTP-FIX just drops thru.
(START-DISPATCH 5 0)
D-FIXNUM-NUMARG2
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SYMBOL HEADER
#+CADR  (P-BIT INHIBIT-XCT-NEXT-BIT FXUNPK-T-2) ;FIX
#+LAMBDA(P-BIT R-BIT)
#+exp   (P-BIT R-BIT)
        (INHIBIT-XCT-NEXT-BIT ARITH-ANY-XNM)    ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LOCATIVE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LIST
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;U CODE ENTRY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ARRAY-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;STACK-GROUP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELECT-METHOD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ENTITY
        (P-BIT INHIBIT-XCT-NEXT-BIT illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
#+CADR  (P-BIT INHIBIT-XCT-NEXT-BIT FXUNPK-T-2) ;CHARACTER
#+LAMBDA(P-BIT R-BIT)
#+exp   (P-BIT R-BIT)
        (p-bit n-bit illop)                     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;spare
        (INHIBIT-XCT-NEXT-BIT ARITH-FIX-SFL)    ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

;;; Data type dispatch for second numeric arg when first was NOT DTP-FIXNUM
;;; DTP-SMALL-FLONUM unpacks and drops through; everything else jumps.
;;; During this dispatch, the I-ARG contains a number code.
;;; The first arg has been unpacked as follows:
;;;  If BIGNUM, M-Q has BIGNUM pointer, M-C HEADER, M-I LENGTH.
;;;  If FLONUM, M-Q has FLONUM pointer, M-C HEADER, M-I exponent, M-1 mantissa.
;;;  If SMALL-FLONUM, M-Q has SMALL-FLONUM pointer, M-I has exponent, M-1 mantissa.
;;;  Also, the original pointer is kept in M-J.
(START-DISPATCH 5 0)
D-NUMARG2
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SYMBOL HEADER
        (ARITH-ANY-FIX)                         ;FIX
        (ARITH-ANY-XNM)                         ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LOCATIVE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;LIST
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;U CODE ENTRY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ARRAY-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;STACK-GROUP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELECT-METHOD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ENTITY
        (P-BIT INHIBIT-XCT-NEXT-BIT illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
        (ARITH-ANY-FIX)                         ;CHARACTER
        (p-bit n-bit illop)                     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;spare
        (P-BIT SFLUNPK-T-2)                     ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

;;; One-argument function.
ARITH-SFL
        (CALL SFLUNPK-P-1)
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FLONUM-1ARG)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC SFLPACK-T)))

ARITH-XNM
        ((VMA-START-READ M-I) C-PDL-BUFFER-POINTER-POP)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-Q) VMA)             ;get transported number address
        (CALL-DATA-TYPE-NOT-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)) ILLOP)
        (DISPATCH-XCT-NEXT HEADER-TYPE-FIELD MD D-XNM-ARG)
       ((M-C) HEADER-REST-FIELD MD)

(LOCALITY D-MEM)
(START-DISPATCH 5 0)
D-XNM-ARG
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-ERROR
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-ARRAY-LEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-UNUSED
        (ARITH-FLO)                             ;%HEADER-TYPE-FLONUM
        (ARITH-OUT)                             ;%HEADER-TYPE-COMPLEX
        (ARITH-BIG)                             ;%HEADER-TYPE-BIGNUM
        (ARITH-OUT)                             ;%HEADER-TYPE-RATIONAL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
 (REPEAT NUM-UNUSED-HEADER-TYPES (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

ARITH-FLO
        ((VMA-START-READ) ADD M-Q (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-I) HEADER-FLONUM-EXPONENT M-C)
        ((M-1) DPB M-C FLONUM-HEADER-HIGH-MANTISSA A-ZERO)
        ((M-1) SELECTIVE-DEPOSIT MD FLONUM-HEADER-LOW-MANTISSA A-1)
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FLONUM-1ARG)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC FLOPACK-T)))

;; Call out to macro-code.
ARITH-OUT
        (CALL P3ZERO)
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-NUM1))
        (DISPATCH TRANSPORT MD)
        ((C-PDL-BUFFER-POINTER-PUSH) READ-MEMORY-DATA)  ;Push function
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the function code.
                Q-POINTER M-A (A-CONSTANT (PLUS (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                                (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the number.
                Q-TYPED-POINTER M-I (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-JUMP MMJCALL) (I-ARG 2))          ;Call tail-recursively.

;;; This dispatch SOMETIMES executes next: viz., when the result will be a number.
;;; Dispatchers can push a PACK routine in the xct-next cycle.
(LOCALITY D-MEM)
(START-DISPATCH 4 0)
D-FLONUM-1ARG
        (FLONUM-ABS)
        (FLONUM-MINUS)
        (INHIBIT-XCT-NEXT-BIT FLONUM-ZEROP)
        (INHIBIT-XCT-NEXT-BIT FLONUM-PLUSP)
        (INHIBIT-XCT-NEXT-BIT FLONUM-MINUSP)
        (FLONUM-ADD1)
        (FLONUM-SUB1)
        (INHIBIT-XCT-NEXT-BIT FLONUM-FIX)
        (INHIBIT-XCT-NEXT-BIT FLOPACK-T)
        (INHIBIT-XCT-NEXT-BIT SFLPACK-T)
        (P-BIT INHIBIT-XCT-NEXT-BIT XHAULFLO)   ;HAULONG DOESN'T WORK FOR FLONUMS
        (P-BIT INHIBIT-XCT-NEXT-BIT XHAULFLO)   ;LDB DOESNT EITHER.
        (P-BIT INHIBIT-XCT-NEXT-BIT XHAULFLO)   ;NOR DPB.
        (FLONUM-ASH)                            ;ASH OF A FLONUM = FSC
        (P-BIT INHIBIT-XCT-NEXT-BIT XHAULFLO)   ;ODDP ILLEGAL
        (P-BIT INHIBIT-XCT-NEXT-BIT XHAULFLO)   ;EVENP ILLEGAL
 (REPEAT NUM-UNUSED-ARITH-1ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)


FLONUM-ABS
        (POPJ-IF-BIT-CLEAR FLONUM-SIGN-BIT M-1)
        (JUMP FNEG1)

;FLONUM-MINUS is the same as FNEG1, see below.

;FLONUM-ZEROP, FLONUM-PLUSP and FLONUM-MINUSP are up with the fixnum cases,
;see above.

FLONUM-ADD1
        ((M-2) DPB M-MINUS-ONE MANTISSA-HIGH-BIT A-ZERO);10_33
        (JUMP-XCT-NEXT FADD)
       ((M-J) M+A+1 M-ZERO (A-CONSTANT 2000))           ;2001

FLONUM-SUB1
        ((M-2) DPB M-MINUS-ONE FLONUM-SIGN-BIT A-ZERO)  ;20_33
        (JUMP-XCT-NEXT FADD)
       ((M-J) (A-CONSTANT 2000))                        ;2000

FLONUM-FIX
        (DISPATCH ARITH-FIX-ROUNDING-MODE-FIELD M-A D-FLONUM-FIX)

(LOCALITY D-MEM)
(START-DISPATCH 2 INHIBIT-XCT-NEXT-BIT)
D-FLONUM-FIX
        (FLONUM-FIX-FLOOR)
        (FLONUM-FIX-CEIL)
        (FLONUM-FIX-TRUNC)
        (FLONUM-FIX-ROUND)
(END-DISPATCH)
(LOCALITY I-MEM)

FLONUM-FIX-ROUND
        ((M-TEM) SUB M-I (A-CONSTANT 2000))     ;Number of bits before binary point.
        ;; If expt is so big that there are no fractional bits left, just take floor.
        (JUMP-LESS-OR-EQUAL (M-CONSTANT 40) A-TEM FLONUM-FIX-FLOOR)
        ;; If -.5 <= x < .5, just go return zero -- the binary point is outside the word.
        (JUMP-LESS-THAN M-TEM A-ZERO FLONUM-FIX-ZERO)
        ((M-TEM) SUB (M-CONSTANT 40) A-TEM)
        ((M-TEM) SUB M-TEM (A-CONSTANT 2))      ;Number of bits to right of binary point, -1.
;M-TEM has position (from bottom).
        ((OA-REG-LOW) oal-mrot m-TEM)
                ;M-2 gets a 1 in first bit after binary point.
        ((M-2) DPB (Byte-Field 1 0) M-MINUS-ONE A-ZERO)
;What follows is like FADD2, except that we clear out Q to prevent rounding up.
        ((M-1) ADD M-1 A-2 OUTPUT-SELECTOR-RIGHTSHIFT-1 ;Do the add, collect
                SHIFT-Q-RIGHT)  ; the overflow, discarded bits to Q
        ((Q-R) A-ZERO)
        (CALL FNORM)
;Now see if it's exactly an integer (that is, if it was exactly half way between before)
;Note M-I may be different now.
        ((M-TEM) SUB M-I (A-CONSTANT 2000))     ;Number of bits before binary point.
        (JUMP-LESS-OR-EQUAL (M-CONSTANT 40) A-TEM FLONUM-FIX-FLOOR)
        ;; If -.5 <= x < .5 NOW, then the floor is certainly right.
        (JUMP-LESS-THAN M-TEM A-ZERO FLONUM-FIX-FLOOR)
        ((M-TEM) SUB (M-CONSTANT 40) A-TEM)
                ;Number of bits to right of binary point, -1.
        ((M-TEM) SUB M-TEM (A-CONSTANT 2))
#+exp   ((m-tem3) add m-tem (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-TEM #+exp m-tem3 A-ZERO OAL-BYTL-1)
       ((M-2) BYTE-INST M-1 A-ZERO)
        (JUMP-NOT-EQUAL M-2 A-ZERO FLONUM-FIX-FLOOR)
;If it is an exact integer,
;clear out the bit just before the binary point to make the result even.
;However, the integer -1.0 must be treated specially since in that case
;the only bit before the binary point is the sign bit and clearing it gives 0, not -2.0!
        (JUMP-EQUAL M-I (A-CONSTANT 2000) FLONUM-FIX-ROUND-MINUS-ONE)
#+exp   ((m-tem3) add m-tem (a-constant 1))
#+exp   ((oa-reg-low) oal-mrot m-tem3)
#+lambda((OA-REG-LOW) ADD M-TEM (A-CONSTANT 1))
       ((M-1) DPB (Byte-Field 1 0) M-ZERO A-1)
        (JUMP FLONUM-FIX-FLOOR)

FLONUM-FIX-CEIL-SMALL
        (JUMP-LESS-OR-EQUAL M-1 A-ZERO FLONUM-FIX-ZERO)
FLONUM-FIX-ONE
        (POPJ-AFTER-NEXT)
       ((M-T) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1)))

FLONUM-FIX-ZERO
        (POPJ-AFTER-NEXT)
       ((M-T) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;Change -1.0 into -2.0.
FLONUM-FIX-ROUND-MINUS-ONE
        ((M-I) ADD M-I (A-CONSTANT 1))
        (JUMP FLONUM-FIX-FLOOR)

FLONUM-FIX-TRUNC
;TRUNC is FLOOR for positive numbers, CEIL for negative ones.
        (JUMP-IF-BIT-CLEAR FLONUM-SIGN-BIT M-1 FLONUM-FIX-FLOOR)
FLONUM-FIX-CEIL
;Add .99999999 to the number, then take the floor.
        ((M-TEM) SUB M-I (A-CONSTANT 2000))     ;Number of bits before binary point.
        ;; If expt is so big that there are no fractional bits left, just take floor.
        (JUMP-LESS-OR-EQUAL (M-CONSTANT 40) A-TEM FLONUM-FIX-FLOOR)
        ;; If -5. <= x < .5, just return 0 or 1 according to sign.
        ;; The normal path would lose since the highest fractional bit doesn't exist.
        (JUMP-LESS-THAN M-TEM A-ZERO FLONUM-FIX-CEIL-SMALL)
        ((M-TEM) SUB (M-CONSTANT 40) A-TEM)
                ;Number of bits to right of binary point, -1.
        ((M-TEM) SUB M-TEM (A-CONSTANT 2))
#+exp   ((m-tem3) add m-tem (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-TEM #+exp m-tem3 OAL-BYTL-1 A-ZERO)
        ((M-2) DPB M-MINUS-ONE A-ZERO)
;What follows is like FADD2, except that we clear out Q to prevent rounding up.
        ((M-1) ADD M-1 A-2 OUTPUT-SELECTOR-RIGHTSHIFT-1 ;Do the add, collect
                SHIFT-Q-RIGHT)  ; the overflow, discarded bits to Q
        ((Q-R) A-ZERO)
        (CALL FNORM)
FLONUM-FIX-FLOOR
#+lambda((oa-reg-high) flonum-sign-bit m-1)
#+exp   ((m-t) flonum-sign-bit m-1)                     ;M-T gets 0 if arg positive,
#+exp   ((oa-reg-high) dpb m-t oah-m-src a-zero)
       ((m-t) ldb q-pointer m-zero (a-constant (byte-value q-data-type dtp-fix)))       ;return 0 or -1 if fractional
        (POPJ-LESS-OR-EQUAL M-I (A-CONSTANT 2000))
                ;jump if big enough to be bignum
        (JUMP-GREATER-OR-EQUAL M-I (A-CONSTANT (PLUS 2000 Q-POINTER-WIDTH)) FLONUM-BIGFIX)
                ;Byte length - 1 (maximum byte length 23.)
        ((M-A) M-A-1 M-I (A-CONSTANT 2000))
        ((M-B) ADD M-A (A-CONSTANT 2))          ;Leftward rotation of M-1.
#+exp   ((m-tem3) add m-a (a-constant 1))
        (POPJ-AFTER-NEXT (OA-REG-LOW) DPB #+lambda M-A #+exp m-tem3 OAL-BYTL-1 A-B)
       ((M-T) (BYTE-FIELD 0 0) M-1 A-T)         ;A boxed signed fixnum!

FLONUM-BIGFIX
        ((M-C) DPB M-T BIGNUM-HEADER-SIGN A-ZERO)       ;Save sign
        (CALL FLONUM-ABS)
        ((M-4) M-1)                             ;Save magnitude of mantissa
        ((M-1) SUB M-I (A-CONSTANT (DIFFERENCE 2000 30.)))      ;Compute bignum length
        (CALL-XCT-NEXT DIV)                     ;Q-R gets number of words,
       ((M-2) (A-CONSTANT 31.))                 ;M-1 gets bits minus one in last word
        ((M-2) M-4)                             ;Restore mantissa magnitude
        ((M-I) Q-R)                             ;Bignum length
        (CALL-XCT-NEXT BNCONS)                  ;Allocate a bignum result
       ((M-B) ADD Q-R (A-CONSTANT 1))
        ((M-3) SUB M-I (A-CONSTANT 2))          ;Zero out all but high 2 words of bignum
        (JUMP-LESS-OR-EQUAL M-3 A-ZERO FLONUM-BIGFIX1)
        ((WRITE-MEMORY-DATA) M-ZERO)
FLONUM-BIGFIX0
        ((VMA-START-WRITE) ADD M-T A-3)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-GREATER-THAN-XCT-NEXT M-3 (A-CONSTANT 1) FLONUM-BIGFIX0)
       ((M-3) SUB M-3 (A-CONSTANT 1))
FLONUM-BIGFIX1
        ((M-3) ADD M-1 (A-CONSTANT 2))          ;Get high-order word of result
                ;[Right-justify high (M-1)+1 bits of 31.]
#+exp   ((m-tem3) add m-1 (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-1 #+exp m-tem3 OAL-BYTL-1 A-3)
        ((WRITE-MEMORY-DATA) (BYTE-FIELD 0 0) M-2)
        ((VMA-START-WRITE) ADD M-T A-I)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-LESS-THAN M-I (A-CONSTANT 2) BIGNUM-DPB-CLEANUP)  ;No low-order word
                ;Get low-order word (may be garbage)
        ((M-3) M-A-1 (M-CONSTANT 32.) A-3)
        ((M-1) ADD M-1 (A-CONSTANT 1))          ;[Left-justify low 30.-(M-1) bits in 31.]
#+exp   ((m-tem3) add m-3 (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-3 #+exp m-tem3 OAL-BYTL-1 A-1)
        ((WRITE-MEMORY-DATA) DPB M-2 (BYTE-FIELD 0 0) A-ZERO)
        ((VMA-START-WRITE) SUB VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP BIGNUM-DPB-CLEANUP)               ;Might really be a fixnum after all! (SETZ)

;;; Two-argument functions.

;;; The first arg, which is on the PDL, is a SMALL-FLONUM.  ARITH-2ARG is in M-A.
ARITH-SFL-ANY           (ERROR-TABLE RESTART ARITH-SFL-ANY)
        ((M-Q) Q-TYPED-POINTER C-PDL-BUFFER-POINTER)    ;Save arg in case error.
        ((M-J) M-Q)                     ;Save arg in case of call-out.
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-T D-NUMARG2 (I-ARG NUMBER-CODE-SMALL-FLONUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 ARITH-SFL-ANY)
       (CALL SFLUNPK-P-1)
        ;; If it comes back here, both flonums are unpacked.
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FORWARD-FLONUM-OPS)
    (ERROR-TABLE ARGTYP INTEGER M-T 1)  ;not easily continuable
    (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC SFLPACK-P)))

;;; Second arg was a fixnum, but first wasn't.
;;; I-ARG contains type of first argument, M-A contains operation.
ARITH-ANY-FIX
        (DISPATCH-XCT-NEXT (BYTE-FIELD 3 0) READ-I-ARG D-ARITH-ANY-FIX)
       ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)
D-ARITH-ANY-FIX
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;FIX, SHOULN'T GET HERE.
        (ARITH-SFL-FIX)                         ;SMALL FLONUM
        (ARITH-FLO-FIX)                         ;FLONUM
        (ARITH-BIG-FIX)                         ;BIGNUM
 (REPEAT NUM-UNUSED-NUMBER-CODES (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

;;; First arg is a fixnum unpacked.  Second arg is a small flonum, packed.
ARITH-FIX-SFL
        (CALL-XCT-NEXT SFLUNPK-T-2)
       ((M-I) (A-CONSTANT 2036))
        (CALL-XCT-NEXT FNORM)
       ((Q-R) M-ZERO)
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FORWARD-FLONUM-OPS)
    (ERROR-TABLE ARGTYP INTEGER M-T 1)
    (ERROR-TABLE ARG-POPPED 0 M-1 M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC SFLPACK-P)))

;;; We have a small flonum in M-1,M-I and a fixnum in M-2.
;;; Reverse the order, normalize the fixnum to a flonum, and call reverse operator.
ARITH-SFL-FIX
        ((M-TEM) M-2)
        ((M-2) M-1)
        ((M-1) M-TEM)
        ((M-J) M-I)
        ((Q-R) M-ZERO)
        (CALL-XCT-NEXT FNORM)
       ((M-I) (A-CONSTANT 2036))
        (DISPATCH (BYTE-FIELD 4 0) M-A D-REVERSE-FLONUM-OPS)
    (ERROR-TABLE FLONUM-NO-GOOD)        ;ARGTYP not usable, arg not saved
    (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC SFLPACK-P)))

;;; We have a flonum in M-1/M-I and a fixnum in M-2.  This is just like the above.
ARITH-FLO-FIX
        ((M-TEM) M-2)
        ((M-2) M-1)
        ((M-1) M-TEM)
        ((M-J) M-I)
        ((Q-R) M-ZERO)
        (CALL-XCT-NEXT FNORM)
       ((M-I) (A-CONSTANT 2036))
        (DISPATCH (BYTE-FIELD 4 0) M-A D-REVERSE-FLONUM-OPS)
    (ERROR-TABLE FLONUM-NO-GOOD)        ;ARGTYP not usable, arg not saved
    (ERROR-TABLE ARG-POPPED M-Q M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC FLOPACK-P)))

;;; Routines that look at the contents of M-A and act on it.

(LOCALITY D-MEM)
(START-DISPATCH 4 0)
D-FORWARD-FLONUM-OPS
        (FADD)                  ;ADD
        (FSUB)                  ;SUB
        (FMPY)                  ;MUL
        (FDIV)                  ;IDIV
        (INHIBIT-XCT-NEXT-BIT FEQL)     ;=
        (INHIBIT-XCT-NEXT-BIT FGRP)     ;>
        (INHIBIT-XCT-NEXT-BIT FLSP)     ;<
        (FMIN)                  ;MIN
        (FMAX)                  ;MAX
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;BOOLE
        (FDIV)                  ;DIV
        (INHIBIT-XCT-NEXT-BIT FEQL)     ;EQL
 (REPEAT NUM-UNUSED-ARITH-2ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

(START-DISPATCH 4 0)
D-REVERSE-FLONUM-OPS
        (FADD)                  ;REVERSE ADD
        (FSUB-REVERSE)          ;REVERSE SUB
        (FMPY)                  ;REVERSE MPY
        (FDIV-REVERSE)          ;REVERSE IDIVIDE
        (INHIBIT-XCT-NEXT-BIT FEQL)     ;REVERSE =
        (INHIBIT-XCT-NEXT-BIT FLSP)     ;REVERSE >
        (INHIBIT-XCT-NEXT-BIT FGRP)     ;REVERSE <
        (FMIN)                  ;REVERSE MIN
        (FMAX)                  ;REVERSE MAX
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;BOOLE
        (FDIV-REVERSE)          ;REVERSE DIVIDE
        (INHIBIT-XCT-NEXT-BIT FEQL)     ;REVERSE EQL
(REPEAT NUM-UNUSED-ARITH-2ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

FSUB-REVERSE
        (JUMP-XCT-NEXT FADD)
       (CALL FNEG1)

FDIV-REVERSE
        (JUMP-XCT-NEXT FDIV)
       (CALL SWAP-FLONUMS)

SWAP-FLONUMS
        ((M-TEM) M-I)
        ((M-I) M-J)
        ((M-J) M-TEM)
        ((M-TEM) M-1)
        (POPJ-AFTER-NEXT (M-1) M-2)
       ((M-2) M-TEM)

;;; Extended numbers.

;;; The first arg is an XNUM.  Arith op in M-A.
ARITH-XNM-ANY
        ((VMA-START-READ M-J) C-PDL-BUFFER-POINTER-POP)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-Q) Q-TYPED-POINTER VMA)             ;get transported number address
        (CALL-DATA-TYPE-NOT-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)) ILLOP)
        (DISPATCH-XCT-NEXT HEADER-TYPE-FIELD MD D-XNM-ARG-1)
       ((M-C) HEADER-REST-FIELD MD)

(LOCALITY D-MEM)
(START-DISPATCH 5 0)
D-XNM-ARG-1
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-ERROR
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-ARRAY-LEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-UNUSED
        (ARITH-FLO-ANY)                         ;%HEADER-TYPE-FLONUM
        (ARITH-OUT-ANY)                         ;%HEADER-TYPE-COMPLEX
        (ARITH-BIG-ANY)                         ;%HEADER-TYPE-BIGNUM
        (ARITH-OUT-ANY)                         ;%HEADER-TYPE-RATIONAL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
 (REPEAT NUM-UNUSED-HEADER-TYPES (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

;This dispatch is used to push a return address of M-T-TO-CPDL if the
;instruction wants its result on the pdl (rather than in M-T).
;In any case we go to MMJCALL to activate the call to the macrocode routine.
(START-DISPATCH 4 0)
D-ARITH-OUT-RETURN
        (MMJCALL)               ;ADD
        (MMJCALL)               ;SUB
        (MMJCALL)               ;MUL
        (MMJCALL)               ;IDIV
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;=
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;>
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;<
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;MIN
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;MAX
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;BOOLE
        (MMJCALL)               ;DIV
        (INHIBIT-XCT-NEXT-BIT MMJCALL)  ;EQL
 (REPEAT NUM-UNUSED-ARITH-2ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

;;; First argument is handled by macrocode.  Call out.
ARITH-OUT-ANY
        (CALL P3ZERO)
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-NUM2))
        (DISPATCH TRANSPORT MD)
        ((C-PDL-BUFFER-POINTER-PUSH) READ-MEMORY-DATA)  ;Push function
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the function code.
                Q-POINTER M-A (A-CONSTANT (PLUS (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                                (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the first number.
                Q-TYPED-POINTER M-J (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the second number.
                Q-TYPED-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        (DISPATCH (BYTE-FIELD 4 0) M-A D-ARITH-OUT-RETURN (I-ARG 3))    ;Call tail-recursively.
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))

;;; First arg is a real flonum.  Pointer in M-Q, header-rest in M-C, op in M-A.
ARITH-FLO-ANY
        ((VMA-START-READ) M-Q ADD (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-I) HEADER-FLONUM-EXPONENT M-C)
        ((M-1) DPB M-C FLONUM-HEADER-HIGH-MANTISSA A-ZERO)
    (ERROR-TABLE RESTART ARITH-FLO-ANY)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-T D-NUMARG2 (I-ARG NUMBER-CODE-FLONUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1 ARITH-FLO-ANY)
       ((M-1) SELECTIVE-DEPOSIT MD FLONUM-HEADER-LOW-MANTISSA A-1)
        ;; If falls through, second arg is a small flonum, already unpacked.
ARITH-FLO-SFL ;This label is not used.  It is here for completeness.
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FORWARD-FLONUM-OPS)
    (ERROR-TABLE ARGTYP INTEGER M-Q 0)  ;not easily continuable
    (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC FLOPACK-P)))

;;; The second arg is an extended number.  First arg is unpacked, type in I-ARG.
;;; Arith op in M-A.
ARITH-ANY-XNM
        ((VMA-START-READ) M-T)
        ((M-R) READ-I-ARG)                      ;Get number code of first arg.
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-T) VMA)             ;get transported number address
        (CALL-DATA-TYPE-NOT-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)) ILLOP)
        (DISPATCH-XCT-NEXT HEADER-TYPE-FIELD MD D-XNM-ARG-2)
       ((M-D) HEADER-REST-FIELD MD)

(LOCALITY D-MEM)
(START-DISPATCH 5 0)
D-XNM-ARG-2
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-ERROR
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-ARRAY-LEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-UNUSED
        (ARITH-ANY-FLO)                         ;%HEADER-TYPE-FLONUM
        (ARITH-ANY-OUT)                         ;%HEADER-TYPE-COMPLEX
        (ARITH-ANY-BIG)                         ;%HEADER-TYPE-BIGNUM
        (ARITH-ANY-OUT)                         ;%HEADER-TYPE-RATIONAL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
 (REPEAT NUM-UNUSED-HEADER-TYPES (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

;;; The second arg requires calling-out to macrocode.  The first
;;; argument is in M-J, unless it was a fixnum in which case
;;; it has been unpacked into M-1.  Second arg is in M-T.
ARITH-ANY-OUT
        (JUMP-NOT-EQUAL M-R (A-CONSTANT NUMBER-CODE-FIXNUM) ARITH-ANY-OUT-1)
        (CALL-XCT-NEXT FIXPACK-T)
       ((C-PDL-BUFFER-POINTER-PUSH) M-T)
        ((M-J) M-T)
        ((M-T) C-PDL-BUFFER-POINTER-POP)
ARITH-ANY-OUT-1
        (CALL P3ZERO)
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-NUM2))
        (DISPATCH TRANSPORT MD)
        ((C-PDL-BUFFER-POINTER-PUSH) READ-MEMORY-DATA)  ;Push function
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the function code.
                Q-POINTER M-A (A-CONSTANT (PLUS (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                                (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the first number.
                Q-TYPED-POINTER M-J (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((C-PDL-BUFFER-POINTER-PUSH)            ;Push the second number.
                Q-TYPED-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        (DISPATCH (BYTE-FIELD 4 0) M-A D-ARITH-OUT-RETURN (I-ARG 3))    ;Call tail-recursively.
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))

;;; Number code of first arg in M-R.
;;; If is is a fixnum, small flonum, or flonum, it is unpacked in M-1/M-I.
;;; Our header rest field in M-D, our pointer in M-T.
ARITH-ANY-FLO
        ((VMA-START-READ) M-T ADD (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-J) HEADER-FLONUM-EXPONENT M-D)
        ((M-2) DPB M-D FLONUM-HEADER-HIGH-MANTISSA A-ZERO)
        (DISPATCH-XCT-NEXT (BYTE-FIELD 3 0) M-R D-ARITH-ANY-FLO)
       ((M-2) SELECTIVE-DEPOSIT MD FLONUM-HEADER-LOW-MANTISSA A-2)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)
D-ARITH-ANY-FLO
        (ARITH-FIX-FLO)
        (ARITH-SFL-FLO)
        (ARITH-FLO-FLO)
        (ARITH-BIG-FLO)
 (REPEAT NUM-UNUSED-NUMBER-CODES (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

ARITH-FIX-FLO
        ((M-Q) M-1)
        ((M-I) (A-CONSTANT 2036))
        (CALL-XCT-NEXT FNORM)
       ((Q-R) M-ZERO)
        ;drop in
ARITH-SFL-FLO
ARITH-FLO-FLO
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FORWARD-FLONUM-OPS)
    (ERROR-TABLE FLONUM-NO-GOOD)        ;ARGTYP not usable, I think I lost the arg
    (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC FLOPACK-P)))

;Flonum arithmetic routines.

;These routines bash M-J, M-2, M-TEM, Q-R, A-TEMn, M-K.
;M-ZERO is an even address in M-memory.  It contains zeros, and the following
;location contains a -1.  This is used to get A-TEM1 below by hacking
;the low bit of the OA-REG-HIGH, which is the low bit of the M-source field,
;to get either a word of zeros or a word of ones depending on the sign bit
;of M-2.

;Floating Subtract.  This changes the sign of M-2 and turns into Add.
FSUB    (CALL FNEG2)
        ;drop through

;Floating Add.
FADD    (JUMP-EQUAL-XCT-NEXT M-I A-J FADD2)     ;Jump if exponents equal, no shifting
       ((Q-R) M-ZERO)                           ;Initialize discarded bits.
        (CALL-LESS-THAN-XCT-NEXT M-I A-J FADD1) ;If M-1 to shift right, exchange args
       ((M-TEM) M-A-1 M-I A-J)                  ;Amt to shift M-2 right minus one
        (JUMP-GREATER-OR-EQUAL M-TEM (A-CONSTANT 37) FADD3)
#+lambda((OA-REG-HIGH) FLONUM-SIGN-BIT M-2)     ;Sign-extend M-2
#+exp   ((m-tem3) flonum-sign-bit m-2)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
       ((M-TEM1) M-ZERO)                        ;Gets either all zeros or all ones.
        ((M-J) M-A-1 (M-CONSTANT 40) A-TEM)     ;40 minus exponent difference
#+exp   ((m-tem3) add m-tem (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-TEM #+exp m-tem3 OAL-BYTL-1 A-J)   ; becomes m-rotate
       ((M-TEM2) DPB M-2 (BYTE-FIELD 0 0) A-ZERO) ;Get bits shifted off right end of M-2
        ((Q-R) A-TEM2)                          ;Put them in Q-R where they belong
        ((M-TEM) SUB M-J (A-CONSTANT 1))        ;Byte length minus one
#+exp   ((m-tem3) add m-tem (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-TEM #+exp m-tem3 OAL-BYTL-1 A-J)
       ((M-2) (BYTE-FIELD 0 0) M-2 A-TEM1)      ;Arithmetically shift M-2 right
FADD2   ((M-1) ADD M-1 A-2 OUTPUT-SELECTOR-RIGHTSHIFT-1 ;Do the add, collect
                SHIFT-Q-RIGHT)  ; the overflow, discarded bits to Q
;Normalizing loop
FNORM   (DISPATCH SIGN-BIT-AND-MANTISSA-HIGH-THREE M-1 D-FNORM) ;Maybe xct-next
       ((M-I) ADD M-I (A-CONSTANT 1))           ;Adjust exponent for right shift

(LOCALITY D-MEM)
(START-DISPATCH 4 0)    ;s.xyz high 4 bits of sum to be normalized
D-FNORM (INHIBIT-XCT-NEXT-BIT FNORM3)   ;0.000 shift left at least 3
        (FNORM2)                        ;0.001 shift left 2
        (FNORM1)                        ;0.010 shift left 1
        (FNORM1)                        ;0.011 shift left 1
        (FRND)                          ;0.100 OK
        (FRND)                          ;0.101 OK
        (FRND)                          ;0.110 OK
        (FRND)                          ;0.111 OK
        (FRND)                          ;1.000 OK
        (FRND)                          ;1.001 OK
        (FRND)                          ;1.010 OK
        (FRND)                          ;1.011 OK
        (FNORM1)                        ;1.100 shift left 1
        (FNORM1)                        ;1.101 shift left 1
        (FNORM2)                        ;1.110 shift left 2
        (INHIBIT-XCT-NEXT-BIT FNORM3)   ;1.111 shift left at least 3
(END-DISPATCH)
(LOCALITY I-MEM)

FNORM3  ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)
        ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)
        ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)
        ((Q-R) ANDCA Q-R (A-CONSTANT 7))        ;Zero the bits brought into Q
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-ZERO FNORM) ;Break the loop if trying
       ((M-I) SUB M-I (A-CONSTANT 3))              ; to normalize zero
;Return a floating-point zero (in internal form)
FLZERO  (declare (values a-i a-1))
        (POPJ-AFTER-NEXT (M-I) A-ZERO)
       ((M-1) A-ZERO)

;If M-2 seems to pale into insignificance, it might be SETZ, which doesn't
FADD3   (POPJ-GREATER-THAN M-TEM (A-CONSTANT 37))
        (POPJ-NOT-EQUAL M-2 (A-CONSTANT (BYTE-MASK FLONUM-SIGN-BIT)))
        (JUMP-XCT-NEXT FADD2)
       ((M-2) (M-CONSTANT -1))

FNORM2  ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)
        ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)
        ((Q-R) ANDCA Q-R (A-CONSTANT 3))        ;Zero the bits brought into Q
        (JUMP-XCT-NEXT FRND)
       ((M-I) SUB M-I (A-CONSTANT 2))

FNORM1  ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)
        ((Q-R) ANDCA Q-R (A-CONSTANT 1))        ;Zero the bit brought into Q
        ((M-I) SUB M-I (A-CONSTANT 1))
        ;drops through
;Floating-point rounding routine.
;Get here with normalized mantissa in M-1, corresponding exponent in M-I,
;residual bits in Q-R.  Rounding cannot produce zero unless given zero,
;since the input is normalized.  Do not come here with zero in M-1
;unless M-I is zero and Q-R is non-negative, or an unnormalized
;result will be returned.
;After rounding, we renormalize with a 3-bit normalize since the rounding
;can make a positive number slightly bigger and a negative number slightly smaller,
;requiring a shift of 0, 1 right, or 1 left.
FRND    (POPJ-GREATER-OR-EQUAL Q-R A-ZERO)      ;Return if discarded bits < 1/2 lsb, no rounding required.
        (JUMP-NOT-EQUAL Q-R (A-CONSTANT 1_31.) FRND2)   ;If discarded bits = 1/2 lsb exactly,
        (POPJ-IF-BIT-CLEAR (BYTE-FIELD 1 0) M-1) ; then round to even
FRND2   ((M-1) ADD M-1 (A-CONSTANT 1)           ;Add 1 lsb to mantissa, and
                OUTPUT-SELECTOR-RIGHTSHIFT-1 SHIFT-Q-RIGHT) ; capture overflow
FRND1   (DISPATCH SIGN-BIT-AND-MANTISSA-HIGH-TWO M-1 D-FRND) ;Renormalize & popj
        ((M-I) ADD M-I (A-CONSTANT 1))          ;Right shift was good, fix exponent, popj

;This code is heavily bummed.  Beware.
;Note that Q normally has full low-order word.  From SFLPACK has garbage but won't be used.
FRND3   ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1 SHIFT-Q-LEFT)    ;Restores the LSB from the Q.
FRND4   (POPJ-AFTER-NEXT (M-I) SUB M-I (A-CONSTANT 1))
       ((M-1) M-1 OUTPUT-SELECTOR-LEFTSHIFT-1)  ;Restores the LSB from the Q.

(LOCALITY D-MEM)
(START-DISPATCH 3 0)  ;s.xx renormalize after round
D-FRND  (INHIBIT-XCT-NEXT-BIT FRND3)    ;0.00 shift left two
        (FRND4)                         ;0.01 shift left one
        (R-BIT)                         ;0.10 OK
        (R-BIT)                         ;0.11 OK
        (R-BIT)                         ;1.00 OK
        (R-BIT)                         ;1.01 OK
        (FRND4)                         ;1.10 shift left one
        (INHIBIT-XCT-NEXT-BIT FRND3)    ;1.11 shift left two
(END-DISPATCH)
(LOCALITY I-MEM)

;Exchange the arguments to FADD when the second has bigger exponent
FADD1   ((M-I) M-J)             ;Result exponent is exp of 2nd arg
        ((M-TEM) M-A-1 (M-CONSTANT -1) A-TEM)   ;Repair exponent difference
        ((M-4) M-2)             ;Exchange mantissas
        (POPJ-AFTER-NEXT (M-2) M-1)
       ((M-1) M-4)

;Negate operand 1.
;Normally just change the sign of the mantissa, but note that
;to retain normalization 1/2 becomes -1 and -1 becomes 1/2, with adjustment of the exponent
FLONUM-MINUS
FNEG1   (declare (args a-i a-1) (values a-i a-1))
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-1 A-ZERO FNEG1A)      ;Jump if input positive
       ((M-1) SUB M-ZERO A-1)                           ;Change sign of mantissa
        (POPJ-IF-BIT-CLEAR FLONUM-SIGN-BIT M-1)         ;Return if negative became positive
        (POPJ-AFTER-NEXT                                ;Otherwise generate 1/2 and increase
         (M-1) DPB (M-CONSTANT -1) MANTISSA-HIGH-BIT A-ZERO) ; exponent since it must have
       ((M-I) ADD M-I (A-CONSTANT 1))                        ; been -1 which is "SETZ"

FNEG1A  (POPJ-NOT-EQUAL M-1 (A-CONSTANT (BYTE-MASK SIGN-BIT-AND-MANTISSA-HIGH-BIT)))
        (POPJ-AFTER-NEXT                        ;If result is -1/2,
         (M-1) DPB (M-CONSTANT -1) FLONUM-SIGN-BIT A-ZERO)      ;Turn it into -1
       ((M-I) SUB M-I (A-CONSTANT 1))           ;and decrease exponent

;Negate operand 2.
;Normally just change the sign of the mantissa, but note that
;to retain normalization 1/2 becomes -1 and -1 becomes 1/2, with adjustment of the exponent
FNEG2   (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-2 A-ZERO FNEG2A)      ;Jump if input positive
       ((M-2) SUB M-ZERO A-2)                           ;Change sign of mantissa
        (POPJ-IF-BIT-CLEAR FLONUM-SIGN-BIT M-2)         ;Return if negative became positive
        (POPJ-AFTER-NEXT                                ;Otherwise generate 1/2 and increase
         (M-2) DPB (M-CONSTANT -1) MANTISSA-HIGH-BIT A-ZERO) ; exponent since it must have
       ((M-J) ADD M-J (A-CONSTANT 1))                        ; been -1 which is "SETZ"

FNEG2A  (POPJ-NOT-EQUAL M-2 (A-CONSTANT (BYTE-MASK SIGN-BIT-AND-MANTISSA-HIGH-BIT)))
        (POPJ-AFTER-NEXT                        ;If result is -1/2,
         (M-2) DPB (M-CONSTANT -1) FLONUM-SIGN-BIT A-ZERO)      ;Turn it into -1
       ((M-J) SUB M-J (A-CONSTANT 1))           ;and decrease exponent

;Floating Division.
;First, make both arguments positive, and remember if the result is to
;be negative.  Also handle arguments of zero at this stage.  Then,
;arrange for the quotient to always be normalized by dividing the
;dividend by 2 if it is greater than the divisor.  This makes the
;result mantissa be between 1/2 and 1.  Note that if the dividend and
;divisor are equal, dividing the dividend by 2 could end up producing
;an unnormalized quotient less than 1/2 because of truncation error.
;We fix this by checking specially for the case of dividend and divisor
;equal.  To get a properly-scaled quotient, we shift the dividend left
;31. bits, plus 1 more bit to get it to a word boundary.  The extra bit
;is compensated for by doing one less divide step.  After dividing, we
;do stable rounding by comparing the remainder against half the
;divisor.  Recall that divide overflow occurs if the high word of the
;dividend is greater than or equal to the divisor.
FDIV    (CALL-EQUAL M-2 A-ZERO TRAP)
                (ERROR-TABLE DIVIDE-BY-ZERO)
                (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
        (POPJ-EQUAL M-1 A-ZERO)                 ;(// 0.0 non-0) = 0.0
        (JUMP-LESS-THAN M-2 A-ZERO FDIV3)       ;Jump if divisor negative
        (JUMP-LESS-THAN M-1 A-ZERO FDIV4)       ;Jump if dividend negative
FDIV1   ((M-I) M-I ADD (A-CONSTANT FLONUM-EXPONENT-EXCESS))
        (JUMP-LESS-THAN-XCT-NEXT M-1 A-2 FDIV2) ;If dividend >= divisor,
       ((M-I) SUB M-I A-J)
        (JUMP-EQUAL M-1 A-2 FDIV7)
        ((M-1) (BYTE-FIELD 31. 1) M-1)          ;shift dividend right 1,
        ((M-I) ADD M-I (A-CONSTANT 1))          ;and increase exponent of result
FDIV2   ((Q-R) M-ZERO)                          ;Low bits of dividend
        ((M-1) DIVIDE-FIRST-STEP M-1 A-2)       ;Do the division, doesn't call DIV due to
(REPEAT 30. ((M-1) DIVIDE-STEP M-1 A-2))        ; register conflicts and orneriness
        ((M-1) DIVIDE-LAST-STEP M-1 A-2)
        ((M-TEM) DIVIDE-REMAINDER-CORRECTION-STEP M-1 A-2)
        ;At this point, the normalized positive quotient is in Q-R, remainder is in M-TEM
        ;We'd like to shift the remainder left and do an unsigned compare, but that
        ;operation isn't available so we shift the divisor right and lose a bit.
        ((M-TEM1) (BYTE-FIELD 31. 1) M-2)
        (POPJ-LESS-THAN-XCT-NEXT M-TEM A-TEM1)  ;Round down if remainder < 1/2 divisor
       ((M-1) Q-R)
        (JUMP-GREATER-THAN M-TEM A-TEM1 FDIV6)  ;Round up if remainder > 1/2 divisor
        (POPJ-IF-BIT-CLEAR (BYTE-FIELD 1 0) M-1);Round to even lsb if remainder = 1/2 divisor
FDIV6   (JUMP-XCT-NEXT FRND1)                   ;Duplicate instruction at FRND2 for speed
       ((M-1) ADD M-1 (A-CONSTANT 1)            ;Add 1 lsb to mantissa, and
                OUTPUT-SELECTOR-RIGHTSHIFT-1 SHIFT-Q-RIGHT) ; capture overflow

;Divisor is negative.  Change its sign and check sign of dividend
FDIV3   (JUMP-GREATER-THAN-XCT-NEXT M-1 A-ZERO FDIV5)   ;Jump on positive dividend
       (CALL FNEG2)
        (JUMP-XCT-NEXT FDIV1)                   ;Both negative, result is positive
       (CALL FNEG1)

;Divisor is positive but dividend is negative.  Result is negative.
FDIV4   (CALL FNEG1)                            ;Change sign of dividend
FDIV5   (JUMP-XCT-NEXT FNEG1)                   ;Result is negative, get positive
       (CALL FDIV1)                             ; quotient and return it negated.

;Dividend and divisor mantissas equal.  Quotient mantissa is 1/2.
FDIV7   (POPJ-AFTER-NEXT (M-1) DPB (M-CONSTANT -1) MANTISSA-HIGH-BIT A-ZERO)
       ((M-I) ADD M-I (A-CONSTANT 1))

;Floating Multiplication.
FMPY    (CALL-XCT-NEXT MPY)     ;Product of mantissas to M-2(high), Q-R(low)
       ((Q-R) M-2)
        (JUMP-EQUAL M-2 A-ZERO FLZERO) ;If high product of normalized operands is zero, the
                                ; whole product is zero.  Return proper zero.
        ((M-1) M-2)             ;Get result of MPY into M-1
        ((M-I) M-I SUB (A-CONSTANT FLONUM-EXPONENT-EXCESS))
        (DISPATCH-XCT-NEXT SIGN-BIT-AND-MANTISSA-HIGH-TWO
                        M-1 D-FMPY) ;Normalize.  May need 0, 1, or 2 left shifts
       ((M-I) M+A+1 M-I A-J)    ;Exponent of product if no shifts

(LOCALITY D-MEM)
(START-DISPATCH 3 0)    ;s.xy high bits of product
D-FMPY  (FNORM2)        ;0.00 shift left 2
        (FNORM1)        ;0.01 shift left 1
        (FRND)          ;0.10 OK
        (FRND)          ;0.11 OK
        (FRND)          ;1.00 OK
        (FRND)          ;1.01 OK
        (FNORM1)        ;1.10 shift left 1
        (FNORM2)        ;1.11 shift left 2
(END-DISPATCH)
(LOCALITY I-MEM)

;= for flonums.
FEQL    (POPJ-NOT-EQUAL-XCT-NEXT M-I A-J)
       ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

FGRP    (JUMP-IF-BIT-SET FLONUM-SIGN-BIT M-1 FGRP-1)
        (JUMP-IF-BIT-SET FLONUM-SIGN-BIT M-2 XTRUE)
        ;; Both operands to GREATERP positive
        (JUMP-GREATER-THAN M-I A-J XTRUE)
        (POPJ-LESS-THAN-XCT-NEXT M-I A-J)
       ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

FGRP-1  (JUMP-IF-BIT-CLEAR FLONUM-SIGN-BIT M-2 XFALSE)
        ;; Both operands to GREATERP negative
        (JUMP-LESS-THAN M-I A-J XTRUE)
        (POPJ-GREATER-THAN-XCT-NEXT M-I A-J)
       ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

FLSP    (JUMP-IF-BIT-SET FLONUM-SIGN-BIT M-1 FLSP-1)
        (JUMP-IF-BIT-SET FLONUM-SIGN-BIT M-2 XFALSE)
        ;; Both operands to LESSP positive
        (JUMP-LESS-THAN M-I A-J XTRUE)
        (POPJ-GREATER-THAN-XCT-NEXT M-I A-J)
       ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

FLSP-1  (JUMP-IF-BIT-CLEAR FLONUM-SIGN-BIT M-2 XTRUE)
        ;; Both operands to LESSP negative
        (JUMP-GREATER-THAN M-I A-J XTRUE)
        (POPJ-LESS-THAN-XCT-NEXT M-I A-J)
       ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

FMAX    (CALL FLSP)
        (JUMP-EQUAL M-T A-V-NIL FIX-FMAX-FMIN-RETURN-ADDRESS)
        ((M-1) M-2)
        ((M-I) M-J)
FIX-FMAX-FMIN-RETURN-ADDRESS
        ((M-TEM) MICRO-STACK-PC-DATA-POP)
        (JUMP-EQUAL M-TEM (A-CONSTANT (I-MEM-LOC SFLPACK-P)) SFLPACK-T)
        (JUMP FLOPACK-T)

FMIN    (CALL FGRP)
        (JUMP-EQUAL M-T A-V-NIL FIX-FMAX-FMIN-RETURN-ADDRESS)
        ((M-1) M-2)
        (JUMP-XCT-NEXT FIX-FMAX-FMIN-RETURN-ADDRESS)
       ((M-I) M-J)

XFLOAT-DOUBLE (MISC-INST-ENTRY %FLOAT-DOUBLE)
        (CALL FXGTPP)
        ((M-1) DPB M-1
         (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 0)
                     (DIFFERENCE 31. Q-POINTER-WIDTH))
         A-ZERO)
        ((M-1)
         (BYTE-FIELD (DIFFERENCE 31. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH
                                 (DIFFERENCE 31. Q-POINTER-WIDTH)))
         M-2 A-1)
        (JUMP-EQUAL M-1 A-ZERO FLOAT-DOUBLE-2)
        ((M-TEM) DPB M-2
         (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH
                                 (DIFFERENCE 31. Q-POINTER-WIDTH))
                     (DIFFERENCE 32.
                                 (DIFFERENCE Q-POINTER-WIDTH
                                             (DIFFERENCE 31. Q-POINTER-WIDTH))))
         A-ZERO)
        ((Q-R) M-TEM)
        ((M-I) (A-CONSTANT (PLUS 2000 Q-POINTER-WIDTH Q-POINTER-WIDTH -1)))
FLOAT-DOUBLE-1
        (JUMP-XCT-NEXT FLOPACK-T)
       (CALL FNORM)

FLOAT-DOUBLE-2
        ((M-1) DPB M-2
         (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 0)
                     (DIFFERENCE 31. Q-POINTER-WIDTH))
         A-ZERO)
        ((Q-R) A-ZERO)
        (JUMP-XCT-NEXT FLOAT-DOUBLE-1)
       ((M-I) (A-CONSTANT (PLUS 2000 Q-POINTER-WIDTH -1)))

;;;  Bignum arithmetic.

(DEF-DATA-FIELD BIGNUM-HEADER-SIGN 1 18.)
(DEF-DATA-FIELD BIGNUM-HEADER-LENGTH 18. 0)

ARITH-BIG-ANY
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-T D-NUMARG2 (I-ARG NUMBER-CODE-BIGNUM))
    (ERROR-TABLE ARGTYP NUMBER M-T 1)   ;not continuable, bignum could move
    (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
       ((M-I) BIGNUM-HEADER-LENGTH M-C)
ARITH-BIG-SFL
ARITH-BIG-FLO
        (CALL FLOAT-A-BIGNUM)
        (DISPATCH (BYTE-FIELD 4 0) M-A D-FORWARD-FLONUM-OPS)
    (ERROR-TABLE ARGTYP INTEGER M-Q 0)
    (ERROR-TABLE ARG-POPPED 0 M-Q M-T)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC FLOPACK-P)))

ARITH-ANY-BIG
        (DISPATCH-XCT-NEXT (BYTE-FIELD 3 0) M-R D-ARITH-ANY-BIG)
       ((M-J) BIGNUM-HEADER-LENGTH M-D)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)
D-ARITH-ANY-BIG
        (ARITH-FIX-BIG)
        (ARITH-SFL-BIG)
        (ARITH-FLO-BIG)
        (ARITH-BIG-BIG)
 (REPEAT NUM-UNUSED-NUMBER-CODES (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

ARITH-SFL-BIG
ARITH-FLO-BIG
        ((M-TEM) M-I)
        ((M-I) M-J)
        ((M-J) M-TEM)
        ((C-PDL-BUFFER-POINTER-PUSH) M-Q)
        ((M-Q) M-T)
        ((M-C) M-D)
        (CALL-XCT-NEXT FLOAT-A-BIGNUM)
       ((M-2) M-1)
        ((M-T) C-PDL-BUFFER-POINTER-POP)
        (DISPATCH (BYTE-FIELD 4 0) M-A D-REVERSE-FLONUM-OPS)
    (ERROR-TABLE ARGTYP INTEGER M-T 1)
    (ERROR-TABLE ARG-POPPED 0 M-T M-Q)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC FLOPACK-P)))

ARITH-BIG
        (DISPATCH-XCT-NEXT (BYTE-FIELD 4 0) M-A D-BIGNUM-1ARG)
       ((M-I) BIGNUM-HEADER-LENGTH M-C)

(LOCALITY D-MEM)
(START-DISPATCH 4 0)
D-BIGNUM-1ARG
        (BIGNUM-ABS)
        (BIGNUM-MINUS)
        (XFALSE)                ;ZEROP OF A BIGNUM!!!!!
        (BIGNUM-PLUSP)
        (BIGNUM-MINUSP)
        (BIGNUM-ADD1)
        (BIGNUM-SUB1)
        (BIGNUM-FIX)
        (BIGNUM-FLOAT)
        (BIGNUM-SMALL-FLOAT)
        (BIGNUM-HAULONG)
        (BIGNUM-LDB)
        (BIGNUM-DPB)
        (BIGASH)
        (BIGNUM-ODDP)
        (BIGNUM-EVENP)
 (REPEAT NUM-UNUSED-ARITH-1ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

;;; Cons up a bignum.
;;; Inputs: M-B length+1, M-C sign in BIGNUM-HEADER-SIGN position
;;; Outputs: M-T boxed bignum, M-C sign/length part of header, M-E,M-K,M-S bashed
;;           M-3, M-4, q-r also bashed.
;;;          VMA same as M-T, MD header
;;; Note that M-1 and M-2 are preserved
BNCONS ;(CALL SCONS-T)                          ;Cons in structure space, extra-pdl
        (call allocate-bignum-storage)
        ((M-TEM) SUB M-B (A-CONSTANT 1))        ;Length to go in header
        ((M-C) SELECTIVE-DEPOSIT M-C BIGNUM-HEADER-SIGN A-TEM)  ;Incorporate sign
        ((WRITE-MEMORY-DATA) ADD M-C            ;Make rest of header
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                  (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        (POPJ-AFTER-NEXT (VMA-START-WRITE M-T)  ;Store header, fix M-T data type
                Q-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
       (CHECK-PAGE-WRITE)

;; MD has the header of the bignum whether got here from ABS or from GCD
BIGNUM-ABS
        (JUMP-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN MD BIGNUM-COPY)
       ((M-C) M-I)                              ;Positive-signed header
RETURN-M-Q
        (POPJ-AFTER-NEXT (M-T) M-Q)
       (NO-OP)

BIGNUM-MINUS
        (JUMP-NOT-EQUAL M-C (A-CONSTANT (BYTE-VALUE BIGNUM-HEADER-LENGTH 1))
                        BIGNUM-MINUS-1) ;check for +setzness
        ((VMA-START-READ) ADD M-Q (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (JUMP-NOT-EQUAL MD (A-CONSTANT POSITIVE-SETZ) BIGNUM-MINUS-1)
        (POPJ-AFTER-NEXT (M-T) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                                 POSITIVE-SETZ)))
       (NO-OP)
BIGNUM-MINUS-1
        ((M-C) XOR M-C (A-CONSTANT (BYTE-MASK BIGNUM-HEADER-SIGN)))
;bignum in M-Q, new header(sign) in M-C, Length in M-I. Result in M-T.
BIGNUM-COPY
        (CALL-XCT-NEXT BNCONS)                  ;ALLOCATE IN STRUCTURE EXTRA-PDL
       ((M-B) ADD M-I (A-CONSTANT 1))
BIGNUM-COPY-L
        ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        ((VMA-START-WRITE) ADD M-T A-I)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-GREATER-THAN-XCT-NEXT M-I (A-CONSTANT 1) BIGNUM-COPY-L)
       ((M-I) SUB M-I (A-CONSTANT 1))
        (POPJ)

DPB-BIGNUM-SETUP  ;CALL HERE TO SET UP FOR DOING A DPB, SEE RELEVANT CODE.
        ((M-K) (BYTE-FIELD 6 0) C-PDL-BUFFER-POINTER)
        ((M-E) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6) C-PDL-BUFFER-POINTER)
        ((M-1) ADD M-K A-E)                     ;COMPUTE BIT POSITION OF LEFT EDGE OF BYTE
        ((M-1) ADD M-1 (A-CONSTANT 31.))        ;ROUND UP
                ;Note the inclusion of one extra bit.  This is in case we produce
                ;a negative "SETZ", which is 1 bit longer in sign-and-magnitude than
                ;in 2's complement.
        (CALL-XCT-NEXT DIV)     ;DIVIDE BY 31. TO GET NUMBER OF WORDS IN BIGNUM
       ((M-2) (A-CONSTANT 31.)) ;RETURN QUOTIENT IN Q-R
        ((M-B) Q-R)             ;NEED AT LEAST THIS MANY WORDS.
BIGNUM-COPY-EXPAND   ;Copy bignum.  Resulting bignum to have at least M-B words of
                     ;significance.  Start with bignum in M-Q, header in M-C, current
                     ;length in M-I.  Result in M-T.  As a special hack, if M-I is zero,
                     ;just allocate a 0 bignum.
        (JUMP-GREATER-OR-EQUAL M-I A-B BIGNUM-COPY)     ;No expansion needed, just copy
        (CALL-XCT-NEXT BNCONS)                          ;Allocate in structure extra-pdl
       ((M-B) ADD M-B (A-CONSTANT 1))                   ;Plus one for header
        ((M-B) SUB M-B (A-CONSTANT 1))
        (CALL-NOT-EQUAL-XCT-NEXT M-I A-ZERO BIGNUM-COPY-L)      ;Copy the number part (if any)
       ((M-ZR) SUB M-B A-I)             ;Save how many words to zero
        ((MD) A-ZERO)
BCE2    ((VMA-START-WRITE) ADD M-T A-B) ;Zero out the new words.
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-ZR) SUB M-ZR (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-ZR A-ZERO BCE2)
       ((M-B) SUB M-B (A-CONSTANT 1))
        (POPJ)

BIGNUM-PLUSP
        ((M-T) A-V-TRUE)                ;CORRECT SINCE NO BIGNUM ZERO
        (POPJ-AFTER-NEXT POPJ-IF-BIT-CLEAR BIGNUM-HEADER-SIGN M-C)
       ((M-T) A-V-NIL)

BIGNUM-MINUSP
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET BIGNUM-HEADER-SIGN M-C)
       ((M-T) A-V-NIL)

BIGNUM-FIX
        (POPJ-AFTER-NEXT (M-T) M-Q)
       (NO-OP)

BIGNUM-HAULONG
        ((VMA-START-READ) ADD M-Q A-I)  ;GET HIGH ORDER WORD
        (CHECK-PAGE-READ)
        ;; (length - 1) * 31. =  (length * 32.) - length - 31.
        ;; XHAUL1 wants this in M-T  and the high bits in M-1.
        ((M-T) DPB M-I (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 5)) 5.)
         (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-T) SUB M-T A-I)
        ((M-T) SUB M-T (A-CONSTANT 31.))
        (JUMP-XCT-NEXT XHAUL1)
       ((M-1) MD)

BIGNUM-FLOAT
        (JUMP-XCT-NEXT FLOPACK-T)
       (CALL FLOAT-A-BIGNUM)

BIGNUM-SMALL-FLOAT
        (JUMP-XCT-NEXT SFLPACK-T)
       (CALL FLOAT-A-BIGNUM)

BIGNUM-ODDP
        ((VMA-START-READ) ADD M-Q (A-CONSTANT 1))       ;Low-order word
        (CHECK-PAGE-READ)
        (JUMP-XCT-NEXT XFALSE)
       (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) READ-MEMORY-DATA XTRUE)

BIGNUM-EVENP
        ((VMA-START-READ) ADD M-Q (A-CONSTANT 1))       ;Low-order word
        (CHECK-PAGE-READ)
        (JUMP-XCT-NEXT XFALSE)
       (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) READ-MEMORY-DATA XTRUE)

;;; Convert a bignum to a flonum.  Takes the length of the bignum in M-I,
;;; the bignum pointer in M-Q, the rest-of-header in M-C.  Leaves an internal-format
;;; flonum in M-I and M-1.  Clobbers M-4, M-3, M-1, M-K, M-TEM, M-T.  Must NOT clobber
;;; M-A, M-2 and M-J!
FLOAT-A-BIGNUM
        ;; First get the second-to-highest order word into M-3.
        ;; (If there is only one word, get zeroes.)
        (JUMP-EQUAL-XCT-NEXT M-I (A-CONSTANT 1) FLOAT-A-BIGNUM-X)
       ((M-3) A-ZERO)
        ((M-TEM) SUB M-I (A-CONSTANT 1))
        ((VMA-START-READ) ADD M-Q A-TEM)
        (CHECK-PAGE-READ)
        ((M-3) MD)
FLOAT-A-BIGNUM-X
        ;; Now get the highest order word in M-1 and get its length in M-T.
        ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        ((M-T) A-ZERO)
        (CALL-XCT-NEXT XHAUL1)
       ((M-1 C-PDL-BUFFER-POINTER-PUSH) MD) ;EVIL ON PDL BUFFER, BUT WILL BE POPPED SOON
        ;; If M-T contains 31. then the mantissa is on the pdl
        ;; no need to ldb/dpb anything (in fact it won't work!)
        (JUMP-EQUAL M-T (A-CONSTANT 31.) FLOAT-A-BIGNUM-31)
        ;; Now piece together the mantissa of the flonum into M-1.
        ;; First LDB from M-3, with:
        ;;              BYTL-1 = (30. - M-T)    MROT = (32. - M-T)
        ;; Then DPB from C-PDL-BUFFER-POINTER-POP into M-1, with:
        ;;              BYTL-1 = (M-T - 1)      MROT = (31. - M-T)
        ((M-TEM) SUB (M-CONSTANT 32.) A-T)
        ((M-4) SUB M-TEM (A-CONSTANT 2))
#+exp   ((m-tem3) add m-4 (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-4 #+exp m-tem3 OAL-BYTL-1 A-TEM)
        ((M-1) (BYTE-FIELD 0 0) M-3 A-ZERO)
        ((OA-REG-LOW) #+exp oal-mrot M-TEM)     ;Rotate first dropped bit into sign of M-3
        ((M-3) (BYTE-FIELD 32. 0) M-3)
        ((M-K) SUB M-T (A-CONSTANT 1))
        ((M-TEM) ADD M-4 (A-CONSTANT 1))
#+exp   ((m-tem3) add m-k (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-K #+exp m-tem3 OAL-BYTL-1 A-TEM)
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 0 0) A-1)
FLOAT-A-BIGNUM-DONE
        ;; length in M-I   nbits (sig bits in high order word) in M-T
        ;; (length - 1) * 31. + nbits + 2000 =
        ;;       (length * 32. + nbits) - length + 1741
        ((M-T) DPB M-I (BYTE-FIELD 27. 5.) A-T) ;Clears data-type
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 31.) M-3 FLOAT-A-BIGNUM-EXIT)
       ((M-T) SUB M-T A-I)
        ((M-1) ADD M-1 (A-CONSTANT 1))          ;First dropped bit was a 1, round up
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 31.) M-1 FLOAT-A-BIGNUM-EXIT)
        ((M-1) (BYTE-FIELD 31. 1) M-1)          ;Overflowed, shift right and
        ((M-T) ADD M-T (A-CONSTANT 1))          ; increase exponent
FLOAT-A-BIGNUM-EXIT
        (POPJ-AFTER-NEXT (M-I) ADD M-T (A-CONSTANT 1741))
       (CALL-IF-BIT-SET BIGNUM-HEADER-SIGN M-C FNEG1)

FLOAT-A-BIGNUM-31
        (JUMP-XCT-NEXT FLOAT-A-BIGNUM-DONE)
       ((M-1) C-PDL-BUFFER-POINTER-POP)


ARITH-BIG-BIG
        (DISPATCH-XCT-NEXT (BYTE-FIELD 4 0) M-A D-FORWARD-BIGNUM-OPS)
       ((M-B) M-T)

(LOCALITY D-MEM)
;BIGNUMS IN M-B AND M-T, M-Q.  THEIR HEADERS IN M-D, M-C.  LENGTHS IN M-J, M-I.
(START-DISPATCH 4 0)
D-FORWARD-BIGNUM-OPS
        (BADD)
        (BSUB)
        (BMPY)
        (BIDIV)
        (BEQL)
        (BGRP)
        (BLSP)
        (BMIN)
        (BMAX)
        (BBOOLE)
        (BDIV)
        (BEQL)
 (REPEAT NUM-UNUSED-ARITH-2ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

BEQL    (POPJ-NOT-EQUAL-XCT-NEXT M-C A-D)
       ((M-T) A-V-NIL)
BEQL1   ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        ((M-1) MD)
        ((VMA-START-READ) ADD M-B A-I)
        (CHECK-PAGE-READ)
        (POPJ-NOT-EQUAL MD A-1)
        (JUMP-GREATER-THAN-XCT-NEXT M-I (A-CONSTANT 1) BEQL1)
       ((M-I) SUB M-I (A-CONSTANT 1))
        (POPJ-AFTER-NEXT (M-T) A-V-TRUE)
       (NO-OP)

;; this loops over two bignum's magnitudes (in M-Q,M-C,M-I and M-B,M-D,M-J) does nothing
;; if the first is larger than the second, puts M-E in M-T if they are equal
;; else moves M-A into M-T.  In any case POPJing out. Smashes M-I (which better equal M-J
;; anyway!!!!)

BSHFFL  ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        ((M-1) MD)
        ((VMA-START-READ) ADD M-B A-I)
        (CHECK-PAGE-READ)
        (POPJ-LESS-THAN MD A-1)                 ;first is bigger so popj
        (JUMP-NOT-EQUAL MD A-1 BSHFFL-1)        ;second is bigger, move and popj
        ;equal continue looping
        (JUMP-GREATER-THAN-XCT-NEXT M-I (A-CONSTANT 1) BSHFFL)
       ((M-I) SUB M-I (A-CONSTANT 1))
        (POPJ-AFTER-NEXT (M-T) M-E)             ;all equal return M-E
       (NO-OP)

BSHFFL-1
        (POPJ-AFTER-NEXT NO-OP)
       ((M-T) M-A)

;; compare two bignums (in M-Q,M-C,M-I and M-B,M-D,M-J) and return T if first
;; is bigger than the second. Uses M-A and M-T and M-E
BGRP    (JUMP-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-C BGRP-1)
       ((M-E) A-V-NIL)                          ;Value to return if equal
        (POPJ-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) A-V-TRUE)                         ;First is pos., second is neg.
        (POPJ-GREATER-THAN M-I A-J)             ;Both pos. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;Both pos. Same length so loop.
       ((M-A) A-V-NIL)                          ;M-A gets alternate answer
        (POPJ-AFTER-NEXT (M-T) A-V-NIL)         ;Both pos. Second longer.
       (NO-OP)

BGRP-1  (POPJ-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) A-V-NIL)                          ;First is neg. second is pos.
        (POPJ-GREATER-THAN M-I A-J)             ;both neg. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;both neg. same length so loop.
       ((M-A) A-V-TRUE)                         ;M-A gets other answer
        (POPJ-AFTER-NEXT (M-T) A-V-TRUE)        ;both neg. second longer.
       (NO-OP)

;; compare two bignums (in M-Q,M-C,M-I and M-B,M-D,M-J) and return T if second
;; is bigger than the first. Uses M-A and M-T and M-E
BLSP    (JUMP-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-C BLSP-1)
       ((M-E) A-V-NIL)                          ;Value to return if equal
        (POPJ-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) A-V-NIL)                          ;First is pos., second is neg.
        (POPJ-GREATER-THAN M-I A-J)             ;Both pos. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;Both pos. Same length so loop.
       ((M-A) A-V-TRUE)                         ;M-A gets alternate answer
        (POPJ-AFTER-NEXT (M-T) A-V-TRUE)        ;Both pos. Second longer.
       (NO-OP)

BLSP-1  (POPJ-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) A-V-TRUE)                         ;First is neg. second is pos.
        (POPJ-GREATER-THAN M-I A-J)             ;both neg. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;both neg. same length so loop.
       ((M-A) A-V-NIL)                          ;M-A gets other answer
        (POPJ-AFTER-NEXT (M-T) A-V-NIL)         ;both neg. second longer.
       (NO-OP)

;; compare two bignums (in M-Q,M-C,M-I and M-B,M-D,M-J) and return the bigger one.
;; Uses M-A and M-T and M-E
BMAX    (JUMP-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-C BMAX-1)
       ((M-E) M-Q)                              ;Value to return if equal
        (POPJ-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) M-Q)                              ;First is pos., second is neg.
        (POPJ-GREATER-THAN M-I A-J)             ;Both pos. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;Both pos. Same length so loop.
       ((M-A) M-B)                              ;M-A gets alternate answer
        (POPJ-AFTER-NEXT (M-T) M-B)             ;Both pos. Second longer.
       (NO-OP)

BMAX-1  (POPJ-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) M-B)                              ;First is neg. second is pos.
        (POPJ-GREATER-THAN M-I A-J)             ;both neg. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;both neg. same length so loop.
       ((M-A) M-Q)                              ;M-A gets other answer
        (POPJ-AFTER-NEXT (M-T) M-Q)             ;both neg. second longer.
       (NO-OP)

;; compare two bignums (in M-Q,M-C,M-I and M-B,M-D,M-J) and return the smaller one.
;; Uses M-A and M-T and M-E
BMIN    (JUMP-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-C BMIN-1)
       ((M-E) M-Q)                              ;Value to return if equal
        (POPJ-IF-BIT-SET-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) M-B)                              ;First is pos., second is neg.
        (POPJ-GREATER-THAN M-I A-J)             ;Both pos. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;Both pos. Same length so loop.
       ((M-A) M-Q)                              ;M-A gets alternate answer
        (POPJ-AFTER-NEXT (M-T) M-Q)             ;Both pos. Second longer.
       (NO-OP)

BMIN-1  (POPJ-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-D)
       ((M-T) M-Q)                              ;First is neg. second is pos.
        (POPJ-GREATER-THAN M-I A-J)             ;both neg. First longer.
        (JUMP-EQUAL-XCT-NEXT M-I A-J BSHFFL)    ;both neg. same length so loop.
       ((M-A) M-B)                              ;M-A gets other answer
        (POPJ-AFTER-NEXT (M-T) M-B)             ;both neg. second longer.
       (NO-OP)

;; For add and subtract build the answer in M-T,M-K . sign of answer is expected to
;; be the sign bit in M-C. First arg in M-Q,M-I second in M-R,M-J (note the move to M-R)

;; For addition we want the longest BIGNUM in M-R,M-J

BADD    ((M-TEM) XOR M-C A-D)
        (JUMP-IF-BIT-SET BIGNUM-HEADER-SIGN M-TEM BSUB1) ;signs don't agree so subtract
BADD1   (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-J A-I BADD2)
       ((M-2) A-ZERO)                           ;M-2 gets the carry
        ((M-TEM) M-I)                           ;Swap if second isn't largest.
        ((M-I) M-J)
        ((M-J) M-TEM)
        ((M-B) M-Q)                             ;M-T and M-B contain the same thing!
        ((M-Q) M-T)
BADD2   ((M-R) M-B)
        (CALL-XCT-NEXT BNCONS)                  ;Allocate result bignum
       ((M-B) ADD M-J (A-CONSTANT 2))
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
                                         (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((M-D) (A-CONSTANT 1))                  ;M-D counts up
BADD3   ((VMA-START-READ) ADD M-Q A-D)
        (CHECK-PAGE-READ)
        ((M-1) ADD MD A-2)                      ;M-2 has carry from last round
        ((VMA-START-READ) ADD M-R A-D)
        (CHECK-PAGE-READ)
        ((M-1) ADD MD A-1)                      ;M-1 now has sum (carry and 31 bits out)
        ((MD) (BYTE-FIELD 31. 0) M-1 A-ZERO)    ;Write 31 bits
        ((M-2) (BYTE-FIELD 1 31.) M-1 A-ZERO)   ;save carry
        ((VMA-START-WRITE) ADD M-T A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-THAN-XCT-NEXT M-D A-I BADD3)
       ((M-D) ADD M-D (A-CONSTANT 1))
        (JUMP-GREATER-THAN M-D A-J BADD4)       ;Jump if lengths (M-I,M-J) were equal,
                                                ; there are no more words to add in
;;FIXNUM - BIGNUM addition joins us here (can drop in)
;; Bignum in M-R,M-J. 1 (sometimes) in M-D. Fixnum in M-2. Answer in M-T with header in M-C.
BADD5   ((VMA-START-READ) ADD M-R A-D)
        (CHECK-PAGE-READ)
        ((M-1) ADD MD A-2)                      ;M-2 has carry
        ((MD) (BYTE-FIELD 31. 0) M-1 A-ZERO)
        ((M-2) (BYTE-FIELD 1 31.) M-1 A-ZERO)
        ((VMA-START-WRITE) ADD M-T A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-THAN-XCT-NEXT M-D A-J BADD5) ;M-J'th word is last in M-R bignum
       ((M-D) ADD M-D (A-CONSTANT 1))
BADD4   (JUMP-GREATER-THAN M-2 A-ZERO BADD6)    ;There was some carry, so store in last word.
        ((M-C) SUB M-C (A-CONSTANT 1))          ;no carry so give word back.
        ((MD) ADD M-C (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE) M-T)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-1) ADD M-T A-D)
        (JUMP-XCT-NEXT UN-CONS)
       ((M-2) (A-CONSTANT 1))

BADD6   ((MD) M-2)
        ((VMA-START-WRITE) ADD M-T A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ)          ;NO POPJ-AFTER-NEXT, COULD BE RETURNING TO MAIN LOOP

;; Subtraction:
BSUB    ((M-TEM) XOR M-C A-D)
        (JUMP-IF-BIT-SET BIGNUM-HEADER-SIGN M-TEM BADD1)        ;signs don't agree so add
;; first we shuffle the bignums around to be sure of subtracting the smaller magnitude
;; from the larger. Note that if we switch them then we must complement the sign bit in M-C.
BSUB1   ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
        ((M-R) M-T)                             ;will need M-T for answer
        (JUMP-GREATER-THAN-XCT-NEXT M-I A-J BSUB-OK)
       ((M-D) M-I)                              ;M-D gets the number of the last different word
        (JUMP-LESS-THAN M-I A-J BSUB-SWITCH)
        ;drops in
;; they are the same length so count M-D down until you find a word that is different.
;; M-J is also kept equal to M-D since there is no need to remember the words out there
;; if when you subtract them you get zero.  (7623456123-7623456032 is the same as 123-032 !)
BSUB-L  ((VMA-START-READ) ADD M-Q A-D)
        (CHECK-PAGE-READ)
        ((M-J) M-D)
        ((M-1) MD)
        ((VMA-START-READ) ADD M-R A-D)
        (CHECK-PAGE-READ)
        (JUMP-LESS-THAN MD A-1 BSUB-OK)
        (JUMP-GREATER-THAN MD A-1 BSUB-SWITCH-1)
        (JUMP-GREATER-THAN-XCT-NEXT M-D (A-CONSTANT 1) BSUB-L)
       ((M-D) SUB M-D (A-CONSTANT 1))
        (POPJ-AFTER-NEXT
         (M-T) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))   ;EQUAL!
       (NO-OP)

BSUB-SWITCH
        ((M-TEM) M-I)                           ;Switch (but nobody cares about M-I)
        ((M-D) M-J)
        ((M-J) M-TEM)
BSUB-SWITCH-1
        ((M-C) XOR M-C (A-CONSTANT (BYTE-MASK BIGNUM-HEADER-SIGN)))     ;Switch sign bit.
        ((M-R) M-Q)
        ((M-Q) M-T)                             ;M-T still contains the original thing!

;; we have now cleverly arranged for M-D to be the length of the longest possible answer
;; M-Q,(M-I *) contain the bigger magnitude bignum M-R,M-J the smaller
;; correct sign bit of answer is in M-C, answer to be built in M-T,M-C (sign bit kept
;; in M-C)
;; (* note that we really don't care about M-I so we havn't actually made sure it contains
;; the correct thing)
BSUB-OK (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-D (A-CONSTANT 1))
        ((M-2) A-ZERO)                          ;borrow
        ((M-B) (A-CONSTANT 1))                  ;counter
BSUB-IT ((VMA-START-READ) ADD M-R A-B)
        (CHECK-PAGE-READ)
        ((M-1) ADD MD A-2)
        ((VMA-START-READ) ADD M-Q A-B)
        (CHECK-PAGE-READ)
        ((M-1) SUB MD A-1)
        ((MD M-3) (BYTE-FIELD 31. 0) M-1 A-ZERO)
        ((VMA-START-WRITE) ADD M-T A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-EQUAL-XCT-NEXT M-3 A-ZERO BSUB-IT1)
       ((M-2) (BYTE-FIELD 1 31.) M-1 A-ZERO)
        ((M-E) M-B)                             ;M-E gets number of last non-zero word stored
BSUB-IT1
        (JUMP-LESS-THAN-XCT-NEXT M-B A-J BSUB-IT)
       ((M-B) ADD M-B (A-CONSTANT 1))
        (JUMP-GREATER-THAN M-B A-D BCLEANUP)    ;Jump if no more words to borrow into
;;FIXNUM - BIGNUM subtraction joins us here.
;; Bignum in M-Q,M-D (yes M-D!). 1 in M-B. Fixnum in M-2. Answer in M-T with header in M-C.
;; 1 should be in M-E (despite the fact that that might be wrong, the answer will be
;; spotted as a fixnum zero anyway!)
BSUB-C  ((VMA-START-READ) ADD M-Q A-B)
        (CHECK-PAGE-READ)
        ((M-1) SUB MD A-2)
        ((MD M-3) (BYTE-FIELD 31. 0) M-1 A-ZERO)
        ((VMA-START-WRITE) ADD M-T A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-EQUAL-XCT-NEXT M-3 A-ZERO BSUB-C1)
       ((M-2) (BYTE-FIELD 1 31.) M-1 A-ZERO)
        ((M-E) M-B)                             ;Index of last non-zero word
BSUB-C1 (JUMP-LESS-THAN-XCT-NEXT M-B A-D BSUB-C)
       ((M-B) ADD M-B (A-CONSTANT 1))
        (JUMP BCLEANUP)

;; multiply two bignums.
BMPY    ((M-R) M-T)
        ((M-K) ADD M-I A-J)     ;Possible length of answer
        ((M-C) XOR M-C A-D)     ;Sign in C is correct
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-K (A-CONSTANT 1))
        ((M-K) BIGNUM-HEADER-LENGTH M-C)                ;M-K was smashed by SCONS
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
                                         (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))

;; Now we have first arg in M-Q,M-I  second in M-R,M-J .  We are building the answer in
;; M-T,M-K .  The correct header for the answer lives in M-C.
;; M-S will index into the answer, M-D into first arg, M-E + 1 into second.
;; it must be true that M-D + M-E = M-S
;; the running total is kept in M-A,M-2,M-1
;; M-B gets M-J - 1 for comparison
;; M-ZR gets M-K - 1 for comparison
        ((M-B) SUB M-J (A-CONSTANT 1))
        ((M-ZR) SUB M-K (A-CONSTANT 1))
        ((M-S) (A-CONSTANT 1))
        ((M-A) A-ZERO)
        ((M-1) A-ZERO)
        ((M-2) A-ZERO)
BMPY-LOOP
        (JUMP-GREATER-THAN-XCT-NEXT M-S A-I BMPY-LOOP-1)
       ((M-D) M-I)
        ((M-D) M-S)             ;M-D gets min{M-I,M-S}
BMPY-LOOP-1
        ((VMA-START-READ) ADD M-Q A-D)
        (CHECK-PAGE-READ)
        ((M-E) SUB M-S A-D)
        ((M-3) MD)
        ((VMA-START-READ) M+A+1 M-R A-E)
        (CHECK-PAGE-READ)
        ((Q-R) MD)
        ;; Having loaded the 2 31 bit things to be multiplied into Q-R and M-3
        ;; this will multiply them and add the result into M-A,M-2,M-1
        ;; (31 bits in M-1 and M-2, less than 24 in M-A)
(REPEAT 31. ((M-1) MULTIPLY-STEP M-1 A-3))
        ((M-1) ADD M-1 A-2)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 31.) M-1 BMPY-C)
       ((M-2) (BYTE-FIELD 31. 0) M-1 A-ZERO)
        ((M-A) ADD M-A (A-CONSTANT 1))
BMPY-C  (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-E A-B BMPY-LOOP-1-DONE)       ;M-B = M-J - 1
       ((M-1) (BYTE-FIELD 31. 1) Q-R A-ZERO)
        (JUMP-GREATER-THAN-XCT-NEXT M-D (A-CONSTANT 1) BMPY-LOOP-1)
       ((M-D) SUB M-D (A-CONSTANT 1))
BMPY-LOOP-1-DONE
        ((MD) M-1)
        ((VMA-START-WRITE) ADD M-T A-S)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-1) M-2)
        ((M-2) M-A)
        ((M-A) A-ZERO)
        (JUMP-LESS-THAN-XCT-NEXT M-S A-ZR BMPY-LOOP)    ;M-ZR = M-K - 1
       ((M-S) ADD M-S (A-CONSTANT 1))
        (JUMP-NOT-EQUAL M-1 A-ZERO BMPY-FULL)
        ((M-C) SUB M-C (A-CONSTANT 1))          ;Result 1 word shorter than expected
        ((MD) ADD M-C (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE) M-T)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-1) ADD M-T A-K)
        (JUMP-XCT-NEXT UN-CONS)
       ((M-2) (A-CONSTANT 1))

BMPY-FULL
        ((MD) M-1)
        ((VMA-START-WRITE) ADD M-T A-S)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ)          ;NO POPJ-AFTER-NEXT, MIGHT BE RETURNING TO MAIN LOOP

;;; Bignum - Bignum division: (algorithm from Knuth Vol 2)
BIDIV
        ;;If second bignum is longer than the first bignum then the answer is 0
        (JUMP-GREATER-THAN M-J A-I RETURN-ZERO)
        ;;Get sign of answer into M-C by xoring with M-D
        ((M-D) SELECTIVE-DEPOSIT BIGNUM-HEADER-SIGN M-D A-ZERO)
        ((M-C) XOR M-C A-D)
        ;;If second is one word long then we can do Bignum - Fixnum division
        (JUMP-GREATER-THAN-XCT-NEXT M-J (A-CONSTANT 1) BIDIV-1)
       ((M-R) M-T)
        ((VMA-START-READ) ADD M-T (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (JUMP-XCT-NEXT BFXIDIV)
       ((M-2) MD)

BIDIV-1
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
        ;;Allocate a bignum for the answer: (put it in M-A)
        ((M-A) SUB M-I A-J)
        ((M-A) ADD M-A (A-CONSTANT 1))  ;Possible length of answer.
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-A (A-CONSTANT 1))
        (CALL-XCT-NEXT BIDIV-REMAINDER-COMMON)
       ((M-A) M-T)
        ;;M-Q,(M-I + 1) contains garbage. M-T,M-K contains the answer (with perhaps
        ;; a zero in the top word). M-C has the correct sign bit for the answer.
        ((M-1) M-Q)
        ((M-2) ADD M-I (A-CONSTANT 2))
        (CALL-XCT-NEXT UN-CONS)
       ((M-Q) A-V-NIL)                          ;clear pointer to possible garbage
        ((VMA-START-READ) ADD M-T A-K)          ;Quotient may be 1 too long.
        (CHECK-PAGE-READ)
        ((M-D) M-K)
        ((M-E) M-D)
        (JUMP-NOT-EQUAL-XCT-NEXT MD A-ZERO BCLEANUP)
       ((M-C) SELECTIVE-DEPOSIT M-C BIGNUM-HEADER-SIGN A-K)
        (JUMP-XCT-NEXT BCLEANUP)
       ((M-E) SUB M-E (A-CONSTANT 1))

;Bignum-bignum remainder
; We enter with the first bignum in M-C the second in M-B and the header
; of the first still in MD.
REMAINDER-BIG-BIG
        ((M-Q) M-C)
        ((M-C) HEADER-REST-FIELD MD)
        ((VMA-START-READ) M-B)
        (CHECK-PAGE-READ)
        ((M-R) M-B)
        ((M-I) BIGNUM-HEADER-LENGTH M-C)
        ((M-J) BIGNUM-HEADER-LENGTH MD)
        ;;If second bignum is longer than the first bignum then the answer is the first
        (JUMP-GREATER-THAN M-J A-I RETURN-M-Q)
        ;;Sign of answer is already in M-C
        ;;If second is one word long then do Bignum - Fixnum remainder
        (JUMP-GREATER-THAN M-J (A-CONSTANT 1) BIDIVR-2)
        ((VMA-START-READ) ADD M-R (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-B) M-Q)
        ((M-2) MD)
        ((M-A) BIGNUM-HEADER-SIGN M-C)
        ((M-C) BIGNUM-HEADER-LENGTH M-C)
        (CALL REMAINDER-BIG-FIX-1)
        (JUMP RETURN-M-1)

BIDIVR-2
        (CALL-XCT-NEXT BIDIV-REMAINDER-COMMON)
       ((M-A) A-ZERO)           ;Indicate that quotient is not being saved.
        ;;Now we have the remainder in M-Q,(M-I + 1) possibly shifted by
        ;; an amount determined by
        ;; the haulong still(!) in M-D. Sign of answer is still in M-C
        ;;To shift back we perform an operation similar to BIDIV-NORMALIZE:
        ;; First we LDB from the current word with: (M-K)
        ;;              BYTL-1 = Haulong - 1
        ;;              MROT = Haulong + 1
        ;; Then we DPB into that from the next higher word with: (M-S)
        ;;              BYTL-1 = 30. - haulong
        ;;              MROT = Haulong
        (JUMP-EQUAL-XCT-NEXT M-D (A-CONSTANT 31.) BIDIVR-3)
       ((M-T) M-Q)
        ((M-K) ADD M-D (A-CONSTANT 1))          ;MROT
        ((M-TEM) SUB M-D (A-CONSTANT 1))        ;BYTL-1
        ((M-K) DPB M-TEM OAL-BYTL-1 A-K)        ;For LDB
        ((M-TEM) (A-CONSTANT 30.))
        ((M-TEM) SUB M-TEM A-D)                 ;BYTL-1, MROT in M-D
        ((M-S) DPB M-TEM OAL-BYTL-1 A-D)        ;For DPB
        ((M-D) (A-CONSTANT 1))                  ;Counts through the bignum
        ((M-E) (A-CONSTANT 1))                  ;Gets number of last non-zero word
        ((VMA-START-READ) ADD M-T A-D)
        (CHECK-PAGE-READ)
        ((M-1) MD)                              ;M-1 has word from last round.
BIDIVR-UNNORMALIZE-LOOP
        ((VMA-START-READ) M+A+1 M-T A-D)
        (CHECK-PAGE-READ)
#+exp   ((m-tem3) add m-k (a-constant 1_5))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-K)
        ((M-2) (BYTE-FIELD 0 0) M-1 A-ZERO)     ;LDB out of lower word
        ((M-1) MD)
#+exp   ((m-tem3) add m-s (a-constant 1_5))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-S)
        ((MD M-2) DPB M-1 (BYTE-FIELD 0 0) A-2) ;DPB in from higher word
        ((VMA-START-WRITE) ADD M-T A-D)         ;Put back into lower word
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-EQUAL M-2 A-ZERO BIDIVR-UNNORMALIZE-1)
        ((M-E) M-D)
BIDIVR-UNNORMALIZE-1
        (JUMP-LESS-THAN-XCT-NEXT M-D A-I BIDIVR-UNNORMALIZE-LOOP)
       ((M-D) ADD M-D (A-CONSTANT 1))
        (JUMP-XCT-NEXT BCLEANUP)
       ((M-C) SELECTIVE-DEPOSIT BIGNUM-HEADER-SIGN M-C A-D)

BIDIVR-3        ;;In this case (no shifting necessary) we must loop downward looking
        ;; for the first non-zero word.  Cannot share code with BIGNUM-DPB-CLEANUP.
        ((M-D) ADD M-I (A-CONSTANT 1))
        ((M-C) SELECTIVE-DEPOSIT BIGNUM-HEADER-SIGN M-C A-D)
        ((M-E) M-D)                             ;Counts down bignum
BIDIVR-4        ((VMA-START-READ) ADD M-T A-E)
        (CHECK-PAGE-READ)
        (JUMP-NOT-EQUAL MD A-ZERO BCLEANUP)
        (JUMP-GREATER-THAN-XCT-NEXT M-E (A-CONSTANT 2) BIDIVR-4)
       ((M-E) SUB M-E (A-CONSTANT 1))
        (JUMP BCLEANUP)                 ;Only one significant word

BIDIV-REMAINDER-COMMON
        ;;allocate a temporary bignum one word longer than first arg (put it in M-D)
        ;; If Bignum remainder got us here then this bignum will BE the answer
        (call-xct-next allocate-bignum-storage)
       ((m-b) add m-i (a-constant 2))
        ((M-TEM) ADD M-I (A-CONSTANT 1))
        ((MD) ADD M-TEM (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                          (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE M-D) Q-POINTER M-T
                               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
        (CHECK-PAGE-WRITE)
        ;;Now do a haulong on the high order word of second arg (for normalization)
        ;; note that if the answer is 31. then there is no need to normalize or allocate
        ;; a second temporary bignum
        ((VMA-START-READ) ADD M-R A-J)
        (CHECK-PAGE-READ)
        ((M-T) A-ZERO)
        (CALL-XCT-NEXT XHAUL1)
       ((M-1) MD)
        (JUMP-EQUAL-XCT-NEXT M-T (A-CONSTANT 31.) BIDIV-PUNT-NORMALIZING)
       ((M-1) M-T)              ;hide away haulong for later
        ;;allocate another temporary bignum as long as the second and keep it in M-T
        (call-xct-next allocate-bignum-storage)
       ((m-b) add m-j (a-constant 1))
        ((MD) ADD M-J (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE M-T) Q-POINTER M-T
                               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
        (CHECK-PAGE-WRITE-UNBOXED)
        ;;So now we build the proper constants from saved haulong in M-1
        ;; for ldbing (in M-K) and dpbing (in M-S) to normalize (see comment in front
        ;; of BIDIV-NORMALIZE)
        (CALL BIDIV-NORMALIZE-ENCODE-SHIFT)
        ;;Perform normalization (subroutine takes old bignum in M-B and new in M-D
        ;; steps length in M-ZR, bashes M-4)
        ((M-E) A-ZERO)          ;No offset for BIDIV-NORMALIZE.
        ((M-B) M-Q)
        ((M-ZR) M-I)
        (CALL-XCT-NEXT BIDIV-NORMALIZE)
       ((M-2) A-ZERO)
        ((M-Q) M-D)             ;Replace original dividend with copy
        ;;Prepare to call it again:
        ((VMA-START-READ) ADD M-R A-J)
        (CHECK-PAGE-READ)
        ((M-D) M-T)
        ((M-B) M-R)
#+exp   ((m-tem3) add m-s (a-constant 1_5))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-S)
        ((M-2) DPB MD (BYTE-FIELD 0 0) A-ZERO)
        (CALL-XCT-NEXT BIDIV-NORMALIZE)
       ((M-ZR) SUB M-J (A-CONSTANT 1))
        (JUMP-XCT-NEXT BIDIV-READY)
       ((M-R) M-D)              ;Replace original divisor with copy

BIDIV-PUNT-NORMALIZING
        ;;In this case all we do is copy the first arg:
        ((M-ZR) M-I)
BIDIV-PUNT-NORMALIZING-1
        ((VMA-START-READ) ADD M-Q A-ZR)
        (CHECK-PAGE-READ)
        ((WRITE-MEMORY-DATA) READ-MEMORY-DATA)
        ((VMA-START-WRITE) ADD M-D A-ZR)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-GREATER-THAN-XCT-NEXT M-ZR (A-CONSTANT 1) BIDIV-PUNT-NORMALIZING-1)
       ((M-ZR) SUB M-ZR (A-CONSTANT 1))
        ((MD) A-ZERO)
        ((VMA-START-WRITE) M+A+1 M-D A-I)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-Q) M-D)             ;Replace original dividend with copy
BIDIV-READY
        ((M-T) M-A)             ;Answer will wind up in M-T so why not now?
                                ;If remainder then this is a zero.
        ((M-K) SUB M-I A-J)
        ((M-K) ADD M-K (A-CONSTANT 1))
        ((C-PDL-BUFFER-POINTER-PUSH) M-1)               ;Saved haulong
        ;;So now the situation is as follows: The sign of the answer is in
        ;; BIGNUM-HEADER-SIGN in M-C. The old haulong of the top word of the second
        ;; argument is on top of the PDL (We have to save
        ;; that information so we know wether or not to un-cons!) We have a bignum
        ;; in M-Q,(M-I + 1) that we are dividing by a normalized bignum in M-R,M-J.
        ;; Answer is being built in M-T,M-K. (M-T = 0 if remaindering.)
        ((M-S) M-K)                     ;M-S will count down through the answer
        ((M-E) M-I)                     ;M-E will step down bignum in M-Q
        ((VMA-START-READ) ADD M-R A-J)
        (CHECK-PAGE-READ)
        ((A-BIDIV-V1) MD)                               ;V1
        ((VMA-START-READ) SUB VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((A-BIDIV-V2) MD)                               ;V2
BIDIV-LOOP
        ;;Now we are ready to make an estimate of what that first 31. bits will be.
        ;;Comments are notation from Knuth.
        ((VMA-START-READ) M+A+1 M-Q A-E)
        (CHECK-PAGE-READ)
        ((M-3) MD)                                      ;U0
        ((VMA-START-READ) ADD M-Q A-E)
        (CHECK-PAGE-READ)
        (JUMP-EQUAL M-3 A-BIDIV-V1 BIDIV-SIMPLE-CASE)
        ((M-TEM) DPB M-3 (BYTE-FIELD 1 31.) A-ZERO)
        ((Q-R) IOR MD A-TEM)                            ;low 32. bits of U0 * B + U1
        ((M-3) (BYTE-FIELD 30. 1) M-3 A-ZERO)           ;high 30. bits of same
        ((M-1) A-BIDIV-V1)                              ;Divide by V1
        ;; Compute QHAT = Floor((U0 * B + U1) / V1) and RHAT = U0 * B + U1 - QHAT * V1
        ((M-3) DIVIDE-FIRST-STEP M-3 A-1)
(REPEAT 31. ((M-3) DIVIDE-STEP M-3 A-1))
        ((M-3) DIVIDE-LAST-STEP M-3 A-1)
        ((M-3) DIVIDE-REMAINDER-CORRECTION-STEP M-3 A-1);RHAT
        (JUMP-XCT-NEXT BIDIV-OPTIMIZE-QHAT)
       ((M-1) Q-R)                                      ;QHAT

BIDIV-SIMPLE-CASE
        ((M-1) (A-CONSTANT 17777777777))                ;QHAT = B - 1
        ((M-3) ADD MD A-BIDIV-V1)                       ;RHAT = U1 + V1
        ;; If sign bit of M-3 is set then we know that RHAT * B + U2 is greater
        ;; than QHAT * V2:
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 31.) M-3 BIDIV-QHAT-IS-GOOD)
BIDIV-OPTIMIZE-QHAT
        ;;Now in order to check if RHAT * B + U2 < QHAT * V2 we first read in U2
        ;; and then compute QHAT * V2 .
        ((M-TEM) SUB M-E (A-CONSTANT 1))
        ((VMA-START-READ) ADD M-Q A-TEM)
        (CHECK-PAGE-READ)
        ((Q-R) A-BIDIV-V2)
        (CALL-XCT-NEXT MPY)
       ((M-4) MD)                                       ;U2
        ((M-2) M-2 OUTPUT-SELECTOR-LEFTSHIFT-1)         ;BRING IN HIGH BIT OF Q
        ;;Now M-2 = High(QHAT * V2)
        ;;    M-3 = RHAT = High(RHAT * B + U2)
        ;;    M-4 = U2 = Low(RHAT * B + U2)
        ;;    M-1 = QHAT
        ;;    Q-R = Low(QHAT * V2) plus junk in sign bit
        (JUMP-GREATER-THAN M-3 A-2 BIDIV-QHAT-IS-GOOD)
        ((M-TEM) (BYTE-FIELD 31. 0) Q-R A-ZERO)         ;Low(QHAT * V2)
        (JUMP-LESS-THAN M-3 A-2 BIDIV-OPTIMIZE-QHAT-SUB1)
        (JUMP-GREATER-OR-EQUAL M-4 A-TEM BIDIV-QHAT-IS-GOOD)
BIDIV-OPTIMIZE-QHAT-SUB1
        ;; So QHAT must be decremented and other quantities adjusted:
        ((M-TEM) SUB M-TEM A-BIDIV-V2)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 31.) M-TEM BIDIV-2);carry into High(QHAT * V2)
       ((M-3) ADD M-3 A-BIDIV-V1)                       ;Adjust RHAT
        ((M-TEM) (BYTE-FIELD 31. 0) M-TEM A-ZERO)
        ((M-2) SUB M-2 (A-CONSTANT 1))
BIDIV-2
        ;;If M-3 is negative then RHAT * B + U2 overflew and must be greater than
        ;; QHAT * V2
        (JUMP-IF-BIT-SET-XCT-NEXT (BYTE-FIELD 1 31.) M-3 BIDIV-QHAT-IS-GOOD)
       ((M-1) SUB M-1 (A-CONSTANT 1))                   ;Decrement QHAT
        (JUMP-GREATER-THAN M-3 A-2 BIDIV-QHAT-IS-GOOD)
        (JUMP-LESS-THAN M-3 A-2 BIDIV-OPTIMIZE-QHAT-SUB2)
        (JUMP-GREATER-OR-EQUAL M-4 A-TEM BIDIV-QHAT-IS-GOOD)
BIDIV-OPTIMIZE-QHAT-SUB2
        ((M-1) SUB M-1 (A-CONSTANT 1))                  ;Decrement QHAT second time.
BIDIV-QHAT-IS-GOOD
        ;;QHAT contains the wrong thing only once every 716 million times!
        ;;We multiply divisor by QHAT and subtract from dividend
        ((M-A) (A-CONSTANT 1))                  ;steps through M-R
        ((M-B) SUB M-E A-J)
        ((M-B) ADD M-B (A-CONSTANT 1))          ;steps through M-Q
        ((M-ZR) A-ZERO)                         ;borrow from last round
        ((M-2) A-ZERO)                          ;for multiplication scratch
BIDIV-MPY-LOOP
        ((VMA-START-READ) ADD M-R A-A)
        (CHECK-PAGE-READ)
        ((Q-R) MD)
(REPEAT 31. ((M-2) MULTIPLY-STEP M-2 A-1))
        ((M-D) (BYTE-FIELD 31. 1) Q-R A-ZERO)   ;Now M-D might contain gubbish.
        ((VMA-START-READ) ADD M-Q A-B)
        (CHECK-PAGE-READ)
        ((M-TEM) SUB MD A-D)
        ((M-TEM) SUB M-TEM A-ZR)
        ((MD) (BYTE-FIELD 31. 0) M-TEM A-ZERO)
        ((M-ZR) (BYTE-FIELD 1 31.) M-TEM A-ZERO)
        ((VMA-START-WRITE) ADD M-Q A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-A) ADD M-A (A-CONSTANT 1))
        (JUMP-LESS-THAN-XCT-NEXT M-B A-E BIDIV-MPY-LOOP)
       ((M-B) ADD M-B (A-CONSTANT 1))
        ((VMA-START-READ) ADD M-Q A-B)
        (CHECK-PAGE-READ)
        ((M-4) SUB MD A-2)
        ((MD-START-WRITE M-4) SUB M-4 A-ZR)
        (CHECK-PAGE-WRITE-UNBOXED)
        (CALL-IF-BIT-SET (BYTE-FIELD 1 31.) M-4 BIDIV-ONCE-IN-716MILLION) ;DAMN! QHAT too big.
        (JUMP-EQUAL-XCT-NEXT M-T A-ZERO BIDIV-DONT-STORE)       ;write QHAT into quotient
       ((M-E) SUB M-E (A-CONSTANT 1))                           ;If not remaindering
        ((MD) M-1)
        ((VMA-START-WRITE) ADD M-T A-S)
        (CHECK-PAGE-WRITE-UNBOXED)
BIDIV-DONT-STORE
        (JUMP-GREATER-THAN-XCT-NEXT M-S (A-CONSTANT 1) BIDIV-LOOP)
       ((M-S) SUB M-S (A-CONSTANT 1))
        ;;Now we have the answer so we give up any temp. storage and cleanup the answer.
        ((M-D) C-PDL-BUFFER-POINTER-POP)        ;Clears any gubbish from M-D
        ;;Now M-D contains the haulong of high word of original M-R
        (POPJ-EQUAL M-D (A-CONSTANT 31.))
        ((M-1) M-R)
        ((M-2) ADD M-J (A-CONSTANT 1))
        (JUMP-XCT-NEXT UN-CONS)                 ;Tail recursive call
       ((M-R) A-V-NIL)                          ;clear pointer to possible garbage

;;We come here in the case where QHAT was 1 too large, we must add divisor back into
;; dividend once.
BIDIV-ONCE-IN-716MILLION
        ((M-A) (A-CONSTANT 1))                  ;steps through M-R
        ((M-B) SUB M-E A-J)
        ((M-B) ADD M-B (A-CONSTANT 1))          ;steps through M-Q
        ((M-ZR) A-ZERO)                         ;carry
BIDIV-ONCE-IN-716MILLION-1
        ((VMA-START-READ) ADD M-R A-A)
        (CHECK-PAGE-READ)
        ((M-4) ADD MD A-ZR)
        ((VMA-START-READ) ADD M-Q A-B)
        (CHECK-PAGE-READ)
        ((M-4) ADD MD A-4)
        ((M-ZR) (BYTE-FIELD 1 31.) M-4 A-ZERO)
        ((MD-START-WRITE) (BYTE-FIELD 31. 0) M-4 A-ZERO)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-A) ADD M-A (A-CONSTANT 1))
        (JUMP-LESS-THAN-XCT-NEXT M-B A-E BIDIV-ONCE-IN-716MILLION-1)
       ((M-B) ADD M-B (A-CONSTANT 1))
        ((MD) A-ZERO)           ;Keep remainder correct.
        ((VMA-START-WRITE) ADD M-Q A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ-AFTER-NEXT (M-1) SUB M-1 (A-CONSTANT 1))  ;decrement QHAT
       (NO-OP)

;;; Set up args for the below from shift in M-1
BIDIV-NORMALIZE-ENCODE-SHIFT
        ((M-TEM) SUB (M-CONSTANT 32.) A-1)      ;MROT = 32. - Haulong
        ((M-K) SUB M-TEM (A-CONSTANT 2))        ;BYTL-1 = 30. - Haulong
        ((M-K) DPB M-K OAL-BYTL-1 A-TEM)        ;M-K constant for LDBing
        ((M-TEM) SUB M-TEM (A-CONSTANT 1))      ;MROT = 31. -Haulong
        (POPJ-AFTER-NEXT
         (M-S) SUB M-1 (A-CONSTANT 1))          ;BYTL-1 = Haulong - 1
       ((M-S) DPB M-S OAL-BYTL-1 A-TEM)         ;M-S constant for DPBing

;;; Subroutine for normalizing bignums:
;;; Does a left shift using M-K to ldb from C(M-B + M-ZR) into M-2 and stored at
;;; (M-D + A-ZR + 1 + M-E), and then dpb using M-S from C(M-B + M-ZR) into M-2 for the
;;; next time around:
;;;
;;;     |0| X |  Y  |   becomes:        |0|  Y  |   |  ;left in M-2 for next round.
;;;     |0|   |     |                   |0|     | X |  ;written out with high half
;;;                                                    ; from last round.
;;;
;;; M-K is the LDB pointer for X.  The thing initially in M-1 is the width of Y.
;;; M-S is the DPB pointer for Y.
;;;
;;; M-4 is bashed. M-2 can be loaded with whatever you want in the high part of the
;;; first word written (at M-D + M-ZR + 1 + M-E), you also load M-ZR
;;;Note that M-E is an offset in words to shift the bignum, that many words of zeros
;;; will be placed in the low bits of the bignum in M-D.
;;;This is crocked to work if M-E is 0, but not if it is negative!
;;; But -1 in M-E causes the bottom word (the final Y) of the bignum
;;; in M-D to disappear (for ASH).
BIDIV-NORMALIZE
        (JUMP-EQUAL M-ZR A-ZERO BIDIV-NORMALIZE-0)
        ((VMA-START-READ) ADD M-B A-ZR)
        (CHECK-PAGE-READ)
#+exp   ((m-tem3) add m-k (a-constant 1_5))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-K)
        ((M-4) (BYTE-FIELD 0 0) MD A-2)
#+exp   ((m-tem3) add m-s (a-constant 1_5))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-S)
        ((M-2) DPB MD (BYTE-FIELD 0 0) A-ZERO)
        ((MD) M-4)
        ((M-4) ADD M-ZR A-E)
        ((VMA-START-WRITE) M+A+1 M-D A-4)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-GREATER-THAN-XCT-NEXT M-ZR (A-CONSTANT 1) BIDIV-NORMALIZE)
       ((M-ZR) SUB M-ZR (A-CONSTANT 1))
BIDIV-NORMALIZE-0
        (POPJ-LESS-THAN M-E A-ZERO)
        ((MD) M-2)
        ((VMA-START-WRITE) M+A+1 M-D A-E)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ-EQUAL M-E A-ZERO)
        ((MD) A-ZERO)
BIDIV-NORMALIZE-1
        ((VMA-START-WRITE) ADD M-D A-E)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-GREATER-THAN-XCT-NEXT M-E (A-CONSTANT 1) BIDIV-NORMALIZE-1)
       ((M-E) SUB M-E (A-CONSTANT 1))
        (POPJ)

ARITH-FIX-BIG
        ((M-2) M-1)     ;UNPACKED FIXNUM ARG
        ((M-Q) M-T)     ;BIGNUM ITSELF (SECOND ARG)
        ((M-C) M-D)     ;BIGNUM HEADER
        (DISPATCH-XCT-NEXT (BYTE-FIELD 4 0) M-A D-FIXNUM-BIGNUM-OPS)
       ((M-I) M-J)      ;BIGNUM LENGTH

(LOCALITY D-MEM)
;FIXNUM IN BOTH M-1, M-2.  BIGNUM IN BOTH M-Q, M-T.  HEADER IN M-C, M-D.  LENGTH IN M-I, M-J.
(START-DISPATCH 4 0)
D-FIXNUM-BIGNUM-OPS
        (FXBADD)
        (FXBSUB)
        (FXBMPY)
        (FXBIDIV)
        (XFALSE)                        ;Fixnum = Bignum ???
        (FXBGRP)
        (FXBLSP)
        (FXBMIN)
        (FXBMAX)
        (FXBBOOLE)
        (FXBDIV)
        (XFALSE)                        ;Fixnum = Bignum ???
 (REPEAT NUM-UNUSED-ARITH-2ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

ARITH-BIG-FIX
        (DISPATCH-XCT-NEXT (BYTE-FIELD 4 0) M-A D-BIGNUM-FIXNUM-OPS)
       (NO-OP)

(LOCALITY D-MEM)
;FIXNUM IN M-T, UNPACKED INTO M-2.
;BIGNUM IN M-Q, HEADER IN M-C, LENGTH IN M-I.
(START-DISPATCH 4 0)
D-BIGNUM-FIXNUM-OPS
        (BFXADD)
        (BFXSUB)
        (BFXMPY)
        (BFXIDIV)
        (XFALSE)                        ;Bignum = Fixnum ???
        (BFXGRP)
        (BFXLSP)
        (BFXMIN)
        (BFXMAX)
        (BFXBOOLE)
        (BFXDIV)
        (XFALSE)                        ;Bignum = Fixnum ???
 (REPEAT NUM-UNUSED-ARITH-2ARGS (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

FXBSUB  (JUMP-XCT-NEXT FXBADD0)
       ((M-C) XOR M-C (A-CONSTANT (BYTE-VALUE BIGNUM-HEADER-SIGN 1)))

FXBRETQ (POPJ-AFTER-NEXT        ;RETURN BIGNUM ARG.
         (M-T) Q-TYPED-POINTER M-Q)
       ((C-PDL-BUFFER-POINTER-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE   ;LEAVE RESULT BOTH PLACES
                         (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))  ;FOR GOOD MEASURE.

BFXSUB  ((M-2) SUB M-ZERO A-2)  ;NO SETZ PROBLEMS!
BFXADD
FXBADD  (JUMP-EQUAL M-2 A-ZERO FXBRETQ)  ;SPECIAL CASE IF ADDING ZERO, JUST RETURN OTHER GUY
FXBADD0 (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-2 A-ZERO BFXADD-1)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
        ((M-2) SUB M-ZERO A-2)                  ;Make positive
        (JUMP-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BFXADD-ADD)
BFXADD-SUB      ;M-Q/M-I bignum, M-2 positive number to be subtracted
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-I (A-CONSTANT 1))
        ((M-D) M-I)
        ((M-B) (A-CONSTANT 1))
        (JUMP-XCT-NEXT BSUB-C)
       ((M-E) (A-CONSTANT 1))

BFXADD-1
        (JUMP-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BFXADD-SUB)
BFXADD-ADD      ;M-Q/M-I bignum, M-2 positive number to be added
        (CALL-XCT-NEXT BNCONS)                  ;ALLOCATE IN STRUCTURE EXTRA-PDL
       ((M-B) ADD M-I (A-CONSTANT 2))
        ((M-I) ADD M-I (A-CONSTANT 1))
        ((M-R) M-Q)
        ((M-J) SUB M-I (A-CONSTANT 1)) ;Recover length of bignum in M-Q (M-R)
        (JUMP-XCT-NEXT BADD5)
       ((M-D) (A-CONSTANT 1))

BIGNUM-ADD1
        (JUMP-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-C BFXADD-ADD)
       ((M-2) (A-CONSTANT 1))
        (JUMP BFXADD-SUB)

BIGNUM-SUB1
        (JUMP-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-C BFXADD-SUB)
       ((M-2) (A-CONSTANT 1))
        (JUMP BFXADD-ADD)

RETURN-ZERO
        (POPJ-AFTER-NEXT (M-T C-PDL-BUFFER-POINTER-PUSH)
                (A-CONSTANT (PLUS (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                  (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))
       (NO-OP)

;; A fixnum multiplied by a bignum can yield a fixnum in just two cases(!):
BFXMPY
FXBMPY  (JUMP-EQUAL M-2 A-ZERO RETURN-ZERO)     ;0*X=0
        (JUMP-NOT-EQUAL M-2 A-MINUS-ONE BFXMPY-OK)      ;(-1)*(+SETZ)=(-SETZ)
        (JUMP-NOT-EQUAL M-I (A-CONSTANT 1) BFXMPY-OK)
        ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        (JUMP-NOT-EQUAL MD (A-CONSTANT POSITIVE-SETZ) BFXMPY-OK)
        (POPJ-AFTER-NEXT (M-T C-PDL-BUFFER-POINTER-PUSH)
                Q-POINTER MD (A-CONSTANT (PLUS (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                               (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))
       (NO-OP)

BFXMPY-OK
        (JUMP-GREATER-OR-EQUAL M-2 A-ZERO BFXMPY-1)
        ((M-2) SUB M-ZERO A-2)                  ;NEGATIVE FIXNUM, CHANGE SIGN OF RESULT
        ((M-C) XOR M-C (A-CONSTANT (BYTE-VALUE BIGNUM-HEADER-SIGN 1)))
BFXMPY-1
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-I (A-CONSTANT 2))
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
                                         (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        (CALL-XCT-NEXT MULTIPLY-ONCE)
       ((M-1) A-ZERO)
        ((MD) M-1)
        ((VMA-START-WRITE) ADD M-T A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ-NOT-EQUAL M-1 A-ZERO)
        ((M-C) SUB M-C (A-CONSTANT 1))
        ((MD) ADD M-C (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE) M-T)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-1) ADD M-T A-D)
        (JUMP-XCT-NEXT UN-CONS)
       ((M-2) (A-CONSTANT 1))

;; MULTIPLY-ONCE multiplies a bignum in M-Q,M-I by a fixnum in M-2 and adds the fixnum in M-1.
;; Writes answer M-T (as if it is a bignum). Leaves last word (not written) in M-1.
;; Bashes M-D to be M-I + 1
MULTIPLY-ONCE
        ((M-D) (A-CONSTANT 1))
BFXMPY-LOOP
        ((VMA-START-READ) ADD M-Q A-D)
        (CHECK-PAGE-READ)
        ((Q-R) MD)
(REPEAT 31. ((M-1) MULTIPLY-STEP M-1 A-2))
        ((M-1) (BYTE-FIELD 31. 0) M-1)
        ((MD) (BYTE-FIELD 31. 1) Q-R)
        ((VMA-START-WRITE) ADD M-T A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-THAN-XCT-NEXT M-D A-I BFXMPY-LOOP)
       ((M-D) ADD M-D (A-CONSTANT 1))
        (POPJ)

;Bignum in M-Q divided by bignum in M-B to give rational, returned on stack.
BDIV
        ((PDL-PUSH) M-Q)
        ((PDL-PUSH) M-B)
        ((PDL-PUSH) M-Q)
        ((PDL-PUSH) M-B)
;This really should do the division once getting a quotient and remainder
;so it only has to be done once if the quotient comes out even.
        (CALL XGCD)             ;Get GCD into M-T.
        (JUMP NORMALIZED-RATIONAL-TO-STACK)

;Fixnum (unpacked in M-2) divided by bignum (in M-T and M-Q) to give a rational (on stack).
;The bignum's header is in M-C.
FXBDIV
        (JUMP-EQUAL M-2 A-ZERO QDIV-ZERO)
        ((M-1) M-2)
        (CALL FIXPACK-P)        ;Push packed fixnum, then push the bignum.
        ((PDL-PUSH) M-Q)
        ((VMA-START-READ M-B) M-Q)
        (CHECK-PAGE-READ)
        (CALL GCD-FIX-BIG)
        (JUMP NORMALIZED-RATIONAL-TO-STACK)

;Bignum (in M-Q) divided by fixnum (unpacked in M-2) to give rational (on stack).
BFXDIV
        ((M-T PDL-PUSH) Q-TYPED-POINTER M-Q);Must not push after popj'ing since next insn may pop.
        (POPJ-EQUAL M-2 (A-CONSTANT 1))
        ((M-1) M-2)
        (CALL FIXPACK-P)        ;Put fixnum on stack and in M-T.
        ((PDL-PUSH) M-Q)
        (CALL BFXDIV1)
        (PDL-POP)
;If remainder is 0, just return the quotient (which is in M-T).
        (JUMP-EQUAL M-3 A-ZERO BFXDIV-EVEN)
;Stack now has bignum, fixnum.  M-3 has remainder from division.
        ((M-1) M-3)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 PDL-TOP)
        (CALL GCD-FIX-FIX)
        (JUMP NORMALIZED-RATIONAL-TO-STACK)

BFXDIV-EVEN
        (POPJ-AFTER-NEXT PDL-POP)
       ((PDL-TOP) M-T)

;Make a rational number from two nonzero boxed args (num and denom) on the stack,
;given their gcd in M-T.  The value is returned on the stack.
NORMALIZED-RATIONAL-TO-STACK
        (JUMP-EQUAL-XCT-NEXT M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1))
                NORMALIZED-RATIONAL-ALREADY)
       ((M-A) PDL-POP)
        ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-A)
;Stack now has arg 1, gcd, arg 2.  M-T has gcd.
        (CALL QIDIV)
;Stack now has arg 1, gcd, final denominator.
        ((M-A) SETA PDL-POP A-T)        ;Pop stack, get value from M-T.
                                ;The same value is in both places, but pdl was just written.
        ((M-A) Q-TYPED-POINTER M-A)
        ((M-T) PDL-POP)
        ((M-1) PDL-POP)
        (JUMP-EQUAL M-A (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1))
                NORMALIZED-RATIONAL-IS-INTEGER)
        (JUMP-EQUAL M-A (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                          (BYTE-MASK BOXED-SIGN-BIT)
                                          (BYTE-MASK BOXED-NUM-EXCEPT-SIGN-BIT)))
                NORMALIZED-RATIONAL-IS-INTEGER-BUT-NEGATE)
        ((PDL-PUSH) M-A)
        ((PDL-PUSH) M-1)
;Stack now has final denominator, arg 1; M-T has gcd.
        (CALL QIDIV)
;Stack now has final denominator, final numerator.  Numerator also in M-T.
;If denominator is negative, change both signs.
NORMALIZED-RATIONAL-FIX-SIGNS
        ((M-J) SETA PDL-POP A-T)        ;Pop stack, but get from M-T
                                ;since the stack was possibly pushed on previous cycle.
        ((PDL-PUSH) PDL-TOP)
        (CALL XMINUSP)
        (JUMP-EQUAL M-T A-V-NIL NORMALIZED-RATIONAL-RIGHT-SIGNS)
        (CALL XMINUS)
        ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-J)
        (CALL XMINUS)
        ((M-J) M-T)
NORMALIZED-RATIONAL-RIGHT-SIGNS
        ((PDL-PUSH) M-J)
        (JUMP-XCT-NEXT MAKE-RATIONAL)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))

;Jump here if GCD is 1.  Numerator on stack, denom in M-A.
NORMALIZED-RATIONAL-ALREADY
        ((M-T) PDL-POP)
        ((PDL-PUSH) M-A)
        ((PDL-PUSH) M-T)
;Stack now has final denominator, final numerator.
        (JUMP NORMALIZED-RATIONAL-FIX-SIGNS)

;Jump here if GCD equals the denominator.
NORMALIZED-RATIONAL-IS-INTEGER
        ((PDL-PUSH) M-1)        ;Divide the 1st arg by the gcd
        (JUMP QIDIV)            ;and return, leaving that on the stack as the answer.

;Here if the GCD is minus the demoninator.
NORMALIZED-RATIONAL-IS-INTEGER-BUT-NEGATE
        (CALL-XCT-NEXT QIDIV)
       ((PDL-PUSH) M-1)
        (JUMP-XCT-NEXT XMINUS)
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))

;Bignum divided by a fixnum to give an integer quotient on the stack.
;A remainder, in unpacked form, is in M-K.
BFXIDIV
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-CPDL)))
BFXDIV1
        (CALL-EQUAL M-2 A-ZERO TRAP)
                (ERROR-TABLE DIVIDE-BY-ZERO)
                (ERROR-TABLE ARG-POPPED M-Q M-T)
        (JUMP-GREATER-THAN M-2 A-ZERO BFXIDIV-1)
        ((M-C) XOR M-C (A-CONSTANT (BYTE-VALUE BIGNUM-HEADER-SIGN 1)))  ;If fixnum is negative
        ((M-2) SUB M-ZERO A-2)  ;then change sign of both args.
BFXIDIV-1
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-I (A-CONSTANT 1))
        ((M-R) M-T)
        (CALL-XCT-NEXT DIVIDE-ONCE)     ;divide once stores into bignum in M-R,M-J
       ((M-J) M-I)
        ((M-D) M-I)             ;current length
        ((VMA-START-READ) ADD M-T A-D)  ;read last word to see if it is zeros
        (CHECK-PAGE-READ)
        ((M-3) M-1)             ;save remainder.  Can not do this in a typed register.
        (JUMP-NOT-EQUAL-XCT-NEXT MD A-ZERO BCLEANUP)    ;not zeros
       ((M-E) M-D)
        (JUMP-XCT-NEXT BCLEANUP)        ;zeros so length should be M-D - 1
       ((M-E) SUB M-E (A-CONSTANT 1))

;; DIVIDE-ONCE divides bignum in M-Q,M-I by positive(!) number in M-2.
;; bashes M-1 M-3 M-TEM M-D
;; answer is stored in M-R,M-J
;; remainder is left in M-1
DIVIDE-ONCE
        ((M-1) A-ZERO)
        ((M-3) (A-CONSTANT 1))
        ((M-D) M-I)
DIVIDE-ONCE-L
        ((VMA-START-READ) ADD M-Q A-D)
        (CHECK-PAGE-READ)
        ((M-TEM) DPB M-3 (BYTE-FIELD 30. 1) A-ZERO)
        ((M-TEM1) DPB MD (BYTE-FIELD 31. 1) A-3)
 #+lambda ((m-garbage) divide-first-step m-zero a-zero)  ;load hardware divisor sign register!
        ((Q-R) A-TEM1)
(REPEAT 31. ((M-1) DIVIDE-STEP M-1 A-2))
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-D A-J DIVIDE-ONCE-1)
       ((M-3) Q-R)                      ;Save Q-R which is bashed by page faults
        ((MD) (BYTE-FIELD 1 30.) Q-R A-TEM)
        ((VMA-START-WRITE) M+A+1 M-R A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
DIVIDE-ONCE-1
        (JUMP-GREATER-THAN-XCT-NEXT M-D (A-CONSTANT 1) DIVIDE-ONCE-L)
       ((M-D) SUB M-D (A-CONSTANT 1))
        ((Q-R) M-3)
        ((M-1) DIVIDE-LAST-STEP M-1 A-2)
        ((M-1) DIVIDE-REMAINDER-CORRECTION-STEP M-1 A-2)
        ((MD) (BYTE-FIELD 31. 0) Q-R)
        (POPJ-AFTER-NEXT (VMA-START-WRITE) ADD M-R (A-CONSTANT 1))
       (CHECK-PAGE-WRITE-UNBOXED)

;Fixnum divided by bignum is 0 except for -setz over +setz which is -1
FXBIDIV
        (POPJ-NOT-EQUAL-XCT-NEXT M-2 (A-CONSTANT NEGATIVE-SETZ))
       ((M-T C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-CDR-CODE CDR-NEXT)
                                                          (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))
        (POPJ-IF-BIT-SET BIGNUM-HEADER-SIGN M-C)
        (POPJ-NOT-EQUAL M-I (A-CONSTANT 1))
        ((VMA-START-READ) ADD M-Q (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL MD (A-CONSTANT POSITIVE-SETZ))
       ((M-T C-PDL-BUFFER-POINTER) DPB M-MINUS-ONE Q-POINTER A-T)

BFXGRP
FXBLSP
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET BIGNUM-HEADER-SIGN M-C)
       ((M-T) A-V-TRUE)

BFXLSP
FXBGRP
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET BIGNUM-HEADER-SIGN M-C)
       ((M-T) A-V-NIL)

BFXMIN
FXBMIN
        ((M-T) M-Q)                     ;neg. bignums are less than fixnums!
        (POPJ-AFTER-NEXT POPJ-IF-BIT-SET BIGNUM-HEADER-SIGN M-C)
       ((M-T) DPB M-2 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

BFXMAX
FXBMAX
        ((M-T) M-Q)                     ;positive bignums are greater than fixnums!
        (POPJ-AFTER-NEXT POPJ-IF-BIT-CLEAR BIGNUM-HEADER-SIGN M-C)
       ((M-T) DPB M-2 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))


;; First arg a bignum second a fixnum.  The bignum is expressed in the base of the fixnum
;; and stuffed in to an appropriate art-q array.
(MISC-INST-ENTRY BIGNUM-TO-ARRAY)
BIG-TO-ARY
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-Q) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
    (ERROR-TABLE RESTART BIG-TO-ARY)
        (CALL-DATA-TYPE-NOT-EQUAL M-Q
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)) TRAP)
    (ERROR-TABLE ARGTYP BIGNUM M-Q 0 BIG-TO-ARY)
    (ERROR-TABLE ARG-POPPED 0 M-Q M-A)
        (DISPATCH Q-DATA-TYPE M-A TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM M-A 1 BIG-TO-ARY)
    (ERROR-TABLE ARG-POPPED 0 M-Q M-A)
        ((VMA-START-READ) M-Q)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-TEM) HEADER-TYPE-FIELD MD)
        (CALL-NOT-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-BIGNUM)) TRAP)
    (ERROR-TABLE ARGTYP BIGNUM M-Q 0 BIG-TO-ARY)
    (ERROR-TABLE ARG-POPPED 0 M-Q M-A)
        ((M-Q) VMA)             ;get transported number address
        ((M-I) BIGNUM-HEADER-LENGTH MD)
        ((M-1) Q-POINTER M-A)
        (CALL-XCT-NEXT XHAUL1)  ;M-T gets number of bits in M-A LESS ONE(!)
       ((M-T) A-MINUS-ONE)
        ;; we must allocate an array at least 31*I/T long
        ;; 31*I = 32*I - I
        ((M-1) DPB M-I (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 5)) 5) A-ZERO)
        ((M-1) SUB M-1 A-I)
        (CALL-XCT-NEXT DIV)
       ((M-2) Q-POINTER M-T)
        (JUMP-EQUAL-XCT-NEXT M-1 A-ZERO BIG-TO-ARY-1)   ;If no remainder then we are o.k.
       ((M-C) Q-R)
        ((M-C) ADD M-C (A-CONSTANT 1))  ;with remainder then allocate 1 more
BIG-TO-ARY-1
;       (CALL-XCT-NEXT SCONS-D) ;Allocate space for art-q array
;      ((M-B) ADD M-C (A-CONSTANT 2))
     ;; Allocate (+ M-C 2) words of boxed structure storage.
        ((pdl-push) m-a)        ;Cons routines now clobber M-A.
        ((m-b) add m-c (a-constant 2))
        (call-xct-next allocate-structure-storage-default)
       ((m-a) m-b)
        ((m-a) pdl-pop)         ;Restore M-A.
        ((MD) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-HEADER)
                                (BYTE-VALUE %%ARRAY-NUMBER-DIMENSIONS 1)
                                (BYTE-VALUE %%ARRAY-LONG-LENGTH-FLAG 1)
                                (EVAL ART-Q))))
        ((VMA-START-WRITE M-R) Q-POINTER M-T
                               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER)))
        (CHECK-PAGE-WRITE)
        ((MD) Q-POINTER M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA-START-WRITE) ADD M-R (A-CONSTANT 1))
        (CHECK-PAGE-WRITE)
       ;; now we have the array in M-R with length in M-C
        ;; we must now allocate a bignum to divide into.
        (call-xct-next allocate-bignum-storage)
       ((m-b) add m-i (a-constant 1))
        ((MD) ADD M-I (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE M-T) Q-POINTER M-T
                               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
        (CHECK-PAGE-WRITE-UNBOXED)
        ;; now we have a (temp.) bignum in M-T so we start to divide
        ((M-E) M-R)             ;move the array into M-E
;       ((M-B) M-I)             ;save length of bignum
        ((M-2) Q-POINTER M-A)   ;fixnum to divide by
        ((M-A) (A-CONSTANT 1))  ;index+1 into array
        ((M-R) M-T)
        (CALL-XCT-NEXT DIVIDE-ONCE)
       ((M-J) M-I)
        ((M-Q) M-R)             ;from now on we divide from the temp bignum to itself.
BIG-TO-ARY-L
        ((MD) Q-POINTER M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA-START-WRITE) M+A+1 M-E A-A)       ;write remainder.
        (CHECK-PAGE-WRITE-UNBOXED)
        ((VMA-START-READ) ADD M-R A-J)  ;Check to see if last word of quotient was zero.
        (CHECK-PAGE-READ)
        (JUMP-NOT-EQUAL MD A-ZERO BIG-TO-ARY-2)
        (JUMP-EQUAL M-I (A-CONSTANT 1) BIG-TO-ARY-CLEANUP)      ;bignum is zero, done
        ((M-I) SUB M-I (A-CONSTANT 1))  ;pretend bignum is shorter
BIG-TO-ARY-2
        (CALL-XCT-NEXT DIVIDE-ONCE)
       ((M-J) M-I)
        (JUMP-XCT-NEXT BIG-TO-ARY-L)
       ((M-A) ADD M-A (A-CONSTANT 1))

BIG-TO-ARY-CLEANUP
        ((M-T) M-E)             ;array to return
;       ((M-1) M-R)
;       ((M-D) M-A)             ;M-A smashed by UN-CONS
;       (CALL-XCT-NEXT UN-CONS) ;Give back the bignum
;      ((M-2) ADD M-B (A-CONSTANT 1))
        (POPJ-EQUAL M-C A-a)    ;all array used so return it!
        ;; else give back unused end of array
        ((MD) Q-POINTER M-a (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA-START-WRITE M-1) ADD M-T (A-CONSTANT 1))
        (CHECK-PAGE-WRITE)
        ((M-1) M+A+1 M-1 A-a)
        (JUMP-XCT-NEXT UN-CONS) ;tail recursive call
       ((M-2) SUB M-C A-a)


;; First arg a art-q array second a fixnum third the sign bit (zero or one).
;; Returns a bignum. Inverse of BIGNUM-TO-ARRAY
(MISC-INST-ENTRY ARRAY-TO-BIGNUM)
(ERROR-TABLE DEFAULT-ARG-LOCATIONS ARRAY-TO-BIGNUM PP M-J M-C)
ARY-TO-BIG
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;sign bit.
        ((M-J) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;fixnum.
    (ERROR-TABLE RESTART ARY-TO-BIG)
        (DISPATCH Q-DATA-TYPE M-C TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM M-C 2 ARY-TO-BIG)
        (DISPATCH Q-DATA-TYPE M-J TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM M-J 1 ARY-TO-BIG)
        ((M-C) DPB M-C BIGNUM-HEADER-SIGN A-ZERO)
        ((M-J) Q-POINTER M-J)
        (CALL-XCT-NEXT GAHDR)                   ;array.
       ((m-array-pointer) invalidate-array-cache c-pdl-buffer-pointer-pop)
        (call store-array-registers-in-accumulators)
        (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) m-b trap)
    (ERROR-TABLE ARGTYP NON-DISPLACED-ARRAY PP 0)
        ((M-B) SELECTIVE-DEPOSIT M-b (LISP-BYTE %%ARRAY-TYPE-FIELD) A-ZERO)
        (CALL-NOT-EQUAL M-B (A-CONSTANT (EVAL ART-Q)) trap)
    (ERROR-TABLE ARGTYP ART-Q-ARRAY PP 0)
        (CALL-NOT-EQUAL M-D (A-CONSTANT 1) trap)
    (ERROR-TABLE ARRAY-NUMBER-DIMENSIONS M-D 1 PP)
        ;; now we have the array in M-A (origin in M-E, length in M-S)
        ;; sign bit in correct spot in M-C
        ;; fixnum in M-J (unboxed)
        ((M-T) A-ZERO)
        (CALL-XCT-NEXT XHAUL1)                  ;Get # bits per array element
       ((M-1) M-J)
        ((M-1) M-T)                             ;Size of bignum in bits
        (CALL-XCT-NEXT MPY)
       ((Q-R) m-s)
        (CALL-NOT-EQUAL M-2 A-ZERO trap)
                (ERROR-TABLE ARGTYP REASONABLE-SIZE-ARRAY M-A)
        ((M-1) Q-R)
        (CALL-XCT-NEXT DIV)                     ;Get size of bignum in words
       ((M-2) (A-CONSTANT 31.))
        ((M-I) ADD Q-R (A-CONSTANT 1))
        ;; we have now computed the amount of space to allocate for the bignum.
        ;; The formula is I := 1+(haulong J)*S/31.
        ((M-R) M-E)             ;shuffle (origin of array)
        ((M-D) M-S)             ;suuffle (length of array)
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-I (A-CONSTANT 1))
        ((M-S) M-D)             ;unshuffle (length of array)
        ((M-2) M-J)             ;"radix"
        ;; now we have the array in M-A (origin in M-R length in M-S),
        ;; we have the fixnum in M-2, we have the bignum in M-T (header in M-C, length in M-I)
        ;; first we must zero the bignum.
        ((M-D) (A-CONSTANT 1))
        ((MD) A-ZERO)
ARY-TO-BIG-2
        ((VMA-START-WRITE) ADD M-T A-D)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-THAN-XCT-NEXT M-D A-I ARY-TO-BIG-2)
       ((M-D) ADD M-D (A-CONSTANT 1))
        ((M-Q) M-T)             ;copy in M-Q
        ;; now we start:
        ((M-S) SUB M-S (A-CONSTANT 1))
ARY-TO-BIG-L
        ((VMA-START-READ) ADD M-R A-S)
        (CHECK-PAGE-READ)
        (CALL-XCT-NEXT MULTIPLY-ONCE)
       ((M-1) Q-POINTER MD)
        (CALL-NOT-EQUAL M-1 A-ZERO ILLOP)       ;overflow (should never happen)
        (JUMP-GREATER-THAN-XCT-NEXT M-S A-ZERO ARY-TO-BIG-L)
       ((M-S) SUB M-S (A-CONSTANT 1))
        ;; now we see how many zeros we have at the end
BIGNUM-DPB-CLEANUP      ;Enters here with bignum in M-T, header in M-C
                        ;Use this only for logical operations, not arithmetic
                        ; ones!  Note the treatment of negative zero!
        ((M-E) BIGNUM-HEADER-LENGTH M-C)
        ((M-D) BIGNUM-HEADER-LENGTH M-C)
ARY-TO-BIG-CLEANUP
        ((VMA-START-READ) ADD M-T A-E)
        (CHECK-PAGE-READ)
        (JUMP-NOT-EQUAL MD A-ZERO BCLEANUP)
        (JUMP-GREATER-THAN-XCT-NEXT M-E (A-CONSTANT 1) ARY-TO-BIG-CLEANUP)
       ((M-E) SUB M-E (A-CONSTANT 1))
        ;; Number is nothing but sign bits
#+lambda((OA-REG-HIGH) BIGNUM-HEADER-SIGN M-C)
#+exp   ((m-tem3) bignum-header-sign m-c)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-TEM) M-ZERO)
        ((M-1) Q-POINTER M-T)                           ;For UN-CONS
        (JUMP-XCT-NEXT BCLEANUP-1)
       ((M-2) ADD M-D (A-CONSTANT 1))

;; Clean up and return a bignum in M-T. Hands back storage and checks for fixnums.
;; Bignum in M-T, header in M-C, length in M-D, actual length in M-E (# of non-zero words).
BCLEANUP        ;must not clobber M-3
        (JUMP-GREATER-THAN M-E (A-CONSTANT 1) BCLEANUP-X)  ;Could answer be a fixnum?
        ((VMA-START-READ) ADD M-T (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-1) Q-POINTER M-T)                           ;For UN-CONS
        ((M-2) ADD M-D (A-CONSTANT 1))
        ((M-A)
         (BYTE-FIELD (DIFFERENCE 33. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 1))
         MD)                    ;All but 23 low bits
        (JUMP-NOT-EQUAL-XCT-NEXT M-A A-ZERO BCLEANUP-SETZP)     ;no. (unless it is SETZ)
       ((M-TEM) MD)
        (JUMP-IF-BIT-CLEAR BIGNUM-HEADER-SIGN M-C BCLEANUP-1)
        ((M-TEM) SUB M-ZERO A-TEM)                              ;Its negative.
BCLEANUP-1
        (JUMP-XCT-NEXT UN-CONS)
       ((M-T) Q-POINTER M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

BCLEANUP-SETZP
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT POSITIVE-SETZ) BCLEANUP-X)    ;Is it setz?
        (JUMP-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BCLEANUP-1)
BCLEANUP-X
        (POPJ-EQUAL M-D A-E)
        ((M-2) SUB M-D A-E)                     ;Number of unused words at end
        ((M-C) SUB M-C A-2)                     ;Fix the header
        ((MD) ADD M-C (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE M-1) Q-POINTER M-T)
        (CHECK-PAGE-WRITE)
        ((M-1) M+A+1 M-1 A-E)
        (JUMP UN-CONS)
))

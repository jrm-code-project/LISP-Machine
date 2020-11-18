;-*-Mode:Midas; base: 8; readtable: ZL-*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

(DEFCONST UC-LOGICAL '(
; SHIFTING WITH CONS ...
;   THE CONS HARDWARE TAKES THE OPPOSITE APPROACH FROM MOST MACHINES IN THAT
; LDB AND DPB ARE PRIMITIVE AND SHIFTS HAVE TO BE BUILT UP OUT OF THEM INSTEAD
; OF THE OTHER WAY AROUND.  FOR THE PURPOSES OF CONS, THIS IS USUALLY A GREAT
; WIN, BUT IT DOES MAKE FOR A CERTAIN AMOUNT OF PAIN WHEN REALLY TRYING TO DO A SHIFT.
;   FURTHER PAIN IS CAUSED WHEN THE AMOUNT OF THE SHIFT MUST COME FROM THE
; "DATA" SIDE OF THE MACHINE (AS WITH ROT AND LSH) INSTEAD OF BEING A CONSTANT AMOUNT
; KNOWN AT MICRO-ASSEMBLY TIME.  WHEN THIS IS THE CASE,
;  (1) THE ARGUMENT MUST BE "MOVED" FROM THE DATA SIDE TO THE CONTROL SIDE BY THE USE
;       OF OA- TYPE DESTINATIONS.  AS A COLLARY OF THIS, IT IS NECESSARY TO MASK THINGS
;       CAREFULLY TO AVOID RANDOMNESS, AND THERE IS NOT MUCH FLEXIBILITY AS TO
;       WHAT SIGNS THINGS HAVE ETC.  VARIOUS "QUIRKS" OF THE HARDWARE, NORMALLY
;       COMPENSATED FOR BY THE MICRO-ASSEMBLER, MUST BE DELT WITH BY THE USER:
;       (A) THE BYTE LENGTH FIELD IS REALLY THE <BYTE-LENGTH-1> FIELD.
;           ALSO, BECAUSE THE FIELD IS 5 BITS LONG, ZERO BIT BYTES DONT WIN
;           AT ALL (THEY "BECOME" 32. BYTES).
;       (B) ONE MUST REMEMBER THE M-ROTATE IS ALWAYS A LEFT ROTATE.  THIS IS
;           "NATURAL" FOR DPB, BUT ON LDB THE MICROASSEMBLER NORMALLY HAS TO
;           BUGGER THINGS TO COMPENSATE AND MAKE IT APPEAR A RIGHT SHIFT IS
;           BEING DONE.  NATURALLY, USING OA- MODIFIERS, THERE IS NO OPPORTUNITY
;           FOR THIS TO HAPPEN WITHOUT BEING EXPLICITLY CODED IN MICRO-INSTRUCTIONS.
;           THE BUGGER REQUIRED IS TO "REPLACE" THE M-ROTATE FIELD WITH
;             (LOGAND 37 (- 40 <"NATURAL" M-ROTATE>)).
;  (2) THE POSSIBITY OF CONSTRUCTING AN "ILLEGAL" BYTE POINTER ON A DPB MUST BE
;       CAREFULLY CONSIDERED.  BRIEFLY, THE SUM <BYTE-LENGTH-1> + <M-ROTATE> MUST BE
;       LESS THAN OR EQUAL TO 37 OCTAL.  IF IT IS GREATER, THE HARDWARE WILL
;       PRODUCE AN ALL-ZERO ANSWER (ACTUALLY, IT IS COMPLETELY IDENTICALLY EQUAL TO THE
;       A-SOURCE).  HERE A LDB DOESNT GIVE SO MUCH PROBLEM SINCE
;       THE HARDWARE JUST ROTATES THE INDICATED AMOUNT AND TAKES THE LOW N BITS.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS LSH PP M-K)

XLSH  (MISC-INST-ENTRY LSH)
                (ERROR-TABLE RESTART XLSH)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                  Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
   (ERROR-TABLE ARGTYP FIXNUM PP 1 XLSH)
   (ERROR-TABLE ARG-POPPED 0 PP PP)
        ((M-K) Q-POINTER C-PDL-BUFFER-POINTER-POP)  ;ARG2, AMT TO SHIFT
                (ERROR-TABLE RESTART XLSH0)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                  Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
   (ERROR-TABLE ARGTYP FIXNUM PP 0 XLSH0)
        (JUMP-IF-BIT-SET BOXED-SIGN-BIT M-K XLSH1)  ;SHIFT TO RIGHT
LSH-LEFT
        ((M-1) SUB (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-K)     ;COMPUTE BYTE LENGTH <24.-SHIFT-1>
        (JUMP-LESS-THAN M-1 A-ZERO XLSH-ZERO)
#+exp   ((m-tem3) add m-1 (a-constant 1))
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-1 #+exp m-tem3 OAL-BYTL-1 A-K)
       ((M-T) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 0 0)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XLSH1   (JUMP-LESS-THAN M-K
         (A-CONSTANT (DIFFERENCE (EVAL (ASH 1 %%Q-POINTER)) (DIFFERENCE Q-POINTER-WIDTH 1)))
         XLSH-ZERO) ;SHIFT RIGHT
        ((M-TEM1) ADD M-K (A-CONSTANT (PLUS (BYTE-MASK BITS-ABOVE-FIXNUM)  ;TO SIGN EXTEND
                                         40)))  ; TO 32. COMPUTE 40-N .
        ((M-1) ADD M-K (A-CONSTANT (PLUS (BYTE-MASK BITS-ABOVE-FIXNUM)
                                         (DIFFERENCE Q-POINTER-WIDTH 1))))      ;COMPUTE 24.-N-1
#+exp   ((m-tem3) add m-1 (a-constant 1))
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-1 #+exp m-tem3 OAL-BYTL-1 A-TEM1)
       ((M-T) BYTE-INST (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                        C-PDL-BUFFER-POINTER-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS ROT PP M-K)

XROT  (MISC-INST-ENTRY ROT)
                (ERROR-TABLE RESTART XROT)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                  Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
   (ERROR-TABLE ARGTYP FIXNUM PP 1 XROT)
   (ERROR-TABLE ARG-POPPED 0 PP PP)
        ((M-K) Q-POINTER C-PDL-BUFFER-POINTER-POP)  ;ARG2, AMT TO ROT
                (ERROR-TABLE RESTART XROT0)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                  Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
   (ERROR-TABLE ARGTYP FIXNUM PP 0 XROT0)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)  ;ARG1, DATA TO ROT
XROT3 ; *** THIS SHOULD PROBABLY LET YOU INTERRUPT AND SEQUENCE-BREAK OUT ***
        (JUMP-IF-BIT-SET BOXED-SIGN-BIT M-K XROT1)  ;ROT TO RIGHT
        (POPJ-EQUAL M-K A-ZERO)                 ;NO CHANGE (AVOID BYTL-1 LOSS)
        (JUMP-GREATER-OR-EQUAL M-K (A-CONSTANT Q-POINTER-WIDTH) XROT2)
;GENERAL IDEA: (1) SHIFT A 24.-N BIT PIECE N PLACES LEFT
;                   (ACTUALLY, A TRUE SHIFT OF A UNMASKED 32 BIT PIECE WOULD DO.
;                    ON THE OTHER HAND, WE HAVE THE UNSAFE BYTE POINTER PROBLEM.)
;              (2) LDB A N BIT PIECE FROM 24-N BITS OVER
;              (3) IOR THE TWO.
XROT3A  ;REALLY DO THE WORK. BY NOW, 0 < M-K < 24.
                                                ;DO LSH OF STEP ONE
        ((M-1) SUB (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-K)     ;COMPUTE BYTE LENGTH
;       (JUMP-LESS-THAN M-1 A-ZERO XLSH-ZERO)   ;CANT BE
#+exp   ((m-tem) add m-1 (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-1 #+exp m-tem OAL-BYTL-1 A-K)
        ((M-TEM3) DPB M-T (BYTE-FIELD 0 0) A-ZERO)      ;PART 1 DONE
        ((M-TEM2) ADD M-K (A-CONSTANT (DIFFERENCE 32. Q-POINTER-WIDTH)))        ; 40-<24.-N>
        ((M-ZR) SUB M-K (A-CONSTANT 1))         ;BYTE LENGTH MINUS ONE
#+exp   ((m-tem) add m-zr (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-ZR #+exp m-tem OAL-BYTL-1 A-TEM2)
        (POPJ-AFTER-NEXT                        ;PART 2 DONE
          (M-T) BYTE-INST M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((M-T) IOR M-T A-TEM3)                   ;PART 3

XROT2   (JUMP-XCT-NEXT XROT3)                   ;LOOP UNTIL RESULT AFTER
       ((M-K) SUB M-K (A-CONSTANT Q-POINTER-WIDTH))             ;SUBTRACTION IS LESS THAN 24.

;ROTATE TO RIGHT.  CONVERT TO EQUIVALENT LEFT ROTATE (24.- <-N>)
XROT1   ((M-K) SELECTIVE-DEPOSIT M-K Q-POINTER (A-CONSTANT -1)) ;EXTEND SIGN
        (JUMP-XCT-NEXT XROT3)
       ((M-K) ADD M-K (A-CONSTANT Q-POINTER-WIDTH))


;;; Boolean operations

QIAND
        (jump-data-type-not-equal c-pdl-buffer-pointer a-t qiand-hard)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 and c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))
qiand-hard
        (JUMP-XCT-NEXT M-T-TO-CPDL)
       (CALL QIAND0)

XMAND (MISC-INST-ENTRY M-LOGAND)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Convert to Instruction calling seq
XTCAND                                          ;MC-LINKAGE
QIAND0  ((M-S) (A-CONSTANT (OA-LOW-CONTEXT (AND)))) ;An extra instruction, but saves hair
    (ERROR-TABLE RESTART QIAND0)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) PP 0 QIAND0)
    (ERROR-TABLE ARG-POPPED 0 PP M-1)
       ((M-A) (A-CONSTANT ARITH-2ARG-BOOLE))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
    (ERROR-TABLE RESTART QIAND1)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) M-T 1 QIAND1)
    (ERROR-TABLE ARG-POPPED 0 M-T M-1)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (POPJ-AFTER-NEXT (M-1) AND M-2 A-1)
       ((M-T) Q-POINTER M-1 (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))

QIIOR
        (jump-data-type-not-equal c-pdl-buffer-pointer a-t qiior-hard)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 ior c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))
qiior-hard
        (JUMP-XCT-NEXT M-T-TO-CPDL)
       (CALL QIIOR0)

XMIOR (MISC-INST-ENTRY M-LOGIOR)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Convert to Instruction calling seq
XTCIOR                                          ;MC-LINKAGE
QIIOR0  ((M-S) (A-CONSTANT (OA-LOW-CONTEXT (IOR)))) ;An extra instruction, but saves hair
                (ERROR-TABLE RESTART QIIOR0)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) PP 0 QIIOR0)
    (ERROR-TABLE ARG-POPPED 0 PP M-1)
       ((M-A) (A-CONSTANT ARITH-2ARG-BOOLE))
                (ERROR-TABLE RESTART QIIOR1)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) M-T 1 QIIOR1)
    (ERROR-TABLE ARG-POPPED 0 M-T M-1)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (POPJ-AFTER-NEXT (M-1) IOR M-2 A-1)
       ((M-T) Q-POINTER M-1 (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))

QIXOR
        (jump-data-type-not-equal c-pdl-buffer-pointer a-t qixor-hard)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 xor c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))
qixor-hard
        (JUMP-XCT-NEXT M-T-TO-CPDL)
       (CALL QIXOR0)

XMXOR (MISC-INST-ENTRY M-LOGXOR)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Convert to Instruction calling seq
XTCXOR                                          ;MC-LINKAGE
QIXOR0  ((M-S) (A-CONSTANT (OA-LOW-CONTEXT (XOR)))) ;An extra instruction, but saves hair
                (ERROR-TABLE RESTART QIXOR0)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) PP 0 QIXOR0)
    (ERROR-TABLE ARG-POPPED 0 PP M-1)
       ((M-A) (A-CONSTANT ARITH-2ARG-BOOLE))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
                (ERROR-TABLE RESTART QIXOR1)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) M-T 1 QIXOR1)
    (ERROR-TABLE ARG-POPPED 0 M-T M-1)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        (POPJ-AFTER-NEXT (M-1) XOR M-2 A-1)
       ((M-T) Q-POINTER M-1 (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))

;The 2nd arg of BOOLE becomes the A operand of the logical instruction.
;The 3rd arg becomes the M operand.
XBOOLE (MISC-INST-ENTRY *BOOLE)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Arg 3
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Arg 2
                (ERROR-TABLE RESTART XBOOLE)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0 XBOOLE)
    (ERROR-TABLE ARG-POPPED 0 PP M-A M-T)
        ((M-B) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((C-PDL-BUFFER-POINTER-PUSH) M-A)       ;Put arg 2 back in standard place
        ((M-S) DPB M-B OAL-ALUF)        ;Arg 1 as OA-REG-LOW alu function
    (ERROR-TABLE RESTART XBOOLE1)
XBOOLE0 (DISPATCH-XCT-NEXT Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG1)
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) PP 1 XBOOLE1)
    (ERROR-TABLE ARG-POPPED 0 PP M-1)
       ((M-A) (A-CONSTANT ARITH-2ARG-BOOLE))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
    (ERROR-TABLE RESTART XBOOLE2)
        (DISPATCH Q-DATA-TYPE M-T D-FIXNUM-NUMARG2 (I-ARG NUMBER-CODE-FIXNUM))
    (ERROR-TABLE ARGTYP (FIXNUM BIGNUM) M-T 2 XBOOLE2)
    (ERROR-TABLE ARG-POPPED 0 M-T M-1)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((OA-REG-LOW) M-S) ;ALU
        (POPJ-AFTER-NEXT (M-1) SETZ M-2 A-1)
       ((M-T) Q-POINTER M-1 (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX))))

;Boolean function of two bignums, M-S has OA-REG-LOW to do the function.
;First arg in M-Q,M-C,M-I.  Second arg in M-B/M-T,M-D,M-J.
;Eventual ACs:  small arg in M-R,M-J.  big arg in M-Q,M-I.  alu func in M-A
; result in M-T,M-C.  M-D has bit flags:
;       bit 0 - sign of smaller arg
;       bit 1 - sign of bigger arg
;This hair is required because bignums are sign-and-magnitude,
;but BOOLE wants to treat them as 2's complement.

BBOOLE  ((M-TEM) BIGNUM-HEADER-SIGN M-C)        ;Sign of 1st arg
        ((M-A OA-REG-LOW) M-S)                  ;save alu func, compute sign of result
        ((M-C) SETZ M-D A-C)                    ; in BIGNUM-HEADER-SIGN bit of M-C
        ((M-D) BIGNUM-HEADER-SIGN M-D)          ;bit 0 of M-D gets sign of 2nd arg
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-I A-J BBOOL0) ;Make M-Q,M-I the longer
       ((M-D) DPB M-TEM (BYTE-FIELD 1 1) A-D)   ;bit 1 of M-D gets sign of 1st arg
        (DISPATCH (BYTE-FIELD 2 0) M-D D-BOOLE-REV)     ;Interchange bits 0 and 1 of M-D
         ((M-D) XOR M-D (A-CONSTANT 3))
        ((M-T) M-Q)
        ((M-TEM) M-J)
        ((M-J) M-I)
        ((M-Q) M-B)
        (JUMP-XCT-NEXT BBOOL1)
       ((M-I) M-TEM)

;If we didn't interchange the args, interchange bits 4 and 5
;of the ALU function so as to make the first argument be on the M side.
BBOOL0  (DISPATCH (BYTE-FIELD 2 4) M-A D-BOOLE-REV)
         ((M-A) XOR M-A (A-CONSTANT 60))        ;Swap bits if different
BBOOL1  ((M-R) M-T)                             ;Small arg in M-R,M-J, big in M-Q,M-I
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-I (A-CONSTANT 2))           ;Allocate result 1 longer than bigger arg
                                                ; due to the damned SETZ case
        ((M-B) (A-CONSTANT 1))                  ;Index
        ((A-BOOLE-CARRY-1) M-ZERO)
        ((A-BOOLE-CARRY-2) M-ZERO)
BBOOL2  ((VMA-START-READ) ADD M-R A-B)          ;Loop over length of smaller arg
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 0) M-D BBOL2A)
       ((M-1) ADD READ-MEMORY-DATA A-BOOLE-CARRY-1)
        ((M-1) SUB M-ZERO A-1)                  ;Smaller arg negative, get 2's comp form
        ((A-BOOLE-CARRY-1) (BYTE-FIELD 1 31.) M-1)
BBOL2A  ((VMA-START-READ) ADD M-Q A-B)
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 1) M-D BBOL2B)
       ((M-2) ADD READ-MEMORY-DATA A-BOOLE-CARRY-2)
        ((M-2) SUB M-ZERO A-2)                  ;Larger arg negative, get 2's comp form
        ((A-BOOLE-CARRY-2) (BYTE-FIELD 1 31.) M-2)
BBOL2B  ((OA-REG-LOW) M-A) ;ALU
        ((WRITE-MEMORY-DATA) SETZ M-2 A-1)
        ((WRITE-MEMORY-DATA) (BYTE-FIELD 31. 0) WRITE-MEMORY-DATA)
        ((VMA-START-WRITE) ADD M-T A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-THAN-XCT-NEXT M-B A-J BBOOL2)
       ((M-B) ADD M-B (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-B A-I BBOOL5)
       ((M-1) SUB M-ZERO A-BOOLE-CARRY-1)       ;Sign bits for smaller arg
BBOOL3  ((VMA-START-READ) ADD M-Q A-B)          ;Do bigger arg against sign of smaller
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 1) M-D BBOL3B)
       ((M-2) ADD READ-MEMORY-DATA A-BOOLE-CARRY-2)
        ((M-2) SUB M-ZERO A-2)                  ;Larger arg negative, get 2's comp form
        ((A-BOOLE-CARRY-2) (BYTE-FIELD 1 31.) M-2)
BBOL3B  ((OA-REG-LOW) M-A) ;ALU
        ((WRITE-MEMORY-DATA) SETZ M-2 A-1)
        ((WRITE-MEMORY-DATA) (BYTE-FIELD 31. 0) WRITE-MEMORY-DATA)
        ((VMA-START-WRITE) ADD M-T A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-THAN-XCT-NEXT M-B A-I BBOOL3)
       ((M-B) ADD M-B (A-CONSTANT 1))
BBOOL5  ((M-2) SUB M-ZERO A-BOOLE-CARRY-2)      ;Sign bits for larger arg
        ((OA-REG-LOW) M-A) ;ALU                 ;High result word comes from sign bits
        ((WRITE-MEMORY-DATA) SETZ M-2 A-1)
        ((WRITE-MEMORY-DATA) (BYTE-FIELD 31. 0) WRITE-MEMORY-DATA)
        ((VMA-START-WRITE) ADD M-T A-B)
        (CHECK-PAGE-WRITE-UNBOXED)
        ((M-I) ADD M-I (A-CONSTANT 1))          ;Actual length of result (for BIGNEG)
        (CALL-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BIGNEG) ;Magnitude of negative result
        (JUMP BIGNUM-DPB-CLEANUP)               ;Dispose of any unnecessary high words

(LOCALITY D-MEM)
(START-DISPATCH 2 (PLUS P-BIT R-BIT)) ;Skip if bits the same
D-BOOLE-REV
        (INHIBIT-XCT-NEXT-BIT)  ;Bits same, no need to swap
        (0)                     ;Bits different, swap by XOR'ing
        (0)                     ;different
        (INHIBIT-XCT-NEXT-BIT)  ;same
(END-DISPATCH)
(LOCALITY I-MEM)

;Mixed-mode cases...
;Bignum arg in M-Q,M-C,M-I.  Fixnum unpacked in M-2.  ALU function in M-S.
;The first arg goes on the A side, and we want the fixnum on the A side.
BFXBOOLE        ;Fixnum second arg, take as first by switching ALU function
        (DISPATCH (BYTE-FIELD 2 4) M-S D-BOOLE-REV)
         ((M-S) XOR M-S (A-CONSTANT 60))
FXBBOOLE
        ((M-A) M-S)                             ;Stash function in M-A
        ((M-D) BIGNUM-HEADER-SIGN M-C)          ;M-D bit 1 gets sign of bigger arg
        ((M-D) DPB M-D (BYTE-FIELD 1 1))
#+lambda((OA-REG-HIGH) (BYTE-FIELD 1 31.) M-2)  ;Get sign bits for smaller arg
#+exp   ((m-1) (byte-field 1 31.) m-2)
#+exp   ((oa-reg-high) dpb m-1 oah-m-src a-zero)
        ((M-1) M-ZERO)
        ((OA-REG-LOW) M-A) ;ALU                 ;Compute sign of result
        ((M-C) SETZ M-C A-1)
        (CALL-XCT-NEXT BNCONS)
       ((M-B) ADD M-I (A-CONSTANT 2))           ;Allocate result one longer than bignum arg
                                                ; due to the damned SETZ case
        ((A-BOOLE-CARRY-2) M-ZERO)
        ((VMA-START-READ) ADD M-Q (A-CONSTANT 1))       ;Combine low word with fixnum arg
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 1) M-D BBOLFX)
       ((M-TEM) ADD READ-MEMORY-DATA A-BOOLE-CARRY-2)
        ((M-TEM) SUB M-ZERO A-TEM)              ;Larger arg negative, get 2's comp form
        ((A-BOOLE-CARRY-2) (BYTE-FIELD 1 31.) M-TEM)
BBOLFX  ((OA-REG-LOW) M-A) ;ALU
        ((WRITE-MEMORY-DATA) SETZ M-TEM A-2)
        ((WRITE-MEMORY-DATA) (BYTE-FIELD 31. 0) WRITE-MEMORY-DATA)
        ((VMA-START-WRITE) ADD M-T (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-I (A-CONSTANT 2) BBOOL3)      ;do more of bignum arg
       ((M-B) (A-CONSTANT 2))
        (JUMP BBOOL5)                           ;bignum arg only 1 word long

;;; Arithmetic shift.  Unlike LSH, ASH works on bignums

XASH (MISC-INST-ENTRY ASH)
                (ERROR-TABLE RESTART XASH)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)  ;arg 2, shift count
    (ERROR-TABLE ARGTYP FIXNUM PP 1 XASH)
    (ERROR-TABLE ARG-POPPED 0 PP PP)
        ((m-zr) q-data-type c-pdl-buffer-pointer)       ;its theoritically possible for this value
                        ;to get to xdpb1 and determine whether we return dtp-fix or dtp-character.
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)  ;M-2 gets arg 2
                (ERROR-TABLE RESTART XASH1)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER D-NUMARG)    ;arg 1, number to shift
    (ERROR-TABLE ARGTYP NUMBER PP 0 XASH1)
    (ERROR-TABLE ARG-POPPED 0 PP M-1)
       ((M-A) (A-CONSTANT ARITH-1ARG-ASH))
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ;Fixnum case
#+lambda((OA-REG-HIGH) (BYTE-FIELD 1 31.) M-1)  ;M-3 gets sign extension of M-1
#+exp   ((m-3) ldb (byte-field 1 31.) m-1)
#+exp   ((oa-reg-high) dpb m-3 oah-m-src a-zero)
        ((M-3) M-ZERO)
        (JUMP-GREATER-THAN M-2 A-ZERO XASH2)    ;Jump if left shift
        ((M-2) ADD M-2 (A-CONSTANT 40))         ;Number of bits preserved by right shift
        (JUMP-GREATER-THAN M-2 A-ZERO XASH1)
        ((M-2) (A-CONSTANT 1))                  ;Shifting too far, preserve only sign
XASH1   ((M-4) SUB M-2 (A-CONSTANT 1))          ;Byte size -1
#+exp   ((m-tem3) add m-4 (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-4 #+exp m-tem3 OAL-BYTL-1 A-2)     ;Use byte hardware
        ((M-1) (BYTE-FIELD 0 0) M-1 A-3)        ;Do the right arithmetic shift
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;Left ASH of a fixnum turns into DPB.
XASH2   ((C-PDL-BUFFER-POINTER-PUSH M-4)        ;Put arg 1 back on pdl
                Q-POINTER M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-1) SELECTIVE-DEPOSIT M-3
         (BYTE-FIELD (DIFFERENCE 33. Q-POINTER-WIDTH)
                     (DIFFERENCE Q-POINTER-WIDTH 1))
         A-ZERO)        ;background to DPB into (signs)
        ((M-K) (A-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)))             ;Byte size
        ;Jump if fit in machine word after shift,
        (JUMP-LESS-THAN-XCT-NEXT M-2 (A-CONSTANT (DIFFERENCE 32. Q-POINTER-WIDTH)) ASHDPB)
       ((M-E) M-2)                              ;Byte position
        ((M-1) M-ZERO)                          ;Bignum, DPB into background of zero
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-3 A-ZERO ASHDPB1) ;if positive
       ((M-C) DPB M-3 BIGNUM-HEADER-SIGN A-ZERO)
        (JUMP-NOT-EQUAL-XCT-NEXT M-4            ;If negative, do magic things that work
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) POSITIVE-SETZ))
                XASH3)
       ((M-4) SUB M-ZERO A-4)                   ;Make it positive
        ((M-4) DPB (M-CONSTANT -1)
         (BYTE-FIELD 1 (DIFFERENCE Q-POINTER-WIDTH 2)) A-ZERO)  ;Divide SETZ by 2
        ((M-E) ADD M-E (A-CONSTANT 1))          ; and increase shift
XASH3   (CALL-XCT-NEXT ASHDPB2)
       ((C-PDL-BUFFER-POINTER) Q-POINTER M-4
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ;; Note that we don't 2's complement it back again.
        (JUMP BIGNUM-DPB-CLEANUP)

;ASH of a bignum, in M-Q,M-C,M-I.  M-2 shift distance.
BIGASH  (JUMP-EQUAL M-2 A-ZERO RETURN-M-Q)      ;Code below doesn't work for shift of 0
        ((M-1 MD) M-2)
        (CALL-XCT-NEXT DIV)                     ;Split shift into words and bits
       ((M-2) (A-CONSTANT 31.))                 ;Q-R gets number of words, M-1 gets bits
        (JUMP-LESS-THAN MD A-ZERO BIGASHR)      ;Jump if right shift
        (JUMP-NOT-EQUAL M-1 A-ZERO BIGASH3)     ;Make BIDIV-NORMALIZE work, cannot shift by 0
        ((M-1) (A-CONSTANT 31.))                ;so shift by 31. bits and one less word.  This
        ((Q-R) SUB Q-R (A-CONSTANT 1))          ;depends on DPB with IR<9:0>=-1 generating 0.
BIGASH3 ((M-R) Q-R)                             ;Number of words of shifting
        ((M-J) A-ZERO)                          ;No words discarded
BIGASH2 ((M-B) ADD Q-R A-I)                     ;Result length is number of zero words shifted
        (CALL-XCT-NEXT BNCONS)                  ; in at bottom, + arg length, +1 at top
       ((M-B) ADD M-B (A-CONSTANT 2))           ; for bits shift, +1 for header
        ((M-E) M-R)                             ;Number of zero words at bottom
        (CALL-XCT-NEXT BIDIV-NORMALIZE-ENCODE-SHIFT)    ;Encode bit shift from M-1
       ((M-1) M-A-1 (M-CONSTANT 32.) A-1)
        ((M-ZR) ADD M-I A-J)                    ;Number of words to read from old bignum
        ((M-B) SUB M-Q A-J)                     ;Address of old bignum (offset if right shift)
        ((M-B) Q-POINTER M-B)                   ;Avoid illegal pointer lying around
        ((M-D) M-T)                             ;Address of new bignum
        (CALL-XCT-NEXT BIDIV-NORMALIZE)         ;Shift subroutine
       ((M-2) A-ZERO)                           ;0 bits in at top
        (JUMP BIGNUM-DPB-CLEANUP)               ;Fix bignum length and return

BIGASHR (JUMP-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BIGASHR-NEGATIVE)
        ((M-R) (A-CONSTANT -1))                 ;Bottom word of left-shift result discarded
        ((M-J) Q-R)                             ;Negative number of words discarded at
                                                ; bottom of input bignum
        ((M-1) ADD M-1 (A-CONSTANT 31.))        ;Convert right bit shift into left shift
        ((Q-R) SUB M-J (A-CONSTANT 1))          ;Cause M-B (to cons) to match M-ZR (to norm)
        ((M-TEM) ADD M-J A-I)
        (JUMP-GREATER-THAN M-TEM A-ZERO BIGASH2)        ;Jump if any significance
#+lambda((OA-REG-HIGH) BIGNUM-HEADER-SIGN M-C)  ;Result is just sign bits
#+exp   ((m-1) bignum-header-sign m-c)
#+exp   ((oa-reg-high) dpb m-1 oah-m-src a-zero)
        ((M-1) M-ZERO)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 0)
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;Fix up for the difference between right-shift and division on negative numbers.
BIGASHR-NEGATIVE
        ((PDL-PUSH) DPB MD Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ;; Compute a positive bignum which would be the 1's complement of the original
        ;; if bignums were stored in 2's complement: minus the original, minus one.
        ;; Shifting commutes with 1's complementing, and shifting this
        ;; positive bignum works right.
        ((PDL-PUSH) DPB M-MINUS-ONE Q-POINTER
                    (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-T) M-Q)
        (CALL QISUB)
        ;; Shift that the desired amount.
        ((M-T) PDL-POP)
        ((M-2) PDL-POP)
        ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-2)
        (CALL XASH)
        ;; 1's complement the result, getting the desired value.
        ((PDL-PUSH) DPB M-MINUS-ONE Q-POINTER
                    (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL QISUB)
        (POPJ-XCT-NEXT)
       ((M-T) PDL-POP)

;ASH of a flonum is FSC, i.e. multiply by appropriate power of 2
FLONUM-ASH
        (POPJ-AFTER-NEXT (M-I) ADD M-I A-2)     ;Add shift count to exponent
       (NO-OP)
))

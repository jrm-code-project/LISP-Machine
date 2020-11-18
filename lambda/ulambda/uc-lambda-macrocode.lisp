;-*- Mode:LISP; Base:8; readtable: ZL -*-
;       ** (c) Copyright 1983,1984,1985 Lisp Machine Inc **

;important note:
;---the PDL-PASS problem is now fixed in hardware!!---
; The lack of a pass around path on the PDL buffer necessitates some care
;in writing macro-instruction-emulation routines.  Due to the direct transfer
;hack, it is possible to pass directly from the end of one macro-instruction
;routine directly to the beginning of another.  If the one writes the PDL-BUFFER
;and the other depends on it, you could lose.  The problem can only arise with
;C-PDL-BUFFER-POINTER, since anything involving the index takes at least two uinsts.
; To avoid taking an extra microinstruction in some cases, the following convention
;is adopted: The last microinstruction is only allowed to affect C-PDL-BUFFER-POINTER
;in fields OTHER than Q-TYPED-POINTER. (ie it is OK to put in the CDR-CODE bits).
; The first microinstruction of a MISC-INST-ENTRY or macro-instruction routine
;may only depend on the Q-TYPED-POINTER fields of C-PDL-BUFFER-POINTER.

; this file for LAMBDA only.
(DEFCONST UC-LAMBDA-MACROCODE '(
;;; Note well, the byte fields *must* be heirarchical and complete.
;;; If sub byte fields overlap into a super byte field, the assembler
;;; will become confused.
        (MACRO-IR-DISPATCH-SPEC
          (OP MACRO-IR-OP
              ( (CALL CALL0 MOVE CAR CDR CADR CDDR CDAR CAAR)
                 (DEST MACRO-IR-DEST
                  ( (IGNORE PDL RETURN LAST)
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                         NIL)
                     )))
                (QIND1-A)
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( (MAKE-CLOSURE-TOP-LEVEL SUB DIV XOR) ;cannot have MUL, see qind1-fef
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                        NIL))))
                (QIND2-A)
                 (SUB-OP  macro-ir-2-bit-sub-opcode
                  ( (= < SETE-CDR SETE-1+)
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                        NIL))
                   ))
                (QIND3-A)
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( (QIBND QIBNDP SETZERO MOVEM)
                    (REGISTER MACRO-IR-REGISTER
                     ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                       NIL))
                   ))
                (BRANCH-a)
                 (BRANCH-TYPE macro-ir-2-bit-sub-opcode
                  ( (QBRALW QBRNNL QBRNNP QBRNAT)
                     (HIGH-DIGIT-OF-DELTA MACRO-IR-REGISTER
                      ()
                      ())))
                (MISC)
                 (DESTINATION MACRO-IR-DEST
                  ( (IGNORE PDL RETURN LAST)
                     (HIGH-DIGIT-OF-MISC-CODE MACRO-IR-REGISTER
                       ()
                       ())))
                (QIND4-A)
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( (CLOSURE-DISCONNECT         ;Low 9 bits local block offset of closure
                     MAKE-CLOSURE       ;Low 9 bits are local index of 4 word block
                     CLOSURE-DISCONNECT-FIRST ;Low 9 bits local block offset of closure
                     PUSH-CDR-STORE-CAR-IF-CONS ;Normal register decoding (store)
                     )
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                        NIL))))
                (ERROR)         ;17 unused
                 NIL
                (AREFI)         ;20
                 (DEST MACRO-IR-DEST
                   ((IGNORE PDL RETURN LAST)
                     (SUB-OP MACRO-IR-REGISTER
                       ((AREFI-ARRAY
                         AREFI-ARRAY-LEADER
                         AREFI-INSTANCE
                         AREFI-ARRAY-COMMON-LISP
                         AREFI-SET-ARRAY
                         AREFI-SET-ARRAY-LEADER
                         AREFI-SET-INSTANCE
                         illop)
                        nil))))
                (QIND5)
                 (SUB-OP MACRO-IR-2-BIT-SUB-OPCODE
                  ((1+ 1- ZEROP ERROR)
                   (REGISTER MACRO-IR-REGISTER
                     ((FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                      ()))))
                (qid1)
                 (dest macro-ir-dest
                       ((ignore pdl return last)
                        (sub-op macro-ir-register
                                ((reference-simple-q-vector
                                  set-simple-q-vector
                                  illop          ;reference-unmapped-instance-variable
                                  illop          ;set-unmapped-instance-variable
                                  illop          ;reference-mapped-instance-variable
                                  illop          ;set-mapped-instance-variable
                                  illop
                                  illop)
                                 nil))))
                (ERROR ERROR ERROR ERROR ERROR ERROR)   ;22-30 unused.
                 NIL
                (QIND1-B)
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( ( ADD MUL AND IOR) ;cannot have DIV, see qind1-fef
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                        NIL))))
                (QIND2-B)  ;(or qind2-b (not qind2-b)) -Hamlet.
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( ( > EQ SETE-CDDR SETE-1-)
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                        NIL))
                   ))
                (QIND3-b)
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( ( QIBNDN SETNIL PUSH-E POP)
                    (REGISTER MACRO-IR-REGISTER
                     ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                       NIL))
                   ))
                (BRANCH-b)
                 (BRANCH-TYPE macro-ir-2-bit-sub-opcode
                  ( ( QBRNL QBRNLP QBRAT ERROR)
                     (HIGH-DIGIT-OF-DELTA MACRO-IR-REGISTER
                      ()
                      ())))
                (MISC1)
                 (DESTINATION MACRO-IR-DEST
                  ( (IGNORE PDL RETURN LAST)
                     (HIGH-DIGIT-OF-MISC-CODE MACRO-IR-REGISTER
                       ()
                       ())))
                (QIND4-B)
                 (SUB-OP macro-ir-2-bit-sub-opcode
                  ( (CLOSURE-UNSHARE    ;Low 9 bits variable index in stack closure vector
                     PUSH-NUMBER                ;Low 9 bits immediate data to push.
                     PUSH-CDR-IF-CAR-EQUAL      ;Normal register decoding
                     PUSH-FEF-CONSTANT)         ;Low 9 bits index into fef q area.
                     (REGISTER MACRO-IR-REGISTER
                      ( (FEF FEF FEF FEF CONSTANT LOCAL ARG PDL-POP)
                        NIL))))
                (error)         ;37 unused
                 NIL
                )))

;;; MAIN INSTRUCTION EXECUTING LOOP
(locality a-mem)
a-qmlp-temporary(0)
(locality i-mem)

#-lambda(begin-comment)
QMLP
        (CALL-CONDITIONAL PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK QMLP-P-OR-I-OR-SB)
;        ((a-qmlp-temporary) md)
;        (call verify-accumulators)
;        ((md) a-qmlp-temporary)

qmlp-continue
        ((MD) READ-MEMORY-DATA MACRO-IR-DISPATCH SOURCE-TO-MACRO-IR)
       ((MICRO-STACK-DATA-PUSH) A-MAIN-DISPATCH)        ;PUT BACK RETURN FOR NEXT TIME
   (ERROR-TABLE ILLEGAL-INSTRUCTION)
#-lambda(end-comment)

#-exp(begin-comment)
(modulo 4)
QMLP
        (CALL-CONDITIONAL PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK QMLP-P-OR-I-OR-SB)
        ((macro-ir) md)
        (dispatch-xct-next (byte-field 7 0) use-mib)
       ((MICRO-STACK-DATA-PUSH) A-MAIN-DISPATCH)        ;PUT BACK RETURN FOR NEXT TIME
   (ERROR-TABLE ILLEGAL-INSTRUCTION)
#-exp(end-comment)

QMLP-P-OR-I-OR-SB
        (JUMP-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-R-I)
;Prepare to take SB, make sure VMA doesnt point to untyped storage.
        ((VMA) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;Funnyness with 0@U.  0@U is not saved as part of the SG.  Instead, it is physically
;replaced with the appropriate main loop return when the SG is resumed.
;Our return is currently in 0@U, so would go away if this happened.  So we
;bugger things so the standard thing is in 0@U, which requires decrementing the PC.
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
        ((LOCATION-COUNTER) SUB LOCATION-COUNTER (A-CONSTANT #+lambda 2 #+exp 1))
        (JUMP-XCT-NEXT SBSER)
       ((MICRO-STACK-DATA-PUSH) A-MAIN-DISPATCH)

#-lambda(begin-comment)
DMLP
        (CALL-CONDITIONAL PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK DMLP-P-OR-I-OR-SB)
        ((MD) READ-MEMORY-DATA SOURCE-TO-MACRO-IR)
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-MACRO-INSTRUCTION-ENABLE)
                M-METER-ENABLES METER-MACRO-INSTRUCTION)
        (NO-OP MACRO-IR-DISPATCH)
       ((MICRO-STACK-DATA-PUSH) A-DEBUG-DISPATCH)
    (error-table illegal-instruction)
#-lambda(end-comment)

#-exp(begin-comment)
(modulo 4)
DMLP
        (CALL-CONDITIONAL PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK DMLP-P-OR-I-OR-SB)
        ((macro-ir) md)
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-MACRO-INSTRUCTION-ENABLE)
                M-METER-ENABLES METER-MACRO-INSTRUCTION)
        (dispatch-xct-next (byte-field 7 0) use-mib)
       ((MICRO-STACK-DATA-PUSH) A-DEBUG-DISPATCH)
    (error-table illegal-instruction)
#-exp(end-comment)

METER-MACRO-INSTRUCTION
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-MACRO-INSTRUCTION-EVENT)))
;can't change to M-FEF because meter-mirco-write-header uses PDL-INDEX
        ((PDL-INDEX) M-AP)              ;Must save LC as half-word offset from FEF
        ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1)
                 (A-CONSTANT 0))        ;Shift 2 to align with location counter
                                 ;Relative PC (hwds)
        ((PDL-PUSH) SUB LOCATION-COUNTER A-TEM1 #+lambda OUTPUT-SELECTOR-RIGHTSHIFT-1)
        (JUMP-XCT-NEXT METER-MICRO-WRITE-HEADER)
       ((A-METER-LENGTH) (A-CONSTANT 1))        ;Number of meters pushed

DMLP-P-OR-I-OR-SB
        (JUMP-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-R-I)
;Prepare to take SB, make sure VMA doesnt point to untyped storage.
        ((VMA) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;Funnyness with 0@U.  0@U is not saved as part of the SG.  Instead, it is physically
;replaced with the appropriate main loop return when the SG is resumed.
;Our return is currently in 0@U, so would go away if this happened.  So we
;bugger things so the standard thing is in 0@U, which requires decrementing the PC.
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
        ((LOCATION-COUNTER) SUB LOCATION-COUNTER (A-CONSTANT #+lambda 2 #+exp 1))
        (JUMP-XCT-NEXT SBSER)
       ((MICRO-STACK-DATA-PUSH) A-DEBUG-DISPATCH)

#-lambda(begin-comment)
SINGLE-STEP
        (CHECK-PAGE-READ)
        ((M-1) (A-CONSTANT (EVAL SG-SINGLE-STEP-TRAP))) ;CHANGE STACK-GROUP-STATE
        ((A-SG-STATE) DPB M-1 (LISP-BYTE %%SG-ST-INST-DISP) A-SG-STATE) ;TO SINGLE-STEP TRAP
        ((MD) READ-MEMORY-DATA MACRO-IR-DISPATCH SOURCE-TO-MACRO-IR)
       ((MICRO-STACK-DATA-PUSH) A-SINGLE-STEP-TRAP)     ;RETURN TO STEP-BREAK
   (error-table illegal-instruction)
#-lambda(end-comment)

#-exp(begin-comment)
(modulo 4)
SINGLE-STEP
        (CHECK-PAGE-READ)
        ((macro-ir) md)
        ((M-1) (A-CONSTANT (EVAL SG-SINGLE-STEP-TRAP))) ;CHANGE STACK-GROUP-STATE
        ((A-SG-STATE) DPB M-1 (LISP-BYTE %%SG-ST-INST-DISP) A-SG-STATE) ;TO SINGLE-STEP TRAP
        (dispatch-xct-next (byte-field 7 0) use-mib)
       ((MICRO-STACK-DATA-PUSH) A-SINGLE-STEP-TRAP)     ;RETURN TO STEP-BREAK
   (error-table illegal-instruction)
#-exp(end-comment)

#+exp(modulo 4)
STEP-BREAK
STEP-BREAK-1
#+exp (no-op)
#+exp (no-op)
                                 ;instr not done yet
        ((LOCATION-COUNTER) SUB LOCATION-COUNTER (A-CONSTANT #+lambda 2 #+exp 1))
        ((MICRO-STACK-DATA-PUSH) A-SINGLE-STEP-TRAP)        ;Mustn't have empty pdl!
        (CALL TRAP)
     (ERROR-TABLE STEP-BREAK)


INSTRUCTION-STREAM-FETCHER   ;DO FETCHES ASSOCIATED WITH MULTI-UNIT INSTUCTIONS.
         (CHECK-PAGE-READ)
         (POPJ-AFTER-NEXT NO-OP)
#+lambda((MD) READ-MEMORY-DATA SOURCE-TO-MACRO-IR)
#+exp   ((macro-ir) md)

;;; THIS IS THE MISC ENTRY SMASHED IN FOR BREAKPOINTS IN FEF'S
BREAKPOINT (MISC-INST-ENTRY BPT)
        (CALL TRAP)
     (ERROR-TABLE BREAKPOINT)
        (popj)

;;CALL WITH NO ARGS
qical0-fef      (macro-ir-decode (call0 * (0 1 2 3)))
        ((m-1) ldb (byte-field 8 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        (dispatch transport read-memory-data)
        ((m-t) q-typed-pointer md)
        (call-return cbm qmrcl)

;;; Calling a constant is wierd, yes, but it's important for error detection.
qical0-constant (macro-ir-decode (call0 * constant))
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
        (call-return cbm qmrcl)

qical0-local    (macro-ir-decode (call0 * local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        ((m-t) q-typed-pointer c-pdl-buffer-index)
        (call-return cbm qmrcl)

qical0-arg      (macro-ir-decode (call0 * arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        ((m-t) q-typed-pointer c-pdl-buffer-index)
        (call-return cbm qmrcl)

qical0-pdl-pop  (macro-ir-decode (call0 * pdl-pop))
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (call-return cbm qmrcl)

qicall-fef      (macro-ir-decode (call * (0 1 2 3)))
        ((m-1) ldb (byte-field 8 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        (dispatch transport read-memory-data)
        ((m-t) q-typed-pointer md)

CBM     ((M-C) macro-ir-DEST)           ;EVENTUAL DESTINATION
CBM0                                    ;%OPEN-CALL-BLOCK etc. call in here
        ((M-ZR) ADD PDL-BUFFER-POINTER  ;Open macro-to-macro call block
                 (A-CONSTANT (EVAL %LP-CALL-BLOCK-LENGTH)))
        ((M-TEM) SUB M-ZR A-IPMARK)     ;Compute delta to prev open block
        ((m-TEM1) DPB M-TEM (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK)
                A-DEFAULT-CALL-STATE)   ;Normally fixnum 0, has %%lp-cls-attention set if
                                        ; metering enabled.
        ((M-TEM) SUB M-ZR A-AP)         ;Compute delta to prev active block
        ((m-TEM1) DPB M-TEM (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) A-TEM1)
        ((C-PDL-BUFFER-POINTER-PUSH)    ;Push LPCLS Q
            DPB M-C (LISP-BYTE %%LP-CLS-DESTINATION) A-TEM1)
        ((C-PDL-BUFFER-POINTER-PUSH)    ;Push LPEXS Q
            (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((C-PDL-BUFFER-POINTER-PUSH)    ;Push LPENS Q
            (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (POPJ-AFTER-NEXT                ;Push LPFEF Q
          (C-PDL-BUFFER-POINTER-PUSH) M-T)
       ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-ZR)        ;A-IPMARK -> new open block

;;; Calling a constant is wierd, yes, but it's important for error detection.
qicall-constant (macro-ir-decode (call * constant))
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        (jump-xct-next cbm)
       ((m-t) q-typed-pointer md)

qicall-local    (macro-ir-decode (call * local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-xct-next cbm)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qicall-arg      (macro-ir-decode (call * arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-xct-next cbm)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qicall-pdl-pop  (macro-ir-decode (call * pdl-pop))
        (jump-xct-next cbm)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qimove-ignore-fef-0 (macro-ir-decode (move ignore 0))   ;   fef 0-77   (4 entries)
        ((vma-start-read) add macro-ir-displacement a-fef)
        (check-page-read)
        (popj-after-next dispatch transport read-memory-data)
       ((m-t) q-typed-pointer read-memory-data)

qimove-ignore-fef (macro-ir-decode (move ignore (1 2 3)))       ;   fef 100-377   (4 entries)
        ((m-1) ldb (byte-field 8 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        (popj-after-next dispatch transport read-memory-data)
       ((m-t) q-typed-pointer read-memory-data)

qimove-ignore-constant (macro-ir-decode (move ignore constant))
#-lambda(begin-comment)
        (popj-after-next (oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
       ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        (popj-after-next)
       ((m-t) md)
#-exp(end-comment)

qimove-ignore-local (macro-ir-decode (move ignore local))
        (popj-after-next (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qimove-ignore-arg (macro-ir-decode (move ignore arg))
        (popj-after-next (pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qimove-ignore-pdl-pop (macro-ir-decode (move ignore pdl-pop))
        (popj-after-next (m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
       (no-op)

qimove-pdl-fef-0 (macro-ir-decode (move pdl 0)) ;   fef 0-77   (4 entries)
        ((vma-start-read) add macro-ir-displacement a-fef)
        (check-page-read)
        (popj-after-next dispatch transport read-memory-data)
       ((c-pdl-buffer-pointer-push m-t) q-typed-pointer read-memory-data)

qimove-pdl-fef (macro-ir-decode (move pdl (1 2 3)))     ;   fef 100-377   (4 entries)
        ((m-1) ldb (byte-field 8 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        (popj-after-next dispatch transport read-memory-data)
       ((c-pdl-buffer-pointer-push m-t) q-typed-pointer read-memory-data)

qimove-pdl-constant (macro-ir-decode (move pdl constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        (popj-after-next (m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        (popj-after-next (m-t) md)
#-exp(end-comment)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))

qimove-pdl-local (macro-ir-decode (move pdl local))
        (popj-after-next
          (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-pointer-push m-t) q-typed-pointer c-pdl-buffer-index)

qimove-pdl-arg (macro-ir-decode (move pdl arg))
        (popj-after-next
          (pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
       ((c-pdl-buffer-pointer-push m-t) q-typed-pointer c-pdl-buffer-index)

qimove-pdl-pdl-pop (macro-ir-decode (move pdl pdl-pop))
        (popj-after-next                ;this OK with respect to last instruction restrictions
          (m-t) q-typed-pointer c-pdl-buffer-pointer-pop) ;pop and push dont quite cancel
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))

qimove-return-fef  (macro-ir-decode (move return (0 1 2 3)))    ;fef 0-377 (four entries)
        ((micro-stack-data-push) (a-constant (i-mem-loc qmddr)))
        (jump-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)

qimove-return-constant (macro-ir-decode (move return constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump qmddr)

qimove-return-local (macro-ir-decode (move return local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-xct-next qmddr)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qimove-return-arg (macro-ir-decode (move return arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-xct-next qmddr)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qimove-return-pdl-pop (macro-ir-decode (move return pdl-pop))
        (jump-xct-next qmddr)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qimove-last-fef (macro-ir-decode (move last (0 1 2 3))) ;   fef 0-377   (4 entries)
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qimove-last-constant (macro-ir-decode (move last constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qimove-last-local (macro-ir-decode (move last local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb c-pdl-buffer-index q-all-but-cdr-code
            (a-constant (byte-value q-cdr-code cdr-nil)))

qimove-last-arg (macro-ir-decode (move last arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb c-pdl-buffer-index q-all-but-cdr-code
            (a-constant (byte-value q-cdr-code cdr-nil)))

qimove-last-pdl-pop (macro-ir-decode (move last pdl-pop))
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop) ;pop and push dont quite cancel
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qicxr-ignore-fef  (macro-ir-decode ((car cdr caar cadr cdar cddr) ignore (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       (no-op)

qicxr-ignore-constant (macro-ir-decode ((car cdr caar cadr cdar cddr) ignore constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       (no-op)

qicxr-ignore-local (macro-ir-decode ((car cdr caar cadr cdar cddr) ignore local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qicxr-ignore-arg (macro-ir-decode ((car cdr caar cadr cdar cddr) ignore arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qicxr-ignore-pdl-pop (macro-ir-decode ((car cdr caar cadr cdar cddr) ignore pdl-pop))
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qicxr-pdl-fef (macro-ir-decode ((car cdr caar cadr cdar cddr) pdl (0 1 2 3)))   ;fef 0-377
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
cxr-of-m-t-to-pdl
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       (no-op)
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicxr-pdl-constant (macro-ir-decode ((car cdr caar cadr cdar cddr) pdl constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump cxr-of-m-t-to-pdl)

qicxr-pdl-local (macro-ir-decode ((caar cadr cdar cddr) pdl local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicar-pdl-local (macro-ir-decode (car pdl local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
                                  (a-constant (byte-value q-data-type dtp-list))
                                  qicar-pdl-local-hard)
        ((vma-start-read) c-pdl-buffer-index)
        (check-page-read)
        (popj-after-next dispatch transport md)
       ((pdl-push m-t) dpb md q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
qicar-pdl-local-hard
        (open-qcar-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next)
       ((pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))

qicar-pdl-arg (macro-ir-decode (car pdl arg))
        ((pdl-buffer-index) m+a+1 macro-ir-displacement a-ap)
        (jump-data-type-not-equal c-pdl-buffer-index
                                  (a-constant (byte-value q-data-type dtp-list))
                                  qicar-pdl-local-hard)
        ((vma-start-read) c-pdl-buffer-index)
        (check-page-read)
        (popj-after-next dispatch transport md)
       ((pdl-push m-t) dpb md q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))

qicdr-pdl-local (macro-ir-decode (cdr pdl local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (open-qcdr-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicdr-pdl-arg (macro-ir-decode (cdr pdl arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (open-qcdr-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicxr-pdl-arg (macro-ir-decode ((caar cadr cdar cddr) pdl arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicar-pdl-pdl-pop (macro-ir-decode (car pdl pdl-pop))
        (jump-data-type-not-equal pdl-top
                                  (a-constant (byte-value q-data-type dtp-list))
                                  qicar-pdl-pdl-pop-hard)
        ((vma-start-read) pdl-pop)
        (check-page-read)
        (popj-after-next dispatch transport md)
       ((pdl-push m-t) dpb md q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
qicar-pdl-pdl-pop-hard
        (open-qcar-xct-next pdl-top)
       ((m-t) q-typed-pointer pdl-pop)
        (popj-after-next)
       ((pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))

qicdr-pdl-pdl-pop (macro-ir-decode (cdr pdl pdl-pop))
        (open-qcdr-xct-next c-pdl-buffer-pointer)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicxr-pdl-pdl-pop (macro-ir-decode ((caar cadr cdar cddr) pdl pdl-pop))
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop) ;pop and push dont quite cancel
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-next)))
       (no-op)

qicxr-return-fef  (macro-ir-decode ((car cdr caar cadr cdar cddr) return (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       (no-op)
        (jump qmddr)

qicxr-return-constant (macro-ir-decode ((car cdr caar cadr cdar cddr) return constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((micro-stack-data-push) (a-constant (i-mem-loc qmddr)))

qicxr-return-local (macro-ir-decode ((car cdr caar cadr cdar cddr) return local))
        ((micro-stack-data-push) (a-constant (i-mem-loc qmddr)))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qicxr-return-arg (macro-ir-decode ((car cdr caar cadr cdar cddr) return arg))
        ((micro-stack-data-push) (a-constant (i-mem-loc qmddr)))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qicxr-return-pdl-pop (macro-ir-decode ((car cdr caar cadr cdar cddr) return pdl-pop))
        ((micro-stack-data-push) (a-constant (i-mem-loc qmddr)))
        (dispatch-xct-next macro-ir-op cxr-execute-jump-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qicxr-last-fef (macro-ir-decode ((car cdr caar cadr cdar cddr) last (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       (no-op)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qicxr-last-constant (macro-ir-decode ((car cdr caar cadr cdar cddr) last constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       (no-op)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qicxr-last-local (macro-ir-decode ((car cdr caar cadr cdar cddr) last local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qicxr-last-arg (macro-ir-decode ((car cdr caar cadr cdar cddr) last arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qicxr-last-pdl-pop (macro-ir-decode ((car cdr caar cadr cdar cddr) last pdl-pop))
        (dispatch-xct-next macro-ir-op cxr-execute-call-xct-next)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-xct-next qmrcl)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
                (a-constant (byte-value q-cdr-code cdr-nil)))

qind1-fef (macro-ir-decode ((qind1-a qind1-b) (MUL DIV) (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (dispatch-xct-next macro-ir-sub-opcode d-nd1)
       (no-op)

qind1-constant  (macro-ir-decode ((qind1-a qind1-b) (MUL DIV) constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (dispatch-xct-next macro-ir-sub-opcode d-nd1)
       (no-op)

qind1-local  (macro-ir-decode ((qind1-a qind1-b) (MUL DIV) local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (dispatch-xct-next macro-ir-sub-opcode d-nd1)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qind1-arg    (macro-ir-decode ((qind1-a qind1-b) (MUL DIV) arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (dispatch-xct-next macro-ir-sub-opcode d-nd1)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

qind1-pdl    (macro-ir-decode ((qind1-a qind1-b) (MUL DIV) pdl-pop))
        (dispatch-xct-next macro-ir-sub-opcode d-nd1)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qadd-local   (macro-ir-decode (qind1-b add local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qadd-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qadd-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-1) ADD M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qadd-arg   (macro-ir-decode (qind1-b add arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qadd-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qadd-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-1) ADD M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qadd-hard-pi (jump-xct-next qiadd-hard)
        ((m-t) q-typed-pointer c-pdl-buffer-index)

qadd-fef (macro-ir-decode (qind1-b add (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qiadd-hard)
        (jump-data-type-not-equal c-pdl-buffer-pointer
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

qadd-constant (macro-ir-decode (qind1-b add constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiadd-hard)
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

qadd-pdl  (macro-ir-decode (qind1-b add pdl-pop))
        (jump-data-type-not-equal-xct-next c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiadd-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
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

qsub-local   (macro-ir-decode (qind1-a sub local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qsub-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qsub-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qsub-arg   (macro-ir-decode (qind1-a sub arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qsub-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qsub-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qsub-hard-pi (jump-xct-next qisub)
        ((m-t) q-typed-pointer c-pdl-buffer-index)

qsub-fef (macro-ir-decode (qind1-a sub (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qisub)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qisub)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

QSUB-CONSTANT (MACRO-IR-DECODE (QIND1-a SUB CONSTANT))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qisub)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qisub)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qsub-pdl  (macro-ir-decode (qind1-a sub pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qisub)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qisub)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-1) SUB M-1 A-2)
        (DISPATCH-POPJ-XCT-NEXT (I-ARG 1)       ;duplicate FIXPACK-P
         (BYTE-FIELD 2 (DIFFERENCE Q-POINTER-WIDTH 1))
         M-1 D-FXOVCK)
       ((C-PDL-BUFFER-POINTER-PUSH M-T) DPB M-1 Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qand-local   (macro-ir-decode (qind1-b and local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 and c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qand-arg   (macro-ir-decode (qind1-b and arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 and c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qand-fef (macro-ir-decode (qind1-b and (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 and c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qand-constant (MACRO-IR-DECODE (QIND1-b AND CONSTANT))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 and c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qand-pdl  (macro-ir-decode (qind1-b and pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiand-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 and c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qior-local   (macro-ir-decode (qind1-b ior local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 ior c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qior-arg   (macro-ir-decode (qind1-b ior arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 ior c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qior-fef (macro-ir-decode (qind1-b ior (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 ior c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qior-constant (MACRO-IR-DECODE (QIND1-b IOR CONSTANT))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 ior c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qior-pdl  (macro-ir-decode (qind1-b ior pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qiior-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 ior c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qxor-local   (macro-ir-decode (qind1-a xor local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 xor c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qxor-arg   (macro-ir-decode (qind1-a xor arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 xor c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qxor-fef (macro-ir-decode (qind1-a xor (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 xor c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qxor-constant (MACRO-IR-DECODE (QIND1-a XOR CONSTANT))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 xor c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qxor-pdl  (macro-ir-decode (qind1-a xor pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        (jump-data-type-not-equal-xct-next c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qixor-hard)
        (popj-after-next
          (m-t) output-selector-mask-25 xor c-pdl-buffer-pointer a-t)
       ((c-pdl-buffer-pointer) dpb m-t q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-next)))

qeql-local  (macro-ir-decode (qind2-a = local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qeql-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qeql-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeql-arg   (macro-ir-decode (qind2-a = arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qeql-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qeql-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeql-hard-pi (jump-xct-next qmeql)
        ((m-t) q-typed-pointer c-pdl-buffer-index)

qeql-fef  (macro-ir-decode (qind2-a = (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmeql)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmeql)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeql-constant (macro-ir-decode (qind2-a = constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmeql)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmeql)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeql-pdl    (macro-ir-decode (qind2-a = pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmeql)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmeql)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qgrp-local  (macro-ir-decode (qind2-b > local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qgrp-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qgrp-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qgrp-arg   (macro-ir-decode (qind2-b > arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qgrp-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qgrp-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qgrp-hard-pi (jump-xct-next qmgrp)
        ((m-t) q-typed-pointer c-pdl-buffer-index)

qgrp-fef  (macro-ir-decode (qind2-b > (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmgrp)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmgrp)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qgrp-constant (macro-ir-decode (qind2-b > constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmgrp)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmgrp)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qgrp-pdl  (macro-ir-decode (qind2-b > pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmgrp)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmgrp)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-LESS-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qlsp-local  (macro-ir-decode (qind2-a < local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qlsp-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qlsp-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qlsp-arg   (macro-ir-decode (qind2-a < arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qlsp-hard-pi)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qlsp-hard-pi)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qlsp-hard-pi (jump-xct-next qmlsp)
        ((m-t) q-typed-pointer c-pdl-buffer-index)

qlsp-fef  (macro-ir-decode (qind2-a < (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmlsp)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmlsp)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qlsp-constant (macro-ir-decode (qind2-a < constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-t) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-t) md)
#-exp(end-comment)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmlsp)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmlsp)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qlsp-pdl   (macro-ir-decode (qind2-a < pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) qmlsp)
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-fix)) qmlsp)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 M-T)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-GREATER-OR-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeq-local  (macro-ir-decode (qind2-b eq local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        ((M-1) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-2) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeq-arg   (macro-ir-decode (qind2-b eq arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        ((M-1) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-2) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeq-fef-0 (macro-ir-decode (qind2-b eq 0))
        ((vma-start-read) add macro-ir-displacement a-fef)
        (check-page-read)
        ((m-1) q-typed-pointer c-pdl-buffer-pointer-pop)
        ((m-t) a-v-nil)
        (dispatch transport read-memory-data)
        ((m-2) q-typed-pointer read-memory-data)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeq-fef  (macro-ir-decode (qind2-b eq (1 2 3)))
        ((m-1) ldb (byte-field 8 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        ((m-1) q-typed-pointer c-pdl-buffer-pointer-pop)
        ((m-t) a-v-nil)
        (dispatch transport read-memory-data)
        ((m-2) q-typed-pointer read-memory-data)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeq-constant (macro-ir-decode (qind2-b eq constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((m-2) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((m-2) md)
#-exp(end-comment)
        ((M-1) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

qeq-pdl   (macro-ir-decode (qind2-b eq pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        ((m-2) q-typed-pointer c-pdl-buffer-pointer-pop)
        ((m-1) q-typed-pointer c-pdl-buffer-pointer-pop)
        ((M-T) A-V-NIL)
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-1 A-2)
       ((M-T) A-V-TRUE)

;;; NON-DESTINATION-GROUP-2
;   E IN VMA, C(E) IN M-T

;THESE COMPARE C(E) TO TOP OF STACK, POP,
; AND LEAVE T OR NIL IN M-T IN LIEU OF SETTING INDICATORS

XMEQ (MISC-INST-ENTRY M-EQ)
        ((M-T) C-PDL-BUFFER-POINTER-POP)
QMEQ                                    ;MC-LINKAGE
QIEQ    ((M-2) Q-TYPED-POINTER M-T)
        ((M-1) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (JUMP-NOT-EQUAL M-1 A-2 XFALSE)
  (MISC-INST-ENTRY TRUE)
XTRUE   (POPJ-AFTER-NEXT (M-T) A-V-TRUE)
       (NO-OP)

;the QIND2s which do modify C(E)  SCDR SCDDR SP1 SM1

qiscdr-fef      (macro-ir-decode (qind2-a sete-cdr (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (open-qcdr m-t)
        (jump store-fef-offset)


;-CONSTANT ILLEGAL

qiscdr-local    (macro-ir-decode (qind2-a sete-cdr local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (open-qcdr-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qiscdr-arg      (macro-ir-decode (qind2-a sete-cdr arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (open-qcdr-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

;-PDL-POP illegal

qiscddr-fef     (macro-ir-decode (qind2-b sete-cddr (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (open-qcdr m-t)
        (open-qcdr m-t)
        (jump store-fef-offset)

;-CONSTANT ILLEGAL

qiscddr-local   (macro-ir-decode (qind2-b sete-cddr local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (open-qcdr-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (open-qcdr m-t)
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qiscddr-arg     (macro-ir-decode (qind2-b sete-cddr arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (open-qcdr-xct-next c-pdl-buffer-index)
       ((m-t) q-typed-pointer c-pdl-buffer-index)
        (open-qcdr m-t)
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

;-PDL-POP illegal

qisp1-fef       (macro-ir-decode (qind2-a sete-1+ (0 1 2 3)))
        (call-xct-next fetch-fef-offset-to-pdl)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-xct-next store-fef-offset)
       (call x1pls)

;-CONSTANT ILLEGAL

qisp1-local     (macro-ir-decode (qind2-a sete-1+ local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
                                  (a-constant (byte-value q-data-type dtp-fix)) qisp1-local-slow)
        ((c-pdl-buffer-index m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix) 1)))
        (popj-not-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                              (byte-mask boxed-sign-bit 1))))
        (call-xct-next fix-overflow-1)
       ((m-1) q-pointer m-t)
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qisp1-local-slow
        (call-xct-next x1pls)
       ((c-pdl-buffer-pointer-push) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qisp1-arg       (macro-ir-decode (qind2-a sete-1+ arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-index
                                  (a-constant (byte-value q-data-type dtp-fix)) qisp1-arg-slow)
        ((c-pdl-buffer-index m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix) 1)))
        (popj-not-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                              (byte-mask boxed-sign-bit 1))))
        (call-xct-next fix-overflow-1)
       ((m-1) q-pointer m-t)
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qisp1-arg-slow
        (call-xct-next x1pls)
       ((c-pdl-buffer-pointer-push) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

;-PDL-POP illegal

qism1-fef       (macro-ir-decode (qind2-b sete-1- (0 1 2 3)))
        (call-xct-next fetch-fef-offset-to-pdl)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-xct-next store-fef-offset)
       (call x1mns)

;-CONSTANT ILLEGAL

qism1-local     (macro-ir-decode (qind2-b sete-1- local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qism1-local-slow)
        ((c-pdl-buffer-index m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix)
                                  (byte-value q-pointer -1))))
        (popj-not-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                              (byte-value boxed-num-except-sign-bit -1))))
        (call-xct-next fix-overflow-1)
       ((m-1) q-pointer m-t a-minus-one)
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qism1-local-slow
        (call-xct-next x1mns)
       ((c-pdl-buffer-pointer-push) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qism1-arg       (macro-ir-decode (qind2-b sete-1- arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-not-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-fix)) qism1-arg-slow)
        ((c-pdl-buffer-index m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix)
                                  (byte-value q-pointer -1))))
        (popj-not-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                              (byte-value boxed-num-except-sign-bit -1))))
        (call-xct-next fix-overflow-1)
       ((m-1) q-pointer m-t a-minus-one)
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

qism1-arg-slow
        (call-xct-next x1mns)
       ((c-pdl-buffer-pointer-push) q-typed-pointer c-pdl-buffer-index)
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) q-typed-pointer m-t)

;-PDL-POP illegal

qibnd-fef       (macro-ir-decode (qind3-a qibnd (0 1 2 3)))
  ;Save current contents, don't change
  ; bound locn returnned in M-B
  ; CDR-CODE of bound locn contents returnned in M-E.
        ((m-1) ldb (byte-field 8 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
qibnd   (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-BIND READ-MEMORY-DATA)
 ;forwarding happens, so VMA gets locn bound.  MD has current contents.
        ((M-E) SELECTIVE-DEPOSIT READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER
                 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((MD) DPB MD Q-TYPED-POINTER A-ZERO)    ;clear out CDR-code bits, which are used as
                                ;flags on PDL.
qbnd4-closure-1
        ((M-1) ADD (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-QLBNDP)
 ;TEST P.C.E. (THIS M-CONST JUST HAPPENED TO
        ((M-1) SUB M-1 A-QLBNDH)                ; BE AROUND AT THE WRONG TIME).
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 TRAP)
   (ERROR-TABLE PDL-OVERFLOW SPECIAL)           ;M-1 SHOULD BE NEGATIVE AS 24-BIT QUANTITY
        (JUMP-IF-BIT-SET-XCT-NEXT M-QBBFL qbf1)  ;JUMP IF NOT FIRST IN BLOCK
       ((M-B) VMA)      ;BIND FINALLY POINTED-TO CELL.  M-B is also a returnned value.
        ((MD) IOR MD (A-CONSTANT (BYTE-VALUE %%SPECPDL-BLOCK-START-FLAG 1)))
        ((M-QBBFL) DPB (M-CONSTANT -1) A-FLAGS)
        ;; Set attention in running frame.
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX
                (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
qbf1    ((VMA-START-WRITE) ADD A-QLBNDP M-ZERO ALU-CARRY-IN-ONE)        ;STORE PREV CONTENTS
        (CHECK-PAGE-WRITE-no-sequence-break)    ;HAVE INCRD A-QLBNDP, NO SEQ BRK
        ((A-QLBNDP) ADD VMA (A-CONSTANT 1))
        (GC-WRITE-TEST)
        ((WRITE-MEMORY-DATA) Q-POINTER M-B      ;LOCATIVE PNTR TO BOUND LOCN
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
        ((VMA-START-WRITE) A-QLBNDP)            ;STORE POINTER TO BOUND CELL
        (CHECK-PAGE-WRITE-no-sequence-break)    ;NO SEQ BRK, BIND NOT REALLY FINISHED
        (GC-WRITE-TEST)         ;NOTE, POPJ MUST BE DELAYED BECAUSE CANNOT START WRITE
        (popj)          ;AND INSTRUCTION FETCH SIMULTANEOUSLY.

;       (call-xct-next bind-fef-offset)         ;leaves address on pdl
;      ((m-1) ldb (byte-field 8. 0) macro-ir)

;QBND2  ((VMA-START-READ M-B) C-PDL-BUFFER-POINTER-POP) ;FETCH CURRENT CONTENTS
;       (CHECK-PAGE-READ)                               ;INT OK, HAVEN'T HACKED YET
;;VMA and M-B have location being bound.  MD has current contents.
;;Will return with old-value saved and Q-ALL-BUT-TYPED-POINTER in M-E,
;;VMA and M-B updated to actual location bound (different if there is a ONE-Q-FORWARD).
;QBND4  (DISPATCH TRANSPORT-BIND-READ-WRITE READ-MEMORY-DATA)   ;DON'T FOLLOW EXTERNAL-VALUE-CELL-PTR

;;LOGICALLY SIMILAR CODE TO BELOW EXISTS AT QBSPCL
;       ;M-E can be an invisible pointer, so don't save typed pointer part.
;       ((M-E) SELECTIVE-DEPOSIT READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER
;                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
;       ((MD) DPB MD Q-TYPED-POINTER A-ZERO)
;QBND4-CLOSURE-1
;       ((M-1) ADD (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-QLBNDP)        ;TEST P.C.E. (THIS M-CONST JUST HAPPENED TO
;       ((M-1) SUB M-1 A-QLBNDH)                ; BE AROUND AT THE WRONG TIME).
;       (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 TRAP)
;   (ERROR-TABLE PDL-OVERFLOW SPECIAL)          ;M-1 SHOULD BE NEGATIVE AS 24-BIT QUANTITY
;       (JUMP-IF-BIT-SET-XCT-NEXT M-QBBFL QBND3)         ;JUMP IF NOT FIRST IN BLOCK
;       ((M-B) VMA)     ;THIS INSTRUCTION MAKES IT FOLLOW FORWARDING POINTERS
;                       ;AND BIND THAT FINALLY POINTED-TO CELL RATHER THAN THE
;                       ;INTERNAL VALUE CELL.  THIS ONLY APPLIES WHEN IT IS
;                       ;FORWARDED WITH DTP-ONE-Q-FORWARD RATHER THAN
;                       ;DTP-EXTERNAL-VALUE-CELL-POINTER
;       ((MD) IOR MD (A-CONSTANT (BYTE-VALUE %%SPECPDL-BLOCK-START-FLAG 1)))
;       ((M-QBBFL) DPB (M-CONSTANT -1) A-FLAGS)
;        ;; Set attention in running frame.
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
;       ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX
;                (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
;QBND3  ((VMA-START-WRITE) ADD A-QLBNDP M-ZERO ALU-CARRY-IN-ONE)        ;STORE PREV CONTENTS
;       (CHECK-PAGE-WRITE)                      ;HAVE INCRD A-QLBNDP, NO SEQ BRK
;       ((A-QLBNDP) ADD VMA (A-CONSTANT 1))
;       (GC-WRITE-TEST)
;       ((WRITE-MEMORY-DATA) Q-POINTER M-B      ;LOCATIVE PNTR TO BOUND LOCN
;               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
;       ((VMA-START-WRITE) A-QLBNDP)            ;STORE POINTER TO BOUND CELL
;       (CHECK-PAGE-WRITE)                      ;NO SEQ BRK, BIND NOT REALLY FINISHED
;       (GC-WRITE-TEST) ;NOTE, POPJ MUST BE DELAYED BECAUSE CANNOT START WRITE
;        (popj-after-next
;         (VMA) M-B)            ;AND INSTRUCTION FETCH SIMULTANEOUSLY.  *** this probably loses***
;       (no-op)

qibnd-local     (macro-ir-decode (qind3-a qibnd local))
        (call qipshe-local)
        (jump-xct-next qibnd)
       ((vma-start-read) c-pdl-buffer-pointer-pop)

 ;      (jump qbnd2)

qibnd-arg       (macro-ir-decode (qind3-a qibnd arg))
        (call qipshe-arg)
        (jump-xct-next qibnd)
       ((vma-start-read) c-pdl-buffer-pointer-pop)

;       (jump qbnd2)

qibndn-fef      (macro-ir-decode (qind3-b qibndn (0 1 2 3)))
        ((M-T) A-V-NIL)         ;AND RE-BIND TO NIL
QIBDN2  (call qibnd-fef)        ;SAVE PRESENT BINDING
QIBDN1  ((vma) m-b)
        ((M-T WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT M-E
                Q-ALL-BUT-TYPED-POINTER A-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj-after-next
          (m-t) q-typed-pointer m-t)
       (no-op)

XUBI  (MISC-INST-ENTRY %USING-BINDING-INSTANCES)  ;One arg, a list of binding instances.
        (JUMP-XCT-NEXT QCLS1)
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)

XBIND (MISC-INST-ENTRY %BIND)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;ARG 2, NEW VALUE TO GIVE
   (ERROR-TABLE RESTART XBIND)
        (check-data-type-call-not-equal c-pdl-buffer-pointer m-zr dtp-locative trap)
                                                ;ARG 1, POINTER TO CELL TO BIND
   (ERROR-TABLE ARGTYP LOCATIVE PP 0 XBIND)
   (ERROR-TABLE ARG-POPPED 0 PP M-T)
XBIND1 ;(JUMP-XCT-NEXT QIBDN1)
      ;(CALL QBND2)
        (call-xct-next qibnd)
       ((vma-start-read) c-pdl-buffer-pointer-pop)
        (jump qibdn1)

;Come here to make a binding "for a closure"; that is, a binding
;whose %%SPECPDL-CLOSURE-BINDING flag is set, saying that the binding
;was done "outside" of the actual function in the stack frame.
QBND4-CLOSURE
 ;      (DISPATCH TRANSPORT-BIND-READ-WRITE READ-MEMORY-DATA)   ;already done.
;LOGICALLY SIMILAR CODE TO BELOW EXISTS AT QBSPCL
        ;M-E can be an invisible pointer, so don't save typed pointer part.
        ((M-E) SELECTIVE-DEPOSIT READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER
                 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

        ((MD) ANDCA MD (A-CONSTANT (BYTE-VALUE %%SPECPDL-BLOCK-START-FLAG 1)))
        (JUMP-XCT-NEXT QBND4-CLOSURE-1)
       ((MD) IOR MD (A-CONSTANT (BYTE-VALUE %%SPECPDL-CLOSURE-BINDING 1)))

qibndn-local    (macro-ir-decode (qind3-b qibndn local))
        (call qibnd-local)
        (popj-after-next
          (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) selective-deposit c-pdl-buffer-index
                q-all-but-typed-pointer a-v-nil)

qibndn-arg      (macro-ir-decode (qind3-b qibndn arg))
        (call qibnd-arg)
        (popj-after-next
          (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) selective-deposit c-pdl-buffer-index
                q-all-but-typed-pointer a-v-nil)

qibndp-fef      (macro-ir-decode (qind3-a qibndp (0 1 2 3)))
        (JUMP-XCT-NEXT QIBDN2)  ;AND REBIND TO POP(PDL)
       ((M-T) C-PDL-BUFFER-POINTER-POP)

qibndp-local    (macro-ir-decode (qind3-a qibndp local))
        (call qibnd-local)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (popj-after-next
          (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index) selective-deposit c-pdl-buffer-index
                q-all-but-typed-pointer a-t)

qibndp-arg      (macro-ir-decode (qind3-a qibndp arg))
        (call qibnd-arg)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (popj-after-next
          (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index) selective-deposit c-pdl-buffer-index
                q-all-but-typed-pointer a-t)

qisetn-fef      (macro-ir-decode (qind3-b setnil (0 1 2 3)))
        (jump-xct-next store-fef-offset)
       ((m-t) a-v-nil)

qisetn-local     (macro-ir-decode (qind3-b setnil local))
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index m-t) a-v-nil)

qisetn-arg      (macro-ir-decode (qind3-b setnil arg))
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index m-t) a-v-nil)

qisetz-fef      (macro-ir-decode (qind3-a setzero (0 1 2 3)))
        (jump-xct-next store-fef-offset)
       ((m-t) (a-constant (byte-value q-data-type dtp-fix)))

qisetz-local     (macro-ir-decode (qind3-a setzero local))
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index m-t) (a-constant (byte-value q-data-type dtp-fix)))

qisetz-arg      (macro-ir-decode (qind3-a setzero arg))
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index m-t) (a-constant (byte-value q-data-type dtp-fix)))

qipshe-fef (macro-ir-decode (qind3-b push-e (0 1 2 3)))
;GET EFFECTIVE ADDRESS, NOT PLANNING TO READ CONTENTS.  POPJ WITH EFF ADR ON PDL
;MUSTN'T TRANSPORT, NOR BARF AT TRAP, BUT MUST FOLLOW EXTERNAL VALUE CELL POINTER
        ((m-1) ldb (byte-field 8. 0) macro-ir)
        ((vma-start-read) add m-fef a-1)
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT-WRITE READ-MEMORY-DATA) ;FOLLOW ALL INVZ
       ((C-PDL-BUFFER-POINTER-PUSH) DPB VMA     ;PUSH VMA AS A LOCATIVE
                Q-POINTER (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                            (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))

qipshe-local    (macro-ir-decode (qind3-b push-e local))
;PUSH LOCATIVE POINTER TO ADDRESS OF LOCAL VARIABLE ONTO THE PDL
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) ADD macro-ir-displacement A-LOCALP)
        ;; Set the flag saying that we have pointers to our stack frame
        ;; so we should not flush it.
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-2) C-PDL-BUFFER-INDEX)
        (POPJ-AFTER-NEXT
         (C-PDL-BUFFER-POINTER-PUSH) DPB M-K Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))
       ((C-PDL-BUFFER-INDEX) DPB M-MINUS-ONE (LISP-BYTE %%LP-ENS-UNSAFE-REST-ARG) A-2)

qipshe-arg      (macro-ir-decode (qind3-b push-e arg))
;PUSH LOCATIVE POINTER TO ADDRESS OF ARGUMENT VARIABLE ONTO THE PDL
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) ADD A-AP macro-ir-displacement ALU-CARRY-IN-ONE)
        ;; Set the flag saying that we have pointers to our stack frame
        ;; so we should not flush it.
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-2) C-PDL-BUFFER-INDEX)
        (POPJ-AFTER-NEXT
         (C-PDL-BUFFER-POINTER-PUSH) DPB M-K Q-POINTER
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))
       ((C-PDL-BUFFER-INDEX) DPB M-MINUS-ONE (LISP-BYTE %%LP-ENS-UNSAFE-REST-ARG) A-2)

qimvm-fef       (macro-ir-decode (qind3-a movem (0 1 2 3)))
        (jump-xct-next store-fef-offset)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer)

qimvm-local     (macro-ir-decode (qind3-a movem local))
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index m-t) q-typed-pointer c-pdl-buffer-pointer)

qimvm-arg       (macro-ir-decode (qind3-a movem arg))
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index m-t) q-typed-pointer c-pdl-buffer-pointer)

qipop-fef       (macro-ir-decode (qind3-b pop (0 1 2 3)))
        (jump-xct-next store-fef-offset)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qipop-local     (macro-ir-decode (qind3-b pop local))
        (popj-after-next
         (pdl-buffer-index) add macro-ir-displacement a-localp)
       ((c-pdl-buffer-index m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

qipop-arg       (macro-ir-decode (qind3-b pop arg))
        (popj-after-next
         (pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
       ((c-pdl-buffer-index m-t) q-typed-pointer c-pdl-buffer-pointer-pop)


;   main dispatch is on 10 bits. since opcode is 4 and branch type is 3, the three high
; bits of the delta are taken in.  The branch delta is in 16 bit words, while the PC
; operates in bytes.  A branch delta of 777, which would otherwise result in a branch .,
; indicates a long branch (in which case the signed delta is in the next 16 bit halfword).
;   Timing of loading the LC relative to the POPJ which returns to the top level:
; The UINST which has the POPJ bit set MAY alter the LC.  However, it does not win for
;  the XCT-NEXT uinst to alter the LC, since the hardware may be forcing a
;  ((VMA-START-READ) LC) during that cycle and the wrong value of the LC would be used.

qbralw-pos  (macro-ir-decode (branch-a qbralw (0 1 2 3)))  ; branch-always, positive delta:
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbralw-neg-7 (macro-ir-decode (branch-a qbralw (7))) ;negative delta, (may be long branch)
        (jump-equal macro-ir-displacement (a-constant 77) qbralw-long)
qbralw-neg   (macro-ir-decode (branch-a qbralw (4 5 6))) ;negative delta, not long
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbralw-long  (advance-instruction-stream)
        (jump-if-bit-set (byte-field 1 15.) macro-ir qbralw-long-minus)
        ((m-1) dpb macro-ir (byte-field 15. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbralw-long-minus
        ((m-1) dpb macro-ir (byte-field 15. #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrnl-pos (macro-ir-decode (branch-b qbrnl (0 1 2 3)))
; (call branch-cdr-code-test)
        (popj-not-equal m-t a-v-nil)            ;POPJ ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbrnl-neg-7 (macro-ir-decode (branch-b qbrnl (7)))
        (jump-equal macro-ir-displacement (a-constant 77) qbrnl-long)
qbrnl-neg    (macro-ir-decode (branch-b qbrnl (4 5 6))) ;negative delta, not long
; (call branch-cdr-code-test)
        (popj-not-equal m-t a-v-nil)            ;POPJ ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8 #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrnl-long
; (call branch-cdr-code-test)
        (jump-equal m-t a-v-nil qbralw-long)
qbrnot-long
        (advance-instruction-stream)
        (popj)


qbrnnl-pos (macro-ir-decode (branch-a qbrnnl (0 1 2 3)))
; (call branch-cdr-code-test)
        (popj-equal m-t a-v-nil)                ;POPJ ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbrnnl-neg-7 (macro-ir-decode (branch-a qbrnnl (7)))
        (jump-equal macro-ir-displacement (a-constant 77) qbrnnl-long)
qbrnnl-neg    (macro-ir-decode (branch-a qbrnnl (4 5 6))) ;negative delta, not long
; (call branch-cdr-code-test)
        (popj-equal m-t a-v-nil)                ;POPJ ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8 #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrnnl-long
; (call branch-cdr-code-test)
        (jump-not-equal m-t a-v-nil qbralw-long)
        (jump qbrnot-long)

qbrat-pos (macro-ir-decode (branch-b qbrat (0 1 2 3)))
        (popj-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)))
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbrat-neg-7 (macro-ir-decode (branch-b qbrat (7)))
        (jump-equal macro-ir-displacement (a-constant 77) qbrat-long)
qbrat-neg   (macro-ir-decode (branch-b qbrat (4 5 6))) ;negative delta, not long
        (popj-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)))
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrat-long
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)) qbrnot-long)
        (jump qbralw-long)


qbrnat-pos (macro-ir-decode (branch-a qbrnat (0 1 2 3)))
        (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)))
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbrnat-neg-7 (macro-ir-decode (branch-a qbrnat (7)))
        (jump-equal macro-ir-displacement (a-constant 77) qbrnat-long)
qbrnat-neg   (macro-ir-decode (branch-a qbrnat (4 5 6))) ;negative delta, not long
        (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)))
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrnat-long
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) qbrnot-long)
        (jump qbralw-long)

;branch-on-nil, pop if not
qbrnlp-pos (macro-ir-decode (branch-b qbrnlp (0 1 2 3)))
; (call branch-cdr-code-test)
        (jump-not-equal m-t a-v-nil qbrpop)     ;XFER ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbrpop  (popj-after-next
          (m-garbage) c-pdl-buffer-pointer-pop)
       (no-op)

qbrnlp-neg-7 (macro-ir-decode (branch-b qbrnlp (7)))
        (jump-equal macro-ir-displacement (a-constant 77) qbrnlp-long)
qbrnlp-neg    (macro-ir-decode (branch-b qbrnlp (4 5 6))) ;negative delta, not long
; (call branch-cdr-code-test)
        (jump-not-equal m-t a-v-nil qbrpop)     ;XFER ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8 #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrnlp-long
; (call branch-cdr-code-test)
        (jump-equal m-t a-v-nil qbralw-long)
        (jump-xct-next qbrnot-long)
       ((m-garbage) c-pdl-buffer-pointer-pop)

;branch-on-not-nil, pop if not
qbrnnp-pos (macro-ir-decode (branch-a qbrnnp (0 1 2 3)))
; (call branch-cdr-code-test)
        (jump-equal m-t a-v-nil qbrpop)         ;XFER ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8. #+lambda 1 #+exp 0) a-zero)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)              ;VMA <- LC, START-READ

qbrnnp-neg-7 (macro-ir-decode (branch-a qbrnnp (7)))
        (jump-equal macro-ir-displacement (a-constant 77) qbrnnp-long)
qbrnnp-neg    (macro-ir-decode (branch-a qbrnnp (4 5 6))) ;negative delta, not long
; (call branch-cdr-code-test)
        (jump-equal m-t a-v-nil qbrpop)         ;XFER ON NO BRANCH. (LONG NOT POSSIBLE)
        ((m-1) dpb macro-ir (byte-field 8 #+lambda 1 #+exp 0) a-minus-one)
        (#+lambda popj-after-next (location-counter) add location-counter a-1)
       (#+lambda no-op #+exp popj)

qbrnnp-long
; (call branch-cdr-code-test)
        (jump-not-equal m-t a-v-nil qbralw-long)
        (jump-xct-next qbrnot-long)
       ((m-garbage) c-pdl-buffer-pointer-pop)


(error-table default-arg-locations %nway-branch m-a m-t)

xnway-branch (misc-inst-entry %nway-branch)
     ;; Two args, number to dispatch on and maximum number.  If n to dispatch is equal to
     ;; or greater than the limit, we use the maximum.

  (call fxgtpp) ;Pop two guaranteed sign extended fixnums into M-1 and M-2

  (jump-less-than m-2 a-zero xnway-branch-bad)
  (jump-less-than m-2 a-1 xnway-branch-good)

xnway-branch-bad
  ((m-2) m-1)

xnway-branch-good
  ((m-2) dpb m-2 (byte-field 8. #+lambda 1 #+exp 0) a-zero)
  (#+lambda popj-after-next (location-counter) add location-counter a-2)
 (#+lambda no-op #+exp popj)


;; The inviolable convention is that results left in M-T, possibly for testing by
;; various conditional branch macroinstructions, must not have a cdr-code.  Since
;; there seem to be a number of cases which violate this, this test has been inserted
;; to track them down.  When the probability that all such offenders have been
;; located is closer to 100% than it is now, the test should be removed.  KHS 840910.

;branch-cdr-code-test
;        ((m-tem) ldb m-t q-cdr-code a-zero)
;        (popj-equal m-tem a-zero)
;        (call illop-debug)
;        ((m-t) q-typed-pointer m-t)            ;Make it continuable.
;        (popj)

;--------------------------------------------------------------------------------

q-one-plus-local (macro-ir-decode (qind5 1+ local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
           (a-constant (byte-value q-data-type dtp-fix)) q-one-plus-local-hard)
        ((m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix) (byte-value q-pointer 1))))
        (jump-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                          (byte-mask boxed-sign-bit 1)))
                    q-one-plus-overflow)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

q-one-plus-arg (macro-ir-decode (qind5 1+ arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-index
           (a-constant (byte-value q-data-type dtp-fix)) q-one-plus-local-hard)
        ((m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix) (byte-value q-pointer 1))))
        (jump-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                          (byte-mask boxed-sign-bit 1)))
                    q-one-plus-overflow)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

q-one-plus-pdl (macro-ir-decode (qind5 1+ pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) fetch-pdl-not-77)
q-one-plus-pdl-kernel
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) q-one-plus-pdl-hard)
        ((m-t) output-selector-mask-25 add c-pdl-buffer-pointer-pop
                (a-constant (plus (byte-value q-data-type dtp-fix) (byte-value q-pointer 1))))
        (jump-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                          (byte-mask boxed-sign-bit 1)))
                    q-one-plus-overflow)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

q-one-plus-fef (macro-ir-decode (qind5 1+ (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-xct-next q-one-plus-pdl-kernel)
       ((c-pdl-buffer-pointer-push) m-t)

q-one-plus-constant (macro-ir-decode (qind5 1+ constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((c-pdl-buffer-pointer-push) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((pdl-push) md)
#-exp(end-comment)
        (jump q-one-plus-pdl-kernel)

q-one-plus-local-hard
        ((pdl-push) c-pdl-buffer-index)
q-one-plus-pdl-hard
        ((micro-stack-data-push) (a-constant (i-mem-loc m-t-to-cpdl)))
    (error-table restart q-one-plus-pdl-hard)
        (dispatch-xct-next q-data-type c-pdl-buffer-pointer d-numarg)
    (error-table argtyp number pp t q-one-plus-pdl-hard)
    (error-table arg-popped 0 pp)
       ((m-a) (a-constant arith-1arg-add1))
 ;d-numarg dispatch drops through on FIX (handled above) and CHARACTER
 ; i guess there is no reason to let characters overflow into bignums...
        (popj-after-next (m-t) output-selector-mask-25
            add c-pdl-buffer-pointer-pop
                (a-constant (plus (byte-value q-data-type dtp-character)
                                  (byte-value q-pointer 1))))
       (no-op)


q-one-plus-overflow
        (call-xct-next fix-overflow-1)
       ((m-1) q-pointer m-t)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

;;;

q-one-minus-local (macro-ir-decode (qind5 1- local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-not-equal c-pdl-buffer-index
            (a-constant (byte-value q-data-type dtp-fix)) q-one-minus-local-hard)
        ((m-t) output-selector-mask-25 add c-pdl-buffer-index
            (a-constant (plus (byte-value q-data-type dtp-fix) (byte-value q-pointer -1))))
        (jump-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                          (byte-value boxed-num-except-sign-bit -1)))
                    q-one-minus-overflow)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

q-one-minus-arg (macro-ir-decode (qind5 1- arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-not-equal c-pdl-buffer-index
           (a-constant (byte-value q-data-type dtp-fix)) q-one-minus-local-hard)
        ((m-t) output-selector-mask-25 add c-pdl-buffer-index
                (a-constant (plus (byte-value q-data-type dtp-fix) (byte-value q-pointer -1))))
        (jump-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                          (byte-value boxed-num-except-sign-bit -1)))
                    q-one-minus-overflow)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

q-one-minus-pdl (macro-ir-decode (qind5 1- pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
q-one-minus-pdl-kernel
        (jump-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) q-one-minus-pdl-hard)
        ((m-t) output-selector-mask-25 add c-pdl-buffer-pointer-pop
                (a-constant (plus (byte-value q-data-type dtp-fix) (byte-value q-pointer -1))))
        (jump-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix)
                                          (byte-value boxed-num-except-sign-bit -1)))
                    q-one-minus-overflow)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

q-one-minus-fef (macro-ir-decode (qind5 1- (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-xct-next q-one-minus-pdl-kernel)
       ((c-pdl-buffer-pointer-push) m-t)

q-one-minus-constant (macro-ir-decode (qind5 1- constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((c-pdl-buffer-pointer-push) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((pdl-push) md)
#-exp(end-comment)
        (jump q-one-minus-pdl-kernel)

q-one-minus-local-hard
        ((pdl-push) c-pdl-buffer-index)
q-one-minus-pdl-hard
        ((micro-stack-data-push) (a-constant (i-mem-loc m-t-to-cpdl)))
    (error-table restart q-one-minus-pdl-hard)
        (dispatch-xct-next q-data-type c-pdl-buffer-pointer d-numarg)
    (error-table argtyp number pp t q-one-minus-pdl-hard)
    (error-table arg-popped 0 pp)
       ((m-a) (a-constant arith-1arg-sub1))
 ;d-numarg dispatch drops through on FIX (handled above) and CHARACTER
 ; i guess there is no reason to let characters underflow into bignums...
        (popj-after-next (m-t) output-selector-mask-25
            add c-pdl-buffer-pointer-pop
                (a-constant (plus (byte-value q-data-type dtp-character)
                                  (byte-value q-pointer -1))))
       (no-op)

q-one-minus-overflow
        (call-xct-next fix-overflow-1)
       ((m-1) q-pointer m-t a-minus-one)
        (popj-after-next
          (pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        (no-op)

;;;

q-zerop-local (macro-ir-decode (qind5 zerop local))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (jump-data-type-equal-xct-next c-pdl-buffer-index
            (a-constant (byte-value q-data-type dtp-fix)) zero?)
       ((m-1) output-selector-extend-25 c-pdl-buffer-index)
        (jump-xct-next xzerop)
       ((c-pdl-buffer-pointer-push) c-pdl-buffer-index)

q-zerop-arg (macro-ir-decode (qind5 zerop arg))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (jump-data-type-equal-xct-next c-pdl-buffer-index
            (a-constant (byte-value q-data-type dtp-fix)) zero?)
       ((m-1) output-selector-extend-25 c-pdl-buffer-index)
        (jump-xct-next xzerop)
       ((c-pdl-buffer-pointer-push) c-pdl-buffer-index)

q-zerop-pdl (macro-ir-decode (qind5 zerop pdl-pop))
;       (call-not-equal macro-ir-displacement (a-constant 77) fetch-pdl-not-77)
q-zerop-pdl-kernel
        (jump-data-type-not-equal c-pdl-buffer-pointer
            (a-constant (byte-value q-data-type dtp-fix)) xzerop)
        ((m-1) output-selector-extend-25 c-pdl-buffer-pointer-pop)
zero?   ((m-t) a-v-true)
        (popj-after-next popj-equal m-1 a-zero)
        ((m-t) a-v-nil)

q-zerop-fef (macro-ir-decode (qind5 zerop (0 1 2 3)))
        (call-xct-next fetch-fef-offset)
       ((m-1) ldb (byte-field 8 0) macro-ir)
        (jump-xct-next q-zerop-pdl-kernel)
       ((c-pdl-buffer-pointer-push) m-t)

q-zerop-constant (macro-ir-decode (qind5 zerop constant))
#-lambda(begin-comment)
        ((oa-reg-high) dpb macro-ir-displacement oah-a-src-6-bits
                 (a-constant (byte-value oah-a-src 2200)))
        ((c-pdl-buffer-pointer-push) seta a-garbage)
#-lambda(end-comment)
#-exp(begin-comment)
        ((vma-start-read) add macro-ir-displacement a-v-constants-area)
        (check-page-read)
        ((pdl-push) md)
#-exp(end-comment)
        (jump q-zerop-pdl-kernel)

;;; MISC INSTRUCTION
;;; Note that the misc function invoked might do a micro-to-macro call,
;;; upon return MACRO-IR would not be set up.  Therefore we must not
;;; depend on it.  The MISC instruction works by doing something similar
;;; to a micro-to-micro call to the misc function, with a return address
;;; dependent on the destination; in the case of D-IGNORE there is no
;;; return address, it returns directly to the main instruction loop.
;;; This means that any misc instruction which can be called to D-IGNORE
;;; must not start a memory cycle in the same instruction that popjs.
;;; MISC insts must return their value in M-T, with 0 in Q-ALL-BUT-TYPED-POINTER.
;;; Note: array references dispatch directly in lambda, only "real" misc instructions here.
;;;  on WWII, misc instructions to IGNORE will dispatch directly to execute routine.
;;;  (on d-pdl, d-return, d-last will read out macro-ir-decode explicitly to get dispatch adr)

#-lambda(begin-comment)
MISC-IGNORE     (MACRO-IR-DECODE (MISC IGNORE (2 3 4 5 6 7)))
                (MACRO-IR-DECODE (MISC1 IGNORE *))
        ((oa-reg-low m-last-micro-entry) dpb macro-ir-decode-misc-enable oal-jump a-zero)
        (JUMP 0)                ;CALL EXECUTION ROUTINE
#-lambda(end-comment)
(begin-comment) (end-comment)
#-exp(begin-comment)
MISC-IGNORE     (macro-ir-decode (misc ignore (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5200))
       (No-op)

                (macro-ir-decode (misc ignore (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5400))
       (No-op)

                (macro-ir-decode (misc ignore (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5600))
       (No-op)

                (macro-ir-decode (misc1 ignore (0 1)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4000))
       (No-op)

                (macro-ir-decode (misc1 ignore (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4200))
       (No-op)

                (macro-ir-decode (misc1 ignore (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4400))
       (No-op)

                (macro-ir-decode (misc1 ignore (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4600))
       (No-op)

#-exp(end-comment)

(begin-comment)
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR)   ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (JUMP 0)                ;CALL EXECUTION ROUTINE
(end-comment)

#-lambda(begin-comment)
MISC-PDL        (MACRO-IR-DECODE (MISC PDL (2 3 4 5 6 7)))
                (MACRO-IR-DECODE (MISC1 PDL *))
        ((oa-reg-low m-last-micro-entry) dpb macro-ir-decode-misc-enable oal-jump a-zero)
        (call 0)                ;CALL EXECUTION ROUTINE
#-lambda(end-comment)

#-exp(begin-comment)
MISC-PDL        (macro-ir-decode (misc pdl (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5200))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

                (macro-ir-decode (misc pdl (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5400))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

                (macro-ir-decode (misc pdl (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5600))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

                (macro-ir-decode (misc1 pdl (0 1)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4000))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

                (macro-ir-decode (misc1 pdl (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4200))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

                (macro-ir-decode (misc1 pdl (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4400))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

                (macro-ir-decode (misc1 pdl (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4600))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC M-T-TO-STACK)))

#-exp(end-comment)

(begin-comment)
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR)   ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (CALL 0)                ;CALL EXECUTION ROUTINE
(end-comment)
M-T-TO-STACK
        (POPJ-AFTER-NEXT (C-PDL-BUFFER-POINTER-PUSH) Q-TYPED-POINTER M-T
                        (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
       (NO-OP)

#-lambda(begin-comment)
MISC-RETURN     (MACRO-IR-DECODE (MISC RETURN (2 3 4 5 6 7)))
                (MACRO-IR-DECODE (MISC1 RETURN *))
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))
        ((oa-reg-low m-last-micro-entry) dpb macro-ir-decode-misc-enable oal-jump a-zero)
        (JUMP 0)                ;CALL EXECUTION ROUTINE
#-lambda(end-comment)

#-exp(begin-comment)
MISC-RETURN     (macro-ir-decode (misc return (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5200))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

                (macro-ir-decode (misc return (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5400))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

                (macro-ir-decode (misc return (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5600))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

                (macro-ir-decode (misc1 return (0 1)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4000))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

                (macro-ir-decode (misc1 return (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4200))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

                (macro-ir-decode (misc1 return (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4400))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

                (macro-ir-decode (misc1 return (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4600))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))

#-exp(end-comment)

(begin-comment)
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR)   ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (JUMP 0)                ;CALL EXECUTION ROUTINE
(end-comment)

#-lambda(begin-comment)
MISC-LAST       (MACRO-IR-DECODE (MISC LAST (2 3 4 5 6 7)))
                (MACRO-IR-DECODE (MISC1 LAST *))
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))
        ((oa-reg-low m-last-micro-entry) dpb macro-ir-decode-misc-enable oal-jump a-zero)
        (JUMP 0)                ;CALL EXECUTION ROUTINE
#-lambda(end-comment)

#-exp(begin-comment)
MISC-LAST       (macro-ir-decode (misc last (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5200))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

                (macro-ir-decode (misc last (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5400))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

                (macro-ir-decode (misc last (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 5600))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

                (macro-ir-decode (misc1 last (0 1)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4000))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

                (macro-ir-decode (misc1 last (2 3)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4200))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

                (macro-ir-decode (misc1 last (4 5)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4400))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

                (macro-ir-decode (misc1 last (6 7)))
        (Dispatch-Xct-Next (Byte-Field 7. 0) macro-ir-adr
                           (Field Dispatch-Address-Multiplier 4600))
       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))

#-exp(end-comment)

(begin-comment)
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR)   ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (JUMP 0)                ;CALL EXECUTION ROUTINE

MISC1-IGNORE    (MACRO-IR-DECODE (MISC1 IGNORE *))
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR (A-CONSTANT 1000)) ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (JUMP 0)                ;CALL EXECUTION ROUTINE

MISC1-PDL       (MACRO-IR-DECODE (MISC1 PDL *))
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR (A-CONSTANT 1000)) ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (CALL 0)                ;CALL EXECUTION ROUTINE
        (POPJ-AFTER-NEXT (C-PDL-BUFFER-POINTER-PUSH) Q-TYPED-POINTER M-T
                        (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
       (NO-OP)

MISC1-RETURN    (MACRO-IR-DECODE (MISC1 RETURN *))
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR (A-CONSTANT 1000)) ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDR)))
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (JUMP 0)                ;CALL EXECUTION ROUTINE

MISC1-LAST      (MACRO-IR-DECODE (MISC1 LAST *))
        ((M-B) LDB (BYTE-FIELD 9 0) MACRO-IR (A-CONSTANT 1000)) ;conceivably could be saved.
        ((VMA-START-READ) ADD A-V-MISC-BASE M-B)
        (CHECK-PAGE-READ)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMDDL)))
       ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        (JUMP 0)                ;CALL EXECUTION ROUTINE

(end-comment)

push-number (macro-ir-decode (qind4-b push-number *))
        (popj-after-next
          (m-t) ldb (byte-field 9 0) macro-ir (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) dpb m-t q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))

push-fef-constant (macro-ir-decode (qind4-b push-fef-constant *))
        ((m-t) ldb (byte-field 9 0) macro-ir)
        ((vma-start-read) add m-t a-fef)
        (check-page-read)
        (dispatch transport read-memory-data)
        (popj-after-next (m-t) q-typed-pointer read-memory-data)
       ((c-pdl-buffer-pointer-push) dpb m-t q-all-but-cdr-code
           (a-constant (byte-value q-cdr-code cdr-next)))

;; If the object from the top of the stack is a cons whose car is EQUAL to our argument,
;; leave the cons's cdr on the stack in its place and return T in the indicators.
;; Otherwise leave nothing on the stack and return NIL in the indicators.
PUSH-CDR-IF-CAR-EQUAL-FEF       (MACRO-IR-DECODE (QIND4-b PUSH-CDR-IF-CAR-EQUAL (0 1 2 3)))
        (CALL-XCT-NEXT FETCH-FEF-OFFSET)
       ((m-1) ldb (byte-field 8 0) macro-ir)
PCICEF-1
        ((M-B) M-T)
        (jump-data-type-not-equal-xct-next pdl-top
                                           (a-constant (byte-value q-data-type dtp-list))
            nullify-inds)
       ((m-t) q-typed-pointer pdl-pop)
        (open-carcdr m-t)
        ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-A)
        (CALL-XCT-NEXT XEQUAL)
       ((PDL-PUSH) M-B)
        (POPJ-NOT-EQUAL M-T A-V-NIL)
        (POPJ-XCT-NEXT)
       (PDL-POP)

;-CONSTANT ILLEGAL

PUSH-CDR-IF-CAR-EQUAL-LOCAL (MACRO-IR-DECODE (QIND4-b PUSH-CDR-IF-CAR-EQUAL LOCAL))
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (JUMP-XCT-NEXT PCICEF-1)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

PUSH-CDR-IF-CAR-EQUAL-ARG  (MACRO-IR-DECODE (QIND4-b PUSH-CDR-IF-CAR-EQUAL ARG))
        ((pdl-buffer-index) add macro-ir-displacement a-ap alu-carry-in-one)
        (JUMP-XCT-NEXT PCICEF-1)
       ((m-t) q-typed-pointer c-pdl-buffer-index)

PUSH-CDR-IF-CAR-EQUAL-PDL-POP  (MACRO-IR-DECODE (QIND4-b PUSH-CDR-IF-CAR-EQUAL PDL-POP))
;       (call-not-equal macro-ir-displacement (a-constant 77) FETCH-PDL-NOT-77)
        (JUMP-XCT-NEXT PCICEF-1)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)

NULLIFY-INDS
        (POPJ-XCT-NEXT)
       ((M-T) A-V-NIL)

;; If the object from the top of the stack is a cons,
;; leave its cdr on the stack in its place and store its car at our source address,
;; and leave T in the indicators.
;; Otherwise leave nothing on the stack and return NIL in the indicators.
push-cdr-store-car-if-cons-fef (macro-ir-decode (qind4-a push-cdr-store-car-if-cons (0 1 2 3)))
        (jump-data-type-not-equal-xct-next pdl-top (a-constant (byte-value q-data-type dtp-list))
                nullify-inds)
       ((m-t) q-typed-pointer pdl-pop)
        (open-carcdr m-t)
        ((PDL-PUSH) M-T)
        (CALL-XCT-NEXT STORE-FEF-OFFSET)
       ((M-T) M-A)
        (POPJ-XCT-NEXT)
       ((M-T) A-V-TRUE)

push-cdr-store-car-if-cons-local (macro-ir-decode (qind4-a push-cdr-store-car-if-cons local))
        (jump-data-type-not-equal-xct-next pdl-top (a-constant (byte-value q-data-type dtp-list))
                nullify-inds)
       ((m-t) q-typed-pointer pdl-pop)
        (open-carcdr m-t)
        ((PDL-PUSH) M-T)
        ((pdl-buffer-index) add macro-ir-displacement a-localp)
        (popj-after-next (c-pdl-buffer-index) m-a)
       ((m-t) a-v-true)

push-cdr-store-car-if-cons-arg (macro-ir-decode (qind4-a push-cdr-store-car-if-cons arg))
        (jump-data-type-not-equal-xct-next pdl-top (a-constant (byte-value q-data-type dtp-list))
                nullify-inds)
       ((m-t) q-typed-pointer pdl-pop)
        (open-carcdr m-t)
        ((PDL-PUSH) M-T)
        ((pdl-buffer-index) add a-ap macro-ir-displacement alu-carry-in-one)
        (popj-after-next (c-pdl-buffer-index) m-a)
       ((m-t) a-v-true)


;PUSH-CDR-STORE-CAR-IF-CONS
;       ((M-1) Q-DATA-TYPE PDL-TOP)
;       (JUMP-NOT-EQUAL-XCT-NEXT M-1 (A-CONSTANT (EVAL DTP-LIST)) NULLIFY-INDS)
;      ((M-T) Q-TYPED-POINTER PDL-POP)
;       (CALL CARCDR)
;       ((PDL-PUSH) M-T)
;       (CALL-XCT-NEXT STOCYC)
;      ((M-T) M-A)
;       (POPJ-XCT-NEXT)
;      ((M-T) A-V-TRUE)

;; If top of stack is a list, return T in indicators.
;; If it is not, pop it and return NIL in indicators.
XCONSP-OR-POP (MISC-INST-ENTRY CONSP-OR-POP)
        ((M-T) A-V-TRUE)
        (popj-after-next popj-data-type-equal pdl-top
                (a-constant (byte-value q-data-type dtp-list)))
       ((M-T) SETA A-V-NIL PDL-POP)

XPUSH-CDR-STORE-INDS (MISC-INST-ENTRY INDICATORS-VALUE)
        (POPJ)   ;Return the contents of M-T!!!

;;; DESTINATION HANDLERS
;   DATA TO STORE IN M-T

;THESE DESTINATIONS ARE NOW HANDLED BY THE INSTRUCTION FOLLOWING THE DESTINATION DISPATCH
;QMDDN          ;NEXT (ARG)
;QMDDS          ;STACK

;;; DESTINATION LAST

QMDDL   ((C-PDL-BUFFER-POINTER-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
            (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))

;;; Activate pending call.
QMRCL   ((M-S PDL-INDEX) M-AP)
     ;; Shift 2 to align with location counter.
        ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1)
                                 (A-CONSTANT 0))
          ;Relative PC (hwds)
        ((M-TEM) SUB LOCATION-COUNTER A-TEM1 #+lambda OUTPUT-SELECTOR-RIGHTSHIFT-1)
        ((M-AP PDL-INDEX) A-IPMARK)
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
        ((m-fef) m-a)
     ;; M-R passes argument count to callee.
#+lambda((M-R) SUB OUTPUT-SELECTOR-MASK-11 PDL-BUFFER-POINTER A-IPMARK)
#+exp   ((pdl-index) sub pdl-buffer-pointer a-ipmark)
#+exp   ((m-r) pdl-index)
     ;; Build exit-state word from PC, M-FLAGS, and previous contents (old QLLV)
        ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL %LP-EXIT-STATE)))
     ;; Code knows that %%LP-EXS-EXIT-PC is 0017
        ((m-TEM1) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT (BYTE-FIELD 21 17) A-TEM)
     ;; Save M-QBBFL then clear it. (cleared after dispatch-xct-next below for speed)
        ((PDL-INDEX-INDIRECT) DPB M-FLAGS (LISP-BYTE %%LP-EXS-PC-STATUS) A-TEM1)
     ;; Following code integrated from old FINISH-ENTERED-FRAME.
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-TEM) C-PDL-BUFFER-INDEX)
        ((C-PDL-BUFFER-INDEX) DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED) A-TEM)
     ;; Compute new pdl level in PDL-INDEX (truncated to 10 bits).
        ((PDL-INDEX) SUB M-AP A-S)
        ((M-PDL-BUFFER-ACTIVE-QS) ADD PDL-INDEX A-PDL-BUFFER-ACTIVE-QS)
     ;; Note: M-FLAGS must be taken care of in PDL-BUFFER-DUMP, also.
        (CALL-GREATER-THAN M-PDL-BUFFER-ACTIVE-QS A-PDL-BUFFER-HIGH-WARNING
            PDL-BUFFER-DUMP-RESET-FLAGS)
        (dispatch-xct-next qmrcl-dispatch m-a)
       ((M-FLAGS) SELECTIVE-DEPOSIT M-FLAGS M-FLAGS-EXCEPT-PROCESSOR-FLAGS A-ZERO)

QMRCL-TRAP
        (declare (clobbers a-a))
        ((VMA) A-V-NIL)
QMRCL-TRAP-1
        (declare (clobbers a-a))
;Many things that call this do so just before checking M-ERROR-SUBSTATUS, so save it.
        ((M-A) M-ERROR-SUBSTATUS)
        (CALL TRAP)
    (ERROR-TABLE CALL-TRAP)
        ((M-ERROR-SUBSTATUS) M-A)
        (POPJ)

fetch-fef-offset  (declare (args a-1) (values a-t))
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        (popj-after-next dispatch transport read-memory-data)
       ((m-t) q-typed-pointer read-memory-data)

fetch-fef-offset-to-pdl         ;result to both M-T and C-PDL-BUFFER-POINTER-PUSH
        ((vma-start-read) add m-fef a-1)
        (check-page-read)
        (popj-after-next dispatch transport read-memory-data)
       ((m-t c-pdl-buffer-pointer-push) dpb read-memory-data q-all-but-cdr-code
                 (a-constant (byte-value q-cdr-code cdr-next)))

store-fef-offset
        ((m-1) ldb (byte-field 8. 0) macro-ir)
;       ((PDL-BUFFER-INDEX) M-AP)                       ;0(AP) -> FEF
;       ((VMA-START-READ) ADD C-PDL-BUFFER-INDEX A-1)
        ((vma-start-read) add m-fef a-1)
        (CHECK-PAGE-READ)
XSET2                                                   ;Entry from SET
        (DISPATCH TRANSPORT-WRITE READ-MEMORY-DATA)     ;FOLLOW ALL INVZ
 ;      (JUMP-IF-BIT-SET Q-FLAG-BIT MD QSTFE-MONITOR)
QSTFE-M ((MD-START-WRITE) SELECTIVE-DEPOSIT
                MD Q-ALL-BUT-TYPED-POINTER A-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj-after-next
          (M-T) Q-TYPED-POINTER M-T)
       (no-op)

;Get here if FLAG-BIT set in a cell about to be written.  Find monitor function
; following the cell, and call it with args  <old-value>, <new-value>.
; If old value is DTP-NULL, don't crash the machine; pass unbound marker instead
;QSTFE-MONITOR
;       (CALL-XCT-NEXT QSTFE-M)         ;Complete store
;       ((M-A) Q-TYPED-POINTER MD)      ;Save copy of old value
;       (POPJ-EQUAL M-A A-T)            ;Same thing, thats all.
;       ((C-PDL-BUFFER-POINTER-PUSH) M-T)       ;Save copy of new to return
;       (CALL P3ZERO)
;       ((VMA-START-READ) M+1 VMA)
;       (CHECK-PAGE-READ)
;       ((C-PDL-BUFFER-POINTER-PUSH) MD)        ;Monitoring function
;       ((C-PDL-BUFFER-POINTER-PUSH) M-A)       ;Old value
;       ((M-TEM) Q-DATA-TYPE M-A)               ;Substitute Unbound marker?
;       (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (EVAL DTP-NULL)) QSTFE-MONITOR-1)
;       ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-UNB))
;       ((C-PDL-BUFFER-POINTER) READ-MEMORY-DATA)
;QSTFE-MONITOR-1
;       ((C-PDL-BUFFER-POINTER-PUSH) M-T)       ;New value --cdr-code
;       ((ARG-JUMP MMCALL) (I-ARG 2))
;       (POPJ-AFTER-NEXT
;         (M-T) C-PDL-BUFFER-POINTER-POP)
;       (NO-OP)

;bind-fef-offset
;SPECIAL KLUDGEY ADDRESS ROUTINE FOR BIND.  ALWAYS INDIRECTS ONE LEVEL.
;RETURNS WITH ADDRESS ON PDL.
;       ((PDL-BUFFER-INDEX) M-AP)               ;0(AP) -> FEF
;       ((VMA-START-READ) ADD C-PDL-BUFFER-INDEX A-1)
;       ((vma-start-read) add m-fef a-1)
;       (CHECK-PAGE-READ)
;       (DISPATCH TRANSPORT-BIND READ-MEMORY-DATA)      ;ONLY TRANSPORT, DON'T DO INVZ
;       (POPJ-AFTER-NEXT                        ;AND RETURN LOCATIVE ON PDL
;        (C-PDL-BUFFER-POINTER-PUSH) DPB READ-MEMORY-DATA
;               Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
;                       ;NO PASS-AROUND PATH ON PDL-BUFFER
;       (call-data-type-not-equal md
;               (a-constant (byte-value q-data-type dtp-one-q-forward)) illop)

md-transport
        (popj-after-next dispatch transport read-memory-data)
       ((m-t) q-typed-pointer read-memory-data)

fetch-pdl-not-77  ;register field of PDL seen with displacement not equal to 77.
        ;fetch operand and leave on pdl and return.  Beware pdl may be ref'ed on
        ;first uinst after return.

;Delta 0 thru 37 is index of unmapped ivar.
;40 thru 67 is index of mapped ivar.  70 thru 76 are reserved.
;; Error if SELF is not an instance.
        ((M-T) DPB M-ZERO Q-POINTER A-SELF)
        (CALL-NOT-EQUAL M-T
         (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE))
         TRAP)
;; Is it unmapped (less than 40)?
    (ERROR-TABLE SELF-NOT-INSTANCE)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 5) MACRO-IR-DISPLACEMENT QADPDLT-MAPPED)
;; Ref to unmapped instance variable.
        ((VMA-START-READ) M+A+1 M-1 A-SELF)
        (CHECK-PAGE-READ)
        (dispatch transport read-memory-data)
        (POPJ-AFTER-NEXT
          (C-PDL-BUFFER-POINTER-PUSH M-T) q-typed-pointer read-memory-data)
       (NO-OP)

QADPDLT-MAPPED
        (CALL-GREATER-OR-EQUAL MACRO-IR-DISPLACEMENT (A-CONSTANT 70) TRAP) ;Reserved.
   (ERROR-TABLE ILLEGAL-INSTRUCTION)
;Map the SELF-REF-INDEX thru the mapping table, an ART-16B array.
        ((M-T) (BYTE-FIELD 20 1) MACRO-IR-DISPLACEMENT)
        ((VMA-START-READ) M+A+1 A-SELF-MAPPING-TABLE M-T)
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) MACRO-IR-DISPLACEMENT QADPDLT-MAPPED-EVEN)
        ((MD) (BYTE-FIELD 20 20) MD)
QADPDLT-MAPPED-EVEN
        ((M-1) (BYTE-FIELD 20 0) MD)
;; Now we have the actual index; ref that slot.
        ((VMA-START-READ) M+A+1 M-1 A-SELF)
        (CHECK-PAGE-READ)
        (dispatch transport read-memory-data)
        (POPJ-AFTER-NEXT
          (C-PDL-BUFFER-POINTER-PUSH M-T) q-typed-pointer read-memory-data)
       (NO-OP)



QCAAR
        (open-qcar m-t)
        (JUMP QCAR)

QCADR
        (open-qcdr m-t)
        (JUMP QCAR)

QCDAR
        (open-qcar m-t)
        (JUMP QCDR)

QCDDR
        (open-qcdr m-t)
        (JUMP QCDR)

(begin-comment) Zwei Lossage (end-comment)

;; These should be flushed.

reference-simple-q-vector (macro-ir-decode (qid1 * reference-simple-q-vector))
        ((m-q) macro-ir-displacement)
        ((vma-start-read) m+a+1 c-pdl-buffer-pointer-pop a-q)
        (check-page-read)
        (dispatch transport read-memory-data)
        ((m-t) q-typed-pointer read-memory-data)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

set-simple-q-vector (macro-ir-decode (qid1 * set-simple-q-vector))
        ((m-q) macro-ir-displacement)
        ((vma) m+a+1 c-pdl-buffer-pointer-pop a-q)
        ((md-start-write m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (check-page-write)
        (gc-write-test)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))


))

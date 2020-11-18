;-*-mode: lisp; base: 8; readtable: ZL-*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;
(DEFCONST UC-STACK-GROUPS '(

;STACK-GROUP STUFF
; THE STACK GROUP QS MAY BE CONSIDERED TO BE DIVIDED INTO THREE GROUPS:
;   STATIC POINTERS, DYNAMIC STATE, AND LINKAGE QS.
; STATIC POINTERS ARE THINGS LIKE PDL ORIGINS.  THEY ARE LOADED, BUT NEVER STORED
;  BY STACK GROUP HACKING ROUTINES.
; DYNAMIC STATE ARE THINGS WHICH ARE CHANGED DURING THE OPERATION OF THE MACHINE.
;  THEY ARE BOTH LOADED AND STORED.
; LINKAGE QS ARE THINGS LIKE SG-STATE, SG-PREVIOUS-STACK-GROUP, ETC.  THEY ARE NOT
;  LOADED AND UNLOADED FROM A-MEMORY BY THE LOW LEVEL ROUTINES, BUT NOT "UPDATED".
;EACH OF THESE GROUPS IS ALLOCATED A CONTIGIOUS AREA WITHIN THE STACK-GROUP-HEADER.

; WHEN SAVING STATE, THINGS ARE FIRST SAVED IN THE PDL-BUFFER.  THE ENTIRE BLOCK
;IS THEN WRITTEN TO MAIN MEMORY.  WHEN RESTORING, THE ENTIRE BLOCK IS READ INTO
;THE PDL BUFFER, THEN RESTORED TO THE APPROPRIATE PLACES.  SINCE GENERALLY THE MOST
;"VOLATILE" THINGS WANT TO BE SAVED FIRST AND RESTORED LAST, A SORT OF PUSH DOWN LIKE
;OPERATION IS APPROPRIATE.  THUS THE VMA PUSHED ONTO THE PDL-BUFFER FIRST.
;ON THE STORE INTO MAIN MEMORY, IT IS STORED LAST.  THE STORING PROCEEDS IN LEADER
;INDEX ORDER (IE COUNTING DOWN IN MEMORY).  THUS THE VMA WINDS UP IN THE LOWEST
;Q OF THE ARRAY LEADER (JUST BEYOND THE LEADER-HEADER).  ON THE RELOAD,
;THE VMA IS THE FIRST THING READ FROM MEMORY.  IT THUS BECOMES DEEPEST ON THE PDL-BUFFER
;STACK, AND IS THE LAST THING RESTORED TO THE REAL MACHINE.  (ACTUALLY, THE VERY FIRST
;THING SAVED IS THE PDL-BUFFER-PHASING Q, WHICH IS SOMEWHAT SPECIAL SINCE IT IS ACTUALLY
;"USED" WHEN FIRST READ ON THE RELOAD).

;STORE DYNAMIC STATE OF MACHINE IN CURRENT STACK GROUP
;  MUST NOT CLOBBER M-A ON IMMEDIATE RETURN (THAT CAN HAVE SG-GOING-TO)

SGLV            ;STORE EVERYTHING IN PDL-BUFFER IN REVERSE ORDER ITS TO BE WRITTEN TO MEMORY
                ;M-TEM HAS DESIRED NEW STATE FOR CURRENT STACK GROUP.  SWAP L-B-P OF
                ;CURRENT STACK GROUP UNLESS 1.7 OF M-TEM IS 1.
     ;; Error check, pdl information correct.
        ((PDL-BUFFER-INDEX) SUB M-AP A-PDL-BUFFER-HEAD)
        ((PDL-BUFFER-INDEX) ADD PDL-BUFFER-INDEX (A-CONSTANT 1))
        (CALL-NOT-EQUAL PDL-BUFFER-INDEX A-PDL-BUFFER-ACTIVE-QS ILLOP)

        ((M-3) M-METER-STACK-GROUP-ENABLE)
        ((A-LAST-STACK-GROUP) DPB M-3 Q-CDR-CODE A-QCSTKG)
        ((M-STACK-GROUP-SWITCH-FLAG) DPB (M-CONSTANT -1) A-FLAGS) ;SHUT OFF TRAPS, ETC.
        ((M-3) A-QCSTKG)
        (call-data-type-not-equal m-3 (a-constant (byte-value q-data-type dtp-stack-group))
                        illop)
        ((A-SG-STATE) DPB M-TEM (LISP-BYTE %%SG-ST-CURRENT-STATE) A-SG-STATE)
        ((M-4) PDL-BUFFER-POINTER)              ;SAVE ORIGINAL PDL LVL

        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-4 Q-POINTER          ;SAVE PDL-BUFFER-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ; FOR PHASING.
        ((C-PDL-BUFFER-POINTER-PUSH) DPB VMA Q-POINTER  ;SAVE VMA AS A LOCATIVE
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-fix)))
        ((M-3) Q-ALL-BUT-POINTER VMA            ;SAVE TAGS OF VMA, M-1, AND M-2 AS FIXNUM
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA) Q-ALL-BUT-POINTER M-1)
        ((M-3) DPB VMA (BYTE-FIELD 8 8) A-3)
        ((VMA) Q-ALL-BUT-POINTER M-2)
        ((C-PDL-BUFFER-POINTER-PUSH) DPB VMA (BYTE-FIELD 8 16.) A-3)
        ((C-PDL-BUFFER-POINTER-PUSH) Q-POINTER M-1      ;SAVE POINTERS OF M-1, M-2 AS FIXNUMS
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((C-PDL-BUFFER-POINTER-PUSH) Q-POINTER M-2
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-1) (A-CONSTANT (M-MEM-LOC M-ZR)))   ;SAVE REGS M-ZR THROUGH M-K
SGLV0   ((OA-REG-HIGH) DPB M-1 OAH-M-SRC A-ZERO)
        ((C-PDL-BUFFER-POINTER-PUSH) M-GARBAGE)         ;M-GARBAGE = 0@M
;;; Verificator in uc-storage-allocation
;       (call verify-accumulator-contents)
        (JUMP-LESS-THAN-XCT-NEXT M-1 (A-CONSTANT (M-MEM-LOC M-K)) SGLV0)
       ((M-1) ADD M-1 (A-CONSTANT 1))

        (JUMP-IF-BIT-SET-XCT-NEXT M-TEM (BYTE-FIELD 1 6) SGLV2) ;WANT TO SWAP THIS GUY' L-B-P?
       ((C-PDL-BUFFER-POINTER-PUSH) DPB M-FLAGS Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;*** M-J contains a pointer, but has DTP = 0
        ((M-J) A-QLBNDP)
SGLV3   (JUMP-LESS-OR-EQUAL M-J A-QLBNDO SGLV4) ;Jump if through
        ((VMA-START-READ) M-J)
        (CHECK-PAGE-READ)
     ;Dispatch after jump because jump doesn't use md.
        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-locative))
                sgvsp)                                 ;Jump if not binding
        (dispatch transport-bind md)
;       (DISPATCH TRANSPORT-NO-EVCP MD)
        ((M-C) READ-MEMORY-DATA)                ;Pointer to cell bound
        ((VMA-START-READ M-J) SUB M-J (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-bind READ-MEMORY-DATA)
;       (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-D) READ-MEMORY-DATA)                ;Old binding to be restored
        ((VMA-START-READ) M-C)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-bind READ-MEMORY-DATA)
;       (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-ZR) READ-MEMORY-DATA)               ;New binding to be saved
        ((WRITE-MEMORY-DATA-START-WRITE) DPB M-D Q-TYPED-POINTER A-ZR)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        ((WRITE-MEMORY-DATA) DPB M-ZR Q-TYPED-POINTER A-D)
        ((VMA-START-WRITE) M-J)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (JUMP-XCT-NEXT SGLV3)           ;In this direction, no need to check flag bits
       ((M-J) SUB M-J (A-CONSTANT 1))   ;Since things must remain paired as long as in bindings

SGVSP   (JUMP-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG)
                                  READ-MEMORY-DATA SGLV3)
       ((M-J) SUB M-J (A-CONSTANT 1))           ;Space past, down to Q with flag bit
        (JUMP-LESS-OR-EQUAL M-J A-QLBNDO SGLV4)
        ((VMA-START-READ) M-J)
        (CHECK-PAGE-READ)
        (JUMP SGVSP)

SGLV4   ((A-SG-STATE) DPB (M-CONSTANT -1) (LISP-BYTE %%SG-ST-IN-SWAPPED-STATE) A-SG-STATE)
SGLV2   ((C-PDL-BUFFER-POINTER-PUSH) A-QLARYL)
        ((C-PDL-BUFFER-POINTER-PUSH) A-QLARYH)

        ((C-PDL-BUFFER-POINTER-PUSH) A-TRAP-MICRO-PC)

        ((M-B) MICRO-STACK-DATA-POP)    ;Save return from SGLV

  ;Save current macro state in frame pointed to by M-AP.
;       ((PDL-INDEX) M-AP)              ;QMDDR
;       ((M-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH 2)
;                (A-CONSTANT 0))        ;Shift 2 to align with location counter
        ((m-tem1) dpb m-fef (byte-field q-pointer-width #+lambda 2 #+exp 1) a-zero)

                         ;Relative PC (hwds)
        ((M-TEM) SUB LOCATION-COUNTER A-TEM1 #+lambda OUTPUT-SELECTOR-RIGHTSHIFT-1)

        ;; Build exit-state word from PC
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((PDL-INDEX-INDIRECT) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT (BYTE-FIELD 21 17) A-TEM)
                        ;CODE KNOWS THAT %%LP-EXS-EXIT-PC IS 0017

;***check that LC is still in current FEF
;       ((pdl-index) m-ap)
;       (jump-data-type-not-equal pdl-index-indirect
;                (a-constant (byte-value q-data-type dtp-fef-pointer))
;               sglv2-no-check)
        (jump-data-type-not-equal m-fef (a-constant (byte-value q-data-type dtp-fef-pointer))
                sglv2-no-check)
        (call-less-than m-tem a-zero illop-debug)       ;result from SUB above.
        ((m-1) ldb (byte-field 31. 1) m-tem)
;       ((vma-start-read) add pdl-index-indirect (a-constant (eval %fefhi-storage-length)))
        ((vma-start-read) add output-selector-mask-25 m-fef (a-constant (eval %fefhi-storage-length)))
        (check-page-read-no-interrupt)
     ;No transport because running FEF must be in newspace.
        ((md) q-pointer md)
        (call-less-or-equal md a-1 illop-debug)
sglv2-no-check
;***

        ((M-TEM) MICRO-STACK-POINTER)
        (JUMP-EQUAL M-TEM (A-CONSTANT 0) TMS-3) ;Return if nothing to save
        ((M-1) M+A+1 M-TEM A-QLBNDP)    ;TEST P.C.E.
        ((M-1) SUB M-1 A-QLBNDH)
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 ILLOP)  ;RED ALERT!
  (ERROR-TABLE PDL-OVERFLOW SPECIAL)            ;M-1 should be negative as 24-bit quantity
        ((M-Q) DPB (M-CONSTANT -1)      ;First Q in block has flag bit
                (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
TMS-2
#+LAMBDA((WRITE-MEMORY-DATA) MICRO-STACK-DATA-POP A-Q)  ;Note- this involves a LDB operation
#+EXP   ((WRITE-MEMORY-DATA) LDB (BYTE-FIELD 17. 0) MICRO-STACK-DATA-POP A-Q)
        ((A-QLBNDP) ADD A-QLBNDP M-ZERO ALU-CARRY-IN-ONE)
        ((VMA-START-WRITE) A-QLBNDP)
        (CHECK-PAGE-WRITE)
        ((M-TEM) MICRO-STACK-POINTER)   ;Loop if not done
        (JUMP-NOT-EQUAL-XCT-NEXT M-TEM A-ZERO TMS-2)
        ;Remaining Q's in block do not have flag bit
       ((M-Q) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT
                (A-CONSTANT (BYTE-MASK %%LP-EXS-MICRO-STACK-SAVED)))

TMS-3   ((MICRO-STACK-DATA-PUSH) M-B)   ;Restore return from SGLV

        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)              ;SAVE A-IPMARK
       ((M-K) A-IPMARK)
        ((M-K) SUB M-K A-QLPDLO)
        ((C-PDL-BUFFER-POINTER-PUSH) DPB Q-POINTER M-K
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)              ;SAVE M-AP
       ((M-K) M-AP)
        ((M-K) SUB M-K A-QLPDLO)
        ((C-PDL-BUFFER-POINTER-PUSH) DPB Q-POINTER M-K
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

        ((M-1) A-QLBNDP)
        ((M-1) SUB M-1 A-QLBNDO)
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-1 Q-POINTER          ;SAVE L-B-P LEVEL
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) M-4)                      ;SAVE P-B-POINTER AS RELATIVE ADR TO ENTIRE PDL
        ((M-1) SUB M-K A-QLPDLO)        ; ARRAY
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-1 Q-POINTER          ;SAVE PDL LEVEL
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

   ;**following slot in stack group available for recycle.  Was A-TRAP-AP-LEVEL.
        ((C-PDL-BUFFER-POINTER-PUSH) (a-constant (byte-value q-data-type dtp-fix)))
;       ((C-PDL-BUFFER-POINTER-PUSH) A-SG-FOLLOWING-STACK-GROUP)
        ((C-PDL-BUFFER-POINTER-PUSH) A-SG-CALLING-ARGS-NUMBER)
        ((C-PDL-BUFFER-POINTER-PUSH) A-SG-CALLING-ARGS-POINTER)
        ((C-PDL-BUFFER-POINTER-PUSH) A-SG-PREVIOUS-STACK-GROUP)
        ((C-PDL-BUFFER-POINTER-PUSH) A-SG-STATE)

        ((M-2) SUB M-AP A-4)            ;GET - QS IN ACTIVE FRAME (AS 10. BIT NEG NUMBER OR 0)
        (JUMP-EQUAL M-2 A-ZERO SGLV1)
        ((M-2) (BYTE-FIELD 10. 0) M-2 (A-CONSTANT -1))  ;EXTEND SIGN TO MAKE REAL NEG NUMBER.
SGLV1   (CALL PDL-BUFFER-MAKE-ROOM) ;CAUSE ENTIRE PDL-BUFFER TO GET WRITTEN OUT.
                                    ;BUT NOT THE SG-LEADER STUFF PUSHED ABOVE
        ((VMA) A-QCSTKG)
                                                                  ;2 FOR LEADER HEADER
        ((VMA) SUB output-selector-mask-25
                VMA (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative) 2 (EVAL SG-STATE))))
        (CALL-XCT-NEXT SG-WRITE-BLOCK-FROM-PDL-BUFFER)
      ((M-ZR) ADD M-ZERO (A-CONSTANT (DIFFERENCE (EVAL SG-PDL-PHASE) (EVAL SG-STATE)))
                        ALU-CARRY-IN-ONE)       ;WANT PHASE-STATE+1
        (POPJ-XCT-NEXT)
       ((M-STACK-GROUP-SWITCH-FLAG) DPB (M-CONSTANT 0) A-FLAGS)

SG-WRITE-BLOCK-FROM-PDL-BUFFER
        ((WRITE-MEMORY-DATA-START-WRITE) C-PDL-BUFFER-POINTER-POP)
        (CHECK-PAGE-WRITE)
        (gc-write-test)

;;; Debugging code, don't write bad pointers in sg-leave
;        ((m-tem) q-pointer vma)
;        (jump-not-equal m-tem (a-constant 2700014) non-magic)
;        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-pointer)) non-magic)

;  ((pdl-push) vma)
;  ((a-gc-trap-md-volatility) map1-volatility l1-map)
;  (dispatch l2-map-status-code d-get-map-bits)
;  (call-if-bit-clear map2c-oldspace-meta-bit l2-map-control illop)

;; Check volatility of magic losing pointer.

;  (call-xct-next read-page-volatility)
; ((m-lam) q-page-number pdl-top)

;  (call-less-than m-tem a-gc-trap-md-volatility illop)

;  ((vma) pdl-pop)

;non-magic
        (POPJ-LESS-OR-EQUAL M-ZR (A-CONSTANT 1))
        ((M-ZR) SUB M-ZR (A-CONSTANT 1))
        (JUMP-XCT-NEXT SG-WRITE-BLOCK-FROM-PDL-BUFFER)
       ((VMA) SUB VMA (A-CONSTANT 1))

SG-LOAD-BLOCK-INTO-PDL-BUFFER
        ((VMA-START-READ) VMA)
        (CHECK-PAGE-READ)
        (POPJ-LESS-OR-EQUAL M-ZR (A-CONSTANT 0))  ;IN THIS CASE, USELESS READ DONE & IGNORED
        ((M-ZR) SUB M-ZR (A-CONSTANT 1))
        (DISPATCH TRANSPORT-AC READ-MEMORY-DATA)
        ((C-PDL-BUFFER-POINTER-PUSH) READ-MEMORY-DATA)
SG-L-P-B-1
        (JUMP-XCT-NEXT SG-LOAD-BLOCK-INTO-PDL-BUFFER)
       ((VMA) ADD VMA (A-CONSTANT 1))

SG-LOAD-STATIC-STATE            ;LOAD STATIC STATE FOR STACK GROUP
        ((M-C) A-QCSTKG)
        (CALL-DATA-TYPE-NOT-EQUAL M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-STACK-GROUP))
                ILLOP)
        ((VMA-START-READ) SUB output-selector-mask-25
                M-C (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative)
                                      2
                                      (EVAL SG-REGULAR-PDL))))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
                ;SET UP ARRAY LENGTH IN M-S AND DATA ORIGIN IN M-E
#+exp   ((vma) md)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA
                   (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE READ-MEMORY-DATA
                   ARRAY-HEADER-SETUP-DISPATCH)
        ((m-a) m-minus-one)
;             invalidate-array-cache read-memory-data
     (call store-array-registers-in-accumulators)
  (ERROR-TABLE CALLS-SUB SG-REG-PDL)
        ((VMA-START-READ) SUB output-selector-mask-25
                M-C (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative)
                                      2
                                      (EVAL SG-REGULAR-PDL-LIMIT))))
        (CHECK-PAGE-READ)
        (dispatch transport md)
        ((A-QLPDLO) q-pointer M-e)
        ((A-QLPDLH) ADD M-e a-s)
        ((M-TEM) Q-POINTER READ-MEMORY-DATA)
        (JUMP-GREATER-THAN M-TEM A-s SG-LOAD-STATIC-STATE-1)
        ((A-QLPDLH) ADD M-e A-TEM)
SG-LOAD-STATIC-STATE-1
        ((VMA-START-READ) SUB output-selector-mask-25
                 M-C (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative)
                                       2
                                       (EVAL SG-SPECIAL-PDL))))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
;COMPUTE SAME THINGS FOR LINEAR-BINDING-ARRAY
#+exp   ((vma) md)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA
                   (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE READ-MEMORY-DATA
                   ARRAY-HEADER-SETUP-DISPATCH)
        ((m-a) m-minus-one)
   (ERROR-TABLE CALLS-SUB SG-SPECIAL-PDL)
     (call store-array-registers-in-accumulators)
        ((VMA-START-READ) SUB output-selector-mask-25
                 M-C (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative)
                                       2
                                       (EVAL SG-SPECIAL-PDL-LIMIT))))
        (CHECK-PAGE-READ)
        (dispatch transport md)
        ((a-qlbndo) m-e)
        ((A-QLBNDRH) add m-e a-s)
        ((A-QLBNDH) A-QLBNDRH)
        ((M-TEM) Q-POINTER READ-MEMORY-DATA)
        (POPJ-AFTER-NEXT POPJ-GREATER-THAN M-TEM A-s)
       ((A-QLBNDH) ADD M-e A-TEM)

;PDL BUFFER "PHASING".  IF A STACK-GROUP IS INTERRUPTED AND LATER RESUMED,
;  IT HAS BEEN DECIDED TO PRESERVE THE "PHASING" OF THE PDL-BUFFER.
;  THIS MEANS THAT (FOR EXAMPLE) M-AP, A-IPMARK, PP, ETC, WILL HAVE THE
;  SAME OCTAL VALUES AS THEY DID (NOT MERELY POINT TO THE SAME QS, ETC).
;  IF THIS WERE NOT DONE, ONE COULD NOT "HOLD" A PDL-BUFFER INDEX ACROSS A
;  POSSIBLE PAGE-FAULT BOUNDARY.  ALTHOUGH THAT MIGHT BE A LIVABLE-WITHABLE RESTRICTION,
;  IT SEEMS WORTH IT TO AVOID THAT CLASS OF POSSIBLE BUGS.

;CHANGE STACK-GROUP STATE TO ACTIVE.  RETURN IN M-TEM PREVIOUS STATE.  IF L-B-P WAS
; SWAPPED, SWAP IT BACK.
SGENT   (CALL-XCT-NEXT SG-LOAD-STATIC-STATE)
       ((M-STACK-GROUP-SWITCH-FLAG) DPB (M-CONSTANT -1) A-FLAGS)
        ((VMA) A-QCSTKG)
        ((VMA-START-READ) SUB output-selector-mask-25
                VMA (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative)
                                      2
                                      (EVAL SG-PDL-PHASE))))
        (CHECK-PAGE-READ)                       ;NO TRANSPORT SINCE IT'S A FIXNUM
        ((PDL-BUFFER-POINTER) READ-MEMORY-DATA)         ;RESTORE PP WITH CORRECT PHASING
        ((M-1) Q-POINTER READ-MEMORY-DATA)
        ((A-PDL-BUFFER-HEAD) ADD M-1 (A-CONSTANT 1))    ;POINTS AT PDL-BUFFER LOCN WITH VALID
                                                ;DATA (IE NONE YET)
        (CALL-XCT-NEXT SG-L-P-B-1)
       ((M-ZR) ADD M-ZERO               ;-1 BECAUSE ONE FROB ALREADY HACKED, BUT +1 BECAUSE
           (A-CONSTANT (DIFFERENCE (EVAL SG-PDL-PHASE) (EVAL SG-STATE))))  ;WANT PHASE-STATE+1

        ((A-SG-STATE) C-PDL-BUFFER-POINTER-POP)
        ((A-SG-PREVIOUS-STACK-GROUP) C-PDL-BUFFER-POINTER-POP)
        ((A-SG-CALLING-ARGS-POINTER) C-PDL-BUFFER-POINTER-POP)
        ((A-SG-CALLING-ARGS-NUMBER) C-PDL-BUFFER-POINTER-POP)
;       ((A-SG-FOLLOWING-STACK-GROUP) C-PDL-BUFFER-POINTER-POP)
        ((A-GARBAGE) C-PDL-BUFFER-POINTER-POP)  ;**available for recycle. was TRAP-AP-LEVEL.
                ;GET PDL-BUFFER RELOAD POINTER BACK INTO PHASE
        ((M-1) ADD A-QLPDLO C-PDL-BUFFER-POINTER-POP ALU-CARRY-IN-ONE)  ;V.A. OF P.B. LOCN W/
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) Q-POINTER M-1)          ;VALID DATA (IE NONE YET).
        ((M-1) ADD A-QLBNDO C-PDL-BUFFER-POINTER-POP)
        ((A-QLBNDP) Q-POINTER M-1)
        (CALL-XCT-NEXT GET-PDL-BUFFER-INDEX)
       ((M-K) ADD C-PDL-BUFFER-POINTER-POP A-QLPDLO)
        ((M-AP pdl-index) M-K)                                  ;RESTORE M-AP
        (CALL-XCT-NEXT GET-PDL-BUFFER-INDEX)
       ((M-K) ADD C-PDL-BUFFER-POINTER-POP A-QLPDLO)
        ((A-IPMARK) M-K)                                ;RESTORE A-IPMARK
        ((M-LAST-MICRO-ENTRY) MICRO-STACK-DATA-POP)     ;SAVE RETURN TO SGENT

;COMPUTE LENGTH OF ACTIVE FRAME AND LOAD MINUS THAT INTO M-PDL-BUFFER-ACTIVE-QS.
;THEN PDL-BUFFER-REFILL WILL RELOAD ENTIRE ACTIVE FRAME PLUS PDL-BUFFER-LOW-WARNING
;WORTH OF OTHER STUFF.
        ((M-1) ADD (M-CONSTANT -1) A-PDL-BUFFER-HEAD)   ;WHAT PP WILL BE WHEN ALL THIS IS OVER
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-AP A-1)         ;GET - LENGTH OF ACTIVE FRAME MODULO
        (JUMP-EQUAL M-PDL-BUFFER-ACTIVE-QS A-ZERO SGENT1)  ;P.B. WRAPAROUND PROBLEMS
                ;THE 10. below is OK even on lambda since we are reloading one frame
                ; the maximum size of which is 8 bits worth.
        ((M-PDL-BUFFER-ACTIVE-QS) (BYTE-FIELD 10. 0) M-PDL-BUFFER-ACTIVE-QS
                 (A-CONSTANT -1))               ;EXTEND SIGN TO MAKE REAL NEG NUMBER.
SGENT1  (CALL PDL-BUFFER-REFILL)                ;REFILL PDL-BUFFER WITH GOOD STUFF
        (CALL-GREATER-THAN M-ZERO A-PDL-BUFFER-HIGH-WARNING TRAP)  ;LOSEY LOSEY IT CANT EVEN
   (ERROR-TABLE PDL-OVERFLOW REGULAR)                              ; HOLD 1 MAXIMUM SIZE FRAME
;       ((PDL-BUFFER-INDEX) M-AP)               ;IF RUNNING MACRO-CODE, RESTORE MACRO PC
        ((m-fef) pdl-index-indirect)    ;do this after PDL-BUFFER-REFILL
        (call-data-type-equal m-fef (a-constant (byte-value q-data-type dtp-fef-pointer)) qllent)
;;; NOW SET UP THE CORRECT BASE OF THE MICRO-STACK
        ;;*** Next 2 lines are temporary
 ;      ((M-TEM) MICRO-STACK-POINTER)
 ;      (CALL-NOT-EQUAL M-TEM A-ZERO ILLOP-DEBUG)
        ;;*** End temporary code
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;POP OFF THE CURRENT BASE
        ((M-1) A-SG-STATE)                      ;GET THE STATE OF THIS STACK GROUP
        ((M-1) (LISP-BYTE %%SG-ST-INST-DISP) M-1)       ;READ OUT THE INSTRUCTION DISPATCH
        (jump-not-equal m-1 a-zero sg-alt-main-return)
        ((micro-stack-data-push) a-main-dispatch)  ;set up macro instruction return.
                  ;clear SINGLE-STEP-MACRO-INST-MODE if previously set.
#+lambda((rg-mode) andca rg-mode (a-constant 1_37))
#+exp   ((mcr) andca mcr (a-constant 1_26.)) ;really turn on the bit later...
sg-alt-main-x
;;; RESTORE THE REST OF THE SG'S MICRO-STACK
        ((PDL-BUFFER-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-EXS-MICRO-STACK-SAVED) C-PDL-BUFFER-INDEX QMMPOP)

        ((MICRO-STACK-DATA-PUSH) M-LAST-MICRO-ENTRY)    ;PUSH BACK RETURN FROM SGENT

        ((A-TRAP-MICRO-PC) C-PDL-BUFFER-POINTER-POP)

        ((A-QLARYH) C-PDL-BUFFER-POINTER-POP)
        ((A-QLARYL) C-PDL-BUFFER-POINTER-POP)
        ((M-FLAGS) C-PDL-BUFFER-POINTER-POP)

        ((M-1) A-SG-STATE)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%SG-ST-IN-SWAPPED-STATE) M-1
                SGENT2)         ;FALL THRU ON L-B-P SWAPPED
;*** should M-A get DTP-LOCATIVE here?
        ((M-A) A-QLBNDO)        ;POINTS TO FIRST WD OF FIRST BLOCK.
SGENT3  ((VMA-START-READ M-A) ADD M-A (A-CONSTANT 1))   ;IS 2ND WD OF BLOCK PNTR TO VALUE
        (CHECK-PAGE-READ)                               ; CELL?
        (JUMP-GREATER-THAN M-A A-QLBNDP SGENT4)         ;XFER ON THRU
        (JUMP-IF-BIT-SET (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG)
                         READ-MEMORY-DATA SGENT3)  ;MUST NOT BE 1ST WD OF BLOCK
                                                        ;IF IT IS, LOOP BACK FOR THAT BLOCK
        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-locative)) sgent6)
SGENT5  (DISPATCH TRANSPORT-bind READ-MEMORY-DATA)
;SGENT5 (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-C) READ-MEMORY-DATA)                        ;M-C HAS POINTER TO INTERNAL V.C.
        ((VMA-START-READ) SUB M-A (A-CONSTANT 1))       ;FIRST WD OF PAIR HOLDS INACTIVE
        (CHECK-PAGE-READ)                               ; BINDING
        (DISPATCH TRANSPORT-bind READ-MEMORY-DATA)
;       (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-D) READ-MEMORY-DATA)                        ;M-D HAS NEW VALUE BEING RESTORED
        ((VMA-START-READ) M-C)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-bind READ-MEMORY-DATA)
;       (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-ZR) READ-MEMORY-DATA)                       ;M-ZR HAS OLD VALUE BEING SAVED
        ((WRITE-MEMORY-DATA-START-WRITE) DPB M-D Q-TYPED-POINTER A-ZR)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        ((WRITE-MEMORY-DATA) DPB M-ZR Q-TYPED-POINTER A-D)
        ((VMA-START-WRITE) SUB M-A (A-CONSTANT 1))
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (JUMP-XCT-NEXT SGENT3)
       ((M-A) ADD M-A (A-CONSTANT 1))                   ;SPACE TO FIRST Q OF NEXT PAIR

SGENT6  ((VMA-START-READ M-A) ADD M-A (A-CONSTANT 1))  ;THIS NOT A BINDING BLOCK, SPACE OVER
        (CHECK-PAGE-READ)                              ; IT.
     ;No transport: we aren't looking at the pointer, and every branch eventually
     ;bashes md.
        (JUMP-IF-BIT-SET (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG)
                         READ-MEMORY-DATA SGENT3)  ;FOUND FIRST Q OF NEXT BLOCK
        (JUMP-GREATER-OR-EQUAL M-A A-QLBNDP SGENT4)
        (JUMP SGENT6)                                   ;KEEP LOOKING

SGENT4  ((A-SG-STATE) DPB M-ZERO (LISP-BYTE %%SG-ST-IN-SWAPPED-STATE) A-SG-STATE)
SGENT2
        ((M-1) (A-CONSTANT (M-MEM-LOC M-K)))            ;RESTORE REGS
SGENT0  ((OA-REG-LOW) DPB M-1 OAL-M-DEST A-ZERO)
        ((M-GARBAGE) C-PDL-BUFFER-POINTER-POP)          ;M-GARBAGE = 0@M
        (JUMP-GREATER-THAN-XCT-NEXT M-1 (A-CONSTANT (M-MEM-LOC M-ZR)) SGENT0)
       ((M-1) SUB M-1 (A-CONSTANT 1))
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-STACK-GROUP-SWITCH-ENABLE)
                M-METER-ENABLES METER-SG-ENTER)

;; The QMRCL dispatch table and the related A-DEFAULT-CALL-STATE are hacked in the
;; following order: (1) they are initialized to the straight fast case; (2) if metering
;; is enabled, QLENTR-METER is installed; (3) if trap-on-call is enabled, QLENTR-TRAP-ON-CALL
;; is installed.  This ordering legislates that trap-on-call takes priority over metering.
;; I think this reasonable.

        ((a-default-call-state) (a-constant (byte-value q-data-type dtp-fix)))
#+lambda((d-qmrcl-qlentr) (a-constant (plus 1_19. (i-mem-loc qlentr)))) ;dispatch-start-mem-read
#+exp   ((m-tem) (a-constant (i-mem-loc qlentr)))
#+exp   (dispatch write-dispatch-memory d-qmrcl-qlentr (byte-field 0 0) (i-arg (a-mem-loc a-tem)))
        (call-if-bit-set m-meter-stack-group-enable set-qlentr-meter-on)
        (call-if-bit-set m-trap-on-calls x-trap-on-next-call)

        ((M-2) C-PDL-BUFFER-POINTER-POP)                ;RESTORE POINTER FIELDS OF M-1,M-2
        ((M-1) C-PDL-BUFFER-POINTER-POP)
        ((M-3) (BYTE-FIELD 8 16.) C-PDL-BUFFER-POINTER) ;THEN RESTORE THEIR TAG FIELDS
        ((M-2) DPB M-3 Q-ALL-BUT-POINTER A-2)
        ((M-3) (BYTE-FIELD 8 8) C-PDL-BUFFER-POINTER)
        ((M-1) DPB M-3 Q-ALL-BUT-POINTER A-1)
        ((M-3) DPB C-PDL-BUFFER-POINTER-POP Q-ALL-BUT-POINTER A-ZERO) ;TAG FOR VMA

        ((M-4) A-QCSTKG)                                ;CHANGE SG-STATE TO ACTIVE
        ((VMA) SUB output-selector-mask-25
                M-4 (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative) 2 (EVAL SG-STATE))))
        ((M-4) (A-CONSTANT (EVAL SG-STATE-ACTIVE)))
        ((WRITE-MEMORY-DATA-START-WRITE M-4)
                DPB M-4 (LISP-BYTE %%SG-ST-CURRENT-STATE) A-SG-STATE)
        (CHECK-PAGE-WRITE)
 ;      ((VMA-START-READ) Q-POINTER C-PDL-BUFFER-POINTER-POP A-3)     ;RESTORE VMA AND MD
 ;      (CHECK-PAGE-READ)
        ((vma) seta (a-constant 177377400) pdl-pop)  ;VMA not restored! This will trap if ref'ed.
        ((md) a-zero)           ;not restored either!  Minimize randomness.
        ((M-STACK-GROUP-SWITCH-FLAG) DPB (M-CONSTANT 0) A-FLAGS)
        (CALL-IF-BIT-SET M-DEFERRED-SEQUENCE-BREAK-FLAG SB-REINSTATE)
        (POPJ-AFTER-NEXT                                ;PGF-x SMASHES M-TEM, DELAY LOADING
                (M-TEM) DPB M-ZERO (ALL-BUT-LISP-BYTE %%SG-ST-CURRENT-STATE) A-SG-STATE)
       ((A-SG-STATE) M-4)


;; High-level stack group stuff.

;; Takes a stack group in M-2, returns the SG-STATE word in M-TEM,
;; and the state subfield in M-1.
GET-SG-STATE
        ((VMA-START-READ) SUB output-selector-mask-25
                M-2 (A-CONSTANT (PLUS (byte-value q-data-type dtp-locative) 2 (EVAL SG-STATE))))
        (CHECK-PAGE-READ)                       ;NO TRANSPORT SINCE IT'S A FIXNUM
        (POPJ-AFTER-NEXT
         (M-TEM) READ-MEMORY-DATA)
      ((M-1) (LISP-BYTE %%SG-ST-CURRENT-STATE) READ-MEMORY-DATA)

(LOCALITY D-MEM)
(START-DISPATCH 4 0)
TRAP-ON-BAD-SG-STATE
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-ERROR
        (P-BIT TRAP INHIBIT-XCT-NEXT-BIT)       ;SG-STATE-ACTIVE
        (P-BIT R-BIT)                           ;SG-STATE-RESUMABLE
        (P-BIT R-BIT)                           ;SG-STATE-AWAITING-RETURN
        (P-BIT R-BIT)                           ;SG-STATE-INVOKE-CALL-ON-RETURN
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-INTERRUPTED-DIRTY
        (P-BIT TRAP INHIBIT-XCT-NEXT-BIT)       ;SG-STATE-AWAITING-ERROR-RECOVERY
        (P-BIT R-BIT)                           ;SG-STATE-AWAITING-CALL
        (P-BIT R-BIT)                           ;SG-STATE-AWAITING-INITIAL-CALL
        (P-BIT TRAP INHIBIT-XCT-NEXT-BIT)       ;SG-STATE-EXHAUSTED
(REPEAT 6 (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT))
(END-DISPATCH)
(LOCALITY I-MEM)

;;     This routine handles a stack group's being called as a function;
;; it is reached from the D-QMRCL dispatch.  Thus, M-A contains the new stack group.
;;  First, error checking: if both SG's are SAFE, then the called one has to be
;; in the AWAITING-CALL or AWAITING-INITIAL-CALL state.
SG-CALL
  ;     (CALL FINISH-ENTERED-FRAME)
        (CALL-XCT-NEXT GET-SG-STATE)        ;GET STATE OF SG GOING TO.  ALSO USE THIS BELOW.
       ((M-2) M-A)
        (DISPATCH (BYTE-FIELD 4 0) M-1 TRAP-ON-BAD-SG-STATE)
    (ERROR-TABLE WRONG-SG-STATE M-A)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%SG-ST-SAFE) M-TEM SG-CALL-1)
        (JUMP-EQUAL M-1 (A-CONSTANT (EVAL SG-STATE-AWAITING-CALL)) SG-CALL-1)
        ((M-2) A-SG-STATE)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%SG-ST-SAFE) M-2 SG-CALL-1)
        (CALL-NOT-EQUAL M-1 (A-CONSTANT (EVAL SG-STATE-AWAITING-INITIAL-CALL)) TRAP)
    (ERROR-TABLE WRONG-SG-STATE M-A)
SG-CALL-1
        ((M-B) M-TEM)           ;Save SG-STATE of SG going to
;; Set up the argument list.  This doesn't handle LEXPR/FEXPR calls!
        ((A-SG-TEM) A-V-NIL)
        (JUMP-EQUAL-XCT-NEXT M-R A-ZERO SG-CALL-2)
       ((A-SG-TEM1) A-V-NIL)                    ; No args, list is NIL.
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K PDL-BUFFER-INDEX) ADD M-AP (A-CONSTANT 1))         ; List pointer to arg list.
        ((A-SG-TEM1) M-K)
        ((A-SG-TEM) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
SG-CALL-2
;; Leave old SG in awaiting-return, and don't swap if both of these bits are off.
        ((M-2) (A-CONSTANT (EVAL SG-STATE-AWAITING-RETURN)))
        ((M-C) A-SG-STATE)
        (JUMP-IF-BIT-SET (LISP-BYTE %%SG-ST-SWAP-SV-ON-CALL-OUT) M-C SG-CALL-3)
        (JUMP-IF-BIT-SET (LISP-BYTE %%SG-ST-SWAP-SV-OF-SG-THAT-CALLS-ME) M-B SG-CALL-3)
        ((M-2) DPB (M-CONSTANT -1) (BYTE-FIELD 1 6) A-2)        ;Set 100 bit; don't swap L-B-P
SG-CALL-3
        ((VMA) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))  ;VMA NOT IMPORTANT IN
                        ;THIS PATH, FLUSH ANY GARBAGE.  CRUFT POSSIBLE VIA PATH FROM
                        ;XUWR2, AT LEAST.
        (CALL-XCT-NEXT SGLV)                    ; Leave!
       ((M-TEM) M-2)   ;M-TEM has the new state, plus 100 bit says to not swap L-B-P.
;; Drops through.

;; More high-level stack group stuff.

;; Drops in.

SG-ENTER
; This is the common routine for activating a new stack group.  It takes the following
; things:  the new stack group itself in M-A, the transmitted value in A-SG-TEM,
; the argument list in A-SG-TEM1, and the argument count in M-R.
        ((A-SG-TEM2) A-QCSTKG)
        (CALL-XCT-NEXT SGENT)
       ((A-QCSTKG) M-A)
        ((A-SG-PREVIOUS-STACK-GROUP) A-SG-TEM2)
SG-ENTER-1
        ((A-SG-CALLING-ARGS-POINTER) A-SG-TEM1)
        ((A-SG-CALLING-ARGS-NUMBER) DPB M-R Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;; Now dispatch to separate routines, based on what state the new SG is in.
;; SGENT left that state in M-TEM.  It only dispatches on the low four bits
;; of the state because there are only 10. states implemented, and although
;; the state is a 6 bit field, it would waste lot of D-MEM to make the table
;; that large.
        (DISPATCH (BYTE-FIELD 4 0) M-TEM D-SG-ENTER)
       ((M-T) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-SG-TEM)      ;SOMETIMES executes next!!

(LOCALITY D-MEM)
(START-DISPATCH 4 0)
D-SG-ENTER
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-ERROR
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-ACTIVE
        (R-BIT INHIBIT-XCT-NEXT-BIT)            ;SG-STATE-RESUMABLE
        (QMDDR0)                                ;SG-STATE-AWAITING-RETURN
        (SG-ENTER-CALL INHIBIT-XCT-NEXT-BIT)    ;SG-STATE-INVOKE-CALL-ON-RETURN
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-INTERRUPTED-DIRTY
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-AWAITING-ERROR-RECOVERY
        (R-BIT)                                 ;SG-STATE-AWAITING-CALL
        (SG-ENTER-CALL INHIBIT-XCT-NEXT-BIT)    ;SG-STATE-AWAITING-INITIAL-CALL
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SG-STATE-EXHAUSTED
(REPEAT 6 (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT))
(END-DISPATCH)
(LOCALITY I-MEM)

SG-ENTER-CALL
;; This is similar to QMRCL, but it never does a "leave".
        ((M-S) M-AP)
        ((PDL-BUFFER-INDEX M-AP) A-IPMARK)
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)              ;M-A := Function to call
        ((m-fef) m-a)
#+exp   ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER A-IPMARK)    ;Count arguments
#+exp   ((M-R) PDL-BUFFER-INDEX)
#+LAMBDA((M-R) SUB OUTPUT-SELECTOR-MASK-11
                PDL-BUFFER-POINTER A-IPMARK)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))      ;old finish-entered-frame
        ((M-TEM) C-PDL-BUFFER-INDEX)
        ((C-PDL-BUFFER-INDEX)  DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED) A-TEM)
        ((PDL-INDEX) SUB M-AP A-S)      ;Increment to M-AP (truncated to 10 bits)
        ((M-PDL-BUFFER-ACTIVE-QS) ADD PDL-INDEX A-PDL-BUFFER-ACTIVE-QS)
        (CALL-GREATER-THAN-XCT-NEXT M-PDL-BUFFER-ACTIVE-QS
                                    A-PDL-BUFFER-HIGH-WARNING PDL-BUFFER-DUMP)
        (DISPATCH-XCT-NEXT qmrcl-dispatch M-A)
       (NO-OP)

;; More high-level stack group stuff.
;; This page contains STACK-GROUP-RESUME, STACK-GROUP-RETURN,
;;  and the SG-ENTER-NO-PREV entry-point.

     (MISC-INST-ENTRY STACK-GROUP-RESUME)
SG-RESUME
        ((A-SG-TEM) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)   ; Get the value being transmitted.
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ; Get the destination SG.
        ((A-SG-TEM1) A-V-NIL)                   ; Argument list.
        (CALL-XCT-NEXT GET-SG-STATE)            ; Get state of destination SG.
       ((M-2) M-A)
        (DISPATCH (BYTE-FIELD 4 0) M-1 TRAP-ON-BAD-SG-STATE)
    (ERROR-TABLE WRONG-SG-STATE A-SG-PREVIOUS-STACK-GROUP)
        ((M-TEM) (A-CONSTANT (EVAL SG-STATE-AWAITING-CALL)))
        (CALL SGLV)
        (JUMP SG-ENTER-NO-PREV)


     (MISC-INST-ENTRY STACK-GROUP-RETURN)
SG-RETURN
        ((M-2) A-SG-STATE)
        (CALL-IF-BIT-CLEAR-XCT-NEXT (LISP-BYTE %%SG-ST-SAFE) M-2 TRAP)
    (ERROR-TABLE SG-RETURN-UNSAFE)
       ((A-SG-TEM1) A-V-NIL)                    ; Arg list (just in case...)
        (CALL-XCT-NEXT GET-SG-STATE)            ; Get state of prev. SG in M-1
       ((M-2) A-SG-PREVIOUS-STACK-GROUP)
        (CALL-EQUAL M-1 (A-CONSTANT (EVAL SG-STATE-AWAITING-CALL)) TRAP)
    (ERROR-TABLE WRONG-SG-STATE A-SG-PREVIOUS-STACK-GROUP)
        (CALL-EQUAL M-1 (A-CONSTANT (EVAL SG-STATE-AWAITING-INITIAL-CALL)) TRAP)
    (ERROR-TABLE WRONG-SG-STATE A-SG-PREVIOUS-STACK-GROUP)
        (DISPATCH (BYTE-FIELD 4 0) M-1 TRAP-ON-BAD-SG-STATE)
    (ERROR-TABLE WRONG-SG-STATE A-SG-PREVIOUS-STACK-GROUP)

SG-RETURN-1
        ((A-SG-TEM) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)   ; Get the value being transmitted.
        ((M-TEM) (A-CONSTANT (EVAL SG-STATE-AWAITING-CALL)))
SG-RETURN-2  ; Entrypoint from QMXSG, which is where we exit the top of a stack group.
        (CALL SGLV)
        ((M-A) A-SG-PREVIOUS-STACK-GROUP)
;; Falls into:


; This is like SG-ENTER (q.v.) except that it doesn't set up the PREVIOUS-STACK-GROUP
; at all, and so it takes no arg in A-SG-TEM2.
SG-ENTER-NO-PREV
        (CALL-XCT-NEXT SGENT)
       ((A-QCSTKG) M-A)
        (JUMP SG-ENTER-1)

;using a funny main macro return.  set single-stepping macro inst flag so hardware will
; always return to main loop between halfwords.
sg-alt-main-return
        ((M-1) ADD M-1 (A-CONSTANT (A-MEM-LOC A-MAIN-DISPATCH)))
        ((OA-REG-HIGH) DPB M-1 A-ZERO OAH-A-SRC)
        ((MICRO-STACK-DATA-PUSH) A-GARBAGE)     ;PUSH THE BASE ADDRESS
        (jump-xct-next sg-alt-main-x)
#+lambda((rg-mode) ior rg-mode (a-constant 1_37))
#+exp   ((mcr) andca mcr (a-constant 1_26.))

))

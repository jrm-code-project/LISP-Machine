;;; Lisp Machine Microcode -*- Mode: Lisp; Base: 8; readtable: ZL -*-

;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; ** (c) Enhancements Copyright 1984,1985,1986 Lisp Machine, Inc. **

;       HERE COMES A MAN
;       TO LEAD YOU TO
;       YOUR VERY OWN MACHINE!
;               TOMMY, THE WHO

; "Microprogramming is a wasteland of time and too dependent on
;       the technology of implementation."  --Gordon Bell

(DEFCONST UC-PARAMETERS  '(

;Notes re the ERROR-TABLE pseudo-op:
;  If the error list contains the symbol CALLS-SUB, the entry serves only
;to mark an interesting item for the user on the micro-stack.  Accordingly,
;the entry should be positioned to match with whats on the stack at the time of
;the error. In particular, it should be after the following instruction in case of
;CALL-XCT-NEXT, etc.  No EHS- properties or routines are associated in this case.
;  Otherwise, the entry is intended to match with trap location saved at location
;TRAP.  In this case it should also come after an XCT-NEXT'ed instruction if there is any.
;  These entries (may well)
;coordinate with error recovery/printout routines in LMWIN;EHR >.  So check
;EHR when making changes in the vicinity of an ERROR-TABLE which does not
;have a CALLS-SUB.  In particular, if the ERROR-TABLE entry is proceedable or to be made
;proceedable, make sure relevant quantities are all held in ACs that get saved
;(M-A, etc) as opposed to ones that dont (M-3, A-TEM1 etc).

;  To mark a point in the code to which proceed-routines may want to go,
;put an (ERROR-TABLE RESTART <name>) BEFORE the instruction.  <name> is any
;arbitrary symbol that does not conflict with any other used.

;Notes re the MC-LINKAGE pseudo-op:
; This mechanism provides linkage between microcompiled code and the main ULOAD.
;You say (MC-LINKAGE (<SYM> ..)) and the address within memory and memory of <SYM>s will
;be available to the microcompiled code loader and/or the assembler operating in incremental
;mode.  If the list element is non-atomic, CAR is the microcompiled symbol, CADR the UCADR
;symbol. Currently, this info is transferred via an entry in the assembler state written
;at the beginning of the SYM file, but it will probably be moved to the TBL file
;eventually.  All A or M memory symbols with values less than 40 are automatically
;MC-LINKAGEified.
;  As a consequence of this mechanism, all microcompiler "dependancy links"
;go through MISC-INST-ENTRYs or MC-LINKAGEs.  The main symbol table is not required.

(MC-LINKAGE ( (T M-T) (B M-B) (R M-R) (C M-C) (TEM M-TEM) (S M-S) (Q M-Q) (E M-E) (A M-A)
              (D M-D) (M-1 M-1) (M-2 M-2)
              (array-pointer m-array-pointer) (array-header m-array-header)
              (array-origin m-array-origin) (array-length m-array-length)
              (array-rank m-array-rank)
              A-ZERO A-V-NIL A-V-TRUE
              A-DISK-RUN-LIGHT
              SKIP-IF-ATOM SKIP-IF-NO-ATOM
              D-READ-EXIT-VECTOR D-READ-EXIT-VECTOR-AND-LOAD-T
              D-WRITE-EXIT-VECTOR D-CALL-EXIT-VECTOR
              D-SE1+ D-SE1- D-SECDR D-SECDDR
              D-MMCALL D-MMCALT D-MMCALB
              D-BNDPOP D-BNDNIL D-SETNIL D-SETZERO
              D-GET-LOCATIVE-TO-PDL D-GET-LOCATIVE-TO-VC D-POP-SPECPDL
              D-XCMV D-UCTOM D-MMISU D-MURV D-MRNV D-MR2V D-MR3V
              D-SUB-PP D-POP-SPECPDL-AND-SUB-PP D-DO-SPECBIND-PP-BASED
              QMADDD QMADD QMAD QMA QMDDDD QMDDD QMDD QMD QMAAAA QMAAA QMAA
              QMAAAD QMDDDA QMAADD QMAAD QMAADA QMDDAA QMDDA QMDDAD QMADAA QMADA
              QMADAD QMADDA QMDADA QMDAD QMDADD QMDAAA QMDAA QMDA QMDAAD
              QMEQL QMEQ QMLSP QMGRP
              XTCADD XTCSUB XTCMUL XTCDIV XTCAND XTCIOR XTCXOR   ;LAST-ARG-IN-T-ENTRYs
              XTNUMB XTLENG XTFIXP XTFLTP
              xt-set-ar1 xt-set-ar2 xt-set-ar3
              xtnot
              MC-SPREAD
              ;UCTO XMBIND
              XFLOOR-1-UC-ENTRY XFLOOR-2-UC-ENTRY
              ARRAY-DECODE-1 ARRAY-DECODE-1-A ARRAY-DECODE-1-D ARRAY-DECODE-1-DISPATCH
              ARRAY-TYPE-REF-DISPATCH ARRAY-TYPE-STORE-DISPATCH
              ARRAY-TYPE-STORE-DISPATCH-PUSHJ
              car-pre-dispatch-direct cdr-pre-dispatch-direct
              GAHDR GAHD1 DSP-ARRAY-SETUP
              decode-1d-array-uncached
              a-array-cache-invalid a-array-cache-valid a-general-array-cache-valid
              instance-invoke-get instance-invoke-getl instance-invoke-get-location-or-nil
              instance-invoke-car instance-invoke-cdr instance-invoke-set-car
              instance-invoke-set-cdr
    ;These for hand code (for now at least)
              TRAP-UNLESS-FIXNUM D-NUMARG D-FIXNUM-NUMARG2 TRAP ILLOP
              SCONS SCONS-D SCONS-T LCONS LCONS-D LIST-OF-NILS
              #+cadr FIXGET #+cadr FIXGET-1 FXGTPP FNORM
              FIXPACK-T FIXPACK-P FXUNPK-P-1 FXUNPK-T-2 M-T-TO-CPDL
              SFLPACK-T SFLPACK-P SFLUNPK-P-1 SFLUNPK-T-2
              FLOPACK-T FLOPACK-P FLOPACK GET-FLONUM
              FADD FSUB FMPY FDIV FEQL FGRP FLSP FMIN FMAX FSUB-REVERSE FDIV-REVERSE
              BNCONS BIGNUM-DPB-CLEANUP FLONUM-FIX SWAP-FLONUMS FNEG1 FNEG2
              MPY MPY12 DIV XFALSE XTRUE
              PGF-R PGF-W PGF-R-I PGF-W-I PGF-W-FORCE ;PGF-R-SB PGF-W-SB
              D-TRANSPORT D-ADVANCE-INSTRUCTION-STREAM D-GC-WRITE-TEST
              D-QMRCL A-IPMARK P3ZERO QMEX1
              CONVERT-PDL-BUFFER-ADDRESS GET-PDL-BUFFER-INDEX BITBLT-DECODE-ARRAY XAR2
))

(ASSIGN-EVAL NQZUSD (EVAL (- 32. (LENGTH Q-DATA-TYPES))))       ;# UNUSED DATA-TYPES
(ASSIGN-EVAL NATUSD (EVAL (- 32. (LENGTH ARRAY-TYPES))))        ;# UNUSED ARRAY-TYPES
    ;note:  ART-32B arrays have been "reverted" in the dispatch tables to ART-INUM for now.
    ;  Everything was updated, however the fonts need to be rehacked and that was too
    ; much trouble for now.  RG 6/19/86.
(ASSIGN-EVAL NHDUSD (EVAL (- 32. (LENGTH Q-HEADER-TYPES))))     ;# UNUSED HEADER-TYPES
(ASSIGN-EVAL VERSION-NUMBER (EVAL VERSION-NUMBER)) ;MAKE SOURCE VERSION A CONSLP SYMBOL

;THESE SYMBOLS GET DEFINED SUITABLY FOR USE IN BYTE INSTRUCTIONS
;DATA LOADED WITH THESE MUST COME FROM M BUS

(DEF-DATA-FIELD Q-CDR-CODE 2 36)
;(DEF-DATA-FIELD Q-FLAG-BIT 1 35)
(DEF-DATA-FIELD Q-CDR-CODE-LOW-BIT 1 36)
(DEF-DATA-FIELD Q-DATA-TYPE 5 31)
(DEF-DATA-FIELD Q-DATA-TYPE-PLUS-ONE-BIT 6 30)
(DEF-DATA-FIELD Q-POINTER 31 0)
(def-data-field q-immediate 31 0)
(DEF-DATA-FIELD Q-POINTER-WITHIN-PAGE 8 0)
(def-data-field q-page-number 17. 8.)
(def-data-field q-page-index 8 0)
(def-data-field q-header-type 5 23)
(def-data-field q-header-rest 23 0)

(DEF-DATA-FIELD Q-TYPED-POINTER 36 0)   ;POINTER+DATA-TYPE
(DEF-DATA-FIELD Q-ALL-BUT-TYPED-POINTER 2 36)
(DEF-DATA-FIELD Q-ALL-BUT-POINTER 7 31)
(DEF-DATA-FIELD Q-ALL-BUT-CDR-CODE 36 0)
(DEF-DATA-FIELD Q-ALL-BUT-POINTER-WITHIN-PAGE 30 8)

(ASSIGN Q-POINTER-WIDTH 25.)
(ASSIGN WORDS-TO-DIRECT-MAP-DURING-BOOTSTRAP (eval (* 900. 400)))
  ;must include all low hard-wired pages.
  ;must also include CCW-BUFFER to avoid faults from BUILD-CCW-LIST, etc.
  ;must also include data buffer for COMPUTE-REGION-CRC
  ;must include space for cold quantum-map to set up temporary quantum map before wired areas are loaded.

;Stuff for address space quantization
(DEF-DATA-FIELD VMA-QUANTUM-BYTE
        (EVAL (- 25. (1- (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE))))
        (EVAL (1- (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE))))
(DEF-DATA-FIELD ADDRESS-SPACE-MAP-WORD-INDEX-BYTE
        (EVAL (- 25. (+ (1- (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE))
                        (1- (HAULONG (// 32. %ADDRESS-SPACE-MAP-BYTE-SIZE))))))
        (EVAL (+ (1- (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE))
                 (1- (HAULONG (// 32. %ADDRESS-SPACE-MAP-BYTE-SIZE))))))
(DEF-DATA-FIELD ADDRESS-SPACE-MAP-BYTE-NUMBER-BYTE
        (EVAL (1- (HAULONG (// 32. %ADDRESS-SPACE-MAP-BYTE-SIZE))))
        (EVAL (1- (HAULONG %ADDRESS-SPACE-QUANTUM-SIZE))))
(DEF-DATA-FIELD ADDRESS-SPACE-MAP-BYTE-MROT
        (EVAL (- 5 (1- (HAULONG %ADDRESS-SPACE-MAP-BYTE-SIZE))))
        (EVAL (1- (HAULONG %ADDRESS-SPACE-MAP-BYTE-SIZE))))

(DEF-DATA-FIELD SIGN-BIT 1 31.)
(DEF-DATA-FIELD BOXED-SIGN-BIT 1 24.)  ;SIGN OF A BOXED FIXNUM
(DEF-DATA-FIELD BIT-ABOVE-BOXED-SIGN-BIT 1 25.)
(DEF-DATA-FIELD BOXED-NUM-EXCEPT-SIGN-BIT 24. 0)
(ASSIGN POSITIVE-SETZ 1_24.)    ;Largest value that fits in a fixnum, plus one
(ASSIGN NEGATIVE-SETZ -1_24.)   ;Smallest value that fits in a fixnum
(DEF-DATA-FIELD BITS-ABOVE-FIXNUM 7 31)  ;BITS NOT USED IN REPRESENTING FIXNUM.

;"INVOKE" OPS   ;GIVEN TO INVOKED ROUTINE TO TELL IT WHAT IS TRYING TO BE DONE TO IT
;These never got used.  DATA-TYPE-INVOKE-OP is the only one of these
;that actually got put in the code, except for CAR and CDR, etc.,
;where they have been replaced by INSTANCE-INVOKE-... symbols
;that actually do their jobs.
(ASSIGN DATA-TYPE-INVOKE-OP 6)

;EXTRA BITS ON MICRO STACK
#-cadr (begin-comment)
(DEF-DATA-FIELD %%-PPBMIR 1 14.)  ;FLAGS MACRO INSTRUCTION RETURN.  WHEN SEEN SET ON
                                  ;MICRO-POPJ, CAUSES INSTRUCTION STREAM HARDWARE TO
                                  ;FETCH NEXT MACROINSTRUCTION (IF NECESSARY).
(DEF-DATA-FIELD %%-PPBINF 2 15.)  ;IF THESE NON-ZERO, THEY SIGNAL PRESENCE OF ADDTL INFO
 (DEF-DATA-FIELD %%-PPBMIA 1 15.)       ;MICRO-TO-MICRO CALL
 (DEF-DATA-FIELD %%-PPBMAA 1 16.)       ;MICRO-TO-MACRO CALL
(DEF-DATA-FIELD %%-PPBSPC 1 17.)  ;DO BBLKP (POPPING A BLOCK OFF LINEAR BINDING PDL)
                ; ON EXIT FROM THIS FCTN. FCTN MUST EXIT TO CBBLKP OR MRNMV TO CAUSE
                ; THIS TO GET LOOKED AT.  ALSO, VARIOUS PDL GRUBLING ROUTINES
                ; LOOK AT THIS.
 #-cadr (end-comment)

#-lambda (begin-comment)
(DEF-DATA-FIELD %%-PPBMIR 1 18.)  ;FLAGS MACRO INSTRUCTION RETURN.  WHEN SEEN SET ON
                                  ;MICRO-POPJ, CAUSES INSTRUCTION STREAM HARDWARE TO
                                  ;FETCH NEXT MACROINSTRUCTION (IF NECESSARY).
(DEF-DATA-FIELD %%-PPBINF 2 16.)  ;IF THESE NON-ZERO, THEY SIGNAL PRESENCE OF ADDTL INFO
 (DEF-DATA-FIELD %%-PPBMIA 1 16.)       ;MICRO-TO-MICRO CALL
 (DEF-DATA-FIELD %%-PPBMAA 1 17.)       ;MICRO-TO-MACRO CALL
(DEF-DATA-FIELD %%-PPBSPC 1 19.)  ;DO BBLKP (POPPING A BLOCK OFF LINEAR BINDING PDL)
                ; ON EXIT FROM THIS FCTN. FCTN MUST EXIT TO CBBLKP OR MRNMV TO CAUSE
                ; THIS TO GET LOOKED AT.  ALSO, VARIOUS PDL GRUBLING ROUTINES
                ; LOOK AT THIS.
 #-lambda (end-comment)

#-exp (begin-comment)
(DEF-DATA-FIELD %%-PPBMIR 1 14.)  ;FLAGS MACRO INSTRUCTION RETURN.  WHEN SEEN SET ON
                                  ;MICRO-POPJ, CAUSES INSTRUCTION STREAM HARDWARE TO
                                  ;FETCH NEXT MACROINSTRUCTION (IF NECESSARY).
(DEF-DATA-FIELD %%-PPBINF 2 15.)  ;IF THESE NON-ZERO, THEY SIGNAL PRESENCE OF ADDTL INFO
 (DEF-DATA-FIELD %%-PPBMIA 1 15.)       ;MICRO-TO-MICRO CALL
 (DEF-DATA-FIELD %%-PPBMAA 1 16.)       ;MICRO-TO-MACRO CALL
(DEF-DATA-FIELD %%-PPBSPC 1 17.)  ;DO BBLKP (POPPING A BLOCK OFF LINEAR BINDING PDL)
                ; ON EXIT FROM THIS FCTN. FCTN MUST EXIT TO CBBLKP OR MRNMV TO CAUSE
                ; THIS TO GET LOOKED AT.  ALSO, VARIOUS PDL GRUBLING ROUTINES
                ; LOOK AT THIS.
(def-data-field %%-ppbnop 1 19.)  ;no-op bits gets saved here during abort sequence.
 #-exp (end-comment)

(ASSIGN *CATCH-U-CODE-ENTRY-/# 0)       ;MUST KNOW ABOUT THESE WHEN GRUBBLING STACK..
                                        ; NOTE U-CODE ENTRY #'S ARE NORMALLY
                                        ; UNCONSTRAINED AND DETERMINE ONLY POSITION
                                        ; IN MICRO-CODE-ENTRY-AREA, ETC.

;DESTINATION CODES
(ASSIGN D-IGNORE 0)             ;Ignore except put into indicators.
(ASSIGN D-PDL 1)                ;Push onto stack
(ASSIGN D-RETURN 2)             ;Return as value of this function
(ASSIGN D-LAST 3)               ;Push onto stack as last argument to function call.
(ASSIGN D-MICRO 4)              ;Appears only in call-blocks.  Return to microcode.

;Location of first page after MICRO-CODE-SYMBOL-AREA.  Used in DISK-RESTORE and DISK-SAVE.
(ASSIGN END-OF-MICRO-CODE-SYMBOL-AREA (eval (* 9 400)))
(assign end-of-micro-code-symbol-area-in-pages 9)

;INDICES IN THE SUPPORT VECTOR
; CAUTION! After a callout, the macro-pc will be restored and the needfetch bit set.
;However, the macro-ir itself is not restored!  This can cause lossage if it is needed
;to supply a destination to store into, etc!
(ASSIGN SVC-NAMED-STRUCTURE-INVOKE 1) ;FUNCALL of a named-structure which isn't a hash table.
(ASSIGN SVC-DEFSTRUCT-DESCRIPTION 2)  ;pointer to symbol SI:DEFSTRUCT-DESCRIPTION
                                      ; for TYPEP of named structures.
(ASSIGN SVC-APL 3)              ;APPLY-LAMBDA
(ASSIGN SVC-EQUAL 4)            ;Call out for EQUAL
(ASSIGN SVC-EXPT 6)             ;CALL OUT FOR EXPT
(ASSIGN SVC-NUM1 7)             ;Call out for numeric functions of one arg.
(ASSIGN SVC-NUM2 10)            ;Call out for numeric functions of two args.
(ASSIGN SVC-UNB 11)             ;Unbound marker (see QSTFE-MONITOR)
(ASSIGN SVC-FMETH 12)           ;Call out on failure to find method
(ASSIGN SVC-INSTANCE-INVOKE-VECTOR 13)  ;Symbol whose value is vector of operation keywords
(ASSIGN SVC-EQUALP 14)          ;Call out for EQUALP recursion.
(ASSIGN SVC-EQUALP-ARRAY 15)    ;Call out for EQUALP of non-string array.
(assign svc-unreconciled 16)    ;Call out for unreconciled pointer.
(assign svc-new-moby-region 17) ;Call out to make new region for moby area.

(ASSIGN INSTANCE-INVOKE-GET 0)  ;Indices in INSTANCE-INVOKE-VECTOR's value.
(ASSIGN INSTANCE-INVOKE-GETL 1) ;For example, element 1 is :GETL.
(ASSIGN INSTANCE-INVOKE-GET-LOCATION-OR-NIL 2)
(ASSIGN INSTANCE-INVOKE-CAR 3)
(ASSIGN INSTANCE-INVOKE-CDR 4)
(ASSIGN INSTANCE-INVOKE-SET-CAR 5)
(ASSIGN INSTANCE-INVOKE-SET-CDR 6)

;;; Meter information definition
(ASSIGN METER-OVERHEAD-LENGTH 7)        ;Standard overhead for a meter
(DEF-DATA-FIELD METER-LENGTH 16. 16.)   ;Event number in header
(DEF-DATA-FIELD METER-EVENT-NUM 16. 0)  ;Length in header


;Instructions for checking for page faults, interrupts, sequence breaks.
;One of these must appear after every instruction that starts a memory cycle.

;This one is used when referencing fixed areas that should be always wired and mapped.
;E.G. inside the page fault routines where a recursive page fault could not be allowed
(ASSIGN ILLOP-IF-PAGE-FAULT (PLUS CALL-CONDITIONAL PG-FAULT ILLOP))

;These two are what are normally used.  They check for page faults and
;interrupts (handled entirely in microcode), but not sequence breaks.
;NOTE!! If a interrupt occurrs on a write, the write will be reperformed after
; the interrupt has returned in order to recapture the L1 map bits for GC-WRITE-TEST.
; Use CHECK-PAGE-WRITE-NO-INTERRUPT if this could cause problems.
(assign check-page-read  (plus call-conditional pg-fault-or-interrupt pgf-r-i))
(ASSIGN CHECK-PAGE-WRITE (PLUS CALL-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-W-I))

;;; This was used to trap a bug on the explorer.  This is a brute force way
;;; to find bugs.  Reads are done more often than writes.
;(assign check-page-read (plus call poor-mans-mar))
;(assign check-page-write (plus call poor-mans-mar))

;;; Same as above for now.  Defined so we can note those places that will not be
;;; sequence-breakable in the new scheme of things.  3/19/85 KHS.  NOTE: see above re writes!
(ASSIGN CHECK-PAGE-READ-NO-SEQUENCE-BREAK (PLUS CALL-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-R-I))
(ASSIGN CHECK-PAGE-WRITE-NO-SEQUENCE-BREAK (PLUS CALL-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-W-I))

;This one is used when you want to be able to write a nominally read-only area,
;for instance in the transporter when it is fixing a pointer to oldspace.
;Not checking for interrupts is just to save code.
(ASSIGN CHECK-PAGE-WRITE-FORCE (PLUS CALL-CONDITIONAL PG-FAULT PGF-W-FORCE))

;This one is used for writing an old binding of a special variable
;back into the value cell, when a binding is being unbound.
;When writing into a location forwarded to A memory,
;it means that an old EVCP is no longer current even if
;the old binding being restored is not an EVCP itself.
;  **NOTE. This feature could not really win anyway, and is now just an error check.**
(ASSIGN CHECK-PAGE-WRITE-BIND (PLUS CALL-CONDITIONAL PG-FAULT PGF-W-BIND))

;These two are used when an interrupt is not allowed, either because we
;are inside the interrupt handler, because we are retrying a cycle in
;PGF-R/PGF-W, or because we don't want to let the interrupt handler change the map.
;** following not true now.
;Note well: these should be used only for references which may need to refill
; the map, but cannot take an actual disk page fault.  If a swap-in from disk
; happens, interrupts will be allowed while waiting for the page to come in;
; however, an interrupt cannot happen after the data has been copied from
; memory into the MD if this is used instead of CHECK-PAGE-READ.  **
(ASSIGN CHECK-PAGE-READ-NO-INTERRUPT (PLUS CALL-CONDITIONAL PG-FAULT PGF-R))
(ASSIGN CHECK-PAGE-WRITE-NO-INTERRUPT (PLUS CALL-CONDITIONAL PG-FAULT PGF-W))
;This one is used from inside the page-fault-handler to try again after some
;progress has been made.  Point is, it must not affect A-PGF-MODE.
(ASSIGN CHECK-PAGE-WRITE-RETRY (PLUS CALL-CONDITIONAL PG-FAULT PGF-W-1))

(assign check-page-read-map-reload-only
        (plus call-conditional pg-fault pgf-r-map-reload-only))
(assign check-page-write-map-reload-only
        (plus call-conditional pg-fault pgf-w-map-reload-only))

;These two are used when we want to allow both interrupts and sequence breaks.
;Note that the VMA is NOT restored by stack group switches now!
;  These guys save in in M-T and restore it from there on resumption.  This is
;  the right thing for QCDR, etc.
(ASSIGN CHECK-PAGE-READ-SEQUENCE-BREAK-saving-vma-in-m-t-if-sequence-break
        (PLUS CALL-CONDITIONAL PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK PGF-R-SB-save-vma-in-t))
; This one is not used.
;(ASSIGN CHECK-PAGE-WRITE-SEQUENCE-BREAK
;       (PLUS CALL-CONDITIONAL PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK PGF-W-SB))

; writes of unboxed data should use this one.  For now, it matters only on
;writes to moby-space which trap out to macro-code, etc.
(assign check-page-write-unboxed
        (plus call-conditional pg-fault pgf-w-unboxed))

;These names are for use with the CALL-CONDITIONAL and JUMP-CONDITIONAL
;instructions when special circumstances dictate special handling
(ASSIGN PG-FAULT JUMP-ON-PAGE-FAULT-CONDITION)
(ASSIGN NO-PG-FAULT (logxor JUMP-ON-PAGE-FAULT-CONDITION INVERT-JUMP-SENSE))
(ASSIGN PG-FAULT-OR-INTERRUPT JUMP-ON-PAGE-FAULT-OR-INTERRUPT-PENDING-CONDITION)
(ASSIGN NO-PG-FAULT-OR-INTERRUPT
        (logxor JUMP-ON-PAGE-FAULT-OR-INTERRUPT-PENDING-CONDITION INVERT-JUMP-SENSE))
(ASSIGN PG-FAULT-INTERRUPT-OR-SEQUENCE-BREAK
        JUMP-ON-PAGE-FAULT-OR-INTERRUPT-PENDING-OR-SEQUENCE-BREAK-CONDITION)


#+CADR (DEF-DATA-FIELD OAL-BYTL-1 5 5)          ;MICRO INSTRUCTION FIELDS
#+CADR (DEF-DATA-FIELD OAL-MROT 5 0)
#+CADR (DEF-DATA-FIELD OAH-A-SRC 10. 6)
#+CADR (DEF-DATA-FIELD OAH-M-SRC 6 0)
#+cadr (DEF-DATA-FIELD OAL-DEST 12. 14.)    ;THIS DEFINITION DOES NOT WIN FOR FUNCTIONAL DESTINATIONS
#+cadr (DEF-DATA-FIELD OAL-A-DEST 10. 14.)  ;USE THIS WHEN SUPPLYING AN A-MEMORY DESTINATION
#+cadr (DEF-DATA-FIELD OAL-A-DEST-10-BITS 10. 14.)  ; for "compatibility" with LAMBDA.
#+cadr (DEF-DATA-FIELD OAL-M-DEST  5. 14.)  ;USE THIS WHEN SUPPLYING AN M-MEMORY DESTINATION
#+cadr (DEF-DATA-FIELD OAL-JUMP 14. 12.)
#+cadr (DEF-DATA-FIELD OAL-DISP 11. 12.)        ;In OAL on CADR, in OAH on LAMBDA
#+CADR (DEF-DATA-FIELD OAH-DISP-DISPATCH-CONSTANT 10. 6)
#+CADR (DEF-DATA-FIELD OAL-ALUF 4 3)

#+LAMBDA (DEF-DATA-FIELD OAL-BYTL-1 6 6)                ;MICRO INSTRUCTION FIELDS
#+LAMBDA (DEF-DATA-FIELD OAL-MROT 6 0)
#+lambda (def-data-field oal-mrot*2 5 1)
#+LAMBDA (DEF-DATA-FIELD OAH-A-SRC 12. 7)
#+LAMBDA (DEF-DATA-FIELD OAH-A-SRC-10-BITS 10. 7)  ;ADDRESSES A SOURCES IN BOTTOM PAGE.
#+LAMBDA (DEF-DATA-FIELD OAH-A-SRC-8-BITS 8. 7)    ;used to address inverse map for CRAM.
#+LAMBDA (DEF-DATA-FIELD OAH-A-SRC-6-BITS 6 7)
#+LAMBDA (DEF-DATA-FIELD OAH-A-SRC-4-BITS 4 7)
#+LAMBDA (DEF-DATA-FIELD OAH-M-SRC 7 0)
#+LAMBDA (DEF-DATA-FIELD OAL-DEST 13. 14.)    ;DOES NOT WIN FOR FUNCTIONAL DESTINATIONS
#+LAMBDA (DEF-DATA-FIELD OAL-A-DEST 12. 14.)  ;USE THIS WHEN SUPPLYING AN A-MEMORY DESTINATION
#+LAMBDA (DEF-DATA-FIELD OAL-A-DEST-10-BITS 10. 14.)  ;A-MEM MAPPED INTO VIRTUAL ADDRESS SPACE
#+LAMBDA (DEF-DATA-FIELD OAL-A-DEST-8-BITS 8. 14.) ;used to address inverse map for CRAM.
#+LAMBDA (DEF-DATA-FIELD OAL-A-DEST-6-BITS 6. 14.)
#+LAMBDA (DEF-DATA-FIELD OAL-A-DEST-4-BITS 4. 14.)
#+LAMBDA (DEF-DATA-FIELD OAL-M-DEST  6. 14.)  ;USE THIS WHEN SUPPLYING AN M-MEMORY DESTINATION
#+LAMBDA (DEF-DATA-FIELD OAL-JUMP 16. 14.)
#+LAMBDA (DEF-DATA-FIELD OAH-DISP 12. 7.)       ;In OAL on CADR, in OAH on LAMBDA
#+LAMBDA (DEF-DATA-FIELD OAL-DISP-DISPATCH-CONSTANT 12. 14.)  ;In OAH on CADR
#+LAMBDA (DEF-DATA-FIELD OAL-ALUF 4 3)

#+EXP (DEF-DATA-FIELD OAL-BYTL-1 5 5)   ;on explorer, this is really oal-bytl, with the
                                        ; exception that 0 = 32.
                ;MICRO INSTRUCTION FIELDS
#+EXP (DEF-DATA-FIELD OAL-MROT 5 0)
#+EXP (DEF-DATA-FIELD OAL-MROT*2 4 1)
#+EXP (DEF-DATA-FIELD OAH-A-SRC 10. 0)
#+EXP (DEF-DATA-FIELD OAH-A-SRC-10-BITS 10. 0)  ;ADDRESSES A SOURCES IN BOTTOM PAGE.
#+EXP (DEF-DATA-FIELD OAH-A-SRC-8-BITS 8. 0)    ;used to address inverse map for CRAM.
#+EXP (DEF-DATA-FIELD OAH-A-SRC-6-BITS 6 0)
#+EXP (DEF-DATA-FIELD OAH-A-SRC-4-BITS 4 0)
#+EXP (DEF-DATA-FIELD OAH-M-SRC 7 10.)
#+EXP (DEF-DATA-FIELD OAL-DEST 13. 19.)    ;DOES NOT WIN FOR FUNCTIONAL DESTINATIONS
#+EXP (DEF-DATA-FIELD OAL-A-DEST 10. 19.)  ;USE THIS WHEN SUPPLYING AN A-MEMORY DESTINATION
#+EXP (DEF-DATA-FIELD OAL-A-DEST-10-BITS 10. 19.)  ;A-MEM MAPPED INTO VIRTUAL ADDRESS SPACE
#+EXP (DEF-DATA-FIELD OAL-A-DEST-8-BITS 8. 19.) ;used to address inverse map for CRAM.
#+EXP (DEF-DATA-FIELD OAL-A-DEST-6-BITS 6. 19.)
#+EXP (DEF-DATA-FIELD OAL-A-DEST-4-BITS 4. 19.)
#+EXP (DEF-DATA-FIELD OAL-M-DEST  6. 19.)  ;USE THIS WHEN SUPPLYING AN M-MEMORY DESTINATION
#+EXP (DEF-DATA-FIELD OAL-JUMP 14. 18.)
#+EXP (DEF-DATA-FIELD OAL-DISP 11. 20.) ;In OAL on CADR, in OAH on LAMBDA, OAL on EXP
#+EXP (DEF-DATA-FIELD OAH-DISP-DISPATCH-CONSTANT 10. 0.)  ;In OAH on CADR, OAL on LAMBDA
#+EXP (DEF-DATA-FIELD OAL-ALUF 4 3)


(ASSIGN PDL-BUFFER-LOW-WARNING 20.)  ;MAX LENGTH BASIC FRAME + ADI
(ASSIGN PDL-BUFFER-SLOP 40.)    ;NUMBER OF EXTRA WORDS TO LEAVE "UNUSED" AT END OF PDL BUFFER
                                ;NOT TERRIBLY WELL ACCOUNTED, BUT 40 SHOULD BE MORE THAN ENOUGH
                                ;NOTE THAT CRUFT PUSHED BY SGLV SHOULD BE COUNTED IN THIS

;NUMBER OF BITS IN PP, PI REGISTERS.
#+CADR (DEF-DATA-FIELD PDL-BUFFER-ADDRESS-MASK 10. 0)
#+LAMBDA (DEF-DATA-FIELD PDL-BUFFER-ADDRESS-MASK 11. 0)
#+EXP  (DEF-DATA-FIELD PDL-BUFFER-ADDRESS-MASK 10. 0)

#+CADR (DEF-DATA-FIELD PDL-BUFFER-ADDRESS-HIGH-BIT 1 9.)
#+LAMBDA (DEF-DATA-FIELD PDL-BUFFER-ADDRESS-HIGH-BIT 1 10.)
#+EXP  (DEF-DATA-FIELD PDL-BUFFER-ADDRESS-HIGH-BIT 1 9.)

#+CADR (ASSIGN PDL-BUFFER-SIZE-IN-WORDS 2000)
#+LAMBDA (ASSIGN PDL-BUFFER-SIZE-IN-WORDS 4000)
#+EXP  (ASSIGN PDL-BUFFER-SIZE-IN-WORDS 2000)

;Max value for M-PDL-BUFFER-ACTIVE-QS.  This allows max size active frame.
(ASSIGN PDL-BUFFER-HIGH-LIMIT
        (DIFFERENCE PDL-BUFFER-SIZE-IN-WORDS (PLUS 400 PDL-BUFFER-SLOP)))

;MODIFIERS FOR THE DISPATCH INSTRUCTION, USED TO INVOKE THE TRANSPORTER
; ((VMA-START-READ) ---)
; (CHECK-PAGE-READ)
; ...
; (DISPATCH TRANSPORT-xxx) ;IF THIS DROPS THROUGH, OLD->NEW SPACE TRANSPORTATION
;                          ; HAS BEEN DONE AND INVISIBLE POINTERS HAVE BEEN FOLLOWED.
;                          ; VMA HAS THE ADDRESS, MD HAS THE CONTENTS.
; IT IS OK TO USE POPJ-AFTER-NEXT ON THE DISPATCH INSTRUCTION.
; DON'T USE POPJ-AFTER-NEXT OR JUMP-XCT-NEXT IN THE INSTRUCTION BEFORE
; A TRANSPORT.

;the transporter and the VMA.
;  TRANS-OLD
;    also TRANS-OLD0
;       assumes VMA has where MD came from.  That locn may be modified, VMA unchanged.
;  TRANS-UNRECONCILED
;  TRANS-EVCP
;    if forwards, VMA has new locn ref'ed.
;  TRANS-OQF
;    if forwards, VMA has new locn ref'ed.
;  TRANS-HFWD
;    if forwards, VMA has new locn ref'ed.
;  TRANS-BFWD
;    if forwards, VMA has new locn ref'ed.
;  TRANS-INDXF
;    if ref's array, VMA has new locn ref'ed.
;    if converts, VMA garbage, MD has OQF to where would have ref'ed.
;  TRANS-SRP
;     VMA has new locn ref'ed.
;  TRANS-HFWD (rplacd-forward)

;THE NAMES OF SOME OF THESE SHOULD BE CHANGED....
(ASSIGN TRANSPORT (PLUS (I-ARG 1) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-NO-TRAP (PLUS (I-ARG 21) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                                DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-NO-TRAP-READ-WRITE
        (PLUS (I-ARG 61) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-CDR (PLUS (I-ARG 32) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                            DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-IVC (PLUS (I-ARG 23) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                      DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-WRITE (PLUS (I-ARG 63) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-READ-WRITE (PLUS (I-ARG 41) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-HEADER (PLUS (I-ARG 4) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
;Saved AC's of stack groups may have funny data types in them, minimize barfage.
(ASSIGN TRANSPORT-AC (PLUS (I-ARG 20) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
(ASSIGN TRANSPORT-SCAV (PLUS (I-ARG 130) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT i-dont-chain))
;The NO-EVCP ones are used by binding.  They have a separate dispatch table to
;save taking frequent useless traps on EVCP's.  Also they have to not barf at
;trap data types, such as DTP-NULL.
;NO-EVCP also used by PDL buffer refill now that its legal to have EVCPs on PDL.
;its is also legal to have one-q-forwards on PDL.

;The above does not seem to be true, one-q-forwards on the pdl cause errors in pdl
;buffer scrolling. 02/13/86 -jrm

(ASSIGN TRANSPORT-NO-EVCP (PLUS (I-ARG 20) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP i-dont-chain))
(ASSIGN TRANSPORT-NO-EVCP-WRITE (PLUS (I-ARG 62) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP i-dont-chain))
(ASSIGN TRANSPORT-NO-EVCP-READ-WRITE
        (PLUS (I-ARG 60) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP i-dont-chain))
(ASSIGN TRANSPORT-NO-EVCP-FOR-PDL-RELOAD        ;now legal to have OQF on PDL.
        (PLUS (I-ARG 30) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP i-dont-chain))
(ASSIGN TRANSPORT-NO-EVCP-FOR-STACK-ENV
        (PLUS (I-ARG 0) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP I-DONT-CHAIN))
;use these for "one-level"  type bind guys.  This causes index forward to turn itself
; into an "equivalent" OQF.  Note that this means the "value-cell" itself can not be forwarded
; (altho its contents can be via OQF).

(ASSIGN TRANSPORT-BIND (PLUS (I-ARG 220) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
                        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP i-dont-chain))
(ASSIGN TRANSPORT-BIND-READ-WRITE
        (PLUS (I-ARG 260) Q-DATA-TYPE-PLUS-ONE-BIT DISPATCH-ON-MAP-19
        DISPATCH-PUSH-OWN-ADDRESS D-TRANSPORT-NO-EVCP i-dont-chain))


        ;I-ARG BIT 0 => MAKE DTP-EXTERNAL-VALUE-CELL-POINTER INVISIBLE.
        ;I-ARG BIT 1 => DON'T TRANSPORT (WRITING OVER THIS Q ANYWAY, OR ONLY CHECKING CDR CODE)
        ;I-ARG BIT 2 => BARF ON DTP-ONE-Q-FORWARD, DTP-EXTERNAL-VALUE-CELL-POINTER.
        ;I-ARG BIT 3 => ONE-Q-FORWARD NOT INVISIBLE.
        ;I-ARG BIT 4 => CAUSES IT NOT TO BARF IF TRAP DATATYPE IS SEEN
        ;I-ARG BIT 5 => WILL WRITE EVENTUALLY (MONITORS SHOULD TRAP).
        ;I-ARG BIT 6 => Don't follow header/body/rplacd-forwards.  Scavenger uses this.
        ;I-ARG BIT 7 => Convert index forward to "equivalent" evcp.

        ;LATER THESE MAY USE MULTIPLE DISPATCH TABLES RATHER THAN I-ARG'S

;(GC-WRITE-TEST)
;DO THIS immediately AFTER STARTING A WRITE.  THIS CHECKS FOR WRITING A POINTER
;TO THE EXTRA-PDL REGION, AND IF SO TRAPS, COPIES THE THING OUT,
;DOES THE WRITE OVER AGAIN.  Return is to after the GC-WRITE-TEST.
;No other cycles must interve between the write and this since the
;level 1 map bits for the address being written are latched on the write.
;However, interrupts are ok because of the special hack that redoes the
;write after the interrupt has been processed.
;*** no longer true *** AND COMES BACK AND DOES THE DISPATCH OVER AGAIN.
;OK TO COMBINE THIS WITH POPJ-AFTER-NEXT.  ** no longer true**
;It is ok, however to do this on the xct-next ed cycle, (as usual, unless
;a return to the main macro instruction loop is possible.)
;;; All I-ARGS having to do with stack closure traps are no longer useful.
; I-ARG bit 0 => comming from PDL-BUFFER dumper, clean up before extra-pdl-trap, etc.
; I-ARG bit 1 => comming from scavenger, ignore extra-pdl-trap, etc.
; I-ARG bit 2 => comming from transporter, etc, error if extra-pdl-trap
;                (only be volatility update should be possible)
; I-ARG bit 3 => comming from stack-group-switch or other context pdl-buffer
;                variables may not be valid.  Bypass check for storing in address
;                mapped into pdl-buffer.  NYI.
; I-ARG bit 4 => comming from extra-pdl-purge, used to ignore stack-closure-trap.  However,
;                test on this is commented since stack-closure pointer should not
;                point to extra-pdl.  This is probably useless now.

#+cadr (ASSIGN GC-WRITE-TEST (PLUS DISPATCH Q-DATA-TYPE-PLUS-ONE-BIT
                                   DISPATCH-ON-MAP-18
                                   WRITE-MEMORY-DATA D-GC-WRITE-TEST))
#+lambda (ASSIGN GC-WRITE-TEST (PLUS DISPATCH GC-MAP-TEST Q-DATA-TYPE-PLUS-ONE-BIT
                                   DISPATCH-ON-MAP-18
                                   WRITE-MEMORY-DATA D-GC-WRITE-TEST
                                   I-DONT-CHAIN))       ;dispatch never does xct-next.
#+exp  (ASSIGN GC-WRITE-TEST (PLUS DISPATCH Q-DATA-TYPE-PLUS-ONE-BIT
                                   DISPATCH-ON-MAP-18
                                   WRITE-MEMORY-DATA D-GC-WRITE-TEST))

;;; Use in garbage collector and related code to update volatilities, without taking any of the
;;; other possible gc-write traps.  I-ARG causes stack-closure traps to fall through, so the
;;; garbage collector doesn't cause needless (and perhaps dangerous) copying of stack-allocated
;;; stack-closures.
#+lambda (ASSIGN GC-WRITE-TEST-VOLATILITY (PLUS DISPATCH GC-MAP-TEST Q-DATA-TYPE-PLUS-ONE-BIT
                                                DISPATCH-ON-MAP-18
                                                WRITE-MEMORY-DATA D-GC-WRITE-TEST
                                                (i-arg 2)
                                                i-dont-chain))
#+exp  (ASSIGN GC-WRITE-TEST-VOLATILITY (PLUS DISPATCH Q-DATA-TYPE-PLUS-ONE-BIT
                                              DISPATCH-ON-MAP-18
                                              WRITE-MEMORY-DATA D-GC-WRITE-TEST
                                              (i-arg 2)))

;;; Similar to gc-write-test-volatility above, but used in stack closure copying.
;;; Problem is, if a lexical variable itself contains a stack closure, we need to
;;; copy it out when the variable is copied out.  However, at the time we are deep
;;; inside the stack closure copying code, and it is probably not sufficiently recursive to win.
;;; For now we halt.

;;; For now, it updates volatilities, without taking any of the
;;; other possible gc-write traps.  I-ARG causes stack-closure traps to fall through, so the
;;; garbage collector doesn't cause needless (and perhaps dangerous) copying of stack-allocated
;;; stack-closures.
#+lambda (ASSIGN GC-WRITE-TEST-VOLATILITY-stack-closure-copy
                 (PLUS DISPATCH GC-MAP-TEST Q-DATA-TYPE-PLUS-ONE-BIT
                       DISPATCH-ON-MAP-18
                       WRITE-MEMORY-DATA D-GC-WRITE-TEST
                       (i-arg 6)
                       i-dont-chain))
#+exp  (ASSIGN GC-WRITE-TEST-VOLATILITY-stack-closure-copy
               (PLUS DISPATCH Q-DATA-TYPE-PLUS-ONE-BIT
                     DISPATCH-ON-MAP-18
                     WRITE-MEMORY-DATA D-GC-WRITE-TEST
                     (i-arg 6)))

;To turn on dispatch-start-mem-read in d-qmrcl
;
;  1. add dispatch-write-vma in the assign for qmrcl-dispatch
;  2. add dispatch-start-mem-read at qlentr in d-qmrcl
;  3. tags search for d-qmrcl-qlentr to find all the places the
;     dispatch entry is written over.  everywhere you see 0_19., change it
;     to 1_19.
;  4. get rid of vma-start-read in qlentr
;  5. get rid of vma-start-read in meter-function-entry

#+lambda (ASSIGN QMRCL-DISPATCH (PLUS DISPATCH-WRITE-VMA Q-DATA-TYPE D-QMRCL))
#+exp    (ASSIGN QMRCL-DISPATCH (PLUS Q-DATA-TYPE D-QMRCL))


(LOCALITY M-MEM)        ;ANYTHING WHICH IT IS DESIRED TO LDB OUT OF MUST BE IN M-MEM

M-GARBAGE       (0)     ;THIS REG RANDOMLY STORED IN WHEN STORING IN FUNCTION
                        ;DESTINATIONS AND NOT SIMULTANEOUSLY IN M-MEM

M-PGF-TEM (0)           ;TEMPORARY LOCATION USED BY PAGE FAULT HANDLER

M-ZERO  (0)             ;CONSTANT 0, USED FOR LDB OPERATIONS  (MUST BE IN LOCATION 2)
M-MINUS-ONE (-1)        ;CONSTANT -1, USED FOR SIGN EXTENSION (MUST BE IN LOCATION 3)

;LETTERED REGISTERS, OR "PDP-10 ACS"
;THESE ARE PRESERVED BY SEQUENCE BREAKS, INTERRUPTS, AND PAGE FAULTS, AND MARKED-THROUGH
;BY THE GARBAGE COLLECTOR.  THEY MUST ALWAYS CONTAIN TYPED DATA.  SMALL
;NUMBERS (WITH TYPE-FIELD OF 0 OR 37) ARE ACCEPTABLE BY SPECIAL DISPENSATION.

M-ZR    (0)             ;.. begin "pointer" ACs. See RESET-EXTRA-PDL,
M-A     (0)             ;..   STORE-0-IN-POINTER-ACS, if this changed.
M-B     (0)             ;..
M-C     (0)             ;..
M-D     (0)             ;..
M-E     (0)             ;..
M-T     (0)             ;..     RESULT REGISTER, PSEUDO INDICATORS
M-R     (0)             ;..
M-Q     (0)             ;..
M-I     (0)             ;..
M-J     (0)             ;.. end "pointer" ACs.
M-S     (0)             ;.. M-S and M-K are typed and part of the stack group,
M-K     (0)             ;..  However, they are clobbered by storage allocation,
                        ;    so are not checked for pointers to EXTRA-PDL at
                        ;    RESET-EXTRA-PDL.
                        ;SEQUENCE BREAKS ALSO SAVE VMA, AND SAVE MD
                        ;BY RE-READING FROM VMA ON RESUME.  THE RE-READING OF
                        ;MEMORY DATA MEANS THE INTERRUPT IS EFFECTIVELY
                        ;INSERTED -BEFORE- THE READ CYCLE.  NOTE THAT THIS
                        ;ORDERING ALLOWS AN EFFECTIVE READ-PAUSE-WRITE CYCLE
                        ;TO BE DONE JUST BY DOING A READ THEN A WRITE, EVEN
                        ;THOUGH AFTER EACH CYCLE IS STARTED SEQ BRKS ARE CHECKED.

M-AP    (0)             ;POINTS AT EXECUTING FRAME
                        ;CAUTION: M-AP HOLDS A PDL BUFFER ADDRESS
                        ;TO GET CORRESPONDING MEMORY ADDRESS,
                        ;USE CONVERT-PDL-BUFFER-ADDRESS
                        ;CAUTION: M-AP MUST NOT CONTAIN ANY GARBAGE IN THE HIGH BITS.

;"NUMBERED REGISTERS".  THESE ARE TEMPORARIES WHICH ARE NOT MARKED THROUGH
;BY THE GARBAGE COLLECTOR.  THE FULL 32-BIT VALUES OF M-1 AND M-2 ARE SAVED
;IN STACK GROUPS, HENCE PRESERVED THROUGH SEQUENCE BREAKS, BUT M-3 AND M-4
;ARE NOT.
;M-1, M-2, M-3, AND M-4 ARE USED TO HOLD UNBOXED FIXNUMS OR FLONUM MANTISSAS;
;M-1, M-2 ARE THE FIRST AND SECOND ARGUMENTS RESPECTIVELY.
;M-1, M-2, M-3, AND M-4 ARE PRESERVED BY PAGE-FAULTS,
;BUT CAUTION SHOULD BE EXERCISED, GENERALLY THESE SHOULD ONLY BE VALID OVER
;A LOCALITY OF A FEW INSTRUCTIONS.
M-1     (0)
M-2     (0)
M-3     (0)
M-4     (0)

(LOC 26)  ;%MODE-FLAGS LISP VARIABLE IS MAPPED TO THIS LOCATION, SEE QCOM
M-FLAGS                 ;"MACHINE STATE FLAGS"
;FIRST COME "PROCESSOR FLAGS" IE THOSE SAVED AND RESTORED OVER MACRO CALL-RETURN
  (DEF-NEXT-BIT M-QBBFL M-FLAGS)        ;BIND BLOCK PUT ON SPECIAL PDL (IN MACRO-CODE)
                                        ;BIND BLOCK "OPEN" SIGNAL TO LOW LEVEL ROUTINES
                                        ; IN MICRO-COMPILED FCTNS
                                        ;ALSO SET IF FRAME HAS CLOSURE BINDING-BLOCK
 (DEF-DATA-FIELD M-FLAGS-PROCESSOR-FLAGS 1 0)   ;BYTE POINTER TO PROCESSOR FLAGS
 (DEF-DATA-FIELD M-FLAGS-EXCEPT-PROCESSOR-FLAGS 37 1)
;END "PROCESSOR FLAGS", BEGIN PROCESSOR "MODES"
  (DEF-NEXT-FIELD M-CAR-SYM-MODE 2 M-FLAGS)     ;CAR OF SYM GIVES:
                                                ;  ERROR
                                                ;  ERROR EXCEPT (CAR NIL) = NIL
                                                ;  NIL
                                                ;  UNUSED, WAS ONCE <P-STRING ARRAY POINTER>
  (DEF-NEXT-FIELD M-CAR-NUM-MODE 2 M-FLAGS)     ;CAR OF NUMBER GIVES:
                                                ;  ERROR
                                                ;  NIL
                                                ;  "WHATEVER IT IS"
  (DEF-NEXT-FIELD M-CDR-SYM-MODE 2 M-FLAGS)     ;CDR OF SYM GIVES:
                                                ;  ERROR
                                                ;  ERROR EXCEPT (CDR NIL) = NIL
                                                ;  NIL
                                                ;  PROPERTY LIST
  (DEF-NEXT-FIELD M-CDR-NUM-MODE 2 M-FLAGS)     ;CDR OF NUM GIVES:
                                                ;  ERROR
                                                ;  NIL
                                                ;  "WHATEVER IT IS"

  (DEF-NEXT-BIT M-DONT-SWAP-IN M-FLAGS)         ;MAGIC FLAG FOR CREATING FRESH PAGES

  (DEF-NEXT-BIT M-TRAP-ENABLE M-FLAGS)          ;1 ENABLE ERROR TRAPPING

  (DEF-NEXT-FIELD M-MAR-MODE 2 M-FLAGS)         ;1 IS READ-TRAP, 2 IS WRITE-TRAP

  (DEF-NEXT-BIT M-PGF-WRITE M-FLAGS)            ;1 IF CURRENT PG FAULT IS WRITING

    (DEF-BIT-FIELD-IN-REG M-FLAGS-MAR-DISP 3 11. M-FLAGS)
                ;INCLUDES M-MAR-MODE AND M-PGF-WRITE

  ;FLAGS FOR CURRENT MAJOR SECTION OF CODE EXECUTING, LOOKED AT
  ;BY PAGE-FAULT AND SEQUENCE-BREAK HANDLERS
  (DEF-NEXT-BIT M-INTERRUPT-FLAG M-FLAGS)       ;1 IF IN INTERRUPT HANDLER, NO PAGING PLEASE
  (DEF-NEXT-BIT M-SCAVENGE-FLAG M-FLAGS)        ;1 IF IN SCAVENGER, NO SEQUENCE BREAKS
  (DEF-NEXT-BIT M-TRANSPORT-FLAG M-FLAGS)       ;1 IF IN TRANSPORTER, NO SEQUENCE BREAKS
  (DEF-NEXT-BIT M-STACK-GROUP-SWITCH-FLAG M-FLAGS) ;1 IF SWITCHING SGS, NO SEQUENCE BREAKS
    (DEF-BIT-FIELD-IN-REG M-FLAGS-NO-SEQUENCE-BREAK 4 14. M-FLAGS)
        ;M-INTERRUPT-FLAG, M-SCAVENGE-FLAG, M-TRANSPORT-FLAG, M-STACK-GROUP-SWITCH-FLAG
    (DEF-BIT-FIELD-IN-REG M-FLAGS-FOR-PAGE-TRACE 3 15. M-FLAGS)
        ;M-SCAVENGE-FLAG, M-TRANSPORT-FLAG, M-STACK-GROUP-SWITCH-FLAG
  (DEF-NEXT-BIT M-DEFERRED-SEQUENCE-BREAK-FLAG M-FLAGS) ;1 IF WANTING TO SEQUENCE-BREAK
        ;Set if want to SB and A-INHIBIT-SCHEDULING-FLAG on.
        ;Checked after popping bindings (and thus maybe affecting A-INHIBIT-SCHEDULING-FLAG).
        ; If appropriate, the bit is stuffed back into the hardware (at SB-REINSTATE).
  (DEF-NEXT-BIT M-METER-STACK-GROUP-ENABLE M-FLAGS)     ;1 IF METERING ON FOR THIS STACK GROUP
  (DEF-NEXT-BIT M-TRAP-ON-CALLS M-FLAGS)                ;1 => TRAP ON ACTIVATING STACK FRAME.
  (DEF-NEXT-BIT M-ENABLE-STORE-UNRECONCILED M-FLAGS)    ;1 => GC-WRITE-TEST fails to illop on DTP-UNRECONCILED
  (def-next-bit m-flags-check-structure-handles m-flags);1 => Enable error checks for bogus handles

  ( (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)      ;SAME STUFF ALSO IN A-FLAGS
          (BYTE-VALUE M-CAR-SYM-MODE 1)         ;INITIAL MODE STATE
          (BYTE-VALUE M-CAR-NUM-MODE 0)
          (BYTE-VALUE M-CDR-SYM-MODE 1)
          (BYTE-VALUE M-CDR-NUM-MODE 0)
          (BYTE-VALUE M-DONT-SWAP-IN 0)
          (BYTE-VALUE M-TRAP-ENABLE 0)
          (BYTE-VALUE M-MAR-MODE 0)
          (BYTE-VALUE M-PGF-WRITE 0)
          (BYTE-VALUE M-INTERRUPT-FLAG 0)
          (BYTE-VALUE M-SCAVENGE-FLAG 0)
          (BYTE-VALUE M-TRANSPORT-FLAG 0)
          (BYTE-VALUE M-STACK-GROUP-SWITCH-FLAG 0)
          (BYTE-VALUE M-DEFERRED-SEQUENCE-BREAK-FLAG 0)
          (BYTE-VALUE M-METER-STACK-GROUP-ENABLE 0)
          ;(byte-value m-trap-on-calls 0)
          ;(byte-value m-enable-store-unreconciled 0)
          ))

M-PDL-BUFFER-ACTIVE-QS (0)      ;HOLDS QS BETWEEN A-PDL-BUFFER-HEAD AND M-AP INCLUSIVE
                                ; SEE DISCUSSION ON PDL-BUFFER DUMP/REFILL

M-ERROR-SUBSTATUS  (0)  ;IDENTIFING INFO WHEN TAKING ERROR.  IF NON-ZERO, THERE IS AN
                        ;ERROR PENDING
                        ;FOR NOW, AT LEAST, THIS IS ONLY USED BY FUNCTION-ENTRY ERRORS.
                        ;SYMBOL ASSIGNMENTS ARE UNIQUE TO EACH ERROR
  (DEF-NEXT-BIT M-QBTFA M-ERROR-SUBSTATUS)      ;TOO FEW ARGS
  (DEF-NEXT-BIT M-QBTMA M-ERROR-SUBSTATUS)      ;TOO MANY ARGS
  (DEF-NEXT-BIT M-QBEQTA M-ERROR-SUBSTATUS)     ;ERRONEOUS QUOTED ARG
  (DEF-NEXT-BIT M-QBEEVA M-ERROR-SUBSTATUS)     ;ERRONEOUS EVALUATED ARG
  (DEF-NEXT-BIT M-QBBDT M-ERROR-SUBSTATUS)      ;BAD DATA TYPE
  (DEF-NEXT-BIT M-QBBQTS M-ERROR-SUBSTATUS)     ;BAD QUOTE STATUS
 (RESET-BIT-POINTER M-ERROR-SUBSTATUS)

#-cadr (begin-comment)
M-INST-BUFFER  (0)      ;LAST MACRO INSTRUCTION Q FETCHED (2 INSTRUCTIONS)
  (DEF-BIT-FIELD-IN-REG M-INST-DEST 2 16 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
  (DEF-BIT-FIELD-IN-REG M-INST-SUB-OPCODE 3 15 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
;Should not be used any more.
;  (DEF-BIT-FIELD-IN-REG M-INST-DEST-LOW-BIT 1 15 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
  (DEF-BIT-FIELD-IN-REG M-INST-OP 5 11 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
  (DEF-BIT-FIELD-IN-REG M-INST-ADR 11 0 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
  (DEF-BIT-FIELD-IN-REG M-INST-ADR-*2+X 12 37 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
  (DEF-BIT-FIELD-IN-REG M-INST-REGISTER 3 6 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
  (DEF-BIT-FIELD-IN-REG M-INST-DELTA 6 0 (PLUS M-INST-BUFFER INSTRUCTION-STREAM))
#-cadr (end-comment)

 #-lambda (begin-comment)
M-LAM   (0)             ;used by cadr-physical to lambda-physical hacker.
                        ; extremely volatile, clobbered by page faults and everything else.
  (DEF-BIT-FIELD-IN-REG MACRO-IR-DEST 2 16 MACRO-IR)
  (DEF-BIT-FIELD-IN-REG MACRO-IR-SUB-OPCODE 3 15 MACRO-IR)
;  (DEF-BIT-FIELD-IN-REG MACRO-IR-DEST-LOW-BIT 1 15 MACRO-IR)
  (DEF-BIT-FIELD-IN-REG MACRO-IR-OP 5 11 MACRO-IR)
  (DEF-BIT-FIELD-IN-REG MACRO-IR-ADR 11 0 MACRO-IR)
  (DEF-BIT-FIELD-IN-REG MACRO-IR-REGISTER 3 6 MACRO-IR)
  (def-bit-field-in-reg macro-ir-2-bit-sub-opcode 2 16 macro-ir)
  ;displacement available directly via MACRO-IR-DISPLACEMENT
 #-lambda (end-comment)

 #-exp (begin-comment)
M-LAM   (0)             ;used by cadr-physical to lambda-physical hacker.
  (DEF-BIT-FIELD-IN-REG MACRO-IR-DEST 2 16 MACRO-IR)
  (DEF-BIT-FIELD-IN-REG MACRO-IR-SUB-OPCODE 3 15 MACRO-IR)
;  (DEF-BIT-FIELD-IN-REG MACRO-IR-DEST-LOW-BIT 1 15 MACRO-IR)
  (DEF-BIT-FIELD-IN-REG MACRO-IR-OP 5 11 MACRO-IR)
;  (DEF-BIT-FIELD-IN-REG MACRO-IR-ADR 11 0 MACRO-IR) - true func src on explorer
  (DEF-BIT-FIELD-IN-REG MACRO-IR-REGISTER 3 6 MACRO-IR)
  (def-bit-field-in-reg macro-ir-2-bit-sub-opcode 2 16 macro-ir)
  ;displacement available directly via MACRO-IR-DISPLACEMENT
 #-exp (end-comment)

M-LAST-MICRO-ENTRY      (0)     ;HOLDS LAST MICRO ENTRY ADDRESS TRANSFERRED TO
                                ;IN THE JUMP FIELD.  SOMETIMES CLOBBERED W/O JUMP?
M-TEM   (0)             ;GENERAL-PURPOSE TEMPORARY, CLOBBERED BY PAGE FAULTS
                        ;THIS SHOULD ONLY BE USED OVER RANGES OF A FEW INSTRUCTIONS.
(LOC 34)  ;%SEQUENCE-BREAK-SOURCE-ENABLE LISP VARIABLE MAPPED HERE, SEE QCOM.
M-SB-SOURCE-ENABLE   ;each bit controls a potential source of sequence-breaks:
  ; Note: the numeric values of these bits are known by SI:SB-ON!
  (DEF-NEXT-BIT M-SBS-CALL M-SB-SOURCE-ENABLE)  ;Just the CALL key (OBSOLETE).
  (DEF-NEXT-BIT M-SBS-UNIBUS M-SB-SOURCE-ENABLE)   ;Any Unibus channel.
  (DEF-NEXT-BIT M-SBS-CHAOS M-SB-SOURCE-ENABLE) ;Any CHAOS packet received.  Its
                                                ; unclear if you really want to set this.
  (DEF-NEXT-BIT M-SBS-CLOCK M-SB-SOURCE-ENABLE) ;The clock, derived from TV

   ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))

(LOC 35)        ;%METER-ENABLES lisp variable is mapped here, see qcom
                ;n.b!! LAMBDA-BINDING this doesnt win any more. Use %set-meter-enables
M-METER-ENABLES ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ; Enables for microcode metering

#+cadr(begin-comment)
(loc 36)
m-36    (0)
m-37    (0)
    ;LAM expects to find version in 40, as well. This will be phased out eventually.
m-40    ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                       VERSION-NUMBER)) ;VERSION NUMBER FROM SECOND FILE NAME OF SOURCE
m-41    (0)
m-42    (0)
m-fef   (0)                                     ;C-PDL(M-AP): i.e. current fef pointer
m-array-pointer         (0)
m-array-header          (0)
m-array-origin          (0)
m-array-length          (0)
m-array-rank            (0)
     ;Temporary registers used so we can move the mouse arrays out of a mem
     ;and get more room there.
m-random-temporary-cursor-pattern-base (0)
m-random-temporary-buttons-buffer-base (0)
m-random-temporary-mouse-x-scale-base  (0)
m-random-temporary-mouse-y-scale-base  (0)
m-55    (0)
m-56    (0)
m-57    (0)
m-60    (0)
m-61    (0)
m-62    (0)
m-63    (0)
m-64    (0)
m-65    (0)
m-66    (0)
m-67    (0)
m-6     (0)                                     ;70
m-5     (0)                                     ;71
m-tem4  (0)                                     ;72
m-tem3  (0)                                     ;73
m-tem2  (0)                                     ;74
m-tem1  (0)                                     ;75
;76-77 reserved for M-constants.
#+cadr(end-comment)

(LOCALITY A-MEM)

;;; THE FIRST 64. (32 on CADR)  LOCATIONS SHADOW M-MEMORY.
;;; THE M-LOCATIONS ARE CONSIDERED PRIMARY.  THE M-LOCATION SHOULD
;;; ALWAYS BE USED IN THE DESTINATION, SINCE WRITING IN M-MEMORY CLOBBERS
;;; A-MEMORY BUT WRITING IN A-MEMORY DOESN'T CLOBBER M-MEMORY.  THE A-LOCATION
;;; CAN BE USED AS A SOURCE WHEN NECESSARY TO GET IT INTO THE CORRECT
;;; SIDE OF THE ADDER OR TO OPERATE ON TWO M-LOCATIONS IN THE SAME INSTRUCTION.

A-GARBAGE (0)
A-PGF-TEM (0)
A-ZERO  (0)             ;CONSTANT 0 USED FOR LDB OPERATIONS-- MUST BE 2
A-MINUS-ONE (-1)        ;CONSTANT -1 -- MUST BE 3
A-ZR    (0)             ;SEE COMMENTS ON CORRESPONDING M-LOCATIONS
A-A     (0)
A-B     (0)
A-C     (0)
A-D     (0)
A-E     (0)
A-T     (0)
A-R     (0)
A-Q     (0)
A-I     (0)
A-J     (0)
A-S     (0)
A-K     (0)
A-AP    (0)
A-1     (0)
A-2     (0)
A-3     (0)
A-4     (0)
(LOC 26)  ;MUST, OF COURSE, BE AT SAME LOCATION AS M-FLAGS
A-FLAGS
  ( (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)      ;SAME STUFF ALSO IN M-FLAGS
          (BYTE-VALUE M-CAR-SYM-MODE 1)         ;INITIAL MODE STATE
          (BYTE-VALUE M-CAR-NUM-MODE 0)
          (BYTE-VALUE M-CDR-SYM-MODE 1)
          (BYTE-VALUE M-CDR-NUM-MODE 0)
          (BYTE-VALUE M-DONT-SWAP-IN 0)
          (BYTE-VALUE M-TRAP-ENABLE 0)
          (BYTE-VALUE M-MAR-MODE 0)
          (BYTE-VALUE M-PGF-WRITE 0)
          (BYTE-VALUE M-INTERRUPT-FLAG 0)
          (BYTE-VALUE M-SCAVENGE-FLAG 0)
          (BYTE-VALUE M-TRANSPORT-FLAG 0)
          (BYTE-VALUE M-STACK-GROUP-SWITCH-FLAG 0)
          (BYTE-VALUE M-DEFERRED-SEQUENCE-BREAK-FLAG 0)
          (BYTE-VALUE M-METER-STACK-GROUP-ENABLE 0)
          ;(byte-value m-trap-on-calls 0)
          ;(byte-value m-enable-store-unreconciled 0)
          ))

A-PDL-BUFFER-ACTIVE-QS (0)
A-ERROR-SUBSTATUS (0)

#-cadr (begin-comment)
A-INST-BUFFER (0)
 #-cadr (end-comment)

#-lambda (begin-comment)
A-LAM (0)
 #-lambda (end-comment)

#-exp (begin-comment)
A-LAM (0)
 #-exp (end-comment)

A-LAST-MICRO-ENTRY (0)
A-TEM (0)
A-SB-SOURCE-ENABLE ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
A-METER-ENABLES ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))

#+cadr(begin-comment)
(loc 36)
a-36    (0)
a-37    (0)
  ;LAM expects to find version in  40.  This will be phased out, eventually
a-40    ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                       VERSION-NUMBER)) ;VERSION NUMBER FROM SECOND FILE NAME OF SOURCE
a-41    (0)
a-42    (0)
a-fef   (0)                                     ;C-PDL(M-AP): i.e. current fef pointer
a-array-pointer         (0)
a-array-header          (0)
a-array-origin          (0)
a-array-length          (0)
a-array-rank            (0)
a-random-temporary-cursor-pattern-base (0)
a-random-temporary-buttons-buffer-base (0)
a-random-temporary-mouse-x-scale-base  (0)
a-random-temporary-mouse-y-scale-base  (0)
a-mouse-save-c  (0)
a-mouse-save-d  (0)
a-57    (0)
a-60    (0)
a-61    (0)
a-62    (0)
a-63    (0)
a-64    (0)
a-65    (0)
a-66    (0)
a-67    (0)
a-6     (0)                                     ;70
a-5     (0)                                     ;71
a-tem4  (0)                                     ;72
a-tem3  (0)                                     ;73
a-tem2  (0)                                     ;74
a-tem1  (0)                                     ;75
;76-77 reserved for M-constants.
(loc 100)
#+cadr(end-comment)
#-cadr(begin-comment)
(loc 40)
#-cadr(end-comment)

;"Q" STORAGE STARTS HERE.. IE THIS CAN "POTENTIALLY" BE RELOCATED DURING A GC

;FOLLOWING VECTOR OF A-MEM LOCATIONS ARE REFERENCED EXTERNALLY.  ORDER HERE MUST
; AGREE WITH A-MEMORY-LOCATION-NAMES IN QCOM

;A-VERSION MUST BE FIRST
A-VERSION       ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                       VERSION-NUMBER)) ;VERSION NUMBER FROM SECOND FILE NAME OF SOURCE
A-AMCENT        (0)     ;NUMBER OF ACTIVE MICRO-CODE ENTRIES
a-default-cons-area
A-CNSADF        (0)     ;DEFAULT AREA TO DO CONSES IN, OK IF HAS TYPE
a-number-cons-area
A-NUM-CNSADF    (0)     ;AREA TO DO POTENTIAL "EXTRA-PDL" CONSES IN.  CAN EITHER BE
                        ; THE REAL EXTRA-PDL-AREA OR REGULAR-AREA.

;"SCRATCHPAD" CONSTANTS AND MODES
;INITIALIZED FROM SCRATCHPAD-INIT-AREA ON STARTUP.

A-SCRATCH-PAD-BEG
A-INITIAL-FEF   (0)     ;POINTER TO FEF OF FUNCTION TO START UP IN
A-QTRSTKG       (0)     ;POINTER TO TRAP HANDLER STACK-GROUP
A-QCSTKG        (0)     ;POINTER TO CURRENT STACK-GROUP
A-QISTKG        (0)     ;POINTER TO INITIAL STACK-GROUP
;A-QSSTKG is below at end of vector section.  Move it here sometime when
; both UCODE and cold-load must be changed.
A-SCRATCH-PAD-END

;STACK-GROUP RELATED
A-SG-STATE      (0)     ;SG-STATE Q OF CURRENT STACK GROUP
A-SG-PREVIOUS-STACK-GROUP (0)
A-SG-CALLING-ARGS-POINTER (0)
A-SG-CALLING-ARGS-NUMBER (0)
;A-SG-FOLLOWING-STACK-GROUP (0)

A-TRAP-MICRO-PC ((BYTE-VALUE Q-DATA-TYPE DTP-FIX)) ;PC OF (CALL TRAP) MICROINSTRUCTION

A-COUNTER-BLOCK-POINTER
                 ((BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                  (A-MEM-LOC A-COUNTER-BLOCK-BASE))
;PAGING CONTROLS


A-CHAOS-CSR-ADDRESS ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) #+cadr CHAOS-CSR-ADDRESS #-cadr 0))
A-MAR-LOW ((BYTE-VALUE Q-DATA-TYPE DTP-FIX)     ;CDR CODE MUST BE ZERO!
            177777777)          ;LOWEST ADDRESS MAR IS SET ON, WITH FIXNUM TYPE
A-MAR-HIGH ((BYTE-VALUE Q-DATA-TYPE DTP-FIX)
            177777776)          ;HIGHEST ADDRESS MAR IS SET ON, FIXNUM TYPE (NOT +1)

                                ;IT'S UNCLEAR HOW THESE GET RELOCATED BY GC,
                                ;WILL HAVE TO FIX UP LATER.
A-SELF  ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)) ;LAST DTP-INSTANCE, ETC INVOKED
A-METHOD-SEARCH-POINTER                       ;POSITION IN METHOD-LIST WHERE LAST
        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)) ; METHOD FOUND.
A-INHIBIT-SCHEDULING-FLAG       ;IF NON-NIL, NO SEQUENCE BREAKS
        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 5)
A-INHIBIT-SCAVENGING-FLAG       ;IF NON-NIL, SCAVENGER DOESN'T RUN
        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 5)
A-DISK-RUN-LIGHT ((BYTE-VALUE Q-DATA-TYPE DTP-FIX) DISK-RUN-LIGHT-VIRTUAL-ADDRESS)
                                ;LOCATION IN TV BUFFER ILLUMINATED WHEN DISK TRANSFERRING
                                ;THAT + 2 is the user run light.
                                ;THAT - 2 is the gc run-light. (rh TRANSPORTER, lh SCAVENGER)
                                ;THAT -12 is the SEQUENCE-BREAK deferred (flashing) light.
A-LOADED-BAND ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                                ;HIGH 24 BITS OF NAME OF BAND LOADED (FOR GREETING MSG)
;THESE TWO GET SET FROM THE LABEL
A-DISK-BLOCKS-PER-TRACK ((PLUS 17. (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
A-DISK-BLOCKS-PER-CYLINDER ((PLUS 85. (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;GC FLIP CONTROL
A-REGION-CONS-ALARM                     ;Counts new regions made
        ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 0))
A-PAGE-CONS-ALARM                       ;Counts pages allocated to new regions
        ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 0))
A-GC-FLIP-READY                         ;If non-NIL, there are no pointers to oldspace
        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)  ;NIL
A-INHIBIT-READ-ONLY                     ;If non-NIL, you can write in read-only
        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)  ;NIL
;A-SCAVENGER-WS-ENABLE                  ;Controls scavenger working set feature.
;       ((BYTE-VALUE Q-DATA-TYPE DTP-FIX) 0)  ;New scheme: lowest physical address
;   ;NOT in scavenger working set.  Note this is semi-compatible with the old T or NIL
;   ;scheme: both of these will turn off WS feature since only pointer is significant.
A-INDEXED-CELL-ARRAY ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)
    ;used in conjunction with DTP-INDEXED-FORWARD.  Gives array into which
    ; pointer field of DTP-INDEXED-FORWARD is the index to get the frob.
A-METHOD-SUBROUTINE-POINTER             ;CONTINUATION POINT FOR SELECT METHOD SUBROUTINE
        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)  ;RETURN OR NIL
A-QLARYH ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0) ;POINTER TO HEADER OF LAST ARRAY REFERENCED
A-QLARYL ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0) ;ELEMENT # OF LAST ARRAY REFERENCED
                                                 ; (W/ DTP-FIX DATA-TYPE)
A-QSSTKG ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0) ;POINTER TO SCHEDULER STACK-GROUP
A-TV-CURRENT-SHEET (0)          ;CURRENTLY-SELECTED SCREEN OR SHEET
                                ;THIS IS THE ONE WHOSE PARAMETERS HAVE BEEN COMPUTED
                                ;INTO A-TV-SCREEN-BUFFER-ADDRESS, ETC.
;was called A-DISK-READ-COMPARE-ENABLES.
A-DISK-SWITCHES ((BYTE-VALUE Q-DATA-TYPE DTP-FIX) 14)
                ;Bit 0 - read-compare after reads
                ;Bit 1 - read-compare after writes
                ;Bit 2 - enable multiple page swapouts
                ;Bit 3 - enable multiple page swapins
                ;Bit 4 - If 1, don't volatility-scan dirty pages.
                ;Bit 10. - enable ucode paging.
                ;This loads as zero so COLD-BOOT won't read-compare
A-MC-CODE-EXIT-VECTOR  ((BYTE-VALUE Q-DATA-TYPE DTP-FIX) 0)     ;Exit vector used by
                                ; microcompiled code to ref Q quantities.
                                ; Replaces MICRO-CODE-EXIT-AREA.
A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)
                                ;If NON-NIL, upper and lower case letters are different
A-ZUNDERFLOW ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)    ;If non-NIL, floating underflow => 0

;If this is not NIL, we try to do tail recursion without gobbling stack space.
A-TAIL-RECURSION     ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)
A-METER-GLOBAL-ENABLE
        ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0))  ;T if all stack groups metered
A-METER-BUFFER-POINTER
        ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;Pointer to disk buffer (must contain 1 block)
A-METER-DISK-ADDRESS
        ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;Next disk address to write buffer out to
A-METER-DISK-COUNT
        ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;Number of disk blocks left to write out
A-CURRENTLY-PREPARED-SHEET ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)
                                        ;Error checking for the TV:PREPARE-SHEET macro
;Variables for mouse tracking
A-MOUSE-CURSOR-STATE ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                                ;0 disabled, 1 open, 2 off, 3 on
A-MOUSE-X ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))    ;Relative to MOUSE-SCREEN
A-MOUSE-Y ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))    ;Relative to MOUSE-SCREEN
A-MOUSE-CURSOR-X-OFFSET ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;From top-left of pattern
A-MOUSE-CURSOR-Y-OFFSET ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;to the reference point
A-MOUSE-CURSOR-WIDTH ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
A-MOUSE-CURSOR-HEIGHT ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
A-MOUSE-X-SPEED ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;100ths per second, time averaged
A-MOUSE-Y-SPEED ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))      ;with time constant of 1/6 second
A-MOUSE-BUTTONS-BUFFER-IN-INDEX ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
A-MOUSE-BUTTONS-BUFFER-OUT-INDEX ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
A-MOUSE-WAKEUP ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0) ;Set to T when move or click

;Remember higher lexical contexts for nonlocal lexical variables.
;Value is a list of pointers to stack frames.
A-LEXICAL-ENVIRONMENT ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)

;Point to an array which holds slots for the EVCPs which
;were "stored" into a-memory locations, above,
;so that closures can bind such locations.
;-- this attempt lost, and has been flushed, so this location available for recycle.
A-AMEM-EVCP-VECTOR ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)

;Area for consing things that are not explicitly requested
;and should not go in a temporary area.
;Initialized from A-CNSADF at startup time.
A-BACKGROUND-CONS-AREA (0)

;Mapping table of instance vars of flavor of current method
;to slots in value of SELF.
A-SELF-MAPPING-TABLE ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)

A-GC-SWITCHES        ((BYTE-VALUE Q-DATA-TYPE DTP-FIX) 0)         ; (byte 2 0) -> volatility level of current flip

;T if arrays are stored with last subscript varying fastest.
A-ARRAY-INDEX-ORDER  ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 5)

;1 for CADR, 2 for LAMBDA.
A-PROCESSOR-TYPE-CODE ((PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) #+cadr 1 #+lambda 2 #+exp 3))

;The following two are available for recycling, but may still be "cleared" by some macrocode.
A-AR-1-ARRAY-POINTER-1 ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)
A-AR-1-ARRAY-POINTER-2 ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0)
A-LOADED-UCODE         ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                                ;HIGH 24 BITS OF NAME OF MICRO BAND LOADED (FOR GREETING MSG)
;END OF VECTOR AREA

;Following locations are gc-able but not user-referenceable.

A-V-NIL         ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0) ;Pointer to NIL
A-V-TRUE        ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 5) ;Pointer to T
A-FLOATING-ZERO ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL) 0) ;Changed to full-flonum 0.0 at start.

;these used by prolog, but need to be transported by GC.
;Potentially could be conditionalized, for now, that involves a possible screw with the
; fast reader since conditionalization occurs at read-in time.  This could be fixed.
a-unify-dispatch((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL))
a-lmp-vector    ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL))
a-lmp-trail     ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL))

A-END-Q-POINTERS ((BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL))  ;Waste a location to fix fencepost error.

;END "Q" STORAGE

;FOLLOWING ARE 32 BIT UNTYPED COUNTERS AND METERS.  ORDER MUST AGREE WITH
; QCOM.  THIS BLOCK IS POINTED TO BY A-COUNTER-BLOCK-POINTER.
A-COUNTER-BLOCK-BASE
A-FIRST-LEVEL-MAP-RELOADS  (0)  ;# FIRST LEVEL MAP RELOADS
A-SECOND-LEVEL-MAP-RELOADS (0)  ;# SECOND LEVEL MAP RELOADS
A-PDL-BUFFER-READ-FAULTS   (0)  ;# TOOK PGF AND DID READ FROM PDL-BUFFER
A-PDL-BUFFER-WRITE-FAULTS  (0)  ;# TOOK PGF AND DID WRITE TO PDL-BUFFER
A-PDL-BUFFER-MEMORY-FAULTS (0)  ;# TOOK PGF FOR PDL-BUF, BUT DATA IN MAIN MEM.
A-DISK-PAGE-READ-COUNT     (0)  ;COUNT OF PAGES READ FROM DISK
A-DISK-PAGE-WRITE-COUNT    (0)  ;COUNT OF PAGES WRITTEN TO DISK
A-DISK-ERROR-COUNT         (0)  ;COUNT OF RECOVERABLE ERRS
A-FRESH-PAGE-COUNT         (0)  ;COUNT OF FRESH PAGES
                                ;  GENERATED IN CORE INSTEAD OF READ FROM DISK
A-PAGE-AGE-COUNT           (0)  ;NUMBER OF TIMES AGER SET AGE TRAP
A-PAGE-FLUSH-COUNT         (0)  ;NUMBER OF TIMES AGE TRAP -> FLUSHABLE
A-DISK-READ-COMPARE-REWRITES (0);NUMBER OF TIMES A WRITE WAS DONE OVER BECAUSE OF EITHER
                                ;DISK ERROR OR R/C DIFFERENCE DURING READ COMPARE
A-DISK-RECALIBRATE-COUNT   (0)  ;DUE TO SEEK ERRORS
A-META-BITS-MAP-RELOADS    (0)  ;RELOADS TO META-BITS-ONLY STATUS
A-COUNT-CHAOS-TRANSMIT-ABORTS (0)
A-DISK-READ-COMPARE-DIFFERENCES (0) ;NUMBER OF READ-COMPARE DIFFERENCES WITHOUT
                                ; ACCOMPANYING READ ERROR
;A-CONS-WORK-DONE (0)   ;K times number of Q's consed up (not a fixnum)
;A-SCAV-WORK-DONE (0)   ;number of Q's cleaned by scavenger (not a fixnum)
A-TV-CLOCK-RATE (12.)   ;TV frame rate divided by this to get clock, default is 1/second
A-AGING-DEPTH (0)       ;Number of laps before page aged.  Don't make bigger than 3!!
                        ;(two bit only in hardware may smash other things if bigger than 3)
A-DISK-ECC-COUNT (0)            ;Count of corrected soft ECC errors
A-COUNT-FINDCORE-STEPS (0)              ;Number of iterations in FINDCORE
A-COUNT-FINDCORE-EMERGENCIES (0)        ;Number of times FINDCORE had to age all pages
A-DISK-READ-COMPARE-REREADS (0) ;Number of times a read was done over because of either
                                ;disk error or R/C difference during read compare
A-DISK-PAGE-READ-OP-COUNT (0)   ;Number of read operations (counts once even if multipage)
A-DISK-PAGE-WRITE-OP-COUNT (0)  ;Number of write operations (counts once even if multipage)
A-DISK-PAGE-WRITE-WAIT-COUNT (0) ;Number of times actually had to wait while a page
                                ;was written out in order to reclaim the core
A-DISK-PAGE-WRITE-BUSY-COUNT (0) ;Number of times had to wait while a page was
                                ;written out because we wanted to use the disk
A-DISK-PREPAGE-USED-COUNT (0)   ;Number of prepaged pages that turned out to be wanted
A-DISK-PREPAGE-NOT-USED-COUNT (0) ;Number of prepaged pages reclaimed before used
A-DISK-ERROR-LOG-POINTER (600)  ;Points to next place in disk error log to store into
                                ;Entries lie in 600-637 range.  Each is 4 words:
                                ;       clp,,cmd                (guaranteed non-zero)
                                ;       disk-address read back
                                ;       status read back
                                ;       ma read back
A-DISK-WAIT-TIME        (0)     ;Amount of time spent in page faults
A-DISK-PAGE-WRITE-APPENDS (0)   ;Pages appended to swapout operations.
A-DISK-PAGE-READ-APPENDS  (0)   ;Pages appended to swapin operations.
A-LOWEST-DIRECT-VIRTUAL-ADDRESS (INTERNAL-LOWEST-A-MEM-VIRTUAL-ADDRESS)  ;Normally unchanged.
                                ;See comment near LOWEST-A-MEM-VIRTUAL-ADDRESS.

;These two are used to start output on the timestamped output device
;when the interval timer interrupts.
A-UNIBUS-TIMED-OUTPUT-CSR-ADDRESS  (0)
A-UNIBUS-TIMED-OUTPUT-CSR-BITS  (0)

;Count number of times we went to output to a timestamped output device.
A-TIMESTAMPED-OUTPUT-COUNT-1 (0)
;Count number of times around busy wait loop for doing such output.
;Together they show the average number of times around the loop per character.
A-TIMESTAMPED-OUTPUT-COUNT-2 (0)

A-COUNT-ILLOP-DEBUG (0)         ;Number of times got to ILLOP-DEBUG.  These are ignored unless
                ;halts enabled in A-PROCESSOR-SWITCHES.
A-COUNT-MICRO-FAULTS (0)        ;Number page faults in pagable-microcode system.

;the variable %processor-conf-watchdog lets the SDU tell if the lambda is crashed
;if it is 0, then the feature is disabled
;otherwise, the SDU decrements it by 50 each second (its internal clock is 50Hz)
;if it gets to 0, it does some as yet fuzzy thing that allows you to reboot.
;every second, the lambda clock interrupt copies A-INITIAL-WATCHDOG into
;%processor-conf-watchdog.
;for now, we set the timeout value to 20 seconds, but will someday have a lisp
;function that can reset it.  It's ok for this number to have a data type

a-initial-watchdog (250.)               ;5. * 50.

a-highest-handcode-page  ((ceiling  (I-MEM-LOC end-wired-ucode) 20))
a-highest-kernal-ucode-page  ((ceiling (i-mem-loc highest-kernal-ucode-location) 20))

a-scavenge-queue-maximum-depth (400)
a-transporter-scavenge-queue-work-quantum (400)

a-transporter-words-copied (0)

a-transporter-time (0)
a-transporter-disk-time (0)
a-scavenger-time (0)
a-scavenger-disk-time (0)

a-volatility-traps (0)

;END OF COUNTER AREA.

;A-MEM WORKING REGISTERS FOR VARIOUS SPECIFIC SECTIONS OF THE MICROCODE.
;THESE ARE NOT PRESERVED THROUGH SEQUENCE BREAKS.
A-TRANS-TEM     (0)     ;TEMPORARY USED BY TRANSPORTER
A-TRANS-MD      (0)     ;..
A-TRANS-VMA     (0)     ;..
A-INTR-TEM1     (0)     ;TEMPORARY USED BY INTERRUPT HANDLERS
A-INTR-TEM2     (0)     ;..
A-INTR-VMA      (0)     ;VMA SAVED HERE THROUGH INTERRUPTS
A-INTR-MD       (0)     ;MD SAVED HERE THROUGH INTERRUPTS
A-INTR-A        (0)     ;SAVE M-A
A-INTR-B        (0)     ;SAVE M-B
A-INTR-T        (0)     ;SAVE M-T
A-PGF-VMA (0)   ;PAGE FAULT HANDLER SAVES VMA HERE
A-PGF-WMD (0)   ;PAGE FAULT HANDLER SAVES WRITE-MEMORY-DATA HERE
A-PGF-T   (0)   ;PAGE FAULT HANDLER SAVES M-T HERE
A-PGF-A   (0)   ;PAGE FAULT HANDLER SAVES M-A HERE
A-PGF-B   (0)   ;PAGE FAULT HANDLER SAVES M-B HERE
A-PGF-MODE (0)  ;PAGE FAULT HANDLER keeps some flags here.  Data type should be DTP-FIX.
        ;  1.0 if 1, this is a binding operation  (formerly T was used for this)
        ;  1.1 if 1, this operation is a write-force (formerly, -1 used for this)
        ;  1.2 if 1, this is an unboxed write.
A-PDLB-TEM      (0)     ;TEMPORARY USED BY PDL-BUFFER LOADING/DUMPING ROUTINES
A-FARY-TEM      (0)     ;TEMPORARY USED BY XFARY
A-CONS-TEM      (0)     ;TEMPORARY FOR THE USE OF CONS

A-TRANS-SAVE-A (0)      ;REGISTER SAVING AT TRANS-COPY
A-TRANS-SAVE-B (0)
A-TRANS-SAVE-E (0)
A-TRANS-SAVE-K (0)
A-TRANS-SAVE-S (0)
A-TRANS-SAVE-T (0)
A-TRANS-SAVE-3 (0)
A-TRANS-SAVE-4 (0)
a-trans-save-5 (0)
a-trans-save-6 (0)

;Chaos net
;A-CHAOS-CSR-ADDRESS moved to vector section
#-cadr (begin-comment)
A-CHAOS-TRANSMIT-RETRY-COUNT (0)        ;0 TRANSMIT NOT ACTIVE, ELSE NUMBER RETRIES TO GO
A-CHAOS-TRANSMIT-ABORTED (0)            ;0 NORMAL, -1 DELAYING FOR A WHILE, +1 RETRYING
#-cadr (end-comment)

A-LCTYP (0)     ;LINEAR-CALL-TYPE DURING QLENTR (NORMAL, LEXPR, FEXPR, ETC)

;PAGING VARIABLES AND CONSTANTS
A-PHT-INDEX-MASK (0)            ;Mask for page hash table indices
A-PHT-INDEX-LIMIT (0)           ;All valid PHT indices are less than this
A-FINDCORE-SCAN-POINTER (0)     ;Page frame number of last page returned by FINDCORE
A-AGING-SCAN-POINTER (0)        ;Page frame number of last page aged by AGER
A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH (0)
;A-V-PHYSICAL-PAGE-DATA-END     ;First location after last valid physical-page-data entry
;                       (1_31.) ;This has to be initialized to the most negative number!
A-PAGE-IN-PHT1 (0)              ;Argument to PAGE-IN-MAKE-KNOWN

A-DISK-REGS-BASE (DISK-REGS-ADDRESS-BASE)
;These two get set from the PAGE partition's descriptor in the label.
;They define the starting disk address and number of disk blocks in the PAGE partition.
;These are "within" the LISP system, ie, must be relocated by A-DISK-CYLINDER-OFFSET
;to address the physical disk.
A-DISK-OFFSET (0)       ;block number of first block of paging partition.
A-DISK-MAXIMUM (0)      ;size of paging partition in blocks.
a-disk-page-unit (0)

;Status of the current disk operation in progress
A-DISK-BUSY     (0)             ;Non-zero if an operation is in progress
A-DISK-READ-WRITE (0)           ;Zero if Read, DISK-WRITE-COMMAND if Write
A-DISK-COMMAND  (0)             ;Command register (including recovery bits)
A-DISK-CLP      (0)             ;Address of command list
A-DISK-ADDRESS  (0)             ;Disk address (encoded into unit/cyl/surf/sec)
                                ; on lambda, this is AFTER A-DISK-CYLINDER-OFFSET
a-disk-transfer-size (0)
A-DISK-STATUS   (0)             ;Status read back
A-DISK-MA       (0)             ;MA read back (last memory location referenced)
A-DISK-FINAL-ADDRESS (0)        ;Disk address read back
A-DISK-ECC      (0)             ;Error correction data read back
A-DISK-RETRY-STATE (0)          ;Count of retries
A-DISK-DOING-READ-COMPARE (0)
A-DISK-IDLE-TIME (0)            ;Time since last disk op (other than background)
A-DISK-RESERVED-FOR-USER (0)    ;%DISK-OP in progress (inhibits background disk ops)

(ASSIGN DISK-SWAP-OUT-CCW-BASE 700) ;build CCW lists for swap out starting here
(ASSIGN DISK-SWAP-OUT-CCW-MAX  720) ; and not above here.
(ASSIGN DISK-SWAP-IN-CCW-BASE 740)  ;build CCW lists for swap in starting here
(ASSIGN DISK-SWAP-IN-CCW-MAX  760)  ; and not above here.

;Fields in A-DISK-ADDRESS.  These are also how the CADR disk control takes them.
(DEF-DATA-FIELD DA-UNIT     3  28.)
(DEF-DATA-FIELD DA-CYLINDER 12. 16.)
;da-cylinder-highest-bit is used to operate on the cylinder after it is extraced
; from a word with da-cylinder - in the lambda ucode, if this bit is set, it
; means to not offset this disk access by a-disk-cylinder-offset
(DEF-DATA-FIELD DA-CYLINDER-HIGHEST-BIT 1. 11.)
(DEF-DATA-FIELD DA-HEAD     8. 8.)
(DEF-DATA-FIELD DA-BLOCK    8. 0.)

;Locations for DISK-SWAP-HANDLER
A-DISK-SWAPIN-SIZE (0)

A-DISK-SWAPIN-VIRTUAL-ADDRESS (0)
A-DISK-SWAPIN-PAGE-FRAME (0)            ;physical page frame
A-DISK-SWAPIN-PHT2-BITS (0)

A-DISK-SWAP-OUT-CCW-POINTER (0)
A-DISK-SWAP-IN-CCW-POINTER (0)

A-DISK-SAVE-PGF-VMA (0)         ;some of these are also used when building CCWs just
A-DISK-SAVE-PGF-WMD (0)         ; before calling DISK-SWAP-HANDLER
A-DISK-SAVE-PGF-T (0)           ; also near SWAPIN.
A-DISK-SAVE-PGF-A (0)
A-DISK-SAVE-PGF-B (0)
A-DISK-SAVE-1 (0)
A-DISK-SAVE-2 (0)
A-DISK-SAVE-MODE (0)            ;save A-PGF-MODE
A-DISK-SAVE-PI (0)
A-DISK-SAVE-FLAGS (0)

A-DISK-CYL-BEG (0)      ;Typeless virtual address that lies at start of a cylinder
A-DISK-CYL-END (0)      ;Typeless virtual address that lies at start of next cylinder

;PARAMETERS OF THE CURRENTLY SELECTED SCREEN (SEE TV-SELECT-SCREEN)
;NOT PRESERVED THROUGH SEQUENCE BREAKS
;A-TV-CURRENT-SHEET (0)         ;CURRENTLY-SELECTED SCREEN, JUST FOR AN EFFICIENCY HACK
;ABOVE IS IN THE A-MEMORY-VARIABLES VECTOR
A-TV-SCREEN-BUFFER-ADDRESS (0)  ;START ADDRESS OF BUFFER (IN VIRTUAL ADDRESS SPACE)
A-TV-SCREEN-BUFFER-END-ADDRESS (0)      ;LAST BUFFER VIRTUAL ADDRESS +1
A-TV-SCREEN-LOCATIONS-PER-LINE (0)      ;AMOUNT TO ADD TO ADDRESS TO GET TO NEXT RASTER LINE
A-TV-SCREEN-BUFFER-BIT-OFFSET (0)       ;OFFSET IN BITS FROM A-TV-SCREEN-BUFFER-ADDRESS OF
                                        ;REAL START OF THE TV BUFFER
A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT (0)  ; (LOG2(N)) OF PIXEL SIZE,
                                        ;IN PLACE FOR OA-REG-LOW OF MROT.
A-TV-SCREEN-WIDTH (0)           ;WIDTH OF SHEET

;CLOCK BASED ON TV FRAME RATE
A-TV-CLOCK-COUNTER (0)
;A-TV-REGS-BASE (TV-REGS-ADDRESS-BASE)

;This is where READ-USEC-TIME puts the microsecond clock value.
;A-LAST-USEC-TIME (0)

;AREA ORIGIN POINTERS  (THESE ARE VIRTUAL ADDRESSES)
; THESE EXIST IN A-MEMORY ONLY TO SAVE TO ENABLE THEM TO BE REFERENCED
; WITHOUT A MEMORY CYCLE.  THEY ARE IN ORDER OF AREA NUMBER, AND ARE INITIALIZED
; AT UCADR STARTUP (BEG) AND NEVER CHANGED.
;NOTE THAT THESE POINT TO FIXED AREAS, WHICH HAVE ONE REGION, SO THAT
;CONFUSION BETWEEN AREAS AND REGIONS AT THIS LEVEL IS ALLOWED AND ENCOURAGED
A-V-RESIDENT-SYMBOL-AREA        (0)     ;RESIDENT SYM AREA
A-V-SYSTEM-COMMUNICATION-AREA   (400)   ;MUST BE AT LOC 400
A-V-SCRATCH-PAD-INIT-AREA       (0)     ;MUST BE AT LOC 1000
A-V-MICRO-CODE-SYMBOL-AREA      (0)     ;FIRST 600 LOCS ARE UCODE STARTING ADRS
                                        ; FOR (MACRO-CODE) MISC-INST S 200-777
                                        ;FOLLOWING ARE OTHER RANDOM UCODE ENTRIES.
  ;following 4 areas are ART-INUM.  Data stored in memory is clean, untyped.
A-V-REGION-ORIGIN       (0)             ;VIRTUAL ADDRESS START OF REGION
A-V-REGION-LENGTH       (0)             ;NUMBER OF QS IN REGION
A-V-REGION-BITS         (0)             ;VARIOUS FIELDS, SEE QCOM
A-V-REGION-FREE-POINTER (0)             ;RELATIVE ALLOCATION POINT.  ALLOCATION IS UPWARDS
A-V-WIRED-DISK-BUFFER   (0)
A-V-QUANTUM-MAP         (0)             ;(relocated to high memory during part of initialization but then gets put back)
  ;cold-reinit-ppd-1 has code dependant on exact order of following areas.
A-V-PAGE-TABLE-AREA     (0)     ;this region hard wired.
  ;a-v-physical-page-data must follow a-v-page-table-area or fix COLD-REINIT-PHT-0.
A-V-PHYSICAL-PAGE-DATA  (0)     ;this region hard-wired.
                                        ;FOR EACH PAGE FRAME, -1 IF IT IS OUT OF SERVICE, OR
                                        ; GC DATA,,PHT INDEX FOR PAGE IN IT
                                        ; -1 IN PHT INDEX IF WIRED PAGE WITH NO PHT ENTRY
                                        ; GC DATA=0 IF NOT IN USE
  ;a-v-address-space-map must follow a-v-physical-page-data or fix COLD-REINIT-PPD-0.
A-V-ADDRESS-SPACE-MAP   (0)     ;this region hard-wired.
                                        ;A BYTE FOR EACH ADDRESS SPACE QUANTUM, GIVING REGION#
                                        ; OR 0 IF FREE OR FIXED-AREA.  BYTE SIZE IS
                                        ; %ADDRESS-SPACE-MAP-BYTE-SIZE (now 16.)
a-v-virtual-page-volatility (0) ;this region hard-wired.
  ;--last wired area.--
a-v-region-moby-bits-array (0)          ;ART-Q
a-v-region-namespace-origin (0)         ;ART-Q
a-v-region-spare        (0)             ;ART-Q

  ;make sure COPY-BUFFER-CCW-PAGE-ORIGIN is set above here so code at COLD-SWAP-IN wins.
A-V-REGION-GC-POINTER   (0)             ;scavenger pointer.  ART-INUM.
A-V-REGION-LIST-THREAD  (0)             ;NEXT REGION# IN AREA, OR BOXED-SIZE
                                        ; + AREA# AT END OF LIST
                                        ; THREADS FREE REGION TABLE SLOTS  ART-INUM.
a-v-region-allocation-status (0)        ;Q per region holds untyped integer which is
        ;number Qs consed since last list header, or (pointerwise) -1 if fresh region
        ;or structure last consed.  ART-INUM.
A-V-region-area-map     (0)             ;(aref region-area-map <region #>) ==> <area #>. ART-INUM.
A-V-AREA-NAME           (0)             ;SYMBOL WHICH NAMES AREA (NIL FOR FREE AREA#S)
A-V-AREA-REGION-LIST    (0)             ;FIRST REGION# IN AREA (FREE LIST FOR FREE AREA#S)
A-V-AREA-REGION-BITS    (0)             ;GET REGION BITS OF NEW REGIONS FROM THIS. ART-INUM.
A-V-AREA-REGION-SIZE    (0)             ;RECOMMENDED SIZE FOR NEW REGIONS. ART-INUM.
A-V-SUPPORT-ENTRY-VECTOR        (0)
A-V-CONSTANTS-AREA      (0)             ;CONSTANTS PAGE (REF'ED IN ADR OF MACRO-CODE)
A-V-EXTRA-PDL-AREA (0)                  ;TEMPORARY NUMERIC RESULTS, SEPARATELY GC'ED
                                        ; MUST BE RIGHT BEFORE MICRO-CODE-ENTRY-AREA
A-V-MICRO-CODE-ENTRY-AREA       (0)     ;MICRO-CODE-ENTRY-AREA
A-V-MICRO-CODE-ENTRY-NAME-AREA  (0)     ;PARALLEL TO PRECEDING, HAS SYMBOL WHICH IS NAME
A-V-MICRO-CODE-ENTRY-ARGS-INFO-AREA (0) ;MICRO-CODE-ENTRY-ARGS-INFO-AREA
A-V-MICRO-CODE-ENTRY-MAX-PDL-USAGE (0)  ;MAXIMUM DEPTH ON PDL BEFORE MICRO TO MACRO CALL
A-V-MICRO-CODE-PAGING-AREA (0)
a-v-virtual-page-data (0)
a-v-scavenge-queue (0)
;Following areas are not used by microcode except for XRGN
;since they are not aligned on quantum boundaries
A-V-MICRO-CODE-ENTRY-ARGLIST-AREA (0)   ;VALUE FOR ARGLIST FUNCTION TO RETURN
A-V-MICRO-CODE-SYMBOL-NAME-AREA (0)     ;NAMES OF MICRO-CODE-SYMBOL-AREA ENTRIES
;A-V-LINEAR-PDL-AREA (0)                        ;MAIN PDL
;A-V-LINEAR-BIND-PDL-AREA (0)           ;CORRESPONDING BIND PDL
A-V-INIT-LIST-AREA (0)                  ;LIST CONSTANTS CREATED BY COLD LOAD
;Microcode -knows- that INIT-LIST-AREA is the last fixed area

;This location -must- immediately follow the above table of fixed areas
A-V-FIRST-UNFIXED-AREA (0)              ;First address above fixed areas

A-V-MISC-BASE   (0)     ;BASE OF DISPATCH TABLE FOR MISC-INST .
                        ; = A-V-MICRO-CODE-SYMBOL-AREA - 200

A-IPMARK        (0)     ;POINTER TO LAST OPEN CALL BLOCK ON IP STACK, (= AP IF NONE)
                        ; CAUTION! THIS IS A PDL-BUFFER ADDRESS NOT A VIRTUAL ONE.
A-PDL-BUFFER-VIRTUAL-ADDRESS    (0)     ;VIRTUAL ADDRESS OF "HEAD" OF PDL BUFFER
                ;IE THAT LOCATION OF PDL BUFFER THAT CORRESPONDS TO LOWEST VIRTUAL MEMORY
                ;LOCATION AT THE CURRENT TIME. PURE NUMBER WITH NO GARBAGE IN HIGH BITS
A-PDL-BUFFER-HEAD       (0)     ;PDL BUFFER INDEX CONSIDERED TO BE THE "HEAD"
                ;IE THAT LOCATION OF PDL BUFFER THAT CORRESPONDS TO LOWEST VIRTUAL
                ;MEMORY LOCATION AT THE CURRENT TIME.  PURE NUMBER, WITH NO GARBAGE IN
                ;HIGH BITS.

;UNTYPED LOCATIONS

A-LOCALP (0)            ;PDL-BUFFER-INDEX OF LOCALS FOR CURRENT FRAME
                        ; (NOT TRUNCATED TO 10 BITS!)

;this goes eventually goes here!  moved to uc-lambda for now to avoid offseting a-memory locations
;A-DEFAULT-CALL-STATE ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))  ;base used to build call state word of
;       ;call block by CBM.  Normally fixnum zero, has %%lp-cls-attention bit set if
;       ;by %set-meter-enables if any metering enabled.

;TEMPORARIES IN FUNCTION ENTRY CODE
 ;TO SAVE SPACE, SHARED WITH *THROW TEMPORARIES

;TEMPORARIES IN *CATCH, *THROW, ETC
A-LAST-STACK-GROUP              ;LAST STACK GROUP LEFT
                (0)     ;MARK, IE, WHAT MUST BE IN FEF POINTER OF DESIRED FRAME

A-SG-TEM2            ;ANOTHER SG TEMP.
A-ARGS-LEFT             ;NUMBER OF ARGS LEFT TO DO
A-CATCH-TAG     (0)     ;WHAT MUST BE IN FIRST ARG POSITION OF THAT FRAME

A-COPY-BAND-TEM
A-SG-TEM                ;TEMPS USED BY SG-CODE.  HOLD INFO OVER SGENT MOSTLY
A-CATCH-COUNT   (0)  ;CAUSES THROWAGE TO STOP, WITH THE CURRENT ACTIVE FRAME
                     ; RETURNING TO THE PREVIOUS FRAME, IF ZERO.  IF NIL, DOESN'T APPLY.
A-COPY-BAND-TEM1
A-SG-TEM1               ; CAN'T USE PDL BUFFER FOR THESE SINCE BEING SWAPPED.
A-CATCH-ACTION  (0)  ;IF NON-NIL, CAUSES RETURN TO ERROR SG INSTEAD OF RESUMING
                     ; CURRENT SG AT CONCLUSION OF THROW.

;PAGE TRACE
A-PAGE-TRACE-PTR (0)    ;0 DISABLED, ELSE ADDRESS OF NEXT 4-WORD ENTRY
A-PAGE-TRACE-START (0)  ;FIRST ENTRY
A-PAGE-TRACE-END (0)    ;LAST ENTRY+1 (WRAP-AROUND POINT)
A-PAGE-TRACE-VMA (0)    ;TEMP: ADDRESS REFERENCED
A-PAGE-TRACE-UPC (0)    ;TEMP: MICRO-PC AND SWAP-OUT FLAG

;;; Metering variables
A-METER-LENGTH  (0)                     ;Length of additional meter info
A-METER-EVENT   (0)                     ;Number of the metered even
A-METER-LOCK    (0)                     ;Lock during swap out of meter buffer
A-METER-START-TIME (0)                  ;Microsecond clock reading saved here

;CONNECTED WITH PDL-BUFFER MANAGEMENT
A-PDL-BUFFER-HIGH-WARNING  (PDL-BUFFER-HIGH-LIMIT)
;Go to PDL-BUFFER-DUMP if M-PDL-BUFFER-ACTIVE-QS is >= to this when frame is pushed.
;Normally stays at PDL-BUFFER-HIGH-LIMIT, but will be less
;if A-PDL-BUFFER-VIRTUAL-ADDRESS within PDL-BUFFER-SIZE-IN-WORDS of A-QLPDLH.
;(thus causing xfer to PDL-BUFFER-DUMP if pdl capacity is exceeded).

A-PDL-FINAL-VMA (0)     ;IN PDL DUMP, FINAL VMA TO DO, PLUS ONE
A-PDL-LOOP-COUNT (0)    ;IN PDL LOAD, LOOP COUNTER FOR INNER LOOP

;PDL POINTERS AND LIMITS.  INITIALIZED BY STACK-GROUP STUFF.
;THESE ARE PURE NUMBERS WITH NO DATA TYPE.
A-QLBNDP        (0)     ;BIND STACK (SPECIAL PDL) POINTER; ADDRESS OF HIGHEST VALID WORD
A-QLBNDO        (0)     ;LOW LIMIT OF BINDING STACK
A-QLBNDH        (0)     ;HIGH LIMIT OF BINDING STACK
A-QLBNDRH       (0)     ;MAXIMUM POSSIBLE HIGH LIMIT OF BINDING STACK
                        ;REGULAR PDL POINTER IS IN HARDWARE PDL-BUFFER-POINTER REGISTER
A-QLPDLO        (0)     ;LOW LIMIT OF REGULAR PDL
A-QLPDLH        (0)     ;HIGH LIMIT OF REGULAR PDL

a-new-region-search-offset ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))  ;base adr for search for
        ;new regions.  Set by arg to %gc-flip.

(assign array-cache-invalid (byte-value q-cdr-code cdr-nil))
(assign array-cache-valid (byte-value q-cdr-code cdr-next))
(assign general-array-cache-valid (byte-value q-cdr-code cdr-error))

a-array-cache-invalid (array-cache-invalid)
a-array-cache-valid (array-cache-valid)                 ;This must be CDR-NEXT.
a-general-array-cache-valid (general-array-cache-valid)

(assign invalidate-array-cache
        (plus byte-inst mr-bit q-all-but-cdr-code a-array-cache-invalid))
(assign validate-array-cache
        (plus byte-inst mr-bit q-all-but-cdr-code a-array-cache-valid))
(assign validate-general-array-cache
        (plus byte-inst mr-bit q-all-but-cdr-code a-general-array-cache-valid))

;;; A-memory variables for TV-DRAW-TRIANGLE, BITBLT, and COLD-RUN-DISK.
a-font-pointer
A-TRI-X1 (0)                    ;X1

a-font-origin
A-TRI-Y1 (0)                    ;Y1 (greatest)

a-font-raster-width
A-TRI-X2 (0)                    ;X2

a-font-raster-height
A-TRI-Y2 (0)                    ;Y2

a-font-words-per-char
A-TRI-X3 (0)                    ;X3

a-font-rows-per-word
A-TRI-Y3 (0)                    ;Y3 (smallest)

a-font-raster-shift
A-TRI-Y1-ADDR (0)               ;Y1 as array offset

A-BITBLT-DST-WIDTH              ;Width of destination region in bits
A-TRI-Y2-ADDR (0)               ;Y2

A-BITBLT-SRC-WIDTH              ;Width of source array in bits
A-TRI-Y3-ADDR (0)               ;Y3

A-BITBLT-SRC-WIDTH-WORDS        ;Width of source array in words
A-TRI-Y-LIM (0)                 ;Current goal

A-BITBLT-SRC-Y                  ;Number of rows down to start at in source
A-TRI-DET (0)                   ;Determinant giving handedness of triangle

A-BITBLT-SRC-Y-OFFSET           ;Same translated to word offset
A-TRI-XLI (0)                   ;Increment for left hand end of line

a-cold-run-disk-1
A-ALUF                          ;OA-REG-LOW for ALU function
A-TRI-XLIR (0)                  ;Increment for remainder

a-cold-run-disk-2
A-BITBLT-HOR-COUNT              ;Counter for horizontal loop
A-TRI-LY (0)                    ;DY for left hand point

a-cold-run-disk-t
A-BITBLT-COUNT                  ;Counter for vertical loop
A-TRI-XRI (0)                   ;Increment for right hand end of line

a-cold-run-disk-first-b
A-BITBLT-TEM                    ;Temporary in inner loop
A-TRI-XRIR (0)                  ;Increment for remainder

a-cold-run-disk-b
A-TRI-RY (0)                    ;DY

;;; A-memory locations for TV-DRAW-LINE
a-fill-memory-bound
A-DRAW-LINE-DRAW-FIRST-POINT (0)        ;Fill in first point on line
A-DRAW-LINE-DRAW-LAST-POINT (0)         ;... last point ...

;;; A-memory location for XTVERS9 [used by %draw-rectangle and %draw-triangle]
A-REPLICATED-PIXEL-WORD (0)

;;; A-memory locations for BBOOLE
A-BOOLE-CARRY-1 (0)
A-BOOLE-CARRY-2 (0)

;;; A-memory locations used by bignum-bignum division. 31. bit numbers both.
A-BIDIV-V1      (0)
A-BIDIV-V2      (0)

;;; LOCATIONS OF THE MAIN LOOP INDEXED BY %%SG-ST-INST-DISP OF SG-CURRENT-STATE
A-MAIN-DISPATCH         ((PLUS (BYTE-MASK %%-PPBMIR) (I-MEM-LOC QMLP)))
A-DEBUG-DISPATCH        ((PLUS (BYTE-MASK %%-PPBMIR) (I-MEM-LOC DMLP)))
A-SINGLE-STEP-DISPATCH  ((PLUS (BYTE-MASK %%-PPBMIR) (I-MEM-LOC SINGLE-STEP)))
A-SINGLE-STEP-TRAP      ((PLUS (BYTE-MASK %%-PPBMIR) (I-MEM-LOC STEP-BREAK)))

;FIRST LEVEL MAP STUFF

A-SECOND-LEVEL-MAP-REUSE-POINTER
        (35)            ;-> BLOCK OF SECOND LEVEL MAP NEXT TO BE REUSED
A-SECOND-LEVEL-MAP-REUSE-POINTER-INIT
        (0)             ;-> LOWEST NUMBERED BLOCK SAFE TO REUSE
;(MODULO 40)
;A-REVERSE-FIRST-LEVEL-MAP
;       (0)             ;FOR EACH BLOCK OF SECOND LEVEL MAP, SAYS WHICH FIRST
;(REPEAT 36 (-1))       ;LEVEL MAP WORD POINTS TO THIS BLOCK CURRENTLY.
;                       ;IS IN FORM OF A VMA TO ADDRESS THAT 1ST LVL MAP ENTRY.
;A-REVERSE-FIRST-LEVEL-MAP-INIT-VALUE
;       (-1)    ;THIS ONE IS NEVER CHANGED (ENTRY #37)

;Variables for mouse tracking
A-MOUSE-INTERNAL-X (0)  ;Used to save up mouse motion until big enough to change A-MOUSE-X.
A-MOUSE-INTERNAL-Y (0)
A-MOUSE-PREVIOUS-INTERNAL-X (0) ;Value of A-MOUSE-INTERNAL-X that goes with current A-MOUSE-X.
A-MOUSE-PREVIOUS-INTERNAL-Y (0)
A-MOUSE-X-FRACTION (0)  ;10 bits of fractional position
A-MOUSE-Y-FRACTION (0)
A-MOUSE-CURSOR-X (0)    ;Current location of cursor
A-MOUSE-CURSOR-Y (0)    ; (only valid if state=3)
A-MOUSE-LAST-H1 (0)     ;Last value input from hardware
A-MOUSE-LAST-H2 (0)
#-cadr (begin-comment)
A-MOUSE-HARDWARE-ADDRESS (MOUSE-HARDWARE-VIRTUAL-ADDRESS)       ;764104 is Y, 764106 is X
#-cadr (end-comment)
A-MOUSE-SCREEN-BUFFER-ADDRESS (0)       ;Data for screen (or sheet) mouse is on
A-MOUSE-SCREEN-BUFFER-END-ADDRESS (0)
A-MOUSE-SCREEN-LOCATIONS-PER-LINE (0)
A-MOUSE-SCREEN-BUFFER-BIT-OFFSET (0)
A-MOUSE-SCREEN-WIDTH (0)
A-MOUSE-SCREEN-BUFFER-PIXEL-SIZE-MROT (0)
A-MOUSE-SCREEN (0)
A-MOUSE-SAVE-1 (0)
A-MOUSE-SAVE-2 (0)
A-MOUSE-SAVE-E (0)

a-video-buffer-base-phys-page (0)

;Arrays at fixed locations in A memory, used for the mouse
; These are indirected to by LISP arrays in the world load!
;(ASSIGN MOUSE-CURSOR-PATTERN-AMEM-LOC 1600)    ;32x32 BIT ARRAY
;(ASSIGN MOUSE-BUTTONS-BUFFER-AMEM-LOC 1640)    ;8 4-WORD ART-Q ENTRIES
;(ASSIGN MOUSE-X-SCALE-ARRAY-AMEM-LOC 1700)     ;8 2-WORD ART-Q ENTRIES
;(ASSIGN MOUSE-Y-SCALE-ARRAY-AMEM-LOC 1720)     ;8 2-WORD ART-Q ENTRIES

;;; DISPATCH TABLES

(LOCALITY D-MEM)

;Last location in D-memory must be drop through for transporter to work right.
#+cadr(LOC 3777)
#+lambda(LOC 7777)
#+exp (LOC 3777)        ;Avoid high D-MEM USED for macro-instruction dispatch.
LAST-DMEM-LOCATION
all-ones-in-d-mem       (P-BIT R-BIT)
(END-DISPATCH)

(START-DISPATCH 1 0)
;USE THIS DISPATCH WITH MULTI-UNIT INSTRUCTIONS.  FETCHES FROM MAIN MEM IF NECESSARY.
D-ADVANCE-INSTRUCTION-STREAM
#+cadr  (P-BIT R-BIT)   ;DROP THRU
#+cadr  (P-BIT INHIBIT-XCT-NEXT-BIT INSTRUCTION-STREAM-FETCHER)
#+lambda(P-BIT INHIBIT-XCT-NEXT-BIT INSTRUCTION-STREAM-FETCHER) ;BIT INVERTED ON LAMBDA.
#+lambda(P-BIT R-BIT)   ;DROP THRU
#+exp   (P-BIT R-BIT)   ;DROP THRU
#+exp   (P-BIT INHIBIT-XCT-NEXT-BIT INSTRUCTION-STREAM-FETCHER)
(END-DISPATCH)


;DISPATCH TO EXECUTE ROUTINE FOR CXR, JUMP-XCT-NEXT.
(start-dispatch 5 0)
cxr-execute-jump-xct-next
        (P-BIT TRAP)    ;0 CALL
        (P-BIT TRAP)    ;1 CALL0
        (P-BIT TRAP)    ;2 MOVE
        (QCAR)          ;3 CAR
        (QCDR)          ;4 CDR
        (QCADR)         ;5 CADR
        (QCDDR)         ;6 CDDR
        (QCDAR)         ;7 CDAR
        (QCAAR)         ;10 CAAR
        (P-BIT TRAP)    ;11 ND1
        (P-BIT TRAP)    ;12 ND2
        (P-BIT TRAP)    ;13 ND3
        (P-BIT TRAP)    ;14 BRANCH
        (P-BIT TRAP)    ;15 MISC
        (P-BIT TRAP)    ;16 UNUSED
        (P-BIT TRAP)    ;17 UNUSED
(repeat 20 (p-bit trap))
(end-dispatch)

;DISPATCH TO EXECUTE ROUTINE FOR CXR, CALL-XCT-NEXT.
(start-dispatch 5 0)
cxr-execute-call-xct-next
        (P-BIT TRAP)    ;0 CALL
        (P-BIT TRAP)    ;1 CALL0
        (P-BIT TRAP)    ;2 MOVE
        (P-BIT QCAR)    ;3 CAR
        (P-BIT QCDR)    ;4 CDR
        (P-BIT QCADR)   ;5 CADR
        (P-BIT QCDDR)   ;6 CDDR
        (P-BIT QCDAR)   ;7 CDAR
        (P-BIT QCAAR)   ;10 CAAR
        (P-BIT TRAP)    ;11 ND1
        (P-BIT TRAP)    ;12 ND2
        (P-BIT TRAP)    ;13 ND3
        (P-BIT TRAP)    ;14 BRANCH
        (P-BIT TRAP)    ;15 MISC
        (P-BIT TRAP)    ;16 UNUSED
        (P-BIT TRAP)    ;17 UNUSED
(repeat 20 (p-bit trap))
(end-dispatch)


;This needed even on Lambda.  Used at QIMOVE-EXIT, and maybe other places now.

(START-DISPATCH 3 0)
;DESTINATION CODE DISPATCH, VALUE IN M-T
;EACH USE OF THIS DISPATCH TABLE MUST BE FOLLOWED BY A PUSH OF M-T WITH CDR-NEXT SET
QMDTBD  (R-BIT INHIBIT-XCT-NEXT-BIT)            ;IGNORE (POPJ IMMEDIATELY)
        (R-BIT)                                 ;D-PDL  (also OK on lambda with PDL-PASS fix)
        (QMDDR INHIBIT-XCT-NEXT-BIT)            ;D-RETURN
        (QMDDL INHIBIT-XCT-NEXT-BIT)            ;D-LAST
        (R-BIT INHIBIT-XCT-NEXT-BIT)            ;D-MICRO, POPJ TO THE MICROCODE
(REPEAT 3 (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

#-cadr (begin-comment)  ;needed if no jump-if-data-type-equal, etc
(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE.  DROPS THROUGH IN EITHER CASE BUT SKIPS IF ATOM.
;AN ATOM IS ANYTHING OTHER THAN A LIST.
SKIP-IF-ATOM
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SYMBOL HEADER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;FIX
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;LOCATIVE
        (P-BIT R-BIT 0)                         ;LIST -- don't skip
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;U CODE ENTRY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;FEF
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ARRAY-HEADER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;STACK-GROUP
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;SELECT-METHOD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;INSTANCE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INSTANCE-HEADER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;ENTITY
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;CHARACTER
        (p-bit n-bit illop)                     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE.  DROPS THROUGH IN EITHER CASE BUT SKIPS IF NOT ATOM.
;AN ATOM IS ANYTHING OTHER THAN A LIST.
;This exists for "symmetry" with SKIP-IF-ATOM for the microcompiler.
;Its not clear that either of these is really used enuf to justify their own
;dispatch tables.  But for now, this is the easiest thing.
SKIP-IF-NO-ATOM
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (P-BIT R-BIT 0)                         ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SYMBOL HEADER
        (P-BIT R-BIT 0)                         ;FIX
        (P-BIT R-BIT 0)                         ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT R-BIT 0)                         ;LOCATIVE
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;LIST -- skip
        (P-BIT R-BIT 0)                         ;U CODE ENTRY
        (P-BIT R-BIT 0)                         ;FEF
        (P-BIT R-BIT 0)                         ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ARRAY-HEADER
        (P-BIT R-BIT 0)                         ;STACK-GROUP
        (P-BIT R-BIT 0)                         ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT R-BIT 0)                         ;SELECT-METHOD
        (P-BIT R-BIT 0)                         ;INSTANCE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INSTANCE-HEADER
        (P-BIT R-BIT 0)                         ;ENTITY
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
        (P-BIT R-BIT 0)                         ;CHARACTER
        (p-bit n-bit illop)                     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;spare
        (P-BIT R-BIT 0)                         ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE.  DROPS THROUGH IN EITHER CASE BUT SKIPS IF LIST.
SKIP-IF-LIST
        (P-BIT R-BIT 0)                 ;TRAP
        (P-BIT R-BIT 0)                 ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP)                 ;UNRECONCILED
        (P-BIT R-BIT 0)                 ;SYMBOL
        (P-BIT R-BIT 0)                 ;SYMBOL-HEADER
        (P-BIT R-BIT 0)                 ;FIX
        (P-BIT R-BIT 0)                 ;EXTENDED NUMBER
        (P-BIT R-BIT 0)                 ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (P-BIT R-BIT 0)                 ;LOCATIVE
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;LIST
        (P-BIT R-BIT 0)                 ;U CODE ENTRY
        (P-BIT R-BIT 0)                 ;FEF
        (P-BIT R-BIT 0)                 ;ARRAY-POINTER
        (P-BIT R-BIT 0)                 ;ARRAY-HEADER
        (P-BIT R-BIT 0)                 ;STACK-GROUP
        (P-BIT R-BIT 0)                 ;CLOSURE [NOT A LIST FOR PURPOSES OF THIS]
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (P-BIT R-BIT 0)                 ;SELECT-METHOD [NOT A LIST FOR PURPOSES OF THIS]
        (P-BIT R-BIT 0)                 ;INSTANCE [NOT A LIST FOR PURPOSES OF THIS]
        (P-BIT R-BIT 0)                 ;INSTANCE-HEADER [NOT A LIST FOR PURPOSES OF THIS]
        (P-BIT R-BIT 0)                 ;ENTITY [NOT A LIST FOR PURPOSES OF THIS]
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
        (P-BIT R-BIT 0)                 ;CHARACTER
        (p-bit n-bit illop)             ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP) ;spare
        (P-BIT R-BIT 0)                 ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)
#-cadr (end-comment)

(START-DISPATCH 5 0)

;*** MERGE LOSSAGE ***
;*** File LAM3: UCADR; UC-PARAMETERS.LISP#262 HAS:
;DISPATCH ON DATA-TYPE OF FUNCTION GETTING CALLED (AT QMRCL OR MMCALL).
;THE FUNCTION MUST BE IN M-A.
;M-AP HAS THE ADDRESS OF THE NEW FRAME, M-R HAS THE NUMBER OF ARGUMENTS.
;JUMPS TO APPROPRIATE CODE TO CALL THAT KIND OF FUNCTION, OR INTERPRETER TRAP.
;M-S HAS FRAME BEING LEFT.
;**NO LONGER, LEAVE ALWAYS DONE BEFORE DISPATCH NOW**
;*** File LAM3: QL.ULAMBDA; UC-PARAMETERS.LISP#248 HAS:
;DISPATCH ON DATA-TYPE OF FUNCTION GETTING CALLED (AT QMRCL OR MMCALL).
;THE FUNCTION MUST BE IN BOTH M-A AND C-PDL-BUFFER-INDEX
;M-S HAS THE ADDRESS OF THE NEW FRAME, M-R HAS THE NUMBER OF ARGUMENTS.
;JUMPS TO APPROPRIATE CODE TO CALL THAT KIND OF FUNCTION, OR INTERPRETER TRAP.
;*** END OF MERGE LOSSAGE ***

; INHIBIT-XCT-NEXT-BIT IS OFF IF A "LEAVE" IS INDICATED.
;  A "LEAVE" IS INDICATED UNLESS
;       (1) A LOOP AROUND TYPE OPERATION IS PLANNED, IE SYM, INVZ
;       (2) DTP-ARRAY-POINTER.  HERE A LEAVE IS UNNECESSARY BECAUSE THE LINEAR PDL STATE
;               ISNT REALLY GOING TO GET CLOBBERED.
;THIS DISPATCH IS DESIGNED TO BE USED FROM BOTH QMRCL AND MMCALL.
;If the caller is NOT normally going to do a leave, he should specify I-ARG 1.
;This is because, if an array is really a hash table, it will have to
;do the leave which was inhibited.  If the I-ARG is 1, it does not do the leave.

D-QMRCL (P-BIT ILLOP)                   ;TRAP
        (P-BIT ILLOP)                   ;NULL
        (P-BIT UNRECONCILED-ILLOP)      ;UNRECONCILED
        (QMRCL1)                        ;SYMBOL
        (P-BIT ILLOP)                   ;SYMBOL-HEADER
        (NUMBER-CALLED-AS-FUNCTION)     ;FIX
        (NUMBER-CALLED-AS-FUNCTION)     ;EXTENDED NUMBER
        (P-BIT ILLOP)                   ;HEADER
        (P-BIT ILLOP)   ;GC-FORWARD
        (P-BIT ILLOP)   ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT ILLOP)   ;ONE-Q-FORWARD
        (P-BIT ILLOP)   ;HEADER-FORWARD
        (P-BIT ILLOP)   ;BODY-FORWARD
        (INTP1)                         ;LOCATIVE
        (INTP1)                         ;LIST
        (QME1)                          ;U CODE ENTRY
d-qmrcl-qlentr  ;modified if metering turned on or off.
#+CADR  (QLENTR)                        ;FEF
#+LAMBDA(QLENTR DISPATCH-START-MEM-READ-BIT)
#+EXP   (QLENTR)                        ;FEF
        (QARYR)                         ;ARRAY-POINTER
        (P-BIT ILLOP)                   ;ARRAY-HEADER
        (SG-CALL)                       ;STACK-GROUP
        (QCLS)                          ;CLOSURE
        (P-BIT ILLOP)                   ;INDEXED-FORWARD
        (CALL-SELECT-METHOD)            ;SELECT-METHOD
        (CALL-INSTANCE)                 ;INSTANCE
        (P-BIT ILLOP)                   ;INSTANCE-HEADER
        (CALL-ENTITY)                   ;ENTITY
        (P-BIT ILLOP)                   ;unused-32
        (P-BIT ILLOP)                   ;SELF-REF-POINTER
        (NUMBER-CALLED-AS-FUNCTION)     ;CHARACTER
        (p-bit illop)                   ;rplacd-forward
        (P-BIT ILLOP)                   ;spare
        (NUMBER-CALLED-AS-FUNCTION)     ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT ILLOP))
(END-DISPATCH)

(START-DISPATCH 5 P-BIT)
;TRAP UNLESS DATA TYPE IS FIXNUM.
TRAP-UNLESS-FIXNUM
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;TRAP
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;NULL
        (INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP)       ;UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SYMBOL
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SYMBOL-HEADER
        (R-BIT)                         ;FIX  P-BIT AND R-BIT CAUSE DISPATCH TO BE A NO-OP
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;EXTENDED NUMBER - IS THIS RIGHT?
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;GC-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;LOCATIVE
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;LIST
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;U CODE ENTRY
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;FEF
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ARRAY-POINTER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ARRAY-HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;STACK-GROUP
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;CLOSURE
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;INSTANCE
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;INSTANCE-HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ENTITY
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;unused-32
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SELF-REF-POINTER
        (R-BIT)                         ;CHARACTER  P-BIT + R-BIT CAUSE DISPATCH TO BE A NO-OP
        (n-bit trap)                    ;rplacd-forward
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;spare
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SMALL-FLONUM
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT TRAP))
(END-DISPATCH)

(START-DISPATCH 5 P-BIT)
;TRAP UNLESS DATA TYPE IS FIXNUM.
array-trap-unless-fixnum
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;ARRAY-TRAP
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;NULL
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;SYMBOL
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;SYMBOL-HEADER
        (R-BIT)                         ;FIX  P-BIT AND R-BIT CAUSE DISPATCH TO BE A NO-OP
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;EXTENDED NUMBER - IS THIS RIGHT?
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;HEADER
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;GC-FORWARD
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;LOCATIVE
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;LIST
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;U CODE ENTRY
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;FEF
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;ARRAY-POINTER
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;ARRAY-HEADER
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;STACK-GROUP
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;CLOSURE
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;INSTANCE
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;INSTANCE-HEADER
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;ENTITY
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;unused-32
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;SELF-REF-POINTER
        (R-BIT)                         ;CHARACTER  P-BIT + R-BIT CAUSE DISPATCH TO BE A NO-OP
        (n-bit array-trap)                      ;rplacd-forward
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;spare
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)       ;SMALL-FLONUM
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP))
(END-DISPATCH)

#+cadr (begin-comment)
(START-DISPATCH 5 0)
;used to speed up calls to GAHDR, etc.  Always does XCT-NEXT.  By the time we arrive at
;GAHDR-D the array pointer must be in M-A.  TRAPS go to GAHDR-TRAP so error
;handling will be consistant.
array-header-setup-dispatch
        (P-BIT GAHDR-TRAP)      ;TRAP
        (P-BIT GAHDR-TRAP)      ;NULL
        (P-BIT UNRECONCILED-TRAP)       ;UNRECONCILED
        (P-BIT GAHDR-TRAP)      ;SYMBOL
        (P-BIT GAHDR-TRAP)      ;SYMBOL-HEADER
        (P-BIT GAHDR-TRAP)      ;FIX
        (P-BIT GAHDR-TRAP)      ;EXTENDED NUMBER - IS THIS RIGHT?
        (P-BIT GAHDR-TRAP)      ;HEADER
        (P-BIT GAHDR-TRAP)      ;GC-FORWARD
        (P-BIT GAHDR-TRAP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT GAHDR-TRAP)      ;ONE-Q-FORWARD
        (P-BIT GAHDR-TRAP)      ;HEADER-FORWARD
        (P-BIT GAHDR-TRAP)      ;BODY-FORWARD
        (P-BIT GAHDR-TRAP)      ;LOCATIVE
        (P-BIT GAHDR-TRAP)      ;LIST
        (P-BIT GAHDR-TRAP)      ;U CODE ENTRY
        (P-BIT GAHDR-TRAP)      ;FEF
        (P-BIT GAHDR-D) ;ARRAY-POINTER
        (P-BIT GAHDR-TRAP)      ;ARRAY-HEADER
        (P-BIT GAHDR-D) ;STACK-GROUP
        (P-BIT GAHDR-TRAP)      ;CLOSURE
        (P-BIT GAHDR-TRAP)      ;INDEXED-FORWARD
        (P-BIT GAHDR-TRAP)      ;SELECT-METHOD
        (P-BIT GAHDR-TRAP)      ;INSTANCE
        (P-BIT GAHDR-TRAP)      ;INSTANCE-HEADER
        (P-BIT GAHDR-TRAP)      ;ENTITY
        (P-BIT GAHDR-TRAP)      ;unused-32
        (P-BIT GAHDR-TRAP)      ;SELF-REF-POINTER
        (P-BIT GAHDR-TRAP)      ;CHARACTER
        (p-bit gahdr-trap)      ;rplacd-forward
        (P-BIT GAHDR-TRAP)      ;spare
        (P-BIT GAHDR-TRAP)      ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT GAHDR-TRAP))
(END-DISPATCH)
#+cadr (end-comment)

(START-DISPATCH 5 P-BIT);DOES CALL-XCT-NEXT
;DISPATCH ON ARRAY TYPE WHEN REF ING ARRAY
ARRAY-TYPE-REF-DISPATCH
        (INHIBIT-XCT-NEXT-BIT array-trap)
        (QB1RY)         ;BIT ARRAY
        (QB2RY)         ;2 BIT ARRAY
        (QB4RY)         ;4 BIT ARRAY
        (QBARY)         ;8 BIT ARRAY
        (QB16RY)        ;16 BIT ARRAY
        (qinary) ;(QB32RY)      ;32 BIT ARRAY
        (QQARY)         ;Q ARRAY
        (QQARY)         ;LIST Q ARRAY
        (QBARY)         ;STRING ARRAY
        (QQARY)         ;STACK-GROUP HEAD
        (QQARY)         ;SPEC-PDL
        (QB16SRY)       ;HALF-FIX
        (QQARY)         ;REG-PDL
        (QFARY)         ;FLOAT
        (QFFARY)        ;FPS-FLOAT
        (QB16RY)        ;FAT-STRING
        (QCFARY)        ;COMPLEX-FLOAT
        (QCARY)         ;COMPLEX
        (QCFFARY)       ;COMPLEX-FPS-FLOAT.
        (qinary)        ;INUM
 (REPEAT NATUSD (INHIBIT-XCT-NEXT-BIT array-trap))
(END-DISPATCH)

(START-DISPATCH 5);DOES JUMP-XCT-NEXT
;DISPATCH ON ARRAY TYPE WHEN REF ING ARRAY
ARRAY-TYPE-REF-DISPATCH-JUMP
        (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP)
        (QB1RY)         ;BIT ARRAY
        (QB2RY)         ;2 BIT ARRAY
        (QB4RY)         ;4 BIT ARRAY
        (QBARY)         ;8 BIT ARRAY
        (QB16RY)        ;16 BIT ARRAY
        (qinary) ;(QB32RY)      ;32 BIT ARRAY
        (QQARY)         ;Q ARRAY
        (QQARY)         ;LIST Q ARRAY
        (QBARY)         ;STRING ARRAY
        (QQARY)         ;STACK-GROUP HEAD
        (QQARY)         ;SPEC-PDL
        (QB16SRY)       ;HALF-FIX
        (QQARY)         ;REG-PDL
        (QFARY)         ;FLOAT
        (QFFARY)        ;FPS-FLOAT
        (QB16RY)        ;FAT-STRING
        (QCFARY)        ;COMPLEX-FLOAT
        (QCARY)         ;COMPLEX
        (QCFFARY)       ;COMPLEX-FPS-FLOAT.
        (qinary)        ;INUM
 (REPEAT NATUSD (INHIBIT-XCT-NEXT-BIT ARRAY-TRAP))
(END-DISPATCH)

;Array references for Common Lisp, where strings contain characters.
(START-DISPATCH 5 P-BIT);DOES CALL-XCT-NEXT
COMMON-LISP-ARRAY-TYPE-REF-DISPATCH
        (INHIBIT-XCT-NEXT-BIT array-trap)
        (QB1RY)         ;BIT ARRAY
        (QB2RY)         ;2 BIT ARRAY
        (QB4RY)         ;4 BIT ARRAY
        (QBARY)         ;8 BIT ARRAY
        (QB16RY)        ;16 BIT ARRAY
        (qinary) ;(QB32RY)      ;32 BIT ARRAY
        (QQARY)         ;Q ARRAY
        (QQARY)         ;LIST Q ARRAY
        (QBARY-COMMON-LISP) ;STRING ARRAY
        (QQARY)         ;STACK-GROUP HEAD
        (QQARY)         ;SPEC-PDL
        (QB16SRY)       ;HALF-FIX
        (QQARY)         ;REG-PDL
        (QFARY)         ;FLOAT
        (QFFARY)        ;FPS-FLOAT
        (QB16RY)        ;FAT-STRING
        (QCFARY)        ;COMPLEX-FLOAT
        (QCARY)         ;COMPLEX
        (QCFFARY)       ;COMPLEX-FPS-FLOAT.
        (qinary)        ;INUM
 (REPEAT NATUSD (INHIBIT-XCT-NEXT-BIT array-trap))
(END-DISPATCH)

(START-DISPATCH 5);DOES JUMP-XCT-NEXT
COMMON-LISP-ARRAY-TYPE-REF-DISPATCH-JUMP
        (INHIBIT-XCT-NEXT-BIT array-trap)
        (QB1RY)         ;BIT ARRAY
        (QB2RY)         ;2 BIT ARRAY
        (QB4RY)         ;4 BIT ARRAY
        (QBARY)         ;8 BIT ARRAY
        (QB16RY)        ;16 BIT ARRAY
        (qinary) ;(QB32RY)      ;32 BIT ARRAY
        (QQARY)         ;Q ARRAY
        (QQARY)         ;LIST Q ARRAY
        (QBARY-COMMON-LISP) ;STRING ARRAY
        (QQARY)         ;STACK-GROUP HEAD
        (QQARY)         ;SPEC-PDL
        (QB16SRY)       ;HALF-FIX
        (QQARY)         ;REG-PDL
        (QFARY)         ;FLOAT
        (QFFARY)        ;FPS-FLOAT
        (QB16RY)        ;FAT-STRING
        (QCFARY)        ;COMPLEX-FLOAT
        (QCARY)         ;COMPLEX
        (QCFFARY)       ;COMPLEX-FPS-FLOAT.
        (qinary)        ;INUM
 (REPEAT NATUSD (INHIBIT-XCT-NEXT-BIT array-trap))
(END-DISPATCH)

(START-DISPATCH 5 0)    ;DOES XCT-NEXT
;DISPATCH ON ARRAY TYPE WHEN STORING INTO ARRAY
ARRAY-TYPE-STORE-DISPATCH
        (P-BIT INHIBIT-XCT-NEXT-BIT array-trap)
        (QS1RY)         ;BIT ARRAY
        (QS2RY)         ;2 BIT ARRAY
        (QS4RY)         ;4 BIT ARRAY
        (QSBARY)        ;8 BIT ARRAY
        (QS16RY)        ;16 BIT ARRAY
        (qsinary) ;(QS32RY)     ;32 BIT ARRAY
        (QSQARY)        ;Q ARRAY
        (QSLQRY)        ;LIST Q ARRAY
        (QSBARY)        ;BYTE ARRAY
        (QSQARY)        ;STACK-GROUP HEAD
        (QSQARY)        ;SPEC-PDL
        (QS16RY)        ;HALF-FIX
        (QSQARY)        ;REG-PDL
        (QSFARY)        ;FLOAT
        (QSFFARY)       ;FPS-FLOAT
        (QS16RY)        ;FAT-STRING
        (QSCFARY)       ;COMPLEX-FLOAT
        (QSCARY)        ;COMPLEX
        (QSCFFARY)      ;COMPLEX-FPS-FLOAT.
        (qsinary)       ;INUM
 (REPEAT NATUSD (P-BIT INHIBIT-XCT-NEXT-BIT array-trap))
(END-DISPATCH)

(START-DISPATCH 5 0)    ;DOES XCT-NEXT
;DISPATCH ON ARRAY TYPE WHEN STORING INTO ARRAY
; this one the same as above except it PUSHJs.  That is needed by micro-compiled code.
ARRAY-TYPE-STORE-DISPATCH-PUSHJ
        (P-BIT INHIBIT-XCT-NEXT-BIT array-trap)
        (P-BIT QS1RY)           ;BIT ARRAY
        (P-BIT QS2RY)           ;2 BIT ARRAY
        (P-BIT QS4RY)           ;4 BIT ARRAY
        (P-BIT QSBARY)  ;8 BIT ARRAY
        (P-BIT QS16RY)  ;16 BIT ARRAY
        (p-bit qsinary)    ;(P-BIT QS32RY)      ;32 BIT ARRAY
        (P-BIT QSQARY)  ;Q ARRAY
        (P-BIT QSLQRY)  ;LIST Q ARRAY
        (P-BIT QSBARY)  ;BYTE ARRAY
        (P-BIT QSQARY)  ;STACK-GROUP HEAD
        (P-BIT QSQARY)  ;SPEC-PDL
        (P-BIT QS16RY)  ;HALF-FIX
        (P-BIT QSQARY)  ;REG-PDL
        (P-BIT QSFARY)  ;FLOAT
        (P-BIT QSFFARY) ;FPS-FLOAT
        (P-BIT QS16RY)  ;FAT-STRING
        (P-BIT QSCFARY) ;COMPLEX-FLOAT
        (P-BIT QSCARY)  ;COMPLEX
        (P-BIT QSCFFARY)        ;COMPLEX-FPS-FLOAT.
        (P-BIT QSINARY) ;INUM
 (REPEAT NATUSD (P-BIT INHIBIT-XCT-NEXT-BIT array-trap))
(END-DISPATCH)

(START-DISPATCH 5 (PLUS P-BIT INHIBIT-XCT-NEXT-BIT))
;DISPATCH ON ARRAY TYPE AT XFARY
ARRAY-TYPE-FILL-DISPATCH
        (array-trap)
        (QS1RY)         ;BIT ARRAY
        (QS2RY)         ;2
        (QS4RY)         ;4
        (QSBARY)        ;8
        (QS16RY)        ;16
        (qsinary) ;(QS32RY)     ;32
        (QSQARY)        ;Q ARRAY
        (XFALAR)        ;LIST -- SPECIAL HACKERY WITH CDR CODES TO EXTEND "LIST"
        (QSBARY)        ;BYTE
        (QSQARY)        ;STACK-GROUP HEAD
        (QSQARY)        ;SPEC-PDL
        (QS16RY)        ;HALF-FIX
        (QSQARY)        ;REG-PDL
        (QSFARY)        ;FLOAT
        (QSFFARY)       ;FPS-FLOAT
        (QS16RY)        ;FAT-STRING
        (QSCFARY)       ;COMPLEX-FLOAT
        (QSCARY)        ;COMPLEX
        (QSCFFARY)      ;COMPLEX-FPS-FLOAT.
        (QSINARY)       ;INUM
 (REPEAT NATUSD (array-trap))
(END-DISPATCH)

;Skip for an array type which can contain only numbers.
;Note: this does not necessarily imply that the words of the array are unboxed!
;For ART-COMPLEX, they are boxed.
(START-DISPATCH 5 0) ;DISPATCH ON ARRAY TYPE
SKIP-IF-NUMERIC-ARRAY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ERROR
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;BIT ARRAY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;2 BIT ARRAY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;4 BIT ARRAY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;8 BIT ARRAY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;16 BIT ARRAY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;32 BIT ARRAY
        (P-BIT R-BIT)                           ;Q ARRAY
        (P-BIT R-BIT)                           ;LIST Q ARRAY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;STRING ARRAY
        (P-BIT R-BIT)                           ;STACK-GROUP HEAD
        (P-BIT R-BIT)                           ;SPEC-PDL
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;HALF-FIX
        (P-BIT R-BIT)                           ;REG-PDL
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;FLOAT
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;FPS-FLOAT
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;FAT-STRING
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;COMPLEX-FLOAT
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;COMPLEX
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;COMPLEX-FPS-FLOAT
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;INUM
 (REPEAT NATUSD (P-BIT INHIBIT-XCT-NEXT-BIT TRAP))
(END-DISPATCH)

;DISPATCH ON TYPE OF ARG, ARG IS SUPPLIED
(START-DISPATCH 3 0)
QREDT1  (QBRQA)         ;REQUIRED ARG
        (QBROP1)        ;OPTIONAL ARG
        (QBRA)          ;REST ARG
        (QBTMA2)        ;AUX VAR, (HAVE REACHED END OF ARG SECT, WITH MORE ARGS)
        (QBTMA2)        ;FREE,      ..
        (QBTMA2)        ;INTERNAL,  ..
        (QBTMA2)        ;INTERNAL-AUX,  ..
        (P-BIT ILLOP)   ;UNUSED
(END-DISPATCH)

;DISPATCH ON INITIALIZING OPTION, OPT ARG HAS BEEN SUPPLIED
(START-DISPATCH 3 INHIBIT-XCT-NEXT-BIT)
QBOPNP  (QBRQA)         ;NONE
        (QBRQA)         ;NIL
        (QBOSP)         ;INIT TO POINTER (SPACE PAST)
        (QBOSP)         ;INIT TO C(POINTER) (LIKEWISE)
        (QBOASA)        ;ALT STARTING ADR, START THERE TO AVOID CLOBBERING IT
        (QBRQA)         ;INIT BY COMPILED CODE
        (QBOSP)         ;INIT TO C(EFF ADR)
        (QBRQA)         ;INIT TO SELF
(END-DISPATCH)

;DISPATCH ON TYPE OF NEXT B-D-L ENTRY AFTER ALL PRESENT ARGS HAVE BEEN PROCESSED
(START-DISPATCH 3 0)    ;DOES XCT-NEXT
QBDT2   (QBTFA1)        ;THIS WAS A REQUIRED ARG, BARF
        (QBOPT1)        ;THIS WAS AN OPT ARG, NOT PRESENT
        (QBRA1)         ;THIS WAS A REST ARG, SET IT TO NIL
        (QBDAUX)        ;AUX VAR, REALLY END OF ARG PART OF B-D-L
        (QBDFRE)        ;FREE,     ..
        (QBDINT)        ;INTERNAL, ..
        (QBDINT)        ;INTERNAL-AUX, ..
        (P-BIT ILLOP)
(END-DISPATCH)

;DISPATCH ON INTIALIZING OPTION, GOING TO INITIALIZE VARIABLE
(START-DISPATCH 3 INHIBIT-XCT-NEXT-BIT)
QBOPTT  (QBOPT3)        ;NONE
        (QBOPT2)        ;NIL
        (QBOPNR)        ;INIT TO POINTER
        (QBOCPT)        ;INIT TO C(POINTER)
        (QBOPT5)        ;OPT ARG, ALT SA
                        ;ARG NOT PRESENT, SO LEAVE STARTING ADR ALONE
                        ;AND LET CODE INIT IT
        (QBOPT3)        ;INIT BY COMPILED CODE
        (QBOEFF)        ;INIT TO CONTENTS OF "EFFECTIVE ADDRESS"
        (QBOPT3)        ;INIT TO SELF (SAME AS NONE)
(END-DISPATCH)

;DISPATCH ON DESIRED DATA TYPE FOR ARG
;(START-DISPATCH 4 0)   ;DOES NXT INSTR
;QBDDT  (R-BIT P-BIT)   ;0 NO DATA TYPE CHECKING
;       (QDTN)          ;1 NUMBER
;       (QDTFXN)        ;2 FIXNUM
;       (QDTSYM)        ;3 SYMBOL
;       (QDTATM)        ;4 ATOM
;       (QDTLST)        ;5 LIST
;       (QDTFRM)        ;6 FRAME
;(REPEAT 11 (P-BIT ILLOP))      ;UNDEF CODE
;(END-DISPATCH)

;DISPATCH ON DESIRED EVAL/QUOTE STATUS FOR ARG
;(START-DISPATCH 2 0)   ;DOES NXT INSTR
;QBEQC  (R-BIT P-BIT)   ;0 NO CHECKING
;       (QBEQE)         ;1 DESIRED EVALUATED
;       (QBEQQ)         ;2 DESIRED QUOTED
;       (QBEQQ)         ;3 DESIRED BROKEN-OFF
;(END-DISPATCH)

;DISPATCH ON REGISTER FIELD OF EFF ADDR FOR INITIALIZING AUX VAR/OPT ARG
(START-DISPATCH 3 0)    ;DOES NXT INSTR
QBOFDT  (QBFE)
        (QBFE)
        (QBFE)
        (QBFE)
        (QBQT)
        (QBDLOC)
        (QBDARG)
        (P-BIT ILLOP)   ;PDL ILLEGAL
(END-DISPATCH)

#-cadr (begin-comment)
(START-DISPATCH 5 P-BIT)
;TRAP UNLESS DATA TYPE IS SYM
TRAP-UNLESS-SYM
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;TRAP
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;NULL
        (INHIBIT-XCT-NEXT-BIT UNRECONCILED-TRAP)        ;UNRECONCILED
        (R-BIT)                         ;SYM  P-BIT & R-BIT CAUSE DISPATCH TO BE A NO-OP
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SYMBOL-HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;FIX
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;EXTENDED NUMBER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;GC-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;LOCATIVE
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;LIST
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;U CODE ENTRY
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;FEF
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ARRAY-POINTER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ARRAY-HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;STACK-GROUP
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;CLOSURE
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;INSTANCE
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;INSTANCE-HEADER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;ENTITY
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;unused-32
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SELF-REF-POINTER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;CHARACTER
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;rplacd-forward
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;spare
        (INHIBIT-XCT-NEXT-BIT TRAP)     ;SMALL-FLONUM
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT TRAP))
(END-DISPATCH)
#-cadr (end-comment)

(START-DISPATCH 5 0) ;DOES XCT-NEXT UNLESS ILLOPS
;POPJ if data type is not numeric.  Used by NUMBERP, EQL and EQUALP.
POPJ-IF-NOT-NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;TRAP
        (R-BIT)                                 ;NULL, POPJ
        (R-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-ILLOP) ;UNRECONCILED
        (R-BIT)                                 ;SYM, POPJ
        (R-BIT)                                 ;SYMBOL-HEADER, POPJ
        (P-BIT R-BIT)                           ;FIX, FALL-THROUGH
        (P-BIT R-BIT)                           ;EXTENDED NUMBER, FALL-THROUGH
        (R-BIT)                                 ;HEADER, POPJ
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;BODY-FORWARD
        (R-BIT)                                 ;LOCATIVE, POPJ
        (R-BIT)                                 ;LIST, POPJ
        (R-BIT)                                 ;U CODE ENTRY, POPJ
        (R-BIT)                                 ;FEF, POPJ
        (R-BIT)                                 ;ARRAY-POINTER, POPJ
        (R-BIT)                                 ;ARRAY-HEADER, POPJ
        (R-BIT)                                 ;STACK-GROUP, POPJ
        (R-BIT)                                 ;CLOSURE, POPJ
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;INDEXED-FORWARD
        (R-BIT)                                 ;SELECT-METHOD, POPJ
        (R-BIT)                                 ;INSTANCE, POPJ
        (R-BIT)                                 ;INSTANCE-HEADER, POPJ
        (R-BIT)                                 ;ENTITY, POPJ
        (P-BIT inhibit-xct-next-bit illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
        (R-BIT)                                 ;CHARACTER, POPJ
        (p-bit n-bit illop)                     ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;spare
        (P-BIT R-BIT)                           ;SMALL-FLONUM, FALL-THROUGH
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP))
(END-DISPATCH)

(START-DISPATCH 5 0)  ;INHIBIT-XCT-NEXT-BIT UNLESS CANT FIGURE IT OUT
                      ; (IE INTERPRETER TRAP)
XARGI-DISPATCH
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;TRAP
        (R-BIT)                                 ;NULL
        (P-BIT UNRECONCILED-ILLOP INHIBIT-XCT-NEXT-BIT) ;UNRECONCILED
        (XARGI3 INHIBIT-XCT-NEXT-BIT)           ;SYM, REPLACE WITH FCTN CELL
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SYMBOL-HEADER
        (R-BIT)                                 ;FIX
        (R-BIT)                                 ;EXTENDED NUMBER
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;HEADER
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;GC-FORWARD
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;ONE-Q-FORWARD
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;HEADER-FORWARD
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;BODY-FORWARD
        (R-BIT)                                 ;LOCATIVE
        (R-BIT)                                 ;LIST, (GO TO INTERPRETER)
        (XAGUE1 INHIBIT-XCT-NEXT-BIT)           ;U CODE ENTRY
        (XAGM1 INHIBIT-XCT-NEXT-BIT)            ;FEF, RETURN FAST OPT Q
        (XAGAR1 INHIBIT-XCT-NEXT-BIT)           ;ARRAY-POINTER
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;ARRAY-HEADER
        (XAGISG INHIBIT-XCT-NEXT-BIT)           ;STACK-GROUP
        (XAGICL INHIBIT-XCT-NEXT-BIT)           ;CLOSURE
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;INDEXED-FORWARD
        (R-BIT)                                 ;SELECT-METHOD. CAN'T SAY WITHOUT KEY
                                                ; SO BE CONSERVATIVE
        (R-BIT)                                 ;INSTANCE (COULD GET FUNCTION BUT WHY BOTHER
                                                ; SINCE IT WILL BE A SELECT-METHOD ANYWAY)
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;INSTANCE-HEADER
        (XAGICL INHIBIT-XCT-NEXT-BIT)           ;ENTITY
        (p-bit illop inhibit-xct-next-bit)      ;unused-32
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;SELF-REF-POINTER
        (R-BIT)                                 ;CHARACTER
        (p-bit n-bit illop)                     ;rplacd-forward
        (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT)      ;spare
        (R-BIT)                                 ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT ILLOP INHIBIT-XCT-NEXT-BIT))
(END-DISPATCH)

(START-DISPATCH 6 P-BIT)        ;TRANSPORTER DISPATCH ON DATA TYPE AND MAP BIT
;EITHER DROPS THROUGH (P-R) OR CALLS (P-N) MAGIC ROUTINE.
;FOR TYPES WHICH AREN'T INUMS, THE 0 CASE GOES TO TRANS-OLD TO CHECK FOR OLD-SPACE
D-TRANSPORT
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;0 TRAP
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;1 TRAP
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 NULL (TRANSPORT FOR SCAVENGER, XPCAL)
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;1 NULL
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;0 UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT TRANS-UNRECONCILED)       ;1 UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SYMBOL
        (R-BIT)                                 ;1 SYMBOL
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SYMBOL-HEADER
        (R-BIT)                                 ;1 SYMBOL-HEADER
        (R-BIT)                                 ;0 FIX
        (R-BIT)                                 ;1 FIX
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 EXTENDED-NUMBER
        (R-BIT)                                 ;1 EXTENDED-NUMBER
        (R-BIT)                                 ;0 HEADER
        (R-BIT)                                 ;1 HEADER
        (INHIBIT-XCT-NEXT-BIT ILLOP)            ;0 GC-FORWARD (SHOULDN'T SEE IN THIS CONTEXT)
        (INHIBIT-XCT-NEXT-BIT ILLOP)            ;1 GC-FORWARD (SHOULDN'T SEE IN THIS CONTEXT)
        (INHIBIT-XCT-NEXT-BIT TRANS-OLDP-EVCP)  ;0 EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT TRANS-EVCP)       ;1 EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD0)       ;0 ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OQF)        ;1 ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD0)       ;0 HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-HFWD)       ;1 HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-BFWD)       ;0 BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-BFWD)       ;1 BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 LOCATIVE
        (R-BIT)                                 ;1 LOCATIVE
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 LIST
        (R-BIT)                                 ;1 LIST
        (R-BIT)                                 ;0 U CODE ENTRY
        (R-BIT)                                 ;1 U CODE ENTRY
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 FEF-POINTER
        (R-BIT)                                 ;1 FEF-POINTER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 ARRAY-POINTER
        (R-BIT)                                 ;1 ARRAY-POINTER
        (R-BIT)                                 ;0 ARRAY-HEADER
        (R-BIT)                                 ;1 ARRAY-HEADER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 STACK-GROUP
        (R-BIT)                                 ;1 STACK-GROUP
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 CLOSURE
        (R-BIT)                                 ;1 CLOSURE
        (INHIBIT-XCT-NEXT-BIT TRANS-INDXF)      ;0 INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-INDXF)      ;1 INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SELECT-METHOD
        (R-BIT)                                 ;1 SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 INSTANCE
        (R-BIT)                                 ;1 INSTANCE
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 INSTANCE-HEADER
        (R-BIT)                                 ;1 INSTANCE-HEADER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 ENTITY
        (R-BIT)                                 ;1 ENTITY
        (inhibit-xct-next-bit trans-trap)       ;0 unused-32
        (inhibit-xct-next-bit trans-trap)       ;1 unused-32
        (INHIBIT-XCT-NEXT-BIT TRANS-SRP)        ;0 SELF-REF-POINTER
        (INHIBIT-XCT-NEXT-BIT TRANS-SRP)        ;1 SELF-REF-POINTER
        (R-BIT)                                 ;0 CHARACTER
        (R-BIT)                                 ;1 CHARACTER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD0)       ;0 rplacd-forward
        (INHIBIT-XCT-NEXT-BIT TRANS-HFWD)       ;1 rplacd-forward
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;0 spare
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;1 spare
        (R-BIT)                                 ;0 SMALL-FLONUM
        (R-BIT)                                 ;1 SMALL-FLONUM
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT TRANS-TRAP))
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT TRANS-TRAP))
(END-DISPATCH)

(START-DISPATCH 6 P-BIT)        ;TRANSPORTER DISPATCH ON DATA TYPE AND MAP BIT
;EITHER DROPS THROUGH (P-R) OR CALLS (P-N) MAGIC ROUTINE.
;FOR TYPES WHICH AREN'T INUMS, THE 0 CASE GOES TO TRANS-OLD TO CHECK FOR OLD-SPACE
D-TRANSPORT-NO-EVCP
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;0 TRAP
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;1 TRAP
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 NULL (TRANSPORT FOR SCAVENGER, XPCAL)
        (R-BIT)                                 ;1 NULL
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;0 UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT TRANS-UNRECONCILED)       ;1 UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SYMBOL
        (R-BIT)                                 ;1 SYMBOL
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SYMBOL-HEADER
        (R-BIT)                                 ;1 SYMBOL-HEADER
        (R-BIT)                                 ;0 FIX
        (R-BIT)                                 ;1 FIX
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 EXTENDED-NUMBER
        (R-BIT)                                 ;1 EXTENDED-NUMBER
        (R-BIT)                                 ;0 HEADER
        (R-BIT)                                 ;1 HEADER
        (INHIBIT-XCT-NEXT-BIT ILLOP)            ;0 GC-FORWARD (SHOULDN'T SEE IN THIS CONTEXT)
        (INHIBIT-XCT-NEXT-BIT ILLOP)            ;1 GC-FORWARD (SHOULDN'T SEE IN THIS CONTEXT)
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 EXTERNAL-VALUE-CELL-POINTER
        (R-BIT)                                 ;1 EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD0)       ;0 ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OQF)        ;1 ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD0)       ;0 HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-HFWD)       ;1 HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-BFWD)       ;0 BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-BFWD)       ;1 BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 LOCATIVE
        (R-BIT)                                 ;1 LOCATIVE
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 LIST
        (R-BIT)                                 ;1 LIST
        (R-BIT)                                 ;0 U CODE ENTRY
        (R-BIT)                                 ;1 U CODE ENTRY
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 FEF-POINTER
        (R-BIT)                                 ;1 FEF-POINTER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 ARRAY-POINTER
        (R-BIT)                                 ;1 ARRAY-POINTER
        (R-BIT)                                 ;0 ARRAY-HEADER
        (R-BIT)                                 ;1 ARRAY-HEADER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 STACK-GROUP
        (R-BIT)                                 ;1 STACK-GROUP
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 CLOSURE
        (R-BIT)                                 ;1 CLOSURE
        (INHIBIT-XCT-NEXT-BIT TRANS-INDXF)      ;0 INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-INDXF)      ;1 INDEXED-FORWARD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SELECT-METHOD
        (R-BIT)                                 ;1 SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 INSTANCE
        (R-BIT)                                 ;1 INSTANCE
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 INSTANCE-HEADER
        (R-BIT)                                 ;1 INSTANCE-HEADER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 ENTITY
        (R-BIT)                                 ;1 ENTITY
        (inhibit-xct-next-bit illop)            ;0 unused-32
        (inhibit-xct-next-bit illop)            ;0 unused-32
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD)        ;0 SELF-REF-POINTER
        (R-BIT)                                 ;1 SELF-REF-POINTER
        (R-BIT)                                 ;0 CHARACTER
        (R-BIT)                                 ;1 CHARACTER
        (INHIBIT-XCT-NEXT-BIT TRANS-OLD0)       ;0 rplacd-forward
        (INHIBIT-XCT-NEXT-BIT TRANS-HFWD)       ;1 rplacd-forward
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;0 spare
        (INHIBIT-XCT-NEXT-BIT TRANS-TRAP)       ;1 spare
        (R-BIT)                                 ;0 SMALL-FLONUM
        (R-BIT)                                 ;1 SMALL-FLONUM
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT TRANS-TRAP))
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT TRANS-TRAP))
(END-DISPATCH)

;THIS FLAVOR OF TRANSPORTER DISPATCH IS FOR THE PDL-BUFFER REFILL ROUTINE,
;WHICH MUST DO SOME FIXUP BEFORE CALLING THE TRANSPORTER
;ON THE 0 CASE OF NON-INUMS, IT MAY BE OLD-SPACE
(START-DISPATCH 6 0)    ;TRANSPORTER DISPATCH ON DATA TYPE AND MAP BIT
;EITHER DROPS THROUGH (P-R) OR JUMPS TO PB-TRANS
D-PB-TRANS
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 TRAP
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;1 TRAP
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 NULL
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;1 NULL
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;0 UNRECONCILED
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;1 UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 SYMBOL
        (P-BIT R-BIT)                           ;1 SYMBOL
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;0 SYMBOL-HEADER
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;1 SYMBOL-HEADER
        (P-BIT R-BIT)                           ;0 FIX
        (P-BIT R-BIT)                           ;1 FIX
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 EXTENDED-NUMBER
        (P-BIT R-BIT)                           ;1 EXTENDED-NUMBER
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;0 HEADER
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;1 HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;0 GC-FORWARD (SHOULDN'T SEE IN THIS CONTEXT)
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;1 GC-FORWARD (SHOULDN'T SEE IN THIS CONTEXT)
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 EXTERNAL-VALUE-CELL-POINTER
        (P-BIT R-BIT)                           ;1 EXTERNAL-VALUE-CELL-POINTER
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 ONE-Q-FORWARD
        (P-BIT R-BIT)                           ;1 ONE-Q-FORWARD
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;1 HEADER-FORWARD
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;1 BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 LOCATIVE
        (P-BIT R-BIT)                           ;1 LOCATIVE
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 LIST
        (P-BIT R-BIT)                           ;1 LIST
        (P-BIT R-BIT)                           ;0 U CODE ENTRY
        (P-BIT R-BIT)                           ;1 U CODE ENTRY
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 FEF-POINTER
        (P-BIT R-BIT)                           ;1 FEF-POINTER
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 ARRAY-POINTER
        (P-BIT R-BIT)                           ;1 ARRAY-POINTER
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;0 ARRAY-HEADER
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;1 ARRAY-HEADER
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 STACK-GROUP
        (P-BIT R-BIT)                           ;1 STACK-GROUP
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 CLOSURE
        (P-BIT R-BIT)                           ;1 CLOSURE
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;0 indexed-forward
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;1 indexed-forward
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 SELECT-METHOD
        (P-BIT R-BIT)                           ;1 SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 INSTANCE
        (P-BIT R-BIT)                           ;1 INSTANCE
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;0 INSTANCE-HEADER
        (p-bit INHIBIT-XCT-NEXT-BIT illop)      ;1 INSTANCE-HEADER
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 ENTITY
        (P-BIT R-BIT)                           ;1 ENTITY
        (p-bit inhibit-xct-next-bit illop)      ;0 unused-32
        (p-bit inhibit-xct-next-bit illop)      ;0 unused-32
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 SELF-REF-POINTER
        (P-BIT R-BIT)                           ;1 SELF-REF-POINTER
        (P-BIT R-BIT)                           ;0 CHARACTER
        (P-BIT R-BIT)                           ;1 CHARACTER
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 rplacd-forward
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;1 rplacd-forward
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;0 spare
        (INHIBIT-XCT-NEXT-BIT PB-TRANS)         ;1 spare
        (P-BIT R-BIT)                           ;0 SMALL-FLONUM
        (P-BIT R-BIT)                           ;1 SMALL-FLONUM
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT PB-TRANS))
 (REPEAT NQZUSD (INHIBIT-XCT-NEXT-BIT PB-TRANS))
(END-DISPATCH)

(START-DISPATCH 6 P-BIT)        ;GC-WRITE-TEST (MAP18: 0=EXTRA-PDL, 1=NORMAL)
;TEST EVERY PIECE OF BOXED DATA EVER STORED FROM PROCESSOR TO VIRTUAL MEMORY.
;EITHER DROPS THROUGH (P-R) OR CALLS (P-N) MAGIC ROUTINE.
;CURRENTLY ANYWAY, DOESN'T TRAP ON ILL DATA TYPES.  THAT WOULD NEED AN I-ARG TO SUPPRESS IT.
;DATA TYPES CURRENTLY CHECKED FOR IN EXTRA-PDL: EXTENDED-NUMBER, LOCATIVE, ARRAY
; must always be transparent to VMA.
D-GC-WRITE-TEST
        (R-BIT)                                 ;0 TRAP
        (R-BIT)                                 ;1 TRAP
        (R-BIT)                                 ;0 NULL
        (R-BIT)                                 ;1 NULL
        (n-bit gc-write-unreconciled)           ;0 UNRECONCILED
        (n-bit gc-write-unreconciled)           ;1 UNRECONCILED
        (n-bit gc-write-trap)                   ;0 SYMBOL
        (R-BIT)                                 ;1 SYMBOL
        (n-bit gc-write-trap)                   ;0 SYMBOL-HEADER
        (R-BIT)                                 ;1 SYMBOL-HEADER
        (R-BIT)                                 ;0 FIX
        (R-BIT)                                 ;1 FIX
        (n-bit gc-write-and-extra-pdl-trap)     ;0 EXTENDED-NUMBER
        (R-BIT)                                 ;1 EXTENDED-NUMBER
        (R-BIT)                                 ;0 HEADER
        (R-BIT)                                 ;1 HEADER
     ;; Normal code will never write GC-FORWARDS, but the volatility scanner will,
     ;; and needs to consider them as pointers to get the volatility right.
        (n-bit gc-write-trap)                   ;0 GC-FORWARD
        (r-bit)                                 ;1 GC-FORWARD
        (n-bit gc-write-trap)                   ;0 EXTERNAL-VALUE-CELL-POINTER
        (R-BIT)                                 ;1 EXTERNAL-VALUE-CELL-POINTER
        (n-bit gc-write-trap)                   ;0 ONE-Q-FORWARD
        (R-BIT)                                 ;1 ONE-Q-FORWARD
        (n-bit gc-write-trap)                   ;0 HEADER-FORWARD
        (R-BIT)                                 ;1 HEADER-FORWARD
     ;; BODY-FORWARDs always point to their associated HEADER-FORWARDs.  It is enough for
     ;; the scavenger to just look at the HEADER-FORWARD, since the object is relocated
     ;; as a whole.
        (r-bit)                                 ;0 BODY-FORWARD
        (R-BIT)                                 ;1 BODY-FORWARD
     ;; When dumping stack-groups registers pointing to extra-pdl have dtp-locative.
        (n-bit gc-write-and-extra-pdl-trap)     ;0 LOCATIVE
        (R-BIT)                                 ;1 LOCATIVE
        (n-bit gc-write-trap)                   ;0 LIST
        (R-BIT)                                 ;1 LIST
        (R-BIT)                                 ;0 U CODE ENTRY
        (R-BIT)                                 ;1 U CODE ENTRY
        (n-bit gc-write-trap)                   ;0 FEF-POINTER
        (R-BIT)                                 ;1 FEF-POINTER
        (n-bit gc-write-trap)                   ;0 ARRAY-POINTER
        (R-BIT)                                 ;1 ARRAY-POINTER
        (R-BIT)                                 ;0 ARRAY-HEADER
        (R-BIT)                                 ;1 ARRAY-HEADER
        (n-bit gc-write-trap)                   ;0 STACK-GROUP
        (R-BIT)                                 ;1 STACK-GROUP
        (n-bit gc-write-trap)                   ;0 CLOSURE
        (R-BIT)                                 ;1 CLOSURE
        (R-BIT)                                 ;0 INDEXED-FORWARD
        (R-BIT)                                 ;1 INDEXED-FORWARD
        (n-bit gc-write-trap)                   ;0 SELECT-METHOD
        (R-BIT)                                 ;1 SELECT-METHOD
        (n-bit gc-write-trap)                   ;0 INSTANCE
        (R-BIT)                                 ;1 INSTANCE
        (n-bit gc-write-trap)                   ;0 INSTANCE-HEADER
        (R-BIT)                                 ;1 INSTANCE-HEADER
        (n-bit gc-write-trap)                   ;0 ENTITY
        (R-BIT)                                 ;1 ENTITY
        (r-bit)                                 ;0 unused-32
        (r-bit)                                 ;1 unused-32
        (R-BIT)                                 ;0 SELF-REF-POINTER
        (R-BIT)                                 ;1 SELF-REF-POINTER
        (R-BIT)                                 ;0 CHARACTER
        (R-BIT)                                 ;1 CHARACTER
        (n-bit gc-write-trap)                   ;0 rplacd-forward
        (R-BIT)                                 ;1 rplacd-forward
        (R-BIT)                                 ;0 spare
        (R-BIT)                                 ;1 spare
        (R-BIT)                                 ;0 SMALL-FLONUM
        (R-BIT)                                 ;1 SMALL-FLONUM
 (REPEAT NQZUSD (R-BIT))
 (REPEAT NQZUSD (R-BIT))
(END-DISPATCH)

;FOR BOTH CADR AND LAMBDA:
(START-DISPATCH 3 0)  ;WANT XCT-NEXT
;DISPATCH ON DEST FIELD OF NON-DEST-GROUP-1 INSTRUCTIONS
;OPERAND FETCHED INTO M-T, VMA -> OPERAND
D-ND1   (ILLOP P-BIT)
        (QIADD)
        (QISUB)
        (QIMUL)
        (QIDIV)
        (QIAND)
        (QIXOR)
        (QIIOR)
(END-DISPATCH)


(START-DISPATCH 3 0)    ;WANT XCT-NEXT
;DISPATCH ON DEST FIELD OF NON-DEST-GROUP-2 INSTRUCTIONS
;OPERAND FETCHED INTO M-T, VMA -> OPERAND
D-ND2   (QIEQL)
        (QIGRP)
        (QILSP)
        (QIEQ)
#-cadr  (begin-comment)
        (QISCDR)
        (QISCDDR)
        (QISP1)
        (QISM1)
#-cadr  (end-comment)
#-lambda (begin-comment)
        (p-bit illop)        ;THESE HANDLED VIA DIRECT DISPATCH.
        (p-bit illop)
        (p-bit illop)
        (p-bit illop)
#-lambda (end-comment)
#-exp    (begin-comment)
        (p-bit illop)        ;THESE HANDLED VIA DIRECT DISPATCH.
        (p-bit illop)
        (p-bit illop)
        (p-bit illop)
#-exp    (end-comment)
(END-DISPATCH)

;DISPATCH ON RETURN STORING OPTION IN MVR
(START-DISPATCH 3 0)    ;DOES NXT INSTR
D-MVR   (P-BIT ILLOP)   ;ERROR
        (MVRB)          ;BLOCK
        (P-BIT ILLOP)   ;STORE INTO LIST
        (MVRC)          ;CONS UP LIST
        (MVRIND)        ;INDIRECT POINTER (OBSOLETE)
        (P-BIT ILLOP)   ;ERROR
        (P-BIT ILLOP)   ;ERROR
        (P-BIT ILLOP)   ;ERROR
(END-DISPATCH)

;THERE MUST NOT BE ANY I-MEM CODE IN THIS FILE!  THE FILE UC-CADR FOLLOWS THIS ONE AND
; HAS FIRST I-MEM CODE!
))
;;---                                   T H E   E N D

;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-
;;; This is file of definitions of the format of stack groups.   dlw 10/15/77
;;; It will be used by error handlers.
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;If this file is changed, it goes without saying that you need to make a new cold load.
;(Comments below may not be correct!)
;  Also SYS:SYS; QCOM and SYS:SYS2;GLOBAL must be changed to agree.

;-----------------------------------------------------------------------------------------
;  SYS:SYS2;SYSTEM has a list of the symbols defined here which belong in the SYSTEM package
;-----------------------------------------------------------------------------------------

;The microcode must be reassembled, and at least the following must be recompiled:
;    SYS:SYS2;EH >, SYS:SYS2;EHR >, SYS:SYS;SGFCTN >, SYS:SYS; QMISC > (for DESCRIBE)
;    SYS:SYS2;PROCES >, LMIO;KBD > (for PROCESS-WAIT), SYS:SYS2;QTRACE (for FUNCTION-ACTIVE-P)
;
;also LMCONS;CC >

(DEFSTRUCT (STACK-GROUP :ARRAY-LEADER (:CONSTRUCTOR NIL) (:ALTERANT NIL))
      SG-NAME                       ;String with the name of the stack group
      SG-REGULAR-PDL                ;Regular PDL array, 0=base of stack
      SG-REGULAR-PDL-LIMIT          ;Max depth before trap
      SG-SPECIAL-PDL                ;Special PDL array, 0=base of stack
      SG-SPECIAL-PDL-LIMIT          ;Max depth before trap
      SG-INITIAL-FUNCTION-INDEX     ;Index into PDL of initial M-AP
                                    ;   (3 unless initial call has adi)
      SG-PLIST
      SG-TRAP-TAG                   ;Symbolic tag corresponding to SG-TRAP-MICRO-PC.
                                    ; gotten via MICROCODE-ERROR-TABLE, etc.  Properties
                                    ; off this symbol drive error recovery.
      SG-RECOVERY-HISTORY           ;Available for hairy SG munging routines to attempt to
                                    ; leave tracks.
      SG-FOOTHOLD-DATA              ;During error recovery, contains pointer to a stack frame
                                    ; which contains saved status of the main stack group.
                                    ; (Preventing it from being lost when the foothold is
                                    ; running.)
      ((SG-STATE)
       (SG-CURRENT-STATE (BYTE 6. 0.))                  ;state of this stack group
       (SG-FOOTHOLD-EXECUTING-FLAG (BYTE 1. 6.))        ;not used
       (SG-PROCESSING-ERROR-FLAG (BYTE 1. 7.))          ;taking error trap (detect recursive errors)
       (SG-PROCESSING-INTERRUPT-FLAG (BYTE 1. 8.))      ;not used
       (SG-SAFE (BYTE 1. 9.))
       (SG-INST-DISP (BYTE 2. 10.))                     ;the instruction dispatch we are using
       (SG-IN-SWAPPED-STATE (BYTE 1. 22.))              ;we are in the swapped state
       (SG-SWAP-SV-ON-CALL-OUT (BYTE 1. 21.))           ;if this is on in the caller, or
       (SG-SWAP-SV-OF-SG-THAT-CALLS-ME (BYTE 1. 20.)))  ;   this in the callee, then swap
      SG-PREVIOUS-STACK-GROUP       ;Stack group who just ran
      SG-CALLING-ARGS-POINTER       ;Pointer into previous stack group's REGPDL to
      SG-CALLING-ARGS-NUMBER        ;   the args passed to us.
      SG-TRAP-AP-LEVEL              ;Locative to a location in PDL Buffer, below which
                                    ;traps occur
      SG-REGULAR-PDL-POINTER        ;Saved pdl pointer (as index into regular-pdl array)
      SG-SPECIAL-PDL-POINTER        ;Saved A-QLBNDP (as index into special-pdl array)
      SG-AP                         ;Saved M-AP
      SG-IPMARK                     ;Saved A-IPMARK
      SG-TRAP-MICRO-PC              ;Address of last call to TRAP
;     SG-ERROR-HANDLING-SG          ;Having these part of the SG would be nice, but
;     SG-INTERRUPT-HANDLING-SG      ; it doesnt buy anything for the time being, and costs
                                    ; a couple microinstructions.
      SG-SAVED-QLARYH               ;Saved A-QLARYH
      SG-SAVED-QLARYL               ;Saved A-QLARYL
      ((SG-SAVED-M-FLAGS)           ;Saved M-FLAGS
       (SG-FLAGS-QBBFL %%M-FLAGS-QBBFL)     ; Binding-block-pushed flag
       (SG-FLAGS-CAR-SYM-MODE %%M-FLAGS-CAR-SYM-MODE)  ;UPDATE PRINT-ERROR-MODE IN QMISC
       (SG-FLAGS-CAR-NUM-MODE %%M-FLAGS-CAR-NUM-MODE)  ;  IF ADD ANY..
       (SG-FLAGS-CDR-SYM-MODE %%M-FLAGS-CDR-SYM-MODE)
       (SG-FLAGS-CDR-NUM-MODE %%M-FLAGS-CDR-NUM-MODE)
       (SG-FLAGS-DONT-SWAP-IN %%M-FLAGS-DONT-SWAP-IN)
       (SG-FLAGS-TRAP-ENABLE %%M-FLAGS-TRAP-ENABLE)
       (SG-FLAGS-MAR-MODE %%M-FLAGS-MAR-MODE)
       (SG-FLAGS-PGF-WRITE %%M-FLAGS-PGF-WRITE)
       (SG-FLAGS-METER-ENABLE %%M-FLAGS-METER-ENABLE)
       (SG-FLAGS-TRAP-ON-CALL %%M-FLAGS-TRAP-ON-CALL)
       )
      SG-AC-K
      SG-AC-S
      SG-AC-J
      SG-AC-I
      SG-AC-Q
      SG-AC-R
      SG-AC-T
      SG-AC-E
      SG-AC-D
      SG-AC-C
      SG-AC-B
      SG-AC-A
      SG-AC-ZR
      SG-AC-2                   ;Pointer field of M-2 as fixnum
      SG-AC-1                   ;Pointer field of M-1 as fixnum
      SG-VMA-M1-M2-TAGS         ;Tag fields of VMA, M-1, M-2 packed into a fixnum
      SG-SAVED-VMA              ;Pointer field of VMA as a locative
      SG-PDL-PHASE              ;If you mung the sg's stack pointer, do same to this
                                ;This is the actual value of PDL-BUFFER-POINTER reg.
      )


(DEFSTRUCT (REGULAR-PDL :ARRAY-LEADER (:CONSTRUCTOR NIL) (:ALTERANT NIL))
      REGULAR-PDL-SG)

(DEFSTRUCT (SPECIAL-PDL :ARRAY-LEADER (:CONSTRUCTOR NIL) (:ALTERANT NIL))
      SPECIAL-PDL-SG)

;; Defsubsts for accessing the Regular Pdl.

(DEFSUBST RP-CALL-WORD     (RP AP) (AR-1 RP (+ AP %LP-CALL-STATE)))
(DEFSUBST RP-EXIT-WORD     (RP AP) (AR-1 RP (+ AP %LP-EXIT-STATE)))
(DEFSUBST RP-ENTRY-WORD    (RP AP) (AR-1 RP (+ AP %LP-ENTRY-STATE)))
(DEFSUBST RP-FUNCTION-WORD (RP AP) (AR-1 RP (+ AP %LP-FEF)))

; (DEFINE-RP-MACROS ((RP-FOO %%FOO) (RP-BAR %%BAR)) RP-CALL-WORD)
;
; produces
;
; (PROGN 'COMPILE
;        (DEFSUBST RP-FOO (RP AP)
;               (LDB %%FOO (RP-CALL-WORD RP AP)))
;        (DEFSUBST RP-BAR (RP AP)
;               (LDB %%BAR (RP-CALL-WORD RP AP))))

(DEFMACRO DEFINE-RP-MACROS (SPEC-LIST WORD-MACRO)
  (DO ((L SPEC-LIST (CDR L))
       (BYTE)
       (NAME)
       (ACCUM))
      ((NULL L) `(PROGN 'COMPILE . ,ACCUM))
    (SETQ NAME (CAAR L) BYTE (CADAR L))
    ;; Use %LOGLDB (%LOGDPB) so we can set the sign bit without bignum coercion.
    (PUSH `(DEFSUBST ,NAME (RP AP)
             (%LOGLDB ,BYTE (,WORD-MACRO RP AP)))
          ACCUM)))

(DEFINE-RP-MACROS ((RP-ATTENTION %%LP-CLS-ATTENTION)
                   (RP-ADI-PRESENT %%LP-CLS-ADI-PRESENT)
                   (RP-DESTINATION %%LP-CLS-DESTINATION)
                   (RP-DELTA-TO-OPEN-BLOCK %%LP-CLS-DELTA-TO-OPEN-BLOCK)
                   (RP-DELTA-TO-ACTIVE-BLOCK %%LP-CLS-DELTA-TO-ACTIVE-BLOCK)
                   (RP-TRAP-ON-EXIT %%LP-CLS-TRAP-ON-EXIT))
                  RP-CALL-WORD)

(DEFINE-RP-MACROS ((RP-MICRO-STACK-SAVED %%LP-EXS-MICRO-STACK-SAVED)
                   (RP-PC-STATUS %%LP-EXS-PC-STATUS)
                   (RP-BINDING-BLOCK-PUSHED %%LP-EXS-BINDING-BLOCK-PUSHED)      ;Same as above
                   (RP-EXIT-PC %%LP-EXS-EXIT-PC))
                  RP-EXIT-WORD)

(DEFINE-RP-MACROS ((RP-NUMBER-ARGS-SUPPLIED %%LP-ENS-NUM-ARGS-SUPPLIED) ;Only for macro frames
                   (RP-LOCAL-BLOCK-ORIGIN                               ;can this be extended?
                    %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN)) ;Only for macro frames
                  RP-ENTRY-WORD)

;; Defsubsts for accessing fields of the headers of Function Entry Frames.

(DEFMACRO DEFINE-OFFSET-BYTE-MACROS (PTR . WORDS)
   (DO ((LL WORDS (CDR LL))
        (ACCUM)
        (INDEX 0 (1+ INDEX)))
       ((NULL LL) `(PROGN 'COMPILE . ,ACCUM))
      (DO ((L (CAR LL) (CDR L))
           (NAME)
           (BYTE))
          ((NULL L))
         (SETQ NAME (CAAR L) BYTE (CADAR L))
         (PUSH `(DEFSUBST ,NAME (,PTR)
                  (%P-LDB-OFFSET ,BYTE ,PTR ,INDEX))
               ACCUM))))

(DEFINE-OFFSET-BYTE-MACROS FEF
 ((FEF-INITIAL-PC %%FEFH-PC)
  (FEF-NO-ADL-P %%FEFH-NO-ADL)
  (FEF-FAST-ARGUMENT-OPTION-P %%FEFH-FAST-ARG)
  (FEF-SPECIALS-BOUND-P %%FEFH-SV-BIND))
 ((FEF-LENGTH %%Q-POINTER))
 ()
 ((FEF-FAST-ARGUMENT-OPTION-WORD %%Q-POINTER))
 ((FEF-BIT-MAP-P %%FEFHI-SVM-ACTIVE)
  (FEF-BIT-MAP %%FEFHI-SVM-BITS))
 ((FEF-NUMBER-OF-REAL-LOCALS %%FEFHI-MS-LOCAL-BLOCK-LENGTH)
  (FEF-ADL-ORIGIN %%FEFHI-ARG-DESC-ORG)
  (FEF-ADL-LENGTH %%FEFHI-BIND-DESC-LENGTH)))

(DEFSUBST FEF-NAME (FEF) (%P-CONTENTS-OFFSET FEF %FEFHI-FCTN-NAME))


;; Randomness.
;   %%US-RPC                                            ;RETURN PC
;   %%US-PPBMIA                                         ;ADI ON MICRO-TO-MICRO-CALL
;   %%US-PPBMAA                                         ;ADI ON MACRO-TO-MICRO-CALL
;   %%US-PPBSPC                                         ;BINDING BLOCK PUSHED

;%%ADI-TYPE                                             ;ADI-KINDS
;    ADI-RETURN-INFO
;       %%ADI-RET-STORING-OPTION                                ;ADI-STORING-OPTIONS
;          ADI-ST-BLOCK ADI-ST-LIST
;         ADI-ST-MAKE-LIST
;         ADI-ST-INDIRECT
;       %%ADI-RET-SWAP-SV
;       %%ADI-RET-NUM-VALS-EXPECTING
;    ADI-RESTART-PC
;       %%ADI-RPC-MICRO-STACK-LEVEL
;    ADI-FEXPR-CALL
;    ADI-LEXPR-CALL
;    ADI-BIND-STACK-LEVEL
;    ADI-USED-UP-RETURN-INFO

(DEFCONST SG-ACCUMULATORS '(
  SG-AC-K
  SG-AC-S
  SG-AC-J
  SG-AC-I
  SG-AC-Q
  SG-AC-R
  SG-AC-T
  SG-AC-E
  SG-AC-D
  SG-AC-C
  SG-AC-B
  SG-AC-A
  SG-AC-ZR
  ))

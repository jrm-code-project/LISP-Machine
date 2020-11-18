;-*-Mode:LISP; BASE: 8; readtable: ZL-*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

(DEFCONST UC-FCTNS '(
;;; CAR AND CDR

;; XCAR and XCDR are the misc instructions.

;; QCAR and QCDR are for use as subroutines;
;; they take arg in M-T and return value in M-T.
;; QCDR-SB is like QCDR but allows sequence breaks;
;; use it in preference to QCDR except when you cannot
;; tell whether a sequence break is safe.

;; QMA and QMD are old names for QCAR and QCDR.
;; They are used only by the microcompiler.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAR M-T)

XCAR   (MISC-INST-ENTRY M-CAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
QMA
   (ERROR-TABLE RESTART CAR)
QCAR (declare (args a-t) (values a-t))
        (DISPATCH (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-T CAR-PRE-DISPATCH)
        ;; I-ARG is in case go to QCARCDR-INSTANCE.
   (ERROR-TABLE ARGTYP CONS M-T T CAR CAR)
;; Drop through for CAR of a CONS.
QCAR3   ((VMA-START-READ) M-T)
QCAR4   (CHECK-PAGE-READ)       ;GET DIRECTLY HERE FROM DISPATCH CAR-PRE-DISPATCH-DIRECT.
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD)
       ((M-T) Q-TYPED-POINTER MD)

;; Here for taking CAR of a symbol.
QCARSY  ((vma) m-zero)
        (DISPATCH-XCT-NEXT M-CAR-SYM-MODE CAR-SYM-DISPATCH)
       ((M-T) Q-TYPED-POINTER M-T)
   (ERROR-TABLE ARGTYP CONS M-T T CAR CAR)
        (POPJ-EQUAL M-T A-V-NIL)
        (CALL TRAP)
   (ERROR-TABLE ARGTYP CONS M-T T CAR CAR)

;; Here for taking CAR of a number.
;; NOTE: you cannot call trap unless VMA contains a reasonable pointer.  Clobber it first.
QCARNM  (DISPATCH-XCT-NEXT M-CAR-NUM-MODE CAR-NUM-DISPATCH)
       ((vma) m-zero)
   (ERROR-TABLE ARGTYP CONS M-T T CAR CAR)

;; Here for CAR or CDR of an instance.  Send a message to it.
;; I-ARG already set up to say what operation we do.
QCARCDR-INSTANCE
        (CALL INSTANCE-INVOKE-1)
        ((C-PDL-BUFFER-POINTER) DPB C-PDL-BUFFER-POINTER Q-TYPED-POINTER
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-CALL MMCALL) (I-ARG 1))   ;Call, 1 arg.  Value comes back in M-T.
        (POPJ)

;; Like QCDR but takes sequence breaks.
QCDR-SB(declare (args a-t) (values a-t))
        (DISPATCH (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE M-T CDR-PRE-DISPATCH)
        ;; I-ARG is in case go to QCARCDR-INSTANCE.
   (ERROR-TABLE ARGTYP CONS M-T T CDR CDR)
;; Drop through for CDR of a CONS.
        ((VMA-START-READ) M-T)
QCDR4-SB (declare (args a-t) (values a-t))      ;get directly here from -direct dispatch
        (CHECK-PAGE-READ-SEQUENCE-BREAK-saving-vma-in-m-t-if-sequence-break)
        (DISPATCH TRANSPORT-CDR MD)     ;Check for invz, don't really transport.
        (DISPATCH Q-CDR-CODE MD CDR-CDR-DISPATCH-MASK-VMA-ON-CDR-NEXT)
   (ERROR-TABLE BAD-CDR-CODE VMA)
;; Drops thru in case of CDR-NEXT.
        (POPJ-AFTER-NEXT (M-T) Q-TYPED-POINTER VMA)
       ((M-T) ADD M-T (A-CONSTANT 1))   ;Same data type as arg.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDR M-T)

XCDR   (MISC-INST-ENTRY M-CDR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
QMD
   (ERROR-TABLE RESTART CDR)
QCDR (declare (args a-t) (values a-t))
        (DISPATCH (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE M-T CDR-PRE-DISPATCH)
        ;; I-ARG is in case go to QCARCDR-INSTANCE.
   (ERROR-TABLE ARGTYP CONS M-T T CDR CDR)
;; Drop through for CDR of a CONS.
QCDR3   ((VMA-START-READ) M-T)
QCDR4   (CHECK-PAGE-READ)               ;COME DIRECTLY HERE FROM CDR-PRE-DISPATCH-DIRECT.
QCDR-SB-1
        (DISPATCH TRANSPORT-CDR MD)     ;Check for invz, don't really transport.
        (DISPATCH Q-CDR-CODE MD CDR-CDR-DISPATCH-MASK-VMA-ON-CDR-NEXT)
   (ERROR-TABLE BAD-CDR-CODE VMA)
;; Drops thru in case of CDR-NEXT.
        (POPJ-AFTER-NEXT (M-T) Q-TYPED-POINTER VMA)
       ((M-T) ADD M-T (A-CONSTANT 1))   ;Same data type as arg.

QCDRSY  ((vma) m-zero)
        (DISPATCH-XCT-NEXT M-CDR-SYM-MODE CDR-SYM-DISPATCH)
       ((M-T) Q-TYPED-POINTER M-T)
   (ERROR-TABLE ARGTYP CONS M-T T CDR CDR)
        (POPJ-EQUAL M-T A-V-NIL)
        (CALL TRAP)
   (ERROR-TABLE ARGTYP CONS M-T T CDR CDR)

QCDRNM  (DISPATCH-XCT-NEXT M-CDR-NUM-MODE CDR-NUM-DISPATCH)
       ((vma) m-zero)
   (ERROR-TABLE ARGTYP CONS M-T T CDR CDR)

CDR-FULL-NODE
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD) ;CHECK FOR INVISIBLE, GC
       ((M-T) Q-TYPED-POINTER MD)

CDR-IS-NIL
   (MISC-INST-ENTRY FALSE)
XFALSE  (POPJ-AFTER-NEXT (M-T) A-V-NIL)
       (NO-OP)

POP-THEN-XFALSE
        (POPJ-AFTER-NEXT (M-T) A-V-NIL)
       ((M-GARBAGE) PDL-POP)

;; CDR of SYMBOL, in the mode where that gets the plist.
QCDPRP  ((M-T) Q-TYPED-POINTER M-T)
        (JUMP-EQUAL M-T A-V-NIL XFALSE)         ;CDR of NIL is still NIL,
        ((M-T) ADD (A-CONSTANT 3) M-T)
        (JUMP-XCT-NEXT QCDR)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))

;; Take car into M-A and cdr into M-T at same time.
;; By default, allow sequence breaks.
CARCDR (declare (args a-t) (values a-a a-t))
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list))
                carcdr-not-list)
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ-SEQUENCE-BREAK-saving-vma-in-m-t-if-sequence-break)
        (DISPATCH TRANSPORT MD)
        ;;better do a full CDR if the CAR was forwarded. --Mats C.
        (jump-not-equal-xct-next vma a-t qcdr3)
       ((M-A) Q-TYPED-POINTER MD)
        (DISPATCH Q-CDR-CODE MD CDR-CDR-DISPATCH)
   (ERROR-TABLE BAD-CDR-CODE VMA)
;; Does POPJ-XCT-NEXT to do this insn for case of CDR-NEXT.
       ((M-T) ADD VMA (A-CONSTANT 1))

;note well: the argument tested by the dispatch which gets here must be exactly the
; same as that which winds up in M-T.  In particular, it must be masked to Q-TYPED-POINTER.
; This may mean you cant load M-T on the XCT-NEXT cycle of the dispatch and instead have
; to do it before the dispatch, taking an extra cycle.  However, that is still better than
; masking the VMA here, which would always take at least one extra cycle.
carcdr-direct   ;reached directly from carcdr-direct dispatch if list.
 (declare (args a-t) (values a-a a-t))
        (CHECK-PAGE-READ-SEQUENCE-BREAK-saving-vma-in-m-t-if-sequence-break)
        (DISPATCH TRANSPORT MD)
        ;;better do a full CDR if the CAR was forwarded. --Mats C.
        (jump-not-equal-xct-next vma a-t qcdr3)
       ((M-A) Q-TYPED-POINTER MD)
        (DISPATCH Q-CDR-CODE MD CDR-CDR-DISPATCH)
   (ERROR-TABLE BAD-CDR-CODE VMA)
;; Does POPJ-XCT-NEXT to do this insn for case of CDR-NEXT.
       ((M-T) ADD VMA (A-CONSTANT 1))

CARCDR-NOT-LIST
        (CALL-XCT-NEXT QCAR)
       ((PDL-PUSH) M-T)
        ((M-A) M-T)
        (JUMP-XCT-NEXT QCDR)
       ((M-T) PDL-POP)

;;; Take car into M-A and cdr into M-T at same time.
CARCDR-NO-SB   (declare (args a-t) (values a-a a-t))
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list))
                carcdr-not-list)
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ;;better do a full CDR if the CAR was forwarded. --Mats C.
        (jump-not-equal-xct-next vma a-t qcdr3)
       ((M-A) Q-TYPED-POINTER MD)
        (DISPATCH Q-CDR-CODE MD CDR-CDR-DISPATCH)
   (ERROR-TABLE BAD-CDR-CODE VMA)
;; Does POPJ-XCT-NEXT to do this insn for case of CDR-NEXT.
       ((M-T) ADD VMA (A-CONSTANT 1))

(LOCALITY D-MEM)
(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE BEFORE TAKING CAR
;IF DROPS THROUGH, NORMAL LIST-TYPE CAR
CAR-PRE-DISPATCH
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-TRAP)  ;UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT QCARSY)           ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SYMBOL-HEADER
        (INHIBIT-XCT-NEXT-BIT QCARNM)           ;FIX
        (INHIBIT-XCT-NEXT-BIT QCARNM)           ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;BODY-FORWARD
        (P-BIT R-BIT)                           ;LOCATIVE
        (P-BIT R-BIT)                           ;LIST
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;U CODE ENTRY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;STACK-GROUP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INDEXED-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT QCARCDR-INSTANCE) ;INSTANCE (send message)
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ENTITY (eventually send message?)
        (P-BIT INHIBIT-XCT-NEXT-BIT illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELF-REF-POINTER
        (INHIBIT-XCT-NEXT-BIT QCARNM)           ;CHARACTER
        (p-bit n-bit trap)                      ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;spare
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT TRAP))
(END-DISPATCH)

#-LAMBDA (begin-comment)
(START-DISPATCH 5 0)
 ;DISPATCH ON DATA TYPE BEFORE TAKING CAR, AND ACTUALLY START MEM REFERENCE
 ;IF APPROPRIATE.  DESIGNED FOR USE WITH DISPATCH-WRITE-VMA-XCT-NEXT.
 ;ALWAYS DOES XCT-NEXT, EVEN IF ERROR, SINCE THAT IS CONSIDERED TO HAPPEN "BEFORE" THE CAR.
CAR-PRE-DISPATCH-DIRECT
        (P-BIT  TRAP)   ;TRAP
        (P-BIT  TRAP)   ;NULL
        (P-BIT  UNRECONCILED-TRAP)      ;UNRECONCILED
        (P-BIT QCARSY)          ;SYMBOL
        (P-BIT  TRAP)   ;SYMBOL-HEADER
        (P-BIT QCARNM)          ;FIX
        (P-BIT QCARNM)          ;EXTENDED NUMBER
        (P-BIT  TRAP)   ;HEADER
        (P-BIT  TRAP)   ;GC-FORWARD
        (P-BIT  TRAP)   ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT  TRAP)   ;ONE-Q-FORWARD
        (P-BIT  TRAP)   ;HEADER-FORWARD
        (P-BIT  TRAP)   ;BODY-FORWARD
        (P-BIT DISPATCH-START-MEM-READ-BIT QCAR4)       ;LOCATIVE
        (P-BIT DISPATCH-START-MEM-READ-BIT QCAR4)       ;LIST
        (P-BIT  TRAP)   ;U CODE ENTRY
        (P-BIT  TRAP)   ;FEF
        (P-BIT  TRAP)   ;ARRAY-POINTER
        (P-BIT  TRAP)   ;ARRAY-HEADER
        (P-BIT  TRAP)   ;STACK-GROUP
        (P-BIT  TRAP)   ;CLOSURE
        (P-BIT  TRAP)   ;INDEXED-FORWARD
        (P-BIT  TRAP)   ;SELECT-METHOD
        (P-BIT QCARCDR-INSTANCE)        ;INSTANCE (send message)
        (P-BIT  TRAP)   ;INSTANCE-HEADER
        (P-BIT  TRAP)   ;ENTITY (eventually send message?)
        (P-BIT  trap)   ;unused-32
        (P-BIT  TRAP)   ;SELF-REF-POINTER
        (P-BIT QCARNM)          ;CHARACTER
        (p-bit trap)    ;rplacd-forward
        (P-BIT  TRAP)   ;spare
        (P-BIT  TRAP)   ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT  TRAP))
(END-DISPATCH)

(START-DISPATCH 5 0)
 ;DISPATCH ON DATA TYPE BEFORE TAKING CARCDR, AND ACTUALLY START MEM REFERENCE
 ;IF APPROPRIATE.  DESIGNED FOR USE WITH DISPATCH-WRITE-VMA-XCT-NEXT.
 ;ALWAYS DOES XCT-NEXT, EVEN IF ERROR, SINCE THAT IS CONSIDERED TO HAPPEN "BEFORE" THE CAR.
 ;goes to CARCDR not list in any case if its not a list.  No immediate arg is necessary
 ;since it does not go directly to the message sender.
CARCDR-DISPATCH-DIRECT
        (P-BIT  CARCDR-NOT-LIST)        ;TRAP
        (P-BIT  CARCDR-NOT-LIST)        ;NULL
        (P-BIT  CARCDR-NOT-LIST)        ;UNRECONCILED
        (P-BIT  CARCDR-NOT-LIST)                ;SYMBOL
        (P-BIT  CARCDR-NOT-LIST)        ;SYMBOL-HEADER
        (P-BIT  CARCDR-NOT-LIST)                ;FIX
        (P-BIT  CARCDR-NOT-LIST)                ;EXTENDED NUMBER
        (P-BIT  CARCDR-NOT-LIST)        ;HEADER
        (P-BIT  CARCDR-NOT-LIST)        ;GC-FORWARD
        (P-BIT  CARCDR-NOT-LIST)        ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT  CARCDR-NOT-LIST)        ;ONE-Q-FORWARD
        (P-BIT  CARCDR-NOT-LIST)        ;HEADER-FORWARD
        (P-BIT  CARCDR-NOT-LIST)        ;BODY-FORWARD
        (P-BIT  CARCDR-NOT-LIST)        ;LOCATIVE
        (P-BIT DISPATCH-START-MEM-READ-BIT CARCDR-DIRECT)       ;LIST
        (P-BIT  CARCDR-NOT-LIST)        ;U CODE ENTRY
        (P-BIT  CARCDR-NOT-LIST)        ;FEF
        (P-BIT  CARCDR-NOT-LIST)        ;ARRAY-POINTER
        (P-BIT  CARCDR-NOT-LIST)        ;ARRAY-HEADER
        (P-BIT  CARCDR-NOT-LIST)        ;STACK-GROUP
        (P-BIT  CARCDR-NOT-LIST)        ;CLOSURE
        (P-BIT  CARCDR-NOT-LIST)        ;indexed-forward
        (P-BIT  CARCDR-NOT-LIST)        ;SELECT-METHOD
        (P-BIT  CARCDR-NOT-LIST)        ;INSTANCE (send message)
        (P-BIT  CARCDR-NOT-LIST)        ;INSTANCE-HEADER
        (P-BIT  CARCDR-NOT-LIST)        ;ENTITY (eventually send message?)
        (P-BIT  CARCDR-NOT-LIST)        ;unused-32
        (P-BIT  CARCDR-NOT-LIST)        ;SELF-REF-POINTER
        (P-BIT  CARCDR-NOT-LIST)                ;CHARACTER
        (p-bit carcdr-not-list)         ;rplacd-forward
        (P-BIT  CARCDR-NOT-LIST)        ;spare
        (P-BIT  CARCDR-NOT-LIST)        ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT  CARCDR-NOT-LIST))
(END-DISPATCH)
#-LAMBDA (end-comment)

(START-DISPATCH 5 0)
;DISPATCH ON INPUT DATA TYPE WHEN TAKING CDR
;DROP THROUGH IF NORMAL LIST-TYPE CDR
CDR-PRE-DISPATCH
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;TRAP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;NULL
        (P-BIT INHIBIT-XCT-NEXT-BIT UNRECONCILED-TRAP)  ;UNRECONCILED
        (INHIBIT-XCT-NEXT-BIT QCDRSY)           ;SYMBOL
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SYMBOL-HEADER
        (INHIBIT-XCT-NEXT-BIT QCDRNM)           ;FIX
        (INHIBIT-XCT-NEXT-BIT QCDRNM)           ;EXTENDED NUMBER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;GC-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ONE-Q-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;HEADER-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;BODY-FORWARD
        (INHIBIT-XCT-NEXT-BIT QCAR3)            ;LOCATIVE.  NOTE CAR!!
        (P-BIT R-BIT)                           ;LIST
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;U CODE ENTRY
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;FEF
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-POINTER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ARRAY-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;STACK-GROUP
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CLOSURE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INDEXED-FORWARD
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELECT-METHOD
        (INHIBIT-XCT-NEXT-BIT QCARCDR-INSTANCE) ;INSTANCE (send message)
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;INSTANCE-HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;ENTITY (eventually send message?)
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SELF-REF-POINTER
        (INHIBIT-XCT-NEXT-BIT QCDRNM)           ;CHARACTER
        (p-bit n-bit trap)                      ;rplacd-forward
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;spare
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT INHIBIT-XCT-NEXT-BIT TRAP))
(END-DISPATCH)

#-LAMBDA (begin-comment)
(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE BEFORE TAKING CDR, AND ACTUALLY START MEM REFERENCE
;IF APPROPRIATE.  DESIGNED FOR USE WHICH DISPATCH-WRITE-VMA-XCT-NEXT.
;ALWAYS DOES XCT-NEXT, EVEN IF ERROR, SINCE THAT IS CONSIDERED TO HAPPEN "BEFORE" THE CDR.
CDR-PRE-DISPATCH-DIRECT
        (P-BIT  TRAP)   ;TRAP
        (P-BIT  TRAP)   ;NULL
        (P-BIT  UNRECONCILED-TRAP)      ;UNRECONCILED
        (P-BIT QCDRSY)  ;SYMBOL
        (P-BIT  TRAP)   ;SYMBOL-HEADER
        (P-BIT QCDRNM)  ;FIX
        (P-BIT QCDRNM)  ;EXTENDED NUMBER
        (P-BIT  TRAP)   ;HEADER
        (P-BIT  TRAP)   ;GC-FORWARD
        (P-BIT  TRAP)   ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT  TRAP)   ;ONE-Q-FORWARD
        (P-BIT  TRAP)   ;HEADER-FORWARD
        (P-BIT  TRAP)   ;BODY-FORWARD
        (P-BIT DISPATCH-START-MEM-READ-BIT QCAR4)       ;LOCATIVE.  NOTE CAR!!
        (P-BIT DISPATCH-START-MEM-READ-BIT QCDR4)       ;LIST
        (P-BIT  TRAP)   ;U CODE ENTRY
        (P-BIT  TRAP)   ;FEF
        (P-BIT  TRAP)   ;ARRAY-POINTER
        (P-BIT  TRAP)   ;ARRAY-HEADER
        (P-BIT  TRAP)   ;STACK-GROUP
        (P-BIT  TRAP)   ;CLOSURE
        (P-BIT  TRAP)   ;INDEXED-FORWARD
        (P-BIT  TRAP)   ;SELECT-METHOD
        (P-BIT QCARCDR-INSTANCE)        ;INSTANCE (send message)
        (P-BIT  TRAP)   ;INSTANCE-HEADER
        (P-BIT  TRAP)   ;ENTITY (eventually send message?)
        (P-BIT  TRAP)   ;unused-32
        (P-BIT  TRAP)   ;SELF-REF-POINTER
        (P-BIT QCDRNM)          ;CHARACTER
        (p-bit trap)    ;rplacd-forward
        (P-BIT TRAP)    ;spare
        (P-BIT  TRAP)   ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT TRAP))
(END-DISPATCH)

(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE BEFORE TAKING CDR, AND ACTUALLY START MEM REFERENCE
;IF APPROPRIATE.  DESIGNED FOR USE WHICH DISPATCH-WRITE-VMA-XCT-NEXT.
;ALWAYS DOES XCT-NEXT, EVEN IF ERROR, SINCE THAT IS CONSIDERED TO HAPPEN "BEFORE" THE CDR.
;this one allows sequence breaks.
CDR-PRE-DISPATCH-SB-DIRECT
        (P-BIT  TRAP)   ;TRAP
        (P-BIT  TRAP)   ;NULL
        (P-BIT  UNRECONCILED-TRAP)      ;UNRECONCILED
        (P-BIT QCDRSY)  ;SYMBOL
        (P-BIT  TRAP)   ;SYMBOL-HEADER
        (P-BIT QCDRNM)  ;FIX
        (P-BIT QCDRNM)  ;EXTENDED NUMBER
        (P-BIT  TRAP)   ;HEADER
        (P-BIT  TRAP)   ;GC-FORWARD
        (P-BIT  TRAP)   ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT  TRAP)   ;ONE-Q-FORWARD
        (P-BIT  TRAP)   ;HEADER-FORWARD
        (P-BIT  TRAP)   ;BODY-FORWARD
        (P-BIT DISPATCH-START-MEM-READ-BIT QCAR4)       ;LOCATIVE.  NOTE CAR!!
        (P-BIT DISPATCH-START-MEM-READ-BIT QCDR4-SB)    ;LIST
        (P-BIT  TRAP)   ;U CODE ENTRY
        (P-BIT  TRAP)   ;FEF
        (P-BIT  TRAP)   ;ARRAY-POINTER
        (P-BIT  TRAP)   ;ARRAY-HEADER
        (P-BIT  TRAP)   ;STACK-GROUP
        (P-BIT  TRAP)   ;CLOSURE
        (P-BIT  TRAP)   ;indexed-forward
        (P-BIT  TRAP)   ;SELECT-METHOD
        (P-BIT QCARCDR-INSTANCE)        ;INSTANCE (send message)
        (P-BIT  TRAP)   ;INSTANCE-HEADER
        (P-BIT  TRAP)   ;ENTITY (eventually send message?)
        (P-BIT  TRAP)   ;unused-32.
        (P-BIT  TRAP)   ;SELF-REF-POINTER
        (P-BIT QCDRNM)          ;CHARACTER
        (P-BIT  TRAP)   ;rplacd-forward
        (P-BIT TRAP)    ;spare
        (P-BIT  TRAP)   ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT TRAP))
(END-DISPATCH)
#-LAMBDA (end-comment)


(LOCALITY D-MEM)
#+cdr-next-is-0 (begin-comment)
(START-DISPATCH 2 0)    ;MAYBE DOES XCT-NEXT
;DISPATCH ON CDR-CODE WHEN TAKING CDR
;POPJ-XCT-NEXT IF CDR-NEXT (PROBABLY MOST FREQUENT CASE)
CDR-CDR-DISPATCH
        (INHIBIT-XCT-NEXT-BIT CDR-FULL-NODE)    ;FULL-NODE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR NOT
        (INHIBIT-XCT-NEXT-BIT CDR-IS-NIL)       ;CDR NIL
        (R-BIT)                                 ;CDR NEXT
(END-DISPATCH)
#+cdr-next-is-0 (end-comment)
#-cdr-next-is-0 (begin-comment)
(START-DISPATCH 2 0)    ;MAYBE DOES XCT-NEXT
;DISPATCH ON CDR-CODE WHEN TAKING CDR
;POPJ-XCT-NEXT IF CDR-NEXT (PROBABLY MOST FREQUENT CASE)
CDR-CDR-DISPATCH
        (R-BIT)                                 ;CDR NEXT
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR NOT
        (INHIBIT-XCT-NEXT-BIT CDR-FULL-NODE)    ;FULL-NODE
        (INHIBIT-XCT-NEXT-BIT CDR-IS-NIL)       ;CDR NIL
(END-DISPATCH)

#-cdr-next-is-0 (end-comment)

#+cdr-next-is-0 (begin-comment)
(START-DISPATCH 2 0)    ;MAYBE DOES XCT-NEXT
;DISPATCH ON CDR-CODE WHEN TAKING CDR
;use this one if the cdr-code of the VMA is not known to be 0.
; instead of popj on CDR-NEXT, drop thru.  This could be flushed if we
; had OUTPUT-SELECTOR-MASK-30.
;POPJ-XCT-NEXT IF CDR-NEXT (PROBABLY MOST FREQUENT CASE)
CDR-CDR-DISPATCH-MASK-VMA-ON-CDR-NEXT
        (INHIBIT-XCT-NEXT-BIT CDR-FULL-NODE)    ;FULL-NODE
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR NOT
        (INHIBIT-XCT-NEXT-BIT CDR-IS-NIL)       ;CDR NIL
        (P-BIT R-BIT)                                   ;CDR NEXT
(END-DISPATCH)
#+cdr-next-is-0 (end-comment)
#-cdr-next-is-0 (begin-comment)
(START-DISPATCH 2 0)    ;MAYBE DOES XCT-NEXT
;DISPATCH ON CDR-CODE WHEN TAKING CDR
;use this one if the cdr-code of the VMA is not known to be 0.
; instead of popj on CDR-NEXT, drop thru.  This could be flushed if we
; had OUTPUT-SELECTOR-MASK-30.
;POPJ-XCT-NEXT IF CDR-NEXT (PROBABLY MOST FREQUENT CASE)
CDR-CDR-DISPATCH-MASK-VMA-ON-CDR-NEXT
        (P-BIT R-BIT)                                   ;CDR NEXT
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR NOT
        (INHIBIT-XCT-NEXT-BIT CDR-FULL-NODE)    ;FULL-NODE
        (INHIBIT-XCT-NEXT-BIT CDR-IS-NIL)       ;CDR NIL
(END-DISPATCH)
#-cdr-next-is-0 (end-comment)

(START-DISPATCH 2 0)
;DISPATCH ON M-CAR-SYM-MODE WHEN TAKING CAR OF SYM
CAR-SYM-DISPATCH
        (P-BIT TRAP)    ;ERROR
        (P-BIT R-BIT)   ;ERROR EXCEPT (CAR NIL) = NIL
        (XFALSE)        ;NIL
        (P-BIT TRAP)    ;UNUSED
(END-DISPATCH)

(START-DISPATCH 2)
;DISPATCH ON M-CAR-NUM-MODE WHEN TAKING CAR OF NUM
CAR-NUM-DISPATCH
        (P-BIT TRAP)    ;ERROR
        (XFALSE)        ;NIL
        (P-BIT TRAP)    ;"WHATEVER IT IS"
        (P-BIT TRAP)    ;ERROR
(END-DISPATCH)

(START-DISPATCH 2 0)
;DISPATCH ON M-CDR-SYM-MODE WHEN TAKING CDR OF SYM
CDR-SYM-DISPATCH
        (P-BIT TRAP)    ;ERROR
        (P-BIT R-BIT)   ;ERROR EXCEPT (CDR NIL) = NIL
        (R-BIT)         ;NIL -> NIL
        (QCDPRP)        ;PROPERTY LIST
(END-DISPATCH)

(START-DISPATCH 2)
;DISPATCH ON M-CDR-NUM-MODE WHEN TAKING CDR OF NUM
CDR-NUM-DISPATCH
        (P-BIT TRAP)    ;ERROR
        (XFALSE)        ;NIL
        (P-BIT TRAP)    ;"WHATEVER IT IS"
        (P-BIT TRAP)
(END-DISPATCH)
(LOCALITY I-MEM)

;; Multiple CAR/CDR functions.

;; XCAAR, etc. pop arg off stack and return value in M-T.
;; They generally clobber M-A to save the original argument for errors.

;; QCDDR, like QCAR and QCDR, is for use as a subroutine from the microcode.
;; It takes arg in M-T and returns value in M-T.
;; If any other multiple car/cdr function is needed as a subroutine,
;; create a QC...R entry point name for it.

;; QMAA, QMDD, etc. are obsolete names for QCAAR, QCDDR, etc.,
;; still used by the microcompiler.  Eventually this series of names
;; should go away and the QCAAR series used for both purposes.
;; Meanwhile, QMAA ... should not be referred to except from this page.
;; and the following page.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADDDR M-A)

XCADDDR (MISC-INST-ENTRY CADDDR)
        ((M-A) Q-TYPED-POINTER PDL-TOP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
QMADDD
        (open-qcdr m-t)
QMADD
        (open-qcdr m-t)
QMAD
        (open-qcdr m-t)
        (JUMP QCAR)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAAAAR M-A)

XCAAAAR (MISC-INST-ENTRY CAAAAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
QMAAAA
        (open-qcar m-t)
QMAAA
        (open-qcar m-t)
QMAA
        (open-qcar m-t)
        (JUMP QCAR)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDDDR M-A)

XCDDDDR (MISC-INST-ENTRY CDDDDR)
        ((M-A) Q-TYPED-POINTER PDL-TOP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
QMDDDD
        (open-qcdr m-t)
QMDDD
        (open-qcdr m-t)
QMDD
        (open-qcdr m-t)
        (JUMP QCDR)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAAADR M-A)

XCAAADR (MISC-INST-ENTRY CAAADR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMAAA)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMAAAD
        (open-qcdr m-t)
        (JUMP QMAAA)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDDAR M-A)

XCDDDAR (MISC-INST-ENTRY CDDDAR)
        (open-qcar-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMDDD)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMDDDA
        (open-qcar m-t)
        (JUMP QMDDD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAADDR M-A)

XCAADDR (MISC-INST-ENTRY CAADDR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
QMAADD
        (open-qcdr m-t)
QMAAD
        (open-qcdr m-t)
        (JUMP QMAA)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAADAR M-A)

XCAADAR (MISC-INST-ENTRY CAADAR)
        (open-qcar-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMAAD)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMAADA
        (open-qcar m-t)
        (JUMP QMAAD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDAAR M-A)

XCDDAAR (MISC-INST-ENTRY CDDAAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
QMDDAA
        (open-qcar m-t)
QMDDA
        (open-qcar m-t)
        (JUMP QMDD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDADR M-A)

XCDDADR (MISC-INST-ENTRY CDDADR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMDDA)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMDDAD
        (open-qcdr m-t)
        (JUMP QMDDA)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADAAR M-A)

XCADAAR (MISC-INST-ENTRY CADAAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
QMADAA
        (open-qcar m-t)
QMADA
        (open-qcar m-t)
        (JUMP QMAD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADADR M-A)

XCADADR (MISC-INST-ENTRY CADADR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMADA)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMADAD
        (open-qcdr m-t)
        (JUMP QMADA)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADDAR M-A)

XCADDAR (MISC-INST-ENTRY CADDAR)
        (open-qcar-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMADD)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMADDA
        (open-qcar m-t)
        (JUMP QMADD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDADAR M-A)

XCDADAR (MISC-INST-ENTRY CDADAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
QMDADA
        (open-qcar m-t)
QMDAD
        (open-qcdr m-t)
        (JUMP QMDA)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDADDR M-A)

XCDADDR (MISC-INST-ENTRY CDADDR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (open-qcdr-xct-next m-t)
       ((M-A) Q-TYPED-POINTER PDL-POP)
        (JUMP QMDA)

QMDADD
        (open-qcdr m-t)
        (JUMP QMDAD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDAAAR M-A)

XCDAAAR (MISC-INST-ENTRY CDAAAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
QMDAAA
        (open-qcar m-t)
QMDAA
        (open-qcar m-t)
QMDA
        (open-qcar m-t)
        (JUMP QCDR)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDAADR M-A)

XCDAADR (MISC-INST-ENTRY CDAADR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QMDAA)
       ((M-A) Q-TYPED-POINTER PDL-POP)

QMDAAD
        (open-qcdr m-t)
        (JUMP QMDAA)


;For CAAAR ... CDDDR, the arg is in M-A whenever an error occurs.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAAAR M-A)

XCAAAR (MISC-INST-ENTRY CAAAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMAAA)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAADR M-A)

XCAADR (MISC-INST-ENTRY CAADR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMAAD)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADAR M-A)

XCADAR (MISC-INST-ENTRY CADAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMADA)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADDR M-A)

XCADDR (MISC-INST-ENTRY CADDR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMADD)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDAAR M-A)

XCDAAR (MISC-INST-ENTRY CDAAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMDAA)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDADR M-A)

XCDADR (MISC-INST-ENTRY CDADR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMDAD)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDAR M-A)

XCDDAR (MISC-INST-ENTRY CDDAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMDDA)
       ((M-A) M-T)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDDR M-A)

XCDDDR (MISC-INST-ENTRY CDDDR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT QMDDD)
       ((M-A) M-T)

;For CAAR ... CDDR, the arg is in M-A unless an ARG-POPPED says it is elsewhere.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAAR M-A)

XCAAR  (MISC-INST-ENTRY M-CAAR)
        (open-qcar-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QCAR)
       ((M-A) Q-TYPED-POINTER PDL-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADR M-A)

XCADR  (MISC-INST-ENTRY M-CADR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QCAR)
       ((M-A) Q-TYPED-POINTER PDL-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDAR M-A)

XCDAR  (MISC-INST-ENTRY M-CDAR)
        (open-qcar-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QCDR)
       ((M-A) Q-TYPED-POINTER PDL-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDR M-A)

XCDDR  (MISC-INST-ENTRY M-CDDR)
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
    (ERROR-TABLE ARG-POPPED 0 PP)
        (JUMP-XCT-NEXT QCDR)
       ((M-A) Q-TYPED-POINTER PDL-POP)

XSETELT (MISC-INST-ENTRY SETELT)
        ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 2))
        (jump-data-type-equal c-pdl-buffer-index
                 (a-constant (byte-value q-data-type dtp-array-pointer)) xset-ar-1)
        (CALL-XCT-NEXT XNTHCDR-REVERSE)
       ((M-D) Q-TYPED-POINTER PDL-POP)
        ((M-S) M-T)
        ((M-T) M-D)
        (JUMP-XCT-NEXT XSETCAR1)
       ((M-A) M-T)

XELT (MISC-INST-ENTRY ELT)
        ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 1))
        (jump-data-type-equal c-pdl-buffer-index
                (a-constant (byte-value q-data-type dtp-array-pointer)) xcommon-lisp-ar-1)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QCAR)))
XNTHCDR-REVERSE
                (ERROR-TABLE RESTART XNTHCDR0)
        (DISPATCH Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP nonnegative-fixnum PP 0 XNTHCDR0)
        (CALL-IF-BIT-SET-XCT-NEXT BOXED-SIGN-BIT PDL-TOP TRAP)
       ((M-B) Q-TYPED-POINTER PDL-TOP)
            (ERROR-TABLE ARGTYP nonnegative-fixnum PP 0 XNTHCDR0)
        ((M-1) Q-POINTER PDL-POP)               ;Count
        (JUMP-XCT-NEXT XNTHCDR-0)
       ((M-T) Q-TYPED-POINTER PDL-POP)  ;List

(ERROR-TABLE DEFAULT-ARG-LOCATIONS NTH PP M-T)
(ERROR-TABLE DEFAULT-ARG-LOCATIONS NTHCDR PP M-T)

;Drops in
XNTH (MISC-INST-ENTRY NTH)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QCAR)))
        ;drops in
XNTHCDR (MISC-INST-ENTRY NTHCDR)
        ((M-T) Q-TYPED-POINTER PDL-POP) ;List
                (ERROR-TABLE RESTART XNTHCDR0)
        (DISPATCH Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP nonnegative-fixnum PP 0 XNTHCDR0)
        (CALL-IF-BIT-SET-XCT-NEXT BOXED-SIGN-BIT PDL-TOP TRAP)
       ((M-B) Q-TYPED-POINTER PDL-TOP)
            (ERROR-TABLE ARGTYP nonnegative-fixnum PP 0 XNTHCDR0)
        ((M-1) Q-POINTER PDL-POP)               ;Count
XNTHCDR-0
        (POPJ-EQUAL-XCT-NEXT M-1 A-ZERO)
       ((M-A) M-T)
XNTHCDR-1
        (POPJ-EQUAL M-T A-V-NIL)
        (open-qcdr-sb m-t)
            (ERROR-TABLE CALLS-SUB NTHCDR)
            (ERROR-TABLE ARG-POPPED 0 M-B M-A)
        (JUMP-GREATER-THAN-XCT-NEXT M-1 (A-CONSTANT 1) XNTHCDR-1)
       ((M-1) SUB M-1 (A-CONSTANT 1))
        (POPJ)

;; Leave the CDR on the stack and return the CAR.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CARCDR M-T)

XCARCDR (MISC-INST-ENTRY CARCDR)
;#+CADR (CALL-XCT-NEXT CARCDR)
;#+CADR ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
#+exp   (open-carcdr m-t)
#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-T CARCDR-DISPATCH-DIRECT)
#+LAMBDA(NO-OP)
        (POPJ-AFTER-NEXT
         (PDL-PUSH) M-T)
       ((M-T) M-A)

;; "Safe" forms of CAR, CDR etc.
;; These treat any non-list as NIL, and never get an error
;; if the arg is a valid Lisp object.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CADR-SAFE M-T)

XCADR-SAFE (MISC-INST-ENTRY CADR-SAFE)
        (CALL-XCT-NEXT CDR-SAFE)
;; DROPS THRU with XCT-NEXT!

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CAR-SAFE M-T)

;; DROPS THRU with XCT-NEXT!
XCAR-SAFE (MISC-INST-ENTRY CAR-SAFE)
        ((M-T) Q-TYPED-POINTER PDL-POP)
CAR-SAFE
        (JUMP-XCT-NEXT QCAR)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) xfalse)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDR-SAFE M-T)

XCDDR-SAFE (MISC-INST-ENTRY CDDR-SAFE)
        (CALL-XCT-NEXT CDR-SAFE)
;; DROPS THRU with XCT-NEXT!

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDR-SAFE M-T)

;; DROPS THRU with XCT-NEXT!
XCDR-SAFE (MISC-INST-ENTRY CDR-SAFE)
        ((M-T) Q-TYPED-POINTER PDL-POP)
CDR-SAFE
        (JUMP-XCT-NEXT QCDR-SB)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) xfalse)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS CDDDDR-SAFE M-T)

XCDDDDR-SAFE (MISC-INST-ENTRY CDDDDR-SAFE)
        (CALL XCDDR-SAFE)
        (CALL CDR-SAFE)
        (JUMP CDR-SAFE)


(ERROR-TABLE DEFAULT-ARG-LOCATIONS NTH-SAFE PP M-T)
(ERROR-TABLE DEFAULT-ARG-LOCATIONS NTHCDR-SAFE PP M-T)

;Drops in
XNTH-SAFE (MISC-INST-ENTRY NTH-SAFE)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC CAR-SAFE)))
        ;drops in
XNTHCDR-SAFE (MISC-INST-ENTRY NTHCDR-SAFE)
        ((M-T) Q-TYPED-POINTER PDL-POP) ;List
                (ERROR-TABLE RESTART XNTHCDR-SAFE-0)
        (DISPATCH Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP nonnegative-fixnum PP 0 XNTHCDR-SAFE-0)
        (CALL-IF-BIT-SET-XCT-NEXT BOXED-SIGN-BIT PDL-TOP TRAP)
       ((M-B) Q-TYPED-POINTER PDL-TOP)
            (ERROR-TABLE ARGTYP nonnegative-fixnum PP 0 XNTHCDR-SAFE-0)
        ((M-1) Q-POINTER PDL-POP)               ;Count
        (POPJ-EQUAL-XCT-NEXT M-1 A-ZERO)
       ((M-A) M-T)
XNTHCDR-SAFE-1
        (POPJ-EQUAL M-T A-V-NIL)
        (CALL CDR-SAFE)
            (ERROR-TABLE CALLS-SUB NTHCDR-SAFE)
            (ERROR-TABLE ARG-POPPED 0 M-B M-A)
        (JUMP-GREATER-THAN-XCT-NEXT M-1 (A-CONSTANT 1) XNTHCDR-SAFE-1)
       ((M-1) SUB M-1 (A-CONSTANT 1))
        (POPJ)

;;; RPLACA AND RPLACD

  (ERROR-TABLE DEFAULT-ARG-LOCATIONS RPLACA M-S M-T)
  (MISC-INST-ENTRY RPLACA)
XRPLCA  ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-S) Q-TYPED-POINTER PDL-POP)
QRAR1   ((M-A) M-S)
;; M-A has the value we should return; differs for RPLACA vs SETCAR.
   (ERROR-TABLE RESTART RPLACA)
XSETCAR1
        (DISPATCH (I-ARG INSTANCE-INVOKE-SET-CAR) Q-DATA-TYPE M-S QRACDT)
   (ERROR-TABLE ARGTYP CONS M-S 0 RPLACA)

QRAR4   ((VMA-START-READ) M-S)                  ;FETCH WORD TO BE SMASHED
        (CHECK-PAGE-READ)                       ;NO INT, CALLED BY MVR
        (DISPATCH TRANSPORT-WRITE MD)   ;CHASE INVISIBLES
        ((MD-START-WRITE) SELECTIVE-DEPOSIT
                MD Q-ALL-BUT-TYPED-POINTER A-T) ;STORE M-T INTO Q-TYPED-PNTR
        (CHECK-PAGE-WRITE)                      ;NO SEQ BRK, CALLED BY MVR (???)
        (GC-WRITE-TEST)
        (popj-after-next)
        ((m-t) q-typed-pointer m-a)

;No longer used for RPLACA, but some random places still call it.
QRAR3   ((VMA-START-READ) M-S)                  ;FETCH WORD TO BE SMASHED
        (CHECK-PAGE-READ)                       ;NO INT, CALLED BY MVR
        (DISPATCH TRANSPORT-WRITE MD)   ;CHASE INVISIBLES
        ((MD-START-WRITE) SELECTIVE-DEPOSIT
                MD Q-ALL-BUT-TYPED-POINTER A-T) ;STORE M-T INTO Q-TYPED-PNTR
        (CHECK-PAGE-WRITE)                      ;NO SEQ BRK, CALLED BY MVR (???)
        (GC-WRITE-TEST)
        (popj-after-next)
        ((m-t) q-typed-pointer m-s)

;; Here for SETCAR or SETCDR of an instance.  Send a message to it.
;; I-ARG already set up to indicate which operation.
XSETCARCDR-INSTANCE
        ((PDL-PUSH) M-A)
        ((M-A) M-T)
        ((M-T) M-S)             ;M-S is what has the instance.  Put in M-T.
        (CALL INSTANCE-INVOKE-1)
        ((PDL-PUSH) dpb M-A q-typed-pointer
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))   ;Pass desired car or cdr as arg.
        ((ARG-CALL MMCALL) (I-ARG 2))   ;Call, 2 arg.  Value comes back in M-T.
        (POPJ-AFTER-NEXT)
       ((M-T) q-typed-pointer PDL-POP)  ;Ignore that value, return what we are supposed to return.

  (ERROR-TABLE DEFAULT-ARG-LOCATIONS RPLACD M-S M-T)
  (MISC-INST-ENTRY RPLACD)
;MUSTN'T CLOBBER M-C OR M-R BECAUSE CALLED BY MULTIPLE-VALUE-LIST
;NOW CLOBBERS M-S, M-T, M-I, M-A
XRPLCD  ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-S) Q-TYPED-POINTER PDL-POP)
QRDR1   ((M-A) M-S)
;; M-A has the value we should return; differs for RPLACD vs SETCDR.
   (ERROR-TABLE RESTART RPLACD)
XSETCDR1
        (DISPATCH (I-ARG INSTANCE-INVOKE-SET-CDR) Q-DATA-TYPE M-S QRDCDT)
   (ERROR-TABLE ARGTYP CONS M-S 0 RPLACD)

QRDRSY  (DISPATCH M-CDR-SYM-MODE RPLACD-SYM-DISPATCH)
   (ERROR-TABLE ARGTYP CONS M-S 0 RPLACD)

(LOCALITY D-MEM)
(START-DISPATCH 2 INHIBIT-XCT-NEXT-BIT)
;DISPATCH ON DOING RPLACD OF SYM
RPLACD-SYM-DISPATCH
        (P-BIT TRAP)    ;ERROR
        (P-BIT TRAP)    ;ERROR
        (P-BIT TRAP)    ;ERROR
        (QRDPRP)        ;SMASH PROP LIST
(END-DISPATCH)
(LOCALITY I-MEM)

QRDPRP  ((M-S) ADD (A-CONSTANT 3) M-S)          ;RPLACD ING SYMBOL (IN P-LIST MODE)
        (JUMP-XCT-NEXT XSETCDR1)
       ((M-S) DPB M-S Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))

QRDR3   ((VMA-START-READ) M-S)                  ;GET CAR OF CONS TO BE SMASHED
        (CHECK-PAGE-READ)                       ;NO SEQ BRK, CDR CODE IN HAND, ALSO MVR
        (DISPATCH TRANSPORT MD) ;CHASE INVISIBLE, must transport since MD used below.
        (DISPATCH-XCT-NEXT Q-CDR-CODE MD RPLACD-CDR-DISPATCH)
   (ERROR-TABLE BAD-CDR-CODE VMA)
       ((M-I) MD)

(LOCALITY D-MEM)
(START-DISPATCH 2 0)    ;DOES XCT-NEXT
;DISPATCH ON CDR-CODE WHEN DOING RPLACD
RPLACD-CDR-DISPATCH
        (RPLACD-CDR-NEXT)       ;CDR NEXT
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR NOT
        (RPLACD-FULL-NODE)      ;FULL NODE
        (RPLACD-NEXT-NIL)       ;CDR NIL
(END-DISPATCH)
(LOCALITY I-MEM)

rplacd-full-node
        ((vma-start-read) add vma (a-constant 1))      ;Get word to smash.
        (check-page-read)
        (dispatch transport-write md)                  ;Chase invisibles.
        ((md-start-write) selective-deposit md q-all-but-typed-pointer a-t)
        (check-page-write)
        (gc-write-test)
rplacd-exit
        (popj-after-next
          (m-t) q-typed-pointer m-a)
       (no-op)

rplacd-next-nil
        (jump-equal m-t a-v-nil rplacd-exit)    ;RPLACD with nil and cdr already nil, no-op.
     ;; Fall through.
rplacd-cdr-next
        ((pdl-push) m-a)                        ;Save for return value.
        ((pdl-push) vma)                        ;Address of cell to be forwarded.
     ;; This used to cons in the same area as the forwarded cell, but I think it's OK
     ;; just to do it in the background area, and it's a lot faster.  KHS 850601.
        ((pdl-push) m-i)                        ;CAR of new cell.
        ((pdl-push) m-t)                        ;CDR of new cell.
        (call-xct-next qcons)
       ((m-s) dpb m-zero q-all-but-typed-pointer a-background-cons-area)
     ;; Forward original CAR with magic forwarding pointer.  It is important that
     ;; this pointer have CDR-ERROR on it (see structure-info).
        ((md) dpb m-t q-pointer (a-constant (plus (byte-value q-data-type dtp-rplacd-forward)
                                                  (byte-value q-cdr-code cdr-error))))
        ((vma-start-write) pdl-pop)
        (check-page-write)
        (gc-write-test)
        (popj-after-next
          (m-t) q-typed-pointer pdl-pop)
       (no-op)

;Same as RPLACD but returns second argument (the value stored).
XSETCDR (MISC-INST-ENTRY SETCDR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-S) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT XSETCDR1)
       ((M-A) M-T)              ;Save the arg where QRDR1 will return it.

;Same as RPLACA but returns second argument (the value stored).
XSETCAR (MISC-INST-ENTRY SETCAR)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-S) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT XSETCAR1)
       ((M-A) M-T)              ;Save the arg where QRAR1 will return it.

(LOCALITY D-MEM)
(START-DISPATCH 5 INHIBIT-XCT-NEXT-BIT)
;DISP ON DATA TYPE OF POINTER-TO-SMASH-CONTENTS-OF WHEN DOING RPLACA
QRACDT  (P-BIT TRAP)    ;TRAP
        (P-BIT TRAP)    ;NULL
        (P-BIT UNRECONCILED-TRAP)       ;UNRECONCILED
        (P-BIT TRAP)    ;SYMBOL
        (P-BIT TRAP)    ;SYMBOL-HEADER
        (P-BIT TRAP)    ;FIX
        (P-BIT TRAP)    ;EXTENDED NUMBER
        (P-BIT TRAP)    ;HEADER
        (P-BIT TRAP)    ;GC-FORWARD
        (P-BIT TRAP)    ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT TRAP)    ;ONE-Q-FORWARD
        (P-BIT TRAP)    ;HEADER-FORWARD
        (P-BIT TRAP)    ;BODY-FORWARD
        (QRAR4)         ;LOCATIVE
        (QRAR4)         ;LIST
        (P-BIT TRAP)    ;U CODE ENTRY
        (P-BIT TRAP)    ;FEF
        (P-BIT TRAP)    ;ARRAY-POINTER
        (P-BIT TRAP)    ;ARRAY-HEADER
        (P-BIT TRAP)    ;STACK-GROUP
        (P-BIT TRAP)    ;CLOSURE
        (P-BIT TRAP)    ;INDEXED-FORWARD
        (P-BIT TRAP)    ;SELECT-METHOD
        (XSETCARCDR-INSTANCE)   ;INSTANCE
        (P-BIT TRAP)    ;INSTANCE-HEADER
        (P-BIT TRAP)    ;ENTITY
        (P-BIT TRAP)    ;unused-32
        (P-BIT TRAP)    ;SELF-REF-POINTER
        (P-BIT TRAP)    ;CHARACTER
        (p-bit trap)    ;rplacd-forward
        (P-BIT TRAP)    ;spare
        (P-BIT TRAP)    ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT TRAP))
(END-DISPATCH)

(START-DISPATCH 5 INHIBIT-XCT-NEXT-BIT)
;DISPATCH ON DATA TYPE OF POINTER-TO-SMASH-CONTENTS-OF WHEN DOING RPLACD
QRDCDT  (P-BIT TRAP)    ;TRAP
        (P-BIT TRAP)    ;NULL
        (P-BIT UNRECONCILED-TRAP)       ;UNRECONCILED
        (QRDRSY)        ;SYMBOL
        (P-BIT TRAP)    ;SYMBOL-HEADER
        (P-BIT TRAP)    ;FIX
        (P-BIT TRAP)    ;EXTENDED NUMBER
        (P-BIT TRAP)    ;HEADER
        (P-BIT TRAP)    ;GC-FORWARD
        (P-BIT TRAP)    ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT TRAP)    ;ONE-Q-FORWARD
        (P-BIT TRAP)    ;HEADER-FORWARD
        (P-BIT TRAP)    ;BODY-FORWARD
        (QRAR4)         ;LOCATIVE. NOTE CAR!!
        (QRDR3)         ;LIST
        (P-BIT TRAP)    ;U CODE ENTRY
        (P-BIT TRAP)    ;FEF
        (P-BIT TRAP)    ;ARRAY-POINTER
        (P-BIT TRAP)    ;ARRAY-HEADER
        (P-BIT TRAP)    ;STACK-GROUP
        (P-BIT TRAP)    ;CLOSURE
        (P-BIT TRAP)    ;INDEXED-FORWARD
        (P-BIT TRAP)    ;SELECT-METHOD
        (XSETCARCDR-INSTANCE)   ;INSTANCE
        (P-BIT TRAP)    ;INSTANCE-HEADER
        (P-BIT TRAP)    ;ENTITY
        (P-BIT TRAP)    ;unused-32
        (P-BIT TRAP)    ;SELF-REF-POINTER
        (P-BIT TRAP)    ;CHARACTER
        (p-bit trap)    ;rplacd-forward
        (P-BIT TRAP)    ;spare
        (P-BIT TRAP)    ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT TRAP))
(END-DISPATCH)
(LOCALITY I-MEM)

;;; EQUAL

XEQUALP (MISC-INST-ENTRY EQUALP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-B) Q-TYPED-POINTER PDL-POP)
XEQUALP-0
        (JUMP-EQUAL M-T A-B XTRUE)
        (CALL XEQUALP-1)        ;Handle numeric case.
        ;Else different types means unequal.
        (jump-data-type-not-equal m-t a-b xfalse)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-character))
                xequalp-common)
;Both args are character objects.  Compare, ignoring bucky bits, font and case.
        ((PDL-PUSH) M-B)
        ((PDL-PUSH) M-T)
        (JUMP XCHAR-EQUAL)

XEQUALP-COMMON
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-array-pointer))
                xequalp-array)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) xfalse)
        ;; Now we are a list
        ((PDL-PUSH) M-T)
        (CALL-XCT-NEXT QCAR3)
       ((PDL-PUSH) M-B)
        ((M-B) M-T)
        (CALL-XCT-NEXT QCAR3)
       ((M-T) PDL-TOP)
        ;; If the micro stack is filling up, make new stack frame.
#+lambda(JUMP-GREATER-THAN MICRO-STACK-PNTR-AND-DATA (A-CONSTANT 20._24.)
                           XEQUALP-SLOW-RECURSE)
#+exp   (JUMP-GREATER-THAN MICRO-STACK-PNTR (A-CONSTANT 10.)
                           XEQUALP-SLOW-RECURSE)
        ;; Otherwise, test for EQUALity of the two cars.
        (CALL XEQUALP-0)

XEQUALP-CDR
        (JUMP-EQUAL M-T A-V-NIL XEQUAL-DIFFERENT-CARS)
        ;; If the cars match, tail-recursively check the two cdrs.
        (open-qcdr-sb-xct-next pdl-top)
       ((M-T) PDL-POP)
        ((M-B) M-T)
        (open-qcdr-sb-xct-next pdl-top)
       ((M-T) PDL-POP)
        (JUMP XEQUALP-0)

XEQUALP-SLOW-RECURSE
        (CALL P3ZERO)
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-EQUALP))
        (DISPATCH TRANSPORT MD)
        ((PDL-PUSH) MD)
        ((PDL-PUSH) DPB M-T Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((PDL-PUSH) DPB M-B Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-CALL MMCALL) (I-ARG 2))
        (JUMP XEQUALP-CDR)

;; If both arrays are strings, call STRING-EQUAL;
;; otherwise answer is computed by macrocode.
xequalp-array
        (CALL XEQUAL-READ-HEADERS)
        ((M-3) (LISP-BYTE %%ARRAY-NUMBER-DIMENSIONS) M-1)
        ((M-2) (LISP-BYTE %%ARRAY-NUMBER-DIMENSIONS) MD)
        (JUMP-NOT-EQUAL M-3 A-2 XFALSE) ;Not same rank.
        (JUMP-NOT-EQUAL M-3 (A-CONSTANT 1) XEQUALP-ARRAY-HARD)
;; Both rank 1.
XEQUALP-STRING-1
        ((M-3) (LISP-BYTE %%ARRAY-TYPE-FIELD) M-1)
        ((M-2) (LISP-BYTE %%ARRAY-TYPE-FIELD) MD)
        (JUMP-EQUAL M-3 (A-CONSTANT (EVAL (LSH ART-STRING ARRAY-TYPE-SHIFT)))
                    XEQUALP-BOTH-STRINGSP-1)
        (JUMP-NOT-EQUAL M-3 (A-CONSTANT (EVAL (LSH ART-FAT-STRING ARRAY-TYPE-SHIFT)))
                        XEQUALP-ARRAY-NOT-STRING)
XEQUALP-BOTH-STRINGSP-1
        (JUMP-EQUAL M-2 (A-CONSTANT (EVAL (LSH ART-STRING ARRAY-TYPE-SHIFT)))
                    XEQUALP-BOTH-STRINGSP-2)
        (JUMP-NOT-EQUAL M-2 (A-CONSTANT (EVAL (LSH ART-FAT-STRING ARRAY-TYPE-SHIFT)))
                        XEQUALP-ARRAY-NOT-STRING)
;; Both strings and rank 1.
XEQUALP-BOTH-STRINGSP-2
;Call STRING-EQUAL, which will check for arrays having same size and same elements.
        ((PDL-PUSH) A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
        ((A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON) A-V-NIL)
        ((PDL-PUSH) A-T)
        ((PDL-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) )
        ((PDL-PUSH) A-B)
        ((PDL-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-XCT-NEXT XSTRING-EQUAL);uses pdl pass-around
       ((PDL-PUSH) A-V-NIL)
        (POPJ-AFTER-NEXT
         (A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON) PDL-POP)
       (NO-OP)

XEQUALP-ARRAY-NOT-STRING
XEQUALP-ARRAY-HARD
 ;ALWAYS CALL MACROCODE, NO NEED TO DETECT ART-1B CASE.
        (CALL P3ZERO)
       ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-EQUALP-ARRAY))
        (DISPATCH TRANSPORT MD)
        ((PDL-PUSH) MD)
        ((PDL-PUSH) DPB M-T Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((PDL-PUSH) DPB M-B Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-CALL MMCALL) (I-ARG 2))
        (POPJ)

;;Numbers are EQUALP if =
XEQUALP-1
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-T POPJ-IF-NOT-NUMBER)
       ((M-A) (A-CONSTANT ARITH-2ARG-EQUAL))
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-B POPJ-IF-NOT-NUMBER)
       ((NO-OP))
        ((PDL-PUSH) M-B)
        (JUMP-XCT-NEXT QMEQL)
       ((M-GARBAGE) MICRO-STACK-DATA-POP)


XEQUAL  (MISC-INST-ENTRY EQUAL)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-B) Q-TYPED-POINTER PDL-POP)
;; Args in M-B, M-T.
XEQUAL-0
        (JUMP-EQUAL M-T A-B XTRUE)
        ;False if args are different data types.
        (jump-data-type-not-equal m-t a-b xfalse)
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-extended-number))
                xequal-xnum)
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-array-pointer))
                xequal-array)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) xfalse)
        ;; Now we are a list
        ((PDL-PUSH) M-T)
        (CALL-XCT-NEXT QCAR3)
       ((PDL-PUSH) M-B)
        ((M-B) M-T)
        (CALL-XCT-NEXT QCAR3)
       ((M-T) PDL-TOP)
        ;; If the micro stack is filling up, make new stack frame.
#+lambda(JUMP-GREATER-THAN MICRO-STACK-PNTR-AND-DATA (A-CONSTANT 20._24.)
                           XEQUAL-SLOW-RECURSE)
#+exp   (JUMP-GREATER-THAN MICRO-STACK-PNTR (A-CONSTANT 10.)
                           XEQUAL-SLOW-RECURSE)
        ;; Otherwise, test for EQUALity of the two cars.
        (CALL XEQUAL-0)

XEQUAL-CDR
        (JUMP-EQUAL M-T A-V-NIL XEQUAL-DIFFERENT-CARS)
        ;; If the cars match, tail-recursively check the two cdrs.
        (open-qcdr-sb-xct-next pdl-top)
       ((M-T) PDL-POP)
        ((M-B) M-T)
        (open-qcdr-sb-xct-next pdl-top)
       ((M-T) PDL-POP)
        (JUMP XEQUAL-0)

XEQUAL-DIFFERENT-CARS   ;also used for XEQUALP
        (POPJ-AFTER-NEXT (PDL-BUFFER-POINTER) SUB PDL-BUFFER-POINTER (A-CONSTANT 2))
       (NO-OP)

XEQUAL-SLOW-RECURSE
        (CALL P3ZERO)
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-EQUAL))
        (DISPATCH TRANSPORT MD)
        ((PDL-PUSH) MD)
        ((PDL-PUSH) DPB M-T Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((PDL-PUSH) DPB M-B Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-CALL MMCALL) (I-ARG 2))
        (JUMP XEQUAL-CDR)

;; Read headers of both args to EQUAL or EQUALP
;; Header from M-T goes in M-1.
;; Header from M-B goes in MD.
XEQUAL-READ-HEADERS
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((M-1) MD)
        ((VMA-START-READ) M-B)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        (POPJ)

;; Here for EQUAL of two extended numbers.
;; Compare their header types first.
XEQUAL-XNUM
        (CALL XEQUAL-READ-HEADERS)
        ((M-1) (LISP-BYTE %%HEADER-TYPE-FIELD) M-1)
        ((M-2) (LISP-BYTE %%HEADER-TYPE-FIELD) MD)
        (JUMP-NOT-EQUAL M-1 A-2 XFALSE)
;; Header type fields match; use EQL (not =) to compare the numbers.
;; (equal #c(3 -4.0) #c(3 -4)) ==> false
;; not to mention that rational and floats wont mix in complex Lisp Machine
;; numbers.
        ((PDL-PUSH) M-B)
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-B D-NUMARG1)
       ((M-A) (A-CONSTANT ARITH-2ARG-EQL))
;; Will not fall through, since numbers are not fixnums.

;; If both arrays are strings, call STRING-EQUAL;
;; otherwise answer is NIL for EQUAL, or computed by macrocode for EQUALP.
XEQUAL-ARRAY
        (CALL XEQUAL-READ-HEADERS)
        ((M-3) (LISP-BYTE %%ARRAY-NUMBER-DIMENSIONS) M-1)
        ((M-2) (LISP-BYTE %%ARRAY-NUMBER-DIMENSIONS) MD)
        (JUMP-NOT-EQUAL M-3 A-2 XFALSE) ;Not same rank.
        (JUMP-NOT-EQUAL M-3 (A-CONSTANT 1) XFALSE)  ;only strings or bit-vectors to have a chance
;; Both rank 1.
XEQUAL-STRING-1
        ((M-3) (LISP-BYTE %%ARRAY-TYPE-FIELD) M-1)
        ((M-2) (LISP-BYTE %%ARRAY-TYPE-FIELD) MD)
        (JUMP-EQUAL M-3 (A-CONSTANT (EVAL (LSH ART-STRING ARRAY-TYPE-SHIFT)))
                    XEQUAL-BOTH-STRINGSP-1)
        (JUMP-NOT-EQUAL M-3 (A-CONSTANT (EVAL (LSH ART-FAT-STRING ARRAY-TYPE-SHIFT)))
                        XEQUAL-ARRAY-NOT-STRING)
XEQUAL-BOTH-STRINGSP-1
        (JUMP-EQUAL M-2 (A-CONSTANT (EVAL (LSH ART-STRING ARRAY-TYPE-SHIFT)))
                    XEQUAL-BOTH-STRINGSP-2)
        (JUMP-NOT-EQUAL M-2 (A-CONSTANT (EVAL (LSH ART-FAT-STRING ARRAY-TYPE-SHIFT)))
                        XEQUAL-ARRAY-NOT-STRING)
;; Both strings and rank 1.
XEQUAL-BOTH-STRINGSP-2
;Call STRING-EQUAL, which will check for arrays having same size and same elements.
        ((PDL-PUSH) A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
        ((A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON) A-V-TRUE)
XEQUAL-BOTH-STRINGSP-3
        ((PDL-PUSH) A-T)
        ((PDL-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) )
        ((PDL-PUSH) A-B)
        ((PDL-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-XCT-NEXT XSTRING-EQUAL) ;uses pdl pass-around
       ((PDL-PUSH) A-V-NIL)
        (POPJ-AFTER-NEXT
         (A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON) PDL-POP)
       (no-op)

XEQUAL-ARRAY-NOT-STRING
        (JUMP-NOT-EQUAL M-3 (A-CONSTANT (EVAL (LSH ART-1B ARRAY-TYPE-SHIFT)))
                        XFALSE)
        (JUMP-NOT-EQUAL M-2 (A-CONSTANT (EVAL (LSH ART-1B ARRAY-TYPE-SHIFT)))
                        XFALSE)
        (JUMP XEQUALP-ARRAY-HARD)       ;BOTH BITVEC, OK TO COMPARE WITH EQUALP MACROCODE.


;(%BLT from-address to-address n-words increment)
;Increment is usually 1, less often -1 for backwards blt.
XBLT (MISC-INST-ENTRY %BLT)
        ((M-D) Q-POINTER PDL-POP)
        ((M-C) Q-POINTER PDL-POP)
        ((M-2) Q-POINTER PDL-POP)
        ((M-1) Q-POINTER PDL-POP)
        ((M-2) SUB M-2 A-D)
        ((M-1) SUB M-1 A-D)
XBLT1   (JUMP-EQUAL M-C (A-CONSTANT 0) XFALSE)
        ((VMA-START-READ M-1) ADD M-1 A-D)
        (CHECK-PAGE-READ)
        ((VMA-START-WRITE M-2) ADD M-2 A-D)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-XCT-NEXT XBLT1)
       ((M-C) SUB M-C (A-CONSTANT 1))

XBLT-TYPED (MISC-INST-ENTRY %BLT-TYPED)
        ((M-D) Q-POINTER PDL-POP)
        ((M-C) Q-POINTER PDL-POP)
        ((M-2) Q-POINTER PDL-POP)
        ((M-1) Q-POINTER PDL-POP)
        ((M-2) SUB M-2 A-D)
        ((M-1) SUB M-1 A-D)
XBLT-TYPED-1
        (JUMP-EQUAL M-C (A-CONSTANT 0) XFALSE)
        ((VMA-START-READ M-1) ADD M-1 A-D)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-SCAV MD)
        ((VMA-START-WRITE M-2) ADD M-2 A-D)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (JUMP-XCT-NEXT XBLT-TYPED-1)
       ((M-C) SUB M-C (A-CONSTANT 1))

;(%BLT-BOOLE ALU from-address to-address n-words increment)
;  if from-address and/or to-address are DTP-ARRAY-POINTER, the data origin of the array
;  is substituted, otherwise, from-address and to-address are raw addresses.
;Increment is usually 1, less often -1 for backwards blt.
;Value is a fixnum between 0 and 3.  Bit 0 is set if IOR of all data is 0,
;  BIT 1 is set if AND of all data is -1.
XBLT-BOOLE (MISC-INST-ENTRY %BLT-BOOLE)
        ((M-R) Q-POINTER PDL-POP)  ;increment
        ((M-C) Q-POINTER PDL-POP)  ;n-words
        (call-data-type-equal pdl-top (a-constant (byte-value q-data-type dtp-array-pointer))
                address-array-data)  ;replaces array pointer on stack with its data origin.
                        ;clobbers M-A, M-B, M-D, M-E, M-S
        ((M-2) Q-POINTER PDL-POP)  ;to address
        (call-data-type-equal pdl-top (a-constant (byte-value q-data-type dtp-array-pointer))
                address-array-data)
        ((M-1) Q-POINTER PDL-POP)  ;from address
        ((A-ALUF) DPB PDL-POP OAL-ALUF A-ZERO)
        ((M-2) SUB M-2 A-R)
        ((M-1) SUB M-1 A-R)
        ((M-3) SETZ)            ;GETS IOR OF ALL DATA
        ((M-4) SETO)            ;GETS AND OF ALL DATA
XBLT-BOOLE-1
        (JUMP-EQUAL M-C (A-CONSTANT 0) XBLT-BOOLE-2)
        ((VMA-START-READ M-1) ADD M-1 A-R)
        (CHECK-PAGE-READ)
        ((M-J) MD)              ;should to moved to untyped when possible. ****
        ((VMA-START-READ M-2) ADD M-2 A-R)
        (CHECK-PAGE-READ)
        ((OA-REG-LOW) A-ALUF)
        ((WRITE-MEMORY-DATA-START-WRITE) SETZ MD A-J)
        (CHECK-PAGE-WRITE-unboxed)
        ((M-3) IOR MD A-3)
        ((M-4) AND MD A-4)
        (JUMP-XCT-NEXT XBLT-BOOLE-1)
       ((M-C) SUB M-C (A-CONSTANT 1))

XBLT-BOOLE-2
        ((m-j) setz)            ;flush garbage.
        ((M-T) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP-NOT-EQUAL M-3 A-ZERO XBLT-BOOLE-3)
        ((M-T) IOR M-T (A-CONSTANT 1))          ;IOR of all data is 0.
XBLT-BOOLE-3
        (POPJ-AFTER-NEXT POPJ-NOT-EQUAL M-4 (A-CONSTANT -1))
       ((M-T) IOR M-T (A-CONSTANT 2))           ;AND of all data is -1.

ADDRESS-ARRAY-DATA  ;REPLACE AN ARRAY POINTER ON TOP OF STACK WITH ITS DATA ORIGIN.
#+exp   ((vma) pdl-top)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA Q-DATA-TYPE PDL-TOP
            ARRAY-HEADER-SETUP-DISPATCH)
       ((m-a) seta a-minus-one c-pdl-buffer-pointer-pop)
        (popj-after-next
          (pdl-push) dpb m-array-origin q-pointer (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)


XNUMBP (MISC-INST-ENTRY NUMBERP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
XTNUMB  (DISPATCH-XCT-NEXT Q-DATA-TYPE M-T POPJ-IF-NOT-NUMBER)  ;MC-LINKAGE
       ((M-T) A-V-NIL)
        (JUMP XTRUE)

XFIXP (MISC-INST-ENTRY INTEGERP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
XTFIXP
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-fix)) xtrue)
        ((M-4) (A-CONSTANT (EVAL %HEADER-TYPE-BIGNUM)))
XFXFLP          ;false unless extended number of type in M-4
        (jump-data-type-not-equal m-t
                (a-constant (byte-value q-data-type dtp-extended-number)) xfalse)
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        ((M-T) A-V-TRUE)
        (DISPATCH TRANSPORT-HEADER MD)
        (POPJ-AFTER-NEXT (M-TEM) (LISP-BYTE %%HEADER-TYPE-FIELD) MD)
       (CALL-NOT-EQUAL M-TEM A-4 XFALSE)

XFLTP (MISC-INST-ENTRY FLOATP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
XTFLTP
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-small-flonum)) xtrue)
        (JUMP-XCT-NEXT XFXFLP)
       ((M-4) (A-CONSTANT (EVAL %HEADER-TYPE-FLONUM)))

XRATIONALP (MISC-INST-ENTRY RATIONALP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) XTRUE)
        (JUMP-DATA-TYPE-NOT-EQUAL M-T
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)) XFALSE)
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((M-TEM) (LISP-BYTE %%HEADER-TYPE-FIELD) MD)
        (JUMP-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-BIGNUM)) XTRUE)
        (JUMP-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-RATIONAL)) XTRUE)
        (JUMP XFALSE)

XRATIOP (MISC-INST-ENTRY RATIOP)
        (JUMP-XCT-NEXT XRATIOP1)
       ((M-4) (A-CONSTANT (EVAL %HEADER-TYPE-RATIONAL)))

XCOMPLEXP (MISC-INST-ENTRY COMPLEXP)
        ((M-4) (A-CONSTANT (EVAL %HEADER-TYPE-COMPLEX)))
XRATIOP1
        (JUMP-XCT-NEXT XFXFLP)
       ((M-T) Q-TYPED-POINTER PDL-POP)

XDATTP (MISC-INST-ENTRY %DATA-TYPE)
        (POPJ-AFTER-NEXT
         (M-T)  PDL-POP
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                Q-DATA-TYPE)
       (NO-OP)

XDAT   (MISC-INST-ENTRY %POINTER)
        (POPJ-AFTER-NEXT
         (M-T)  PDL-POP
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                Q-POINTER)
       (NO-OP)

XSDATP (MISC-INST-ENTRY %MAKE-POINTER)
        (POPJ-AFTER-NEXT
         (m-TEM1) Q-TYPED-POINTER PDL-POP)    ;ARG2, THE POINTER
       ((M-T) DPB PDL-POP Q-DATA-TYPE A-TEM1) ;ARG1, THE DATA TYPE

XSTND (MISC-INST-ENTRY %P-STORE-CONTENTS)
        ((M-T) Q-TYPED-POINTER PDL-POP) ;NEED IN M-T FOR RETURNED VALUE
     ;; Pointer from PDL has already been transported, or is a fixnum virtual address.
        ((VMA-START-READ) PDL-POP)
        (CHECK-PAGE-READ)
        ((MD-START-WRITE) SELECTIVE-DEPOSIT MD Q-ALL-BUT-TYPED-POINTER A-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-LDB-OFFSET PP M-C M-B)

XOPLDB(MISC-INST-ENTRY %P-LDB-OFFSET)
        (JUMP-XCT-NEXT XOPLD1)                  ;JOIN XLDB, BUT FIRST
       (CALL XOMR0)                             ;REFERENCE THE LOCATION

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %LOGLDB PP M-1)

XLLDB (MISC-INST-ENTRY %LOGLDB)                 ;LDB FOR FIXNUMS
        (JUMP-XCT-NEXT XLLDB1)
       ((M-1) Q-POINTER PDL-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-LDB PP VMA)

;%P-LDB treats target Q just as 32 bits.  Data type is not interpreted.
XPLDB (MISC-INST-ENTRY %P-LDB)
        ((VMA-START-READ) Q-TYPED-POINTER PDL-POP)
        (CHECK-PAGE-READ)                       ;VMA MAY POINT AT UNBOXED DATA.
XOPLD1  ((M-1) MD)              ;VMA MAY BE LEFT POINTING AT UNBOXED DATA..
XLLDB1  (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP) ;ARG1, BYTE POINTER.  MUST BE FIXNUM.
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
  (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP)    ;GET NUMBER OF BITS
        (JUMP-EQUAL M-K A-ZERO XLDB-ZERO)  ;WANT 0 BITS, RETURN 0
                                           ; (THIS IS A FAIRLY RANDOM THING TO CHECK FOR
                                           ; BUT IF WE DIDNT, IT WOULD CAUSE LOSSAGE)
        (CALL-GREATER-THAN M-K (A-CONSTANT Q-POINTER-WIDTH) TRAP)
    (ERROR-TABLE ARGTYP FIXNUM-FIELD PP 0)
        ((M-J) SUB M-K (A-CONSTANT 1))     ;BYTE LENGTH MINUS ONE FIELD
        ((M-E) (BYTE-FIELD 6 6) PDL-POP) ;GET NUMBER OF PLACES OVER
        ((m-TEM2) SUB (M-CONSTANT 40) A-E)        ;COMPENSATE FOR SHIFTER LOSSAGE
#+exp   ((m-tem3) add m-j (a-constant 1))
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 A-TEM2 OAL-BYTL-1)
       ((M-T) BYTE-INST
                M-1
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;LDB can only extract from fixnums and bignums.  The target is considered to
; have infinite sign extension.  LDB "should" always return a positive number.
; This issue currently doesn't arise, since LDB is implemented only for
; positive-fixnum-sized bytes, i.e. a maximum of 23. bits wide.  Note the
; presence of %LOGLDB, which will load a 24-bit byte of a fixnum and return
; it as a possibly-negative fixnum.
XLDB  (MISC-INST-ENTRY LDB) (ERROR-TABLE RESTART XLDB)
        (DISPATCH Q-DATA-TYPE PDL-TOP D-NUMARG)  ;Only the second operand is
            (ERROR-TABLE ARGTYP NUMBER PP 1 XLDB)   ;processed via NUMARG.  Thus LDB is
            (ERROR-TABLE ARG-POPPED 0 PP PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-LDB))          ;considered to be a one operand op.
                (ERROR-TABLE RESTART XLDB0)
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)   ;Arg1, byte pointer.  Must be fixnum.
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 0 XLDB0)
            (ERROR-TABLE ARG-POPPED 0 PP (FIXPACK M-1))
;Fixnum case.  Data to LDB out of (arg2) sign extended in M-1.
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP)    ;Get number of bits
        (JUMP-EQUAL M-K A-ZERO XLDB-ZERO)  ;Want 0 bits, return 0
                                           ; (This is a fairly random thing to check for
                                           ; but if we didnt, it would cause lossage)
        (CALL-GREATER-THAN M-K (A-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) TRAP)
    (ERROR-TABLE ARGTYP FIXNUM-FIELD PP 0 XLDB0)
    (ERROR-TABLE ARG-POPPED 0 PP (FIXPACK M-1))
        ((M-J) SUB M-K (A-CONSTANT 1))     ;Byte length minus one field
        ((M-E) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6)
                 PDL-POP) ;Get number of places over
        ((M-2) SUB (M-CONSTANT 40) A-K)    ;Maximum M-rotate to keep byte within a word
XLDB3   (JUMP-GREATER-THAN M-E A-2 XLDB2)  ;Jump if left edge of byte off end of word
        ((m-TEM2) SUB (M-CONSTANT 40) A-E) ;Compensate for shifter lossage
#+exp   ((m-tem3) add m-j (a-constant 1))
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 OAL-BYTL-1 A-TEM2)
       ((M-T) BYTE-INST M-1
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;Get here if left edge of byte is off 32. bit word.  Arithmetic shift right until it fits.
XLDB2   ((M-1) LDB (BYTE-FIELD 31. 1) M-1 A-1)
        (JUMP-XCT-NEXT XLDB3)
       ((M-E) SUB M-E (A-CONSTANT 1))

BIGNUM-LDB      ;M-Q has bignum, M-C has bignum header, M-I has length of bignum.
                (ERROR-TABLE RESTART BIGNUM-LDB)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)   ;Arg1, byte pointer.  Must be fixnum.
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 0 BIGNUM-LDB)
            (ERROR-TABLE ARG-POPPED 0 PP M-Q)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP)    ;Get number of bits
        (CALL-GREATER-THAN M-K (A-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) TRAP)
            (ERROR-TABLE ARGTYP FIXNUM-FIELD PP 0 BIGNUM-LDB)
            (ERROR-TABLE ARG-POPPED 0 PP M-Q)
        ((M-E) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6)
                         PDL-TOP)  ;Number of places over
        ((M-D) (A-CONSTANT 1))                  ;Offset within bignum
BIGLDB2 (JUMP-LESS-THAN M-E (A-CONSTANT 31.) BIGLDB1)  ;Found word desired byte starts in
        ((M-D) ADD M-D (A-CONSTANT 1))
        (JUMP-LESS-OR-EQUAL-XCT-NEXT M-D A-I BIGLDB2)
       ((M-E) SUB M-E (A-CONSTANT 31.))
#+lambda((OA-REG-HIGH) BIGNUM-HEADER-SIGN M-C)  ;Byte off top of bignum, return sign bits
#+exp   ((m-t) bignum-header-sign m-c)
#+exp   ((oa-reg-high) dpb m-t oah-m-src a-zero)
        ((M-T) M-ZERO)
        (JUMP PDL-POP BIGLDB6)  ;Truncate byte and return (also flush arg)

BIGLDB1 ((VMA-START-READ) ADD M-Q A-D)  ;Fetch word of bignum
        (CHECK-PAGE-READ)
        ((M-ZR) (A-CONSTANT 31.))       ;31. useful bits in bignum word.
        (CALL-XCT-NEXT I-LDB)           ;Get at least some of the right stuff into M-2
       ((M-1) MD)
        ((M-T) Q-POINTER M-2
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))) ;Force result into fixnum
        (JUMP-EQUAL M-4 A-K BIGLDB3)   ;and return it if that is entire byte
        (JUMP-EQUAL M-D A-I BIGLDB3)   ;Also return if that was last word of bignum
        ((VMA-START-READ) M+A+1 M-Q A-D)        ;Get next word of bignum
        (CHECK-PAGE-READ)
        ((M-J) M-A-1 M-K A-4)           ;Number of bits left to go minus one
#+exp   ((m-tem3) add m-j (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 OAL-BYTL-1 A-ZERO)
        ((M-1) BYTE-INST MD A-ZERO)  ;Get bits from second word
                                   ;Put those bits above the previous bits.
        ((OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 OAL-BYTL-1 A-4)
        ((M-T) DPB M-1 A-T)
BIGLDB3 (POPJ-IF-BIT-CLEAR-XCT-NEXT BIGNUM-HEADER-SIGN M-C)     ;Done if bignum was positive
       ((M-E) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6)    ;Retrieve byte pos, flush arg from pdl
                         PDL-POP)
        ;; Bignum was negative.  Take complement of the byte value retrieved.
        ;; This is a 1's or 2's complement depending on whether all bits to the
        ;; right are zero.  M-K still has the byte size.
        ((M-T) XOR M-T (A-CONSTANT -1)) ;1's complement the byte and some extra bits to left
        ((VMA) M-Q)                     ;Scan the bignum for zeros, until start of the byte
BIGLDB4 (JUMP-LESS-OR-EQUAL M-E A-ZERO BIGLDB7)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (JUMP-LESS-THAN M-E (A-CONSTANT 31.) BIGLDB5)
        (JUMP-EQUAL-XCT-NEXT MD A-ZERO BIGLDB4)
       ((M-E) SUB M-E (A-CONSTANT 31.))
BIGLDB6 ((M-K) SUB M-K (A-CONSTANT 1))  ;OK, truncate the byte value and return it
#+exp   ((m-tem3) add m-k (a-constant 1))
        (POPJ-AFTER-NEXT (OA-REG-LOW) DPB #+lambda M-K #+exp m-tem3 OAL-BYTL-1 A-ZERO)
       ((M-T) (BYTE-FIELD 0 0) M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

BIGLDB5 ((M-E) SUB M-E (A-CONSTANT 1))  ;Check bits in last word
#+exp   ((m-tem3) add m-e (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-E #+exp m-tem3 OAL-BYTL-1 A-ZERO)
        ((M-TEM) (BYTE-FIELD 0 0) MD)
        (JUMP-NOT-EQUAL M-TEM A-ZERO BIGLDB6)
BIGLDB7 (JUMP-XCT-NEXT BIGLDB6)         ;2's complement
       ((M-T) ADD M-T (A-CONSTANT 1))

XLSH-ZERO
XLDB-ZERO
        (POPJ-AFTER-NEXT
         (M-T) SETA (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) ;RESULT = 0
                    PDL-POP)    ;DON'T FORGET TO POP ARG1
       (NO-OP)

;INTERNAL LDB.  TAKES DATA IN M-1, BITS IN M-K, PLACES OVER IN M-E.
; SIZE OF DATA IN M-1 IN M-ZR (MAX 32.).
; RETURNS BYTE IN M-2.  M-4 GETS NUMBER OF BITS OF M-2 THAT ACTUALLY
; CONTAIN DESIRED BYTE, IE, SAME AS M-K IF ENTIRE BYTE WAS WITHIN M-ZR BITS,
; OTHERWISE ONE LESS FOR EACH BIT BYTE EXTENDED BEYOND M-ZR BITS, OR ZERO IF
; BYTE WAS ENTIRELY TO THE LEFT OF M-ZR BITS.  REST OF M-2 IS ZERO.
I-LDB   ((M-2) ADD M-K A-E)
        (JUMP-GREATER-THAN M-2 A-ZR I-LDB0)     ;LEFT EDGE OF BYTE OFF TOP
        ((M-4) M-K)                             ;ENTIRE BYTE WILL FIT.
I-LDB2  (POPJ-EQUAL-XCT-NEXT M-4 A-ZERO)
       ((M-2) A-ZERO)                           ;RETURN 0 FOR 0 LENGTH BYTE.
        ((m-TEM2) SUB (M-CONSTANT 40) A-E)
        ((M-TEM) SUB M-4 (A-CONSTANT 1))        ;HARDWARE BYTE LENGTH IS REAL VALUE -1.
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-TEM #+exp m-4 OAL-BYTL-1 A-TEM2)
       ((M-2) BYTE-INST M-1 A-ZERO)

I-LDB0  ((M-2) SUB M-2 A-ZR)                    ;NUMBER OF BITS OFF TOP
        (JUMP-LESS-THAN-XCT-NEXT M-E A-ZR I-LDB2) ;JUMP IF ANY BITS OF BYTE IN THIS WORD
       ((M-4) SUB M-K A-2)                      ;REDUCE SIZE OF BYTE TO AS MUCH AS WILL FIT
        (POPJ-AFTER-NEXT (M-4) A-ZERO)          ;BYTE NOT IN THIS WORD, RETURN 0 BITS
       ((M-2) A-ZERO)

;INTERNAL DPB. TAKES DATA TO DEPOSIT IN M-1, DATA TO DEPOSIT INTO IN M-2,
; SIZE OF M-2 (MAX 32.) IN M-ZR.  BITS IN M-K, PLACES OVER IN M-E.
; RESULT IN M-2.  M-K REDUCED BY BITS THAT WERE DEPOSITED (IE WILL BE ZERO IF
; ENTIRE BYTE FIT).  IF BYTE DID NOT COMPLETELY FIT, M-1 IS SHIFTED RIGHT BY
; AMOUNT THAT DID FIT.  SMASHES M-4, TEMPS
I-DPB   (POPJ-EQUAL M-K A-ZERO)
        ((M-4) ADD M-K A-E)
        (JUMP-GREATER-THAN-XCT-NEXT M-4 A-ZR I-DPB0)    ;JUMP IF LEFT EDGE OF BYTE OFF TOP
       ((M-TEM) SUB M-K (A-CONSTANT 1))
        ((M-K) A-ZERO)                          ;NONE LEFT TO DO, WHOLE BYTE IN THIS WORD
#+exp   ((m-tem3) add m-tem (a-constant 1))
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-TEM #+exp m-tem3 OAL-BYTL-1 A-E)
       ((M-2) DPB M-1 A-2)

I-DPB0  (POPJ-GREATER-OR-EQUAL M-E A-ZR)        ;RETURN IF ENTIRE BYTE OFF TO LEFT
        ((M-K) SUB M-4 A-ZR)                    ;M-K GETS NUMBER OF BITS LEFT OVER
        ((M-TEM) SUB M-TEM A-K)                 ;REDUCE SIZE OF BYTE
#+exp   ((m-tem3) add m-tem (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-TEM #+exp m-tem3 OAL-BYTL-1 A-E)
        ((M-2) DPB M-1 A-2)                     ;DO THE DPB
        ((m-TEM2) M-A-1 (M-CONSTANT 40) A-TEM)  ;SHIFT OVER TO USE UP WHATS BEEN DPB'ED
#+exp   ((m-tem3) add m-k (a-constant 1))
        (POPJ-AFTER-NEXT                        ;FACT BYTE SIZE IS +1 DOESNT HURT,
         (OA-REG-LOW) DPB #+lambda M-K #+exp m-tem3 OAL-BYTL-1 A-TEM2)  ; SINCE M-1 WASN'T 32 BITS
       ((M-1) BYTE-INST M-1 A-ZERO)             ;RIGHT ADJUST BITS IN M-1 FOR NEXT TIME.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-DPB-OFFSET PP PP M-C M-B)

XOPDPB(MISC-INST-ENTRY %P-DPB-OFFSET)
        (JUMP-XCT-NEXT XOPDP1)                  ;JOIN XDPB, BUT FIRST
       (CALL XOMR0)                             ;REFERENCE THE DATA AND SET VMA

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %LOGDPB M-1 (+ (LSH M-E 6) M-K) M-2)

XLDPB (MISC-INST-ENTRY %LOGDPB)    ;DPB FOR FIXNUMS ONLY, CAN STORE INTO SIGN BIT
        ((M-2) Q-TYPED-POINTER PDL-POP)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP)
        ((M-E) (BYTE-FIELD 6 6) PDL-POP)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((M-1) PDL-POP)
        (CALL-XCT-NEXT I-DPB)     ;SEMI-RANDOM TO USE THIS ROUTINE, BUT SPEED DOESNT
       ((M-ZR) (A-CONSTANT Q-POINTER-WIDTH))  ; MATTER AND IT SAVES A UINST OR TWO.
        (POPJ-AFTER-NEXT
          (M-T) M-2)    ;if DPBing into character, return character DTP.
       (NO-OP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-DPB PP PP VMA)

XPDPB (MISC-INST-ENTRY %P-DPB)
        ((VMA-START-READ) PDL-POP)
        (CHECK-PAGE-READ)       ;VMA MAY POINT TO UNBOXED DATA
XOPDP1  ((M-1) MD)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)            ;ARG2, BYTE POINTER
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
   (ERROR-TABLE ARGTYP FIXNUM PP 1)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP) ;GET NUMBER OF BITS
        (JUMP-EQUAL M-K A-ZERO XDPB-ZERO)
        ((M-K) SUB M-K (A-CONSTANT 1))
        ((m-TEM1) (BYTE-FIELD 6 6) PDL-POP) ;GET NUMBER OF PLACES OVER
#+exp   ((m-tem3) add m-k (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-K #+exp m-tem3 A-TEM1 OAL-BYTL-1)
        ((MD-START-WRITE)       ;VMA CAN BE LEFT POINTING AT UNBOXED DATA
                DPB PDL-POP A-1)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP XFALSE)

; DPB never changes the sign of quantity DPB'ed into, it extends
; the sign arbitrarily far to the left past the byte.
XDPB (MISC-INST-ENTRY DPB) (ERROR-TABLE RESTART XDPB)
        ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER (A-CONSTANT 2))      ;ADDRESS ARG1
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-INDEX TRAP-UNLESS-FIXNUM)    ;MAKE SURE NOT BIGNUM
    (ERROR-TABLE ARGTYP FIXNUM (PP -2) 0 XDPB)
    (ERROR-TABLE ARG-POPPED 0 PP PP PP)
        ((M-ZR) Q-DATA-TYPE PDL-TOP)            ;save data type for xdpb1 (character or not).
        (DISPATCH Q-DATA-TYPE PDL-TOP D-NUMARG) ;ONLY THE THIRD OPERAND IS
            (ERROR-TABLE ARGTYP NUMBER PP T XDPB)  ;PROCESSED VIA NUMARG. THUS DPB IS A
            (ERROR-TABLE ARG-POPPED 0 PP PP PP)
       ((M-A) (A-CONSTANT ARITH-1ARG-DPB))         ;ONE OPERAND OP.
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
;FIXNUM CASE.  DATA TO DPB INTO (ARG3) SIGN EXTENDED IN M-1.
                (ERROR-TABLE RESTART XDPB0)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)            ;ARG2, BYTE POINTER
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 1 XDPB0)
    (ERROR-TABLE ARG-POPPED 0 PP PP M-1)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP) ;GET NUMBER OF BITS
        (JUMP-EQUAL M-K A-ZERO XDPB-ZERO)
        (CALL-GREATER-THAN M-K (A-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) TRAP)
    (ERROR-TABLE ARGTYP FIXNUM-FIELD PP 0 XDPB0)
    (ERROR-TABLE ARG-POPPED 0 PP PP M-1)
        ((M-E) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6)
                        PDL-POP) ;GET NUMBER OF PLACES OVER
ASHDPB  ((M-2) ADD M-K A-E)                     ;M-2 maximum number of bits in result
        (JUMP-GREATER-THAN M-2 (A-CONSTANT 32.) XDPB2A) ;Multi-word => use bignum code
        (JUMP-LESS-THAN-XCT-NEXT M-1 A-ZERO ASHDPB-NEG)
       ((M-J) SUB M-K (A-CONSTANT 1))           ;Single-word => use hardware DPB
#+exp   ((m-tem3) add m-j (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 OAL-BYTL-1 A-E)
        ((M-1) DPB PDL-POP A-1)
        (JUMP-GREATER-OR-EQUAL M-1 A-ZERO XDPB1) ;Result in M-1 if sign didn't change
        ((M-C) A-ZERO)                          ;Else it's a 2-word bignum
        (JUMP-XCT-NEXT OVERFLOW-BIGNUM-CREATE)
       ((M-2) A-ZERO)

;; Here if DPB into fixnum or character fits in pointer field.
;; Return a character if the arg was a character.
XDPB1   (JUMP-NOT-EQUAL M-ZR (A-CONSTANT (EVAL DTP-CHARACTER)) RETURN-M-1)
        (POPJ-XCT-NEXT)
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)))

ASHDPB-NEG                                      ;Single-word DPB into negative number
#+exp   ((m-tem3) add m-j (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 OAL-BYTL-1 A-E)
        ((M-1) DPB PDL-POP A-1)
        (JUMP-LESS-THAN M-1 A-ZERO XDPB1)       ;Result in M-1 if sign didn't change
        ((M-1) SUB M-ZERO A-1)                  ;Else it's a 2-word bignum
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-ZERO OVERFLOW-BIGNUM-CREATE-NEGATIVE)
       ((M-2) A-ZERO)
        (JUMP-XCT-NEXT OVERFLOW-BIGNUM-CREATE-NEGATIVE)
       ((M-2) (A-CONSTANT 1))

;Get here on DPB ing into fixnum at position beyond 31. bits.  Fake up bignum
; and fall into bignum case.  Hair is that it avoids creating a
; garbage bignum just to copy out of.
XDPB2A  (CALL-LESS-THAN-XCT-NEXT M-1 A-ZERO XDPB-BM)   ;MAGNITUDIFY M-1 AND SAVE SIGN
       ((M-C) A-ZERO)               ;IN BIGNUM-HEADER-SIGN POSITION.
ASHDPB1 ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC BIGDPB3)))
ASHDPB2 ((M-J) DPB M-E (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6) A-K)
        ((PDL-PUSH) DPB M-J Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ;PUSH ARG2 BACK
        ((M-D) DPB M-1 Q-POINTER    ;SUBROUTINE SMASHES M-1
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ;THIS IS NOW ALWAYS A POSITIVE
        (CALL-XCT-NEXT DPB-BIGNUM-SETUP)                        ; NUMBER EVEN IF IT IS SETZ
       ((M-I) A-ZERO)   ;INDICATE SPECIAL CASE TO BIGNUM-COPY-EXPAND. HEADER SIGN IN M-C.
        ((MD) Q-POINTER M-D)
        ((VMA-START-WRITE) ADD M-T (A-CONSTANT 1))  ;STORE AWAY SAVED PIECE, CREATING
        (CHECK-PAGE-WRITE-UNBOXED)                          ;BIGNUM TO SMASH
;Smashable bignum in M-T, header in M-C.  Length in M-I has been smashed.
BIGDPB0 ((M-I) BIGNUM-HEADER-LENGTH M-C)        ;NEW LENGTH
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP)  ;NUMBER-OF-BITS
        (CALL-GREATER-THAN M-K (A-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) TRAP)
            (ERROR-TABLE ARGTYP FIXNUM-FIELD PP 0)
            (ERROR-TABLE ARG-POPPED PP PP M-T)
        (CALL-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BIGNEG) ;GET 2'S COMPLEMENT REPRESENTATION
        ((M-E) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH (PLUS 1 6)) 6)
                PDL-POP)  ;NUMBER OF PLACES OVER
        ((M-1) Q-POINTER PDL-POP)       ;DATA TO DEPOSIT.
        ((M-D) (A-CONSTANT 1))          ;OFFSET WITHIN BIGNUM
BIGDPB2 (JUMP-LESS-THAN M-E (A-CONSTANT 31.) BIGDPB1)
        ((M-D) ADD M-D (A-CONSTANT 1))          ;BYTE DOES NOT START IN THIS WORD
        (JUMP-LESS-OR-EQUAL-XCT-NEXT M-D A-I BIGDPB2)
       ((M-E) SUB M-E (A-CONSTANT 31.))
        (CALL TRAP)
           (ERROR-TABLE BIGNUM-NOT-BIG-ENOUGH-DPB)      ;SHOULDN'T HAPPEN

BIGDPB1 ((VMA-START-READ) ADD M-T A-D)    ;FETCH WORD OF BIGNUM
        (CHECK-PAGE-READ)
        ((M-ZR) (A-CONSTANT 31.))
        (CALL-XCT-NEXT I-DPB)           ;DEPOSIT IN SOME
       ((M-2) MD)
        ((MD-START-WRITE) M-2)          ;WRITE THAT WORD BACK.
        (CHECK-PAGE-WRITE-UNBOXED)
        (POPJ-EQUAL M-K A-ZERO)                 ;NO BITS LEFT TO DEPOSIT
        ((VMA-START-READ) ADD M-T A-D ALU-CARRY-IN-ONE)
        (CHECK-PAGE-READ)
        ((M-E) A-ZERO)
        (CALL-XCT-NEXT I-DPB)           ;DEPOSIT THE REST OF THE BITS.
       ((M-2) MD)
        (POPJ-AFTER-NEXT (MD-START-WRITE) M-2)
       (CHECK-PAGE-WRITE-UNBOXED)

XDPB-BM (POPJ-AFTER-NEXT   ;MAKING NEGATIVE NUMBER.  MAGNITUDIFY AND SET BIGNUM SIGN BIT.
         (M-1) SUB M-ZERO A-1)
       ((M-C) DPB M-MINUS-ONE BIGNUM-HEADER-SIGN A-ZERO)

;Bignum in M-T, length in M-I.  Take 2's complement of it.  Bashes M-3, M-4
BIGNEG  ((M-3) (A-CONSTANT 1))          ;Offset into bignum
        ((M-4) (A-CONSTANT 0))          ;0 if borrow, -1 if no borrow
BIGNEG1 ((VMA-START-READ) ADD M-T A-3)
        (CHECK-PAGE-READ)
        ((M-3) ADD M-3 (A-CONSTANT 1))
        ((M-TEM) MD)
        (JUMP-EQUAL-XCT-NEXT MD A-ZERO BIGNEG2)
       ((M-TEM) SUB M-4 A-TEM)
        ((M-4) (A-CONSTANT -1))         ;No more borrow
BIGNEG2 ((MD-START-WRITE) (BYTE-FIELD 31. 0) M-TEM)     ;Make sure high bit stays clear
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP-LESS-OR-EQUAL M-3 A-I BIGNEG1)
        (POPJ)

BIGNUM-DPB  ;bignum in M-Q, header in M-C, length in M-I.
        (CALL DPB-BIGNUM-SETUP)
        (CALL BIGDPB0)
BIGDPB3 (CALL-IF-BIT-SET BIGNUM-HEADER-SIGN M-C BIGNEG) ;If was negated, put in sign-magn form
        (JUMP BIGNUM-DPB-CLEANUP)       ;bignum in M-T, header in M-C, length in M-I.

XDPB-ZERO
        ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (POPJ-AFTER-NEXT                        ;RESULT IS ARG3
            (M-GARBAGE) PDL-POP)
       ((M-GARBAGE) PDL-POP)    ;AND POP OTHER TWO ARGS

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-MASK-FIELD-OFFSET PP M-C M-B)

XOPMF (MISC-INST-ENTRY %P-MASK-FIELD-OFFSET)
        (JUMP-XCT-NEXT XOPMF1)                  ;JOIN XMF, BUT FIRST
       (CALL XOMR0)                             ;REFERENCE THE LOCATION

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-MASK-FIELD PP VMA)

XPMF  (MISC-INST-ENTRY %P-MASK-FIELD)
        ((VMA-START-READ) Q-TYPED-POINTER PDL-POP)
        (CHECK-PAGE-READ)
XOPMF1  (JUMP-XCT-NEXT XPFM1)
       ((M-1) MD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS MASK-FIELD PP M-1)

XMF   (MISC-INST-ENTRY MASK-FIELD)      ;LIKE LDB BUT DATA IN ORIGINAL POSITION IN Q
        ((M-1) Q-TYPED-POINTER PDL-POP) ;DATA TO EXTRACT
XPFM1   (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP) ;ARG1, BYTE POINTER.  MUST BE FIXNUM.
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
  (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP)    ;GET NUMBER OF BITS
        (JUMP-EQUAL M-K A-ZERO XLDB-ZERO)  ;WANT 0 BITS, RETURN 0
                                           ; (THIS IS A FAIRLY RANDOM THING TO CHECK FOR
                                           ; BUT IF WE DIDNT, IT WOULD CAUSE LOSSAGE)
        ((M-J) SUB M-K (A-CONSTANT 1))     ;BECAUSE BITS IN LDB IS +1
        ((m-TEM2) (BYTE-FIELD 6 6) PDL-POP) ;GET NUMBER OF PLACES OVER
#+exp   ((m-tem3) add m-j (a-constant 1))
        (POPJ-AFTER-NEXT                   ;NO "SHIFTER LOSSAGE" ON SELECTIVE-DEPOSIT
         (OA-REG-LOW) DPB #+lambda M-J #+exp m-tem3 A-TEM2 OAL-BYTL-1)
       ((M-T) SELECTIVE-DEPOSIT
                M-1
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-DEPOSIT-FIELD-OFFSET PP PP M-C M-B)

XOPDF(MISC-INST-ENTRY %P-DEPOSIT-FIELD-OFFSET)
        (JUMP-XCT-NEXT XOPDF1)                  ;JOIN XDF, BUT FIRST
       (CALL XOMR0)                             ;REFERENCE THE LOCATION AND SET VMA

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-DEPOSIT-FIELD PP PP VMA)

XPDF (MISC-INST-ENTRY %P-DEPOSIT-FIELD)
        ((VMA-START-READ) Q-TYPED-POINTER PDL-POP)
        (CHECK-PAGE-READ)
XOPDF1  (CALL-XCT-NEXT XPDF1)
       ((m-TEM3) MD)
        ((MD-START-WRITE) M-T)
        (CHECK-PAGE-WRITE-UNBOXED)
        (JUMP XFALSE)

;This can return untyped data.  It also doesn't work on bignums.
;Fortunately no one has ever called it.
XDF  (MISC-INST-ENTRY DEPOSIT-FIELD)
        ((m-TEM3) Q-TYPED-POINTER PDL-POP) ;ARG3, DATA TO STORE IN
XPDF1   (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)            ;ARG2, BYTE POINTER
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
  (ERROR-TABLE ARGTYP FIXNUM PP 1)
        ((M-K) (BYTE-FIELD 6 0) PDL-TOP) ;GET NUMBER OF BITS
        (JUMP-EQUAL M-K A-ZERO XDPB-ZERO)
        ((M-K) SUB M-K (A-CONSTANT 1))
        ((m-TEM1) (BYTE-FIELD 6 6) PDL-POP) ;GET NUMBER OF PLACES OVER
#+exp   ((m-tem3) add m-k (a-constant 1))
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) DPB #+lambda M-K #+exp m-tem3 A-TEM1 OAL-BYTL-1)
       ((M-T) SELECTIVE-DEPOSIT PDL-POP A-TEM3)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %P-STORE-TAG-AND-POINTER PP M-A)

XCMBS (MISC-INST-ENTRY %P-STORE-TAG-AND-POINTER)
        ((M-A) Q-TYPED-POINTER PDL-POP) ;ARG3, VALUE FOR POINTER FIELD
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)         ;ARG3 ANY TYPE, MISCBITS MUST BE FIXNUM
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 2)
        ((MD) DPB PDL-POP ;ARG2, VALUE FOR TYPE, ETC.
                Q-ALL-BUT-POINTER A-A)
        ((VMA-START-WRITE) PDL-POP) ;ARG1, WHERE TO STORE
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (JUMP XFALSE)

xps-tag-and-pointer  ;This can be used to gronk ONE-Q-FORWARDs, etc.
     (misc-inst-entry %p-store-data-type-and-pointer)
        ((m-a) q-typed-pointer pdl-pop)     ;ARG3, value for pointer field
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)         ;ARG2, data type
                        Q-DATA-TYPE PDL-TOP TRAP-UNLESS-FIXNUM)
        ((m-t) q-typed-pointer pdl-pop)
        ((vma-start-read) pdl-pop)      ;arg1, where to store.
        (check-page-read)
        (dispatch transport-cdr md)     ;read CDR-CODE from newspace.
        ((m-tem) md)
        ((m-tem) dpb m-a q-pointer a-tem)
        ((md-start-write) dpb m-t q-data-type a-tem)
        (check-page-write)
        (gc-write-test)
        (popj)          ;return data-type, which is safe.

XPDAT (MISC-INST-ENTRY %P-POINTER)
        ((M-1) (A-CONSTANT (OA-LOW-CONTEXT (BYTE-INST Q-POINTER))))
XPDAT1  ((VMA-START-READ) PDL-POP)
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT
         (OA-REG-LOW) M-1)
       ((M-T) BYTE-INST MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XPDATP (MISC-INST-ENTRY %P-DATA-TYPE)
        (JUMP-XCT-NEXT XPDAT1)
       ((M-1) (A-CONSTANT (OA-LOW-CONTEXT (BYTE-INST Q-DATA-TYPE))))

XPCDRC (MISC-INST-ENTRY %P-CDR-CODE)
        (JUMP-XCT-NEXT XPDAT1)
       ((M-1) (A-CONSTANT (OA-LOW-CONTEXT (BYTE-INST Q-CDR-CODE))))

XSPDTP (MISC-INST-ENTRY %P-STORE-DATA-TYPE)
        ((M-1) (A-CONSTANT (OA-LOW-CONTEXT (DPB Q-DATA-TYPE))))
XSPDTP1 ((M-T) Q-TYPED-POINTER PDL-POP) ;DATA TO DPB IN (ALSO RETURN AS VALUE)
        ((VMA-START-READ) Q-TYPED-POINTER PDL-POP)
        (CHECK-PAGE-READ)
        ((m-TEM2) MD)
        ((OA-REG-LOW) M-1)
        ((MD-START-WRITE) DPB M-T A-TEM2)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj)

XSPDAT (MISC-INST-ENTRY %P-STORE-POINTER)
        (JUMP-XCT-NEXT XSPDTP1)
       ((M-1) (A-CONSTANT (OA-LOW-CONTEXT (DPB Q-POINTER))))

XSPCDR (MISC-INST-ENTRY %P-STORE-CDR-CODE)
        (JUMP-XCT-NEXT XSPDTP1)
       ((M-1) (A-CONSTANT (OA-LOW-CONTEXT (DPB Q-CDR-CODE))))

;Provides a way to pick up the pointer-field of an external-value-cell
;pointer or a dtp-null pointer, or any invisible pointer,
;converting it into a locative and transporting it if it points to old-space.
XPCAL (MISC-INST-ENTRY %P-CONTENTS-AS-LOCATIVE)
        ((VMA-START-READ) Q-TYPED-POINTER PDL-POP)      ;GET SPECD LOCATION
        (CHECK-PAGE-READ)
XPCAL1  (CALL-XCT-NEXT TRANS-OLD0)                      ;TRANSPORT OLDSPACE POINTER, BUT
       ((M-1) MD)                                       ; DON'T CHASE INVISIBLE POINTERS
        ((M-T) Q-POINTER MD
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
        (POPJ-EQUAL MD A-1)                     ;REPEAT IF E.G. SNAPPED OUT HDR-FWD
        (jump xpcal1)

XPCALO (MISC-INST-ENTRY %P-CONTENTS-AS-LOCATIVE-OFFSET)
        (JUMP-XCT-NEXT XPCAL1)
       (CALL XOMR0)                                     ;GET SPECD LOCATION

XPDIF (MISC-INST-ENTRY %POINTER-DIFFERENCE)
        ((M-T) Q-POINTER PDL-POP)
        (POPJ-AFTER-NEXT
          (M-T) SUB PDL-POP A-T)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

xpless (misc-inst-entry %pointer-lessp)
        ((m-1) q-pointer pdl-pop)
        ((m-2) q-pointer pdl-pop)
        (jump-less-than m-2 a-1 xtrue)
        (jump xfalse)

xpgreater (misc-inst-entry %pointer-greaterp)
        ((m-1) q-pointer pdl-pop)
        ((m-2) q-pointer pdl-pop)
        (jump-greater-than m-2 a-1 xtrue)
        (jump xfalse)

XOMR (MISC-INST-ENTRY %P-CONTENTS-OFFSET)
        (CALL XOMR0)                            ;READ THE SPECIFIED LOCATION
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD)
       ((M-T) Q-TYPED-POINTER MD)       ;RETURN ITS CONTENTS

XOMR0   ((M-B) PDL-POP) ;GET THE OFFSET
        ((VMA-START-READ M-C) PDL-POP)  ;READ THE HEADER WORD
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)  ;FOLLOW FORWARDING PTR
        (POPJ-AFTER-NEXT
         (VMA-START-READ) ADD VMA A-B)          ;NOW REFERENCE THE SPECIFIED LOCATION
       (CHECK-PAGE-READ)                        ;VMA COULD BE POINTING INTO UNFORWARDED DATA

XOMS  (MISC-INST-ENTRY %P-STORE-CONTENTS-OFFSET)
        (CALL XOMR0)                            ;READ THE SPECIFIED LOCATION, SET VMA
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((MD-START-WRITE) SELECTIVE-DEPOSIT MD
                Q-ALL-BUT-TYPED-POINTER A-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj)

;%MAKE-POINTER-OFFSET <new data type> <pointer> <offset> returns a pointer whose pointer
;   is (+ (%POINTER <pointer>) <offset>) and whose data type is <new data type>.  No data
;   type checks.
XMOP (MISC-INST-ENTRY %MAKE-POINTER-OFFSET)
     ;Can this make pointers to oldspace?
     ;Well, if it is indexing into a structure, it can't because the original structure
     ;must be in newspace.  If it is indexing into a cdr-coded list (questionable code)
     ;I think it will work because the list is transported all at once.
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-T) ADD PDL-POP A-T)
        (popj-after-next (M-T) DPB Q-DATA-TYPE PDL-POP A-T)
       ((m-t) q-typed-pointer m-t)      ;avoid garbage in cdr-code.


XSFP (MISC-INST-ENTRY %STACK-FRAME-POINTER)
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) M-AP)
        (POPJ-AFTER-NEXT (M-T) M-K)
       (NO-OP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS INTERNAL-GET-3 M-B M-D M-E)

XINTERNAL-GET-3 (MISC-INST-ENTRY INTERNAL-GET-3)
        ((M-E) Q-TYPED-POINTER PDL-POP)      ;Arg3, default value.
        ((M-D) Q-TYPED-POINTER PDL-POP)      ;Arg2, property name.
        ((M-T) Q-TYPED-POINTER PDL-POP)      ;Arg1, symbol or plist.
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-instance)) xget3)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-array-pointer)) xget2)
xget3   ((ARG-CALL INSTANCE-INVOKE-1) (I-ARG INSTANCE-INVOKE-GET))
        ((PDL-PUSH) DPB M-D Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((PDL-PUSH) DPB M-E Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-CALL MMCALL) (I-ARG 3))   ;Call, 3 args.  Value comes back in M-T.
        (POPJ)

XGET2 ;; The following instruction can popj out of XGET.
        ((ARG-CALL-XCT-NEXT PLGET) (I-ARG INSTANCE-INVOKE-GET))
       ((M-B) M-T)              ;Save copy of arg in M-B.
XGET1   (JUMP-EQUAL M-T A-V-NIL XGET-NOT-FOUND)
XGET1A
        (open-carcdr m-t)
;; If the car matches desired property,
;; return the car of the following link (now in M-T).
        (JUMP-EQUAL M-A A-D QCAR)
        (open-qcdr m-t)
        (JUMP-NOT-EQUAL M-T A-V-NIL XGET1A)
XGET-NOT-FOUND
        (POPJ-AFTER-NEXT (M-T) M-E)
       (NO-OP)

xget (misc-inst-entry internal-get-2)
        ((m-d) q-typed-pointer c-pdl-buffer-pointer-pop)
        (call-xct-next xget-property-list)
       ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (popj-equal m-t a-v-nil)
xget0
#+lambda(dispatch-xct-next dispatch-write-vma q-data-type m-t carcdr-dispatch-direct)
#+exp   (open-carcdr-xct-next m-t)
       (no-op)
        (jump-equal m-a a-d qcar)
#+lambda(dispatch-xct-next dispatch-write-vma (i-arg instance-invoke-cdr)
            q-data-type m-t cdr-pre-dispatch-direct)
#+exp   (open-qcdr-xct-next m-t)
       (no-op)
        (jump-xct-next xget0)
       (popj-equal m-t a-v-nil)

xget-property-list
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-symbol))
            xget-property-list-hard)
        ((vma-start-read) add m-t (a-constant 3))
        (check-page-read)
        (popj-after-next dispatch transport md)
       ((m-t) q-typed-pointer md)

xget-property-list-hard
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)) qcdr)
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-locative)) qcdr)
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-instance))
            xgeth-i)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-array-pointer))
            xfalse)
xgeth-i ((arg-jump-xct-next instance-invoke-pl) (i-arg instance-invoke-get))
       ((m-a) m-d)


;(DEFUN GET-LEXICAL-VALUE-CELL (X Y) (GET-LOCATION-OR-NIL (LOCF X) Y))
;except it runs much faster when X is a list that lives inside the pdl buffer.
;does not try to work on weird things.
XGETLV (MISC-INST-ENTRY GET-LEXICAL-VALUE-CELL)
        ((M-D) Q-TYPED-POINTER PDL-POP) ;Arg2, cell locative to search for.
        ((M-T) Q-TYPED-POINTER PDL-POP) ;Arg1, plist contents.
        ((M-2) Q-POINTER M-2)
        ((PDL-INDEX M-2) SUB M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS)
        (JUMP-NOT-EQUAL PDL-INDEX M-2 XGETI1)
        ((PDL-INDEX) ADD PDL-INDEX A-PDL-BUFFER-HEAD)
XGETLV1 ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
        (JUMP-EQUAL M-A A-D XGETI2)
        ((M-1) Q-CDR-CODE C-PDL-BUFFER-INDEX)
        (JUMP-EQUAL M-1 (A-CONSTANT (EVAL CDR-NIL)) XFALSE)
        ((PDL-INDEX) ADD PDL-INDEX (A-CONSTANT 1))
        (JUMP-XCT-NEXT XGETLV1)
       ((M-T) ADD M-T (A-CONSTANT 1))

(ERROR-TABLE DEFAULT-ARG-LOCATIONS GET-LOCATION-OR-NIL M-B M-D)

XGETI (MISC-INST-ENTRY GET-LOCATION-OR-NIL)
        ((M-D) Q-TYPED-POINTER PDL-POP) ;Arg2, property name.
        ((M-T) Q-TYPED-POINTER PDL-POP) ;Arg1, symbol or plist.
        ((M-A) M-D)             ;Arg must go here as well, for INSTANCE-INVOKE-pl.
        ;; The following instruction can popj out of XGETI.
        ((ARG-CALL-XCT-NEXT PLGET) (I-ARG INSTANCE-INVOKE-GET-LOCATION-OR-NIL))
       ((M-B) M-T)              ;Save copy of arg in M-B.
XGETI1  (POPJ-EQUAL M-T A-V-NIL)                ;END OF PLIST REACHED
XGETI1A
        (open-carcdr m-t)
        (JUMP-EQUAL M-A A-D XGETI2)
        (open-qcdr m-t)
        (JUMP-NOT-EQUAL M-T A-V-NIL XGETI1A)
        (POPJ)

;Convert address of next link (whose car is the property value) into a locative.
XGETI2  (POPJ-AFTER-NEXT (M-T) DPB M-T Q-POINTER
            (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
       (NO-OP)

;SUBROUTINE TO PICK UP THE PLIST OF THE OBJECT IN M-T, RETURNING IT IN M-T.
;RETURNS NIL IF A RANDOM TYPE, FOR MACLISP COMPATIBILITY.  UNFORTUNATELY
;NOT USEFUL FOR PLIST-CHANGING THINGS, BUT THOSE AREN'T CURRENTLY IN MICROCODE ANYWAY.
  ;used for :GET (2 args), :GET-LOCATION-OR-NIL (2 args), :GETL (2 args)
  ;IARG has keyword index.
PLGET   (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)) PLGET2)
        (JUMP-DATA-TYPE-EQUAL M-T
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)) QCDR) ;"DISEMBODIED" PROPERTY LIST
        (JUMP-DATA-TYPE-EQUAL M-T
           (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)) QCDR) ;"DISEMBODIED" PROPERTY LIST
        (JUMP-DATA-TYPE-EQUAL M-T
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE)) INSTANCE-INVOKE-pl)
        (jump-data-type-equal m-t
           ;assume its a named structure.  If not, it will bomb fairly comprehensibly.
                (a-constant (byte-value q-data-type dtp-array-pointer)) instance-invoke-pl)
        (JUMP XFALSE)                           ;GET OF RANDOM THINGS NIL IN MACLISP, SO ...

PLGET2  ((VMA-START-READ) ADD M-T               ;ARG1, SYMBOL TO GET FROM
                      (A-CONSTANT 3))           ;GET PLIST CELL OF ARG1
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD)
       ((M-T) Q-TYPED-POINTER MD)

;; Send a message to the instance in M-T, with argument in M-A.
;; The I-ARG controls what message we send; it is an INSTANCE-INVOKE code.
;; POPJ's twice, so exits the calling function.
INSTANCE-INVOKE-pl
        (CALL INSTANCE-INVOKE-1);Push frame and the instance.
        ((PDL-PUSH) DPB M-A Q-TYPED-POINTER
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))   ;Push the arg.
        ((ARG-CALL MMCALL) (I-ARG 2))   ;Call, 2 args.  Value comes back in M-T.
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;Flush return address in GET or GETL.
        (POPJ)

;***Lots of things depend on CAR and CDR to save ACs, which could get clobberred here!!!***
  ;IARG is index of message to send.
  ;push object, and keyword.
INSTANCE-INVOKE-1
        ((M-B) DPB READ-I-ARG Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL P3ZERO)
        ((PDL-PUSH) M-T)        ;First push the instance -- that's what we call.
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-INSTANCE-INVOKE-VECTOR))
        (DISPATCH TRANSPORT MD)
        ((VMA-START-READ) ADD MD (A-CONSTANT 1))
        (CHECK-PAGE-READ)       ;Get the value of INSTANCE-INVOKE-VECTOR.
        (DISPATCH TRANSPORT MD)
        ((VMA-START-READ) M+A+1 MD A-B)
        (CHECK-PAGE-READ)       ;Get the operation keyword out of the vector.
        (DISPATCH TRANSPORT MD)
        (POPJ-AFTER-NEXT (PDL-PUSH) DPB MD Q-TYPED-POINTER
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
       (NO-OP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS GETL M-B M-S)

XGETL (MISC-INST-ENTRY GETL)
        ((M-A) Q-TYPED-POINTER PDL-POP) ;ARG2, LIST OF PROPERTIES
        ((M-B) Q-TYPED-POINTER PDL-POP) ;ARG1, THING TO GET FROM
        ;; The following instruction can popj out of XGETL.
        ((ARG-CALL-XCT-NEXT PLGET) (I-ARG INSTANCE-INVOKE-GETL))
       ((M-T) M-B)
        ((M-S) M-A)
XGETL1  (POPJ-EQUAL M-T A-V-NIL)                ;EXHAUSTED THE PLIST
XGETL1A
        (open-qcar-xct-next m-t)                ;GET NEXT INDICATOR
       ((PDL-PUSH) M-T)                         ;SAVE CURRENT PLIST NODE
        ((M-A) Q-TYPED-POINTER M-T)             ;SAVE INDICATOR.
        ((M-T) Q-TYPED-POINTER M-S)             ;GET LIST OF PROPERTY NAMES
XGETL2  (JUMP-EQUAL M-T A-V-NIL XGETL3)         ;NO MATCH THIS ONE
        (open-qcar-xct-next m-t)                ;GET NEXT PROP NAME TO TRY
       ((PDL-PUSH) M-T)
        (JUMP-EQUAL M-T A-A POP1TJ)             ;GOT IT
        (open-qcdr-sb-xct-next pdl-top)
       ((M-T) PDL-POP)          ;TRY NEXT PROP NAME
        (JUMP XGETL2)

XGETL3
        (open-qcdr-sb-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-POP)
        (open-qcdr m-t)
        (JUMP-NOT-EQUAL M-T A-V-NIL XGETL1A)
        (POPJ)

POP1TJ  (POPJ-AFTER-NEXT
          (M-GARBAGE) PDL-POP)
       ((M-T) PDL-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS ASSQ M-B M-D)

;; Called indirectly from QLENTR -- watch out.
XASSQ (MISC-INST-ENTRY ASSQ)
        ((M-T) Q-TYPED-POINTER PDL-POP) ;ARG2
        ((M-B) Q-TYPED-POINTER PDL-POP) ;ARG1
        ((M-D) M-T)
XASSQ1  (POPJ-EQUAL M-T A-V-NIL)
        (open-carcdr m-t)
     ;Skip nil's in alist.
        (jump-equal m-a a-v-nil xassq1)
        ((PDL-PUSH) M-T)
;; Next link in alist is on pdl, this alist element in M-A.  Take its car.
        (open-qcar-xct-next m-a)
       ((M-T) M-A)
;; Next link still on pdl, this alist element in M-A,
;; this element's key in M-T.
        (JUMP-NOT-EQUAL-XCT-NEXT M-T A-B XASSQ1)
       ((M-T) PDL-POP)
        (POPJ-XCT-NEXT)
       ((M-T) M-A)

(error-table default-arg-locations assoc m-b m-d)

XASSOC (MISC-INST-ENTRY ASSOC)
        ((M-T) Q-TYPED-POINTER PDL-POP) ;ARG2
        ((M-B) Q-TYPED-POINTER PDL-POP) ;ARG1
        ((m-d) m-t)
        (jump-data-type-equal m-b (a-constant (byte-value q-data-type dtp-symbol)) xassq1)
        (jump-data-type-equal m-b (a-constant (byte-value q-data-type dtp-fix)) xassq1)
XASSOC1 (POPJ-EQUAL M-T A-V-NIL)
        (open-carcdr m-t)
     ;Skip nil's in alist.
        (jump-equal m-a a-v-nil xassoc1)
        ((PDL-PUSH) M-T)
;; Next link in alist is on pdl, this alist element in M-A.  Take its car.
        (open-qcar-xct-next m-a)
       ((M-T) M-A)
;; Next link still on pdl, this alist element in M-A,
;; this element's key in M-T.
        ((pdl-push) m-a)
        (call-xct-next xequal-0)
       ((pdl-push) m-b)
        ((m-b) q-typed-pointer pdl-pop)
        ((m-a) pdl-pop)
        (jump-equal-xct-next m-t a-v-nil xassoc1)
       ((m-t) pdl-pop)
        (popj-xct-next)
       ((m-t) q-typed-pointer m-a)

POPTJ   (POPJ-AFTER-NEXT
          (M-T) Q-TYPED-POINTER PDL-POP)
       (NO-OP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS LAST (PP -1))

XLAST (MISC-INST-ENTRY LAST)
        ((M-T PDL-PUSH) Q-TYPED-POINTER PDL-TOP)
XLAST1
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list))
                popt1j)
        (open-qcdr-sb-xct-next m-t)
       ((PDL-TOP) M-T)
        (JUMP XLAST1)

POPT1J  (POPJ-AFTER-NEXT
          (M-T) PDL-POP)
        (PDL-POP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS LENGTH M-A)

XLENGT (MISC-INST-ENTRY LENGTH)
   (ERROR-TABLE RESTART LENGTH)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-A) M-T)
XTLENG
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)) xlen3)
        (JUMP-NOT-EQUAL M-T A-V-NIL XLEN2)
xlen3   ((PDL-PUSH)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
XLEN1
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) poptj)
        (open-qcdr-sb m-t)
        (JUMP-XCT-NEXT XLEN1)
       ((PDL-TOP) ADD PDL-TOP (A-CONSTANT 1))

;Arg is not a list of any sort.  If it's an array, return the active length.
XLEN2
        (jump-data-type-equal-xct-next m-t (a-constant (byte-value q-data-type dtp-array-pointer))
                xaaixl)
       ((PDL-PUSH) M-T)
        (CALL TRAP)
   (ERROR-TABLE ARGTYP LIST PP T LENGTH)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS SET M-S M-T)

XSET (MISC-INST-ENTRY SET)
        ((M-T) Q-TYPED-POINTER PDL-POP);ARG2, NEW VALUE & RESULT
                (ERROR-TABLE RESTART XSET)
     ;; PDL has arg1, the symbol to set.
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-symbol)) trap)
   (ERROR-TABLE ARGTYP SYMBOL PP 0 XSET)
   (ERROR-TABLE ARG-POPPED 0 PP M-T)
        ((M-S) Q-TYPED-POINTER PDL-POP)
        ((VMA-START-READ) ADD M-S (A-CONSTANT 1))       ;ACCESS V.C.
        (CHECK-PAGE-READ)                               ;READ VALUE CELL FIRST
        (JUMP-NOT-EQUAL M-S A-V-NIL XSET2)              ;Merge with STOCYC.
        (CALL TRAP)                                     ;Don't clobber NIL!
   (ERROR-TABLE ARGTYP NON-NIL M-S 0)

XNOT (MISC-INST-ENTRY NOT)
        ((M-J) Q-TYPED-POINTER PDL-POP)
        ((M-T) A-V-TRUE)
        (POPJ-AFTER-NEXT POPJ-EQUAL M-J A-V-NIL)
       ((M-T) A-V-NIL)

XTNOT   (JUMP-EQUAL M-T A-V-NIL XTRUE)          ;MC-LINKAGE
        (POPJ-AFTER-NEXT (M-T) A-V-NIL)         ;XFALSE
       (NO-OP)

XATOM (MISC-INST-ENTRY ATOM)
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-list)) xfalse)
        (JUMP XTRUE)

XGPN  (MISC-INST-ENTRY GET-PNAME)
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-symbol)) trap)
   (ERROR-TABLE ARGTYP SYMBOL PP T)
   (ERROR-TABLE ARG-POPPED 0 PP)
        ((VMA-START-READ) Q-TYPED-POINTER PDL-POP)
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD)
       ((M-T) DPB MD Q-POINTER
                 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER)))

   (MISC-INST-ENTRY %BINDING-INSTANCES)    ;(%BINDING-INSTANCES <LIST-OF-SYMBOLS>)
  ;SIMILAR TO CLOSURE, BUT TAKES NO FUNCTION.  VALUE RETURNNED IS LIST OF
  ;LOCATIVES WHICH ARE ALTERNATELY INTERNAL AND EXTERNAL VALUE CELL POINTERS.
XBINS   ((M-T) Q-TYPED-POINTER PDL-TOP)
        (JUMP-EQUAL M-T A-V-NIL POPTJ)
        (CALL XTLENG)
        ((M-B) ADD M-T A-T)                     ;TWO CELLS FOR EACH VAR
        ((M-B) Q-POINTER M-B)
        (CALL-XCT-NEXT LIST-OF-NILS)            ;ALLOCATE CLOSURE OUT OF LIST SPACE
       ((M-S) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-CNSADF)  ;LIST OF NILS SETS UP CDR CODES
        ((M-T PDL-PUSH) Q-POINTER M-T
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))  ;VALUE TO RETURN, EVENTUALLY
        (JUMP-XCT-NEXT XBINS1)
       ((PDL-PUSH) M-T) ;FILLING POINTER.

   (MISC-INST-ENTRY CLOSURE)    ;(CLOSURE <CLOSURE-LIST> <FCTN>)
XCLOS   ((M-J) Q-TYPED-POINTER PDL-POP)   ;FCTN
        (CALL-XCT-NEXT XTLENG)
       ((M-T) Q-TYPED-POINTER PDL-TOP)
        ((M-B) ADD M-T A-T ALU-CARRY-IN-ONE)    ;TWO CELLS FOR EACH VAR PLUS ONE FOR FCTN
        ((M-B) Q-POINTER M-B)
        (CALL-XCT-NEXT LIST-OF-NILS)            ;ALLOCATE CLOSURE OUT OF LIST SPACE
       ((M-S) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-CNSADF)  ;LIST OF NILS SETS UP CDR CODES
        ((PDL-PUSH)     ;EVENTUAL VALUE
                Q-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CLOSURE)))
        ((M-S) Q-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        (CALL-XCT-NEXT QRAR1)                   ;(RPLACA <CLOSURE-BLOCK> <FCTN>)
       ((M-T) M-J)                              ;FCTN
        ((PDL-PUSH) ADD M-T A-ZERO ALU-CARRY-IN-ONE)    ;STEP FILLING POINTER
XBINS1  ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER (A-CONSTANT 2))
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
;0(IP) - POINTER TO BINDING INSTANCE BLOCK BEING FILLED IN
;-1(IP)- VALUE TO RETURN EVENTUALLY.
;-2(IP)- LIST OF VARS TO CLOSE OVER.
XCLOS4  (JUMP-EQUAL M-T A-V-NIL XCLOSX)         ;LIST OF SYMS TO CLOSE IN M-T
        (open-qcar m-t)
        (call-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-symbol)) trap)
   (ERROR-TABLE ARGTYP SYMBOL M-T NIL)
        ((M-S) PDL-POP) ;FILLING POINTER  (IN POSITION FOR RPLACA)
        ((M-T) DPB M-T Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
        (CALL-XCT-NEXT QRAR1)
       ((M-T PDL-PUSH) ADD M-T A-ZERO ALU-CARRY-IN-ONE)
                                        ;POINTER TO INTERNAL VALUE CELL
                                        ;M-T GETS LOCATION FILLED.
        ((VMA-START-READ) Q-POINTER PDL-POP)    ;READ INTERNAL VALUE CELL
        (CALL-CONDITIONAL PG-FAULT XCLOS-PG-FAULT)
        ;(CHECK-PAGE-READ)
        ((PDL-PUSH) ADD M-T A-ZERO ALU-CARRY-IN-ONE) ;BUMP FILLING POINTER
        (DISPATCH TRANSPORT-NO-EVCP MD)
     ;; Jump if already EVCP.
        (JUMP-DATA-TYPE-EQUAL MD
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTERNAL-VALUE-CELL-POINTER)) XCLOS3A)
        ((PDL-PUSH) VMA)        ;SAVE POINTER TO INTERNAL VALUE CELL
        ((PDL-PUSH) MD) ;SAVE INTERNAL VALUE CELL CONTENTS
        (call-xct-next allocate-list-storage-default)
       ((m-b) (a-constant 1))
        ((VMA M-T) Q-POINTER M-T                ;ADDRESS OF NEW EXTERNAL V-C
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTERNAL-VALUE-CELL-POINTER)))
        ((MD-START-WRITE)
                DPB PDL-TOP Q-TYPED-POINTER  ;V-C CONTENTS
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        (CHECK-PAGE-WRITE)
        (gc-write-test)                         ;***??
        ((MD) SELECTIVE-DEPOSIT PDL-POP
                Q-ALL-BUT-TYPED-POINTER A-T)
        ((VMA-START-WRITE) PDL-POP)     ;WRITE INTO INTERNAL V-C
        (CHECK-PAGE-WRITE)
        (gc-write-test)                         ;***??
XCLOS3  ((M-T) DPB M-T Q-POINTER       ;TO AVOID PROFUSION OF RANDOM D.T.S.  AVOIDS LOSSAGE
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE))) ;WITH CAR IN QCLS1
                                        ;QCLS1 CHANGES BACK TO DTP-EXT-V-C EVENTUALLY
        (CALL-XCT-NEXT QRAR1)                           ;FORWARDING PNTR IN M-T
       ((M-S) PDL-POP)                  ;GET BACK FILL POINTER
        ((PDL-PUSH) ADD M-T A-ZERO ALU-CARRY-IN-ONE)   ;BUMP FILL POINTER
        ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER (A-CONSTANT 2))      ;BUMP VARS POINTER
;#+CADR (CALL-XCT-NEXT QCDR-SB)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE C-PDL-BUFFER-INDEX
;                CDR-PRE-DISPATCH-SB-DIRECT)
        (open-qcdr-sb-xct-next c-pdl-buffer-index)
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
                ;PDL-BUFFER-INDEX NOT SAVED ACROSS SEQUENCE BREAKS
        ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-POINTER (A-CONSTANT 2))
        (JUMP-XCT-NEXT XCLOS4)
       ((C-PDL-BUFFER-INDEX) M-T)

XCLOS3A (JUMP-XCT-NEXT XCLOS3)
       ((M-T) MD)               ;POINTER TO EXTERNAL V-C

XCLOSX  ((M-GARBAGE) PDL-POP)   ;FLUSH FILLING POINTER
        (POPJ-AFTER-NEXT
         (M-T) Q-TYPED-POINTER PDL-POP)
       ((M-GARBAGE) PDL-POP)    ;FLUSH CLOSURE-LIST

XCLOS-PG-FAULT
        (JUMP-LESS-THAN VMA (A-CONSTANT LOWEST-A-MEM-VIRTUAL-ADDRESS) PGF-R)
        (CALL TRAP)
    (ERROR-TABLE ATTEMPT-CLOSE-OVER-A-MEMORY-LOCATION)


      (MISC-INST-ENTRY %EXTERNAL-VALUE-CELL)
XEVC    (CALL XVCL)             ;Returns address of IVC.  Does not follow EVCPs.
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT-IVC MD) ;GC
       ((M-T) DPB VMA Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))

XVCL  (MISC-INST-ENTRY VALUE-CELL-LOCATION)
        ((m-TEM1) (A-CONSTANT 1))
XCL1
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-symbol)) trap)
   (ERROR-TABLE ARGTYP SYMBOL PP T)
   (ERROR-TABLE ARG-POPPED 0 PP)
        (POPJ-AFTER-NEXT
         (M-T) DPB Q-POINTER PDL-POP
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
       ((M-T) ADD M-T A-TEM1)

XFCL  (MISC-INST-ENTRY FUNCTION-CELL-LOCATION)
        (JUMP-XCT-NEXT XCL1)
       ((m-TEM1) (A-CONSTANT 2))

XPRPCL (MISC-INST-ENTRY PROPERTY-CELL-LOCATION)
        (JUMP-XCT-NEXT XCL1)
       ((m-TEM1) (A-CONSTANT 3))

XPACKAGE-CELL-LOCATION (MISC-INST-ENTRY PACKAGE-CELL-LOCATION)
        (JUMP-XCT-NEXT XCL1)
       ((m-TEM1) (A-CONSTANT 4))

XFCTEV (MISC-INST-ENTRY FSYMEVAL)
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-symbol)) trap)
     (error-table argtyp symbol pp t)
        ((VMA-START-READ) ADD PDL-POP (a-constant 2))
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD)
       ((M-T) Q-TYPED-POINTER MD)

XSYMEV (MISC-INST-ENTRY SYMEVAL)
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-symbol)) trap)
     (error-table argtyp symbol pp t)
        ((VMA-START-READ) ADD PDL-POP (a-constant 1))
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT MD)
       ((M-T) Q-TYPED-POINTER MD)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS MEMQ M-B M-J)

XMEMQ (MISC-INST-ENTRY MEMQ)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-B) Q-TYPED-POINTER PDL-POP)
XMEMQ2  ((M-J) M-T)                             ;entry from XMEMBER-EQL
XMEMQ1  (POPJ-EQUAL M-T A-V-NIL)
        (open-carcdr-xct-next m-t)
;#+CADR (CALL-XCT-NEXT CARCDR)  ;Get car in M-A, cdr in M-T.
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-T CARCDR-DISPATCH-DIRECT)
       ((M-D) M-T)              ;Save this link, as value if this elt matches.
        (JUMP-NOT-EQUAL M-A A-B XMEMQ1)
        (POPJ-XCT-NEXT)
       ((M-T) M-D)

(error-table default-arg-locations member m-b m-j)

XMEMBER (MISC-INST-ENTRY MEMBER)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-B) Q-TYPED-POINTER PDL-POP)
        ((M-J) M-T)
        (jump-data-type-equal m-b (a-constant (byte-value q-data-type dtp-symbol)) xmemq1)
        (jump-data-type-equal m-b (a-constant (byte-value q-data-type dtp-fix)) xmemq1)
XMEMBER1
        (POPJ-EQUAL M-T A-V-NIL)
        (open-carcdr-xct-next m-t)
;#+CADR (CALL-XCT-NEXT CARCDR)  ;Get car in M-A, cdr in M-T.
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-T CARCDR-DISPATCH-DIRECT)
       ((M-D) M-T)              ;Save this link, as value if this elt matches.
        ((PDL-PUSH) M-T)
        ((M-T) M-A)
        ((PDL-PUSH) M-D)                ;VALUE IF EQUAL T.
        (CALL-XCT-NEXT XEQUAL-0)
       ((PDL-PUSH) M-B)
        ((M-B) Q-TYPED-POINTER PDL-POP)
        ((M-D) PDL-POP)
        (JUMP-EQUAL-XCT-NEXT M-T A-V-NIL XMEMBER1)
       ((M-T) PDL-POP)
        (POPJ-XCT-NEXT)
       ((M-T) Q-TYPED-POINTER M-D)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS MEMBER-EQL M-C M-B)
XMEMBER-EQL (MISC-INST-ENTRY MEMBER-EQL)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-C) Q-TYPED-POINTER PDL-POP)
;; Do it using MEMQ if 1st arg is not a number.
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC XMEMQ2)))
        (DISPATCH-XCT-NEXT Q-DATA-TYPE M-C POPJ-IF-NOT-NUMBER)
       ((M-B) M-C)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
XMEMBER-EQL-1
        (POPJ-EQUAL M-T A-V-NIL)
        (open-carcdr-xct-next m-t)
;       (CALL-XCT-NEXT CARCDR)  ;Get car in M-A, cdr in M-T.
       ((PDL-PUSH) M-T)         ;Save this link, as value if this elt matches.
        ((PDL-PUSH) M-T)        ;Save the following link.
        ((PDL-PUSH) M-C)        ;Save m-c, which is trashed by arith-xnm-any
        ((M-T) M-C)             ;Second arg.
        (CALL-xct-next XEQL1)
       ((pdl-push) m-a)         ;First arg to xeql1
        (JUMP-NOT-EQUAL M-T A-V-NIL XMEMBER-EQL-2)  ;Jump if they are EQL.
        ((m-c) pdl-pop)         ;restore m-c
        ((M-T) PDL-POP)         ;Else continue with next link,
        (JUMP-XCT-NEXT XMEMBER-EQL-1)
       (PDL-POP)                ;discard this link whose car did not match.
XMEMBER-EQL-2
        ((m-c) pdl-pop)         ;discard saved m-c
        (POPJ-XCT-NEXT PDL-POP) ;Discard next link,
       ((M-T) PDL-POP)          ;return this link whose car matched.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS FIND-POSITION-IN-LIST M-D M-C)

XFPIL (MISC-INST-ENTRY FIND-POSITION-IN-LIST)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-C) M-T)
        ((M-D) Q-TYPED-POINTER PDL-POP)
        ((M-B) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
XFPIL1  (POPJ-EQUAL M-T A-V-NIL)
        (open-carcdr-xct-next m-t)
;#+CADR (CALL-XCT-NEXT CARCDR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-T CARCDR-DISPATCH-DIRECT)
       ((M-B) ADD M-B (A-CONSTANT 1))
        (JUMP-NOT-EQUAL M-A A-D XFPIL1)
        (POPJ-XCT-NEXT)
       ((M-T) SUB M-B (A-CONSTANT 1))

XLOCATE-IN-INSTANCE (MISC-INST-ENTRY LOCATE-IN-INSTANCE)
    (ERROR-TABLE RESTART XLOCATE-IN-INSTANCE)
        (JUMP-DATA-TYPE-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL))
                XLOCATE-IN-INSTANCE-1)
        (CALL-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)) TRAP)
    (ERROR-TABLE ARGTYP SYMBOL-OR-LOCATIVE PP 1 XLOCATE-IN-INSTANCE)
        (call find-structure-header)
        (JUMP-XCT-NEXT XLOCATE-IN-INSTANCE-2)
       ((M-C) M-T)

XLOCATE-IN-INSTANCE-1
        ((M-C) LDB Q-TYPED-POINTER PDL-POP)
;Decode the first arg.
XLOCATE-IN-INSTANCE-2
    (ERROR-TABLE RESTART XLOCATE-IN-INSTANCE-2)
        (CALL-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE)) TRAP)
    (ERROR-TABLE ARGTYP INSTANCE PP 1 XLOCATE-IN-INSTANCE-2)
    (ERROR-TABLE ARG-POPPED M-C)
        ((M-A VMA-START-READ) Q-TYPED-POINTER PDL-POP)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((VMA-START-READ) ADD MD (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-ALL-INSTANCE-VARIABLES)))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ((VMA M-D) MD)
;; M-A holds the instance.
;; M-D holds the list of all instance variables.
;; VMA is a tail of that list.
;; M-C is the symbol we want.
XLOCATE-IN-INSTANCE-LOOP
        ((VMA-START-READ) VMA)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ((M-TEM) Q-TYPED-POINTER MD)
        (JUMP-EQUAL M-TEM A-C XLOCATE-IN-INSTANCE-FOUND)
        ((M-TEM) Q-CDR-CODE MD)
        (JUMP-EQUAL-XCT-NEXT M-TEM (A-CONSTANT (EVAL CDR-NEXT)) XLOCATE-IN-INSTANCE-LOOP)
       ((VMA) ADD VMA (A-CONSTANT 1))
;; Variable is not found.
        (CALL TRAP)
    (ERROR-TABLE INSTANCE-LACKS-INSTANCE-VARIABLE M-C M-A)

XLOCATE-IN-INSTANCE-FOUND
        ((M-1) SUB VMA A-D)
        ((M-T) M+A+1 M-A A-1)
        (POPJ-XCT-NEXT)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))

   (MISC-INST-ENTRY COMMON-LISP-LISTP)
XCOMMON-LISP-LISTP
        (JUMP-NOT-EQUAL M-T A-V-NIL XLISTP)
        (jump-xct-next xtrue)
       (pdl-pop)
   (MISC-INST-ENTRY LISTP)
XLISTP
        (jump-data-type-not-equal pdl-pop (a-constant (byte-value q-data-type dtp-list)) xfalse)
        (JUMP XTRUE)

XNLISTP   (MISC-INST-ENTRY NLISTP)
XNLSTP
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-list)) xfalse)
        (JUMP XTRUE)

XSYMBOLP   (MISC-INST-ENTRY SYMBOLP)
XSYMP
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-symbol)) xtrue)
        (JUMP XFALSE)

XNSYMBOLP   (MISC-INST-ENTRY NSYMBOLP)
XNSYMP
        (jump-data-type-not-equal pdl-pop (a-constant (byte-value q-data-type dtp-symbol)) xtrue)
        (JUMP XFALSE)

XARRAYP   (MISC-INST-ENTRY ARRAYP)
XARRYP
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-array-pointer))
                xtrue)
        (JUMP XFALSE)

XFIXNUMP (MISC-INST-ENTRY FIXNUMP)
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-fix)) xtrue)
        (JUMP XFALSE)

XSMALL-FLOATP (MISC-INST-ENTRY SMALL-FLOATP)
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-small-flonum))
                xtrue)
        (JUMP XFALSE)

XCHARACTERP (MISC-INST-ENTRY CHARACTERP)
        (jump-data-type-equal pdl-pop (a-constant (byte-value q-data-type dtp-character))
                xtrue)
        (JUMP XFALSE)

   (MISC-INST-ENTRY FBOUNDP)
XFCTNP  (JUMP-XCT-NEXT XBOUNP1)
       ((M-1) (A-CONSTANT 2))

   (MISC-INST-ENTRY BOUNDP)
XBOUNP  ((M-1) (A-CONSTANT 1))
XBOUNP1 (ERROR-TABLE RESTART XBOUNP1)
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-symbol)) trap)
   (ERROR-TABLE ARGTYP SYMBOL PP 0 XBOUNP1)
   (ERROR-TABLE ARG-POPPED 0 PP)
        ((VMA-START-READ) ADD PDL-POP A-1)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-IVC MD)     ;NOT USING CONTENTS, DON'T BARF IF NULL
        (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-null)) xfalse)
        (JUMP XTRUE)

XENDP (MISC-INST-ENTRY ENDP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
    (ERROR-TABLE RESTART XENDP)
        (JUMP-EQUAL M-T A-V-NIL XTRUE)
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)) xfalse)
        (CALL TRAP)
    (ERROR-TABLE ARGTYP LIST M-T 0 XENDP)
    (ERROR-TABLE ARG-POPPED M-T)

XNAMED-STRUCTURE-P (MISC-INST-ENTRY NAMED-STRUCTURE-P)
        ((M-A) Q-TYPED-POINTER PDL-POP)
        (popj-data-type-not-equal-xct-next m-a
                (a-constant (byte-value q-data-type dtp-array-pointer)))
XNAMED-STRUCTURE-P-0
       ((M-T) A-V-NIL)
        ((VMA-START-READ) M-A)  ;Fetch array header.
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        (POPJ-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-NAMED-STRUCTURE-FLAG) MD)  ;Not named str!
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-LEADER-BIT) MD XNAMED-STRUCTURE-P-1)
        ((VMA-START-READ) SUB VMA (A-CONSTANT 1))  ;Array has leader; fetch leader length.
        (CHECK-PAGE-READ)
        ((M-1) Q-POINTER MD)
        (POPJ-LESS-OR-EQUAL M-1 (A-CONSTANT 1)) ;Ldr length = 1 => no structure type.
        ((VMA-START-READ) SUB VMA (A-CONSTANT 2))       ;Yes => fetch leader elt. 1.
        (JUMP XNAMED-STRUCTURE-P-CHECK-CLOSURE)

XNAMED-STRUCTURE-P-1            ;Array has no leader.  Fetch element 0.
        ((M-Q) M-MINUS-ONE)     ;Prevent any subscript-oob error.
        (CALL-xct-next decode-1d-array-force-1)
       ((m-array-pointer) validate-array-cache m-a)
    (call store-array-registers-in-accumulators)
        (JUMP-EQUAL M-S A-ZERO XFALSE)  ;Now check the subscript, but don't err, just ret NIL.
        ((VMA-START-READ) M-E)  ;It checks; get the first element of the array.
;Here we have the contents of the appropriate array slot or leader slot.
XNAMED-STRUCTURE-P-CHECK-CLOSURE
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
     ;; A symbol => return it.
        (popj-data-type-equal-xct-next md (a-constant (byte-value q-data-type dtp-symbol)))
       ((M-T) Q-TYPED-POINTER MD)
;If it's not a symbol, only a closure is a valid thing to find here.
        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-closure)) xfalse)
        (CALL-XCT-NEXT QCAR)
;Get the function closed over.  It should be a symbol.  Return it if so, else NIL.
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        (popj-data-type-equal m-t (a-constant (byte-value q-data-type dtp-symbol)))
        (JUMP XFALSE)

XTYPEP-STRUCTURE-OR-FLAVOR (MISC-INST-ENTRY TYPEP-STRUCTURE-OR-FLAVOR)
        ((M-I) Q-TYPED-POINTER PDL-POP)
        ((M-A) Q-TYPED-POINTER PDL-POP)
        (jump-data-type-equal m-a (a-constant (byte-value q-data-type dtp-array-pointer))
                xtypep-structure)
        (jump-data-type-equal m-a (a-constant (byte-value q-data-type dtp-instance))
                xtypep-flavor)
        (JUMP XFALSE)

XTYPEP-STRUCTURE
        (CALL XNAMED-STRUCTURE-P-0)
XTYPEP-STRUCTURE-1
        (JUMP-EQUAL M-T A-I XTRUE)
;; Not exact match of types.  See if the actual type INCLUDEs some other type.
;; (and (setq d (get xname 'defstruct-description))
;;      (defstruct-description-named-p d)
;;      (setq xname (car (defstruct-description-include d))))
;; (unless xname (return nil))
        ((PDL-PUSH) M-A)
        ((PDL-PUSH) M-I)
        ((PDL-PUSH) M-T)
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-DEFSTRUCT-DESCRIPTION))
        (DISPATCH TRANSPORT MD)
        ((PDL-PUSH) MD)
        (CALL XGET)
        (JUMP-EQUAL M-T A-V-NIL XTYPEP-STRUCTURE-2)
;; Ref the DEFSTRUCT-DESCRIPTION-INCLUDE slot.
        ((PDL-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 13)))
        ((PDL-PUSH) M-T)
        (CALL XNTH)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-T CAR-PRE-DISPATCH-DIRECT)
;#+LAMBDA(NO-OP)
;#+CADR (CALL QCAR)
        (open-qcar m-t)
XTYPEP-STRUCTURE-2
        ((M-I) PDL-POP)
        ((M-A) PDL-POP)
;; If object's type doesn't INCLUDE another, return NIL.
        (POPJ-EQUAL M-T A-V-NIL)
;; The type does INCLUDE another--is that other the one we are looking for?
        (JUMP XTYPEP-STRUCTURE-1)

XTYPEP-FLAVOR
        ((VMA-START-READ) M-A)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((M-B) DPB MD Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER)))
;; M-B has the flavor structure for the flavor of this instance.
        ((VMA-START-READ) ADD M-B (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-DEPENDS-ON-ALL)))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-list)) xfalse)
        ((PDL-PUSH) M-I)
        ((PDL-PUSH) Q-TYPED-POINTER MD)
        (CALL XMEMQ)
        (JUMP-NOT-EQUAL M-T A-V-NIL XTRUE)
        (POPJ)

;;;

mapping-function-call-out
        (call-xct-next carcdr)
       ((pdl-push) m-q)         ;Save function.
        (call-xct-next p3zero)  ;Set up micro-macro call block.
       ((pdl-push) m-t)         ;Save remainder of list.
        ((pdl-push) m-q)        ;Push function.
        ((pdl-push) dpb m-a q-typed-pointer
                (a-constant (byte-value q-cdr-code cdr-nil)))   ;Push list element as argument.
        ((arg-call mmcall) (i-arg 1))
        ((m-a) m-t)             ;Result.
        (popj-after-next
          (m-t) pdl-pop)        ;List.
       ((m-q) pdl-pop)          ;Function.

x-internal-mapc (misc-inst-entry %internal-mapc)
        ((m-t) q-typed-pointer pdl-pop)         ;List.
        ((m-q) q-typed-pointer pdl-pop)         ;Function.
        (popj-equal m-t a-v-nil)
        ((pdl-push) m-t)        ;Save list for result.
        (call-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) trap)
    (error-table argtyp list m-t 0)
x-internal-mapc-loop
        (call mapping-function-call-out)
        (jump-not-equal m-t a-v-nil x-internal-mapc-loop)
x-internal-mapc-finish
        (popj-after-next
          (m-t) q-typed-pointer pdl-pop)        ;Restore initial list as argument.
       (no-op)

x-internal-mapcar (misc-inst-entry %internal-mapcar)
        ((m-t) q-typed-pointer pdl-pop)         ;List.
        ((m-q) q-typed-pointer pdl-pop)         ;Function.
        (popj-equal m-t a-v-nil)
        (call-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) trap)
          (error-table argtyp list m-t 0)
        ((m-j) m-t)                             ;Save argument list.
        (call-xct-next xlen1)
       ((pdl-push) (a-constant (byte-value q-data-type dtp-fix)))
        ((m-b) q-pointer m-t)
        (call-xct-next list-of-nils)
       ((m-s) dpb m-zero q-all-but-typed-pointer a-cnsadf)
        ((pdl-push m-s) dpb q-pointer m-t (a-constant (byte-value q-data-type dtp-list)))
        ((m-t) m-j)                             ;Restore argument list.
        ;; M-S = loc of result list (also on pdl), M-T argument list, M-Q funarg.
x-internal-mapcar-loop
        (call-xct-next mapping-function-call-out)
       ((pdl-push) m-s)
        (jump-equal m-t a-v-nil x-internal-mapcar-finish)
        ((md) ldb m-a q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        ((vma-start-write m-s) pdl-pop)
        (check-page-write)
        (gc-write-test)
        (jump-xct-next x-internal-mapcar-loop)
       ((m-s) add m-s (a-constant 1))
x-internal-mapcar-finish
        ((md) ldb m-a q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-nil)))
        ((vma-start-write) pdl-pop)
        (check-page-write)
        ((m-s) a-v-nil)                                ;Could contain garbage from above.
        (gc-write-test)
        ((m-t) pdl-pop)
        (popj)

;;;two args on stack.
x-append (misc-inst-entry %internal-append-2)
        ((m-d) q-typed-pointer c-pdl-buffer-pointer-pop)   ;2nd arg
        ((m-c) q-typed-pointer c-pdl-buffer-pointer-pop)   ;1st arg
        (call-xct-next xtleng)
       ((m-t) m-c)
        ((m-b) q-pointer m-t)
        (jump-equal m-b a-zero return-m-d)
        (jump-equal m-d a-v-nil xapp1)
        ((m-b) m+1 m-b a-zero)          ;extra for cdr-normal
xapp1 ;necessary to initialize storage since taking CDR could involve sending a message,
      ;which would get to macrocode and allow sequence breaks, etc.
        (call-xct-next list-of-nils)
       ((m-s) dpb m-zero q-all-but-typed-pointer a-cnsadf)
        ((pdl-push m-s) dpb q-pointer m-t (a-constant (byte-value q-data-type dtp-list)))
        ((m-t) m-c)
xappend-copy-list-loop
        (open-carcdr m-t)
        ((md) dpb m-a q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))
        ((vma-start-write) m-s)
        (check-page-write)
        (gc-write-test)
        (jump-data-type-equal-xct-next m-t (a-constant (byte-value q-data-type dtp-list))
            xappend-copy-list-loop)
       ((m-s) add m-s (a-constant 1))

        (jump-equal-xct-next m-d a-v-nil xapp2)
       ((m-s) a-v-nil)                                 ;Could contain garbage from above.
        ((md-start-write) dpb md q-typed-pointer (a-constant (byte-value q-cdr-code cdr-normal)))
        (check-page-write)
        (gc-write-test)
        ((md) dpb m-d q-typed-pointer (a-constant (byte-value q-cdr-code cdr-error)))
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write)
        (gc-write-test)
        ((m-t) q-typed-pointer pdl-pop)
        (popj)

xapp2   ((md-start-write) dpb md q-typed-pointer (a-constant (byte-value q-cdr-code cdr-nil)))
        (check-page-write)
        (gc-write-test)
        (popj-after-next
         (m-t) q-typed-pointer pdl-pop)
       (no-op)

return-m-d
        (popj-after-next
          (m-t) q-typed-pointer m-d)
       (no-op)

x-internal-nconc (misc-inst-entry %internal-nconc-2)
        ((m-d) q-typed-pointer pdl-pop)         ;arg2
        ((m-t) q-typed-pointer pdl-pop)         ;arg1
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) return-m-d)
        (call-xct-next xlast)
       ((pdl-push m-j) m-t)             ;used by last.
        ((pdl-push) m-t)
        (call-xct-next xrplcd)
       ((pdl-push) m-d)
        (popj-after-next (m-t) m-j)
       (no-op)

(begin-comment) Zwei Lossage (end-comment)

     (error-table restart delq)
delq (misc-inst-entry %internal-delq)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 2 :restart delq)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)             ;# times.
        ((m-s) q-typed-pointer c-pdl-buffer-pointer-pop)       ;list
        ((m-r) q-typed-pointer c-pdl-buffer-pointer-pop)       ;item to delete
        ((m-t) q-typed-pointer m-s)
delq-0
        (jump-equal m-q a-zero delq-return)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) delq-return)
        (open-carcdr m-t)
;       (dispatch-xct-next dispatch-write-vma q-data-type m-t carcdr-dispatch-direct)
;      (no-op)
        (jump-not-equal m-a a-r delq-1)
        ((m-s) q-typed-pointer m-t)
        (jump-xct-next delq-0)
       ((m-q) add m-q a-minus-one)
delq-1
        ((m-t) q-typed-pointer m-s)
delq-2
        ((m-c) q-typed-pointer m-d)
delq-3
        (jump-equal m-q a-zero delq-return)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) delq-return)
;       (dispatch-xct-next dispatch-write-vma q-data-type m-t carcdr-dispatch-direct)
        (open-carcdr-xct-next m-t)
       ((m-d) q-typed-pointer m-t)
        (jump-not-equal m-a a-r delq-2)
        (call-xct-next delq-rplacd)
       ((pdl-push) q-typed-pointer m-s)
        (jump-xct-next delq-3)
       ((m-s) q-typed-pointer pdl-pop)

delq-return
        (popj-after-next
          (m-t) q-typed-pointer m-s)
        (no-op)

delq-rplacd
        ((vma-start-read) m-c)                  ;GET CAR OF CONS TO BE SMASHED (need cdr codes)
        (check-page-read)
        ((m-a) q-typed-pointer m-t)
        ((m-q) add m-q a-minus-one)
        (dispatch transport-cdr md)             ;CHASE INVISIBLE, NO NEED TO TRANSPORT
        (dispatch-xct-next q-cdr-code md rplacd-cdr-dispatch)
   (error-table bad-cdr-code vma)
       ((m-i) md)

))

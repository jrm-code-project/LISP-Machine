;-*- Mode:LISP; Base:8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

(DEFCONST UC-CALL-RETURN '(
;;; Push a micro-to-macro call block (just the first 3 words, not the function)
;;; in the case where ADI has been pushed
P3ADI
        (declare (clobbers a-zr a-tem a-tem1))
        (JUMP-XCT-NEXT P3ZER1)
       ((PDL-PUSH) (A-CONSTANT (PLUS
                (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                (BYTE-VALUE %%LP-CLS-ADI-PRESENT 1)
                (BYTE-VALUE %%LP-CLS-ATTENTION 1))))

;;; Push a micro-to-macro call block (just the first 3 words, not the function)
P3ZERO
        (declare (clobbers a-zr a-tem a-tem1))
        ((PDL-PUSH) (A-CONSTANT
                (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
P3ZER1  ((M-ZR) ADD PDL-POINTER ;1- because of push just done
                 (A-CONSTANT (EVAL (1- %LP-CALL-BLOCK-LENGTH))))
        ((M-TEM) SUB M-ZR A-IPMARK)
        ((m-TEM1) DPB M-TEM (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK)
                (A-CONSTANT (BYTE-VALUE %%LP-CLS-DESTINATION D-MICRO)))
        ((M-TEM) SUB M-ZR A-AP)
        ((m-TEM1) DPB M-TEM (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) A-TEM1)
        ((PDL-TOP)              ;IOR with LPCLS Q already pushed
                IOR PDL-TOP A-TEM1)
        ((PDL-PUSH)     ;Push LPEXS Q
            (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (POPJ-AFTER-NEXT                ;Push LPENS Q
          (PDL-PUSH)
            (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-ZR)        ;Caller must push LPFEF Q


;;; Calling a function of strange data-type.  Call the interpreter (APPLY-LAMBDA).

INTP1

;; ??? Get the lexpr-call flag before FINISH-ENTERED-FRAME clobbers it. ???
;; Finish the frame for the interpreted function
;; Leaves PDL-INDEX on the entry state word.
 ;      (CALL FINISH-ENTERED-FRAME)
 ;Don't do this now; arglist is in a confusing state if lexpr call.
 ;      (CALL-IF-BIT-SET M-TRAP-ON-CALLS QMRCL-TRAP)
        ;; Get the arg-list.  Could be passed by FEXPR/LEXPR call, could be
        ;; NIL, or could be a stack-list of the spread arguments.
        (JUMP-EQUAL M-R A-ZERO INTP-NO-ARGS)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        (JUMP-IF-BIT-SET (LISP-BYTE %%LP-ENS-LCTYP) PDL-INDEX-INDIRECT INTP-LEXPR-CALL)
INTP-LEXPR-CALL-CONTINUE
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)      ;TO VIRTUAL ADDRESS
       ((M-K) ADD A-ZERO M-AP ALU-CARRY-IN-ONE)
        ((M-T) DPB M-K Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST))) ;ARG LIST PTR
;Here with list-of-args in M-T.
INTP-ARGLIST-MADE
        (CALL P3ZERO)                                   ;Open micro-to-macro call block
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-APL)) ;Get function cell of APPLY-LAMBDA
        ((PDL-PUSH) READ-MEMORY-DATA)   ;Push it
 ;I'm not sure if this PDL-INDEX is used later, so I wont change it to M-FEF for now
        ((PDL-INDEX) M-AP)
        ((PDL-PUSH) DPB PDL-INDEX-INDIRECT      ;Arg 1 = fcn being called
                Q-ALL-BUT-CDR-CODE (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
        ((PDL-PUSH) DPB M-T                     ;Arg 2 = arg list
                Q-ALL-BUT-CDR-CODE (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-JUMP MMJCALR) (I-ARG 2))          ;Call to D-RETURN

INTP-NO-ARGS
        (JUMP-XCT-NEXT INTP-ARGLIST-MADE)
       ((M-T) A-V-NIL)

;Here if last slot has rest arg.
INTP-LEXPR-CALL
        ((PDL-INDEX) ADD M-AP A-R)      ;PI points to last arg slot.
        (JUMP-GREATER-THAN M-R (A-CONSTANT 1) INTP21)
;First (and only) slot is already list of args.  Get it in M-T.
        (JUMP-XCT-NEXT INTP-ARGLIST-MADE)
       ((M-T) Q-TYPED-POINTER PDL-INDEX-INDIRECT)

;Here if both spread args and rest arg.
;Hack the cdr codes to make them form one list,
;then use a pointer to the first spread arg (as we do if no rest arg),
INTP21  ((PDL-INDEX-INDIRECT) DPB PDL-INDEX-INDIRECT Q-ALL-BUT-CDR-CODE
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-ERROR)))
        ((PDL-INDEX) SUB PDL-INDEX (A-CONSTANT 1))
        (JUMP-XCT-NEXT INTP-LEXPR-CALL-CONTINUE)
       ((PDL-INDEX-INDIRECT) DPB PDL-INDEX-INDIRECT Q-ALL-BUT-CDR-CODE
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NORMAL)))

REF-SUPPORT-VECTOR
        ((VMA-START-READ) ADD READ-I-ARG A-V-SUPPORT-ENTRY-VECTOR)
        (CHECK-PAGE-READ)
        (popj-after-next dispatch transport md)
       (no-op)

;;; This code is also duplicated at QLENTR and QME1 to save time.
;;; Make the new frame current, maintain pdl buffer, store arg count into frame.
;;; Does not clobber %LP-ENS-LCTYP.
;;; Note that the frame being entered may already be in M-AP!
;;;  in that case, FINISH-ENTERED-FRAME will turn out to be a NO-OP.  But only if M-S
;;;  is not disturbed!
;FINISH-ENTERED-FRAME
;       ((PDL-INDEX) SUB M-S A-AP)      ;Increment to M-AP (truncated to 10 bits)
;       ((M-PDL-BUFFER-ACTIVE-QS) ADD PDL-INDEX A-PDL-BUFFER-ACTIVE-QS)
;       (CALL-GREATER-THAN-XCT-NEXT M-PDL-BUFFER-ACTIVE-QS
;                                   A-PDL-BUFFER-HIGH-WARNING PDL-BUFFER-DUMP)
;       ((M-AP) M-S)
; store M-FEF if this code is reactivated
;       ((M-TEM) DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED)
;               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       (POPJ-AFTER-NEXT
;        (PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
;       ((PDL-INDEX-INDIRECT) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT
;         (LISP-BYTE %%LP-ENS-LCTYP) A-TEM)

;CALLING NUMBER AS FUNCTION
NUMBER-CALLED-AS-FUNCTION
  ;     (CALL FINISH-ENTERED-FRAME)
        (CALL-IF-BIT-SET M-TRAP-ON-CALLS QMRCL-TRAP)
        (CALL TRAP)
    (ERROR-TABLE NUMBER-CALLED-AS-FUNCTION M-A)
        ((M-T) M-A)
        (JUMP CSM-6)

;CALLING SYMBOL AS FUNCTION

QMRCL1  ((VMA-START-READ) ADD output-selector-mask-25           ;GET FUNCTION CELL
               M-A (A-CONSTANT (plus (byte-value q-data-type dtp-locative)
                                     (byte-value q-pointer 2))))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ((PDL-INDEX) M-AP)
        ((PDL-INDEX-INDIRECT M-A) Q-TYPED-POINTER READ-MEMORY-DATA)     ;Store new frob to call
        (DISPATCH-XCT-NEXT qmrcl-dispatch M-A)  ;DISPATCH ON DATA TYPE
       ((m-fef) m-a)


;DON'T CALL QBND4 TO AVOID REFERENCING A-SELF VIA SLOW VIRTUAL-MEMORY PATH
BIND-SELF       ;Bind SELF to M-A
        ((M-TEM) ADD (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-QLBNDP)
        ((M-TEM) SUB M-TEM A-QLBNDH)
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-TEM TRAP)
            (ERROR-TABLE PDL-OVERFLOW SPECIAL)
        (JUMP-IF-BIT-SET-XCT-NEXT M-QBBFL BIND-SELF-1)
       ((M-TEM WRITE-MEMORY-DATA) DPB M-MINUS-ONE
               (LISP-BYTE %%SPECPDL-CLOSURE-BINDING) A-SELF)
        ((WRITE-MEMORY-DATA) DPB (M-CONSTANT -1) (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG) A-TEM) ;START NEW BINDING BLOCK
        ((M-QBBFL) DPB (M-CONSTANT -1) A-FLAGS)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX            ;running frame
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
BIND-SELF-1
        ((VMA-START-WRITE) A-QLBNDP M+A+1 M-ZERO)       ;STORE PREVIOUS CONTENTS
        (CHECK-PAGE-WRITE)              ;NO SEQ BRK HERE
        (gc-write-test) ;850503
        ((A-QLBNDP) ADD VMA (A-CONSTANT 1))
        ((A-SELF) Q-TYPED-POINTER M-A)
        ((WRITE-MEMORY-DATA)            ;LOCATIVE POINTER TO BOUND LOCATION
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                  LOWEST-A-MEM-VIRTUAL-ADDRESS
                                  (A-MEM-LOC A-SELF))))
     ;; A-memory is non-volatile, so we don't need to GC-WRITE-TEST here.
        (POPJ-AFTER-NEXT (VMA-START-WRITE) A-QLBNDP)
       (CHECK-PAGE-WRITE)

;Bind SELF-MAPPING-TABLE to M-B
BIND-SELF-MAP
        ((M-TEM) ADD (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-QLBNDP)
        ((M-TEM) SUB M-TEM A-QLBNDH)
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-TEM TRAP)
            (ERROR-TABLE PDL-OVERFLOW SPECIAL)
        (JUMP-IF-BIT-SET-XCT-NEXT M-QBBFL BIND-SELF-MAP-1)
       ((M-TEM WRITE-MEMORY-DATA) DPB M-MINUS-ONE
                 (LISP-BYTE %%SPECPDL-CLOSURE-BINDING) A-SELF-MAPPING-TABLE)
        ((WRITE-MEMORY-DATA) DPB (M-CONSTANT -1) (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG) A-TEM)
                                         ;START NEW BINDING BLOCK
        ((M-QBBFL) DPB (M-CONSTANT -1) A-FLAGS)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX            ;running frame
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
BIND-SELF-MAP-1
        ((VMA-START-WRITE) M+A+1 A-QLBNDP M-ZERO)       ;STORE PREVIOUS CONTENTS
        (CHECK-PAGE-WRITE)              ;NO SEQ BRK HERE
        (gc-write-test) ;850503
        ((A-QLBNDP) ADD VMA (A-CONSTANT 1))
        ((A-SELF-MAPPING-TABLE) Q-TYPED-POINTER M-B)
        ((WRITE-MEMORY-DATA)            ;LOCATIVE POINTER TO BOUND LOCATION
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                  LOWEST-A-MEM-VIRTUAL-ADDRESS
                                  (A-MEM-LOC A-SELF-MAPPING-TABLE))))
     ;; A-memory is non-volatile, so we don't need to GC-WRITE-TEST here.
        (POPJ-AFTER-NEXT (VMA-START-WRITE) A-QLBNDP)
       (CHECK-PAGE-WRITE)

;Calling an instance as a function.  Bind SELF to it, bind its instance-variables
;to its value slots, then call its handler function.
CALL-INSTANCE
        ((VMA-START-READ) M-A)          ;Get instance header
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((M-C) Q-POINTER READ-MEMORY-DATA       ;Get address of instance-descriptor
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-instance-header))
                        trap)
                (ERROR-TABLE DATA-TYPE-SCREWUP DTP-INSTANCE-HEADER)
        ((M-A) VMA)                     ;Possibly-forwarded instance is where inst vars are
        (CALL BIND-SELF)
        ((VMA-START-READ) ADD M-C  ;m-c has dtp-locative
                              (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-BINDINGS)))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ((M-T) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-EQUAL M-T A-V-NIL CALL-INSTANCE-3)        ;() => no bindings
        (call-data-type-not-equal m-t                   ;Other cases not implemented yet
                                  (a-constant (byte-value q-data-type dtp-list))
            trap)
                (ERROR-TABLE DATA-TYPE-SCREWUP %INSTANCE-DESCRIPTOR-BINDINGS)
        ((M-D) Q-POINTER M-A
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTERNAL-VALUE-CELL-POINTER)))
;This loop depends on the fact that the bindings list is cdr-coded,
;and saves time and register-shuffling by not calling CAR and CDR.
;However, it does check to make sure that this assumption is true.
CALL-INSTANCE-1                         ;Bind them up
        ((VMA-START-READ) M-T)          ;Get locative to location to bind
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        (JUMP-DATA-TYPE-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) CALL-INSTANCE-4)
        ((VMA-START-READ M-B) READ-MEMORY-DATA) ;Get current binding
        (CHECK-PAGE-READ)
       ((M-D) ADD M-D (A-CONSTANT 1))   ;Points to next value slot
        (DISPATCH TRANSPORT-BIND-READ-WRITE READ-MEMORY-DATA)
        ((M-TEM) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-EQUAL M-D A-TEM CALL-INSTANCE-2)  ;Already there, avoid re-binding
        (CALL QBND4-CLOSURE)                    ;Bind it up
        ((vma) m-b)
        ((WRITE-MEMORY-DATA-START-WRITE) Q-TYPED-POINTER M-D A-E)
        (CHECK-PAGE-WRITE-BIND)
        (gc-write-test) ;850503
CALL-INSTANCE-2
           ;More bindings if this was CDR-NEXT
           ;only xct-next's if it is jumping back to call-instance-1
        (DISPATCH-xct-next Q-CDR-CODE M-B D-CALL-INSTANCE)
                (ERROR-TABLE DATA-TYPE-SCREWUP CDR-CODE-IN-INSTANCE-BINDINGS)
       ((M-T) ADD M-T (A-CONSTANT 1)) ; skip if dispatch falling through
CALL-INSTANCE-3
        ((VMA-START-READ) ADD M-C (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-FUNCTION)))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        (JUMP-DATA-TYPE-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER))
                CALL-INSTANCE-ARRAY)
        (DISPATCH-XCT-NEXT qmrcl-dispatch md)
       ((M-A) Q-TYPED-POINTER md)

(LOCALITY D-MEM)
(START-DISPATCH 2 0)    ;MAYBE DOES XCT-NEXT
;DISPATCH ON CDR-CODE WHEN AT CALL-INSTANCE-2
;LOOP IF CDR-NEXT, DROP THROUGH IF CDR-NIL, OTHERWISE ERROR
D-CALL-INSTANCE
        (CALL-INSTANCE-1)                       ;CDR-NEXT
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR-ERROR
        (P-BIT INHIBIT-XCT-NEXT-BIT TRAP)       ;CDR-NORMAL
        (P-BIT R-BIT inhibit-xct-next-bit)      ;CDR-NIL
(END-DISPATCH)

(LOCALITY I-MEM)

;Fixnum in flavor's bindings list.
;Its value is the number of instance slots to skip over and not bind.
;We assume that this is not the last element of the bindings list!
CALL-INSTANCE-4
        ((m-d) add output-selector-mask-25 md a-d)
        (JUMP-xct-next CALL-INSTANCE-1)
       ((M-T) ADD M-T (A-CONSTANT 1))

;Here to call an instance whose descriptor's function is an array
;rather than a dtp-select-method.
;We treat the array as a hash array and expect to find in it
;a locative pointing to a function cell.  We call what is in that cell.
;If we don't find the operation in the hash table, we call a method failure function
;from the support vector (SVC-FMETH).

;These arrays are not supposed to be forwarded.
;We make a check to see if it is forwarded; if so, we treat it as a hash failure.
;The hash failure function will eliminate the forwarding and try again.

(ASSIGN %HASH-TABLE-MODULUS -6)

;Enter here if the hash table itself is called by the user as a function.
CALL-HASH-TABLE
 ;      (CALL FINISH-ENTERED-FRAME)     ;Keep active frames and specpdl blocks in phase.
        ((MD) M-A)

;Enter here from calling an instance as a function.
;The hash array is presently in MD, but is moved into M-B, and type DTP-LOCATIVE is added.
;Remember M-R contains number of args and M-S contains the pdl idx of the previous frame,
;and neither may be clobbered.
CALL-INSTANCE-ARRAY
        ((M-B) dpb MD q-pointer (a-constant (byte-value q-data-type dtp-locative)))
        (CALL-EQUAL M-R A-ZERO TRAP)    ;NOT ENUF ARGS
  (ERROR-TABLE ZERO-ARGS-TO-SELECT-METHOD M-A)
        ((VMA-START-READ) ADD M-B (A-CONSTANT %HASH-TABLE-MODULUS)) ;M-B has DTP-LOCATIVE
        (CHECK-PAGE-READ)
;If the modulus is not a fixnum, array is forwarded.
;No need to transport this because we only actually use it if it is a fixnum.
;(also, it's already in the machine so it's not old-space)
        (JUMP-DATA-TYPE-NOT-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                CALL-INSTANCE-HASH-FAILURE)
;M-3 gets the length of the hash table in entries.
;That is a power of 2, we assume.
;M-1 gets a number one less, a mask of bits.
        ((M-3) Q-POINTER MD)
        ((M-1) SUB M-3 (A-CONSTANT 1))
;Look at the array header, and make M-T point at the first word of array contents.
;No need to transport the header; we already know the array isn't forwarded.
        ((VMA-START-READ) M-B) ;M-B has DTP-LOCATIVE
        (CHECK-PAGE-READ)
        ((M-2) (LISP-BYTE %%ARRAY-LONG-LENGTH-FLAG) MD)
        ((M-T) M+A+1 M-B A-2)
;Get the first arg (the operation, used as the hash key) into M-C.
        ((PDL-INDEX) ADD M-AP (A-CONSTANT 1))
        ((M-C) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
;Compute the hash code
;       ((M-2) DPB M-C (LISP-BYTE 0325))
;This change will lose with system 89.
; *** M-1 is assumed to be untagged below ***
;       ((M-2) Q-POINTER M-C (a-constant (byte-value q-data-type dtp-locative)))
;       ((M-1) AND output-selector-mask-25 M-1 A-2)
;;Triple it to get the starting index into the array.
;       ((M-2) ADD output-selector-mask-25 M-1 A-1)
;       ((M-2) ADD output-selector-mask-25 M-1 A-2)
;       ((VMA) ADD output-selector-mask-25 M-T A-2) ;VMA gets DTP-LOCATIVE
        ((M-2) Q-POINTER M-C)
        ((M-1) AND M-1 A-2)
;Triple it to get the starting index into the array.
        ((M-2) ADD M-1 A-1)
        ((M-2) ADD M-1 A-2)
        ((VMA) ADD M-T A-2)
;VMA points to the next hash table entry's key.
;M-1 is the index in the hash table, in units of entries (1/3 of index in words).
CALL-INSTANCE-ARRAY-SEARCH
        ((VMA-START-READ) q-pointer VMA (a-constant (byte-value q-data-type dtp-locative)))
        (CHECK-PAGE-READ)
        ((M-2) Q-TYPED-POINTER MD)
;Jump if find the desired key.
;No need to transport -- if key is in old space, we need to rehash the array,
;which will be done by the hash failure function.
        (JUMP-EQUAL M-2 A-C CALL-INSTANCE-KEY-FOUND)
;Jump if we are sure the key is not to be found.
        (JUMP-EQUAL M-2 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-NULL)) CALL-INSTANCE-HASH-FAILURE)
        ((M-1) ADD M-1 (A-CONSTANT 1))
        (JUMP-LESS-THAN-XCT-NEXT M-1 A-3 CALL-INSTANCE-ARRAY-SEARCH)
       ((VMA) ADD VMA (A-CONSTANT 3)) ;still DTP-LOCATIVE
;At end of hash table, wrap to beginning.
        ((VMA) q-pointer M-T (a-constant (byte-value q-data-type dtp-locative)))
        (JUMP-xct-next CALL-INSTANCE-ARRAY-SEARCH)
       ((M-1) A-ZERO)

CALL-INSTANCE-KEY-FOUND
        ((VMA-START-READ M-B) ADD VMA (A-CONSTANT 1)) ; VMA still has DTP-LOCATIVE
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ;; The array word should be a locative to a cell containing a function.
        ;; Get the function.
        ((VMA-START-READ) MD)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ((M-T) MD)                      ;Put function to call in M-T.
        ((VMA-START-READ) ADD M-B (A-CONSTANT 1))  ;Assumes no ONE-Q-FORWARDs in the hash table.
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ((M-B) Q-TYPED-POINTER MD)
        (JUMP-EQUAL M-B A-V-NIL CSM-6)          ;If non-nil mapping table found,
        (CALL BIND-SELF-MAP)                    ;bind SELF-MAPPING-TABLE to it.
;Bind flag in stack frame saying we have provided the self map;
;but not if the function is a symbol.
        (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)) CSM-6)
        ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-B) PDL-INDEX-INDIRECT)
        ((PDL-INDEX-INDIRECT)
         DPB (LISP-BYTE %%LP-CLS-SELF-MAP-PROVIDED)
         (M-CONSTANT -1) A-B)
        (JUMP CSM-6)                            ;Put function in M-T into stack frame and call.

CALL-INSTANCE-HASH-FAILURE
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-FMETH))
        (DISPATCH-XCT-NEXT qmrcl-dispatch MD)
       ((M-A) Q-TYPED-POINTER MD)

;CALLING ENTITY AS FUNCTION.  BIND SELF THEN TURN INTO CLOSURE CALL.
;DON'T CALL QBND4 TO AVOID REFERENCING A-SELF VIA SLOW VIRTUAL-MEMORY PATH
CALL-ENTITY
   ;    (CALL FINISH-ENTERED-FRAME)     ;Keep active frames and specpdl blocks in phase.
        (CALL BIND-SELF)
        ((MD) DPB M-MINUS-ONE
         (LISP-BYTE %%SPECPDL-CLOSURE-BINDING) A-METHOD-SUBROUTINE-POINTER)
        ((A-QLBNDP) ADD M-ZERO A-QLBNDP ALU-CARRY-IN-ONE)
        ((VMA-START-WRITE) A-QLBNDP)    ;STORE PREVIOUS CONTENTS
        (CHECK-PAGE-WRITE-no-sequence-break)    ;NO SEQ BRK HERE
        (gc-write-test) ;850503
        ((A-QLBNDP) ADD M-ZERO A-QLBNDP ALU-CARRY-IN-ONE)
          ;LOCATIVE POINTER TO BOUND LOCATION
        ((MD) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                LOWEST-A-MEM-VIRTUAL-ADDRESS
                                (A-MEM-LOC A-METHOD-SUBROUTINE-POINTER))))
        ((VMA-START-WRITE) A-QLBNDP)
        (CHECK-PAGE-WRITE-no-sequence-break)
     ;; A-memory is non-volatile, so we don't need to GC-WRITE-TEST here.
        ((MD) DPB M-MINUS-ONE
         (LISP-BYTE %%SPECPDL-CLOSURE-BINDING) A-METHOD-SEARCH-POINTER)
        ((A-QLBNDP) ADD M-ZERO A-QLBNDP ALU-CARRY-IN-ONE)
        ((VMA-START-WRITE) A-QLBNDP)    ;STORE PREVIOUS CONTENTS
        (CHECK-PAGE-WRITE-no-sequence-break)            ;NO SEQ BRK HERE
        (gc-write-test) ;850503
        ((A-QLBNDP) ADD M-ZERO A-QLBNDP ALU-CARRY-IN-ONE)
          ;LOCATIVE POINTER TO BOUND LOCATION
        ((MD) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                LOWEST-A-MEM-VIRTUAL-ADDRESS
                                (A-MEM-LOC A-METHOD-SEARCH-POINTER))))
        ((VMA-START-WRITE) A-QLBNDP)
        (CHECK-PAGE-WRITE-no-sequence-break)
        (gc-write-test)                                ;KHS 860602.
;CALLING CLOSURE AS FUNCTION
QCLS  ; (CALL FINISH-ENTERED-FRAME)     ;Keep active frames and specpdl blocks in phase.

        ((M-T) Q-POINTER M-A (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
                ;Seq brk ok now (can happen here).
#+LAMBDA (DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-T CARCDR-DISPATCH-DIRECT)
#+LAMBDA((m-b) m-a)
#+exp   (open-carcdr-xct-next m-t)
#+exp  ((m-b) m-a)
        ((PDL-INDEX) M-AP)
        ((PDL-INDEX-INDIRECT M-fef) M-A)        ;Replace closure with closed fctn.
     ;; Call APPLY-LAMBDA directly to do its own bindings if closure function is interpreted.
        (jump-data-type-equal m-fef (a-constant (byte-value q-data-type dtp-list))
                                 qcls-interpreted)
        (CALL QCLS1) ; clobbers M-A
        (DISPATCH-XCT-NEXT qmrcl-dispatch M-fef)
       ((M-A) Q-TYPED-POINTER M-fef)

qcls-interpreted
        (jump-xct-next intp1)
       ((c-pdl-buffer-index m-fef) m-b) ;put back closure itself

;M-T has a list of bindings (alternating cell-to-bind and cell-containing-binding).
;Perform the bindings.  Clobbers M-A, M-B, M-TEM, M-E.
QCLS1 (declare (args a-t) (clobbers a-a a-b a-tem a-e))
        (POPJ-EQUAL M-T A-V-NIL)        ;Return if no bindings to do
        (open-carcdr-xct-next m-t)
       (no-op)
        (JUMP-EQUAL M-T A-V-NIL QCLS1-LEXICAL-ENVIRONMENT)
        (open-carcdr-xct-next m-t)
;M-B has locn to bind.
       ((M-B) M-A)
;M-A has value to bind it to.
;Normally only M-A's pointer field matters and it is changed to an EVCP below.
        ((VMA-START-READ) M-B)          ;Get current binding
        (CHECK-PAGE-READ)
        ((M-A) DPB M-A Q-POINTER        ;SWITCH DATA TYPE.. (DOING IT THIS WAY AVOIDS PROBLEMS
                                        ;WITH CAR ABOVE AS WELL AS GENERALLY REDUCING
                                        ;PROFUSION OF FUNNY DATA TYPES)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTERNAL-VALUE-CELL-POINTER)))
        (DISPATCH TRANSPORT-BIND-READ-WRITE MD)
        ((M-TEM) Q-TYPED-POINTER MD)
        (JUMP-EQUAL M-A A-TEM QCLS1)    ;Already there, avoid re-binding.  This saves on
                                        ;special-pdl overflows in recursive message passing.
        (CALL QBND4-CLOSURE)            ;Bind it up
        ((vma) m-b)
        ((WRITE-MEMORY-DATA-START-WRITE) Q-TYPED-POINTER M-A A-E)
        (CHECK-PAGE-WRITE-BIND)
        (gc-write-test) ;850503
        (JUMP QCLS1)

;If cdr of closure binding list is NIL, it means we should bind LEXICAL-ENVIRONMENT
;and the first element of the binding list (in M-A) is the value to bind it to.
;This is used by stack closures to avoid the slowdown to ref it via page fault handler.
QCLS1-LEXICAL-ENVIRONMENT
        (POPJ-EQUAL M-A A-LEXICAL-ENVIRONMENT)

;Bind LEXICAL-ENVIRONMENT to M-A
BIND-LEXICAL-ENVIRONMENT
        ((M-TEM) ADD (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-QLBNDP)
        ((M-TEM) SUB M-TEM A-QLBNDH)
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-TEM TRAP)
            (ERROR-TABLE PDL-OVERFLOW SPECIAL)
        (JUMP-IF-BIT-SET-XCT-NEXT M-QBBFL BIND-LEXICAL-ENVIRONMENT-1)
       ((M-TEM MD) DPB M-MINUS-ONE (LISP-BYTE %%SPECPDL-CLOSURE-BINDING) A-LEXICAL-ENVIRONMENT)
                                                 ;START NEW BINDING BLOCK
        ((MD) DPB (M-CONSTANT -1) (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG) A-TEM)
        ((M-QBBFL) DPB (M-CONSTANT -1) A-FLAGS)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX            ;running frame
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
BIND-LEXICAL-ENVIRONMENT-1
        ((VMA-START-WRITE) M+A+1 A-QLBNDP M-ZERO)       ;STORE PREVIOUS CONTENTS
        (CHECK-PAGE-WRITE-no-sequence-break)            ;NO SEQ BRK HERE
        (gc-write-test) ;850503
        ((A-QLBNDP) ADD VMA (A-CONSTANT 1))
        ((A-LEXICAL-ENVIRONMENT) Q-TYPED-POINTER M-A)
                ;LOCATIVE POINTER TO BOUND LOCATION
        ((MD) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)
                                LOWEST-A-MEM-VIRTUAL-ADDRESS
                                (A-MEM-LOC A-LEXICAL-ENVIRONMENT))))
     ;; A-memory is non-volatile, so we don't need to GC-WRITE-TEST here.
        (POPJ-AFTER-NEXT (VMA-START-WRITE) A-QLBNDP)
       (CHECK-PAGE-WRITE-no-sequence-break)

;; Must not sequence break in this code!  **NOT TRUE, THE CLAIM IS..
;; Because we have already done MLLV or QLLV on the active frame
;; and stack group switch would want to do it again.
;; So the non-sequence-break versions of CDR and MEMQ must be used.

CALL-SELECT-METHOD
        (CALL-EQUAL M-R A-ZERO TRAP)    ;NOT ENUF ARGS
  (ERROR-TABLE ZERO-ARGS-TO-SELECT-METHOD M-A)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT 1))  ;FETCH MESSAGE KEY
        ((M-C) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
        ((M-B) A-V-NIL)         ;HOLDS CONSTANT ON M-SIDE, FOR EASY COMPARISON
        ((M-T) DPB M-A Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        (JUMP-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)) CSM-R)  ;RESUME
        ((A-METHOD-SUBROUTINE-POINTER) A-V-NIL) ;"SUBROUTINE" CONTINUATION POINT,
                                                ;  OR NIL IF AT TOP LEVEL.
CSM-3
        (open-qcar-xct-next m-t)
       ((PDL-PUSH) M-T)

;M-T HAS ASSQ-LIST ELEMENT
;NOT METHOD-KEY, METHOD PAIR
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) csm-1)
        (open-qcar-xct-next m-t)
       ((M-J) M-T)
        (JUMP-EQUAL M-T A-C CSM-2)              ;FOUND IT
;ASSQ KEY A LIST, DO MEMQ ON IT
        (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)) CSM-7)
CSM-5
        (open-qcdr-xct-next pdl-top)
       ((M-T) Q-TYPED-POINTER PDL-POP)
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list)) csm-3)
        (JUMP-NOT-EQUAL M-B A-METHOD-SUBROUTINE-POINTER CSM-8A)   ;IF IN SUBROUTINE, RETURN.
        (JUMP-NOT-EQUAL M-T A-V-NIL CSM-6)
  ;NON-NIL TERMINATION IS SUPERCLASS POINTER.
  ; USE IT TO REPLACE DTP-SELECT-METHOD AND REINVOKE. THE TWO COMMON CASES ARE (1) THIS SYMBOL
  ; IS A SUPERCLASS POINTER AND IT'S FUNCTION CELL CONTAINS A DTP-SELECT-METHOD.  THE SEARCH
  ; WILL CONTINUE. (2)  THIS SYMBOL IS A LISP FUNCTION AND WILL GET CALLED IN THE USUAL WAY.
  ; THIS SERVES AS AN "OTHERWISE" CLAUSE.
        (CALL TRAP)                     ;SELECTED METHOD NOT FOUND
  (ERROR-TABLE SELECTED-METHOD-NOT-FOUND M-A M-C)

CSM-R   (JUMP-XCT-NEXT CSM-5)               ;RESUME SEARCH AT SAVED POINT
       ((PDL-PUSH) A-METHOD-SEARCH-POINTER)  ;PUT IT WHERE CSM-5 EXPECT IT.

CSM-7   ((PDL-PUSH) M-A)                ;ASSQ key is a list, so do MEMQ on it.
        (CALL XMEMQ1-NO-SB)             ; Takes args in M-C, M-T.  Clobbers M-A.
        (JUMP-EQUAL-XCT-NEXT M-T A-V-NIL CSM-5)
       ((M-A) PDL-POP)                  ;Restore M-A
CSM-2   ((A-METHOD-SEARCH-POINTER) PDL-POP) ;SAVE IN CASE METHOD SEARCH IS RESUMED.

;FOUND DESIRED METHOD KEY.  GET ASSOC FCTN
        (open-qcdr-xct-next m-j)
       ((M-T) Q-TYPED-POINTER M-J)         ; FROM ASSQ ELEMENT.
CSM-6   ((PDL-INDEX) M-AP)
        ((PDL-INDEX-INDIRECT M-A) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT  ;CLOBBER INTO
                Q-ALL-BUT-TYPED-POINTER A-T)    ; LP-FEF SLOT, REPLACING DTP-SELECT-METHOD
        (DISPATCH-XCT-NEXT qmrcl-dispatch M-A)
       ((m-fef) m-a)

XMEMQ1-NO-SB (declare (args a-t a-c) (clobbers a-a a-d) (values a-t))
        (POPJ-EQUAL M-T A-V-NIL)
        (CALL-XCT-NEXT CARCDR-NO-SB)    ;Get car in M-A, cdr in M-T.
       ((M-D) M-T)                      ;Save this link, as value if this elt matches.
        (JUMP-NOT-EQUAL M-A A-C XMEMQ1-NO-SB)
        (POPJ-XCT-NEXT)
       ((M-T) M-D)


(begin-comment) zwei-lossage (end-comment)

;GET HERE IF SELECT-METHOD LIST-ELEMENT NOT A CONS.
CSM-1
        (CALL-DATA-TYPE-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)) TRAP)
 (ERROR-TABLE SELECT-METHOD-GARBAGE-IN-SELECT-METHOD-LIST M-T)
   ;DO A ONE LEVEL "SUBROUTINE" CALL.  SAVE CONTINUATION POINTER IN M-B.
        (JUMP-NOT-EQUAL M-B A-METHOD-SUBROUTINE-POINTER CSM-8) ;ALREADY IN A SUBROUTINE, RETURN
        ((A-METHOD-SUBROUTINE-POINTER) PDL-POP)    ;SAVE CONTINUATION POINT.
        ((VMA-START-READ) ADD output-selector-mask-25
                M-T (A-CONSTANT (plus (byte-value q-data-type dtp-locative) 2)))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
 ;NO METHODS IN THIS CLASS,
 ; IMMEDIATELY RETURN.
        (JUMP-DATA-TYPE-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)) CSM-8A)
        (CALL-DATA-TYPE-NOT-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SELECT-METHOD))
                TRAP)
 (ERROR-TABLE SELECT-METHOD-BAD-SUBROUTINE-CALL M-A)
        (JUMP-XCT-NEXT CSM-3)
       ((M-T) LDB Q-POINTER MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))

;HERE IF IN A SUBROUTINE, BUT DIDNT FIND IT.  RETURN FROM SUBROUTINE AND CONTINUE.
CSM-8   ((M-GARBAGE) PDL-POP)
CSM-8A  ((PDL-PUSH) A-METHOD-SUBROUTINE-POINTER)  ;PUT CONTINUATION
        (JUMP-XCT-NEXT CSM-5)                           ; WHERE IT IS EXPECTED.
       ((A-METHOD-SUBROUTINE-POINTER) A-V-NIL)          ;AT TOP LEVEL AGAIN.

;;; Frame-leaving routines.  Save appropriate state in the call-block.

;"Micro" leave.  This is the same as the normal frame leave except that
;it checks for any micro-stack which needs to be saved; if so it is
;transferred to the special-pdl and %%LP-EXS-MICRO-STACK-SAVED is set.
;However, the top entry on the micro-stack is the return from MLLV and
;is not saved.
;We always go to QLLV-NOT-TAIL-REC because, in the cases where MLLV is used,
;flushing the frame being left may not work (eg, stack group switching).
;MLLV   ((M-TEM) MICRO-STACK-POINTER)
;       (JUMP-EQUAL M-TEM (A-CONSTANT 1) QLLV-NOT-TAIL-REC)     ;Jump if nothing to save
;       ((M-2) MICRO-STACK-DATA-POP)    ;Get real return off micro-stack
;       ((M-1) ADD (M-CONSTANT 40) A-QLBNDP)    ;TEST P.C.E. (THIS M-CONST JUST HAPPENED TO
;       ((M-1) SUB M-1 A-QLBNDH)                ; BE AROUND AT THE WRONG TIME).
;       (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 TRAP)
;  (ERROR-TABLE PDL-OVERFLOW SPECIAL)           ;M-1 should be negative as 24-bit quantity
;       ((M-Q) DPB (M-CONSTANT -1)      ;First Q in block has flag bit
;               (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG)
;               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;MLLV1  ((WRITE-MEMORY-DATA) MICRO-STACK-DATA-POP A-Q)  ;Note- this involves a LDB operation
;       ((A-QLBNDP) ADD A-QLBNDP M-ZERO ALU-CARRY-IN-ONE)
;       ((VMA-START-WRITE) A-QLBNDP)
;       (CHECK-PAGE-WRITE)
;       ((M-TEM) MICRO-STACK-POINTER)   ;Loop if not done
;       (JUMP-NOT-EQUAL-XCT-NEXT M-TEM A-ZERO MLLV1)
;       ;Remaining Q's in block do not have flag bit
;       ((M-Q) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
;       ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT
;               (A-CONSTANT (BYTE-MASK %%LP-EXS-MICRO-STACK-SAVED)))
;  ;set attention in frame being created, NOT running frame.  This is because
;  ;we want this stuff to get popped when frame we are creating now returns.
;  ;it sort of makes sense if you consider the MICRO-STACK-SAVED to be part of the
;  ;PC and thus should be stored in the running frame.
;       ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL %LP-CALL-STATE)))
;       ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX
;                          (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
;       (JUMP-XCT-NEXT QLLV-NOT-TAIL-REC)
;       ((MICRO-STACK-DATA-PUSH) M-2)   ;Push back return address, drop into QLLV

;;Leave a frame when we're running just macrocode, and no micro-stack needs to be saved.
;;This routine saves and clears M-QBBFL, and saves the LC (even if the current frame
;;is not a FEF frame; in that case it won't be looked at).
;QLLV   ((M-TEM) A-V-NIL)                       ;Bletcherous 2-instruction test.
;       (JUMP-NOT-EQUAL A-TAIL-RECURSION M-TEM QLLV-TAIL-REC)
;;Re-enter here if tail recursion is not happening now.
;;Enter here from MLLV.
;QLLV-NOT-TAIL-REC
;       ((PDL-INDEX) M-AP)              ;Must save LC as half-word offset from FEF
;       ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH 2)
;                (A-CONSTANT 0))        ;Shift 2 to align with location counter
;       ((M-TEM) SUB LOCATION-COUNTER A-TEM1 OUTPUT-SELECTOR-RIGHTSHIFT-1) ;Relative PC (hwds)
;       ;; Build exit-state word from PC, M-FLAGS, and previous contents
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
;       ((m-TEM1) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT (BYTE-FIELD 21 17) A-TEM)
;                       ;CODE KNOWS THAT %%LP-EXS-EXIT-PC IS 0017
;       (POPJ-AFTER-NEXT                        ;Save M-QBBFL then clear it
;        (PDL-INDEX-INDIRECT) DPB M-FLAGS (LISP-BYTE %%LP-EXS-PC-STATUS) A-TEM1)
;       ((M-FLAGS) SELECTIVE-DEPOSIT M-FLAGS M-FLAGS-EXCEPT-PROCESSOR-FLAGS A-ZERO)

;QLLV-TAIL-REC
;       (JUMP-IF-BIT-SET M-QBBFL QLLV-NOT-TAIL-REC)
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
;       (JUMP-IF-BIT-SET (LISP-BYTE %%LP-ENS-UNSAFE-REST-ARG) PDL-INDEX-INDIRECT
;               QLLV-NOT-TAIL-REC)
;       ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL %LP-CALL-STATE)))
;       ((M-TEM) (LISP-BYTE %%LP-CLS-DESTINATION) PDL-INDEX-INDIRECT)
;       (JUMP-NOT-EQUAL M-TEM (A-CONSTANT D-RETURN) QLLV-NOT-TAIL-REC)
;QLLV-TAIL-REC-0
;;It is a call with D-RETURN, and no specials are bound.
;;Are there any catches in the frame that is to be thrown away?
;       ((m-TEM1) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-U-ENTRY)
;                                         *CATCH-U-CODE-ENTRY-/#)))
;       ;; Extract just four specific bits from called frame call state.
;       ((M-K) AND PDL-INDEX-INDIRECT
;        (A-CONSTANT (PLUS (BYTE-VALUE %%LP-CLS-ATTENTION 1)
;                          (BYTE-VALUE %%LP-CLS-TRAP-ON-EXIT 1)
;                          (BYTE-VALUE %%LP-CLS-SELF-MAP-PROVIDED 1)
;                          (BYTE-VALUE %%LP-CLS-ADI-PRESENT 1))))
;;Look at frame being called and each open frame within that frame.
;;PDL-INDEX points to the %LP-CALL-STATE word of an open frame, here.
;QLLV-TAIL-REC-1
;       ((M-TEM) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) PDL-INDEX-INDIRECT)
;       ;; PDL-INDEX gets the function pointer word of the same frame.
;       ((PDL-INDEX) SUB PDL-INDEX (A-CONSTANT (EVAL %LP-CALL-STATE)))
;       ;; If the function is *CATCH, we cannot discard the open frame.
;       ((MD) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
;       (JUMP-EQUAL MD A-TEM1 QLLV-NOT-TAIL-REC)
;       ;; Get address of next frame out.
;       ((PDL-INDEX) SUB PDL-INDEX A-TEM)
;       ;; If we come to the currently active frame (M-AP), there are no catches in it.
;       (JUMP-NOT-EQUAL-XCT-NEXT PDL-INDEX A-AP QLLV-TAIL-REC-1)
;       ((PDL-INDEX) ADD PDL-INDEX (A-CONSTANT (EVAL %LP-CALL-STATE)))
;;; This is a tail recursive call from a frame with no specials and no catches.
;;; Copy the frame being entered down on top of the one that was being left.
;;; First clear the bit saying that the self-map was provided to the calling function,
;;; because that does not mean it was provided to the called function.
;;; PDL-INDEX already points to the frame's call-state word.
;;; Also or together the trap-on-exit bits of the two frames.
;       (CALL-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%LP-CLS-ADI-PRESENT) M-K
;           QLLV-TAIL-REC-ADI)
;       ((M-TEM) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;;; M-TEM now has the data on type of call, to put in the frame's entry state.
;;; Now merge the two call states: M-K has relevant info from the called frame's call state.
;       ((MD) ANDCA PDL-INDEX-INDIRECT
;                (A-CONSTANT (BYTE-VALUE %%LP-CLS-SELF-MAP-PROVIDED 1)))
;       ((PDL-INDEX-INDIRECT) IOR MD A-K)
;;; We only need to copy the frame contents; the call state has just been merged,
;;; and the entry state of the frame being moved has not been set up yet.
;       ((m-TEM1) PDL-POINTER)
;       ((PDL-POINTER) SUB M-AP (A-CONSTANT 1))
;       ((PDL-INDEX) SUB M-S (A-CONSTANT 1))
;       ((PDL-TOP) M-TEM)
;QLLV-TAIL-REC-COPY
;       ((PDL-INDEX) ADD PDL-INDEX (A-CONSTANT 1))
;       (JUMP-NOT-EQUAL-XCT-NEXT PDL-INDEX A-TEM1 QLLV-TAIL-REC-COPY)
;       ((PDL-PUSH) PDL-INDEX-INDIRECT)
;       (POPJ-AFTER-NEXT
;        (A-IPMARK) M-AP)
;;Update the address of the frame to be entered.
;       ((M-S) M-AP)

;;Set up M-TEM with the %%LP-ENS-ADI-TYPE field.
;;Sets the pdl index back to the call state word of the calling frame,
;;which is what we assume it was when we were called.
;QLLV-TAIL-REC-ADI
;       ((M-K) DPB M-ZERO (LISP-BYTE %%LP-CLS-ADI-PRESENT) A-K) ;Don't set ADI-PRESENT in calling frame.
;       ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL (1- %LP-CALL-STATE))))
;       (POPJ-AFTER-NEXT
;        (M-TEM) SELECTIVE-DEPOSIT (LISP-BYTE %%ADI-TYPE) PDL-INDEX-INDIRECT A-TEM)
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))

;XPERMIT-TAIL-RECURSION (MISC-INST-ENTRY %PERMIT-TAIL-RECURSION)
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
;       (POPJ-AFTER-NEXT
;        (M-A) PDL-INDEX-INDIRECT)
;       ((PDL-INDEX-INDIRECT) DPB M-ZERO (LISP-BYTE %%LP-ENS-UNSAFE-REST-ARG) A-A)

;Get here when resuming a stack group whose active frame is a FEF.
;Restore M-INST-BUFFER and A-LOCALP.
;Dont restore M-FLAGS, etc, because that is handled by SG resume mechanism.
QLLENT  (declare (clobbers a-1 a-tem a-tem1))
        ((m-1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1)
                   (A-CONSTANT 0))  ;SET UP FROM M-AP.  SHIFT TO BYTE ALIGN
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
                                                 ;RELATIVE PC IN BYTES
        ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD 17 #+lambda 1 #+exp 0) A-ZERO)
                        ;CODE KNOWS THAT %%LP-EXS-EXIT-PC IS 0017
        ((LOCATION-COUNTER) ADD m-1 A-TEM1)     ;RESTORE LC
        ((LOCATION-COUNTER) SUB LOCATION-COUNTER (A-CONSTANT #+lambda 2 #+exp 1))
                        ;IT IS NECESSARY THAT
                        ;M-INSTRUCTION-BUFFER ACTUALLY HAVE THE LAST INSTRUCTION
                        ;EXECUTED (IE NOT SUFFICIENT MERELY THAT THE CORRECT INSTRUCTION
                        ;WILL BE FETCHED NEXT TIME AROUND THE MAIN LOOP).  THIS IS BECAUSE
                        ;THE CURRENT MACRO-INSTRUCTION, WHICH MAY BE BEING REENTERED
                        ;IN THE MIDDLE, CAN DISPATCH AGAIN ON M-INSTRUCTION-STREAM
                        ;(TO GET THE DESTINATION IN MISC, FOR EXAMPLE).  THE SIMPLEST
                        ;WAY TO ASSURE THIS IS TO BACK UP THE LOCATION COUNTER AND
                        ;RE-ADVANCE IT.
        (DISPATCH ADVANCE-INSTRUCTION-STREAM)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        (POPJ-AFTER-NEXT                        ;START INSTRUCTION FETCH, GET LOCAL BLOCK
         (M-TEM) (LISP-BYTE %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN) PDL-INDEX-INDIRECT)
       ((A-LOCALP) ADD M-AP A-TEM)

;DTP-U-ENTRY turned out not to be microcoded.  Snap it out, and try again.
QME2    ((PDL-INDEX) M-AP)
        ((PDL-INDEX-INDIRECT M-A) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT  ;CLOBBER INTO
                Q-ALL-BUT-TYPED-POINTER A-T)    ; LP-FEF SLOT, REPLACING DTP-U-ENTRY
        (DISPATCH-XCT-NEXT qmrcl-dispatch M-A)
       ((m-fef) m-a)

;Enter micro-code entry function, called by XXX-TO-MACRO call.
; M-S has new value for M-AP, 0(M-S) is function being
; called (also in M-A), 1(M-S) is 1st arg, 2(M-S) is 2nd, etc.
; Calling function has been left.  M-R has number of args.

QME1    ((M-D) Q-POINTER M-A A-AMCENT) ;a-amcent has DTP-FIX
        (CALL-GREATER-OR-EQUAL M-D A-AMCENT TRAP)       ;OUT OF RANGE
  (ERROR-TABLE MICRO-CODE-ENTRY-OUT-OF-RANGE M-D)
;IF THIS A FIXNUM, ITS
;INDEX TO MICRO-CODE-SYMBOL-AREA.  OTHERWISE, FCTN
;IS NOT REALLY MICROCODED NOW, AND THIS IS OTHER DEF.
;IF SO, PUT THIS IN LP-FEF SLOT AND TRY AGAIN.
        ((VMA-START-READ) ADD output-selector-mask-25
                 M-D A-V-MICRO-CODE-ENTRY-AREA)
        (CHECK-PAGE-READ)
        (dispatch transport md) ;gak!
        ((M-ERROR-SUBSTATUS) A-ZERO)
        ((M-T) MD)
        (JUMP-DATA-TYPE-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) QME2)
        ((VMA-START-READ) ADD output-selector-mask-25 M-D A-V-MICRO-CODE-ENTRY-ARGS-INFO-AREA)
        (CHECK-PAGE-READ)
        ((M-TEM) (LISP-BYTE %%ARG-DESC-MIN-ARGS) READ-MEMORY-DATA)
        (CALL-GREATER-THAN M-TEM A-R SET-TOO-FEW-ARGS)
        ((M-TEM) (LISP-BYTE %%ARG-DESC-MAX-ARGS) READ-MEMORY-DATA)
        (CALL-LESS-THAN M-TEM A-R SET-TOO-MANY-ARGS)
                        ;NOTE, THIS DOESN'T CHECK FOR LEXPR/FEXPR CALL.
                        ;WE DO PROVIDE FOR MICROCODED FUNCTIONS WITH VARIABLE NUMBER
                        ;OF ARGS, WHICH ARE LEGAL PROVIDED THEY ARE NOT MISC INSTRUCTIONS.
        ((VMA-START-READ) ADD output-selector-mask-25
               M-T A-V-MICRO-CODE-SYMBOL-AREA)  ;M-T HAS DATA READ FROM
        (CHECK-PAGE-READ)                       ;MICRO-CODE-ENTRY-AREA
QME1A   (JUMP-IF-BIT-SET M-TRAP-ON-CALLS QME1-QMRCL-TRAP)
qme1a-r (JUMP-NOT-EQUAL M-ERROR-SUBSTATUS A-ZERO QLEERR) ;SIGNAL WRONG NUMBER OF ARGS ERROR
        ((OA-REG-LOW M-LAST-MICRO-ENTRY) DPB READ-MEMORY-DATA OAL-JUMP A-ZERO)
        ;; Drop into MISC-TO-RETURN.  Calls the micro-entry function with a return
        ;; address of QMDDR.  Upon return the frame will be flushed.
        ;; This return address of QMDDR causes multiple-values to work right.

MISC-TO-RETURN
        (CALL 0)                                ;CALL MISC FUNCTION, DROP INTO QMDDR

;;; DESTINATION RETURN  value in M-T.  Q-ALL-BUT-TYPED-POINTER bits must be 0.
QMDDR

FAST-QMDDR
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
;       (jump-if-bit-set (lisp-byte %%lp-cls-attention) pdl-index-indirect attention-check-ok)
;  ;attention bit not set, none of the following conditions should be true.
;       (call-if-bit-set (LISP-BYTE %%LP-CLS-TRAP-ON-EXIT) PDL-INDEX-INDIRECT ILLOP-debug)
;       (CALL-IF-BIT-SET (LISP-BYTE %%LP-CLS-ADI-PRESENT) PDL-INDEX-INDIRECT ILLOP-debug)
;       ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) PDL-INDEX-INDIRECT)
;       (CALL-EQUAL M-ZERO A-TEM1 ILLOP-debug)

;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
;       (CALL-IF-BIT-SET (LISP-BYTE %%LP-EXS-MICRO-STACK-SAVED) PDL-INDEX-INDIRECT ILLOP-debug)
;       (CALL-IF-BIT-SET M-QBBFL ILLOP-debug)

;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
;       (CALL-IF-BIT-SET (LISP-BYTE %%LP-ENS-ENVIRONMENT-POINTER-POINTS-HERE)
;                        PDL-INDEX-INDIRECT
;                        ILLOP-debug)
;       (CALL-IF-BIT-SET (LISP-BYTE %%METER-FUNCTION-ENTRY-EXIT-ENABLE)
;               M-METER-ENABLES ILLOP-debug)
;    ;  (CALL-NOT-EQUAL M-AP A-IPMARK ILLOP-debug)  ;this check could lose, yep, fraid it does

;attention-check-ok

;;; DESTINATION RETURN  value in M-T.  Q-ALL-BUT-TYPED-POINTER bits must be 0.
;FAST-QMDDR
;;; No more dtp-stack-closures. ~jrm
;       (CALL-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-STACK-CLOSURE))
;                   STACK-CLOSURE-RETURN-TRAP)   ;do this first because it can result
;               ;in attention getting set in current frame!.
;       (check-data-type-call-equal m-t m-tem dtp-stack-closure stack-closure-return-trap)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-C) PDL-INDEX-INDIRECT)
        (JUMP-IF-BIT-SET (LISP-BYTE %%LP-CLS-ATTENTION) PDL-INDEX-INDIRECT QMDDR-SLOW)
        ((PDL-POINTER) SUB M-AP (A-CONSTANT (EVAL %LP-CALL-BLOCK-LENGTH))) ;FLUSH PDL
        ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) M-C)
#+exp   ((M-TEM) SUB M-AP A-TEM1)               ;COMPUTE PREV A-IPMARK
#+exp   ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-TEM)      ;RESTORE THAT
#+LAMBDA((A-IPMARK) SUB OUTPUT-SELECTOR-MASK-11 M-AP A-TEM1)            ;COMPUTE PREV A-IPMARK
        ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) M-C)
#+exp   ((PDL-INDEX) SUB M-AP A-TEM1)   ;RESTORE M-AP
#+exp   ((M-AP) PDL-INDEX)              ;THIS OPERATION MASKS M-AP TO 10 BITS.
#+LAMBDA((M-AP PDL-INDEX) SUB OUTPUT-SELECTOR-MASK-11 M-AP A-TEM1)      ;RESTORE M-AP
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM1)
        ;; Make sure frame being returned to is in the pdl buffer
        (CALL-LESS-THAN M-PDL-BUFFER-ACTIVE-QS
                        (A-CONSTANT PDL-BUFFER-LOW-WARNING) PDL-BUFFER-REFILL)
        ;; Now restore the state of the frame being returned to.  We will restore
        ;; the FEF stuff even if it's not a FEF frame, at the cost of a slight
        ;; amount of time.

        ((M-A) Q-POINTER PDL-INDEX-INDIRECT)    ;FUNCTION RETURNING TO
  ;** speed this up, go directly to m-fef and use it from there.
        ((m-fef) pdl-index-indirect)            ;do this after pdl-buffer-refill.
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-TEM) (LISP-BYTE %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN) PDL-INDEX-INDIRECT)
        ((A-LOCALP) ADD M-AP A-TEM)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((M-FLAGS) (LISP-BYTE %%LP-EXS-PC-STATUS) PDL-INDEX-INDIRECT A-FLAGS)

                                ;FEF address in bytes
        ((M-TEM) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1) (A-CONSTANT 0))
        ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD 17 #+lambda 1 #+exp 0) A-ZERO)
                        ;CODE KNOWS THAT %%LP-EXS-EXIT-PC IS 0017
        ((LOCATION-COUNTER) ADD M-TEM A-TEM1)
F-QIMOVE-EXIT   ;Store into destination in M-C.  Could be D-MICRO
        (DISPATCH (LISP-BYTE %%LP-CLS-DESTINATION) M-C QMDTBD)
       ((PDL-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
                        (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))

QMDDR-SLOW
        (JUMP-NOT-EQUAL M-AP A-IPMARK QMDDR-THROW) ;CHECK FOR UNWIND-PROTECT
QMDDR0  ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-C) PDL-INDEX-INDIRECT)
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-CLS-TRAP-ON-EXIT) PDL-INDEX-INDIRECT QMEX1-TRAP)
        (CALL-IF-BIT-SET M-QBBFL BBLKP)         ;POP BINDING BLOCK (IF STORED ONE)
QMEX1A
;;; No more dtp-stack-closures. ~jrm
;       (CALL-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-STACK-CLOSURE))
;                   STACK-CLOSURE-RETURN-TRAP)
;       ((PDL-INDEX) M-AP)              ;Save returning function for metering
;       ((M-A) PDL-INDEX-INDIRECT)
        ((m-a) m-fef)           ;** use directly
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-ENS-ENVIRONMENT-POINTER-POINTS-HERE)
                         PDL-INDEX-INDIRECT
                         QMEX1-COPY)
        ;;*** next 2 instructions are temporary
 ;      ((M-TEM) MICRO-STACK-POINTER)
 ;      (CALL-NOT-EQUAL M-TEM (A-CONSTANT 0) ILLOP-DEBUG)
        ;;*** end of temporary code
        ((PDL-POINTER) SUB M-AP (A-CONSTANT (EVAL %LP-CALL-BLOCK-LENGTH))) ;FLUSH PDL
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-CLS-ADI-PRESENT) M-C QRAD1)  ;FLUSH ADDTL INFO
        ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) M-C)
        (JUMP-EQUAL M-ZERO A-TEM1 QMXSG)        ;RETURNING OUT TOP OF STACK-GROUP
#+exp   ((M-TEM) SUB M-AP A-TEM1)               ;COMPUTE PREV A-IPMARK
#+exp   ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-TEM)      ;RESTORE THAT
#+LAMBDA((A-IPMARK) SUB OUTPUT-SELECTOR-MASK-11 M-AP A-TEM1)            ;COMPUTE PREV A-IPMARK
        ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) M-C)
#+exp   ((PDL-INDEX) SUB M-AP A-TEM1)   ;RESTORE M-AP
#+exp   ((M-AP) PDL-INDEX)              ;THIS OPERATION MASKS M-AP TO 10 BITS.
#+LAMBDA((M-AP PDL-INDEX) SUB OUTPUT-SELECTOR-MASK-11 M-AP A-TEM1)      ;RESTORE M-AP
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM1)
        ;; Make sure frame being returned to is in the pdl buffer
        (CALL-LESS-THAN M-PDL-BUFFER-ACTIVE-QS
                        (A-CONSTANT PDL-BUFFER-LOW-WARNING) PDL-BUFFER-REFILL)
        ((m-fef) pdl-index-indirect)    ;do this after pdl-buffer refilled.
        ;; Now restore the state of the frame being returned to.  We will restore
        ;; the FEF stuff even if it's not a FEF frame, at the cost of a slight
        ;; amount of time.
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-FUNCTION-ENTRY-EXIT-ENABLE)
                M-METER-ENABLES METER-FUNCTION-EXIT)
        ((M-A) Q-POINTER PDL-INDEX-INDIRECT)    ;FUNCTION RETURNING TO
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-TEM) (LISP-BYTE %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN) PDL-INDEX-INDIRECT)
        ((A-LOCALP) ADD M-AP A-TEM)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((M-FLAGS) (LISP-BYTE %%LP-EXS-PC-STATUS) PDL-INDEX-INDIRECT A-FLAGS)
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-EXS-MICRO-STACK-SAVED) PDL-INDEX-INDIRECT QMMPOP)
                                ;FEF address in bytes
        ((M-TEM) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1) (A-CONSTANT 0))
        ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD 17 #+lambda 1 #+exp 0) A-ZERO)
                        ;CODE KNOWS THAT %%LP-EXS-EXIT-PC IS 0017
        ((LOCATION-COUNTER) ADD M-TEM A-TEM1)
QIMOVE-EXIT     ;Store into destination in M-C.  Could be D-MICRO
        (DISPATCH (LISP-BYTE %%LP-CLS-DESTINATION) M-C QMDTBD)
       ((PDL-PUSH) DPB M-T Q-ALL-BUT-CDR-CODE
                        (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))


;Here from THROW - BBLKP should not be called.
;It is certain that no trap on exit is needed
;since they are not done on *CATCHes.
QMEX1   ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (JUMP-XCT-NEXT QMEX1A)
       ((M-C) PDL-INDEX-INDIRECT)

;If we trap from QME1A, we need to "preserve" MD
;by reloading it after we get back from the trap.
QME1-QMRCL-TRAP
        ((VMA) Q-POINTER VMA)
        (CALL QMRCL-TRAP-1)
        ;VMA not restored as par of SG anymore,
        ; so recompute it like just before qme1a
        ; (can't just jump back there since we mustn't recheck trap on calls)
        ((VMA-START-READ) ADD output-selector-mask-25
               M-T A-V-MICRO-CODE-SYMBOL-AREA)
        (CHECK-PAGE-READ)
        (jump qme1a-r)

;;Here from QMDDR if data type of M-T is DTP-STACK-CLOSURE.
;;Copy the closure into the heap, in case the frame it is in
;;is about to go away.
;;; No more dtp-stack-closure ~jrm
;STACK-CLOSURE-RETURN-TRAP
;       ((md) m-t)
;        ((vma-start-write) (a-constant (eval (+ 400 %sys-com-temporary))))
;        (illop-if-page-fault)
;       (gc-write-test)
;        (popj-after-next
;          (m-t) md)
;       (no-op)

;;; M-A has the function returning from
METER-FUNCTION-EXIT
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-FUNCTION-EXIT-EVENT)))
        ((PDL-PUSH) M-A)
        (JUMP-XCT-NEXT METER-MICRO-WRITE-HEADER)
       ((A-METER-LENGTH) (A-CONSTANT 1))        ;Number of meters pushed

;This is here so I can put breakpoints before and after trapping.
QMEX1-TRAP
        ((VMA) A-ZERO)          ;Avoid illop due to pointer not in any region,
        ((M-Q) A-ZERO)          ;which seems frequently to be true of VMA at QMEX1.
        (CALL TRAP)
    (ERROR-TABLE EXIT-TRAP)
        (POPJ)

;Copy the lexical frame of the stack frame being exited.
;Called if the bit is set saying that the lexical frame is needed.

;Must preserve M-A and M-C, as well as M-T (the returned value).
; M-A has FEF, **could be bummed**
; M-C has call-state pdl word.
QMEX1-COPY
        ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-A)
        (call-xct-next closure-prepare-to-pop-stack-frame)
       ((PDL-PUSH) M-C)

        ((M-C) PDL-POP)
        (POPJ-AFTER-NEXT
         (M-A) PDL-POP)
       ((M-T) PDL-POP)


;Restore the micro-stack from the binding stack
QMMPOP  ((PDL-INDEX-INDIRECT) ANDCA PDL-INDEX-INDIRECT  ;Clear flag since flushing saved stack
                (A-CONSTANT (BYTE-MASK %%LP-EXS-MICRO-STACK-SAVED)))
        ((M-S) MICRO-STACK-DATA-POP)            ;Pop off return
QMMPO2  ((VMA-START-READ) A-QLBNDP)             ;No transport, known to be a fixnum
        (CHECK-PAGE-READ)                       ;Bind stack not really consistent, no seq brk
        ((A-QLBNDP) SUB VMA (A-CONSTANT 1))
        ((MICRO-STACK-DATA-PUSH) READ-MEMORY-DATA)
        (CALL-DATA-TYPE-NOT-EQUAL READ-MEMORY-DATA (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                ILLOP)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG) READ-MEMORY-DATA QMMPO2)      ;Jump if not last
        ((OA-REG-LOW) DPB M-S OAL-JUMP A-ZERO)
        (JUMP 0)

;GET HERE WHEN RETURNING OUT TOP OF STACK GROUP
QMXSG   ((PDL-POINTER) M-AP)    ;AVOID GROSS SCREW WHERE P-F ROUTINES GET CONFUSED
                ;ABOUT WHATS IN THE PDL-BUFFER DUE TO FACT PDL-POINTER WAS DECREMENTED
                ;TO BEFORE ACTIVE CALL BLOCK (IE 1777 IF SG STARTED OFF AT 0@P)
        ((VMA) A-QCSTKG)        ;ERROR CHECK TO SEE IF DELTA S SCREWWED OR SOMETHING
        ((VMA-START-READ) SUB VMA
                 (A-CONSTANT (PLUS 2 (EVAL SG-INITIAL-FUNCTION-INDEX))))
        (CHECK-PAGE-READ)
        ((A-SG-TEM) M-T)        ;VALUE GETTING RETURNED
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ((PDL-INDEX) ADD READ-MEMORY-DATA A-PDL-BUFFER-HEAD)
        (CALL-NOT-EQUAL PDL-INDEX A-AP ILLOP)
        (JUMP-XCT-NEXT SG-RETURN-2)     ;RETURN THIS LAST VALUE AND GO TO EXHAUSTED STATE
       ((M-TEM) (A-CONSTANT (EVAL SG-STATE-EXHAUSTED)))


;STORE LAST VALUE IN ADI CALL, FLUSH ADI FROM PDL
;MAY CLOBBER ALL REGISTERS EXCEPT M-C and M-A
QRAD1   ((PDL-POINTER) M-AP)    ;IN CASE WE SWITCH STACK GROUPS INSIDE MVR
        ((M-K) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((PDL-INDEX) SUB M-K A-PDL-BUFFER-HEAD)
        ((M-K) ADD PDL-INDEX A-PDL-BUFFER-VIRTUAL-ADDRESS)
        (CALL-XCT-NEXT MVR)     ;STORE THE LAST VALUE INTO MV IF ANY
       ((M-S) (A-CONSTANT 2))

QRAD1R  (declare (clobbers a-k))
        ((PDL-INDEX M-K) SUB M-AP
                (A-CONSTANT (PLUS 1 (EVAL %LP-CALL-BLOCK-LENGTH)))) ;FLUSH ADI FROM PDL
QRAD2   (POPJ-IF-BIT-CLEAR-XCT-NEXT (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) PDL-INDEX-INDIRECT)
       ((PDL-POINTER) SUB M-K (A-CONSTANT 1))
        (JUMP-XCT-NEXT QRAD2)
       ((PDL-INDEX M-K) SUB M-K (A-CONSTANT 2))

XRETN (MISC-INST-ENTRY %RETURN-N)               ;RETURN N VALUES, LAST ARG IS N.
        (CALL-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                TRAP)
  (ERROR-TABLE ARGTYP FIXNUM PP NIL)
  (ERROR-TABLE ARG-POPPED 0 PP)
        ((M-C) Q-POINTER PDL-POP) ;NUMBER OF VALUES TO RETURN
        (CALL FIND-MVR-FRAME)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-ADI-PRESENT) MD XRETN-SINGLE-VALUE)
        (CALL-XCT-NEXT XRETN1)
       ((M-J) M-K)
        (JUMP QMDDR)

;Return M-C values (on top of stack) from the frame
;whose call-state word is addressed by M-A.
;Actually, the last value we simply return to our caller, in M-T.
XRETN1  ((M-C) SUB M-C (A-CONSTANT 1))
        ((PDL-INDEX) SUB PDL-POINTER A-C)       ;NEXT ARGUMENT SLOT
        (POPJ-LESS-OR-EQUAL-XCT-NEXT M-C A-ZERO) ;LAST
       ((M-T) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
        ((M-S) A-ZERO)
        (CALL-XCT-NEXT MVR)
       ((M-K) M-J)
        (JUMP-NOT-EQUAL M-I A-ZERO XRETN1)
        (POPJ)

XRETN-SINGLE-VALUE
        ((M-C) SUB M-C (A-CONSTANT 1))
        ((PDL-POINTER) SUB PDL-POINTER A-C)     ;NEXT ARGUMENT SLOT
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP QMDDR)

;(ERROR-TABLE DEFAULT-ARG-LOCATIONS RETURN-LIST M-A)  no longer works.

XRETURN-LIST (MISC-INST-ENTRY RETURN-LIST)      ;This is always used with dest D-RETURN!
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-EQUAL M-T A-V-NIL RETURN-NO-VALUES)
        (CALL FIND-MVR-FRAME)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-ADI-PRESENT) MD QCAR)
;Return the elements of M-T from the frame
;whose call-state word is addressed by M-K.
;Actually, the last value we just return in M-T to our caller.
XRETURN-LIST1
#+exp   (open-carcdr m-t)
#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-T CARCDR-DISPATCH-DIRECT)
#+LAMBDA(NO-OP)
        ((M-C) M-T)             ;Save the CDR.
        (POPJ-EQUAL-XCT-NEXT M-C A-V-NIL)       ;If this is the last value, popj to QMDDR.
       ((M-T) Q-TYPED-POINTER M-A)              ;Return the CAR as a value, one way or the other.
        ((M-A) M-K)
        (CALL-XCT-NEXT MVR)             ;Do so.
       ((M-S) A-ZERO)
        (POPJ-EQUAL M-I A-ZERO)
        ((M-K) M-A)
        (JUMP-XCT-NEXT XRETURN-LIST1)
       ((M-T) M-C)              ;Get back the CDR.

XRET3 (MISC-INST-ENTRY %RETURN-3)
        ((M-S) A-ZERO)
        (CALL-XCT-NEXT XRNVRPI)
       ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 2))
        (JUMP-EQUAL M-I A-ZERO QMDDR)
XRET2 (MISC-INST-ENTRY %RETURN-2)
        ((M-S) A-ZERO)
        (CALL-XCT-NEXT XRNVRPI)
       ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 1))
        (JUMP-EQUAL M-I A-ZERO QMDDR)
        (JUMP-XCT-NEXT QMDDR)                   ;RETURN LAST VALUE REGULAR WAY
       ((M-T) Q-TYPED-POINTER PDL-TOP)

XRNV (MISC-INST-ENTRY RETURN-NEXT-VALUE)
        ((M-S) A-ZERO)
        (CALL-XCT-NEXT XRNVR)
       ((M-T) Q-TYPED-POINTER PDL-POP)          ;FROB TO RETURN
   (ERROR-TABLE ARG-POPPED 0 M-T)
        (POPJ-NOT-EQUAL M-I A-ZERO)
        (JUMP QMDDR)

;Come here with a NIL on the top of the stack.
;Calls XRNVR with the M-S flag, then returns (should always be returning to QMDDR).
;We go through MVR so that in case the caller used a multiple-value-list,
;we will clobber the ADI so that QMDDR won't return any values into that list.
RETURN-NO-VALUES
        ((M-S) (A-CONSTANT 1))
        (JUMP XRNVR)

XRNVRPI ((M-T) Q-TYPED-POINTER PDL-INDEX-INDIRECT)      ;Return value from PDL[PI]
;Return next value.  Called like MVR, except we find the frame, so
;you don't need to set up M-K.
;M-S must have the flag for MVR, and M-T the value to return.
XRNVR   (CALL FIND-MVR-FRAME)
        (JUMP-IF-BIT-SET (LISP-BYTE %%LP-CLS-ADI-PRESENT) MD MVR)       ;Doing mult vals
        (POPJ-AFTER-NEXT (M-I) A-ZERO)  ;Tell caller, no multiple values.
       (NO-OP)

;; Return in M-K the address of the call-state word
;; of the frame from which we should return multiple values from this frame.
;; The caller should test the %%LP-CLS-ADI-PRESENT bit in MD on return
;; to see whether that frame is really wanting multiple values.
;; Clobbers M-TEM, VMA, MD.
FIND-MVR-FRAME
        (declare (clobbers a-tem) (values a-k))
        ((M-K) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((PDL-INDEX) SUB M-K A-PDL-BUFFER-HEAD)
        ((M-K) ADD PDL-INDEX A-PDL-BUFFER-VIRTUAL-ADDRESS)
FIND-MVR-FRAME-1
        (CALL-XCT-NEXT MKCONT)                          ;Get this frame's call state
       ((M-K) Q-POINTER M-K)
        ((M-TEM) (LISP-BYTE %%LP-CLS-DESTINATION) MD)
        (POPJ-NOT-EQUAL M-TEM (A-CONSTANT D-RETURN))
FIND-MVR-FRAME-3
        ((M-TEM) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) MD) ;Chain back to previous frame
        (JUMP-NOT-EQUAL-XCT-NEXT M-TEM A-ZERO FIND-MVR-FRAME-1)
       ((M-K) SUB M-K A-TEM)
        (CALL ILLOP)                            ;Stack exhausted

;MD gets contents of untyped virtual address in M-K, when likely to be in pdl buffer
;and known not to be off the top end of the pdl buffer.
MKCONT  (declare (args a-k) (clobbers m-tem))
        (JUMP-LESS-THAN M-K A-PDL-BUFFER-VIRTUAL-ADDRESS MKCONT1)
        ((M-TEM) SUB M-K A-PDL-BUFFER-VIRTUAL-ADDRESS)
        (POPJ-AFTER-NEXT (PDL-INDEX) ADD M-TEM A-PDL-BUFFER-HEAD)
       ((MD) PDL-INDEX-INDIRECT)

MKCONT1 (POPJ-AFTER-NEXT (VMA-START-READ) M-K)
       (CHECK-PAGE-READ)

;Contents of untyped virtual address in M-K gets MD, when likely to be in pdl buffer
;and known not to be off the top end of the pdl buffer.
MKWRIT
;;; No more dtp-stack-closure. ~jrm
;       (JUMP-DATA-TYPE-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-STACK-CLOSURE))
;               MKWRIT2)
        (JUMP-LESS-THAN M-K A-PDL-BUFFER-VIRTUAL-ADDRESS MKWRIT1)
        ((M-TEM) SUB M-K A-PDL-BUFFER-VIRTUAL-ADDRESS)
        (POPJ-AFTER-NEXT (PDL-INDEX) ADD M-TEM A-PDL-BUFFER-HEAD)
       ((PDL-INDEX-INDIRECT) MD)

MKWRIT1
   ((VMA-START-WRITE) M-K)
   (CHECK-PAGE-WRITE)
   (gc-write-test)
   (popj)

;;; No more stack closures.
;;Stack closure, copy it if necessary.
;MKWRIT2        ((VMA-START-WRITE) M-K)
;       (CHECK-PAGE-WRITE)
;       (GC-WRITE-TEST)
;       (POPJ)

;Documentation on calling sequence for MVR:
;M-T has the value to be returned.
;M-K has virtual address of LPCLS Q for the frame from which value is to be returned.
;M-S has a flag which is 1 when we are returning no values;
; this only happens from (return-list nil).
; The flag is 2 if we are called from QRAD1.
; This affects MVRIND.  In this case, M-I is not valid on return.

;On return, M-I is zero iff this frame's caller wants no more values,
;or if he didn't want multiple values at all.
;In this case, M-T will be the same as it was passed.
;The caller of MVR should usually jump to QMDDR if M-I is zero,
;to return the value as a single value in case that is warranted.

;Certainly clobbers A-TEM1, M-B, M-I, M-J, M-K, M-S, M-R, M-T, M-TEM.
;Certainly preserves M-A, M-C, M-D, M-J, M-Q, M-ZR, M-1, M-2

;At this point M-K has the virtual address of the LPCLS Q for the frame
;from which the value is to be returned, which is known to have ADI.
;Investigate that ADI to see if there is a multiple-value receiver.
MVR
;;; No more stack closures ~jrm
;(CALL-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-STACK-CLOSURE))
;                   STACK-CLOSURE-RETURN-TRAP)
        ;; Get address of highest word of ADI
        ((M-K) SUB M-K (A-CONSTANT (EVAL (+ %LP-CALL-BLOCK-LENGTH %LP-CALL-STATE))))
MVR0    (CALL MKCONT)                           ;MD gets ADI Q
        (CALL-DATA-TYPE-NOT-EQUAL MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) TRAP)
    (ERROR-TABLE DATA-TYPE-SCREWUP ADI)
        ((M-TEM) (LISP-BYTE %%ADI-TYPE) MD)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (EVAL ADI-RETURN-INFO)) MVR1)
        (DISPATCH-XCT-NEXT (LISP-BYTE %%ADI-RET-STORING-OPTION) MD D-MVR)
       ((M-I) (LISP-BYTE %%ADI-RET-NUM-VALS-EXPECTING) MD)

MVR1    (CALL-IF-BIT-CLEAR (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) MD ILLOP)        ;Info out of phase
        (CALL-XCT-NEXT MKCONT)
       ((M-K) SUB M-K (A-CONSTANT 1))
        (JUMP-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) MD MVR0) ;More
       ((M-K) SUB M-K (A-CONSTANT 1))
        ;; No ADI, this the last value
MVR-CANCEL
        (POPJ-AFTER-NEXT (M-I) A-ZERO)
       (NO-OP)

;Indirect link.  Only allowed to indirect to something in the same pdl,
;so that MKCONT and MKWRIT can work.
;Currently, the check for 2 in M-S is based on the assumption
;that the values are being THROWn to the frame the indirect link is to.
;Since 2 is used only in calls from QRAD1, and QRAD1 doesn't look at M-I,
;we need not set it up.
MVRIND  (POPJ-EQUAL M-S (A-CONSTANT 2))
        (CALL-XCT-NEXT MKCONT)                  ;Get pointer to ADI to use
       ((M-K) SUB M-K (A-CONSTANT 1))
        ((M-K) Q-POINTER MD)
;; Indirect pointer is zero if it is NIL; that is,
;; if the frame we wanted to indirect to was not being asked for mult. values.
        (JUMP-NOT-EQUAL M-K A-ZERO MVR0)
        (JUMP MVR-CANCEL)

;Store in block
MVRB    (CALL-LESS-OR-EQUAL M-I A-ZERO ILLOP)   ;Returning too many values
        ((M-I) SUB M-I (A-CONSTANT 1))
        ((M-TEM) MD)                            ;Store back decremented values count
        ((MD M-TEM) DPB M-I (LISP-BYTE %%ADI-RET-NUM-VALS-EXPECTING) A-TEM)
        (JUMP-NOT-EQUAL M-I A-ZERO MVRB0)       ;If last val expected, clobber ADI.
        ((MD) DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-TYPE) A-TEM)
MVRB0   (CALL MKWRIT)
        (CALL-XCT-NEXT MKCONT)                  ;Get storing pointer
       ((M-K) SUB M-K (A-CONSTANT 1))
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        (CALL-XCT-NEXT MKWRIT)
       ((MD M-R) ADD MD (A-CONSTANT 1))
MVRB1   ((VMA-START-READ) SUB M-R (A-CONSTANT 1))       ;No transport, since writing and no
MVRB2   (CHECK-PAGE-READ)                               ;need to follow invisible pntrs here
        ((WRITE-MEMORY-DATA-START-WRITE)        ;Store the value
                SELECTIVE-DEPOSIT READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER A-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)                        ;More expected, or doing return and that was
        (popj)

;Cons up list
;2nd (lower) ADI word points to list tail.  Initially it is a locative
;to the location which will eventually hold the list of returned values,
;which should be initialized to NIL.
;After the first time, it is a list-pointer to the last cons in the list.
;XNCONS mustn't clobber M-I, M-R; XSETCDR1 mustn't clobber M-R.
MVRC    (JUMP-EQUAL M-S (A-CONSTANT 1) MVRC1)   ;Returning no values?
        ((M-I) ADD M-K                          ;Save address of prev ADI Q
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE) -1)))
        ((pdl-push) m-a)        ;clobbered by xncons
        (CALL-XCT-NEXT XNCONS)                  ;Cons up a 2-Q cons, cdr NIL, to M-T
       ((PDL-PUSH M-R) M-T)     ;Save value returning, will be car
        ((m-a) pdl-pop)
        (CALL-XCT-NEXT MKCONT)                  ;Get pointer to list tail
       ((M-K) Q-POINTER M-I)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ((M-S) MD)                              ;Save pntr to list tail
        (CALL-XCT-NEXT MKWRIT)
       ((MD) DPB M-T Q-TYPED-POINTER A-S)       ;Change pntr to new list tail
        (CALL XSETCDR1)                         ;RPLACD tail of list
        (POPJ-AFTER-NEXT (M-I) (A-CONSTANT 1))
       ((M-T) SETA A-R)    ;Restore value being returned

;Returning no values.  Don't affect list, and clobber ADI-TYPE so that when
;QRAD1 calls MVR, it won't affect the list either.
MVRC1   ((M-TEM) MD)
        ((MD) DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-TYPE) A-TEM)
        (JUMP-XCT-NEXT MKWRIT)
       ((M-I) (A-CONSTANT 1))

;;; THROW CODE (*THROW, *UNWIND-STACK)
;;; Register Conventions:
;;; A-CATCH-TAG is the first argument to that function.  Except, T and 0 are special.
;;; A-CATCH-COUNT contains a count of active frames.  If this reaches zero, we resume
;;;  that frame instead of throwing farther.  If this is NIL, no count.
;;; A-CATCH-ACTION contains the "action", which is usually NIL, but can if non-NIL,
;;;  when the resumption point is reached, instead of resuming it is a function
;;;  (or a stack-group) which gets called with one argument, the value being thrown.
;;; M-T value being thrown

;;; Special *CATCH tags are:
;;;  NIL  CATCH-ALL
;;;  T    UNWIND-PROTECT.  The difference between UNWIND-PROTECT and CATCH-ALL
;;;       is that UNWIND-PROTECT will always continue throwing.

;;; Special *THROW, *UNWIND-STACK tags are:
;;;  0    Return from function (like destination-return)
;;;  T    Throw all the way out the top of the stack-group.  In this case we
;;;       bypass CATCH-ALLs.  This is used for unwinding "old" stack groups.
;;;       This must be used in connection with a non-null A-CATCH-ACTION.
;;;  NIL  *CATCH returns NIL as the tag if no throw or return operation occurred.
;;; If the tag is neither T nor 0, we throw to the nearest catch with that tag,
;;; or UNWIND-PROTECT, or CATCH-ALL.

;;; *UNWIND-STACK is a generalized *THROW, used by the error handler and
;;; by UNWIND-PROTECT.  The first two arguments are the same as to *THROW.
;;; The third argument is a count; if this NIL things are the same as *THROW,
;;; otherwise if this many frames are passed we resume as if a catch had been found.
;;; The fourth argument, if non-NIL, means that instead of resuming when
;;; we find the point to throw to, we call that function with one argument,
;;; the second arg to *UNWIND-STACK.

XCATCH (MISC-INST-ENTRY *CATCH)         ;ONLY GET HERE WHEN NO *THROW
        (POPJ-AFTER-NEXT                ;*CATCH WHICH COMPLETES RETURNS NIL AS SECOND VALUE
         (M-T) Q-TYPED-POINTER PDL-POP) ;VALUE OF FROB
       ((M-GARBAGE) PDL-POP)

METER-FUNCTION-UNWIND
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-FUNCTION-UNWIND-EVENT)))
        (JUMP-XCT-NEXT METER-MICRO-WRITE-HEADER)
       ((A-METER-LENGTH) M-ZERO)        ;Number of meters pushed

;;; This like *UNWIND-STACK but takes its args in the order value, tag, count, action
;;; and simply moves value to the destination if tag is NIL (normal exit from unwind-protect)
XUWPCON (MISC-INST-ENTRY %UNWIND-PROTECT-CONTINUE)
        ((A-CATCH-ACTION) Q-TYPED-POINTER PDL-POP)
        ((A-CATCH-COUNT) Q-TYPED-POINTER PDL-POP)
        ((M-1) Q-TYPED-POINTER PDL-POP) ;Tag
        (POPJ-EQUAL-XCT-NEXT M-1 A-V-NIL)
       ((M-T) Q-TYPED-POINTER PDL-POP)  ;Value  "native" offset in FEF (byte-lambda, halfword-exp)
        (JUMP-EQUAL M-1 (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1))
          XUWPCON-POP-OPEN-CALL)
        ((PDL-PUSH) M-1)                ;Clobbered by meter code
        (JUMP-XCT-NEXT XUWPCN1)                         ;Join *UNWIND-STACK
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-FUNCTION-ENTRY-EXIT-ENABLE)
                M-METER-ENABLES METER-FUNCTION-UNWIND)


;A tag of 1 in %UNWIND-PROTECT-CONTINUE means
; return to a POP-OPEN-CALL instruction
; that popped an unwind protect's frame.
;The "value being thrown" records the location counter
; after the POP-OPEN-CALL instruction, in querterwords.
;We ignore the destination of the%UNWIND-PROTECT-CONTINUE
;and never push the "value" on the stack.

;we seem to get here via pop-open-call -> %unwind-protect-continue -> us
XUWPCON-POP-OPEN-CALL
        ((M-1) MACRO-IR-DEST)
        (JUMP-EQUAL M-1 A-ZERO XUWPCON-POP-OPEN-CALL-1)
       ((M-GARBAGE) MICRO-STACK-DATA-POP)       ;Don't store in our destination.
XUWPCON-POP-OPEN-CALL-1
;** M-FEF
        ((PDL-INDEX) M-AP)
;Get the address of the current fef, shifted left 2.
        ((M-1) DPB (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1) PDL-INDEX-INDIRECT A-ZERO)
        ((M-T) Q-POINTER M-T) ;"native" form
        ((LOCATION-COUNTER) ADD M-1 A-T)        ;QMDDR
;I don't know whether it would work to popj any sooner,
;since it will do an instruction fetch.
        (POPJ)

XUWSTK (MISC-INST-ENTRY *UNWIND-STACK)
   (ERROR-TABLE RESTART *UNWIND-STACK)
        (CALL-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%METER-FUNCTION-ENTRY-EXIT-ENABLE)
                M-METER-ENABLES METER-FUNCTION-UNWIND)
        ((A-CATCH-ACTION) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT XUWS0)
        ((A-CATCH-COUNT) Q-TYPED-POINTER PDL-POP)

;; Like THROW-N except returns the values from the active frame.
XRETURN-N-KEEP-CONTROL (MISC-INST-ENTRY RETURN-N-KEEP-CONTROL)
        ((M-C) Q-POINTER PDL-TOP)
        ((M-K) M-AP)
        (CALL CONVERT-PDL-BUFFER-ADDRESS)
        ((M-A) M-K)
        (JUMP XRETURN-N-KEEP-CONTROL-1)

;; This does not really throw!  It returns values in preparation for a throw.
;; Pass tag, values (N of them), and N itself.
;; On return, only the tag and one value (the last one, usually) remain on the stack.
;; Then you can eventually do a *THROW of those two things.
XTHROW-N (MISC-INST-ENTRY THROW-N)   ;Always D-IGNORE.
        ((M-C) Q-POINTER PDL-TOP)
        ((PDL-INDEX) SUB PDL-POINTER A-C)
        ((PDL-INDEX) SUB PDL-INDEX (A-CONSTANT 1))
        ((A-CATCH-TAG) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
        (CALL FIND-CATCH-CHECK) ;Find the catch frame we will throw to.
XRETURN-N-KEEP-CONTROL-1
        ((M-K) ADD M-A (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (CALL FIND-MVR-FRAME-1)
;M-K now has mem address of call-state word of frame to return values from.
        ((M-C) Q-POINTER PDL-POP)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-ADI-PRESENT) MD XTHROW-N-SINGLE-VALUE)
        ((M-D) M-C)             ;Save number of values we have.
        (CALL-XCT-NEXT XRETN1)  ;Return all but the last value.
       ((M-J) M-K)
        (POPJ-AFTER-NEXT (PDL-POINTER) SUB PDL-POINTER A-D)   ;Discard all args except tag.
       ((PDL-PUSH) M-T) ;Push back the last value, which XRETN1 didn't return.

XTHROW-N-SINGLE-VALUE
        (POPJ-AFTER-NEXT (M-C) SUB M-C (A-CONSTANT 1))
       ((PDL-POINTER) SUB PDL-POINTER A-C)      ;NEXT ARGUMENT SLOT

;; Like THROW-SPREAD except returns the values from the active frame.
XRETURN-SPREAD-KEEP-CONTROL (MISC-INST-ENTRY RETURN-SPREAD-KEEP-CONTROL)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-K) M-AP)
        (CALL CONVERT-PDL-BUFFER-ADDRESS)
        ((M-A) M-K)
        (JUMP XRETURN-SPREAD-KEEP-CONTROL-1)

;; This does not really throw!  It returns values in preparation for a throw.
;; Pass tag and a list of values.  Values are extracted from the list
;; and all but the last one is returned from the catch frame.
;; That last one is left on the stack.
;; On return, the stack contains the tag and a single value,
;; which you can pass to *THROW to complete the throw.
XTHROW-SPREAD (MISC-INST-ENTRY THROW-SPREAD)  ;Always D-IGNORE.
        ((M-T) Q-TYPED-POINTER PDL-POP)
        ((A-CATCH-TAG) Q-TYPED-POINTER PDL-TOP)
        (CALL FIND-CATCH-CHECK)
XRETURN-SPREAD-KEEP-CONTROL-1
        ((M-K) ADD M-A (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (CALL FIND-MVR-FRAME-1)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-ADI-PRESENT) MD XTHROW-SPREAD-SINGLE-VALUE)
        (JUMP-EQUAL M-T A-V-NIL XTHROW-SPREAD-NO-VALUES)
        (CALL XRETURN-LIST1)
        (POPJ-XCT-NEXT)
       ((PDL-PUSH) M-T)


XTHROW-SPREAD-SINGLE-VALUE
;#+CADR (CALL QCAR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-T CAR-PRE-DISPATCH-DIRECT)
;#+LAMBDA(NO-OP)
        (open-qcar m-t)
        (POPJ-XCT-NEXT)
       ((PDL-PUSH) M-T)

XTHROW-SPREAD-NO-VALUES
        ((M-S) (A-CONSTANT 1))
        (CALL MVR)
        (POPJ-XCT-NEXT)
       ((PDL-PUSH) A-V-NIL)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS *THROW A-CATCH-TAG M-T)

XTHROW (MISC-INST-ENTRY *THROW)
   (ERROR-TABLE RESTART *THROW)
        (CALL-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%METER-FUNCTION-ENTRY-EXIT-ENABLE)
                M-METER-ENABLES METER-FUNCTION-UNWIND)
        ((A-CATCH-ACTION) A-V-NIL)
        ((A-CATCH-COUNT) A-V-NIL)
XUWS0   ((M-T) Q-TYPED-POINTER PDL-POP)  ;Value thrown

;;;*** I think this is what we want to do here. ~jrm
;;; That is, of course, if stack closures existed.  Which they don't.
;       (call-data-type-equal m-t (a-constant (byte-value q-data-type dtp-stack-closure))
;                   stack-closure-return-trap)   ;do this first because it can result
                                                ;in attention getting set.

XUWPCN1 ((M-1) Q-TYPED-POINTER PDL-POP)  ;Tag
        (JUMP-EQUAL-XCT-NEXT M-1 A-V-TRUE XTHRW7)         ;Tag of T means all the way
       ((A-CATCH-TAG) M-1)                                ; so don't check first
        (JUMP-EQUAL M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                XTHRW7)                                   ;Tag of 0 also special
;Before actually going and munging anything, follow the open-call-block chain
;and find out whether the catch tag we're looking for actually exists.
XTHC0   (CALL FIND-CATCH)
;  M-D  Typeless virtual address of outermost active frame we are popping
;       that has the %%LP-CLS-TRAP-ON-EXIT bit set; or zero, if there is none.
;  M-A  Typeless virtual address of the frame that was found, or NIL.
        (JUMP-EQUAL M-A A-V-NIL XTHC1)
        (JUMP-EQUAL M-D A-ZERO XTHRW7)
        ((M-A) DPB M-A Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
XTHC1   (CALL XTHC-ERROR)
        (JUMP XTHC0)

XTHC-ERROR
        ((M-E) M-A)
        ((M-A) A-CATCH-TAG)  ;; The A-locations are not saved over SG switches.
        ((M-B) A-CATCH-COUNT)
        ((M-C) A-CATCH-ACTION)
        ((M-D) DPB M-D Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
        (CALL TRAP)
    (ERROR-TABLE THROW-TRAP)
;;Trap here means tag not seen if M-E is NIL,
;;means throwing thru trap-on-exit frame otherwise.
;;The error handler knows which M-locations contain the information, here.
        ((A-CATCH-TAG) M-A)
        ((A-CATCH-COUNT) M-B)
        ((A-CATCH-ACTION) M-C)
        (POPJ)

;FIND-CATCH plus error checking.
FIND-CATCH-CHECK
        (CALL FIND-CATCH)
        (POPJ-NOT-EQUAL M-A A-V-NIL)
        (CALL XTHC-ERROR)
        (JUMP FIND-CATCH-CHECK)

FIND-CATCH
;Find the catch frame for a catch tag.
;Register usage:
;  A-CATCH-TAG - tag to search for, as first arg of frame.
;  M-A  Virtual address of next call block (typeless) (either active or open)
;  M-B  Virtual address of next active call block (typeless)
;  M-C  Pdl buffer address of next call block (only low 10 bits valid)
;  M-D  Typeless virtual address of outermost active frame we are popping
;       that has the %%LP-CLS-TRAP-ON-EXIT bit set; or zero, if there is none.
;  M-1  arg into / result out of XTHCG
;Must preserve M-T, M-S and M-D for the sake of LMVRB.
        ((M-D) A-ZERO)
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) M-AP)
        ((M-B) Q-POINTER M-K)
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) A-IPMARK)
        ((M-A) Q-POINTER M-K)
        ((M-C) A-IPMARK)
        (JUMP-NOT-EQUAL M-A A-B FIND-CATCH2)                    ;JUMP IF FOUND OPEN CALL BLOCK
FIND-CATCH1
        (CALL-XCT-NEXT XTHCG)                           ;GET CALL STATE Q
       ((M-1) ADD M-A (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-ZR) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) M-1)
        (JUMP-EQUAL M-ZR A-ZERO FIND-CATCH-NOT-FOUND)    ;Reached bottom of PDL.
        ((M-B) SUB M-B A-ZR)
FIND-CATCH4
        ((M-ZR) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) M-1)
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-CLS-TRAP-ON-EXIT) M-1 FIND-CATCH-TRAP-LATER)
        ((M-A) SUB M-A A-ZR)
        (JUMP-EQUAL-XCT-NEXT M-A A-B FIND-CATCH1)
       ((M-C) SUB M-C A-ZR)
FIND-CATCH2
        (CALL-XCT-NEXT XTHCG)                           ;GET LPFEF Q
       ((M-1) M-A)
        (JUMP-NOT-EQUAL M-1 (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-U-ENTRY)
                                        *CATCH-U-CODE-ENTRY-/#))
                        FIND-CATCH3)            ;NO GOOD
        (CALL-XCT-NEXT XTHCG)
       ((M-1) ADD M-A (A-CONSTANT 1))                   ;GET FIRST ARG
        (POPJ-EQUAL M-1 A-CATCH-TAG)            ;FOUND THE ONE WE'RE LOOKING FOR.
        (POPJ-EQUAL M-1 A-V-NIL)                        ;FOUND CATCH-ALL, THATS OK TOO.
FIND-CATCH3
        (CALL-XCT-NEXT XTHCG)                           ;GET CALL STATE Q
       ((M-1) ADD M-A (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (JUMP FIND-CATCH4)

FIND-CATCH-NOT-FOUND
        (POPJ-XCT-NEXT)
       ((M-A) A-V-NIL)

;Keep track of the lowest stack frame that has the %%LP-CLS-TRAP-ON-EXIT bit set.
FIND-CATCH-TRAP-LATER
        (POPJ-XCT-NEXT)
        ((M-D) M-A)

;GET A WORD WHOSE UNTYPED VIRTUAL ADDRESS IS IN M-1.  FOR SPEED, ATTEMPTS
;TO FIGURE OUT IF IT IS IN THE PDL BUFFER AND IF SO GET IT DIRECTLY
;WITHOUT BOTHERING WITH PAGE TRAPS.  BASHES M-1 TO Q-TYPED-POINTER OF THE FETCHED DATA.
XTHCG   (JUMP-LESS-THAN M-1 A-PDL-BUFFER-VIRTUAL-ADDRESS XTHCG1)
        ((M-1) SUB M-1 A-A)
        (POPJ-AFTER-NEXT (PDL-INDEX) ADD M-1 A-C)
       ((M-1) Q-TYPED-POINTER PDL-INDEX-INDIRECT)

XTHCG1  ((VMA-START-READ) M-1)
        (CHECK-PAGE-READ)                       ;WILL PROBABLY ALWAYS FAULT
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT READ-MEMORY-DATA)
       ((M-1) Q-TYPED-POINTER READ-MEMORY-DATA)

;Here from QMDDR if there are open call blocks in this frame.  It could
;be an UNWIND-PROTECT, so we come here to check it out by doing a throw
;of the value being returned, to the tag 0.
QMDDR-THROW
        ((A-CATCH-TAG) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))) ;0
        ((A-CATCH-ACTION) A-V-NIL)
        ((A-CATCH-COUNT) A-V-NIL)
                ;drop into XTHRW7

;This is the main throw loop for actually exiting frames.  Come here for each frame.
XTHRW7  (JUMP-EQUAL-XCT-NEXT M-AP A-IPMARK XTHRW1) ;LAST FRAME ACTIVE, UNWIND IT
       ((M-R) A-V-NIL)                          ;GET NIL ON THE M SIDE FOR LATER
        ((M-I PDL-INDEX) A-IPMARK)      ;LAST FRAME OPEN, NOTE IT MUST ALREADY BE IN
                                                ; PDL BUFFER, SINCE ENTIRE ACTIVE FRAME IS.
        ((M-A) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
        (JUMP-NOT-EQUAL M-A (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-U-ENTRY)
                                        *CATCH-U-CODE-ENTRY-/#))
                 XTHRW2)                        ;That's not what we are looking for.
        ((PDL-INDEX) ADD A-IPMARK M-ZERO ALU-CARRY-IN-ONE)
        ((M-A) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
        (JUMP-EQUAL M-A A-V-TRUE XTHRW4)        ;FOUND UNWIND-PROTECT, RESUME IT
        ((M-1) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))   ;If coming from QMDDR-THROW,
        (JUMP-EQUAL A-CATCH-TAG M-1 XTHRW2)     ;recognize only an UNWIND-PROTECT.
        ((M-1) A-V-TRUE)
        (JUMP-EQUAL A-CATCH-TAG M-1 XTHRW2)     ;IF UNWINDING ALL THE WAY, KEEP LOOKING
        (JUMP-EQUAL M-A A-V-NIL XTHRW4)         ;FOUND CATCH-ALL, RESUME IT
        (JUMP-NOT-EQUAL M-A A-CATCH-TAG XTHRW2) ;DIDN'T FIND RIGHT TAG, KEEP LOOKING
;FOUND FRAME TO RESUME
XTHRW4  ((PDL-INDEX) ADD M-I (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-B) Q-TYPED-POINTER PDL-INDEX-INDIRECT)      ;PRESERVE FOR USE BELOW
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-ADI-PRESENT) PDL-INDEX-INDIRECT
                XTHRW9)         ;NO ADI, HAD BETTER BE DESTINATION RETURN
                                ;Reenters at XTHR5 with -1 in M-D.
        ((PDL-INDEX) SUB M-I (A-CONSTANT (EVAL %LP-CALL-BLOCK-LENGTH)))
        ((M-D) PDL-INDEX)
XTHRW3
        (CALL-DATA-TYPE-NOT-EQUAL PDL-INDEX-INDIRECT (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                TRAP)
    (ERROR-TABLE DATA-TYPE-SCREWUP ADI)
        ((M-J) (LISP-BYTE %%ADI-TYPE) PDL-INDEX-INDIRECT)
        (JUMP-NOT-EQUAL M-J (A-CONSTANT (EVAL ADI-RESTART-PC)) XTHRW8)
        ((M-J) (LISP-BYTE %%ADI-RPC-MICRO-STACK-LEVEL) PDL-INDEX-INDIRECT)
        ((PDL-INDEX) SUB M-D (A-CONSTANT 1))
        ((M-E) Q-POINTER PDL-INDEX-INDIRECT)        ;Restart PC
;** M-FEF
        ((PDL-INDEX) M-AP)
        ;; To make *CATCH in a micro-compiled function work will require more hair
        (CALL-DATA-TYPE-NOT-EQUAL PDL-INDEX-INDIRECT
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FEF-POINTER)) ILLOP)
        ;; Change frame's return PC to restart PC
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((M-TEM) PDL-INDEX-INDIRECT)
        ((PDL-INDEX-INDIRECT) DPB M-E (LISP-BYTE %%LP-EXS-EXIT-PC) A-TEM)
        ;; Pop micro-stack back to specified level
XTHRW5  ((M-ZR) MICRO-STACK-POINTER)            ;INVOLVES A LDB OP
        (JUMP-EQUAL M-ZR A-J XTHRW6)
        (CALL-LESS-THAN M-ZR A-J ILLOP)         ;Already popped more than that?
        ((M-ZR) MICRO-STACK-DATA-POP)
        (JUMP-XCT-NEXT XTHRW5)
       (CALL-IF-BIT-SET %%-PPBSPC M-ZR BBLKP)

 ;ON ENTRY HERE, M-D HAS PDL-BUFFER INDEX OF ADI-RESTART-PC ADI, OR -1 IF NONE.
XTHRW6  (JUMP-LESS-THAN M-D A-ZERO XTHRW6B)     ;IF ENCOUNTERED *CATCH W/O ADI-RESTART-PC ADI,
                        ;DONT TRY TO HACK BIND STACK.  THIS CAN HAPPEN VIA INTERPRETED
                        ;*CATCH S.  SINCE FRAME DESTINATION MUST BE D-RETURN,
                        ;NO NEED TO HACK BIND STACK ANYWAY.
        ((PDL-INDEX) SUB M-D (A-CONSTANT 3))  ;MOVE BACK TO THE DATA Q
                ;PREVIOUS ADI BLOCK WHICH HAD BETTER BE AN ADI-BIND-STACK-LEVEL BLOCK
        ((M-J) Q-POINTER PDL-INDEX-INDIRECT)    ;GET BIND-STACK-LEVEL
        (JUMP-IF-BIT-CLEAR BOXED-SIGN-BIT M-J XTHRW6C)  ;SIGN EXTEND SINCE EMPTY STACK
        ((M-J) SELECTIVE-DEPOSIT M-J Q-POINTER (A-CONSTANT -1)) ;IS LEVEL OF -1
XTHRW6C ((M-J) ADD M-J A-QLBNDO)
        (JUMP-EQUAL M-J A-QLBNDP XTHRW6A)
        (CALL-GREATER-THAN M-J A-QLBNDP ILLOP)  ;ALREADY OVERPOPPED?
XTHRW6F (CALL-IF-BIT-CLEAR M-QBBFL ILLOP)
        (CALL QUNBND)
        (JUMP-NOT-EQUAL M-J A-QLBNDP XTHRW6F)
XTHRW6A ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))  ;STORE BACK QBBFL
        ((M-TEM) PDL-INDEX-INDIRECT)            ;WHICH MAY HAVE BEEN CLEARED
        ((PDL-INDEX-INDIRECT) DPB M-FLAGS (LISP-BYTE %%LP-EXS-PC-STATUS) A-TEM)
XTHRW6B ((PDL-INDEX) SUB M-I A-AP)      ;THIS EFFECTIVELY CANCELS WHAT WILL BE
        ((M-PDL-BUFFER-ACTIVE-QS) ADD           ; DONE AT QMEX1
                PDL-INDEX A-PDL-BUFFER-ACTIVE-QS)
        ((M-AP pdl-index) PDL-BUFFER-ADDRESS-MASK M-I)          ;SIMULATE ACTIVATING CATCH FRAME
        ((m-fef) pdl-index-indirect)
        ((M-TEM) A-CATCH-TAG)                   ;IF THROWING OUT TOP, DON'T STOP ON
        (JUMP-EQUAL M-TEM A-V-TRUE XTHRW6D)     ; UNWIND-PROTECT, GO WHOLE WAY
        (JUMP-NOT-EQUAL A-CATCH-ACTION M-R XUWR2);ACTION NON-NIL => DONT REALLY RESUME
                                                ; EXECUTION, CALL FUNCTION INSTEAD.
;; If not an UNWIND-PROTECT and not a CATCH-ALL, just return the one value.
;; If multiple values are being thrown, this is the last one anyway.
XTHRW6D (JUMP-EQUAL M-A A-V-TRUE XTHRW6E)
        (JUMP-NOT-EQUAL M-A A-V-NIL QMEX1)
XTHRW6E ((M-S) A-ZERO)
        (CALL XRNVR)                            ;FIRST VALUE IS VALUE THROWN (STILL IN M-T)
        (JUMP-EQUAL M-I A-ZERO QMEX1)
        ((M-S) A-ZERO)
        (CALL-XCT-NEXT XRNVR)                   ;SECOND VALUE IS TAG
       ((M-T) A-CATCH-TAG)
        (JUMP-EQUAL M-I A-ZERO QMEX1)
        ((M-S) A-ZERO)
        (CALL-XCT-NEXT XRNVR)                   ;THIRD VALUE IS COUNT
       ((M-T) A-CATCH-COUNT)
        (JUMP-EQUAL M-I A-ZERO QMEX1)
        (JUMP-XCT-NEXT QMEX1)                   ;FOURTH VALUE IS ACTION
       ((M-T) A-CATCH-ACTION)

XTHRW8  (CALL-IF-BIT-CLEAR (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) PDL-INDEX-INDIRECT ILLOP)
        ((PDL-INDEX M-D) SUB M-D (A-CONSTANT 1))
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) PDL-INDEX-INDIRECT XTHRW9)
        ((PDL-INDEX M-D) SUB M-D (A-CONSTANT 1))
        (JUMP-XCT-NEXT XTHRW3)
       ((M-D) PDL-INDEX)        ;ASSURE M-D POSITIVE SO CHECK AT XTHRW6 WINS.

;RAN OUT OF ADI.  THE SAVED DESTINATION HAD BETTER BE D-RETURN OR ERROR.  THIS
;CAN HAPPEN MAINLY THRU INTERPRETED CALLS TO *CATCH.
XTHRW9  ((PDL-INDEX) ADD M-I (A-CONSTANT (EVAL %LP-CALL-STATE)))
;       ((M-C) (LISP-BYTE %%LP-CLS-DESTINATION) PDL-INDEX-INDIRECT)
;       (CALL-NOT-EQUAL M-C (A-CONSTANT D-RETURN) ILLOP)
        ((M-D) (M-CONSTANT -1))         ;SET FLAG THAT RESTART-PC ADI NOT FOUND, SO
                                ;BIND PDL HACKERY NOT ATTEMPTED.
        ((M-S) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) PDL-INDEX-INDIRECT)
        ((M-I) SUB M-I A-S)
        (JUMP-XCT-NEXT XTHRW5)
       ((M-J) M-ZERO)                           ;Flush whole micro-stack

;Skip this open frame
XTHRW2  ((PDL-INDEX) ADD M-I (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-ZR) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) PDL-INDEX-INDIRECT)
        ((M-ZR) SUB M-I A-ZR)
        (JUMP-XCT-NEXT XTHRW7)
       ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-ZR)        ;ASSURE NO GARBAGE IN A-IPMARK

;Unwind an active frame
XTHRW1  ((M-TEM) MICRO-STACK-POINTER)           ;INVOLVES A LDB OP
        (JUMP-EQUAL M-TEM A-ZERO XTHRW1A)       ;FLUSH MICRO-STACK
        ((M-TEM) MICRO-STACK-DATA-POP)
        (JUMP-XCT-NEXT XTHRW1)
       (CALL-IF-BIT-SET %%-PPBSPC M-TEM BBLKP)

XTHRW1A ((M-TEM) A-CATCH-TAG)                   ;CHECK FOR THROW TAG OF 0
        (JUMP-EQUAL M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                QMDDR0)                         ;YES, RETURN FROM THIS FRAME
        (JUMP-EQUAL M-R A-CATCH-COUNT XTHRW1B)  ;JUMP IF NO COUNT
        ((A-CATCH-COUNT Q-R) ADD A-CATCH-COUNT (M-CONSTANT -1))
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 23.) Q-R XUWR1)  ;REACHED MAGIC COUNT, RESUME BY RETURNING
XTHRW1B (CALL-IF-BIT-SET M-QBBFL BBLKP)         ;POP BINDING-BLOCK IF FRAME HAS ONE
        ((pdl-index) add m-ap (a-constant (eval %lp-entry-state)))
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-ENS-ENVIRONMENT-POINTER-POINTS-HERE)
                         PDL-INDEX-INDIRECT
                         QMEX1-COPY)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) PDL-INDEX-INDIRECT)
#+exp   ((M-ZR) SUB M-AP A-TEM1)                ;COMPUTE PREV A-IPMARK
#+exp   ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-ZR)       ;RESTORE THAT
#+lambda((a-ipmark) sub output-selector-mask-11 m-ap a-tem1)
        ((m-TEM1) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) PDL-INDEX-INDIRECT)
        ((PDL-POINTER) SUB M-AP (A-CONSTANT (EVAL %LP-CALL-BLOCK-LENGTH))) ;FLUSH PDL
        (JUMP-EQUAL A-TEM1 M-ZERO XUWR2)        ;OFF THE BOTTOM OF THE STACK, GO CALL THE
                                                ; ACTION, HAVING THROWN ALL THE WAY
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-CLS-ADI-PRESENT)
                PDL-INDEX-INDIRECT QRAD1R)      ;FLUSH ADDTL INFO
#+exp   ((PDL-INDEX) SUB M-AP A-TEM1)   ;RESTORE M-AP
#+exp   ((M-AP) PDL-INDEX)      ;THIS MASKS M-AP TO 10 BITS
#+lambda((m-ap pdl-index) sub output-selector-mask-11 m-ap a-tem1)
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM1)
        (CALL-LESS-THAN M-PDL-BUFFER-ACTIVE-QS
                        (A-CONSTANT PDL-BUFFER-LOW-WARNING) PDL-BUFFER-REFILL)
        ((m-fef) pdl-index-indirect)    ;do this after pdl-buffer-refill.

          ;Restore A-LOCALP.  In order to flush any stack closures in this frame we
          ;need A-LOCALP
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-TEM) (LISP-BYTE %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN) PDL-INDEX-INDIRECT)
        ((A-LOCALP) ADD M-AP A-TEM)

        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((M-FLAGS) (LISP-BYTE %%LP-EXS-PC-STATUS) PDL-INDEX-INDIRECT A-FLAGS)
        (CALL-IF-BIT-SET (LISP-BYTE %%LP-EXS-MICRO-STACK-SAVED) PDL-INDEX-INDIRECT QMMPOP)
                                                ;RESTORE USTACK FROM BINDING STACK
        (JUMP XTHRW7)

;HERE WHEN THE COUNT RUNS OUT
XUWR1   (CALL-NOT-EQUAL A-CATCH-ACTION M-R XUWR2)       ;CALL FUNCTION?
        (JUMP QMDDR0)   ;CAUSE ACTIVE FRAME TO RETURN VALUE

;HERE WHEN ACTION NOT NIL, IT IS A FUNCTION TO BE CALLED.
XUWR2   (CALL P3ZERO)
        ((PDL-PUSH) A-CATCH-ACTION)
        ((PDL-PUSH) Q-TYPED-POINTER M-T
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((ARG-JUMP MMCALL) (I-ARG 1))
;IF THROWING OUT WHOLE WAY, SHOULDN'T RETURN.  MICROSTACK MUST BE CLEAR
;IN THIS CASE OR MLLV WILL STORE IT IN THE WRONG FRAME, BECAUSE OF THE
;ANOMALOUS CASE OF M-AP = M-S.  IF NOT THROWING OUT WHOLE WAY, FUNCTION
;MAY RETURN AND ITS VALUE WILL BE RETURNED FROM THE *CATCH BY THE EXIT
;TO QMDDR0 AT XUWR1.

;;; STUFF FOR CALLS WITH NUMBER OF ARGUMENTS NOT KNOWN AT COMPILE TIME
;;; AND FOR MAKING CALLS WITH SPECIAL ADI OF DIVERS SORTS

XOCB  (MISC-INST-ENTRY %OPEN-CALL-BLOCK)  ;<FCTN><ADI-PAIRS><DEST>
        ((M-C) Q-POINTER PDL-POP)
        (JUMP-NOT-EQUAL M-C (A-CONSTANT 4) XOCB2)
        ((M-C) (A-CONSTANT D-RETURN))
XOCB2   ((M-A) Q-POINTER PDL-POP)
        ((M-T) Q-TYPED-POINTER PDL-POP)
        (JUMP-EQUAL M-A A-ZERO CBM0)            ;If no ADI, push regular call block
;ADI.  Fix up the %%ADI-PREVIOUS-ADI-FLAG flags,
;and check for any ADI that specifies LEXPR call or FEXPR call.
;M-3 will get the value for the %%LP-ENS-LCTYP field.
        ((PDL-INDEX) PDL-POINTER)       ;ADI, fix the flag bits
        ((M-3) A-ZERO)
        ((M-A) ADD M-A A-A)                     ;2 QS per ADI pair
XOCB1   ((PDL-INDEX-INDIRECT M-1) IOR PDL-INDEX-INDIRECT
                (A-CONSTANT (BYTE-MASK %%ADI-PREVIOUS-ADI-FLAG)))
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) M-A XOCB3)
        ((M-2) (LISP-BYTE %%ADI-TYPE) M-1)
        (JUMP-NOT-EQUAL M-2 (A-CONSTANT (EVAL ADI-FEXPR-CALL)) XOCB3)
        ((M-3) (A-CONSTANT (BYTE-VALUE %%LP-ENS-LCTYP 1)))
XOCB3   ((PDL-INDEX) SUB PDL-INDEX (A-CONSTANT 1))
        (JUMP-NOT-EQUAL-XCT-NEXT M-A (A-CONSTANT 2) XOCB1)
       ((M-A) SUB M-A (A-CONSTANT 1))
        (CALL-XCT-NEXT CBM0)            ;Push call block but take dest from M-C
       ((PDL-INDEX-INDIRECT)            ;Clear flag bit in last wd of ADI
                ANDCA PDL-INDEX-INDIRECT (A-CONSTANT (BYTE-MASK %%ADI-PREVIOUS-ADI-FLAG)))
        ((PDL-INDEX) ADD PDL-POINTER (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT A-3)
        (POPJ-AFTER-NEXT                ;Fix the ADI-present flag
         (PDL-INDEX) ADD PDL-POINTER (A-CONSTANT (EVAL %LP-CALL-STATE)))
       ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT
                (A-CONSTANT (PLUS (BYTE-MASK %%LP-CLS-ADI-PRESENT)
                                  (BYTE-MASK %%LP-CLS-ATTENTION))))

XAOCB (MISC-INST-ENTRY %ACTIVATE-OPEN-CALL-BLOCK)
        ;;*** this code is temporary to get around compiler bug
        ((M-TEM) MICRO-STACK-POINTER)
        (JUMP-EQUAL M-TEM A-ZERO XAOCB0)
#+lambda(JUMP XAOCB MICRO-STACK-PNTR-AND-DATA-POP)
#+exp   (JUMP XAOCB MICRO-STACK-DATA-POP)

XAOCB0  ;;*** end of temporary code
        (JUMP-XCT-NEXT QMRCL)           ;Fix CDR-code of last arg then activate call
       ((PDL-TOP) DPB PDL-TOP
                Q-ALL-BUT-CDR-CODE (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))

;;; I would be rather surprised if this is ever called!!  Foo, I'm surprised!
XPUSH (MISC-INST-ENTRY %PUSH)
        (POPJ-AFTER-NEXT
          (M-T) Q-TYPED-POINTER PDL-TOP)
       (NO-OP)

XAPDLR (MISC-INST-ENTRY %ASSURE-PDL-ROOM)
        ((M-1) Q-POINTER PDL-POP)       ;NUMBER OF PUSHES PLANNING TO DO
        ((PDL-INDEX) M-A-1 PDL-POINTER A-AP)    ;CURRENT FRAME SIZE
        ((M-2) ADD PDL-INDEX A-1)       ;PROPOSED NEW FRAME SIZE
       (popj-less-or-equal M-2 (A-CONSTANT 370)) ;NOTE FUDGE FACTOR OF 10 SINCE WE DON'T
                                                ;CURRENTLY KNOW HOW MANY COMPILER-GENERATED
                                                ;PUSHES MIGHT BE GOING TO HAPPEN
XAPDLR1 (CALL TRAP)
    (ERROR-TABLE STACK-FRAME-TOO-LARGE)
    (ERROR-TABLE ARG-POPPED 0 M-1)

;This makes a list of specified length, full of NILs, on the stack.  Because it
;pushes on the stack it must be done at "top level" in the function body, rather
;than as an argument to a function, unless a SHRINK-PDL-SAVE-TOP instruction is
;emitted at a suitable place.
XMSL (MISC-INST-ENTRY %MAKE-STACK-LIST)
        (CALL XAPDLR)                           ;M-1 GETS LIST LENGTH, CHECK FOR ROOM
        (JUMP-EQUAL M-1 A-ZERO XFALSE)          ;0-LENGTH LIST IS NIL
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)      ;MAKE RETURN VALUE
       ((M-K) ADD PDL-POINTER (A-CONSTANT 1))
XMSL1   #-cdr-next-is-0((PDL-PUSH) DPB (M-CONSTANT -1)  ;CDR-NEXT
                Q-CDR-CODE A-V-NIL)
        #+cdr-next-is-0((pdl-push) dpb m-zero q-cdr-code a-v-nil)
        (JUMP-GREATER-THAN-XCT-NEXT M-1 (A-CONSTANT 1) XMSL1)
       ((M-1) SUB M-1 (A-CONSTANT 1))
        (POPJ-AFTER-NEXT (PDL-TOP) Q-TYPED-POINTER PDL-TOP
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
       ((M-T) Q-POINTER M-K (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))

(begin-comment) Zwei Lossage (end-comment)

;Like %MAKE-STACK-LIST but expects the contents of
;the list to be on the stack already,
;followed by a word containing the length, which we discard.
;We fix the cdr codes and return a pointer.
XMESL (MISC-INST-ENTRY %MAKE-EXPLICIT-STACK-LIST)
        ((M-A) Q-POINTER PDL-POP)
        (JUMP-EQUAL M-A A-ZERO XFALSE)
        ((M-K) SUB PDL-POINTER A-A)
        ;Compute pointer to beginning of list.
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K PDL-INDEX) ADD M-K (A-CONSTANT 1))
  ;** following inst seems unnecessary. is it ever changed?
        ((m-b) (a-constant (byte-value q-cdr-code cdr-next)))
        (JUMP-EQUAL M-A (A-CONSTANT 1) XMESL2)
;Give all but last element of list CDR-NEXT.
XMESL1
        ((PDL-INDEX-INDIRECT) Q-TYPED-POINTER PDL-INDEX-INDIRECT A-B)
        ((M-A) SUB M-A (A-CONSTANT 1))
        ((PDL-INDEX) ADD PDL-INDEX (A-CONSTANT 1))
        (JUMP-GREATER-THAN M-A (A-CONSTANT 1) XMESL1)
XMESL2
;Give last element CDR-NIL.
        (POPJ-AFTER-NEXT
         (PDL-INDEX-INDIRECT) Q-TYPED-POINTER PDL-INDEX-INDIRECT
                              (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        ((M-T) Q-POINTER M-K (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))

;Like %MAKE-EXPLICIT-STACK-LIST except makes the last arg be the CDR.
XMESL* (MISC-INST-ENTRY %MAKE-EXPLICIT-STACK-LIST*)
        (CALL XMESL)
        ;; After first making an ordinary list, fix up the last to cdr codes.
        ((PDL-INDEX-INDIRECT) Q-TYPED-POINTER PDL-INDEX-INDIRECT
                              (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-ERROR)))
        (POPJ-AFTER-NEXT
         (PDL-INDEX) SUB PDL-INDEX (A-CONSTANT 1))
       ((PDL-INDEX-INDIRECT) Q-TYPED-POINTER PDL-INDEX-INDIRECT
                             (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NORMAL)))

;(%SPREAD-N list number) pushes the first <number> elements of <list>
;onto the stack.  If the destination is D-LAST, we then activate the call block.
;If the list is not long enough, we keep CDRing and presumably pushing NILs.
XSPREAD-N (MISC-INST-ENTRY %SPREAD-N)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;DON'T STORE IN DESTINATION
        ((M-K) Q-POINTER PDL-POP)       ;NUMBER OF ELEMENTS TO SPREAD.
        ((M-T) Q-TYPED-POINTER PDL-POP) ;LIST TO BE SPREAD
        ((M-C) MACRO-IR-DEST)
        ((M-D) M-T)             ;SAVE ORIGINAL ARGS FOR ERROR MSG.
        ((M-E) SUB M-K (A-CONSTANT 1))
        ((PDL-INDEX) M-A-1 PDL-POINTER A-AP)    ;CURRENT FRAME SIZE (MOD 2000)
        ((M-B) SUB PDL-INDEX (A-CONSTANT 370))  ;-# PUSHES ALLOWED (FUDGE FACTOR OF 10)
        ((M-B) SUB M-ZERO A-B)
        (CALL-LESS-THAN M-B A-E TRAP)
    (ERROR-TABLE STACK-FRAME-TOO-LARGE)
;M-E counts down to zero how many things we want to push.
;(It is number of pushes to do minus 1).
;M-C is the destination, saved for XSPREAD-EMPTY.
;M-T is the rest of the list.
;M-D and M-K are copies of the original arguments.
XSPREAD-N-1
;#+CADR (CALL-XCT-NEXT QCAR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-T CAR-PRE-DISPATCH-DIRECT)
        (open-qcar-xct-next m-t)
       ((M-A) M-T)
        ((PDL-PUSH) DPB M-T
                Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
;#+CADR (CALL-XCT-NEXT QCDR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE M-A CDR-PRE-DISPATCH-DIRECT)
        (open-qcdr-xct-next m-a)
       ((M-T) Q-TYPED-POINTER M-A)
        (JUMP-GREATER-THAN-XCT-NEXT M-E A-ZERO XSPREAD-N-1)
       ((M-E) SUB M-E (A-CONSTANT 1))
        (JUMP XSPREAD-EMPTY)

;(%SPREAD LIST)D-NEXT sends the elements of the list which is
;on the top of the stack to D-NEXT.  (%SPREAD LIST)D-LAST is similar
;but sends the last one to D-LAST (i.e. activates an open-call).
;(%SPREAD LIST)D-PDL is identical to (%SPREAD LIST)D-NEXT
(ERROR-TABLE DEFAULT-ARG-LOCATIONS %SPREAD M-D)

XSPREAD (MISC-INST-ENTRY %SPREAD)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;DON'T STORE IN DESTINATION
        ((M-T) Q-TYPED-POINTER PDL-POP) ;LIST TO BE SPREAD
        ((M-C) MACRO-IR-DEST)
        ((M-D) M-T)                                     ;SAVE ORIGINAL ARG FOR ERROR MSG.
MC-SPREAD-0                                             ;ENTRY FOR MICROCOMPILED CODE
        ((PDL-INDEX) M-A-1 PDL-POINTER A-AP)    ;CURRENT FRAME SIZE (MOD 2000)
        ((M-B) SUB PDL-INDEX (A-CONSTANT 370))  ;-# PUSHES ALLOWED (FUDGE FACTOR OF 10)
XSPREAD-1
        (JUMP-EQUAL M-T A-V-NIL XSPREAD-EMPTY)
;#+CADR (CALL-XCT-NEXT QCAR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-T CAR-PRE-DISPATCH-DIRECT)
        (open-qcar-xct-next m-t)
       ((M-A) M-T)
        ((PDL-PUSH) DPB M-T
                Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
;#+CADR (CALL-XCT-NEXT QCDR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE M-A CDR-PRE-DISPATCH-DIRECT)
        (open-qcdr-xct-next m-a)
       ((M-T) Q-TYPED-POINTER M-A)
        (JUMP-LESS-THAN-XCT-NEXT M-B A-ZERO XSPREAD-1)
       ((M-B) ADD M-B (A-CONSTANT 1))           ;DECREASE NEGATIVE COUNT OF PUSHES ALLOWED
        (CALL TRAP)
    (ERROR-TABLE STACK-FRAME-TOO-LARGE)

XSPREAD-EMPTY
        (JUMP-EQUAL M-C (A-CONSTANT D-LAST) XAOCB)
        (POPJ)

XCTO (MISC-INST-ENTRY %CATCH-OPEN)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX    ;running frame as well as *CATCH frame.
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
        (CALL-XCT-NEXT FLUSH-DESTINATION-RETURN-PC)
       ((M-T) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-U-ENTRY)
                                  *CATCH-U-CODE-ENTRY-/#)))
        (CALL-XCT-NEXT SBPL-ADI)        ;PUSH ADI-BIND-STACK-LEVEL BLOCK
       ((M-S) Q-TYPED-POINTER PDL-POP)  ;GET RESTART PC OFF STACK
        ((PDL-PUSH)
                DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) A-S)    ;PUSH RESTART PC
        ((M-R) MICRO-STACK-POINTER)
        (JUMP-XCT-NEXT XCTO1)
       ((PDL-PUSH) DPB M-R (LISP-BYTE %%ADI-RPC-MICRO-STACK-LEVEL)
             (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                               (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                               (BYTE-VALUE %%ADI-TYPE ADI-RESTART-PC))))

SBPL-ADI((M-1) A-QLBNDP)                ;STORE ADI-BIND-STACK-LEVEL ADI BLOCK
        ((M-1) SUB M-1 A-QLBNDO)
        (POPJ-AFTER-NEXT
         (PDL-PUSH) DPB M-1 Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((PDL-PUSH)
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                                  (BYTE-VALUE %%ADI-TYPE ADI-BIND-STACK-LEVEL))))

XCTOM (MISC-INST-ENTRY %CATCH-OPEN-MV)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX    ;running frame as well as *CATCH frame.
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
        (CALL-XCT-NEXT FLUSH-DESTINATION-RETURN-PC)
       ((M-T) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-U-ENTRY)
                                  *CATCH-U-CODE-ENTRY-/#)))
        ((M-D) Q-TYPED-POINTER PDL-POP) ;# VALS TO BE RECVD
        (CALL-XCT-NEXT LMVRB)                           ;LEAVE RM ON PDL TO RECEIVE VALS
       ((M-S) Q-TYPED-POINTER PDL-POP)  ;RESTART PC
        (CALL SBPL-ADI)         ;PUSH ADI-BIND-STACK-LEVEL BLOCK
        ((PDL-PUSH) DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) A-S)
        ((M-R) MICRO-STACK-POINTER)
        ((PDL-PUSH) DPB M-R (LISP-BYTE %%ADI-RPC-MICRO-STACK-LEVEL)
             (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                               (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                               (BYTE-VALUE %%ADI-TYPE ADI-RESTART-PC))))
        (JUMP-XCT-NEXT XCTOM1)
       ((M-K) DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) A-K) ;THIS ISN'T LAST ADI

XCTOMVL (MISC-INST-ENTRY %CATCH-OPEN-MV-LIST)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX    ;running frame as well as *CATCH frame.
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
        (CALL-XCT-NEXT FLUSH-DESTINATION-RETURN-PC)
       ((M-T) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-U-ENTRY)
                                  *CATCH-U-CODE-ENTRY-/#)))
        ((M-S) Q-TYPED-POINTER PDL-POP) ;RESTART PC
        ((PDL-PUSH)     ;INIT CDR OF LIST, ON RET WILL BE LIST
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)  ;CDR-NEXT because this
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))  ;can get left on stack
                                                ;and eventually become an argument.
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) PDL-POINTER)              ;GET LOCATIVE POINTER TO THAT NIL
        ((M-D) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                 (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                                 (BYTE-VALUE %%ADI-TYPE ADI-RETURN-INFO)
                                 (BYTE-VALUE %%ADI-RET-STORING-OPTION ADI-ST-MAKE-LIST))))
        (CALL SBPL-ADI)         ;PUSH ADI-BIND-STACK-LEVEL BLOCK
        ((PDL-PUSH) DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) A-S)
        ((M-R) MICRO-STACK-POINTER)
        ((PDL-PUSH) DPB M-R (LISP-BYTE %%ADI-RPC-MICRO-STACK-LEVEL)
             (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                               (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                               (BYTE-VALUE %%ADI-TYPE ADI-RESTART-PC))))
        (JUMP-XCT-NEXT XCTOM1)
       ((M-K) DPB (M-CONSTANT -1) (LISP-BYTE %%ADI-PREVIOUS-ADI-FLAG) A-K) ;THIS ISN'T LAST ADI

XFEC (MISC-INST-ENTRY %FEXPR-CALL)
        (CALL-XCT-NEXT FLUSH-DESTINATION-RETURN-PC)
       ((M-T) PDL-POP)  ;FUNCTION TO CALL
        (CALL CBM)
        (POPJ-AFTER-NEXT
         (PDL-INDEX) ADD M-ZR (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
       ((PDL-INDEX-INDIRECT) (A-CONSTANT (BYTE-VALUE %%LP-ENS-LCTYP 1)))

XFECM (MISC-INST-ENTRY %FEXPR-CALL-MV)
        (CALL XCMV)
        (POPJ-AFTER-NEXT
         (PDL-INDEX) ADD M-ZR (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
       ((PDL-INDEX-INDIRECT) (A-CONSTANT (BYTE-VALUE %%LP-ENS-LCTYP 1)))

XFECMVL (MISC-INST-ENTRY %FEXPR-CALL-MV-LIST)
        (CALL XCMVL)
        (POPJ-AFTER-NEXT
         (PDL-INDEX) ADD M-ZR (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
       ((PDL-INDEX-INDIRECT) (A-CONSTANT (BYTE-VALUE %%LP-ENS-LCTYP 1)))

XC0MVL (MISC-INST-ENTRY %CALL0-MULT-VALUE-LIST)
        ((M-TEM) MICRO-STACK-POINTER)           ;Insert continuation to QMRCL in pdl
        (JUMP-EQUAL M-TEM A-ZERO XC0MVL1)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMRCL)))
XC0MVL1 ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMRCL)))
XCMVL (MISC-INST-ENTRY %CALL-MULT-VALUE-LIST)
        (CALL-XCT-NEXT FLUSH-DESTINATION-RETURN-PC)
       ((M-T) PDL-POP)  ;FCN TO CALL
        ((PDL-PUSH)     ;INIT CDR OF LIST, ON RET WILL BE LIST
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)  ;CDR-NEXT because this
                                  (BYTE-VALUE Q-CDR-CODE CDR-NEXT))))  ;can get left on stack
                                                ;and eventually become an argument.
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) PDL-POINTER)              ;GET LOCATIVE POINTER TO THAT NIL
        ((PDL-PUSH) M-K)        ;AS 2ND ADI WORD
        (JUMP-XCT-NEXT XCTO1)
       ((PDL-PUSH)              ;ADI FOR RETURN VALUES INFO
             (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                               (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                               (BYTE-VALUE %%ADI-TYPE ADI-RETURN-INFO)
                               (BYTE-VALUE %%ADI-RET-STORING-OPTION ADI-ST-MAKE-LIST))))

XC0MV (MISC-INST-ENTRY %CALL0-MULT-VALUE)
        ((M-TEM) MICRO-STACK-POINTER)           ;Insert continuation to QMRCL in pdl
        (JUMP-EQUAL M-TEM A-ZERO XC0MV1)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMRCL)))
XC0MV1  ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC QMRCL)))
XCMV (MISC-INST-ENTRY %CALL-MULT-VALUE)
        ((M-TEM) MACRO-IR-DEST)
        (JUMP-EQUAL M-TEM (A-CONSTANT D-IGNORE) XCMV0)
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
XCMV0   ((M-D) Q-TYPED-POINTER PDL-POP)         ;# VALUES DESIRED
        (CALL-XCT-NEXT LMVRB)                   ;MAKE ROOM ON PDL
       ((M-T) PDL-POP)          ;FCN TO CALL
XCTOM1  ((PDL-PUSH) M-K)        ;RETURN VALUES BLOCK POINTER
        ((PDL-PUSH) M-D)
XCTO1   (CALL CBM)                              ;STORE CALL BLOCK
        (POPJ-AFTER-NEXT
         (PDL-INDEX) ADD M-ZR (A-CONSTANT (EVAL %LP-CALL-STATE)))
       ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT
                (A-CONSTANT (PLUS (BYTE-MASK %%LP-CLS-ADI-PRESENT)
                                  (BYTE-MASK %%LP-CLS-ATTENTION))))

;; Push slots for multiple values to go in.
;; M-D should be a fixnum saying how many slots.
;; Alternatively, if M-D is NIL, it means we want to arrange to
;; throw our multiple values, and the throw tag is on the top of the stack.
;; We use the throw tag we find, but we leave it on the stack.
;; Returns two words to push as ADI, in M-K and M-D.
;; If the caller has pushed any ADI already,
;; he should set %%ADI-PREVIOUS-ADI-FLAG when pushing M-K.
;; Preserves M-T and M-S.
LMVRB   (JUMP-EQUAL M-D A-V-NIL LMVRB-THROW)
        (JUMP-EQUAL M-D A-V-TRUE LMVRB-RETURN)
        ((M-E) Q-POINTER M-D)   ;Count NILs left to push.
        ((M-K) DPB M-D          ;ADI for return values info
          (LISP-BYTE %%ADI-RET-NUM-VALS-EXPECTING)
             (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                               (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                               (BYTE-VALUE %%ADI-TYPE ADI-RETURN-INFO)
                               (BYTE-VALUE %%ADI-RET-STORING-OPTION ADI-ST-BLOCK))))
        ((M-D) DPB M-D
          (LISP-BYTE %%ADI-RET-NUM-VALS-TOTAL) A-K)
        ((M-K) ADD PDL-POINTER (A-CONSTANT 1))          ;LOC OF BLOCK AS PDL INDEX
;RESERVE SLOTS, FILL WITH NIL
LMVRB1  #+cdr-next-is-0((pdl-push) dpb m-zero q-cdr-code a-v-nil)
        #-cdr-next-is-0((pdl-push) dpb (m-constant -1) q-cdr-code a-v-nil)
        (JUMP-GREATER-THAN-XCT-NEXT M-E (A-CONSTANT 1) LMVRB1)
       ((M-E) SUB M-E (A-CONSTANT 1))
        (JUMP CONVERT-PDL-BUFFER-ADDRESS)       ;RET BLK PNTR AS LOCATIVE

;; Here if "number of values" is T.  Return this call's values from the active frame.
LMVRB-RETURN
        ((M-K) M-AP)
        (CALL CONVERT-PDL-BUFFER-ADDRESS)
        ((M-A) M-K)
        (JUMP LMVRB-RETURN-1)

;; Here if "number of values" is NIL.  Top of stack contains a catch tag;
;; return this call's values from the catch for that tag.
LMVRB-THROW
        ((A-CATCH-TAG) Q-TYPED-POINTER PDL-TOP)
        (CALL FIND-CATCH-CHECK)
;; M-A gets address of the catch frame.
LMVRB-RETURN-1
        ((M-K) ADD M-A (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (CALL FIND-MVR-FRAME-1)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-ADI-PRESENT) MD LMVRB-THROW-ONE-VALUE)
        ((M-K) SUB M-K (A-CONSTANT (EVAL (+ %LP-CALL-BLOCK-LENGTH %LP-CALL-STATE))))
        (POPJ-AFTER-NEXT
         (M-K) DPB M-K Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
        ((M-D)
         (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                           (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                           (BYTE-VALUE %%ADI-TYPE ADI-RETURN-INFO)
                           (BYTE-VALUE %%ADI-RET-STORING-OPTION ADI-ST-INDIRECT))))

LMVRB-THROW-ONE-VALUE
        (POPJ-AFTER-NEXT (M-K) A-V-NIL)
        ((M-D)
         (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                           (BYTE-VALUE %%ADI-PREVIOUS-ADI-FLAG 1)
                           (BYTE-VALUE %%ADI-TYPE ADI-RETURN-INFO)
                           (BYTE-VALUE %%ADI-RET-STORING-OPTION ADI-ST-INDIRECT))))

;;; The above misc instructions use their destination as a sub-opcode
;;; rather than as a normal destination.  This subroutine flushes the
;;; destination return address, if it is present.
;;; Note that none of the above will work anyway when called from micro-compiled code.
FLUSH-DESTINATION-RETURN-PC
        ((M-TEM) MACRO-IR-DEST)
        (POPJ-AFTER-NEXT POPJ-EQUAL M-TEM (A-CONSTANT D-IGNORE))
       ((M-GARBAGE) MICRO-STACK-DATA-POP)

;;; APPLY and MICRO-TO-MACRO calls (used by micro-compiled code and by
;;; certain things in the base microcode.)

UAPLY  (MISC-INST-ENTRY INTERNAL-APPLY)
        ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 1))
        ((M-S) Q-TYPED-POINTER PDL-INDEX-INDIRECT)              ;Function
        (CALL-XCT-NEXT XARGI0)          ;RETURN ARG-INFO IN M-T
       ((M-J) M-S)                      ;Save a copy of function for later.
        ((M-A) Q-TYPED-POINTER PDL-TOP) ;Arguments
        (JUMP-IF-BIT-SET (LISP-BYTE %%ARG-DESC-QUOTED-REST) M-T UAPFX)
        (JUMP-IF-BIT-SET (LISP-BYTE %%ARG-DESC-EVALED-REST) M-T UAPFX)
;Note: calls to instances and select-methods come here because
;%args-info never says "rest arg" for them.  Ditto for funcallable hash tables.
;This causes trouble for interpreted fexprs since they expect safe rest args.
;APPLY-LAMBDA should fix that up for them.
UAPLY1  (CALL P3ZERO)                           ;PUSH MICRO-TO-MACRO CALL BLOCK, NO ADI
        ((PDL-PUSH) M-J)        ;FINISH CALL BLOCK BY PUSHING FCTN
        ((M-R) A-ZERO)                  ;COUNT OF # ARGS PUSHED
        (jump-data-type-not-equal m-a (a-constant (byte-value q-data-type dtp-list)) uaply4)
UAPLY5
;#+CADR (CALL-XCT-NEXT QCAR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE M-A CAR-PRE-DISPATCH-DIRECT)
        (open-qcar-xct-next m-a)
       ((M-T) M-A)
        ((PDL-PUSH) DPB M-T Q-TYPED-POINTER
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
;#+CADR (CALL-XCT-NEXT QCDR)
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE M-A CDR-PRE-DISPATCH-DIRECT)
        (open-qcdr-xct-next m-a)
       ((M-T) Q-TYPED-POINTER M-A)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)) uaply6)
        ((M-A) M-T)
        (JUMP-XCT-NEXT UAPLY5)
       ((M-R) ADD M-R (A-CONSTANT 1))

UAPLY6  ((PDL-TOP) DPB PDL-TOP Q-ALL-BUT-CDR-CODE
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        (JUMP-XCT-NEXT UAPLY4)
       ((M-R) ADD M-R (A-CONSTANT 1))

UAPFX   (CALL P3ZERO)           ;Push micro-to-macro call block, FEXPR call
        ((PDL-TOP) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                     (BYTE-VALUE %%LP-ENS-LCTYP 1))))
        ((PDL-PUSH) M-J)        ;function
        ((PDL-PUSH) M-A)        ;list of args
        ((M-R) (A-CONSTANT 1))
;This is like MMJCALL except that the number of args is already in M-R.
UAPLY4  ((M-TEM) MICRO-STACK-PC-DATA)           ;Check the return address
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (I-MEM-LOC QMDDR)) UAPLY4R) ;ordinary return
;;; Change destination to D-RETURN so that multiple values will be passed
;;; back correctly.  Dont worry about args.  They will be flushed by frame unwindage.
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;Flush return to QMDDR
        ((M-TEM) A-IPMARK)                      ;Find LP-CLS Q of open call block
        ((PDL-INDEX) ADD M-TEM (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (JUMP-XCT-NEXT MMCAL4)
       ((PDL-INDEX-INDIRECT) SUB PDL-INDEX-INDIRECT
                (A-CONSTANT (BYTE-VALUE %%LP-CLS-DESTINATION
                                        (DIFFERENCE D-MICRO D-RETURN))))

UAPLY4R (CALL MMCAL4)
        (POPJ-AFTER-NEXT
         (PDL-POINTER) SUB PDL-POINTER (A-CONSTANT 2))  ;remove args.
       (NO-OP)

;;; Activate a pending micro-to-macro call block.
;;; ((ARG-JUMP MMJCALL) (I-ARG number-args-pushed)) if you want to return the result(s)
;;; of the call as your own result(s).
;;; Changes the destination in the call-block from D-MICRO to D-RETURN if necessary
MMJCALL ((M-TEM) MICRO-STACK-PC-DATA)           ;Check the return address
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (I-MEM-LOC QMDDR)) MMCALL) ;ordinary return
;;; Change destination to D-RETURN so that multiple values will be passed
;;; back correctly.
        ((M-GARBAGE) MICRO-STACK-DATA-POP)      ;Flush return to QMDDR
MMJCALR ((M-TEM) A-IPMARK)                      ;Find LP-CLS Q of open call block
        ((PDL-INDEX) ADD M-TEM (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((PDL-INDEX-INDIRECT) SUB PDL-INDEX-INDIRECT
                (A-CONSTANT (BYTE-VALUE %%LP-CLS-DESTINATION
                                        (DIFFERENCE D-MICRO D-RETURN))))
        ;; Drop into MMCALL, dispatch-constant (I-ARG) still valid.
;;; Activate a pending micro-to-macro call block.
;;; ((ARG-CALL MMCALL) (I-ARG number-args-pushed)) if you want to get back the
;;; result of the function.  You can receive multiple values if you opened
;;; the call by pushing ADI and calling P3ADI rather than P3ZERO.
MMCALL  ((M-R) READ-I-ARG)
;;; Here if M-R is already set up.
MMCAL4  ((M-S PDL-INDEX) M-AP)
        ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH #+lambda 2 #+exp 1)
                 (A-CONSTANT 0))        ;Shift 2 to align with location counter
                         ;Relative PC (hwds)
        ((M-TEM) SUB LOCATION-COUNTER A-TEM1 #+lambda OUTPUT-SELECTOR-RIGHTSHIFT-1)
        ((M-AP PDL-INDEX) A-IPMARK)
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
        ((m-fef) m-a)
#+LAMBDA((M-R) SUB OUTPUT-SELECTOR-MASK-11
               PDL-BUFFER-POINTER A-IPMARK)     ;M-R PASSES ARG COUNT TO CALLED FCTN
#+EXP   ((PDL-INDEX) SUB PDL-BUFFER-POINTER A-IPMARK)
#+EXP   ((M-R) PDL-BUFFER-INDEX)
        ;; Build exit-state word from PC, M-FLAGS, and previous contents (old QLLV)
        ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((m-TEM1) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT (BYTE-FIELD 21 17) A-TEM)
                        ;CODE KNOWS THAT %%LP-EXS-EXIT-PC IS 0017
                ;Save M-QBBFL then clear it
        ((PDL-INDEX-INDIRECT) DPB M-FLAGS (LISP-BYTE %%LP-EXS-PC-STATUS) A-TEM1)
        ((M-FLAGS) SELECTIVE-DEPOSIT M-FLAGS M-FLAGS-EXCEPT-PROCESSOR-FLAGS A-ZERO)

        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))      ;old finish-entered-frame
        ((M-TEM) C-PDL-BUFFER-INDEX)
        ((C-PDL-BUFFER-INDEX)  DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED) A-TEM)
        ((PDL-INDEX) SUB M-AP A-S)      ;Increment to M-AP (truncated to 10 bits)
        ((M-PDL-BUFFER-ACTIVE-QS) ADD PDL-INDEX A-PDL-BUFFER-ACTIVE-QS)
;KHS, if you tweak this like QMRCL, be sure to reset flags properly.  KHS 10/02/84.
        (CALL-GREATER-THAN M-PDL-BUFFER-ACTIVE-QS
                                    A-PDL-BUFFER-HIGH-WARNING PDL-BUFFER-DUMP)
        (CALL TRANSFER-MICRO-STACK-TO-SPECPDL)
        (DISPATCH-XCT-NEXT qmrcl-dispatch M-A)
       (NO-OP)


;       ((PDL-INDEX) SUB PDL-POINTER A-R)       ;Address of new frame
;       ((M-S) PDL-INDEX)               ;Must be in both M-S and PDL-INDEX
;       (CALL-NOT-EQUAL M-S A-IPMARK ILLOP)     ;Frame not where it should be.  M-R lied?
;        ((M-A) PDL-INDEX-INDIRECT)             ;M-A := FUNCTION TO CALL

;       ((PDL-INDEX) M-AP)              ;Must save LC as half-word offset from FEF
;       ((m-TEM1) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH 2)
;                (A-CONSTANT 0))        ;Shift 2 to align with location counter
;       ((M-TEM) SUB LOCATION-COUNTER A-TEM1 OUTPUT-SELECTOR-RIGHTSHIFT-1) ;Relative PC (hwds)
;       ;; Build exit-state word from PC, M-FLAGS, and previous contents
;       ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-EXIT-STATE)))
;       ((m-TEM1) SELECTIVE-DEPOSIT PDL-INDEX-INDIRECT (BYTE-FIELD 21 17) A-TEM)
;                       ;Save M-QBBFL then clear it
;       ((PDL-INDEX-INDIRECT) DPB M-FLAGS (LISP-BYTE %%LP-EXS-PC-STATUS) A-TEM1)
;       ((M-FLAGS) SELECTIVE-DEPOSIT M-FLAGS M-FLAGS-EXCEPT-PROCESSOR-FLAGS A-ZERO)

;       (DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA Q-DATA-TYPE M-A D-QMRCL)
;       (NO-OP)

 ;      (DISPATCH Q-DATA-TYPE M-A D-QMRCL)      ;Does MLLV if necc
 ;     (CALL MLLV)

TRANSFER-MICRO-STACK-TO-SPECPDL
        ((M-TEM) MICRO-STACK-POINTER)
        (POPJ-EQUAL M-TEM (A-CONSTANT 1))       ;Return if nothing to save
        ((M-2) MICRO-STACK-DATA-POP)    ;Get real return off micro-stack
        ((M-1) ADD (M-CONSTANT 40) A-QLBNDP)    ;TEST P.C.E. (THIS M-CONST JUST HAPPENED TO
        ((M-1) SUB M-1 A-QLBNDH)                ; BE AROUND AT THE WRONG TIME).
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 TRAP)
  (ERROR-TABLE PDL-OVERFLOW SPECIAL)            ;M-1 should be negative as 24-bit quantity
        ((M-Q) DPB (M-CONSTANT -1)      ;First Q in block has flag bit
                (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
TMS-1
#+lambda((WRITE-MEMORY-DATA) MICRO-STACK-DATA-POP A-Q)  ;Note- this involves a LDB operation
#+exp   ((write-memory-data) ldb (byte-field 17. 0) micro-stack-data-pop a-q)
        ((A-QLBNDP) ADD A-QLBNDP M-ZERO ALU-CARRY-IN-ONE)
        ((VMA-START-WRITE) A-QLBNDP)
        (CHECK-PAGE-WRITE)
      ;;writing FIXNUMs ... no gc-write-test
        ((M-TEM) MICRO-STACK-POINTER)   ;Loop if not done
        (JUMP-NOT-EQUAL-XCT-NEXT M-TEM A-ZERO TMS-1)
        ;Remaining Q's in block do not have flag bit
       ((M-Q) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-INDEX) ADD M-S (A-CONSTANT (EVAL %LP-EXIT-STATE)))
        ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT
                (A-CONSTANT (BYTE-MASK %%LP-EXS-MICRO-STACK-SAVED)))

        ((MICRO-STACK-DATA-PUSH) M-2)   ;Push back return address

  ;set attention in frame being created, NOT running frame.  This is because
  ;we want this stuff to get popped when frame we are creating now returns.
  ;it sort of makes sense if you consider the MICRO-STACK-SAVED to be part of the
  ;PC and thus should be stored in the running frame.
        (POPJ-AFTER-NEXT (PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
       ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))



QLENTR-FAST-FIXED-NO-LOCALS
        ((M-1) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH 1) (A-CONSTANT 0))   ;NOW UNRELOCATE PC
        ((LOCATION-COUNTER) ADD M-1 A-J #+lambda OUTPUT-SELECTOR-LEFTSHIFT-1)
        ((M-E) (LISP-BYTE %%FEFH-ARGS-FOR-FANL) MD)
        (POPJ-EQUAL M-E A-R)
       ((A-IPMARK) M-AP)        ;NO OPEN CALL BLOCK YET
  ;DROP THRU ON WRONG NUMBER ARGS.
FAST-FIXED-WNA
        ((M-ERROR-SUBSTATUS) M-ZERO)    ;CLEAR OUT ERRORS
        (CALL-LESS-THAN M-R A-E SET-TOO-FEW-ARGS)
        (CALL-GREATER-THAN M-R A-E SET-TOO-MANY-ARGS)
        (JUMP QLEERR)

QL-FAST-TOO-FEW-ARGS
        (CALL-XCT-NEXT SET-TOO-FEW-ARGS)
       ((M-ERROR-SUBSTATUS) M-ZERO)
        (JUMP QLEERR)

QL-FAST-TOO-MANY-ARGS
        ((M-GARBAGE) MICRO-STACK-DATA-POP)
        (CALL-XCT-NEXT SET-TOO-MANY-ARGS)
       ((M-ERROR-SUBSTATUS) M-ZERO)
        (JUMP QLEERR)

QLENTR-FAST-VAR-NO-LOCALS
        ((M-1) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH 1) (A-CONSTANT 0))   ;NOW UNRELOCATE PC
        ((M-2) (LISP-BYTE %%FEFH-MIN-ARGS-FOR-VANL) MD)
        (JUMP-GREATER-THAN M-2 A-R QL-FAST-TOO-FEW-ARGS)
        ((M-E) (LISP-BYTE %%FEFH-MAX-ARGS-FOR-VANL) MD)
        ((M-C) SUB M-E A-R)
        (DISPATCH-XCT-NEXT (BYTE-FIELD 4 0) M-C D-PUSH-NILS)
       (JUMP-LESS-THAN M-E A-R QL-FAST-TOO-MANY-ARGS)  ;if xfers, there is a garbage on us.
        (#+lambda POPJ-AFTER-NEXT
         (LOCATION-COUNTER) ADD M-1 A-J #+lambda OUTPUT-SELECTOR-LEFTSHIFT-1)
       (#+exp popj-after-next (A-IPMARK) M-AP)  ;NO OPEN CALL BLOCK YET
#+exp (no-op)

QLENTR-FAST-FIXED-W-LOCALS
        ((M-1) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH 1) (A-CONSTANT 0))   ;NOW UNRELOCATE PC
        ((M-E) (LISP-BYTE %%FEFH-ARGS-FOR-FAWL) MD)
        ((A-LOCALP) M+A+1 M-E A-AP)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((m-TEM1) M+A+1 M-E (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-INDEX-INDIRECT) DPB M-R
                 (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED) A-TEM1)
        (DISPATCH-XCT-NEXT (LISP-BYTE %%FEFH-LOCALS-FOR-FAWL) MD D-PUSH-NILS)
       ((LOCATION-COUNTER) ADD M-1 A-J #+lambda OUTPUT-SELECTOR-LEFTSHIFT-1)
        (POPJ-EQUAL M-E A-R)
       ((A-IPMARK) M-AP)        ;NO OPEN CALL BLOCK YET
        (JUMP FAST-FIXED-WNA)   ;DROP THRU ON WRONG NUMBER ARGS.

QLENTR-FAST-VAR-W-LOCALS
        ((M-2) (LISP-BYTE %%FEFH-MIN-ARGS-FOR-VAWL) MD)
        (JUMP-GREATER-THAN M-2 A-R QL-FAST-TOO-FEW-ARGS)
        ((M-E) (LISP-BYTE %%FEFH-MAX-ARGS-FOR-VAWL) MD)
        ((A-LOCALP) M+A+1 M-E A-AP)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((m-TEM1) M+A+1 M-E (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-INDEX-INDIRECT) DPB M-R
                 (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED) A-TEM1)
        ((M-C) SUB M-E A-R)
        (DISPATCH-XCT-NEXT (BYTE-FIELD 4 0) M-C D-PUSH-NILS)
       (JUMP-LESS-THAN M-E A-R QL-FAST-TOO-MANY-ARGS)  ;if xfers, there is a garbage on us.
        (DISPATCH-XCT-NEXT (LISP-BYTE %%FEFH-LOCALS-FOR-VAWL) MD D-PUSH-NILS)
       ((M-1) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH 1) (A-CONSTANT 0))    ;NOW UNRELOCATE PC
        (#+lambda POPJ-AFTER-NEXT
         (LOCATION-COUNTER) ADD M-1 A-J #+lambda OUTPUT-SELECTOR-LEFTSHIFT-1)
       (#+exp popj-after-next (A-IPMARK) M-AP)  ;NO OPEN CALL BLOCK YET
#+exp (no-op)

;;; "LINEAR" ENTER
;   M-A HAS PNTR TO FEF TO CALL
;   m-ap has already been updated
;   m-s has previous m-ap (for frame leaving)
;   M-R HAS NUMBER OF ARGUMENTS, (%%lp-ens-num-args-supplied already stored)
;WE DON'T SUPPORT USER COPYING AND FORWARDING OF FEFS,
;SO IT'S NOT NECESSARY TO CALL THE TRANSPORTER EVERYWHERE.
;CAN SEQUENCE BREAK ONCE WE GET PAST THE ARGUMENTS AND START DOING VARIABLE
;INITIALIZATIONS, WHICH CAN CAUSE ERRORS.  THIS WILL INVALIDATE A-LCTYP BUT
;PRESERVE THE LETTERED M-REGISTERS.
;*** WE STILL HAVE A PROBLEM WITH M-ERROR-SUBSTATUS NOT BEING PRESERVED

x-trap-on-next-call (misc-inst-entry %trap-on-next-call)
#+LAMBDA(popj-after-next                   ;1_19 is dispatch-start-mem-read bit.
         (d-qmrcl-qlentr) (a-constant (plus 1_19. (i-mem-loc qlentr-trap-on-call))))
#+lambda(no-op)
#+exp   ((m-tem) (a-constant (i-mem-loc qlentr-trap-on-call)))
#+exp   (dispatch write-dispatch-memory d-qmrcl-qlentr (byte-field 0 0) (i-arg (a-mem-loc a-tem)))
#+exp   (popj)

qlentr-trap-on-call
        ;; Immediately set dispatch back to normal mode, so that traps out of QLENTR and
        ;; QMRCL-TRAP will use the right entrypoint when the error-handler starts up.
#+LAMBDA((d-qmrcl-qlentr) (a-constant (plus 1_19. (i-mem-loc qlentr)))) ;dispatch-start-mem-read
#+exp   ((m-tem) (a-constant (i-mem-loc qlentr)))
#+exp   (dispatch write-dispatch-memory d-qmrcl-qlentr (byte-field 0 0) (i-arg (a-mem-loc a-tem)))
        (call qlentr)
        (jump qmrcl-trap)

qlentr-meter
        (JUMP-IF-BIT-SET (LISP-BYTE %%METER-FUNCTION-ENTRY-EXIT-ENABLE)
                M-METER-ENABLES METER-FUNCTION-ENTRY)
QLENTR
#-LAMBDA((VMA-START-READ) M-A)  ;THIS CYCLE STARTED BY DISPATCH-START-MEM-READ ON LAMBDA.
        (CHECK-PAGE-READ)
   ;no transport necessary since MD not a pointer.
meter-function-entry-return
        (DISPATCH-XCT-NEXT (LISP-BYTE %%HEADER-TYPE-FIELD) MD D-QLENTR-DISPATCH)
       ((M-J) (LISP-BYTE %%FEFH-PC) MD) ;MAY GET CHANGED DUE TO OPTIONAL ARGS.


(LOCALITY D-MEM)
(START-DISPATCH 5 0)    ;DISPATCH ON HEADER SUBTYPE
D-QLENTR-DISPATCH
        (P-BIT ILLOP)   ;%HEADER-TYPE-ERROR
        (QLENTR-NORMAL) ;%HEADER-TYPE-FEF
        (P-BIT ILLOP)   ;%HEADER-TYPE-ARRAY-LEADER
        (P-BIT ILLOP)   ;unused
        (P-BIT ILLOP)   ;%HEADER-TYPE-FLONUM
        (P-BIT ILLOP)   ;%HEADER-TYPE-COMPLEX
        (P-BIT ILLOP)   ;%HEADER-TYPE-BIGNUM
        (P-BIT ILLOP)   ;%HEADER-TYPE-RATIONAL
        (QLENTR-FAST-FIXED-NO-LOCALS)   ;%HEADER-TYPE-FAST-FEF-FIXED-ARG-NO-LOCALS
        (QLENTR-FAST-VAR-NO-LOCALS)     ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
        (QLENTR-FAST-FIXED-W-LOCALS)    ;%HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
        (QLENTR-FAST-VAR-W-LOCALS)      ;%HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
(REPEAT NHDUSD (P-BIT ILLOP))
(END-DISPATCH)
(LOCALITY I-MEM)

QLENTR-NORMAL
        (JUMP-IF-BIT-CLEAR-XCT-NEXT
         (LISP-BYTE %%FEFH-GET-SELF-MAPPING-TABLE) MD
         QLENTR-NOT-METHOD)
       ((M-D) Q-POINTER READ-MEMORY-DATA)       ;GET FEF HEADER WORD
                                        ; ALSO NOTE RELATIVE TO FEF STILL
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (CALL-IF-BIT-CLEAR (LISP-BYTE %%LP-CLS-SELF-MAP-PROVIDED) PDL-INDEX-INDIRECT
                           QLENTR-GET-SELF-MAPPING-TABLE)
QLENTR-NOT-METHOD
        ((M-ERROR-SUBSTATUS) M-ZERO)    ;CLEAR OUT ERRORS
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEFH-FAST-ARG) M-D QRENT)  ;NO FAST-OPTION
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %FEFHI-FAST-ARG-OPT)))
        (CHECK-PAGE-READ)               ;GET FAST-OPTION WORD
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((M-E) (LISP-BYTE %%FEFHI-FSO-MAX-ARGS) READ-MEMORY-DATA)
        (JUMP-IF-BIT-SET (LISP-BYTE %%ARG-DESC-QUOTED-REST) READ-MEMORY-DATA QLFOA1)
        (JUMP-IF-BIT-SET (LISP-BYTE %%ARG-DESC-EVALED-REST) READ-MEMORY-DATA QLFOA1)
        ((A-LOCALP) M+A+1 M-E A-AP)
        ((m-TEM1) M+A+1 M-E (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;Quickly detect case of all desired spread args supplied.
        (JUMP-EQUAL-XCT-NEXT M-E A-R QFL1)
       ((PDL-INDEX-INDIRECT) DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED) A-TEM1)
        ((M-C) (LISP-BYTE %%FEFHI-FSO-MIN-ARGS) READ-MEMORY-DATA)
        (CALL-GREATER-THAN M-C A-R SET-TOO-FEW-ARGS)
        (CALL-LESS-THAN M-E A-R SET-TOO-MANY-ARGS)
QFL2    (JUMP-LESS-OR-EQUAL M-E A-R QFL1)
        ((M-E) SUB M-E (A-CONSTANT 1))
QFL3    ((PDL-PUSH) A-V-NIL)    ;DEFAULT UNSUPPLIED ARGS TO NIL
        (JUMP-GREATER-THAN-XCT-NEXT M-E A-R QFL3)
       ((M-E) SUB M-E (A-CONSTANT 1))
QFL1    ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %FEFHI-MISC)))
        (CHECK-PAGE-READ)
        ((M-T) (LISP-BYTE %%FEFHI-MS-LOCAL-BLOCK-LENGTH) READ-MEMORY-DATA)
;M-T has number of locals not yet pushed.  Push NILs for them.
QFL1C   (JUMP-EQUAL M-T A-ZERO QFL1A)
QFL1B   ((PDL-PUSH) A-V-NIL)    ;INIT LOCAL BLOCK TO NIL
        (JUMP-GREATER-THAN-XCT-NEXT M-T (A-CONSTANT 1) QFL1B)
       ((M-T) SUB M-T (A-CONSTANT 1))
QFL1A
;If any args or locals should be bound as special, go do that.
        (CALL-IF-BIT-SET (LISP-BYTE %%FEFH-SV-BIND) M-D FRMBN1)
;FINISH LINEARLY ENTERING
QLENX   ((M-TEM) DPB M-A (BYTE-FIELD Q-POINTER-WIDTH 1) (A-CONSTANT 0)) ;NOW UNRELOCATE PC
        ((LOCATION-COUNTER) ADD M-TEM A-J #+lambda OUTPUT-SELECTOR-LEFTSHIFT-1)
 ;      (CALL-IF-BIT-SET M-TRAP-ON-CALLS QMRCL-TRAP)    ;See QLENTR-TRAP-ON-CALL, above.
        (POPJ-EQUAL-XCT-NEXT M-ERROR-SUBSTATUS A-ZERO)  ;RETURN TO MAIN LOOP IF NO ERROR
       ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-AP)        ;NO OPEN CALL BLOCK YET
QLEERR  ((PDL-PUSH) DPB M-ERROR-SUBSTATUS Q-POINTER ;PUSH M-ERROR-SUBSTATUS
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))) ; ONTO STACK SO ERROR HANDLER
        (CALL TRAP)                                            ; CAN FIND IT.
   (ERROR-TABLE FUNCTION-ENTRY) ;This table entry is specially known about.

METER-FUNCTION-ENTRY
#-LAMBDA((VMA-START-READ) M-A)  ;This cycle started by DISPATCH-START-MEM-READ on LAMBDA.
        (CHECK-PAGE-READ)
        ((PDL-PUSH) MD)
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-FUNCTION-ENTRY-EVENT)))
        ((PDL-PUSH) M-A)
        (CALL-XCT-NEXT METER-MICRO-WRITE-HEADER)
       ((A-METER-LENGTH) (A-CONSTANT 1))        ;Number of meters pushed
        (jump-xct-next meter-function-entry-return)
       ((MD) PDL-POP)

SET-TOO-FEW-ARGS
        (POPJ-AFTER-NEXT (M-QBTFA) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)
       (NO-OP)

SET-TOO-MANY-ARGS
        (POPJ-AFTER-NEXT (M-QBTMA) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)
       (NO-OP)

;Here for function with fast arg option that wants a rest arg.
;M-E has # reg+opt args.  PI points to entry-state word.
QLFOA1  ((M-C) (LISP-BYTE %%FEFHI-FSO-MIN-ARGS) READ-MEMORY-DATA)
;Initialize call type (normal vs lexpr vs fexpr);
        ((A-LCTYP) (LISP-BYTE %%LP-ENS-LCTYP) PDL-INDEX-INDIRECT)
        (JUMP-NOT-EQUAL A-LCTYP M-ZERO QLFRA1)  ;Called with LEXPR/FEXPR call
QLFRA2  (CALL-GREATER-THAN M-C A-R SET-TOO-FEW-ARGS)
        (JUMP-LESS-THAN M-E A-R QLFSA2)
        ;; Called with just spread arguments.
        ;; If the rest arg will be NIL, push NILs for it and any missing optionals.
        ((M-TEM) SUB M-E A-R)                   ;1- number of NILs to push
QLFSA1  ((PDL-PUSH) A-V-NIL)
        (JUMP-GREATER-THAN-XCT-NEXT M-TEM A-ZERO QLFSA1)
       ((M-TEM) SUB M-TEM (A-CONSTANT 1))
        ((Q-R) ADD M-E (A-CONSTANT (EVAL %LP-INITIAL-LOCAL-BLOCK-OFFSET)))
        ;; Args set up.  Set up entry-state and local-block (offset is in Q-R)
QLFOA5  ((m-TEM1) DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
QLFOA6  ((A-LOCALP) ADD Q-R A-AP)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((PDL-INDEX-INDIRECT) DPB Q-R (LISP-BYTE %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN)
                A-TEM1)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %FEFHI-MISC)))
        (CHECK-PAGE-READ)
        ((M-T) (LISP-BYTE %%FEFHI-MS-LOCAL-BLOCK-LENGTH) READ-MEMORY-DATA)
        (JUMP-XCT-NEXT QFL1C)
       ((M-T) SUB M-T (A-CONSTANT 1))           ;First local (rest arg) already pushed

        ;; Called with enough spread args to get into the rest arg
QLFSA2  (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) M+A+1 M-AP A-E)           ;First of rest, %LP-INITIAL-LOCAL-BLOCK-OFFSET = 1
        ((PDL-PUSH)             ;Push the rest-arg
                Q-POINTER M-K (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        ((m-TEM1) DPB M-R (LISP-BYTE %%LP-ENS-NUM-ARGS-SUPPLIED)
                (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                  (BYTE-VALUE %%LP-ENS-UNSAFE-REST-ARG 1))))
        (JUMP-XCT-NEXT QLFOA6)                  ;Put the local block after the supplied args
       ((Q-R) ADD M-R (A-CONSTANT (EVAL %LP-INITIAL-LOCAL-BLOCK-OFFSET)))

;Call with rest arg to a function which uses the fast arg option and wants a rest arg.
QLFRA1  ((M-TEM) SUB M-R (A-CONSTANT 1))        ;Number of spread args passed.
        (JUMP-EQUAL-XCT-NEXT M-E A-TEM QLFOA5)  ;Matches number desired, enter.
       ((Q-R) ADD M-TEM (A-CONSTANT (EVAL %LP-INITIAL-LOCAL-BLOCK-OFFSET)))
        (CALL-LESS-THAN M-E A-TEM ILLOP)        ;Too many spread args => lose.
;; Too few spread args => spread one off the rest arg.
        ((M-T) Q-TYPED-POINTER PDL-TOP)
        (JUMP-EQUAL M-T A-V-NIL QLFRA3) ;But if rest arg is NIL, pretend there was none.
        (CALL SPREAD-REST-ARG-ONCE)
        (JUMP-XCT-NEXT QLFRA1)
       ((M-R) ADD M-R (A-CONSTANT 1))

;Pop stack, decrement M-R and go to QLFRA2 (as if there was no rest arg).
QLFRA3  (JUMP-XCT-NEXT QLFRA2)
       ((M-R) PDL-POP SETA A-TEM)

;Pop a value off the stack, then push its car and its cdr.
;Also leaves the cdr in M-T, sans cdr code.
SPREAD-REST-ARG-ONCE
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CAR) Q-DATA-TYPE PDL-TOP CAR-PRE-DISPATCH-DIRECT)
;#+CADR (CALL-XCT-NEXT QCAR)
        (open-qcar-xct-next pdl-top)
       ((M-T) PDL-TOP)
        ((M-TEM) PDL-TOP)
        ((PDL-TOP) DPB M-T Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
;#+LAMBDA(DISPATCH-XCT-NEXT DISPATCH-WRITE-VMA
;                  (I-ARG INSTANCE-INVOKE-CDR) Q-DATA-TYPE M-TEM CDR-PRE-DISPATCH-DIRECT)
;#+CADR (CALL-XCT-NEXT QCDR)
        (open-qcdr-xct-next m-tem)
       ((M-T) Q-TYPED-POINTER M-TEM)
        (POPJ-AFTER-NEXT
         (PDL-PUSH) DPB M-T Q-TYPED-POINTER (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
       (NO-OP)

;Bind SELF-MAPPING-TABLE to the right mapping table
;for the flavor whose name is stored in the fef in M-A.
;Must not clobber: M-A, M-D (only pointer field matters), M-J, M-R, M-S.
QLENTR-GET-SELF-MAPPING-TABLE
;Get the FEFHI-MISC word which contains the index of the start of the ADL.
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %FEFHI-MISC)))
        (CHECK-PAGE-READ)
;       ((PDL-INDEX) M-AP)
  ;make the location-counter point inside the FEF, since if a sequence
  ;break occurs in GET-SELF-MAPPING-TABLE, SGENT will do a fetch from it
  ;before continuing QLENTR
  ;(also, location-counter can't be set to 0, since we want to check
  ; in SGLV that it always points within the current FEF.)
;       ((location-counter) dpb pdl-index-indirect (byte-field q-pointer-width 2) a-zero)
        ((location-counter) dpb m-fef (byte-field q-pointer-width #+lambda 2 #+exp 1) a-zero)
        ((m-TEM1) (LISP-BYTE %%FEFHI-MS-ARG-DESC-ORG) MD)
        ((m-TEM1) ADD (M-CONSTANT -1) A-TEM1)
;Access the word before the ADL.  It contains the flavor name.
        ((VMA-START-READ) ADD M-A A-TEM1)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
;Get the mapping table for SELF for that flavor.
        ((PDL-PUSH) M-A)
        (CALL-XCT-NEXT GET-SELF-MAPPING-TABLE)
       ((M-B) Q-TYPED-POINTER MD)       ;Method-flavor-name into M-B.
        ((M-A) PDL-POP)
;Bind SELF-MAPPING-TABLE to it.
        (JUMP-XCT-NEXT BIND-SELF-MAP)
       ((M-B) M-T)

;Index wrt mapping table of the array leader slot that holds the name of
;the method-flavor the mapping table is for.
(ASSIGN %MAPPING-TABLE-FLAVOR -3)
XGET-SELF-MAPPING-TABLE
        (MISC-INST-ENTRY %GET-SELF-MAPPING-TABLE)
        ((M-B) Q-TYPED-POINTER PDL-POP)

;Given flavor name in M-B, return mapping table in M-B.
GET-SELF-MAPPING-TABLE
        ((M-T) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-SELF-MAPPING-TABLE)
        (JUMP-EQUAL M-T A-V-NIL GET-SELF-MAPPING-TABLE-1)
;If we currently have a non-nil mapping table, is it for this flavor?
;The mapping table can be a flavor name.  If so, it's good iff equals desired flavor.
        (POPJ-EQUAL M-T A-B)
;Any other non-array means it's the wrong table.
        (JUMP-DATA-TYPE-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER))
                GET-SELF-MAPPING-TABLE-1)
;An array: get the leader element that says what it is for.
        ((VMA-START-READ) ADD M-T (A-CONSTANT %MAPPING-TABLE-FLAVOR)) ;this is negative
        (CHECK-PAGE-READ)                       ;Look in leader element 1.
        (DISPATCH TRANSPORT MD)
        ((VMA-START-READ) ADD MD (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-TYPENAME)))
        (CHECK-PAGE-READ)                       ;Access the flavor name in the flavor object
        (DISPATCH TRANSPORT MD)
        (POPJ-EQUAL MD A-B)                     ;Match => return this array, no need to search
GET-SELF-MAPPING-TABLE-1
        ((M-TEM) A-SELF)                        ;If SELF is not an instance, return NIL.
        (JUMP-DATA-TYPE-NOT-EQUAL M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE))
                XFALSE)
        ((VMA-START-READ) A-SELF)               ;Get the flavor object from SELF.
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ((VMA-START-READ) ADD MD (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-MAPPING-TABLE-ALIST)))
        (CHECK-PAGE-READ)                       ;Access the word in the flavor object
        (DISPATCH TRANSPORT MD)                 ;that contains the alist of mapping tables.
        ((PDL-PUSH) DPB M-D Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-PUSH) M-B)        ;Do (ASSQ method-flavor-name alist)
        ((PDL-PUSH) MD)
        (CALL XASSQ)
        (CALL-EQUAL M-T A-V-NIL TRAP)           ;The alist MUST have an entry
    (ERROR-TABLE NO-MAPPING-TABLE)
        ((M-D) PDL-POP)
        (JUMP QCDDR)                            ;Return CDDR of ASSQ's value.

;Set SELF-MAPPING-TABLE to our arg,
;and set the bit in the open call block saying we are providing it.
;Destination is either D-IGNORE or D-LAST.
XSET-SELF-MAPPING-TABLE (MISC-INST-ENTRY %SET-SELF-MAPPING-TABLE)
        ((M-TEM) MACRO-IR-DEST)
        (JUMP-EQUAL-XCT-NEXT M-TEM A-ZERO XSET-SELF-MAPPING-TABLE-1)
       ((A-SELF-MAPPING-TABLE) Q-TYPED-POINTER PDL-POP)
;       (JUMP-IF-BIT-CLEAR M-INST-DEST-LOW-BIT XSET-SELF-MAPPING-TABLE-1)
;If this is D-LAST, pop the last arg (D-LAST will push it back on).
        ((M-T) PDL-POP)
XSET-SELF-MAPPING-TABLE-1
        ((PDL-INDEX) A-IPMARK)
;Is the function we will be calling a symbol?
;If so, return, since we can't be sure what mapping table it wants.
        (POPJ-DATA-TYPE-EQUAL PDL-INDEX-INDIRECT (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SYMBOL)))
;Otherwise, set the bit saying that the function's mapping table is provided.
        (POPJ-AFTER-NEXT
         (PDL-INDEX) ADD PDL-INDEX (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((PDL-INDEX-INDIRECT) IOR PDL-INDEX-INDIRECT
         (A-CONSTANT (BYTE-VALUE %%LP-CLS-SELF-MAP-PROVIDED 1)))

;LINEAR ENTER WITHOUT FAST OPTION
; M-A FEF                       M-R number of args called with
; M-B flags/temp                M-Q bind desc Q
; M-C flags/temp                M-I address of bind desc
; M-D pdl index of arg          M-J start PC of FEF
; M-E count of bind descs       M-S pdl index of previous frame
; M-T address of sv slot        M-K temp

QRENT
;Initialize call type (normal vs lexpr vs fexpr);
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        ((A-LCTYP) (LISP-BYTE %%LP-ENS-LCTYP) PDL-INDEX-INDIRECT)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %FEFHI-MISC)))
        (CHECK-PAGE-READ)
        ((M-D PDL-INDEX) ADD M-AP
                (A-CONSTANT (EVAL %LP-INITIAL-LOCAL-BLOCK-OFFSET)))        ;-> FIRST ARG
        ((M-T) output-selector-mask-25                 ; -> S-V slots
               add m-a (a-constant (plus (eval %fefhi-special-value-cell-pntrs)
                                         (byte-value q-data-type dtp-locative))))
        ((A-ARGS-LEFT) M-R)                                           ;# ARGS YET TO DO
        ((m-TEM1) (LISP-BYTE %%FEFHI-MS-ARG-DESC-ORG) READ-MEMORY-DATA)
        ((M-I) ADD M-A A-TEM1)                                      ;-> FIRST BIND DESC
        ((m-i) q-pointer m-i (a-constant (byte-value q-data-type dtp-locative)))
        ((M-E) (LISP-BYTE %%FEFHI-MS-BIND-DESC-LENGTH) READ-MEMORY-DATA)  ;# BIND DESCS
        ((A-LOCALP) SETO)                           ;SIGNAL LOCAL BLOCK NOT YET LOCATED
        (JUMP-EQUAL A-LCTYP M-ZERO QBINDL)
        ((A-ARGS-LEFT) ADD (M-CONSTANT -1) A-ARGS-LEFT) ;WAS FEXPR OR LEXPR CALL
                        ;FLUSH NO-SPREAD-ARG AND PROCESS ANY SPREAD ARGS
;BIND LOOP USED WHILE ARGS REMAIN TO BE PROCESSED
QBINDL  (JUMP-GREATER-OR-EQUAL M-ZERO A-ARGS-LEFT QBD0) ;OUT OF SPREAD ARGS
        (JUMP-LESS-THAN
                M-E (A-CONSTANT 1) QBTMA1) ;OUT OF BIND DESC, TOO MANY ARGS
        ((VMA-START-READ) M-I)          ;ACCESS WORD OF BINDING OPTIONS
        (CHECK-PAGE-READ)
        ((M-E) SUB M-E (A-CONSTANT 1))
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEF-NAME-PRESENT) READ-MEMORY-DATA QBNDL1)
        ((M-I) ADD A-ZERO M-I ALU-CARRY-IN-ONE) ;SKIP NAME Q IF PRESENT
QBNDL1  (DISPATCH-XCT-NEXT (LISP-BYTE %%FEF-ARG-SYNTAX) READ-MEMORY-DATA QREDT1)
       ((M-Q) READ-MEMORY-DATA)         ;SAVE BIND DESC IN M-Q

QREW1   (CALL-LESS-THAN M-E (A-CONSTANT 1) ILLOP)
        ((VMA-START-READ) M-I)          ;ACCESS WORD OF BINDING OPTIONS
        (CHECK-PAGE-READ)
        ((M-E) SUB M-E (A-CONSTANT 1))
        ((M-Q) READ-MEMORY-DATA)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEF-NAME-PRESENT) M-Q QBNDL2)
        ((M-I) ADD A-ZERO M-I ALU-CARRY-IN-ONE) ;SKIP NAME Q IF PRESENT
QBNDL2  ((M-TEM) (LISP-BYTE %%FEF-ARG-SYNTAX) M-Q)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT 2) QBNDL2-NOT-REST)
        (CALL-GREATER-THAN M-ZERO A-LOCALP QLLOCB)      ;SET UP LOCAL BLOCK OVER ARG
        ((PDL-POINTER) M-D)             ;SO DONT STORE LOCALS OVER ARG
        (CALL-IF-BIT-SET (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBLSPCL)
        (JUMP-XCT-NEXT QBD1)
       ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE)

;Rest arg is supplied, but we want a spread arg.
;Spread the first element of the rest arg, putting the car and cdr back on the stack,
;then go process the car as a spread arg.
QBNDL2-NOT-REST
        ((PDL-POINTER) M-D)
        ((M-1) Q-TYPED-POINTER PDL-TOP)
        (JUMP-EQUAL M-1 A-V-NIL QBNDL2-REST-ARG-NIL)
        ((M-K) M-T)
        (CALL-XCT-NEXT SPREAD-REST-ARG-ONCE)  ;Clobbers M-T.
       ((M-R) ADD M-R (A-CONSTANT 1))
        ((MD) M-Q)      ;QBNDL1 wants the ADL word in MD.
        ((M-T) M-K)
        (JUMP-XCT-NEXT QBNDL1)
       ((A-ARGS-LEFT) M+A+1 M-ZERO A-ARGS-LEFT)

;We want a spread arg, we have a rest arg, but that rest arg is NIL.
;So decide that we are really out of args and default this one (or get error).
QBNDL2-REST-ARG-NIL
        ((MD) M-Q)
        (JUMP-XCT-NEXT QBD2A)
       (PDL-POP)

;OPTIONAL ARG IS PRESENT, SPACE PAST INITIALIZATION INFO IF ANY
QBROP1  (DISPATCH (LISP-BYTE %%FEF-INIT-OPTION) M-Q QBOPNP)

QBOSP   (JUMP-XCT-NEXT QBRQA)
       ((M-I) ADD A-ZERO M-I ALU-CARRY-IN-ONE)

QBOASA  ((VMA-START-READ M-I) ADD A-ZERO M-I ALU-CARRY-IN-ONE)
        (CHECK-PAGE-READ)
        ((M-J) Q-POINTER READ-MEMORY-DATA)      ;START LATER TO AVOID CLOBBERING
;REQUIRED ARGUMENT IS PRESENT
QBRQA   (CALL-IF-BIT-SET (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBSPCL)

;ENTER HERE WHEN ARG HAS BEEN BOUND.  THESE CHECKS ONLY CAUSE EXCEPTIONS
QBDL1   ;(DISPATCH-XCT-NEXT (LISP-BYTE %%FEF-DES-DT) M-Q QBDDT)
       ;((M-C) Q-DATA-TYPE PDL-INDEX-INDIRECT)
QBDDT1  ;(DISPATCH-XCT-NEXT (LISP-BYTE %%FEF-QUOTE-STATUS) M-Q QBEQC)
       ;((M-C) Q-FLAG-BIT PDL-INDEX-INDIRECT)
QBEQC1  ((M-D PDL-INDEX) ADD M-D A-ZERO ALU-CARRY-IN-ONE) ;NEXT ARG SLOT
        ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE) ;NEXT BIND DESC ENTRY
        (JUMP-XCT-NEXT QBINDL)                  ;PROCEED TO NEXT ARG
       ((A-ARGS-LEFT) ADD (M-CONSTANT -1) A-ARGS-LEFT)

;REST ARG - FOR NOW I ASSUME MICRO-COMPILED FUNCTIONS DO STORE CDR CODES
QBRA    (CALL-NOT-EQUAL A-LCTYP M-ZERO ILLOP)   ;IF A NON-SPREAD ARG, SHOULD NOT
                                                ;GET TO REST ARG HERE.
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)      ;MAKE PNTR TO LIST OF ARGS
       ((M-K) M-D)
        (CALL-GREATER-THAN-XCT-NEXT M-ZERO A-LOCALP QLLOCB)
       ((M-D) ADD M-D A-ARGS-LEFT)              ;LOCATE LOCAL BLOCK AFTER LAST ARG
        ((PDL-PUSH) DPB M-K     ;STORE REST ARG AS FIRST LOCAL
                Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        ((M-R) DPB M-MINUS-ONE (LISP-BYTE %%LP-ENS-UNSAFE-REST-ARG-1) A-R)
        (CALL-IF-BIT-SET (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBLSPCL)
        ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE) ;ADVANCE TO NEXT BIND DESC

QBD0    (JUMP-NOT-EQUAL A-LCTYP M-ZERO QREW1)   ;ALSO IS A NO-SPREAD ARG
;BINDING LOOP FOR WHEN ALL ARGS HAVE BEEN USED UP
QBD1    (JUMP-LESS-THAN
                M-E (A-CONSTANT 1) QBD2)        ;JUMP IF FINISHED ALL BINDING
        ((VMA-START-READ) M-I)                  ;GET NEXT BINDING DESC Q
        (CHECK-PAGE-READ)
        ((M-E) SUB M-E (A-CONSTANT 1))
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEF-NAME-PRESENT) READ-MEMORY-DATA QBD2A)
        ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE) ;SKIP NAME IF PRESENT
QBD2A   (DISPATCH-XCT-NEXT (LISP-BYTE %%FEF-ARG-SYNTAX) READ-MEMORY-DATA QBDT2)
       ((M-Q) READ-MEMORY-DATA)                 ;SAVE BINDING DESC IN M-Q

;LOCATE LOCAL BLOCK TO WHERE M-D POINTS
;AFTER THIS HAS BEEN CALLED, USE PDL-PUSH TO STORE LOCALS
QLLOCB  (declare (args a-d))
        (POPJ-AFTER-NEXT                ;PDL-BUFFER-PTR SHOULD BE SET ALREADY?
                                        ;  --NOT IF TOO FEW ARGS FOR ONE--.
         (PDL-POINTER) SUB M-D (A-CONSTANT 1))  ;FIRST PUSH WILL STORE @ M-D
       ((A-LOCALP) M-D)                 ;PDL INDEX OF LOCALS

;GOT ARG DESCRIPTOR WHEN OUT OF ARGS
QBTFA1  (JUMP-XCT-NEXT QBOPT2)                  ;SUPPLY ARG OF NIL
       ((M-QBTFA) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)        ;GIVE TOO FEW ARGS ERR LATER

QBRA1   (CALL-GREATER-THAN M-ZERO A-LOCALP QLLOCB)      ;REST ARG MISSING, MAKE 1ST LOCAL NIL
QBOPT2  ((PDL-PUSH) A-V-NIL)    ;STORE MISSING ARG AS NIL (CDR CODE?)
QBD1A   (CALL-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBLSPCL)
       ((M-D) ADD M-D A-ZERO ALU-CARRY-IN-ONE)
QBDIN1  (JUMP-XCT-NEXT QBD1)
       ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE)

;INTERNAL
QBDINT  (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBDIN2)
        (JUMP-XCT-NEXT QBDIN1)  ;IF SPECIAL, NO LOCAL SLOT, TAKES S-V SLOT
       ((M-T) ADD M-T A-ZERO ALU-CARRY-IN-ONE)

QBDIN2  (JUMP-XCT-NEXT QBOPT2)  ;IF LOCAL, IGNORE AT BIND TIME BUT RESERVE LOCAL SLOT
       (CALL-GREATER-THAN M-ZERO A-LOCALP QLLOCB)       ;ALSO MUST LOCATE LOCAL BLOCK


;FREE
QBDFRE  (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBDIN1) ;TAKES NO LCL SLOT
        (JUMP-XCT-NEXT QBDIN1)                              ;IF SPECIAL, TAKES S-V SLOT
       ((M-T) ADD M-T A-ZERO ALU-CARRY-IN-ONE)

;AUX
QBDAUX  (CALL-GREATER-THAN M-ZERO A-LOCALP QLLOCB)      ;LOCATE LOCAL BLOCK,
                                                ; THEN DROP THROUGH TO INITIALIZE
QBOPT4  (DISPATCH (LISP-BYTE %%FEF-INIT-OPTION) M-Q QBOPTT)

;OPTIONAL NOT PRESENT
QBOPT1  (JUMP-GREATER-THAN M-ZERO A-LOCALP QBOPT4)
        (CALL ILLOP)            ;SHOULDN'T HAVE ARGS AFTER LOCAL BLOCK IS LOCATED

;OPTIONAL ARGUMENT INIT VIA ALTERNATE STARTING ADDRESS AND NOT PRESENT
;LEAVE STARTING ADDRESS ALONE AND INIT TO SELF, COMPILED CODE WILL
;RE-INIT.  BUT DON'T FORGET TO SKIP OVER THE START ADDRESS.
QBOPT5  ((M-I) ADD M-I (A-CONSTANT 1))
;OPTIONAL OR AUX, INIT TO SELF OR NONE, LATER MAY BE REINITED BY COMPILED CODE
QBOPT3  (JUMP-IF-BIT-CLEAR (LISP-BYTE %%FEF-SPECIAL-BIT)
                        M-Q QBOPT2)             ;LOCAL, INIT TO NIL
        ((VMA-START-READ) M-T)                  ;SPECIAL, GET POINTER TO VALUE CELL
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-NO-TRAP READ-MEMORY-DATA)   ;FETCH EXTERNAL VALUE CELL.
                                                        ;MUST GET CURRENT VALUE, BUT NOT BARF
                                                        ;IF DTP-NULL.  MUST NOT LEAVE AN EVCP
                                                        ;SINCE THAT WOULD SCREW PREVIOUS
                                                        ;BINDING IF IT WAS SETQ'ED.
        ((PDL-PUSH) READ-MEMORY-DATA)
;THIS IS LIKE QBD1A, EXCEPT THAT THE THING WE ARE BINDING IT TO
;MAY BE DTP-NULL, WHICH IS ILLEGAL TO LEAVE ON THE PDL BUFFER.
;ALSO, THE VARIABLE IS KNOWN NOT TO BE AN ARGUMENT THAT WAS SUPPLIED,
;SO THERE'S NO DANGER OF CLOBBERING USEFUL DEBUGGING INFORMATION
        (CALL-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%FEF-SPECIAL-BIT) M-Q QBLSPCL)
       ((M-D) ADD M-D A-ZERO ALU-CARRY-IN-ONE)
        ((PDL-TOP) A-V-NIL)     ;STORE NIL OVER POSSIBLE GARBAGE
        (JUMP-XCT-NEXT QBD1)
       ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE)

;INIT TO POINTER
QBOPNR  ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE)
        ((VMA-START-READ) M-I)                  ;FETCH THING TO INIT TOO, TRANSPORT IT
QBDR1   (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        (JUMP-XCT-NEXT QBD1A)
       ((PDL-PUSH) READ-MEMORY-DATA)

;INIT TO C(POINTER)
QBOCPT  ((M-I) ADD M-I A-ZERO ALU-CARRY-IN-ONE)
        ((VMA-START-READ) M-I)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        (JUMP-XCT-NEXT QBDR1)
       ((VMA-START-READ) READ-MEMORY-DATA)

;INIT TO CONTENTS OF "EFFECTIVE ADDRESS"
QBOEFF  ((M-I VMA-START-READ) ADD M-I A-ZERO ALU-CARRY-IN-ONE)
        (CHECK-PAGE-READ)
        (DISPATCH-XCT-NEXT (BYTE-FIELD 3 6) READ-MEMORY-DATA QBOFDT) ;DISPATCH ON REG
       ((M-1) (BYTE-FIELD 6 0) READ-MEMORY-DATA)        ;PICK UP DELTA FIELD

QBFE    ((M-1) (BYTE-FIELD 8 0) READ-MEMORY-DATA)       ;FULL DELTA
        (JUMP-XCT-NEXT QBDR1)
       ((VMA-START-READ) ADD M-A A-1)           ;FETCH FROM FEF OF FCN ENTERING

QBQT    (JUMP-XCT-NEXT QBDR1)
       ((VMA-START-READ) ADD M-1 A-V-CONSTANTS-AREA)    ;FETCH FROM CONSTANTS PAGE

QBDLOC  (CALL-GREATER-THAN M-ZERO A-LOCALP ILLOP) ;TRYING TO ADDRESS LOCALS BEFORE LOCATED
        ((PDL-INDEX) ADD M-1 A-LOCALP)  ;FETCH LOCAL
        (JUMP-XCT-NEXT QBD1A)
       ((PDL-PUSH) PDL-INDEX-INDIRECT)

QBDARG  ((PDL-INDEX) ADD M-1 A-AP ALU-CARRY-IN-ONE)     ;FETCH ARG
        (JUMP-XCT-NEXT QBD1A)           ;(%LP-INITIAL-LOCAL-BLOCK-OFFSET = 1)
       ((PDL-PUSH) PDL-INDEX-INDIRECT)

;TOO MANY ARGS
QBTMA2  ((M-QBTMA) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)
        (DISPATCH-XCT-NEXT  (LISP-BYTE %%FEF-ARG-SYNTAX) M-Q QBDT2) ;FINISH BIND DESCS
       ((M-D) ADD M-D A-ARGS-LEFT)      ;ADVANCING LCL PNTR PAST THE EXTRA ARGS

;TOO MANY ARGS AND BIND DESC LIST ALL USED UP
QBTMA1  ((M-QBTMA) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)
        ((M-D) ADD M-D A-ARGS-LEFT)     ;ADVANCE LCL PNTR PAST THE EXTRA ARGS

;HERE WHEN BIND DESC LIST HAS BEEN USED UP
;By now, M-R contains the number of args in its low 6 bits
;and the flag saying there is an unsafe rest arg in bit 7.  (Bit 6 is zero).
QBD2    ((m-c) a-v-nil)                                ;QBSPCL leaves garbage in M-C.
        (CALL-GREATER-THAN M-ZERO A-LOCALP QLLOCB)  ;SET UP LOCAL BLOCK
        ((M-TEM) A-LOCALP)
        ((M-TEM) SUB M-TEM A-AP)
        ((m-TEM1) DPB M-TEM (LISP-BYTE %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN)
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ;ASSEMBLE ENTRY STATE Q
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-ENTRY-STATE)))
        (JUMP-XCT-NEXT QLENX)
       ((PDL-INDEX-INDIRECT) DPB M-R
                (LISP-BYTE %%LP-ENS-NUM-ARGS-AND-UNSAFE-FLAG) A-TEM1)

;COME HERE WHEN BINDING A SPECIAL TO A LOCAL
QBLSPCL ((PDL-INDEX) PDL-POINTER)

;COME HERE WHEN BINDING A SPECIAL
; NOTE CODE BELOW CLEARS %%FEFHI-SVM-HIGH-BIT IN M-C.  THIS IS FOR THE BENEFIT OF
;FRMBN1.  ITS A CROCK, BUT NON-MODULARITY WAS DEEMED WORTH IT BECAUSE OTHERWISE
;CLEAR WOULD HAVE TO BE DONE IN A LOOP.
;NOTE THAT IF WE CAME HERE FROM QBOPT3 THERE MAY BE ILLEGAL DATA TEMPORARILY ON THE PDL BUFFER!
;LETTERED REGS CLOBBERED: M-B, M-K.  M-T HAS S-V PNTR TABLE ADDR, M-C HAS FLAGS.
;PDL-INDEX points to the slot on the stack containing the new value.
QBSPCL  ((M-B) PDL-INDEX-INDIRECT)              ;GET VAL TO BIND TO (ARG OR LOCAL)
                                ;Note that PDL-INDEX is clobbered by overflow trap.
        ((VMA-START-READ) M-T)                  ;GET SPECIAL VALUE CELL POINTER
        (CHECK-PAGE-READ)
                ;following inst TESTs P.C.E. (M-CONST JUST HAPPENED TO
                ; BE AROUND AT THE WRONG TIME).
        ((M-1) ADD (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-QLBNDP)
        ((M-1) SUB M-1 A-QLBNDH)
        (CALL-IF-BIT-CLEAR BOXED-SIGN-BIT M-1 TRAP)
            (ERROR-TABLE PDL-OVERFLOW SPECIAL)
                ;The pointer must be EVCP, but must check for OLD space.
                ;The dispatch just drops through if EVCP to new space.
        (DISPATCH TRANSPORT-BIND-READ-WRITE READ-MEMORY-DATA)
  ;     ((VMA-START-READ) DPB READ-MEMORY-DATA Q-POINTER
  ;             (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
  ;     (CHECK-PAGE-READ)                       ;GET CONTENTS OF INTERNAL VALUE CELL
;CODE BELOW IS LOGICALLY SOMEWHAT SIMILAR TO QBND2.
  ;     (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)   ;CHASE FORWARDING PTR IF ANY
        ((M-K) Q-TYPED-POINTER READ-MEMORY-DATA) ;BINDING TO SAVE
                ;write NEW VALUE CELL CONTENTS
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT   ;(gc-write-test below...)
                READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER
                A-B)
        ((M-B) dpb q-pointer VMA (a-constant (byte-value q-data-type dtp-locative)))
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        ((M-C) DPB M-ZERO (LISP-BYTE %%FEFHI-SVM-HIGH-BIT) A-C) ;FOR FRMBN1'S BENEFIT
                                                ;IF WE ARE COMING FROM THERE.
        ((WRITE-MEMORY-DATA) M-K)
        (JUMP-IF-BIT-SET-XCT-NEXT M-QBBFL QBSPCL1)      ;JUMP IF NOT FIRST IN BLOCK
       ((M-T) ADD M-T A-ZERO ALU-CARRY-IN-ONE)   ;ADVANCE TO NEXT S-V SLOT
        ((M-K WRITE-MEMORY-DATA) DPB (M-CONSTANT -1) (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG) A-K)
        ((M-QBBFL) DPB (M-CONSTANT -1) A-FLAGS)
        ((PDL-INDEX) ADD M-AP (A-CONSTANT (EVAL %LP-CALL-STATE)))       ;set attention in
        ((C-PDL-BUFFER-INDEX) IOR C-PDL-BUFFER-INDEX                    ;frame being called.
                           (A-CONSTANT (BYTE-VALUE %%LP-CLS-ATTENTION 1)))
QBSPCL1 ((VMA-START-WRITE) M+A+1 M-ZERO A-QLBNDP)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        ((WRITE-MEMORY-DATA) M-B)
        ((VMA-START-WRITE M-K) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE)                      ;Note possible invz pntr cleared from M-K
        (GC-WRITE-TEST)
        (popj-after-next
          (A-QLBNDP) VMA)
       (no-op)

;DATA TYPE CHECKS
;QDTATM (JUMP-EQUAL M-C (A-CONSTANT (EVAL DTP-SYMBOL)) QBDDT1)
;QDTN   (JUMP-EQUAL M-C (A-CONSTANT (EVAL DTP-FIX)) QBDDT1)
;       (JUMP-EQUAL M-C (A-CONSTANT (EVAL DTP-EXTENDED-NUMBER)) QBDDT1)
;QBDDT3 (JUMP-XCT-NEXT QBDDT1)          ;BAD DATA TYPE
;       ((M-QBBDT) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)

;QDTFXN (JUMP-EQUAL M-C (A-CONSTANT (EVAL DTP-FIX)) QBDDT1)
;       (JUMP QBDDT3)

;QDTSYM (JUMP-EQUAL M-C (A-CONSTANT (EVAL DTP-SYMBOL)) QBDDT1)
;       (JUMP QBDDT3)

;QDTLST ((M-C) Q-TYPED-POINTER PDL-INDEX-INDIRECT)
;       (JUMP-EQUAL M-C A-V-NIL QBDDT1)
;       (DISPATCH Q-DATA-TYPE PDL-INDEX-INDIRECT SKIP-IF-LIST)
;        (JUMP QBDDT3)
;       (JUMP QBDDT1)

;QDTFRM (JUMP-EQUAL M-C (A-CONSTANT (EVAL DTP-FEF-POINTER)) QBDDT1)
;       (JUMP QBDDT3)

;EVAL/QUOTE CHECKS
;QBEQE  (JUMP-EQUAL M-C A-ZERO QBEQC1)
;QBEQQ1 (JUMP-XCT-NEXT QBEQC1)
;      ((M-QBBQTS) DPB (M-CONSTANT -1) A-ERROR-SUBSTATUS)
;
;QBEQQ  (JUMP-NOT-EQUAL M-C A-ZERO QBEQC1)
;       (JUMP QBEQQ1)

;;FRAME BIND. BIND S-V S FROM FRAME FAST ENTERED USING S.V. MAP
FRMBN1  ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %FEFHI-SV-BITMAP)))
        (CHECK-PAGE-READ)
        ((M-D PDL-INDEX) M-AP)
        ((M-T) output-selector-mask-25                 ; -> S-V slots
               add m-a (a-constant (plus (eval %fefhi-special-value-cell-pntrs)
                                         (byte-value q-data-type dtp-locative))))
        (CALL-IF-BIT-CLEAR (LISP-BYTE %%FEFHI-SVM-ACTIVE)
                READ-MEMORY-DATA ILLOP)  ;FOO FAST OPT
                        ;SHOULD NOT BE ON UNLESS SVM IS. (IT ISNT WORTH IT TO HAVE
                        ;ALL THE HAIRY MICROCODE TO SPEED THIS CASE UP A TAD.)
        ((M-C) (LISP-BYTE %%FEFHI-SVM-BITS) READ-MEMORY-DATA)
FRMBN2  (POPJ-EQUAL M-C A-ZERO)   ;POPJ IF NO MORE BITS
        (CALL-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%FEFHI-SVM-HIGH-BIT)
                        M-C QBSPCL)     ;QBSPCL CLEARS %%FEFHI-SVM-HIGH-BIT IN M-C; uses PDL-INDEX
       ((M-D PDL-INDEX) ADD M-D A-ZERO ALU-CARRY-IN-ONE)
        (JUMP-XCT-NEXT FRMBN2)
       ((M-C) M+M M-C A-ZERO)

;POP A BLOCK OF BINDINGS
BBLKP   (JUMP-XCT-NEXT BBLKP1)
       ((M-ZR) SETCA A-ZERO)

;POP A BINDING (MUSTN'T BASH M-T, M-J, M-R, M-D, M-C, M-A)
QUNBND  ((M-ZR) A-ZERO)
BBLKP1  ((VMA-START-READ) A-QLBNDP)             ;Get pntr to bound cell
        (CHECK-PAGE-READ)
        ((A-QLBNDP) ADD A-QLBNDP (M-CONSTANT -1))
        ((A-QLBNDP) ADD A-QLBNDP (M-CONSTANT -1))
        (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-Q) READ-MEMORY-DATA)
        ((VMA-START-READ) M+A+1 M-ZERO A-QLBNDP)        ;Previous contents
        (CHECK-PAGE-READ)
        (CALL-DATA-TYPE-NOT-EQUAL M-Q (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)) ILLOP)
        (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
        ((M-B) READ-MEMORY-DATA)
        ((VMA-START-READ) Q-POINTER M-Q)                        ;Access bound cell
        (CALL-CONDITIONAL PG-FAULT BBLKP-PG-FAULT)
        (CHECK-PAGE-READ)                       ;This is only to preserve cdr code.
        (dispatch transport-no-evcp md)                ;KHS 860602.
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT
                READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER A-B)
        (CHECK-PAGE-WRITE-BIND)
        (gc-write-test) ;850503
BBLKP3  (JUMP-IF-BIT-SET (LISP-BYTE %%SPECPDL-BLOCK-START-FLAG) M-B BBLKP2)     ;Jump if last binding in block
        (JUMP-NOT-EQUAL M-ZR A-ZERO BBLKP1)     ;Loop if BBLKP
        (POPJ-IF-BIT-CLEAR-XCT-NEXT M-DEFERRED-SEQUENCE-BREAK-FLAG)     ;Exit if QUNBND
       ((M-B) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ;Dont leave a DTP-E-V-C-P in M-B
        (JUMP SB-REINSTATE)                     ; (If SB, this might make SG switch bomb).

BBLKP2  ((M-B) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))) ;Dont leave a DTP-E-V-P in M-B
        (POPJ-IF-BIT-CLEAR-XCT-NEXT M-DEFERRED-SEQUENCE-BREAK-FLAG)
       ((M-QBBFL) DPB M-ZERO A-FLAGS)           ;NO MORE B.B.
SB-REINSTATE            ;SB deferred.  Take it now?
        (declare (clobbers m-tem))
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-INHIBIT-SCHEDULING-FLAG)
        (POPJ-NOT-EQUAL M-TEM A-V-NIL)
        ((LOCATION-COUNTER) LOCATION-COUNTER)   ;write LC (assuring fetch of PC)
#+LAMBDA(POPJ-AFTER-NEXT
          (RG-MODE) ANDCA RG-MODE (A-CONSTANT 1_26.))  ;sense opposite on LAMBDA.
#+exp   (popj-after-next
          (mcr) ior mcr (a-constant 1_14.))
       ((M-DEFERRED-SEQUENCE-BREAK-FLAG) DPB M-ZERO A-FLAGS)

BBLKP-PG-FAULT
                ;SIZE-OF-M-MEMORY ****
        (JUMP-LESS-THAN VMA (A-CONSTANT (PLUS LOWEST-A-MEM-VIRTUAL-ADDRESS 100)) PGF-R)
                ;PLUS 100 TO AVOID HACKING M-MEM LOCATIONS. PROBABLY NONE OF THESE ARE EVER
                ;LAMBDA BOUND, BUT JUST TO BE SURE..
;; Page fault due to unbinding an A-memory variable.
;; Handle the unbinding with special code to make this much faster.
;; No need to worry about preserving cdr code of an a-mem loc; just zero it.
        ((OA-REG-LOW) DPB VMA OAL-A-DEST-10-BITS A-ZERO)
        ((A-GARBAGE) Q-TYPED-POINTER M-B)
;; "Return" past where the write normally happens.
        (JUMP-XCT-NEXT BBLKP3)
       ((M-GARBAGE) MICRO-STACK-DATA-POP)

XUB (MISC-INST-ENTRY UNBIND-0)                  ;UNBIND N BLOCKS
    (MISC-INST-ENTRY UNBIND-1)
    (MISC-INST-ENTRY UNBIND-2)
    (MISC-INST-ENTRY UNBIND-3)
    (MISC-INST-ENTRY UNBIND-4)
    (MISC-INST-ENTRY UNBIND-5)
    (MISC-INST-ENTRY UNBIND-6)
    (MISC-INST-ENTRY UNBIND-7)
    (MISC-INST-ENTRY UNBIND-10)
    (MISC-INST-ENTRY UNBIND-11)
    (MISC-INST-ENTRY UNBIND-12)
    (MISC-INST-ENTRY UNBIND-13)
    (MISC-INST-ENTRY UNBIND-14)
    (MISC-INST-ENTRY UNBIND-15)
    (MISC-INST-ENTRY UNBIND-16)
    (MISC-INST-ENTRY UNBIND-17)
        ((m-d) (byte-field 4 0) macro-ir)
XUB1    (CALL-IF-BIT-CLEAR M-QBBFL ILLOP)       ;TRYING TO OVERPOP FRAME
        (CALL QUNBND)
        (POPJ-EQUAL M-D A-ZERO)
        (JUMP-XCT-NEXT XUB1)
       ((M-D) SUB M-D (A-CONSTANT 1))

XPOPIP (MISC-INST-ENTRY POPPDL-0)
       (MISC-INST-ENTRY POPPDL-1)
       (MISC-INST-ENTRY POPPDL-2)
       (MISC-INST-ENTRY POPPDL-3)
       (MISC-INST-ENTRY POPPDL-4)
       (MISC-INST-ENTRY POPPDL-5)
       (MISC-INST-ENTRY POPPDL-6)
       (MISC-INST-ENTRY POPPDL-7)
       (MISC-INST-ENTRY POPPDL-10)
       (MISC-INST-ENTRY POPPDL-11)
       (MISC-INST-ENTRY POPPDL-12)
       (MISC-INST-ENTRY POPPDL-13)
       (MISC-INST-ENTRY POPPDL-14)
       (MISC-INST-ENTRY POPPDL-15)
       (MISC-INST-ENTRY POPPDL-16)
       (MISC-INST-ENTRY POPPDL-17)
;       (POPJ-AFTER-NEXT
;        (M-B) (BYTE-FIELD 4 0) M-B)    ;POP PDL 1-16.  NOTE THIS CAN NOT BE CALLED BY
;                                       ;COMPILED MICROCODE SINCE B WONT BE SET UP
;      ((PDL-POINTER) SUB PDL-POINTER A-B)
;THE FOLLOWING IS A TEMPORARY KLUDGE UNTIL THE COMPILER BUG IS FIXED. 12/19/78 MOON, PER RMS
        ((m-b) (byte-field 4 0) macro-ir)
XPOPIP-2
        ((PDL-POINTER M-B) SUB PDL-POINTER A-B)
;Flush all open call blocks above stack level in M-B.
XPOPIP-1
        ((M-TEM) SUB M-B A-IPMARK)
        (POPJ-IF-BIT-CLEAR PDL-BUFFER-ADDRESS-HIGH-BIT M-TEM) ;PP >= A-IPMARK mod length-of-pb
        (CALL POP-OPEN-CALL)            ;Compiler forgot to flush this open call block
        (JUMP XPOPIP-1) ;Try again

XMOVE-PDL-TOP (MISC-INST-ENTRY MOVE-PDL-TOP)
        (POPJ-AFTER-NEXT (M-T) Q-TYPED-POINTER PDL-TOP)
       (NO-OP)

XSHRINK-PDL-SAVE-TOP (MISC-INST-ENTRY SHRINK-PDL-SAVE-TOP)
        ((M-B) Q-POINTER PDL-POP)       ;AMT TO DECREMENT PP BY
        (JUMP-XCT-NEXT XPOPIP-2)
       ((M-T) Q-TYPED-POINTER PDL-POP)  ;THING TO RETURN

;;; The interpreter uses this extensively.  It returns the virtual address
;;; of the stack-pointer at the time the instruction is executed. (i.e. it
;;; returns the same value whether the destination is D-PDL or D-IGNORE).
xregular-pdl-index (misc-inst-entry %regular-pdl-index)
        (call-xct-next convert-pdl-buffer-address)
       ((m-k) pdl-pointer)
        (popj-after-next
          (m-t) q-typed-pointer m-k)
       (no-op)

;Now actually returns a locative to the last slot bound.
XSPECIAL-PDL-INDEX (MISC-INST-ENTRY SPECIAL-PDL-INDEX)
        (POPJ-AFTER-NEXT (M-T) A-QLBNDP)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))

;Now actually take a locative to the slot to unwind to.
XUNBIND-TO-INDEX-MOVE (MISC-INST-ENTRY UNBIND-TO-INDEX-MOVE)
        ((M-T) Q-TYPED-POINTER PDL-POP) ;VALUE TO RETURN LATER
XUNBIND-TO-INDEX (MISC-INST-ENTRY UNBIND-TO-INDEX)
        ((M-D) Q-POINTER PDL-POP)
        ((M-J) A-QLBNDP)   ;Remember starting value for debugging.
XUNBIND-TO-INDEX-0
        (POPJ-GREATER-OR-EQUAL M-D A-QLBNDP)
        (CALL-IF-BIT-CLEAR M-QBBFL ILLOP)
        (JUMP-XCT-NEXT XUNBIND-TO-INDEX-0)
       (CALL QUNBND)

XUNBIND-TO-INDEX-UNDER-N (MISC-INST-ENTRY UNBIND-TO-INDEX-UNDER-N)
        ((M-1) Q-POINTER PDL-POP)
        ((PDL-INDEX) SUB PDL-POINTER A-1)
;; M-D gets the special pdl index we want to unwind to.
;; It is passed to XUNBIND-TO-INDEX-0.
        ((M-D) Q-POINTER PDL-INDEX-INDIRECT)
;; Now discard that word from the stack by copying down everything above it.
;; Use a loop of pushes.
        ((PDL-POINTER) SUB PDL-INDEX (A-CONSTANT 1))
;; PDL-POINTER is the next word to push into, minus 1.
;; PDL-INDEX is the next word to fetch from, minus 1.
;; M-1 is the number of words to be copied.
XUNBIND-TO-INDEX-UNDER-N-1
        ((M-1) SUB M-1 (A-CONSTANT 1))
        ((PDL-INDEX) ADD PDL-INDEX (A-CONSTANT 1))
        (JUMP-GREATER-XCT-NEXT M-1 A-ZERO XUNBIND-TO-INDEX-UNDER-N-1)
       ((PDL-PUSH) PDL-INDEX-INDIRECT)
        (JUMP XUNBIND-TO-INDEX-0)

XPDL-WORD (MISC-INST-ENTRY PDL-WORD)
        ((M-1) Q-POINTER PDL-POP)
        (POPJ-AFTER-NEXT (PDL-INDEX) SUB PDL-POINTER M-1)
       ((M-T) PDL-INDEX-INDIRECT)

XPOP-M-FROM-UNDER-N (MISC-INST-ENTRY POP-M-FROM-UNDER-N)
        ((M-1) Q-POINTER PDL-POP)   ;Number of values to keep.
        ((M-2) Q-POINTER PDL-POP)   ;Number of words to pop.
        ((PDL-INDEX) SUB PDL-POINTER A-1)
;M-B gets final pdl level below values we are preserving.
        ((M-B) SUB PDL-INDEX A-2)
;Flush all open call blocks above there.
        (CALL XPOPIP-1)
        ((PDL-INDEX) SUB PDL-POINTER A-1)       ;XPOPIP-1 clobbered PDL-INDEX.
        ((PDL-POINTER) M-B)
;; PDL-POINTER is the next word to push into, minus 1.
;; PDL-INDEX is the next word to fetch from, minus 1.
;; M-1 is the number of words to be copied.
XPOP-M-FROM-UNDER-N-1
        ((M-1) SUB M-1 (A-CONSTANT 1))
        ((PDL-INDEX) ADD PDL-INDEX (A-CONSTANT 1))
        (JUMP-GREATER-XCT-NEXT M-1 A-ZERO XPOP-M-FROM-UNDER-N-1)
       ((PDL-PUSH) PDL-INDEX-INDIRECT)
        (POPJ)

;Get rid of one open call block, but don't change the pdl pointer.
;Does not run the unwind form if the call block is an unwind protect!
;  Note that an open call block never has any
;associated binding-pdl slots, since closures and so forth are processed
;when the call is activated.
POP-OPEN-CALL
        (CALL-EQUAL M-AP A-IPMARK TRAP) ;Trying to pop call block that isn't open
    (ERROR-TABLE ILLEGAL-INSTRUCTION)
        ((M-K) A-IPMARK)
        ((PDL-INDEX) ADD M-K (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-TEM) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) PDL-INDEX-INDIRECT)
        (POPJ-AFTER-NEXT
         (M-TEM) SUB M-K A-TEM)
        ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-TEM)

;Get rid of one open call block, but don't change the pdl pointer.
;Takes one arg, which is 0 if the frame being flushed is not an unwind-protect,
;or else the pc of the restart for the unwind protect.
;  The compiler always generates this to D-IGNORE.
XPOP-OPEN-CALL (MISC-INST-ENTRY POP-OPEN-CALL)
        (CALL-EQUAL M-AP A-IPMARK TRAP) ;Trying to pop call block that isn't open
    (ERROR-TABLE ILLEGAL-INSTRUCTION)
        ((M-T) Q-POINTER PDL-POP)
        ((M-K) A-IPMARK)
        ((PDL-INDEX) ADD M-K (A-CONSTANT (EVAL %LP-CALL-STATE)))
        ((M-TEM) (LISP-BYTE %%LP-CLS-DELTA-TO-OPEN-BLOCK) PDL-INDEX-INDIRECT)
        ((M-TEM) SUB M-K A-TEM)
        (POPJ-EQUAL-XCT-NEXT M-T A-ZERO)
       ((A-IPMARK) PDL-BUFFER-ADDRESS-MASK M-TEM)
;It is an unwind-protect, so jump to the unwind form (pc is in M-T)
;after pushing four words saying how to come back to this pc
;(see XUWPCON-POP-OPEN-CALL).
;       ((PDL-INDEX) M-AP)
;Get the fef address, shifted left 2.
;       ((M-K) DPB PDL-INDEX-INDIRECT (BYTE-FIELD Q-POINTER-WIDTH 2) A-ZERO)
        ((m-k) dpb m-fef (byte-field q-pointer-width #+lambda 2 #+exp 1) a-zero)
        ;m-tem is byte(lambda) or halfword(exp) offset of current LC in FEF
        ((M-TEM) SUB LOCATION-COUNTER A-K)              ;QMDDR
        ;M-T is halfword address to go to in FEF, convert to byte for lambda
#+lambda((M-T) ADD M-T A-T)
        ((LOCATION-COUNTER) ADD M-T A-K)
        ;push offset in FEF in "native" form
        ((PDL-PUSH) DPB M-TEM Q-POINTER
         (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1)))
        ((PDL-PUSH) A-V-NIL)
        ((PDL-PUSH) A-V-NIL)
        (POPJ)

;;; Some support for instances

XFUNCTION-INSIDE-SELF (MISC-INST-ENTRY %FUNCTION-INSIDE-SELF)
        ((M-T) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-SELF)
        ;Default is to return self
        (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE)) XFIS-I)
        (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ENTITY)) XFIS-C)
        (POPJ-DATA-TYPE-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CLOSURE)))
XFIS-C  (JUMP-XCT-NEXT QCAR4)                   ;Get function of closure
      ;((VMA-START-READ) M-T)
XFIS-I  ((VMA-START-READ) M-T)                  ;Get instance header
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        (JUMP-XCT-NEXT QCAR4)
       ((VMA-START-READ) ADD MD (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-FUNCTION)))

XINSTANCE-REF (MISC-INST-ENTRY %INSTANCE-REF)
        (JUMP-XCT-NEXT QCAR3)
       (CALL XINSTANCE-LOC)

XSET-INSTANCE-REF (MISC-INST-ENTRY SET-%INSTANCE-REF)
        ((M-A) Q-TYPED-POINTER PDL-POP)
        (CALL XINSTANCE-LOC)
    (ERROR-TABLE CALLS-SUB SET-%INSTANCE-REF)
        ((M-S) M-T)
        (JUMP-XCT-NEXT QRAR4)
       ((M-T) M-A)

XINSTANCE-SET (MISC-INST-ENTRY %INSTANCE-SET)
        (CALL XINSTANCE-LOC)
                (ERROR-TABLE CALLS-SUB %INSTANCE-SET)
        ((M-S) M-T)
        ((M-A) PDL-TOP)
        (JUMP-XCT-NEXT QRAR4)
       ((M-T) PDL-POP)

XINSTANCE-LOC (MISC-INST-ENTRY %INSTANCE-LOC)
        ((M-1) Q-POINTER PDL-POP)       ;Index
                (ERROR-TABLE RESTART XINSTANCE-LOC)
        (CALL-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE))
                        TRAP)
                (ERROR-TABLE ARGTYP INSTANCE PP 0 XINSTANCE-LOC %INSTANCE-LOC)
        ((VMA-START-READ) PDL-POP)      ;Get instance header
XINSTANCE-LOC-1
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        (CALL-DATA-TYPE-NOT-EQUAL READ-MEMORY-DATA
                   (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE-HEADER)) TRAP)
                (ERROR-TABLE DATA-TYPE-SCREWUP DTP-INSTANCE-HEADER)
        ((M-T) VMA)                                     ;Possibly-forwarded instance
        ((VMA-START-READ) ADD MD (A-CONSTANT (EVAL %INSTANCE-DESCRIPTOR-SIZE)))
        (CHECK-PAGE-READ)
        (CALL-EQUAL M-1 A-ZERO TRAP)                    ;Don't access the header!
                (ERROR-TABLE ARGTYP PLUSP M-1 1 NIL %INSTANCE-LOC)
        ((M-2) Q-POINTER READ-MEMORY-DATA)              ;Size of instance
XINSTANCE-LOC-RESTART
    (ERROR-TABLE RESTART XINSTANCE-LOC-RESTART)
        (CALL-GREATER-OR-EQUAL M-1 A-2 TRAP)
    (ERROR-TABLE SUBSCRIPT-OOB M-1 M-2 XINSTANCE-LOC-RESTART M-T)
        (POPJ-AFTER-NEXT (M-T) ADD M-T A-1)
       ((M-T) Q-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))

;%ARGS-INFO <FUNCTION>   FUNCTION CAN BE ANYTHING MEANINGFUL IN
;FUNCTION CONTEXT. RETURNS FIXNUM.  FIELDS AS IN NUMERIC-ARG-DESC-INFO IN QCOM.

XARGI (MISC-INST-ENTRY %ARGS-INFO)
        ((M-S) Q-TYPED-POINTER PDL-POP)
;ENTER HERE FROM APPLY, ALSO REENTER TO TRY AGAIN (CLOSURE, ETC).
XARGI0  (DISPATCH-XCT-NEXT Q-DATA-TYPE M-S XARGI-DISPATCH)  ;INHIBIT-XCT-NEXT UNLESS
       ((M-T) (A-CONSTANT (PLUS (PLUS                       ; INTERPRETER TRAP
                (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                (BYTE-MASK %%ARG-DESC-INTERPRETED))
                (BYTE-MASK %%ARG-DESC-MAX-ARGS))))

XAGISG  (POPJ-AFTER-NEXT                                ;STACK GROUP ACCEPTS ANY NUMBER
         (M-T) DPB (M-CONSTANT -1) (LISP-BYTE %%ARG-DESC-MAX-ARGS)      ;OF EVALED ARGS
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       (NO-OP)

XAGUE1  ((VMA-START-READ) ADD M-S A-V-MICRO-CODE-ENTRY-AREA)
        (CHECK-PAGE-READ)
        (JUMP-DATA-TYPE-NOT-EQUAL READ-MEMORY-DATA (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                XAGUE3)
        (JUMP-XCT-NEXT XAGUE2)                              ;UCODE-ENTRY
       ((VMA-START-READ) ADD M-S A-V-MICRO-CODE-ENTRY-ARGS-INFO-AREA)

XAGICL  (CALL-XCT-NEXT QCAR)                                ;CLOSURE
       ((M-T) Q-POINTER M-S              ;REPLACE BY CAR OF IT AND TRY AGAIN.
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        (JUMP-XCT-NEXT XARGI0)
       ((M-S) M-T)

XAGAR1  ((VMA-START-READ) M-S)                  ;ARRAY-POINTER
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        (POPJ-AFTER-NEXT
         (M-T)
          (LISP-BYTE %%ARRAY-NUMBER-DIMENSIONS)
                 READ-MEMORY-DATA
                 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((M-T) DPB M-T (LISP-BYTE %%ARG-DESC-MIN-ARGS) A-T)  ;COPY INTO BOTH MAX AND MIN

XAGM1   ((VMA-START-READ) ADD
                 M-S (A-CONSTANT (EVAL %FEFHI-FAST-ARG-OPT)));MACRO-COMPILED
XAGUE2  (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT READ-MEMORY-DATA)
       ((M-T) Q-TYPED-POINTER READ-MEMORY-DATA)

XARGI3  ((VMA-START-READ) ADD M-S (A-CONSTANT 2))       ;SYM, REPLACE W FCTN CELL
        (CHECK-PAGE-READ)
XAGUE3  (DISPATCH TRANSPORT READ-MEMORY-DATA)
        (JUMP-XCT-NEXT XARGI0)
       ((M-S) Q-TYPED-POINTER READ-MEMORY-DATA)

;CONVERT PDL BUFFER ADDRESS IN M-K TO VIRTUAL ADDRESS IN M-K WITH LOCATIVE
; DATA-TYPE.  ANY REFERENCE VIRTUAL ADDRESS WHICH MAY BE IN PDL-BUFFER WILL TRAP,
; AND PAGE FAULT HANDLER WILL FIGURE OUT WHAT TO DO.

CONVERT-PDL-BUFFER-ADDRESS
        (declare (args a-k) (values a-k))
        ((M-K) SUB M-K A-PDL-BUFFER-HEAD)
        (POPJ-AFTER-NEXT
         (M-K) DPB M-K PDL-BUFFER-ADDRESS-MASK  ;ASSURE POSITIVE OFFSET IN CASE OF WRAPAROUND
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
       ((M-K) ADD M-K A-PDL-BUFFER-VIRTUAL-ADDRESS)

; CONVERT VIRTUAL ADDRESS IN M-K INTO PDL-INDEX (ASSUMING IT REFERENCES THE CURRENT
;STACK GROUP).  NOTE THIS DOES NOT ASSURE THAT SECTION OF PDL SWAPPED IN OR ANYTHING.
;IF AND WHEN IT IS SWAPPED IN, HOWEVER, IT WILL OCUPPY THE INDICATED PDL-BUFFER ADDRESS.

GET-PDL-BUFFER-INDEX
        (declare (args a-k) (values a-k))
        ((M-K) SUB M-K A-PDL-BUFFER-VIRTUAL-ADDRESS)
        (POPJ-AFTER-NEXT
         (M-K) ADD M-K A-PDL-BUFFER-HEAD)
       ((M-K) PDL-BUFFER-ADDRESS-MASK M-K)

LOAD-PDL-BUFFER-INDEX
        (declare (args a-k))
        (POPJ-AFTER-NEXT
         (PDL-INDEX) SUB M-K A-PDL-BUFFER-VIRTUAL-ADDRESS)
       ((PDL-INDEX) ADD PDL-INDEX A-PDL-BUFFER-HEAD)

(LOCALITY D-MEM)
(START-DISPATCH 4 0)
D-PUSH-NILS
        (P-BIT R-BIT)
        (P-BIT PN-1)
        (P-BIT PN-2)
        (P-BIT PN-3)
        (P-BIT PN-4)
        (P-BIT PN-5)
        (P-BIT PN-6)
        (P-BIT PN-7)
        (P-BIT PN-8)
        (P-BIT PN-9)
        (P-BIT PN-10)
        (P-BIT PN-11)
        (P-BIT PN-12)
        (P-BIT PN-13)
        (P-BIT PN-14)
        (P-BIT PN-15)
(END-DISPATCH)
(LOCALITY I-MEM)

PN-15   ((PDL-PUSH) A-V-NIL)
PN-14   ((PDL-PUSH) A-V-NIL)
PN-13   ((PDL-PUSH) A-V-NIL)
PN-12   ((PDL-PUSH) A-V-NIL)
PN-11   ((PDL-PUSH) A-V-NIL)
PN-10   ((PDL-PUSH) A-V-NIL)
PN-9    ((PDL-PUSH) A-V-NIL)
PN-8    ((PDL-PUSH) A-V-NIL)
PN-7    ((PDL-PUSH) A-V-NIL)
PN-6    ((PDL-PUSH) A-V-NIL)
PN-5    ((PDL-PUSH) A-V-NIL)
PN-4    ((PDL-PUSH) A-V-NIL)
PN-3    ((PDL-PUSH) A-V-NIL)
PN-2    (POPJ-AFTER-NEXT (PDL-PUSH) A-V-NIL)
       ((PDL-PUSH) A-V-NIL)

PN-1    (POPJ-AFTER-NEXT (PDL-PUSH) A-V-NIL)
       (NO-OP)
))

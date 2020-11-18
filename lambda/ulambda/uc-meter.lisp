;-*- mode: lisp; base: 8; readtable: ZL -*-

(DEFCONST UC-METER '(
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;
;;; METERING STUFF

        (MISC-INST-ENTRY %SET-METER-ENABLES)
XSET-METER-ENABLES
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-METER-ENABLES) M-T)
        ((M-1) Q-POINTER M-T)
        (JUMP-EQUAL M-1 A-ZERO SET-QLENTR-METER-OFF)    ;else fall through...

set-qlentr-meter-on
#+lambda(popj-after-next
                                                ;1_19. is dispatch-start-mem-read
          (d-qmrcl-qlentr) (a-constant (plus 1_19. (i-mem-loc qlentr-meter))))
#+lambda((a-default-call-state) (a-constant (plus (byte-value q-data-type dtp-fix)
                                                  (byte-value %%lp-cls-attention 1))))
#+exp   ((m-tem) (a-constant (i-mem-loc qlentr-meter)))
#+exp   (dispatch write-dispatch-memory d-qmrcl-qlentr (byte-field 0 0) (i-arg (a-mem-loc a-tem)))
#+exp   ((a-default-call-state) (a-constant (plus (byte-value q-data-type dtp-fix)
                                                  (byte-value %%lp-cls-attention 1))))
#+exp   (popj)

set-qlentr-meter-off
#+lambda(popj-after-next
                                                ;1_19. is dispatch-start-mem-read
          (d-qmrcl-qlentr) (a-constant (plus 1_19. (i-mem-loc qlentr))))
#+lambda((a-default-call-state) (a-constant (byte-value q-data-type dtp-fix)))

#+exp   ((m-tem) (a-constant (i-mem-loc qlentr)))
#+exp   (dispatch write-dispatch-memory d-qmrcl-qlentr (byte-field 0 0) (i-arg (a-mem-loc a-tem)))
#+exp   ((a-default-call-state) (a-constant (byte-value q-data-type dtp-fix)))
#+exp   (popj)

;;; (%RECORD-EVENT DATA-1 ... DATA-N N-FUNCTIONS-UP EVENT-NUM N)
;;; records an event number and labels function N-FUNCTIONS-UP stack frames
;;; up the stack. Additional info DATA-n, and N which is needed so that
;;; it knows what the number of data are.
X-RECORD-EVENT  (MISC-INST-ENTRY %RECORD-EVENT)
        ((A-METER-LENGTH) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        (CALL-XCT-NEXT METER-SETUP)
       ((A-METER-EVENT) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;get levels to go back
        (POPJ-EQUAL M-ZERO A-TEM1)      ;punt if not appropriate
        (CALL-XCT-NEXT CONVERT-PDL-BUFFER-ADDRESS)
       ((M-K) M-AP)
        (JUMP-XCT-NEXT XRECEV2)
XRECEV1 ((VMA-START-READ) ADD M-K (A-CONSTANT (EVAL %LP-CALL-STATE)))
        (CHECK-PAGE-READ-NO-INTERRUPT)
        ((M-TEM) (LISP-BYTE %%LP-CLS-DELTA-TO-ACTIVE-BLOCK) READ-MEMORY-DATA)
        ((M-K) SUB M-K A-TEM)
XRECEV2 (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-ZERO XRECEV1)
       ((M-1) SUB M-1 (A-CONSTANT 1))
        (CALL METER-ASSURE-ROOM)
        ((VMA-START-READ) M-K)
        (CHECK-PAGE-READ-NO-INTERRUPT)
        (CALL-XCT-NEXT METER-WRITE-HEADER)
       ((M-1) READ-MEMORY-DATA)
        (JUMP METER-CLEANUP)


;;; Takes number of data to push in A-METER-LENGTH. Assumes that the disk count is non zero
METER-ASSURE-ROOM
        ((M-TEM) DPB M-ZERO (BYTE-FIELD 30 10) A-METER-BUFFER-POINTER)
        ((M-TEM) ADD M-TEM A-METER-LENGTH)
        (POPJ-LESS-THAN-XCT-NEXT M-TEM (A-CONSTANT (DIFFERENCE (EVAL PAGE-SIZE)
                                                               METER-OVERHEAD-LENGTH)))
       ((A-METER-LOCK) (A-CONSTANT 1))  ;Lock out everyone
        ((VMA) A-METER-BUFFER-POINTER)  ;Write a word of zero, it wont fit
        ((WRITE-MEMORY-DATA-START-WRITE) SETZ)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)

;;; Flush the meter buffer, and maintain disk count and disk address
METER-FLUSH-BUFFER
        ((A-METER-LOCK) M+A+1 M-ZERO A-METER-LOCK)      ;Lock out everyone
        ((C-PDL-BUFFER-POINTER-PUSH) M-B)
        ((C-PDL-BUFFER-POINTER-PUSH) M-C)
        ((C-PDL-BUFFER-POINTER-PUSH) M-T)
        ((A-METER-BUFFER-POINTER) DPB M-ZERO (BYTE-FIELD 8 0) A-METER-BUFFER-POINTER)
                                        ;Reset buffer pointer and address map
        ((MD) A-METER-BUFFER-POINTER)   ;Address map
        (no-op)         ;give map time.
        ((M-1) L2-MAP-STATUS-CODE)      ;Paranoia checks to see if map is set up
        (CALL-LESS-THAN M-1 (A-CONSTANT 2) ILLOP)
        ((M-B) L2-MAP-PHYSICAL-PAGE-NUMBER      ;Get physical page number
                (A-CONSTANT (BYTE-MASK (BYTE-FIELD 1 31.)))) ;signal NUBUS physical page
        ;convert to NUBUS word address and signal this via sign bit.
        ((M-1) DPB M-ZERO Q-ALL-BUT-POINTER A-METER-DISK-ADDRESS)
        ((A-METER-DISK-ADDRESS) M+A+1 M-ZERO A-METER-DISK-ADDRESS)      ;Inc disk address
        ((A-METER-DISK-COUNT) ADD (M-CONSTANT -1) A-METER-DISK-COUNT)   ;Dec meter disk count
        ((A-METER-START-TIME) M-2)              ;Save microsecond clock
        (CALL-XCT-NEXT START-DISK-1-PAGE)       ;Do the disk operation
       ((M-T) (A-CONSTANT DISK-WRITE-COMMAND))
        (CALL-XCT-NEXT AWAIT-DISK)
       ((M-T) C-PDL-BUFFER-POINTER-POP)
        ((M-2) A-METER-START-TIME)
        ((M-C) C-PDL-BUFFER-POINTER-POP)
        (POPJ-AFTER-NEXT
            (M-B) C-PDL-BUFFER-POINTER-POP)
       ((A-METER-LOCK) ADD (M-CONSTANT -1) A-METER-LOCK)        ;Free lock up

;;; Assumes A-METER-EVENT is set to the event we want to record
;;; and A-METER-LENGTH is set to the number of extra data words we want to push
;;; Bashes M-1 and M-2
METER-MICRO-WRITE-HEADER
        (CALL METER-SETUP)
METER-MICRO-WRITE-HEADER-1
        (POPJ-EQUAL M-ZERO A-TEM1)                              ;punt if not appropriate
        ((C-PDL-BUFFER-POINTER-PUSH) PDL-BUFFER-INDEX)          ;save PI
        (CALL METER-ASSURE-ROOM)
        ((PDL-BUFFER-INDEX) M-AP)
        (CALL-XCT-NEXT METER-WRITE-HEADER)
       ((M-1) C-PDL-BUFFER-INDEX)               ;record this in "function" slot.
        (JUMP-XCT-NEXT METER-CLEANUP)
       ((PDL-BUFFER-INDEX) C-PDL-BUFFER-POINTER-POP)

METER-MICRO-WRITE-HEADER-NO-SG-TEST
        (JUMP-XCT-NEXT METER-MICRO-WRITE-HEADER-1)
       (CALL METER-SETUP-NO-SG-TEST)

;;; Take A-METER-LENGTH objects from the pdl, and put into meter buffer
;;; M-2 has microsecond clock reading when we started, so that metering
;;; overhead can be charged to A-DISK-WAIT-TIME.
METER-PUSH-LP
        ((VMA) A-METER-BUFFER-POINTER)
        ((WRITE-MEMORY-DATA-START-WRITE) C-PDL-BUFFER-POINTER-POP)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((A-METER-BUFFER-POINTER) M+A+1 M-ZERO A-METER-BUFFER-POINTER)
METER-CLEANUP
        (JUMP-LESS-THAN-XCT-NEXT M-ZERO A-METER-LENGTH METER-PUSH-LP)
       ((A-METER-LENGTH) ADD (M-CONSTANT -1) A-METER-LENGTH)
        ((M-1) (BYTE-FIELD 8 0) VMA A-MINUS-ONE)
                                        ;Screw case where we are pointing to last word
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-MINUS-ONE METER-CLEANUP-1)       ;Still buffer left
       ((A-METER-LOCK) ADD (M-CONSTANT -1) A-METER-LOCK)
        (CALL-XCT-NEXT METER-FLUSH-BUFFER)      ;Flush current buffer
       ((A-METER-BUFFER-POINTER) ADD (M-CONSTANT -1) A-METER-BUFFER-POINTER)
                                        ;Decrement so that it points to the right block again
METER-CLEANUP-1
        (CALL-XCT-NEXT READ-MICROSECOND-CLOCK)
       ((M-TEM1) M-2)
        (POPJ-AFTER-NEXT (M-TEM) SUB M-2 A-TEM1)        ;Time spent metering
       ((A-DISK-WAIT-TIME) ADD M-TEM A-DISK-WAIT-TIME)

;;; Returns with A-TEM1 = 0 if not appropriate to make this meter entry
;;; If not appropriate, pop off A-METER-LENGTH of pdl
;;; If appropriate, microsecond clock is in M-2
METER-SETUP
        (JUMP-IF-BIT-SET M-METER-STACK-GROUP-ENABLE METER-SETUP-NO-SG-TEST)     ;This SG
        ((M-TEM) A-METER-GLOBAL-ENABLE)                                         ;Any SG
        (JUMP-NOT-EQUAL M-TEM A-V-TRUE METER-SETUP-1)
METER-SETUP-NO-SG-TEST
        (JUMP-NOT-EQUAL M-ZERO A-METER-LOCK METER-SETUP-1)
        ((M-TEM1) DPB M-ZERO Q-ALL-BUT-POINTER A-METER-DISK-COUNT)
        (JUMP-NOT-EQUAL M-ZERO A-TEM1 READ-MICROSECOND-CLOCK)
METER-SETUP-1
        (POPJ-AFTER-NEXT
                (PDL-BUFFER-POINTER) SUB PDL-BUFFER-POINTER A-METER-LENGTH)     ;Pop args
       ((M-TEM1) SETZ)

;;; Writes the header of the meter info, function is in M-1
;;; M-2 has the microsecond clock as of the time of entry
METER-WRITE-HEADER
        ((C-PDL-BUFFER-POINTER-PUSH) M-1)
        ;; Write length,,event
        ((M-1 VMA) A-METER-BUFFER-POINTER)
        ((M-TEM) A-METER-LENGTH)
        ((M-TEM) ADD M-TEM (A-CONSTANT METER-OVERHEAD-LENGTH))
        ((WRITE-MEMORY-DATA-START-WRITE) DPB M-TEM METER-LENGTH A-METER-EVENT)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ;; Write Usec timer
        ((M-1 VMA) ADD M-1 (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) M-2)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ;; Write the page fault time
        ((M-1 VMA) ADD M-1 (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) A-DISK-WAIT-TIME)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ;; Write page fault count
        ((M-1 VMA) ADD M-1 (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) A-DISK-PAGE-READ-COUNT)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ;; Write current stack group
        ((M-1 VMA) ADD M-1 (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) A-QCSTKG)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ;; Write current function
        ((M-1 VMA) ADD M-1 (A-CONSTANT 1))
        ((WRITE-MEMORY-DATA-START-WRITE) C-PDL-BUFFER-POINTER-POP)      ;Current function
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ;; Write current stack depth (M-AP)
        ((M-1 VMA) ADD M-1 (A-CONSTANT 1))
        ((M-TEM) SUB M-AP A-PDL-BUFFER-HEAD)
        ((M-TEM) DPB M-TEM PDL-BUFFER-ADDRESS-MASK A-ZERO)
        ((M-TEM) ADD M-TEM A-PDL-BUFFER-VIRTUAL-ADDRESS)
        ((WRITE-MEMORY-DATA-START-WRITE) SUB M-TEM A-QLPDLO)    ;Current stack depth
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        (POPJ-AFTER-NEXT                        ;Update buffer pointer
                (A-METER-BUFFER-POINTER) ADD M-1 (A-CONSTANT 1))


METER-PAGE-OUT
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-PAGE-OUT-EVENT)))
        (JUMP-XCT-NEXT METER-PAGE)
       ((M-TEM) SELECTIVE-DEPOSIT M-A PHT1-VIRTUAL-PAGE-NUMBER A-ZERO)

METER-PAGE-IN
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-PAGE-IN-EVENT)))
        ((M-TEM) A-DISK-SWAPIN-VIRTUAL-ADDRESS)

METER-PAGE
        (CALL-XCT-NEXT DISK-PGF-SAVE)
       ((A-METER-LENGTH) (A-CONSTANT 2))        ;Two words of info
        ((M-TEM1) MICRO-STACK-DATA-POP)         ;Kludgey way you have to look back
        ((M-TEM2) MICRO-STACK-DATA-POP)         ; up the micro-stack
        ((M-TEM3) MICRO-STACK-DATA-POP)
#+LAMBDA((M-1) (BYTE-FIELD 16. 0)       ;Just the return address, not any funny flags
                MICRO-STACK-PNTR-AND-DATA       ;Call to PGF-R, PGF-W
                A-ZERO)
#+EXP   ((M-1) (BYTE-FIELD 14. 0)       ;Just the return address, not any funny flags
                MICRO-STACK-DATA        ;Call to PGF-R, PGF-W
                A-ZERO)
        ((MICRO-STACK-DATA-PUSH) A-TEM3)
        ((MICRO-STACK-DATA-PUSH) A-TEM2)
        ((MICRO-STACK-DATA-PUSH) A-TEM1)
        ((M-2) M-FLAGS-FOR-PAGE-TRACE)          ;Get flags
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-2 (BYTE-FIELD 3 28.) A-1)
        (CALL-XCT-NEXT METER-MICRO-WRITE-HEADER)        ;Write meter info
       ((C-PDL-BUFFER-POINTER-PUSH) M-TEM)      ;VMA of reference
        (JUMP DISK-PGF-RESTORE)


METER-SG-ENTER
        ((A-METER-EVENT) (A-CONSTANT (EVAL %METER-STACK-GROUP-SWITCH-EVENT)))
        ((M-1) A-LAST-STACK-GROUP)
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-1)
        (JUMP-IF-BIT-SET-XCT-NEXT Q-CDR-CODE-LOW-BIT M-1 METER-MICRO-WRITE-HEADER-NO-SG-TEST)
       ((A-METER-LENGTH) (A-CONSTANT 1))
        (JUMP METER-MICRO-WRITE-HEADER)


;Set up to do page-tracing.  We get a wired-down array and fill in 4-word
;entries for page-in and page-out.  An entry looks like:
;       Microsecond clock value
;       Virtual address
;       Miscellany:
;        bit 31: swap-out flag,
;        bit 30: stack-group-switch flag
;        bit 29: transport flag
;        bit 28: scavenge flag
;        bits 15-0: micro-pc
;       Current function (just randomly picks up @M-AP, hopefully reasonable)
;If A-PAGE-TRACE-PTR is non-zero, it's the next location to write into,
;and A-PAGE-TRACE-START is the lowest value, A-PAGE-TRACE-END is the wrap-around point
;The array better be wired, have 32-bit elements, and be a multiple of 4 long
;or the machine will blow totally away.
X-PAGE-TRACE (MISC-INST-ENTRY %PAGE-TRACE)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (POPJ-EQUAL-XCT-NEXT M-T A-V-NIL)
       ((A-PAGE-TRACE-PTR) SETZ)                ;Assume trace to be shut off
#+exp   ((vma) m-t)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA
                   (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE M-T ARRAY-HEADER-SETUP-DISPATCH)
       ((m-a) invalidate-array-cache M-T)  ;M-E origin, M-S length, untyped
        (call store-array-registers-in-accumulators)
        ((A-PAGE-TRACE-PTR) M-E)
        (POPJ-AFTER-NEXT (A-PAGE-TRACE-START) M-E)
       ((A-PAGE-TRACE-END) ADD M-E A-S)

;Make a page-trace entry for swap in.
;Only call this if A-PAGE-TRACE-PTR is non-zero
;Can take recursive page faults.
;Note that map faults, such as the interrupt routine can take, don't cause page tracing.
PAGE-TRACE-OUT  ;Here when swapping page out
;;M-B is phys page
        ((A-PAGE-TRACE-UPC) (A-CONSTANT (BYTE-MASK SIGN-BIT)))
        (JUMP-XCT-NEXT PAGE-TRACE-0)
       ((A-PAGE-TRACE-VMA) SELECTIVE-DEPOSIT M-A PHT1-VIRTUAL-PAGE-NUMBER A-ZERO)

PAGE-TRACE-IN   ;Here when swapping page in
;;M-B points to word that has phys page
        ((A-PAGE-TRACE-VMA) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        ((A-PAGE-TRACE-UPC) (A-CONSTANT 0))
PAGE-TRACE-0                                    ;clobbers M-1, M-2.
        ((M-TEM1) MICRO-STACK-DATA-POP)         ;Kludgey way you have to look back
        ((M-TEM2) MICRO-STACK-DATA-POP)         ; up the micro-stack
        ((M-TEM3) MICRO-STACK-DATA-POP)
#+LAMBDA((A-PAGE-TRACE-UPC) (BYTE-FIELD 16. 0)  ;Just the return address, not any funny flags
                MICRO-STACK-PNTR-AND-DATA       ;Call to PGF-R, PGF-W
                A-PAGE-TRACE-UPC)               ;Appropriate flags
#+EXP   ((A-PAGE-TRACE-UPC) (BYTE-FIELD 14. 0)  ;Just the return address, not any funny flags
                MICRO-STACK-DATA                ;Call to PGF-R, PGF-W
                A-PAGE-TRACE-UPC)
        ((MICRO-STACK-DATA-PUSH) m-TEM3)
        ((MICRO-STACK-DATA-PUSH) m-TEM2)
        ((MICRO-STACK-DATA-PUSH) m-TEM1)
        (CALL DISK-PGF-SAVE)                    ;Allow recursive faulting.
        ((A-DISK-SAVE-PI) PDL-BUFFER-INDEX)
        ((A-DISK-SAVE-FLAGS) M-FLAGS)
        ((M-INTERRUPT-FLAG) DPB (M-CONSTANT -1) A-FLAGS) ;No page swapping (error check)
        (CALL READ-MICROSECOND-CLOCK-INTO-MD)
        ((VMA-START-WRITE) A-PAGE-TRACE-PTR)    ;1st trace word: clock
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((WRITE-MEMORY-DATA) A-PAGE-TRACE-VMA)  ;2nd trace word: address referenced
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((M-TEM) M-FLAGS-FOR-PAGE-TRACE)        ;3rd trace word: flags, micro-pc
        ((WRITE-MEMORY-DATA) DPB M-TEM (BYTE-FIELD 3 28.) A-PAGE-TRACE-UPC)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
;       ((PDL-BUFFER-INDEX) M-AP)               ;4th trace word: macro-function
;       ((WRITE-MEMORY-DATA) C-PDL-BUFFER-INDEX)
;       ((write-memory-data) m-fef)
        ((md) m-b)
        ((m-tem) a-page-trace-upc)
        (jump-if-bit-set (byte 1 31.) m-tem page-trace-2)
        ((vma-start-read) m-b)
        (illop-if-page-fault)                   ;should be CCW list
        ((vma) a-page-trace-ptr)
        ((vma) add vma (a-constant 2))
page-trace-2
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((VMA) ADD VMA (A-CONSTANT 1))          ;Next trace entry address
        (JUMP-LESS-THAN VMA A-PAGE-TRACE-END PAGE-TRACE-1)
        ((VMA) A-PAGE-TRACE-START)              ;Wrap around
PAGE-TRACE-1
        (CALL-XCT-NEXT DISK-PGF-RESTORE)        ;Restore and return
       ((A-PAGE-TRACE-PTR) VMA)
        (POPJ-AFTER-NEXT
          (M-FLAGS) A-DISK-SAVE-FLAGS)
       ((PDL-BUFFER-INDEX) A-DISK-SAVE-PI)
))

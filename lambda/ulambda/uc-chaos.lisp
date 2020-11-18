;-*- mode: lisp; base: 8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;
(DEFCONST UC-CHAOS '(
;;; Come here for Unibus interrupt from Chaos network

#-cadr (begin-comment)

(ASSIGN CHAOS-NUMBER-TRANSMIT-RETRIES 3)        ;Send once and retry twice if aborted

CHAOS-INTR
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC UB-INTR-RET)))
        ((VMA-START-READ M-B) A-CHAOS-CSR-ADDRESS) ;M-B has base address of hardware
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%CHAOS-CSR-RECEIVE-DONE) READ-MEMORY-DATA
                CHAOS-XMT-INTR)                 ;See if received a packet
        ((A-INTR-TEM2) Q-POINTER READ-MEMORY-DATA       ;Save CSR for later
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-XCT-NEXT CHAOS-LIST-GET)          ;M-A gets next packet from free list
       ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-FREE-LIST))))
        (JUMP-EQUAL M-A A-V-NIL CHAOS-XMT-INTR) ;Can't receive now, hold up
        ;; Read out the packet into this buffer, along with CSR1, CSR2, Bit-count
        ;; M-A points at the buffer and M-B points at the hardware
        ;; Buffer is assumed to be big enough for max possible word count (255)
        ((WRITE-MEMORY-DATA) A-INTR-TEM2)       ;Save CSR1
        ((VMA-START-WRITE) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-CSR-1))))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((VMA-START-READ) ADD M-B (A-CONSTANT (EVAL %CHAOS-BIT-COUNT-OFFSET)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((WRITE-MEMORY-DATA M-TEM) M+A+1 READ-MEMORY-DATA       ;Type bits are 0, bit count is
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ;off by 1
        ((A-INTR-TEM1) (BYTE-FIELD 8 4) M-TEM)  ;Get word count, then save bit count
        ((VMA-START-WRITE) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-BIT-COUNT))))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((WRITE-MEMORY-DATA) SELECTIVE-DEPOSIT WRITE-MEMORY-DATA
                Q-ALL-BUT-POINTER A-INTR-TEM1)  ;Save word count
        ((VMA-START-WRITE) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-WORD-COUNT))))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((A-INTR-TEM2) (A-CONSTANT 1))          ;Offset in buffer array
CHAOS-RCV-INTR-LOOP     ;Read two words out of the hardware, then store them
        ((VMA-START-READ) ADD M-B (A-CONSTANT (EVAL %CHAOS-READ-BUFFER-OFFSET)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((A-INTR-TEM1) ADD (M-CONSTANT -1) A-INTR-TEM1) ;Count down word count
        ((M-TEM) READ-MEMORY-DATA)              ;Save low word
        (JUMP-EQUAL M-ZERO A-INTR-TEM1 CHAOS-RCV-INTR-2)        ;If word count was odd
        ((VMA-START-READ) VMA)                  ;Get high word
        (ILLOP-IF-PAGE-FAULT)                   ;Mustn't bash M-TEM
        ((A-INTR-TEM1) ADD (M-CONSTANT -1) A-INTR-TEM1) ;Count down word count
        ;If the disk is busy, give it time to get three Xbus cycles after our two
        ;Unibus cycles.  Combined with the one Xbus cycle it gets between the
        ;two Unibus cycles, this should be enough to keep it from overrunning,
        ;although just barely.
        (JUMP-NOT-EQUAL-XCT-NEXT A-DISK-BUSY M-ZERO CHAOS-RCV-INTR-1)
       ((M-T) (A-CONSTANT 16.))                 ;6.0 microseconds
        ((M-T) M-ZERO)                          ;Default delay count (no delay)
CHAOS-RCV-INTR-1
        ((WRITE-MEMORY-DATA) DPB READ-MEMORY-DATA (BYTE-FIELD 20 20) A-TEM)
CHAOS-RCV-DELAY
        (JUMP-NOT-EQUAL-XCT-NEXT M-T A-ZERO CHAOS-RCV-DELAY)
       ((M-T) SUB M-T (A-CONSTANT 1))
CHAOS-RCV-INTR-2
        ((VMA-START-WRITE) ADD M-A A-INTR-TEM2) ;Write two halfwords into buffer
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        (JUMP-LESS-THAN-XCT-NEXT M-ZERO A-INTR-TEM1 CHAOS-RCV-INTR-LOOP)
       ((A-INTR-TEM2) M+A+1 M-ZERO A-INTR-TEM2)
        ;; Now save CSR2, enable next receive, and cons onto receive list
        ((VMA-START-READ) M-B)                  ;Get CSR
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((WRITE-MEMORY-DATA) Q-POINTER READ-MEMORY-DATA
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA-START-WRITE) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-CSR-2))))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((WRITE-MEMORY-DATA) IOR WRITE-MEMORY-DATA
                (A-CONSTANT (BYTE-MASK %%CHAOS-CSR-RECEIVER-CLEAR)))
        ((VMA-START-WRITE) M-B)                 ;Write CSR to clear receiver
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        (CALL-XCT-NEXT CHAOS-LIST-PUT)          ;Add packet in M-A to receive list
       ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-RECEIVE-LIST))))
        (JUMP-IF-BIT-CLEAR M-SBS-CHAOS CHAOS-INTR-EXIT) ;Request SB if enabled
        ((INTERRUPT-CONTROL) IOR LOCATION-COUNTER (A-CONSTANT 1_26.))
;drops through
;drops in
CHAOS-WAKEUP (MISC-INST-ENTRY %CHAOS-WAKEUP)
        ;drops in
;; Here to dismiss the interrupt.  We must decide on the interrupt enables.
;; If there are any free buffers, we can enable receive interrupts.
;; If there are any buffers wanting to be transmitted, we can enable transmit interrupts.
CHAOS-INTR-EXIT
        ((M-A) SETZ)                            ;20 = receive-enable, 40 = transmit-enable
        ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-FREE-LIST))))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((M-TEM) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-EQUAL M-TEM A-V-NIL CHAOS-INTR-EXIT-1)
        ((M-A) DPB (M-CONSTANT -1) (LISP-BYTE %%CHAOS-CSR-RECEIVE-ENABLE) A-A)
                                                ;Free list not empty, enable receive done
CHAOS-INTR-EXIT-1
        (JUMP-GREATER-THAN M-ZERO A-CHAOS-TRANSMIT-ABORTED
                CHAOS-INTR-EXIT-2)              ;Disable transmit-done if in abort-timeout
        ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-TRANSMIT-LIST))))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((M-TEM) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-EQUAL M-TEM A-V-NIL CHAOS-INTR-EXIT-2)
        ((M-A) DPB (M-CONSTANT -1) (LISP-BYTE %%CHAOS-CSR-TRANSMIT-ENABLE) A-A)
                                                ;Xmt list not empty, enable transmit done
CHAOS-INTR-EXIT-2
        ((VMA-START-READ) A-CHAOS-CSR-ADDRESS)  ;M-B not valid if called as misc inst
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((M-TEM) READ-MEMORY-DATA)
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT M-A
                (LISP-BYTE %%CHAOS-CSR-INTERRUPT-ENABLES) A-TEM)
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        (JUMP XFALSE)                           ;Could be called as misc inst, mustn't popj

;;; Transmit interrupt handler
;;; A-CHAOS-TRANSMIT-RETRY-COUNT is 0 if nothing going on, otherwise number of retries
;;; before we should give up.  Note buffer not removed from list until done.
CHAOS-XMT-INTR
        ((VMA-START-READ) M-B)                  ;Fetch CSR again
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%CHAOS-CSR-TRANSMIT-DONE) READ-MEMORY-DATA
                CHAOS-INTR-EXIT)                ;Transmit in progress
        (JUMP-EQUAL A-CHAOS-TRANSMIT-RETRY-COUNT M-ZERO CHAOS-XMT-0) ;Jump if transmit idle
        ;; Next instruction starts a retransmission if either we have finished delaying
        ;; after a transmit abort, or if we get woken up or receive a packet during
        ;; a transmit abort delay (this prevents infinite hang if the clock is off and doesn't
        ;; sound too unreasonable).
        (JUMP-NOT-EQUAL A-CHAOS-TRANSMIT-ABORTED M-ZERO CHAOS-XMT-0)
        ;; Here if a transmission really just completed
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%CHAOS-CSR-TRANSMIT-ABORT) READ-MEMORY-DATA
                CHAOS-XMT-DONE)                 ;Jump if transmit done and not aborted
        ((A-COUNT-CHAOS-TRANSMIT-ABORTS) M+A+1 M-ZERO A-COUNT-CHAOS-TRANSMIT-ABORTS)
        ;; If transmit aborted, keep trying until count runs out, then give up
        ((A-CHAOS-TRANSMIT-RETRY-COUNT) ADD (M-CONSTANT -1) A-CHAOS-TRANSMIT-RETRY-COUNT)
        (JUMP-EQUAL M-ZERO A-CHAOS-TRANSMIT-RETRY-COUNT CHAOS-XMT-DONE) ;Give up
        (JUMP-XCT-NEXT CHAOS-INTR-EXIT)         ;Wait a while, then retransmit
       ((A-CHAOS-TRANSMIT-ABORTED) SETO)

CHAOS-XMT-0
        ;; Get current or next transmit packet
        ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-TRANSMIT-LIST))))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((A-CHAOS-TRANSMIT-ABORTED) SETZ)       ;Forget this state left from previous packet
        ((M-A) Q-TYPED-POINTER READ-MEMORY-DATA) ;Note, don't call CHAOS-LIST-GET
                                                ;since we are leaving it on the list for now
        (JUMP-EQUAL M-A A-V-NIL CHAOS-INTR-EXIT)        ;Nothing to transmit, give up
        ;; If this is not a retransmission, initialize retry count
        (JUMP-NOT-EQUAL M-ZERO A-CHAOS-TRANSMIT-RETRY-COUNT CHAOS-XMT-1)
        ((A-CHAOS-TRANSMIT-RETRY-COUNT) (A-CONSTANT CHAOS-NUMBER-TRANSMIT-RETRIES))
CHAOS-XMT-1
        ((VMA-START-READ) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-WORD-COUNT))))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((A-INTR-TEM2) (A-CONSTANT 1))          ;Offset in buffer
        ((A-INTR-TEM1) Q-POINTER READ-MEMORY-DATA)      ;Halfword count
CHAOS-XMT-2
        ((VMA-START-READ) ADD M-A A-INTR-TEM2)  ;Get a pair of halfwords
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((A-INTR-TEM1) ADD (M-CONSTANT -1) A-INTR-TEM1) ;Count down word count
        ;See comments in receive loop above
        (JUMP-NOT-EQUAL-XCT-NEXT A-DISK-BUSY M-ZERO CHAOS-XMT-4)
       ((M-T) (A-CONSTANT 16.))                 ;6.0 microseconds
        ((M-T) M-ZERO)                          ;Default delay count (no delay)
CHAOS-XMT-4
        ((WRITE-MEMORY-DATA) READ-MEMORY-DATA)  ;Write first halfword into hardware
CHAOS-XMT-DELAY
        (JUMP-NOT-EQUAL-XCT-NEXT M-T A-ZERO CHAOS-XMT-DELAY)
       ((M-T) SUB M-T (A-CONSTANT 1))
        ((VMA-START-WRITE) ADD M-B (A-CONSTANT (EVAL %CHAOS-WRITE-BUFFER-OFFSET)))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        (JUMP-EQUAL M-ZERO A-INTR-TEM1 CHAOS-XMT-3)     ;Done if was odd number of words
        ((WRITE-MEMORY-DATA-START-WRITE) (BYTE-FIELD 20 20) WRITE-MEMORY-DATA)
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)              ;Write second halfword into hardware
        ((A-INTR-TEM1) ADD (M-CONSTANT -1) A-INTR-TEM1) ;Count down word count
        (JUMP-LESS-THAN-XCT-NEXT M-ZERO A-INTR-TEM1 CHAOS-XMT-2)
       ((A-INTR-TEM2) M+A+1 M-ZERO A-INTR-TEM2)
CHAOS-XMT-3
        ((VMA-START-READ) ADD M-B (A-CONSTANT (EVAL %CHAOS-START-TRANSMIT-OFFSET)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)               ;Initiate transmission
        (JUMP CHAOS-INTR-EXIT)

;; Here when we are through with a transmit packet.
CHAOS-XMT-DONE
        (CALL-XCT-NEXT CHAOS-LIST-GET)          ;Pull this guy off xmt list, we're done with it
       ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-TRANSMIT-LIST))))
        (CALL-NOT-EQUAL-XCT-NEXT M-A A-V-NIL CHAOS-LIST-PUT)    ;Add to free list
       ((VMA-START-READ) (A-CONSTANT (EVAL (+ 400 %SYS-COM-CHAOS-FREE-LIST))))
        (JUMP-XCT-NEXT CHAOS-XMT-0)             ;Now transmit more if possible
       ((A-CHAOS-TRANSMIT-RETRY-COUNT) SETZ)    ;Transmit not in progress now
#-cadr (end-comment)

#-lambda (begin-comment)

(begin-comment) (end-comment)

;;; This segment cannot be pagable because it is called from the
;;; interrupt routine and may not take a page fault.  This is probably
;;; what causes the mysterious (si:set-processor-owning-ethernet) lossage.
;;; This also causes the incremental micro-assembler to crap out.
;;; Incidentally, what could anyone have had in mind by making a whole 6
;;; instructions pagable?
;(begin-pagable-ucode)

CHAOS-WAKEUP (MISC-INST-ENTRY %CHAOS-WAKEUP)
        ;on the cadr, this enabled interrupts, but we will leave them on all of the time
        (jump xfalse)

lambda-ether-intr       ;only get here if ucode enabled.
  ;     ((m-tem) a-processor-switches)
  ;     (popj-if-bit-clear (lisp-byte %%processor-switch-chaos-ucode-enable) m-tem)

 ;A-INTR-TEM2 gets phys adr of 3com CSR
        ((m-tem) a-sdu-quad-slot)
        ((a-intr-tem2) dpb m-tem (byte-field 8 22.) (a-constant (eval (// #x30000 4))))

        (call-return l-ether-rcv-intr l-ether-xmit-intr)
;       (call l-ether-rcv-intr)
;       (call l-ether-xmit-intr)
;       (popj)
;(end-pagable-ucode)

l-ether-rcv-intr
 ;check for receive done
        ;read the 3com csr, it seems to not mind being read as 32 bits
  ;***
;       ((vma-start-read)
;          (a-constant (plus lowest-multibus-virtual-address (eval (// #x30000 4)))))
;       (check-page-read-map-reload-only)

        (call-xct-next phys-read-low-halfword)
       ((vma) a-intr-tem2)

        ((m-t) md)
        ((m-tem) ldb (byte-field 2 6) md)       ;a+b-bsw
        (popj-equal m-tem (a-constant 3))       ;return - no packets available
        (call-xct-next chaos-list-get)          ;clobbers A-INTR-TEM1
       ((vma-start-read) (a-constant (eval (+ 400 %sys-com-chaos-free-list))))
        (popj-equal m-a a-v-nil)

 ;M-A has an INT-PKT

 ;save CSR
        ((md) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-write) sub m-a (a-constant (eval (+ 2 %chaos-leader-csr-1))))
        (check-page-write-map-reload-only)

 ;Check whether A or B buffer is older (bit 2 = 1 means A is oldest)
        (jump-if-bit-clear (byte-field 1 2) md l-ether-rcv-intr-ba)
        (jump-if-bit-clear-xct-next (byte-field 1 6) md l-ether-rcv-intr-1)
       ((m-b) (a-constant (eval (// #x1000 4))))        ;offset of buffer A
        (jump-xct-next l-ether-rcv-intr-1)
       ((m-b) (a-constant (eval (// #x1800 4))))        ;offset of buffer B

l-ether-rcv-intr-ba
        (jump-if-bit-clear-xct-next (byte-field 1 7) md l-ether-rcv-intr-1)
       ((m-b) (a-constant (eval (// #x1800 4))))        ;offset of buffer B
        ((m-b) (a-constant (eval (// #x1000 4))))       ;offset of buffer A

l-ether-rcv-intr-1
        ((m-b) add m-b a-intr-tem2)

 ;M-B is 30 bit phys address of pkt in multibus

 ;store ethernet type in "bit-count" position of int-pkt leader
        ((vma) add m-b (a-constant 3))
        (call phys-read-as-halfwords)
        ((md) ldb (byte-field 16. 16.) md (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-write) sub m-a (a-constant (eval (+ 2 %chaos-leader-bit-count))))
        (check-page-write-map-reload-only)

 ;store byte-count-swapped (+??) in csr-2 position of int-pkt-leader
        ((vma) m-b)
        (call phys-read-as-halfwords)
        ((md) dpb md q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-write) sub m-a (a-constant (eval (+ 2 %chaos-leader-csr-2))))
        (check-page-write-map-reload-only)
 ;byte swap
        ((m-tem) ldb (byte-field 8 8) md)
        ((md) dpb md (byte-field 8 8) a-tem)

 ;round up to words
        ((md) add md (a-constant 3))
        ((md) (byte-field 10. 2) md)

        ((a-intr-tem1) sub md (a-constant 4))   ;skip 4 words of ethernet header

 ;make sure we stay in bounds of the INT-PKT
        ((vma-start-read) m-a)                  ;read the header
        (check-page-read-map-reload-only)
        ((m-tem) ldb (lisp-byte %%array-index-length-if-short) md)
        ((m-tem) ldb (byte-field 24. 1) m-tem)  ;divide by 2 since ART-16B array
        (jump-greater-than m-tem a-intr-tem1 l-ether-rcv-intr-2)
        ((a-intr-tem1) m-tem)
l-ether-rcv-intr-2

 ;A-INTR-TEM1 is size in 32 bit words

        ((m-t) setz)
        ((m-b) add m-b (a-constant 4))          ;skip ethernet header
 ;m-t is the current offset from M-B in hardware buffer and
 ; one less than the current offset from M-A in the INT-PKT
 ; (we assume the array does not have the LONG-LENGTH-FLAG)

l-ether-rcv-copy
        ((vma) add m-b a-t)
        (call phys-read-as-halfwords)
        ((vma-start-write) m+a+1 m-a a-t)
        (check-page-write-map-reload-only)

        ((m-t) add m-t (a-constant 1))
        ((a-intr-tem1) add m-minus-one a-intr-tem1)

        (jump-less-than m-zero a-intr-tem1 l-ether-rcv-copy)

 ;done copying

        (call-xct-next chaos-list-put)          ;Add packet in M-A to receive list
       ((vma-start-read) (a-constant (eval (+ 400 %sys-com-chaos-receive-list))))

 ;reenable receiver - write csr as 32 bits
        ;check which buffer
        ((md) (a-constant 100))                 ;buf A
        (jump-if-bit-clear (byte-field 1 9.) m-b l-ether-enable-rcv-1)
        ((md) (a-constant 200))                 ;buf B
l-ether-enable-rcv-1
        ((md) add md (a-constant 3410)) ;3com-csr-background-bits
 ;***
;       ((vma-start-write)                      ;3com-csr
;          (a-constant (plus lowest-multibus-virtual-address (eval (// #x30000 4)))))
;       (check-page-write-map-reload-only)

        (call-xct-next phys-write-low-halfword)
       ((vma) a-intr-tem2)

        (popj-if-bit-clear m-sbs-chaos) ;Request SB if enabled
#+lambda((rg-mode) andca rg-mode (a-constant 1_26.))
;**EXP
        (popj)

reset-3com-jam                                  ;32 bit OK
        ((A-COUNT-CHAOS-TRANSMIT-ABORTS) M+A+1 M-ZERO A-COUNT-CHAOS-TRANSMIT-ABORTS)

;       ((md) (a-constant 1))           ;reset interface
;       (call-xct-next phys-write-low-halfword)
;       ((vma) a-intr-tem2)

        ((md) (a-constant (eval (ash -5 16.)))) ;write MEBACK
        (call-xct-next phys-write-high-halfword)
       ((vma) a-intr-tem2)
        ((md) (a-constant 20))                  ;reset JAM
        (call-xct-next phys-write-low-halfword)
       ((vma) a-intr-tem2)

        ((m-tem) (a-constant 500.))
reset-3com-jam-delay
        (jump-greater-than-xct-next m-tem a-zero reset-3com-jam-delay)
       ((m-tem) sub m-tem (a-constant 1))
 ;***
;       ((md-start-write) (a-constant 3410))
;       (check-page-write-map-reload-only)

        ((md) (a-constant 3410))
        (call-xct-next phys-write-low-halfword)
       ((vma) a-intr-tem2)

        (popj)


;;; Transmit interrupt handler
l-ether-xmit-intr
 ;make sure xmit buffer avail
 ;***
;       ((vma-start-read)                       ;3com-csr - 32 bit OK
;          (a-constant (plus lowest-multibus-virtual-address (eval (// #x30000 4)))))
;       (check-page-read-map-reload-only)

        (call-xct-next phys-read-low-halfword)
       ((vma) a-intr-tem2)

        (jump-if-bit-set (byte-field 1 4) md reset-3com-jam)    ;jam
        (popj-if-bit-set (byte-field 1 5) md)   ;tbsw

l-ether-xmit-next-pkt
        (call-xct-next chaos-list-get)
       ((vma-start-read) (a-constant (eval (+ 400 %sys-com-chaos-transmit-list))))

        (popj-equal m-a a-v-nil)        ;Nothing to transmit, give up

 ;got an INT-PKT in M-A

        ;read number of 16 bit words
        ((vma-start-read) sub m-a (a-constant (eval (+ 2 %chaos-leader-word-count))))
        (check-page-read-map-reload-only)
        ((md) q-pointer md)
        ((m-t) add md (a-constant 1))           ;increase to multiple of 4 bytes
        ((m-t) dpb m-zero (byte-field 1 0) a-t)
        ((m-t) add m-t a-t)                     ;convert to number of bytes
        ((m-t) add m-t (a-constant 14.))        ;add size of ethernet header (now only 16b align)
        ((m-tem) (a-constant #x800))            ;size of hardware buffer in bytes
        ((m-t) sub m-tem a-t)                   ;m-t is first byte offset to store into
        ((m-tem) ldb (byte-field 8 8) m-t)
        ((md) dpb m-t (byte-field 8 8) a-tem)   ;byte swap m-t into md
        ;and write into hardware
        ((vma) a-intr-tem2)
        ((vma) add vma (a-constant (eval (// #x800 4))))
        (call phys-write-as-halfwords)

        ((m-t) ldb (byte-field 9 2) m-t)        ;convert M-T to words
        ((m-b) a-intr-tem2)
        ((m-b) add m-b (a-constant (eval (// #x800 4))))
        ((m-b) add m-t a-b)

;copy over address
     ;read the first 3 bytes of address
        ((vma-start-read) sub m-a (a-constant (eval (+ 2 %chaos-leader-csr-1))))
        (check-page-read-map-reload-only)
        ((m-t) md)
        ((md) dpb md (byte-field 16. 16.) a-zero)
        ((vma) m-b)
        (call phys-write-as-halfwords)
        ((m-b) add m-b (a-constant 1))
     ;read next 3 bytes
        ((vma-start-read) sub m-a (a-constant (eval (+ 2 %chaos-leader-csr-2))))
        (check-page-read-map-reload-only)
        ((m-tem) ldb (byte-field 8 16.) m-t)
        ((md) dpb md (byte-field 24. 8) a-tem)
        ((vma) m-b)
        (call phys-write-as-halfwords)
        ((m-b) add m-b (a-constant 1))
     ;now my address
        ((m-tem) a-sdu-quad-slot)
        (call-xct-next phys-read-as-halfwords)  ;read first 4 bytes of address
       ((vma) dpb m-tem (byte-field 8 22.) (a-constant (eval (// #x30600 4))))
        (call-xct-next phys-write-as-halfwords)
       ((vma) m-b)
        ((m-b) add m-b (a-constant 1))

        ((m-tem) a-sdu-quad-slot)
        (call-xct-next phys-read-low-halfword)  ;get next two bytes of address
       ((vma) dpb m-tem (byte-field 8 22.) (a-constant (eval (// #x30604 4))))

        ((m-t) md)
     ;and the type
        ((vma-start-read) sub m-a (a-constant (eval (+ 2 %chaos-leader-bit-count))))
        (check-page-read-map-reload-only)
        ((md) dpb md (byte-field 16. 16.) a-t)
        ((vma) m-b)
        (call phys-write-as-halfwords)
        ((m-b) add m-b (a-constant 1))

        ((m-t) setz)            ;offset

        ;read half-word count and convert to 32 bit words
        ((vma-start-read) sub m-a (a-constant (eval (+ 2 %chaos-leader-word-count))))
        (check-page-read-map-reload-only)
        ((md) add md (a-constant 1))
        ((a-intr-tem1) ldb (byte-field 24. 1) md)

l-chaos-xmit-copy
        ((vma-start-read) m+a+1 m-a a-t)        ;read from INT-PKT
        (check-page-read-map-reload-only)

        ((vma) add m-b a-t)     ;write to multibus
        (call phys-write-as-halfwords)

        ((a-intr-tem1) add m-minus-one a-intr-tem1)     ;step the counters
        ((m-t) add m-t (a-constant 1))

        (jump-less-than m-zero a-intr-tem1 l-chaos-xmit-copy)

 ;start xmit
        ((md) (a-constant (eval (+ 3410 40)))) ;3com-csr-background-bits
  ;***
;       ((vma-start-write)                      ;3com-csr - 32 bit OK
;          (a-constant (plus lowest-multibus-virtual-address (eval (// #x30000 4)))))
;       (check-page-write-map-reload-only)
        (call-xct-next phys-write-low-halfword)
       ((vma) a-intr-tem2)

 ;put INT-PKT on free list
        (call-xct-next chaos-list-put)
       ((vma-start-read) (a-constant (eval (+ 400 %sys-com-chaos-free-list))))
        (popj)

;30 bit phys adr in VMA (i.e. adr of 32. bit word)
;result in MD
;clobbers M-LAM, VMA, MD
phys-read-as-halfwords
        (declare (clobbers a-lam))
        ((md) a-map-scratch-block)
        (no-op)
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((l2-map-physical-page) ldb (byte-field 22. 8.)
                                                ;a-constant means low-halfword mode
                                    vma (a-constant (byte-value (byte-field 2 22.) 1)))

        ((vma-start-read) ldb (byte-field 8 0) vma a-map-scratch-block)
        (illop-if-page-fault)
        ((m-lam) md)
        ((md) a-map-scratch-block)
        (no-op)
        ((l2-map-physical-page) ldb (byte-field 22. 0) l2-map-physical-page
                                                ;now high half-word
                                        (a-constant (byte-value (byte-field 2 22.) 3)))
        ((vma-start-read) vma)
        (illop-if-page-fault)
        ((md) selective-deposit md (byte-field 16. 16.) a-lam)
        (popj)

;clobbers M-LAM, VMA, MD
;data returnned as per hardware, in low half for read low, high for read high.
phys-read-high-halfword
        (jump-xct-next phys-read-halfword)
       ((m-lam) (a-constant (byte-value (byte-field 2 22.) 3)))

phys-read-low-halfword
        ((m-lam) (a-constant (byte-value (byte-field 2 22.) 1)))
phys-read-halfword
        (declare (clobbers a-lam))
        ((md) a-map-scratch-block)
        (no-op)
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((l2-map-physical-page) ldb (byte-field 22. 8.)
                                    vma a-lam)
        ((vma-start-read) ldb (byte-field 8 0) vma a-map-scratch-block)
        (illop-if-page-fault)
        (popj)

;30 bit phys adr in VMA
;data in MD
;clobbers M-LAM, VMA, MD
phys-write-as-halfwords
        (declare (clobbers a-lam))
        ((m-lam) md)
        ((md) a-map-scratch-block)
        (no-op)
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((l2-map-physical-page) ldb (byte-field 22. 8.)
                                                ;a-constant means low-halfword mode
                                    vma (a-constant (byte-value (byte-field 2 22.) 1)))

        ((md) m-lam)
        ((vma-start-write) ldb (byte-field 8 0) vma a-map-scratch-block)
        (illop-if-page-fault)
        ((md) a-map-scratch-block)
        (no-op)
        ((l2-map-physical-page) ldb (byte-field 22. 0) l2-map-physical-page
                                                ;now high half-word
                                        (a-constant (byte-value (byte-field 2 22.) 3)))
        ((md) m-lam)
        ((vma-start-write) vma)
        (illop-if-page-fault)
        (popj)

;30 bit phys adr in VMA
;data in MD
;clobbers M-LAM, VMA, MD, M-TEM
phys-write-high-halfword
        (jump-xct-next phys-write-halfword)
       ((m-lam) (a-constant (byte-value (byte-field 2 22.) 3)))

phys-write-low-halfword
        ((m-lam) (a-constant (byte-value (byte-field 2 22.) 1)))
phys-write-halfword
        (declare (clobbers a-lam))
        ((m-tem) md)
        ((md) a-map-scratch-block)
        (no-op)
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((l2-map-physical-page) ldb (byte-field 22. 8.)
                                    vma a-lam)
        ((md) m-tem)
        ((vma-start-write) ldb (byte-field 8 0) vma a-map-scratch-block)
        (illop-if-page-fault)
        (popj)

#-lambda (end-comment)

#-exp (begin-comment)

(begin-comment) (end-comment)

CHAOS-WAKEUP (MISC-INST-ENTRY %CHAOS-WAKEUP)
        (jump xfalse)
#-exp(end-comment)


;;; Take packet off list which has been VMA-START-READ, return it in M-A
;;; M-A can return with NIL in it.  Uses A-INTR-TEM1
CHAOS-LIST-GET
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)               ;MD gets first buffer on list
        ((A-INTR-TEM1) VMA)                     ;Save address of list header
        ((M-A) Q-TYPED-POINTER READ-MEMORY-DATA)
        (POPJ-EQUAL M-A A-V-NIL)                ;Return if list empty
        ((VMA-START-READ) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-THREAD))))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)               ;MD gets next buffer on list
        ((WRITE-MEMORY-DATA) Q-TYPED-POINTER READ-MEMORY-DATA)
        (POPJ-AFTER-NEXT (VMA-START-WRITE) A-INTR-TEM1)
       (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)

;;; Put packet in M-A onto list which has been VMA-START-READ
;;; Uses A-INTR-TEM1
CHAOS-LIST-PUT
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)               ;MD gets present first buffer on list
        ((A-INTR-TEM1) VMA)                     ;Save address of list header
;cl-put0        ;begin check for buffer already on list.
;       ((WRITE-MEMORY-DATA) Q-TYPED-POINTER READ-MEMORY-DATA)
;       (call-equal md a-a illop)
;       (jump-equal md a-v-nil cl-put1)         ;ok, its not on list.
;       ((vma-start-read) sub md (a-constant (eval (+ 2 %chaos-leader-thread))))
;       (check-page-read-map-reload-only)
;       (jump cl-put0)

;cl-put1        ((vma-start-read) a-intr-tem1)
;       (check-page-read-map-reload-only)

        ((WRITE-MEMORY-DATA) Q-TYPED-POINTER READ-MEMORY-DATA)  ;Thread onto new first buffer
        ((VMA-START-WRITE) SUB M-A (A-CONSTANT (EVAL (+ 2 %CHAOS-LEADER-THREAD))))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((WRITE-MEMORY-DATA) Q-TYPED-POINTER M-A)       ;Change list header
        (POPJ-AFTER-NEXT (VMA-START-WRITE) A-INTR-TEM1)
       (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)


))

;-*-Mode:MIDAS; base: 8; readtable: ZL -*-

;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

;add a-intr-unibus-channel (and change to m memory someday)
;a-kbd-last-two-chars
(DEFCONST UC-LAMBDA-INTERRUPT '(

array-trap
        (call store-array-registers-in-accumulators)

;PUSHJ HERE FOR ERRORS WHICH CAN TRAP TO MACROCODE.
;ON THE CONS MACHINE THIS USED THE OPCS, BUT WE HAVE REPUDIATED THAT PRACTICE.
;THEREFORE THIS CAN'T BE CALLED FROM THE INSTRUCTION AFTER A POPJ-AFTER-NEXT,
;AND SHOULDN'T BE CALLED FROM A CALL-XCT-NEXT UNLESS YOU MOVE THE ERROR-TABLE
;ENTRY DOWN.
TRAP (declare (suspend-flow-tracing))
        (JUMP-IF-BIT-CLEAR M-TRAP-ENABLE ILLOP) ;TURN INTO ILLOP UNLESS TRAPS ENABLED
        ((M-TEM) M-FLAGS-NO-SEQUENCE-BREAK)     ;TURN INTO ILLOP IF TRAP AT BAD TIME
        (JUMP-NOT-EQUAL M-TEM A-ZERO ILLOP)     ;NOTE WOULD PROBABLY DIE LATER ANYWAY
        ((M-TEM) A-SG-STATE)                    ;RECURSIVE TRAP?
        (CALL-IF-BIT-SET (LISP-BYTE %%SG-ST-PROCESSING-ERROR) M-TEM ILLOP)      ;IF SO, HALT
        ((A-SG-STATE) DPB (M-CONSTANT -1) (LISP-BYTE %%SG-ST-PROCESSING-ERROR) A-TEM)
        ((M-TEM) MICRO-STACK-PC-DATA-POP                ;INVOLVES A LDB
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((A-TRAP-MICRO-PC) SUB M-TEM (A-CONSTANT 1))    ;PRESUMED ADDRESS OF CALL
        ((M-TEM3) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-QTRSTKG)
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-QCSTKG)
        (CALL-EQUAL M-TEM A-TEM3 ILLOP)         ;RECURSIVE ERRORS
        ((A-QLBNDH) A-QLBNDRH)                  ;ENSURE NO SPECIAL PDL OVERFLOW STORING STATUS
                                                ;REGULAR PDL IS PREWITHDRAWN, CAN'T OVERFLOW
        (CALL-XCT-NEXT SGLV)                    ;STORE CURRENT STATUS
       ((M-TEM) (A-CONSTANT (EVAL SG-STATE-AWAITING-ERROR-RECOVERY))) ;AND SWAP SPECIAL-PDL
        ((A-SG-TEM) A-V-NIL)    ;Transmit NIL (do not change, EH knows about this.)
        (JUMP-XCT-NEXT SG-ENTER)                        ;"CALL" TRAP HANDLER STACK GROUP
       ((M-A) A-QTRSTKG)

;PUSHJ HERE ON ACTIVATE INVOKE.  OPERATION TYPE FROM I-ARG.
;INVOKE-ACTIVATE
;       (CALL-XCT-NEXT TRAP)    ;NO HANDLER YET
;      ((C-PDL-BUFFER-POINTER-PUSH) DPB READ-I-ARG Q-POINTER    ;SAVE OP-CODE
;               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;      (ERROR-TABLE INVOKE)

;;; INTERRUPTS

;;; This code looks for a Unibus interrupt.  If it finds one it checks whether
;;; it was a keyboard interrupt; if so the character is read out and stored
;;; into the keyboard buffer.  Then the bus interface is readied to take another
;;; interrupt.

;;; Interrupts may clobber only what page faults clobber, plus the A-INTR-TEM
;;; registers.  Interrupts may take page faults to set up the map, but may
;;; not swap in pages.  Interrupts save and restore VMA and MD, but may
;;; possibly invalidate MAP[VMA] and MAP[MD].  Note that if you use
;;; (CHECK-PAGE-READ), an interrupt may occur after the read, and if you
;;; use (CHECK-PAGE-WRITE), an interrupt may occur after the write,
;;; or before it if the page is not swapped in.
;;; It is best if interrupts don't touch the pdl buffer.

;multibus interrupts go to 8259A "programmable" interrupt controllers on the SDU card.
;The SDU allows a multibus interrupt to be reflected to a NUBUS location.  Our convention
;is that multibus interrupt code N (as wired into the 8259As) will be reflected
;into RG interrupt location 100+N (this avoids tying up the special low latency interrupt
;locations 0-17), if it is reflected at all.  The SDU wiring is as follows:
;  0 multibus timeout
;  1 nubus timeout
;  2 quarter inch tape exception
;  3 quarter inch tape ready
;  4 AC lo
;  5 8087 exception (ha ha)
;  6 PIC-2
;  7 PIC-1
; PIC-1
;  10 serial port 0 "remote" receive interrupt   (console)
;  11 serial port 0 "remote" transmit
;  12 serial port 1 "local" receive
;  13 serial port 1 "local" transmit
;  14 programmable interval timer 0
;  15 programmable interval timer 1
;  16 programmable interval timer 2
;  17 unused
; PIC-2
;  20 multibus channel 0, assigned to 3COM ethernet
;  21  " 1, unused
;  22  " 2, assigned to magtape
;  23  " 3, unused
;  24  " 4, assigned to disk
;  25  " 5, assigned to OSI or MTI serial
;  26  " 6, OSI, second interrupt
;  27  " 7, unused

; m-tem is used most of the time to hold option bits from %unibus-channel-csr-bits
; m-a always holds a pointer to the vector-address word of the approiate unibus channel

INTR    (CALL-IF-BIT-SET M-INTERRUPT-FLAG ILLOP);Recursive interrupt!
   (declare (saves (vma a-intr-vma) (md a-intr-md) (a-a a-intr-a) (a-b a-intr-b) (a-t a-intr-t))
            (local a-fake-microsecond-clock))
        ((A-INTR-VMA) VMA)                      ;Mustn't bash the VMA
        ((A-INTR-MD) MD)                        ; nor the MD
        ((M-INTERRUPT-FLAG) DPB (M-CONSTANT -1) A-FLAGS) ;No page faults allowed here
        ((A-INTR-A) M-A)                        ;I need a couple M registers
        ((A-INTR-B) M-B)
        ((A-INTR-T) M-T)                        ;Convenient to be able to clobber this

#+LAMBDA((M-B INTERRUPT-CLEAR) INTERRUPT-POINTER)
#-exp(begin-comment)
        ;get interrupt number
        ((m-b) ldb (byte-field 4 16.) mcr)

        ;clear interrupt in processor table
        ((md) setz)
        ((vma) dpb m-b (byte-field 4 2) a-zero) ;multiply by 4
 ;Bug finding instruction
        ((pdl-push) vma)

        ((vma-start-write-unmapped) add vma (a-constant #xf6e00000))
        (no-op)

;;; Bug finding code.
        ((vma-start-read) (a-constant 10006)) ;magic location
        (illop-if-page-fault)
        (call-equal md (a-constant 377) illop) ;magic number

        (jump-equal m-b (a-constant 14) clock-intr) ;explorer clock intr doesn't have
                ;entry in unibus channel table
#-exp(end-comment)

        ((M-A) (A-CONSTANT (EVAL (+ 400 %SYS-COM-UNIBUS-INTERRUPT-LIST
                                    %UNIBUS-CHANNEL-VECTOR-ADDRESS
                                    (- %UNIBUS-CHANNEL-LINK)))))
INTR-0
        ((VMA-START-READ) ADD M-A
                (A-CONSTANT (EVAL (- %UNIBUS-CHANNEL-LINK %UNIBUS-CHANNEL-VECTOR-ADDRESS))))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((MD) Q-POINTER MD)
        (JUMP-EQUAL MD A-ZERO INTR-RET) ; got to end of list; no takers
        ((VMA-START-READ M-A) ADD MD (A-CONSTANT (EVAL %UNIBUS-CHANNEL-VECTOR-ADDRESS)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((md) q-pointer md)
        (JUMP-NOT-EQUAL MD A-B INTR-0)          ;Loop until find device with this vector

        ((a-intr-unibus-channel) sub m-a (a-constant (eval %unibus-channel-vector-address)))

#-lambda(begin-comment)
        (jump-equal m-b (a-constant 260) vcmem-intr)
        (jump-equal m-b (a-constant 261) quad-intr)
        (jump-equal m-b (a-constant 110) sdu-serial-remote-in)
        (jump-equal m-b (a-constant 111) sdu-serial-remote-out)
        (jump-equal m-b (a-constant 112) sdu-serial-local-in)
        (jump-equal m-b (a-constant 113) sdu-serial-local-out)
        (jump-equal m-b (a-constant 200) ether-intr)
#-lambda(end-comment)
#-exp(begin-comment)
        (jump-equal m-b (a-constant 15) si-keyboard-intr)
#-exp(end-comment)

unknown-micro-interrupt
        (jump intr-ret) ;no ucode for that device, ignore it.


#-lambda(begin-comment)

ether-intr
        ((m-tem) a-processor-switches)
#+LAMBDA(call-if-bit-set (lisp-byte %%processor-switch-chaos-ucode-enable) m-tem
               lambda-ether-intr)
;**EXP
        (jump intr-ret)

vcmem-intr
        ;assume a 60 hz interrupt if vertical-blanking interrupts enabled on VCMEM
        ((vma-start-read) (a-constant (plus 177370400 3)))  ;status register
        (check-page-read-map-reload-only)
        (call-if-bit-set (byte-field 1 0) md tv-vertical-blank-interrupt)

 ;fake chaos interrupt ... call every 60ith
        ((m-tem) a-processor-switches)
#+LAMBDA(call-if-bit-set (lisp-byte %%processor-switch-chaos-ucode-enable) m-tem
               lambda-ether-intr)
;**EXP

kbdin   ((write-memory-data) (a-constant #x00))
        ((vma-start-write) (a-constant (plus 177370400 15)))    ;First address register 0
        (check-page-write-map-reload-only)
        ((vma-start-read) (a-constant (plus 177370400 15)))
        (check-page-read-map-reload-only)
        (jump-if-bit-clear (byte-field 1 0) read-memory-data msein) ;Keyboard char avail?
        ((vma-start-read) (a-constant (plus 177370400 14)))
        (check-page-read-map-reload-only)
        ((m-t) md)
        (call process-keyboard-character)
        (jump kbdin)

;character in M-T
process-keyboard-character
        ;set kbd version number
        ((m-b) dpb md (byte-field 8 0) (a-constant (byte-value (byte-field 3 20) 2)))
        (call unibus-channel-put)
        ((m-b) a-processor-switches)
        (popj-if-bit-clear (lisp-byte %%processor-switch-allow-boot-chars)
                            m-b) ;boot chars not enabled
        ((m-b) a-kbd-last-two-chars)
        ((m-b) dpb m-b (byte-field 8 8) a-zero)
        ((m-b) ldb m-t (byte-field 8 0) a-b)
        ((a-kbd-last-two-chars) m-b)
        (call-equal m-b (a-constant 56642) kbd-boot-char-c-m-c-m-rubout)
        (call-equal m-b (a-constant 27321) kbd-boot-char-c-m-c-m-return)
        (call-equal m-b (a-constant 60237) kbd-boot-char-c-m-c-m-line)
        (call-equal m-b (a-constant 41674) kbd-boot-char-c-m-c-m-end)
        (popj)

msein   ((write-memory-data) (a-constant #x00))
        ((vma-start-write) (a-constant (plus 177370400 17)))    ;First address register 0
        (check-page-write-map-reload-only)
        ((vma-start-read) (a-constant (plus 177370400 17)))
        (check-page-read-map-reload-only)
        (jump-if-bit-clear (byte-field 1 0) md msein-done)

        ((vma-start-read) (a-constant (plus 177370400 16)))     ;Get a mouse character
        (check-page-read-no-interrupt)
        ((m-a) read-memory-data)
        (call process-mouse-char)
        (jump msein)

;;; All done getting input - reset the world
msein-done
        ((write-memory-data) (a-constant #x00)) ;No more input - reset DART interrupt
        ((vma-start-write) (a-constant (plus 177370400 15)))    ;First address register 0 of channel A
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x70))    ;Now issue return from interrupt command
        (check-page-write-map-reload-only)
        (jump intr-ret)         ;Nothing left


quad-intr
        ;interrupt status register
        ((vma-start-read) (a-constant (plus 177277400 5)))
        (check-page-read-no-interrupt)
        (call-if-bit-set (byte-field 1 7) md quad-vsync-intr)
        ((vma-start-read) (a-constant (plus 177277400 5)))
        (check-page-read-no-interrupt)
        (call-if-bit-set (byte-field 1 1) md quad-keyboard-intr)
        ((vma-start-read) (a-constant (plus 177277400 5)))
        (check-page-read-no-interrupt)
        (call-if-bit-set (byte-field 1 5) md quad-mouse-intr)
        ((vma-start-read) (a-constant (plus 177277400 5)))
        (check-page-read-no-interrupt)
        ((md) and md (a-constant (plus 1_7 1_1 1_5)))
        (jump-not-equal md a-zero quad-intr)
        (jump intr-ret)

quad-vsync-intr
        (call tv-vertical-blank-interrupt)
 ;fake chaos interrupt ... call every 60ith
        ((m-tem) a-processor-switches)
        (call-if-bit-set (lisp-byte %%processor-switch-chaos-ucode-enable) m-tem
               lambda-ether-intr)
        ;;this read resets vertical blank interrupts
        ((vma-start-read) (a-constant (plus 177277400 4)))
        (check-page-read-no-interrupt)
        (popj)

quad-keyboard-intr
        ((vma-start-read) (a-constant (plus 177277400 1)))
        (check-page-read-no-interrupt)
        (popj-if-bit-clear (byte-field 1 0) md)
        ((vma-start-read) (a-constant (plus 177277400 3)))
        (check-page-read-no-interrupt)
        ((m-t) md)
        (call process-keyboard-character)
        (jump quad-keyboard-intr)

quad-mouse-intr
        ((vma-start-read) (a-constant (plus 177277400 9)))
        (check-page-read-no-interrupt)
        (popj-if-bit-clear (byte-field 1 0) md)
        ((vma-start-read) (a-constant (plus 177277400 11.)))
        (check-page-read-no-interrupt)
        ((m-a) md)
        (call process-mouse-char)
        (jump quad-mouse-intr)

sdu-serial-remote-in            ;console usually hooked here.
        (call-xct-next internal-multibus-read-8)
       ((m-a) (a-constant #x1c154))
        (jump-if-bit-clear (byte-field 1 1) m-b intr-ret)
        (call-xct-next internal-multibus-read-8)
       ((m-a) (a-constant #x1c150))
        ;store the low 24 bits of the microsecond time in the rest of the word
        (CALL-xct-next unibus-channel-put)
       ((m-b) dpb stat-counter-aux (byte-field 24. 8) a-b)
;       ((VMA-START-READ) (A-CONSTANT (PLUS 177400000 70125)))  ;uart status, port a
;       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
;       (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 1) MD INTR-ret)
;       ((VMA-START-READ) (A-CONSTANT (PLUS 177400000 70124)))  ;uart read data, port a
;       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
;       ;store the low 24 bits of the microsecond time in the rest of the word
;       ((m-b) dpb stat-counter-aux (byte-field 24. 8) a-zero)
;       (CALL-xct-next unibus-channel-put)
;       ((m-b) dpb md (byte-field 8 0) a-b)
        (JUMP sdu-serial-remote-in)

sdu-serial-remote-out   ;"console" is hooked to this one!!
        (call-xct-next internal-multibus-read-8)
       ((m-a) (a-constant #x1c154))
        (jump-if-bit-clear (byte-field 1 0) m-b intr-ret)       ;UART not really ready.
        (call unibus-channel-get)
        (jump-if-bit-set (byte-field 1 0) m-t intr-ret)         ;buffer empty; assume intr edge triggered.
        (call-xct-next internal-multibus-write-8)
       ((m-a) (a-constant #x1c150))

;       ((VMA-START-READ) (A-CONSTANT (PLUS 177400000 70125)))  ;status register
;       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
;       (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) MD INTR-ret)   ;UART not really ready..
;        (call unibus-channel-get)
;       (jump-if-bit-set (byte-field 1 0) m-t intr-ret)  ;buffer empty; assume intr edge triggered
;       ((md) m-b)
;       ((VMA-START-WRITE) (A-CONSTANT (PLUS 177400000 70124)))  ;multibus #x1c150
;       (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)

        (JUMP sdu-serial-remote-out)

sdu-serial-local-in    ;random printers and stuff hooked to this one!!
        (call-xct-next internal-multibus-read-8)
       ((m-a) (a-constant #x1c15c))
        (jump-if-bit-clear (byte-field 1 1) m-b intr-ret)
        (call-xct-next internal-multibus-read-8)
       ((m-a) (a-constant #x1c158))
        ;store the low 24 bits of the microsecond time in the rest of the word
        (CALL-xct-next unibus-channel-put)
       ((m-b) dpb stat-counter-aux (byte-field 24. 8) a-b)

;       ((VMA-START-READ) (A-CONSTANT (PLUS 177400000 70127)))  ;uart status, port b
;       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
;       (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 1) MD INTR-ret)
;       ((VMA-START-READ) (A-CONSTANT (PLUS 177400000 70126)))  ;uart read data, port b
;       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
;       ;store the low 24 bits of the microsecond time in the rest of the word
;       ((m-b) dpb stat-counter-aux (byte-field 24. 8) a-zero)
;       (CALL-xct-next unibus-channel-put)
;       ((m-b) dpb md (byte-field 8 0) a-b)

        (JUMP sdu-serial-local-in)

sdu-serial-local-out
        (call-xct-next internal-multibus-read-8)
       ((m-a) (a-constant #x1c15c))
        (jump-if-bit-clear (byte-field 1 0) m-b intr-ret)       ;UART not really ready.
        (call unibus-channel-get)
        (jump-if-bit-set (byte-field 1 0) m-t intr-ret)         ;buffer empty; assume intr edge triggered.
        (call-xct-next internal-multibus-write-8)
       ((m-a) (a-constant #x1c158))

;       ((VMA-START-READ) (A-CONSTANT (PLUS 177400000 70127)))  ;status register
;       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
;       (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) MD INTR-ret)   ;UART not really ready..
;        (call unibus-channel-get)
;       (jump-if-bit-set (byte-field 1 0) m-t intr-ret)  ;buffer empty; assume intr edge triggered
;       ((md) m-b)
;       ((VMA-START-WRITE) (A-CONSTANT (PLUS 177400000 70126)))
;       (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)

        (JUMP sdu-serial-local-out)

#-lambda (end-comment)

(begin-comment) (end-comment)

#-exp(begin-comment)
clock-intr
        ;read SI board to reset interrupt
        ((vma-start-read-unmapped) (a-constant #xf5e00068))
        (no-op)
        (call TV-VERTICAL-BLANK-INTERRUPT)
        (jump intr-ret)

si-keyboard-intr
        ((vma-start-read-unmapped) (a-constant #xf5fc0000)) ;keyboard status register
        (no-op)
        (jump-if-bit-clear (byte-field 1 1) md intr-ret)
        ((vma-start-read-unmapped) (a-constant #xf5fc0004))
        (no-op)
        ;set kbd version number
        ((m-b) dpb md (byte-field 8 0) (a-constant (byte-value (byte-field 3 20) 3)))
        (call unibus-channel-put)
        (jump si-keyboard-intr)
#-exp(end-comment)

; inputs
;   a-intr-unibus-channel
;   m-b                     received data
; uses
;   m-a
unibus-channel-put  ; used to be intr-2
        ((m-a) a-intr-unibus-channel)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-IN-PTR)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((vma) READ-MEMORY-DATA)
        ((md-START-WRITE) M-B)                  ;Write into buffer
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((m-b) ADD vma (A-CONSTANT 1))          ;Advance storing pointer
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-END)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        (JUMP-GREATER-THAN READ-MEMORY-DATA A-B unibus-channel-put-no-wrap)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-START)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((M-B) READ-MEMORY-DATA)
unibus-channel-put-no-wrap
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ;check for space in buffer - buffer is full if incrementing the in ptr makes
        ; it catch up with the output ptr
        (JUMP-EQUAL READ-MEMORY-DATA A-B unibus-channel-put-check-sb)
        ;buffer has space, store new input ptr
        ((WRITE-MEMORY-DATA) M-B)
        ((VMA-START-WRITE) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-IN-PTR)))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
unibus-channel-put-check-sb
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-CSR-BITS)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        (popj-IF-BIT-CLEAR (LISP-BYTE %%UNIBUS-CSR-SB-ENABLE) MD)
        (popj-after-next popj-IF-BIT-CLEAR M-SBS-UNIBUS)     ;This bit is also required.
#+lambda((RG-MODE) ANDCA RG-MODE (A-CONSTANT 1_26.))    ;Request SEQUENCE-BREAK
#+exp  ((mcr) ior mcr (a-constant 1_14.))


;get next word from unibus channel
;inputs:
;  a-intr-unibus-channel
;outputs
;  m-b  next data word
;  m-t  0 means m-b contains a valid word, 1 means the buffer is empty
;uses
;  m-a
unibus-channel-get
        ((m-a) a-intr-unibus-channel)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-in-PTR)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((m-b) md)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        (popj-equal-xct-next md a-b)
       ((m-t) (a-constant 1))  ;buffer empty flag
        ((VMA-START-READ) READ-MEMORY-DATA)     ;Get next word to go out
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((m-b) md)
        ((M-t) add VMA (a-constant 1))  ;new out pointer
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-END)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        (JUMP-GREATER-THAN READ-MEMORY-DATA A-t unibus-channel-get-no-wrap)
        ((VMA-START-READ) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-START)))
        (CHECK-PAGE-READ-MAP-RELOAD-ONLY)
        ((M-t) READ-MEMORY-DATA)
unibus-channel-get-no-wrap
        ((WRITE-MEMORY-DATA) M-t)
        ((VMA-START-WRITE) ADD M-A (A-CONSTANT (EVAL %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        (popj-after-next (m-t) (a-constant 0))
       (no-op)

INTR-RET
;#-exp(begin-comment)
;;clear all interrupts
;       ((md) setz)
;       ((vma) (a-constant #xf6e00000))
;       ((m-tem) setz)

;intr-ret-0
;       ((vma-start-write-unmapped) vma)
;       (no-op)
;       ((vma) add vma (a-constant 4))
;       ((m-tem) add m-tem (a-constant 1))
;       (jump-less-than m-tem (a-constant 16.) intr-ret-0)
;#-exp(end-comment)

        ((M-INTERRUPT-FLAG) DPB (M-CONSTANT 0) A-FLAGS) ;Allow page faults again
   (declare (restores (vma a-intr-vma) (md a-intr-md)
                 (a-a a-intr-a) (a-b a-intr-b) (a-t a-intr-t)))
        ((MD) A-INTR-MD)
        ((VMA) A-INTR-VMA)

 ;Bug finding instruction
#+exp   ((a-intr-vma) pdl-pop)

        ((M-T) A-INTR-T)
        (POPJ-AFTER-NEXT (M-B) A-INTR-B)        ;Dismiss
       ((M-A) A-INTR-A)


TV-VERTICAL-BLANK-INTERRUPT
        ((M-TEM) A-FAKE-MICROSECOND-CLOCK)
        ((A-FAKE-MICROSECOND-CLOCK) ADD M-TEM (A-CONSTANT 16667.))

        (JUMP-NOT-EQUAL A-DISK-BUSY M-ZERO 60CYC-1)
        ((A-DISK-IDLE-TIME) M+A+1 M-ZERO A-DISK-IDLE-TIME)

60CYC-1 (CALL TRACK-MOUSE)      ;See if the mouse has moved

        (popj-LESS-THAN-XCT-NEXT M-ZERO A-TV-CLOCK-COUNTER)
       ((A-TV-CLOCK-COUNTER) ADD (M-CONSTANT -1) A-TV-CLOCK-COUNTER)
        (popj-IF-BIT-CLEAR-XCT-NEXT M-SBS-CLOCK)
       ((A-TV-CLOCK-COUNTER) ADD (M-CONSTANT -1) A-TV-CLOCK-RATE) ;Counted down, recycle
#+lambda((RG-MODE) ANDCA RG-MODE (A-CONSTANT 1_26.)) ;make a seq break
#+exp   ((mcr) ior mcr (a-constant 1_14.))

reset-watchdog
#-lambda(begin-comment)
        ;reset watchdog
        ;call this occationally from code that may hang the machine for
        ;more than about 3 seconds
        ;currently called from every 60th clock interrupt, cold-disk-[read,write], and xbeep
        ;clobbers only VMA and MD
        ;first make sure our proc-conf really has the new variable
        ;remove these four lines after release 1 is just a memory
        ((vma) (a-constant sys-conf-virtual-adr))
        ((vma-start-read) add vma (a-constant (eval %system-configuration-processor-block-size)))
        (check-page-read-map-reload-only)
        (popj-less-or-equal md (a-constant (eval %processor-conf-watchdog)))

        ;don't touch it if it is -1
        ((vma) a-proc-conf-virtual-adr)
        ((vma-start-read) add vma (a-constant (eval %processor-conf-watchdog)))
        (check-page-read-map-reload-only)
        (popj-equal md (a-constant -1))

        ;reset it to the initial value
        ((md-start-write) dpb m-zero q-all-but-pointer a-initial-watchdog)
        (check-page-write-map-reload-only)
#-lambda(end-comment)
        (popj)

#-lambda (begin-comment)
internal-multibus-write-8       ;write 8 bits in m-b to <SDU-quad-slot> byte address in m-a.
                ;clobbers m-tem and m-lam.
          (declare (args a-b a-a) (clobbers a-tem a-lam))
        ((md) a-map-scratch-block)
        ((m-tem) a-sdu-quad-slot)               ;gives maps time to settle
        ((l2-map-control) (a-constant 5460))    ;packet size code 1
        ((m-lam) dpb m-tem (byte-field 8 14.) a-zero)
        ((m-lam) dpb m-a (byte-field 2 22.) a-lam)  ;low bits of byte address
                        ;to high bits of l2-map-physical-page
        ((l2-map-physical-page) ldb m-a (byte-field 13. 10.) a-lam)
                        ;page number bits to page number section of map.
        ((m-tem) dpb (byte-field 2 3) m-a a-zero)
        ((oa-reg-low) dpb m-tem oal-mrot a-zero)
        ((md) dpb (byte-field 8 0) m-b a-zero)
        ((vma-start-write) ldb (byte-field 8 2) m-a a-map-scratch-block)
        (illop-if-page-fault)
        (popj)

internal-multibus-read-8      ;read 8 bits into m-b from <sdu-quad-slot> byte address in M-A.
          (declare (args a-a) (clobbers a-tem a-lam) (values a-b))
                        ;clobbers md, vma, m-tem and m-lam.
        ((md) a-map-scratch-block)
        ((m-tem) a-sdu-quad-slot)               ;give L1 map time to settle..
        ((l2-map-control) (a-constant 5460))    ;packet size code 1
        ((m-lam) dpb m-tem (byte-field 8 14.) a-zero)
        ((m-lam) dpb m-a (byte-field 2 22.) a-lam)  ;low bits of byte address
                        ;to high bits of l2-map-physical-page
        ((l2-map-physical-page) ldb m-a (byte-field 13. 10.) a-lam)
                        ;page number bits to page number section of map.
        ((vma-start-read) ldb (byte-field 8 2) m-a a-map-scratch-block)
        (illop-if-page-fault)
        ((m-tem) dpb (byte-field 2 3) m-a a-zero)
        ((m-tem) sub (m-constant 40) a-tem)
        ((oa-reg-low) dpb m-tem oal-mrot a-zero)
        (popj-after-next        ;see comments in uc-lambda re p.a.n.
         (m-b) ldb (byte-field 8 0) md a-zero)
       (no-op)

#-lambda (end-comment)
))

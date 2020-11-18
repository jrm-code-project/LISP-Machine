;-*- Mode:LISP; Base:8; readtable: ZL -*-
;       ** (c) Copyright 1983, Lisp Machine Inc **
;       ** (c) Copyright 1984, Lisp Machine Inc **
;       ** (c) Copyright 1985, Lisp Machine Inc **
;       ** (c) Copyright 1986, Lisp Machine Inc **
;NOTE: THIS FILE FOLLOWS UC-PARAMETERS AND HAS FIRST I-MEM CODE.
; THIS FILE IS ONLY FOR LAMBDA.  A COMPLETELY DIFFERENT FILE (UC-CADR) IS USED WITH CADR.

;on LAMBDA, we distinguish a PHYSICAL-ADDRESS which is a 32 bit NUBUS address versus
; a HARDWARE-VIRTUAL-ADDRESS which is a 24 bit virtual address which references the
; physical address with the "standard" mapping in place.  Note that this does not imply
; the map is wired, just that if a page fault is taken the map reloader will know
; (by convention) how to set up the map just by range checking the virtual address.

(DEFCONST UC-LAMBDA '(
;also note: A-LOWEST-DIRECT-VIRTUAL-ADDRESS  holds the lowest direct mapped
; virtual address, normally LOWEST-A-MEM-VIRTUAL-ADDRESS.  But it can be set lower
; ie if you are using the new color TV board you need 128K of direct mapped space
; below that to reference the video buffer.
(ASSIGN LOWEST-A-MEM-VIRTUAL-ADDRESS 176776000) ;MUST BE 0 MODULO SIZE OF A-MEM
(ASSIGN LOWEST-IO-SPACE-VIRTUAL-ADDRESS 177000000)  ;BEGINING OF X-BUS IO SPACE
(ASSIGN LOWEST-UNIBUS-VIRTUAL-ADDRESS 177400000)    ;END OF X-BUS, BEGINNING OF UNIBUS
                                                ;really multibus.
(assign lowest-multibus-virtual-address 177400000)
;On CADR, compare with these after clearing the sign bit of the address
;(which is done since that bit is meaningless in the map on a CADR).  On Lambda, these
;provided for "compatibility"
(ASSIGN INTERNAL-LOWEST-A-MEM-VIRTUAL-ADDRESS 176776000)    ;MUST BE 0 MODULO SIZE OF A-MEM
(ASSIGN INTERNAL-LOWEST-IO-SPACE-VIRTUAL-ADDRESS 177000000) ;BEGINING OF X-BUS IO SPACE
(ASSIGN INTERNAL-LOWEST-UNIBUS-VIRTUAL-ADDRESS 177400000)   ;END OF X-BUS, BEGINNING OF UNIBUS

(ASSIGN DISK-REGS-ADDRESS-BASE 177377100)       ;maped to multibus IO space so as to ref
                ; disk control registers.  Includes low bits of disk device number


;these are for the CADR disk control.  Used for purposes of interpretation only.
(ASSIGN DISK-READ-COMMAND 0)
(ASSIGN DISK-WRITE-COMMAND 11)
(ASSIGN DISK-READ-COMPARE-COMMAND 10)
(ASSIGN DISK-RECALIBRATE-COMMAND 10001005)

;these are for the INTERPHASE disk controller.
(ASSIGN IP-DISK-READ-COMMAND-WORD 10601)
(ASSIGN IP-DISK-WRITE-COMMAND-WORD 10602)
(ASSIGN IP-DISK-READ-COMPARE-COMMAND-WORD 10603)        ;verify command.
(ASSIGN IP-DISK-RECALIBRATE-COMMAND-WORD 10611)         ;called RESTORE

(ASSIGN IP-DISK-START-COMMAND 43)       ;word mode, clear interrupt, and go.

;(ASSIGN TV-REGS-ADDRESS-BASE 177300000)                ;mapped to tv card, slot address, 0-37777
 ;IN REGISTER 0, BIT 3 IS INTERRUPT ENABLE, BIT 4 IS INTERRUPT FLAG
(ASSIGN DISK-RUN-LIGHT-VIRTUAL-ADDRESS 177051763)       ;ok since video buffer mapped same

;(ASSIGN MOUSE-HARDWARE-VIRTUAL-ADDRESS 77400377)   ;multibus 3FC-3FF.
        ;CADR is. Unibus 764104 Y, 764106 X
        ;This shouldn't actually be used by anyone.

;(ADVANCE-INSTRUCTION-STREAM) TO GET NEXT HALFWORD
#+lambda (ASSIGN ADVANCE-INSTRUCTION-STREAM
                 (plus macro-stream-advance
                       dispatch
                       (byte-field 1 20)               ;NEEDFETCH-L BIT
                       rg-mode
                       d-advance-instruction-stream))
#+exp   (ASSIGN ADVANCE-INSTRUCTION-STREAM
                 (plus macro-stream-advance
                       dispatch
                       needfetch                       ;NEEDFETCH BIT (not L!)
                       d-advance-instruction-stream))

;a-memory allocation:
;  0-100 image of M mem
;  to approx 1500 (1506 used as of ucode 721)  A memory variables and constants.
; to 1600 (*a-constant-table-limit*) can be used for a-constants for micro-compiled code.
;-- no longer  1600 to 1740  mouse cursor stuff.  See mouse-cursor-pattern-amem-loc, etc.
;                Indirected to by arrays in the world load!
;   ** these two only in lambda! **
;  1740 to 1757  findcore-scan-pointer (per hexadec)
;  1760 to 1777  aging-scan-pointer  (per hexadec)
;   ** 1774 to 1777 used by explorer prom to pass boot info to main microcode **
;   ** it also seems it might trash 1700-1773 ** -rg
; currently on lambda only, should be moved to mem as on explorer
;  2000 to 2177  reverse first level map
;  2200 to 2277  copy of constants page (loaded by LOAD-CONSTANTS-PAGE).
;  2300 to 2377  was gap, then was dispatch mem, now gap again.  Reserved for Ucompiler A-MEM constants..
; lambda-only
;  2400 to 3000  reverse map for control mem paging, 1 wd ea page of c-mem from 30000-37777.
;       has virtual c-mem address of page which resides here, or 0 if free, or -1 wired.
;  3400-7777  dispatch memory.
#+lambda (a-constant-limit 1740)
#+exp    (a-constant-limit 1770)

(LOCALITY A-MEM)
A-RG-QUAD-SLOT      (#xF0)                      ;rg slot, set from RG-MODE at RESET-MACHINE
A-SDU-QUAD-SLOT     (#xFF)                      ;slot SDU plugged in.
A-TV-QUAD-SLOT      (#xF8)                      ;slot video memory board plugged in.
;see also A-GREY-QUAD-SLOT below.
A-PROCESSOR-SWITCHES (0)                ;reserved.  Also check QCOM for bit defs.  Defined in COLD;SYSCONF
                                ;left half "need dependant" bits.
                                ; bit 31. -> use stat2 for usec clock (3.x+ boards only)
                                ; bit 30. -> allow boot chars to halt machine, or cold boot
                                ; bit 29. -> use multiplier in UC-TV
                                ; bit 28. -> use disk sharing protocol
                                ; bit 27. -> prom jumps to cold boot
                                ; bit 26. -> slot numbers set up
                                ; bit 25. -> 2x2 stuff valid in conf structure
                                ; bit 24. -> new sys conf mapping
                                ; bit 23. -> allow debug-illops to halt machine.
                                ; bit 1,0 -> packet.size.code
                                ; bit 2,  -> cache.permit
                                ; bit 3,  -> cache.permit for video buffer.
                                ; bit 4,  -> inhibit fast cache mode (swap pages in such that
                                ;               virtual-adr&17 = physical-adr&17
                                ;       (this used to be the opposite sense, now the new
                                ;        thing is on by default, set this bit to clear it).
                                ; bit 5,  -> enable quantum map fast booting

#+lambda A-L2-MAP-CONTROL-BITS (0) ;bits for L2-MAP-CONTROL which are not in cadr come from here
                                ;these are:  bits 11,10 packet.size.code
                                ;       bit 12, packetize.writes
                                ;       bit 13, cache.permit
                                ;       bit 14, lock.nubus
                                ;       bit 15, unused.

#+lambda A-IOPB-BASE    #+lambda (640)  ;build IOPBs starting from here.
#+exp a-nupi-command-block-base #+exp (640)
#+exp a-nupi-special-event-vadr #+exp (650)

A-MULTIBUS-DISK-DEVICE-NUMBER (100)     ;see also DISK-REGS-ADDRESS-BASE and page mapping code
                                        ; lossage..
A-MULTIBUS-DISK-MAP-BASE  (577) ;First multibus map register used for mapping disk xfer
                                ; to NUBUS.  First page is used for mapping the IOPB to
                                ; A-IOPB-BASE in system-communication-area.  Rest of pages
                                ; map data area to be transferred.
                                ; multibus address used for transfer, ie 60000 hex
                                ;note, tho, these map register must be written bytewise
                ;can be changed by conf structure..
A-MULTIBUS-MAP-HARDWARE-VIRTUAL-ADDRESS (177371000)  ;base maps to multibus 18000 hex, byte 0
                                ; next page to high regs, byte 0; low regs, byte 1, etc
                                ; for a total of 6 pages.

A-LAST-L2-MAP-CONTROL       (0)  ;last map entries clobbered by LOAD-L2-MAP-FROM-CADR-PHYSICAL
A-LAST-L2-MAP-PHYSICAL-PAGE (0)  ; used by PHYS-MEM-READ.  may be useful for debugging.
A-FAKE-MICROSECOND-CLOCK    (0)  ; gets incremented by 16667 every 1/60th of a second

a-disk-cylinder-offset (100.)   ;offset entire disk world by yea much!!!

A-PMH-0 (0)     ;first Hardware-virtual-address-page NOT in memory 0
A-PMH-1 (0)     ;first page not in memory 1, etc
A-PMH-2 (0)
A-PMH-3 (0)
A-PMH-4 (0)
A-PMH-5 (0)
A-PMH-6 (0)
A-PMH-7 (0)
A-PMH-8 (0)
A-PMH-9 (0)

A-PMO-0 (0)     ;nubus page number of first page in memory 0
A-PMO-1 (0)
A-PMO-2 (0)
A-PMO-3 (0)
A-PMO-4 (0)
A-PMO-5 (0)
A-PMO-6 (0)
A-PMO-7 (0)
A-PMO-8 (0)
A-PMO-9 (0)

A-INTR-UNIBUS-CHANNEL (0)                       ;temp for new interrupt stuff
A-KBD-LAST-TWO-CHARS (0)                        ;last two chars from keyboard for booting

;;; Stuff for the serial mouse handler
a-mouse-fake-register   (0)     ;This pretends to be the mouse register
a-mouse-phase   (-1)                    ;Records what the mouse expects to get next
a-mouse-temp-x  (0)                     ;Saves the delta-x from mouse until delta-y received

(assign sys-conf-virtual-adr 177367000)
a-proc-conf-local-phys-adr (0)
a-proc-conf-virtual-adr (0)
a-sys-conf-base-phys-page (0)
a-my-iopb-valid-flag-physical-adr (0)

a-disk-page-partition-name      (10521640520)   ; PAGE = 105 107 101 120 = 10521640520
a-disk-band-partition-name      (0)     ;maybe read from config structure.  If non-zero,
                        ;this used instead of "current" band from label.
a-disk-ucode-partition-name     (0)
a-disk-ucode-partition-start    (0)     ;so as to be available for reading UCODE into
a-disk-ucode-partition-length   (0)     ; micro-code-paging-area
a-disk-ucode-partition-unit     (0)

a-disk-lod-partition-start      (0)
a-disk-lod-partition-length     (0)
a-disk-lod-partition-unit       (0)

a-grey-quad-slot  (#xF9)                ;slot where medium resolution color plugged in.

a-map-scratch-block (177340000)  ;A block of 32. pages on a 32. page boundary starting here.
        ;level-1 map entry set to point to level 2 block 176 at inimap.
        ;%nubus-read, etc, can clobber these map entries without restoring them.

a-local-phys-adr-convert (0) ;#x10000000 if on far side of bus coupler

;TEMPORARY HOME FOR THIS, SHOULD BE MOVED INTO UC-PARAMETERS.
A-DEFAULT-CALL-STATE ((BYTE-VALUE Q-DATA-TYPE DTP-FIX))  ;base used to build call state word of
        ;call block by CBM.  Normally fixnum zero, has %%lp-cls-attention bit set if
        ;by %set-meter-enables if any metering enabled.

A-MACRO-HISTORY-POINTER  (2300) ;points to next location about to be clobbered in
                                ; macro history ram in A-MEM.  ranges 2300-2777

A-MICRO-FAULT-DC (0)    ;Save dispatch constant at micro-fault.
A-MICRO-FAULT-M-1 (0)
A-MICRO-FAULT-M-2 (0)
A-MICRO-FAULT-M-3 (0)
A-MICRO-FAULT-VMA (0)
A-MICRO-FAULT-MD  (0)
A-MICRO-FAULT-PI  (0)
a-micro-fault-tem (0)  ;saved M-TEM

;number of multibus mapping registers starting
;at A-MULTIBUS-DISK-MAP-BASE + 2 available for
;disk transfers
;the first 2 pages are used for the IOPB and share IOPB
;real value loaded from SYSCONF
a-number-of-data-mapping-registers-for-disk (176)

a-defer-boot-char-mode (0)


;;; INITIALIZATION

(LOCALITY I-MEM)
#-lambda(begin-comment)
        (loc 36001)
prom-36001
        (loc 36004)
prom-done
        (loc 36007)
prom-got-error
#-lambda (end-comment)

(loc 0)

#-lambda(begin-comment)
ZERO    (JUMP ZERO HALT-CONS)           ;WILD TRANSFER TO ZERO

;This is location 1.  Enter here if virtual memory is valid.
BEG
        (JUMP warm-boot)

;loc 2
cold-start-with-conf-ptr
        (jump cold-boot-with-conf-ptr)

;loc 3 - cold-boot
        (JUMP COLD-BOOT)

;loc 4 - PUSHJ HERE FOR FATAL ERRORS, E.G. THINGS THAT CAN'T HAPPEN.
;        ALSO FOR THINGS WHICH DON'T HAVE ERROR-TABLE ENTRIES YET.

;; Unused entries go to UNKNOWN-MISC, below, now.  Pace 850601.
;; (MICRO-CODE-ILLEGAL-ENTRY-HERE)      ;FILL IN UNUSED ENTRIES IN MICRO-CODE-SYMBOL-AREA.

ILLOP   (declare (suspend-flow-tracing))
        (POPJ HALT-CONS)                ;Halt with place called from in lights
        (NO-OP)

;loc 6
illop-debug-halt    (declare (suspend-flow-tracing))
        (POPJ HALT-CONS)
        (no-op)

;loc 10
sdu-halt                                        ;stop and wait for SDU to do something
        (popj halt-cons)
        (no-op)

;loc 12
        (no-op halt-cons)

;loc 13
        (no-op halt-cons)

;loc 14
        (no-op halt-cons)

;loc 15
        (no-op halt-cons)

;loc 16
        (no-op halt-cons)

;loc 17
        (no-op halt-cons)

(modulo 20)
micro-fault-page        ;this should be location 20, ie, CRAM page 1.
        (repeat 20 (call micro-fault))

#-lambda (end-comment)

#-exp (begin-comment)
ZERO    (JUMP ZERO HALT-CONS)           ;WILD TRANSFER TO ZERO

;This is location 1.  Enter here if virtual memory is valid.
BEG
        (JUMP warm-boot)

;loc 2 - power-fail warning
        (no-op halt-cons)

;loc 3 - enter here from PROM
        (jump cold-boot-with-conf-ptr)

;loc 4 - control store parity error
        (no-op halt-cons)

;loc 5
        (no-op halt-cons)

;loc 6 - warm start interrupt (really handled by prom)
;        prom clobbers 1@m which is m-pgf-tem, then jumps here
;        someday, we will look at the SI board to find out what kind of boot to do
;        for now, assume warm boot
        (jump beg)

;loc 7
        (no-op halt-cons)

;loc 10 - call to ILLOP abbrv jump
        (jump illop)

;loc 11
        (no-op halt-cons)

;loc 12 - call to trap abbrv jump
        (jump trap)

;loc 13
        (no-op halt-cons)

;loc 14 - call to buserr abbrv jump
        (no-op halt-cons)

;loc 15
        (no-op halt-cons)

;loc 16 - unused abbrv jump
        (no-op halt-cons)

;loc 17
        (no-op halt-cons)

;loc 20 - bus error abort trap
        (no-op halt-cons)

;loc 21
        (no-op)



; PUSHJ HERE FOR FATAL ERRORS, E.G. THINGS THAT CAN'T HAPPEN.

;loc 22
ILLOP   (declare (suspend-flow-tracing))
        (jump write-crash-record)
        (HALT-CONS)
        (NO-OP)
        (popj)
        (no-op)

;loc 26
illop-debug-halt    (declare (suspend-flow-tracing))
        (HALT-CONS)
        (no-op)
        (popj)
        (no-op)

#-exp (end-comment)

illop-debug  ;debugging error checks come here.  They are ignored unless
               ; debug error halts enabled.
        (declare (clobbers a-tem1) (local a-count-illop-debug))
        ((a-count-illop-debug) M+A+1 m-zero a-count-illop-debug)
        ((m-tem1) a-processor-switches)
        (call-if-bit-set (byte-field 1 23.) m-tem1 illop-debug-halt)
        (popj)

;; (%WRITE-INTERNAL-PROCESSOR-MEMORIES CODE ADR D-HI D-LOW)
;;   CODE SELECTS WHICH MEMORY GETS WRITTEN. 1 -> I, 2 -> D, 4 -> A/M, 5 -> MID, 6 -> read MID.
;;  for LAMBDA, only code 4, 5 hacks this way.  1 temporarily reinstalled..
;;    (THIS IS A SUBSET OF THE CODE USED IN MCR FILES).
XWIPM (MISC-INST-ENTRY %WRITE-INTERNAL-PROCESSOR-MEMORIES)
        ((M-1) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-2) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-A) Q-POINTER C-PDL-BUFFER-POINTER-POP)              ;ADDRESS
        ((M-B) Q-POINTER C-PDL-BUFFER-POINTER-POP)              ;CODE
        (JUMP-EQUAL M-B (A-CONSTANT 1) XWIPM-I)
        (jump-equal m-b (a-constant 4) xwipm-a)
#+lambda(jump-equal m-b (a-constant 5) xwipm-mid)
#+lambda(jump-equal m-b (a-constant 6) xripm-mid)
#+exp   (jump-equal m-b (a-constant 7) xwipm-d)
        (call trap)
   (ERROR-TABLE BAD-INTERNAL-MEMORY-SELECTOR-ARG M-B)
  ;for WRITE-A, 3rd arg is high 8 bits, 4th arg is low 24.  (this losing way for
  ; "compatibility").
xwipm-a
        ((M-1) DPB M-2 (BYTE-FIELD 10 30) A-1)  ;M-1 GETS 32 BITS DATA
        (JUMP-LESS-THAN M-A (A-CONSTANT 100) XWIPM-M)
        ((OA-REG-LOW) DPB M-A OAL-A-DEST A-ZERO)
        ((A-GARBAGE) M-1)
        (JUMP XFALSE)

XWIPM-M ((OA-REG-LOW) DPB M-A OAL-M-DEST A-ZERO)
        ((M-GARBAGE) M-1)
        (JUMP XFALSE)

XWIPM-I
       ((c-pdl-buffer-pointer-push) m-1)
        (call-xct-next get-32-bits)
       ((c-pdl-buffer-pointer-push) m-2)
        (call-xct-next get-32-bits)
       ((m-2) m-1)
        ;;; Now the first word of the instruction is in M-1, second word is in M-2,
        ;;; and the address in I-MEM is in M-A.
#-lambda (begin-comment)

        ((OA-REG-LOW) DPB M-A OAL-JUMP A-ZERO)
        (CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT (CRAM-HIGH) M-2)        ;write high word first to guard against
        ((OA-REG-LOW) DPB M-A OAL-JUMP A-ZERO)  ; certain randomnesses..
        (CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT (CRAM-LOW) M-1)
#-lambda (end-comment)
#-exp  (begin-comment)

        ((OA-REG-LOW) DPB M-A OAL-JUMP A-ZERO)  ;IMOD SELECTS I-MEM ADDRESS
        (CALL-XCT-NEXT 0)                       ;GENERATE ADDRESS FOR WRITE
        (POPJ-AFTER-NEXT WRITE-CONTROL-STORE A-2 M-1)
#-exp (end-comment)
        (JUMP XFALSE)

#-lambda (begin-comment)
xwipm-mid
;m-a address, m-1 low bits, m-2 hi bits
        ((m-3) rg-mode)

        ((m-tem) dpb m-zero (byte-field 1 31) a-3)      ;turn off enable MISC-MID
        ((md) ldb (byte-field 2 10.) m-a a-zero) ;get high 2 bits of address
        ((rg-mode) dpb md (byte-field 2 34) a-tem)

        ((m-tem) dpb m-a (byte-field 10. 6) a-zero)
        ((md) dpb m-tem (byte-field 20 20) a-tem)
        (source-to-macro-ir md)
        ((macro-ir-decode) m-1)

        ((rg-mode) m-3)

        ((location-counter) location-counter) ;force return to QMLP to reload macro-ir

        (jump xfalse)

xripm-mid
        ((m-3) rg-mode)

        ((m-tem) dpb m-zero (byte-field 1 31) a-3)
        ((md) ldb (byte-field 2 10.) m-a a-zero)
        ((rg-mode) dpb md (byte-field 2 34) a-tem)

        ((m-tem) dpb m-a (byte-field 10. 6) a-zero)
        ((md) dpb m-tem (byte-field 20 20) a-tem)
        (source-to-macro-ir md)
        ((m-t) dpb macro-ir-decode q-pointer (a-constant (byte-value q-data-type dtp-fix)))

        ((rg-mode) m-3)

        ((location-counter) location-counter) ;force return to QMLP to reload macro-ir

        (popj)
#-lambda (end-comment)
#-exp    (begin-comment)
XWIPM-D ;; M-A has address M-1 has data.
        ((OA-REG-LOW) DPB M-A OAL-DISP A-ZERO)  ;IMOD SELECTS D-MEM ADR
         (DISPATCH A-1 WRITE-DISPATCH-MEMORY)   ;WRITE SELECTED D-MEM LOCATION
        (JUMP XFALSE)
#-exp   (end-comment)


;unknown misc's come here
  (MICRO-CODE-ILLEGAL-ENTRY-HERE)       ;FILL IN UNUSED ENTRIES IN
                                        ; MICRO-CODE-SYMBOL-AREA
unknown-misc
        (call trap)
    (error-table illegal-instruction)

XIO-READ (MISC-INST-ENTRY %IO-SPACE-READ)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
    (ERROR-TABLE ARG-POPPED 0 PP)
        ((VMA-START-READ) ADD C-PDL-BUFFER-POINTER-POP  ;XBUS word addr
                (A-CONSTANT LOWEST-IO-SPACE-VIRTUAL-ADDRESS))

        (CHECK-PAGE-READ-no-interrupt)          ;Mustn't check for sequence breaks since
        (JUMP-XCT-NEXT RETURN-M-1-UNSIGNED) ;on some devices reading has side effects and if
      ((M-1) READ-MEMORY-DATA)          ;a sequence break occurred we would read it twice

XIO-WRITE (MISC-INST-ENTRY %IO-SPACE-WRITE)
  ;this can be OK even on explorer for writing to video buffer, etc.
        (CALL-XCT-NEXT GET-32-BITS)             ;M-1 gets value to write
       ((m-t) q-typed-pointer c-pdl-buffer-pointer)     ;Save copy to return.
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
           (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((WRITE-MEMORY-DATA) M-1)
        ((VMA-START-WRITE) ADD C-PDL-BUFFER-POINTER-POP ;Return random fixnum in M-T
                (A-CONSTANT LOWEST-IO-SPACE-VIRTUAL-ADDRESS))
        (CHECK-PAGE-WRITE-no-interrupt) ;avoid double write if interrupt.
        (POPJ)

#-lambda (begin-comment)
xmultibus-read-32 (misc-inst-entry %multibus-read-32)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
      (error-table arg-popped 0 pp)
         ((vma-start-read) ldb (byte-field 17. 2)
                               c-pdl-buffer-pointer-pop
                               (a-constant lowest-unibus-virtual-address))
         (check-page-read)
         (jump-xct-next return-m-1-unsigned)
        ((m-1) md)

xmultibus-read-16 (misc-inst-entry %multibus-read-16)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
      (error-table arg-popped 0 pp)
        ((md) a-map-scratch-block)
        ((m-a) a-sdu-quad-slot)         ;give L1 map time to settle..
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((m-i) dpb m-a (byte-field 8 14.) a-zero)
        ((m-i) dpb c-pdl-buffer-pointer (byte-field 2 22.) a-i)  ;get bit 1 of adr into
                                                                 ;map.phys.1
        ((m-i) dpb m-minus-one (byte-field 1 22.) a-i) ;turn on map.phys.0
        ((l2-map-physical-page) ldb c-pdl-buffer-pointer (byte-field 13. 10.) a-i)
                        ;page number bits to page number section of map.
        ((vma-start-read) ldb (byte-field 8 2) c-pdl-buffer-pointer a-map-scratch-block)
        (illop-if-page-fault)
        ((m-a) ldb (byte-field 1 1) c-pdl-buffer-pointer-pop) ; get bit 1 of adr
        ((m-a) dpb m-a (byte-field 1 4) a-zero) ; multiply by 16
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        (popj-after-next  ;no p.a.n. on prev uinst since this one needs md.
         (m-t) ldb (byte-field 16. 0) md (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)

xmultibus-read-8 (misc-inst-entry %multibus-read-8)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
      (error-table arg-popped 0 pp)
        ((md) a-map-scratch-block)
        ((m-a) a-sdu-quad-slot)         ;give L1 map time to settle..
        ((l2-map-control) (a-constant 5460))    ;packet size code 1
        ((m-i) dpb m-a (byte-field 8 14.) a-zero)
        ((m-i) dpb c-pdl-buffer-pointer (byte-field 2 22.) a-i)  ;low bits of byte address
                        ;to high bits of l2-map-physical-page
        ((l2-map-physical-page) ldb c-pdl-buffer-pointer (byte-field 13. 10.) a-i)
                        ;page number bits to page number section of map.
        ((vma-start-read) ldb (byte-field 8 2) c-pdl-buffer-pointer a-map-scratch-block)
        (illop-if-page-fault)
        ((m-a) dpb (byte-field 2 3) c-pdl-buffer-pointer-pop a-zero)
        ((m-a) sub (m-constant 40) a-a)
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        (popj-after-next        ;see above re p.a.n.
         (m-t) ldb (byte-field 8 0) md (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)
#-lambda(end-comment)

#-lambda (begin-comment)
XNUBUS-READ     (MISC-INST-ENTRY %NUBUS-READ)   ;quad-slot, slot-byte-address.
XNUBUS-READ-SAFE (MISC-INST-ENTRY %NUBUS-READ-SAFE) ;quad-slot, slot-byte-address.
        (CALL GET-32-BITS)              ;slot byte adr (which can be 24 bits).
                                        ;value  in M-1.
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
                (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((MD) a-map-scratch-block)      ;following inst gives maps time to settle.
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 8 24.) A-1)  ;FULL 32 BIT NUBUS ADR
        ((L2-MAP-CONTROL) (a-constant 1464))    ;no caching.
        ((L2-MAP-PHYSICAL-PAGE) LDB M-1 (BYTE-FIELD 22. 10.) A-ZERO)
        ((VMA-START-READ) LDB (BYTE-FIELD 8 2) M-1 a-map-scratch-block)
        (ILLOP-IF-PAGE-FAULT)
        (JUMP-XCT-NEXT RETURN-M-1-UNSIGNED)
       ((M-1) MD)                       ;SAVE 32 BIT RESULT
#-lambda (end-comment)

#-exp (begin-comment)
XNUBUS-READ     (MISC-INST-ENTRY %NUBUS-READ)   ;quad-slot, slot-byte-address.
        (call do-nubus-read)
        (jump-not-equal-xct-next vma a-minus-one return-m-1-unsigned)
       ((m-1) md)
        ((m-a) (a-constant 1))                  ;cycle code
        ((m-1) m-2)                             ;phys adr
        (jump nubus-cycle-trap)

; M-1 contains nubus physical address
; M-2 contains the data (on a write)
; M-A has cycle code:
;       1 = %nubus-read
;       2 = %nubus-read-byte
;       3 = %nubus-write
;       4 = %nubus-write-byte
; MD has contents of memory status register
;
; The error handler can do the cycle over, and set M-T to the desired return value
nubus-cycle-trap
        ((m-b) ldb (byte 16. 0) md)
        ((m-t) (a-constant (byte-value q-data-type dtp-fix)))
        (call trap)
    (error-table nubus-error)
        (popj)

XNUBUS-READ-SAFE (MISC-INST-ENTRY %NUBUS-READ-SAFE) ;quad-slot, slot-byte-address.
        (call do-nubus-read)
        (jump-not-equal-xct-next vma a-minus-one return-m-1-unsigned)
       ((m-1) md)
        (popj-xct-next)
       ((M-T) A-V-NIL)

do-nubus-read
        ((m-tem) q-typed-pointer pdl-top)
        (jump-equal m-tem a-v-true test-nubus-read)
        (CALL GET-32-BITS)              ;slot byte adr (which can be 24 bits).
                                        ;value  in M-1.
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
                (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((vma m-2) dpb pdl-pop (byte-field 8 24.) a-1)
        (jump nubus-read)
#-exp (end-comment)

(begin-comment) zwei lossage (end-comment)

#-exp (begin-comment)
;32 bit physical address in VMA, returned data in MD
;will do try again later's, but if there is a bus error, then
;return with VMA = 0
;clobbers M-TEM
nubus-read
        ((md) md) ;wait for previous cycle
        ;;turn off bus error and parity aborts
        ((m-tem) mcr)
        ((mcr) andca mcr (a-constant (plus 1_12. 1_13.)))
        ((vma-start-read-unmapped) vma)
        (no-op)
        ((md) md) ;wait for cycle
        ((mcr) m-tem)
        (popj-if-no-bus-error)
        ((m-tem) (a-constant 1))
        (call nubus-error)
        (jump nubus-read)

test-nubus-read
        ((m-garbage) pdl-pop)
        (CALL GET-32-BITS)              ;slot byte adr (which can be 24 bits).
                                        ;value  in M-1.
        ((vma m-2) m-1)
        ((md) md) ;wait for previous cycle
        ;;turn off bus error and parity aborts
        ((m-tem) mcr)
        ((mcr) andca mcr (a-constant (plus 1_12. 1_13.)))
        ((vma-start-read-unmapped) vma)
        (no-op)
        ((md) md) ;wait for cycle
        ((mcr) m-tem)
        ((m-t) a-v-true)
        (popj-if-no-bus-error)
        ((vma-start-read-unmapped) (a-constant #xf4ffc014))
        (no-op)
        ((m-1) md)
        (jump return-m-1-unsigned)

nubus-read-byte
        ((md) md) ;wait for previous cycle
        ;;turn off bus error and parity aborts
        ((m-tem) mcr)
        ((mcr) andca mcr (a-constant (plus 1_12. 1_13.)))
        ((vma-start-read-byte-unmapped) vma)
        (no-op)
        ((md) md) ;wait for cycle
        ((mcr) m-tem)
        (popj-if-no-bus-error)
        ((m-tem) (a-constant 2))
        (call nubus-error)
        (jump nubus-read-byte)

;32 bit physical address in VMA, data in MD
;will do try again later's, but if there is a bus error, then
;return with VMA = 0
;clobbers M-TEM
nubus-write
        ((md) md) ;wait for previous cycle
        ;;turn off bus error and parity aborts
        ((m-tem) mcr)
        ((mcr) andca mcr (a-constant (plus 1_12. 1_13.)))
        ((vma-start-write-unmapped) vma)
        (no-op)
        ((md) md) ;wait for cycle
        ((mcr) m-tem)
        (popj-if-no-bus-error)
        ((m-tem) (a-constant 3))
        (call nubus-error)
        (jump nubus-write)

nubus-write-byte
        ((md) md) ;wait for previous cycle
        ;;turn off bus error and parity aborts
        ((m-tem) mcr)
        ((mcr) andca mcr (a-constant (plus 1_12. 1_13.)))
        ((vma-start-write-byte-unmapped) vma)
        (no-op)
        ((md) md) ;wait for cycle
        ((mcr) m-tem)
        (popj-if-no-bus-error)
        ((m-tem) (a-constant 3))
        (call nubus-error)
        (jump nubus-write-byte)

nubus-error
        ((m-tem) vma)
        ((vma-start-read-unmapped) (a-constant #xf4ffc014))
        (no-op)
        (jump-if-bit-set (byte-field 1 14.) md nubus-error-bus-timeout)
        (jump-if-bit-set (byte-field 1 15.) md nubus-error-parity-error)
        ;fall in for 0
        ((md) ldb (byte 2 6.) md)
        (jump-equal md (a-constant 1) nubus-error-parity-error)
        (jump-equal md (a-constant 2) nubus-error-bus-timeout)
        ;fall in for 3 "try again later" (or 0, "no error", but we shouldn't be here)
        ;try cycle again
        ((vma) m-tem)
        (popj)

nubus-error-bus-timeout
nubus-error-parity-error
        ((vma-start-read-unmapped) (a-constant #xf4ffc014))
        (no-op)
        ((vma) m-minus-one)
        ((m-garbage) micro-stack-data-pop)
        (popj)

#-exp(end-comment)

#-lambda (begin-comment)
XNUBUS-READ-8   (MISC-INST-ENTRY %NUBUS-READ-8) ;quad-slot, slot-byte-address.
XNUBUS-READ-8-SAFE (MISC-INST-ENTRY %NUBUS-READ-8-SAFE) ;quad-slot, slot-byte-address.
        (CALL GET-32-BITS)              ;slot byte adr (which can be 24 bits).
                                        ;value  in M-1.
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
                (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((MD) a-map-scratch-block)      ;following inst gives maps time to settle.
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 8 24.) A-1)  ;FULL 32 BIT NUBUS ADR
        ((L2-MAP-CONTROL) (a-constant 5464))    ;no caching, packet size code 1
        ((m-i) LDB M-1 (BYTE-FIELD 22. 10.) A-ZERO)
        ((L2-MAP-PHYSICAL-PAGE) DPB M-1 (byte-field 2 22.) a-i)  ;low bits of byte address
                        ;to high bits of l2-map-physical-page
        ((VMA-START-READ) LDB (BYTE-FIELD 8 2) M-1 a-map-scratch-block)
        (ILLOP-IF-PAGE-FAULT)
        ((m-a) dpb (byte-field 2 3) m-1 a-zero)
        ((m-a) sub (m-constant 40) a-a)
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        (popj-after-next        ;see above re p.a.n.
         (m-t) ldb (byte-field 8 0) md (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)
#-lambda (end-comment)

#-exp (begin-comment)
XNUBUS-READ-8   (MISC-INST-ENTRY %NUBUS-READ-8) ;quad-slot, slot-byte-address.
        (call do-nubus-read-8)
        (jump-not-equal vma a-minus-one xnubus-read-8-got-it)
        ((m-a) (a-constant 2))                  ;cycle code
        ((m-1) m-2)                             ;phys adr
        (jump nubus-cycle-trap)
xnubus-read-8-got-it
        ((m-a) dpb (byte-field 2 3) m-1 a-zero)
        ((m-a) sub (m-constant 40) a-a)
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        (popj-after-next        ;see above re p.a.n.
         (m-t) ldb (byte-field 8 0) md (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)

XNUBUS-READ-8-SAFE (MISC-INST-ENTRY %NUBUS-READ-8-SAFE) ;quad-slot, slot-byte-address.
        (call do-nubus-read-8)
        (jump-not-equal vma a-minus-one xnubus-read-8-got-it)
        (popj-xct-next)
       ((M-T) A-V-NIL)

do-nubus-read-8
        (CALL GET-32-BITS)              ;slot byte adr (which can be 24 bits).
                                        ;value  in M-1.
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
                (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((vma m-2) dpb c-pdl-buffer-pointer-pop (byte-field 8 24.) a-1)
        (jump nubus-read-byte)
#-exp (end-comment)

#-lambda (begin-comment)
xmultibus-write-32 (misc-inst-entry %multibus-write-32)
        (call-xct-next get-32-bits)                     ;m-1 gets value to write
       ((m-t) q-typed-pointer c-pdl-buffer-pointer)     ;Save copy to return.
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
           (error-table argtyp fixnum pp 0)
        ((write-memory-data) m-1)
        ((vma-start-write) ldb (byte-field 17. 2)
                                   c-pdl-buffer-pointer-pop
                                   (a-constant lowest-unibus-virtual-address))
        (check-page-write-no-interrupt) ;avoid double write if interrupt
        (popj)

xmultibus-write-16 (misc-inst-entry %multibus-write-16)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 1)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop) ; word to write
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
      (error-table arg-popped 0 pp)
        ((md) a-map-scratch-block)
        ((m-a) a-sdu-quad-slot)         ;give L1 map time to settle..
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((m-i) dpb m-a (byte-field 8 14.) a-zero)
        ((m-i) dpb c-pdl-buffer-pointer (byte-field 2 22.) a-i)  ;get bit 1 of adr into
                                                                 ;map.phys.1
        ((m-i) dpb m-minus-one (byte-field 1 22.) a-i) ;turn on map.phys.0
        ((l2-map-physical-page) ldb c-pdl-buffer-pointer (byte-field 13. 10.) a-i)
                        ;page number bits to page number section of map.
        ((m-a) ldb (byte-field 1 1) c-pdl-buffer-pointer) ; get bit 1 of adr
        ((m-a) dpb m-a (byte-field 1 4) a-zero) ; multiply by 16
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        ((md) dpb (byte-field 16. 0) m-t a-zero)
        ((vma-start-write) ldb (byte-field 8 2) c-pdl-buffer-pointer-pop a-map-scratch-block)
        (illop-if-page-fault)
        (popj)          ;no popj after next, since return to main loop could start mem cycle

xmultibus-write-8 (misc-inst-entry %multibus-write-8)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (ERROR-TABLE ARGTYP FIXNUM PP 1)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;WORD TO WRITE
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
      (error-table argtyp fixnum pp 0)
      (error-table arg-popped 0 pp)
        ((md) a-map-scratch-block)
        ((m-a) a-sdu-quad-slot)         ;gives maps time to settle
        ((l2-map-control) (a-constant 5460))    ;packet size code 1
        ((m-i) dpb m-a (byte-field 8 14.) a-zero)
        ((m-i) dpb c-pdl-buffer-pointer (byte-field 2 22.) a-i)  ;low bits of byte address
                        ;to high bits of l2-map-physical-page
        ((l2-map-physical-page) ldb c-pdl-buffer-pointer (byte-field 13. 10.) a-i)
                        ;page number bits to page number section of map.
        ((m-a) dpb (byte-field 2 3) c-pdl-buffer-pointer a-zero)
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        ((md) dpb (byte-field 8 0) m-t a-zero)
        ((vma-start-write) ldb (byte-field 8 2) c-pdl-buffer-pointer-pop a-map-scratch-block)
        (illop-if-page-fault)
        (popj)          ;no popj after next, since return to main loop could start mem cycle
#-lambda (end-comment)

#-exp (begin-comment)
XNUBUS-WRITE    (MISC-INST-ENTRY %NUBUS-WRITE)  ;range-slot, slot-byte-address, data
        (call do-nubus-write)
        (popj-not-equal vma a-minus-one)
        ((m-a) (a-constant 3))                  ;cycle code
        ;M-1 already contains the phys adr
        (jump nubus-cycle-trap)

XNUBUS-WRITE-NO-TRAP (MISC-INST-ENTRY %NUBUS-WRITE-SAFE) ;range-slot, slot-byte-address, data
        (call do-nubus-write)
        (popj-after-next popj-not-equal vma a-minus-one)
       ((M-T) A-V-NIL)

do-nubus-write
        (CALL-XCT-NEXT GET-32-BITS)             ;get data to write.
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER)     ;Save copy to return.
        (CALL-XCT-NEXT GET-32-BITS)             ;slot byte adr (which can be 24 bits).
       ((M-2) M-1)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
          (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((md) m-2)
        ((vma m-1) dpb c-pdl-buffer-pointer-pop (byte-field 8 24.) a-1)
        (jump nubus-write)
#-exp (end-comment)

#-lambda (begin-comment)
XNUBUS-WRITE    (MISC-INST-ENTRY %NUBUS-WRITE)  ;range-slot, slot-byte-address, data
XNUBUS-WRITE-NO-TRAP (MISC-INST-ENTRY %NUBUS-WRITE-SAFE) ;range-slot, slot-byte-address, data
        (CALL-XCT-NEXT GET-32-BITS)             ;get data to write.
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER)     ;Save copy to return.
        (CALL-XCT-NEXT GET-32-BITS)             ;slot byte adr (which can be 24 bits).
       ((M-2) M-1)
        (call-data-type-not-equal c-pdl-buffer-pointer
                (a-constant (byte-value q-data-type dtp-fix)) trap)
          (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((MD) a-map-scratch-block)              ;following inst gives maps time to settle
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 8 24.) A-1)  ;FULL 32 BIT NUBUS ADR
        ((L2-MAP-CONTROL) (a-constant 1464))    ;no caching.
        ((L2-MAP-PHYSICAL-PAGE) LDB M-1 (BYTE-FIELD 22. 10.) A-ZERO)
        ((MD) M-2)
        ((VMA-START-WRITE) LDB (BYTE-FIELD 8 2) M-1 a-map-scratch-block)
        (ILLOP-IF-PAGE-FAULT)
        (POPJ)          ;No POPJ-AFTER-NEXT, etc.  Value in M-T.
#-lambda (end-comment)

#-exp (begin-comment)
XNUBUS-WRITE-8  (MISC-INST-ENTRY %NUBUS-WRITE-8)        ;range-slot, slot-byte-address, data
        (call do-nubus-write-8)
        (popj-not-equal vma a-minus-one)
        ((m-a) (a-constant 4))                  ;cycle code
        ;M-1 already has phys adr
        (jump nubus-cycle-trap)

XNUBUS-WRITE-8-SAFE (MISC-INST-ENTRY %NUBUS-WRITE-8-SAFE) ;range-slot, slot-byte-address, data
        (call do-nubus-write-8)
        (popj-after-next popj-not-equal vma a-minus-one)
       ((M-T) A-V-NIL)

do-nubus-write-8
        (CALL-XCT-NEXT GET-32-BITS)             ;get data to write.
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER)     ;Save copy to return.
        (CALL-XCT-NEXT GET-32-BITS)             ;slot byte adr (which can be 24 bits).
       ((M-2) M-1)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
                (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((m-a) dpb (byte-field 2 3) m-1 a-zero)
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        ((md m-2) dpb (byte-field 8 0) m-2 a-zero)
        ((vma m-1) dpb pdl-pop (byte-field 8 24.) a-1)
        (jump nubus-write-byte)
#-exp (end-comment)

#-lambda (begin-comment)
XNUBUS-WRITE-8  (MISC-INST-ENTRY %NUBUS-WRITE-8)        ;range-slot, slot-byte-address, data
XNUBUS-WRITE-8-SAFE (MISC-INST-ENTRY %NUBUS-WRITE-8-SAFE) ;range-slot, slot-byte-address, data
        (CALL-XCT-NEXT GET-32-BITS)             ;get data to write.
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER)     ;Save copy to return.
        (CALL-XCT-NEXT GET-32-BITS)             ;slot byte adr (which can be 24 bits).
       ((M-2) M-1)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
                (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((MD) a-map-scratch-block)              ;following inst gives maps time to settle
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 8 24.) A-1)  ;FULL 32 BIT NUBUS ADR
        ((L2-MAP-CONTROL) (a-constant 5464))    ;no caching, packet size code 1.
        ((m-i) LDB M-1 (BYTE-FIELD 22. 10.) A-ZERO)   ;page number bits
        ((L2-MAP-PHYSICAL-PAGE) DPB M-1 (byte-field 2 22.) a-i)  ;low bits of byte address
                        ;to high bits of l2-map-physical-page
        ((m-a) dpb (byte-field 2 3) m-1 a-zero)
        ((oa-reg-low) dpb m-a oal-mrot a-zero)
        ((md) dpb (byte-field 8 0) m-2 a-zero)
        ((VMA-START-WRITE) LDB (BYTE-FIELD 8 2) M-1 a-map-scratch-block)
        (ILLOP-IF-PAGE-FAULT)
        (POPJ)          ;No POPJ-AFTER-NEXT, etc.  Value in M-T.
#-lambda (end-comment)


;;; %STORE-CONDITIONAL pointer, old-val, new-val
;;; This is protected against interrupts, provided that the value you
;;; are storing does not point at the EXTRA-PDL, and that the location
;;; is guaranteed never to contain a pointer to old-space (i.e. it
;;; only points to static areas.)  This is always protected against
;;; sequence breaks (other macrocode processes).
XSTACQ (MISC-INST-ENTRY %STORE-CONDITIONAL) ;args are pointer, old-val, new-val
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;new
        ((M-B) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;old
        (CALL-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)) TRAP)
    (ERROR-TABLE ARGTYP LOCATIVE PP 0)
;Won't interrupt between reading out the data here
        ((VMA-START-READ) C-PDL-BUFFER-POINTER-POP) ;pntr
xstacq1 (CHECK-PAGE-READ-NO-INTERRUPT)
        (DISPATCH TRANSPORT-READ-WRITE READ-MEMORY-DATA)
        ((M-1) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-NOT-EQUAL M-B A-1 XFALSE)         ;Return NIL if old-val was wrong
        ((WRITE-MEMORY-DATA-START-WRITE)        ;Otherwise, store new-val
                SELECTIVE-DEPOSIT
                READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER A-A)
;and writing the replacement data here
        (CHECK-PAGE-WRITE-no-interrupt)
        (GC-WRITE-TEST)
        (popj-after-next
          (M-T) A-V-TRUE)
       (no-op)

;;; %STORE-CONDITIONAL-DOUBLE pointer, old-val, new-val-pointer, new-val
;;;  similar to %STORE-CONDITIONAL, but location <new-val-pointer> must ALSO
;;;  contain <new>-val for a successful store to happen.
;;; This is protected against interrupts, provided that the value you
;;; are storing does not point at the EXTRA-PDL, and that the location
;;; is guaranteed never to contain a pointer to old-space (i.e. it
;;; only points to static areas.)  This is always protected against
;;; sequence breaks (other macrocode processes).
XDSTACQ (MISC-INST-ENTRY %STORE-CONDITIONAL-DOUBLE) ;args are pointer, old-val, new-val-pointer, new-val
        ((M-A) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;new
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;new-val-pointer
        (CALL-DATA-TYPE-NOT-EQUAL M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)) TRAP)
    (ERROR-TABLE ARGTYP LOCATIVE M-C)
        ((M-B) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP) ;old
        (CALL-DATA-TYPE-NOT-EQUAL PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)) TRAP)
    (ERROR-TABLE ARGTYP LOCATIVE PP 0)
        ((M-D) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((VMA-START-READ) M-C)  ;NEW-VAL-POINTER
        (CHECK-PAGE-READ-NO-INTERRUPT)
        ((M-1) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-NOT-EQUAL M-1 A-A XFALSE)         ;jump on c(new-val-pointer) .neq. new-val
        (jump-xct-next xstacq1)
       ((VMA-START-READ) M-D) ;pntr

;;; Read microsecond clock into M-2  (preserve A-TEM1)
READ-MICROSECOND-CLOCK
        (declare (values a-2) (local a-fake-microsecond-clock))
        ((m-2) a-processor-switches)
        (jump-if-bit-set (byte-field 1 31.) m-2 rmc-1)
        ((M-2) A-FAKE-MICROSECOND-CLOCK)
        (POPJ)
rmc-1
#+lambda((m-2) stat-counter-aux)
#+exp (call illop)
        (popj)

;the following two routines should be combined into the above one.  but be careful,
; registers are very touchy.

read-microsecond-clock-into-md
        (declare (local a-fake-microsecond-clock))
        ((md) a-processor-switches)
        (jump-if-bit-set (byte-field 1 31.) md rmc-2)
        ((MD) A-FAKE-MICROSECOND-CLOCK)
        (POPJ)
rmc-2
#+lambda((md) stat-counter-aux)
#+exp (call illop)
        (popj)

xmicrosecond-time (misc-inst-entry %microsecond-time)
#-lambda(begin-comment)
        ((m-lam) a-processor-switches)
        ((m-1) stat-counter-aux)
        (jump-if-bit-set (byte-field 1 31.) m-lam return-m-1-unsigned)
#-lambda(end-comment)
        ((m-1) a-fake-microsecond-clock)
        (jump return-m-1-unsigned)

xfixnum-microsecond-time (misc-inst-entry %fixnum-microsecond-time)
#-lambda(begin-comment)
        ((m-lam) a-processor-switches)
        ((m-t) stat-counter-aux)
        (popj-if-bit-set-xct-next (byte-field 1 31.) m-lam)
        ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-fix)))
#-lambda(end-comment)
        (popj-after-next (m-t) a-fake-microsecond-clock)
       ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-fix)))

XHALT (MISC-INST-ENTRY %HALT)
        (JUMP HALT-LAMBDA XFALSE)               ;CONTINUING RETURNS NIL

;routines which translate addresses

;to try to keep things straight, the following names are used
; -CADR-PHYSICAL
;    CADR software expects to see 21 bits worth of contigious physical main memory,
;  but NUBUS address space is 32 bits (of bytes) and not contigious.  So the CADR-PHYSICAL
;  to NUBUS mapping is introduced.  This mapping remains entirely static once system
;  has been booted.

; -HARDWARE-VIRTUAL-ADDRESSS  virtual address which maps thru to specific hardware register
; -NUBUS-ADDRESS              32 bit NUBUS byte address

;input: nubus address in M-LAM
;output: cadr-physical in M-LAM
translate-nubus-to-cadr-physical
        ;hack!!
        (jump-greater-than m-lam (a-constant #xf4000000) tncp-first-board)
        ((m-lam) sub m-lam (a-constant #xf3000000))
        ((m-lam) ldb (byte-field 30. 2.) m-lam)
        ((m-lam) add m-lam (a-constant 4000_8))
        (popj)
tncp-first-board
        ((m-lam) sub m-lam (a-constant #xf4000000))
        ((m-lam) ldb (byte-field 30. 2.) m-lam)
        (popj)

(DEF-DATA-FIELD BITS-WITHIN-PAGE-IN-NUBUS-ADDRESS 8 2)
(DEF-DATA-FIELD BITS-FROM-MAP-IN-NUBUS-ADDRESS 22. 10.)

TRANSLATE-CADR-PHYSICAL-TO-NUBUS        ;M-T has CADR-PHYSICAL, RESULT IN M-LAM
 ;must not clobber M-TEM1
        (declare (args a-t) (values a-lam) (clobbers a-tem))
        (jump-if-bit-set m-t (byte-field 1 31.) tcp-1)  ;if sign bit set, m-t is a
                                        ;NUBUS WORD address. Shift two and return it.
        (CALL-XCT-NEXT CADR-PHYSICAL-PAGE-TO-NUBUS-PAGE)
       ((M-TEM) LDB VMA-PHYS-PAGE-ADDR-PART M-T A-ZERO)
        (POPJ-AFTER-NEXT
         (M-LAM) DPB M-T BITS-WITHIN-PAGE-IN-NUBUS-ADDRESS A-ZERO)
       ((M-LAM) DPB M-TEM BITS-FROM-MAP-IN-NUBUS-ADDRESS A-LAM)
tcp-1   (popj-after-next
          (m-lam) dpb m-t (byte-field 30. 2) a-zero)
       (no-op)

LOAD-L2-MAP-FROM-CADR-PHYSICAL
 ;come here with desired map already addressed (by MD) and
 ;CADR L2 map entry in M-LAM.  Load L2-MAP-CONTROL from appropriate piece,
 ;and L2-MAP-PHYSICAL-PAGE from converted physical page.
 ;DO NOT CHANGE MD ON XCT-NEXT THAT GETS HERE!
 ;MAP2C-VOLATILITY bits are always in map2c-volatility bits, even on explorer.
        (declare (args a-lam) (clobbers a-tem))
        ((A-LAST-L2-MAP-CONTROL) L2-MAP-CONTROL)   ;used by PHYS-MEM-READ to restore map.
                                                   ; also may be useful for debugging.
        ((A-LAST-L2-MAP-PHYSICAL-PAGE) L2-MAP-PHYSICAL-PAGE)
        ((m-tem) ldb pht2-access-status-and-meta-bits m-lam
                #+lambda a-l2-map-control-bits #+exp a-zero)  ;insert appropriate cache
           ;and packet code bits on lambda.
;on LAMBDA, complement bits and write stuff.
#+lambda((l2-map-control) xor m-tem (a-constant (byte-value map2c-volatility 3)))
;on EXP, move bits as well.
#+exp   ((m-tem) xor m-tem (a-constant (byte-value map2c-volatility 3)))
#+exp   ((vma-write-l2-map-control) dpb m-tem exp-map2c-volatility a-tem)
                  ;depends on map2c-volatility being right justified.

        ((M-TEM) LDB MAP2C-STATUS-CODE L2-MAP-CONTROL)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT 5) LL2M-CP1)  ;NOT MAY BE  IN PDL BUFFER
        ((M-TEM) L2-MAP-CONTROL)        ;Turn on VALID-IF-FORCE bit.  This allows
                        ;pdl buffer map hacking routines do accesses without modifing map
                        ;and modifing it back.
  ;unfortunately, however, this bit does not seem to work on EXP, so its not depended on.
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) DPB M-MINUS-ONE MAP2C-READ-ACCESS-IF-FORCE A-TEM)

LL2M-CP1

        (CALL-XCT-NEXT CADR-PHYSICAL-PAGE-TO-NUBUS-PAGE)
       ((M-TEM) LDB pht2-PHYSICAL-PAGE-NUMBER M-LAM A-ZERO)
        (POPJ-AFTER-NEXT
           (#+lambda L2-MAP-PHYSICAL-PAGE
            #+exp vma-write-l2-map-physical-page) M-TEM)

       (NO-OP)

NUBUS-PHYSICAL-ADDRESS
        (MISC-INST-ENTRY %NUBUS-PHYSICAL-ADDRESS)
        (CALL-XCT-NEXT CADR-PHYSICAL-PAGE-TO-NUBUS-PAGE)
       ((M-TEM) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        (POPJ-AFTER-NEXT
          (M-T) DPB M-TEM Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       (NO-OP)

;For now, we provide for 10. physical memories. Note that we do not assume they are all
; the same size.
CADR-PHYSICAL-PAGE-TO-NUBUS-PAGE (declare (args a-tem) (values a-tem))
        (JUMP-LESS-THAN M-TEM A-PMH-0 PM0)
        ((M-TEM) SUB M-TEM A-PMH-0)
        (JUMP-LESS-THAN M-TEM A-PMH-1 PM1)
        ((M-TEM) SUB M-TEM A-PMH-1)
        (JUMP-LESS-THAN M-TEM A-PMH-2 PM2)
        ((M-TEM) SUB M-TEM A-PMH-2)
        (JUMP-LESS-THAN M-TEM A-PMH-3 PM3)
        ((M-TEM) SUB M-TEM A-PMH-3)
        (JUMP-LESS-THAN M-TEM A-PMH-4 PM4)
        ((M-TEM) SUB M-TEM A-PMH-4)
        (JUMP-LESS-THAN M-TEM A-PMH-5 PM5)
        ((M-TEM) SUB M-TEM A-PMH-5)
        (JUMP-LESS-THAN M-TEM A-PMH-6 PM6)
        ((M-TEM) SUB M-TEM A-PMH-6)
        (JUMP-LESS-THAN M-TEM A-PMH-7 PM7)
        ((M-TEM) SUB M-TEM A-PMH-7)
        (JUMP-LESS-THAN M-TEM A-PMH-8 PM8)
        ((M-TEM) SUB M-TEM A-PMH-8)
        (JUMP-LESS-THAN M-TEM A-PMH-9 PM9)
        (CALL ILLOP)

PM0     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-0)

PM1     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-1)

PM2     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-2)

PM3     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-3)

PM4     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-4)

PM5     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-5)

PM6     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-6)

PM7     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-7)

PM8     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-8)

PM9     (POPJ-AFTER-NEXT NO-OP)
       ((M-TEM) ADD M-TEM A-PMO-9)



#-lambda (begin-comment)
;MICRO-PAGING
;  control memory consists of the following regions:
; 0 < END-WIRED-UCODE     --  loaded by PROM, always resident.
;end-wired-ucode < highest-kernal-ucode-location  -- loaded by PROM,
;       evicted by turn-pagable-ucode-on-and-evict-initialization
;       reinstated by turn-pagable-ucode-off-and-restore-initialization
;36000 < 40000   prom.  overwritten if micro-code-paging-reset done with bit 1 set.
;               if that happens, it is recorded by a-prom-flushed .ne. 0.
;40000 < 60000  available for hand coded pagable ucode.
;60000 <100000  available for microcompiled (or incrementally assembled) pagable ucode.
;                       (this boundary is arbitrary, just dont clobber the hand coded stuff).

;Initially, the initialization is loaded but the hand-coded pagable stuff is not (because
;  the prom refuses to load anything above 36000).
;After the lisp environment is initialized but before any lisp code is actually run,
;  the I-MEM part of the selected LMC file is read into MICRO-CODE-PAGING-AREA
;  (read-ucode-into-ucode-paging-area).
;After that, but still before LISP really starts, the initialization is evicted.
;  (turn-pagable-ucode-on-and-evict-initialization).

;At any time while lisp is running, a micro-paging-reset can be done (possible with the
; overwrite PROM option).  This merely throws all pagable ucode out of physical control
; memory.  If referenced, it will simply be paged back in.

;If a cold boot, warm boot, or disk save begins to happen,
; turn-pagable-ucode-off-and-restore-initialization is called.

(def-data-field oal-cram-page-number 12. 18.)

(LOCALITY A-MEM)

a-micro-paging-cram-base ((ceiling 30000 20))  ;base of potentially pagable physical cmem.
;moved to counter block in UC-PARAMETERS
;a-highest-handcode-page  ((ceiling  (I-MEM-LOC end-wired-ucode) 20))
;a-highest-kernal-ucode-page  ((ceiling (i-mem-loc highest-kernal-ucode-location) 20))

a-prom-low-page-number  (1700)          ;36000
a-prom-high-page-number (2000)          ;40000
a-cram-paging-scan-pointer (0)          ;points to A-MEM location in inverse map to consider
                                        ; next for swapout.  (table based at 2400@a).
a-prom-flushed (0)      ;-1 means prom has been wiped.

(locality d-mem)
        (start-dispatch 0 0)
d-popj
        (r-bit inhibit-xct-next-bit)
(end-dispatch)

(LOCALITY I-MEM)

;note micro-fault must be transparent to DISPATCH-CONSTANT register.
micro-fault
        ((m-lam) a-disk-switches)
        (call-if-bit-clear m-lam (byte-field 1 10.) illop)
        ((a-micro-fault-dc) read-i-arg)
  (declare (saves (a-1 a-micro-fault-m-1) (a-2 a-micro-fault-m-2) (a-3 a-micro-fault-m-3)
                  (vma a-micro-fault-vma) (md a-micro-fault-md) (pi a-micro-fault-pi)
                  (a-tem a-micro-fault-tem)))
        ((a-micro-fault-m-1) m-1)
        ((a-micro-fault-m-2) m-2)
        ((a-micro-fault-m-3) m-3)
        ((a-micro-fault-vma) vma)
        ((a-micro-fault-md) md)
        ((a-micro-fault-pi) pdl-index)
        ((a-micro-fault-tem) m-tem)
        ((m-3) micro-stack-data-pop)
        ((m-3) sub m-3 (a-constant 1))  ;back up PC
        ((micro-stack-data-push) m-3)
        ((m-3) (byte-field 12. 4) m-3)  ;get page number
        (call micro-findpage)   ;return page found in m-2
        ((m-2) add m-2 a-micro-paging-cram-base) ;page number was relative to micro pagable pages
  ;load micro-page in m-3 into physical control memory page in m-2.
micro-page-swap-in      ;enter here from turn-pagable-ucode-off-and-restore-initialization.
                        ;note lots of registers (which werent saved) get clobbered.
        ((m-1) ldb (byte-field 4 4) m-2)        ;compute odd parity of M-2 and put it in bit 11.
        ((m-1) xor m-1 a-2)
        ((m-tem) ldb (byte-field 2 8) m-2 (a-constant 4))  ;this bit makes the parity odd.
        ((m-1) xor m-1 a-tem)
        ((m-tem) ldb (byte-field 2 2) m-1)
        ((m-1) xor m-1 a-tem)
        ((m-tem) ldb (byte-field 1 1) m-1)
        ((m-1) xor m-1 a-tem)
        ((m-2) dpb m-1 (byte-field 1 11.) a-2)  ;put parity in bit 11.
        ((oa-reg-low) dpb m-3 oal-cram-page-number a-zero)  ;m-3 has micro-page to swap in.
        (call-xct-next)
       (popj-after-next (cram-adr-map) m-2)
load-micro-page
        ((m-2) (a-constant 17))
        ((m-1) dpb m-3 (byte-field 12. 4) a-zero)       ;get virtual control-mem location
load-mp-1
        ((vma) m+a+1 m-1 a-v-micro-code-paging-area)
        ((vma-start-read) add vma a-1)          ;2 memory wds per control mem location
        (check-page-read)       ;write high word first ...
        ((oa-reg-low) dpb m-1 oal-jump a-zero)
        (call-xct-next)
       (popj-after-next (cram-high) md)
        ((vma-start-read) sub vma (a-constant 1)) ;now for the low word.
        (check-page-read)
        ((oa-reg-low) dpb m-1 oal-jump a-zero)
        (call-xct-next)
       (popj-after-next (cram-low) md)
        (jump-equal m-2 a-zero micro-fault-exit)
        ((m-2) sub m-2 (a-constant 1))
        (jump-xct-next load-mp-1)
       ((m-1) add m-1 (a-constant 1))

micro-fault-exit
        ((a-count-micro-faults) m+a+1 m-zero a-count-micro-faults)
(declare (restores (a-1 a-micro-fault-m-1) (a-2 a-micro-fault-m-2) (a-3 a-micro-fault-m-3)
                   (vma a-micro-fault-vma) (md a-micro-fault-md) (pi a-micro-fault-pi)
                   (a-tem a-micro-fault-tem)))
        ((m-tem) a-micro-fault-tem)
        ((pdl-index) a-micro-fault-pi)
        ((m-1) a-micro-fault-m-1)
        ((m-2) a-micro-fault-m-2)
        ((m-3) a-micro-fault-m-3)
        ((vma) a-micro-fault-vma)
        ((md) a-micro-fault-md)
        ((m-lam) a-micro-fault-dc)
        ((oa-reg-low) dpb m-lam oal-disp-dispatch-constant a-zero)
        (dispatch d-popj)
   ;;** can speed up by using this xct next cycle for one of above

micro-findpage ;m-2 returns physical page found.  m-3 has page number which will get swapped in.
        ((m-2) dpb m-zero (byte-field 24. 8) a-cram-paging-scan-pointer)  ;ring pointer.
        ((a-cram-paging-scan-pointer) add m-2 (a-constant 1))  ;above inst. rings it.
        ((oa-reg-high) dpb m-2 oah-a-src-8-bits (a-constant (byte-value oah-a-src 2400)))
        ((m-1) seta a-garbage)
        (jump-less-than m-1 a-zero micro-findpage)  ;this page not really available.
        (call-not-equal m-1 a-zero micro-mark-out)  ;flush guy currently using this page
        (popj-after-next
         (oa-reg-low) dpb m-2 oal-a-dest-8-bits (a-constant (byte-value oal-a-dest 2400)))
       ((a-garbage) m-3)        ;store who is using this page now.

micro-mark-out
        ((oa-reg-low) dpb m-1 oal-cram-page-number a-zero)
        (call-xct-next)
       (popj-after-next (cram-adr-map) (a-constant 1))
        (popj)

micro-paging-reset   ;bit 1.1 of m-t -> flush prom.
        ((m-1) a-micro-paging-cram-base)
        ((m-a) (a-constant 2400))       ;a-mem base of inverse map.
micro-rs-0
        ((m-b) (a-constant -1))
        (jump-less-than m-1 a-highest-handcode-page micro-rs-1)  ;hand code.
        (jump-if-bit-set (byte-field 1 1) m-t micro-rs-2)  ;jump if flush prom.
        (jump-less-than m-1 a-prom-low-page-number micro-rs-2)
        (jump-less-than m-1 a-prom-high-page-number micro-rs-1)
micro-rs-2
        ((m-b) (a-constant 0))
        (call micro-mark-out)
        (jump-greater-or-equal m-1 (a-constant 2000) micro-rs-4)  ;above physical region.
micro-rs-1
        ((oa-reg-low) dpb m-a oal-a-dest a-zero)
        ((a-garbage) m-b)
micro-rs-4
        ((m-1) add m-1 (a-constant 1))
        (jump-less-than-xct-next m-1 (a-constant 7777) micro-rs-0)
       ((m-a) add m-a (a-constant 1))
        ((a-cram-paging-scan-pointer) setz)
        (popj-after-next popj-if-bit-clear (byte-field 1 1) m-t)
       ((a-prom-flushed) seto)  ;record PROM is no more.

#-lambda (end-comment)

xmicro-paging   (misc-inst-entry %micro-paging)
  ;arg is interpreted bitwise:
  ; bit 0 -> reset
  ; bit 1 -> when you reset, clobber PROM.
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
#+lambda(call-if-bit-set (byte-field 1 0) m-t micro-paging-reset)
        (popj)

#-lambda (begin-comment)
turn-pagable-ucode-on-and-evict-initialization
         (call-xct-next micro-paging-reset)
        ((m-t) (a-constant 1))          ;dont clobber PROM.
         ((a-disk-switches) dpb m-minus-one (byte-field 1 10.) a-disk-switches)
         (popj)

turn-pagable-ucode-off-and-restore-initialization
        (call-xct-next micro-paging-reset)
        ((m-t) (a-constant 1))          ;dont clobber PROM.
         ((a-disk-switches) dpb m-zero (byte-field 1 10.) a-disk-switches) ;turn off pagable ucode
        ((m-q) add m-minus-one a-highest-handcode-page)
xm-ri-loop
        ((m-3) m-q)
        (call-xct-next micro-page-swap-in)
       ((m-2) m-q)
        ((m-q) m+1 m-q)
        (jump-less-than m-q a-highest-kernal-ucode-page xm-ri-loop)
        (popj)
#-lambda (end-comment)

; (%processor-switches nil) => current value of a-processor-switches (maybe as a bignum)
; (%processor-switches n) sets a-processor-switches to the number n (also maybe a bignum)
;    then some code is run to activate whatever new special features are specified

; the variable a-processor-switches either starts as 0 (if booted by old code) or
; with values passed in from the booting agent

xprocessor-switches (misc-inst-entry %processor-switches)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer)
        (jump-equal m-t a-v-nil xps-1)
        (call get-32-bits)
        ((a-processor-switches) m-1)

activate-processor-switches ;reset-machine calls this
        ((m-1) a-processor-switches)
                ;store away cache.permit and packet.size.code.
#+lambda((a-l2-map-control-bits) dpb m-1 (byte-field 2 10.) a-l2-map-control-bits)
#+lambda((m-2) ldb (byte-field 1 2) m-1)
#+lambda((a-l2-map-control-bits) dpb m-2 (byte-field 1 14.) a-l2-map-control-bits)
#+lambda(call-if-bit-set (lisp-byte %%processor-switch-use-stat2-for-usec-clock)
                         m-1 enable-stat2-clock)
        (popj-after-next (m-t) a-v-nil)
       (no-op)

xps-1   (jump-xct-next return-m-1-unsigned)
       ((m-1) seta a-processor-switches c-pdl-buffer-pointer-pop)

#-lambda (begin-comment)
enable-stat2-clock
        ((m-2) rg-mode)         ;set AUX-STAT counter to be usec clock.
        (popj-after-next (m-3) (a-constant 6))
       ((rg-mode) dpb m-3 (byte-field 3 0) a-2)
#-lambda (end-comment)

xcold-boot (misc-inst-entry %cold-boot)
kbd-boot-char-c-m-c-m-rubout
    (declare (suspend-flow-tracing))
#-lambda (begin-comment)
        ;;give ourselves 15 seconds to go to the prom and get back to system ucode
        ((a-initial-watchdog) (a-constant (eval (* 50. 15.))))
        (call reset-watchdog)
        ;;may have gotten here from interrupt routine - but we aren't going back
        ((M-INTERRUPT-FLAG) DPB m-zero A-FLAGS)
        (call turn-pagable-ucode-off-and-restore-initialization)
        ((q-r) a-proc-conf-local-phys-adr)
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                           m-tem xcold-boot-1)
        ((md) setz)
        ((vma) a-my-iopb-valid-flag-physical-adr)
        (call new-cold-nubus-write) ;for now, this is in wired ucode memory

xcold-boot-1
        (jump-equal m-zero a-prom-flushed prom-36001)
        ;if the prom is not there anymore, hope the sdu will be there to boot us
        ((md) (a-constant (eval %proc-conf-boot-command-boot)))
        ((vma) a-proc-conf-virtual-adr)
        ((vma-start-write) add vma (a-constant (eval %processor-conf-boot-command)))
        (check-page-write-no-interrupt)         ;may reload map, but won't do
                                                ;any memory cycles
        (call sdu-halt)
#-lambda (end-comment)
#-exp (begin-comment)
        (call illop)
#-exp (end-comment)

;; flush cache by reading low four pages of memory
;; a-pmo-0, a-pmh-0 have to already be set up, and the low four pages
;; have to have good parity
;; clobbers only m-tem1, m-tem2

#-lambda (begin-comment)
flush-cache   ;currently called only from initialization, so could be moved to UC-INITIALIZATION
        ((m-tem1) vma)
        ((m-tem2) md)
        ((md) a-map-scratch-block)
        ((l2-map-control) (a-constant 1460)) ;word ops, cache off
        ((l2-map-physical-page) a-pmo-0)
        ((l2-map-physical-page) add l2-map-physical-page (a-constant 16.))
flush-cache-outer-loop
        ((l2-map-physical-page) sub l2-map-physical-page (a-constant 1))
        ((vma) (a-constant 400))
flush-cache-inner-loop
        ((vma-start-read) sub vma (a-constant 1))
        (illop-if-page-fault)
        ((md) setz)
        (jump-not-equal vma a-zero flush-cache-inner-loop)
        (jump-not-equal l2-map-physical-page a-pmo-0 flush-cache-outer-loop)

        ((vma) m-tem1)
        ((md) m-tem2)
        (popj)
#-lambda (end-comment)

(begin-pagable-ucode)
;; %mult-16 should take two operands, check to see that they are 16 bits or less,
;; then do an unsigned multiply using the 16x16 matrix multiplier on the rg board
;; if the arguments are not 16 bit fixnums, we want to get an error
;;
;; get-32-bits already checks for data type, allows only bignum or fixnum through
;; if its a bignum, we get the low 32 bits.  Considering that this is only a test
;; function, how careful do we want to be? ...this should be ok for the moment
#-lambda (begin-comment)
multiply-16 (misc-inst-entry %mult-16)
        (call trap)
     (error-table illegal-instruction)
;       (call get-32-bits)                      ;pops pdl, result in m-1
;       ((m-t) dpb m-1 (byte-field 16. 16.) a-zero)     ;put low bits in high half of m-t
;       (call get-32-bits)                      ;get the second operand
;       ((multiplier) dpb m-1 (byte-field 16. 0.) a-t)
;                                               ;put the second operand in the low
;                                               ;bits, the first in the high bits
;                                               ;and deposit it in the multiplier
;       ((multiplier) setz)                     ;RATS! extra uinst to clock the multiplier
;                                               ;because source-ft seems not to work
;       (jump-xct-next return-m-1-unsigned)     ;return a 32 bit unsigned quantity
;      ((m-1) multiplier)
;;
;; %mult-32 is an attempt to implement signed 32 bit number multiply

multiply-32 (misc-inst-entry %mult-32)
        (call trap)
     (error-table illegal-instruction)
;       (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
;       ((m-2) output-selector-extend-25 c-pdl-buffer-pointer-pop)
;       (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
;       ((m-1) output-selector-extend-25 c-pdl-buffer-pointer-pop)
;       ((multiplier) dpb (byte-field 20 20) m-1 a-2)   ;two low halves to multiplier
;       ((multiplier) ldb (byte-field 20 20) m-1 a-2 X-MULT-TC Y-MULT-TC)  ;two high to mult
;       ((m-t) multiplier)                      ;parial low result now ready
;       ((multiplier) ldb (byte-field 20 0) m-1 a-2 X-MULT-TC)  ;low m-1 high m-2
;        ((m-3) multiplier)                     ;partial high result now ready
;       ((multiplier) ldb (byte-field 20 0) a-1 M-2 X-MULT-TC)  ;low m-2 high m-1
;       ((m-tem multiplier) multiplier)         ;partial medium result now ready, push pipe
;       ((m-4) add multiplier a-tem)            ;full medium result
;       ((m-tem) add output-selector-rightshift-1 multiplier a-tem)
;                                                ;get sign and carry of medium result
;       ((m-lam) dpb (byte-field 20 20) m-4 a-zero)  ;low half of medium result to high m-lam.
;       ((m-4) ldb (byte-field 20 20) m-4 a-zero)
;       ((oa-reg-high) ldb (byte-field 1 31.) m-tem)
;       ((m-4) selective-deposit m-zero (byte-field 20 20) a-4)  ;get sign extend high half
;                                       ;of medium result in low m-4.
;        ((m-1 q-r) add m-t a-lam)
;       (jump-if-bit-clear (byte-field 1 6) dp-mode nocarry)
;       ((m-3) m+1 m-3)
;nocarry
;        ((m-2) add m-4 a-3)    ;m-2 has high part, m-1 and q-r low part.
;       ((M-TEM) SELECTIVE-DEPOSIT Q-R
;        (BYTE-FIELD (DIFFERENCE 33. Q-POINTER-WIDTH)
;                    (DIFFERENCE Q-POINTER-WIDTH 1))
;        A-2)   ;DISCARDED BITS AND SIGN
;       (JUMP-EQUAL-XCT-NEXT M-TEM A-ZERO FIXPACK-T)   ;JUMP IF NON-OVERFLOWING POSITIVE RESULT
;       ((M-1) Q-POINTER Q-R A-TEM)                  ;SIGN EXTEND (IF NON-OVERFLOWING)
;       (JUMP-EQUAL M-TEM (A-CONSTANT -1) FIXPACK-T)   ;JUMP IF NON-OVERFLOWING NEGATIVE
;       (JUMP-XCT-NEXT FIX-2-WORD-OVERFLOW-TO-M-T)
;       ((M-1) Q-R)

;; Quarter-inch tape support.

xquart  (misc-inst-entry %quart-transfer)       ;quart-flags array n-blocks
        ;quart-flags bit0 0-> read, 1-> write
        ;value is number blocks transferred
        ((m-1) q-pointer pdl-pop)       ;count to xfer.
        (call-xct-next gahdr)           ;First data word returns in M-E.
       ((m-a) invalidate-array-cache c-pdl-buffer-pointer-pop)
        (call store-array-registers-in-accumulators)
        ((m-2) q-pointer pdl-pop)
        ((m-k) (a-constant (byte-value q-data-type dtp-fix)))  ;blocks xferred.
        (jump-if-bit-clear (byte-field 1 0) m-2 xquart-read)
xqrt-w1 (call xquart-wait-for-ready)
        (jump-if-bit-set (byte-field 1 7) m-t xqrt-return)      ;exception.
        ((m-3) (a-constant 127.))       ;xfer 128. words
xqrt-w2 ((vma-start-read) m-e)
        (check-page-read)
        (call-xct-next xqrt-write-data)
       ((m-4) md)
        (call-xct-next xqrt-write-data)
       ((md) ldb (byte-field 8 8) m-4)
        (call-xct-next xqrt-write-data)
       ((md) ldb (byte-field 8 16.) m-4)
        (call-xct-next xqrt-write-data)
       ((md) ldb (byte-field 8 24.) m-4)
        ((m-e) add m-e (a-constant 1))
        (jump-greater-than-xct-next m-3 a-zero xqrt-w2)
       ((m-3) sub m-3 (a-constant 1))
        ((m-k) add m-k (a-constant 1))
        ((m-1) sub m-1 (a-constant 1))
        (jump-greater-than m-1 a-zero xqrt-w1)
xqrt-return
        ((m-t) m-k)
        (popj)

xqrt-write-data
        ((pdl-push) (a-constant (plus (byte-value q-data-type dtp-fix)
                                      #x1c400)))
        ((pdl-push) dpb md q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (jump xmultibus-write-8)    ;no xct-next to avoid pdl-pass lossage on old boards.

xquart-wait-for-ready
        ((pdl-push) (a-constant (plus (byte-value q-data-type dtp-fix)
                                     #x1c088)))
        (call xmultibus-read-8)
        (popj-if-bit-set (byte-field 1 7) m-t)  ;exception
        (popj-if-bit-set (byte-field 1 0) m-t)  ;ready
        (jump xquart-wait-for-ready)

xquart-read
xqrt-r1 (call xquart-wait-for-ready)
        (jump-if-bit-set (byte-field 1 7) m-t xqrt-return)
        ((m-3) (a-constant 127.))
xqrt-r2 (call xqrt-read-data)
        (call-xct-next xqrt-read-data)
       ((m-4) dpb m-t (byte-field 8 0) a-zero)
        (call-xct-next xqrt-read-data)
       ((m-4) dpb m-t (byte-field 8 8) a-4)
        (call-xct-next xqrt-read-data)
       ((m-4) dpb m-t (byte-field 8 16.) a-4)
        ((md) dpb m-t (byte-field 8 24.) a-4)
        ((vma-start-write) m-e)
        (check-page-write-no-interrupt) ;avoid double write.
        ((m-e) add m-e (a-constant 1))
        (jump-greater-than-xct-next m-3 a-zero xqrt-r2)
       ((m-3) sub m-3 (a-constant 1))
        ((m-k) add m-k (a-constant 1))
        ((m-1) sub m-1 (a-constant 1))
        (jump-greater-than m-1 a-zero xqrt-r1)
        (jump xqrt-return)

xqrt-read-data
        ((pdl-push) (a-constant (plus (byte-value q-data-type dtp-fix)
                                      #x1c400)))
        (jump xmultibus-read-8)

;;
#-lambda (end-comment)

xrg-quad-slot  (misc-inst-entry %lambda-rg-quad-slot)
        (popj-after-next (m-1) a-rg-quad-slot)
       ((m-t) dpb m-1 q-pointer (a-constant (byte-value q-data-type dtp-fix)))

xtv-quad-slot  (misc-inst-entry %lambda-tv-quad-slot)
        (popj-after-next (m-1) a-tv-quad-slot)
       ((m-t) dpb m-1 q-pointer (a-constant (byte-value q-data-type dtp-fix)))

xsdu-quad-slot (misc-inst-entry %lambda-sdu-quad-slot)
        (popj-after-next (m-1) a-sdu-quad-slot)
       ((m-t) dpb m-1 q-pointer (a-constant (byte-value q-data-type dtp-fix)))

x-mouse-buttons (misc-inst-entry %lambda-mouse-buttons)
        (popj-after-next (m-1) #+lambda a-mouse-fake-register #+exp a-mouse-last-h1)
       ((m-t) ldb m-1 (byte-field 3 14) (a-constant (byte-value q-data-type dtp-fix)))

#-lambda (begin-comment)

;(%stat-counter op-code value)
; op-code is
;  0  read main stat counter
;  1  write main stat counter
;  2  read aux stat counter
;  3  write aux stat counter
;  4  read rg mode
;  5  write rg mode stat counter bits
; the value should be 0 if it is not used
xstat-counter (misc-inst-entry %stat-counter)
        (call get-32-bits)                      ;result in M-1
        ((m-a) q-pointer c-pdl-buffer-pointer-pop)
        (jump-equal m-a (a-constant 0) x-read-main-stat-counter)
        (jump-equal m-a (a-constant 1) x-write-main-stat-counter)
        (jump-equal m-a (a-constant 2) x-read-aux-stat-counter)
        (jump-equal m-a (a-constant 3) x-write-aux-stat-counter)
        (jump-equal m-a (a-constant 4) x-stat-counter-read-rg-mode)
        (jump-equal m-a (a-constant 5) x-stat-counter-write-control)
        (jump xfalse)

x-read-main-stat-counter
        (jump-xct-next return-m-1)
       ((m-1) stat-counter)

x-write-main-stat-counter
        (popj-after-next (stat-counter) m-1)
       (no-op)

x-read-aux-stat-counter
        (jump-xct-next return-m-1)
       ((m-1) stat-counter-aux)

x-write-aux-stat-counter
        (popj-after-next (stat-counter-aux) m-1)
       (no-op)

x-stat-counter-read-rg-mode
        (jump-xct-next return-m-1)
       ((m-1) rg-mode)

x-stat-counter-write-control
        ((m-1) and m-1     (a-constant   134000027)) ;just the stat counter bits
        (popj-after-next
         (m-2) and rg-mode (a-constant 37643777750)) ;all the bits except stat counter
       ((rg-mode) ior m-1 a-2)

(end-pagable-ucode)

#-lambda (end-comment)

#-lambda(begin-comment)
;called from keyboard hardware interrupt routine
kbd-boot-char-c-m-c-m-end
        ((md) (a-constant (eval %proc-conf-boot-command-menu)))
        ((vma) a-proc-conf-virtual-adr)
        ((vma-start-write) add vma (a-constant (eval %processor-conf-boot-command)))
        (check-page-write-no-interrupt)
        (call sdu-halt)
        (popj)

kbd-boot-char-c-m-c-m-line
        (popj-after-next (a-defer-boot-char-mode) (a-constant 2))
       (no-op)

kbd-boot-char-c-m-c-m-return
        (popj-after-next (a-defer-boot-char-mode) (a-constant 1))
       (no-op)

#-lambda(end-comment)

;called from SBSER before taking a sequence break
kbd-boot-char-xct-now
        ((md) a-defer-boot-char-mode)
        ((a-defer-boot-char-mode) setz)
        (jump-equal md (a-constant 1) warm-boot)
#+lambda(jump kbd-boot-char-c-m-c-m-end)
#+exp   (call illop)

#-lambda (begin-comment)
        (misc-inst-entry %multibus-blt-16)
x-multibus-blt-16
        (call get-32-bits)
        ((m-2) m-1)                             ;multibus address
        ((m-a) ldb (byte-field (difference q-pointer-width 1) 1) pdl-top)       ;skip-in-words
        ((m-b) ldb (byte-field 1 0) pdl-pop)    ;skip-phase
        ((m-c) q-pointer pdl-pop)               ;rows-to-go
        ((m-d) q-pointer pdl-pop)               ;width-in-16s
        ((m-e) q-pointer pdl-pop)               ;phase
        ((m-r) q-pointer pdl-pop)               ;data-pointer
        ((m-garbage) q-pointer pdl-pop)         ;alu

        (call multibus-blt-map-setup)

multibus-blt-outer-loop
        ((m-q) m-d)                             ;columns to go

        ;;do first halfword
        (jump-equal m-e a-zero multibus-blt-inner-loop)
        ((vma-start-read) m-r)
        (call-if-page-fault-or-interrupt multibus-blt-page-fault-recover)
        ((m-tem) md)
        ((md) ldb m-tem (byte-field 16. 16.) a-tem)
        ((vma-start-write) m-s)
        (illop-if-page-fault)
        ((m-r) add m-r (a-constant 1))
        ((m-q) sub m-q (a-constant 1))
        (jump-less-or-equal m-q (a-constant 1) multibus-blt-check-for-one-more)

multibus-blt-inner-loop
;       ((vma-start-read) m-r)
;       (call-if-page-fault-or-interrupt multibus-blt-page-fault-recover)
;       ((m-tem) md)
;       ((md) dpb md (byte-field 16. 16.) a-tem)
;       ((vma-start-write) m-s)
;       (illop-if-page-fault)
;       ((md-start-write) ldb (byte-field 16. 16.) m-tem a-tem)
;       (illop-if-page-fault)
;       ((m-r) add m-r (a-constant 1))
;       ((m-q) sub m-q (a-constant 2))
;       (jump-greater-than m-q (a-constant 1) multibus-blt-inner-loop)

;new optimized code
        ((vma-start-read) m-r)
        (call-if-page-fault-or-interrupt multibus-blt-page-fault-recover)
        ((m-tem) md)
        ((md) dpb md (byte-field 16. 16.) a-tem)
        ((vma-start-write) m-s)
        (illop-if-page-fault)
        ((m-q) sub m-q (a-constant 2))
        ((md-start-write) ldb (byte-field 16. 16.) m-tem a-tem)
        (illop-if-page-fault)
        (jump-greater-than-xct-next m-q (a-constant 1) multibus-blt-inner-loop)
       ((m-r) add m-r (a-constant 1))

multibus-blt-check-for-one-more
        (jump-equal m-q a-zero multibus-blt-next-row)
        ((vma-start-read) m-r)
        (call-if-page-fault-or-interrupt multibus-blt-page-fault-recover)
        ((m-tem) md)
        ((md) dpb md (byte-field 16. 16.) a-tem)
        ((vma-start-write) m-s)
        (illop-if-page-fault)
multibus-blt-next-row
        ((m-r) add m-r a-a)
        ((m-e) m-b)                             ;m-phase gets m-skip-phase
        ((m-c) sub m-c (a-constant 1))          ;rows to go minus one
        (jump-greater-than m-c a-zero multibus-blt-outer-loop)

        (jump xfalse)


multibus-blt-page-fault-recover
        (check-page-read)
        ((a-tem1) md)
multibus-blt-map-setup
        ((md) a-map-scratch-block)
        ((m-tem) a-sdu-quad-slot)
        ((l2-map-control) (a-constant 1460)) ;normal word r/w
        ((m-3) dpb m-tem (byte-field 8 14.) a-zero)
        ((m-3) dpb m-2 (byte-field 2 22.) a-3)  ;get bit 1 of adr into map.phys.1
        ((m-3) dpb m-minus-one (byte-field 1 22.) a-3)  ;turn on map.phys.0
        ((l2-map-physical-page) ldb m-2 (byte-field 13. 10.) a-3)
        ((m-s) ldb (byte-field 8 2) m-2 a-map-scratch-block)
        ((md) a-tem1)
        (popj)
#-lambda (end-comment)

#-exp (begin-comment)
;;; Code for NVRAM crash records.

(LOCALITY A-MEM)
a-crash-record-pointer  (0)
(LOCALITY I-MEM)

;;; call very early in boot process
;;; allocate a crash record to this boot.
;;; the crash record pointer always points to a
;;; a free crash record. So, save the current
;;; crash record pointer and increment it to point
;;; to the next free crash record.
setup-crash-record-pointer
                ; read crash record pointer
        (Call-xct-next Read-NVRAM-16)
       ((vma) (a-constant 144.))
        ((a-crash-record-pointer) m-1)
                ; read last crash record pointer
        (Call-xct-next Read-NVRAM-16)
       ((vma) (a-constant 160.))
        (jump-equal m-1 a-crash-record-pointer set-crash-record-pointer)
                ; increment crash record pointer and write to NVRAM
        (Call-xct-next Read-NVRAM-16)
       ((vma) (a-constant 152.))
        (jump-xct-next write-crash-record-pointer)
       ((md) add m-1 a-crash-record-pointer)
                ; set crash record pointer to first record
set-crash-record-pointer
        (Call-xct-Next Read-NVRAM-16)
       ((vma) (a-constant 168.))
        ((md) m-1)
write-crash-record-pointer
        (Call-xct-next Write-NVRAM-16)
       ((vma) (a-constant 144.))
                ; set boot progress (crash record allocated)
        ((md) (a-constant 5))
        (Call-xct-Next Write-CR-8)
       ((vma) (a-constant 0))
        (popj)

;;; called when the load band and ucode band are known.
Add-Boot-Info-to-Crash-Record
                ; write ucode unit into Crash Record
        ((md) a-disk-ucode-partition-unit)
        (Call-Xct-Next Write-CR-8)
       ((vma) (a-constant 8.))
                ; write ucode band into Crash Record
        ((md) (a-constant 115))         ; "M"
        ((md) dpb (byte-field 8 30) md a-loaded-ucode)
        (Call-Xct-Next Write-CR-32)
       ((vma) (a-constant 16.))
                ; write load unit into Crash Record
        ((md) a-disk-lod-partition-unit)
        (Call-Xct-Next Write-CR-8)
       ((vma) (a-constant 12.))
                ; write load band into Crash Record
        ((md) (a-constant 114))         ; "L"
        ((md) dpb (byte-field 8 30) md a-loaded-band)
        (Call-Xct-Next Write-CR-32)
       ((vma) (a-constant 32.))
        (popj)


;;; *jump* here from illop to save crash record info
;;; probably want test at illop to not call here for
;;; debugging purposes as this code trashes some registers

;;; Things yet to be saved
;;; CONTROLLER 4
;;; Ucode-Version 48.
;;; Report-Flags 112.
;;; M-FEF 192.

write-crash-record
                ; save registers
        ((m-a) md)
        ((m-b) vma)
                ; save m-1, this must be first
                ; because Write-CR-32 trashes M-1
        ((md) m-1)
        (call-xct-next Write-CR-32)
       ((vma) (a-constant 128.))
                ; save m-2
        ((md) m-2)
        (call-xct-next Write-CR-32)
       ((vma) (a-constant 144.))
                ; save MD
        ((md) m-a)
        (call-xct-next Write-CR-32)
       ((vma) (a-constant 160.))
                ; save VMA
        ((md) m-b)
        (call-xct-next Write-CR-32)
       ((vma) (a-constant 176.))
                ; save HALT-KIND (Ucode Halt)
        ((md) (a-constant 1))
        (call-xct-next Write-CR-8)
        ((vma) (a-constant 124.))
                ; save UPC Stack - Halt Address
        ((md) micro-stack-data-pop)
        (call-xct-next Write-CR-16)
       ((vma) (a-constant 116.))
                ; save UPC Stack - UPCSTK-0
        ((md) micro-stack-data-pop)
        (call-xct-next Write-CR-16)
       ((vma) (a-constant 192.))
                ; save UPC Stack - UPCSTK-1
        ((md) micro-stack-data-pop)
        (call-xct-next Write-CR-16)
       ((vma) (a-constant 200.))
                ; save UPC Stack - UPCSTK-2
        ((md) micro-stack-data-pop)
        (call-xct-next Write-CR-16)
       ((vma) (a-constant 208.))
                ; save UPC Stack - UPCSTK-3
        ((md) micro-stack-data-pop)
        (call-xct-next Write-CR-16)
       ((vma) (a-constant 216.))
                ; halt machine
        (no-op halt-cons)


; read a 16 bit number from the nvram
; vma has the offset of the first byte (low byte)
; result in m-1, trashes md and m-tem
Read-NVRAM-16
        (call-xct-next nubus-read)
       ((vma) add vma (a-constant #xF5FA0000))
        ((m-1) ldb md (byte-field 10 0))
        (call-xct-next nubus-read)
       ((vma) add vma (a-constant 4))
        (popj-xct-next)
       ((m-1) dpb md (byte-field 10 10) a-1)

; write a 32 bit number to the crash record
; vma has the offset of the first byte (low byte)
; md has the value, trashes m-tem and m-1
Write-CR-32
        ((vma) add vma a-crash-record-pointer)
                ; drop through to Write-NVRAM-32

; write a 32 bit number to the nvram
; vma has the offset of the first byte (low byte)
; md has the value, trashes m-tem and m-1
Write-NVRAM-32
        ((m-1) md)
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant #xF5FA0000))
        ((md) ldb (byte-field 10 10) m-1)
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant 4))
        ((md) ldb (byte-field 10 20) m-1)
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant 4))
        ((md) ldb (byte-field 10 30) m-1)
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant 4))
        (popj)

; write a 16 bit number to the crash record
; vma has the offset of the first byte (low byte)
; md has the value, trashes m-tem and m-1
Write-CR-16
        ((vma) add vma a-crash-record-pointer)
                ; drop through to Write-NVRAM-16

; write a 16 bit number to the nvram
; vma has the offset of the first byte (low byte)
; md has the value, trashes m-tem and m-1
Write-NVRAM-16
        ((m-1) md)
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant #xF5FA0000))
        ((md) ldb (byte-field 10 10) m-1)
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant 4))
        (popj)

; write an 8 bit number to the crash record
; vma has the offset of the byte
; md has the value, trashes m-tem
Write-CR-8
        ((vma) add vma a-crash-record-pointer)
                ; drop through to Write-NVRAM-8

; write an 8 bit number to the nvram
; vma has the offset of the byte
; md has the value, trashes m-tem
Write-NVRAM-8
        (call-xct-next nubus-write)
       ((vma) add vma (a-constant #xF5FA0000))
        (popj)

#-exp (end-comment)
))

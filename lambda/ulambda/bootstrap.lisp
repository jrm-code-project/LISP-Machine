;;; LAMBDA pseudo-PROM Microcode.                       -*- Mode:Text -*-

;;;Copyright (c) LISP Machine, Inc. 1984
;;;   See filename "Copyright" for
;;;licensing and release information.

;;; to do
;;; *** things that think the system configuration area is 1 page

;;; There are many places in this code in which cycles are wasted.
;;; This is to make the code clearer, since it is far more important
;;; that code in a PROM be bug-free than that the boot sequence be
;;; 150 nanoseconds faster.

;;; Still to be done is the feature for loading microcode off the Chaos net.

;;;used to be PROM-DIRECT.TEXT  - this version started 4/29/84

;;;to do
;;;   check the version number of the system config structure before using
;;;     data from it to clear memories

(SETQ bootstrap '(

(LOCALITY M-MEM)

M-GARBAGE       (0)
M-HUNOZ         (0)
M-ZERO          (0)
M-ONES          (0)

M-A             (0)
M-B             (0)
M-C             (0)
M-D             (0)
M-FROM          (0)
M-TO            (0)

M-TEMP-1        (0)
M-TEMP-2        (0)

M-MICR-OFFSET   (0)
M-SAVE-A        (0)
M-DISK-RETURN-CODE (0)
M-TEM           (0)

m-sys-conf-virtual-adr  (0)
m-proc-conf-virtual-adr (0)
m-proc-switches (0)
m-share-struct-virtual-adr (0)

m-nubus-address (0)

m-convert-phys-adr (0) ;this is set to 0 if lambda is on same bus as SDU,
        ;or #x10000000 if on the opposite bus.
        ;therefore to convert from a local physical adr to an sdu-physical-adr,
        ;xor by this variable

(LOCALITY A-MEM)

A-GARBAGE       (0)
A-HUNOZ         (0)
A-ZERO          (0)
A-ONES          (0)

A-A             (0)
A-B             (0)
A-C             (0)
A-D             (0)
A-FROM          (0)
A-TO            (0)

A-TEMP-1        (0)
A-TEMP-2        (0)

A-MICR-OFFSET   (0)
A-SAVE-A        (0)
A-DISK-RETURN-CODE (0)
A-TEM           (0)

a-sys-conf-virtual-adr  (0)
a-proc-conf-virtual-adr (0)
a-proc-switches (0)
a-share-struct-virtual-adr (0)

a-nubus-address (0)

a-convert-phys-adr (0)

(LOC 40)

;;; Constants.
A-1             (0)
A-2             (0)
A-3             (0)
A-4             (0)
A-5             (0)
A-6             (0)
A-11            (0)
A-20            (0)
A-26            (0)
A-31            (0)
A-40            (0)
A-140           (0)
A-200           (0)
A-202           (0)
A-340           (0)
A-400           (0)
A-1000          (0)
A-1400          (0)
a-1576          (0)
A-2400          (0)
A-3777          (0)
A-10400         (0)
A-20000         (0)
A-36000         (0)
a-1374000       (0)
a-7             (0)
a-3000          (0)
a-3177          (0)
a-3400          (0)
a-5400          (0)
a-17740160      (0)
a-1464          (0)
a-1363200       (0)
a-5000          (0)

a-save-b        (0)

a-%processor-conf-share-runme (0)

a-proc-conf-nubus-adr (0)
a-max-iopbs (0)

;;; Disk Characteristics (from label)
A-NCYLS         (0)     ;Total number of cylinders
A-NHEADS        (0)     ;Number of data heads (i.e. tracks per cylinder)
A-NBLKS         (0)     ;Number of blocks ("sectors") per track
A-HEADS-TIMES-BLOCKS    (0)     ;Number of blocks per cylinder
A-MICR-ORIGIN   (0)     ;Base disk address of microcode partition
A-MICR-ADDRESS  (0)     ;Address of next block of microcode partition
A-MICR-N-BLOCKS (0)     ;Remaining blocks of microcode partition
A-TEM1          (0)     ;Temporary used in division

A-DISK-REGS     (0)     ;Virtual address of disk control registers
                        ; (points to multibus io space, via page set up for byte xfers!)
  ;this assumes eagle is already initialized (UIB hacked, etc).

a-base-physical-page (0) ;NUBUS page number of main memory locn "0"
a-disk-cylinder-offset (0)  ;offset all refs by yah much

a-valid-flag-virtual-adr (0)

a-lmc-part-name (0)             ;if non-zero, use this.  loaded from config structure.
a-mapping-register-base (0)     ;initialized to 570 when building constants.
                                ;  may be changed when config structure loaded.
        ;we actually use mapping registers offset 0, 1 and 2 from this.


(DEF-DATA-FIELD OAL-BYTL-1 6 6)         ;MICRO INSTRUCTION FIELDS
(DEF-DATA-FIELD OAL-MROT 6 0)
(DEF-DATA-FIELD OAH-A-SRC 12. 7)
(DEF-DATA-FIELD OAH-M-SRC 7 0)
(DEF-DATA-FIELD OAL-DEST 13. 14.)    ;DOES NOT WIN FOR FUNCTIONAL DESTINATIONS
(DEF-DATA-FIELD OAL-A-DEST 12. 14.)  ;USE THIS WHEN SUPPLYING AN A-MEMORY DESTINATION
(DEF-DATA-FIELD OAL-M-DEST  6. 14.)  ;USE THIS WHEN SUPPLYING AN M-MEMORY DESTINATION
(DEF-DATA-FIELD OAL-JUMP 16. 14.)
(DEF-DATA-FIELD OAH-DISP 12. 7.)     ;note in OAH on LAMBDA
(DEF-DATA-FIELD OAL-ALUF 4 3)
(DEF-DATA-FIELD OAL-CRAM-PAGE-NUMBER 12. 18.)

(LOCALITY I-MEM)

(loc 2)
cold-boot-start-addr

;loc 3 is for use by the console program - because of the complications of
; the configuration stuff, it can't be used here

; starting at 36000 loads ucode using the original disk driver, then halts
; starting at 36001 allows you use a configuration structure

(LOC 36000)
BEG
        (JUMP GO)
        (jump go-with-proc-conf-nubus-adr-in-q-r)
        (no-op halt-lambda)
        (no-op halt-lambda)

;;; Error halts - starting at address 4
;;; Put a NO-OP after each error halt so that they will be at even addresses
;;; and the 2 possible values in the PC lights will agree except for the low bit.

PROM-IS-DONE                    ;4
        (JUMP HALT-LAMBDA PROM-IS-DONE)
    (ERROR-TABLE PROM-IS-DONE)
        (NO-OP)

ERROR-ADD-LOSES                 ;6
        (JUMP HALT-LAMBDA ERROR-ADD-LOSES)
    (ERROR-TABLE ERROR-ADD-LOSES)
        (NO-OP)

ERROR-A-MEM                     ;10
        (JUMP HALT-LAMBDA ERROR-A-MEM)
    (ERROR-TABLE ERROR-A-MEM)
        (NO-OP)

ERROR-M-MEM                     ;12
        (JUMP HALT-LAMBDA ERROR-M-MEM)
    (ERROR-TABLE ERROR-M-MEM)
        (NO-OP)

ERROR-LEVEL-1-MAP               ;14
        (JUMP HALT-LAMBDA ERROR-LEVEL-1-MAP)
    (ERROR-TABLE ERROR-LEVEL-1-MAP)
        (NO-OP)

ERROR-PAGE-FAULT                ;16
        (JUMP HALT-LAMBDA ERROR-PAGE-FAULT)
    (ERROR-TABLE ERROR-PAGE-FAULT)
        (NO-OP)

ERROR-BAD-LABEL                 ;20
        (JUMP HALT-LAMBDA ERROR-BAD-LABEL)
    (ERROR-TABLE ERROR-BAD-LABEL)
        (NO-OP)

ERROR-NO-MICR                   ;22
        (JUMP HALT-LAMBDA ERROR-NO-MICR)
    (ERROR-TABLE ERROR-NO-MICR)
        (NO-OP)

ERROR-BAD-SECTION-TYPE          ;24
        (JUMP HALT-LAMBDA ERROR-BAD-SECTION-TYPE)
    (ERROR-TABLE ERROR-BAD-SECTION-TYPE)
        (NO-OP)

ERROR-BAD-ADDRESS               ;26
        (JUMP HALT-LAMBDA ERROR-BAD-ADDRESS)
    (ERROR-TABLE ERROR-BAD-ADDRESS)
        (NO-OP)

ERROR-END-OF-PARTITION          ;30
        (JUMP HALT-LAMBDA ERROR-END-OF-PARTITION)
    (ERROR-TABLE ERROR-END-OF-PARTITION)
        (NO-OP)

ERROR-BAD-FILE-FORMAT           ;32
        (JUMP HALT-LAMBDA ERROR-BAD-FILE-FORMAT)
    (ERROR-TABLE ERROR-BAD-FILE-FORMAT)
        (NO-OP)

ERROR-DISK-ERROR                ;34
        (JUMP HALT-LAMBDA ERROR-DISK-ERROR)
    (ERROR-TABLE ERROR-DISK-ERROR)
        (NO-OP)

ERROR-DIVIDE-BY-ZERO            ;36
        (JUMP HALT-LAMBDA ERROR-DIVIDE-BY-ZERO)
    (ERROR-TABLE ERROR-DIVIDE-BY-ZERO)
        (NO-OP)

ERROR-PDL-BUFFER                ;40
        (JUMP HALT-LAMBDA ERROR-PDL-BUFFER)
    (ERROR-TABLE ERROR-PDL-BUFFER)
        (NO-OP)

ERROR-BAD-MINI-LABEL            ;42
        (JUMP HALT-LAMBDA ERROR-BAD-MINI-LABEL)
    (ERROR-TABLE ERROR-BAD-MINI-LABEL)
        (NO-OP)

ERROR-BAD-CMOS                  ;44
        (JUMP HALT-LAMBDA ERROR-BAD-CMOS)
    (ERROR-TABLE ERROR-BAD-CMOS)
        (NO-OP)

ERROR-BAD-CONF-STRUCTURE        ;46
        (JUMP HALT-LAMBDA ERROR-BAD-CONF-STRUCTURE)
    (ERROR-TABLE ERROR-BAD-CONF-STRUCUTRE)
        (NO-OP)

ERROR-BAD-BIT                   ;50
        (JUMP HALT-LAMBDA ERROR-BAD-BIT)
    (ERROR-TABLE ERROR-BAD-BIT)
        (NO-OP)

error-bad-cram-adr-map          ;52
        (jump halt-lambda error-bad-cram-adr-map)
    (error-table error-bad-cram-adr-map)
        (no-op)


;;; Program starts here.  Create M-ZERO and M-ONES, and check the hardware a little.
go      ((q-r) setz)

go-with-proc-conf-nubus-adr-in-q-r
        ((a-proc-conf-nubus-adr) q-r)

        ((M-ZERO Q-R) SETZ)             ;Make all zeros. Result to Q-R, not to depend on M mem
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 1) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 2) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 3) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 4) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 5) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 6) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 7) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 10) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 11) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 12) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 13) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 14) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 15) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 16) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 17) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 20) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 21) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 22) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 23) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 24) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 25) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 26) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 27) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 30) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 31) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 32) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 33) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 34) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 35) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 36) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 37) Q-R ERROR-BAD-BIT)

        ((M-ONES Q-R) SETO)             ;Make all ones, in Q-R not to trust M Mem
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 1) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 2) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 3) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 4) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 5) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 6) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 7) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 10) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 11) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 12) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 13) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 14) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 15) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 16) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 17) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 20) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 21) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 22) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 23) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 24) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 25) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 26) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 27) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 30) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 31) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 32) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 33) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 34) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 35) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 36) Q-R ERROR-BAD-BIT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 37) Q-R ERROR-BAD-BIT)
;;; ALU and Shifter don't drop or pick bits.
;;; Test M-mem, A-mem, and M=A logic
        (JUMP-NOT-EQUAL Q-R A-ONES ERROR-A-MEM)
        (JUMP-NOT-EQUAL M-ONES A-ONES ERROR-M-MEM)
        ((Q-R) SETZ)
        (JUMP-NOT-EQUAL Q-R A-ZERO ERROR-A-MEM)
        (JUMP-NOT-EQUAL M-ZERO A-ZERO ERROR-M-MEM)
;;; See if all carries in ALU really carry.
        ((Q-R) ADD M-ONES A-ZERO ALU-CARRY-IN-ONE)
        (JUMP-NOT-EQUAL Q-R A-ZERO ERROR-ADD-LOSES)
;;; Another simple carry test
        ((Q-R) ADD M-ONES A-ONES)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) Q-R ERROR-ADD-LOSES)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 1) Q-R ERROR-ADD-LOSES)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 37) Q-R ERROR-ADD-LOSES)
;;; Prepare to test pdl buffer.  Care required since no pass-around path.
        ((C-PDL-BUFFER-POINTER-PUSH) M-ZERO)
        ((C-PDL-BUFFER-POINTER-PUSH) M-ONES)
;;; This verifies that -1 + -1 is -2 and also tests the byte hardware a little
        ((MD) (BYTE-FIELD 37 1) Q-R A-ONES)
        (JUMP-NOT-EQUAL MD A-ONES ERROR-ADD-LOSES)
;;; Foo, the byte hardware could be tested a little bit better than that!
        (JUMP-NOT-EQUAL C-PDL-BUFFER-POINTER-POP A-ONES ERROR-PDL-BUFFER)
        (JUMP-NOT-EQUAL C-PDL-BUFFER-POINTER-POP A-ZERO ERROR-PDL-BUFFER)

;;; Clear all memories to make sure they contain good parity
;;; But don't bash the M-ZERO and M-ONES constants.
;;; save the conf ptr in q-r while memory is being cleared
        ((q-r) a-proc-conf-nubus-adr)

        ((M-HUNOZ) DPB M-ONES (BYTE-FIELD 1 6) A-ZERO)  ;100, size of M memory
        ((MD) DPB M-ONES (BYTE-FIELD 1 2) A-ZERO)       ;4, lowest location to clear
CLEAR-M-MEMORY
        ((M-HUNOZ) ADD M-HUNOZ A-ONES)  ;Note that M-HUNOZ is used, to get good par in it
        ((OA-REG-LOW) DPB M-HUNOZ OAL-M-DEST A-ZERO)    ;M destination
        ((M-GARBAGE) M-ZERO)
        (JUMP-NOT-EQUAL MD A-HUNOZ CLEAR-M-MEMORY)

        ((M-HUNOZ) DPB M-ONES (BYTE-FIELD 1 6) A-ZERO)  ;100, size of M memory
CLEAR-A-MEMORY
        ((OA-REG-LOW) DPB M-HUNOZ OAL-A-DEST A-ZERO)    ;A destination
        ((A-GARBAGE) M-ZERO)
        ((M-HUNOZ) M+A+1 M-HUNOZ A-ZERO)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 12.) M-HUNOZ CLEAR-A-MEMORY)

        ((a-proc-conf-nubus-adr) q-r)

        ((dp-mode) m-zero)              ;select M memory via pdl-buffer
 ;      ((pdl-buffer-pointer) m-zero)
 ;      ((c-pdl-buffer-pointer) m-zero) ;0@m cannot be written normally.  Assure good parity.
        ((pdl-buffer-pointer) dpb m-ones (byte-field 6 0) a-zero)  ;77
clear-shadowwed-pdl-buffer
  ;Notes:  when pdl buffer pointer is 0, 0@m has been written, writing good parity there.
  ;  Also clears entire shadowwed memory area.
  ;  If bits 31.-11. of pdl pointer fail to read 0 (a hardware error which has sometimes
  ;     been seen), it will hang in a loop here.
  ;  DP-MODE is reselected below.
        ((c-pdl-buffer-pointer-push) m-zero)
        (jump-not-equal pdl-buffer-pointer a-zero clear-shadowwed-pdl-buffer)

;;; Construct useful constants.
MAKE-CONSTANTS
        (build-constant m-ones a-zero a-1 1)
        (build-constant m-ones a-zero A-2 2)
        (build-constant m-ones a-zero A-3 3)
        (build-constant m-ones a-zero A-4 4)
        (build-constant m-ones a-zero A-5 5)
        (build-constant m-ones a-zero A-6 6)
        (build-constant m-ones a-zero A-11 11)
        (build-constant m-ones a-zero A-20 20)
        (build-constant m-ones a-zero A-26 26)
        (build-constant m-ones a-zero A-31 31)
        (build-constant m-ones a-zero A-40 40)
        (build-constant m-ones a-zero A-140 140)
        (build-constant m-ones a-zero A-200 200)
        (build-constant m-ones a-zero A-202 202)
        (build-constant m-ones a-zero a-340 340)
        (build-constant m-ones a-zero A-400 400)
        (build-constant m-ones a-zero A-1000 1000)
        (build-constant m-ones a-zero A-1400 1400)
        (build-constant m-ones a-zero a-1576 1576)
        (build-constant m-ones a-zero A-2400 2400)
        (build-constant m-ones a-zero A-3777 3777)
        (build-constant m-ones a-zero A-10400 10400)
        (build-constant m-ones a-zero A-20000 20000)
        (build-constant m-ones a-zero A-36000 36000)
        (build-constant m-ones a-zero a-1374000 1374000)
        (build-constant m-ones a-zero a-7 7)
        (build-constant m-ones a-zero a-3000 3000)
        (build-constant m-ones a-zero a-3177 3177)
        (build-constant m-ones a-zero a-3400 3400)
        (build-constant m-ones a-zero a-5400 5400)
        (build-constant m-ones a-zero a-17740160 17740160)
        (build-constant m-ones a-zero a-1464 1464)
        (build-constant m-ones a-zero a-1363200 1363200)
        (build-constant m-ones a-zero a-5000 5000)

        (build-constant m-ones a-zero a-%processor-conf-share-runme
                (eval %processor-conf-share-runme))

        (build-constant m-ones a-zero a-mapping-register-base 570)

        (build-constant m-ones a-zero a-disk-regs 1100) ;1100, page 2, io adr 100

        ((DP-MODE) A-1)         ;SET HIGH BIT OF PDL ADDRESS TO AVOID M-MEMORY
        ((M-A) RG-MODE)         ;SET 24 BIT ADDRESS MODE.
        ((RG-MODE) DPB M-ONES (BYTE-FIELD 1 30.) A-A)

;;; Clear some more memories
        ((PDL-BUFFER-POINTER M-A)       ;4000, size of pdl buffer
                DPB M-ONES (BYTE-FIELD 1 11.) A-ZERO)
CLEAR-PDL-BUFFER
        ((C-PDL-BUFFER-POINTER-PUSH) M-ZERO)
        ((M-A) ADD M-A A-ONES)
        (JUMP-NOT-EQUAL M-A A-ZERO CLEAR-PDL-BUFFER)

        ((M-A) A-400)                   ;Size of SPC stack
CLEAR-SPC-MEMORY
        ((MICRO-STACK-DATA-PUSH) SETZ)
        ((M-A) ADD M-A A-ONES)
        (JUMP-NOT-EQUAL M-A A-ZERO CLEAR-SPC-MEMORY)

        ((rg-mode) m-zero)  ;set 25 bit virtual address mode.  Initialize other bits.
        ((MD M-A) SETZ)
CLEAR-LEVEL-1-MAP
        ((L1-MAP) M-A)  ;Clear level-1 map
        ((MD) ADD MD A-20000)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 25.) MD CLEAR-LEVEL-1-MAP)

        ((MD) SETZ)
CLEAR-LEVEL-2-MAP
        ((L1-MAP) (BYTE-FIELD 7 13.) MD)         ;Make level-1 map point at level-2
        ((L2-MAP-CONTROL) SETZ) ;Clear level-2 map
        ((L2-MAP-PHYSICAL-PAGE) SETZ)
        ((MD) ADD MD A-400)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 25.) MD CLEAR-LEVEL-2-MAP)


;I'm only guessing at how the parity should work here...
;it seems that the cram-adr-map has 12 address bits, and 12 data bits
;the low 10 data bits are used for the physical address, so there are 2
;bits for parity.  All 12 data bits go into a 12 bit parity checker, so
;we can use the 2 parity bits any way we want as long as the result is
;odd parity.

;though it might be useful to use both parity bits, the other programs
;don't so we always leave the second to highest bit 0

;   P0XX XXXX XXXX

        (build-constant m-ones a-zero m-a 7777) ;page number
load-straight-cram-adr-map
        ((m-c) ldb (byte-field 4 0) m-a)
        ((m-d) ldb (byte-field 4 4) m-a)
        ((m-c) xor m-c a-d)
        ((m-d) ldb (byte-field 2 8) m-a)
        ((m-c) xor m-c a-d)
        ((m-d) ldb (byte-field 2 2) m-c)
        ((m-c) xor m-c a-d)
        ((m-d) ldb (byte-field 1 1) m-c)
        ((m-c) xor m-c a-d)
        ((m-c) xor m-c a-1)
        ((m-b) ldb (byte-field 10. 0) m-a)
        ((m-b) dpb m-c (byte-field 1 11.) a-b)

        ((oa-reg-low) dpb m-a oal-cram-page-number a-zero)
        (call-xct-next)
        (popj-after-next (cram-adr-map) m-b)

 ;can't check on old boards  - it starts working at version 3.2
 ;      ((oa-reg-low) dpb m-a oal-cram-page-number a-zero)
 ;      (call-xct-next)
 ;      (popj-after-next (m-d) ldb (byte-field 12. 0) cram-adr-map)
 ;      (jump-not-equal m-d a-b error-bad-cram-adr-map)

        (jump-greater-xct-next m-a a-zero load-straight-cram-adr-map)
       ((m-a) sub m-a a-1)

        ((M-A MD) SETZ)
        (BUILD-CONSTANT M-ONES A-ZERO M-B 36000) ;clear from 0 to beginning of bootstrap
        ((M-C) DPB M-ONES (BYTE-FIELD 4 28.) A-ZERO)    ;HIGH WORD OF IZERO-GOOD-PARITY
CLEAR-I-MEMORY
        ((OA-REG-LOW) DPB MD OAL-JUMP A-ZERO)
        (CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT
        (CRAM-HIGH) M-C)

        ((OA-REG-LOW) DPB MD OAL-JUMP A-ZERO)
        (CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT
        (CRAM-LOW) SETZ)

        ((MD) ADD MD A-1)
        (JUMP-LESS-THAN MD A-B CLEAR-I-MEMORY)                 ;RAM, clear to limit

        ((m-hunoz) rg-mode)
        ((rg-mode) dpb m-zero (byte-field 2 34) a-hunoz)  ;select 0 for MID.HIGH.ADR
clear-mid-memory        ;clear it in 4 sections by frobbing the HIGH.MID.ADR bits in RG-MODE
        ((m-a) m-zero)
clear-mid-section
        ((m-b) dpb m-a (byte-field 10. 6) a-zero)
        ((md) dpb m-b (byte-field 20 20) a-b)
        (source-to-macro-ir md)
        ((macro-ir-decode) m-zero)
        ((m-a) add m-a a-zero alu-carry-in-one)
        (jump-if-bit-clear (byte-field 1 10.) m-a clear-mid-section)
        ((m-a) ldb (byte-field 2 34) rg-mode a-zero)
        ((m-a) add m-a a-zero alu-carry-in-one)
        ((m-hunoz) rg-mode)
        ((rg-mode) dpb m-a (byte-field 2 34) a-hunoz)
        (jump-if-bit-clear (byte-field 1 2) m-a clear-mid-memory)
                        ;HIGH.MID.ADR bits have been set back to zero for normal operation.

        ((rg-mode) dpb m-ones (byte-field 1 30.) a-zero)  ;switch to 24 bit virtual adr mode.


;;; Set up the map.  First, write a 0 into the first location of the level
;;; one map.  This is the only location we use.  Read back to be safe.
SET-UP-THE-MAP

;;; Set up the following pages:
;;;  page 0   (0-377): mapped to the first page of main memory
;;;  page 1 (400-777): mapped to second page of main memory. (system communication area)
;;;                      (has IOPB at hardware virtual address 640 - 650)
;;;  page 2 (1000-1377): mapped to disk control registers (mbus IO space)
;;;unfortunately, mapping registers only work if written a byte at a time!!
;;; so we set up 3 pages in byte mode, one for each byte position we need to write.
;;;  page 3 (1400-1777): mapped to multibus-nubus map registers in SDU.  byte 0
;;;             we use two mapping registers, one so disk can reference IOPB,
;;;              and one to reference buffer.  570 for buffer, 571 for IOPB.
;;      (now offsets 0 and 1 from a-mapping-register-base).
;;;  page 4  (2000-2377): above, byte 1
;;;  page 5  (2400-2777): above, byte 2
;;;  page 6  (3000-3377): low byte of SDU register page (#xff01c000)
;;;  page 7  (3400-3777): to map in random nubus pages in nubus-read
;;;  page 10 (4000-4377): share-struct page 1
;;;  page 11 (4400-4777): share-struct, in case it crosses a page boundary
;;;  page 12 (5000-5377): configuration-structure
;;;  page 13 (5400-5777): configuration structure, page 2 (necessary if > 4 processors)
SET-UP-PAGES
        ((m-convert-phys-adr) setz)
        ((a-base-physical-page) dpb m-ones (byte-field 4 18.) a-zero)   ;17000000 (hi nubus)
        (build-constant m-ones a-zero m-a 14)
        ((a-base-physical-page) dpb m-a (byte-field 4 14.) a-base-physical-page)
        (jump-equal m-zero a-proc-conf-nubus-adr clear-main-memory)

        ((m-a) a-proc-conf-nubus-adr)
        ((m-b) ldb (byte-field 4 28.) m-a a-zero)
        (build-constant m-ones a-zero m-c 17)
        (jump-equal m-b a-c set-up-pages-1)
        (build-constant m-ones a-zero m-convert-phys-adr #x10000000)
set-up-pages-1

        ;first get the configuration structure set up

        ;get sys conf local physical address
        (call-xct-next nubus-read)
       ((m-nubus-address) a-proc-conf-nubus-adr)
        ((m-a) xor md a-convert-phys-adr)

        ((md) a-5000) ;page 12
        ((l2-map-control) a-1400)
        ((l2-map-physical-page m-d) ldb (byte-field 22. 10.) m-a)
        ((md) add md a-400) ;page 13
        ((l2-map-control) a-1400)
        ((l2-map-physical-page) add m-d a-1)

        ((m-sys-conf-virtual-adr) a-5000)

        ((m-d) a-proc-conf-nubus-adr)
        ((m-d) sub m-d a-a)
        ((m-d) ldb (byte-field 30. 2) m-d)
        ((m-proc-conf-virtual-adr) add m-sys-conf-virtual-adr a-d)


        (build-constant m-ones a-zero m-a (eval %processor-conf-memory-base-0))
        ((vma-start-read) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        ((md) xor md a-convert-phys-adr)
        ((a-base-physical-page) ldb (byte-field 22. 10.) md)

clear-main-memory
; CLEAR MAIN MEMORY TO SET UP GOOD PARITY
        ;;this value is only used if the following jump happens ... i.e. we
        ;;started at location 36000 and are running without a sys conf structure
        (build-constant m-ones a-zero m-b (eval (* 1000 256.))) ;highest address
        ((m-c) setz)                            ;lowest

        (jump-equal m-zero a-proc-conf-nubus-adr clear-main-memory-loop)

        (build-constant m-ones a-zero m-b (eval %processor-conf-memory-bytes-0))
        ((vma-start-read) add m-proc-conf-virtual-adr a-b)
        (call-if-page-fault error-page-fault)
        ((m-b) ldb (byte-field 30. 2) md)

        ;the new booter clears the first 2 pages itself so the system configuration
        ;area wont get clobbered in as many places
        ((m-c) a-1000)

;;; a-base-physical-page is hi 22. bits of nubus address of first page
;;; m-b is number of words to clear above a-base-physical-page

CLEAR-MAIN-MEMORY-loop
        ((M-B) ADD M-B A-ONES)
        ((MD) A-ZERO)   ;data to write and also address map page 0.
        ((M-A) LDB M-B (BYTE-FIELD 22. 8) A-ZERO)       ;get page number within memory.
        ((L2-MAP-CONTROL) DPB M-ONES (BYTE-FIELD 1 9) A-400)  ;1400, rw access
        ((L2-MAP-PHYSICAL-PAGE) add M-A a-base-physical-page)
        ((VMA-START-WRITE) LDB M-B (BYTE-FIELD 8 0) A-ZERO)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        (JUMP-NOT-EQUAL M-B a-c CLEAR-MAIN-MEMORY-loop)

        ; page 0 to first page of memory
        ((md) setz)             ;page 0, main mem
        ((l2-map-control) a-1400)
        ((l2-map-physical-page) a-base-physical-page)


        ((md) add md a-400)     ;page 1, set up to second page in main mem, sys-com-area.
        ((l2-map-control) a-1400)
        ((l2-map-physical-page) m+a+1 m-zero a-base-physical-page)

        ((md) add md a-400)     ;page 2, disk control regs.

        ; now set page 2 to multibus IO space
        ;multibus IO space is at #xff100000 on the SDU
        ((m-a) a-5400)  ;5400, packet size code 1
        ((l2-map-control) dpb m-ones (byte-field 1 11.) a-a)
        (build-constant m-ones a-zero m-a #xff100000)
        ((m-a) xor m-a a-convert-phys-adr)
        ((l2-map-physical-page) ldb (byte-field 22. 10.) m-a)

        ((md) a-3000)                           ;page 6, SDU control regs
        ((l2-map-control) a-5400)               ;5400, packet size code 1
        (build-constant m-ones a-zero m-a #xff01c000)
        ((m-a) xor m-a a-convert-phys-adr)
        ((l2-map-physical-page) ldb (byte-field 22. 10.) m-a)


;;; Configuration and options

get-configuration
        ((m-proc-switches) setz)
        (jump-equal m-zero a-proc-conf-nubus-adr get-configuration-done)

        (build-constant m-ones a-zero m-a (eval %system-configuration-version-number))
        ((vma-start-read) add m-sys-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        (call-not-equal md a-1 error-bad-conf-structure)        ;must be version 1

read-processor-block
        (build-constant m-ones a-zero m-a (eval %processor-conf-starting-processor-switches))
        ((vma-start-read) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        ((m-proc-switches) md)

        (jump-if-bit-clear (lisp-byte %%processor-switch-2x2-stuff-valid-in-conf-structure)
                           m-proc-switches rpb-0)
  ;load new stuff, ie, micro-band and base-multibus-mapping-register
        (build-constant m-ones a-zero m-a (eval %processor-conf-micro-band))
        ((vma-start-read) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        ((a-lmc-part-name) md)

        (build-constant m-ones a-zero m-a
                (eval %processor-conf-base-multibus-mapping-register))
        ((vma-start-read) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        ((a-mapping-register-base) md)

rpb-0

  ;wait to set up page 3,4,5 until after config is read and a-mapping-register-base is known.
        ((md) a-1400)   ;page 3, multibus -> nubus mapping register in SDU, byte 0
        (build-constant m-ones a-zero m-b 5400) ;packet code 1
        ((l2-map-control) m-b)

      ;sdu in slot 17   17740141
      ;multibus mem space at 0 within slot space
      ;address map begins at multibus 18000(hex) ie 300000 bytes, ie 60000 wds, ie 140 pages
      ;we need 140, 141, 142, or 143 depending on a-mapping-register-base

        ;there are 1024 mapping registers starting at nubus address #xff018000
        ;each register is 4 bytes long, so they occupy 4 pages
        ;We map virtual page 3 to the page which contains the handful of mapping
        ;registers we will use.
        ;a-mapping-register-base contains the first mapping page we want, so
        ;(ldb (byte 2 8) a-mapping-register-base) tells how many pages past
        ;#xff018000 to go for the first register

rpb-set-up-sdu-mapping-regs
        (build-constant m-ones a-zero m-a #xff018000)
        ((m-tem) a-mapping-register-base)
        ((m-c) add m-tem a-2)           ;will eventually use 0, 1, and 2 from here.
        ((m-tem) ldb (byte-field 2 8.) m-tem a-zero)
        ((m-c) ldb (byte-field 2 8.) m-c a-zero)
          ;all these SDU mapping registers had better be addressible by the same page.
          ;if not, we lose.  (otherwise, it would have to set up another set
          ;   of 3 to hack it, etc)
        (jump-not-equal m-tem a-c sdu-mapping-registers-inappropriate)

        ((m-a) dpb m-tem (byte-field 2 10.) a-a) ;m-a is now sdu-phys adr of 1st mapping reg
        ((m-a) xor m-a a-convert-phys-adr) ;convert to local-phys adr
        ((l2-map-physical-page m-c) ldb (byte-field 22. 10.) m-a) ;get page number

        ((md) add md a-400)     ;page 4, multibus -> nubus mapping register in SDU, byte 1
        ((l2-map-control) m-b)
        ((l2-map-physical-page) dpb m-ones (byte-field 1 22.) a-c)

        ((md) add md a-400)     ;page 5, multibus -> nubus mapping register in SDU, byte 2
        ((l2-map-control) m-b)
        ((l2-map-physical-page) dpb m-ones (byte-field 1 23.) a-c)

  ;set up multibus->nubus mapping registers.  570 -> main mem page 0, 571 -> page 1
rpb-set-up-disk-mapping-regs
        ((m-a) a-base-physical-page)
        ((m-b) ldb (byte-field 22. 10.) m-convert-phys-adr)
        ((m-a) xor m-b a-a)
        ((md)  dpb m-ones (byte-field 1 23.) a-a)       ;enable bit

        ((m-a) a-mapping-register-base)
        ((m-a) ldb (byte-field 8. 0) m-a a-zero)

        ((vma-start-write m-c) dpb m-ones (byte-field 2 8) a-a) ;pg 3, low byte of mapping reg
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)       ;do three writes, one for each byte.
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)
        (call-if-page-fault error-page-fault)

        ((md) add md a-1)
        ((vma-start-write) add m-c a-1)
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)       ;do three writes, one for each byte.
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)
        (call-if-page-fault error-page-fault)


        (jump-if-bit-clear (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                           m-proc-switches get-configuration-done)

rpb-init-share-iopb
        ;initialize share-iopb structure
        ((md) a-zero)
        ((vma-start-write) add m-proc-conf-virtual-adr a-%processor-conf-share-runme)
        (call-if-page-fault error-page-fault)
        (build-constant m-ones a-zero m-a (eval %processor-conf-share-interrupt-addr))
        ((vma-start-write) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)

        (build-constant m-ones a-zero m-a (eval %processor-conf-share-spare-1))
        ((vma-start-write) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-1)         ;spare 2
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-1)         ;spare 3
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-1)         ;spare 4
        (call-if-page-fault error-page-fault)

        (build-constant m-ones a-zero m-a (eval %processor-conf-slot-number))
        ((vma-start-read) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        (build-constant m-ones a-zero m-a (eval %processor-conf-share-slot))
        ((vma-start-write) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)

        (build-constant m-ones a-zero m-a (eval %processor-conf-share-type))
        ((md) a-3)
        ((vma-start-write) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)

        ;;construct a pointer for the SDU to our IOPB
        ;;SDU mapping register 571 points to the IOPB
        ;;the iopb is at virtual address 640 - 400 a page offset of 240
        ;;therefore, the multibus offset is (+ (ash 571 10.) (* 240 4.)) => 1363200

  ;     (build-constant m-ones a-zero m-a (eval (+ (ash 571 10.) (* (ldb 0010 640) 4.))))
rpb-make-share-iopb-ptr
        (build-constant m-ones a-zero m-a (eval (* (ldb 0010 640) 4.)))
        ((m-tem) m+a+1 a-mapping-register-base m-zero)
        ((m-a) dpb m-tem (byte-field 10. 10.) a-a)

        ((md) m-a)                              ;address in multibus that maps to iopb
        (call convert-md-to-8086)               ;make into 8086 pointer
        (build-constant m-ones a-zero m-a (eval %processor-conf-share-iopb))
        ((vma-start-write) add m-proc-conf-virtual-adr a-a)
        (call-if-page-fault error-page-fault)

        ;;set up SDU mapping register 576 to point to share-iopb structure
        ;;we don't worry about the byte offset since we assume that our proc
        ;;conf will be on the first page (this is true with up to 4 lambdas,
        ;;and all of the lambdas are always first in the table
        ((m-a) a-proc-conf-nubus-adr)
        ((m-a) xor m-a a-convert-phys-adr)
        ((m-a) ldb m-a (byte-field 22. 10.) a-zero)     ; was 8
        ((md) dpb m-ones (byte-field 1 23.) a-a)        ; enable bit
        ;;virtual page 3 is first of sdu mapping registers

;       (build-constant m-ones a-zero m-a (eval (+ (* 3 256.) (ldb 0010 576))))
        (build-constant m-ones a-zero m-a (eval (* 3 256.) ))
        ((m-tem) a-mapping-register-base)
        ((m-tem) add m-tem a-2)                 ;was 6
        ((m-tem) ldb (byte-field 8 0) m-tem a-zero)
        ((m-a) add m-a a-tem)

        ((vma-start-write) m-a)
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)       ;write 3 times
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)
        (call-if-page-fault error-page-fault)

rpb-share-iopb-link
        ;;do the work to link into the iopb "chain"
        ;;get 2 pages of virtual address space pointing to the share struct
        ;;(2 in case it crosses a page boundary)
        (build-constant m-ones a-zero m-a (eval %system-configuration-share-struct-pointer))
        ((vma-start-read) add m-sys-conf-virtual-adr a-a) ;32 bit byte adr of share struct
        (call-if-page-fault error-page-fault)
        ((md) xor md a-convert-phys-adr)
        ((m-a) ldb (byte-field 22. 10.) md)
        (build-constant m-ones a-zero m-share-struct-virtual-adr 4000) ;pg 10 and 11
        ((m-share-struct-virtual-adr) ldb (byte-field 8 2) md a-share-struct-virtual-adr)
        ((md) m-share-struct-virtual-adr)
        ((l2-map-control) a-1400)
        ((l2-map-physical-page) m-a)
        ((md) add md a-400)
        ((l2-map-control) a-1400)
        ((l2-map-physical-page) add m-a a-1)

        (build-constant m-ones a-zero m-a (eval %share-struct-max-iopbs))
        ((vma-start-read) add m-share-struct-virtual-adr a-a)
        (call-if-page-fault error-page-fault)
        ((a-max-iopbs) md)

        ;m-b is the iopb slot number counter
        ;m-d is virtual address of base of valid table
        ((m-b) setz)
        (build-constant m-ones a-zero m-d (eval %share-struct-start-of-valid-table))
        ((m-d) add m-share-struct-virtual-adr a-d)
find-empty-entry
        ((vma-start-read) add m-d a-b)
        (call-if-page-fault error-page-fault)
        (jump-equal md a-zero found-empty-entry)
        ((m-b) add m-b a-1)
        (call-equal m-b a-max-iopbs error-bad-conf-structure)
        (jump find-empty-entry)

   ;get here if mapping registers split across a losing boundary.  Move up one notch and
   ;  try again.  Must win on second try (ha ha).
sdu-mapping-registers-inappropriate
        ((m-tem) a-3)
        (jump-xct-next rpb-set-up-sdu-mapping-regs)
       ((a-mapping-register-base) add m-tem a-mapping-register-base)

found-empty-entry
        ;; now m-b is number of an empty share-iopb slot
        ;; now we have to make an 8086 pointer to the share-iopb in multibus mapping
        ;; register 576.  The multibus offset is
        ;; (+ (ash 576 10.) (ldb 0012 (* 4 (+ m-proc-conf-virtual-adr
        ;;                                    a-%processor-conf-share-runme))))
        ;; (ash 576 10.) = 1374000
        ((m-a) add m-proc-conf-virtual-adr a-%processor-conf-share-runme)
  ;     ((md) dpb m-a (byte-field 8 2) a-1374000)
        ((m-tem) a-mapping-register-base)
        ((m-tem) add m-tem a-2)         ;was 6
        ((m-tem) dpb m-tem (byte-field 10. 10.) a-zero)
        ((md) dpb m-a (byte-field 8 2) a-tem)

        (call convert-md-to-8086)

        ;; now md is the approiate pointer, store it in the iopb table
        ((m-a) add m-d a-max-iopbs)
        ((vma-start-write) add m-a a-b)
        (call-if-page-fault error-page-fault)


        ;; set the valid flag
        ((md) a-1)
        ((a-valid-flag-virtual-adr) add m-d a-b)
        ((vma-start-write) a-valid-flag-virtual-adr)
        (call-if-page-fault error-page-fault)

get-configuration-done
        (no-op)

;;; The size of the disk is not known until the label is read in and parsed.
;;; In order to make the disk operations win, we have to initialize the
;;; size parameters to something reasonable; it doesn't matter what they
;;; are exactly since only blocks 0 and 1 will be dealt with.

;;; the above comment was true for the cadr disk controller, but unfortunately not
;;; for the lambda's.  Instead, we read the number of sectors and heads from the
;;; SDU cmos ram
FUDGE-INITIAL-DISK-PARAMETERS
        (CALL COLD-GET-DISK-GEOMETRY-FROM-CMOS)
        ((A-HEADS-TIMES-BLOCKS) A-200)  ;LARGE NUMBER TO KEEP THINGS ON CYL 0.

        ((A-DISK-CYLINDER-OFFSET) A-ZERO)       ;until mini-label read in.

;;; Initialize the disk.
 ; Not necessary on lambda
 ;      (CALL DISK-RECALIBRATE)


read-mini-label
        ((m-temp-1) a-26)               ;mini label in block 22.
        (call-xct-next disk-read)
       ((m-a) setz)                     ;phys mem address
       ;; M-B/ FOOB = 106 117 117 102 = 10223647506
        (build-constant m-ones a-zero m-b (eval (logior      #/F
                                                        (ash #/O 8)
                                                        (ash #/O 16.)
                                                        (ash #/B 24.))))
        ((VMA-START-READ M-C) A-ZERO)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)   ;First location of mini-label must be ascii
                                                ; FOOB
        (CALL-NOT-EQUAL READ-MEMORY-DATA A-B ERROR-BAD-MINI-LABEL)
       ;; M-B/ LISP = 114 111 123 120 = 12024644514
        (build-constant m-ones a-zero m-b (eval (logior      #/L
                                                        (ash #/I 8)
                                                        (ash #/S 16.)
                                                        (ash #/P 24.))))
        ((VMA-START-READ M-C) ADD M-C A-1)      ;1
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)   ;Second location of label must be ascii LISP
        (CALL-NOT-EQUAL READ-MEMORY-DATA A-B ERROR-BAD-MINI-LABEL)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;2
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-DISK-CYLINDER-OFFSET) READ-MEMORY-DATA)     ;CYLINDER OFFSET
        ((VMA-START-READ M-C) ADD M-C A-1)      ;3
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)           ;LENGTH IN CYLINDERS
        ((VMA-START-READ M-C) ADD M-C A-1)      ;4
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)           ;END FLAG
        (CALL-NOT-EQUAL READ-MEMORY-DATA A-ONES ERROR-BAD-MINI-LABEL)

;;; Read the label (block 0) into physical memory page 0, get disk
;;; parameters, and find out where the MICR partition is.
READ-LABEL
        ((M-TEMP-1) SETZ)                               ;disk block
        (CALL-XCT-NEXT DISK-READ)
       ((M-A) SETZ)                                     ;phys mem address

        ;; M-B/ LABL = 114 102 101 114 = 11420440514
        (build-constant m-ones a-zero m-b (eval (logior      #/L
                                                        (ash #/A 8)
                                                        (ash #/B 16.)
                                                        (ash #/L 24.))))
DECODE-LABEL
        ((VMA-START-READ M-C) A-ZERO)           ;0
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)   ;First location of label must be ascii LABL
        (JUMP-NOT-EQUAL READ-MEMORY-DATA A-B ERROR-BAD-LABEL)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;1
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)   ;Second location of label must be version 1.
        (JUMP-NOT-EQUAL READ-MEMORY-DATA A-1 ERROR-BAD-LABEL)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;2
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-NCYLS) READ-MEMORY-DATA)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;3
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-NHEADS) READ-MEMORY-DATA)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;4
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-NBLKS) READ-MEMORY-DATA)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;5
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-HEADS-TIMES-BLOCKS) READ-MEMORY-DATA)
        ((VMA-START-READ M-C) ADD M-C A-1)      ;6
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((M-B) READ-MEMORY-DATA)        ;name of microload partition (old-style)
        (jump-equal m-zero a-lmc-part-name decode-label-1)
        ((m-b) a-lmc-part-name)         ;use microload from config structure.
decode-label-1
        ((VMA-START-READ M-C)                   ;200
                DPB M-ONES (BYTE-FIELD 1 7) A-ZERO)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((M-D) READ-MEMORY-DATA)                ;M-D gets number of partitions
        ((VMA-START-READ M-C) ADD M-C A-1)      ;201
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((M-A) READ-MEMORY-DATA)                ;M-A gets words per partition descriptor
        ((M-C) ADD M-C A-1)                     ;202 (start of partition table)


        ;; M-B/ Name of microload partition.
        ;; M-C/ Address of partition descriptor.
        ;; M-D/ Number of partitions
        ;; M-A/ Number of words per partition descriptor.
SEARCH-LABEL
        (JUMP-EQUAL M-D A-ZERO ERROR-NO-MICR)
        ((VMA-START-READ) M-C)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        (JUMP-EQUAL READ-MEMORY-DATA A-B FOUND-PARTITION)
        ((M-C) ADD M-C A-A)
        (JUMP-XCT-NEXT SEARCH-LABEL)
       ((M-D) SUB M-D A-1)

FOUND-PARTITION
        ((VMA-START-READ M-C) ADD M-C A-1)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-MICR-ADDRESS) READ-MEMORY-DATA)
        ((A-MICR-ORIGIN) A-MICR-ADDRESS)
        ((VMA-START-READ) ADD M-C A-1)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((A-MICR-N-BLOCKS) READ-MEMORY-DATA)
        ((M-MICR-OFFSET) A-400) ;So will read in a new page first off

;;; Process one section.  Each section starts with three words:
;;; The section type, the initial address, and the number of locations.
;;; These are gotten into M-B, M-C, and M-D; then the section type is
;;; "dispatched" on.
;;; Section codes are:
;;; 1 = I-MEM, 2 = D-MEM (obsolete), 3 = MAIN-MEM, 4 = A-M-MEM, 5 MACRO-INSTRUCTION-DECODE
PROCESS-SECTION
        (CALL GET-NEXT-WORD)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-B) M-A)                      ;gets section code (first word)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-C) M-A)                      ;gets starting adr (second word)
        ((M-D) M-A)                     ;size (third word)
        (JUMP-EQUAL M-B A-1 PROCESS-I-MEM-SECTION)
        (JUMP-EQUAL M-B A-3 PROCESS-MAIN-MEM-SECTION)
        (JUMP-EQUAL M-B A-4 PROCESS-A-MEM-SECTION)
        (JUMP-EQUAL M-B A-5 PROCESS-MID-SECTION)
        (JUMP ERROR-BAD-SECTION-TYPE)

PROCESS-I-MEM-SECTION
        (JUMP-NOT-EQUAL M-C A-ZERO ERROR-BAD-ADDRESS)
PROCESS-I-MEM-SECTION-0
        (JUMP-EQUAL M-D A-ZERO PROCESS-SECTION)         ;count exhausted
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-D) SUB M-D A-1)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-B) M-A)
  ;dont clobber self!  There can be stuff up there which will eventually be pagable, but
  ;dont worry about it now.  (It will be read in by band when it starts).
        (jump-greater-or-equal m-c a-36000 process-i-mem-section-1)
        ;;; Now the first word of the instruction is in M-B, second word is in M-A,
        ;;; and the address in I-MEM is in M-C.
        ((OA-REG-LOW) DPB M-C OAL-JUMP A-ZERO)
        (CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT (CRAM-HIGH) M-A)        ;write high word first to guard against
        ((OA-REG-LOW) DPB M-C OAL-JUMP A-ZERO)  ; certain randomnesses..
        (CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT (CRAM-LOW) M-B)
process-i-mem-section-1
        (JUMP-XCT-NEXT PROCESS-I-MEM-SECTION-0)
       ((M-C) ADD M-C A-1)

PROCESS-MID-SECTION
        (JUMP-EQUAL M-D A-ZERO PROCESS-SECTION)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-D) SUB M-D A-1)
        ((m-temp-2) rg-mode)
        ((md) ldb (byte-field 2 10.) m-c a-zero)
        ((rg-mode) dpb md (byte-field 2 34) a-temp-2)   ;set up high address bits.
        ((M-TEMP-1) DPB M-C (BYTE-FIELD 20. 6) A-ZERO)
        ((MD) DPB M-TEMP-1 (BYTE-FIELD 20 20) A-TEMP-1)
        (SOURCE-TO-MACRO-IR MD)
        ((MACRO-IR-DECODE) M-A)
        ((rg-mode) dpb m-zero (byte-field 2 34) a-temp-2) ;set high MID bits to zero in case
        (JUMP-XCT-NEXT PROCESS-MID-SECTION)               ; this is last time thru.
       ((M-C) ADD M-C A-1)


PROCESS-MAIN-MEM-SECTION
        (CALL GET-NEXT-WORD)
        ;;; M-C/ Number of blocks.
        ;;; M-D/ Address of first block, relative to beginning of partition.
        ;;; M-A/ Physical memory address of first word.
        ((M-B) ADD M-D A-MICR-ORIGIN)
MAIN-MEM-LOOP
        (JUMP-EQUAL M-C A-ZERO PROCESS-SECTION)
        (CALL-XCT-NEXT DISK-READ)
       ((M-TEMP-1) M-B)
        ((M-B) ADD M-B A-1)
        ((M-A) ADD M-A A-400)
        (JUMP-XCT-NEXT MAIN-MEM-LOOP)
       ((M-C) SUB M-C A-1)

PROCESS-A-MEM-SECTION
        (JUMP-NOT-EQUAL M-C A-ZERO ERROR-BAD-ADDRESS)
        ((PDL-BUFFER-POINTER) SUB M-C A-1)
A-MEM-LOOP
        (JUMP-EQUAL M-D A-ZERO DONE-LOADING)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-D) SUB M-D A-1)
        ((C-PDL-BUFFER-POINTER-PUSH) M-A)
        (JUMP-NOT-EQUAL PDL-BUFFER-POINTER A-3777 A-MEM-LOOP)
        ((M-C) ADD PDL-BUFFER-POINTER A-1)
A-MEM-LOOP-2            ;high half is loaded directly into A-MEM, not via PDL-BUFFER.
        (JUMP-EQUAL M-D A-ZERO DONE-LOADING)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-D) SUB M-D A-1)
        ((OA-REG-LOW) DPB M-C OAL-A-DEST A-ZERO)
        ((A-GARBAGE) M-A)
        ((M-C) ADD M-C A-1)
        (JUMP A-MEM-LOOP-2)

DONE-LOADING
        (jump-if-bit-clear (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                m-proc-switches done-loading-1)
        ;remove prom from IOPB table
        ((md) setz)
        ((vma-start-write) a-valid-flag-virtual-adr)
        (call-if-page-fault error-page-fault)

done-loading-1
        ;save these variables while a and m memory are initialized
        ((q-r) a-proc-conf-nubus-adr)
        ((md) m-proc-switches)

;;; Copy the PDL buffer into lower half of A/M memory.
        ((PDL-BUFFER-INDEX) SETZ)
FILL-M-LOOP
        ((OA-REG-LOW) DPB PDL-BUFFER-INDEX OAL-M-DEST A-ZERO)
        ((M-GARBAGE) C-PDL-BUFFER-INDEX)
        ((PDL-BUFFER-INDEX) M+1 PDL-BUFFER-INDEX)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 6) PDL-BUFFER-INDEX FILL-M-LOOP)
FILL-A-LOOP
        ((OA-REG-LOW) DPB PDL-BUFFER-INDEX OAL-A-DEST A-ZERO)
        ((A-GARBAGE) C-PDL-BUFFER-INDEX)
        ((PDL-BUFFER-INDEX) M+1 PDL-BUFFER-INDEX)
        (JUMP-NOT-EQUAL PDL-BUFFER-INDEX A-ZERO FILL-A-LOOP)

        ((PDL-BUFFER-POINTER M-A)       ;4000, size of pdl buffer
                DPB M-ONES (BYTE-FIELD 1 11.) A-ZERO)
CLEAR-PDL-BUFFER-AGAIN
        ((C-PDL-BUFFER-POINTER-PUSH) M-ZERO)
        ((M-A) ADD M-A A-ONES)
        (JUMP-NOT-EQUAL M-A A-ZERO CLEAR-PDL-BUFFER-AGAIN)

jump-to-cold-boot
        ;m-proc-switches was saved in md before filling a and m memories
        (jump-if-bit-set md (lisp-byte %%processor-switch-prom-jumps-to-cold-boot)
                        cold-boot-start-addr) ;conf ptr in q-r

        (jump prom-is-done)


GET-NEXT-WORD
;;; Get the next word of the MICR partition into M-A.  A-MICR-ADDRESS
;;; contains the number of the next page to be read in from it.
;;; A-MICR-N-BLOCKS has the number of remaining pages in it.
;;; M-MICR-OFFSET has the physical address of the next word.
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
        (JUMP-GREATER-OR-EQUAL M-MICR-OFFSET A-400 GET-NEXT-PAGE)
        ((VMA-START-READ) M-MICR-OFFSET)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        (POPJ-AFTER-NEXT (M-A) READ-MEMORY-DATA)
       ((M-MICR-OFFSET) ADD M-MICR-OFFSET A-1)

GET-NEXT-PAGE
        ((M-MICR-OFFSET) SETZ)
        (JUMP-GREATER-OR-EQUAL M-ZERO A-MICR-N-BLOCKS ERROR-END-OF-PARTITION)
        ((A-MICR-N-BLOCKS) ADD M-ONES A-MICR-N-BLOCKS)  ;Subtract 1
        ((M-TEMP-1) A-MICR-ADDRESS)
        (CALL-XCT-NEXT DISK-READ)
       ((M-A) SETZ)
        (JUMP-XCT-NEXT GET-NEXT-WORD)
       ((A-MICR-ADDRESS) M+A+1 M-ZERO A-MICR-ADDRESS)

;;; Disk commands.

;;; Read one block.
;;; Takes disk block number in M-TEMP-1, phys. mem. address in M-A
;;; Does not clobber M-A.
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
DISK-READ
        ((M-SAVE-A) M-A)
        ((a-save-b) m-b)
        (CALL-XCT-NEXT DISK-OP)
       ((M-TEMP-2) DPB M-ONES (BYTE-FIELD 1 0) A-200)   ;201 READ COMMAND
        ((M-A) M-SAVE-A)
        ((m-b) a-save-b)
        (POPJ)

;This isn't needed in the lambda
;;; Write one block.
;;; Takes disk block number in M-TEMP-1, phys. mem. address in M-A
;;; Does not clobber M-A.  ... actually it does seem to clobber M-A
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
;DISK-WRITE
;       ((M-TEMP-2) A-202)      ;WRITE COMMAND
;       ; drops in.

DISK-OP         ;COMMAND IN M-TEMP-2
        (CALL DISK-OP-LOW)
        (JUMP-NOT-EQUAL M-DISK-RETURN-CODE A-200 ERROR-DISK-ERROR)
        (POPJ)

DISK-OP-LOW
        (jump-if-bit-set (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                                m-proc-switches disk-op-low-1)
        ((VMA-START-READ) A-DISK-REGS)          ;Wait for control ready
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 4) READ-MEMORY-DATA DISK-OP-LOW) ;20 bit
    (ERROR-TABLE AWAIT-DISK-CONTROL-READY)      ;Hangs near here if control hung or absent

disk-op-low-1
        ((m-a) ldb (byte-field 16. 8) m-a a-zero)       ;convert to page number.
        (jump-equal m-a a-1 error-bad-file-format)      ;better not load on top of IOPB.
        ((m-a) add m-a a-base-physical-page)
        ((m-b) ldb (byte-field 22. 10.) m-convert-phys-adr)
        ((m-a) xor m-b a-a)
        ((md)  dpb m-ones (byte-field 1 23.) a-a)       ;enable bit

        ((m-a) a-mapping-register-base)
        ((m-a) ldb (byte-field 8. 0) m-a a-zero)

        ((vma-start-write) dpb m-ones (byte-field 2 8) a-a)     ;page 3, locn 170+1400= 1570
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)       ;do three writes, one for each byte.
        (call-if-page-fault error-page-fault)
        ((vma-start-write) add vma a-400)
        (call-if-page-fault error-page-fault)

        ;fill in iopb
         ; wd0: (errors,status,command-options,command) 10400+command
         ; wd1: (cyl lsb, cyl msb, head, unit)              0
         ; wd2: (sect count lsb, sect count msb, sect lsb, sect msb) 0
         ; wd3: (buf lsb, buf msb, buf xmb, dma count)      ;70002402
         ; wd4: (rel adr lsb, rel adr msb, io adr lsb, io adr msb)   40000
         ; wd5: (iopb link lsb, iopb link msb, iopb llink xmb, reserved) 0

        ((vma) dpb m-ones (byte-field 2 7.) a-40)               ;640

        ((md-start-write) dpb m-temp-2 (byte-field 8 0) a-10400)        ;wd0
        (call-if-page-fault error-page-fault)

        ;; Convert block number in M-TEMP-1 to DISK ADDRESS word.

        (CALL-XCT-NEXT DIV)                     ;quotient is cylinders
       ((M-TEMP-2) A-HEADS-TIMES-BLOCKS)

        ((q-r) add q-r a-disk-cylinder-offset)          ;offset entire world

        ((m-to) dpb q-r (byte-field 8 24.) a-zero)      ;cyl lsb
        ((m-temp-2) ldb q-r (byte-field 8 8) a-zero)
        ((m-to) dpb m-temp-2 (byte-field 8 16.) a-to)   ;cyl msb

        (CALL-XCT-NEXT DIV)                     ;quotient is heads, remainder is sectors.
       ((M-TEMP-2) A-NBLKS)
        ((md) dpb q-r (byte-field 8 8) a-to)    ;head.
        ((vma-start-write) add vma a-1)         ;wd1
        (call-if-page-fault error-page-fault)

        ((m-from) dpb m-temp-1 (byte-field 8 8) a-zero) ;sec lsb
        ((m-temp-2) ldb m-temp-1 (byte-field 8 8) a-zero)
        ((m-from) dpb m-temp-2 (byte-field 8 0) a-from) ;sec msb
        ((m-temp-2) a-1)
        ((md) dpb m-temp-2 (byte-field 8 24.) a-from) ;sector count 1 now for 1024 byte sects
        ((vma-start-write) add vma a-1)         ;wd2
        (call-if-page-fault error-page-fault)

;       ((m-a) dpb m-ones (byte-field 3 21.) a-2)
;       ((m-a) dpb m-ones (byte-field 1 8) a-a)
;       ((md) dpb m-ones (byte-field 1 10.) a-a)        ;70002402
        ((M-TEM) A-MAPPING-REGISTER-BASE)  ;mapping reg used for data.
        ((M-TEM) DPB M-TEM (BYTE-FIELD 10. 2.) A-ZERO)  ;first multibus byte adr for data.n
                                ;shifted -8. (lsb of address is always 0)
        ((M-TEMP-2) DPB M-TEM (BYTE-FIELD 8 16.) a-200)  ;200 is DMA count, dpb in msb
        ((MD) SELECTIVE-DEPOSIT M-TEM (BYTE-FIELD 8 8) A-TEMP-2) ;stick in xsb

        ((vma-start-write) add vma a-1)         ;wd3
        (call-if-page-fault error-page-fault)

        ((MD) DPB M-ONES (BYTE-FIELD 1 14.) A-ZERO) ;40000
        ((VMA-START-WRITE) ADD VMA A-1)     ;wd4
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((MD) A-ZERO)
        ((VMA-START-WRITE) ADD VMA A-1)     ;random cruft
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)

  ;give disk GO command
        (jump-if-bit-set (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                                m-proc-switches share-start)

        ;write the iopb base pointer
        ;get the multibus address of the iopb
        (build-constant m-ones a-zero m-a (eval (* (ldb 0010 640) 4.))) ;offset in page
        ((m-tem) m+a+1 a-mapping-register-base m-zero)  ;mapping reg base+1
        ((m-tem) dpb m-tem (byte-field 10. 10.) a-a)

        ((md) ldb (byte-field 8 16.) m-tem)
        ((vma-start-write m-a) m+a+1 a-disk-regs m-zero)
        (call-if-page-fault error-page-fault)
        ((md) ldb (byte-field 8 8) m-tem)
        ((vma-start-write m-a) m+1 m-a)
        (call-if-page-fault error-page-fault)
        ((md) ldb (byte-field 8 0) m-tem)
        ((vma-start-write m-a) m+1 m-a)
        (call-if-page-fault error-page-fault)

        ((VMA) A-DISK-REGS)
        ((MD-START-WRITE) DPB M-ONES (BYTE-FIELD 1 5) A-3)  ;43
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        (jump disk-wait)
share-start
        ; set runme flag
        ((md) a-1)
        ((vma-start-write) add m-proc-conf-virtual-adr a-%processor-conf-share-runme)
        (call-if-page-fault error-page-fault)

        ; make interrupt 7 in SDU
        ; write 1 into address #xff01c1fc
        ; this is on page 6 of the lisp machine
        ; (+ (* 6 (// 1024. 4)) (// (ldb 0012 #xff01c1fc) 4))
        ((vma-start-write) a-3177)
        (call-if-page-fault error-page-fault)

  ;await response
disk-wait
        ((VMA-START-READ)  DPB M-ONES (BYTE-FIELD 2 7.) A-40)           ;640
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((M-DISK-RETURN-CODE) LDB (BYTE-FIELD 8 16.) READ-MEMORY-DATA)  ;status byte
        (POPJ-EQUAL M-DISK-RETURN-CODE A-202)           ;ERROR, WILL BOMB AT HIGHER LEVEL
        (JUMP-NOT-EQUAL M-DISK-RETURN-CODE A-200 DISK-WAIT)     ;command successfully done

    (ERROR-TABLE AWAIT-DISK-DONE)               ;Hangs near here while waiting for disk
        (POPJ)

;;; Divide two numbers.  This routine taken from UCADR 108.
;;; Dividend in M-TEMP-1, divisor in M-TEMP-2
;;; Quotient In Q-R, remainder in M-TEMP-1
;;; Clobbers A-TEM1.

DIV     (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-TEMP-1 A-ZERO DIV1)
       ((A-TEM1 Q-R) M-TEMP-1)
        ((Q-R) SUB M-ZERO A-TEM1)
DIV1    ((M-TEMP-1) DIVIDE-FIRST-STEP M-ZERO A-TEMP-2)
DIV1A   (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) Q-R ERROR-DIVIDE-BY-ZERO)
(REPEAT 31. ((M-TEMP-1) DIVIDE-STEP M-TEMP-1 A-TEMP-2))
        ((M-TEMP-1) DIVIDE-LAST-STEP M-TEMP-1 A-TEMP-2)
        (JUMP-LESS-OR-EQUAL-XCT-NEXT M-ZERO A-TEM1 DIV2)
       ((M-TEMP-1) DIVIDE-REMAINDER-CORRECTION-STEP M-TEMP-1 A-TEMP-2)
        ((M-TEMP-1) SUB M-ZERO A-TEMP-1)
DIV2    ((A-TEM1) XOR M-TEMP-2 A-TEM1)
        (POPJ-LESS-OR-EQUAL M-ZERO A-TEM1)
        (POPJ-AFTER-NEXT
         (A-TEM1) Q-R)
       ((Q-R) SUB M-ZERO A-TEM1)


COLD-GET-DISK-GEOMETRY-FROM-CMOS
        ((M-A) DPB M-ONES (BYTE-FIELD 4 13.) A-140)     ;START AT 360140
        (CALL XMULTIBUS-READ-8)
        ((M-D) DPB M-ONES (BYTE-FIELD 1 2) A-140)       ;MAKE 144
        (CALL-NOT-EQUAL M-B A-D ERROR-BAD-CMOS) ;d

        ((M-A) ADD M-A A-4)
        (CALL XMULTIBUS-READ-8)
        ((M-D) DPB M-ONES (BYTE-FIELD 2 5) A-11)        ;MAKE 151
        (CALL-NOT-EQUAL M-B A-D ERROR-BAD-CMOS) ;i

        ((M-A) ADD M-A A-4)
        (CALL XMULTIBUS-READ-8)
        ((M-D) DPB M-ONES (BYTE-FIELD 3 4) A-3)         ;MAKE 163
        (CALL-NOT-EQUAL M-B A-D ERROR-BAD-CMOS) ;s

        ((M-A) ADD M-A A-4)
        (CALL XMULTIBUS-READ-8)
        ((M-D) DPB M-ONES (BYTE-FIELD 1 3) A-140)       ;MAKE 150
        ((M-D) ADD M-D A-3)                             ;MAKE 153
        (CALL-NOT-EQUAL M-B A-D ERROR-BAD-CMOS) ;k

        ((M-A) DPB M-ONES (BYTE-FIELD 4 13.) A-200)     ;MAKE 360200
        ((M-A) DPB M-ONES (BYTE-FIELD 2 2) A-A)         ;MAKE 360214
        (CALL XMULTIBUS-READ-8)
        ((A-NBLKS) LDB (BYTE-FIELD 8. 0.) M-B) ; sectors per track
        (POPJ)

; only works on addresses that are multiples of 4
; input address in M-A
; output data in M-B
; clobbers M-C
XMULTIBUS-READ-8
        ((M-A) DPB M-ONES (BYTE-FIELD 8 24.) A-A) ; m-a = m-a + 0xff000000 = sdu-slot-base
        ((m-a) xor m-a a-convert-phys-adr)
        ((MD) M-ZERO)
        ((M-C) DPB M-ONES (BYTE-FIELD 2 4) A-400) ;make 460
        ((M-C) DPB M-ONES (BYTE-FIELD 1 9.) A-C)  ;make 1460
        ((L2-MAP-CONTROL) DPB M-ONES (BYTE-FIELD 1 11.) A-C) ;make 5460, pkt code 1, rw access
        ((M-C) L2-MAP-PHYSICAL-PAGE)
        ((L2-MAP-PHYSICAL-PAGE) LDB M-A (BYTE-FIELD 22. 10.) A-ZERO)
        ((VMA-START-READ) LDB (BYTE-FIELD 8 2) M-A A-ZERO)
        (CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
        ((M-B) LDB (BYTE-FIELD 8 0) MD A-ZERO)  ;SAVE 8 BIT RESULT
        (POPJ-AFTER-NEXT (MD) A-ZERO)
       ((L2-MAP-PHYSICAL-PAGE) M-C)     ;PUT BACK MAP.


;nubus-read and nubus-write work, but aren't needed at the moment
;;doesn't clobber anything except md
;;m-nubus-address contains full 32 bit nubus address
nubus-read
        ((md) a-3400)   ;following inst gives maps time to settle.
        (no-op)
        ((l2-map-control) a-1464)       ;no caching.
        ((l2-map-physical-page) ldb m-nubus-address (byte-field 22. 10.) a-zero)
        ((vma-start-read) ldb (byte-field 8 2) m-nubus-address a-3400)
        (call-if-page-fault error-page-fault)
        (popj)

;nubus-write
;       ((a-nubus-write-temp) md)
;        ((md) a-3400)  ;following inst gives maps time to settle.
;       (no-op)
;       ((l2-map-control) a-1464)       ;no caching.
;       ((l2-map-physical-page) ldb m-nubus-addres (byte-field 22. 8.) a-zero)
;       ((md) a-nubus-write-temp)
;       ((vma-start-write) ldb (byte-field 8 0) m-nubus-addres a-3400)
;       (call-if-page-fault error-page-fault)
;       (popj)
;
convert-md-to-8086
        ((m-temp-1) ldb (byte-field 4 0) md a-zero)
        (popj-after-next (md) ldb (byte-field 16. 4) md a-zero)
       ((md) dpb md (byte-field 16. 16.) a-temp-1)
))

;;; -*- Mode:Lisp; Package:LAMBDA; Base:8; Readtable:ZL -*-

;;;Copyright (c) LISP Machine, Inc. 1984
;;;   See filename "Copyright" for
;;;licensing and release information.

;;; New lambda bootstrap: 10-Apr-86 Pace

;;; SDU must loads this code and the initial A-memory.
;;; Note that since there are some initialized a-memory variables
;;; above the end of M memory, all of M memory will be written by
;;; the sdu, and therefore will have good parity.  This program
;;; clears the rest of A memory.  In order to do this, the sdu
;;; writes the highest a-address initialized into 1@A


;;; There are many places in this code in which cycles are wasted.
;;; This is to make the code clearer, since it is far more important
;;; that code in a PROM be bug-free than that the boot sequence be
;;; 150 nanoseconds faster.

(SETQ bootstrap2 '(

(LOCALITY M-MEM)

M-GARBAGE	(0)
M-highest-used-a-mem	(0)			;initialized by SDU
M-ZERO		(0)
M-minus-ONE	(-1)

M-A		(0)
M-B		(0)
M-C		(0)
M-D		(0)
M-FROM		(0)
M-TO		(0)

M-TEMP-1	(0)
M-TEMP-2	(0)

M-MICR-OFFSET	(0)
M-SAVE-A	(0)
M-DISK-RETURN-CODE (0)
M-TEM		(0)

m-sys-conf-virtual-adr	(0)
m-proc-conf-virtual-adr (0)
m-proc-switches (0)
m-share-struct-virtual-adr (0)

m-nubus-address (0)


(LOCALITY A-MEM)

A-GARBAGE	(0)
A-highest-used-a-mem		(0)		;initialized by sdu
A-ZERO		(0)
A-ONES		(0)

A-A		(0)
A-B		(0)
A-C		(0)
A-D		(0)
A-FROM		(0)
A-TO		(0)

A-TEMP-1	(0)
A-TEMP-2	(0)

A-MICR-OFFSET	(0)
A-SAVE-A	(0)
A-DISK-RETURN-CODE (0)
A-TEM		(0)

a-sys-conf-virtual-adr	(0)
a-proc-conf-virtual-adr (0)
a-proc-switches (0)
a-share-struct-virtual-adr (0)

a-nubus-address (0)


(LOC 40)

a-save-b	(0)

a-%processor-conf-share-runme (0)

a-proc-conf-nubus-adr (0)
a-sys-conf-nubus-adr (0)
a-max-iopbs (0)

;;; Disk Characteristics (from label)
A-NCYLS		(0)	;Total number of cylinders
A-NHEADS	(0)	;Number of data heads (i.e. tracks per cylinder)
A-NBLKS		(0)	;Number of blocks ("sectors") per track
A-HEADS-TIMES-BLOCKS	(0)	;Number of blocks per cylinder
A-MICR-ORIGIN	(0)	;Base disk address of microcode partition
A-MICR-ADDRESS	(0)	;Address of next block of microcode partition
A-MICR-N-BLOCKS	(0)	;Remaining blocks of microcode partition
A-TEM1		(0)	;Temporary used in division

A-DISK-REGS	(0)	;Virtual address of disk control registers
			; (points to multibus io space, via page set up for byte xfers!)
  ;this assumes eagle is already initialized (UIB hacked, etc).

a-base-physical-page (0) ;NUBUS page number of main memory locn "0"
a-disk-cylinder-offset (0)  ;offset all refs by yah much

a-valid-flag-virtual-adr (0)

a-lmc-part-name (0)		;if non-zero, use this.  loaded from config structure.
a-mapping-register-base (0)	;initialized to 570 when building constants.
				;  may be changed when config structure loaded.
	;we actually use mapping registers offset 0, 1 and 2 from this.


(DEF-DATA-FIELD OAL-BYTL-1 6 6)		;MICRO INSTRUCTION FIELDS
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

(LOC 36000)
BEG
	(JUMP beg halt-lambda)
	(jump go-with-proc-conf-nubus-adr-in-q-r)
	(no-op halt-lambda)
	(no-op halt-lambda)

;;; Error halts - starting at address 4
;;; Put a NO-OP after each error halt so that they will be at even addresses
;;; These values are published in field service manuals, so don't make them change

PROM-IS-DONE			;4
	(JUMP HALT-LAMBDA PROM-IS-DONE)
    (ERROR-TABLE PROM-IS-DONE)
	(NO-OP)

ERROR-ADD-LOSES			;6
	(JUMP HALT-LAMBDA ERROR-ADD-LOSES)
    (ERROR-TABLE ERROR-ADD-LOSES)
	(NO-OP)

ERROR-A-MEM			;10
	(JUMP HALT-LAMBDA ERROR-A-MEM)
    (ERROR-TABLE ERROR-A-MEM)
	(NO-OP)

ERROR-M-MEM			;12
	(JUMP HALT-LAMBDA ERROR-M-MEM)
    (ERROR-TABLE ERROR-M-MEM)
	(NO-OP)

ERROR-LEVEL-1-MAP		;14
	(JUMP HALT-LAMBDA ERROR-LEVEL-1-MAP)
    (ERROR-TABLE ERROR-LEVEL-1-MAP)
	(NO-OP)

ERROR-PAGE-FAULT		;16
	(JUMP HALT-LAMBDA ERROR-PAGE-FAULT)
    (ERROR-TABLE ERROR-PAGE-FAULT)
	(NO-OP)

ERROR-BAD-LABEL			;20
	(JUMP HALT-LAMBDA ERROR-BAD-LABEL)
    (ERROR-TABLE ERROR-BAD-LABEL)
	(NO-OP)

ERROR-NO-MICR			;22
	(JUMP HALT-LAMBDA ERROR-NO-MICR)
    (ERROR-TABLE ERROR-NO-MICR)
	(NO-OP)

ERROR-BAD-SECTION-TYPE		;24
	(JUMP HALT-LAMBDA ERROR-BAD-SECTION-TYPE)
    (ERROR-TABLE ERROR-BAD-SECTION-TYPE)
	(NO-OP)

ERROR-BAD-ADDRESS		;26
	(JUMP HALT-LAMBDA ERROR-BAD-ADDRESS)
    (ERROR-TABLE ERROR-BAD-ADDRESS)
	(NO-OP)

ERROR-END-OF-PARTITION		;30
	(JUMP HALT-LAMBDA ERROR-END-OF-PARTITION)
    (ERROR-TABLE ERROR-END-OF-PARTITION)
	(NO-OP)

ERROR-BAD-FILE-FORMAT		;32
	(JUMP HALT-LAMBDA ERROR-BAD-FILE-FORMAT)
    (ERROR-TABLE ERROR-BAD-FILE-FORMAT)
	(NO-OP)

ERROR-DISK-ERROR		;34
	(JUMP HALT-LAMBDA ERROR-DISK-ERROR)
    (ERROR-TABLE ERROR-DISK-ERROR)
	(NO-OP)

ERROR-DIVIDE-BY-ZERO		;36
	(JUMP HALT-LAMBDA ERROR-DIVIDE-BY-ZERO)
    (ERROR-TABLE ERROR-DIVIDE-BY-ZERO)
	(NO-OP)

ERROR-PDL-BUFFER		;40
	(JUMP HALT-LAMBDA ERROR-PDL-BUFFER)
    (ERROR-TABLE ERROR-PDL-BUFFER)
	(NO-OP)

ERROR-BAD-MINI-LABEL		;42
	(JUMP HALT-LAMBDA ERROR-BAD-MINI-LABEL)
    (ERROR-TABLE ERROR-BAD-MINI-LABEL)
	(NO-OP)

ERROR-BAD-CMOS			;44
	(JUMP HALT-LAMBDA ERROR-BAD-CMOS)
    (ERROR-TABLE ERROR-BAD-CMOS)
	(NO-OP)

ERROR-BAD-CONF-STRUCTURE	;46
	(JUMP HALT-LAMBDA ERROR-BAD-CONF-STRUCTURE)
    (ERROR-TABLE ERROR-BAD-CONF-STRUCUTRE)
	(NO-OP)

ERROR-BAD-BIT			;50
	(JUMP HALT-LAMBDA ERROR-BAD-BIT)
    (ERROR-TABLE ERROR-BAD-BIT)
	(NO-OP)

error-bad-cram-adr-map		;52
	(jump halt-lambda error-bad-cram-adr-map)
    (error-table error-bad-cram-adr-map)
	(no-op)


go-with-proc-conf-nubus-adr-in-q-r
	((a-proc-conf-nubus-adr) q-r)

	((M-ZERO Q-R) SETZ)		;Make all zeros. Result to Q-R, not to depend on M mem
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

	((M-minus-ONE Q-R) SETO)		;Make all ones, in Q-R not to trust M Mem
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
	(JUMP-NOT-EQUAL Q-R A-minus-ONE ERROR-A-MEM)
	(JUMP-NOT-EQUAL M-minus-ONE A-ONES ERROR-M-MEM)
	((Q-R) SETZ)
	(JUMP-NOT-EQUAL Q-R A-ZERO ERROR-A-MEM)
	(JUMP-NOT-EQUAL M-ZERO A-ZERO ERROR-M-MEM)
;;; See if all carries in ALU really carry.
	((Q-R) ADD M-minus-ONE A-ZERO ALU-CARRY-IN-ONE)
	(JUMP-NOT-EQUAL Q-R A-ZERO ERROR-ADD-LOSES)
;;; Another simple carry test
	((Q-R) ADD M-minus-ONE A-minus-ONE)
	(JUMP-IF-BIT-SET (BYTE-FIELD 1 0) Q-R ERROR-ADD-LOSES)
	(JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 1) Q-R ERROR-ADD-LOSES)
	(JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 37) Q-R ERROR-ADD-LOSES)
;;; Prepare to test pdl buffer.
	((C-PDL-BUFFER-POINTER-PUSH) M-ZERO)
	((C-PDL-BUFFER-POINTER-PUSH) M-minus-ONE)
;;; This verifies that -1 + -1 is -2 and also tests the byte hardware a little
	((MD) (BYTE-FIELD 37 1) Q-R A-minus-ONE)
	(JUMP-NOT-EQUAL MD A-minus-ONE ERROR-ADD-LOSES)
;;; Foo, the byte hardware could be tested a little bit better than that!
  	(JUMP-NOT-EQUAL C-PDL-BUFFER-POINTER-POP A-minus-ONE ERROR-PDL-BUFFER)
  	(JUMP-NOT-EQUAL C-PDL-BUFFER-POINTER-POP A-ZERO ERROR-PDL-BUFFER)

CLEAR-A-MEMORY
	((OA-REG-LOW) DPB M-highest-used-a-mem OAL-A-DEST A-ZERO)	;A destination
	((A-GARBAGE) setz)
	((M-highest-used-a-mem) M+A+1 M-highest-used-a-mem A-ZERO)
	(JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 12.) M-highest-used-a-mem CLEAR-A-MEMORY)

	((dp-mode) m-zero)		;select M memory via pdl-buffer
	((pdl-buffer-pointer) (a-constant 77))
clear-shadowed-pdl-buffer
  ;Notes:  when pdl buffer pointer is 0, 0@m has been written, writing good parity there.
  ;  Also clears entire shadowwed memory area.
  ;  If bits 31.-11. of pdl pointer fail to read 0 (a hardware error which has sometimes
  ;     been seen), it will hang in a loop here.
  ;  DP-MODE is reselected below.
	((c-pdl-buffer-pointer-push) m-zero)
	(jump-not-equal pdl-buffer-pointer a-zero clear-shadowed-pdl-buffer)

	((DP-MODE) (a-constant 1)))		;SET HIGH BIT OF PDL ADDRESS TO AVOID M-MEMORY
	((rg-mode) setz)			;SET 25 BIT ADDRESS MODE & set other bits

;;; Clear some more memories
	((pdl-buffer-pointer m-a) (a-constant 2048.))	;size of pdl buffer
CLEAR-PDL-BUFFER
	((C-PDL-BUFFER-POINTER-PUSH) M-ZERO)
	((M-A) sub M-A (a-constant 1))
	(JUMP-NOT-EQUAL M-A A-ZERO CLEAR-PDL-BUFFER)

	((M-A) (A-constant 400))			;Size of micro-PC stack
CLEAR-micro-stack
	((MICRO-STACK-DATA-PUSH) SETZ)
	((M-A) sub M-A (a-constant 1))
	(JUMP-NOT-EQUAL M-A A-ZERO CLEAR-micro-stack)

	((MD M-A) SETZ)
CLEAR-LEVEL-1-MAP
	((L1-MAP) M-A)	;Clear level-1 map
	((MD) ADD MD (A-constant 20000))
	(JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 25.) MD CLEAR-LEVEL-1-MAP)

	((MD) SETZ)
CLEAR-LEVEL-2-MAP
	((L1-MAP) (BYTE-FIELD 7 13.) MD)	 ;Make level-1 map point at level-2
	((L2-MAP-CONTROL) SETZ)	;Clear level-2 map
	((L2-MAP-PHYSICAL-PAGE) SETZ)
	((MD) ADD MD (A-constant 400))
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

	((m-a) (a-constant 7777))		;page number
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
 ;	((oa-reg-low) dpb m-a oal-cram-page-number a-zero)
 ;	(call-xct-next)
 ;	(popj-after-next (m-d) ldb (byte-field 12. 0) cram-adr-map)
 ;	(jump-not-equal m-d a-b error-bad-cram-adr-map)

	(jump-greater-xct-next m-a a-zero load-straight-cram-adr-map)
       ((m-a) sub m-a (a-constant 1))

	((M-A MD) SETZ)
	((m-c) (a-constant (eval (dpb -1 (byte 4 28.) 0))))	;HIGH WORD OF IZERO-GOOD-PARITY
CLEAR-I-MEMORY
	((OA-REG-LOW) DPB MD OAL-JUMP A-ZERO)
	(CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT
	(CRAM-HIGH) M-C)

	((OA-REG-LOW) DPB MD OAL-JUMP A-ZERO)
	(CALL-XCT-NEXT 0)
       (POPJ-AFTER-NEXT
	(CRAM-LOW) SETZ)

	((MD) ADD MD (A-constant 1))
	(JUMP-LESS-THAN MD (A-constant 36000) CLEAR-I-MEMORY)	;RAM, clear to beginning of bootstrap

clear-mid-memory	;clear it in 4 sections by frobbing the HIGH.MID.ADR bits in RG-MODE
	((m-a) m-zero)
clear-mid-section
	((m-b) dpb m-a (byte-field 10. 6) a-zero)
	((md) dpb m-b (byte-field 20 20) a-b)
	(source-to-macro-ir md)
	((macro-ir-decode) m-zero)
	((m-a) add m-a (a-constant 1))
	(jump-if-bit-clear (byte-field 1 10.) m-a clear-mid-section)
	((m-a) ldb (byte-field 2 34) rg-mode a-zero)
	((m-a) add m-a (a-constant 1))
	((m-tem) rg-mode)
	((rg-mode) dpb m-a (byte-field 2 34) a-tem)
	(jump-if-bit-clear (byte-field 1 2) m-a clear-mid-memory)
			;HIGH.MID.ADR bits have been set back to zero for normal operation.

	((rg-mode) setz)


clear-main-memory
	((vma) (a-constant (eval %processor-conf-memory-base-0)))
	(call read-proc-conf)
	((a-base-physical-page) ldb (byte-field 22. 10.) md)

	((vma) (a-constant (eval %processor-conf-memory-bytes-0)))
	(call read-proc-conf)
	((md) sub md (a-constant 2048.))	;SDU does first two pages
	((m-b) ldb (byte-field 30. 2) md)	;convert to words

	((m-a) a-base-physical-page)
	((m-a) dpb m-a (byte 22. 10.) a-zero)
	((m-a) add m-a (a-constant 2048.))	;SDU does first two pages

clear-main-memory-loop
	((vma) m-a)
	((md) setz)
	(call nubus-write)
	((m-a) add m-a (a-constant 4))
	((m-b) sub m-b (a-constant 1))
	(jump-not-equal m-b a-zero clear-main-memory-loop)


;;; Configuration and options

get-configuration
	((vma) (a-constant (eval %processor-conf-sys-conf-ptr)))
	(call read-proc-conf)
	((a-sys-conf-nubus-adr) md)

	((vma) (a-constant (eval %system-configuration-version-number)))
	(call read-sys-conf)
	(call-not-equal md (a-constant 1) error-bad-conf-structure)

read-processor-block
	((vma) (a-constant (eval %processor-conf-starting-processor-switches)))
	(call read-proc-conf)
	((m-processor-switches) md)

	((vma) (a-constant (eval %processor-conf-micro-band)))
	(call read-proc-conf)
	((a-lmc-part-name) md)

	((vma) (a-constant (eval %processor-conf-base-multibus-mapping-register))))
	(call read-proc-conf)
	((a-mapping-register-base) md)


;;; mapping register 0 -> phys mem 0      for IOPB
;;; mapping register 1 & 2 -> proc conf   for share iopb
;;; mapping register 3 -> moves around

set-up-disk-mapping-regs
	;;write register 0
	((md) dpb m-ones (byte-field 1 23.) a-base-physical-page)	;enable bit
	((m-a) a-mapping-register-base)
	(call write-multibus-mapping-register)

	;;write register 1
	((m-tem) a-proc-conf-nubus-address)
	((m-tem) add m-tem (a-constant (eval (* 4 %processor-conf-share-runme))))
	((m-tem) ldb m-tem (byte 22. 10.) a-zero)
	((md) dpb m-minus-one (byte-field 1 23.) a-tem)
	((m-a) a-mapping-regiseter)
	((m-a) add m-a (a-constant 1))
	(call write-multibus-mapping-register)

	;;write register 2
	((md) a-proc-conf-nubus-address)
	((md) add md (a-constant (eval (+ 1024. (* 4 %processor-conf-share-runme)))))
	((md) ldb md (byte 22. 10.) a-zero)
	((m-a) a-mapping-register)
	((m-a) add m-a (a-constant 2))
	(call write-multibus-mapping-register)

(begin-comment) (end-comment)
init-share-iopb
	;initialize share-iopb structure
	((md) a-zero)
	((vma) (a-constant (eval (* 4 %processor-conf-share-runme))))
	(call write-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-runme))))
	(call write-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-interrupt-addr))))
	(call write-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-spare-1))))
	(call write-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-spare-2))))
	(call write-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-spare-3))))
	(call write-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-spare-4))))
	(call write-proc-conf)

	((vma) (a-constant (eval (* 4 %processor-conf-slot-number))))
	(call read-proc-conf)
	((vma) (a-constant (eval (* 4 %processor-conf-share-slot))))
	(call write-proc-conf)

	((md) (a-constant 3))
	((vma) (a-constant (eval (* 4 %processor-conf-share-type))))
	(call write-proc-conf)

	;;;IOPB will be at physical address 0, and we will point to with with mapping register 0
	((md) a-mapping-register-base)		;a mapping register number
	((md) dpb md (byte 22. 10.) a-zero)	;convert to byte address
	(call convert-md-to-8086)		;now md is a 8086 pointer to beginning of that page
	((vma) (a-constant (eval (* 4 %processor-conf-share-iopb))))
	(call write-proc-conf)

share-iopb-hookup
	;;do the work to link into the iopb "chain"
	((vma) (a-constant (eval (* 4 %system-configuration-share-struct-pointer))))
	(call read-sys-conf)
	((m-a) md)

	((vma) m-a)
	((vma) add vma (a-constant (eval (* 4 %share-struct-max-iopbs))))
	(call nubus-read)
	((a-max-iopbs) md)

	;m-b is the iopb slot number counter
	;m-d is nubus physical address of base of valid table
	((m-b) setz)
	((m-d) m-a)
	((m-d) add m-a (a-constant (eval (* 4 %share-struct-start-of-valid-table))))

find-empty-entry
	((m-tem) dpb m-b (byte 30. 2) a-zero)
	((vma) add m-d a-tem)
	(call nubus-read)
	(jump-equal md a-zero found-empty-entry)
	((m-b) add m-b a-1)
	(call-equal m-b a-max-iopbs error-no-free-share-iopb-slot)
	(jump find-empty-entry)

found-empty-entry
	;; now m-b is number of an empty share-iopb slot
	;; we have to store an 8086 pointer to our share-iopb into that slot
	((m-b) dpb m-b (byte 30. 2) a-zero)	;convert from word offset to byte offset
	((a-valid-flag-nubus-physical-address) add m-d a-b)

	((m-c) a-proc-conf-nubus-address)
	((m-c) add m-c (a-constant (eval (* 4 %processor-conf-share-runme))))
	((m-tem) a-mapping-register-base)
	((m-tem) add m-tem (a-constant 2))		;mapping register 2 (& 3) is for share-iopb
	((m-tem) dpb m-tem (byte 10. 10.) a-zero)
	((md) dpb m-c (byte 10. 0) a-tem)
	(call convert-md-to-8086)

	;; now md is the approiate pointer, store it in the iopb table
	((m-tem) a-max-iopbs)
	((m-tem) dpb m-tem (byte 30. 2) a-zero)
	((vma) add m-d a-tem)			;physical address of beginning of share-iopb table
	((vma) add vma a-b)			;add offset to open slot
	(call nubus-write)

	;; set the valid flag
	((md) (a-constant 1))
	((vma) a-valid-flag-nubus-physical-address)
	(call nubus-write)

read-new-mini-label
	...


;;; Read the label (block 0) into physical memory page 1-4, get disk
;;; parameters, and find out where the microcode partition is.
READ-LABEL
	((m-a) (a-constant 0))			;disk address
	((m-b) (a-constant 1))			;core address
	(call read-disk-block)

	((m-a) (a-constant 2))			;disk address
	((m-b) (a-constant 2))			;core address
	(call read-disk-block)

	((m-a) (a-constant 4))			;disk address
	((m-b) (a-constant 3))			;core address
	(call read-disk-block)

	((m-a) (a-constant 6))			;disk address
	((m-b) (a-constant 4))			;core address
	(call read-disk-block)

DECODE-LABEL
	;;label check word
	((vma) (a-constant 0))
	(call ref-disk-buffer)
	(jump-not-equal md (a-constant (eval (logior #/L
						     (ash #/A 8)
						     (ash #/B 16.)
						     (ash #/L 24.))))
			ERROR-BAD-LABEL)
	;;label version number
	((vma) (a-constant 1))
	(call ref-disk-buffer)
	(jump-not-equal md (a-constant 1) error-bad-label)
	((vma) add vma (a-constant 1))		;2
	(call ref-disk-buffer)
	((a-ncyls) md)
	((vma) add vma (a-constant 1))		;3
	(call ref-disk-buffer)
	((a-nheads) md)
	((vma) add vma (a-constant 1))		;4
	(call ref-disk-buffer)
	((a-nblks) md)
	((vma) add vma (a-constant 1))		;5
	(call ref-disk-buffer)
	((a-heads-times-blocks) md)
	((vma) add vma (a-constant 1))		;6
	(call ref-disk-buffer)
	((m-b) md)				;name of microload partition (old-style, but still used a lot)
	(jump-equal m-zero a-lmc-part-name decode-label-1)
	((m-b) a-lmc-part-name)			;use microload from config structure.
decode-label-1
	((vma) (a-constant 200))
	(call ref-disk-buffer)
	((M-D) md)				;M-D gets number of partitions
	((vma) add vma (a-constant 1))		;201
	(call ref-disk-buffer)
	((M-A) md)				;M-A gets words per partition descriptor
	((M-C) ADD vma (A-constant 1))		;202 (start of partition table)


	;; M-B/ Name of microload partition.
	;; M-C/ Address of partition descriptor.
	;; M-D/ Number of partitions
	;; M-A/ Number of words per partition descriptor.
SEARCH-LABEL
	(JUMP-EQUAL M-D A-ZERO ERROR-NO-MICR)
	((VMA) M-C)
	(call ref-disk-buffer)
	(JUMP-EQUAL md A-B FOUND-PARTITION)
	((M-C) ADD M-C A-A)
	(JUMP-XCT-NEXT SEARCH-LABEL)
       ((M-D) SUB M-D A-1)

FOUND-PARTITION
	((vma m-c) add m-c (a-constant 1))
	(call ref-disk-buffer)
	((A-MICR-ADDRESS) md)
	((A-MICR-ORIGIN) A-MICR-ADDRESS)
	((vma) add m-c (a-constant 1))
	(call ref-disk-buffer)
	((A-MICR-N-BLOCKS) md)

	((vma) (a-constant (eval (* 4 %processor-conf-memory-bytes-0))))
	(call read-proc-conf)
	((md) ldb (byte 22. 10.) md)
	(jump-greater-or-equal md a-micr-n-blocks micr-size-ok)
	((a-micr-n-blocks) md)
micr-size-ok

	((m-a) a-micr-address)			;starting disk address
	((m-b) (a-constant 3))			;starting core address
	((m-c) a-micr-n-blocks)

read-partition-loop
	(jump-equal m-c a-zero read-partition-loop-done)
	(call read-disk-block)
	((m-a) add m-a (a-constant 1))
	((m-b) add m-b (a-constant 1))
	((m-c) sub m-c (a-constant 1))
	(jump read-partition-loop)

read-partition-loop-done
	((m-tem) a-base-physical-page)
	((a-buffer-pointer) add m-tem (a-constant 1024.))

;;; Process one section.  Each section starts with three words:
;;; The section type, the initial address, and the number of locations.
;;; These are gotten into M-B, M-C, and M-D; then the section type is
;;; "dispatched" on.
;;; Section codes are:
;;; 1 = I-MEM, 2 = D-MEM (obsolete), 3 = MAIN-MEM, 4 = A-M-MEM, 5 MACRO-INSTRUCTION-DECODE
PROCESS-SECTION
	(CALL GET-NEXT-WORD)
	(CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-B) M-A)			;gets section code (first word)
	(CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-C) M-A)			;gets starting adr (second word)
	((M-D) M-A)			;size (third word)
	(JUMP-EQUAL M-B A-1 PROCESS-I-MEM-SECTION)
	(JUMP-EQUAL M-B A-3 PROCESS-MAIN-MEM-SECTION)
	(JUMP-EQUAL M-B A-4 PROCESS-A-MEM-SECTION)
	(JUMP-EQUAL M-B A-5 PROCESS-MID-SECTION)
	(JUMP ERROR-BAD-SECTION-TYPE)

PROCESS-I-MEM-SECTION
	(JUMP-NOT-EQUAL M-C A-ZERO ERROR-BAD-ADDRESS)
PROCESS-I-MEM-SECTION-0
	(JUMP-EQUAL M-D A-ZERO PROCESS-SECTION)		;count exhausted
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
       (POPJ-AFTER-NEXT (CRAM-HIGH) M-A)	;write high word first to guard against
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
	((rg-mode) dpb md (byte-field 2 34) a-temp-2)	;set up high address bits.
	((M-TEMP-1) DPB M-C (BYTE-FIELD 20. 6) A-ZERO)
	((MD) DPB M-TEMP-1 (BYTE-FIELD 20 20) A-TEMP-1)
	(SOURCE-TO-MACRO-IR MD)
	((MACRO-IR-DECODE) M-A)
	((rg-mode) dpb m-zero (byte-field 2 34) a-temp-2) ;set high MID bits to zero in case
	(JUMP-XCT-NEXT PROCESS-MID-SECTION)		  ; this is last time thru.
       ((M-C) ADD M-C A-1)


PROCESS-MAIN-MEM-SECTION
	(CALL GET-NEXT-WORD)
	;;; M-C/ Number of blocks.
	;;; M-D/ Address of first block, relative to beginning of partition.
	;;; M-A/ Physical memory address of first word.
	((m-e) a-base-physical-page)
	((m-e) dpb m-tem (byte 22. 10.) a-zero)

	((m-tem) add m-d (a-constant 1))
	((m-tem) dpb m-tem (byte 22. 10.) a-zero)
	((a-main-mem-start) add m-tem a-e)

	((a-main-mem-n-words) dpb m-c (byte 24. 8) a-zero)

	((m-a) dpb m-a (byte 30. 2) a-zero)
	((a-main-mem-dest) add m-a a-tem)
	(jump process-section)

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
A-MEM-LOOP-2		;high half is loaded directly into A-MEM, not via PDL-BUFFER.
	(JUMP-EQUAL M-D A-ZERO DONE-LOADING)
	(CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-D) SUB M-D A-1)
	((OA-REG-LOW) DPB M-C OAL-A-DEST A-ZERO)
	((A-GARBAGE) M-A)
	((M-C) ADD M-C A-1)
	(JUMP A-MEM-LOOP-2)

DONE-LOADING
	;remove prom from IOPB table
	((md) setz)
	((vma) a-valid-flag-nubus-physical-adr)
	(call nubus-write)

	((m-a) a-main-mem-start)
	((m-b) a-main-mem-n-words)
	((m-c) a-main-mem-dest)

done-loading-2
	(jump-equal m-b a-zero done-loading-1)
	((vma) m-a)
	(call nubus-read)
	((vma) m-c)
	(call nubus-write)
	((m-a) add m-a (a-constant 4))
	((m-b) sub m-b (a-constant 1))
	((m-c) add m-c (a-constant 4))
	(jump done-loading-2)

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

jump-to-cold-boot
	;m-proc-switches was saved in md before filling a and m memories
	(jump-if-bit-set md (lisp-byte %%processor-switch-prom-jumps-to-cold-boot)
			cold-boot-start-addr) ;conf ptr in q-r

	(jump prom-is-done)


GET-NEXT-WORD
	((vma) a-buffer-pointer)
	((a-buffer-pointer) add vma (a-constant 4))
	(call nubus-read)
	(popj)


;;; Disk commands.

;;; m-a disk-address in pages
;;; m-b core-address in pages
read-disk-block
	((m-disk-a) m-a)
	((m-disk-b) m-b)
	((m-tem) dpb m-disk-b (byte 22. 10.) a-zero)
	((m-tem) add m-tem a-base-physical-page)
	((m-tem) ldb (byte 22. 10.) a-zero)
	((m-tem) dpb m-minus-one (byte 1 23.) a-zero)
	((m-a) (a-constant 3))
	(call write-multibus-mapping-register)

;;; Read one block.
;;; Takes disk block number in M-TEMP-1, phys. mem. address in M-A
;;; Does not clobber M-A.
;;; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
DISK-READ
	((M-SAVE-A) M-A)
	((a-save-b) m-b)
	(CALL-XCT-NEXT DISK-OP)
       ((M-TEMP-2) DPB M-ONES (BYTE-FIELD 1 0) A-200)	;201 READ COMMAND
	((M-A) M-SAVE-A)
	((m-b) a-save-b)
	(POPJ)

DISK-OP		;COMMAND IN M-TEMP-2
	(CALL DISK-OP-LOW)
	(JUMP-NOT-EQUAL M-DISK-RETURN-CODE A-200 ERROR-DISK-ERROR)
	(POPJ)

DISK-OP-LOW
	((m-a) ldb (byte-field 16. 8) m-a a-zero)	;convert to page number.
	(jump-equal m-a a-1 error-bad-file-format)      ;better not load on top of IOPB.
	((m-a) add m-a a-base-physical-page)
	((m-b) ldb (byte-field 22. 10.) m-convert-phys-adr)
	((m-a) xor m-b a-a)
	((md)  dpb m-ones (byte-field 1 23.) a-a)	;enable bit

	((m-a) a-mapping-register-base)
	((m-a) ldb (byte-field 8. 0) m-a a-zero)

	((vma-start-write) dpb m-ones (byte-field 2 8) a-a)	;page 3, locn 170+1400= 1570
	(call-if-page-fault error-page-fault)
	((vma-start-write) add vma a-400)	;do three writes, one for each byte.
	(call-if-page-fault error-page-fault)
	((vma-start-write) add vma a-400)
	(call-if-page-fault error-page-fault)

	;fill in iopb
	 ; wd0: (errors,status,command-options,command) 10400+command
	 ; wd1: (cyl lsb, cyl msb, head, unit)		    0
	 ; wd2: (sect count lsb, sect count msb, sect lsb, sect msb) 0
	 ; wd3: (buf lsb, buf msb, buf xmb, dma count)	    ;70002402
	 ; wd4: (rel adr lsb, rel adr msb, io adr lsb, io adr msb)   40000
	 ; wd5: (iopb link lsb, iopb link msb, iopb llink xmb, reserved) 0

	((vma) dpb m-ones (byte-field 2 7.) a-40)		;640

	((md-start-write) dpb m-temp-2 (byte-field 8 0) a-10400)	;wd0
	(call-if-page-fault error-page-fault)

	;; Convert block number in M-TEMP-1 to DISK ADDRESS word.

	(CALL-XCT-NEXT DIV)			;quotient is cylinders
       ((M-TEMP-2) A-HEADS-TIMES-BLOCKS)

	((q-r) add q-r a-disk-cylinder-offset)		;offset entire world

	((m-to) dpb q-r (byte-field 8 24.) a-zero)	;cyl lsb
	((m-temp-2) ldb q-r (byte-field 8 8) a-zero)
	((m-to) dpb m-temp-2 (byte-field 8 16.) a-to)	;cyl msb

	(CALL-XCT-NEXT DIV)			;quotient is heads, remainder is sectors.
       ((M-TEMP-2) A-NBLKS)
	((md) dpb q-r (byte-field 8 8) a-to)	;head.
	((vma-start-write) add vma a-1)		;wd1
	(call-if-page-fault error-page-fault)

	((m-from) dpb m-temp-1 (byte-field 8 8) a-zero) ;sec lsb
	((m-temp-2) ldb m-temp-1 (byte-field 8 8) a-zero)
	((m-from) dpb m-temp-2 (byte-field 8 0) a-from) ;sec msb
	((m-temp-2) a-1)
	((md) dpb m-temp-2 (byte-field 8 24.) a-from) ;sector count 1 now for 1024 byte sects
	((vma-start-write) add vma a-1)		;wd2
	(call-if-page-fault error-page-fault)

;	((m-a) dpb m-ones (byte-field 3 21.) a-2)
;	((m-a) dpb m-ones (byte-field 1 8) a-a)
;	((md) dpb m-ones (byte-field 1 10.) a-a)	;70002402
	((M-TEM) A-MAPPING-REGISTER-BASE)  ;mapping reg used for data.
	((M-TEM) DPB M-TEM (BYTE-FIELD 10. 2.) A-ZERO)  ;first multibus byte adr for data.n
				;shifted -8. (lsb of address is always 0)
	((M-TEMP-2) DPB M-TEM (BYTE-FIELD 8 16.) a-200)  ;200 is DMA count, dpb in msb
	((MD) SELECTIVE-DEPOSIT M-TEM (BYTE-FIELD 8 8) A-TEMP-2) ;stick in xsb

	((vma-start-write) add vma a-1)		;wd3
	(call-if-page-fault error-page-fault)

	((MD) DPB M-ONES (BYTE-FIELD 1 14.) A-ZERO) ;40000
	((VMA-START-WRITE) ADD VMA A-1)	    ;wd4
	(CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
	((MD) A-ZERO)
	((VMA-START-WRITE) ADD VMA A-1)	    ;random cruft
	(CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)

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
	((VMA-START-READ)  DPB M-ONES (BYTE-FIELD 2 7.) A-40)		;640
	(CALL-IF-PAGE-FAULT ERROR-PAGE-FAULT)
	((M-DISK-RETURN-CODE) LDB (BYTE-FIELD 8 16.) READ-MEMORY-DATA)	;status byte
	(POPJ-EQUAL M-DISK-RETURN-CODE A-202)		;ERROR, WILL BOMB AT HIGHER LEVEL
	(JUMP-NOT-EQUAL M-DISK-RETURN-CODE A-200 DISK-WAIT)	;command successfully done

    (ERROR-TABLE AWAIT-DISK-DONE)		;Hangs near here while waiting for disk
	(POPJ)

;;; Divide two numbers.  This routine taken from UCADR 108.
;;; Dividend in M-TEMP-1, divisor in M-TEMP-2
;;; Quotient In Q-R, remainder in M-TEMP-1
;;; Clobbers A-TEM1.

DIV	(JUMP-GREATER-OR-EQUAL-XCT-NEXT M-TEMP-1 A-ZERO DIV1)
       ((A-TEM1 Q-R) M-TEMP-1)
	((Q-R) SUB M-ZERO A-TEM1)
DIV1	((M-TEMP-1) DIVIDE-FIRST-STEP M-ZERO A-TEMP-2)
DIV1A	(JUMP-IF-BIT-SET (BYTE-FIELD 1 0) Q-R ERROR-DIVIDE-BY-ZERO)
(REPEAT 31. ((M-TEMP-1) DIVIDE-STEP M-TEMP-1 A-TEMP-2))
	((M-TEMP-1) DIVIDE-LAST-STEP M-TEMP-1 A-TEMP-2)
	(JUMP-LESS-OR-EQUAL-XCT-NEXT M-ZERO A-TEM1 DIV2)
       ((M-TEMP-1) DIVIDE-REMAINDER-CORRECTION-STEP M-TEMP-1 A-TEMP-2)
	((M-TEMP-1) SUB M-ZERO A-TEMP-1)
DIV2	((A-TEM1) XOR M-TEMP-2 A-TEM1)
	(POPJ-LESS-OR-EQUAL M-ZERO A-TEM1)
	(POPJ-AFTER-NEXT
	 (A-TEM1) Q-R)
       ((Q-R) SUB M-ZERO A-TEM1)


nubus-read
        ((md) setz)	;following inst gives maps time to settle.
	((m-nubus-op-vma) vma)
	((l2-map-control) (a-constant 1400))	;no caching.
	((l2-map-physical-page) ldb m-nubus-op-vma (byte-field 22. 10.) a-zero)
	((vma-start-read) ldb (byte 8 2) m-nubus-op-vma)
	(call-if-page-fault error-page-fault)
	((vma) m-nubus-op-vma)
	(popj)

nubus-write
	((m-nubus-op-vma) vma)
        ((md) setz)	;following inst gives maps time to settle.
	((m-nubus-op-md) md)
	((l2-map-control) (a-constant 1400))	;no caching.
	((l2-map-physical-page) ldb m-nubus-op-vma (byte-field 22. 8.) a-zero)
	((md) m-nubus-op-md)
	((vma-start-write) ldb (byte-field 8 2) m-nubus-op-vma)
	(call-if-page-fault error-page-fault)
	((vma) m-nubus-op-vma)
	(popj)

convert-md-to-8086
	((m-temp-1) ldb (byte-field 4 0) md a-zero)
	(popj-after-next (md) ldb (byte-field 16. 4) md a-zero)
       ((md) dpb md (byte-field 16. 16.) a-temp-1)
))

;-*- mode: lisp; base: 8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;
(DEFCONST UC-HACKS '(
;;; Sophisticated audio home entertainment center.

#-exp(begin-comment)
XBEEP (MISC-INST-ENTRY %BEEP)
;;; First argument is half-wavelength, second is duration.  Both are in microseconds.
;;; M-1 has 2nd argument (duration) which is added to initial time-check
;;; M-2 contains most recent time check
;;;     to compute quitting time
;;; M-C contains 1st argument, the wavelength
;;; M-4 contains the time at which the next click must be done.
;;; Note that the 32-bit clock wraps around once an hour, we have to be careful
;;; to compare clock values in the correct way, namely without overflow checking.
        (CALL-XCT-NEXT READ-MICROSECOND-CLOCK)
       ((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-1) M-1 ADD A-2)
        ((M-C) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-4) M-2)
        (jump-equal m-2 a-zero xfalse) ;forget it if the clock hasn't started yet
BEEP-NEXT-CLICK
        ((M-4) M-4 ADD A-C)
BEEP-WAIT
        ((vma-start-read) setz)
        (check-page-read) ;let an interrupt happen
        (CALL READ-MICROSECOND-CLOCK)
        ((M-TEM) SUB M-2 A-1)
        (JUMP-GREATER-OR-EQUAL M-TEM A-ZERO XFALSE)
        ((M-TEM) SUB M-2 A-4)
        (JUMP-LESS-OR-EQUAL M-TEM A-ZERO BEEP-WAIT)
        ;do the click here
        (JUMP BEEP-NEXT-CLICK)
#-exp(end-comment)

#-cadr (begin-comment)
;CADR version
XBEEP (MISC-INST-ENTRY %BEEP)
;;; First argument is half-wavelength, second is duration.  Both are in microseconds.
;;; M-1 has 2nd argument (duration) which is added to initial time-check
;;; M-2 contains most recent time check
;;;     to compute quitting time
;;; M-C contains 1st argument, the wavelength
;;; M-4 contains the time at which the next click must be done.
;;; Note that the 32-bit clock wraps around once an hour, we have to be careful
;;; to compare clock values in the correct way, namely without overflow checking.
        (CALL-XCT-NEXT READ-MICROSECOND-CLOCK)
       ((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-1) M-1 ADD A-2)
        ((M-C) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-4) M-2)
BEEP-NEXT-CLICK
        ((M-4) M-4 ADD A-C)
BEEP-WAIT
        (CALL READ-MICROSECOND-CLOCK)
        ((M-TEM) SUB M-2 A-1)
        (JUMP-GREATER-OR-EQUAL M-TEM A-ZERO XFALSE)
        ((M-TEM) SUB M-2 A-4)
        (JUMP-LESS-OR-EQUAL M-TEM A-ZERO BEEP-WAIT)
        ((VMA-START-WRITE) (A-CONSTANT BEEP-HARDWARE-VIRTUAL-ADDRESS))  ;Unibus 764110
        (CHECK-PAGE-WRITE)
        (JUMP BEEP-NEXT-CLICK)

#-cadr (end-comment)

#-lambda (begin-comment)

(begin-pagable-ucode)

XBEEP (MISC-INST-ENTRY %BEEP)
;;; First argument is half-wavelength, second is duration.  Both are in microseconds.
;;; M-1 has 2nd argument (duration) which is added to initial time-check
;;; M-2 contains most recent time check
;;;     to compute quitting time
;;; M-C contains 1st argument, the wavelength
;;; M-3 contains the word to write to the serial chip for the next click
;;; M-4 contains the time at which the next click must be done.
;;; Note that the 32-bit clock wraps around once an hour, we have to be careful
;;; to compare clock values in the correct way, namely without overflow checking.
;;; M-5 gets code for video-board type.
        (CALL-XCT-NEXT READ-MICROSECOND-CLOCK)
       ((M-1) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-1) M-1 ADD A-2)
        ((M-C) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-4) M-2)
        ((m-5) a-zero)          ;vcmem
        ((m-tem) a-tv-quad-slot)
        ((m-tem) ldb (byte-field 8 8) m-tem)
        (jump-equal m-tem (a-constant 0) xbeep-vcmem)
        (jump-equal m-tem (a-constant 1) xbeep-vcmem)
        (jump-equal m-tem (a-constant 2) xbeep-quad)
        (call illop)

xbeep-quad
        ((m-5) (a-constant 1))
xbeep-vcmem
        ((m-3) (a-constant #xea))  ; normal mode for vcmem serial chip (doesnt matter for quad)
beep-next-click
        (call reset-watchdog)   ;don't let the SDU reset us if this is going to be long
                                ;(also let boot characters come in)
        ((m-4) m-4 add a-c)
beep-wait
        ((vma-start-read) a-zero)               ;allow an interrupt if one is pending
        (check-page-read)                       ;this is important for the 60Hz microsec
                                                ;clock hack
        (call read-microsecond-clock)
        ((m-tem) sub m-2 a-1)
        (jump-greater-or-equal m-tem a-zero beep-done)
        ((m-tem) sub m-2 a-4)
        (jump-less-or-equal m-tem a-zero beep-wait)
        (jump-equal m-5 (a-constant 1) beep-click-on-quad)
beep-click-on-vcmem
        ((md) (a-constant 5))
        ((vma-start-write) (a-constant (plus 177370400 15))) ;vcmem keyboard out control reg
        (check-page-write-map-reload-only)
        ((md-start-write m-3) xor m-3 (a-constant 20)) ; toggle "send break" bit
        (check-page-write)
        (jump beep-next-click)

beep-click-on-quad
        ((vma) (a-constant (plus 177277400 23)))
        ((md-start-write m-3) xor m-3 (a-constant 1)) ; toggle bit
        (check-page-write-map-reload-only)
        (jump beep-next-click)

beep-done
        (jump-equal m-5 (a-constant 1) beep-quad-done)
        ((md) (a-constant 5))
        ((vma-start-write) (a-constant (plus 177370400 15)))
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #xea))
        (check-page-write)
        (jump xfalse)

beep-quad-done
        ((vma) (a-constant (plus 177277400 23)))        ;leave bit clear, randomly.
        ((md-start-write) a-zero)
        (check-page-write-map-reload-only)
        (jump xfalse)

(end-pagable-ucode)

#-lambda (end-comment)


;;; D R A W    T R I A N G L E

;-- this reverted since it lost on explorer.
;;; The only changes necessary for the old DRAW TRIANGLE to use color (since it so
;;; conveniently calls DRAW-RECTANGLE) is to have it call XTVERS9 instead of setting
;;; up the ALU function itself.                                        -rdm 6/19/86

(begin-pagable-ucode)

;;; %DRAW-TRIANGLE X1 Y1 X2 Y2 X3 Y3 ALU SHEET

;;;(define-micro-function X-DRAW-TRIANGLE (x1 y1 x2 y2 x3 y3 alu-function CURRENT-SHEET)
X-DRAW-TRIANGLE (MISC-INST-ENTRY %DRAW-TRIANGLE)
        (CALL SELECT-SHEET)
;       (call xtvers9)
        ((M-J) DPB C-PDL-BUFFER-POINTER-POP OAL-ALUF)   ;M-J ALU function
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 5)
        ;M-1 Y3 sign extended
        ((M-C) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 4)
        ;M-1 X3 sign extended
                ;M-C Y3
        ((M-3) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 3)
        ;M-1 Y2 sign extended
                ;M-3 X3
        ((M-B) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 2)
        ;M-1 X2 sign extended
                ;M-B Y2
        ((M-2) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 1)
        ;M-1 Y1 sign extended
                ;M-2 X2
        ((M-A) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ;M-1 X1 sign extended
                ;M-A Y1
        ((M-1) OUTPUT-SELECTOR-EXTEND-25 C-PDL-BUFFER-POINTER-POP)
        ;;Sort by Y co-ordinate
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-A A-B TV-DRAW-TRI-SORT-1)
       ((M-TEM) M-1)
        ((M-1) M-2)
        ((M-2) M-TEM)
        ((M-TEM) M-A)
        ((M-A) M-B)
        ((M-B) M-TEM)
TV-DRAW-TRI-SORT-1
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-A A-C TV-DRAW-TRI-SORT-2)
       ((M-TEM) M-1)
        ((M-1) M-3)
        ((M-3) M-TEM)
        ((M-TEM) M-A)
        ((M-A) M-C)
        ((M-C) M-TEM)
TV-DRAW-TRI-SORT-2
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-B A-C TV-DRAW-TRI-SORT-3)
       ((M-TEM) M-2)
        ((M-2) M-3)
        ((M-3) M-TEM)
        ((M-TEM) M-B)
        ((M-B) M-C)
        ((M-C) M-TEM)
TV-DRAW-TRI-SORT-3
        ;;Now sorted, Y1 > Y2 > Y3
        ((A-TRI-Y1) M-A)
        ((A-TRI-X1) M-1)
        ((A-TRI-Y2) M-B)
        ((A-TRI-X2) M-2)
        ((A-TRI-Y3) M-C)
        ((A-TRI-X3) M-3)
        ;;Now compute Y co-ordinates as array addresses
        ((Q-R) A-TV-SCREEN-LOCATIONS-PER-LINE)
        (CALL-XCT-NEXT MPY12)
       ((M-1) DPB M-A (BYTE-FIELD 20. 12.) A-ZERO)
        ((A-TRI-Y1-ADDR) SUB M-2 A-TV-SCREEN-LOCATIONS-PER-LINE)
        ((Q-R) A-TV-SCREEN-LOCATIONS-PER-LINE)
        (CALL-XCT-NEXT MPY12)
       ((M-1) DPB M-B (BYTE-FIELD 20. 12.) A-ZERO)
        ((A-TRI-Y2-ADDR) M-2)
        ((Q-R) A-TV-SCREEN-LOCATIONS-PER-LINE)
        (CALL-XCT-NEXT MPY12)
       ((M-1) DPB M-C (BYTE-FIELD 20. 12.) A-ZERO)
        ((A-TRI-Y3-ADDR) M-2)
        ;;Compute determinant to get handedness
        ((M-1) SUB M-3 A-TRI-X1)        ;X3 - X1
        (CALL-XCT-NEXT MPY)
       ((Q-R) SUB M-B A-A)              ;Y2 - Y1
        ((M-3) Q-R)                     ;(X3 - X1) * (Y2 - Y1)
        ((M-1) A-TRI-X1)
        ((M-1) SUB M-1 A-TRI-X2)        ;X1 - X2
        (CALL-XCT-NEXT MPY)
       ((Q-R) SUB M-A A-C)              ;Y1 - Y3
        ((A-TRI-DET) SUB Q-R A-3)       ;((X1 - X2) * (Y1 - Y3)) - ((Y1 - Y2) * (X1 - X3))
        (JUMP-EQUAL-XCT-NEXT M-ZERO A-TRI-DET XFALSE)   ;Colinear, draw nothing
       ((M-1) A-TRI-X1)
        ((M-B) A-TRI-Y1)
        ((M-2) A-TRI-X2)
        (CALL-XCT-NEXT TV-DRAW-TRI-1)
       ((M-C) A-TRI-Y2)
        ((M-1) A-TRI-X1)
        ((M-B) A-TRI-Y1)
        ((M-2) A-TRI-X3)
        (CALL-XCT-NEXT TV-DRAW-TRI-1)
       ((M-C) A-TRI-Y3)
        ((M-A) A-TRI-Y1-ADDR)           ;Initial Y address
        ((A-TRI-Y-LIM) A-TRI-Y2-ADDR)   ;Ending Y address for bottom half
TV-DRAW-TRI-LOOP
        (JUMP-GREATER-OR-EQUAL M-A A-TRI-Y-LIM TV-DRAW-TRI-LOOP-1)
TV-DRAW-TRI-HALF-DONE
        (JUMP-LESS-THAN-XCT-NEXT M-A A-TRI-Y3-ADDR XFALSE)      ;Done with second half
       ((A-TRI-Y-LIM) A-TRI-Y3-ADDR)
        ((M-1) A-TRI-X2)
        ((M-B) A-TRI-Y2)
        ((M-2) A-TRI-X3)
        (CALL-XCT-NEXT TV-DRAW-TRI-1)
       ((M-C) A-TRI-Y3)
TV-DRAW-TRI-LOOP-1
        (JUMP-LESS-THAN-XCT-NEXT M-A A-ZERO TV-DRAW-TRI-SKIP-LINE)
       ((M-D) M-S)              ;Nominal right end
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-R A-ZERO TV-DRAW-TRI-X0-OK)
       ((M-C) M-R)              ;Nominal left end
        ((M-C) SETZ)            ;M-C clipped left end
        (JUMP-GREATER-OR-EQUAL M-D A-C TV-DRAW-TRI-X0-OK)
        ((M-D) SETZ)            ;Right may be to left of clipped left
TV-DRAW-TRI-X0-OK
        ((C-PDL-BUFFER-POINTER-PUSH) M-C)       ;Setup x co-ordinate
        (CALL-XCT-NEXT TVXYAD0)
       ((M-2) M-A)                      ;Setup Y co-ordinate
        (JUMP-GREATER-OR-EQUAL M-E A-TV-SCREEN-BUFFER-END-ADDRESS TV-DRAW-TRI-SKIP-LINE)
        (JUMP-LESS-OR-EQUAL M-D A-TV-SCREEN-WIDTH TV-DRAW-TRI-X1-OK)
        ((M-D) A-TV-SCREEN-WIDTH)       ;M-D clipped right end
        (JUMP-GREATER-OR-EQUAL M-D A-C TV-DRAW-TRI-X1-OK)
        ((M-C) M-D)             ;Left may be to right of clipped right
TV-DRAW-TRI-X1-OK
        ((C-PDL-BUFFER-POINTER-PUSH) SUB M-D A-C)       ;Setup width
        (CALL-XCT-NEXT XTVERS5)
       ((M-D) (A-CONSTANT 1))           ;Height is 1
TV-DRAW-TRI-SKIP-LINE
        ((M-A) SUB M-A A-TV-SCREEN-LOCATIONS-PER-LINE)  ;Y := Y - 1
        ((M-3) SUB M-3 A-TRI-XLIR)      ;XLR := XLR - XLIR
        (JUMP-LESS-THAN-XCT-NEXT M-3 A-ZERO TV-DRAW-TRI-XLR-NEG)
       ((M-R) SUB M-R A-TRI-XLI)        ;XL := XL - XLI
        (JUMP-GREATER-OR-EQUAL M-3 A-TRI-LY TV-DRAW-TRI-XLR-WRAP)
TV-DRAW-TRI-INCR-1
        ((M-4) SUB M-4 A-TRI-XRIR)      ;XRR := XRR - XRIR
        (JUMP-LESS-THAN-XCT-NEXT M-4 A-ZERO TV-DRAW-TRI-XRR-NEG)
       ((M-S) SUB M-S A-TRI-XRI)        ;XR := XR - XRI
        (JUMP-LESS-THAN M-4 A-TRI-RY TV-DRAW-TRI-LOOP)
TV-DRAW-TRI-XRR-WRAP
        ((M-4) SUB M-4 A-TRI-RY)
        (JUMP-XCT-NEXT TV-DRAW-TRI-LOOP)
       ((M-S) ADD M-S (A-CONSTANT 1))
TV-DRAW-TRI-XLR-NEG
        ((M-3) ADD M-3 A-TRI-LY)
        (JUMP-XCT-NEXT TV-DRAW-TRI-INCR-1)
       ((M-R) SUB M-R (A-CONSTANT 1))
TV-DRAW-TRI-XLR-WRAP
        ((M-3) SUB M-3 A-TRI-LY)
        (JUMP-XCT-NEXT TV-DRAW-TRI-INCR-1)
       ((M-R) ADD M-R (A-CONSTANT 1))
TV-DRAW-TRI-XRR-NEG
        ((M-4) ADD M-4 A-TRI-RY)
        (JUMP-XCT-NEXT TV-DRAW-TRI-LOOP)
       ((M-S) SUB M-S (A-CONSTANT 1))

;;;This sets up the starting and incrementing remainders and quotients for the left or right
;;;point depending on the sign of det, which it complements, so as to do the other one next
;;;time.
TV-DRAW-TRI-1
        ((A-TRI-DET) SUB M-ZERO A-TRI-DET)
        ((M-C) SUB M-B A-C)             ;Y1 - Y2
        (JUMP-EQUAL-XCT-NEXT M-C A-ZERO XFALSE) ;Avoid divide by 0
       ((M-T) SUB M-1 A-2)              ;X1 - X2
        ((M-1) DPB M-1 (BYTE-FIELD 31. 1) (A-CONSTANT 1))       ;(2 * X1) + 1
        (CALL-XCT-NEXT MPY)
       ((Q-R) M-C)
        ((M-1) SUB Q-R A-T)             ;L := (((2 * X1) + 1) * (Y1 - Y2)) - (X1 - X2)
        ((M-C) ADD M-C A-C)             ;DY := 2 * (Y1 - Y2)
        (CALL-XCT-NEXT DIV)
       ((M-2) M-C)
        ((M-B) Q-R)                     ;Save L DIV DY
        ((M-I) M-1)                     ;Save L REM DY
        ((M-1) ADD M-T A-T)
        (CALL-XCT-NEXT DIV)
       ((M-2) M-C)
        (JUMP-LESS-THAN M-ZERO A-TRI-DET TV-DRAW-TRI-1-R)
TV-DRAW-TRI-1-L
        ((A-TRI-LY) M-C)
        ((M-R) M-B)
        ((M-3) M-I)
        (POPJ-AFTER-NEXT (A-TRI-XLI) Q-R)
       ((A-TRI-XLIR) M-1)

TV-DRAW-TRI-1-R
        ((A-TRI-RY) M-C)
        ((M-S) M-B)
        ((M-4) M-I)
        (POPJ-AFTER-NEXT (A-TRI-XRI) Q-R)
       ((A-TRI-XRIR) M-1)
(end-pagable-ucode)

;;; %AOS-TRIANGLE X1 Y1 X2 Y2 X3 Y3 INCREMENT SHEET
;;; Increment each pixel inside the triangle by the specified amount

X-AOS-TRIANGLE (MISC-INST-ENTRY %AOS-TRIANGLE)
        (call trap)
     (error-table illegal-instruction)

;;;; Given a rectangle of an ART-4B array, and 16 values which specify new values
;;;; for the pixels (indexed by current pixel value), hacks the ART-4B array appropriately
;;;;
;;;; (%COLOR-TRANSFORM N17 N16 N15 N14 N13 N12 N11 N10 N7 N6 N5 N4 N3 N2 N1 N0
;;;;                  WIDTH HEIGHT ARRAY START-X START-Y)

XCOLOR-TRANSFORM (MISC-INST-ENTRY %COLOR-TRANSFORM)
        (call trap)
     (error-table illegal-instruction)


(begin-pagable-ucode)
;;; GCD IS SYMMETRICAL (BIGNUM IN M-B, FIXNUM IN M-2)
;;; THIS DEPENDS ON REMAINDER-BIG-FIX NOT SMASHING M-2 AND LEAVING RESULT IN M-1
;;; SO THAT WE CAN CALL GCD-FIX-FIX IMMEDIATELY
;;;If you want you can call GCD-BIG-FIX-1 with the length of the bignum in M-C
;;; and the sign bit in the low order bit of M-A. Note that GCD-BIG-FIX-1 doesn't handle
;;; the case with a fixnum 0!
GCD-BIG-FIX
GCD-FIX-BIG
        (JUMP-EQUAL M-2 A-ZERO GCD-IS-ABS-M-B)
        ((M-C) BIGNUM-HEADER-LENGTH MD)
        ((M-A) BIGNUM-HEADER-SIGN MD)
GCD-BIG-FIX-1
GCD-FIX-BIG-1
        (JUMP-XCT-NEXT GCD-FIX-FIX)             ;DO A FIXNUM FIXNUM GCD,
       (CALL REMAINDER-BIG-FIX-1)               ;BUT FIRST GET (\ BIGNUM FIXNUM)

GCD-IS-ABS-M-B
        ((M-I) BIGNUM-HEADER-LENGTH MD)
        (JUMP-XCT-NEXT BIGNUM-ABS)
       ((M-Q) M-B)

;;;We get here with a bignum in M-B and a bignum in M-C with its header in MD
;;; M-T also has the same thing in it as M-B
GCD-BIG-BIG
        ((M-E) HEADER-REST-FIELD MD)
        ((VMA-START-READ) M-B)
        (CHECK-PAGE-READ)
        ((M-J) BIGNUM-HEADER-LENGTH M-E)
        ((M-I) BIGNUM-HEADER-LENGTH MD)
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-I A-J GCDBB-1)
       ((M-D) HEADER-REST-FIELD MD)
        ((M-J) BIGNUM-HEADER-LENGTH M-D)
        ((M-I) BIGNUM-HEADER-LENGTH M-E)
        ((M-D) M-E)
        ((M-B) M-C)
        ((M-C) M-T)     ;Remember M-T and M-B start with the same thing.
GCDBB-1
        (JUMP-GREATER-THAN M-J (A-CONSTANT 1) GCDBB-LONG)
        ;; since M-J = 1 we can use Bignum-Fixnum case.
        ((VMA-START-READ) ADD M-C (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-2) MD)
        ((M-C) BIGNUM-HEADER-LENGTH M-D)
        ((M-A) BIGNUM-HEADER-SIGN M-D)
        ;;First do (\ bignum fixnum). REMAINDER-BIG-FIX must leave answer in
        ;;M-1 and not bash M-2.
        (JUMP-XCT-NEXT GCD-FIX-FIX)
       (CALL REMAINDER-BIG-FIX-1)

;;;When we get here we have the longer bignum in M-B,M-I shorter in M-C,M-J
;;; So M-I and M-J are both 2 or more.
;;;To make this work BIDIV-REMAINDER-COMMON must not touch the contents of M-C
GCDBB-LONG
        ((M-Q) M-B)
        ((M-R) M-C)             ;saved by BIDIV-REMAINDER-COMMON
        (CALL-XCT-NEXT BIDIV-REMAINDER-COMMON)
       ((M-A) A-ZERO)           ;indicate quotient is not being saved.
        ;; Now we have a bignum in M-Q,(M-I + 1) that is the remainder
        ;;shifted left by an amount determined by M-D. M-C,M-J contains
        ;;the bignum we were dividing by.
        ;; We are going to pretend from now on that the bignum in
        ;;M-Q is only M-J long.
        ((M-T) M-C)
        ((M-K) ADD M-I (A-CONSTANT 2))          ;(length of bignum in M-Q) + 1
        ;;Now shift down the bignum in M-Q
        ((M-C) M-Q)
        (CALL-XCT-NEXT GCDBB-SHIFT)
       ((M-S) M-Q)
        (JUMP-NOT-EQUAL-XCT-NEXT M-E A-ZERO GCDBB-NO-LUCK)
       ((M-C) M-T)
        ((M-1) M-Q)
        ((M-Q) A-V-NIL)                         ;Possible pointer to garbage.
        ((M-S) A-V-NIL)                         ;Possible pointer to garbage.
        (JUMP-XCT-NEXT UN-CONS)
       ((M-2) M-K)                              ;saved just for the occasion.

GCDBB-NO-LUCK
        ;;Figure out how much it was shifted:
        ((M-TEM) DPB M-3 (BYTE-FIELD 27. 5.) A-ZERO)
        ((M-TEM) SUB M-TEM A-3)
        ((M-TEM) ADD M-TEM A-4)
        ((M-TEM) SUB M-TEM (A-CONSTANT 31.))
        ((M-D) ADD M-TEM A-D)   ;M-D had 31. - (the number of extra zeros that
                                ;BIDIV-REMAINDER-COMMON introduced) .
        (call-xct-next allocate-bignum-storage)
       ((m-b) add m-j (a-constant 1))
        ((MD) ADD M-J (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                                        (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-BIGNUM))))
        ((VMA-START-WRITE M-R) Q-POINTER M-T
                               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER)))
        (CHECK-PAGE-WRITE)
        (CALL-XCT-NEXT GCDBB-SHIFT)
       ((M-S) M-R)              ;Prepare to call GCDBB-SHIFT
        ;;M-TEM gets the number of factors of two in the bignum in M-R.
        ((M-TEM) DPB M-3 (BYTE-FIELD 27. 5.) A-ZERO)
        ((M-TEM) SUB M-TEM A-3)
        ((M-TEM) ADD M-TEM A-4)
        ;;M-D gets the power of two in the answer:
        (JUMP-LESS-THAN M-D A-TEM GCDBB-4)
        ((M-D) M-TEM)
GCDBB-4
        ((M-T) M-Q)             ;This one will be the answer
        ((M-K) M-J)             ;This is its length

;;;We get here with two odd bignums in M-Q and M-R, their actual length is in M-K,
;;; the one to return as answer is also in M-T, the number of powers of 2 in the
;;; answer is in M-D, M-J contains the length of the bignums that might still be nonzero.
GCDBB-LOOP
        ((M-I) M-J)             ;Step down the bignums
GCDBB-L1
        ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        ((M-2) MD)
        ((VMA-START-READ) ADD M-R A-I)
        (CHECK-PAGE-READ)
;       (JUMP-EQUAL-XCT-NEXT M-I (A-CONSTANT 1) GCD-FIX-FIX) ;something like this
;      ((M-1) MD)                                               ;should be done.
        (JUMP-NOT-EQUAL-XCT-NEXT M-2 A-ZERO GCDBB-SUB)
       ((M-I) SUB M-I (A-CONSTANT 1))
        (JUMP-NOT-EQUAL MD A-ZERO GCDBB-ORDER)
        (JUMP-XCT-NEXT GCDBB-L1)
       ((M-J) M-I)

GCDBB-SUB-L
        ((VMA-START-READ) ADD M-Q A-I)
        (CHECK-PAGE-READ)
        ((M-2) MD)
        ((VMA-START-READ) ADD M-R A-I)
        (CHECK-PAGE-READ)
        ((M-I) SUB M-I (A-CONSTANT 1))
GCDBB-SUB
        (JUMP-GREATER-THAN MD A-2 GCDBB-ORDER)
        (JUMP-LESS-THAN MD A-2 GCDBB-NORDER)
        (JUMP-GREATER-THAN M-I A-ZERO GCDBB-SUB-L)

;;;Here we are done, the answer is in M-T, although it might have to be trimmed and shifted.
;;; There is a bignum to give back in M-Q or M-R.
        (JUMP-EQUAL-XCT-NEXT M-R A-T GCDBB-GIVE-BACK-M-Q)
       ((M-2) ADD M-K (A-CONSTANT 1))   ;This is how much to give back
        ((M-Q) M-R)
        ((M-R) A-V-NIL)                         ;Possible pointer to garbage.
GCDBB-GIVE-BACK-M-Q
        ((M-1) Q-POINTER M-Q)
        ((M-S) A-V-NIL)                         ;Possible pointer to garbage.
        (CALL-XCT-NEXT UN-CONS)
       ((M-Q) A-V-NIL)                          ;Possible pointer to garbage.
        ((M-1) M-D)
        (CALL-XCT-NEXT DIV)
       ((M-2) (A-CONSTANT 31.))
        (JUMP-EQUAL-XCT-NEXT M-1 A-ZERO GCDBB-COPY-WORDS)
       ((M-E) Q-R)              ;This is the offset we want
        ((M-I) M-K)             ;Real length (sig. length in M-J)
        ((M-B) M-T)                             ;From
        ((M-D) M-T)                             ;To
        ;;Constant for LDB (M-K):
        ((M-K) ADD M-1 (A-CONSTANT 1))          ;MROT   = M-1 + 1
        ((M-TEM) SUB M-1 (A-CONSTANT 1))        ;BYTL-1 = M-1 + 1
        ((M-K) DPB M-TEM OAL-BYTL-1 A-K)
        ;;Constant for DPB (M-S):
        ((M-TEM) (A-CONSTANT 30.))
        ((M-TEM) SUB M-TEM A-1)                 ;BYTL-1 = 30. - M-1
        ((M-S) DPB M-TEM OAL-BYTL-1 A-1)        ;MROT   = M-1
        ((M-ZR) SUB M-I A-E)                    ;Read first word from here.
        ((VMA-START-READ) ADD M-D A-ZR)
        (CHECK-PAGE-READ)
#+exp   ((m-tem3) add m-s (a-constant 1_5))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) M-S)
        ((M-2) DPB MD (BYTE-FIELD 0 0) A-ZERO)
        (CALL-XCT-NEXT BIDIV-NORMALIZE)
       ((M-ZR) SUB M-ZR (A-CONSTANT 1))
GCDBB-RETURN
        ;;Cleanup Bignum in M-Q (Have to reread header to get actual length!)
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        ((M-D) BIGNUM-HEADER-LENGTH MD)
        ((M-C) M-D)
        (JUMP-XCT-NEXT ARY-TO-BIG-CLEANUP)
       ((M-E) M-D)

GCDBB-COPY-WORDS
        (JUMP-EQUAL M-E A-ZERO GCDBB-RETURN)
        ((M-A) M-K)                             ;Move words to here,
        ((M-B) SUB M-A A-E)                     ;from here.
GCDBB-COPY-WORDS-1
        ((VMA-START-READ) ADD M-T A-B)
        (CHECK-PAGE-READ)
        ((VMA-START-WRITE) ADD M-T A-A)
        (CHECK-PAGE-WRITE-unboxed)
        ((M-A) SUB M-A (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-B (A-CONSTANT 1) GCDBB-COPY-WORDS-1)
       ((M-B) SUB M-B (A-CONSTANT 1))
        ((MD) A-ZERO)
GCDBB-COPY-WORDS-2
        ((VMA-START-WRITE) ADD M-T A-A)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-GREATER-THAN-XCT-NEXT M-A (A-CONSTANT 1) GCDBB-COPY-WORDS-2)
       ((M-A) SUB M-A (A-CONSTANT 1))
        (JUMP GCDBB-RETURN)

GCDBB-NORDER
        ((M-TEM) M-Q)
        ((M-Q) M-R)
        ((M-R) M-TEM)
GCDBB-ORDER
        ((M-ZR) (A-CONSTANT 1)) ;steps (up) thru bignums
        ((M-C) A-ZERO)          ;borrow from last round
        ((M-S) M-R)             ;we subtract into this guy
        ((M-E) A-ZERO)          ;For BIGNUM-RIGHT-JUST
GCDBB-STUFF
        ((VMA-START-READ) ADD M-Q A-ZR)
        (CHECK-PAGE-READ)
        ((M-2) ADD MD A-C)      ;remember to borrow
        ((VMA-START-READ) ADD M-R A-ZR)
        (CHECK-PAGE-READ)
        ((M-2) SUB MD A-2)
        ((M-C) (BYTE-FIELD 1 31.) M-2 A-ZERO)
        (CALL-XCT-NEXT BIGNUM-RIGHT-JUST)
       ((M-2) (BYTE-FIELD 31. 0) M-2 A-ZERO)
        (JUMP-LESS-THAN-XCT-NEXT M-ZR A-J GCDBB-STUFF)
       ((M-ZR) ADD M-ZR (A-CONSTANT 1))
        (CALL-XCT-NEXT BIGNUM-RIGHT-JUST)       ;Flush last bits.
       ((M-2) A-ZERO)
        (JUMP-GREATER-THAN M-E A-J GCDBB-LOOP)
GCDBB-STUFF-1
        ((MD) A-ZERO)
        ((VMA-START-WRITE) ADD M-S A-E)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-LESS-THAN-XCT-NEXT M-E A-J GCDBB-STUFF-1)
       ((M-E) ADD M-E (A-CONSTANT 1))
        (JUMP GCDBB-LOOP)

;;;Right justify a bignum in M-C into a bignum in M-S (M-J contains the length for both.)
;;; M-I steps through M-C and BIGNUM-RIGHT-JUST is used.
;;;In case M-C contains all zeros M-E will contain 0 instead of M-J + 1.
GCDBB-SHIFT
        ((M-E) A-ZERO)
        ((M-3) A-MINUS-ONE)
        ((M-I) (A-CONSTANT 1))  ;step thru bignum in M-C
GCDBB-2
        ((VMA-START-READ) ADD M-C A-I)
        (CHECK-PAGE-READ)
        (CALL-XCT-NEXT BIGNUM-RIGHT-JUST)
       ((M-2) MD)
        (JUMP-LESS-THAN-XCT-NEXT M-I A-J GCDBB-2)
       ((M-I) ADD M-I (A-CONSTANT 1))
        (CALL-XCT-NEXT BIGNUM-RIGHT-JUST)       ;Flush last bits
       ((M-2) A-ZERO)
        (POPJ-GREATER-THAN M-E A-J)
        (POPJ-EQUAL M-E A-ZERO)                 ;M-C was all zeros!
        ((MD) A-ZERO)
GCDBB-3
        ((VMA-START-WRITE) ADD M-S A-E)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-LESS-THAN-XCT-NEXT M-E A-J GCDBB-3)
       ((M-E) ADD M-E (A-CONSTANT 1))
        (POPJ)
;;talk about hax. this NOOP is needed until we fix the assembler
;;to automatically fill the last location in virtual microcode
;;with something, for the benefit of the parity checking hardware.
        (NO-OP)

(end-pagable-ucode)

))

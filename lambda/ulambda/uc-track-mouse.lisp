;-*- mode: lisp; base: 8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;
(DEFCONST UC-TRACK-MOUSE '(
;;; "Build a better mousetrap and the world will beat a path to your door"
;;;                                     -- Samuel F. B. Morse

xset-mouse-arrays (misc-inst-entry %set-mouse-arrays)
        ((m-random-temporary-mouse-y-scale-base) pdl-pop)
        ((m-random-temporary-mouse-x-scale-base) pdl-pop)
        (popj-after-next
          (m-random-temporary-buttons-buffer-base) pdl-pop)
        ((m-random-temporary-cursor-pattern-base) pdl-pop)

XSET-MOUSE-SCREEN (MISC-INST-ENTRY %SET-MOUSE-SCREEN)
        (CALL SWAP-TV-AND-MOUSE-SCREENS)
        (CALL SELECT-SHEET-0)
                ;drops through
SWAP-TV-AND-MOUSE-SCREENS
        ((M-TEM) A-MOUSE-SCREEN-BUFFER-ADDRESS)
        ((A-MOUSE-SCREEN-BUFFER-ADDRESS) A-TV-SCREEN-BUFFER-ADDRESS)
        ((A-TV-SCREEN-BUFFER-ADDRESS) M-TEM)
        ((M-TEM) A-MOUSE-SCREEN-BUFFER-END-ADDRESS)
        ((A-MOUSE-SCREEN-BUFFER-END-ADDRESS) A-TV-SCREEN-BUFFER-END-ADDRESS)
        ((A-TV-SCREEN-BUFFER-END-ADDRESS) M-TEM)
        ((M-TEM) A-MOUSE-SCREEN-LOCATIONS-PER-LINE)
        ((A-MOUSE-SCREEN-LOCATIONS-PER-LINE) A-TV-SCREEN-LOCATIONS-PER-LINE)
        ((A-TV-SCREEN-LOCATIONS-PER-LINE) M-TEM)
        ((M-TEM) A-MOUSE-SCREEN-BUFFER-BIT-OFFSET)
        ((A-MOUSE-SCREEN-BUFFER-BIT-OFFSET) A-TV-SCREEN-BUFFER-BIT-OFFSET)
        ((A-TV-SCREEN-BUFFER-BIT-OFFSET) M-TEM)
        ((M-TEM) A-MOUSE-SCREEN-WIDTH)
        ((A-MOUSE-SCREEN-WIDTH) A-TV-SCREEN-WIDTH)
        ((A-TV-SCREEN-WIDTH) M-TEM)
        ((M-TEM) A-MOUSE-SCREEN-BUFFER-PIXEL-SIZE-MROT)
        ((A-MOUSE-SCREEN-BUFFER-PIXEL-SIZE-MROT) A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT)
        ((A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT) M-TEM)
        ((M-TEM) A-MOUSE-SCREEN)
        (POPJ-AFTER-NEXT (A-MOUSE-SCREEN) A-TV-CURRENT-SHEET)
       ((A-TV-CURRENT-SHEET) M-TEM)

SELECT-SHEET-0
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (JUMP-DATA-TYPE-NOT-EQUAL M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE))
                                  SELECT-SHEET-ARRAY)
        (JUMP SELECT-SHEET-1)

;XOR the mouse cursor into the screen at its current position (except that if
;MOUSE-X, MOUSE-Y lie outside the MOUSE-SHEET, force the cursor to stay inside.)
;Due to the offset, the bits of the cursor can hang out in any direction.  We
;clip at the top and bottom, but let it wrap around at left and right for now.
;Uses M-1, M-2, M-A, M-B, M-E, M-T, Q-R, M-TEM only
XOR-MOUSE-CURSOR
        ((M-A) A-MOUSE-CURSOR-X)
        ((M-B) A-MOUSE-CURSOR-Y)
XOR-MOUSE-CURSOR-0
        ;Confine reference position in M-A, M-B to within the sheet
        (JUMP-IF-BIT-CLEAR BOXED-SIGN-BIT M-A CONFINE-CURSOR-1)
        ((M-A) A-ZERO)
CONFINE-CURSOR-1
        (JUMP-LESS-THAN M-A A-MOUSE-SCREEN-WIDTH CONFINE-CURSOR-2)
        ((M-A) ADD (M-CONSTANT -1) A-MOUSE-SCREEN-WIDTH)
CONFINE-CURSOR-2
        (JUMP-IF-BIT-CLEAR BOXED-SIGN-BIT M-B CONFINE-CURSOR-3)
        ((M-B) A-ZERO)
CONFINE-CURSOR-3
        ;Don't bother at the bottom, since SELECT-SHEET didn't save the
        ;right value and for most cursors the difference would be indistinguishable.

        ;Convert (X,Y) address like TVXYADR
        ((M-1) DPB M-B (BYTE-FIELD 20. 12.) A-ZERO)     ;Y POSITION (LSH 12)
        (CALL-XCT-NEXT MPY12)
       ((Q-R) A-MOUSE-SCREEN-LOCATIONS-PER-LINE)        ;M-2 GETS OFFSET TO START OF LINE
        ((OA-REG-LOW) A-MOUSE-SCREEN-BUFFER-PIXEL-SIZE-MROT)
        ;; X coordinate gets multiplied by pixel size
        ((M-TEM) DPB M-A (BYTE-FIELD 24. 0) A-ZERO)
        ((M-TEM) ADD M-TEM A-MOUSE-SCREEN-BUFFER-BIT-OFFSET)
        ((M-1) (BYTE-FIELD 19. 5) M-TEM)                ;WORD PART OF X POSITION
#+lambda((OA-REG-HIGH) (BYTE-FIELD 1 18.) M-1)
#+exp   ((m-tem3) (byte-field 1 18.) m-1)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-1) SELECTIVE-DEPOSIT M-ZERO (BYTE-FIELD 13. 19.) A-1)
        ((M-E) ADD M-2 A-1)                             ;RELATIVE WORD ADDRESS
        ((M-E) ADD M-E A-MOUSE-SCREEN-BUFFER-ADDRESS)
        ((M-T) (BYTE-FIELD 5 0) M-TEM)                  ;BIT PART OF X POSITION
        ((M-E) Q-POINTER M-E)                   ;Truncate in case of ridiculous X,Y
        ;M-E word address, M-T bit offset in that word
        ;Now output one or two columns, depending on whether it crosses word boundary
        ((M-B) ADD (M-CONSTANT -1) A-MOUSE-CURSOR-WIDTH)
        ((M-B) (BYTE-FIELD 5 0) M-B)
        ((M-TEM) M-A-1 (M-CONSTANT 40) A-T)
        (JUMP-LESS-OR-EQUAL M-B A-TEM XOR-MOUSE-CURSOR-3)
        ;Do second column first
        ((M-2) M-A-1 M-B A-TEM)                 ;Byte width-1 for second column
        ((M-B) M-TEM)                           ;Byte width-1 for first column
        ((M-TEM) M-A-1 (M-CONSTANT 40) A-B)
        ((M-2) DPB M-2 OAL-BYTL-1 A-TEM)        ;LDB pointer for second column

        ((m-a) a-random-temporary-cursor-pattern-base)
        ((M-1) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-CURSOR-HEIGHT)
        ((m-d) SUB M-E A-MOUSE-SCREEN-LOCATIONS-PER-LINE)
        ((m-d) ADD m-d (A-CONSTANT 1))
XOR-MOUSE-CURSOR-1
        (JUMP-LESS-THAN m-d A-MOUSE-SCREEN-BUFFER-ADDRESS XOR-MOUSE-CURSOR-2)
        (JUMP-GREATER-OR-EQUAL m-d A-MOUSE-SCREEN-BUFFER-END-ADDRESS XOR-MOUSE-CURSOR-3)

        ((vma-start-read) m-a)
        (check-page-read-map-reload-only)
#+lambda((OA-REG-LOW) M-2)
#+exp   ((m-tem3) ADD M-2 (A-Constant 1_5))
#+exp   ((OA-REG-LOW) (Byte-Field 10. 0) m-tem3)
        ((m-c) (BYTE-FIELD 0 0) md)             ;Cursor pattern aligned

        ((VMA-START-READ m-d) ADD m-d A-MOUSE-SCREEN-LOCATIONS-PER-LINE)
        (CHECK-PAGE-READ-map-reload-only)
        ((md-start-write) xor md a-c)
        (CHECK-PAGE-WRITE-map-reload-only)

XOR-MOUSE-CURSOR-2
        ((M-A) ADD M-A (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-1 (A-CONSTANT 1) XOR-MOUSE-CURSOR-1)
       ((M-1) SUB M-1 (A-CONSTANT 1))

XOR-MOUSE-CURSOR-3
        ;Now do first column
        ((m-a) a-random-temporary-cursor-pattern-base)
        ((M-1) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-CURSOR-HEIGHT)
        ((m-d) SUB M-E A-MOUSE-SCREEN-LOCATIONS-PER-LINE)
XOR-MOUSE-CURSOR-4
        (JUMP-LESS-THAN m-d A-MOUSE-SCREEN-BUFFER-ADDRESS XOR-MOUSE-CURSOR-5)
        ;Clobber M-C before leaving so we don't get an instance header left around. -dg
        ((m-c) setz)
        (POPJ-GREATER-OR-EQUAL m-d A-MOUSE-SCREEN-BUFFER-END-ADDRESS)

        ((vma-start-read) m-a)
        (check-page-read-map-reload-only)
#+lambda((OA-REG-LOW) DPB M-B OAL-BYTL-1 A-T)
#+exp   ((m-tem3) M+1 M-B)
#+exp   ((OA-REG-LOW) DPB M-tem3 OAL-BYTL-1 A-T)
        ((m-c) dpb md (byte-field 0 0) a-zero)

        ((VMA-START-READ m-d) ADD m-d A-MOUSE-SCREEN-LOCATIONS-PER-LINE)
        (CHECK-PAGE-READ-map-reload-only)
        ((md-start-write) xor md a-c)
        (CHECK-PAGE-WRITE-map-reload-only)
XOR-MOUSE-CURSOR-5
        ((M-A) ADD M-A (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-1 (A-CONSTANT 1) XOR-MOUSE-CURSOR-4)
       ((M-1) SUB M-1 (A-CONSTANT 1))
        ;Clobber M-C before leaving so we don't get an instance header left around. -dg
        ((m-c) setz)
        (POPJ)

XOPEN-MOUSE-CURSOR (MISC-INST-ENTRY %OPEN-MOUSE-CURSOR)
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-CURSOR-STATE)
        (CALL-EQUAL M-TEM (A-CONSTANT 3) XOR-MOUSE-CURSOR)
        (JUMP-XCT-NEXT XFALSE)
       ((A-MOUSE-CURSOR-STATE) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX) 1)))

;Here every 60th of a second
TRACK-MOUSE  (declare (local a-mouse-x a-mouse-y a-mouse-cursor-x-offset a-mouse-cursor-y-offset
                             a-mouse-cursor-width a-mouse-cursor-height
                             a-mouse-x-speed a-mouse-y-speed
                             a-mouse-buttons-buffer-in-index a-mouse-buttons-out-index
                             a-mouse-wakeup))
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-CURSOR-STATE)
        (POPJ-EQUAL M-TEM A-ZERO)               ;Disabled
        (popj-equal m-zero a-random-temporary-buttons-buffer-base) ;No mouse yet.
        ((A-MOUSE-SAVE-1) M-1)
        ((A-MOUSE-SAVE-2) M-2)
        ((A-MOUSE-SAVE-E) M-E)
        ((a-mouse-save-c) m-c)
        ((a-mouse-save-d) m-d)
#-lambda(begin-comment)
        ((M-TEM) A-MOUSE-FAKE-REGISTER)
        ((M-A) (BYTE-FIELD 20 0) M-TEM)
        ((M-B) (BYTE-FIELD 20 20) M-TEM)
#-lambda(end-comment)
#-exp(begin-comment)
        ;;get y pos
        ((vma-start-read-unmapped) (a-constant #xf5f20000))
        (no-op)
        ((m-a) ldb (byte-field 12. 0) md) ;divide by 2
        ;;get x pos
        ((vma-start-read-unmapped) add vma (a-constant 4))
        (no-op)
        ((m-b) ldb (byte-field 12. 0) md) ;divide by 2
        ;;get buttons
        ((vma-start-read-unmapped) add vma (a-constant 4))
        (no-op)
        ;right
        ((m-tem) ldb (byte-field 1 4) md)
        ((m-a) dpb m-tem (byte-field 1 14.) a-a)
        ;middle
        ((m-tem) ldb (byte-field 1 5) md)
        ((m-a) dpb m-tem (byte-field 1 13.) a-a)
        ;left
        ((m-tem) ldb (byte-field 1 6) md)
        ((m-a) dpb m-tem (byte-field 1 12.) a-a)
#-exp(end-comment)
        ((M-TEM) XOR M-A A-MOUSE-LAST-H1)       ;Have buttons changed state?
        ((M-TEM) (BYTE-FIELD 3 12.) M-TEM)
        (JUMP-EQUAL M-TEM A-ZERO TRACK-MOUSE-1)
        ;Store new state of buttons into buttons buffer
        ((A-MOUSE-WAKEUP) A-V-TRUE)
        ((M-T) DPB M-ZERO (BYTE-FIELD 27. 5) A-MOUSE-BUTTONS-BUFFER-IN-INDEX)
;       ((M-T) ADD M-T (A-CONSTANT MOUSE-BUTTONS-BUFFER-AMEM-LOC))
     ;This may look completely random, but the lower 5 bits are all that are used and
     ;they are masked off below.
        ((vma m-t) add m-t a-random-temporary-buttons-buffer-base)
        (CALL READ-MICROSECOND-CLOCK)           ;M-2 gets clock
;       ((OA-REG-LOW M-TEM) DPB M-T OAL-A-DEST A-ZERO)
;       ((A-GARBAGE) BOXED-NUM-EXCEPT-SIGN-BIT M-2      ;FIXNUM-MICROSECOND-TIME
;                       (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       ((OA-REG-LOW M-TEM) ADD M-TEM (A-CONSTANT (BYTE-VALUE OAL-A-DEST 1)))
;       ((A-GARBAGE) A-MOUSE-X)
;       ((OA-REG-LOW M-TEM) ADD M-TEM (A-CONSTANT (BYTE-VALUE OAL-A-DEST 1)))
;       ((A-GARBAGE) A-MOUSE-Y)
;       ((OA-REG-LOW M-TEM) ADD M-TEM (A-CONSTANT (BYTE-VALUE OAL-A-DEST 1)))
;       ((A-GARBAGE) (BYTE-FIELD 3 12.) M-A (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((md-start-write) boxed-num-except-sign-bit m-2 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (check-page-write-map-reload-only)
        ((vma) add vma (a-constant 1))
        ((md-start-write) a-mouse-x)            ;has data type, etc.
        (check-page-write-map-reload-only)
        ((vma) add vma (a-constant 1))
        ((md-start-write) a-mouse-y)            ;has data type, etc.
        (check-page-write-map-reload-only)
        ((vma) add vma (a-constant 1))
        ((md-start-write) (byte-field 3 12.) m-a (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (check-page-write-map-reload-only)
        ((M-T) ADD M-T (A-CONSTANT 4))
        ((M-T) (BYTE-FIELD 5 0) M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP-NOT-EQUAL-XCT-NEXT M-T A-MOUSE-BUTTONS-BUFFER-OUT-INDEX TRACK-MOUSE-1)
       ((A-MOUSE-BUTTONS-BUFFER-IN-INDEX) M-T)
        ((M-T) ADD M-T (A-CONSTANT 4))          ;Buffer full: discard oldest value
        ((A-MOUSE-BUTTONS-BUFFER-OUT-INDEX) (BYTE-FIELD 5 0) M-T
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
TRACK-MOUSE-1
        ;Compute physical delta-X (in M-B) and delta-Y (in M-A)
        ((M-A) SUB M-A A-MOUSE-LAST-H1)
        ((A-MOUSE-LAST-H1) ADD M-A A-MOUSE-LAST-H1)
#+lambda((OA-REG-HIGH) (BYTE-FIELD 1 11.) M-A)
#+exp   ((m-tem3) (byte-field 1 11.) m-a)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-A) DPB M-ZERO (BYTE-FIELD 21. 11.) A-A)
        ((M-B) SUB M-B A-MOUSE-LAST-H2)
        ((A-MOUSE-LAST-H2) ADD M-B A-MOUSE-LAST-H2)
#+lambda((OA-REG-HIGH) (BYTE-FIELD 1 11.) M-B)
#+exp   ((m-tem3) (byte-field 1 11.) m-b)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-B) DPB M-ZERO (BYTE-FIELD 21. 11.) A-B)
        ;Compute physical speed, which involves time averaging
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-Y-SPEED)
        (JUMP-EQUAL M-TEM A-ZERO TRACK-MOUSE-1C)
        ((M-TEM1) (BYTE-FIELD 20. 4) M-TEM)
        ((M-TEM) M-A-1 M-TEM A-TEM1)            ;Speed times 15/16 or less
TRACK-MOUSE-1C
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-A A-ZERO TRACK-MOUSE-1A)
       ((M-TEM1) DPB M-A (BYTE-FIELD 30. 2) A-ZERO)     ;delta-Y times 4
        ((M-TEM1) SUB M-ZERO A-TEM1)            ;sum((15/16)^i,i,0,inf)=16
TRACK-MOUSE-1A
        ((M-TEM) ADD M-TEM A-TEM1)              ;New speed
        ((A-MOUSE-Y-SPEED) Q-POINTER M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-X-SPEED)
        (JUMP-EQUAL M-TEM A-ZERO TRACK-MOUSE-1D)
        ((M-TEM1) (BYTE-FIELD 20. 4) M-TEM)
        ((M-TEM) M-A-1 M-TEM A-TEM1)            ;Speed times 15/16 or less
TRACK-MOUSE-1D
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-B A-ZERO TRACK-MOUSE-1B)
       ((M-TEM1) DPB M-B (BYTE-FIELD 30. 2) A-ZERO)     ;delta-X times 4
        ((M-TEM1) SUB M-ZERO A-TEM1)
TRACK-MOUSE-1B
        ((M-TEM) ADD M-TEM A-TEM1)              ;New speed
        ((A-MOUSE-X-SPEED) Q-POINTER M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ;Speed bum if mouse hasn't moved
        ((M-TEM) IOR M-A A-B)
        (JUMP-EQUAL M-TEM A-ZERO TRACK-MOUSE-NO-MOTION)
        ;Do speed-dependent scaling into logical delta-X, delta-Y, and update position
;       ((M-1) (A-CONSTANT MOUSE-Y-SCALE-ARRAY-AMEM-LOC))
;       ((M-2) ADD M-1 (A-CONSTANT 12.))
        ((vma) a-random-temporary-mouse-y-scale-base)
        ((m-2) add vma (a-constant 12.))
TRACK-MOUSE-YSC-LOOP
;       ((OA-REG-HI) DPB M-1 OAH-A-SRC A-ZERO)
;       ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-GARBAGE)
;       (JUMP-GREATER-THAN M-TEM A-MOUSE-Y-SPEED TRACK-MOUSE-YSC)
;       (JUMP-LESS-THAN-XCT-NEXT M-1 A-2 TRACK-MOUSE-YSC-LOOP)
;      ((M-1) ADD M-1 (A-CONSTANT 2))
        ((vma-start-read) vma)
        (check-page-read-map-reload-only)
        ;;a-mouse-y-speed has dtp-fix
        ((m-tem) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))
        (jump-greater-than m-tem a-mouse-y-speed track-mouse-ysc)
        (jump-less-than-xct-next vma a-2 track-mouse-ysc-loop)
       ((vma) add vma (a-constant 2))

TRACK-MOUSE-YSC
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read-map-reload-only)
        ((m-1) q-pointer md)
;       ((OA-REG-HI) DPB M-1 OAH-A-SRC A-ZERO)
;       ((M-1) A-PGF-TEM)                       ;A-PGF-TEM = 1@A
#+lambda((OA-REG-HIGH) BOXED-SIGN-BIT M-1)
#+exp   ((m-tem3) boxed-sign-bit m-1)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-1) DPB M-ZERO Q-ALL-BUT-POINTER A-1)
        (CALL-XCT-NEXT MPY)
       ((Q-R) M-A)
        ((M-TEM) ADD Q-R A-MOUSE-Y-FRACTION)    ;Delta-Y times 1024
        ((A-MOUSE-Y-FRACTION) (BYTE-FIELD 10. 0) M-TEM)
        ((M-TEM) (BYTE-FIELD 22. 10.) M-TEM A-TEM)
        ((A-MOUSE-INTERNAL-Y) ADD M-TEM A-MOUSE-INTERNAL-Y)
        ;Note that we do not clip the mouse position at this level; the macrocode
        ;will handle that by warping the cursor back into the valid region
;       ((M-1) (A-CONSTANT MOUSE-X-SCALE-ARRAY-AMEM-LOC))
;       ((M-2) ADD m-1 (A-CONSTANT 12.))
        ((vma) a-random-temporary-mouse-x-scale-base)
        ((M-2) ADD vma (A-CONSTANT 12.))
TRACK-MOUSE-XSC-LOOP
;       ((OA-REG-HI) DPB M-1 OAH-A-SRC A-ZERO)
;       ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-GARBAGE)
        ((vma-start-read) vma)
        (check-page-read-map-reload-only)
        ;;a-mouse-x-speed has dtp-fix
        ((m-tem) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))
;       (JUMP-GREATER-THAN M-tem A-MOUSE-X-SPEED TRACK-MOUSE-XSC)
;       (JUMP-LESS-THAN-XCT-NEXT m-1 A-2 TRACK-MOUSE-XSC-LOOP)
;      ((m-1) add m-1 (a-constant 2))
        (jump-greater-than m-tem a-mouse-x-speed track-mouse-xsc)
        (jump-less-than-xct-next vma a-2 track-mouse-xsc-loop)
       ((vma) add vma (a-constant 2))

TRACK-MOUSE-XSC
;       ((OA-REG-HI) DPB M-1 OAH-A-SRC A-ZERO)
;       ((M-1) A-PGF-TEM)                       ;A-PGF-TEM = 1@A
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read-map-reload-only)
        ((m-1) q-pointer md)
#+lambda((OA-REG-HIGH) BOXED-SIGN-BIT M-1)
#+exp   ((m-tem3) boxed-sign-bit m-1)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-1) DPB M-ZERO Q-ALL-BUT-POINTER A-1)
        (CALL-XCT-NEXT MPY)
       ((Q-R) M-B)
        ((M-TEM) ADD Q-R A-MOUSE-X-FRACTION)    ;Delta-X times 1024
        ((A-MOUSE-X-FRACTION) (BYTE-FIELD 10. 0) M-TEM)
        ((M-TEM) (BYTE-FIELD 22. 10.) M-TEM A-TEM)
        ((A-MOUSE-INTERNAL-X) ADD M-TEM A-MOUSE-INTERNAL-X)
TRACK-MOUSE-NO-MOTION
;Now that we have moved the "internal" mouse location,
;move the external one too, with a pixel of hysteresis.

;Now see if the X position has changed enough to warrant waking up the mouse process
;and redrawing the cursor.  We lag a little so that small changes of position
;of less than a pixel away from the edge of the pixel the mouse is at
;do not get seen at a higher level.  They are not forgotten; motion of the
;internal mouse coordinates accumulates till it gets big enough to report.

;The Mouse Systems rodents do this sort of filtering themselves, so this hysteresis only
;impaired resolution.  KHS 10/05/84
        ((M-A) A-MOUSE-INTERNAL-X)
        (JUMP-EQUAL M-A A-MOUSE-PREVIOUS-INTERNAL-X TRACK-MOUSE-X-OK)
        ((M-1) SUB M-A A-MOUSE-PREVIOUS-INTERNAL-X)
;       (JUMP-EQUAL M-1 A-MINUS-ONE TRACK-MOUSE-X-OK)
;       (JUMP-EQUAL M-1 (A-CONSTANT 1) TRACK-MOUSE-X-OK)
;When the internal coordinate moves "enough", move the external one and wake up mouse process.
        ((A-MOUSE-WAKEUP) A-V-TRUE)
        ((A-MOUSE-PREVIOUS-INTERNAL-X) ADD M-1 A-MOUSE-PREVIOUS-INTERNAL-X)
        ((M-TEM) ADD M-1 A-MOUSE-X)
        ((A-MOUSE-X) Q-POINTER M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
TRACK-MOUSE-X-OK
;Do the same thing for Y.
        ((M-A) A-MOUSE-INTERNAL-Y)
        (JUMP-EQUAL M-A A-MOUSE-PREVIOUS-INTERNAL-Y TRACK-MOUSE-Y-OK)
        ((M-1) SUB M-A A-MOUSE-PREVIOUS-INTERNAL-Y)
;       (JUMP-EQUAL M-1 A-MINUS-ONE TRACK-MOUSE-Y-OK)
;       (JUMP-EQUAL M-1 (A-CONSTANT 1) TRACK-MOUSE-Y-OK)
;When the internal coordinate moves "enough", move the external one and wake up mouse process.
        ((A-MOUSE-WAKEUP) A-V-TRUE)
        ((A-MOUSE-PREVIOUS-INTERNAL-Y) ADD M-1 A-MOUSE-PREVIOUS-INTERNAL-Y)
        ((M-TEM) ADD M-1 A-MOUSE-Y)
        ((A-MOUSE-Y) Q-POINTER M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
TRACK-MOUSE-Y-OK

;Now see if the cursor needs attention.  If it is off, turn it on.  If it
;is on but in the wrong place, turn it off, move it, turn it on.
        ((M-T) DPB M-ZERO Q-ALL-BUT-POINTER A-MOUSE-CURSOR-STATE)
        (JUMP-LESS-THAN M-T (A-CONSTANT 2) TRACK-MOUSE-9)       ;Cursor is open
        ((M-A) A-MOUSE-CURSOR-X)
        ((M-1) A-MOUSE-X)                       ;Compute where cursor should be
        ((M-1) SUB M-1 A-MOUSE-CURSOR-X-OFFSET)
        ((A-MOUSE-CURSOR-X) Q-POINTER M-1)
        ((M-B) A-MOUSE-CURSOR-Y)
        ((M-2) A-MOUSE-Y)
        ((M-2) SUB M-2 A-MOUSE-CURSOR-Y-OFFSET)
        ((A-MOUSE-CURSOR-Y) Q-POINTER M-2)
        ((A-MOUSE-CURSOR-STATE) DPB (M-CONSTANT -1)     ;Set it to 3 (on)
                (BYTE-FIELD 2 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP-NOT-EQUAL M-T (A-CONSTANT 3) TRACK-MOUSE-8)
        (JUMP-NOT-EQUAL M-A A-MOUSE-CURSOR-X TRACK-MOUSE-7)
        (JUMP-EQUAL M-B A-MOUSE-CURSOR-Y TRACK-MOUSE-9)
TRACK-MOUSE-7
        (CALL XOR-MOUSE-CURSOR-0)               ;Undraw old cursor
TRACK-MOUSE-8
        (CALL XOR-MOUSE-CURSOR)                 ;Draw new cursor
TRACK-MOUSE-9
        ((M-1) A-MOUSE-SAVE-1)
        ((m-c) a-mouse-save-c)
        ((m-d) a-mouse-save-d)
        (POPJ-AFTER-NEXT (M-2) A-MOUSE-SAVE-2)
       ((M-E) A-MOUSE-SAVE-E)

process-mouse-char
 ;      ((vma-start-read) (a-constant (plus 177370400 16)))     ;Get a mouse character
 ;      (check-page-read-no-interrupt)
 ;      ((m-a) read-memory-data)
  ;char in M-A
        (jump-not-equal a-mouse-phase (m-constant -1) process-delta)    ;Buttons?
        ((m-tem) (byte-field 5 3) m-a)  ;Check validity of button byte
        (popj-not-equal m-tem (a-constant 20))

;;; Mouse Systems buttons are:
;;; Left: bit 2
;;; Middle: bit 1
;;; Right: bit 0
;;;
;;; MIT buttons are:
;;; Left: bit 12.
;;; Middle: bit 13.
;;; Right: bit 14.
;;;
;;; Also, the sense of the buttons is inverted:
;;; 1 = down in MIT, up in Mouse Systems.
;;;
;;; This code translates.

        ((m-tem) (a-constant 0))
        ((m-b) (byte-field 1 0) m-a)
        ((m-tem) dpb m-b (byte-field 1 2) a-tem)
        ((m-b) (byte-field 1 1) m-a)
        ((m-tem) dpb m-b (byte-field 1 1) a-tem)
        ((m-b) (byte-field 1 2) m-a)
        ((m-tem) dpb m-b (byte-field 1 0) a-tem)
        ((m-tem) setcm m-tem)
        ((a-mouse-fake-register) dpb m-tem (byte-field 3 14) a-mouse-fake-register)
        (popj-after-next (a-mouse-phase) (a-constant 0))
       (no-op)

process-delta

;;; The format of what the mouse sends is as follows:
;;; Byte 1: Buttons (as explained above)
;;; Byte 2: Delta-X-1
;;; Byte 3: Delta-Y-1
;;; Byte 4: Delta-X-2
;;; Byte 5: Delta-Y-2
;;;
;;; Each pair of deltas is independent.  Each delta is an 8-bit signed number, so it must
;;; be sign extended.  Also, the Delta-Y values must be negated, since Mouse Systems has
;;; Y increase with upward movement, like math but unlike Lisp Machines.  Finally, all the
;;; deltas must be multiplied by 2 to scale them to be compatible with the MIT mice.

        ((m-a) dpb m-a (byte-field 10 1) a-zero)                        ;Multiply by 2.
        (jump-if-bit-clear m-a (byte-field 1 10) delta-positive)        ;Positive?
        ((m-a) dpb m-minus-one (byte-field 27 11) a-a)                  ;Sign extend.

delta-positive
        ((m-b) a-mouse-phase)
        (jump-if-bit-set m-b (byte-field 1 0) delta-y-received)
        (jump-xct-next increment-mouse-phase)
       ((a-mouse-temp-x) m-a)

delta-y-received
        ((m-b) a-mouse-fake-register)
        ((m-tem) (byte-field 14 0) m-b) ;Add Delta-Y to Y position
        ((m-tem) sub m-tem a-a)
        ((a-mouse-fake-register) dpb m-tem (byte-field 14 0) a-mouse-fake-register)
        ((m-tem) (byte-field 14 20) m-b)        ;Add Delta-X to X position
        ((m-tem) add m-tem a-mouse-temp-x)
        ((a-mouse-fake-register) dpb m-tem (byte-field 14 20) a-mouse-fake-register)

increment-mouse-phase
        ((m-tem) m+a+1 m-zero a-mouse-phase)
        (jump-if-bit-clear m-tem (byte-field 1 2) not-buttons-next)
        ((m-tem) (a-constant -1))

not-buttons-next
        (popj-after-next (a-mouse-phase) m-tem)
       (no-op)
))

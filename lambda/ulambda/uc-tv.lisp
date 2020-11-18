;-*- mode: lisp; base: 8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;
(DEFCONST UC-TV '(
;;; TV ROUTINES

;(%DRAW-CHAR FONT-ARRAY-PNTR CHAR-CODE X-BIT-POS Y-BIT-POS ALU-FUNC SHEET)
;THE X-BIT-POS AND Y-BIT-POS ARE OF THE TOP LEFT CORNER OF THE CHARACTER.
; (0,0) IS THE TOP LEFT CORNER OF THE SCREEN
;THE ALU-FUNC IS SUITABLE FOR OA-REG-LOW.  GOOD VALUES ARE:
;       IOR     740
;       XOR     540
;       ANDCA   560
;       SETA    640
;YOU SHOULD USE THE TV:ALU- VARIABLES, THESE NUMBERS ARE MACHINE-DEPENDENT

;(%DRAW-RECTANGLE WIDTH HEIGHT X-BIT-POS Y-BIT-POS ALU-FUNC SHEET)
;WIDTH AND HEIGHT ARE IN BITS.  A RECTANGLE OF THE INDICATED
;SIZE, OF ALL 1S, IS CREATED AND STORED INTO THE SPECIFIED
;PART OF THE TV BUFFER USING THE SPECIFIED ALU-FUNC.  USUALLY
;THE ANDCA FUNCTION IS USED FOR ERASING, BUT XOR COULD BE USED
;FOR THE BLINKING CURSOR ETC.

;A FONT ARRAY MAY NOT BE DISPLACED OR ANYTHING HAIRY LIKE THAT.
;ITS ARRAY LEADER CONTAINS:
;       0       NOT USED IN CASE MIGHT BE FILL POINTER?
;       1       FONT (NAME-STRUCTURE-SYMBOL)
;       2       NAME
;       3       CHARACTER CELL HEIGHT
;       4       CHARACTER CELL WIDTH (USED IF ITEM 7 IS NIL)
;       5       RASTER HEIGHT
;       6       RASTER WIDTH
;       7       FLOOR 32./RASTER WIDTH (# ROWS PER WORD)
;       8       CEILING RASTER HEIGHT/#5 (# WORDS PER CHAR)
;       9       NIL OR ARRAY POINTER TO CHARACTER WIDTH TABLE
;       10      NIL OR ARRAY POINTER TO LEFT KERN TABLE

;THE DATA PART OF THE ARRAY CONTAINS AN INTEGRAL NUMBER OF WORDS
;PER CHARACTER.  EACH WORD CONTAINS AN INTEGRAL NUMBER OF ROWS
;OF RASTER, LEFT ADJUSTED AND PROCESSED FROM LEFT TO RIGHT.
;(RIGHT TO LEFT ON 32-BIT TVS)
;ALL 32 BITS OF EACH Q IN THIS ARRAY ARE USED.  FOR EASIEST PROCESSING
;BY LISP PROGRAMS, IT SHOULD BE OF 1-BIT BYTE ARRAY TYPE.

;%DRAW-CHAR ONLY WORKS FOR RASTER WIDTHS OF AT MOST 32 (DECIMAL).
;FOR LARGER WIDTHS IT TRAPS TO ILLOP.  MACROCODE DRAWS LARGER CHARACTERS
;BY DRAWING SEVERAL NARROWER CHARACTERS SIDE BY SIDE.

;NO SEQUENCE BREAKS IN TV ROUTINES DUE TO LARGE NUMBER OF ACS USED
;ALSO DUE TO SELECT-SHEET

;Will someone who understands these please document what the values mean?
(ASSIGN RASTER-ALIGN-BITS #+cadr 5 #+lambda 6 #+exp 5)
(ASSIGN RASTER-ALIGN-OFFSET #+cadr 40 #+lambda 100 #+exp 40)

;;; SELECT A SHEET FOR USE BY THE OTHER FUNCTIONS
;;; HERE ARE VARIABLES WE SET UP:
;;; A-TV-CURRENT-SHEET A-TV-SCREEN-BUFFER-ADDRESS A-TV-SCREEN-BUFFER-END-ADDRESS
;;; A-TV-SCREEN-LOCATIONS-PER-LINE A-TV-SCREEN-BUFFER-BIT-OFFSET
;;; A-TV-SCREEN-WIDTH A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT

;SUBROUTINE TO SELECT SHEET POPPED FROM PDL
;SMASHES M-A, M-B, M-C, M-D, M-E, M-Q, M-S, M-1
;ONLY REALLY GUARANTEED TO PRESERVE M-I, M-K, M-ZR
SELECT-SHEET    (ERROR-TABLE RESTART SELECT-SHEET)
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (JUMP-DATA-TYPE-NOT-EQUAL M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE))
                                  SELECT-SHEET-ARRAY)
        (POPJ-EQUAL M-C A-TV-CURRENT-SHEET)     ;Already got data
        (CALL-NOT-EQUAL M-C A-CURRENTLY-PREPARED-SHEET TRAP)
                (ERROR-TABLE TURD-ALERT M-C)
SELECT-SHEET-1
        ((VMA-START-READ) ADD M-C (A-CONSTANT 2)) ;Locations per line is second inst var
        (CHECK-PAGE-READ)
        ((A-TV-SCREEN-LOCATIONS-PER-LINE) Q-POINTER READ-MEMORY-DATA)
        ((VMA-START-READ) ADD M-C (A-CONSTANT 14.))     ;Width is 14th instance variable
        (CHECK-PAGE-READ)
        ((A-TV-SCREEN-WIDTH) Q-POINTER READ-MEMORY-DATA)
        ((VMA-START-READ) ADD M-C (A-CONSTANT 1)) ;The array is the first instance variable
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
#+exp   ((vma) md)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA
                   (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE READ-MEMORY-DATA
                   ARRAY-HEADER-SETUP-DISPATCH)
       ((m-a) m-minus-one)
     (call store-array-registers-in-accumulators)
        (call-if-bit-set-xct-next (lisp-byte %%array-displaced-bit)
                 m-array-header decode-displaced-array)
       ((m-q) a-zero)
        ((m-tem) (lisp-byte %%array-type-field) m-array-header)
        ((M-TEM) SUB M-TEM (A-CONSTANT 1))
        ((A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT) DPB M-TEM OAL-MROT A-ZERO)
        (CALL-GREATER-THAN M-TEM (A-CONSTANT 5) array-trap)     ;*****
                (ERROR-TABLE ARGTYP NUMERIC-ARRAY M-A)
        (CALL-NOT-EQUAL M-D (A-CONSTANT 2) array-trap)  ;*****
                (ERROR-TABLE ARRAY-NUMBER-DIMENSIONS M-D 2 M-A)
        ((OA-REG-LOW) A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT)
        ;; Offset of start of buffer in bits
        ((A-TV-SCREEN-BUFFER-BIT-OFFSET) DPB M-Q (BYTE-FIELD 27. 0) A-ZERO)
        ((A-TV-SCREEN-BUFFER-ADDRESS) m-array-origin)
        ((M-TEM) ADD (M-CONSTANT 40) A-TEM)     ;Size in words depends on element size
        ((M-TEM) SUB M-TEM (A-CONSTANT 5))      ; (also calculate MROT in same calc)
        ((OA-REG-LOW) DPB M-TEM OAL-MROT A-ZERO)
       ((M-TEM) (BYTE-FIELD 27. 0) m-array-length)              ;Size of buffer in words
        (POPJ-AFTER-NEXT
         (A-TV-SCREEN-BUFFER-END-ADDRESS) ADD M-TEM A-TV-SCREEN-BUFFER-ADDRESS)
       ((A-TV-CURRENT-SHEET) M-C)

SELECT-SHEET-ARRAY
        (CALL-DATA-TYPE-NOT-EQUAL M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER))
                        TRAP)
                (ERROR-TABLE ARGTYP (INSTANCE ARRAY) M-C NIL SELECT-SHEET)
        (POPJ-EQUAL M-C A-TV-CURRENT-SHEET)     ;Already got data
        ((C-PDL-BUFFER-POINTER-PUSH) M-C)
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL BITBLT-DECODE-ARRAY)
        ((A-TV-SCREEN-LOCATIONS-PER-LINE) (BYTE-FIELD 27. 5) M-1)
        ((A-TV-SCREEN-WIDTH) Q-POINTER M-D)
        ((M-TEM) (LISP-BYTE %%ARRAY-TYPE-FIELD) m-array-header)
        ((M-TEM) SUB M-TEM (A-CONSTANT 1))
        ((A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT) DPB M-TEM OAL-MROT A-ZERO)
        (CALL-GREATER-THAN M-TEM (A-CONSTANT 5) TRAP)
                (ERROR-TABLE ARGTYP NUMERIC-ARRAY M-C)
        ((A-TV-SCREEN-BUFFER-ADDRESS) M-A)
        ((A-TV-SCREEN-BUFFER-BIT-OFFSET) (BYTE-FIELD 5 0) M-Q)
        (CALL-XCT-NEXT MPY)                     ;Q-R has Y dimension
       ((M-1) A-TV-SCREEN-LOCATIONS-PER-LINE)
        (POPJ-AFTER-NEXT (A-TV-SCREEN-BUFFER-END-ADDRESS) ADD Q-R A-TV-SCREEN-BUFFER-ADDRESS)
       ((A-TV-CURRENT-SHEET) M-C)

;;; NEW TV-DRAW-CHAR MICROCODE, FOR 32-BIT TV BUFFERS, BITS NUMBERED RIGHT-TO-LEFT

;; THE CODE BELOW WILL NEVER READ OR STORE OUTSIDE THE MEMORY LIMITS SET BY THE SCREEN.
;;STORING BELOW THE REGULAR TV-BUFFER IS A PARTICULAR SCREW, SINCE A-MEMORY IS MAPPED THERE!
;;STORING ABOVE THE TV-BUFFER IS LESS DISASTEROUS NOW, BUT COULD EASILY CAUSE LOSSAGE
;;IN THE FUTURE.  IF (IN THE FUTURE) THIS CODE IS USED TO WRITE DIRECTLY INTO MEMORY
;;ARRAYS, IT WILL BE ESSENTIAL THAT IT NOT CLOBBER OUT OF BOUNDS.
;; THE ALTERNATIVE DECISION WOULD BE TO PUT THE RESPONSIBILITY ON THE CALLER OF TV-DRAW-CHAR
;;TO ASSURE THE ARGUMENTS WERE IN RANGE.  AGAINST THIS IS, (1) ITS CALLED TV-DRAW-CHAR
;;NOT %TV-DRAW-CHAR, SO IT SHOULDN'T BE CAPABLE OF DESTROYING STORAGE INTEGRITY AND
;;(2) STICKY PROBLEMS ARISE WITH CURSORS WHICH ARE PAINFUL TO DEAL WITH IN MACROCODE.
;;BASICALLY THE CURSOR WANTS TO BE ABLE TO POINT ANYWHERE ON THE SCREEN (INCLUDING THE
;;EDGE), MOVE SMOOTHLY, AND BE AT LEAST PARTIALLY VISIBLE AT ALL TIMES.
;; THE DISADVANTAGE OF CHECKING IN TV-DRAW-CHAR IS THAT SLOWS DOWN THE INNER LOOP
;;OF DRAWING CHARACTERS.  THIS IS CURRENTLY NOT TOO IMPORTANT SINCE CASES OF INTEREST ARE
;;DOMINATED BY PER CHARACTER MACRO-CODE EXECUTION TIMES, AND THE PERCENT SLOWDOWN
;;EVEN WITHIN TV-DRAW-CHAR IS SMALL.


draw-char-get-font-data
        ((pdl-buffer-pointer) add pdl-buffer-pointer (a-constant 1))
#+exp   ((vma) pdl-top)
        (dispatch-call-xct-next q-data-type c-pdl-buffer-pointer #+lambda dispatch-write-vma
           array-header-setup-dispatch)
       ((m-a) seta a-minus-one c-pdl-buffer-pointer-pop)
        ((a-font-pointer) validate-array-cache m-array-pointer)
        ((a-font-origin) m-array-origin)
        ((vma-start-read) sub m-array-pointer (a-constant 7.))
        (check-page-read)
        ((a-font-raster-height) q-pointer md)
        ((vma-start-read) sub m-array-pointer (a-constant 8.))
        (check-page-read)
        ((a-font-raster-width) q-pointer md)
        ((m-1) a-font-raster-width)
        (call-greater-than m-1 (a-constant 32.) illop)
        ((m-1) sub (m-constant 40) a-font-raster-width)        ;40 - raster width
                ;THIS HAS OVERFLOW BUG IF M-B=40, BUT WILL NEVER BE USED IN THAT CASE ANYWAY
                ;aha, but if M-B=40, it must not OA modify the dpb instruction into randomness
                ; this happens if M-Q winds up 0 and it gets a negative number after
                ; subtracting off 40 (or 100)!!
        ((a-font-raster-shift) dpb m-1 oal-bytl-1 a-1)         ;ldb pntr +40 to shift font word.
        (jump-not-equal a-font-raster-shift m-zero xd-char0)
        ((a-font-raster-shift) (a-constant 100))               ;screw case.
xd-char0
        ((vma-start-read) sub m-array-pointer (a-constant 9.))
        (check-page-read)
        ((a-font-rows-per-word) q-pointer md)
        ((vma-start-read) sub m-array-pointer (a-constant 10.))
        (check-page-read)
        ((a-font-words-per-char) q-pointer md)
        (popj)

X-DRAW-CHAR (MISC-INST-ENTRY %DRAW-CHAR)
        (CALL SELECT-SHEET)
        (CALL-XCT-NEXT TVXYADR)                         ;M-E GETS WORD ADDR, M-T BIT OFFSET
       ((M-J) DPB C-PDL-BUFFER-POINTER-POP OAL-ALUF)    ;M-J GETS ALU FUNCTION
     (ERROR-TABLE CALLS-SUB %DRAW-CHAR)
        ((M-I) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;M-I GETS CHARACTER CODE
        (call-not-equal-xct-next c-pdl-buffer-pointer-pop a-font-pointer draw-char-get-font-data)
       ((m-c) m-e)
        ((m-1) a-font-words-per-char)
        ((q-r) m-i)                                     ;and multiply by character code
        (call-xct-next mpy12)
       ((m-1) dpb m-1 (byte-field 20. 12.) a-zero)      ;M-2 GETS PRODUCT
        ((m-b) a-font-raster-width)
        ((m-q) a-font-raster-shift)
        ((m-r) a-font-rows-per-word)
        ((m-k) add m-b a-t)                             ;RASTER WIDTH PLUS BIT OFFSET
        (jump-less-or-equal-xct-next m-k (a-constant 40) xtvch4) ;JUMP IF DOESN'T CROSS
       ((m-d) a-font-raster-height)
                                                        ;NOTE C(M-T) > 0, SO NO OVERFLOW
        ((m-tem) sub (m-constant 40) a-t)               ;LENGTH OF BYTE AT LEFT OF 1ST WORD
        ((m-t) dpb m-tem oal-bytl-1 a-t)                ;DPB PNTR +40 FOR THAT BYTE
        ((m-i) dpb m-k oal-bytl-1 a-t)                  ;LDB PNTR +40 FOR BYTE AT RIGHT OF 2ND

;DROPS THROUGH
;DROPS IN
        ((vma-start-read m-e) add m-2 a-font-origin)            ;FETCH FIRST WORD OF RASTER
;M-1 WORD FROM FONT ARRAY
;M-A FONT ARRAY POINTER
;M-B RASTER WIDTH
;M-C TV BUFFER WORD ADDRESS
;M-D RASTER HEIGHT (NUMBER OF ROWS TO GO)
;M-E ADDRESS OF WORD IN FONT ARRAY
;M-I LDB PNTR +40 TO STORE INTO SECOND TV WORD
;M-J ALU FUNCTION
;M-Q LDB PNTR +40 TO SHIFT FONT WORD RIGHT ONE RASTER ROW
;M-R NUMBER OF RASTER ROWS PER WORD
;M-S NUMBER OF RASTER ROWS IN M-1
;M-T DPB PNTR +40 TO STORE INTO FIRST TV WORD
;HERE WITH FETCH OF NEXT RASTER WORD STARTED, IN THE CASE WHERE IT CROSSES A WORD BOUNDARY
XTVCH1  (CHECK-PAGE-READ)
        ((M-S) M-R)                                     ;THIS MANY ROWS IN THIS WORD
        ((M-1) READ-MEMORY-DATA)                        ;M-1 GETS WORD FROM FONT ARRAY
;HERE FOR EACH ROW OF RASTER, IN THE CASE WHERE IT CROSSES A WORD BOUNDARY
XTVCH2  (JUMP-LESS-THAN M-C A-TV-SCREEN-BUFFER-ADDRESS XTVCHO1)  ;COMMENT ABOUT RANGE CHECKING
        (JUMP-GREATER-OR-EQUAL M-C A-TV-SCREEN-BUFFER-END-ADDRESS XTVCHO1)  ;ABOVE
        ((VMA-START-READ) M-C)                          ;GET FIRST TV BUFFER WORD
        (CHECK-PAGE-READ-NO-INTERRUPT)
#+exp   ((m-tem3) sub m-t (a-constant (plus raster-align-offset -1_5)))
#+exp   ((oa-reg-low) (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-T (A-CONSTANT RASTER-ALIGN-OFFSET))         ;ALIGN RASTER
       ((M-2) DPB M-1 (BYTE-FIELD 0 0) A-ZERO)
        ((OA-REG-LOW) M-J)                              ;COMBINE AND STORE BACK
       ((WRITE-MEMORY-DATA-START-WRITE) SETZ READ-MEMORY-DATA A-2)
        (CHECK-PAGE-WRITE-unboxed)
XTVCHO1 ((VMA) ADD M-C (A-CONSTANT 1))
        (JUMP-LESS-THAN VMA A-TV-SCREEN-BUFFER-ADDRESS XTVCHO2)
        (JUMP-GREATER-OR-EQUAL VMA A-TV-SCREEN-BUFFER-END-ADDRESS XTVCHO2)
        ((VMA-START-READ) ADD M-C (A-CONSTANT 1))       ;GET SECOND TV BUFFER WORD
        (CHECK-PAGE-READ-NO-INTERRUPT)
#+exp   ((m-tem3) sub m-i (a-constant (plus raster-align-offset -1_5)))
#+exp   ((OA-REG-LOW) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-I (A-CONSTANT RASTER-ALIGN-OFFSET))         ;ALIGN RASTER
       ((M-2) (BYTE-FIELD 0 0) M-1)
        ((OA-REG-LOW) M-J)                              ;COMBINE AND STORE BACK
       ((WRITE-MEMORY-DATA-START-WRITE) SETZ READ-MEMORY-DATA A-2)
        (CHECK-PAGE-WRITE-unboxed)
XTVCHO2 (JUMP-LESS-OR-EQUAL M-D (A-CONSTANT 1) XFALSE)  ;STOP IF DONE
        ((M-D) SUB M-D (A-CONSTANT 1))
        ((M-C) ADD M-C A-TV-SCREEN-LOCATIONS-PER-LINE)  ;ADVANCE TO NEXT LINE
#+exp   ((m-tem3) sub m-q (a-constant (plus raster-align-offset -1_5)))
#+exp   ((OA-REG-LOW) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-Q (A-CONSTANT RASTER-ALIGN-OFFSET))         ;SHIFT RASTER RIGHT
       ((M-1) (BYTE-FIELD 0 0) M-1)
        (JUMP-GREATER-THAN-XCT-NEXT M-S (A-CONSTANT 1) XTVCH2)  ;JUMP IF WORD NOT USED UP
       ((M-S) SUB M-S (A-CONSTANT 1))
        (JUMP-XCT-NEXT XTVCH1)                          ;FETCH NEW WORD
       ((VMA-START-READ M-E) ADD M-E (A-CONSTANT 1))

;THIS VERSION OF THE ABOVE IS FOR THE FAST CASE, WHERE IT DOES NOT CROSS A WORD BOUNDARY
XTVCH4  (jump-less-than m-c a-tv-screen-buffer-address xtvch4-slow)
     ;; 2048. is 64 32-word lines, a fairly conservative cutoff.
        ((m-3) sub a-tv-screen-buffer-end-address (a-constant 2048.))
        (jump-greater-than m-c m-3 xtvch4-slow)

;; This case doesn't bother to check for clipping.
        ((M-T) DPB M-B (BYTE-FIELD 6 #+cadr 5 #+lambda 6 #+exp 5)
                   A-T) ;DPB PNTR +40 FOR ALIGNING RASTER
                ;BYTE-FIELD IS ALMOST OAL-BYTL-1
        ((VMA-START-READ M-E) ADD M-2 a-font-origin)            ;FETCH FIRST WORD OF RASTER
;M-1 WORD FROM FONT ARRAY
;M-A FONT ARRAY POINTER
;M-B RASTER WIDTH
;M-C TV BUFFER WORD ADDRESS
;M-D RASTER HEIGHT (NUMBER OF ROWS TO GO)
;M-E ADDRESS OF WORD IN FONT ARRAY
;M-J ALU FUNCTION
;M-Q LDB PNTR +RASTER-ALIGN-OFFSET TO SHIFT FONT WORD RIGHT ONE RASTER ROW
;M-R NUMBER OF RASTER ROWS PER WORD
;M-S NUMBER OF RASTER ROWS IN M-1
;M-T DPB PNTR +RASTER-ALIGN-OFFSET TO STORE INTO TV WORD
;HERE WITH FETCH OF NEXT RASTER WORD STARTED
XTVCH5  (CHECK-PAGE-READ)
        ((M-S) M-R)                                     ;THIS MANY ROWS IN THIS WORD
        ((M-1) READ-MEMORY-DATA)                        ;M-1 GETS WORD FROM FONT ARRAY
;HERE FOR EACH ROW OF RASTER
XTVCH6  ((VMA-START-READ) M-C)                          ;GET TV BUFFER WORD
        (CHECK-PAGE-READ-NO-INTERRUPT)
#+exp   ((m-tem3) sub m-t (a-constant (plus raster-align-offset -1_5)))
#+exp   ((OA-REG-LOW) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-T (A-CONSTANT RASTER-ALIGN-OFFSET))         ;ALIGN RASTER
       ((M-2) DPB M-1 (BYTE-FIELD 0 0) A-ZERO)
        ((OA-REG-LOW) M-J)                              ;COMBINE AND STORE BACK
       ((WRITE-MEMORY-DATA-START-WRITE) SETZ READ-MEMORY-DATA A-2)
        (CHECK-PAGE-WRITE-unboxed)
XTVCHO3 (JUMP-LESS-OR-EQUAL M-D (A-CONSTANT 1) XFALSE)  ;STOP IF DONE
        ((M-D) SUB M-D (A-CONSTANT 1))
        ((M-C) ADD M-C A-TV-SCREEN-LOCATIONS-PER-LINE)  ;ADVANCE TO NEXT LINE
#+exp   ((m-tem3) sub m-q (a-constant (plus raster-align-offset -1_5)))
#+exp   ((OA-REG-LOW) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-Q (A-CONSTANT RASTER-ALIGN-OFFSET))         ;SHIFT RASTER RIGHT
       ((M-1) (BYTE-FIELD 0 0) M-1)
        (JUMP-GREATER-THAN-XCT-NEXT M-S (A-CONSTANT 1) XTVCH6)  ;JUMP IF WORD NOT USED UP
       ((M-S) SUB M-S (A-CONSTANT 1))
        (JUMP-XCT-NEXT XTVCH5)                          ;FETCH NEW WORD
       ((VMA-START-READ M-E) ADD M-E (A-CONSTANT 1))

;; This case checks for clipping every time around, which is why it's slow.
xtvch4-slow
        ((M-T) DPB M-B (BYTE-FIELD 6 #+cadr 5 #+lambda 6 #+exp 5) A-T)
                ;DPB PNTR +40 FOR ALIGNING RASTER BYTE-FIELD IS ALMOST OAL-BYTL-1
        ((VMA-START-READ M-E) ADD M-2 a-font-origin)            ;FETCH FIRST WORD OF RASTER
XTVCH5-slow
        (CHECK-PAGE-READ)
        ((M-S) M-R)                                     ;THIS MANY ROWS IN THIS WORD
        ((M-1) READ-MEMORY-DATA)                        ;M-1 GETS WORD FROM FONT ARRAY
;HERE FOR EACH ROW OF RASTER
XTVCH6-slow
        (JUMP-LESS-THAN M-C A-TV-SCREEN-BUFFER-ADDRESS XTVCHO3-slow)  ;COMMENT ABOUT RANGE CHECKING
        (JUMP-GREATER-OR-EQUAL M-C A-TV-SCREEN-BUFFER-END-ADDRESS XTVCHO3-slow)  ;ABOVE
        ((VMA-START-READ) M-C)                          ;GET TV BUFFER WORD
        (CHECK-PAGE-READ-NO-INTERRUPT)
#+exp   ((m-tem3) sub m-t (a-constant (plus raster-align-offset -1_5)))
#+exp   ((OA-REG-LOW) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-T (A-CONSTANT RASTER-ALIGN-OFFSET))         ;ALIGN RASTER
       ((M-2) DPB M-1 (BYTE-FIELD 0 0) A-ZERO)
        ((OA-REG-LOW) M-J)                              ;COMBINE AND STORE BACK
       ((WRITE-MEMORY-DATA-START-WRITE) SETZ READ-MEMORY-DATA A-2)
        (CHECK-PAGE-WRITE-unboxed)
XTVCHO3-slow
        (JUMP-LESS-OR-EQUAL M-D (A-CONSTANT 1) XFALSE)  ;STOP IF DONE
        ((M-D) SUB M-D (A-CONSTANT 1))
        ((M-C) ADD M-C A-TV-SCREEN-LOCATIONS-PER-LINE)  ;ADVANCE TO NEXT LINE
#+exp   ((m-tem3) sub m-q (a-constant (plus raster-align-offset -1_5)))
#+exp   ((OA-REG-LOW) ldb (byte-field 10. 0) m-tem3)
#+lambda((OA-REG-LOW) SUB M-Q (A-CONSTANT RASTER-ALIGN-OFFSET))         ;SHIFT RASTER RIGHT
       ((M-1) (BYTE-FIELD 0 0) M-1)
        (JUMP-GREATER-THAN-XCT-NEXT M-S (A-CONSTANT 1) XTVCH6-slow)     ;JUMP IF WORD NOT USED UP
       ((M-S) SUB M-S (A-CONSTANT 1))
        (JUMP-XCT-NEXT XTVCH5-slow)                             ;FETCH NEW WORD
       ((VMA-START-READ M-E) ADD M-E (A-CONSTANT 1))

;12-BIT UNSIGNED MULTIPLY
; M-1<31:12> TIMES Q-R <11:0> TO M-2<31:0>.  M-1<11:0> MUST BE ZERO.
MPY12
;       ((m-2) a-processor-switches)
;       (jump-if-bit-set (lisp-byte %%processor-switch-use-multiplier-in-uc-tv)
;                        m-2 mpy12-use-chip)
        ((M-2) MULTIPLY-STEP A-1 M-ZERO)
(REPEAT 9 ((M-2) MULTIPLY-STEP M-2 A-1))
        (POPJ-AFTER-NEXT (M-2) MULTIPLY-STEP M-2 A-1)
       ((M-2) MULTIPLY-STEP M-2 A-1)

;mpy12-use-chip  ; probably buggy - check before using
;       ((m-1) ldb m-1 (byte-field 20. 12.) a-zero)
;       ((multiplier) dpb q-r (byte-field 16. 16.) a-1)
;                                               ;put the second operand in the low
;                                               ;bits, the first in the high bits
;                                               ;and deposit it in the multiplier
;       (popj-after-next (multiplier) setz)     ;RATS! extra uinst to clock the multiplier
;                                               ;because source-ft seems not to work
;       ((m-2) ldb (byte-field 24. 0) multiplier a-zero)

;NEW VERSION OF TVXYADR
;POP OFF Y-BIT-POS AND X-BIT-POS AND CONVERT TO WORD AND BIT ADDRESS
;M-E GETS ABSOLUTE WORD ADDRESS, M-T GETS BIT OFFSET FROM RIGHT (LEFT) IF 32 (16) BIT.
;CLOBBER M-1, M-2, M-TEM
TVXYADR (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP NIL)
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 20. 12.) A-ZERO) ;Y POSITION (LSH 12)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP NIL)
        (CALL-XCT-NEXT MPY12)
       ((Q-R) A-TV-SCREEN-LOCATIONS-PER-LINE)           ;M-2 GETS OFFSET TO START OF LINE
TVXYAD0 ((OA-REG-LOW) A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT)
        ;; X coordinate gets multiplied by pixel size
        ((M-TEM) DPB C-PDL-BUFFER-POINTER-POP Q-POINTER A-ZERO)
        ((M-TEM) ADD M-TEM A-TV-SCREEN-BUFFER-BIT-OFFSET)
        ((M-1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 5) 5) M-TEM)             ;WORD PART OF X POSITION
#+lambda((OA-REG-HIGH) (BYTE-FIELD 1 (DIFFERENCE Q-POINTER-WIDTH 5 1)) M-1)
#+exp   ((m-tem3) (byte-field 1 (difference q-pointer-width 5 1)) m-1)
#+exp   ((oa-reg-high) dpb m-tem3 oah-m-src a-zero)
        ((M-1) SELECTIVE-DEPOSIT M-ZERO
         (BYTE-FIELD (DIFFERENCE 37. Q-POINTER-WIDTH) (DIFFERENCE Q-POINTER-WIDTH 5))
         A-1)
        ((M-E) ADD M-2 A-1)                             ;RELATIVE WORD ADDRESS
        (POPJ-AFTER-NEXT (M-E) ADD M-E A-TV-SCREEN-BUFFER-ADDRESS)
       ((M-T) (BYTE-FIELD 5 0) M-TEM)                   ;BIT PART OF X POSITION

;;; TV-ERASE width height x y alu

X-DRAW-RECTANGLE (MISC-INST-ENTRY %DRAW-RECTANGLE)
        (CALL SELECT-SHEET)
        (CALL-XCT-NEXT TVXYADR)                         ;M-E := ADDR, M-T := BIT OFFSET
       ((M-J) DPB C-PDL-BUFFER-POINTER-POP OAL-ALUF)    ;ALU FUNC
            (ERROR-TABLE CALLS-SUB %DRAW-RECTANGLE)
        ((M-D) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;HEIGHT IN RASTER LINES
XTVERS5 ((OA-REG-LOW) A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT)
        ((M-C) DPB C-PDL-BUFFER-POINTER-POP Q-POINTER A-ZERO)   ;WIDTH IN BITS
        ;; Fix up tag field
        ((M-C) SELECTIVE-DEPOSIT M-C Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP-EQUAL M-D A-ZERO XFALSE)                  ;DO NOTHING IF HEIGHT IS ZERO
        (JUMP-EQUAL M-C (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) XFALSE)   ;OR WIDTH
        ((M-C C-PDL-BUFFER-POINTER-PUSH) ADD M-T A-C)   ;ADJUST WIDTH TO PRETEND
                                                        ; STARTING ON WORD BOUNDARY
        ((M-C) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 5) 5) M-C)       ;WIDTH IN WORDS
        ((M-Q) (BYTE-FIELD 5 0) (M-CONSTANT -1))        ;37 ;LOAD HANDY CONSTANT, USED LATER
        ((M-K) SUB M-Q A-T)                             ;BYTL-1 FOR FIRST WORD
#+lambda((OA-REG-LOW) DPB M-K OAL-BYTL-1 A-T)           ;GET MASK FOR BITS IN LEFT OF 1ST WD
#+exp   ((m-tem3) add m-k (a-constant 1))
#+exp   ((oa-reg-low) dpb m-tem3 oal-bytl-1 a-t)
        ((M-K) DPB (M-CONSTANT -1) (BYTE-FIELD 0 0) A-ZERO)
        (JUMP-EQUAL-XCT-NEXT M-C A-ZERO XTVERS3)        ;JUMP IF NARROW (LESS THAN 1 WORD)
XTVERS0((M-B) M-D)                                      ;COPY OF HEIGHT
        (jump-not-equal m-j (a-constant 20) xtvers4)
        (jump-equal m-k a-minus-one xtvers6)
xtvers4
        (CALL-LESS-THAN M-E A-TV-SCREEN-BUFFER-ADDRESS TRAP)
  (ERROR-TABLE TV-ERASE-OFF-SCREEN)
        (CALL-GREATER-OR-EQUAL M-E A-TV-SCREEN-BUFFER-END-ADDRESS TRAP)
  (ERROR-TABLE TV-ERASE-OFF-SCREEN)                     ;This is special.
        ((VMA-START-READ) M-E)                          ;FETCH TOP LEFT-HAND WORD

;;; BUG:  This can read 1 location past a-tv-screen-buffer-end-address
;;; I'm tired of this silly bug crashing my machine.  First attempt to fix it without
;;; slowing down much follows.

        (CHECK-PAGE-READ-NO-INTERRUPT)                  ;DO FIRST COLUMN
        (JUMP-LESS-OR-EQUAL M-B A-ZERO XTVERS2)         ;JUMP IF COLUMN ALL DONE

XTVERS1 (CHECK-PAGE-READ-NO-INTERRUPT)                  ;DO FIRST COLUMN
;       (JUMP-LESS-OR-EQUAL M-B A-ZERO XTVERS2)         ;JUMP IF COLUMN ALL DONE
        (CALL-GREATER-OR-EQUAL VMA A-TV-SCREEN-BUFFER-END-ADDRESS TRAP)
  (ERROR-TABLE TV-ERASE-OFF-SCREEN)                     ;This is special.
        ((OA-REG-LOW) M-J)
        ((WRITE-MEMORY-DATA-START-WRITE) SETZ READ-MEMORY-DATA A-K)
        (CHECK-PAGE-WRITE-unboxed)
        ((M-B) SUB M-B (A-CONSTANT 1))
        (jump-less-or-equal m-b a-zero xtvers2)         ;jump if column all done.
        (JUMP-XCT-NEXT XTVERS1)
       ((VMA-START-READ) ADD VMA A-TV-SCREEN-LOCATIONS-PER-LINE)

XTVERS2 ((M-E) ADD M-E (A-CONSTANT 1))                  ;NEXT COLUMN
        ((M-C) SUB M-C (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-C A-ZERO XTVERS0)
       ((M-K) SETO)                                     ;DO MIDDLE COLUMNS, MASK IS ALL BITS
        (JUMP-LESS-THAN M-C A-ZERO XFALSE)              ;ALL DONE (SECOND TIME THROUGH HERE)
XTVERS3 ((M-B) AND C-PDL-BUFFER-POINTER-POP A-Q)        ;NUMBER BITS TO DO IN LAST COLUMN
        (JUMP-EQUAL M-B A-ZERO XFALSE)                  ;NO LAST COLUMN, RETURN NIL
        ((M-B) SUB M-B (A-CONSTANT 1))
                ;CLEAR THAT MANY BITS ON THE LEFT
#+exp   ((m-tem3) add m-b (a-constant 1))
        ((OA-REG-LOW) DPB #+lambda M-B #+exp m-tem3 OAL-BYTL-1 A-ZERO)
        ((M-K) (BYTE-FIELD 0 0) M-K)
        (JUMP XTVERS0)

xtvers6 ((md) setz)
        ((vma) sub m-e a-tv-screen-locations-per-line)
xtvers7 (jump-less-or-equal m-b a-zero xtvers2)         ;jump if column all done.
        ((vma-start-write) add vma a-tv-screen-locations-per-line)
        (check-page-write-unboxed)
        (jump-xct-next xtvers7)
       ((m-b) sub m-b (a-constant 1))


;;;Line drawing

X-DRAW-LINE (MISC-INST-ENTRY %DRAW-LINE)
        (CALL SELECT-SHEET)
TVDRL0  ((A-DRAW-LINE-DRAW-LAST-POINT) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;endpoint flag
        ((A-DRAW-LINE-DRAW-FIRST-POINT) Q-POINTER A-V-TRUE)
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 4)
        ((M-J) DPB C-PDL-BUFFER-POINTER-POP OAL-ALUF)   ;M-J ALU function
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 3)
            (ERROR-TABLE ARG-POPPED 0 M-J A-DRAW-LINE-DRAW-LAST-POINT)
        ((M-4) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;M-4 Y
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 2)
            (ERROR-TABLE ARG-POPPED 0 M-4 M-J A-DRAW-LINE-DRAW-LAST-POINT)
        ((M-TEM) Q-POINTER C-PDL-BUFFER-POINTER-POP)    ;M-TEM X
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 1)
            (ERROR-TABLE ARG-POPPED 0 M-TEM M-4 M-J A-DRAW-LINE-DRAW-LAST-POINT)
        ((M-2) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;M-2 Y0
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 0)
            (ERROR-TABLE ARG-POPPED 0 M-2 M-TEM M-4 M-J A-DRAW-LINE-DRAW-LAST-POINT)
        ((M-S) SUB M-4 A-2)                             ;M-S DY
        ((M-1) Q-POINTER C-PDL-BUFFER-POINTER)          ;M-1 X0
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-TEM A-1 TVDRL1)       ;DX0?
       ((M-R) SUB M-TEM A-1)                            ;M-R DX
        ((M-R) SUB M-ZERO A-R)                          ;yes, exch X and X0
        ((C-PDL-BUFFER-POINTER) DPB M-TEM Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-S) SUB M-ZERO A-S)                          ;and Y and Y0
        ((M-2) M-4)
        ((A-DRAW-LINE-DRAW-FIRST-POINT) A-DRAW-LINE-DRAW-LAST-POINT)    ;and endpoint flags
        ((A-DRAW-LINE-DRAW-LAST-POINT) Q-POINTER A-V-TRUE)
        ;;DX now assured of being non-negative
TVDRL1  ((C-PDL-BUFFER-POINTER-PUSH) DPB M-2 Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL TVXYADR)                                  ;M-E addr M-T bit offset
            (ERROR-TABLE CALLS-SUB %DRAW-LINE)
            (ERROR-TABLE ARG-POPPED 0 M-1 M-2 M-TEM M-4 M-J A-DRAW-LINE-DRAW-LAST-POINT)
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-S A-ZERO TVDRL2)
       ((M-I) A-TV-SCREEN-LOCATIONS-PER-LINE)           ;M-I Y increment with correct sign
        ((M-I) SUB M-ZERO A-I)
        ((M-S) SUB M-ZERO A-S)
TVDRL2  ((M-K) SUB M-R A-S)                             ;M-K flag for DY>DX
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-K A-ZERO TVDRL3)
       ((M-C) M-R)                                      ;number of points to do on long side
        ((M-C) M-S)                                     ;exch DX and DY
        ((M-S) M-R)
        ((M-R) M-C)
TVDRL3  ((M-A) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 1) 1) M-R)       ;M-A <length of long side>/2
        ((OA-REG-LOW) A-TV-SCREEN-BUFFER-PIXEL-SIZE-MROT)       ;Log of pixel size
        ((M-2) DPB (M-CONSTANT -1) (BYTE-FIELD 1 0) A-ZERO)     ;Number of bits in pixel
        ((M-1) SUB M-2 (A-CONSTANT 1))
        ((M-1) DPB M-1 OAL-BYTL-1 A-ZERO)               ;Position for hardware byte size
        (JUMP-EQUAL M-ZERO A-DRAW-LINE-DRAW-FIRST-POINT TVDRL7) ;Skip first point?
TVDRL4  (JUMP-LESS-THAN M-E A-TV-SCREEN-BUFFER-ADDRESS TVDRL7) ;Clip
        (JUMP-GREATER-OR-EQUAL M-E A-TV-SCREEN-BUFFER-END-ADDRESS TVDRL7) ;Clip
        ((VMA-START-READ) M-E)                          ;get data
        (CHECK-PAGE-READ-NO-INTERRUPT)
#+lambda((OA-REG-LOW) DPB M-T OAL-MROT A-1)             ;bit offset
#+exp   ((m-tem3) add m-1 (a-constant 1_5))
#+exp   ((m-tem3) ldb (byte-field 10. 0) m-tem3)
#+exp   ((oa-reg-low) dpb m-t oal-mrot a-tem3)
       ((M-TEM) SELECTIVE-DEPOSIT (BYTE-FIELD 0 0) (M-CONSTANT -1))     ;M-TEM byte to twiddle
        ((OA-REG-LOW) M-J)                              ;ALU
       ((WRITE-MEMORY-DATA-START-WRITE) SETZ READ-MEMORY-DATA A-TEM)    ;munge it
        (CHECK-PAGE-WRITE-unboxed)
TVDRL7  (JUMP-GREATER-THAN-XCT-NEXT M-C (A-CONSTANT 1) TVDRL8)  ;lots more to do
       ((M-C) SUB M-C (A-CONSTANT 1))
        (JUMP-LESS-THAN M-C A-ZERO XFALSE)              ;return if done stepping long side
        (JUMP-EQUAL M-ZERO A-DRAW-LINE-DRAW-LAST-POINT XFALSE)  ;or skipping last point
TVDRL8  ((M-A) SUB M-A A-S)
        (JUMP-LESS-THAN M-A A-ZERO TVDRL5)              ;time to bump short side too?
        (JUMP-GREATER-OR-EQUAL M-K A-ZERO TVDRL6)       ;just increment long side
        (JUMP-XCT-NEXT TVDRL4)                          ;y side longer
TVDRL5 ((M-E) ADD M-E A-I)                              ;increment both x and y
        ((M-A) ADD M-A A-R)
TVDRL6  ((M-T) ADD M-T A-2)                             ;increment x
        (JUMP-LESS-THAN M-T (A-CONSTANT 40) TVDRL4)     ;see if past end of word
        ((M-E) ADD M-E (A-CONSTANT 1))                  ;move to next word
        (JUMP-XCT-NEXT TVDRL4)
       ((M-T) SETZ)

; (BITBLT alu width height from-array from-x from-y to-array to-x to-y)
;Features:
; The X and Y arguments specify the coordinates of the upper-left-hand
;  corner of the <width> x <height> region to be operated on.  The operation
;  is normally performed top to bottom then left to right, but making
;  width or height negative will make it go the other way, useful when
;  regions overlap.  The X and Y should still be for the top-left corner.
; Works on any numeric array type.  For more than 1-bit bytes, the X and Y arguments
;  are in bytes rather than bits.
; If you run off the edge of the source array, it wraps around
;  to the opposite edge.  This is intended to allow such
;  things as replication of small stipple patterns through a large screen area.
;  If you run off the edge of the destination array, an error occurs.
; The function cannot be made to reference outside of the argument arrays
;  by giving it bad arguments.
;Crocks:
; Requires that the first dimension of the array be a multiple of 32. bits.
; Index-offset arrays do not work with wrap-around.
;Register conventions are commented a little bit later.

BITBLT (MISC-INST-ENTRY BITBLT)
        (CALL BITBLT-DECODE-ARRAY)                      ;Decode destination
        ((M-C) M-1)                                     ;Save BITBLT-DST-WIDTH
        ((M-ZR) SUB Q-R A-4)                            ;Save eventual contents of M-T
        ((M-I) M-A)                                     ;Save eventual contents of M-D
        ((M-R) M-Q)                                     ;X offset in bits
        ((M-1) SUB (M-CONSTANT (DIFFERENCE Q-POINTER-WIDTH 1)) A-3)     ;Make DPB ptr to convert width
        ((M-K) DPB M-1 OAL-BYTL-1 A-3)                  ; from bytes to bits
        (CALL BITBLT-DECODE-ARRAY)                      ;Decode source
        ;; No sequence breaks after this point
        ((A-BITBLT-DST-WIDTH) M-C)                      ;Get dest parameters saved above
        ((M-T) M-ZR)
        ((M-D) M-I)
        ((A-BITBLT-SRC-WIDTH) M-1)                      ;Save source parameters
        ((A-BITBLT-SRC-WIDTH-WORDS) (BYTE-FIELD 27. 5) M-1) ;This copy is always positive
        ;; Set up the vertical address increments and column heights for the arrays
        ((M-B) (BYTE-FIELD 27. 5) M-1)                  ;Word increment between source rows
        ((M-C) Q-R)                                     ;Total number of source rows
        ((A-BITBLT-SRC-Y Q-R) M-4)                      ;Number of rows down we start at
        (CALL-XCT-NEXT MPY)                             ;Initial Y
       ((M-1) A-BITBLT-SRC-WIDTH-WORDS)                 ; times words per row
        ((A-BITBLT-SRC-Y-OFFSET) Q-R)                   ; gives offset from top of column
        ((M-A) SUB M-A A-BITBLT-SRC-Y-OFFSET)           ;Start M-A at top of column
        ((M-1) A-BITBLT-DST-WIDTH)
        ((M-E) (BYTE-FIELD 27. 5) M-1)                  ;Word increment between dest rows
        ;; Get the height in M-S.  If negative, make positive and rearrange parameters
        ;; so that it will start at the bottom and move up.
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 2)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT BOXED-SIGN-BIT C-PDL-BUFFER-POINTER BITBLT-1)
       ((M-S) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-S) SUB M-ZERO A-S)                          ;Negative height, change around
        ((M-S) Q-POINTER M-S)
        ((M-B) SUB M-ZERO A-B)
        ((M-E) SUB M-ZERO A-E)
        ((M-1) SUB M-C (A-CONSTANT 1))                  ;Change tops of columns to bottoms
        (CALL-XCT-NEXT MPY)
       ((Q-R) SUB M-ZERO A-B)
        ((M-A) ADD Q-R A-A)
        ((M-1) ADD M-S A-BITBLT-SRC-Y)                  ;Move source offset to other end
        (CALL-XCT-NEXT DIV)                             ;Taking modulo size of source
       ((M-2) M-C)
        ((A-BITBLT-SRC-Y Q-R) SUB M-C A-1)              ;Number rows offset is up from bottom
        (CALL-XCT-NEXT MPY)
       ((M-1) SUB M-ZERO A-BITBLT-SRC-WIDTH-WORDS)
        ((A-BITBLT-SRC-Y-OFFSET) Q-R)                   ;Negative offset up from bottom
        ((M-1) SUB M-S (A-CONSTANT 1))
        (CALL-XCT-NEXT MPY)
       ((Q-R) SUB M-ZERO A-E)
        ((M-D) ADD Q-R A-D)
BITBLT-1        ;Now get the width, check for negative
        (DISPATCH Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
            (ERROR-TABLE ARGTYP FIXNUM PP 1)
#+lambda((OA-REG-LOW) M-K)                              ;Convert from bytes to bits
#+exp   ((m-tem3) add m-k (a-constant 1_5))
#+exp   ((oa-reg-low) ldb (byte-field 10. 0) m-tem3)
        ((M-1) DPB C-PDL-BUFFER-POINTER-POP A-ZERO)
        (JUMP-IF-BIT-SET-XCT-NEXT BOXED-SIGN-BIT M-1 BITBLT-RTL) ;Neg width means right to left
       ((A-ALUF) DPB C-PDL-BUFFER-POINTER-POP OAL-ALUF) ;Alu function
        ((A-BITBLT-HOR-COUNT) SUB M-ZERO A-1)           ;Sign-extended negative width
;Drops through into BITBLT-LTR
;drops in
        ;; Now, enter a loop by columns.  Each column is as wide as will avoid
        ;; crossing word boundaries in source and in destination.
        ;; This is for left-to-right case
BITBLT-LTR
        ;; Compute width of column to be done.
        ((M-1) (BYTE-FIELD 5 0) M-Q)                    ;Source bit offset
        ((M-2) (BYTE-FIELD 5 0) M-R)                    ;Destination bit offset
        ((M-3) SUB M-Q A-BITBLT-SRC-WIDTH)              ;Negative bits left in source array
        ((M-J) SUB M-1 (A-CONSTANT 40))                 ;Negative bits left in source word
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-J A-3 BITBLT-LTR-1)   ;Take the smaller
       ((M-I) SUB M-2 A-1)                              ;Left rotate for source
        ((M-J) M-3)                                     ;Reached right-hand end of array
BITBLT-LTR-1
        ((M-3) SUB M-R A-BITBLT-DST-WIDTH)              ;Negative bits left in dest array
        ((M-1) SUB M-2 (A-CONSTANT 40))                 ;Negative bits left in dest word
        (JUMP-GREATER-OR-EQUAL M-1 A-3 BITBLT-LTR-2)    ;Take the smaller
        ((M-1) M-3)
BITBLT-LTR-2
        (JUMP-GREATER-OR-EQUAL M-J A-1 BITBLT-LTR-3)    ;Take smaller of src, dest
        ((M-J) M-1)
BITBLT-LTR-3
        (JUMP-GREATER-OR-EQUAL M-J A-BITBLT-HOR-COUNT BITBLT-LTR-4) ;Min with overall count
        ((M-J) A-BITBLT-HOR-COUNT)
BITBLT-LTR-4    ;Here M-J has negative width of this column
        (JUMP-GREATER-OR-EQUAL M-J A-ZERO XFALSE)       ;Return NIL if zero width (can't do)
        ((M-TEM) M-A-1 M-ZERO A-J)                      ;Positive byte length minus one
        ((M-K) DPB M-TEM
         (BYTE-FIELD 27. RASTER-ALIGN-BITS) A-R)        ;Byte pointer to part of destination
        (CALL-XCT-NEXT BITBLT-INNER-LOOP)               ; to be modified
       ((A-BITBLT-HOR-COUNT) M+A+1 M-TEM A-BITBLT-HOR-COUNT)    ;Advance negative bit count
        (JUMP-LESS-OR-EQUAL M-ZERO A-BITBLT-HOR-COUNT XFALSE)   ;Return NIL if done
        ((M-TEM) (BYTE-FIELD 5 0) (M-CONSTANT -1) A-Q)  ;Last bit this source word
        ((M-Q) SUB M-Q A-J)                             ;Advance source X bit offset
        (JUMP-LESS-OR-EQUAL M-Q A-TEM BITBLT-LTR-5)
        ((M-A) ADD M-A (A-CONSTANT 1))                  ;Entered next word
BITBLT-LTR-5
        (JUMP-LESS-THAN M-Q A-BITBLT-SRC-WIDTH BITBLT-LTR-6)
        ((M-Q) SUB M-Q A-BITBLT-SRC-WIDTH)              ;Wrap around (M-Q should get 0 here)
        ((M-A) SUB M-A A-BITBLT-SRC-WIDTH-WORDS)
BITBLT-LTR-6
        ((M-TEM) (BYTE-FIELD 5 0) (M-CONSTANT -1) A-R)  ;Last bit this destination word
        ((M-R) SUB M-R A-J)                             ;Advance destination X bit offset
        (JUMP-LESS-OR-EQUAL M-R A-TEM BITBLT-LTR-7)
        ((M-D) ADD M-D (A-CONSTANT 1))                  ;Entered next word
BITBLT-LTR-7
        (JUMP-LESS-THAN M-R A-BITBLT-DST-WIDTH BITBLT-LTR)      ;Loop for more columns
        (CALL TRAP)
            (ERROR-TABLE BITBLT-DESTINATION-TOO-SMALL)

        ;; Now, enter a loop by columns.  Each column is as wide as will avoid
        ;; crossing word boundaries in source and in destination.
        ;; This is for right-to-left case
BITBLT-RTL
        ((M-1) Q-POINTER M-1 (A-CONSTANT -1))           ;Sign-extended negative width
        ((A-BITBLT-HOR-COUNT) SUB M-ZERO A-1)           ;We want it positive
        ;; Adjust parameters to point to after right-most column to be done
        ((M-TEM) (BYTE-FIELD 27. 5) M-R)
        ((M-D) SUB M-D A-TEM)
        ((M-R) ADD M-R A-BITBLT-HOR-COUNT)              ;Bit offset to right of dest area
        (CALL-GREATER-THAN M-R A-BITBLT-DST-WIDTH TRAP)
            (ERROR-TABLE BITBLT-DESTINATION-TOO-SMALL)
        ((M-TEM) (BYTE-FIELD 27. 5) M-R)
        ((M-D) ADD M-D A-TEM)                           ;Corresponding word address
        ((M-TEM) (BYTE-FIELD 27. 5) M-Q)
        ((M-A) SUB M-A A-TEM)
        ((M-1) ADD M-Q A-BITBLT-HOR-COUNT)              ;Bit offset to right of source area
        (CALL-XCT-NEXT DIV)                             ;Take modulo source width
       ((M-2) A-BITBLT-SRC-WIDTH)                       ; to effect wrap-around
        ((M-Q) M-1)                                     ;Remainder is initial bit offset
        ((M-TEM) (BYTE-FIELD 27. 5) M-Q)
        ((M-A) ADD M-A A-TEM)                           ;Corresponding word address
BITBLT-RTL-LOOP
        ;; Compute width of column to be done, to left of these bit offsets
        (JUMP-GREATER-THAN M-Q A-ZERO BITBLT-RTL-0)     ;Check for wrap-around
        ((M-Q) A-BITBLT-SRC-WIDTH)
        ((M-A) ADD M-A A-BITBLT-SRC-WIDTH-WORDS)
BITBLT-RTL-0
        ((M-J) (BYTE-FIELD 5 0) M-Q)                    ;Source bit offset
        (JUMP-NOT-EQUAL M-J A-ZERO BITBLT-RTL-1)        ;Jump if not at left of word
        ((M-A) SUB M-A (A-CONSTANT 1))                  ;Else back up to previous word
        ((M-J) (A-CONSTANT 40))                         ;And there are 40 bits in it
BITBLT-RTL-1
        ((M-2) (BYTE-FIELD 5 0) M-R)                    ;Destination bit offset
        (JUMP-NOT-EQUAL-XCT-NEXT M-2 A-ZERO BITBLT-RTL-2) ;Jump if not at left of word
       ((M-I) SUB M-2 A-J)                              ;Left rotate for source
        ((M-D) SUB M-D (A-CONSTANT 1))                  ;Else back up to previous word
        ((M-2) (A-CONSTANT 40))                         ;And there are 40 bits in it
BITBLT-RTL-2
        (JUMP-LESS-OR-EQUAL M-J A-2 BITBLT-RTL-3)       ;Take lesser of bits left in words
        ((M-J) M-2)
BITBLT-RTL-3
        (JUMP-LESS-OR-EQUAL M-J A-Q BITBLT-RTL-4)       ;Min with bits left in source array
        ((M-J) M-Q)                                     ;(Dest array already range-checked)
BITBLT-RTL-4
        (JUMP-LESS-OR-EQUAL M-J A-BITBLT-HOR-COUNT BITBLT-RTL-5)
        ((M-J) A-BITBLT-HOR-COUNT)                      ;Min with bits left to do
BITBLT-RTL-5    ;M-J now has positive number of bits in this column
        (JUMP-LESS-OR-EQUAL M-J A-ZERO XFALSE)          ;Return NIL if zero width (can't do)
        ((M-K) SUB M-R A-J)                             ;<5:0>=MROT for dest bits to modify
        ((M-TEM) SUB M-J (A-CONSTANT 1))                ;BYTL-1 for dest bits to modify
        ((M-K) DPB M-TEM
         (BYTE-FIELD 27. RASTER-ALIGN-BITS) A-K)        ;Byte pointer to part of destination
        ((M-TEM) SUB M-ZERO A-J)                        ; to be modified
        (CALL-XCT-NEXT BITBLT-INNER-LOOP)
       ((A-BITBLT-HOR-COUNT) ADD M-TEM A-BITBLT-HOR-COUNT)      ;Decrease bit count
        ((M-Q) SUB M-Q A-J)                             ;Decrease source bit offset
        (JUMP-LESS-THAN-XCT-NEXT M-ZERO A-BITBLT-HOR-COUNT BITBLT-RTL-LOOP)
       ((M-R) SUB M-R A-J)                              ;Decrease destination bit offset
        (JUMP XFALSE)                                   ;Done

;;; Inner loop of vertical BITBLT
;;; Note, does wrap-around in the vertical coordinates of the source
;;; Args:  (none of these are modified)
;;;  A-ALUF alu-function (source is "A" operand)
;;;  M-I left rotate for source word (only low 5 bits looked at)
;;;  M-K selective-deposit B.P. for part of destination to change
;;;  M-S height of column
;;;  M-A source column address (top if M-B positive, bottom inclusive if M-B negative)
;;;  M-B source address increment, M-C source column height
;;;  A-BITBLT-SRC-Y, A-BITBLT-SRC-Y-OFFSET Y coord and word offset thereof
;;;   These determine the initial location referenced in the source column
;;;  M-D destination column address (top if M-E positive, bottom inclusive if M-E negative)
;;;      This is the first destination address referenced
;;;  M-E destination address increment, M-T destination column height
;;; Temps:
;;;  M-1 source address, M-2 destination address, A-BITBLT-TEM rotated source word
;;;  M-3 source rows before wrap-around
;;;  A-BITBLT-COUNT negative rows before done, M-4 loop counter for inner inner loop
;;; Only used in caller:
;;;  M-Q horizontal bit offset in source
;;;  M-R horizontal bit offset in destination
;;;  M-J bit count (width of this column)

BITBLT-INNER-LOOP
        ((M-1) SUB M-A A-B)                             ;Init source address
        ((M-1) ADD M-1 A-BITBLT-SRC-Y-OFFSET)           ;Offset to actual starting place
        ((M-3) SUB M-C A-BITBLT-SRC-Y)                  ;Number source rows until wrap-around
        ((M-2) SUB M-D A-E)                             ;Init destination address
        (CALL-LESS-THAN M-T A-S TRAP)                   ;Range-check destination
            (ERROR-TABLE BITBLT-DESTINATION-TOO-SMALL)
        ((A-BITBLT-COUNT) SUB M-ZERO A-S)               ;Init negative total row count
BITBLT-INNER-0          ;Loops back to here
        (JUMP-GREATER-THAN-XCT-NEXT M-3 A-ZERO BITBLT-INNER-1)  ;Check source wrap-around
       ((M-4) SUB M-ZERO A-BITBLT-COUNT)                ;Assume we'll be doing all rows at once
        ((M-1) SUB M-A A-B)                             ;Wrap-around, init source address
        ((M-3) M-C)                                     ; and row count to top
BITBLT-INNER-1
        (JUMP-GREATER-THAN M-3 A-4 BITBLT-INNER-2)      ;Do only up to
        ((M-4) M-3)                                     ; next source wrap point
BITBLT-INNER-2
        (POPJ-LESS-OR-EQUAL M-4 A-ZERO)                 ;Zero-length array, or we're done
        ((A-BITBLT-COUNT) ADD M-4 A-BITBLT-COUNT)       ;Count down remaining rows
        ((M-3) SUB M-3 A-4)                             ;Count down source rows before wrap
        ;; Check for fast case not requiring rotate nor read of destination
        ((M-TEM) (BYTE-FIELD 5 0) M-I A-K)              ;Check for rotate or part-word
        ((M-TEM) DPB M-TEM (BYTE-FIELD 10. 6) A-ALUF)   ;Check for ALU function of SETA
        (JUMP-EQUAL M-TEM (A-CONSTANT 174050) BITBLT-INNER-4)   ;Go to fast case
BITBLT-INNER-3          ;This is the inner inner loop
        ((VMA-START-READ M-1) ADD M-1 A-B)              ;Fetch source word
        (CHECK-PAGE-READ)
        ((OA-REG-LOW) DPB M-I OAL-MROT A-ZERO)          ;Rotate it into position
        ((A-BITBLT-TEM) (BYTE-FIELD 32. 0) READ-MEMORY-DATA)
        ((VMA-START-READ M-2) ADD M-2 A-E)              ;Fetch destination word
        (CHECK-PAGE-READ-NO-INTERRUPT)
        ((OA-REG-LOW) A-ALUF)                           ;ALU func
        ((M-TEM) SETZ READ-MEMORY-DATA A-BITBLT-TEM)    ;Combine source and dest
        ((M-TEM1) READ-MEMORY-DATA)                     ;Get onto A side
#+lambda((OA-REG-LOW) M-K)                              ;Store back under byte control
#+exp   ((m-tem3) add m-k (a-constant 1_5))
#+exp   ((oa-reg-low) ldb (byte-field 10. 0) m-tem3)
        ((WRITE-MEMORY-DATA-START-WRITE)
                SELECTIVE-DEPOSIT M-TEM (BYTE-FIELD 0 0) A-TEM1)
        (CHECK-PAGE-WRITE-unboxed)
        (JUMP-GREATER-THAN-XCT-NEXT M-4 (A-CONSTANT 1) BITBLT-INNER-3)
       ((M-4) SUB M-4 (A-CONSTANT 1))
        (POPJ-XCT-NEXT)
       (CALL-NOT-EQUAL M-ZERO A-BITBLT-COUNT BITBLT-INNER-0)    ;Jump if more to do

BITBLT-INNER-5          ;This is the fast inner inner loop
        ((VMA-START-WRITE M-2) ADD M-2 A-E)             ;Store destination word
        (CHECK-PAGE-WRITE-unboxed)
BITBLT-INNER-4
        ((VMA-START-READ M-1) ADD M-1 A-B)              ;Fetch source word
        (CHECK-PAGE-READ)
        (jump-greater-than-xct-next m-4 (a-constant 1) bitblt-inner-5)
       ((M-4) SUB M-4 (A-CONSTANT 1))
;       (JUMP-GREATER-THAN-XCT-NEXT M-4 A-ZERO BITBLT-INNER-5)
;       ((WRITE-MEMORY-DATA) READ-MEMORY-DATA)          ;Check parity
        ((VMA-START-WRITE M-2) ADD M-2 A-E)             ;Store last destination word
        (CHECK-PAGE-WRITE-unboxed)
        (POPJ-XCT-NEXT)
       (CALL-NOT-EQUAL M-ZERO A-BITBLT-COUNT BITBLT-INNER-0)    ;Jump if more to do

;;; Decode array, x, y on the stack into:
;;; M-1 X dimension of array in bits
;;; Q-R Y dimension of array
;;; M-4 initial Y-coordinate
;;; M-Q initial X coordinate in bits
;;; M-A word address of selected bit
;;; M-3 OA-REG-LOW value to convert bytes to bits
;;;             actually the number of bits per element minue 1
;;; Preserves: M-C, M-I, M-K, M-R, M-ZR (array routines better preserve these)

BITBLT-DECODE-ARRAY
        (CALL XAR-2-REVERSE)                            ;Access the array in usual way
        ((pdl-push) m-d)
        (call store-array-registers-in-accumulators)
        ((m-d) pdl-pop)
        ;; Leaves following stuff sitting around:
        ;; M-A the array, M-E base address, VMA word address, M-Q 1-D index,
        ;; M-D first dimension, M-S product of dimensions, M-B array header
        ((M-3) (LISP-BYTE %%ARRAY-TYPE-FIELD) m-array-header)   ;Array type.  For a numeric array,
        ((M-3) SUB M-3 (A-CONSTANT 1))                  ; it is 1+ log2 of the byte size.
        (CALL-GREATER-THAN M-3 (A-CONSTANT 5) array-trap)
     (error-table argtyp numeric-array m-a)
        ((M-1) Q-POINTER M-Q)                           ;Convert index into (X,Y) coords
        (CALL-XCT-NEXT DIV)                             ;Q-R gets Y, M-1 gets X
       ((M-2) Q-POINTER M-D)
        ((M-4) Q-R)
        ((M-Q) M-1)
        ((M-1) Q-POINTER m-array-length)        ;Compute second dimension (in Q-R)
        (CALL-XCT-NEXT DIV)
       ((M-2) Q-POINTER M-D)
#+lambda((OA-REG-LOW) M-3)                              ;Rotate first dimension left
#+exp   ((oa-reg-low) ldb (byte-field 10. 0) m-3)
        ((M-1) (BYTE-FIELD 32. 0) M-2)
        ((M-2) (BYTE-FIELD 5 0) M-1)                    ;Width must be multiple of 32 bits
        (CALL-NOT-EQUAL M-2 A-ZERO array-trap)
     (error-table bitblt-array-fractional-word-width m-a)
#+lambda((OA-REG-LOW) M-3)                              ;Convert X coordinate to bits
#+exp   ((oa-reg-low) ldb (byte-field 10. 0) m-3)
        (POPJ-AFTER-NEXT (M-Q) (BYTE-FIELD 32. 0) M-Q)
       ((M-A) Q-POINTER VMA)                            ;Word address of selected bit

))

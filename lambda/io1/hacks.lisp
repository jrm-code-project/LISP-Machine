;-*-MODE: LISP; PACKAGE: USER; BASE: 8-*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

(SETQ SINE-EPSILON '30.)
(SETQ MASTER-DURATION 5000)
(SETQ TEMPER
      '((A 200.) (W 189.) (S 178.) (E 168.) (D 159.) (F 150.) (T 141.)
        (G 133.) (Y 126.) (H 119.) (U 112.) (J 106.) (K 100.) (O 94.)
        (L 89.) (P 84.) (/; 79.) (/: 75.) (/] 71.)
        (/a 200.) (/w 189.) (/s 178.) (/e 168.) (/d 159.) (/f 150.) (/t 141.)
        (/g 133.) (/y 126.) (/h 119.) (/u 112.) (/j 106.) (/k 100.) (/o 94.)
        (/l 89.) (/p 84.) (/; 79.) (/: 75.) (/] 71.)))

(DECLARE (SPECIAL SINE-EPSILON TEMPER TEMPER-ARRAY))

(DEFUN BUZZ (DURATION WAVELENGTH &OPTIONAL (WAVELENGTH2 WAVELENGTH)
             &AUX (LOC 764110))
  (AND (MINUSP WAVELENGTH)
       (SETQ WAVELENGTH (MINUS WAVELENGTH)))
  (AND (MINUSP WAVELENGTH2)
       (SETQ WAVELENGTH2 (MINUS WAVELENGTH2)))
  (DO I 0 (1+ I) (= I DURATION)
    (DO J 0 (1+ J) (= J WAVELENGTH))
    (%UNIBUS-READ LOC)
    (DO J 0 (1+ J) (= J WAVELENGTH2))
    (%UNIBUS-READ LOC)))

(DEFUN PLAY (NNOTES INITIAL-NOTE DECREMENT DURATION)
  (DO ((K 0 (1+ K))
       (NOTE INITIAL-NOTE (- NOTE DECREMENT)))
      ((= K NNOTES))
    (BUZZ DURATION NOTE)))

(DECLARE (SPECIAL MASTER-DURATION))

(DEFUN ORGAN (&OPTIONAL RETUNE)
    (COND ((NOT (BOUNDP 'TEMPER-ARRAY))
           (SETQ TEMPER-ARRAY (MAKE-ARRAY
                               NIL 'ART-Q-LIST
                               '(220)))
           (SETQ RETUNE T)))
    (COND (RETUNE
           (DO N 0 (1+ N) (= N 220)
             (AS-1 10 TEMPER-ARRAY N))
           (DO X TEMPER (CDR X) (NULL X)
             (AS-1 (CADAR X) TEMPER-ARRAY
                   (GETCHARN (CAAR X) 1)))))
    (DO ((A (KBD-TYI) (KBD-TYI)))
        ((= (SETQ A (LOGAND A 377)) 202)
         (RETURN "End of sonata in Q minor"))
        ((LAMBDA (WAVELENGTH)
                 (BUZZ (// MASTER-DURATION WAVELENGTH) WAVELENGTH))
         (AR-1 TEMPER-ARRAY A))))

(DEFUN ZOWIE (&OPTIONAL RETUNE &AUX (MASTER-DURATION 5600))
    (COND ((NOT (BOUNDP 'TEMPER-ARRAY))
           (SETQ TEMPER-ARRAY (MAKE-ARRAY
                               NIL 'ART-Q-LIST
                               '(220)))
           (SETQ RETUNE T)))
    (COND (RETUNE
           (DO N 0 (1+ N) (= N 220)
             (AS-1 10 TEMPER-ARRAY N))
           (DO X TEMPER (CDR X) (NULL X)
             (AS-1 (CADAR X) TEMPER-ARRAY
                   (GETCHARN (CAAR X) 1)))))
    (DO ((W '(150. 150. 150. 159. 150. 133. 150. 133. 119. 133. 150. 159. 178. 159. 150.)
            (CDR W))
         (D '(1 1 2 1 1 2 1 1 2 1 1 2 1 1 2) (CDR D))
         (TEM))
        ((NULL W))
      (SETQ TEM (// (CAR W) 2))
      (DO I 0 (1+ I) (= I 1000))
      (BUZZ (// (* (CAR D) MASTER-DURATION) TEM) TEM)))

(comment

(DEFUN SINE (A)
  (COND ((< (ABS A) SINE-EPSILON) A)
        (T
         (SETQ A (SINE (RESCALE (* A -333.) 1000.)))
         (RESCALE (* A (- (* 4 (RESCALE (* A A) 1000.)) 3000.)) 1000.))))

(DEFUN RESCALE (X Y)
  (COND ((> X 0.)
         (// (+ X (// Y 2.)) Y))
        ((// (- X (// Y 2.)) Y))))

(DEFUN WAVE NIL
  (DO I 0. (1+ I)
    (= I 400.)
    (AS-2 1. TV-BUFFER I (+ 200. (// (SINE (* I 15.)) 10.)))))

(DEFUN CIRCLE NIL
  (DO I 0. (+ I 100.)
    (> I 6300.)
    (AS-2 1.
          TV-BUFFER
          (+ 250. (// (SINE I) 5.))
          (+ 250. (// (SINE (+ I 1571.)) 5.)))))

;TRY THIS WITH ARGUMENTS OF 20. AND 500.
(DEFUN MARV (&OPTIONAL (MARV-E 259.) (N 200.) (X 50.) (Y 0))
  (PROG NIL
   A    (AS-2 1. TV-BUFFER (+ X 200.) (+ Y 200.))
        (AND (ZEROP (SETQ N (1- N)))
             (RETURN T))
        (SETQ X (- X (// (* MARV-E Y) 1000.))
              Y (+ Y (// (* MARV-E X) 1000.)))
        (GO A)))
);end of comment


;INPUT CHARACTER WITH DDT STYLE ECHOING
(DEFUN CARPET-TYI (&AUX CH)
  (SETQ CH (FUNCALL TERMINAL-IO ':TYI))
  (AND (< CH 200)
       (FUNCALL TERMINAL-IO ':TYO))
  CH)

;BAG THE BYTE OR WORD
(DEFUN CARPET-BAG (LOC NUM BYTEP)
  (COND ((NOT BYTEP)
         (%UNIBUS-WRITE LOC NUM))
        (T
         (SETQ BYTEP (COND ((= 0 (LOGAND 1 LOC)) 0010)
                           (T 1010)))
         (%UNIBUS-WRITE LOC (DPB NUM BYTEP (%UNIBUS-READ LOC))))))

;UNIBUS OCTAL DEBUGGER
(DEFUN CARPET ()
  (PROG ((LOC 0) OPENP (BYTEP NIL) NUM SOME CH (CLOC 0))
TOP  (TERPRI)
     (PRINC '/!)
     (SETQ OPENP NIL)
READ (SETQ NUM 0 SOME NIL)
RNUM (SETQ CH (CARPET-TYI))
     (COND ((= CH #\BACKSPACE)
            (TV:SHEET-BACKSPACE TERMINAL-IO)
            (TV:SHEET-CLEAR-CHAR TERMINAL-IO)
            (SETQ NUM (// NUM 8))
            (GO RNUM))
           ((NOT (AND (>= CH 60) (<= CH 71)))
            (GO CMD)))
     (SETQ NUM (+ (* NUM 8) (- CH 60)))
     (SETQ SOME T)
     (GO RNUM)

CMD  (COND ((= CH 57)
            (SETQ BYTEP NIL LOC (LOGAND 777776 NUM) OPENP T)
            (GO OPN1))
           ((= CH 201)
            (RETURN T))
           ((= CH 56)
            (SETQ SOME T
                  NUM LOC)
            (GO RNUM))
           ((= CH 134)
            (SETQ BYTEP T LOC NUM OPENP T)
            (GO OPN1))
           ((= CH 40)
            (GO READ))
           ((= CH 215)
            (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
            (GO TOP))
           ((= CH 212)
            (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
            (SETQ LOC (+ LOC (COND (BYTEP 1) (T 2)))))
           ((= CH 136)
            (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
            (SETQ LOC (- LOC (COND (BYTEP 1) (T 2)))))
           ((= CH 211)
            (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
            (SETQ LOC (COND (SOME NUM) (T CLOC))))
           (T (PRINC "??  ")
              (GO READ)))
     (TERPRI)
     (PRIN1 (SETQ LOC (LOGAND 777777 LOC)))
     (PRINC (COND (BYTEP '\) (T '//)))
OPN1 (PRINC " ")
     (PRIN1 (SETQ CLOC (LDB (COND ((NULL BYTEP) 0020)
                                   ((= 0 (LOGAND 1 LOC)) 0010)
                                   (T 1010))
                             (%UNIBUS-READ LOC))))
     (PRINC "   ")
     (SETQ OPENP T)
     (GO READ)
))

(DECLARE (SPECIAL MUNCH MUNCH-SWITCHES KBD-SUPER-IMAGE-P))

(SETQ MUNCH '401)
;TRY ALSO 1, 10421, 11111, 100001, ETC.

(DEFMACRO BIND-FONT ((FONT) . BODY)
  `(LET ((OLD-FONT (TV:SHEET-CURRENT-FONT TERMINAL-IO)))
     (FUNCALL TERMINAL-IO ':SET-CURRENT-FONT ,FONT)
     (UNWIND-PROTECT (PROGN ,@BODY)
       (FUNCALL TERMINAL-IO ':SET-CURRENT-FONT OLD-FONT))))

(DEFMACRO BIND-FONT-MAP ((FONT-LIST) . BODY)
  `(LET ((OLD-FONT-MAP (TV:SHEET-FONT-MAP TERMINAL-IO)))
     (FUNCALL TERMINAL-IO ':SET-FONT-MAP ,FONT-LIST)
     (UNWIND-PROTECT (PROGN ,@BODY)
        (FUNCALL TERMINAL-IO ':SET-FONT-MAP OLD-FONT-MAP))))

(DEFUN MUNCH (&OPTIONAL M &AUX CHC TEM FLAG TVB)
  (AND M (SETQ MUNCH M))
  (BIND-FONT-MAP ('(FONTS:CPTFONT FONTS:43VXMS))
   (OR (BOUNDP 'MUNCH)
       (SETQ MUNCH 1))
   (SETQ TVB (TV:SHEET-SCREEN-ARRAY TERMINAL-IO))
   (FUNCALL TERMINAL-IO ':CLEAR-SCREEN)
   (OR (BOUNDP 'MUNCH-SWITCHES)
       (SETQ MUNCH-SWITCHES
             (CREATE-SWITCH-REGISTER TERMINAL-IO 20 NIL (* 3 (// 1100 4)))))
   (FUNCALL MUNCH-SWITCHES NIL)
   (FUNCALL MUNCH-SWITCHES MUNCH)
   (BIND-FONT (FONTS:43VXMS)
        (FUNCALL TERMINAL-IO ':SET-CURSORPOS 0 0)
        (FUNCALL TERMINAL-IO ':CLEAR-EOL)
        (PRINC MUNCH))
   (DO
    ((AB 0) (X) (Y) (CH) (NEW-KBD-P)
     (XOFF (+ (TV:SHEET-INSIDE-LEFT TERMINAL-IO)
              (// (TV:SHEET-INSIDE-WIDTH TERMINAL-IO) 2)
              -128.))
     (YOFF (+ 143 (TV:SHEET-INSIDE-TOP TERMINAL-IO))))
    (NIL)
    L (MULTIPLE-VALUE (CH NEW-KBD-P)    ;this doesnt really get returnned yet
        (KBD-TYI-NO-HANG))
    (COND ((NULL CH) (GO DIS)))
    (SETQ CHC (CHAR-UPCASE (LOGAND 377 CH)))
    (COND ((= CHC #\BREAK)
           (RETURN NIL))
          ((= CHC #\ESCAPE)
           (SETQ FLAG T)
           (SETQ MUNCH (NHNWSNOOB MUNCH)))
          ((MEMQ CHC '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9))
           (SETQ FLAG T)
           (SETQ MUNCH (+ (LSH MUNCH 3) (- CHC #/0))))
          ((= CHC #\CLEAR)
           (SETQ FLAG T)
           (SETQ MUNCH 0))
          ((= CHC #/+)
           (SETQ FLAG T)
           (SETQ MUNCH (1+ MUNCH)))
          ((= CHC #/)
           (SETQ FLAG T)
           (SETQ MUNCH (1+ (LSH MUNCH 1))))
          ((= CHC #/<)
           (SETQ FLAG T)
           (SETQ MUNCH (LSH MUNCH 1)))
          ((= CHC #/>)
           (SETQ FLAG T)
           (SETQ MUNCH (LSH MUNCH -1)))
          ((= CHC #\FORM)
           (SETQ FLAG T))
          ((= CHC #/?)
           (SETQ FLAG T)
           (FUNCALL TERMINAL-IO 'SET-CURSORPOS 0 0)
           (PRINC "SPACE increments, ESC does NHNWSNOOB, < is a left shift,")
           (PRINC "M shift left, but shift in a 1 bit,")
           (PRINC "> is a right shift, FORM clears the AC, CLEAR is like form but")
           (PRINC "zeroes the switches, the center row of keys toggles the switches,")
           (PRINC "CIRCLE-PLUS single steps,")
           (PRINC "BREAK exits.")
           (KBD-TYI)
           (FUNCALL MUNCH-SWITCHES NIL))
          ((SETQ TEM (FIND-POSITION-IN-LIST
                       CHC
                       (COND (NEW-KBD-P '(#/\ #\LF #\CR #/' #/; #/L #/K #/J #/H #/G
                                          #/F #/D #/S #/A #/W #/Q))
                             (T '(#\BACK-NEXT #\LF #\CR #/: #/; #/L #/K #/J #/H #/G
                                  #/F #/D #/S #/A #\RUBOUT #\VT)))))
           (SETQ FLAG T)
           (SETQ MUNCH (LOGXOR MUNCH (LSH 1 TEM)))))
    (COND (FLAG
           (SETQ FLAG NIL)
           (SETQ AB 0)
           (SETQ MUNCH (LOGAND 177777 MUNCH))
           (FUNCALL TERMINAL-IO ':DRAW-RECTANGLE 256. 256. XOFF YOFF
                    (TV:SHEET-ERASE-ALUF TERMINAL-IO))
           (FUNCALL MUNCH-SWITCHES MUNCH)
           (BIND-FONT (FONTS:43VXMS)
                      (FUNCALL TERMINAL-IO ':SET-CURSORPOS 0 0)
                      (FUNCALL TERMINAL-IO ':CLEAR-EOL)
                      (PRINC MUNCH))))
    (GO L)
 DIS
    (TV:PREPARE-SHEET (TERMINAL-IO)
       (DO () ((SI:KBD-HARDWARE-CHAR-AVAILABLE))
         (SETQ AB (LOGAND 177777 (+ AB MUNCH)))
         (SETQ X (LOGAND AB 377))
         (SETQ Y (+ YOFF (LOGXOR X (LDB 1010 AB))))
         (SETQ X (+ X XOFF))
         (AS-2 (LOGXOR 1 (AR-2 TVB X Y)) TVB X Y)))
    (GO L))))

(DEFUN NHNWSNOOB (A)    ;NEXT HIGHER NUMBER WITH SAME NUMBER OF ONE BITS (SEE HAKMEM)
  (PROG (B C)
        (AND (= A 0) (RETURN 0))
        (SETQ C (LOGAND A (- 0 A)))
        (SETQ B (+ A C))
        (RETURN (LOGIOR B (// (LSH (LOGXOR A B) -2) C)))))

(LOCAL-DECLARE ((SPECIAL SR-SHEET SR-SIZE
                         X-ORG Y-ORG
                         COLOR CHAR-ORIGIN CURRENT-SWITCHES FONTS:TOG))
(DEFUN CREATE-SWITCH-REGISTER (SR-SHEET SR-SIZE X-ORG Y-ORG
                               &OPTIONAL LIGHTSP (COLOR 7070707)
                               &AUX CHAR-ORIGIN CURRENT-SWITCHES)
  (IF (NOT (BOUNDP 'FONTS:TOG))
      (LOAD "lmfont;tog" "fonts"))
  (IF (NULL Y-ORG) (SETQ Y-ORG (- (// (TV:SHEET-INSIDE-HEIGHT SR-SHEET) 2) 15.)))
  (IF (NULL X-ORG) (SETQ X-ORG (- (// (TV:SHEET-INSIDE-WIDTH SR-SHEET) 2)
                                  (// (* 23. SR-SIZE) 2))))
  (SETQ CHAR-ORIGIN (COND (LIGHTSP 101) (T 60)))
  (CLOSURE '(SR-SHEET SR-SIZE X-ORG Y-ORG
             COLOR CHAR-ORIGIN CURRENT-SWITCHES)
     #'(LAMBDA (ARG &AUX NUM)
         (SETQ NUM (IF (NUMBERP ARG) ARG 0))
         (DO ((M (- 1 SR-SIZE) (1+ M))
              (X X-ORG (+ X 23.)))
             ((> M 0) NIL)
           (IF (OR (NULL ARG)
                   (BIT-TEST 1 (LSH (LOGXOR NUM CURRENT-SWITCHES) M)))
               (SHEET-PLUNK-CHAR SR-SHEET FONTS:TOG
                        (+ CHAR-ORIGIN
                           (+ (* 2 (LOGAND 1 (LSH COLOR M)))
                              (LOGAND 1 (LSH NUM M))))
                        X Y-ORG)))
         (SETQ CURRENT-SWITCHES NUM)))))


(DEFUN SHEET-PLUNK-CHAR (SHEET FONT CHAR X-BITPOS Y-BITPOS
                         &AUX TEM (FIT (FONT-INDEXING-TABLE FONT)))
  (TV:PREPARE-SHEET (SHEET)
    (SETQ X-BITPOS (+ X-BITPOS (TV:SHEET-INSIDE-LEFT SHEET))
          Y-BITPOS (+ Y-BITPOS (TV:SHEET-INSIDE-TOP SHEET)))
    (TV:%DRAW-RECTANGLE
       (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT)) (AREF TEM CHAR))
             (T (FONT-CHAR-WIDTH FONT)))
       (FONT-CHAR-HEIGHT FONT)
       X-BITPOS Y-BITPOS
       (TV:SHEET-ERASE-ALUF SHEET)
       SHEET)
    (IF (NULL FIT)
        (TV:%DRAW-CHAR FONT CHAR X-BITPOS Y-BITPOS (TV:SHEET-CHAR-ALUF SHEET) SHEET)
        ;;Wide character, draw in segments
        (DO ((CH (AREF FIT CHAR) (1+ CH))
             (LIM (AREF FIT (1+ CHAR)))
             (BPP (TV:SHEET-BITS-PER-PIXEL SHEET))
             (X X-BITPOS (+ X (// (FONT-RASTER-WIDTH FONT) BPP))))
            ((= CH LIM))
          (TV:%DRAW-CHAR FONT CH X Y-BITPOS  (TV:SHEET-CHAR-ALUF SHEET) SHEET)))))


; A 3RD ARG OF 0 STARTS AT THE BEGINNING, WHICH IS A LITTLE MECHANICAL.
; A 3RD ARG OF 571565 STARTS YOU OUT IN THE MIDDLE OF AN INTERESTING PART.
; N IS THE MAGIC CONSTANT, M IS SLOWNESS, A IS STARTING POINT.
; WHEN STOPPED THIS HACK RETURNS THE LAST VALUE OF A.

(DEFUN MUNCHING-TUNES (&OPTIONAL (N 1001) (A 0) (M 30000) (O 3))
 (WITHOUT-INTERRUPTS                            ;So it doesn't stutter. -ACW, 11/12/80
  (DO ((ACC (REMAINDER A 1000000) (REMAINDER (+ ACC N) 1000000))
       (FREQ (LOGXOR (\ A 1000)
                     (// A 1000))
             (LOGXOR (\ ACC 1000)
                     (// ACC 1000))))
      ((KBD-TYI-NO-HANG) ACC)
    (SI:%BEEP (LSH FREQ O) M))))



(DEFUN HIST (L &AUX AR N)
  (SETQ N (APPLY 'MAX L))
  (SETQ AR (MAKE-ARRAY NIL
                       'ART-Q-LIST
                       (LIST (1+ N))))
  (DO I 0 (1+ I) (> I N)
    (AS-1 0 AR I))
  (DO L L (CDR L) (NULL L)
    (AS-1 (1+ (AR-1 AR (CAR L))) AR (CAR L)))
  (GRAPH (SETQ L (G-L-P AR))
         (MAX 1 (// 764 N))
         (MAX 1 (// 310 (APPLY 'MAX L)))))

(DEFUN GRAPH (L &OPTIONAL (XF 2) (YF 10))
  (TV-CLEAR-PC-PPR CONSOLE-IO-PC-PPR)
  (TV-SET-CURSORPOS CONSOLE-IO-PC-PPR
                    0
                    (* 3 (// (SCREEN-HEIGHT TV-DEFAULT-SCREEN) 4)))
  (PRINT -)
  (DO ((I 0 (+ I XF))
       (L L (CDR L)))
      ((NULL L))
    (DRAW-LINE (- I 372) 0 (- I 372) (* (CAR L) YF))))

(DEFUN /:LISTF (DIR &AUX STR)
    (SETQ DIR (STRING-TRIM '(40 73) (STRING DIR)))
    (SETQ STR (OPEN (STRING-APPEND DIR ";.FILE. (DIR)") '(READ)))
    (TV-CLEAR-PC-PPR CONSOLE-IO-PC-PPR)
    (STREAM-COPY-UNTIL-EOF STR STANDARD-OUTPUT)
    (CLOSE STR)
    T)

(DEFUN /:PRINT (FILENAME &AUX S)
    (SETQ S (OPEN FILENAME '(READ)))
    (TV-CLEAR-PC-PPR CONSOLE-IO-PC-PPR)
    (STREAM-COPY-UNTIL-EOF S STANDARD-OUTPUT)
    (CLOSE S)
    T)

(DECLARE (SPECIAL LIVE-BOUNCE-LL))

(DEFUN LIVE-BOUNCE (&OPTIONAL (DELAY 0))
  (OR (BOUNDP 'LIVE-BOUNCE-LL)
      (SETQ LIVE-BOUNCE-LL (CREATE-SWITCH-REGISTER (// 576. 2) 50. 25 T)))
  (DO ((NB 1)
       (DNB 1)
       (DIR 1)
       (LT 1)
       (COMP 0))
      ((KBD-TYI-NO-HANG))
    (FUNCALL LIVE-BOUNCE-LL (LOGXOR LT COMP))
    (SETQ LT (LSH LT DIR))
    (COND ((= 1 (LOGAND LT 1))
           (SETQ DIR 1)
           (SETQ NB (+ NB DNB))
           (COND ((ZEROP NB)
                  (SETQ NB 1
                        DNB 1))
                 ((= NB 25)
                  (SETQ NB 1
                        COMP (LOGXOR -1 COMP)
                        DNB 1)))
           (SETQ LT (1- (LSH 1 NB))))
          ((NEQ 0 (LOGAND 4000000 LT))
           (SETQ DIR -1)
           (SETQ NB (+ NB DNB))
           (COND ((ZEROP NB)
                  (SETQ NB 1
                        DNB 1))
                 ((= NB 25)
                  (SETQ NB 1
                        COMP (LOGXOR -1 COMP)
                        DNB 1)))
           (SETQ LT (LSH (1- (LSH 1 NB)) (- 25 NB)))))
    (DO I DELAY (1- I) (= I 0)
    )))

(DEFUN GREEN-HORNET ()
    (TV-CLEAR-SCREEN)
    (TV-SET-CURSORPOS CONSOLE-IO-PC-PPR 560. 0)
    (DO I 310 (1- I) (= I 5)
      (DRAW-CIRCLE (COND ((ZEROP (LOGAND 20 I)) I) (T (- 0 I)))0 I 'XOR))
    (KBD-TYI)
    T)

(DEFUN CIRCLES ()
    (TV-CLEAR-SCREEN)
    (TV-SET-CURSORPOS CONSOLE-IO-PC-PPR 560. 0)
    (DO I 200. (1- I) (= I 5)
      (DRAW-CIRCLE 0 0 I))
    (KBD-TYI)
    T)



(DECLARE (SPECIAL LEXIPHAGE-PC-PPR FONTS:43VXMS LEXIPHAGE-ARRAY TV-BUFFER)
         (SPECIAL DRAW-SWAP-X-Y DRAW-NEGATE-Y DRAW-NEGATE-X
                  DRAW-X0 DRAW-Y0 DRAW-X-OFFSET DRAW-Y-OFFSET))

(DEFUN LEXIPHAGE (&OPTIONAL (TEXT "LEXIPHAGE") &AUX RIGHT)
  (OR (BOUNDP 'FONTS:43VXMS)
      (FERROR NIL "Please load LMFONT; 43VXMS QFASL"))
  (OR (BOUNDP 'LEXIPHAGE-PC-PPR)
      (SETQ LEXIPHAGE-PC-PPR (TV-DEFINE-PC-PPR 'LEXIPHAGE-PC-PPR (LIST FONTS:43VXMS)
                                               'BOTTOM 200. 'BLINKER-P NIL 'MORE-P NIL)))
  (OR (BOUNDP 'LEXIPHAGE-ARRAY)
      (SETQ LEXIPHAGE-ARRAY (MAKE-ARRAY NIL 'ART-16B '(136))))
  (TV-CLEAR-PC-PPR LEXIPHAGE-PC-PPR)
  (TV-SET-CURSORPOS LEXIPHAGE-PC-PPR 140 60)
  (TV-STRING-OUT LEXIPHAGE-PC-PPR TEXT)
  (SETQ RIGHT (PC-PPR-CURRENT-X LEXIPHAGE-PC-PPR))
  (DO I 0 (1+ I) (= I 136) (AS-1 0 LEXIPHAGE-ARRAY I))
  (BIND (FUNCTION-CELL-LOCATION 'DRAW-PLOT) 'LEXIPHAGE-1)
  (DO ((X 0 (1+ X))
       (DX 10))
      ((>= X RIGHT))
    (DRAW-LINE X 104 (+ X DX) (+ 44 DX))
    (DRAW-LINE X 104 (+ X DX) (- 144 DX))
    (DO I 0 (1+ I) (= I 1000))
    (DO I 0 (1+ I) (= I 136) (AS-2 0 TV-BUFFER (AR-1 LEXIPHAGE-ARRAY I) I))
    (SETQ DX (+ DX 1))
    (AND (>= DX 32) (SETQ DX 10)))
  "LEXIPHAGE!")

(DEFUN LEXIPHAGE-1 (X Y)
  (AND DRAW-SWAP-X-Y (SETQ X (PROG2 NIL Y (SETQ Y X))))
  (AND DRAW-NEGATE-Y (SETQ Y (MINUS Y)))
  (AND DRAW-NEGATE-X (SETQ X (MINUS X)))
  (SETQ X (+ X DRAW-X0))
  (SETQ Y (+ Y DRAW-Y0))
  (AND (>= X 0) (< X (SCREEN-WIDTH TV-DEFAULT-SCREEN))
       (>= Y 0) (< Y (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
       (PROGN (AND (> X (AR-1 LEXIPHAGE-ARRAY Y))
                   (DO XX (AR-1 LEXIPHAGE-ARRAY Y) (1+ XX) (>= XX X)
                     (AS-2 0 TV-BUFFER XX Y)))
              (AND (< X (AR-1 LEXIPHAGE-ARRAY Y))
                   (AS-2 0 TV-BUFFER (AR-1 LEXIPHAGE-ARRAY Y) Y))
              (AS-1 X LEXIPHAGE-ARRAY Y)
              (AS-2 1 TV-BUFFER X Y))))



(DECLARE (SPECIAL MF10-HACK5-BAR))

(DEFUN MF10-HACK5 (&OPTIONAL (ZAP 100) (13357ALIAS 13357))
   (OR (BOUNDP 'MF10-HACK5-BAR)
       (SETQ MF10-HACK5-BAR (CREATE-SWITCH-REGISTER (// 576. 2) 100 20 T)))
   (DO I 13357ALIAS (ROT I 1) (KBD-TYI-NO-HANG)
     (FUNCALL MF10-HACK5-BAR I)
     (DO J 0 (+ J 1) (= J ZAP)
         )))

(DEFVAR PRINT-BIG-PREVIOUS)

(DEFUN PRINT-BIG (&OPTIONAL (FONT PRINT-BIG-PREVIOUS))
   (TERPRI)
   (COND ((NULL FONT) (SETQ FONT FONTS:BIGFNT)))
   (SETQ PRINT-BIG-PREVIOUS (TV:SHEET-CURRENT-FONT TERMINAL-IO))
   (FUNCALL TERMINAL-IO ':SET-FONT-MAP (LIST FONT))
   (FUNCALL TERMINAL-IO ':SET-CURRENT-FONT FONT))

(DEFUN PRINT-SMALL ()
   (TERPRI)
   (FUNCALL TERMINAL-IO ':SET-FONT-MAP (LIST FONTS:CPTFONT))
   (FUNCALL TERMINAL-IO ':SET-CURRENT-FONT FONTS:CPTFONT))


(DECLARE (SPECIAL FEFS ADL-QS ADL-VARS NAME-QS FREE-QS TOTAL-BOXED-LENGTH
                 TOTAL-LENGTH))

(DEFUN COMPUTE-FEF-STATS NIL
  (PROG ((FEFS 0) (ADL-QS 0) (ADL-VARS 0) (NAME-QS 0) (FREE-QS 0) (TOTAL-BOXED-LENGTH 0)
         (TOTAL-LENGTH 0))
        (MAPATOMS-ALL (FUNCTION (LAMBDA (X &AUX FEFP TEM)
                              (COND ((AND
                                      (FBOUNDP X)
                                      (=
                                       (%P-DATA-TYPE (SETQ FEFP (FUNCTION-CELL-LOCATION X)))
                                       DTP-FEF-POINTER))
                                     (SETQ FEFP (CDR FEFP))
                                     (SETQ FEFS (1+ FEFS))
                                     (SETQ
                                      ADL-VARS
                                      (+ ADL-VARS
                                         (%P-LDB-OFFSET SI:%%FEFHI-MS-BIND-DESC-LENGTH
                                                        FEFP
                                                        SI:%FEFHI-MISC)))
                                     (SETQ
                                      TOTAL-BOXED-LENGTH
                                      (+ TOTAL-BOXED-LENGTH
                                         (// (%P-LDB-OFFSET SI:%%FEFH-PC FEFP SI:%FEFHI-IPC) 2)))
                                     (SETQ
                                      TOTAL-LENGTH
                                      (+ TOTAL-LENGTH
                                         (%P-CONTENTS-OFFSET FEFP SI:%FEFHI-STORAGE-LENGTH)))
                                     (PROG ((LST (SI:GET-MACRO-ARG-DESC-POINTER FEFP))
                                            VAR-DESC)
                                           (SETQ ADL-QS (+ ADL-QS (LENGTH LST)))
                                      L    (COND ((NULL LST)
                                                  (RETURN NIL)))
                                           (SETQ VAR-DESC (CAR LST))
                                           (COND ((NOT
                                                   (ZEROP (LOGAND VAR-DESC
                                                                  SI:%FEF-NAME-PRESENT)))
                                                  (SETQ LST (CDR LST))
                                                  (SETQ NAME-QS (1+ NAME-QS))))
                                           (COND ((= (MASK-FIELD SI:%%FEF-ARG-SYNTAX
                                                                 VAR-DESC)
                                                     SI:FEF-ARG-FREE)
                                                  (SETQ FREE-QS (1+ FREE-QS))))
                                           (COND ((OR
                                                   (= (SETQ TEM (LDB SI:%%FEF-INIT-OPTION
                                                                     VAR-DESC))
                                                      SI:FEF-INI-PNTR)
                                                   (= TEM SI:FEF-INI-C-PNTR)
                                                   (= TEM SI:FEF-INI-OPT-SA)
                                                   (= TEM SI:FEF-INI-EFF-ADR))
                                                  (SETQ LST (CDR LST))))
                                           (SETQ LST (CDR LST))
                                           (GO L)))))))
        (FORMAT STANDARD-OUTPUT
                       "~%FEFS ~D, TOTAL LENGTH ~D, BOXED LENGTH ~D, ADL-VARS ~D,/
ADL-QS ~D, LOCAL NAME QS ~D, FREE-VARIABLE-ADL-QS ~D,"
                       FEFS
                       TOTAL-LENGTH
                       TOTAL-BOXED-LENGTH
                       ADL-VARS
                       ADL-QS
                       NAME-QS
                       FREE-QS)))

;(own-closure ((foo nil) (baz (gos hawk))) (function rumplestiltskin))
;
;(defmacro own-closure (vars-and-inits fcn)
;  `((lambda ,(mapcar (function car) vars-and-inits)
;       (closure ',(mapcar (function car) vars-and-inits) fcn))
;    ,@(mapcar (function cadr) vars-and-inits)))


(DEFUN TV-DEMO ()
    (TV-CLEAR-ALL)
    (TV-SET-CURSORPOS CONSOLE-IO-PC-PPR 0 0)
    (DO ((CHANNEL 0 (1+ CHANNEL))
         (BIT 1 (LSH BIT 1)))
        ((>= CHANNEL 4) (TV-SELECT 0) NIL)
      (TV-SELECT CHANNEL)
      (DO ((SEC 0 (1+ SEC))
           (SEC2 0 (+ 2 SEC2))
           (X))
          ((= SEC 20))
        (SETQ X (COND ((ZEROP (LOGAND BIT SEC)) 0)
                      (T -1)))
        (DO I 0 (+ I 44) (>= I 37000)
          (AS-1 X TV-BUFFER-WORDS (+ I SEC2))
          (AS-1 X TV-BUFFER-WORDS (1+ (+ I SEC2))))))
    (KBD-TYI)
    NIL)


(DEFUN TV-SELECT (&OPTIONAL (CHANNEL 0))
       (%UNIBUS-WRITE 776660
                      (DPB CHANNEL 1003 (%UNIBUS-READ 776660))))

(DEFUN FLASH ( &AUX (SCREEN-HEIGHT (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
                    (SCREEN-WIDTH (SCREEN-WIDTH TV-DEFAULT-SCREEN))
                    (SIZE (MIN SCREEN-WIDTH SCREEN-HEIGHT)))
    (TV-CLEAR-SCREEN)
    (DO ((LEFT 0 (1+ LEFT))
         (WIDTH SIZE (- WIDTH 2))
         (TOP 0 (1+ TOP))
         (HEIGHT SIZE (- HEIGHT 2)))
        ((<= HEIGHT 1) (KBD-TYI) NIL)
      (TV-ERASE WIDTH HEIGHT LEFT TOP TV-ALU-XOR)))

(DEFUN FLOSH ( &AUX (SCREEN-HEIGHT (SCREEN-HEIGHT TV-DEFAULT-SCREEN))
                    (SCREEN-WIDTH (SCREEN-WIDTH TV-DEFAULT-SCREEN))
                    (SIZE (MIN SCREEN-WIDTH SCREEN-HEIGHT)))
    (TV-CLEAR-SCREEN)
    (DO ((LEFT (// SIZE 2) (1- LEFT))
         (WIDTH 0 (+ WIDTH 2))
         (TOP (// SIZE 2) (1- TOP))
         (HEIGHT 0 (+ HEIGHT 2)))
        ((>= HEIGHT SIZE) (KBD-TYI) NIL)
      (TV-ERASE WIDTH HEIGHT LEFT TOP TV-ALU-XOR)))


(DECLARE (SPECIAL DISPLAY-LOCATION-EXAMINE-LIGHTS))

(DEFUN DISPLAY-MOUSE-REGS NIL
  (DISPLAY-UNIBUS-LOCATION TV:MOUSE-REG1 TV:MOUSE-REG2))

(DEFUN DISPLAY-UNIBUS-LOCATION (&REST ADDRESS-LIST &AUX (N (LENGTH ADDRESS-LIST)))
       (COND ((NOT (BOUNDP 'DISPLAY-LOCATION-EXAMINE-LIGHTS))
              (SETQ DISPLAY-LOCATION-EXAMINE-LIGHTS (MAKE-ARRAY NIL ART-Q 12.))
              (DO I 0 (1+ I) (>= I 12.)
                  (AS-1 (CREATE-SWITCH-REGISTER TERMINAL-IO 16. NIL  (+ (* I 44) 40) T)
                        DISPLAY-LOCATION-EXAMINE-LIGHTS
                        I))))
       (FUNCALL TERMINAL-IO ':CLEAR-SCREEN)
       (DO I 0 (1+ I) (>= I N)
         (FUNCALL (AR-1 DISPLAY-LOCATION-EXAMINE-LIGHTS I) NIL))
       (DO NIL ((EQ 203 (KBD-TYI-NO-HANG)))
         (DO ((I 0 (1+ I))
              (L ADDRESS-LIST (CDR L)))
             ((>= I N))
           (FUNCALL (AR-1 DISPLAY-LOCATION-EXAMINE-LIGHTS I)
                    (%UNIBUS-READ (CAR L))))))

(DEFUN RANDOM-TEST (&AUX TEM)
    (TV-CLEAR-SCREEN)
    (DO () (())
      (SETQ TEM (RANDOM))
      (AND (KBD-TYI-NO-HANG) (RETURN NIL))
      (AS-2 1 TV-BUFFER (LDB 2010 TEM) (LDB 1010 TEM))))

;; Print "Random Numerals", used in ancient Rand.   DLW 1/8/78

(DECLARE (SPECIAL ROMAN-STREAM ROMAN-OLD))
(SETQ ROMAN-OLD NIL)

(DEFUN ROMAN-STEP (X N)
    (COND ((> X 9.)
           (ROMAN-STEP (// X 10.) (1+ N))
           (SETQ X (\ X 10.))))
    (COND ((AND (= X 9) (NOT ROMAN-OLD))
           (ROMAN-CHAR 0 N)
           (ROMAN-CHAR 0 (1+ N)))
          ((= X 5)
           (ROMAN-CHAR 1 N))
          ((AND (= X 4) (NOT ROMAN-OLD))
           (ROMAN-CHAR 0 N)
           (ROMAN-CHAR 1 N))
          (T (COND ((> X 5)
                    (ROMAN-CHAR 1 N)
                    (SETQ X (- X 5))))
             (DO I 0 (1+ I) (>= I X)
               (ROMAN-CHAR 0 N)))))

(DEFUN ROMAN-CHAR (I X)
    (FUNCALL ROMAN-STREAM 'TYO (NTH (+ I X X) '(#/I #/V #/X #/L #/C #/D #/M))))

(DEFUN ROMAN-PRINC (X ROMAN-STREAM)
    (SETQ X (- X))
    (COND ((AND (< X 4000.)
                (> X 0))
           (ROMAN-STEP X 0))
          (T (LET ((BASE 10.))
                  (PRINC X ROMAN-STREAM)))))

(DEFPROP ROMAN ROMAN-PRINC PRINC-FUNCTION)

;; (SETQ BASE 'ROMAN-OLD) will print out in old-style Roman numerals.
(DEFUN ROMAN-OLD-PRINC (X ROMAN-STREAM)
   (LET ((ROMAN-OLD T))
     (ROMAN-PRINC X ROMAN-STREAM)))

(DEFPROP ROMAN-OLD ROMAN-OLD-PRINC PRINC-FUNCTION)

(comment
(DEFUN STACKS ( &AUX SG RP SP)
    (SETQ SG SI:%CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP)
    (DO ((STACK (SG-REGULAR-PDL SG) (SG-SPECIAL-PDL SG))
         (START (SG-REGULAR-PDL-POINTER SG) (SG-SPECIAL-PDL-POINTER SG))
         (MESS "Regular PDL" "Special PDL")
         (KLUDGE 0 (1+ KLUDGE)))
        ((>= KLUDGE 2) T)
      (TERPRI) (TERPRI) (PRINC MESS) (TERPRI)
      (DO I 0 (1+ I) (> I START)
        (COND ((= 1 (%P-FLAG-BIT (AP-1-CAREFUL STACK I)))
               (TYO #/*))
              (T (TYO 40)))
        (PRINC I) (TYO 40)
        (PRIN1-C-LOCATIVE-CAREFUL (AP-1-CAREFUL STACK I)) (TERPRI))))

(DEFUN SKIP-INTERNAL ( &AUX RP SFP SFP1 PTR1 OFFSET)
    (SETQ RP (SG-REGULAR-PDL SI:%CURRENT-STACK-GROUP))
    (SETQ SFP (- (%POINTER-DIFFERENCE (%STACK-FRAME-POINTER) RP) 2))
    (SETQ SFP1 (- SFP (RP-DELTA-TO-ACTIVE-BLOCK RP SFP)))
    (SETQ SFP1 (- SFP1 (RP-DELTA-TO-ACTIVE-BLOCK RP SFP1)))
    (SETQ SFP1 (- SFP1 (RP-DELTA-TO-ACTIVE-BLOCK RP SFP1)))
    (SETQ PTR1 (AP-1 RP SFP1))
    (SETQ OFFSET (+ 3 (RP-LOCAL-BLOCK-ORIGIN RP SFP1)))
    (%P-STORE-CONTENTS-OFFSET (CDR (%P-CONTENTS-OFFSET PTR1 OFFSET)) PTR1 OFFSET))

(DEFUN SKIPA (&QUOTE VAR FORM)
   (SET VAR (EVAL FORM))
   (SKIP-INTERNAL))

(DEFUN SKIPE (&QUOTE VAR FORM)
   (SET VAR (EVAL FORM))
   (AND (ZEROP (SYMEVAL VAR))
        (SKIP-INTERNAL)))

(DEFUN SKIPN (&QUOTE VAR FORM)
   (SET VAR (EVAL FORM))
   (OR (ZEROP (SYMEVAL VAR))
       (SKIP-INTERNAL)))
);end comment

(defun dance (&optional (mina 100) (maxa 456) (minb 200) (maxb 565)
                        (minc 60) (maxc 1076) (mind 300) (maxd 1100))
       (tv-draw-line mina minb maxc maxd tv-alu-xor tv-default-screen)
       (do ((a mina)
            (b minb)
            (c maxc)
            (d maxd)
            (oa)(ob)(oc)(od)
            (da 1)
            (db 1)
            (dc -1)
            (dd -1))
         ((kbd-tyi-no-hang))
         (setq oa a ob b oc c od d)
         (setq a (+ a da))
         (cond ((= da 1)
                (cond (( a maxa)
                       (setq da -1))))
               (( a mina)
                (setq da 1)))
         (setq b (+ b db))
         (cond ((= db 1)
                (cond (( b maxb)
                       (setq db -1))))
               (( b minb)
                (setq db 1)))
         (setq c (+ c dc))
         (cond ((= dc 1)
                (cond (( c maxc)
                       (setq dc -1))))
               (( c minc)
                (setq dc 1)))
         (setq d (+ d dd))
         (cond ((= dd 1)
                (cond (( d maxd)
                       (setq dd -1))))
               (( d mind)
                (setq dd 1)))
         (tv-draw-line a b c d tv-alu-xor tv-default-screen)
         (tv-draw-line oa ob oc od tv-alu-xor tv-default-screen)))

(defun spazz (&aux (mina (random (screen-x2 tv-default-screen)))
                   (minc (random (screen-x2 tv-default-screen)))
                   (minb (random (screen-y2 tv-default-screen)))
                   (mind (random (screen-y2 tv-default-screen)))
                   (awid (random (- (screen-x2 tv-default-screen) mina)))
                   (cwid (random (- (screen-x2 tv-default-screen) minc)))
                   (bwid (random (- (screen-y2 tv-default-screen) minb)))
                   (dwid (random (- (screen-y2 tv-default-screen) mind))))
  (dance mina (+ mina awid) minb (+ minb bwid) minc (+ minc cwid) mind (+ mind dwid)))

(declare (special rcavic-saved-array rcavic-hacked-array rcavic-hacked-16b-array))
(defun rcavic (&optional (screen tv-default-screen) (interval 3000000) &aux array
                       16b-array len)
    (setq array (screen-buffer-pixel-array screen)
          16b-array (screen-buffer-halfword-array screen)
          len (array-length 16b-array))
    (or (boundp 'rcavic-saved-array)
        (let ((dims (arraydims (screen-buffer-pixel-array screen))))
         (setq rcavic-saved-array (make-array nil (car dims) (cdr dims))
               rcavic-hacked-array (make-array nil (car dims) (cdr dims))
               rcavic-hacked-16b-array (make-array nil 'art-16b len rcavic-hacked-array))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((< j 0))
        (do ((arg (ar-1 16b-array i) (lsh arg -1))
             (val 0)
             (bit 100000 (lsh bit -1)))
            ((zerop arg) (as-1 val rcavic-hacked-16b-array j))
            (and (bit-test arg 1)
                 (setq val (logior val bit)))))
    (bitblt tv-alu-seta (screen-width screen) (screen-height screen)
             array 0 0 rcavic-saved-array 0 0)
    (do i 0 (1+ i) ( i 3) (tv-beep))
    (bitblt tv-alu-seta (screen-width screen) (screen-height screen)
             rcavic-hacked-array 0 0 array 0 0)
    (do i 0 (1+ i) ( i interval))
    (bitblt tv-alu-seta (screen-width screen) (screen-height screen)
            rcavic-saved-array 0 0 array 0 0))

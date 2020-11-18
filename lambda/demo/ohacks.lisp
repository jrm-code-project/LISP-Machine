;-*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;TRY THIS WITH ARGUMENTS OF 20. AND 500.
;(DEFUN MARV (&OPTIONAL (MARV-E 259.) (N 200.) (X 50.) (Y 0))
;  (PROG NIL
;    A   ;(AS-2 1. TV-BUFFER (+ X 200.) (+ Y 200.))
;        (FUNCALL TERMINAL-IO ':DRAW-POINT (+ X 200.) (+ Y 200.) TV:ALU-XOR)
;        (AND (ZEROP (SETQ N (1- N)))
;             (RETURN T))
;        (SETQ X (- X (TRUNCATE (* MARV-E Y) 1000.))
;              Y (+ Y (TRUNCATE (* MARV-E X) 1000.)))
;        (GO A)))


;;INPUT CHARACTER WITH DDT STYLE ECHOING
;(DEFUN CARPET-TYI (&AUX CH)                    ;
;  (SETQ CH (FUNCALL TERMINAL-IO ':TYI))
;  (AND (< CH 200)
;       (FUNCALL TERMINAL-IO ':TYO CH))
;  CH)

;;BAG THE BYTE OR WORD
;(DEFUN CARPET-BAG (LOC NUM BYTEP)
;  (COND ((NOT BYTEP)
;        (%UNIBUS-WRITE LOC NUM))
;       (T
;        (SETQ BYTEP (COND ((= 0 (LOGAND 1 LOC)) 0010)
;                          (T 1010)))
;        (%UNIBUS-WRITE LOC (DPB NUM BYTEP (%UNIBUS-READ LOC))))))

;;UNIBUS OCTAL DEBUGGER
;(DEFUN CARPET ()
;  (PROG ((LOC 0) OPENP (BYTEP NIL) NUM SOME CH (CLOC 0))
;TOP  (TERPRI)
;     (PRINC '/!)
;     (SETQ OPENP NIL)
;READ (SETQ NUM 0 SOME NIL)
;RNUM (SETQ CH (CARPET-TYI))
;     (COND ((= CH #\BACKSPACE)
;           (SEND TERMINAL-IO ':BACKWARD-CHAR)
;           (SEND TERMINAL-IO ':CLEAR-CHAR)
;           (SETQ NUM (TRUNCATE NUM 8))
;           (GO RNUM))
;          ((NOT (AND (>= CH 60) (<= CH 71)))
;           (GO CMD)))
;     (SETQ NUM (+ (* NUM 8) (- CH 60)))
;     (SETQ SOME T)
;     (GO RNUM)

;CMD  (COND ((= CH #//)
;           (SETQ BYTEP NIL LOC (LOGAND 777776 NUM) OPENP T)
;           (GO OPN1))
;          ((= CH #\ABORT)
;           (RETURN T))
;          ((= CH #/.)
;           (SETQ SOME T
;                 NUM LOC)
;           (GO RNUM))
;          ((= CH #/\)
;           (SETQ BYTEP T LOC NUM OPENP T)
;           (GO OPN1))
;          ((= CH #\SPACE)
;           (GO READ))
;          ((= CH #\CR)
;           (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
;           (GO TOP))
;          ((= CH #\LF)
;           (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
;           (SETQ LOC (+ LOC (COND (BYTEP 1) (T 2)))))
;          ((= CH #/^)
;           (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
;           (SETQ LOC (- LOC (COND (BYTEP 1) (T 2)))))
;          ((= CH #\TAB)
;           (AND OPENP SOME (CARPET-BAG LOC NUM BYTEP))
;           (SETQ LOC (COND (SOME NUM) (T CLOC))))
;          (T (PRINC "??  ")
;             (GO READ)))
;     (TERPRI)
;     (PRIN1 (SETQ LOC (LOGAND 777777 LOC)))
;     (PRINC (COND (BYTEP '\) (T '//)))
;OPN1 (PRINC " ")
;     (PRIN1 (SETQ CLOC (LDB (COND ((NULL BYTEP) 0020)
;                                  ((= 0 (LOGAND 1 LOC)) 0010)
;                                  (T 1010))
;                            (%UNIBUS-READ LOC))))
;     (PRINC "   ")
;     (SETQ OPENP T)
;     (GO READ)
;))

(LOCAL-DECLARE ((SPECIAL SR-SHEET SR-SIZE
                         X-ORG Y-ORG
                         COLOR CHAR-ORIGIN CURRENT-SWITCHES FONTS:TOG))
(DEFUN CREATE-SWITCH-REGISTER (SR-SHEET SR-SIZE X-ORG Y-ORG
                               &OPTIONAL LIGHTSP (COLOR 7070707)
                               &AUX CHAR-ORIGIN CURRENT-SWITCHES)
  (IF (NOT (BOUNDP 'FONTS:TOG))
      (LOAD "lmfont;tog" "fonts"))
  (IF (NULL Y-ORG) (SETQ Y-ORG (- (TRUNCATE (TV:SHEET-INSIDE-HEIGHT SR-SHEET) 2) 15.)))
  (IF (NULL X-ORG) (SETQ X-ORG (- (TRUNCATE (TV:SHEET-INSIDE-WIDTH SR-SHEET) 2)
                                  (TRUNCATE (* 23. SR-SIZE) 2))))
  (SETQ CHAR-ORIGIN (COND (LIGHTSP 101) (T 60)))
  (CLOSURE '(SR-SHEET SR-SIZE X-ORG Y-ORG
             COLOR CHAR-ORIGIN CURRENT-SWITCHES)
     #'(LAMBDA (ARG &AUX NUM)
         (if (eq arg ':get-window)
             sr-sheet
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
           (SETQ CURRENT-SWITCHES NUM))))))


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
             (X X-BITPOS (+ X (TRUNCATE (FONT-RASTER-WIDTH FONT) BPP))))
            ((= CH LIM))
          (TV:%DRAW-CHAR FONT CH X Y-BITPOS  (TV:SHEET-CHAR-ALUF SHEET) SHEET)))))


; A 3RD ARG OF 0 STARTS AT THE BEGINNING, WHICH IS A LITTLE MECHANICAL.
; A 3RD ARG OF 571565 STARTS YOU OUT IN THE MIDDLE OF AN INTERESTING PART.
; N IS THE MAGIC CONSTANT, M IS SLOWNESS, A IS STARTING POINT.
; WHEN STOPPED THIS HACK RETURNS THE LAST VALUE OF A.

(DEFUN MUNCHING-TUNES (&OPTIONAL (N 1001) (A 0) (M 30000) (O 3))
  "Plays a tune on the keyboard.  Plays an interesting tune with m = 571565. Any char stops."
 (WITH-REAL-TIME
  (DO ((ACC (REMAINDER A 1000000) (REMAINDER (+ ACC N) 1000000))
       (FREQ (LOGXOR (\ A 1000)
                     (TRUNCATE A 1000))
             (LOGXOR (\ ACC 1000)
                     (TRUNCATE ACC 1000))))
      ((FUNCALL TERMINAL-IO ':TYI-NO-HANG)
       ACC)
    (SI:%BEEP (LSH FREQ O) M))))

(DEFDEMO "Munching Tunes" "Computer-composed music, based on the Munching Squares algorithm."
  "Munching Tunes"
  ("From the beginning" "Play the whole thing from the beginning." (MUNCHING-TUNES 1001 0))
  ("Interesting" "Start in the middle at an interesting point." (MUNCHING-TUNES 1001 571565)))

(PROCLAIM '(SPECIAL LIVE-BOUNCE-LL))

(DEFUN LIVE-BOUNCE (&OPTIONAL (DELAY 0) &aux window)
 (WITH-REAL-TIME
  (OR (BOUNDP 'LIVE-BOUNCE-LL)
      (SETQ LIVE-BOUNCE-LL (CREATE-SWITCH-REGISTER
                             (make-instance 'tv:window
                                            :edges '(122 40 1035 100)
                                            :blinker-p nil
                                            :borders nil
                                            :label nil
                                            :save-bits t)
                             20. 1 1 T)))
  (setq window (send live-bounce-ll :get-window))
  (send window :expose)
  (FUNCALL LIVE-BOUNCE-LL NIL)
  (DO ((NB 1)
       (DNB 1)
       (DIR 1)
       (LT 1)
       (COMP 0))
      ((send window :TYI-NO-HANG))
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
          ((NEQ 0 (LOGAND #o4000000 LT))
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
    (DO ((I DELAY (1- I)))
        ((= I 0))))
  (send window :deselect)))

(DEFDEMO "Live Bounce" "A light-hack based on a program for the late SIPB PDP8/S."
  (LIVE-BOUNCE))

(DEFUN GREEN-HORNET (&OPTIONAL (WINDOW TERMINAL-IO) (SEPARATION 40))
  (WITH-REAL-TIME
    (SEND WINDOW :CLEAR-WINDOW)
    (SEND WINDOW :HOME-DOWN)
    (MULTIPLE-VALUE-BIND (IW IH)
        (FUNCALL WINDOW ':INSIDE-SIZE)
      (LET ((CENTER-X1 (- (TRUNCATE IW 2) (TRUNCATE SEPARATION 2)))
            (CENTER-X2 (+ (TRUNCATE IW 2) (TRUNCATE SEPARATION 2)))
            (CENTER-Y (TRUNCATE IH 2)))
        (DO ((I (- (MIN CENTER-Y CENTER-X1) 10.) (1- I)))
            (( I 5))
          (SEND WINDOW
                :DRAW-CIRCLE (IF (BIT-TEST 20 I) CENTER-X1 CENTER-X2)
                             CENTER-Y I))))
    (SEND WINDOW :TYI)
    T))

(DEFUN CIRCLES (&OPTIONAL (WINDOW *TERMINAL-IO*))
  (WITH-REAL-TIME
    (SEND WINDOW :CLEAR-WINDOW)
    (SEND WINDOW :HOME-DOWN)
    (MULTIPLE-VALUE-BIND (IW IH)
        (SEND WINDOW :INSIDE-SIZE)
      (LET ((CENTER-X (// IW 2))
            (CENTER-Y (// IH 2)))
        (DO ((I (- (MIN CENTER-X CENTER-Y) 40) (- I 5)))
            (( I 5))
          (SEND WINDOW :DRAW-CIRCLE CENTER-X CENTER-Y I))))
    (SEND WINDOW :TYI)
    T))

;;; The following are so boring that they should not show up on the menu. - DLW
;; ;These are here to demonstrate how absurdly slow the :DRAW-CIRCLE message is
;; ;Also the hornet isn't green any more, it's white
;(DEFDEMO "Green Hornet" (GREEN-HORNET))
;(DEFDEMO "Circles" (CIRCLES))

(DEFCONST *LEXIPHAGE-INITIAL-DELAY* 50000.)     ; Initial delay to read string.
(DEFCONST *LEXIPHAGE-PERIOD* 2000.)             ; Slowness of the phage.
(DEFCONST *LEXIPHAGE-MOUTH-X* 40.)              ; Width of the whole mouth.
(DEFCONST *LEXIPHAGE-MOUTH-Y* 4.)               ; Width of one jaw at right end.
(DEFCONST *LEXIPHAGE-TOOTH-Y* 10.)              ; Width of tooth at right end.

(DEFUN LEXIPHAGE (&OPTIONAL (STRING "LEXIPHAGE"))
  (OR (BOUNDP 'FONTS:43VXMS)
      (LOAD "SYS: FONTS; 43VXMS" :PACKAGE 'FONTS))
  (LET ((WINDOW (MAKE-INSTANCE 'TV:WINDOW
                               :BOTTOM 300.
                               :FONT-MAP (LIST FONTS:43VXMS)
                               :BLINKER-P NIL
                               :MORE-P NIL
                               :SAVE-BITS T
                               :LABEL NIL)))
    (MULTIPLE-VALUE-BIND (WIDTH HEIGHT)
        (SEND WINDOW :INSIDE-SIZE)
      (LET* ((STRING-WIDTH (SEND WINDOW :STRING-LENGTH STRING))
             (CENTER-Y (TRUNCATE HEIGHT 2))
             (HALF-STRING-HEIGHT 30)
             (LEFT-EDGE (MAX 0 (TRUNCATE (- WIDTH STRING-WIDTH) 2)))
             (TOP-EDGE (MAX 0 (- CENTER-Y HALF-STRING-HEIGHT)))
             (CHAR-ALUF (TV:SHEET-CHAR-ALUF WINDOW))
             (ERASE-ALUF (TV:SHEET-ERASE-ALUF WINDOW)))
        (TV:SHEET-FORCE-ACCESS (WINDOW)
           (SEND WINDOW :CLEAR-WINDOW)
           (SEND WINDOW :SET-CURSORPOS LEFT-EDGE TOP-EDGE)
           (SEND WINDOW :STRING-OUT STRING))
        (TV:WINDOW-CALL (WINDOW :DEACTIVATE)
          (WITH-REAL-TIME
            ;; Initial delay, so user can read the string.
            (DOTIMES (I *LEXIPHAGE-INITIAL-DELAY*))
            (DO ((X (- LEFT-EDGE 100) (1+ X))
                 (DY 0)
                 (END-X (- (+ LEFT-EDGE STRING-WIDTH 30)        ; Fudge by 30
                           *LEXIPHAGE-MOUTH-X*)))
                (( X END-X))
              (SETQ DY (IF (ZEROP DY) HALF-STRING-HEIGHT (1- DY)))
              (SEND WINDOW ':DRAW-TRIANGLE
                       X CENTER-Y
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y DY)
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       CHAR-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       X CENTER-Y
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y DY)
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       CHAR-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y (- DY *LEXIPHAGE-TOOTH-Y*))
                       (+ X (FIX (* 0.9S0 *LEXIPHAGE-MOUTH-X*)))
                       (+ CENTER-Y (FIX (* 0.9S0 (- DY *LEXIPHAGE-MOUTH-Y*))))
                       CHAR-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y (- DY *LEXIPHAGE-TOOTH-Y*))
                       (+ X (FIX (* 0.9S0 *LEXIPHAGE-MOUTH-X*)))
                       (- CENTER-Y (FIX (* 0.9S0 (- DY *LEXIPHAGE-MOUTH-Y*))))
                       CHAR-ALUF)
              (DOTIMES (I *LEXIPHAGE-PERIOD*))
              (SEND WINDOW ':DRAW-TRIANGLE
                       X CENTER-Y
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y DY)
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       ERASE-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       X CENTER-Y
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y DY)
                       X (+ CENTER-Y DY)
                       ERASE-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       (+ X *LEXIPHAGE-MOUTH-X*) (+ CENTER-Y (- DY *LEXIPHAGE-TOOTH-Y*))
                       (+ X (FIX (* 0.8S0 *LEXIPHAGE-MOUTH-X*)))
                       (+ CENTER-Y (FIX (* 0.8S0 (- DY *LEXIPHAGE-MOUTH-Y*))))
                       ERASE-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y (- DY *LEXIPHAGE-TOOTH-Y*))
                       (+ X (FIX (* 0.8S0 *LEXIPHAGE-MOUTH-X*)))
                       (- CENTER-Y (FIX (* 0.8S0 (- DY *LEXIPHAGE-MOUTH-Y*))))
                       ERASE-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       X CENTER-Y
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y DY)
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y (- DY *LEXIPHAGE-MOUTH-Y*))
                       ERASE-ALUF)
              (SEND WINDOW ':DRAW-TRIANGLE
                       X CENTER-Y
                       (+ X *LEXIPHAGE-MOUTH-X*) (- CENTER-Y DY)
                       X (- CENTER-Y DY)
                       ERASE-ALUF)))))))
  "Lexiphage!")

(DEFDEMO "Lexiphage" "The word eater, based on a hack by John Doty." (LEXIPHAGE))

;(DECLARE (SPECIAL MF10-HACK5-BAR))

;(DEFUN MF10-HACK5 (&OPTIONAL (ZAP 1000) (13357ALIAS 13357))
; (WITH-REAL-TIME
;  (SEND *TERMINAL-IO* :CLEAR-WINDOW)
;  (SEND *TERMINAL-IO* :HOME-DOWN)
;  (OR (BOUNDP 'MF10-HACK5-BAR)
;      (SETQ MF10-HACK5-BAR (CREATE-SWITCH-REGISTER *TERMINAL-IO* 20 100 20 T)))
;  (SEND MF10-HACK5-BAR NIL)
;  (DO ((I 13357ALIAS (ROT I 1)))
;      ((SEND *TERMINAL-IO* :TYI-NO-HANG))
;    (SEND MF10-HACK5-BAR I)
;    (DO ((J 0 (+ J 1)))
;       ((= J ZAP))))))

;(DEFVAR PRINT-BIG-PREVIOUS NIL)

;(DEFUN PRINT-BIG (&OPTIONAL (FONT PRINT-BIG-PREVIOUS))
;   (TERPRI)
;   (COND ((NULL FONT) (SETQ FONT FONTS:BIGFNT)))
;   (SETQ PRINT-BIG-PREVIOUS (TV:SHEET-CURRENT-FONT TERMINAL-IO))
;   (SEND *TERMINAL-IO* :SET-FONT-MAP (LIST FONT))
;   (SEND *TERMINAL-IO* :SET-CURRENT-FONT FONT))

;(DEFUN PRINT-SMALL ()
;   (TERPRI)
;   (SEND *TERMINAL-IO* :SET-FONT-MAP (LIST FONTS:CPTFONT))
;   (SEND *TERMINAL-IO* :SET-CURRENT-FONT FONTS:CPTFONT))


;(DECLARE (SPECIAL FEFS ADL-QS ADL-VARS NAME-QS FREE-QS TOTAL-BOXED-LENGTH
;                TOTAL-LENGTH))

;(DEFUN COMPUTE-FEF-STATS NIL
;  (PROG ((FEFS 0) (ADL-QS 0) (ADL-VARS 0) (NAME-QS 0) (FREE-QS 0) (TOTAL-BOXED-LENGTH 0)
;         (TOTAL-LENGTH 0))
;        (MAPATOMS-ALL (FUNCTION (LAMBDA (X &AUX FEFP TEM)
;                              (COND ((AND
;                                      (FBOUNDP X)
;                                      (=
;                                       (%P-DATA-TYPE (SETQ FEFP (FUNCTION-CELL-LOCATION X)))
;                                       DTP-FEF-POINTER))
;                                     (SETQ FEFP (CDR FEFP))
;                                     (SETQ FEFS (1+ FEFS))
;                                     (SETQ
;                                      ADL-VARS
;                                      (+ ADL-VARS
;                                         (%P-LDB-OFFSET SI:%%FEFHI-MS-BIND-DESC-LENGTH
;                                                        FEFP
;                                                        SI:%FEFHI-MISC)))
;                                     (SETQ
;                                      TOTAL-BOXED-LENGTH
;                                      (+ TOTAL-BOXED-LENGTH
;                                         (TRUNCATE (%P-LDB-OFFSET SI:%%FEFH-PC FEFP SI:%FEFHI-IPC) 2)))
;                                     (SETQ
;                                      TOTAL-LENGTH
;                                      (+ TOTAL-LENGTH
;                                         (%P-CONTENTS-OFFSET FEFP SI:%FEFHI-STORAGE-LENGTH)))
;                                     (PROG ((LST (SI:GET-MACRO-ARG-DESC-POINTER FEFP))
;                                            VAR-DESC)
;                                           (SETQ ADL-QS (+ ADL-QS (LENGTH LST)))
;                                      L    (COND ((NULL LST)
;                                                  (RETURN NIL)))
;                                           (SETQ VAR-DESC (CAR LST))
;                                           (COND ((NOT
;                                                   (ZEROP (LOGAND VAR-DESC
;                                                                  SI:%FEF-NAME-PRESENT)))
;                                                  (SETQ LST (CDR LST))
;                                                  (SETQ NAME-QS (1+ NAME-QS))))
;                                           (COND ((= (MASK-FIELD SI:%%FEF-ARG-SYNTAX
;                                                                VAR-DESC)
;                                                     SI:FEF-ARG-FREE)
;                                                  (SETQ FREE-QS (1+ FREE-QS))))
;                                           (COND ((OR
;                                                   (= (SETQ TEM (LDB SI:%%FEF-INIT-OPTION
;                                                                    VAR-DESC))
;                                                      SI:FEF-INI-PNTR)
;                                                   (= TEM SI:FEF-INI-C-PNTR)
;                                                   (= TEM SI:FEF-INI-OPT-SA)
;                                                   (= TEM SI:FEF-INI-EFF-ADR))
;                                                  (SETQ LST (CDR LST))))
;                                           (SETQ LST (CDR LST))
;                                           (GO L)))))))
;        (FORMAT STANDARD-OUTPUT
;                       "~%FEFS ~D, TOTAL LENGTH ~D, BOXED LENGTH ~D, ADL-VARS ~D,/
;ADL-QS ~D, LOCAL NAME QS ~D, FREE-VARIABLE-ADL-QS ~D,"
;                       FEFS
;                       TOTAL-LENGTH
;                       TOTAL-BOXED-LENGTH
;                       ADL-VARS
;                       ADL-QS
;                       NAME-QS
;                       FREE-QS)))


;(DEFUN DISPLAY-MOUSE-REGS NIL
;  (DISPLAY-UNIBUS-LOCATION TV:MOUSE-REG1 TV:MOUSE-REG2))

;(DEFUN DISPLAY-UNIBUS-LOCATION (&REST ADDRESS-LIST)
;  (LEXPR-FUNCALL 'DISPLAY-LOCATION '%UNIBUS-READ 16. ADDRESS-LIST))

;(DEFUN DISPLAY-CC-KEYBOARD-REGS NIL
;  (DISPLAY-CC-UNIBUS-LOCATION 764112 764102 764100))

;(DEFUN DISPLAY-CC-MOUSE-REGS NIL
;  (DISPLAY-CC-UNIBUS-LOCATION TV:MOUSE-REG1 TV:MOUSE-REG2))

;(DEFUN DISPLAY-CC-UNIBUS-LOCATION (&REST ADDRESS-LIST)
;  (LEXPR-FUNCALL 'DISPLAY-LOCATION 'CADR:DBG-READ 16. ADDRESS-LIST))

;(DEFUN DISPLAY-XBUS-LOCATION (&REST ADDRESS-LIST)
;  (LEXPR-FUNCALL 'DISPLAY-LOCATION '%XBUS-READ 32. ADDRESS-LIST))

;(DEFUN DISPLAY-LOCATION (FCTN BITS &REST ADDRESS-LIST)
;  (LET* ((N (LENGTH ADDRESS-LIST))
;        (LITES (MAKE-ARRAY N)))
;    (DOTIMES (I N)
;      (AS-1 (CREATE-SWITCH-REGISTER *TERMINAL-IO* BITS NIL  (+ (* I 44) 40) T)
;           LITES
;           I))
;    (SEND TERMINAL-IO :CLEAR-WINDOW)
;    (DOTIMES (I N)
;      (SEND (AR-1 LITES I) NIL))
;    (DO NIL ((EQ 203 (SEND TERMINAL-IO ':TYI-NO-HANG)))
;      (DO ((I 0 (1+ I))
;          (L ADDRESS-LIST (CDR L)))
;         ((>= I N))
;       (SEND (AR-1 LITES I)
;                (SEND FCTN (CAR L)))))))

;(DEFUN RANDOM-TEST (&OPTIONAL (WINDOW *TERMINAL-IO*) &AUX TEM)
;    (SEND WINDOW ':CLEAR-WINDOW)
;    (DO () (())
;      (SETQ TEM (RANDOM))
;      (AND (SEND TERMINAL-IO ':TYI-NO-HANG) (RETURN NIL))
;      (SEND WINDOW ':DRAW-POINT (LDB 2010 TEM) (LDB 1010 TEM))))

;(defun dance (&optional (WINDOW *TERMINAL-IO*)
;                       (mina 100) (maxa 456) (minb 200) (maxb 565)
;                        (minc 60) (maxc 1076) (mind 300) (maxd 1100))
; (WITH-REAL-TIME
;       (SEND WINDOW ':DRAW-LINE mina minb maxc maxd tv:alu-xor)
;       (do ((a mina)
;            (b minb)
;            (c maxc)
;            (d maxd)
;            (oa)(ob)(oc)(od)
;            (da 1)
;            (db 1)
;            (dc -1)
;            (dd -1))
;         ((send *terminal-io* :tyi-no-hang)
;         (SEND WINDOW :draw-line a b c d tv:alu-xor))
;       (setq oa a ob b oc c od d)
;       (setq a (+ a da))
;       (cond ((= da 1)
;             (cond (( a maxa)
;                    (setq da -1))))
;            (( a mina)
;             (setq da 1)))
;       (setq b (+ b db))
;       (cond ((= db 1)
;             (cond (( b maxb)
;                    (setq db -1))))
;            (( b minb)
;             (setq db 1)))
;       (setq c (+ c dc))
;       (cond ((= dc 1)
;             (cond (( c maxc)
;                    (setq dc -1))))
;            (( c minc)
;             (setq dc 1)))
;       (setq d (+ d dd))
;       (cond ((= dd 1)
;             (cond (( d maxd)
;                    (setq dd -1))))
;            (( d mind)
;             (setq dd 1)))
;       (SEND WINDOW :draw-line a b c d tv:alu-xor)
;       (SEND WINDOW :draw-line oa ob oc od tv:alu-xor))))

;(defun spazz (&OPTIONAL (WINDOW TERMINAL-IO))
;  (MULTIPLE-VALUE-BIND (X2 Y2)
;      (SEND WINDOW ':INSIDE-SIZE)
;    (LET* ((mina (random x2))
;          (minc (random x2))
;          (minb (random y2))
;          (mind (random y2))
;          (awid (random (- x2 mina)))
;          (cwid (random (- x2 minc)))
;          (bwid (random (- y2 minb)))
;          (dwid (random (- y2 mind))))
;      (dance WINDOW
;            mina (+ mina awid) minb (+ minb bwid) minc (+ minc cwid) mind (+ mind dwid)))))

;(COMMENT ;This is not really very interesting by today's standards.
;(DEFDEMO "Dance" "An animated line moving around on the screen." "Dance"
;        ("Normal" "" (DANCE))
;        ("Spastic" "" (SPAZZ)))
;)

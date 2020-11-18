;;; -*- Mode:LISP; Package:TV; Readtable:ZL; Base:10 -*-

(DEFUN-METHOD DRAW-STRING-HORIZONTAL GRAPHICS-MIXIN
              (STRING LAST-INDEX FROM-X Y EXTRA EXTRA-PER-GAP PERIOD
               SHORTFALL BACK-P FAT-P ALU FONT-TABLE DEFAULT-FONT)
  (MULTIPLE-VALUE-BIND (X-EDGE Y-EDGE) (SEND SELF ':INSIDE-SIZE)
    (LOOP FOR I FROM 0 TO LAST-INDEX
          FOR CHAR = (IF BACK-P (AREF STRING (- LAST-INDEX I)) (AREF STRING I))
          FOR FONT = (IF FAT-P (AREF FONT-TABLE (LDB %%ch-font CHAR)) DEFAULT-FONT)
          FOR FONT-CHAR-WIDTH-TABLE = (FONT-CHAR-WIDTH-TABLE FONT)
          FOR FONT-BASELINE = (FONT-BASELINE FONT)
          FOR FONT-HEIGHT = (FONT-CHAR-HEIGHT FONT)
          WITH ADJUST = (COND ((MINUSP EXTRA) -1)
                              ((ZEROP EXTRA) 0)
                              (T 1))
          FOR CONDITION = (AND PERIOD (NOT (ZEROP SHORTFALL)) (ZEROP (REMAINDER I PERIOD)))
          FOR X = FROM-X THEN (IF BACK-P
                                  (- X CH-WIDTH EXTRA-PER-GAP
                                     (COND (CONDITION (SETQ SHORTFALL (- SHORTFALL ADJUST))
                                                      ADJUST)
                                           (T 0)))
                                  (+ X CH-WIDTH EXTRA-PER-GAP
                                     (COND (CONDITION (SETQ SHORTFALL (- SHORTFALL ADJUST))
                                                      ADJUST)
                                           (T 0))))
          FOR CH-WIDTH = (IF FONT-CHAR-WIDTH-TABLE
                               (AREF FONT-CHAR-WIDTH-TABLE (LDB %%CH-CHAR CHAR))
                               (FONT-CHAR-WIDTH FONT))
          DO (IF BACK-P
                 (DRAW-CHAR-TO-LEFT (LDB %%CH-CHAR CHAR) FONT X Y FONT-HEIGHT CH-WIDTH
                                    FONT-BASELINE SELF X-EDGE Y-EDGE ALU)
                 (DRAW-CHAR-TO-RIGHT (LDB %%CH-CHAR CHAR) FONT X Y FONT-HEIGHT CH-WIDTH
                                     FONT-BASELINE SELF X-EDGE Y-EDGE ALU))
        FINALLY (RETURN X Y))))

;;; Draw a (perhaps multi-font) string vertically, expanding or compressing
(DEFUN-METHOD DRAW-STRING-VERTICAL GRAPHICS-MIXIN
              (STRING LAST-INDEX FROM-Y X EXTRA EXTRA-PER-GAP PERIOD SHORTFALL
               FAT-P ALU FONT-TABLE DEFAULT-FONT RIGHT-P)
  (MULTIPLE-VALUE-BIND (X-EDGE Y-EDGE) (SEND SELF ':INSIDE-SIZE)
    (LOOP FOR I FROM 0 TO LAST-INDEX
          FOR CHAR = (AREF STRING I)
          FOR FONT = (IF FAT-P (AREF FONT-TABLE (LDB %%CH-FONT CHAR)) DEFAULT-FONT)
          FOR FONT-CHAR-WIDTH-TABLE = (FONT-CHAR-WIDTH-TABLE FONT)
          FOR FONT-BASELINE = 0                 ;to fake out character drawing code
          FOR FONT-HEIGHT = (FONT-CHAR-HEIGHT FONT)
          WITH ADJUST = (COND ((MINUSP EXTRA) -1)
                              ((ZEROP EXTRA) 0)
                              (T 1))
          FOR CONDITION = (AND PERIOD (NOT (ZEROP SHORTFALL)) (ZEROP (REMAINDER I PERIOD)))
          FOR Y = FROM-Y THEN (+ Y FONT-HEIGHT EXTRA-PER-GAP
                                 (COND (CONDITION (SETQ SHORTFALL (- SHORTFALL ADJUST))
                                                  ADJUST)
                                       (T 0)))
          FOR CH-WIDTH = (IF FONT-CHAR-WIDTH-TABLE
                               (AREF FONT-CHAR-WIDTH-TABLE (LDB %%CH-CHAR CHAR))
                               (FONT-CHAR-WIDTH FONT))
          DO (IF RIGHT-P
                 (DRAW-CHAR-TO-RIGHT (LDB %%CH-CHAR CHAR) FONT X Y FONT-HEIGHT CH-WIDTH
                                     FONT-BASELINE SELF X-EDGE Y-EDGE ALU)
                 (DRAW-CHAR-TO-LEFT (LDB %%CH-CHAR CHAR) FONT X Y FONT-HEIGHT CH-WIDTH
                                    FONT-BASELINE SELF X-EDGE Y-EDGE ALU))
        FINALLY (RETURN X Y))))

;;; Draw a (perhaps multi-font) string diagonally, expanding or compressing
(DEFUN-METHOD DRAW-STRING GRAPHICS-MIXIN
              (STRING LAST-INDEX FROM-X FROM-Y TO-X TO-Y X-EXTRA Y-EXTRA
               X-EXTRA-PER-GAP Y-EXTRA-PER-GAP X-PERIOD Y-PERIOD SHORTFALL-X
               SHORTFALL-Y BACK-P FAT-P ALU FONT-TABLE DEFAULT-FONT
               DELTA-X DELTA-Y STRETCH-P)
  (MULTIPLE-VALUE-BIND (X-EDGE Y-EDGE)
      (SEND SELF ':INSIDE-SIZE)
      (LOOP FOR I FROM 0 TO LAST-INDEX
            FOR CHAR = (IF BACK-P (AREF STRING (- LAST-INDEX I)) (AREF STRING I))
            FOR FONT = (IF FAT-P (AREF FONT-TABLE (LDB %%CH-FONT CHAR)) DEFAULT-FONT)
            FOR FONT-CHAR-WIDTH-TABLE = (FONT-CHAR-WIDTH-TABLE FONT)
            FOR FONT-BASELINE = (FONT-BASELINE FONT)
            WITH X-ADJUST = (COND ((MINUSP X-EXTRA) -1)
                                  ((ZEROP X-EXTRA) 0)
                                  (T 1))
            AND Y-ADJUST = (COND ((MINUSP Y-EXTRA) -1)
                                 ((ZEROP Y-EXTRA) 0)
                                  (T 1))
            WITH UP-P = (LESSP TO-Y FROM-Y)
            FOR CONDITION-X = (AND X-PERIOD (NOT (ZEROP SHORTFALL-X))
                                   (ZEROP (REMAINDER I X-PERIOD)))
            AND CONDITION-Y = (AND Y-PERIOD (NOT (ZEROP SHORTFALL-Y))
                                   (ZEROP (REMAINDER I Y-PERIOD)))
            FOR LX = FROM-X THEN (IF BACK-P
                                    (- LX CH-WIDTH X-EXTRA-PER-GAP ;(break back)
                                       (COND (CONDITION-X (SETQ SHORTFALL-X
                                                                (- SHORTFALL-X X-ADJUST))
                                                        X-ADJUST)
                                             (T 0)))
                                    (+ LX CH-WIDTH X-EXTRA-PER-GAP ;(break forward)
                                       (COND (CONDITION-X (SETQ SHORTFALL-X
                                                                (- SHORTFALL-X X-ADJUST))
                                                        X-ADJUST)
                                             (T 0))))
            FOR LY = FROM-Y THEN (IF UP-P
                                    (- LY FONT-HEIGHT Y-EXTRA-PER-GAP ;(break up)
                                       (COND (CONDITION-Y (SETQ SHORTFALL-Y
                                                                (- SHORTFALL-Y Y-ADJUST))
                                                        Y-ADJUST)
                                             (T 0)))
                                    (+ LY FONT-HEIGHT Y-EXTRA-PER-GAP ;(break down)
                                       (COND (CONDITION-Y (SETQ SHORTFALL-Y
                                                                (- SHORTFALL-Y Y-ADJUST))
                                                        Y-ADJUST)
                                             (T 0))))
            FOR CH-WIDTH = (IF FONT-CHAR-WIDTH-TABLE
                                 (AREF FONT-CHAR-WIDTH-TABLE (LDB %%CH-CHAR CHAR))
                                 (FONT-CHAR-WIDTH FONT))
            FOR FONT-HEIGHT = (FONT-CHAR-HEIGHT FONT)
            WITH X-LONGER-P = (GREATERP (ABS DELTA-X) (ABS DELTA-Y))
            WITH TEST = (// (IF X-LONGER-P (ABS DELTA-X) (ABS DELTA-Y)) 2)
            WITH X = FROM-X AND Y = FROM-Y
            WITH RIGHT-P = (GREATERP TO-X FROM-X)
            DO (MULTIPLE-VALUE (X Y TEST)
                 (NEXT-FREE-PIXEL X Y TEST DELTA-X DELTA-Y
                                  X-LONGER-P LX LY UP-P RIGHT-P STRETCH-P))
            (IF RIGHT-P (DRAW-CHAR-TO-RIGHT (LDB %%CH-CHAR CHAR) FONT X Y FONT-HEIGHT
                                            CH-WIDTH FONT-BASELINE SELF X-EDGE Y-EDGE ALU)
                (DRAW-CHAR-TO-LEFT (LDB %%CH-CHAR CHAR) FONT X Y FONT-HEIGHT CH-WIDTH
                                   FONT-BASELINE SELF X-EDGE Y-EDGE ALU)))))

;;; Draw a character string at an arbitrary place in a window.  Only
;;; printing characters (including space) are allowed in the string.  The
;;; left baseline points of the characters lie on the line defined by FROM-X, FROM-Y
;;; TOWARD-X and TOWARD-Y.  If TOWARD-X is less than FROM-X, the string is written backwards
;;; (i.e. the right-most character is at FROM-X, FROM-Y and the right baseline points
;;; are used); otherwise the leftmost character starts at FROM-X, FROM-Y.
;;; If STRETCH-P is true, the string will end near END-X by END-Y (though maybe no
;;; exactly, because of roundoff error.)
;;; The given ALUF and FONT are used.  Art-fat-strings (with font changes) are OK.
;;; The stopping location is returned (as two values).

;;; Problems:
;;; extreme slopes cause character overstrikes
;;; doesn't check for illegal characters
;;; doesn't use vsp in vertical space calculation
;;; doesn't skip everything if string lies completely outside screen
;;; doesn't apportion compress/squeeze space with regard to character size
;;; should really return place to start continuation of string, rather than coordinates
;;;    of last char

(DEFMETHOD (GRAPHICS-MIXIN :DRAW-STRING)
           (STRING FROM-X FROM-Y &OPTIONAL (TOWARD-X (1+ FROM-X)) (TOWARD-Y FROM-Y)
                                           (STRETCH-P NIL) (FONT (SHEET-CURRENT-FONT SELF))
                                           (ALU CHAR-ALUF))
  "Draws a character string between given points.
If stretch-p is non-nil, the characters will be spaced to closely fit
between the points;  otherwise, they merely use the line defined by
the points.  If toward-x is less than from-x, the string ends at from-x from-y;
otherwise it starts there and in both cases it will read from left to right."

 (LET* ((STRING (STRING STRING))                        ;convert possible randomness to string
        (LENGTH (ARRAY-ACTIVE-LENGTH STRING))
        (END-DISPLACEMENT (1- LENGTH))          ;highest aref index and also number of gaps
                                                ;between characters
        (DELTA-X (- TOWARD-X FROM-X))
        (DELTA-Y (- TOWARD-Y FROM-Y))
        (FONT-TABLE (SEND SELF ':FONT-MAP))
        (FAT-STRING-P (EQ 'ART-FAT-STRING (ARRAY-TYPE STRING))) ;predicate for 16-bit strings
        (BACKWARDS-P (MINUSP DELTA-X))          ;predicate for backwards strings
        (VERTICAL-P (ZEROP DELTA-X))
        (HORIZONTAL-P (ZEROP DELTA-Y)))
  (COND (HORIZONTAL-P
         (COND (STRETCH-P
                (MULTIPLE-VALUE-BIND (X-DESIRED IGNORE)
                    (STRING-NORMAL-SIZE STRING END-DISPLACEMENT FAT-STRING-P FONT-TABLE FONT)
                  (LET* ((EXCESS-X-SPACE (- (ABS DELTA-X) (ABS X-DESIRED)))     ;+ = expand
                         (EXCESS-X-PER-GAP (// EXCESS-X-SPACE END-DISPLACEMENT))
                         (SHORTFALL-X (- EXCESS-X-SPACE
                                         (* END-DISPLACEMENT EXCESS-X-PER-GAP)))
                         (X-PERIOD (IF (ZEROP SHORTFALL-X) NIL
                                       (// END-DISPLACEMENT SHORTFALL-X))))
                    (DRAW-STRING-HORIZONTAL STRING END-DISPLACEMENT FROM-X FROM-Y
                                            EXCESS-X-SPACE EXCESS-X-PER-GAP X-PERIOD
                                            SHORTFALL-X BACKWARDS-P FAT-STRING-P ALU
                                            FONT-TABLE FONT))))
               (T (DRAW-STRING-HORIZONTAL STRING END-DISPLACEMENT FROM-X FROM-Y 0 0 NIL 0
                                          BACKWARDS-P FAT-STRING-P ALU FONT-TABLE FONT))))
        (VERTICAL-P
         (LET* ((Y-START (MIN FROM-Y TOWARD-Y)) ;canonicalize vertical to always go down
                (Y-END (MAX FROM-Y TOWARD-Y))
                (RIGHT-FLAG (LESSP FROM-Y TOWARD-Y))  ;put chars on right side of line if down
                (DELTA-Y (- Y-END Y-START)))
           (COND (STRETCH-P
                  (MULTIPLE-VALUE-BIND (IGNORE Y-DESIRED)
                      (STRING-NORMAL-SIZE STRING END-DISPLACEMENT FAT-STRING-P
                                          FONT-TABLE FONT)
                    (LET* ((EXCESS-Y-SPACE (- (ABS DELTA-Y) (ABS Y-DESIRED)))
                           (EXCESS-Y-PER-GAP (// EXCESS-Y-SPACE END-DISPLACEMENT))
                           (SHORTFALL-Y (- EXCESS-Y-SPACE (* END-DISPLACEMENT
                                                             EXCESS-Y-PER-GAP)))
                           (Y-PERIOD (IF (ZEROP SHORTFALL-Y) NIL (// END-DISPLACEMENT
                                                                     SHORTFALL-Y))))
                      (DRAW-STRING-VERTICAL STRING END-DISPLACEMENT Y-START FROM-X
                                            EXCESS-Y-SPACE EXCESS-Y-PER-GAP Y-PERIOD
                                            SHORTFALL-Y FAT-STRING-P ALU FONT-TABLE FONT
                                            RIGHT-FLAG))))
                 (T (DRAW-STRING-VERTICAL STRING END-DISPLACEMENT Y-START FROM-X
                                          0 0 NIL 0 FAT-STRING-P ALU FONT-TABLE FONT
                                          RIGHT-FLAG)))))
        (T (COND (STRETCH-P
                  (MULTIPLE-VALUE-BIND (X-DESIRED Y-DESIRED)
                      (STRING-NORMAL-SIZE STRING END-DISPLACEMENT FAT-STRING-P
                                          FONT-TABLE FONT)
                    (LET* ((EXCESS-X-SPACE (- (ABS DELTA-X) (ABS X-DESIRED)))
                           (EXCESS-X-PER-GAP (// EXCESS-X-SPACE END-DISPLACEMENT))
                           (SHORTFALL-X (- EXCESS-X-SPACE (* END-DISPLACEMENT
                                                             EXCESS-X-PER-GAP)))
                           (X-PERIOD (IF (ZEROP SHORTFALL-X) NIL (// END-DISPLACEMENT
                                                                     SHORTFALL-X)))
                           (EXCESS-Y-SPACE (- (ABS DELTA-Y) (ABS Y-DESIRED)))
                           (EXCESS-Y-PER-GAP (// EXCESS-Y-SPACE END-DISPLACEMENT))
                           (SHORTFALL-Y (- EXCESS-Y-SPACE (* END-DISPLACEMENT
                                                             EXCESS-Y-PER-GAP)))
                           (Y-PERIOD (IF (ZEROP SHORTFALL-Y) NIL (// END-DISPLACEMENT
                                                                     SHORTFALL-Y))))
                      (DRAW-STRING STRING END-DISPLACEMENT FROM-X FROM-Y TOWARD-X TOWARD-Y
                                   EXCESS-X-SPACE EXCESS-Y-SPACE EXCESS-X-PER-GAP
                                   EXCESS-Y-PER-GAP X-PERIOD Y-PERIOD SHORTFALL-X SHORTFALL-Y
                                   BACKWARDS-P FAT-STRING-P ALU FONT-TABLE FONT
                                   DELTA-X DELTA-Y STRETCH-P))))
                 (T (DRAW-STRING STRING END-DISPLACEMENT FROM-X FROM-Y TOWARD-X TOWARD-Y
                                 0 0 0 0
                                 NIL NIL        0 0 BACKWARDS-P FAT-STRING-P ALU FONT-TABLE
                                 FONT DELTA-X DELTA-Y STRETCH-P)))))))

;Go through a string (perhaps multi-font) accumulating character height and width
(DEFUN STRING-NORMAL-SIZE (STRING LAST-INDEX FAT-P FONT-MAP DEFAULT-FONT)
  (LOOP FOR I FROM 0 TO LAST-INDEX
        FOR CHAR = (AREF STRING I)
        FOR FONT = (IF FAT-P (AREF FONT-MAP (LDB %%CH-FONT CHAR)) DEFAULT-FONT)
        FOR FONT-CHAR-WIDTH-TABLE = (FONT-CHAR-WIDTH-TABLE FONT)
        SUM (IF FONT-CHAR-WIDTH-TABLE (AREF FONT-CHAR-WIDTH-TABLE (LDB %%CH-CHAR CHAR))
                (FONT-CHAR-WIDTH FONT)) INTO X-TOTAL
        SUM (FONT-CHAR-HEIGHT FONT) INTO Y-TOTAL
        FINALLY ;(format t "x-size=~D, y-size=~D" x-total y-total)
                (RETURN X-TOTAL Y-TOTAL)))

(DEFUN NEXT-FREE-PIXEL (CURRENT-X CURRENT-Y TEST DELTA-X DELTA-Y X-LONG-P X-LIM Y-LIM
                        UP-P RIGHT-P STRETCH-P)
  (LOOP UNTIL (AND-OR-OR-BASED-ON-STRETCH-P STRETCH-P
                                            (COND (RIGHT-P (>= CURRENT-X X-LIM))
                                                  (T (<= CURRENT-X X-LIM)))
                                            (COND (UP-P (<= CURRENT-Y Y-LIM))
                                                  (T (>= CURRENT-Y Y-LIM))))
        WITH LAST-X = CURRENT-X AND LAST-Y = CURRENT-Y
        AND LAST-TEST = TEST
        DO (SETQ LAST-X CURRENT-X LAST-Y CURRENT-Y LAST-TEST TEST)
           ;(format t "x=~D, y=~D, lx=~D, ly=~D~%" current-x current-y last-x last-y)
           (MULTIPLE-VALUE (CURRENT-X CURRENT-Y TEST)
             (COND ((MINUSP DELTA-X)
                    (NEXT-PIXEL-TO-LEFT CURRENT-X CURRENT-Y TEST DELTA-X DELTA-Y X-LONG-P))
                   (T (NEXT-PIXEL-TO-RIGHT CURRENT-X CURRENT-Y TEST
                                           DELTA-X DELTA-Y X-LONG-P))))
        FINALLY (RETURN LAST-X LAST-Y LAST-TEST)))

(DEFUN AND-OR-OR-BASED-ON-STRETCH-P (FLAG C1 C2)
  (COND (FLAG (AND C1 C2))
        (T (OR C1 C2))))

;;; given current point, total displacements and test (initially long/2)
;;; return new x, y and test
(DEFUN NEXT-PIXEL-TO-RIGHT (CURRENT-X CURRENT-Y TEST DELTA-X DELTA-Y X-LONG-P)
  (LET ((Y-INCR (COND ((LESSP DELTA-Y 0)
                       -1)
                      ((GREATERP DELTA-Y 0)
                       1)
                      (T 0))))
    (COND (X-LONG-P
           (SETQ TEST (- TEST (ABS DELTA-Y)))
           (IF (MINUSP TEST) (SETQ TEST (+ TEST DELTA-X)
                                  CURRENT-Y (+ CURRENT-Y Y-INCR)))
           (SETQ CURRENT-X (1+ CURRENT-X)))
          (T
           (SETQ TEST (- TEST DELTA-X))
           (IF (MINUSP TEST) (SETQ TEST (+ TEST (ABS DELTA-Y))
                                  CURRENT-X (1+ CURRENT-X)))
           (SETQ CURRENT-Y (+ CURRENT-Y Y-INCR))))
    (VALUES CURRENT-X CURRENT-Y TEST)))

(DEFUN NEXT-PIXEL-TO-LEFT (CURRENT-X CURRENT-Y TEST DELTA-X DELTA-Y X-LONG-P)
  (LET ((Y-INCR (COND ((LESSP DELTA-Y 0)
                       -1)
                      ((GREATERP DELTA-Y 0)
                       1)
                      (T 0))))
    (COND (X-LONG-P
           (SETQ TEST (- TEST (ABS DELTA-Y)))
           (IF (MINUSP TEST) (SETQ TEST (+ TEST (MINUS DELTA-X))
                                  CURRENT-Y (+ CURRENT-Y Y-INCR)))
           (SETQ CURRENT-X (1- CURRENT-X)))
          (T
           (SETQ TEST (- TEST (MINUS DELTA-X)))
           (IF (MINUSP TEST) (SETQ TEST (+ TEST (ABS DELTA-Y))
                                  CURRENT-X (1- CURRENT-X)))
           (SETQ CURRENT-Y (+ CURRENT-Y Y-INCR))))
    (VALUES CURRENT-X CURRENT-Y TEST)))

;;; draw character so left-side baseline is at x,y and clip if whole character won't fit
(DEFUN DRAW-CHAR-TO-RIGHT (CHAR FONT X Y HEIGHT WIDTH BASELINE WINDOW X-LIM Y-LIM ALU)
  (LET ((NEW-X (- X (LEFT-KERN CHAR FONT)))
        (NEW-Y (- Y BASELINE)))
    (AND (>= NEW-X 0)
         (>= NEW-Y 0)
         (LESSP (+ NEW-X WIDTH) X-LIM)
         (LESSP (+ NEW-Y HEIGHT) Y-LIM)
         (SEND WINDOW ':DRAW-CHAR FONT CHAR NEW-X NEW-Y ALU))))

;;; draw character so right-side baseline is at x,y and clip if whole character won't fit
(DEFUN DRAW-CHAR-TO-LEFT (CHAR FONT X Y HEIGHT WIDTH BASELINE WINDOW X-LIM Y-LIM ALU)
  (LET ((NEW-X (- X WIDTH (LEFT-KERN CHAR FONT)))
        (NEW-Y (- Y BASELINE)))
    (AND (>= NEW-X 0)
         (>= NEW-Y 0)
         (LESSP (+ NEW-X WIDTH) X-LIM)
         (LESSP (+ NEW-Y HEIGHT) Y-LIM)
         (SEND WINDOW ':DRAW-CHAR FONT CHAR NEW-X NEW-Y ALU))))

;;; draw character so center of baseline is at x, y and clip if whole character won't fit
(DEFUN DRAW-CHAR-CENTERED (CHAR FONT X Y HEIGHT WIDTH BASELINE WINDOW X-LIM Y-LIM ALU)
  (LET ((NEW-X (- X (// WIDTH 2) (LEFT-KERN CHAR FONT)))
        (NEW-Y (- Y BASELINE)))
    (AND (>= NEW-X 0)
         (>= NEW-Y 0)
         (LESSP (+ NEW-X (// WIDTH 2)) X-LIM)
         (LESSP (+ NEW-Y HEIGHT) Y-LIM)
         (SEND WINDOW ':DRAW-CHAR FONT CHAR NEW-X NEW-Y ALU))))

;;; get left kern of character
(DEFUN LEFT-KERN (CHAR FONT)
   (LET ((TABLE (TV:FONT-LEFT-KERN-TABLE FONT)))
     (IF TABLE (AREF TABLE CHAR) 0)))

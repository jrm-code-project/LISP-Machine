;;;Hollerith card editor -*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

(OR (BOUNDP 'FONTS:TVFONT) (LOAD "SYS: FONTS; TVFONT QFASL"))
(OR (BOUNDP 'FONTS:TINY) (LOAD "SYS: FONTS; TINY QFASL"))

(DEFVAR CARD-WINDOW)
(DEFCONST CARD-WINDOW-TOP 300)
(DEFCONST CARD-WINDOW-BOTTOM 1200)

(DEFVAR CARD-TOP 140)
(DEFVAR CARD-WIDTH 1400)
(DEFVAR CARD-BOTTOM-HEIGHT 310)
(DEFVAR CARD-TOP-HEIGHT 50)
(DEFVAR CARD-CUT-SIZE 24)
(DEFVAR CARD-MARGIN 30)
(DEFVAR CARD-NUMBER-TOP-HEIGHT 14)
(DEFVAR CARD-NUMBER-HEIGHT 22)
(DEFVAR CARD-LETTER-TOP 2)
(DEFVAR CARD-PUNCH-ARRAY)
(DEFVAR CARD-IMAGE)

;;; Draw the outline
(DEFUN DRAW-CARD (&AUX (WID (1- CARD-WIDTH)) (TOP (+ CARD-TOP CARD-TOP-HEIGHT))
                       (TOT (+ CARD-TOP-HEIGHT CARD-BOTTOM-HEIGHT CARD-TOP)))
  (SYS:%DRAW-RECTANGLE WID CARD-BOTTOM-HEIGHT 0 TOP TV:ALU-ANDCA CARD-WINDOW)
  (SYS:%DRAW-RECTANGLE (- WID CARD-CUT-SIZE) CARD-TOP-HEIGHT CARD-CUT-SIZE CARD-TOP
                       TV:ALU-ANDCA CARD-WINDOW)
  (SYS:%DRAW-TRIANGLE (1- CARD-CUT-SIZE) CARD-TOP (1- CARD-CUT-SIZE) TOP 0 TOP
                      TV:ALU-ANDCA CARD-WINDOW)
  (SYS:%DRAW-LINE (1- CARD-CUT-SIZE) CARD-TOP WID CARD-TOP TV:ALU-IOR T CARD-WINDOW)
  (SYS:%DRAW-LINE WID CARD-TOP WID TOT TV:ALU-IOR T CARD-WINDOW)
  (SYS:%DRAW-LINE WID TOT 0 TOT TV:ALU-IOR T CARD-WINDOW)
  (SYS:%DRAW-LINE 0 TOT 0 TOP TV:ALU-IOR T CARD-WINDOW)
  (SYS:%DRAW-LINE 0 TOP (1- CARD-CUT-SIZE) CARD-TOP TV:ALU-IOR T CARD-WINDOW))

(DEFUN DRAW-LARGE-NUMBERS ()
  (DO ((CHAR #/0 (1+ CHAR))
       (Y (+ CARD-TOP CARD-TOP-HEIGHT CARD-NUMBER-TOP-HEIGHT) (+ Y CARD-NUMBER-HEIGHT)))
      ((> CHAR #/9))
    (DO ((I 1. (1+ I))
         (X (+ CARD-MARGIN 2) (+ X 9)))
        ((> I 80.))
      (SYS:%DRAW-CHAR FONTS:TVFONT CHAR X Y TV:ALU-IOR CARD-WINDOW))))

(DEFUN DRAW-SMALL-NUMBERS (&AUX Y)
  (SETQ Y (+ CARD-TOP CARD-TOP-HEIGHT CARD-NUMBER-TOP-HEIGHT (TRUNCATE CARD-NUMBER-HEIGHT 2)))
  (DRAW-SMALL-NUMBERS-1 Y)
  (SETQ Y (+ Y (* CARD-NUMBER-HEIGHT 9)))
  (DRAW-SMALL-NUMBERS-1 Y))

(DEFUN DRAW-SMALL-NUMBERS-1 (Y)
  (DO ((I 1. (1+ I))
       (X (+ CARD-MARGIN 2) (+ X 9)))
      ((> I 80.))
    (DRAW-SMALL-NUMBER I X Y)))

(DEFUN DRAW-SMALL-NUMBER (I X Y)
    (IF (< I 10.)
        (SYS:%DRAW-CHAR FONTS:TINY (+ I #/0) (1+ X) Y TV:ALU-IOR CARD-WINDOW)
        (SYS:%DRAW-CHAR FONTS:TINY (+ (TRUNCATE I 10.) #/0) (1- X) Y TV:ALU-IOR CARD-WINDOW)
        (SYS:%DRAW-CHAR FONTS:TINY (+ (\ I 10.) #/0) (+ X 3) Y
                        TV:ALU-IOR CARD-WINDOW)))

(DEFUN DRAW-LOGO (&AUX X Y)
  (SETQ X (+ CARD-MARGIN 2 (* 9 6))
        Y (+ CARD-TOP CARD-TOP-HEIGHT CARD-NUMBER-TOP-HEIGHT (* CARD-NUMBER-HEIGHT 10.)))
  (DRAW-TINY "IBM" X Y)
  (SETQ X (+ X (* 9 2)))
  (LET ((X0 (1- X)) (Y0 (1- Y))
        (X1 (+ X 15.)) (Y1 (+ Y 7)))
    (SYS:%DRAW-LINE X0 Y0 X1 Y0 TV:ALU-IOR T CARD-WINDOW)
    (SYS:%DRAW-LINE X1 Y0 X1 Y1 TV:ALU-IOR T CARD-WINDOW)
    (SYS:%DRAW-LINE X1 Y1 X0 Y1 TV:ALU-IOR T CARD-WINDOW)
    (SYS:%DRAW-LINE X0 Y1 X0 Y0 TV:ALU-IOR T CARD-WINDOW))
  (DRAW-TINY "5081" X Y))

(DEFUN DRAW-TINY (STR X Y)
  (DO ((I 0 (1+ I))
       (LEN (ARRAY-ACTIVE-LENGTH STR))
       (X X (+ X 4)))
      (( I LEN))
    (SYS:%DRAW-CHAR FONTS:TINY (AREF STR I) X Y TV:ALU-IOR CARD-WINDOW)))

(DEFUN DRAW-IT ()
  (TV:PREPARE-SHEET (CARD-WINDOW)
    (DRAW-CARD)
    (DRAW-LARGE-NUMBERS)
    (DRAW-SMALL-NUMBERS)
    (DRAW-LOGO)))

(DEFUN DRAW-CARD-CHAR (CHAR CHAR-X &AUX X BITS)
  (TV:PREPARE-SHEET (CARD-WINDOW)
    (SETQ X (+ CARD-MARGIN 2 (* CHAR-X 9)))
    (SYS:%DRAW-CHAR FONTS:TVFONT CHAR X
                    (+ CARD-TOP CARD-LETTER-TOP) TV:ALU-IOR CARD-WINDOW)
    (AND (= (SETQ BITS (AREF CARD-PUNCH-ARRAY CHAR)) 177777)
         (FERROR NIL "Attempt to punch ~C" CHAR))
    (DO ((BITS BITS (LSH BITS -1))
         (Y (+ CARD-TOP CARD-TOP-HEIGHT CARD-NUMBER-TOP-HEIGHT (* CARD-NUMBER-HEIGHT 9))
            (- Y CARD-NUMBER-HEIGHT)))
        ((ZEROP BITS))
      (AND (BIT-TEST 1 BITS)
           (SYS:%DRAW-RECTANGLE 6 (- CARD-NUMBER-HEIGHT 3) X Y
                                TV:ALU-IOR CARD-WINDOW)))))

(DEFUN ERASE-CARD-CHAR (CHAR CHAR-X &AUX X BITS)
  (TV:PREPARE-SHEET (CARD-WINDOW)
    (SETQ X (+ CARD-MARGIN 2 (* CHAR-X 9)))
    (SYS:%DRAW-RECTANGLE 5 10. X (+ CARD-TOP CARD-LETTER-TOP)
                         TV:ALU-ANDCA CARD-WINDOW)
    (AND (= (SETQ BITS (AREF CARD-PUNCH-ARRAY CHAR)) 177777)
         (FERROR NIL "Attempt to unpunch ~C" CHAR))
    (DO ((BITS BITS (LSH BITS -1))
         (Y (+ CARD-TOP CARD-TOP-HEIGHT CARD-NUMBER-TOP-HEIGHT (* CARD-NUMBER-HEIGHT 9))
            (- Y CARD-NUMBER-HEIGHT))
         (I 9 (1- I)))
        ((ZEROP BITS))
      (COND ((BIT-TEST 1 BITS)
             (SYS:%DRAW-RECTANGLE 6 (- CARD-NUMBER-HEIGHT 3) X Y
                                  TV:ALU-ANDCA CARD-WINDOW)
             (COND (( I 0)
                    (SYS:%DRAW-CHAR FONTS:TVFONT (+ #/0 I) X Y TV:ALU-IOR CARD-WINDOW)
                    (AND (OR (= I 0) (= I 9))
                         (DRAW-SMALL-NUMBER (1+ CHAR-X) X
                                            (+ Y (TRUNCATE CARD-NUMBER-HEIGHT 2)))))))))))

(DEFVAR CARD-PUNCH-FORMAT
        '((#\SP . ())
          (#/. . (12. 8 3))
          (#/) . (12. 8 4))
          (#/] . (12. 8 5))
          (#/< . (12. 8 6))
          (#/_ . (12. 8 7))
          (#/+ . (12.))
          (#/! . (11. 8 2))
          (#/$ . (11. 8 3))
          (#/* . (11. 8 4))
          (#/[ . (11. 8 5))
          (#/> . (11. 8 6))
          (#/& . (11. 8 7))
          (#/- . (11.))
          (#// . (0 1))
          (#/' . (0 8 3))
          (#/( . (0 8 4))
          (#/" . (0 8 5))
          (#/# . (0 8 6))
          (#/% . (0 8 7))
          (#/= . (8 3))
          (#/@ . (8 4))
          (#/^ . (8 5))
          (#/, . (8 6))
          (#/\ . (8 7))
          (#/A . (12. 1))
          (#/B . (12. 2))
          (#/C . (12. 3))
          (#/D . (12. 4))
          (#/E . (12. 5))
          (#/F . (12. 6))
          (#/G . (12. 7))
          (#/H . (12. 8))
          (#/I . (12. 9))
          (#/J . (11. 1))
          (#/K . (11. 2))
          (#/L . (11. 3))
          (#/M . (11. 4))
          (#/N . (11. 5))
          (#/O . (11. 6))
          (#/P . (11. 7))
          (#/Q . (11. 8))
          (#/R . (11. 9))
          (#/; . (0 8 2))
          (#/S . (0 2))
          (#/T . (0 3))
          (#/U . (0 4))
          (#/V . (0 5))
          (#/W . (0 6))
          (#/X . (0 7))
          (#/Y . (0 8))
          (#/Z . (0 9))
          (#/0 . (0))
          (#/1 . (1))
          (#/2 . (2))
          (#/3 . (3))
          (#/4 . (4))
          (#/5 . (5))
          (#/6 . (6))
          (#/7 . (7))
          (#/8 . (8))
          (#/9 . (9))))

(DEFUN INITIALIZE-CARD-PUNCH-ARRAY ()
  (OR (BOUNDP 'CARD-PUNCH-ARRAY)
      (SETQ CARD-PUNCH-ARRAY (MAKE-ARRAY 200 ':TYPE 'ART-16B)))
  (FILLARRAY CARD-PUNCH-ARRAY '(-1))
  (DOLIST (LIST CARD-PUNCH-FORMAT)
    (ASET (DO ((PUNCHES (CDR LIST) (CDR PUNCHES))
               (PUNCH)
               (VALUE 0))
              ((NULL PUNCHES) VALUE)
            (SETQ PUNCH (CAR PUNCHES))
            (AND (< PUNCH 10.) (SETQ PUNCH (- 10. PUNCH)))
            (SETQ VALUE (LOGIOR VALUE (LSH 1 (1- PUNCH)))))
          CARD-PUNCH-ARRAY (CAR LIST))))

(OR (BOUNDP 'CARD-PUNCH-ARRAY) (INITIALIZE-CARD-PUNCH-ARRAY))

;;; Now the top level editor
(DEFUN CARD-EDITOR ()
  (SETUP-CARD-WINDOW)
  (TV:WINDOW-CALL (CARD-WINDOW :DEACTIVATE)
    (SEND CARD-WINDOW :CLEAR-WINDOW)
    (CARD-EDITOR-INTERNAL)))

(DEFUN SETUP-CARD-WINDOW ()
  (OR (BOUNDP 'CARD-WINDOW)
      (SETQ CARD-WINDOW (MAKE-INSTANCE 'TV:WINDOW
                                       :LABEL NIL
                                       :SUPERIOR TV:MOUSE-SHEET
                                       :TOP CARD-WINDOW-TOP
                                       :BOTTOM CARD-WINDOW-BOTTOM))))

(DEFUN CARD-EDITOR-INTERNAL ()
  (OR (BOUNDP 'CARD-IMAGE)
      (SETQ CARD-IMAGE (MAKE-ARRAY 80.
                                   ':TYPE 'ART-STRING
                                   ':LEADER-LENGTH 1)))
  (STORE-ARRAY-LEADER 0 CARD-IMAGE 0)
  (DRAW-IT)
  (*CATCH 'RETURN-FROM-EDITOR
    (DO () (NIL)
      (*CATCH 'ABORT-COMMAND
        (PROG (CH)
          (TV:SHEET-SET-CURSORPOS CARD-WINDOW
                                  (+ CARD-MARGIN 2 (* (ARRAY-LEADER CARD-IMAGE 0) 9))
                                  (+ CARD-TOP CARD-LETTER-TOP))
          (SETQ CH (CHAR-UPCASE (FUNCALL CARD-WINDOW ':TYI)))
          (SELECTQ CH
            (#\CR
             (*THROW 'RETURN-FROM-EDITOR T))
            (#\RUBOUT
             (LET ((I (1- (ARRAY-LEADER CARD-IMAGE 0))))
               (COND ((MINUSP I)
                      (BEEP)
                      (*THROW 'ABORT-COMMAND T)))
               (ERASE-CARD-CHAR (AREF CARD-IMAGE I) I)
               (STORE-ARRAY-LEADER I CARD-IMAGE 0)))
            (#\CLEAR
             (DO ((I (1- (ARRAY-LEADER CARD-IMAGE 0)) (1- I)))
                 ((MINUSP I))
               (ERASE-CARD-CHAR (AREF CARD-IMAGE I) I))
             (STORE-ARRAY-LEADER 0 CARD-IMAGE 0))
            (OTHERWISE
             (COND ((OR ( CH 200)
                        (= (AREF CARD-PUNCH-ARRAY CH) 177777)
                        (NOT (ARRAY-PUSH CARD-IMAGE CH)))
                    (BEEP)
                    (*THROW 'ABORT-COMMAND T)))
             (DRAW-CARD-CHAR CH (1- (ARRAY-LEADER CARD-IMAGE 0)))))))))
  (TV:SHEET-SET-CURSORPOS CARD-WINDOW 0
                          (+ CARD-TOP-HEIGHT CARD-BOTTOM-HEIGHT CARD-TOP))
  CARD-IMAGE)

(DEFDEMO "Hollerith Editor" "Upward compatibilty with primitive computers." (CARD-EDITOR))
(DEFDEMO "Multiple Hollerith Editor"
         "Upward compatibilty with primitive computers."
  (EDIT-MULTIPLE-CARDS))

(DEFCONST CARD-Y-OFFSET -20)
(DEFCONST CARD-X-OFFSET -5)
(DEFUN EDIT-MULTIPLE-CARDS ()
  (SETUP-CARD-WINDOW)
  (TV:WINDOW-CALL (CARD-WINDOW :DEACTIVATE)
    (SEND CARD-WINDOW :CLEAR-WINDOW)
    (DO () (())
      (CARD-EDITOR-INTERNAL)
      (LET ((X-WID (- (TV:SHEET-INSIDE-WIDTH CARD-WINDOW) (ABS CARD-X-OFFSET)))
            (Y-WID (- (TV:SHEET-INSIDE-HEIGHT CARD-WINDOW) (ABS CARD-Y-OFFSET))))
        (SEND CARD-WINDOW
              :BITBLT-WITHIN-SHEET TV:ALU-SETA
                                   (IF (< CARD-X-OFFSET 0) X-WID (- X-WID))
                                   (IF (< CARD-Y-OFFSET 0) Y-WID (- Y-WID))
                                   (IF (< CARD-X-OFFSET 0) (- CARD-X-OFFSET) 0)
                                   (IF (< CARD-Y-OFFSET 0) (- CARD-Y-OFFSET) 0)
                                   (IF (< CARD-X-OFFSET 0) 0 CARD-X-OFFSET)
                                   (IF (< CARD-Y-OFFSET 0) 0 CARD-Y-OFFSET))
        (SEND CARD-WINDOW :DRAW-RECTANGLE
                          (ABS CARD-X-OFFSET) (TV:SHEET-INSIDE-HEIGHT CARD-WINDOW)
                          (IF (< CARD-X-OFFSET 0)
                              (+ (TV:SHEET-INSIDE-WIDTH CARD-WINDOW) CARD-X-OFFSET)
                            0)
                          0
                          TV:ALU-ANDCA)
        (SEND CARD-WINDOW :DRAW-RECTANGLE
                          (TV:SHEET-INSIDE-WIDTH CARD-WINDOW) (ABS CARD-Y-OFFSET)
                          0
                          (IF (< CARD-Y-OFFSET 0)
                              (+ (TV:SHEET-INSIDE-WIDTH CARD-WINDOW) CARD-Y-OFFSET)
                            0)
                          TV:ALU-ANDCA)))))

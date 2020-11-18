;; -*-Package:HACKS; Mode:LISP; Base: 8-*-

(DEFFLAVOR FOM-MIXIN () ()
  (:INCLUDED-FLAVORS TV:SHEET))

(DEFVAR FOM-ARRAY (MAKE-PIXEL-ARRAY 1100 706 ':TYPE 'ART-4B))

(DEFUN READ-FOM (&OPTIONAL (FILE-NAME "SYS:DEMO;RISING SUN >"))
  (WITH-OPEN-FILE (FILE FILE-NAME '(READ FIXNUM))
    (LOOP FOR X FROM 0 TO 600
          DO (LOOP FOR Y FROM 519. DOWNTO -120. BY 2
                   DO (LET ((WORD (FUNCALL FILE 'TYI)))
                        (COND ((AND (< Y 453.) ( Y 0))
                               (AS-2-REVERSE (LDB 1404 WORD) FOM-ARRAY X Y)
                               (AS-2-REVERSE (LDB 0404 WORD) FOM-ARRAY X (1+ Y)))))))))

(DEFFLAVOR FOM () (FOM-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST :LABEL NIL :BLINKER-P NIL :SAVE-BITS T))

(DEFMETHOD (FOM :NAME-FOR-SELECTION) () NIL)

(DEFMETHOD (FOM :AFTER :REFRESH) (&OPTIONAL TYPE)
  (AND (OR (NOT TV:RESTORED-BITS-P) (EQ TYPE ':SIZE-CHANGED))
       (TV:SHEET-FORCE-ACCESS (SELF)
         (FUNCALL-SELF ':BITBLT TV:ALU-SETA (TV:SHEET-INSIDE-WIDTH) (TV:SHEET-INSIDE-HEIGHT)
                       FOM-ARRAY (IF (> (TV:SHEET-INSIDE-WIDTH) 600) 0
                                     (// (- 600 (TV:SHEET-INSIDE-WIDTH)) 2))
                                 (// 706 (TV:SHEET-INSIDE-HEIGHT))
                       0 0))))

(DEFVAR FOM-WINDOW (TV:MAKE-WINDOW 'FOM ':SUPERIOR COLOR:COLOR-SCREEN))

(DEFUN SETUP-FOM ()
  (COLOR:GRAY-COLOR-MAP)
  (TV:WITH-SHEET-DEEXPOSED (FOM-WINDOW)
    (TV:SHEET-FORCE-ACCESS (FOM-WINDOW)
      (FUNCALL FOM-WINDOW ':REFRESH)))
  (FUNCALL FOM-WINDOW ':EXPOSE))

;;; Lunar turkey (actually, any window)
(DEFVAR RECT-HEIGHT)
(DEFVAR RECT-WIDTH)
(DEFVAR TURKEY-WINDOW)
(DEFVAR HOLE-X)
(DEFVAR HOLE-Y)
(DEFUN TURKEY (&OPTIONAL (WINDOW FOM-WINDOW) (NUMBER-HORIZONTAL 4) (NUMBER-VERTICAL 4))
  (LET ((RECT-HEIGHT (// (TV:SHEET-INSIDE-HEIGHT WINDOW) NUMBER-VERTICAL))
        (RECT-WIDTH (// (TV:SHEET-INSIDE-WIDTH WINDOW) NUMBER-HORIZONTAL))
        (HOLE-X 0) (HOLE-Y 0)
        (TURKEY-WINDOW WINDOW))
    (FUNCALL WINDOW ':DRAW-RECTANGLE (- (TV:SHEET-INSIDE-WIDTH WINDOW)
                                        (* RECT-WIDTH NUMBER-HORIZONTAL))
             (TV:SHEET-INSIDE-HEIGHT WINDOW)
             (* RECT-WIDTH NUMBER-HORIZONTAL) 0
             (TV:SHEET-ERASE-ALUF WINDOW))
    (FUNCALL WINDOW ':DRAW-RECTANGLE (TV:SHEET-INSIDE-WIDTH WINDOW)
             (- (TV:SHEET-INSIDE-HEIGHT WINDOW)
                (* RECT-HEIGHT NUMBER-VERTICAL))
             0 (* RECT-HEIGHT NUMBER-VERTICAL)
             (TV:SHEET-ERASE-ALUF WINDOW))
    (ERASE-SQUARE 0 0)
    (LET ((LAST-MOVE -1))
      (DOTIMES (I (+ 30 (RANDOM 20)))
        (DO ((X HOLE-X HOLE-X)
             (Y HOLE-Y HOLE-Y)
             (MOVE))
            (())
          (SELECTQ (SETQ MOVE (RANDOM 4))
            (0 (SETQ X (1+ X)))
            (1 (SETQ Y (1+ Y)))
            (2 (SETQ X (1- X)))
            (3 (SETQ Y (1- Y))))
          (COND ((AND ( X 0) ( Y 0) (< X NUMBER-HORIZONTAL) (< Y NUMBER-VERTICAL)
                      ( MOVE
                         (CDR (ASSQ LAST-MOVE '((-1 . -1) (0 . 2) (1 . 3) (2 . 0) (3 . 1))))))
                 (MOVE-SQUARE X Y HOLE-X HOLE-Y)
                 (SETQ HOLE-X X
                       HOLE-Y Y)
                 (SETQ LAST-MOVE MOVE)
                 (RETURN))))))
    (PLAY-GAME)))

(DEFUN ERASE-SQUARE (SX SY)
  (FUNCALL TURKEY-WINDOW ':DRAW-RECTANGLE RECT-WIDTH RECT-HEIGHT
           (* RECT-WIDTH SX) (* RECT-HEIGHT SY)
           (TV:SHEET-CHAR-ALUF TURKEY-WINDOW)))

(DEFCONST TURKEY-NUMBER-OF-MOVES 10.)
(DEFCONST TURKEY-SLEEP 0)
(DEFUN MOVE-SQUARE (FX FY TX TY)
  (SETQ FX (* FX RECT-WIDTH)
        FY (* FY RECT-HEIGHT)
        TX (* TX RECT-WIDTH)
        TY (* TY RECT-HEIGHT))
  (LET ((DX (// (- TX FX) TURKEY-NUMBER-OF-MOVES))
        (DY (// (- TY FY) TURKEY-NUMBER-OF-MOVES)))
    (DO ((X FX NX)
         (Y FY NY)
         (NX) (NY)
         (I (1- TURKEY-NUMBER-OF-MOVES) (1- I)))
        ((< I 0))
      (FUNCALL TURKEY-WINDOW ':BITBLT-WITHIN-SHEET TV:ALU-SETA
               (IF (< DX 0) RECT-WIDTH (- RECT-WIDTH))
               (IF (< DY 0) RECT-HEIGHT (- RECT-HEIGHT))
               X Y
               (SETQ NX (IF (= I 0) TX (+ X DX)))
               (SETQ NY (IF (= I 0) TY (+ Y DY))))
      (FUNCALL TURKEY-WINDOW ':DRAW-RECTANGLE
               (IF (= DX 0) RECT-WIDTH (ABS (- NX X)))
               (IF (= DY 0) RECT-HEIGHT (ABS (- NY Y)))
               (COND ((= DX 0) X)
                     ((< DX 0) (+ NX RECT-WIDTH))
                     (T X))
               (COND ((= DY 0) Y)
                     ((< DY 0) (+ NY RECT-HEIGHT))
                     (T Y))
               (TV:SHEET-CHAR-ALUF TURKEY-WINDOW))
      (PROCESS-SLEEP TURKEY-SLEEP))))

(DEFUN PLAY-GAME (&AUX (OLD-MOUSE-SHEET TV:MOUSE-SHEET))
  (UNWIND-PROTECT
    (PROGN
      (TV:MOUSE-SET-SHEET TURKEY-WINDOW)
      (TV:WITH-MOUSE-GRABBED
        (DO () (())
          (PROCESS-WAIT "Turkey Mouse" #'(LAMBDA () ( TV:MOUSE-LAST-BUTTONS 0)))
          (AND (BIT-TEST TV:MOUSE-LAST-BUTTONS 4) (RETURN))
          (LET ((SX (// (- TV:MOUSE-X (TV:SHEET-INSIDE-LEFT TURKEY-WINDOW)) RECT-WIDTH))
                (SY (// (- TV:MOUSE-Y (TV:SHEET-INSIDE-TOP TURKEY-WINDOW)) RECT-HEIGHT)))
            (COND ((OR (AND (= SX HOLE-X) (= 1 (ABS (- SY HOLE-Y))))
                       (AND (= SY HOLE-Y) (= 1 (ABS (- SX HOLE-X)))))
                   (MOVE-SQUARE SX SY HOLE-X HOLE-Y)
                   (SETQ HOLE-X SX
                         HOLE-Y SY))
                  (T (TV:BEEP))))
          (PROCESS-WAIT "Turkey off the mouse" #'(LAMBDA () (ZEROP TV:MOUSE-LAST-BUTTONS))))))
    (TV:MOUSE-SET-SHEET OLD-MOUSE-SHEET)))

;;; -*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

(DEFVAR *WORM-WINDOW* nil)
(defun worm-window ()
  (cond (*worm-window*)
        (t (setq *worm-window* (MAKE-INSTANCE 'HOF-WINDOW :BLINKER-P NIL)))))

(DEFVAR *CHAR*)
(defvar *bits*)
(defvar *order*)
(defvar *worm-x*)
(defvar *worm-y*)
(defvar *x-worm*)
(defvar *y-worm*)
(defvar *WORM-ALU-FUNCTION*)
(defvar *dir*)
(defvar *fitp*)


;These are the "character codes" in the worm font
;for the various kinds of spots.
(DEFCONST WORM-BIG-CHAR 5)
(DEFCONST WORM-STRIPE-CHAR 4)
(DEFCONST WORM-GRAY-CHAR 3)
(DEFCONST WORM-BLACK-CHAR 2)

;This is a 6-long array of stack groups,
;each of which runs one worm.
(DEFVAR *WORMS*
        (LET ((ARRAY (MAKE-ARRAY 6 :TYPE 'ART-Q-LIST)))
          (DOTIMES (I 6)
            (SETF (AREF ARRAY I) (MAKE-STACK-GROUP (FORMAT NIL "WORM-~D" I))))
          ARRAY))

(DEFUN PRESET (SG CHAR ALU-FN N)
       (STACK-GROUP-PRESET SG
                           #'FLOP
                           (SYMBOL-VALUE CHAR)
                           (SYMBOL-VALUE ALU-FN)
                           *ORDER*
                           *WORM-X*
                           *WORM-Y*
                           *BITS*
                           (* N (^ 3 (1- *ORDER*)))
                           *TERMINAL-IO*
                           SYS:%CURRENT-STACK-GROUP))

(DEFUN WORM (&OPTIONAL (*BITS* 0) (*ORDER* 7) (*WORM-X* 222) (*WORM-Y* 777)
             &AUX LENGTH (*TERMINAL-IO* (worm-window)))
  (TV:WINDOW-CALL (*WORM-WINDOW* :DEACTIVATE)
    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit WORM.")
      (SEND *WORM-WINDOW* :SET-LABEL "Worm")
      (OR (BOUNDP 'FONTS:WORM) (LOAD "SYS: FONTS ;WORM" :PACKAGE 'FONTS))
      (SETQ LENGTH (^ 3 (1+ *ORDER*)))
      (PRESET (AREF *WORMS* 0) 'WORM-BIG-CHAR 'TV:ALU-IOR 0)
      (SETQ *FITP* NIL)
      (DO ((I 0 (1+ I)))
          ((OR *FITP* ( I 2)))                 ; Paint blackness over whole worm
        (SETQ *FITP* T)
        (SEND *TERMINAL-IO* :CLEAR-WINDOW)
        (DOTIMES (I (1+ LENGTH))
          (IF (FUNCALL (AREF *WORMS* 0))
              (signal eh:abort-object))))
      (SETQ *WORM-X* *X-WORM* *WORM-Y* *Y-WORM*)
      (MAPC #'PRESET                    ; Preset wormlets
            (G-L-P *WORMS*)
            '(WORM-GRAY-CHAR WORM-BLACK-CHAR WORM-STRIPE-CHAR WORM-BLACK-CHAR
                             WORM-BLACK-CHAR WORM-BLACK-CHAR)
            '(TV:ALU-XOR TV:ALU-ANDCA TV:ALU-IOR TV:ALU-IOR TV:ALU-ANDCA TV:ALU-IOR)
            '(0 1 2 3 5 6))                     ;4 is intentionally missing!
      (WORM-COMMAND-LOOP))))

(DEFUN WORM-COMMAND-LOOP (&AUX (YPOS (- (TV:SHEET-INSIDE-HEIGHT *TERMINAL-IO*)
                                        (* 3 (TV:SHEET-LINE-HEIGHT *TERMINAL-IO*)))))
  (DO ((I 0 (1+ I))
       RUN
       (STOP-VAL 0))
      (NIL)
    (COND ((OR RUN (< I STOP-VAL))
           (AND (SEND *TERMINAL-IO* :TYI-NO-HANG)
                (SETQ RUN NIL STOP-VAL 0)))
          (T (SEND *WORM-WINDOW* :SET-LABEL (LET ((*PRINT-BASE* 9))
                                              (FORMAT NIL "Worm    Generation ~S (base 9)" I)))
             (SEND *TERMINAL-IO* :SET-CURSORPOS 0 YPOS)
             (FORMAT *TERMINAL-IO* "    P: Run  nR: Run until n  nN: Run n steps  nS: Run till nth order  Abort: exit~%    ")
             (PROG (CH VAL)
                   (SETQ VAL 0)
                LOOP (SETQ CH (CHAR-UPCASE (TYI)))
                   (COND ((AND ( CH #/0)( CH #/9))
                          (SETQ VAL (+ (* VAL 9) (- CH #/0)))
                          (GO LOOP))
                         ((EQ CH #/N) (SETQ STOP-VAL (+ VAL I)))
                         ((EQ CH #/R) (SETQ STOP-VAL VAL))
                         ((EQ CH #/S) (SETQ VAL (^ 3 VAL)
                                            STOP-VAL (* VAL (1+ (TRUNCATE I VAL)))))
                         ((EQ CH #/P) (SETQ RUN T))))
             (IF (OR RUN (< I STOP-VAL))
                 (SEND *WORM-WINDOW* :SET-LABEL "Worm    Type any character to stop"))))
    (DOTIMES (I 6)
      (IF (FUNCALL (AREF *WORMS* I))
          (SIGNAL EH:ABORT-OBJECT)))))

(DEFVAR *WORM-TURNS* (MAKE-ARRAY 12. :TYPE 'ART-Q-LIST
                                     :initial-contents '(6   0
                                                         3   5
                                                         -3  5
                                                         -6  0
                                                         -3 -5
                                                         3  -5)))

;This is the coroutining function for each worm.
;Returns to the calling stack group with NIL normally,
;T if the user typed Abort.
(DEFUN FLOP (*CHAR* *WORM-ALU-FUNCTION* *ORDER* *WORM-X* *WORM-Y* *BITS* SNOOZE *TERMINAL-IO* IGNORE)
  (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to Worm command level.")
    (DO ((I 0 (1+ I)))
        (( I SNOOZE))
      (STACK-GROUP-RETURN NIL))
    (DO ((I 0 (1+ I))
         (*DIR* (BOOLE 4 (- *ORDER*) 1)))
        (NIL)
      (TERD *ORDER* *BITS*)
      (WORM-STEP)
      (SETQ *X-WORM* *WORM-X* *Y-WORM* *WORM-Y*)
      (SETQ *DIR* (+ *DIR* 4))))
  (DO () (()) (STACK-GROUP-RETURN T)))

(DEFUN TERD (N *BITS*)
   (IF (PLUSP N)
          (COND ((Bit-Test *BITS* 1)
                   (TERD (1- N) (LSH *BITS* -1))
                   (SETQ *DIR* (- *DIR* -2))
                   (WORM-STEP)
                   (SETQ *DIR* (- *DIR* 4))
                   (TERD (1- N) (LSH *BITS* -1))
                   (WORM-STEP)
                   (SETQ *DIR* (+ *DIR* 2))
                   (TERD (1- N) (LSH *BITS* -1)))
                (T (TERD (1- N) (LSH *BITS* -1))
                   (WORM-STEP)
                   (SETQ *DIR* (+ *DIR* 4))
                   (TERD (1- N) (LSH *BITS* -1))
                   (SETQ *DIR* (- *DIR* 2))
                   (WORM-STEP)
                   (SETQ *DIR* (- *DIR* 2))
                   (TERD (1- N) (LSH *BITS* -1))))))

(DEFUN WORM-STEP ()
   (CLIP '*WORM-X* (TV:SHEET-INSIDE-WIDTH *TERMINAL-IO*) (SETQ *DIR* (\ (+ 12. *DIR*) 12.)))
   (CLIP '*WORM-Y* (- (TV:SHEET-INSIDE-HEIGHT *TERMINAL-IO*) 55) (1+ *DIR*))
   (TV:PREPARE-SHEET (*TERMINAL-IO*)
      (TV:%DRAW-CHAR FONTS:WORM *CHAR* *WORM-X* *WORM-Y* *WORM-ALU-FUNCTION* *TERMINAL-IO*))
   (STACK-GROUP-RETURN NIL))

(DEFUN CLIP (Z N D)
  (CASE (TRUNCATE (+ N (SET Z (+ (SYMBOL-VALUE Z) (AREF *WORM-TURNS* D))))
                  N)
    (0 (SET Z 1) (SETQ *FITP* NIL))
    (1)
    (2 (SET Z (1- N)) (SETQ *FITP* NIL))))

(DEFDEMO "Worm" "Pretty fractal patters, by Gosper and Holloway." (WORM))

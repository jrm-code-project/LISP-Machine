;;; Mouse commands for ZWEI -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; Note: some screen system primitives live in SCREEN

;;;  [1] Mark some characters.
;;;  [11] No region -> Select window, Region -> You are moving it.
;;;  [2] Mark some things.
;;;  [22] Kill, Yank, Yank-pop
;;;  [3] Put ZWEI menu here.
;;;  [33] Call system menu

(DEFVAR *MOUSE-P* :UNBOUND "T if the mouse is inside any ZWEI window.")
(DEFVAR *MOUSE-BLINKER* :UNBOUND "Blinker to use for showing what character the mouse points at.
Normally this is the same as *MOUSE-BOX-BLINKER*.
Copying *MOUSE-CHAR-BLINKER* into this also works;
then the character being pointed at flashes rapidly.")

(DEFVAR *MOUSE-HOOK* NIL
  "If non-NIL, function to handle mouse clicks, overriding the comtab.")

(DEFVAR *MOUSE-CHAR-BLINKER* :UNBOUND
  "A blinker which, if put into *MOUSE-BLINKER*, blinks the character the mouse is pointing at.")

(DEFVAR *MOUSE-BOX-BLINKER* :UNBOUND
  "A blinker which, if put into *MOUSE-BLINKER*, puts a box around the character the mouse is pointing at.")

(DEFVAR *GLOBAL-MOUSE-CHAR-BLINKER* :UNBOUND
  "Blinker used to make boxes around symbols being pointed at.")

(DEFVAR *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER* :UNBOUND
  "Function called in deciding where to put the box that goes around a symbol being pointed at.")

(DEFVAR *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING* :UNBOUND
  "If non-NIL, a string to override the who-line documentation string.")

;;; Called by the editor to initialize the mouse
(DEFUN INITIALIZE-MOUSE (&AUX (INHIBIT-SCHEDULING-FLAG T))
  (AND (BOUNDP '*MOUSE-CHAR-BLINKER*)
       (TV:OPEN-BLINKER *MOUSE-CHAR-BLINKER*))
  (SETQ *MOUSE-P* NIL
        *MOUSE-CHAR-BLINKER* (TV:MAKE-BLINKER TV:MOUSE-SHEET 'TV:CHARACTER-BLINKER
                                              ':VISIBILITY NIL
                                              ':HALF-PERIOD 4
                                              ':FONT TV:(SCREEN-DEFAULT-FONT DEFAULT-SCREEN)
                                              ':CHAR #/?)
        *MOUSE-BOX-BLINKER* (TV:MAKE-BLINKER TV:MOUSE-SHEET 'TV:HOLLOW-RECTANGULAR-BLINKER
                                             ':VISIBILITY NIL)
        *MOUSE-BLINKER* *MOUSE-BOX-BLINKER*
        *GLOBAL-MOUSE-CHAR-BLINKER* (TV:MAKE-BLINKER TV:MOUSE-SHEET
                                                     'TV:HOLLOW-RECTANGULAR-BLINKER
                                                     ':VISIBILITY NIL
                                                     ':HALF-PERIOD 4)
        *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER* NIL
        *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING* NIL))

;;;Wait for the mouse to do something, return non-nil if released buttons or left window
;;;LAST-X, LAST-Y are relative to the inside of the window (like *MOUSE-X*, *MOUSE-Y*).
(DEFUN WAIT-FOR-MOUSE (LAST-X LAST-Y &OPTIONAL MAX-SPEED)
  "Wait for the mouse to move from LAST-X, LAST-Y, or for all buttons to be released.
If MAX-SPEED is supplied, we do not return until mouse slows to that speed.
Returns NIL if all buttons released or the mouse has left the window.
LAST-X and LAST-Y are relative to inside the window margins."
  (LET ((SHEET (WINDOW-SHEET *WINDOW*)))
    (MULTIPLE-VALUE-BIND (XOFF YOFF) (TV:SHEET-CALCULATE-OFFSETS SHEET TV:MOUSE-SHEET)
      (PROCESS-WAIT "MOUSE"
                    #'(LAMBDA (LX LY MS)
                        (OR (AND (OR ( TV:MOUSE-X LX) ( TV:MOUSE-Y LY))
                                 (OR (NULL MS) ( TV:MOUSE-SPEED MS)))
                            (ZEROP (TV:MOUSE-BUTTONS T))
                            (NOT *MOUSE-P*)))
                    (+ LAST-X (TV:SHEET-INSIDE-LEFT SHEET) XOFF)
                    (+ LAST-Y (TV:SHEET-INSIDE-TOP SHEET) YOFF)
                    MAX-SPEED)))
  (AND (NOT (ZEROP (TV:MOUSE-BUTTONS T))) *MOUSE-P*))

(DEFUN MOUSE-POSITION (&OPTIONAL (WINDOW *WINDOW*))
  "Return the X and Y position of the mouse in window-defstruct WINDOW.
Returns NIL if the mouse is not in that window.
The values are relative to the margins of the window."
  (LET ((SHEET (WINDOW-SHEET WINDOW)))
    (MULTIPLE-VALUE-BIND (XOFF YOFF) (TV:SHEET-CALCULATE-OFFSETS SHEET TV:MOUSE-SHEET)
      (VALUES (- TV:MOUSE-X XOFF (TV:SHEET-INSIDE-LEFT SHEET))
              (- TV:MOUSE-Y YOFF (TV:SHEET-INSIDE-TOP SHEET))))))

;;; Call MOUSE-CHAR so we can be sure that the BP points the thing that's blinking
;;; If X and Y are supplied, they are the coordinates to use, otherwise
;;; we use the coordinates of where the mouse is now.
(DEFUN MOUSE-BP (WINDOW &OPTIONAL X Y &AUX CHAR LINE CHAR-POS)
  "Returns a BP to the character the mouse is pointing at, in window-defstruct WINDOW.
Returns NIL if the mouse is not in that window or not pointing at text.
X and Y, if non-NIL, are used instead of the mouse position.
They should be relative to the window margins."
  (MULTIPLE-VALUE (CHAR X Y LINE CHAR-POS)
    (MOUSE-CHAR WINDOW T X Y))
  (COND ((NULL CHAR)      ;Couldn't anything, use end of buffer for want of anything better
         (COPY-BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW))))
        (T
         (CREATE-BP LINE CHAR-POS))))

;;; The mouse must be in the selected window's area of the screen
;;; Returns the character at which the mouse points, and the X and Y positions
;;; of that character relative to its sheet.  If the mouse is not at a character,
;;; returns NIL.
(DEFUN MOUSE-CHAR (WINDOW &OPTIONAL FIXUP-P X Y)
  "Return information on where the mouse points, in the window-defstruct WINDOW.
Uses X and Y if specified, or else the mouse position.  X, Y are relative to the margins!
All values are NIL if the mouse is not in the window or not pointing at text.
Otherwise, the values are CHAR, X, Y, LINE, INDEX, WIDTH.
 CHAR is the character code for the character pointed at.
 X is the X position within the window margins of the left edge of the character.
 Y is the Y position within the window margins of the top edge of the character.
 LINE is the line the character is in.  INDEX is the position in that line.
 WIDTH is the width in pixels of the character."
  (DECLARE (VALUES CHAR X Y LINE INDEX WIDTH))
  (PROG (SHEET LINE PLINE CHAR-POS LH REAL-PLINE START END)
    (SETQ SHEET (WINDOW-SHEET WINDOW))
    (COND ((NULL Y)
           (MULTIPLE-VALUE (X Y) (MOUSE-POSITION WINDOW))))
    (SETQ LH (TV:SHEET-LINE-HEIGHT SHEET)
          PLINE (SETQ REAL-PLINE (FLOOR Y LH)))
    ;; If mouse moves to out of range, protect against error and return
    (AND (OR (MINUSP PLINE) ( PLINE (WINDOW-N-PLINES WINDOW)))
         (IF FIXUP-P
             (SETQ PLINE (MAX 0 (MIN PLINE (1- (WINDOW-N-PLINES WINDOW)))))
             (RETURN NIL)))
    (DO NIL ((SETQ LINE (PLINE-LINE WINDOW PLINE)))
      (AND (ZEROP PLINE) (RETURN NIL))
      (SETQ PLINE (1- PLINE)))
    (OR LINE (RETURN NIL))
    (SETQ START (PLINE-FROM-INDEX WINDOW PLINE))
    ;; I can't see why this could happen, but it did, so avoid blowing out.
    (OR START (RETURN NIL))
    (WHEN (WINDOW-INTERVAL WINDOW)              ;for robustness...
      (LET ((BP (INTERVAL-FIRST-BP (WINDOW-INTERVAL WINDOW))))
        (AND (EQ LINE (BP-LINE BP)) (SETQ START (MAX START (BP-INDEX BP)))))
      (LET ((BP (INTERVAL-LAST-BP (WINDOW-INTERVAL WINDOW))))
        (AND (EQ LINE (BP-LINE BP)) (SETQ END (BP-INDEX BP))))
      (MULTIPLE-VALUE (X Y CHAR-POS)            ;Find character to right of mouse
        (TV:SHEET-COMPUTE-MOTION SHEET 0 (* PLINE LH) LINE START END NIL
                                 (MAX 0 X) (* REAL-PLINE LH)))
      (IF (NULL CHAR-POS)                       ;Mouse is off end of line, pointing at the CR
          (RETURN
            (VALUES
              #/CR
              X
              Y
              LINE
              (OR END (LINE-LENGTH LINE))))
        ;; X, Y, CHAR-POS are for char to right of mouse
        ;; Find the character which is just over the mouse
        (SETQ CHAR-POS (MAX 0 (1- CHAR-POS)))
        (LET ((CHAR (IF (= CHAR-POS (LINE-LENGTH LINE)) #/CR
                      (AREF LINE CHAR-POS)))
              (FONT-MAP (TV:SHEET-FONT-MAP SHEET)))
          (LET ((CH (LDB %%CH-CHAR CHAR))
                (FONT (LDB %%CH-FONT CHAR))
                CHAR-X CHAR-WIDTH)
            (SETQ FONT (AREF FONT-MAP (IF ( FONT
                                             (ARRAY-ACTIVE-LENGTH FONT-MAP))
                                          0 FONT)))
            (IF (= CH #/TAB)
                (SETQ CHAR-X
                      (TV:SHEET-COMPUTE-MOTION SHEET 0 0 LINE START CHAR-POS)
                      CHAR-WIDTH (- X CHAR-X))
              (SETQ CHAR-WIDTH (TV:SHEET-CHARACTER-WIDTH SHEET CH FONT)
                    CHAR-X (MAX 0 (- X CHAR-WIDTH))))
            (RETURN (VALUES CHAR
                            CHAR-X
                            (+ Y (- (TV:SHEET-BASELINE SHEET) (FONT-BASELINE FONT)))
                            LINE CHAR-POS CHAR-WIDTH))))))))

(DEFUN READ-FUNCTION-NAME (PROMPT &OPTIONAL DEFAULT MUST-BE-DEFINED STRINGP
                                  &AUX EXPLICIT-PACKAGE-P
                                  (*MINI-BUFFER-DEFAULT-STRING* DEFAULT)
                                  (READ-FUNCTION-NAME-MUST-BE-DEFINED MUST-BE-DEFINED)
                                  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
                                    *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*)
                                  (READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
                                    *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*)
                                  (READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
                                    *MOUSE-FONT-CHAR*)
                                  (READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
                                    *MOUSE-X-OFFSET*)
                                  (READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET
                                    *MOUSE-Y-OFFSET*))
  "Read a function name using mini buffer or mouse.
PROMPT is a string that goes in the mode line.
DEFAULT is a function spec to return if the user types just Return.
MUST-BE-DEFINED can be T (allow only defined functions), NIL (allow anything)
 or AARRAY-OK (allow anything either defined as a function
 or known as a section by the editor).
STRINGP can be T, NIL, ALWAYS-READ or MULTIPLE-OK.
 T means if user types text, just return a string; don't try to intern it.
 ALWAYS-READ means intern the user's string afresh now;
  don't use the symbol or list recorded in the completion aarray.
 MULTIPLE-OK means it is ok to return more than one possible function
  the user could have meant, if they differ only in their package.

The first value is a list of function specs (only one, unless STRINGP is MULTIPLE-OK).
 If STRINGP is T, this is NIL.
The second value is the string the user typed, sans package prefix.
The third value is T if the user typed a package prefix."
  (DECLARE (VALUES COMPLETIONS STRING EXPLICIT-PACKAGE-P))
  (DECLARE (SPECIAL READ-FUNCTION-NAME-MUST-BE-DEFINED
                    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
                    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
                    READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
                    READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
                    READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET))
  (AND (EQ MUST-BE-DEFINED T) (SETQ STRINGP 'ALWAYS-READ))
  (SETQ PROMPT (FORMAT NIL "~A~:[:~; (Default: ~S)~]" PROMPT DEFAULT DEFAULT))
  (LET ((NAME
          (LET ((*POST-COMMAND-HOOK*
                  (APPEND *POST-COMMAND-HOOK* '(READ-FUNCTION-NAME-COMMAND-HOOK)))
                (*MINI-BUFFER-VALUE-HISTORY*
                  *DEFINITION-NAME-HISTORY*))
            (LET ((*BATCH-UNDO-SAVE* T))
              (DELETE-INTERVAL (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
            (UNWIND-PROTECT
                (PROGN (READ-FUNCTION-NAME-COMMAND-HOOK NIL)
                       (COMPLETING-READ-FROM-MINI-BUFFER PROMPT *ZMACS-COMPLETION-AARRAY*
                                                         (OR (NEQ STRINGP 'ALWAYS-READ)
                                                             'ALWAYS-STRING)))
              (READ-FUNCTION-NAME-COMMAND-HOOK T))))
        SYM ERROR-P)
    (COND ((EQUAL NAME "")
           (OR DEFAULT (BARF))
           (SETQ SYM DEFAULT
                 NAME (IF (SYMBOLP NAME) (STRING NAME)
                        (FORMAT:OUTPUT NIL (PRINC DEFAULT)))))
          ((CONSP NAME)
           (SETQ SYM (CDR NAME)
                 NAME (CAR NAME))
           (AND (CONSP SYM) (NEQ STRINGP 'MULTIPLE-OK)
                (SETQ SYM (CAR SYM))))
          ((EQ STRINGP T)                       ;If returning a string, don't intern it
           (SETQ SYM NAME))
          (T
           ;; If the string that was specified started with a package prefix,
           ;; return a flag saying so.
           ;; SYMBOL-FROM-STRING will flush the prefix from NAME.
           (LET ((NON-LETTER-INDEX
                   (STRING-SEARCH-NOT-SET " ABCDEFGHIJKLMNOPQRSTUVWXYZ-" NAME)))
             (AND NON-LETTER-INDEX (= (AREF NAME NON-LETTER-INDEX) #/:)
                  (SETQ EXPLICIT-PACKAGE-P T)))
           (let (err)
             (MULTIPLE-VALUE (SYM NAME ERROR-P err)
               (SYMBOL-FROM-STRING NAME NIL T))
             (AND (CONSP SYM) (EQ STRINGP 'MULTIPLE-OK)
                  (SETQ SYM (NCONS SYM)))
             (when ERROR-P
               (BARF (format nil "Read error~@[: ~A~]"
                             (if (errorp err) (send err :report-string))))))))
    (AND (EQ MUST-BE-DEFINED T)
         (NOT (OR (FDEFINEDP SYM)
                  (AND (SYMBOLP SYM)
                       (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST SYM)))))
         (OR (AND (SYMBOLP SYM)
                  (DOLIST (SPEC (PACKAGE-LOOKALIKE-SYMBOLS SYM))
                    (AND (FQUERY '(:SELECT T)
                                 ;; Always print prefix
                                 ;; Don't leave PACKAGE in keyword during query.
                                 (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
                                   (FORMAT NIL "Do you mean ~S? " SPEC)))
                         (RETURN (SETQ SYM SPEC)))))
             (BARF "~S is not defined" SYM)))
    (PUSH-ON-HISTORY SYM *DEFINITION-NAME-HISTORY*)
    (VALUES SYM NAME EXPLICIT-PACKAGE-P)))

(defun read-function-name-or-string (&optional reverse-p prompt comtab
                                     (default *mini-buffer-default-string*))
  (declare (values search-function key))
  (let ((*mini-buffer-default-string* default)
        (read-function-name-must-be-defined nil)
        (read-function-name-old-global-mouse-char-blinker-handler
          *global-mouse-char-blinker-handler*)
        (read-function-name-old-global-mouse-char-blinker-documentation-string
          *global-mouse-char-blinker-documentation-string*)
        (read-function-name-old-mouse-font-char
          *mouse-font-char*)
        (read-function-name-old-mouse-x-offset
          *mouse-x-offset*)
        (read-function-name-old-mouse-y-offset
          *mouse-y-offset*))
    (declare (special read-function-name-must-be-defined
                      read-function-name-old-global-mouse-char-blinker-handler
                      read-function-name-old-global-mouse-char-blinker-documentation-string
                      read-function-name-old-mouse-font-char
                      read-function-name-old-mouse-x-offset
                      read-function-name-old-mouse-y-offset))
    (let ((*post-command-hook*
            (append *post-command-hook* '(read-function-name-command-hook)))
          (*mini-buffer-value-history*
            *definition-name-history*))
      (let ((*batch-undo-save* t))
        (delete-interval (window-interval *mini-buffer-window*)))
      (unwind-protect
          (progn (read-function-name-command-hook nil)
                 (get-extended-string-search-strings reverse-p prompt comtab))
        (read-function-name-command-hook t)))))

(DEFPROP READ-FUNCTION-NAME-COMMAND-HOOK 10000000 COMMAND-HOOK-PRIORITY)

;; CHAR-OR-T is T meaning turn off special blinker hack;
;; otherwise turn it on iff mini buffer is empty.
(DEFUN READ-FUNCTION-NAME-COMMAND-HOOK (CHAR-OR-T)
  (DECLARE (SPECIAL READ-FUNCTION-NAME-MUST-BE-DEFINED
                    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
                    READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
                    READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
                    READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
                    READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET))
  (flet ((revert-blinker ()
          (SEND *GLOBAL-MOUSE-CHAR-BLINKER* ':SET-VISIBILITY NIL)
          (SETQ *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*
                READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-HANDLER
                *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
                READ-FUNCTION-NAME-OLD-GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING
                *MOUSE-HOOK* NIL
                *MOUSE-FONT-CHAR*
                READ-FUNCTION-NAME-OLD-MOUSE-FONT-CHAR
                *MOUSE-X-OFFSET*
                READ-FUNCTION-NAME-OLD-MOUSE-X-OFFSET
                *MOUSE-Y-OFFSET*
                READ-FUNCTION-NAME-OLD-MOUSE-Y-OFFSET)
           (TV:MOUSE-SET-BLINKER-DEFINITION ':CHARACTER *MOUSE-X-OFFSET* *MOUSE-Y-OFFSET*
                                            ':ON ':SET-CHARACTER *MOUSE-FONT-CHAR*)))
    (COND ((AND (NEQ CHAR-OR-T T)
                (BP-= (INTERVAL-FIRST-BP (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
                      (INTERVAL-LAST-BP (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))))
           (SETQ *GLOBAL-MOUSE-CHAR-BLINKER-HANDLER*
                 (IF READ-FUNCTION-NAME-MUST-BE-DEFINED
                     'BLINK-FUNCTION
                   'BLINK-ATOM)
                 *GLOBAL-MOUSE-CHAR-BLINKER-DOCUMENTATION-STRING*
                 "Click left on highlighted name to select it, right to pull into mini-buffer"
                 *MOUSE-HOOK*
                 #'(LAMBDA (WINDOW CHAR IGNORE IGNORE &AUX TEM)
                     (case CHAR
                       (#/MOUSE-1-1
                        (MULTIPLE-VALUE-BIND (FCTN LINE START END)
                            (ATOM-UNDER-MOUSE WINDOW)
                          (WHEN (AND LINE
                                     (OR (FBOUNDP (SETQ TEM FCTN))
                                         (GET TEM ':SOURCE-FILE-NAME)
                                         (GET TEM 'ZMACS-BUFFERS)
                                         (STRING-IN-AARRAY-P TEM *ZMACS-COMPLETION-AARRAY*)
                                         (AND (NOT READ-FUNCTION-NAME-MUST-BE-DEFINED) TEM)))
                            (LET ((INT (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
                              (DELETE-INTERVAL INT)
                              (INSERT (INTERVAL-FIRST-BP INT) LINE START END))
                            (revert-blinker)
                            (*THROW 'RETURN-FROM-COMMAND-LOOP
                                    (SUBSTRING LINE START END)))))
                       (#/MOUSE-3-1
                        (MULTIPLE-VALUE-BIND (FCTN LINE START END)
                            (ATOM-UNDER-MOUSE WINDOW)
                          (WHEN (AND LINE
                                     (OR (FBOUNDP (SETQ TEM FCTN))
                                         (GET TEM ':SOURCE-FILE-NAME)
                                         (GET TEM 'ZMACS-BUFFERS)
                                         (STRING-IN-AARRAY-P TEM *ZMACS-COMPLETION-AARRAY*)
                                         (AND (NOT READ-FUNCTION-NAME-MUST-BE-DEFINED) TEM)))
                            (LET ((INT (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
                              (DELETE-INTERVAL INT)
                              (move-bp (window-point *mini-buffer-window*)
                                       (INSERT (INTERVAL-FIRST-BP INT) LINE START END)))
                            (must-redisplay *mini-buffer-window* dis-text)
                            (revert-blinker)
                            (throw 'zwei-command-loop nil))))))
                 *MOUSE-FONT-CHAR* 0
                 *MOUSE-X-OFFSET* 4
                 *MOUSE-Y-OFFSET* 0)
           (TV:MOUSE-SET-BLINKER-DEFINITION ':CHARACTER *MOUSE-X-OFFSET* *MOUSE-Y-OFFSET*
                                            ':ON ':SET-CHARACTER *MOUSE-FONT-CHAR*))
          (T (revert-blinker)))
    (TV:MOUSE-WAKEUP)))

(DEFUN ATOM-UNDER-MOUSE (WINDOW &OPTIONAL CHAR X Y LINE INDEX &AUX SYMBOL END)
  "Returns the symbol which the mouse is pointing at in WINDOW.  NIL if not pointing at one.
Normally, CHAR, X, Y, LINE, and INDEX are set from the mouse position.
If you pass them, then the mouse position is irrelevant.
Actually, X and Y are irrelevant in any case.
All that matters is LINE and INDEX, and CHAR which would be the character there.
The values are the symbol pointed at, the line it is in,
and the start and end indices of the symbol as a substring in that line.
All values are NIL if the position is not on a valid symbol."
  (DECLARE (VALUES SYMBOL LINE START END))
  (OR CHAR (MULTIPLE-VALUE (CHAR X Y LINE INDEX)
               (MOUSE-CHAR WINDOW)))
  (AND CHAR
       ( CHAR #/CR)
       (DO ((I INDEX (1- I)))
           ((OR (ZEROP I)
                ( (ATOM-WORD-SYNTAX (CHAR-CODE (AREF LINE I))) WORD-ALPHABETIC))
            (AND ( I INDEX)
                 (CATCH-ERROR (LET ((*PACKAGE* *PACKAGE*)
                                    (*read-base* *read-base*)
                                    (*print-base* *print-base*)
                                    (*readtable* *readtable*)
                                    (READ-PRESERVE-DELIMITERS T)
                                    (INTERVAL (WINDOW-INTERVAL WINDOW)))
                                (COMPUTE-BUFFER-PACKAGE INTERVAL)
                                (MULTIPLE-VALUE (SYMBOL END)
                                  (CLI:READ-FROM-STRING LINE NIL NIL
                                                        ':START (SETQ I (1+ I))
                                                        ':PRESERVE-WHITESPACE T))
                                (SETQ END (MIN (ARRAY-ACTIVE-LENGTH LINE) END)))
                              NIL)
                 (SYMBOLP SYMBOL)
                 (VALUES SYMBOL LINE I END))))))

;;; This blinks functions that you point to
;;; This maximum speed thing is a crock, since the mouse can be moving fast
;;; and at the same time have come to rest, such that another :MOUSE-MOVES
;;; message is not going to be sent.  I guess I'll just set the number very high.
;;; This was probably put in to make mouse tracking smoother, and hence is no
;;; longer needed.
(DEFVAR *BLINKING-FUNCTION-MAXIMUM-MOUSE-SPEED* 50.)
(DEFUN BLINK-FUNCTION (BLINKER WINDOW CHAR X Y LINE INDEX &OPTIONAL NOT-DEFINED-OK
                                                          &AUX SYMBOL BEG END SHEET)
  (COND ((> TV:MOUSE-SPEED *BLINKING-FUNCTION-MAXIMUM-MOUSE-SPEED*)
         (TV:BLINKER-SET-VISIBILITY BLINKER NIL))       ;Moving too fast, forget it
        (T
         (MULTIPLE-VALUE (SYMBOL NIL BEG END)
           (ATOM-UNDER-MOUSE WINDOW CHAR X Y LINE INDEX))
         (COND ((AND (NOT (NULL BEG))
                     (OR NOT-DEFINED-OK
                         (FBOUNDP SYMBOL)
                         (GET SYMBOL 'ZMACS-BUFFERS)
                         (GET SYMBOL ':SOURCE-FILE-NAME)
                         (STRING-IN-AARRAY-P SYMBOL *ZMACS-COMPLETION-AARRAY*)))
                (SETQ SHEET (WINDOW-SHEET WINDOW))
                (TV:BLINKER-SET-SHEET BLINKER SHEET)
                (SHEET-SET-BLINKER-CURSORPOS SHEET BLINKER
                                             (- X (TV:SHEET-STRING-LENGTH SHEET LINE BEG
                                                                          INDEX))
                                             Y)
                (TV:BLINKER-SET-SIZE BLINKER
                                     (TV:SHEET-STRING-LENGTH SHEET LINE BEG END)
                                     (FONT-CHAR-HEIGHT (AREF (TV:SHEET-FONT-MAP SHEET)
                                                             (LDB %%CH-FONT CHAR))))
                (TV:BLINKER-SET-VISIBILITY BLINKER T))
               (T
                (TV:BLINKER-SET-VISIBILITY BLINKER NIL))))))

(DEFUN BLINK-ATOM (BLINKER WINDOW CHAR X Y LINE INDEX)
  (BLINK-FUNCTION BLINKER WINDOW CHAR X Y LINE INDEX T))

;;; The commands themselves

;;; Single click on the left button.
(DEFPROP COM-MOUSE-MARK-REGION "Move point" :MOUSE-SHORT-DOCUMENTATION)
(DEFCOM COM-MOUSE-MARK-REGION "Jump point and mark to where the mouse is.
Then as the mouse is moved with the button held down point follows the mouse." (KM)
  (REDISPLAY *WINDOW* ':NONE)
  (LET ((POINT (POINT))
        (MARK (MARK))
        (OLD-REGION-P (WINDOW-MARK-P *WINDOW*))
        (BP (MOUSE-BP *WINDOW* *MOUSE-X* *MOUSE-Y*)))
    (MOVE-BP MARK BP)
    (SETF (WINDOW-MARK-P *WINDOW*) T)
    (DO ((LAST-X *MOUSE-X*)
         (LAST-Y *MOUSE-Y*))
        (NIL)
      (MOVE-BP POINT BP)
      (MUST-REDISPLAY *WINDOW* DIS-BPS)
      (REDISPLAY *WINDOW* ':POINT)
      (OR (WAIT-FOR-MOUSE LAST-X LAST-Y) (RETURN NIL))
      (MULTIPLE-VALUE (LAST-X LAST-Y) (MOUSE-POSITION))
      (SETQ BP (MOUSE-BP *WINDOW* LAST-X LAST-Y)))
    (AND (BP-= POINT MARK)
         (SETF (WINDOW-MARK-P *WINDOW*) OLD-REGION-P)))
    DIS-NONE)

(DEFPROP COM-MOUSE-MOVE-REGION "Move to point" :MOUSE-SHORT-DOCUMENTATION)
(DEFCOM COM-MOUSE-MOVE-REGION "Select window, or adjust the region.
If there is a region, jump the mouse to point or mark (whichever
is closer), and move it with the mouse as long as the button is
held down.  If there is no region, select the window without
affecting point (or mark)." (KM)
  (LET ((SHEET (WINDOW-SHEET *WINDOW*))
        PX PY MX MY BP BP1 LAST-X LAST-Y)
    (MULTIPLE-VALUE (MX MY)
        (FIND-BP-IN-WINDOW-COORDS (MARK) *WINDOW*))
    (MULTIPLE-VALUE (PX PY)
        (FIND-BP-IN-WINDOW-COORDS (POINT) *WINDOW*))
    (MULTIPLE-VALUE (LAST-X LAST-Y) (MOUSE-POSITION))
    (SETQ BP (COND ((NOT (AND (WINDOW-MARK-P *WINDOW*) MX))
                    (IF PX (POINT) (BARF)))
                   ((AND PX
                         (< (+ (^ (- LAST-X PX) 2) (^ (- LAST-Y PY) 2))
                            (+ (^ (- LAST-X MX) 2) (^ (- LAST-Y MY) 2))))
                    (POINT))
                   (T
                    (SETQ PX MX PY MY)
                    (MARK))))
    (FUNCALL SHEET ':SET-MOUSE-CURSORPOS
                   (+ PX (FLOOR (TV:SHEET-CHAR-WIDTH SHEET) 2))
                   (+ PY (FLOOR (* 3 (TV:SHEET-LINE-HEIGHT SHEET)) 4)))
    (DO ()
        (NIL)
      (OR (WAIT-FOR-MOUSE LAST-X LAST-Y) (RETURN NIL))
      (MULTIPLE-VALUE (LAST-X LAST-Y) (MOUSE-POSITION))
      (SETQ BP1 (MOUSE-BP *WINDOW* LAST-X LAST-Y))
      (MOVE-BP BP BP1)
      (MUST-REDISPLAY *WINDOW* DIS-BPS)
      (REDISPLAY *WINDOW* ':POINT)))
  DIS-NONE)

(DEFPROP COM-MOUSE-MARK-THING "Mark thing" :MOUSE-SHORT-DOCUMENTATION)
(DEFCOM COM-MOUSE-MARK-THING "Mark the thing you are pointing at." (SM)
  (DO ((POINT (POINT))
       (MARK (MARK))
       (LAST-X *MOUSE-X*)
       (LAST-Y *MOUSE-Y*)
       (X) (Y) (CHAR) (LINE) (CHAR-POS) (OL) (OCP))
      (NIL)
    (MULTIPLE-VALUE (CHAR X Y LINE CHAR-POS)
        (MOUSE-CHAR *WINDOW* NIL LAST-X LAST-Y))        ;Figure out where mouse is
    (COND ((AND CHAR (OR (NEQ LINE OL) ( CHAR-POS OCP)))
           (SETQ OL LINE OCP CHAR-POS)
           (MOVE-BP POINT LINE CHAR-POS)
;          (FUNCALL (SELECTQ (GET *MAJOR-MODE* 'EDITING-TYPE)
;                     (:LISP 'LISP-MARK-THING)
;                     (:TEXT 'TEXT-MARK-THING)
;                     (OTHERWISE 'DEFAULT-MARK-THING))
;                   POINT MARK CHAR LINE CHAR-POS)
           (funcall (or (call-editing-type-function *major-mode*
                                                    'mark-thing-function
                                                    nil)
                        'default-mark-thing)
                    POINT MARK CHAR LINE CHAR-POS)

           (MUST-REDISPLAY *WINDOW* DIS-BPS)
           (REDISPLAY *WINDOW* ':POINT)))
    (OR (WAIT-FOR-MOUSE LAST-X LAST-Y) (RETURN NIL))
    (MULTIPLE-VALUE (LAST-X LAST-Y) (MOUSE-POSITION)))
  DIS-NONE)

(DEFUN LISP-MARK-THING (POINT MARK CHAR LINE CHAR-POS)
  (ATOM-WORD-SYNTAX-BIND
    (SELECT (LIST-SYNTAX CHAR)
      ((LIST-OPEN LIST-SINGLE-QUOTE)
       (MOVE-BP MARK (FORWARD-SEXP POINT 1 T)))
      (LIST-CLOSE
       (MOVE-BP POINT (FORWARD-CHAR POINT 1))
       (MOVE-BP MARK (FORWARD-SEXP POINT -1 T 0 NIL NIL)))
      (LIST-DOUBLE-QUOTE
       (COND ((LISP-BP-SYNTACTIC-CONTEXT POINT)
              (MOVE-BP POINT (FORWARD-CHAR POINT 1 T))
              (MOVE-BP MARK (FORWARD-SEXP POINT -1)))
             (T
              (MOVE-BP MARK (FORWARD-SEXP POINT 1 T)))))
      (LIST-COMMENT
       (MOVE-BP POINT (BACKWARD-OVER *BLANKS* POINT))
       (MOVE-BP MARK LINE (LINE-LENGTH LINE)))
      (OTHERWISE
       (DEFAULT-MARK-THING POINT MARK CHAR LINE CHAR-POS)))))

(DEFUN TEXT-MARK-THING (POINT MARK CHAR LINE CHAR-POS)
  (COND ((MEMQ CHAR '(#/. #/? #/!))
         (MOVE-BP POINT (FORWARD-CHAR POINT 1))
         (MOVE-BP MARK (FORWARD-SENTENCE POINT -1 T)))
        ((MEMQ CHAR '(#/: #/; #/,))
         (MOVE-BP MARK (FORWARD-OVER *BLANKS* (FORWARD-CHAR
                                               (SEARCH-SET POINT
                                                           (IF (= CHAR #/,)
                                                               '(#/. #/? #/! #/: #/; #/,)
                                                               '(#/, #/? #/! #/: #/;))
                                                           T T)
                                               1 T)))
         (MOVE-BP POINT (FORWARD-CHAR POINT 1)))
        (T
         (DEFAULT-MARK-THING POINT MARK CHAR LINE CHAR-POS))))

(DEFUN DEFAULT-MARK-THING (POINT MARK CHAR LINE CHAR-POS &AUX TEM)
  (COND ((= CHAR #/FF)
         (MOVE-BP MARK (FORWARD-PAGE POINT -1 T)))
        ((MEMQ CHAR '(#/SP #/TAB))
         (COND ((STRING-REVERSE-SEARCH-NOT-SET *BLANKS* LINE CHAR-POS)
                (MOVE-BP MARK (FORWARD-WORD POINT 1 T)))
               (T
                (MOVE-BP POINT LINE 0)
                (MOVE-BP MARK LINE (LINE-LENGTH LINE)))))
        ((= CHAR #/CR)
         (MOVE-BP MARK LINE 0))
        ((SETQ TEM (ASS 'CHAR-EQUAL CHAR *MATCHING-DELIMITER-LIST*))
         (MOVE-BP MARK (ZWEI-SEARCH POINT (CADR TEM) NIL T)))
        ((SETQ TEM (RASS #'(LAMBDA (X Y) (CHAR-EQUAL X (CAR Y)))
                         CHAR *MATCHING-DELIMITER-LIST*))
         (MOVE-BP POINT (FORWARD-CHAR POINT 1 T))
         (MOVE-BP MARK (ZWEI-SEARCH POINT (CAR TEM) T T)))
        (T
         (MOVE-BP MARK (FORWARD-WORD POINT 1 T))
         (MOVE-BP POINT (FORWARD-WORD MARK -1 T))
         ;; Now try to attach the right whitespace to the word
         (OR *KILL-INTERVAL-SMARTS*
             (LET ((BP (FORWARD-OVER *BLANKS* MARK)))
               (COND ((NOT (BP-= BP MARK))
                      (MOVE-BP MARK BP))
                     (T
                      (SETQ BP (BACKWARD-OVER *BLANKS* POINT))
                      (OR (ZEROP (BP-INDEX BP)) (MOVE-BP POINT BP)))))))))

(DEFPROP COM-MOUSE-KILL-YANK "Save//Kill//Yank" :MOUSE-SHORT-DOCUMENTATION)
(DEFCOM COM-MOUSE-KILL-YANK "Kill region, unkill, or unkill pop.
If there is a region, save it; if it was saved last time, kill it;
else if the last command was an unkill, do unkill-pop, else unkill." ()
  (COND ((EQ *LAST-COMMAND-TYPE* 'SAVE)
         (DELETE-INTERVAL (POINT) (MARK))
         DIS-TEXT)
        ((WINDOW-MARK-P *WINDOW*)
         (SETQ *CURRENT-COMMAND-TYPE* 'SAVE)
         (COM-SAVE-REGION))
        ((EQ *LAST-COMMAND-TYPE* 'YANK)
         (COM-YANK-POP))
        (T
         (COM-YANK))))

;;; This is on mouse-left in the mini-buffer, exit if you are pointing in it, else
;;; do the standard thing
(DEFPROP COM-MOUSE-END-OF-MINI-BUFFER "Exit" :MOUSE-SHORT-DOCUMENTATION)
(DEFCOM COM-MOUSE-END-OF-MINI-BUFFER "Finish up the mini-buffer command" ()
  (COND ((NEQ *WINDOW* *MINI-BUFFER-WINDOW*)
         (COMMAND-EXECUTE (COMMAND-LOOKUP #/MOUSE-1-2 *STANDARD-COMTAB*) #/MOUSE-1-2))
        (T
         (KEY-EXECUTE #/CONTROL-RETURN))))

;;; This is on mouse-right in the completing-reader, give a menu of the possibilities
(DEFCOM COM-MOUSE-LIST-COMPLETIONS "Give a menu of possible completions" ()
  (MULTIPLE-VALUE-BIND (NIL POSS)
      (COMPLETE-STRING (BP-LINE (POINT)) *COMPLETING-ALIST* *COMPLETING-DELIMS*)
    (OR POSS (BARF))
    (MULTIPLE-VALUE-BIND (CHOICE ITEM)
        (TV:MENU-CHOOSE POSS)
      (IF CHOICE
          (*THROW 'RETURN-FROM-COMMAND-LOOP ITEM)
          DIS-NONE))))

(DEFCOM COM-MOUSE-INDENT-RIGIDLY "Track indentation with the mouse.
If there is a region, moves the whole region, else the current line.  Continues until the
mouse is released." (KM)
  (LET ((POINT (POINT))
        (SHEET (WINDOW-SHEET *WINDOW*))
        (START-LINE)
        (END-LINE))
    (COND ((WINDOW-MARK-P *WINDOW*)             ;If there is a region, use it
           (REGION (BP1 BP2)
                   (SETQ START-LINE (BP-LINE BP1)
                         END-LINE (BP-LINE BP2))
                   (OR (ZEROP (BP-INDEX BP2))
                       (SETQ END-LINE (LINE-NEXT END-LINE)))))
          (T
           (SETQ START-LINE (BP-LINE POINT)
                 END-LINE (LINE-NEXT START-LINE))))
    (MULTIPLE-VALUE-BIND (X Y)
        (FIND-BP-IN-WINDOW-COORDS (FORWARD-OVER *BLANKS* (BEG-OF-LINE START-LINE)) *WINDOW*)
    (FUNCALL SHEET ':SET-MOUSE-CURSORPOS X Y))
    (process-wait "Mouse" #'(lambda () (not (zerop (tv:mouse-buttons)))))
    (DO ((LAST-X)
         (LAST-Y)
         (BP (COPY-BP POINT))
         (DELTA))
        (NIL)
      (MULTIPLE-VALUE (LAST-X LAST-Y) (MOUSE-POSITION))
      (SETQ DELTA (LINE-INDENTATION START-LINE SHEET))
      (MOVE-BP BP START-LINE 0)
      (INDENT-LINE BP (MAX 0 LAST-X) SHEET)
      (SETQ DELTA (- (LINE-INDENTATION START-LINE SHEET) DELTA))
      (OR (= DELTA 0)
              (DO ((LINE START-LINE (LINE-NEXT LINE)))
              ((EQ LINE END-LINE))
            (AND (NEQ LINE START-LINE)
                 (INDENT-LINE (MOVE-BP BP LINE 0)
                              (MAX 0 (+ DELTA (LINE-INDENTATION LINE SHEET))) SHEET))))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT)
      (REDISPLAY *WINDOW* ':POINT nil nil t)
      (wait-for-mouse last-x last-y 10)
      (when (zerop (tv:mouse-buttons)) (return))))
  DIS-TEXT)

;;; *** This should figure out some other kind of mouse-blinker ***
(DEFCOM COM-MOUSE-INDENT-UNDER "Indent the current line as selected by the mouse." (KM)
  (LET ((CH (FUNCALL STANDARD-INPUT ':MOUSE-OR-KBD-TYI)))
    (COND ((= CH #/MOUSE-1-1)
           (INDENT-LINE (POINT) (BP-INDENTATION (MOUSE-BP *WINDOW*)))
           (INDENT-BP-ADJUSTMENT (POINT))
           DIS-TEXT)
          (T
           (FUNCALL STANDARD-INPUT ':UNTYI CH)
           (COM-INDENT-UNDER)))))

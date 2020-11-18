;;; Some simple ZWEI command functions. -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:T -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; DEFS contains a list of all free variables used by these commands.
;;; Any editor that wishes to use these commands must bind all of them.
;;; When adding any to this file, or to primitives called by functions
;;; in this file, update the list in DEFS.

;;; The caller of these functions should also provide a function
;;; called BARF, to report errors.  It should take arguments like
;;; the &REST to FORMAT.

;;; Commands in this file also use the PROMPT and TYPEIN lines
;;; to interact with the user.  In order to keep the implementation
;;; of these lines as flexible as possible, a very minimal interface
;;; is defined herein.  The following functions may be used to access
;;; these lines:

;;; (PROMPT-LINE <ctl-string> . <args>)
;;;    Do formatted output to the prompt line.  The line is cleared and homed first.

;;; (PROMPT-LINE-MORE <ctl-string> . <args>)
;;;    Do formatted output to the prompt line, without clearing and homeing.

;;; *QUERY-IO*
;;;    The typein window under the mode line.

;;; (TYPEIN-LINE-Y-OR-N-P <ctl-string> . <args>)
;;;    Do formatted output to the typein line, and ask for a Y or N answer (see Y-OR-N-P).

;;; (TYPEIN-LINE-ACTIVATE . <body>)
;;;    This is a SPECIAL FORM.  Within the body, the blinker for the typein line
;;;    will be on, and input from the typein line is allowed by using the
;;;    value of TYPEIN-LINE-STREAM, on which regular stream input operations will
;;;    work.  None of the other TYPEIN line or PROMPT line functions should
;;;    be called while in the scope of a TYPEIN-LINE-ACTIVATE; all you may
;;;    do is read from the stream.

;;; (TYPEIN-LINE-READLINE <ctl-string> . <args>)
;;;    Read in a line from the typein line, with editting.  The arguments
;;;    are passed along to the prompt line.

;;; (TYPEIN-LINE-READ <ctl-string> . <args>)
;;;    Like the above, but does a READ instead of a READLINE.

;;; *STANDARD-OUTPUT*
;;;    A larger window with mouse sensitive items for random stream output.

(DEFINE-COMMAND-DOCUMENTATION COM-SELF-INSERT
  (CASE OP
    (:NAME "Self Insert")
    (:FULL
     (IF CHAR
         (FORMAT *STANDARD-OUTPUT*
                 "~&Self insertion: ~:C inserts a /"~:[~C~;~\lozenged-char\~]/"."
                 CHAR (EQ CHAR #/OVERSTRIKE) CHAR)
       (FORMAT *STANDARD-OUTPUT*
               "~&Inserts the last command character typed.")))
    (:SHORT (FORMAT *STANDARD-OUTPUT* "Self insertion"))))

(DEFCOM COM-SELF-INSERT "Inserts itself." (NM)
  (LET ((CHAR (IN-CURRENT-FONT *LAST-COMMAND-CHAR*))
        (POINT (POINT)))
    (LET ((LINE (BP-LINE POINT)) (INDEX (BP-INDEX POINT)))
         (DOTIMES (I *NUMERIC-ARG*)
           (INSERT-MOVING POINT CHAR))
         (SETQ *CURRENT-COMMAND-TYPE* 'SELF-INSERT)
         (IF (CHAR= CHAR #/RETURN)
             DIS-TEXT
             (VALUES DIS-LINE LINE INDEX)))))

(DEFCOM COM-QUOTED-INSERT "Insert a quoted character.
Another character is read and inserted.  Even ABORT can be inserted in this way.
If the next character is a control character, the ASCII equivalent is inserted.
If the next character is an octal digit, then up to two more octal digits are read,
  and the character from the lispm character set with that code is intersted."
        (NM)
  (LET (CH)
    ;; Read next char turning off normal intercepted meanings of Abort and Break.
    (LET ((TV:KBD-INTERCEPTED-CHARACTERS
            (REM-IF (LAMBDA (ELT)
                      (ZEROP (CHAR-BITS (CAR ELT))))
             TV:KBD-INTERCEPTED-CHARACTERS)))
      (SETQ CH (INPUT-WITH-PROMPTS *STANDARD-INPUT* :READ-CHAR)))
    (COND ((CHAR-BIT CH :CONTROL)
           (DOTIMES (I *NUMERIC-ARG*)
             (INSERT-MOVING (POINT) (INT-CHAR (LOGAND #o37 (CHAR-CODE CH)))))
           DIS-TEXT)
          (T
           (WHEN (CHAR #/0 CH CH #/7)
             (DISCARD-LAST-PROMPT)
             (PRINT-PROMPTS)
             (WRITE-CHAR CH *QUERY-IO*)
             (DO ((I 2 (1- I))
                  (CODE (- (CHAR-INT CH) (CHAR-INT #/0)))
                  (CH1))
                 (( I 0) (SETQ CH (INT-CHAR CODE)))
               (SETQ CH1 (SEND *STANDARD-INPUT* :READ-CHAR))
               (COND ((CHAR #/0 CH1 #/7)
                      (WRITE-CHAR CH1 *QUERY-IO*)
                      (SETQ CODE (+ (* CODE 8.) (- (CHAR-INT CH1) (CHAR-INT #/0)))))
                     (T
                      (OR (CHAR= CH1 #/SP)
                          (SEND *STANDARD-INPUT* :UNREAD-CHAR CH1))
                      (RETURN (SETQ CH (INT-CHAR CODE)))))))
           (LET ((*LAST-COMMAND-CHAR* CH))
             (COM-SELF-INSERT))))))

(DEFCOM COM-FORWARD "Move one or more characters forward.
Move point one character forward.  With a numeric argument,
move point that many characters forward." (KM R)
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (OR (FORWARD-CHAR POINT *NUMERIC-ARG*) (BARF))))
  (SET-CENTERING-FRACTION *NUMERIC-ARG*)
  DIS-BPS)

(DEFCOM COM-BACKWARD "Move one or more characters backward.
Move point one character backward.  With a numeric argument,
move point that many characters backward." (KM -R)
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (OR (FORWARD-CHAR POINT (- *NUMERIC-ARG*)) (BARF))))
  (SET-CENTERING-FRACTION (- *NUMERIC-ARG*))
  DIS-BPS)

(DEFCOM COM-GOTO-CHARACTER "Move point to the nth character in the buffer.
With a negative argument, use the absolute value of the argument, and
 count the characters the way ITS would count them, namely,
 count newlines as two characters rather than one.  This is useful for interpreting
 character counts returned by R and BOLIO.
With no argument, just feep; the user was probably in Bolio mode and confused." (KM)
  (IF (NOT *NUMERIC-ARG-P*)
      (BARF))
  (LET ((DEST (FUNCALL (IF (MINUSP *NUMERIC-ARG*) #'FORWARD-ITS-CHAR #'FORWARD-CHAR)
                       (INTERVAL-FIRST-BP *INTERVAL*) (ABS *NUMERIC-ARG*))))
    (IF (NULL DEST)
        (BARF "There are fewer than ~D. characters in the buffer." *NUMERIC-ARG*)
      (MOVE-BP (POINT) DEST)))
  DIS-BPS)

(DEFCOM COM-DOWN-REAL-LINE "Move down vertically to next real line.
Moves as far as possible horizontally toward the goal column for successive
commands.  The goal column is normally the column you start at,
but the command C-X C-N sets a semipermanent goal column." (KM R)
  (DOWN-REAL-LINE *NUMERIC-ARG*))

(DEFCOM COM-UP-REAL-LINE "Move up vertically to previous real line.
Moves as far as possible horizontally toward the goal column for successive
commands.  The goal column is normally the column you start at,
but the command C-X C-N sets a semipermanent goal column." (KM -R)
  (DOWN-REAL-LINE (- *NUMERIC-ARG*)))

(DEFUN DOWN-REAL-LINE (N-LINES)
  (SETQ *CURRENT-COMMAND-TYPE* 'REAL-MOVE)
  (SET-CENTERING-FRACTION N-LINES)
  (LET ((POINT (POINT))
        (RET DIS-BPS))
    (LET ((DEST (FORWARD-LINE POINT N-LINES)))
      (COND ((NULL DEST)
             ;; He overshot.
             (COND ((MINUSP N-LINES)
                    ;; He was going backwards, go to beginnning.
                    (MOVE-BP POINT (INTERVAL-FIRST-BP *INTERVAL*))
                    (SETQ *REAL-LINE-GOAL-XPOS* 0))
                   ((NOT *NUMERIC-ARG-P*)
                    ;; No argument give, going down.  Create a line.
                    (SETQ RET DIS-TEXT)
                    (MOVE-BP POINT (INSERT (INTERVAL-LAST-BP *INTERVAL*) #/NEWLINE)))
                   (T
                    ;; He was going forwards, go to end.
                    (MOVE-BP POINT (INTERVAL-LAST-BP *INTERVAL*))
                    (SETQ *REAL-LINE-GOAL-XPOS* (BP-INDENTATION POINT)))))
            (T
             (SETQ DEST (BP-LINE DEST))
             (SETQ *REAL-LINE-GOAL-XPOS*
                   (COND (*PERMANENT-REAL-LINE-GOAL-XPOS*)
                         ((EQ *LAST-COMMAND-TYPE* 'REAL-MOVE)
                          *REAL-LINE-GOAL-XPOS*)
                         (T (BP-INDENTATION POINT))))
             (LET ((INDEX (INDENTATION-INDEX DEST *REAL-LINE-GOAL-XPOS*)))
               (MOVE-BP POINT DEST
                        (COND (INDEX)
                              ((NEQ DEST (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                               (LINE-LENGTH DEST))
                              (T (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*)))))))))
    RET))

(DEFCOM COM-SET-GOAL-COLUMN "Sets the goal column for Up Real Line and Down Real Line.
This command takes the current horizontal position of the cursor
and makes it the /"goal column/" for the default definitions of C-N and C-P.
They try to move to the goal column in the line they move to.
This command with a numeric argument gets rid of the goal column.

Supply a numeric argument to cancel any goal column setting."
        (KM)
  (REPORT-COLUMN-SETTING "c-N//c-P goal column"
                         (SETQ *PERMANENT-REAL-LINE-GOAL-XPOS* (IF *NUMERIC-ARG-P* NIL
                                                                   (BP-INDENTATION (POINT)))))
  DIS-NONE)

(DEFCOM COM-RECENTER-WINDOW "Choose a new point in buffer to begin redisplay.
With no argument, center point on the screen.  An argument is the
line of the window to put point on.  Negative arguments count
up from the bottom." (KM)
  (OR *NUMERIC-ARG-P* (MUST-REDISPLAY *WINDOW* DIS-ALL))
  (LET ((N-PLINES (WINDOW-N-PLINES *WINDOW*)))
    (RECENTER-WINDOW *WINDOW*
               ':ABSOLUTE
               (IF *NUMERIC-ARG-P*
                   (// (RANGE (+ *NUMERIC-ARG*
                                 (IF (MINUSP *NUMERIC-ARG*) N-PLINES 0))
                              0 (1- N-PLINES))
                       (SMALL-FLOAT N-PLINES))
                   *CENTER-FRACTION*)))
  DIS-NONE)

(DEFCOM COM-COMPLETE-REDISPLAY "Redisplay all windows.
By default, the text is not scrolled on the screen.
However, a numeric argument specifies the screen line
to scroll point to (negative counting from the bottom)." (KM)
  (DOLIST (W (FRAME-EXPOSED-WINDOWS))
    (PREPARE-WINDOW-FOR-REDISPLAY W))
  (SEND (SEND *WINDOW* ':ALIAS-FOR-SELECTED-WINDOWS) ':REFRESH)
  (WHEN *NUMERIC-ARG-P*
    (COM-RECENTER-WINDOW))
  DIS-NONE)

(DEFCOM COM-NEXT-SCREEN "Move down to display next screenful of text.
With argument, move window down <arg> lines." (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (IF *NUMERIC-ARG-P*
                                         *NUMERIC-ARG*
                                         (- (WINDOW-N-PLINES *WINDOW*)
                                            *NEXT-SCREEN-CONTEXT-LINES*)))
  DIS-NONE)

(DEFCOM COM-PREVIOUS-SCREEN "Move up to display previous screenful of text.
With argument, move window up <arg> lines." (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (IF *NUMERIC-ARG-P*
                                         (- *NUMERIC-ARG*)
                                         (- *NEXT-SCREEN-CONTEXT-LINES*
                                            (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

(DEFCOM COM-NEXT-SEVERAL-SCREENS "Move down argument screenfuls of text" (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (* *NUMERIC-ARG* (1- (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

(DEFCOM COM-PREVIOUS-SEVERAL-SCREENS "Move down argument screenfuls of text" (KM)
  (RECENTER-WINDOW-RELATIVE *WINDOW* (* *NUMERIC-ARG* (- 1 (WINDOW-N-PLINES *WINDOW*))))
  DIS-NONE)

(DEFCOM COM-BEGINNING-OF-LINE "Move to the beginning of the line.
With a numeric argument, also moves forward by a number of lines
one less than the argument." (KM)
  (MOVE-BP (POINT) (BEG-LINE (POINT) (1- *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-END-OF-LINE "Move to the end of the line.
With a numeric argument, also moves forward by a number of lines
one less than the argument." (KM)
  (MOVE-BP (POINT) (END-LINE (POINT) (1- *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-MOVE-TO-SCREEN-EDGE "Jump to top or bottom of screen.
A numeric argument specifies the screen line to go to, negative arguments count
up from the bottom." (KM)
  (REDISPLAY *WINDOW* ':POINT NIL NIL T)        ;Force redisplay to completion first
  (LET ((N-PLINES (WINDOW-N-PLINES *WINDOW*)))
    (LET ((PLINE (RANGE (IF *NUMERIC-ARG-P*
                            (+ *NUMERIC-ARG*
                               (IF (MINUSP *NUMERIC-ARG*) N-PLINES 0))
                            (FIX (* *CENTER-FRACTION* N-PLINES)))
                        0 N-PLINES)))
      (LET ((LINE (PLINE-LINE *WINDOW* PLINE)))
        (COND ((NOT (NULL LINE))
               (MOVE-BP (POINT) LINE (PLINE-FROM-INDEX *WINDOW* PLINE)))
              ((OR (NOT *NUMERIC-ARG-P*) (MINUSP *NUMERIC-ARG*))
               (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*)))
              (T (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))))))
  DIS-BPS)

(DEFCOM COM-GOTO-BEGINNING "Go to beginning of buffer.
With an argument from 0 to 10, goes that many tenths of the length of the buffer
down from the beginning." (KM PUSH)
  (COND ((NOT *NUMERIC-ARG-P*)
         (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))
        (T (MOVE-FRACTIONALLY *NUMERIC-ARG*)))
  DIS-BPS)

(DEFCOM COM-GOTO-END "Go to the end of the buffer.
With an argument from 0 to 10, goes that many tenths of the length of the buffer
from the end." (KM PUSH)
  (COND ((NOT *NUMERIC-ARG-P*)
         (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*)))
        (T (MOVE-FRACTIONALLY (- 10. *NUMERIC-ARG*))))
  DIS-BPS)

(DEFUN MOVE-FRACTIONALLY (TENTHS)
  (COND ((OR (> TENTHS 10.)
             (< TENTHS 0))
         (BARF "The argument must be between 0 and 10."))
        (T
         (MOVE-BP (POINT)
          (FORWARD-LINE
           (INTERVAL-FIRST-BP *INTERVAL*)
           (FLOOR (* (COUNT-LINES *INTERVAL*) TENTHS) 10.)
           T)))))

(DEFCOM COM-MARK-BEGINNING "Put the mark at the beginning of the buffer." (SM)
  (MOVE-BP (MARK) (INTERVAL-FIRST-BP *INTERVAL*))
  DIS-BPS)

(DEFCOM COM-MARK-END "Put the mark at the end of the buffer." (SM)
  (MOVE-BP (MARK) (INTERVAL-LAST-BP *INTERVAL*))
  DIS-BPS)

(DEFCOM COM-SWAP-POINT-AND-MARK "Exchange point and the mark." (SM)
  (OR (EQ (BP-TOP-LEVEL-NODE (POINT)) (BP-TOP-LEVEL-NODE (MARK)))
      (BARF "Point and mark not in same buffer"))
  (SWAP-BPS (POINT) (MARK))
  DIS-BPS)

(DEFCOM COM-SET-POP-MARK "Sets or pops the mark.
With no Control-U's, sets the mark at the point, and pushes point onto the point pdl.
With one Control-U, pops the point pdl and moves to last point.
With two Control-U's, pops the point pdl but does not move." (KM)
  (COND ((OR (NEQ *NUMERIC-ARG-P* ':CONTROL-U)
             (< *NUMERIC-ARG* 4))
         (POINT-PDL-PUSH (POINT) *WINDOW* NIL T)
         (MOVE-BP (MARK) (POINT))
         (SETF (WINDOW-MARK-P *WINDOW*) T)
         DIS-BPS)
        ((< *NUMERIC-ARG* 16.)
         (MULTIPLE-VALUE-BIND (BP PLINE)
             (POINT-PDL-POP *WINDOW*)
           (POINT-PDL-MOVE BP PLINE))
         DIS-BPS)
        (T
         (POINT-PDL-POP *WINDOW*)
         DIS-NONE)))

(DEFCOM COM-PUSH-POP-POINT-EXPLICIT "Push or pop point onto the point pdl.
With no argument, push point onto the point pdl.
With an argument, exchanges point with the nth position on the stack." (KM)
  (COND ((NOT *NUMERIC-ARG-P*)
         (POINT-PDL-PUSH (POINT) *WINDOW* T T)
         DIS-NONE)
        (T
         (MULTIPLE-VALUE-BIND (BP PLINE)
              (POINT-PDL-EXCH (POINT) *WINDOW* *NUMERIC-ARG-P* *NUMERIC-ARG*)
           (POINT-PDL-MOVE BP PLINE))
         DIS-BPS)))

(DEFCOM COM-MOVE-TO-PREVIOUS-POINT "Exchange point and top of point pdl.
A numeric argument rotates top arg entries of the point pdl (the default
numeric argument is 2).  An argument of 1 rotates the whole point pdl
and a negative argument rotates the other way." ()
  (ROTATE-POINT-PDL *WINDOW* (IF (MEMQ *NUMERIC-ARG-P* '(:SIGN NIL))
                                 (* 2 *NUMERIC-ARG*) *NUMERIC-ARG*)))

(DEFVAR *DEFAULT-PREVIOUS-POINT-ARG* 3)
(DEFCOM COM-MOVE-TO-DEFAULT-PREVIOUS-POINT "Rotate the point pdl.
A numeric argument specifies the number of entries to rotate, and sets the new default." ()
  (OR (MEMQ *NUMERIC-ARG-P* '(:SIGN NIL))
      (SETQ *DEFAULT-PREVIOUS-POINT-ARG* *NUMERIC-ARG*))
  (ROTATE-POINT-PDL *WINDOW* (IF (EQ *NUMERIC-ARG-P* ':SIGN)
                                 (* *NUMERIC-ARG* *DEFAULT-PREVIOUS-POINT-ARG*)
                                 *DEFAULT-PREVIOUS-POINT-ARG*)))

(DEFCOM COM-INSERT-CRS "Insert one or more Returns into the buffer, moving point.
In auto fill mode, if no numeric argument,
fills the line before point as well as inserting a Return.
This might cause another Return to be inserted earlier in the line." ()
  (LET ((POINT (POINT)))
    (SETQ *CURRENT-COMMAND-TYPE* 'INSERT-CR)
    (DOTIMES (I *NUMERIC-ARG*)
      (INSERT-MOVING POINT #/NEWLINE)))
  DIS-TEXT)

(DEFCOM COM-MAKE-ROOM "Insert one or more blank lines after point." ()
  (DOTIMES (I *NUMERIC-ARG*)
     (INSERT (POINT) #/NEWLINE))
  DIS-TEXT)

(DEFCOM COM-SPLIT-LINE "Move rest of current line down vertically.
Inserts a carriage-return and updates indentation of the new line to be below the
old position." ()
  (LET ((POINT (POINT)))
    (MOVE-BP POINT (FORWARD-OVER *BLANKS* POINT))
    (LET ((IND (BP-INDENTATION POINT))
          (BP (COPY-BP POINT)))
      (DOTIMES (I (MAX *NUMERIC-ARG* 1))
        (INSERT-MOVING BP #/NEWLINE))
      (INDENT-LINE BP IND)))
  DIS-TEXT)

(DEFCOM COM-THIS-INDENTATION "Insert new line after this one, indent it to under point.
With arg, indent this line to here." ()
  (LET ((BP1 (FORWARD-OVER *BLANKS* (POINT)))
        (BP2 (IF *NUMERIC-ARG-P* (POINT) (INSERT-MOVING (END-LINE (POINT)) #/NEWLINE))))
    (MOVE-BP (POINT) (INDENT-LINE BP2 (BP-INDENTATION BP1))))
  DIS-TEXT)

(DEFCOM COM-DELETE-INDENTATION "Delete the Return and any indentation at front of line.
Leaves a space in place of them where appropriate.
A numeric argument means move down a line first
 (operating on the end of the current line and start of next)." ()
  (LET ((POINT (POINT)))
    (LET ((LINE (BP-LINE POINT)))
       (COND ((AND *NUMERIC-ARG-P*
                   (NOT (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))))
              (SETQ LINE (LINE-NEXT LINE))))
       (MOVE-BP POINT LINE 0)
       (UNLESS (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
         (DELETE-INTERVAL (END-OF-LINE (LINE-PREVIOUS LINE)) POINT))
       (DELETE-AROUND *BLANKS* POINT)
       (LET* ((CHAR-BEFORE (CHAR-CODE (BP-CHAR-BEFORE POINT)))
              (SYNTAX-BEFORE (LIST-SYNTAX CHAR-BEFORE)))
         (OR (= (LIST-SYNTAX (BP-CHARACTER POINT)) LIST-CLOSE)
             (= SYNTAX-BEFORE LIST-OPEN)
             (AND (= SYNTAX-BEFORE LIST-SINGLE-QUOTE)
                  ;(EQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
                  (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                  )
             (PROGN
               (INSERT-MOVING POINT (IN-CURRENT-FONT #/SP))
               (AND ;(NEQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
                    (not (call-editing-type-function *major-mode* 'lisp-syntax-p nil))
;character lossage
                    (MEMQ CHAR-BEFORE *FILL-EXTRA-SPACE-LIST*)
                    (INSERT-MOVING POINT (IN-CURRENT-FONT #/SP))))))))
  DIS-TEXT)

(DEFCOM COM-DELETE-FORWARD "Delete one or more characters forward." ()
  (LET ((POINT (POINT)))
    (LET ((BP (FORWARD-CHAR POINT *NUMERIC-ARG*)))
      (COND ((NULL BP) (BARF))
            ((EQ (BP-LINE POINT) (BP-LINE BP))
             (MUST-REDISPLAY *WINDOW*
                             DIS-LINE
                             (BP-LINE BP)
                             (MIN (BP-INDEX BP) (BP-INDEX POINT))))
            (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
      (FUNCALL (IF *NUMERIC-ARG-P* #'KILL-INTERVAL #'DELETE-INTERVAL) BP POINT)))
  DIS-NONE)

(DEFCOM COM-RUBOUT "Delete one or more characters backward." ()
  (LET ((POINT (POINT)))
    (LET ((BP (FORWARD-CHAR POINT (- *NUMERIC-ARG*) T)))
      (COND ((EQ (BP-LINE POINT) (BP-LINE BP))
             (MUST-REDISPLAY *WINDOW*
                             DIS-LINE
                             (BP-LINE BP)
                             (MIN (BP-INDEX BP) (BP-INDEX POINT))))
            (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
      (FUNCALL (IF *NUMERIC-ARG-P* #'KILL-INTERVAL #'DELETE-INTERVAL) BP POINT)))
  DIS-NONE)

(DEFCOM COM-KILL-LINE "Kill to end of line, or kill an end of line.
If at end of line aside from possible whitespace, kill to beginning of next line.
Otherwise, kill up to the end of the current line.
With a numeric argument, always kill the specified number of lines." ()
  (LET ((POINT (POINT)))
    (COND ((AND (BP-= POINT (INTERVAL-LAST-BP *INTERVAL*)) (PLUSP *NUMERIC-ARG*))
           (BARF "Attempt to kill past the end of the buffer."))
          (T
           (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
           (COND (*NUMERIC-ARG-P*
                  (KILL-INTERVAL-ARG POINT
                                     (BEG-LINE POINT *NUMERIC-ARG* T)
                                     *NUMERIC-ARG*)
                  DIS-TEXT)
                 ((END-LINE-P (FORWARD-OVER *BLANKS* POINT))
                  (KILL-INTERVAL POINT (BEG-LINE POINT 1 T) T T)
                  DIS-TEXT)
                 (T
                  (KILL-INTERVAL POINT (END-LINE POINT) T T)
                  (VALUES DIS-LINE (BP-LINE POINT) (BP-INDEX POINT))))))))

(DEFCOM COM-CLEAR "Kill to the start of the current line." ()
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (LET ((POINT (POINT)))
    (LET ((BP (BEG-LINE POINT (COND (*NUMERIC-ARG-P* (- *NUMERIC-ARG*))
                                    ((BEG-LINE-P POINT) -1)
                                    (T 0)) T)))
      (KILL-INTERVAL BP POINT NIL NIL)))
  DIS-TEXT)

(DEFCOM COM-SAVE-REGION "Put region on the kill history without deleting it." ()
  (REGION (BP1 BP2)
    (KILL-RING-SAVE-INTERVAL BP1 BP2 T))
  DIS-NONE)

(DEFCOM COM-KILL-REGION "Kill from point to mark.
Killed text is placed on the kill history for retrieval" ()
  (AND (TYPEP *LAST-COMMAND-TYPE* 'HISTORY)                     ;By special case.
       (SETF (WINDOW-MARK-P *WINDOW*) T))
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("kill" BP1 BP2 T)
      (KILL-INTERVAL BP1 BP2 T T T)))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (CLEAN-POINT-PDL *WINDOW*)
  (LET ((PDL (WINDOW-POINT-PDL *WINDOW*)))
    (AND PDL (MOVE-BP (MARK) (CAAR PDL))))
  DIS-TEXT)

(DEFCOM COM-APPEND-NEXT-KILL "Make next kill command append text to previous one." (KM)
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-NONE)

;;; If there was no arg at all, *NUMERIC-ARG-P* is NIL and *NUMERIC-ARG* is 1.
;;; If user just typed -, then *NUMERIC-ARG-P* is :SIGN and *NUMERIC-ARG* is -1.
;;; If numeric arg commands were typed, *NUMERIC-ARG-P* is :DIGITS and
;;; *NUMERIC-ARG* is the number.
;;; Note that - does not toggle negativeness, it turns it on.

(DEFCOM COM-QUADRUPLE-NUMERIC-ARG "Multiply the next command's numeric argument by 4." ()
  (SETQ *NUMERIC-ARG* (* *NUMERIC-ARG* 4)
        *NUMERIC-ARG-P* ':CONTROL-U)
  ':ARGUMENT)

(DEFCOM COM-NUMBERS "part of the next command's numeric argument." ()
  (LET ((FLAG NIL)
        (DIGIT (- (CHAR-CODE *LAST-COMMAND-CHAR*) (CHAR-INT #/0))))
    (COND ((< *NUMERIC-ARG* 0)
           (SETQ FLAG T)
           (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*))))
    (SETQ *NUMERIC-ARG*
          (IF (EQ *NUMERIC-ARG-P* ':DIGITS)
              (+ (* 10. *NUMERIC-ARG*) DIGIT)
              DIGIT))
    (AND FLAG (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*))))
  (SETQ *NUMERIC-ARG-P* ':DIGITS)
  :ARGUMENT)

(DEFCOM COM-NEGATE-NUMERIC-ARG "Negate the next command's numeric argument." ()
  (SETQ *NUMERIC-ARG* (MINUS (ABS *NUMERIC-ARG*))
        *NUMERIC-ARG-P* ':SIGN)
  :ARGUMENT)

(DEFCOM COM-SIMPLE-EXCHANGE-CHARACTERS
        "Interchange the characters before and after the cursor.
With a positive argument it interchanges the characters before and
after the cursor, moves right, and repeats the specified number of
times, dragging the character to the left of the cursor right.  With a
negative argument, it interchanges the two characters to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the characters at point and mark." ()
  (EXCHANGE-SUBR #'FORWARD-CHAR *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-CHARACTERS "Interchange the characters before and after the cursor.
With a positive argument it interchanges the characters before and
after the cursor, moves right, and repeats the specified number of
times, dragging the character to the left of the cursor right.  With a
negative argument, it interchanges the two characters to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the characters at point and mark.
No argument is like an argument of 1, except at the end of a line
the previous two characters are interchanged." ()
  (COND ((AND (NOT *NUMERIC-ARG-P*)
              (EQ (BP-CHARACTER (POINT)) #/NEWLINE))
         (MOVE-BP (POINT) (OR (FORWARD-CHAR (POINT) -1) (BARF)))))
  (EXCHANGE-SUBR #'FORWARD-CHAR *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-WORDS "Interchange the words before and after the cursor.
With a positive argument it interchanges the words before and
after the cursor, moves right, and repeats the specified number of
times, dragging the word to the left of the cursor right.  With a
negative argument, it interchanges the two words to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the words at point and mark." ()
  (EXCHANGE-SUBR #'FORWARD-WORD *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-LINES "Interchange the lines before and after the cursor.
With a positive argument it interchanges the lines before and
after the cursor, moves right, and repeats the specified number of
times, dragging the word to the left of the cursor right.  With a
negative argument, it interchanges the two lines to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the lines at point and mark." ()
  (EXCHANGE-SUBR #'FORWARD-LINE *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-EXCHANGE-SEXPS "Interchange the S-expressions before and after the cursor.
With a positive argument it interchanges the S-expressions before and
after the cursor, moves right, and repeats the specified number of
times, dragging the S-expression to the left of the cursor right.  With a
negative argument, it interchanges the two S-expressions to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it interchanges the S-expressions at point and mark." ()
  (EXCHANGE-SUBR #'FORWARD-SEXP *NUMERIC-ARG*)
  DIS-TEXT)

;;;This is arranged so weirdly because it runs out of local variables as just one function.
(DEFUN EXCHANGE-SUBR (FN N &AUX BUF1 BUF2)
  "Perform an /"exchange/" operation, such as M-T or C-M-T.
FN is a function to move forward over a syntactic unit,
and N is the numeric argument."
  (COND ((PLUSP N)
         (EXCHANGE-SUBR-1 FN N))
        ((MINUSP N)
         (EXCHANGE-SUBR-2 FN N))
        (T ;0 argument -- exchange at point & mark
         (REGION (BP1 BP2)
           (WITH-BP (BP1 (OR (FUNCALL FN BP1 1) (BARF)) ':NORMAL)
             (OR (SETQ BP1 (FUNCALL FN BP1 -1)) (BARF))
             (WITH-BP (BP2 (OR (FUNCALL FN BP2 1) (BARF)) ':NORMAL)
               (WITH-BP (BP3 (OR (FUNCALL FN BP2 -1) (BARF)) ':NORMAL)
                 (WITH-BP (BP4 (OR (FUNCALL FN BP1 1) (BARF)) ':NORMAL)
                   (LET ((BP-< (BP-< BP1 BP3)))
                     (WITH-UNDO-SAVE ("Exchange" (IF BP-< BP1 BP3) (IF BP-< BP2 BP4) T)
                       (SETQ BUF1 (COPY-INTERVAL BP3 BP2 T)
                             BUF2 (COPY-INTERVAL BP1 BP4 T))
                       (DELETE-INTERVAL BP3 BP2 T)
                       (MOVE-BP (POINT) (INSERT-INTERVAL BP3 BUF2))
                       (MOVE-BP (MARK) (INSERT-INTERVAL BP4 BUF1))
                       (DELETE-INTERVAL BP1 BP4 T))))))))
           (SETQ *MARK-STAYS* T))))

;;;+ve argument
(DEFUN EXCHANGE-SUBR-1 (FN N &AUX BP1 BP0 BUF1 BUF2)
  (OR (SETQ BP1 (FUNCALL FN (POINT) 1)) (BARF))
  (OR (SETQ BP0 (FUNCALL FN BP1 -2)) (BARF))
  (OR (SETQ BP1 (FUNCALL FN BP0 1)) (BARF))
  (MOVE-BP (POINT) BP1)
  (WITH-UNDO-SAVE ("Exchange" BP0 (OR (FUNCALL FN BP1 N) (BARF)) T)
    (DOTIMES (I N)
      (WITH-BP (BP1 (POINT) ':NORMAL)
        (WITH-BP (BP2 (OR (FUNCALL FN BP1 1) (BARF)) ':NORMAL)
          (WITH-BP (BP3 (OR (FUNCALL FN BP2 -1) (BARF)) ':NORMAL)
            (WITH-BP (BP4 (OR (FUNCALL FN BP1 -1) (BARF)) ':NORMAL)
              (SETQ BUF1 (COPY-INTERVAL BP3 BP2 T)
                    BUF2 (COPY-INTERVAL BP4 BP1 T))
              (DELETE-INTERVAL BP3 BP2 T)
              (MOVE-BP (POINT) (INSERT-INTERVAL BP3 BUF2))
              (INSERT-INTERVAL BP1 BUF1)
              (DELETE-INTERVAL BP4 BP1 T))))))))

;;;-ve argument
(DEFUN EXCHANGE-SUBR-2 (FN N &AUX BP1 BUF1 BUF2)
  (OR (SETQ BP1 (FUNCALL FN (POINT) -1)) (BARF))
  (OR (SETQ BP1 (FUNCALL FN BP1 1)) (BARF))
  (MOVE-BP (POINT) BP1)
  (WITH-UNDO-SAVE ("Exchange" (OR (FUNCALL FN BP1 (1- N)) (BARF)) BP1 T)
    (DO ((I 0 (1- I))) (( I N))
        (WITH-BP (BP1 (POINT) ':NORMAL)
          (WITH-BP (BP2 (OR (FUNCALL FN BP1 -2) (BARF)) ':NORMAL)
            (WITH-BP (BP3 (OR (FUNCALL FN BP2 1) (BARF)) ':NORMAL)
              (WITH-BP (BP4 (OR (FUNCALL FN BP1 -1) (BARF)) ':NORMAL)
                (SETQ BUF1 (COPY-INTERVAL BP2 BP3 T)
                      BUF2 (COPY-INTERVAL BP4 BP1 T))
                (DELETE-INTERVAL BP4 BP1 T)
                (INSERT-INTERVAL BP4 BUF1)
                (MOVE-BP (POINT) (INSERT-INTERVAL BP3 BUF2))
                (DELETE-INTERVAL BP2 BP3 T))))))))

(DEFCOM COM-EXCHANGE-REGIONS "Exchange region delimited by point and last three marks." (KM)
  (OR (WINDOW-MARK-P *WINDOW*) (BARF "There is no region"))
  (LET (BP1 BP2 BP3 BP4)
    (OR (BP-= (MARK) (CAAR (WINDOW-POINT-PDL *WINDOW*)))
        (BARF "Mark not at the same place as top of point pdl"))
    (SETQ BP1 (POINT)
          BP2 (POINT-PDL-POP *WINDOW*)
          BP3 (POINT-PDL-POP *WINDOW*)
          BP4 (POINT-PDL-POP *WINDOW*))
    (LET ((LIST (LIST BP1 BP2 BP3 BP4)))
      (SETQ LIST (SORT LIST #'(LAMBDA (BP1 BP2)
                                (AND (EQ (BP-TOP-LEVEL-NODE BP1) (BP-TOP-LEVEL-NODE BP2))
                                     (BP-< BP1 BP2)))))
      (SETQ BP1 (FIRST LIST)
            BP2 (SECOND LIST)
            BP3 (THIRD LIST)
            BP4 (FOURTH LIST)))
    (OR (AND (EQ (BP-TOP-LEVEL-NODE BP1) (BP-TOP-LEVEL-NODE BP2))
             (EQ (BP-TOP-LEVEL-NODE BP3) (BP-TOP-LEVEL-NODE BP4)))
        (BARF "Regions are not both within single buffers"))
    ;; copy-bp's to get around macroexpansion/movingbp screw
    (WITH-UNDO-SAVE ("Exchange" (COPY-BP BP1) (COPY-BP BP4) T)
      (WITH-BP (NBP2 (INSERT-INTERVAL BP2 BP3 BP4 T) ':NORMAL)
        (WITH-BP (NBP4 (INSERT-INTERVAL BP4 BP1 BP2 T) ':NORMAL)
          (DELETE-INTERVAL BP1 BP2 T)
          (DELETE-INTERVAL BP3 BP4 T)
          (POINT-PDL-PUSH BP1 *WINDOW*)
          (POINT-PDL-PUSH NBP2 *WINDOW*)
          (POINT-PDL-PUSH BP3 *WINDOW*)
          (MOVE-BP (MARK) BP3)
          (MOVE-BP (POINT) NBP4)))))
  DIS-TEXT)

(DEFUN REVERSE-SUBR (FN N &OPTIONAL (BP (POINT)) BP-LIST)
  (AND (MINUSP N)
       (SETQ BP (FUNCALL FN BP N)
             N (- N)))
  (UNWIND-PROTECT
    (LET (END-BP)
      (DO ((I 0 (1+ I))
           (START-BP BP END-BP))
          (( I N))
        (SETQ END-BP (OR (FUNCALL FN START-BP 1) (BARF))
              START-BP (OR (FUNCALL FN END-BP -1) (BARF)))
        (PUSH (LIST (COPY-BP START-BP ':MOVES) (COPY-BP END-BP ':NORMAL)) BP-LIST))
      (WITH-UNDO-SAVE ("Reverse" BP END-BP T)
        (DO ((I 0 (1+ I))
             (N (TRUNCATE N 2))
             (LIST-FROM-THE-RIGHT BP-LIST (CDR LIST-FROM-THE-RIGHT))
             (LIST-FROM-THE-LEFT (REVERSE BP-LIST) (CDR LIST-FROM-THE-LEFT))
             (RIGHT-START-BP) (RIGHT-END-BP)
             (LEFT-START-BP) (LEFT-END-BP))
            (( I N))
          (SETQ LEFT-START-BP (CAAR LIST-FROM-THE-LEFT)
                LEFT-END-BP (CADAR LIST-FROM-THE-LEFT))
          (SETQ RIGHT-START-BP (CAAR LIST-FROM-THE-RIGHT)
                RIGHT-END-BP (CADAR LIST-FROM-THE-RIGHT))
          (INSERT-INTERVAL LEFT-START-BP
                           (PROG1 (COPY-INTERVAL RIGHT-START-BP RIGHT-END-BP T)
                                  (DELETE-INTERVAL RIGHT-START-BP RIGHT-END-BP T)))
          (INSERT-INTERVAL RIGHT-START-BP
                           (PROG1 (COPY-INTERVAL LEFT-START-BP LEFT-END-BP T)
                                  (DELETE-INTERVAL LEFT-START-BP LEFT-END-BP T))))))
    (DO ((BPS BP-LIST (CDR BPS)))
        ((NULL BPS))
      (FLUSH-BP (CAAR BPS))
      (FLUSH-BP (CADAR BPS)))))

(DEFCOM COM-REVERSE-LINES "Reverse the order of the specified number of lines" ()
  (REVERSE-SUBR 'FORWARD-LINE *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-REVERSE-FOLLOWING-LIST "Reverse the elements of the list after point" ()
  (LET* ((BP (POINT))
         (COUNT (OR (COUNT-LIST-ELEMENTS BP) (BARF))))
    (REVERSE-SUBR #'FORWARD-SEXP COUNT (FORWARD-LIST BP 1 NIL -1 T)))
  DIS-TEXT)

(DEFUN KILL-COMMAND-INTERNAL (FUNCTION ARG &AUX (POINT (POINT)))
  (LET* ((OTHER-END (OR (FUNCALL FUNCTION POINT ARG) (BARF)))
         (SAME-LINE-P (EQ (BP-LINE POINT) (BP-LINE OTHER-END))))
    (KILL-INTERVAL-ARG POINT OTHER-END ARG)
    (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
    (MOVE-BP (MARK) POINT)
    (COND ((AND SAME-LINE-P
                (= (BP-INDEX POINT) (LINE-LENGTH (BP-LINE POINT))))
           (VALUES DIS-LINE (BP-LINE POINT) (BP-INDEX POINT)))
          (T DIS-TEXT))))

(DEFCOM COM-FORWARD-WORD "Move one or more words forward." (KM)
  (MOVE-BP (POINT)
           (OR (FORWARD-WORD (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-WORD "Move one or more words backward." (KM)
  (MOVE-BP (POINT)
           (OR (FORWARD-WORD (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-KILL-WORD "Kill one or more words forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-WORD *NUMERIC-ARG*))

(DEFCOM COM-BACKWARD-KILL-WORD "Kill one or more words backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-WORD (- *NUMERIC-ARG*)))

(DEFCOM COM-MARK-WORD "Set mark one or more words from point." (SM)
  (MOVE-BP (MARK) (OR (FORWARD-WORD (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-SEXP "Move one or more s-expressions forward." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-SEXP (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-SEXP-NO-UP "Move forward one or more s-expressions,
but never over an unbalanced ).  Useful in keyboard macros, e.g." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-SEXP (POINT) *NUMERIC-ARG* NIL 0 NIL T T) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-SEXP-NO-UP "Move backward one or more s-expressions,
but never over an unbalanced (.  Useful in keyboard macros, e.g." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-SEXP (POINT) (- *NUMERIC-ARG*) NIL 0 NIL T T) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-LIST "Move one or more lists forward." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-LIST (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-SEXP "Move one or more s-expressions backward." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-SEXP (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-LIST "Move one or more lists backwards." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-LIST (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-KILL-SEXP "Kill one or more s-expressions forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP *NUMERIC-ARG*))

(DEFCOM COM-KILL-SEXP-NO-UP "Kill one or more s-expressions forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP-NO-UP *NUMERIC-ARG*))

(DEFCOM COM-BACKWARD-KILL-SEXP "Kill one or more s-expressions backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP (- *NUMERIC-ARG*)))

(DEFCOM COM-BACKWARD-KILL-SEXP-NO-UP "Kill one or more s-expressions backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-SEXP-NO-UP (- *NUMERIC-ARG*)))

(DEFCOM COM-MARK-SEXP "Set mark one or more s-expressions from point." (SM)
  (MOVE-BP (MARK) (OR (FORWARD-SEXP (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-FORWARD-UP-LIST "Move up one level of list structure, forward.
Also, if called inside of a string, moves up out of that string." (KM)
  (LET ((BP (FORWARD-UP-LIST-OR-STRING (POINT) *NUMERIC-ARG*)))
    (OR BP (BARF))
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-BACKWARD-UP-LIST "Move up one level of list structure, backward.
Also, if called inside of a string, moves back up out of that string." (KM)
  (LET ((BP (FORWARD-UP-LIST-OR-STRING (POINT) (- *NUMERIC-ARG*))))
    (OR BP (BARF))
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-BEGINNING-OF-DEFUN "Go to the beginning of the current defun." (KM)
  (LET ((BP (OR (FORWARD-DEFUN (POINT) (- *NUMERIC-ARG*)) (BARF))))
    (POINT-PDL-PUSH (POINT) *WINDOW*)
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-END-OF-DEFUN "Go to the end of the current defun." (KM)
  (LET ((BP (FORWARD-DEFUN (POINT) -1 T)))              ;Go to front of defun.
    (OR (SETQ BP (FORWARD-SEXP BP)) (BARF))             ; and forward over it.
    (SETQ BP (BEG-LINE BP 1 T))
    (COND ((OR (BP-< BP (POINT))                      ;If we were between defuns,
               (AND (PLUSP *NUMERIC-ARG*) (BP-= BP (POINT))))
           (SETQ BP (END-LINE BP -1 T))
           (OR (SETQ BP (FORWARD-SEXP (FORWARD-DEFUN BP 1 T)))
               (BARF))
           (SETQ BP (BEG-LINE BP 1 T))))              ; then move ahead another.
    (POINT-PDL-PUSH (POINT) *WINDOW*)
    (OR (= *NUMERIC-ARG* 1)
        (SETQ BP (BEG-LINE (FORWARD-SEXP (FORWARD-DEFUN BP (1- *NUMERIC-ARG*) T) 1 T) 1 T)))
    (MOVE-BP (POINT) BP))
  DIS-BPS)

(DEFCOM COM-DOWN-LIST "Move down one or more levels of list structure." (KM)
  (MOVE-BP (POINT)
           (OR (FORWARD-LIST (POINT) 1 NIL (- *NUMERIC-ARG*) T) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-DOWN-LIST
        "Move down one or more levels of list structure, backward." (KM)
  (MOVE-BP (POINT)
           (OR (FORWARD-LIST (POINT) -1 NIL (- *NUMERIC-ARG*) T T) (BARF)))
  DIS-BPS)

(DEFCOM COM-CLOSE-DEFUN "Insert enough close parentheses to terminate this defun." ()
  (LET ((START-BP (FORWARD-DEFUN (POINT) -1)))
    (MULTIPLE-VALUE-BIND (STRING SLASH COMMENT)
        (LISP-BP-SYNTACTIC-CONTEXT (POINT) START-BP)
      (IF STRING (BARF "You are inside a string."))
      (IF SLASH (BARF "The next character will be quoted."))
      (IF COMMENT (BARF "You are inside a comment.")))
    (LET (LINE IN-STRING DEPTH1)
        (MULTIPLE-VALUE (LINE IN-STRING DEPTH1)
          (LISP-FORWARD-LIST-AUX (BP-LINE START-BP) NIL 0 (BP-LINE (POINT))))
        (IF (EQ LINE (BP-LINE (POINT)))
            (LET ((DEPTH2 (LISP-PARSE-LINE LINE IN-STRING 0 (BP-INDEX (POINT)))))
              (IF (CONSP DEPTH2) (SETQ DEPTH2 (CAR DEPTH2)))
              (DOTIMES (I (+ DEPTH1 DEPTH2))
                (INSERT-MOVING (POINT) ")"))))))
  DIS-TEXT)

(DEFCOM COM-SHOW-LIST-START "Displays the start of the list which the point lies after." (KM)
  (LET ((BP (FORWARD-LIST (POINT) -1 NIL 0 NIL T)))
    (COND ((NULL BP) (BARF "No list found before point."))
          (T (LET* ((END-BP (FORWARD-SEXP BP 1 NIL 0))
                    (CLOSE (BP-CHAR-BEFORE END-BP))
                    (BALANCE-LENGTH (MAX 1 (- (FUNCALL *TYPEIN-WINDOW* ':SIZE-IN-CHARACTERS)
                                              25.))))
               (DO ((INDEX (BP-INDEX BP) (1+ INDEX))
                    (OPEN))
                   ((= (LIST-SYNTAX (SETQ OPEN (INT-CHAR (CHAR-CODE
                                                           (CLI:AREF (BP-LINE BP) INDEX)))))
                       LIST-OPEN)
                    (TYPEIN-LINE "") ;clear line, since typein-line-more used if paren ok
                    (UNLESS (= (SECOND (ASSQ OPEN *MATCHING-DELIMITER-LIST*)) CLOSE)
                      (PROGN (BEEP) (TYPEIN-LINE "Non-matching parenthesis.~%")))))
               (DO ((STRING "" (STRING-APPEND STRING (IF (EQ CH #\RETURN) "   " CH)))
                    (CH (BP-CH-CHAR BP)
                        (COND ((MEMQ (BP-CH-CHAR BP) *WHITESPACE-CHARS*)
                               (SELECTOR CH STRING-EQUAL
                                 ((#\SP #\RETURN #\TAB "") "")
                                 (T (BP-CH-CHAR BP))))
                              (T (BP-CH-CHAR BP))))
                    (BP (IBP BP) (IBP BP)))
                   ((OR (> (STRING-LENGTH STRING) BALANCE-LENGTH)
                        (BP-= BP END-BP))
                    (TYPEIN-LINE-MORE "~a ... balances the last closeparen"
                                      (SUBSTRING STRING
                                                 0 (MIN (STRING-LENGTH STRING)
                                                        BALANCE-LENGTH)))))))))
  DIS-BPS)

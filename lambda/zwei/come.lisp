;;; Zwei commands, see ZWEI;COMA for comments -*- Mode:LISP; Package:ZWEI; Base:8 -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Various Quantities.

(DEFCOM COM-VARIOUS-QUANTITIES "Performs some operation on some textual unit.
First character following is operation:
  F forward, B backward, K kill, R rubout, X transpose, @ mark region, U uppercase,
  L lowercase, S save, C copy, Z reverse.
Second character following is quantity type:
  C character, W word, S sentence, P paragraph, L line, A atom, - S-expression,
  ( or ) list, D defun, Clear-Screen page separated by Control-Ls, H buffer.
Numeric arguments are obeyed.  ? for help." ()
  (LET (CH MODE-NAME MODE QUANTITY)
    (SETQ CH (INPUT-WITH-PROMPTS *STANDARD-INPUT* ':TYI))
    (PROG ()
       GET-A-MODE
          (SELECTQ CH
            (#/?
             (PRINT-DOC ':FULL 'COM-VARIOUS-QUANTITIES)
             (FORMAT *QUERY-IO* "~&Type the OPERATION (F, B, K, R, X, @, U, L, S, C or Z)")
             (DISCARD-LAST-PROMPT)
             (PRINT-PROMPTS)
             (SETQ CH (CHAR-UPCASE (INPUT-WITH-PROMPTS *STANDARD-INPUT* ':TYI)))
             (GO GET-A-MODE))
            (#/F
             (SETQ MODE-NAME "Forward"
                   MODE 'COM-QUANTITY-FORWARD))
            (#/B
             (SETQ MODE-NAME "Backward"
                   MODE 'COM-QUANTITY-BACKWARD))
            ((#/D #/K)
             (SETQ MODE-NAME "Delete"
                   MODE 'COM-QUANTITY-DELETE))
            (#/R
             (SETQ MODE-NAME "Rubout"
                   MODE 'COM-QUANTITY-RUBOUT))
            ((#/X #/T)
             (SETQ MODE-NAME "Exchange"
                   MODE 'COM-QUANTITY-TWIDDLE))
            (#/@
             (SETQ MODE-NAME "Mark"
                   MODE 'COM-QUANTITY-MARK))
            (#/U
             (SETQ MODE-NAME "Uppercase"
                   MODE 'COM-QUANTITY-UPPERCASE))
            (#/L
             (SETQ MODE-NAME "Lowercase"
                   MODE 'COM-QUANTITY-LOWERCASE))
            (#/S
             (SETQ MODE-NAME "Save"
                   MODE 'COM-QUANTITY-SAVE))
            (#/C
             (SETQ MODE-NAME "Copy"
                   MODE 'COM-QUANTITY-COPY))
            (#/Z
             (SETQ MODE-NAME "Reverse"
                   MODE 'COM-QUANTITY-REVERSE))
            (OTHERWISE
             (BARF "Invalid quantity operation")))
       GET-A-QUANTITY
          (SETQ CH (CHAR-UPCASE (INPUT-WITH-PROMPTS *STANDARD-INPUT* ':TYI)))
          (SELECTQ CH
            (#/?
             (PRINT-DOC ':FULL 'COM-VARIOUS-QUANTITIES)
             (FORMAT *QUERY-IO* "~&Type quantity name (C, W, S, P, A, L, -, ( or ), D, ~C, or H)"
                     #/CLEAR-SCREEN)
             (DISCARD-LAST-PROMPT)
             (PRINT-PROMPTS)
             (GO GET-A-QUANTITY))
            (#/C
             (SETQ MODE-NAME "Character"
                   QUANTITY 'FORWARD-CHAR))
            (#/W
             (SETQ MODE-NAME "Word"
                   QUANTITY 'FORWARD-WORD))
            (#/A
             (SETQ MODE-NAME "Atom"
                   QUANTITY 'FORWARD-ATOM))
            (#/S
             (SETQ MODE-NAME "Sentence"
                   QUANTITY 'FORWARD-SENTENCE))
            (#/P
             (SETQ MODE-NAME "Paragraph"
                   QUANTITY 'FORWARD-PARAGRAPH))
            (#/L
             (SETQ MODE-NAME "Line"
                   QUANTITY 'FORWARD-LINE))
            (#/-
             (SETQ MODE-NAME "S-Expression"
                   QUANTITY 'FORWARD-SEXP))
            ((#/( #/))
             (SETQ MODE-NAME "List"
                   QUANTITY 'FORWARD-LIST))
            (#/D
             (SETQ MODE-NAME "Defun"
                   QUANTITY 'FORWARD-DEFUN))
            (#/FF
             (SETQ MODE-NAME "Page"
                   QUANTITY 'FORWARD-PAGE))
            (#/H
             (SETQ MODE-NAME "Buffer"
                   QUANTITY 'FORWARD-BUFFER))
            (OTHERWISE
             (BARF "Invalid quantity type")))
          (LET ((*QUANTITY-MODE* QUANTITY))
            (FUNCALL MODE)))))

(DEFCOM COM-QUANTITY-FORWARD "Move forward according to the current quantity mode." (KM)
  (MOVE-BP (POINT) (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-QUANTITY-BACKWARD "Move backward according to the current quantity mode." (KM)
  (MOVE-BP (POINT) (OR (FUNCALL *QUANTITY-MODE* (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-QUANTITY-DELETE "Kill forward according to the current quantity mode." ()
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (KILL-INTERVAL (POINT)
                 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*)
                     (BARF))
                 NIL
                 T)
  DIS-TEXT)

(DEFCOM COM-QUANTITY-RUBOUT "Kill backward according to the current quantity mode." ()
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  (KILL-INTERVAL (OR (FUNCALL *QUANTITY-MODE* (POINT) (- *NUMERIC-ARG*))
                     (BARF))
                 (POINT)
                 NIL
                 NIL)
  DIS-TEXT)

(DEFCOM COM-QUANTITY-TWIDDLE "Exchange things according to the current quantity mode." ()
  (EXCHANGE-SUBR *QUANTITY-MODE* *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-QUANTITY-REVERSE "Reverse things according to the current quantity mode." ()
  (REVERSE-SUBR *QUANTITY-MODE* *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-QUANTITY-MARK "Mark according to the current quantity mode." (SM)
  (LET (BP1 BP2)
    (OR (SETQ BP1 (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*))
        (BARF))
    (OR (SETQ BP2 (FUNCALL *QUANTITY-MODE* BP1 (MINUS *NUMERIC-ARG*)))
        (BARF))
    (AND (MINUSP *NUMERIC-ARG*)
         (SETQ BP2 (PROG1 BP1 (SETQ BP1 BP2))))
    (MOVE-BP (POINT) BP1)
    (MOVE-BP (MARK) BP2))
  DIS-BPS)

(DEFCOM COM-QUANTITY-UPPERCASE "Uppercase according to the current quantity mode." ()
  (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
    (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
      (WITH-UNDO-SAVE ("Upcase" BP1 BP2 NIL)
        (UPCASE-INTERVAL BP1 BP2))
      (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))))
  DIS-TEXT)

(DEFCOM COM-QUANTITY-LOWERCASE "Lowercase according to the current quantity mode." ()
  (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
    (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
      (WITH-UNDO-SAVE ("Downcase" BP1 BP2 NIL)
        (DOWNCASE-INTERVAL BP1 BP2))
      (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1))))
  DIS-TEXT)

(DEFCOM COM-QUANTITY-SAVE "Save on kill ring according to the current quantity mode." ()
  (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
    (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
      (KILL-RING-PUSH (COPY-INTERVAL BP1 BP2))
      (MOVE-BP (POINT) BP1)))
  DIS-TEXT)

(DEFCOM COM-QUANTITY-COPY "Insert a copy according to the current quantity mode." ()
  (LET ((BP1 (OR (FUNCALL *QUANTITY-MODE* (POINT) *NUMERIC-ARG*) (BARF))))
    (LET ((BP2 (OR (FUNCALL *QUANTITY-MODE* BP1 (- *NUMERIC-ARG*)) (BARF))))
      (MOVE-BP (POINT)
               (INSERT-INTERVAL BP2 (COPY-INTERVAL BP1 BP2)))))
  DIS-TEXT)

(DEFCOM COM-PREVIOUS-PAGE "Move to the previous page" (KM)
  (MOVE-BP (POINT) (FORWARD-PAGE (POINT) (MINUS *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-NEXT-PAGE "Move to the next page" (KM)
  (MOVE-BP (POINT) (FORWARD-PAGE (POINT) *NUMERIC-ARG* T))
  DIS-BPS)

(DEFCOM COM-MARK-WHOLE "Put mark at beginning of buffer and point end,
or with a numeric argument, vice versa" (SM)
  (LET ((BP1 (POINT)) (BP2 (MARK)))
    (AND *NUMERIC-ARG-P* (PSETQ BP1 BP2 BP2 BP1))
    (MOVE-BP BP1 (INTERVAL-LAST-BP *INTERVAL*))
    (MOVE-BP BP2 (INTERVAL-FIRST-BP *INTERVAL*)))
  DIS-BPS)

(DEFCOM COM-MARK-DEFUN "Put point and mark around current defun." ()
  (LET ((INT (DEFUN-INTERVAL (POINT) *NUMERIC-ARG* NIL T T)))
    (OR INT (BARF))
    (SETF (WINDOW-MARK-P *WINDOW*) T)
    (SETQ *MARK-STAYS* T)
    (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
    (MOVE-BP (POINT) (INTERVAL-FIRST-BP INT))
    (MOVE-BP (MARK) (INTERVAL-LAST-BP INT)))
  DIS-BPS)

(DEFCOM COM-REPOSITION-WINDOW "Try to get all of current defun in the window.
If function beginning is on the screen,
 scrolls down to bring the end onto the screen.
If function beginning is off screen, scrolls up so beginning appears,
 but will not push point off the bottom.
If function beginning is at the top of the screen,
 tries omitting or including the comments before it.
If function is entirely on screen, positions it at the top
 (or, with numeric arg, at the bottom) of the screen." (KM)
  (LET ((POINT (POINT))
        (SHEET (WINDOW-SHEET *WINDOW*))
        (N-PLINES (WINDOW-N-PLINES *WINDOW*))
        (INT (DEFUN-INTERVAL (POINT) 1 T T))
        START-BP END-BP
        RECENTER-BP)
    (COND ((NOT (NULL INT))
           (SETQ START-BP (INTERVAL-FIRST-BP INT)
                 END-BP (INTERVAL-LAST-BP INT))
           ;; Don't include the blank line after the defun
           (AND (ZEROP (BP-INDEX END-BP)) (SETQ END-BP (END-LINE END-BP -1 T)))
           (COND ((AND (PLINE-OF-POINT T *WINDOW* START-BP) ;If start of defun on the screen
                       (NULL (PLINE-OF-POINT T *WINDOW* END-BP))        ;and not bottom
                       (MULTIPLE-VALUE-BIND (LINE INDEX)
                           (PUT-POINT-AT-PLINE SHEET (BP-LINE END-BP) (BP-INDEX END-BP)
                                               (1- N-PLINES) (INTERVAL-FIRST-BP *INTERVAL*)
                                               (INTERVAL-LAST-BP *INTERVAL*))
                         (SETQ RECENTER-BP (CREATE-BP LINE INDEX))
                         ;; And can fit bottom of the defun on as well
                         ;; then start at the top of the function.
                         (NOT (BP-< START-BP RECENTER-BP)))))
                 ((BP-< START-BP
                        (SETQ RECENTER-BP (MULTIPLE-VALUE-BIND (LINE INDEX)
                                               (PUT-POINT-AT-PLINE SHEET (BP-LINE POINT)
                                                  (BP-INDEX POINT) (1- N-PLINES)
                                                  (INTERVAL-FIRST-BP *INTERVAL*)
                                                  (INTERVAL-LAST-BP *INTERVAL*))
                                            (CREATE-BP LINE INDEX))))
                  ;; If displaying from the start of the defun would push point off
                  ;; the bottom, complain, and bring in as much as possible anyway.
                  (BEEP))
                 ;; Start of defun thru point fits on the screen.
                 ((AND *NUMERIC-ARG-P* (PLINE-OF-POINT T *WINDOW* END-BP))
                  ;; If numeric arg, and end of function is on screen,
                  ;; try putting end of function at bottom.
                  (MULTIPLE-VALUE-BIND (LINE INDEX)
                      (PUT-POINT-AT-PLINE SHEET (BP-LINE END-BP) (BP-INDEX END-BP)
                                          (1- N-PLINES) (INTERVAL-FIRST-BP *INTERVAL*)
                                          (INTERVAL-LAST-BP *INTERVAL*))
                         (SETQ RECENTER-BP (CREATE-BP LINE INDEX))))
                 (T
                  ;; If already at the default place, try omitting comments above the defun.
                  (AND (BP-= (WINDOW-START-BP *WINDOW*) START-BP)
                       (SETQ START-BP (INTERVAL-FIRST-BP (DEFUN-INTERVAL (POINT) 1 T NIL))))
                  (SETQ RECENTER-BP START-BP)))
           (RECENTER-WINDOW *WINDOW* ':START RECENTER-BP))
          (T (BARF "no defun here")))
    DIS-NONE))

(DEFCOM COM-UPCASE-DIGIT "Up-shift the previous digit on this or the previous line." ()
  (LET ((BP (COPY-BP (POINT))))
    (RCHARMAP (BP (BEG-LINE (POINT) -1 T) NIL)
      (COND ((MEMQ (RCHARMAP-CH-CHAR) '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9))
             (RCHARMAP-SET-CHAR (LET* ((CHAR (RCHARMAP-CHAR))
                                       (FONT (LDB %%CH-FONT CHAR))
                                       (CH-CHAR (LDB %%CH-CHAR CHAR)))
                                  (DPB FONT %%CH-FONT (SHIFT-CHARACTER CH-CHAR))))
             (RCHARMAP-RETURN NIL)))))
  DIS-TEXT)

(DEFUN SHIFT-CHARACTER (CHAR)
  "Return the character above CHAR on the same keyboard key."
  (DOTIMES (I 200)
    (AND (= CHAR (AREF SI:KBD-NEW-TABLE 0 I))
         (RETURN (AREF SI:KBD-NEW-TABLE 1 I)))))

;Now that all windows record their input, this is a no-op (starting in system 87).
(DEFUN MAKE-RECORDING-STREAM (STREAM &REST IGNORE)
  STREAM)

(DEFCOM COM-WHAT-LOSSAGE "What commands did I type to cause this lossage?
Prints out descriptions of the last sixty characters typed on the keyboard." (KM)
  (COND ((NOT (MEMQ ':PLAYBACK (SEND *STANDARD-INPUT* ':WHICH-OPERATIONS)))
         (BARF "Your input was not being recorded; sorry."))
        (T (LET ((A (SEND *STANDARD-INPUT* ':PLAYBACK))
                 (WIDTH (OR (SEND *STANDARD-OUTPUT* ':SEND-IF-HANDLES ':SIZE-IN-CHARACTERS)
                            95.)))
             (LET ((P (ARRAY-LEADER A 1))
                   (L (ARRAY-LEADER A 0)))
               (DO ((I (\ (1+ P) L) (\ (1+ I) L))
                    (J 0 (1+ J)))
                   (( J L))
                 (LET ((CH (AREF A I)))
                   (AND CH (NOT (LISTP CH))
                        (FORMAT:BREAKLINE WIDTH NIL
                          (FORMAT:OCHAR CH ':EDITOR)
                          " "))))))
           (SEND *STANDARD-OUTPUT* ':FRESH-LINE)))
  DIS-NONE)

(DEFCOM COM-EXIT-CONTROL-R "Exits from a recursive edit" ()
  (*THROW 'EXIT-CONTROL-R NIL))

(DEFCOM COM-QUIT "Return from the top-level edit" ()
  (*THROW 'EXIT-TOP-LEVEL NIL))

;; Copied from LAD: RELEASE-3.ZWEI; COME.LISP#140 on 2-Oct-86 02:58:06
(DEFCOM COM-ABORT-AT-TOP-LEVEL
        "Abort a command that is unfinished or reading arguments.
Aborts minibuffers and recursive edits, and things like C-X M and DIRED.

Actually, this particular definition is the one used at top level only,
and it does nothing except abort any keyboard macro being defined.
When you are actually typing the arguments to a command,
the Abort key has a different definition.

A special case also occurs when a menu-driven DIRED frame is current. ABORT
then returns to the previous environment, by analogy to the replacement of
a non-menu-driven DIRED buffer with the previous buffer."
        ()
  (if (and (boundp 'zwei:*dired-constraint-frame*)
           (typep (car (send tv:main-screen :exposed-inferiors)) 'zwei:dired-constraint-frame))
      (com-dired-abort)

    (IF (WINDOW-MARK-P *WINDOW*)
        (SETQ *MARK-STAYS* NIL)
      (BARF (IF (MEMQ (SEND *STANDARD-INPUT* ':SEND-IF-HANDLES ':MACRO-LEVEL)
                      '(0 NIL))
                "Already at top level."
              "Aborting definition of keyboard macro."))))
  DIS-NONE)

(DEFCOM COM-BREAK "Enter a lisp break loop" ()
  (LET ((*INSIDE-BREAK* T))
    (BREAK "ZMACS"))
  (SEND *STANDARD-OUTPUT* ':MAKE-COMPLETE)
  DIS-NONE)

; TAB TO TAB STOP stuff.
;This should really be something hairy with the mouse...
; A "tab stop buffer" has two lines: one to indicate characters to fill
;with, and the second to indicate where the tab stops are.  In the second line
;colons and periods are the only significant characters.  Everything else
;is ignored.  If there is a :, fill with spaces, else with contents of
;the first line.  I dont think this can work reasonably with variable
;width fonts very well, so the initial version, at least, will assume
;that you are using only one fixed width font.

(DEFUN INITIALIZE-TAB-STOP-BUFFER ()
  (LET ((*BATCH-UNDO-SAVE* T))
    (SETQ *TAB-STOP-BUFFER* (CREATE-INTERVAL NIL NIL T))
    (INSERT (INTERVAL-FIRST-BP *TAB-STOP-BUFFER*)
            "
        :       :       :       :       :       :       :       :       :       :       :       :")
    NIL))

(DEFCOM COM-EDIT-TAB-STOPS "Edit the tab-stop buffer.
In the second line of the tab-stop buffer,
a colon indicates a tab stop to move out to with spaces.
A period indicates a tab stop to space out to
by copying the text from the corresponding columns
in the first line of the tab stop buffer."
        ()
  (RECURSIVE-EDIT *TAB-STOP-BUFFER* "Edit tab stops")
  DIS-ALL)

(DEFCOM COM-TAB-TO-TAB-STOP "Tab to fixed column as specified by the tab-stop buffer.
Use the Edit Tab Stops command to edit the tab-stop settings." ()
  (LET* ((POINT (POINT))
         (GOAL (BP-VIRTUAL-INDENTATION POINT))
         (L2 (LINE-NEXT (BP-LINE (INTERVAL-FIRST-BP *TAB-STOP-BUFFER*))))
         (CHAR-POS))
    (MULTIPLE-VALUE (NIL CHAR-POS)
      (TV:SHEET-STRING-LENGTH (WINDOW-SHEET *WINDOW*) L2 0 NIL GOAL))
    (AND CHAR-POS
         (SETQ GOAL (DO ((I 0 (1+ I))
                         (CP CHAR-POS))
                        (( I *NUMERIC-ARG*) CP)
                      (SETQ CP (OR (STRING-SEARCH-SET '(#/: #/.) L2 (1+ CP))
                                   (LET ((BP (END-OF-LINE L2)))
                                     (INSERT BP "       :")
                                     (INSERT (END-LINE BP -1) "        ")
                                     (SETQ I (1- I))
                                     CP)))))
         (IF (NOT (CHAR-EQUAL (AREF L2 GOAL) #/:))
             (INSERT-MOVING POINT (NSUBSTRING (LINE-PREVIOUS L2) CHAR-POS GOAL))
             (DELETE-AROUND *BLANKS* POINT)
             (INDENT-TO POINT (BP-VIRTUAL-INDENTATION (CREATE-BP L2 GOAL))))))
  DIS-TEXT)

(DEFCOM COM-COMPILE-AND-EXIT "Compile the buffer and return from top-level" ()
  (SEND *STANDARD-OUTPUT* ':MAKE-COMPLETE)
  (COM-COMPILE-BUFFER)
  (OR (AND (SEND *STANDARD-OUTPUT* ':INCOMPLETE-P)      ;If any compiler messages
           (NOT (LET ((*QUERY-IO* *STANDARD-OUTPUT*))
                  (Y-OR-N-P "Exit anyway? "))))
      (*THROW 'EXIT-TOP-LEVEL NIL))
  DIS-NONE)

(DEFCOM COM-EVALUATE-AND-EXIT "Evaluate the buffer and return from top-level" ()
  (COM-EVALUATE-BUFFER)
  (*THROW 'EXIT-TOP-LEVEL NIL))

(DEFCOM COM-GRIND-DEFINITION "Grind the definition of a function into the buffer.
Reads the name of the function from the mini-buffer and inserts its ground definition
at point." ()
  (LET ((FUNSPEC (TYPEIN-LINE-READ "Name of function:")))
    (SETQ FUNSPEC (OR (SI:DWIMIFY-PACKAGE-0 FUNSPEC 'FDEFINEDP) FUNSPEC))
    (IF (NOT (FDEFINEDP FUNSPEC))
        (BARF "~A is not a defined function spec." FUNSPEC))
    (SI:GRIND-1 FUNSPEC 90. (INTERVAL-STREAM-INTO-BP (POINT)) T))
  DIS-TEXT)

(DEFCOM COM-GRIND-EXPRESSION "Grind the evaluation of a form into the buffer.
Reads a form from the mini-buffer, evals it and inserts the result, ground, at
point." ()
  (LET ((TEM (SI:EVAL-ABORT-TRIVIAL-ERRORS
               (TYPEIN-LINE-MULTI-LINE-READ "Lisp form: (end with )"))))
    (GRIND-INTO-BP (POINT) TEM))
  DIS-TEXT)

(DEFCOM COM-DOWN-INDENTED-LINE "Move to the next line and past any indentation." (KM)
  (LET ((POINT (POINT)) (EOL))
    (COND ((AND (NOT *NUMERIC-ARG-P*)
                (BP-= (SETQ EOL (END-LINE POINT))
                      (INTERVAL-LAST-BP *INTERVAL*)))
           (MOVE-BP POINT (INSERT-MOVING EOL #/CR))
           DIS-TEXT)
          (T
           (MOVE-BP POINT (FORWARD-OVER *BLANKS* (FORWARD-LINE POINT *NUMERIC-ARG* T)))
           DIS-BPS))))

(DEFCOM COM-UP-INDENTED-LINE "Move to previous line and after any indentation." (KM)
  (MOVE-BP (POINT) (FORWARD-OVER *BLANKS* (FORWARD-LINE (POINT) (- *NUMERIC-ARG*) T)))
  DIS-BPS)

(DEFCOM COM-TEXT-JUSTIFIER-CHANGE-FONT-WORD "Puts the previous word in a different font (R).
The font to change to is specified with a numeric argument.
No arg means move last font change forward past next word.
A negative arg means move last font change back one word." ()
  (IF (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*))
      (LET ((BP1 (OR (FORWARD-WORD (POINT) -1) (BARF))) ;Positive explicit arg,
            BP2)
        (SETQ BP2 (FORWARD-WORD BP1 1 T))               ;Surround previous word
        (MOVE-BP (POINT) (INSERT BP2 "*"))
        (SETQ BP1 (INSERT BP1 #/))
        (INSERT BP1 (+ *NUMERIC-ARG* #/0)))             ;With indicated font change
      (MULTIPLE-VALUE-BIND (BP1 BP2 TYPE)
          (FIND-FONT-CHANGE (POINT) (INTERVAL-FIRST-BP *INTERVAL*) T)
        (OR BP1 (BARF))                                 ;Find previous font change
        (DELETE-INTERVAL BP1 BP2 T)                     ;Flush it
        (LET ((BP3 (FORWARD-WORD BP1 (IF (MINUSP *NUMERIC-ARG*) -1 1) T))       ;Where it goes
              BP4 BP5 NTYPE TYPE-2)
          (MULTIPLE-VALUE (BP4 BP5 NTYPE)
            (FIND-FONT-CHANGE BP1 BP3 (MINUSP *NUMERIC-ARG*)))  ;If moving over another one
          (SETQ TYPE-2
                (IF (MINUSP *NUMERIC-ARG*)
                    TYPE NTYPE))
          (OR (COND (BP4
                     (DELETE-INTERVAL BP4 BP5 T)        ;flush it
                     (CHAR-EQUAL (AREF TYPE-2 1) #/*)))
              (MOVE-BP (POINT) (INSERT BP3 TYPE))))))   ;Put in one moved unless was *
  DIS-TEXT)

(DEFCOM COM-TEXT-JUSTIFIER-CHANGE-FONT-REGION "Puts the region in a different font (R).
The font to change to is specified with a numeric argument.
Inserts ^F<n> before and ^F* after.
A negative arg removes font changes in or next to region." ()
  (REGION (BP1 BP2)
    (COND ((NOT (MINUSP *NUMERIC-ARG*))
           (INSERT BP2 "*")
           (INSERT-MOVING BP1 #/)
           (INSERT-MOVING BP1 (+ #/0 *NUMERIC-ARG*)))
          (T
           (AND (LOOKING-AT BP2 #/)
                (DELETE-INTERVAL BP2 (FORWARD-CHAR BP2 2) T))
           (AND (LOOKING-AT-BACKWARD BP2 #/)
                (IBP BP2))
           (OR (LOOKING-AT-BACKWARD BP1 #/)
               (DBP BP1))
           (AND (LOOKING-AT-BACKWARD BP1 #/)
                (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) (FORWARD-CHAR BP1 1) T))
           (DO ((BP3))
               (NIL)
             (MULTIPLE-VALUE (BP1 BP3)
               (FIND-FONT-CHANGE BP1 BP2 NIL))
             (OR BP1 (RETURN NIL))
             (DELETE-INTERVAL BP1 BP3 T)))))
  DIS-TEXT)

(DEFUN FIND-FONT-CHANGE (BP LIMIT-BP REVERSE-P &AUX BP1 BP2)
  (COND ((SETQ BP1 (ZWEI-SEARCH BP #/ REVERSE-P NIL NIL LIMIT-BP))
         (IF (NOT REVERSE-P)
             (SETQ BP1 (DBP BP1)))
         (SETQ BP2 (FORWARD-CHAR BP1 2 T))
         (VALUES BP1 BP2 (STRING-INTERVAL BP1 BP2 T)))))

(DEFCOM COM-TEXT-JUSTIFIER-UNDERLINE-WORD " Puts underlines around the previous word (R).
If there is an underline begin or end near that word, it is moved forward one word.
An argument specifies the number of words, and the direction: positive means forward.
*TEXT-JUSTIFIER-UNDERLINE-BEGIN* is the string that begins underlines and
*TEXT-JUSTIFIER-UNDERLINE-END* is the string that ends it." ()
  (LET ((LIST (LIST *TEXT-JUSTIFIER-UNDERLINE-BEGIN* *TEXT-JUSTIFIER-UNDERLINE-END*))
        (BP (FORWARD-TO-WORD (POINT) 1 T))
        BP1 TYPE)
    (SETQ BP1 (FORWARD-WORD (FORWARD-WORD BP -2 T)))
    (MULTIPLE-VALUE (BP TYPE)
      (SEARCH-STRING-SET-KLUDGE BP LIST T NIL BP1))
    (IF (NULL BP)
        (LET ((ARG (IF *NUMERIC-ARG-P* *NUMERIC-ARG* -1)))
          (SETQ BP1 (POINT))
          (LET ((BP2 (OR (FORWARD-WORD BP1 ARG) (BARF))))
            (COND ((MINUSP ARG)
                   (MOVE-BP BP1 (FORWARD-WORD BP2 (- ARG)))
                   (INSERT-MOVING BP1 *TEXT-JUSTIFIER-UNDERLINE-END*)
                   (INSERT BP2 *TEXT-JUSTIFIER-UNDERLINE-BEGIN*))
                  (T
                   (INSERT BP2 *TEXT-JUSTIFIER-UNDERLINE-END*)
                   (INSERT BP1 *TEXT-JUSTIFIER-UNDERLINE-BEGIN*)))))
      (DELETE-INTERVAL BP (FORWARD-CHAR BP (STRING-LENGTH TYPE)) T)
      (SETQ BP1 (IF (MINUSP *NUMERIC-ARG*)
                    (FORWARD-WORD (FORWARD-WORD BP (1- *NUMERIC-ARG*) T))
                  (FORWARD-TO-WORD BP (1+ *NUMERIC-ARG*) T)))
      (MULTIPLE-VALUE-BIND (BP2 NTYPE)
          (SEARCH-STRING-SET-KLUDGE BP LIST (MINUSP *NUMERIC-ARG*) NIL BP1 )
        (OR (COND (BP2
                   (DELETE-INTERVAL BP2 (FORWARD-CHAR BP2
                                                      (* (IF (MINUSP *NUMERIC-ARG*) 1 -1)
                                                         (STRING-LENGTH NTYPE))))
                   (NOT (EQUAL TYPE NTYPE))))
            (LET ((BP3 (IF (MINUSP *NUMERIC-ARG*)
                           (FORWARD-WORD (FORWARD-WORD BP (1- *NUMERIC-ARG*)))
                         (FORWARD-WORD BP *NUMERIC-ARG*))))
              (MOVE-BP (POINT) (INSERT BP3 TYPE)))))))
  DIS-TEXT)

(DEFUN SEARCH-STRING-SET-KLUDGE (BP STRING-LIST REVERSEP FIXUP-P LIMIT-BP)
  (LET (BP-LIST BP-FIRST-FOUND STRING)
    (SETQ BP-LIST (MAPCAR #'ZWEI-SEARCH (CIRCULAR-LIST BP)
                          STRING-LIST
                          (CIRCULAR-LIST REVERSEP)
                          (CIRCULAR-LIST FIXUP-P)
                          (CIRCULAR-LIST NIL)
                          (CIRCULAR-LIST LIMIT-BP)))
    (DO ((BPS BP-LIST (CDR BPS))
         (STRINGS STRING-LIST (CDR STRINGS)))
        ((NULL BPS)
         (VALUES BP-FIRST-FOUND STRING))
      (AND (CAR BPS)
           (OR (NULL BP-FIRST-FOUND)
               (IF REVERSEP
                   (BP-< BP-FIRST-FOUND (CAR BPS))
                 (BP-< (CAR BPS) BP-FIRST-FOUND)))
           (SETQ BP-FIRST-FOUND (CAR BPS) STRING (CAR STRINGS))))))

(DEFCOM COM-TEXT-JUSTIFIER-UNDERLINE-REGION "Puts underlines a la R around the region.
A negative argument removes underlines in or next to region.
*TEXT-JUSTIFIER-UNDERLINE-BEGIN* is the string that begins underlines and
*TEXT-JUSTIFIER-UNDERLINE-END* is the string that ends it." ()
  (REGION (BP1 BP2)
    (LET ((LIST (LIST *TEXT-JUSTIFIER-UNDERLINE-BEGIN* *TEXT-JUSTIFIER-UNDERLINE-END*)))
      (IF (MINUSP *NUMERIC-ARG*)
          (DO ((BP (FORWARD-WORD (FORWARD-WORD BP1 -1 T)))
               TYPE
               (LIM-BP (FORWARD-WORD BP2 1 T)))
              (NIL)
            (SETF (VALUES BP TYPE) (SEARCH-STRING-SET-KLUDGE BP LIST NIL NIL LIM-BP))
            (OR BP
                (RETURN NIL))
            (LET ((BP3 BP))
              (SETQ BP (FORWARD-CHAR BP (- (STRING-LENGTH TYPE))))
              (DELETE-INTERVAL BP BP3 T)))
          (INSERT BP2 *TEXT-JUSTIFIER-UNDERLINE-END*)
          (INSERT BP1 *TEXT-JUSTIFIER-UNDERLINE-BEGIN*))))
  DIS-TEXT)

(DEFCOM COM-COPY-FROM-PREVIOUS-LINE "Copy characters from the last non-blank line.
Argument is the number of characters" ()
  (LET* ((POINT (POINT))
         (BP (DO ((BP (BEG-LINE POINT -1) (BEG-LINE BP -1))
                  (LIM (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
                  (LINE)
                  (IDX (BP-INDEX POINT)))
                 ((EQ (SETQ LINE (BP-LINE BP)) LIM)
                  (BARF))
               (AND (NOT (LINE-BLANK-OR-DIAGRAM-P LINE))
                    (> (LINE-LENGTH LINE) IDX)
                    (RETURN (CREATE-BP LINE IDX))))))
    (INSERT-INTERVAL-MOVING POINT (COPY-INTERVAL BP (FORWARD-CHAR BP *NUMERIC-ARG*))))
  DIS-TEXT)

;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Functions which deal with indentation
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFUN LINE-INDENTATION (LINE &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
  "Horizontal position in pixels of first non-whitespace character on LINE.
If the line is all whitespace, the end of the line is what counts.
This is 0 if the first character is non-whitespace.
SHEET is used to determine the fonts to use."
  (STRING-WIDTH LINE
                0
                (BP-INDEX (FORWARD-OVER *BLANKS* (CREATE-BP LINE 0)))
                SHEET))

(DEFUN BP-INDENTATION (BP &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
  "Horizontal position in pixels of BP, taking account continuation of line.
A character in column 0 is at horizontal position 0.
SHEET is used to determine the fonts to use."
  (STRING-WIDTH (BP-LINE BP) 0 (BP-INDEX BP) SHEET T))

(DEFUN BP-VIRTUAL-INDENTATION (BP &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)))
  "Horizontal position in pixels of BP, if window were infinitely wide.
SHEET is used for determining fonts."
  (STRING-WIDTH (BP-LINE BP) 0 (BP-INDEX BP) SHEET))

;;; This is the only function which knows to use spaces and tabs
;;; to perform indentation!!!  Nobody else should know that.
(DEFUN INDENT-TO (BP GOAL &OPTIONAL (SHEET (WINDOW-SHEET *WINDOW*)) &AUX SPACES)
  "Insert spaces and maybe tabs at BP until it reaches hpos GOAL.
Returns a bp to the end of the inserted whitespace.
If *INDENT-WITH-TABS* is NIL, just spaces are used.
GOAL is measured in pixels.  SHEET determines the fonts to use to calculate positions.
If we cannot get exactly to the GOAL pixel, we go a little past it."
  (LET (N M)
    (LET ((BPI (BP-VIRTUAL-INDENTATION BP SHEET))
          (SW (FONT-SPACE-WIDTH))
          (TW (SEND SHEET :EDITOR-TAB-WIDTH)))
      (SETQ M (FLOOR GOAL TW)                   ;Number of tabs to get to goal
            N (* TW M))                         ;Position of rightmost tab
      (IF (OR (> BPI N)                         ;Past there, no tabs can be used,
              (NOT *INDENT-WITH-TABS*))         ;and also if user says "no tabs"
          (SETQ N (CEILING (- GOAL BPI) SW) M 0)        ;so use all spaces.
          (SETQ M (- M (FLOOR BPI TW)) N (CEILING (- GOAL N) SW))))     ;else tabs and spaces
    ;; M has number of tabs, N has number of spaces.
    ;; Set up SPACES with the string to be inserted.
    (SETQ SPACES (MAKE-ARRAY (+ M N)
                             :TYPE (IF (ZEROP *FONT*)
;>> art-fat-string lossage
                                       'ART-STRING
                                       'ART-FAT-STRING)))
    (LET ((TAB (IN-CURRENT-FONT #/TAB))
          (SPACE (IN-CURRENT-FONT #/SPACE)))
      (DO ((I (1- M) (1- I))) ((MINUSP I))
        (SETF (CHAR SPACES I) TAB))
      (DO ((I 1 (1+ I))
           (J M (1+ J)))
          ((> I N))
        (SETF (CHAR SPACES J) SPACE))))
  (INSERT-MOVING BP SPACES))


(DEFUN FONT-SPACE-WIDTH ()
  "Return the width of a space character in FONT."
  (IF (TYPEP *WINDOW* 'WINDOW)
      (LET* ((FONT (CURRENT-FONT *WINDOW*))
             (CHAR-WIDTH-TABLE (FONT-CHAR-WIDTH-TABLE FONT)))
        (IF CHAR-WIDTH-TABLE
            (AREF CHAR-WIDTH-TABLE (CHAR-CODE #/SPACE))
          (FONT-CHAR-WIDTH FONT)))
    (SEND *WINDOW* :CHARACTER-WIDTH)))

(DEFUN INDENT-LINE (BP INDENTATION &OPTIONAL
                       (SHEET (WINDOW-SHEET *WINDOW*))
                       (BP1 (CREATE-BP (BP-LINE BP) 0)))
  "Adjust the indentation at the front of BP's line to be INDENTATION pixels wide.
If the indentation is already as desired, the line is not changed.
SHEET determines the fonts for computing the indentation.
Returns a BP to the end of what it inserted.
Preserves the indentations of bps pointing within the indentation, if possible.
If BP1 is specified, the indentation after BP1 is processed.
In this case, BP is ignored; its only purpose is to set BP1."
  (LET (BP-AFTER BP-LIST NONBLANK-INDEX)
    (SETQ BP-AFTER (FORWARD-OVER *BLANKS* BP1))
    (SETQ NONBLANK-INDEX (BP-INDEX BP-AFTER))
    (IF (= INDENTATION (STRING-WIDTH (BP-LINE BP1) 0 NONBLANK-INDEX SHEET))
        BP-AFTER
      (DOLIST (BP2 (LINE-BP-LIST (BP-LINE BP1)))
        (IF (OR (< (BP-INDEX BP2) NONBLANK-INDEX)
                (AND (= (BP-INDEX BP2) NONBLANK-INDEX)
                     (EQ (BP-STATUS BP2) ':NORMAL)))
            (PUSH (CONS BP2 (BP-INDENTATION BP2 SHEET)) BP-LIST)))
      (DELETE-INTERVAL BP1 BP-AFTER T)
      ;; Don't screw up the undo information for this change
      ;; by trying to preserve indentation of its bps.
      (LET ((US (NODE-UNDO-STATUS *INTERVAL*)))
        (UNLESS (EQ US ':DONT)
          (SETQ BP-LIST (DELQ (ASSQ (UNDO-STATUS-START-BP US) BP-LIST) BP-LIST))
          (SETQ BP-LIST (DELQ (ASSQ (UNDO-STATUS-END-BP US) BP-LIST) BP-LIST))))
      (PROG1 (INDENT-TO BP1 INDENTATION SHEET)
             (LET ((NONBLANK-INDEX (BP-INDEX (FORWARD-OVER *BLANKS*
                                                           (CREATE-BP (BP-LINE BP) 0)))))
               (DOLIST (BP-AND-INDENTATION BP-LIST)
                 (LET ((INDEX (INDENTATION-INDEX (BP-LINE (CAR BP-AND-INDENTATION))
                                                 (CDR BP-AND-INDENTATION) SHEET)))
                   (AND INDEX (SETF (BP-INDEX (CAR BP-AND-INDENTATION))
                                    (MIN NONBLANK-INDEX INDEX))))))))))

(DEFUN INDENT-BP-ADJUSTMENT (BP)
  "If BP is within the indentation of its line, move it past.
The indentation of the line is the whitespace characters at the beginning of it, if any.
BP is modified, and returned."
  (LET ((BP1 (FORWARD-OVER *BLANKS* (CREATE-BP (BP-LINE BP) 0))))
    (COND ((AND (< (BP-INDEX (POINT))
                   (BP-INDEX BP1)))
           (MOVE-BP BP BP1)))
    BP))

(DEFUN INDENTATION-INDEX (LINE XPOS &OPTIONAL SHEET ROUND-DOWN-P)
  "Returns the index in LINE which would be at pixel XPOS.
If XPOS is greater than the length of the string, return NIL.
It the answer is between N and N+1, returns N if ROUND-DOWN-P, else N+1."
  (MULTIPLE-VALUE-BIND (X INDEX)
      (STRING-WIDTH LINE 0 (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                               (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
                               NIL)
                    (OR SHEET (WINDOW-SHEET *WINDOW*))
                    T XPOS)
    (AND INDEX ROUND-DOWN-P ( X XPOS)
         (SETQ INDEX (1- INDEX)))
    INDEX))

(DEFCOM COM-TAB-HACKING-DELETE-FORWARD "Delete characters forward, changing tabs into spaces.
Argument is repeat count." ()
  (DELETE-CHARS-CONVERTING-TABS (POINT) *NUMERIC-ARG*))

(DEFCOM COM-TAB-HACKING-RUBOUT "Rub out a character, changing tabs to spaces.
So tabs rub out as if they had been spaces all along.
A numeric argument is a repeat count." ()
  (DELETE-CHARS-CONVERTING-TABS (POINT) (- *NUMERIC-ARG*)))

(DEFUN DELETE-CHARS-CONVERTING-TABS (POINT COUNT
                                     &AUX (BP (COPY-BP POINT))
                                     (MIN-INDEX (BP-INDEX BP)))
  "Delete COUNT characters after POINT, converting tabs to spaces before deletion."
  ;; Scan across what we will delete, converting tabs to spaces.
  ;; BP gets set to the other end of the range to be deleted.
  (COND ((> COUNT 0)
         (DOTIMES (I COUNT)
           (AND (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))
                (RETURN (BEEP)))
           ;; When moving forward, whenever we find a blank we must
           ;; convert all tabs within the blanks that follow.
;character lossage
           (WHEN (OR (MEMQ (BP-CH-CHARACTER BP) *BLANKS*)
                     (MEMQ (CHAR-INT (BP-CH-CHARACTER BP)) *BLANKS*))
             (LET ((BP1 (COPY-BP BP)))
               (DO ()
                   ((OR (BP-= BP1 (INTERVAL-LAST-BP *INTERVAL*))
;character lossage
                        (NOT (OR (MEMQ (BP-CH-CHARACTER BP1) *BLANKS*)
                                 (MEMQ (CHAR-INT (BP-CH-CHARACTER BP1)) *BLANKS*)))))
                 (IF (CHAR= (BP-CH-CHARACTER BP1) #/TAB)
                     (TAB-CONVERT BP1 (FORWARD-CHAR BP1 1))
                   (IBP BP1)))))
           (IBP BP)))
        (T
         (DOTIMES (I (- COUNT))
           (AND (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
                (RETURN (BEEP)))
           (WHEN (CHAR-EQUAL (BP-CHARACTER-BEFORE BP) #/TAB)
             (SETQ MIN-INDEX (1- (BP-INDEX BP)))
             (TAB-CONVERT (FORWARD-CHAR BP -1) BP))
           (DBP BP))))
  (IF (EQ (BP-LINE POINT) (BP-LINE BP))
      (MUST-REDISPLAY *WINDOW* DIS-LINE (BP-LINE POINT)
                      (MIN (BP-INDEX POINT) MIN-INDEX (BP-INDEX BP)))
      (MUST-REDISPLAY *WINDOW* DIS-TEXT))
  (FUNCALL (IF *NUMERIC-ARG-P* #'KILL-INTERVAL #'DELETE-INTERVAL) POINT BP)
  DIS-NONE)

(DEFUN TAB-CONVERT (BP-BEFORE BP-AFTER)
  "Convert a tab to the right number of spaces, preserving the font.
BP-BEFORE and BP-AFTER should be temporary BPs to before and after the tab character."
  (LET ((INDENT-BEFORE (BP-VIRTUAL-INDENTATION BP-BEFORE))
        (INDENT-AFTER (BP-VIRTUAL-INDENTATION BP-AFTER))
        (*FONT* (CHAR-FONT (BP-CHARACTER BP-BEFORE)))
        SPACE NSPACES)
    (IF (BP-STATUS BP-BEFORE)
        (FERROR NIL "~S is not a temporary BP." BP-BEFORE))
    (IF (BP-STATUS BP-AFTER)
        (FERROR NIL "~S is not a temporary BP." BP-AFTER))
    (SETQ SPACE (IN-CURRENT-FONT #/SPACE))
    (SETQ NSPACES (FLOOR (- INDENT-AFTER INDENT-BEFORE) (FONT-SPACE-WIDTH)))
    (MUNG-BP-LINE-AND-INTERVAL BP-BEFORE)
    (ASET SPACE (BP-LINE BP-BEFORE) (BP-INDEX BP-BEFORE))
    (WHEN  (PLUSP NSPACES)
      (INSERT-CHARS BP-BEFORE SPACE (1- NSPACES))
      (MOVE-BP BP-AFTER (BP-LINE BP-AFTER) (+ (BP-INDEX BP-AFTER) NSPACES -1)))
    BP-AFTER))

(DEFCOM COM-UNTABIFY "Replace tabs with spaces in region, or from point to end.
All tab characters are replaced by spaces, preserving indentation.
A numeric argument specifies the number of spaces a tab is equivalent to." ()
  (LET ((*TAB-WIDTH* (IF *NUMERIC-ARG-P* *NUMERIC-ARG*
                           *TAB-WIDTH*)))
        (IF (WINDOW-MARK-P *WINDOW*)
        (REGION (BP1 BP2)
          (UNTABIFY-INTERVAL BP1 BP2 T))
      (UNTABIFY-INTERVAL (POINT) (INTERVAL-LAST-BP *INTERVAL*) T)))
  DIS-TEXT)

(DEFUN UNTABIFY-INTERVAL (BP1 BP2 &OPTIONAL IN-ORDER-P)
  "Convert all tabs to spaces within the specified interval.
Give either one arg, an interval, or two BPs."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((BP BP1)
       (*INDENT-WITH-TABS* NIL)
       AFTER-BP)
      ((NOT (SETQ AFTER-BP (ZWEI-SEARCH BP #/TAB NIL NIL NIL BP2))))
    (SETQ BP (FORWARD-CHAR AFTER-BP -1))
    (TAB-CONVERT BP AFTER-BP)))

(DEFCOM COM-TABIFY "Replace spaces with tabs in region, or from point to end.
All runs of three or more spaces are replaced as much as possible with tabs,
preserving the indentation.
A numeric argument specifies the number of spaces a tab is equivalent to." ()
  (LET ((*TAB-WIDTH* (IF *NUMERIC-ARG-P* *NUMERIC-ARG*
                       *TAB-WIDTH*)))
    (IF (WINDOW-MARK-P *WINDOW*)
        (REGION (BP1 BP2)
          (TABIFY-INTERVAL BP1 BP2 T))
      (TABIFY-INTERVAL (POINT) (INTERVAL-LAST-BP *INTERVAL*) T)))
  DIS-TEXT)

(DEFUN TABIFY-INTERVAL (BP1 BP2 &OPTIONAL IN-ORDER-P)
  "Convert multiple spaces to tabs within the specified interval.
Runs of three or more spaces which span a tab stop are converted.
Give either one arg, an interval, or two BPs."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((BP BP1)
       (*INDENT-WITH-TABS* T)
       AFTER-BP)
      ((NOT (SETQ AFTER-BP (ZWEI-SEARCH BP "   " NIL NIL NIL BP2))))
    ;; Get BP and AFTER-BP around this run of spaces.
    (SETQ BP (FORWARD-CHAR AFTER-BP -3))
    (SETQ AFTER-BP (FORWARD-OVER '(#/SPACE) AFTER-BP))
    ;; Delete them and replace with standard indentation (which uses tabs if possible).
    (LET ((INDENTATION (BP-VIRTUAL-INDENTATION AFTER-BP))
          (*FONT* (CHAR-FONT (BP-CHARACTER BP))))
      (DELETE-INTERVAL BP AFTER-BP T)
      (INDENT-TO BP INDENTATION))))

(DEFCOM COM-INDENT-FOR-LISP-COMMENTS-SPECIAL
  "Like LISP Tab, except in comments just inserts Tab." ()
  (LET ((POINT (POINT)))
    (IF (NTH-VALUE 2 (LISP-BP-SYNTACTIC-CONTEXT POINT))
        ;; in a comment
        (COM-INSERT-TAB)
        (COM-INDENT-FOR-LISP))))

(DEFCOM COM-INDENT-FOR-LISP "Indent this line to make ground LISP code.
Numeric argument is number of lines to indent." ()
  (LET ((PT (POINT)) END FLAG)
    (SETQ END (OR (BEG-LINE PT *NUMERIC-ARG*)
                  (INSERT (SETQ FLAG (INTERVAL-LAST-BP *INTERVAL*)) #/NEWLINE)))
    (SETQ END (INDENT-INTERVAL-FOR-LISP (BEG-LINE PT) END NIL NIL *NUMERIC-ARG-P*))
    (IF (= *NUMERIC-ARG* 1)
        (INDENT-BP-ADJUSTMENT PT)
        (MOVE-BP PT END))
    (AND FLAG
         (DELETE-INTERVAL (FORWARD-CHAR FLAG -1) FLAG T)))
  DIS-TEXT)

(DEFCOM COM-INDENT-NEW-LINE "Insert a Return and the proper indentation on the new line." ()
  (MOVE-BP (POINT) (DELETE-BACKWARD-OVER *BLANKS* (POINT)))
  (LET ((*LAST-COMMAND-TYPE* 'INDENT-NEW-LINE)
        *CURRENT-COMMAND-TYPE*)                 ;Don't be fooled
    (MAX (IF *INDENT-NEW-LINE-NEW-LINE-FUNCTION*
             (FUNCALL *INDENT-NEW-LINE-NEW-LINE-FUNCTION*)
           (KEY-EXECUTE #/NEWLINE *NUMERIC-ARG-P* *NUMERIC-ARG*))
         (IF *INDENT-NEW-LINE-INDENT-FUNCTION*
             (FUNCALL *INDENT-NEW-LINE-INDENT-FUNCTION*)
           (KEY-EXECUTE #/TAB)))))

(DEFCOM COM-INDENT-SEXP "Indent the following s-expression.
Each line that starts within the s-expression is indented for Lisp.
/(This implies that the line that point is on is NOT adjusted).
If there is not a complete s-expression, it will indent to the
next line that has a non-white-space character at the start."
        ()
  (LET ((BP1 (OR (BEG-LINE (POINT) 1) (BARF)))
        (BP2 (OR (FORWARD-SEXP (POINT))
                 (LET ((NEXT-BP (FORWARD-TO-NEXT-LINE-WITH-NON-WHITE-SPACE-START (POINT))))
                   (IF NEXT-BP (CREATE-BP (LINE-PREVIOUS (BP-LINE NEXT-BP)) 0)
                     (BARF))))))
    (AND (BP-< BP1 BP2)
         (WITH-UNDO-SAVE ("Indent Sexp" BP1 BP2 T)
           (INDENT-INTERVAL-FOR-LISP BP1 BP2 T))))
  DIS-TEXT)

(DEFCOM COM-INDENT-NEW-LINE-AT-PREVIOUS-SEXP
        "Insert a Return and the proper indentation at the s-expression before point." ()
  (LET* ((POINT (POINT))
         (BP (OR (FORWARD-SEXP POINT (- *NUMERIC-ARG*)) (BARF))))
    (WITH-BP (OLD-POINT POINT :NORMAL)
      (MOVE-BP POINT BP)
      (UNWIND-PROTECT
          (COM-INDENT-NEW-LINE)
        (MOVE-BP POINT OLD-POINT)))))

;;;; Text grinding functions

(DEFUN FILL-INTERVAL (START-BP END-BP &OPTIONAL IN-ORDER-P ADJUST &AUX (FILLCOL *FILL-COLUMN*)
                      LINE1 LINE2 TEM
                      NON-PREFIX-LINES)
  "Fill or justify the text in the specified interval, by paragraphs.
Either START-BP is really an interval, or else
START-BP and END-BP are BPs delimiting the interval.
ADJUST non-NIL says justify; otherwise just fill."
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (WITH-UNDO-SAVE ("Fill" START-BP END-BP T)
    (SETQ LINE1 (BP-LINE START-BP)
          LINE2 (LET ((LINE (BP-LINE END-BP)))
                  (IF (OR (LINE-BLANK-OR-DIAGRAM-P LINE)
                          (END-LINE-P END-BP)
                          (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
                      (LINE-NEXT LINE) LINE)))
    ;; Remove any fill prefixes that are there already
    (WHEN (PLUSP (SETQ TEM (STRING-LENGTH *FILL-PREFIX*)))
      (SETQ FILLCOL (- FILLCOL (STRING-WIDTH *FILL-PREFIX*)))
      (DO ((LINE LINE1 (LINE-NEXT LINE)))
          ((EQ LINE LINE2))
        (IF (STRING-EQUAL LINE *FILL-PREFIX* :END1 TEM)
            (DELETE-INTERVAL (CREATE-BP LINE 0) (CREATE-BP LINE TEM) T)
          (PUSH LINE NON-PREFIX-LINES))))
    ;; Make sentences ending at eol have extra space
    (DO ((LINE LINE1 (LINE-NEXT LINE)))
        ((EQ LINE LINE2))
      (AND ( (SETQ TEM (1- (LINE-LENGTH LINE))) 0)
;character lossage
           (OR (MEMQ (CHAR-CODE (CHAR LINE TEM)) *FILL-EXTRA-SPACE-LIST*)
               (MEMQ (MAKE-CHAR (CHAR LINE TEM)) *FILL-EXTRA-SPACE-LIST*))
           (NEQ LINE2 (SETQ TEM (LINE-NEXT LINE)))
           (NOT (ZEROP (LINE-LENGTH TEM)))
           (INSERT (CREATE-BP LINE (LINE-LENGTH LINE)) #/SPACE)))
    ;; Remove excess spaces except after periods, which we leave alone (e.g. vs sentences),
    ;; or at the start of lines.
    (DO ((BP START-BP) (CH))
        (())
      (MULTIPLE-VALUE-SETQ (BP CH) (SEARCH-SET BP *BLANKS*))
      (OR (AND BP (BP-< BP END-BP))
          (RETURN NIL))
      (SETQ CH (BP-CHARACTER-BEFORE
                 (BACKWARD-OVER '(#/" #/' #/) #/]) (FORWARD-CHAR BP -1))))
      (COND ((CHAR-EQUAL CH #/NEWLINE)
             (SETQ BP (FORWARD-OVER *BLANKS* BP)))
;character lossage
            ((NOT (OR (MEMQ (CHAR-CODE CH) *FILL-EXTRA-SPACE-LIST*)
                      (MEMQ (MAKE-CHAR CH) *FILL-EXTRA-SPACE-LIST*)))
             (SETQ BP (DELETE-OVER *BLANKS* BP))
             (AND (END-LINE-P BP) (DELETE-BACKWARD-OVER *BLANKS* BP)))))
    ;; And now start filling
    (DO ((LINE LINE1 (LINE-NEXT LINE))
         (TEM) (TEM-BP (CREATE-BP LINE1 0))
         (BREAK-NEXT)
         (SHEET (WINDOW-SHEET *WINDOW*))
         (FONT (SEND *WINDOW* :CURRENT-FONT)))
        ((EQ LINE LINE2))
      ;; Break generated by next line?
      (SETQ BREAK-NEXT (OR (EQ LINE2 (SETQ TEM (LINE-NEXT LINE)))
                           (MEMQ TEM NON-PREFIX-LINES)
                           (BP-AT-PARAGRAPH-TERMINATOR (MOVE-BP TEM-BP TEM 0) NIL))
            TEM (DO ((I 0 (1+ I))               ;Initial blanks count in first word
                     (LEN (LINE-LENGTH LINE)))
                    ((OR (= I LEN)
;character lossage
                         (NOT (OR (MEMQ (MAKE-CHAR (CHAR LINE I))
                                        *PARAGRAPH-DELIMITER-LIST*)
                                  (MEMQ (CHAR-CODE (CHAR LINE I))
                                        *PARAGRAPH-DELIMITER-LIST*))))
                     I)))
      ;; Handle this line
      (OR (BP-AT-PARAGRAPH-DELIMITER (MOVE-BP TEM-BP LINE 0))
          ;; A line that's only an initial segment of the fill prefix
          ;; counts as blank.
          (AND NON-PREFIX-LINES
               (MEMQ LINE NON-PREFIX-LINES)
               (STRING-EQUAL LINE *FILL-PREFIX* :END2 (STRING-LENGTH LINE)))
          (DO ((POS 0)
               (CHAR-POS 0)
               (CP TEM)
               (BP1 (COPY-BP START-BP))
               (BP2 (COPY-BP START-BP))
               (NBLANKS 0))
              ((EQ LINE LINE2))
            (SETQ POS (TV:SHEET-STRING-LENGTH SHEET LINE CHAR-POS CP NIL FONT POS))
            (WHEN (> POS FILLCOL)               ;Line overflew
              (IF ( NBLANKS 1) (RETURN NIL))
              (MOVE-BP BP1 LINE CHAR-POS)
              (INSERT-MOVING BP1 #/NEWLINE)
              (DELETE-OVER *BLANKS* BP1)
              (MOVE-BP BP2 LINE (LINE-LENGTH LINE))
              (DELETE-BACKWARD-OVER *BLANKS* BP2)
              (UNLESS BREAK-NEXT
                (SETQ BP1 (END-LINE BP1))
                (OR (BEG-LINE-P BP1) (INSERT-MOVING BP1 #/SPACE))
                (MOVE-BP BP2 (LINE-NEXT (BP-LINE BP1)) 0)
                (DELETE-INTERVAL BP1 BP2 T))
              (SETQ NBLANKS (- NBLANKS 2))
              (AND ADJUST (PLUSP NBLANKS) (ADJUST-LINE LINE NBLANKS FILLCOL))
              (RETURN NIL))
            (SETQ CHAR-POS CP)
            (COND ((= CHAR-POS (LINE-LENGTH LINE))
                   (AND BREAK-NEXT (RETURN NIL))
                   (MOVE-BP BP1 LINE CHAR-POS)
                   (INSERT-MOVING BP1 #/SPACE)
                   (MOVE-BP BP2 (LINE-NEXT (BP-LINE BP1)) 0)
                   (DELETE-INTERVAL BP1 BP2 T)
                   (SETQ BREAK-NEXT (OR (EQ LINE2 (SETQ TEM (LINE-NEXT LINE)))
                                        (MEMQ TEM NON-PREFIX-LINES)
                                        (BP-AT-PARAGRAPH-TERMINATOR (MOVE-BP BP2 TEM 0) NIL)))
                   (OR (= (CHAR LINE CHAR-POS) #/SPACE)
                       (SETQ NBLANKS (1+ NBLANKS))))
                  ((SETQ CP (STRING-SEARCH-CHAR #/SPACE LINE (1+ CHAR-POS)))
                   (OR (= CP (1+ CHAR-POS))
                       (SETQ NBLANKS (1+ NBLANKS))))
                  (T
                   (SETQ CP (LINE-LENGTH LINE)))))))
    (AND (PLUSP (STRING-LENGTH *FILL-PREFIX*))
         (DO ((LINE LINE1 (LINE-NEXT LINE)))
             ((EQ LINE LINE2))
           (OR (MEMQ LINE NON-PREFIX-LINES)
               (INSERT (CREATE-BP LINE 0) *FILL-PREFIX*))))))

(DEFUN ADJUST-LINE (LINE NBLANKS FILL-COLUMN &AUX NEEDED AVG EXTRA EXPER)
  "Justify LINE to extend to FILL-COLUMN (in units of pixels).
NBLANKS is the number of word-separators within LINE
at which blanks can be inserted.  Yes, this is redundant."
  (SETQ NEEDED (FLOOR (- FILL-COLUMN (STRING-WIDTH LINE)) (FONT-SPACE-WIDTH))
        AVG (FLOOR NEEDED NBLANKS)
        EXTRA (\ NEEDED NBLANKS)
        EXPER (COND ((ZEROP EXTRA) 0) (T (FLOOR (+ NBLANKS (1- EXTRA)) EXTRA))))
  (DO ((N NBLANKS (1- N))
       (BP (CREATE-BP LINE 0))
       (EXP EXPER (1- EXP))
       (I AVG AVG))
      ((= N 0))
    (OR (SETQ BP (ZWEI-SEARCH BP #/SPACE NIL NIL NIL (END-OF-LINE LINE)))
        (FERROR NIL "Not enough spaces to adjust with in ~S" LINE))
    (SETQ BP (FORWARD-OVER *BLANKS* BP))
    (AND (> EXTRA 0) (= EXP 1)
         (SETQ I (1+ I)
               EXTRA (1- EXTRA)
               EXP EXPER))
    (DO ((I I (1- I)))
        ((= I 0))
      (INSERT-MOVING BP #/SPACE))))

;;;Common indenter for Tab, C-M-Q, and friends
(DEFUN INDENT-INTERVAL-FOR-LISP (BP1 &OPTIONAL BP2 IN-ORDER-P START-BP (COMMENTS-P T))
  "Indent all the lines in the specified interval for Lisp.
Specify either an interval or two BPs.
A line is in the interval iff its beginning is included.
START-BP is a place to start parsing from; it defaults /"right/".
COMMENTS-P if NIL means do not reindent the comments as comments."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (OR START-BP (SETQ START-BP (FORWARD-DEFUN BP1 -1 T)))
  (LISP-PARSE-FROM-DEFUN (BP-LINE BP2) START-BP)
  (INTERVAL-LINES (BP1 BP2) (START-LINE STOP-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
         (*LISP-PARSE-PREPARSED-FLAG* T)
         (BP)
         (INDENTATION)
         (IN-STRING))
        ((EQ LINE STOP-LINE) BP2)
      (SETQ BP (CREATE-BP LINE 0))
      (COND ((AND COMMENTS-P
                  (PLUSP (LINE-LENGTH LINE))
                  (MULTIPLE-VALUE-BIND (NIL NIL COMMENT)
                      (LISP-BP-SYNTACTIC-CONTEXT (CREATE-BP LINE 1) START-BP)
                    COMMENT))
             (SETQ INDENTATION NIL))
            (T
             (MULTIPLE-VALUE-SETQ (INDENTATION IN-STRING)
               (INDENT-FOR-LISP BP START-BP))))
      (COND ((NOT IN-STRING)                    ;Dont touch lines inside a string
             (AND INDENTATION (INDENT-LINE BP INDENTATION))
             (AND COMMENTS-P (INDENT-FOR-COMMENT BP))))
      (OR (EQ (LINE-NEXT LINE) STOP-LINE)
          (LISP-PARSE-LINE-MEMOIZED LINE IN-STRING)))))

(DEFUN INDENT-FOR-LISP (BP &OPTIONAL START-DEFUN-BP
                           &AUX BP1 BP2 INDENTATION OFFSET SYM
                           TEM SPACE-WIDTH NSEXPS LASTPAREN LASTSEXP IN-STRING)
  "Returns the indentation in pixels BP's line should have, for Lisp indent.
Second value is non-NIL if the line starts inside a string,
in which case the caller may decide not to reindent it at all.
START-DEFUN-BP is a BP to start parsing from.  Presumably before BP's line.
*LISP-INDENT-OFFSET* is the amount to offset if there isnt a complete sexp on another line
*LISP-DEFUN-INDENTION* is the amount to indent for top-level forms
*LISP-INDENT-OFFSET-ALIST* is an alist of the form ((FUNCTION . OFFSET-LIST) ...)
OFFSET-LIST is a list specifying (number-of-sexps-to-skip amount-to-change-indentation ...)
or if OFFSET-LIST is a symbol or function, it is funcall'ed and can return
the indentation, an offset, or a bp whose indentation to use."
  (DECLARE (VALUES LINE-INDENTATION-IN-PIXELS START-OF-LINE-IN-STRING-P))
  (SETQ BP (CREATE-BP (BP-LINE BP) 0)
        BP1 (OR START-DEFUN-BP (SETQ START-DEFUN-BP (FORWARD-DEFUN BP -1 T))))
  (SETQ IN-STRING (LISP-PARSE-FROM-DEFUN (BP-LINE BP) BP1))
  (IF IN-STRING
      (VALUES 0 IN-STRING)
    ;; Get BP to last unterminated paren (up one level).  Sixth argument of NIL makes
    ;; sure we get an open paren and not a single-quote (forward or backward).
    (SETQ LASTPAREN (FORWARD-SEXP BP -1 NIL 1 BP1 NIL))
    ;; Get BP to start of last complete sexp, or NIL if none at this level.
    (SETQ LASTSEXP (FORWARD-SEXP BP -1 NIL 0 BP1))
    (AND LASTPAREN LASTSEXP (BP-= LASTSEXP LASTPAREN) (SETQ LASTSEXP NIL))
    (SETQ OFFSET 0
          SPACE-WIDTH (FONT-SPACE-WIDTH))
    (AND LASTPAREN                  ;Try to find the indentation for the current function
         (LET ((BP2 (FORWARD-CHAR LASTPAREN)))
           (LET ((I (BP-INDEX BP2)))
             (SETQ SYM (DO ((J I (1+ J))
                            (LINE (BP-LINE BP2))
                            (LENGTH (LINE-LENGTH (BP-LINE BP2))))
                           ((OR ( J LENGTH)
                                (AND ( (LIST-SYNTAX (CHAR LINE J)) LIST-ALPHABETIC)
                                     ( (LIST-SYNTAX (CHAR LINE J)) LIST-COLON)))
                            (AND ( I J)
                                 (CATCH-ERROR (CLI:READ-FROM-STRING
                                                (STRING-REMOVE-FONTS (NSUBSTRING LINE I J))
                                                NIL NIL)
                                              NIL)))))
             ;; Beware of funny read syntax, numbers, etc.
             (OR (SYMBOLP SYM) (SETQ SYM NIL))))
         (SETQ TEM (GET-FUNCTION-LISP-INDENTATION SYM))
         ;; This function on the alist => value is either
         ;; an indentation list or a function to call.
         (COND ((CONSP TEM)         ;Indentation list, see how do handle this depth
                ;; How many sexps at this level precede point?  Set NSEXPS.
                ;; But, first, let's see how many are interesting (that's (1- MAX-I) ).
                ;; Don't keep counting NSEXPS when it's already larger than is interesting.
                (DO ((BP3 (FORWARD-CHAR LASTPAREN 1) (FORWARD-SEXP BP3 1 NIL 0 BP))
                     (MAX-I (1+ (CAR (NLEFT 2 TEM))))
                     (I 0 (1+ I)))
                    ((NULL BP3) (SETQ NSEXPS (- I 2)))
                  (AND (> I MAX-I) (RETURN NIL)))
                ;; Now see what the indentation lists says about that many sexps.
                (AND NSEXPS
                     (DO ((L TEM (CDDR L))
                          (I 0))
                         ((OR (NULL L) (> I NSEXPS)))
                       (AND (= (SETQ I (CAR L)) NSEXPS)
                            (SETQ OFFSET (CADR L) LASTSEXP NIL)))))
               (T
                (MULTIPLE-VALUE-SETQ (BP2 INDENTATION OFFSET)
                  ;; funcalled with arguments:
                  ;;  bp to start of current defun
                  ;;  bp to start of line (the one we are indenting)
                  ;;  bp to last unterminated paren (up one level) (such as "(PROG ...)")
                  ;;  bp to start of last complete sexp, or NIL if none at this level.
                  ;;  space width of current font
                  ;;  symbol at start of sexp (such as PROG)
                  ;; values returned:
                  ;;  bp to which to indent, or NIL
                  ;;   (may be affected by OFFSET, third value returned)
                  ;;  explicit indentation in pixels desired, or NIL.
                  ;;   Overrides any other values
                  ;;  offset from first first value (in space-widths) to indent, or 0
                  ;;   (eg indent 3 spaces from bp to start of tag in a tagbody to get
                  ;;    indentation for start of this sexp)
                  (FUNCALL TEM BP1 BP LASTPAREN LASTSEXP SPACE-WIDTH SYM)))))
    (SETQ BP1 (DO-FOREVER
                (COND ((NULL LASTPAREN)         ;If already balanced, nothing to do
                       (RETURN BP))
                      (BP2                      ;Specified what to indent to
                       (RETURN BP2))
                      (INDENTATION)             ;Specified how far to indent
                      ;; If there is no complete sexp at this paren depth, line up just after
                      ;; the leftparen.
                      ((OR (NULL LASTSEXP) (BP-< LASTSEXP LASTPAREN))
                       (RETURN (FORWARD-CHAR LASTPAREN)))
                      (T
                       (SETQ BP1 (CREATE-BP (BP-LINE LASTSEXP) 0))
                       ;; If complete sexp is on different line than the unmatched leftparen,
                       ;; line up with start of sexp's line.
                       (COND ((OR (NULL LASTPAREN) (BP-< LASTPAREN BP1))
                              (SETQ BP1 (FORWARD-OVER *BLANKS* BP1))
                              ;;OK only if the first on the line or at that level.
                              ;; If LASTSEXP is first nonblank thing on its line, use it.
                              ;; Also if there are no unmatched close parens preceding it,
                              ;; use the first nonblank thing on that line
                              ;; since that must be at LASTSEXP's level.
                              (AND (OR (BP-= BP1 LASTSEXP)
                                       (NOT (FORWARD-SEXP BP1 1 NIL 1 LASTSEXP)))
                                   (RETURN BP1))
                              ;; LASTSEXP follows on the same line as an unmatched close.
                              ;; Back up one sexp from it.
                              ;; Eventually this moves back across that unmatched close
                              ;; and the sexp that it terminates, to another line.
                              (SETQ LASTSEXP (FORWARD-SEXP LASTSEXP -1 NIL))
;                                   LASTPAREN (FORWARD-SEXP LASTSEXP -1 NIL 1 LASTPAREN NIL)
;                                   LASTSEXP (FORWARD-SEXP LASTSEXP -1 NIL 0 LASTPAREN))
                              )
                             ;; Otherwise, maybe user specified how to handle this case
                             (*LISP-INDENT-OFFSET*
                              (SETQ OFFSET (+ *LISP-INDENT-OFFSET* OFFSET))
                              (RETURN (FORWARD-CHAR LASTPAREN)))
                             ;; If only one element in list so far, line up under left-paren
                             ;; also if the CAR doesnt look like the name of a function
                             ((SETQ TEM (INDENT-NOT-FUNCTION-P
                                          LASTPAREN
                                          (SETQ BP2 (FORWARD-CHAR LASTPAREN))
                                          START-DEFUN-BP))
                              (IF (NUMBERP TEM) (SETQ OFFSET TEM))
                              (RETURN BP2))
                             ((BP-< LASTSEXP (SETQ BP1 (FORWARD-SEXP BP2)))
                              (SETQ OFFSET
                                    (IF (COND-CLAUSE-SUPERIOR-P LASTPAREN START-DEFUN-BP)
                                        0
                                      *LISP-INDENT-LONE-FUNCTION-OFFSET*))
                              (RETURN BP2))
                             ;; Otherwise line up with start of the second elt of that list
                             (T
                              (RETURN (SKIP-OVER-BLANK-LINES-AND-COMMENTS
                                        (SKIP-OVER-BLANK-LINES-AND-COMMENTS BP1)))))))))
    (OR INDENTATION
        (SETQ INDENTATION (MAX 0 (+ (* OFFSET SPACE-WIDTH) (BP-INDENTATION BP1)))))
    (VALUES INDENTATION IN-STRING)))

(DEFUN GET-FUNCTION-LISP-INDENTATION (SYM &AUX TEM)
  (COND ((CDR (ASSQ SYM *LISP-INDENT-OFFSET-ALIST*)))
        ((OR (AND (FBOUNDP SYM)
                  (SETQ TEM (CDR (ASSQ 'INDENTATION (DEBUGGING-INFO SYM)))))
             (AND (SI:INTERPRETER-SPECIAL-FORM SYM)
                  (SETQ TEM (CDR (ASSQ 'INDENTATION (DEBUGGING-INFO (SI:INTERPRETER-SPECIAL-FORM-HANDLER
                                                                      (SI:INTERPRETER-SPECIAL-FORM SYM))))))))
         (AND (CONSP TEM)
              (= (LENGTH TEM) 1)
              (SETQ TEM (CAR TEM)))
         TEM)
        ((STRING= SYM "DEF" :END1 3)
         *LISP-DEFUN-INDENTATION*)
        (T
         NIL)))

;;>> These next two lists are a horrible kludge.
;;>> What is the right general-porpoise strategy which catches the effects of these
;;>>  two following functions (and also FLET, MACROLET and LABELS)??
(DEFCONST *COND-CLAUSE-SUPERIORS*
          '(COND SELECT SELECTQ SELECTOR COND-EVERY SELECTQ-EVERY SELECT-MATCH
            CASE CASEQ TYPECASE CTYPECASE ETYPECASE CCASE ECASE
            CONDITION-CASE CONDITION-CASE-IF CONDITION-CALL CONDITION-CALL-IF
            WITH-OPEN-FILE-CASE WITH-OPEN-STREAM-CASE
            SIGNAL-PROCEED-CASE UNWIND-PROTECT-CASE FS:READING-FROM-FILE-CASE)
  "Functions whose arguments should be indented internally as clauses.")

(DEFUN COND-CLAUSE-SUPERIOR-P (BP START-DEFUN-BP &AUX SUPBP LINE IDX)
  "T if the list at BP should have its lines indented as a COND clause.
This looks for the function called by the list around BP
in *COND-CLAUSE-SUPERIORS*."
  (AND (NOT (BEG-LINE-P BP))
       (SETQ SUPBP (FORWARD-SEXP BP -1 NIL 1 START-DEFUN-BP NIL))
       (EQ (SETQ IDX (BP-INDEX (IBP SUPBP)) LINE (BP-LINE SUPBP))
           (BP-LINE (SETQ SUPBP (FORWARD-SEXP SUPBP 1 NIL 0
                                              ;; Don't let it take took long!
                                              ;; Quite likely there are closeparens missing
                                              ;; and this would scan to the end of the buffer
                                              (BEG-OF-LINE (LINE-NEXT LINE))))))
       (MEMQ (INTERN-SOFT (STRING-UPCASE (NSUBSTRING LINE IDX (BP-INDEX SUPBP)))
                          SI:PKG-GLOBAL-PACKAGE)
             *COND-CLAUSE-SUPERIORS*)))

(DEFCONST *INDENT-NOT-FUNCTION-SUPERIORS*
          '(LET LET* LETF LETF* FLET LABELS MACROLET LET-GLOBALLY
            LAMBDA PROG PROG* DO DO*
            (DEFSELECT T 1)
            MULTIPLE-VALUE MULTIPLE-VALUE-BIND MULTIPLE-VALUE-SETQ
            (DEFSUBST 2) (DEFMACRO 2)
            (DEFMETHOD 2) (DEFUN 2)
            (DEFFLAVOR 2 T 3 T))
  "List that says which functions have args that are not indented as expressions.
Each element is a function name or a list that starts with one.
If just a function, then that function's first argument is not an expression.
Otherwise, it can be a list (FUNCTION T OFFSET); then all args are not expressions.
Else it can be (FUNCTION ARGNUM OFFSET ARGNUM OFFSET ...)
and each arg whose number is mentioned is not an expression.
The OFFSET is used to indent the second line of the corresponding arg.
An OFFSET can also be missing; that is equivalent to zero.")

(DEFUN INDENT-NOT-FUNCTION-P (BP BP2 LIMIT-BP &AUX SUPBP LINE IDX TEM)
  "Returns non-NIL if the list at BP should not be indented as an expression.
The value may be T, or it may be an offset to use
in indenting the second line of that list.
The decision is based on *INDENT-NOT-FUNCTION-SUPERIORS*.
BP2 should point to the first element of the list that surrounds BP.
LIMIT-BP should be a place to stop backward searches
/(such as the start of the defun)."
  ;; Do that for any list whose car is not an atom,
  ;; unless it is a cond-clause (as determined by the list one level up).
  (OR (AND ( (LIST-SYNTAX (SETQ TEM (BP-CH-CHARACTER BP2))) LIST-ALPHABETIC)
           (NOT (MEMQ TEM '(#/: #// #/|)))   ;These are really atoms
           (NOT (COND-CLAUSE-SUPERIOR-P BP LIMIT-BP)))
      ;; Do that also if the list one level up makes it clear
      ;; that this is a list of variables to be bound, or some such thing.
      (AND (NOT (BEG-LINE-P BP))
           (SETQ SUPBP (FORWARD-SEXP BP -1 NIL 1 LIMIT-BP NIL))
           ;; Does that first element fit on one line?
           (EQ (SETQ IDX (BP-INDEX (IBP SUPBP)) LINE (BP-LINE SUPBP))
               (BP-LINE (SETQ SUPBP (FORWARD-SEXP SUPBP 1 NIL 0 (BEG-LINE SUPBP 1)))))
           ;; Is it a function mentioned in *INDENT-NOT-FUNCTION-SUPERIORS*?
           (SETQ TEM (DO ((SYM (INTERN-SOFT (STRING-UPCASE (NSUBSTRING LINE IDX
                                                                       (BP-INDEX SUPBP)))
                                            SI:PKG-GLOBAL-PACKAGE))
                          (L *INDENT-NOT-FUNCTION-SUPERIORS* (CDR L)))
                         ((NULL L) NIL)
                       (AND (EQ SYM (IF (ATOM (CAR L)) (CAR L) (CAAR L)))
                            (RETURN (CAR L)))))
           ;; TEM is the element of *INDENT-NOT-FUNCTION-SUPERIORS* for our superior.
           ;; Now, is the position of our list in the superior list
           ;; that which the superior says is not a function?
           ;; If it isn't, return T or an offset.
           (COND ((ATOM TEM)
                  (BP-= BP (FORWARD-OVER *WHITESPACE-CHARS* SUPBP)))
                 ((EQ (CADR TEM) T)
                  (IF (NUMBERP (CADDR TEM)) (CADDR TEM) T))
                 (T
                  (DO ((ELTS (CDR TEM) (CDDR ELTS))) ((NULL ELTS))
                    (IF (BP-= BP (FORWARD-OVER *WHITESPACE-CHARS*
                                               (FORWARD-SEXP SUPBP (1- (CAR ELTS)) T 0
                                                             (FORWARD-CHAR BP 1))))
                        (RETURN (IF (NUMBERP (CADR ELTS)) (CADR ELTS) T)))))))))


;;;; Special-purpose lisp indenters

;; funcalled with arguments:
;;  bp to start of current defun
;;  bp to start of line (the one we are indenting)
;;  bp to last unterminated paren (up one level) (such as "(PROG ...)")
;;  bp to start of last complete sexp, or NIL if none at this level.
;;  space width of current font
;;  symbol at start of sexp (such as PROG)
;; values returned:
;;  bp to which to indent, or NIL (may be affected by OFFSET, third value returned)
;;  explicit indentation in pixels desired, or NIL. Overrides any other values
;;  offset from first first value (in space-widths) to indent, or 0
;;   (eg indent 3 spaces from bp to start of tag in a tagbody to get
;;    indentation for start of this sexp)

;;; This is the default indenter for PROG, PROG* and TAGBDODY;
;;;  tags and forms must be handled separately.
(DEFVAR *PROG-TAG-INDENT-OFFSET* -3)
(DEFVAR *PROG-FORM-INDENT-OFFSET* 0)

;; Also used for TAGBODY; then it knows there is no variable list.
(DEFUN INDENT-PROG (IGNORE BP LASTPAREN IGNORE IGNORE SYM)
  (LET* ((THIS-LINE (BP-LINE BP))
         (ATOM-P (EQ (LINE-TYPE THIS-LINE) :ATOM))
         (LAST-TAG-BP NIL)
         (LAST-NON-TAG-BP NIL)
         (BP2 (FORWARD-SEXP (FORWARD-CHAR LASTPAREN)
                            (IF (EQ SYM 'TAGBODY) 1 2)
                            T 0 BP)))
    (AND BP2
         ;; Find the last tag and the last statement.
         (DO ((LINE))
             (NIL)
           (SETQ BP2 (FORWARD-OVER *WHITESPACE-CHARS* BP2))
           (OR (BP-< BP2 BP) (RETURN NIL))
           (AND (BEG-LINE-P (BACKWARD-OVER *BLANKS* BP2))
                (NEQ (SETQ LINE (BP-LINE BP2)) THIS-LINE)
                (CASE (LINE-TYPE LINE)
                  (:ATOM (SETQ LAST-TAG-BP BP2))
                  (:NORMAL (SETQ LAST-NON-TAG-BP BP2))))
           (OR (SETQ BP2 (FORWARD-SEXP BP2 1 NIL 0 BP))
               (RETURN NIL))))
    ;; Try indenting a tag on basis of last tag, and a statement on basis of last statement.
    (COND ((AND ATOM-P LAST-TAG-BP)
           (VALUES LAST-TAG-BP NIL 0))
          ((AND (NOT ATOM-P) LAST-NON-TAG-BP)
           (VALUES LAST-NON-TAG-BP NIL 0))
          ;; This is a statement and nothing so far except tags; use the last tag.
          (LAST-TAG-BP
           (VALUES LAST-TAG-BP NIL
                   (- *PROG-FORM-INDENT-OFFSET*
                      *PROG-TAG-INDENT-OFFSET*)))
          ;; Tag with no preceding tags, only statements.
          (LAST-NON-TAG-BP
           (VALUES LAST-NON-TAG-BP NIL
                   (- *PROG-TAG-INDENT-OFFSET*
                      *PROG-FORM-INDENT-OFFSET*)))
          ;; This is the first tag or statement.
          ((EQ SYM 'TAGBODY)
           (VALUES LASTPAREN NIL
                   (+ 4 (IF ATOM-P *PROG-TAG-INDENT-OFFSET*
                          *PROG-FORM-INDENT-OFFSET*))))
          (T
           (VALUES NIL NIL
                   (IF ATOM-P *PROG-TAG-INDENT-OFFSET*
                     *PROG-FORM-INDENT-OFFSET*))))))

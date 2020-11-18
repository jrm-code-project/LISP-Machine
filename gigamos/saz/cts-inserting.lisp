;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Zwei commands, see ZWEI;COMA for comments

;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-MARK-PAGE "Put point at top of page, mark at end.
A numeric arg specifies the page: 0 for current, 1 for next,
-1 for previous, larger numbers to move many pages.
A new page is started by any line whose first character
is one of the elements of the variable *PAGE-DELIMITER-LIST*." (SM)
  (MULTIPLE-VALUE-BIND (BP1 BP2)
      (MARK-PAGE-INTERNAL (POINT) (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 0))
    (MOVE-BP (POINT) BP1)
    (MOVE-BP (MARK) BP2))
  DIS-BPS)

;;; For COM-MARK-PAGE and COM-SET-BOUNDS-PAGE (latter not yet written).
(DEFUN MARK-PAGE-INTERNAL (BP ARG)
  (COND ((AND (ZEROP ARG)
;character lossage
              (MEMQ (BP-CH-CHAR (BEG-LINE BP)) *PAGE-DELIMITER-LIST*)))
        (( ARG 0)
         (DOTIMES (I (1+ (MINUS ARG)))
           (SETQ BP (FORWARD-PAGE BP -1 T))))
        (T
         (DOTIMES (I ARG)
           (SETQ BP (FORWARD-PAGE BP 1 T)))))
  (VALUES BP (FORWARD-PAGE BP)))

(DEFCOM COM-FORWARD-OVER-MATCHING-DELIMITERS "Move over matching delimiters" (KM)
  (LET ((POINT (POINT))
        (CLOSE) (OPEN))
    (DO ((L *MATCHING-DELIMITER-LIST* (CDR L))
         (CH (CHAR-CODE *LAST-COMMAND-CHAR*)))
        ((NULL L))
      (COND ((= CH (CHAR-INT (CADAR L)))
             (SETQ CLOSE (INT-CHAR CH)
                   OPEN (CAAR L))
             (RETURN))))
    (MOVE-BP POINT (OR (FORWARD-OVER-MATCHING-DELIMITERS POINT *NUMERIC-ARG* NIL 0 OPEN CLOSE)
                       (BARF))))
  DIS-BPS)

(DEFCOM COM-BACKWARD-OVER-MATCHING-DELIMITERS "Move over matching delimiters" (KM)
  (LET ((POINT (POINT))
        (CLOSE) (OPEN))
    (DO ((L *MATCHING-DELIMITER-LIST* (CDR L))
         (CH (CHAR-CODE *LAST-COMMAND-CHAR*)))
        ((NULL L))
      (COND ((= CH (CHAR-INT (CAAR L)))
             (SETQ OPEN (INT-CHAR CH)
                   CLOSE (CADAR L))
             (RETURN))))
    (MOVE-BP POINT (OR (FORWARD-OVER-MATCHING-DELIMITERS POINT (- *NUMERIC-ARG*)
                                                         NIL 0 OPEN CLOSE)
                       (BARF))))
  DIS-BPS)

(DEFCOM COM-MAKE-/(/) "Insert matching delimiters, putting point between them.
With an argument, puts that many s-exprs within the new (),
either by inserting the close delimiter after n s-exprs,
or by inserting the open delimiter before n s-exprs."
        ()
  (LET ((OPEN #/() (CLOSE #/))
        (MOVER 'FORWARD-SEXP) (POINT (POINT)))
    (DO ((CH (CHAR-CODE *LAST-COMMAND-CHAR*))
         (L *MATCHING-DELIMITER-LIST* (CDR L)))
        ((NULL L))
      (WHEN (OR (= CH (CHAR-CODE (CAAR L))) (= CH (CHAR-CODE (CADAR L))))
        (SETQ OPEN (CAAR L) CLOSE (CADAR L) MOVER (CADDAR L))
        (RETURN T)))
    (LET ((BP (IF *NUMERIC-ARG-P*
                  (OR (IF (EQ MOVER 'FORWARD-SEXP)
                          (FORWARD-SEXP POINT *NUMERIC-ARG* NIL 0 NIL T T)      ;No UP
                          (FUNCALL MOVER POINT *NUMERIC-ARG*))
                      (BARF))
                  POINT)))
      (AND (MINUSP *NUMERIC-ARG*) (PSETQ BP POINT POINT BP))
      (INSERT BP (IN-CURRENT-FONT CLOSE))
      (INSERT-MOVING POINT (IN-CURRENT-FONT OPEN))
      DIS-TEXT)))

(DEFCOM COM-MAKE-/(/)-BACKWARD "Insert matching delimiters backwards." ()
  (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*) *NUMERIC-ARG-P* T)
  (COM-MAKE-/(/)))

(DEFCOM COM-DELETE-/(/) "Delete both of the nth innermost pair of parens enclosing point." ()
  (LET ((POINT (POINT)))
    (LET ((BP1 (OR (FORWARD-UP-LIST-OR-STRING POINT *NUMERIC-ARG*) (BARF)))
          (BP2 (OR (FORWARD-UP-LIST-OR-STRING POINT (- *NUMERIC-ARG*) NIL NIL) (BARF))))
      (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) BP1)
      (DELETE-INTERVAL BP2 (FORWARD-CHAR BP2 1))
      DIS-TEXT)))

(DEFCOM COM-MOVE-OVER-/) "Moves over the next ), updating indentation.
Any indentation before the ) is deleted.
LISP-style indentation is inserted after the )." ()
  (LET ((POINT (POINT)) (CHAR NIL)
        (CH *LAST-COMMAND-CHAR*))
    (OR (= (LIST-SYNTAX CH) LIST-CLOSE)
        (DOLIST (L *MATCHING-DELIMITER-LIST*)
          (COND ((= (CHAR-CODE CH) (CHAR-CODE (CADR L)))
                 (SETQ CHAR CH)
                 (RETURN T)))))
    (LET ((BP (OR (IF CHAR (ZWEI-SEARCH (POINT) CHAR) (FORWARD-LIST POINT 1 NIL 1)) (BARF))))
      (MOVE-BP (POINT) BP)
      (LET* ((BP1 (FORWARD-CHAR BP -1))
             (BP2 (BACKWARD-OVER *WHITESPACE-CHARS* BP1)))
        ;; Flush whitespace before this point,
        ;; unless that would move us to the end of a comment.
        (OR (MULTIPLE-VALUE-BIND (NIL NIL X) (LISP-BP-SYNTACTIC-CONTEXT BP2) X)
            (DELETE-INTERVAL BP2 BP1 T)))
      (LET ((ARG (1- *NUMERIC-ARG*)))
        (AND (> ARG 0)
             (MOVE-BP (POINT) (OR (IF CHAR (ZWEI-SEARCH (POINT) CHAR ARG)
                                      (FORWARD-LIST POINT ARG))
                                  (BARF)))))))
  (LET ((*NUMERIC-ARG-P* NIL)
        (*NUMERIC-ARG* 1))
    (COM-INSERT-CRS)
    (COM-INDENT-FOR-LISP))
  DIS-TEXT)

(DEFCOM COM-GROW-LIST-FORWARD
"Move the closing delimiter of the current list forward over one or more sexps.
With negative arg, shrink list by moving closing delimiter backwards.
Marks the end of the resulting list for visibility.
Always leaves point where the same command with a negative arg will undo it." (RM)
  (LET ((OLD-END (OR (FORWARD-LIST (POINT) 1 NIL 1) (BARF)))
        (POINT (POINT)) OLD-END-1)
    (SETQ OLD-END-1 (FORWARD-CHAR OLD-END -1))
    (LET ((NEW-END (OR (FORWARD-SEXP (IF (MINUSP *NUMERIC-ARG*) OLD-END-1 OLD-END)
                                     *NUMERIC-ARG* NIL 0 NIL T T)
                       (BARF))))
      (AND (MINUSP *NUMERIC-ARG*)
           (SETQ NEW-END (BACKWARD-OVER *WHITESPACE-CHARS* NEW-END)))
      (LET ((CHAR (BP-CHAR-BEFORE OLD-END)))
        (WITH-BP (BP NEW-END ':NORMAL)
          (DELETE-INTERVAL OLD-END-1 OLD-END T)
          (INSERT BP CHAR)
          (COND ((BP-< NEW-END POINT)
                 (MOVE-BP POINT NEW-END))
                (T
                 (MOVE-BP (MARK) (FORWARD-CHAR BP 1 T))
                 (SETF (WINDOW-MARK-P *WINDOW*) T)
                 (SETQ *MARK-STAYS* T)))))))
  DIS-TEXT)

(DEFCOM COM-GROW-LIST-BACKWARD
"Move the opening delimiter of the current list backward over one or more sexps.
With negative arg, shrink list by moving opening delimiter forwards.
Marks the beginning of the resulting list for visibility.
Always leaves point where the same command with a negative arg will undo it." (RM)
  (LET ((OLD-BEGIN (OR (FORWARD-LIST (POINT) -1 NIL 1) (BARF)))
        (POINT (POINT)) OLD-BEGIN+1)
    (SETQ OLD-BEGIN+1 (FORWARD-CHAR OLD-BEGIN 1))
    (LET ((NEW-BEGIN (OR (FORWARD-SEXP (IF (MINUSP *NUMERIC-ARG*) OLD-BEGIN+1 OLD-BEGIN)
                                       (- *NUMERIC-ARG*) NIL 0 NIL NIL T)
                         (BARF))))
      (AND (MINUSP *NUMERIC-ARG*)
           (SETQ NEW-BEGIN (FORWARD-OVER *WHITESPACE-CHARS* NEW-BEGIN)))
      (LET ((CHAR (BP-CHAR OLD-BEGIN)))
        (WITH-BP (BP NEW-BEGIN ':MOVES)
          (DELETE-INTERVAL OLD-BEGIN OLD-BEGIN+1 T)
          (INSERT BP CHAR)
          (COND ((BP-< POINT NEW-BEGIN)
                 (MOVE-BP POINT BP))
                (T
                 (MOVE-BP (MARK) (FORWARD-CHAR BP -1 T))
                 (SETF (WINDOW-MARK-P *WINDOW*) T)
                 (SETQ *MARK-STAYS* T)))))))
  DIS-TEXT)


(DEFCOM COM-KILL-BACKWARD-UP-LIST "Delete the list that contains the sexp after point,
but leave that sexp itself." ()
  (LET ((POINT (POINT))
        BP1 BP2 BP3)
    (OR (AND (SETQ BP1 (FORWARD-SEXP POINT -1 NIL 1 NIL NIL))
             (SETQ BP2 (FORWARD-SEXP POINT *NUMERIC-ARG* NIL 0 NIL NIL T))
             (SETQ BP3 (FORWARD-SEXP BP1 1)))
        (BARF))
    (WITH-UNDO-SAVE ("Kill up" BP1 BP3 T)
      (DELETE-INTERVAL BP2 BP3 T)
      (DELETE-INTERVAL BP1 POINT T)))
  DIS-TEXT)

(DEFCOM COM-FORMAT-CODE "Grind the sexp after the pointer.
WARNING: This calls the Lisp grinder, and will delete comments!
A copy of the sexp is first saved on the kill history." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT)))
        (EOF '())
        (POINT (POINT))
        )
    (LET ((SEXP (READ STREAM EOF)))
      (AND (EQ SEXP EOF) (BARF "Missing close parentheses"))
      (WITH-UNDO-SAVE ("Grind" POINT (SEND STREAM ':READ-BP) T)
        (GRIND-INTO-BP (DELETE-INTERVAL POINT (SEND STREAM ':READ-BP)) SEXP))))
  DIS-TEXT)

(DEFCOM COM-FORWARD-PARAGRAPH "Move to start of next paragraph.
Paragraphs are delimited by blank lines or by lines which start with
a delimiter in *PARAGRAPH-DELIMITER-LIST* or in *PAGE-DELIMITER-LIST*.
If there is a fill prefix, any line that does not start with it starts
a paragraph.
Lines which start with a character in *TEXT-JUSTIFIER-ESCAPE-LIST*, if that
character is also in *PARAGRAPH-DELIMITER-LIST*, count as blank lines in
that they separate paragraphs and are not part of them." (KM)
  (MOVE-BP (POINT) (FORWARD-PARAGRAPH (POINT) *NUMERIC-ARG* T))
  DIS-BPS)

(DEFCOM COM-BACKWARD-PARAGRAPH "Move to start of this (or last) paragraph.
See Forward Paragraph for the definition of a paragraph." (KM)
  (MOVE-BP (POINT) (FORWARD-PARAGRAPH (POINT) (- *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-MARK-PARAGRAPH "Set point and mark around current paragraph.
See Forward Paragraph for the definition of a paragraph." (SM)
  (LET ((INT (PARAGRAPH-INTERVAL (POINT) *NUMERIC-ARG*)))
    (MOVE-BP (POINT) (INTERVAL-FIRST-BP INT))
    (MOVE-BP (MARK) (INTERVAL-LAST-BP INT)))
  DIS-BPS)

(DEFCOM COM-FORWARD-SENTENCE "Move to end of this sentence.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." (KM)
  (MOVE-BP (POINT) (FORWARD-SENTENCE (POINT) *NUMERIC-ARG* T))
  DIS-BPS)

(DEFCOM COM-BACKWARD-SENTENCE "Move to beginning of sentence.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." (KM)
  (MOVE-BP (POINT) (FORWARD-SENTENCE (POINT) (- *NUMERIC-ARG*) T))
  DIS-BPS)

(DEFCOM COM-KILL-SENTENCE "Kill one or more sentences forward.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." ()
  (KILL-INTERVAL-ARG (POINT)
                     (FORWARD-SENTENCE (POINT) *NUMERIC-ARG* T)
                     *NUMERIC-ARG*)
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-TEXT)

(DEFCOM COM-BACKWARD-KILL-SENTENCE "Kill one or more sentences backward.
A sentence is ended by a ., ? or ! followed by
two spaces or a CRLF (with optional space), with
any number of /"closing characters/" /", ', ) and ] between.
A sentence also starts after a blank line." ()
  (KILL-INTERVAL-ARG (POINT)
                     (FORWARD-SENTENCE (POINT) (- *NUMERIC-ARG*) T)
                     (- *NUMERIC-ARG*))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-TEXT)

;;; The standard c-G command.
(DEFCOM COM-BEEP "Beep, and if not given a numeric arg turn off the region." ()
  (SEND *STANDARD-INPUT* ':SEND-IF-HANDLES ':MACRO-ERROR)
  (IF (OR *NUMERIC-ARG-P* (NOT (WINDOW-MARK-P *WINDOW*)))
      (BEEP))
  (IF *NUMERIC-ARG-P*
      (SETQ *MARK-STAYS* T)
    (SETQ *MARK-STAYS* NIL))
  DIS-NONE)

;;; The standard c-X c-G command.
(DEFCOM COM-PREFIX-BEEP "Beep and don't do anything else." (KM)
  (SEND *STANDARD-INPUT* ':SEND-IF-HANDLES ':MACRO-ERROR)
  (BEEP)
  DIS-NONE)

;;; The standard c-X abort command.
(DEFCOM COM-PREFIX-ABORT
  "Same as just Abort." (KM)
  (FUNCALL (COMMAND-LOOKUP #/ABORT *COMTAB*)))

(DEFCOM COM-INDENT-FOR-COMMENT "Move to or create comment.
Finds start of existing comments or creates one at end of current line.
With numeric argument, re-aligns existing comments for n lines, but does
not create any.
Note that unlike EMACS, all units are raster pixels, not character counts!
*COMMENT-COLUMN* is the minimum column for aligning comments.
*COMMENT-START* is the string used to recognize existing comments.
*COMMENT-BEGIN* is the string used to start new comments.
*COMMENT-ROUND-FUNCTION* is the function used to compute the column for comments past the
comment column." ()
  (MOVE-BP (POINT)
           (INDENT-FOR-COMMENT (POINT) *NUMERIC-ARG* (NOT *NUMERIC-ARG-P*) *NUMERIC-ARG-P*))
  DIS-TEXT)

(defun indent-for-cts (symbol-string &aux already-indented)
  ;;Insert ";;; <symbol> <date> <user> template" at *comment-start*.
  (let* ((line (bp-line (point)))
                                                ;; $$$  <25-Oct-88 saz>
         (despaced-line (string-trim *whitespace-chars* line))
         ;; Does this line consist solely of 1 or more tabs
         ;; and zero or more spaces?
         (indented-p (and (string-equal "" despaced-line)
                          (setq already-indented t)))
         (*comment-begin* (string-append
                            (cond ((zerop (string-length line)) ";;; ")
                                  (indented-p ";; ")
                                  (t "; "))
                            symbol-string)))
    (if (not already-indented)
        (indent-for-comment *point* 1 t nil t)
      (INSERT (point) *COMMENT-BEGIN*))
    (com-end-of-line)
    (let ((*last-command-char* #\Space))
      (dotimes (i 2) (com-self-insert)))
    (let ((*last-command-char* #\<))
      (com-self-insert))
      ;; to just print out the date, not the time
    (com-insert-short-date)
    (let ((*last-command-char* #\Space))
      (com-self-insert))
    (com-insert-current-user-id)
    (let ((*last-command-char* #\>))
      (com-self-insert))
    (dotimes (i 2)
      (com-backward-sexp))
    (com-backward)))

(defcom com-indent-for-local-modification-cts ";;; $$$ <date> <user> template" ()
  (indent-for-cts "$$$")
  dis-text)

(defcom com-indent-for-systematic-modification-cts ";;; &&& <date> <user> template" ()
  (indent-for-cts "&&&")
  dis-text)

(defcom com-indent-for-enhancement-suggestive-cts ";;; @@@ <date> <user> template" ()
  (indent-for-cts "@@@")
  dis-text)

(defcom com-indent-for-maintenance-suggestive-cts ";;; +++ <date> <user> template" ()
  (indent-for-cts "+++")
  dis-text)

(DEFUN INDENT-FOR-COMMENT (BP &OPTIONAL (TIMES 1) CREATE-P MOVE-TO-NEXT-P BEG-LINE-NOT-SPECIAL
                              &AUX (UP-P 1))
  "Indent the comment on BP's line, or create one if CREATE-P is non-NIL.
Returns a BP that is a copy of the original one, relocated for the insertion.
MOVE-TO-NEXT-P says advance the BP to the following line;
 then TIMES is the number of lines to process.
BEG-LINE-NOT-SPECIAL says do not exempt comments starting at
 the front of the line from relocation."
  (SETQ BP (COPY-BP BP ':MOVES))
  (AND (MINUSP TIMES)
       (SETQ UP-P -1 TIMES (MINUS TIMES)))
  (UNDO-SAVE-CURRENT-RANGE)
  (UNWIND-PROTECT
    (DO ((I 0 (1+ I))
         (LINE)
         (LEN)
         (CH)
         (START-START-INDEX)            ;Index in line of start of existing comment starter.
         (START-END-INDEX))             ;Index in line of end of ...
        (( I TIMES))
      (SETQ LINE (BP-LINE BP)
            LEN (LINE-LENGTH LINE))
      (MULTIPLE-VALUE (START-START-INDEX START-END-INDEX)
        (FIND-COMMENT-START LINE T))
      (COND (START-START-INDEX
             ;; A comment already exists.  Move BP to it.
             (MOVE-BP BP LINE START-START-INDEX)
             ;; Distinguish between ";", ";;" and ";;;" type comments.
             (COND ((AND (ZEROP START-START-INDEX)      ;At the beginning of the line stays
                         (NOT BEG-LINE-NOT-SPECIAL)))
                   ((AND (> LEN (1+ START-START-INDEX))
                         (CHAR-EQUAL (CLI:AREF LINE (1+ START-START-INDEX))
                                     (SETQ CH (CLI:AREF LINE START-START-INDEX))))
                    (COND ((OR ( LEN (+ START-START-INDEX 2))  ; ";;;" doesn't move
                               (NOT (CHAR-EQUAL CH (CLI:AREF LINE (+ START-START-INDEX 2)))))
                           ;; It is a double semicolon, indent as code.
                           (INDENT-LINE BP (INDENT-FOR-LISP BP)))))
                   (T
                    (DELETE-BACKWARD-OVER *BLANKS* BP)
                    (INDENT-TO-COMMENT-COLUMN BP)))
             ;; Now that indentation is adjusted, move over the comment starter.
             (MOVE-BP BP (FORWARD-CHAR BP (- START-END-INDEX START-START-INDEX))))
            (CREATE-P
             ;; No existing comment, and no numeric arg, means make a comment.
             (MOVE-BP BP LINE LEN)              ; Move to end of line
             (DELETE-BACKWARD-OVER *BLANKS* BP)
             (INDENT-TO-COMMENT-COLUMN BP)
             (INSERT BP *COMMENT-BEGIN*)))
      (AND MOVE-TO-NEXT-P                               ; Move to next line
           (MOVE-BP BP (OR (BEG-LINE BP UP-P) (RETURN NIL)))))
    (FLUSH-BP BP))
  BP)

;;; Internal function of above.
(DEFUN INDENT-TO-COMMENT-COLUMN (BP)
  (LET ((HERE (BP-VIRTUAL-INDENTATION BP))
        (GOAL *COMMENT-COLUMN*))
    (COND (( HERE GOAL)
           (SETQ GOAL (FUNCALL *COMMENT-ROUND-FUNCTION* HERE))))
    (INDENT-TO BP GOAL)))

;;; This is the default value of *COMMENT-ROUND-FUNCTION*, the function used
;;; to figure out how to round up the position of comments in an attempt to
;;; make the comments line up nicely.
(DEFUN ROUND-FOR-COMMENT (IND)
  (LET ((SPACE-WIDTH (FONT-SPACE-WIDTH)))
    (LET ((X (* 8 SPACE-WIDTH)))
      (+ *COMMENT-COLUMN* (* (+ (FLOOR (- IND *COMMENT-COLUMN*) X) 1) X)))))

(DEFUN FIND-COMMENT-START (LINE &OPTIONAL BEGIN-ALSO)
  "Return a description of where on LINE a comment starts, if anywhere.
BEGIN-ALSO says, if you cannot find *COMMENT-START*, recognize *COMMENT-BEGIN*.
There are three values: START-START-INDEX, START-END-INDEX, INSIDE-STRING.
The first two are the indices in LINE of the beginning and end
 of the comment starter, or NIL if there is no comment starter.
INSIDE-STRING is non-NIL if this whole line is inside a string.
In that case, you might want to ignore the comment starter even if there is one."
  (DECLARE (RETURN-LIST START-START-INDEX START-END-INDEX INSIDE-STRING))
  (PROG (START-START-INDEX START-END-INDEX INSIDE-STRING)
    (IF (AND *COMMENT-START* (SYMBOLP *COMMENT-START*))
        (MULTIPLE-VALUE (START-START-INDEX START-END-INDEX INSIDE-STRING)
          (FUNCALL *COMMENT-START* LINE))
        (AND (OR *COMMENT-START* BEGIN-ALSO)
             (SETQ START-START-INDEX (STRING-SEARCH (OR *COMMENT-START* *COMMENT-BEGIN*)
                                                    LINE))
             (PROGN
               (SETQ START-END-INDEX
                     (+ START-START-INDEX
                        (STRING-LENGTH (OR *COMMENT-START* *COMMENT-BEGIN*))))
               (WHEN (AND *COMMENT-START* (= (STRING-LENGTH *COMMENT-START*) 1))
                 ;; If *COMMENT-START* is a single character, skip over any number of it,
                 ;; and skip over following spaces as well.
                 (SETQ START-END-INDEX
                       (OR (STRING-SEARCH-NOT-CHAR
                             (CLI:CHARACTER *COMMENT-START*) LINE START-END-INDEX)
                           (STRING-LENGTH LINE)))
                 (SETQ START-END-INDEX
                       (OR (STRING-SEARCH-NOT-SET *BLANKS* LINE START-END-INDEX)
                           (STRING-LENGTH LINE)))))))
    (RETURN (VALUES START-START-INDEX START-END-INDEX INSIDE-STRING))))

(DEFCOM COM-KILL-COMMENT "Delete any comment on the current line.
If region exists, kill all comments in region." ()
  (IF (WINDOW-MARK-P *WINDOW*)
      (COM-KILL-COMMENTS-IN-REGION)
    (LET ((LEN (LINE-LENGTH (BP-LINE (POINT)))))
      (KILL-COMMENT (BP-LINE (POINT)))
      (OR (= LEN (LINE-LENGTH (BP-LINE (POINT))))
          (MOVE-BP (POINT) (END-LINE (POINT))))))
  DIS-TEXT)

; this command used to be misnamed uncomment-region
(DEFCOM COM-KILL-COMMENTS-IN-REGION "Delete any comments within the region." ()
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("Kill Comments" BP1 BP2 T)
      (REGION-LINES (START-LINE STOP-LINE)
        (DO ((LINE START-LINE (LINE-NEXT LINE)))
            ((EQ LINE STOP-LINE))
          (KILL-COMMENT LINE)))))
  DIS-TEXT)

;;; Kill the comment on the line with BP.
(DEFUN KILL-COMMENT (LINE &AUX START-INDEX)
  "Kill any comment found on LINE."
  (AND (SETQ START-INDEX (FIND-COMMENT-START LINE T))
       (LET ((BP (CREATE-BP LINE START-INDEX)))
         (KILL-INTERVAL (BACKWARD-OVER *BLANKS* BP) (END-LINE BP) T)))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL))

(DEFCOM COM-DOWN-COMMENT-LINE "Move to the comment position in the next line.
Equivalent to COM-DOWN-REAL-LINE followed by COM-INDENT-FOR-COMMENT, except
that any blank comment on the current line is deleted first." ()
  (LET ((LINE (BP-LINE (POINT)))
        (LEN (ARRAY-ACTIVE-LENGTH *COMMENT-BEGIN*)))
    (AND (FIND-COMMENT-START LINE T)            ;Will be NIL if line is inside a string!
         ( (LINE-LENGTH LINE) LEN)
         (STRING-EQUAL *COMMENT-BEGIN* LINE     ;Delete any empty comment on this line
                       :START1 0 :START2 (- (LINE-LENGTH LINE) LEN))
         (LET ((BP1 (END-LINE (POINT))))
           (LET ((BP2 (BACKWARD-OVER *BLANKS* (FORWARD-CHAR BP1 (MINUS LEN)))))
             (DELETE-INTERVAL BP2 BP1 T)))))
  (COM-DOWN-REAL-LINE)
  (MULTIPLE-VALUE-BIND (NIL NIL IN-STRING)
      (FIND-COMMENT-START (BP-LINE (POINT)) T)
    (IF IN-STRING
        DIS-TEXT
      (LET ((*NUMERIC-ARG-P* NIL) (*NUMERIC-ARG* 1))
        (COM-INDENT-FOR-COMMENT)))))

(DEFCOM COM-UP-COMMENT-LINE "Move to comment position in the previous line.
Equivalent to COM-UP-REAL-LINE followed by COM-INDENT-FOR-COMMENT, except
that any blank comment on the current line is deleted first." ()
  (LET ((*NUMERIC-ARG* (MINUS *NUMERIC-ARG*)))
    (COM-DOWN-COMMENT-LINE)))

(DEFCOM COM-INDENT-COMMENT-RELATIVE "Align new comment with previous one.
Sets *COMMENT-COLUMN* to position of previous comment then does COM-INDENT-FOR-COMMENT." ()
  (LET (START-INDEX BP)
    ;; Find a line, before our starting one, which has a comment on it.
    (DO ((LINE (LINE-PREVIOUS (BP-LINE (POINT))) (LINE-PREVIOUS LINE)))
        ((NULL LINE) (BARF))
      (SETQ START-INDEX (FIND-COMMENT-START LINE T))
      (AND START-INDEX (RETURN (SETQ BP (CREATE-BP LINE START-INDEX)))))
    (SETQ *COMMENT-COLUMN* (BP-INDENTATION BP))
    (COM-INDENT-FOR-COMMENT)))

(DEFCOM COM-SET-COMMENT-COLUMN "Set *COMMENT-COLUMN* to the current horizontal position.
With an argument, sets it to position of previous comment then aligns or creates a comment
on the current line." ()
  (IF *NUMERIC-ARG-P*
      (LET ((*NUMERIC-ARG-P* NIL)
            (*NUMERIC-ARG* 1))
        (COM-INDENT-COMMENT-RELATIVE))
    (REPORT-COLUMN-SETTING "Comment column"
                           (SETQ *COMMENT-COLUMN* (BP-INDENTATION (POINT))))
    DIS-NONE))

(DEFUN REPORT-COLUMN-SETTING (NAME NPIXELS)
  (IF (NUMBERP NPIXELS)
      (FORMAT *QUERY-IO* "~&~A = ~D pixels (~D spaces)"
              NAME NPIXELS (FLOOR NPIXELS (FONT-SPACE-WIDTH)))
      (FORMAT *QUERY-IO* "~&~A disabled" NAME))
  NPIXELS)

(DEFCOM COM-INDENT-NEW-COMMENT-LINE "Insert newline, then start new comment.
If done when not in a comment, acts like COM-INDENT-NEW-LINE.  Otherwise,
the comment is ended." ()
  (LET ((PT (POINT))
        START END)
    (DELETE-BACKWARD-OVER *BLANKS* PT)
    (MULTIPLE-VALUE (START END)
      (FIND-COMMENT-START (BP-LINE PT)))
    (COND ((OR (NOT START) (< (BP-INDEX PT) START))
           (MUST-REDISPLAY *WINDOW* (KEY-EXECUTE #/CR))
           (COND ((PLUSP (STRING-LENGTH *FILL-PREFIX*))
                  (INSERT-MOVING (POINT) *FILL-PREFIX*)
                  DIS-TEXT)
                 (*SPACE-INDENT-FLAG*
                  (KEY-EXECUTE #/TAB))
                 (T
                  (DELETE-OVER *BLANKS* (POINT))
                  DIS-TEXT)))
          (T
           (INSERT-MOVING PT *COMMENT-END*)
           (INSERT PT (SUBSTRING (BP-LINE PT) START END))
           (MUST-REDISPLAY *WINDOW* (KEY-EXECUTE #/CR))
           (MOVE-BP PT (INDENT-FOR-COMMENT PT 1 NIL NIL T))
           DIS-TEXT))))

(DEFCOM COM-END-COMMENT "Terminate comment on this line and move to the next.
Terminates the comment if there is one on this line and moves to the next line
down.  Primarily useful when a comment terminator exists (TECO or MACSYMA mode)." ()
  (LET ((PT (POINT)))
    (COND ((FIND-COMMENT-START (BP-LINE PT))
           ;; This line has a comment on it.
           (INSERT (END-LINE PT) *COMMENT-END*)
           ;; Make sure interval ends in a newline.
           (UNLESS (EQ (BP-CHARACTER (INTERVAL-LAST-BP *INTERVAL*)) #/NEWLINE)
             (INSERT (INTERVAL-LAST-BP *INTERVAL*) #/NEWLINE))
           (MOVE-BP (LINE-NEXT (BP-LINE PT)) 0)
           DIS-TEXT)
          (T DIS-NONE))))

(DEFCOM COM-SET-FILL-COLUMN "Set the fill column from point's current hpos.
With an argument, if it is less than 200., set fill column to that many characters;
otherwise set it to that many pixels." (KM)
  (SETQ *FILL-COLUMN*
        (REPORT-COLUMN-SETTING "Fill column"
                               (COND (*NUMERIC-ARG-P*
                                      (COND ((< *NUMERIC-ARG* 200.)
                                             (* *NUMERIC-ARG* (FONT-SPACE-WIDTH)))
                                            (T *NUMERIC-ARG*)))
                                     (T (BP-INDENTATION (POINT))))))
  DIS-NONE)

(DEFCOM COM-FILL-PARAGRAPH "Fill (or adjust) this (or next) paragraph.
Point stays the same.  A positive argument means to adjust rather than fill." ()
  (LET ((INT (PARAGRAPH-INTERVAL (POINT))))
    (FILL-INTERVAL INT NIL T (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFCOM COM-FILL-REGION "Fill (or adjust) the region." ()
  (REGION (BP1 BP2)
    (FILL-INTERVAL BP1 BP2 T (AND *NUMERIC-ARG-P* (PLUSP *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFCOM COM-SET-FILL-PREFIX
  "Define Fill Prefix from the current line.
All of the current line up to point becomes the Fill Prefix.
When there is a non-empty fill prefix, any line that fails to start
with the fill prefix is considered a separator of paragraphs.
Fill Region assumes that each non-blank line starts with the prefix
/(which is ignored for filling purposes).  To stop using a Fill Prefix, do
a Set Fill Prefix at the beginning of a line." ()
  (SETQ *FILL-PREFIX* (SUBSTRING (BP-LINE (POINT)) 0 (BP-INDEX (POINT))))
  (FORMAT *QUERY-IO* "~&Fill prefix = ~S" *FILL-PREFIX*)
  DIS-NONE)

(DEFCOM COM-FILL-LONG-COMMENT "Fill a multi-line comment's paragraphs.
An entire run of comment lines is filled, each paragraph separately.
The comments must begin at the start of the line." (KM)
  (LET (BP1 BP2 LINE1 LINE2 (MINEND 177777) LINE3)
    (INTERVAL-LINES ((INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
                    (START-LINE STOP-LINE)
      ;; Find beginning of this run of comment lines.
      (DO ((LINE (BP-LINE (POINT)) (LINE-PREVIOUS LINE)))
          ((NEQ (LINE-TYPE LINE) ':COMMENT))
        (SETQ LINE1 LINE)
        (IF (EQ LINE START-LINE) (RETURN)))
      ;; If we found nothing, point was not on a comment line.
      ;; If point's line is blank and the next nonblank is a comment line, set LINE1 to that.
      (OR LINE1
          (PROGN
            (DO ((LINE (BP-LINE (POINT)) (LINE-NEXT LINE)))
                ((EQ LINE STOP-LINE))
              (SELECTQ (LINE-TYPE LINE)
                (:COMMENT (RETURN (SETQ LINE1 LINE)))
                (:BLANK NIL)
                (T (BARF "Point is not unambiguously at some comment lines."))))))
      (DO ((LINE LINE1 (LINE-NEXT LINE)))
          ((OR (EQ LINE STOP-LINE)
               (NEQ (LINE-TYPE LINE) ':COMMENT))
           (SETQ LINE2 LINE))))
    (AND (EQ LINE1 LINE2) (BARF "No comment starting at beginning of line"))
    (SETQ BP1 (CREATE-BP LINE1 0)
          BP2 (IF LINE2 (DBP (CREATE-BP LINE2 0))
                (INTERVAL-LAST-BP *INTERVAL*)))
    (DO ((LINE LINE1 (LINE-NEXT LINE))
         (START) (END))
        ((EQ LINE LINE2))
      (MULTIPLE-VALUE (START END)
        (FIND-COMMENT-START LINE))
      (WHEN (AND START (< END (STRING-LENGTH LINE)))
        (SETQ LINE3 LINE)                       ;Remember a non-blank line
        (SETQ MINEND (MIN MINEND END))))
    (OR LINE3 (BARF "No comment starting at beginning of line"))
    (LET ((*FILL-PREFIX* (SUBSTRING LINE3 0 MINEND)))
      (FILL-INTERVAL BP1 BP2 T)))
  DIS-TEXT)

(DEFCOM COM-DELETE-HORIZONTAL-SPACE "Delete any spaces or tabs around point.
If given a numeric argument, that many spaces are then inserted." ()
  (DELETE-AROUND *BLANKS* (POINT))
  (AND *NUMERIC-ARG-P* (MOVE-BP (POINT) (INSERT-CHARS (POINT) #/SP *NUMERIC-ARG*)))
  DIS-TEXT)

(DEFCOM COM-BACK-TO-INDENTATION "Move to start of current line and past any blanks.
If there is a fill prefix, move to after any blanks after the fill prefix
/(even if the fill prefix is not blank)." (KM)
  (MOVE-BP (POINT)
           (FORWARD-OVER *BLANKS*
                         (IF (LOOKING-AT (BEG-LINE (POINT)) *FILL-PREFIX*)
                             (CREATE-BP (BP-LINE (POINT)) (STRING-LENGTH *FILL-PREFIX*))
                           (BEG-LINE (POINT)))))
  DIS-BPS)

(DEFCOM COM-UPPERCASE-REGION "Uppercase from point to the mark." ()
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("Upcase" BP1 BP2 T)
      (UPCASE-INTERVAL BP1 BP2 T)))
  DIS-TEXT)

(DEFCOM COM-LOWERCASE-REGION "Lowercase from point to the mark." ()
  (REGION (BP1 BP2)
    (WITH-UNDO-SAVE ("Downcase" BP1 BP2 T)
      (DOWNCASE-INTERVAL BP1 BP2 T)))
  DIS-TEXT)

(DEFCOM COM-UPPERCASE-WORD "Uppercase one or more words.
Moves forward over the words affected.
With a negative argument, uppercases words before point
but does not move point." ()
  (LET ((TEM (FORWARD-WORD (POINT) *NUMERIC-ARG* T)))
    (UPCASE-INTERVAL (POINT) TEM)
    (AND (PLUSP *NUMERIC-ARG*)
         (MOVE-BP (POINT) TEM)))
  DIS-TEXT)

(DEFCOM COM-LOWERCASE-WORD "Lowercase one or more words.
Moves forward over the words affected.
With a negative argument, lowercases words before point
but does not move point." ()
  (LET ((TEM (FORWARD-WORD (POINT) *NUMERIC-ARG* T)))
    (DOWNCASE-INTERVAL (POINT) TEM)
    (AND (PLUSP *NUMERIC-ARG*)
         (MOVE-BP (POINT) TEM)))
  DIS-TEXT)

(DEFCOM COM-UPPERCASE-INITIAL "Put next word in lowercase, but capitalize initial.
With an argument, captializes that many words." ()
  (LET ((BP1 (COPY-BP (POINT))) (ARG *NUMERIC-ARG*))
    (COND ((MINUSP ARG)
           (OR (SETQ BP1 (FORWARD-WORD BP1 ARG)) (BARF))
           (SETQ ARG (MINUS ARG))))
    (DO ((I 0 (1+ I))
         (BP) CH)
        (( I ARG))
      (OR (SETQ BP (FORWARD-TO-WORD BP1)) (BARF))
      (OR (SETQ BP1 (FORWARD-WORD BP)) (BARF))
      (DO-FOREVER
        (SETQ CH (BP-CHARACTER BP))
        (WHEN (OR (BP-= BP BP1)
                  (ALPHA-CHAR-P CH))
          (RETURN))
        (IBP BP))
      (DOWNCASE-INTERVAL BP BP1)
      (UPCASE-CHAR BP))     ;Note no undo hair needed; this is already part of current range.
    (AND (PLUSP *NUMERIC-ARG*) (MOVE-BP (POINT) BP1)))
  DIS-TEXT)

(DEFCOM COM-DELETE-BLANK-LINES "Delete any blank lines around the end of the current line." ()
  (LET ((FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
        (LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
        (LINE (BP-LINE (POINT)))
        (TEM))
    (COND ((LINE-BLANK-P LINE)
           (SETQ TEM LINE)
           (DO ((L TEM))                        ;Move backward over blank lines.
               ((EQ L FIRST-LINE))
             (SETQ L (LINE-PREVIOUS L))
             (OR (LINE-BLANK-P L) (RETURN NIL))
             (SETQ TEM L))
           (MOVE-BP (POINT) TEM 0)
           (DO ((L LINE))                       ;Move forward over more blank lines.
               ((EQ L LAST-LINE))
             (SETQ L (LINE-NEXT L))
             (OR (LINE-BLANK-P L) (RETURN NIL))
             (SETQ LINE L))
           (AND (EQ LINE TEM) (NEQ LINE LAST-LINE) (SETQ LINE (LINE-NEXT LINE)))
           (DELETE-INTERVAL (POINT) (BEG-OF-LINE LINE)))
          (T
           (SETQ TEM (BACKWARD-OVER *BLANKS* (END-OF-LINE LINE)))
           (DO ((L LINE))
               ((EQ L LAST-LINE))
             (SETQ L (LINE-NEXT L))
             (OR (LINE-BLANK-P L) (RETURN NIL))
             (SETQ LINE L))
           (DELETE-INTERVAL TEM (END-OF-LINE LINE)))))
  DIS-TEXT)

(DEFCOM COM-INDENT-RIGIDLY "Shift text in the region sideways as a unit.
All lines in the region have their indentation increased by the numeric
argument of this command (the argument may be negative).  The argument
is a number of SPACE characters in the default font." ()
  (AND (EQ *LAST-COMMAND-TYPE* 'REGION) (SETF (WINDOW-MARK-P *WINDOW*) T))
  (REGION-LINES (START-LINE STOP-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
         (DELTA (* *NUMERIC-ARG* (FONT-SPACE-WIDTH))))
        ((EQ LINE STOP-LINE))
      (INDENT-LINE (CREATE-BP LINE 0) (MAX 0 (+ DELTA (LINE-INDENTATION LINE))))))
  (SETQ *CURRENT-COMMAND-TYPE* 'REGION)
  DIS-TEXT)

(DEFCOM COM-INDENT-REGION "Indent each line in the region.
With no argument, it calls the current TAB command to indent.
With an argument, makes the indentation of each line be as wide as that
many SPACEs in the current font." ()
  (LET ((COMMAND (COMMAND-LOOKUP #/TAB *COMTAB*)))
    (REGION (BP1 BP2)
      (IF (AND (EQ COMMAND 'COM-INDENT-FOR-LISP) (NOT *NUMERIC-ARG-P*))
          (INDENT-INTERVAL-FOR-LISP BP1 BP2 T)  ;Efficiency hack
        (REGION-LINES (START-LINE IGNORE)
          (LET ((WIDTH (* *NUMERIC-ARG*
                          (FONT-SPACE-WIDTH)))
                (STOP-LINE (BP-LINE BP2))
                (POINT (POINT))
                (OLD-POINT (COPY-BP (POINT))))
            (MOVE-BP POINT START-LINE 0)
            (DO-FOREVER
              (AND (ZEROP (BP-INDEX BP2))
                   (EQ STOP-LINE (BP-LINE POINT))
                   (RETURN NIL))
              (IF *NUMERIC-ARG-P*
                  (INDENT-LINE POINT WIDTH)
                (FUNCALL COMMAND))
              (AND (NOT (ZEROP (BP-INDEX BP2)))
                   (EQ STOP-LINE (BP-LINE POINT))
                   (RETURN NIL))
              (MOVE-BP POINT (BEG-LINE POINT 1 T)))
            (MOVE-BP POINT OLD-POINT))))))
  DIS-TEXT)

(DEFCOM COM-STUPID-TAB "Insert spaces to next even multiple of 8 in current font." ()
  (LET ((PT (POINT))
        (FONT-SPACE-WIDTH (FONT-SPACE-WIDTH)))
    (LET ((POS (BP-INDENTATION PT))
          (X (* 10 FONT-SPACE-WIDTH))
          (SPACE (IN-CURRENT-FONT #/SP)))
      (DO ((L (FLOOR (- (* X (1+ (FLOOR POS X))) POS) FONT-SPACE-WIDTH) (1- L)))
          (( L 0))
        (INSERT-MOVING PT SPACE))))
  DIS-TEXT)

(DEFCOM COM-INSERT-TAB "Insert a Tab in the buffer at point." ()
  (DOTIMES (I *NUMERIC-ARG*) (INSERT-MOVING (POINT) #/TAB))
  DIS-TEXT)

(DEFCOM COM-INSERT-FF "Insert a Form-feed in the buffer at point." ()
  (DOTIMES (I *NUMERIC-ARG*) (INSERT-MOVING (POINT) #/FF))
  DIS-TEXT)

(DEFCOM COM-RIGHT-ADJUST-LINE "Adjust the current line to the right margin.
Non-zero argument means adjust from point to the end of the line." ()
  (COND ((NOT *NUMERIC-ARG-P*)
         (MOVE-BP (POINT) (FORWARD-OVER *BLANKS* (BEG-LINE (POINT))))))
  (LET ((LINE (BP-LINE (POINT))))
    (LET ((SWID (STRING-WIDTH LINE
                              (BP-INDEX (POINT))
                              (BP-INDEX (BACKWARD-OVER *BLANKS* (END-LINE (POINT))))))
          (RPOS (OR *FILL-COLUMN* (TV:SHEET-INSIDE-WIDTH (WINDOW-SHEET *WINDOW*)))))
      (MOVE-BP (POINT) (INDENT-TO (POINT) (- RPOS SWID)))))
  DIS-TEXT)

(DEFCOM COM-CENTER-LINE "Center this line's text within the line.
With argument, centers that many lines and moves past." ()
  (COND ((MINUSP *NUMERIC-ARG*)
         (MOVE-BP (POINT) (OR (BEG-LINE (POINT) *NUMERIC-ARG*) (BARF)))
         (SETQ *NUMERIC-ARG* (MINUS *NUMERIC-ARG*))))
  (LET ((SHEET (WINDOW-SHEET *WINDOW*)))
    (DO ((I 0 (1+ I))
         (LINE (BP-LINE (POINT)) (LINE-NEXT LINE))
         (LIMIT-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
         (BP)
         (TEM))
        (( I *NUMERIC-ARG*)
         (AND *NUMERIC-ARG-P* (MOVE-BP (POINT) LINE 0)))
      (SETQ BP (FORWARD-OVER *BLANKS* (BEG-OF-LINE LINE)))
      (SETQ TEM (BP-INDEX BP))
      (SETQ BP (BACKWARD-OVER *BLANKS* (END-LINE BP)))
      (SETQ TEM (STRING-WIDTH LINE TEM (BP-INDEX BP) SHEET))
      (AND (> TEM *FILL-COLUMN*)
           (BARF "The text of the line is too long."))
      (INDENT-LINE BP (TRUNCATE (- *FILL-COLUMN* TEM) 2))
      (COND ((EQ LINE LIMIT-LINE)
             (AND *NUMERIC-ARG-P*
                  (MOVE-BP (POINT) (END-LINE BP)))
             (RETURN NIL)))))
  DIS-TEXT)

(DEFCOM COM-INDENT-NESTED "Indent line for specified nesting level.
With no argument (or argument 1) indents the line at the same nesting
level as the last nonblank line (ie, directly under it).
A larger argument means that this line is that many levels
closer to the surface, and should indent under the last line
above it whose level is the same.  The previous lines are scanned
under the assumption that any line less indented than its successors
is one level higher than they.
However, unindented lines and comment lines are ignored.
If the cursor is not at the beginning of a line, the whole line
is indented, but the cursor stays fixed with respect to the text." ()
  (LET ((PT (POINT))
        (IND-SEEN MOST-POSITIVE-FIXNUM))
    (DO-NAMED LUPO
        ((J 0 (1+ J))
         (LINE (BP-LINE PT))
         (LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
        (( J *NUMERIC-ARG*))
      (DO ((BP)
           (IND))
          ((EQ LINE LIMIT-LINE)
           (SETQ IND-SEEN 0)
           (RETURN-FROM LUPO))
        (SETQ LINE (LINE-PREVIOUS LINE))
        (COND ((NOT (LINE-BLANK-P LINE))
               ;; We have found a non-blank line.
               (SETQ BP (FORWARD-OVER *BLANKS* (BEG-OF-LINE LINE)))
               ;; BP is now just past lines's indentation.
               (UNLESS (OR (AND *COMMENT-START*
                                ;;Lines starting with a comment don't count.
                                (LOOKING-AT BP *COMMENT-START*))
                           ;; Line is unindented, doesn't count.
                           (ZEROP (SETQ IND (LINE-INDENTATION LINE)))
                           ;; Is this less indented than anything we have seen yet?
                           ( IND IND-SEEN))
                 (SETQ IND-SEEN IND)
                 (RETURN NIL))))))
    ;; Now IND-SEEN is the place to which to indent.
    (INDENT-LINE PT IND-SEEN)
    (INDENT-BP-ADJUSTMENT PT))
  DIS-TEXT)

(DEFVAR *STRING-UNDER*)
(DEFCOM COM-INDENT-UNDER "Indent to align under STRING (read from tty).
Searches back, line by line, forward in each line, for a string
that matches the one read and that is more to the right than the
caller's cursor already is.  Indents to align with string found,
removing any previous indentation around point first." ()
  (LET ((ORIGINAL-IND (BP-INDENTATION (POINT)))
        (STRING (TYPEIN-LINE-READLINE "String to align with:"))
        (PT (POINT))
        (STRING-LEN 0)
        (LINE NIL)                              ;The line we finally found.
        (INDENTATION NIL))                      ;Its indentation.
    (SETQ STRING-LEN (STRING-LENGTH STRING)
          LINE (BP-LINE PT))
    (IF (PLUSP STRING-LEN)
        (SETQ *STRING-UNDER* STRING)
      (SETQ STRING *STRING-UNDER*))
    (DO-NAMED LUPO
        ((LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
         (BP (COPY-BP PT)))
        ((EQ LINE LIMIT-LINE)
         (BARF "String not found."))
      (SETQ LINE (LINE-PREVIOUS LINE))
      (SETF (BP-LINE BP) LINE)
      (DO ((INDEX 0))
          ((NULL (SETQ INDEX (STRING-SEARCH STRING LINE (+ STRING-LEN INDEX)))))
        (SETF (BP-INDEX BP) INDEX)
        (WHEN (> (SETQ INDENTATION (BP-INDENTATION BP))
                 ORIGINAL-IND)
          (RETURN-FROM LUPO))))
    (OR (FIND-BP-IN-WINDOW *WINDOW* LINE 0)
        (SEND *QUERY-IO* ':LINE-OUT LINE))
    (INDENT-TO PT INDENTATION))
  DIS-TEXT)

(DEFCOM COM-INDENT-RELATIVE "Indent Relative to the previous line.
With non-null argument, does Tab-to-Tab-Stop.  Otherwise,
Add whitespace characters until underneath an indentation point
in the previous non-null line.  Successive calls find successive
indentation points.  An indentation point is the end
of a sequence of spaces and tabs.  The end of the line counts;
after that, we cycle back to the first suitable indentation.
If there is no suitable indentation point, Tab-to-Tab-Stop
is done." ()
  (LET ((PT (POINT)) IND)
    (IF (OR *NUMERIC-ARG-P*
             (NULL (SETQ IND (INDENT-RELATIVE PT))))
         (COM-TAB-TO-TAB-STOP)
         (DELETE-AROUND *BLANKS* PT)
         (MOVE-BP PT (INDENT-TO PT IND))
         DIS-TEXT)))

(DEFUN INDENT-RELATIVE (BP &OPTIONAL (RESTART-OK T) INDENT-TO-WORDS &AUX START DEST BP1 L)
  (SETQ BP1 (BACKWARD-OVER *BLANKS* BP)
        BP (FORWARD-OVER *BLANKS* BP)
        L (DO ((L (BP-LINE BP))
               (FIRST (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
              ((EQ L FIRST) NIL)
            (SETQ L (LINE-PREVIOUS L))
            (OR (ZEROP (LINE-LENGTH L))
                (RETURN L))))
  (COND ((NULL L) NIL)
        ;; L is the previous non-blank line.
        ;; BP1 is at the beginning of the current line whitespace.
        ((OR (AND (SETQ START (INDENTATION-INDEX L (BP-INDENTATION BP) NIL T))
                  (< START (LINE-LENGTH L)))
             (AND RESTART-OK
                  (SETQ START (INDENTATION-INDEX L (BP-INDENTATION BP1) NIL T))))
         (SETQ DEST (IF (AND INDENT-TO-WORDS (ZEROP START)) START
                        (STRING-SEARCH-SET *BLANKS* L START)))
         (MOVE-BP BP1 L (OR DEST (LINE-LENGTH L)))
         (SETQ BP1 (IF INDENT-TO-WORDS
                       (LET ((BP2 (FORWARD-TO-WORD BP1))
                             (BP3 (END-LINE BP1)))
                         (IF (OR (NULL BP2) (BP-< BP3 BP2))
                             BP3 BP2))
                       (FORWARD-OVER *BLANKS* BP1)))
         (BP-INDENTATION BP1))))

(DEFCOM COM-STACK-LIST-VERTICALLY "Indent the list after point, first insertings crlfs" ()
  (LET ((PT (POINT)))
    (WITH-BP (END (BACKWARD-OVER '(#/) #/SP #/TAB #/CR) (OR (FORWARD-SEXP PT) (BARF)))
                  ':MOVES)
      (DO ((BP (FORWARD-SEXP (FORWARD-LIST PT 1 NIL -1 T)
                             (IF *NUMERIC-ARG-P* 1 2))
               (FORWARD-SEXP BP)))
          ((NOT (BP-< BP END)))
        (INSERT-MOVING BP #/NEWLINE))
      (INDENT-INTERVAL-FOR-LISP PT END T)))
  DIS-TEXT)

(DEFCOM COM-MULTIPLE-TRY-LISP-TAB "Indent line differently if called more than once" ()
  (IF *NUMERIC-ARG-P* (COM-INDENT-FOR-LISP) (COM-INDENT-DIFFERENTLY)))

(DEFVAR *INDENT-DIFFERENTLY-REPETITION-LEVEL*)
(DEFVAR *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*)

(DEFCOM COM-INDENT-DIFFERENTLY "Try to indent this line differently
If called repeatedly, makes multiple attempts." ()
  (LET ((POINT (POINT)) IND)
    (SETQ POINT (FORWARD-OVER *BLANKS* (BEG-LINE POINT)))
    (OR (EQ *LAST-COMMAND-TYPE* 'INDENT-DIFFERENTLY)
        (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 0
              *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS* (LIST (BP-INDENTATION POINT))))
    (SETQ *CURRENT-COMMAND-TYPE* 'INDENT-DIFFERENTLY)
    (DO ((BP (BEG-LINE POINT))
         (TIMES *NUMERIC-ARG*))
        (NIL)
      (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* (1+ *INDENT-DIFFERENTLY-REPETITION-LEVEL*))
      (SETQ IND (COND ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1000)
                       (NTH (- *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1001)
                            *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))
                      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 4)
                       (SETQ IND NIL)
                       (IF (> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 400)
                           (LET ((OIND (CAR *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))
                                 (LINE (BP-LINE BP)))
                             (INDENT-LINE BP OIND)
                             (MOVE-BP BP LINE (INDENTATION-INDEX LINE OIND)))
                           (OR (> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 100)
                               (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 101))
                           (LET ((BP1 (FORWARD-SEXP POINT
                                                    (- 100
                                                       *INDENT-DIFFERENTLY-REPETITION-LEVEL*)
                                                    NIL 0 NIL T T)))
                             (IF BP1
                                 (SETQ IND (BP-INDENTATION
                                             (IF (EQ (BP-LINE BP1) (BP-LINE POINT))
                                                 POINT BP1)))
                                 (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 400))))
                       (OR IND
                           (ATOM-WORD-SYNTAX-BIND
                             (INDENT-RELATIVE BP NIL T))))
                      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 3)
                       (LET ((*LISP-INDENT-OFFSET* 1))
                         (INDENT-FOR-LISP BP)))
                      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 2)
                       (LET ((*LISP-INDENT-OFFSET* 0))
                         (INDENT-FOR-LISP BP)))
                      ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1)
                       (LET ((*LISP-INDENT-OFFSET-ALIST* NIL)
                             (*LISP-DEFUN-INDENTATION* NIL))
                         (INDENT-FOR-LISP BP)))
                      (T
                       (INDENT-FOR-LISP BP))))
      (COND ((NULL IND)
             (SETQ *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1000)
             (SETQ *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*
                   (SI:ELIMINATE-DUPLICATES *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))
             (SETQ *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*
                   (SORT *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS* #'LESSP)))
            ((> *INDENT-DIFFERENTLY-REPETITION-LEVEL* 1000)
             (RETURN T))
            ((NOT (MEMQ IND (PROG1 *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*
                                   (PUSH IND *INDENT-DIFFERENTLY-POSSIBLE-INDENTATIONS*))))
             (OR (PLUSP (SETQ TIMES (1- TIMES))) (RETURN T)))))
    (INDENT-LINE POINT IND)
    (INDENT-BP-ADJUSTMENT (POINT)))
  DIS-TEXT)


#|| ;; These are probably obsolete now that com-comment-out-region
   ;; and com-uncomment-out-region exist.
(DEFCOM COM-/;-REGION "Stick a /";/" in front of every line of the region.
Adds regardless of any that may already be there. Adds more than one if given an argument" ()
  (REGION (BP1 BP2)
    (with-undo-save ("; region" bp1 bp2)
      (DO ((BP (COPY-BP BP1) (MOVE-BP BP (LINE-NEXT L) 0))
           (L (BP-LINE BP1) (BP-LINE BP))
           (L2 (BP-LINE BP2))
           (INSERT (MAKE-ARRAY *NUMERIC-ARG* ':TYPE ART-STRING ':INITIAL-VALUE #/;)))
          ((EQ L L2)
           (INSERT BP INSERT))
        (INSERT BP INSERT))))
  DIS-TEXT)

(DEFCOM COM-UN-/;-REGION "Remove /";/"'s from the beginning of each line
in the region which begins with them. A numeric arg means delete only that many." ()
  (SETQ *NUMERIC-ARG* (IF *NUMERIC-ARG-P* *NUMERIC-ARG* 100))
  (REGION (BP1 BP2)
    (with-undo-save ("Un ; region" bp1 bp2)
      (DO ((BP (COPY-BP BP1) (MOVE-BP BP (LINE-NEXT L) 0))
           (L (BP-LINE BP1) (BP-LINE BP))
           (L2 (BP-LINE BP2)))
          ((EQ L L2)
           (DELETE-/; BP *NUMERIC-ARG*)
           (FLUSH-BP BP))
        (DELETE-/; BP *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFUN DELETE-/; (BP ARG)
  (WHEN (= (BP-CH-CHAR BP) #/;)
    (DELETE-INTERVAL BP (DO ((N 1 (1+ N))
                             (BP1 (COPY-BP BP) (OR (FORWARD-CHAR BP1 1) BP1)))
                            ((OR (> N ARG) ( (BP-CH-CHAR BP1) #/;) )
                             BP1)) T)))
||#

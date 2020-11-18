;;; Macros for ZWEI.   -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFMACRO CHARMAP ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) &BODY BODY)
  "Execute BODY for each character from one BP to another.
Like CHARMAP-PER-LINE except that there are no LINE-FORMS.
See the documentation of CHARMAP-PER-LINE."
  `(CHARMAP-PER-LINE (,FROM-BP-FORM ,TO-BP-FORM . ,RETURN-FORMS) (NIL) . ,BODY))

(DEFMACRO CHARMAP-PER-LINE ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) LINE-FORMS &BODY BODY)
  "Execute BODY for each character from one BP to another.
FROM-BP-FORM and TO-BP-FORM compute the places to start and end the loop.
On reaching the to-bp, RETURN-FORMS are evaluated and the last one's values returned.
LINE-FORMS are evaluated at the beginning of each line.

Within the BODY and LINE-FORMS, you can use (CHARMAP-CHARACTER) to get the
next character (the one after the point the loop has reached)
and (CHARMAP-SET-CHAR char) to modify it.  (CHARMAP-LINE) returns the current line.
/(CHARMAP-BP-BEFORE) returns a BP before that character and
/(CHARMAP-BP-AFTER) returns a BP to after it.
/(CHARMAP-INCREMENT) skips one extra character so that the BODY
 will not be executed for it.
/(CHARMAP-RETURN forms) returns the values of the forms from the CHARMAP."
  `(LET ((*FROM-BP* ,FROM-BP-FORM)
         (*TO-BP* ,TO-BP-FORM))
     (BLOCK *CHARMAP*
       (DO ((LINE (BP-LINE *FROM-BP*) (LINE-NEXT LINE))
            (*FIRST-INDEX* (BP-INDEX *FROM-BP*) 0)
            (*LAST-LINE* (BP-LINE *TO-BP*))
            (*THIS-IS-THE-LAST-LINE*))
           (())
         (SETQ *THIS-IS-THE-LAST-LINE* (or (EQ LINE *LAST-LINE*)
                                           (null (line-next line))))  ;defensive
         ,@LINE-FORMS
         (DO ((INDEX *FIRST-INDEX* (1+ INDEX))
              (*LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE* (1- (BP-INDEX *TO-BP*))
                              (LINE-LENGTH LINE))))
             ((> INDEX *LAST-INDEX*)
              (IF *THIS-IS-THE-LAST-LINE*
                  (RETURN-FROM *CHARMAP* . ,RETURN-FORMS)))
           . ,BODY)))))

;;>> should these things should be macroletted? (we'd lose the documentation, though, that way)
(DEFMACRO CHARMAP-RETURN RETURN-FORMS
  "Within the body of a CHARMAP, return from it.
The values returned are the values of the RETURN-FORMS."
  `(RETURN-FROM *CHARMAP* . ,RETURN-FORMS))

(DEFMACRO CHARMAP-CHARACTER ()
  "Within the body of a CHARMAP, the character being scanned."
  `(IF (AND (= INDEX *LAST-INDEX*)
            (NOT *THIS-IS-THE-LAST-LINE*))
       #/NEWLINE
       (CL:AREF LINE INDEX)))
(DEFMACRO CHARMAP-CHAR ()
  "Within the body of a CHARMAP, INT-CHAR of the character being scanned."
  `(IF (AND (= INDEX *LAST-INDEX*)
            (NOT *THIS-IS-THE-LAST-LINE*))
       #/NEWLINE
       (ZL:AREF LINE INDEX)))

(DEFMACRO CHARMAP-CH-CHARACTER ()
  "Within the body of a CHARMAP, the character being scanned, sans font and meta-bits"
  `(INT-CHAR (CHAR-CODE (CHARMAP-CHARACTER))))
(DEFMACRO CHARMAP-CH-CHAR ()
  "Within the body of a CHARMAP, CHAR-CODE of the character being scanned."
  `(CHAR-CODE (CHARMAP-CHARACTER)))

(DEFMACRO CHARMAP-SET-CHAR (CHAR)
  "Within the body of a CHARMAP, set the character being scanned at to CHAR."
  `(PROGN
    (MUNG-BP-INTERVAL *FROM-BP*)
    (MUNG-LINE LINE)
    (IF (AND (= INDEX *LAST-INDEX*)
             (NOT *THIS-IS-THE-LAST-LINE*))
        (INSERT (DELETE-INTERVAL (END-LINE LINE) (BEG-LINE LINE 1) T) ,CHAR)
        (SETF (CHAR LINE INDEX) ,CHAR))))

(DEFMACRO CHARMAP-BP-BEFORE ()
  "Within the body of a CHARMAP, return a bp to before the character being scanned."
  `(CREATE-BP LINE INDEX))

(DEFMACRO CHARMAP-BP-AFTER ()
  "Within the body of a CHARMAP, return a bp to after the character being scanned."
  `(IF (AND (= INDEX *LAST-INDEX*)
            (NOT *THIS-IS-THE-LAST-LINE*))
       (CREATE-BP (LINE-NEXT LINE) 0)
       (CREATE-BP LINE (1+ INDEX))))

(DEFMACRO CHARMAP-INCREMENT RETURN-FORMS
  "Within the body of a CHARMAP, skip over one character.
Doing this changes which character CHARMAP-CHAR, CHARMAP-SET-CHAR,
CHARMAP-BP-BEFORE and CHARMAP-BP-AFTER refer to."
  `(WHEN (> (SETQ INDEX (1+ INDEX)) *LAST-INDEX*)
     (IF *THIS-IS-THE-LAST-LINE*
         (RETURN-FROM *CHARMAP* . ,RETURN-FORMS)
       (SETQ INDEX 0
             LINE (LINE-NEXT LINE)
             *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*)
             *LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE* (1- (BP-INDEX *TO-BP*))
                            (LINE-LENGTH LINE))))))

(DEFMACRO CHARMAP-LINE ()
  "Within the body of a CHARMAP, the line being scanned."
  `LINE)

(DEFMACRO RCHARMAP ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) &BODY BODY)
  "Like CHARMAP but scans backwards through the interval.
The to-bp should be earlier in the interval than the from-bp.
Instead of using CHARMAP-CHAR, CHARMAP-BP-BEFORE, etc.,
you use RCHARMAP-CHAR, RCHARMAP-BP-BEFORE, etc."
  `(RCHARMAP-PER-LINE (,FROM-BP-FORM ,TO-BP-FORM . ,RETURN-FORMS) (NIL) . ,BODY))

(DEFMACRO RCHARMAP-PER-LINE ((FROM-BP-FORM TO-BP-FORM . RETURN-FORMS) LINE-FORMS &BODY BODY)
  "Like CHARMAP-PER-LINE but scans backwards through the interval.
The to-bp should be earlier in the interval than the from-bp.
Instead of using CHARMAP-CHAR, CHARMAP-BP-BEFORE, etc.,
you use RCHARMAP-CHAR, RCHARMAP-BP-BEFORE, etc."
  `(LET ((*FROM-BP* ,FROM-BP-FORM)
         (*TO-BP* ,TO-BP-FORM))
     (BLOCK *RCHARMAP*
       (DO ((LINE (BP-LINE *FROM-BP*) (LINE-PREVIOUS LINE))
            (*FIRST-LINE-P* T NIL)
            (*LAST-LINE* (BP-LINE *TO-BP*))
            (*THIS-IS-THE-LAST-LINE*))
           (())
         (SETQ *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*))
         ,@LINE-FORMS
         ;; Note that index can take on the value of the length of a line, which means the CR
         (DO ((INDEX (IF *FIRST-LINE-P* (1- (BP-INDEX *FROM-BP*)) (LINE-LENGTH LINE))
                     (1- INDEX))
              (*LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE*
                                (BP-INDEX *TO-BP*)
                              0)))
             ((< INDEX *LAST-INDEX*)
              (IF *THIS-IS-THE-LAST-LINE*
                  (RETURN-FROM *RCHARMAP* . ,RETURN-FORMS)))
           . ,BODY)))))

(DEFMACRO RCHARMAP-RETURN RETURN-FORMS
  "Within the body of a RCHARMAP, return from it.
The values returned are the values of the RETURN-FORMS."
  `(RETURN-FROM *RCHARMAP* . ,RETURN-FORMS))

(DEFMACRO RCHARMAP-CHARACTER ()
  "Within the body of a RCHARMAP, the character at the scanning point."
  `(IF (= INDEX (LINE-LENGTH LINE))
       #/NEWLINE
       (CHAR LINE INDEX)))
(DEFMACRO RCHARMAP-CHAR ()
  "Within the body of a RCHARMAP, INT-CHAR of the character at the scanning point."
  `(IF (= INDEX (LINE-LENGTH LINE))
       #/NEWLINE
       (ZL:AREF LINE INDEX)))


(DEFMACRO RCHARMAP-CH-CHARACTER ()
  "Within the body of a RCHARMAP, the character at the scanning point sans font and meta-bits"
  `(INT-CHAR (CHAR-CODE (RCHARMAP-CHARACTER))))
(DEFMACRO RCHARMAP-CH-CHAR ()
  "Within the body of a RCHARMAP, CHAR-CODE of the character at the scanning point."
  `(CHAR-CODE (RCHARMAP-CHARACTER)))

(DEFMACRO RCHARMAP-CHARACTER-BEFORE ()
  "Within the body of a RCHARMAP, the character before the scanning point."
  `(IF (ZEROP INDEX)
       #/NEWLINE
       (CHAR LINE (1- INDEX))))
(DEFMACRO RCHARMAP-CHAR-BEFORE ()
  "Within the body of a RCHARMAP, INT-CHAR of the character before the scanning point."
  `(IF (ZEROP INDEX)
       #/NEWLINE
       (ZL:AREF LINE (1- INDEX))))

(DEFMACRO RCHARMAP-CH-CHARACTER-BEFORE ()
  "Within the body of a RCHARMAP, the character before the scanning point,
sans font and meta-bits."
  `(CHAR-INT (CHAR-CODE (RCHARMAP-CHARACTER-BEFORE))))
(DEFMACRO RCHARMAP-CH-CHAR-BEFORE ()
  "Within the body of a RCHARMAP, CHAR-CODE of the character before the scanning point."
  `(CHAR-CODE (RCHARMAP-CHARACTER-BEFORE)))

(DEFMACRO RCHARMAP-SET-CHAR (CHAR)
  "Within the body of a RCHARMAP, set the character at the scanning point to CHAR."
  `(PROGN
    (MUNG-BP-INTERVAL *FROM-BP*)
    (MUNG-LINE LINE)
    (IF (= INDEX (LINE-LENGTH LINE))
        (INSERT (DELETE-INTERVAL (END-LINE LINE) (BEG-LINE LINE 1) T) ,CHAR)
        (SETF (CHAR LINE INDEX) ,CHAR))))

(DEFMACRO RCHARMAP-BP-BEFORE ()
  "Within the body of a RCHARMAP, return a bp to before the character at the scanning point."
  `(CREATE-BP LINE INDEX))

(DEFMACRO RCHARMAP-BP-AFTER ()
  "Within the body of a RCHARMAP, return a bp to after the char at the scanning point."
  `(IF (= INDEX (LINE-LENGTH LINE))
       (CREATE-BP (LINE-NEXT LINE) 0)
     (CREATE-BP LINE (1+ INDEX))))

(DEFMACRO RCHARMAP-DECREMENT RETURN-FORMS
  "Within the body of a RCHARMAP, skip backward over one character.
Doing this changes which character RCHARMAP-CHAR, RCHARMAP-SET-CHAR,
RCHARMAP-BP-BEFORE and RCHARMAP-BP-AFTER refer to."
  `(COND ((< (SETQ INDEX (1- INDEX)) *LAST-INDEX*)
          (COND (*THIS-IS-THE-LAST-LINE*
                 (RETURN-FROM *RCHARMAP* . ,RETURN-FORMS))
                (T (SETQ LINE (LINE-PREVIOUS LINE)
                         *FIRST-LINE-P* NIL
                         INDEX (LINE-LENGTH LINE)
                         *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*)
                         *LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE* (BP-INDEX *TO-BP*) 0)))))))

(DEFMACRO RCHARMAP-LINE ()
  "Within the body of a RCHARMAP, the line being scanned."
  `LINE)

;;; Am I PUSHNEWing yet?
(DEFMACRO PUSH* (I R)
  "Push the value of I onto the list in R unless I is already a member of that list."
  `(OR (MEMQ ,I ,R) (PUSH ,I ,R)))

(DEFMACRO ORDER-BPS (BP1 BP2)
  "Given two variables BP1 and BP2, swap their values if nec so that BP1 is before BP2.
The values of both variables are assumed to be buffer pointers."
  `(IF (BP-< ,BP2 ,BP1)
     (PSETQ ,BP1 ,BP2 ,BP2 ,BP1)))

(DEFMACRO WORD-SYNTAX (CHAR)
  "Return the word syntax setting for character CHAR."
  `(CHAR-SYNTAX ,CHAR *MODE-WORD-SYNTAX-TABLE*))

(DEFMACRO ATOM-WORD-SYNTAX (CHAR)
  "Return the word syntax setting for character CHAR for Atom Word Mode."
  `(CHAR-SYNTAX ,CHAR *ATOM-WORD-SYNTAX-TABLE*))

(DEFMACRO LIST-SYNTAX (CHAR)
  "Return the list syntax setting for character CHAR."
  `(CHAR-SYNTAX ,CHAR *MODE-LIST-SYNTAX-TABLE*))

(DEFMACRO LIST-SYNTAX-OPEN-CODED (CHAR)
  "Return the list syntax setting for character CHAR."
  `(CHAR-SYNTAX-OPEN-CODED ,CHAR *MODE-LIST-SYNTAX-TABLE*))

(DEFSUBST CHAR-SYNTAX-OPEN-CODED (CHAR SYNTAX-TABLE)
  "Like CHAR-SYNTAX but faster."
  (IF (ARRAYP SYNTAX-TABLE)
      (AREF SYNTAX-TABLE (CHAR-CODE CHAR))
    (CHAR-SYNTAX CHAR SYNTAX-TABLE)))

(DEFMACRO ATOM-WORD-SYNTAX-BIND (&BODY BODY)
  "Execute BODY in Atom Word mode.
The Atom Word syntax table is temporarily installed as the word syntax table."
  `(LET ((*MODE-WORD-SYNTAX-TABLE* *ATOM-WORD-SYNTAX-TABLE*))
     . ,BODY))

(DEFMACRO BP-CH-CHARACTER (BP)
  "Return the character after BP sans font and meta-bits"
  `(INT-CHAR (CHAR-CODE (BP-CHARACTER ,BP))))
(DEFMACRO BP-CH-CHAR (BP)
  "Return CHAR-CODE of the character after BP."
  `(CHAR-CODE (BP-CHAR ,BP)))

;;; Is it really necessary/right to count lines like this???
(DEFMACRO PRESERVE-POINT (&BODY BODY)
  "Execute BODY, moving (POINT) back afterward to where it was before BODY."
  `(LET ((NLINES (1- (COUNT-LINES (INTERVAL-FIRST-BP *INTERVAL*) (POINT) T)))
         (NCHARS (BP-INDEX (POINT))))
     (PROGN . ,BODY)
     (MOVE-BP (POINT)
              (FORWARD-CHAR (FORWARD-LINE (INTERVAL-FIRST-BP *INTERVAL*)
                                          NLINES T)
                            NCHARS T))))

(DEFMACRO PRESERVE-BUFFER-POINT ((BUFFER) &BODY BODY)
  "Execute BODY, preserving BUFFER's POINT whether BUFFER is selected or not."
  `(LET (SAVED-POINT NLINES NCHARS)
     (SETQ SAVED-POINT (SEND ,BUFFER :POINT))
     (SETQ NLINES (COUNT-LINES (INTERVAL-FIRST-BP ,BUFFER) SAVED-POINT T)
           NCHARS (BP-INDEX SAVED-POINT))
     (PROGN . ,BODY)
     (MOVE-BP (SEND ,BUFFER :POINT)
              (LET ((*INTERVAL* ,BUFFER))       ;Range check right
                (FORWARD-CHAR (FORWARD-LINE (INTERVAL-FIRST-BP ,BUFFER)
                                            (1- NLINES) T)
                              NCHARS T)))))

(DEFMACRO KILL-NEW-BUFFER-ON-ABORT ((BUFFER) &BODY BODY)
  "If the BODY aborts, kill the current buffer and reselect BUFFER.
Do not do either thing if the BODY returns normally."
  `(LET (.WINS. (.BUFFER. ,BUFFER))
     (UNWIND-PROTECT
       (PROG1 (PROGN . ,BODY)
              (SETQ .WINS. T))
       (OR .WINS. (EQ *INTERVAL* .BUFFER.)
           (LET ((OBUFFER *INTERVAL*))
             (SEND .BUFFER. :SELECT)
             (KILL-BUFFER OBUFFER T))))))

(DEFMACRO WITH-REGION-OR-WHOLE-INTERVAL ((REGION-P-VAR) &BODY BODY)
  "If there is a mark now, bind *INTERVAL* to the region and REGION-P-VAR to T."
  `(MULTIPLE-VALUE-BIND (*INTERVAL* ,REGION-P-VAR)
       (REGION-OR-WHOLE-INTERVAL)
     (UNWIND-PROTECT
       (PROGN . ,BODY)
       (COND (,REGION-P-VAR
              (FLUSH-BP (INTERVAL-FIRST-BP *INTERVAL*))
              (FLUSH-BP (INTERVAL-LAST-BP *INTERVAL*)))))))

(DEFMACRO DPRINT LIST
  (DO ((L LIST (CDR L))
       (RET NIL (CONS `(FORMAT T "~S = ~S; " ',(CAR L) ,(CAR L)) RET)))
      ((NULL L)
       `(PROGN (TERPRI) . ,(NREVERSE RET)))))

(DEFMACRO CURRENT-FONT (WINDOW &OPTIONAL (NUMBER '*FONT*))
  "Return the font which is now current in the zwei-window WINDOW.
The value of *FONT* is used as the current font number.
If NUMBER is specified, it is an expression for the font number.
This should be used only on WINDOWs, not other kinds of displayers."
  `(AREF (TV:SHEET-FONT-MAP (WINDOW-SHEET ,WINDOW)) ,NUMBER))

(DEFMACRO TYPEIN-LINE-ACTIVATE (&BODY BODY)
  "Execute BODY with a cursor blinking in the typein line."
  `(LET (OLD-SELECTED-WINDOW OLD-SUBSTITUTE)
     (UNWIND-PROTECT
       (PROGN
         (COND ((SEND *QUERY-IO* :OPERATION-HANDLED-P :SELECT)
                (SEND *QUERY-IO* :SEND-IF-HANDLES :OUTPUT-HOLD-EXCEPTION)
                (SETQ OLD-SELECTED-WINDOW
                      (SEND *QUERY-IO* :TOP-OF-EDITOR-HIERARCHY))
                (SETQ OLD-SUBSTITUTE (SEND OLD-SELECTED-WINDOW :SELECTION-SUBSTITUTE))
                (SEND OLD-SELECTED-WINDOW :SET-SELECTION-SUBSTITUTE *TYPEIN-WINDOW*)))
         . ,BODY)
       (AND OLD-SELECTED-WINDOW
            (SEND OLD-SELECTED-WINDOW :SET-SELECTION-SUBSTITUTE OLD-SUBSTITUTE)))))

(DEFMACRO TEMPORARY-WINDOW-SELECT ((ZWEI-WINDOW) &BODY BODY)
  "Temporarily select ZWEI-WINDOW, such as the one used for View File, etc.
We arrange that any attempt to use the original window for typeout
selects it again and deactivates the temporary window."
  `(LET* ((NEW-WINDOW ,ZWEI-WINDOW)
          (*TERMINAL-IO* (SEND NEW-WINDOW :TYPEOUT-WINDOW)))
     (TV:WITH-SELECTION-SUBSTITUTE
       (NEW-WINDOW (SEND *WINDOW* :TOP-OF-EDITOR-HIERARCHY))
       . ,BODY)))

(DEFMACRO WITHOUT-IO-BUFFER-OUTPUT-FUNCTION (&BODY BODY)
  "Executes BODY inhibiting synchronous intercepted characters (Break, Abort, etc).
Asynchronous characters such as Control-Abort are not affected."
  `(LET (TV:KBD-INTERCEPTED-CHARACTERS)
     . ,BODY))

;;;; Macros used to make command easy to write.
(DEFMACRO POINT ()
  "The current editing pointer in the current window."
  `(WINDOW-POINT *WINDOW*))

(DEFMACRO MARK ()
  "The current window's mark, or NIL."
  `(WINDOW-MARK *WINDOW*))

(DEFMACRO REGION ((BP1 BP2) &BODY BODY)
  "Execute BODY with BP1, BP2 bound to bps to the ends of the region.
Barfs if there is no region now."
  `(LET ((,BP1 (POINT)) (,BP2 (MARK)))
     (COND ((NOT (WINDOW-MARK-P *WINDOW*))
            (BARF "There is no region."))
           ((BP-< ,BP2 ,BP1)
            (PSETQ ,BP1 ,BP2 ,BP2 ,BP1)))
     . ,BODY))

(DEFMACRO REGION-LINES ((START-LINE STOP-LINE) &BODY BODY)
  "Execute BODY with START-LINE, END-LINE bound to the first and last lines in the region.
Barfs if there is no region now.
A line is /"in/" the region iff its beginning is included."
  `(LET ((REGION-LINES-BP1 (POINT)) (REGION-LINES-BP2 (MARK)))
     (COND ((NOT (WINDOW-MARK-P *WINDOW*))
            (BARF "There is no region."))
           ((BP-< REGION-LINES-BP2 REGION-LINES-BP1)
            (PSETQ REGION-LINES-BP1 REGION-LINES-BP2
                   REGION-LINES-BP2 REGION-LINES-BP1)))
     (INTERVAL-LINES (REGION-LINES-BP1 REGION-LINES-BP2)
       (,START-LINE ,STOP-LINE)
       . ,BODY)))

(DEFMACRO INTERVAL-LINES ((BP1 BP2) (START-LINE STOP-LINE) &BODY BODY)
  "Executes BODY with START-LINE, STOP-LINE bound to the first and last lines
in the interval designated by BP1, BP2.  BP1, BP2 are inputs.
A line is /"in/" the interval iff its beginning is included."
  `(LET ((,START-LINE
          (COND ((ZEROP (BP-INDEX ,BP1))
                 (BP-LINE ,BP1))
                (T (LINE-NEXT (BP-LINE ,BP1)))))
         (,STOP-LINE
          (COND ((ZEROP (BP-INDEX ,BP2))
                 (BP-LINE ,BP2))
                (T (LINE-NEXT (BP-LINE ,BP2))))))
     . ,BODY))

;;;; PRESERVE-POINT

(DEFMACRO WITH-BP ((VARIABLE BP TYPE) &BODY BODY)
  "Execute BODY with VARIABLE bound to a bp copied from BP but with type TYPE.
TYPE is :NORMAL or :MOVES.  It could be NIL, but that probably defeats the purpose.
The new BP is FLUSH-BP'd when BODY is finished."
  `(LET ((,VARIABLE (COPY-BP ,BP ,TYPE)))
     (UNWIND-PROTECT
       (PROGN . ,BODY)
       (FLUSH-BP ,VARIABLE))))

(DEFMACRO BIND-MODE-LINE (LIST &BODY BODY)
  "Execute BODY with LIST as the mode-line list."
  `(LET ((*MODE-LINE-LIST* ,LIST))
     . ,BODY))

(DEFMACRO BIND-MOUSE-DOCUMENTATION-STRING (STRING &BODY BODY)
  "Execute BODY with STRING displayed in the who line as mouse documentation."
  `(LET ()
     (BIND (SEND *WINDOW* :MOUSE-DOCUMENTATION-STRING-LOCATION) ,STRING)
     . ,BODY))

;;; This is for things that take an interval as an argument, or two bps, maybe in order,
;;; it canonicalises them into two ordered bps
(DEFMACRO GET-INTERVAL (START-BP END-BP IN-ORDER-P)
  "Given variables START-BP, END-BP that specify an interval, canonicalize them.
There are two valid ways thet interval can be specified.
Either START-BP can actually be an interval, or START-BP and END-BP
can be buffer pointers.  In that case, IN-ORDER-P says it is ok
to assume that START-BP comes earlier in the buffer than END-BP.

When this macro exits, START-BP and END-BP are always buffer pointers,
and always in order (unless you lied with specifying IN-ORDER-P)."
  `(COND ((NULL ,END-BP)
          (SETQ ,END-BP (INTERVAL-LAST-BP ,START-BP)
                ,START-BP (INTERVAL-FIRST-BP ,START-BP)))
         ((NOT ,IN-ORDER-P)
          (ORDER-BPS ,START-BP ,END-BP))))

(DEFMACRO WITH-READ-ONLY-SUPPRESSED ((INTERVAL) &BODY BODY)
  "Execute BODY, allowing it to modify INTERVAL even if it is read-only."
  `(LET ((*READ-ONLY-SUPPRESSED-INTERVAL* ,INTERVAL))
     . ,BODY))

(DEFMACRO WITH-UNDO-SAVE ((TYPE BP1 BP2 IN-ORDER-P) &BODY BODY)
  "Record changes made by BODY to interval from BP1 to BP2 for undo.
TYPE is a string saying the kind of operation, for the user.
It should start with a capital letter."
  `(UNWIND-PROTECT
     (LET ((*BATCH-UNDO-SAVE* T))
       (UNDO-SAVE-BEGIN ,BP1 ,BP2 ,IN-ORDER-P ,TYPE)
       . ,BODY)
     (UNDO-SAVE-END)))

(DEFMACRO WITH-UNDO-SAVE-IF (COND-FORM (TYPE BP1 BP2 IN-ORDER-P) &BODY BODY)
  "Record changes made by BODY to interval from BP1 to BP2 for undo, if COND-FORM evals non-NIL.
TYPE is a string saying the kind of operation, for the user.
It should start with a capital letter."
  (LET ((GENSYM (GENSYM)))
    `(LET ((,GENSYM ,COND-FORM))
       (UNWIND-PROTECT
           (LET-IF ,GENSYM ((*BATCH-UNDO-SAVE* T))
             (WHEN ,GENSYM (UNDO-SAVE-BEGIN ,BP1 ,BP2 ,IN-ORDER-P ,TYPE))
             . ,BODY)
         (WHEN ,GENSYM (UNDO-SAVE-END))))))

;;;; DEFCOM
;;; Defines a command.  Form is:
;;; (DEFCOM COM-foo "Documentation." OPTIONS-LIST . BODY)
;;; Note: unlike EINE, there is no lambda-list.
(DEFMACRO DEFCOM (FN &BODY REST)
  "Options are:  (KM)   -- This command always preserves MARK.
              (SM)   -- Set MARK. There will be a region after this command
              (NM)   -- Reset MARK. There will be no region after this command
              (PUSH) -- Push POINT onto the point-pdl
              (R)    -- Frob recentering fraction for forward move
              (-R)   -- Frob recentering fraction for backward move."
  ;; use this so indentation for defcom is pretty
  (DECLARE (ARGLIST FN DOC OPTIONS &BODY DEF))
  (declare (zwei:indentation 3 1))
  (LET ((DOC (CAR REST))
        (OPTIONS (CADR REST))
        (DEF (CDDR REST)))
    `(PROGN
       (COMMAND-DEFINE ',FN ',DOC ',OPTIONS)
       (DEFUN ,FN ()
         ,@(PROCESS-COMMAND-OPTIONS OPTIONS)
         . ,DEF))))

(DEFVAR *COMMAND-ALIST* NIL
  "Alist of all ZWEI command names (strings) versus the commands themselves (symbols).")

(DEFUN COMMAND-DEFINE (COMMAND DOC IGNORE)
  (COND ((STRINGP DOC)
         (PUTPROP COMMAND DOC 'DOCUMENTATION))
        ((OR (SYMBOLP DOC)
             (AND (NOT (ATOM DOC))
                  (MEMQ (CAR DOC) '(FUNCTION LAMBDA))))
         (PUTPROP COMMAND DOC 'DOCUMENTATION-FUNCTION))
        (T
         (FERROR NIL "The command ~S has invalid self-documentation ~S" COMMAND DOC)))
  (LET* ((NAME (MAKE-COMMAND-NAME COMMAND))
         (AENTRY (ASS 'EQUALP NAME *COMMAND-ALIST*)))
    (PUTPROP COMMAND NAME 'COMMAND-NAME)
    (IF AENTRY
        (UNLESS (EQ COMMAND (CDR AENTRY))
          (WHEN (FQUERY NIL "The command name ~S is currently set up to call ~S;
redefine it to call ~S? "
                        NAME (CDR AENTRY) COMMAND)
            (SETF (CDR AENTRY) COMMAND)))
      (PUSH (CONS NAME COMMAND) *COMMAND-ALIST*))))

;;; No need to record COMMAND-DEFINE forms in QFASL files,
;;; since they accompany DEFUNs which get recorded.
(DEFPROP COMMAND-DEFINE T SI:QFASL-DONT-RECORD)

(DEFUN PROCESS-COMMAND-OPTIONS (OPTIONS)
  (DO ((L OPTIONS (CDR L))
       (RET NIL (APPEND (CDR (ASSQ (CAR L)
                                  '((NM (SETF (WINDOW-MARK-P *WINDOW*) NIL))
                                    (SM (SETF (WINDOW-MARK-P *WINDOW*) T)
                                        (SETQ *MARK-STAYS* T))
                                    (KM (SETQ *MARK-STAYS* T))
                                    (R (SETQ *CENTERING-FRACTION*
                                             (IF (PLUSP *NUMERIC-ARG*)
                                                 *MIN-RESET-FRACTION*
                                                 *MAX-RESET-FRACTION*)))
                                    (-R (SETQ *CENTERING-FRACTION*
                                              (IF (PLUSP *NUMERIC-ARG*)
                                                  *MAX-RESET-FRACTION*
                                                  *MIN-RESET-FRACTION*)))
                                    (PUSH (POINT-PDL-PUSH (POINT) *WINDOW*))
                                    (OTHERWISE (FERROR NIL "Unknown DEFCOM option ~S"
                                                       (CAR L))))))
                        RET)))
      ((NULL L) RET)))

;;; Convert a string into human-readable form.  Remove leading COM-, or leading
;;; and trailing *'s.  Conver hyphens into spaces, and capitalize each word.
;;; This is used both for command names and variable names.
(DEFUN MAKE-COMMAND-NAME (COMMAND)
  (SETQ COMMAND (STRING COMMAND))
  (LET ((CLEN (STRING-LENGTH COMMAND)))
    (LET ((STR (SUBSTRING COMMAND
                          (COND ((STRING-EQUAL "COM-" COMMAND :END2 4) 4)
                                ((STRING-EQUAL "*" COMMAND :END2 1) 1)
                                (T 0))
                          (COND ((EQL #/* (CHAR COMMAND (1- CLEN))) (1- CLEN))
                                (T CLEN)))))
       (STRING-CAPITALIZE-WORDS STR))))

;;; Macro to define a major mode.
(DEFMACRO DEFMAJOR (COMMAND-NAME MODE-SYMBOL MODE-NAME &BODY REST)
  "Define a major mode.
COMMAND-NAME is a symbol, such as COM-MY-MODE.
MODE-SYMBOL is a symbol for the mode, such as MY-MODE.
MODE-NAME is a string for the mode, such as /"My/".
COMMAND-DOCUMENTATION is the doc string for COM-MY-MODE.
COMMAND-OPTIONS is the DEFCOM options list.
BODY is not simply a list of arbitrary forms;
only SETQ, PUSH, ASET, SET-COMTAB, SET-CHAR-SYNTAX,
 SET-SYNTAX-TABLE-INDIRECTION and COMMAND-HOOK forms may be used.
[Actually, one may enclose arbitrary forms using PROGN. No attempt
to undo them is made -- they are just evaluated when the mode is
turned on.]"
  (DECLARE (ARGLIST COMMAND-NAME MODE-SYMBOL MODE-NAME
                    COMMAND-DOCUMENTATION COMMAND-OPTIONS &BODY BODY))
  (DEFINE-MODE-MACRO T MODE-SYMBOL MODE-NAME 0 COMMAND-NAME
                     (CAR REST) (CADR REST) (CDDR REST)))

;;; Macro to define a minor mode.
(DEFMACRO DEFMINOR (COMMAND-NAME MODE-SYMBOL MODE-NAME MODE-LINE-POSITION &BODY REST)
  "Define a minor mode.  Usage is like DEFMAJOR (which see)."
  (DECLARE (ARGLIST COMMAND-NAME MODE-SYMBOL MODE-NAME MODE-LINE-POSITION
                    COMMAND-DOCUMENTATION COMMAND-OPTIONS &BODY BODY))
  (DEFINE-MODE-MACRO NIL MODE-SYMBOL MODE-NAME MODE-LINE-POSITION
                     COMMAND-NAME (CAR REST) (CADR REST) (CDDR REST)))

;;; Internal function to generate code for a major or minor mode.
(DEFUN DEFINE-MODE-MACRO (MAJOR-P MODE-SYMBOL MODE-NAME MODE-LINE-POSITION
                          COMMAND-NAME COMMAND-DOCUMENTATION COMMAND-OPTIONS BODY)
  `(PROGN
     (PUTPROP ',MODE-SYMBOL ',BODY 'MODE)
     (PUTPROP ',MODE-SYMBOL ',MAJOR-P 'MAJOR-MODE-P)
     (PUTPROP ',MODE-SYMBOL ,MODE-LINE-POSITION 'MODE-LINE-POSITION)
     (AND ',MAJOR-P (PUTPROP ',MODE-SYMBOL
                             ',(INTERN (STRING-APPEND MODE-SYMBOL "-HOOK")
                                       (SYMBOL-PACKAGE 'FOO))
                             'MODE-HOOK-SYMBOL))
     (DEFVAR ,(INTERN (STRING-APPEND MODE-SYMBOL "-HOOK")
                      (SYMBOL-PACKAGE 'FOO)))
     (PROCLAIM '(SPECIAL ,MODE-SYMBOL))
     (SETQ ,MODE-SYMBOL ,(COND ((ZEROP (STRING-LENGTH MODE-NAME)) NIL)
                               (MAJOR-P MODE-NAME)
                               (T (STRING-APPEND #/SPACE MODE-NAME))))
     (DEFCOM ,COMMAND-NAME ,COMMAND-DOCUMENTATION ,COMMAND-OPTIONS
             ,(IF MAJOR-P
                  `(PROGN (TURN-OFF-MODE *MAJOR-MODE*)
                          (DOLIST (MODE *UNSTICKY-MINOR-MODES*)
                            (TURN-OFF-MODE MODE))
                          (TURN-ON-MODE ',MODE-SYMBOL))
                `(IF (IF *NUMERIC-ARG-P* (ZEROP *NUMERIC-ARG*)
                       (ASSQ ',MODE-SYMBOL *MODE-LIST*))
                     (TURN-OFF-MODE ',MODE-SYMBOL)
                   (TURN-ON-MODE ',MODE-SYMBOL)))
             DIS-NONE)))

(DEFMACRO WITH-EDITOR-STREAM ((STREAM-VAR . OPTIONS) &BODY BODY)
 "Executes BODY with STREAM-VAR bound to a stream reading and writing an editor buffer.
OPTIONS are as for OPEN-EDITOR-STREAM."
 (LET ((GENVAR (GENSYM)))
   `(LET* ((,GENVAR (OPEN-EDITOR-STREAM . ,OPTIONS))
           (,STREAM-VAR ,GENVAR))
      (UNWIND-PROTECT
        (PROGN . ,BODY)
        (SEND ,GENVAR :FORCE-REDISPLAY)))))

;;;; Variawubbles
;;; A variable is a symbol, whose print name starts and ends with "*".
;;; The value of the variable is the value of the symbol.
;;; It has the following  properties:
;;; VARIABLE-NAME             The name, a string derived from the print-name of the symbol.
;;; VARIABLE-INIT             The initial value.
;;; VARIABLE-TYPE             One of the type symbols below.
;;; (documentation <var> 'variable)
;;;                           A string documenting the variable.  The first line
;;;                             is the "short form.

(DEFMACRO DEFVARIABLE (VAR INIT TYPE &BODY BODY)
  "Define a ZWEI option variable, initializing it.
VAR is the variable to be defined.  INIT is an expression to initialize it with.
TYPE is a keyword saying what kind of value the variable should have:
 :BOOLEAN           T or NIL.
 :KEYWORD           A symbol in the keyword package.
 :STRING            A string.
 :CHAR              A character as a fixnum.
 :CHAR-LIST         A list of characters as fixnums.
 :FIXNUM            A fixnum.
 :FIXNUM-OR-NIL     A fixnum or NIL.
 :PIXEL             A fixnum which is a number of pixels, horizontally.
 :PIXEL-OR-NIL      A fixnum which is a number of pixels, horizontally, or NIL.
 :SMALL-FRACTION    A small flonum between 0.0s0 and 1.0s0, inclusively.
 :ANYTHING          Any Lisp object.
DOC is a documentation string for the variable.
All ZWEI option variables are reinitialized from their INITs
when ZWEI is initialized."
  (DECLARE (ARGLIST VAR INIT TYPE DOC))
  `(PROGN 'COMPILE
     (DEFINE-VARIABLE ',VAR ,INIT ',TYPE)
     (DEFCONST ,VAR ,INIT ,(CAR BODY))))

(DEFVAR *VARIABLE-ALIST* NIL
  "Alist of ZWEI option variable pretty names versus the symbols themselves.")

(DEFUN DEFINE-VARIABLE (VAR INIT TYPE)
  (CHECK-TYPE TYPE (MEMBER :BOOLEAN :KEYWORD :STRING :FIXNUM-OR-NIL :SMALL-FRACTION
                           :CHAR :CHAR-LIST :FIXNUM :ANYTHING :PIXEL :PIXEL-OR-NIL)
             "a valid ZWEI variable type")
  (LET ((NAME (MAKE-COMMAND-NAME VAR)))
    (PUTPROP VAR NAME 'VARIABLE-NAME)
    (OR (ASS #'EQUALP NAME *VARIABLE-ALIST*)
        (PUSH (CONS NAME VAR) *VARIABLE-ALIST*)))
  (PUTPROP VAR INIT 'VARIABLE-INIT)
  (PUTPROP VAR TYPE 'VARIABLE-TYPE))

(DEFUN SETQ-ZWEI-VARIABLES ()
  "Set all ZWEI option variables (defined with DEFVARIABLE) to their initial values."
  (DO ((L *VARIABLE-ALIST* (CDR L))) ((NULL L))
    (LET ((V (CDAR L)))
      (SET V (GET V 'VARIABLE-INIT)))))

(DEFVARIABLE *FILL-COLUMN* 576. :PIXEL
   "Width in pixels used for filling text.")
; character lossage
(DEFVARIABLE *PARAGRAPH-DELIMITER-LIST* '(#/. #/SP #/TAB) :CHAR-LIST
   "Characters which at beginning of line mean a new paragraph.")
; character lossage
(DEFVARIABLE *PAGE-DELIMITER-LIST* '(#/FF) :CHAR-LIST
   "Characters which separate pages.")
(DEFVARIABLE *STICKY-MINOR-MODES* '(ATOM-WORD-MODE WORD-ABBREV-MODE EMACS-MODE) :ANYTHING
   "Minor modes to carry from current buffer to new ones.")
(DEFVARIABLE *UNSTICKY-MINOR-MODES*
             '(ELECTRIC-SHIFT-LOCK-MODE ELECTRIC-FONT-LOCK-MODE RETURN-INDENTS-MODE)
  :ANYTHING
   "Minor modes that are turned off when the mode is changed explicitly")
(DEFVARIABLE *INITIAL-MINOR-MODES* NIL :ANYTHING
   "Minor modes turned on in any major mode")
(DEFVARIABLE *DIRECTORY-LISTER* 'DEFAULT-DIRECTORY-LISTER :ANYTHING
   "Function used by Display Directory and auto directory display option.")
(DEFVARIABLE *DIRECTORY-SINGLE-FILE-LISTER* 'DEFAULT-LIST-ONE-FILE :ANYTHING
   "Function normally called to display each file")
(DEFVARIABLE *AUTO-PUSH-POINT-OPTION* 12 :FIXNUM-OR-NIL
   "Searches push point if it moves more than this many lines.")
(DEFVARIABLE *AUTO-PUSH-POINT-NOTIFICATION* "Point pushed" :STRING
   "This is typed in the echo area when point is automatically pushed.")
(DEFVARIABLE *AUTO-DIRECTORY-DISPLAY* NIL :KEYWORD
   "Tells on which kind of file commands to display directory (NIL, :READ, :WRITE, T).")
(DEFVARIABLE *TAB-BLINKER-FLAG* T :BOOLEAN
   "If a blinker is placed over a tab, make the blinker the width of a space.")
(DEFVARIABLE *FILL-PREFIX* "" :STRING
   "String to put before each line when filling.")
; character lossage
(DEFVARIABLE *FILL-EXTRA-SPACE-LIST* '(#/. #/! #/?) :CHAR-LIST
   "Characters that should be followed by two spaces when filling is done.")
(DEFVARIABLE *FLASH-MATCHING-PAREN* T :BOOLEAN
   "When point is to the right of a close paren, flash the matching open paren.")
(DEFVARIABLE *FLASH-MATCHING-PAREN-MAX-LINES* 400. :FIXNUM
   "Max number of lines to scan when trying to flash the matching open paren.")
(DEFVARIABLE *COMMENT-START* NIL :STRING
   "String that indicates the start of a comment.")
(DEFVARIABLE *COMMENT-BEGIN* ";" :STRING
   "String for beginning new comments.")
(DEFVARIABLE *COMMENT-END* "" :STRING
   "String for ending comments.")
(DEFVARIABLE *COMMENT-COLUMN* (* 48. 8) :PIXEL
   "Column (in pixels) in which to start new comments.")
(DEFVARIABLE *CASE-REPLACE* T :BOOLEAN
   "Replacing commands try to preserve case.")
(DEFVARIABLE *ALPHABETIC-CASE-AFFECTS-SEARCH* NIL :BOOLEAN
   "Search commands demand exact match of case of letters.")
(DEFVARIABLE *PERMANENT-REAL-LINE-GOAL-XPOS* NIL :PIXEL-OR-NIL
   "If non-NIL, goal for Up and Down Real Line commands.")
(DEFVARIABLE *SPACE-INDENT-FLAG* NIL :BOOLEAN
   "If true, Auto Fill mode will indent new lines.")
(DEFVARIABLE *POINT-PDL-MAX* 10 :FIXNUM
   "The maximum number of elements on the point PDL.
The point PDL is the push-down-list of saved places in the buffer
where the POINT has been.")
;New history stuff
;(DEFVARIABLE *KILL-RING-MAX* 10 :FIXNUM
;   "The maximum number of elements on the kill ring.
;The kill ring is the ring buffer of pieces of text saved by command
;that delete text and brought back by commands that yank text.")
(DEFVARIABLE *SEARCH-RING-MAX* 3 :FIXNUM
   "The maximum number of elements on the search ring.
The search ring is the ring buffer of default search strings.")
(DEFVARIABLE *CENTER-FRACTION* 0.5s0 :SMALL-FRACTION
   "Where to recenter the window.
This is how far down in the window the point should be placed when ZWEI
recenters POINT in the window, as a fraction from 0.0 to 1.0.")
(DEFVARIABLE *NEXT-SCREEN-CONTEXT-LINES* 1 :FIXNUM
   "The number of lines of overlap for Next Screen and Previous Screen commands")
(DEFVARIABLE *MIN-RESET-FRACTION* 0.8s0 :SMALL-FRACTION
   "Where to recenter the window when you go off the bottom.
This is how far down in the window the point should be placed when ZWEI
moves the text in the window because you moved off the bottom.
It should be a fraction from 0.0 to 1.0.")
(DEFVARIABLE *MAX-RESET-FRACTION* 0.2s0 :SMALL-FRACTION
   "Where to recenter the window when you go off the top.
This is how far down in the window the point should be placed when ZWEI
moves the text in the window because you moved off the top.
It should be a fraction from 0.0 to 1.0.")
;character lossage
(DEFVARIABLE *BLANKS* '(#/SP #/TAB #/BS) :CHAR-LIST
   "List of characters that ZWEI thinks of as blanks.
The initial contents of this variable are the characters BLANK, TAB, and BACKSPACE.")
;character losssage
(DEFVARIABLE *WHITESPACE-CHARS* '(#/SP #/TAB #/CR #/BS) :CHAR-LIST
   "List of characters that ZWEI thinks of as blanks.
The initial contents of this variable are the characters BLANK, TAB, CR, and BACKSPACE.")
(DEFVARIABLE *REGION-MARKING-MODE* :UNDERLINE :KEYWORD
   "How to mark the region.
This variable tells ZWEI how to denote the region between POINT and MARK.
It should be a symbol, either :UNDERLINE or :REVERSE-VIDEO.")
(DEFVARIABLE *REGION-RIGHT-MARGIN-MODE* NIL :BOOLEAN
   "Should region marking extend to right margin, if region extends past the line?")
(DEFVARIABLE *DEFAULT-MAJOR-MODE* :LISP :KEYWORD
   "The major mode in which new buffers are placed by default.")
(DEFVARIABLE *DEFAULT-PACKAGE* NIL :ANYTHING
   "The package new buffers are initially in.")
(DEFVARIABLE *INDENT-WITH-TABS* T :BOOLEAN
   "T if indentation commands should generate Tab characters.")
(DEFVARIABLE *LISP-INDENT-OFFSET* NIL :FIXNUM-OR-NIL
   "Same as QLisp Indent Offset in EMACS.  Good luck trying to use it. - DLW & MMcM")
(DEFVARIABLE *COMMENT-ROUND-FUNCTION* 'ROUND-FOR-COMMENT :ANYTHING
   "Function used to round up column when comments cannot be aligned to comment column.")
(DEFVARIABLE *LISP-DEFUN-INDENTATION* '(2 1) :ANYTHING
   "Amount to indent the second line of a defun.")
;; No longer a DEFVARIABLE so it won't show in List Variables.  It's too long to be useful there.
(DEFVAR *LISP-INDENT-OFFSET-ALIST* '((catch 1 1) (*catch 1 1))
   "Describe this someday when all figured out.")
(DEFVARIABLE *LISP-INDENT-LONE-FUNCTION-OFFSET* 1 :FIXNUM
   "Amount to offset indentation of car of list.")
(DEFVARIABLE *FILE-VERSIONS-KEPT* 2 :FIXNUM
   "Number of non-superfluous versions of a file in Dired.")
(DEFVARIABLE *TEMP-FILE-TYPE-LIST* '("MEMO" "XGP" "@XGP" "UNFASL" "OUTPUT" "OLREC" "PRESS")
           :ANYTHING
   "List of strings which are file types to be automatically
marked for deletion in Dired.")
;character lossage
(DEFVARIABLE *TEXT-JUSTIFIER-ESCAPE-LIST* '(#/. #/@ #/- #/\ #/') :CHAR-LIST
   "List of characters that start text justifier commands when at the start of the line.")
(DEFVARIABLE *TEXT-JUSTIFIER-UNDERLINE-BEGIN* "" :STRING
   "String to start an underlining.")
(DEFVARIABLE *TEXT-JUSTIFIER-UNDERLINE-END* "" :STRING
   "String to end an underlining.")
(DEFVARIABLE *PL1-INDING-STYLE* 1 :FIXNUM
   "Pl1 indentation style.")
(DEFVARIABLE *ELECTRIC-SHIFT-LOCK-XORS* T :BOOLEAN
   "Shift key acts as xor of shift in electric shift-lock mode")
(DEFVARIABLE *KILL-INTERVAL-SMARTS* NIL :BOOLEAN
   "Kill and yank commands try to optimize whitespace")
(DEFVARIABLE *INDENT-NEW-LINE-NEW-LINE-FUNCTION* NIL :ANYTHING
             "Function called by Indent New Line to move to a new line.
If NIL, the function assigned to the Return key is used.")
(DEFPROP *INDENT-NEW-LINE-NEW-LINE-FUNCTION* T MODE-SETTABLE-P)
(DEFVARIABLE *INDENT-NEW-LINE-INDENT-FUNCTION* NIL :ANYTHING
             "Function called by Indent New Line to perform indentation.
If NIL, the function assigned to the Tab key is used.")
(DEFPROP *INDENT-NEW-LINE-INDENT-FUNCTION* T MODE-SETTABLE-P)

;character lossage
(DEFVARIABLE *AUTO-FILL-ACTIVATION-CHARACTERS* '(#/SP #/CR #/. #/? #/!) :CHAR-LIST
   "Characters which cause filling in auto fill mode")

(DEFVARIABLE *UNDO-SAVE-SMALL-CHANGES* T :BOOLEAN
  "Should minor editing be recorded for M-X Undo?")

(DEFVARIABLE *ONE-WINDOW-DEFAULT* :CURRENT :KEYWORD
  "Controls which window is selected after the One Window command.
:TOP means the uppermost window.  :BOTTOM the lowermost.
:CURRENT the current window.  :OTHER some other window.")

(DEFVARIABLE *FIND-FILE-EARLY-SELECT* NIL :BOOLEAN
  "T => Visiting a file should display the first screenful as soon as it is read in.")

(DEFVARIABLE *FIND-FILE-NOT-FOUND-IS-AN-ERROR* NIL :BOOLEAN
  "T => Find File when file does not exist does not make an empty buffer.")

(DEFVARIABLE *DEFAULT-BASE* NIL :FIXNUM
  "Default value for *READ-BASE* and *PRINT-BASE* when file attribute line does not specify one.")
(DEFVARIABLE *DEFAULT-READTABLE* NIL :ANYTHING
  "Default value for *READTABLE* when file attribute line does not specify one.")

(ADD-INITIALIZATION 'CLEAR-DEFAULT-ATTRIBUTES '(SETQ *DEFAULT-BASE* NIL
                                                     *DEFAULT-READTABLE* NIL) '(LOGOUT))
(ADD-INITIALIZATION 'CLEAR-DEFAULT-ATTRIBUTES '(SETQ *DEFAULT-BASE* NIL
                                                     *DEFAULT-READTABLE* NIL) '(BEFORE-COLD))

(DEFVARIABLE *DISCARD-UNDO-INFO-ON-SAVING* T :BOOLEAN
  "T => when a buffer is saved, throw away its Undo information.")

(DEFVARIABLE *CHECK-UNBALANCED-PARENTHESES-WHEN-SAVING* NIL :BOOLEAN
  "T => when saving a Lisp mode buffer, check for unbalanced parentheses.")

(DEFVARIABLE *DEFAULT-ZMACS-MAIL-TEMPLATE* NIL :ANYTHING
  "NIL or else name of template to invoke by default when sending mail in ZMACS.")

(DEFVARIABLE *DEFAULT-ZMACS-BUG-TEMPLATE* NIL :ANYTHING
  "NIL or else name of template to invoke by default when sending bug reports in ZMACS.")

(DEFVARIABLE *HISTORY-MENU-LENGTH* 20. :FIXNUM
  "Number of history elements to display by default in C-0 C-Y, etc.")

(DEFVARIABLE *HISTORY-YANK-WRAPAROUND* T :BOOLEAN
  "T means Meta-Y will wrap from end of history to beginning, or vice versa.")

(DEFVARIABLE *HISTORY-ROTATE-IF-NUMERIC-ARG* NIL :BOOLEAN
  "T means 1 as arg to yank command uses the history's current yank pointer.
Newer elements have negative numbers.")

(defvariable *history-buffer-lists-per-window* T :boolean
  "T means each window has its own list of buffers for selection,
NIL means to use one global one.  Buffer lists are used by c-2 c-m-L, etc")

(defvariable *inhibit-explanation-of-completion* nil :BOOLEAN
             "T means don't print out all the text explaining how completion works.")

;; Copied from LAD: RELEASE-3.ZWEI; ZMACS.LISP#559 on 26-Mar-87 18:46:54
(defsubst safe-get-zwei-buffer-instance-variable (interval var)
  (send interval :eval-inside-yourself
        `(when (boundp ',var) ,var)))

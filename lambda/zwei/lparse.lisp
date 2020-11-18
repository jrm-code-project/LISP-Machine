;; -*- Mode:LISP; Package:ZWEI; Base:8; readtable: ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;; The purpose of this file is parsing and moving over units of Lisp code.
;; We avoid having to parse all the text that we move over each time we move
;; by remembering information about each line that we parse and using
;; that information the next time we parse across that line.
;; This information is stored on the LINE-CONTENTS-PLIST of the line
;; as the LISP-PARSE-LINE property, and its form is the same as the value
;; returned by LISP-PARSE-LINE (q.v.).  LINE-CONTENTS-PLIST is cleared
;; out whenever the contents of the line are changed.  Thus, if there is
;; any remembered information available, it has to be valid.

;; Useful functions include:
;; FORWARD-SEXP which is the normal way to move across or up list structure;
;; LISP-PARSE-FROM-DEFUN which can tell you whether a given line
;;   starts inside a string or not;
;; LISP-BP-SYNTACTIC-CONTEXT which tells you whether a given char is
;;   inside a string, slashified, or in a comment;
;; LISP-FIND-COMMENT-START which can tell you whether a line contains
;;   a comment, and, if so, where in the line it starts;
;; LISP-FIND-COMMENT-START-AND-END which does that
;;   and also says where the comment starter ends.

;; If *LISP-PARSE-PREPARSED-FLAG* is T, LISP-PARSE-FROM-DEFUN assumes that the
;; lines all already have memoized parsings which are consistent and accurate.

(DEFUN LISP-PARSE-LINE (LINE START-IN-STRING
                        &OPTIONAL (START-INDEX 0) (END-INDEX (LINE-LENGTH LINE)))
  "Parse LINE's Lisp syntax as it pertains to moving over a multi-line sexp.
START-INDEX and END-INDEX specify a part of LINE to parse.
START-IN-STRING should be the character #\| or #\/",
 meaning parse as if the line started within such a grouping,
 or NIL meaning parse the line as if not in a string.
The value is either a number or a list.  If it is a number, it is the
total change in paren depth across the line (positive means open-parens).
If it is a list, the first element is the total change in depth,
the second is the smallest depth encountered in the line
/(for the obscure case of lines looking like /" ...) (... /"),
the third is copied from START-IN-STRING, and the fourth
is like START-IN-STRING but applies to the end of the line instead of the start.
Trailing NILs might be omitted from the list.
If the value is just a number, you can assume that the minimum depth
was equal to (MIN 0 total-change-in-depth)."
  (DO ((DEPTH 0) (MINDEPTH 0) (IN-STRING START-IN-STRING)
       (INDEX START-INDEX (1+ INDEX))
       (CH) (SYNTAX))
      (( INDEX END-INDEX)
       (COND ((OR IN-STRING START-IN-STRING)
              (LIST DEPTH MINDEPTH START-IN-STRING IN-STRING))
             (( MINDEPTH (MIN 0 DEPTH)) DEPTH)
             (T (LIST DEPTH MINDEPTH))))
      (SETQ CH (LDB %%CH-CHAR (AREF LINE INDEX)))
      (COND ((= (SETQ SYNTAX (LIST-SYNTAX-OPEN-CODED CH)) LIST-SLASH)
             (SETQ INDEX (1+ INDEX)))           ;This one a / => skip over next char.
            (IN-STRING                          ;In a string, the opening char can close it.
             (AND (= CH IN-STRING)              ;Aside from that and slashes, nothing matters.
                  (SETQ IN-STRING NIL)))
            ((= SYNTAX LIST-DOUBLE-QUOTE)       ;String-starting chars.
             (SETQ IN-STRING CH))
            ((= SYNTAX LIST-OPEN)               ;Open
             (SETQ DEPTH (1+ DEPTH)))
            ((= SYNTAX LIST-CLOSE)              ;Close
             (SETQ DEPTH (1- DEPTH))
             (SETQ MINDEPTH (MIN DEPTH MINDEPTH)))
            ((= SYNTAX LIST-COMMENT)            ; ; starts comment
             (SETQ INDEX END-INDEX)))))         ; No need to scan through it!

(DEFUN LISP-PARSE-LINE-MEMOIZED (LINE START-IN-STRING
                                 &OPTIONAL (START-INDEX 0) (END-INDEX (LINE-LENGTH LINE))
                                 &AUX TEM)
  "Like LISP-PARSE-LINE but remember the result and reuse it if called again.
It remembers its parsing of a line as the LISP-PARSE-LINE property
in the LINE-CONTENTS-PLIST of the line."
  ;; Memoizing is only done when the entire line is being parsed.
  ;; Because START-IN-STRING is remembered as part of the value,
  ;; we can tell whether the remembered value is for the same
  ;; setting of START-IN-STRING as we are now using.
  ;; The LINE-CONTENTS-PLIST of a line is cleared when the line is munged.
  ;; Thus, if we have a remembered value, we know it matches the current contents.
  (COND ((AND (ZEROP START-INDEX)
              (= END-INDEX (LINE-LENGTH LINE))
              ;; If we are parsing the whole string,
              ;; and there is a remembered value for this string,
              (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'LISP-PARSE-LINE))
              ;; and it used the same START-IN-STRING as we are using now,
              ;; then just return it.
              (EQUAL START-IN-STRING
                     (AND (CONSP TEM) (CADDR TEM)))))
        ;; Otherwise, reparse the line
        (T (SETQ TEM (LISP-PARSE-LINE LINE START-IN-STRING START-INDEX END-INDEX))
           ;; and, if we are parsing the entire line, remember the result.
           (AND (ZEROP START-INDEX)
                (= END-INDEX (LINE-LENGTH LINE))
                (PUTPROP (LOCF (LINE-CONTENTS-PLIST LINE)) TEM 'LISP-PARSE-LINE))))
  TEM)

(DEFUN LISP-PARSE-FROM-DEFUN (LINE &OPTIONAL DEFUN-BEG &AUX TEM)
  "Return an indication of whether LINE begins inside a string.
This is done by parsing down from the beginning of the containing defun.
If DEFUN-BEG is non-NIL, it is used as the parse starting point.
The value can be passed to LISP-PARSE-LINE as the START-IN-STRING arg."
  (COND (*LISP-PARSE-PREPARSED-FLAG*
          (AND (SETQ LINE (LINE-PREVIOUS LINE))
               (CONSP (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'LISP-PARSE-LINE)))
               (CADDDR TEM)))
        (T
          (OR DEFUN-BEG (SETQ DEFUN-BEG (FORWARD-DEFUN (BEG-OF-LINE LINE) -1 T)))
          (DO ((LINE1 (BP-LINE DEFUN-BEG) (LINE-NEXT LINE1))
               (START-INDEX (BP-INDEX DEFUN-BEG) 0)
               (IN-STRING))
              ((EQ LINE LINE1) IN-STRING)
            (SETQ TEM (LISP-PARSE-LINE-MEMOIZED LINE1 IN-STRING START-INDEX))
            (SETQ IN-STRING
                  (AND (CONSP TEM) (CADDDR TEM)))))))

(DEFUN LISP-BP-SYNTACTIC-CONTEXT (BP &OPTIONAL (START-BP (FORWARD-DEFUN BP -1 T)))
  "Describe the syntactic context of a spot identified by BP.
The first value is non-NIL if that spot is in a string.
The second is non-NIL if that spot is slashified.
The third is non-NIL if that spot is in a comment.
START-BP is where to parse from to compute these
 (default is start of containing defun)."
  (DECLARE (RETURN-LIST IN-STRING SLASHIFIED IN-COMMENT))
  (DO ((I (COND ((EQ (BP-LINE BP) (BP-LINE START-BP)) (BP-INDEX START-BP)) (T 0))
          (1+ I))
       (LINE (BP-LINE BP))
       (END-IDX (BP-INDEX BP))
       ;; Start of line is not slashified, and may be in a string.
       (SLASH NIL)
       (IN-STRING (LISP-PARSE-FROM-DEFUN (BP-LINE BP) START-BP))
       (CH))
      (( I END-IDX)
       (RETURN (VALUES IN-STRING SLASH NIL)))
    ;; Now scan through the line parsing till we reach the spot.
    (SETQ CH (LDB %%CH-CHAR (AREF LINE I)))
    (COND (SLASH
            (SETQ SLASH NIL))
          (T
            (SELECT (LIST-SYNTAX-OPEN-CODED CH)
              (LIST-SLASH
                (SETQ SLASH T))
              ;; Once we reach the start of a comment, we know the answer, so exit.
              (LIST-COMMENT
                (OR IN-STRING (RETURN (VALUES NIL NIL T))))
              (LIST-DOUBLE-QUOTE
                (COND ((NOT IN-STRING)
                       (SETQ IN-STRING CH))
                      ((= CH IN-STRING)
                       (SETQ IN-STRING NIL)))))))))

;; Help parse lists backwards.
;; Given a line, and the status at the end of that line (IN-STRING and DEPTH),
;; look backward for the line following that containing the beginning of the list.
;; Return that line, and the in-string and depth for the end of that line.
(DEFUN LISP-BACKWARD-LIST-AUX (LINE IN-STRING DEPTH &OPTIONAL STOP-LINE &AUX DEFUN-BEG)
  ;; First, make sure everything from the start of this defun is all parsed.
  ;; That way, each line's LISP-PARSE-LINE property is set up for
  ;; the correct setting of START-IN-STRING - which is a setting we
  ;; would have no simple way of computing on our backwards scan,
  ;; due to the losing interaction of comments with strings.
  (LISP-PARSE-FROM-DEFUN (LINE-NEXT LINE)
                         (SETQ DEFUN-BEG (FORWARD-DEFUN (END-OF-LINE LINE) -1 T)))
  (DO ((LINE LINE (LINE-PREVIOUS LINE)) (TEM)
       ;; Paren depth of end of previous line.
       (PREVIOUS-DEPTH)
       ;; Minimum paren depth reached moving back over this line.
       (MIN-DEPTH))
      ((OR (EQ LINE STOP-LINE) (EQ LINE (BP-LINE DEFUN-BEG)) (NULL (LINE-PREVIOUS LINE)))
       (RETURN (VALUES LINE IN-STRING DEPTH)))
    ;; To move back to end of previous line, get parsing info on this line.
    (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'LISP-PARSE-LINE))
    ;; Depth changes count negatively when scanned backwards.
    (COND ((NUMBERP TEM) (SETQ PREVIOUS-DEPTH (- DEPTH TEM) MIN-DEPTH PREVIOUS-DEPTH))
          (T (SETQ PREVIOUS-DEPTH (- DEPTH (CAR TEM))
                   ;; An explicit minimum depth is relative to the depth at start of line.
                   MIN-DEPTH (+ PREVIOUS-DEPTH (CADR TEM)))))
    (AND ( MIN-DEPTH 0) (RETURN (VALUES LINE IN-STRING DEPTH)))
    (SETQ DEPTH PREVIOUS-DEPTH
          IN-STRING (AND (CONSP TEM) (CADDR TEM)))))

(DEFUN LISP-FIND-COMMENT-START (LINE &OPTIONAL BEG-INDEX END-INDEX)
  "Return the index in LINE at which the comment starts, or NIL if no comment.
Knows how to check for strings, even strings which started on lines above this one,
and says there is no comment starter for lines inside strings.
Second value is non-NIL if the line ends inside a string."
  (DECLARE (RETURN-LIST START-INDEX IN-STRING))
  (OR BEG-INDEX (SETQ BEG-INDEX 0))
  (OR END-INDEX (SETQ END-INDEX (LINE-LENGTH LINE)))
  (DO ((INDEX 0 (1+ INDEX)) (CH) (SYNTAX)
       (IN-STRING (LISP-PARSE-FROM-DEFUN LINE)))
      (( INDEX END-INDEX) (VALUES NIL IN-STRING))
    (SETQ CH (LDB %%CH-CHAR (AREF LINE INDEX)))
    (SETQ SYNTAX (LIST-SYNTAX-OPEN-CODED CH))
    (COND ((= SYNTAX LIST-SLASH) (SETQ INDEX (1+ INDEX)))
          (IN-STRING (AND (= CH IN-STRING) (SETQ IN-STRING NIL)))
          ((= SYNTAX LIST-DOUBLE-QUOTE) (SETQ IN-STRING CH))
          ((= SYNTAX LIST-COMMENT) (RETURN INDEX)))))

(DEFUN LISP-FIND-COMMENT-START-AND-END (LINE)
  "Find the start and end indices of the comment starter on LINE.
This is useful as the value of *COMMENT-START*.
The third value is non-NIL if the line ends inside a string."
  (DECLARE (RETURN-LIST START-START-INDEX START-END-INDEX IN-STRING))
  (PROG (INDEX I2 IN-STRING
         (BEG-INDEX (COND ((EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
                           (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*)))
                          (T 0)))
         (END-INDEX (COND ((EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                           (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*)))
                          (T (LINE-LENGTH LINE)))))
        ;; Find start of comment.  Return NIL if none.
        (SETF (VALUES INDEX IN-STRING)
              (LISP-FIND-COMMENT-START LINE BEG-INDEX END-INDEX))
        (OR INDEX (RETURN (VALUES NIL NIL IN-STRING)))
        ;; Now find the end.  Skip over the semicolons and the spaces after them.
        (DO ((I1 INDEX (1+ I1)))
            ((OR (= I1 END-INDEX)
                 ( (LIST-SYNTAX (AREF LINE I1)) LIST-COMMENT))
             (SETQ I2 I1)))
        (RETURN (VALUES INDEX (BP-INDEX (FORWARD-OVER *BLANKS* (CREATE-BP LINE I2)))))))

;; Starting from the start of LINE at depth DEPTH inside a list,
;; find the line at which that list ends,
;; and return it and the in-string and depth at the start of that line.
(DEFUN LISP-FORWARD-LIST-AUX (LINE IN-STRING DEPTH &OPTIONAL STOP-LINE)
  (DO ((LINE LINE (LINE-NEXT LINE)) (TEM)
       ;; Holds paren depth of start of next line.
       (NEXT-DEPTH)
       ;; Minimum paren depth reached during this line.
       (MIN-DEPTH))
      ((OR (EQ LINE STOP-LINE) (NULL (LINE-NEXT LINE)))
       (RETURN (VALUES LINE IN-STRING DEPTH)))
    (SETQ TEM (LISP-PARSE-LINE-MEMOIZED LINE IN-STRING))
    (COND ((NUMBERP TEM) (SETQ NEXT-DEPTH (+ TEM DEPTH) MIN-DEPTH NEXT-DEPTH))
          (T (SETQ NEXT-DEPTH (+ (CAR TEM) DEPTH)
                   MIN-DEPTH (+ (CADR TEM) DEPTH))))
    (AND ( MIN-DEPTH 0) (RETURN (VALUES LINE IN-STRING DEPTH)))
    (SETQ DEPTH NEXT-DEPTH
          IN-STRING (AND (CONSP TEM) (CADDDR TEM)))))

(DEFUN FORWARD-SEXP (BP &OPTIONAL (TIMES 1) FIXUP-P
                                  (LEVEL 0) STOP-BP (MOVE-OVER-SINGLEQUOTES-P T) NO-UP-P
                        &AUX CH STRCH)
  "Return a bp which is TIMES sexps (list objects) forward from BP.  Comments are ignored.
TIMES may be negative meaning go backward.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case.
LEVEL > 0 means move up that many levels of list structure.
STOP-BP is a place to give up and return if it is reached;
 the value is NIL or STOP-BP.
MOVE-OVER-SINGLEQUOTES-P if T means that a backward motion should
 move back over any singlequote-like characters before the open-paren.
 Forward motion also moves over them after a string, for the sake of #|foo|#.
NO-UP-P means it is forbidden to move up and then down again.
 NIL is returned if that starts to happen."
   (WHEN (LISP-BP-SYNTACTIC-CONTEXT BP)
     ;; BP is within a string.
     ;; Allow motion over sexps within the string,
     ;; but don't allow motion past boundary of this string.
     ;; Now make an exception for the case of what looks like a defun-beginning
     ;; which is inside a string (such as a #| |# after the previous defun).
     (UNLESS (AND (PLUSP TIMES)
                  (ZEROP (BP-INDEX BP))
                  (= (LIST-SYNTAX (BP-CHAR BP)) LIST-OPEN))
       (SETQ STOP-BP (FORWARD-UP-STRING BP (MINUSP TIMES) NIL))))
   (COND ((ZEROP TIMES) (COPY-BP BP))
         ((PLUSP TIMES)
          (LET ((STATE 'NORMAL)  ;STATE is NORMAL, STRING or ALPHABETIC.
                (TIME 0)
                (LAST-BP (OR STOP-BP (INTERVAL-LAST-BP *INTERVAL*))))
            (CHARMAP-PER-LINE (BP LAST-BP (IF (OR FIXUP-P
                                                  (AND (EQ STATE 'ALPHABETIC)
                                                       ( LEVEL 0)
                                                       (= (1+ TIME) TIMES)))
                                              (COPY-BP LAST-BP)
                                              NIL))
                     ;; Per-line forms
                     ;; If at start of line and inside some parens,
                     ;; skip over some lines using memoized LISP-PARSE-LINE info.
                     ;; This is an invisible speed-up for the rest of this loop.
                     ((COND ((AND (ZEROP *FIRST-INDEX*) (> LEVEL 0))
                             (MULTIPLE-VALUE (LINE STRCH LEVEL)
                               (LISP-FORWARD-LIST-AUX LINE
                                                      (AND (EQ STATE 'STRING) STRCH)
                                                      LEVEL *LAST-LINE*))
                             (SETQ STATE (COND (STRCH 'STRING) (T 'NORMAL)))
                             (SETQ *THIS-IS-THE-LAST-LINE*
                                   (EQ LINE *LAST-LINE*)))))
              RESTART
              (LET ((SYNTAX (LIST-SYNTAX-OPEN-CODED (SETQ CH (CHARMAP-CH-CHAR)))))
                (SELECTQ STATE
                  (ALPHABETIC
                   (SELECT SYNTAX
                     (LIST-ALPHABETIC)
                     (LIST-SINGLE-QUOTE)
                     (LIST-SLASH
                      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))
                     (LIST-COLON
                      (SETQ STATE 'NORMAL))
                     (OTHERWISE
                      (IF ( LEVEL 0)
                          (IF ( (SETQ TIME (1+ TIME)) TIMES)
                              (CHARMAP-RETURN (CHARMAP-BP-BEFORE))))
                      (SETQ STATE 'NORMAL)
                      (GO RESTART))))
                  (STRING
                   (SELECT SYNTAX
                    (LIST-DOUBLE-QUOTE
                      (COND ((= CH STRCH)
                             (IF ( LEVEL 0)
                                 (IF ( (SETQ TIME (1+ TIME)) TIMES)
                                     (CHARMAP-RETURN
                                       ;; Maybe move forward over singlequote-syntax chars
                                       ;; for the sake of #|foo|#.
                                       (IF MOVE-OVER-SINGLEQUOTES-P
                                           (DO ((BP (CHARMAP-BP-AFTER) (IBP BP)))
                                               ((OR (NULL BP)
                                                    (BP-= BP LAST-BP)
                                                    (NOT (= (LIST-SYNTAX (BP-CHAR BP))
                                                            LIST-SINGLE-QUOTE)))
                                                (OR BP (IF FIXUP-P (COPY-BP LAST-BP)))))
                                         (CHARMAP-BP-AFTER)))))
                             (SETQ STATE 'NORMAL))))
                     (LIST-SLASH
                      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL)))))
                  (NORMAL
                   (SELECT SYNTAX
                     (LIST-ALPHABETIC
                      (SETQ STATE 'ALPHABETIC))
                     (LIST-DELIMITER)
                     (LIST-SLASH
                      (CHARMAP-INCREMENT (IF FIXUP-P (COPY-BP LAST-BP) NIL))
                      (SETQ STATE 'ALPHABETIC))
                     (LIST-COMMENT
                      (SETQ INDEX *LAST-INDEX*))
                     (LIST-DOUBLE-QUOTE
                      (SETQ STATE 'STRING STRCH CH))
                     (LIST-SINGLE-QUOTE)
                     (LIST-CLOSE
                      (SETQ LEVEL (1- LEVEL))
                      (COND ((AND NO-UP-P (< LEVEL 0))
                             (CHARMAP-RETURN NIL))
                            (( LEVEL 0)
                             (IF ( (SETQ TIME (1+ TIME)) TIMES)
                                 (CHARMAP-RETURN (CHARMAP-BP-AFTER)))))
                      (SETQ STATE 'NORMAL))
                     (LIST-OPEN
                      (SETQ LEVEL (1+ LEVEL)))
                     (LIST-COLON))))))))
         (T
          (LET ((STATE 'NORMAL)
                (TIME 0)
                NEW-LINE-FLAG
                (FIRST-BP (OR STOP-BP (INTERVAL-FIRST-BP *INTERVAL*))))
            (RCHARMAP-PER-LINE (BP FIRST-BP
                                   (IF (OR FIXUP-P
                                           (AND (OR (AND (EQ STATE 'ALPHABETIC) ( LEVEL 0))
                                                    (AND (EQ STATE
                                                             'SKIP-LEADING-SINGLE-QUOTES)
                                                         ( LEVEL 1)))
                                                (= (1- TIME) TIMES)))
                                       (COPY-BP FIRST-BP)
                                       NIL))
                     ;; Per-line forms.  Work like those for forward case.
                     ((COND ((AND (NOT *FIRST-LINE-P*) (> LEVEL 0))
                             (MULTIPLE-VALUE (LINE STRCH LEVEL)
                               (LISP-BACKWARD-LIST-AUX LINE
                                                       (AND (EQ STATE 'STRING) STRCH)
                                                       LEVEL *LAST-LINE*))
                             (SETQ STATE (COND (STRCH 'STRING) (T 'NORMAL)))
                             (SETQ *THIS-IS-THE-LAST-LINE*
                                   (EQ LINE *LAST-LINE*))))
                      (SETQ NEW-LINE-FLAG T))
              ;; After arriving on a line and processing the implicit Return at the end,
              ;; skip back to start of any comment on that line.
              (AND ( #\RETURN (RCHARMAP-CH-CHAR))
                   (PROG1 NEW-LINE-FLAG (SETQ NEW-LINE-FLAG NIL))
                   (NOT *FIRST-LINE-P*)
                   (SETQ INDEX (OR (LISP-FIND-COMMENT-START LINE *LAST-INDEX*) INDEX)))
              RESTART
              (COND ((AND (= (LIST-SYNTAX-OPEN-CODED (RCHARMAP-CH-CHAR-BEFORE)) LIST-SLASH)
                          (DO ((SL NIL (NOT SL))
                               (BP (FORWARD-CHAR (RCHARMAP-BP-BEFORE) -1)
                                   (FORWARD-CHAR BP -1)))
                              ((OR (NULL BP)
                                   ( (LIST-SYNTAX-OPEN-CODED (BP-CH-CHAR BP)) LIST-SLASH))
                               SL)))
                     ;; Odd number of preceding slashes means non-special character
                     (RCHARMAP-DECREMENT NIL)
                     (AND (EQ STATE 'NORMAL)
                          (SETQ STATE 'ALPHABETIC)))
                    (T
                     (LET ((SYNTAX (LIST-SYNTAX-OPEN-CODED (SETQ CH (RCHARMAP-CH-CHAR)))))
                       (SELECTQ STATE
                         (ALPHABETIC
                          (SELECT SYNTAX
                            ((LIST-ALPHABETIC LIST-COLON))
                            (LIST-SINGLE-QUOTE)
                            (OTHERWISE
                             (IF ( LEVEL 0)
                                 (IF ( (SETQ TIME (1- TIME)) TIMES)
                                     (RCHARMAP-RETURN
                                      (RCHARMAP-BP-AFTER))))
                             (SETQ STATE 'NORMAL)
                             (GO RESTART))))
                         (STRING
                          (SELECT SYNTAX
                            (LIST-DOUBLE-QUOTE
                             (COND ((= CH STRCH)
                                    (IF ( LEVEL 0)
                                        (IF ( (SETQ TIME (1- TIME)) TIMES)
                                            (RCHARMAP-RETURN
                                              (MAYBE-BACKWARD-LEADING-SINGLE-QUOTES
                                                (RCHARMAP-BP-BEFORE)
                                                FIRST-BP MOVE-OVER-SINGLEQUOTES-P))))
                                    (SETQ STATE 'NORMAL))))))
                         (NORMAL
                          (SELECT SYNTAX
                            (LIST-ALPHABETIC
                             (SETQ STATE 'ALPHABETIC))
                            (LIST-COMMENT
                             (WHEN *FIRST-LINE-P*
                               (SETQ LEVEL 0)))
                            ((LIST-SLASH LIST-COLON)
                             ;; Crock.
                             (SETQ STATE 'ALPHABETIC))
                            (LIST-DOUBLE-QUOTE
                             (SETQ STATE 'STRING STRCH CH))
                            (LIST-CLOSE
                             (SETQ LEVEL (1+ LEVEL)))
                            (LIST-OPEN
                             (SETQ LEVEL (1- LEVEL))
                             (COND ((AND NO-UP-P (< LEVEL 0) (NOT FIXUP-P))
                                    (RCHARMAP-RETURN NIL))
                                   ((AND (= (LIST-SYNTAX (RCHARMAP-CH-CHAR-BEFORE)) LIST-COLON)
                                         MOVE-OVER-SINGLEQUOTES-P)
                                    (SETQ STATE 'ALPHABETIC))
                                   (( LEVEL 0)
                                    (IF ( (SETQ TIME (1- TIME)) TIMES)
                                        (RCHARMAP-RETURN
                                          (MAYBE-BACKWARD-LEADING-SINGLE-QUOTES
                                            (RCHARMAP-BP-BEFORE)
                                            FIRST-BP MOVE-OVER-SINGLEQUOTES-P))))))
                            (LIST-COLON))))))))))))

(DEFUN MAYBE-BACKWARD-LEADING-SINGLE-QUOTES (BP FIRST-BP MOVE-FLAG)
  (IF (AND BP MOVE-FLAG) (BACKWARD-LEADING-SINGLE-QUOTES BP FIRST-BP)
    BP))

;; After moving back past a sexp, move back past any singlequote-syntax chars
;; preceding the sexp.  We accept a BP to the beginning of the sexp,
;; and return a bp to the first singlequote of those that precede it.
(DEFUN BACKWARD-LEADING-SINGLE-QUOTES (BP FIRST-BP)
  "Move back from BP over singlequote characters, and return a bp.
But do not move past FIRST-BP.
Checks for slashified singlequotes and doesn't move over them."
  (CREATE-BP (BP-LINE BP)
             (DO ((INDEX (BP-INDEX BP) (1- INDEX))
                  (STOP-INDEX (COND ((EQ (BP-LINE BP) (BP-LINE FIRST-BP)) (BP-INDEX FIRST-BP))
                                    (T 0)))
                  (LINE (BP-LINE BP))
                  (SYNTAX) (SLASH-PARITY-ODD))
                 ((= INDEX STOP-INDEX) STOP-INDEX)
               (SETQ SYNTAX (LIST-SYNTAX (AREF LINE (1- INDEX))))
               (COND ((= SYNTAX LIST-SINGLE-QUOTE))
                     ;; We have found all the singlequotes.
                     ;; INDEX is the index of the first singlequote.
                     ((= SYNTAX LIST-SLASH)
                      ;; Don't worry about slashes if there weren't any singlequotes.
                      (AND (= INDEX (BP-INDEX BP))
                           (RETURN INDEX))
                      ;; Count the parity of slashes here.
                      (DO ((INDEX1 INDEX (1- INDEX1)))
                          ((= INDEX1 STOP-INDEX))
                        (COND ((= (LIST-SYNTAX (AREF LINE (1- INDEX1))) LIST-SLASH)
                               (SETQ SLASH-PARITY-ODD (NOT SLASH-PARITY-ODD)))
                              (T (RETURN NIL))))
                      ;; If odd # of slashes, the first singlequote doesn't count as one.
                      (RETURN (COND (SLASH-PARITY-ODD (+ 1 INDEX))
                                    ;; If even # of slashes, the first singlequote counts.
                                    (T INDEX))))
                     (T (RETURN INDEX))))))

(DEFUN FORWARD-UP-LIST-OR-STRING (BP &OPTIONAL (TIMES 1) FIXUP-P (SINGLE-QUOTES-P T))
  "Return a bp up TIMES levels of list or string from BP.
If TIMES is negative, still moves up but backwards.
FIXUP-P means if trying to move past beginning or end of buffer
 return the beginning or end of buffer; otherwise return NIL in this case.
SINGLE-QUOTES-P non-NIL (as it is by default) means move over
any singlequote characters past the stopping parenthesis."
  (IF (LISP-BP-SYNTACTIC-CONTEXT BP)
      (FORWARD-UP-STRING BP (MINUSP TIMES))
    (FORWARD-SEXP BP TIMES FIXUP-P 1 NIL SINGLE-QUOTES-P)))

(DEFUN FORWARD-UP-STRING (BP REVERSE-FLAG &OPTIONAL (MOVE-OVER-SINGLEQUOTES-FLAG T))
  "Return a bp to the end or beginning of the string BP is in.
REVERSE-FLAG non-NIL means move backward; otherwise forward.
If BP is not in a string, it counts as the beginning or end itself.
MOVE-OVER-SINGLEQUOTES-FLAG non-NIL means after getting out of the string
 move over any singlequote-like characters that are encountered."
  (SETQ BP (COPY-BP BP))
  (IF REVERSE-FLAG
      (DO ((BP BP (DBP BP)))
          ((OR (NULL BP)
               (AND (EQ (LIST-SYNTAX (BP-CHAR BP)) LIST-DOUBLE-QUOTE)
                    (NOT (LISP-BP-SYNTACTIC-CONTEXT BP))))
           (AND BP
                (MAYBE-BACKWARD-LEADING-SINGLE-QUOTES BP (BEG-OF-LINE (BP-LINE BP))
                                                      MOVE-OVER-SINGLEQUOTES-FLAG))))
    (DO ((BP BP (IBP BP)))
        ((OR (NULL BP)
             (AND (EQ (LIST-SYNTAX (BP-CHAR-BEFORE BP)) LIST-DOUBLE-QUOTE)
                  (NOT (LISP-BP-SYNTACTIC-CONTEXT BP))))
         (IF (AND BP MOVE-OVER-SINGLEQUOTES-FLAG)
             (DO ((BP BP (IBP BP)))
                 ((OR (NULL BP)
                      (NOT (= (LIST-SYNTAX (BP-CHAR BP))
                              LIST-SINGLE-QUOTE)))
                  BP))
           BP)))))

;;; For things with a standard interface, like KILL-COMMAND-INTERNAL
(DEFUN FORWARD-SEXP-NO-UP (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is TIMES sexps (list objects) forward from BP.  Comments are ignored.
TIMES may be negative meaning go backward.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case.
Backward motion should move back over any singlequote-like characters before the open-paren.
 Forward motion also moves over them after a string, for the sake of #|foo|#.
If an unmatched closeparen is found going forward
 or an unmatched open going backward, NIL is returned."
  (FORWARD-SEXP BP TIMES FIXUP-P 0 NIL T T))

;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; Even even more ZWEI commands

(DEFCOM COM-UPPERCASE-LISP-CODE-IN-REGION
  "Uppercase the region, but not strings, comments, etc.
Characters preceded by escape characters are also immune." ()
  (LISP-CHANGE-CASE T))

(DEFCOM COM-LOWERCASE-LISP-CODE-IN-REGION
  "Lowercase the region, but not strings, comments, etc.
Characters preceded by escape characters are also immune." ()
  (LISP-CHANGE-CASE NIL))

(DEFUN LISP-CHANGE-CASE (UP-P &AUX START-BP BP CH SYNTAX)
  (REGION (BP1 BP2)
    (MULTIPLE-VALUE-BIND (STRING-P SLASHIFIED-P COMMENT-P) (LISP-BP-SYNTACTIC-CONTEXT BP1)
      (FLET ((DO-IT () (WITH-UNDO-SAVE ((IF UP-P "Upcase Lisp Code" "Downcase Lisp Code")
                                        START-BP BP T)
                         (IF UP-P (UPCASE-INTERVAL START-BP BP T)
                           (DOWNCASE-INTERVAL START-BP BP T)))))
          (SETQ START-BP (COPY-BP BP1) BP (COPY-BP BP1))
          (DO ()
              ((BP-= BP BP2)
               (UNLESS (OR COMMENT-P STRING-P SLASHIFIED-P)
                 (DO-IT)))
            (IBP BP)
            (SETQ CH (BP-CH-CHARACTER BP))
            (SETQ SYNTAX (LIST-SYNTAX CH))
            (COND (COMMENT-P
                   (WHEN (EQ CH #/RETURN)
                     (SETQ COMMENT-P NIL)
                     (MOVE-BP START-BP BP)
                     (IBP START-BP)))
                  (SLASHIFIED-P
                   (SETQ SLASHIFIED-P NIL)
                   (MOVE-BP START-BP BP)
                   (IBP START-BP))
                  ((= SYNTAX LIST-SLASH)
                   (SETQ SLASHIFIED-P BP)
                   (UNLESS STRING-P
                     (DO-IT)))
                  (STRING-P
                   (WHEN (EQ CH STRING-P)
                     (SETQ STRING-P NIL)
                     (MOVE-BP START-BP BP)
                     (IBP START-BP)))
                  ((OR (= SYNTAX LIST-COMMENT)
                       (= SYNTAX LIST-DOUBLE-QUOTE))
                   (IF (= SYNTAX LIST-COMMENT)
                       (SETQ COMMENT-P T)
                     (SETQ STRING-P CH))
                   (DO-IT))
                  ;; special kludge for Zetalisp #\ #/ equivalence. Sigh
                  ((AND (NOT (OR COMMENT-P STRING-P SLASHIFIED-P))
                        (EQ CH #/\)
                        (CHAR-EQUAL (BP-CHARACTER-BEFORE BP) #/#))
                   (SETQ SLASHIFIED-P T)
                   (DO-IT)))))))
  DIS-TEXT)

(DEFCOM COM-COMMENT-OUT-REGION "Stick comment start characters at the start of each line in the region.
Adds regardless of any that may already be there.
A numeric argument specifies how many to add. A negative argument species how many to delete.
An argument of c-U is treated like -1: it deletes single comment starts." ()
  (IF (EQ *NUMERIC-ARG-P* ':CONTROL-U)
      (SETQ *NUMERIC-ARG* (CLI:// *NUMERIC-ARG* -4)))
  (REGION-LINES (START-LINE END-LINE)
    (IF (> *NUMERIC-ARG* 0)
        (LET* ((LEN (LENGTH *COMMENT-BEGIN*))
               (INSERT (MAKE-STRING (* *NUMERIC-ARG* LEN))))
          (DOTIMES (I *NUMERIC-ARG*)
            (COPY-ARRAY-PORTION *COMMENT-BEGIN* 0 LEN
                                INSERT (* I LEN) (+ (* I LEN) LEN)))
          (DO ((LINE START-LINE (LINE-NEXT LINE))
               (BP (CREATE-BP START-LINE 0)))
              ((EQ LINE END-LINE))
            (MOVE-BP BP LINE 0)
            (UNLESS (EQ (LINE-TYPE LINE) ':BLANK)
              (INSERT BP INSERT)))))
    (SETQ *NUMERIC-ARG* (- *NUMERIC-ARG*))
    (DO ((LINE START-LINE (LINE-NEXT LINE))
         (BP (CREATE-BP START-LINE 0))
         (BPA (CREATE-BP START-LINE 1)))
        ((EQ LINE END-LINE))
      (DOTIMES (I *NUMERIC-ARG*)
        (IF (OR (EQ (LINE-TYPE LINE) ':BLANK)
                (NOT (STRING-EQUAL LINE *COMMENT-BEGIN* :END1 (LENGTH *COMMENT-BEGIN*))))
            (RETURN)
            (MOVE-BP BP LINE 0)
            (MOVE-BP BPA LINE (LENGTH *COMMENT-BEGIN*))
            (DELETE-INTERVAL BP BPA T)))))
  DIS-TEXT)

;;; Pattern finding command
(DEFVAR *LAST-LISP-MATCH-SEARCH-STRING* NIL
  "Previous string searched for in Lisp Match Search command.")

(DEFCOM COM-LISP-MATCH-SEARCH "Move to next occurrence of the given pattern of Lisp code.
In matching, differences in whitespace characters are ignored
 except for characters that are quoted or inside strings.
The characters # as an atom in the pattern match any sexp in the buffer.
The characters ... as an atom in the pattern match any number of sexps.
Patterns starting with infrequent characters such as open parentheses
are found much faster.  Those starting with common letters are likely to be slow.
Patterns starting with very infrequent characters are fastest.

A negative argument means search backwards.
An empty pattern string means repeat the last pattern specified." ()
  (LET ((FORM (TYPEIN-LINE-READLINE "Pattern to search for:")))
    (COND ((EQUAL FORM "")
           (SETQ FORM (OR *LAST-LISP-MATCH-SEARCH-STRING* (BARF "No previous pattern")))
           (FORMAT *QUERY-IO* "~&Finding ~S" FORM))
          (T
           (SETQ *LAST-LISP-MATCH-SEARCH-STRING* FORM)))
    (LET ((BP (LISP-MATCH-SEARCH (POINT) FORM (MINUSP *NUMERIC-ARG*))))
      (UNLESS BP (BARF))
      (MAYBE-PUSH-POINT BP)
      (MOVE-BP (POINT) BP)))
  DIS-BPS)

(DEFUN LISP-MATCH-SEARCH (BP STRING &OPTIONAL REVERSEP FIXUP-P IGNORE LIMIT-BP
                          &AUX (START 0) (END (LENGTH STRING))
                               (final-limit-bp (if reversep
                                                   (interval-first-bp *interval*)
                                                 (interval-last-bp *interval*))))
  "Search from BP for Lisp code that matches against STRING.
Matching at any given place is done with LISP-STRING-BUFFER-MATCH.
Differences in whitespace characters are ignored except when quoted or inside strings.
The characters # as an atom in the STRING match any sexp in the buffer.
The characters ... as an atom in the STRING match any number of sexps.

If a match is found, the value is a bp to the end (start, if reverse) of the matching text.
A second value is a bp to the start (end, if reverse) of the matching text.

REVERSEP means search backward from BP; the code matched must end before BP.
 Otherwise, search goes forward from BP.
LIMIT-BP is a place to stop searching; the matched code cannot
 continue past there in forward search or begin before there in backward search.
FIXUP-P says what to do if no match is found.
 T means return the end of the range to be searched
 (either LIMIT-BP or the beginning or end of the interval).
 NIL means return NIL.  Second value is NIL in either case."
  ;; Ignore leading delimiter chars in STRING.
  (DO ()
      ((OR (= START END)
           (NOT (= LIST-DELIMITER (LIST-SYNTAX (CHAR STRING START))))))
    (INCF START))
  ;; Strings that start with ... or # are handled specially.
  (COND ((= START END)
         (AND FIXUP-P (OR LIMIT-BP FINAL-LIMIT-BP)))
        ((AND (STRING-EQUAL STRING "..." :START1 START :END1 (+ START 3))
              (OR (= (+ START 3) END)
                  (NOT (MEMQ (LIST-SYNTAX (CHAR STRING (+ START 3)))
                             '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
         (BARF "A search pattern starting with ... is not meaningful."))
        ((AND (STRING-EQUAL STRING "#" :START1 START :END1 (+ START 1))
              (OR (= (+ START 2) END)
                  (NOT (MEMQ (LIST-SYNTAX (CHAR STRING (+ START 2)))
                             '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
         (LET ((TAIL-FOUND (LISP-MATCH-SEARCH (FORWARD-SEXP BP 1 T)
                                              (SUBSTRING STRING (+ START 3)))))
           (OR (AND TAIL-FOUND
                    (FORWARD-SEXP TAIL-FOUND -1 NIL 0 BP T T))
               (AND FIXUP-P LIMIT-BP FINAL-LIMIT-BP))))
        (REVERSEP
         (LET ((BP1 (COPY-BP BP))
               TEM)
           (DO-FOREVER
             (SETQ BP1 (ZWEI-SEARCH BP1 (CHAR STRING START) T NIL NIL
                                    (OR LIMIT-BP FINAL-LIMIT-BP)))
             (UNLESS BP1 (RETURN (AND FIXUP-P (OR LIMIT-BP FINAL-LIMIT-BP))))
             (IF (SETQ TEM (LISP-STRING-BUFFER-MATCH BP1 BP STRING START))
                 (RETURN (VALUES BP1 TEM))))))
        (T
         (LET ((BP1 (COPY-BP BP))
               TEM)
           (DO-FOREVER
             (SETQ BP1 (ZWEI-SEARCH BP1 (CHAR STRING START) NIL NIL NIL LIMIT-BP))
             (UNLESS BP1 (RETURN (AND FIXUP-P (OR LIMIT-BP FINAL-LIMIT-BP))))
             (DBP BP1)
             (IF (SETQ TEM (LISP-STRING-BUFFER-MATCH BP1 (OR LIMIT-BP FINAL-LIMIT-BP)
                                                     STRING START))
                 (RETURN (VALUES TEM BP1)))
             (IF (OR (BP-= BP1 LIMIT-BP) (BP-= BP1 FINAL-LIMIT-BP))
                 (RETURN (IF FIXUP-P BP1)))
             (UNLESS (IBP BP1) (RETURN (IF FIXUP-P BP1 NIL))))))))

(DEFUN LISP-STRING-BUFFER-MATCH (START-BP LIMIT-BP PATTERN-STRING &OPTIONAL (START 0) END)
  "Match part of a string against part of an editor buffer, comparing as Lisp code.
The string is PATTERN-STRING; START and END specify the range to use.
The buffer text starts at START-BP.  It will not match past LIMIT-BP.
If there is a match, the value is a bp to the end of the buffer text matched.
Otherwise, the value is NIL.
Differences in whitespace characters are ignored except when quoted or inside strings.
The characters # as an atom in the PATTERN-STRING match any sexp in the buffer.
The characters ... as an atom in the PATTERN-STRING match any number of sexps."
  (UNLESS END (SETQ END (LENGTH PATTERN-STRING)))
  (DO-NAMED OUTER
            ((I START (1+ I))
             (BP (COPY-BP START-BP))
             IN-STRING QUOTED IN-COMMENT IN-ATOM
             (P-SYN -1))
            ((= I END) BP)
    (IF (BP-= LIMIT-BP BP)
        (RETURN NIL))
    (LET* ((S-CHAR (CHAR PATTERN-STRING I))
           (S-SYN (LIST-SYNTAX S-CHAR)))
      ;; S-SYN is this pattern character's syntax.
      ;; P-SYN is the previous significant pattern character's syntax.
      ;; It is LIST-ALPHABETIC iff the last pattern character, not counting delimiters,
      ;; was such as to be part of an atom.  This is the case in which
      ;; at least one delimiter is required in the buffer in order to match.
      (COND (IN-STRING
             ;; First update the syntactic state.
             (COND (QUOTED (SETQ QUOTED NIL))
                   ((= S-SYN LIST-DOUBLE-QUOTE)
                    (SETQ IN-STRING NIL))
                   ((= S-SYN LIST-SLASH)
                    (SETQ QUOTED T)))
             ;; Now always match against buffer.
             (UNLESS (EQ S-CHAR (BP-CH-CHARACTER BP))
               (RETURN NIL))
             (SETQ P-SYN -1)
             (IBP BP))
            (IN-COMMENT
             (IF (EQ S-CHAR #/RETURN) (SETQ IN-COMMENT NIL))
             ;; Now always match against buffer.
             (UNLESS (CHAR-EQUAL S-CHAR (BP-CHARACTER BP))
               (RETURN NIL))
             (SETQ P-SYN -1)
             (IBP BP))
            (QUOTED
             (SETQ QUOTED NIL)
             ;; Quoted char, always match against buffer.
             (UNLESS (EQ S-CHAR (BP-CH-CHARACTER BP))
               (RETURN NIL))
             (SETQ P-SYN LIST-ALPHABETIC)
             (IBP BP))
            ;; Not in string or comment, not slashified.
            ((= S-SYN LIST-DELIMITER)
             ;; Just skip all delimiters in the pattern.
             (SETQ IN-ATOM NIL))
            ((AND (NOT IN-ATOM)
                  (<= (+ I 3) END)
                  (STRING-EQUAL PATTERN-STRING "..." :START1 I :END1 (+ I 3))
                  (OR (= (+ I 3) END)
                      (NOT (MEMQ (LIST-SYNTAX (CHAR PATTERN-STRING (+ I 3)))
                                 '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
             ;; "..." has been encountered, and its an atom by itself.
             (DO-FOREVER
               ;; Try matching the rest of the pattern at one spot.
               (LET ((TEM (LISP-STRING-BUFFER-MATCH BP LIMIT-BP PATTERN-STRING (+ I 3) END)))
                 (WHEN TEM
                   (RETURN-FROM OUTER TEM)))
               ;; SKip one more sexp and try again.
               (SETQ BP (FORWARD-SEXP BP 1 NIL 0 LIMIT-BP NIL T))
               (UNLESS BP (RETURN NIL))))
            ((AND (NOT IN-ATOM)
                  (<= (1+ I) END)
                  (STRING-EQUAL PATTERN-STRING "#" :START1 I :END1 (1+ I))
                  (OR (= (1+ I) END)
                      (NOT (MEMQ (LIST-SYNTAX (CHAR PATTERN-STRING (1+ I)))
                                 '(#,LIST-ALPHABETIC #,LIST-SINGLE-QUOTE)))))
             ;; "#" has been encountered as an atom in the pattern.
             ;; SKip it, and skip one sexp in the buffer, then keep matching.
             (SETQ BP (FORWARD-SEXP BP 1 NIL 0 LIMIT-BP NIL T))
             (SETQ P-SYN -1)
             (UNLESS BP (RETURN NIL)))
            (T
             ;; Skip all delimiters here in the buffer, if not within an atom.
             (UNLESS IN-ATOM
               (DO ((COUNT 0 (1+ COUNT)))  ;Count number of delimiters skipped.
                   ((NOT (= LIST-DELIMITER (LIST-SYNTAX (BP-CHARACTER BP))))
                    (AND (ZEROP COUNT)
                         (= S-SYN LIST-ALPHABETIC)
                         (= P-SYN LIST-ALPHABETIC)
                         (RETURN-FROM OUTER NIL)))
                 (IBP BP)))
             ;; Set up syntax context of next pattern character.
             (SELECT S-SYN
               (LIST-DOUBLE-QUOTE
                (SETQ IN-STRING T))
               (LIST-SLASH
                (SETQ QUOTED T))
               (LIST-COMMENT
                (SETQ IN-COMMENT T))
               (LIST-ALPHABETIC
                (SETQ IN-ATOM T)))
             (IF (EQ S-CHAR #/.) (SETQ IN-ATOM T))
             ;; Now always match against buffer.
             (UNLESS (CHAR-EQUAL S-CHAR (BP-CHARACTER BP))
               (RETURN NIL))
             (IBP BP)
             (SETQ P-SYN S-SYN))))))

;;; Some random file viewing commands
(DEFCOM COM-VIEW-DIRECTORY "List contents of a file directory.
While viewing, you can use Space and Overstrike to scroll forward and backward.
Type Rubout to exit." ()
  (LET ((PATHNAME (READ-DIRECTORY-NAME "View directory" (DEFAULT-PATHNAME))))
    (VIEW-DIRECTORY PATHNAME)))

(DEFCOM COM-VIEW-LOGIN-DIRECTORY "List files in user's directory." ()
  (VIEW-DIRECTORY (SEND (FS:USER-HOMEDIR) :NEW-PATHNAME :NAME :WILD
                                                        :TYPE :WILD
                                                        :VERSION :WILD)))

(DEFUN NEW-MAIL-EXISTS-P ()   ;Perhaps args for machines or user to check for.
  "Return t if there is new mail"
  (PROBEF (SEND (FS:USER-HOMEDIR) :NEW-MAIL-PATHNAME)))

(DEFCOM COM-VIEW-MAIL "View any new mail (your inbox file).
While viewing, you can use Space and Overstrike to scroll forward and backward.
Type Rubout to exit." ()
  (LET ((PATHNAME (SEND (FS:USER-HOMEDIR) :NEW-MAIL-PATHNAME)))
    (COND ((PROBEF PATHNAME)
           (VIEW-FILE PATHNAME))
          (T
           (FORMAT *QUERY-IO* "~&No new mail."))))
  DIS-NONE)

(DEFCOM COM-VIEW-OLD-MAIL "View old mail (your BABYL file, etc.).
While viewing, you can use Space and Overstrike to scroll forward and backward.
Type Rubout to exit." ()
  (LOOP FOR MAIL-FILE IN (SEND (FS:USER-HOMEDIR) :POSSIBLE-MAIL-FILE-NAMES)
        WHEN (NOT (ERRORP (OPEN MAIL-FILE :DIRECTION :PROBE)))
        DO (VIEW-FILE MAIL-FILE) (SETQ MAIL-FILE ':FOUND) (LOOP-FINISH)
        FINALLY (IF (NOT (STRING-EQUAL MAIL-FILE ':FOUND))
                    (FORMAT *QUERY-IO* "~&No old mail.")))
  DIS-NONE)

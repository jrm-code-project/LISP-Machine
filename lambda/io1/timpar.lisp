;;;     -*- Mode: Lisp; Package: Time; Base: 10.; readtable: ZL -*-
;;; Convert Date To Binary for the Lisp Machine    DLW 9/14/80
;;; Take a description of a time (including a date) in any of many commonly
;;; understood formats, parse it and return it in a machine-usable form.

;;  We want to be able to manipulate times in a manner where
;; we can compute how long one hour and 4 minutes is, and
;; print it out in a variety of formats.

;; add and subtract times, as well as multiply them
;; ie.  6 units of one month and one day

;; different calanders (seperate file)
;;  default =  Christian Era (A. D.) (Anno Domino) (A. C. E.)
;;  we can have long dates printed out like that.
;; if we store the UT in long format we can hack BC etc.
;; don't forget about lost days in 1580 or so.


(DEFVAR *TIME-PACKAGE* (PKG-FIND-PACKAGE "TIME") "The package TIME")
(DEFVAR *DEFAULT-LANGUAGE* "English"
  "The default language to use when telling the time.") ;belongs in site info
;;so put these things there anyway!!
;; where are latitude and longitude anyway???
;; they should be in site info.

(DEFUN DELQ-ALL (LIST SYMBOLS)
  "Delete all occurrences of all elements of SYMBOLS from LIST."
  (DOLIST (SYMBOL SYMBOLS)
    (SETQ LIST (DELQ SYMBOL LIST)))
  LIST)

;;; Lexical analyzer.

(DEFCONST *SPECIAL-CHAR-SYMBOLS* '((#/- . -) (#/+ . +) (#// . //)
                                   (#/: . /:) (#/, . /,) (#/. . /.) (#/; . /;))
  "The list of all the special chars you can find when parsing a date.")

;;; This function is the lexical analyzer of the parser; it splits a string
;;; up into tokens.  It takes a string, and optionally the starting and finishing
;;; indexes of a substring within the string to use.  It returns a list.
;;; Each element of the list corresponds to a token.  Numbers, interpreted
;;; in decimal, appear in the list as two-lists; the first element is a fixnum
;;; giving the value of the number, and the second is the number of digits
;;; (including leading zeroes!).  Any other token appears as
;;; a symbol (interned in the Time package).
(DEFUN LEXICALLY-ANALYZE (STRING &OPTIONAL (START 0) (END (STRING-LENGTH STRING)))
  ;; Each time around this loop we either skip over some uninteresting text,
  ;; or we cons another token onto the list of tokens.
  (DO ((INDEX START)
       (RESULT NIL))
      (( INDEX END)
       (NREVERSE RESULT))
    (LET ((CHAR (AREF STRING INDEX)))
      (COND ((MEMQ CHAR '(#/SPACE #/TAB #/' #/RETURN))
             ;; This is a whitespace character, ignore it.
             (SETQ INDEX (1+ INDEX)))
            ((ASSQ CHAR *SPECIAL-CHAR-SYMBOLS*)
             ;; This is a special character.  Make a new token which is is
             ;; symbol whose name is that character.
             (PUSH (CDR (ASSQ CHAR *SPECIAL-CHAR-SYMBOLS*)) RESULT)
             (SETQ INDEX (1+ INDEX)))
            ((DIGIT-CHAR-P CHAR)
             ;; This is the beginning of a number in decimal.  Make a new token
             ;; which is a fixnum of this number's value.
             (DO ((I (1+ INDEX) (1+ I))
                  (DIGITS 1 (1+ DIGITS))
                  (N 0))
                 (NIL)
               (SETQ N (+ (* N 10.) (- CHAR #/0)))
               (COND ((OR ( I END)
                          (NOT (DIGIT-CHAR-P (SETQ CHAR (AREF STRING I)))))
                      (PUSH (LIST N DIGITS) RESULT)
                      (SETQ INDEX I)
                      (RETURN NIL)))))
            ((ALPHA-CHAR-P CHAR)
             ;; This is the beginning of an alphabetic token.  Scan over all contiguous
             ;; letters, upcasing them, and make a new token which is a symbol.
             (DO ((I INDEX (1+ I)))
                 ((OR ( I END)
                      (LET ((CHAR (AREF STRING I)))
                        (AND (NOT (OR (= CHAR #/@) (ALPHA-CHAR-P CHAR)))
                             ;; This clause is to make "A.M." and "P.M." work, but note
                             ;; that trailing dots are ignored (see below).
                             (NOT (CHAR-EQUAL CHAR #/.))
                             ;; If we are inside an alphabetic token and see a hypen followed
                             ;; by a letter, accept this; e.g. "twenty-third".
                             (NOT (AND (CHAR-EQUAL CHAR #/-)
                                       (< (1+ I) END)
                                       (ALPHA-CHAR-P (AREF STRING (1+ I)))
                                       ;; Special kludge: if it is AM- or PM-, break
                                       ;; here; some hosts send AM-EDT and such.
                                       (NOT (MEMBER (SUBSTRING STRING (MAX 0 (- I 2)) I)
                                                    '("AM" "PM" "am" "pm"))))))))
                  (PUSH (MONTH-INTERN
                          (STRING-UPCASE
                            (NSUBSTRING STRING
                                        INDEX
                                        ;; Strip trailing dots, for "Wed." and "Oct.".
                                        ;; Unfortunately also for "A.M.", see way below.
                                        (IF (CHAR-EQUAL (AREF STRING (1- I)) #/.)
                                            (1- I)
                                          I)))
                          *TIME-PACKAGE*)
                        RESULT)
                  (SETQ INDEX I))))
            ((= CHAR #/()
             ;; This is the beginning of a parenthesized string.  RFC733 defines such
             ;; to be equivalent to whitespace, so we ignore this string.  The "Laurel"
             ;; program puts days of the week in parens, but these are redundant so it
             ;; is OK to ignore them.
             (DO ((I INDEX (1+ I)))
                 ((OR ( I END)
                      (= (AREF STRING I) #/)))
                  (SETQ INDEX (1+ I)))))
            (T
             (BARF "Unknown character ~C" CHAR))))))

(DEFCONST *MONTH-SYMBOLS* '(("JAN" . JAN) ("FEB" . FEB) ("MAR" . MAR) ("APR" . APR)
                            ("MAY" . MAY) ("JUN" . JUN) ("JUL" . JUL) ("AUG" . AUG)
                            ("SEP" . SEP) ("OCT" . OCT) ("NOV" . NOV) ("DEC" . DEC))
  "List of symbols for the months.")

(DEFUN MONTH-INTERN (STRING PACKAGE)
  "The same as INTERN, but faster if STRING is a 3-character month name.
Assumes that the package is the TIME package for them."
  (OR (AND (= (STRING-LENGTH STRING) 3)
           (CDR (ASS 'EQUALP STRING *MONTH-SYMBOLS*)))
      (INTERN STRING PACKAGE)))

;;; Defining patterns.

;;; A pattern is a list of N pattern elements, which are compared one-for-one
;;; with the first N elements of the token list we are analyzing.  Each
;;; pattern element may be:
;;; (a) A symbol or fixnum, which matches exactly that symbol or fixnum.
;;; (b) A list whose CAR is a "special pattern" symbol, in which case a special
;;;        function associated with that symbol is invoked.
;;; (c) A list of one symbol, which is interpreted as an arbitrary predicate
;;;        of one argument, which is applied to the token and should return
;;;        true if the token "matches".
;;;
;;; Note: symbols with FIXNUM-STRING properties are treated as if they
;;; were fixnums, rather than symbols.  This is for English cardinal and
;;; ordinal numbers.
;;;
;;; The following special pattern symbols exist, with the following special "forms":
;;;  (FIXP), which matches any fixnum.
;;;  (FIXP <n>), which matches any fixnum with exactly n digits.
;;;  (FIXP <m> <n>), which matches any fixnum with between m and n digits inclusive.
;;;  (FRACTION), which matches any fraction, with articles before or after.
;;;    These fractions are phrases like "a half", not numbers and slashes.
;;;  (FRACTION T), which matches any fraction, not including articles after.
;;;  (ANY-OF <pattern-element> <pattern-element> ...), which matches if any of the
;;;    pattern elements match.
;;;  (GET <property-indicator>), which matches if the token is a symbol with a
;;;    non-NIL <property-indicator> property.
;;;  (ANY), which matches any single token.

;;; Examples:
;;;   ((FIXP 1 2) /: (FIXP 2) /: (FIXP 2))    Matches 11:30:15
;;;   ((GET MONTH) (FIXP 1 2) /, (FIXP 4))    Matches Jan 23, 1980
;;;   (12. (GET MERIDIAN))                    Matches 12 pm or 12 am
;;;   ()                                      Matches anything
;;;   ((ANY))                                 Matches anything except no tokens.

;;; The special form DEFPATTERN defines a pattern, information about when to try to
;;; match it, and what to do if it is matched.  The form looks like this:
;;; (DEFPATTERN <from-state> <pattern> <to-state> <lambda-list> . <body>)
;;; The parser has a state, represented by a symbol.
;;; It finds all the DEFPATTERNs for its current
;;; state by finding all those with <from-state> EQ to the current state.
;;; It applies each pattern in succession.  When it finds a pattern that
;;; matches, it invokes the associated function (defined by the <lambda-list>
;;; and <body> of the DEFPATTERN) and sets the state to <to-state>; it also
;;; CDRs off the matched portion of the token list, proceeding with the rest
;;; of the list.  The argument to the function defined by DEFPATTERN are
;;; all those tokens in the token list that matched the pattern, except
;;; those that were expressed in the pattern as symbols or fixnums (since
;;; these are just constants, the function is not interested in them).
;;; (Those that were expressed as ANY-OF pattern elements ARE passed
;;; to the function, even if the token is a constant, just in case the
;;; function cares which of the choices was taken.
;;;
;;; The parse proceeds until there are no tokens left and the state
;;; has a FINAL-STATE property of T.
;;;
;;; There is another version of DEFPATTERN called DEFPATTERN-PEEK, which
;;; is the same except that it "looks ahead" at the tokens, without
;;; passing over them.  Also, the tokens are not passed to the function;
;;; the function must take zero arguments.
;;;
;;; NOTE that the order of DEFPATTERNs in this file is significant, and
;;; defines the order in which patterns are checked.
;;;
;;; A data structure that allows the parser to find all the patterns is
;;; constructed at LOAD-time.  A list of known states is maintained as
;;; the value of *STATES*.  Each state is given a PATTERNS property,
;;; consisting of a list of elements of the form:
;;;  (<pattern> <to-state> <function-to-call> <skip-tokens>)
;;; These lists are CONSed up in reverse order, and NREVERSEd at the end.
;;; <skip-tokens> is true for DEFPATTERN and false for DEFPATTERN-PEEK.

(DEFMACRO DEFPATTERN (FROM-STATE PATTERN TO-STATE LAMBDA-LIST . BODY)
  (LET ((FUNCTION-NAME (GENSYM)))
    `(PROGN 'COMPILE
            (DEFUN ,FUNCTION-NAME ,LAMBDA-LIST . ,BODY)
            (DEFINE-PATTERN ',FROM-STATE ',PATTERN ',TO-STATE ',FUNCTION-NAME T))))

(DEFMACRO DEFPATTERN-PEEK (FROM-STATE PATTERN TO-STATE LAMBDA-LIST . BODY)
  (LET ((FUNCTION-NAME (GENSYM)))
    `(PROGN 'COMPILE
            (DEFUN ,FUNCTION-NAME ,LAMBDA-LIST . ,BODY)
            (DEFINE-PATTERN ',FROM-STATE ',PATTERN ',TO-STATE ',FUNCTION-NAME NIL))))

;(DEFVAR *STATES* NIL)  Need to check if unbound.
(DECLARE (SPECIAL *STATES*))

;;; This function gets invoked once at load-time before any of the patters
;;; are defined.  There must be exactly one top-level call to this
;;; function in the file, and it must be before all the DEFPATTERNs.
(DEFUN START-PATTERNS ()
  (IF (BOUNDP '*STATES*)
      ;; We are reloading.
      (DOLIST (STATE *STATES*)
        (REMPROP STATE 'PATTERNS)))
  (SETQ *STATES* NIL))

;;; This function runs once at load-time for each DEFPATTERN.  This DEFUN must
;;; appear before any calls to DEFPATTERN in this file.
(DEFUN DEFINE-PATTERN (FROM-STATE PATTERN TO-STATE FUNCTION-NAME SKIP-PATTERNS)
  (OR (MEMQ FROM-STATE *STATES*)
      (PUSH FROM-STATE *STATES*))
  (PUSH (LIST PATTERN TO-STATE FUNCTION-NAME SKIP-PATTERNS)
        (GET FROM-STATE 'PATTERNS)))

;;; This function gets invoked once at load-time after all the patterns
;;; are defined.  There must be exactly one top-level call to this
;;; function in this file, and it must be after all the DEFPATTERNs.
(DEFUN FINISH-PATTERNS ()
  (DOLIST (STATE *STATES*)
    (SETF (GET STATE 'PATTERNS)
          (NREVERSE (GET STATE 'PATTERNS)))))

;;; Parser.

;;; This is the function that interprets patterns according to the algorithm
;;; described above.  It returns the final value of STATE.

(DEFUN PARSE-1 (TOKEN-LIST INITIAL-STATE)
  (DO ((STATE INITIAL-STATE)
       (TOKENS TOKEN-LIST))
      ((AND (NULL TOKENS) (GET STATE 'FINAL-STATE))
       STATE)
    ;; Try matching the first tokens of TOKENS against all the patterns
    ;; associated with STATE.
    (DO ((TRIES (GET STATE 'PATTERNS) (CDR TRIES)))
        ((NULL TRIES)
         (BARF "No pattern matches the tokens ~S in state ~S" TOKENS STATE))
     (LET ((TRY (CAR TRIES))
           MATCHED-TOKENS)
       ;; TRY represents one of the patterns associated with STATE; it looks
       ;; like (<pattern> <to-state> <function-name> <skip-tokens>).
       (COND ((MULTIPLE-VALUE (NIL MATCHED-TOKENS)
                (PATTERN-MATCH (FIRST TRY) TOKENS))
              ;; Found it!  Run the function, advance over the matched tokens,
              ;; go to the new state and continue.
              (LET ((RESULT (PATTERN-INVOKE (FIRST TRY) MATCHED-TOKENS
                                            (THIRD TRY) (FOURTH TRY))))
                (IF (FOURTH TRY)
                    (SETQ TOKENS RESULT)))
              (SETQ STATE (SECOND TRY))
              (RETURN NIL)))))))

;;; Try to match PATTERN against the beginning of TOKEN-LIST.
;;; Return possibly altered token list if they match, else NIL.
(DEFUN PATTERN-MATCH (PATTERN TOKEN-LIST)
  (DECLARE (RETURN-LIST MATCHP EDITED-TOKEN-LIST))
  ;; Check specially for two possible first elements of the pattern
  ;; that are the ones we check for in parsing dates from file servers.
  (COND ((AND (EQUAL (CAR PATTERN) '(FIXP 1 2))
              (CADR PATTERN) (SYMBOLP (CADR PATTERN))
              (NEQ (CADR TOKEN-LIST) (CADR PATTERN)))
         NIL)
        ((AND (EQUAL (CAR PATTERN) '(ANY-OF /: /.))
              (NOT (MEMQ (CAR TOKEN-LIST) '(/: /.))))
         NIL)
        (T
         (DO ((PATTERN PATTERN (CDR PATTERN))
              (ENTIRE-TOKEN-LIST TOKEN-LIST)
              (EDITED-TOKEN-LIST)
              (TOKEN-LIST TOKEN-LIST))
             (NIL)
           (COND ((NULL PATTERN)
                  ;; We've matched each element.  Matches!
                  (RETURN (VALUES T ENTIRE-TOKEN-LIST)))
                 ((NULL TOKEN-LIST)
                  ;; There is more pattern, but no more tokens.  No match.
                  (RETURN NIL))
                 ((NOT (MULTIPLE-VALUE (NIL TOKEN-LIST EDITED-TOKEN-LIST)
                         (MATCH-ELEMENT (CAR PATTERN) TOKEN-LIST ENTIRE-TOKEN-LIST)))
                  ;; This element does not match, lose.
                  (RETURN NIL))
                 (EDITED-TOKEN-LIST
                  (SETQ ENTIRE-TOKEN-LIST EDITED-TOKEN-LIST)))))))

;;; Predicate: Does PATTERN-ELEMENT match the first TOKEN(s) of TOKEN-LIST?
;;; Second value the remaining tokens not matched.
(DEFUN MATCH-ELEMENT (PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST
                      &AUX (TOKEN (CAR TOKEN-LIST))
                      (REMAINING-TOKEN-LIST (CDR TOKEN-LIST))
                      EDITED-TOKEN-LIST
                      MATCHP)
  (DECLARE (RETURN-LIST MATCHP REMAINING-TOKENS EDITED-TOKEN-LIST))
  (SETQ MATCHP
    (COND ((SYMBOLP PATTERN-ELEMENT)
           ;; The pattern element is a symbol; matching is judged by EQness.
           (EQ PATTERN-ELEMENT TOKEN))
          ((FIXP PATTERN-ELEMENT)
           ;; Match any fixnum of this value, no matter what its length.
           (OR
             ;; Detect multi-token fractions of all sorts, plus noise words.
             (MULTIPLE-VALUE-BIND (FLAG REMAINING EDITED-LIST)
                 (MATCH-FRACTION PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST)
               (IF FLAG
                   (SETQ REMAINING-TOKEN-LIST REMAINING
                         EDITED-TOKEN-LIST EDITED-LIST))
               FLAG)
             ;; Next possibility: a number made of digits.
             (AND (LISTP TOKEN)
                  (FIXP (FIRST TOKEN))
                  (= (FIRST TOKEN) PATTERN-ELEMENT))
             ;; Other possibility: a string number.
             (AND (SYMBOLP TOKEN)
                  (GET TOKEN 'FIXNUM-STRING)
                  (= (GET TOKEN 'VALUE) PATTERN-ELEMENT))))
          ((EQ (FIRST PATTERN-ELEMENT) 'FRACTION)
           (MULTIPLE-VALUE-BIND (FLAG REMAINING EDITED-LIST)
               (MATCH-FRACTION PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST
                               (SECOND PATTERN-ELEMENT))
             (IF FLAG
                 (SETQ REMAINING-TOKEN-LIST REMAINING
                       EDITED-TOKEN-LIST EDITED-LIST))
             FLAG))
          ((EQ (FIRST PATTERN-ELEMENT) 'FIXP)
           ;; Match certain fixnums.
           (OR
             ;; Detect multi-token fractions of all sorts, plus noise words.
             (AND (EQUAL PATTERN-ELEMENT '(FIXP))
                  (MULTIPLE-VALUE-BIND (FLAG REMAINING EDITED-LIST)
                      (MATCH-FRACTION PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST)
                    (IF FLAG
                        (SETQ REMAINING-TOKEN-LIST REMAINING
                              EDITED-TOKEN-LIST EDITED-LIST))
                    FLAG))
             ;; Next possibility: a number made of digits.
             (AND (LISTP TOKEN)
                  (FIXP (FIRST TOKEN))
                  (MATCH-NUMBER PATTERN-ELEMENT (SECOND TOKEN)))
             ;; Other possibility: a string number.
             (AND (SYMBOLP TOKEN)
                  (GET TOKEN 'FIXNUM-STRING)
                  (MATCH-NUMBER PATTERN-ELEMENT (IF (> (GET TOKEN 'VALUE) 9.) 2 1)))))
          ((EQ (FIRST PATTERN-ELEMENT) 'ANY)
           ;; Match any token.
           T)
          ((EQ (FIRST PATTERN-ELEMENT) 'ANY-OF)
           ;; If the TOKEN is any of these things, match.
           (MEMQ TOKEN (REST1 PATTERN-ELEMENT)))
          ((EQ (FIRST PATTERN-ELEMENT) 'GET)
           ;; If TOKEN is a symbol with this property, match.
           (AND (SYMBOLP TOKEN)
                (GET TOKEN (SECOND PATTERN-ELEMENT))))
          (T
           ;; Not a "special" form.  This is a predicate to apply.
           (FUNCALL (FIRST PATTERN-ELEMENT) TOKEN))))
  (VALUES MATCHP
          REMAINING-TOKEN-LIST
          EDITED-TOKEN-LIST))

(DEFUN MATCH-FRACTION (PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST
                       &OPTIONAL DONT-INCLUDE-FOLLOWING-ARTICLES
                       &AUX (TOKEN (CAR TOKEN-LIST))
                       (REMAINING-TOKEN-LIST (CDR TOKEN-LIST))
                       EDITED-TOKEN-LIST
                       MATCHP (NUMBER-OF-TOKENS 1))
  (DECLARE (RETURN-LIST MATCHP REMAINING-TOKEN-LIST EDITED-TOKEN-LIST))
  (OR
    ;; "2.5"
    (AND (LISTP TOKEN)
         (FIXP (CAR TOKEN))
         (EQ (SECOND TOKEN-LIST) '/.)
         (LISTP (THIRD TOKEN-LIST))
         (FIXP (CAR (THIRD TOKEN-LIST)))
         (SETQ MATCHP (READ-FROM-STRING
                        (FORMAT NIL "~D.~D" (CAR TOKEN) (CAR (THIRD TOKEN-LIST)))))
         (SETQ NUMBER-OF-TOKENS 3))
    ;; ".5"
    (AND (EQ TOKEN '/.)
         (LISTP (SECOND TOKEN-LIST))
         (FIXP (CAR (SECOND TOKEN-LIST)))
         (SETQ MATCHP (READ-FROM-STRING
                        (FORMAT NIL "~D.~D" 0 (CAR (SECOND TOKEN-LIST)))))
         (SETQ NUMBER-OF-TOKENS 2))
    ;; "2 a half", which is what we get from "2 and a half"
    ;; since "and" is a noise word.
    (AND (LISTP TOKEN)
         (FIXP (CAR TOKEN))
         (MULTIPLE-VALUE-BIND (FRACTION REMAINING)
             (MATCH-FRACTION '(FIXP) (CDR TOKEN-LIST) ENTIRE-TOKEN-LIST
                             DONT-INCLUDE-FOLLOWING-ARTICLES)
           (IF FRACTION
               (SETQ MATCHP (+ (CAR TOKEN) FRACTION)
                     NUMBER-OF-TOKENS (LENGTH (LDIFF TOKEN-LIST REMAINING))
                     DONT-INCLUDE-FOLLOWING-ARTICLES T))
           FRACTION))
    ;; "A half", etc.
    (AND (SYMBOLP TOKEN)
         (GET TOKEN 'ARTICLE)
         (SYMBOLP (CADR TOKEN-LIST))
         (GET (CADR TOKEN-LIST) 'FRACTION)
         (SETQ MATCHP (GET (CADR TOKEN-LIST) 'VALUE))
         (SETQ NUMBER-OF-TOKENS 2))
    ;; just "Half".
    (AND (SYMBOLP TOKEN)
         (GET TOKEN 'FRACTION)
         (SETQ MATCHP (GET TOKEN 'VALUE))))
  (AND (FIXP PATTERN-ELEMENT)
       MATCHP
       ( PATTERN-ELEMENT MATCHP)
       (SETQ MATCHP NIL))
  ;; Now discard an article or proposition following the fraction, if any.
  ;; "half a", etc.
  (COND ((NOT DONT-INCLUDE-FOLLOWING-ARTICLES)
         (LET ((TOKEN-AFTER (NTH NUMBER-OF-TOKENS TOKEN-LIST)))
           (AND (EQ TOKEN-AFTER 'OF)
                (INCF NUMBER-OF-TOKENS)))
         (LET ((TOKEN-AFTER (NTH NUMBER-OF-TOKENS TOKEN-LIST)))
           (AND (SYMBOLP TOKEN-AFTER)
                (GET TOKEN-AFTER 'ARTICLE)
                (INCF NUMBER-OF-TOKENS)))))
  ;; Now edit out the tokens we want to replace, if more than one.
  (IF ( NUMBER-OF-TOKENS 1)
      (SETQ REMAINING-TOKEN-LIST (NTHCDR NUMBER-OF-TOKENS TOKEN-LIST)
            EDITED-TOKEN-LIST
            (APPEND (LDIFF ENTIRE-TOKEN-LIST TOKEN-LIST)
                    (LIST (LIST MATCHP))
                    REMAINING-TOKEN-LIST)))
  (VALUES MATCHP REMAINING-TOKEN-LIST EDITED-TOKEN-LIST))


;;; Internal function of MATCH-ELEMENT for matching numbers.
(DEFUN MATCH-NUMBER (PATTERN-ELEMENT LENGTH)
  (SELECTQ (LENGTH PATTERN-ELEMENT)
    (1 T)
    (2 (= (SECOND PATTERN-ELEMENT) LENGTH))
    (3 (AND ( (SECOND PATTERN-ELEMENT) LENGTH)
            ( (THIRD PATTERN-ELEMENT) LENGTH)))))

;;; Call FUNCTION, passing it all the tokens of TOKEN-LIST that were
;;; matched by PATTERN, except the constants.
(DEFUN PATTERN-INVOKE (PATTERN TOKEN-LIST FUNCTION PASS-ARGUMENTS)
  (PROG ()
        (%OPEN-CALL-BLOCK FUNCTION 0 0)         ; No ADI, destination IGNORE.
        (IF (NOT PASS-ARGUMENTS) (GO END-LOOP)) ; Don't give it arguments.
        (%ASSURE-PDL-ROOM (+ 4 (LENGTH PATTERN)))       ; (Conservative.)
     LOOP
        (IF (NULL PATTERN)
            (GO END-LOOP))
        (IF (NOT (ATOM (CAR PATTERN)))
            (%PUSH (CAR TOKEN-LIST)))
        (SETQ PATTERN (CDR PATTERN))
        (SETQ TOKEN-LIST (CDR TOKEN-LIST))
        (GO LOOP)
     END-LOOP
        (%ACTIVATE-OPEN-CALL-BLOCK)
        (RETURN TOKEN-LIST)))

;;; Given a token that represents a number, return the number's value.
(DEFUN NUMBER-VALUE (TOKEN)
  (COND ((AND (LISTP TOKEN)
              (NUMBERP (FIRST TOKEN)))
         ;; This is a number token made of digits.
         (FIRST TOKEN))
        ((AND (SYMBOLP TOKEN)
              (GET TOKEN 'FIXNUM-STRING))
         ;; This is an English ordinal or cardinal.
         (GET TOKEN 'VALUE))
        (T (FERROR NIL "The token ~S is not a number at all." TOKEN))))

;;; Keywords.

;;; This stuff runs at LOAD time.  It sets up properties on various interesting
;;; keyword symbols, so that patterns can check for these properties.

;;; The argument is a list of lists.  Each list is a bunch of spellings
;;; of a value, each of which is a string; successive lists have successive values,
;;; starting at FIRST-VALUE.  Each spelling is turned into a symbol, which gets
;;; a VALUE property of the fixnum value, and a <TYPE> property of T.
(DEFUN ASSIGN-TYPE-AND-VALUES (TYPE LIST-OF-LISTS FIRST-VALUE)
  (DO ((REST LIST-OF-LISTS (CDR REST))
       (I FIRST-VALUE (1+ I)))
      ((NULL REST))
    (DOLIST (STRING (CAR REST))
      (IF (STRINGP STRING)                      ; Don't bash plist of NIL.
          (LET ((SYMBOL (INTERN (STRING-UPCASE STRING) *TIME-PACKAGE*)))
            (PUTPROP SYMBOL I 'VALUE)
            (PUTPROP SYMBOL T TYPE))))))

;;; NOTE: This file must be loaded after the TIME file.
(ASSIGN-TYPE-AND-VALUES 'DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK* 0)
(ASSIGN-TYPE-AND-VALUES 'MONTH *MONTHS* 1)

;;; Take a list of lists of symbols.  Every symbol gets a <type> property
;;; of T and a VALUE property of the first symbol of the list.
(DEFUN ASSIGN-TYPE-AND-VALUES-SYMBOLS (TYPE VALUE-PROP-NAME LIST-OF-LISTS)
  (DOLIST (LIST-OF-SYMBOLS LIST-OF-LISTS)
    (LET ((FIRST-SYMBOL (FIRST LIST-OF-SYMBOLS)))
      (DOLIST (SYMBOL LIST-OF-SYMBOLS)
        (PUTPROP SYMBOL FIRST-SYMBOL VALUE-PROP-NAME)
        (PUTPROP SYMBOL T TYPE)))))

(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'HALF-DAY 'VALUE
 '((NOON N)
   (MIDNIGHT M)))

(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'OFFSET 'OFFSET-VALUE
 '((YEARS YEAR YR Y)
   (MONTHS MONTH MO)
   (WEEKS WEEK WK)
   (DAYS DAY DA DY D)
   (HOURS HOUR HR H)
   (MINUTES MINUTE MINS MIN MN M)
   (SECONDS SECOND SECS SEC SC S)))

(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'MERIDIAN 'VALUE
 '((AM |A.M.| |A.M|)
   (PM |P.M.| |P.M|)))

(DEFUN ASSIGN-TIME-ZONES ()
  (DOLIST (ZONE-SPEC *TIMEZONES*)
    (LET ((VALUE (FIRST ZONE-SPEC)))
      (IF (NOT (NULL (SECOND ZONE-SPEC)))
          (ASSIGN-ZONE (SECOND ZONE-SPEC) VALUE))
      (IF (NOT (NULL (THIRD ZONE-SPEC)))
          (ASSIGN-ZONE (THIRD ZONE-SPEC) VALUE))
      (IF (PLUSP (FOURTH ZONE-SPEC))
          (ASSIGN-ZONE (STRING (FOURTH ZONE-SPEC)) VALUE)))))

(DEFUN ASSIGN-ZONE (STRING VALUE)
  (IF (STRINGP STRING)                          ; Don't bash plist of NIL.
      (LET ((SYMBOL (INTERN STRING *TIME-PACKAGE*)))
        (PUTPROP SYMBOL VALUE 'ZONE-VALUE)      ; Can't use VALUE: N and M are half-days too!
        (PUTPROP SYMBOL T 'TIME-ZONE))))

(ASSIGN-TIME-ZONES)

(PUTPROP '- 'SIGN T)
(PUTPROP '+ 'SIGN T)

;;; Cardinal and ordinal numbers.
(DEFUN ASSIGN-NUMBERS ()
  (DOTIMES (I 31.)
    (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT NIL "~:R" I)) *TIME-PACKAGE*))
    (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT NIL "~R" I)) *TIME-PACKAGE*))))

(DEFUN ASSIGN-NUMBER (NUMBER SYMBOL)
  (PUTPROP SYMBOL T 'FIXNUM-STRING)
  (PUTPROP SYMBOL NUMBER 'VALUE))

(ASSIGN-NUMBERS)

;;; Make indefinite articles work, so that "a minute" and "an hour" will be accepted.
(ASSIGN-NUMBER 1 'A)
(ASSIGN-NUMBER 1 'AN)

;;; Make "a half" and "half a" work in MATCH-ELEMENT.
(DEFPROP A T ARTICLE)
(DEFPROP AN T ARTICLE)
(DEFPROP ONE T ARTICLE)
(DEFPROP HALF T FRACTION)
(DEFPROP QUARTER T FRACTION)
(ASSIGN-NUMBER .5 'HALF)
(ASSIGN-NUMBER .25 'QUARTER)

;;; German numbers.
(IF (GET ':GERMAN 'SI:PRINC-FUNCTION)
    (ASSIGN-GERMAN-NUMBERS))

(DEFUN ASSIGN-GERMAN-NUMBERS ()
  (LET ((*PRINT-BASE* ':GERMAN))
    (DOTIMES (I 31.)
      (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT NIL "~S" I)) *TIME-PACKAGE*)))))

;;; The patterns.

;;; Buzz words.  The, And, Of, 1st, 2nd, 3rd, 4th.
(DEFCONST *NOISE-WORDS* '(THE AND OF AT ST ND RD TH /;)
  "The buzz words in the TIME package.  Handles stuff like THE, AND, OF, 1ST, 2ND ...")

(START-PATTERNS)

;;; 3-Jan-80 means Jan 3, 1980.
;;; Put this first so Twenex file servers run fast.
;;; This pattern is so specific that nothing that
;;; follows ought to override it.
(DEFPATTERN MAIN ((FIXP 1 2) - (GET MONTH) - (FIXP 2)) MAIN
            (DATE MONTH YEAR)
  (SET-DATE DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-YEAR-OF-CENTURY YEAR))

;;; 3/15/80 means March 15, 1980.  15/3/80 means March 15, 1980 to a non-amerikan
;;; If both the numbers are small, an ambuguity must be dealt with.
;;; Put this here so that ITS file servers run fast.
(DEFPATTERN MAIN ((FIXP 1 2) // (FIXP 1 2) // (FIXP 2)) MAIN
            (MONTH DATE YEAR-OF-CENTURY)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR-OF-CENTURY YEAR-OF-CENTURY))

;;; 11:30 means 11 hours and 30 minutes, go look for seconds.
;;; This is also used by ITS and Twenex file servers.
(DEFPATTERN MAIN ((FIXP 1 2) /: (FIXP 2)) SECOND
            (HOUR MINUTE)
  (SET-HOUR HOUR)
  (SET-MINUTE MINUTE))

;;; ISO formats
;;; 1980-3-15 means 15 March, 1980.
(DEFPATTERN MAIN ((FIXP 4) - (FIXP 1 2) - (FIXP 1 2)) MAIN
            (YEAR MONTH DATE)
  (SET-MONTH MONTH)
  (SET-DATE DATE)
  (SET-YEAR YEAR))

;;; 1980-MAR-15 means 15 March, 1980.
(DEFPATTERN MAIN ((FIXP 4) - (GET MONTH) - (FIXP 1 2)) MAIN
            (YEAR MONTH DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE)
  (SET-YEAR YEAR))

;;; March 15 means 15 March; go look for a year preceeded by a comma.
(DEFPATTERN MAIN ((GET MONTH) (FIXP 1 2)) YEAR-COMMA
            (MONTH DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE))

;;; 15 March means 15 March; go look for a year.
(DEFPATTERN MAIN ((FIXP 1 2) (GET MONTH)) YEAR-COMMA
            (DATE MONTH)
  (SET-DATE DATE)
  (SET-MONTH-FROM-NAME MONTH))

;;; 3/15/1980 means 15 March 1980.  Same non-amerikan problem.
(DEFPATTERN MAIN ((FIXP 1 2) // (FIXP 1 2) // (FIXP 4)) MAIN
            (MONTH DATE YEAR)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR YEAR))

;;; 3/15 means 15 March, year defaults.  Same non-amerikan problem.
(DEFPATTERN MAIN ((FIXP 1 2) // (FIXP 1 2)) MAIN
            (MONTH DATE)
  (SET-MONTH-AND-DATE MONTH DATE))

;;; Note: GDixon's convert_date_to_binary_.rd believes in YY-MM-DD; the code
;;; below believes in MM-DD-YY.  RFC733 does not allow numeric months at all.

;;; 3-15-80 means 15 March 1980.  Same non-amerikan problem.
(DEFPATTERN MAIN ((FIXP 1 2) - (FIXP 1 2) - (FIXP 2)) MAIN
            (MONTH DATE YEAR-OF-CENTURY)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR-OF-CENTURY YEAR-OF-CENTURY))

;;; 3-15-1980 means 15 March 1980.  Same non-amerikan problem.
(DEFPATTERN MAIN ((FIXP 1 2) - (FIXP 1 2) - (FIXP 4)) MAIN
            (MONTH DATE YEAR)
  (SET-MONTH-AND-DATE MONTH DATE)
  (SET-YEAR YEAR))

;;; 3-15 means 15 March, year defaults.  Same non-amerikan problem.
(DEFPATTERN MAIN ((FIXP 1 2) - (FIXP 1 2)) MAIN
            (MONTH DATE)
  (SET-MONTH-AND-DATE MONTH DATE))

;;; 3-Jan-1980 means 3 Jan 1980.
(DEFPATTERN MAIN ((FIXP 1 2) - (GET MONTH) - (FIXP 4)) MAIN
            (DATE MONTH YEAR)
  (SET-DATE DATE)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-YEAR YEAR))

;;; Jan-3-80 means 3 Jan 1980.
(DEFPATTERN MAIN ((GET MONTH) - (FIXP 1 2) - (FIXP 2)) MAIN
            (MONTH DATE YEAR)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE)
  (SET-YEAR-OF-CENTURY YEAR))

;;; Jan-3-1980 means 3 Jan 1980.
(DEFPATTERN MAIN ((GET MONTH) - (FIXP 1 2) - (FIXP 4)) MAIN
            (MONTH DATE YEAR)
  (SET-MONTH-FROM-NAME MONTH)
  (SET-DATE DATE)
  (SET-YEAR YEAR))

;;; 1130.4 means 11 hours and 30.4 minutes, in Multics internal headers,
;;; which Zmail sometimes actually sees.  (I think this happens when
;;; a QSEND from Multics turns into mail.)
(DEFPATTERN MAIN ((FIXP 4) /. (FIXP 1)) MAIN
            (HHMM TENTHS-OF-MINUTES)
  (SET-HHMM HHMM)
  (SET-TENTHS-OF-MINUTE TENTHS-OF-MINUTES))

;;; 1130. means 11 hours and 30 minutes and zero seconds.
(DEFPATTERN MAIN ((FIXP 4) /.) MAIN
            (HHMM)
  (SET-HHMM HHMM))

;;; 1130 means 11 hours and 30 minutes and zero seconds.
(DEFPATTERN MAIN ((FIXP 4)) MAIN
            (HHMM)
  (SET-HHMM HHMM))

;;; 113015 means 11 hours, 30 minutes and 15 seconds.
(DEFPATTERN MAIN ((FIXP 6)) MAIN
            (HHMMSS)
  (SET-HHMMSS HHMMSS))

;;; Allow the format 11:12:03 1982 which UNIX seems to put in messages.
(DEFPATTERN SECOND ((ANY-OF /: /.) (FIXP 1 2) (FIXP 4)) MAIN
            (IGNORE SECOND YEAR)
  (SET-YEAR YEAR)
  (SET-SECOND SECOND))

;;; Looking for seconds, :23 means 23 seconds, look for AM/PM.
;;; .23 works too; this is a European form.
(DEFPATTERN SECOND ((ANY-OF /: /.) (FIXP 1 2)) MERIDIAN
            (IGNORE SECOND)
  (SET-SECOND SECOND))

;;; Looking for seconds, not finding them, look for AM/PM.
(DEFPATTERN SECOND () MERIDIAN
            ()
  (SET-SECOND '(0 2)))

;;; Looking for meridian, AM means AM and PM means PM, go back to main state.
(DEFPATTERN MERIDIAN ((GET MERIDIAN)) MAIN
            (MERIDIAN)
  (SET-MERIDIAN MERIDIAN))

;;; Looking for meridian, not finding it, go back to main state.
(DEFPATTERN MERIDIAN () MAIN
            ()
   )

;;; 4 PM means what you would think.
(DEFPATTERN MAIN ((FIXP 1 2) (GET MERIDIAN)) MAIN
            (HOUR MERIDIAN)
  (SET-HOUR HOUR)
  (SET-MERIDIAN MERIDIAN)
  (SET-MINUTE '(0 2))
  (SET-SECOND '(0 2)))

;;; Day of the week, as in "Friday, Jan 5"
(DEFPATTERN MAIN ((GET DAY-OF-THE-WEEK) /,) MAIN
            (DAY-OF-THE-WEEK)
  (SET-DAY-OF-THE-WEEK DAY-OF-THE-WEEK))

;;; Day of the week.
(DEFPATTERN MAIN ((GET DAY-OF-THE-WEEK)) MAIN
            (DAY-OF-THE-WEEK)
  (SET-DAY-OF-THE-WEEK DAY-OF-THE-WEEK))

;;; These patterns inserted by CAL 10/24/80

;;; "today"
(DEFPATTERN MAIN (TODAY) MAIN
            ()
            (SET-TODAY))

;;; "yesterday"
(DEFPATTERN MAIN (YESTERDAY) MAIN
            ()
            (SET-YESTERDAY))

;;; "tomorrow"
(DEFPATTERN MAIN (TOMORROW) MAIN
            ()
            (SET-TOMORROW))

;;; "now"
(DEFPATTERN MAIN (NOW) MAIN
            ()
            (SET-NOW))

;;; "2 days before jan 30"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) BEFORE) MAIN
            (OFFSET-VALUE OFFSET-UNITS)
            (SET-OFFSET '- OFFSET-VALUE OFFSET-UNITS))

;;; "2 minutes past 3 pm"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) PAST) MAIN
            (OFFSET-VALUE OFFSET-UNITS)
            (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; "half past 3pm"
(DEFPATTERN MAIN ((FRACTION) PAST) MAIN
            (HOUR-VALUE)
            (SET-OFFSET '+ HOUR-VALUE 'HOURS))

;;; "20 past 3 pm"
(DEFPATTERN MAIN ((FIXP) PAST) MAIN
            (MINUTE-VALUE)
            (SET-OFFSET '+ MINUTE-VALUE 'MINUTES))

(DEFPROP OF T OF-OR-TO)
(DEFPROP TO T OF-OR-TO)

;;; "2 minutes of 3 pm"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) (GET OF-OR-TO)) MAIN
            (OFFSET-VALUE IGNORE OFFSET-UNITS)
            (SET-OFFSET '- OFFSET-VALUE OFFSET-UNITS))

;;; "a quarter of 3pm"
(DEFPATTERN MAIN ((FRACTION T) (GET OF-OR-TO)) MAIN
            (HOUR-VALUE IGNORE)
            (SET-OFFSET '- HOUR-VALUE 'HOURS))

;;; "20 of 3 pm"
(DEFPATTERN MAIN ((FIXP) (GET OF-OR-TO)) MAIN
            (MINUTE-VALUE IGNORE)
            (SET-OFFSET '- MINUTE-VALUE 'MINUTES))

;;; "The day before yesterday" or "day before yesterday"
(DEFPATTERN MAIN ((GET OFFSET) BEFORE) MAIN
            (OFFSET-UNITS)
            (SET-OFFSET '- '(1 1) OFFSET-UNITS))

;;; "2 days after jan 15"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) AFTER) MAIN
            (OFFSET-VALUE OFFSET-UNITS)
            (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; "The day after jan 15", "day after tomorrow"
(DEFPATTERN MAIN ((GET OFFSET) AFTER) MAIN
            (OFFSET-UNITS)
            (SET-OFFSET '+ '(1 1) OFFSET-UNITS))

;;; "5 minutes from now"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) FROM) MAIN
            (OFFSET-VALUE OFFSET-UNITS)
            (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; "3 days ago"
(DEFPATTERN MAIN ((FIXP) (GET OFFSET) AGO) MAIN
            (OFFSET-VALUE OFFSET-UNITS)
            (SET-NOW)
            (SET-OFFSET '- OFFSET-VALUE OFFSET-UNITS))

;;; "dlw's birthday"
(DEFPATTERN MAIN ((ANY) S BIRTHDAY) MAIN
            (NAME)
            (SET-BIRTHDAY NAME))

;;; "my birthday"
(DEFPATTERN MAIN (MY BIRTHDAY) MAIN
            ()
            (SET-BIRTHDAY USER-ID))

;;; 11.30 works like 11:30; this is a European form.
(DEFPATTERN MAIN ((FIXP 1 2) /. (FIXP 2)) SECOND
            (HOUR MINUTE)
  (SET-HOUR HOUR)
  (SET-MINUTE MINUTE))

;;; Ed says that Agatha Christie books use 11.3 to mean 11:30:00, also.
(DEFPATTERN MAIN ((FIXP 1 2) /. (FIXP 1)) SECOND
            (HOUR TENS-OF-MINUTES)
  (SET-HOUR HOUR)
  (SET-TENS-OF-MINUTES TENS-OF-MINUTES))

;;; 12 Noon and friends.
;;; This must follow "3 minutes from ...", which includes "12 m from ...".
(DEFPATTERN MAIN (12. (GET HALF-DAY)) MAIN
            (HALF-DAY)
            (SET-HALF-DAY HALF-DAY))

;;; Noon and friends.
(DEFPATTERN MAIN ((GET HALF-DAY)) MAIN
            (HALF-DAY)
            (SET-HALF-DAY HALF-DAY))

;;; - 3 minutes
(DEFPATTERN MAIN ((GET SIGN) (FIXP) (GET OFFSET)) MAIN
            (SIGN OFFSET-VALUE OFFSET-UNITS)
  (SET-OFFSET SIGN OFFSET-VALUE OFFSET-UNITS))

;;; 3 minutes
(DEFPATTERN MAIN ((FIXP) (GET OFFSET)) MAIN
            (OFFSET-VALUE OFFSET-UNITS)
  (SET-OFFSET '+ OFFSET-VALUE OFFSET-UNITS))

;;; Time zones
(DEFPATTERN MAIN ((GET TIME-ZONE)) MAIN
            (TIME-ZONE)
  (SET-TIME-ZONE TIME-ZONE))

;;; Time zones preceeded by a hyphen
(DEFPATTERN MAIN (- (GET TIME-ZONE)) MAIN
            (TIME-ZONE)
  (SET-TIME-ZONE TIME-ZONE))

(DEFPATTERN MAIN (CHRISTMAS) MAIN
            ()
            (SET-CHRISTMAS))

(DEFPATTERN MAIN (HALLOWEEN) MAIN
            ()
            (SET-HALLOWEEN))

(DEFPATTERN MAIN (NEW YEARS) MAIN
            ()
            (SET-NEW-YEARS))

(DEFPATTERN MAIN (NEW YEARS DAY) MAIN
            ()
            (SET-NEW-YEARS))

;;; If we encounter random commas in MAIN state, we have to just ignore them
;;; in order to win in such cases as "Thursday, 21 May 1981, 00:27-EDT"
(DEFPATTERN MAIN (/,) MAIN
            ()
  )

;;; Handle a general time followed by FROM (or something like it)
;;; and another time.
(DEFPROP FROM + DIRECTION)
(DEFPROP AFTER + DIRECTION)
(DEFPROP BEFORE - DIRECTION)
(DEFPROP TILL - DIRECTION)
(DEFPROP TO - DIRECTION)

(DEFPATTERN MAIN ((GET DIRECTION)) MAIN
            (DIRECTION)
            (MOVE-ABS-TO-OFFSET (GET DIRECTION 'DIRECTION)))

;;; Anything else
(DEFPATTERN MAIN ((ANY)) MAIN
            (TOKEN)
  (BARF "Unrecognized date//time format, starting with token ~A." TOKEN))

;;; If nothing is left and we are in MAIN state, that is the end.
(PUTPROP 'MAIN 'T 'FINAL-STATE)

;;; We just saw "Jan 23", look for a comma followed by a year, e.g. "Jan 23, 80"
(DEFPATTERN YEAR-COMMA (/, (FIXP 2)) MAIN
            (YEAR-OF-CENTURY)
  (SET-YEAR-OF-CENTURY YEAR-OF-CENTURY))

;;; We just saw "Jan 23", look for a comma followed by a year, e.g. "Jan 23, 1980"
(DEFPATTERN YEAR-COMMA (/, (FIXP 4)) MAIN
            (YEAR)
  (SET-YEAR YEAR))

;;; If there isn't a comma, go look for the regular kinds of years.
(DEFPATTERN YEAR-COMMA () YEAR
            ()
  )

;many of the fixed dates would best be implimented setting up
;an array to be searched...

;;; We are now in the state of looking for a year.  If we see a number,
;;; that may be a year or it may be the start of something else.  For
;;; example, "6 Jan 59" versus "6 Jan 59 minutes" or "6 Jan 3:23:12".
;;; So we have to look ahead for various possibilities and return to
;;; the main state if any of them are happening.  Otherwise, a number
;;; gets interpreted as a year in this context.
(DEFPATTERN-PEEK YEAR ((FIXP) /.) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) //) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) /:) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) (GET MERIDIAN)) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR (12. (GET HALF-DAY)) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR ((GET SIGN) (FIXP) (GET OFFSET)) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) (GET OFFSET)) MAIN
                 ()
  )

(DEFPATTERN-PEEK YEAR ((FIXP) (GET MONTH)) MAIN
                 ()
  (BARF "Date and month seen where year expected."))



;;; Finally, there is no other way to interpret the number.  If there
;;; is a number it must be a year.
(DEFPATTERN YEAR ((FIXP)) MAIN
            (YEAR)
  (SET-YEAR YEAR))

;;; Not a number at all.
(DEFPATTERN YEAR () MAIN
            ()
  )

;;; This is the end of the patterns.  Don't add new ones after this!
(FINISH-PATTERNS)

;;; Special variables.

;;; These variables hold the time values found in the string.  NIL means
;;; that no such value has been seen yet.

;;; Absolute values.
(DEFVAR *ABS-YEAR*)
(DEFVAR *ABS-MONTH*)
(DEFVAR *ABS-DATE*)
(DEFVAR *ABS-HOUR*)
(DEFVAR *ABS-MINUTE*)
(DEFVAR *ABS-SECOND*)
(DEFVAR *ABS-DAY-OF-THE-WEEK*)
(DEFVAR *ABS-TIME-ZONE*)

;;; Relative values, from offsets.
(DEFVAR *REL-YEAR*)
(DEFVAR *REL-MONTH*)
(DEFVAR *REL-DATE*)
(DEFVAR *REL-HOUR*)
(DEFVAR *REL-MINUTE*)
(DEFVAR *REL-SECOND*)
(DEFVAR *REL-DAY-OF-THE-WEEK*)
;(DEFVAR *REL-TIME-ZONE*)

;;; Values of the "base" time.
(DEFVAR *BASE-YEAR*)
(DEFVAR *BASE-MONTH*)
(DEFVAR *BASE-DATE*)
(DEFVAR *BASE-HOUR*)
(DEFVAR *BASE-MINUTE*)
(DEFVAR *BASE-SECOND*)

(DEFVAR *RELATIVE-P*)

;;; Action functions.

;;; These are the functions invoked by the bodies of the DEFPATTERNs.

(DEFUN SET-MONTH-FROM-NAME (MONTH)
  (IF (NOT (NULL *ABS-MONTH*))
      (BARF "Month specified twice."))
  (SETQ *ABS-MONTH* (GET MONTH 'VALUE)))

(DEFUN SET-MONTH (MONTH)
  (IF (NOT (NULL *ABS-MONTH*))
      (BARF "Month specified twice."))
  (SETQ *ABS-MONTH* (NUMBER-VALUE MONTH)))

(DEFUN SET-DATE (DATE)
  (IF (NOT (NULL *ABS-DATE*))
      (BARF "Date specified twice."))
  (SETQ *ABS-DATE* (NUMBER-VALUE DATE)))

;;; Here we have to deal with the incompatibility betweeen U.S. and Everybodyelseintheworld
;;; date format.  If either number is greater than 12., then that number
;;; cannot be the month and so must be the date.  Otherwise, default based
;;; on the location of the machine.
(DEFUN SET-MONTH-AND-DATE (FIRST SECOND)
  (SETQ FIRST (NUMBER-VALUE FIRST) SECOND (NUMBER-VALUE SECOND))
  (COND ((> FIRST 12.)
         (SETQ *ABS-MONTH* SECOND *ABS-DATE* FIRST))
        ((> SECOND 12.)
         (SETQ *ABS-MONTH* FIRST *ABS-DATE* SECOND))
        ((MEMQ *TIMEZONE* '(4 5 6 7 8 9 10.)) ;these timezones cover all of God's Own Country
         ;; Braindamaged Patriotic American date format.
         (SETQ *ABS-MONTH* FIRST *ABS-DATE* SECOND))
        (T
         (SETQ *ABS-MONTH* SECOND *ABS-DATE* FIRST))))

;;; This version takes a fixnum, rather than a two-list.
(DEFUN SET-YEAR-INTERNAL (YEAR)
  (IF (NOT (NULL *ABS-YEAR*))
      (BARF "Year specified twice."))
  (SETQ *ABS-YEAR* YEAR))

(DEFUN SET-YEAR (YEAR)
  (SET-YEAR-INTERNAL (NUMBER-VALUE YEAR)))

(DEFUN SET-YEAR-OF-CENTURY (YEAR-OF-CENTURY)
  (SET-YEAR-INTERNAL
    (+ (NUMBER-VALUE YEAR-OF-CENTURY)
       (* 100. (FLOOR *BASE-YEAR* 100.)))))     ; Multics crockishly assumes 1900.

(DEFUN SET-HHMM (TIME)
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice."))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (SETQ TIME (NUMBER-VALUE TIME)
        *ABS-HOUR* (FLOOR TIME 100.)
        *ABS-MINUTE* (\ (FIX TIME) 100.)))

(DEFUN SET-HHMMSS (TIME)
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice."))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice."))
  (SETQ TIME (NUMBER-VALUE TIME)
        *ABS-HOUR* (FLOOR TIME 10000.)
        TIME (- TIME (* *ABS-HOUR* 10000.))
        *ABS-MINUTE* (FLOOR TIME 100.)
        *ABS-SECOND* (\ (FIX TIME) 100.)))

(DEFUN SET-HOUR (HOUR)
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice."))
  (SETQ *ABS-HOUR* (NUMBER-VALUE HOUR)))

(DEFUN SET-MINUTE (MINUTE)
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (SETQ *ABS-MINUTE* (NUMBER-VALUE MINUTE)))

(DEFUN SET-TENS-OF-MINUTES (TENS-OF-MINUTES)
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice."))
  (SETQ *ABS-MINUTE* (* 10. (NUMBER-VALUE TENS-OF-MINUTES))))

(DEFUN SET-SECOND (SECOND)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice."))
  (SETQ *ABS-SECOND* (NUMBER-VALUE SECOND)))

(DEFUN SET-TENTHS-OF-MINUTE (TENTHS)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice."))
  (SETQ *ABS-SECOND* (* 6 (NUMBER-VALUE TENTHS))))

(DEFUN SET-MERIDIAN (MERIDIAN)
  (IF (OR (NOT (NUMBERP *ABS-HOUR*))
          (< *ABS-HOUR* 0.)
          (> *ABS-HOUR* 12.))
      (BARF "Meridian value ~A seen in bad context." MERIDIAN))
  (SETQ *ABS-HOUR*
        (IF (EQ (GET MERIDIAN 'VALUE) 'PM)
            (IF (= *ABS-HOUR* 12.) 12. (+ *ABS-HOUR* 12.))
            (IF (= *ABS-HOUR* 12.) 0 *ABS-HOUR*))))

(DEFUN SET-HALF-DAY (HALF-DAY)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice, by the half-day value /"~A/"." HALF-DAY))
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice, by the half-day value /"~A/"." HALF-DAY))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice, by the half-day value /"~A/"." HALF-DAY))
  (SETQ *ABS-HOUR* (IF (EQ (GET HALF-DAY 'VALUE) 'NOON)
                       12.
                       0.)
        *ABS-MINUTE* 0
        *ABS-SECOND* 0))

(DEFUN SET-DAY-OF-THE-WEEK (DAY-OF-THE-WEEK)
  (IF (NOT (NULL *ABS-DAY-OF-THE-WEEK*))
      (BARF "Day of the week specified twice."))
  (SETQ *ABS-DAY-OF-THE-WEEK* (GET DAY-OF-THE-WEEK 'VALUE)))

(DEFUN SET-TIME-ZONE (TIME-ZONE)
  (IF (NOT (NULL *ABS-TIME-ZONE*))
      (BARF "Time zone specified twice."))
  (SETQ *ABS-TIME-ZONE* (GET TIME-ZONE 'ZONE-VALUE)))

(DEFUN SET-OFFSET (SIGN VALUE UNITS)
  (LET ((VALUE (* (NUMBER-VALUE VALUE) (IF (EQ SIGN '+) 1. -1.))))
    (SELECTQ (GET UNITS 'OFFSET-VALUE)
      (YEARS (SETQ *REL-YEAR* (+ *REL-YEAR* VALUE)))
      (MONTHS (SETQ *REL-MONTH* (+ *REL-MONTH* VALUE)))
      (WEEKS (SETQ *REL-DATE* (+ *REL-DATE* (* 7 VALUE))))
      (DAYS (SETQ *REL-DATE* (+ *REL-DATE* VALUE)))
      (HOURS (SETQ *REL-HOUR* (+ *REL-HOUR* VALUE)))
      (MINUTES (SETQ *REL-MINUTE* (+ *REL-MINUTE* VALUE)))
      (SECONDS (SETQ *REL-SECOND* (+ *REL-SECOND* VALUE)))
      (OTHERWISE (BARF "Bad units" UNITS)))))

;Used in handling "2:30 from now".
;Turn the time we have so far into an offset,
;and clear out the absolute time.
(DEFUN MOVE-ABS-TO-OFFSET (SIGN)
  (AND *ABS-YEAR* (SETQ *REL-YEAR* (FUNCALL SIGN (OR *REL-YEAR* 0) *ABS-YEAR*)))
  (AND *ABS-MONTH* (SETQ *REL-MONTH* (FUNCALL SIGN (OR *REL-MONTH* 0) *ABS-MONTH*)))
  (AND *ABS-DATE* (SETQ *REL-DATE* (FUNCALL SIGN (OR *REL-DATE* 0) *ABS-DATE*)))
  (AND *ABS-HOUR* (SETQ *REL-HOUR* (FUNCALL SIGN (OR *REL-HOUR* 0) *ABS-HOUR*)))
  (AND *ABS-MINUTE*
       (SETQ *REL-MINUTE* (FUNCALL SIGN (OR *REL-MINUTE* 0) *ABS-MINUTE*)))
  (AND *ABS-SECOND*
       (SETQ *REL-SECOND* (FUNCALL SIGN (OR *REL-SECOND* 0) *ABS-SECOND*)))
  (SETQ *ABS-YEAR* NIL
        *ABS-MONTH* NIL
        *ABS-DATE* NIL
        *ABS-HOUR* NIL
        *ABS-MINUTE* NIL
        *ABS-SECOND* NIL)
  (SETQ *RELATIVE-P* ':RELAVTIVE))

(DEFUN SET-TODAY ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))

(DEFUN SET-YESTERDAY ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *REL-DATE* (1- *REL-DATE*))
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))

(DEFUN SET-TOMORROW ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *REL-DATE* (1+ *REL-DATE*))
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))


(DEFUN SET-NOW ()
  (SETQ *ABS-SECOND* *BASE-SECOND*)
  (SETQ *ABS-MINUTE* *BASE-MINUTE*)
  (SETQ *ABS-HOUR* *BASE-HOUR*)
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))



(DEFUN SET-CHRISTMAS ()
  (SETQ *ABS-SECOND* 0)
  (SETQ *ABS-MINUTE* 0)
  (SETQ *ABS-HOUR* 0)
  (SETQ *ABS-DATE* 25)
  (SETQ *ABS-MONTH* 12)
  (SETQ *ABS-YEAR*
        (COND ((AND (= *BASE-MONTH* 12) (> *BASE-DATE* 25))
               (1+ *BASE-YEAR*))
              (T *BASE-YEAR*))) ;if after December 25, then next year
  )

(DEFUN SET-HALLOWEEN ()
  (SETQ *ABS-SECOND* 0)
  (SETQ *ABS-MINUTE* 0)
  (SETQ *ABS-HOUR* 0)
  (SETQ *ABS-DATE* 31)
  (SETQ *ABS-MONTH* 10)
  (SETQ *ABS-YEAR*
        (COND ((AND (= *BASE-MONTH* 12) (> *BASE-DATE* 25))
               (1+ *BASE-YEAR*))
              (T *BASE-YEAR*))) ;if after December 25, then next year
  )

(DEFUN SET-NEW-YEARS ()
  (SETQ *ABS-SECOND* *BASE-SECOND*)
  (SETQ *ABS-MINUTE* *BASE-MINUTE*)
  (SETQ *ABS-HOUR* *BASE-HOUR*)
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* ':RELATIVE))


(DEFUN SET-BIRTHDAY (USER-ID)
  (PARSE-1 (LEXICALLY-ANALYZE
             (FIND-BIRTHDAY (WITH-OUTPUT-TO-STRING (STANDARD-OUTPUT)
                               (CHAOS:FINGER (STRING-APPEND
                                               "//W "
                                               (IF (STRING-SEARCH-CHAR #/@ USER-ID)
                                                   USER-ID
                                                 (STRING-APPEND
                                                   USER-ID
                                                   "@"
                                                   (SEND (OR FS:USER-LOGIN-MACHINE
                                                             SI:ASSOCIATED-MACHINE)
                                                         ':NAME))))))))
           'MAIN))


;;; Top level.

;;; These are the top level functions and external entrypoints that call
;;; the lexical analyzer and parser; the parser calls the action routines.
;;; Any of these callees may call BARF to report an error; BARF is guaranteed
;;; to THROW out, and therefore not return to its caller.

(DEFMACRO CHECK-RANGE (VARIABLE LOWER UPPER STRING)
  `(IF (OR (< ,VARIABLE ,LOWER)
           (> ,VARIABLE ,UPPER))
       (BARF "~D is ~:[more~;less~] than the number of ~A."
             ,VARIABLE (< ,VARIABLE ,LOWER) ,STRING)))

(DEFUN PARSE (STRING &OPTIONAL (START 0) END (FUTUREP T)
              BASE-TIME MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
              TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  (DECLARE (RETURN-LIST SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P
                        RELATIVE-P))
  (MULTIPLE-VALUE-BIND (ANSWER RELATIVE-P)
      (PARSE-UNIVERSAL-TIME STRING START END FUTUREP BASE-TIME MUST-HAVE-TIME
                            DATE-MUST-HAVE-YEAR TIME-MUST-HAVE-SECOND DAY-MUST-BE-VALID)
    (MULTIPLE-VALUE-BIND (SECS MINUTES HOURS DAY MONTH
                          YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P)
        (DECODE-UNIVERSAL-TIME ANSWER)
      (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P
              RELATIVE-P))))

(DEFUN PARSE-UNIVERSAL-TIME (STRING &OPTIONAL (START 0) END (FUTUREP T) BASE-TIME
                                        MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
                                        TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  "Return a universal-time parsed from STRING, or the part from START to END.
FUTUREP controls the interpretation if there is just a day-of-the-week:
 T means use the next such day, NIL means use the previous.
BASE-TIME is used if the string is a relative time.
 It is what the relative time is relative to.  Default is now.
MUST-HAVE-TIME if T means error if the string is empty.
DATE-MUST-HAVE-YEAR if T means error if no year number.
TIME-MUST-HAVE-SECOND if T means error if time doesn't
 include a number of seconds.
DAY-MUST-BE-VALID if NIL means allow things like February 29
 (which equals March 1 or March 2)."
  (DECLARE (RETURN-LIST UNIVERSAL-TIME RELATIVE-P))
  (PROG KLUDGE ()                               ;This is needed because multiple values
   (IF (AND MUST-HAVE-TIME (EQ STRING ""))
       (FERROR 'PARSE-ERROR "The supplied time string is empty."))
   (IF (NULL END)
       (SETQ END (STRING-LENGTH STRING)))
   (LET ((TEM (PARSE-TWENEX-TIME STRING START END)))
     (IF TEM (RETURN (values TEM NIL))))
   (LET (*ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*
         *ABS-DAY-OF-THE-WEEK* *ABS-TIME-ZONE*
         (*REL-YEAR* 0) (*REL-MONTH* 0) (*REL-DATE* 0) (*REL-HOUR* 0) (*REL-MINUTE* 0)
         (*REL-SECOND* 0) *REL-DAY-OF-THE-WEEK*
;            *REL-TIME-ZONE*
         *BASE-YEAR* *BASE-MONTH* *BASE-DATE* *BASE-HOUR* *BASE-MINUTE* *BASE-SECOND*
         *RELATIVE-P*)

     ;; Compute the "base" time: the time to which the string is relative.
     (COND ((NULL BASE-TIME)
            ;; Time is relative to right now.
            (MULTIPLE-VALUE (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR* *BASE-DATE*
                             *BASE-MONTH* *BASE-YEAR*)
              (GET-TIME))
            ;; If the time is not known, assume a default base time so that we
            ;; can still parse fully-specified date/times (e.g. in the file system)
            (IF (NULL *BASE-SECOND*)
                (SETQ *BASE-SECOND* 0 *BASE-MINUTE* 0 *BASE-HOUR* 0
                      *BASE-DATE* 1 *BASE-MONTH* 1 *BASE-YEAR* 0)))
           (T
            ;; Time is relative to a specified time.
            (MULTIPLE-VALUE (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR*
                             *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
              (DECODE-UNIVERSAL-TIME  BASE-TIME))))

     ;; Do the parse, calling the action routines, which work by setting the
     ;; ABS and REL special variables bound above.
     (PARSE-1 (DELQ-ALL (LEXICALLY-ANALYZE STRING START END) *NOISE-WORDS*) 'MAIN)
     (IF (AND DATE-MUST-HAVE-YEAR (NULL *ABS-YEAR*))
         (BARF "no year supplied"))
     (IF (AND TIME-MUST-HAVE-SECOND (NULL *ABS-SECOND*))
         (BARF "no seconds supplied"))

     ;; Now apply lots of defaults.

     ;; There are many terms, from the lowest order (seconds) to the highest
     ;; order (years).  A legal date must specify some contiguous subsequence
     ;; of these.  The low unspecified ones get zeroed; the high unspecified
     ;; ones are either the next in the future or the previous in the past.
     ;; Time zones and days of the week are handled specially.

     ;; First, the following code allows a day of the week to be used to
     ;; specify a year, month, and date, when it is supposed to.
     (IF (AND (NULL *ABS-YEAR*)
              (NULL *ABS-MONTH*)
              (NULL *ABS-DATE*)
              (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
         ;; Day of week specified the year, month, and date.
         (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)))
           (MULTIPLE-VALUE-BIND (NIL NIL NIL NIL NIL NIL BASE-DAY-OF-THE-WEEK)
               (DECODE-UNIVERSAL-TIME UT)
             (LET ((DELTA-DAYS (- *ABS-DAY-OF-THE-WEEK* BASE-DAY-OF-THE-WEEK)))
               (IF FUTUREP
                   (DO () ((> DELTA-DAYS 0))
                     (SETQ DELTA-DAYS (+ DELTA-DAYS 7)))
                   (DO () ((< DELTA-DAYS 0))
                     (SETQ DELTA-DAYS (- DELTA-DAYS 7))))
               (MULTIPLE-VALUE (NIL NIL NIL *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
                 (COMPUTE-RELATIVE 0 0 0 (+ *BASE-DATE* DELTA-DAYS)
                                   *BASE-MONTH* *BASE-YEAR*))))))

     ;; If everything was specified (as in a date read from a file server)
     ;; then skip worrying about defaulting.
     (OR (AND *ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*)
       ;; Non-specified low-order terms get set to zero (or the moral equivalent
       ;; of zero), up to the first speicified term.
       (DO ((TERMS '(*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR*
                                  *ABS-DATE* *ABS-MONTH* *ABS-YEAR*) (CDR TERMS))
            (BASE-TERMS '(*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR*
                                        *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
                        (CDR BASE-TERMS))
            (LOWEST '(0 0 0 1 1 -100000000) (CDR LOWEST))
            (HIGHEST '(59. 59. 23. NIL 12. 100000000) (CDR HIGHEST))
            (STATE 'DEFAULT-LOW-TERMS)
            (COMPARISON 'EQUAL)
            (OPERATION NIL))
           ((NULL TERMS)
            (IF (EQ STATE 'DEFAULT-LOW-TERMS)
                (BARF "No time was specified.")))
         RESTART
         (LET ((TERM-VALUE (SYMEVAL (CAR TERMS)))
               (BASE-TERM-VALUE (SYMEVAL (CAR BASE-TERMS))))
           (SELECTQ STATE
             (DEFAULT-LOW-TERMS
              ;; Non-specified low-order terms get set to default values, which
              ;; are zero or one depending on whether the quantity is zero-based
              ;; or one-based.
              (COND ((NULL TERM-VALUE)
                     ;; Term is non-specified, default it.
                     (SET (CAR TERMS) (CAR LOWEST)))
                    (T
                     ;; Term is specified: go to the next state and try again.
                     (SETQ STATE 'SKIP-OVER-SPECIFIED)
                     (GO RESTART))))
             (SKIP-OVER-SPECIFIED
              ;; Now we are moving over the contiguous subsequence of values
              ;; specified by the user.
              (COND ((NOT (NULL TERM-VALUE))
                     ;; This value was specified by the user.
                     (COND ((> TERM-VALUE BASE-TERM-VALUE)
                            ;; Specified time is later than the base time.
                            (SETQ COMPARISON 'LATER))
                           ((< TERM-VALUE BASE-TERM-VALUE)
                            ;; Specified time is earlier than the base time.
                            (SETQ COMPARISON 'EARLIER))
                           ;; If these terms are equal, use the old value of
                           ;;   COMPARISON based on the lower order terms.
                           ))
                    (T
                     ;; Term is not specified; go to the next state and try again.
                     ;; This SETQ is documented at the next state.
                     (SETQ OPERATION
                           (SELECTQ COMPARISON
                             (EQUAL
                              ;; The specified and base times are equal, do nothing.
                              'EQUAL)
                             (LATER
                              ;; Specified time is later than base time.
                              (IF FUTUREP 'EQUAL 'SUB1))
                             (EARLIER
                              ;; Specified time is earlier than base time.
                              (IF FUTUREP 'ADD1 'EQUAL))))
                     (SETQ STATE 'DEFAULT-HIGH-TERMS)
                     (GO RESTART))))
             (DEFAULT-HIGH-TERMS
              ;; Non-specified high-order terms come from the base time.  The
              ;; tricky thing is that we may have to add or subtract one, depending
              ;; on FUTUREP and COMPARISON, which requires propagating carry or
              ;; borrow.  This information is encoded in OPERATION, which is SETQed
              ;; above (so that we don't do it each time around the loop!).
              (IF (NOT (NULL TERM-VALUE))
                  ;; Foo, the rest of the high-order terms have to be unspecified.
                  (BARF "Unrecognized pattern of defaulting."))
              (SELECTQ OPERATION
                (EQUAL
                 ;; We are just copying base time into abs time.  Keep doing it.
                 (SET (CAR TERMS) BASE-TERM-VALUE))
                (ADD1
                 ;; Set this term one higher than it is in the base time.
                 (LET ((HIGHEST-VALUE
                         ;; Compute the highest legal value for this term.
                         (IF (EQ (CAR TERMS) '*ABS-DATE*)
                             ;; Highest possible value for dates depends on
                             ;; which month this is.
                             (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*)
                             ;; Other highest values are just constants.
                             (CAR HIGHEST))))
                   (COND ((< BASE-TERM-VALUE HIGHEST-VALUE)
                          ;; No carry.  Just add one, and copy the rest.
                          (SET (CAR TERMS) (1+ BASE-TERM-VALUE))
                          (SETQ OPERATION 'EQUAL))
                         (T
                          ;; Carry into next term.
                          (SET (CAR TERMS) (CAR LOWEST))))))
                (SUB1
                 ;; Set this term one lower than it is in the base time.
                 (COND ((> BASE-TERM-VALUE (CAR LOWEST))
                        ;; No borrow.  Just subtract one, and copy the rest.
                        (SET (CAR TERMS) (1- BASE-TERM-VALUE))
                        (SETQ OPERATION 'EQUAL))
                       (T
                        ;; Borrow from the next term.
                        (SET (CAR TERMS)
                             (IF (EQ (CAR TERMS) '*ABS-DATE*)
                                 ;; Highest possible value for dates depends on
                                 ;; which month this is.
                                 (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*)
                                 ;; Other highest values are just constants.
                                 (CAR HIGHEST))))))
                (OTHERWISE
                 (FERROR NIL "Bad value of OPERATION ~S" OPERATION))))
             (OTHERWISE
              (FERROR NIL "Bad value of STATE ~S" STATE))))))

     ;; Now hack other random defaults.
;        (IF (NULL *ABS-TIME-ZONE*)
;            (SETQ *ABS-TIME-ZONE* *TIMEZONE*))
;        (SETQ *REL-TIME-ZONE* *ABS-TIME-ZONE*)

     ;; Check ranges.
     (CHECK-RANGE *ABS-SECOND* 0 59. "seconds in a minute")
     (CHECK-RANGE *ABS-MINUTE* 0 59. "minutes in an hour")
     (CHECK-RANGE *ABS-HOUR* 0 23. "hours in a day")
                                            ;Check this before MONTH-STRING call!
     (CHECK-RANGE *ABS-MONTH* 1 12. "months in a year")
     (CHECK-RANGE *ABS-DATE*
                  1
                  (MONTH-LENGTH *ABS-MONTH* *ABS-YEAR*)
                  (FORMAT NIL "days in ~A" (MONTH-STRING *ABS-MONTH*)))
     (IF (AND DAY-MUST-BE-VALID (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
         (VERIFY-DATE *ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-DAY-OF-THE-WEEK*))

     ;; Now put it together.
     (MULTIPLE-VALUE (*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
       (COMPUTE-RELATIVE (+ *ABS-SECOND* *REL-SECOND*)
                         (+ *ABS-MINUTE* *REL-MINUTE*)
                         (+ *ABS-HOUR* *REL-HOUR*)
                         (+ *ABS-DATE* *REL-DATE*)
                         (+ *ABS-MONTH* *REL-MONTH*)
                         (+ *ABS-YEAR* *REL-YEAR*)))
     (RETURN
       (values
         (ENCODE-UNIVERSAL-TIME *ABS-SECOND* *ABS-MINUTE* *ABS-HOUR*
                                *ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-TIME-ZONE*)
         *RELATIVE-P*)))))

;;; This function will accept dates such as -1,March 1980 and return 28,Febuary 1980
;;; CAL 10/24/80

(DEFUN COMPUTE-RELATIVE (SECOND MINUTE HOUR DATE MONTH YEAR)
  (PROG (M)
    (SETQ SECOND (+ SECOND (* 60 (+ MINUTE (* 60 (+ HOUR (* 24 DATE)))))))
    (SETQ DATE (FLOOR SECOND 86400))
    (SETQ SECOND (- SECOND (* DATE 86400)))
    (SETQ HOUR (FLOOR SECOND 3600))
    (SETQ SECOND (\ (FIX SECOND) 3600))
    (SETQ MINUTE (FLOOR SECOND 60))
    (SETQ SECOND (\ SECOND 60))
    (SETQ YEAR (+ YEAR (FLOOR (1- MONTH) 12)))
    (SETQ MONTH (1+ (\ (+ 12 (\ (1- MONTH) 12)) 12)))
 L1 (SETQ M (MONTH-LENGTH MONTH YEAR))
    (COND ((> DATE M)
           (SETQ DATE (- DATE M))
           (SETQ MONTH (1+ MONTH))
           (COND ((> MONTH 12) (SETQ MONTH 1) (SETQ YEAR (1+ YEAR))))
           (GO L1))
          ((< DATE 1)
           (SETQ MONTH (1- MONTH))
           (COND ((= MONTH 0) (SETQ MONTH 12) (SETQ YEAR (1- YEAR))))
           (SETQ DATE (+ (MONTH-LENGTH MONTH YEAR) DATE))
           (GO L1)))
    (RETURN (values SECOND MINUTE HOUR DATE MONTH YEAR))))


(DEFUN PARSE-TWENEX-TIME (STRING START END)
  "If STRING (between START and END) is a Twenex file server format date,
return the universal time for it.  Otherwise, return NIL."
  (PROG (IDX YEAR MONTH DATE HOUR MINUTE SECOND SUBSTRING)
        (IF (AND (> END START) (= (AREF STRING START) #/SPACE))
            (INCF START))
        (SETQ IDX (STRING-SEARCH #/- STRING START END))
        (OR IDX (RETURN NIL))
        (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END))
            (RETURN NIL))
        (SETQ DATE (PARSE-NUMBER STRING START IDX))
        (SETQ START (1+ IDX))
        ;; Now the month name.
        (SETQ IDX  (STRING-SEARCH #/- STRING START END))
        (SETQ SUBSTRING (SUBSTRING STRING START IDX))
        (OR (EQ IDX (STRING-SEARCH-SET "0123456789" SUBSTRING))
            (RETURN NIL))
        (SETQ MONTH (1+ (FIND-POSITION-IN-LIST
                          (ASS 'EQUALP SUBSTRING *MONTH-SYMBOLS*)
                          *MONTH-SYMBOLS*)))
        (OR MONTH (RETURN NIL))
        (SETQ START (1+ IDX))
        ;; Now the year.
        (SETQ IDX (STRING-SEARCH #/SPACE STRING START END))
        (OR IDX (RETURN NIL))
        (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END))
            (RETURN NIL))
        (SETQ YEAR (PARSE-NUMBER STRING START IDX))
        (SETQ START (1+ IDX))
        ;; Now the hour
        (SETQ IDX (STRING-SEARCH #/: STRING START END))
        (OR IDX (RETURN NIL))
        (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END))
            (RETURN NIL))
        (SETQ HOUR (PARSE-NUMBER STRING START IDX))
        (SETQ START (1+ IDX))
        ;; Now the minute
        (SETQ IDX (STRING-SEARCH #/: STRING START END))
        (OR IDX (RETURN NIL))
        (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END))
            (RETURN NIL))
        (SETQ MINUTE (PARSE-NUMBER STRING START IDX))
        (SETQ START (1+ IDX))
        ;; Now the second
        (OR (EQ END
                (OR (STRING-SEARCH-NOT-SET "0123456789" STRING START END)
                    END))
            (RETURN NIL))
        (SETQ SECOND (PARSE-NUMBER STRING START END))
        (RETURN (ENCODE-UNIVERSAL-TIME SECOND MINUTE HOUR DATE MONTH YEAR
                                       *TIMEZONE*))))

(DEFUN FIND-BIRTHDAY (STRING &AUX X)
  (SETQ X (STRING-SEARCH "birthday" STRING))
  (IF (NULL X) (BARF "Cannot find /"BIRTHDAY/"."))
  (SUBSTRING STRING (+ 9 X) (STRING-SEARCH ";" STRING (+ 9 X))))

(DEFPROP BARF T :ERROR-REPORTER)
(DEFPROP BARF T :ERROR-REPORTER)
(DEFUN BARF (STRING &REST ARGS)
  (LEXPR-FUNCALL 'FERROR 'PARSE-ERROR STRING ARGS))

(DEFUN TEST ()
  (DO ((S (READLINE) (READLINE))
       (NOW (GET-UNIVERSAL-TIME)))
      ((EQUAL S ""))
    (CONDITION-CASE (VAL RELATIVE-P)
        (PARSE-UNIVERSAL-TIME S 0 NIL T NOW)
      (ERROR (PRINC VAL))
      (:NO-ERROR
        (FORMAT T "~15A" (OR RELATIVE-P "Absolute"))
        (PRINT-UNIVERSAL-TIME VAL)))
    (TERPRI) (TERPRI)))

;;; This function should be run whenever you make a major change to the
;;; parser.  It has an exhaustive set of test cases, all of which should
;;; be verified.
(DEFCONST *TEST-CASES*
        '("March 15, 1960" "15 March 1960" "3//15//60" "15//3//60" "3//15//1960"
          "3-15-60" "15-3-1960" "3-15" "3-March-60" "3-Mar-60" "March-3-60"
          "1130." "11:30" "11:30:17" "11:30 pm" "11:30 AM" "1130" "113000"
          "11.30" "11.30.00" "11.3" "11 pm" "12 noon"
          "midnight" "m" "Friday, March 15, 1980" "6:00 gmt" "3:00 pdt"
          "15 March 60" "15 march 60 seconds"
          "Fifteen March 60" "The Fifteenth of March, 1960;"
          "Thursday, 21 May 1981, 00:27-EDT"
          "One minute after March 3, 1960"
          "Three days ago" "5 hours ago"
          "Two days after March 3, 1960"
          "Three minutes after 23:59:59 Dec 31, 1959"
          "Now" "Today" "Yesterday" "two days after tomorrow"
          "one day before yesterday" "the day after tomorrow"
          "half past noon"
          "half a minute past noon"
          "20 past noon"
          "a quarter of an hour from now"
          "2.5 days from now"
          "2.5 hours after tomorrow"
          ".5 days from now"
          "2 and a half days from now"
          "2 hours and 20 minutes from tomorrow"
          "5h3m from tomorrow"
          ;; Leave these last in case server is down!
          "my birthday" "the day before my birthday"
          "1 hour before dlw's birthday"
          )
  "A list of test cases which should be used anytime a major change is made to the parser.")

(DEFUN TEST-PARSER ()
  (TERPRI)
  (DOLIST (CASE *TEST-CASES*)
    (FORMAT T "~40A   " CASE)
    (CONDITION-CASE (VAL RELATIVE-P)
        (PARSE-UNIVERSAL-TIME CASE)
      (ERROR (PRINC VAL))
      (:NO-ERROR
        (FORMAT T "~15A" (OR RELATIVE-P "Absolute"))
        (PRINT-UNIVERSAL-TIME VAL)))
    (TERPRI)))

;;; Time interval stuff.
(DEFVAR TIME-INTERVAL-ARRAY (MAKE-ARRAY '(50. 2.)))

(DEFVAR TIME-INTERVAL-UNIT-TYPES 0)

(DEFUN TIME-INTERVAL-TO-SECONDS (STRING &AUX (TOTAL 0))
  "Return a number of seconds parsed from STRING.
If the string cannot be parsed, the first value is NIL
and the second is a string describing the problem."
  (IF (NUMBERP STRING) STRING
    (DO ((IX 0)
         (L (STRING-LENGTH STRING)))
        ((OR (NULL IX) ( IX L)) TOTAL)
      (LET ((TOKEN-START (STRING-SEARCH-NOT-CHAR #/SPACE STRING IX)))
        (IF (NULL TOKEN-START) (RETURN TOTAL))
        (LET* ((TOKEN-END (STRING-SEARCH-CHAR #/SPACE STRING TOKEN-START))
               ;;works even if end nil!
               (UNITS (ZWEI:PARSE-NUMBER STRING TOKEN-START TOKEN-END)))
          (IF (NULL UNITS)
              (RETURN
                (values
                  NIL
                  (FORMAT
                    NIL "Invalid number: ~A" (SUBSTRING STRING TOKEN-START TOKEN-END)))))
          (LET ((TOKEN-START (STRING-SEARCH-NOT-CHAR #/SPACE STRING TOKEN-END)))
            (IF (NULL TOKEN-START)
                (RETURN (values NIL "Units specification missing from time string")))
            (SETQ IX (STRING-SEARCH-CHAR #/SPACE STRING TOKEN-START))
            (LET ((UVAL (LOOP FOR I FROM 0 BELOW TIME-INTERVAL-UNIT-TYPES
                              FINALLY (RETURN NIL)
                              DO
                              (IF (STRING-EQUAL
                                    (AREF TIME-INTERVAL-ARRAY I 0) STRING
                                    :start1 0 :start2 TOKEN-START :end1 NIL :end2 IX)
                                  (RETURN (AREF TIME-INTERVAL-ARRAY I 1))))))
              (IF UVAL
                  (PROGN
                    (IF (CHAR-EQUAL #/y (AREF STRING TOKEN-START))      ;years?
                        (IF (> UNITS 3)         ;good till 1999.
                            (INCF TOTAL (* (FLOOR UNITS 4)
                                           (TIME-INTERVAL-TO-SECONDS "1 day")))))
                    (INCF TOTAL (* UVAL UNITS)))
                (RETURN (values
                          NIL (FORMAT NIL "Unknown time spec: ~A"
                                      (SUBSTRING STRING TOKEN-START IX))))))))))))

(DEFUN INIT-TIME-INTERVAL-ARRAY ()
  (SETF (AREF TIME-INTERVAL-ARRAY 0 0) "second")
  (SETF (AREF TIME-INTERVAL-ARRAY 0 1) 1)
  (SETQ TIME-INTERVAL-UNIT-TYPES 1)
  (DOLIST (L '(("1 second" "seconds" "s" "sec" "secs")
               ("60 seconds" "minute" "minutes" "min" "mins" "m")
               ("60 minutes" "hour" "hours" "hr" "hrs" "h")
               ("24 hours" "day" "days")
               ("7 days" "week" "weeks" "wk" "wks")
               ("365 days" "year" "years" "yr" "yrs")))
    (LET ((VALUE (TIME-INTERVAL-TO-SECONDS (CAR L))))
      (DOLIST (NEWNAME (CDR L))
        (SETF (AREF TIME-INTERVAL-ARRAY TIME-INTERVAL-UNIT-TYPES 0) NEWNAME)
        (SETF (AREF TIME-INTERVAL-ARRAY TIME-INTERVAL-UNIT-TYPES 1) VALUE)
        (INCF TIME-INTERVAL-UNIT-TYPES)))))

(INIT-TIME-INTERVAL-ARRAY)

(DEFUN SECONDS-TO-INTERVAL-STRING (SECS)
  "Return a string describing a time interval SECS in seconds."
  (IF (ZEROP SECS)
      "0 seconds"
    (DO ((I 0 (1+ I))
           (LAST NIL))
        (( I TIME-INTERVAL-UNIT-TYPES)
         (SECONDS-TO-INTERVAL-STRING-1 LAST SECS))
      (IF (> (AREF TIME-INTERVAL-ARRAY I 1) SECS)
          (RETURN (SECONDS-TO-INTERVAL-STRING-1 LAST SECS))
        (IF (OR (NULL LAST)
                (NOT (= (AREF TIME-INTERVAL-ARRAY I 1)
                        (AREF TIME-INTERVAL-ARRAY LAST 1))))
            (SETQ LAST I))))))

(DEFVAR *FOUR-YEAR-CYCLE* (TIME-INTERVAL-TO-SECONDS "4 Years"))
(DEFVAR *SECONDS-IN-DAY* (TIME-INTERVAL-TO-SECONDS "1 day") "The number of seconds in a day.")

(DEFUN SECONDS-TO-INTERVAL-STRING-1 (INDEX SECS)
  (IF (NOT (ZEROP (FLOOR SECS *FOUR-YEAR-CYCLE*)))
      (DECF SECS (* (FLOOR SECS *FOUR-YEAR-CYCLE*) *SECONDS-IN-DAY*)))
  (LET ((QUO (FLOOR SECS (AREF TIME-INTERVAL-ARRAY INDEX 1)))
        (REM (\ SECS (AREF TIME-INTERVAL-ARRAY INDEX 1))))
    (IF (ZEROP REM)
        (FORMAT NIL "~D ~A~P" QUO (AREF TIME-INTERVAL-ARRAY INDEX 0) QUO)
        (FORMAT NIL "~D ~A~P ~A" QUO (AREF TIME-INTERVAL-ARRAY INDEX 0) QUO
                (SECONDS-TO-INTERVAL-STRING REM)))))

(DEFPROP :TIME-INTERVAL-OR-NEVER (PRINT-INTERVAL-OR-NEVER READ-INTERVAL-OR-NEVER
                                  NIL NIL NIL
                                  "Click left to input a time interval or /"never/".")
         TV:CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN PARSE-INTERVAL-OR-NEVER (STRING &OPTIONAL FROM TO)
  "Parse a string either describing a time interval or /"never/".
For a time interval, the number of seconds is returned.
For /"never/" or variations, NIL is returned."
  (IF (NUMBERP STRING) STRING
    (SETQ STRING (STRING-TRIM '(#/SPACE #/TAB)
                              (IF (NULL (OR FROM TO))
                                  STRING
                                (SUBSTRING STRING FROM TO))))
    (IF (MEM 'EQUALP STRING '("none" "no" "" "never" "not ever" "nil" "()"))
        NIL
      (MULTIPLE-VALUE-BIND (VAL ERR)
          (TIME-INTERVAL-TO-SECONDS STRING)
        (IF ERR
            (FERROR NIL "~A: ~A" STRING ERR)
          VAL)))))

(DEFUN READ-INTERVAL-OR-NEVER (&OPTIONAL (STREAM STANDARD-INPUT))
  "Read a line from STREAM and parse into time interval or NIL for never."
  (PARSE-INTERVAL-OR-NEVER (READLINE STREAM)))

(DEFUN PRINT-INTERVAL-OR-NEVER (VAL &OPTIONAL (STREAM T))
  "Print the interval-or-never VAL on STREAM.
VAL can be a number of seconds, or NIL for never."
  (COND ((NULL VAL)
         (FORMAT STREAM "Never"))
        ((MINUSP VAL)
         (FORMAT STREAM "NEGATIVE?? ~a" (seconds-to-interval-string (minus val))))
        (t
         (FORMAT STREAM "~a" (SECONDS-TO-INTERVAL-STRING VAL)))))

;;; Now that the time parser is loaded, we can fix up times remembered as strings by
;;; the system generator.
(ADD-INITIALIZATION "TIME-PARSER-LOADED" '(FS:CANONICALIZE-COLD-LOADED-TIMES) '(:ONCE))

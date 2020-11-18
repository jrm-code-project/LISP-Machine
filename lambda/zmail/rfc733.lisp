; -*- Mode:LISP; Package:ZWEI; Base:8 ; Readtable:ZL -*-
;;;
;;; RFC733 address parser
;;;

(DEFMACRO PARSE-ERROR (FORMAT-STRING &REST ARGS)
  `(THROW 'PARSE-ERROR ,(IF ARGS `(FORMAT NIL ,FORMAT-STRING . ,ARGS) FORMAT-STRING)))

;;; Lexer routines, fsm defined in ZMAIL; LEX733
(DEFVAR RFC733)

(DEFPROP ATOM LEX-SUBSTRING RFC733)
(DEFPROP AT-ATOM LEX-SUBSTRING RFC733)
(DEFPROP QUOTED-STRING LEX-QUOTED-STRING RFC733)

(DEFUN (COMMENT RFC733) (TYPE RDTBL START-STRING START-INDEX END-STRING END-INDEX
                         &AUX STRING INDEX)
  (MULTIPLE-VALUE (NIL NIL STRING INDEX)
    (LEXEME-TYI RDTBL START-STRING START-INDEX END-STRING END-INDEX))
  (DO ((TEM)) (NIL)
    (MULTIPLE-VALUE (TEM STRING INDEX)
      (READ-LEXEME RDTBL STRING INDEX END-STRING END-INDEX :RECURSIVE T))
    (CASE (CAR TEM)
      (CLOSE (RETURN nil))
      (EOF (PARSE-ERROR "EOF in the middle of a comment"))))
  (LEX-SUBSTRING TYPE RDTBL START-STRING START-INDEX STRING INDEX))

(DEFUN RFC733-LEXER (STRING &OPTIONAL (START 0) END (ERROR-P T))
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (RDTBL-LEXER RFC733 STRING START STRING END ERROR-P))

;;; FSM readtable lexer support

;;; This returns the next lexeme from the string
;;; Backslash-p is used for reading the insides of comments.
;;; It causes a backslash to start a two-character lexeme.
(DEFUN READ-LEXEME (RDTBL START-STRING START-INDEX END-STRING END-INDEX ERROR-P
                    &OPTIONAL BACKSLASH-P
                    &AUX CH CODE STATE FSM PROPNAME STRING INDEX VALUE ERRMES)
  (SETQ PROPNAME (SI:RDTBL-READ-FUNCTION-PROPERTY RDTBL)
        FSM (SI:RDTBL-FSM RDTBL)
        STATE (SI:RDTBL-STARTING-STATE RDTBL)
        STRING START-STRING
        INDEX START-INDEX)
  (flet ((check-backslash-end (ch)
           ;; If we hit the end inside a blackslash state, then it's a definite error.
           (when (null ch) (parse-error "Unexpected end inside a comment or literal area."))))
    (SETQ ERRMES
          (CATCH 'PARSE-ERROR
            (MULTIPLE-VALUE (CH CODE STRING INDEX)
              (LEXEME-WHITE-TYI RDTBL STRING INDEX END-STRING END-INDEX))
            (SETQ STATE (AREF FSM STATE CODE))
            (when backslash-p
              ;; If we hit the end inside a blackslash state, then it's a definite error.
              (check-backslash-end ch)
              (when (char= ch #/\)
                (setf (values ch nil string index)
                      (lexeme-tyi rdtbl string index end-string end-index))
                (setq state '(backslashed))))
            (IF (NOT (NUMBERP STATE))
                (LET ((FLAG (CAR STATE))
                      (ACTION (CDR STATE)))
                  (CASE FLAG
                    (SINGLE
                     (SETQ VALUE (LIST ACTION CH (LIST STRING INDEX))))
                    (BACKSLASHED (SETQ VALUE (LIST 'BACKSLASH CH (LIST STRING INDEX))))
                    (START
                     (MULTIPLE-VALUE (STRING INDEX) (LEXEME-UNTYI CH STRING INDEX))
                     (MULTIPLE-VALUE (VALUE STRING INDEX)
                       (FUNCALL (GET ACTION PROPNAME) ACTION RDTBL STRING INDEX
                                END-STRING END-INDEX)))
                    (OTHERWISE
                     (PARSE-ERROR "~S found in the fsm" FLAG))))
              (MULTIPLE-VALUE (START-STRING START-INDEX) (LEXEME-UNTYI CH STRING INDEX))
              (DO-FOREVER
                (MULTIPLE-VALUE (CH CODE STRING INDEX)
                  (LEXEME-TYI RDTBL STRING INDEX END-STRING END-INDEX))
                (SETQ STATE (AREF FSM STATE CODE))
                (when backslash-p
                  (check-backslash-end ch)
                  (when (char= ch #/\)
                    (setq state '(untyi . atom))))
                (AND (NOT (NUMBERP STATE))
                     (LET ((FLAG (CAR STATE))
                           (ACTION (CDR STATE)))
                       (CASE FLAG
                         (UNTYI
                          (AND CH
                               (MULTIPLE-VALUE (STRING INDEX) (LEXEME-UNTYI CH STRING INDEX))))
                         (OTHERWISE
                          (PARSE-ERROR "~S found in the fsm" FLAG)))
                       (SETQ VALUE (FUNCALL (GET ACTION PROPNAME) ACTION RDTBL
                                            START-STRING START-INDEX STRING INDEX))
                       (RETURN nil)))))
            NIL))
    (AND ERRMES ERROR-P
         (IF (EQ ERROR-P :RECURSIVE)
             (PARSE-ERROR ERRMES)
           (ZMAIL-ERROR "~A" ERRMES)))
    (VALUES VALUE STRING INDEX ERRMES)))

(DEFUN LEXEME-WHITE-TYI (RDTBL STRING INDEX END-STRING END-INDEX &AUX CH CODE)
  (DO-FOREVER
    (MULTIPLE-VALUE (CH CODE STRING INDEX)
      (LEXEME-TYI RDTBL STRING INDEX END-STRING END-INDEX))
    (AND (OR (NULL CH)
             (NOT (BIT-TEST 1 (SI:RDTBL-BITS RDTBL CH))))
         (RETURN (values CH CODE STRING INDEX)))))

(DEFCONST *LEXEME-RDTBL-MAX* (1- (SECOND (SI:RDTBL-ARRAY-DIMS))))

;;; This will work with ZWEI lines over multiple line regions or with regular strings
;;; since the EOF check will happen before the LINE-NEXT attempt.
(DEFUN LEXEME-TYI (RDTBL STRING INDEX END-STRING END-INDEX)
  (DECLARE (RETURN-LIST CH CODE STRING INDEX))
  (COND ((AND (EQ STRING END-STRING) (= INDEX END-INDEX))
         (VALUES NIL (SI:RDTBL-EOF-CODE RDTBL) STRING INDEX))
        ((= INDEX (ARRAY-ACTIVE-LENGTH STRING))
         (VALUES #/CR (SI:RDTBL-CODE RDTBL #/CR) (LINE-NEXT STRING) 0))
        (T
         (LET ((CH (MIN (AREF STRING INDEX) *LEXEME-RDTBL-MAX*)))
           (VALUES CH (SI:RDTBL-CODE RDTBL CH) STRING (1+ INDEX))))))

(DEFUN LEXEME-UNTYI (IGNORE STRING INDEX)
  (IF (ZEROP INDEX)
      (LET ((LINE (LINE-PREVIOUS STRING)))
        (VALUES LINE (ARRAY-ACTIVE-LENGTH LINE)))
    (VALUES STRING (1- INDEX))))

(DEFUN LEXEME-SLASH-TYI (RDTBL STRING INDEX END-STRING END-INDEX &AUX CH CODE)
  (MULTIPLE-VALUE (CH CODE STRING INDEX)
    (LEXEME-TYI RDTBL STRING INDEX END-STRING END-INDEX))
  (IF (OR (NULL CH) (NOT (BIT-TEST 2 (SI:RDTBL-BITS RDTBL CH))))
      (VALUES CH CODE STRING INDEX)
    (MULTIPLE-VALUE (CH NIL STRING INDEX)
      (LEXEME-TYI RDTBL STRING INDEX END-STRING END-INDEX))
    (AND (NULL CH) (PARSE-ERROR "EOF after slash"))
    (VALUES CH (SI:RDTBL-SLASH-CODE RDTBL) STRING INDEX)))

(DEFUN RDTBL-LEXER (RDTBL START-STRING START-INDEX END-STRING END-INDEX ERROR-P)
  (LOOP WITH STRING = START-STRING
        AND INDEX = START-INDEX
        AND TEM AND ERRMES
        DO (MULTIPLE-VALUE (TEM STRING INDEX ERRMES)
             (READ-LEXEME RDTBL STRING INDEX END-STRING END-INDEX ERROR-P))
        WHEN ERRMES
        DO (RETURN ERRMES)
        COLLECT TEM
        UNTIL (EQ (CAR TEM) 'EOF)))

;;; Some useful read functions
(DEFUN LEX-SUBSTRING (TYPE IGNORE START-STRING START-INDEX END-STRING END-INDEX)
  (VALUES (LET ((START (LIST START-STRING START-INDEX))
                (END (LIST END-STRING END-INDEX)))
            (LIST TYPE (STRING-INTERVAL START END T) START END))
          END-STRING
          END-INDEX))

(DEFUN LEX-QUOTED-STRING (TYPE RDTBL START-STRING START-INDEX END-STRING END-INDEX
                          &AUX MATCH STRING INDEX)
  (MULTIPLE-VALUE (MATCH NIL STRING INDEX)
    (LEXEME-TYI RDTBL START-STRING START-INDEX END-STRING END-INDEX))
  (DO ((SLASH (SI:RDTBL-SLASH-CODE RDTBL))
       (CH) (CODE))
      (NIL)
    (MULTIPLE-VALUE (CH CODE STRING INDEX)
      (LEXEME-SLASH-TYI RDTBL STRING INDEX END-STRING END-INDEX))
    (AND (NULL CH) (PARSE-ERROR "EOF in the middle of a string"))
    (AND (CHAR= CH MATCH) (NEQ CODE SLASH)
         (RETURN (SETQ END-STRING STRING END-INDEX INDEX))))
  (LEX-SUBSTRING TYPE RDTBL START-STRING START-INDEX END-STRING END-INDEX))


;;; Parsing grammar itself
;;; An interval is parsed into a list of ADDRESS'es.  Each address is a plist.
;;; The indicators are:
;;; :NAME (string) - the mailbox at the particular site.
;;; :HOST (string-list) - path to the site.
;;; :PERSONAL-NAME (string) - a string that might possibly be the user's real name.
;;; :INTERVAL (`((,start-string ,start-index) (,end-string ,end-index))) - parsed area
;;;           [Note those are really bp's.]
;;; :DISTRIBUTION-LIST (string) - for Foo: mumble ;
;;; :BRACKETED-LIST (string) - for Foo <mumble, frotz> with more than 1 address
;;; :INFERIORS (addresses) - for bracketing type headers like above
;;; :POSTAL (string) - for :postal:
;;; :INCLUDE (string) - likewise for :include:
(defsignal parsing-error (ferror) ())

(DEFUN PARSE-ADDRESSES-INTERVAL (START-BP &OPTIONAL END-BP IN-ORDER-P &AUX LEXEMES)
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (SETQ LEXEMES (RDTBL-LEXER RFC733 (BP-LINE START-BP) (BP-INDEX START-BP)
                             (BP-LINE END-BP) (BP-INDEX END-BP) NIL))
  (IF (STRINGP LEXEMES)                         ;An error
      LEXEMES
    (CONDITION-CASE (ERROR)
        (READ-ADDRESSES-TOP-LEVEL LEXEMES)
      (PARSING-ERROR
       (SEND ERROR :REPORT-STRING)))))

(DEFUN PARSE-ADDRESSES (STRING &OPTIONAL (START 0) END &AUX LEXEMES)
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (SETQ LEXEMES (RDTBL-LEXER RFC733 STRING START STRING END NIL))
  (IF (STRINGP LEXEMES)                         ;An error
      LEXEMES
    (CONDITION-CASE (ERROR)
        (READ-ADDRESSES-TOP-LEVEL LEXEMES)
      (PARSING-ERROR
       (SEND ERROR :REPORT-STRING)))))

(DEFMACRO DISCARD-COMMENTS ()
  `(DO () ((NEQ (CAAR LEXEMES) 'COMMENT))
     (POP LEXEMES)))

(defun parsing-error (format-string &rest format-args)
  (declare (eh:error-reporter))
  (apply #'ferror 'parsing-error format-string format-args))

(DEFUN READ-ADDRESSES-TOP-LEVEL (LEXEMES)
  (MULTIPLE-VALUE-BIND (ADDRESSES LEXEMES-LEFT)
      (READ-ADDRESSES LEXEMES)
    (UNLESS (EQ (CAAR LEXEMES-LEFT) 'EOF)
      (PARSING-ERROR
              "Bad character /"~C/" at top level in address list."
              (SECOND (CAR LEXEMES))))
    ADDRESSES))

;;; An ADDRESSES is just a list of property lists.
(DEFUN READ-ADDRESSES (LEXEMES)
  (DO (ADDRESS ADDRESSES)
      ((MEMQ (CAAR LEXEMES) '(EOF RIGHT-BRACKET SEMI))
       (VALUES ADDRESSES LEXEMES))
    (MULTIPLE-VALUE (ADDRESS LEXEMES)
      (READ-ADDRESS LEXEMES))
    (SETQ ADDRESSES (NCONC ADDRESSES ADDRESS))
    (DISCARD-COMMENTS)
    (UNLESS (MEMQ (CAAR LEXEMES) '(EOF RIGHT-BRACKET SEMI COMMA))
      (ZMAIL-ERROR "Invalid terminator ~S for an ADDRESS." (CAR LEXEMES)))
    (IF (EQ (CAAR LEXEMES) 'COMMA)
        (POP LEXEMES))))

;;; Format is @<host0>,@<host1>,...@<hostn>:address
(defun gobble-routing-information (lexemes)
  (if (eq (caar lexemes) 'atsign)
      (let ((lexemes (cdr lexemes))
            lexeme)
        (flet ((pop-token ()
                 (when (null lexemes)
                   (parsing-error "Routing information (@...:) ended suddenly."))
                 (pop lexemes lexeme)
                 (car lexeme)))
          (loop
            (unless (eq (pop-token) 'atom)
              (parsing-error "Expected host name in routing information."))
            (case (pop-token)
              (comma) ; drop through
              (colon (return-from gobble-routing-information lexemes))
              (otherwise (parsing-error "Routing information ended illegally")))
            (unless (eq (pop-token) 'atsign)
              (parsing-error "Expected atsign in routing information.")))))
    lexemes))

;;; An ADDRESS looks just like an ADDRESSES,
;;; except that usually it is of length one.
(DEFUN READ-ADDRESS (LEXEMES)
  (declare (values address remaining-lexemes))
  (PROG (HOST-PHRASE)
        (LET ((LEXEMES LEXEMES)
               ADDRESS ATOM COLON)
          (DISCARD-COMMENTS)
          (WHEN (EQ (CAAR LEXEMES) 'COLON)
            ;; : atom : ...
            (POP LEXEMES)
            (DISCARD-COMMENTS)
            (UNLESS (EQ (CAAR LEXEMES) 'ATOM)
              (PARSING-ERROR "/"~A/" invalid after colon at start of address."
                      (SECOND (CAR LEXEMES))))
            (SETQ ATOM (POP LEXEMES))
            (DISCARD-COMMENTS)
            (UNLESS (EQ (CAAR LEXEMES) 'COLON)
              (PARSING-ERROR "No colon after :~A at start of address."
                      (SECOND ATOM)))
            (POP LEXEMES)
            (DISCARD-COMMENTS)
            (MULTIPLE-VALUE (ADDRESS LEXEMES)
              (READ-ADDRESS LEXEMES))
            (LET ((IND (INTERN (STRING-UPCASE (SECOND ATOM)) ""))
                  TEM)
              (DOLIST (ADD ADDRESS)
                (AND (SETQ TEM (GETL (LOCF ADD) '(:NAME)))
                     (SETF (CAR TEM) IND))))
            (AND (= (LENGTH ADDRESS) 1)
                 (LET ((INT (GET (LOCF (CAR ADDRESS)) :INTERVAL)))
                   (SETF (FIRST INT) (THIRD COLON))))
            (RETURN (values ADDRESS LEXEMES))))
        ;; >> Skip over hairy routing information.  Maybe ZMail could use this later.
        (setq lexemes (gobble-routing-information lexemes))
        ;; It is not : atom : address.
        (MULTIPLE-VALUE (HOST-PHRASE LEXEMES)
          (READ-HOST-PHRASE LEXEMES))
        ;; Treat a following comment as the personal name.
        (AND HOST-PHRASE
             (EQ (CAAR LEXEMES) 'COMMENT)
             (LET ((PLIST (LOCF HOST-PHRASE))
                   (COMMENT (POP LEXEMES)))
               (LET ((STRING (SECOND COMMENT)))
                 (PUTPROP PLIST (SUBSTRING STRING 1 (1- (STRING-LENGTH STRING))) :PERSONAL-NAME))
               ;; Include comment in interval
               (SETF (SECOND (GET PLIST :INTERVAL)) (FOURTH COMMENT))))
        ;; Discard any remaining comments.
        (DISCARD-COMMENTS)
        ;; If a colon follows, this is a distribution list.
        (WHEN (EQ (CAAR LEXEMES) 'COLON)
          (POP LEXEMES)
          ;; Read a list of addresses, and check for and discard the ";".
          (MULTIPLE-VALUE-BIND (ADDRESSES LEXEMES)
              (READ-ADDRESSES LEXEMES)
            (UNLESS (EQ (CAAR LEXEMES) 'SEMI)
              (PARSING-ERROR
                      "Distribution list terminated by /"~C/" rather that /";/"."
                      (SECOND (CAR LEXEMES))))
            (LET ((SEMI (CAR LEXEMES)))
              (AND HOST-PHRASE
                   (LET* ((ADDRESS NIL)
                          (PLIST (LOCF ADDRESS)))
                     (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :NAME) :DISTRIBUTION-LIST)
                     (SETF (SECOND (GET (LOCF HOST-PHRASE) :INTERVAL)) (THIRD SEMI))
                     (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :INTERVAL) :INTERVAL)
                     (PUTPROP PLIST ADDRESSES :INFERIORS)
                     (PUSH ADDRESS ADDRESSES)))
              (RETURN (values ADDRESSES (CDR LEXEMES))))))
        ;; If a "<" follows, the real thing is within the angle brackets.
        (WHEN (EQ (CAAR LEXEMES) 'LEFT-BRACKET)
          (POP LEXEMES)
          ;; Read a list of addresses, and check for and discard the ">".
          (MULTIPLE-VALUE-BIND (ADDRESSES LEXEMES)
              (READ-ADDRESSES LEXEMES)
            (UNLESS (EQ (CAAR LEXEMES) 'RIGHT-BRACKET)
              (PARSING-ERROR
                      "Bracketed list terminated by /"~C/" rather that /">/"."
                      (SECOND (CAR LEXEMES))))
            (LET ((RIGHT-BRACKET (CAR LEXEMES)))
              (AND HOST-PHRASE
                   (IF (= (LENGTH ADDRESSES) 1)
                       ;; For just one address, treat as Fred Foobar <FOO at MIT-AI>
                       (LET ((PLIST (LOCF (CAR ADDRESSES))))
                         (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :NAME) :PERSONAL-NAME)
                         ;; Include phrases and bracket in interval
                         (LET ((INTERVAL (GET PLIST :INTERVAL)))
                           (SETF (FIRST INTERVAL) (FIRST (GET (LOCF HOST-PHRASE) :INTERVAL)))
                           (SETF (SECOND INTERVAL) (THIRD RIGHT-BRACKET))))
                     ;; Otherwise treat as a list
                     (LET* ((ADDRESS NIL)
                            (PLIST (LOCF ADDRESS)))
                       (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :NAME) :BRACKETED-LIST)
                       (SETF (SECOND (GET (LOCF HOST-PHRASE) :INTERVAL)) (THIRD RIGHT-BRACKET))
                       (PUTPROP PLIST (GET (LOCF HOST-PHRASE) :INTERVAL) :INTERVAL)
                       (PUTPROP PLIST ADDRESSES :INFERIORS)
                       (PUSH ADDRESS ADDRESSES)))))
            ;; Discard the ">".
            (RETURN (values ADDRESSES (CDR LEXEMES)))))
        (RETURN (values (LIST HOST-PHRASE) LEXEMES))))

;;; A HOST-PHRASE looks like (:NAME name :HOST host :INTERVAL interval).
;;; We can also return NIL if we find none here.
;;; Like the lower levels, we discard leading and internal comments
;;; (unless we interpret them specially) but do not discard trailing ones.
(DEFUN READ-HOST-PHRASE (LEXEMES)
  (LET (HOST-INDICATOR PHRASE)
    ;; First see if we have a comment followed by an atsign, as in "(BUG LISPM) @ AI"
    (IF (AND (EQ (CAAR LEXEMES) 'COMMENT)
             (MEMQ (CAADR LEXEMES) '(ATSIGN AT-ATOM)))
        (LET (NAME INTERVAL)
          (SETF `(COMMENT ,NAME . ,INTERVAL)
                (POP LEXEMES))
          (AND (STRING-EQUAL NAME "(BUG " :END1 5 :END2 5)
               (SETQ NAME (STRING-APPEND (SUBSTRING NAME 1 4) "-"
                                         (SUBSTRING NAME 5 (1- (STRING-LENGTH NAME))))))
          (SETQ PHRASE `(,NAME . ,(COPYLIST INTERVAL))))
      (MULTIPLE-VALUE (PHRASE LEXEMES)
        (READ-PHRASE LEXEMES)))
    (IF (AND (NULL PHRASE)
             (NOT (MEMQ (CAAR LEXEMES) '(ATSIGN AT-ATOM))))
        (VALUES NIL LEXEMES)
      (UNLESS PHRASE
        (SETQ PHRASE (LIST "" (THIRD (CAR LEXEMES)) (THIRD (CAR LEXEMES)))))
      (MULTIPLE-VALUE (HOST-INDICATOR LEXEMES)
        (READ-HOST-INDICATOR LEXEMES))
      (AND HOST-INDICATOR (SETF (THIRD PHRASE) (THIRD HOST-INDICATOR)))
      (VALUES `(:NAME ,(CAR PHRASE) :HOST ,(CAR HOST-INDICATOR) :INTERVAL ,(CDR PHRASE))
              LEXEMES))))

;;; These hooks are for canonializing all the "@"'s and host names
(DEFVAR *HOST-INTERVALS*)
(DEFVAR *ACCUMULATE-HOST-INTERVALS* NIL)

;;; A HOST-INDICATOR is like a PHRASE except its first element is a host-path,
;;; a list of host names.
;;; This function ignores leading comments if it finds something,
;;; and ignores internal comments, but does not discard trailing comments.
(DEFUN READ-HOST-INDICATOR (LEXEMES)
  (DO (HOST-INDICATOR AT WORD UNUSED-LEXEMES)
      (())
    (SETQ UNUSED-LEXEMES LEXEMES)
    (DISCARD-COMMENTS)
    (UNLESS (MEMQ (CAAR LEXEMES) '(AT-ATOM ATSIGN))
      (RETURN (values HOST-INDICATOR UNUSED-LEXEMES)))
    (SETQ AT (POP LEXEMES))
    (MULTIPLE-VALUE (WORD LEXEMES)
      (READ-WORD LEXEMES))
    (UNLESS WORD
      (PARSING-ERROR
              "No host name following ~A in address." (SECOND AT)))
    (COND (*ACCUMULATE-HOST-INTERVALS*
           (PUSH AT *HOST-INTERVALS*)
           (PUSH (CONS 'HOST (COPYLIST WORD)) *HOST-INTERVALS*)))
    (SETF (SECOND WORD) (THIRD AT))
    (UNLESS HOST-INDICATOR
      (SETQ HOST-INDICATOR (LIST NIL (SECOND WORD) NIL)))
    (SETF (FIRST HOST-INDICATOR)
          (NCONC (FIRST HOST-INDICATOR) (NCONS (FIRST WORD))))
    (SETF (THIRD HOST-INDICATOR) (THIRD WORD))))

;;; A PHRASE is a list (STRING START END).  START and END are BPs.
;;; Just read WORDs and concatenate them.
(DEFUN READ-PHRASE (LEXEMES)
  (DO (PHRASE WORD)
      (())
    (MULTIPLE-VALUE (WORD LEXEMES)
      (READ-WORD LEXEMES))
    (UNLESS WORD
      (RETURN (values PHRASE LEXEMES)))
    (IF (NULL PHRASE)
        (SETQ PHRASE (LIST (FIRST WORD) (SECOND WORD) NIL))
      (SETF (FIRST PHRASE) (STRING-APPEND (FIRST PHRASE) #/SP (FIRST WORD))))
    (SETF (THIRD PHRASE) (THIRD WORD))))

;;; A WORD looks just like a PHRASE.
;;; Either return a word and the lexemes left after it
;;; or return NIL and all the lexemes we were given.
(DEFUN READ-WORD (LEXEMES &AUX (ORIGINAL-LEXEMES LEXEMES))
  (DISCARD-COMMENTS)
  (IF (MEMQ (CAAR LEXEMES) '(ATOM QUOTED-STRING))
      (VALUES (CDAR LEXEMES) (CDR LEXEMES))
    (VALUES NIL ORIGINAL-LEXEMES)))

;;;; Address movement
(DEFCOM COM-FORWARD-ADDRESS "Move one or more addresses forward." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-ADDRESS (POINT) *NUMERIC-ARG*) (BARF)))
  DIS-BPS)

(DEFCOM COM-BACKWARD-ADDRESS "Move one or more addresses backward." (KM)
  (MOVE-BP (POINT) (OR (FORWARD-ADDRESS (POINT) (- *NUMERIC-ARG*)) (BARF)))
  DIS-BPS)

(DEFCOM COM-KILL-ADDRESS "Kill one or more addresses forward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-ADDRESS *NUMERIC-ARG*))

(DEFCOM COM-BACKWARD-KILL-ADDRESS "Kill one or more addresses backward." ()
  (KILL-COMMAND-INTERNAL #'FORWARD-ADDRESS (- *NUMERIC-ARG*)))

(DEFCOM COM-EXCHANGE-ADDRESSES "Interchange the addresses before and after the cursor." ()
  (EXCHANGE-SUBR 'FORWARD-ADDRESS *NUMERIC-ARG*)
  DIS-TEXT)

(DEFCOM COM-MARK-ADDRESS "Set mark one or more addresses from point." (SM)
  (LET (BP1 BP2)
    (SETQ BP1 (FORWARD-ADDRESS (POINT) *NUMERIC-ARG* T))
    (SETQ BP2 (FORWARD-ADDRESS BP1 (MINUS *NUMERIC-ARG*) T))
    (AND (MINUSP *NUMERIC-ARG*)
         (SETQ BP2 (PROG1 BP1 (SETQ BP1 BP2))))
    (MOVE-BP (POINT) BP1)
    (MOVE-BP (MARK) BP2))
  DIS-BPS)

(DEFUN FORWARD-ADDRESS (BP &OPTIONAL (TIMES 1) FIXUP-P)
  (COND ((ZEROP TIMES)
         (COPY-BP BP))
        ((PLUSP TIMES)
         (DO ((LINE (BP-LINE BP))
              (END-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
              (START-BP) (START-ADDRESS)
              (END-BP) (END-ADDRESS)
              (INTERVAL))
             (NIL)
           (SETQ START-ADDRESS NIL
                 END-ADDRESS NIL)
           (DOLIST (ADDRESS (ADDRESSES-STARTING-ON-LINE LINE))
             (AND (SETQ INTERVAL (GET (LOCF ADDRESS) :INTERVAL))
                  (NOT (BP-< (FIRST INTERVAL) BP))
                  (OR (NULL START-ADDRESS) (BP-< (FIRST INTERVAL) START-BP))
                  (SETQ START-BP (FIRST INTERVAL)
                        START-ADDRESS ADDRESS)))
           (DOLIST (ADDRESS (ADDRESSES-ENDING-ON-LINE LINE))
             (AND (SETQ INTERVAL (GET (LOCF ADDRESS) :INTERVAL))
                  (BP-< BP (SECOND INTERVAL))
                  (OR (NULL END-ADDRESS) (BP-< (SECOND INTERVAL) END-BP))
                  (SETQ END-BP (SECOND INTERVAL)
                        END-ADDRESS ADDRESS)))
           (COND ((AND (NULL START-ADDRESS) (NULL END-ADDRESS))
                  (AND (EQ LINE END-LINE)
                       (RETURN (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))))
                  (SETQ LINE (LINE-NEXT LINE)))
                 (T
                  (SETQ BP (IF (OR (NULL END-BP) (AND START-BP (BP-< START-BP END-BP)))
                               (SECOND (GET (LOCF START-ADDRESS) :INTERVAL))
                               END-BP)
                        LINE (BP-LINE BP)
                        TIMES (1- TIMES))
                  (AND (ZEROP TIMES) (RETURN BP))))))
        (T
         (DO ((LINE (BP-LINE BP))
              (START-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
              (END-BP) (END-ADDRESS)
              (START-BP) (START-ADDRESS)
              (INTERVAL))
             (NIL)
           (SETQ END-ADDRESS NIL
                 START-ADDRESS NIL)
           (DOLIST (ADDRESS (ADDRESSES-ENDING-ON-LINE LINE))
             (AND (SETQ INTERVAL (GET (LOCF ADDRESS) :INTERVAL))
                  (NOT (BP-< BP (SECOND INTERVAL)))
                  (OR (NULL END-BP) (BP-< END-BP (SECOND INTERVAL)))
                  (SETQ END-BP (SECOND INTERVAL)
                        END-ADDRESS ADDRESS)))
           (DOLIST (ADDRESS (ADDRESSES-STARTING-ON-LINE LINE))
             (AND (SETQ INTERVAL (GET (LOCF ADDRESS) :INTERVAL))
                  (BP-< (FIRST INTERVAL) BP)
                  (OR (NULL START-BP) (BP-< START-BP (FIRST INTERVAL)))
                  (SETQ START-BP (FIRST INTERVAL)
                        START-ADDRESS ADDRESS)))
           (COND ((AND (NULL END-ADDRESS) (NULL START-ADDRESS))
                  (AND (EQ LINE START-LINE)
                       (RETURN (AND FIXUP-P (COPY-BP (INTERVAL-LAST-BP *INTERVAL*)))))
                  (SETQ LINE (LINE-PREVIOUS LINE)))
                 (T
                  (SETQ BP (IF (OR (NULL START-BP) (AND END-BP (BP-< START-BP END-BP)))
                               (FIRST (GET (LOCF END-ADDRESS) :INTERVAL))
                               START-BP)
                        LINE (BP-LINE BP)
                        TIMES (1+ TIMES))
                  (AND (ZEROP TIMES) (RETURN BP))))))))

(DEFUN ADDRESSES-STARTING-ON-LINE (LINE &AUX PLIST TEM)
  (SETQ PLIST (LOCF (LINE-CONTENTS-PLIST LINE)))
  (IF (SETQ TEM (GETL PLIST '(STARTING-ADDRESSES)))
      (CADR TEM)
      (PARSE-ADDRESSES-AROUND-LINE LINE)
      (GET PLIST 'STARTING-ADDRESSES)))

(DEFUN ADDRESSES-ENDING-ON-LINE (LINE &AUX PLIST TEM)
  (SETQ PLIST (LOCF (LINE-CONTENTS-PLIST LINE)))
  (IF (SETQ TEM (GETL PLIST '(ENDING-ADDRESSES)))
      (CADR TEM)
      (PARSE-ADDRESSES-AROUND-LINE LINE)
      (GET PLIST 'ENDING-ADDRESSES)))

(DEFUN PARSE-ADDRESSES-AROUND-LINE (LINE &AUX START-LINE END-LINE)
  (DO ((FIRST-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
      ((OR (EQ LINE FIRST-LINE)
           (NOT (CONTINUATION-LINE-P LINE))))
    (SETQ LINE (LINE-PREVIOUS LINE)))
  (SETQ START-LINE LINE)
  (DO ((LAST-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
      (NIL)
    (SETQ END-LINE LINE)
    (AND (EQ LINE LAST-LINE) (RETURN nil))
    (SETQ LINE (LINE-NEXT LINE))
    (AND (NOT (CONTINUATION-LINE-P LINE)) (RETURN nil)))
  (DO ((LINE START-LINE (LINE-NEXT LINE))
       (PLIST))
      (NIL)
    (SETQ PLIST (LOCF (LINE-CONTENTS-PLIST LINE)))
    (PUTPROP PLIST NIL 'STARTING-ADDRESSES)
    (PUTPROP PLIST NIL 'ENDING-ADDRESSES)
    (AND (EQ LINE END-LINE) (RETURN nil)))
  (LOOP FOR (TYPE ADDRESSES) ON (PARSE-ONE-HEADER START-LINE END-LINE) BY 'CDDR
        WHEN (MEMQ TYPE *ADDRESS-TYPE-HEADERS*)
        DO (DOLIST (ADDRESS ADDRESSES)
             (LET ((INTERVAL (GET (LOCF ADDRESS) :INTERVAL)))
               (COND (INTERVAL
                      (LET ((START-LINE (BP-LINE (FIRST INTERVAL))))
                        (PUSH ADDRESS (GET (LOCF (LINE-CONTENTS-PLIST START-LINE))
                                           'STARTING-ADDRESSES)))
                      (LET ((END-LINE (BP-LINE (SECOND INTERVAL))))
                        (PUSH ADDRESS (GET (LOCF (LINE-CONTENTS-PLIST END-LINE))
                                           'ENDING-ADDRESSES)))))))))

(DEFUN CONTINUATION-LINE-P (LINE)
  (AND (PLUSP (LINE-LENGTH LINE))
       (MEMQ (CHAR LINE 0) '(#/SP #/TAB))))

;;;; Header parsing
;;; Parse headers in the given interval, returning a list
(DEFUN PARSE-HEADERS-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P STOP-AT-BLANK-LINE &AUX LIST)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((LINE (BP-LINE BP1) (AND (NEQ LINE LIMIT-LINE) (LINE-NEXT LINE)))
       (LAST-LINE NIL LINE)
       (LIMIT-LINE (BP-LINE BP2))
       (RETURN-P NIL)
       (BLANK-P NIL NIL)
       (START-LINE NIL))
      (NIL)
    (when (COND ((NULL LINE)
                 (SETQ RETURN-P T))
                ((EQ last-line LIMIT-LINE)
                 (AND (ZEROP (BP-INDEX BP2)) (SETQ RETURN-P T))
                 T)
                ((ZEROP (LINE-LENGTH LINE))     ;Blank line
                 (AND START-LINE STOP-AT-BLANK-LINE
                      (SETQ BP2 (CREATE-BP (OR (LINE-NEXT LINE) LINE) 0)
                            RETURN-P T))
                 (SETQ BLANK-P T)
                 T)
                ((MEMQ (CHAR LINE 0) '(#/SP #/TAB))   ;Continuation line
                 NIL)
                (T
                 T))
      ;; Are now at the start of something new
      (AND START-LINE
           (LET ((HEADERS (PARSE-ONE-HEADER START-LINE LAST-LINE)))
             (LOOP FOR LINE = START-LINE THEN (LINE-NEXT LINE)
                   DO (PUTPROP (LOCF (LINE-CONTENTS-PLIST LINE)) HEADERS 'PARSED-HEADERS)
                   UNTIL (EQ LINE LAST-LINE))
             (LOOP FOR (TYPE PROP) ON HEADERS BY 'CDDR
                   AS SUBLIST = (CAR (REMPROP (LOCF LIST) TYPE))
                   DO (SETQ SUBLIST
                            (IF SUBLIST
                                (IF (MEMQ TYPE *DATE-TYPE-HEADERS*)
                                    (MAX SUBLIST PROP)
                                  (APPEND (IF (LISTP SUBLIST) SUBLIST
                                            (NCONS SUBLIST))
                                          (IF (LISTP PROP) PROP (NCONS PROP))))
                              PROP)
                            LIST (NCONC LIST (LIST TYPE SUBLIST))))))
      (SETQ START-LINE (AND (NOT BLANK-P) LINE)))
    (AND RETURN-P (RETURN NIL)))
  (VALUES LIST BP2))

;;; Parse a single line or line and continuation line(s)
(DEFUN PARSE-ONE-HEADER (START-LINE END-LINE &AUX INDEX TYPE FLAG)
  (IF (NULL (SETQ INDEX (STRING-SEARCH-CHAR #/: START-LINE)))
      '(LOSING-HEADERS "Line without a colon")
      (SETQ TYPE (INTERN (STRING-UPCASE (NSUBSTRING START-LINE 0 INDEX)) ""))
      (AND (EQ TYPE :RE) (SETQ TYPE :SUBJECT))
      (SETQ INDEX (OR (STRING-SEARCH-NOT-SET '(#/SP #/TAB) START-LINE (1+ INDEX))
                      (LINE-LENGTH START-LINE)))
      (COND ((MEMQ TYPE *ADDRESS-TYPE-HEADERS*)
             (LET ((PROP (PARSE-ADDRESSES-INTERVAL (CREATE-BP START-LINE INDEX)
                                                   (END-OF-LINE END-LINE)
                                                   T)))
               (IF (STRINGP PROP)
                   `(LOSING-HEADERS ,PROP)
                 `(,TYPE ,PROP))))
            (T
             (AND (NEQ START-LINE END-LINE)
                  (SETQ START-LINE (STRING-INTERVAL (CREATE-BP START-LINE INDEX)
                                                    (END-OF-LINE END-LINE)
                                                    T)
                        INDEX 0
                        FLAG T))
             (COND ((MEMQ TYPE *DATE-TYPE-HEADERS*)
                    (CONDITION-CASE (VAL)
                        (TIME:PARSE-UNIVERSAL-TIME START-LINE INDEX
                                                   NIL NIL NIL NIL T NIL NIL)
                      (ERROR `(LOSING-HEADERS ,(SEND VAL :REPORT-STRING)))
                      (:NO-ERROR `(,TYPE ,VAL))))
                   ((AND (MEMQ TYPE *SINGLE-LINE-TYPE-HEADERS*)
                         (OR FLAG (MEMQ TYPE *REFERENCE-TYPE-HEADERS*)))
                    `(,TYPE ,(LOOP FOR START-IDX = INDEX
                                                 THEN (STRING-SEARCH-NOT-SET '(#/SP #/TAB)
                                                                             START-LINE
                                                                             (+ END-IDX 2))
                                   AS END-IDX = (STRING-SEARCH ",
"
                                                               START-LINE START-IDX)
                                   COLLECT (IF (MEMQ TYPE *REFERENCE-TYPE-HEADERS*)
                                               (PARSE-REFERENCE START-LINE START-IDX END-IDX)
                                               (SUBSTRING START-LINE START-IDX END-IDX))
                                   UNTIL (NULL END-IDX))))
                   ((EQ TYPE :FORWARDED-TO)
                    (PARSE-COMSYS-FORWARDED-TO START-LINE INDEX))
                   (T
                    `(,TYPE ,(IF FLAG START-LINE (SUBSTRING START-LINE INDEX)))))))))

(DEFUN PROBABLE-ITS-HEADER-P (LINE &OPTIONAL (START 0) END &AUX TEM)
  (DECLARE (RETURN-LIST DATE-START FROM-ADDRESSES))
  (OR END (SETQ END (STRING-LENGTH LINE)))
  (AND (OR (NULL (SETQ TEM (STRING-SEARCH-CHAR #/: LINE START END)))
           (> TEM (OR (STRING-SEARCH-CHAR #/SP LINE START END) END)))
       (LISTP (SETQ TEM (RFC733-LEXER LINE START (OR TEM END) NIL)))
       (LOOP WITH LEXEMES = TEM AND LAST-HAD-AT
             UNLESS (EQ (CAAR LEXEMES) 'ATOM) RETURN NIL
             AS ADDRESS = `(:NAME ,(CADAR LEXEMES))
             IF (MEMQ (CAADR LEXEMES) '(ATSIGN AT-ATOM))
             DO (OR (EQ (CAADDR LEXEMES) 'ATOM) (RETURN NIL))
                (SETQ ADDRESS (NCONC ADDRESS `(:HOST (,(SECOND (THIRD LEXEMES)))
                                               :INTERVAL (,(THIRD (FIRST LEXEMES))
                                                          ,(FOURTH (THIRD LEXEMES)))))
                      LEXEMES (CDDDR LEXEMES)
                      LAST-HAD-AT T)
             ELSE DO (SETQ LEXEMES (CDR LEXEMES)
                           LAST-HAD-AT NIL)
             COLLECT ADDRESS INTO ADDRESSES
             DO (LET ((NEXT (CAAR LEXEMES)))
                  (COND ((EQ NEXT 'COMMA))      ;Continue
                        ((AND LAST-HAD-AT (MEMQ NEXT '(ATOM COMMENT)))
                         (RETURN (values (SECOND (THIRD (FIRST LEXEMES))) ADDRESSES)))
                        (T
                         (RETURN NIL))))
             DO (SETQ LEXEMES (CDR LEXEMES)))))

;;; Parse an abbreviated ITS style header
(DEFUN PARSE-ITS-HEADER (LINE DATE-START END FROM-ADDRESSES &AUX LIST LOSE-P)
  (SETQ LIST `(:FROM ,FROM-ADDRESSES))
  (LET ((SENT-BY-END (+ DATE-START 11)))
    (AND (STRING-EQUAL "(Sent by " LINE :START1 0 :START2 DATE-START :END1 11 :END2 SENT-BY-END)
         (LET ((BEFORE-AT-POS (STRING-SEARCH-CHAR #/@ LINE SENT-BY-END)))
           (SETQ LIST `(,@LIST :SENDER ((,@(IF BEFORE-AT-POS
                                               `(:NAME ,(SUBSTRING LINE SENT-BY-END
                                                                   BEFORE-AT-POS)
                                                 :HOST (,(SUBSTRING
                                                           LINE (1+ BEFORE-AT-POS)
                                                           (SETQ DATE-START
                                                                 (STRING-SEARCH-CHAR
                                                                   #/) LINE BEFORE-AT-POS))))
                                                 )
                                               `(:NAME ,(SUBSTRING
                                                          LINE SENT-BY-END
                                                          (SETQ DATE-START (STRING-SEARCH-CHAR
                                                                             #/) LINE
                                                                             SENT-BY-END)))))
                                         :INTERVAL ((,LINE ,SENT-BY-END)
                                                    (,LINE ,DATE-START)))))
                 DATE-START (1+ DATE-START)))))
  (LET ((RE (STRING-SEARCH "Re: " LINE DATE-START END)))
    (CONDITION-CASE (TIME)
        (TIME:PARSE-UNIVERSAL-TIME LINE DATE-START RE NIL NIL T NIL NIL NIL)
      (ERROR (SETQ LOSE-P (SEND TIME :REPORT-STRING)))
      (:NO-ERROR (SETQ LIST `(,@LIST :DATE ,TIME))))
    (AND RE
         (SETQ LIST `(,@LIST
                      :SUBJECT ,(STRING-TRIM '(#/SP) (NSUBSTRING LINE (+ RE 4) END))))))
  (AND LOSE-P (SETQ LIST `(LOSING-HEADERS ,LOSE-P . ,LIST)))
  `(,@LIST ITS-HEADER-P T))

;;; Parse the head of a *MSG type message
(DEFUN PARSE-*MSG-START (START-LINE)
  (DECLARE (RETURN-LIST STATUS START-LINE))
  (DO ((LINE START-LINE (LINE-NEXT LINE))
       (STATUS NIL))
      (NIL)
    (COND ((STRING-EQUAL-START LINE :MSG)
           (SETQ STATUS (APPEND STATUS (LIST :*MSG
                                             (DO ((I)
                                                  (J 4)
                                                  (LIST NIL (CONS (SUBSTRING LINE I J) LIST)))
                                                 (NIL)
                                               (OR (AND J
                                                        (SETQ I (STRING-SEARCH-NOT-CHAR
                                                                  #/SP LINE J)))
                                                   (RETURN (NREVERSE LIST)))
                                               (SETQ J (STRING-SEARCH-CHAR #/SP LINE I)))))))
          ((STRING-EQUAL-START LINE :DISTRIB)
           (SETQ STATUS (APPEND STATUS (LIST :DISTRIB
                                             (DO ((I)
                                                  (J 8 (1+ J))
                                                  (LIST NIL))
                                                 (NIL)
                                               (SETQ I (STRING-SEARCH-NOT-CHAR #/SP LINE J)
                                                     J (STRING-SEARCH-CHAR #/, LINE I))
                                               (PUSH (SUBSTRING LINE I J) LIST)
                                               (OR J (RETURN (NREVERSE LIST))))))))
          ((STRING-EQUAL-START LINE :EXPIRES)
           (SETQ STATUS (APPEND STATUS (LIST :EXPIRES
                                             (CONDITION-CASE ()
                                                 (TIME:PARSE-UNIVERSAL-TIME
                                                   LINE 8 (LINE-LENGTH LINE)
                                                   NIL NIL T NIL NIL NIL)
                                               (ERROR -1))))))
          (T
           (RETURN (values STATUS LINE))))))

(DEFUN PARSE-COMSYS-FORWARDED-TO (STRING &OPTIONAL (START 0) END &AUX TO FROM DATE LOSE-P)
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (IF (NOT (AND (SETQ DATE (STRING-REVERSE-SEARCH " on " STRING END))
                (SETQ FROM (STRING-REVERSE-SEARCH " by " STRING DATE))))
      '(LOSING-HEADERS "Bad format Forward-to field")
      (SETQ TO (PARSE-ADDRESSES STRING START FROM)
            FROM (PARSE-ADDRESSES STRING (+ FROM 4) DATE)
            DATE (CONDITION-CASE (ERROR)
                     (TIME:PARSE-UNIVERSAL-TIME STRING (+ DATE 4) END NIL NIL T NIL NIL NIL)
                   (ERROR (SEND ERROR :REPORT-STRING))))
      (IF (SETQ LOSE-P (COND ((STRINGP TO) TO)
                             ((STRINGP FROM) FROM)
                             ((STRINGP DATE) DATE)))
          `(LOSING-HEADERS ,LOSE-P)
          `(:FORWARDED-TO (:TO ,TO :FROM ,FROM :DATE ,DATE)
            :FORWARDED-TO-TO ,TO
            :FORWARDED-TO-FROM ,FROM
            :FORWARDED-TO-DATE ,DATE))))

(DEFUN PARSE-ITS-MSG-HEADERS (START-BP &OPTIONAL END-BP IN-ORDER-P REFORMATTED
                                       &AUX LINE STOP-BP (NEWSTAT NIL) TEM TEM1)
  (GET-INTERVAL START-BP END-BP IN-ORDER-P)
  (SETQ LINE (BP-LINE START-BP))
  (DO () ((NOT (LINE-BLANK-P LINE))) (SETQ LINE (LINE-NEXT LINE)))
  (AND (STRING-EQUAL-START LINE "MSG:")
       (MULTIPLE-VALUE (NEWSTAT LINE)
         (PARSE-*MSG-START LINE)))
  (COND ((AND (NOT REFORMATTED)
              (MULTIPLE-VALUE (TEM TEM1)
                (PROBABLE-ITS-HEADER-P LINE)))
         (SETQ NEWSTAT (APPEND NEWSTAT
                               (PARSE-ITS-HEADER LINE TEM (LINE-LENGTH LINE) TEM1)))
         (LET* ((START-LINE (LINE-NEXT LINE))
                (END-LINE (DO ((LINE START-LINE (LINE-NEXT LINE)))
                              ((NOT (OR (STRING-EQUAL-START LINE "To: ")
                                        (STRING-EQUAL-START LINE "Cc: ")))
                               LINE))))
           (OR (EQ END-LINE START-LINE)
               (SETQ NEWSTAT (APPEND NEWSTAT
                                     (PARSE-HEADERS-INTERVAL
                                       (CREATE-BP START-LINE 0)
                                       (LET ((LINE (LINE-PREVIOUS END-LINE)))
                                         (CREATE-BP LINE (LINE-LENGTH LINE)))
                                       T))))
           (SETQ STOP-BP (CREATE-BP END-LINE 0))))
        (T
         (MULTIPLE-VALUE (TEM STOP-BP)
           (PARSE-HEADERS-INTERVAL START-BP END-BP T T))
         (SETQ NEWSTAT (APPEND NEWSTAT TEM))))
  (VALUES NEWSTAT STOP-BP))

(DEFUN STRING-EQUAL-START (STRING PATTERN &OPTIONAL (START 0))
  (OR (STRINGP PATTERN)
      (SETQ PATTERN (STRING PATTERN)))
  (%STRING-EQUAL STRING START PATTERN 0 (ARRAY-ACTIVE-LENGTH PATTERN)))

(DEFPROP :ADDRESS-LIST
         (PRINT-ADDRESS-LIST READ-ADDRESS-LIST NIL
          NIL NIL "Click left to enter list of addresses from the keyboard.")
         TV:CHOOSE-VARIABLE-VALUES-KEYWORD)

(DEFUN READ-ADDRESS-LIST (STREAM &AUX ADDRESSES)
  (SETQ ADDRESSES (PARSE-ADDRESSES (READLINE STREAM)))
  (AND (ERRORP ADDRESSES) (ZMAIL-ERROR "Bad addresses: ~A" ADDRESSES))
  (LOOP FOR ADDRESSES ON ADDRESSES
        DO (REMPROP (LOCF (CAR ADDRESSES)) :INTERVAL))
  ADDRESSES)

(DEFUN PRINT-ADDRESS-LIST (ADDRESSES STREAM)
  (cond ((listp addresses)
         (LOOP FOR ADDRESS IN ADDRESSES
               WITH COMMA-P = NIL
               DO (IF COMMA-P (SEND STREAM :STRING-OUT ", ") (SETQ COMMA-P T))
               (SEND STREAM :STRING-OUT (STRING-FROM-HEADER ADDRESS :SHORT))))
 ;things screwwed up, try not to bomb at least.
        ((stringp addresses)
         (send stream :string-out addresses))))

;;;; Reference parsing

;;; Parse the in-reply-to field.  This understands everything i could find in the minutes
;;; of HEADER-PEOPLE, which will serve as the network standard for now.
(DEFUN PARSE-REFERENCE (STRING &OPTIONAL (START 0) END)
  (OR END (SETQ END (STRING-LENGTH STRING)))
  ;;"<80265.32584.6980 @ Darcom-hq>"
  (IF (AND (= (AREF STRING START) #/<)
           (= (AREF STRING (1- END)) #/>))
      `(:MESSAGE-ID ,(SUBSTRING STRING START END))
    (LET (DATE FROM FROM-OK TEM TEM1)
      (CONDITION-CASE ()
          (COND ((NULL (SETQ START (STRING-SEARCH-NOT-SET *BLANKS* STRING START END))))
                ;;"Your message of 21 Sep 1980 22:12 PDT"
                ((STRING-EQUAL-START STRING "Your message of " START)
                 ;; RAND peculiarity
                 (AND (STRING-EQUAL STRING ")." :START1 (- END 2) :START2 0 :END1 END)
                      (SETQ END (1- END)))
                 (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME STRING (+ START 16.) END
                                                       NIL NIL NIL NIL NIL NIL)
                       FROM-OK T))
                ;;"Message of 12 Mar 81 at 1937 PST by Admin.MRC@SU-SCORE"
                ((AND (STRING-EQUAL-START STRING "Message of " START)
                      (SETQ TEM (OR (STRING-SEARCH " by " STRING START END)
                                    (STRING-SEARCH " from " STRING START END))))
                 (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME STRING (+ START 11.) TEM
                                                       NIL NIL NIL NIL NIL NIL)
                       FROM (PARSE-ADDRESSES STRING
                                             (1+ (STRING-SEARCH-CHAR #/SP STRING (1+ TEM)))
                                             END))
                 (AND (LISTP FROM)
                      (SETQ FROM-OK T
                            FROM (SOME-PLIST (CAR FROM) '(:NAME :HOST)))))
                ;;"Earl A. Killian's message of 23 Mar 81 03:41-EST"
                ((AND (SETQ TEM (STRING-SEARCH " message of " STRING START END))
                      ;;"Rick Gumpertz' message ..."
                      (SETQ TEM1 (STRING-REVERSE-SEARCH-CHAR #/' STRING TEM START)))
                 (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME STRING (+ TEM 12.) END
                                                       NIL NIL NIL NIL NIL NIL)
                       FROM (PARSE-ADDRESSES STRING START TEM1))
                 (COND ((LISTP FROM)
                        (SETQ FROM-OK T
                              FROM (CAR FROM))
                        (IF (NULL (GET (LOCF FROM) :HOST))
                            (SETQ FROM `(:PERSONAL-NAME ,(GET (LOCF FROM) :NAME)))
                            (SETQ FROM (SOME-PLIST FROM '(:NAME :HOST)))))))
                ;;"Message from Richard M. Stallman <RMS at MIT-AI>
                ;;  of 28-May-81 2333-EDT"
                ((AND (STRING-EQUAL-START STRING "Message from " START)
                      (SETQ TEM (STRING-REVERSE-SEARCH " of " STRING END START)))
                 (SETQ FROM (PARSE-ADDRESSES STRING (+ 13. START) TEM)
                       DATE (TIME:PARSE-UNIVERSAL-TIME STRING (+ TEM 4) END
                                                       NIL NIL NIL NIL NIL NIL))
                 (AND (LISTP FROM)
                      (SETQ FROM-OK T
                            FROM (SOME-PLIST (CAR FROM) '(:NAME :HOST)))))
                ;;"The message of 27 Apr 81 13:53-EDT from Daniel L. Weinreb <DLW at MIT-AI>"
                ((AND (STRING-EQUAL-START STRING "The message of " START)
                      (SETQ TEM (STRING-SEARCH " from " STRING START END)))
                 (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME STRING (+ START 15.) TEM
                                                       NIL NIL NIL NIL NIL NIL)
                       FROM (PARSE-ADDRESSES STRING (+ TEM 6) END))
                 (AND (LISTP FROM)
                      (SETQ FROM-OK T
                            FROM (SOME-PLIST (CAR FROM) '(:NAME :HOST)))))
                ;; Anything else?
                )
        (ERROR (SETQ DATE NIL)))
      (AND FROM-OK (NUMBERP DATE)
           `(:DATE ,DATE . ,(AND FROM `(:FROM ,FROM)))))))

;;; Figure out what message if any was yanked into the body of this one as a reply.
(DEFUN GET-MSG-TEXT-REFERENCES (MSG &AUX REFLIST)
  (DO ((LINE (BP-LINE (MSG-START-BP MSG)) (LINE-NEXT LINE))
       (END-LINE (BP-LINE (MSG-END-BP MSG)))
       (LEN) (TEM) (TEM1) (FROM) (DATE))
      ((EQ LINE END-LINE))
    (CATCH-ERROR
      (COND ((NOT (PLUSP (SETQ LEN (LINE-LENGTH LINE)))))
            ((MEMQ (AREF LINE 0) *BLANKS*)      ;Possibly indented line
             (LET ((START-IDX (STRING-SEARCH-NOT-SET *BLANKS* LINE)))
               (COND ((MULTIPLE-VALUE (TEM TEM1)
                        (PROBABLE-ITS-HEADER-P LINE START-IDX))
                      (SETQ TEM (PARSE-ITS-HEADER LINE TEM LEN TEM1))
                      (COND ((SETQ TEM (SOME-PLIST TEM '(:DATE :FROM)))
                             (LET* ((PLIST (LOCF TEM))
                                    (TEM1 (GET PLIST :FROM)))
                               (AND TEM1
                                    (PUTPROP PLIST (SOME-PLIST (CAR TEM1) '(:NAME :HOST))
                                             :FROM)))
                             (PUSH TEM REFLIST))))
                     ((AND (SETQ TEM (STRING-SEARCH-CHAR #/: LINE START-IDX LEN))
                           (STRING-EQUAL LINE "DATE" :START1 START-IDX :START2 0 :END1 TEM)
                           (NUMBERP (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME LINE (1+ TEM) LEN
                                                                          NIL NIL NIL NIL NIL
                                                                          NIL))))
                      (SETQ LINE (LINE-NEXT LINE)
                            LEN (LINE-LENGTH LINE))
                      (AND (EQ START-IDX (STRING-SEARCH-NOT-SET *BLANKS* LINE))
                           (SETQ TEM (STRING-SEARCH-CHAR #/: LINE START-IDX LEN))
                           (STRING-EQUAL LINE "FROM" :START1 START-IDX :START2 0 :END1 TEM)
                           (LISTP (SETQ TEM (PARSE-ADDRESSES LINE (1+ TEM) LEN)))
                           (PUSH `(:DATE ,DATE :FROM ,(SOME-PLIST (CAR TEM) '(:NAME :HOST)))
                                 REFLIST))))))
            ((COND ;; Good old MSG
                   ((AND (STRING-EQUAL-START LINE "In response to the message sent ")
                         (SETQ TEM (STRING-SEARCH " from " LINE)))
                    (SETQ DATE (TIME:PARSE-UNIVERSAL-TIME LINE 32. TEM
                                                          NIL NIL NIL NIL NIL NIL)
                          FROM (PARSE-ADDRESSES LINE (+ TEM 6)))
                    T)
                   ;; In reply to in body of message from some weird mail system in NLS.
                   ((AND (STRING-EQUAL-START LINE "In reply to the message from ")
                         (SETQ TEM (STRING-REVERSE-SEARCH-CHAR #/, LINE LEN)))
                    (SETQ FROM (PARSE-ADDRESSES LINE 29. TEM)
                          DATE (TIME:PARSE-UNIVERSAL-TIME LINE (1+ TEM)
                                                          NIL NIL NIL NIL NIL NIL NIL))
                    T)
                   (T NIL))
             (COND ((AND (LISTP FROM) (NUMBERP DATE))
                    (SETQ FROM (CAR FROM))
                    (REMPROP (LOCF FROM) :INTERVAL)
                    (PUSH `(:DATE ,DATE :FROM ,FROM) REFLIST)))))
      NIL))
  (NREVERSE REFLIST))

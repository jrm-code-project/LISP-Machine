;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable: ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;;
;;;  PL/I Mode for EINE.
;;;  DLW & BSG 6/10/78, copied from Multics EMACS.
;;;  Converted for ZWEI 12/03/78 by DLW.
;;;

;;; NOTE: ONLY WORKS FOR FIXED WIDTH FONTS!

;;; A TOKEN is either a fixnum (meaning a single character which is interesting
;;; to the PL/1 mode commands), or a string.

(DEFVAR *PL1-PACKAGE* (PKG-FIND-PACKAGE "ZWEI"))
(DEFVAR *PL1-DELIMS* '(#/- #/+ #/. #/* #/; #/: #/, #/& #/^ #/< #/> #/= #/| #/( #/)))
(DEFVAR *PL1-INTERESTING-KEYWORDS* '(PROC PROCEDURE BEGIN END DO IF ELSE ON DCL DECLARE))

;; Leaves BP after all blanks, counting comments as blanks.
;; Returns BP.
(DEFUN PL1-SKIP-BLANKS (BP)
  (DO ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
      (NIL)
    (AND (BP-= BP LAST-BP) (RETURN NIL))
    (MOVE-BP BP (FORWARD-OVER *WHITESPACE-CHARS* BP))
    (OR (LOOKING-AT BP "//*") (RETURN NIL))
    (PL1-SKIP-COMMENT BP))
  BP)

;; BP should be right before the beginning of a comment.
;; Leaves BP after the comment, returns BP.
(DEFUN PL1-SKIP-COMMENT (BP)
  (MOVE-BP BP (FORWARD-CHAR BP 2))
  (LET ((X (ZWEI-SEARCH BP "*//")))
    (COND ((NULL X) (BARF "Unbalanced comment."))
          (T (MOVE-BP BP X)))))

;; Starts at BP and scans forward.  Returns NIL at EOB, else
;; the token.  Moves BP.
(DEFUN PL1-GET-TOKEN-FORWARD (BP)
  (PL1-SKIP-BLANKS BP)
  (COND ((BP-= BP (INTERVAL-LAST-BP *INTERVAL*)) NIL)
        (T (LET ((CH (BP-CHAR BP)))
             (COND ((MEM #'CHAR-EQUAL CH *PL1-DELIMS*)
                    (MOVE-BP BP (FORWARD-CHAR BP))
                    CH)
                   ((CHAR-EQUAL CH #/$)
                    (MOVE-BP BP (FORWARD-CHAR BP))
                    "$")
                   ((CHAR-EQUAL CH #//)
                    (MOVE-BP BP (FORWARD-CHAR BP))
                    CH)
                   ((CHAR-EQUAL CH #/")
                    (PL1-GET-STRING-FORWARD BP))
                   (T (LET ((M (FORWARD-WORD BP)))
                        (LET ((N (FORWARD-WORD M -1)))
                          (PROG1 (STRING-INTERVAL N M T)
                                 (MOVE-BP BP M))))))))))

;; Subfunction of GET-TOKEN-FORWARD
(DEFUN PL1-GET-STRING-FORWARD (BP)
  (PROG (SAVE-BP)
     RETRY
        (SETQ SAVE-BP (COPY-BP BP))
        (MOVE-BP BP (FORWARD-CHAR BP))
        (LET ((X (ZWEI-SEARCH BP "/"")))
          (COND ((NULL X) (BARF "Unbalanced string"))
                (T (MOVE-BP BP X))))
        (AND (CHAR-EQUAL (BP-CHAR BP) #/")
             (GO RETRY))
        (RETURN (STRING-INTERVAL SAVE-BP BP T))
        ))

;; Leaves BP before all blanks, counting comments as blanks.
;; Returns BP.
(DEFUN PL1-SKIP-BLANKS-BACKWARD (BP)
  (DO ((FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*)))
      (NIL)
    (AND (BP-= BP FIRST-BP) (RETURN NIL))
    (MOVE-BP BP (BACKWARD-OVER *WHITESPACE-CHARS* BP))
    (OR (LOOKING-AT-BACKWARD BP "*//") (RETURN NIL))
    (PL1-SKIP-COMMENT-BACKWARD BP))
  BP)

;; BP should be right after the end of a comment.
;; Leaves BP before the comment, returns BP.
(DEFUN PL1-SKIP-COMMENT-BACKWARD (BP)
  (MOVE-BP BP (FORWARD-CHAR BP -2))
  (LET ((X (ZWEI-SEARCH BP "//*" T)))
    (COND ((NULL X) (BARF "Unbalanced comment."))
          (T (MOVE-BP BP X)))))

;; Starts at BP and scans backward.  Returns NIL at BOB, else
;; the token.  Moves BP.
(DEFUN PL1-GET-TOKEN-BACKWARD (BP)
  (PL1-SKIP-BLANKS-BACKWARD BP)
  (COND ((BP-= BP (INTERVAL-FIRST-BP *INTERVAL*)) NIL)
        (T (LET ((CH (BP-CHAR-BEFORE BP)))
             (COND ((MEM #'CHAR-EQUAL CH *PL1-DELIMS*)
                    (MOVE-BP BP (FORWARD-CHAR BP -1))
                    CH)
                   ((CHAR-EQUAL CH #/$)
                    (MOVE-BP BP (FORWARD-CHAR BP -1))
                    "$")
                   ((CHAR-EQUAL CH #//)
                    (MOVE-BP BP (FORWARD-CHAR BP -1))
                    CH)
                   ((CHAR-EQUAL CH #/")
                    (PL1-GET-STRING-BACKWARD BP))
                   (T (LET ((M (FORWARD-WORD BP -1)))
                        (LET ((N (FORWARD-WORD M)))
                          (PROG1 (STRING-INTERVAL M N T)
                                 (MOVE-BP BP M))))))))))

;; Subfunction of GET-TOKEN-BACKWARD
(DEFUN PL1-GET-STRING-BACKWARD (BP)
  (PROG (SAVE-BP)
     RETRY
        (SETQ SAVE-BP (COPY-BP BP))
        (MOVE-BP BP (FORWARD-CHAR BP -1))
        (LET ((X (ZWEI-SEARCH BP "/"" T)))
          (COND ((NULL X) (BARF "Unbalanced string"))
                (T (MOVE-BP BP X))))
        (AND (= (BP-CHAR-BEFORE BP) #/")
             (GO RETRY))
        (RETURN (STRING-INTERVAL BP SAVE-BP T))
        ))

;; Returns a cons.  Car is the last token, cdr is a list of tokens from
;; the beginning of the statement up to where BP started.  Moves BP.
(DEFUN PL1-GET-STATEMENT-BACKWARD (BP)
  (LET ((LT (PL1-GET-TOKEN-BACKWARD BP)))
    (AND LT
         (DO ((TOK)
              (A-BUILDING (NCONS LT) (CONS TOK A-BUILDING)))
             (NIL)
           (SETQ TOK (PL1-GET-TOKEN-BACKWARD BP))
           (SELECTQ TOK
             (NIL (RETURN (CONS LT A-BUILDING)))
             (#/; (MOVE-BP BP (FORWARD-CHAR BP 1))
              (RETURN (CONS LT A-BUILDING))))))))

;; Returns four values.
;; First is the BP pointing right before the first token of the stmt.
;; Second is the hpos of that stmt.
;; Third is the statement itself.
;; Fourth is T if the statement is incomplete.
(DEFUN PL1-FIND-START-PREV-STA (BP)
  (PROG (PREV-STA INCOMPLETE-FLAG)
     CHOMP-BACKWARD-SOME-MORE
        (OR (SETQ PREV-STA (PL1-GET-STATEMENT-BACKWARD BP))
            (RETURN NIL))
        (AND (EQ (CAR PREV-STA) #/:)
             (GO CHOMP-BACKWARD-SOME-MORE))
        (SETQ INCOMPLETE-FLAG (NOT (EQ (CAR PREV-STA) #/;)))
        (SETQ PREV-STA (PL1-SKIP-OVER-LABELS (CDR PREV-STA) BP))
        (PL1-SKIP-BLANKS BP)
        (RETURN (VALUES BP (BP-INDEX BP) PREV-STA INCOMPLETE-FLAG))))

;; Takes a statement, and returns a tail of that statement with the
;;   labels CDRed off.  Wins for label arrays and condition prefixes!
;; If BP is given, it will be moved as we parse.
(DEFUN PL1-SKIP-OVER-LABELS (STA &OPTIONAL BP)
  (PROG (CLOSE-PTR)
     RESCAN
        ;; Skip over regular labels.
        (COND ((EQ (SECOND STA) #/:)
               (COND (BP
                      (PL1-PARSE-CHK BP (FIRST STA))
                      (PL1-PARSE-CHK BP #/:)))
               (SETQ STA (REST2 STA))
               (GO RESCAN)))

           ;; Look for label arrays: "   FOO(56):  "
           (COND ((AND (STRINGP (FIRST STA))
                       (EQ (SECOND STA) #/()
                       (PL1-STRING-FIXNUM-P (THIRD STA))
                       (EQ (FOURTH STA) #/))
                       (EQ (FIFTH STA) #/:))
                  (COND (BP
                         (PL1-PARSE-CHK BP (FIRST STA))
                         (PL1-PARSE-CHK BP #/()
                         (PL1-PARSE-CHK BP (THIRD STA))
                         (PL1-PARSE-CHK BP #/))
                         (PL1-PARSE-CHK BP #/:)))
                  (SETQ STA (NTHCDR 5 STA))
                  (GO RESCAN)))

              ;; Skip over condition prefixes.
              (COND ((AND (EQ (FIRST STA) #/()
                          (SETQ CLOSE-PTR (MEMQ #/) (REST1 STA)))
                          (EQ (SECOND CLOSE-PTR) #/:))
                     (DO X STA (CDR X) (EQ X (CDDR CLOSE-PTR))
                         (AND BP (PL1-PARSE-CHK BP (CAR STA)))
                         (SETQ STA (CDR STA)))
                     (GO RESCAN)))

                 (RETURN STA)))

;; T => This string represents a number in PL1 syntax.
(DEFUN PL1-STRING-FIXNUM-P (X)
  (AND (STRINGP X)
       (PLUSP (STRING-LENGTH X))
       (LET ((CH (AREF X 0)))
         (AND ( CH #/0) ( CH #/9)))))

;; Returns two values: a type (a keyword symbol), and ???
;; If BP is given, it will be moved as we parse.
(DEFUN PL1-TYPIFY-STATEMENT (STA &OPTIONAL BP &AUX (KEY (CAR STA)))
  (PROG ()
        (COND ((EQ KEY #/;)
               (RETURN (VALUES 'NULL NIL)))
              ((NOT (STRINGP KEY))
               (RETURN (VALUES 'RANDOM NIL))))
        (SETQ KEY (INTERN (STRING-UPCASE (STRING-TRIM '(#\SP #\TAB) KEY))
                          *PL1-PACKAGE*))
        (COND ((NOT (MEMQ KEY *PL1-INTERESTING-KEYWORDS*))
               (RETURN (VALUES 'RANDOM STA)))
              ((EQ (SECOND STA) #/;)
               (AND BP (PL1-PARSE-CHK BP (FIRST STA)))
               (RETURN (VALUES KEY (CDR STA))))
              ((EQ KEY 'IF)
               (PL1-TYPIFY-IF-HACKER STA BP))
              ((AND (FIXP (SECOND STA))
                    (NOT (EQ (SECOND STA) #/()))
               (RETURN (VALUES 'RANDOM STA)))
              ((EQ KEY 'BEGIN)
               (COND ((STRINGP (SECOND STA))
                      (RETURN (VALUES KEY STA)))
                     (T (RETURN (VALUES 'RANDOM STA)))))
              ((EQ KEY 'ON)
               (PL1-TYPIFY-ON-HACKER STA BP))
              ((EQ KEY 'DO)
               (PL1-TYPIFY-DO-HACKER STA BP))
              ((EQ KEY 'ELSE)
               (AND BP (PL1-PARSE-CHK BP "ELSE"))
               (RETURN (VALUES 'ELSE (CDR STA))))
              ((PL1-TYPIFY-0LEV-PARENCHECK STA BP)
               (RETURN (VALUES 'RANDOM STA)))
              (T (RETURN (VALUES KEY (CDR STA)))))))

;; T => This is an assignment statment.
(DEFUN PL1-TYPIFY-0LEV-PARENCHECK (STA IGNORE)
  (DO ((PARNCT 0)
       (X STA (CDR X)))
      ((OR (NULL X)
           (EQ (CAR X) #/;))
       NIL)
    (COND ((EQ (CAR X) #/()
           (SETQ PARNCT (1+ PARNCT)))
          ((EQ (CAR X) #/))
           (SETQ PARNCT (1- PARNCT)))
          ((NOT (ZEROP PARNCT)))
          ((EQ (CAR X) #/=)
           (RETURN T)))))

(DEFUN PL1-TYPIFY-DO-HACKER (STA IGNORE)
  (COND ((OR (STRINGP (SECOND STA))
             (EQ (SECOND STA) #/;))
         (VALUES 'DO STA))
        (T (VALUES 'RANDOM STA))))

(DEFUN PL1-TYPIFY-IF-HACKER (STA BP)
  (PROG (VAL1)
        (COND ((AND (FIXP (SECOND STA))
                    (NOT (MEMQ (SECOND STA) '(#/- #/+ #/^ #/())))
               (SETQ VAL1 'RANDOM))
              ((AND (EQ (SECOND STA) #/-)
                    (EQ (THIRD STA) #/>))
               (SETQ VAL1 'RANDOM))
              (T (DO ((PARNCT 0)
                      (PREV #/=)
                      (TSTA STA (CDR TSTA)))
                     ((OR (NULL TSTA)
                          (EQ (FIRST TSTA) #/;))
                      (SETQ VAL1 'RANDOM))
                   (COND ((EQ (FIRST TSTA) #/()
                          (SETQ PARNCT (1+ PARNCT)))
                         ((EQ (FIRST TSTA) #/))
                          (SETQ PARNCT (1- PARNCT)))
                         ((NOT (ZEROP PARNCT)))
                         ((NOT (STRINGP (FIRST TSTA))))
                         ((NOT (STRING-EQUAL (FIRST TSTA) "THEN")))
                         ((OR (STRINGP PREV)
                              (EQ PREV #/))
                              (EQ PREV #/.))
                          ;; It is really an IF statement!
                          (RETURN
                            (DO ((X STA (CDR X)))
                                ((EQ X (CDR TSTA))
                                 (SETQ VAL1 'IF STA X))
                              (AND BP (PL1-PARSE-CHK BP (CAR X)))))))
                   (SETQ PREV (CAR TSTA)))))
        (RETURN (VALUES VAL1 STA))))

(DEFUN PL1-TYPIFY-ON-HACKER (STA BP)
  (COND ((NOT (STRINGP (SECOND STA)))
         (VALUES 'RANDOM STA))
        (T (AND BP (PL1-PARSE-CHK BP "ON"))
           (AND BP (PL1-PARSE-CHK BP (SECOND STA)))
           (SETQ STA (CDDR STA))
           (DO () (NIL)
             (COND ((AND (STRINGP (SECOND STA))
                         (EQ (CAR STA) #/,))
                    (COND (BP
                           (PL1-PARSE-CHK BP (FIRST STA))
                           (PL1-PARSE-CHK BP (SECOND STA))))
                    (SETQ STA (CDDR STA)))
                   (T (RETURN NIL))))
           (COND ((AND (EQ (SECOND STA) #/;)
                       (STRINGP (FIRST STA))
                       (STRING-EQUAL (FIRST STA) "SYSTEM"))
                  (AND BP (PL1-PARSE-CHK BP "SYSTEM"))
                  (SETQ STA (CDR STA))))
           (COND ((AND (STRINGP (FIRST STA))
                       (STRING-EQUAL (FIRST STA) "SNAP")
                       (PL1-TYPIFY-RIDICULOUS-SNAP-SCREW STA BP))
                  (AND BP (PL1-PARSE-CHK BP "SNAP"))
                  (SETQ STA (CDR STA))))
           (VALUES 'ON STA))))

(DEFUN PL1-TYPIFY-RIDICULOUS-SNAP-SCREW (STA IGNORE)
  (COND ((EQ (SECOND STA) #/;) T)
        ((NULL (CDR STA)) T)
        ((STRINGP (CADR STA)) T)
        ((NOT (EQ (SECOND STA) #/()) NIL)
        ;; Now we worry about whether we have
        ;;     SNAP (13) = 5; or SNAP (FIXEDOVERFLOW): or SNAP (13):
        ((NOT (EQ (PL1-SKIP-OVER-LABELS STA NIL) STA)) NIL)     ; Label array.
        ((EQ (PL1-SKIP-OVER-LABELS (CDR STA) NIL) (CDR STA)) NIL)       ; Assignment stmt.
        (T T)))

(DEFUN PL1-PARSE-CHK (BP LEXEME)
  (LET ((PARSED (PL1-GET-TOKEN-FORWARD BP)))
    (COND ((FIXP PARSED)
           (OR (EQ LEXEME PARSED)
               (BARF "PL1 PARSE CHK LOSES 1")))
          ((NOT (STRINGP LEXEME))
           (BARF "PL1 PARSE CHK LOSES 2"))
          ((NOT (STRING-EQUAL PARSED LEXEME))
           (BARF "PL1 PARSE CHK LOSES 3")))))


;; T => This statement is a declaration.
(DEFUN PL1-DECLARE-P (STA)
  (MEMQ (PL1-TYPIFY-STATEMENT STA) '(DCL DECLARE)))

(DEFUN COMPUTE-PL1-INDENTATION (BP)
  (PROG (PREVHPOS PREV-STA INCOMP-FLAG BP1 S S-TYPE)
        (MULTIPLE-VALUE (BP1 PREVHPOS PREV-STA INCOMP-FLAG)
          (PL1-FIND-START-PREV-STA BP))
        (COND ((AND BP1 (PL1-DECLARE-P PREV-STA))
               (DO () (NIL)
                 (MULTIPLE-VALUE (BP1 PREVHPOS PREV-STA INCOMP-FLAG)
                   (PL1-FIND-START-PREV-STA BP))
                 (OR (AND BP1 (PL1-DECLARE-P PREV-STA))
                     (RETURN NIL)))))
        (OR BP1 (RETURN 10.))
        (AND INCOMP-FLAG (RETURN (+ 5 PREVHPOS)))
        (MULTIPLE-VALUE (S-TYPE S)
          (PL1-TYPIFY-STATEMENT PREV-STA NIL))
        (DO ((LEVELS 0))
            (NIL)
          (COND ((MEMQ S-TYPE '(IF ELSE ON))
                 (SETQ LEVELS (1+ LEVELS)))
                ((MEMQ S-TYPE '(DO BEGIN))
                 (SETQ PREVHPOS (+ PREVHPOS (* 5 (MAX LEVELS 1))))
                 (RETURN T))
                ((AND (EQ S-TYPE 'END)
                      (= *PL1-INDING-STYLE* 2))
                 (SETQ PREVHPOS (- PREVHPOS 5))
                 (RETURN T))
                (T (RETURN NIL)))
          (MULTIPLE-VALUE (S-TYPE S)
            (PL1-TYPIFY-STATEMENT (PL1-SKIP-OVER-LABELS S) NIL)))
        (RETURN PREVHPOS)))

(DEFUN WHITESPACE-TO-HPOS (BP GOAL)
  (LET ((HERE (BP-INDEX BP)))
    (AND (> GOAL HERE)
         (DO ((I 0 (1+ I))
              (CHAR (IN-CURRENT-FONT #\SP))
              (SPACES (- GOAL HERE)))
             (( I SPACES))
           (INSERT-MOVING BP CHAR)))))

(DEFCOM COM-INDENT-FOR-PL1 "Indent sufficiently for the PL/I statement
or statement fragment that I am about to type." ()
  (DELETE-AROUND *BLANKS* (POINT))
  (WHITESPACE-TO-HPOS (POINT)
                      (COMPUTE-PL1-INDENTATION (COPY-BP (POINT))))
  DIS-TEXT)

(DEFCOM COM-SET-PL1-STYLE "Set the PL/I mode indentation style.
1 = Standard indentation.
2 = /"end/" line up with statements within their group (they are indented)." ()
  (SETQ *PL1-INDING-STYLE* *NUMERIC-ARG*)
  DIS-NONE)

(DEFCOM COM-ROLL-BACK-PL1-INDENTATION "Undent 5 spaces." ()
  (LET ((INDEX (BP-INDEX (POINT))))
    (DELETE-AROUND *BLANKS* (POINT))
    (WHITESPACE-TO-HPOS (POINT) (- INDEX 5)))
  DIS-TEXT)

(DEFCOM COM-PL1-ELECTRIC-SEMICOLON "Try it, you'll like it." ()
  (LET ((BP (POINT)))
    (COND ((AND (= *PL1-INDING-STYLE* 1)
                (LOOKING-AT-BACKWARD BP "END"))
           (MOVE-BP BP (FORWARD-CHAR BP -3))
           (COM-ROLL-BACK-PL1-INDENTATION)
           (MOVE-BP BP (FORWARD-CHAR BP 3))
           ))
    (INSERT-MOVING BP #/;)
    (COM-INSERT-CRS)
    (COM-INDENT-FOR-PL1))
  DIS-TEXT)

(DEFCOM COM-PL1-ELECTRIC-COLON "Try it, you'll like it." ()
  (LET ((BP (BEG-LINE (POINT))))
    (DELETE-OVER *BLANKS* BP))
  (INSERT-MOVING (POINT) ":")
  (COM-INDENT-FOR-PL1)
  DIS-TEXT)

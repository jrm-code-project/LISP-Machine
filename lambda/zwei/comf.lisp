;;; -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8 -*-
;;; Zwei commands, see ZWEI;COMA for comments
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-FROB-LISP-CONDITIONAL "Change CONDs to ANDs or ORs or IFs, and vice versa.
When changing to COND, point is left in such a place that LF will add another
form to this clause, and M-) will add another clause.  Also in this case
an argument specifies the number of clauses that are left in the
consequent, the default is 1, i.e. all forms but the last are assumed to
be for value, and to belong in the antecedent." ()
  (ATOM-WORD-SYNTAX-BIND
    (LET ((POINT (POINT))
          FIXBP1 FIXBP2)
     (UNWIND-PROTECT
      (LET (COND-BP COND-TYPE UPCASE-P BP)
        (MULTIPLE-VALUE (COND-BP COND-TYPE)
          (FIND-CONTAINING-ATOM POINT '(COND AND OR IF)))       ;Locate the COND or AND or OR
        (OR COND-BP (BARF))
        (SETQ UPCASE-P (UPPER-CASE-P (BP-CHARACTER COND-BP)))   ;Remember if have to lowercase
        (LET ((START-DEFUN-BP (FORWARD-DEFUN POINT -1 T))
              (END-DEFUN-BP (FORWARD-DEFUN POINT 1 T))
              DEPTH)
          ;; Parse it all once, then don't even bother checking.
          (LISP-PARSE-FROM-DEFUN (BP-LINE END-DEFUN-BP) START-DEFUN-BP)
          ;; Count how many levels down the next defun is from the start of this one.
          (LET ((*LISP-PARSE-PREPARSED-FLAG* T))
            (DO ((I -1 (1+ I))
                 (BP3 END-DEFUN-BP (FORWARD-SEXP BP3 -1 NIL 1 START-DEFUN-BP)))
                ((NULL BP3) (SETQ DEPTH I))))
          ;; Insert that many ")"'s just before point, so everything is balanced.
          ;; These ")"'s lie between FIXBP1 and FIXBP2.  We use that to delete them later.
          (COND ((> DEPTH 0)
                 (LET ((BP (LIKELY-UNBALANCED-POINT (FORWARD-LIST COND-BP -1 NIL 1)
                                                    END-DEFUN-BP)))
                   (SETQ FIXBP1 (COPY-BP BP :NORMAL)
                         FIXBP2 (COPY-BP BP :MOVES)))
                 (INSERT FIXBP2 #/NEWLINE)
                 (DOTIMES (I DEPTH) (INSERT FIXBP2 #/) ))
                 (INSERT FIXBP2 #/NEWLINE))))
        (COND ((EQ COND-TYPE 'COND)             ;Changing COND to AND or OR
               (LET* ((N (COUNT-LIST-ELEMENTS (FORWARD-LIST COND-BP -1 NIL 1)))
                      (AFTER-COND-SYMBOL (FORWARD-SEXP COND-BP))
                      (FIRST-CLAUSE-LENGTH (COUNT-LIST-ELEMENTS AFTER-COND-SYMBOL)))
                 (AND (> N 3) (BARF "Too many clauses"))
                 (AND (= N 3)
                      (LET ((BP1 (FORWARD-SEXP COND-BP 2)) BP2 BP3)
                        (SETQ BP2 (FORWARD-LIST BP1 1 NIL -1 T)
                              BP3 (FORWARD-WORD BP2))
                        (OR (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
                                 (STRING-EQUAL (BP-LINE BP2) "T"
                                               :START1 (BP-INDEX BP2) :START2 0
                                               :END1 (BP-INDEX BP3)))
                            (BARF "Too many clauses"))
                        (SETQ BP1 (BACKWARD-OVER '(#/CR #/TAB #/SP) BP1))
                        (SETQ BP1 (FORWARD-CHAR BP1 -1))
                        (DELETE-INTERVAL BP1 BP3 T)
                        (SETQ COND-TYPE (IF (= FIRST-CLAUSE-LENGTH 1) "OR" "IF"))))
                 (AND (> FIRST-CLAUSE-LENGTH 2)
                      (LET* ((BP2 (FORWARD-LIST AFTER-COND-SYMBOL 1 NIL -1 T))
                             (BP3 (FORWARD-SEXP (FORWARD-SEXP BP2 2) -1)))
                        (INSERT BP3 "(PROGN ")
                        (INSERT (FORWARD-SEXP AFTER-COND-SYMBOL) ")")))
                 (DELETE-INTERVAL COND-BP AFTER-COND-SYMBOL T))
               (AND (EQ COND-TYPE 'COND)        ;Still not determined
                    ;; Check for (COND ((NOT ...)))
                    (LET ((BP1 (FORWARD-LIST COND-BP 1 NIL -2 T)))
                      (LET ((BP2 (FORWARD-WORD COND-BP 1 T)))
                        (LET ((WORD (STRING-INTERVAL BP1 BP2)))
                          (COND ((OR (STRING-EQUAL WORD "NULL") (STRING-EQUAL WORD "NOT"))
                                 (SETQ BP1 (FORWARD-LIST BP1 -1 NIL 1))
                                 (LET ((BP3 (FORWARD-LIST BP1)))
                                   (DELETE-INTERVAL (FORWARD-CHAR BP3 -1) BP3 T))
                                 (DELETE-INTERVAL BP1 (FORWARD-OVER *BLANKS* BP2) T)
                                 (SETQ COND-TYPE "OR"))
                                (T
                                 (SETQ COND-TYPE "AND")))))))
               (SETQ BP (FORWARD-OVER *BLANKS* (INSERT COND-BP COND-TYPE)))
               (LET ((BP1 (FORWARD-LIST BP)))   ;Remove a level of parens
                 (DELETE-INTERVAL (FORWARD-CHAR BP1 -1) BP1 T))
               (DELETE-INTERVAL BP (FORWARD-CHAR BP) T))
              (T
               (LET ((BP1 (FORWARD-LIST (FORWARD-LIST (FORWARD-CHAR COND-BP -1))
                                        -1 NIL -1 T)))
                 (INSERT BP1 #/) )
                 (DO ((N -1 (1+ N))
                      (BP2 BP1 (FORWARD-SEXP BP2 -1))
                      (ARG (COND (*NUMERIC-ARG-P* (- 1 *NUMERIC-ARG*))
                                 ((EQ COND-TYPE 'IF) -1)
                                 (T 0))))
                     ((BP-= BP2 COND-BP)
                      (COND ((MINUSP (+ ARG N -3))
                             (DELETE-INTERVAL COND-BP (FORWARD-WORD COND-BP) T)
                             (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
                                                    (INSERT COND-BP "COND")))
                             (INSERT-MOVING BP #/()
                             (COND ((EQ COND-TYPE 'IF)
                                    (SETQ BP (FORWARD-SEXP BP 2))
                                    (INSERT-MOVING BP ")
 (T"))))
                            (T
                             (SETQ BP (INSERT COND-BP "COND (("))
                             (LET ((BP1 (IF (PLUSP ARG) (FORWARD-LIST BP 1 NIL 1)
                                            (FORWARD-SEXP BP (+ ARG N)))))
                               (INSERT BP1 #/) ))
                             (SETQ BP (FORWARD-CHAR BP -1))))
                      (COND ((EQ COND-TYPE 'OR)
                             (INSERT (FORWARD-SEXP BP) #/) )
                             (INSERT-MOVING BP "(NOT "))))))))
        (OR UPCASE-P (DOWNCASE-INTERVAL COND-BP BP T))
        (MOVE-BP POINT (FORWARD-LIST BP -1 NIL (IF (MEMQ COND-TYPE '(IF OR)) 2 1)))
        (COM-INDENT-SEXP)                       ;Regrind changed stuff
        (MOVE-BP POINT (FORWARD-LIST (FORWARD-SEXP POINT) -1 NIL -1 T)))
      (COND (FIXBP1
             (DELETE-INTERVAL FIXBP1 FIXBP2 T)
             (FLUSH-BP FIXBP1)
             (FLUSH-BP FIXBP2))))))
    DIS-TEXT)

(DEFUN FIND-CONTAINING-ATOM (BP SET)
  "Find the list around BP whose first element is a symbol STRNG-EQUAL to an elt of SET.
The first value is the BP at the front of the symbol;
the second is the element of SET.
The values are NIL if no containing list starts that way."
  (DO ((BP BP)
       (BP1) (BP2) (TEM))
      (NIL)
    (OR (SETQ BP (FORWARD-LIST BP -1 NIL 1))
        (RETURN NIL))
    (SETQ BP1 (FORWARD-LIST BP 1 NIL -1 T)
          BP2 (FORWARD-ATOM BP1))
    (AND (SETQ TEM (MEM #'STRING-EQUAL (STRING-INTERVAL BP1 BP2 T) SET))
         (RETURN (VALUES BP1 (CAR TEM))))))

(DEFUN COUNT-LIST-ELEMENTS (BP &AUX START-BP END-BP)
  "Return the number of elements in the list whose text follows BP."
  (SETQ START-BP (FORWARD-LIST BP 1 NIL -1 T))
  (SETQ END-BP (FORWARD-SEXP (DBP START-BP)))
  (DO ((BP (FORWARD-LIST BP 1 NIL -1 T) (FORWARD-SEXP BP))
       (I -1 (1+ I)))
      (NIL)
    (AND (NULL BP) (RETURN NIL))
    (AND (BP-= BP END-BP) (RETURN I))))

;;; This tries to find someplace that looks like it probably doesn't have enough parens
;;; It takes the first place that has a lesser indentation level than the given BP.
(DEFUN LIKELY-UNBALANCED-POINT (BP LIMIT-BP)
  "Return a bp to a point after BP that may not have enough parens.
The scan stops at LIMIT-BP.  Value can be LIMIT-BP."
  (DO ((IND (BP-INDENTATION BP))
       (LINE (LINE-NEXT (BP-LINE BP)) (LINE-NEXT LINE))
       (LIMIT-LINE (BP-LINE LIMIT-BP))
       (OLINE (BP-LINE BP)))
      ((EQ LINE LIMIT-LINE) LIMIT-BP)
    (WHEN (NOT (MEMQ (LINE-TYPE LINE) '(:COMMENT :BLANK)))
      (AND ( (LINE-INDENTATION LINE) IND)
           (RETURN (END-OF-LINE OLINE)))
      (SETQ OLINE LINE))))

(DEFCOM COM-FROB-DO "Interchange old and new style DO's" ()
  (ATOM-WORD-SYNTAX-BIND
   (LET (DO-BP DO-TYPE
        BP BP1 BP2 BP3)
    (MULTIPLE-VALUE (DO-BP DO-TYPE)
      (FIND-CONTAINING-ATOM (POINT) '(DO DOTIMES DOLIST)))
    (OR DO-BP (BARF))
    (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* (FORWARD-WORD DO-BP)))
    (COND ((AND (EQ DO-TYPE 'DO)
                (= (LIST-SYNTAX (BP-CH-CHAR BP)) LIST-OPEN))    ;New style
           (OR (= (COUNT-LIST-ELEMENTS BP) 1)
               (BARF "Too many DO variables"))
           (OR (SETQ BP1 (FORWARD-SEXP BP)) (BARF))
           (OR (= (COUNT-LIST-ELEMENTS BP1) 1)
               (BARF "Cannot have ending form"))
           (OR (SETQ BP2 (FORWARD-SEXP BP1)) (BARF))
           (SETQ BP3 (FORWARD-SEXP BP2 -1))
           (DELETE-INTERVAL (FORWARD-LIST BP2 -1 NIL -1 T) BP2 T)
           (MOVE-BP (POINT) (DELETE-INTERVAL (FORWARD-LIST BP1 -1 NIL -2 T)
                                             (FORWARD-LIST BP3 1 NIL -1 T) T))
           (INSERT-MOVING (POINT) #/SP)
           (DELETE-INTERVAL BP (FORWARD-LIST BP 1 NIL -2 T) T))
          (T                                    ;Old style or special
           (COND ((NEQ DO-TYPE 'DO)
                  (OR (SETQ BP1 (FORWARD-LIST BP 1 NIL -1 T)) (BARF))
                  (SETQ BP2 (FORWARD-SEXP BP1))
                  (LET ((VARNAME (STRING-INTERVAL BP1 BP2 T)))
                    (DELETE-INTERVAL BP BP1 T)
                    (COND ((EQ DO-TYPE 'DOTIMES)
                           (SETQ BP2 (FORWARD-SEXP BP))
                           (INSERT-MOVING BP2 (IN-CURRENT-FONT " 0 (1+ "))
                           (INSERT-MOVING BP2 VARNAME)
                           (INSERT-MOVING BP2 (IN-CURRENT-FONT ") ( ")))
                          ((EQ DO-TYPE 'DOLIST)
                           (SETQ BP2 (FORWARD-SEXP BP 2))
                           (INSERT-MOVING BP2 (IN-CURRENT-FONT " (CDR "))
                           (INSERT-MOVING BP2 VARNAME)
                           (INSERT-MOVING BP2 (IN-CURRENT-FONT ") (NULL "))))
                    (INSERT-MOVING BP2 VARNAME))
                  (DELETE-INTERVAL DO-BP (FORWARD-WORD DO-BP) T)
                  (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS*
                                         (INSERT DO-BP (IN-CURRENT-FONT "DO"))))))
           (OR (SETQ BP1 (FORWARD-SEXP BP 3)) (BARF))
           (DELETE-AROUND *WHITESPACE-CHARS* BP1)
           (MOVE-BP (POINT) (INSERT-MOVING BP1 (IN-CURRENT-FONT #/) )))
           (INSERT-MOVING BP1 (IN-CURRENT-FONT ")
 ("))
           (INSERT BP (IN-CURRENT-FONT "(("))
           (INSERT (FORWARD-SEXP BP1) (IN-CURRENT-FONT #/) ))
           (INDENT-INTERVAL-FOR-LISP BP BP1 T)))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE-LET-BINDING "Replace variable of LET with its value.
Point must be after or within the binding to be modified." ()
  (ATOM-WORD-SYNTAX-BIND
   (LET ((POINT (POINT))
        LET-BP BINDING-BP BP1 BP2 FROM TO)
    (OR (SETQ LET-BP (FIND-CONTAINING-ATOM POINT '(LET))) (BARF))
    (DO ((BP (FORWARD-LIST LET-BP 1 NIL -1 T) NBP)
         (NBP))
        (NIL)
      (OR (SETQ NBP (FORWARD-SEXP BP 1 NIL 0 NIL NIL T)) (BARF))
      (OR (BP-< NBP POINT) (RETURN (SETQ BINDING-BP BP))))
    (SETQ BP1 (FORWARD-LIST BINDING-BP 1 NIL -1 T)
          BP2 (FORWARD-SEXP BP1)
          FROM (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-OVER *WHITESPACE-CHARS* BP2)
          BP2 (FORWARD-SEXP BP1)
          TO (STRING-INTERVAL BP1 BP2 T))
    (SETQ BP1 (FORWARD-SEXP LET-BP 2)
          BP2 (FORWARD-SEXP BP1 1 NIL 1))
    (OR *NUMERIC-ARG-P* (PSETQ FROM TO TO FROM))
    (MOVE-BP POINT BP1)
    (LET ((*INTERVAL* (CREATE-INTERVAL BP1 BP2 T)))
      (QUERY-REPLACE POINT (INTERVAL-LAST-BP *INTERVAL*) FROM TO T))))
  DIS-TEXT)

(DEFCOM COM-QUERY-REPLACE-LAST-KILL "Replace top of kill ring with region." ()
  (LET ((POINT (POINT)) (MARK (MARK)))
    (QUERY-REPLACE POINT (INTERVAL-LAST-BP *INTERVAL*)
                   (STRING-INTERVAL (HISTORY-LATEST-ELEMENT *KILL-HISTORY*))
                   (STRING-INTERVAL MARK POINT)))
  DIS-TEXT)

(DEFCOM COM-JUST-ONE-SPACE "Replace all whitespace around point with arg spaces" ()
  (LET ((SPACES-BEFORE
          (max 0
               (- (BP-INDEX (POINT))
                          (BP-INDEX (BACKWARD-OVER *WHITESPACE-CHARS* (POINT)))))))
    (DELETE-AROUND *WHITESPACE-CHARS* (POINT))
    (DOTIMES (I *NUMERIC-ARG*)
      (INSERT (POINT) #/SP))
    (MOVE-BP (POINT) (FORWARD-CHAR (POINT) (MIN SPACES-BEFORE *NUMERIC-ARG*))))
  DIS-TEXT)

(DEFCOM COM-CANONICALIZE-WHITESPACE "Try to fixup wrong spacing heuristically.
If given an argument, or called just after a yank type command, operates
at the mark, else at point." ()
  (LET ((BP (IF (OR *NUMERIC-ARG-P* (EQ *LAST-COMMAND-TYPE* 'YANK)) (MARK) (POINT)))
        BP1 CH1 CH2 SYN1 SYN2)
    (SETQ BP (BACKWARD-OVER *BLANKS* BP)
          BP1 (FORWARD-OVER *BLANKS* BP)
          CH1 (BP-CH-CHARACTER (OR (FORWARD-CHAR BP -1) (BARF)))
          CH2 (BP-CH-CHARACTER BP1)
          SYN1 (LIST-SYNTAX CH1)
          SYN2 (LIST-SYNTAX CH2))
    (COND ((OR (EQ CH2 #/CR)                   ;If at the end of the line,
               (MULTIPLE-VALUE-BIND (STRING SLASH COMMENT)
                   (LISP-BP-SYNTACTIC-CONTEXT BP)
                 (OR STRING SLASH COMMENT))))   ;or any funny syntax, leave it alone
          ((NEQ CH1 #/CR)                      ;If not at beginning of line,
           (DELETE-INTERVAL BP BP1 T)
           (IF (AND ( SYN1 LIST-OPEN) ( SYN1 LIST-SINGLE-QUOTE)
                    ( SYN2 LIST-CLOSE))
               (INSERT BP #/SP)))
          ((NEQ CH2 #/()                       ;If not start of defun
           (INDENT-INTERVAL-FOR-LISP BP (BEG-LINE BP 1 T) T NIL T))     ;run tab
          ((DO ((LINE (LINE-PREVIOUS (BP-LINE BP)) (LINE-PREVIOUS LINE))
                (OLINE (BP-LINE BP) LINE)       ;Flush blank lines, and
                (TYPE))                         ;unless previous non-blank is a comment
               (NIL)
             (SETQ TYPE (AND LINE (LINE-TYPE LINE)))
             (UNLESS (EQ TYPE ':BLANK)
               (DELETE-INTERVAL (CREATE-BP OLINE 0) BP T)
               (RETURN (NEQ TYPE ':COMMENT))))
           (INSERT BP #/NEWLINE))))            ;leave just one in their place
  DIS-TEXT)

(DEFCOM COM-FIND-UNBALANCED-PARENTHESES "Find parenthesis error in buffer" ()
  (LET ((BEG-BP (INTERVAL-FIRST-BP *INTERVAL*))
        (END-BP (INTERVAL-LAST-BP *INTERVAL*))
        (POINT (POINT))
        (*BATCH-UNDO-SAVE* T)
        (OLD-TICK (NODE-TICK *INTERVAL*))
        BEG-BP-1 END-BP-1 BP)
    (UNWIND-PROTECT
      (PROGN
        (SETQ BEG-BP-1 (COPY-BP BEG-BP :MOVES)
              END-BP-1 (COPY-BP END-BP :NORMAL))
        (INSERT BEG-BP-1 "(
")
        (INSERT END-BP-1 "
)")
        (IF (SETQ BP (FORWARD-SEXP BEG-BP))
            (IF (BP-= BP END-BP)                ;All ok
                (FORMAT *QUERY-IO* "~&All parens appear balanced.")
              (POINT-PDL-PUSH POINT *WINDOW*)
              (MOVE-BP POINT BP)
              (FORMAT *QUERY-IO* "~&Probably extra right-paren here."))
          (OR (SETQ BP (FORWARD-SEXP END-BP -1))
              (BARF "Cannot find unbalanced parenthesis"))
          (POINT-PDL-PUSH POINT *WINDOW*)
          (MOVE-BP POINT BP)
          (FORMAT *QUERY-IO* "~&Probably no right-paren for this left-paren.")))
      (WHEN BEG-BP-1
        (DELETE-INTERVAL BEG-BP BEG-BP-1 T)
        (FLUSH-BP BEG-BP-1))
      (WHEN END-BP-1
        (DELETE-INTERVAL END-BP-1 END-BP T)
        (FLUSH-BP END-BP-1))
      (SETF (NODE-TICK *INTERVAL*) OLD-TICK)))
  DIS-BPS)

;(DEFCOM COM-DECLARE-SPECIAL "Add the nth previous word to the last special declaration
;This is totally bogus now, due to the fact that you should use (PROCLAIM '(SPECIAL ...))
;instead since Common Lisp says that top-level DECLAREs are illegal. So foo on you." ()
;  (ATOM-WORD-SYNTAX-BIND
;    (LET (WORD)
;      (LET ((BP1 (FORWARD-WORD (POINT) (- *NUMERIC-ARG*)))
;           BP2)
;       (OR BP1 (BARF))
;       (SETQ BP2 (FORWARD-WORD BP1 1))
;       (OR BP2 (BARF))
;       (SETQ WORD (STRING-INTERVAL BP1 BP2 T)))
;      (LET ((BP (BLOCK DECLARES
;                 (DO ((LINE (BP-LINE (POINT)) (LINE-PREVIOUS LINE))
;                      (LIMIT-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
;                     (NIL)
;                   (AND (STRING-EQUAL "(DECLARE " LINE 0 0 9 9)
;                        ;;Found a (DECLARE ...), look for SPECIAL in the CARs of the elements
;                        (DO ((BP1 (CREATE-BP LINE 9) (FORWARD-SEXP BP1))
;                             (BP2)
;                             (BP3))
;                            ((NULL BP1))
;                          (OR (SETQ BP2 (FORWARD-LIST BP1 1 NIL 1 T))
;                              (RETURN NIL))
;                          (OR (SETQ BP3 (FORWARD-WORD BP2)) (RETURN NIL))
;                          (AND (EQ (BP-LINE BP2) (BP-LINE BP3))
;                               (STRING-EQUAL "SPECIAL" (BP-LINE BP2) 0 (BP-INDEX BP2) 7
;                                             (BP-INDEX BP3))
;                               (SETQ BP2 (FORWARD-LIST BP1))   ;Found one
;                               (RETURN-FROM DECLARES (FORWARD-CHAR BP2 -1)))))
;                   ;;If there isnt a special declaration, make one at the start of the file
;                   (AND (EQ LINE LIMIT-LINE)
;                        (RETURN (FORWARD-CHAR (INSERT
;                                                (SKIP-OVER-BLANK-LINES-AND-COMMENTS
;                                                  (INTERVAL-FIRST-BP *INTERVAL*) T)
;                                                "(DECLARE (SPECIAL))

;")
;                                              -3)))))))
;       ;; Now put it in and try not to overflow the line
;       (WITH-BP (PT (POINT) :MOVES)            ;Preserve point
;         (MOVE-BP (POINT) BP)
;         (INSERT-MOVING (POINT) (STRING-APPEND #/SP WORD))
;         (AUTO-FILL-HOOK #/SP)
;         (COND ((END-LINE-P (POINT))
;                (MOVE-BP (POINT) (END-LINE (POINT) 1))
;                (INSERT (DELETE-BACKWARD-OVER *BLANKS* (POINT)) #/CR)
;                (COM-INDENT-FOR-LISP)))
;         (MOVE-BP (POINT) PT)))))
;  DIS-TEXT)

#|  ;; This is commented out since com-lisp-match-search is better.

;;; Pattern finding command
(DEFVAR *LAST-PATTERN* NIL)
(DEFVAR *LAST-PATTERN-BP* NIL)
(DEFVAR *LAST-PATTERN-RESTART-LIST*)

(DEFCOM COM-FIND-PATTERN "Move to next occurrence of the given pattern.
The pattern must be a list: # matches any one element, ... any number of elements.
Everything else must match exactly except for arbitrary whitespace.
A numeric argument repeats the last search." ()
  (LET (FORM RESTART BP)
    (COND (*NUMERIC-ARG-P*
           (SETQ FORM (OR *LAST-PATTERN* (BARF "No previous pattern")))
           (FORMAT *QUERY-IO* "~&Finding ~S" FORM)
           (AND (BP-= (POINT) *LAST-PATTERN-BP*) (SETQ RESTART *LAST-PATTERN-RESTART-LIST*)))
          (T
           (LET ((FORM-STRING (TYPEIN-LINE-READLINE "Pattern to search for:"))
                 (EOF '(())))
             (SETQ FORM (READ-FROM-STRING FORM-STRING EOF))
             (AND (EQ FORM EOF) (BARF "Unbalanced parens"))
             (OR (CONSP FORM) (BARF "I only know how to search for lists"))
             ;; This is sort of a kludge
             (OR (EQ *PACKAGE* SI:PKG-KEYWORD-PACKAGE)
                 (SETQ FORM (NSUBLIS (LIST (CONS (INTERN "...") ':...)
                                           (CONS (INTERN "#") ':/#)     ;what the printer says
                                           (CONS (INTERN "**") ':**))   ;same as :#
                                    FORM))))))
    (MULTIPLE-VALUE (BP RESTART) (FIND-PATTERN (POINT) FORM RESTART))
    (OR BP (BARF))
    (MAYBE-PUSH-POINT BP)
    (MOVE-BP (POINT) BP)
    (SETQ *LAST-PATTERN* FORM *LAST-PATTERN-BP* BP *LAST-PATTERN-RESTART-LIST* RESTART))
  DIS-BPS)

;;; Attempt to find an instance of THING after BP, return a new BP if successful
(DEFUN FIND-PATTERN (BP PATTERN &OPTIONAL RESTART-LIST)
  (DO ((BP1 (FORWARD-DEFUN BP -1 T) (FORWARD-DEFUN BP2))
       (BP2)
       (STREAM (INTERVAL-STREAM *INTERVAL*))
       (FORM)
       (SI:XR-CORRESPONDENCE-FLAG T)
       (SI:XR-CORRESPONDENCE NIL NIL)
       (RESTART-LIST RESTART-LIST NIL)
       (PLIST)
       (TEM))
      ((NULL BP1) NIL)
    (SETQ PLIST (LOCF (LINE-CONTENTS-PLIST (BP-LINE BP1))))
    (SETQ BP2 (FORWARD-SEXP BP1))               ;Find the end of this defun
    ;; Now get the form and correspondence for this defun, using previous if there
    (COND (BP2
           (COND ((AND (SETQ TEM (GET PLIST 'CORRESPONDENCE))
                       (COND ((> (CADR TEM) (INTERVAL-REAL-TICK BP1 BP2 T))
                              (SETQ FORM (CAR TEM) SI:XR-CORRESPONDENCE (CADDR TEM))
                              T)
                             (T
                              (REMPROP PLIST 'CORRESPONDENCE)
                              NIL))))
                 (T
                  (SEND STREAM :SET-BP BP1)
                  (SETQ FORM (READ STREAM))
                  (PUTPROP PLIST (LIST FORM (TICK) SI:XR-CORRESPONDENCE) 'CORRESPONDENCE)))
           (AND RESTART-LIST (SETQ FORM (CAR RESTART-LIST) RESTART-LIST (CDR RESTART-LIST)))
           (DO ((FORM FORM (CAR RESTART-LIST))
                (RESTART-LIST RESTART-LIST (CDR RESTART-LIST))
                (FOUND)
                (BP3))
               (NIL)
             (MULTIPLE-VALUE (FOUND RESTART-LIST)
               (FIND-FROB PATTERN FORM RESTART-LIST))
             (OR FOUND (RETURN NIL))
             (AND (SETQ BP3 (CADR (MEMQ FOUND SI:XR-CORRESPONDENCE)))
                  (BP-< BP BP3)
                  (RETURN-FROM FIND-PATTERN BP3 RESTART-LIST))))
          (T                                    ;Look forward for next defun
           (SETQ BP2 BP1)))))

;;; Attempt to find an instance of THING in LIST
(DEFUN FIND-FROB (THING LIST &OPTIONAL RESTART-LIST &AUX VAL)
  (COND ((AND RESTART-LIST (MULTIPLE-VALUE (VAL RESTART-LIST)
                             (FIND-FROB THING (CAR RESTART-LIST) (CDR RESTART-LIST))))
         (PUSH LIST RESTART-LIST))
        ((MATCH THING LIST) (SETQ VAL LIST RESTART-LIST NIL))
        ((ATOM LIST) (SETQ VAL NIL RESTART-LIST NIL))
        (T
         (DO ((LIST LIST (CDR LIST)))
             ((NULL LIST) (SETQ VAL NIL RESTART-LIST NIL))
           (MULTIPLE-VALUE (VAL RESTART-LIST) (FIND-FROB THING (CAR LIST)))
           (WHEN VAL
             (PUSH (CDR LIST) RESTART-LIST)
             (RETURN NIL)))))
  (VALUES VAL RESTART-LIST))

;;; Simple minded pattern matcher
;;; ** matches an arbitrary frob, ... an arbitrary number (possibly 0) of frobs
(DEFUN MATCH (A B)
  (DO ((A A (CDR A))
       (B B (CDR B))
       (VAL))
      (NIL)
    (COND ((EQ A B) (RETURN T))
          ((EQ A ':...) (RETURN 'CDR))
          ((EQ A ':**) (RETURN T))
          ((EQ A ':/#) (RETURN T))
          ((NOT (= (%DATA-TYPE A) (%DATA-TYPE B))) (RETURN NIL))
          ((NUMBERP A) (RETURN (= A B)))
          ((ARRAYP A) (RETURN (AND (STRINGP A) (STRINGP B) (STRING-EQUAL A B))))
          ((ATOM A) (RETURN NIL))
          ((NOT (SETQ VAL (MATCH (CAR A) (CAR B)))) (RETURN NIL))
          ((NEQ VAL T) (RETURN (OR (NULL (SETQ A (CDR A)))
                                   (DO ((B B (CDR B)))
                                       ((NULL B))
                                     (AND (MATCH A B) (RETURN T)))))))))
  |#

(DEFCOM COM-EXECUTE-COMMAND-INTO-BUFFER
        "Execute following editor command, printing into the buffer.
Any output from the command which would ordinarily appear as type out
is inserted into the current buffer instead.
Trace and warning output are also inserted in the buffer." ()
  (LET* ((*TYPEOUT-WINDOW* (MAKE-INTERVAL-TYPEOUT-STREAM))
         (*STANDARD-OUTPUT* *TYPEOUT-WINDOW*)
         (*TRACE-OUTPUT* *TYPEOUT-WINDOW*)
         (*ERROR-OUTPUT* *TYPEOUT-WINDOW*))
    (CLEAR-PROMPTS)
    (ADD-PROMPT "Key: ")
    (ALWAYS-DISPLAY-PROMPTS)
    (WITH-UNDO-SAVE ("Command output" (POINT) (POINT) T)
      (UNWIND-PROTECT
        (DO ()
            ((NEQ ':ARGUMENT
                  (PROCESS-COMMAND-CHAR (INPUT-WITH-PROMPTS *STANDARD-INPUT* :TYI)))))
        ;; Redisplay properly if command is aborted.
        (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
    (MOVE-BP (MARK) (POINT))
    (MOVE-BP (POINT) (SEND *STANDARD-OUTPUT* :READ-BP))
    (SETQ *CURRENT-COMMAND-TYPE* 'YANK))
  DIS-TEXT)

(defun insert-formatted-output (printing-function-1 &optional printing-function-2)
  ;;format output of arg1 to point if numeric arg supplied, format output of arg2 if not
  (LET ((STREAM (INTERVAL-STREAM (POINT) (POINT) T)))
    (FUNCALL (IF *NUMERIC-ARG-P* printing-function-2 printing-function-1)
             STREAM)
    (MOVE-BP (MARK) (POINT))
    (MOVE-BP (POINT) (SEND STREAM :READ-BP))))

(DEFCOM COM-INSERT-DATE "Print the current date into the buffer.
Calls TIME:PRINT-CURRENT-TIME, or if given an argument TIME:PRINT-CURRENT-DATE" ()
  (insert-formatted-output #'TIME:PRINT-CURRENT-TIME #'TIME:PRINT-CURRENT-DATE)
  DIS-TEXT)

(DEFCOM COM-INSERT-SHORT-DATE "Print the current date into the buffer.
Calls TIME:PRINT-DATE-ONLY, which prints only  the numeric date, the month name, and the year." ()
  (insert-formatted-output #'time:print-date-only #'time:print-current-time)
  DIS-TEXT)

(defcom com-insert-current-user-id "Print the currently logged in user's name into the buffer at point." ()
  (flet ((upcase-user-id-printer (stream) (format stream "~A" (string-upcase user-id)))
         (user-id-printer (stream) (format stream "~A" user-id)))
    (insert-formatted-output #'user-id-printer #'upcase-user-id-printer))
  dis-text)

(DEFCOM COM-COUNT-LINES-REGION "Print the number of lines in the region in the echo area." (KM)
  (REGION (BP1 BP2)
    (FORMAT *QUERY-IO* "~&~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T))))
  DIS-NONE)

(DEFCOM COM-WHERE-AM-I "Print various things about where the point is.
Print the X and Y positions, the octal code for the following character,
the current line number and its percentage of the total file size.
If there is a region, the number of lines in it is printed.
Fast Where Am I prints a subset of this information faster." (KM)
  (REDISPLAY *WINDOW* :POINT NIL NIL T)
  (LET ((POINT (POINT))
        (FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*))
        (LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (LET ((POINT-LINES (1- (COUNT-LINES FIRST-BP POINT)))
          (INTERVAL-LINES (1- (COUNT-LINES FIRST-BP LAST-BP)))
          (AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
          (BP-IND (BP-INDENTATION POINT))
          (SW (FONT-SPACE-WIDTH)))
      (FORMAT *QUERY-IO* "~&X=[~D chars|~D pixels|~:[~S~;~D~] columns] ~
                        Y=~D~@[ Char=#o~O~] Line=~D(~D%)"
              (BP-INDEX POINT)
              BP-IND
              (ZEROP (\ BP-IND SW))
              (IF (ZEROP (\ BP-IND SW))
                  (TRUNCATE BP-IND SW)
                  (// (FLOAT BP-IND) SW))
              (FIND-BP-IN-WINDOW *WINDOW* POINT)
              (AND (NOT AT-END-P) (BP-CHAR POINT))
              POINT-LINES
              (IF (ZEROP INTERVAL-LINES)
                  0
                  (TRUNCATE (* 100. POINT-LINES) INTERVAL-LINES)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
         (FORMAT *QUERY-IO* ", Region has ~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T)))))
  DIS-NONE)

(DEFCOM COM-FAST-WHERE-AM-I "Quickly print various things about where the point is.
Print the X and Y positions, and the octal code for the following character.
If there is a region, the number of lines in it is printed.
Where Am I prints the same things and more." (KM)
  (REDISPLAY *WINDOW* :POINT NIL NIL T)
  (LET ((POINT (POINT)))
    (LET ((AT-END-P (BP-= (INTERVAL-LAST-BP *INTERVAL*) POINT))
          (BP-IND (BP-INDENTATION POINT))
          (SW (FONT-SPACE-WIDTH)))
      (FORMAT *QUERY-IO* "~&X=[~D chars|~D pixels|~:[~S~;~D~] columns] Y=~D~@[ Char=#o~O~]"
              (BP-INDEX POINT)
              BP-IND
              (ZEROP (\ BP-IND SW))
              (IF (ZEROP (\ BP-IND SW))
                  (TRUNCATE BP-IND SW)
                  (// (FLOAT BP-IND) SW))
              (FIND-BP-IN-WINDOW *WINDOW* POINT)
              (AND (NOT AT-END-P) (BP-CHAR POINT)))))
  (AND (WINDOW-MARK-P *WINDOW*)
       (REGION (BP1 BP2)
         (FORMAT *QUERY-IO* ", Region has ~D line~:P.  " (1- (COUNT-LINES BP1 BP2 T)))))
  DIS-NONE)

(DEFCOM COM-ARGLIST "Print the argument list of the specified function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) and prints the arglist
in the echo area." ()
  (LET ((NAME (READ-FUNCTION-NAME "Arglist" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (PRINT-ARGLIST NAME))
  DIS-NONE)

(DEFCOM COM-QUICK-ARGLIST "Print the argument list of the function to left of cursor." ()
  (QUICK-ARGLIST)
  DIS-NONE)

(DEFUN QUICK-ARGLIST (&OPTIONAL (STREAM *QUERY-IO*))
  (IF *NUMERIC-ARG-P*
      (LET ((NAME (READ-FUNCTION-NAME "Arglist" (RELEVANT-FUNCTION-NAME (POINT)) T)))
        (PRINT-ARGLIST NAME STREAM))
    (LET ((SYMBOL (RELEVANT-FUNCTION-NAME (POINT))))
      (COND ((COND ((MEMQ SYMBOL '(FUNCALL FUNCALL-SELF SEND
                                           LEXPR-FUNCALL LEXPR-FUNCALL-SELF LEXPR-SEND
                                           <-)) ;crock...
                    (LET ((TEMP-SYMBOL (RELEVANT-METHOD-NAME
                                         (POINT)
                                         (IF (MEMQ SYMBOL '(FUNCALL-SELF LEXPR-FUNCALL-SELF))
                                             1 2))))
                      (AND TEMP-SYMBOL (SETQ SYMBOL TEMP-SYMBOL))))
                   ((EQ SYMBOL 'DEFMETHOD)
                    (LET ((METHOD-SYMBOL (RELEVANT-DEFMETHOD-METHOD-NAME (POINT))))
                      (COND (METHOD-SYMBOL
                             (SETQ SYMBOL METHOD-SYMBOL)
                             T)))))
             (MULTIPLE-VALUE-BIND (ARGLIST NAME RETLIST)
                 (METHOD-ARGLIST SYMBOL)
               (WHEN (EQ STREAM *QUERY-IO*)
                 (FRESH-LINE *QUERY-IO*)
                 (SEND STREAM :SEND-IF-HANDLES :TYPEOUT-STAYS))
               (FORMAT STREAM "~S: ~:A~@[  ~:A~]"
                       (OR NAME SYMBOL) ARGLIST RETLIST)))
            ((AND SYMBOL (OR (FDEFINEDP SYMBOL)
                             (FUNCTIONP SYMBOL T)
                             (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST SYMBOL))))
             (PRINT-ARGLIST SYMBOL STREAM))
            ((BARF))))))                        ;Looked hard but couldn't find a defined function

(DEFUN PRINT-ARGLIST (FSPEC &OPTIONAL (STREAM *QUERY-IO*))
  (WHEN (EQ STREAM *QUERY-IO*)
    (FRESH-LINE *QUERY-IO*)
    (SEND STREAM :SEND-IF-HANDLES :TYPEOUT-STAYS))
  (MULTIPLE-VALUE-BIND (ARGLIST RETURNS TYPE)
      (ARGLIST FSPEC)
    (FORMAT STREAM "~S~@[ (~A)~]: " FSPEC TYPE)
    (IF (CL:LISTP ARGLIST)
        (PRINT-ARGLIST-INTERNAL ARGLIST STREAM)
        (PRINC "??" STREAM))
    (AND RETURNS (FORMAT STREAM "  ~:A" RETURNS))
    (AND (NOT (FDEFINEDP FSPEC))
         (SYMBOLP FSPEC)
         (SI:MEMQ-ALTERNATED 'SI:ARGLIST (PLIST FSPEC))
         (PRINC " (only in compiled code)" STREAM))))

;; This prints an arglist in a convenient form, ie:
;; (si:first &special si:second &local &optional (si:third (quote si:default)))
;; prints: (first &special si:second &local &optional (third 'si:default))
(DEFUN PRINT-ARGLIST-INTERNAL (LIST STREAM &AUX SPECIAL)
  (WRITE-CHAR #/( STREAM)
  (DO ((L LIST (CDR L)))
      ((NULL L)
       (WRITE-CHAR #/) STREAM))
    (COND ((SYMBOLP L)
           (SEND STREAM :STRING-OUT ". ")
           (SEND (IF SPECIAL #'PRIN1 #'PRINC) L STREAM)
           (SEND STREAM :TYO #/) )
           (RETURN NIL)))
    (CASE (CAR L)
      (&SPECIAL (SETQ SPECIAL T))
      (&LOCAL (SETQ SPECIAL NIL)))
    (COND ((OR (ATOM (CAR L))                   ;If the element is a symbol
               (ATOM (CDAR L))                  ;Or if it's not a list with exactly two elmts.
               (NOT (NULL (CDDAR L))))
           (FUNCALL (IF SPECIAL #'PRIN1 #'PRINC) (CAR L) STREAM))       ;Just print it.
          (T ;; This is the special case of an element with a default.
             (WRITE-CHAR #/( STREAM)
             (SEND (IF SPECIAL #'PRIN1 #'PRINC) (CAAR L) STREAM)
             (WRITE-CHAR #/SP STREAM)
             (COND ((EQ (CAR-SAFE (CADAR L)) 'QUOTE)
                    ;; If the default is quoted, print it nicely.
                    (WRITE-CHAR #/' STREAM)
                    (PRIN1 (CADR (CADAR L)) STREAM))
                   ((EQ (CADAR L) 'SI:*HAIRY*)
                    (SEND STREAM :STRING-OUT "??"))
                   (T (PRIN1 (CADAR L) STREAM)))
             (WRITE-CHAR #/) STREAM)))
    (IF (CDR L) (WRITE-CHAR #/SP STREAM))))

(DEFCOM COM-QUICK-DOCUMENTATION "Prints documentation for the function which point is inside a call to.
At the top level of a buffer, prints documentation for the top-level form being defined.
With a numeric argument, reads the name of the function to be documented
from the mini-buffer." ()
    (LET ((NAME (RELEVANT-FUNCTION-NAME (POINT))))
      (IF *NUMERIC-ARG-P*
          (SETQ NAME (READ-FUNCTION-NAME "Document" NAME T)))
      (IF (NULL NAME) (BARF "No function at point.")
        (flet ((print-args (stream)
                (IF (OR (FDEFINEDP NAME) (AND (SYMBOLP NAME) (FUNCTIONP NAME T)))
                    (PROGN (SEND stream :FRESH-LINE)
                           (PRINT-ARGLIST NAME stream)))))
          (LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
            (COND ((NULL DOC)
                   (print-args *query-io*)
                   (FORMAT *QUERY-IO* "~&~S is not documented as a function." NAME))
                  (T
                   (print-args *standard-output*)
                   (FORMAT T "~%~A" DOC)))))))
    DIS-NONE)

(DEFCOM COM-BRIEF-DOCUMENTATION "Prints brief documentation for the specified function.
Reads the name of the function from the mini-buffer (the default is
the /"current/" function from the buffer) and prints the first
line of its documentation in the echo area." ()
    (LET ((NAME (READ-FUNCTION-NAME "Brief Document" (RELEVANT-FUNCTION-NAME (POINT)) T)))
      (IF (NULL NAME) (BARF)
        (LET ((DOC (DOCUMENTATION NAME 'FUNCTION)))
          (COND ((NULL DOC) (FORMAT *QUERY-IO* "~&~S is not documented as a function" NAME))
                (T (FORMAT *QUERY-IO* "~&~S: ~A" NAME
                           (SUBSTRING DOC 0 (STRING-SEARCH-CHAR #/CR DOC))))))))
    DIS-NONE)

(DEFCOM COM-LONG-DOCUMENTATION "Prints long documentation for the specified symbol or function.
Reads the name of the function or symbol from the mini-buffer
/(the default is the /"current/" function from the buffer).
First comes the arglist of the name as a function, if it is defined,
and then the documentation of the function.
Then, if the name is a symbol, comes the documentation for the name in its other roles." ()
  (LET ((NAME (READ-FUNCTION-NAME "Document" (RELEVANT-FUNCTION-NAME (POINT)))))
    (LET ((DOC (DOCUMENTATION NAME 'FUNCTION))
          (ALL-DOC (AND (SYMBOLP NAME) (GET NAME 'SI::DOCUMENTATION-PROPERTY))))
      (IF (NOT (FDEFINEDP NAME))
          (FORMAT T "~S:" NAME)
        (SEND *STANDARD-OUTPUT* :FRESH-LINE)
        (PRINT-ARGLIST NAME *STANDARD-OUTPUT*))
      (COND (DOC
             (FORMAT T "~&~A" DOC))
            (ALL-DOC ; use FUNCTION type here if no DOC (ucoded function, usually)
             (let ((docstring (cdr (assoc "FUNCTION" all-doc))))
               (when docstring
                 (format t "~&~A" docstring)))))
      (dolist (d all-doc)
        (let ((kind (car d)))
          (unless (string= kind "FUNCTION")
            (when (cdr d)
              (FORMAT T "~&~%Documentation of ~S as a ~(~A~):~%~A~%"
                      NAME kind (cdr d))))))
      (unless (or doc all-doc)
        (FORMAT *QUERY-IO* "~&~S is not documented." NAME))))
  DIS-NONE)

(DEFCOM COM-DESCRIBE-VARIABLE-AT-POINT "Prints information about variable at or before cursor.
The information printed is whether it is declared special, whether it has a value,
and whether it has documentation put on by DEFVAR.  If none of these are present,
looks for lookalike symbols in other packages." ()
  (LET* ((BP1 (FORWARD-ATOM (FORWARD-CHAR (POINT) 1 T) -1 T))
         (BP2 (FORWARD-ATOM BP1)))
    (IF (NULL BP2) (BARF))
    (MULTIPLE-VALUE-BIND (VAR ERROR)
        (CATCH-ERROR (WITH-INPUT-FROM-STRING (S (BP-LINE BP1) :start (BP-INDEX BP1) :end (BP-INDEX BP2))
                       (READ S)))
      (IF (OR ERROR (NOT (SYMBOLP VAR))) (BARF))
      (OR (DESCRIBE-VARIABLE-INTERNAL VAR)
          (DOLIST (SYM (PACKAGE-LOOKALIKE-SYMBOLS VAR NIL
                                                  '(SPECIAL COMPILER:SYSTEM-CONSTANT)))
            (IF (DESCRIBE-VARIABLE-INTERNAL SYM)
                (RETURN T)))
          (PROGN
            (SEND *QUERY-IO* :CLEAR-WINDOW)                     ;Clear the echo area
            (SEND *QUERY-IO* :SEND-IF-HANDLES :TYPEOUT-STAYS)
            (FORMAT *QUERY-IO* "~&~S is not a declared variable." VAR)))
      DIS-NONE)))

(DEFUN DESCRIBE-VARIABLE-INTERNAL (VAR)
  (LET* ((DECL (GETL VAR '(SPECIAL SYS:SYSTEM-CONSTANT)))
         (BOUND (BOUNDP VAR))
         (DOC (DOCUMENTATION VAR 'VARIABLE))
         (STREAM (IF DOC *STANDARD-OUTPUT* *QUERY-IO*)))
    (COND ((OR DECL BOUND DOC)
           (IF BOUND
               (FORMAT STREAM "~&~S has a value" VAR)
             (FORMAT STREAM "~&~S is void" VAR))
           (IF (EQ (CAR DECL) 'SPECIAL)
               (FORMAT STREAM " and is declared special ~:[by file ~A~]"
                       (EQ (CADR DECL) T) (CADR DECL)))
           (IF (EQ (CAR DECL) 'SYS:SYSTEM-CONSTANT)
               (FORMAT STREAM " and is marked as constant ~:[by file ~A~]"
                       (EQ (CADR DECL) T) (CADR DECL)))
           (IF DOC
               (FORMAT STREAM "~%~A" DOC))
           T))))

(DEFCOM COM-TRACE "Trace or untrace a function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer) then pops up a menu
of trace options.  With an argument, omits menu step" ()
  (LET ((FCN (READ-FUNCTION-NAME "Trace" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (IF (NOT *NUMERIC-ARG-P*)
        (TV:TRACE-VIA-MENUS FCN)
        (EVAL (IF (ATOM FCN) `(TRACE (,FCN)) `(TRACE (:FUNCTION ,FCN))))))
  DIS-NONE)

(DEFCOM COM-UNTRACE "Untrace a function.
Reads the name of the function from the mini-buffer (the top of the kill
ring has the /"current/" function from the buffer). " ()
  (LET ((FCN (READ-FUNCTION-NAME "Untrace" (RELEVANT-FUNCTION-NAME (POINT)) T)))
    (EVAL `(UNTRACE ,FCN) ))
  DIS-NONE)

(DEFCOM COM-WHERE-IS-SYMBOL "Show which packages contain the specified symbol." ()
  (MULTIPLE-VALUE-BIND (SYMBOL NAME)
      (READ-FUNCTION-NAME "Where is symbol" NIL NIL T)
    (WHERE-IS (OR NAME SYMBOL)))
  DIS-NONE)

(DEFCOM COM-COUNT-LINES-PAGE "Type number of lines on this page.
Also add, in parentheses, the number of lines on the page
before point, and the number of lines after point." (KM)
   (LET* ((POINT (POINT))
          (PAGE-START (FORWARD-PAGE POINT -1 T))
          (PAGE-END (FORWARD-PAGE PAGE-START 1 T)))
     (LET ((N1 (1- (COUNT-LINES PAGE-START POINT)))
           (N2 (1- (COUNT-LINES POINT PAGE-END))))
       (FORMAT *QUERY-IO* "~&Page has ~D (~D + ~D) lines" (+ N1 N2) N1 N2)))
   DIS-NONE)

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FUNCTION-NAME "Disassemble" DO-DISASSEMBLE
                          NIL "Disassemble this function.")

(DEFCOM COM-DISASSEMBLE "Disassemble the specified function." ()
  (DO-DISASSEMBLE (READ-FUNCTION-NAME "Disassemble" (RELEVANT-FUNCTION-NAME (POINT)) T))
  DIS-NONE)

(DEFCOM COM-QUICK-DISASSEMBLE "Disassemble the function to the left of the cursor." ()
  (IF *NUMERIC-ARG-P*
      (DO-DISASSEMBLE (READ-FUNCTION-NAME "Disassemble" (RELEVANT-FUNCTION-NAME (POINT)) T))
      (DO-DISASSEMBLE (RELEVANT-FUNCTION-NAME (POINT))))
  DIS-NONE)

(DEFUN DO-DISASSEMBLE (SYMBOL &AUX FSPEC)
  (COND ((NULL SYMBOL) (BARF))
        ((FDEFINEDP SYMBOL)
         (SETQ FSPEC (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC SYMBOL)))
         (COND ((OR (= (%DATA-TYPE FSPEC) DTP-FEF-POINTER)
                    (AND (CONSP FSPEC)
                         (EQ (CAR FSPEC) 'MACRO)
                         (= (%DATA-TYPE (CDR FSPEC)) DTP-FEF-POINTER)))
                (FORMAT T "~&~S:" SYMBOL)
                (DISASSEMBLE SYMBOL))
               ((BARF "Can't find FEF for ~S" SYMBOL))))
        ((BARF)))
  NIL)

(DEFCOM COM-UNDEFUN "UNDEFUN the specified function." ()
  (UNDEFUN (READ-FUNCTION-NAME "Undefun" (RELEVANT-FUNCTION-NAME (POINT)) T))
  DIS-NONE)

(DEFUN USER:TEACH-ZMACS ()
  "Run the ZMacs tutorial."
  (ED (TEACH-ZMACS-1)))

(DEFUN TEACH-ZMACS-1 ()
  (LET ((TEACH-FILE-NAME (FS:MERGE-PATHNAME-DEFAULTS "Teach-ZMacs" (FS:USER-HOMEDIR) :TEXT)))
    (UNLESS (PROBEF TEACH-FILE-NAME)
      (COPY-FILE "SYS: ZWEI; TEACH-ZMACS TEXT" TEACH-FILE-NAME))
    TEACH-FILE-NAME))

(DEFCOM COM-TEACH-ZMACS "Set up the ZMacs tutorial.
Makes a private copy of the ZMacs tutorial file in the user's home directory
unless one is already there, then reads it into the editor and selects it." ()
  (FIND-FILE (TEACH-ZMACS-1))
  DIS-TEXT)

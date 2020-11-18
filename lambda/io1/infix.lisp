;;;-*- Mode:LISP; Package:SI; Fonts:(CPTFONT TR12 TR12I); Base:8; readtable: ZL -*-

#|

1You can now include infix expressions in your Lisp code.
For example,*
   #X:Y+CAR(A1[I,J])
1is equivalent to*
   (SETQ X (+ Y (CAR (AREF A1 I J))))

#1 begins an infix expression, and *1 ends it.

The atomic terms of infix expressions include

      symbols: use *\1 to quote special characters.

      numbers: any valid Lisp real or imaginary number is accepted.
        Complex numbers can be constructed by addition or subtraction.

      strings: the same as in ordinary Lisp syntax.

      raw Lisp data: *!1 followed by any Lisp expression, as in
          *# FOO . !(CAR BAR)  1 *=> 1 *(LIST* FOO (CAR BAR))

1Combining operations:

Highest precedence*
        2a* [ 2i* ]         (AREF 2a* 2i*)
        2a* [ 2i*, 2j* ]  (AREF 2a* 2i* 2j*)        1and so on
  examples*
        X[I,J+3]  ==>  (AREF X (+ J 3))
      (GET-MY-ARRAY(FOO))[I]  ==>  (AREF (GET-MY-ARRAY FOO) I)

        2f* ( 2a* ) (2f* 2a*)
        2f* ( 2a*, 2b* )  (2f* 2a* 2b*)             1and so on
  examples*
        CAR(X)  ==>  (CAR X)

        ( 2exp* )     2exp*                 1parentheses control order of evaluation.
  examples*
        (X+1)*Y  ==>  (* (+ X 1) Y)

        ( 2e1*, 2e2* )      (PROGN 2e1* 2e2*)   1and so on
  examples*
        (X:5, X*X)  ==>  (PROGN (SETQ X 5) (* X X))

        [ 2elt* ]     (LIST 2elt*)
        [ 2e1*, 2e2* ]      (LIST 2e1* 2e2*)    1and so on
  examples*
        [!'X,Y,Z]  ==>  (LIST 'X Y Z)

1Precedence* 1801 on left, *201 on right*
        2a* : 2b*           (SETF 2a* 2b*)
1  examples*
        X: 1 + Y: Z+5  ==>  (SETQ X (+ 1 (SETQ Y (+ Z 5))))

1Precedence* 140
        2a* ^ 2b*           (EXPT 2a* 2b1)**  1right associative
  examples*
        X ^ N ^ 2       (EXPT X (EXPT N 2))

1Precedence* 120
        2a* * 2b*           (* 2a* 2b*)
        2a* * 2b* * 2c*   (* 2a* 2b* 2c*)   1and so on*
        2a* / 2b*           (// 2a* 2b*)
        2a* / 2b* / 2c*   (// 2a* 2b* 2c*)  1and so on

Precedence* 100
        - 2a*         (- 2a*)
        2a* + 2b*           (+ 2a* 2b*)
        2a* + 2b* + 2c*   (+ 2a* 2b* 2c*)   1and so on*
        2a* - 2b*           (- 2a* 2b*)
        2a* - 2b* - 2c*   (- 2a* 2b* 2c*)   1and so on

Precedence* 95
        2a* . 2b*           (LIST* 2a* 2b*)
        2a* . 2b* . 2c*   (LIST* 2a* 2b* 2c*)       1and so on*
        2a* @ 2b*           (APPEND 2a* 2b*)
        2a* @ 2b* @ 2c*   (APPEND 2a* 2b* 2c*)      1and so on

Precedence* 80
        2a*  2b*          (MEMQ 2a* 2b*)
        2a* = 2b*           (= 2a* 2b*)
        2a* = 2b* = 2c*   (= 2a* 2b* 2c*)   1and so on*
        <, >, , ,  1are like* =.

1Precedence* 70
        NOT 2a*               (NOT 2a*)

1Precedence* 60
        2a* AND 2b*         (AND 2a* 2b*)
        2a* AND 2b* AND 2c*       (AND 2a* 2b* 2c*) 1and so on

Precedence* 50
        2a* OR 2b*          (OR 2a* 2b*)
        2a* OR 2b* OR 2c* (OR 2a* 2b* 2c*)  1and so on

Precedence *451 for 2c*, *251 for 2a* and 2b*.*
        IF 2c* THEN 2a*             (IF 2c* 2a*)
        IF 2c* THEN 2a* ELSE 2b1 **     (IF 2c* 2a* 2b*)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;     Based on a theory of parsing presented in:                       ;;;
;;;                                                                      ;;;
;;;         Pratt, Vaughan R., ``Top Down Operator Precedence,''         ;;;
;;;         ACM Symposium on Principles of Programming Languages         ;;;
;;;         Boston, MA; October, 1973.                                   ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; On this page is the tokenizer.  The next page is the parser.
;;; After that come the operator definitions.

;;; Macros and functions used by the tokenizer loop.

(DEFUN INFIX-TYI ()
  (MULTIPLE-VALUE-BIND (NIL NIL CH)
      (XR-XRTYI *STANDARD-INPUT* NIL T)
    CH))
(DEFSUBST INFIX-UNTYI (CH) (SEND *STANDARD-INPUT* ':UNTYI CH))

(DEFUN INFIX-TYIPEEK ()
  (LET ((CH (INFIX-TYI)))
    (PROG1 CH (SEND *STANDARD-INPUT* ':UNTYI CH))))

(DEFMACRO INFIX-RETURN-TOKEN (C STRING)
  `(PROGN ,(IF C `(INFIX-UNTYI ,C))
          (RETURN (IF NUMBER-SO-FAR (READ-FROM-STRING ,STRING)
                    (IF PREVIOUS-LOOKUP (CDR PREVIOUS-LOOKUP)
                      (INTERN STRING))))))

(DEFSUBST INFIX-WHITESPACEP (C)
  (MEMQ C '(#/SP #/RETURN #/LINEFEED #/TAB #/PAGE)))

(DEFUN INFIX-DIGITP (X) (NOT (OR (< X #/0) (> X #/9))))

;;;; First step: The tokenizer.

(DEFVAR INFIX-TOKEN-TABLE NIL)
;;; The tokenizer is a simple loop with the character TYI'd pushed on the
;;; token buffer after a series of special cases are checked.
;;; The boolean state variables could be replaced with predicates
;;; that look back on what in in the buffer, however the present implementation
;;; is highly straightforward.

(DEFUN INFIX-READ-TOKEN ()
  (DO ((C (INFIX-SKIP-WHITESPACE) (INFIX-TYI))
       (STRING (MAKE-ARRAY #o40 ':TYPE ART-STRING ':FILL-POINTER 0))
       TEM
       (THIS-QUOTED-P NIL NIL)
       (QUOTED-P NIL)
       (NUMBER-SO-FAR T)
       (PREVIOUS-LOOKUP NIL)
       )
      (())
    (COND ((NULL C)
           (IF (EQUAL STRING "")
               (PROGN
                 (CERROR ':NO-ACTION NIL
                         'SYS:READ-END-OF-FILE
                         "End of file on ~S within infix expression."
                         *STANDARD-INPUT*)
                 (RETURN '))
             (INFIX-RETURN-TOKEN C STRING)))
          ((OR (INFIX-WHITESPACEP C)
               (EQ C #/))
           (IF (EQUAL STRING "")
               (RETURN ')
             (INFIX-RETURN-TOKEN C STRING)))
          ((= C #/!)
           (IF (EQUAL STRING "")
               (PROGN (INFIX-UNTYI #/!)
                      (RETURN '/!))
             (INFIX-RETURN-TOKEN C STRING)))
          ((= C #/\)
           (SETQ QUOTED-P T THIS-QUOTED-P T NUMBER-SO-FAR NIL)
           (SETQ C (INFIX-TYI)))
          ((= C #/")
           (IF (EQUAL STRING "")
               (PROGN (INFIX-UNTYI C)
                      (RETURN (READ)))
             (INFIX-RETURN-TOKEN C STRING))))
    (ARRAY-PUSH-EXTEND STRING
                       (IF THIS-QUOTED-P C (CHAR-UPCASE C)))
    (WHEN NUMBER-SO-FAR
      (UNLESS (INFIX-NUMBER-TOKEN-P STRING)
        (COND ((= (CHAR-UPCASE C) #/E)
               (IF (EQ NUMBER-SO-FAR T)
                   (SETQ NUMBER-SO-FAR 'E)
                 (SETQ NUMBER-SO-FAR NIL)))
              (( #/A (CHAR-UPCASE C) #/Z)
               (SETQ NUMBER-SO-FAR NIL))
              ((AND (MEMQ C '(#/+ #/-))
                    (EQ NUMBER-SO-FAR 'E))
               (SETQ NUMBER-SO-FAR 'ESIGN))
              ((EQUAL STRING ".")
               (IF (INFIX-DIGITP (INFIX-TYIPEEK))
                   (SETQ NUMBER-SO-FAR T)
                 (RETURN '/.)))
              ((= (LENGTH STRING) 1)
               (SETQ NUMBER-SO-FAR NIL))
              (T
               (ARRAY-POP STRING)
               (INFIX-RETURN-TOKEN C STRING)))))
    (WHEN (AND (NOT PREVIOUS-LOOKUP)
               (NOT NUMBER-SO-FAR)
               (NOT THIS-QUOTED-P)
               ( (LENGTH STRING) 1)
               (DOLIST (ELT INFIX-TOKEN-TABLE)
                 (WHEN (= (AREF (CAR ELT) 0) C)
                   (RETURN T))))
      (ARRAY-POP STRING)
      (INFIX-RETURN-TOKEN C STRING))
    (UNLESS QUOTED-P
      (SETQ TEM NIL)
      (DOLIST (ELT INFIX-TOKEN-TABLE)
        (IF (STRING-EQUAL (CAR ELT) STRING :start1 0 :start2 0 :end1 (LENGTH STRING))
            (RETURN (SETQ TEM ELT))))
      (AND (NULL TEM) PREVIOUS-LOOKUP
           (PROGN (INFIX-UNTYI C) (RETURN (CDR PREVIOUS-LOOKUP))))
      (SETQ PREVIOUS-LOOKUP TEM))))

;;; Skip past whitespace and comments.
;;; Return the first nonwhite charater not in a comment.
(DEFUN INFIX-SKIP-WHITESPACE ()
  (DO ((COMMENTP ())(C))
      (())
    (SETQ C (INFIX-TYI))
    (COND ((NULL C) (RETURN C))
          ((= C #/%)
           (SETQ COMMENTP (NOT COMMENTP)))
          ((INFIX-WHITESPACEP C))
          ((NOT COMMENTP)
           (RETURN C)))))

;;; Make an entry for TOKEN (a symbol) in our token-table.
(DEFUN INFIX-PUTTOK (TOKEN &AUX (STRING (GET-PNAME TOKEN)) LETTERS NONLETTERS)
  (DOTIMES (I (LENGTH STRING))
    (IF ( #/A (AREF STRING I) #/Z)
        (SETQ LETTERS T)
      (SETQ NONLETTERS T)))
  (IF LETTERS
      (WHEN NONLETTERS (FERROR NIL "Invalid infix token ~S defined." TOKEN))
    (UNLESS (ASSOC (GET-PNAME TOKEN) INFIX-TOKEN-TABLE)
      (PUSH (CONS (GET-PNAME TOKEN) TOKEN) INFIX-TOKEN-TABLE))))

(DEFUN INFIX-NUMBER-TOKEN-P (STRING)
  ;; its more efficient to determine the type of
  ;; the token by collecting information in state variables
  ;; as it is read. However we aren't that sure of our book-keeping.
  ;; This way we accept whatever the reader does.
  (IGNORE-ERRORS
    (MULTIPLE-VALUE-BIND (VALUE END-POS)
        (READ-FROM-STRING STRING)
      (AND (NUMBERP VALUE)
           (>= END-POS (LENGTH STRING))))))

;;;; The actual parser.

(DEFVAR INFIX-TOKEN NIL
  "The token waiting to be examined, in parsing an infix expression.")

(DEFVAR INFIX-LAST-TOKEN NIL
  "While invoking a token's handlers, this is that token.
INFIX-TOKEN will be the following token.")

(DEFCONST INFIX-NUDL '(INFIX-START-EXP-FUNCTION))
(DEFCONST INFIX-LEDL '(INFIX-CONTINUE-EXP-FUNCTION))
(DEFCONST INFIX-LBPL '(INFIX-LEFT-BINDING-POWER))

(DEFUN INFIX-TOPLEVEL-PARSE (*STANDARD-INPUT* IGNORE IGNORE)
  (LET ((INFIX-TOKEN (INFIX-READ-TOKEN)))
    (INFIX-PARSE -1)))

(DEFUN INFIX-TEST (STRING)
  (WITH-INPUT-FROM-STRING (*STANDARD-INPUT* STRING)
    (LET ((INFIX-TOKEN (INFIX-READ-TOKEN)))
      (INFIX-PARSE -1))))

(DEFUN INFIX-PARSE (RIGHT-BINDING-POWER)
  "Reads and returns one expression, using right binding power RIGHT-BINDING-POWER.
Encountering a token whose left binding power is  RIGHT-BINDING-POWER
causes us to return, leaving that token ungobbled."
  (DO ((TRANSLATION
         ;; Process a token that begins an expression (or should do so).
         (LET ((START-EXP-FUNCTION (INFIX-GETDEN INFIX-NUDL))
               (LEFT-BINDING-POWER (INFIX-GETDEN INFIX-LBPL)))
           (IF START-EXP-FUNCTION
               (PROGN
                 (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))
                 (FUNCALL START-EXP-FUNCTION))
             (IF LEFT-BINDING-POWER
                 (PROGN
                   (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
                           "Missing token in infix expression before /"~A/"."
                           INFIX-TOKEN)
                   (RETURN NIL))
               (PROG1 INFIX-TOKEN
                      (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))))))
         ;; Process a token that extends an expression.
         (LET ((CONTINUE-EXP-FUNCTION (INFIX-GETDEN INFIX-LEDL)))
           (IF (NULL CONTINUE-EXP-FUNCTION)
               (PROGN
                 (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
                         "/"~A/" with left-argument in infix expression."
                         INFIX-TOKEN)
                 (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))
                 TRANSLATION)                   ;Ignore this token.
             (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))
             (FUNCALL CONTINUE-EXP-FUNCTION TRANSLATION)))))
      (())
    (WHEN ( RIGHT-BINDING-POWER (OR (INFIX-GETDEN INFIX-LBPL) 0))
      (RETURN TRANSLATION))))

(DEFUN INFIX-GETDEN (INDL)
  (AND (SYMBOLP INFIX-TOKEN)
       (CADR (GETL INFIX-TOKEN INDL))))

(DEFUN INFIX-PARSE-LIST (RIGHT-BINDING-POWER SEPARATOR-TOKEN END-TOKEN)
  "Reads a list of expressions, using RIGHT-BINDING-POWER for each one.
We expect expressions in the list to be separated by tokens EQ to SEPARATOR-TOKEN
and the list to be terminated by a token EQ to END-TOKEN.
The END-TOKEN is gobbled.
If END-TOKEN is NIL, we accept any ending token and don't gobble it."
  (DO (ACCUM
       (FIRST T NIL))
      (())
    (SELECT INFIX-TOKEN
      (SEPARATOR-TOKEN
       (SETQ INFIX-TOKEN (INFIX-READ-TOKEN)))
      (END-TOKEN
       (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))
       (RETURN (NREVERSE ACCUM)))
      (T (UNLESS FIRST
           (IF END-TOKEN
               (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1
                       "/"~A/" read in infix expression where /"~A/" or /"~A/" was expected."
                       INFIX-TOKEN SEPARATOR-TOKEN END-TOKEN))
           (RETURN (NREVERSE ACCUM)))))
    (PUSH (INFIX-PARSE RIGHT-BINDING-POWER) ACCUM)))

;;;; Operator-defining macros.

(DEFMACRO DEFINFIX (TOKEN LEFT-BINDING-POWER (ARG) &BODY BODY)
  "Define TOKEN (a symbol) as an infix operator for infix expression parsing.
LEFT-BINDING-POWER is a measure of precedence on the left.
ARG is a symbol used in BODY to refer to the expression on the left.
BODY should parse the arguments on the right
and return the expression that contains this operation.
Example:
   (DEFINFIX + 50 (LEFT)
     `(+ ,LEFT ,(INFIX-PARSE 51)))
51 is used within to make it left-associative.
47 would be right-associative."
  `(PROGN (INFIX-PUTTOK ',TOKEN)
          (DEFPROP ,TOKEN ,LEFT-BINDING-POWER INFIX-LEFT-BINDING-POWER)
          (DEFUN (,TOKEN INFIX-CONTINUE-EXP-FUNCTION) (,ARG)
            . ,BODY)))

(DEFMACRO DEFPREFIX (TOKEN &BODY BODY)
  "Define TOKEN (a symbol) as an prefix operator for infix expression parsing.
BODY should parse the arguments on the right
and return the expression that contains this operation.
Example:
   (DEFPREFIX - `(- ,(INFIX-PARSE 1000)))"
  `(PROGN (INFIX-PUTTOK ',TOKEN)
          (DEFUN (,TOKEN INFIX-START-EXP-FUNCTION) ()
            . ,BODY)))

(DEFMACRO DEFDELIMITER (TOKEN)
  "Define TOKEN (a symbol) as a delimiter token for infix expression parsing.
This token has no syntax assigned to it; other tokens' definitions
will check explicitly for encountering this token.
This is used for comma and close parenthesis."
  `(PROGN (INFIX-PUTTOK ',TOKEN)
          (DEFPROP ,TOKEN 0 INFIX-LEFT-BINDING-POWER)))

(DEFMACRO DEFREPINFIX (TOKEN LEFT-BINDING-POWER
                       &OPTIONAL (RIGHT-BINDING-POWER LEFT-BINDING-POWER)
                       (FUNCTION TOKEN))
  "Define TOKEN as a multi-argument infix operator for infix expression parsing.
TOKEN is also used by default as the function for the expression to call,
 unless you override that by specifying a FUNCTION.
Example:   (DEFREPINFIX + 50) makes A+B+C parse into (+ A B C).
RIGHT-BINDING-POWER better be greater than or equal to LEFT-BINDING-POWER;
it defaults to be equal."
  `(DEFINFIX ,TOKEN ,LEFT-BINDING-POWER (LEFT)
     `(,',FUNCTION ,LEFT . ,(INFIX-PARSE-LIST ,RIGHT-BINDING-POWER ',TOKEN NIL))))

;;;; Definitions of operators.

(DEFPROP  -1 INFIX-LEFT-BINDING-POWER)

(DEFUN (/( INFIX-START-EXP-FUNCTION) ()
  `(PROGN . ,(INFIX-PARSE-LIST 0 '/, '/))))

(DEFINFIX /[ 200. (LEFT)
  `(AREF ,LEFT . ,(INFIX-PARSE-LIST 0 '/, '/])))

(DEFUN (/[ INFIX-START-EXP-FUNCTION) ()
  `(LIST . ,(INFIX-PARSE-LIST 0 '/, '/])))

(DEFINFIX /( 200. (LEFT)
  `(,LEFT . ,(INFIX-PARSE-LIST 0 '/, '/))))

(DEFDELIMITER /))
(DEFDELIMITER /,)
(DEFDELIMITER /])

(DEFINFIX /: 180. (LEFT)
  `(SETF ,LEFT ,(INFIX-PARSE 20.)))

(DEFINFIX /^ 140. (LEFT)
  `(^ ,LEFT ,(INFIX-PARSE 139.)))

(DEFREPINFIX * 120.)
(DEFREPINFIX // 120.)
(DEFREPINFIX CLI:// 120.)

(DEFREPINFIX + 100.)
(DEFREPINFIX - 100.)
(DEFPREFIX - `(- ,(INFIX-PARSE 100.)))
(DEFPREFIX + (INFIX-PARSE 100.))

(DEFINFIX /. 95. (LEFT)
  (LET ((RIGHT (INFIX-PARSE 94.)))
    (IF (AND (CONSP RIGHT)
             (EQ (CAR RIGHT) 'LIST*))
        `(LIST* ,LEFT . ,(CDR RIGHT))
      `(LIST* ,LEFT ,RIGHT))))

(DEFINFIX /@ 95. (LEFT)
  (LET ((RIGHT (INFIX-PARSE 94.)))
    (IF (AND (CONSP RIGHT)
             (EQ (CAR RIGHT) 'APPEND))
        `(APPEND ,LEFT . ,(CDR RIGHT))
      `(APPEND ,LEFT ,RIGHT))))

(DEFINFIX / 80. (LEFT)
  `(MEMQ ,LEFT ,(INFIX-PARSE 79.)))

(DEFREPINFIX < 80.)
(DEFREPINFIX > 80.)
(DEFREPINFIX = 80.)
(DEFREPINFIX  80.)
(DEFREPINFIX  80.)
(DEFREPINFIX  80.)

(DEFPREFIX NOT `(NOT ,(INFIX-PARSE 70.)))

(DEFREPINFIX AND 60.)
(DEFREPINFIX OR 50.)

;; RBP of ":" (assignment) is 20.

(DEFDELIMITER THEN)
(DEFDELIMITER ELSE)

(DEFPREFIX IF NIL
  (LET ((COND-FORM (INFIX-PARSE 45.)))
    (IF (EQ INFIX-TOKEN 'THEN)
        (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))
      (CERROR ':NO-ACTION NIL 'SYS:READ-ERROR-1 "No THEN in an infix IF expression."))
    (LET ((THEN-FORMS (INFIX-PARSE-LIST 25. '/, NIL))
          ELSE-FORMS)
      (WHEN (EQ INFIX-TOKEN 'ELSE)
         (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))
         (SETQ ELSE-FORMS (INFIX-PARSE-LIST 25. '/, NIL)))
      (COND ((NULL ELSE-FORMS)
             `(WHEN ,COND-FORM . ,THEN-FORMS))
            ((NULL THEN-FORMS)
             `(UNLESS ,COND-FORM . ,ELSE-FORMS))
            ((NULL (CDR THEN-FORMS))
             `(IF ,COND-FORM ,(CAR THEN-FORMS) . ,ELSE-FORMS))
            (T
             `(IF ,COND-FORM (PROGN . ,THEN-FORMS) . ,ELSE-FORMS))))))

(DEFPREFIX /! NIL
  ;; Reading ! as a token UNTYI's it.  So flush the UNTYI'd one.
  (INFIX-TYI)
  (PROG1 (READ)
         ;; Read in the token that follows the !'d s-expression.
         (SETQ INFIX-TOKEN (INFIX-READ-TOKEN))))

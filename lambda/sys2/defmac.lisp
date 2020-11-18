;;; DEFMACRO -*- Mode:LISP; Package:SI; Readtable:ZL; Base:8 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Note: differences from Common Lisp specs:
;;;  except at the top level of destructuring, all args are optional.
;;;  Thus, if the arglist is (X (Y Z &OPTIONAL A) &REST B)
;;;  then Y and Z are optional just like A, and the &OPTIONAL is
;;;  really a no-op.
;;;  Extra args are only checked for at the top level, too.
;;;  This is for backward compatibility with lots of old macro definitions.
;;;  Also, when &KEY is used, unrecognized keywords are not checked for.

;;;>>These are UNACCEPTABLE DEFICIENCIES and should be fixed asap
;;;>> (Actually, this code is so ancient as to be unusable, and must be replaced.)

(DEFCONST DEFMACRO-CHECK-ARGS T
  "T means DEFMACRO puts code in the macro definition to check the number of args.")
(DEFVAR *VARLIST* :UNBOUND
  "Used within DEFMACRO.")
(DEFVAR *VALLIST* :UNBOUND
  "Used within DEFMACRO.")

(DEFMACRO DEFMACRO (&ENVIRONMENT ENV &REST X)
  "Define FUNCTION-SPEC as a macro.
When a call to the macro is expanded, the argument list LAMBDA-LIST
is matched against the arguments supplied to the macro.
The variables in it are bound.  Then the BODY is executed,
and the value of the last form in it is the expansion of the macro."
  (DECLARE (ARGLIST FUNCTION-SPEC LAMBDA-LIST &BODY BODY))
  (DEFMACRO1 X 'MACRO ENV))

(DEFF MACRO-DISPLACE 'MACRO)
(COMPILER:MAKE-OBSOLETE MACRO-DISPLACE "it is now identical to MACRO")
(DEFF DEFMACRO-DISPLACE 'DEFMACRO)
(COMPILER:MAKE-OBSOLETE DEFMACRO-DISPLACE "it is now identical to DEFMACRO")


(DEFMACRO DEFLAMBDA-MACRO (&ENVIRONMENT ENV &REST X)
  "Define LAMBDA-MACRO-NAME as a lambda macro.
When a list starting with LAMBDA-MACRO-NAME is encountered as a function,
it is expanded by executing the lambda macro definition
to get a new function to use instead.  The argument list LAMBDA-LIST
is matched against the remainder of the list which is the function.
The variables in it are bound.  Then the BODY is executed,
and the value of the last form in it is the expansion of the macro,
the new function."
  (DECLARE (ARGLIST LAMBDA-MACRO-NAME LAMBDA-LIST &BODY BODY))
  (DEFMACRO1 X 'LAMBDA-MACRO ENV))

(DEFF DEFLAMBDA-MACRO-DISPLACE 'DEFLAMBDA-MACRO)
(COMPILER:MAKE-OBSOLETE DEFLAMBDA-MACRO-DISPLACE "it is now identical to DEFLAMBDA-MACRO")

;;; Onto this are pushed all the specified-flags of optional args
;;; (such as, FOOP in &OPTIONAL (FOO 69 FOOP)).
(DEFVAR *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*)

;;; This is set to T if the pattern used &body instead of &rest.
;;; This allows us to tell ZWEI about how to indent the form.
(DEFVAR DEFMACRO-&BODY-FLAG)

;;; X is the cdr of the DEFMACRO form.  TYPE is MACRO or MACRO-DISPLACE.
(DEFUN DEFMACRO1 (X TYPE &OPTIONAL ENV)
  (LET (*VARLIST*
        *VALLIST*
        *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
        DEFMACRO-&BODY-FLAG
        (ARGLIST (CADR X)))
    (WHEN (EQ (CAR-SAFE ARGLIST) '&WHOLE)
      (SETQ ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
           (MIN-ARGS (CAR ARGS-DATA))
           (OPT-ARGS (CADR ARGS-DATA))
           (arglist (labels ((args (l)
                               (loop for tail on l
                                     with state = 'required
                                  when (and (atom tail) tail)
                                    return l
                                  as car = (car tail)
                                  until (eq car '&aux)
                                  ;; user doesn't want to see these
                                  when (memq car '(&environment &whole))
                                    do (setq tail (cdr tail))
                                  else collect
                                    (case car
                                      ((&rest &optional &key) (setq state car))
                                      (t
                                       (if (and (memq state '(&optional &key))
                                                (cddr-safe car))
                                           ;; nuke suppliedp vars
                                           (setq car (list (car car) (cadr car))))
                                       (args car))))))
                      (args (cadr x)))))
      `(,TYPE ,(STANDARDIZE-FUNCTION-SPEC (CAR X))
        . ,(LAMBDA-EXP-ARGS-AND-BODY
             (EXPAND-DEFMACRO X ENV
                              `((ARGLIST . ,arglist)
                                ,@(IF DEFMACRO-&BODY-FLAG
                                      `((ZWEI:INDENTATION ,(+ MIN-ARGS OPT-ARGS) 1))))))))))

;;; X is the cdr of the DEFMACRO form.
;;; Return a LAMBDA expression for the expander function.
(DEFUN EXPAND-DEFMACRO (X ENV &OPTIONAL EXTRA-DECLARATIONS)
  (LET (*VARLIST*
        *VALLIST*
        *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
        DEFMACRO-&BODY-FLAG
        (ARGLIST (CADR X))
        WHOLE-ARG-DATA)
    (WHEN (EQ (CAR-SAFE ARGLIST) '&WHOLE)
      (SETQ WHOLE-ARG-DATA `((,(CADR ARGLIST) *MACROARG*))
            ARGLIST (CDDR ARGLIST)))
    (LET* ((ARGS-DATA (DEFMACRO-&MUMBLE-CHEVEUX ARGLIST '(CDR *MACROARG*) 0))
           (MIN-ARGS (CAR ARGS-DATA))
           (MAX-ARGS (CADDR ARGS-DATA))
           (BODY (CDDR X))
           DOC-STRING DECLS)
      (MULTIPLE-VALUE-SETQ (BODY DECLS DOC-STRING)
        (EXTRACT-DECLARATIONS BODY NIL T ENV))
      (SETQ DECLS (NCONC DECLS EXTRA-DECLARATIONS))
      (IF DOC-STRING (PUSH `(DOCUMENTATION ,DOC-STRING) DECLS))
      `(NAMED-LAMBDA ,(CAR X) (*MACROARG* &OPTIONAL *MACROENVIRONMENT*)
        (DECLARE . ,DECLS)
        *MACROENVIRONMENT*                      ; Ok not to refer to it.
        (,(IF (SYMBOLP (CAR X)) 'BLOCK 'PROGN) ,(IF (SYMBOLP (CAR X)) (CAR X))
          ,@(COND ((AND (null whole-arg-data)
                        DEFMACRO-CHECK-ARGS
                        (NOT (AND (ZEROP MIN-ARGS) (NULL MAX-ARGS))))
                   `((AND ,(COND ((ZEROP MIN-ARGS)
                                  `(> (LENGTH *MACROARG*)
                                      ,(1+ MAX-ARGS)))
                                 ((NULL MAX-ARGS)
                                  `(< (LENGTH *MACROARG*)
                                      ,(1+ MIN-ARGS)))
                                 (T `(OR (< (LENGTH *MACROARG*)
                                            ,(1+ MIN-ARGS))
                                         (> (LENGTH *MACROARG*)
                                            ,(1+ MAX-ARGS)))))
                          (MACRO-REPORT-ARGS-ERROR *MACROARG* ,MIN-ARGS ,MAX-ARGS))))
                  (T ()))
          (LET* (,@*DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
                 ,@WHOLE-ARG-DATA
                 . ,(MAPCAR #'LIST (NREVERSE *VARLIST*) (NREVERSE *VALLIST*)))
            (DECLARE . ,DECLS)
            . ,BODY))))))

(DEFUN MACRO-REPORT-ARGS-ERROR (MACRO-FORM MIN MAX &AUX (NARGS (1- (LENGTH MACRO-FORM))))
  (DECLARE (DBG:ERROR-REPORTER))
  (IF (< NARGS MIN)
      (FERROR "Too few arguments to macro: ~D passed, ~D required." NARGS MIN)
    (IF (AND MAX (> NARGS MAX))
        (FERROR "Too many arguments to macro: ~D passed, ~D allowed." NARGS MAX))))

(DEFMACRO DESTRUCTURING-BIND (VARIABLES DATA &BODY BODY)
  "Bind the VARIABLES to the components of DATA that they match, then execute the BODY.
DATA is evaluated; the VARIABLES list or tree is not evaluated."
  (declare (zwei:indentation 0 4 2 3))
  (LET (*VARLIST*
        *VALLIST*
        *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
        DEFMACRO-&BODY-FLAG)
    (DEFMACRO-&MUMBLE-CHEVEUX VARIABLES DATA 0)
    `(LET* (,@*DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*
            . ,(MAPCAR #'LIST (NREVERSE *VARLIST*) (NREVERSE *VALLIST*)))
       . ,BODY)))


;;; STATE is 0 for mandatory args, 1 for optional args, 2 for rest args, 3 for aux vars,
;;; 4 for &key args.
;;; If it is 8 or more, the 8 bit signifies &LIST-OF and the low three bits
;;; are as usual.
;;; If it is 16 or more, it signifies that the next arg is an &ENVIRONMENT arg;
;;; the low 4 bits say what state to revert to following that arg.
;;; PATH is the form which, using CAR and CDR, would extract the part of the macro arg
;;; which corresponds to this arg and the following args at the same level.
;;; Thus, a simple arg would be set to `(CAR ,PATH).
;;; PATTERN is the rest of the arglist at this level.
;;; We push arg names on *VARLIST* and their appropriate values on *VALLIST*.
;;; We return a list describing how many args are wanted:
;;;  its car is the minimum number of args needed,
;;;  its cadr is the number of optional args accepted,
;;;  its caddr is the maximum number of args accepted, or NIL if any number are allowed.
;;;    If non-NIL, this is normally the sum of the car and cadr.
(DEFUN DEFMACRO-&MUMBLE-CHEVEUX (PATTERN PATH STATE)
  (COND ((NULL PATTERN) (LIST 0 0 0))
        ((ATOM PATTERN)
         (COND ((> STATE 1)
                (FERROR "Non-NIL end of list, ~S, following ~S in destructuring pattern."
                        PATTERN
                        (CASE STATE
                          (2 '&REST)
                          (3 '&AUX)
                          (4 '&KEY)
                          (T (IF ( STATE #o20) '&ENVIRONMENT '&LIST-OF)))))
               (T (DEFMACRO-CHEVEUX PATTERN PATH)
                  (LIST 0 0 NIL))))
        ((EQ (CAR PATTERN) '&OPTIONAL)
         (COND ((> STATE 1)
                (FERROR "~S in bad context in destructuring pattern." '&OPTIONAL))
               (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 1))))
        ((MEMQ (CAR PATTERN) '(&REST &BODY))
         (AND (EQ (CAR PATTERN) '&BODY)
              (SETQ DEFMACRO-&BODY-FLAG T))
         (AND (NULL (CDR PATTERN))
              (FERROR "~S or ~S followed by no argument, in destructuring pattern."
                      '&REST '&BODY))
         (COND ((> STATE 1) (FERROR "~S or ~S in bad context in destructuring pattern."
                                    '&REST '&BODY))
               (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 2))))
        ((EQ (CAR PATTERN) '&AUX)
         (COND (( STATE #o10) (FERROR "~S following a ~S in destructuring pattern."
                                       '&AUX '&LIST-OF))
               (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 3))))
        ((EQ (CAR PATTERN) '&KEY)
         (COND ((> STATE 2) (FERROR "~S in bad context in destructuring pattern." '&KEY))
               (T (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 4))))
        ((EQ (CAR PATTERN) '&ENVIRONMENT)
         (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (+ STATE #o20)))
        ((EQ (CAR PATTERN) '&LIST-OF)
         (format *error-output* "~S is an obsolete ~S lambda-list keyword"
                 '&list-of 'defmacro)
         (COND ((< STATE 3)
                (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (+ #o10 STATE)))
               (T (FERROR "~S used incorrectly in destructuring pattern." '&LIST-OF))))
        ((EQ (CAR PATTERN) '&ALLOW-OTHER-KEYS)
         (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH STATE))
        ((= STATE 0)
         (DEFMACRO-CHEVEUX (CAR PATTERN) (LIST 'CAR PATH))
         (DEFMACRO-REQUIRED
           (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 0)))
        ((= STATE 1)
         (COND ((ATOM (CAR PATTERN))
                (DEFMACRO-CHEVEUX (CAR PATTERN) `(CAR ,PATH)))
               (T
                (AND (CADDAR PATTERN)
                     (PUSH (CADDAR PATTERN) *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*))
                (DEFMACRO-CHEVEUX (CAAR PATTERN)
                                  `(COND (,PATH
                                          ,(AND (CADDAR PATTERN)
                                                `(SETQ ,(CADDAR PATTERN) T))
                                          (CAR ,PATH))
                                         (T ,(CADAR PATTERN))))))
         (DEFMACRO-OPTIONAL
           (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 1)))
        ((= STATE 2)
         (DEFMACRO-CHEVEUX (CAR PATTERN) PATH)
         (COND ((CDR PATTERN)
                (AND (OR (ATOM (CDR PATTERN))
                         (NOT (MEMQ (CADR PATTERN) '(&AUX &KEY))))
                     (FERROR "More than one ~S argument in a macro." '&REST))
                (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 2)))
         (LIST 0 0 NIL))
        ((= STATE 3)
         (COND ((ATOM (CAR PATTERN))
                (DEFMACRO-CHEVEUX (CAR PATTERN) NIL))
               (T (DEFMACRO-CHEVEUX (CAAR PATTERN) (CADAR PATTERN))))
         (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) (LIST 'CDR PATH) 3))
        ((= STATE 4)
         (LET* ((SYMBOL
                  (COND ((ATOM (CAR PATTERN)) (CAR PATTERN))
                        ((ATOM (CAAR PATTERN)) (CAAR PATTERN))
                        (T (CADAAR PATTERN))))
                (KEYWORD
                  (IF (AND (CONSP (CAR PATTERN)) (CONSP (CAAR PATTERN)))
                      (CAAAR PATTERN)
                      (INTERN (STRING SYMBOL) PKG-KEYWORD-PACKAGE)))
                (DEFAULT
                  (IF (CONSP (CAR PATTERN)) (CADAR PATTERN) NIL))
                (FLAGVAR (IF (CONSP (CAR PATTERN)) (CADDAR PATTERN))))
           (PUSH SYMBOL *VARLIST*)
           (PUSH `(GETF ,PATH ',KEYWORD ,DEFAULT) *VALLIST*)
           (WHEN FLAGVAR
             (PUSH FLAGVAR *VARLIST*)
             (PUSH `(NOT (NULL (GET-LOCATION-OR-NIL (LOCF ,PATH) ',KEYWORD))) *VALLIST*))
           (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH 4)
           (LIST 0 0 NIL)))
        ((= STATE 8.)                           ;&LIST-OF not optional
         (DEFMACRO-&LIST-OF-CHEVEUX (CAR PATTERN) `(CAR ,PATH))
         (DEFMACRO-REQUIRED
           (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) `(CDR ,PATH) 0)))
        ((= STATE 9.)                           ;&LIST-OF optional
         (AND (ATOM (CAR PATTERN))
              (FERROR "Incorrect use of ~S in destructuring pattern." '&LIST-OF))
         (AND (CADDAR PATTERN)
              (PUSH (CADDAR PATTERN) *DEFMACRO-OPTIONAL-SPECIFIED-FLAGS*))
         (DEFMACRO-&LIST-OF-CHEVEUX (CAAR PATTERN)
                                    `(COND (,PATH
                                            ,(AND (CADDAR PATTERN)
                                                  `(SETQ ,(CADDAR PATTERN) T))
                                            (CAR ,PATH))
                                           (T ,(CADAR PATTERN))))
         (DEFMACRO-OPTIONAL
           (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) `(CDR ,PATH) 1)))
        ((= STATE 10.)
         (DEFMACRO-&LIST-OF-CHEVEUX (CAR PATTERN) PATH)
         (WHEN (CDR PATTERN)
           (AND (OR (ATOM (CDR PATTERN))
                    (NOT (EQ (CADR PATTERN) '&AUX)))
                (FERROR "More than one ~S argument in destructuring pattern." '&REST))
           (DEFMACRO-&MUMBLE-CHEVEUX (CDDR PATTERN) PATH 3))
         (LIST 0 0 NIL))
        (( STATE #o20)
         (DEFMACRO-CHEVEUX (CAR PATTERN) '*MACROENVIRONMENT*)
         (DEFMACRO-&MUMBLE-CHEVEUX (CDR PATTERN) PATH (LOGAND STATE #o17)))
        ))

(DEFUN DEFMACRO-&LIST-OF-CHEVEUX (PATTERN PATH)
  (SETQ *VALLIST*
        (LET (*VALLIST* (VALS *VALLIST*))
          (DEFMACRO-CHEVEUX PATTERN 'X)
          (DO ((NVALS (NREVERSE *VALLIST*) (CDR NVALS))
               (VALS VALS
                     (CONS `(MAPCAR (LAMBDA (X) ,(CAR NVALS))
                                    ,PATH)
                           VALS)))
              ((NULL NVALS) VALS)))))

(DEFUN DEFMACRO-CHEVEUX (PATTERN PATH)
  (COND ((NULL PATTERN))
        ((ATOM PATTERN)
         (AND (SYMBOLP PATTERN)
              (EQL (CHAR (SYMBOL-NAME PATTERN) 0) #/&)
              (FERROR "~S -- unrecognized &-keyword in ~S." PATTERN 'DEFMACRO))
         (SETQ *VARLIST* (CONS PATTERN *VARLIST*))
         (SETQ *VALLIST* (CONS PATH *VALLIST*)))
        (T
         (DEFMACRO-&MUMBLE-CHEVEUX PATTERN PATH 0))))

;;; Returns ARGS-DATA modified for one additional optional argument.
(DEFUN DEFMACRO-OPTIONAL (ARGS-DATA)
  (LIST (CAR ARGS-DATA)
        (1+ (CADR ARGS-DATA))
        (IF (CADDR ARGS-DATA) (1+ (CADDR ARGS-DATA)) NIL)))

;;; Returns ARGS-DATA modified for one additional required argument.
(DEFUN DEFMACRO-REQUIRED (ARGS-DATA)
  (LIST (1+ (CAR ARGS-DATA))
        (CADR ARGS-DATA)
        (IF (CADDR ARGS-DATA) (1+ (CADDR ARGS-DATA)) NIL)))

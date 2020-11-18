;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-

;;; Macros to do things similar to BLISS' SELECT.

(DEFMACRO COND-EVERY (&BODY CLAUSES)
  "COND-EVERY has a COND-like syntax.  Unlike COND, though, it executes all the
clauses whose tests succede.  It also recognizes two special keywords (instead of
a test):  :ALWAYS executes in all cases, and :OTHERWISE executes if no previous
clause has executed.  The value returned is that of the last clause executed,
or NIL if no clauses executed,  and the macro will not return multiple-values."
  (LET ((FLAG (GENSYM))
        (VALUE (GENSYM)))
    `(LET ((,FLAG) (,VALUE))
       ,@(DO ((CS CLAUSES (CDR CS))
              (CLAUSE (CAR CLAUSES) (CADR CS))
              (FORMS NIL)
              (SEEN-OTHERWISE-OR-ALWAYS))
             ((NULL CS) (NREVERSE FORMS))
           (PUSH
            (CASE (CAR CLAUSE)
              ((:ALWAYS T ALWAYS)
               (SETQ SEEN-OTHERWISE-OR-ALWAYS ':ALWAYS)
               `(SETQ ,VALUE (PROGN . ,(CDR CLAUSE))))
              ((:OTHERWISE OTHERWISE)
               (IF SEEN-OTHERWISE-OR-ALWAYS
                   (FERROR "~S after a previous ~S or ~S" :OTHERWISE :OTHERWISE :ALWAYS)
                 (SETQ SEEN-OTHERWISE-OR-ALWAYS ':OTHERWISE)
                 `(OR ,FLAG
                      (SETQ ,VALUE (PROGN . ,(CDR CLAUSE))))))
              (OTHERWISE
               `(AND ,(CAR CLAUSE)
                     (SETQ ,VALUE (PROGN . ,(CDR CLAUSE))
                           . ,(IF SEEN-OTHERWISE-OR-ALWAYS
                                  NIL
                                `(,FLAG T))))))
            FORMS))
       ,VALUE)))

(DEFMACRO SELECTQ-EVERY (OBJ &BODY CLAUSES)
  "Just like COND-EVERY but with SELECTQ-like syntax."
  (IF (ATOM OBJ)
      (SELECTQ-EVERY-GENERATE-CODE OBJ CLAUSES)
      (LET ((SYM (GENSYM)))
        `(LET ((,SYM ,OBJ))
           ,(SELECTQ-EVERY-GENERATE-CODE SYM CLAUSES)))))

(DEFUN SELECTQ-EVERY-GENERATE-CODE (COMPARE-AGAINST CLAUSES)
  `(COND-EVERY
    . ,(DO ((CS CLAUSES (CDR CS))
            (CLAUSE (CAR CLAUSES) (CADR CS))
            (FORMS NIL))
           ((NULL CS) (NREVERSE FORMS))
           (PUSH
            (COND ((MEMQ (CAR CLAUSE) '(:OTHERWISE :ALWAYS OTHERWISE ALWAYS T))
                   CLAUSE)
                  (T
                   `((,(IF (CONSP (CAR CLAUSE)) 'MEMQ 'EQ) ,COMPARE-AGAINST ',(CAR CLAUSE))
                     . ,(CDR CLAUSE))))
            FORMS))))

;;;; SELECT-MATCH
#||
The syntax is
   (SELECT-MATCH <object>
     (`<pattern> <condition> <sexp> ... <sexp>)
     (`<pattern> <condition> <sexp> ... <sexp>)
     ...
     (`<pattern> <condition> <sexp> ... <sexp>)
     (OTHERWISE <sexp> ... <sexp>))

The value of <object> is matched against the <pattern>s one at a time until a
match succeeds and the accompanying <condition> evaluates to something
non-null, at which point the <sexp>'s associated with the <pattern> and
<condition> are evaluated one at a time and the last value is returned.  The
<pattern>s can be arbitrary s-expressions, with variables signified by
,variable.  When the pattern is matched against the <object>'s value, the
variables are to bound to their matched values.  Different occurences of the
same variable in a given pattern must match to the same thing, so that

    (SELECT-MATCH '(A B C)
      (`(,X B ,X) T 'LOSE)
      (`(,X B ,Y) T 'WIN)
      (OTHERWISE 'LOSE-BIG))

returns WIN.  The variables mentioned in the <pattern>s need not be bound by
the user; they are bound by the expression resulting from the expansion of the
macro.  (The variable ,IGNORE matches everything and doesn't become bound.
Using ,IGNORE for a variable you don't intend to make any use of avoids
"bound but not used" warnings from the compiler.)

The patterns are expanded into code to test their predicates and set their variables.
The example above expands into this code (which, surprisingly, compiles very well).

(LET ((#:G0036 '(A B C))
      X
      Y)
  (COND ((MATCHCARCDR #:G0036
                      (LAMBDA (OBJ)
                        (PROGN (SETQ X OBJ)
                               T))
                      (LAMBDA (OBJ)
                        (MATCHCARCDR OBJ
                                     (LAMBDA (OBJ)
                                       (EQUAL OBJ 'B))
                                     (LAMBDA (OBJ)
                                       (MATCHCARCDR OBJ
                                                    (LAMBDA (OBJ)
                                                      (EQUAL OBJ X))
                                                    (LAMBDA (OBJ)
                                                      (NULL OBJ)))))))
         'LOSE)
        ((MATCHCARCDR #:G0036
                      (LAMBDA (OBJ)
                        (PROGN (SETQ X OBJ)
                               T))
                      (LAMBDA (OBJ)
                        (MATCHCARCDR OBJ
                                     (LAMBDA (OBJ)
                                       (EQUAL OBJ 'B))
                                     (LAMBDA (OBJ)
                                       (MATCHCARCDR OBJ
                                                    (LAMBDA (OBJ)
                                                      (PROGN (SETQ Y OBJ)
                                                             T))
                                                    (LAMBDA (OBJ)
                                                      (NULL OBJ)))))))
         'WIN)
        (T 'LOSE-BIG)))

||#

(DEFMACRO SELECT-MATCH (OBJECT &REST CLAUSES)
  (DECLARE (ZWEI:INDENTATION 1 1))              ;indents like CASE
  "Execute the first clause whose pattern matches the value of OBJECT.
The syntax is

   (SELECT-MATCH OBJECT
     (`PATTERN CONDITION CLAUSE-BODY...)
     (`PATTERN CONDITION CLAUSE-BODY...)
     ...
     (`PATTERN CONDITION CLAUSE-BODY...)
     (OTHERWISE CLAUSE-BODY...)

The value of OBJECT is matched against the PATTERNs one at a time until a
match succeeds and the accompanying CONDITION evaluates to non-NIL.
Then the CLAUSE-BODY of that clause is executed and its last expression's
value is returned.

,VARIABLE can appear in a pattern; it matches anything, and the variable
is bound to what it matched for the execution of the CONDITION and CLAUSE-BODY.
If one variable appears twice in a pattern, it must match EQUAL objects
in both occurrences:
    (SELECT-MATCH '(A B C)
      (`(,X B ,X) T 'LOSE)
      (`(,X B ,Y) T 'WIN)
      (OTHERWISE 'LOSE-BIG))
returns WIN.  Use ,IGNORE to match anything and not use it."
  (let* (*boundvars* (gensym (gensym)))
    (declare (special *boundvars*))
    `(block ,gensym
       (let ((,gensym ,object))
         . ,(loop for clause in clauses
              do (setq *boundvars* nil)
              collect (if (memq (car clause) '(otherwise t))
                          `(return-from ,gensym (progn . ,(cdr clause)))
                        (let ((patcond (select-match-matchitems (car clause) gensym)))
                          `(let ,(loop for x in *boundvars*
                                    collect `(,x (compiler::undefined-value)))
                             (when ,(if (eq (cadr clause) 't)
                                        patcond
                                      `(and ,patcond ,(cadr clause)))
                               (return-from ,gensym (progn . ,(cddr clause))))))))))))

(DEFMACRO LIST-MATCH-P (LIST PATTERN)
  "T if the value of LIST matches PATTERN.  PATTERN is a backquote expression.
Constant parts of PATTERN are matched against the corresponsing parts of LIST.
Variables preceded by commas are SETQ'd to the corresponding parts of LIST.
If the same variable appears twice, it must match EQUAL objects both times.
Example: (LIST-MATCH-P '(FOO BAR BAR) `(FOO ,X ,X)) returns T and sets X to BAR."
  (LET (*BOUNDVARS*)
    (DECLARE (SPECIAL *BOUNDVARS*))
    (SELECT-MATCH-MATCHITEMS PATTERN LIST)))

;;; MATCHCARCDR evals ARG, tests its car with CAR (a function of one arg)
;;; and its cdr with CDR.  The COMPILER::P1 property below takes care
;;; of open coding it with very fast code.
(defmacro matchcarcdr (arg car cdr)
  `(and (consp ,arg)
        (,car (car ,arg))
        (,cdr (cdr ,arg))))

;; don't use defoptimizer here, as compiler support is not loaded early enough.
;;  This is patched up in qcopt
;(compiler::defoptimizer matchcarcdr-hack matchcarcdr)
(defun matchcarcdr-hack (FORM)
  (if (and (eq (car-safe (caddr-safe form)) 'lambda)
           (eq (car-safe (cadddr-safe form)) 'lambda)
           (null (cddddr form)))
      `(compiler::matchcarcdr-hack ,(cadr form) ,(caddr form) ,(cadddr form))
    form))

(defun (:property compiler::matchcarcdr-hack compiler::p1) (form)
  (let ((carexp (matchcarcdr-convert-lambda (caddr form)))
        (arg (cadr form))
        (cdr (cadddr form)))
    (cond ((eq (car carexp) 'equal)
           `(and (compiler::push-cdr-if-car-equal
                   ,(compiler::p1v arg 1)
                   ,(compiler::p1v (caddr carexp) 1))
                 ,(compiler::p1 (matchcarcdr-convert-lambda cdr))))
          ((and (eq (car carexp) 'progn)
                (eq (car (cadr carexp)) 'setq))
           (compiler::p1setvar (cadr (cadr carexp)))
           `(and (compiler::push-cdr-store-car-if-cons
                   ,(compiler::p1v arg 1)
                   ,(compiler::p1v (cadr (cadr carexp)) 1))
                 ,(compiler::p1 (matchcarcdr-convert-lambda cdr))))
          (t
           `(and (compiler::consp-or-pop ,(compiler::p1v arg 1))
                 (progn (%push (compiler::carcdr (%pop)))
                        (cond (,(compiler::p1 carexp)
                               ,(compiler::p1 (matchcarcdr-convert-lambda cdr)))
                              ;; must be 't
                              ('t (%pop) 'nil))))))))

(defun matchcarcdr-convert-lambda (lambda-exp)
  (let ((argname (car (cadr lambda-exp))))
    (if (and (consp (third lambda-exp))
             (eq (second (third lambda-exp))
                 argname))
        (list* (first (third lambda-exp)) '(%pop) (cddr (third lambda-exp)))
      (if (and (consp (third lambda-exp))
               (eq (first (third lambda-exp)) 'progn)
               (eq (first (second (third lambda-exp))) 'setq))
          `(progn (setq ,(second (second (third lambda-exp))) (%pop)) t)
        `(progn (%pop) ,(third lambda-exp))))))

;;; Note that MATCHCARCDR-CONVERT-LAMBDA knows exactly what kinds of
;;; expressions this function can generate.
(defun select-match-matchitems (patt expr)
  (declare (special *boundvars*))
  (cond ((eq (car-safe patt) 'xr-bq-cons)
         (let ((obj (gensym)))
           `(matchcarcdr ,expr
                         (lambda (,obj) ,(select-match-matchitems (cadr patt) obj))
                         (lambda (,obj) ,(select-match-matchitems (caddr patt) obj)))))
        ((eq (car-safe patt) 'xr-bq-list)
         (labels ((foo (expr patt obj)
                    (if patt
                        `(matchcarcdr ,(or expr obj)
                                      (lambda (,obj)
                                        ,(select-match-matchitems (car patt) obj))
                                      (lambda (,obj)
                                        ,(foo nil (cdr patt) obj)))
                      `(equal ,obj ()))))
           (foo expr (cdr patt) (gensym))))
        ((eq (car-safe patt) 'xr-bq-list*)
         (labels ((foo (expr patt obj)
                    (if (cdr patt)
                        `(matchcarcdr ,(or expr obj)
                                      (lambda (,obj)
                                        ,(select-match-matchitems (car patt) obj))
                                      (lambda (,obj)
                                        ,(foo nil (cdr patt) obj)))
                      (select-match-matchitems (car patt) obj))))
           (foo expr (cdr patt) (gensym))))
        ((memq (car-safe patt) '(xr-bq-append xr-bq-nconc xr-bq-vector))
         (ferror "Appending, nconcing or vector construction in ~S pattern." 'select-match))
        ((memq (car-safe patt) '(and or not))
         (ferror "~S ~S patterns not suppported for the time being." (car patt) 'select-match))
         ;;>> this isn't right... (*boundvars* needs to be hacked specially)
         ;`(,(car patt) . ,(mapcar #'select-match-matchitems (cdr patt) (circular-list expr)))
        ((self-evaluating-p patt)
         (if (consp patt)                       ;(quote foo)
             `(equal ,expr ,patt)
             `(equal ,expr ',patt)))
        ((symbolp patt)
         (cond ((eq patt 'ignore) `(progn ,expr t))     ;the progn is so that the var expr is used
               ((memq patt *boundvars*) `(equal ,expr ,patt))
               (t (push patt *boundvars*)
                  `(progn (setq ,patt ,expr) t))))
        (t
         (ferror "Unexpected function ~S found in ~S pattern."
                 (car patt) 'select-match))))

;Note: value is a list of tests, to be ANDed together, rather than one test.
;(DEFUN SELECT-MATCH-MATCHITEMS (PATT EXPR)
;  (DECLARE (SPECIAL *BOUNDVARS*))
;  (COND        ((NULL PATT) `((NULL ,EXPR)))
;       ((SYMBOLP PATT)
;        (COND ((EQ PATT 'IGNORE) NIL)
;              ((MEMQ PATT *BOUNDVARS*) `(EQUAL ,PATT ,EXPR))
;              (T (PUSH PATT *BOUNDVARS*)
;                 `(PROGN (SETQ ,PATT ,EXPR) T))))
;       ((EQ (CAR PATT) 'XR-BQ-CONS)
;        (CONS `(CONSP ,EXPR)
;              (APPEND (SELECT-MATCH-MATCHITEMS (CADR PATT) `(CAR-SAFE ,EXPR))
;                      (SELECT-MATCH-MATCHITEMS (CADDR PATT) `(CDR-SAFE ,EXPR)))))
;       ((EQ (CAR PATT) 'XR-BQ-LIST)
;        (LIST* `(CONSP ,EXPR)
;               `(NULL (NTHCDR-SAFE ,(LENGTH (CDR PATT)) ,EXPR))
;               (LOOP FOR I = 0 THEN (1+ I)
;                     FOR ELT IN (CDR PATT)
;                     APPEND (SELECT-MATCH-MATCHITEMS ELT `(NTH-SAFE ,I ,EXPR)))))
;       ((EQ (CAR PATT) 'XR-BQ-LIST*)
;        (LIST* `(CONSP ,EXPR)
;               (APPEND
;                 (LOOP FOR I = 0 THEN (1+ I)
;                       FOR ELT IN (BUTLAST (CDR PATT))
;                       APPEND (SELECT-MATCH-MATCHITEMS ELT `(NTH-SAFE ,I ,EXPR)))
;                 (SELECT-MATCH-MATCHITEMS (CAR (LAST PATT))
;                                          `(NTHCDR-SAFE ,(1- (LENGTH (CDR PATT))) ,EXPR)))))
;       ((EQ (CAR PATT) 'QUOTE)
;        `((EQUAL ,EXPR ',(CADR PATT))))
;       ((MEMQ (CAR PATT) '(XR-BQ-APPEND XR-BQ-NCONC XR-BQ-VECTOR))
;        (FERROR NIL "Appending, nconcing or vector construction in SELECT-MATCH pattern."))
;       (T (FERROR NIL "Unexpected function ~S found in SELECT-MATCH pattern."
;                  (CAR PATT)))))

;;; -*- Mode:LISP; Package:INTERPRETER; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;;
;;; INTERPRETER.LISP
;;;
;;; Copyright 1986, Lisp Machine Incorporated
;;; Written by James Rauen


;;; Outside world:
;;;
;;;  GLOBAL:IF
;;;  SI::EXPAND-DEFMACRO
;;;  SI::GOBBLE-DECLARATIONS
;;;  USER::PARSE-LAMBDA-LIST
;;;  USER::PARSE-OPTIONAL-PARAMETER
;;;  USER::PARSE-KEY-PARAMETER
;;;  USER::PARSE-AUX-PARAMETER
;;;  USER::PARSE-LET-BINDING

;;; Contains:
;;;    - Startup code and variable definitions
;;;    - For the outside world: EVAL and EVAL-SPECIAL-OK
;;;    - Read-eval-print loops (for debugging)
;;;    - Frames and environments
;;;    - Intermediate expressions
;;;    - Lambda lists & let bindings
;;;    - Parsing special forms
;;;    - First pass of evaluator
;;;    - Second pass of evaluator
;;;    - Apply (for interpreter lexical closures)
;;;    - Preprocessor environment abstractions
;;;    - Lexical closures
;;;    - Errors
;;;    - Insurance
;;;    - Declarations
;;;    - Call history
;;;    - Rings
;;;    - Macro expanders
;;;    - Proclamations
;;;    - Metering
;;;    - Special forms
;;;    - Evalhook and applyhook


;;; Change log
;;;
;;; 24-Apr-87 11:58:43 - JRM
;;;    Added *ALLOW-LOSING-&-SYMBOLS?*.
;;;
;;; 3-May-87 15:53:10 - rauen
;;;    Changed package malarkey to K-LISP.

;;; Startup code and interpreter global variables
;;;

(eval-when (global:compile)
  (common-lisp t)
  (in-package 'interpreter))

;;; Hook into specialness of the rest of the world.
;;; We redefine it in this file, and we probably shouldn't.

(eval-when (global:load global:eval)

  (defun import-winning (symbols pkg)
    (let ((package (find-package pkg)))
      (dolist (symbol-list symbols)
        (let ((from (find-package (first symbol-list))))
          (dolist (s (rest symbol-list))
            (import (intern s from) package))))))

  (import-winning '((:si
                      :proclaim-special
                      :proclaim-unspecial
                      :proclaimed-special-p)
                    (:user
                      :parse-lambda-list
                      :parse-optional-parameter
                      :parse-key-parameter
                      :parse-aux-parameter
                      :parse-let-binding))
                  (find-package "INTERPRETER")))

;(eval-when (load)
;  (eval '(shadowing-import
;          '(
;            k-lisp:eval
;            k-lisp:evalhook
;            k-lisp:applyhook
;            k-lisp:*evalhook*
;            k-lisp:*applyhook*
;            k-lisp:macroexpand
;            k-lisp:macroexpand-1
;            k-lisp:*macroexpand-hook*
;            ))))

(shadow 'eval-special-ok)

(eval-when (global:compile)
  (format *trace-output* "~&Beginning compilation of interpreter."))



;; this sorta belongs in interpreter-hooks.lisp
;;
(defvar *ki-allow-apply-lambda-bagbiting* t
  "If T, then allow NAMED-LAMBDA expressions to be used like LAMBDA expressions
and have the old evaluator handle other non-Common-Lisp lambda-type expressions.
Print a warning whenever either of these situations occurs.  If NIL, then error
on any attempt to apply a non-Common-Lisp lambda expression.")

(defvar *ki-optimize-unreferenced-blocks* t
  "Make BLOCKs tail recursive when they aren't referred to by any RETURN-FROM forms.
This will cause an evalhook function to lose if it tries to execute a RETURN-FROM
inside a tail recursive BLOCK.")

(defvar *ki-intermediate-form-indentation* 0
  "Indentation for the interpreter's intermediate-form print function.")

(defvar *ki-verbose-intermediate-forms* :print-with-angles
  "T for a verbose intermediate-form print function, NIL for a terse one,
   :PRINT-WITH-ANGLES for #<foo 23>")

(defvar *ki-evalhook-first-position-functions* t
  "Use 'eval' to evaluate the function in a function application.  If NIL, the
function is evaluated without using 'eval'.  This determines whether or not
evaluating the function will be evalhooked.")

(defvar *ki-allow-free-variables* NIL
  "Bound to T while EVAL-SPECIAL-OK is running.  When NIL, free variable references
cause an error.")

(defvar *ki-fancy-error-handler* NIL
  "If NIL, proceedable errors are raised using CERROR.  Only one proceed type can
be used in this mode.  Otherwise, bound to the name of an error handler to use
for proceedable errors.  The error hander is a function of one argument, an
interpreter-error.")

(defvar *ki-call-history* nil ;(make-call-history)
  "The interpreter's call history ring")

(defvar *apply-count* 0
  "Counts number of calls to APPLY-LEXICAL-CLOSURE")

(defvar *eval-count* 0
  "Counts number of calls to MAIN-EVAL")

(defvar *evalhook* NIL)
(defvar *applyhook* NIL)
(defvar *macroexpand-hook* 'FUNCALL)


;;; FOR THE OUTSIDE WORLD
;;;

(defun eval (exp &optional nohook)
  "Evaluate a Lisp form."
  (let* ((*ki-allow-free-variables* NIL)
         (pass-1-result (first-pass-eval exp))
         (pass-2-result (multiple-value-list (second-pass-eval pass-1-result nohook))))
    (values-list pass-2-result)))

(defun eval-special-ok (exp)
  "Evaluate a Lisp form, treating free variable references as special variables."
  (let* ((*ki-allow-free-variables* T)
         (pass-1-result (first-pass-eval exp))
         (pass-2-result (multiple-value-list (second-pass-eval pass-1-result))))
    (values-list pass-2-result)))


;;; READ-EVAL-PRINT LOOPS
;;;
;;;

(defun rep ()
  (do-forever
    (fresh-line)
    (princ " ")
    (fresh-line)
    (princ "==> ")
    (let ((expr (read)))
      (print (eval expr)))))

(defun rep1 ()
  (do-forever
    (fresh-line)
    (princ "[1]==> ")
    (let ((expr (read)))
      (pprint (first-pass-eval expr)))))

(defun rep12 ()
  (do-forever
    (format *standard-output* "~&[1&2]==> ")
    (let ((expr (read)))
      (let ((intermediate (first-pass-eval expr)))
        (pprint intermediate)
        (fresh-line)
        (let ((results (multiple-value-list (second-pass-eval intermediate))))
          (mapcar #'(lambda (result) (print result) (fresh-line)) results))))))

(defstruct decl-info
  specials body)




;;; FRAMES AND ENVIRONMENTS
;;;
;;; parallel structure.  form's ppenv & eval env have same pattern of frames, same variables
;;; bound the same way, but different values.
;;; fast lookup, second value from lookup-binding-in-environment
;;;
;;;

(defstruct (nframe (:constructor make-nframe (parent type))
                   (:print-function print-nframe))
  parent
  type
  (bindings nil)
  (special-variables nil)
  (special-values nil))
;  (bindings (make-array '(16) :adjustable t :fill-pointer t))) ;start off allowing 16 bindings

(defun make-empty-environment ()
  nil)

(defsubst adjoin-variable-frame (parent-frame)
  (make-nframe parent-frame :variable))

(defsubst adjoin-function-frame (parent-frame)
  (make-nframe parent-frame :function))

(defsubst adjoin-block-frame (parent-frame)
  (make-nframe parent-frame :block))

(defsubst adjoin-gotag-frame (parent-frame)
  (make-nframe parent-frame :gotag))

(defsubst adjoin-junk-frame (parent-frame)
  (make-nframe parent-frame :placeholder))

(defun bind-in-frame (id value frame &optional type)
  (when (and type (not (eq (nframe-type frame) type)))
    (ferror "internal: Tried to make ~S binding in ~S frame." type (nframe-type frame)))
  (push `(,id . ,value) (nframe-bindings frame)))

(defun declare-special-variable-within-frame (id value frame)
  (when (not (eq (nframe-type frame) :variable))
    (ferror "internal: Tried to declare special variable in ~S frame." (nframe-type frame)))
  (push id (nframe-special-variables frame))
  (push value (nframe-special-values frame)))

(defun lookup-binding-in-environment (id first-frame type)
  (lookup-binding-in-environment-loop id first-frame type 0))

(defun lookup-binding-in-environment-loop (id frame type depth)
  (cond
    ((null frame)
     nil)
    ((not (eq type (nframe-type frame)))
     (lookup-binding-in-environment-loop id (nframe-parent frame) type (1+ depth)))
    (t
     (multiple-value-bind (binding depth-within-frame)
         (assoc-including-depth id (nframe-bindings frame))
       (if binding
           (values binding (make-speedy-lookup-proc depth depth-within-frame))
           (lookup-binding-in-environment-loop
             id (nframe-parent frame) type (1+ depth)))))))

(defun assoc-including-depth (id a-list &optional (depth 0))
  (cond ((null a-list)
         (values NIL NIL))
        ((eq id (caar a-list))
         (values (car a-list) depth))
        (t
         (assoc-including-depth id (cdr a-list) (1+ depth)))))

(defun backtrack-n-frames (n frame)
  (if (zerop n)
      frame
      (backtrack-n-frames (1- n) (nframe-parent frame))))

(defun make-speedy-lookup-proc (depth depth-within-frame)
  #'(lambda (frame)
      (cdr (nth depth-within-frame
                (nframe-bindings (backtrack-n-frames depth frame))))))

(defun print-nframe (nframe stream depth)
  (declare (ignore depth))
  (si::printing-random-object (nframe stream)
    (format stream "FRAME ~S" (nframe-type nframe))))
;;; True, this gives more info, but it clutters up the screen.
;  (format stream
;         "#<FRAME ~S ~S, Parent ~S>"
;         (nframe-type nframe)
;         (mapcar #'car (nframe-bindings nframe))
;         (if (nframe-parent nframe) (nframe-type (nframe-parent nframe)) NIL)))



;;; INTERMEDIATE EXPRESSIONS
;;;
;;; These are the intermediate expressions generated by the first pass of the evaluator
;;; (the preprocessor).  They are, in turn, evaluated by the second (main) pass
;;; of the evaluator.
;;;
;;; An intermediate expression is a structure with the following fields:
;;;    original-form       The corresponding Lisp form before preprocessing
;;;    preprocessed-form   Described below
;;;    preprocessor-env    The preprocessor environment used to preprocess original-form
;;;    eval-procedure      An optimization hack; a lookup of the evaluation procedure
;;;                          for (car preprocessed-form)
;;;
;;; Following are the legitimate preprocessed forms.
;;;
;;; (self-evaluating <exp>)
;;;    <exp> is a number, string, keyword, t, or nil.
;;;    The corresponding Lisp form is <exp>.
;;;
;;; (lexical-variable <id> speedy-lookup-proc)
;;; (special-variable <id>)
;;; (free-variable <id>)
;;;    The corresponding Lisp form is <id>.
;;;
;;; (regular-function <id>)
;;; (lexical-function <id>)
;;;    The corresponding Lisp form is (function <id>).
;;;
;;; (lexical-closure {<name>} <nice-lambda-list> . <body>)
;;;
;;; (funcall <closure> . <arguments>)
;;;
;;; (progn-with-frame . <body>)
;;;    Evaluates just like (progn . <body>) except that it creates an empty frame.  This
;;;    is generated by macrolet and optimized blocks; a frame must be created to keep
;;;    the evaluation environment in parallel with the preprocessor environment.
;;;
;;; (<special-form-name> . <special-form-args>)
;;;
(defstruct (intermediate-expression
             (:constructor
               make-intermediate-expr-rep
               (original-form preprocessed-form preprocessor-env eval-procedure))
             (:print-function print-intermediate-expression))
  original-form
  preprocessed-form
  preprocessor-env
  eval-procedure)

(defun make-intermediate-expression (original-form preprocessed-form preprocessor-env)
  (make-intermediate-expr-rep original-form
                              preprocessed-form
                              preprocessor-env
                              (lookup-eval-procedure (car preprocessed-form))))

(defun print-intermediate-expression (expr stream depth)
  (declare (ignore depth))
  (cond ((eq *ki-verbose-intermediate-forms* t)
         (let ((indent *ki-intermediate-form-indentation*)
               (*ki-intermediate-form-indentation* (+ *ki-intermediate-form-indentation* 2)))
           (format stream "~&~V@T#<Intermediate form:" indent)
           (format stream "~&~V@T  Before: ~S" indent
                   (intermediate-expression-original-form expr))
           (format stream "~&~V@T  After:  ~S" indent
                   (intermediate-expression-preprocessed-form expr))
           (format stream "~&~V@T >" indent)))
        ((eq *ki-verbose-intermediate-forms* nil)
         (print (intermediate-expression-preprocessed-form expr) stream))
        ((eq *ki-verbose-intermediate-forms* :original)
         (print (intermediate-expression-original-form expr) stream))
        ((eq *ki-verbose-intermediate-forms* :print-with-angles)
         (si::printing-random-object (expr stream)
           (format stream "~S" (car (intermediate-expression-preprocessed-form expr)))))
        (t (error "Bad verbosity ~s in *ki-verbose-intermediate-forms*" *ki-verbose-intermediate-forms*))))



;;; LAMBDA LISTS & LET BINDINGS
;;;
;;; These are the intermediate forms generated by the preprocessor to represent
;;; lambda-lists and let bindings.

(defstruct (nice-lambda-list (:print-function print-nice-lambda-list))
  required optional rest key allow-other-keys aux)

(defstruct (optional-parameter (:print-function print-optional-parameter))
  var initform svar)

(defstruct (key-parameter (:print-function print-key-parameter))
  var keyword initform svar)

(defstruct (aux-parameter (:print-function print-aux-parameter))
  var value)

(defstruct (let-binding (:type list))
  var value)

(defun print-nice-lambda-list (nice-lambda-list stream depth)
  (declare (ignore depth))
  (princ "(Required: " stream)
  (prin1 (nice-lambda-list-required nice-lambda-list) stream)
  (when (nice-lambda-list-optional nice-lambda-list)
    (progn (princ " Optional: " stream)
           (prin1 (nice-lambda-list-optional nice-lambda-list) stream)))
  (when (nice-lambda-list-rest nice-lambda-list)
    (progn (princ " Rest: " stream)
           (prin1 (nice-lambda-list-rest nice-lambda-list) stream)))
  (when (nice-lambda-list-key nice-lambda-list)
    (progn (princ " Key: " stream)
           (prin1 (nice-lambda-list-key nice-lambda-list) stream)))
  (when (nice-lambda-list-allow-other-keys nice-lambda-list)
    (princ " Allow-Other-Keys" stream))
  (when (nice-lambda-list-aux nice-lambda-list)
    (progn (princ " Aux: " stream)
           (prin1 (nice-lambda-list-aux nice-lambda-list) stream)))
  (princ ")" stream))

(defun print-a-field (first-field-p name value stream)
  (unless first-field-p (princ ", " stream))
  (princ name stream)
  (princ ": " stream)
  (prin1 value stream))

; Optional-parameter print function
;
; An optional parameter is printed in one of these forms:
;     foo
;     [var: foo, initform: <initform>]
;     [var: foo, initform: <initform>, svar: bar]
;
(defun print-optional-parameter (optional-parameter stream depth)
  (declare (ignore depth))
  (if (or (optional-parameter-initform optional-parameter)
          (optional-parameter-svar optional-parameter))
      (progn
        (princ "[" stream)
        (print-a-field t "var" (optional-parameter-var optional-parameter) stream)
        (print-a-field nil "initform" (optional-parameter-initform optional-parameter) stream)
        (when (optional-parameter-svar optional-parameter)
          (print-a-field nil "svar" (optional-parameter-svar optional-parameter) stream))
        (princ "] " stream))
      (prin1 (optional-parameter-var optional-parameter) stream)))

; Key-parameter print function
;
; A key parameter is printed in one of these forms:
;    [var: foo, keyword: :foo]
;    [var: foo, keyword: :foo, initform: <initform>]
;    [var: foo, keyword: :foo, initform: <initform>, svar: bar]
;
(defun print-key-parameter (key-parameter stream depth)
  (declare (ignore depth))
  (princ "[" stream)
  (print-a-field t "var" (key-parameter-var key-parameter) stream)
  (print-a-field nil "keyword" (key-parameter-keyword key-parameter) stream)
  (when (key-parameter-initform key-parameter)
    (print-a-field nil "initform" (key-parameter-initform key-parameter) stream))
  (when (key-parameter-svar key-parameter)
    (print-a-field nil "svar" (key-parameter-svar key-parameter) stream))
  (princ "]" stream))

; Aux-parameter print function
;
; An aux parameter is printed in this form:
;    [var: foo, value: bar]
(defun print-aux-parameter (aux-parameter stream depth)
  (declare (ignore depth))
  (princ "[" stream)
  (print-a-field t "var" (aux-parameter-var aux-parameter) stream)
  (print-a-field nil "value" (aux-parameter-value aux-parameter) stream)
  (princ "]" stream))



;;; PARSING SPECIAL FORMS
;;;

(defsubst block-name (block-expr) (cadr block-expr))
(defsubst block-body (block-expr) (cddr block-expr))
(defsubst catch-tag (catch-expr) (cadr catch-expr))
(defsubst catch-forms (catch-expr) (cddr catch-expr))
(defsubst eval-when-situation-list (eval-when-expr) (cadr eval-when-expr))
(defsubst eval-when-forms (eval-when-expr) (cddr eval-when-expr))
(defsubst function-fn (function-expr) (cadr function-expr))
(defsubst go-gotag (go-expr) (cadr go-expr))
(defsubst if-predicate (if-expr) (cadr if-expr))
(defsubst if-consequent (if-expr) (caddr if-expr))
(defsubst if-alternate (if-expr)
  (if (> (length if-expr) 3) (cadddr if-expr)))
(defsubst let-bindings (let-expr) (cadr let-expr))
(defsubst let-body (let-expr) (cddr let-expr))
(defsubst multiple-value-call-function (multiple-value-call-expr) (cadr multiple-value-call-expr))
(defsubst multiple-value-call-forms (multiple-value-call-expr) (cddr multiple-value-call-expr))
(defsubst multiple-value-prog1-first-form (multiple-value-prog1-expr)
  (cadr multiple-value-prog1-expr))
(defsubst multiple-value-prog1-other-forms (multiple-value-prog1-expr)
  (cddr multiple-value-prog1-expr))
(defsubst progn-forms (progn-expr) (cdr progn-expr))
(defsubst progv-symbols (progv-expr) (cadr progv-expr))
(defsubst progv-values (progv-expr) (caddr progv-expr))
(defsubst progv-forms (progv-expr) (cdddr progv-expr))
(defsubst quote-object (quote-expr) (cadr quote-expr))
(defsubst return-from-name (return-from-expr) (cadr return-from-expr))
(defsubst return-from-result (return-from-expr)
  (if (> (length return-from-expr) 2) (caddr return-from-expr) nil))
(defsubst setq-args (setq-expr) (cdr setq-expr))
(defsubst tagbody-body (tagbody-expr) (cdr tagbody-expr))
(defsubst the-value-type (the-expr) (cadr the-expr))
(defsubst the-form (the-expr) (caddr the-expr))
(defsubst throw-tag (throw-expr) (cadr throw-expr))
(defsubst throw-result (throw-expr) (caddr throw-expr))
(defsubst unwind-protect-protected-form (unwind-protect-expr) (cadr unwind-protect-expr))
(defsubst unwind-protect-cleanup-forms (unwind-protect-expr) (cddr unwind-protect-expr))



;;; FIRST PASS
;;;
;;; The first pass of the interpreter, the preprocessor, generates an intermediate-expression
;;; from a Lisp form.  The intermediate-expression is a structure containing the original
;;; form, the preprocessed form, and a handle on the preprocessor's environment.
;;;
;;; Preprocessing a form does the following:
;;;    - Identifies self-evaluating expressions.
;;;    - Determines the scope of variable and function identifiers.
;;;    - Expands macros, including macros defined by MACROLET.
;;;    - Converts first-position function applications to funcalls.
;;;    - Reports syntax errors
;;;    - Optimizes BLOCK expressions (this is optional) by converting them to PROGN forms
;;;        when the BLOCK identifiers aren't referred to.
;;;    - Converts (FUNCTION (LAMBDA ..)) expressions to lexical closures.
;;;    - Parses lambda lists.
;;;
;;; A preprocessed form is a list beginning with one of the symbols {self-evaluating,
;;; lexical-variable, special-variable, free-variable, regular-function, lexical-function,
;;; lexical-closure}, or a special form name.  The second pass of the evaluator dispatches
;;; on this symbol to evaluate the preprocessed form.
;;;
;;; Preprocessed form syntax is described more completely in the documentation for
;;; intermediate expressions.
;;;

(eval-when (global:compile)
  (format *trace-output* "~&Compiling first pass."))

(defun first-pass-eval (exp)
  (preprocess exp (make-empty-environment)))

;intern symbol-name hack is needed for macros
;like AND, which expand into things like GLOBAL:IF
;
(defun preprocess (exp env)
     (cond
      ((numberp exp)
       (preprocess-self-evaluating exp env))
      ((characterp exp)
       (preprocess-self-evaluating exp env))
      ((stringp exp)
       (preprocess-self-evaluating exp env))
      ((keywordp exp)
       (preprocess-self-evaluating exp env))
      ((member exp '(t nil))
       (preprocess-self-evaluating exp env))
      ((arrayp exp)
       (preprocess-self-evaluating exp env))
      ((symbolp exp)
       (preprocess-variable exp env))
      ((not (listp exp))
       (error "Expression is neither an atom nor a list"))
      ((common-lisp-special-form-p (car exp))
       (preprocess-special-form exp env))
      ((implementation-special-form-p (car exp))
       (preprocess-special-form exp env))
      ((eq (car exp) 'global:if)
       (preprocess-special-form `(if ,@(cdr exp)) env))
      ((symbolp (car exp))
       (preprocess-funmac exp env))
      ((lambda-expression-p (car exp))
       (preprocess-function-application exp env))
      (t
       (error "user: Bad form in first position"))))


(defun preprocess-sequence (exps env)
  (mapcar #'(lambda (exp) (preprocess exp env))
          exps))


;;; Preprocessing a self-evaluating expression
;;;
(defun preprocess-self-evaluating (exp env)
  (make-intermediate-expression exp
                                `(self-evaluating ,exp)
                                env))


;;; Preprocessing a variable.  A variable can be lexical, special, or free; this is
;;; determined by applying the following four rules in order:
;;;
;;;     1. If the variable has been proclaimed special, it is special.
;;;     2. If the lexical environment doesn't know about the variable, it is free.
;;;     3. If the variable has been lexically declared special, it is special.
;;;     4. Otherwise, the variable is lexical.
;;;
(defun preprocess-variable (sym env)
  (multiple-value-bind (lexical-binding speedy-lookup-proc)
      (lookup-binding-in-environment sym env :variable)
    (cond ((proclaimed-special-p sym)
           (make-intermediate-expression sym `(SPECIAL-VARIABLE ,sym) env))
          ((null lexical-binding)
           (make-intermediate-expression sym `(FREE-VARIABLE ,sym) env))
          ((eq (cdr lexical-binding) 'special)
           (make-intermediate-expression sym `(SPECIAL-VARIABLE ,sym) env))
          (t
           (make-intermediate-expression sym `(LEXICAL-VARIABLE ,sym ,speedy-lookup-proc) env)))))


;;; Preprocessing a named function or macro application.
;;;
;;; When the expression in the first position of a form is a symbol, and it doesn't
;;; name a special form, this function is called.
;;;
;;; The preprocessor's environment is searched for a lexical function/macro binding of the
;;; symbol.  If a lexical binding is found, then the form is either preprocessed into
;;; a function application, or macroexpanded and reprocessed.  If no lexical binding
;;; exists, the symbol is checked for a global macro definition.  If one is found,
;;; the form is macroexpanded and reprocessed; otherwise, it is assumed to be a regular
;;; function application and preprocessed as such.
;;;
(defun preprocess-funmac (exp env)
  (let* ((name (car exp))
         (lexical-binding (lookup-binding-in-environment name env :function)))
    (if lexical-binding
        (case (preprocessor-funmac-binding-type lexical-binding)
          (function (preprocess-function-application exp env))
          (macro
           (let* ((expander-fn (preprocessor-funmac-binding-value lexical-binding))
                  (expanded-macro (funcall expander-fn exp nil)))
             (preprocess expanded-macro env)))
          (otherwise (error "internal: unrecognized funmac binding type in preprocess-funmac")))
        (cond
          ((macro-function name)
           (let* ((expander-fn (macro-function name))
                  (expanded-macro (funcall expander-fn exp nil)))
             (preprocess expanded-macro env)))
          (t
           (preprocess-function-application exp env))))))


;;; Preprocessing a function application
;;;
;;; This function is called to preprocess the application of a function to arguments.
;;; The function can be a regular function, a lexically defined function (by FLET or
;;; LABELS), or a lambda-expression.
;;;
;;; A function application has the form (fn arg1 arg2 ...).
;;;
;;; A lexical closure is defined by preprocessing the form (FUNCTION fn), which
;;; returns a lexical-function, regular-function, or lexical-closure intermediate
;;; expression.  All three kinds of intermediate expressions will evaluate to lexical
;;; closures in the second pass.
;;;
;;; An intermediate expression is constructed and returned which contains the preprocessed
;;; lexical closure and the preprocessed arguments.
;;;
(defun preprocess-function-application (exp env)
  (let ((name (car exp))
        (arguments (cdr exp)))
    (make-intermediate-expression
      exp
      `(funcall ,(preprocess-special-form `(function ,name) env)
                ,@(preprocess-sequence arguments env))
      env)))


;;; Preprocessing a special form.
;;;
;;; Special forms are handled individually.  The special form's name is looked up
;;; in a dispatch table, and the function found there is called to preprocess the
;;; special form.
;;;
(defun preprocess-special-form (exp env)
  (let ((name (car exp)))
    (let ((preprocessing-function (lookup-special-form-preprocessing-function name)))
      (unless preprocessing-function
        (error "internal: Preprocessor can't handle that special form."))
      (make-intermediate-expression exp (funcall preprocessing-function exp env) env))))

(defun lookup-special-form-preprocessing-function (name)
  (case name
    (block                #'preprocess-block)
    (catch                #'preprocess-catch)
    (compiler-let         #'preprocess-compiler-let)
    (declare              #'preprocess-declare)
    (eval-when            #'preprocess-eval-when)
    (flet                 #'preprocess-flet)
    (function             #'preprocess-function)
    (go                   #'preprocess-go)
    (if                   #'preprocess-if)
    (labels               #'preprocess-labels)
    (let                  #'preprocess-let)
    (let*                 #'preprocess-let*)
    (macrolet             #'preprocess-macrolet)
    (multiple-value-call  #'preprocess-multiple-value-call)
    (multiple-value-prog1 #'preprocess-multiple-value-prog1)
    (progn                #'preprocess-progn)
    (progv                #'preprocess-progv)
    (quote                #'preprocess-quote)
    (return-from          #'preprocess-return-from)
    (setq                 #'preprocess-setq)
    (tagbody              #'preprocess-tagbody)
    (the                  #'preprocess-the)
    (throw                #'preprocess-throw)
    (unwind-protect       #'preprocess-unwind-protect)
    (describe-frame       #'preprocess-describe-frame)
    (describe-pp          #'preprocess-describe-pp)
    (otherwise            nil)))


;;; Preprocessing DESCRIBE-FRAME expressions.
;;;
(defun preprocess-describe-frame (exp env)
  (declare (ignore exp env))
  `(describe-frame))

;;; Preprocessing DESCRIBE-PP expressions.
;;;
(defun preprocess-describe-pp (exp env)
  (if (null env)
      (progn
        (terpri)
        (princ "NIL is the null preprocessor environment.")
        (terpri)
        `(describe-frame))
      (progn
        (describe-defstruct env)
        (preprocess-describe-pp exp (nframe-parent env)))))

;;; Preprocessing BLOCK expressions.
;;;
(defun preprocess-block (exp env)
  (require-n-arguments exp 1)
  (let ((block-name (block-name exp))
        (block-body (block-body exp)))
    (unless (symbolp block-name)
      (ferror "The BLOCK id ~S is not a symbol." block-name))
    (let ((inner-frame (adjoin-block-frame env)))
      (bind-in-frame block-name NIL inner-frame :block)
      (let ((preprocessed-body (preprocess-sequence block-body inner-frame))
            (block-id-used (cdr (lookup-binding-in-environment block-name inner-frame :block))))
        (if (or block-id-used (not *ki-optimize-unreferenced-blocks*))
            `(block ,block-name ,@preprocessed-body)
            `(progn ,@(preprocess-sequence block-body env))))))) ; repreprocess


;;; Preprocessing RETURN-FROM expressions.
;;;
(defun preprocess-return-from (exp env)
  (require-n-arguments exp 1 2)
  (let ((name (return-from-name exp))
        (result (return-from-result exp)))
    (unless (symbolp name)
      (ferror "The RETURN-FROM id ~S is not a symbol." name))
    (unless (lookup-binding-in-environment name env :block)
      (ferror nil "~S is not a lexically visible BLOCK tag." name))
    (rplacd (lookup-binding-in-environment name env :block) T)
    `(return-from ,name ,(preprocess result env))))


;;; Preprocessing TAGBODY expressions.
;;;
(defun preprocess-tagbody (exp env)
  (let ((inner-frame (adjoin-gotag-frame env)))
    `(tagbody ,@(mapcar #'(lambda (arg) (preprocess-tagbody-arg arg inner-frame))
                        (tagbody-body exp)))))

(defun preprocess-tagbody-arg (arg frame)
  (cond
    ((gotagp arg)
     (bind-in-frame arg NIL frame :gotag)   ; this isn't necessary unless gotags have compiled
                                            ; lookups.
     arg)
    ((listp arg)
     (preprocess arg frame))
    (t
     (ferror "~S form may not appear inside a TAGBODY expression." arg))))


;;; Preprocessing GO expressions.
;;;
(defun preprocess-go (exp env)
  (declare (ignore env))
  (require-n-arguments exp 1 1)
  (let ((gotag (go-gotag exp)))
    (unless (gotagp gotag)
      (ferror "~S not a GO tag." gotag))
    `(go ,gotag)))

(defun gotagp (expr)
  (if (or (symbolp expr) (numberp expr)) t nil))


;;; Preprocessing CATCH expressions.
;;;
(defun preprocess-catch (exp env)
  (require-n-arguments exp 1)
  (let ((tag (catch-tag exp))
        (forms (catch-forms exp)))
    `(catch ,(preprocess tag env) ,@(preprocess-sequence forms env))))


;;; Preprocessing THROW expressions.
;;;
(defun preprocess-throw (exp env)
  (require-n-arguments exp 2 2)
  (let ((tag (throw-tag exp))
        (result (throw-result exp)))
    `(throw ,(preprocess tag env) ,(preprocess result env))))


;;; Preprocessing UNWIND-PROTECT expressions.
;;;
(defun preprocess-unwind-protect (exp env)
  (require-n-arguments exp 1)
  (let ((protected-form (unwind-protect-protected-form exp))
        (cleanup-forms (unwind-protect-cleanup-forms exp)))
    `(unwind-protect ,(preprocess protected-form env)
                     ,@(preprocess-sequence cleanup-forms env))))


;;; Preprocessing PROGN expressions.
;;;
(defun preprocess-progn (exp env)
  `(progn ,@(preprocess-sequence (progn-forms exp) env)))


;;; Preprocessing PROGV expressions.
;;;
(defun preprocess-progv (exp env)
  (require-n-arguments exp 2)
  (let ((symbols (progv-symbols exp))
        (values (progv-values exp))
        (forms (progv-forms exp)))
    (unless (listp symbols)
      (error "PROGV symbols not a list"))
    (unless (listp values)
      (error "PROGV values not a list"))
    `(progv ,(preprocess symbols env)
            ,(preprocess values env)
            ,@(preprocess-sequence forms env))))


;;; Preprocessing LET, LET*, and COMPILER-LET expressions.
;;;
;;; A uniform procedure is used to preprocess let expressions.  These
;;; include LET, LET*, and COMPILER-LET.
;;;
;;;    1. Raise an error if there is no binding list.
;;;
;;;    2. Examine the declarations inside the expression.  The resulting
;;;       structure, decl-info, contains the following fields:
;;;
;;;           specials:  a list of the variables declared special
;;;           body:      the statements in the let expression which follow
;;;                      the declarations and documentation strings.
;;;
;;;    3. Preprocess the bindings.  The resulting structure, let-info,
;;;       contains the following fields:
;;;
;;;           preprocessed-bindings:  the preprocessed bindings
;;;           new-environment:        the new preprocessor environment created
;;;                                   by preprocessing the bindings
;;;
;;;    4. Preprocess the body of the let expression in the new environment.
;;;
;;;    5. Construct and return the preprocessed let form.
;;;

(defun preprocess-let (exp env)
  (preprocess-general-let 'LET exp env))

(defun preprocess-let* (exp env)
  (preprocess-general-let 'LET* exp env))

(defun preprocess-compiler-let (exp env)
  (require-n-arguments exp 1)
  (let*
    ((bindings (let-bindings exp))
     (body (let-body exp))
     (special-variables
       (mapcar
            #'(lambda (binding)
                (require-bindable-symbol (if (listp binding) (car binding) binding)))
            bindings))
     (special-values
       (mapcar
         #'(lambda (binding)
             (if (listp binding) (si::eval-special-ok (second binding)) NIL))
         bindings)))

;     (inner-frame (adjoin-variable-frame env)))

    (progv special-variables special-values
      `(progn ,@(preprocess-sequence body env)))))

;      `(compiler-let ,(preprocess-let-bindings 'LET bindings special-variables inner-frame))
;     (preprocessed-body
;       (preprocess-sequence body inner-frame)))
;   `(compiler-let ,preprocessed-bindings ,@preprocessed-body)))

;;; The declarations in the body of the LET are in effect during the
;;; evaluation of the binding values also (see Common LISP, pg. 155)
;;; To make this work, I kludged
;;; the preprocessor to add an extra frame that holds the special variables
;;; during the binding evaluation.  This seems to work nicely.  - JRM 13-Mar-87 15:30:05

(defun preprocess-general-let (special-form-name exp env)
  (require-n-arguments exp 1)
  (let*
    ((let-bindings (let-bindings exp))
     (let-body (let-body exp))
     (decl-info (examine-declarations let-body env))
     (special-variables (decl-info-specials decl-info))
     (binding-frame (adjoin-variable-frame env))
     (inner-frame (adjoin-variable-frame binding-frame))
     (foo0 (declare-special-variables special-variables binding-frame))
     (foo1 (declare-special-variables special-variables inner-frame))
     (preprocessed-bindings
       (preprocess-let-bindings special-form-name let-bindings special-variables inner-frame))
     (preprocessed-body
       (preprocess-sequence (decl-info-body decl-info) inner-frame)))
    (declare (ignore foo0 foo1))
   `(,special-form-name ,preprocessed-bindings ,@preprocessed-body)))

(defun preprocess-let-bindings (special-form-name binding-list special-variables frame)
  (when binding-list
    (multiple-value-bind (var value)
        (parse-let-binding (first binding-list))
      (let ((preprocessed-value
              (case special-form-name
                (LET (preprocess value (nframe-parent frame)))
                (LET* (preprocess value frame))
                (otherwise (error "internal: preprocess-general-let-bindings")))))
        (cons `(,(preprocessor-bind-variable var special-variables frame) ,preprocessed-value)
              (preprocess-let-bindings
                special-form-name (cdr binding-list) special-variables frame))))))


;;; Preprocess MACROLET
;;;
(defun preprocess-macrolet (exp env)
  (require-n-arguments exp 1)
  (let*
    ((macrolet-bindings (let-bindings exp))
     (macrolet-body (let-body exp))
     (inner-frame (adjoin-function-frame env))
     (preprocessed-bindings (preprocess-macrolet-bindings macrolet-bindings inner-frame))
     (preprocessed-body (preprocess-sequence macrolet-body inner-frame)))
    (declare (ignore preprocessed-bindings))
    `(progn-with-frame ,@preprocessed-body)))

(defun preprocess-macrolet-bindings (binding-list frame)
  (when binding-list
    (let ((binding (first binding-list)))
      (when (< (length binding) 2)
        (error "MACROLET binding too short."))
      (let* ((name (car binding))
             (expander-fn (SI::EXPAND-DEFMACRO binding nil)))
        (bind-in-frame name `(MACRO . ,expander-fn) frame :function)
        (preprocess-macrolet-bindings (rest binding-list) frame)))))

;;; Preprocessing FUNCTION expressions
;;;
;;; A FUNCTION expression must have one argument, which must be a symbol or
;;; a lambda expression.
;;;
;;; (function sym) preprocesses to (lexical-function sym) if sym is locally
;;; bound as a function, or (regular-function sym) if sym isn't locally bound
;;; to a function.
;;;
;;; (function lambda-expression) preprocesses to a lexical closure.
;;;
(defun preprocess-function (exp env)
  (require-n-arguments exp 1 1)
  (let ((fn (function-fn exp)))
    (cond
      ((symbolp fn)
       (preprocess-function-symbol fn env))
      ((lambda-expression-p fn)
       (preprocess-function-lambda fn env))
      ((named-lambda-expression-p fn)
       (preprocess-function-named-lambda fn env))
      (t
       (error "Argument to ~S, (~S ...) is not valid." 'function (car fn))))))

(defun preprocess-function-symbol (sym env)
  (let ((lexical-binding (lookup-binding-in-environment sym env :function)))
    (cond
      ((null lexical-binding)
       ;;; Don't check now, defun may happen later.
       ; (if (macro-function sym)
       ;     (error "(~S ~S) names a macro." 'function sym)
       `(regular-function ,sym))
      ((eq (preprocessor-funmac-binding-type lexical-binding) 'macro)
       (ferror "~S names a lexical macro." sym))
      ((eq (preprocessor-funmac-binding-type lexical-binding) 'function)
       `(lexical-function ,sym))
      (t
       (error "internal: preprocess-function-symbol")))))

(defun preprocess-function-lambda (lambda-expr env)
  (preprocess-lexical-closure :UNNAMED lambda-expr env))

(defun preprocess-function-named-lambda (named-lambda-expr env)
  (let ((name (cadr named-lambda-expr))
        (bindings (caddr named-lambda-expr))
        (body (cdddr named-lambda-expr)))
    (preprocess-lexical-closure name `(LAMBDA ,bindings ,@body) env)))


;;;; Preprocessing lexical closures
;;;
(defun preprocess-lexical-closure (name lambda-expr env)
  (when (< (length lambda-expr) 2)
    (error "Too few elements in lambda expression"))
  (let* ((lambda-list (cadr lambda-expr))
         (lambda-body (cddr lambda-expr))
         (decl-info (examine-declarations lambda-body env))
         (special-variables (decl-info-specials decl-info))
         (inner-frame (adjoin-variable-frame env))
         (foo (declare-special-variables special-variables inner-frame))
         (preprocessed-lambda-list
           (preprocess-lambda-list lambda-list special-variables inner-frame)))
    (declare (ignore foo))
    `(lexical-closure ,name
                      ,preprocessed-lambda-list
                      ,@(preprocess-sequence (decl-info-body decl-info)
                                             inner-frame))))

(defun lambda-expression-p (expr)
  (and (listp expr) (eq (car expr) 'lambda)))

(defun named-lambda-expression-p (expr)
  (and (listp expr) (eq (car expr) 'interpreter-named-lambda)))

;;; Preprocessing lambda lists
;;;
;;; Preprocesses a lambda-list.  Special-variables is a list of variables
;;; that are declared special within the lambda expression.  This function
;;; returns a nice-lambda-list structure and binds the lambda-list variables
;;; in the frame.  If there is a syntax error in the lambda-list, it is raised here.
;;;
(defun preprocess-lambda-list (lambda-list special-variables frame)
  (multiple-value-bind (required-arguments optional-arguments rest-argument
                        key-arguments      allow-argument    aux-arguments)
      (USER::PARSE-LAMBDA-LIST lambda-list '(:REQUIRED :OPTIONAL :REST
                                             :KEY :ALLOW-OTHER-KEYS :AUX))
    (let*
      ((preprocessed-required
         (preprocess-lambda-list-required required-arguments special-variables frame))
       (preprocessed-optional
         (preprocess-lambda-list-optional optional-arguments special-variables frame))
       (preprocessed-rest
         (when rest-argument
           (preprocess-lambda-list-rest rest-argument special-variables frame)))
       (preprocessed-key
         (preprocess-lambda-list-key key-arguments special-variables frame))
       (preprocessed-allow
         allow-argument)
       (preprocessed-aux
         (preprocess-lambda-list-aux aux-arguments special-variables frame)))
      (make-nice-lambda-list :required preprocessed-required
                             :optional preprocessed-optional
                             :rest preprocessed-rest
                             :key preprocessed-key
                             :allow-other-keys preprocessed-allow
                             :aux preprocessed-aux))))


; Return the preprocessed parameters, making bindings in the frame.
;
(defun preprocess-lambda-list-required (parameters special-variables frame)
  (when parameters
    (let ((next-parameter (require-bindable-symbol (car parameters))))
      (cons
        (preprocessor-bind-variable next-parameter special-variables frame)
        (preprocess-lambda-list-required (cdr parameters) special-variables frame)))))

(defun preprocess-lambda-list-optional (parameters special-variables frame)
  (when parameters
    (multiple-value-bind (var initform svar)
        (USER::PARSE-OPTIONAL-PARAMETER (car parameters))
      (let*
        ((preprocessed-var
           (preprocessor-bind-variable var special-variables frame))
         (preprocessed-initform
           (if initform (preprocess initform frame) nil))
         (preprocessed-svar
           (if svar (preprocessor-bind-variable svar special-variables frame) nil)))
        (cons (make-optional-parameter :var preprocessed-var
                                       :initform preprocessed-initform
                                       :svar preprocessed-svar)
              (preprocess-lambda-list-optional (cdr parameters) special-variables frame))))))

(defun preprocess-lambda-list-rest (parameter special-variables frame)
  (preprocessor-bind-variable parameter special-variables frame))

(defun preprocess-lambda-list-key (parameters special-variables frame)
  (when parameters
    (multiple-value-bind (var initform svar keyword)
        (USER::PARSE-KEY-PARAMETER (car parameters))
      (let*
        ((preprocessed-initform
           (if initform (preprocess initform frame) nil))
         (preprocessed-var
           (preprocessor-bind-variable var special-variables frame))
         (preprocessed-svar
           (if svar (preprocessor-bind-variable svar special-variables frame) nil)))
        (cons (make-key-parameter :var preprocessed-var
                                  :keyword keyword
                                  :initform preprocessed-initform
                                  :svar preprocessed-svar)
              (preprocess-lambda-list-key (cdr parameters) special-variables frame))))))

(defun preprocess-lambda-list-aux (parameters special-variables frame)
  (when parameters
    (multiple-value-bind (var initform)
        (USER::PARSE-AUX-PARAMETER (car parameters))
      (let*
        ((preprocessed-initform
           (if initform (preprocess initform frame) nil))
         (preprocessed-var
           (preprocessor-bind-variable var special-variables frame)))
        (cons (make-aux-parameter :var preprocessed-var
                                  :value preprocessed-initform)
              (preprocess-lambda-list-aux (cdr parameters) special-variables frame))))))

(defun lambda-list-keyword-p (sym)
  (member sym lambda-list-keywords))

(defun looks-like-lambda-list-keyword? (sym)
  (char= (char (symbol-name sym) 0) #\&))

;;; Binding a variable in a preprocessor environment
;;;
;;; Variables can be bound in a preprocessor environment by any of the lambda or
;;; let special forms.  This function takes three arguments:  a variable name,
;;; a list of variables that are declared special within the special form, and
;;; a preprocessor frame.  It determines if the variable should be special
;;; or lexical; special if the variable has been proclaimed that way or is on the
;;; special variable list, lexical otherwise.
;;;
;;; If the variable is special, it has already been bound in the preprocessor frame
;;; by declare-special-variables.  If it is lexical, the function binds it
;;; in the preprocessor frame.  The function then preprocesses the variable and
;;; returns it.
;;;
(defun preprocessor-bind-variable (variable-id special-variables frame)
  (let ((idn (require-bindable-symbol variable-id)))
    (let ((is-special (or (proclaimed-special-p idn)
                          (member idn special-variables))))
      (unless is-special
        (bind-in-frame idn 'unspecial frame :variable))
      (preprocess idn frame))))


;;; When a new frame is created by a lambda or let special form, there might be
;;; some special variable declarations.  These declarations have two effects:
;;; they affect the bindings in the new frame, and they affect variable references
;;; in the new frame.
;;;
;;; This procedure takes care of variable references by indicating to the preprocessor
;;; that the variables are to be considered special.
;;;
(defun declare-special-variables (special-variables frame)
  (mapcar
       #'(lambda (special-variable) (bind-in-frame special-variable 'special frame :variable))
       special-variables)
  t)

;;; Preprocessing FLET expressions.
;;;
(defun preprocess-flet (exp env)
  (require-n-arguments exp 1)
  (let*
    ((flet-bindings (let-bindings exp))
     (flet-body (let-body exp))
     (inner-frame (adjoin-function-frame env))
     (preprocessed-bindings (preprocess-flet-bindings flet-bindings inner-frame))
     (preprocessed-body (preprocess-sequence flet-body inner-frame)))
    `(flet ,preprocessed-bindings  ,@preprocessed-body)))

(defun preprocess-flet-bindings (binding-list frame)
  (when binding-list
    (let ((binding (first binding-list)))
      (when (< (length binding) 2)
        (error "FLET binding too short."))
      (let* ((name (car binding))
             (named-lambda-expr `(INTERPRETER-NAMED-LAMBDA ,name ,@(cdr binding))))
        (bind-in-frame name '(FUNCTION . NIL) frame :function)
        (cons `(,name ,(preprocess `(function ,named-lambda-expr) (nframe-parent frame)))
              (preprocess-flet-bindings (rest binding-list) frame))))))


;;; Preprocessing LABELS expressions.
;;;
(defun preprocess-labels (exp env)
  (require-n-arguments exp 1)
  (let* ((labels-bindings (let-bindings exp))
         (labels-body (let-body exp))
         (frame (adjoin-function-frame env)))
    (labels-extend-preprocessor-frame labels-bindings frame)
    (let ((preprocessed-bindings (preprocess-labels-bindings labels-bindings frame))
          (preprocessed-body (preprocess-sequence labels-body frame)))
      `(labels ,preprocessed-bindings ,@preprocessed-body))))

(defun labels-extend-preprocessor-frame (binding-list frame)
  (when binding-list
    (let ((binding (first binding-list)))
      (when (< (length binding) 2)
        (error "LABELS binding too short."))
      (let* ((name (car binding)))
        (bind-in-frame name '(FUNCTION . NIL) frame :function))
      (labels-extend-preprocessor-frame (rest binding-list) frame))))

(defun preprocess-labels-bindings (binding-list frame)
  (when binding-list
    (let ((binding (first binding-list)))
      (let* ((name (car binding))
             (named-lambda-expr `(INTERPRETER-NAMED-LAMBDA ,name ,@(cdr binding))))
        (cons `(,name ,(preprocess `(FUNCTION ,named-lambda-expr) frame))
              (preprocess-labels-bindings (rest binding-list) frame))))))


;;; Preprocessing IF expressions
;;;
(defun preprocess-if (exp env)
  (require-n-arguments exp 2 3)
  (let ((predicate (if-predicate exp))
        (consequent (if-consequent exp))
        (alternate (if-alternate exp)))
    `(if ,(preprocess predicate env)
         ,(preprocess consequent env)
         ,(preprocess alternate env))))


;;; Preprocessing QUOTE expressions
;;;
(defun preprocess-quote (exp env)
  (declare (ignore env))
  (require-n-arguments exp 1 1)
  `(quote ,(quote-object exp)))


;;; Preprocessing SETQ expressions
;;;
(defun preprocess-setq (exp env)
  `(setq ,@(preprocess-setq-args (setq-args exp) env)))

(defun preprocess-setq-args (args env)
  (cond
    ((= (length args) 0) nil)
    ((= (length args) 1)
     (error "Odd number of arguments to setq"))
    (t
     (let ((var (car args))
           (form (cadr args)))
       (unless (symbolp var)
         (error "Argument to setq not a variable"))
       `(,(preprocess-variable var env)
         ,(preprocess form env)
         ,@(preprocess-setq-args (cddr args) env))))))


;;; Preprocessing MULTIPLE-VALUE-CALL expressions
;;;
(defun preprocess-multiple-value-call (exp env)
  (require-n-arguments exp 1)
  (let ((function (multiple-value-call-function exp))
        (forms (multiple-value-call-forms exp)))
    `(multiple-value-call ,(preprocess function env) ,@(preprocess-sequence forms env))))


;;; Preprocessing MULTIPLE-VALUE-PROG1 expressions
;;;
(defun preprocess-multiple-value-prog1 (exp env)
  (require-n-arguments exp 1)
  (let ((first-form (multiple-value-prog1-first-form exp))
        (other-forms (multiple-value-prog1-other-forms exp)))
    `(multiple-value-prog1 ,(preprocess first-form env)
                           ,@(preprocess-sequence other-forms env))))


;;; Preprocessing EVAL-WHEN expressions
;;;
(defun preprocess-eval-when (exp env)
  (require-n-arguments exp 1)
  (let* ((situation-list (eval-when-situation-list exp))
         (keyword-situation-list (mapcar #'(lambda (symbol)
                                             (intern (symbol-name symbol) (find-package "KEYWORD")))
                                         situation-list))
         (forms (eval-when-forms exp)))
    (let ((losers
            (member-if-not #'(lambda (situation) (member situation '(:compile :load :eval)))
                           keyword-situation-list)))
      (when losers
        (error "Unrecognized symbol ~S in times-list of ~S" (first losers) exp)))
    `(eval-when ,keyword-situation-list ,@(preprocess-sequence forms env))))


;;; Preprocessing DECLARE expressions
;;;
(defun preprocess-declare (exp env)
  (declare (ignore env))
  (format *error-output* "~&>>WARNING: Attempt to evaluate declaration ~S." exp)
  `(self-evaluating :declaration))


;;; Preprocessing THE expressions
;;;
;;; For now, be excruciatingly forgiving with value-type
;;;
(defun preprocess-the (exp env)
  (require-n-arguments exp 2 2)
  (let ((value-type (the-value-type exp))
        (form (the-form exp)))
    `(the ,value-type ,(preprocess form env))))



;;; SECOND PASS
;;;
;;; The second pass of the interpreter evaluates an intermediate-expression generated
;;; by the preprocessor.
;;;
;;; Evaluation is traced by a history mechanism.
;;;
;;; Dispatching by main-eval
;;; Forms which immediately return values:
;;;   SELF-EVALUATING, LEXICAL-VARIABLE, SPECIAL-VARIABLE, FREE-VARIABLE,
;;;   REGULAR-FUNCTION, LEXICAL-FUNCTION, DESCRIBE-FRAME, QUOTE, TAGBODY,
;;;   SETQ, MULTIPLE-VALUE-CALL, FUNCALL, LEXICAL-CLOSURE
;;;
;;; Forms which recurse into subproblems and/or reductions:
;;;   IF, BLOCK, CATCH, UNWIND-PROTECT, PROGN, PROGN-WITH-FRAME, PROGV,
;;;   COMPILER-LET, LET, LET*, FLET, LABELS, MULTIPLE-VALUE-PROG1, EVAL-WHEN,
;;;   THE
;;;
;;; Forms which do not return:
;;;   RETURN-FROM, GO, THROW
;;;
;;; Note that EVAL-SEQUENCE can return a value or recurse.
;;;
;;; It seems to me that MULTIPLE-VALUE-CALL and FUNCALL should turn into
;;; reductions. - JRM


(eval-when (global:compile)                     ;
  (format *trace-output* "~&Compiling second pass."))

(defsubst main-eval (iexp env &optional nohook)
  (incf *eval-count*)
  (if (and *evalhook* (not nohook))
      (relinquish-to-evalhook iexp env)
      (funcall (intermediate-expression-eval-procedure iexp)
               (intermediate-expression-preprocessed-form iexp)
               iexp
               env)))
;         (unless eval-procedure
;           (ferror "internal: unrecognized tag ~S for dispatch in main-eval" (car expr)))

(defsubst eval-subproblem (exp env &optional nohook)
  (unless *evalhook*
    (add-subproblem-to-history exp *ki-call-history*))
  (main-eval exp env nohook))

(defsubst eval-reduction (exp env &optional nohook)
  (unless *evalhook*
    (add-reduction-to-history exp *ki-call-history*))
  (main-eval exp env nohook))

(defun second-pass-eval (exp &optional nohook)
    (eval-subproblem exp (make-empty-environment) nohook))
;  (let ((*ki-call-history* (make-call-history)))

(defun eval-sequence (exps env)
  (cond ((null exps)
         (prog1 NIL
                (pop-subproblem)))
        ((= (length exps) 1)
         (eval-reduction (car exps) env))
        (t
         (eval-subproblem (car exps) env)
         (eval-sequence (cdr exps) env))))

;; This is really where functions are "applied".  (Function application preprocesses
;; to a FUNCALL intermediate form.)  If *ki-evalhook-first-position-functions* is nil,
;; don't hook the evaluation of the function.
;;
(defun eval-funcall (exp iform env)
  (let* ((function (eval-subproblem (cadr exp) env (not *ki-evalhook-first-position-functions*)))
         (arguments (mapcar #'(lambda (expr) (eval-subproblem expr env))
                            (cddr exp))))
    (if *applyhook*
        (relinquish-to-applyhook function arguments (intermediate-expression-preprocessor-env iform) env)
        (multiple-value-prog1 (apply function arguments) (pop-subproblem)))))


(defun eval-lexical-closure (exp iform env)
  (declare (ignore exp))
  (make-lexical-closure iform env))

(defun lookup-eval-procedure (tag)
  ;; Avoid screw of having to recompile this when changing
  ;; second pass eval forms by making this link through
  ;; the function cell.
  (case tag
    (self-evaluating      'eval-self-evaluating)
    (lexical-variable     'eval-lexical-variable)
    (special-variable     'eval-special-variable)
    (free-variable        'eval-free-variable)
    (regular-function     'eval-regular-function)
    (lexical-function     'eval-lexical-function)
    (lexical-closure      'eval-lexical-closure)
    (funcall              'eval-funcall)
    (progn-with-frame     'eval-progn-with-frame)
    (describe-frame       'eval-describe-frame)
    (block                'eval-block)
    (catch                'eval-catch)
    (compiler-let         'eval-compiler-let)
    (eval-when            'eval-eval-when)
    (flet                 'eval-flet)
    (go                   'eval-go)
    (if                   'eval-if)
    (labels               'eval-labels)
    (let                  'eval-let)
    (let*                 'eval-let*)
    (multiple-value-call  'eval-multiple-value-call)
    (multiple-value-prog1 'eval-multiple-value-prog1)
    (progn                'eval-progn)
    (progv                'eval-progv)
    (quote                'eval-quote)
    (return-from          'eval-return-from)
    (setq                 'eval-setq)
    (tagbody              'eval-tagbody)
    (throw                'eval-throw)
    (the                  'eval-the)
    (unwind-protect       'eval-unwind-protect) ;
    (otherwise            nil)))

(defun eval-self-evaluating (exp iform env)
  (declare (ignore iform))
  (declare (ignore env))
  (prog1 (cadr exp)
         (pop-subproblem)))

(defun eval-lexical-variable (exp iform env)
  (declare (ignore iform))
  (prog1 (funcall (caddr exp) env)
         (pop-subproblem)))
;  (cdr (lookup-binding-in-environment (cadr exp) env :variable)))

(defun eval-special-variable (exp iform env)
  (declare (ignore iform))
  (declare (ignore env))
  (prog1 (symbol-value (cadr exp))
         (pop-subproblem)))

(defun eval-free-variable (exp iform env)
  (declare (ignore iform))
  (declare (ignore env))
  (let ((variable (cadr exp)))
    (if *ki-allow-free-variables*
        (symbol-value variable)
        ;; Maybe the variable became special since we did pass one.
        (if (proclaimed-special-p variable)
            ;; Should we bash the form?
            (symbol-value variable)
            (raise-interpreter-error
              (list "Free reference made to symbol ~S, attempting to evaluate it."
                    (cadr exp))
              (list #'(lambda ()
                        (symbol-value (cadr exp)))
                    "Use the dynamic value of ~S."
                    (cadr exp))
              (list #'(lambda ()
                        (proclaim-special (cadr exp))
                        (symbol-value (cadr exp)))
                    "Proclaim ~S special, and use its dynamic value."
                    (cadr exp))
              (list #'(lambda ()
                        (eval (prompt-user-for-form)))
                    "Evaluate something else instead."))))))

(defun eval-regular-function (exp iform env)
  (declare (ignore iform))
  (declare (ignore env))
  (prog1 (symbol-function (cadr exp))
         (pop-subproblem)))

(defun eval-lexical-function (exp iform env)
  (declare (ignore iform))
  (prog1 (cdr (lookup-binding-in-environment (cadr exp) env :function))
         (pop-subproblem)))


;; DESCRIBE-FRAME
;;
(defun eval-describe-frame (exp ignore env)
  (if (null env)
      (progn
        (terpri)
        (princ "NIL is the null lexical environment.")
        (print-full-history *trace-output* *ki-call-history*)
        (print-call-history *trace-output* *ki-call-history*)
        (values))
      (progn
        (describe-defstruct env)
        (eval-describe-frame exp NIL (nframe-parent env)))))

;; IF
;;
(defun eval-if (exp ignore env)
  (let ((predicate (if-predicate exp))
        (consequent (if-consequent exp))
        (alternate (if-alternate exp)))
    (if (eval-subproblem predicate env)
        (eval-reduction consequent env)
        (eval-reduction alternate env))))


;; QUOTE
;;
(defun eval-quote (exp ignore env)
  (declare (ignore env))
  (prog1 (quote-object exp)
         (pop-subproblem)))


;; Block
;;
;; Bind the block's name to a lexical closure, a function of one argument.  During
;; the dynamic extent of the block, the function exits the block and returns its
;; argument.  After the block's time is up, the function raises an error.
;;
(defun eval-block (exp ignore env)
  (let ((block-name (block-name exp))
        (block-body (block-body exp))
        (tag (prog1 (gensym 'block-) (gensym 'g))))
    (let* ((return-fn
             #'(lambda (value) (throw tag value)))
           (expired-fn
             #'(lambda (value)
                 (declare (ignore value))
                 (ferror "The dynamic extent of block ~S has expired." block-name)))
           (new-env (adjoin-block-frame env)))
      (bind-in-frame block-name return-fn new-env :block)
      (unwind-protect
          (catch tag (eval-sequence block-body new-env))
        (rplacd (lookup-binding-in-environment block-name new-env :block) expired-fn)))))


;; RETURN-FROM
;;
;; Look up the block's exit function, and funcall it.
;;
(defun eval-return-from (exp ignore env)
  (let ((block-name (return-from-name exp))
        (value (return-from-result exp)))
    (let ((tag-binding (lookup-binding-in-environment block-name env :block)))
      (unless tag-binding
        (ferror "internal: ~S binding not found in eval-return-from" block-name))
      (funcall (cdr tag-binding) (eval-subproblem value env)))))


;; TAGBODY
;;
;; All throws are to the top of the tagbody.
;;;
(defun eval-tagbody (exp ignore env)
  (let* ((tagbody-body (tagbody-body exp))
         (gotags (extract-gotags tagbody-body))
         (tagbody-name (prog1 (gensym 'TAGBODY-) (gensym 'g)))
         (inner-frame (adjoin-gotag-frame env)))
    (bind-gotags gotags tagbody-name inner-frame)
    (unwind-protect
        (eval-tagbody-dispatch 'start tagbody-name tagbody-body inner-frame)
      (rebind-gotags! gotags inner-frame))))

(defun eval-tagbody-dispatch (instruction tagbody-name body env)
  (tagbody
   eval-next-instruction
      (let ((next-instruction
              (catch tagbody-name
                (cond ((eq instruction 'start)
                       (eval-tagbody-body body env))
                      ((not (consp instruction))
                       (eval-tagbody-dispatch-error))
                      ((eq (car instruction) 'goto)
                       (eval-tagbody-body (member (cdr instruction) body) env))
                      (t
                       (eval-tagbody-dispatch-error))))))
        (if (eq next-instruction 'halt)
            ;;(prog1 NIL (pop-subproblem))
            (progn (pop-subproblem) (go exit))
            ;; Ideally, we would tail recurse here, but on the LAMBDA, we
            ;; have to make this happen like a do loop.
            ;;(eval-tagbody-dispatch next-instruction tagbody-name body env))
            (progn (setq instruction next-instruction)
                   (go eval-next-instruction))))
   exit
        ))

(defun eval-tagbody-body (body env)
  (cond
    ((null body)
     'halt)
    ((intermediate-expression-p (car body))
     (eval-subproblem (car body) env)
     (eval-tagbody-body (cdr body) env))
    (t
     (eval-tagbody-body (cdr body) env))))

(defun extract-gotags (body)
  (remove-if-not #'(lambda (exp) (or (symbolp exp) (numberp exp))) body))

;; Bind each gotag to a lexical closure which throws to the tagbody's dispatch
;; procedure.
;;
(defun bind-gotags (vars tagbody-name frame)
  (when vars
    (let ((throw-fn #'(lambda () (throw tagbody-name `(goto . ,(car vars))))))
      (bind-in-frame (car vars) throw-fn frame)
      (bind-gotags (cdr vars) tagbody-name frame))))

;; When the dynamic extent of the tagbody is over, rebind each gotag to a lexical
;; closure which raises an error.
;;
(defun rebind-gotags! (vars env)
  (when vars
    (rplacd (lookup-binding-in-environment (car vars) env :gotag)
            #'(lambda () (ferror "The dynamic extent of GO tag ~S has expired." (car vars))))
    (rebind-gotags! (cdr vars) env)))


(defun eval-tagbody-dispatch-error ()
  (error "internal: unrecognized instruction in tagbody dispatch"))


;; GO
;;
;; Look up the tag's binding.  If there isn't one, raise an error.  If there is, call
;; the function it is bound to.
;;
(defun eval-go (exp ignore env)
  (let ((gotag-binding (lookup-binding-in-environment (go-gotag exp) env :gotag)))
    (unless gotag-binding
      (ferror "~S is not a lexically visible GO tag." (go-gotag exp)))
    (funcall (cdr gotag-binding))))


;; CATCH
;;
(defun eval-catch (exp ignore env)
  (let ((tag (catch-tag exp))
        (forms (catch-forms exp)))
    (let ((evaluated-tag (eval-subproblem tag env)))
      (unless (symbolp evaluated-tag)
        (error "CATCH tag does not evaluate to a symbol."))
      (catch evaluated-tag
        (eval-sequence forms env)))))


;; THROW
;;
(defun eval-throw (exp ignore env)
  (let ((tag (throw-tag exp))
        (result (throw-result exp)))
    (let ((evaluated-tag (eval-subproblem tag env)))
      (loop
        (if (symbolp evaluated-tag)
            (return)
            (setq evaluated-tag
                  (cerror t nil nil "THROW tag does not evaluate to a symbol."))))
      (throw evaluated-tag (eval-subproblem result env)))))


;; UNWIND-PROTECT
;;
(defun eval-unwind-protect (exp ignore env)
  (let ((protected-form (unwind-protect-protected-form exp))
        (cleanup-forms (unwind-protect-cleanup-forms exp)))
    (unwind-protect
        (eval-subproblem protected-form env)
      (eval-sequence cleanup-forms env))))

;; PROGN
;;
(defun eval-progn (exp ignore env)
  (let ((forms (progn-forms exp)))
    (eval-sequence forms env)))


;; PROGN-WITH-FRAME
;;
(defun eval-progn-with-frame (exp ignore env)
  (let ((forms (progn-forms exp))
        (inner-frame (adjoin-junk-frame env)))
    (eval-sequence forms inner-frame)))


;; PROGV
;;
;; No frame is created because progv doesn't affect lexical environments.
(defun eval-progv (exp ignore env)
  (let* ((symbols (progv-symbols exp))
         (values (progv-values exp))
         (forms (progv-forms exp))
         (evaluated-symbols (mapcar #'require-bindable-symbol (eval-subproblem symbols env)))
         (evaluated-values (eval-subproblem values env)))
    (progv `(list ,@evaluated-symbols)
           `(list ,@evaluated-values)
           (eval-sequence forms env))))



;; COMPILER-LET, LET, and LET*
;;
(defun eval-compiler-let (exp ignore env)
  (evaluate-general-let 'LET exp env))

(defun eval-let (exp ignore env)
  (evaluate-general-let 'LET exp env))

(defun eval-let* (exp ignore env)
  (evaluate-general-let 'LET* exp env))

(defun evaluate-general-let (special-form exp env)
  (let* ((let-bindings (let-bindings exp))
         (let-body (let-body exp))
         (loser-frame (adjoin-variable-frame env))      ;See note preceding PREPROCESS-GENERAL-LET
         (inner-frame (adjoin-variable-frame loser-frame)))
    (evaluate-let-bindings special-form let-bindings inner-frame)
    (progv (nframe-special-variables inner-frame) (nframe-special-values inner-frame)
      (eval-sequence let-body inner-frame))))

(defun evaluate-let-bindings (special-form bindings env)
  (when bindings
    (let* ((binding (car bindings))
           (var (let-binding-var binding))
           (value (let-binding-value binding))
           (evaluated-value
             (case special-form
               (LET  (if value (eval-subproblem value (nframe-parent env))))
               (LET* (if value (eval-subproblem value env) nil))))
           (preprocessed-var (intermediate-expression-preprocessed-form var))
           (var-type (car preprocessed-var))
           (var-id (cadr preprocessed-var)))
      (case var-type
        (lexical-variable (bind-in-frame var-id evaluated-value env :variable))
        (special-variable (declare-special-variable-within-frame var-id evaluated-value env)))
      (if (and (eq special-form 'LET*) (eq (car preprocessed-var) 'special-variable))
          (progv (list (cadr preprocessed-var)) (list evaluated-value)
            (evaluate-let-bindings special-form (cdr bindings) env))
        (evaluate-let-bindings special-form (cdr bindings) env)))))

;(defun let-bind-variable (var value frame)
;  (let* ((preprocessed-var (intermediate-expression-preprocessed-form var))
;        (var-type (car preprocessed-var))
;        (var-id (cadr preprocessed-var)))
;    (case var-type
;      (lexical-variable (lexically-bind-in-frame var-id value frame))
;      (special-variable (specially-bind-in-frame var-id value frame))
;      (otherwise (error "internal: let-bind-variable")))))


;;; FLET
;;;
(defun eval-flet (exp ignore env)
  (let ((flet-bindings (let-bindings exp))
        (flet-body (let-body exp))
        (inner-frame (adjoin-function-frame env)))
    (evaluate-flet-bindings flet-bindings inner-frame)
    (eval-sequence flet-body inner-frame)))

(defun evaluate-flet-bindings (binding-list frame)
  (when binding-list
    (let* ((binding (car binding-list))
           (name (first binding))
           (closure (second binding))
           (evaluated-closure (eval-subproblem closure (nframe-parent frame))))
      (bind-in-frame name evaluated-closure frame :function)
      (evaluate-flet-bindings (cdr binding-list) frame))))


;;; LABELS
;;;
(defun eval-labels (exp ignore env)
  (let* ((labels-bindings (let-bindings exp))
         (labels-body (let-body exp))
         (inner-frame (adjoin-function-frame env)))
    (make-temporary-labels-bindings labels-bindings inner-frame)
    (evaluate-labels-bindings labels-bindings inner-frame)
    (eval-sequence labels-body inner-frame)))

(defun make-temporary-labels-bindings (binding-list frame)
  (when binding-list
    (let* ((binding (car binding-list))
           (name (first binding)))
      (bind-in-frame name 'fill-in-the-blank frame :function)
      (make-temporary-labels-bindings (cdr binding-list) frame))))

(defun evaluate-labels-bindings (binding-list inner-env)
  (when binding-list
    (let* ((binding (car binding-list))
           (name (first binding))
           (closure (second binding))
           (evaluated-closure (eval-subproblem closure inner-env)))
      (rplacd (lookup-binding-in-environment name inner-env :function) evaluated-closure)
      (evaluate-labels-bindings (cdr binding-list) inner-env))))


;;; SETQ
;;;
(defun eval-setq (exp ignore env)
  (let ((args (setq-args exp)))
    (if args (evaluate-setq-loop (cdr exp) env) nil)))

(defun evaluate-setq-loop (args env)
  (let* ((variable (intermediate-expression-preprocessed-form (first args)))
         (variable-type (car variable))
         (variable-id (require-bindable-symbol (cadr variable) "set"))
         (form (second args))
         (value (eval-subproblem form env))
         (rest (cddr args)))
    (case variable-type
      (lexical-variable (rplacd (lookup-binding-in-environment variable-id env :variable) value))
      (special-variable (set variable-id value))
      (free-variable (set variable-id value))
      (otherwise (error "internal: evaluate-setq-loop")))
    (if rest
        (evaluate-setq-loop rest env)
        (prog1 value (pop-subproblem)))))


;;; MULTIPLE-VALUE-CALL
;;;
(defun eval-multiple-value-call (exp ignore env)
  (let* ((function (multiple-value-call-function exp))
         (forms (multiple-value-call-forms exp))
         (evaluated-function (eval-subproblem function env)))
    (unless (functionp evaluated-function)
      (error "First argument to MULTIPLE-VALUE-CALL not a function"))
    (let ((arguments (eval-multiple-value-call-args forms env)))
      ;; Evaluation can return multiple values, too.
      (multiple-value-prog1 (apply evaluated-function arguments)
                            (pop-subproblem)))))

(defun eval-multiple-value-call-args (forms env)
  (when forms
    (append (multiple-value-list (eval-subproblem (car forms) env))
            (eval-multiple-value-call-args (cdr forms) env))))


;;; MULTIPLE-VALUE-PROG1
;;;
(defun eval-multiple-value-prog1 (exp ignore env)
  (let* ((first-form (multiple-value-prog1-first-form exp))
         (other-forms (multiple-value-prog1-other-forms exp))
         (evaluated-first-form (multiple-value-list (eval-subproblem first-form env))))
    (eval-sequence other-forms env)
    (values-list evaluated-first-form)))


;;; EVAL-WHEN
;;;
(defun eval-eval-when (exp ignore env)
  (let ((situation-list (eval-when-situation-list exp))
        (forms (eval-when-forms exp)))
    (when (member :eval situation-list)
      (eval-sequence forms env))))


;;; THE
;;;
(defun eval-the (exp ignore env)
  (let ((value-type (the-value-type exp))
        (form (eval-subproblem (the-form exp) env)))
    (if (typep form value-type)
        (eval-reduction (the-form exp) env)
        (ferror "(THE) Object ~S is not of type ~S"
                form
                value-type))))




;;; APPLY LEXICAL CLOSURE
;;;
;;; This is the interpreter's apply procedure.  It is called with three arguments:
;;; a lexical closure, a list of arguments which have already been evaluated, and
;;; a lexical environment.  The environment is necessary to evaluate initforms and
;;; &aux parameters in the closure's lambda list.
;;;
;;; Apply-lexical-closure creates a new frame.  In this frame, it sequentially binds
;;; the formal parameters of the closure's lambda list to the list of arguments.
;;; After lexical binding is completed, a progv form dynamically binds any special
;;; variables and then evaluates the body of the lexical closure in the new frame's
;;; environment.
;;;

(eval-when (global:compile)
  (format *trace-output* "~&Compiling APPLY."))

(defun apply-lexical-closure (closure arguments env)
  (incf *apply-count*)
  (let ((lambda-list (caddr closure))
        (lambda-body (cdddr closure))
        (inner-frame (adjoin-variable-frame env)))
    (flet ((punt (value) (return-from apply-lexical-closure value)))
      (setq arguments (require-ample-arguments lambda-list arguments closure #'punt))
      (evaluate-lambda-bindings lambda-list arguments inner-frame)
      (progv (nframe-special-variables inner-frame) (nframe-special-values inner-frame)
        (eval-sequence lambda-body inner-frame)))))

;(defun evaluate-lambda-bindings (lambda-list args frame)
;  (let ((actuals args)
;       (required-formals (nice-lambda-list-required lambda-list))
;       (optional-formals (nice-lambda-list-optional lambda-list))
;       (rest-formal (nice-lambda-list-rest lambda-list))
;       (key-formals (nice-lambda-list-key lambda-list))
;       (allow-other-keys (nice-lambda-list-allow-other-keys lambda-list))
;       (aux-parameters (nice-lambda-list-aux lambda-list)))
;    (flet ((next-actual () (pop actuals))
;          (more-actuals () (not (null actuals))))
;      (when (< (length actuals) (length required-formals))
;       (error "internal: not enough actual arguments to lambda expression"))
;      (bind-required-parameters
;       required-formals #'next-actual frame)
;      (bind-optional-parameters
;       optional-formals #'next-actual #'more-actuals frame)
;      (bind-rest-parameter
;       rest-formal actuals frame)
;      (bind-key-parameters
;       key-formals actuals frame allow-other-keys)
;      (when (and (more-actuals) (not rest-formal) (not key-formals))
;       (error "internal: too many actual arguments to lambda expression"))
;      (bind-aux-parameters
;       aux-parameters frame))))

(defun evaluate-lambda-bindings (lambda-list args frame)
  (let ((actuals args)
        (required-formals (nice-lambda-list-required lambda-list))
        (optional-formals (nice-lambda-list-optional lambda-list))
        (rest-formal (nice-lambda-list-rest lambda-list))
        (key-formals (nice-lambda-list-key lambda-list))
        (allow-other-keys (nice-lambda-list-allow-other-keys lambda-list))
        (aux-parameters (nice-lambda-list-aux lambda-list)))
    (flet ((next-actual () (pop actuals))
           (more-actuals () (not (null actuals))))
      (when (< (length actuals) (length required-formals))
        (error "internal: not enough actual arguments to lambda expression"))
      (bind-required-parameters
        required-formals #'next-actual frame)
      (when optional-formals
        (bind-optional-parameters
          optional-formals #'next-actual #'more-actuals frame))
      (when rest-formal
        (bind-rest-parameter
          rest-formal actuals frame))
      (when key-formals
        (bind-key-parameters
          key-formals actuals frame allow-other-keys))
      (when (and (more-actuals) (not rest-formal) (not key-formals))
        (error "internal: too many actual arguments to lambda expression"))
      (when aux-parameters
        (bind-aux-parameters
          aux-parameters frame)))))

(defun fast-evaluate-lambda-bindings (lambda-list args frame)
  (let ((actuals args)
        (required-formals (nice-lambda-list-required lambda-list)))
    (flet ((next-actual () (pop actuals)))
      (bind-required-parameters
        required-formals #'next-actual frame))))


(defun require-ample-arguments (lambda-list args closure punt-proc)
  (let* ((number-of-required-args (length (nice-lambda-list-required lambda-list)))
         (number-of-optional-args (length (nice-lambda-list-optional lambda-list)))
         (minimum number-of-required-args)
         (maximum (if (or (nice-lambda-list-rest lambda-list)
                          (nice-lambda-list-key lambda-list))
                      nil ;infinite
                      (+ number-of-required-args number-of-optional-args))))
    (cond

      ((< (length args) number-of-required-args)
       (raise-interpreter-error
         (list "Too few arguments to function ~S; ~S provided, ~S required."
               (cadr closure)
               (length args)
               minimum)
         (list #'(lambda ()
                   (funcall punt-proc (eval (prompt-user-for-form))))
               "Pretend the function ran; you type an expression for its value.")))

      ((and maximum (> (length args) maximum))
       (raise-interpreter-error
         (list "Too many arguments to function ~S; ~S allowed, ~S provided."
               (cadr closure)
               maximum
               (length args))
         (list #'(lambda ()
                   (funcall punt-proc (eval (prompt-user-for-form))))
               "Pretend the function ran; you type an expression for its value.")
         (list #'(lambda ()
                   (butlast args (- (length args) maximum)))
               "Truncate the argument list, discarding the last ~S argument~:P."
               (- (length args) maximum))))

      (t
       args))))


; Bind all "required" parameters.
;
(defun bind-required-parameters (formals actual-proc frame)
  (when formals
    (bind-lambda-parameter (car formals) (funcall actual-proc) frame)
    (bind-required-parameters (cdr formals) actual-proc frame)))

; Bind all &optional parameters.
;
; actual-proc pops and returns the next actual parameter.
; more-pred is true if there are more actual parameters.
;
(defun bind-optional-parameters (formals actual-proc more-pred frame)
  (when formals
    (progv (nframe-special-variables frame) (nframe-special-values frame)
      (let* ((optional-parameter (car formals))
             (var (optional-parameter-var optional-parameter))
             (initform (optional-parameter-initform optional-parameter))
             (svar (optional-parameter-svar optional-parameter)))
        (if (funcall more-pred)
            (progn
              (bind-lambda-parameter var (funcall actual-proc) frame)
              (when svar
                (bind-lambda-parameter svar t frame)))
            (let ((initial-value
                    (if initform (eval-subproblem initform frame) nil)))
              (bind-lambda-parameter var initial-value frame)
              (when svar
                (bind-lambda-parameter svar nil frame))))))
    (bind-optional-parameters (cdr formals) actual-proc more-pred frame)))

; Bind the &rest parameter.
;
(defun bind-rest-parameter (formal actual-list frame)
  (when formal
    (bind-lambda-parameter formal actual-list frame)))

; Bind all &key parameters.
;
;    1. The list of arguments is converted to an a-list matching keywords to values.
;    2. If &allow-other-keys appeared in the lambda list, or the a-list contains
;       a pair matching the keyword :allow-other-keys to a true value, the variable
;       a-o-k is bound to true; otherwise it is bound to nil.
;    3. Each parameter is processed in sequence.
;       (a) The keyword for the parameter is looked up in the a-list of arguments
;       (b) If an argument pair is found,
;           1. The parameter variable is bound to the corresponding argument value.
;           2. All argument pairs in the a-list with that particular keyword are
;              removed from the a-list.  It is assumed that there are no repeated
;              keywords in the parameter list.
;           3. The parameter's svar, if present, is bound to T.
;       (c) If an argument pair is not found,
;           1. The parameter's initform is evaluated.
;           2. The parameter's variable is bound to the resulting value.
;           3. The parameter's svar, if present, is bound to NIL.
;    4. If a-o-k is nil, and there are argument pairs remaining on the a-list, an
;       error is raised.
;
(defun bind-key-parameters (formals actual-list frame allow-other-keys)
  (when formals
    (let*
      ((a-list
         (convert-argument-list-to-a-list actual-list))
       (a-o-k
         (or allow-other-keys
             (member-if #'(lambda (arg-pair)
                            (and (eq (car arg-pair) :allow-other-keys) (cdr arg-pair)))
                        a-list)))
       (remaining-arg-pairs
         (bind-key-parameters-loop formals a-list frame)))
      (when (and (not a-o-k) remaining-arg-pairs)
        (ferror "Unrecognized parameter ~S in argument list."
                (caar remaining-arg-pairs))))))

(defun bind-key-parameters-loop (formals arg-pair-list frame)
  (if (null formals)
      arg-pair-list
      (let* ((key-parameter (car formals))
             (var (key-parameter-var key-parameter))
             (keyword (key-parameter-keyword key-parameter))
             (initform (key-parameter-initform key-parameter))
             (svar (key-parameter-svar key-parameter))
             (matching-arg-pair (assoc keyword arg-pair-list)))
            (progv (nframe-special-variables frame) (nframe-special-values frame)
              (if matching-arg-pair
                  (progn
                    (bind-lambda-parameter var (cdr matching-arg-pair) frame)
                    (when svar
                      (bind-lambda-parameter svar t frame)))
                  (let ((initial-value
                        (if initform (eval-subproblem initform frame) nil)))
                    (bind-lambda-parameter var initial-value frame)
                    (when svar
                      (bind-lambda-parameter svar nil frame))
                    (bind-key-parameters-loop
                      (cdr formals) arg-pair-list frame))))
            (bind-key-parameters-loop (cdr formals)
                                  (remove-if #'(lambda (pair) (eq (car pair) keyword))
                                             arg-pair-list)
                                  frame))))


(defun bind-aux-parameters (parameters frame)
  (when parameters
    (progv (nframe-special-variables frame) (nframe-special-values frame)
      (let* ((aux-parameter (car parameters))
             (var (aux-parameter-var aux-parameter))
             (initform (aux-parameter-value aux-parameter))
             (initial-value (if initform (eval-subproblem initform frame) nil)))
        (bind-lambda-parameter var initial-value frame)))
    (bind-aux-parameters (cdr parameters) frame)))

(defun convert-argument-list-to-a-list (arglist)
  (cond
    ((null arglist)
     nil)
    ((= (length arglist) 1)
     (error "odd number of keyword arguments"))
    (t
     (let ((keyword (car arglist))
           (value (cadr arglist)))
       (unless (keywordp keyword)
         (error "&key argument not a keyword"))
       (cons (cons keyword value)
             (convert-argument-list-to-a-list (cddr arglist)))))))

(defun bind-lambda-parameter (formal-parameter value frame)
  (let* ((preprocessed-formal (intermediate-expression-preprocessed-form formal-parameter))
         (formal-parameter-type (car preprocessed-formal))
         (formal-parameter-id (cadr preprocessed-formal)))
    (case formal-parameter-type
      (lexical-variable (bind-in-frame formal-parameter-id value frame :variable))
      (special-variable (declare-special-variable-within-frame formal-parameter-id value frame))
;      (special-variable (specially-bind-in-frame formal-parameter-id value frame))
      (otherwise (error "internal: bind-lambda-parameter")))))



;;; PREPROCESSOR ENVIRONMENT ABSTRACTIONS
;;;

(defun preprocessor-variable-binding-type (variable-binding)
  (cdr variable-binding))

(defun preprocessor-funmac-binding-type (funmac-binding)
  (cadr funmac-binding))

(defun preprocessor-funmac-binding-value (funmac-binding)
  (cddr funmac-binding))

(defun preprocessor-block-binding-value (block-binding)
  (cdr block-binding))



;;; LEXICAL CLOSURE WIZARDRY
;;;
;;; This bit of code creates a lexical closure.  In the abstract, a lexical closure
;;; is an object created from a function and an environment.  When the closure is
;;; applied to arguments, it applies the function to the arguments in the environment.
;;;
;;; Here, the lexical closure (the #'(lambda ...) form) is itself enclosed by a
;;; dynamic closure.  This dynamic closure binds two variables: a flag called
;;; interpreter-closure and a variable called original-definition.  The flag
;;; identifies the closure as having been created by the interpreter.  The
;;; original-definition points to the lambda-expression used to define the
;;; lexical closure.
;;;
;;; The safest way to use the interpreter-closure flag, to test if an expression is
;;; one of these lexical closures, is:
;;;      (AND (CLOSUREP exp)
;;;           (FIND-PACKAGE "INTERPRETER")
;;;           (BOUNDP-IN-CLOSURE exp (INTERN "INTERPRETER-CLOSURE" "INTERPRETER"))
;;;           (SYMEVAL-IN-CLOSURE exp (INTERN "INTERPRETER-CLOSURE" "INTERPRETER")))
;;;
;;; The original lambda expression, which is needed by the compiler, can be retrieved
;;; from a lexical closure by:
;;;      (SYMEVAL-IN-CLOSURE exp (INTERN "ORIGINAL-DEFINITION" "INTERPRETER"))
;;;
;;; This method avoids confusing the reader if the new interpreter isn't loaded and
;;; its package doesn't exist.
;;;
;;; The first argument to make-lexical-closure, exp, is an intermediate-expression.
;;; Its original-form is a (FUNCTION (LAMBDA ...)) expression; the FUNCTION part
;;; must be unwrapped to obtain the lambda expression.
;;;
(defun make-lexical-closure (exp env)
  (let ((interpreter-closure t)
        (closure-name (cadr (intermediate-expression-preprocessed-form exp)))
        (original-definition (cadr (intermediate-expression-original-form exp))))
    (declare (special interpreter-closure closure-name original-definition))
    (closure '(interpreter-closure closure-name original-definition)
      #'(lambda (&rest args)
          (setq args (copylist args))
          (let ((interpreter-closure nil))
            (declare (special interpreter-closure))
            (if *ki-allow-free-variables*
                (let ((*ki-allow-free-variables* NIL))
                  (apply-lexical-closure
                    (intermediate-expression-preprocessed-form exp) args env))
                (apply-lexical-closure
                  (intermediate-expression-preprocessed-form exp) args env)))))))

(defun interpreter-closure-p (exp)
  (and (closurep exp)
       (symeval-in-closure exp 'interpreter-closure)))

(defun interpreter-closure-name (closure)
  (symeval-in-closure closure 'closure-name))

(defun name-interpreter-closure (closure name)
  (if (interpreter-closure-p closure)
      (set-in-closure closure 'closure-name name)
      (ferror "internal: name-lexical-closure argument ~S is not a lexical-closure." closure)))

(defun name-of-closure (closure)
  (cond
    ((interpreter-closure-p closure)
     (interpreter-closure-name closure))
    ((SI::COMPILED-FUNCTION-P closure)
     (SI::%P-CONTENTS-OFFSET closure SI::%FEFHI-FCTN-NAME))
    (t
     nil)))



;;; ERRORS
;;;
;;; (require-n-arguments exp n) will cause an error if exp has fewer than n arguments.
;;; (require-n-arguments exp n m) will cause an error if exp has fewer than n or more than m
;;;     arguments.
;;;
;;; An interpreter error is a structure with the following fields:
;;;
;;;     report:  A list of arguments to FORMAT, i.e. a format string and the corresponding
;;;        arguments.
;;;
;;;     proceed-types:  A list of proceed types.  Each proceed type is a list of at
;;;        least two elements:  (<continuation> <format-string> <format-arg>*).
;;;
;;;     history:  The call history at the point where the error was raised.


(defstruct interpreter-error
  report
  proceed-types
  history)

(defun raise-interpreter-error (report-args &rest proceed-types)
  (handle-interpreter-error (make-interpreter-error :report        report-args
                                                    :proceed-types (copy-list proceed-types)
                                                    :history       *ki-call-history*)))

(defun handle-interpreter-error (error-form)
  (if *ki-fancy-error-handler*
      (funcall *ki-fancy-error-handler* error-form)
      (invoke-simple-error-handler error-form)))

(defun nth-proceed-type-report-list (n error-form)
  (cdr (nth n (interpreter-error-proceed-types error-form))))

(defun nth-proceed-type-continuation (n error-form)
  (car (nth n (interpreter-error-proceed-types error-form))))

(defun invoke-simple-error-handler (error-form)
  (format *error-output* "~&>>INTERPRETER ERROR")
  (format *error-output* "~&  History: ")
  (print-call-history *error-output* (interpreter-error-history error-form))
  (let ((proceed-types (interpreter-error-proceed-types error-form))
        (error-report  (interpreter-error-report error-form)))
    (if proceed-types
        (let ((report-list (nth-proceed-type-report-list 0 error-form))
              (continuation (nth-proceed-type-continuation 0 error-form)))
          (apply #'cerror
                 `(,(car report-list)
                   ,(car error-report)
                   ,@(cdr report-list)
                   ,@(cdr error-report)))
          (funcall continuation))
        (funcall #'error error-report))))



;;; PROCEED TYPES
;;;
;;; These are various, random functions used to make implementing proceed types a
;;; little bit easier.

(defun prompt-user-for-form ()
  (format *query-io* "~&Type a new form: ")
  (read *query-io*))



;;; INSURANCE
;;;

(defvar *allow-losing-&-symbols?* t
  "If NIL, any symbol beginning with & is not legal to bind.
If non-nil, only real lambda-list keywords are illegal.")

(defun require-bindable-symbol (var &optional (verb "bind"))
  (cond ((not (symbolp var))
         (ferror "Attempt to ~A ~S; a symbol is required" verb var))
        ((lambda-list-keyword-p var)
         (ferror "Attempt to ~A the lambda-list-keyword ~S" verb var))
        ((and (not *allow-losing-&-symbols?*)
              (looks-like-lambda-list-keyword? var))
         (cerror "Do it anyway."
                 "Attempt to ~A what appears to be a lambda-list-keyword ~s."
                 verb var)
         var)
        ((eq var 'nil)
         (ferror "Nihil ex nihil: Don't ~A ~S" verb var))
        ((eq var 't)
         (ferror "Veritas aeternae: Don't ~A ~S" verb var))
        ((keywordp var)
         (ferror "Attempt to ~A the keyword ~S" verb var))
        ((constantp var)
         (ferror "Attempt to ~A the constant ~S" verb var))
        (t
         var)))

(defun require-n-arguments (expr min &optional (max nil max-specified))
  (let* ((name (car expr))
         (arguments (cdr expr))
         (length (length arguments)))
    (cond ((< length min)
           (ferror "~S expression requires at least ~S argument~:P." name min))
          ((and max-specified (> length max))
           (ferror "Too many arguments in ~S expression." name))
          (t
           "okay"))))



;;; DECLARATIONS
;;;
;;; "Declarations may occur only at the beginning of the bodies of certain special
;;; forms; ... It is an error to attempt to evaluate a declaration."  (CL, p. 154)
;;;
;;; The following procedure examines all declarations and documentation strings
;;; at the top of a list of expressions.  It returns a structure of type "decl-info"
;;; which contains the following information:
;;;
;;;    specials - list of variables declared "special"
;;;    body     - the expressions following the declarations/documentation

(defun examine-declarations (exprs env)
  (multiple-value-bind (body declarations) (GOBBLE-DECLARATIONS exprs NIL env)
    (make-decl-info :specials (extract-special-variable-list declarations)
                    :body     body)))

(defun extract-special-variable-list (declarations)
  (when declarations
    (append (extract-special-variables (car declarations))
            (extract-special-variable-list (cdr declarations)))))

(defun extract-special-variables (declaration)
  (let ((special-variables nil))
    (mapcar
         #'(lambda (decl-spec)
             (if (special-decl-spec-p decl-spec)
                 (setq special-variables
                       (append special-variables
                               (special-vars-in-decl-spec decl-spec)))))
         declaration)
    special-variables))

(defun special-vars-in-decl-spec (decl-spec)
  (mapcar
       #'(lambda (element)
           (if (symbolp element)
               element
               (error "element of special decl-spec not a symbol")))
       (cdr decl-spec)))

(defun special-decl-spec-p (decl-spec)
  (and (listp decl-spec)
       (eq (car decl-spec) 'special)))

;;; Takes two arguments:
;;;    List of forms to gobble
;;;    Flag indicating whether or not to gobble documentation strings too
;;;
;;; Returns three values:
;;;    Body (forms after declarations and documentation strings)
;;;    List of declarations
;;;    Documentation string (if applicable)
;;;
;;; Weird cases, if gobble-doc-strings-too:
;;;    If there is more than one string in the forms, the second string and
;;;       everything following it (including declarations) are part of the body.
;;;    If the body is null, and a documentation string is present, a list
;;;       containing the documentation string is the body.
;;;    These cases follow from a strict interpretation of decl-spec/doc-string
;;;    syntax described in Common Lisp.
;;;
;;; Macroexpand is used to handle the obnoxious case of macros which expand into
;;;    declarations or documentation strings.

(defun gobble-declarations (list-of-forms &optional gobble-doc-strings-too env)
  (let ((declarations '())
        (doc-string nil)
        (body nil))
    (loop
      (let ((form (MACROEXPAND (car list-of-forms) env)))
        (cond ((and (listp form) (eq (car form) 'declare))
               (push form declarations))
              ((and (stringp form) gobble-doc-strings-too (not doc-string))
               (setq doc-string form))
              (t
               (setq body list-of-forms)
               (return))))
      (pop list-of-forms))
    (when (and (null body) doc-string) (setq body (list doc-string)))
    (values body (reverse declarations) doc-string)))



;;; CALL HISTORY
;;;

(defvar *ki-subproblem-list-size* 20
  "Initial size of the interpreter's call-history subproblem list.")

(defvar *ki-reduction-ring-size* 10
  "Depth of tail recursions allowed before a reduction ring begins to overwrite itself.")

(defun make-call-history ()
  (make-array '(0) :fill-pointer T :adjustable T))

;(defun reset-call-history (history)
;  (setq history NIL))

(defun add-subproblem-to-history (subproblem-expression history)
  (return-from add-subproblem-to-history NIL)
  (format *trace-output* "~&Enter subproblem: ~S" NIL)
  (let ((reduction-ring (make-ring *ki-reduction-ring-size*)))
    (push-onto-ring subproblem-expression reduction-ring)
    (vector-push-extend reduction-ring history))
  T)

(defun add-reduction-to-history (reduction-expression history)
  (return-from add-reduction-to-history NIL)
  (let ((reduction-ring (high-element history)))
    (push-onto-ring reduction-expression reduction-ring))
  T)

(defun pop-subproblem-from-history (history)
  (return-from pop-subproblem-from-history NIL)
  (format *trace-output* "~&Exit subproblem: ~S"
          (high-element (ring-rep-elements (high-element history))))
  (vector-pop history)
  T)

(defun pop-subproblem ()
  (pop-subproblem-from-history *ki-call-history*))

(defun print-full-history (ignore ignore)
  NIL)

(defun high-element (array)
  (aref array (1- (fill-pointer array))))

;(defun print-full-history (stream history)
;  (return-from print-full-history NIL)
;  (flet
;    ((report-any-form (intermediate-expression)
;       (format stream
;              "~&Did a ~S form."
;              (car (intermediate-expression-preprocessed-form intermediate-expression))))
;     (report-next-subproblem-level ()
;       (format stream
;              "~&---Yow! Another subproblem deeper!---")))
;    (walk-call-history history #'report-any-form NIL #'report-next-subproblem-level)))

(defun print-call-history (ignore ignore)
  NIL)

;(defun print-call-history (stream history)
;  (return-from print-call-history NIL)
;  (flet
;    ((report-form (intermediate-expression)
;       (let ((preprocessed-form
;              (intermediate-expression-preprocessed-form intermediate-expression)))
;        (when (eq (car preprocessed-form) 'FUNCALL)
;          (let* ((function-form (intermediate-expression-preprocessed-form (cadr preprocessed-form)))
;                 (function-name (case (car function-form)
;                                  (lexical-function (cadr function-form))
;                                  (regular-function (cadr function-form))
;                                  (lexical-closure  (cadr function-form))
;                                  (otherwise        :unrecognized))))
;            (format stream "~S  " function-name)))))
;     (report-gap ()
;       (format stream "...  "))
;     (bsp () NIL))
;    (walk-call-history history #'report-form #'report-gap #'bsp)
;    (format stream "Eval")))

;(defun walk-call-history (history proc gap-proc between-subproblems-proc)
;  (declare (ignore gap-proc))
;  (labels
;    ((walk-reduction-ring (reduction-ring)
;       (if (ring-is-reset-p reduction-ring)
;          (funcall proc (active-element reduction-ring))
;          (progn
;            (funcall proc (active-element reduction-ring))
;            (rotate-right reduction-ring)
;            (walk-reduction-ring reduction-ring))))
;     (walk-subproblem-ring (subproblem-ring)
;       (unless (ring-is-reset-p subproblem-ring)
;        (walk-reduction-ring (active-element subproblem-ring))
;        (funcall between-subproblems-proc)
;        (rotate-right subproblem-ring)
;        (walk-subproblem-ring subproblem-ring))))
;    (walk-subproblem-ring history)))





;;; RINGS
;;;
;;;
;;; S (size):  The capacity of the ring; the size of the elements vector.
;;; O (overflow):  While this flag is NIL, all elements that have been pushed
;;;    onto the ring are still there.  If this flag is T, then some entries
;;;    on the ring have been lost.
;;; N (next):  Index of the next slot to fill
;;; V (valid-count):  Number of valid items on the ring.
;;;
;;; The elements of the ring may be obtained by counting V elements backwards
;;; from N-1 (mod S).
;;;

(defstruct ring-rep
  size
  overflow
  next
  valid-count
  elements)

(defun make-ring (size)
  (make-ring-rep :size        size
                 :overflow    NIL
                 :next        0
                 :valid-count 0
                 :elements    (make-array `(,size))))

(defun push-onto-ring (item ring)
  (let ((S (ring-rep-size ring))
        (N (ring-rep-next ring)))
    (cond ((ring-rep-overflow ring)
           (setf (aref (ring-rep-elements ring) N)
                 item)
           (setf (ring-rep-next ring)
                 (inc-mod N S))
           (setf (ring-rep-valid-count ring)
                 (min (+ (ring-rep-valid-count ring) 1) S)))
          ((< N S)
           (setf (aref (ring-rep-elements ring) N) item)
           (incf (ring-rep-next ring))
           (incf (ring-rep-valid-count ring)))
          ((= N S)
           (setf (aref (ring-rep-elements ring) 0) item)
           (setf (ring-rep-next ring) 1)
           (setf (ring-rep-overflow ring) T))))
  T)

(defun pop-from-ring (ring)
  (let ((S (ring-rep-size        ring))
        (N (ring-rep-next        ring))
        (V (ring-rep-valid-count ring)))
    (when (= V 0)
      (error "Popped too far!"))
    (setf (ring-rep-next ring)
          (if (= N 0) (- S 1) (- N 1)))
    (decf (ring-rep-valid-count ring))
    (aref (ring-rep-elements ring) (ring-rep-next ring))))

(defun inc-mod (i base)
  (let ((foo (+ i 1)))
    (if (= foo base) 0 foo)))

(defun reset-ring (ring)
  (setf (ring-rep-overflow    ring) NIL)
  (setf (ring-rep-next        ring) 0)
  (setf (ring-rep-valid-count ring) 0)
  T)

(defun map-ring (proc ring)
  (let ((size     (ring-rep-size ring))
        (foo      (ring-rep-next ring))
        (elements (ring-rep-elements ring)))
    (dotimes (bar (ring-rep-valid-count ring))
      (setq foo (if (zerop foo)
                    (- size 1)
                    (- foo  1)))
      (funcall proc (aref elements foo))))
  (ring-rep-overflow ring))



;;; MACRO EXPANDERS
;;;
;;; These both assume that NIL = null environment.  This could be fixed, though.
;;; Also, macroexpand-1 technically should make no lexical lookup if env is
;;; not supplied.

(defun macroexpand (form &optional env)
  (multiple-value-bind (expanded-form expanded-p) (macroexpand-1 form env)
    (if expanded-p
        (values (macroexpand expanded-form env) T)
        (values expanded-form NIL))))

(defun macroexpand-1 (form &optional env)
  (if (not (listp form))
      (values form NIL)
      (let ((lexical-binding (lookup-binding-in-environment (car form) env :function)))
        (if lexical-binding
            (if (eq (preprocessor-funmac-binding-type lexical-binding) 'macro)
                (values (funcall *macroexpand-hook*
                                 (preprocessor-funmac-binding-value lexical-binding)
                                 form
                                 env)
                        T)
                (values form NIL))
            (if (AND (symbolp (car form)) (macro-function (car form)) (NOT (COMMON-LISP-SPECIAL-FORM-P (CAR FORM))))
                (values (funcall *macroexpand-hook*
                                 (macro-function (car form))
                                 form
                                 env)
                        T)
                (values form NIL))))))



;;; PROCLAMATIONS
;;;
;;; Sometimes variables are proclaimed special.  This information needs to
;;; be kept in a global state.  Previously, the information was kept on
;;; atoms' property lists.  The following code uses a different approach:
;;; keeping a table of variables that have been proclaimed special.
;;;
;;; If the use-prop-lists switch is true, then the atoms' property lists
;;; are also used.  Proclaim-special and proclaim-unspecial will update the
;;; property lists.

;;; *the following is no longer true*
;;; If is-special-p encounters a discrepancy between what a
;;; symbol's property list says and what the special-variable-table says,
;;; the property list gets the benefit of the doubt.  The special-variable-
;;; table is updated {and a warning is issued}.

(defun setup-special-variable-table ()
  (let ((special-variable-table (make-hash-table :test #'eq))
        (use-prop-lists t))
    (labels ((make-special (var)
             (when use-prop-lists
               (setf (get var 'special) t))
             (puthash var t special-variable-table)
             t)
           (make-unspecial (var)
             (when use-prop-lists
               (remf (plist var) 'special))
             (remhash var special-variable-table)
             t)
           (is-special-p (var)
             (multiple-value-bind (special-p found)
                 (gethash var special-variable-table)
               special-p
               (if found
                   t
                   (when use-prop-lists
                     (let ((on-plist (getl var '(special system:system-constant))))
                       (when on-plist
                         (make-special var)
                         t))))))
           (special-list ()
             special-variable-table)
           (use-property-lists (bool)
             (setf use-prop-lists bool)))
      (fdefine 'proclaim-special #'make-special)
      (fdefine 'proclaim-unspecial #'make-unspecial)
      (fdefine 'proclaimed-special-p #'is-special-p)
      (fdefine 'special-proclamations #'special-list)
      (fdefine 'proclamations-use-property-lists #'use-property-lists))))

(eval-when (global:load global:eval) (setup-special-variable-table))



;;; METERING
;;;

(defun reset-counts ()
  (setq *eval-count* 0 *apply-count* 0))

(defun meter ()
  (coerce (/ *eval-count* *apply-count*) 'float))



;;; SPECIAL FORMS
;;;
;;; The first predicate determines whether or not a symbol names a Common Lisp
;;; special form.  Taken from Steele, Table 5-1, p. 57.
;;;
;;; The second predicate determines whether or not a symbol names a nonstandard
;;; special form.

(defun common-lisp-special-form-p (sym)
  (if (member sym
              '(block catch compiler-let declare eval-when flet function go if
                labels let let* macrolet multiple-value-call multiple-value-prog1
                progn progv quote return-from setq tagbody the throw unwind-protect))
      t
      nil))

(defun implementation-special-form-p (sym)
  (if (member sym
              '(describe-frame describe-pp))
      t
      nil))



;;; EVALHOOK and APPLYHOOK
;;;
;;; If the variable *evalhook* is not nil, then it should be bound to a function of
;;; two arguments.  This function is called whenever main-eval is called, and is
;;; responsible for evaluating the expression passed to the evaluator.
;;;
;;; The first argument that *evalhook* is called with is a Lisp form.
;;;
;;; The second argument that *evalhook* is called with is an "environment".  This is
;;; actually a lexical closure, a function of one argument (a Lisp form), which evaluates
;;; the Lisp form in the lexical environment containing the *evalhook* call.
;;; (Implementing this requires a bit of kludgery; the form must be preprocessed in
;;; the appropriate preprocessor environment before it can be evaluated.  Hence there
;;; must be a handle on the preprocessor environment at the time *evalhook* is called.)

;; Relinquish-to-evalhook is called by main-eval.
;;
(defun relinquish-to-evalhook (exp env)
  (let ((hook-fn *evalhook*)
        (*evalhook* nil)
        (*applyhook* nil))
    (flet ((eval-fn (lisp-form)
             (let*
               ((pass-1-result
                  (preprocess lisp-form (intermediate-expression-preprocessor-env exp)))
                (pass-2-result
                  (multiple-value-list (main-eval pass-1-result env t))))
               (values-list pass-2-result))))
      (funcall hook-fn (intermediate-expression-original-form exp) #'eval-fn))))

(defun relinquish-to-applyhook (fun args ppenv env)
  (let ((hook-fn *applyhook*)
        (*evalhook* nil)
        (*applyhook* nil))
    (flet ((eval-fn (lisp-form)
             (let*
               ((pass-1-result
                  (preprocess lisp-form ppenv))
                (pass-2-result
                  (multiple-value-list (main-eval pass-1-result env t))))
               (values-list pass-2-result))))
      (funcall hook-fn fun args #'eval-fn))))

(defun evalhook (form evalhookfn applyhookfn &optional env)
  (let ((*evalhook* evalhookfn)
        (*applyhook* applyhookfn))
    (if env
        (funcall env form)
        (eval form))))

(defun applyhook (fn args evalhookfn applyhookfn &optional env)
  (declare (ignore env))
  (let ((*evalhook* evalhookfn)
        (*applyhook* applyhookfn))
    (apply fn args)))

(eval-when (global:compile)
  (format *trace-output* "~&Compilation done."))

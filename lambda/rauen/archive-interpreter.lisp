;;; -*- Mode:LISP; Package:INTERPRETER; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;;
;;; INTERPRETER.LISP
;;;
;;; Copyright 1986, Lisp Machine Incorporated
;;;


;;; Outside world:
;;;
;;;  GLOBAL:IF
;;;  SI::EXPAND-DEFMACRO
;;;  SI::GOBBLE-DECLARATIONS


;;; Startup code and interpreter global variables
;;;

(eval-when (compile)
  (common-lisp t)
  (in-package 'interpreter))

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

(defvar *ki-verbose-intermediate-forms* t
  "T for a verbose intermediate-form print function, NIL for a terse one.")

(defvar *ki-evalhook-first-position-functions* t
  "Use 'eval' to evaluate the function in a function application.  If NIL, the
function is evaluated without using 'eval'.  This determines whether or not
evaluating the function will be evalhooked.")

(defvar *ki-allow-free-variables* NIL
  "Bound to T while EVAL-SPECIAL-OK is running.  When NIL, free variable references
cause an error.")


;;; FOR THE OUTSIDE WORLD
;;;

(defun eval-exp (exp &optional nohook)
  (let* ((*ki-allow-free-variables* NIL)
         (pass-1-result (first-pass-eval exp))
         (pass-2-result (multiple-value-list (second-pass-eval pass-1-result nohook))))
    (values-list pass-2-result)))

(defun eval-exp-special-ok (exp)
  "Evaluates a Lisp form, treating free variable references as special variables."
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
      (print (eval-exp expr)))))

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
          (map 'list #'(lambda (result) (print result) (fresh-line)) results))))))



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
  (map 'list
       #'(lambda (exp) (preprocess exp env))
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
    `(tagbody ,@(map 'list
                     #'(lambda (arg) (preprocess-tagbody-arg arg inner-frame))
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
       (map 'list
            #'(lambda (binding)
                (require-bindable-symbol (if (listp binding) (car binding) binding)))
            bindings))
     (inner-frame (adjoin-variable-frame env))
     (preprocessed-bindings
       (preprocess-let-bindings 'LET bindings special-variables inner-frame))
     (preprocessed-body
       (preprocess-sequence body inner-frame)))
   `(compiler-let ,preprocessed-bindings ,@preprocessed-body)))

(defun preprocess-general-let (special-form-name exp env)
  (require-n-arguments exp 1)
  (let*
    ((let-bindings (let-bindings exp))
     (let-body (let-body exp))
     (decl-info (examine-declarations let-body env))
     (special-variables (decl-info-specials decl-info))
     (inner-frame (adjoin-variable-frame env))
     (foo (declare-special-variables special-variables inner-frame))
     (preprocessed-bindings
       (preprocess-let-bindings special-form-name let-bindings special-variables inner-frame))
     (preprocessed-body
       (preprocess-sequence (decl-info-body decl-info) inner-frame)))
    (declare (ignore foo))
   `(,special-form-name ,preprocessed-bindings ,@preprocessed-body)))

;;; Let bindings are preprocessed in the following manner:
;;;

(defun preprocess-let-bindings (special-form-name binding-list special-variables frame)
  (when binding-list
    (let* ((binding (parse-let-binding (first binding-list)))
           (var (let-binding-var binding))
           (value (let-binding-value binding))
           (preprocessed-value
             (case special-form-name
               (LET (preprocess value (nframe-parent frame)))
               (LET* (preprocess value frame))
               (otherwise (error "internal: preprocess-general-let-bindings")))))
      (cons `(,(preprocessor-bind-variable var special-variables frame) ,preprocessed-value)
            (preprocess-let-bindings
              special-form-name (cdr binding-list) special-variables frame)))))

(defun parse-let-binding (let-binding)
  (parse-expr let-binding
              '(:var
                (:var :value))
              #'make-let-binding
              "Let binding syntax"
              '(:value)))

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
      (t
       (error "Bad argument to FUNCTION")))))

(defun preprocess-function-symbol (sym env)
  (let ((lexical-binding (lookup-binding-in-environment sym env :function)))
    (cond
      ((null lexical-binding)
       (if (macro-function sym)
           (ferror "~S names a macro." sym)
           `(regular-function ,sym)))
      ((eq (preprocessor-funmac-binding-type lexical-binding) 'macro)
       (ferror "~S names a lexical macro." sym))
      ((eq (preprocessor-funmac-binding-type lexical-binding) 'function)
       `(lexical-function ,sym))
      (t
       (error "internal: preprocess-function-symbol")))))

(defun preprocess-function-lambda (lambda-expr env)
  (preprocess-lexical-closure lambda-expr env))


;;;; Preprocessing lexical closures
;;;
(defun preprocess-lexical-closure (lambda-expr env)
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
    `(lexical-closure ,preprocessed-lambda-list
                      ,@(preprocess-sequence (decl-info-body decl-info)
                                             inner-frame))))

(defun lambda-expression-p (expr)
  (and (listp expr) (eq (car expr) 'lambda)))


;;; Preprocessing lambda lists
;;;
;;; Preprocesses a lambda-list.  Special-variables is a list of variables
;;; that are declared special within the lambda expression.  This function
;;; returns a nice-lambda-list structure and binds the lambda-list variables
;;; in the frame.  If there is a syntax error in the lambda-list, it is raised here.
;;;
(defun preprocess-lambda-list (lambda-list special-variables frame)
  (flet ((peek () (car lambda-list))
         (pop () (pop lambda-list))
         (emptyp () (null lambda-list)))
    (let*
      ((required-arguments (lambda-list-arguments '&required #'peek #'pop #'emptyp))
       (optional-arguments (lambda-list-arguments '&optional #'peek #'pop #'emptyp))
       (rest-arguments (lambda-list-arguments '&rest #'peek #'pop #'emptyp))
       (key-arguments (lambda-list-arguments '&key #'peek #'pop #'emptyp))
       (allow-arguments (lambda-list-arguments '&allow-other-keys #'peek #'pop #'emptyp))
       (aux-arguments (lambda-list-arguments '&aux #'peek #'pop #'emptyp))
       (preprocessed-required
         (preprocess-lambda-list-required (cdr required-arguments) special-variables frame))
       (preprocessed-optional
         (preprocess-lambda-list-optional (cdr optional-arguments) special-variables frame))
       (preprocessed-rest
         (when rest-arguments
           (preprocess-lambda-list-rest (cdr rest-arguments) special-variables frame)))
       (preprocessed-key
         (preprocess-lambda-list-key (cdr key-arguments) special-variables frame))
       (preprocessed-allow
         (when allow-arguments
           (preprocess-lambda-list-allow-other-keys key-arguments (cdr allow-arguments))))
       (preprocessed-aux
         (preprocess-lambda-list-aux (cdr aux-arguments) special-variables frame)))
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
    (let* ((next-parameter (parse-optional-parameter (car parameters)))
           (var (optional-parameter-var next-parameter))
           (initform (optional-parameter-initform next-parameter))
           (svar (optional-parameter-svar next-parameter)))
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

(defun preprocess-lambda-list-rest (parameters special-variables frame)
  (when ( (length parameters) 1)
    (error "Rest parameter syntax in lambda list"))
  (preprocessor-bind-variable (car parameters) special-variables frame))

(defun preprocess-lambda-list-key (parameters special-variables frame)
  (when parameters
    (let* ((next-parameter (parse-key-parameter (car parameters)))
           (var (key-parameter-var next-parameter))
           (keyword (key-parameter-keyword next-parameter))
           (initform (key-parameter-initform next-parameter))
           (svar (key-parameter-svar next-parameter)))
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

(defun preprocess-lambda-list-allow-other-keys (key-arguments parameters)
  (unless key-arguments
    (error "&allow-other-keys without &key"))
  (unless (null parameters)
    (error "allow-other-keys syntax in lambda-list"))
  t)

(defun preprocess-lambda-list-aux (parameters special-variables frame)
  (when parameters
    (let* ((next-parameter (parse-aux-parameter (car parameters)))
           (var (aux-parameter-var next-parameter))
           (initform (aux-parameter-value next-parameter)))
      (let*
        ((preprocessed-initform
           (if initform (preprocess initform frame) nil))
         (preprocessed-var
           (preprocessor-bind-variable var special-variables frame)))
        (cons (make-aux-parameter :var preprocessed-var
                                  :value preprocessed-initform)
              (preprocess-lambda-list-aux (cdr parameters) special-variables frame))))))

(defun lambda-list-keyword-p (sym)
  (member sym '(&optional &rest &key &allow-other-keys &aux)))

(defun ok-lambda-keyword-order (first-keyword second-keyword)
  (and (not (eq first-keyword second-keyword))
       (member second-keyword
               (member first-keyword '(&optional &rest &key &allow-other-keys &aux)))))


;;; Parsing lambda-lists
;;;
;;; The first group of procedures incrementally chop away at the arguments in a
;;; lambda-list.  The lambda-list-arguments function takes four arguments:
;;; a lambda-list keyword, a function to peek at the next parameter on the
;;; lambda-list, a function to pop the next parameter off the lambda-list,
;;; and a predicate to indicate if the lambda-list is empty.  If the
;;; keyword is at the front of the lambda-list, the function returns a list
;;; containing the keyword and all subsequent parameters up to the next
;;; keyword or the end of the list.
;;;
;;; The second group of procedures parse specific types of lambda-list arguments.
;;; The functions parse-optional-parameter, parse-key-parameter, and
;;; parse-aux-parameter return a structure containing the parsed information.
;;; These structures are the same ones used to represent the corresponding
;;; intermediate forms.

(defun lambda-list-arguments (keyword peek-proc pop-proc empty-pred)
  (cond
    ((eq keyword '&required)
     (cons '&required
           (lambda-list-arguments-before-next-keyword keyword peek-proc pop-proc empty-pred)))
    ((funcall empty-pred)
     nil)
    ((eq keyword (funcall peek-proc))
     (funcall pop-proc)
     (cons keyword
           (lambda-list-arguments-before-next-keyword keyword peek-proc pop-proc empty-pred)))
    (t
     nil)))

(defun lambda-list-arguments-before-next-keyword (keyword peek-proc pop-proc empty-pred)
  (cond
    ((funcall empty-pred)
     nil)
    ((lambda-list-keyword-p (funcall peek-proc))
     (unless (or (ok-lambda-keyword-order keyword (funcall peek-proc))
                 (eq keyword '&required))
       (error "Incorrect order of keywords in lambda expressions"))
     nil)
    (t
     (cons (funcall pop-proc)
           (lambda-list-arguments-before-next-keyword keyword peek-proc pop-proc empty-pred)))))

(defun parse-optional-parameter (optional-parameter)
  (parse-expr optional-parameter
              '(:var
                (:var)
                (:var :initform)
                (:var :initform :svar))
              #'make-optional-parameter
              "Optional parameter syntax in lambda list"
              '(:initform)))

(defun parse-key-parameter (key-parameter)
  (let
    ((parsed-expr
       (parse-expr key-parameter
                   '(:var
                     (:var)
                     (:var :initform)
                     (:var :initform :svar)
                     ((:keyword :var))
                     ((:keyword :var) :initform)
                     ((:keyword :var) :initform :svar))
                   #'make-key-parameter
                   "Key parameter syntax in lambda list"
                   '(:initform))))
    (unless (key-parameter-keyword parsed-expr)
      (setf (key-parameter-keyword parsed-expr)
            (make-keyword (key-parameter-var parsed-expr))))
    parsed-expr))

(defun parse-aux-parameter (aux-parameter)
  (parse-expr aux-parameter
              '(:var
                (:var)
                (:var :value))
              #'make-aux-parameter
              "Aux parameter syntax in lambda list"
              '(:value)))

(defun make-keyword (sym)
  (intern (symbol-name sym) (find-package 'keyword)))


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
  (map 'list
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
             (lambda-expr `(lambda ,@(cdr binding))))
        (bind-in-frame name '(FUNCTION . NIL) frame :function)
        (cons `(,name ,(preprocess `(function ,lambda-expr) (nframe-parent frame)))
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
             (lambda-expr `(lambda ,@(cdr binding))))
        (cons `(,name ,(preprocess `(function ,lambda-expr) frame))
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
  (let ((situation-list (eval-when-situation-list exp))
        (forms (eval-when-forms exp)))
    (when (member-if-not #'(lambda (situation) (member situation '(compile load eval)))
                         situation-list)
      (error "Bad symbol in situation list"))
    `(eval-when ,situation-list ,@(preprocess-sequence forms env))))


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
(defun second-pass-eval (exp &optional nohook)
  (main-eval exp (make-empty-environment) nohook))

(defun main-eval (exp env &optional nohook)
  (if (and *evalhook* (not nohook))
      (relinquish-to-evalhook exp env)
      (let ((expr (intermediate-expression-preprocessed-form exp))
            (eval-procedure (intermediate-expression-eval-procedure exp)))
;       (let ((eval-procedure (lookup-eval-procedure (car expr))))
          (unless eval-procedure
            (ferror "internal: unrecognized tag ~S for dispatch in main-eval" (car expr)))
          (case (car expr)
            (funcall
             (eval-funcall expr (intermediate-expression-preprocessor-env exp) env))
            (lexical-closure
             (make-lexical-closure exp env))
            (otherwise
             (funcall eval-procedure expr env))))))

(defun eval-subproblem (exp env)
  (unless *evalhook*
    (add-subproblem-to-history exp))
  (main-eval exp env))

(defun eval-reduction (exp env)
  (unless *evalhook*
    (add-reduction-to-history exp))
  (main-eval exp env))


(defun eval-sequence (exps env)
  (cond ((null exps)
         nil)
        ((= (length exps) 1)
         (main-eval (car exps) env))
        (t
         (main-eval (car exps) env)
         (eval-sequence (cdr exps) env))))

;; This is really where functions are "applied".  (Function application preprocesses
;; to a FUNCALL intermediate form.)  If *ki-evalhook-first-position-functions* is nil,
;; don't hook the evaluation of the function.
;;
(defun eval-funcall (exp ppenv env)
  (let* ((function (main-eval (cadr exp) env (not *ki-evalhook-first-position-functions*)))
         (arguments (map 'list
                         #'(lambda (expr) (main-eval expr env))
                         (cddr exp))))
    (if *applyhook*
        (relinquish-to-applyhook function arguments ppenv env)
        (apply function arguments))))


(defun lookup-eval-procedure (tag)
  (case tag
    (self-evaluating      #'eval-self-evaluating)
    (lexical-variable     #'eval-lexical-variable)
    (special-variable     #'eval-special-variable)
    (free-variable        #'eval-free-variable)
    (regular-function     #'eval-regular-function)
    (lexical-function     #'eval-lexical-function)
    (lexical-closure      'special-case)
    (funcall              'special-case)
    (progn-with-frame     #'eval-progn-with-frame)
    (describe-frame       #'eval-describe-frame)
    (block                #'eval-block)
    (catch                #'eval-catch)
    (compiler-let         #'eval-compiler-let)
    (eval-when            #'eval-eval-when)
    (flet                 #'eval-flet)
    (go                   #'eval-go)
    (if                   #'eval-if)
    (labels               #'eval-labels)
    (let                  #'eval-let)
    (let*                 #'eval-let*)
    (multiple-value-call  #'eval-multiple-value-call)
    (multiple-value-prog1 #'eval-multiple-value-prog1)
    (progn                #'eval-progn)
    (progv                #'eval-progv)
    (quote                #'eval-quote)
    (return-from          #'eval-return-from)
    (setq                 #'eval-setq)
    (tagbody              #'eval-tagbody)
    (throw                #'eval-throw)
    (the                  #'eval-the)
    (unwind-protect       #'eval-unwind-protect)        ;
    (otherwise            nil)))

(defun eval-self-evaluating (exp env)
  (declare (ignore env))
  (cadr exp))

(defun eval-lexical-variable (exp env)
  (funcall (caddr exp) env))
;  (cdr (lookup-binding-in-environment (cadr exp) env :variable)))

(defun eval-special-variable (exp env)
  (declare (ignore env))
  (symbol-value (cadr exp)))

(defun eval-free-variable (exp env)
  (declare (ignore env))
  (if *ki-allow-free-variables*
      (symbol-value (cadr exp))
      (signal-proceed-case ((val) 'eval-free-variable-error
                                  :symbol (cadr exp))
        (:new-value
         val)
        (:use-dynamic-value
         (symbol-value (cadr exp)))
        (:make-special
         (proclaim-special (cadr exp))
         (symbol-value (cadr exp))))))

(defun eval-regular-function (exp env)
  (declare (ignore env))
  (symbol-function (cadr exp)))

(defun eval-lexical-function (exp env)
  (cdr (lookup-binding-in-environment (cadr exp) env :function)))


;; DESCRIBE-FRAME
;;
(defun eval-describe-frame (exp env)
  (if (null env)
      (progn
        (terpri)
        (princ "NIL is the null lexical environment.")
        (values))
      (progn
        (describe-defstruct env)
        (eval-describe-frame exp (nframe-parent env)))))

;; IF
;;
(defun eval-if (exp env)
  (let ((predicate (if-predicate exp))
        (consequent (if-consequent exp))
        (alternate (if-alternate exp)))
    (if (main-eval predicate env)
        (main-eval consequent env)
        (main-eval alternate env))))


;; QUOTE
;;
(defun eval-quote (exp env)
  (declare (ignore env))
  (quote-object exp))


;; BLOCK
;;
;; Bind the block's name to a lexical closure, a function of one argument.  During
;; the dynamic extent of the block, the function exits the block and returns its
;; argument.  After the block's time is up, the function raises an error.
;;
(defun eval-block (exp env)
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
(defun eval-return-from (exp env)
  (let ((block-name (return-from-name exp))
        (value (return-from-result exp)))
    (let ((tag-binding (lookup-binding-in-environment block-name env :block)))
      (unless tag-binding
        (ferror "internal: ~S binding not found in eval-return-from" block-name))
      (funcall (cdr tag-binding) (main-eval value env)))))


;; TAGBODY
;;
;; All throws are to the top of the tagbody.
;;;
(defun eval-tagbody (exp env)
  (let* ((tagbody-body (tagbody-body exp))
         (gotags (extract-gotags tagbody-body))
         (tagbody-name (prog1 (gensym 'TAGBODY-) (gensym 'g)))
         (inner-frame (adjoin-gotag-frame env)))
    (bind-gotags gotags tagbody-name inner-frame)
    (unwind-protect
        (eval-tagbody-dispatch 'start tagbody-name tagbody-body inner-frame)
      (rebind-gotags! gotags inner-frame))))

(defun eval-tagbody-dispatch (instruction tagbody-name body env)
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
        nil
        (eval-tagbody-dispatch next-instruction tagbody-name body env))))

(defun eval-tagbody-body (body env)
  (cond
    ((null body)
     'halt)
    ((intermediate-expression-p (car body))
     (main-eval (car body) env)
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
(defun eval-go (exp env)
  (let ((gotag-binding (lookup-binding-in-environment (go-gotag exp) env :gotag)))
    (unless gotag-binding
      (ferror "~S is not a lexically visible GO tag." (go-gotag exp)))
    (funcall (cdr gotag-binding))))


;; CATCH
;;
(defun eval-catch (exp env)
  (let ((tag (catch-tag exp))
        (forms (catch-forms exp)))
    (let ((evaluated-tag (main-eval tag env)))
      (unless (symbolp evaluated-tag)
        (error "CATCH tag does not evaluate to a symbol."))
      (catch evaluated-tag
        (eval-sequence forms env)))))


;; THROW
;;
(defun eval-throw (exp env)
  (let ((tag (throw-tag exp))
        (result (throw-result exp)))
    (let ((evaluated-tag (main-eval tag env)))
      (loop
        (if (symbolp evaluated-tag)
            (return)
            (setq evaluated-tag
                  (cerror t nil nil "THROW tag does not evaluate to a symbol."))))
      (throw evaluated-tag (main-eval result env)))))


;; UNWIND-PROTECT
;;
(defun eval-unwind-protect (exp env)
  (let ((protected-form (unwind-protect-protected-form exp))
        (cleanup-forms (unwind-protect-cleanup-forms exp)))
    (unwind-protect
        (main-eval protected-form env)
      (eval-sequence cleanup-forms env))))

;; PROGN
;;
(defun eval-progn (exp env)
  (let ((forms (progn-forms exp)))
    (eval-sequence forms env)))


;; PROGN-WITH-FRAME
;;
(defun eval-progn-with-frame (exp env)
  (let ((forms (progn-forms exp))
        (inner-frame (adjoin-junk-frame env)))
    (eval-sequence forms inner-frame)))


;; PROGV
;;
;; No frame is created because progv doesn't affect lexical environments.
(defun eval-progv (exp env)
  (let* ((symbols (progv-symbols exp))
         (values (progv-values exp))
         (forms (progv-forms exp))
         (evaluated-symbols (map 'list #'require-bindable-symbol (main-eval symbols env)))
         (evaluated-values (main-eval values env)))
    (progv `(list ,@evaluated-symbols)
           `(list ,@evaluated-values)
           (eval-sequence forms env))))



;; COMPILER-LET, LET, and LET*
;;
(defun eval-compiler-let (exp env)
  (evaluate-general-let 'LET exp env))

(defun eval-let (exp env)
  (evaluate-general-let 'LET exp env))

(defun eval-let* (exp env)
  (evaluate-general-let 'LET* exp env))

(defun evaluate-general-let (special-form exp env)
  (let ((let-bindings (let-bindings exp))
        (let-body (let-body exp))
        (inner-frame (adjoin-variable-frame env)))
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
               (LET  (if value (main-eval value (nframe-parent env))))
               (LET* (if value (main-eval value env) nil))))
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
(defun eval-flet (exp env)
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
           (evaluated-closure (main-eval closure (nframe-parent frame))))
      (bind-in-frame name evaluated-closure frame :function)
      (evaluate-flet-bindings (cdr binding-list) frame))))


;;; LABELS
;;;
(defun eval-labels (exp env)
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
           (evaluated-closure (main-eval closure inner-env)))
      (rplacd (lookup-binding-in-environment name inner-env :function) evaluated-closure)
      (evaluate-labels-bindings (cdr binding-list) inner-env))))


;;; SETQ
;;;
(defun eval-setq (exp env)
  (let ((args (setq-args exp)))
    (if args (evaluate-setq-loop (cdr exp) env) nil)))

(defun evaluate-setq-loop (args env)
  (let* ((variable (intermediate-expression-preprocessed-form (first args)))
         (variable-type (car variable))
         (variable-id (require-bindable-symbol (cadr variable) "set"))
         (form (second args))
         (value (main-eval form env))
         (rest (cddr args)))
    (case variable-type
      (lexical-variable (rplacd (lookup-binding-in-environment variable-id env :variable) value))
      (special-variable (set variable-id value))
      (free-variable (set variable-id value))
      (otherwise (error "internal: evaluate-setq-loop")))
    (if rest
        (evaluate-setq-loop rest env)
        value)))


;;; MULTIPLE-VALUE-CALL
;;;
(defun eval-multiple-value-call (exp env)
  (let* ((function (multiple-value-call-function exp))
         (forms (multiple-value-call-forms exp))
         (evaluated-function (main-eval function env)))
    (unless (functionp evaluated-function)
      (error "First argument to MULTIPLE-VALUE-CALL not a function"))
    (let ((arguments (eval-multiple-value-call-args forms env)))
      (apply evaluated-function arguments))))

(defun eval-multiple-value-call-args (forms env)
  (when forms
    (append (multiple-value-list (main-eval (car forms) env))
            (eval-multiple-value-call-args (cdr forms) env))))


;;; MULTIPLE-VALUE-PROG1
;;;
(defun eval-multiple-value-prog1 (exp env)
  (let* ((first-form (multiple-value-prog1-first-form exp))
         (other-forms (multiple-value-prog1-other-forms exp))
         (evaluated-first-form (multiple-value-list (main-eval first-form env))))
    (eval-sequence other-forms env)
    (values-list evaluated-first-form)))


;;; EVAL-WHEN
;;;
(defun eval-eval-when (exp env)
  (let ((situation-list (eval-when-situation-list exp))
        (forms (eval-when-forms exp)))
    (when (member 'eval situation-list)
      (eval-sequence forms env))))


;;; THE
;;;
(defun eval-the (exp env)
  (let ((value-type (the-value-type exp))
        (form (the-form exp)))
    (declare (ignore value-type))
    (main-eval form env)))



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

(defun apply-lexical-closure (closure arguments env)
  (let ((lambda-list (cadr closure))
        (lambda-body (cddr closure))
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
       (signal-proceed-case
         ((value) 'too-few-arguments-error
                  :function closure
                  :minimum  minimum
                  :maximum  maximum
                  :arglist  args)
         (:new-value
          (funcall punt-proc value))))
      ((and maximum (> (length args) maximum))
       (signal-proceed-case
         ((value) 'too-many-arguments-error
                  :function closure
                  :minimum  minimum
                  :maximum  maximum
                  :arglist  args)
         (:new-value
          (funcall punt-proc value))
         (:truncate-argument-list
          value)))
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
                    (if initform (main-eval initform frame) nil)))
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
                        (if initform (main-eval initform frame) nil)))
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
             (initial-value (if initform (main-eval initform frame) nil)))
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
  (format stream
          "#<FRAME ~S ~S, Parent ~S>"
          (nframe-type nframe)
          (map 'list #'car (nframe-bindings nframe))
          (if (nframe-parent nframe) (nframe-type (nframe-parent nframe)) NIL)))



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
  (if *ki-verbose-intermediate-forms*
      (let ((indent *ki-intermediate-form-indentation*)
            (*ki-intermediate-form-indentation* (+ *ki-intermediate-form-indentation* 2)))
        (format stream "~&~V@T#<Intermediate form:" indent)
        (format stream "~&~V@T  Before: ~S" indent
                (intermediate-expression-original-form expr))
        (format stream "~&~V@T  After:  ~S" indent
                (intermediate-expression-preprocessed-form expr))
        (format stream "~&~V@T >" indent))
      (print (intermediate-expression-preprocessed-form expr) stream)))



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



;;; EXPRESSION PARSER
;;;
;;; Pattern:    ((:foo :bar) :baz)
;;; Expression: ((a b) c)
;;; Result:     (apply constructor '(:foo a :bar b :baz c))

(defun parse-expr (expr pattern-list constructor error-message match-anything-list)
  (when (null pattern-list)
    (error error-message))
  (let ((trial-match (pattern-match (car pattern-list) expr match-anything-list)))
    (if trial-match
        (apply constructor trial-match)
        (parse-expr expr (cdr pattern-list) constructor error-message match-anything-list))))

(defun pattern-match (pattern expr match-anything-list)
  (cond
    ((and (symbolp pattern) (member pattern match-anything-list))
     (list pattern expr))
    ((and (null pattern) (null expr))
     'nil-match)
    ((or (null pattern) (null expr))
     nil)
    ((and (symbolp pattern) (symbolp expr))
     (list pattern expr))
    ((and (consp pattern) (consp expr))
     (let ((car-match (pattern-match (car pattern) (car expr) match-anything-list))
           (cdr-match (pattern-match (cdr pattern) (cdr expr) match-anything-list)))
       (if (and car-match cdr-match)
           (if (eq cdr-match 'nil-match)
               car-match
               (append car-match cdr-match))
           nil)))
    (t
     nil)))

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
        (closure-name nil)
        (original-definition (cadr (intermediate-expression-original-form exp))))
    (declare (special interpreter-closure closure-name original-definition))
    (closure '(interpreter-closure closure-name original-definition)
      #'(lambda (&rest args)
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

(defun testerr ()
  (signal 'cerror
          :proceed-types '(:continue)
          :continue-format-string "Continue format string"
          :format-string "Format string"
          :format-args (list* 'foo-signal-name "Another format string" nil)))

(defsignal throw-error error (throw-tag)
  "Caused when an unrecognized tag is thrown to.")

(defun throw-error ()
  (signal-condition
    (make-condition 'throw-error "Can't throw to tag ~S" 'foof)))

;;; INSURANCE
;;;
(defun require-bindable-symbol (var &optional (verb "bind"))
  (cond ((not (symbolp var))
         (ferror "Attempt to ~A ~S; a symbol is required" verb var))
        ((lambda-list-keyword-p var)
         (ferror "Attempt to ~A the lambda-list-keyword ~S" verb var))
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

(defstruct decl-info
  specials body)

(defun examine-declarations (exprs env)
  (multiple-value-bind (body declarations) (SI::GOBBLE-DECLARATIONS exprs NIL env)
    (make-decl-info :specials (extract-special-variable-list declarations)
                    :body     body)))

(defun extract-special-variable-list (declarations)
  (when declarations
    (append (extract-special-variables (car declarations))
            (extract-special-variable-list (cdr declarations)))))

(defun extract-special-variables (declaration)
  (let ((special-variables nil))
    (map 'list
         #'(lambda (decl-spec)
             (if (special-decl-spec-p decl-spec)
                 (setq special-variables
                       (append special-variables
                               (special-vars-in-decl-spec decl-spec)))))
         declaration)
    special-variables))

(defun special-vars-in-decl-spec (decl-spec)
  (map 'list
       #'(lambda (element)
           (if (symbolp element)
               element
               (error "element of special decl-spec not a symbol")))
       (cdr decl-spec)))

(defun special-decl-spec-p (decl-spec)
  (and (listp decl-spec)
       (eq (car decl-spec) 'special)))



;;; CALL HISTORY
;;;



;;; MACRO EXPANDERS
;;;
;;; These both assume that NIL = null environment.  This could be fixed, though.
;;; Also, kmacroexpand-1 technically should make no lexical lookup if env is
;;; not supplied.

(defun kmacroexpand (form &optional env)
  (multiple-value-bind (expanded-form expanded-p) (kmacroexpand-1 form env)
    (if expanded-p
        (values (kmacroexpand expanded-form env) T)
        (values expanded-form NIL))))

(defun kmacroexpand-1 (form &optional env)
  (if (not (listp form))
      (values form NIL)
      (let ((lexical-binding (lookup-binding-in-environment (car form) env :function)))
        (if lexical-binding
            (if (eq (preprocessor-funmac-binding-type lexical-binding) 'macro)
                (values (funcall *macroexpand-hook*
                                 (preprocessor-funmac-binding-value lexical-binding)
                                 form)
                        T)
                (values form NIL))
            (if (AND (macro-function (car form)) (NOT (COMMON-LISP-SPECIAL-FORM-P (CAR FORM))))
                (values (funcall *macroexpand-hook*
                                 (macro-function (car form))
                                 form)
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
;;; property lists.  If is-special-p encounters a discrepancy between what a
;;; symbol's property list says and what the special-variable-table says,
;;; the property list gets the benefit of the doubt.  The special-variable-
;;; table is updated {and a warning is issued}.


(defun setup-special-variable-table ()
  (let ((special-variable-table nil)
        (use-prop-lists t))
    (flet ((make-special (var)
             (if use-prop-lists
                 (setf (get var 'special) t))
             (unless (member var special-variable-table)
               (push var special-variable-table))
             t)
           (make-unspecial (var)
             (if use-prop-lists
                 (setf (get var 'special) nil))
             (if (member var special-variable-table)
                 (setf special-variable-table (remove var special-variable-table)))
             t)
           (is-special-p (var)
             (if use-prop-lists
                 (let ((pl-is-special (get var 'special))
                       (table-special (member var special-variable-table)))
                   (cond
                     ((and pl-is-special (not table-special))
                      (push var special-variable-table))
                     ((and (not pl-is-special) table-special)
                      (remove var special-variable-table)))))
             (if (member var special-variable-table) t nil))
           (special-list ()
             special-variable-table)
           (use-property-lists (bool)
             (setf use-prop-lists bool)))
      (fdefine 'proclaim-special #'make-special)
      (fdefine 'proclaim-unspecial #'make-unspecial)
      (fdefine 'proclaimed-special-p #'is-special-p)
      (fdefine 'special-proclamations #'special-list)
      (fdefine 'proclamations-use-property-lists #'use-property-lists))))

(eval-when (load eval) (setup-special-variable-table))


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

(defun kevalhook (form evalhookfn applyhookfn &optional env)
  (let ((*evalhook* evalhookfn)
        (*applyhook* applyhookfn))
    (if env
        (funcall env form)
        (eval-exp form))))

(defun kapplyhook (fn args evalhookfn applyhookfn &optional env)
  (declare (ignore env))
  (let ((*evalhook* evalhookfn)
        (*applyhook* applyhookfn))
    (apply fn args)))

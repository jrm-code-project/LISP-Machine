;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;


;;; Macro expansion and alphatization
;;;============================================================================

;;;  This performs the dispatch for macro expansion and performs alphatization;
;;; e.g. symbols that represent lexically bound variables are replaced by
;;; structures that represent variables unambiguously.  The lexical scoping is
;;; handled by the ENV.
;;;  The expression returned by ALPHA contains syntax objects that will be
;;; looked up in the compilator table, primops
;;; and structures representing lexically bound variables.

(defun alpha (exp env)
  (cond ((symbolp exp)
         (cond ((global-register exp)
                (global-register exp))
               ((constantp exp)
                (integrate-constant-value exp env))
               (t (alpha-variable exp env))))
        ((atom exp)
         (list 'QUOTE exp))
        ((symbolp (car exp))
         (alpha-combination (car exp) exp env))
        ((and (listp (car exp))
              (member (caar exp) '(lambda zl:named-lambda)))     ;t
         (alpha-list exp env))
        (t (syntax-error "~s is not a valid function" (car exp)))))


(defun integrate-constant-value (symbol env)
  (let ((value (symbol-value symbol)))
    (if (or (numberp value)
            (symbolp value)
            (and (consp value)
                 (or (eq (car value) 'hw:unboxed-constant)
                     (eq (car value) 'hw:boxed-constant))))
        (list 'QUOTE value)
      (create-special-ref symbol env))))

;;; Table of all handlers for special forms
(defvar *special-form-table*
  (make-table '*special-form-table*))

(NOTE "~%Primop arg check in ALPHA-COMBINATION")

(defun alpha-combination (sym exp env &aux probe)
  (cond ((setq probe (table-entry *special-form-table* sym))
         ;; alpha-special-form
         (funcall probe exp env))
        ((setq probe (obtain-fvariable env sym))
         (cond
           ;; Alpha substitute FLET or LABELS vars
           ((variable-p probe)
            (cons probe (alpha-list (cdr exp) env)))
           ;; Expand local macros
           ((and (consp probe)
                 (eq (car probe) 'MACRO))
            (alpha (funcall (cdr probe) exp env) env))
           (t (bug "unknown alpha binding"))))
        ((setq probe (table-entry *primop-table* sym))
         ;; do this for real sometime
         (unless (or (member '&rest (primop.arglist probe))
                     (= (length (cdr exp))
                        (length (primop.arglist probe))))
           (warn "Wrong number of args to ~a" (primop.name probe)))
         (cons probe (alpha-list (cdr exp) env)))
        (t
         (let ((new-exp (expand-rewriter exp env)))
           (if (not (eq new-exp exp))
               (alpha new-exp env)
             (if (setq probe (get-subst sym))
                 ;; substitute substs
                 (cons
                   ;; alphatize body of subst in null environment
                   ;; to maintain its scoping
                   (let ((subst-name (if (consp (second probe))
                                         (car (second probe))
                                       (second probe))))
                     (alpha-lambda subst-name (third probe) (cdddr probe) (make-compiler-env)))
                   (alpha-list (cdr exp) env))
               ;; is it a macro?
               (let ((new-exp (nlisp:macroexpand exp env)))
                 (if (eq new-exp exp)
                     ;; no macro, just make a function call
                     (alpha-function-call sym exp env)
                   ;; was a macro, try all over again
                   (alpha new-exp env)))))))))


(defun alpha-function-call (sym exp env)
  (let ((v (create-variable sym)))
;    (bind-fvariables env (list v))
;    (cons v (alpha-list (cdr exp) env))))
    ;; this way sym gets converted to a variable
    ;; by locale-variable-reference and is on *free-variables*
    ;; but this doesn't quite do the right thing for declarations
    (cons sym (alpha-list (cdr exp) env))))

;;; Maps ALPHA down a list of expressions.

(defun alpha-list (exp env)
  (mapcar #'(lambda (exp)
              (alpha exp env))
          exp))

;;;   Does syntax error checking, primitive syntax dispatch, and macro
;;; expansion.  The error checking is done by CHECK-SPECIAL-FORM, a T system
;;; procedure.
;(defun alpha-special-form (handler exp env)
;  (let ((proc (table-entry primitive-handler-table descr))
;        (new-exp (check-special-form-syntax descr exp)))
;    (cond ((not (eq exp new-exp))
;           ;; An error was reported, and luser gave us a new form.
;           (alpha new-exp env))
;          (proc
;           (funcall proc exp env))
;;          ((macro-expander? descr)
;;           (alpha (expand-macro-form descr exp syntax) env))
;          (t
;           (bug "special form unknown to this compiler~%  ~S" exp)))))

;;; Special Forms
;;;===========================================================================
;;;   The compiler knows about special forms.
;;;
;;; Names of All Common Lisp Special Forms
;;;
;;;   block        if                   progv
;;;   catch        labels               quote
;;;   compiler-let let                  return-from
;;;   declare      let*                 setq
;;;   eval-when    macrolet             tagbody
;;;   flet         multiple-value-call  the
;;;   function     multiple-value-prog1 throw
;;;   go           progn                unwind-protect
;;;
;;; "The set of special forms is fixed in Common Lisp; no way is provided
;;;  for the user to define more."

;;;   Syntax for defining the special forms that the compiler recognizes.  The
;;; form is:
;;; (DEFINE-SPECIAL-FORM <name> <argument form> (<env-variable>)
;;;   . <expansion code>)
;;;    Puts a handler into *special-form-table* using <name> as a key

(defmacro define-special-form (name pattern vars &body body)
  (let ((sym (concatenate-symbol 'syntax/ name))
        (exp (gensym 'exp)))
    `(progn
       (defun ,sym (,exp ,@vars)
         (destructure ((,pattern (cdr ,exp)))
                      ,@body))
       (setf (table-entry *special-form-table* ',name)
             #',sym))))

(defun nlisp:special-form-p (symbol)
  (if (table-entry *special-form-table* symbol) t))

;;; (QUOTE . blah)

(define-special-form QUOTE (value) (env)
  (declare (ignore env))
  (list 'QUOTE (copy-tree value)))

(define-special-form FUNCTION (fspec) (env)
  (cond ((symbolp fspec)
         (cond ((obtain-fvariable env fspec))
               (t ;; free function reference
                `(SYMBOL:%SYMBOL-FUNCTION (QUOTE ,fspec)))))
        ((and (consp fspec)
              (member (car fspec) '(lambda zl:named-lambda)))
         (alpha fspec env))
        (t (syntax-error "~s is not a valid function spec" fspec))))


(define-special-form LAMBDA (vars . body) (env)
  (alpha-lambda "P" vars body env))

(define-special-form NAMED-LAMBDA (name vars . body) (env)
  (alpha-lambda name vars body env))


;;; Take a lambda-list possibly containg &-keywords
;;; and return lists of parameter specifiers
(defun parse-lambda-list (lambda-list)
  (declare (values args optionals rest keys allow-other-keys-p auxes))
  (let ((ll lambda-list)
        (allow-other-keys-p nil))
    (labels ((collect-parameter-specs ()
                (do ((args '() (cons (pop ll) args)))
                    ((or (null ll)
                         (member (car ll) lambda-list-keywords))
                     (nreverse args)))))
      (multiple-value-prog1
        (values (collect-parameter-specs)
                (when (eq (car ll) '&optional)
                  (pop ll)
                  ;; init form defaults to nil
                  (mapcar #'(lambda (op)
                              (if (consp op)
                                  op
                                  (list op nil)))
                          (collect-parameter-specs)))
                (when (eq (car ll) '&rest)
                  (pop ll)
                  (pop ll))
                (when (eq (car ll) '&key)
                  (pop ll)
                  (prog1 (collect-parameter-specs)
                         (setq allow-other-keys-p
                               (when (eq (car ll) '&allow-other-keys)
                                 (pop ll) t))))
                allow-other-keys-p
                (when (eq (car ll) '&aux)
                  (pop ll)
                  (collect-parameter-specs)))
        (when ll
          (syntax-error "Malformed lambda-list: ~s" lambda-list))))))




;;; Take a lambda list and body, and return a modified body
;;;  and an arglist containing no lambda-list keywords.
;;; This needs to put declarations in the right places.
;;;
(defun hack-lambda-list (lambda-list body)
  (declare (values args rest optionals body))
  (multiple-value-bind (args optionals rest keys allow-other-keys-p auxes)
      (parse-lambda-list lambda-list)
    (when auxes
      (setq body
            `((LET ,auxes
                ,@body
                ))))
    (when keys
      (setq body
            (hack-keys (or rest (setq rest (gensym 'rest)))
                       keys
                       allow-other-keys-p
                       body)))
    (let ((optional-vars (mapcar #'create-variable (mapcar #'car optionals))))
      (values args rest
              optional-vars
              (if optionals
                  (hack-optionals optional-vars optionals (length args) body)
                body
                )))))


(defun getsym (n x)
  (make-symbol (format nil "~a~a" n x)))

;;; &optional
;;;
;;; (labels
;;;   ((2x () (optional-init x (x-init) x-p 3x)) ;=> (optional-init) (setq x (x-init)) (setq x-p nil) (3x)
;;;    (3x () (optional-init y (y-init) nil 4x)) ;=> (optional-init) (setq y (y-init)) (4x)
;;;    (4x () (optional-init z (z-init) z-p 5x)) ;=> (optional-init) (setq z (z-init)) (setq z-p nil) (5x)
;;;    (5x () <body>))
;;;   (optionals-setup 2x 2 '(nil x-p nil z-p) '5x '2x '3x '4x))
;;;
(defun hack-optionals (vars optionals nargs body)
  (let* ((supplied-p-vars '())
         body-label
         (first-label (getsym nargs "x"))
         (label-procs (do ((ops optionals (cdr ops))
                          supplied-p last-supplied-p
                          (vars vars (cdr vars))
                          (nargs nargs (1+ nargs))
                          (setup first-label next-setup)
                          (next-setup (getsym (1+ nargs) "x")
                                      (getsym (+ nargs 2) "x"))
                          (labels '()))
                         ((null ops) (push supplied-p supplied-p-vars)
                          (setq body-label setup)
                          (nreverse (cons `(,setup ()
                                            ,@body)
                                          labels)))
                       (let* ((op (car ops))
                              (init-form (second op)))
                         (setq last-supplied-p supplied-p)
                         (setq supplied-p (if (third op) (create-variable (third op))))
                         (push last-supplied-p supplied-p-vars)
                         (setq labels (list* `(,setup ()
                                               (optional-init ,(car vars) ,init-form ,supplied-p)
                                               (,next-setup))
                                             labels))))))
    `(((lambda ,(remove-if-not #'identity supplied-p-vars)
         (labels ,label-procs
           (optional-setup ,first-label ,nargs ,(setq supplied-p-vars (nreverse supplied-p-vars))
                           ,body-label ,@(butlast (mapcar #'car label-procs)))
;       (,body-label)
           ))
       ,@(make-list (count nil supplied-p-vars :test-not #'eq)
           :initial-value `',undefined)))))

(define-special-form OPTIONAL-INIT (var init-form init-p) (env)
  (prog1 `(PROGN
            ((OPTIONAL-INIT)
             (SETQ-LEXICAL ,var
                           ,(alpha init-form env))
             . ,(when init-p `((SETQ-LEXICAL ,init-p
                                             ,(create-literal-node nil))))))
         (bind-variables env (list var init-p))))

(define-special-form OPTIONAL-SETUP (first-label nargs supplied-p-vars . init-labels) (env)
  `(OPTIONAL-SETUP ,(obtain-fvariable env first-label)
                   ,(create-literal-node nargs)
                   ,(create-literal-node supplied-p-vars)
                   ,(mapcar #'(lambda (label) (create-literal-node (obtain-fvariable enV label)))
                            init-labels)))
;;; &key
;;;
;;; (defun foo (x y &key (a 'aval) (b 'bval bar-p) ((:ckey cvar) (cons a b) c-p))
;;;   ...)
;;;
;;;(defun foo (x y &rest .rest.)
;;;  (multiple-value-bind (a001 b001 cvar001)
;;;      (get-keyword-arg-values .rest. '(:a :b :ckey) <allow-other-keys>)
;;;    (let ((ainit001 #'(lambda () 'aval))
;;;          (a a001))
;;;      (let ((binit001 #'(lambda () 'bval))
;;;            (b b001)
;;;            (b-p '#.undefined))
;;;     (let ((cinit001 #'(lambda () (cons a b)))
;;;           (c c001)
;;;           (c-p '#.undefined))
;;;       (if (eq a <keyword-garbage>)
;;;           (setq a (funcall ainit001)))
;;;       (if (eq b <keyword-garbage>)
;;;           (progn (setq b (funcall binit001))
;;;                  (setq b-p nil))
;;;         (setq b-p t))
;;;       (if (eq cvar <keyword-garbage>)
;;;           (progn (setq cvar (funcall cinit001))
;;;                  (setq c-p nil))
;;;         (setq c-p t))
;;;       ...)))))
;;;
(defun hack-keys (rest keys allow-other-keys-p body)
  (let ((keywords '())
        (vars '())
        (inits '())
        (supplied-ps '()))
    (dolist (keyspec keys)
      (typecase keyspec
        (symbol (push keyspec vars)
                (push (intern (symbol-name keyspec) 'keyword) keywords)
                (push nil inits)
                (push nil supplied-ps))
        (list
         (let ((vspec (car keyspec)))
           (typecase vspec
             (symbol (push vspec vars)
                     (push (intern (symbol-name vspec) 'keyword) keywords))
             (list
              (if (and (symbolp (first vspec))
                       (symbolp (second vspec)))
                  (progn (push (first vspec) keywords)
                         (push (second vspec) vars))
                (syntax-error "Bad keyword specifier: ~s" keyspec)))
             (t (syntax-error "Bad keyword specifier: ~s" keyspec))))
         (push (second keyspec) inits)
         (push (third keyspec) supplied-ps))
        (t (syntax-error "Bad keyword specifier: ~s" keyspec))))
    (setq vars (nreverse vars))
    (setq inits (nreverse inits))
    (setq supplied-ps (nreverse supplied-ps))
    (let ((gen-vars (mapcar #'gensym vars))
          (init-vars (mapcar #'gensym vars)))
      `((MULTIPLE-value-bind ,(copy-list gen-vars)
            (LI:GET-KEYWORD-ARG-VALUES ,rest ',(nreverse keywords) ,allow-other-keys-p)
          ,(do ((form `(PROGN
                         ,@(mapcar #'(lambda (var init-var supplied-p)
                                       `(IF (EQ ,var 'LI:KEYWORD-GARBAGE)
                                            (PROGN (SETQ ,var (FUNCALL ,init-var))
                                                   ,(if supplied-p `(SETQ ,supplied-p NIL)))
                                          ,(if supplied-p `(SETQ ,supplied-p T))))
                                   vars init-vars supplied-ps)
                         . ,body))
                (init-vars (nreverse init-vars) (cdr init-vars))
                (inits     (nreverse inits)     (cdr inits))
                (vars      (nreverse vars)      (cdr vars))
                (gen-vars  (nreverse gen-vars)  (cdr gen-vars))
                (supplied-ps (nreverse supplied-ps) (cdr supplied-ps)))
               ((null vars) form)
             (setq form
                   `(LET ((,(car init-vars) #'(LAMBDA () ,(car inits)))
                          (,(car vars) ,(car gen-vars))
                          ,@(if (car supplied-ps)
                                `((,(car supplied-ps) ',nc:undefined))))
                      ,form))))))))


;;; Make the variables for a lambda, add them to the env, alphatize the
;;; body, and then remove the variables from the env.
(defun alpha-lambda (name lambda-list body env)
  (multiple-value-bind (args rest op-vars body)
      (hack-lambda-list lambda-list body)
    (dolist (op-var op-vars)
      (setf (variable-optional-p op-var) t))
    (let* ((vars (mapcar #'(lambda (name)
                             (let ((v (if name
                                          (if (variable-p name)
                                              name
                                            (create-variable name)))))
                               (if (and (variable-p v)
                                        (symbolp name)
                                        (special-p name))
                                   (setf (variable-special-p v) t))
                               v))
                       (list* rest 'k args)))
           (real-vars (cons (car vars) (cddr vars))))
      (bind-variables env real-vars)
      (let ((exp (list 'LAMBDA name (append vars op-vars)
                       (alpha-list body env))))
        (unbind-variables env op-vars)
        (unbind-variables env real-vars)
;      (return-to-freelist real-vars)
        exp))))


;;; Dispatch
;;; This is not Common Lisp, but is used by system code
;;;
;;;  (prims:dispatch (byte 3 4) word
;;;    (0     (code1))
;;;    ((2 4) (code2))
;;;    (6     (code3))
;;;    (t     (otherwise-code)))
;;;
;;;   ==>
;;;
;;;  (block .dispatch.
;;;    (dispatch-special-form
;;;      #'(lambda () (return-from .dispatch. (progn (otherwise-code))))
;;;      #'(lambda () (return-from .dispatch. (progn (code1))))
;;;      #'(lambda () (return-from .dispatch. (progn (code2))))
;;;      #'(lambda () (return-from .dispatch. (progn (code3))))
;;;      (byte 3 4)
;;;      word
;;;      1 0
;;;      2 2 4))
;;;
;;; dispatch-special-form needs to be a special form so that
;;; we can make the lambdas be exit args
(prims:defmacro prims:dispatch (byte-spec word &body clauses)
  "Extract the byte BYTE-SPEC from WORD and execute a clause selected by the value.
The first element of each clause is a value to compare with the byte value,
or a list of byte values.  These byte values are evaluated, but should be compile
time constants (this is admittedly not well defined).
T or OTHERWISE as the first element of a clause matches any test object.
This is an exception, in that OTHERWISE is not evaluated."
  (declare (zwei:indentation 1 1))
  (let ((procs '())
        (otherwise '#'(lambda () (return-from .dispatch.)))
        (case-list '()))
    (dolist (clause clauses)
      (let ((proc `#'(lambda () (return-from .dispatch. (progn ,@(cdr clause)))))
            (cases (car clause)))
        (if (member cases '(t otherwise))
            (progn (setq otherwise proc) (return))
          (progn
            (push proc procs)
            (setq case-list
                  (nconc case-list
                         (if (consp cases)
                             (cons (length cases) (copy-list cases))
                           (list 1 cases))))))))
    `(block .dispatch.
       (dispatch-special-form ,otherwise
                              ,@(nreverse procs)
                              ,byte-spec ,word
                              ,@case-list))))


(define-special-form dispatch-special-form args (env)
  (cons 'DISPATCH-SPECIAL-FORM (alpha-list args env)))


;;;; Variable references

(defun special-p (symbol)
  (get symbol 'special))


(defun alpha-variable (name env)
  (let ((var (obtain-variable env name)))
    (cond (var
           (if (variable-special-p var)
               (create-special-ref name env)
             var))
          ((special-p name) (create-special-ref name env))
          (t
           (create-free-ref name env)))))

(defun create-special-ref (name env)
  (alpha `(SYMBOL::%SYMBOL-VALUE (QUOTE ,name)) env))

(defun create-free-ref (name env)
  (warn "The variable ~s is used free; assumed special" name)
  (make-special name env)
  (create-special-ref name env))


(defvar *allow-setq-of-global-constant-register* nil)

(define-special-form SETQ name-value-pairs (env)
  (let ((forms '()))
    (do ((pairs name-value-pairs (cddr pairs)))
        ((null pairs))
      (if (cdr pairs)
        (push (alpha-setq (first pairs) (second pairs) env)
              forms)
        (syntax-error "SETQ with an odd number of arguments, the last one is ~s"
                      (car pairs))))
    `(PROGN ,(nreverse forms))))

(defun alpha-setq (name value env)
  (cond ((or (not (symbolp name))
             (and (constantp name)
                  (not (and (global-register name)
                            *allow-setq-of-global-constant-register*))))
         (syntax-error "Can't SETQ ~s" name)
         (alpha value env))
        ((global-register name)
         (list 'SETQ-LEXICAL (global-register name) (alpha value env)))
        (t
         (let ((var (obtain-variable env name)))
           (cond (var
                  (if (variable-special-p var)
                      (list primop/setq-special (create-literal-node name) (alpha value env))
                    (list 'SETQ-LEXICAL var (alpha value env))))
                 ((special-p name)
                  (alpha `(SYMBOL::%%SET (QUOTE ,name) ,value) env)
                  ;(list primop/setq-special (create-literal-node name) (alpha value env))
                  )
                 (t
                  (warn "The variable ~s is used free; assumed special" name)
                  (let ((variable (create-variable name)))
                    (setf (variable-special-p variable) t)
                    (bind-variables env (list variable)))
                  (alpha `(SYMBOL::%%SET (QUOTE ,name) ,value) env)
                  ;(list primop/setq-special (create-literal-node name) (alpha value env))
                  ))))))


;;;; Declarations

(defvar *declaration-handler-table* (make-table '*declaration-handler-table*))

;;; (DECLARE . decl-specs)
;;; Call the handler in the table
;;; Declarations in the wrong places don't get any warning now.
(define-special-form DECLARE decl-specs (env)
  (dolist (decl-spec decl-specs)
    (let ((handler (table-entry *declaration-handler-table* (car decl-spec))))
      (if handler
          (apply handler env (cdr decl-spec))
        (warn "Unknown declaration specifier: ~s" (car decl-spec)))))
  'declare)

(defmacro define-declaration (decl-type lambda-list &body body)
  (let ((sym (concatenate-symbol 'declaration-handler/ decl-type)))
    `(progn
       (defun ,sym ,lambda-list
         ,@body)
       (setf (table-entry *declaration-handler-table* ',decl-type) #',sym))))


(defun make-special (name env)
  (let ((variable (create-variable name)))
    (setf (variable-special-p variable) t)
    (bind-variables env (list variable))))


(defun declare-specialness (special-p env vars)
  (dolist (var vars)
    (let ((variable (obtain-variable env var)))
      (unless variable
        (setq variable (create-variable var))
        (bind-variables env (list variable)))
      (setf (variable-special-p variable) special-p))))


(define-declaration SPECIAL (env &rest vars)
  (declare-specialness t env vars))

(define-declaration UNSPECIAL (env &rest vars)
  (declare-specialness nil env vars))

(defun declare-type (env type vars)
  (dolist (var vars)
    (let ((variable (obtain-variable env var)))
      (if variable
          (setf (variable-type variable) type)
        (warn "There is a type declaration of ~s, which is not bound." var)))))

;;; check if valid type?
(define-declaration TYPE (env type &rest vars)
 (declare-type env type vars))

(defmacro define-type-declarations (&rest types)
  `(progn
     ,@(mapcar #'(lambda (type)
                   `(define-declaration ,type (env &rest vars)
                      (declare-type env ',type vars)))
               types)))

(define-type-declarations
  ARRAY              FIXNUM      PACKAGE            SIMPLE-VECTOR
  ATOM               FLOAT       PATHNAME           SINGLE-FLOAT
  BIGNUM             FUNCTION    RANDOM-STATE       STANDARD-CHAR
  BIT                HASH-TABLE  RATIO              STREAM
  BIT-VECTOR         INTEGER     RATIONAL           STRING
  CHARACTER          KEYWORD     READTABLE          STRING-CHAR
  COMMON             LIST        SEQUENCE           SYMBOL
  COMPILED-FUNCTION  LONG-FLOAT  SHORT-FLOAT        T
  COMPLEX            NIL         SIMPLE-ARRAY       VECTOR
  CONS               NULL        SIMPLE-BIT-VECTOR
  DOUBLE-FLOAT       NUMBER      SIMPLE-STRING)


;;; This is not really right because
;;; the declarations are only supposed to apply within
;;; the special form in which the declaration appears
(defun declare-ftype (env type fcns)
  (dolist (fcn fcns)
    (let ((variable (obtain-fvariable env fcn)))
      (unless variable
        (setq variable (create-variable fcn))
        (bind-fvariables env (list variable)))
      (setf (variable-type variable) type))))

(define-declaration FTYPE (env type &rest function-names)
  (declare-ftype env type function-names))

(define-declaration FUNCTION (env name arg-types &rest result-types)
  (declare-ftype env `(FUNCTION ,arg-types ,@result-types) (list name)))

(define-declaration INLINE (env &rest functions))

(define-declaration NOTINLINE (env &rest functions))

(define-declaration IGNORE (env &rest vars))

(define-declaration OPTIMIZE (env &rest quality-value-pairs))

(define-declaration DECLARATION (env &rest declarations)
  (declare (ignore declarations))
  (warn "DECLARATION is not a valid declaration specifier in DECLARE,~%~
         only in PROCLAIM"))


(define-special-form THE (value-type form) (env)
 (list 'THE value-type (alpha form env)))




;;; (IF p c a)
(define-special-form IF (tested con . maybe-alt) (env)
  (list 'IF (alpha tested env)
        (alpha con env)
        (if maybe-alt
            (alpha (car maybe-alt) env)
          ''NIL)))  ;(create-literal-node nil))))   ;primop/undefined)))


;;; (PROGN . forms)
(define-special-form PROGN exp-list (env)
  (list 'PROGN (alpha-list exp-list env)))


;;; (BLOCK name . forms)

(defvar *blocks* nil)

(define-special-form BLOCK (name . exp-list) (env)
  (let* ((*blocks* (cons (list name) *blocks*))
         (forms (alpha-list exp-list env)))
      (if (cdar *blocks*)
          (list 'BLOCK (cdar *blocks*) forms)
        (list 'PROGN forms))))



(define-special-form unwind (block) (env)
  (error "don't call this"))

;;; (RETURN-FROM name result)
(define-special-form RETURN-FROM (name . value) (env)
  (let ((block (assoc name *blocks*)))
    (cond ((null block)
           (syntax-error "There is a RETURN-FROM ~a not inside a BLOCK of that name." name))
          (t
           (let ((var (or (cdr block)
                          (setf (cdr block) (create-variable (car block))))))
             (list 'RETURN-FROM var
                 (cond (value
                        (if (cdr value)
                            (syntax-error "RETURN-FROM called with multiple value arguments"))
                        (alpha (car value) env))
                       (t nil))))))))


(defvar *tags* nil)

(define-special-form TAGBODY body (env)
  (let ((*tags* *tags*))
  (let ((tagbody-cont (create-variable "K"))
        (initial-body)
        (new-body '())
        (tagname)
        (tags '())
        (tag-procs '()))
    (do ((forms body (cdr forms)))
        ((null forms)
         (if tags
             (push `(NAMED-LAMBDA ,tagname () ,@(nreverse new-body)) tag-procs)
           (setq initial-body (nreverse new-body))))
      (let ((form (car forms)))
        (cond ((or (symbolp form)
                   (integerp form))
               (let ((tag (create-variable form)))
                 (push `(GO-INTERNAL ,tag ,tagbody-cont) new-body)
                 (if tags
                     (push `(NAMED-LAMBDA ,tagname () ,@(nreverse new-body)) tag-procs)
                   (setq initial-body (nreverse new-body)))
                 (setq new-body nil)
                 (push tag tags)
                 (setq tagname form)
                 (push (list* tagname tag tagbody-cont) *tags*)))
              (t (push form new-body)))))
    (if tags
        `(TAGBODY ,tagbody-cont ,(nreverse tags)
           ,(alpha-list (nreverse tag-procs) env)
           ,(alpha-list initial-body env))
      `(PROGN ,(alpha-list initial-body env))))))


(define-special-form GO (tag) (env)
  (let ((tagvar (assoc tag *tags*)))
    (unless tagvar
      (if (or (symbolp tag)
              (integerp tag))
          (syntax-error "There is a GO to tag ~a but no such tag exists" tag)
        (syntax-error "Invalid GO tag: ~a, only symbols or integers allowed" tag)))
    `(GO-INTERNAL ,(cadr tagvar) ,(cddr tagvar))))

;;; GO's created by tagbody to fall into next tag
;;; we already have tagvar
(define-special-form GO-INTERNAL args (env)
  `(GO-INTERNAL . ,args))


;;; CATCH

;;; (defmacro CATCH (tag &body body)
;;;   (let ((cont (gensym 'cont)))
;;;     `(PROGN
;;;        (BLOCK ,cont
;;;          (%CATCH-OPEN ,cont ,tag)
;;;          (%CATCH-BODY (PROGN ,@body)))
;;;        (%CATCH-CONTINUE))))
;;;
(define-special-form CATCH (tag . body) (env)
  (let ((cont (create-variable "CONT")))
    `(PROGN
       ((BLOCK ,cont
          ((%CATCH-OPEN ,cont ,(alpha tag env))
           (%CATCH-BODY ,(alpha `(PROGN ,@body)
                                env))))
        (,primop/%catch-continue)))))

(define-special-form THROW (tag result) (env)
  (let ((tagvar (gensym 'tag)))
    (alpha `(LET ((,tagvar ,tag))
              (THROW-INTERNAL ,tagvar ,result))
           env)))


;;; (LABELS ((v1 e1) (v2 e2) ... (vn en)) . body)

(define-special-form LABELS (specs . body) (env)
  (cond ((null specs)
         (list 'PROGN (alpha-list body env)))
        (t
         (let ((vars (mapcar #'(lambda (spec)
                                 (let ((name (car spec)))
                                   (if (symbolp name)
                                     (create-variable name)
                                     (syntax-error "~s can't be the name of a LABELS function, must be a symbol" name))))
                             specs)))
           (bind-fvariables env vars)
           (let ((exp (list 'LABELS
                            vars
                            (mapcar #'(lambda (spec)
                                        (alpha-lambda (car spec) (cadr spec) (cddr spec)
                                                      env))
                                    specs)
                            (alpha-list body env))))
             (unbind-fvariables env vars)
             exp)))))

(define-special-form FLET (specs . body) (env)
  (cond ((null specs)
         (list 'PROGN (alpha-list body env)))
        (t
         (let ((vars (mapcar #'(lambda (spec)
                                 (let ((name (car spec)))
                                   (if (symbolp name)
                                     (create-variable name)
                                     (syntax-error "~s can't be the name of an FLET function, must be a symbol" name))))
                             specs))
               (procs (mapcar #'(lambda (spec)
                                        (alpha-lambda (car spec) (cadr spec) (cddr spec)
                                                      env))
                                    specs)))
           (bind-fvariables env vars)
           (prog1
             (list 'LABELS
                   vars
                   procs
                   (alpha-list body env))
             (unbind-fvariables env vars))))))


(define-special-form MACROLET (specs . body) (env)
  (dolist (spec specs)
    (fbind (car spec) (cons 'MACRO (si:expand-defmacro spec ())) env))
  (prog1
    (list 'PROGN
          (alpha-list body env))
    (dolist (spec specs)
      (unfbind (car spec) env))))



;;; this is not a Common Lisp special form
;;; it needs an alternate macro definition
(define-special-form MULTIPLE-VALUE-BIND (vars exp . body) (env)
  (let* ((vars (mapcar #'(lambda (name)
                           (when name
                             (create-variable name)))
                       vars)))
    (bind-variables env vars)
    (let ((body-exps (alpha-list body env)))
      (unbind-variables env vars)
      (list 'MULTIPLE-VALUE-BIND vars (alpha exp env) body-exps))))

;;; not Common Lisp special form
(define-special-form VALUES vals (env)
  (list 'VALUES (alpha-list vals env)))


;;; this is sort of like values, but doesn't
;;; get optimised out of CPS
;;; it is mostly used for CATCH
(define-special-form %VALUES (body) (env)
  (list '%VALUES (alpha body env)))


(define-special-form COMPILER-LET (bindlist . body) (env)
  (progv (mapcar #'(lambda (x) (if (atom x) x (car x))) bindlist)
         (mapcar #'(lambda (x) (if (atom x) nil (eval (cadr x)))) bindlist)
    (list 'PROGN (alpha-list body env))))

;;; -*- Mode:LISP; Package:COMPILER; Lowercase:T; Base:8; Readtable:ZL -*-
;;; This file contains the source-level optimizers of the Lisp machine compiler.

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;     "This is insane.  What we clearly want to do is not completely
;;      clear, and is rooted in NCOMPLR."   -- BSG/Dissociated Press.

;>> Foo.  We can Lose Big if we rewrite into a function which the luser has lexically
;>>  redefined (such as QUOTE, CAR, AND, VARIABLE-LOCATION...) Without much frobbing
;>>  and inconvenience there seems little way to avoid that.

;;; This seems to be fairly useful.
(defun function-eq (expression function-name &optional default)
  "Return non-NIL if EXPRESSION is functional reference FUNCTION-NAME (a symbol).
If EXPRESSION is NIL, then the function name is simply compared (with EQ) with DEFAULT."
  (typecase expression
    (cons
     (and (= (list-length expression) 2)
          (lisp:member (car expression) '(function quote))
          ;; >> For some reason, this isn't catching local macro definitions, but it
          ;; >> doesn't matter if you're interested in functions anyway.
          (not (fsymeval-in-function-environment function-name))
          (eq (cadr expression) function-name)))
    (null
     (if default (eq default function-name)))))

;;; this is for frobs which are DEFFed to functions rather than DEFSUBSTed,
;;;  and for various other hairyness
(defcompiler-synonym +$         +)
(defcompiler-synonym plus       +)
(defcompiler-synonym -$         -)
(defcompiler-synonym *$         *)
(defcompiler-synonym times      *)
(defcompiler-synonym //$        //)
(defcompiler-synonym ^$         ^)
(defcompiler-synonym expt       ^)
(defcompiler-synonym 1+$        1+)
(defcompiler-synonym 1-$        1-)
(defcompiler-synonym add1       1+)
(defcompiler-synonym sub1       1-)
(defcompiler-synonym gcd        \\)
(defcompiler-synonym remainder  \)
(defcompiler-synonym cl:rem     \)
(defcompiler-synonym greaterp   >)
(defcompiler-synonym lessp      <)
(defcompiler-synonym >=         )
(defcompiler-synonym <=         )
(defcompiler-synonym //=        )
(defcompiler-synonym short-float small-float)
(defcompiler-synonym null       not)
(defcompiler-synonym lexpr-funcall apply)
(defcompiler-synonym atan2      cl:atan)
(defcompiler-synonym catch      *catch)
(defcompiler-synonym throw      *throw)
(defcompiler-synonym multiple-value-setq multiple-value)
(defcompiler-synonym fixp       integerp)
(defcompiler-synonym bind       %bind)
(defcompiler-synonym zl:map     mapl)
(defcompiler-synonym zl:listp   consp)

;;; The following are here to make list-type structures work more efficiently.
;;; It's easier to put the optimization in the compiler than in DEFSTRUCT.
(defoptimizer nth-optimize nth (x)
  (let ((tem (assq (cadr x) '((0 . car) (1 . cadr) (2 . caddr) (3 . cadddr)))))
    (if (and (= (length x) 3)
             tem)
        `(,(cdr tem) ,(caddr x))
      x)))

(defoptimizer nthcdr-optimize nthcdr (x)
  (if (= (length x) 3)
      (let ((tem (assq (cadr x) '((1 . cdr) (2 . cddr) (3 . cdddr) (4 . cddddr)))))
        (cond ((eq (cadr x) 0) (caddr x))
              (tem `(,(cdr tem) ,(caddr x)))
              (t x)))
    x))

;;; Optimize (CAR (CDR X)) into (CADR X) -- LOOP generates this all the time.
;;; This is really the wrong place in the compiler for this...
;;; EVAL-WHEN so the #.'s below will win the first time.
(eval-when (compile load eval)
(defvar cxrs '(car cdr caar cadr cdar cddr caaar caadr
               cadar caddr cdaar cdadr cddar cdddr
               caaaar caaadr caadar caaddr cadaar cadadr
               caddar cadddr cdaaar cdaadr cdadar cdaddr
               cddaar cddadr cdddar cddddr))

(defvar 3cxrs '(car cdr caar cadr cdar cddr caaar caadr
               cadar caddr cdaar cdadr cddar cdddr)))

(defconst cxr-pop-table
          '#.(loop for sym in (cddr cxrs) collecting
                   (cons sym (intern (string-append #/C (substring sym 2))))))

(defconst cxr-append-table
          '#.(loop for sym in 3cxrs
                   as first = (substring (symbol-name sym)
                                         0 (1- (array-active-length (symbol-name sym))))
                   collect (list sym
                                 (intern (string-append first "AR"))
                                 (intern (string-append first "DR")))))

(defoptimizer 3cxr-optimize     car)
(defoptimizer 3cxr-optimize     cdr)
(defoptimizer 3cxr-optimize     caar)
(defoptimizer 3cxr-optimize     cadr)
(defoptimizer 3cxr-optimize     cdar)
(defoptimizer 3cxr-optimize     cddr)
(defoptimizer 3cxr-optimize     caaar)
(defoptimizer 3cxr-optimize     caadr)
(defoptimizer 3cxr-optimize     cadar)
(defoptimizer 3cxr-optimize     caddr)
(defoptimizer 3cxr-optimize     cdaar)
(defoptimizer 3cxr-optimize     cdadr)
(defoptimizer 3cxr-optimize     cddar)
(defoptimizer 3cxr-optimize     cdddr)

(defun 3cxr-optimize (form)
  (or (when (= (length form) 2)
        (let ((argform (cadr form)))
          (cond ((and (consp argform)
                      (memq (car argform) cxrs))
                 `(,(funcall (if (eql (char (symbol-name (car argform)) 1) #/A)
                                 #'cadr #'caddr)
                             (assq (car form) cxr-append-table))
                   ,(let ((x (cdr (assq (car argform) cxr-pop-table))))
                      (if x `(,x . ,(cdr argform)) (cadr argform)))))
                ((not (equal argform (cadr form)))
                 `(,(car form) ,argform)))))
      form))

;;; don't quote numbers or nil or t -- make life easier for other optimizers.
;;; The $64000 question is whether keywords should keep their QUOTEs... I wonder.
;;;  That would make some things much easier when real compiler support for
;;;  the commonlisp 69 different keyword argument functions is written.
(defoptimizer unquote quote (form)
  (if (and (= (length form) 2)
           (not (keywordp (cadr form)))
           (not (eq (car-safe (cadr form)) 'quote))
           (self-evaluating-p (cadr form)))
      (cadr form)
    form))

(defun fold-constants (form)
  "Replace an expression by its value...if it evaluates ok."
  (condition-case (val)
      (multiple-value-list (eval form))
    (error
     (warn 'constant-folding :error
           "~Error during constant-folding on expression ~S:~%  ~A~" form val)
     form)
    (:no-error
     (if (= (length val) 1)
         `',(first val)
       `(values . ,(mapcar (lambda (elt) `',elt) val))))))      ; Get multiple-values

;;; This really can't do much in the Lambda compiler -- or can it ?
(defun expression-typep (expression type)
  (declare (values typep knownp))
  (condition-case (error)
      (cond ((constantp expression)
             (values (typep (eval expression) type) t))
            ;; A variable
            ((symbolp expression)
             (if (and (get expression 'system-constant) (boundp expression))
                 (values (typep (symbol-value expression) type) t)
               (let ((home (find-variable-home expression)))
                 (when home
                   (let ((typedecl (get-var-declaration home 'type)))
                     (when typedecl
                       (si::compilation-subtypep typedecl type)))))))
            ;; >> General form.  Could check FTYPE and FUNCTION declarations.
            ((and (consp expression) (eq (first expression) 'the))
             (si::compilation-subtypep (second expression) type))
            (t (values nil nil)))
    (si::invalid-type-specifier (values nil nil))
    (error (warn 'bad-type-usage :probable-error
                 "Error while trying to check type of ~S as ~S:~%   ~A"
                 expression type (send error :report-string))
           (values nil nil))))

(defun every-expression-typep (expressions type)
  (lisp:every #'(lambda (exp) (expression-typep exp type)) (the list expressions)))

(defun expression-subtypep (expression type)
  (declare (values subtypep knownp))
  (condition-case (error)
      (cond ((constantp expression)
             (values (si::compilation-subtypep (eval expression) type) t))
            ((and (symbolp expression) (get expression 'system-constant) (boundp expression))
             (values (si::compilation-subtypep (symbol-value expression) type) t))
            (t (values nil nil)))
    (si::invalid-type-specifier (values nil nil))
    (error (warn 'bad-type-usages :probable-error
                 "Error while trying to check subtype of ~S as ~S:~%   ~A"
                 expression type (send error :report-string))
           (values nil nil))))

(defun every-expression-subtypep (types type)
  (lisp:every #'(lambda (typ) (expression-subtypep typ type)) (the list types)))

(defun expression-type-equal (expression type)
  (flet ((type-equal (t0 t1)
           (or (equal t0 t1)
               (and (si::compilation-subtypep t0 t1) (si::compilation-subtypep t1 t0)))))
    (condition-case (error)
        (values
         (cond ((constantp expression)
                (type-equal (eval expression) type))
               ((and (symbolp expression) (get expression 'system-constant) (boundp expression))
                (type-equal (symbol-value expression) type) t)))
    (si::invalid-type-specifier nil)
    (error (warn 'bad-type-usages :probable-error
                 "Error while trying to check type equivalance of ~S and ~S:~%   ~A"
                 expression type (send error :report-string))
           nil))))

(defun every-expression-type-equal (types type)
  (lisp:every #'(lambda (typ) (expression-type-equal typ type)) (the list types)))

(defun make-expression-match-check (after-rest-p param)
  (let ((type (second param)))
    (or (when (consp type)
          (case (first type)
            (:subtype
             `(,(if after-rest-p 'every-expression-subtypep 'expression-subtypep)
               ,(first param) ',(second type)))
            (:supertype
             `(,(if after-rest-p 'every-expression-subtypep 'expression-subtypep)
               ',(second type) ,(first param)))
            (:type
             `(,(if after-rest-p 'every-expression-type-equal 'expression-type-equal)
               ,(first param) ',(second type)))
             ))
        `(,(if after-rest-p 'every-expression-typep 'expression-typep)
          ,(first param) ',(second param)))))

;;; Parameters for matching argument types:
;;; IGNORE -- ignore
;;; symbol -- Parameter, matches any type
;;; (symbol type [optional-default] [optional-supplied-p]) -- match a variable of a type,
;;;  optional frobs legal after &OPTIONAL.
;;; Understood keywords: &OPTIONAL, &KEY, &REST, &ALLOW-OTHER-KEYS, &WHOLE
;;; The optional-default form should itself return a FORM.
;;; For the &REST argument, ALL expressions must of the specified type
;;; Special ``type specifiers'':
;;; (:SUBTYPE type-specifier): Useful when the parameter itself gets a type specifier;
;;;  matches when it's (SI::COMPILATION-)SUBTYPEP the type specifier.
;;; (:SUPERTYPE type-specifier): Matches when the type-specifier is SUBTYPEP the parameter.
;;; As a special case, if the last parameter is &REST, a match will occur only if no
;;; other arguments were passed.
(defun typed-optimizer-function (parameters body)
  (declare (values lambda-list form doc))
  (multiple-value-bind (body decls doc) (extract-declarations body '((sys:downward-function)) t)
    (do* ((params parameters (cdr params))
          (param (car params) (car params))
          (whole '.form.)
          allow-optional-vars after-rest-p
          lambda-list
          checks)
         ((null params) (values (list whole)
                                `(apply #'(lambda ,(nreverse lambda-list)
                                            (declare ,@decls)
                                            (if (and ,@checks) (progn ,@body) ,whole))
                                        (cdr ,whole))
                                doc))
      (macrolet ((collect (x) `(push ,x lambda-list)))
        (case param
          ((&optional &key)
           (collect param)
           (setq allow-optional-vars t after-rest-p nil))
          (&allow-other-keys
           (collect param)
           (setq allow-optional-vars nil))
          (&rest
           (collect param)
           (unless (cdr params) ;; check for &REST at end
             (collect '.others.)
             (push '(null .others.) checks))
           (setq allow-optional-vars nil after-rest-p t))
          (&whole
           (pop params)
           (setq whole (car params)))
          (otherwise
           (etypecase param
             ((and symbol (not null))
              (if (lisp:member param lambda-list-keywords)
                  (error "Unknown lambda list keyword ~S" param)
                (collect param)))
             (cons
              (push (make-expression-match-check after-rest-p param)
                    checks)
              (collect (if (and allow-optional-vars (cddr param))
                           (cons (first param) (cddr param))
                         (first param)))))
           (setq after-rest-p nil)))))))

(defmacro define-typed-optimizer (name parameters &body body)
  "Define a function which will either return a new form, or the old one.
PARAMETERS is like a lambda-list for matching the types of arguments.
Each element can be:
a symbol -- an argument of any type
/(symbol type default supplied-p) -- matches the symbol with an expression
 of known type.  The second two elements are used with parameters that can
 be optional (after &OPTIONAL and &KEY).
A &REST parameter matches when every expression matches.  If &REST is the
 last element of the parameter list, more arguments are allowed in the
 original form, but they never match.  This is useful when optimizing a
 function for getting just one (or no) rest arguments.
&ALLOW-OTHER-KEYS is allowed; &WHOLE is also allowed.

If the parameters match, BODY computes the optimized form.  If it turns that
that the form cannot be optimized even with the match, the original form should
be returned by using the &WHOLE parameter.

Somes special ``type'' specifiers are accepted: (:SUBTYPE x).  This matches if the
parameter itself is a type specifier (known at compile time), and it is a
subtype of x.  (:SUPERTYPE x) matches when x is a subtype of the parameter.
(:TYPE x) x and the parameter are type-equivalent."
  (multiple-value-bind (opt-lambda-list opt-form) (typed-optimizer-function parameters body)
    `(defun ,name ,opt-lambda-list ,opt-form)))

(defmacro add-typed-optimizer (function optimizer-name &optional parameters &body body)
  "Install a type-based optimizer for FUNCTION.
Either OPTIMIZER-NAME has been defined with DEFINE-TYPED-OPTIMIZER, or the
parameters and body are supplied to define it.  See DEFINE-TYPED-OPTIMIZER for
detail on matching types."
  `(progn
     ,(when parameters
        `(define-typed-optimizer ,optimizer-name ,parameters ,@body))
     (add-optimizer-internal ',function ',optimizer-name)))

;;; Optimize forms such as (+ 3 2) and (+ 3 a 2).  These must be done before ARITHEXP
(defoptimizer arith-opt +)
(defoptimizer arith-opt *)
(defoptimizer arith-opt -)
(defoptimizer arith-opt difference)
(defoptimizer arith-opt //)
(defoptimizer arith-opt quotient)
(defoptimizer arith-opt cl://)
(defoptimizer arith-opt logior)
(defoptimizer arith-opt logand)
(defoptimizer arith-opt logxor)
(defoptimizer arith-opt min)
(defoptimizer arith-opt max)

(defun arith-opt (form)
  (if ( (length form) 2) form                  ;Let ARITHEXP handle this.
    (loop for arg in (cdr form)
          when (numberp arg)
               collect arg into winners
          else collect arg into losers
       finally
         (return (cond ((null (cdr winners)) form)      ;Can't hope to optimize.
                       ((null losers) (fold-constants form))    ;Easy optimization.
                       ;; Now we are left with at least two args which are numbers, but at
                       ;; least one which is not.  Frobbing with divide from here on is
                       ;; dangerous, eg, (// 5 a 4) must not optimize into (// 1 a).
                       ((memq (car form) '(// quotient cl://)) form)
                       ;; The only special case left is DIFFERENCE, which treats
                       ;; its first arg differently.
                       ((or (not (memq (car form) '(- difference)))
                            (numberp (cadr form)))
                        `(,(car form) ,(apply (car form) winners) . ,losers))
                       (t `(,(car form) ,@losers ,(apply #'+ winners))))))))

;;; Express multi-argument arithmetic functions in terms of two-argument versions.
(defoptimizer arithexp +)
(defoptimizer arithexp -)
(defoptimizer arithexp difference)
(defoptimizer arithexp *)
(defoptimizer arithexp //)
(defoptimizer arithexp quotient)
(defoptimizer arithexp cl://)
(defoptimizer arithexp logand)
(defoptimizer arithexp logior)
(defoptimizer arithexp logxor)
(defoptimizer arithexp min)
(defoptimizer arithexp max)

(defprop +              *plus   two-argument-function)
(defprop *              *times  two-argument-function)
(defprop -              *dif    two-argument-function)
(defprop difference     *dif    two-argument-function)
(defprop //             *quo    two-argument-function)
(defprop quotient       *quo    two-argument-function)
(defprop cl://          %div    two-argument-function)
(defprop logior         *logior two-argument-function)
(defprop logand         *logand two-argument-function)
(defprop logxor         *logxor two-argument-function)
(defprop min            *min    two-argument-function)
(defprop max            *max    two-argument-function)

(defun arithexp (x &aux (l (length (cdr x))) (op (get (car x) 'two-argument-function)))
  (cond ((null op)
         (barf x 'bad-op-arithexp 'barf))
        ((= 0 l)
         (or (setq l (assq op '((*plus . 0) (*dif . 0) (*times . 1) (*quo . 1) (%div . 1))))
             (warn 'bad-arithmetic ':implausible
                   "~S called with no arguments" x))
         (cdr l))
        ((= l 1)
         (case (car x)
           (-           `(minus ,(cadr x)))
           (//          `(*quo 1 ,(cadr x)))
           (cl://       `(%div 1 ,(cadr x)))
           (t           (cadr x))))             ;+ * logior logxor logand mix max
        ((= l 2) `(,op . ,(cdr x)))
        (t `(,op (,(car x) . ,(butlast (cdr x))) . ,(last x)))))

(defoptimizer *plus-to-1+ *plus (form)
  (cond ((eq (cadr form) 1) `(1+ ,(caddr form)))        ;(+ 1 x)
        ((eq (caddr form) 1) `(1+ ,(cadr form)))        ;(+ x 1)
        ((eq (cadr form) 0) (caddr form))               ;(+ 0 x)
        ((eq (caddr form) 0) (cadr form))               ;(+ x 0)
        (t form)))

(defoptimizer *dif-to-1- *dif (form)
  (cond ((eq (caddr form) 1) `(1- ,(cadr form)))        ;(- x 1)
        ((eq (cadr form) 0) `(minus ,(caddr form)))     ;(- 0 x)
        ((eq (caddr form) 0) (cadr form))               ;(- x 0)
        (t form)))

;;; Forms such as (SQRT 5) optimize into 2.236
(defoptimizer arith-opt-non-associative sqrt)
(defoptimizer arith-opt-non-associative exp)
(defoptimizer arith-opt-non-associative log)
(defoptimizer arith-opt-non-associative sin)
(defoptimizer arith-opt-non-associative sind)
(defoptimizer arith-opt-non-associative cos)
(defoptimizer arith-opt-non-associative cosd)
(defoptimizer arith-opt-non-associative tan)
(defoptimizer arith-opt-non-associative tand)
(defoptimizer arith-opt-non-associative asin)
(defoptimizer arith-opt-non-associative acos)
(defoptimizer arith-opt-non-associative atan)
(defoptimizer arith-opt-non-associative cl:atan)
(defoptimizer arith-opt-non-associative sinh)
(defoptimizer arith-opt-non-associative cosh)
(defoptimizer arith-opt-non-associative tanh)
(defoptimizer arith-opt-non-associative asinh)
(defoptimizer arith-opt-non-associative acosh)
(defoptimizer arith-opt-non-associative atanh)
(defoptimizer arith-opt-non-associative ^)
(defoptimizer arith-opt-non-associative minus)
(defoptimizer arith-opt-non-associative 1+)
(defoptimizer arith-opt-non-associative 1-)
(defoptimizer arith-opt-non-associative ash)
(defoptimizer arith-opt-non-associative lsh)
(defoptimizer arith-opt-non-associative rot)
(defoptimizer arith-opt-non-associative dpb)
(defoptimizer arith-opt-non-associative ldb)
(defoptimizer arith-opt-non-associative deposit-byte)
(defoptimizer arith-opt-non-associative load-byte)
(defoptimizer arith-opt-non-associative floor)
(defoptimizer arith-opt-non-associative ceiling)
(defoptimizer arith-opt-non-associative truncate)
(defoptimizer arith-opt-non-associative round)
(defoptimizer arith-opt-non-associative eq)
(defoptimizer arith-opt-non-associative equal)
(defoptimizer arith-opt-non-associative =)
(defoptimizer arith-opt-non-associative \)
(defoptimizer arith-opt-non-associative \\)
(defoptimizer arith-opt-non-associative assq)
(defoptimizer arith-opt-non-associative cdr)
;(defoptimizer arith-opt-non-associative float) ;done in float-optimizer
(defoptimizer arith-opt-non-associative small-float)
(defoptimizer arith-opt-non-associative char-code)      ;perhaps stretching "arith"
(defoptimizer arith-opt-non-associative char-bits)      ; a little too far...
(defoptimizer arith-opt-non-associative char-int)       ;
(defoptimizer arith-opt-non-associative int-char)       ;


(defun arith-opt-non-associative (form)
  (if (loop for arg in (cdr form)
         always (constantp arg))
      (fold-constants form)
    form))


(defoptimizer not-not not (form)
  ;; we actually catch a couple cases here which aren't peephole-optimized
  (cond (( (list-length form) 2)
         form)
        ((constantp (cadr form))
         (fold-constants form))
        ((list-match-p (cadr form) `(not ,ignore))
         (cond ((eq *p1value* 'predicate)
                (cadr (cadr form)))
               ((eq (car-safe (cadr (cadr form))) 'not)
                (cadr (cadr form)))
               (t
                form)))
        (t
         form)))

(defoptimizer boole-expand boole (x)
  (let ((l (length x))
        (op (cadr x))
        inst)
    (cond ((< l 3) x)
          ((= l 3) (caddr x))
          ((and (numberp op)
                (setq inst (assq op '((#,boole-and . logand)
                                      (#,boole-xor . logxor)
                                      (#,boole-ior . logior)))))
           `(,(cdr inst) . ,(cddr x)))
          ((= l 4) `(*boole . ,(cdr x)))
          (t `(*boole ,op
                      (boole ,op . ,(butlast (cddr x)))
                      ,(car (last x)))))))

;;;BYTE

(defoptimizer byte-expand byte (x)
  (or (when (and (= (length x) 3)
                 (numberp (cadr x))
                 (numberp (caddr x)))
        `(dpb ,(caddr x) ,%%byte-specifier-position ,(cadr x)))
      x))

;;;BYTE SPEC callers

(defun check-byte-spec (form &optional (fcn (car form)) (warn t) &aux flag)
  (let* ((argpos (or (get fcn 'check-byte-spec-arg) 0))
         (barg (nth (1+ argpos) form))
         (definitely-not-portable t))
    (when
      (setq flag
            ;;Return to FLAG the warning, if any
            (macrolet ((middle (prefix)
                               (format nil "~A ~~S as a BYTE specifier" prefix))
                       (at-end (prefix)
                               (format nil "~A BYTE specifier /"~~S/"" prefix))
                       (simple (prefix)
                               (format nil "~A BYTE specifier~~*" prefix)))
              (typecase barg
                (keyword (middle "the keyword"))        ;Keywords will never be right
                ((member t nil)                         ; T, NIL other selv-evaling vars
                 (if barg (simple "T as a")
                   (simple "a null")))
                (symbol nil)                            ;Variables? We can't be sure.
                (list                                   ;"Random" function calls are not portable.
                 (setq barg (car barg))
                 (cond
                   ((get barg 'legal-byte-specifier) nil)       ;The ideal: calls to BYTE
                   (t (setq definitely-not-portable nil)        ;Can't be sure
                      (middle "a call to"))))
                (number (middle "the numeric constant"))        ;Numbers
                (t (at-end "an invalid")))))
      (let ((msg (format nil "~@[~S called with ~]~A (this ~:[may not be~;is not~] portable)"
                         fcn
                         (format nil flag barg)
                         definitely-not-portable)))
        (if warn
            (warn 'invalid-byte-spec :not-portable msg)
          msg)))))

;;;Put the LEGAL-BYTE-SPECIFIER property on things like BYTE that return
;;;portable, legally formatted byte specifiers -- i.e., they don't do any
;;;processor-specific arithmetic.
(defprop byte t legal-byte-specifier)

(defmacro check-byte-spec-arg (sym &optional (argpos 0))
  (check-type sym symbol)
  (check-type argpos (integer 0))
  `(eval-when (eval compile load)
     (progn
       (unless (fboundp ',sym)
         (warn 'function-not-valid :implausible
           "Establishing byte-spec checker for undefined function ~S" ',sym))
       (let ((prev-check (get ',sym 'style-checker)))
         (and prev-check (neq prev-check 'check-byte-spec)
              (warn 'redefining-style-checker :probable-error
                "Redefining style check on ~S from ~S to ~S"
                ',sym prev-check 'check-byte-spec)))
       (putprop ',sym 'check-byte-spec 'style-checker)
       (putprop ',sym ,argpos 'check-byte-spec-arg))))

(check-byte-spec-arg byte-position)
(check-byte-spec-arg byte-size)
(check-byte-spec-arg dpb 1)
(check-byte-spec-arg ldb)
(check-byte-spec-arg ldb-test)
(check-byte-spec-arg mask-field)
(check-byte-spec-arg deposit-field 1)
(check-byte-spec-arg %logldb)
(check-byte-spec-arg %logdpb 1)
(check-byte-spec-arg %p-ldb)
(check-byte-spec-arg %p-ldb-offset)
(check-byte-spec-arg %p-mask-field)
(check-byte-spec-arg %p-mask-field-offset)
(check-byte-spec-arg %p-dpb 1)
(check-byte-spec-arg %p-dpb-offset 1)
(check-byte-spec-arg %p-deposit-field 1)
(check-byte-spec-arg %p-deposit-field-offset 1)


;;; something that won't care about order of evaluation
(defun trivial-form-p (x)
  (or (symbolp x)
      (constantp x)))

(defoptimizer convert-\\ \\ (form)
  (loop for arg-form in (cdddr form)
        with answer = `(internal-\\ ,(second form) ,(third form))
     do (setq answer `(internal-\\ ,answer ,arg-form))
     finally (return answer)))

(defoptimizer float-optimizer float (form)
  (cond ((null (cddr form))                     ;One arg
         `(internal-float ,(cadr form)))
        ((numberp (caddr form))                 ;Second arg a number
         (if (small-floatp (caddr form))
             (if (numberp (cadr form))
                 (small-float (cadr form))
               `(small-float ,(cadr form)))
           (if (numberp (cadr form))
               (float (cadr form))
             `(internal-float ,(cadr form)))))
        (t form)))

;;;; Expand the numerical equality/sign predicates.

(defoptimizer =-optimizer = (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           (warn 'wrong-number-of-arguments :implausible
                 "~S called with too few arguments" (car form))
           ''t)
          ((= n-args 2)
           (cond ((eq (first args) 0)
                  `(zerop ,(second args)))
                 ((eq (second args) 0)
                  `(zerop ,(first args)))
                 (t `(internal-= . ,args))))
          ((cl:every #'trivial-form-p (cdr args))
           `(and . ,(loop for arg in (cdr args)
                          and for last-arg first (car args) then arg
                          collect `(internal-= ,last-arg ,arg))))
          (t form))))

(defoptimizer char-equal-optimizer char-equal (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           `(progn ,(car args) 't))
          ((= n-args 2)
           `(internal-char-equal . ,args))
          ((cl:every #'trivial-form-p (cdr args))
           `(and . ,(loop for arg in (cdr args)
                          and for last-arg first (car args) then arg
                          collect `(internal-char-equal ,last-arg ,arg))))
          (t form))))

(defoptimizer >-optimizer > (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           `(progn ,(car args) 't))
          ((= n-args 2)
           (if (eq (second args) '0)
               `(plusp ,(first args))
             `(internal-> . ,args)))
          ((cl:every #'trivial-form-p (cdr args))
           `(and . ,(loop for arg in (cdr args)
                          and for last-arg first (car args) then arg
                          collect `(internal-> ,last-arg ,arg))))
          (t form))))

(defoptimizer <-optimizer < (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           `(progn ,(car args) 't))
          ((= n-args 2)
           (if (eq (second args) 0)
               `(minusp ,(first args))
             `(internal-< . ,args)))
          ((cl:every #'trivial-form-p (cdr args))
           `(and . ,(loop for arg in (cdr args)
                          and for last-arg first (car args) then arg
                          collect `(internal-< ,last-arg ,arg))))
          (t form))))


(defoptimizer -optimizer  (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           `(progn ,(car args) 't))
          ((= n-args 2)
           (if (eq (second args) 0)
               `(not (minusp ,(first args)))
             `(not (internal-< . ,args))))
          ((cl:every #'trivial-form-p (cdr args))
           `(and . ,(loop for arg in (cdr args)
                          and for last-arg first (car args) then arg
                          collect `(not (internal-< ,last-arg ,arg)))))
          (t form))))

(defoptimizer -optimizer  (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           `(progn ,(car args) 't))
          ((= n-args 2)
           (if (eq (second args) 0)
               `(not (plusp ,(first args)))
             `(not (internal-> . ,args))))
          ((cl:every #'trivial-form-p (cdr args))
           `(and . ,(loop for arg in (cdr args)
                          and for last-arg first (car args) then arg
                          collect `(not (internal-> ,last-arg ,arg)))))
          (t form))))

(defoptimizer -optimizer  (form)
  (let* ((args (cdr form))
         (n-args (length args)))
    (cond ((< n-args 2)
           `(progn ,(car args) 't))
          ((= n-args 2)
           (if (eq (second args) 0)
               `(not (zerop ,(first args)))
             `(not (= . ,args))))
          ((and (= n-args 3)
                (cl:every #'trivial-form-p args))
           `(not (or (= ,(car args) ,(cadr args))
                     (= ,(car args) ,(caddr args))
                     (= ,(cadr args) ,(caddr args)))))
          (t form))))

(defun constant-function-p (x)
  (cond ((eq (car-safe x) 'lambda)
         x)
        ((and (memq (car-safe x) '(function quote))
              (functionp (cadr x)))
         (cadr x))
        (t
         nil)))

(defun call-function (function-exp arg-exps)
  (let ((tem (constant-function-p function-exp)))
    (if tem `(,tem . ,arg-exps)
      `(funcall ,function-exp . ,arg-exps))))

;;; Optimize (FUNCALL (FUNCTION (LAMBDA ...)) ...) into ((LAMBDA ...) ...).
;;; Does not optimize (FUNCALL (FUNCTION FOO) ...) if FOO is not defined
;;; or takes quoted args (FUNCTIONP checks for that).
(defoptimizer funcall-function funcall (form)
  (let ((tem (constant-function-p (cadr form))))
    (if tem `(,tem . ,(cddr form))
      form)))

;; (list) => ()
(defrewrite list-no-args list (form)
  (if (equal form '(list))
      'nil
    form))

;;; Turn (CONS foo NIL) into (NCONS foo), saving one instruction.
(defoptimizer cons-ncons cons (form)
  (or (and (= (length form) 2)
           (null (caddr form))
           `(ncons ,(cadr form)))
      form))

(defrewrite list-in-area-fencepost-case list-in-area (form)
  ;; The %MISC instruction for LIST-IN-AREA can't handle this case.
  (if (= (length form) 2)
      'nil
    form))

(defrewrite list*-in-area-fencepost-case list*-in-area (form)
  ;; The %MISC instruction for LIST*-IN-AREA can't handle this case.
  (case (length form)
    (2
     'nil)
    (3
     (third form))
    (t
     form)))

;; (function (lambda ...)) => (lambda ...)
(defrewrite function-lambda-to-lambda function (form)
  (if (list-match-p form `(,ignore (lambda . ,ignore)))
      (cadr form)
    form))

(defoptimizer call-to-multiple-value-list call (form)
  (if (and (= (length form) 4)
           (si:member-equal (caddr form)
                            '('(:optional :spread) '(:spread :optional)
                              :spread ':spread)))
      (let ((argform (cadddr form))
            (function (cadr form)))
        (if (and (eq (car-safe argform) 'multiple-value-list)
                 (eq (car-safe function) 'lambda)
                 (not (memq '&key (cadr function)))
                 (if (si:member-equal (caddr form) '(:spread ':spread))
                     ;; Horrible, horrible, horrible kludge.
                     ;; I'm embarrassed to have ever thought of this.
                     ;; USED to be used by the catch-continuation continuations
                     ;; generated by condition-system macros
                     (eq (cadr (memq '&rest (cadr function))) 'ignore)
                   (not (memq '&rest (cadr function)))))
            ;; (call (lambda (x y z) ..) '(:spread :optional) (multiple-value-list ...))
            ;; and the lambda does not have a rest arg.
            ;; since we know how many args it wants, we can avoid consing the list of vals.
            ;; This weird optimization is for the sake of code made by CATCH-CONTINUATION.
            (let ((nargs (ldb %%arg-desc-max-args
                              (si::args-info-from-lambda-list
                                (ldiff (cadr function) (memq '&rest (cadr function)))))))
              (if (= nargs 1)
                  `(,function ,(cadr argform))
                `(progn (multiple-value-push ,nargs ,(cadr argform))
                        (,function . ,(make-list nargs :initial-element '(%pop))))))
          form))
    ;; The optimizations done on APPLY are not correct to do here,
    ;; because they would cause the function to get an error
    ;; if it does not want all the arguments.
    form))

;;; Turn (MULTIPLE-VALUE-BIND (one-variable) (form) body...) into
;;;   (LET ((one-variable form)) body...)
(defoptimizer optimize-simple-mv-bind multiple-value-bind (form)
  (if (cl:listp (cadr form))
      (case (length (second form))
        (0 `(progn . ,(cddr form)))
        (1 `(let ((,(car (cadr form)) ,(caddr form)))
              . ,(cdddr form)))
        (t form))                               ;Want  2 values
    form))                                      ;losing utterly

;;; Turn (FUNCALL SELF ...) into (FUNCALL-SELF ...) if within a method or a function with a
;;;   SELF-FLAVOR declaration.
;;; Leave it alone otherwise -- that would be a pessimization.
(defoptimizer optimize-funcall-self funcall (form)
  (if (and (not (null self-flavor-declaration))
           (eq (second form) 'self))
      `(funcall-self . ,(cddr form))
    form))

;;>> this is extremely obsolete
(defrewrite apply-on-no-args apply (form)
  (case (length form)
    (2 (let* ((arg (cadr form))
              (result (once-only (arg)
                        `(apply (car ,arg) (cdr ,arg)))))
         (warn 'apply-on-one-arg :obsolete
               "~~S called on one argument.~@
                This is being compiled to be ~S for compatibility,~@
                however, it will soon be changed to mean just ~S~"
               'apply result `(funcall ,arg))
         result))
    (t
     form)))

(defrewrite apply-on-interesting-function apply (form)
  (block nil
    (let ((function (cadr form)))
      (cond ((eq (cadr form) 'self)
             (return (if (not (null self-flavor-declaration))
                         `(lexpr-funcall-self . ,(cddr form))
                       form)))
            ((and (eq (car-safe function) 'function)
                  (symbolp (cadr function))
                  (not (fsymeval-in-function-environment (cadr function))))
             (setq function (cadr function)))
            ((and (eq (car-safe function) 'quote)
                  (symbolp (cadr function)))
             (setq function (cadr function)))
            (t (return form)))
      (cond ((and (eq function 'values)
                  (null (cdddr form)))
             ;; (apply #'values #) => (values-list #)
             `(values-list ,(caddr form)))
            ((eq function 'send)
             `(lexpr-send . ,(cddr form)))
            ((eq function 'send-if-handles)
             `(lexpr-send-if-handles . ,(cddr form)))
            ((eq function 'funcall)
             `(apply . ,(cddr form)))
            (t form)))))

(defoptimizer apply-on-list apply (form)
  (let ((lastarg (car (last form)))
        (function (cadr form)))
    (cond ((atom lastarg) form)
          ((eq (car lastarg) 'list)
           ;; If function to be called is quoted symbol, optimize out the "funcall"
           ;; in case the symbol is a subst function.
           (call-function function (nconc (butlast (cddr form)) (cdr lastarg))))
          ((and (eq (car-safe function) 'lambda)
                (eq (car lastarg) 'multiple-value-list)
                ;; Horrible, horrible, horrible kludge.
                ;; I'm embarrassed to have ever thought of this.
                ;; USED to be used by the catch-continuation continuations
                ;; generated by condition-system macros
                (and (eq (cadr (memq '&rest (cadr function))) 'ignore)
                     (not (memq '&key (cadr function)))))
           ;; (apply (lambda (... &rest ignore ...) ...) (multiple-value-list ...))
           (let* ((ordinary (butlast (cddr form)))
                  (nargs (ldb %%arg-desc-max-args
                              (si::args-info-from-lambda-list
                                (ldiff (cadr function) (memq '&rest (cadr function))))))
                  (rargs (- nargs (length ordinary))))
             (case rargs
               (0 `(,function ,@ordinary (progn ,(cadr lastarg) nil)))
               (1 `(,function ,@ordinary ,(cadr lastarg)))
               (t (if (null ordinary)
                      `(progn (multiple-value-push ,rargs ,(cadr lastarg))
                              (,function . ,(make-list rargs :initial-element '(%pop))))
                    form)))))
          ((memq (car lastarg) '(list* cons))
           `(apply ,@(butlast (cdr form)) . ,(cdr lastarg)))
          ((and (eq (car lastarg) 'quote)
                (consp (cadr lastarg)))
           ;; (apply # ... '(#))
           `(funcall ,@(butlast (cdr form))
                     . ,(mapcar (lambda (x) (list 'quote x)) (cadr lastarg))))
          (t form))))

(defoptimizer and-or-no-op and)
(defoptimizer and-or-no-op or (form)
  (cond ((null (cdr form))
         (if (eq (car form) 'and)
             ''t
             ''nil))
        ((null (cddr form))
         (cadr form))
        (t form)))

(defoptimizer 1-arg-no-op progn)
(defoptimizer 1-arg-no-op list* (form)
  (if (cddr form)
      form
    (cadr form)))

(defoptimizer prog2-no-op prog2 (form)
  (if (or (cadr form) (cdddr form))
      form
    `(values ,(caddr form))))

;;; This is really only here for catching system code using the Common Lisp readtable
;;; -- when IF became incompatible, the lossage started.  Callers should be FIXED !
(defoptimizer fix-cli-if cli:if (form)
  (cond ((< (length form) 5)
         form)
        (t
         (warn 'improper-common-lisp :obsolete "~S used with ~D extra argument~:P."
               'cli:if (- (length form) 4))
         `(cond (,(cadr form) ,(caddr form)) (t ,@(cdddr form))))))

;;; Turn EQUAL into EQ when that is safe.
;;; EQUAL can never be turned into = alone because = signals an error if either
;;; arg is not a number, whereas EQUAL does not.  However, (EQUAL <fixnum> xxx)
;;; can be turned into EQ since EQ "works" for fixnums.
;;; Also EQUALwith one of the arguments a number turns into
;;; (AND (NUMBERP <form>) (= <number> <form>))
(defoptimizer equal-eq-= equal (form)
  (flet ((eq-same (x)
           (or (fixnump x)
               (eq x 'nil)
               (eq x 't)
               (keywordp x)
               (and (eq (car-safe x) 'quote)
                    (or (fixnump (cadr x))
                        (symbolp (cadr x))))))
         (equal-= (number atom)
           `(and (numberp ,atom) (= ,number ,atom))))
    (cond ((or (eq-same (cadr form))
               (eq-same (caddr form)))
           `(eq . ,(cdr form)))
          ((and (numberp (cadr form)) (atom (caddr form)))
           (equal-= (cadr form) (caddr form)))
          ((and (numberp (caddr form)) (atom (cadr form)))
           (equal-= (caddr form) (cadr form)))
          (t form))))

;;; Turn (EQ FOO NIL) into (NOT FOO).
(defoptimizer eq-nil eq (form)
  (if (= (length form) 3)
      (let ((x (cadr form))
            (y (caddr form)))
        (cond ((null x) `(not ,y))
              ((null y) `(not ,x))
              (t form)))
    form))

(defoptimizer memq-eq zl:member)
(defoptimizer memq-eq si:member-equal)
(defoptimizer memq-eq member-eql)
(defoptimizer memq-eq member-equalp)
(defoptimizer memq-eq memq (form)
  (or (when (= (length form) 3)
        (let ((item (cadr form))
              (list (caddr form)))
          (if (eq (car-safe list) 'quote)
              (case (list-length (cadr list))
                (0 `(progn ,item nil))
                (1 `(and (,(car (rassq (car form) '((eq . memq)
                                                    (equal . si:member-equal)
                                                    (equal . zl:member)
                                                    (eql . member-eql)
                                                    (equalp . si:member-equalp))))
                          ,item ',(car (cadr list)))
                         ',(cadr list)))))))
      form))

;;; (eql x 1) => (eq x 1)
(defoptimizer eql-eq eql (form)
  (or (when (and (= (length form) 3)
                 (or (and (self-evaluating-p (cadr form))
                          (typep (cadr form) '(or (not number) fixnum short-float)))
                     (and (self-evaluating-p (caddr form))
                          (typep (caddr form) '(or (not number) fixnum short-float)))))
        `(eq ,(cadr form) ,(caddr form)))
      form))

;;; (member-eql x '(a b c)) => (memq x '(a b c))
(defoptimizer member-eql-memq member-eql (form)
  (or (when (= (length form) 3)
        (let ((item (cadr form))
              (list (caddr form)))
          (if (or (and (self-evaluating-p item)
                       (typep item '(or (not number) fixnum short-float)))
                  (and (eq (car-safe list) 'quote)
                       (cl:listp (cadr list))
                       (loop for x in (cadr list)
                          always (typep x '(or (not number) fixnum short-float)))))
              `(memq ,item ,list))))
      form))

;;; List search optimizers
;;; * Not in microcode in 3.0.  Someday, ASSOC-EQL should be in microcode.
;;; A       B         C     D
;;; ZL:MEM  CL:MEMBER MEMQ  SI:MEMBER-{EQL,EQUAL,EQUALP*}
;;; ZL:ASS  CL:ASSOC  ASSQ  SI:ASSOC-{EQL*,EQUAL,EQUALP*}
;;; ZL:RASS ZL:RASSOC RASSQ SI:RASSOC-{EQL*,EQUAL*,EQUALP*}
;;;
;;; A: (pred item list)
;;; B: (item list &key test test-not), CL:MEMBER also has &KEY
;;; C: (item list), test is EQ
;;; D: (item list), test is wired in as named.  These functions reside in
;;;    SYS; QFCTNS
;;;
;;; For now, even though ZL:MEMBER  SI:MEMBER-EQUAL &c., things are not rewritten that way.

;;; This gives up when :key arg (only legal for CL:MEMBER) is supplied and
;;; not IDENTITY, or when :test-not is supplied.
(defun optimize-cl-list-search (form)
  (macrolet ((get-arg (&rest get-args)
               `(get (cddr form) ,@get-args)))
    (if (or (get-arg :test-not)
            (not (function-eq (get-arg :key) 'identity 'identity)))
        form
      (let ((new-function
              (cdr (cli:assoc (get-arg :test ''eql)
                              (get (get (car form) 'list-search-family)
                                   'test-function-alist)
                              :test 'function-eq))))
        (if new-function
            `(,new-function ,(second form) ,(third form))
          form)))))

;;; For the three-letter versions
(defun optimize-zl-list-search (form)
  (let ((new-function (cdr (cli:assoc (second form)
                                      (get (get (car form) 'list-search-family)
                                           'test-function-alist)
                                      :test 'function-eq))))
    (if new-function
        `(,new-function ,(third form) ,(fourth form))
      form)))

(defmacro add-list-search-optimizers (cl-function zl-function test-function-alist)
  `(progn
     (defprop ,zl-function ,cl-function list-search-family)
     (defprop ,cl-function ,cl-function list-search-family)
     (defprop ,cl-function ,test-function-alist test-function-alist)
     (defoptimizer optimize-zl-list-search ,zl-function)
     (defoptimizer optimize-cl-list-search ,cl-function)))

(add-list-search-optimizers cl:member zl:mem
                            ((eq . memq) (eql . si:member-eql) (equal . si:member-equal)
                             (equalp . si:member-equalp)))

(add-list-search-optimizers cl:assoc zl:ass
                            ((eq . assq) (eql . si:assoc-eql) (equal . si:assoc-equal)
                             (equalp . si:assoc-equalp)))

(add-list-search-optimizers cl:rassoc zl:rass
                            ((eq . rassq) (eql . si:rassoc-eql) (equal . si:rassoc-equal)
                             (equalp . si:rassoc-equalp)))

;;; These should be done sometime.
;;; ZL:DEL DELQ
;;; ZL:REM REMQ

(defoptimizer optimize-lisp-gethash lisp:gethash (form)
  (if (eq *p1value* t) ;; Compiling for all values
      form
    ;; Use ZL version, which is a subst.
    (cons 'zl:gethash (cdr form))))

;;; Optimize (EQ (TYPEP ...) 'SYMBOL), etc.
;>> Why waste the effort?
;(defoptimizer eq-typep eq (form)
;  (and (not (atom (cadr form)))
;       (not (atom (caddr form)))
;       (cond ((and (eq (caadr form) 'typep)
;                  (null (cddadr form))  ;Check that TYPEP has only one arg!
;                  (eq (caaddr form) 'quote))
;             (return-from eq-typep (eq-typep-1 (cadadr form) (cadr (caddr form)) form)))
;            ((and (eq (caadr form) 'quote)
;                  (eq (caaddr form) 'typep)
;                  (null (cddr (caddr form))))
;             (return-from eq-typep (eq-typep-1 (cadr (caddr form)) (cadadr form) form)))))
;  form)

;(defun eq-typep-1 (object-form type original-form &aux pred)
;  (setq pred (or (car (rassq type '((stringp . string) (symbolp . symbol) (cl:listp . list)
;                                   (stringp . :string) (symbolp . :symbol) (consp . :list))))
;                (car (si:rassoc-equal type si::typep-one-arg-alist))
;                (car (si:rassoc-equal type si::type-of-alist))))
;  (cond ((null pred) original-form)
;       ((numberp pred) `(= (%data-type ,object-form) ,pred))
;       ((symbolp pred) `(,pred ,object-form))
;       (t original-form)))

;;; Open coding of TYPEP and COERCE.  Optimizers defined in SYS; TYPES.
(defoptimizer si::typep-two-args typep)
(defoptimizer si::coerce-optimizer coerce)

;; defined in SYS2; SELEV
(defoptimizer si::matchcarcdr-hack si::matchcarcdr)

;; Copied from LAD: RELEASE-3.SYS; QCOPT.LISP#170 on 2-Oct-86 05:05:40
;;; modify signp to be (AND (NUMBERP <form>) (<op> <form>)) if form is an atom
;;; and therefore can't have side effects
(defrewrite signp-expand signp (x)
  (or (= (length x) 3)
      (warn 'bad-signp ':impossible
            "SIGNP called with too ~:[few~;many~] arguments" (> (length x) 3)))
  (let ((operation (cadr x))
        (operand (caddr x))
        new-form notp)
    (cond ((atom operand)
           (setq new-form
                 `(,(cond ((string-equal operation 'e) 'zerop)
                          ((string-equal operation 'n) (setq notp t) 'zerop)
                          ((string-equal operation 'l) 'minusp)
                          ((string-equal operation 'ge) (setq notp t) 'minusp)
                          ((string-equal operation 'g) 'plusp)
                          ((string-equal operation 'le) (setq notp t) 'plusp)
                          (t (warn 'bad-signp ':impossible
                                   "~S called with invalid condition ~S" 'signp operation)
                             'progn))
                   ,operand))
           (if notp (setq new-form `(not ,new-form)))
           `(and (numberp ,operand) ,new-form))
          ('else
           ;; *MUST* open compile this to avoid call to gross interpreter special form.
           (let ((g (gentemp "operand")))
             `(let ((,g ,operand))
                (signp ,operation ,g)))))))

(defun simple-form-p (form)
  (or (atom form)               ; Cautious about quoted lists that might get bashed
      (and (eq (car-safe form) 'quote)
           (atom (second form)))))

(defsubst invariable-form-p (form)
  (constantp form))

(defoptimizer make-array-simple-make-array make-array (form)
  (let ((len (length form))
        (dimensions-form nil)
        (initial-value-form nil)
        (initial-value-specified nil)
        (area-form nil)
        ;;;||| This run-time target-K stuff generates a read error at compile
        ;;;time, unless you load the K software, which doesn't make sense when
        ;;;you're compiling for Lambda...???-Keith 9/24/88
        (type-form (if (eq *target-computer* 'k)
                       ;;art-q ;;WKF: 5/18/88
                       #+lambda art-q #+falcon art-q
                     ''art-q))
        (leader-length-form nil)
        (fill-pointer-form nil)
        (fill-pointer-specified nil)
        (named-structure-symbol-form nil)
        (named-structure-symbol-specified nil)
        out-of-order
        startform)
    (when (or (< len 2) (oddp len))
      (return-from make-array-simple-make-array form))
    (setq dimensions-form (second form))
    (loop for (keyword-form argument-form) on (rest2 form) by #'cddr
          do (case (if (eq (car-safe keyword-form) 'quote)
                          (cadr keyword-form)
                        keyword-form)
               (:type
                (setq type-form argument-form)
                (or (constantp type-form)
                    (and (constantp area-form)
                         (constantp leader-length-form)
                         (constantp initial-value-form)
                         (constantp fill-pointer-form)
                         (constantp named-structure-symbol-form))
                    (setq out-of-order t)))
               (:element-type
                (setq type-form argument-form)
                (or (constantp type-form)
                    (if (symbolp type-form)
                        (and (trivial-form-p area-form)
                             (trivial-form-p leader-length-form)
                             (trivial-form-p initial-value-form)
                             (trivial-form-p fill-pointer-form)
                             (trivial-form-p named-structure-symbol-form))
                      (and (constantp area-form)
                           (constantp leader-length-form)
                           (constantp initial-value-form)
                           (constantp fill-pointer-form)
                           (constantp named-structure-symbol-form)))
                    (setq out-of-order t))
                (setq type-form
                      (if (constantp type-form)
                          `',(si::array-type-from-element-type (eval type-form))
                          `  (si::array-type-from-element-type ,type-form))))
               (:area
                (setq area-form argument-form)
                (or (constantp area-form)
                    (if (symbolp area-form)
                        (and (trivial-form-p leader-length-form)
                             (trivial-form-p initial-value-form)
                             (trivial-form-p fill-pointer-form)
                             (trivial-form-p named-structure-symbol-form))
                      (and (constantp leader-length-form)
                           (constantp initial-value-form)
                           (constantp fill-pointer-form)
                           (constantp named-structure-symbol-form)))
                    (setq out-of-order t)))
               (:leader-length
                (setq leader-length-form argument-form)
                (or (constantp leader-length-form)
                    (if (symbolp leader-length-form)
                        (and (trivial-form-p initial-value-form)
                             (trivial-form-p fill-pointer-form)
                             (trivial-form-p named-structure-symbol-form))
                      (and (constantp initial-value-form)
                           (constantp fill-pointer-form)
                           (constantp named-structure-symbol-form)))
                    (setq out-of-order t)))
               ((:initial-value :initial-element)
                (setq initial-value-form argument-form initial-value-specified t)
                (or (constantp initial-value-form)
                    (if (symbolp initial-value-form)
                        (and (trivial-form-p fill-pointer-form)
                             (trivial-form-p named-structure-symbol-form))
                      (and (constantp fill-pointer-form)
                           (constantp named-structure-symbol-form)))
                    (setq out-of-order t)))
               (:fill-pointer
                (setq fill-pointer-form argument-form fill-pointer-specified t)
                (or (constantp fill-pointer-form)
                    (if (symbolp fill-pointer-form)
                        (trivial-form-p named-structure-symbol-form)
                      (constantp named-structure-symbol-form))
                    (setq out-of-order t)))
               (:named-structure-symbol
                (setq named-structure-symbol-form argument-form
                      named-structure-symbol-specified t))
               (otherwise
                (return-from make-array-simple-make-array form))))
    (if out-of-order
        ;; Don't optimize if it means exchanging two subforms
        ;; which could affect each other.
        form
      (if fill-pointer-specified
          (setq leader-length-form
                (if leader-length-form
                    `(max 1 ,leader-length-form)
                  1)))
      (setq startform
            (cond
              (initial-value-specified
               `(si:simple-make-array ,dimensions-form ,type-form ,area-form
                                      ,leader-length-form ,initial-value-form))
              (leader-length-form
               `(si:simple-make-array ,dimensions-form ,type-form ,area-form
                                      ,leader-length-form))
              (area-form
               `(si:simple-make-array ,dimensions-form ,type-form ,area-form))
              (t
               `(si:simple-make-array ,dimensions-form ,type-form))))
      (if (or fill-pointer-specified named-structure-symbol-specified)
          ;;Construct optimized form:
          (let ((array-var (gensym)))
            `(let ((,array-var ,startform))
               ,(if fill-pointer-specified
                    `(setf (fill-pointer ,array-var)
                           ;;Specifying :FILL-POINTER T was causing run-time
                           ;;error from SET-FILL-POINTER because a "new"
                           ;;fill-pointer must be a fixnum - but this isn't a
                           ;;new fill-pointer.  Solution is to use dimensions
                           ;;to init. fill-pointer as caller intended.  This
                           ;;works for all cases where fill-pointer is legal,
                           ;;i.e. one-dimensional arrays only.  The run-time
                           ;;error for combining a fill-pointer and multiple
                           ;;dimensions will be somewhat misleading, 'tho.
                           ;;|||Keith 9/24/88
                           ,(if (memq fill-pointer-form '(t 't)) dimensions-form fill-pointer-form)))
               ,(if named-structure-symbol-specified
                    `(make-array-into-named-structure
                       ,array-var ,named-structure-symbol-form))
               , array-var))
        startform))))

(defoptimizer make-string-simple-make-array make-string (form)
  (let* ((loss `(make-array ,(cadr form) :type art-string . ,(cddr form)))
         (loser (make-array-simple-make-array loss)))
    (if (eq loss loser) form loser)))

(defoptimizer simple-make-array-simple-make-array-1d-q-short si:simple-make-array (form)
  (let ((dims (second form))
        (type (third form)))
    (cond ((and (numberp dims)  ;DEFSTRUCT makes a lot of these!
                (< dims si:%array-max-short-index-length)
                (null (cdddr form))
                (if (eq *target-computer* 'k)  ;;wkf: 5/18/88 handle different art-q's
                    (or (and (numberp type)
                             (= type #+lambda art-q #+falcon art-q))
                        (equal type '(quote art-q))
                        (eq type 'art-q))
                  (or (equal type '(quote art-q))
                      (eq type 'art-q)
                      (and (numberp type)
                           (= type art-q)))))
           `(si:simple-make-array-1d-q-short ,(cadr form)))
          ((and (if (eq *target-computer* 'k)                  ;;wkf: 5/18/88
                    (or (equal type '(quote art-string))
                        (eq type 'art-string)
                        (and (numberp type)
                             (= type #+lambda art-string #+falcon art-string)))
                  (or (equal type '(quote art-string))
                      (eq type 'art-string)
                      (and (numberp type)
                           (= type art-string))))
                (null (fourth form))
                (eq (fifth form) 1)
                (null (nthcdr 5 form)))
           `(si:simple-make-array-1d-string-with-fill-pointer ,(cadr form)))
          (t form))))

(defoptimizer optimize-lisp-make-array cli:make-array (form)
  ;; The only difference of substance is the DISPLACED-TO option, so if it's not
  ;; there, go ahead and use ZL:MAKE-ARRAY, which itself gets optimised.
  (if (getf (cddr form) :displaced-to) ; Options
      form
    (let ((new (cons 'zl:make-array (cdr form))))
      ;; Only bother to be explicit about ONE value if that's what the compilation wants.
      (if (eq *p1value* t)
          `(values ,new)
        new))))

;;;|||ART-Q-LIST may be going away... warning is to find occurrences in system code. --Keith 9/88
(defun (:property make-array style-checker) (form &aux type)
  ;;Handle quoted and keyword forms of :TYPE :ART-Q-LIST
  (and (setq type (or (getf (cddr form) :type)
                      (cadr (member '(quote :type) (cddr form)))))
       (or (equal type '(quote art-q-list))
           (and (symbolp type) (string-equal type :art-q-list)))
       (warn 'art-q-list-warning :not-portable
         "~S called with :TYPE ART-Q-LIST (this may not be portable)" (car form))))

;;; This will actually come in pretty handy when LISP:MAP is open-coded for vectors.
(defoptimizer make-sequence-known-type make-sequence (form)
  (let ((type-form (second form)))
    (if (constantp type-form)
        (let ((type (eval type-form)))
          (cond ((eq type 'list)
                 `(make-list ,@(cddr form)))
                ((memq type '(string simple-string))
                 `(make-string ,@(cddr form)))
                ((ignore-errors  (si::compilation-subtypep type 'vector))
                 (let ((atype (si::type-canonicalize type nil nil)))
                   `(cli:make-array ,(third form)
                                    :element-type ',(if (eq (second atype) '*)
                                                        t
                                                      (second atype))
                                    ,@(cdddr form))))
                (t form)))
      form)))

(defoptimizer aref-expander zl:aref (form)
  (case (length form)
    (3 `(ar-1 . ,(cdr form)))
    (4 `(ar-2 . ,(cdr form)))   ;note that ar-2, ar-3 are common-lisp-aref!
    (5 `(ar-3 . ,(cdr form)))   ;ie return characters from strings.
    (t form)))

(defoptimizer common-lisp-aref-expander common-lisp-aref (form)
  (case (length form)
    (3 `(common-lisp-ar-1 . ,(cdr form)))
    (4 `(ar-2 . ,(cdr form)))
    (5 `(ar-3 . ,(cdr form)))
    (t form)))

(defoptimizer aset-expander aset (form)
  (case (length form)
    (4 `(as-1 . ,(cdr form)))
    (5 `(as-2 . ,(cdr form)))
    (6 `(as-3 . ,(cdr form)))
    (t form)))

(defoptimizer set-aref-expander set-aref (form)
  (case (length form)
    (4 `(set-ar-1 . ,(cdr form)))
    (5 `(set-ar-2 . ,(cdr form)))
    (6 `(set-ar-3 . ,(cdr form)))
    (t form)))

(defoptimizer aloc-expander aloc (form)
  (case (length form)
    (3 `(ap-1 . ,(cdr form)))
    (4 `(ap-2 . ,(cdr form)))
    (5 `(ap-3 . ,(cdr form)))
    (t form)))

;;; Find simple calls to MAKE-LIST and convert them into calls to the
;;; microcoded %MAKE-LIST.  NOTE THAT THIS CHANGES ORDER OF EVALUATION!
(defoptimizer make-list-%make-list make-list (form)
  (or (let ((length-of-form (length form)))
        (if (= length-of-form 3)
            ;; It is old-style.
            ;; There has been a style-checker against this for some time.
            ;; May be flushed for 105, probably
            `(%make-list 'nil ,(second form) ,(third form))
          ;; It is new-style.
          (if (evenp length-of-form)
              (let ((area-form nil) (initial-value-form nil)
                    (cdr-coded-form si:*make-list-cdr-code-default*))
                (do ((options (cddr form) (cddr options)))
                    ((null options)
                     `(,(if cdr-coded-form '%make-list 'si:make-list-with-cons)
                        ,initial-value-form ,area-form ,(second form)))
                  (let ((keyword-form (car options))
                        (value-form (cadr options)))
                    (if (eq (car-safe keyword-form) 'quote) (pop keyword-form))
                    (case keyword-form
                      (:area (setq area-form value-form))
                      ((:initial-value :initial-element)
                       (setq initial-value-form value-form))
                      (:cdr-coded
                       (case value-form
                         ((t nil) (setq cdr-coded-form value-form))
                         (otherwise (return nil))))
                      (otherwise (return nil)))))))))
      form))

(defrewrite status-optimizer status (form)
  (let ((status-function (cadr form))
        (item-p (cddr form)))
    (selector status-function string-equal
      (('feature 'features) (if item-p
                                (if (and (not (cdddr form))
                                         (symbolp (caddr form)))
                                    `(not (not (memq ',(intern (symbol-name (caddr form))
                                                               si:pkg-keyword-package)
                                                     *features*)))
                                  form)
                              `*features*))
      (('tabsize) `8)
      (('userid) `user-id)
      (('site) `si:local-host-name)
      (('opsys) `':lispm)
      (otherwise (or (mem #'string-equal status-function si::status-status-list)
                     (warn 'unknown-status-function :impossible "Unknown ~S function ~A."
                           'status status-function))
                 form))))

;;; Next two are here mainly to avoid getting an error message from
;;;  GETARGDESC about random FSUBR.
(defrewrite comment-expand declare)
(defrewrite comment-expand comment (ignore)
  `'comment)

(defrewrite defprop-expand defprop (x)
  `(putprop ',(cadr x) ',(caddr x) ',(cadddr x)))

;;; Optimizers for GETF and GET-PROPERTIES that use LOCF inline.

;;Locatives are losing brain damage because they don't work for arbitrary forms.
;;- smh 8aug88
;;
;;(defoptimizer getf-inline getf (form)
;;  `(get (locf ,(cadr form)) ,@(cddr form)))

(defoptimizer getf-inline getf (form)
  (if (symbolp (cadr form))
      `(get (locf ,(cadr form)) ,@(cddr form))
    form))

;;; SETPROPF might be used in GETF's SETF method --but not for now
(defoptimizer setpropf-use-loc si::setpropf (form) ; (SETPROPF place property value)
  (if (symbolp (cadr form))
      ;; A few shortcuts here -- already assuming SETPROP and VARIABLE-LOCATION
      `(si::setprop (variable-location ,(cadr form)) ,@(cddr form))
    form))

(defoptimizer get-properties-inline get-properties (form)
  `(si::get-properties-internal (locf ,(cadr form)) ,(caddr form)))

;;; use getf. what a crock.
;(defoptimizer gfal-get get-from-alternating-list (getf) (x)
;  (or (when (and (= (length x) 3)
;                (or (symbolp (cadr x))
;                    (and (consp (cadr x))
;                         (do ((form1 (cadr x)))
;                             (())
;                           (when (or (get (car form1) 'si:locf-method)
;                                     (get (car form1) 'si:setf-method))
;                             (return t))
;                           (when (eq form1 (setq form1 (macroexpand-1 form1)))
;                             (return nil))))))
;       `(get (locf ,(cadr x)) . ,(cddr x)))
;    x))


;;; Make PROGV work compiled.
(defrewrite progv-expand progv (form)
  (let ((varnames (cadr form)) (vals (caddr form)) (body (cdddr form))
        (vars-var (gensym))
        (vals-var (gensym)))
    `(prog ((,vars-var ,varnames) (,vals-var ,vals))
        loop
           (cond (,vars-var
                  (%bind (inhibit-style-warnings (value-cell-location (car ,vars-var)))
                         (car ,vals-var))
                  (unless ,vals-var
                    (makunbound (car ,vars-var)))
                  (setq ,vars-var (cdr ,vars-var))
                  (setq ,vals-var (cdr ,vals-var))
                  (go loop)))
           (return (progn . ,body)))))

;;;; Turn PROG1 into PROG2 since that is open-coded.
;;;; Also turn (PROG1 FOO NIL) into FOO since PBIND generates that and it makes better code
;(defoptimizer prog1-prog2 prog1 (form)
;  (if (equal (cddr form) '(nil))
;      (cadr form)
;    `(prog2 nil . ,(cdr form))))

(defrewrite progw-expand progw (form)
  (destructuring-bind (ignore vars-and-vals &body body) form
    (let ((vars-and-vals-var (gensym)))
      `(prog ((,vars-and-vals-var ,vars-and-vals))
          loop
             (cond (,vars-and-vals-var
                    (%bind (value-cell-location (caar ,vars-and-vals-var))
                          (eval (cadar ,vars-and-vals-var)))
                    (setq ,vars-and-vals-var (cdr ,vars-and-vals-var))
                    (go loop)))
             (return (progn . ,body))))))

(defun pbind (vars-and-vals loc)
  (when vars-and-vals
    `(%bind (,loc ,(caar vars-and-vals))
           (prog1 ,(cadar vars-and-vals)
                  ,(pbind (cdr vars-and-vals) loc)))))

(defrewrite let-if-expand let-if (form)
  (destructuring-bind (ignore cond vars-and-vals &body body) form
    (cond ((null cond) `(let () . ,body))               ;Macros generate this
          ((eq cond t) `(let ,vars-and-vals . ,body))   ;and this
          (t (multiple-value-bind (body decls)
                 (with-stack-list (env *function-environment*)
                   (extract-declarations body local-declarations nil env))
               `(let ()
                  (declare . ,decls)
                  (cond (,cond ,(pbind vars-and-vals 'variable-location)))
                  . ,body))))))

(defrewrite letf-expand letf (form)
  `(let ()
     ,(pbind (cadr form) 'locf)
     . ,(cddr form)))

(defrewrite letf*-expand letf* (form)
  `(let ()
     ,@(loop for (place value) in (cadr form)
          collect `(%bind (locf ,place) ,value))
     . ,(cddr form)))

(defrewrite letf-if-expand letf-if (form)
  (destructuring-bind (ignore cond vars-and-vals &body body) form
    (cond ((null cond) `(letf () . ,body))
          ((eq cond t) `(letf ,vars-and-vals . ,body))
          (t (multiple-value-bind (body decls)
                 (with-stack-list (env *function-environment*)
                   (extract-declarations body local-declarations nil env))
               `(let ()
                  (declare . ,decls)
                  (cond (,cond ,(pbind vars-and-vals 'locf)))
                  . ,body))))))

;; alas this conses...
(defrewrite multiple-value-call-expand multiple-value-call (form)
  (cond ((null (cdr form)) form)
        ((null (cddr form))
         `(funcall ,(cadr form)))
        ;; (multiple-value-call #'list stuff) => (multiple-value-list stuff)
        ((and (si:member-equal (cadr-safe form) '('list #'list))
              (not (fsymeval-in-function-environment 'list)))
         (if (null (cdddr form))
             `(multiple-value-list ,(caddr form))
           `(append . ,(mapcar (lambda (elt) `(multiple-value-list ,elt)) (cddr form)))))
        (t
         `(call ,(cadr form) . ,(mapcan (lambda (elt)
                                          `(:spread (multiple-value-list ,elt)))
                                        (cddr form))))))


;;; Turn (CONS X (CONS Y NIL)) into (LIST X Y).  Doesn't change (CONS X NIL), though.
;;; Perhaps we want a hairier criterion, for the sake of
;;; those times when you create a list you are going to RPLACA
;;; and don't want LIST to be used.
(defoptimizer cons-list cons (form)
  (cond ((atom (caddr form)) form)
        ((and (eq (caaddr form) 'cons)
              (not (fsymeval-in-function-environment 'cons)))
         (let ((tem (cons-list (caddr form))))
           (cond ((eq (car tem) 'list)
                  `(list ,(cadr form) . ,(cdr tem)))
                 ((si:member-equal (caddr tem) '(nil 'nil))
                  `(list ,(cadr form) ,(cadr tem)))
                 (t form))))
        (t
         form)))

(defoptimizer string-search-string-search-char string-search (form)
  (let ((key (second form)) quotep)
    (if (quotep key)
        (setq key (cadr key) quotep t))
    (if (or (and (or (stringp key)
                     (and quotep (symbolp key)))
                 (= (string-length key) 1))
            (typep key '(or character integer)))
        `(string-search-char ,(cl:character key) . ,(cddr form))
      form)))

;;;; Convert DOs into PROGs.
(defrewrite doexpander  do)
(defrewrite doexpander  do-named)
(defrewrite doexpander  do*)
(defrewrite doexpander  do*-named)

(defun doexpander (x)
  (let ((progname) (progrest) serial decls)
    (setq progrest
      (prog (dospecs endtest endvals tag1 tag3 pvars stepdvars once)
            (cond ((eq (car x) 'do-named)
                   (setq progname (cadr x))
                   (setq x (cddr x)))
                  ((eq (car x) 'do*-named)
                   (setq progname (cadr x))
                   (setq x (cddr x))
                   (setq serial t))
                  ((eq (car x) 'do*)
                   (setq x (cdr x))
                   (setq serial t))
                  (t (setq x (cdr x))))                 ;Get rid of "DO".
            (cond ((and (car x) (atom (car x)))
                   (setq  dospecs `((,(car x) ,(cadr x) ,(caddr x)))
                          endtest (car (setq x (cdddr x)))
                          endvals nil))
                  (t (setq dospecs (car x))
                     (setq x (cdr x))
                     (cond ((car x)
                            (setq endtest (caar x)
                                  endvals (and (or (cddar x)
                                                   (cadar x))
                                               (cdar x))))
                           (t (setq once t)))))
            (setq x (cdr x))
            (setq dospecs (reverse dospecs)); Do NOT use NREVERSE, or you will destroy
                                            ; every macro definition in sight!! -DLW
            ;; DOVARS has new-style list of DO variable specs,
            ;; ENDTEST has the end test form,
            ;; ENDVALS has the list of forms to be evaluated when the end test succeeds,
            ;; ONCE is T if this is a DO-once as in (DO ((VAR)) () ...),
            ;; X has the body.
            (setf (values x decls)
                  (extract-declarations-record-macros x))
            ;; Now process the variable specs.
            (do ((x dospecs (cdr x))) ((null x))
                (cond ((atom (car x))
                       (push (car x) pvars))
                      ((or (> (length (car x)) 3) (not (atom (caar x))))
                       (warn 'bad-binding-list ':impossible
                             "Malformatted DO-variable specification ~S"
                             (car x)))
                      (t (push `(,(caar x) ,(cadar x)) pvars)
                         (and (cddar x)
                              (push `(,(caar x) ,(caddar x)) stepdvars)))))
            (when once
              (and stepdvars
                   (warn 'bad-do ':implausible
                         "A once-only DO contains variables to be stepped: ~S."
                         stepdvars))
              (return `(,pvars . ,x)))
            ;; Turn STEPDVARS into a PSETQ form to step the vars,
            ;; or into NIL if there are no vars to be stepped.
            (setq stepdvars (apply #'nconc stepdvars))
            (and stepdvars (setq stepdvars (cons (if serial 'setq 'psetq) stepdvars)))
            (setq tag3 (gensym))
            (setq tag1 (gensym))
            (let ((*p1value* 'predicate))
              (setq endtest (compiler-optimize endtest)))
            (cond ((null endtest)
                   ;>> I don't understand this -- we just did this above!
                   ;(compiler-optimize endtest) ;Get any style warnings we were supposed to get,
                                        ;since ENDTEST won't actually be compiled.
                   (and endvals
                        (warn 'bad-do ':impossible
                              "The end-test of a DO is NIL, but it says to evaluate ~S on exit."
                              endvals))
                   (return `(,pvars ,tag1
                             ,@x
                             ,stepdvars
                             (go ,tag1)))))
            (setq endvals `(return-from ,progname (progn nil . ,endvals)))
            (return `(,pvars
                      (go ,tag3)
                      ,tag1
                      ,@x       ;body
                      ,stepdvars
                      ,tag3
                      (or ,endtest (go ,tag1))
                      ,endvals))))
    (and progname (setq progrest (cons progname progrest)))
    (if decls
        `(,(if serial 'prog* 'prog)
          ,(car progrest)
          ,.(mapcar (lambda (d) `(declare ,d)) decls)
          . ,(cdr progrest))
        `(,(if serial 'prog* 'prog)
          . ,progrest))))

(defoptimizer mapexpand mapl)
(defoptimizer mapexpand mapc)
(defoptimizer mapexpand mapcar)
(defoptimizer mapexpand maplist)
(defoptimizer mapexpand mapcan)
(defoptimizer mapexpand mapcon)

(defun mapexpand (form)
  (if (or (null (cddr form))            ;Don't bomb out if no args for the function to map.
          (not open-code-map-switch))
      form
    (let ((fn (cadr form))
          (take-cars (memq (car form) '(mapc mapcar mapcan)))
          (circular-loss (fsymeval-in-function-environment 'circular-list)))
      ;; Expand maps only if specified function is a quoted LAMBDA or a SUBST,
      ;; or some arg is a call to CIRCULAR-LIST and we are mapping on cars.
      ;; or OPEN-CODE-MAP-SWITCH is set to :ALWAYS.
      (cond ((list-match-p fn `(quote (lambda . ,ignore)))
             (warn 'obsolete :obsolete
                   "The~ form (~S '(~S ...)) appears~@
                        The /"'/" almost certainly should be replaced by /"#'/".~@
                        The code as written will not work evaluated if the form~@
                        to be applied contains a dynamic reference to a non-special~@
                        variable.~"
                   (car form) 'lambda)
             ;; don't get this warning again
             `(dont-optimize ,form))
            ((not (or (eq open-code-map-switch ':always)
                      (eq (car-safe fn) 'lambda)
                      ;; You deserve to lose
                      ;(and (eq (car-safe fn) 'quote)
                      ;     (consp (cadr fn)))
                      ;(and (eq (car-safe fn) 'function)
                      ;     (atom (cadr fn))
                      ;     (neq (car-safe (declared-definition (cadr fn)))
                      ;     'macro))
                      (and take-cars
                           (not circular-loss)
                           (loop for x in (cddr form)
                                 thereis (and (eq (car-safe x) 'circular-list)
                                              (null (cddr x)))))
                      t))
             form)
            (t
             ;; VARNMS gets a list of gensymmed variables to use to hold
             ;; the tails of the lists we are mapping down.
             (let ((call-fn (cond ((eq (car-safe fn) 'lambda) (list fn))
                                  ((eq (car-safe fn) 'function) (list (cadr fn)))
                                  (t (list 'funcall fn))))
                   (varnms) (doclauses) (endtest) (cars-or-tails) (tem))
               ;; DOCLAUSES looks like ((#:G0001 expression (CDR #:G0001)) ...)
               ;;  repeated for each variable.
               ;; ENDTEST is (OR (NULL #:G0001) (NULL #:G0002) ...)
               ;; CARS-OR-TAILS is what to pass to the specified function:
               ;;  either (#:G0001 #:G0002 ...) or ((CAR #:G0001) (CAR #:G0002) ...)
               (setq varnms (do ((l (cddr form) (cdr l)) (output) )
                                ((null l) output)
                              (push (gensym) output)))
               (setq doclauses
                     (mapcar (lambda (v l)
                               (cond ((and take-cars
                                           (not (atom l))
                                           (eq (car l) 'circular-list)
                                           (null (cddr l)))
                                      `(,v ,(cadr l)))
                                     (t `(,v ,l (cdr ,v)))))
                             varnms (cddr form)))
               (setq endtest
                     (cons 'or (mapcan (lambda (vl)
                                         (and (cddr vl) `((null ,(car vl)))))
                                       doclauses)))
               (setq cars-or-tails
                     (cond (take-cars
                            (mapcar (lambda (dc)
                                      (cond ((cddr dc) `(car ,(car dc)))
                                            (t (car dc))))
                                    doclauses))
                           (t varnms)))
               (cond ((memq (car form) '(mapl mapc))    ;No result
                      (setq tem `(inhibit-style-warnings
                                   (do-named t ,doclauses
                                             (,endtest)
                                     (,@call-fn . ,cars-or-tails))))
                      ;; Special hack for MAPL or MAPC for value:
                      ;; Bind an extra local to 2nd list and return that.
                      (if *p1value*
                          `(let ((map-result ,(prog1 (cadar doclauses)
                                                     (rplaca (cdar doclauses)
                                                             'map-result))))
                             ,tem
                             map-result)
                        tem))
                     ((memq (car form) '(mapcar maplist))
                      ;; Cons up result
                      (let ((map-result (gensym))
                            (map-temp (gensym)))
                        `(let ((,map-result))
                           (inhibit-style-warnings
                             (do-named t ((,map-temp (inhibit-style-warnings
                                                       (variable-location ,map-result)))
                                          . ,doclauses)
                                       (,endtest)
                               (rplacd ,map-temp
                                       (setq ,map-temp
                                             (ncons (,@call-fn . ,cars-or-tails))))))
                           ,map-result)))
                     (t
                      ;; MAPCAN and MAPCON:  NCONC the result.
                      (let ((map-tem (gensym))
                            (map-result (gensym)))
                        `(inhibit-style-warnings
                           (do-named t (,@doclauses (,map-tem) (,map-result))
                                     (,endtest ,map-result)
                             (setq ,map-tem (nconc ,map-tem (,@call-fn . ,cars-or-tails)))
                             (or ,map-result (setq ,map-result ,map-tem))
                             (setq ,map-tem (last ,map-tem)))))))
               ))))))

(add-typed-optimizer cli:map list-lisp-map->mapcar
                     ((result-type (:type list)) function &rest (sequences list))
  `(mapcar ,function ,@sequences))

(add-typed-optimizer cli:map list-lisp-map->mapc
                     ((result-type (member nil)) function &rest (sequences list))
  `(progn (mapc ,function ,@sequences) nil))

;;; >> Need optimizers for LISP:MAP with vector argument or result.
;;; >> Could also open-code LISP:MAP in the general case, but is it worth ?


(defoptimizer subset-expand subset)
(defoptimizer subset-expand subset-not)
(defun subset-expand (form)
  (let ((fn (cadr form))
        predargs doclauses map-result map-temp)
    (cond ((not open-code-map-switch) form)
          ;; Expand only if specified function is a quoted LAMBDA or a SUBST,
          ((not (or (eq (car-safe fn) 'lambda)
                    ;; You deserve to lose
                    ;(and (eq (car-safe fn) 'quote)
                    ;     (consp (cadr fn)))
                    ;(and (eq (car-safe fn) 'function)
                    ;     (atom (cadr fn))
                    ;     (neq (car-safe (declared-definition (cadr fn)))
                    ;          'macro)))
                    ))
           form)
          (t
           ;(if (eq (car fn) 'function) (setq fn (cadr fn)))
           ;; Generate N local variable names.
           (do ((l (cddr form) (cdr l)) (i 0 (1+ i)))
               ((null l))
             (let ((v (gensym)))
               (push `(,v ,(car l) (cdr ,v)) doclauses)
               (push `(car ,v) predargs)))
           (setq doclauses (nreverse doclauses)
                 predargs (nreverse predargs))
           (setq map-result (gensym)
                 map-temp (gensym))
           `(let (,map-result)
              (inhibit-style-warnings
                (do-named t
                          ((,map-temp (inhibit-style-warnings (variable-location ,map-result)))
                           . ,doclauses)
                          ((null ,(caar doclauses)))    ;Stop when first local variable runs out
                  (,(cond ((eq (car form) 'subset) 'and) (t 'or))
                   (,fn . ,predargs)
                   (rplacd ,map-temp (setq ,map-temp (ncons ,(car predargs)))))))
              ,map-result)))))

(define-typed-optimizer use-zl-list-predicate-checker (&whole form predicate (sequence list) &rest)
  `(,(cdr (assq (car form) '((cli:every . zl:every) (cli:some . zl:some))))
    ,sequence ,predicate))

(add-typed-optimizer cli:every use-zl-list-predicate-checker)
(add-typed-optimizer cli:some use-zl-list-predicate-checker)

(defun fix-synonym-special-form (form)
  (cons (let ((original (si::interpreter-special-form (car form))))
          (if (null original) ;; not really a special form ??  Well...
              (function-name (symbol-function (car form)))
            (si::interpreter-special-form-name original)))
        (cdr form)))

;;; These functions are defined in ENCAPS, but loaded here
(defrewrite fix-synonym-special-form si:encapsulation-let)
(defrewrite fix-synonym-special-form si:encapsulation-list*)

(defrewrite fix-synonym-special-form si::advise-prog)
(defrewrite fix-synonym-special-form si::advise-setq)
(defrewrite fix-synonym-special-form si::advise-progn)
(defrewrite fix-synonym-special-form si::advise-multiple-value-list)
(defrewrite fix-synonym-special-form si::advise-return-list)
(defrewrite fix-synonym-special-form si::advise-apply)
(defrewrite fix-synonym-special-form si::advise-let)
(defrewrite fix-synonym-special-form si::advise-list*)

(defoptimizer time-no-args time (x)
  (if (second x) ; Given an argument
      x
    '(si::time-in-60ths)))

;;; Style checkers are run before optimizers and, in effect, both before
;;; and after macro expansion.  They do not rewrite their input (the
;;; form to style-check), merely print warnings if there is a style
;;; violation within it.  They are expected to return NIL, indicating
;;; that no warning was issued, or to return the two values from the
;;; call to COMPILER:WARN that actually issues the desired warning.
;;;
;;; Style checkers are generally used for: 1) warning about illegal
;;; arguments that the compiler, for any reason, cannot detect; and 2)
;;; anything else that is ugly or frowned upon, though legal.

;;; Currently, there can only be one style checker invoked per form
;;; name; this may be changed.
;;;
;;; See QCP1 for the actual implementation of style-checking.
;;; COMPILER:INVOKE-STYLE-CHECKER is invoked by COMPILER-OPTIMIZE.
;;;
;;; Some style checkers below -- old, obsolete stuff -- were
;;; commented out when the MACLISP switches and style checkers were
;;; deimplemented some time ago.   They are still in this file, I
;;; suppose so they could be added back in if desired.
;;;
;;;      -Keith 9/88

(defun obsolete (form)
  (and ;obsolete-function-warning-switch
       ;(not run-in-maclisp-switch)
       (warn 'obsolete ':obsolete
             "~S ~A."
             (car form)
             (or (get (car form) 'obsolete)
                 "is an obsolete function"))))

;;;MAKE-OBSOLETE was moved to QRAND some time ago.

(defun unimplemented (form)
  (warn 'unimplemented ':implementation-limit
        "~S is not implemented in Zetalisp"
        (car form)))

(make-obsolete getchar "use strings")
(make-obsolete getcharn "use strings")
(make-obsolete implode "use strings")
(make-obsolete maknam "use strings")
(make-obsolete explode "use strings")
(make-obsolete explodec "use strings")
(make-obsolete exploden "use strings")
(make-obsolete samepnamep "use strings")
;(make-obsolete si:process-run-temporary-function "PROCESS-RUN-FUNCTION is identical.")
(make-obsolete fs:file-read-property-list "the new name is FS:READ-ATTRIBUTE-LIST")
(make-obsolete fs:file-property-list "the new name is FS:FILE-ATTRIBUTE-LIST")
(make-obsolete fs:file-property-bindings "the new name is FS:FILE-ATTRIBUTE-BINDINGS")

;;;>>>What did this person intend to say?
;;; I guess these have to be on SYSTEM for this to work.
;(defprop maknum unimplemented style-checker)
;(defprop munkam unimplemented style-checker)
;(defprop *rearray unimplemented style-checker)
;(defprop *function unimplemented style-checker)
;(defprop subrcall unimplemented style-checker)
;(defprop lsubrcall unimplemented style-checker)
;(defprop pnget unimplemented style-checker)
;(defprop pnput unimplemented style-checker)
;(defprop fsc unimplemented style-checker)

(defprop prog1 need-two-args style-checker)
(defprop prog2 need-two-args style-checker)
(defprop + need-two-args style-checker)
(defprop * need-two-args style-checker)
(defprop plus need-two-args style-checker)
(defprop times need-two-args style-checker)
(defprop quotient need-two-args style-checker)
(defprop difference need-two-args style-checker)
(defprop nconc need-two-args style-checker)

(defun need-two-args (form)
  (when (null (cddr form))
    (warn 'wrong-number-of-arguments :implausible
          "~S called with fewer than two arguments" (car form))))

;;;Functions defined with (DECLARE(ARGLIST)) could be called with
;;;incorrect syntax; we may use the human-readable ARGLIST to check:

(defun check-arglist-with-arglist (form &optional (sym (first form)))
  (declare (arglist form &optional sym))
  (check-number-of-args form `(lambda ,(arglist sym nil))))

(defun (:property append style-checker) (form)
  (need-two-args form)
  (when (and (= (length form) 3)
             (si:member-equal (third form) '(nil 'nil)))
     (warn 'obsolete :obsolete
           "(~S ... NIL) ~is an obsolete way to copy lists.~@
                    Instead of: (~{~S~^ ~})~@
                    Use:        (~S ~S)~"
           'append form
           'copy-list (cadr form))))

(defun (:property zl:subst style-checker) (form)
  (when (and (= (length form) 4)
             (si:member-equal (cadr form) '(nil 'nil))
             (si:member-equal (caddr form) '(nil 'nil)))
    (warn 'obsolete :obsolete
          "(~S NIL NIL ...) ~is an obsolete way to copy trees.~@
                    Instead of: (~{~S~^ ~})~@
                    Use:        (~S ~S)~"
          'subst form
          'copy-tree (cadddr form))))

(defun (:property make-list style-checker) (form)
  (if (= (length form) 3)
      (warn 'obsolete :obsolete
        "~S ~called with obsolete calling sequence: ~S~&~
                Use (~S ~S~{ ~S~}) instead.~"
        'make-list form 'make-list (third form)
        (if (si:member-equal (second form) '(nil 'nil)) () `(:area ,(second form))))
    (check-arglist-with-arglist form 'make-list)))

(defun (:property typep style-checker) (form)
  (if (= (length form) 2)
      (warn 'obsolete :obsolete
            "~S ~called with one argument; this format is obsolete.~&~
                  Either call ~S with two arguments or call ~S.~"
            'typep 'typep 'type-of)))

(defun (:property string-equal style-checker) (form)
  ;; Old style: (string-equal s1 s2 &optional start1 start2 end1 end2)
  ;; New style: (string-equal s1 s2 &key start1 end1 start2 end2)
  ;; Arglist seen by compiler: (string-equal string1 string2 &rest args)
  (let ((length (- (length form) 3))
        keys)
    (tagbody
        (cond (( length 0)
               (go done))
              ((> length 4)
               (go &key))
          ((cl:some (lambda (x)
                      (or (keywordp x)
                          (and (eq (car-safe x) 'quote) (keywordp (cadr x)))))
                    (setq keys (cdddr form)))
           (go &key))
          ((or (numberp (car keys))
               (numberp (caddr keys)))
           (go obsolete))
          ((or (self-evaluating-p (car keys))
               (and (cddr keys)
                    (self-evaluating-p (caddr keys))))
           (go &key))
          ((oddp length)
           (go obsolete))
          (t
           ;; cannot tell
           ;;  (either (string-equal s1 s2 foo bar) or (string-equal s1 s2 foo bar baz zap))
           ;; since foo and baz may be keywords when the function is called.
           (go maybe-obsolete)))
     obsolete
        (warn 'obsolete :obsolete
              ;; the ~{~S~} stuff is to avoid *print-length* screws
              ;;  in si::record-and-print-warning
              "~S ~called in an obsolete fashion, with optional (non-keyword) arguments.~@
                    Instead of: (~{~S~^ ~})~@
                    Use:        (~{~S~^ ~})~"
              'string-equal form
              `(string-equal ,(cadr form) ,(caddr form)
                             ,@(loop for x in keys
                                     for y in '(:start1 :start2 :end1 :end2)
                                  collect y collect x)))
        (go done)
     maybe-obsolete
        (warn 'foo :warning
              "~S ~called in what may be an obsolete fashion: ~S -~@
                If ~:[~S is a keyword~*~;~S and ~S are keywords~] ~
                  when ~S is called, then this form is correct,~%  ~
                  and agrees with ~:*~S's new arglist:~%  (~{~A~^ ~})~@
                If not, use (~{~S~^ ~}) instead.~"
              'string-equal form
              (cddr keys) (car keys) (caddr keys)
              'string-equal '(string1 string2 &key start1 end1 start2 end2)
              `(string-equal ,(cadr form) ,(caddr form)
                             ,@(loop for x in keys
                                     for y in '(:start1 :start2 :end1 :end2)
                                  collect y collect x)))
        (go done)
     &key
        (check-arglist-with-arglist form 'string-equal)
        (go done)
     done
        )))


;;;Another general case of argument #

(defprop setq  need-an-arg style-checker)
(defprop psetq need-an-arg style-checker)
(defprop cond  need-an-arg style-checker)
(defprop -     need-an-arg style-checker)
(defprop //    need-an-arg style-checker)
(defprop cl:// need-an-arg style-checker)

(defun need-an-arg (form)
  (or (cdr form)
      (warn 'wrong-number-of-arguments ':implausible
            "~S called with no arguments" (car form))))

(defun (:property format style-checker) (form)
  ;;This gets caught by compiler
  ;;(need-two-args form)
  ;;>> It would be -kind- of nice to parse the format string for syntactic illegality...
  (if (typep (cadr form) '(or string number array))
      (warn 'bad-argument ':implausible
        "~S ~called with ~S as its first argument,~&~
 which should be ~S, ~S, a stream, or a string with fill-pointer~"
        'format (cadr form) t nil)))

;>> "(locf (symbol-value '*foo*))" seems a reasonable thing to me.
;(defrewrite value-cell-location-quoted-lossage value-cell-location (form)
;  ;(not-maclisp form)
;  (if (neq (car-safe (cadr form)) 'quote)
;      form
;    (warn 'value-cell-location ':obsolete
;         "~S of quoted variable ~S is obsolete; use ~S"
;         'value-cell-location (cadr (cadr form)) 'variable-location)
;    `(variable-location ,(cadr (cadr form)))))

;(defun (boundp style-checker) (form)
;  (and (consp (cadr form))
;       (eq (caadr form) 'quote)
;       (not (specialp (cadadr form)))
;       (warn 'boundp ':obsolete
;            "BOUNDP of a quoted nonspecial variable is obsolete; use VARIABLE-BOUNDP")))

;;;; Style-checkers for things that don't work in Maclisp.

;;; These symbols don't exist in Maclisp, though they could, but they are likely losers.
;(defprop zl:listp not-maclisp style-checker)
;(defprop zl:nlistp not-maclisp style-checker)
;(defprop nsymbolp not-maclisp style-checker)

;;; these functions can't be added to maclisp by a user.
;(defprop intern-local not-maclisp style-checker)
;(defprop intern-soft not-maclisp style-checker)
;(defprop intern-local-soft not-maclisp style-checker)
;(defprop make-array not-maclisp style-checker)
;(defprop g-l-p not-maclisp style-checker)
;(defprop array-leader not-maclisp style-checker)
;(defprop store-array-leader not-maclisp style-checker)
;(defprop multiple-value not-maclisp style-checker)
;(defprop multiple-value-list not-maclisp style-checker)
;(defprop do-named not-maclisp style-checker)
;(defprop return-from not-maclisp style-checker)
;(defprop return-list not-maclisp style-checker)
;(defprop bind not-maclisp style-checker)
;(defprop %bind not-maclisp style-checker)
;(defprop compiler-let not-maclisp style-checker)
;(defprop local-declare not-maclisp style-checker)
;(defprop cons-in-area not-maclisp style-checker)
;(defprop list-in-area not-maclisp style-checker)
;(defprop ncons-in-area not-maclisp style-checker)
;(defprop variable-location not-maclisp style-checker)
;(defprop variable-boundp not-maclisp style-checker)
;(defprop car-location not-maclisp style-checker)
;(defprop property-cell-location not-maclisp style-checker)
;(defprop function-cell-location not-maclisp style-checker)
;(defprop fset not-maclisp style-checker)
;(defprop fboundp not-maclisp style-checker)
;(defprop fsymeval not-maclisp style-checker)
;(defprop closure not-maclisp style-checker)

;(defun not-maclisp (form)
;  (and run-in-maclisp-switch
;       (warn 'not-in-maclisp ':maclisp
;            "~S is not implemented in Maclisp." (car form))))

;Nobody is planning to make (BREAK 'FOO) stop working, were they?
;(defrewrite break-decruftify break (form)
;  (if (not (and (symbolp (cadr-safe form))
;               (not (constantp (cadr-safe form)))))
;      form
;    (warn 'break-arg ':obsolete
;         "A symbol as the first argument to ~S is an obsolete construct;
;change it to a string before it stops working: ~S" 'break form)
;    `(break ',(cadr form))))

;;; Return with more than one argument won't work in Maclisp.
;(defprop return return-style style-checker)
;(defun return-style (form)
;  (and run-in-maclisp-switch
;       (cddr form)
;       (warn 'not-in-maclisp ':maclisp
;            "Returning multiple values doesn't work in Maclisp")))

;;; Named PROGs don't work in Maclisp.  PROG variables can't be initialized.
;;; Also, lots of tags and things like a GO to a RETURN are ugly.
;(defprop prog prog-style style-checker)
;(defun prog-style (form)
;  (prog (progname)
;       (and (atom (cadr form))
;            (cadr form)
;            (progn (setq progname (cadr form))
;                   (setq form (cdr form))))
;       (cond (run-in-maclisp-switch
;              (and progname (neq progname t)
;                   (warn 'not-in-maclisp ':maclisp
;                         "The PROG name ~S is used; PROG names won't work in Maclisp."
;                         progname))
;              (dolist (var (cadr form))
;                (or (atom var)
;                    (return
;                      (warn 'not-in-maclisp ':maclisp
;                            "The PROG variable ~S is initialized; this won't work in Maclisp."
;                            (car var)))))))))

;;; Check a LAMBDA for things that aren't allowed in Maclisp.
;;; Called only if RUN-IN-MACLISP-SWITCH is set.
;(defun lambda-style (lambda-exp)
;  (do ((varlist (cadr lambda-exp) (cdr varlist)) (kwdbarf)) ((null varlist))
;    (cond ((atom (car varlist))
;          (and (not kwdbarf)
;               (memq (car varlist) lambda-list-keywords)
;               (setq kwdbarf t)
;               (warn 'not-in-maclisp ':maclisp
;                     "Lambda-list keywords such as ~S don't work in Maclisp."
;                     (car varlist))))
;         (t (warn 'not-in-maclisp ':maclisp
;                  "The lambda-variable ~S is initialized; this won't work in Maclisp."
;                  (caar varlist))))))


(defun *lexpr (&quote &rest l)
  "Declares each symbol in L to be the name of a function.  In
addition it prevents these functions from appearing in the list of
functions referenced but not defined, printed at the end of
compilation."
  (dolist (x l)
    (compilation-define x)
    (putprop x '((#o1005 (fef-arg-opt fef-qt-eval)))
             'argdesc)))

(defun *expr (&quote &rest l)
  "Declares each symbol in L to be the name of a function.  In
addition it prevents these functions from appearing in the list of
functions referenced but not defined, printed at the end of
compilation."
  (dolist (x l)
    (compilation-define x)
    (putprop x '((#o1005 (fef-arg-opt fef-qt-eval)))
             'argdesc)))

(defun *fexpr (&quote &rest l)
  "Declares each symbol in L to be the name of a special form.  In
addition it prevents these names from appearing in the list of
functions referenced but not defined, printed at the end of
compilation."
  (dolist (x l)
    (compilation-define x)
    (putprop x '((#o1005 (fef-arg-opt fef-qt-qt)))
             'argdesc)))

;;;DEFSTRUCT style checks

(defconstant *defstruct-style-warning* 'defstruct-style-warning)

(defvar *defstruct-check-name* nil)

(defmacro defstruct-check-warn (severity msg &rest args)
  (declare (zwei:indentation 1 1))
  `(warn *defstruct-style-warning* ,severity
     (format nil "DEFSTRUCT~@[ ~A~]: ~A" *defstruct-check-name* ,msg)
     ,@args))

(defun defstruct-check-name (form &optional (warn t))
  (let ((name (second form)))
    (if (consp name)
        (setq name (first name)))
    (cond
      ((symbolp name) (setq *defstruct-check-name* name) nil)
      ((null warn) nil)
      (t (defstruct-check-warn nil :implausible "Invalid name ~S" name)))))

(defun defstruct-check-no-byte-fields (form)
  (let ((slots (cddr form)))
    (dolist (slot slots)
      (when (and (consp slot) (consp (car slot)))               ;probably a byte field
        (return (defstruct-check-warn :not-portable
                  "contains subslot fields (this may not be portable)"))))))

(defun defstruct-check-valid-byte-fields (form)
  (let ((slots (cddr form)))
    (dolist (slot slots)
      (when (and (consp slot) (consp (car slot)))       ;probably a byte field
        (let (whole-word-slot)
          (dolist (subslot slot)
            (and (consp subslot)
                 (cond
                   ((second subslot)
                    (let ((flag (check-byte-spec subslot nil nil)))
                      (when flag
                        (return (defstruct-check-warn :not-portable
                                  "invalid byte spec -- ~A" flag)))))
                   (whole-word-slot
                    (return (defstruct-check-warn :implausible
                              "more than one whole-word byte field slot specified")))
                   (t (setq whole-word-slot t))))))))))

(defun defstruct-style-checker (form)
  (let ((commonlisp-p (eq (car form) 'lisp:defstruct))
        (*defstruct-check-name* nil))
    ;;;OR through cases so multiple values get passed
    (or (defstruct-check-name form)
        (if commonlisp-p (defstruct-check-no-byte-fields form))
        (defstruct-check-valid-byte-fields form))))

(putprop 'lisp:defstruct 'defstruct-style-checker 'style-checker)
(putprop 'zl:defstruct 'defstruct-style-checker 'style-checker)


;;;Miscellaneous things we're frowning at

(defmacro discourage-feature (thing &optional instead)
  `(defun (:property ,thing style-checker) (form)
     (warn 'feature-is-not-portable :not-portable
       ,(string-append
          "~~S should be avoided; it is not portable."
          (typecase instead
            (symbol (format nil " Consider using ~S instead." instead))
            (string (string-append  " " instead))
            (t ""))
          "~")
       (car form))))

(discourage-feature zl:call apply)
(discourage-feature zl:g-l-p "Try to find an alternative to ART-Q-LIST arrays.")

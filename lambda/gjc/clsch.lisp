;;; -*- Mode:LISP; Package:(CLSCH LISP); Syntax:COMMON-LISP; Base:10 -*-

;;; An implementation of Scheme, less continuations, in Common Lisp.

;;; ***** Initial version - not fully debugged *****

;;; The dialect here is a union of the features needed for
;;;  (a) the examples in Abelson & Sussman's book "Structure and
;;;      Interpretation of Computer Programs"
;;;  (b) the problem sets for the 6.001 course
;;;  (c) the "essential" subset of the Revised Revised Report on Scheme
;;;      Scheme

;;; The only thing from (a) that's missing is MAKE-ENVIRONMENT.
;;; The only thing from (c) that's missing is CALL-WITH-CURRENT-CONTINUATION.
;;; Some things from (b) might be missing; I haven't checked thoroughly.

;;; A small number of important nonstandard features are present also:
;;; RANDOM, tables, "named LET", FLUID-LET, and DEFINE-MACRO.

;;; Implementing MAKE-ENVIRONMENT and CALL-WITH-CURRENT-CONTINUATION
;;; would be a major headache.  Fortunately, Scheme is still fairly
;;; useful without them.

;;; *** RECENT CHANGES ***
;;; Version 156 (11/22/85)
;;;  - JDR's latest set of fixes
;;; Version 154 (11/21/85)
;;;  - Flushed idea of exporting things in Scheme package.
;;;  - Translator passes an ENV argument around (doesn't use it yet).
;;; Version 149 (11/16/85)
;;;  - Winning quasiquote macro.
;;;  - Scheme's very own printer.
;;; Version 146 (11/12/85)
;;;  - Eric Benson's colon read macro.
;;;  - Fixed a bug in WRITE.
;;;  - John Ramsdell's meta-. hacks.
;;; Version 134 (11/10/85)
;;;  - Entry function S renamed to be SCHEME.
;;;  - Object file extension is now SBIN istead of SFASL.
;;;  - Improvements in package structure; SCHEME now inherits from CLSCH.
;;;     E.g., Scheme's QUOTE is now the same as Lisp's.
;;;  - Definition of a DEFINE macro in the SCHEME package, so that it
;;;     will now sort of work to use Common Lisp LOAD and EVAL and editor
;;;     commands to cause Scheme values to get defined.
;;;  - No more call to MACROEXPAND (avoid Symbolics DEFSUBST screw).
;;;  - Elimination of REC in DEFINE expansion (for speed and debugging).
;;;  - Some nonstandard features: tables, FLUID-LET, and DEFINE-MACRO.
;;;  - Scheme DEFINE also sets the function cell.
;;;  - Trivial definition of NUMBER->STRING.

;;; A minor flaw:
;;;  (define (foo x) y) is the same as
;;;  (define foo (lambda (x) y)), NOT
;;;  (define foo (rec foo (lambda (x) y))), as the RRRS would dictate.
;;; Fortunately, this delinquency leads to faster execution, and makes
;;; TRACE work besides.

;;; To do:
;;;  - It would be nice if there was an evaluator.  Code runs impossibly
;;;    slow if it's not compiled; and debugging is a pain.
;;;  - CAR and CDR of () should be error.
;;;  - Error reporting is bad; e.g., no procedure names.
;;;  - Need equivalent of GRINDEF.
;;;  - Want optimization of LETREC.
;;;  - Variants of the compiler: (1) real continuations, (2) CL's which
;;;    already have tail recursion, (3) CL's which can kludge tail recursion
;;;    (e.g. 3600's)

;;; Thanks to John Ramsdell of Mitre and Eric Benson of Lucid for various
;;; contributions.

;;; Jonathan Rees  Aug-Sept 1985

(in-package 'clsch)                             ;Common Lisp SCHeme

(shadow '(help lambda))

(export '(scheme help))

(defvar help

"How to use Scheme:
 - To start a scheme read-eval-print loop, evaluate (CLSCH:SCHEME).
 - To exit scheme once in it, do (EXIT).
 - To get back into the scheme read-eval-print loop, e.g. when an
   error occurs, do (RESET).
 - Use MIT AI memo 848, the Revised Revised Report on Scheme, as a
   reference manual.  All features marked 'essential' are here,
   along with many of the non-essential features.
 - CALL-WITH-CURRENT-CONTINUATION exists, but continuations will have
   indefinite extent only if the host Common Lisp's BLOCK tags do.
 - Additional features used in S&ICP are also defined, with the
   exception of COLLECT and MAKE-ENVIRONMENT.
 - The expression ## evaluates to the value of the read-eval-print
   loop's last evaluation.
 - To compile a scheme file, use scheme's COMPILE-FILE procedure.")

;;; Utility for avoiding compiler warnings
(defmacro ignorable (&rest vars)
  `(progn ,@vars))

;;;----- Scheme-like macros for Common Lisp code

;;; The main reason the following things exist is so that the translator
;;; itself can be written in Scheme instead of in Common Lisp.  This is
;;; mostly just an exercise.

(eval-when (global:compile global:eval global:load)

(defun ampersandify (bvl)
  (do ((l bvl (cdr l))
       (z '() (cons (car l) z)))
      ((atom l)
       (cond ((null l)
              (nreverse z))
             (t
              (nreconc z `(&rest ,l)))))))

(defun concatenate-symbol (&rest things)        ;   T
  (values (intern (apply #'concatenate 'string (mapcar #'string things)))))
)

;;; Same as SETQ but inhibits "not declared special" warning.

(defmacro setq-global-value (var val)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (declare (special ,var))
       (setq ,var ,g)
       ',var)))

(defmacro define (pat &body body)
  (cond ((consp pat)
         `(defun ,(car pat) ,(ampersandify (cdr pat)) ,@body))
        (t
         `(setq-global-value ,pat ,@body))))

;;; LAMBDA is shadowed in the CLSCH package, to avoid a conflict here with
;;; any possible LAMBDA macro defined in the host Common Lisp.

(defmacro lambda (vars &body body)
  `#'(lisp:lambda ,(ampersandify vars) ,@body))

(defmacro iterate (tag specs &body body)
  (let ((vars (mapcar #'car specs))
        (id (gensym)))
    `(block ,id
       (let ,specs
         (tagbody
           ,id (macrolet ((,tag ,vars
                           `(progn (psetq ,@(list ,@(mapcan #'(lisp:lambda (var)
                                                                `(',var ,var))
                                                            vars)))
                                   (go ,',id))))
                 (return-from ,id (progn ,@body))))))))

#|The following version is better, but isn't efficiently compiled by most CL's.
(defmacro iterate (tag specs &body body)
  `(labels ((,tag ,(mapcar #'car specs)
             ,@body))
     (,tag ,@(mapcar #'cadr specs))))|#

(defmacro defsynonym (dst src)
  `(defmacro ,dst (&rest x) `(,',src ,@x)))

(defsynonym eq?      eq)
(defsynonym equal?   equal)
(defsynonym null?    null)
(defsynonym pair?    consp)
(defsynonym atom?    atom)
(defsynonym symbol?  symbolp)
(defsynonym append!  nconc)
(defsynonym reverse! nreverse)
(defsynonym vector?  simple-vector-p)

(defconstant else t)

;;; Pretty useless, but the existence of the definitions may help
;;; indentation work on the 3600.

(defmacro letrec (specs &body body)
  `(labels ,(mapcar #'(lisp:lambda (spec)
                        (let ((var (car spec)) (val (cadr spec)))
                          (cond ((and (pair? val)
                                      (eq? (car val) 'lambda))
                                 `(,var ,(ampersandify (cadr val))
                                        ,@(cddr val)))
                                (t
                                 (error "losing LETREC spec - ~s" spec)))))
                    specs)
     ,@body))

(defmacro fluid-let (specs &body body)
  `(let ,specs
     (declare (special ,@(mapcar #'car specs)))
     ,@body))

(defmacro define-macro ((name . bvl) &body body)
  `(progn 'compile
          (defmacro ,name ,(ampersandify bvl) ,@body)
          (*define-scheme-macro ',name ',name)))

(defmacro set! (var val)
  `(setq ,var ,val))


;;;----- Basic things which need to be defined before everything else

;;; Scheme lives within its own package, but happens to inherit the
;;; symbols that CLSCH exports.  It does NOT inherit from LISP!

(eval-when (eval load compile)
  (defvar clsch-package (find-package "CLSCH"))
  (defvar scheme-package (make-package 'scheme :use (list clsch-package))))

(eval-when (eval load compile)

  (export 'else clsch-package)

  (defun clsch-symbol (symbol)
    (cond ((eq (symbol-package symbol) clsch-package)
           symbol)
          (t
           (let ((symbol (intern (symbol-name symbol) scheme-package)))
             ;; (export symbol scheme-package) - necessary??
             symbol))))
  (defun clsch-export (symbol)
    (cond ((eq (symbol-package symbol) clsch-package)
           (export symbol clsch-package))
          ((not (eq (symbol-package symbol) scheme-package))
           (error "bogus symbol: (CLSCH-EXPORT '~s)" symbol)))
    symbol))


;;; Registers for passing arguments

(defvar *argument-registers*
  (list '**a1** '**a2** '**a3** '**a4** '**a5**))

(defvar *proc+arg-regs*
  (cons '**p** *argument-registers*))

(defvar *number-of-arg-regs*
  (length *argument-registers*))

(defvar **p**)
(defvar **a1**)
(defvar **a2**)
(defvar **a3**)
(defvar **a4**)
(defvar **a5**)

(define (ensure-enough-arg-regs n)
  (cond ((> n *number-of-arg-regs*)
         (iterate loop ((i (1+ *number-of-arg-regs*))
                        (l '()))
           (cond ((> i n)
                  (setf (cdr (last *argument-registers*))
                        (reverse! l))
                  (setq *number-of-arg-regs* n))
                 (else
                  (loop (1+ i)
                        (cons (intern (format nil "**A~D**" i)
                                      (find-package "CLSCH"))
                              l))))))))

;;; The Scheme readtable isn't needed until run time, but this seems
;;; like a good place to put it.

(defvar cl-readtable *readtable*)
(defvar scheme-readtable (copy-readtable nil))

(defconstant quasiquote-marker 'quasiquote-marker)
(defconstant unquote-marker    'unquote-marker)
(defconstant splice-marker     'splice-marker)

(defun sharp-excl-read-macro (stream subchar arg)
  (ignorable subchar arg)
  (let ((name (read stream t nil t)))
    (ccase name
      ((scheme::true) t)
      ((scheme::false) nil)
      ((scheme::null) '())
      ((scheme::quasiquote) quasiquote-marker)
      ((scheme::unquote) unquote-marker)
      ((scheme::splice) splice-marker)
      )))

(defun sharp-sharp-read-macro (stream subchar arg)
  (cond (arg (funcall (get-dispatch-macro-character #\# #\# cl-readtable)
                      stream subchar arg))
        (t '*)))

;;; Thanks to Eric Benson for the following hack.
;;; Small bug: a symbol beginning ":#" can't be read.

(defun read-alphabetic (stream char)
  (let ((following-char (peek-char nil stream nil stream t)))
    ;; EOF is OK, STREAM is EOF-VALUE.
    (if (or (eq following-char stream)
            (not (constituentp following-char)))
        (values (intern (string char)))
        (let ((subsymbol (read stream t nil t)))
          (check-type subsymbol symbol)
          (values (intern (concatenate 'string
                                       (string char)
                                       (symbol-name subsymbol))))))))

(defun constituentp (char)
  (multiple-value-bind (fun n-t-p)
      (get-macro-character char)
    (cond (fun (not n-t-p))
          (t (and (graphic-char-p char)
                  (not (find char " ',;`()[]{}")))))))

(defun quasiquote-read-macro (stream ignore)
  (list quasiquote-marker (read stream t nil t)))

(defun unquote-read-macro (stream ignore)
  (list (let ((following-char
                (peek-char nil stream nil stream t)))
          (cond ((char= following-char #\@)
                 (read-char stream)
                 splice-marker)
                (t unquote-marker)))
        (read stream t nil t)))

(defun illegal-read-macro (ignore char)
  (cerror "treat the character as whitespace"
          "illegal character read - ~s"
          char)
  (values))

(let ((*readtable* scheme-readtable))
  (set-macro-character #\` #'quasiquote-read-macro)
  (set-macro-character #\, #'unquote-read-macro)
  #-Symbolics
  (set-macro-character #\: #'read-alphabetic t)
  #+Symbolics   ;Symbolics Common Lisp has many bugs.
  (set-syntax-from-char #\: #\!)
  (set-macro-character #\| #'illegal-read-macro)
  (set-macro-character #\\ #'illegal-read-macro)
  (set-macro-character #\[ #'illegal-read-macro)
  (set-macro-character #\] #'illegal-read-macro)
  (set-macro-character #\{ #'illegal-read-macro)
  (set-macro-character #\} #'illegal-read-macro)
  (set-dispatch-macro-character #\# #\! #'sharp-excl-read-macro)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp-read-macro))


;;;----- The Scheme-to-Common-Lisp Compiler

;;; Each definition of a Scheme special form or procedure is annotated
;;; with one or more letters indicating why that feature is included in
;;; this Scheme implementation.
;;;   E = essential feature of RRRSS
;;;   R = inessential feature of RRRSS
;;;   S = something used in the book S&ICP
;;;   M = MIT Scheme feature needed to run 6.001 course software
;;;   T = random T feature which JAR likes to use

;;; Brief description of compilation strategy:
;;; Scheme code is translated into Common Lisp code, which can then be
;;; either interpreted or compiled.  Scheme parasitizes Common Lisp's
;;; data types.  Also, the compiler doesn't need to keep track of
;;; lexical variable references, since Common Lisp's lexical scoping can
;;; be used.  The main thing the compiler worries about is tail recursion.
;;; If a Scheme procedure returns a non-null second value, that means that
;;; it wants to do a tail-recursive call.  A driver loop is expected to
;;; call the procedure in the **P** register.  Procedures always expect
;;; to receive their arguments in registers **A1**, **A2**, etc.

;;; Currently the ENV argument is ignored.

;;; TRANSLATE returns two values: the output expression, and a flag
;;; saying whether or not the expression was trivial (i.e. made
;;; assignments to the argument registers).

(define (translate exp env ret?)
  (cond ((not (pair? exp))
         (translate-return exp t ret?))
        ((special-form? exp)
         (translate-special-form exp env ret?))
        (else
         (translate-call exp env ret?))))

(define (translate-return exp trivial? ret?)
  (ignorable ret?)
  (values exp trivial?))  ;!

;;; Procedure call

(define (translate-call proc+arg-exps env ret?)
  (let ((proc-exp (car proc+arg-exps))
        (arg-exps (cdr proc+arg-exps)))
    (cond ((and (symbol? proc-exp)
                (get proc-exp 'trivial))
           (translate-trivial-call (get proc-exp 'trivial)
                                   arg-exps env t ret?))
          ((and (pair? proc-exp) (eq? (car proc-exp) 'lambda))
           ;; LAMBDA-combinations are trivial.
           (multiple-value-bind (code trivial?)
               (translate-lambda-body (cddr proc-exp) env ret?)
             (let ((new-bvl (ampersandify (cadr proc-exp))))
               (mapc (lambda (b) (warn-if-redefining b "binding")) new-bvl)
               (translate-trivial-call `(lisp:lambda ,new-bvl
                                          ,code)
                                       arg-exps
                                       env
                                       trivial?
                                       ret?))))
          (else
           (translate-nontrivial-call proc-exp arg-exps env ret?)))))

(define (translate-trivial-call proc-code arg-exps env trivial? ret?)
  (iterate loop ((a arg-exps)
                 (z '())
                 (trivial? trivial?))
    (cond ((null? a)
           (translate-return `(,proc-code ,@(reverse! z))
                             trivial?
                             ret?))
          (else
           (multiple-value-bind (code arg-trivial?)
               (translate (car a) env nil)
             (loop (cdr a) (cons code z) (and trivial? arg-trivial?)))))))

(define (translate-nontrivial-call proc-exp arg-exps env ret?)
  (let ((nargs (length arg-exps)))
    (ensure-enough-arg-regs nargs)
    (let ((setup-code (psetqify-args proc-exp arg-exps env)))
      (cond (ret? (values `(progn ,@setup-code
                                  (values nil ,nargs))
                          nil))
            (else (values `(progn ,@setup-code
                                  (driver-loop ,nargs))
                          nil))))))

;;; Tries to do SETQ's in preference to PSETQ's, where possible.

(define (psetqify-args proc-exp arg-exps env)
  (iterate loop ((a (cons proc-exp arg-exps))
                 (r *proc+arg-regs*)
                 (setqs '())
                 (psetqs '()))
    (cond ((null? a)
           (append (if (null? psetqs) '() `((psetq ,@(reverse! psetqs))))
                   (if (null?  setqs) '() `(( setq ,@(reverse!  setqs))))))
          (else
           (multiple-value-bind (code trivial?)
               (translate (car a) env nil)
             (let ((reg (car r)))
               (cond (trivial?
                      (loop (cdr a) (cdr r) (list* code reg setqs) psetqs))
                     (else
                      (loop (cdr a) (cdr r) setqs (list* code reg psetqs))
                      ))))))))

;;; Generates adequate, but potentially inferior, code.

(define (psetqify-args-ez proc-exp arg-exps env)
  `((psetq ,@(mapcan (lambda (reg exp)
                       (list reg (translate exp env nil)))
                     *proc+arg-regs*
                     (cons proc-exp arg-exps)))))

;;; Scheme special forms

(defvar special-form-table)
(setq special-form-table (make-hash-table :test #'eq))

(define (special-form? exp)
  (and (pair? exp) (gethash (car exp) special-form-table nil)))

(define (translate-special-form exp env ret?)
  (funcall (gethash (car exp) special-form-table) exp env ret?))

(defmacro define-scheme-special-form (name args &body body)
  ;; Symbolics CL sucks.  Anonymous functions don't get compiled!
  (let ((internal-name (concatenate-symbol name '/scheme-special-form)))
    `(progn 'compile
            (export ',name clsch-package)
            (defun ,internal-name ,args ,@args ,@body)
            (setf (gethash ',name special-form-table) #',internal-name))))

;;; (Definitions of special forms appear in alphabetical order.)

;;; BEGIN, SEQUENCE

(define-scheme-special-form begin (exp env ret?)        ;E
  (translate-sequence (cdr exp) env ret?))

(define-scheme-special-form sequence (exp env ret?)     ; RS
  (translate-sequence (cdr exp) env ret?))

(define (translate-sequence exp-list env ret?)
  (cond ((null? (cdr exp-list))
         (translate (car exp-list) env ret?))
        (else
         (iterate loop ((l exp-list)
                        (z '())
                        (trivial? t))
           (multiple-value-bind (code arg-trivial?)
               (translate (car l) env (and (null? (cdr l)) ret?))
             (let ((trivial? (and trivial? arg-trivial?)))
               (cond ((null? (cdr l))
                      (values `(progn ,@(reverse! (cons code z))) trivial?))
                     (else
                      (loop (cdr l)
                            (cons code z)
                            trivial?)))))))))

;;; Top-level DEFINE

(defvar top-level-env '())

(define (translate-top-level exp)
  (cond ((definition? exp)
         (translate-top-level-define exp))
        (t
         (translate exp top-level-env nil))))

(define (translate-top-level-define exp)
  (let ((pat (cadr exp))
        (body (cddr exp)))
    (multiple-value-bind (name val)
        (parse-define pat body)
      (warn-if-redefining name "defining")
      (multiple-value-bind (code trivial?)
          (translate val top-level-env nil)
        (translate-return
          `(*define ',name ,code)
          trivial?
          nil)))))

(define (parse-define pat body)
  (cond ((pair? pat)
         (values (car pat)
                 ;; `(rec ,(car pat) ...)
                 `(lambda               ;Want NAMED-LAMBDA
                    ,(cdr pat)
                    ,@body)))
        (else
         (values pat (car body)))))

(define (definition? exp)
  (and (pair? exp) (eq? (car exp) 'scheme::define)))

(define (warn-if-redefining name what)
  (cond ((get name 'trivial)
         (in-scheme-package
          (lambda ()
            (format *error-output*
                    "~&Warning: ~A ~S, which is a built-in procedure.~&"
                    what
                    name)))
         (remprop name 'trivial))))

;;; IF

(define-scheme-special-form if (exp env ret?)   ;E
  (multiple-value-bind (test-code test-trivial?)
      (translate (cadr exp) env nil)
    (multiple-value-bind (con-code con-trivial?)
        (translate (caddr exp) env ret?)
      (multiple-value-bind (alt-code alt-trivial?)
          (translate (cadddr exp) env ret?)
        (values `(if ,test-code
                     ,con-code
                     ,alt-code)
                (and test-trivial? con-trivial? alt-trivial?))))))

;;; LAMBDA

(define-scheme-special-form lambda (exp env ret?)       ;E S
  (multiple-value-bind (code trivial?)
      (translate-lambda-body (cddr exp) (cons (cadr exp) env) t)
    (ignorable trivial?)
    (translate-return `(create-procedure
                         (lambda (nargs)
                           ;; Eventually, check number of args.
                           (ignorable nargs)
                           (let ,(bind-arguments (cadr exp))
                             ,code))
                         nil)
                      t                         ;yes, closures are trivial
                      ret?)))

(define (bind-arguments bvl)
  (ensure-enough-arg-regs (improper-length bvl))
  (do ((l bvl (cdr l))
       (a *argument-registers* (cdr a))
       (n 0 (+ n 1))
       (z '() (cons `(,(car l) ,(car a)) z)))
      ((not (pair? l))
       (reverse! (cond ((null? l) z)
                       (else (cons `(,l (get-n-ary-arg nargs ,n)) z)))))
    (assert (not (null? a)))
    (warn-if-redefining (car l) "binding")))

(define (improper-length l)
  (do ((l l (cdr l))
       (n 0 (1+ n)))
      ((atom? l) n)))

(define (translate-lambda-body body env ret?)
  (iterate loop ((b body)
                 (vars '())
                 (z '()))
    (cond ((null? (cdr b))
           (multiple-value-bind (code trivial?)
               (translate-sequence (reverse! (cons (car b) z))
                                   (cons vars env)
                                   ret?)
             (values (if (null? vars)
                         code
                         `(let ,(mapcar (lambda (var) `(,var '*undefined*))
                                        vars)
                            ,code))
                     trivial?)))
          (else
           (let ((exp (car b)))
             (cond ((definition? exp)
                    (multiple-value-bind (name val)
                        (parse-define (cadr exp) (cddr exp))
                      (loop (cdr b)
                            (cons name vars)
                            (cons `(set! ,name ,val)
                                  z))))
                   (else
                    (loop (cdr b)
                          vars
                          (cons (car b) z)))))))))

(define-scheme-special-form delay (exp env ret?)        ;  S
  (translate-return `(make-delay :thunk-or-value
                                 #'(lisp:lambda ()
                                     ,(translate (cadr exp) env nil)))
                    t ret?))

(setf (get 'make-delay 'trivial) 'make-delay)

;;; QUOTE

(define-scheme-special-form quote (exp env ret?)        ;E S
  (translate-return `(quote ,(cadr exp)) t ret?))

;;; SET!

(define-scheme-special-form set! (exp env ret?) ;E S
  (let ((var (cadr exp))
        (val-exp (caddr exp)))
    (multiple-value-bind (code trivial?)
        (translate val-exp env nil)
      (warn-if-redefining var "assigning")
      (translate-return `(setq ,var ,code)
                        trivial?
                        ret?))))

;;; Renegade DEFINE-MACRO - sort of compatible with MIT Scheme

(define-scheme-special-form define-macro (exp env ret?) ;   M
  (let ((name (caadr exp))
        (bvl  (cdadr exp))
        (body (cddr exp)))
    (let ((internal-name (concatenate-symbol name '/scheme-macro)))
      (multiple-value-bind (code trivial?)
          (translate-sequence body env nil)
        (ignorable trivial?)
        (translate-return `(progn 'compile
                                  (defmacro ,internal-name ,(ampersandify bvl)
                                    ,code)
                                  (*define-scheme-macro ',name
                                                        ',internal-name))
                          t
                          ret?)))))

;;; Renegade FLUID-LET - like MIT Scheme, but only works on top level
;;; variables!  (For obvious reasons.)

(define-scheme-special-form fluid-let (exp env ret?)
  (let ((specs (cadr exp))
        (body (cddr exp)))
    (multiple-value-bind (code trivial?)
        (translate-sequence body env ret?)
      (ignorable trivial?)
      (translate-return `(let ,(mapcar (lambda (spec)
                                         `(,(car spec)
                                           ,(translate (cadr spec) env nil)))
                                       specs)
                           (declare (special ,@(mapcar #'car specs)))
                           ,code)
                        nil
                        ret?))))

;;; Test routines for translator

(define (tst exp)
  (pretty-print (translate-top-level (schemify exp)))
  '*)

(define (schemify sexpr)      ;don't try to use this with backquote forms!
  (cond ((null? sexpr) sexpr)
        ((symbol? sexpr) (intern (symbol-name sexpr) scheme-package))
        ((pair? sexpr) (cons (schemify (car sexpr)) (schemify (cdr sexpr))))
        (else sexpr)))

#|(define (show obj)
    (check-type obj function)
    (pretty-print (si::undigest (sys::%p-contents-offset obj 1)))
    '*)|#

;;;----- Scheme macros

(defmacro define-scheme-macro ((name . args) &body body)
  (let ((internal-name (concatenate-symbol name '/scheme-macro)))
    `(progn 'compile
            (export ',name clsch-package)
            (defmacro ,internal-name ,(ampersandify args) ,@body)
            (*define-scheme-macro ',name ',internal-name))))

(defun *define-scheme-macro (sym fun)
  (setf (gethash sym special-form-table)
        (lambda (exp env ret?)
          (translate (macroexpand-1 (cons fun (cdr exp)))
                     env
                     ret?)))
  sym)

;;; (Definitions of macros appear in alphabetical order.)

(define-scheme-macro (and . forms)              ; RS
  (labels ((expand-and (forms)
            (cond ((atom? forms) 't)
                  ((atom? (cdr forms)) (car forms))
                  (else `(if ,(car forms)
                             ,(expand-and (cdr forms))
                             nil)))))
    (expand-and forms)))

;;; case                                        ; R
;;; collect                                     ;  S (!)

(define-scheme-macro (cond . clauses)   ;E S
  (labels ((expand-cond (clauses)
            (cond ((atom? clauses) ''**no-more-cond-clauses**)
                  ((atom? (car clauses))
                   (cerror "ignore it" "atomic COND clause: ~s" (car clauses))
                   (expand-cond (cdr clauses)))
                  ((atom? (cdar clauses))
                   `(or ,(caar clauses)
                        ,(expand-cond (cdr clauses))))
                  ((eq? (caar clauses) 'else)
                   (if (not (atom? (cdr clauses)))
                       (cerror "ignore clauses following ELSE clause"
                               "ELSE clause not last in COND: ~s"
                               `(cond ,@clauses)))
                   `(begin ,@(cdar clauses)))
                  (else `(if ,(caar clauses)
                             (begin ,@(cdar clauses))
                             ,(expand-cond (cdr clauses)))))))
    (expand-cond clauses)))

(define-scheme-macro (cons-stream head tail)    ;  S
  `(scheme::cons ,head (delay ,tail)))

(define-scheme-macro (do specs end &body body)  ; R
  (let ((loop (gensym "DO")))
    `(letrec ((,loop (lambda ,(mapcar #'car specs)
                       (cond ,end
                             (else ,@body
                                   (,loop
                                    ,@(mapcar (lambda (y)
                                                (if (and (cdr y) (cddr y))
                                                    (caddr y)
                                                    (car y)))
                                              specs)))))))
       (,loop ,@(mapcar (lambda (y) (if (cdr y) (cadr y) 'nil))
                        specs)))))

(define-scheme-macro (let specs &body body)     ;E S
  (cond ((and (not (pair? specs))   ;Weird MIT extension
              (not (null? specs)))
         (let ((tag specs)
               (specs (car body))
               (body (cdr body)))
           `(letrec ((,tag (lambda ,(mapcar #'car specs) ,@body)))
              (,tag ,@(mapcar #'cadr specs)))))
        (else
         `((lambda ,(mapcar #'car specs) ,@body)
           ,@(mapcar (lambda (x)
                       (cond ((atom? (cdr x)) ''**let-missing-initializer**)
                             (else (cadr x))))
                     specs)))))

;;; let*                                        ; R

(define-scheme-macro (letrec specs &body body)  ;E
  (iterate loop ((s specs)
                 (vars '())
                 (inits '()))
    (cond ((null? s)
           `((lambda ,vars
               ,@(reverse! inits)
               . ,body)
             ,@(mapcar (lambda (var) (ignorable var) ''**unbound-label**)
                       vars)))
          (else
           (let ((spec (car s)))
             (cond ((atom? spec)
                    (cerror "ignore it"
                            "bad spec - (LETREC (... ~S ...) ...)"
                            spec)
                    (loop (cdr s) vars inits))
                   ((atom? (car spec))
                    (loop (cdr s)
                          (cons (car spec) vars)
                          (cons `(set! ,@spec)
                                inits)))
                   (else
                    (loop (cdr s)
                          (cons (caar spec) vars)
                          (cons `(set! ,(caar spec)
                                       (lambda ,(cdar spec)
                                         ,@(cdr spec)))
                                inits)))))))))

;;; make-environment                            ;  S (!)

(define-scheme-macro (or . args)                ; RS
  (labels ((expand-or (args)
             (cond ((atom? args) ''nil)
                   ((atom? (cdr args)) (car args))
                   (else `((lambda (p)
                             (if p p ,(expand-or (cdr args))))
                           ,(car args))))))
    (expand-or args)))

(define-scheme-macro (rec var exp)              ;E S
  `(letrec ((,var ,exp)) ,var))

(define-scheme-macro (trace var)
  `(set! ,var (*trace ,var ',var)))

(define-scheme-macro (untrace var)
  `(set! ,var (*untrace ,var)))

;;; Quasiquote

(*define-scheme-macro quasiquote-marker quasiquote-marker)
(defmacro quasiquote-marker (x)
  (expand-quasiquote x 0))

(*define-scheme-macro unquote-marker unquote-marker)
(defmacro unquote-marker (x)
  (cerror "as as if the comma wasn't there at all"
          "comma not inside backquote form - ,~S" x)
  x)

(*define-scheme-macro splice-marker splice-marker)
(defmacro splice-marker (x)
  (cerror "act as if the ,@ wasn't there at all"
          "\",@\" not inside backquote form - ,@~S" x)
  x)

(define (expand-quasiquote x level)
  (multiple-value-bind (mode arg)
      (descend-quasiquote x level)
    (finalize-quasiquote mode arg)))

(define (finalize-quasiquote mode arg)
  (cond ((eq? mode 'quote) `',arg)
        ((eq? mode 'unquote) arg)
        ((eq? mode 'splice)
         (cerror "act as if () had been seen instead of ,@<form>"
                 ",@ in illegal context - ,@~s"
                 arg))
        (else `(,mode ,@arg))))

;;; The two return values, mode and arg, are interpreted as follows:
;;;    mode    arg          meaning
;;;    QUOTE   x            'x
;;;    UNQUOTE x            x
;;;    LIST    (x1 x2 ...)  (LIST x1 x2 ...)
;;;    CONS*   (x1 x2 ...)  (CONS* x1 x2 ...)
;;;    APPEND  (x1 x2 ...)  (APPEND x1 x2 ...)

(define (descend-quasiquote x level)
  (cond ((atom? x)
         (values 'quote x))
        ((interesting-to-quasiquote? x quasiquote-marker)
         (descend-quasiquote-pair x (1+ level)))
        ((interesting-to-quasiquote? x unquote-marker)
         (cond ((= level 0)
                (values 'unquote (cadr x)))
               (else
                ;; BUG: ,,@ doesn't work.  I think this is the spot
                ;; where it would have to be hacked in.
                (descend-quasiquote-pair x (- level 1)))))
        ((interesting-to-quasiquote? x splice-marker)
         (cond ((= level 0)
                (values 'splice (cadr x)))
               (else
                (descend-quasiquote-pair x (- level 1)))))
        (else
         (descend-quasiquote-pair x level))))

(define (descend-quasiquote-pair x level)
  (multiple-value-bind (car-mode car-arg)
      (descend-quasiquote (car x) level)
    (multiple-value-bind (cdr-mode cdr-arg)
        (descend-quasiquote (cdr x) level)
      (cond ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote))
             (values 'quote x))
            ((eq? car-mode 'splice)
             ;; (,@mumble ...)
             (cond ((and (eq? cdr-mode 'quote) (null? cdr-arg))
                    (values 'unquote
                            car-arg))
                   ((eq? cdr-mode 'scheme::append)
                    (values 'scheme::append
                            (cons car-arg cdr-arg)))
                   (else
                    (values 'scheme::append
                            (list car-arg (finalize-quasiquote cdr-mode cdr-arg))))))
            ((and (eq? cdr-mode 'quote) (null? cdr-arg))
             (values 'scheme::list
                     (list (finalize-quasiquote car-mode car-arg))))
            ((or (eq? cdr-mode 'scheme::list) (eq? cdr-mode 'cons*))
             (values cdr-mode
                     (cons (finalize-quasiquote car-mode car-arg)
                           cdr-arg)))
            (else
             (values 'cons*
                     (list (finalize-quasiquote car-mode car-arg)
                           (finalize-quasiquote cdr-mode cdr-arg))))))))

(define (interesting-to-quasiquote? x marker)
  (and (pair? x)
       (eq? (car x) marker)
       (pair? (cdr x))
       (null? (cddr x))))

;;;----- Runtime system

(defstruct (procedure (:predicate procedure?)
                      (:print-function (lisp:lambda (struct stream ignore)
                                         (scheme::write struct stream))))
  function
  name)

(defvar *procedure-uid-counter* 0)
(defun create-procedure (fun name)
  (make-procedure :function fun :name (or name (incf *procedure-uid-counter*))))

(defun *define (name value)
  (set-name value name)
  (setf (symbol-value name) value)
  ;; Set function cell, but avoid clobbering things like WRITE and LAMBDA.
  (if (and (procedure? value)
           (eq (symbol-package name) scheme-package)
           (not (get name 'trivial)))
      (setf (symbol-function name) (procedure->function value)))
  #+Symbolics
  (si:record-source-file-name name 'scheme::define)     ; JDR Hack.
  name)

(defun set-name (obj name)
  (if (and (procedure? obj) (numberp (procedure-name obj)))
      (setf (procedure-name obj) name)))

(defun driver-loop (nargs)
  (prog (val)
   loop
    (multiple-value-setq (val nargs)
      (funcall (procedure-function **p**) nargs))
    (if (not nargs) (return val))
    (go loop)))

(defun get-n-ary-arg (nargs n)
  (do ((i n (1+ i))
       (l (nthcdr n *argument-registers*) (cdr l))
       (z '() (cons (symbol-value (car l)) z)))
      ((= i nargs) (nreverse z))
    (declare (fixnum i))))

(defun function->procedure (fun name)
  (create-procedure (lambda (nargs)
                        (apply fun (get-n-ary-arg nargs 0)))    ;Suboptimal
                    name))

(defun procedure->function (proc)
  (lambda args
    (driver-loop (set-registers-from-list proc args))))

;;; Returns number of args.

(defun set-registers-from-list (proc args)
  (let ((nargs (length args)))
    (ensure-enough-arg-regs nargs)
    (mapc (lambda (arg-reg arg)
              (setf (symbol-value arg-reg) arg))
          *argument-registers*
          args)
    (setf **p** proc)
    nargs))

(defun scheme-call (proc &rest args)
  (driver-loop (set-registers-from-list proc args)))

(defun scheme-eval (exp)
  (eval (translate-top-level exp)))

;;; Macro for defining scheme procedures.

;;; A nontrivial procedure is one which potentially modifies the
;;; registers.

(defmacro define-scheme-nontrivial ((name . bvl) &body body)
  (let ((scheme-name (clsch-symbol name)))
    `(progn 'compile
            (clsch-export ',scheme-name)
            (defun ,scheme-name ,(ampersandify bvl) ,@body)
            (setq-global-value ,scheme-name
                               (function->procedure #',scheme-name
                                                    ',scheme-name)))))

(defmacro define-scheme (pat &body body)
  (cond ((consp pat)
         (let ((scheme-name (clsch-symbol (car pat))))
           `(progn 'compile
                   (define-scheme-nontrivial ,pat ,@body)
                   (setf (get ',scheme-name 'trivial) ',scheme-name))))
        (t
         (let ((scheme-name (clsch-symbol pat)))
           `(progn 'compile
                   (clsch-export ',scheme-name)
                   (setq-global-value ,scheme-name ,(car body)))))))


;;;----- LOAD/COMPILE subsystem

(defvar *scheme-object-file-type* "SBIN")
(defvar *scheme-source-file-type* "SCM")

;;; READ-SCHEME-FILE returns two values:
;;;    1. A list of source expressions
;;;    2. The truename of the file it openend

(defun read-scheme-file (name)
  (with-open-file (s (scheme-source-pathname name)
                     :direction :input)
    (in-scheme-package
      (lambda ()
        (iterate loop ((z '()))
          (let ((form (read s nil '*eof-object*)))
            (cond ((eq form '*eof-object*)
                   (values (nreverse z) (truename s)))
                  (t
                   (loop (cons form z))))))))))

;;; Like READ-SCHEME-FILE, but translates as well.

(defun translate-scheme-file (name)
  (multiple-value-bind (code true-name)
      (read-scheme-file name)
    (values (mapcar #'translate-top-level code) true-name)))

;;; Smart LOAD routine.

(define-scheme-nontrivial (load name &optional (compile? nil))  ;E
  (cond ((scheme-object-file? name)
         (load-scheme-object-file name))
        ((member (pathname-type name) '(nil :unspecific))
         (let* ((src-name (scheme-source-pathname name))
                (obj-name (scheme-object-pathname name))
                (src (probe-file src-name))
                (obj (probe-file obj-name)))
           (cond ((not obj)
                  (load-scheme-source-file (or src src-name) compile?))
                 ((not src)
                  (load-scheme-object-file obj))
                 (t
                  (let ((src-version (pathname-version src))
                        (obj-version (pathname-version obj)))
                    (cond ((or (not (numberp obj-version))
                               (not (numberp src-version))
                               (>= obj-version src-version))
                           (load-scheme-object-file obj))
                          (t
                           (format t "~&Source file is newer, loading source file~%")
                           (load-scheme-source-file src compile?))))))))
        (t
         (load-scheme-source-file name compile?))))

(defun scheme-object-file? (name)
  (let ((type (pathname-type name)))
    (and (stringp type) (string-equal type *scheme-object-file-type*))))

(defun load-scheme-source-file (name compile?)
  (let (#+Symbolics (si:fdefine-file-pathname (unspecify name)))        ;JDR hack.
    (multiple-value-bind (code name)
        (translate-scheme-file name)
      (format t "~&Loading ~S~%" (namestring name))
      (cond (compile?
             (funcall (let (#+Symbolics (compiler:suppress-compiler-warnings t))
                        (compile nil `(lisp:lambda () ,@code)))))
            (t
             (mapc #'eval code)))
      name)))

(defun unspecify (name)
  (make-pathname :defaults name
                 :type nil  ;*scheme-source-file-type*  - ??
                 :version nil))

(defun load-scheme-object-file (name)
  (in-scheme-package
   (lambda ()
     (let ((*readtable* cl-readtable))
       (load name))))
  ;; Use FUNCALL ' to suppress undefined function warning.
  (funcall 'run-top-level-forms)
  ;; Avoid function redefinition message.
  (fmakunbound 'run-top-level-forms)
  ;; Flush any information compiler or LOAD might have stored.
  (setf (symbol-plist 'run-top-level-forms) '())
  name)

;;; File compiler.

(define-scheme-nontrivial (compile-file scheme-pathname
                                        &optional keep-lisp-file?)
  (multiple-value-bind (code name)
      (translate-scheme-file scheme-pathname)
    (format t "~&Compiling ~S~%" (namestring name))
    (let ((form `(defun run-top-level-forms ()
                   (let (#+Symbolics                    ;JDR hack
                         (si:fdefine-file-pathname
                           ;; parse-pathname ??
                           (parse-namestring ',(namestring (unspecify name)))))
                     ,@code)))
          (lisp-pathname (make-pathname :type "TEMP" :defaults name))
          (fasl-pathname (scheme-object-pathname name)))
      (unwind-protect (in-scheme-package
                        (lambda ()
                          (let ((*readtable* cl-readtable))
                            (with-open-file (out lisp-pathname :direction :out)
                              (format out ";-*-Package:SCHEME-*-~%")
                              (print '(in-package "SCHEME") out)
                              (if keep-lisp-file?
                                  (pretty-print form out)
                                  (print form out)))
                            (let (#+Symbolics
                                  (compiler:suppress-compiler-warnings t))
                              (compile-file lisp-pathname
                                            :output-file fasl-pathname)
                              fasl-pathname))))
        (if (not keep-lisp-file?) (delete-file lisp-pathname))))))

;;; Pathname hacking auxiliaries.

(defun scheme-source-pathname (name)
  (let* ((pathname (pathname name))
         (type (pathname-type pathname))
         (type (cond ((member (pathname-type pathname)
                              '(nil :unspecific))
                      *scheme-source-file-type*)
                     (t type)))
         (version (or (pathname-version pathname) :newest)))
    (merge-pathnames                                    ;JDR hack
      (make-pathname :defaults pathname
                     :type type
                     :version version))))

(defun scheme-object-pathname (name)
  (merge-pathnames                                      ;JDR hack
    (make-pathname :type *scheme-object-file-type* :defaults name)))

#+Symbolics
(let ((type (zl:string *scheme-source-file-type*)))
  (cond ((not (member type fs:*its-uninteresting-types* :test #'equal))
         (push type fs:*its-uninteresting-types*))))

#+Symbolics
(fs:define-canonical-type :scheme *scheme-source-file-type*) ;Scheme source

#+Symbolics
(fs:define-canonical-type :scheme-bin *scheme-object-file-type*) ;Scheme binary

;;; Default mode for scheme source is lisp.
#+Symbolics
(unless (assoc :scheme fs:*file-type-mode-alist*)
  (push (cons :scheme :lisp) fs:*file-type-mode-alist*))

;;; Utility for getting the package bound properly.  Nontrivial on a
;;; Lisp Machine!

(defun in-scheme-package (thunk)
  (let ((*readtable* scheme-readtable))
    #-Symbolics
    (let ((*package* scheme-package))
      (funcall thunk))
    #+Symbolics
    (zl:standard-value-let ((zl:package scheme-package))
      (funcall thunk))))

#+Symbolics
(push scheme-package si:*reasonable-packages*)

;;; Hack to allow loading scheme code as lisp code.
;;; Also makes indenting work right in ZMACS.

(defmacro scheme::define (pat &body body)
  (translate-top-level `(scheme::define ,pat ,@body)))


;;;----- Definitions for various Scheme procedures

;;; In alphabetical order

(define-scheme-nontrivial (apply proc . rest)   ;E S
  (values nil (set-registers-from-list proc (apply #'list* rest))))

(define-scheme (assq obj list)                  ;E S
  (assoc obj list :test #'eq))

(define-scheme (assoc obj list)                 ;E
  (assoc obj list :test #'equal))

;;; Note that in a true Scheme, the escape procedure would have
;;; indefinite extent.

(define-scheme-nontrivial (call-with-current-continuation proc) ;E (!!)
  (block cwcc
    (scheme-call proc
                 (function->procedure (lambda (val)
                                          (return-from cwcc val))
                                      'scheme::continuation))))

(define-scheme-nontrivial (call-with-input-file string proc)    ;E
  (with-open-file (port string :direction :input)
    (scheme-call proc port)))

(define-scheme-nontrivial (call-with-output-file string proc)   ;E
  (with-open-file (port string :direction :output)
    (scheme-call proc port)))

(define-scheme (ceiling num)                    ; R
  (values (ceiling num 1)))

(define-scheme (char-whitespace? char)          ; R
  (or (char= char #\space)
      (not (graphic-char-p char))))

(define-scheme (current-input-port)             ;E
  *standard-input*)

(define-scheme (current-output-port)            ;E
  *standard-output*)

(define-scheme (display obj &optional (port *standard-output*))
  (fluid-let ((*print-escape* nil))
    (scheme::write obj port)))                  ;E

(define-scheme (eof-object? obj)                ;E
  (eq obj '*eof-object*))

(define-scheme-nontrivial (error . items)       ;  S
  (apply #'error
         (apply #'concatenate
                'string
                "~a"
                (mapcar (lambda (item) (ignorable item) "~%~10t~s")
                        (cdr items)))
         items))

(define-scheme-nontrivial (eval exp env)        ;  S
  (declare (special user-initial-environment))  ;inhibit warning
  (if (not (eq? env user-initial-environment))
      (cerror "use the USER-INITIAL-ENVIRONMENT instead"
              "illegal environment argument - (EVAL '~s '~s)"
              exp env))
  (scheme-eval exp))

(define-scheme (exact? num)                     ;E
  (ignorable num) nil)

(define-scheme (exit . vals)                    ;   T
  (throw 'exit-scheme (values-list vals)))

(define-scheme (explode atom)                   ;  S
  (map 'list
       (lambda (ch)
         (intern (string ch) scheme-package))
       (symbol-name atom)))

(define-scheme (floor num)                      ; R
  (values (floor num 1)))

(define-scheme (for-each proc . lists)          ;E
  (apply #'mapc (procedure->function proc) lists))

(define-scheme mapc for-each)                   ;  S

(defstruct delay (forced-yet? nil) thunk-or-value)

(define-scheme-nontrivial (force obj)           ;  S
  (cond ((delay-p obj)
         (let ((tv (delay-thunk-or-value obj)))
           (cond ((delay-forced-yet? obj) tv)
                 (t (let ((val (funcall tv)))
                      (setf (delay-thunk-or-value obj) val)
                      (setf (delay-forced-yet? obj) t)
                      val)))))
        (t obj)))

(define-scheme (implode list)                   ;  S
  (intern (map 'string #'character list) scheme-package))

(define-scheme (inexact? num)                   ;E
  (ignorable num) t)

(define-scheme (input-port? obj)                ;E
  (and (streamp obj) (input-stream-p obj)))

(define-scheme (list->string list)              ;E
  (coerce list 'string))

(define-scheme (list->vector list)              ;E
  (coerce list 'vector))

(define-scheme (list-ref list n)                ; R
  (nth n list))

(define-scheme (list-tail list n)               ; R
  (nthcdr n list))

(define-scheme (make-table &optional (name nil)) ;   T
  (ignorable name)
  (values (make-hash-table)))

(define-scheme (make-vector size                ;E S
                            &optional
                            (fill '*uninitialized-vector-element*))
  (make-sequence 'vector size :initial-element fill))

(define-scheme (map proc . lists)               ;E
  (apply #'mapcar (procedure->function proc) lists))

(define-scheme mapcar scheme::map)              ;  S

(define-scheme (memq obj list)                  ;E S
  (member obj list :test #'eq))

(define-scheme (member obj list)                ;E
  (member obj list :test #'equal))

(define-scheme nil nil)                         ; RS

(define-scheme (number->string num format)
  (if (not (equal? format '(scheme::heur)))
      (cerror "act as if the format was (HEUR)"
              "unimplemented format: (NUMBER->STRING '~s '~s)"
              num format))
  (write-to-string num))

(define-scheme (output-port? obj)               ;E
  (and (streamp obj) (output-stream-p obj)))

(define-scheme (quotient n1 n2)                 ;E S
  (values (truncate n1 n2)))

(define-scheme (read &optional (port *standard-input*))         ;E
  (read port nil '*eof-object*))

(define-scheme (read-char &optional (port *standard-input*))    ;E
  (read-char port nil '*eof-object*))

(define-scheme (real? obj)                      ;E
  (and (numberp obj) (not (complexp obj))))

(define-scheme (reset)
  (throw 'reset-scheme nil))

(define-scheme (round num)                      ; R
  (values (round num 1)))

(define-scheme (set-car! pair obj)              ;E
  (setf (car pair) obj))

(define-scheme (set-cdr! pair obj)              ;E
  (setf (cdr pair) obj))

(define-scheme (string->list string)            ;E
  (coerce string 'list))

(define-scheme (string->symbol string)          ;E
  (values (intern string scheme-package)))

(define-scheme (string-null? string)            ;E
  (= (length string) 0))

(define-scheme (pretty-print obj &optional (port *standard-output*))
  (let ((*print-pretty* t))
    (print obj port)
    t))

(define-scheme (put sym ind val)                ;  S  !
  (setf (get sym ind) val))

(define-scheme (set-table-entry! table key val) ;   T (sort of)
  (setf (gethash key table) val))

(define-scheme (string-append . strings)        ;E
  (apply #'concatenate 'string strings))

(define-scheme t t)                             ; RS

(define-scheme (table-entry table key)          ;   T
  (gethash key table nil))

(define-scheme-nontrivial (tail stream)         ;  S
  (let ((d (force (cdr stream))))
    (setf (cdr stream) d)
    d))

(define-scheme the-empty-stream '())            ;  S

(define-scheme (truncate num)                   ; R
  (values (truncate num 1)))

(define-scheme (vector->list vec)               ;E
  (coerce vec 'list))

(define-scheme (vector-set! vec k obj)          ;E S
  (setf (svref vec k) obj))

(define-scheme-nontrivial (with-input-from-file string thunk)   ; R
  (with-open-file (*standard-input* string :direction :input)
    (scheme-call thunk)))

(define-scheme-nontrivial (with-input-from-file string thunk)   ; R
  (with-open-file (*standard-output* string :direction :output)
    (scheme-call thunk)))

(define-scheme user-initial-environment         ;  S
  'user-initial-environment)

(define-scheme (write obj &optional (port *standard-output*))  ;E
  (fluid-let ((*standard-output* port))
    (really-write obj)))

(define (really-write obj)
  (cond ((null? obj)
         (format *standard-output* "()"))
        ((pair? obj) (write-list obj))
        ((eq? obj t)
         (format *standard-output* "#!TRUE"))
        ((eq? obj quasiquote-marker)
         (format *standard-output* "#!QUASIQUOTE"))
        ((eq? obj unquote-marker)
         (format *standard-output* "#!UNQUOTE"))
        ((eq? obj splice-marker)
         (format *standard-output* "#!SPLICE"))
        ((procedure? obj)
         (format *standard-output* "#{Procedure ")
         (really-write (procedure-name obj))
         (format *standard-output* "}"))
        ((vector? obj) (write-vector obj))
        (else
         (write obj))))

(define (write-list obj)
  (cond ((and *print-level* (<= *print-level* 0))
         (write-string "#"))
        ((and (symbol? (car obj))
              (get (car obj) 'printer))
         (funcall (get (car obj) 'printer) obj))
        (else
         (write-list-normally obj))))

(define (write-list-normally obj)
  (write-char #\()
  (fluid-let ((*print-level* (and *print-level* (1- *print-level*))))
    (really-write (car obj))
    (iterate loop ((l (cdr obj))
                   (n 1))
      (cond ((atom? l)
             (cond ((not (null? l))
                    (write-char #\space)
                    (write-char #\.)
                    (write-char #\space)
                    (really-write l))))
            (else
             (write-char #\space)
             (cond ((and *print-length* (>= n *print-length*))
                    (write-string "..."))
                   (else
                    (really-write (car l))
                    (loop (cdr l) (1+ n))))))))
  (write-char #\))
  t)

(define (write-vector obj)
   (write-char #\#)
   (write-char #\()
   (let ((z (length obj)))
     (cond ((> z 0)
            (really-write (svref obj 0))
            (iterate loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space)
                     (cond ((and *print-length* (>= i *print-length*))
                            (write-string "..."))
                           (else
                            (really-write (svref obj i))
                            (loop (1+ i))))))))))
   (write-char #\))
   t)

;;; Stuff for making printers understand Scheme backquote

(define (make-macro-char-printer prefix)
  (lambda (obj)
    (cond ((or (not (pair? (cdr obj)))
               (not (null? (cddr obj))))
           (write-list-normally obj))
          (else
           (write-string prefix)
           (really-write (cadr obj))))))

(setf (get 'quote            'printer) (make-macro-char-printer "'"))
(setf (get quasiquote-marker 'printer) (make-macro-char-printer "`"))
(setf (get unquote-marker    'printer) (make-macro-char-printer ","))
(setf (get splice-marker     'printer) (make-macro-char-printer ",@"))

#+Symbolics
(progn 'compile   ;Until a pretty-printer exists
       (setf (get quasiquote-marker 'gprint::formatter)
             (lambda (x) (gprint::format-quote "`" x)))
       (setf (get unquote-marker 'gprint::formatter)
             (lambda (x) (gprint::format-quote "," x)))
       (setf (get splice-marker 'gprint::formatter)
             (lambda (x) (gprint::format-quote ",@" x))))

(defvar *trace-level* 0)

(define-scheme (*trace proc name)
  (function->procedure
   (lambda args
     (fluid-let ((*trace-level* (1+ *trace-level*)))
       (format t "~&~vt~d Enter ~s" *trace-level* *trace-level* name)
       (mapc (lambda (arg) (princ #\space) (scheme::write arg)) args)
       (let ((val (apply #'scheme-call proc args)))
         (format t "~&~vt~d Exit ~s "
                 *trace-level* *trace-level* name)
         (scheme::write val)
         val)))
   `(trace ,proc)))

(define-scheme (*untrace proc)
  (let ((name (procedure-name proc)))
    (cond ((and (consp name) (eq (car name) 'trace))
           (cadr name))
          (t
           (format t "~&Wasn't traced in the first place.~%")
           proc))))

;;; Entries are in alphabetical order.  (Any other order would seem
;;; arbitrary.  This way it's easy to tell whether something exists.)

(mapc (lambda (z)
        (multiple-value-bind (from to)
            (cond ((consp z) (values (cadr z) (car z)))
                  (t         (values z        z)))
          (assert (not (eq? (symbol-package from) clsch-package)) from)
          (let ((to (clsch-symbol to)))
            (clsch-export to)
            (setf (symbol-value to)
                  (function->procedure (symbol-function from) to))
            (setf (get to 'trivial) from)
            (setf (symbol-function to) (symbol-function from)))))
      '(
        1+                                      ; RS
        (-1+ 1-)                                ; RS
        + - * /                                 ;E S
        = < >                                   ;E S
        <= >=                                   ;E S ?
        (=? =) (<? <) (>? >) (<=? <=) (>=? >=)  ;E
        abs                                     ;E S
        acos                                    ; R
        append                                  ;E
        (append! nconc)                         ; R
        asin                                    ; R
        atan                                    ; R
        (assv assoc)                            ;E
        (atom? atom)                            ;  S
        car cdr caar cadr cdar cddr             ;E S
        caaar caadr cadar caddr                 ;E S
        cdaar cdadr cddar cdddr                 ;E S
        caaaar caaadr caadar caaddr             ;E S
        cadaar cadadr caddar cadddr             ;E S
        cdaaar cdaadr cdadar cdaddr             ;E S
        cddaar cddadr cdddar cddddr             ;E S
        (char->integer char-code)               ;E
        (char-alphabetic? alpha-char-p)         ; R
        (char-ci<=? char-not-greaterp)          ; R
        (char-ci<?  char-lessp)                 ; R
        (char-ci=?  char-equal)                 ; R
        (char-ci>=? char-not-lessp)             ; R
        (char-ci>?  char-greaterp)              ; R
        (char-numeric? digit-char-p)            ; R  (almost)
        char-downcase                           ; R
        (char-lower-case? lower-case-p)         ; R
        char-upcase                             ; R
        (char-upper-case? upper-case-p)         ; R
        (char<=? char<=)                        ;E
        (char<?  char<)                         ;E
        (char=?  char=)                         ;E
        (char>=? char>=)                        ;E
        (char>?  char>)                         ;E
        (char? characterp)                      ;E
        (cons* list*)                           ;   T
        cos                                     ; RS
        (complex? numberp)                      ;E
        cons                                    ;E S
        (empty-stream? null)                    ;  S
        (eq? eq)                                ;E S
        (equal? equal)                          ;E
        (eqv? eql)                              ;E
        (even? evenp)                           ;E
        exp                                     ; RS
        expt                                    ; RS
        gcd                                     ; RS
        get                                     ;  S  ?
        (head car)                              ;  S
        (integer->char code-char)               ;E
        (integer? integerp)                     ;E
        (last-pair last)                        ; R
        (length list-length)                    ;E S
        lcm                                     ; R
        list                                    ;E S
        log                                     ; R
        max                                     ;E S
        (memv member)                           ;E
        min                                     ;E S
        (modulo mod)                            ; R
        (negative? minusp)                      ;E
        (newline terpri)                        ;E
        not                                     ;E S
        nth                                     ;   M
        (null? null)                            ;E S
        (number? numberp)                       ;E S
        (odd? oddp)                             ;E
        (pair? consp)                           ;E
        (positive? plusp)                       ;E
        princ                                   ;  S
        print                                   ;  S
        random                                  ;   M
        (rational? rationalp)                   ;E
        (remainder rem)                         ;E S
        reverse                                 ; R
        (reverse! nreverse)                     ;   MT
        sin                                     ; RS
        sqrt                                    ; RS
        (string-length length)                  ;E
        (string-ref char)                       ;E
        (string-ci<=? string-not-greaterp)      ; R
        (string-ci<?  string-lessp)             ; R
        (string-ci=?  string-equal)             ; R
        (string-ci>=? string-not-lessp)         ; R
        (string-ci>?  string-greaterp)          ; R
        (string<=? string<=)                    ;E
        (string<?  string<)                     ;E
        (string=?  string=)                     ;E
        (string>=? string>=)                    ;E
        (string>?  string>)                     ;E
        (string? stringp)                       ;E
        (substring subseq)                      ;E
        (symbol->string symbol-name)            ;E
        (symbol? symbolp)                       ;E S
        tan                                     ; R
        vector                                  ;E
        (vector-length length)                  ;E
        (vector-ref svref)                      ;E S
        (vector? simple-vector-p)               ;E
        write-char                              ;E
        (zero? zerop)                           ;E
        ))

;;; Missing inessential RRRS features:
;;;   exact and complex numbers
;;;   make-rectangular make-polar real-part imag-part magnitude angle
;;;   exact->inexact inexact->exact
;;;   string->number
;;;   make-string string-set! string-fill! string-copy substring-fill!
;;;   substring-move-right! substring-move-left!
;;;   vector-fill!
;;;   object-hash object-unhash
;;;   open-input-file open-output-file
;;;   close-input-port close-output-port
;;;   char-ready?
;;;   transcript-on transcript-off

;;;   T, MacScheme, and CL all have peek-char; should add this?

;;; For more efficient execution on a Lisp Machine, see this file:
;;;   z:>multilisp>emulator>emulator.lisp

;;;----- Top level  (need a definition for running in a real CL!)

;;; Read-eval-print loop.

#-Symbolics
(defun scheme ()
  (catch 'exit-scheme
    (in-scheme-package
     (lambda ()
       (progv *proc+arg-regs*
              '()              ;Make them all be unbound.  CLtL p. 112
         (iterate loop ()
           (catch 'reset-scheme
             (format *terminal-io* "~&==> ")
             (let ((form (read *terminal-io* nil '*eof-object*)))
               (cond ((eq form '*eof-object*)
                      (return-from s t))
                     (t
                      (setq * (scheme-eval form))
                      (format *terminal-io* "~&")
                      (scheme::write * *terminal-io*)))))
           (loop)))))))

#+Symbolics
(defun scheme ()
  (catch 'exit-scheme
    (fluid-let ((si:*command-loop-eval-function* #'scheme-eval)
                (si:*command-loop-print-function*
                  (lambda (values)
                    (zl:send zl:standard-output :fresh-line)
                    (cond ((or (null values) (not (null (cdr values))))
                           (format t
                              "~&[Strange, there should have been exactly one value.]~&")
                           (scheme::write values))
                          (t
                           (scheme::write (car values))))))
                (si:*read-form-edit-trivial-errors-p* nil)
                (si:*cp-prompt* "Scheme: "))
      (in-scheme-package
        (lambda ()
          (progv *proc+arg-regs*
                 '()
            (block done
              (iterate loop ()
                (catch 'reset-scheme
                  (si:lisp-command-loop *terminal-io*
                                        :name "Scheme Command Loop")
                  (return-from done (values)))
                (loop)))))))))


;;; Local Modes:
;;; Lisp lisp:lambda Indent:1
;;; Lisp zl:standard-value-let Indent:1
;;; Lisp catch Indent:1
;;; Lisp iterate Indent:2
;;; End:


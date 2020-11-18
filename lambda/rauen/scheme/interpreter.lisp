;;; -*- Mode:LISP; Package:SCHEME; Readtable:CL; Base:10 -*-
;;;
;;; Scheme interpreter


;; no abstraction for bindings

;;;----------------------------------------
;;; Procedures
;;;----------------------------------------

(defstruct (procedure
             (:constructor make-procedure (formal-parameters body environment))
             (:predicate procedure?)
             (:print-function print-procedure))
  formal-parameters
  body
  environment)

(defun print-procedure (procedure stream ignore)
  procedure
  (format stream "#<Compound Procedure>"))

(defstruct (primitive-procedure
             (:constructor make-primitive-procedure (name symbol-function))
             (:predicate primitive-procedure?)
             (:print-function print-primitive-procedure))
  name
  symbol-function)

(defun print-primitive-procedure (procedure stream ignore)
  (format stream "#<Primitive Procedure ~A>"
          (primitive-procedure-name procedure)))


;;;----------------------------------------
;;; Environments
;;;----------------------------------------

(defstruct (environment
             (:constructor make-environment (parent))
             (:predicate environment?)
             (:print-function print-environment))
  parent
  (bindings NIL))

(defun print-environment (env stream ignore)
  env
  (format stream "#<Environment>"))


(defvar *scheme-global-environment* (make-environment NIL)
  "The environment where all predefined Scheme names are bound.")

(defvar *scheme-initial-environment* (make-environment *scheme-global-environment*)
  "The environment that the user gets to play with.")

(defvar *undefined* (cons nil nil)
  "The value of an undefined variable.")

(defun add-binding (variable value env)
  (push (cons variable value) (environment-bindings env)))

(defun bind-lambda-list (lambda-list values env)
  (cond ((and (null lambda-list) (null values))
         NIL)
        ((or (null lambda-list) (null values))
         (error "Wrong number of arguments to lambda list."))
        ((atom lambda-list)
         (add-binding lambda-list values env))
        ((consp lambda-list)
         (add-binding (car lambda-list) (car values) env)
         (bind-lambda-list (cdr lambda-list) (cdr values) env))))


;;;----------------------------------------
;;; Eval and Apply
;;;----------------------------------------

(defun scheme-eval (exp env)
  (cond ((self-evaluating? exp) exp)
        ((macro? exp)           (scheme-eval (macro-expand exp) env))
        ((quoted? exp)          (text-of-quotation exp))
        ((variable? exp)        (lookup-variable-value exp env))
        ((definition? exp)      (eval-definition exp env))
        ((assignment? exp)      (eval-assignment exp env))
        ((lambda? exp)          (eval-lambda exp env))
        ((if? exp)              (eval-if exp env))
        ((conditional? exp)     (eval-cond (clauses exp) env))
        ((sequence? exp)        (eval-sequence (sequence-body exp) env))
        ((combination? exp)     (eval-combination exp env))
        (t
         (error "Unknown expression type -- EVAL" exp))))

(defun scheme-apply (procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((procedure? procedure)
         (let ((new-environment (make-environment
                                  (procedure-environment procedure))))
           (bind-lambda-list (procedure-formal-parameters procedure)
                             arguments
                             new-environment)
           (eval-sequence (procedure-body procedure) new-environment)))
        (t
         (error "Unknown procedure type -- APPLY" procedure))))


;;;----------------------------------------
;;; Self evaluating
;;;----------------------------------------

(defun self-evaluating? (exp)
  (or (numberp exp)))


;;;----------------------------------------
;;; Macro expansion
;;;----------------------------------------

(defun macro? (exp)
  (and (listp exp)
       (member (car exp) '(let let* letrec))))

(defun macro-expand (exp)
  (case (car exp)
    (LET (expand-let exp))
    (LET* (expand-let* exp))
    (LETREC (expand-letrec exp))
    (t exp)))

(defun expand-let (exp)
  (let ((variables (mapcar #'car (second exp)))
        (values    (mapcar #'cadr (second exp)))
        (body      (cddr exp)))
    `((LAMBDA ,variables ,@body) ,@values)))

(defun expand-letrec (exp)
  (let* ((bindings (second exp))
         (defines  (mapcar #'(lambda (binding)
                               `(DEFINE ,@binding))
                           bindings))
         (body     (cddr exp)))
    `(SEQUENCE ,@defines ,@body)))


;;;----------------------------------------
;;; Quote
;;;----------------------------------------

(defun quoted? (exp)
  (and (listp exp) (eq (car exp) 'QUOTE)))

(defun text-of-quotation (exp)
  (cadr exp))


;;;----------------------------------------
;;; Variables
;;;----------------------------------------

(defun variable? (exp)
  (symbolp exp))

(defun lookup-variable-value (variable env)
  (let ((binding (binding-in-environment variable env)))
    (if binding
        (cdr binding)
        (error "~S is unbound in the environment ~S." variable env))))

(defun binding-in-environment (variable env)
  (let ((binding (assoc variable (environment-bindings env)))
        (parent  (environment-parent env)))
    (cond (binding
           binding)
          (parent
           (binding-in-environment variable parent))
          (t
           NIL))))


;;;----------------------------------------
;;; Definition
;;;----------------------------------------

(defun definition? (exp)
  (and (listp exp) (eq (car exp) 'DEFINE)))

(defun eval-definition (exp env)
  (let ((variable (definition-variable exp))
        (value    (definition-value exp)))
    (define-variable! variable (scheme-eval value env) env)
    variable))

(defun define-variable! (variable value env)
  (let ((binding (assoc variable (environment-bindings env))))
    (if binding
        (rplacd binding value)
        (push (cons variable value) (environment-bindings env)))))

(defun definition-variable (exp)
  (if (variable? (second exp))
      (second exp)
      (car (second exp))))

(defun definition-value (exp)
  (if (variable? (second exp))
      (third exp)
      `(LAMBDA ,(cdr (second exp)) ,@(nthcdr 2 exp))))


;;;----------------------------------------
;;; Assignment
;;;----------------------------------------

(defun assignment? (exp)
  (and (listp exp) (eq (car exp) 'SET!)))

(defun eval-assignment (exp env)
  (let ((variable (assignment-variable exp))
        (value    (assignment-value exp)))
    (assign-variable! variable (scheme-eval value env) env)
    variable))

(defun assign-variable! (variable value env)
  (let ((binding (binding-in-environment variable env)))
    (if binding
        (rplacd binding value)
        (error "The variable ~S is unbound." variable))))

(defun assignment-variable (exp)
  (second exp))

(defun assignment-value (exp)
  (third exp))


;;;----------------------------------------
;;; Lambda
;;;----------------------------------------

(defun lambda? (exp)
  (and (listp exp) (eq (car exp) 'LAMBDA)))

(defun eval-lambda (exp env)
  (make-procedure (lambda-bindings exp)
                  (lambda-body exp)
                  env))

(defun lambda-bindings (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))


;;;----------------------------------------
;;; If
;;;----------------------------------------

(defun if? (exp)
  (and (listp exp) (eq (car exp) 'IF)))

(defun eval-if (exp env)
  (if (scheme-eval (if-predicate exp) env)
      (scheme-eval (if-consequent exp) env)
      (scheme-eval (if-alternate exp) env)))

(defun if-predicate (exp)
  (second exp))

(defun if-consequent (exp)
  (third exp))

(defun if-alternate (exp)
  (fourth exp))



;;;----------------------------------------
;;; Conditional
;;;----------------------------------------

(defun conditional? (exp)
  (and (listp exp) (eq (car exp) 'COND)))


;;;----------------------------------------
;;; Combination
;;;----------------------------------------

(defun combination? (exp)
  (listp exp))

(defun eval-combination (exp env)
  (scheme-apply (scheme-eval (combination-function exp) env)
                (mapcar #'(lambda (expr)
                            (scheme-eval expr env))
                        (combination-arguments exp))))

(defun combination-function (exp)
  (car exp))

(defun combination-arguments (exp)
  (cdr exp))


;;;----------------------------------------
;;; Sequence
;;;----------------------------------------

(defun sequence? (exp)
  (and (listp exp) (eq (car exp) 'SEQUENCE)))

(defun eval-sequence (exps env)
  (let ((frame (make-environment env)))
    (mapcar #'(lambda (var)
                (add-binding var *undefined* frame))
            (extract-defined-variables exps))
    (loop
      (case (length exps)
        (0 (return NIL))
        (1 (return (scheme-eval (car exps) frame)))
        (t (scheme-eval (car exps) frame)
           (setq exps (cdr exps)))))))

(defun extract-defined-variables (exps)
  (cond ((null exps)
         NIL)
        ((definition? (car exps))
         (cons (definition-variable (car exps))
               (extract-defined-variables (cdr exps))))
        (t
         (extract-defined-variables (cdr exps)))))

(defun sequence-body (exp)
  (cdr exp))


;;;----------------------------------------
;;; Primitive procedures
;;;----------------------------------------

(defun define-primitive-procedure (name function)
  (add-binding name
               (make-primitive-procedure name function)
               *scheme-global-environment*))

(defun apply-primitive-procedure (proc arguments)
  (apply (primitive-procedure-symbol-function proc)
         arguments))


;;;----------------------------------------
;;; Read-Eval-Print
;;;----------------------------------------

(defun rep ()
  (let ((counter 1))
    (loop
      (format t "~&[~D]==> " counter)
      (print (scheme-eval (read) *scheme-initial-environment*))
      (incf counter))))

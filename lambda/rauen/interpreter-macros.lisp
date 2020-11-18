;;; -*- Mode:LISP; Package:INTERPRETER; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;;
;;; INTERPRETER MACROS
;;;
;;; defun, defvar, defparameter, defconstant
;;;
;;; Something like this eventually belongs in SI: for use by the interpreter and
;;; the compiler.


;;; DEFUN
;;;
;;; (defun foo (args) body) does the following:
;;;
;;; Raises an error if foo is not a symbol.
;;;
;;; If the new evaluator is installed and active, puts #'(lambda (args) body) in the
;;; function cell of foo.  Using a closure rather than the text speeds up execution,
;;; especially of recursive functions, because the preprocessing is only done once.
;;;
;;; If the old evaluator is running, defun expands the way the old defun did, by calling
;;; si:process-defun-body and putting the result (text) in the function cell of foo.
;;; This definition, a named-lambda expression, will be properly handled by the new evaluator,
;;; if *ki-allow-apply-lambda-bagbiting* is T.
;;;
(defmacro defun (name lambda-list &body function-body)
  (cond ((not (symbolp name))
         `(FERROR "~S is an invalid function identifier." ',name))
        ((and (boundp 'user::*use-old-evaluator*) (not user::*use-old-evaluator*))
         (multiple-value-bind (body declarations doc-string)
             (si::gobble-declarations function-body t)
           `(PROGN
              (SETF (SYMBOL-FUNCTION ',name)
                    #'(INTERPRETER-NAMED-LAMBDA
                        ,name ,lambda-list ,@declarations (BLOCK ,name ,@body)))
              (SETF (DOCUMENTATION ',name :FUNCTION) ,doc-string)
              ',name)))
        (t
         `(progn
            (fdefine ',name
                     (si::process-defun-body ',name '(,lambda-list . ,function-body))
                     t)
            ',name))))


;;; DEFVAR
;;;
(defmacro defvar (si::name &optional (si::initial-value nil si::initial-value-supplied) si::documentation)
  `si::(PROGN (EVAL-WHEN (COMPILE LOAD EVAL)
            (PROCLAIM-SPECIAL ',name))
          (EVAL-WHEN (LOAD EVAL)
            (WHEN (AND (NOT (BOUNDP ',name)) ,initial-value-supplied)
              (SETF (SYMBOL-VALUE ',name) ,initial-value))
            (SETF (DOCUMENTATION ',name :VARIABLE) ,documentation))
          ',name))

(defmacro defvar-1 (&rest stuff)
  `(DEFVAR ,@stuff))


;;; DEFPARAMETER
;;;
(defmacro defparameter (si::name si::initial-value &optional si::documentation)
  `si::(progn (eval-when (compile load eval)
            (proclaim-special ',name))
          (eval-when (load eval)
            (setf (symbol-value ',name) ,initial-value)
            (setf (documentation ',name :variable) ,documentation))
          ',name))


;;; DEFCONSTANT
;;;
(defmacro defconstant (SI::name si::initial-value &optional documentation)
  `SI:(PROGN (EVAL-WHEN (COMPILE LOAD EVAL)
            (PROCLAIM-SPECIAL (QUOTE ,name)))
          (EVAL-WHEN (LOAD EVAL)
            (LET ((NEW-VALUE ,initial-value))
              (WHEN (BOUNDP (QUOTE ,name))
                (UNLESS (EQL NEW-VALUE (SYMBOL-VALUE (QUOTE ,name)))
                  (format *error-output* "~%WARNING:  Changing value of constant ~S" (QUOTE ,name))))
              (SETF (SYMBOL-VALUE (QUOTE ,name)) NEW-VALUE)
              (SETF (GET (QUOTE ,name) 'SYSTEM:SYSTEM-CONSTANT) T)
              (SETF (DOCUMENTATION (QUOTE ,name) :VARIABLE) ,documentation)
              (QUOTE ,name)))))

(defmacro defconst-1 (&rest stuff)
  `(DEFCONSTANT ,@stuff))

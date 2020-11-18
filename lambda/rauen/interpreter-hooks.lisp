;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-
;;;
;;; INTERPRETER-HOOKS.LISP
;;;
;;; Let the new interpreter slither into the old evaluator's wake.
;;;

(defvar *use-old-evaluator* t)

(defvar *inform-user* t)

(defun inform-user (stream format-string &rest format-args)
  (when *inform-user*
    (apply #'format stream format-string format-args)))

(defun make-hook-function (old-version new-version)
  #'(lambda (&rest args)
      (setq args (copylist args))
      (if *use-old-evaluator*
          (apply old-version args)
          (apply new-version old-version args))))

(defun install-hook (old-function new-function)
  (fdefine old-function
           (make-hook-function (fsymeval old-function) new-function)))

(defun eval-hook (old-version form &optional (nohook nil n-sup))
  (declare (ignore old-version nohook))
  (when n-sup
    (format t "~&Eval called with optional nohook."))
  (k-lisp:eval form))

(defun eval1-hook (old-version form &optional (nohook nil n-sup))
  (declare (ignore old-version nohook))
  (inform-user t "~&Eval1 called")
  (if n-sup
      (inform-user t " with optional nohook.")
      (inform-user t "."))
  (k-lisp:eval form))

(defun apply-lambda-hook (old-version fctn value-list &optional (environment nil env-sup))
  (cond ((and (null environment)
              (consp fctn)
              (eq (car fctn) 'lambda))
         (let ((closure (k-lisp:eval `(FUNCTION ,fctn))))
           (apply closure value-list)))
        ((not *ki-allow-apply-lambda-bagbiting*)
         (ferror "Bagbiting in APPLY-LAMBDA"))
        ((and (null environment)
              (consp fctn)
              (eq (car fctn) 'named-lambda))
         (inform-user *error-output*
                 "~&>>WARNING: NAMED-LAMBDA is not valid in Common Lisp.")
         (let ((closure (k-lisp:eval `(FUNCTION (LAMBDA ,@(cddr fctn))))))
           (apply closure value-list)))
        (t
         (let ((*use-old-evaluator* t))
           (inform-user *error-output*
                   "~&>>WARNING: Biting the bag with APPLY-LAMBDA.")
           (if env-sup
               (funcall old-version fctn value-list environment)
               (funcall old-version fctn value-list))))))

(defun evalhook-hook (old-version form *evalhook* *applyhook* &optional (env nil env-sup))
  (declare (ignore old-version))
  ; (format t "~&Called evalhook.")
  (if env-sup
      (k-lisp:evalhook form *evalhook* *applyhook* env)
      (k-lisp:evalhook form *evalhook* *applyhook*)))

(defun applyhook-hook (old-version fn args *evalhook* *applyhook* &optional (env nil env-sup))
  (declare (ignore old-version))
  (if env-sup
      (k-lisp:applyhook fn args *evalhook* *applyhook* env)
      (k-lisp:applyhook fn args *evalhook* *applyhook*)))

(defun eval-special-ok-hook (old-version form &optional (nohook nil n-sup))
  (declare (ignore old-version nohook))
  (when n-sup
    (format t "~&Eval-special-ok called with optional nohook."))
  (interpreter::eval-special-ok form))

(defun fsymeval-in-environment-hook (old-version symbol env check-symbol-function)
  "This behaves incorrectly for lexical functions"
  (if (interp::nframe-p env)
      (let ((lexical-binding (interp::lookup-binding-in-environment symbol env :function)))
        (if (not lexical-binding)
            (if check-symbol-function (symbol-function symbol) NIL)
            (case (interp::preprocessor-funmac-binding-type lexical-binding)
              (function (error "internal: lexical value of function not yet available"))
              (macro (interp::preprocessor-funmac-binding-value lexical-binding)))))
      (funcall old-version symbol env check-symbol-function)))

;;; This variable prevents re-hooking.

(defvar *hooks-installed*)

(eval-when (load global:eval)
  (when (not (boundp '*hooks-installed*))
    (install-hook 'si::eval            'eval-hook)
    (install-hook 'si::eval1           'eval1-hook)
    (install-hook 'si::apply-lambda    'apply-lambda-hook)
    (install-hook 'si::evalhook        'evalhook-hook)
    (install-hook 'si::applyhook       'applyhook-hook)
    (install-hook 'si::eval-special-ok 'eval-special-ok-hook)
    (install-hook 'si::fsymeval-in-environment 'fsymeval-in-environment-hook)
    (setq *hooks-installed* t)
    ))

(eval-when (load global:eval)
  (setf (macro-function 'global:defun)
        (macro-function 'k-lisp:defun))
  (setf (macro-function 'global:defvar)
        (macro-function 'k-lisp:defvar))
  (setf (macro-function 'si::defvar-1)
        (macro-function 'interpreter::defvar-1))
  (setf (macro-function 'global:defparameter)
        (macro-function 'k-lisp:defparameter))
  (setf (macro-function 'global:defconstant)
        (macro-function 'k-lisp:defconstant))
  (setf (macro-function 'si::defconst-1)
        (macro-function 'interpreter::defconst-1)))

(defun new () (setq *use-old-evaluator* nil))

(defun old () (setq *use-old-evaluator* t))

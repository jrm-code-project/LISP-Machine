;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-


;;;; Compile Time Environment


(defstruct compiler-env
  (fcn  #'(lambda (symbol environment) (obtain-fvariable environment symbol)))
  (fenv (make-table 'fenv))
  (venv (make-table 'venv)))


;;; Get the lexically apparent variable (if any) with name NAME.

(defun obtain-variable (env name)
  (car (table-entry (compiler-env-venv env) name)))

(defun obtain-fvariable (env name)
  (car (table-entry (compiler-env-fenv env) name)))


;;; Add variables VARS to the table.

(defun bind-variables (env vars)
  (let ((table (compiler-env-venv env)))
    (mapc #'(lambda (var)
              (if var
                  (table-push table (variable-name var) var)))
          vars)))

(defun bind-fvariables (env vars)
  (let ((table (compiler-env-fenv env)))
    (mapc #'(lambda (var)
              (if var
                  (table-push table (variable-name var) var)))
          vars)))

(defun fbind (name expander-fcn env)
  (table-push (compiler-env-fenv env) name expander-fcn))


;;; Remove the variables from the table.

(defun unbind-variables (env vars)
  (let ((table (compiler-env-venv env)))
    (mapc #'(lambda (var)
              (if var
                  (let ((entry (table-entry table (variable-name var))))
                    (cond ((and entry (eq var (car entry)))
                           (table-pop table (variable-name var)))
;                        ((some #'(lambda (v)
;                                 (and (variable-p v)
;                                      (eq (variable-name v)
;                                          (variable-name var))
;                                      (not (eq v var))))
;                               vars)
;                         (push 'duplicate (variable-flags var)))
                        (t
                         (bug "variable ~S not in venv ~S" var env))))))
          vars)))

(defun unbind-fvariables (env vars)
  (let ((table (compiler-env-fenv env)))
    (mapc #'(lambda (var)
              (if var
                  (let ((entry (table-entry table (variable-name var))))
                    (cond ((and entry (eq var (car entry)))
                           (table-pop table (variable-name var)))
                        (t
                         (bug "variable ~S not in fenv ~S" var env))))))
          vars)))


(defun unfbind (name env)
  (table-pop (compiler-env-fenv env) name))

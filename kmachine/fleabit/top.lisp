;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; COMPILE-FILE and Top Level Forms

(export 'nlisp:compile-file 'nlisp)

(defstruct (ncompiled-function
                (:print-function print-ncompiled-function))
  name
  starting-address
  entry-points
  local-refs
  refs
  length
  code
  immediates
  callees
  load-time-evals
  )

(defun print-ncompiled-function (cfun stream depth)
  (declare (ignore depth))
  (format stream "#<NCOMPILED-FUNCTION ~s ~o>"
          (ncompiled-function-name cfun) (si:%pointer cfun)))


(defun nca (lambda  &optional (env (make-compiler-env)))
  (really-compile lambda env))

(defun really-compile (exp env)
  ;; given an expression (a named lambda) and an environment
  ;; return an ncompiled-function object
  (let ((tree (make-code-tree exp env)))
    (analyze tree)
    (generate tree)))


(defun c (defun)
  (if (eq (car defun) 'DEFUN)
      (nca `(NAMED-LAMBDA ,(second defun) ,(third defun)
                          (BLOCK ,(second defun)
                            ,@(cdddr defun))))
    (let ((new-defun (nlisp::macroexpand-1 defun)))
      (if (eq new-defun defun)
          (error "~%This is not a defun:~%~a" defun)
        (c new-defun)))))


(defvar *env* nil "Compile time environment")

(defvar *kbin-output-stream*)

(defparameter *make-kenv-file* t
  "T if a KENV file should be made for loading into the lambda
for cross compiling")

(defvar *kenv-forms*)

(defun dump-to-kenv-file (form)
  (when *make-kenv-file*
    (push form *kenv-forms*)))


;;; this doesn't belong here
(DEFUN PATHNAME-DEFAULT-BINARY-FILE-TYPE (PATHNAME)
  "Given a pathname, return the default binary file type (possibly canonical) to use with it.
This is computed from the SYSTEM which the pathname belongs to."
  (OR (zl:SEND (zl:SEND PATHNAME :GENERIC-PATHNAME) :GET :DEFAULT-BINARY-FILE-TYPE)
      "KFASL"))

(defun nlisp:compile-file (input-file &optional output-file)
  (let ((si:*target-features* si:*falcon-features*)
        (inpath (fs:merge-pathname-defaults input-file fs:load-pathname-defaults))
        ;; $$$ Added *bug-report-recipient-system* binding. <01-Nov-88 JIM>
        (eh:*bug-report-recipient-system* "FLEABIT-COMPILER"))
    (with-open-file (input-stream inpath)
      (format t "~%Compiling ~a" (pathname input-stream))
      (let* ((output-version (zl:send (zl:send input-stream :truename) :version))
             (outpath
               (cond ((typep output-file 'pathname)
                      (if (zl:send output-file :version)
                          output-file
                        (zl:send output-file :new-pathname
                                 :version  output-version)))
                     (output-file
                      (fs:merge-pathname-defaults
                        output-file inpath
                        (pathname-default-binary-file-type
                          (zl:send inpath :generic-pathname))
                        output-version))
                     (t
                      (zl:send inpath :new-pathname
                               :type (pathname-default-binary-file-type
                                       (zl:send inpath :generic-pathname))
                               :version output-version)))))
        (with-open-file (*kbin-output-stream* outpath :direction :output)
          (let ((*kenv-forms* '())
                (*local-declarations* nil)
                (*env* (make-compiler-env))
                (*package* (let ((package-spec (progn (fs:read-attribute-list inpath input-stream)
                                                      (zl:send inpath :get :package))))
                             (or (if package-spec
                                     (find-package package-spec))
                                 *package*)))
                (*readtable* (si:find-readtable-named
                               (or (zl:send inpath :get :readtable)
                                   :cl)))
                (*read-base* (or (zl:send inpath :get :base) 10.)))
            (dump-to-kenv-file `(IN-PACKAGE ',(package-name *package*)))
            (do* ((eof (list ()))
                  (form (read input-stream nil eof)
                        (read input-stream nil eof)))
                 ((eq form eof))
              (process-toplevel-form form))
            (fasdump:fasd-eof *kbin-output-stream*)
            (when *make-kenv-file*
              (zl:dump-forms-to-file (zl:send outpath :new-pathname
                                              :type "KENV")
                                     (nreverse *kenv-forms*)
                                     `(:package ,(package-name *package*)
                                       :readtable ,(car (si:rdtbl-names *readtable*))
                                       )))))))))


;;; The compiler recognizes several top level forms.
;;; Functions to compile them are kept in this hash table.

(defvar *top-level-form-handler-table* (make-table '*top-level-form-handler-table*))

(defmacro define-toplevel-form-handler (name pattern &body body)
  (let ((sym (concatenate-symbol 'toplevel/ name))
        (form (gensym 'form)))
    `(progn
       (defun ,sym (,form)
         (destructure ((,pattern (cdr ,form)))
                      ,@body))
       (setf (table-entry *top-level-form-handler-table* ',name)
             #',sym))))

(defun process-toplevel-form (form)
  (if (not (consp form))
      (warn "The atom ~s was found at toplevel, this would do nothing" form)
    (let ((handler (table-entry *top-level-form-handler-table* (car form))))
      (if handler
          (funcall handler form)
        (let ((new-form (nlisp::macroexpand-1 form *env*)))
          (if (eq new-form form)
              (arrange-for-eval form)
            (process-toplevel-form new-form)))))))

(defun arrange-for-eval (form)
  (dump-to-kenv-file form)
  (fasdump:fasd-eval *kbin-output-stream* form))

(define-toplevel-form-handler DEFUN (name lambda-list . body)
  (fasdump:fasd-defun (nca `(NAMED-LAMBDA ,name ,lambda-list
                                          (BLOCK ,name . ,body))
                           *env*)
                      *kbin-output-stream*))

(define-toplevel-form-handler PRIMS:DEFMACRO macrobody
  (dump-to-kenv-file `(PRIMS:DEFMACRO . ,macrobody))
  (let ((macrofun (si::expand-defmacro macrobody nil))) ;** env
    ;; $$$ Put back fasdumping of macros. <21-Nov-88 wkf>
    (fasdump:fasd-defmacro (nca `(named-lambda . ,(cdr macrofun)) *env*)
                           *kbin-output-stream*)
    (fbind (car macrobody) (cons 'MACRO macrofun) *env*)))

(define-toplevel-form-handler PRIMS:DEFSUBST (name lambda-list . body)
  (let ((source `(NAMED-LAMBDA ,name ,lambda-list . ,body)))
  (put-local-declaration name 'PRIMS:SUBST source)
  (dump-to-kenv-file `(NC:DEF-DECLARATION ,name PRIMS:SUBST ',source))
  (fasdump:fasd-defsubst (nca source *env*) *kbin-output-stream* source)))

(define-toplevel-form-handler DEFAFUN (name lambda-list . body)
  (fasdump:fasd-defafun (assemble-instruction-list name (cons name body)
                                                   `((,(length lambda-list) . ,name)))
                        *kbin-output-stream*))

(define-toplevel-form-handler DEFCONSTANT (name value . optional-doc-string)
  (eval `(DEFCONSTANT ,name ,value . ,optional-doc-string)) ;crock
  (dump-to-kenv-file `(DEFCONSTANT ,name ,value . ,optional-doc-string ))
  (apply #'fasdump:fasd-defconstant *kbin-output-stream* name value optional-doc-string))

;;; I don't think defparameter should be in prims
(define-toplevel-form-handler PRIMS:DEFPARAMETER (name value . optional-doc-string)
  (si:proclaim-special name) ;crock
  (dump-to-kenv-file `(DEFPARAMETER ,name ,value . ,optional-doc-string ))
  (apply #'fasdump:fasd-defparameter *kbin-output-stream* name value optional-doc-string))

;;; I don't think defvar should be in prims
(define-toplevel-form-handler PRIMS:DEFVAR (name . optional-value-and-doc-string)
  (si:proclaim-special name) ;crock
  (dump-to-kenv-file `(DEFVAR ,name . ,optional-value-and-doc-string ))
  (apply #'fasdump:fasd-defvar *kbin-output-stream* name optional-value-and-doc-string))

;;; I don't think we need this - JRM
;(define-toplevel-form-handler DEFSETF rest
;  (eval `(DEFSETF . ,rest))
;  (format t "~&FasDump DEFSETF"))


(define-toplevel-form-handler PROGN body
  (dolist (form body)
    (process-toplevel-form form)))

(define-toplevel-form-handler IN-PACKAGE rest
  (dump-to-kenv-file `(IN-PACKAGE . ,rest))
  (eval `(IN-PACKAGE . ,rest))
  (fasdump:fasd-in-package *kbin-output-stream* rest))

(define-toplevel-form-handler EVAL-WHEN (when . body)
  (if (member 'compile when)
      (eval `(progn . ,body)))
  (if (member 'load when)
      (dolist (form body)
        (process-toplevel-form form))))

(define-toplevel-form-handler COMPILER-LET  (bindlist . body)
  (progv (mapcar #'(lambda (x) (if (atom x) x (car x))) bindlist)
         (mapcar #'(lambda (x) (if (atom x) nil (eval (cadr x)))) bindlist)
    (dolist (form body)
      (process-toplevel-form form))))


;----------------------------------------------------------------
;;; Editor Interface

;;; ||| Moved the editor functions to falcon:k;zwei-coms <04-Nov-88 wkf>

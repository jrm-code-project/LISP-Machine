;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; COMPILE-FILE and Top Level Forms

(export 'nlisp:compile-file 'nlisp)

(defun nca (lambda  &optional (env (make-compiler-env)))
  (really-compile lambda env))

(defun really-compile (exp env)
  (declare (values code local-refs refs immediates entry-points))
  (let ((tree (make-code-tree exp env)))
    (analyze tree)
    (multiple-value-bind (instructions entry-points)
        (generate tree)
      (assemble-instruction-list instructions entry-points))))


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


;;; this doesn't belong here
(DEFUN PATHNAME-DEFAULT-BINARY-FILE-TYPE (PATHNAME)
  "Given a pathname, return the default binary file type (possibly canonical) to use with it.
This is computed from the SYSTEM which the pathname belongs to."
  (OR (zl:SEND (zl:SEND PATHNAME :GENERIC-PATHNAME) :GET :DEFAULT-BINARY-FILE-TYPE)
      "KFASL"))

(defun nlisp:compile-file (input-file &optional output-file)
  (let ((inpath (fs:merge-pathname-defaults input-file fs:load-pathname-defaults)))
    (let ((outpath
            (cond ((typep output-file 'pathname)
                   (if (zl:send output-file :version)
                       output-file
                     (zl:send output-file :new-pathname
                              :version :newest)))
                  (output-file
                   (fs:merge-pathname-defaults
                     output-file inpath
                     (pathname-default-binary-file-type
                       (zl:send inpath :generic-pathname))
                     :newest))
                  (t
                   (zl:send inpath :new-pathname
                            :type (pathname-default-binary-file-type
                                    (zl:send inpath :generic-pathname))
                            :version :newest)))))
      (with-open-file (input-stream inpath)
        (with-open-file (*kbin-output-stream* outpath :direction :output)
          (let ((*env* (make-compiler-env))
                (*package* (let ((package-spec (progn (fs:read-attribute-list inpath input-stream)
                                                      (zl:send inpath :get :package))))
                             (or (if package-spec
                                     (find-package package-spec))
                                 *package*)))
                (*readtable* (si:find-readtable-named
                                 (or (zl:send inpath :get :readtable)
                                     :cl))))
            (do* ((eof (list ()))
                  (form (read input-stream nil eof)
                        (read input-stream nil eof)))
                 ((eq form eof))
              (process-toplevel-form form)))
          (fasdump:fasd-eof *kbin-output-stream*))))))

;;; For each top level form that the compiler specially recognizes, there will probably
;;; be a corresponding fasl op.

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
  form
  (fasdump:fasd-eval *kbin-output-stream* form))

(define-toplevel-form-handler DEFUN (name lambda-list . body)
  (multiple-value-bind (code local-refs refs immediates entry-points)
      (nca `(named-lambda ,name ,lambda-list (block ,name . ,body)) *env*)
    (fasdump:fasd-defun *kbin-output-stream*
      name
      local-refs
      refs
      immediates
      entry-points
      code)))

(define-toplevel-form-handler PRIMS:DEFSUBST (name lambda-list . body)
  (eval `(GLOBAL::DEFSUBST ,name ,lambda-list . ,body))   ;crock
  (let ((source `(named-lambda ,name ,lambda-list . ,body)))
    (multiple-value-bind (code local-refs refs immediates entry-points)
        (nca source *env*)
    (fasdump:fasd-defsubst *kbin-output-stream*
      name
      source
      local-refs
      refs
      immediates
      entry-points
      code))))

(define-toplevel-form-handler DEFAFUN (name lambda-list . body)
  (multiple-value-bind (code local-refs refs immediates entry-points)
      (assemble-instruction-list (cons name body) `((,(length lambda-list) . ,name)))
    (fasdump:fasd-defafun *kbin-output-stream*
      name
      local-refs
      refs
      immediates
      entry-points
      code)))

;;; value optional
;;; doc string (also optional)
(define-toplevel-form-handler DEFCONSTANT (name value)
  (eval `(DEFCONSTANT ,name ,value)) ;crock
  (fasdump:fasd-defconstant *kbin-output-stream* name value ""))

;;; I don't think we need this - JRM
;(define-toplevel-form-handler DEFSETF rest
;  (eval `(DEFSETF . ,rest))
;  (format t "~&FasDump DEFSETF"))

(define-toplevel-form-handler PRIMS:DEFMACRO macrobody
  (let ((macrofun (si::expand-defmacro macrobody nil))) ;** env
    (multiple-value-bind (code local-refs refs immediates entry-points)
        (nca `(named-lambda . ,macrobody) *env*)
      (fasdump:fasd-defmacro *kbin-output-stream*
        (car macrobody)
        local-refs
        refs
        immediates
        entry-points
        code))
    (fbind (car macrobody) (cons 'MACRO macrofun) *env*)
    ))

(define-toplevel-form-handler PROGN body
  (dolist (form body)
    (process-toplevel-form form)))

(define-toplevel-form-handler IN-PACKAGE rest
;  (eval (cons 'IN-PACKAGE rest))
  (fasdump:fasd-in-package *kbin-output-stream* rest))

(define-toplevel-form-handler EVAL-WHEN (when . body)
  (if (member 'compile when)
      (eval `(progn . ,body)))
  (if (member 'load when)
      (arrange-for-eval `(progn . ,body))))



;----------------------------------------------------------------
;;; Editor Interface


;;; more here
(defun compile-region-for-new-processor (form)
  (cc form))

(zwei::DEFCOM com-compile-region-for-new-processor "" ()
  (zwei::COMPILE-DEFUN-INTERNAL #'compile-region-for-new-processor "Hacking" "hacked.")
  zwei::DIS-NONE)


(zwei::COMMAND-STORE 'com-compile-region-for-new-processor #\s-sh-C zwei::*ZMACS-COMTAB*)

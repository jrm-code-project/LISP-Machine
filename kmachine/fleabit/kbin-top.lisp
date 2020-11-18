;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Top Level Functions and Temporary Sleazy Fasdumper

(export 'nlisp:compile-file 'nlisp)

(defun nca (lambda  &optional (env (make-compiler-env)))
  (really-compile lambda env))

(defun really-compile (exp env)
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
    (let ((new-defun (nlisp:macroexpand-1 defun)))
      (if (eq new-defun defun)
          (error "~%This is not a defun:~%~a" defun)
        (c new-defun)))))


(defvar *env* nil "Compile time environment")

(defvar *kbin-output-stream*)

(defun kbin-out (thing)
  ;; is this cool?
  ;; print with package
  (let ((*package* nil)
        (*readtable* si:common-lisp-readtable))
    (cond ((null thing) (princ "()" *kbin-output-stream*))
          ((consp thing) (prin1 thing *kbin-output-stream*))
          (t (print thing *kbin-output-stream*)))))

(defun kbin-write-byte (byte)
  (write-byte byte *kbin-output-stream*))

(defun kbin-write-instruction (inst)
  (kbin-write-byte (ldb (byte 8.  0.) inst))
  (kbin-write-byte (ldb (byte 8.  8.) inst))
  (kbin-write-byte (ldb (byte 8. 16.) inst))
  (kbin-write-byte (ldb (byte 8. 24.) inst))
  (kbin-write-byte (ldb (byte 8. 32.) inst))
  (kbin-write-byte (ldb (byte 8. 40.) inst))
  (kbin-write-byte (ldb (byte 8. 48.) inst))
  (kbin-write-byte (ldb (byte 8. 56.) inst)))

(defconstant FASL-OP/DEFUN    0)
(defconstant FASL-OP/MACRO    1)
(defconstant FASL-OP/SUBST    2)
(defconstant FASL-OP/CONSTANT 3)
(defconstant FASL-OP/EVAL     4)
(defconstant n-fasl-ops 5)

;;; this doesn't belong here
(DEFUN PATHNAME-DEFAULT-BINARY-FILE-TYPE (PATHNAME)
  "Given a pathname, return the default binary file type (possibly canonical) to use with it.
This is computed from the SYSTEM which the pathname belongs to."
  (OR (zl:SEND (zl:SEND PATHNAME :GENERIC-PATHNAME) :GET :DEFAULT-BINARY-FILE-TYPE)
      "KBIN"))

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
              (comp-form form))))))))

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

(defun comp-form (form)
  (if (not (consp form))
      (warn "The atom ~s was found at toplevel, this would do nothing" form)
    (let ((handler (table-entry *top-level-form-handler-table* (car form))))
      (if handler
          (funcall handler form)
        (let ((new-form (nlisp:macroexpand-1 form *env*)))
          (if (eq new-form form)
              (arrange-for-eval form)
            (comp-form new-form)))))))

(defun arrange-for-eval (form)
  (kbin-write-byte FASL-OP/EVAL)
  (kbin-out form))

(define-toplevel-form-handler DEFUN (name lambda-list . body)
  (multiple-value-bind (code local-refs refs immediates)
      (nca `(named-lambda ,name ,lambda-list (block ,name . ,body)) *env*)
    (kbin-write-byte FASL-OP/DEFUN)
    (kbin-out name)
    (kbin-out lambda-list)
    (kbin-out local-refs)
    (kbin-out refs)
    (kbin-out immediates)
    (kbin-out (length code))
    (map nil #'kbin-write-instruction code)))

(define-toplevel-form-handler PRIMS:DEFSUBST (name lambda-list . body)
  (eval `(GLOBAL::DEFSUBST ,name ,lambda-list . ,body))   ;crock
  (multiple-value-bind (code local-refs refs immediates)
      (nca `(named-lambda ,name ,lambda-list . ,body) *env*)
    (kbin-write-byte FASL-OP/SUBST)
    (kbin-out name)
    (kbin-out lambda-list)
    (kbin-out body)
    (kbin-out local-refs)
    (kbin-out refs)
    (kbin-out immediates)
    (kbin-out (length code))
    (map nil #'kbin-write-instruction code)))

(define-toplevel-form-handler DEFAFUN (name lambda-list . body)
  (multiple-value-bind (code local-refs refs immediates)
      (assemble-instruction-list (cons name body) `((,(length lambda-list) . ,name)))
    (kbin-write-byte FASL-OP/DEFUN)
    (kbin-out name)
    (kbin-out lambda-list)
    (kbin-out local-refs)
    (kbin-out refs)
    (kbin-out immediates)
    (kbin-out (length code))
    (map nil #'kbin-write-instruction code)))

;;; value optional
;;; doc string
(define-toplevel-form-handler DEFCONSTANT (name value)
  (eval `(DEFCONSTANT ,name ,value)) ;crock
  (kbin-write-byte FASL-OP/EVAL)
  (kbin-out `(DEFCONSTANT ,name ,value)))

(define-toplevel-form-handler DEFSETF rest
  (eval `(DEFSETF . ,rest))
  (kbin-write-byte FASL-OP/EVAL)
  (kbin-out `(DEFSETF . ,rest)))

(define-toplevel-form-handler PRIMS:DEFMACRO macrobody
  (let ((macrofun (si:expand-defmacro macrobody nil)))  ;** env
    (multiple-value-bind (code local-refs refs immediates)
        (nca `(named-lambda . ,macrobody) *env*)
      (kbin-write-byte FASL-OP/MACRO)
      (kbin-out (first macrobody))
      (kbin-out (second macrobody))
      (kbin-out local-refs)
      (kbin-out refs)
      (kbin-out immediates)
      (kbin-out (length code))
      (map nil #'kbin-write-instruction code))
    (fbind (car macrobody) (cons 'MACRO macrofun) *env*)        ;??bind?
    ))

(define-toplevel-form-handler PROGN body
  (dolist (form body)
    (comp-form form)))

(define-toplevel-form-handler IN-PACKAGE rest
  (eval (cons 'IN-PACKAGE rest))
  (arrange-for-eval `(IN-PACKAGE . ,rest)))

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

(zwei:DEFCOM com-compile-region-for-new-processor "" ()
  (zwei:COMPILE-DEFUN-INTERNAL #'compile-region-for-new-processor "Hacking" "hacked.")
  zwei:DIS-NONE)


(zwei:COMMAND-STORE 'com-compile-region-for-new-processor #\s-sh-C zwei:*ZMACS-COMTAB*)

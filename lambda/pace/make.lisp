;;; -*- Mode:LISP; Package:(MAKE USE LISP); Base:10; Readtable:CL -*-

(defstruct (file-info :named)
  pathname
  plist)

(defstruct (sysdef :named)
  name
  modules
  plist
  )

; (define-system foo
;   (:pathname-default "sys:foo;")
;   (:module defs ("d1" "d2"))
;   (:module main ("m1" "m2"))
;   (:module top ("t1" "t1")))

(define-system foo
   (:pathname-default "sys:foo;")
   (:module defs ("d1" "d2"))
   (:module main ("m1" "m2"))
   (:module top ("t1" "t1")))

(defmacro define-system (name &rest args)
  (let ((pathname-default (or (cadr (assoc :pathname-default args))
                              *default-pathname-defaults*)))
    `(setf (getf (symbol-plist ',name) '
    `(make-sysdef
       :name ',name
       :modules ,(let ((modules '()))
                   (dolist (clause args)
                     (when (eq (car clause) :module)
                       (push (mapcar #'(lambda (name)
                                         (make-file-info
                                           :pathname (merge-pathnames name pathname-default)
                                           :plist nil))
                                     (caddr clause))
                             modules)))
                   (reverse modules))
       :plist nil)))

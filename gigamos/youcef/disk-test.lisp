;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;

(defparameter *buffer-area* nil)
(defparameter *buffer-as-array* nil)

(defun get-disk-area ()
  (multiple-value-setq
    (*buffer-as-array* *buffer-area*)
    (nubus-stuff:make-disk-io-buffer 16.))
  (loop)
  )

(defun init-iopb ()
  (insert-share-iopb)
  (loop)
  )

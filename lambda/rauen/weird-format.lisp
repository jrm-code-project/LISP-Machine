;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-
;;;
;;; WEIRD-FORMAT.LISP


(defun self (x)
  (format nil "~V,,,'*<~>" x))

(defun add (x y)
  (length (format nil "~V,,,'*<~>~V,,,'*<~>" x y)))

(defun sum (&rest args)
  (format nil "~{~V,,,'*<~>~}" args))

(defun multiply (x y))

(defvar *ones*
        (let ((ones (cons 1 nil)))
          (rplacd ones ones)
          ones))
~{

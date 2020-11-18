;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

;;(defvar a)

(defun foo (&optional (a a))
  (bar))

(defun bar ()
  (1+ a))

(defun baz ()
  (goo (let ((a 9))
       (bar))
       (bar)))

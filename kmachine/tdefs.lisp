;;; -*- Mode:LISP; Package:LISP-INTERNALS -*-

;;; this is a test
;;; delete this file

(defstruct foo a b c)

(defun test (s)
  (setf (foo-a s) (foo-b s)))


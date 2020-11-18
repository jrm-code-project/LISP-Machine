;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-



(defstruct foo
  a
  b
  c)

(defun bar ()
  (let ((my-foo (make-foo)))
    (foo-a my-foo)))

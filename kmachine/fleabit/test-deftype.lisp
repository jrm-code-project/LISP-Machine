;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10 -*-



(deftype frob () '(satisfies frob-p))


(defun foo (x)
  (when (typep x 'frob)
    (print "x is a frob")))

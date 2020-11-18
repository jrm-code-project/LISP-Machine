;;; -*- Mode:LISP; Package:K2; Compile-In-Roots:("K-GLOBAL"); Base:10; Readtable:CL -*-

(defun identify-processor (x)
  (select-processor
    (:k
      (setq x 33)
      (loop))
    (:lambda
      (setq x 44)
      x)))

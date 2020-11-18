;;; -*- Mode:LISP; Package:SIM; Base:10 -*-


(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))

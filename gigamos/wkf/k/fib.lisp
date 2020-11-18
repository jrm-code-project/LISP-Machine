;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(defun fib (x)
  (cond ((zerop x) 1)
        ((= x 1) 1)
        (t (fib-internal 1 1 (- x 2)))))

(defun fib-internal (x y n)
  (if (zerop n)
      (+ x y)
    (fib-internal y (+ x y) (1- n))))

(defun golden (n)
  (/ (fib n) (fib (1- n))))

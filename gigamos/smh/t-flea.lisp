;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(defun callee (x y)
  (add x y))

(defun add (x y)
  (+ x y))

(defun one+ (z)
  (1+ z))

(defun caller1 (x)
  (callee x x))

(defun caller2 (x)
  (callee (one+ x) x))

(defun caller3 (x)
  (callee (one+ x) (one+ x)))

(defun caller4 (x)
  (callee (one+ (one+ x)) (one+ x)))

(defun slabel (x y)
  (labels ((lab1 (a b)
                 (cons (one+ a) (one+ b)))
           (lab2 (a b)
                 (cons (one+ b) (one+ a))))
    (list (lab1 x y) (lab2 x y) (lab1 x y))))

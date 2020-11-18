;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-





















(defun foo (arg) (macro-for-k arg))

(defun bar (arg) (foo arg 1))

(defun lose () (print))

(defun lose (x f)
  (getf (gethash f (compiler:compilation-environment-plist-hashtab x))
        'compiler:compiler-arglist))

(defun lose (x)
  (getf x
        'compiler:compiler-arglist
        'foo))

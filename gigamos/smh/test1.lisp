;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

(defconstant const-for-k 27)

(defvar var-for-k 23)

(defmacro macro-for-k (x)
  (format t "~%Expanding MACRO-FOR-K ~s" x)
  `(cons ,x ,x))

;; (eval-when (compile) (print #.const-for-k))

(defun foo (x) (macro-for-k x))

(defdecl foo bar goo)

(cl:defstruct unreal foo bar)

(defun bar(x) (1+ (unreal-bar x)))

(barf bag)

(proclaim '(fixnum gnobble))

(in-package 'luser)

(proclaim '(special gnobble))

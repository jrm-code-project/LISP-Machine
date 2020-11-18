;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(defun get-stuff-from-lam6 ()
  (dolist (x '(illustrate interlisp lcomp lph xref yaps nick))
    (fs:copy-directory (format nil "lam6:~a;*.*#>" x)
                       (format nil "lm:~A;" x))))

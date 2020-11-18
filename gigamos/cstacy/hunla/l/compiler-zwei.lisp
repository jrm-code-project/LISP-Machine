;;; -*- Mode:LISP; Package:CL-ZWEI; Readtable:CL; Base:10 -*-

;;;; Interface between the compilers and Zwei.

(defcom com-compile-region-dwim
        "Crosscompile the current region or defun, using the appropriate compiler."
        ()
  (let ((nc:*debug-flag* (cons :post nc:*debug-flag*)))
    (let-if zwei:*numeric-arg-p* ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
            (crosscompile-region-1 nil))))


(defun pick-compiler

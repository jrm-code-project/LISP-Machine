;;; -*- Mode:LISP; Package:K-DEBUG; Base:10; Readtable:CL -*-

;;; A debug interface for the K machine.





(defun debug-arg (n)
  (boot::read-boot-vector n))

(defmacro debug-call (f n-args)
  (labels ((make-arglist (n argl)
             (if (zerop n)
                 argl
                 (make-arglist (1- n)
                               (cons `(DEBUG-ARG ,(1- n)) argl)))))
    `(,F ,@(make-arglist n-args '()))))


(defun debug-handler ()
  (debug-main-loop))

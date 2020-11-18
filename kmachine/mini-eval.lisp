;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-

;;;; Mini Evaluator

(defun eval (form)
  (cond ((symbolp form) (symbol-value form))
        ((atom form) form)
        (t (case (car form)
             (QUOTE (second form))
             (SETQ (set (second form) (eval (third form))))
             (IF    (if (eval (first form))
                        (eval (second form))
                      (eval (third form))))
             (t (apply (car form)
                       (mapcar #'eval (cdr form))))))))



(defun toplevel ()
  (loop (print (eval (read)))
        (terpri)))

(defun error (msg &optional a b c d e f g h i j)
  (print msg)
  (loop))

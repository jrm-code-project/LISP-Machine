;;; -*- Mode:LISP; Package:USER -*-


(defun foo ()
  (tagbody
      loop-tag
         (print 'foo)
         (bar #'(lambda () (go loop-tag)))))


(defun bar (f) (funcall f))

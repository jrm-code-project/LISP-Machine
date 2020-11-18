;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;; TEST-DEFSTRUCT.LISP

(defstruct (ship (:constructor make-ship (x-position y-position speed)))
  x-position
  y-position
  speed)

(defun foo (x)
  (setq x (make-ship 6 7 8))
  (loop))

(defun bar (x)
  (setq x (make-nframe 1 2))
  (loop))

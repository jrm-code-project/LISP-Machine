;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10 -*-


(defvar *ssf* nil)
(defvar *sgp* nil)

(defun ssf-setup ()
  (setq *ssf*  (system-source-files 'system))
  (setq *sgp*  (mapcar #'(lambda (x) (send x :generic-pathname)) *ssf*))
  "done")

(defun ssf-explain ()
  (format t "~&; System source file info:~%")
  (dolist (i *sgp*)
    (format t "~A with base ~D readable ~S, package ~S, by ~S on ~\time\~%"
            i (get i :base) (get i :readtable)
            (get i :package) (first (get i :compile-data))
            (or (third (get i :compile-data)) 0))))

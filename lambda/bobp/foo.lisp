;;; -*- Mode:LISP; Readtable:CL; Base:10 -*-


(defun depth (x)
  (cond
    ((consp x)
     (1+ (loop for l in x
               maximize (depth l))))
    (t
     0)))

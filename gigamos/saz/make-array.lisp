;;; -*- Mode:LISP; Readtable:CL; Base:10 -*-
(defun TEST ()
  (loop for *print-length* in '(nil 3 4 5) do
        (loop for *print-array* in '(nil 3 4 5 ) do
              (loop for si:*print-simple-vector-length in '(nil 3 4 5) do
                    (format t "~%~%~S~%~
                               *print-length* is ~A~%~
                               *print-array* is ~A~%~
                               si:*print-simple-vector-length* is ~A"
                               (make-array 4)
                               *print-length*
                               *print-array*
                               si:*print-simple-vector-length)))))


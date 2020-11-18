;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


(defun afoo (a b)
  (with-open-file (sa a)
    (with-open-file (sb b :direction :output)
      (do ((line))
          ((null (setq line (readline sa nil))))
        (cond ((zerop (string-length line)))
              ((= #\; (aref line 0))
               (princ line sb)
               (terpri sb)))))))

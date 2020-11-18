;;; -*- Mode:LISP; Base:10; Readtable:CL; Package:HUNLA -*-

(defun what-paths-internal (from to)
  (macrolet ((caller-name (caller-info) `(car ,caller-info))
             (caller-type (caller-info) `(cdr ,caller-info)))
    (let ((answer nil))
      (dolist (caller (who-calls-internal to))
        (when (member ':function (caller-type caller))
          (cond ((eq (caller-name caller) from)
                 (format t "~&found ~S" (caller-name caller))
                 (push to answer)
                 (push from answer))
                (t
                 (format t "~&searching callers of ~S" (caller-name caller))
                 (setq answer (append answer
                                      (what-paths-internal from (caller-name caller))))))))
      answer)))



;;;; Edit History for WHO-CALLS-AUX.LISP.1
;;; [11/08/88 15:34 CStacy] Get paths-from/to feature idea from SMHTOOLS.

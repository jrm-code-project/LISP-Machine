;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defun blimp ()
  (let (*print-array*)
    #'(lambda ()
        (multiple-value-setq (*print-array*)
          (foo)))))

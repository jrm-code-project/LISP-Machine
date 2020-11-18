;;; -*- Mode:Lisp; Package:NC; Readtable:CL; Base:10 -*-

;;;; Extra Stuff

;;; things that weren't found
;;; stand-ins, glue, etc


(defun constant-p (symbol)
  (get symbol 'system:system-constant))

(defun return-list-to-freelist (l))


(defun set-eq (s1 s2)
  (and (= (length s1) (length s2))
       (every #'(lambda (x) (member x s2 :test #'eq)) s1)))


;;; used in front-end;compilators
;;; (in front-end;declare)
(defun unused-variable-check (var) var)

(defun self-evaluating? (exp)
  (or (eq exp nil)
      (eq exp t)
      (numberp exp)
      (stringp exp))) ;???



(defun check-special-form-syntax (descr exp)
  exp)

;;; -*- Mode:LISP; Package:INTERPRETER; Base:10; Readtable:CL -*-
;;;
;;; INTERPRETER-EH.LISP
;;;
;;; Interpreter error handler.  Takes advantage of implementation dependent features;
;;; currently it uses the Lambda's condition handler system.  It can handle up to
;;; four proceed types.
;;;
;;; The structure of an interpreter error is defined in INTERPRETER.LISP
;;;

(defun invoke-interpreter-error-handler (error-form)
  (let ((condition
          (make-condition 'INTERPRETER-EH-ERROR :interpreter-error-form error-form))
        (proceed-types
          (first-n-proceed-types (length (interpreter-error-proceed-types error-form)))))
    (multiple-value-bind (proceed-type)
        (signal-condition condition proceed-types)
      (funcall (nth-proceed-type-continuation
                 (position proceed-type *ki-eh-proceed-type-list*)
                 error-form)))))

(defvar *ki-eh-proceed-type-list*
        '(:proceed-type-0 :proceed-type-1 :proceed-type-2 :proceed-type-3))

(defun first-n-proceed-types (n)
  (ldiff *ki-eh-proceed-type-list* (nthcdr n *ki-eh-proceed-type-list*)))


(defflavor INTERPRETER-EH-ERROR
         (interpreter-error-form)
         (error)
  :inittable-instance-variables
  :gettable-instance-variables)

(defmethod (INTERPRETER-EH-ERROR :REPORT)
           (stream)
  (apply #'format stream (interpreter-error-report interpreter-error-form)))

(defmethod (INTERPRETER-EH-ERROR :CASE :PROCEED-ASKING-USER :PROCEED-TYPE-0)
           (continuation ignore)
  (funcall continuation :PROCEED-TYPE-0))

(defmethod (INTERPRETER-EH-ERROR :CASE :PROCEED-ASKING-USER :PROCEED-TYPE-1)
           (continuation ignore)
  (funcall continuation :PROCEED-TYPE-1))

(defmethod (INTERPRETER-EH-ERROR :CASE :PROCEED-ASKING-USER :PROCEED-TYPE-2)
           (continuation ignore)
  (funcall continuation :PROCEED-TYPE-2))

(defmethod (INTERPRETER-EH-ERROR :CASE :PROCEED-ASKING-USER :PROCEED-TYPE-3)
           (continuation ignore)
  (funcall continuation :PROCEED-TYPE-3))

(defmethod (INTERPRETER-EH-ERROR :CASE :DOCUMENT-PROCEED-TYPE :PROCEED-TYPE-0)
           (stream &optional ignore)
  (apply #'format stream (nth-proceed-type-report-list 0 interpreter-error-form)))

(defmethod (INTERPRETER-EH-ERROR :CASE :DOCUMENT-PROCEED-TYPE :PROCEED-TYPE-1)
           (stream &optional ignore)
  (apply #'format stream (nth-proceed-type-report-list 1 interpreter-error-form)))

(defmethod (INTERPRETER-EH-ERROR :CASE :DOCUMENT-PROCEED-TYPE :PROCEED-TYPE-2)
           (stream &optional ignore)
  (apply #'format stream (nth-proceed-type-report-list 2 interpreter-error-form)))

(defmethod (INTERPRETER-EH-ERROR :CASE :DOCUMENT-PROCEED-TYPE :PROCEED-TYPE-3)
           (stream &optional ignore)
  (apply #'format stream (nth-proceed-type-report-list 3 interpreter-error-form)))

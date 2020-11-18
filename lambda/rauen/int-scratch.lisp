;;; -*- Mode:LISP; Package:JR-INT; Base:10 -*-
(defmacro my-defun (name blist &body body)
  `(FDEFINE (QUOTE ,name) (FUNCTION (LAMBDA ,blist ,@body))))

(defmacro cl:defvar (name &optional value)

(defun hookfn (exp env)
  (declare (ignore env))
  (format *terminal-io* "~&Evaluating (eval) expression: ~S.~%" exp)
  (let ((*evalhook* 'hookfn))
    (eval exp)))

(defvar *hooklevel* 0)

(defun hook (x)
  (let ((*evalhook* 'eval-hook-function))
    (eval-exp x)))

(defun eval-hook-function (form &optional env)
  (let ((*hooklevel* (+ *hooklevel* 1)))
    (format *trace-output* "~%~V@TForm:  ~S"
            (* *hooklevel* 2) form)
    (let ((values (multiple-value-list
                    (evalhook form
                              #'eval-hook-function
                              nil
                              env))))
      (format *trace-output* "~%~V@TValue:  ~{~S~}"
              (* *hooklevel* 2) values)
      (values-list values))))

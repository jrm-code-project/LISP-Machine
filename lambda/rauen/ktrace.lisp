;;; -*- Mode:LISP; Package:INTERPRETER; Base:10; Readtable:CL -*-

;;; KTRACE.LISP
;;;

(defvar *ktrace-level* 0)
(defvar *atrace-level* 0)

(defun ktrace (x)
  (let ((*evalhook* 'ktrace-function))
    (eval x)))

(defun ktrace-function (form &optional env)
  (let ((*ktrace-level* (+ *ktrace-level* 1)))
    (format *trace-output*
            "~%~V@TForm:  ~S"
            (* *ktrace-level* 2)
            form)
    (let ((values (multiple-value-list
                    (evalhook form #'ktrace-function nil env))))
      (format *trace-output*
              "~%~V@TValue:~{ ~S~}"
              (* *ktrace-level* 2)
              values)
      (values-list values))))

(defun atrace (x)
  (let ((*applyhook* 'atrace-function))
    (eval x)))

(defun atrace-function (fn args &optional env)
  (let ((*atrace-level* (+ *atrace-level* 1)))
    (format *trace-output*
            "~%~V@TFunction:  ~S"
            (* *atrace-level* 2)
            fn)
    (format *trace-output*
            "~%~V@TArguments: ~S"
            (* *atrace-level* 2)
            args)
    (let ((values (multiple-value-list
                    (applyhook fn args nil #'atrace-function env))))
      (format *trace-output*
              "~%~V@TValue:~{ ~S~}"
              (* *atrace-level* 2)
              values)
      (values-list values))))

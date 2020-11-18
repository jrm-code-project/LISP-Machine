;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; DESCRIBE.LISP
;;;

(defun describe-compiled-function (compiled-function &optional (stream t))
  (format stream "~&Name:         ~A" (k2::%compiled-function-name compiled-function))
  (format stream "~&Entry points: ~A" (k2::%compiled-function-entry-points compiled-function))
  (format stream "~&Length:       ~A" (k2::%compiled-function-length compiled-function)))

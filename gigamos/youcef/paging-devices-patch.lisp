;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-

(defvar *paging-devices* nil)

(defun make-paging-devices (&optional (number-of-devices 16.))
  (setq *paging-devices* (array:make-1d-array number-of-devices))
  )

(defvar *null-paging-device*         0)
(defvar *boot-band-paging-device-id* 1)
(defvar *page-band-paging-device-id* 2)

;;; -*- Mode:LISP; Package:CL-ZWEI; Readtable:CL; Base:10; Patch-file: T -*-
;;;
;;; Edit Histories
;;; Adapted from "Kung Pao" Copyright 1988, 1987 (c) by Christopher C. Stacy

;;; The edit history is a series of formatted textual entries located at the end
;;; of the source file.  The history is seperated by the EDIT-HISTORY-MARKER,
;;; followed by a header identifying the name of the file, etc.
;;; Each entry then begins with an EDIT-HISTORY-BEGIN, followed by an entry header
;;; the associated commentary, and then the EDIT-HISTORY-END characters if any.


;;; Each major mode defines comment markers for use in the edit history.

(defgeneric edit-history-delimiter (mode))
(defgeneric edit-history-begin (mode))
(defgeneric edit-history-end (mode))

(defmethod edit-history-begin (major-mode) *comment-begin*)
(defmethod edit-history-end (major-mode) *comment-end*)

(defmethod edit-history-delimiter (major-mode)
  (format nil "~|~%~A" *comment-begin*))

(defmethod edit-history-delimiter (lisp-syntax-mixin)
  (format nil "~|~%;;;;"))




(defmethod edit-history-marker (interval)
  (format nil "~A Edit History for"
          (edit-history-delimiter (major-mode self))))












;;;; Interface to the Patch System

(defvar *patch-system-edit-history* nil
  "The unconsumated edit-history for the system we are trying to patch.")

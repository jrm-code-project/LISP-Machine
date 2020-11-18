;;; -*- Mode:LISP; Package:HL; Readtable:CL; Base:10; Patch-file: T -*-


(defmacro printing-package-names (&body body)
  `(let ((*package* nil))
     ,@body))


(defun am-or-pm (hours minutes seconds)
  (cond ((and (zerop seconds)
              (zerop minutes))
         (if (= hours 0) "M " "N "))
        ((>= hours 12.) "pm")
        (t "am")))

(defun hours-mod-12 (hours)
  (if (or (= hours 12) (= hours 0))
      12
    (mod hours 12)))




;;;; Edit history for HUNLA:L;LISP-PATCHES.LISP.27
;;;
;;; [10/27/88 03:30 CStacy] Macro WITH-WHOSTATE: Added.
;;; [10/27/88 03:30 CStacy] Macro PRINTING-PACKAGE-NAMES: Added.
;;; [11/17/88 23:05 CStacy] NWATCH and package display hacks.

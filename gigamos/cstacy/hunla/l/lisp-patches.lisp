;;; -*- Mode:LISP; Package:HL; Readtable:CL; Base:10; Patch-file: T -*-

;(defun lisp:break (&optional format-string &rest args)
;  (when format-string
;    (apply #'format *error-output* format-string args))
;  (eh:debug))
;(defun breakpoint-in-code-style-checker (form)
;  (ignore form)
;  (compiler:warn 'breakpoint-in-code ':implausible "Breakpoint in code"))
;(EVAL-WHEN (EVAL LOAD COMPILE)
;  (zl:defprop lisp:break breakpoint-in-code-style-checker 'compiler:style-checker))

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



(defun rdtbl-shortest-name (&optional (readtable *readtable*))
  (let ((shortest-name (first (si:rdtbl-names readtable))))
    (dolist (nick (si:rdtbl-names readtable))
      (if (and (not (string-equal nick "T"))    ;ZL idiocy
               (< (zl:string-length nick)
                  (zl:string-length shortest-name)))
          (setq shortest-name nick)))
    shortest-name))


(defun set-default-process-quanta (&optional (quantum 60.))
  (setq si::default-quantum quantum)
  (mapcar #'(lambda (proc) (zl:send proc :set-quantum quantum))
          si:all-processes))





;;;; Edit history for HUNLA:L;LISP-PATCHES.LISP.27
;;;
;;; [10/27/88 03:30 CStacy] Macro WITH-WHOSTATE: Added.
;;; [10/27/88 03:30 CStacy] Macro PRINTING-PACKAGE-NAMES: Added.
;;; [11/17/88 23:05 CStacy] NWATCH hacks for prettier who-line.
;;; [11/17/88 23:57 CStacy] Might as well fix the package display also.

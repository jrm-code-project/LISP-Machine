;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; Patch-file: T -*-

;;; Process stuff belongs on SYS:SYS2;PROCES

(defun set-default-process-quanta (&optional (quantum 60.))
  (setq default-quantum quantum)
  (mapcar #'(lambda (proc) (send proc :set-quantum quantum))
          all-processes))


;;; Readtable stuff belongs in SYS:IO;RDDEFS

(defun rdtbl-shortest-name (&optional (readtable *readtable*))
  (let ((shortest-name (first (rdtbl-names readtable))))
    (dolist (nick (rdtbl-names readtable))
      (if (and (not (string-equal nick "T"))    ;ZL idiocy
               (< (string-length nick)
                  (string-length shortest-name)))
          (setq shortest-name nick)))
    shortest-name))


;;; Breakpoint hackery.

;(defun lisp:break (&optional format-string &rest args)
;  (when format-string
;    (apply #'format *error-output* format-string args))
;  (eh:debug))
;(defun breakpoint-in-code-style-checker (form)
;  (ignore form)
;  (compiler:warn 'breakpoint-in-code ':implausible "Breakpoint in code"))
;(EVAL-WHEN (EVAL LOAD COMPILE)
;  (zl:defprop lisp:break breakpoint-in-code-style-checker 'compiler:style-checker))

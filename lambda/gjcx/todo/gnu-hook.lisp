;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Patch-File:T; Base:10 -*-


(defvar *notify-f* nil)

(defun lmfs-parse-for-server (string)
  (let ((obj (condition-case (result)
                 (cond ((string-equal string "//")
                        (fs:parse-pathname "LM:~;*.*#*"))
                       ((string-search "//" string)
                        (send (fs:parse-pathname string (si:parse-host "angel"))
                              :new-pathname :host si:local-host))
                       ('else
                        (fs:merge-pathname-defaults
                          string
                          local-host-pathname
                          ':unspecific ':newest)))
               (:pathname-parse-error
                result))))
    (when *notify-f*
      (tv:notify nil "File serving ~A: ~A"
                 (and (boundp 'conn)
                      (or (si:get-host-from-address (chaos:foreign-address conn) :chaos)
                          (format nil "CHAOS|~O" (chaos:foreign-address conn) )))
                 obj))
    obj))

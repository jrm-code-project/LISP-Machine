;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-


(defvar *punt-file-types* '("QFASL"))
(defvar *punt-subdirectories* '("MACDOC" "MAXDOC" "VMSLINK" "REL2-PATCHES" "REL2-OBJECTS"))

(defun copy-macsyma (from-machine &optional (to-machine "LM"))
  (check-type from-machine string)
  (check-type to-machine string)
  (fs:copy-directory (string-append from-machine ":DOE-MACSYMA;*.*#>")
                     (string-append to-machine ":DOE-MACSYMA;")
                     :copy-only :newest
                     :filter #'(lambda (p)
                                 (let ((type (send (car p) :type))
                                       (subdir (if (atom (send (car p) :directory)) nil
                                                 (cadr (send (car p) :directory)))))
                                   (cond ((mem #'string-equal type *punt-file-types*)
                                          nil)
                                         ((mem #'string-equal subdir *punt-subdirectories*)
                                          nil)
                                         ('else
                                          t))))))

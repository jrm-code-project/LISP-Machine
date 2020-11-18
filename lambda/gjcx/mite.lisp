;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-



(defvar *source-files* ())

;; run load-source-files on a lambda.

(defun load-source-files (&optional (f "dj:release.reports;mit-explorer-release.text"))
  (with-open-file (stream f)
    (setq *source-files* nil)
    (do ((line))
        ((null (setq line (readline stream nil))))
      (cond ((zerop (string-length line)))
            ((char= (aref line 0) #\;))
            ('else
             (print line)
             (let ((p (send (fs:parse-pathname line) :translated-pathname)))
               (let ((p* (probe-file (send p :new-pathname :host "LAM3"
                                           :directory (append '("RELEASE-3")
                                                              (cdr (send p :directory)))))))
                 (setq *source-files* (nconc *source-files*
                                             (ncons p*))))))))))


(defun dump-source-files (&optional (f "exp1:tmp;source-files.lisp"))
  (with-open-file (stream f :direction :output)
    (format stream ";;;-*-mode:lisp;package:user-*-~2%")
    (format stream "(defvar *source-files* '(~%~{ \"~A\"~%~} ))~%"
            *source-files*)))

(defun copy-source-files-to-tape ()
  (dolist (f *source-files*)
    (fs:copy-directory f "MT:")))

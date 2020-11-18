;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-


(defun find-copyright-notice-place (filename)
  (declare (values truename skip-lines))
  (with-open-file (input filename)
    (values (send input :truename)
            (do ((-*-count 0)
                 (line)
                 (temp)
                 (line-count 1 (1+ line-count)))
                ((null (setq line (readline input nil)))
                 nil)
              (cond ((not (setq temp (string-search "-*-" line))))
                    ((= -*-count 1)
                     (return line-count))
                    ((string-search "-*-" line (+ temp 3))
                     (return line-count))
                    ('else
                     (incf -*-count)))))))


(defvar *copyright-notice*)

(defun insert-copyright-notice (source-filename)
  (multiple-value-bind (truename skip)
      (find-copyright-notice-place source-filename)
      (or skip
          (ferror nil "No mode line in file? ~S" truename))
    (with-open-file (input truename)
      (with-open-file (output (send truename :new-version (1+ (send truename :version)))
                              :direction :output
                              :if-exists :error
                              :if-does-not-exist :create)
        (dotimes (j skip)
          (send output :line-out (readline input)))
        (send output :string-out *copyright-notice*)
        (stream-copy-until-eof input output)))))





(setq *copyright-notice*
"
;;; *****************************************
;;; ** (C) COPYRIGHT 1986 LISP MACHINE INC **
;;; **  Also see file named Copyright.Text **
;;; *****************************************

"
)

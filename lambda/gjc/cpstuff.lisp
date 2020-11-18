;;; -*- Mode:LISP; Base:10 -*-

;;; stuff to move to archive on MOE.

(defvar *dirs1* '(
      BENCHMARKS
      diag
      EXP-FORD
      HYPER
      HYPERCALC
      HYPERWINDOW
      KHS
      LCOMP
      LFILE-COPY
      MLY
      QL
      RELEASE
      SYSTEM
      TEST
      WIRELIST
      XREF
      YAPS)       )


(defun move-dirs1 ()
  (dolist (d *dirs1*)
    (fs:copy-directory (format nil "DJ:~A;*.*#>" d) (format nil "LM:~A;" d))))


(defun kill-dirs1 ()
  (dolist (d *dirs1*)
    (filesystem-purge :versions-to-keep 0
                      :directory (string d))))

(defun kill-dirs11 ()
  (dolist (d *dirs1*)
    (catch-error (filesystem-expunge :directory (string d)))))





(defun lm-prolog-files ()
  (let (good reject)
    (map-all-files #'(lambda (p)
                       (cond ((mem #'string-equal (send (car p) :type)
                                   '("QFASL"))
                              (push p reject))
                             ('else
                              (push p good))))
                   (fs:make-pathname :host "LAM3"
                                     :directory '("QL" "LM-PROLOG"))
                   :version :newest
                   :call-on-plist t
                   :before-f #'(lambda (x) (print x)))
    (values (nreverse good) (nreverse reject))))

(defun copy-lm-prolog-files ()
  (dolist (f (lm-prolog-files))
    (fs:fs-copy-file (car f)
                     (send (car f) :new-pathname
                           :host "DJ"
                           :directory (cons "L" (cdr (send (car f) :directory))))
                     :output-directory-list (cdr f))))

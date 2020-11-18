;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

(defvar *to-snarf* nil)

(defun set-snarf (path)
  (setq *to-snarf* nil)
  (snarf (fs:parse-pathname path)))

(defun belist (x)
  (if (atom x) (list x) x))

(defun snarf (x &aux recurse)
  (format t "~&; looking at ~S~%" x)
  (dolist (f (fs:directory-list x))
    (cond ((null (car f)))
          ((get f :directory)
           (push (send (car f) :new-pathname
                       :directory (append (belist (send (car f) :directory))
                                          (list (send (car f) :name)))
                       :name :wild
                       :type :wild
                       :version (send x :version))
                 recurse))
          ('else
           (push f *to-snarf*))))
  (dolist (x recurse)
    (snarf x)))

(defun doit (&optional &key (to-host si:local-host) print-only
             (files (reverse *to-snarf*))
             new-root)
  (format t "~&~D files to copy" (length files))
  (dolist (x files)
    (format t "~&~D files to go" (length (memq x files)))
    (let ((from (car x))
          (to (send (car x) :new-pathname
                    :host to-host
                    :directory (if new-root
                                   (append (belist new-root)
                                           (cdr (belist (send (car x) :directory))))
                                 (send (car x) :directory)))))
      (cond (print-only
             (format t "~A ==> ~A~%" from to))
            ('else
             (fs:fs-copy-file from to :directory-list (cdr x)))))))

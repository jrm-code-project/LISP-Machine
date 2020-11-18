;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

;;; parse the output of the ITS DUMP program and find the
;;; optimal tapes to get the latest versions of a set of files.

(defvar *its-dump-data* nil)

(defstruct (tape-file (:print-function tape-file-print-function))
  tape
  index
  directory
  name
  type
  version
  creation-date)


(defun tape-file-print-function (object stream level)
  level
  (format stream "#<~A;~A ~A on TAPE ~A>"
          (tape-file-directory object)
          (tape-file-name object)
          (or (tape-file-version object)
              (tape-file-type object))
          (tape-file-tape object)))


(defun load-its-dump-data (filename)
  (with-open-file (stream filename)
    (do ((line))
        ((not (setq line (readline stream nil))))
      (when (string-equal " TAPE" line :start2 0 :end2 5)
        (let* ((type (substring line 29 35))
               (version (ignore-errors (parse-integer type))))
          (cond (version
                 (setq type nil))
                ('else
                 (setq type (intern (string-trim '(#\space) type) ""))))
          (push (make-tape-file :tape (subtoken line 8 14)
                                :directory (subtoken line 15 21)
                                :name (subtoken line 22 28)
                                :version version
                                :type type
                                :index (parse-integer line :start 37 :end 41)
                                :creation-date (time:parse-universal-time
                                                 line
                                                 41
                                                 (string-search "." line 41)))
                *its-dump-data*))))))

(defun subtoken (line start end)
  (intern (string-trim '(#\space) (substring line start end))
          ""))


(defun latest-versions ()
  (let ((list))
    (dolist (e *its-dump-data*)
      (when (tape-file-version e)
        (let ((cell (CAR (MEM #'(lambda (e1 e2)
                                  (and (eq (tape-file-directory e1)
                                           (tape-file-directory e2))
                                       (eq (tape-file-name e1)
                                           (tape-file-name e2))))
                              e
                              list))))
          (cond ((not cell)
                 (push (make-tape-file :directory (tape-file-directory e)
                                       :name (tape-file-name e)
                                       :version (tape-file-version e))
                       list))
                ('else
                 (setf (tape-file-version cell)
                       (max (tape-file-version cell) (tape-file-version e))))))))
    (dolist (e1 *its-dump-data*)
      (dolist (e2 list)
        (when (and (eq (tape-file-directory e1) (tape-file-directory e2))
                   (eq (tape-file-name e1) (tape-file-name e2))
                   (eq (tape-file-version e1) (tape-file-version e2)))
          (push (tape-file-tape e1) (tape-file-tape e2)))))
    list))



(defun tape-summary (tape-files)
  "Print a tape of tapes, percent winnage and filenames"
  (let (alist)
    (dolist (f tape-files)
      (dolist (tf (tape-file-tape f))
        (let ((cell (assq tf alist)))
          (cond ((not cell)
                 (push (cons tf (list f)) alist))
                ('else
                 (push f (cdr cell)))))))
    (format t "~&TAPE  %WIN  Files~%")
    (setq alist (sort alist #'(lambda (a b) (> (length a) (length b)))))
    (dolist (a alist)
      (format t "~6A ~3D ~{~A~^ ~}~%"
              (car a)
              (quotient (times (length (cdr a)) 100)
                        (length tape-files))
              (sort (mapcar #'tape-file-name (cdr a)) #'string-lessp)))))

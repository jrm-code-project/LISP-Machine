;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(defvar *machines-and-dirs* nil)

(load "dj:dawna;backup.data" :set-default-pathname nil)


(defun non-qfasl-filter (p)
  (not (mem #'string-equal (send (car p) :type) '("QFASL" "XFASL" "QF3"))))

(defmacro background-terminal (&body body)
  `(let ((old-more-p (si:send-if-handles terminal-io :more-p))
         (old-deexposed-typeout-action
           (si:send-if-handles terminal-io :deexposed-typeout-action))
         (old-priority (send current-process :priority)))
     (unwind-protect
         (progn
           (si:send-if-handles terminal-io :set-more-p nil)
           (si:send-if-handles terminal-io :set-deexposed-typeout-action :permit)
           (send current-process :set-priority -1)
           ,@body)
       (si:send-if-handles terminal-io :set-more-p old-more-p)
       (si:send-if-handles terminal-io :set-deexposed-typeout-action old-deexposed-typeout-action)
       (send current-process :set-priority old-priority))))

(defun backup-a-machine (name)
  (let ((cell (ass #'(lambda (x y) (eq (si:parse-host x) (si:parse-host y))) name *machines-and-dirs*)))
    (or cell (ferror nil "~A is not in the list ~S" '*machines-and-dirs*))
    (fs:tm-init)
    (fs:mt-rewind)
    (background-terminal (backup-files-given cell))
    (fs:mt-write-eof)))


(defun backup-all-machines ()
  (fs:tm-init)
  (fs:mt-rewind)
  (background-terminal
    (mapcar #'(lambda (cell)
                (backup-files-given cell)
                (fs:mt-write-eof))
            *machines-and-dirs*))
  (fs:mt-write-eof)
  (fs:mt-unload))


(defun backup-files-given (desc)
  (format t "~&Backing up directories on ~S~%" (car desc))
  (dolist (dir (cdr desc))
    (fs:copy-directory (format nil "~A:~A;*.*#>" (car desc) dir) "MT:"
                       :filter #'non-qfasl-filter)))


(or (boundp 'FS:*disk-space-warner-threshold*)
    (set 'FS:*disk-space-warner-threshold* 2000))

(defun backup-lam15-onto-lam10 ()
  (or (eq si:local-host (si:parse-host "LAM10"))
      (ferror nil "You must be ON LAM10 to run this function"))
  (backup-host-to-here "lam15"))

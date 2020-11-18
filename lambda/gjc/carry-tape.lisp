;;; -*- Mode:LISP; Package:USER; Base:10; readtable:zl -*-
;;; TI machines want to see MODE:ZETALISP,
;;; damn, MODE:LISP gives nothing!
;;; -*- Mode:ZETALISP; Package:USER; Base:10; readtable:zl -*-
;;;
;;; Minor support for Symbolics' CARRY-TAPE format
;;;
;;; -dg 5/13/85
;;; -gjc 11-Aug-86 13:09:07

(defun make-carry-tape-stream (&rest ignored)
  #+TI
  (mt:make-mt-stream :characters t :byte-size 8 :error nil :density 0 :direction :input
                     :record-size 1024)
  #+LMI
  (fs:make-mt-stream :characters t :byte-size 8 :error nil :density 0 :direction :input
        ;; this is actually a max record size. if too small then there
        ;; will be an error during read.
                                     :record-size 16384))


(defun read-prop-line (stream)
  (let ((line (readline stream nil)))
    (cond ((null line) nil)
          ('else
           (let ((n (string-search " " line)))
             (cond ((null n)
                    (intern line ""))
                   ('else
                    (let ((key (intern (substring line 0 n) "")))
                      (values key
                              (funcall (or (get key 'prop-line-value)
                                           #'(lambda (line n)
                                               (string-trim '(#\space)
                                                            (substring line n))))
                                       line
                                       n))))))))))

(defprop :creation-date ut-prop-line prop-line-value)
(defprop :time ut-prop-line prop-line-value)

(defun ut-prop-line (line start)
  (or (catch-error (parse-integer line :start start) nil)
      (time:parse-universal-time line start)))

(defun lisp-prop-line-value (line start)
  (let ((base 10.)
        (ibase 10.)
        (*package* (find-package 'user))
        (*readtable* (si:find-readtable-named "CL")))
    (read-from-string line nil start)))

(defprop :tape-system-version lisp-prop-line-value prop-line-value)
(defprop :dump-list-follows lisp-prop-line-value prop-line-value)

(defun read-plist-lines (stream name)
  (do ((key)(value)(plist (ncons name)))
      ((not (multiple-value-setq (key value) (read-prop-line stream)))
       plist)
    (and (eq :end key) (return plist))
    (putprop plist value key)))

(defun read-carry-tape-header ()
  (let ((stream (make-carry-tape-stream)))
    (let ((plist (read-plist-lines stream 'header)))
      (flush-tape-stream stream)
      plist)))


(defun flush-tape-stream (stream)
  (stream-copy-until-eof stream 'si:null-stream)
  (send stream :close :raw))


(defun describe-carry-tape-header (h)
  (format t "~&       ~A~%" (car h))
  (do ((l (cdr h) (cddr l)))
      ((null l))
    (format t "~S ~S~%" (car l) (cadr l))))

(defresource random-string-resource (size)
  "A resource for copying data from a raw quart-stream to another stream."
  :constructor (make-random-string size)
  :matcher (>= (caddr object) size))

(defun make-random-string (size)
  (let ((string (make-string size :fill-pointer 0)))
    (list string
          (make-array (floor size 2) :type 'art-16b :displaced-to string
                      :fill-pointer 0)
          size)))

(defun preservable-properties (plist)
  (do ((return-list (ncons 'preservable-properties))
       (l '(:creation-date :author) (cdr l))
       (value))
      ((null l) (cdr return-list))
    (if (setq value (get plist (car l)))
        (putprop return-list value (car l)))))

(defprop :version lisp-prop-line-value prop-line-value)
(defprop :characters lisp-prop-line-value prop-line-value)
(defprop :byte-size lisp-prop-line-value prop-line-value)

(defun copy-carry-tape-file (host &key
                             (create-directory t)
                             overwrite
                             query
                             from-stream
                             (parse-pathname-on-carry-tape 'default-parse-pathname-on-carry-tape))
  (setq host (si:parse-host host))
  (let ((instream (if from-stream
                      (open from-stream)
                    (make-carry-tape-stream))))
    (let ((plist (read-plist-lines instream 'carry-tape-file-plist))
          (new-pathname)
          (preservable-properties)
          (byte-size))
      (when (not (setq new-pathname (funcall parse-pathname-on-carry-tape
                                             (get plist :pathname) host plist)))
        (format t "~&Skipping tape file ~A~%" (get plist :pathname))
        (return-from copy-carry-tape-file (close instream)))
      (condition-case ()
          (probef new-pathname)
        (fs:directory-not-found
         (when (and create-directory (or (not query)
                                         (y-or-n-p "~&Create directory ~A?" new-pathname)))
           (format t "~&Creating directory for ~A~%" new-pathname)
           (fs:create-directory new-pathname)
           (setq overwrite t)))
        (:no-error (if (probef new-pathname)
                       (when (eq overwrite :ask)
                         (setq overwrite (yes-or-no-p "~&Overwrite file [~A]? ")))
                       (setq overwrite t))))
      (cond ((not overwrite)
             (format t "~&File Already exists - ~A - Not Overwriting~%" new-pathname)
             (close instream))
            ((and query (not (y-or-n-p "~&Restore ~A?" new-pathname)))
             (close instream))
            ('else
             (format t "~&Copying file ~A to ~A - " (get plist :pathname) new-pathname)
             (fs:create-directory new-pathname)
             (setq preservable-properties (preservable-properties plist))
             (setq byte-size (or (get plist :byte-size)
                                 (if (get plist :characters) 8. 16.)))
             (with-open-file (outstream new-pathname
                                        :direction :output
                                        :characters (get plist :characters)
                                        :byte-size byte-size)
               (when preservable-properties
                 (lexpr-send outstream :change-properties t preservable-properties))
               (copy-carry-tape-blocks instream
                                       outstream
                                       byte-size)))))))



(defun copy-carry-tape-blocks (instream outstream byte-size)
  (do-forever
    (ecase (send instream :tyi)
      (#\B
       (do ((l '(#\L #\O #\K) (cdr l)))
           ((null l))
         (or (eq (car l) (send instream :tyi))
             (ferror nil "bad carry tape file record")))
       (let ((record-size 0))
         (dotimes (j 12)
           (setq record-size (+ (or (digit-char-p (send instream :tyi))
                                    (ferror nil "bad carry tape file record"))
                                (* record-size 10))))
         (using-resource (string-r random-string-resource record-size)
           (let ((string (car string-r)))
             (send instream :string-in t string 0 record-size)
             (cond ((= byte-size 8.)
                    (send outstream :string-out string))
                   ((= byte-size 16.)
                    (setf (fill-pointer (cadr string-r)) (ceiling record-size 2))
                    (send outstream :string-out (cadr string-r))))))))
      (#\E
       (do ((l '(#\O #\F) (cdr l)))
           ((null l))
         (or (eq (car l) (send instream :tyi))
             (ferror nil "bad carry tape file record")))
       (flush-tape-stream instream)
       (return (format t "done.~%"))))))

(defun restore-carry-tape (&key
                           (host si:local-host)
                           (overwrite nil)
                           (query nil)
                           transform)
  (setq host (si:parse-host host))
  (fs:mt-rewind)
  (describe-carry-tape-header (read-carry-tape-header))
  (do-forever
    (condition-case ()
        (copy-carry-tape-file
         host :overwrite overwrite :query query
         :parse-pathname-on-carry-tape (or transform
                                           'default-parse-pathname-on-carry-tape))
      (fs:read-end-of-file))))

(defvar *dummy-lispm-host* (si:make-dummy-host :lispm))

(defvar *dummy-symbolics-host* (si:make-dummy-host :lispm))
(putprop *dummy-symbolics-host* :lmfs 'fs:file-system-type)

(defvar *dummy-unix-host* (si:make-dummy-host :unix))
(defvar *dummy-vms-host* (si:make-dummy-host :vms))
(defvar *dummy-tops20-host* (si:make-dummy-host :tops-20))

(defun strip-host (string)
  (let ((n (string-search ":" string)))
    (cond (n
           (substring string n))
          ('else
           string))))

(defun default-parse-pathname-on-carry-tape (string host plist)
  (default-transform-pathname
    (cond ((string-equal (get plist :system-type) "LISPM")
           (cond ((string-search ">" (get plist :directory))
                  (fs:parse-pathname (strip-host string)
                                     *dummy-symbolics-host*))
                 ('else
                  (fs:parse-pathname (strip-host string) *dummy-lispm-host*))))
          ((string-equal (get plist :system-type) "UNIX")
           (fs:parse-pathname (strip-host string)
                              *dummy-unix-host*))
          ((string-equal (get plist :system-type) "VMS")
           (fs:parse-pathname (strip-host string)
                              *dummy-VMS-host*))
          ((string-equal (get plist :system-type) "TOPS-20")
           (fs:parse-pathname (strip-host string)
                              *dummy-tops20-host*))

          ('else
           (ferror nil "unknown system type: ~S" (get plist :system-type))))
    host))

(defvar *default-ignore-types* '("BIN" "QFASL" "XFASL" "PRESS"))

(defvar *default-select-directories* nil)

(defun default-transform-pathname (x h)
  (and (default-select-pathname x)
       (send x :new-pathname :host h)))

(defun default-select-pathname (x)
  (cond ((mem #'(lambda (a b) (si:string-matchp b a))
              (send x :type)
              *default-ignore-types*)
         ())
        (*default-select-directories*
         (mem #'directory-matchp (if (atom (send x :directory))
                                     (list (send x :directory))
                                   (send x :directory))
              *default-select-directories*))
        ('else
         t)))


(defun directory-matchp (dir pattern)
  (do ((ldir dir (cdr ldir))
       (lp pattern (cdr lp)))
      ((or (null ldir) (null lp))
       (or (and (null ldir) (null lp))
           (eq (cadr lp) '**)))
    (cond ((eq (car lp) '**)
           (return t))
          ((eq (car lp) '*))
          ((si:string-matchp (car lp) (car ldir)))
          ('else
           (return nil)))))


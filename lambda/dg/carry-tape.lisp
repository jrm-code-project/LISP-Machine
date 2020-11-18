;;; -*- Mode:lisp; Package:USER; Base:10; readtable:zl -*-
;;;
;;; Minor support for Symbolics' CARRY-TAPE format
;;;
;;; -dg 5/13/85
;;;

(defun keyword-read (stream)
  "Read symbols into the package keyword"
  (let ((package (find-package "")))
    (read stream)))

(defun make-carry-tape-stream (&rest ignored)
  #+TI
  (mt:make-mt-stream :characters t :byte-size 8 :error nil :density 0 :direction :input
                     :record-size 1024)
  #+LMI
  (fs:make-mt-stream :characters t :byte-size 8 :error nil :density 0 :direction :input
                     :record-size 1024))

(defresource random-string-resource (size)
  "A resource for copying data from a raw quart-stream to another stream."
  :constructor (make-string size))

(defun read-carry-tape-header ()
  (with-open-stream (stream (make-carry-tape-stream))
    (do ((plist (list (intern (string-trim '(#/space) (readline stream))))))
        (())
      (let ((prop (keyword-read stream)))
        (when (eq prop :end)
          (when (get plist :dump-list-follows)
            (putprop plist
                     (if (and (string-equal (readline stream) "RECORD-TYPE DUMP-LIST")
                              (string-equal (readline stream) "END"))
                         (do ((dump-list)
                              (line (readline stream) (readline stream)))
                             ((zerop (length line)) dump-list)
                           (setq dump-list (append dump-list (list line))))
                       (format t "~&Error in header: Bad dump list.~%"))
                     :dump-list))
          (return plist))
        (putprop plist
                 (case prop
                   ((:version :tape-system-version :dump-list-follows)    ;symbols or numbers
                    (read stream))
                   ((:time :machine :user-id :tape-host :tape-drive)           ;strings
                    (readline stream))
                   (t (ferror nil "unhandled prop - ~A" prop)))
                 prop)))))

(defun describe-carry-tape-header (h)
  (print h))

(defun preservable-properties (plist)
  (let ((return-list (list "Preservable Properties")))
    (cond-every ((get plist :creation-date)
                 (putprop return-list
                          (typecase (get plist :creation-date)
                            (:string (time:parse-universal-time
                                       (get plist :creation-date)))
                            (:number (get plist :creation-date))
                            (t (time:get-universal-time)))
                          :creation-date))
                ((get plist :author)
                 (putprop return-list
                          (string (get plist :author))
                          :author)))
    (cdr return-list)))

(defun read-carry-record-plist (instream &aux (plist (list 'carry-tape-file-plist)))
  (do ((prop (keyword-read instream) (keyword-read instream)))
      ((eq prop :end) plist)
    (putprop plist
             (case prop
               ((:creation-date :author :type :raw-type :name
                                :raw-name :pathname :directory :host :dump-group)
                (readline instream))
               ((:version :characters :system-type :canonical-type :tape-system-version
                          :record-type :byte-size)
                (read instream)))
             prop)))

(defun copy-carry-tape-record (host &key
                               (create-directory t)
                               overwrite
                               query
                               from-stream)
  (setq host (si:parse-host host))
  (with-open-stream (instream (if from-stream
                                  (open from-stream)
                                  (make-carry-tape-stream)))
    (let ((plist (read-carry-record-plist instream))
          (new-pathname)
          (preservable-properties))
      (or (setq new-pathname (parse-pathname-on-carry-tape (get plist :pathname) host))
          (return-from copy-carry-tape-record nil))
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
             (format t "~&File Already exists - ~A - Not Overwriting~%" new-pathname))
            ((and query (not (y-or-n-p "~&Restore ~A?" new-pathname)))
             ())
            ('else
             (format t "~&Copying file ~A to ~A - " (get plist :pathname) new-pathname)
             (fs:create-directory new-pathname)
             (setq preservable-properties (preservable-properties plist))
             (with-open-stream (outstream
                                 (if overwrite
                                     (open new-pathname
                                           :direction :output
                                           :characters (get plist :characters)
                                           :byte-size (or (get plist :byte-size)
                                                          (if (get plist :characters) 8. 16.)))
                                     'si:null-stream))
               (when (and preservable-properties (neq outstream 'si:null-stream))
                 (lexpr-send outstream :change-properties t preservable-properties))
               (let ((block-string (make-string 12)))
                 (do ((eof (progn (send instream :string-in t block-string 0 4)
                                  (string-equal block-string "EOF " :end1 4))
                           (progn (send instream :string-in t block-string 0 4)
                                  (string-equal block-string "EOF " :end1 4))))
                     (eof)
                   (send instream :string-in t block-string 0)
                   (let ((*read-base* 10.))
                     (using-resource (string random-string-resource (read-from-string block-string))
                       (send instream :string-in t string)
                       (send outstream :string-out string))))))
             (when overwrite (format t "done.~%")))))))

(defun restore-carry-tape (&key
                           (host si:local-host)
                           (overwrite nil)
                           (query nil))
  (setq host (si:parse-host host))
  (fs:mt-rewind)
  (describe-carry-tape-header (read-carry-tape-header))
  (do-forever
    (condition-case ()
        (copy-carry-tape-record host :overwrite overwrite :query query)
      (fs:read-end-of-file))))

(defconst *dummy-lmfs-host*
          (progn (si:define-host "dummy"
                                 :host-names (list "dummy")
                                 :machine-type :lispm
                                 :system-type :lispm
                                 :chaos '(#o01))
                 (fs:add-file-computer '("dummy" :lmfs))
                 (si:parse-host "dummy")))

(defun parse-pathname-on-carry-tape (string host)
  (let ((spn (fs:parse-pathname string *dummy-lmfs-host*)))
    (fs:merge-pathnames (fs:make-pathname :host host) spn)))

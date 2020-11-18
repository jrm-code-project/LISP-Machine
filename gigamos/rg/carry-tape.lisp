;;; -*- Mode:Zetalisp; Package:USER; Base:10; readtable:zl -*-
;;;
;;; Minor support for Symbolics' CARRY-TAPE format
;;;
;;; -dg 5/13/85
;;; -chuck kollar @CGI 02/20/86
;;;
;;;
;;;


(defun keyword-read (stream)
  "Read symbols into the package keyword.
"
  (let ((package (find-package "")))
    (read stream)))


(defun make-carry-tape-stream (&rest ignored)
  "Return a carry tape stream.
"
  #+TI
  (mt:make-mt-stream :characters t :byte-size 8 :error nil :density 0 :direction :input
                     :record-size 1024)
  #+LMI
  (fs:make-mt-stream :characters t :byte-size 8 :error nil :density 0 :direction :input
                     :record-size 1024))


(defun read-carry-tape-header ()
  (with-open-stream (stream (make-carry-tape-stream))
    (let ((plist (list (intern (string-trim '(#/space) (readline stream))))))
      (loop
        (let ((prop (keyword-read stream)))
          (when (eq prop :END)
            (when (get plist :DUMP-LIST-FOLLOWS)
              (putprop plist
                       (if (and (string-equal (readline stream) "RECORD-TYPE DUMP-LIST")
                                (string-equal (readline stream) "END"))
                           (do* ((line (readline stream) (readline stream))
                                 (dump-list nil))
                                ((zerop (length line)) dump-list)
                             (setq dump-list (append dump-list (list line))))
                           (format t "~&Error in header: Bad dump list.~%"))
                       :dump-list))
            (return plist))
          (putprop plist
                   (case prop
                         ((:VERSION :TAPE-SYSTEM-VERSION :DUMP-LIST-FOLLOWS)    ;symbols or numbers
                          (read stream))
                         ((:TIME :MACHINE :USER-ID :TAPE-HOST :TAPE-DRIVE)      ;strings
                          (readline stream))
                         (t (ferror nil "unhandled prop - ~A" prop)))
                   prop))))))


(defun describe-carry-tape-header (h)
  (print h))


(defresource random-string-resource (size)
  "A resource for copying data from a raw quart-stream to another stream."
  :constructor (make-string size))


(defun decode-block-mark (thing)
  (if (string-equal "BLOK" thing :end2 4)
      (do ((num 0)
           (word (string thing))
           (count 15 (1- count)))
          ((= count 3) num)
        (setq num (+ num
                     (* (- (aref word count) 48)
                        (^ 10. (- 15 count))))))
      (ferror nil "~&Bad block marker - ~a" thing)))


(defun preservable-properties (plist)
  (let ((return-list (list "Preservable Properties")))
    (when (get plist :CREATION-DATE)
      (setf (get return-list :CREATION-DATE)
            (typecase (get plist :CREATION-DATE)
              (:STRING (time:parse-universal-time (get plist :CREATION-DATE)))
              (:NUMBER (get plist :CREATION-DATE))
              (ELSE (time:get-universal-time)))))
    (when (get plist :AUTHOR)
      (setf (get return-list :AUTHOR)
            (string (get plist :AUTHOR))))
    (cdr return-list)))


(defun read-carry-record-plist (instream &aux (plist (list 'carry-tape-file-plist)))
  (do ((prop (keyword-read instream) (keyword-read instream)))
      ((eq prop :END) plist)
    (setf (get plist prop)
          (case prop
                ((:CREATION-DATE :AUTHOR :TYPE :RAW-TYPE :NAME
                                 :RAW-NAME :PATHNAME :DIRECTORY :HOST :DUMP-GROUP)
                 (readline instream))
                ((:VERSION :CHARACTERS :SYSTEM-TYPE :CANONICAL-TYPE :TAPE-SYSTEM-VERSION
                           :RECORD-TYPE :BYTE-SIZE)
                 (read instream))))))


(defun copy-carry-tape-record (host &key
                               (create-directory t)
                               overwrite
                               query
                               from-stream)
  (setq host (si:parse-host host))
  (with-open-stream (instream (if from-stream
                                  (open from-stream)
                                  (make-carry-tape-stream)))
    (let* ((plist (read-carry-record-plist instream))
           (new-pathname (parse-pathname-on-carry-tape (get plist :PATHNAME) host))
           (preservable-properties nil))
      (unless new-pathname
        (return-from copy-carry-tape-record nil))

      (condition-case ()
          (directory new-pathname)              ; error if dir not there
        (fs:directory-not-found
         (when (and create-directory
                    (if query
                        (y-or-n-p "~&Create directory ~A?" new-pathname)
                        t))
           (format t "~&Creating directory for ~A~%" new-pathname)
           (fs:create-directory new-pathname)
           (setq overwrite t)))
        (:no-error (if (probe-file new-pathname)
                       (when (eq overwrite :ASK)
                         (setq overwrite (yes-or-no-p "~&Overwrite file [~A]? ")))
                       (setq overwrite t))))

      (cond ((not overwrite)
             (format t "~&File Already exists - ~A - Not Overwriting.~%" new-pathname))
            ((if query (y-or-n-p "~&Restore ~A?" new-pathname) t)
             (format t "~&Copying file ~A to ~A - " (get plist :PATHNAME) new-pathname)
             (setq preservable-properties (preservable-properties plist))
             (with-open-stream (outstream
                                 (if overwrite
                                     (open new-pathname
                                           :DIRECTION :OUTPUT
                                           :CHARACTERS (get plist :CHARACTERS)
                                           :BYTE-SIZE (or (get plist :BYTE-SIZE)
                                                          (if (get plist :CHARACTERS) 8. 16.)))
                                     'si:null-stream))
               (when (and preservable-properties (neq outstream 'si:null-stream))
                 (lexpr-send outstream :CHANGE-PROPERTIES t preservable-properties))
               (using-resource (block-string random-string-resource 16)
                 (do ((block-mark (progn (send instream :STRING-IN t block-string 0 3)
                                         (if (string-equal block-string "EOF" :END1 3)
                                             :EOF
                                             (send instream :STRING-IN t block-string 3)
                                             block-string))
                                  (progn (send instream :STRING-IN t block-string 0 3)
                                         (if (string-equal block-string "EOF" :END1 3)
                                             :EOF
                                             (send instream :STRING-IN t block-string 3)
                                             block-string))))
                     ((eq block-mark :EOF))
                   (using-resource (string random-string-resource (decode-block-mark block-mark))
                     (send instream :STRING-IN t string)
                     (send outstream :STRING-OUT string)))))
             (when overwrite (format t "done.~%")))))))


(defun restore-carry-tape (&key
                           (host si:local-host)
                           (overwrite nil)
                           (query nil)
                           (rewind t))
  (setq host (si:parse-host host))
  (when rewind (mt:rewind))
  (describe-carry-tape-header (read-carry-tape-header))
  (loop
    (condition-case ()
        (copy-carry-tape-record host :overwrite overwrite :query query)
      (fs:read-end-of-file))))


;; you may want to modify this function to serve your own purposes.

(defvar *pathname-string-replacements*
        '((#/[ #/<) (#/] #/>) (#/; #/#)))


(defun parse-pathname-on-carry-tape (string host)
  (let ((n (string-search ":" string)))
    (do ((s (substring string (if n (1+ n) 0)))
         (j 0 (1+ j)))
        ((= j (length s))
         (fs:parse-pathname s host))
      (setf (aref s j)
            (or (cadr (assq (aref s j) *pathname-string-replacements*))
                (aref s j))))))

;;; -*- Mode:Zetalisp; Package:USER; Base:10 -*-

;;; code to run in the TI-SYSTEM to restore magtapes.
;;; Just cant trust the winning TI software to do the predictable
;;; right thing for this. Too much hair and not enough functional
;;; arguments.

;;; before using this you must go into the SYSTEM-B thing and do PREPARE-TAPE.


(defun (:no-transform restore-source-directory) (from-dir)
  from-dir)

(defun (:lmi-sys restore-source-directory) (from-dir)
  (append '("LMI-SYS") (if (atom from-dir) (list from-dir) from-dir)))


(defun (:ti-sys restore-source-directory) (from-dir)
  (append '("TI-RELEASE-2") (if (atom from-dir) (list from-dir) from-dir)))

(defun (:gjc-root restore-source-directory) (from-dir)
  (append '("GJC") (if (atom from-dir) (list from-dir) from-dir)))

(defun (:ti-sys-mit restore-source-directory) (from-dir)
  (append '("TI-RELEASE-2") (if (atom from-dir) () (cdr from-dir))))


(defun (:mit-stuff restore-source-directory) (from-dir)
  (append '("MIT-STUFF") (cond ((atom from-dir) (list from-dir))
                               ((null (cdr from-dir)) from-dir)
                               ('else (cdr from-dir)))))


(defun restore-source-tape (to-host &optional (transform-directory :lmi-sys))
  (do ()
      ((not (restore-one-source-file to-host transform-directory)))))


(defun restore-one-source-file (to-host transform-directory)
  (with-open-stream (s (mt:make-mt-file-stream))
    (when (errorp s)
      (format t "~&RESTORE COMPLETE: ~A~%" (send s :report-string))
      (return-from restore-one-source-file nil))
    (let ((byte-size (send s :byte-size))
          (characters (send s :characters))
          (author (send s :author))
          (creation-date (send s :creation-date))
          (truename (send s :truename)))
      (let ((target-pathname (send truename :new-pathname :host to-host
                                   :directory (funcall (get transform-directory 'restore-source-directory)
                                                       (send truename :directory)))))
        (format t "~&~A => ~A" truename target-pathname)
        (cond ((probe-file-create-dir target-pathname)
               (format t " already exists"))
              ('else
               (restore-one-file-1 s target-pathname (list :byte-size byte-size
                                                           :characters characters
                                                           :author author
                                                           :creation-date creation-date)))))))
  t)

(defun probe-file-create-dir (x)
  (condition-case ()
      (open x :direction nil)
    (fs:directory-not-found
     (let ((name (send x :new-pathname :name nil :type nil :version nil)))
       (format t "~&Creating directory for ~A" name)
       (create-directory-recursive name))
     ())
    (fs:file-lookup-error
     ())))

(defun create-directory-recursive (pathname)
  (condition-case ()
      (fs:create-directory pathname)
    (fs:directory-not-found
     (let ((need (send pathname :new-directory (butlast (send pathname :directory)))))
       (format t "~&Also must create superior ~A" need)
       (create-directory-recursive need))
     (fs:create-directory pathname))))




(DEFVAR *RESTORE-ONE-FILE-PROPERTIES* '(:AUTHOR :CREATION-DATE)
  "Properties to carry over to the new file via a :CHANGE-PROPERTIES message")

(DEFUN RESTORE-ONE-FILE-1 (INSTREAM OUTPUT-PATHNAME PLIST &AUX TIME)
  (SETQ TIME (TIME))
  (WITH-OPEN-FILE (OUTSTREAM OUTPUT-PATHNAME
                             :DIRECTION :OUTPUT
                             :CHARACTERS (GETF PLIST :CHARACTERS)
                             :BYTE-SIZE (GETF PLIST :BYTE-SIZE))
    (LET ((RESTORE-PLIST NIL)
          (NOT-VALUE (LIST NIL)))
      (DOLIST (P *RESTORE-ONE-FILE-PROPERTIES*)
        (LET ((VALUE (GETF PLIST P NOT-VALUE)))
          (OR (EQ VALUE NOT-VALUE)
              (SETF (GETF RESTORE-PLIST P) VALUE))))
      (WHEN RESTORE-PLIST
        (LEXPR-SEND OUTSTREAM :CHANGE-PROPERTIES NIL RESTORE-PLIST)))
    (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
    (FORMAT T "~%  ==> ~A (~$ seconds)~%" (SEND OUTSTREAM :TRUENAME) TIME)
    (SETQ TIME (TIME))
    (DO ((BUFFER)
         (OFFSET)
         (LIMIT)
         (BYTES 0))
        (())
      (MULTIPLE-VALUE (BUFFER OFFSET LIMIT)
        (SEND INSTREAM :READ-INPUT-BUFFER))
      (WHEN (NULL BUFFER)
        (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
        (FORMAT T
                " copy: ~$ seconds, ~$ bytes per second~%"
                TIME (QUOTIENT BYTES (IF (ZEROP TIME) 1 TIME)))
        (RETURN NIL))
      (INCF BYTES (- LIMIT OFFSET))
      (SEND OUTSTREAM :STRING-OUT BUFFER OFFSET LIMIT)
      (SEND INSTREAM :ADVANCE-INPUT-BUFFER))
    (SETQ TIME (TIME)))
  (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
  (FORMAT T " output-close: ~$ seconds" TIME))

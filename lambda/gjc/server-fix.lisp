;-*- Mode:LISP; Package:FS; Readtable:ZL; Base:8; Lowercase:T; Patch-File:T -*-


;; this fix lets symbolics systems send symbolics-style pathnames to
;; an lmi machine. Those symbolics machines then see only symbolics
;; style pathnames in directory lists, probes, etc.
;; 21-Nov-86 11:16:37 -gjc



(defun lmfs-parse-for-server (string)
  (condition-case (result)
      (let ((fhost (and (boundp 'conn)
                        conn
                        (si:get-host-from-address (chaos:foreign-address conn) :chaos))))
        (cond ((and fhost
                    (eq :lmfs (send-if-handles fhost :file-system-type))
                    (or (get fhost 'wants-lmfs-pathnames-only)
                        (string-search ">" string)))
               (putprop fhost t 'wants-lmfs-pathnames-only)
               (send (fs:merge-pathname-defaults
                       string
                       (fs:default-pathname nil fhost nil nil t)
                       ':unspecific ':newest)
                     :new-pathname :host si:local-host))
              ('else
               (fs:merge-pathname-defaults
                 string
                 local-host-pathname
                 ':unspecific ':newest))))
    (pathname-error
     result)))


(defun wants-lmfs-pathnames-only ()
  (let ((fhost (and (boundp 'conn)
                    conn
                    (si:get-host-from-address (chaos:foreign-address conn) :chaos))))
    (when (and fhost (get fhost 'wants-lmfs-pathnames-only))
      fhost)))


(defun other-guy-pathname (pathname fhost)
  (let ((p (make-pathname :host fhost
                          :device nil
                          :directory (send pathname :directory)
                          :name (send pathname :name)
                          :type (send pathname :type)
                          :version (send pathname :version))))
    (cond ((eq (send pathname :directory) :root)
           ;; kludge fix. bug work around
           (parse-pathname (string-append fhost
                                          ":>"
                                          (send pathname :name)
                                          "."
                                          (send pathname :type)
                                          "."
                                          (format nil "~D" (send pathname :Version)))
                           fhost))
          ('else
           p))))




(defun server-print-pathname (pathname)
  (let ((fhost (wants-lmfs-pathnames-only)))
    (cond ((not fhost)
           (if (eq (send pathname ':host) si:local-host)
               (send pathname ':string-for-host)
             (send pathname ':string-for-printing)))
          ('else
           (send (other-guy-pathname pathname fhost)
                 :string-for-host)))))



(defun server-dirlist-single (props pn conn-stream &aux (*print-base* 10.) (*nopoint t))
  (format conn-stream "~%")
  (if pn (let ((fhost (wants-lmfs-pathnames-only)))
           (cond ((not fhost)
                  (format conn-stream "~A~%"
                          (send pn ':string-for-host)))
                 ('else
                  (format conn-stream "~A~%"
                          (send (other-guy-pathname pn fhost)
                                :string-for-host))))))
  (tv:doplist (props prop ind)
    (format conn-stream "~A " ind)
    (if (eq ind ':settable-properties)
        (loop for x on prop do (princ (car x) conn-stream) (if (cdr x) (tyo #/SP conn-stream)))
        (or (dolist (spec fs:*known-directory-properties*)
              (if (memq ind (cdr spec))
                  (progn
                    (funcall (or (cadar spec) #'princ) prop conn-stream)
                    (return t))))
            (princ prop conn-stream)))
    (format conn-stream "~%")))



(defun file-server-close-connection (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
        (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
        (let ((direction (server-dataproc-comm-iotype data))
              (opening (server-dataproc-comm-opening data))
              (cell (server-dataproc-comm-cell data)))
          (cond ((null opening)
                 (format conn-stream "~A ~A ERROR UFH F No opening on handle ~A" tid fh fh))
                (t
                 (if (eq direction 'input)
                     (rplaca cell 'wsync))
                 (%store-conditional (locf (car cell)) 'async-mark 'async-abort)
                 (cond ((eq opening 'directory)
                        (format conn-stream "~A ~A CLOSE" tid fh))
                       (t
                        (selectq server-protocol-version
                          (0
                           (format conn-stream "~A ~A CLOSE ~D ~A ~D~%~A~%"
                                   tid fh
                                   (send (send opening ':truename) ':version)
                                   (cv-time (send opening ':creation-date))
                                   (send opening ':length)
                                   (server-print-pathname (send opening ':truename))))
                          (1
                           (format conn-stream "~A ~A CLOSE ~A ~D~%~A~%"
                                   tid fh
                                   (cv-time (send opening ':creation-date))
                                   (send opening ':length)
                                   (server-print-pathname (send opening ':truename)))))))
                 (send conn-stream ':force-output)
                 (if (eq direction 'input)
                     (process-wait "Read Finish" #'null-car cell)
                   (process-wait "Write Finish" #'null-car cell))       ;!!
                 (setf (server-dataproc-comm-opening data) nil)
                 (cond ((not (eq opening 'directory))
                        (send opening ':close)
                        (setq server-openings (delq opening server-openings))))))))))



(DEFVAR *KNOWN-DIRECTORY-PROPERTIES*
  '(((PARSE-DIRECTORY-BOOLEAN-PROPERTY PRIN1 :BOOLEAN)
     . (:DELETED :DONT-DELETE :DONT-DUMP :DONT-REAP :DELETE-PROTECT :SUPERSEDE-PROTECT
        :NOT-BACKED-UP :OFFLINE :TEMPORARY :CHARACTERS :DUMPED :DIRECTORY
        ;; Supported by LM
        :QFASLP :PDP10 :MAY-BE-REAPED))
    ((SUBSTRING PRINC :STRING) . (:ACCOUNT :AUTHOR :LINK-TO :PHYSICAL-VOLUME :PROTECTION
                                  :VOLUME-NAME :PACK-NUMBER :READER :DISK-SPACE-DESCRIPTION
                                  :INCREMENTAL-DUMP-TAPE :COMPLETE-DUMP-TAPE))
    ((ZWEI:PARSE-NUMBER PRINT-DECIMAL-PROPERTY :NUMBER)
     . (:BLOCK-SIZE :BYTE-SIZE :GENERATION-RETENTION-COUNT :LENGTH-IN-BLOCKS
        :LENGTH-IN-BYTES :DEFAULT-GENERATION-RETENTION-COUNT))
    ((PARSE-DIRECTORY-DATE-PROPERTY PRINT-DIRECTORY-DATE-PROPERTY :DATE)
     . (:CREATION-DATE :MODIFICATION-DATE))
    ((PARSE-DIRECTORY-DATE-PROPERTY PRINT-UNIVERSAL-TIME-OR-NEVER-FOR-DIRLIST :DATE-OR-NEVER)
     . ( :REFERENCE-DATE :INCREMENTAL-DUMP-DATE :COMPLETE-DUMP-DATE :DATE-LAST-EXPUNGED
         :EXPIRATION-DATE))
    ((PARSE-SETTABLE-PROPERTIES PRINT-SETTABLE-PROPERTIES)
     . (:SETTABLE-PROPERTIES :LINK-TRANSPARENCIES :DEFAULT-LINK-TRANSPARENCIES))
    ((PARSE-DIRECTORY-FREE-SPACE PRINT-DIRECTORY-FREE-SPACE) . (:PHYSICAL-VOLUME-FREE-BLOCKS))
    ((TIME:PARSE-INTERVAL-OR-NEVER TIME:PRINT-INTERVAL-OR-NEVER :TIME-INTERVAL-OR-NEVER)
         . (:AUTO-EXPUNGE-INTERVAL))
    ))


(DEFUN PRINT-UNIVERSAL-TIME-OR-NEVER-FOR-DIRLIST (TIME STREAM)
  (IF (NULL TIME) (PRINC "never" STREAM)
    (TIME:PRINT-UNIVERSAL-TIME TIME STREAM NIL ':mm//dd//yy)))


;; this is a needed bug fix.
;; strict time protocol says that the year must be given as two digits.
;; What happens 16 years from now?

(DEFUN PRINT-DIRECTORY-DATE-PROPERTY (UT STREAM)
  (if (numberp ut)      ;"defensive"
      (MULTIPLE-VALUE-BIND (SEC MIN HR DAY MON YR)
          (TIME:DECODE-UNIVERSAL-TIME UT)
        (FORMAT STREAM "~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                MON DAY (MOD YR 100.) HR MIN SEC))))

;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:10; Readtable:ZL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.


Access definitions to call procedures in the FTP package.
In a sense the interface between the wonderful "Lispmachine Way of Doing Things"
and the crude, cold, harsh "Real World."

|#

(defflavor ftp-access
           ()
           (host-unit-access-mixin directory-list-mixin basic-access))


#|
HOST-UNIT-ACCESS-MIXIN has a method :GET-HOST-UNIT that we need
to shadow. This method is to return a valid host unit.  If no units, make one.
If any unit is still open, use it.
Errors if fails to connect unless noerror-p is T.
|#

(defmethod (ftp-access :get-host-unit) (&optional noerror-p)
  ;; change the semantics slightly to both GET and RESERVE a unit.
  (let ((unit (do ((l host-units (cdr l)))
                  ((null l)
                   (send (send self :new-host-unit noerror-p) :reserve))
                (when (send (car l) :reserve)
                  (return (car l))))))
    (send unit :setup-control-connection)
    unit))

(defmacro with-host-unit ((unit) &body body &aux (temp (gentemp unit)))
  "Use this to get a host unit making sure it is deallocated afterwards"
  `(let ((,temp))
     (unwind-protect
         (let ((,unit (setq ,temp (send self :get-host-unit))))
           ,@body)
       (and ,temp (send ,temp :free)))))

(defmacro allocating-host-unit ((unit) &body body &aux (temp (gentemp unit))
                                (normalp (gentemp 'normalp)))
  "use this to get a host unit, which is deallocated if exited abnormally"
  `(let (,temp ,normalp)
     (unwind-protect
         (let ((,unit (setq ,temp (send self :get-host-unit))))
           (multiple-value-prog1 (progn ,@body) (setq ,normalp t)))
       (and ,temp                               ;We got a host unit...
            (not ,normalp)                      ;Body did not complete normally...
            (send ,temp :reset)))))             ;We must reset it.  control connection in unknown state.

(defmacro command-using-unit ((unit error-p) &body body)
  `(handling-file-errors (,error-p)
     (with-host-unit (,unit)
       ,@body)))

(define-file-access ftp-access 0.8
  (:network :internet)
  (:protocol "FTP"))

(defmethod (ftp-access :access-description) ()
  "TCP//FTP")

(defmethod (ftp-access :host-unit-flavor) ()
  'ftp-host-unit)

(defun ftp-lose (file operation &optional proceedable)
  (file-process-error 'fs:unknown-operation "Unknown operation" file proceedable
                      nil :operation operation))

(defprop ftp-lose t :error-reporter)


#|
There is the whole question of what FTP modes to open files in.
Image mode is the fastest for both the FTP server and us, and
probably provides the easiest translation to other formats,
where we take full responsibility for doing the translation.
However, then we might have to know not only operating system
types (e.g. if a Unix, then there is a simple one-to-one translation from
IMAGE mode to LISPMACHINE-CHARACTERS) but perhaps also processor type,
to account for byte ordering in the case of 16 bit bytes.
Only time will tell what the best approach is.


Note on subtle file listing kludgery. We need a good simple data-driven
approach to the protocol of dealing with what operations the remote ftp
server provides. Unfortunately there is no defined HANDLE <OP>
command, instead the only way of know if a command is handled is to try
it and see what the reply is. This sort of thing complicates the control
structure of our code considerably. We would like an XSTOR, and XRETR,
that would provide the TRUENAME and other file properties in the reply
line. XLIST would provide a universal format for at least file properties,
since we can already handle file name parsing and many systems are very
touchy about that sort of thing. Barring that, before each STOR or RETR
we need to do a LIST, find the truename and properties of the file
in question and then do the STOR or RETR. Problems:
 * If we want to STOR FOO.BAR, do a LIST and see FOO.BAR.3, then decide
   that FOO.BAR.4 will be our truename, at the same time that somebody
   else is doing the same thing then we will either get an error
   storing FOO.BAR.4, or not get an error, or somehow get confusing results.
 * The LIST operation is usually much slower than a RETR or STOR, because
   of extra overhead of creating a listing fork process (under unix),
   of extra random disk accesses to get at the directory, and because
   in listing FOO.BAR.* there may be an unbounded number of versions to
   parse through.

Presently we are doing a LIST via "lazy evaluation" only on demand.
This is prone to the multiple-store confusion problem. e.g. we open
FOO.BAR, and really get FOO.BAR.3, then list later on and see FOO.BAR.4
because somebody else wrote it out behind our back.

If you set *FTP-DIRECTORY-LINE-PARSERP* to NIL you will avoid all attempts
to support truenames and file properties.

|#

(defvar *ftp-directory-line-parserp* t
  "Set to NIL to disable parsing kludge or set to a list of those system types to try parsing on")


#|
With *ftp-probe-before-open-p* set to T, a probe stream is associated with each open
FTP-STREAM before the file is actually opened.  This allows a single FTP-ACCESS host
unit to be used for the probe and for the open file, and only has one data connection
open at once to deal with a particular opening of a file.

*ftp-probe-before-open-directions* determines which directions this occurs for.
:input is safe and efficient.
:output is bogus for two reasons:
  (1) file might not already exist.
  (2) creation date is screwed up.

With *ftp-probe-before-open-p* set to NIL, the probe stream is opened to find the
:truename after the file is open.  This means that two FTP-ACCESS host units will be
used -- but has advantages.  When (make-system), for example, loads a whole series
of files from one host, the QFASL files pretty much all end up coming in on one host
unit and the probes on the other -- and FTP doesn't need to keep sending "TYPE A"
and "TYPE L 16" commands to switch transfer types.
|#

(defvar *ftp-probe-before-open-p* nil "If T then probe-stream before opening")

(defvar *ftp-probe-before-open-directions* '(:input))

(defmethod (ftp-access :open) (file pathname
                               &key &optional
                               (direction :input)
                               (characters t)
                               (byte-size :default)
                               (error t)
                               if-exists if-does-not-exist
                               raw-directory-list
                               probe-directory-list
                               &allow-other-keys &aux probe-stream)
  (cond ((memq direction '(nil :probe))
         ;;This was <always> returning the probe-stream before -- and that meant
         ;;that a user-level probe was useless.  Now, process :error and
         ;;:if-does-not-exist values.  Callers (including this,
         ;;recursively, below!)  that want the old behavior should use
         ;;the keyword :IF-DOES-NOT-EXIST :STREAM.
         (let ((probe-stream
                 (make-instance 'ftp-probe-stream
                                :truename nil
                                :pathname pathname
                                :host host
                                :raw-data
                                (and (ftp-directory-line-parserp host t)
                                     (with-output-to-string (s)
                                       (with-open-file (d file :raw-directory-list t
                                                          :if-does-not-exist :stream
                                                          :probe-directory-list t
                                                          :error error)
                                         (when (streamp d)
                                           (stream-copy-until-eof d s))))))))
           (if (send probe-stream :truename)
               probe-stream                     ;;File exists.
             ;;File does not exist.  Result depends on :IF-DOES-NOT-EXIST.
             ;;Note new value, :STREAM, used below to setup recursive call to here.
             (progn
               ;;Unify :error and :if-does-not-exist keywords
               (when (and error (null if-does-not-exist)
                          (setq if-does-not-exist :error)))
               (case if-does-not-exist
                 (:stream probe-stream)
                 ;;A "standard" value
                 (:create
                  (error "Cannot create ~A on OPEN for ~A" pathname direction))
                 (:error
                  (file-process-error 'file-not-found
                                      (format nil "File not found for ~A" pathname)
                                      pathname
                                      nil (null error) :open))
                 (t nil))))))
        ('else
         (when (and (memq direction *ftp-probe-before-open-directions*)
                    (not raw-directory-list)
                    (or (eq *ftp-probe-before-open-p* t)
                        (memq (send host :system-type)
                              *ftp-probe-before-open-p*)
                        (get host :ftp-probe-before-open-p)))
           (setq probe-stream (open file :direction nil :error error :if-does-not-exist :stream))
           (when (and (eq direction :output)
                      (memq (send file :version) '(nil :newest)))
             (cond ((not (send probe-stream :truename))
                    (send probe-stream :set-truename
                          (send file :new-version 1)))
                   ((numberp (send (send probe-stream :truename) :version))
                    ;; kludge-a-rama. The *FTP-PROBE-BEFORE-OPEN-P* is useful
                    ;; if the host cant take so many simultaneous servers
                    ;; as would be created if we probe *after* the opening of
                    ;; the file data connection. The most sensible solution to
                    ;; all of this is to use another file protocol.
                    (send probe-stream :set-truename
                          (send (send probe-stream :truename)
                                :new-version (1+ (send (send probe-stream :truename)
                                                       :version))))))
             (when (not (plist probe-stream))
               (setf (plist probe-stream)
                     `(:creation-date ,(time:get-universal-time))))))
         (handling-file-errors (error)
           (allocating-host-unit (unit)
             (let ((command nil)
                   (buffers nil)
                   (optimistic nil)
                   (tcp:*tcp-stream-whostate* (cond (probe-directory-list "Probe")
                                                    (raw-directory-list "Directory")
                                                    (t "Open"))))
               (when probe-directory-list
                 (setq buffers 1)
                 (setq optimistic t))
               (cond (raw-directory-list
                      (setq command "LIST ~A"))
                     ((eq direction :output)
                      (setq command (caseq if-exists
                                      (:append "APPE ~A")
                                      (t "STOR ~A"))))
                     ((eq direction :input)
                      (setq command "RETR ~A")))
               (when (eq byte-size :default)
                 (cond ((eq characters t)
                        (setq byte-size 8))
                       ((eq characters nil)
                        (setq byte-size 16.))
                       ((eq characters :default)
                        ;; almost no way of getting this information
                        ;; unix uses magic numbers, on VMS maybe character if
                        ;; record format variable length CR.
                        ;; for now kludge it so that at least load works on
                        ;; qfasl files.
                        (multiple-value-setq (characters byte-size)
                          (kludge-ftp-characterp file pathname)))))
               (cond ((not characters)
                      (send unit :setbinary byte-size))
                     ((eq (send host :system-type) :lispm)
                      (send unit :setbinary 8))
                     ('else
                      ;; in fact if the host is a unix host then
                      ;; binary mode with our own easier-to-do translation
                      ;; would be better than forcing kludgy ascii
                      ;; translation to happen on both hosts.
                      (send unit :setascii)))
               (ftp-access-operation file unit :initconn direction buffers optimistic)
               (multiple-value-bind (c code)
                   (send unit :command
                         command (fixup-ftp-string-for-host
                                   (send file :string-for-host)
                                   file))
                 (cond ((= c (tcp-application:sym ftp:PRELIM))
                        (ftp-access-operation file unit :dataconn
                                              file pathname direction byte-size
                                              raw-directory-list probe-stream))
                       ((and (= code 530.)
                             (progn (send unit :login unit t host)
                                    ;; some hosts forget about the PORT
                                    ;; command after a USER command is
                                    ;; given, so give it again.
                                    (ftp-access-operation file unit :initconn direction buffers optimistic)
                                    (= (tcp-application:sym ftp:PRELIM)
                                       (send unit :command command
                                             (send file :string-for-host)))))
                        (ftp-access-operation file unit :dataconn
                                              file pathname direction byte-size
                                              raw-directory-list probe-stream))
                       ('else
                        (send unit :close-dataconn nil)
                        (send unit :ftp-error "Access error" file))))))))))

(defun fixup-ftp-string-for-host (string pathname)
  (selectq (send (send pathname :host) :system-type)
    (:vms
     (if (string-search ";" string)
         string
       (string-append string ";")))
    (:lispm
     (string-append (send (send pathname :host) :name)
                    ":"
                    string))
    (t
     string)))

(defun kludge-ftp-characterp (file pathname)
  pathname
  (cond ((mem #'string-equal (send file :canonical-type)
              '("QFASL" "XFASL" "NFASL" "BIN"))
         (values nil 16))
        ('else
         (values t 8))))

(defun ftp-access-operation (file unit operation &rest args)
  (unless (send unit :reserved-p)
    (cerror "continue" "access operation on unreserved host unit"))
  (or (lexpr-send unit operation args)
      (send unit :ftp-error operation file)))

(defmethod (ftp-access :rename) (file new-pathname error-p)
  (command-using-unit (unit error-p)
    (let ((tcp:*tcp-stream-whostate* "Rename"))
      (ftp-access-operation file unit :renamefile
                            (send file :string-for-host)
                            (send new-pathname :string-for-host))))
  ;; should be real truename here...
  new-pathname)

(defmethod (ftp-access :delete) (file error-p)
  (command-using-unit (unit error-p)
    (let ((tcp:*tcp-stream-whostate* "Delete"))
      (ftp-access-operation file unit :delete (send file :string-for-host))))
  ;; should be real truename here
  file)

(defmethod (ftp-access :complete-string) (file string options)
  file options
  (values string t))

;;; This can only be done via a new command
(defmethod (ftp-access :change-properties) (file error-p &rest properties)
  (if (change-properties-is-undelete properties)
      (command-using-unit (unit error-p)
        (let ((tcp:*tcp-stream-whostate* "Undelete"))
          (ftp-access-operation file unit :undelete (send file :string-for-host))))
    (handling-file-errors (error-p)
      (ftp-lose file :change-properties))))

(defun change-properties-is-undelete (properties)
  (do ((list properties (cddr list))
       (undelete-p))
      ((null list) undelete-p)
    (case (car list)
      (:deleted
       (setq undelete-p (not (cadr list))))
      (t
       (return nil)))))

(defmethod (ftp-access :homedir) (user)
  (command-using-unit (unit t)
    (let ((tcp:*tcp-stream-whostate* "Home Directory"))
      (send unit :homedir user))))

;;; Isn't there a non-standard command for this ?
(defmethod (ftp-access :create-link) (file link-to error)
  link-to
  (handling-file-errors (error)
    (ftp-lose file :create-link)))

(defmethod (ftp-access :expunge) (pathname error)
  (command-using-unit (unit error)
    (let ((tcp:*tcp-stream-whostate* "Expunge"))
      (ftp-access-operation pathname unit :expunge (send pathname :string-for-host)))))

(defmethod (ftp-access :remote-connect) (file error access-mode &optional unit)
  ;; Connect to the directory named by FILE.  If ACCESS-MODE is T, then do TOPS-20
  ;; access.  If UNIT is given, connect for just that unit.  (This argument should be
  ;; be ignored if it does not make sense the access object.
  ;; some file servers dont support CWD. A fairly useless operation in
  ;; lispmachine usage anyway.
  access-mode unit
  (handling-file-errors (error)
    (ftp-lose file :remote-connect)))

(defmethod (ftp-access :create-directory) (file error)
  (command-using-unit (unit error)
    (let ((tcp:*tcp-stream-whostate* "Create Directory"))
      (ftp-access-operation file unit :makedir (send file :string-for-host)))))

(defmethod (ftp-access :directory-list) (given-pathname options)
  (let ((pathname (normalize-ftp-directory-list-pathname given-pathname host))
        (no-error-p nil)                        ;T to not generate errors
        (deleted-p nil)                         ;T to include deleted files
        (sorted-p nil)                          ;T to sort generated list
        (tcp:*tcp-stream-whostate* "Directory List"))
    (do ((l options (cdr l)))
        ((null l))
      (case (car l)
        (:noerror (setq no-error-p t))
        (:sorted (setq sorted-p t))
        (:deleted (setq deleted-p t))
        (:fast nil)                             ;Not supported, not a problem
        (otherwise (ferror nil "~S is not a known DIRECTORY option" (car l)))))
    (with-open-file (s pathname :raw-directory-list t :error (not no-error-p))
      (if (errorp s)
          s
        (let ((dir-list (ftp-access-canonicalize-directory-list
                          (cons
                            `(nil :pathname ,pathname)
                            (do ((line) (eofp) (list))
                                (eofp (nreverse list))
                              (multiple-value (line eofp) (send s :line-in))
                              (when line
                                (let ((plist (ftp-parse-directory-list-line line host pathname)))
                                  (when plist
                                    (let ((truename (getf (cdr plist) :truename))
                                          (deleted (getf (cdr plist) :deleted)))
                                      (when (or (not deleted) deleted-p)
                                        (when truename
                                          (setf (car plist) truename))
                                        (push plist list)))))))))))
          (when sorted-p
            (let ((null-elem (assq nil dir-list)))
              (and null-elem (setq dir-list (delq null-elem dir-list)))
              (setq dir-list (sortcar dir-list #'pathname-lessp))
              (and null-elem (push null-elem dir-list))))
          dir-list)))))

(defmethod (ftp-access :multiple-file-plists) (files options)
  (do* ((list files (cdr list))
        (file (car list) (car list))
        (error-p (not (memq :noerror options)))
        (result nil))
       ((null list)
        (nreverse result))
    (let* ((pathname (normalize-ftp-directory-list-pathname file host))
           (tcp:*tcp-stream-whostate* "File Properties")
           (plist (with-open-file (s pathname :raw-directory-list t :error error-p)
                    (if (errorp s)
                        s
                      (ftp-access-canonicalize-directory-list
                        (cons
                          `(nil :pathname ,pathname)
                          (do ((line) (eofp) (list))
                              (eofp (nreverse list))
                            (multiple-value (line eofp) (send s :line-in))
                            (when line
                              (let* ((plist (ftp-parse-directory-list-line line host pathname)))
                                (when (and plist (car plist))
                                  (push plist list)))))))))))
      (if (errorp plist)
          (push plist result)
        (dolist (elt plist)
          (push elt result))))))

(defun ftp-access-canonicalize-directory-list (l)
  (do ((new-header)(temp))
      ((not (setq temp (assq nil l)))
       (cons (cons nil new-header) l))
    (setq new-header (append (cdr temp) new-header))
    (setq l (delq temp l))))

(defun normalize-ftp-directory-list-pathname (p h)
  ;; maybe add a new kind of message, :string-for-ftp-list-command
  (selectq (send h :system-type)
    (:unix
     (cond ((and (eq :wild (send p :name))
                 (memq (send p :type) '(:unspecific :wild nil)))
            ;; this would result in a :string-for-host
            ;; of /foo/bar/*, which does directory descent,
            ;; which is not what we want.
            (send p :new-pathname
                  :name :unspecific :type :unspecific :version :unspecific))
           ('else
            p)))
    (t p)))

;; what kind of :directory-line-parser property to look for
;; should depend on additional information in the site file or
;; host table, perhaps, because the exact syntax of directory
;; listings could be system version dependant.

(defun ftp-directory-line-parserp (host &optional override)
  "Return a directory listing parsing function for those systems that support it"
  (and (or override
           (eq *ftp-directory-line-parserp* t)
           (memq (send host :system-type) *ftp-directory-line-parserp*))
       (get (send host :system-type)
            :directory-line-parser)))

(defun ftp-parse-directory-list-line (line host pathname)
  (let ((parser (ftp-directory-line-parserp host t))
        (trimmed-line (string-right-trim '(#\return) line)))
    (if parser
        (let ((p (funcall parser trimmed-line host (send pathname :translated-pathname))))
          (if (null (car p))
              p
            (append `(,pathname :truename) p)))
      (let ((p (parse-pathname trimmed-line host)))
        (list p :pathname p)))))

(defun string-find-token (string start &aux (ws '(#\space #\tab #\return #\line)))
  "Returns start and end of next non-whitespace object in string"
  (declare (values start end))
  (cond ((null (setq start (string-search-not-set ws string start)))
         nil)
        ('else
         (values start
                 (or (string-search-set ws string start)
                     (string-length string))))))


(defprop :unix unix-directory-line-parser :directory-line-parser)

(defun unix-directory-line-parser (line host pathname)
  (caseq (send host :file-system-type)
    (:unix-sgi
     (unix-sgi-directory-line-parser line host pathname))
    (t
     (unix-4.2bsd-directory-line-parser line host pathname))))

(defun unix-4.2bsd-directory-line-parser (line host pathname &aux start end plist)
;;-rw-rw-rw-  1 gjc      wheel       21597 Apr 11 09:05 /lmi/gjc/ftp.lisp
;; but the above line was before we fixed the string being passed to
;; the list command. We not get a line where there is no directory given,
;; so must get the directory out of the original pathname.
  (cond ((> (length line) 20)
         (setq start 0)
         ;; protection, or mode.
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :protection (substring line start end) plist))
         (setq start end)
         ;; link count
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :link-count (parse-number line start end) plist))
         (setq start end)
         ;; owner
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :author (substring line start end) plist))
         (setq start end)
         ;; group
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :group (substring line start end) plist))
         (setq start end)
         ;; size-in-bytes
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :length-in-8b-bytes (parse-number line start end) plist))
         (setq start end)
         ;; month
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :month (substring line start end) plist))
         (setq start end)
         ;; day
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :day (parse-number line start end) plist))
         (setq start end)
         ;; hour-minute
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :hour-minute (substring line start end) plist))
         (setq start end)
         ;; namestring
         (multiple-value (start end) (string-find-token line start))
         ;; normalize
         (setf (getf plist :pathname) (send (parse-pathname line
                                                            host
                                                            pathname
                                                            start
                                                            end)
                                            :new-directory
                                            (send pathname :directory)))
         (setf (getf plist :creation-date)
               (time:parse-universal-time (format nil "~A ~D ~A"
                                                  (getf plist :month)
                                                  (getf plist :day)
                                                  (getf plist :hour-minute))
                                          0
                                          nil
                                          nil))
         (setf (getf plist :length-in-blocks)
               (ceiling (getf plist :length-in-8b-bytes) 1024))
         (multiple-value-bind (characters byte-size)
             (kludge-ftp-characterp (getf plist :pathname) nil)

           (setf (getf plist :length-in-bytes)
                 (floor (* 8 (getf plist :length-in-8b-bytes))
                        byte-size))
           (setf (getf plist :byte-size) byte-size)
           (setf (getf plist :characters) characters))
         (if (char-equal #\D (aref (getf plist :protection) 0))
             (setf (getf plist :directory) t))
         (cons (getf plist :pathname) plist))))


(defun unix-sgi-directory-line-parser (line host pathname &aux start end plist)
  ;; drwxr-xr-x 3 games       528 May  8 08:02 games
  ;; drwxrwxrwx11 sys        1264 May  8 08:25 include
  ;; so beware of the links field running into the protection field
  (cond ((> (length line) 20)
         (setq plist (list* :protection (substring line 0 10) nil))
         (setq start 10)
         ;; link count
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :link-count (parse-number line start end) plist))
         (setq start end)
         ;; owner
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :author (substring line start end) plist))
         (setq start end)
         ;; size-in-bytes
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :length-in-8b-bytes (parse-number line start end) plist))
         (setq start end)
         ;; month
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :month (substring line start end) plist))
         (setq start end)
         ;; day
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :day (parse-number line start end) plist))
         (setq start end)
         ;; hour-minute
         (multiple-value (start end) (string-find-token line start))
         (setq plist (list* :hour-minute (substring line start end) plist))
         (setq start end)
         ;; namestring
         (multiple-value (start end) (string-find-token line start))
         ;; normalize
         (setf (getf plist :pathname) (send (parse-pathname line
                                                            host
                                                            pathname
                                                            start
                                                            end)
                                            :new-directory
                                            (send pathname :directory)))
         (setf (getf plist :creation-date)
               (time:parse-universal-time (format nil "~A ~D ~A"
                                                  (getf plist :month)
                                                  (getf plist :day)
                                                  (getf plist :hour-minute))
                                          0
                                          nil
                                          nil))
         (setf (getf plist :length-in-blocks)
               (ceiling (getf plist :length-in-8b-bytes) 1024))
         (multiple-value-bind (characters byte-size)
             (kludge-ftp-characterp (getf plist :pathname) nil)

           (setf (getf plist :length-in-bytes)
                 (floor (* 8 (getf plist :length-in-8b-bytes))
                        byte-size))
           (setf (getf plist :byte-size) byte-size)
           (setf (getf plist :characters) characters))
         (if (char-equal #\D (aref (getf plist :protection) 0))
             (setf (getf plist :directory) t))
         (cons (getf plist :pathname) plist))))

(defprop :vms excelan-8040-vms-directory-line-parser :directory-line-parser)

(defun excelan-8040-vms-directory-line-parser (line host pathname &aux start end p e)
;; this implementation will print out
;;RWE-  [201,001]            3/3        27-JUN-1985 12:57       exos.dir
;;Or
;;RWED  [201,001]            0/0        16-FEB-1985 17:45       amort.lsp;4
;;Or maybe
;;RWED  [201,001]            0/0        16-FEB-1985 17:45       [.foo]amort.lsp;4
;;So the interpretation depends on the pathname given.
  (cond ((string-search "%" line)
         ;; an error message, which unfortunately isnt reported
         ;; by the ftp server in the normal way.
         ())
        ((string-search "file not found." line)
         ())
        ((= (length line) 0)
         nil)
        ('else
         (setq start 0)
         (multiple-value (start end) (string-find-token line start))
         (setf (getf p :owner-protection) (substring line start end))
         (setq start end)
         (multiple-value (start end) (string-find-token line start))
         (setf (getf p :author) (substring line start end))
         (setq start end)
         (multiple-value (start end) (string-find-token line start))
         (setq e (string-search-char #\/ line start end))
         (setf (getf p :length-in-blocks) (parse-number line start e))
         (setq start end)
         (multiple-value (start end) (string-find-token line start))
         (multiple-value (nil end) (string-find-token line end))
         (setf (getf p :creation-date) (time:parse-universal-time line start end))
         (setq start end)
         (multiple-value (start end) (string-find-token line start))
         (let ((try (fs:parse-pathname line host pathname start end)))
           (setq try (excelan-8040-vms-directory-fixup try pathname))
           (multiple-value-bind (chars byte-size)
               (kludge-ftp-characterp try nil)
             (setf (getf p :length-in-bytes)
                   (floor (* 512 8 (getf p :length-in-blocks)) byte-size))
             (setf (getf p :byte-size) byte-size)
             (setf (getf p :characters) chars))
           (if (string-equal (send try :type) "DIR")
               (setf (getf p :directory) t))
           (if (not (numberp (send try :version)))
               ;; It is a real pain to compensate for this,
               ;; and it shows up dearly in PROBE streams,
               ;; where the alternative is a possibly very expensive
               ;; list of :WILD on the versions, and picking out the
               ;; greatest one. Best get EXCELAN to fix this.
               (setq try (send try :new-version (lsh -1 -1))))
           (cons try p)))))

(defun excelan-8040-vms-directory-fixup (try pathname)
  (setq try (send try :new-pathname
                  :device (send pathname :device)
                  :directory (if (null (send try :directory))
                                 (send pathname :directory)
                               (send try :directory))))
  (cond ((atom (send try :directory)))
        ((mem #'string-equal "" (send try :directory))
         (let ((fd (send try :directory))
               (pd (send pathname :directory)))
           (setq try (send try :new-directory
                           (do ((j 0 (1+ j))
                                (l fd (cdr l)))
                               ((or (null l) (not (string-equal "" (car l))))
                                (append (butlast pd j) l))))))))
  try)

;; somewhat amusing to have to write this.


(defprop :lispm lispm-directory-line-parser :directory-line-parser)


(defun lispm-directory-line-parser (line host pathname)
  (caseq (send host :file-system-type)
    (:lmfs
     (lmfs-directory-line-parser line host pathname))
    (t
     (lmi-lispm-directory-line-parser line host pathname))))


(defun lmfs-directory-line-parser (line host pathname)
  pathname
  (cond ((zerop (length line))
         ())
        ((= #\> (aref line 0))
         (list (parse-pathname line host)))
        (t
         nil)))


#|
The ftp-host-unit contains all the special variables of the "interactive" FTP program.
Most operations simply call the approriate function from FTP.LISP.

|#

(defvar *ftp-host-unit-debug* nil "set to T for default debugging info printing")
(defparameter *ftp-access-record-history* t "Determines whether or not to save FTP command history")

(defflavor ftp-host-unit
           ((ftp:*auto-login* nil)
            (ftp:*trace* *ftp-host-unit-debug*)
            (ftp:*hash* *ftp-host-unit-debug*)
            (ftp:*sendport* t)
            (ftp:*verbose* *ftp-host-unit-debug*)
            (ftp:*debug* *ftp-host-unit-debug*)
            (ftp:*bell* nil)
            (ftp:*glob* t)
            (ftp:*prompt* nil)
            (ftp:*type* 'ftp:ascii)
            (ftp:*struct* 'ftp:file)
            (ftp:*form* 'ftp:non-print)
            (ftp:*mode* 'ftp:stream)
            (ftp:*bytesize* 8.)
            (ftp:*user* nil)
            (ftp:*pass* nil)
            (ftp:*acct* nil)
            (ftp:*history* nil)
            (ftp:*last-reply* nil)
            (ftp:*connected* nil)
            (ftp:*remote-hostname* nil)
            (ftp:*control* nil)
            (ftp:*data* nil)
            (reserve-lock (LIST NIL))
            (file-stream nil)
            (working-directory))
           (basic-host-unit)
  :special-instance-variables)

(defmethod (ftp-host-unit :reserve) ()
  (IF (%STORE-CONDITIONAL (LOCF (CAR RESERVE-LOCK)) NIL T)
      SELF))

(defmethod (ftp-host-unit :reserved-p) ()
  (CAR RESERVE-LOCK))

(defmethod (ftp-host-unit :free) ()
  (RPLACA RESERVE-LOCK NIL))

(defmethod (ftp-host-unit :real-login) (login-p uname-host)
  (lock-host-unit (self)
    (cond (login-p
           (do ((need-password? nil t))
               (nil)
             (multiple-value-bind (username password)
                 (fs:determine-user-id-and-password uname-host host need-password?)
               (when (ftp:cmd-user username password)
                 (setq ftp:*user* nil)          ;Don't leave these visible in host unit
                 (setq ftp:*pass* nil)
                 (setq ftp:*acct* nil)
                 (return))
               (when need-password?
                 ;; The password we used must have been wrong, so forget
                 ;; it and force the system to prompt for a new one.
                 (forget-password username host)))
             ;; Since this password is wrong, flush it
             (unless catch-login-problems-p
               (send self :ftp-error "trying to login" nil))))
          ((send self :open-control-connection-p)
           (send self :close-control-connection)))))

(defmethod (ftp-host-unit :login) (-unit- login-p uname-host)
  (send -unit- :real-login login-p uname-host))

(defmethod (ftp-host-unit :command) (fmt &rest args)
  (setq last-use-time (time))
  (apply #'ftp:command fmt args))

(defmethod (ftp-host-unit :reset) (&optional dont-unlock-lock-p)
  ;; Close all data streams in abort mode, close control connection
  dont-unlock-lock-p ;; we think we can ignore this.
  (when file-stream
    (send file-stream :close :abort))
  (unwind-protect
      (let ((tcp:*tcp-stream-whostate* "Reset Host Unit"))
        (send self :close-control-connection))
    (send self :free)))

(defvar *ftp-host-unit-lifetime* (* 3. 3600.)
  ;; three minutes sounds good, won't give the FTP servers time to wedge.
  "Lifetime for an FTP connection in 60ths of a second")

(defmethod (ftp-host-unit :dormant-p) ()
  (> (time-difference (time) last-use-time) *ftp-host-unit-lifetime*))

(defmethod (ftp-host-unit :dormant-reset) ()
  (cond ((send self :reserved-p))               ;If someone has it reserved, not dormant
        (file-stream)                           ;or if there is an open file-stream
        ((not (send self :dormant-p)))          ;or it hasn't been dormant long enough
        ((send self :reserve)                   ;otherwise, reserve host-unit
         (unwind-protect
             (send self :close-control-connection)      ;And close the control connection
           (send self :free)))
        (t nil)))                               ;Someone else grabbed it -- no longer dormant


;; the :NEW-HOST-UNIT method still calls this.

(defmethod (ftp-host-unit :validate-control-connection)  (&optional no-error-p)
  ;; Check that connection hasn't gone away, making a new one if necessary.  Return
  ;; NIL when failure or host unit busy and NO-ERROR-P equal to T, otherwise
  ;; barf on failure
  no-error-p
  (and (not (send self :reserved-p))
       (send self :setup-control-connection)))


(defmethod (ftp-host-unit :setup-control-connection) ()
  (unless (and ftp:*control* (send ftp:*control* :remote-address))
    (setq ftp:*connected* nil))
  (unless ftp:*connected*
    (setq ftp:*history* (if *ftp-access-record-history* nil :dont-record))
    (ftp:cmd-open (send host :name))
    (send self :real-login t host))
  t)

(defmethod (ftp-host-unit :close-all-files) (&optional (mode :abort))
  (when file-stream
    (format *error-output* "~%Closing ~S" file-stream)
    (send file-stream :close mode)))

(defmethod (ftp-host-unit :open-streams) ()
  (and file-stream
       (list file-stream)))

(defmethod (ftp-host-unit :add-file-stream) (s)
  (if file-stream
      (cerror "add anyway" "already have a file stream"))
  (lock-host-unit (self)
    (setq file-stream s))
  s)

(defmethod (ftp-host-unit :remove-file-stream) (the-file-stream)
  (lock-host-unit (self)
    (when file-stream                           ;Don't bother if already gone
      (when (not (eq file-stream the-file-stream))
        (cerror "remove anyway" "removing ~S but current stream is ~S"
                the-file-stream file-stream))
      (setq file-stream nil))))

(defmethod (ftp-host-unit :open-control-connection-p)  ()
  ;; Returns T if the connection is in an open state
  ftp:*connected*)

(defmethod (ftp-host-unit :close-control-connection) ()
  ;; Close connection, for logging out
  (unless (eq (send self :host) si:local-host)
    (ftp:cmd-close))
  (setq working-directory nil)
  (setq ftp:*last-reply* nil)
  (setq ftp:*history* nil))


(defmethod (ftp-host-unit :close-dormant-data-connections) ()
  ;; If they are around, they are not dormant
  nil)


(defmethod (ftp-host-unit :setbinary) (&optional byte-size)
  (ecase byte-size
    (8 (ftp:cmd-binary))
    (16 (ftp:cmd-16bit))))

(defmethod (ftp-host-unit :setascii) ()
  (ftp:cmd-ascii))


(defmethod (ftp-host-unit :cmd) (&rest l)
  (apply #'ftp:ftp l))

(defprop :initconn "init data connection" english)

(defmethod (ftp-host-unit :initconn) (direction &optional buffers optimistic)
  (not (ftp:initconn direction buffers optimistic)))

(defprop :dataconn "Getting real data connection" english)

(defmethod (ftp-host-unit :dataconn) (file pathname direction byte-size rawp ps)
  ;; in fact the only real way to get the truename
  ;; would be to do a directory listing.
  ;; We will in fact do that someday in order to get all the
  ;; info the lispm wants, such as the length, byte size, creation date.
  ;; Info about the protection could also be useful in interpreting
  ;; error messages. For now we keep around the string for host
  ;; just to have a pathname for printing the file stream.
  (let* ((host-unit self)
         (fs (ftp:dataconn direction
                          #'(lambda (raw-stream dir typearg)
                              ;; type can be NIL, :ASCII, :EBCDIC
                              (make-instance
                                (ecase dir
                                  (:input
                                   (cond ((eq typearg :ascii) 'ftp-input-character-stream)
                                         ((= byte-size 8) 'ftp-input-raw-stream)
                                         (t 'ftp-input-binary-stream)))
                                  (:output
                                   (cond ((eq typearg :ascii) 'ftp-output-character-stream)
                                         ((= byte-size 8) 'ftp-output-raw-stream)
                                         (t 'ftp-output-binary-stream))))
                                :byte-size byte-size
                                :actual raw-stream
                                :host-unit host-unit
                                :truename file
                                :pathname pathname
                                :support-plistp (not rawp)
                                :probe-stream ps
                                )))))
    (if fs (send self :add-file-stream fs))
    fs))

(defmethod (ftp-host-unit :close-dataconn) (&optional (normal t))
  (ftp:close-dataconn normal))

(defvar ftp-error-code-condition-alist
        '((550 file-not-found)
          (530 login-problems)
          ;; this error code i invented. it happens when
          ;; FTP servers die randomly.
          (599 network-lossage)
          ))

(defmethod (ftp-host-unit :ftp-error) (how on-what)
  (let* ((reply ftp:*last-reply*)
         (end (and reply (string-search-char #\return reply))))
    (file-process-error (if reply
                            (cadr (assq (parse-number reply 0 3 10 t) ftp-error-code-condition-alist))
                          'network-lossage)
                        (string-append (if (keywordp how)
                                           (or (get how 'english)
                                               (string how))
                                         how)
                                       " /""
                                       (cond (end (substring reply 0 (1- end)))
                                             (reply)
                                             (t ""))
                                       "/"")
                        on-what
                        nil
                        nil)))


(defmethod (ftp-host-unit :homedir) (user &aux s start end homepath)
  (setq working-directory
        (cond (working-directory)
              ((and (ftp:cmd-pwd)
                    (setq s ftp:*last-reply*)
                    (setq start (string-search-char #/" s))
                    (setq end (string-search-char #/" s (1+ start)))
                    (setq homepath (fs:parse-pathname s
                                                      host
                                                      *default-pathname-defaults*
                                                      (1+ start)
                                                      end
                                                      t)))
               homepath)
              ('else
               (send (send host :sample-pathname) :new-directory user)))))


(defmethod (ftp-host-unit :renamefile) (a b)
  (ftp:cmd-rename a b))

(defmethod (ftp-host-unit :delete) (x)
  (ftp:cmd-delete x))

(defprop :makedir "create directory" english)

(defmethod (ftp-host-unit :makedir) (x)
  (ftp:cmd-mkdir x))

(defmethod (ftp-host-unit :expunge) (x)
  (ftp:cmd-expunge x))

(defmethod (ftp-host-unit :undelete) (x)
  (ftp:cmd-undelete x))

(defmethod (ftp-host-unit :getreply) ()
  (ftp:getreply nil))

(defmethod (ftp-host-unit :toggle-debug) ()
  (ftp:cmd-hash)
  (ftp:cmd-verbose)
  (ftp:cmd-debug)
  (ftp:cmd-status))


(defmethod (ftp-host-unit :valid-control-connection-p) ()
  (and ftp:*connected*
       (send self :open-control-connection-p)))


;;;***Depends on ftp-host-unit

(defun lmi-lispm-directory-line-parser (line host pathname &aux start end endn p temp pn)
  (cond ((string-equal "Free=" line :end2 5)
         (list nil :disk-space-description line))
        ((or (< (string-length line) 40)
             (not (or (memq (aref line 0) '(#\space #\tab))
                      (when (and (= (aref line 0) #/D)
                                 (memq (aref line 1) '(#\space #\tab)))
                        (setq line (substring line 1))
                        (setf (getf p :deleted) t)
                        t))))
         nil)
        ('else
;;      A.TEXT         #5   6   5475(8)  !   03/15/85 19:52:04 possibleauthor
         (setq start 0)
         (do ((j 0 (1+ j))
              (n (length line))
              (slashp nil)
              (after#p nil))
             ((= j n) (return-from lmi-lispm-directory-line-parser nil))
           (cond (after#p
                  (cond ((digit-char-p (aref line j)))
                        ('else
                         (return (setq end j)))))
                 (slashp
                  (setq slashp nil))
                 ((= (aref line j) #//)
                  (setq slashp t))
                 ((= (aref line j) #\#)
                  (setq after#p t))))
         (setq pn (send (parse-pathname line host pathname start end)
                        :new-directory (send pathname :directory)))
         (multiple-value (start end) (string-find-token line end))
         (setf (getf p :length-in-blocks) (parse-integer line :start start :end end))
         (multiple-value (start end) (string-find-token line end))
         (cond ((digit-char-p (aref line start))
                (multiple-value (temp endn) (parse-integer line :start start :end end :junk-allowed t))
                (setf (getf p :length-in-bytes) temp)
                (setq start (1+ endn))
                (multiple-value (temp endn) (parse-integer line :start start :end end :junk-allowed t))
                (setf (getf p :byte-size) temp))
               ((string-equal "DIRECTORY" line :start2 start :end2 end)
                (setf (getf p :directory) t)))
         (when (setq temp (string-search-set "0123456789" line end))
           ;;Release 3.0 Lambda's don't give you the creation date or author
           (multiple-value (start end) (string-find-token line temp))
           (multiple-value (temp end) (string-find-token line end))
           (setf (getf p :creation-date) (time:parse-universal-time line start end))
           (unless (when ( end (length line))
                     ;; the author field wont show up if it is the same as the userid.
                     (multiple-value (start end) (string-find-token line end))
                     (when start
                       (setf (getf p :author) (substring line start end))))
             (and (setq temp (send host :access))
                  (setq temp (send temp :host-units))
                  (setq temp (delq nil (mapcar #'(lambda (x)
                                                   (and (typep x 'ftp-host-unit)
                                                        (symeval-in-instance
                                                          x 'ftp:*user*)))
                                               temp)))
                  (setf (getf p :author) (car temp)))))
         (setf (getf p :characters)
               (cond ((getf p :directory)
                      nil)
                     ((not (= (getf p :byte-size) 8))
                      nil)
                     ('else
                      (kludge-ftp-characterp pn nil))))
         (cons pn p))))


#|
These are the simplest possible encapsulated flavor definitions and
methods. If there were a way to return a single stream flavor,
being able to forward messages bidirectionaly I would do it,
but the way the si:*-file-stream-mixin works preclude that.
|#

(defflavor ftp-file-stream-mixin
           (truename pathname
            (status :open)
            actual
            host-unit
            byte-size
            support-plistp
            (probe-stream nil)
            (property-list-lock nil))
           (si:file-stream-mixin si:file-data-stream-mixin si:property-list-mixin)
  :initable-instance-variables)

(defmethod (ftp-file-stream-mixin :after :close) (&optional abortp)
  (unless (eq status :closed)
    (unwind-protect
        (let ((tcp:*tcp-stream-whostate* "Close"))
          (send actual :close abortp))
      (send host-unit :remove-file-stream self)
      (send host-unit :close-dataconn)
      (send host-unit :free)
      (setq status :closed))))

(defmethod (ftp-file-stream-mixin :length) ()
  (get self :length-in-bytes))

(defmethod (ftp-file-stream-mixin :real-file-stream-p) ()
  t)

(defmethod (ftp-file-stream-mixin :truename) ()
  (send self :plist)
  truename)

(defmethod (ftp-file-stream-mixin :qfaslp) ()
  ;; a gratuitous and obsolete required method.
  ;;(cerror "continue" "obsolete method being called")
  (string-equal "QFASL" (send pathname :canonical-type)))

(defmethod (ftp-file-stream-mixin :plist) (&aux p)
  (or (and (variable-boundp si:property-list) si:property-list)
      ;;Property-list already initialized
      (if (and (null si:current-process) property-list-lock)
          ;;We are the scheduler updating the wholine and somebody else is getting the property list
          nil
        (with-lock (property-list-lock :whostate "Property List Lock")
          (cond ((and (variable-boundp si:property-list) si:property-list)
                 ;;Wholine got it for us while we waited for the lock
                 )
                ((not support-plistp)
                 ;;Can't get property list -- either raw-directory list or we've tried and failed.
                 nil)
                ((setq support-plistp nil)      ;For effect only
                 ;; If we dont set this to NIL then the interaction of the stack of TV:WHO-LINE file streams
                 ;; will cause failure.  Doing the OPEN is ok, and causes a new stream (:Raw-directory-list t)
                 ;; to be seen in the who-line, (which unfortunately *looks* like the present one, but is not)
                 ;; but when that stream is closed it removes itself, causes the previous stream, *us* to be seen,
                 ;; and to be sent yet another :LENGTH message, and we get a recursive call to this code.
                 nil)
                ((not (ftp-directory-line-parserp (send host-unit :host)))
                 ;;No parser for this host -- can't get property list
                 nil)
                ((setq p (or probe-stream (open truename :direction nil)))
                 ;;We have a probe stream -- copy its property list
                 (setq truename (send p :truename))
                 (setq si:property-list (plist p)))
                (t
                 ;;No probe stream??
                 nil))))))

(defmethod (ftp-file-stream-mixin :rename) (new-pathname &optional (error-p t))
  (setq truename (send (send host-unit :access) :rename (send self :truename) new-pathname error-p))
  (send tv::who-line-file-state-sheet :clobbered)
  t)

(defmethod (ftp-file-stream-mixin :delete) (&optional (error-p t))
  (send (send host-unit :access) :delete (send self :truename) error-p)
  t)

(defmethod (ftp-file-stream-mixin :change-properties) (error-p &rest properties)
  (send (send host-unit :access) :change-properties (send self :truename) error-p properties))

(defresource simple-string-buffer (size)
  :constructor (make-array size :element-type 'string-char)
  :matcher (<= size (string-length object)))

(defresource simple-art-16b-buffer (size)
  :constructor (make-array size :element-type '(unsigned-byte 16.))
  :matcher (<= size (array-length object)))

(defflavor ftp-input-raw-stream
           ()
           (si:buffered-input-character-stream si:input-file-stream-mixin ftp-file-stream-mixin))

(defmethod (ftp-input-raw-stream :next-input-buffer) (&optional no-hang-p)
  (let ((tcp:*tcp-stream-whostate* "Net File Input"))
    (send actual :next-input-buffer no-hang-p)))

(defmethod (ftp-input-raw-stream :discard-input-buffer) (array)
  (send actual :discard-input-buffer array))

(defflavor ftp-input-binary-stream
           ()
           (si:buffered-input-stream si:input-file-stream-mixin ftp-file-stream-mixin))

(defmethod (ftp-input-binary-stream :next-input-buffer) (&optional no-hang-p)
  ;; maybe change this in the future to call :next-input-buffer
  ;; copy the contents, then call :discard-input-buffer on actual.
  (let ((tcp:*tcp-stream-whostate* "Net File Input"))
    (ecase byte-size
      (8
       (send actual :next-input-buffer no-hang-p))
      (16
       (multiple-value-bind (array start end)
           (send actual :next-input-buffer no-hang-p)
         (when array
           (when (oddp (- end start))
             (error "16 bit stream, but odd byte count in input buffer"))
           (let ((length (ceiling (- end start) 2)))
             (values (make-array length
                                 :element-type '(unsigned-byte 16)
                                 :displaced-to array
                                 :displaced-index-offset start)
                     0
                     length))))))))

(defmethod (ftp-input-binary-stream :discard-input-buffer) (array)
  (ecase byte-size
    (8
     (send actual :discard-input-buffer array))
    (16
     (send actual :discard-input-buffer (net:original-array array)))))


(defflavor ftp-input-character-stream
           ()
           (si:buffered-input-character-stream
            si:input-file-stream-mixin
            ftp-file-stream-mixin))

(defmethod (ftp-input-character-stream :next-input-buffer) (&OPTIONAL NO-HANG-P)
  (multiple-value-bind (ARRAY START END)
      (let ((tcp:*tcp-stream-whostate* "Net File Input"))
        (send actual :next-input-buffer no-hang-p))
    (cond ((null array)
           nil)
          ('else
           ;; we know that the array-cache makes things faster
           ;; when we are concentrating on a single array at a time
           ;; so we preallocate a big enough buffer and do an in-place
           ;; translation.
           (let* ((size (- end start))
                  (string (allocate-resource 'simple-string-buffer size)))
             (copy-array-portion array start end string 0 size)
             ;; might as well free up the dma resource now in the
             ;; actual stream.
             (send actual :discard-input-buffer array)
             ;; the VAX instruction MOVTUC would be useful here.
             ;; (move translated until character)
             (do ((j 0 (1+ j))
                  (i 0)
                  (c))
                 ((= j size)
                  (values string 0 i))
               (cond ((= 13 (setq c (aref string j))))
                     ((= c 10)
                      (aset #\return string i)
                      (incf i))
                     ((or (= c 8) (= c 9) (= c 12) (= c 13))
                      (aset (+ c #o200) string i)
                      (incf i))
                     ('else
                      (aset c string i)
                      (incf i)))))))))

(defmethod (ftp-input-character-stream :discard-input-buffer) (array)
  (deallocate-resource 'simple-string-buffer array))

(defmethod (ftp-input-character-stream :byte-size) ()
  ;; this is needed when using the QFILE server-bridge-kludge
  8)

(defflavor ftp-output-raw-stream
           ()
           (si:buffered-output-character-stream si:output-file-stream-mixin ftp-file-stream-mixin))

(defmethod (ftp-output-raw-stream :new-output-buffer) ()
  (declare (values array start end))
  (let ((tcp:*tcp-stream-whostate* "Net File Output"))
    (send actual :new-output-buffer)))

(defmethod (ftp-output-raw-stream :send-output-buffer) (array end)
  (send actual :send-output-buffer array end))

(defmethod (ftp-output-raw-stream :discard-output-buffer) (array)
  (send actual :discard-output-buffer array))

(defflavor ftp-output-binary-stream
           ()
           (si:buffered-output-stream
            si:output-file-stream-mixin
            ftp-file-stream-mixin))

(defmethod (ftp-output-binary-stream :new-output-buffer) ()
  (declare (values array start end))
  (let ((tcp:*tcp-stream-whostate* "Net File Output"))
    (ecase byte-size
      (8
       (send actual :new-output-buffer))
      (16
       (let* ((array (send actual :new-output-buffer))
              (length (floor (array-length array) 2)))
         (values (make-array length
                             :element-type '(unsigned-byte 16)
                             :displaced-to array)
                 0
                 length))))))


(defmethod (ftp-output-binary-stream :send-output-buffer) (array end)
  (ecase byte-size
    (8
     (send actual :send-output-buffer array end))
    (16
     (send actual :send-output-buffer (net:original-array array) (* end 2)))))

(defmethod (ftp-output-binary-stream :discard-output-buffer) (array)
  (ecase byte-size
    (8
     (send actual :discard-output-buffer array))
    (16
     (send actual :discard-output-buffer (net:original-array array)))))

(defflavor ftp-output-character-stream
           ()
           (si:buffered-output-character-stream
            si:output-file-stream-mixin
            ftp-file-stream-mixin))

(defmethod (ftp-output-character-stream :new-output-buffer) ()
  (values (allocate-resource 'simple-string-buffer 2000)
          0
          2000))

(defmethod (ftp-output-character-stream :send-output-buffer) (buff limit)
  (let ((offset 0)
        (actual-stream actual)
        (tcp:*tcp-stream-whostate* "Net File Output"))
    (labels ((outbuff (end)
                      (do ((j offset (1+ j))
                           (s buff)
                           (c))
                          ((= j end)
                           (send actual-stream :string-out s offset end))
                        (if (> (setq c (aref s j)) #o200)
                            (aset (- c #o200) s j))))
             (outc (c) (send actual-stream :tyo c)))
      (do ((n))
          ((null (setq n (string-search-char #\return buff offset limit)))
           (outbuff limit))
        (outbuff n)
        (outc (tcp-application:sym ftp:CR))
        (outc (tcp-application:sym ftp:LF))
        (setq offset (1+ n)))))
  (deallocate-resource 'simple-string-buffer BUFF))


(defmethod (ftp-output-character-stream :discard-output-buffer) (array)
  (deallocate-resource 'simple-string-buffer array))

(defflavor ftp-probe-stream
           (truename pathname raw-data host)
           (SI:PROPERTY-LIST-MIXIN SI:FILE-STREAM-MIXIN)
  :initable-instance-variables)


(defmethod (ftp-probe-stream :direction) ()
  nil)

(defmethod (ftp-probe-stream :truename) ()
  ;; maybe parse the data to make sure truename is actual correct.
  truename)

(defmethod (ftp-probe-stream :set-truename) (x)
  ;; maybe should set other properties also back to some empty value.
  ;; we are doing this mainly for the benefit of ZMACS.
  (setq truename x))

(defmethod (ftp-probe-stream :after :init) (&rest ignored)
  (when (ftp-directory-line-parserp host t)
    (cond ((string-search #\return raw-data)
           (with-input-from-string (s raw-data)
             (do ((dplist)
                  (new))
                 ((not (setq new (readline s nil)))
                  (setq si:property-list nil truename nil))
               (when (car (setq dplist
                                (ftp-parse-directory-list-line new host pathname)))
                 (return (setq si:property-list (cdr dplist)
                               truename (getf (cdr dplist) :truename)))))))
          ('else
           (let ((dplist (ftp-parse-directory-list-line raw-data host pathname)))
             (setq si:property-list (cdr dplist))
             (setq truename (getf (cdr dplist) :truename)))))))

(DEFMETHOD (ftp-PROBE-STREAM :STATUS) ()
  :CLOSED)

(DEFMETHOD (ftp-PROBE-STREAM :BYTE-SIZE) ()
  (get self :byte-size))


(compile-flavor-methods ftp-access
                        ftp-host-unit
                        ftp-input-character-stream
                        ftp-output-character-stream
                        ftp-input-raw-stream
                        ftp-output-raw-stream
                        ftp-input-binary-stream
                        ftp-output-binary-stream
                        ftp-probe-stream)


#||

These are debugging hacks. The present mechanisms don't seem good
enough to tell what access method to use on a per host basis.
Chaos is getting used by default. But if the desirability of FTP is
made higher than chaos then connections to lambdas with internet address
also uses ftp. To win during debugging just use this macro around
your first call to open on the pathname in particular.

||#

(defmacro with-ftp-access-only (&rest body)
  `(let ((fs:*FILE-ACCESS-PATHS* (list (assq 'ftp-access *FILE-ACCESS-PATHS*))))
     ,@body))

(defun get-host-unit (x)
  (if (typep x 'basic-host-unit)
      x
    (let* ((pathname (parse-pathname x))
           (host (and pathname (send pathname :host)))
           (access (and host (send host :access))))
      (when access
        (values-list (send access :host-units))))))

(defun ftp-unit-quit (u)
  (setq u (get-host-unit u))
  (when u
    (send u :close-control-connection)
    (set-in-instance u 'fs:file-stream nil)))

(defun ftp-history (u)
  (setq u (get-host-unit u))
  (when u
    (send u :funcall-inside-yourself 'ftp:cmd-history)))


(defun ftp-listf (p)
  (with-open-file (s p :raw-directory-list t)
    (format t "~&For directory ~S~%" p)
    (stream-copy-until-eof s standard-output)))

;;; -*- Mode:LISP; Package:FTP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defmacro defopt (symbol command-fmt)
  `(setf (get ',symbol :defopt-fmt) ,command-fmt))

;;; modes
(defopt stream "MODE S")
(defopt block "MODE B")
(defopt compressed "MODE C")

;;; types
(defopt ascii "TYPE A")
(defopt ebcdic "TYPE E")
(defopt image "TYPE I")
(defopt binary "TYPE I")
(defopt tenex "TYPE L ~D")
(defopt 16bit "TYPE L 16")

;;; forms
(defopt non-print "FORM N")
(defopt telnet "FORM T")
(defopt carriage-control "FORM C")

;;; structs
(defopt file "STRU F")
(defopt record "STRU R")
(defopt page "STRU P")


(defvar *auto-login*)
(defvar *trace*)
(defvar *hash* nil)
(defvar *sendport*)
(defvar *verbose*)
(defvar *debug*)
(defvar *bell*)
(defvar *glob*)
(defvar *prompt*)
(defvar *type*)
(defvar *struct*)
(defvar *form*)
(defvar *mode*)
(defvar *bytesize*)
(defvar *remote-hostname*)
(defvar *connected*)
(defvar *user*)
(defvar *pass*)
(defvar *acct*)
(defvar *control*)
(defvar *data*)
(defvar *history*)
(defvar *last-reply*)

(eval-when (eval compile load)
;;; this is needed at compile time by the macro defcmd.

(defvar *ftp-command-alist* '(nil))
(defvar *ftp-connected-command-list* nil)
)

(defun find-command-entry (name)
  (let* (match-entry
         (match-count 0)
         (length (length name)))
    (dolist (entry *ftp-command-alist*)
      (when (null entry)
        (cond ((= match-count 0)
               (format t "~&?Invalid command~%")
               (return-from find-command-entry nil))
              ((= match-count 1)
               (return-from find-command-entry match-entry))
              (t
               (format t "~&?Ambiguous command~%")
               (return-from find-command-entry nil))))
      (when (string-equal name (car entry))
        (return-from find-command-entry entry))
      (when (string-equal name (car entry) :end1 length :end2 length)
        (incf match-count)
        (setq match-entry entry)))))

(defun is-required-arg (prompt)
  (/= #\[ (aref prompt 0)))

(defun execute-ftp-command-list (command-list &aux entry)
  (when command-list
    (setq entry (find-command-entry (car (last command-list)))))
  (when entry
    (let* ((func-name (car entry))
           (func-sym (cdr entry))
           (prompt-list (nthcdr (1- (length command-list))
                                (get func-sym :prompt-list)))
           input)
      (when (and (member func-sym *ftp-connected-command-list*)
                 (not *connected*))
        (format t "~&Connect to a remote host before doing ~S" func-name)
        (return-from execute-ftp-command-list nil))
      (when (and prompt-list (is-required-arg (car prompt-list)))
        (dolist (prompt prompt-list)
          (fresh-line)
          (setq input (global:prompt-and-read :string-or-nil prompt))
          (when (and (null input) (is-required-arg prompt))
            (when *bell*
              (global:beep))
            (format t "~&~A ~A~%" func-name (get func-sym :usage-string))
            (return-from execute-ftp-command-list nil))
          (push input command-list)))
      (setq command-list
            (do () ((car command-list) (reverse command-list))
              (pop command-list)))

      ;; typing  blows away our connection to the remote-host; this should be fixed.
      ;; this condition-case is here to prevent  from bombing us out of ftp.
      (global:condition-case ()
          (apply func-sym (cdr command-list))
        (sys:abort)))))

(defun parse-line-into-list (line)
  (when line
    (let ((end 0)
          command-list)
      (do (start)
          ((null (multiple-value-setq (start end) (fs:string-find-token line end)))
           command-list)
        (push (substring line start end) command-list)))))


(defun hookup (host port &optional (keyword "FTP Control Connection"))
  (setq *remote-hostname* (if (numberp host)
                              (format nil "~X" host)
                            (string host)))
  (let ((tcp:*tcp-stream-whostate* "Open Control Connection"))
    (setq *control* (open (string-append "TCP-HOST:" host "." port)
                          :keyword keyword))
    (setq *connected* t)
    (if *verbose* (format t "~&Connected to ~S~%" host))
    (getreply nil)))

(defun ftp-getreply (expecteof)
  (let ((tcp:*tcp-stream-whostate* "Reply"))
    (getreply expecteof)))

(defun getreply (expecteof)
  (push-history)
  (catch 'lostpeer
    (prog (code continuationp)
       error-loop
          (multiple-value-setq (code continuationp) (getreply-line expecteof))
          (cond ((null code)
                 ;; an error condition, first line must contain a code
                 ;; Sometimes ftp servers have bugs the cause system error
                 ;; messages and other garbage down the line.
                 ;; This code skips over such lines:
                 (go error-loop))
                (continuationp
                 (getreply-recursive code expecteof)))
          (return (values (floor code 100) code)))))

(defun getreply-recursive (code expecteof)
  (prog ()
     tail-recursive
        (multiple-value-bind (new-code continuationp)
            (getreply-line expecteof)
          (cond ((null new-code)
                 (go tail-recursive))
                ((eq new-code code)
                 (cond (continuationp
                        (go tail-recursive))
                       ('else
                        (return nil))))
                (continuationp
                 ;; this is a nested message and how i think it
                 ;; is proper to handle. never have seen one though.
                 (return (getreply-recursive new-code expecteof)))
                ('else
                 (go tail-recursive))))))


(defun getreply-line (expecteof)
  (labels ((peekc ()
                  (or (and *control* (send *control* :tyipeek))
                      (if expecteof nil (lostpeer))))
           (getc ()
                 (let ((c (and *control* (send *control* :tyi))))
                   (or expecteof c (lostpeer))
                   (or (not c) (= c (sym lf))
                       (history-record-char (if (= c (sym cr)) #\return c)))
                   (cond ((or (not c) (not *verbose*)))
                         ((= c (sym lf)))
                         ((= c (sym cr))
                          (terpri))
                         ('else
                          (write-char c)))
                   c)))
    (prog (code j c weight continuationp)
          (when *verbose* (format t "~&"))
          (setq code 0 j 0)
       get-code
          (cond ((not (= j 3)))
                ((eq #\- (int-char (peekc)))
                 (setq continuationp t)
                 (getc)
                 (go get-crlf))
                ('else
                 (go get-crlf)))
          (setq c (getc))
          (cond ((null c)
                 (return (values -1 nil)))
                ((null (setq weight (digit-char-p c)))
                 (setq code nil)
                 (go get-crlf))
                ('else
                 (setq code (+ (* code 10) weight))
                 (incf j)
                 (go get-code)))
       get-crlf
          (setq c (getc))
          (cond ((null c)
                 (return (values code continuationp)))
                ((= c (sym cr))
                 (setq c (getc))
                 (or (eq c (sym lf))
                     (not *debug*)
                     (error "expecting LF after CR but got: ~@C" c))
                 (return (values code continuationp)))
                ('else
                 (go get-crlf))))))

(defun close-control-connection ()
  (when *control*
    (let ((tcp:*tcp-stream-whostate* "Close Control Connection"))
      (unwind-protect
          (close *control*)
        (setq *control* nil)))))

(defun close-data-connection ()
  (when *data*
    (let ((tcp:*tcp-stream-whostate* "Close Data Connection"))
      (unwind-protect
          (close *data*)
        (setq *data* nil)))))

(defun lostpeer ()
  (when *connected*
    (setq *type* 'ascii)
    (setq *struct* 'file)
    (setq *form* 'non-print)
    (setq *mode* 'stream)
    (setq *bytesize* 8.)
    (close-control-connection)
    (close-data-connection)
    (setq *connected* nil)
    (let ((s (last-reply))
          (bogo "599 server randomly died, lost connection, did not print this"))
      (when s
        (setf (fill-pointer s) (length bogo))
        (copy-array-contents bogo s)))
    (throw 'lostpeer (values (sym error) 599))))


(defvar *ignore-reply-from-quit* nil)

(defun ftp-command (fmt &rest args)
  "Send an FTP command to the control connection.  For use within (ftp:ftp)"
  (let ((tcp:*tcp-stream-whostate* "Command"))
    (command-1 (apply #'format nil fmt args))))

(defun command (fmt &rest args)
  "Send an FTP command to the control connection."
  (command-1 (apply #'format nil fmt args)))

(defun command-1 (command-string)
  (catch 'lostpeer
    (unless *control*
      (and *debug* (format *error-output* "~&No control connection for command~%"))
      (return-from command-1 0))

    (do ()
        ((not (send *control* :listen)))
      ;;Server has a bug -- shouldn't be extra replies here!
      (push-history)
      (let ((string *last-reply*))
        (copy-array-contents "BUG: " string)
        (setf (fill-pointer string) 5)
        (do ((c nil))
            ((eq c #\return))
          (setq c (send *control* :tyi))
          (when (null c) (lostpeer))
          (unless (eq c (sym cr))
            (when (eq c (sym lf))
              (setq c #\return))
            (history-record-char c)))
        (when *verbose*
          (format t "~&~A" string))))

    (if *debug* (format t "~&---> ~A~%" command-string))
    (unless (eq *history* :dont-record)
      (push (string-append "" command-string) *history*))
    (send *control* :string-out command-string)
    (send *control* :tyo (sym cr))
    (send *control* :tyo (sym lf))
    (send *control* :force-output)
    (getreply (if (string-equal command-string "QUIT")
                  (if *ignore-reply-from-quit*
                      (return-from command-1 nil)
                    t)))
    ))

(defun commandp (value fmt &rest args)
  (= value (apply #'command fmt args)))

;; data connection handling: RECVREQUEST and SENDREQUEST

(defun recvrequest (cmd local remote)
  "local is a string describing the local place to put the
requested data. remote is a remote description."
  (prog (start stop bytes din fout)
        (setq bytes 0)
        (unwind-protect
            (progn (when (initconn :input)
                     (go done))
                   (let ((x (if remote
                                (ftp-command "~A ~A" cmd remote)
                              (ftp-command cmd))))
                     (unless (= x (sym prelim))
                       (go done)))
                   (setq din (dataconn :input))
                   (if (not din) (go bad))
                   (setq fout (cond ((null local) *standard-output*)
                                    ((streamp local) local)
                                    ((stringp local)
                                     (if (eq *type* '16bit)
                                         (make-8b-to-16b-translating-output-stream
                                           (open local :direction :output :byte-size 16 :characters nil))
                                       (open local :direction :output)))))
                   (setq start (time:time))
                   (let ((tcp:*tcp-stream-whostate* "Net File Input"))
                     (global:stream-copy-until-eof din fout))
                   (setq stop (time:time))
                   (setq bytes (send din :bytes)))
          (unless stop
            (setq stop (time:time))
            (if din (setq bytes (send din :bytes))))
          (close-data-connection)
          (when fout
            (unless (eq fout *standard-output*)
              (close fout))))
     bad
        (ftp-getreply nil)
     done
        (if (and (> bytes 0) *verbose*)
            (ptransfer "received" bytes start stop))
        (return nil)))

(defun sendrequest (cmd local remote)
  (prog (start stop bytes dout)
        (setq bytes 0)
        (unwind-protect
            (progn (if (initconn :output) (go bad))
                   (unless (= (ftp-command "~A ~A" cmd remote) (sym prelim))
                     (go done))
                   (setq dout (dataconn :output))
                   (if (null dout) (go bad))
                   (when (eq *type* '16bit)
                     (setq dout (make-16b-to-8b-translating-output-stream dout)))
                   (with-open-stream (fin (if (eq *type* '16bit)
                                              (open local :byte-size 16 :characters nil)
                                            (open local)))
                     (setq start (time:time))
                     (let ((tcp:*tcp-stream-whostate* "Net File Output"))
                       (global:stream-copy-until-eof fin dout))
                     (send dout :force-output)
                     (setq stop (time:time))
                     (setq bytes (send dout :bytes))))
          (unless stop
            (setq stop (time:time))
            (if dout (setq bytes (send dout :bytes))))
          (close-data-connection))
        (close dout)
     bad
        (ftp-getreply nil)
     done
        (if (and (> bytes 0) *verbose*)
            (ptransfer "sent" bytes start stop))
        (return nil)))

(defun ptransfer (direction bytes t0 t1)
  (if *verbose*
      (let ((sec (/ (- t1 t0) 60.0)))
        (if (zerop sec) (setq sec 1))
        (format t "~D bytes ~A in ~$ seconds (~$ Kbytes per second)~%"
                bytes direction
                sec
                (/ bytes (* sec 1000))))))

#|
Parts of the initconn and dataconn combination should be
abstracted into the :OPEN method for TCP-HOST. Use of WITH-OPEN-FILE
in RECVREQUEST and SENDREQUEST could be prefered.
|#

(defparameter *ftp-buffers* 16. "Number of buffers for a data connection")

(defun initconn (direction &optional buffers optimistic)
  (unless (numberp buffers)
    (setq buffers *ftp-buffers*))
  (close-data-connection)
  (let ((addr nil)
        (port nil)
        (result nil)
        (tcp:*tcp-stream-whostate* "Open Data Connection"))
    (setq *data* (tcpa:open-easy-tcp-stream (send *control* :remote-address)
                                            (sym-value 'tcpa:ipport-ftp-data)
                                            (unless *sendport*
                                              (send *control* :local-port))
                                            :direction direction
                                            :input-buffers (ecase direction
                                                             (:input buffers)
                                                             (:output 0))
                                            :output-buffers (ecase direction
                                                              (:output buffers)
                                                              (:input 0))
                                            :optimistic optimistic
                                            :keyword "FTP Data Connection"
                                            :connect nil))
    (when *sendport*
      (setq addr (send *data* :local-address))
      (setq port (send *data* :local-port))
      (setq result (command "PORT ~D,~D,~D,~D,~D,~D"
                            (ldb (byte 8 24) addr)
                            (ldb (byte 8 16) addr)
                            (ldb (byte 8 8) addr)
                            (ldb (byte 8 0) addr)
                            (ldb (byte 8 8) port)
                            (ldb (byte 8 0) port)))
        (if (= result (sym error))
            (let ((*sendport* nil))
              (initconn direction))
          (not (= result (sym complete)))))))

(defun dataconn (direction &optional (translator-maker 'make-translating-stream))
  ;; arrange to return NIL here if the accept times out or other network error.
  (send *data* :accept)
  (funcall translator-maker *data* direction (cond ((eq *type* 'ascii) :ascii)
                                                 ((eq *type* 'ebcdic) :ebcdic))))

(defun close-dataconn (&optional (getreply t))
  (close-data-connection)
  (and getreply *connected* (ftp-getreply nil)))

#|
Character translation hair. The operations provided by the
stream-default-handler are:
 :tyipeek :listen
 :any-tyi :tyi-no-hang
 :any-tyi-no-hang
 :read-char
 :any-read-char :read-char-no-hang
 :any-read-char-no-hang
 :read-byte
 :unread-char
 :write-char
 :write-byte
 :clear-output :clear-input :force-output :finish :close :eof
 :fresh-line
 :string-out :line-out
 :line-in
 :string-in
 :string-line-in
 :operation-handled-p
 :characters
 :element-type
 :direction
 :send-if-handles

Although we are only concerned with what stream-copy-until-eof will
send, which is :read-input-buffer on :input, and :string-out on :output.

|#

(defun make-translating-stream (raw-stream direction typearg)
  (ecase direction
    (:input
     (ecase typearg
       (:ascii
        (make-ascii-translating-input-stream raw-stream))
       (nil
        (make-non-translating-input-stream raw-stream))))
    (:output
     (ecase typearg
       (:ascii
        (make-ascii-translating-output-stream raw-stream))
       (nil
        (make-non-translating-output-stream raw-stream))))))

(defun make-non-translating-input-stream (raw-stream &optional (hash-marks t) &aux (bytes 0) stream)
  (setq stream
        #'(lambda (op &optional arg1 &rest args)
            (si:selectq-with-which-operations op
              (:read-input-buffer
                (multiple-value-bind (buf offset limit)
                    (send raw-stream :read-input-buffer)
                  (cond ((null buf)
                         ())
                        ('else
                         (if (and hash-marks *hash*) (princ "#"))
                         (incf bytes (- limit offset))
                         (values buf offset limit)))))
              (:advance-input-buffer
                (send raw-stream :advance-input-buffer))
              (:bytes
                (if (and hash-marks *hash*) (terpri))
                bytes)
              (:close
                (close raw-stream))
              (t
                (global:stream-default-handler stream op arg1 args))))))

(defun make-ascii-translating-input-stream (raw-stream &optional (hash-marks t)  &aux (bytes 0) stream)
  (setq stream
        #'(lambda (op &optional arg1 &rest args)
            (si:selectq-with-which-operations op
              (:read-input-buffer
                (multiple-value-bind (buf offset limit)
                    (send raw-stream :read-input-buffer)
                  (cond ((null buf)
                         nil)
                        ('else
                         (do ((j offset (1+ j))
                              (i offset)
                              (c))
                             ((= j limit)
                              (if (and hash-marks *hash*) (princ "#"))
                              (incf bytes (- i offset))
                              (values buf offset i))
                           (cond ((= 13 (setq c (aref buf j)))
                                  ;; theory is to ignore CR in the CRLF sequence
                                  ;; because somebody might send CR LF LF to mean
                                  ;; two lines, and it is the LF that carry the meaning.
                                  ;; An actual reading of the protocal handbook
                                  ;; might not be a bad idea.
                                  )
                                 ((= c 10)
                                  (setf (aref buf i) #\return)
                                  (incf i))
                                 ((or (= c 8) (= c 9) (= c 12) (= c 13))
                                  (setf (aref buf i) (+ c #o200))
                                  (incf i))
                                 ('else
                                  (setf (aref buf i) c)
                                  (incf i))))))))
              (:advance-input-buffer
                (send raw-stream :advance-input-buffer))
              (:bytes
                (if (and hash-marks *hash*) (terpri))
                bytes)
              (:close
                (close raw-stream))
              (t
                (global:stream-default-handler stream op arg1 args))))))

(defun make-non-translating-output-stream (raw-stream &optional (hash-marks t) &aux (bytes 0) stream)
  (setq stream
        #'(lambda (op &optional arg1 &rest args)
            (si:selectq-with-which-operations op
              (:string-out
                (let ((buff arg1)
                      (offset (car args))
                      (limit (cadr args)))
                  (when (and hash-marks *hash*)
                    (dotimes (i (truncate (- limit offset) 1024))
                      (princ "#")))
                  (incf bytes (- limit offset))
                  (send raw-stream :string-out buff offset limit)))
              (:close
                (close raw-stream))
              (:bytes
                (if (and hash-marks *hash*) (terpri))
                bytes)
              (t
                (global:stream-default-handler stream op arg1 args))))))

(defun make-ascii-translating-output-stream (raw-stream &optional (hash-marks t) &aux (bytes 0) stream)
  (setq stream
        #'(lambda (op &optional arg1 &rest args)
            (si:selectq-with-which-operations op
              (:tyo
                (incf bytes)
                (let ((c arg1))
                  (cond ((= c #\return)
                         (send raw-stream :tyo (sym cr))
                         (send raw-stream :tyo (sym lf)))
                        ('else
                         (if (> c #o200)
                             (setq c (- c #o200)))
                         (send raw-stream :tyo c)))))
              (:string-out
                (let ((buff arg1)
                      (offset (car args))
                      (limit (cadr args)))
                  (or offset (setq offset 0))
                  (or limit (setq limit (length buff)))
                  (if (and hash-marks *hash*) (princ "#"))
                  (incf bytes (- limit offset))
                  (labels ((outbuff (end)
                                    (do ((j offset (1+ j))
                                         (s buff)
                                         (c))
                                        ((= j end)
                                         (send raw-stream :string-out s offset end))
                                      (if (> (setq c (aref s j)) #o200)
                                          (setf (aref s j) (- c #o200)))))
                           (outc (c) (send raw-stream :tyo c)))
                    (do ((n))
                        ((null (setq n (global:string-search-char #\return buff offset limit)))
                         (outbuff limit))
                      (outbuff n)
                      (outc (sym cr))
                      (outc (sym lf))
                      (setq offset (1+ n))))))
              (:close
                (close raw-stream))
              (:bytes
                (if (and hash-marks *hash*) (terpri))
                bytes)
              (t
                (global:stream-default-handler stream op arg1 args))))))

;;; FTP command table and commands

(eval-when (eval compile load)
;;; the following two functions are needed at compile time by the macro defcmd.

(defun usage-string (arglist)
  (let (optional (usage ""))
    (dolist (arg arglist usage)
      (cond ((eq arg '&optional)
             (setq optional t))
            ((eq arg '&rest))
            (t
             (when (listp arg)
               (setq arg (car arg)))
             (setq usage (string-append usage
                                        (format nil " ~:[~;[ ~]~(~A~)~:[~; ]~]" optional arg optional))))))))

(defun prompt-list (arglist)
  (let (optional prompt)
    (cond ((null arglist)
           nil)
          ((eq (car arglist) '&optional)
           nil)
          ((eq (car arglist) '&rest)
           `(,(format nil "(~(~A~)) " (cadr arglist))))
          (t
           (dolist (arg arglist (reverse prompt))
             (cond ((eq arg '&optional)
                    (setq optional t))
                   (t
                    (when (listp arg)
                      (setq arg (car arg)))
                    (push (format nil "~:[(~;[~]~(~A~)~:[)~;]~] " optional arg optional) prompt))))))))
)

(defmacro defcmd (cmd-names arglist connection-needed-p &body body)
  (declare (zwei:indentation 2 1))
  (when (atom cmd-names)
    (setq cmd-names (list cmd-names)))
  (let ((cmd-sym (car cmd-names))
        (usage (usage-string arglist))
        (prompt (prompt-list arglist))
        (forms nil))

    ;; hack arglist so the cmd-func will tolerate
    ;; any number of arguments passed to it.
    (unless (member '&rest arglist :test #'eq)
      (setq arglist `(,@arglist &rest ignore)))

    (dolist (cmd-name cmd-names)
      (let ((name (substring (format nil "~(~A~)" cmd-name) 4)))
        (push `(progn
                 (global:record-source-file-name ',cmd-name 'defcmd)
                 (unless (assoc ,name *ftp-command-alist* :test #'string-equal)
                   (push `(,',name . ,',cmd-sym) *ftp-command-alist*))
                 (when ,connection-needed-p
                   (unless (member ',cmd-sym *ftp-connected-command-list* :test #'eq)
                     (push ',cmd-sym *ftp-connected-command-list*)))
                 )
              forms)))

    `(progn
       ;; add all aliases into the alist of command names.
       ;; notice that names in the table have the "cmd-" prefix stripped.
       (eval-when (eval compile load)
         ,@forms)
       (defun ,cmd-sym ,arglist
         ,@body)
       (setf (get ',cmd-sym :usage-string) ',usage)
       (setf (get ',cmd-sym :prompt-list) ',prompt))))

#|
 (defcmd (testit tstit) (&rest fwazz1)
  "document string"
  (format t "~&fwazz1=~A; fwazz2=~A; fwazz3=~A" fwazz1 fwazz2 fwazz3))
|#


(defun multiple-command (prompt func-spec arg-list)
  (when (= 1 (length arg-list))
    (setq arg-list (parse-line-into-list (car arg-list))))
  (dolist (arg arg-list)
    (when (or (not prompt) (y-or-n-p (format nil "~A ~A? " prompt arg)))
      (funcall func-spec arg))))

(defun wildcard-multiple-command (prompt func-spec arg-list)
  (when (= 1 (length arg-list))
    (setq arg-list (parse-line-into-list (car arg-list))))
  (dolist (arg arg-list)
    (dolist (file (expand-remote-wildcard arg))
      (when (or (not prompt) (y-or-n-p (format nil "~A ~A? " prompt file)))
        (funcall func-spec file)))))

(defun expand-remote-wildcard (name)
  (with-input-from-string (string (with-output-to-string (s)
                                    (recvrequest "NLST" s name)))
    (do* ((result nil)
          (line (read-line string) (read-line string)))
         ((null line)
          (nreverse result))
      (push line result))))

;;; file transfer commands

(defcmd cmd-append (local-file &optional (remote-file local-file)) t
  "append to a file"
  (sendrequest "APPE" local-file remote-file))

(defcmd (cmd-get cmd-recv) (remote-file &optional (local-file remote-file)) t
  "receive one file"
  (recvrequest "RETR" local-file remote-file))

(defcmd (cmd-put cmd-send) (local-file &optional (remote-file local-file)) t
  "send one file"
  (sendrequest "STOR" local-file remote-file))

(defcmd (cmd-mput cmd-msend) (&rest local-files) t
  "send multiple files"
  (multiple-command "put" #'cmd-put local-files))

(defcmd (cmd-mget cmd-mrecv) (&rest remote-files) t
  "receive multiple files"
  (wildcard-multiple-command "get" #'cmd-get remote-files))

;;; connection hacking commands

(defcmd (cmd-quit cmd-bye) () nil
  "terminate ftp session and exit"
  (throw 'quit nil))

(defcmd cmd-open (to) nil
  "connect to remote ftp"
  (when (and *connected* *control* (send *control* :remote-address))
    (cmd-close))
  (hookup to "FTP")
  (when *auto-login*
    (cmd-user)))

(defun prompt-and-read-no-echo (prompt)
  (let ((line (make-string 30 :fill-pointer 0)))
    (loop
      (format *query-io* prompt)
      (loop
        (let ((char (send *query-io* :tyi)))
          (cond ((= char #\c-q)                 ;quoting character.
                 (vector-push-extend (send *query-io* :tyi) line))
                ((= char #\rubout)
                 (when (zerop (fill-pointer line))
                   (return))
                 (vector-pop line))
                ((= char #\clear-input)
                 (return))
                ((= char #\return)
                 (fresh-line *query-io*)
                 (send *query-io* :send-if-handles :make-complete)
                 (return-from prompt-and-read-no-echo
                   (when (> (fill-pointer line) 0)
                     line)))
                ((/= 0 (char-bits char))
                 (global:beep))
                (t
                 (vector-push-extend char line)))))
      (format *query-io* "flushed.~&")
      (setf (fill-pointer line) 0))))

(defun send-user-info (prompt command arg &optional (echo? t))
  (declare (values string logged-in))
  (unless arg
    (setq arg (if echo?
                  (global:prompt-and-read :string-or-nil prompt)
                (prompt-and-read-no-echo prompt))))
  (when arg
    (let ((n (command "~A ~A" command arg)))
      (cond ((= n (sym continue))
             (values arg nil))
            ((= n (sym complete))
             (values arg t))
            (t
             (values nil nil))))))

(defun try-login (u p a)
  (let ((logged-in nil)
        (tcp:*tcp-stream-whostate* "Login"))
    (multiple-value-setq (u logged-in)
      (send-user-info "(username) " "USER" u))
    (cond ((null u)
           (return-from try-login))
          (logged-in
           (return-from try-login (values u p a))))

    (multiple-value-setq (p logged-in)
      (send-user-info "(password) " "PASS" p nil))
    (cond ((null p)
           (return-from try-login))
          (logged-in
           (return-from try-login (values u p a))))

    (multiple-value-setq (a logged-in)
      (send-user-info "(account) " "ACCT" a))
    (cond ((null a)
           (return-from try-login))
          (logged-in
           (return-from try-login (values u p a))))))

(defcmd cmd-user (&optional username password account) t
  "send new user information (login)"
  (cond ((multiple-value-setq (*user* *pass* *acct*)
            (if username
                (try-login username password account)
              (try-login *user* *pass* *acct*))))
        (*verbose*
         (format t "~&Login failed~%"))))

(defcmd cmd-close () t
  "terminate ftp session"
  (when *connected*
    (setq *type* 'ascii)
    (setq *struct* 'file)
    (setq *form* 'non-print)
    (setq *mode* 'stream)
    (setq *bytesize* 8.)
    (close-data-connection)
    (command "QUIT")
    (close-control-connection)
    (setq *connected* nil)))

(defcmd cmd-quote (&rest command-line-to-send) t
  "send arbitrary ftp command"
  (= (sym complete) (command-1 (format nil "~{~A ~}" command-line-to-send))))

;;; help and status commands

(defcmd (cmd-help cmd-?) (&optional command) nil
  "print local help information"
  (let (last-entry
        alias-list)
    (fresh-line)
    (cond (command
           (when (setq command (find-command-entry command))
             (format t "~A:~A~%  ~A~%"
                     (car command)
                     (get (cdr command) :usage-string)
                     (or (documentation (cdr command))
                         ""))))
          (t
           (dolist (entry *ftp-command-alist*)
             (when (and last-entry
                        (not (eq (cdr entry) (cdr last-entry))))
               (format t "*~{ ~A~^,~}:~A~%    ~A~%"
                       alias-list
                       (get (cdr last-entry) :usage-string)
                       (or (documentation (cdr last-entry)) ""))
               (setq alias-list nil))
             (push (car entry) alias-list)
             (setq last-entry entry))))))

(defun onoff (bool)
  (if bool "ON" "OFF"))

(defcmd cmd-status () nil
  "show current status"
  (if *connected*
      (format t "~&Connected to ~A~%" *remote-hostname*)
    (format t "~&Not connected~%"))
  (format t "Mode: ~A; Type: ~A; Form: ~A; Structure: ~A~%"
          *mode* *type* *form* *struct*)
  (format t "Verbose: ~A; Bell: ~A; Prompting: ~A; Globbing: ~A~
           ~&Hash mark printing: ~A; Use of PORT cmds: ~A~%"
          (onoff *verbose*)
          (onoff *bell*)
          (onoff *prompt*)
          (onoff *glob*)
          (onoff *hash*)
          (onoff *sendport*)))

(defcmd cmd-remotehelp (&optional subject) t
  "get help from remote server"
  (let ((*verbose* t))
    (if subject
        (commandp (sym complete) "HELP ~A" subject)
      (commandp (sym complete) "HELP"))))

;;; type setting commands

(defmacro setopt (opt-sym opt &rest args)
  `(cond ((eq ,opt-sym ,opt) ,opt)
         ((commandp (sym complete) (get ,opt :defopt-fmt) ,@args)
          (setq ,opt-sym ,opt))
         (t nil)))

(defcmd cmd-type (&optional type-name) nil
  "show/set file transfer type"
  (if type-name
      (execute-ftp-command-list `(,type-name))
    (format t "~&Using ~A type to transfer files.~%" *type*)))

(defcmd cmd-ascii () t
  "set ascii transfer type"
  (setopt *type* 'ascii))

(defcmd cmd-binary () t
  "set binary transfer type"
  (setopt *type* 'binary))

(defcmd cmd-image () t
  "set image transfer type"
  (setopt *type* 'image))

(defcmd cmd-ebcdic () t
  "set ebcdic transfer type"
  (setopt *type* 'ebcdic))

(defcmd cmd-tenex () t
  "set tenex transfer type"
  (setopt *type* 'tenex *bytesize*))

(defcmd cmd-16bit () t
  "set 16bit transfer type"
  (unless (setopt *type* '16bit)
    (setopt *type* 'binary)))

;;; format setting commands

(defcmd cmd-form (&optional format-name) nil
  "show/set file transfer format"
  (if format-name
      (execute-ftp-command-list `(,format-name))
    (format t "~&Using ~A format to transfer files.~%" *form*)))

(defcmd cmd-non-print () t
  "set non-print transfer format"
  (setopt *form* 'non-print))

(defcmd cmd-telnet () t
  "set telnet transfer format"
  (setopt *form* 'telnet))

(defcmd cmd-carriage-control () t
  "set carriage-control transfer format"
  (setopt *form* 'carriage-control))

;;; struct setting commands

(defcmd cmd-struct (&optional structure-name) nil
  "show/set file transfer structure"
  (if structure-name
      (execute-ftp-command-list `(,structure-name))
    (format t "~&Using ~A structure to transfer files.~%" *struct*)))

(defcmd cmd-file () t
  "set file transfer structure"
  (setopt *struct* 'file))

(defcmd cmd-record () t
  "set record transfer structure"
  (setopt *struct* 'record))

(defcmd cmd-page () t
  "set page transfer structure"
  (setopt *struct* 'page))

;;; mode setting commands

(defcmd cmd-mode (&optional mode-name) nil
  "show/set file transfer mode"
  (if mode-name
      (execute-ftp-command-list `(,mode-name))
    (format t "~&Using ~A mode to transfer files.~%" *mode*)))

(defcmd cmd-stream () t
  "set stream transfer mode"
  (setopt *mode* 'stream))

(defcmd cmd-block () t
  "set block transfer mode"
  (setopt *mode* 'block))

(defcmd cmd-compressed () t
  "set compressed transfer mode"
  (setopt *mode* 'compressed))

;;; toggle setting commands

(defcmd cmd-bell () nil
  "beep when command completed"
  (setq *bell* (not *bell*))
  (format t "~&Bell mode ~A.~%" (onoff *bell*)))

(defcmd cmd-debug () nil
  "toggle debugging mode"
  (setq *debug* (not *debug*))
  (format t "~&Debugging ~A.~%" (onoff *debug*)))

(defcmd cmd-glob () nil
  "toggle metacharacter expansion of local file names"
  (setq *glob* (not *glob*))
  (format t "~&Globbing ~A.~%" (onoff *glob*)))

(defcmd cmd-trace () nil
  "toggle packet tracing"
  (setq *trace* (not *trace*))
  (format t "~&Packet tracing ~A.~%" (onoff *trace*)))

(defcmd cmd-hash () nil
  "toggle printing `#' for each buffer transferred"
  (setq *hash* (not *hash*))
  (format t "~&Hash mark printing ~A.~%" (onoff *hash*)))

(defcmd cmd-verbose () nil
  "toggle verbose mode"
  (setq *verbose* (not *verbose*))
  (format t "~&Verbose mode ~A.~%" (onoff *verbose*)))

(defcmd cmd-prompt () nil
  "toggle interactive confirmation on `multiple' commands"
  (setq *prompt* (not *prompt*))
  (format t "~&Interactive mode ~A.~%" (onoff *prompt*)))

(defcmd cmd-sendport () nil
  "toggle use of PORT cmd use before each data connection"
  (setq *sendport* (not *sendport*))
  (format t "~&Use of PORT cmds ~A.~%" (onoff *sendport*)))

;;; directory hacking commands

(defcmd cmd-cd (&optional remote-directory) t
  "change remote working directory"
  (commandp (sym complete) "CWD~@[ ~A~]" remote-directory))

(defcmd cmd-lcd (&optional local-directory) nil
  "change local working directory"
  (let* ((defaults (fs:make-pathname-defaults))
         (colon (string-search-char #\: local-directory))
         (host (if colon (si:parse-host (substring local-directory 0 colon) t) si:local-host))
         (pathname (fs:parse-pathname local-directory host defaults (if colon (1+ colon) 0))))
    (fs:set-default-pathname pathname)
    (format t "~&Local pathname default now ~A.~%" pathname)
    pathname))

(defcmd (cmd-ls cmd-dir) (&optional remote-directory local-file) t
  "list contents of remote directory"
  (cmd-ascii)
  (recvrequest "LIST" local-file remote-directory))

(defcmd (cmd-nlst) (&optional remote-directory local-file) t
  "list contents of remote directory"
  (cmd-ascii)
  (recvrequest "NLST" local-file remote-directory))

(defcmd (cmd-mls cmd-mdir) (&rest remote-directories) t
  "list contents of multiple remote directories"
  (multiple-command "ls" #'cmd-ls remote-directories))

(defcmd cmd-pwd () t
  "print working directory on remote machine"
  (or (commandp (sym complete) "PWD")
      (commandp (sym complete) "XPWD")))

(defcmd cmd-mkdir (remote-directory) t
   "make a directory on remote machine"
   (or (commandp (sym complete) "MKD ~A" remote-directory)
       (commandp (sym complete) "XMKD ~A" remote-directory)))

(defcmd cmd-rmdir (remote-directory) t
  "remove a directory on remote machine"
  (or (commandp (sym complete) "RMD ~A" remote-directory)
      (commandp (sym complete) "XRMD ~A" remote-directory)))

(defcmd cmd-expunge (remote-directory) t
  "expunge the contents of a directory"
  (when (commandp (sym complete) "XPNG ~A" remote-directory)
    (let* ((reply (last-reply))
           (start (and reply (string-search-char #\space reply)))
           (blocks-freed (and start (parse-integer reply :start start :junk-allowed t))))
      (or blocks-freed 0))))

;;; file hacking commands

(defcmd cmd-delete (remote-file) t
  "delete one remote file"
  (commandp (sym complete) "DELE ~A" remote-file))

(defcmd cmd-undelete (remote-file) t
  "undelete one remote file"
  (commandp (sym complete) "XUND ~A" remote-file))

(defcmd cmd-mdelete (&rest remote-files) t
  "delete multiple remote files"
  (wildcard-multiple-command "delete" #'cmd-delete remote-files))

(defcmd cmd-rename (from-name to-name) t
  "rename a remote file"
  (when (member (command "RNFR ~A" from-name)
                ;; vms file server has bug, sends complete.
                (list (sym continue) (sym complete))
                :test #'eq)
    (commandp (sym complete) "RNTO ~A" to-name)))

(defun push-history ()
  (cond ((eq *history* :dont-record)
         (if *last-reply*
             (setf (fill-pointer *last-reply*) 0)
           (setq *last-reply* (make-history-element))))
        (t
         (push (setq *last-reply* (make-history-element)) *history*))))

(defun make-history-element ()
  (make-array 80. :element-type 'string-char :fill-pointer 0 :adjustable t))

(defun history-record-char (c)
  (vector-push-extend c *last-reply*))

(defun last-reply ()
  *last-reply*)

(defun history-record-stream (op &optional arg1 &rest args)
  (si:selectq-with-which-operations op
    (:tyo (history-record-char arg1))
    (t
      (global:stream-default-handler #'history-record-stream op arg1 args))))

(defcmd cmd-history () nil
  "print command/reply history"
  (when (not (eq *history* :dont-record))
    (dolist (x (reverse *history*))
      (fresh-line)
      (princ x))))


(export 'ftp)
(defun ftp (&optional remote-hostname
            &key
            (auto-login t)
            (trace t)
            (hash nil)
            (sendport t)
            (verbose t)
            (debug nil)
            (bell nil)
            (glob t)
            (prompt t)
            user pass acct)

  (let ((*print-radix* nil)                     ;prevent garbage in numbers printed by princ and prin1
        (*history* nil)
        (*last-reply* nil)
        (*connected* nil)
        (*control* nil)
        (*data* nil)
        (*remote-hostname* remote-hostname)
        (*auto-login* auto-login)
        (*trace* trace)
        (*hash* hash)
        (*sendport* sendport)
        (*verbose* verbose)
        (*debug* debug)
        (*bell* bell)
        (*glob* glob)
        (*prompt* prompt)
        (*type* 'ascii)
        (*struct* 'file)
        (*form* 'non-print)
        (*mode* 'stream)
        (*bytesize* 8.)
        (*user* user)
        (*pass* pass)
        (*acct* acct))

    (when *remote-hostname*
      (cmd-open *remote-hostname*))
    (catch 'quit
      (unwind-protect
          (global:error-restart ((error) "Return to FTP Command Loop.")
            (loop
              (execute-ftp-command-list
                (parse-line-into-list
                  (global:prompt-and-read :string-or-nil "~&ftp> ")))))
        (cmd-close)))))

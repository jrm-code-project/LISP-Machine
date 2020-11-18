;;; -*- Mode:LISP; Package:FTP; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

TO DO: Implement SITE, STAT, REST, and ACCT commands,

|#

(defvar *ftp-server-timeout* (* 60. 60.)
  "The time interval in seconds after which an idle FTP connection will time out.
If NIL, then the connection never times out.")

(defvar *ftp-server-administrator* "GJC@LMI-VAX")

(defun ftp-server-version ()
  (multiple-value-bind (major minor)
      (si:get-system-version)
    (format nil "LMI System ~D.~D"
            major
            minor)))

(defparameter ftp-catch-errors t "set to NIL for debugging")

(define-network-service *tcp-ftp-service* :ftp :tcp "File Transfer Protocol (RFC 765)"
  :listen-port (sym tcp-application:ipport-ftp)
  :toplevel-function 'ftp-server-function
  :auto-enable? t)

(defstruct (ftpstate
             (:print-function (lambda (ftpstate stream ignore)
                                (sys:printing-random-object
                                  (ftpstate stream :type :no-pointer)
                                  (format stream "~S ~S"
                                          (ftpstate-user-unames ftpstate)
                                          (ftpstate-current-cmd ftpstate))))))
  (ctrl-connected-p nil)
  (logged-in-p nil)
  (pn-defaults nil)
  (homedir-pn nil)
  (user-unames nil)
  (user-host-password-alist nil)
  (current-cmd nil)
  (next-cmd nil)
  (last-data nil)
  (estimated-length nil)                        ; not actually used at present

  (transfer-type :ascii)
  (byte-size 8.)
  (transfer-form :non-print)
  (transfer-structure :file)
  (transfer-mode :stream)

  (ctrl-stream nil)

  (data-stream nil)
  (data-my-port nil)
  (data-my-address nil)
  (data-his-port nil)
  (data-his-address :default)
  (data-connection-method :active)              ; either :ACTIVE or :PASSIVE
  (data-transfer-in-progress nil)
  (data-transfer-process nil)
  (cmd-reply-history nil)
  )

(defun reinitialize-ftpstate (state)
  (ftp-cleanup-data-connection state t)
  (setf (ftpstate-logged-in-p state) nil)
  (setf (ftpstate-user-unames state) nil)
  (setf (ftpstate-user-host-password-alist state) nil)
  (setf (ftpstate-estimated-length state) nil)
  (setf (ftpstate-next-cmd state) nil)
  (setf (ftpstate-transfer-type state) :ascii)
  (setf (ftpstate-byte-size state) 8)
  (setf (ftpstate-transfer-form state) :non-print)
  (setf (ftpstate-transfer-structure state) :file)
  (setf (ftpstate-transfer-mode state) :stream))


(defun ftp-server-function (stream &aux state)
  (unwind-protect
      (progn (setq state (make-ftpstate))
             (setf (ftpstate-ctrl-stream state) stream)
             (ftp-reply state 220 "~A FTP server (~A) ready."
                        si:local-host-name
                        (ftp-server-version))
             (loop (unless (ftp-cmdparse state) (return nil))))
    (and state (reinitialize-ftpstate state))))

(defun ftp-valid-lm-password-p (username password host)
  "Redefine this function if security is required at your site.
   The username and password are accepted only if this function returns T."
  (validate-network-server-password username password host))

; login ftp user.  Returns NIL if failed, otherwise T.   Also sends appropriate
; FTP reply over control connection.

(defun ftp-login (state user host-object password homedir)
  (cond ((not (ftp-valid-lm-password-p user password host-object))
         (ftp-reply state 530 "Login incorrect.")
         nil)
        (t
         (ftp-reply state 230 "User ~A~A logged in (default pathname = \"~A\")."
                    user
                    (cond ((eq host-object si:local-host) "")
                          (t (format nil "@~A" (send host-object :name)) ""))
                    homedir)
         (fs:set-default-pathname (setf (ftpstate-homedir-pn state) homedir)
                                  (setf (ftpstate-pn-defaults state) (fs:make-pathname-defaults)))
         (setf (ftpstate-logged-in-p state) t)
         t)))


(defun ftp-get-homedir (state username host)
  (ftp-file-operation state #'ftp-user-homedir host username))

(defun ftp-user-homedir (host username)
  (let ((x (send (fs:sample-pathname host) :homedir username)))
    (global:condition-case ()
        (probe-file (send x :new-pathname :name "LISPM" :type "INI"))
      (fs:directory-not-found nil))
    x))

(defun ftp-retrieve (state name directory-p)
  "Get a file from the local TCP host (modulo chaosnet file servers)
   and send it to the ftp user.  This is also used for getting directory listings."
  (if (not name)
      (if directory-p
          (setq name (fs:default-pathname (ftpstate-pn-defaults state)))
        (error "FTP-RETRIEVE called with NULL filename.~%")))
  (unless (and (not (pathnamep name))
               (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name directory-p))))
    (ftp-file-error-reply state (ftp-file-operation state #'ftp-retrieve-1
                                                            state name directory-p))))


(defun ftp-retrieve-1 (state pn directory-p &aux os is)
  (setq is (cond (directory-p
                  (or (send pn :name) (setq pn (send pn :new-name :wild)))
                  (setq pn (fs:merge-pathname-components
                             pn
                             (ftpstate-pn-defaults state)
                             :default-type :wild
                             :default-version :wild))
                  (cond ((member directory-p '(:name-list :directory-list) :test #'eq)
                         (fs:directory-list-stream pn))
                        (t (zwei:directory-input-stream pn :deleted))))
                 (t (open pn
                          :characters (eq (ftpstate-transfer-type state) :ascii)
                          :byte-size (ftpstate-byte-size state)))))
  (setq os (get-ftp-data-connection state
                                    (send is :send-if-handles :truename)
                                    (send is :send-if-handles :length)
                                    :output))
  (when (streamp os)
    (ftp-send-data state is os directory-p)))


(defun ftp-store (state name mode)
  (unless (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name)))
    (ftp-file-error-reply state (ftp-file-operation state #'ftp-store-1 state name mode))))


(defun ftp-store-1 (state pn mode &aux os is)
  "Write a file to the local TCP host (modulo chaosnet file servers)
    Mode can be any keyword acceptable for the :IF-EXISTS option to OPEN."
  (setq os (open pn
                 ;; ||| Added the (or ........ :default) wrapper
                 ;; ||| to cause either ascii mode or the default mode to happen JIM 10/27/88
                 :characters (or (eq (ftpstate-transfer-type state) :ascii)
                                 :default)
                 :byte-size (ftpstate-byte-size state)
                 :if-exists mode
;;; not used, so commented out for release 3.0, should implement this
;;; in our local file system then use it.
;;;     :ESTIMATED-LENGTH (PROG1 (FTPSTATE-ESTIMATED-LENGTH STATE)
;;;                           (SETF (FTPSTATE-ESTIMATED-LENGTH STATE) NIL))

                 :direction :output))
  (setq is (get-ftp-data-connection state
                                    (send os :send-if-handles :truename)
                                    nil
                                    :input))
  (when (streamp is)
    (ftp-receive-data state is os)))

(defun get-ftp-data-connection (state name size mode)
  ;; Establish connection if necessary
  (when (eq (ftpstate-data-his-address state) :default)
    (setf (ftpstate-data-his-address state) (send (ftpstate-ctrl-stream state) :remote-address))
    (setf (ftpstate-data-his-port state) (send (ftpstate-ctrl-stream state) :remote-port)))

  (let ((success (or (eq (ftpstate-data-connection-method state) :passive)
                     (make-ftp-data-connection state name size mode))))

    ;; reset defaults for next data transfer
    (setf (ftpstate-data-connection-method state) :active)
    (setf (ftpstate-data-his-address state) :default)

    (and success (ftpstate-data-stream state))))

(defun make-ftp-data-connection (state name size mode)
  (global:condition-case-if ftp-catch-errors (sig)
      (progn
        (setf (ftpstate-data-my-address state) (send (ftpstate-ctrl-stream state) :local-address))
        (setf (ftpstate-data-my-port state) (sym tcp-application:ipport-ftp-data))
        (setf (ftpstate-data-stream state) (tcpa:open-easy-tcp-stream (ftpstate-data-his-address state)
                                                                      (ftpstate-data-his-port state)
                                                                      (ftpstate-data-my-port state)
                                                                      :input-buffers (ecase mode
                                                                                       (:output 0)
                                                                                       (:input *ftp-buffers*))
                                                                      :output-buffers (ecase mode
                                                                                       (:output *ftp-buffers*)
                                                                                       (:input 0))
                                                                      :keyword "FTP Data Connection"))
        (ftp-reply state 150
                   "Opening data connection ~A(~A,~D)~A"
                   (if name (format nil "for ~A " name) "")
                   (canonical-ip (ftpstate-data-his-address state))
                   (ftpstate-data-his-port state)
                   (if size (format nil " (~D bytes)" size) ""))
        t)
    (error
      (let ((string (send sig :report-string)))
        (tv:notify nil "FTP SERVER DATA CONNECTION ERROR on ~S ~D: ~A"
                   (canonical-ip (ftpstate-data-his-address state))
                   (ftpstate-data-his-port state)
                   string)
        (ftp-reply state 425 "Can't open data connection: ~A" string))
      nil)))

(defun ftp-establish-passive-connection (state)
  (let ((stream (ftpstate-data-stream state)))
    (send stream :accept)
    (setf (ftpstate-data-his-address state) (send stream :remote-address))
    (setf (ftpstate-data-his-port state) (send stream :remote-port)))
  (setf (ftpstate-data-connection-method state) :passive)
  (setf (ftpstate-data-transfer-in-progress state) nil))


(defun ftp-cleanup-data-connection (state kill-process-p)
  (if (ftpstate-data-stream state)
      (global:condition-case-if ftp-catch-errors (err)
          (send (ftpstate-data-stream state) :close)
        (error (send (ftpstate-data-stream state) :abort))))
  (if kill-process-p
      (if (ftpstate-data-transfer-process state)
          (kill-subprocess (ftpstate-data-transfer-process state))))
  (setf (ftpstate-data-stream state) nil)
  (setf (ftpstate-data-my-address state) nil)
  (setf (ftpstate-data-his-address state) :default)
  (setf (ftpstate-data-connection-method state) :active)
  (setf (ftpstate-data-transfer-in-progress state) nil)
  (setf (ftpstate-data-transfer-process state) nil))


(defun ftp-send-data (state instream outstream directory-p)
  (setf (ftpstate-data-transfer-in-progress state) t)
  (setf (ftpstate-data-transfer-process state)
        (subprocess (ftp-send-data-function state
                                            instream outstream directory-p))))

(defun ftp-send-data-function (state instream outstream directory-p)
  (let ((translating-outstream
          (case (ftpstate-transfer-type state)
            (:ascii
             (ftp:make-ascii-translating-output-stream outstream nil))
            ((:image :logical-byte-size)
             (case (ftpstate-byte-size state)
               (8 outstream)
               (16 (make-16b-to-8b-translating-output-stream outstream))))
            (t
             nil)))
        (completedp nil))
    (unwind-protect
        (if (not translating-outstream)
            (ftp-reply state 504 "Unimplemented type ~A." (ftpstate-transfer-type state))
          (global:condition-case-if ftp-catch-errors (err)
              (progn (cond ((not directory-p)
                            (global:stream-copy-until-eof instream translating-outstream)
                            (or (eq translating-outstream outstream)
                                (send translating-outstream :force-output)))
                           ((eq directory-p t)
                            (do ((entry))
                                ((null (setq entry (send instream :line-in))))
                              (send translating-outstream :string-out entry)
                              (terpri translating-outstream)))
                           ((eq directory-p :directory-list)
                            (do ((entry))
                                ((null (setq entry (send instream :entry))))
                              (prin1 entry translating-outstream)
                              (terpri translating-outstream)))
                           ((eq directory-p :name-list)
                            (send instream :entry) ;; GET RID OF DISK-SPACE-DESCRIPTION
                            (do ((entry))
                                ((null (setq entry (send instream :entry))))
                              ;;***want lower case filename.type#version with no host or directory
                              (send translating-outstream :string-out
                                    (string-downcase (fs:enough-namestring (car entry) (ftpstate-pn-defaults state))))
                              (terpri translating-outstream))))
                     (setq completedp t))
            (fs:file-error (ftp-file-error-reply state err))
            (error (ftp-reply state 451 "Local error in processing: ~A." (send err :report-string)) nil)))
      (close instream)
      (if completedp (send outstream :force-output))
      (ftp-cleanup-data-connection state nil)
      (setf (ftpstate-data-transfer-in-progress state) nil)
      (if completedp (ftp-reply state 226 "Transfer complete")))))



(defun ftp-receive-data (state instream outstream)
  (setf (ftpstate-data-transfer-in-progress state) t)
  (setf (ftpstate-data-transfer-process state)
        (subprocess (ftp-receive-data-function state instream outstream))))


(defun ftp-receive-data-function (state instream outstream &aux success)
  "Transfer the contents of net instream to local outstream"
  (global:condition-case-if ftp-catch-errors (err)
      (case (ftpstate-transfer-type state)
        (:ascii
         (let ((ftp:*hash* nil))
           (declare (special ftp:*hash*))
           (with-open-stream (is (ftp:make-ascii-translating-input-stream instream))
             (global:stream-copy-until-eof is outstream)))
         (setq success t))
        ((:image :logical-byte-size)
         (case (ftpstate-byte-size state)
           (8
            (global:stream-copy-until-eof instream outstream))
           (16
            (let ((trans (make-8b-to-16b-translating-output-stream outstream)))
              (global:stream-copy-until-eof instream trans)
              (send trans :force-output))))
         (setq success t))
        (otherwise (error "Bad transfer type in FTPSTATE.")))
    (fs:file-error (ftp-file-error-reply state err) nil)
    (error (ftp-reply state 451 "Local error in processing: ~A." (send err :report-string)) nil))
  (close outstream)
  (ftp-cleanup-data-connection state nil)
  (setf (ftpstate-data-transfer-in-progress state) nil)
  (if success (ftp-reply state 226 "Transfer complete.")))

(defun ftp-reply (state code &rest format-args)
  (if (ftpstate-ctrl-stream state)
   (let ((*print-base* 10.))
     (push (apply #'format nil "~D ~@?" code format-args)
           (ftpstate-cmd-reply-history state))
     (when code
       (prin1 code (ftpstate-ctrl-stream state))
       (princ #\space (ftpstate-ctrl-stream state)))
     (apply #'format (ftpstate-ctrl-stream state) format-args)
     (send (ftpstate-ctrl-stream state) :tyo #o15)
     (send (ftpstate-ctrl-stream state) :tyo #o12)
     (send (ftpstate-ctrl-stream state) :force-output))))

(defun ftp-ack (state)
  (ftp-reply state 200 "~A command okay."
             (if (ftpstate-current-cmd state) (ftpstate-current-cmd state) "")))

(defun ftp-not-implemented (state command)
  (ftp-reply state 502 "~A command not implemented." command))

(defun ftp-delete (state name)
  (unless (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name)))
    (unless (ftp-file-error-reply state (ftp-file-operation state #'delete-file name))
      (ftp-ack state))))

(defun ftp-undelete (state name)
  (unless (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name)))
    (unless (ftp-file-error-reply state (ftp-file-operation state #'fs:undelete-file name))
      (ftp-ack state))))

(defun ftp-cwd (state name)
  (if (not name) (setq name (ftpstate-homedir-pn state)))
  (unless (and (not (pathnamep name))
               (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name))))
    (unless (ftp-file-error-reply state (ftp-file-operation state
                                                            #'fs:merge-and-set-pathname-defaults
                                                            name
                                                            (ftpstate-pn-defaults state)))
      (ftp-ack state))))

(defun ftp-xcup (state)
  (let ((fs:*defaults-are-per-host* nil) pn dirlist)
    (setq pn (fs:default-pathname (ftpstate-pn-defaults state)))
    (cond ((and (listp (setq dirlist (send pn :directory))) (cdr dirlist))
           (setq pn (send pn :new-pathname :directory (butlast dirlist)))
           (ftp-cwd state pn))
          (t (ftp-reply state 550 "~A is a top level directory." pn)))))

(defun ftp-expungedir (state &optional (name (fs:default-pathname (ftpstate-pn-defaults state))) &aux result)
  (unless (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name)))
    (unless (ftp-file-error-reply state (setq result (ftp-file-operation state #'fs:expunge-directory name)))
      (ftp-reply state 200 "~A blocks freed" result))))

(defun ftp-makedir (state name)
  (unless (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name)))
    (unless (ftp-file-error-reply state (ftp-file-operation state #'fs:create-directory name))
      (ftp-ack state))))

(defun ftp-xpwd (state)
  (let ((fs:*defaults-are-per-host* nil))
    (ftp-reply state 251 "\"~A\" is the current default pathname."
               (fs:default-pathname (ftpstate-pn-defaults state)))))

(defun ftp-renamefrom (state name)
  (unless (ftp-pathname-error-reply state (setq name (ftp-parse-pathname state name)))
    (unless (ftp-file-error-reply state (ftp-file-operation state #'probe-file name))
      (ftp-reply state 350 "File exists, ready for destination name.")
      name)))

(defun ftp-renameto (state from to)
  (unless (ftp-pathname-error-reply state (setq to (ftp-parse-pathname state to)))
    (unless (ftp-file-error-reply state (ftp-file-operation state #'rename-file from to))
      (ftp-ack state))))


(defun ftp-file-operation (state operation &rest args)
  (let* ((fs:user-id (cdar (ftpstate-user-unames state)) )
         (fs:host (fs:get-pathname-host (caar (ftpstate-user-unames state))))
         (fs:user-login-machine fs:host)
         (fs:user-unames (ftpstate-user-unames state))
         (fs:user-host-password-alist (ftpstate-user-host-password-alist state))
         (fs:catch-login-problems-p nil))
  (declare (special fs:user-id fs:host fs:user-login-machine fs:user-unames
                    fs:user-host-password-alist fs:catch-login-problems-p))
  (global:condition-case-if ftp-catch-errors (sig)
      (apply operation args)
    (error sig))))


(defun ftp-file-error-reply (state returned-value)
  (cond ((and (global:errorp returned-value) (global:condition-typep returned-value 'fs:login-problems))
         (ftp-reply state 550 "Valid password needed for remote Chaos file access")
         t)
        ((typep returned-value 'fs:file-error)
         (ftp-reply-to-file-system-error state returned-value)
         t)
        ((global:errorp returned-value)
         (ftp-reply state 451 "Unknown error /[~A/]" (type-of returned-value))
         t)))

(defun ftp-reply-to-file-system-error (state err)
  (ftp-reply state
             (cond ((get (type-of err) 'ftp-error-code)) (t 550))
             "File error /[~A/] ~A"
             (type-of err)
             (send err :report-string)))

(setf (get 'fs:file-open-for-output 'ftp-error-code) 450)
(setf (get 'fs:file-locked 'ftp-error-code) 450)
(setf (get 'fs:no-more-room 'ftp-error-code) 452)

(defun ftp-parse-pathname (state name &optional directory-p)
  (global:condition-case-if ftp-catch-errors (sig)
      (let ((fs:*defaults-are-per-host* nil)
            (fs:*always-merge-type-and-version* nil)
            (fs:*name-specified-default-type* :lisp)
            (parsed (fs:parse-pathname name
                                       (cond ((global:get-site-option :ftp-disallow-nonlocal-access)
                                              si:local-host)
                                             ((string-search ":" name)
                                              nil)
                                             (t
                                              (caar (ftpstate-pn-defaults state)))))))
        (fs:merge-pathname-components parsed
                                      (ftpstate-pn-defaults state)
                                      :default-name :wild
                                      :default-type :wild
                                      :default-version (if directory-p :wild :newest)))
    (error sig)))

(defun ftp-pathname-error-reply (state err)
  (cond ((global:errorp err) (ftp-reply state 451 "Error parsing pathname") t)))

(defparameter telnet-iac 255)

;;; a hacked up version of readline to ignore TELNET escape codes.
(defun ftp-readline (stream &optional (getc #'(lambda (stream)
                                                (send stream :tyi))))
  (catch 'eof-tag
    (with-output-to-string (s)
      (do ((c (funcall getc stream) (funcall getc stream)))
          ((eq c #o12))
        (cond ((null c)
               (throw 'eof-tag nil))
              ((= c telnet-iac)                 ;Telnet escape code
               (unless (setq c (funcall getc stream))
                 (throw 'eof-tag nil))
               (case (cadr (assoc c telnet:*telsyms* :test #'eq))
                 (telnet:do                     ;DO -- send a WONT
                  (unless (setq c (funcall getc stream))
                    (throw 'eof-tag nil))
                  (telnet:send-iac stream 'telnet:wont c))
                 (telnet:will                   ;WILL -- send a DONT
                  (unless (setq c (funcall getc stream))
                    (throw 'eof-tag nil))
                  (telnet:send-iac stream 'telnet:dont c))
                 ((telnet:dont telnet:wont)     ;DONT or WONT -- ignore option
                  (unless (setq c (funcall getc stream))
                    (throw 'eof-tag nil)))))
              ((= c #o15))                      ;Ignore CR -- wait for LF
              (t
               (send s :tyo c)))))))


(defparameter ftp-cmdlist '(user pass acct cwd  cdup smnt quit
                                 rein port pasv type stru mode
                                 retr stor stou appe allo rest
                            rnfr rnto abor dele rmd  mkd  pwd
                            list nlst site syst stat help noop
                            xlst xmkd xrmd xpwd xcup xpng xund
                            mlfl mail msnd msom msam srsq mrcp
                            ))

(setf (get 'user 'ftp-help) " <userid> or <userid>@<Chaos-host>")
(setf (get 'pass 'ftp-help) " <password>")
(setf (get 'cwd 'ftp-help) " <optional-pathname>")
(setf (get 'cdup 'ftp-help) "")
(setf (get 'quit 'ftp-help) "")
(setf (get 'rein 'ftp-help) "")
(setf (get 'port 'ftp-help) " <a1>,<a2>,<a3>,<a4>,<p1>,<p2>")
(setf (get 'pasv 'ftp-help) "")
(setf (get 'type 'ftp-help) " <type-code>")
(setf (get 'stru 'ftp-help) " <structure-code>")
(setf (get 'mode 'ftp-help) " <mode-code>")
(setf (get 'retr 'ftp-help) " <pathname>")
(setf (get 'stor 'ftp-help) " <pathname>")
(setf (get 'appe 'ftp-help) " <pathname>")
(setf (get 'allo 'ftp-help) "")
(setf (get 'rnfr 'ftp-help) " <pathname>")
(setf (get 'rnto 'ftp-help) " <pathname>")
(setf (get 'abor 'ftp-help) "")
(setf (get 'dele 'ftp-help) " <pathname>")
(setf (get 'mkd 'ftp-help) " <pathname>")
(setf (get 'pwd 'ftp-help) "")
(setf (get 'list 'ftp-help) " <optional-pathname> -- LISTF format")
(setf (get 'nlst 'ftp-help) " <optional-pathname> -- names only")
(setf (get 'help 'ftp-help) " <optional-cmd>")
(setf (get 'noop 'ftp-help) "")
(setf (get 'xlst 'ftp-help) " <optional-pathname> -- :DIRECTORY-LIST format")
(setf (get 'xmkd 'ftp-help) " <pathname>")
(setf (get 'xpwd 'ftp-help) "")
(setf (get 'xcup 'ftp-help) "")
(setf (get 'xpng 'ftp-help) " <optional-pathname>")
(setf (get 'xund 'ftp-help) " <pathname>")

(defparameter ftp-unimplemented-cmdlist '(acct smnt stou rest rmd site syst stat
                                          mlfl mail msnd msom msam srsq mrcp xrmd))
(defparameter ftp-logged-in-cmdlist '(cwd  cdup port pasv retr stor appe allo rnfr rnto abor
                                      dele rmd  mkd  pwd  list nlst
                                      xlst xmkd xrmd xpwd xcup xpng xund))
(defparameter ftp-require-arg-cmdlist '(user port type stru mode retr stor appe rnfr rnto
                                        dele mkd  xmkd xund))
(defparameter ftp-allow-spaces-in-arg-cmdlist '(pass retr stor appe rnfr rnto dele cwd
                                                list nlst mkd  xlst xmkd xpng xund))

(defun ftp-help (state cmdstring &aux cmd)
  (cond (cmdstring
         (setq cmd (ftp-cmd-from-string cmdstring))
         (if (symbolp cmd)
             (if (member cmd ftp-unimplemented-cmdlist :test #'eq)
                 (ftp-reply state 214 "~A unimplemented." cmd)
               (ftp-reply state 214 "Syntax: ~A~A" cmd (get cmd 'ftp-help)))
           (ftp-reply state 504 "Unknown command: ~A" cmd)))
        (t
         (ftp-reply state nil
                    "214-The following commands are recognized (* =>'s unimplemented).")

         (zl:loop for l on ftp-cmdlist by #'(lambda (l) (nthcdr 6 l))
                  while l
                  do (ftp-reply state nil
                                (apply #'string-append
                                       "    "
                                       (zl:loop for x in l
                                                for count from 1 to 6
                                                collect (format nil "~A~4A"
                                                                (if (member x ftp-unimplemented-cmdlist
                                                                              :test #'eq)
                                                                    " *"
                                                                  "  ")
                                                                x))))
                  finally (ftp-reply state 214 "Direct comments to ~A."
                                     (or (global:get-site-option :ftp-server-administrator)
                                         *ftp-server-administrator*))))))


(defun ftp-cmd-from-string (string)
  ; returns an atom if the command is recognized, otherwise a string.
  (setq string (string-trim '(#\Space) string))
  (setq string (substring string 0 (string-search-char #\Space string)))
  (cond ((car (member string ftp-cmdlist :test #'string-equal)))
        (string)))

(defun ftp-first-cmd-arg (cmdline allow-spaces-p &aux temp)
  (if (setq temp (string-search-char #\Space (setq cmdline (string-trim '(#\Space) cmdline))))
      (string-trim '(#\Space)
                   (substring cmdline
                              (1+ temp)
                              (string-search-set
                                (if allow-spaces-p '(#o15) '(#o15 #\Space)) cmdline (1+ temp))))))

(defun ftp-second-cmd-arg (cmdline)
  (ftp-first-cmd-arg (substring cmdline (string-search-char #\Space cmdline)) nil))

(defun parse-his-port (arg)
  (cond ((or (not arg) (zerop (length arg))) nil)
        (t
         (do ((j 1 (1+ j))
              (inet nil)
              (port nil)
              (n)
              (from 0))
             ((> j 6)
              (values inet port))
           (let ((to (or (string-search "," arg from)
                         (length arg))))
             (cond ((not (setq n (parse-integer arg :start from :end to))))
                   ((> j 4)
                    (setq port (+ n (* 256 (or port 0)))))
                   (t
                    (setq inet (+ n (* 256 (or inet 0))))))
             (setq from (min (1+ to) (length arg))))))))

(defun ftp-cmdparse (state)
  (let ((cmdline (ftp-readline (ftpstate-ctrl-stream state)
                               #'(lambda (stream)
                                   (loop
                                     (when (process-wait-with-timeout "FTP command"
                                                                      (and *ftp-server-timeout* (* 60. *ftp-server-timeout*))
                                                                      #'(lambda (s)
                                                                          (send s :listen))
                                                                      stream)
                                       ;;:listen returned non-NIL -- return what :tyi returns
                                       (return (send stream :tyi)))
                                     (unless (ftpstate-data-transfer-in-progress state)
                                       ;;:listen returned NIL -- timeout.  If control connection idle, return NIL
                                       (return nil))))))
        cmd arg)
    (cond ((not cmdline) (ftp-reply state 221 "You could at least say goodbye.") nil)
          (t
           (push cmdline (ftpstate-cmd-reply-history state))
           (setq cmd (ftp-cmd-from-string cmdline))
           (if (and (ftpstate-data-transfer-in-progress state) (not (eq cmd 'abor)))
               (process-wait "wait for data transfer"
                             #'(lambda nil (not (ftpstate-data-transfer-in-progress state)))))
           (setf (ftpstate-current-cmd state) cmd)
           (setq arg (ftp-first-cmd-arg cmdline (member cmd ftp-allow-spaces-in-arg-cmdlist :test #'eq)))
           (cond ((and (ftpstate-next-cmd state)
                       (not (eq cmd 'rein))
                       (not (eq cmd 'quit))
                       (not (eq cmd (ftpstate-next-cmd state))))
                  (ftp-reply state 503 "Bad sequence of commands.")
                  (setf (ftpstate-next-cmd state) nil))

                 ((stringp cmd) (ftp-reply state 500 "~A command unrecognized" cmd))

                 ((member cmd ftp-unimplemented-cmdlist :test #'eq) (ftp-not-implemented state cmd))

                 ((and (not (ftpstate-logged-in-p state)) (member cmd ftp-logged-in-cmdlist :test #'eq))
                  (ftp-reply state 530 "Not logged in."))

                 ((and (or (not arg) (zerop (length arg)))
                       (member cmd ftp-require-arg-cmdlist :test #'eq))
                  (ftp-reply state 501 "Syntax error: missing argument."))

                 (t (funcall (get cmd 'ftp-server-handle) state arg cmdline)))
           (not (eq cmd 'quit))))))

(defun (:property user ftp-server-handle) (state arg cmdline) cmdline
  (let ((user arg) host idx hd (host-object si:local-host))
    (if (setq idx (string-search-char #\@ arg))
        (setq user (substring arg 0 idx)
              host (substring arg (1+ idx))))
    (cond ((and host (not (setq host-object (si:parse-host host t))))
           (ftp-reply state 530 "~A unknown remote host." host))
          (t
           (setf (ftpstate-user-unames state)
                 (cons
                   (setf (ftpstate-last-data state)
                         `(,host-object . ,user))
                   (ftpstate-user-unames state)))
           (setq hd (ftp-get-homedir state user host-object))
           (cond ((and (global:errorp hd) (global:condition-typep hd 'fs:login-problems))
                  (ftp-reply state 331 "Password required for ~A." arg)
                  (setf (ftpstate-user-unames state)
                        (cdr (ftpstate-user-unames state)))
                  (setf (ftpstate-next-cmd state) 'pass))
                 ((global:errorp hd)
                  (ftp-reply state 530
                             "Error getting homedir [~A], not logged in."
                             (type-of hd))
                  (setf (ftpstate-user-unames state)
                        (cdr (ftpstate-user-unames state))))
                 ((si:global:get-site-option :server-password-database)
                  (ftp-reply state 331 "Password required for ~A." arg)
                  (setf (ftpstate-user-unames state)
                        (cdr (ftpstate-user-unames state)))
                  (setf (ftpstate-next-cmd state) 'pass))
                 (t
                  (ftp-login state user host-object nil hd)))))))

(defun (:property pass ftp-server-handle) (state arg cmdline) cmdline
  (cond ((not (eq 'pass (ftpstate-next-cmd state)))
         (ftp-reply state 503 "Bad sequence of commands."))
        (t (setf (ftpstate-next-cmd state) nil)
           (setf (ftpstate-user-unames state)
                 (cons (ftpstate-last-data state)
                       (ftpstate-user-unames state)))
           (if arg
               (setf (ftpstate-user-host-password-alist state)
                     (cons `((,(cdr (ftpstate-last-data state))
                              ,(send (car (ftpstate-last-data state)) :name))
                             ,arg)
                           (ftpstate-user-host-password-alist state))))
           (let ((hd (ftp-get-homedir state (cdr (ftpstate-last-data state))
                                      (car (ftpstate-last-data state)))))
             (cond ((global:errorp hd)
                    (ftp-reply state 530 "Not logged in: ~A."
                               (send hd :report-string))
                    (setf (ftpstate-user-unames state)
                          (cdr (ftpstate-user-unames state)))
                    (if arg
                        (setf (ftpstate-user-host-password-alist state)
                              (cdr (ftpstate-user-host-password-alist state)))))
                   (t (ftp-login state
                                 (cdr (ftpstate-last-data state))
                                 (car (ftpstate-last-data state))
                                 arg hd)))))))

(defun (:property rein ftp-server-handle) (state arg cmdline) arg cmdline
  (reinitialize-ftpstate state)
  (ftp-reply state 220 "Service ready for new user."))

(defun (:property quit ftp-server-handle) (state arg cmdline) arg cmdline
  (reinitialize-ftpstate state)
  (ftp-reply state 221 "Goodbye."))

(defun (:property port ftp-server-handle) (state arg cmdline) cmdline
  (multiple-value-bind (addr port)
      (parse-his-port arg)
    (cond ((and addr port)
           (setf (ftpstate-data-his-address state) addr)
           (setf (ftpstate-data-his-port state) port)
           (ftp-ack state))
          (t (ftp-reply state 501 "Syntax error in parameters or arguments.")))))

(defun (:property pasv ftp-server-handle) (state arg cmdline) cmdline arg
  (let* ((stream (open "TCP-HOST:" :connect nil))
         (address (send stream :local-address))
         (port (send stream :local-port)))
    (setf (ftpstate-data-stream state) stream)
    (setf (ftpstate-data-my-address state) address)
    (setf (ftpstate-data-my-port state) port)
    (ftp-reply state 227 "Entering passive mode.  ~D,~D,~D,~D,~D,~D"
               (ldb (byte 8 24) address)
               (ldb (byte 8 16) address)
               (ldb (byte 8 8) address)
               (ldb (byte 8 0) address)
               (ldb (byte 8 8) port)
               (ldb (byte 8 0) port)))
  (setf (ftpstate-data-transfer-in-progress state) t)
  (setf (ftpstate-data-transfer-process state)
        (subprocess (ftp-establish-passive-connection state))))

(defun (:property type ftp-server-handle) (state arg cmdline)
  (let ((arg2 (ftp-second-cmd-arg cmdline)))
    (case (char-upcase (aref arg 0))
      (#\A
       (cond ((and arg2 (not (zerop (length arg2)))
                   (char-not-equal (aref arg2 0) #\n))
              (ftp-reply state 504 "Form must be N."))
             (t
              (setf (ftpstate-byte-size state) 8)
              (setf (ftpstate-transfer-type state) :ascii)
              (ftp-reply state 200 "Type set to A."))))
      (#\E
       (ftp-reply state 504 "Type E not implemented."))
      (#\I
       (ftp-reply state 200 "Type set to I.")
       (setf (ftpstate-byte-size state) 8)
       (setf (ftpstate-transfer-type state) :image))
      (#\L
       (cond ((not arg2)
              (setq arg2 8))
             ((setq arg2 (parse-integer arg2 :radix 10 :junk-allowed t))))
       (cond ((member arg2 '(8 16) :test #'eq)
              (setf (ftpstate-transfer-type state)
                    :logical-byte-size)
              (setf (ftpstate-byte-size state) arg2)
              (ftp-reply state 200 "Type set to L (byte size ~D)." arg2))
             (t
              (ftp-reply state 504 "Byte size must be 8 or 16"))))
      (otherwise
       (ftp-reply state 504 "Command not implemented for that parameter.")))))

(defun (:property stru ftp-server-handle) (state arg cmdline) cmdline
  (case (aref arg 0)
    ((#\f #\F)
     (setf (ftpstate-transfer-structure state) :file)
     (ftp-reply state 200 "Structure set to F."))
    (otherwise
     (ftp-reply state 504 "Command not implemented for that parameter."))))

(defun (:property mode ftp-server-handle) (state arg cmdline) cmdline
  (case (aref arg 0)
    ((#\s #\S)
     (setf (ftpstate-transfer-mode state) :stream)
     (ftp-reply state 200 "Mode set to S."))
    (otherwise
     (ftp-reply state 504 "Command not implemented for that parameter."))))

(defun (:property allo ftp-server-handle) (state arg cmdline) cmdline
  (cond ((setq arg (parse-integer arg :radix 10. :junk-allowed t))
         (setf (ftpstate-estimated-length state) arg)
         (ftp-ack state))
        (t (ftp-reply state 501 "Syntax error in parameters or arguments."))))

(defun (:property retr ftp-server-handle) (state arg cmdline) cmdline
  (ftp-retrieve state arg nil))

(defun (:property stor ftp-server-handle) (state arg cmdline) cmdline
  (ftp-store state arg :new-version))

(defun (:property appe ftp-server-handle) (state arg cmdline) cmdline
  (ftp-store state arg :append))

(defun (:property rnfr ftp-server-handle) (state arg cmdline) cmdline
  (let ((from-pn (ftp-renamefrom state arg)))
    (cond (from-pn
           (setf (ftpstate-next-cmd state) 'rnto)
           (setf (ftpstate-last-data state) from-pn)))))

(defun (:property rnto ftp-server-handle) (state arg cmdline) cmdline
  (ftp-renameto state (ftpstate-last-data state) arg))

(defun (:property abor ftp-server-handle) (state arg cmdline) arg cmdline
  (if (ftpstate-data-transfer-in-progress state)
      (ftp-reply state 426 "Connection closed, transfer aborted."))
  (ftp-reply state 226 "Abort command complete.")
  (ftp-cleanup-data-connection state t))

(defun (:property dele ftp-server-handle) (state arg cmdline) cmdline
  (ftp-delete state arg))

(defun (:property xund ftp-server-handle) (state arg cmdline) cmdline
  (ftp-undelete state arg))

(defun (:property cwd ftp-server-handle) (state arg cmdline) cmdline
  (ftp-cwd state (if (or (not arg) (zerop (length arg))) nil arg)))

(defun (:property xpng ftp-server-handle) (state arg cmdline) cmdline
  (ftp-expungedir state (nullify-arg-if-empty arg)))

(defun (:property list ftp-server-handle) (state arg cmdline) cmdline
  (ftp-retrieve state (nullify-arg-if-empty arg) t))

(defun (:property nlst ftp-server-handle) (state arg cmdline) cmdline
  (ftp-retrieve state (nullify-arg-if-empty arg) :name-list))

(defun (:property xlst ftp-server-handle) (state arg cmdline) cmdline
  (ftp-retrieve state (nullify-arg-if-empty arg) :directory-list))

(defun (:property mkd ftp-server-handle) (state arg cmdline) cmdline
  (ftp-makedir state arg))

(defun (:property pwd ftp-server-handle) (state arg cmdline) arg cmdline
  (ftp-xpwd state))

(defun (:property cdup ftp-server-handle) (state arg cmdline) arg cmdline
  (ftp-xcup state))

(defun (:property xmkd ftp-server-handle) (state arg cmdline) cmdline
  (ftp-makedir state arg))

(defun (:property xpwd ftp-server-handle) (state arg cmdline) arg cmdline
  (ftp-xpwd state))

(defun (:property xcup ftp-server-handle) (state arg cmdline) arg cmdline
  (ftp-xcup state))

(defun (:property help ftp-server-handle) (state arg cmdline) cmdline
  (ftp-help state (nullify-arg-if-empty arg)))

(defun (:property noop ftp-server-handle) (state arg cmdline) arg cmdline
  (ftp-ack state))


;; some translating streams


(defflavor 16b-to-8b-translating-output-stream
         ((output nil)
          (bytes 0))
         (si:buffered-output-stream)
  (:initable-instance-variables output))

(defmethod (16b-to-8b-translating-output-stream :new-output-buffer) ()
  (declare (values array start end))
  (values (global:allocate-resource 'fs:simple-art-16b-buffer 1000)
          0
          1000))

(defmethod (16b-to-8b-translating-output-stream :send-output-buffer) (array end)
  (let ((byte-count (* end 2)))
    (incf bytes byte-count)
    (send output
          :string-out
          (make-array byte-count :element-type 'string-char :displaced-to array)
          0
          byte-count))
  (global:deallocate-resource 'fs:simple-art-16b-buffer array))

(defmethod (16b-to-8b-translating-output-stream :discard-output-buffer) (array)
  (global:deallocate-resource 'fs:simple-art-16b-buffer array))

(defmethod (16b-to-8b-translating-output-stream :after :close) (&optional mode)
  (send output :close mode))

(defmethod (16b-to-8b-translating-output-stream :bytes) ()
  bytes)

(defflavor 8b-to-16b-translating-output-stream
         (output)
         (si:buffered-output-stream)
  (:initable-instance-variables output))

(defmethod (8b-to-16b-translating-output-stream :new-output-buffer) ()
  (declare (values array start end))
  (values (global:allocate-resource 'fs:simple-string-buffer 2000)
          0
          2000))

(defmethod (8b-to-16b-translating-output-stream :send-output-buffer) (array end)
  (send output :string-out (make-array (floor end 2)
                                       :element-type '(unsigned-byte 16)
                                       :displaced-to array))
  (global:deallocate-resource 'fs:simple-string-buffer array))

(defmethod (8b-to-16b-translating-output-stream :discard-output-buffer) (array)
  (global:deallocate-resource 'fs:simple-string-buffer array))

(defmethod (8b-to-16b-translating-output-stream :after :close) (&optional mode)
  (send output :close mode))

(global:compile-flavor-methods 16b-to-8b-translating-output-stream 8b-to-16b-translating-output-stream)


(defun make-16b-to-8b-translating-output-stream (output)
  (make-instance '16b-to-8b-translating-output-stream :output output))

(defun make-8b-to-16b-translating-output-stream (output)
  (make-instance '8b-to-16b-translating-output-stream :output output))

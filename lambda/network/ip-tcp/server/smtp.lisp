;;; -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Readtable:CL -*-

#||

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

Simple implementation of SMTP.

||#


(define-network-service *tcp-smtp-service* :smtp :tcp "Simple Mail Transfer Protocol (RFC 821)"
  :listen-port (sym ipport-smtp)
  :toplevel-function 'smtp-server-function
  :auto-enable? t)

(defvar *smtp-server-state* nil "This is a plist")

(defmacro smtp-server-state (keyword)
  `(get *smtp-server-state* ,keyword))

(defun smtp-server-function (stream)
  (smtp-reply stream 220 "~A Simple Mail Transfer Service Ready"
              (send si:local-host :name))
  (catch 'smtp-server-function
    (do ((*smtp-server-state* nil)
         (si:user-id "SMTP_SERVER")
         (string (ftp:ftp-readline stream)
                 (ftp:ftp-readline stream)))
        ((null string))
      (smtp-server-command string stream))))

(defun smtp-reply (stream code &rest format-args)
  (apply #'format stream "~D ~@?" code format-args)
  (smtp-terpri stream)
  (send stream :force-output))

(defun smtp-terpri (stream)
  (send stream :tyo #o15)
  (send stream :tyo #o12))

(defvar *smtp-commands*
        '(
          ;;The following are the SMTP commands:
          (helo "<SP> <domain> <CRLF>")
          (mail "<SP> FROM:<reverse-path> <CRLF>")
          (rcpt "<SP> TO:<forward-path> <CRLF>")
          (data "<CRLF>")
          (rset "<CRLF>")
          (send "<SP> FROM:<reverse-path> <CRLF>")
          (soml "<SP> FROM:<reverse-path> <CRLF>")
          (saml "<SP> FROM:<reverse-path> <CRLF>")
          (vrfy "<SP> <string> <CRLF>")
          (expn "<SP> <string> <CRLF>")
          (help "[<SP> <string>] <CRLF>")
          (noop "<CRLF>")
          (quit "<CRLF>")
          (turn "<CRLF>")))

(defun smtp-server-command (string stream)
  (let ((command (global:intern-soft (string-upcase
                                       (if (string-search " " string)
                                           (substring string 0 (string-search " " string))
                                         string))
                                     "TCP-APPLICATION")))
    (cond ((and command (get command 'smtp-command))
           (funcall (get command 'smtp-command)
                    (string-trim " " (substring string (string-length command)))
                    stream))
          ((and command (assoc command *smtp-commands* :test #'eq))
           (smtp-reply stream 502 "Command not implemented"))
          (t
           (smtp-reply stream 500 "Syntax error, command unrecognized: ~A"
                       string)))))


(defun (helo smtp-command) (string stream)
  (setf (smtp-server-state :correspondant-host) string)
  (smtp-reply stream 250 "~A" (send si:local-host :name)))

(defun (help smtp-command) (string stream &aux temp)
  (cond ((string-equal string "")
         (format stream "211- Commands==> (* means not implemented)" stream)
         (smtp-terpri stream)
         (princ "211- " stream)
         (do ((j 0 (1+ j))
              (l *smtp-commands* (cdr l)))
             ((null l)
              (smtp-terpri stream))
           (princ (caar l) stream)
           (or (get (caar l) 'smtp-command) (princ "*" stream))
           (cond ((null (cdr l)))
                 ((= 5 (mod j 6))
                  (smtp-terpri stream)
                  (princ "211- " stream))
                 (t
                  (princ "   " stream))))
         (smtp-reply stream 211 "HELP <command> for more information"))
        ((setq temp (assoc string *smtp-commands* :test #'string-equal))
         (smtp-reply stream 214 "~A ~A" (car temp) (cadr temp)))
        (t
         (smtp-reply stream 214 "Unknown command: ~A" string))))


(defun (noop smtp-command) (string stream)
  string
  (smtp-reply-ok stream))


(defun smtp-reply-ok (stream)
  (smtp-reply stream 250 "Ok"))

(defun (quit smtp-command) (string stream)
  string
  (smtp-reply stream 221 "~A Service closing transmission channel"
              (send si:local-host :name))
  (throw 'smtp-server-function nil))



(defun (rset smtp-command) (string stream)
  string
  (setq *smtp-server-state* nil)
  (smtp-reply-ok stream))



(defun (mail smtp-command) (string stream)
  (smtp-mail-cmd string stream nil))

(defun smtp-mail-cmd (string stream terminal-modep)
  (let ((f (string-search "FROM:" string)))
    (cond ((null f)
           (smtp-reply stream 501 "No FROM: field found"))
          (t
           (setf (smtp-server-state 'from)
                 (list (substring string 0)
                       (string-right-trim
                         ">"
                         (string-left-trim "<" (substring string (+ f (length "FROM:")))))))
           (dolist (x '(to data))
             (setf (smtp-server-state x) nil))
           (setf (smtp-server-state 'terminal-modep) terminal-modep)
           (smtp-reply-ok stream)))))


(defun (send smtp-command) (string stream)
  (smtp-mail-cmd string stream t))

(defvar *smtp-mailer* :simple
  "A keyword identifying the mailer for use with the server, when
not using interactive ``terminal'' messages.

It should have two properties:

SMTP-RCPT-VERIFIER:
A function which takes arguments of the original argument string,
the address string and SMTP stream.  It should frob the SMTP-SERVER-STATE,
and do replies with SMTP-REPLY{-OK}.

SMTP-MAILER:
A function which delivers mail.  The arguments are the transmitting host
as a string, a list of the lines of the message, and the SMTP stream.
It should also look at SMTP-SERVER-STATE and do the replies with
SMTP-REPLY{-OK}.")

(defun simple-rcpt-verifier (string address-string stream)
  (let ((addr (smtp-parse-to-address address-string)))
    (if (null addr)
        (smtp-reply stream 501 "Syntax error in parameters or arguments")
      (let ((mb (smtp-lookup-mailbox addr)))
        (cond ((null mb)
               (smtp-reply stream 550 "unknown local user"))
              ((not (smtp-mailbox-probe mb))
               (smtp-reply stream 450 "mailbox unavailable"))
              (t
               (push (list (string-append string "") addr mb) (smtp-server-state 'to))
               (smtp-reply-ok stream)))))))

(setf (get :simple 'smtp-rcpt-verifier) 'simple-rcpt-verifier)

(defun (rcpt smtp-command) (string stream)
  (let ((f (string-search "TO:" string)))
    (if (null f)
        (smtp-reply stream 501 "No TO: field found")
      ;; Only use the mailer hook when not sending to the user.
      (funcall (if (smtp-server-state 'terminal-modep)
                   #'simple-rcpt-verifier
                 (get *smtp-mailer* 'smtp-rcpt-verifier))
               (copy-seq string) ; original argument, copied
               (string-left-trim " <"
                                 (string-right-trim "> "
                                                    (substring string
                                                               (+ f (length "TO:")))))
               stream))))

(defun smtp-parse-to-address (st &aux n host)
  (cond ((setq n (string-search "@" st))
         (setq host (si:parse-host (substring st (1+ n)) t nil))
         (if host
             (list (substring st 0 n) host)))
        (t
         (list st si:local-host))))

(defun smtp-lookup-mailbox (address)
  (cond ((smtp-server-state 'terminal-modep)
         (if (and (string-equal (car address)
                                (si:symbol-value-globally 'si:user-id))
                  (eq (cadr address) si:local-host))
             ;; in fact, not correct, we must also handle the si:*other-processors*
             address))
        (t
         (smtp-lookup-mailbox-1 address))))

(defun smtp-lookup-mailbox-1 (address &aux f)
  ;; this could probably be modified to work off of a list of
  ;; users names and mailboxes. In fact, without that there is
  ;; no way of telling the difference between an unknown user
  ;; and a SYS:REMOTE-NETWORK-ERROR (that is, if there is a remote
  ;; network error due to host unavailability then we cannot tell if
  ;; the name is legal and can be retried).
  (or (eq (cadr address) si:local-host)
      (do ((l si:*other-processors* (cdr l)))
          ((null l) (return-from smtp-lookup-mailbox-1 nil))
        (let ((h (si:get-host-from-address (si:%processor-conf-chaos-address
                                             (si:op-proc-conf (car l)))
                                           :chaos)))
          (and (eq (send h :system-type) :lispm)
               (eq (cadr address) h)
               (return)))))
  (setf f (fs:make-pathname :host (cadr address)
                            :directory (car address)
                            :name "MAIL"
                            :type "TEXT"))
  (global:condition-case ()
      (list address (probe-file f) f)
    (fs:directory-not-found nil)
    (sys:remote-network-error
     (list address :host-not-available))))

(defun smtp-mailbox-probe (mb)
  (cond ((smtp-server-state 'terminal-modep)
         (cond (zwei:*converse-gagged*
                nil)
               (t
                mb)))
        ((eq (cadr mb) :host-not-available)
         nil)
        (t
         mb)))

(defun simple-local-mailer (host data stream)
  (unless (smtp-server-state 'terminal-modep)
    (push (format nil "Received: from ~A by ~A with SMTP; ~\\time\\"
                  host
                  si:local-host
                  (time:get-universal-time))
          data))
  (dolist (mbx (smtp-server-state 'to))
    (smtp-send-one-message mbx
                           (smtp-server-state 'from)
                           (mapcar 'car (smtp-server-state 'to))
                           data))
  (smtp-reply-ok stream))

(setf (get :simple 'smtp-mailer) 'simple-local-mailer)

(defun (data smtp-command) (IGNORE stream &aux data)
  (cond ((not (smtp-server-state 'from))
         (smtp-reply stream 554 "no from: given"))
        ((not (smtp-server-state 'to))
         (smtp-reply stream 554 "no legal to: given; nobody to send it to"))
        (t
         (let ((string (make-array 80
                                   :element-type 'string-char
                                   :fill-pointer 0 :adjustable t)))
           (smtp-reply stream 354 "Start mail input; end with <CRLF>.<CRLF>")
           (do ()
               ((progn (setf (fill-pointer string) 0)
                       (do ((c))
                           ((null (setq c (send stream :tyi)))
                            (throw 'smtp-server-function nil))
                         (cond ((and (= c #\.) (= (length string) 1)))
                               ((= c #o15)
                                (send stream :tyi)
                                (if (string-equal string ".") (return t))
                                (push (substring string 0) data)
                                (return nil))
                               (t
                                (vector-push-extend
                                  (if (member c '(12 9) :test #'eq) (+ c #o200) c)
                                  string)))))))
           (setq data (nreverse data))
           (funcall (if (smtp-server-state 'terminal-modep)
                        #'simple-local-mailer
                      (get *smtp-mailer* 'smtp-mailer))
                  (smtp-identify-corresponding-host)
                  data
                  stream)
           ;; Reset the state -- there can be more than one session.
           (setf (smtp-server-state 'from) nil)
           (setf (smtp-server-state 'to) nil)))))

(defun smtp-identify-corresponding-host ()
  (if (get *smtp-server-state* :correspondant-host)
      (format nil "~A(~A)"
              (get *smtp-server-state* :correspondant-host)
              (network-server-client))
    (network-server-client)))

(defun smtp-send-one-message (desc from to data)
  (cond ((smtp-server-state 'terminal-modep)
         (smtp-send-one-message-to-terminal desc (cadr from) to data))
        (t
         (smtp-send-one-message-to-mailbox desc (car from) to data))))

(defun smtp-send-one-message-to-mailbox (desc from to data)
  ;; actually need a lock on the FILE in question.
  ;; e.g. ZMAIL might be hacking this file too.
  (with-lock ((get 'smtp-send-one-message-to-mailbox 'lock))
    (let ((mbx (caddr desc)))
      (let ((pfile (cadr mbx))
            (file (caddr mbx)))
        ;; if (probe-file file) is not eq to pfile
        ;; then there has been activity. could be interesting.
      (cond ((not (setq pfile (probe-file file)))
             (with-open-file (stream file :out)
               (smtp-output-one-message from to data stream)))
            (t
             ;; But we would also really like to use APPEND mode
             ;; on this file!
             (with-open-file (stream-out (send pfile :new-version
                                               (1+ (send pfile :version)))
                                         :out)
               (with-open-file (stream-in pfile :in)
                 (global:stream-copy-until-eof stream-in stream-out))
               (smtp-output-one-message from to data stream-out))
             (delete-file pfile)
             (fs:expunge-directory pfile)))))))

(defun smtp-send-one-message-to-terminal (desc from to data)
  desc to
  (zwei:converse-receive-from-network (format nil "~{~A~^~%~}" data)
                                      :sender from))


(defvar *smtp-output-synthetic-header* nil)

(defun smtp-output-one-message (from to data stream)
  (when *smtp-output-synthetic-header*
    (send stream :line-out from)
    (dolist (x to)
      (send stream :line-out x)))
  (dolist (x data)
    (send stream :line-out x))
  (send stream :line-out ""))

(defun valid-lispm-host (host)
  (and (si:parse-host host t nil)
       (eq :lispm (send (si:parse-host host t nil) :system-type))))

(defun make-lispm-zmail-init-file (username host)
  "This function makes a ZMAIL init file for a user on a lispmachine"
  (check-type username string)
  (check-type host (satisfies valid-lispm-host) "the name of a lispmachine host")
  (setq host (send (si:parse-host host) :name))
  (let ((mail-file (fs:make-pathname :host host
                                     :directory username
                                     :name "MAIL"
                                     :type "TEXT"))
        (babyl-file (fs:make-pathname :host host
                                      :directory username
                                      :name "BABYL"
                                      :type "TEXT"))
        (zmail-init (fs:make-pathname :host host
                                      :directory username
                                      :name "ZMAIL"
                                      :type "INIT")))
    (with-open-file (stream zmail-init :out)
      (format stream ";;-*-mode:lisp;package:zwei;base:10-*-~
                      ~%(login-setq *zmail-startup-file-name* ~S)~
                      ~%(login-setq *from-user-id* ~S)~
                      ~%(login-setq *from-host* ~S)~%"
              (send babyl-file :string-for-printing)
              username
              host))
    (with-open-file (stream babyl-file :out)
      (format stream "Babyl Options:~
                        ~%Append:1~
                        ~%Version:5~
                        ~%Mail: ~A~
                        ~%Owner:~A~
                        ~%Summary-window-format: T~
                        ~%"
              (send mail-file :string-for-printing)
              username))))

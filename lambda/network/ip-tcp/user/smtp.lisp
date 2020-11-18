;;; -*- Mode:LISP; Package:FTP; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1987
   See filename "Copyright.Text" for
  licensing and release information.

This calls subroutines and shares variables with the FTP user.

Quick hack version: The function FTP:SMTP-SEND-IT may be
the value of ZWEI:*MAIL-SENDING-MODE* and enable mail to be sent
to people on the tcp/ip supporting machines on your local ethernet.
 5/31/85 16:40:26 -George Carrette

|#

(defflavor smtp-user
           ((*trace* nil)
            (*hash* nil)
            (*verbose* nil)
            (*debug* nil)
            (*connected* nil)
            (*remote-hostname* nil)
            (*control* nil)
            (*data* nil)
            (*history* nil))
           ()
  :special-instance-variables)


#|

                     Example of the SMTP Procedure

         This SMTP example shows mail sent by Smith at host Alpha.ARPA,
         to Jones, Green, and Brown at host Beta.ARPA.  Here we assume
         that host Alpha contacts host Beta directly.

            S: MAIL FROM:<Smith@Alpha.ARPA>
            R: 250 OK

            S: RCPT TO:<Jones@Beta.ARPA>
            R: 250 OK

            S: RCPT TO:<Green@Beta.ARPA>
            R: 550 No such user here

            S: RCPT TO:<Brown@Beta.ARPA>
            R: 250 OK

            S: DATA
            R: 354 Start mail input; end with <CRLF>.<CRLF>
            S: Blah blah blah...
            S: ...etc. etc. etc.
            S: <CRLF>.<CRLF>
            R: 250 OK

|#

(defmethod (smtp-user :set-debug) (x)
  (setq *hash* x)
  (setq *trace* x)
  (setq *debug* x)
  (setq *verbose* x))


(defun make-smtp-user (&optional debug)
  (let ((u (make-instance 'smtp-user)))
    (send u :set-debug debug)
    u))

(defvar *smtp-user* nil)

(defun smtp-user ()
  "This is mainly used for debugging"
  (or *smtp-user* (setq *smtp-user* (make-smtp-user t)))
  (global:catch-error-restart ((error)
                        "Return to SMTP CMD command loop.")
    (do ((cmd))
        (nil)
      (setq cmd (global:prompt-and-read :read "~&SMTP ~S:" *smtp-user*))
      (cond ((stringp cmd)
             (send *smtp-user* :message-line cmd))
            ((atom cmd)
             (send *smtp-user* cmd))
            ('else
             (global:lexpr-send *smtp-user* cmd))))))

(defmethod (smtp-user :print-self) (stream &rest ignored)
  (si:printing-random-object (self stream :type)
    (format stream "~:[~;connected~] to ~A"
            *connected*
            *remote-hostname*)))

(defmethod (smtp-user :connect) (host)
  (hookup host "SMTP" "SMTP User Connection")
  (setq *connected* t))

(defmethod (smtp-user :quit) ()
  (cmd-close))

(defmethod (smtp-user :close) (&rest ignored)
  (unwind-protect
      (cmd-close)
    (and *control* (close *control*))))

(defmethod (smtp-user :quote) (x)
  (cmd-quote x))

(defmethod (smtp-user :command) (s &rest l)
  (apply #'command s l))

(defmethod (smtp-user :message-line) (x)
  (cond ((string-equal x ".")
         (princ ".." *control*))
        ('else
         (princ x *control*)))
  (send *control* :tyo (sym cr))
  (send *control* :tyo (sym lf)))


(defmethod (smtp-user :end-message) ()
  (send *control* :tyo (sym cr))
  (send *control* :tyo (sym lf))
  (send *control* :tyo #\.)
  (send *control* :tyo (sym cr))
  (send *control* :tyo (sym lf))
  (send *control* :force-output)
  (getreply nil))

(defun make-smtp-data-stream (lispm-stream &aux stream (bol t))
  (setq stream #'(lambda (op &optional arg1 &rest args)
                   (si:selectq-with-which-operations op
                     (:tyo
                       (cond ((= arg1 #\return)
                              (setq bol t))
                             ((and (= arg1 #\.) bol)
                              (send lispm-stream :tyo #\.)
                              (setq bol nil))
                             ('else
                              (setq bol nil)))
                       (send lispm-stream :tyo arg1))
                     (t
                       (global:stream-default-handler stream op arg1 args))))))

(defmethod (smtp-user :data-stream) (stream)
  (global:stream-copy-until-eof
    stream
    (make-smtp-data-stream (ftp:make-ascii-translating-output-stream *control*))))


(defmethod (smtp-user :funcall-on-data-stream) (f &rest l)
  (apply f
         (make-smtp-data-stream (ftp:make-ascii-translating-output-stream *control*))
         l))

(defmethod (smtp-user :last-reply) ()
  (last-reply))

(defvar *smtp-send-it-debug* nil)

(defun smtp-send-it (locf-to-plist interval template)
  "a function that can be the value of the variable ZWEI:*MAIL-SENDING-MODE*"
  template
  (zwei:canonicalize-headers locf-to-plist)
  (let ((recipients (zwei:get-mail-recipients locf-to-plist))
        (plist (cdr locf-to-plist))
        (possible-hosts)
        (winning-host))
    ;; the recipients will be a plist with important keys :NAME and :HOST
    ;; (:NAME "gjc" :HOST ("angel"))
    ;; The PLIST will have such items as
    ;; :FROM ((:PERSONAL-NAME "" :NAME "GJC" :HOST ("LMI-LAMBDA-3")))
    ;; :DATE 2695327073
    ;; :SUBJECT "foo"
    ;; Since we dont have any host table with server information
    ;; in it, just try hard to win.
    (dolist (r recipients)
      (let ((h (si:parse-host (car (getf r :host)) t)))
        (and h
             (send h :network-address :internet)
             (member h zwei:*mail-smtp-hosts* :test #'eq)
             (push h possible-hosts))))
    (setq possible-hosts (append zwei:*mail-smtp-hosts* possible-hosts))
    (or possible-hosts
        (error "no possible hosts to send to"))
    (let ((k 0))
      (dolist (w possible-hosts)
        (let ((m (count w possible-hosts)))
          (when (> m k)
            (setq k m)
            (setq winning-host w)))))
    (with-open-stream (u (make-smtp-user *smtp-send-it-debug*))
      (send u :connect (send winning-host :name))
      (or (= 250 (nth-value 1
                   (send u :command "HELO ~A" (send si:local-host :name))))
          (error "our host name ~S unacceptable because ~A"
                 (send si:local-host :name)
                 (send u :last-reply)))
      (or (= 250 (nth-value 1
                   (send u :command
                         "MAIL FROM:~A" (smtp-rcpt (car (getf plist :from))))))
          (error "can't accept mail from: ~S because ~A"
                 (car (getf plist :from))
                 (send u :last-reply)))
      (dolist (r recipients)
        (or (= 250 (nth-value 1
                     (send u :command
                           "RCPT TO:~A" (smtp-rcpt r))))
          (cerror "ignore this recipient"
                  "can't send mail to: ~S because ~A"
                  r
                  (send u :last-reply))))
      (or (= 354 (nth-value 1
                   (send u :command "DATA")))
          (error "can't send the mail text data because ~A"
                 (send u :last-reply)))
      (send u :funcall-on-data-stream
            #'zwei:output-header-and-msg
            locf-to-plist interval template)
      (or (= 250 (nth-value 1 (send u :end-message)))
          (error "some problem in ending message data: ~A"
                 (send u :last-reply))))))


(defun smtp-rcpt (r)
  (format nil "<~A~{@~A~}>" (getf r :name) (getf r :host)))


(compile-flavor-methods smtp-user)

;;;I moved MAKE-UNIX-ZMAIL-INIT-FILE and friends to ZWEI;MFHOST, since
;;;that is really a system-dependent ZMail support function, and has
;;;nothing to do with SMTP per-se. -KmC 5/5/88

;;; QSEND interface

(define-network-function (net:send-terminal-message :internet) (host person message-generator)
  (send-terminal-message host
                         (format nil "~A@~A" si:user-id (send si:local-host :name))
                         (format nil "~A@~A" person (send host :name))
                         message-generator))


(defun merror (format-string &rest arguments)
  "Returns an error object by calling ferror"
  (global:condition-case (obj)
      (apply #'global:ferror nil format-string arguments)
    (error obj)))


(defun send-terminal-message (host from to message-generator)
  (with-open-stream (u (make-smtp-user *smtp-send-it-debug*))
    (send u :connect (send host :name))
    (cond ((not (= 250 (nth-value 1  (send u :command "HELO ~A" (send si:local-host :name)))))
           (error "our host name ~S unacceptable because ~A"
                  (send si:local-host :name)
                  (send u :last-reply)))
          ((not (= 250 (nth-value 1  (send u :command "SEND FROM:<~A>" from))))
           (merror "cant accept message from: ~S because ~A"
                   from
                   (send u :last-reply)))
          ((not (= 250 (nth-value 1 (send u :command "RCPT TO:<~A>" to))))
           (merror "cant send message to: ~S because ~A" to (send u :last-reply)))
          ((not (= 354 (nth-value 1 (send u :command "DATA"))))
           (merror "cant send the terminal message text data because ~A"
                   (send u :last-reply)))
          ((progn (send u :funcall-on-data-stream (if (stringp message-generator)
                                                      #'(lambda (stream)
                                                          (format stream "~A" message-generator))
                                                    message-generator))
                  (not (= 250 (nth-value 1 (send u :end-message)))))
           (merror "some problem in ending message data: ~A" (send u :last-reply))))))

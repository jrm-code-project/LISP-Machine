;;; -*- Mode:LISP; Package:TELNET; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1985, 1987
   See filename "Copyright.Text" for
  licensing and release information.

  This was a little 3k character file before I added :rubout-handler capabilities.
  Still basically a one-day-hack though. 5/13/85 10:59:22 -George Carrette
  Well, i've moved the terminal stuff to another file.

|#

(define-network-service *tcp-telnet-service* :telnet :tcp "Network ASCII Terminal Capability"
  :listen-port (sym tcp-application:ipport-telnet)
  :toplevel-function 'telnet-server-function
  :auto-enable? t)

(defresource telnet-server (&optional ascii-output-stream ascii-input-stream)
  :constructor (make-instance 'telnet-server
                              :output ascii-output-stream
                              :input ascii-input-stream)
  :matcher (progn object)
  :initializer (progn (setf (global:symeval-in-instance object 'output) ascii-output-stream)
                      (setf (global:symeval-in-instance object 'input) ascii-input-stream)
                      (setf (global:symeval-in-instance object 'output-lock) nil)
                      (send object :termcap :default))
  :deinitializer (progn (setf (global:symeval-in-instance object 'output) nil)
                        (setf (global:symeval-in-instance object 'input) nil)
                        (setf (global:symeval-in-instance object 'telnet-options-received) nil)
                        (setf (global:symeval-in-instance object 'telnet-options-sent) nil)
                        (send object :termcap :default))
  :initial-copies 1)

(defvar network-user:*term* nil "The terminal type, as a keyword, eg. :H19")

(defvar *network-user-process-bindings*
        '((*package* (find-package "NETWORK-USER"))
          (*print-base* 10.)
          (base 10.)
          (*read-base* 10.)
          (ibase 10.)
          (*error-output* (make-synonym-stream '*terminal-io*)))
  "These are used in addition to *BREAK-BINDINGS*")

(defvar *telnet-interrupt-characters*
        `((,(glass-tty-ascii-code #\Control-g) network-user:handle-abort)
          (,(glass-tty-ascii-code #\Control-t) network-user:status-interrupt process-status-info)
          (,(glass-tty-ascii-code #\Control-z) network-user:handle-error-break)))

(defvar *telnet-ascii-quote-character* (glass-tty-ascii-code #\Control-V))
(defvar *telnet-quote-character* #\Control-V)
(defvar *telnet-ascii-stop-output-character* (glass-tty-ascii-code #\Control-S))
(defvar *telnet-ascii-resume-output-character* (glass-tty-ascii-code #\Control-Q))

(defvar *telnet-asynchronous-force-output-period* 60.)

(defun telnet-server-function (remote-stream)
  (let* ((safe-input-stream (make-eof-throwing-stream remote-stream))
         (si:user-id nil)
         (network-user:*term* nil)
         (cvars '(network-user:*term* si:user-id)))
    (global:using-resource (terminal telnet-server remote-stream safe-input-stream)
      (catch 'eof
        (send terminal :send-if-handles :send-initial-telnet-frobs)
        (format terminal "~%Welcome to ~A Server Telnet.~%" (send si:local-host :name))
        (send terminal :force-output)
        (network-user-login terminal)
        (send terminal :initialize-terminal)
        (global:print-herald terminal)
        (format terminal "~%Type (help) for keyboard help~%~%")
        (send terminal :force-output)
        (subprocess :closure-variables cvars
                    (loop
                      (send terminal :force-output)
                      (sys:process-sleep *telnet-asynchronous-force-output-period*)))
        (let ((buffer-stream (make-io-buffer-stream (global:symeval-in-instance terminal 'tv:io-buffer))))
          (send terminal :set-input-stream buffer-stream)
          (send terminal :set-more-p t)
          (send sys:current-process :set-priority 1)
          (catch 'telnet-server-logout
            (telnet-server-input (subprocess :closure-variables cvars
                                             :restart-after-reset t
                                             (global:progw (append *network-user-process-bindings* si:*break-bindings*)
                                               (catch 'telnet-server-logout
                                                 (si:lisp-top-level1 terminal))
                                               (send (network-server-process *server*)
                                                     :interrupt #'network-user:logout)))
                                 buffer-stream
                                 remote-stream
                                 terminal)))))))

(defun telnet-server-input (user-process buffer-stream remote-stream terminal)
  (do (c int quote extended)
      ((null (setq c (send remote-stream :tyi))))
    (setq extended (termcap.extended-keyboard (global:symeval-in-instance terminal 'termcap)))
    (cond ((= c (get 'iac 'telnet-sym))
           (let* ((c1 (send remote-stream :tyi))
                  (action (cadr (assoc c1 *telsyms* :test #'eq))))
             (cond ((eq action 'ip)
                    (send user-process :interrupt 'network-user:handle-abort-all))
                   ((eq action 'ao)
                    ;;Abort Output
                    )
                   ((eq action 'ayt)
                    (send user-process :interrupt 'network-user:status-interrupt (process-status-info user-process)))
                   (t
                    (send buffer-stream :tyo c)
                    (send buffer-stream :tyo c1)))))
          ((and extended (= c #o34))
           (let ((bits (send remote-stream :tyi)))
             (cond ((= bits #o034)
                    (send buffer-stream :tyo c))
                   (t
                    (setq c (make-char (global:char-flipcase (send remote-stream :tyi))
                                       (logand bits #o77)))
                    (case c
                      (#\Control-Abort
                       (send user-process :interrupt 'network-user:handle-abort))
                      (#\Control-Meta-Abort
                       (send user-process :interrupt 'network-user:handle-abort-all))
                      (#\Control-Break
                       (send user-process :interrupt 'network-user:handle-break))
                      (#\Control-Meta-Break
                       (send user-process :interrupt 'network-user:handle-error-break))
                      (otherwise
                       (send buffer-stream :tyo (char-int c))))))))
          ((and (not quote) (= c *telnet-ascii-quote-character*))
           (setq quote t))
          ((and (not quote) (= c *telnet-ascii-stop-output-character*))
           (unless (eq (global:symeval-in-instance terminal 'output-lock) global:current-process)
             (global:process-lock (locf (global:symeval-in-instance terminal 'output-lock)))))
          ((and (not quote) (= c *telnet-ascii-resume-output-character*))
           (when (eq (global:symeval-in-instance terminal 'output-lock) global:current-process)
             (global:process-unlock (locf (global:symeval-in-instance terminal 'output-lock)))))
          ((and (not quote) (setq int (assoc c *telnet-interrupt-characters* :test #'eq)))
           (cond ((third int)
                  (send user-process :interrupt
                        (second int)
                        (funcall (third int) user-process)))
                 (t
                  (send user-process :interrupt (second int)))))
          ((send buffer-stream :buffer-full-p)
           ;; GOOD QUESTION. LETS JUST THROW AWAY CHARACTERS, OTHERWISE
           ;; WE WILL MISS ANY #\CONTROL-G'S COMING DOWN.
           (send remote-stream :tyo (glass-tty-ascii-code #\Control-g)))
          (t
           (when quote
             (send buffer-stream :tyo *telnet-ascii-quote-character*)
             (setq quote nil))
           (send buffer-stream :tyo c)))))

(defun read-command-line (stream format &rest args)
  (let ((st (read-line stream t nil nil `((:prompt ,(apply #'format nil format args))))))
    (cond ((null st) nil)
          ((zerop (length st)) nil)
          (t st))))

(defun read-command-line-unechoed (stream format &rest args)
  (apply #'format stream format args)
  (do ((char (send stream :tyi) (send stream :tyi))
       (line (make-string 30 :fill-pointer 0)))
      ((null char) nil)
    (cond ((= char #\Rubout)
           (when (plusp (fill-pointer line))
             (vector-pop line)))
          ((= char #\Clear-Input)
           (setf (fill-pointer line) 0))
          ((= char #\Return)
           (fresh-line stream)
           (return (if (plusp (fill-pointer line)) line nil)))
          ((/= 0 (char-bits char)) (send stream :beep))
          (t
           (vector-push-extend char line)))))

(defvar *network-user-login-punt* 3)

(defun network-user-login (terminal &aux user pass)
  (do ((j 1 (1+ j)))
      (nil)
    (setq user (read-command-line terminal "Username: "))
    (setq pass (read-command-line-unechoed terminal "Password: "))
    (if (validate-network-server-password user pass si:local-host) (return nil))
    (format terminal "%ERROR: Invalid Username or Password~%")
    (when (and *network-user-login-punt* (>= j *network-user-login-punt*))
      (format terminal "Autologout after ~D tries~%" j)
      (throw 'eof nil)))
  (setq si:user-id user)
  (unless (member (global:symeval-in-instance terminal 'term) '(:supdup :supdup-output))
    (loop
      (setq network-user:*term* (read-command-line terminal "Terminal-type: "))
      (and (null network-user:*term*) (return nil))
      (setq network-user:*term* (intern (string-upcase network-user:*term*) ""))
      (and (get network-user:*term* 'termcap)
           (return (send terminal :termcap network-user:*term*)))
      (format terminal "~&Unknown terminal type: ~S (hit <RETURN> to punt)~%"
              network-user:*term*))))

(defun process-status-info (&optional (p sys:current-process))
  (list :hostname (send si:local-host :short-name)
        :state    (or (si:process-wait-whostate p)
                      (si:process-run-whostate p))
        :cpu-time (* (send p :cpu-time) 1.0e-6)
        :disk-wait-time  (* (send p :disk-wait-time) 1.0e-6)
        :page-faults (si:process-page-fault-count p)
        :percent-utilization (send p :percent-utilization)
        :current-time (time:get-universal-time)))

(define-network-service *tcp-supdup-service* :supdup :tcp "SUPDUP Terminal Capability"
  :listen-port (sym tcp-application:ipport-supdup)
  :toplevel-function 'supdup-server-function
  :auto-enable? t)

(defresource supdup-server (&optional ascii-output-stream ascii-input-stream)
  :constructor (make-instance 'supdup-server
                              :output ascii-output-stream
                              :input ascii-input-stream)
  :matcher (progn object)
  :initializer (progn (setf (global:symeval-in-instance object 'output) ascii-output-stream)
                      (setf (global:symeval-in-instance object 'input) ascii-input-stream)
                      (setf (global:symeval-in-instance object 'output-lock) nil)
                      (send object :termcap :default))
  :deinitializer (progn (setf (global:symeval-in-instance object 'output) nil)
                        (setf (global:symeval-in-instance object 'input) nil)
                        (send object :termcap :default))
  :initial-copies 1)

(defun supdup-server-function (remote-stream)
  (let* ((safe-input-stream (make-eof-throwing-stream remote-stream))
         (si:user-id nil)
         (network-user:*term* nil)
         (cvars '(network-user:*term* si:user-id)))
    (global:using-resource (terminal supdup-server remote-stream safe-input-stream)
      (catch 'eof
        (send terminal :supdup-greeting "Welcome to ~A SUPDUP Server." (send si:local-host :name))
        (send terminal :initialize-terminal)
        (network-user-login terminal)
        (send terminal :terpri)
        (global:print-herald terminal)
        (format terminal "~%Type (help) for keyboard help~%~%")
        (send terminal :force-output)
        (subprocess :closure-variables cvars
                    (loop
                      (send terminal :force-output)
                      (sys:process-sleep *telnet-asynchronous-force-output-period*)))
        (let ((buffer-stream (make-io-buffer-stream (global:symeval-in-instance terminal 'tv:io-buffer))))
          (send terminal :set-input-stream buffer-stream)
          (send terminal :set-more-p t)
          (send sys:current-process :set-priority 1)
          (catch 'telnet-server-logout
            (supdup-server-input (subprocess :closure-variables cvars
                                             :restart-after-reset t
                                             (global:progw (append *network-user-process-bindings* si:*break-bindings*)
                                               (catch 'telnet-server-logout
                                                 (si:lisp-top-level1 terminal))
                                               (send (network-server-process *server*)
                                                     :interrupt #'network-user:logout)))
                                 buffer-stream
                                 remote-stream
                                 terminal)))))))

(defun supdup-server-input (user-process buffer-stream remote-stream terminal)
  (declare (ignore terminal))
  (do (c bits code)
      ((null (setq c (send remote-stream :tyi))))
    (when (= c #o34)
      (setq bits (send remote-stream :tyi))
      (cond ((null bits)
             (setq c nil))
            ((= bits #o34)
             (setq c (int-char c)))
            (t
             (setq bits (logand bits #o37))
             (setq code (send remote-stream :tyi))
             (cond ((null code)
                    (setq c nil))
                   (t
                    (setq c (dpb (logand bits #o20) (byte 5 7) (logand #o177 code)))
                    (setq c (make-char (cond ((cadr (assoc c *ascii-supdup-translations* :test #'eq)))
                                             ((= bits #o20) code)
                                             (t (global:char-flipcase code)))
                                       (logand bits #o17))))))))
    (cond ((null c)                             ;End Of File
           (send buffer-stream :tyo nil))
          ((eq c #\Control-Abort)
           (send user-process :interrupt 'network-user:handle-abort))
          ((eq c #\Control-Meta-Abort)
           (send user-process :interrupt 'network-user:handle-abort-all))
          ((eq c #\Control-Break)
           (send user-process :interrupt 'network-user:handle-break))
          ((eq c #\Control-Meta-Break)
           (send user-process :interrupt 'network-user:handle-error-break))
          ((integerp c)                         ;Not prefixed by #o34 -- pass through
           (send buffer-stream :tyo c))
          ((graphic-char-p c)                   ;Special Printing character -- pass through
           (send buffer-stream :tyo c))
          (t
           (send buffer-stream :tyo (char-int c))))))

(add-initialization "SUPDUP"
                    '(process-run-function "SUPDUP Server" 'chaos-supdup-server)
                    NIL
                    'chaos:server-alist)

(defun chaos-supdup-server ()
  (let* ((conn (chaos:listen "SUPDUP"))
         (process-name (format nil "SUPDUP serving ~A" (chaos:host-short-name (chaos:foreign-address conn))))
         (tcpa:*server* (tcpa:make-network-server :service (tcpa:make-network-service :name "SUPDUP")
                                                  :process global:current-process
                                                  :process-name process-name))
         (stream nil))
    (global:condition-case-if (not tcpa:*tcp-generic-server-toplevel-debug*) ()
        (unwind-protect
            (progn
              (chaos:accept conn)
              (setq stream (chaos:make-stream conn))
              (send tv:who-line-file-state-sheet :add-server conn "SUPDUP")
              (supdup-server-function stream))
          (and stream (send stream :force-output))
          (chaos:close-conn conn)
          (dolist (proc (tcpa:network-server-subprocesses *server*))
            (send proc :kill))
          (send tv:who-line-file-state-sheet :delete-server conn))
      (error nil))))

(defun make-io-buffer-stream (buffer &aux stream)
  (declare (values stream buffer))
  (setq stream #'(lambda (op &optional arg1 &rest args)
                   (si:selectq-with-which-operations op
                     (:tyi
                       (tv:io-buffer-get buffer))
                     (:tyo
                       (apply 'tv:io-buffer-put buffer arg1 args))
                     (:listen
                       (not (tv:io-buffer-empty-p buffer)))
                     (:clear-input
                       (tv:io-buffer-clear buffer))
                     (:buffer-full-p
                       (tv:io-buffer-full-p buffer))
                     (t
                       ;; no other operations are expected but might as well
                       ;; allow for them.
                       (global:stream-default-handler stream op arg1 args)))))
  (values stream buffer))

(defun make-eof-throwing-stream (substream &aux stream)
  (setq stream #'(lambda (op &optional arg1 &rest args)
                   (si:selectq-with-which-operations op
                     (:tyo
                       (send substream :tyo arg1))
                     (:tyi
                       (or (send substream :tyi)
                           (throw 'eof nil)))
                     (:listen
                       (send substream :listen))
                     (:force-output
                       (send substream :force-output))
                     (:clear-output
                       (send substream :clear-output))
                     (:clear-input
                       (send substream :clear-input))
                     (t
                       ;; no other operations are expected but might as well
                       ;; allow for them.
                       (global:stream-default-handler stream op arg1 args))))))

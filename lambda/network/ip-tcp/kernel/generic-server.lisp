;;; -*- Mode:LISP; Package:TCP-APPLICATION; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '(define-network-service
          *server*
          subprocess
          kill-subprocess
          network-server-process
          validate-network-server-password
          ))

#|
Note: One of the things we are trying to be careful about here is our
      process book-keeping. Processes are started up and always recorded
      someplace, on a plist, in a variable, or in a structure slot.
      We always try to :KILL processes which are wedged and useless,
      although we have to be careful (by setting the place the process
      is kept track of in back to NIL when done) not to :KILL a process structure that
      has already been killed. The normal exit of a process or kill of it puts it on the list
      SI:PROCESS-RUN-FUNCTION-SPARE-PROCESSES, which would alow it to be possibly reused again
      by some other call to PROCESS-RUN-FUNCTION, before our own cleanup code would
      get a chance to run.
|#

;;;***A Network Service describes one TCP or UDP service and keeps track of who is listening
;;;for it, which processes are actively handling connections, etc.

(defstruct (network-service
             (:print-function (lambda (network-service stream ignore)
                                (sys:printing-random-object
                                  (network-service stream :type :no-pointer)
                                  (format stream "~A (~A port ~D)"
                                       (network-service-name network-service)
                                       (network-service-transport-protocol network-service)
                                       (network-service-listen-port network-service))))))
  name                                          ;Name of the network service
  transport-protocol                            ;:TCP or :UDP
  listen-port                                   ;The port to listen on
  toplevel-function                             ;Function to call when connection opens
  listening-server                              ;If currently listening, the network-server structure
  active-servers                                ;List of servers currently open for this service
  auto-enable?                                  ;T if should be enabled when system starts
  stream-flavor                                 ;Flavor of stream to make for this connection
  lock                                          ;Interlock on fields of this structure
  )

(defmacro with-network-service-lock ((service) &body body)
  (declare (zwei:indentation 1 1))
  `(with-lock ((network-service-lock ,service) :norecursive t :whostate "TCP Service Lock")
     ,@body))

;;;***A Network Server is one incarnation of a Network Service.
(defstruct (network-server
             (:print-function (lambda (network-server stream ignore)
                                (sys:printing-random-object
                                  (network-server stream :type :no-pointer)
                                  (format stream "~A ~A"
                                  (if (network-server-service network-server)
                                              (network-service-name (network-server-service network-server))
                                            nil)
                                      (network-server-process-name network-server))))))
  service                                       ;Back-pointer to service this is an instance of
  socket                                        ;The tcp-socket or udp-socket
  stream                                        ;If tcp, the tcp-stream flavor instance
  client-name                                   ;The host name of the remote host
  process                                       ;The process forked to handle this connection
  process-name                                  ;The process name of the process, if any
  subprocesses                                  ;List of subprocesses forked by server process
  lock                                          ;Interlock on fields of this structure
  )

(defmacro with-network-server-lock ((server) &body body)
  (declare (zwei:indentation 1 1))
  `(with-lock ((network-server-lock ,server) :norecursive t :whostate "TCP Server Lock")
     ,@body))

(defun allocate-network-server (service)
  (make-network-server :service service))

(defun deallocate-network-server (server)
  (with-network-server-lock (server)
    (setf (network-server-service server) nil)
    (when (network-server-socket server)
      (send (network-server-socket server) :close)
      (setf (network-server-socket server) nil))
    (setf (network-server-stream server) nil)
    (setf (network-server-client-name server) nil)
    (setf (network-server-process-name server) "inactive")
    (when (network-server-subprocesses server)
      (dolist (proc (network-server-subprocesses server))
        (send proc :kill))
      (setf (network-server-subprocesses server) nil))
    ))

(defvar *network-services* nil "A list of names of generic servers to enable")

(defmacro define-network-service (name protocol-name transport-protocol documentation &body keywords)
  "Causes the system to create a socket to listen on LISTEN-PORT.
TRANSPORT-PROTOCOL may be either :TCP or :UDP for stream and datagram oriented services respectively.
When somebody connects to this port a process is created in which the TOPLEVEL-FUNCTION is called
on a stream connected to the active socket.
The stream and socket are automatically closed when the function exits.
If AUTO-ENABLE? is non-NIL, this server will enabled by calls to (enable-all-network-services),
which is run whenever TCP is initialized."
  (declare (arglist name protocol-name transport-protocol documentation
                    &key toplevel-function listen-port auto-enable? stream-flavor))
  `(*define-network-service ',name ',protocol-name ',transport-protocol ',documentation ,@keywords))

(defun *define-network-service (name protocol-name transport-protocol documentation
                                &key toplevel-function listen-port auto-enable? stream-flavor)
  (when (global:record-source-file-name name 'define-network-service)
    (proclaim `(special ,name))
    (setf (documentation name 'network-service) (or documentation name))
    (let ((service (set name (make-network-service))))
      (pushnew name *network-services*)
      (setf (network-service-name service) protocol-name)
      (setf (network-service-transport-protocol service) transport-protocol)
      (setf (network-service-listen-port service) (eval listen-port))
      (setf (network-service-toplevel-function service) toplevel-function)
      (setf (network-service-auto-enable? service) auto-enable?)
      (setf (network-service-stream-flavor service) stream-flavor)
      (setf (network-service-active-servers service) nil)
      (setf (network-service-listening-server service) nil)
      (setf (network-service-lock service) nil))
    name))

;;;; >>>> Initialization stuff <<<<

(defun enable-one-network-service (service)
  (unless (network-service-listening-server service)
    (listen-on-network-service-port service)))

(defun enable-all-network-services (&optional also-do-non-auto-enable?)
  (dolist (symbol *network-services*)
    (let ((service (symbol-value symbol)))
      (when (or also-do-non-auto-enable? (network-service-auto-enable? service))
        (enable-one-network-service service)))))

(defun disable-one-network-service (service)
  (with-network-service-lock (service)
    (when (network-service-listening-server service)
      (deallocate-network-server (network-service-listening-server service))
      (setf (network-service-listening-server service) nil))
    (dolist (server (network-service-active-servers service))
      (deallocate-network-server server))
    (setf (network-service-active-servers service) nil)))

(defun disable-all-network-services ()
  (dolist (service *network-services*)
    (disable-one-network-service (symbol-value service))))

(defun reset-network-servers (&optional (enablep t))
  (disable-all-network-services)
  (when enablep
    (enable-all-network-services)))

(defun listen-on-network-service-port (service)
  (with-network-service-lock (service)
    (unless (network-service-listening-server service)
      (let ((server (allocate-network-server service))
            (socket (funcall (ecase (network-service-transport-protocol service)
                               (:tcp 'tcp:make-tcp-socket)
                               (:udp 'udp:make-udp-socket))
                             :keyword
                             (string-append (string (network-service-name service))
                                            " Server"))))
        (setf (network-server-socket server) socket)
        (setf (network-server-process-name server) "listening")
        (cond ((send socket :open :local-port (network-service-listen-port service))
               (setf (network-service-listening-server service) server)
               (when (eq :udp (network-service-transport-protocol service))
                 (send socket :receive)))
              (t
               ;;(cerror "continue" "Can't open socket for service: ~A" service)
               (setf (network-server-socket server) nil)        ;socket not open
               (deallocate-network-server server)))))))

;;;***The process that listens for TCP/UDP connections

(defvar *tcp-server-process* (make-process "TCP/UDP Server listener"
                                           :warm-boot-action 'ignore
                                           :arrest-reasons '(:not-running)))

(defun initialize-tcp-server-process ()
  (send *tcp-server-process* :preset 'tcp-server-process)
  (send *tcp-server-process* :reset)
  (send *tcp-server-process* :run-reason :enable)
  (send *tcp-server-process* :revoke-arrest-reason :not-running))

(defun tcp-server-process ()
  (loop                                         ;do-forever
    (let ((service nil)                         ;the service whose LISTEN succeeded
          (server nil))                         ;the server handling the LISTEN
      ;;Wait for a connection to any server
      (process-wait "Await Connection"
                    #'(lambda ()
                        (or service
                            (dolist (symbol *network-services*)
                              (let* ((server (network-service-listening-server (symbol-value symbol)))
                                     (socket (when server (network-server-socket server))))
                                (and socket
                                     (send socket :listen)
                                     (return (setq service (symbol-value symbol)))))))))

      ;;We got one -- get the server and indicate no longer listening
      (with-network-service-lock (service)
        (setq server (network-service-listening-server service))
        (setf (network-service-listening-server service) nil))

      ;;If it's UDP, turn generic connection into specific connection
      (when (eq :udp (network-service-transport-protocol service))
        (send (network-server-socket server) :bind))

      ;;And hang out another listen.
      (listen-on-network-service-port service)

      ;;Check whether this connection is acceptable
      (cond ((null (send (network-server-socket server) :remote-address))
             ;;Failed open
             (deallocate-network-server server))
            ((unwanted-connection-rejected-p (network-service-transport-protocol service)
                                             (network-server-socket server))
             ;;Administrative rejection
             (deallocate-network-server server))
            (t
             ;;Acceptable -- fork off a process to handle it
             (fork-network-process service server))))))

(global:define-site-variable *unwanted-connection-p* :unwanted-internet-connection-p
  "A function called with transport protocol (:TCP or :UDP) and socket (which
supports :local-port, :remote-port, and :remote-address operations)
If it returns T then the connection will be aborted.")

(defun unwanted-connection-rejected-p (transport-protocol socket)
  (when (and *unwanted-connection-p*
             (funcall *unwanted-connection-p* transport-protocol socket))
    (send socket :close :abort)
    t))

(defun fork-network-process (service server)
  (with-network-server-lock (server)
    (let ((process-name (format nil "~A serving ~A port ~D"
                                (network-service-name service)
                                (network-service-transport-protocol service)
                                (network-service-listen-port service)))
          (inet-addr (send (network-server-socket server) :remote-address)))
      (setf (network-server-client-name server)
            (unless (zerop inet-addr)
              (si:get-host-from-address inet-addr :internet)))
      (setf (network-server-process-name server)
            (format nil "~A serving ~A,~D"
                    (network-service-name service)
                    (cond ((network-server-client-name server)
                           (send (network-server-client-name server) :name))
                          ((not (zerop inet-addr))
                           (canonical-ip inet-addr))
                          (t
                           :unspecified))
                    (send (network-server-socket server) :remote-port)))
      (setf (network-server-process server)
            (process-run-function process-name
                                  'start-network-server-toplevel-function
                                  service
                                  server)))))

(defvar *server* :unbound "Bound to the NETWORK-SERVER when serving")
(defvar *tcp-generic-server-toplevel-debug* nil
  "Set to T if debugging a server -- errors aren't caught and flushed.")

(defun start-network-server-toplevel-function (service server)
  (let ((*server* server)
        (flavor (or (network-service-stream-flavor service)
                    (ecase (network-service-transport-protocol service)
                      (:tcp 'tcp:tcp-buffered-stream)
                      (:udp 'udp:udp-stream)))))
    (with-open-stream (stream (send (make-instance flavor :socket (network-server-socket server)) :open))
      (with-network-server-lock (server)
        (setf (network-server-stream server) stream))
      (global:condition-case-if (not *tcp-generic-server-toplevel-debug*) ()
          (unwind-protect
              (progn
                (send tv:who-line-file-state-sheet :add-server stream (network-service-name service))
                (with-network-service-lock (service)
                  (push server (network-service-active-servers service)))
                (funcall (network-service-toplevel-function service) stream))
            (with-network-service-lock (service)
              (setf (network-service-active-servers service)
                    (remove server (network-service-active-servers service))))
            (send stream :force-output)
            (send tv:who-line-file-state-sheet :delete-server stream)
            (deallocate-network-server server))
        (error nil)))))

;;;; >>>> Subprocess tracking <<<<

(defmacro subprocess (&body forms)
  "Execute the FORMS in the context of a subprocess"
  (declare (arglist &optional &key :closure-variables &body forms))
  (do ((keyword-arguments nil))
      ((not (keywordp (car forms)))
       `(*subprocess #'(lambda () ,@forms) ,@keyword-arguments))
    (setq keyword-arguments (append keyword-arguments
                                    (list (pop forms) (pop forms))))))

(defvar *subprocess-closure-variables*
        '(*server* *terminal-io* *standard-output* *standard-input* *query-io* *error-output* *package*))

(defun *subprocess (function &optional &key closure-variables restart-after-reset &allow-other-keys)
  "Does a PROCESS-RUN-FUNCTION but keeps track of the process with the NETWORK-SERVER so that
it will get killed at the right time, etc."
  (process-run-function (list :name (format nil
                                            "Sub#~D: ~A"
                                            (1+ (length (network-server-subprocesses *server*)))
                                            (network-server-process-name *server*))
                              :restart-after-reset restart-after-reset)
                        (global:closure (append closure-variables
                                                *subprocess-closure-variables*)
                                        #'tcp-generic-server-subprocess-function)
                        function))

(defun tcp-generic-server-subprocess-function (f)
  (unwind-protect
      (progn
        (with-network-server-lock (*server*)
          (push sys:current-process (network-server-subprocesses *server*)))
        (global:condition-case-if (not *tcp-generic-server-toplevel-debug*) ()
            (funcall f)
          (tcp::tcp-error
            (tcp-generic-server-subprocess-error))
          (tcp::exos-error
            (tcp-generic-server-subprocess-error))))
    (with-network-server-lock (*server*)
      (setf (network-server-subprocesses *server*)
            (remove sys:current-process (network-server-subprocesses *server*))))))

(global:defsignal subprocess-error error ())

(defun superior-process ()
  (network-server-process *server*))

(defun tcp-generic-server-subprocess-error ()
  (send (superior-process)
        :interrupt
        #'(lambda (x)
            (global:signal-condition (global:make-condition 'subprocess-error "~S got an error" x)))
        sys:current-process)
  (process-wait "to die" #'false))

(defun kill-subprocess (p)
  (with-network-server-lock (*server*)
    (when (member p (network-server-subprocesses *server*) :test #'eq)
      (send p :kill)
      (setf (network-server-subprocesses *server*)
            (remove p (network-server-subprocesses *server*))))))

;;;; >>>> passwords <<<<

(defvar *failed-login-notification-p* nil)

(defun validate-network-server-password (username password to-host)
  "Return T if legal to login on TO-HOST with username and password given"
  (let ((dbase (si:get-site-option :server-password-database))
        (from-host (network-server-client))
        (using-service (and (boundp '*server*)
                            (network-service-name (network-server-service *server*)))))
    (if dbase
        (with-open-stream (ds (let ((si:user-id "SYSTEM"))
                                (open dbase :error nil)))
          (unless (global:errorp ds)
            (do ((item (list (string username)
                             (string password)
                             (and to-host (send to-host :name))
                             (and from-host (if (numberp from-host)
                                                from-host
                                              (send from-host :name)))
                             (and using-service (string using-service))))
                 (pattern))
                ((null (setq pattern (read ds nil)))
                 (when *failed-login-notification-p*
                   (tv:notify nil "FAILED LOGIN: ~S" item))
                 nil)
              (if (network-server-password-match item pattern)
                  (return t)))))
      t)))

(defun network-server-password-match (item pattern)
  "Patterns in the password database are (<username> <password> <to-host> <from-host> <service>)"
  (labels ((amatch (a b)
            (cond ((member b '(* nil) :test #'eq) t)
                  ((not a) t)
                  ((equalp a b) t)
                  ((and (or (symbolp a) (stringp a))
                        (or (symbolp b) (stringp b)))
                   (string-equal a b)))))
    (dotimes (j 5)
      (or (amatch (nth j item) (nth j pattern))
          (return-from network-server-password-match nil)))
    t))

(defun network-server-client ()
  (and (boundp '*server*)
       (or (network-server-client-name *server*)
           (and (network-server-socket *server*)
                (send (network-server-socket *server*) :remote-address)))))

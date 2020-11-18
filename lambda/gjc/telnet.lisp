#| -*- Mode:LISP; Package:TCP-APPLICATION; Base:10 -*-

  Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
  licensing and release information.

  This was a little 3k character file before I added :rubout-handler capabilities.
  Still basically a one-day-hack though. 5/13/85 10:59:22 -George Carrette
  Well, i've moved the terminal stuff to another file.

|#

(DEFINE-TCP-SERVER TELNET "Network ASCII Terminal Capability"
  :LISTEN-PORT (TCP:SYM TCP:IPPORT-TELNET)
  :TOPLEVEL-FUNCTION 'TELNET-SERVER-FUNCTION
  :STREAM-GENERIC-P T
  )

(DEFVAR TELNET-USER:*TERM* NIL "The terminal type, as a keyword, eg. :H19")

(DEFVAR *TELNET-USER-PROCESS-BINDINGS*
        '((*PACKAGE* (FIND-PACKAGE "TELNET-USER"))
          (BASE 10)
          (IBASE 10)
          (*ERROR-OUTPUT* (make-synonym-stream '*terminal-io*)))
  "These are used in addition to *BREAK-BINDINGS*")


(DEFUN TELNET-SERVER-FUNCTION (REMOTE-STREAM)
  (LET* ((SAFE-INPUT-STREAM (MAKE-EOF-THROWING-STREAM REMOTE-STREAM))
         (TERMINAL (MAKE-STREAM-TERMINAL NIL REMOTE-STREAM SAFE-INPUT-STREAM)))
    (*CATCH 'EOF
      (LET ((HOST (TELNET-ANNOUNCE-AND-PROMPT TERMINAL)))
        (COND ((EQ HOST SI:LOCAL-HOST)
               (TELNET-SERVER-LOCAL REMOTE-STREAM TERMINAL))
              ('ELSE
               (TELNET-SERVER-REMOTE HOST REMOTE-STREAM)))))))


(DEFUN TELNET-SERVER-REMOTE (HOST REMOTE-STREAM)
  (CONDITION-CASE ()
      (WITH-OPEN-STREAM (HOST-STREAM (CHAOS:OPEN-STREAM HOST "TELNET"))
        (SUBPROCESS (STREAM-COPY-AND-FORCE-OUTPUT HOST-STREAM REMOTE-STREAM))
        (STREAM-COPY-AND-FORCE-OUTPUT REMOTE-STREAM HOST-STREAM))
    (SI:REMOTE-NETWORK-ERROR NIL)))


(DEFUN STREAM-COPY-AND-FORCE-OUTPUT (FROM-STREAM TO-STREAM)
  (DO ((BUF) (OFFSET) (LIMIT))
      (())
    (MULTIPLE-VALUE (BUF OFFSET LIMIT)
      (SEND FROM-STREAM :READ-INPUT-BUFFER))
    (COND ((NULL BUF) (RETURN NIL)))
    (SEND TO-STREAM :STRING-OUT BUF OFFSET LIMIT)
    (SEND TO-STREAM :FORCE-OUTPUT)
    (SEND FROM-STREAM :ADVANCE-INPUT-BUFFER)))


(DEFVAR *TELNET-INTERRUPT-CHARACTERS*
        `#(,(GLASS-TTY-ASCII-CODE #\CONTROL-G) TELNET-USER:ABORTION-INTERRUPT)
          (,(GLASS-TTY-ASCII-CODE #\CONTROL-T) TELNET-USER:STATUS-INTERRUPT PROCESS-STATUS-INFO)
          (,(GLASS-TTY-ASCII-CODE #\CONTROL-B) TELNET-USER:BREAK-INTERRUPT)))

(DEFVAR *TELNET-ASYNCRONOUS-FORCE-OUTPUT-PERIOD* 60)

(DEFUN TELNET-SERVER-LOCAL (REMOTE-STREAM TERMINAL)
  (LET ((CVARS '(TELNET-USER:*TERM* SI:USER-ID)))
    (MULTIPLE-VALUE-BIND (BUFFER-STREAM BUFFER)
        (MAKE-SIMPLE-IO-BUFFER-STREAM)
      (LET ((SI:USER-ID NIL)
            (TELNET-USER:*TERM* NIL))
        (TELNET-USER-LOGIN TERMINAL)
        (PRINT-HERALD TERMINAL)
        (TERPRI TERMINAL)
        (SEND TERMINAL :FORCE-OUTPUT)
        (SUBPROCESS
          :CLOSURE-VARIABLES CVARS
          (DO-FOREVER
            (SEND TERMINAL :ASYNCRONOUS-FORCE-OUTPUT)
            (PROCESS-SLEEP *TELNET-ASYNCRONOUS-FORCE-OUTPUT-PERIOD*)))
        (SEND TERMINAL :SET-INPUT-STREAM BUFFER-STREAM)
        (SEND TERMINAL :SET-MORE-P T)
        (SEND CURRENT-PROCESS :SET-PRIORITY 1)
        (*CATCH 'TELNET-SERVER-LOGOUT
          (DO ((C)(INT)
               (USER-PROCESS (SUBPROCESS
                               :CLOSURE-VARIABLES CVARS
                               (PROGW (APPEND *TELNET-USER-PROCESS-BINDINGS*
                                              SI:*BREAK-BINDINGS*)
                                 (*CATCH 'TELNET-SERVER-LOGOUT
                                   (TELNET-LISP-TOP-LEVEL TERMINAL))
                                 (SEND (TCP-SERVER-PROCESS *SERVER*)
                                       :INTERRUPT #'TELNET-USER:LOGOUT)))))
              ((NULL (SETQ C (SEND REMOTE-STREAM :TYI))))
            (COND ((SETQ INT (ASSQ C *TELNET-INTERRUPT-CHARACTERS*))
                   (COND ((CADDR INT)
                          (SEND USER-PROCESS :INTERRUPT (CADR INT)
                                (FUNCALL (CADDR INT) USER-PROCESS)))
                         ('ELSE
                          (SEND USER-PROCESS :INTERRUPT (CADR INT)))))
                  ((TCP:SIMPLE-IO-BUFFER-FULL-P BUFFER)
                   ;; GOOD QUESTION. LETS JUST THROW AWAY CHARACTERS, OTHERWISE
                   ;; WE WILL MISS ANY #\CONTROL-G'S COMING DOWN.
                   (SEND REMOTE-STREAM :TYO (GLASS-TTY-ASCII-CODE #\CONTROL-G)))
                  ('ELSE
                   (TCP:SIMPLE-IO-BUFFER-PUT BUFFER C)))))))))


(DEFUN READ-COMMAND-LINE (STREAM FORMAT &REST ARGS)
  (APPLY #'FORMAT STREAM FORMAT ARGS)
  (LET ((ST (READLINE STREAM)))
    (COND (#NULL ST) NIL)
          ((ZEROP (LENGTH ST)) NIL)
          ('ELSE ST))))

(DEFVAR *TELNET-USER-LOGIN-PUNT* 3)

(DEFUN TELNET-USER-LOGIN (TERMINAL &AUX USER PASS)
  (DO ((J 1 (1+ J)))
      (NIL)
    (DO ()
        ((SETQ USER (READ-COMMAND-LINE TERMINAL "~&Username: "))))
    (SEND TERMINAL :SET-ECHO-P NIL)
    (SETQ PASS (READ-COMMAND-LINE TERMINAL "~&Password: "))
    (SEND TERMINAL :SET-ECHO-P T)
    (IF (VALIDATE-TCP-SERVER-PASSWORD USER PASS SI:LOCAL-HOST) (RETURN NIL))
    (FORMAT TERMINAL "%ERROR: Invalid Username or Password~%")
    (WHEN (AND *TELNET-USER-LOGIN-PUNT* (>= J *TELNET-USER-LOGIN-PUNT*))
      (FORMAT TERMINAL "Autologout after ~D tries~%" J)
      (*THROW 'EOF NIL)))
  (SETQ SI:USER-ID USER)
  (DO ()
      (NIL)
    (SETQ TELNET-USER:*TERM* (READ-COMMAND-LINE TERMINAL "~&Terminal-type> "))
    (AND (NULL TELNET-USER:*TERM*) (RETURN NIL))
    (SETQ TELNET-USER:*TERM* (INTERN (string-upcase TELNET-USER:*TERM*) ""))
    (AND (GET TELNET-USER:*TERM* 'TERMCAP)
         (RETURN (SEND TERMINAL :TERMCAP telnet-user:*term*)))
    (FORMAT TERMINAL "~&Unknown terminal type: ~S (hit <RETURN> to punt)~%"
            TELNET-USER:*TERM*)))

(DEFUN PROCESS-STATUS-INFO (&OPTIONAL (p current-process))
  (LIST :HOSTNAME (send si:local-host :short-name)
        :STATE    (OR (si:process-wait-whostate p)
                      (si:process-run-whostate p))
        :CPU-TIME (times (send p :cpu-time) 1.0E-6)
        :DISK-WAIT-TIME  (times (send p :disk-wait-time) 1.0E-6)
        :PAGE-FAULTS (si:process-page-fault-count p)
        :PERCENT-UTILIZATION (send p :percent-utilization)
        :CURRENT-TIME (TIME:GET-UNIVERSAL-TIME)))

(DEFVAR *TELNET-SERVER-DEBUG* NIL "If T then dont call LISP-TOP-LEVEL1")

(DEFUN TELNET-LISP-TOP-LEVEL (X)
  (COND ((NOT *TELNET-SERVER-DEBUG*)
         ;; THIS WILL BIND *TERMINAL-IO*
         (SI:LISP-TOP-LEVEL1 X))
        ('ELSE
         (DOTIMES (J 10)
           (send x :tyo (send x :tyi)))
         (DO ()
             (nil)
           (PRINT (EVAL (LET ((QUERY-IO X)) (PROMPT-AND-READ :READ "~&Input> ")))
                  X
                  )))))


(DEFUN BUS-LOCAL-HOSTS ()
  (CONS SI:LOCAL-HOS#
        (DELQ NIL (MAPCAR #'(LAMBDA (X) (SI:GET-HOST-FROM-ADDRESS
                                          (SI:%PROCESSOR-CONF-CHAOS-ADDRESS
                                            (SI:OP-PROC-CONF X))
                                          :CHAOS))
                          SI:*OTHER-PROCESSORS*))))



(DEFUN TELNET-ANNOUNCE-AND-PROMPT (TERMINAL)
  (SEND TERMINAL :SEND-IF-HANDLES :SEND-INITIAL-TELNET-FROBS)
  (SEND TERMINAL :FORCE-OUTPUT)
  (FORMAT TERMINAL "~%~A telnet server version ~D.~D. <RETURN> to procede, ? for menu"
           (send si:local-host :name)
           (nth-value 0 (si:get-system-version 'tcp-user))
           (nth-value 1 (si:get-system-version 'tcp-user)))
  (SEND TERMINAL :FORCE-OUTPUT)
  (DO ((H (BUS-LOCAL-HOSTS))
       (C))
      (NIL)
    (SETQ C (SEND TERMINAL :TYI))
    (SELECTQ C
      (#\SPACE)
      (#\?
       (FORMAT TERMINAL "~%Menu of processors available:~%")
       (DO ((J 1 (1+ J))
            (l h (cdr l)))
           ((null l))
         (format TERMINAL "~D - ~A a ~A type host~%"
                 J (send (car L) :name) (send (car L) :system-type)))
       (format TERMINAL "Select a host by typing an integer: ")
       (setq c (send TERMINAL :tyi))
       (send TERMINAL :tyo c)
       (TERPRI TERMINAL)
       (let ((n (digit-char-p c)))
         (COND ((and n (> n 0) (nth (1- n) h))
                (format TERMINAL "Connecting to ~A~%" (send (nth (1- n) h) :name))
                (SEND TERMINAL :FORCE-OUTPUT)
                (return (nth (1- n) h)))
               ('ELSE
                (FORMAT TERMINAL "invalid choice. <RETURN> to procede, ? for menu" C)))))
      (#\return
       (return si:local-host))
      (t
       (format TERMINAL "~%Unknown command character <RETURN> to procede, ? for menu")))))



(DEFUN MAKE-SIMPLE-IO-BUFFER-STREAM ()
  (DECLARE (VALUES STREAM BUFFER))
  (LET ((BUFFER (TCP:MAKE-SIMPLE-IO-BUFFER))
        (STREAM))
    (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                     (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                       (:TYI
                         (TCP:SIMPLE-IO-BUFFER-GET BUFFER))
                       (:LISTEN
                         (NOT (ZEROP (TCP:SIMPLE-IO-BUFFER-CHARS-AVAILABLE BUFFER))))
                       (:CLEAR-INPUT
                         (TCP:SIMPLE-IO-BUFFER-CLEAR BUFFER))
                       (T
                        #;; no other operations are expected but might as well
                         ;; allow for them.
                         (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS)))))
    (VALUES STREAM BUFFER)))


(DEFUN MAKE-EOF-THROWING-STREAM (SUBSTREAM &AUX STREAM)
  (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                   (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                     (:TYO
                       (SEND SUBSTREAM :TYO ARG1))
                     (:TYI
                       (OR (SEND SUBSTREAM :TYI)
                           (*THROW 'EOF NIL)))
                     (:LISTEN
                       (SEND SUBSTREAM :LISTEN))
                     (:FORCE-OUTPUT
                       (SEND SUBSTREAM :FORCE-OUTPUT))
                     (:CLEAR-OUTPUT
                       (SEND SUBSTREAM :CLEAR-OUTPUT))
                     (:CLEAR-INPUT
                       (SEND SUBSTREAM :CLEAR-INPUT))
                     (T
                       ;; no other operations are expected but might as well
                       ;; allow for them.
                       (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS))))))

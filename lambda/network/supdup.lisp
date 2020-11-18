;;; Windows that hack the network -*- Mode:LISP; Package:SUPDUP; Base:8; Readtable:ZL -*-

;;; "Connect to:" may be answered by hostname or hostname/contactname.

(DEFFLAVOR BASIC-NVT
           ((ESCAPE-CHAR #/NETWORK)     ;Escape character (in Lisp machine character set)
            (CONNECTION NIL)            ;The connection itself
            (CONNECT-TO NIL)            ;Host to connect to (for TYPEIN-TOP-LEVEL)
            STREAM                      ;A stream to the above
            (TERMINAL-STREAM NIL)       ;Stream for output. If NIL, (which is the usual case)
                                        ; output to SELF.
            (OUTPUT-BUFFER (MAKE-STRING #o200 :FILL-POINTER 0))
            TYPEOUT-PROCESS             ;Network to screen
            TYPEIN-PROCESS              ;Keyboard to network
            (OUTPUT-LOCK NIL)           ;Some typeout occurs in TYPEIN-PROCESS
            (RETURN-TO-CALLER NIL)      ;Set to T when :TYPEIN-TOP-LEVEL should return
            (OVERPRINT T)               ;NIL means erase chars before outputing.
            (BLACK-ON-WHITE NIL)
            (ALIAS-WINDOW NIL)          ;Our :ALIAS-FOR-SELECTED-WINDOWS, if non-NIL.
            PROGRAM-NAME)               ;In the "Connect to host" message and help message.
            ()
  (:REQUIRED-FLAVORS TV:LABEL-MIXIN TV:STREAM-MIXIN TV:SHEET)
  (:GETTABLE-INSTANCE-VARIABLES CONNECTION STREAM OUTPUT-BUFFER ALIAS-WINDOW)
  (:INITABLE-INSTANCE-VARIABLES ESCAPE-CHAR TYPEIN-PROCESS TYPEOUT-PROCESS PROGRAM-NAME)
  (:SETTABLE-INSTANCE-VARIABLES CONNECT-TO TERMINAL-STREAM BLACK-ON-WHITE)
  (:REQUIRED-METHODS :CONNECT :GOBBLE-GREETING :NET-OUTPUT :NET-OUTPUT-TRANSLATED)
  (:DEFAULT-INIT-PLIST :DEEXPOSED-TYPEOUT-ACTION ':NOTIFY)
  (:DOCUMENTATION "Network virtual terminal windows"))

(DEFMETHOD (BASIC-NVT :BUFFERED-TYO) (CH)
  (DO () ((ARRAY-PUSH OUTPUT-BUFFER CH))
    (SEND SELF ':FORCE-OUTPUT)))

(DEFMETHOD (BASIC-NVT :FORCE-OUTPUT) ()
  (LET-IF BLACK-ON-WHITE
                   ((TV:CHAR-ALUF TV:ERASE-ALUF))
    (TV:SHEET-STRING-OUT SELF OUTPUT-BUFFER))
  (STORE-ARRAY-LEADER 0 OUTPUT-BUFFER 0))

(DEFMETHOD (BASIC-NVT :ALIAS-FOR-SELECTED-WINDOWS) ()
  (OR ALIAS-WINDOW
      (AND TV:SUPERIOR (SEND TV:SUPERIOR ':ALIAS-FOR-INFERIORS))
      SELF))

(DEFFLAVOR BASIC-SUPDUP () (BASIC-NVT GRAPHICS-MIXIN TV:FULL-SCREEN-HACK-MIXIN)
  (:DEFAULT-INIT-PLIST :PROGRAM-NAME "Supdup")
  (:DOCUMENTATION "A Supdup NVT"))

(DEFFLAVOR SUPDUP () (BASIC-SUPDUP TV:INITIALLY-INVISIBLE-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :COMBINATION))

(DEFMACRO LOCK-OUTPUT BODY
  `(WITH-LOCK (OUTPUT-LOCK)
     . ,BODY))

(DEFMETHOD (BASIC-NVT :SET-SUPER-IMAGE-MODE) (FLAG)
  (COND (FLAG
         (SETF (TV:IO-BUFFER-OUTPUT-FUNCTION TV:IO-BUFFER) NIL)
         (SETF (GETF (TV:IO-BUFFER-PLIST TV:IO-BUFFER) ':ASYNCHRONOUS-CHARACTERS)
               NIL)
         (SETF (GETF (TV:IO-BUFFER-PLIST TV:IO-BUFFER) ':SUPER-IMAGE) t)
         (SETF (GETF (TV:IO-BUFFER-PLIST TV:IO-BUFFER) ':DONT-UPCASE-CONTROL-CHARACTERS) T))
        (T
         (SETF (TV:IO-BUFFER-OUTPUT-FUNCTION TV:IO-BUFFER) 'TV:KBD-DEFAULT-OUTPUT-FUNCTION)
         (SETF (GETF (TV:IO-BUFFER-PLIST TV:IO-BUFFER) ':ASYNCHRONOUS-CHARACTERS)
               TV:KBD-STANDARD-ASYNCHRONOUS-CHARACTERS)
         (SETF (GETF (TV:IO-BUFFER-PLIST TV:IO-BUFFER) ':SUPER-IMAGE) NIL)
         (SETF (GETF (TV:IO-BUFFER-PLIST TV:IO-BUFFER) ':DONT-UPCASE-CONTROL-CHARACTERS)
               NIL))))


;The following is just like TV:PROCESS-MIXIN except that there are two processes
;Also we have to provide for the two I/O buffers
(DEFMETHOD (BASIC-NVT :AFTER :INIT) (IGNORE)
  (UNLESS (VARIABLE-BOUNDP TYPEOUT-PROCESS)
    (SETQ TYPEOUT-PROCESS (MAKE-PROCESS (STRING-APPEND TV:NAME "-Typeout")
                                        :SPECIAL-PDL-SIZE 2000.))
    (SEND TYPEOUT-PROCESS :PRESET SELF ':TYPEOUT-TOP-LEVEL))
  (UNLESS (VARIABLE-BOUNDP TYPEIN-PROCESS)
    (SETQ TYPEIN-PROCESS (MAKE-PROCESS (STRING-APPEND TV:NAME "-Typein")
                                       :SPECIAL-PDL-SIZE 2000.))
    (SEND TYPEIN-PROCESS :PRESET 'TYPEIN-TOP-LEVEL SELF))
  (SEND SELF ':SET-LABEL (FORMAT NIL "~A -- not connected" TV:NAME)))

;Delay starting up processes until they start to get used, to save paging on cold-boot
(DEFMETHOD (BASIC-NVT :BEFORE :SELECT) (&REST IGNORE)
  (MAYBE-RESET-PROCESS TYPEIN-PROCESS)
  (MAYBE-RESET-PROCESS TYPEOUT-PROCESS))

(DEFMETHOD (BASIC-NVT :BEFORE :EXPOSE) (&REST IGNORE)
  (MAYBE-RESET-PROCESS TYPEIN-PROCESS)
  (MAYBE-RESET-PROCESS TYPEOUT-PROCESS))

(DEFUN MAYBE-RESET-PROCESS (PROCESS)
  (WHEN (AND PROCESS (TYPEP PROCESS 'SI:PROCESS))
    (AND (EQ (PROCESS-WAIT-FUNCTION PROCESS) 'SI:FLUSHED-PROCESS)
         (SEND PROCESS ':RESET))
    (SEND PROCESS ':RUN-REASON SELF)))

;; Return a list of our extra processes to be killed.
(DEFMETHOD (BASIC-NVT :PROCESSES) ()
  (APPEND (AND TYPEIN-PROCESS (LIST TYPEIN-PROCESS))
          (AND TYPEOUT-PROCESS (LIST TYPEOUT-PROCESS))))

(DEFMETHOD (BASIC-NVT :AFTER :REFRESH) (&OPTIONAL IGNORE)
  (OR TV:RESTORED-BITS-P (SEND SELF ':HOME-CURSOR)))

(defmethod (basic-nvt :connected-p) ()
  (and connection
       (etypecase connection
         (chaos:conn
          (eq (chaos:state connection) 'chaos:open-state))
         (tcp:tcp-buffered-stream
          (not (null (send connection :remote-address)))))))

(DEFMETHOD (BASIC-NVT :BEFORE :CONNECT) (&REST IGNORE)
  (IF CONNECTION
      (SEND SELF ':DISCONNECT)
      (FS:FORCE-USER-TO-LOGIN)))

(DEFMETHOD (BASIC-NVT :AFTER :SET-CONNECT-TO) (&REST IGNORE)
  (AND TYPEIN-PROCESS (SEND TYPEIN-PROCESS ':RESET)))

(defmethod (basic-nvt :new-connection) (host protocol contact contact-p window
                                        &aux label-spec conn)
  (multiple-value-setq (host contact label-spec)
    (expand-path tv:name host contact contact-p))
  (when host
    (do ((save-conn nil))
        (nil)
      (setq conn (condition-case (error)
                     (ecase protocol
                       (:chaos
                        (chaos:connect host contact window))
                       (:internet
                        (open (format nil "TCP-HOST:~D.~D" host contact) :keyword tv:name :optimistic nil)))
                   (error error)))
      (cond ((not (errorp conn))
             (send self :set-label label-spec)
             (send self :set-connection conn)
             (return conn))
            ((null save-conn)
             (setq save-conn conn)
             (setq protocol (if (eq protocol :chaos) :internet :chaos)))
            (t
             (return save-conn))))))

;;; Path may be any of these:
;;; NIL: Use associated machine.
;;; a fixnum: Use the host whose Chaos address is that number.
;;; host-name: Use that host.
;;; host-name/contact-string: Use that chaos host and that contact string.
;;; internet-host-name/number: Use that Internet host and that socket.
;;;  The ARPA-SOCKET and CONNECT-NAME arguments are overriden by the above.
;;;  Socket numbers are in octal.
;;; If this returns NIL, that means an error occured while parsing the path
(defun parse-path (path contact-name &optional (protocol :chaos))
  (declare (values host protocol contact contact-specified-p))
  (and path
       (symbolp path)
       (setq path (symbol-name path)))
  (condition-case (lossage)
      (let (address host specified-contact slash-p number)
        (typecase path
          (null
           (setq host si:associated-machine))
          (integer
           (or (setq host (si:get-host-from-address path :chaos))
               (setq host path)))
          (string
           (when (setq slash-p (string-search-char #// path))
             (cond ((setq number (parse-number path (1+ slash-p) nil 10. t))
                    ;; Format is "internet-host-name/number".
                    (setq specified-contact number)
                    (setq protocol :internet))
                   (t
                    ;; Format is "host-name/contact-string".
                    (setq specified-contact (substring path (1+ slash-p)))
                    (unless (eq protocol :chaos)
                      (let* ((upper (string-upcase specified-contact))
                             (temp (intern-soft (string-append "IPPORT-" upper) "TCP-APPLICATION")))
                        (if (tcpa:sym-boundp temp)
                            (setq specified-contact upper)      ;If known TCP port, make it upper case
                          (setq protocol :chaos))))))           ;If unknown TCP port name, force Chaos
             (setq path (substring path 0 slash-p)))
           (cond ((setq host (si:parse-host path t nil))        ;Known host
                  (unless (send host :network-typep protocol)   ;Unknown protocol for this host
                    (if specified-contact                       ;Can't switch protocols...
                        (setq host nil)
                      (setq protocol (ecase protocol            ;Otherwise, check other protocol
                                       (:chaos :internet)
                                       (:internet :chaos)))
                      (unless (send host :network-typep protocol)
                        (setq host nil)))))
                 (specified-contact
                  ;;If contact name or number specified, must find particular format of address
                  (multiple-value-setq (address host)
                    (funcall (ecase protocol
                               (:chaos 'chaos:address-parse)
                               (:internet 'ip:parse-internet-address))
                             path)))
                 ((multiple-value-setq (address host) (ip:parse-internet-address path))
                  (setq protocol :internet))
                 ((multiple-value-setq (address host) (chaos:address-parse path))
                  (setq protocol :chaos))
                 (t
                  (setq host nil))))
          (si:host
           (setq host path))
          (t
           (error "Invalid host specification")))
        (values (or host                        ;Known host...
                    (when (eq protocol :internet) path) ;Unknown Internet host specified as dotted decimal...
                    address)                    ;Chaos address
                protocol
                (or specified-contact contact-name)
                (not (null specified-contact))))
    (error
     (format *terminal-io* "~&Error: ~A~%" lossage)
     (signal eh:abort-object))
     nil))

(DEFUN EXPAND-PATH (PROGNAME HOST CONTACT CONTACT-P)
  (DECLARE (VALUES HOST CONTACT LABEL))
  (VALUES HOST
          CONTACT
          (FORMAT NIL "~A -- ~A~:[ (~D)~]" PROGNAME HOST (NOT CONTACT-P) CONTACT)))

(defmethod (basic-nvt :set-connection) (new-connection)
  (send typein-process :reset)
  (send typeout-process :reset)
  (setq stream (etypecase new-connection
                 (chaos:conn
                  (chaos:make-stream new-connection))
                 (tcp:tcp-buffered-stream
                  new-connection)))
  (send self :gobble-greeting)
;; Typeout process initially waits to see CONNECTION non-NIL.
  (setq connection new-connection)
  (setq black-on-white nil))

(DEFMETHOD (BASIC-NVT :DISCONNECT) ()
  (SEND TYPEIN-PROCESS ':FLUSH)
  (SEND TYPEOUT-PROCESS ':FLUSH)
  (WHEN CONNECTION
    (when (typep connection 'chaos:conn)
      (CHAOS:CLOSE-CONN CONNECTION)
      (CHAOS:REMOVE-CONN CONNECTION))
    (SETQ CONNECTION NIL)
    (close stream))
  (SEND TYPEIN-PROCESS ':RESET)
  (SEND TYPEOUT-PROCESS ':RESET))

;;;This is the typein process
(DEFUN TYPEIN-TOP-LEVEL (WINDOW)
  (DO () (())
    (SEND WINDOW ':TYPEIN-TOP-LEVEL)
    (TV:DESELECT-AND-MAYBE-BURY-WINDOW WINDOW)))

(DEFMETHOD (BASIC-NVT :TYPEIN-TOP-LEVEL) (&OPTIONAL (TOP-LEVEL-P T) &AUX (TERMINAL-IO SELF))
  (DO ((STR NIL NIL)) (NIL)
    (SETQ RETURN-TO-CALLER NIL)
    (SETQ STR ':ABORT)
    (CATCH-ERROR-RESTART-IF TOP-LEVEL-P
                            ((SYS:ABORT ERROR) "Return to command level; optionally disconnect.")
      (SETQ STR
            (CATCH 'NVT-DONE
              (COND (CONNECTION
                     (SEND SELF :SET-SUPER-IMAGE-MODE T)
                     (CONDITION-BIND ((SYS:REMOTE-NETWORK-ERROR 'NET-ERROR))
                       (DO ((CH)) (NIL)                  ;this is the MAIN LOOP looking at what loser types.
                         (OR (SEND SELF :LISTEN)
                             (LOCK-OUTPUT                ;unless more input has already been typed, unbuffer
                               (SEND STREAM :FORCE-OUTPUT)))  ;stuff already typed.
                         (SETQ CH (SEND SELF :ANY-TYI))
                         (IF (CONSP CH)
                             (CASE (CAR CH)
                               (:ERROR (THROW 'NVT-DONE (CADR CH)))
                               (:MORE (SEND SELF ':MORE-TYI))
                               (OTHERWISE
                                (SEND SELF :NET-OUTPUT-TRANSLATED CH)))
                           (etypecase connection
                             (chaos:conn
                              (SELECTQ (CHAOS:STATE CONNECTION)
                                (CHAOS:OPEN-STATE)
                                (CHAOS:HOST-DOWN-STATE
                                 (THROW 'NVT-DONE "Foreign Host died"))
                                (CHAOS:CLS-RECEIVED-STATE
                                 (THROW 'NVT-DONE "Closed by foreign host"))
                                (CHAOS:LOS-RECEIVED-STATE
                                 (THROW 'NVT-DONE "Connection closed due to lossage:"))
                                (OTHERWISE
                                 (THROW 'NVT-DONE
                                        (FORMAT NIL "Connection in unknown state:~S"
                                                (CHAOS:STATE CONNECTION))))))
                             (tcp:tcp-buffered-stream
                              (unless (send stream :remote-address)
                                (throw 'nvt-done "Connection closed"))))
                           (IF (= (CHAR-UPCASE CH) ESCAPE-CHAR)
                               ;;Handle the escape character,
                               (SEND SELF ':HANDLE-ESCAPE)
                             ;; otherwise just send through what user typed.
                             (SEND SELF ':NET-OUTPUT-TRANSLATED CH))))))
                    (CONNECT-TO
                     (SEND SELF :CLEAR-WINDOW)
                     (CONDITION-CASE (ERROR)
                         (SEND SELF :CONNECT (PROG1 CONNECT-TO
                                                    (SETQ CONNECT-TO NIL)))
                       (SYS:REMOTE-NETWORK-ERROR ERROR)))
                    (T (SEND SELF :SET-SUPER-IMAGE-MODE NIL)
                       (LETF (((TV:IO-BUFFER-OUTPUT-FUNCTION TV:IO-BUFFER)
                               'SUPDUP-IO-BUFFER-OUTPUT-FUNCTION))
                         (DO () (())
                           ;; Loop until loser types in something non-blank.
                           (FORMAT T "~&~A.  Type the HELP key for help.~@
                                      Connect to host: " PROGRAM-NAME)
                           (LET ((HOST-NAME (STRING-TRIM  '(#/SPACE #/TAB) (READLINE))))
                             (WHEN (PLUSP (STRING-LENGTH HOST-NAME))
                               (RETURN
                                 (CONDITION-CASE (ERROR)
                                     (SEND SELF :CONNECT HOST-NAME)
                                   (SYS:REMOTE-NETWORK-ERROR ERROR))))))))))))
    (COND ((ERRORP STR)
           (SETQ STR (SEND STR :REPORT-STRING)))
          ((EQ STR ':ABORT)
           (SETQ STR (IF (AND CONNECTION (Y-OR-N-P "Disconnect the ~A connection? " program-name))
                         "Connection aborted" NIL))))
    (WHEN (STRINGP STR)
      (SEND SELF :DISCONNECT)
      (FORMAT SELF "~%~A~%" STR)
      (AND RETURN-TO-CALLER (RETURN T)))))

(DEFVAR INHIBIT-TOP-LEVEL-HELP NIL)

;;; This is used as the I/O buffer output function while reading a hostname.
;;; It intercepts Help and Network, as well as the usual things.
(DEFUN SUPDUP-IO-BUFFER-OUTPUT-FUNCTION (IGNORE CHAR)
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (COND ((NOT (NUMBERP CHAR)) CHAR)             ;Blips shouldn't get here, but don't die
        ((AND (EQ CHAR #/HELP)
              (NOT INHIBIT-TOP-LEVEL-HELP))     ;T if recursive, from clause below.
         (SETQ INHIBIT-SCHEDULING-FLAG NIL)
         (TV:OUTPUT-BEFORE-RUBOUT-HANDLER (SELF)
           (SEND SELF :HELP-MESSAGE))
         (VALUES CHAR T))
        ((= (CHAR-UPCASE CHAR) ESCAPE-CHAR)
         (SETQ INHIBIT-SCHEDULING-FLAG NIL)
         (LET ((INHIBIT-TOP-LEVEL-HELP T))
           (SEND SELF :HANDLE-ESCAPE))
         (VALUES CHAR T))
        (T (LET ((TEM (ASSQ CHAR TV:KBD-INTERCEPTED-CHARACTERS)))
             (IF TEM (FUNCALL (CADR TEM) CHAR)
               CHAR)))))

;;; Call this before inputting things.  It peeks for an escape and handles it.
(DEFMETHOD (BASIC-NVT :ALLOW-ESCAPE) ()
  (DO ()
      ((LET ((CHAR (SEND STANDARD-INPUT :ANY-TYI)))
         (COND ((AND (or (NUMBERP CHAR) (CHARACTERP CHAR))
                     (CHAR-EQUAL CHAR ESCAPE-CHAR))
                (SEND SELF :HANDLE-ESCAPE)      ; Handle the escape character.
                NIL)                            ; Keep looping.
               ((AND (OR (NUMBERP CHAR) (CHARACTERP CHAR))
                     (CHAR-EQUAL CHAR #/HELP))
                (SEND SELF :HELP-MESSAGE)       ; Give the user some help.
                NIL)                            ; Keep looping.
               (T
                (SEND SELF :UNTYI CHAR)         ; Put back character, exit loop.
                T)))
       NIL)))

;;; Default help message.
(DEFMETHOD (BASIC-NVT :HELP-MESSAGE)
           (&AUX (FORMAT-ARGS
                   (LIST "~
~&You are using the ~A remote-login program.
To connect to any Chaosnet or Internet host, just type the target host name.
If you want to connect to a specific port on an Internet host, follow
the name of the Internet host by a slash and either the port number
in decimal or a known port name (like FTP or SMTP).
If you want to connect to a specific connect-name on a Chaosnet host,
follow the name of the Chaosnet host by a slash and the connect name.

Summary:
  host      (for either network)
  internet-host//port-number (decimal)
  internet-host//port-name
  chaos-host//connect-name

At any time you can type the [Network] key to give any of a number of useful
commands.  For descriptions of the available commands, type [Network] [Help].

Connect to host: "
                         PROGRAM-NAME)))
  (COND ((NULL CONNECTION)
         (SEND *STANDARD-OUTPUT* :CLEAR-WINDOW)
         (APPLY #'FORMAT T FORMAT-ARGS))
        (T
         (SI:WITH-HELP-STREAM (HELP-STREAM :LABEL "Keyboard system commands")
           (APPLY #'FORMAT HELP-STREAM FORMAT-ARGS)))))

;;;Condition handler for typein side.
(DEFUN NET-ERROR (CONDITION)
  (THROW 'NVT-DONE (SEND CONDITION ':REPORT-STRING)))

(DEFMETHOD (BASIC-NVT :TOGGLE-OVERPRINTING) ()
  (SETQ OVERPRINT (NOT OVERPRINT)))

;;;Handle a command to the SUPDUP program itself.
(defmethod (basic-nvt :handle-escape) (&aux ch xpos ypos command)
  (unwind-protect
    (progn
      (multiple-value (xpos ypos) (tv:sheet-read-cursorpos self))
      (put-down-string self "CMND-->")
      (setq ch (char-upcase (send self :tyi)))
      (selectq ch
        ((#/Call #/End)
         (tv:deselect-and-maybe-bury-window self))
        (#/A
         (unless (null connection)
           (setq command '(send self :send-if-handles :send-ao))))
        ((#/B #/Break)
         (send self :set-super-image-mode nil)
         (break "BREAK")
         (send self :set-super-image-mode t))
        (#/Clear-input
         (unless (null connection)
           (setq command '(send self :send-if-handles :send-el))))
        (#/C                          ;C = Change escape character.
         (put-down-string self "Change escape character to -->")
         (send self :set-super-image-mode nil)
         (setq escape-char (char-upcase (send self :tyi)))
         (send self :set-super-image-mode t))
        (#/Delete
         (unless (null connection)
           (setq command '(send self :send-if-handles :send-ec))))
        (#/D                         ;D = Disconnect, ask for new host to connect to.
         (if (null connection)
             (throw 'nvt-done "(Already disconnected.)")
           (send self :disconnect)
           (throw 'nvt-done "Disconnected")))
        (#/E
         (unless (null connection)
           (setq command '(send self :send-if-handles :toggle-local-echo))))
        (#/Control-shift-e
         (unless (null connection)
           (setq command '(send self :send-if-handles :toggle-remote-echo))))
        (#/I                          ;I = Imlac.
         (if (not (null connection))
             (send self :send-if-handles :toggle-imlac-simulation)))
        (#/L                          ;L = Logout.
         (if (null connection)
             (quit)
           (setq command '(progn
                            (send self :logout)
                            (quit "Logout")))))
        (#/M                          ;M = More.
         (if (not (null connection))
             (send self :send-if-handles :user-set-more-p (not (send self :more-p)))))
;       (#/N
;        (send self :set-super-image-mode nil))
        (#/O
         (send self :send-if-handles :toggle-overprinting))
        (#/P
         (unless (null connection)
           (setq command '(send self :send-if-handles :send-ip))))
        (#/Q                          ;Q = Quit.
         (quit))
        (#/Status
         (unless (null connection)
           (setq command '(send self :send-if-handles :send-ayt))))
        (#/V
         (send self :send-if-handles :toggle-verbose-mode))
        (#/Y
         (let* ((node (zwei:history-latest-element zwei:*kill-history*))
                (string (if (arrayp node) node (zwei:string-interval node nil nil t))))
           (dotimes (i (array-length string))
             (send self :force-kbd-input (aref string i)))))
        (#/c-sh-Y       ;similar to Y, but send it directly to the net instead of via KBD buffering.
         (let* ((node (zwei:history-latest-element zwei:*kill-history*))
                (string (if (arrayp node) node (zwei:string-interval node nil nil t))))
           (dotimes (i (array-length string))
             (send self :net-output-translated (aref string i)))))
        ((#/Help #/?)                 ;<HELP> or ? = Help
         (si:with-help-stream (window :label "Help for Network commands")
           (send window :clear-window)
           (format window "After typing the Escape character, which is ~:C,
you can type these commands:~%" escape-char)
           (format window "
CALL -- Do a local CALL (return to top window).
BREAK-- Enter a breakpoint.
~:[~;A    -- Send an AO (Abort Output)
~]~
~:[~;Clear Input    -- Send an EL (Erase Line)
~]~
C    -- Change the ~A escape character.
~:[~;Delete    -- Send an EC (Erase Character)
~]~
D    -- Disconnect and connect to new host.
End  -- Return to top window, but don't break connection.
~:[~;E    -- Toggle local echo
~]~
~:[~;C-E  -- Toggle remote echo
~]~
Help -- Type this message.
~:[~;I    -- Toggle imlac simulation.
~]~
L    -- Log out of remote host, and break the connection.
~:[~;M    -- Toggle more processing.
~]~
~:[~;O    -- Toggle overprinting (for servers that expect non-overprinting terminals).
~]~
~:[~;P    -- Send an IP (Interrupt Process)
~]~
Q    -- Disconnect and return to top window.
~:[~;Status -- Send an AYT (Are You There)
~]~
~:[~;V    -- Toggle verbose mode
~]~
Y    -- Send the most recently killed string through the connection.
c-Y  -- Similar to Y, but send it directly to net instead of via typin loop.
"
                   (get-handler-for self :send-ao)
                   (get-handler-for self :send-el)
                   program-name
                   (get-handler-for self :send-ec)
                   (get-handler-for self :toggle-local-echo)
                   (get-handler-for self :toggle-remote-echo)
                   (get-handler-for self :toggle-imlac-simulation)
                   (get-handler-for self :user-set-more-p)
                   (get-handler-for self :toggle-overprinting)
                   (get-handler-for self :send-ip)
                   (get-handler-for self :send-ayt)
                   (get-handler-for self :toggle-verbose-mode))
           (format window "~4A -- Send ~:C through~%"
                   (format nil "~:C" escape-char)
                   escape-char)))
        (#/Rubout)                              ;<RUBOUT> = Do nothing.
        (otherwise
          (cond ((= ch escape-char)
                 (send self :net-output-translated ch)
                 (lock-output
                   (send stream :force-output)))
                (t (tv:beep))))))
    (tv:sheet-force-access (self t)
      (put-down-string self "")      ;Clear the bottom line.
      (tv:sheet-set-cursorpos self xpos ypos))
    (when command
      (eval command))))

(DEFUN QUIT (&OPTIONAL (STRING "Quit"))
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (SEND SELF :DISCONNECT)
  (SETQ RETURN-TO-CALLER T)
  (THROW 'NVT-DONE STRING))

(DEFUN PUT-DOWN-STRING (SHEET STRING)
  (SEND SHEET :HOME-DOWN)
  (TV:SHEET-CLEAR-EOL SHEET)
  (TV:SHEET-STRING-OUT SHEET STRING))

;;; In the typeout process, this is bound to the SUPDUP window.
(DEFVAR SUPDUP-WINDOW)

;;; This is the output process.
;;; We leave *TERMINAL-IO* as the background stream because having it be the SUPDUP
;;; window wedges things when this process gets an error.
(DEFMETHOD (BASIC-NVT :TYPEOUT-TOP-LEVEL) (&AUX (SUPDUP-WINDOW SELF))
  (PROCESS-WAIT "Never-open" #'CAR (LOCATE-IN-INSTANCE SELF 'CONNECTION))
  (CONDITION-BIND ((SYS:REMOTE-NETWORK-ERROR
                    'TYPEOUT-NET-ERROR))
    (DO ((OUTPUT-FUN (OR TERMINAL-STREAM (GET-HANDLER-FOR SELF ':BUFFERED-TYO))))
        (NIL)
      (DO ((CH (NVT-NETI) (SEND STREAM ':TYI-NO-HANG)))
          ((NULL CH)
           (OR TERMINAL-STREAM (SEND SELF ':FORCE-OUTPUT)))
        ;; If not buffered, clear each char pos before we output it.
        ;; (If buffered, our :before :force-output will do it).
        (IF (OR BLACK-ON-WHITE (NOT OVERPRINT))
            (IF TERMINAL-STREAM
                (SEND OUTPUT-FUN ':CLEAR-CHAR CH)))
        (SEND OUTPUT-FUN ':TYO CH)))))

(DEFMETHOD (BASIC-NVT :BEFORE :FORCE-OUTPUT) ()
  (LET-IF BLACK-ON-WHITE
                   ((TV:ERASE-ALUF TV:CHAR-ALUF))
    (IF (OR BLACK-ON-WHITE (NOT OVERPRINT))
        (TV:SHEET-CLEAR-STRING SELF OUTPUT-BUFFER))))

(DEFVAR LOG-STREAM NIL)

(DEFUN NVT-NETI (&AUX CH)
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (COND ((SETQ CH (SEND STREAM :TYI))
         (WHEN LOG-STREAM
           (LET ((*STANDARD-OUTPUT* LOG-STREAM))
             (IF (>= CH #O200) (PRIN1 CH)
                 (FORMAT:OCHAR CH :EDITOR))))
         CH)
        (T
         (SEND SUPDUP-WINDOW ':FORCE-KBD-INPUT '(:ERROR "Closed by foreign host"))
         (PROCESS-WAIT "Connection closed" #'FALSE))))

(DEFUN TYPEOUT-NET-ERROR (CONDITION)
  (SEND SUPDUP-WINDOW ':FORCE-KBD-INPUT
        (LIST ':ERROR (SEND CONDITION ':REPORT-STRING)))
  (SI:PROCESS-WAIT-FOREVER))

(DEFMETHOD (BASIC-NVT :REMOTE-BEEP) ()
  (TV:PREPARE-SHEET (SELF))                     ;Modular way to deal with output hold.
  (WHEN (OR (MEMQ (SEND SELF :STATUS) '(:EXPOSED :SELECTED))
            (EQ (SEND SELF :DEEXPOSED-TYPEIN-ACTION) :NOTIFY))
    (SEND SELF :BEEP 'TERMINAL-BELL)))

;;; Suppress notification if we do not have a connection
(DEFMETHOD (BASIC-NVT :NOTICE) (EVENT &REST IGNORE)
  (AND (MEMQ EVENT '(:INPUT :OUTPUT))
       (NOT CONNECTION)))

;;;; SUPDUP Graphics Protocol

(DEFFLAVOR GRAPHICS-MIXIN (GRAPHICS-X-OFFSET GRAPHICS-Y-OFFSET
                           GRAPHICS-VIRT-SCALE
                           GRAPHICS-XPOS GRAPHICS-YPOS
                           GRAPHICS-LEFT GRAPHICS-TOP
                           GRAPHICS-RIGHT GRAPHICS-BOTTOM
                           GRAPHICS-XOR-MODE GRAPHICS-VIRTUAL-MODE)
  ()
  (:INCLUDED-FLAVORS TV:SHEET))

;;; Note that ALL four edge coordinates are INCLUSIVE:
;;; they are values corresponding to points which actually exist.
;;; This is in contrast to the window system,
;;; in which the lower limits are inclusive and the upper are exclusive.

(DEFMETHOD (GRAPHICS-MIXIN :AFTER :INIT) (&REST IGNORE)
  (GRAPHICS-RESET SELF)
  (SETQ GRAPHICS-XPOS 0 GRAPHICS-YPOS 0))

;;; Initialize all the variables used for graphics commands.
(DEFUN GRAPHICS-RESET (WINDOW)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (LET ((CORRECTED-RIGHT (+ (TV:SHEET-INSIDE-LEFT WINDOW)
                              (* (TV:SHEET-CHAR-WIDTH WINDOW)
                                 (TRUNCATE (TV:SHEET-INSIDE-WIDTH WINDOW)
                                           (TV:SHEET-CHAR-WIDTH WINDOW)))))
          (CORRECTED-BOTTOM (+ (TV:SHEET-INSIDE-TOP WINDOW)
                               (* (TV:SHEET-LINE-HEIGHT WINDOW)
                                  (TRUNCATE (TV:SHEET-INSIDE-HEIGHT WINDOW)
                                            (TV:SHEET-LINE-HEIGHT WINDOW))))))
      (SETQ GRAPHICS-X-OFFSET (TRUNCATE (+ (TV:SHEET-INSIDE-LEFT WINDOW) CORRECTED-RIGHT) 2)
            GRAPHICS-Y-OFFSET (TRUNCATE (+ (TV:SHEET-INSIDE-TOP WINDOW) CORRECTED-BOTTOM) 2)
            GRAPHICS-XPOS 0
            GRAPHICS-YPOS 0
            GRAPHICS-VIRT-SCALE (// (MIN (- CORRECTED-RIGHT (TV:SHEET-INSIDE-LEFT WINDOW))
                                         (- CORRECTED-BOTTOM (TV:SHEET-INSIDE-TOP WINDOW)))
                                    2.0s0
                                    #o4000)
            GRAPHICS-XOR-MODE NIL
            GRAPHICS-VIRTUAL-MODE NIL
            GRAPHICS-LEFT (- (TV:SHEET-INSIDE-LEFT WINDOW) GRAPHICS-X-OFFSET)
            GRAPHICS-RIGHT (- (TV:SHEET-INSIDE-RIGHT WINDOW) GRAPHICS-X-OFFSET 1)
            GRAPHICS-BOTTOM (- GRAPHICS-Y-OFFSET (1- (TV:SHEET-INSIDE-BOTTOM WINDOW)))
            GRAPHICS-TOP (- GRAPHICS-Y-OFFSET (TV:SHEET-INSIDE-TOP WINDOW)))))

(DEFVAR GRAPHICS-DISPATCH (MAKE-ARRAY #o100))
(FILLARRAY GRAPHICS-DISPATCH
  '(GRAPHICS-NOTHING GRAPHICS-MOVE GRAPHICS-XOR GRAPHICS-NOTHING
    GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-NOTHING
    GRAPHICS-ERASE-SCREEN GRAPHICS-PUSH GRAPHICS-VIRTUAL GRAPHICS-NOTHING
    GRAPHICS-NOTHING GRAPHICS-LIMIT GRAPHICS-NOTHING GRAPHICS-NOTHING
    GRAPHICS-NOTHING GRAPHICS-MOVE GRAPHICS-IOR GRAPHICS-NOTHING
    GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-NOTHING
    GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-PHYSICAL GRAPHICS-NOTHING
    GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-NOTHING GRAPHICS-NOTHING))

(DEFVAR DRAW-DISPATCH (MAKE-ARRAY #o20))
(FILLARRAY DRAW-DISPATCH
  '(GRAPHICS-NOTHING GRAPHICS-DRAW-LINE GRAPHICS-DRAW-POINT GRAPHICS-DRAW-RECT
    GRAPHICS-DRAW-STRING GRAPHICS-DRAW-BITS GRAPHICS-DRAW-RUNS GRAPHICS-NOTHING))

(DEFUN SUPDUP-GRAPHICS (WINDOW)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (CATCH 'SUPDUP-GRAPHICS
    (DO (CH) (())
      (SETQ CH (GRAPHICS-NETI))
      (COND ((BIT-TEST CH #o100)
             (FUNCALL (OR (AREF DRAW-DISPATCH (LOGAND CH #o17))
                          'GRAPHICS-NOTHING)
                      WINDOW CH))
            (T
             (FUNCALL (OR (AREF GRAPHICS-DISPATCH CH)
                          'GRAPHICS-NOTHING)
                      WINDOW CH))))))


;;;; Subroutines for graphics commands.

(DEFUN GRAPHICS-NETI ()
  (DECLARE (:SELF-FLAVOR BASIC-SUPDUP))
  (LET ((CH (NVT-NETI)))
    (AND (BIT-TEST CH #o200)
         (THROW 'SUPDUP-GRAPHICS
                (SEND STREAM :UNTYI CH)))
    CH))

(DEFUN GRAPHICS-READ-POINT (CH &AUX CH1 CH2 CH3 CH4)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (COND ((BIT-TEST CH #o20)
         (SETQ GRAPHICS-XPOS (14-BIT-SIGN-EXTEND (+ (SETQ CH1 (GRAPHICS-NETI))
                                                    (LSH (SETQ CH2 (GRAPHICS-NETI)) 7))))
         (SETQ GRAPHICS-YPOS (14-BIT-SIGN-EXTEND (+ (SETQ CH3 (GRAPHICS-NETI))
                                                    (LSH (SETQ CH4 (GRAPHICS-NETI)) 7)))))
        (T
         (INCF GRAPHICS-XPOS (SETQ CH1 (7-BIT-SIGN-EXTEND (GRAPHICS-NETI))))
         (INCF GRAPHICS-YPOS (SETQ CH2 (7-BIT-SIGN-EXTEND (GRAPHICS-NETI)))))))

(DEFUN 7-BIT-SIGN-EXTEND (NUMBER)
  (IF (BIT-TEST NUMBER #o100)
      (- NUMBER #o200)
    NUMBER))

(DEFUN 14-BIT-SIGN-EXTEND (NUMBER)
  (IF (BIT-TEST NUMBER #o20000)
      (- NUMBER #o40000)
    NUMBER))

(DEFUN GRAPHICS-ALU (CH)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (COND (GRAPHICS-XOR-MODE TV:ALU-XOR)
        ((BIT-TEST CH #o40) TV:ALU-ANDCA)
        (T TV:ALU-IOR)))

(DEFUN GRAPHICS-X-COORD (COORD)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (AND GRAPHICS-VIRTUAL-MODE
       (SETQ COORD (FIXR (* COORD GRAPHICS-VIRT-SCALE))))
  (+ GRAPHICS-X-OFFSET (MIN GRAPHICS-RIGHT (MAX GRAPHICS-LEFT COORD))))

(DEFUN GRAPHICS-Y-COORD (COORD)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (AND GRAPHICS-VIRTUAL-MODE
       (SETQ COORD (FIXR (* COORD GRAPHICS-VIRT-SCALE))))
  (- GRAPHICS-Y-OFFSET (MIN GRAPHICS-TOP (MAX GRAPHICS-BOTTOM COORD))))

(DEFUN GRAPHICS-Y-IN-RANGE (&AUX COORD)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (SETQ COORD GRAPHICS-YPOS)
  (AND GRAPHICS-VIRTUAL-MODE
       (SETQ COORD (FIXR (* COORD GRAPHICS-VIRT-SCALE))))
  (<= GRAPHICS-BOTTOM COORD GRAPHICS-TOP))

;;;; Graphics commands.

(DEFUN GRAPHICS-NOTHING (IGNORE IGNORE)
  NIL)

(DEFUN GRAPHICS-XOR (IGNORE IGNORE)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (SETQ GRAPHICS-XOR-MODE T))

(DEFUN GRAPHICS-IOR (IGNORE IGNORE)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (SETQ GRAPHICS-XOR-MODE NIL))

(DEFUN GRAPHICS-VIRTUAL (IGNORE IGNORE)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (SETQ GRAPHICS-VIRTUAL-MODE T))

(DEFUN GRAPHICS-PHYSICAL (IGNORE IGNORE)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (SETQ GRAPHICS-VIRTUAL-MODE NIL))

(DEFUN GRAPHICS-MOVE (IGNORE CH)
  (GRAPHICS-READ-POINT CH))

(DEFUN GRAPHICS-PUSH (WINDOW IGNORE)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (LETF
    ((GRAPHICS-XPOS GRAPHICS-XPOS)
     (GRAPHICS-YPOS GRAPHICS-YPOS)
     (GRAPHICS-RIGHT GRAPHICS-RIGHT)
     (GRAPHICS-LEFT GRAPHICS-LEFT)
     (GRAPHICS-TOP GRAPHICS-TOP)
     (GRAPHICS-BOTTOM GRAPHICS-BOTTOM)
     (GRAPHICS-XOR-MODE GRAPHICS-XOR-MODE)
     (GRAPHICS-VIRTUAL-MODE GRAPHICS-VIRTUAL-MODE))
    (DO (CH) (())
      (SETQ CH (GRAPHICS-NETI))
      (COND ((BIT-TEST CH #o100)
             (FUNCALL (OR (AREF DRAW-DISPATCH (LOGAND CH #o17))
                          'GRAPHICS-NOTHING)
                      WINDOW CH))
            (T
             (FUNCALL (OR (AREF GRAPHICS-DISPATCH CH)
                          'GRAPHICS-NOTHING)
                      WINDOW CH))))))

(DEFUN GRAPHICS-LIMIT (IGNORE CH)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (GRAPHICS-READ-POINT CH)
  (LET ((OXPOS GRAPHICS-XPOS) (OYPOS GRAPHICS-YPOS))
    (GRAPHICS-READ-POINT CH)
    (SETQ GRAPHICS-LEFT (MIN GRAPHICS-XPOS OXPOS)
          GRAPHICS-RIGHT (MAX GRAPHICS-XPOS OXPOS)
          GRAPHICS-BOTTOM (MIN GRAPHICS-YPOS OYPOS)
          GRAPHICS-TOP (MAX GRAPHICS-YPOS OYPOS))))


;;;; Drawing commands.

(DEFUN GRAPHICS-DRAW-LINE (WINDOW CH)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (TV:PREPARE-SHEET (WINDOW)
    (LET ((OXPOS GRAPHICS-XPOS)
          (OYPOS GRAPHICS-YPOS))
      (GRAPHICS-READ-POINT CH)
      (SYSTEM:%DRAW-LINE (GRAPHICS-X-COORD OXPOS) (GRAPHICS-Y-COORD OYPOS)
                         (GRAPHICS-X-COORD GRAPHICS-XPOS)
                         (GRAPHICS-Y-COORD GRAPHICS-YPOS)
                         (GRAPHICS-ALU CH)
                         T
                         WINDOW))))

(DEFUN GRAPHICS-DRAW-POINT (WINDOW CH)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (TV:PREPARE-SHEET (WINDOW)
    (GRAPHICS-READ-POINT CH)
    (SYSTEM:%DRAW-LINE (GRAPHICS-X-COORD GRAPHICS-XPOS)
                       (GRAPHICS-Y-COORD GRAPHICS-YPOS)
                       (GRAPHICS-X-COORD GRAPHICS-XPOS)
                       (GRAPHICS-Y-COORD GRAPHICS-YPOS)
                       (GRAPHICS-ALU CH)
                       T
                       WINDOW)))

(DEFUN GRAPHICS-DRAW-RECT (WINDOW CH)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (TV:PREPARE-SHEET (WINDOW)
    (LET ((OXPOS GRAPHICS-XPOS)
          (OYPOS GRAPHICS-YPOS))
      (GRAPHICS-READ-POINT CH)
      (TV:%DRAW-RECTANGLE
        (ABS (1+ (- (GRAPHICS-X-COORD GRAPHICS-XPOS) (GRAPHICS-X-COORD OXPOS))))
        (ABS (1+ (- (GRAPHICS-Y-COORD GRAPHICS-YPOS) (GRAPHICS-Y-COORD OYPOS))))
        (MIN (GRAPHICS-X-COORD GRAPHICS-XPOS) (GRAPHICS-X-COORD OXPOS))
        (MIN (GRAPHICS-Y-COORD GRAPHICS-YPOS) (GRAPHICS-Y-COORD OYPOS))
        (GRAPHICS-ALU CH)
        WINDOW))))

(DEFUN GRAPHICS-ERASE-SCREEN (WINDOW IGNORE)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (TV:PREPARE-SHEET (WINDOW)
    (TV:%DRAW-RECTANGLE
      (1+ (- (GRAPHICS-X-COORD GRAPHICS-RIGHT) (GRAPHICS-X-COORD GRAPHICS-LEFT)))
      (1+ (- (GRAPHICS-Y-COORD GRAPHICS-BOTTOM) (GRAPHICS-Y-COORD GRAPHICS-TOP)))
      (GRAPHICS-X-COORD GRAPHICS-LEFT)
      (GRAPHICS-Y-COORD GRAPHICS-TOP)
      (TV:SHEET-ERASE-ALUF WINDOW)
      WINDOW)))

(DEFUN GRAPHICS-DRAW-STRING (WINDOW CH)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (LET ((STRING (MAKE-STRING #o100 :FILL-POINTER 0)))
    (DO-FOREVER
      (LET ((CH (GRAPHICS-NETI)))
        (AND (ZEROP CH) (RETURN))
        (ARRAY-PUSH-EXTEND STRING CH)))
    (SEND WINDOW ':STRING-OUT-EXPLICIT STRING
          (GRAPHICS-X-COORD GRAPHICS-XPOS)
          (GRAPHICS-Y-COORD GRAPHICS-YPOS)
          (GRAPHICS-X-COORD GRAPHICS-RIGHT)
          NIL
          (TV:SHEET-CURRENT-FONT WINDOW)
          (GRAPHICS-ALU CH))))

(DEFUN GRAPHICS-DRAW-BITS (WINDOW CH &AUX BIT-CHANGE-FUNCTION)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (TV:PREPARE-SHEET (WINDOW)
    (SETQ BIT-CHANGE-FUNCTION (GRAPHICS-ALU CH))
    (DO ((I 1 (1+ I))) (())
      (LET ((NBITS (IF (ZEROP (\ I 3)) 4 6))
            (X (GRAPHICS-X-COORD GRAPHICS-XPOS))
            (Y (GRAPHICS-Y-COORD GRAPHICS-YPOS))
            (CH (GRAPHICS-NETI)))
        (IF (BIT-TEST CH #o100)
            (RETURN NIL)
          (WHEN (GRAPHICS-Y-IN-RANGE)
            (DOTIMES (BIT NBITS)
              (UNLESS (>= GRAPHICS-XPOS GRAPHICS-RIGHT)
                (IF (BIT-TEST (LSH 1 (- NBITS 1 BIT)) CH)
                    (SETF (AR-2-REVERSE TV:SCREEN-ARRAY (+ BIT X) Y)
                          (BOOLE BIT-CHANGE-FUNCTION
                                 (AR-2-REVERSE TV:SCREEN-ARRAY (+ BIT X) Y)
                                 1))))))
          (INCF GRAPHICS-XPOS NBITS))))))

(DEFUN GRAPHICS-DRAW-RUNS (WINDOW CH &AUX BIT-CHANGE-FUNCTION)
  (DECLARE (:SELF-FLAVOR GRAPHICS-MIXIN))
  (TV:PREPARE-SHEET (WINDOW)
    (SETQ BIT-CHANGE-FUNCTION (GRAPHICS-ALU CH))
    (DO-FOREVER
      (LET ((CH (GRAPHICS-NETI))
            (OLDX (GRAPHICS-X-COORD GRAPHICS-XPOS))
            (Y (GRAPHICS-Y-COORD GRAPHICS-YPOS)))
        (IF (ZEROP CH)
            (RETURN NIL)
          (INCF GRAPHICS-XPOS (LOGAND CH #o77))
          (AND (BIT-TEST CH #o100)
               (GRAPHICS-Y-IN-RANGE)
               (SYS:%DRAW-LINE OLDX Y (MAX OLDX (1- (GRAPHICS-X-COORD GRAPHICS-XPOS))) Y
                               BIT-CHANGE-FUNCTION T WINDOW)))))))

(DEFRESOURCE TYPEOUT-PROCESSES ()
  "Typeout processes for NVTs"
  :CONSTRUCTOR (MAKE-PROCESS "NVT-Typeout" :SPECIAL-PDL-SIZE 2000.))

(DEFVAR *SUPDUP-WINDOWS* NIL)
(DEFVAR *SUPDUP-DEFAULT-PATH* NIL
  "NIL => use associated machine.")
(DEFVAR *SUPDUP-MODE* T
  "NIL => New window default")

(DEFVAR SUPDUP-WINDOWS)
(DEFVAR SUPDUP-DEFAULT-PATH)
(DEFVAR SUPDUP-MODE)
(FORWARD-VALUE-CELL 'SUPDUP-WINDOWS '*SUPDUP-WINDOWS*)
(FORWARD-VALUE-CELL 'SUPDUP-DEFAULT-PATH '*SUPDUP-DEFAULT-PATH*)
(FORWARD-VALUE-CELL 'SUPDUP-MODE '*SUPDUP-MODE*)

(DEFVAR JOURNAL-STREAM NIL
  "Stream for reccording history of events for debugging.")

(DEFUN FIND-SELECTABLE-SUPDUP (CONNECTED-P &OPTIONAL (SUP TV:MOUSE-SHEET))
  (DOLIST (W SUPDUP-WINDOWS)
    (AND (EQ (SEND W ':CONNECTED-P) CONNECTED-P)
         (OR (NULL SUP) (EQ SUP (TV:SHEET-SUPERIOR W)))
         (RETURN W))))

(DEFUN SUPDUP (&OPTIONAL PATH (MODE SUPDUP-MODE))
  "Make a SUPDUP connection to machine specified by PATH.
PATH is a machine name or a string saying how to get to one, such as
<arpanet-gateway><host> or <host>//<contact-name or socket number>
or <gateway><host>//<socket-number>.
If MODE is NIL, SUPDUP runs in (a window substituting for) this window.
Otherwise a separate SUPDUP window is selected."
  (IF MODE
      (SUPDUP-SEPARATE PATH)
      (SUPDUP-BIND PATH)))

(DEFVAR SUPDUP-FLAVOR 'SUPDUP)

(DEFUN SUPDUP-SEPARATE (&OPTIONAL PATH &AUX SW)
  "Switch to a non-connected SUPDUP window and connect it to machine PATH.
If PATH is NIL, a connected SUPDUP window will be selected if there is one."
  (COND ((AND (NULL PATH) (SETQ SW (FIND-SELECTABLE-SUPDUP T NIL)))
         (SEND SW :SELECT)
         NIL)
        (T
         (SETQ SW (OR (FIND-SELECTABLE-SUPDUP NIL) (TV:MAKE-WINDOW SUPDUP-FLAVOR)))
         (SEND SW :SET-CONNECT-TO (OR PATH *SUPDUP-DEFAULT-PATH* SI:ASSOCIATED-MACHINE))
         (SEND SW :EXPOSE NIL :CLEAN)           ;Don't come up with old garbage
         (SEND SW :SELECT)
         T)))

(TV:DEFWINDOW-RESOURCE SUPDUP-WINDOWS ()
  :INITIAL-COPIES 0
  :MAKE-WINDOW (SUPDUP :TYPEIN-PROCESS NIL :TYPEOUT-PROCESS NIL))

(DEFMETHOD (SUPDUP :SETUP) (WINDOW IN-P OUT-P)
  (LEXPR-SEND SELF ':SET-EDGES (MULTIPLE-VALUE-LIST (SEND WINDOW ':EDGES)))
  (SETQ ALIAS-WINDOW WINDOW)
  (SETQ TYPEOUT-PROCESS OUT-P
        TYPEIN-PROCESS IN-P)
  (SEND TYPEOUT-PROCESS :PRESET SELF ':TYPEOUT-TOP-LEVEL))

(DEFUN SUPDUP-BIND (&OPTIONAL (PATH SI:ASSOCIATED-MACHINE)
                    (WINDOW (SEND TERMINAL-IO ':ALIAS-FOR-SELECTED-WINDOWS)))
  "Enter SUPDUP connection to machine specified by PATH, /"in/" this window.
The I//O is done in a window that overlies the one that is TERMINAL-IO.
Network Q exits SUPDUP and returns from this function."
  (USING-RESOURCE (SUPDUP-WINDOW SUPDUP-WINDOWS)
    (USING-RESOURCE (TP TYPEOUT-PROCESSES)
      (SEND SUPDUP-WINDOW ':SETUP WINDOW CURRENT-PROCESS TP)
      (TV:WITH-SELECTION-SUBSTITUTE (SUPDUP-WINDOW WINDOW)
        (SEND SUPDUP-WINDOW ':CONNECT PATH)
        (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit SUPDUP.")
          (SEND SUPDUP-WINDOW ':TYPEIN-TOP-LEVEL NIL))
;       (SETF (TV:SHEET-BIT-ARRAY WINDOW) NIL)
        T))))

(DEFMETHOD (BASIC-SUPDUP :BEFORE :INIT) (INIT-PLIST)
  (SETQ TV:LABEL "FOO")
  (PUTPROP INIT-PLIST NIL :MORE-P))

(DEFMETHOD (BASIC-SUPDUP :BEFORE :SELECT) (&REST IGNORE)
  ;Move ourselves to the head of the list
  (WITHOUT-INTERRUPTS
    (SETQ SUPDUP-WINDOWS (DELQ SELF SUPDUP-WINDOWS))
    (PUSH SELF SUPDUP-WINDOWS)))

(DEFMETHOD (BASIC-SUPDUP :BEFORE :DEACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS (SETQ SUPDUP-WINDOWS (DELQ SELF SUPDUP-WINDOWS))))

(DEFMETHOD (BASIC-SUPDUP :AFTER :ACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
    (OR (MEMQ SELF SUPDUP-WINDOWS)
        (IF SUPDUP-WINDOWS
            (RPLACD (LAST SUPDUP-WINDOWS) (NCONS SELF))
          (SETQ SUPDUP-WINDOWS (NCONS SELF))))))

(DEFMETHOD (BASIC-SUPDUP :VERIFY-NEW-EDGES) (NEW-LEFT NEW-TOP NEW-WIDTH NEW-HEIGHT)
  (DECLARE (IGNORE NEW-LEFT NEW-TOP))
  (AND CONNECTION
       (OR (/= NEW-WIDTH TV:WIDTH) (/= NEW-HEIGHT TV:HEIGHT))
       "Attempt to change size while connected"))

;;; If more processing is turned on, a disaster results!
;;; The output process gets stuck and you can't type input at it to unhang it.
(DEFMETHOD (BASIC-SUPDUP :SET-MORE-P) (IGNORE)
  NIL)

(DEFMETHOD (BASIC-SUPDUP :CONNECT) (&OPTIONAL PATH (NET-WINDOW 3))
  (MULTIPLE-VALUE-BIND (HOST protocol CONTACT CONTACT-P)
      (PARSE-PATH PATH "SUPDUP" :chaos)
    ;; If the host runs the WAITS operating system, it will require char i/d.
    (LET ((SUPDUP-%TOCID (COND ((TYPEP HOST 'SI:HOST) (MEMQ (SEND HOST ':SYSTEM-TYPE)
                                                            '(:MULTICS :WAITS)))
                               ((STRINGP HOST)          ;No host name server.  flush this when
                                (MEM 'EQUALP HOST '("SAIL" "S1-A")))  ;above thing wins.
                               (T SUPDUP-%TOCID))))
      (SEND SELF :NEW-CONNECTION HOST protocol CONTACT CONTACT-P NET-WINDOW))))

(DEFMETHOD (BASIC-SUPDUP :GOBBLE-GREETING) ()
  (SEND-TTY-VARIABLES STREAM SELF NIL OVERPRINT)
  (SEND-FINGER-STRING STREAM)
  ;;Print out the greeting message ITS sends in ASCII.
  (DO ((CH #/CR (SEND STREAM :TYI)))
      ((OR (NULL CH) (= CH 210)))       ;The end is marked with a %TDNOP, NIL is eof
    (AND (< CH #o40) (SETQ CH (+ #o200 CH)))
    (OR (= CH #o212)                    ;Don't type linefeeds (ITS sends CRLFs).
        (TYO CH SELF))))

;;; In case you forgot...
;;;
;;; Bit Name      Value (pdp10 style)      This terminal ...
;;;
;;; %toers        40000,,0                 can ERase Selectively
;;; %tomvb        10000,,0                 can MoVe cursor Backwards
;;; %tosai         4000,,0                 has the SAIL (Stanford AI Lab/ITS/LispM) character set
;;; %toovr         1000,,0                 can OVeRprint
;;; %tomvu          400,,0                 can MoVe cursor Upwards
;;; %tolwr          200,,0                 can print in LoWeR case
;;; %tofci           10,,0                 can generate control and meta characters (12 bit form)
;;; %tolid            2,,0                 can do Line Insert and Delete operations
;;; %tocid            1,,0                 can do Character Insert and Delete operations

;;; Send the initial information describing the Lisp Machine as an
;;; intelligent terminal.  The TTYOPT word contains the following:
;;; %TOERS+%TOMVB+%TOSAI+%TOOVR+%TOMVU+%TOLWR+%TOFCI+%TOMOR+%TOLID,,%TPCBS+%TPORS+%TPRSC
;;; Furthermore, if SUPDUP-%TOCID is non-NIL, then %TOCID will be on as well.
;;; It is off by default, because the Lispm is so fast at outputting characters
;;; that EMACS is effectively faster for the user without CID capability
;;; [BUT, the network is often so slow that speed of output is not even a factor;
;;; speed of input is. So put the CID capability back on by default. -mhd@lmi, 7/21/86]
;;;
;;; SUPDUPing to SAIL and using SUPDUP-OUTPUT are kludged to bind this (SUPDUP-%TOCID)
;;; to t.


(DEFVAR SUPDUP-%TOCID T)

(DEFUN SEND-TTY-VARIABLES (STREAM SHEET LOCAL-EDIT-FLAG OVERPRINT)
    (18BIT-OUT STREAM -6)               ;First word LH has minus the count of following wds.
    (18BIT-OUT STREAM 0)
    (18BIT-OUT STREAM 0)                ;TCTYP word must be %TNSFW: 0,,7
    (18BIT-OUT STREAM 7)
    (18BIT-OUT STREAM
               (+ #o(+ 40000
                       10000
                        4000

                         400
                         200
                          10
                           2

                           )
                  (IF OVERPRINT #o1000 0)
                  (IF SUPDUP-%TOCID 1 0)))      ;TTYOPT word explained above.
    (18BIT-OUT STREAM #o54)
    (18BIT-OUT STREAM 0)                ;TCMXV
    (18BIT-OUT STREAM (1- (TRUNCATE (TV:SHEET-INSIDE-HEIGHT SHEET)
                                    (TV:SHEET-LINE-HEIGHT SHEET))))
    (18BIT-OUT STREAM 0)                ;TCMXH
    (18BIT-OUT STREAM (1- (TRUNCATE (TV:SHEET-INSIDE-WIDTH SHEET)
                                    (TV:SHEET-CHAR-WIDTH SHEET))))
    (18BIT-OUT STREAM 0)                ;TTYROL
    (18BIT-OUT STREAM 0)                ;No scrolling
    (18BIT-OUT STREAM (+ (LSH (TV:SHEET-LINE-HEIGHT SHEET) 10.) ;TTYSMT
                         (LSH (TV:SHEET-CHAR-WIDTH SHEET) 6)
                         #o55))
    (18BIT-OUT STREAM (+ #o040000 (IF LOCAL-EDIT-FLAG (+ #o100000
;Don't turn on line-saving.  It slows TECO a lot to think about it. - RMS
                                                       0 ;(LSH 5 11.)
                                                       ) 0)))
    (SEND STREAM ':FORCE-OUTPUT))

(DEFUN 18BIT-OUT (STREAM N)
    (SEND STREAM ':TYO (LDB #o1406 N))
    (SEND STREAM ':TYO (LDB #o0606 N))
    (SEND STREAM ':TYO (LDB #o0006 N)))

;;;Send the string to TELSER saying where we are, so that NAME can find it inside
;;;the TELSER and print it.  Boy, what a kludge.
(DEFUN SEND-FINGER-STRING (STREAM)
  (SEND STREAM ':TYO #o300)                     ;SUPDUP escape string meaning that the FINGER
  (SEND STREAM ':TYO #o302)                     ;identification string follows.
  (SEND STREAM ':STRING-OUT SI:LOCAL-FINGER-LOCATION)
  (SEND STREAM ':TYO 0)                         ; End with a 0.
  (SEND STREAM ':FORCE-OUTPUT))

(DEFMETHOD (BASIC-SUPDUP :AFTER :DISCONNECT) ()
  (SEND SELF ':SET-LABEL (FORMAT NIL "~A -- not connected" TV:NAME)))

(DEFVAR SUPDUP-KEYS (MAKE-ARRAY #o201 ':TYPE 'ART-16B))
(FILLARRAY SUPDUP-KEYS '(4177                   ;integral
                         0    4102 4103 32      ;null, break, clear, call
                         4101 37   4110 177     ;esc, backnext, help, rubout
                         10   11   12   13      ;bs, tab, lf, vt
                         14   15   4102 4113    ;form, cr, quote, hold-output
                         37   4111 4112 4115    ;stop-output, abort, resume, status
                         4114 0    0    0    0  ;end, I, II, III, IV
                         0    0    0    0    0  ;up, down, left, right, system, network
                         4102))                 ;system

(DEFMETHOD (BASIC-SUPDUP :NET-OUTPUT-TRANSLATED) (CH)
  (UNLESS (CONSP CH)
    (LET ((CHAR (CHAR-CODE CH)))
      (SEND SELF :NET-OUTPUT (LOGIOR (LSH (CHAR-BITS CH) 7)
                                     (COND ((= CHAR #o33) CHAR) ;(Special case)
                                           ((< CHAR #o40) (LOGIOR CHAR #o4000))
                                           ((< CHAR #o177) CHAR)
                                           (T (AREF SUPDUP-KEYS (- CHAR #o177)))))))))

;;; This sends a character of the ITS 12-bit character set to the network,
;;; using the ITS Intelligent Terminal Protocol to get the extra bits through.
(DEFMETHOD (BASIC-SUPDUP :NET-OUTPUT) (CH &AUX BITS)
  (SETQ BITS (LDB #o0705 CH))
  (COND ((NOT (ZEROP BITS))
         (LOCK-OUTPUT
           (SEND STREAM :TYO #o34)
           (SEND STREAM :TYO (LOGIOR #o100 BITS))
           (SEND STREAM :TYO (LOGAND #o177 CH))))
        ((= CH #o34)
         (LOCK-OUTPUT
           (SEND STREAM :TYO #o34)
           (SEND STREAM :TYO CH)))
        (T (LOCK-OUTPUT (SEND STREAM :TYO CH)))))

(DEFMETHOD (BASIC-SUPDUP :LOGOUT) ()
  (LOCK-OUTPUT
    (SEND STREAM :TYO #o300)
    (SEND STREAM :TYO #o301)
    (SEND STREAM :FINISH)))

;;; Dispatch table for the %TD codes.
(DEFVAR SUPDUP-%TD-DISPATCH (MAKE-ARRAY 40))

(FILLARRAY SUPDUP-%TD-DISPATCH
   '(SUPDUP-TDMOV SUPDUP-TDMV0 TV:SHEET-CLEAR-EOF TV:SHEET-CLEAR-EOL TV:SHEET-CLEAR-CHAR
;;;  %TDMOV       %TDMV0       %TDEOF             %TDEOL             %TDDLF

     SUPDUP-NOTHING SUPDUP-GT40 TV:SHEET-CRLF SUPDUP-TDBS SUPDUP-TDBS SUPDUP-TDLF
;;;  %TDMTF         %TDMTN      %TDCRL        %TDNOP         %TDBS          %TDLF

     SUPDUP-TDCR SUPDUP-TDORS SUPDUP-TDQOT TV:SHEET-SPACE SUPDUP-TDMV0 SUPDUP-CLEAR
;;;  %TDCR          %TDORS       %TDQOT       %TDFS    %TDMV0       %TDCLR

     SUPDUP-BEEP    SUPDUP-NOTHING SUPDUP-INSERT-LINE SUPDUP-DELETE-LINE
;;;  %TDBEL         %TDINI         %TDILP             %TDDLP

     SUPDUP-INSERT-CHAR SUPDUP-DELETE-CHAR SUPDUP-TDBOW SUPDUP-RESET SUPDUP-GRAPHICS
;;;  %TDICP             %TDDCP             %TDBOW         %TDRST         %TDGRF
     SUPDUP-REGION-UP SUPDUP-REGION-DOWN
;;;  %TDRSU             %TDRSD

;;; PTV compatibility hacks (ARDS, etc.)
     SUPDUP-NOTHING SUPDUP-ARDS-SET
;;;  %TDGXT         %TDLNG

     SUPDUP-ARDS-LONG   SUPDUP-ARDS-SHORT
;;;  %TDLV              %TDSV
     ))

(DEFMETHOD (BASIC-SUPDUP :BUFFERED-TYO) SUPDUP-BUFFERED-TYO)

(DEFUN SUPDUP-BUFFERED-TYO (IGNORE CH)
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (COND ((< CH #o200)
         (DO () ((VECTOR-PUSH CH OUTPUT-BUFFER))
           (SEND SELF :FORCE-OUTPUT)))
        (T
         (SEND SELF :FORCE-OUTPUT)
         (OR (>= (SETQ CH (- CH #o200)) (ARRAY-LENGTH SUPDUP-%TD-DISPATCH))
             (FUNCALL (AREF SUPDUP-%TD-DISPATCH CH) SELF)))))

;;;Handle %TDMOV by ignoring two characters and then acting as if it were a %TDMV0.
(DEFUN SUPDUP-TDMOV (SHEET)
  (NVT-NETI)
  (NVT-NETI)
  (SUPDUP-TDMV0 SHEET))

;;;Handle %TDMV0 or %TDMV1 by moving the cursor.  This is kludgey because
;;;ITS sends out positions as VPOS followed by HPOS.
(DEFUN SUPDUP-TDMV0 (SHEET &AUX YPOS)
  (DECLARE (:SELF-FLAVOR TV:WINDOW))
  (SETQ YPOS (* (NVT-NETI) TV:LINE-HEIGHT))
  (TV:SHEET-SET-CURSORPOS SHEET
                          (* (NVT-NETI) TV:CHAR-WIDTH)
                          YPOS))

(DEFUN SUPDUP-TDBS (SHEET)
  (SEND SHEET ':BACKWARD-CHAR))

(DEFUN SUPDUP-TDCR (SHEET)
  (TV:SHEET-SET-CURSORPOS SHEET 0 (NTH-VALUE 1 (TV:SHEET-READ-CURSORPOS SHEET))))

(DEFUN SUPDUP-TDLF (SHEET)
  (DECLARE (:SELF-FLAVOR TV:WINDOW))
  (SEND SHEET ':INCREMENT-CURSORPOS 0 TV:LINE-HEIGHT))

;;; This "null function" is used for codes which we should ignore.
(DEFUN SUPDUP-NOTHING (IGNORE) NIL)

;;; Handle %TDORS.  Just tell ITS where the cursor position is, using the
;;; Intelligent Terminal Protocol's ^\ ^P command.
(DEFUN SUPDUP-TDORS (SHEET &AUX VPOS HPOS)
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (MULTIPLE-VALUE (HPOS VPOS)
    (TV:SHEET-READ-CURSORPOS SHEET))
  (LOCK-OUTPUT
    (SEND STREAM ':TYO #o34)                    ;^\
    (SEND STREAM ':TYO #o20)                    ;^P
    (SEND STREAM ':TYO (TRUNCATE VPOS TV:LINE-HEIGHT))
    (SEND STREAM ':TYO (TRUNCATE HPOS TV:CHAR-WIDTH))
    (SEND STREAM ':FORCE-OUTPUT)))

;;; %TDQOT means the next character should be quoted.
(DEFUN SUPDUP-TDQOT (SHEET)
  (TV:SHEET-TYO SHEET (NVT-NETI)))

;;; %TDBEL means to ring the "bell".
;;; To avoid gross obnoxosity, we merge multiple consecutive beeps into one
(DEFUN SUPDUP-BEEP (IGNORE)
  (DECLARE (:SELF-FLAVOR BASIC-NVT))
  (SEND SELF ':REMOTE-BEEP)
  (DO ((CH (SEND STREAM ':TYI-NO-HANG) (SEND STREAM ':TYI-NO-HANG)))
      ((OR (NULL CH) (/= CH #o221))
       (AND CH (SEND STREAM ':UNTYI CH)))))


;;; Display list array.
(DEFVAR GT40-DISPLAY-LIST (MAKE-ARRAY 10. ':TYPE 'ART-Q-LIST))
(DEFVAR GT40-BLINKER NIL)
(DEFVAR GT40-CURRENT-ITEM-NUMBER)
(DEFVAR SUDS-KBD-NEW-TABLE                      ;allows thumb keys to be used
  (LET ((TBL (SI:KBD-MAKE-NEW-TABLE)))
    (DOLIST (L '((#o176 #// #/ #// #/) (#o106 #/\ #/| #/\ #/|)
                 (#o117 #/[ #/{ #/[ #/{) (#o17 #/] #/} #/] #/})))
      (LET ((NCH (FIRST L)) (LCH (REST1 L)))
        (DOTIMES (I 5)
          (ASET (CAR LCH) TBL I NCH)
          (IF (REST1 LCH) (SETQ LCH (REST1 LCH))))))
    TBL))

;;; %TDCLR
(DEFUN SUPDUP-CLEAR (SHEET)
  (TV:SHEET-CLEAR SHEET)
  (FILLARRAY GT40-DISPLAY-LIST '(NIL)))

;;; %TDILP means to insert lines, takes one arg from stream which is number of lines to insert
;;; Lines are inserted at current VPOS.  The current line is affected.
(DEFUN SUPDUP-INSERT-LINE (SHEET)
  (TV:SHEET-INSERT-LINE SHEET (NVT-NETI)))

;;; %TDDLP means to delete lines, takes one arg from stream which is the number of lines.
;;; Affects the current line.
(DEFUN SUPDUP-DELETE-LINE (SHEET)
  (TV:SHEET-DELETE-LINE SHEET (NVT-NETI)))

;;; %TDRSU, %TDRSD followed by height, n-lines
(DEFUN SUPDUP-REGION-UP (SHEET &OPTIONAL
                         (REGION-HEIGHT (NVT-NETI))
                         (SCROLL-AMOUNT (NVT-NETI)))
  (TV:PREPARE-SHEET (SHEET)
    (LET ((ARRAY (TV:SHEET-SCREEN-ARRAY SHEET))
          (WIDTH (TV:SHEET-INSIDE-WIDTH SHEET))
          (LINE-HEIGHT (TV:SHEET-LINE-HEIGHT SHEET))
          REGION-BOTTOM
          DELTA-HEIGHT)
      (SETQ REGION-HEIGHT (* REGION-HEIGHT LINE-HEIGHT)
            REGION-BOTTOM (MIN (+ (TV:SHEET-CURSOR-Y SHEET) REGION-HEIGHT)
                               (* LINE-HEIGHT
                                  (TRUNCATE (TV:SHEET-INSIDE-BOTTOM SHEET)
                                            LINE-HEIGHT)))
            REGION-HEIGHT (- REGION-BOTTOM (TV:SHEET-CURSOR-Y SHEET))
            SCROLL-AMOUNT (MIN (* SCROLL-AMOUNT LINE-HEIGHT) REGION-HEIGHT))
      ;; Get size of region to BLT up
      (SETQ DELTA-HEIGHT (- REGION-HEIGHT SCROLL-AMOUNT))
      (OR (<= DELTA-HEIGHT 0)                   ;If some bits to move, move them
          (BITBLT TV:ALU-SETA
                  WIDTH DELTA-HEIGHT
                  ARRAY (TV:SHEET-INSIDE-LEFT SHEET)
                  (+ (TV:SHEET-CURSOR-Y SHEET) SCROLL-AMOUNT)
                  ARRAY (TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET)))
      (TV:%DRAW-RECTANGLE WIDTH SCROLL-AMOUNT
                          (TV:SHEET-INSIDE-LEFT SHEET)
                          (- REGION-BOTTOM SCROLL-AMOUNT)
                          (TV:SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SUPDUP-REGION-DOWN (SHEET &OPTIONAL
                           (REGION-HEIGHT (NVT-NETI))
                           (SCROLL-AMOUNT (NVT-NETI)))
  (TV:PREPARE-SHEET (SHEET)
    (LET ((ARRAY (TV:SHEET-SCREEN-ARRAY SHEET))
          (WIDTH (TV:SHEET-INSIDE-WIDTH SHEET))
          (LINE-HEIGHT (TV:SHEET-LINE-HEIGHT SHEET))
          REGION-BOTTOM
          DELTA-HEIGHT)
      (SETQ REGION-HEIGHT (* REGION-HEIGHT LINE-HEIGHT)
            REGION-BOTTOM (MIN (+ (TV:SHEET-CURSOR-Y SHEET) REGION-HEIGHT)
                               (* LINE-HEIGHT
                                  (TRUNCATE (TV:SHEET-INSIDE-BOTTOM SHEET)
                                            LINE-HEIGHT)))
            REGION-HEIGHT (- REGION-BOTTOM (TV:SHEET-CURSOR-Y SHEET))
            SCROLL-AMOUNT (MIN (* SCROLL-AMOUNT LINE-HEIGHT) REGION-HEIGHT))
      ;; Get negative size of region to BLT down
      (SETQ DELTA-HEIGHT (- SCROLL-AMOUNT REGION-HEIGHT))
      (OR (>= DELTA-HEIGHT 0)                   ;If some bits to move, move them
          (BITBLT TV:ALU-SETA
                  WIDTH DELTA-HEIGHT
                  ARRAY (TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET)
                  ARRAY (TV:SHEET-INSIDE-LEFT SHEET)
                  (+ (TV:SHEET-CURSOR-Y SHEET) SCROLL-AMOUNT)))
      (TV:%DRAW-RECTANGLE WIDTH SCROLL-AMOUNT
                          (TV:SHEET-INSIDE-LEFT SHEET) (TV:SHEET-CURSOR-Y SHEET)
                          (TV:SHEET-ERASE-ALUF SHEET) SHEET))))

;;; %TDICP insert character positions, takes an arg.
(DEFUN SUPDUP-INSERT-CHAR (SHEET)
  (TV:SHEET-INSERT-CHAR SHEET (NVT-NETI)))

;;; %TDDCP delete character positions, takes an arg.
(DEFUN SUPDUP-DELETE-CHAR (SHEET)
  (TV:SHEET-DELETE-CHAR SHEET (NVT-NETI)))

(DEFUN SUPDUP-TDBOW (SHEET)
  (SEND SHEET ':SET-BLACK-ON-WHITE T))

(DEFUN SUPDUP-RESET (SHEET)
  (SEND SHEET ':SET-BLACK-ON-WHITE NIL)
  (GRAPHICS-RESET SHEET))

;;;; GT40 Simulator (used with the DEC simulator on I.T.S. for running SUDS)

;;; This crock maintains a display list for writing, erasing, and moving display objects
;;; consisting of characters, vectors, and points.  This protocol is not documented
;;; anywhere except in the code for DECUUO.

;; Dispatch table for the GT40 simulator.  These functions take one argument, the pc-ppr.
(DEFVAR GT40-DISPATCH (MAKE-ARRAY 17))
(FILLARRAY GT40-DISPATCH
           '(GT40-INSERT-OR-DELETE
             GT40-INSERT
             GT40-DELETE
;            GT40-RESET
;            GT40-TURN-ON
;            GT40-TURN-OFF
;            GT40-COPY
;            GT40-MOVE
;            GT40-MODE
;            GT40-APPEND
;            GT40-SUBROUTINIZE
;            GT40-UNSUBROUTINIZE
             SUPDUP-NOTHING))                   ;most are not used by DECUUO

;;; %TDMTN is a crock for simulating GT-40's, used by DECUUO on ITS for Imlacs...

(DEFUN SUPDUP-GT40 (SHEET &AUX (BYTE (- (NVT-NETI) 100)))
  (IF (/= (AREF SUDS-KBD-NEW-TABLE 0 #o176)     ;crock for thumb keys, only when
         (AREF SI:KBD-NEW-TABLE 0 #o176))               ;doing GT40 simulation
      (SETQ SI:KBD-NEW-TABLE SUDS-KBD-NEW-TABLE))
  (OR (< BYTE 0)
      (SEND (AREF GT40-DISPATCH (LOGAND 17 BYTE)) SHEET)))

;;; Macros used below to pack characters into words, decode vector formats, etc.

;;; Make a 16-bit "word" from 3 chars in 6-4-6 format
(DEFMACRO GT40-WORD ()
  '(DPB (NVT-NETI) #o0006
        (DPB (NVT-NETI) #o0604
             (DPB (NVT-NETI) #o1206 0))))

;;; Get a word count
(DEFMACRO GT40-COUNT () '(LSH (- (GT40-WORD) 5) -1))

;;; Used in constructing display objects - used only in GT40-INSERT.
(DEFMACRO APUSH (DOB ITEM) `(ARRAY-PUSH-EXTEND ,DOB ,ITEM 500.))

;;; Compute the index of the last thing pushed
(DEFMACRO GT40-LAST-INDEX (DOB) `(1- (ARRAY-ACTIVE-LENGTH ,DOB)))

;;; Get the last item pushed onto a display object
(DEFMACRO GT40-LAST-ITEM (DOB) `(AREF ,DOB (GT40-LAST-INDEX ,DOB)))

;;; Short vector format
(DEFMACRO GT40-SHORT (DOB WORD)
  `(PROGN
     (APUSH ,DOB (* (LDB 0706 ,WORD) (IF (BIT-TEST #o20000 ,WORD) -1 1)))
     (APUSH ,DOB (* (LDB 0006 ,WORD) (IF (BIT-TEST #o100 ,WORD) -1 1)))
     (APUSH ,DOB (BIT-TEST 40000 ,WORD))))

;;; Long vector format
(DEFMACRO GT40-LONG (DOB WORD1 WORD2)
   `(LET ((WORD2 ,WORD2))
      (APUSH ,DOB (* (LOGAND #o1777 ,WORD1) (IF (BIT-TEST #o20000 ,WORD1) -1 1)))
      (APUSH ,DOB (* (LOGAND #o1777 WORD2) (IF (BIT-TEST #o20000 WORD2) -1 1)))
      (APUSH ,DOB (BIT-TEST #o40000 ,WORD1))))

;;; Coordinate scaling macro
(DEFMACRO GT40-COORD (X) `(MAX 0 (TRUNCATE (* 7 ,X) 10.)))

;;; Draw a string.  Note special end of line hackery.  XPOS and YPOS must be symbols.
(DEFMACRO GT40-DRAW-STRING (STRING XPOS YPOS SHEET)
  `(LET ((MAX-Y 750.))
     (SEND ,SHEET ':STRING-OUT-EXPLICIT ,STRING
           (GT40-COORD ,XPOS) (- MAX-Y (GT40-COORD ,YPOS) 11.)
           (TV:SHEET-INSIDE-RIGHT ,SHEET) NIL
           (TV:SHEET-CURRENT-FONT ,SHEET)
           TV:ALU-XOR)))

;;; Draw a vector.  XPOS and YPOS must be symbols
(DEFMACRO GT40-DRAW-VECTOR (XPOS YPOS X Y FLAG SHEET)
  `(LET ((MAX-Y 750.) (OXPOS ,XPOS) (OYPOS ,YPOS))
     (SETQ ,XPOS (+ ,XPOS ,X) ,YPOS (+ ,YPOS ,Y))
     (IF ,FLAG
         (TV:PREPARE-SHEET (,SHEET)
           (TV:%DRAW-LINE (GT40-COORD OXPOS) (- MAX-Y (GT40-COORD OYPOS))
                          (GT40-COORD ,XPOS) (- MAX-Y (GT40-COORD ,YPOS))
                          TV:ALU-XOR NIL ,SHEET)))))

;;; Read a vector out of the display list and draw it
(DEFMACRO GT40-VECTOR (DOB XPOS YPOS SHEET)
  `(LET ((I (GT40-LAST-INDEX ,DOB)))
     (GT40-DRAW-VECTOR
      ,XPOS ,YPOS
      (AREF ,DOB (- I 2)) (AREF ,DOB (- I 1))   ;new x y
      (AREF ,DOB I) ,SHEET)))                   ;visibility flag

;;; Display list format:  The display list is an ART-Q array of display objects, each of
;;; which is, in turn, an ART-Q array.  The format of display objects is a sequence of
;;; display items.  A display item is either a single string of characters or an in-line
;;; subsequence consisting of a symbol describing the item-type followed by 2 numbers (x,y)
;;; and a visibility flag.  Numbers and flags are repeated until a new symbol is encountered
;;; indicating a type change.

;;; GT40 Command 0 - Insert or delete display items
(DEFUN GT40-INSERT-OR-DELETE (SHEET)
  (SELECTQ (LOGAND 3 (GT40-WORD))               ;only 1 and 2 are recognized for now
    (1 (GT40-INSERT SHEET))                     ;insert a new display item
    (2 (GT40-DELETE SHEET (1+ (GT40-COUNT)))))) ;delete n items

;;; GT40 Command 1 - Insert a display item into the display list.
(DEFUN GT40-INSERT (SHEET &AUX (WORD-COUNT (GT40-COUNT)))
  (GT40-DELETE SHEET 1 NIL)             ;Delete the item we are about to insert
  (DO ((I 0 (1+ I))                     ;Loop over words, contructing a display list
       (WORD)(MODE -1)                  ;Mode is initially undefined.
       (XPOS 0) (YPOS 0) (BLINK-THIS)
       (DOB                             ;Display OBject
        (OR (AREF GT40-DISPLAY-LIST GT40-CURRENT-ITEM-NUMBER)   ;Already an array or
            (ASET (MAKE-ARRAY 200.
                              ':LEADER-LIST '(0 NIL)) ;cons an array with leader
                  GT40-DISPLAY-LIST GT40-CURRENT-ITEM-NUMBER))))        ;and install it
      ((>= I WORD-COUNT)
       (IF (= 0 MODE)                   ; was char mode, display the string
           (GT40-DRAW-STRING (GT40-LAST-ITEM DOB) XPOS YPOS SHEET))
       (IF BLINK-THIS (STORE-ARRAY-LEADER 'ON DOB 1)))
    (SETQ WORD (GT40-WORD))
    (COND ((BIT-TEST #o100000 WORD)     ;If command, only look at blink bit and mode
           (IF (NOT (BIT-TEST 40000 WORD))      ;ignore words with the 40000 bit on
               (LET ((NMODE (LDB 1303 WORD))
                     (BLINK-FLAG (AND (BIT-TEST 20 WORD) (BIT-TEST 10 WORD))))
                  (COND ((NOT (= MODE NMODE))   ;get the new datatype mode
                         (IF (= 0 MODE) ; was char mode, display the string
                             (GT40-DRAW-STRING (GT40-LAST-ITEM DOB) XPOS YPOS SHEET))
                         (SETQ MODE NMODE)
                         (APUSH DOB (SELECTQ MODE       ;initializings
                                      (0 (MAKE-ARRAY 10.
                                                     ':TYPE 'ART-STRING
                                                     ':LEADER-LIST '(0)))
                                      (1 'VECTOR)
                                      (2 'VECTOR)
                                      (3 'POINT)
                                      (6 'RPOINT)
                                      ((4 5 7) 'UNKNOWN)))))
                  (COND (BLINK-FLAG
                         (OR (MEMQ GT40-BLINKER (TV:SHEET-BLINKER-LIST SHEET))
                             (SETQ GT40-BLINKER
                                   (TV:MAKE-BLINKER SHEET 'GT40-BLINKER)))
                         (SETQ BLINK-THIS T))))))
          (T (SELECTQ MODE
               (0 (DO ((CHAR (LDB #o0007 WORD) (LDB 1007 WORD)) ;character mode
                       (STRING (GT40-LAST-ITEM DOB))
                       (I 0 (1+ I)))
                      ((= I 2))
                      (OR (= 0 CHAR) (= 17 CHAR) (ARRAY-PUSH-EXTEND STRING CHAR))))
               (1 (GT40-SHORT DOB WORD) ;short vector
                  (GT40-VECTOR DOB XPOS YPOS SHEET))
               (2 (SETQ I (1+ I))       ;long vector
                  (GT40-LONG DOB WORD (GT40-WORD))
                  (GT40-VECTOR DOB XPOS YPOS SHEET))
               (3 (SETQ I (1+ I))       ;point data
                  (GT40-LONG DOB WORD (GT40-WORD))
                  (LET ((I (GT40-LAST-INDEX DOB)))
                       (SETQ XPOS (AREF DOB (- I 2))
                             YPOS (AREF DOB (- I 1)))
                       (GT40-DRAW-VECTOR XPOS YPOS 0 0 (AREF DOB I) SHEET)))
               (4)                      ;graphplot x data (not used)
               (5)                      ;graphplot y data (not used)
               (6 (GT40-SHORT DOB WORD) ;relative point data
                  (LET ((I (GT40-LAST-INDEX DOB)))
                       (SETQ XPOS (+ XPOS (AREF DOB (- I 2)))
                             YPOS (+ YPOS (AREF DOB (- I 1))))
                       (GT40-DRAW-VECTOR XPOS YPOS 0 0 (AREF DOB I) SHEET)))
               (7)))))                  ;not used
  (GT40-WORD))                          ;gobble the checksum

;;; GT40 Command 2 - Delete a display item from the display list
(DEFUN GT40-DELETE (SHEET &OPTIONAL (NITEMS 1) (CHECKSUM-FLAG T))
  (DO ((I 0 (1+ I)) (DOB) (ITEM-NUMBER))
      ((>= I NITEMS))
    (SETQ ITEM-NUMBER (GT40-WORD)
          GT40-CURRENT-ITEM-NUMBER ITEM-NUMBER  ;record item # being hacked
          DOB (AREF GT40-DISPLAY-LIST ITEM-NUMBER))
    (IF DOB (PROGN (OR (EQ 'OFF (ARRAY-LEADER DOB 1))   ;don't erase if its already off
                       (GT40-DISPLAY-ITEM DOB SHEET))
                   (FILLARRAY DOB '(NIL))
                   (STORE-ARRAY-LEADER 0 DOB 0) ;zero the fill pointer
                   (STORE-ARRAY-LEADER NIL DOB 1))))    ;blinking is off
  (IF CHECKSUM-FLAG (GT40-WORD)))               ;gobble the checksum

;;; Display a display item.
(DEFUN GT40-DISPLAY-ITEM (DOB SHEET)
  (DO ((I 0 (1+ I))
       (END (ARRAY-ACTIVE-LENGTH DOB))
       (ITEM) (X) (Y) (FLAG) (XPOS 0) (YPOS 0))
      ((>= I END))
    (SETQ ITEM (AREF DOB I))
    (COND ((STRINGP ITEM) (GT40-DRAW-STRING ITEM XPOS YPOS SHEET))
          ((EQ 'UNKNOWN ITEM))          ;ignore
          (T (DO NIL
                 ((OR (<= (- END I) 3)
                      (SYMBOLP (AREF DOB (1+ I)))
                      (STRINGP (AREF DOB (1+ I)))))
                 (SETQ I (+ 3 I)
                       X    (AREF DOB (- I 2))
                       Y    (AREF DOB (- I 1))
                       FLAG (AREF DOB I))
                 (SELECTQ ITEM
                   (VECTOR (GT40-DRAW-VECTOR XPOS YPOS X Y FLAG SHEET))
                   (POINT (SETQ XPOS X YPOS Y)
                          (GT40-DRAW-VECTOR XPOS YPOS 0 0 FLAG SHEET))
                   (RPOINT (SETQ XPOS (+ XPOS X) YPOS (+ YPOS Y))
                           (GT40-DRAW-VECTOR XPOS YPOS 0 0 FLAG SHEET))))))))

(DEFFLAVOR GT40-BLINKER () (TV:BLINKER))

;;; Blink a display item
(DEFMETHOD (GT40-BLINKER :BLINK) ()
  (LETF ((TV:PHASE NIL))
    (DO ((ITEM (G-L-P GT40-DISPLAY-LIST) (CDR ITEM))
         (BLINK-FLAG NIL NIL)
         (DITEM))
        ((NULL ITEM))
      (SETQ DITEM (CAR ITEM))
      (IF DITEM (SETQ BLINK-FLAG (ARRAY-LEADER DITEM 1)))
      (IF (MEMQ BLINK-FLAG '(ON OFF))
          (PROGN (GT40-DISPLAY-ITEM DITEM TV:SHEET)
                 (STORE-ARRAY-LEADER (SELECTQ BLINK-FLAG (ON 'OFF) (OFF 'ON)) DITEM 1))))))

(DEFMETHOD (GT40-BLINKER :SIZE) ()
  (VALUES (TV:SHEET-INSIDE-WIDTH TV:SHEET) (TV:SHEET-INSIDE-HEIGHT TV:SHEET)))

;;;; ARDS simulator (for compatibility with PTV's)

;;; Todo: these variables should be instance variables
;;;       scaling and offset doesn't work right in this version...
;;;       SHOULD SEND LINE DRAWING MESSAGES RATHER THAN CALLING %DRAW-LINE

(DEFVAR ARDS-XPOS 0)                            ;current pos in ARDS coordinates
(DEFVAR ARDS-YPOS 0)
(DEFVAR ARDS-SCALE 1.0)
(DEFVAR ARDS-SCR-XPOS 0)                        ;current pos in screen coordinates
(DEFVAR ARDS-SCR-YPOS 0)

;;; Setup scaling and offsets, then loop until exit condition
(DEFMACRO ARDS-LOOP (&REST BODY)
  `(LET* ((ARDS-MAX-X (+ TV:X-OFFSET TV:WIDTH))
          (ARDS-MAX-Y (+ TV:Y-OFFSET TV:HEIGHT))
          (ARDS-X-OFFSET TV:X-OFFSET)
          (ARDS-Y-OFFSET TV:Y-OFFSET)
          (ARDS-SCR-SCALE (* ARDS-SCALE (// (MIN TV:WIDTH TV:HEIGHT) 1023.0)))
          (ARDS-CENTER-OFFSET (TRUNCATE (1+ (- (MAX TV:WIDTH TV:HEIGHT) (MIN TV:WIDTH TV:HEIGHT)))
                                        2))
          (ARDS-FLAG NIL))
     (IF (< TV:WIDTH TV:HEIGHT) (SETQ ARDS-MAX-Y (- ARDS-MAX-Y ARDS-CENTER-OFFSET))
         (SETQ ARDS-X-OFFSET (+ ARDS-X-OFFSET ARDS-CENTER-OFFSET)))
     (CATCH 'ARDS-RETURN
       (DO-FOREVER ,@BODY))))

;;; Convert -512./511. to 0/1023. and scale if the user wants it.
(DEFMACRO ARDS-COORD (X)
  `(MAX 1 (FIX (+ .5 (* ARDS-SCR-SCALE (+ 512. ,X))))))

;;; Get a character and punt out of graphics mode if it is a control char or %TD code
(DEFMACRO ARDS-GET ()
  '(LET ((X (NVT-NETI)))
     (IF (OR (< X 100) (> X 177))
         (THROW 'ARDS-RETURN
                (PROGN (SEND STREAM ':UNTYI X)
                       (TV:SHEET-SET-CURSORPOS SHEET
                                               ARDS-SCR-XPOS
                                               (- ARDS-SCR-YPOS 11.)))))
     X))

;;; Unpack long and short format coordinates
(DEFMACRO ARDS-LONG (F)
  `(LET ((A (ARDS-GET)) (B (ARDS-GET)))
     ,(IF F '(SETQ ARDS-FLAG (NOT (BIT-TEST B 40))))
     (* (IF (BIT-TEST A 1) -1 1)
        (LOGIOR (LSH (LOGAND 77 A) -1) (LSH (LOGAND 37 B) 5)))))

(DEFMACRO ARDS-SHORT ()
  `(LET ((A (ARDS-GET)))
     (SETQ ARDS-FLAG T)
     (* (IF (BIT-TEST A 1) -1 1)
        (LSH (LOGAND 77 A) -1))))

;;; Draw a vector
(DEFMACRO ARDS-VECTOR (DX DY)
  `(LET ((X0 ARDS-XPOS) (Y0 ARDS-YPOS))
     (SETQ ARDS-XPOS (+ ARDS-XPOS ,DX)
           ARDS-YPOS (+ ARDS-YPOS ,DY)
           ARDS-SCR-XPOS (MIN ARDS-MAX-X (+ ARDS-X-OFFSET (ARDS-COORD ARDS-XPOS)))
           ARDS-SCR-YPOS (MAX ARDS-Y-OFFSET (- ARDS-MAX-Y (ARDS-COORD ARDS-YPOS))))
     (IF ARDS-FLAG
         (TV:PREPARE-SHEET (SHEET)
           (TV:%DRAW-LINE (MIN ARDS-MAX-X (+ ARDS-X-OFFSET (ARDS-COORD X0)))
                          (MAX ARDS-Y-OFFSET (- ARDS-MAX-Y (ARDS-COORD Y0)))
                          ARDS-SCR-XPOS
                          ARDS-SCR-YPOS
                          TV:ALU-IOR
                          T
                          SHEET)))))

(DEFUN SUPDUP-ARDS-SET (SHEET)
  (DECLARE (:SELF-FLAVOR BASIC-SUPDUP))
  (ARDS-LOOP
    (SETQ ARDS-XPOS (ARDS-LONG T) ARDS-YPOS (ARDS-LONG NIL))
    (ARDS-VECTOR 0 0)))         ;for plotting points

(DEFUN SUPDUP-ARDS-LONG (SHEET)
  (DECLARE (:SELF-FLAVOR BASIC-SUPDUP))
  (ARDS-LOOP (ARDS-VECTOR (ARDS-LONG T) (ARDS-LONG NIL))))

(DEFUN SUPDUP-ARDS-SHORT (SHEET)
  (DECLARE (:SELF-FLAVOR BASIC-SUPDUP))
  (ARDS-LOOP (ARDS-VECTOR (ARDS-SHORT) (ARDS-SHORT))))


;;; This is a sort of SUPDUP which records the current contents of the screen
;;; at all times in a vector of lines.  Each line is a string of fixed length
;;; whose unused chars at the end are all filled with #/RETURN.

;;; It is building block for the SUPDUP which does local editing operations.

;;; SCREEN-LINE-ARRAY is an array of strings, one per screen line.
;;; Each string is as long as the width of the terminal.
;;; It has a leader containing a fill pointer (always constant)
;;; and two other slots, which record whether the line begins
;;; or ends with a continuation.
(DEFSUBST SCREEN-LINE-BEG-CONTINUED (VPOS)
  (ARRAY-LEADER (AREF SCREEN-LINE-ARRAY VPOS) 1))

(DEFSUBST SCREEN-LINE-END-CONTINUED (VPOS)
  (ARRAY-LEADER (AREF SCREEN-LINE-ARRAY VPOS) 2))

(DEFFLAVOR RECORDING-SUPDUP (SCREEN-LINE-ARRAY OLD-SCREEN-LINE-ARRAY
                             (CURRENT-XPOS 0) (CURRENT-YPOS 0)
                             HEIGHT-IN-LINES
                             (FIRST-COL-TO-SAVE 0)
                             LAST-COL-TO-SAVE+1
                             (LINE-LABEL-MAX 2000)
                             REDISPLAY-STRING
                             SAVED-LINE-ARRAY
                             (MULTI-POS-CHAR-BEG 0)
                             (MULTI-POS-CHAR-END 0)
                             (ALLOW-LOCAL-EDITING NIL))
           (SUPDUP))

(DEFMETHOD (RECORDING-SUPDUP :AFTER :CHANGE-OF-SIZE-OR-MARGINS)
           (&REST IGNORE)
  (SETQ HEIGHT-IN-LINES (1- (TRUNCATE (TV:SHEET-INSIDE-HEIGHT SELF) (TV:SHEET-LINE-HEIGHT SELF))))
  (LET ((WIDTH (TRUNCATE (TV:SHEET-INSIDE-WIDTH SELF) (TV:SHEET-CHAR-WIDTH SELF))))
    ;; Record one line which doesn't appear on the screen.
    ;; This is so that line saving can be used to save a line
    ;; which isn't actually needed now, in case it is needed later.
    (SETQ SCREEN-LINE-ARRAY (MAKE-ARRAY (1+ HEIGHT-IN-LINES)))
    (SETQ OLD-SCREEN-LINE-ARRAY (MAKE-ARRAY HEIGHT-IN-LINES))
    (SETQ LAST-COL-TO-SAVE+1 WIDTH)
    (SETQ SAVED-LINE-ARRAY (MAKE-ARRAY LINE-LABEL-MAX))
    (SETQ REDISPLAY-STRING (MAKE-ARRAY WIDTH ':TYPE ART-FAT-STRING ':LEADER-LENGTH 1))
    (DOTIMES (I (1+ HEIGHT-IN-LINES))
      (SETF (AREF SCREEN-LINE-ARRAY I)
            (MAKE-ARRAY WIDTH ':TYPE ART-FAT-STRING ':LEADER-LENGTH 3))
      (SETF (ARRAY-LEADER (AREF SCREEN-LINE-ARRAY I) 0) WIDTH))))

(DEFMETHOD (RECORDING-SUPDUP :AFTER :INIT) (&REST IGNORE)
  (SEND SELF ':CHANGE-OF-SIZE-OR-MARGINS))

;This is what the recording supdup puts in a character position
;to record the fact that a %TDTSP was used to space over that position.
;We use code 211 so that its word-syntax will be correct.
(DEFVAR TAB-PLACEHOLDER 211)

;;;Dispatch table for the %TD codes.
(DEFVAR REC-SUPDUP-%TD-DISPATCH (MAKE-ARRAY 100))
(FILLARRAY REC-SUPDUP-%TD-DISPATCH
   '(SUPDUP-TDMOV SUPDUP-TDMV0 REC-SUPDUP-EOF REC-SUPDUP-EOL REC-SUPDUP-DLF
;;;  %TDMOV       %TDMV0       %TDEOF         %TDEOL         %TDDLF

     SUPDUP-NOTHING SUPDUP-GT40 REC-SUPDUP-CRLF SUPDUP-NOTHING SUPDUP-NOTHING SUPDUP-NOTHING
;;;  %TDMTF         %TDMTN      %TDCRL          %TDNOP         %TDBS          %TDLF

     SUPDUP-NOTHING SUPDUP-TDORS SUPDUP-TDQOT TV:SHEET-SPACE SUPDUP-TDMV0 REC-SUPDUP-CLEAR
;;;  %TDCR          %TDORS       %TDQOT       %TDFS          %TDMV0       %TDCLR

     SUPDUP-BEEP    SUPDUP-NOTHING REC-SUPDUP-INSERT-LINE REC-SUPDUP-DELETE-LINE
;;;  %TDBEL         %TDINI         %TDILP                 %TDDLP

     REC-SUPDUP-INSERT-CHAR REC-SUPDUP-DELETE-CHAR SUPDUP-TDBOW SUPDUP-RESET SUPDUP-GRAPHICS
;;;  %TDICP                 %TDDCP                 %TDBOW         %TDRST       %TDGRF

     REC-SUPDUP-REGION-UP REC-SUPDUP-REGION-DOWN
;;;  %TDRSU             %TDRSD

;;; PTV compatibility hacks (ARDS, etc.)
     SUPDUP-NOTHING SUPDUP-ARDS-SET
;;;  %TDGXT         %TDLNG

     SUPDUP-ARDS-LONG   SUPDUP-ARDS-SHORT
;;;  %TDLV              %TDSV

     L-E-SUPDUP-RESYNCH-REPLY-RECEIVED
;;;  %TDSYN

     L-E-SUPDUP-ALLOW-LOCAL-EDITING
;;;  %TDECO

     L-E-SUPDUP-DEFINE-COMMAND
;;;  %TDEDF

     L-E-SUPDUP-STOP-LOCAL-EDITING
;;;  %TDNLE

     REC-SUPDUP-SPACE-FOR-TAB
;;;  %TDTSP

     REC-SUPDUP-LINE-BEG-CONTINUED REC-SUPDUP-LINE-END-CONTINUED
;;;  %TDCTB                        %TDCTE

     REC-SUPDUP-MULTI-POS-CHAR
;;;  %TDMLT

     REC-SUPDUP-SAVE-LINES REC-SUPDUP-RESTORE-LINES
;;;  %TDSVL                %TDSVL

     REC-SUPDUP-SET-SAVING-RANGE REC-SUPDUP-SET-LOCAL-LABEL
;;;  %TDSSR                      %TDSLL
     ))

(DEFMETHOD (RECORDING-SUPDUP :BUFFERED-TYO) (CH &AUX LINE)
  (COND ((< CH #o200)
         (SETF (AREF (SETQ LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)) CURRENT-XPOS)
               CH)
         (INCF CURRENT-XPOS)
         ;; Prevent errors storing past end of line
         ;; if remote site sends garbage.
         (AND (= CURRENT-XPOS (ARRAY-LENGTH LINE))
              (DECF CURRENT-XPOS))
         ;; Use top 8 bits of char to indicate start and end
         ;; of multi-position chars.
         ;; Put in 1 for 1st char, 2 for remaining chars.
         (AND (= CURRENT-XPOS MULTI-POS-CHAR-END)
              (DO ((I MULTI-POS-CHAR-BEG (1+ I)))
                  ((= I MULTI-POS-CHAR-END))
                (INCF (AREF (AREF SCREEN-LINE-ARRAY CURRENT-YPOS) I)
                      (IF (= I MULTI-POS-CHAR-BEG)
                          #o400 #o1000))))
         ;; Output below the screen bottom is just for recording.
         ;; Don't try to put the characters up-- it would bomb out.
         (OR (>= CURRENT-YPOS HEIGHT-IN-LINES)
             (DO () ((ARRAY-PUSH OUTPUT-BUFFER CH))
               (SEND SELF ':FORCE-OUTPUT))))
        (T
         (SEND SELF ':FORCE-OUTPUT)
         (SETQ MULTI-POS-CHAR-BEG 0 MULTI-POS-CHAR-END 0)
         (OR (>= (SETQ CH (- CH 200)) (ARRAY-LENGTH REC-SUPDUP-%TD-DISPATCH))
             (SEND (AREF REC-SUPDUP-%TD-DISPATCH CH) SELF))
         (SETQ CURRENT-XPOS (TRUNCATE (- TV:CURSOR-X (TV:SHEET-INSIDE-LEFT SELF))
                                      TV:CHAR-WIDTH)
               CURRENT-YPOS (TRUNCATE (- TV:CURSOR-Y (TV:SHEET-INSIDE-TOP SELF))
                                      TV:LINE-HEIGHT)))))

(DEFUN REC-SUPDUP-EOF (WINDOW)
  (REC-SUPDUP-EOF-1 WINDOW)
  (TV:SHEET-CLEAR-EOF WINDOW))

(DEFUN REC-SUPDUP-EOF-1 (WINDOW &OPTIONAL REGION-END)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (OR REGION-END (SETQ REGION-END (1+ HEIGHT-IN-LINES)))
  (REC-SUPDUP-EOL-1 WINDOW)
  (DO ((I (1+ CURRENT-YPOS) (1+ I)))
      ((= I REGION-END))
    (SETF (SCREEN-LINE-BEG-CONTINUED I) NIL)
    (SETF (SCREEN-LINE-END-CONTINUED I) NIL)
    (FILLARRAY (AREF SCREEN-LINE-ARRAY I) '(#/RETURN))))

(DEFUN REC-SUPDUP-EOL (WINDOW)
  (REC-SUPDUP-EOL-1 WINDOW)
  (TV:SHEET-CLEAR-EOL WINDOW))

(DEFUN REC-SUPDUP-EOL-1 (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (SETF (SCREEN-LINE-END-CONTINUED CURRENT-YPOS) NIL)
  (LET ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)))
    (DO ((XPOS CURRENT-XPOS (1+ XPOS))
         (END (ARRAY-LENGTH LINE)))
        ((= XPOS END))
      (SETF (AREF LINE XPOS) #/RETURN))))

(DEFUN REC-SUPDUP-CRLF (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (TV:SHEET-CRLF WINDOW)
  (SETQ CURRENT-XPOS (TRUNCATE (- TV:CURSOR-X (TV:SHEET-INSIDE-LEFT SELF))
                               TV:CHAR-WIDTH)
        CURRENT-YPOS (TRUNCATE (- TV:CURSOR-Y (TV:SHEET-INSIDE-TOP SELF))
                               TV:LINE-HEIGHT))
  (REC-SUPDUP-EOL-1 WINDOW))

;;; It is ok to store #/RETURN into the erased char unconditionally
;;; because EMACS only uses this when it is about to
;;; write something into the cleared positions.
(DEFUN REC-SUPDUP-DLF (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LET ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)))
    (SETF (AREF LINE CURRENT-XPOS) #/RETURN))
    (TV:SHEET-CLEAR-CHAR WINDOW))

(DEFUN REC-SUPDUP-SPACE-FOR-TAB (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (SETF (AREF (AREF SCREEN-LINE-ARRAY CURRENT-YPOS) CURRENT-XPOS) TAB-PLACEHOLDER)
  (TV:SHEET-SPACE WINDOW))

(DEFUN REC-SUPDUP-CLEAR (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LETF ((CURRENT-XPOS 0) (CURRENT-YPOS 0))
    (REC-SUPDUP-EOF-1 WINDOW))
  (TV:SHEET-CLEAR WINDOW)
  (FILLARRAY GT40-DISPLAY-LIST '(NIL)))         ;Miracle of modularity

(DEFUN REC-SUPDUP-INSERT-LINE (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LET ((N-LINES (NVT-NETI)))
    (LETF ((CURRENT-YPOS (- HEIGHT-IN-LINES N-LINES))
                   (CURRENT-XPOS 0))
      (REC-SUPDUP-EOF-1 WINDOW))
    (COPY-ARRAY-CONTENTS SCREEN-LINE-ARRAY OLD-SCREEN-LINE-ARRAY)
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY CURRENT-YPOS
                        (- HEIGHT-IN-LINES N-LINES)
                        SCREEN-LINE-ARRAY (+ CURRENT-YPOS N-LINES)
                        HEIGHT-IN-LINES)
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY
                        (- HEIGHT-IN-LINES N-LINES)
                        HEIGHT-IN-LINES
                        SCREEN-LINE-ARRAY
                        CURRENT-YPOS (+ CURRENT-YPOS N-LINES))
    (TV:SHEET-INSERT-LINE WINDOW N-LINES)))

(DEFUN REC-SUPDUP-DELETE-LINE (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LET ((N-LINES (NVT-NETI)))
    (COPY-ARRAY-CONTENTS SCREEN-LINE-ARRAY OLD-SCREEN-LINE-ARRAY)
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY (+ CURRENT-YPOS N-LINES)
                        HEIGHT-IN-LINES
                        SCREEN-LINE-ARRAY CURRENT-YPOS
                        (- HEIGHT-IN-LINES N-LINES))
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY
                        CURRENT-YPOS (+ CURRENT-YPOS N-LINES)
                        SCREEN-LINE-ARRAY
                        (- HEIGHT-IN-LINES N-LINES)
                        HEIGHT-IN-LINES)
    (LETF ((CURRENT-YPOS (- HEIGHT-IN-LINES N-LINES))
                   (CURRENT-XPOS 0))
      (REC-SUPDUP-EOF-1 WINDOW))
    (TV:SHEET-DELETE-LINE WINDOW N-LINES)))

(DEFUN REC-SUPDUP-REGION-DOWN (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LET* ((REGION-HEIGHT (NVT-NETI))
         (N-LINES (NVT-NETI))
         (REGION-END (+ REGION-HEIGHT CURRENT-YPOS)))
    (LETF ((CURRENT-YPOS (- REGION-END N-LINES))
                   (CURRENT-XPOS 0))
      (REC-SUPDUP-EOF-1 WINDOW REGION-END))
    (COPY-ARRAY-CONTENTS SCREEN-LINE-ARRAY OLD-SCREEN-LINE-ARRAY)
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY
                        CURRENT-YPOS (- REGION-END N-LINES)
                        SCREEN-LINE-ARRAY
                        (+ CURRENT-YPOS N-LINES) REGION-END)
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY
                        (- REGION-END N-LINES) REGION-END
                        SCREEN-LINE-ARRAY
                        CURRENT-YPOS (+ CURRENT-YPOS N-LINES))
    (SUPDUP-REGION-DOWN WINDOW REGION-HEIGHT N-LINES)))

(DEFUN REC-SUPDUP-REGION-UP (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LET* ((REGION-HEIGHT (NVT-NETI))
         (N-LINES (NVT-NETI))
         (REGION-END (+ CURRENT-YPOS REGION-HEIGHT)))
    (COPY-ARRAY-CONTENTS SCREEN-LINE-ARRAY OLD-SCREEN-LINE-ARRAY)
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY
                        (+ CURRENT-YPOS N-LINES) REGION-END
                        SCREEN-LINE-ARRAY
                        CURRENT-YPOS (- REGION-END N-LINES))
    (COPY-ARRAY-PORTION OLD-SCREEN-LINE-ARRAY
                        CURRENT-YPOS (+ CURRENT-YPOS N-LINES)
                        SCREEN-LINE-ARRAY
                        (- REGION-END N-LINES) REGION-END)
    (LETF ((CURRENT-YPOS (- REGION-END N-LINES))
                   (CURRENT-XPOS 0))
      (REC-SUPDUP-EOF-1 WINDOW REGION-END))
    (SUPDUP-REGION-UP WINDOW REGION-HEIGHT N-LINES)))

(DEFUN REC-SUPDUP-INSERT-CHAR (WINDOW &OPTIONAL COUNT)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (OR COUNT (SETQ COUNT (NVT-NETI)))
  (TV:SHEET-INSERT-CHAR WINDOW COUNT)
  (LET ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)))
    (DO ((I (1- (ARRAY-LENGTH LINE)) (1- I)))
        ((< I CURRENT-XPOS))
      (SETF (AREF LINE I)
            (IF (< (- I COUNT) CURRENT-XPOS)
                #/SPACE
              (AREF LINE (- I COUNT)))))))

(DEFUN REC-SUPDUP-DELETE-CHAR (WINDOW &OPTIONAL COUNT)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (OR COUNT (SETQ COUNT (NVT-NETI)))
  (TV:SHEET-DELETE-CHAR WINDOW COUNT)
  (LET ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)))
    (DO ((I CURRENT-XPOS (1+ I))
         (END (ARRAY-LENGTH LINE)))
        ((= I END))
      (SETF (AREF LINE I)
            (IF (>= (+ I COUNT) END)
                #/RETURN
              (AREF LINE (+ I COUNT)))))))

(DEFUN REC-SUPDUP-REDISPLAY (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (TV:SHEET-CLEAR WINDOW)
  (DO ((I 0 (1+ I))
       (END HEIGHT-IN-LINES)
       LINE-END)
      ((= I END))
    (SEND WINDOW ':SET-CURSORPOS 0 I ':CHARACTER)
    (SETQ LINE-END (STRING-REVERSE-SEARCH-NOT-CHAR #/RETURN (AREF SCREEN-LINE-ARRAY I)))
    (COND (LINE-END
           (COPY-ARRAY-PORTION (AREF SCREEN-LINE-ARRAY I) 0 (1+ LINE-END)
                               REDISPLAY-STRING 0 (1+ LINE-END))
           (SETF (ARRAY-LEADER REDISPLAY-STRING 0) (1+ LINE-END))
           (DO ((XPOS 0 (1+ XPOS)))
               ((> XPOS LINE-END))
             (LET ((CH (LOGAND 377 (AREF REDISPLAY-STRING XPOS))))
               (AND (OR (= CH TAB-PLACEHOLDER)
                        (= CH #/RETURN))
                    (SETF (AREF REDISPLAY-STRING XPOS) #/SPACE))))
           (SEND WINDOW ':STRING-OUT REDISPLAY-STRING))))
  (SEND WINDOW ':SET-CURSORPOS CURRENT-XPOS CURRENT-YPOS ':CHARACTER))

(DEFUN REC-SUPDUP-LINE-BEG-CONTINUED (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (SETF (SCREEN-LINE-BEG-CONTINUED CURRENT-YPOS) T))

(DEFUN REC-SUPDUP-LINE-END-CONTINUED (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (SETF (SCREEN-LINE-END-CONTINUED CURRENT-YPOS) T))

(DEFUN REC-SUPDUP-MULTI-POS-CHAR (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (SETQ MULTI-POS-CHAR-BEG CURRENT-XPOS)
  (SETQ MULTI-POS-CHAR-END (+ MULTI-POS-CHAR-BEG (NVT-NETI)))
  (NVT-NETI))

(DEFMETHOD (RECORDING-SUPDUP :ALLOW-LOCAL-EDITING) ()
  NIL)

(DEFMETHOD (RECORDING-SUPDUP :DEFINE-COMMAND) ()
  (NVT-NETI) (NVT-NETI))

(DEFUN REC-SUPDUP-SAVE-LINES (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (LET ((N-LINES (NVT-NETI))
        (LABEL (+ (NVT-NETI) (LSH (NVT-NETI) 7)))
        (HEIGHT (1+ HEIGHT-IN-LINES)))
    (DO ((I CURRENT-YPOS (1+ I))
         (N N-LINES (1- N)))
        ((OR (= I HEIGHT) (<= N 0)))
      (LET ((LINE (AREF SCREEN-LINE-ARRAY I)))
        (OR (AREF SAVED-LINE-ARRAY LABEL)
            (SETF (AREF SAVED-LINE-ARRAY LABEL)
                  (MAKE-ARRAY (ARRAY-LENGTH LINE) ':TYPE ART-STRING)))
        (FILLARRAY (AREF SAVED-LINE-ARRAY LABEL) '(#/RETURN))
        (COPY-ARRAY-PORTION LINE FIRST-COL-TO-SAVE LAST-COL-TO-SAVE+1
                            (AREF SAVED-LINE-ARRAY LABEL)
                            FIRST-COL-TO-SAVE LAST-COL-TO-SAVE+1))
      (SETQ LABEL (LOGAND (1- LINE-LABEL-MAX) (1+ LABEL))))))

(DEFUN REC-SUPDUP-RESTORE-LINES (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  (LET ((N-LINES (NVT-NETI))
        (LABEL (+ (NVT-NETI) (LSH (NVT-NETI) 7)))
        (HEIGHT (1+ HEIGHT-IN-LINES)))
    (DO ((I CURRENT-YPOS (1+ I))
         (N N-LINES (1- N)))
        ((OR (= I HEIGHT) (<= N 0)))
      (LET ((LINE (AREF SCREEN-LINE-ARRAY I))
            (SAVED-LINE (AREF SAVED-LINE-ARRAY LABEL))
            LINE-END)
        (COND (SAVED-LINE
               (COPY-ARRAY-PORTION SAVED-LINE FIRST-COL-TO-SAVE LAST-COL-TO-SAVE+1
                                   LINE FIRST-COL-TO-SAVE LAST-COL-TO-SAVE+1)
               ;; Output changes onto screen, unless now off-screen.
               (COND ((< I HEIGHT-IN-LINES)
                      (SEND WINDOW ':SET-CURSORPOS FIRST-COL-TO-SAVE I ':CHARACTER)
                      (TV:SHEET-CLEAR-EOL WINDOW)
                      (SETQ LINE-END (STRING-REVERSE-SEARCH-NOT-CHAR #/RETURN LINE))
                      (COND (LINE-END
                             (COPY-ARRAY-PORTION (AREF SCREEN-LINE-ARRAY I) 0 (1+ LINE-END)
                                                 REDISPLAY-STRING 0 (1+ LINE-END))
                             (SETF (ARRAY-LEADER REDISPLAY-STRING 0) (1+ LINE-END))
                             (DO ((XPOS 0 (1+ XPOS)))
                                 ((> XPOS LINE-END))
                               (LET ((CH (LOGAND 377 (AREF REDISPLAY-STRING XPOS))))
                                 (AND (OR (= CH TAB-PLACEHOLDER)
                                          (= CH #/RETURN))
                                      (SETF (AREF REDISPLAY-STRING XPOS) #/SPACE))))
                             (SEND WINDOW ':STRING-OUT REDISPLAY-STRING))))))))
      (SETQ LABEL (LOGAND (1- LINE-LABEL-MAX) (1+ LABEL))))
    (SEND WINDOW ':SET-CURSORPOS CURRENT-XPOS CURRENT-YPOS ':CHARACTER)))

(DEFUN REC-SUPDUP-SET-SAVING-RANGE (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (SETQ FIRST-COL-TO-SAVE (NVT-NETI))
  (SETQ LAST-COL-TO-SAVE+1 (NVT-NETI)))

;;; No need to support this until we support local handling
;;; of something that can move lines off the screen.
;;; But do pass by the arguments.
(DEFUN REC-SUPDUP-SET-LOCAL-LABEL (WINDOW)
  (DECLARE (:SELF-FLAVOR RECORDING-SUPDUP))
  WINDOW
  (NVT-NETI) (NVT-NETI))

(DEFCONST TOP-BIT #o4000)

(DEFFLAVOR LOCAL-EDITING-SUPDUP
        (INPUT-CHAR-COUNT LAST-RESYNCH-CHAR-COUNT LAST-RESYNCH-CODE
         RESYNCH-REPLY-CODE RESYNCH-REPLY-CHAR-COUNT
         (SEND-RESYNCH-NOW NIL)
         (LOCAL-EDITING-ENABLE NIL)
         (LOCAL-EDIT-METER 0)
         (CHAR-TABLE (MAKE-ARRAY 2000))
         (WORD-SYNTAX-TABLE (MAKE-ARRAY 200 ':TYPE ART-1B))
         (TOP-EDITING-MARGIN 0) (BOTTOM-EDITING-MARGIN 0)
         (LEFT-EDITING-MARGIN 0) (RIGHT-EDITING-MARGIN 0)
         (INSERT-MODE 'INSERT))
        (RECORDING-SUPDUP))

(DEFMETHOD (LOCAL-EDITING-SUPDUP :AFTER :SET-CONNECTION) (&REST IGNORE)
  (SETQ INPUT-CHAR-COUNT 0 LOCAL-EDIT-METER 0 LAST-RESYNCH-CODE NIL
        SEND-RESYNCH-NOW T
        ALLOW-LOCAL-EDITING NIL
        LOCAL-EDITING-ENABLE NIL))

(DEFVAR BEEP-ON-LOCAL-EDIT NIL)

(DEFVAR INPUT-CHAR-IN-SUPDUP-CODE)

;Given a Lispm keyboard character to "send" to the server,
;first we consider processing it locally if that is enabled now.
(DEFMETHOD (LOCAL-EDITING-SUPDUP :NET-OUTPUT-TRANSLATED) (CH)
  (COND ((CONSP CH)
         (SEND SELF ':NET-OUTPUT CH))
        (T
         ;; If we aren't doing local echoing now, but could do it,
         ;; then send a resynch every so often, so that we are never
         ;; more than 200. or so input chars past a resynch
         ;; unless we are already local editing.
         ;; This is so that the #-chars-since-resynch in a %TDSYN
         ;; always fits in one character.

         ;; Send the resynch BEFORE the input char
         ;; so we don't pre-empt TECO output by mistake.
         (COND ((AND ALLOW-LOCAL-EDITING
                     (NOT LOCAL-EDITING-ENABLE)
                     (OR SEND-RESYNCH-NOW
                         (> (- INPUT-CHAR-COUNT LAST-RESYNCH-CHAR-COUNT) 200.)))
                (SEND SELF ':NET-OUTPUT (+ TOP-BIT #/S))
                (COND ((NULL LAST-RESYNCH-CODE)
                       (SETQ LAST-RESYNCH-CODE 40))
                      ((= LAST-RESYNCH-CODE 176)
                       (SETQ LAST-RESYNCH-CODE 40))
                      (T (INCF LAST-RESYNCH-CODE)))
                (SEND SELF ':NET-OUTPUT LAST-RESYNCH-CODE)
                (SETQ LAST-RESYNCH-CHAR-COUNT INPUT-CHAR-COUNT)))
         ;; Translate char to SUPDUP char set and send it.
         (LET* ((CHAR (CHAR-CODE CH))
                (INPUT-CHAR-IN-SUPDUP-CODE
                  (LOGIOR (LSH (CHAR-BITS CH) 7)
                          (COND ((= CHAR #o33) CHAR)    ;(Special case)
                                ((< CHAR #o40) (LOGIOR CHAR #o4000))
                                ((< CHAR #o177) CHAR)
                                (T (AREF SUPDUP-KEYS (- CHAR #o177)))))))
           (AND LOCAL-EDITING-ENABLE
                (< INPUT-CHAR-IN-SUPDUP-CODE 1000)
                (IF (FUNCALL (OR (AREF CHAR-TABLE INPUT-CHAR-IN-SUPDUP-CODE) 'NOT-HANDLED)
                             SELF INPUT-CHAR-IN-SUPDUP-CODE)
                    ;; If char has just been echoed here,
                    ;; tell the remote machine that it is a pre-echoed char.
                    (PROGN
                      (AND BEEP-ON-LOCAL-EDIT (BEEP))
                      (INCF LOCAL-EDIT-METER)
                      (AND JOURNAL-STREAM (SEND JOURNAL-STREAM ':STRING-OUT "L:"))
                      (SETQ CURRENT-XPOS (TRUNCATE (- TV:CURSOR-X (TV:SHEET-INSIDE-LEFT SELF))
                                                   TV:CHAR-WIDTH)
                            CURRENT-YPOS (TRUNCATE (- TV:CURSOR-Y (TV:SHEET-INSIDE-TOP SELF))
                                                   TV:LINE-HEIGHT))
                      (SEND SELF ':NET-OUTPUT (+ TOP-BIT #/E))
                      (SEND SELF ':NET-OUTPUT 1))
                    ;; Any char that can't be echoed here
                    ;; turns off local echo.
                    (SETQ LOCAL-EDITING-ENABLE NIL)))
           (SEND SELF ':NET-OUTPUT INPUT-CHAR-IN-SUPDUP-CODE)
           (AND JOURNAL-STREAM (FORMAT JOURNAL-STREAM "~C " CH)))
         (INCF INPUT-CHAR-COUNT))))

(DEFUN NOT-HANDLED (&REST IGNORE) NIL)

;When the remote machine decides we can do local editing,
;it sends us a %TDSYN based on the last resynch we sent.
;The output process comes here and tells the input process
;to go ahead and do local editing (LOCAL-EDITING-ENABLE <- T).
;Local editing stops when LOCAL-EDITING-ENABLE becomes NIL again.
;This can be because the input process sees something it can't handle locally,
;or it can be because more output arrives from the remote machine.
;In the latter case, we assume we are talking to a different program
;which does not understand the local editing protocol, so we turn
;off the use of it, until further notice is received.

;If we receive a resynch reply when we are not expecting one,
;or we get a mismatched reply, then we set flags to send another
;resynch in the hope of unconfusing the other side.
(DEFUN L-E-SUPDUP-RESYNCH-REPLY-RECEIVED (WINDOW)
  (declare (:self-flavor local-editing-supdup))
  (COND (ALLOW-LOCAL-EDITING
         (SETQ RESYNCH-REPLY-CODE (NVT-NETI)
               RESYNCH-REPLY-CHAR-COUNT (+ (NVT-NETI) LAST-RESYNCH-CHAR-COUNT))
         (AND JOURNAL-STREAM (PRINC "Resynch: " JOURNAL-STREAM))
         (IF (AND LAST-RESYNCH-CODE
                  (= RESYNCH-REPLY-CODE LAST-RESYNCH-CODE)
                  (= RESYNCH-REPLY-CHAR-COUNT INPUT-CHAR-COUNT))
             (LETF ((LOCAL-EDITING-ENABLE T))
               (AND JOURNAL-STREAM (PRINC "Enable: " JOURNAL-STREAM))
               (PROCESS-WAIT "LOCAL EDIT"
                             #'(LAMBDA (LOC -STREAM-)
                                 (IF (OR (NOT (CDR LOC))
                                         (LET ((CH (SEND -STREAM- ':TYI-NO-HANG)))
                                           (AND CH (SEND -STREAM- ':UNTYI CH))
                                           CH))
                                     T))
                             (FOLLOW-CELL-FORWARDING
                               (VALUE-CELL-LOCATION LOCAL-EDITING-ENABLE) T)
                             STREAM)
               (AND LOCAL-EDITING-ENABLE
                    (SETQ ALLOW-LOCAL-EDITING NIL)))
           (PROGN (AND JOURNAL-STREAM
                       (FORMAT JOURNAL-STREAM "~D ~D ~D ~D "
                               LAST-RESYNCH-CODE
                               RESYNCH-REPLY-CODE
                               RESYNCH-REPLY-CHAR-COUNT
                               INPUT-CHAR-COUNT))
                  (SETQ SEND-RESYNCH-NOW T))))
        (T (NVT-NETI) (NVT-NETI)
           (SEND WINDOW ':ALLOW-LOCAL-EDITING))))

;This is where we process a %TDECO, which says that we should
;start attempting to use the local editing protocol
;by sending resynchs from time to time.
(DEFUN L-E-SUPDUP-ALLOW-LOCAL-EDITING (WINDOW)
  (SEND WINDOW ':ALLOW-LOCAL-EDITING))

(DEFUN L-E-SUPDUP-STOP-LOCAL-EDITING (WINDOW)
  (SEND WINDOW ':STOP-LOCAL-EDITING))

(DEFMETHOD (LOCAL-EDITING-SUPDUP :ALLOW-LOCAL-EDITING) ()
  (SETQ ALLOW-LOCAL-EDITING T)
  (SETQ SEND-RESYNCH-NOW T))

(DEFMETHOD (LOCAL-EDITING-SUPDUP :STOP-LOCAL-EDITING) ()
  (SETQ ALLOW-LOCAL-EDITING NIL)
  (SETQ LOCAL-EDITING-ENABLE NIL))

(DEFMETHOD (LOCAL-EDITING-SUPDUP :GOBBLE-GREETING) ()
  (SEND-TTY-VARIABLES STREAM SELF T OVERPRINT)
  (SEND-FINGER-STRING STREAM)
  ;;Print out the greeting message ITS sends in ASCII.
  (DO ((CH #/CR (SEND STREAM ':TYI)))
      ((OR (NULL CH) (= CH 210)))       ;The end is marked with a %TDNOP, NIL is eof
    (AND (< CH 40) (SETQ CH (+ 200 CH)))
    (OR (= CH 212)                      ;Don't type linefeeds (ITS sends CRLFs).
        (TYO CH SELF))))

;;;; Editing commands for local-editing-supdup.

;;;%TDEDF nn specifies what a certain character should do, for local editing.
;;;nn represents two 7-bit characters of information, which are merged into
;;;a 14-bit number, which is then divided into its top 5 bits (the function code)
;;;and its bottom 9 bits (which are the character, in SUPDUP code, whose
;;;meaning is being defined).

;;;These are the function codes:

;;; 0 -- random (no remote echo possible)
;;; 1 -- fwd char
;;; 2 -- back char
;;; 3 -- fwd delete
;;; 4 -- back delete
;;; 5 -- back char, no tabs
;;; 6 -- back delete, no tabs
;;; 7 -- self insert or replace
;;; 10 - vert. up.
;;; 11 - vert down.
;;; 12 - vert up, no tabs
;;; 13 - vert down, no tabs
;;; 14 - up to line beginning
;;; 15 - down to line beginning
;;; 16 - insert CRLF after point
;;; 17 - insert CRLF before point
;;; 20 - beg of line
;;; 21 - end of line
;;; 22 - equivalence to another character's definition.
;;;      A char whose low 7 bits are lower case
;;;      equivs to the corresponding upper case char.
;;;      Any other char which has the control bit
;;;      equivs to the char with bits 300 cleared out
;;;      (control-I and control-Tab both go to Tab).
;;; 23 - fwd word
;;; 24 - back word
;;; 25 - fwd del word
;;; 26 - back del word
;;; 27 - arg digit
;;; 30 - begins arg, followed by digits
;;; 31 - specify word syntax of associated character.
;;;      Since only 7-bit chars have a word syntax, the 200 bit is used
;;;      to say what the syntax is: 1 means the character is a separator.
;;; 32 - specify insert vs replace for chars with definition code 7.
;;;      The arg for this command (what is supplied as the "character
;;;      to be defined") says what to do with all characters whose
;;;      definition code is 7.
;;;      An arg of 0 means they cannot be handled at all.
;;;      An arg of 1 means they insert.  2 means they replace.
;;; 33 - reset all characters to an initial state:
;;;      All characters 40 to 176 self-insert except l.c. letters,
;;;      L.c. letters with any combination of control/meta are equivalenced,
;;;      Digits with control and/or meta are are digits,
;;;      All other characters defined as NIL.
;;;      Syntax table: digits and letters (both cases) make up words.
;;;      Insert mode.
;;;      Margins all zero.
;;; 34 - specify right, left, top or bottom margin
;;;      outside of which any text that appears is not text to be edited.
;;;      The "ASCII character" is the margin value.
;;;      The control/meta bits say which margin to set:
;;;      0 - left   1 - top   2 - right   3 - bottom.

(DEFVAR DEFINITION-CODE-TABLE (MAKE-ARRAY 40))
(FILLARRAY DEFINITION-CODE-TABLE
           `(NIL L-E-SUPDUP-FORWARD-CHAR L-E-SUPDUP-BACKWARD-CHAR
             L-E-SUPDUP-FORWARD-DELETE-CHAR L-E-SUPDUP-BACKWARD-DELETE-CHAR
             L-E-SUPDUP-BACKWARD-CHAR-NO-TABS L-E-SUPDUP-BACKWARD-DELETE-CHAR-NO-TABS
             L-E-SUPDUP-INSERT-CHAR
             NIL NIL
;            L-E-SUPDUP-VERTICALLY-UP L-E-SUPDUP-VERTICALLY-DOWN
             NIL NIL
;            L-E-SUPDUP-VERTICALLY-UP-NO-TABS L-E-SUPDUP-VERTICALLY-DOWN-NO-TABS
             NIL NIL
;            L-E-SUPDUP-UP-TO-LINE-BEG L-E-SUPDUP-DOWN-TO-LINE-BEG
             NIL NIL
;            L-E-SUPDUP-CRLF-AFTER-POINT L-E-SUPDUP-CRLF-BEFORE-POINT
             L-E-SUPDUP-BEG-OF-LINE L-E-SUPDUP-END-OF-LINE
             L-E-SUPDUP-EQUIVALENCE
             L-E-SUPDUP-FORWARD-WORD L-E-SUPDUP-BACKWARD-WORD
             L-E-SUPDUP-FORWARD-KILL-WORD L-E-SUPDUP-BACKWARD-KILL-WORD
             NIL NIL
;            L-E-SUPDUP-ARG-DIGIT L-E-SUPDUP-ARG-STARTER
             (L-E-SUPDUP-SET-WORD-SYNTAX) (L-E-SUPDUP-SET-INSERT-MODE)
             (L-E-SUPDUP-INITIALIZE)
             (L-E-SUPDUP-SET-MARGIN)
             NIL))

(DEFUN L-E-SUPDUP-DEFINE-COMMAND (WINDOW)
  (SEND WINDOW ':DEFINE-COMMAND-CHARACTER))

;Process one %TDEDF.
(DEFMETHOD (LOCAL-EDITING-SUPDUP :DEFINE-COMMAND-CHARACTER) ()
  (LET* ((ARG (+ (LSH (NVT-NETI) 7) (NVT-NETI)))
         (CH (LDB 0011 ARG))
         (CODE (LDB 1105 ARG))
         (DEFN (AREF DEFINITION-CODE-TABLE CODE)))
    (IF (ATOM DEFN)
        (SETF (AREF CHAR-TABLE CH) DEFN)
        (SEND (CAR DEFN) CH))))

(DEFUN L-E-SUPDUP-INITIALIZE (IGNORE)
  (DECLARE (:self-flavor LOCAL-EDITING-SUPDUP))
  (FILLARRAY CHAR-TABLE '(NIL))
  (DO ((I 40 (1+ I)))
      ((= I 177))
    (SETF (AREF CHAR-TABLE I) 'L-E-SUPDUP-INSERT-CHAR))
  (DO ((I #/a (1+ I)))
      ((> I #/z))
    (SETF (AREF CHAR-TABLE I) 'L-E-SUPDUP-EQUIVALENCE)
    (SETF (AREF CHAR-TABLE (+ I 200)) 'L-E-SUPDUP-EQUIVALENCE)
    (SETF (AREF CHAR-TABLE (+ I 400)) 'L-E-SUPDUP-EQUIVALENCE)
    (SETF (AREF CHAR-TABLE (+ I 600)) 'L-E-SUPDUP-EQUIVALENCE))
;    (DO ((I #/0 (1+ I)))
;       ((> I #/9))
;      (SETF (AREF CHAR-TABLE (+ I 200)) 'L-E-SUPDUP-ARG-DIGIT)
;      (SETF (AREF CHAR-TABLE (+ I 400)) 'L-E-SUPDUP-ARG-DIGIT)
;      (SETF (AREF CHAR-TABLE (+ I 600)) 'L-E-SUPDUP-ARG-DIGIT))
  (FILLARRAY WORD-SYNTAX-TABLE '(0))
  (DO ((I #/A (1+ I)))
      ((> I #/Z))
    (SETF (AREF WORD-SYNTAX-TABLE I) 1)
    (SETF (AREF WORD-SYNTAX-TABLE (+ I 40)) 1))
  (DO ((I #/0 (1+ I)))
      ((> I #/9))
    (SETF (AREF WORD-SYNTAX-TABLE I) 1))
  (SETQ INSERT-MODE 'INSERT)
  (SETQ LEFT-EDITING-MARGIN 0
        TOP-EDITING-MARGIN 0
        RIGHT-EDITING-MARGIN 0
        BOTTOM-EDITING-MARGIN 0))

(DEFUN L-E-SUPDUP-SET-WORD-SYNTAX (CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  (SETF (AREF WORD-SYNTAX-TABLE (LDB 0007 CH))
        (LDB #o0701 CH)))

(DEFUN L-E-SUPDUP-SET-INSERT-MODE (CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  (SETQ INSERT-MODE
        (SELECTQ CH
          (1 'INSERT)
          (2 'REPLACE))))

(DEFUN L-E-SUPDUP-SET-MARGIN (CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  (SET (NTH (LDB #o0702 CH) '(LEFT-EDITING-MARGIN
                             TOP-EDITING-MARGIN
                             RIGHT-EDITING-MARGIN
                             BOTTOM-EDITING-MARGIN))
       (LDB #o0007 CH)))

;;; Return T if there is a tab in the current line after the cursor.
(DEFUN L-E-SUPDUP-TAB-CHECK ()
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  (LET ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)))
    (%STRING-SEARCH-CHAR TAB-PLACEHOLDER LINE
                         CURRENT-XPOS
                         (ARRAY-LENGTH LINE))))

(DEFUN L-E-SUPDUP-FORWARD-CHAR (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  CH
  (LET ((BUFFER-CHAR (AREF (AREF SCREEN-LINE-ARRAY CURRENT-YPOS) CURRENT-XPOS)))
    (IF (>= CURRENT-XPOS (- (ARRAY-LENGTH (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
                           1
                           RIGHT-EDITING-MARGIN))
        NIL
      (IF (LDB-TEST 1010 BUFFER-CHAR) NIL
        (SELECT BUFFER-CHAR
          (#/RETURN NIL)
          (TAB-PLACEHOLDER NIL)                 ;a tab.
          (T (SEND WINDOW ':SET-CURSORPOS (1+ CURRENT-XPOS) CURRENT-YPOS ':CHARACTER)
             T))))))

(DEFUN L-E-SUPDUP-BACKWARD-CHAR-NO-TABS (WINDOW CH)
  (L-E-SUPDUP-BACKWARD-CHAR WINDOW CH T))

(DEFUN L-E-SUPDUP-BACKWARD-CHAR (WINDOW CH &OPTIONAL NO-TABS)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  CH
  (COND ((<= CURRENT-XPOS LEFT-EDITING-MARGIN) NIL)
        (T
         (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
                (PREV-CHAR (AREF LINE (- CURRENT-XPOS 1))))
           (COND ((LDB-TEST #o1010 PREV-CHAR) NIL)
                 ((AND (NOT NO-TABS)
                       (= PREV-CHAR TAB-PLACEHOLDER))
                  NIL)
                 (T (SEND WINDOW ':SET-CURSORPOS (1- CURRENT-XPOS) CURRENT-YPOS ':CHARACTER)))))))

(DEFUN L-E-SUPDUP-FORWARD-DELETE-CHAR (WINDOW CH)
  (DECLARE (:self-flavor LOCAL-EDITING-SUPDUP)
           (ignore ch))
  (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
         (BUFFER-CHAR (AREF LINE CURRENT-XPOS)))
    (IF (LDB-TEST #o1010 BUFFER-CHAR) NIL
      (SELECT BUFFER-CHAR
        (#/RETURN NIL)
        (TAB-PLACEHOLDER NIL)                   ;a tab.
        (T (COND ((SCREEN-LINE-END-CONTINUED CURRENT-YPOS) NIL)
                 ((L-E-SUPDUP-TAB-CHECK) NIL)
                 (T
                  (REC-SUPDUP-DELETE-CHAR WINDOW 1)
                  T)))))))

(DEFUN L-E-SUPDUP-BACKWARD-DELETE-CHAR-NO-TABS (WINDOW CH)
  (L-E-SUPDUP-BACKWARD-DELETE-CHAR WINDOW CH T))

(DEFUN L-E-SUPDUP-BACKWARD-DELETE-CHAR (WINDOW CH &OPTIONAL NO-TABS)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (ignore ch))
  (COND ((<= CURRENT-XPOS LEFT-EDITING-MARGIN) NIL)
        ((L-E-SUPDUP-TAB-CHECK) NIL)
        ((SCREEN-LINE-END-CONTINUED CURRENT-YPOS) NIL)
        (T
         (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
                (PREV-CHAR (IF (AND (= CURRENT-XPOS (1+ LEFT-EDITING-MARGIN))
                                    (SCREEN-LINE-BEG-CONTINUED CURRENT-YPOS))
                               -1
                             (AREF LINE (- CURRENT-XPOS 1)))))
           (COND ((LDB-TEST 1010 PREV-CHAR) NIL)
                 ((AND (NOT NO-TABS)
                       (= PREV-CHAR TAB-PLACEHOLDER))
                  NIL)
                 (T
                  (SEND WINDOW ':SET-CURSORPOS
                        (SETQ CURRENT-XPOS (1- CURRENT-XPOS))
                        CURRENT-YPOS ':CHARACTER)
                  (REC-SUPDUP-DELETE-CHAR WINDOW 1)
                  T))))))

(DEFUN L-E-SUPDUP-INSERT-CHAR (WINDOW IGNORE &AUX CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  (SETQ CH (LOGAND #o177 INPUT-CHAR-IN-SUPDUP-CODE))
  (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
         (END-XPOS (1+ (OR (STRING-REVERSE-SEARCH-NOT-CHAR #/RETURN LINE) -1))))
    (COND ((SCREEN-LINE-END-CONTINUED CURRENT-YPOS) NIL)
          ((= END-XPOS (- (ARRAY-LENGTH LINE) 1)) NIL)
          ((NULL INSERT-MODE) NIL)
          ((L-E-SUPDUP-TAB-CHECK) NIL)
          ((OR (EQ INSERT-MODE 'INSERT)
               (= END-XPOS CURRENT-XPOS))
           (REC-SUPDUP-INSERT-CHAR WINDOW 1)
           (SEND WINDOW ':BUFFERED-TYO CH)
           (SEND WINDOW ':FORCE-OUTPUT)
           T)
          ((NOT (LDB-TEST 1010 (AREF LINE CURRENT-XPOS)))
           (REC-SUPDUP-DLF WINDOW)
           (SEND WINDOW ':BUFFERED-TYO CH)
           (SEND WINDOW ':FORCE-OUTPUT)
           T))))

(DEFUN L-E-SUPDUP-BEG-OF-LINE (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (ignore ch))
  (COND ((ZEROP CURRENT-YPOS) NIL)
        ((SCREEN-LINE-BEG-CONTINUED CURRENT-YPOS) NIL)
        (T (SEND WINDOW ':SET-CURSORPOS 0 CURRENT-YPOS ':CHARACTER)
           T)))

(DEFUN L-E-SUPDUP-END-OF-LINE (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (ignore ch))
  (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
         (END-XPOS (OR (STRING-REVERSE-SEARCH-NOT-CHAR #/RETURN LINE) -1)))
    (COND ((SCREEN-LINE-END-CONTINUED CURRENT-YPOS) NIL)
          (T (SEND WINDOW ':SET-CURSORPOS (1+ END-XPOS) CURRENT-YPOS ':CHARACTER)
             T))))

(DEFUN L-E-SUPDUP-EQUIVALENCE (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP))
  (PROG (NEW-CH)
        (COND ((= (LOGAND CH 140) 140) (SETQ NEW-CH (LOGXOR CH 40)))
              ;; If control bit is present, clear it and also alphabetic bit.
              ((= (LOGAND CH 200) 200) (SETQ NEW-CH (LOGAND CH 477)))
              (T (RETURN NIL)))
        (RETURN (SEND (OR (AREF CHAR-TABLE NEW-CH) 'NOT-HANDLED) WINDOW NEW-CH))))

(DEFUN L-E-SUPDUP-FORWARD-WORD (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (IGNORE CH))
  (LET (FOUND-WORD
        (LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS)))
    (DO ((I CURRENT-XPOS (1+ I))
         (END (IF (SCREEN-LINE-END-CONTINUED CURRENT-YPOS)
                  (1- (ARRAY-LENGTH LINE))
                (ARRAY-LENGTH LINE))))
        ((= I END) NIL)
      (LET ((CHAR (AREF LINE I)))
        (COND ((LDB-TEST 1010 CHAR) (RETURN NIL))
              ((NOT (ZEROP (AREF WORD-SYNTAX-TABLE
                                 (SETQ CHAR (LDB 0007 CHAR)))))
               (SETQ FOUND-WORD T))
              ;; Is this a separator reached after we passed a word?
              (FOUND-WORD
               (SEND WINDOW ':SET-CURSORPOS I CURRENT-YPOS ':CHARACTER)
               (RETURN T)))))))

(DEFUN L-E-SUPDUP-BACKWARD-WORD (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (IGNORE CH))
  (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
         FOUND-WORD)
    (DO ((I CURRENT-XPOS (1- I)))
        ((= I 0)
         ;; If we reach the beginning of the line,
         ;; that is a fine stopping place for the word
         ;; as long as the previous lineis not continued.
         (AND (NOT (SCREEN-LINE-BEG-CONTINUED CURRENT-YPOS))
              FOUND-WORD
              (PROGN (SEND WINDOW ':SET-CURSORPOS 0 CURRENT-YPOS ':CHARACTER)
                     T)))

      (AND (LDB-TEST 1010 (AREF LINE (1- I)))
           (RETURN NIL))
      (COND ((NOT (ZEROP (AREF WORD-SYNTAX-TABLE (LDB 0007 (AREF LINE (1- I))))))
             (SETQ FOUND-WORD T))
            (FOUND-WORD
             (SEND WINDOW ':SET-CURSORPOS I CURRENT-YPOS ':CHARACTER)
             (RETURN T))))))

(DEFUN L-E-SUPDUP-FORWARD-KILL-WORD (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (IGNORE CH))
  (LET* (FOUND-WORD
         (LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
         (END (ARRAY-LENGTH LINE)))
    (COND ((SCREEN-LINE-END-CONTINUED CURRENT-YPOS) NIL)
          ((L-E-SUPDUP-TAB-CHECK) NIL)
          (T (DO ((I CURRENT-XPOS (1+ I)))
                 ((= I END) NIL)
               (LET ((CHAR (AREF LINE I)))
                 (COND ((LDB-TEST 1010 CHAR) (RETURN NIL))
                       ((NOT (ZEROP (AREF WORD-SYNTAX-TABLE
                                          (SETQ CHAR (LDB 0007 CHAR)))))
                        (SETQ FOUND-WORD T))
                       ;; Is this a separator reached after we passed a word?
                       (FOUND-WORD
                        (REC-SUPDUP-DELETE-CHAR WINDOW (- I CURRENT-XPOS))
                        (RETURN T)))))))))

(DEFUN L-E-SUPDUP-BACKWARD-KILL-WORD (WINDOW CH)
  (DECLARE (:SELF-FLAVOR LOCAL-EDITING-SUPDUP) (IGNORE CH))
  (LET* ((LINE (AREF SCREEN-LINE-ARRAY CURRENT-YPOS))
         (OLD-XPOS CURRENT-XPOS)
         FOUND-WORD)
    (AND (NOT (SCREEN-LINE-END-CONTINUED CURRENT-YPOS))
         (NOT (L-E-SUPDUP-TAB-CHECK))
         (DO ((I CURRENT-XPOS (1- I)))
             ((= I 0)
              ;; If we reach the beginning of the line,
              ;; that is a fine stopping place for the word
              ;; as long as the previous lineis not continued.
              (AND (> CURRENT-YPOS 0)
                   (NOT (SCREEN-LINE-BEG-CONTINUED CURRENT-YPOS))
                   FOUND-WORD
                   (PROGN (SEND WINDOW ':SET-CURSORPOS 0 CURRENT-YPOS ':CHARACTER)
                          (SETQ CURRENT-XPOS 0)
                          (REC-SUPDUP-DELETE-CHAR WINDOW OLD-XPOS)
                          T)))
           (AND (LDB-TEST 1010 (AREF LINE (1- I)))
                (RETURN NIL))
           (COND ((NOT (ZEROP (AREF WORD-SYNTAX-TABLE (LDB 0007 (AREF LINE (1- I))))))
                  (SETQ FOUND-WORD T))
                 (FOUND-WORD
                  (LET ((OLD-XPOS CURRENT-XPOS))
                    (SEND WINDOW ':SET-CURSORPOS I CURRENT-YPOS ':CHARACTER)
                    (SETQ CURRENT-XPOS I)
                    (REC-SUPDUP-DELETE-CHAR WINDOW (- OLD-XPOS I)))
                  (RETURN T)))))))

(DEFFLAVOR BASIC-TELNET
            ((MORE-FLAG NIL)
             (ECHO-FLAG NIL)
             (SIMULATE-IMLAC-FLAG NIL)
             (BINARY-OUTPUT-FLAG NIL)
             (SUPDUP-OUTPUT-FLAG NIL)
             (verbose-flag nil))
            (BASIC-NVT TV:FULL-SCREEN-HACK-MIXIN TV:LIST-MOUSE-BUTTONS-MIXIN)
  (:DEFAULT-INIT-PLIST :PROGRAM-NAME "Telnet")
  (:DOCUMENTATION "A TELNET NVT")
  (:SETTABLE-INSTANCE-VARIABLES SIMULATE-IMLAC-FLAG verbose-flag))

(DEFFLAVOR TELNET () (BASIC-TELNET TV:WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:DOCUMENTATION :COMBINATION))

(DEFUN TELNET (&OPTIONAL PATH (MODE SUPDUP-MODE))
  "Make a TELNET connection to machine specified by PATH.
PATH is a machine name with optional contact name or port number.
<host> or <host>//<contact-name or port number>.
If MODE is NIL, TELNET runs in (a window substituting for) this window.
Otherwise a separate TELNET window is selected."
  (IF MODE
      (TELNET-SEPARATE PATH)
      (TELNET-BIND PATH)))

(DEFUN TELNET-SEPARATE (&OPTIONAL PATH &AUX SW)
  "Switch to a non-connected TELNET window and connect it to machine PATH.
If PATH is NIL, a connected TELNET window will be selected if there is one."
  (COND ((AND (NULL PATH) (SETQ SW (FIND-SELECTABLE-TELNET T NIL)))
         (SEND SW ':SELECT)
         NIL)
        (T
         (SETQ SW (OR (FIND-SELECTABLE-TELNET NIL) (TV:MAKE-WINDOW 'TELNET)))
         (SEND SW ':SET-CONNECT-TO (OR PATH SUPDUP-DEFAULT-PATH
                                          SI:ASSOCIATED-MACHINE))
         (SEND SW ':EXPOSE NIL ':CLEAN) ;Don't come up with old garbage
         (SEND SW ':SELECT)
         T)))

(DEFVAR TELNET-WINDOWS NIL
  "List of all ordinary non-resource TELNET windows.")

(DEFUN FIND-SELECTABLE-TELNET (CONNECTED-P &OPTIONAL (SUP TV:MOUSE-SHEET))
  (DOLIST (W TELNET-WINDOWS)
    (AND (EQ (SEND W ':CONNECTED-P) CONNECTED-P)
         (OR (NULL SUP) (EQ SUP (TV:SHEET-SUPERIOR W)))
         (RETURN W))))

(TV:DEFWINDOW-RESOURCE TELNET-WINDOWS ()
  :INITIAL-COPIES 0
  :MAKE-WINDOW (TELNET :TYPEIN-PROCESS NIL :TYPEOUT-PROCESS NIL))

(DEFMETHOD (TELNET :BEFORE :SELECT) (&REST IGNORE)
  ;Move ourselves to the head of the list
  (WITHOUT-INTERRUPTS
    (SETQ TELNET-WINDOWS (DELQ SELF TELNET-WINDOWS))
    (PUSH SELF TELNET-WINDOWS)))

(DEFMETHOD (TELNET :BEFORE :DEACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS (SETQ TELNET-WINDOWS (DELQ SELF TELNET-WINDOWS))))

(DEFMETHOD (TELNET :AFTER :ACTIVATE) (&REST IGNORE)
  (WITHOUT-INTERRUPTS
    (OR (MEMQ SELF TELNET-WINDOWS)
        (IF TELNET-WINDOWS
            (RPLACD (LAST TELNET-WINDOWS) (NCONS SELF))
          (SETQ TELNET-WINDOWS (NCONS SELF))))))

(DEFMETHOD (TELNET :SETUP) (WINDOW IN-P OUT-P SIMULATE-IMLAC)
  (LEXPR-SEND SELF ':SET-EDGES
              (MULTIPLE-VALUE-LIST (SEND WINDOW ':EDGES)))
  (SETQ ALIAS-WINDOW WINDOW)
  (SETQ TYPEOUT-PROCESS OUT-P
        TYPEIN-PROCESS IN-P
        SIMULATE-IMLAC-FLAG SIMULATE-IMLAC)
  (SEND TYPEOUT-PROCESS :PRESET SELF ':TYPEOUT-TOP-LEVEL))

(DEFUN TELNET-BIND (&OPTIONAL (PATH SI:ASSOCIATED-MACHINE)
               SIMULATE-IMLAC-P (WINDOW (SEND TERMINAL-IO ':ALIAS-FOR-SELECTED-WINDOWS)))
  "Enter TELNET connection to machine specified by PATH.
SIMULATE-IMLAC-P non-NIL says simulate MIT's PDS-1 display codes.
The I//O is done in a window that overlies the one that is TERMINAL-IO."
  (USING-RESOURCE (TELNET-WINDOW TELNET-WINDOWS)
    (USING-RESOURCE (TP TYPEOUT-PROCESSES)
      (SEND TELNET-WINDOW ':SETUP WINDOW CURRENT-PROCESS TP SIMULATE-IMLAC-P)
      (TV:WITH-SELECTION-SUBSTITUTE (TELNET-WINDOW WINDOW)
        (SEND TELNET-WINDOW ':CONNECT PATH)
        (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Exit TELNET.")
          (SEND TELNET-WINDOW ':TYPEIN-TOP-LEVEL NIL))
;       (SETF (TV:SHEET-BIT-ARRAY WINDOW) NIL)
        T))))

(defmethod (basic-telnet :connect) (&optional path (net-window 3) &aux conn)
  (multiple-value-bind (host protocol contact contact-p)
      (parse-path path "TELNET" :internet)
    (when (setq conn (send self :new-connection host protocol contact contact-p net-window))
      (unless (errorp conn)
        (telnet-send-option nvt-do nvt-echo)
        (telnet-send-option nvt-do nvt-suppress-go-ahead)
        (telnet-send-option nvt-will nvt-suppress-go-ahead)
        (setq overprint nil))
      conn)))

(DEFMETHOD (BASIC-TELNET :GOBBLE-GREETING) ()
  (TERPRI SELF))

(DEFMETHOD (BASIC-TELNET :AFTER :DISCONNECT) ()
  (SETQ ECHO-FLAG NIL SUPDUP-OUTPUT-FLAG NIL BINARY-OUTPUT-FLAG NIL)
  (SEND SELF ':SET-LABEL (FORMAT NIL "~A -- not connected" TV:NAME)))

(DEFVAR TELNET-KEYS (MAKE-ARRAY #o200 :TYPE 'ART-16B))
(FILLARRAY TELNET-KEYS
           #o'(0 #/break 25 0                   ;null break clear-input call
               0 0 #/help 177 10 11 12          ;esc back-next help rubout bs tab lf
               13 14 15 26 0                    ;vt form return quote hold-output
               0 #/abort #/resume 0             ;stop-output abort resume status
               0 0 0 0 0 0 0 0 0 0              ;end ...
               0 0))                            ;network

;;;Convert to NVT ASCII (except don't convert CR to two characters).
(defmethod (basic-telnet :net-output-translated) (ch)
  (cond ((consp ch)
         ;;Mouse click?
         )
        (supdup-output-flag
         (let ((char (char-code ch))
               (bits (char-bits ch)))
           (cond ((= ch #o34)
                  (send self :net-output #o34)
                  (send self :net-output ch))
                 ((not (zerop bits))
                  (cond ((and (= bits char-control-bit) ;only control bit
                              (<= #o140 char #o177))    ;And upper case
                         (send self :net-output (logand char #o37)))
                        (t
                         (send self :net-output #o34)
                         (send self :net-output (logior #o100 bits))
                         (send self :net-output char))))
                 (t (send self :net-output ch)))))
        (t
         (let ((char (ldb %%kbd-char ch)))
           (unless echo-flag
             ;; Echo the character.
             (if (ldb-test %%kbd-control ch)
                 (send self :tyo #/up-arrow))
             (send self :tyo char))
           (when (> char #o200)
             (setq char (aref telnet-keys (- char #o200))))
           (when (plusp char)
             (and (ldb-test %%kbd-control ch) (setq char (ldb (byte 5 0) ch)))  ;controlify
             (and (ldb-test %%kbd-super ch) (send self :net-output #o34))       ;control-\
             (and (ldb-test %%kbd-meta ch) (send self :net-output #o33))        ;escape
             (send self :net-output char))))))

(defconstant nvt-subnegotiation-end 360)
(defconstant nvt-nop 361)
(defconstant nvt-dm 362)
(defconstant nvt-break 363)
(defconstant nvt-ip 364)
(defconstant nvt-ao 365)
(defconstant nvt-ayt 366)
(defconstant nvt-ec 367)
(defconstant nvt-el 370)
(defconstant nvt-go-ahead 371)
(defconstant nvt-subnegotiation-begin 372)
(defconstant nvt-will 373)
(defconstant nvt-wont 374)
(defconstant nvt-do 375)
(defconstant nvt-dont 376)
(defconstant nvt-iac 377)

(DEFCONST NVT-TRANSMIT-BINARY 0)
(DEFCONST NVT-ECHO 1)
(DEFCONST NVT-SUPPRESS-GO-AHEAD 3)
(DEFCONST NVT-TIMING-MARK 6)
(DEFCONST NVT-LOGOUT 22)
(DEFCONST NVT-SUPDUP-OUTPUT 26)

(DEFMETHOD (BASIC-TELNET :NET-OUTPUT) (CH)
  (LOCK-OUTPUT
    (COND ((LDB-TEST #o1701 CH)
           (SEND STREAM ':TYO NVT-IAC)
           (SETQ CH (LDB #o0010 CH))))
    (SEND STREAM ':TYO CH)
    (COND ((= CH 15)
           (SEND STREAM ':TYO #o12))            ;CR is two chars
          ((= CH NVT-IAC)
           (SEND STREAM ':TYO #o377)))))        ;IAC's must be quoted

(defmethod (basic-telnet :buffered-tyo) (ch)
  (telnet-buffered-tyo ch))

(defun telnet-buffered-tyo (ch &aux ch1)
  (declare (:self-flavor basic-telnet))
  (COND ((= CH NVT-IAC)
         (SEND SELF ':HANDLE-IAC))              ;Perform new telnet negotiations.
        ((>= CH #o200))                         ;Ignore otelnet negotiations
        ((AND (= CH 7) (NOT (AND SIMULATE-IMLAC-FLAG BINARY-OUTPUT-FLAG)))
         (SEND SELF ':REMOTE-BEEP))             ;^G rings the bell.
        ((AND (= CH #o15)
              (IF (= (SETQ CH1 (NVT-NETI)) #o12)        ;CR LF is NVT newline "character"
                  NIL                           ;Output normally
                  ;; A CR not followed by a LF.  Move the "carriage" to the start of the
                  ;; current line.  Then if the next character is anything other than a NUL,
                  ;; assume the other end if not obeying protocol and output it too.
                  (SEND SELF ':FORCE-OUTPUT)
                  (MULTIPLE-VALUE-BIND (IGNORE Y) (SEND SELF ':READ-CURSORPOS)
                    (SEND SELF ':SET-CURSORPOS 0 Y))
                  (= (SETQ CH CH1) 0))))        ;If NUL, skip any output
        ((AND (= CH #o177) SIMULATE-IMLAC-FLAG) ;Escape character
         (SEND SELF ':HANDLE-IMLAC-ESCAPE))
        ((= ch #o10)                            ;Back Space
         (send self :force-output)
         (send self :backward-char))
        (T
         (AND (memq ch '(#o11 #o12 #o14 #o15))  ;Convert formatting controls
              (NOT (AND SIMULATE-IMLAC-FLAG BINARY-OUTPUT-FLAG))
              (SETQ CH (+ CH 200)))             ;to Lisp machine char set.
         (DO () ((ARRAY-PUSH OUTPUT-BUFFER CH))
           (SEND SELF ':FORCE-OUTPUT)))))

;;; New telnet protocol IAC handler
(DEFMETHOD (BASIC-TELNET :HANDLE-IAC) (&AUX COMMAND OPTION)
  (SETQ COMMAND (NVT-NETI))
  (AND (>= COMMAND NVT-WILL) (<= COMMAND NVT-DONT)
       (SETQ OPTION (NVT-NETI)))
  (when verbose-flag
    (send self :force-output)
    (format self
            "~&Received IAC~@[ ~A~]~@[ ~A~]~%"
            (cadr (assq command telnet:*telsyms*))
            (and option
                 (let ((name (cadr (assq option telnet:*telopts*))))
                   (and name (substring (symbol-name name) 7))))))
  (SELECT COMMAND
    (NVT-WILL
     (SELECT OPTION
       (NVT-SUPPRESS-GO-AHEAD)                  ;ignore things we requested
       (NVT-ECHO
        (telnet-echo t))
       (NVT-TRANSMIT-BINARY
        (SETQ BINARY-OUTPUT-FLAG T)
        (TELNET-SEND-OPTION NVT-DO OPTION))
       (NVT-SUPDUP-OUTPUT
        (TELNET-START-SUPDUP-OUTPUT))
       (nvt-logout
        (telnet-send-option nvt-do option))
       (OTHERWISE
        (TELNET-SEND-OPTION NVT-DONT OPTION))))
    (NVT-DO
     (select option
       (nvt-suppress-go-ahead)                  ;ignore things we requested
       ((NVT-TIMING-MARK NVT-TRANSMIT-BINARY)
        (TELNET-SEND-OPTION NVT-WILL OPTION))
       (T (TELNET-SEND-OPTION NVT-WONT OPTION))))
    (NVT-DONT
     (TELNET-SEND-OPTION NVT-WONT OPTION))
    (NVT-WONT
     (select option
       (NVT-ECHO
        (telnet-echo nil))
       (NVT-TRANSMIT-BINARY
        (SETQ BINARY-OUTPUT-FLAG NIL)
        (TELNET-SEND-OPTION NVT-DONT OPTION))))
    (NVT-SUBNEGOTIATION-BEGIN
     (TELNET-HANDLE-SUBNEGOTIATION))))

(DEFMETHOD (BASIC-TELNET :HANDLE-IMLAC-ESCAPE) (&AUX CH)
  (SEND SELF ':FORCE-OUTPUT)
  (SETQ CH (+ (NVT-NETI) #o176))
  (COND ((= CH 177)
         (LETF ((SIMULATE-IMLAC-FLAG NIL))
           (SEND SELF ':BUFFERED-TYO CH)))
        ((< (SETQ CH (- CH #o200)) (ARRAY-LENGTH SUPDUP-%TD-DISPATCH))
         (FUNCALL (AREF SUPDUP-%TD-DISPATCH CH) SELF))))

;;; Set our idea of who is echoing, and send a DO or DONT,
;;; unless the state is already this way.
;;; The argument to TELNET-ECHO is the new value of ECHO-FLAG,
;;; which is NIL for local echo (the official default) and T for remote echo.
;;; So (TELNET-ECHO T) means that we want remote echoing.
(defun telnet-echo (on-p)
  (declare (:self-flavor basic-telnet))
  (unless (eq echo-flag on-p) ;If not the right way already
    (setq echo-flag on-p)
    (when verbose-flag
      (send self :force-output)
      (format self "~&Setting ~:[local~;remote~] echo~%" on-p))))

(defun telnet-send-command (command)
  (declare (:self-flavor basic-telnet))
  (when verbose-flag
    (send self :force-output)
    (format self
            "~&Sending IAC~@[ ~A~]~%"
            (cadr (assq command telnet:*telsyms*))))
  (lock-output
    (send stream :tyo nvt-iac)
    (send stream :tyo command)
    (send stream :force-output)))

(defun telnet-send-option (command option)
  (declare (:self-flavor basic-telnet))
  (when verbose-flag
    (send self :force-output)
    (format self
            "~&Sending IAC~@[ ~A~]~@[ ~A~]~%"
            (cadr (assq command telnet:*telsyms*))
            (let ((name (cadr (assq option telnet:*telopts*))))
              (and name (substring (symbol-name name) 7)))))
  (lock-output
    (send stream :tyo nvt-iac)
    (send stream :tyo command)
    (send stream :tyo option)
    (send stream :force-output)))

(DEFUN TELNET-START-SUPDUP-OUTPUT ()
  (declare (:self-flavor BASIC-TELNET))
  (SETQ SUPDUP-OUTPUT-FLAG T)
  (LOCK-OUTPUT
    (SEND STREAM ':TYO NVT-IAC)
    (SEND STREAM ':TYO NVT-SUBNEGOTIATION-BEGIN)
    (SEND STREAM ':TYO NVT-SUPDUP-OUTPUT)
    (SEND STREAM ':TYO 1)
    (LET ((SUPDUP-%TOCID T))
      (SEND-TTY-VARIABLES STREAM SELF NIL OVERPRINT))
    (SEND STREAM ':TYO NVT-IAC)
    (SEND STREAM ':TYO NVT-SUBNEGOTIATION-END)
    (SEND STREAM ':FORCE-OUTPUT)))

(DEFUN TELNET-HANDLE-SUBNEGOTIATION ()
  (IF (AND (= (NVT-NETI) NVT-SUPDUP-OUTPUT) (= (NVT-NETI) 2))
      (TELNET-SUPDUP-OUTPUT-SUBNEGOTIATION)
      (DO ((CH) (STATE)) (NIL)
        (SETQ CH (NVT-NETI))
        (COND (STATE
               (AND (= CH NVT-SUBNEGOTIATION-END)
                    (RETURN NIL))
               (SETQ STATE NIL))
              ((= CH NVT-IAC)
               (SETQ STATE T))))))

(DEFUN TELNET-SUPDUP-OUTPUT-SUBNEGOTIATION ()
  (DECLARE (:SELF-FLAVOR BASIC-TELNET))
  (LET ((SUPDUP-OUTPUT-OLD-STREAM STREAM)
        (SUPDUP-OUTPUT-BYTE-COUNT (NVT-NETI)))
    (DECLARE (SPECIAL SUPDUP-OUTPUT-BYTE-COUNT SUPDUP-OUTPUT-OLD-STREAM))
    (LETF ((STREAM 'SUPDUP-OUTPUT-COUNTING-STREAM))
      (DO ()
          ((<= SUPDUP-OUTPUT-BYTE-COUNT 0)
           (OR (AND (= SUPDUP-OUTPUT-BYTE-COUNT 0)
                    (NVT-NETI) (NVT-NETI)               ;We already know the cursor position
                    (= (NVT-NETI) NVT-IAC) (= (NVT-NETI) NVT-SUBNEGOTIATION-END))
               (FERROR "SUPDUP-OUTPUT subnegotiation out of phase")))
        (SUPDUP-BUFFERED-TYO :BUFFERED-TYO (NVT-NETI))))))

(DEFUN SUPDUP-OUTPUT-COUNTING-STREAM (OP &REST ARGS)
  (DECLARE (SPECIAL SUPDUP-OUTPUT-BYTE-COUNT SUPDUP-OUTPUT-OLD-STREAM))
  (PROG1 (LEXPR-SEND SUPDUP-OUTPUT-OLD-STREAM OP ARGS)
         (AND (EQ OP :TYI)
              (SETQ SUPDUP-OUTPUT-BYTE-COUNT (1- SUPDUP-OUTPUT-BYTE-COUNT)))))

(DEFMETHOD (BASIC-TELNET :WHO-LINE-DOCUMENTATION-STRING) ()
  "Click right twice for System Menu.")

(DEFUN MOUSE-OUT (X Y BUTTONS)
  (DECLARE (:SELF-FLAVOR BASIC-TELNET))
  (SEND SELF :NET-OUTPUT 33)
  (MOUSE-COORD-OUT (TRUNCATE X TV:CHAR-WIDTH))
  (MOUSE-COORD-OUT (TRUNCATE Y TV:LINE-HEIGHT))
  (SEND SELF :NET-OUTPUT (+ (1+ (LDB (BYTE 3 0) BUTTONS))
                            (IF (NOT (ZEROP (LDB (BYTE 3 3) BUTTONS))) 1 0)
                            #/0))
  (SEND SELF :NET-OUTPUT #o33)
  (SEND SELF :NET-OUTPUT #o12))

(DEFUN MOUSE-COORD-OUT (N)
  (DECLARE (:SELF-FLAVOR BASIC-TELNET))
  (SEND SELF :NET-OUTPUT (+ #/0 (TRUNCATE N 100.)))
  (SETQ N (\ N 100.))
  (SEND SELF :NET-OUTPUT (+ #/0 (TRUNCATE N 10.)))
  (SETQ N (\ N 10.))
  (SEND SELF :NET-OUTPUT (+ #/0 N)))

(DEFMETHOD (BASIC-TELNET :LOGOUT) ()
  (TELNET-SEND-OPTION NVT-DO NVT-LOGOUT))

(DEFMETHOD (BASIC-TELNET :TOGGLE-IMLAC-SIMULATION) ()
  (SETQ SIMULATE-IMLAC-FLAG (NOT SIMULATE-IMLAC-FLAG)))

(defmethod (basic-telnet :toggle-remote-echo) ()
  (telnet-send-option (if echo-flag nvt-dont nvt-do) nvt-echo))

(defmethod (basic-telnet :toggle-local-echo) ()
  (telnet-echo (not echo-flag)))

(defmethod (basic-telnet :toggle-verbose-mode) ()
  (setq verbose-flag (not verbose-flag)))

(DEFMETHOD (BASIC-TELNET :USER-SET-MORE-P) (NEW-MORE-P)
  (SEND SELF ':SET-MORE-P NEW-MORE-P))

(DEFMETHOD (BASIC-TELNET :MORE-EXCEPTION) ()
  (TV:SHEET-MORE-HANDLER ':MORE-TYI))

(DEFMETHOD (BASIC-TELNET :MORE-TYI) ()
  (SETQ MORE-FLAG T)
  (COND ((EQ CURRENT-PROCESS TYPEOUT-PROCESS)
         (SEND SELF ':FORCE-KBD-INPUT '(:MORE))
         (PROCESS-WAIT "MORE"
                       #'(LAMBDA (LOC) (NOT (CAR LOC)))
                       (LOCATE-IN-INSTANCE SELF 'MORE-FLAG)))
        (T
         (SEND SELF ':ALLOW-ESCAPE)
         (SEND SELF ':TYI)
         (SETQ MORE-FLAG NIL))))

(defmethod (basic-telnet :send-ip) ()
  ;; Send a New Telnet "Interrupt Process".
  (lock-output
    (send stream :force-output))
  (send stream :set-urgent-output t)
  (telnet-send-command nvt-ip)
  (telnet-send-command nvt-dm)
  (send stream :set-urgent-output nil))

(defmethod (basic-telnet :send-ao) ()
  ;; Send a New Telnet "Abort Output".
  (lock-output
    (send stream :force-output))
  (send stream :set-urgent-output t)
  (telnet-send-command nvt-ao)
  (telnet-send-command nvt-dm)
  (send stream :set-urgent-output nil))

(defmethod (basic-telnet :send-ayt) ()
  (telnet-send-command nvt-ayt))

(defmethod (basic-telnet :send-ec) ()
  (telnet-send-command nvt-ec))

(defmethod (basic-telnet :send-el) ()
  (telnet-send-command nvt-el))

(COMMENT

;; assumes instance vars LINE-EDITOR-BUFFER

(DEFMETHOD READLINE-FOR-NVT ()
  (declare (:self-flavor BASIC-NVT))
    (COND ((NOT RUBOUT-HANDLER)
           ;;Stream with rubouts assumed not to have EOFs
           (SEND STREAM ':RUBOUT-HANDLER '() (FUNCTION READLINE-FOR-NVT) SELF))
          ;; Accumulate a string until CR, ignoring control characters
          (T
           (DO ((IDX 0)
                (LEN (ARRAY-ACTIVE-LENGTH LINE-EDITOR-BUFFER))
                (CH))
               (NIL)
             (SETQ CH (SEND SELF ':TYI))
             (COND ((OR (NULL CH) (= CH #/CR))
                    (RETURN NIL))
                   ((LDB-TEST %%KBD-CONTROL-META CH) )  ;Ignore controls
                   (T (AND (= IDX LEN)
                           (ADJUST-ARRAY-SIZE LINE-EDITOR-BUFFER (SETQ LEN (+ LEN 100))))
                      (ASET CH LINE-EDITOR-BUFFER IDX)
                      (SETQ IDX (1+ IDX))))))))

);end COMMENT

(COMPILE-FLAVOR-METHODS SUPDUP TELNET LOCAL-EDITING-SUPDUP)

;;; Always have at least one supdup window in the world
(OR SUPDUP-WINDOWS
    (TV:WITHOUT-SCREEN-MANAGEMENT (SEND (TV:WINDOW-CREATE SUPDUP-FLAVOR) ':ACTIVATE)))

(tv:add-system-key #/S '(PROGN SUPDUP:SUPDUP-FLAVOR) "Supdup" T)
(tv:add-system-key #/T 'SUPDUP:TELNET "Telnet" T)

;;; -*- Mode: LISP; Package: FILE-SYSTEM; Base: 8; Readtable:ZL -*-
;;;  ** (c) Copyright 1980, 1984 Massachusetts Institute of Technology
;;; This is SYS: NETWORK;CHAOS; QFILE
;;;
;;; Chaosnet file protocol support.  Could be conceivably coaxed into being used on other
;;; networks.  Chaosnet primitives are used only in a few places.  Defines the access,
;;; host-unit, and stream flavors needed.

;;; Don't lose before window system loaded.
(UNLESS (FBOUNDP 'TV:NOTIFY)
  (FSET 'TV:NOTIFY '(LAMBDA (IGNORE &REST ARGS)
                      "Default definition, provided in QFILE."
                      (APPLY 'FORMAT TV:COLD-LOAD-STREAM ARGS))))

;;; Number is the protocol version number
(DEFCONST *QFILE-CONTACT-NAME* "FILE 1")
(DEFCONST *QFILE-CONTROL-WINDOW-SIZE* 5)
(DEFVAR *HOST-QFILE-CONTACT-NAME-ALIST* NIL
  "Alist of host primary names and the contact name for the file.")

(DEFVAR *QFILE-DATA-WINDOW-SIZE* 15
  "Window size used on file data connections.")

(DEFCONSTANT %QFILE-CHARACTER-OPCODE CHAOS:DAT-OP)
(DEFCONSTANT %QFILE-BINARY-OPCODE (LOGIOR CHAOS:DAT-OP #o100))
(DEFCONSTANT %QFILE-COMMAND-OPCODE CHAOS:DAT-OP)
(DEFCONSTANT %QFILE-SYNCHRONOUS-MARK-OPCODE (1+ CHAOS:DAT-OP))
(DEFCONSTANT %QFILE-ASYNCHRONOUS-MARK-OPCODE (+ CHAOS:DAT-OP 2))
(DEFCONSTANT %QFILE-NOTIFICATION-OPCODE (+ CHAOS:DAT-OP 3))
(DEFCONSTANT %QFILE-EOF-OPCODE CHAOS:EOF-OP)

(DEFFLAVOR basic-QFILE-ACCESS () (DC-ACCESS-MIXIN DIRECTORY-STREAM-ACCESS-MIXIN BASIC-ACCESS)
  (:required-methods :open-command))

(DEFMETHOD (BASIC-QFILE-ACCESS :HOST-UNIT-FLAVOR) () 'QFILE-HOST-UNIT)
(DEFMETHOD (BASIC-QFILE-ACCESS :ACCESS-DESCRIPTION) () "Chaos FILE")

(DEFMETHOD (BASIC-QFILE-ACCESS :PEEK-FILE-SYSTEM-HEADER) ()
  (CHAOS:HOST-CHAOS-PEEK-FILE-SYSTEM-HEADER SELF HOST))

(defflavor qfile-access () (basic-qfile-access))
(define-file-access qfile-access .8s0 (:network :chaos))

(defflavor lispm-qfile-access () (basic-qfile-access))
(define-file-access lispm-qfile-access .85s0
  (:network :chaos)
  (:file-system-type :lispm :lmfile))

;;; One HOST-UNIT is associated with each control connection
(DEFFLAVOR QFILE-HOST-UNIT
        (HOST                                   ;Host object
         (CONTROL-CONNECTION NIL)               ;Control connection for this host
         (DATA-CONNECTIONS NIL)                 ;List of DATA-CONNECTION's
         MAX-DATA-CONNECTIONS                   ;Maximum number of data connections
         (LOCK NIL)                             ;Lock to insure no timing screws
         (LAST-USE-TIME (TIME)))
        (BIDIRECTIONAL-DATA-CONNECTION-MIXIN BASIC-HOST-UNIT)
  :ORDERED-INSTANCE-VARIABLES
  (:GETTABLE-INSTANCE-VARIABLES CONTROL-CONNECTION)
  :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES
  (:INITABLE-INSTANCE-VARIABLES HOST))

(DEFMETHOD (QFILE-HOST-UNIT :OPEN-CONTROL-CONNECTION-P) ()
  (EQ (CHAOS:STATE CONTROL-CONNECTION) 'CHAOS:OPEN-STATE))

(DEFMETHOD (QFILE-HOST-UNIT :CLOSE-CONTROL-CONNECTION) ()
  (LET ((CONN CONTROL-CONNECTION))
    (SETQ CONTROL-CONNECTION NIL)
    (CHAOS:CLOSE-CONN CONN "Logging out [LISPM]")))

(DEFMETHOD (QFILE-HOST-UNIT :RESET) (&OPTIONAL DONT-UNLOCK-LOCK-P)
  (COND (CONTROL-CONNECTION
         (CHAOS:REMOVE-CONN CONTROL-CONNECTION)
         (SETQ CONTROL-CONNECTION NIL)))
  (DO ((DATA-CONNS DATA-CONNECTIONS (CDR DATA-CONNS))
       (DATA-CONN))
      ((NULL DATA-CONNS)
       (SETQ DATA-CONNECTIONS NIL))
    (SETQ DATA-CONN (CAR DATA-CONNS))
    (DO ((LIST (DATA-STREAM-LIST DATA-CONN) (CDDR LIST))
         (STREAM))
        ((NULL LIST))
      (UNLESS (SYMBOLP (SETQ STREAM (CADR LIST)))
        (SEND TV:WHO-LINE-FILE-STATE-SHEET :DELETE-STREAM STREAM)
        (SEND STREAM :SET-STATUS :CLOSED)))
    (CHAOS:REMOVE-CONN (DATA-CONNECTION DATA-CONN)))
  (OR DONT-UNLOCK-LOCK-P (SETQ LOCK NIL)))

;;; Check that connection hasn't gone away, making a new one if necessary
(DEFMETHOD (QFILE-HOST-UNIT :VALIDATE-CONTROL-CONNECTION) (&OPTIONAL NO-ERROR-P)
  (LOCK-HOST-UNIT (SELF)
    (IF (AND CONTROL-CONNECTION
             (EQ (CHAOS:STATE CONTROL-CONNECTION) 'CHAOS:OPEN-STATE)
             (LOOP FOR DATA-CONN IN DATA-CONNECTIONS
                   ALWAYS (EQ (CHAOS:STATE (DATA-CONNECTION DATA-CONN))
                              'CHAOS:OPEN-STATE)))
        T
      (SEND SELF :RESET T)              ;Arg of T means don't unlock lock
      (CONDITION-CASE-IF NO-ERROR-P ()
          (PROGN
           (SETQ CONTROL-CONNECTION
                 (CHAOS:CONNECT HOST
                                (OR (SECOND (SI:ASSOC-EQUALP (SEND HOST :NAME)
                                                             *HOST-QFILE-CONTACT-NAME-ALIST*))
                                    *QFILE-CONTACT-NAME*)
                                *QFILE-CONTROL-WINDOW-SIZE*))
           (SETF (CHAOS:INTERRUPT-FUNCTION CONTROL-CONNECTION)
                 (LET-CLOSED ((HOST-UNIT SELF)) 'HOST-CHAOS-INTERRUPT-FUNCTION))
           (SEND HOST :LOGIN-UNIT SELF T)
           T)
        (REMOTE-NETWORK-ERROR NIL)))))


;;; Transaction management
(DEFSTRUCT (QFILE-TRANSACTION-ID :LIST :CONC-NAME
                                (:ALTERANT NIL)
                                (:CONSTRUCTOR MAKE-QFILE-TRANSACTION-ID-INTERNAL
                                              (ID SIMPLE-P)))
  ID
  SIMPLE-P
  (PKT NIL))

(DEFVAR *QFILE-UNIQUE-NUMBER* 259.)
(DEFVAR *QFILE-PENDING-TRANSACTIONS* NIL)

(DEFUN FILE-GENSYM (LEADER)
  "Create a string which is LEADER followed by a unique number (printed out)."
  (LET ((NUMBER (WITHOUT-INTERRUPTS
                  (SETQ *QFILE-UNIQUE-NUMBER*
                        (CL:REM (1+ *QFILE-UNIQUE-NUMBER*) 10000.)))))
    (STRING-APPEND LEADER
                   (DIGIT-CHAR (CL:REM (TRUNCATE NUMBER 1000.) 10.))
                   (DIGIT-CHAR (CL:REM (TRUNCATE NUMBER 100.) 10.))
                   (DIGIT-CHAR (CL:REM (TRUNCATE NUMBER 10.) 10.))
                   (DIGIT-CHAR (CL:rem NUMBER 10.)))))

(DEFUN FILE-MAKE-TRANSACTION-ID (&OPTIONAL (SIMPLE-P NIL) &AUX ID
                                 (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  "Return a new transaction ID string, and perhaps record a pending transaction.
If SIMPLE-P is not non-NIL, a pending transaction is recorded forthis transaction ID."
  (WITHOUT-INTERRUPTS
    (SETQ ID (FILE-GENSYM "T"))
    (PUSH (MAKE-QFILE-TRANSACTION-ID-INTERNAL ID SIMPLE-P) *QFILE-PENDING-TRANSACTIONS*))
  ID)

(DEFUN FILE-WAIT-FOR-TRANSACTION (TID &OPTIONAL CONN (WHOSTATE "File Transaction") &AUX ID)
  "Wait for completion of transaction with id TID, which should be on the pending list.
CONN is the connection the reply should arrive on (the control connection).
WHOSTATE is what to say in the who-line while waiting."
  (IF (NULL (SETQ ID (SYS:ASSOC-EQUAL TID *QFILE-PENDING-TRANSACTIONS*)))
      (FERROR NIL "Transaction ID ~A not found on pending list" TID)
    (PROCESS-WAIT WHOSTATE #'(LAMBDA (ID CONN)
                               (OR (QFILE-TRANSACTION-ID-PKT ID)
                                   (NEQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)))
                  ID CONN)
    (COND ((NEQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)
           ;; Get an error of the appropriate sort.
           (CHAOS:REPORT-BAD-CONNECTION-STATE CONN
                                              "read file command reply from"))
          (T
           (WITHOUT-INTERRUPTS
             (SETQ *QFILE-PENDING-TRANSACTIONS* (DELQ ID *QFILE-PENDING-TRANSACTIONS*))
             (QFILE-TRANSACTION-ID-PKT ID))))))

;;; This is the interrupt function we put into the control connection
;;; so we can detect asynchronous arrival of packets.
(DEFUN HOST-CHAOS-INTERRUPT-FUNCTION (REASON CONN &REST IGNORE)
  (DECLARE (SPECIAL HOST-UNIT))
  (CASE REASON
    (:INPUT
     (DO ((PKT (CHAOS:GET-NEXT-PKT CONN T)
               (CHAOS:GET-NEXT-PKT CONN T))
          (STRING) (TEM))
         ((NULL PKT))
       (SETQ STRING (CHAOS:PKT-STRING PKT))
       (SELECT (CHAOS:PKT-OPCODE PKT)
         (%QFILE-ASYNCHRONOUS-MARK-OPCODE
          (SETQ STRING (NSUBSTRING STRING
                                   (1+ (STRING-SEARCH-CHAR #/SPACE (CHAOS:PKT-STRING PKT)))))
          (DO ((DATA-CONNS (QFILE-HOST-UNIT-DATA-CONNECTIONS HOST-UNIT) (CDR DATA-CONNS))
               (HANDLE-LEN (OR (STRING-SEARCH-CHAR #/SPACE STRING)
                               (STRING-LENGTH STRING)))
               (STREAM))
              ((NULL DATA-CONNS) (CHAOS:RETURN-PKT PKT))
            (WHEN (STRING-EQUAL STRING (DATA-HANDLE (CAR DATA-CONNS) :OUTPUT)
                                :END1 HANDLE-LEN)
              (SETQ STREAM (DATA-STREAM (CAR DATA-CONNS) :OUTPUT))
              (SEND STREAM :ASYNC-MARK PKT)
              (RETURN NIL))))
         (%QFILE-COMMAND-OPCODE
          (SETQ STRING (SUBSTRING STRING 0 (STRING-SEARCH-CHAR #/SPACE STRING)))
          (SETQ TEM (SYS:ASSOC-EQUAL STRING *QFILE-PENDING-TRANSACTIONS*))
          (COND ((NULL TEM)
                 (PROCESS-RUN-FUNCTION
                   "QFILE Protocol Error"
                   #'(LAMBDA (PKT)
                       (UNWIND-PROTECT
                         (FERROR NIL "QFILE protocol violated, unknown transaction id in ~S"
                                 (CHAOS:PKT-STRING PKT))
                         (CHAOS:RETURN-PKT PKT)))
                   PKT))
                ((QFILE-TRANSACTION-ID-SIMPLE-P TEM)
                 ;;If simple transaction, make sure no error
                 (LET ((STRING (NSUBSTRING (CHAOS:PKT-STRING PKT)
                                           (1+ (STRING-SEARCH-CHAR #/SPACE
                                                                   (CHAOS:PKT-STRING PKT)))))
                       (FROM))
                   (SETQ FROM (1+ (STRING-SEARCH-SET '(#/SPACE #/NEWLINE) STRING)))
                   ;; If simple transaction fails, barf in another process
                   (OR (NOT (STRING-EQUAL "ERROR" STRING
                                          :START2 FROM
                                          :END2 (STRING-SEARCH-SET '(#/SPACE #/NEWLINE)
                                                                   STRING FROM)))
                       (PROCESS-RUN-FUNCTION "QFILE Protocol Error"
                                             'QFILE-PROCESS-ERROR-NEW
                                             (COPY-SEQ STRING))))
                 (SETQ *QFILE-PENDING-TRANSACTIONS* (DELQ TEM *QFILE-PENDING-TRANSACTIONS*))
                 (chaos:return-pkt pkt))
                (T (SETF (QFILE-TRANSACTION-ID-PKT TEM) PKT))))
         (%QFILE-NOTIFICATION-OPCODE
          (UNWIND-PROTECT
              (TV:NOTIFY NIL "File server ~A: ~A" (QFILE-HOST-UNIT-HOST HOST-UNIT) STRING)
            (CHAOS:RETURN-PKT PKT)))
         (OTHERWISE (CHAOS:RETURN-PKT PKT)))))))

;;; Allocate a new data connection
(DEFMETHOD (QFILE-HOST-UNIT :NEW-DATA-CONNECTION) ()
  (LET* ((DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA)
         (INPUT-HANDLE (FILE-GENSYM "I"))
         (OUTPUT-HANDLE (FILE-GENSYM "O"))
         CONNECTION
         SUCCESS)                               ;T => don't remove-conn the connection.
    (UNWIND-PROTECT
      (PROG ()
         RETRY
           (SETQ CONNECTION
                 (CHAOS:LISTEN OUTPUT-HANDLE *QFILE-DATA-WINDOW-SIZE* NIL))
           (LET ((PKT (CHAOS:GET-PKT))
                 (ID (FILE-MAKE-TRANSACTION-ID))
                 (DATA-CONN))
             (CHAOS:SET-PKT-STRING PKT ID "  DATA-CONNECTION " INPUT-HANDLE " " OUTPUT-HANDLE)
             (CHAOS:SEND-PKT CONTROL-CONNECTION PKT)
             (UNLESS (CHAOS:WAIT CONNECTION 'CHAOS:LISTENING-STATE (* 60. 30.) "File Data Connection")
               ;; Attempt to establish connection timed out -- give reasonable error
               (CERROR :RETRY-FILE-OPERATION NIL 'NETWORK-LOSSAGE
                       "Attempt to establish file data connection timed out.")
               ;; It lost; tell the connection we gave up,
               (CHAOS:CLOSE-CONN CONNECTION)
               ;; wait for the server to report the failure on our side,
               ;; or say why it failed, or something.
               (CHAOS:RETURN-PKT
                 (FILE-WAIT-FOR-TRANSACTION ID CONTROL-CONNECTION "File Data Connection"))
               ;; then try again
               (GO RETRY))
             (CHAOS:ACCEPT CONNECTION)
             (SETQ PKT (FILE-WAIT-FOR-TRANSACTION ID CONTROL-CONNECTION "File Data Connection"))
             (UNWIND-PROTECT
               (LET ((STRING (CHAOS:PKT-STRING PKT)))
                 (SETQ STRING (NSUBSTRING STRING (1+ (STRING-SEARCH-CHAR #/SPACE STRING))))
                 (COND ((QFILE-CHECK-COMMAND "DATA-CONNECTION" STRING T)
                        (SETQ DATA-CONN (MAKE-DATA-CONNECTION CONNECTION INPUT-HANDLE OUTPUT-HANDLE))
                        (PUSH DATA-CONN DATA-CONNECTIONS))
                       (T (QFILE-PROCESS-ERROR-NEW STRING))))   ;not proceedable
               (CHAOS:RETURN-PKT PKT))
             (SETQ SUCCESS T)
             (RETURN DATA-CONN)))
      ;; If we are not putting CONNECTION into the host unit, free it.
      (UNLESS SUCCESS
        (WHEN CONNECTION
          (SEND SELF :COMMAND NIL INPUT-HANDLE T "Undata" "UNDATA-CONNECTION")
          (CHAOS:CLOSE-CONN CONNECTION "Aborted")
          (CHAOS:REMOVE-CONN CONNECTION))))))

;;; An error string is as follows:
;;;  FHN<SP>ERROR<SP>Error-code<SP>Error-severity<SP>Error-description
;;; The error code is a three letter code that uniquely determines the error.  In general,
;;; this code will be ignored, but some codes may be of interest.  FNF is file not found,
;;; and NER is not enough resources.  The severity is either F (Fatal) or R (Restartable).
;;; If an error is Fatal, it can not be continued from, even if it is an asynchronous
;;; error.  If an error is Restartable, sending a CONTINUE command for the appropriate
;;; file handle will cause the file job to proceed where it left off.  In general, before
;;; the error is continued from, the error condition should be corrected, or the error
;;; will happen again immediately.
;;; The string that is passed in is expected to be "temporary" (contained in a chaos packet,
;;; for example).  Therefore, if an error handler gets called and it wants to save some
;;; of the strings, it must copy the ones it wishes to save.

(DEFPROP QFILE-PROCESS-ERROR-NEW T :ERROR-REPORTER)
(DEFUN QFILE-PROCESS-ERROR-NEW (STRING &OPTIONAL PATHNAME-OR-STREAM
                               PROCEEDABLE NOERROR &REST MAKE-CONDITION-ARGS
                               &AUX S-P ERROR-CODE ERROR-SEVERITY ERROR-STRING WHO-FOR
                               (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (TYPECASE PATHNAME-OR-STREAM
    (PATHNAME (SETQ WHO-FOR PATHNAME-OR-STREAM))
    (SI:FILE-STREAM-MIXIN (SETQ WHO-FOR (SEND PATHNAME-OR-STREAM :PATHNAME)))
    (T (SETQ WHO-FOR PATHNAME-OR-STREAM)))
  (SETQ S-P (QFILE-CHECK-COMMAND "ERROR" STRING))
  (SETQ ERROR-CODE (SUBSTRING STRING S-P (SETQ S-P (STRING-SEARCH-CHAR #/SPACE STRING S-P))))
  (SETQ S-P (1+ S-P))
  (SETQ ERROR-SEVERITY
        (SUBSTRING STRING S-P (SETQ S-P (STRING-SEARCH-CHAR #/SPACE STRING S-P))))
  ;;Some file errors to which we wish to append more text have ugly periods attached.
  (SETQ ERROR-STRING (string-right-trim "." (NSUBSTRING STRING (1+ S-P) (STRING-LENGTH STRING))))
  (APPLY 'FILE-PROCESS-ERROR
         (GET (INTERN ERROR-CODE (SYMBOL-PACKAGE 'FOO)) 'FS:FILE-ERROR)
         ERROR-STRING PATHNAME-OR-STREAM PROCEEDABLE NOERROR MAKE-CONDITION-ARGS))

(DEFUN QFILE-CHECK-COMMAND (COMMAND RETURNED-STRING &OPTIONAL (Y-OR-N-P NIL)
                                                   &AUX START END)
  "Verify a reply from a file server using the FILE protocol.
Returns the index in the reply of the start of the data.
Gets an error if the reply's command name is not COMMAND."
  (SETQ START (1+ (STRING-SEARCH-CHAR #/SPACE RETURNED-STRING)))
  (SETQ END (OR (STRING-SEARCH-SET '(#/SPACE #/NEWLINE) RETURNED-STRING START)
                (STRING-LENGTH RETURNED-STRING)))
  (COND ((STRING-EQUAL RETURNED-STRING COMMAND :START1 START :END1 END)
         (1+ END))                              ;Index of character after the delimiting space
        (Y-OR-N-P NIL)
        (T (FERROR NIL  ;I think this is best for an internal bug in FILE protocol.
                   "Incorrect command name ~S in acknowledge from file computer"
                   (NSUBSTRING RETURNED-STRING START END)))))

;;; Send a command over the control connection.
;;; MARK-P means writing or reading (expecting) a synchronous mark.
;;; STREAM-OR-HANDLE is a stream whose file handle should be used, or the handle itself.
;;;  if MARK-P, this had better really be a stream.
;;; SIMPLE-P means do not wait for a response, get an asynchronous error if any.
(DEFMETHOD (QFILE-HOST-UNIT :COMMAND)
           (MARK-P STREAM-OR-HANDLE SIMPLE-P WHOSTATE &REST COMMANDS
            &AUX HANDLE STREAM)
  (DECLARE (VALUES PKT SUCCESS STRING))
  (SETQ LAST-USE-TIME (TIME))
  (COND ((STRINGP STREAM-OR-HANDLE)
         (SETQ HANDLE STREAM-OR-HANDLE))
        (STREAM-OR-HANDLE
         (SETQ STREAM STREAM-OR-HANDLE
               HANDLE (SEND STREAM :FILE-HANDLE))
         (AND MARK-P (SETQ MARK-P (SEND STREAM :DIRECTION)))))
  (LET ((PKT (CHAOS:GET-PKT))
        (TRANSACTION-ID (FILE-MAKE-TRANSACTION-ID SIMPLE-P))
        SUCCESS STRING)
    ;; Make up a packet containing the command to be sent over
    (APPLY #'CHAOS:SET-PKT-STRING PKT TRANSACTION-ID " " (OR HANDLE "") " " COMMANDS)
    (CHAOS:SEND-PKT CONTROL-CONNECTION PKT %QFILE-COMMAND-OPCODE)
    (IF (EQ MARK-P :OUTPUT) (SEND STREAM :WRITE-SYNCHRONOUS-MARK))
    ;; Get the portion of the response after the transaction ID.
    (COND (SIMPLE-P
           (AND (EQ MARK-P :INPUT)
                (SEND STREAM :READ-UNTIL-SYNCHRONOUS-MARK))
           (VALUES NIL T ""))
          ((SETQ PKT (FILE-WAIT-FOR-TRANSACTION TRANSACTION-ID CONTROL-CONNECTION WHOSTATE))
           (SETQ STRING (NSUBSTRING (CHAOS:PKT-STRING PKT)
                                    (1+ (STRING-SEARCH-CHAR #/SP (CHAOS:PKT-STRING PKT)))))
           (SETQ SUCCESS (LET ((FROM (IF HANDLE
                                         (QFILE-CHECK-HANDLE HANDLE STRING)
                                         (1+ (STRING-SEARCH-SET '(#/SPACE #/NEWLINE)
                                                                STRING)))))
                           (NOT (STRING-EQUAL "ERROR" STRING
                                              :START2 FROM
                                              :END2 (STRING-SEARCH-SET '(#/SPACE #/NEWLINE)
                                                                       STRING FROM)))))
           (AND SUCCESS (EQ MARK-P :INPUT)
                (SEND STREAM :READ-UNTIL-SYNCHRONOUS-MARK))
           (VALUES PKT SUCCESS STRING)))))

(DEFUN QFILE-CHECK-HANDLE (HANDLE STRING)
  "Validate a reply STRING received supposedly for file HANDLE.
Gets an error if the string does not start with HANDLE.
If it does, returns the index of the first character in STRING after the handle."
  (LET ((HANDLE-END (STRING-SEARCH-SET '(#/SPACE #/NEWLINE) STRING)))
    (AND (NULL HANDLE-END)
         (FERROR NIL "Response over control connection was incorrectly formatted"))
    (OR (STRING-EQUAL STRING HANDLE :END1 HANDLE-END)
        (FERROR NIL "Response over control connection was for wrong file handle"))
    (1+ HANDLE-END)))

(DEFVAR CATCH-LOGIN-PROBLEMS-P T
  "If set to NIL, then QFILE-LOGIN-HOST-UNIT won't try to catch LOGIN-PROBLEMS conditions")

(DEFUN QFILE-LOGIN-HOST-UNIT (UNIT LOGIN-P UNAME-HOST
                              &AUX HOST CONN SUCCESS PKT ENABLE-CAPABILITIES
        (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  "Log the host unit UNIT in or out.  LOGIN-P = NIL means log out, otherwise log in.
Note that logging in must be done on each host unit before it can be used,
whether or not this is the host that the user actually specified when
he said /"log in/".  UNAME-HOST should be the host that the user actually logged in on."
  (SETQ HOST (SEND UNIT :HOST)
        CONN (SEND UNIT :CONTROL-CONNECTION))
  ;; First thing we should do is check to see if the connection is in a valid state,
  ;; and then logout.
  (UNLESS LOGIN-P
    (AND (SEND UNIT :VALID-CONTROL-CONNECTION-P)
         (SEND UNIT :CLOSE-CONTROL-CONNECTION)))
  (AND CONN (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)
       (UNWIND-PROTECT
         (DO ((ID (FILE-MAKE-TRANSACTION-ID))
              (PASSWORD "") (ACCOUNT "") (NEED-PASSWORD NIL) NEW-USER-ID)
             (SUCCESS)
           (SETQ PKT (CHAOS:GET-PKT)
                 ID (FILE-MAKE-TRANSACTION-ID))
           ;; Only hack user name or password if logging in, not out.
           (WHEN LOGIN-P
             (MULTIPLE-VALUE (NEW-USER-ID PASSWORD ENABLE-CAPABILITIES)
               (DETERMINE-USER-ID-AND-PASSWORD UNAME-HOST HOST NEED-PASSWORD)))
           ;; If the connection got closed while we waited for input, reconnect.
           (UNLESS (EQ (CHAOS:STATE CONN) 'CHAOS:OPEN-STATE)
             (CHAOS:CLOSE-CONN CONN)
             (CONDITION-CASE (CONN1)
                 (CHAOS:CONNECT HOST *QFILE-CONTACT-NAME* *QFILE-CONTROL-WINDOW-SIZE*)
               (:NO-ERROR
                (SETF (QFILE-HOST-UNIT-CONTROL-CONNECTION UNIT) CONN1)
                (SETQ CONN CONN1)
                (SETF (CHAOS:INTERRUPT-FUNCTION CONN)
                      (LET-CLOSED ((HOST-UNIT UNIT)) 'HOST-CHAOS-INTERRUPT-FUNCTION)))
               (SYS:CONNECTION-REFUSED
                (FERROR 'FS:HOST-NOT-AVAILABLE
                        "File server ~2@*~A is refusing file connections."
                        NIL NIL HOST))))
           ;; Send the login command.
           (CHAOS:SET-PKT-STRING PKT
                                 ID "  LOGIN " (OR NEW-USER-ID "") " " PASSWORD " " ACCOUNT)
           (CHAOS:SEND-PKT CONN PKT)
           ;; Avoid doing RETURN-PKT on a PKT that has been returned already by SEND-PKT.
           (SETQ PKT NIL)
           (SETQ PKT (FILE-WAIT-FOR-TRANSACTION ID CONN "Login"))
           (IF LOGIN-P
               (LET ((STR (CHAOS:PKT-STRING PKT)) IDX)
                 (SETQ STR (NSUBSTRING STR (1+ (STRING-SEARCH-CHAR #/SPACE STR))))
                 (SETQ IDX (QFILE-CHECK-COMMAND "LOGIN" STR T))
                 (IF (NULL IDX)
                     (CONDITION-CASE-IF CATCH-LOGIN-PROBLEMS-P () (QFILE-PROCESS-ERROR-NEW STR)
                       (FS:LOGIN-PROBLEMS
                        ;; Since this password is wrong, flush it
                        (FORGET-PASSWORD NEW-USER-ID HOST)
                        (SETQ NEED-PASSWORD T)))
                   (SETQ IDX (STRING-SEARCH-CHAR #/SPACE STR IDX))
                   (MULTIPLE-VALUE-BIND
                     (HSNAME-PATHNAME PERSONAL-NAME GROUP PERSONAL-NAME-1)
                       (SEND HOST :HSNAME-INFORMATION UNIT STR IDX)
                     (SET-LOCAL-VARIABLES-FROM-HOST-INFO HOST NEW-USER-ID
                       HSNAME-PATHNAME PERSONAL-NAME GROUP PERSONAL-NAME-1))
                   ;; If we have done remote connect or access on this host,
                   ;; tell the new file server about it.
                   (SEND SELF :RESTORE-SERVER-STATE ENABLE-CAPABILITIES)
                   (SETQ SUCCESS T)))
             (SETQ SUCCESS T))
           (CHAOS:RETURN-PKT PKT)
           (SETQ PKT NIL))
         (UNLESS SUCCESS
           (AND PKT (CHAOS:RETURN-PKT PKT))
           (CHAOS:CLOSE-CONN CONN "Login failed [LISPM]"))))
  T)

(DEFMETHOD (QFILE-HOST-UNIT :LOGIN) (LOGIN-P UNAME-HOST)
  (QFILE-LOGIN-HOST-UNIT SELF LOGIN-P UNAME-HOST))

;;; Initializations

(DEFUN QFILE-SYSTEM-INIT ()
  (WITHOUT-INTERRUPTS
    (DO ((L *QFILE-PENDING-TRANSACTIONS* (CDR L))
         (PKT))
        ((NULL L)
         (SETQ *QFILE-PENDING-TRANSACTIONS* NIL))
      (AND (SETQ PKT (QFILE-TRANSACTION-ID-PKT (CAR L)))
           (CHAOS:RETURN-PKT PKT))))
  (DOLIST (HOST *PATHNAME-HOST-LIST*)
    (SEND HOST :SEND-IF-HANDLES :RESET)))

(ADD-INITIALIZATION "Clean up QFILE transactions" '(QFILE-SYSTEM-INIT) '(SYSTEM))

;;; Passing and reading information about pathnames and properties
(DEFUN FILE-PRINT-PATHNAME (PATHNAME)
  "Return namestring for PATHNAME, including the host if it isn't an actual machine."
  (LET ((HN (SEND (SEND PATHNAME :HOST) :SEND-IF-HANDLES :REMOTE-HOST-NAME))
        (SFH (SEND PATHNAME :STRING-FOR-HOST)))
    (IF HN (STRING-APPEND HN ": " SFH) SFH)))

(DEFUN FILE-PRINT-DIRECTORY (PATHNAME)
  "Return namestring for PATHNAME's dir, including the host if it isn't an actual machine."
  (LET ((HN (SEND (SEND PATHNAME :HOST) :SEND-IF-HANDLES :REMOTE-HOST-NAME))
        (SFD (SEND PATHNAME :STRING-FOR-DIRECTORY)))
    (IF HN (STRING-APPEND HN ": " SFD) SFD)))

;; Copied from LAD: RELEASE-3.NETWORK.CHAOS; QFILE.LISP#379 on 2-Oct-86 23:53:59
;;; PATHNAME is only used as a source of a host with respect to which to parse
(DEFUN READ-FILE-PROPERTY-LIST-STRING (STRING OPERATION PATHNAME
                                       &OPTIONAL (PROPERTIES-TO-READ
   ;properties are in order as expected in reply from file-computer.
   ; T is a special kludge to group time and date into one field for :CREATION-DATE.
   ;   it turns out not to be really general enuf, see other part of kludge in :LENGTH below.
                                                   '((:CREATION-DATE) (:CREATION-TIME)
                                                     (:LENGTH T) (:QFASLP T)
                                                     (:CHARACTERS T) (:AUTHOR T)
                                                     (:byte-size t)))
                                       &AUX PATHNAME-ORIGIN PROPERTY-LIST
                                            (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (OR (SETQ PATHNAME-ORIGIN (STRING-SEARCH-CHAR #/NEWLINE STRING))
      (FERROR NIL "Illegally formatted string ~S." STRING))
  (DO ((I (QFILE-CHECK-COMMAND OPERATION STRING))
       (PROP PROPERTIES-TO-READ (CDR PROP))
       (*READ-BASE* 10.) (*READTABLE* SI:INITIAL-READTABLE)
       (TYPE) (DATE-START))
      ((OR (NULL I) (> I PATHNAME-ORIGIN) (NULL PROP)))
    (SETQ TYPE (CAAR PROP))
    (CASE TYPE
      (:CREATION-DATE (SETQ DATE-START I))
      (:LENGTH (PUSH (OR (FS:PARSE-DIRECTORY-DATE-PROPERTY STRING DATE-START I)
                         ;; When bootstrapping, dates are recorded as strings.
                         (SUBSTRING STRING DATE-START I))
                     PROPERTY-LIST)
               (PUSH :CREATION-DATE PROPERTY-LIST)))
    (COND ((CADAR PROP)
           (MULTIPLE-VALUE-BIND (PROPVAL ENDPOS)
               (CL:READ-FROM-STRING STRING NIL NIL :START I :end pathname-origin)
             (SETQ I ENDPOS)
             (PUSH PROPVAL PROPERTY-LIST)
             (PUSH TYPE PROPERTY-LIST)))
          (T (SETQ I (STRING-SEARCH-CHAR #/SPACE STRING (1+ I))))))
  (PUSH (SEND PATHNAME :PARSE-TRUENAME
              (SUBSTRING STRING (SETQ PATHNAME-ORIGIN (1+ PATHNAME-ORIGIN))
                         (STRING-SEARCH-CHAR #/NEWLINE STRING PATHNAME-ORIGIN)))
        PROPERTY-LIST)
  (PUSH :TRUENAME PROPERTY-LIST)
  PROPERTY-LIST)

(DEFUN QFILE-LISPM-OPEN-OPTIONS-STRING (DIRECTION OPTIONS IF-EXISTS IF-EXISTS-P
                                        IF-DOES-NOT-EXIST)
  (LET ((*PRINT-BASE* 10.)
        (*NOPOINT T) (*PRINT-RADIX* NIL)
        (*PACKAGE* SI:PKG-USER-PACKAGE)
        (*READTABLE* SI:INITIAL-COMMON-LISP-READTABLE)
        (*PRINT-LENGTH* NIL) (*PRINT-LEVEL* NIL))
    (AND (EQ DIRECTION :OUTPUT)
         (NULL IF-EXISTS)
         (SETQ OPTIONS `(:IF-EXISTS :ERROR . ,OPTIONS)))
    (AND (NOT IF-EXISTS-P)
         (SI:GET-LOCATION-OR-NIL (LOCF OPTIONS) :IF-EXISTS)
         (PROGN
           (SETQ OPTIONS (COPY-LIST OPTIONS))
           (REMF OPTIONS :IF-EXISTS)))
    (AND (NULL IF-DOES-NOT-EXIST)
         (SETQ OPTIONS `(:IF-DOES-NOT-EXIST :ERROR . ,OPTIONS)))
    (PRIN1-TO-STRING OPTIONS)))

(DEFUN QFILE-IF-DOES-NOT-EXIST-STRING (DIRECTION IF-DOES-NOT-EXIST IF-EXISTS-P)
  (IF (OR IF-EXISTS-P
          (NEQ IF-DOES-NOT-EXIST
               (CASE DIRECTION
                 ((:INPUT NIL :PROBE-DIRECTORY :PROBE-LINK)
                  :ERROR)
                 (:probe
                  nil)
                 (:OUTPUT
                  :CREATE))))
      (STRING-APPEND " IF-DOES-NOT-EXIST "
                     (IF (EQ IF-DOES-NOT-EXIST NIL)
                         "ERROR"
                         IF-DOES-NOT-EXIST))
    ""))

(DEFUN QFILE-OPEN-DIRECTION-STRING (DIRECTION)
  ;; should get protocol-violation error of some form
  (ECASE DIRECTION
    ((:probe NIL) "PROBE")
    (:PROBE-DIRECTORY "PROBE-DIRECTORY")
    (:PROBE-LINK "PROBE INHIBIT-LINKS")
    (:INPUT "READ")
    (:OUTPUT "WRITE")))

(DEFUN QFILE-OO-STRING (OPTION OPTION-STRING) (IF OPTION OPTION-STRING ""))

;;; Functions to be called by pathname interface.
;;; Commands without associated streams.
(DEFUN DELETE-CHAOS (ACCESS PATHNAME ERROR-P &AUX HOST-UNIT PKT SUCCESS STRING)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
    (UNWIND-PROTECT
        (PROGN
          (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
            (SEND HOST-UNIT :COMMAND NIL NIL NIL "Delete" "DELETE" #/NEWLINE
                  (FILE-PRINT-PATHNAME PATHNAME) #/NEWLINE))
          (OR SUCCESS
              (QFILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR-P) :DELETE)))
      (AND PKT (CHAOS:RETURN-PKT PKT)))))

;Problem is, it might exceed size of packet in the command or the reply.
;(DEFUN DELETE-OR-UNDELETE-MULTIPLE-PATHNAMES-CHAOS (HOST PATHNAMES COMMAND OP ERROR-P
;                                                   &AUX HOST-UNIT PKT SUCCESS STRING)
;  (HANDLING-FILE-ERRORS (ERROR-P)
;    (SETQ HOST-UNIT (SEND HOST :GET-HOST-UNIT))
;    (MULTIPLE-VALUE (PKT SUCCESS STRING)
;      (SEND HOST-UNIT :COMMAND NIL NIL NIL command
;              COMMAND #/NEWLINE
;              (MAPCAN #'(LAMBDA (PATHNAME)
;                          (LIST (FILE-PRINT-PATHNAME PATHNAME) #/NEWLINE))
;                      PATHNAMES)))
;    (UNWIND-PROTECT
;      (IF SUCCESS
;         (LET (RESULTS)
;           (DO ((I 0)
;                (PTAIL PATHNAMES (CDR PTAIL)))
;               ((NULL PTAIL))
;             (SETQ J (STRING-SEARCH-CHAR #/NEWLINE STRING I))
;             (IF (= J I)
;                 (PUSH NIL RESULTS)
;               (PUSH (QFILE-PROCESS-ERROR-NEW (SUBSTRING STRING I J) (CAR PTAIL) NIL T OP)
;                     RESULTS))
;             (SETQ I (1+ J)))
;           RESULTS)
;         (QFILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR-P) :DELETE))
;      (CHAOS:RETURN-PKT PKT))))

(DEFUN RENAME-CHAOS (ACCESS OLD-PATHNAME NEW-PATHNAME ERROR &AUX PKT SUCCESS STRING)
  (DECLARE (VALUES TRUENAME OLD-TRUENAME))
  (HANDLING-FILE-ERRORS (ERROR)
    (LET ((HOST-UNIT (SEND ACCESS :GET-HOST-UNIT)))
      (UNWIND-PROTECT
          (PROGN (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
                   (SEND HOST-UNIT :COMMAND NIL NIL NIL "Rename" "RENAME" #/NEWLINE
                         (FILE-PRINT-PATHNAME OLD-PATHNAME) #/NEWLINE
                         (FILE-PRINT-PATHNAME NEW-PATHNAME) #/NEWLINE))
                 (IF SUCCESS
                     ;; If there is a second line coming from the file server,
                     ;; it is the new truename.
                     (let* ((from (string-search-char #/newline string)) truename)
                       (if (null from) (values new-pathname old-pathname)
                         (let* ((old (string-search-char #/newline string (1+ from)))
                                (host (send old-pathname :host)))
                           (setq truename (parse-pathname string host nil (1+ from) old))
                           (if (null old) (values truename old-pathname)
                             (values truename
                                     (fs:parse-pathname string host nil (1+ old)))))))
                   (QFILE-PROCESS-ERROR-NEW STRING OLD-PATHNAME NIL (NOT ERROR) :RENAME)))
      (AND PKT (CHAOS:RETURN-PKT PKT))))))

(DEFUN COMPLETE-CHAOS (ACCESS PATHNAME STRING OPTIONS
                       &AUX HOST-UNIT STRING-ORIGIN
                            DELETED-P WRITE-P NEW-OK PKT SUCCESS FILE-STRING
                            (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (DOLIST (KEY OPTIONS)
    (CASE KEY
      (:DELETED
       (SETQ DELETED-P T))
      ((:READ :IN)
       (SETQ WRITE-P NIL))
      ((:PRINT :OUT :WRITE)
       (SETQ WRITE-P T))
      (:OLD
       (SETQ NEW-OK NIL))
      (:NEW-OK
       (SETQ NEW-OK T))
      (OTHERWISE
       (FERROR NIL "~S is not a recognized option." KEY :COMPLETE-STRING))))
  (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
  (UNWIND-PROTECT
      (PROGN
        (MULTIPLE-VALUE-SETQ (PKT SUCCESS FILE-STRING)
          (SEND HOST-UNIT :COMMAND NIL NIL NIL "Complete" "COMPLETE"
                (IF DELETED-P " DELETED" "")
                (IF WRITE-P   " WRITE"   "")
                (IF NEW-OK    " NEW-OK"  "")
                #/NEWLINE
                (FILE-PRINT-PATHNAME PATHNAME) #/NEWLINE
                STRING #/NEWLINE))
        (WHEN SUCCESS
          (OR (SETQ STRING-ORIGIN (STRING-SEARCH-CHAR #/NEWLINE FILE-STRING))
              (FERROR NIL "Illegally formatted string ~S from file server." FILE-STRING))
          (SETQ SUCCESS (LET ((*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
                          (CLI:READ-FROM-STRING FILE-STRING NIL NIL
                                                :START (QFILE-CHECK-COMMAND "COMPLETE"
                                                                            FILE-STRING))))
          (SETQ STRING (SUBSTRING FILE-STRING
                                  (SETQ STRING-ORIGIN (1+ STRING-ORIGIN))
                                  (STRING-SEARCH-CHAR #/NEWLINE FILE-STRING STRING-ORIGIN))))
        (IF (EQ SUCCESS :NIL) (SETQ SUCCESS NIL))
        (VALUES STRING SUCCESS))
    (AND PKT (CHAOS:RETURN-PKT PKT))))

(DEFUN CHANGE-PROPERTIES-CHAOS (ACCESS PATHNAME ERROR-P PROPERTIES
                                &AUX HOST-UNIT PKT SUCCESS STRING)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
    (SETQ STRING (CHANGE-PROPERTIES-STRING PROPERTIES PATHNAME))
    (UNWIND-PROTECT
        (PROGN
          (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
            (SEND HOST-UNIT :COMMAND NIL NIL NIL "Change Properties" STRING))
          (UNLESS SUCCESS
            (QFILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR-P) :CHANGE-PROPERTIES)))
      (AND PKT (CHAOS:RETURN-PKT PKT)))))

(DEFUN CHANGE-PROPERTIES-STRING (PROPERTIES &OPTIONAL PATHNAME)
  (WITH-OUTPUT-TO-STRING (STREAM)
    (FORMAT STREAM "CHANGE-PROPERTIES~%")
    (AND PATHNAME (FORMAT STREAM "~A~%" (FILE-PRINT-PATHNAME PATHNAME)))
    (TV:DOPLIST (PROPERTIES PROP IND)
      (FORMAT STREAM "~A " IND)
      (FUNCALL (DO ((L FS:*KNOWN-DIRECTORY-PROPERTIES* (CDR L)))
                   ((NULL L) #'PRINC)
                 (AND (MEMQ IND (CDAR L))
                      (RETURN (CADAAR L))))
               PROP STREAM)
      (SEND STREAM :TYO #/NEWLINE))))

(DEFUN CREATE-LINK-CHAOS (ACCESS LINK LINK-TO ERROR-P &AUX HOST-UNIT PKT SUCCESS STRING)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
    (UNWIND-PROTECT
        (PROGN
          (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
            (SEND HOST-UNIT :COMMAND NIL NIL NIL "Create Link" "CREATE-LINK" #/NEWLINE
                  (FILE-PRINT-PATHNAME LINK) #/NEWLINE
                  (FILE-PRINT-PATHNAME LINK-TO)))
          (UNLESS SUCCESS
            (QFILE-PROCESS-ERROR-NEW STRING LINK NIL (NOT ERROR-P) :CREATE-LINK)))
      (AND PKT (CHAOS:RETURN-PKT PKT)))))

(DEFUN HOMEDIR-CHAOS (ACCESS &OPTIONAL (USER USER-ID) &AUX (HOST (SEND ACCESS :HOST)))
  (OR (CDR (ASSQ HOST FS:USER-HOMEDIRS))
      (WHEN (NOT (SI:MEMBER-EQUAL USER '(NIL "")))
        ;; Try logging in a file connection, in case that works.
        (SEND ACCESS :GET-HOST-UNIT T)
        (CDR (ASSQ HOST FS:USER-HOMEDIRS)))
      ;; If we fail to establish a connection and don't already know a homedir,
      ;; return something innocuous.  Don't get an error.
      (QUIET-USER-HOMEDIR HOST)))

(DEFUN DIRECTORY-OPERATION-CHAOS (OPERATION ACCESS PATHNAME ERRORP WHOSTATE
                                  &AUX HOST-UNIT PKT SUCCESS FILE-STRING)
  (HANDLING-FILE-ERRORS (ERRORP)
    (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
    (UNWIND-PROTECT
        (PROGN
          (MULTIPLE-VALUE-SETQ (PKT SUCCESS FILE-STRING)
            (SEND HOST-UNIT :COMMAND NIL NIL NIL WHOSTATE
                  (STRING OPERATION) #/NEWLINE
                  (FILE-PRINT-DIRECTORY PATHNAME) #/NEWLINE))
          (COND (SUCCESS
                 (LET ((START (QFILE-CHECK-COMMAND (STRING OPERATION) FILE-STRING)))
                   (VALUES (PARSE-NUMBER FILE-STRING START))))
                (T
                 (QFILE-PROCESS-ERROR-NEW FILE-STRING PATHNAME NIL (NOT ERRORP) OPERATION))))
      (AND PKT (CHAOS:RETURN-PKT PKT)))))

(DEFMETHOD (BASIC-QFILE-ACCESS :CHANGE-CAPABILITIES)
           (ENABLEP CAPABILITIES &OPTIONAL HOST-UNIT
            &AUX WHOSTATE COMMAND (ALIST (GET HOST 'CAPABILITIES-ALIST)) LOSERS
            (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (IF ENABLEP
      (SETQ WHOSTATE "Enable" COMMAND 'ENABLE-CAPABILITIES)
      (SETQ WHOSTATE "Disable" COMMAND 'DISABLE-CAPABILITIES))
  (WITH-STACK-LIST (HU HOST-UNIT)               ;Are we CONNSING yet?
    (SEND SELF :GET-HOST-UNIT T)                        ;need at least one
    (IF (EQUAL HU '(NIL)) (SETQ HU (SEND SELF :HOST-UNITS)))
    ;; have to do them one at a time so that can get success/failure on individual capability
    ;;  basis, rather than error for whole transaction if once capability is losing.
    (DOLIST (CAP CAPABILITIES)
      (SETQ CAP (STRING-UPCASE CAP))
      (LET ((ELEM (ASSOC-EQUAL CAP ALIST))
            (FLAG NIL))
        (DO ((HU HU (CDR HU))
             PKT SUCCESS STRING)
            ((NULL HU))
          (LOCK-HOST-UNIT ((CAR HU))
            (IF (SEND (CAR HU) :VALIDATE-CONTROL-CONNECTION)
                (UNWIND-PROTECT
                  (PROGN
                    (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
                      (SEND (CAR HU) :COMMAND NIL NIL NIL WHOSTATE
                            COMMAND " " CAP))
                    (COND (FLAG)
                          ((NOT SUCCESS)
                           (WHEN (NULL (CDR HU))
                             (SETQ ALIST (DELQ ELEM ALIST))
                             (PUSHNEW CAP LOSERS :TEST #'EQUAL)))
                          (T
                           (LOOP WITH I = 0
                                 WHILE (SETQ I (STRING-SEARCH-CHAR #/SPACE STRING (1+ I)))
                              WHEN (STRING-EQUAL CAP STRING :START2 (1+ I)
                                                            :END2 (+ I 1 (LENGTH CAP)))
                                DO (SETQ FLAG T)
                                   (UNLESS ELEM
                                     (SETQ ELEM (CONS CAP NIL))
                                     (PUSH ELEM ALIST))
                                   (SETF (CDR ELEM)
                                         (CHAR-EQUAL
                                           (CHAR STRING (SETQ I (+ I 2 (LENGTH CAP))))
                                           ;; will be either T or NIL
                                           (IF ENABLEP #/T #/N)))))))
                  (CHAOS:RETURN-PKT PKT))))))))
  (VALUES (SETF (GET HOST 'CAPABILITIES-ALIST) ALIST)
          LOSERS))

;(DEFUN CHANGE-CAPABILITIES-CHAOS (ACCESS CAPABILITIES ENABLEP &OPTIONAL HOST-UNIT)
;  (SEND ACCESS :CHANGE-CAPABILITIES CAPABILITIES ENABLEP HOST-UNIT))

(DEFUN CWD-CHAOS (ACCESS PATHNAME ERROR-P ACCESSP &OPTIONAL (HOST-UNIT NIL HOST-UNIT-SPECD)
                  &AUX PKT SUCCESS FILE-STRING COMMAND NEED-PASSWORD ENABLE-CAPABILITIES
                  (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA) (HOST (SEND ACCESS :HOST)))
  (SETQ COMMAND (IF ACCESSP "ACCESS" "CWD"))
  (DO-FOREVER
    (LET ((DIR (FILE-PRINT-DIRECTORY PATHNAME))
          (PASSWORD ""))
      ;; If we have failed once, ask for a new password.
      ;; The first time, if we remember a password, use it.
      (COND (NEED-PASSWORD
             (MULTIPLE-VALUE (DIR PASSWORD ENABLE-CAPABILITIES)
               (FS:FILE-GET-PASSWORD DIR HOST T))
             (WHEN ENABLE-CAPABILITIES (SEND ACCESS :ENABLE-CAPABILITIES))
             (SETQ PATHNAME (FS:PARSE-PATHNAME DIR HOST))
             (SETQ DIR (FILE-PRINT-DIRECTORY PATHNAME)))
            ;; We know the user id; use remembered password if any.
            ((EQUAL PASSWORD "")
             (SETQ PASSWORD
                   (OR (CADR (SI:ASSOC-EQUALP (LIST DIR (SEND HOST :NAME))
                                              FS:USER-HOST-PASSWORD-ALIST))
                       ""))))
      (OR HOST-UNIT-SPECD (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT)))
      (UNWIND-PROTECT
          (PROGN
            (MULTIPLE-VALUE-SETQ (PKT SUCCESS FILE-STRING)
              (SEND HOST-UNIT :COMMAND NIL NIL NIL COMMAND
                    COMMAND #/NEWLINE
                    DIR #/NEWLINE
                    PASSWORD #/NEWLINE))
            (COND (SUCCESS
                   ;; Succeeded on one host unit.
                   ;; Record what our connected or accessed directory is for this host.
                   (IF ACCESSP
                       (PUSHNEW PATHNAME (GET HOST 'QFILE-ACCESSED-DIRECTORIES))
                       (SEND HOST :SET :GET 'QFILE-CONNECTED-DIRECTORY PATHNAME))
                   ;; Also inform any other host units that are connected now.
                   (OR HOST-UNIT-SPECD
                       (LET ((UNITS (SEND ACCESS :HOST-UNITS)))
                         (DOLIST (UNIT UNITS)
                           (AND (NEQ UNIT HOST-UNIT)
                                (SEND UNIT :VALIDATE-CONTROL-CONNECTION T)
                                (SEND UNIT :COMMAND NIL NIL NIL COMMAND
                                      COMMAND #/NEWLINE
                                      DIR #/NEWLINE
                                      PASSWORD #/NEWLINE)))))
                   (RETURN T))
                  (T
                   (CONDITION-CASE-IF (NOT ERROR-P) (ERROR-OBJECT)
                       (CONDITION-CASE ()
                           (QFILE-PROCESS-ERROR-NEW FILE-STRING)
                         (LOGIN-PROBLEMS
                          ;; Since this password is wrong,
                          ;;  flush it from list of remembered ones.
                          (LET ((ALIST-ELEMENT
                                  (SI:ASSOC-EQUALP (LIST DIR (SEND HOST :NAME))
                                                   FS:USER-HOST-PASSWORD-ALIST)))
                            (IF ALIST-ELEMENT
                                (SETQ FS:USER-HOST-PASSWORD-ALIST
                                      (DELQ ALIST-ELEMENT FS:USER-HOST-PASSWORD-ALIST))))
                          (SETQ NEED-PASSWORD T)))
                     (ERROR ERROR-OBJECT)))))
        (AND PKT (CHAOS:RETURN-PKT PKT))))))

(defmethod (qfile-access :open-command) (hu handle file real-characters real-byte-size
                                         real-direction real-if-exists if-exists-p
                                         real-if-does-not-exist
                                         &key
                                         ;; these are ignored because they have been
                                         ;; canonicalised elsewhere
                                         characters byte-size direction if-exists
                                         if-does-not-exist error
                                         ;; we need to look at these
                                         temporary deleted raw super-image
                                         preserve-dates inhibit-links submit estimated-length)
  (declare (ignore characters byte-size direction if-exists if-does-not-exist error))
  (SEND hu :COMMAND NIL handle NIL "Open" "OPEN "
        (QFILE-OPEN-DIRECTION-STRING real-DIRECTION)
        " "
        (COND ((NULL real-CHARACTERS) "BINARY")
              ((EQ real-CHARACTERS :DEFAULT) "DEFAULT")
              (T "CHARACTER"))
        (QFILE-OO-STRING (AND (EQ real-DIRECTION :OUTPUT) IF-EXISTS-P)
                         " IF-EXISTS ")
        (IF (AND (EQ real-DIRECTION :OUTPUT) IF-EXISTS-P)
            (IF (EQ real-IF-EXISTS NIL) "ERROR" real-IF-EXISTS) "")
        (QFILE-IF-DOES-NOT-EXIST-STRING real-DIRECTION real-IF-DOES-NOT-EXIST IF-EXISTS-P)
        (QFILE-OO-STRING INHIBIT-LINKS " INHIBIT-LINKS")
        (IF (NEQ real-BYTE-SIZE :DEFAULT)
            (FORMAT NIL " BYTE-SIZE ~D" real-BYTE-SIZE)
          "")
        (QFILE-OO-STRING DELETED " DELETED")
        (IF ESTIMATED-LENGTH
            (FORMAT NIL " ESTIMATED-LENGTH ~D" ESTIMATED-LENGTH)
          "")
        (QFILE-OO-STRING TEMPORARY " TEMPORARY")
        (QFILE-OO-STRING RAW " RAW")
        (QFILE-OO-STRING SUPER-IMAGE " SUPER")
        (QFILE-OO-STRING PRESERVE-DATES " PRESERVE-DATES")
        (QFILE-OO-STRING SUBMIT " SUBMIT")
        #/NEWLINE
        (FILE-PRINT-PATHNAME FILE)
        #/NEWLINE))

(defmethod (lispm-qfile-access :open-command) (hu handle file characters byte-size direction
                                               if-exists if-exists-p if-does-not-exist
                                               &rest options)
  (declare (ignore characters byte-size))
  (SEND hu :COMMAND NIL handle nil "LispM Open" "OPEN-FOR-LISPM "
        #/NEWLINE
        (FILE-PRINT-PATHNAME FILE) #/NEWLINE
        (QFILE-LISPM-OPEN-OPTIONS-STRING
          DIRECTION OPTIONS IF-EXISTS IF-EXISTS-P IF-DOES-NOT-EXIST)))

;;;; Stream generating functions
(DEFUN OPEN-CHAOS (ACCESS FILE PATHNAME &REST OPTIONS
                   &KEY (DIRECTION :INPUT)
                        (CHARACTERS T)
                        (ERROR T)
                        (ELEMENT-TYPE 'STRING-CHAR ELEMENT-TYPE-P)
                        (IF-EXISTS nil if-exists-p)
                        (IF-DOES-NOT-EXIST nil if-does-not-exist-p)
                        (BYTE-SIZE :DEFAULT)
                        MOBY-MAPPED
                   &ALLOW-OTHER-KEYS
                   &AUX HOST-UNIT DATA-CONN PKT NOT-ABORTED
                        PHONY-CHARACTERS SIGN-EXTEND-BYTES
                        (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (SETQ PATHNAME (FS:PARSE-PATHNAME PATHNAME))
  (IF IF-EXISTS-P
      (CHECK-TYPE IF-EXISTS (MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE
                                    :OVERWRITE :APPEND :TRUNCATE :SUPERSEDE NIL))
    (SETQ IF-EXISTS (IF (MEMQ (PATHNAME-VERSION PATHNAME)
                              ;; :UNSPECIFIC here is to prevent lossage
                              ;; writing ITS files with no version numbers.
                              '(:NEWEST :UNSPECIFIC))
                        :NEW-VERSION :ERROR)))
  (IF IF-DOES-NOT-EXIST-P
      (CHECK-TYPE IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
    (SETQ IF-DOES-NOT-EXIST
          (COND ((MEMQ DIRECTION '(:PROBE :PROBE-LINK :PROBE-DIRECTORY))
                 NIL)
                ((AND (EQ DIRECTION :OUTPUT)
                      (NOT (MEMQ IF-EXISTS '(:OVERWRITE :TRUNCATE :APPEND))))
                 :CREATE)
                ;; Note: if DIRECTION is NIL, this defaults to :ERROR
                ;; for compatibility with the past.
                ;; A Common-Lisp program would use :PROBE
                ;; and get NIL as the default for this.
                (T :ERROR))))
  (ECASE DIRECTION
    ((:INPUT :OUTPUT :PROBE-DIRECTORY :PROBE-LINK))
    (:IO (FERROR "Bidirectional file streams are not yet supported."))
    ((NIL :PROBE)))
  ;; IF-EXISTS-P is T if we need to give the IF-EXISTS to the server.
  (SETQ IF-EXISTS-P
        (NOT (MEMQ IF-EXISTS
                   (CASE (PATHNAME-VERSION PATHNAME)
                     (:NEWEST '(:NEW-VERSION))
                     (:UNSPECIFIC '(:NEW-VERSION :SUPERSEDE))))))
  (WHEN ELEMENT-TYPE-P
    (SETF (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES)
          (DECODE-ELEMENT-TYPE ELEMENT-TYPE BYTE-SIZE)))
  (flet ((make-stream (string)
           (LET ((PROPERTIES (READ-FILE-PROPERTY-LIST-STRING STRING "OPEN" PATHNAME)))
             (AND (EQ CHARACTERS :DEFAULT)
                  (SETQ CHARACTERS (GETF PROPERTIES :CHARACTERS)))
             (UNLESS (OR (EQ BYTE-SIZE :DEFAULT)
                         (GETF PROPERTIES :BYTE-SIZE))
               (SETF (GETF PROPERTIES :BYTE-SIZE) BYTE-SIZE))
             (PROG1
               (cond (moby-mapped
                      (open-chaos-moby-mapped host-unit properties string))  ;defined in moby stuff.
                     (t
                      (MAKE-INSTANCE (CASE DIRECTION
                                (:INPUT
                                 (IF CHARACTERS
                                     'QFILE-INPUT-CHARACTER-STREAM
                                   (COND (SIGN-EXTEND-BYTES
                                          'QFILE-INPUT-SIGNED-BINARY-STREAM)
                                         (PHONY-CHARACTERS
                                          'QFILE-INPUT-PHONY-CHARACTER-STREAM)
                                         (T
                                          'QFILE-INPUT-BINARY-STREAM))))
                                (:OUTPUT
                                 (IF CHARACTERS
                                     'QFILE-OUTPUT-CHARACTER-STREAM
                                   (IF PHONY-CHARACTERS
                                       'QFILE-OUTPUT-PHONY-CHARACTER-STREAM
                                     'QFILE-OUTPUT-BINARY-STREAM)))
                                (T 'QFILE-PROBE-STREAM))
                              :HOST-UNIT HOST-UNIT
                              :DATA-CONNECTION DATA-CONN
                              :PROPERTY-LIST PROPERTIES
                              :PATHNAME PATHNAME)))
               (SETQ NOT-ABORTED T))))
         (data-connection-handle ()
           (CASE DIRECTION
             (:INPUT (DATA-INPUT-HANDLE DATA-CONN))
             (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONN)))))
    (HANDLING-FILE-ERRORS (ERROR)
      (PROGN (IF (or moby-mapped (MEMQ DIRECTION '(NIL :probe :PROBE-DIRECTORY :PROBE-LINK)))
                   ;; PROBE mode implies no need for data connection
                     (SETQ HOST-UNIT (SEND ACCESS :GET-HOST-UNIT))
                   (MULTIPLE-VALUE (DATA-CONN HOST-UNIT)
                     (SEND ACCESS :GET-DATA-CONNECTION DIRECTION)))
             (UNWIND-PROTECT
                 (let ((success nil) (string nil))
                   (MULTIPLE-VALUE (PKT SUCCESS STRING)
                     (lexpr-send access :open-command host-unit (data-connection-handle)
                                 file characters byte-size direction
                                 if-exists if-exists-p if-does-not-exist options))
                   (SETQ STRING (STRING-APPEND STRING))
                   (AND PKT (CHAOS:RETURN-PKT PKT))
                   (COND ((NOT SUCCESS)
                          (SETQ NOT-ABORTED T)
                          (OR (NULL DATA-CONN)
                              (SETF (DATA-STREAM DATA-CONN DIRECTION) NIL))
                          (CONDITION-CASE-IF (NOT IF-DOES-NOT-EXIST)
                                             ()
                              (CONDITION-CASE-IF (NOT IF-EXISTS)
                                                 ()
                                  (QFILE-PROCESS-ERROR-NEW STRING PATHNAME NIL (NOT ERROR) :OPEN)
                                (FILE-ALREADY-EXISTS NIL))
                            (FILE-NOT-FOUND NIL)))
                         (T (make-stream string))))
               ;; cleanup forms of the unwind-protect
               (UNLESS (OR NOT-ABORTED
                           (NULL DATA-CONN)
                           (NULL (SEND HOST-UNIT :CONTROL-CONNECTION)))
                 ;; Here if aborted out of it and server may have file open.
                 (CONDITION-CASE ()
                     (PROGN
                      (when (EQ DIRECTION :OUTPUT)
                        (let ((pkt (SEND HOST-UNIT :COMMAND NIL (DATA-OUTPUT-HANDLE DATA-CONN) NIL "Delete"
                                         "DELETE")))
                          (and pkt (chaos:return-pkt pkt))))
                      (multiple-value-bind (pkt success ignore)
                          (SEND HOST-UNIT :COMMAND NIL (data-connection-handle) NIL "Close" "CLOSE")
                        (and pkt (chaos:return-pkt pkt))
                        (WHEN success
                          (CASE DIRECTION
                            (:INPUT (READ-UNTIL-SYNCHRONOUS-MARK (DATA-CONNECTION DATA-CONN)))
                            (:OUTPUT (CHAOS:SEND-PKT (DATA-CONNECTION DATA-CONN)
                                                     (CHAOS:GET-PKT) %QFILE-SYNCHRONOUS-MARK-OPCODE)))))
                      (SEND HOST-UNIT :FREE-DATA-CONNECTION DATA-CONN DIRECTION))
                   (SYS:HOST-STOPPED-RESPONDING NIL))))))))

(DEFUN DECODE-ELEMENT-TYPE (ELEMENT-TYPE BYTE-SIZE)
  (DECLARE (VALUES CHARACTERS BYTE-SIZE PHONY-CHARACTERS SIGN-EXTEND-BYTES))
  (IF (ATOM ELEMENT-TYPE)
      (CASE ELEMENT-TYPE
        (:DEFAULT
         (VALUES :DEFAULT BYTE-SIZE))
        (BIT
         (VALUES NIL 1))
;No way to find out what byte size was used in this case.
;       (SIGNED-BYTE (ferror ...)
        (UNSIGNED-BYTE
         (VALUES NIL :DEFAULT))
        (STRING-CHAR
         (VALUES T :DEFAULT))
        (STANDARD-CHAR
         (VALUES T :DEFAULT))
        (CHARACTER
         (VALUES NIL 16. T))
        (T (FERROR 'FS:UNIMPLEMENTED-OPTION "~S is not implemented as an ELEMENT-TYPE."
                   ELEMENT-TYPE)))
    (CASE (CAR ELEMENT-TYPE)
      (UNSIGNED-BYTE
       (VALUES NIL (CADR ELEMENT-TYPE) NIL NIL))
      (SIGNED-BYTE
       (VALUES NIL (CADR ELEMENT-TYPE) NIL T))
      (MOD
       (VALUES NIL (HAULONG (1- (CADR ELEMENT-TYPE)))))
      (T (FERROR 'FS:UNIMPLEMENTED-OPTION "~S is not implemented as an ELEMENT-TYPE."
                 ELEMENT-TYPE)))))

(DEFUN READ-UNTIL-SYNCHRONOUS-MARK (CONN)
  "Discard data from chaosnet connection CONN up thru synchronous mark.
Used on file data connections when there is no stream yet."
  (DO (PKT DONE) (DONE)
    (SETQ PKT (CHAOS:GET-NEXT-PKT CONN NIL "File Input"))
    (SELECT (CHAOS:PKT-OPCODE PKT)
      ;; No data, but a synchronous mark
      (%QFILE-SYNCHRONOUS-MARK-OPCODE
       (CHAOS:RETURN-PKT PKT)
       (RETURN NIL))
      ;; Received an asynchronous mark, meaning some sort of error condition
      ((%QFILE-ASYNCHRONOUS-MARK-OPCODE %QFILE-EOF-OPCODE
        %QFILE-BINARY-OPCODE %QFILE-CHARACTER-OPCODE)
       NIL)
      ;; Connection closed or broken with message
      ((CHAOS:CLS-OP CHAOS:LOS-OP)
       (CHAOS:REPORT-BAD-CONNECTION-STATE CONN "read file data from"))
      ;; Not a recognized opcode, huh?
      (OTHERWISE
       (FERROR NIL "Receieved data packet (~S) with illegal opcode for file data conn."
               PKT)))
    (CHAOS:RETURN-PKT PKT)))

(DEFUN MULTIPLE-PLISTS-CHAOS (ACCESS PATHNAMES OPTIONS
                              &AUX FILE-LIST CONNECTION (CHARACTERS T)
                              (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (LOOP FOR (IND OPT) ON OPTIONS BY 'CDDR
        DO (CASE IND
             (:CHARACTERS (SETQ CHARACTERS OPT))
             (OTHERWISE (FERROR NIL "~S is not a known MULTIPLE-FILE-PLISTS option" IND))))
  (SETQ CONNECTION (QFILE-HOST-UNIT-CONTROL-CONNECTION (SEND ACCESS :GET-HOST-UNIT)))
  (SETQ FILE-LIST (LOOP FOR PATHNAME IN PATHNAMES
                        COLLECT (LIST PATHNAME NIL)))
  (DO ((LIST-TO-DO FILE-LIST (CDR LIST-TO-DO))
       (PENDING-LIST (COPYLIST FILE-LIST))
       (ELEM-TO-DO))
      ((NULL PENDING-LIST))
    (SETQ ELEM-TO-DO (CAR LIST-TO-DO))
    (DO ((P-L PENDING-LIST (CDR P-L))
         (ELEM))
        ((OR (NULL P-L)
             (AND ELEM-TO-DO
                  (NOT (CHAOS:DATA-AVAILABLE CONNECTION))
                  (CHAOS:MAY-TRANSMIT CONNECTION))))
      (SETQ ELEM (CAR P-L))
      (LET ((TRANSACTION-ID (SECOND ELEM)))
        (AND TRANSACTION-ID
             (LET* ((PKT (FILE-WAIT-FOR-TRANSACTION TRANSACTION-ID CONNECTION "Probe"))
                    (PKT-STRING (CHAOS:PKT-STRING PKT))
                    (STRING (NSUBSTRING PKT-STRING
                                        (1+ (STRING-SEARCH-CHAR #/SPACE PKT-STRING))))
                    (FROM (1+ (STRING-SEARCH-SET '(#/SPACE #/NEWLINE) STRING)))
                    (SUCCESS (NOT (STRING-EQUAL "ERROR" STRING
                                                :START2 FROM
                                                :END2 (STRING-SEARCH-SET
                                                        '(#/SPACE #/NEWLINE) STRING FROM))))
                    (PROPERTY-LIST NIL))
               (AND SUCCESS (SETQ PROPERTY-LIST (READ-FILE-PROPERTY-LIST-STRING
                                                  STRING "OPEN" (FIRST ELEM))))
               (CHAOS:RETURN-PKT PKT)
               (SETF (CDR ELEM) PROPERTY-LIST)
               (SETQ PENDING-LIST (DELQ ELEM PENDING-LIST))))))
    (AND ELEM-TO-DO
         (LET ((MODE (CASE CHARACTERS
                       ((NIL) :BINARY)
                       (:DEFAULT :DEFAULT)
                       (T :CHARACTER)))
               (PKT (CHAOS:GET-PKT))
               (TRANSACTION-ID (FILE-MAKE-TRANSACTION-ID NIL)))
           (CHAOS:SET-PKT-STRING PKT TRANSACTION-ID
                                     "  OPEN PROBE " MODE #/NEWLINE
                                     (FILE-PRINT-PATHNAME (FIRST ELEM-TO-DO)) #/NEWLINE)
           (CHAOS:SEND-PKT CONNECTION PKT %QFILE-COMMAND-OPCODE)
           (SETF (SECOND ELEM-TO-DO) TRANSACTION-ID))))
  FILE-LIST)

;;; This returns a directory stream.  Can be used with DIRECTORY and PROPERTIES commands
(DEFUN MAKE-FILE-PROPERTY-LIST-STREAM-CHAOS (ACCESS COMMAND WHOSTATE
                                             STRING-ARG TOKEN-ARGS PATHNAME NO-ERROR-P
                                             &AUX DATA-CONN HOST-UNIT PKT SUCCESS NOT-ABORTED
                                             STRING
                                             (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (MULTIPLE-VALUE (DATA-CONN HOST-UNIT)
    (SEND ACCESS :GET-DATA-CONNECTION :INPUT))
  (UNWIND-PROTECT
      (PROGN
        (MULTIPLE-VALUE-SETQ (PKT SUCCESS STRING)
          (SEND HOST-UNIT :COMMAND NIL (DATA-INPUT-HANDLE DATA-CONN) NIL WHOSTATE
                                   COMMAND
                                   TOKEN-ARGS #/NEWLINE
                                   STRING-ARG #/NEWLINE))
        (COND ((NOT SUCCESS)
               (SETQ NOT-ABORTED T)
               (SETQ STRING (STRING-APPEND STRING))
               (SETF (DATA-STREAM DATA-CONN :INPUT) NIL)
               (QFILE-PROCESS-ERROR-NEW STRING PATHNAME NIL NO-ERROR-P :DIRECTORY-STREAM))
              (T
               (QFILE-CHECK-COMMAND COMMAND STRING)
               (PROG1 (MAKE-INSTANCE 'QFILE-DIRECTORY-STREAM
                                     :HOST-UNIT HOST-UNIT
                                     :DATA-CONNECTION DATA-CONN
                                     :PATHNAME PATHNAME)
                      (SETQ NOT-ABORTED T)))))
    (AND PKT (CHAOS:RETURN-PKT PKT))
    ;; Both success and failure set NOT-ABORTED once they get past critical section.
    (UNLESS (OR NOT-ABORTED
                (NULL DATA-CONN)
                (NULL (SEND HOST-UNIT :CONTROL-CONNECTION)))
      ;; Here if aborted out of it and server may have directory stream open.
      (CONDITION-CASE ()
          (MULTIPLE-VALUE-BIND (pkt CLOSE-SUCCESS)
              (SEND HOST-UNIT :COMMAND NIL (DATA-INPUT-HANDLE DATA-CONN) NIL "Close" "CLOSE")
            (and pkt (chaos:return-pkt pkt))
            (WHEN CLOSE-SUCCESS
              (READ-UNTIL-SYNCHRONOUS-MARK (DATA-CONNECTION DATA-CONN)))
            (SEND HOST-UNIT :FREE-DATA-CONNECTION DATA-CONN :INPUT))
        (SYS:HOST-STOPPED-RESPONDING NIL)))))

;; Copied from LAD: RELEASE-3.NETWORK.CHAOS; QFILE.LISP#379 on 2-Oct-86 23:54:10
(DEFUN DIRECTORY-CHAOS (ACCESS PATHNAME OPTIONS
                        &AUX (NO-ERROR-P NIL) (DELETED-P NIL) (FAST-P NIL) (DIRS-ONLY-P NIL)
                             (NO-EXTRA-INFO NIL) (SORTED-P NIL))
  (DO ((L OPTIONS (CDR L)))
      ((NULL L))
    (CASE (CAR L)
      (:NOERROR (SETQ NO-ERROR-P T))
      (:FAST (SETQ FAST-P T))
      (:NO-EXTRA-INFO
       (WHEN (NOT (TYPEP (SEND ACCESS :HOST) 'FS:UNIX-HOST))
         (SETQ NO-EXTRA-INFO T)))
      (:SORTED (SETQ SORTED-P T))
      ;; This is for the :ALL-DIRECTORIES message
      (:DIRECTORIES-ONLY (SETQ DIRS-ONLY-P T))
      (:DELETED (SETQ DELETED-P T))
      (OTHERWISE (FERROR NIL "~S is not a known DIRECTORY option" (CAR L)))))
  (HANDLING-FILE-ERRORS ((NOT NO-ERROR-P))
    (MAKE-FILE-PROPERTY-LIST-STREAM-CHAOS ACCESS "DIRECTORY" "Directory"
      (FILE-PRINT-PATHNAME PATHNAME)
      (FORMAT NIL "~:[~; DELETED~]~:[~; FAST~]~:[~; DIRECTORIES-ONLY~]~:[~; NO-EXTRA-INFO~]~
                   ~:[~; SORTED~]"
              DELETED-P FAST-P DIRS-ONLY-P NO-EXTRA-INFO SORTED-P)
      PATHNAME NO-ERROR-P)))

(DEFMETHOD (BASIC-QFILE-ACCESS :DIRECTORY-STREAM) (PATHNAME OPTIONS)
  (DIRECTORY-CHAOS SELF PATHNAME OPTIONS))

(DEFUN PROPERTIES-CHAOS (ACCESS TYPE THING ERROR-P
                         &AUX (PATHNAME
                                (CASE TYPE
                                  (:FILE THING)
                                  (:STREAM (SEND THING :TRUENAME))))
                         SETTABLE-PROPERTIES GOT-ERROR PLIST)
  "TYPE is either :FILE or :STREAM."
  (DECLARE (VALUES PLIST SETTABLE-PROPERTIES))
  (WITH-OPEN-STREAM-CASE
    (S (MAKE-FILE-PROPERTY-LIST-STREAM-CHAOS
         ACCESS "PROPERTIES" "Properties"
         (IF (EQ TYPE :FILE) (FILE-PRINT-PATHNAME THING) "")
         (IF (EQ TYPE :STREAM) (SEND THING :FILE-HANDLE) "")
         PATHNAME
         ()))
    (ERROR (IF (NOT ERROR-P) (SETQ GOT-ERROR S) (SIGNAL-CONDITION S)))
    (:NO-ERROR
     (SETQ SETTABLE-PROPERTIES (FS:PARSE-SETTABLE-PROPERTIES (SEND S :LINE-IN) 0))
     (SETQ PLIST (SEND :ACCESS :READ-DIRECTORY-STREAM-ENTRY S PATHNAME))))
  (OR GOT-ERROR (VALUES PLIST SETTABLE-PROPERTIES)))

#| ;;; Don't install this until most FILE servers make the PROPERTIES command work
(DEFMETHOD (BASIC-QFILE-ACCESS :PROPERTIES) (FILE &OPTIONAL (ERROR-P T))
  (PROPERTIES-CHAOS SELF :FILE FILE ERROR-P))
|#



(DEFUN QFILE-PROCESS-ASYNC-MARK (PKT)
  (LET ((STRING (NSUBSTRING (CHAOS:PKT-STRING PKT)
                            (1+ (STRING-SEARCH-CHAR #/SPACE (CHAOS:PKT-STRING PKT))))))
    (QFILE-PROCESS-ERROR-NEW STRING SELF '(:NO-ACTION)))        ;Process error allowing proceeding
  ;; If user says to continue, attempt to do so.
  (SEND SELF :CONTINUE))

(DEFFLAVOR QFILE-STREAM-MIXIN
        (HOST-UNIT
         STATUS)
        (SI:PROPERTY-LIST-MIXIN SI:FILE-STREAM-MIXIN)
  (:INITABLE-INSTANCE-VARIABLES HOST-UNIT))

(DEFMETHOD (QFILE-STREAM-MIXIN :QFASLP) () (GETF SI:PROPERTY-LIST :QFASLP))

(DEFMETHOD (QFILE-STREAM-MIXIN :TRUENAME) () (GETF SI:PROPERTY-LIST :TRUENAME))

(DEFMETHOD (QFILE-STREAM-MIXIN :LENGTH) () (GETF SI:PROPERTY-LIST :LENGTH))

(DEFMETHOD (QFILE-STREAM-MIXIN :PROPERTIES) (&OPTIONAL (ERROR-P T))
  (HANDLING-FILE-ERRORS (ERROR-P)
    (IF (EQ STATUS :CLOSED)
        (PROPERTIES-CHAOS (SEND HOST-UNIT :ACCESS) :FILE (SEND SELF :TRUENAME) ERROR-P)
      (PROPERTIES-CHAOS (SEND HOST-UNIT :ACCESS) :STREAM SELF ERROR-P))))

(DEFUN QFILE-PROCESS-OUTPUT-ASYNC-MARK ()
  (DECLARE (:SELF-FLAVOR QFILE-STREAM-MIXIN))
  (LET ((PKT (CAR (REMF SI:PROPERTY-LIST 'ASYNC-MARK-PKT))))
    (OR PKT (FERROR NIL "Output stream ~S in ASYNC-MARKED state, but no async mark pkt" SELF))
    (UNWIND-PROTECT
      (QFILE-PROCESS-ASYNC-MARK PKT)
      (CHAOS:RETURN-PKT PKT))))

;;; Flavors that really have an open connection
;;; STATUS is one of
;;;  :OPEN - a file is currently open on this channel
;;;  :CLOSED - no file is open, but the channel exists
;;;  :EOF - a file is open, but is at its end (no more data available).
;;;  :SYNC-MARKED - a mark that was requested has been received
;;;  :ASYNC-MARKED - an asynchronous (error) mark has been received
(DEFFLAVOR QFILE-DATA-STREAM-MIXIN
        ((STATUS :OPEN)
         DATA-CONNECTION
         FILE-HANDLE
         CHAOS:CONNECTION)
        (QFILE-STREAM-MIXIN)
  (:INCLUDED-FLAVORS SI:FILE-DATA-STREAM-MIXIN)
  (:SETTABLE-INSTANCE-VARIABLES STATUS)
  (:GETTABLE-INSTANCE-VARIABLES FILE-HANDLE)
  (:INITABLE-INSTANCE-VARIABLES DATA-CONNECTION))

(DEFFLAVOR QFILE-INPUT-STREAM-MIXIN
        (CHAOS:INPUT-PACKET)
        (QFILE-DATA-STREAM-MIXIN)
  (:INCLUDED-FLAVORS SI:INPUT-FILE-STREAM-MIXIN))

(DEFUN QFILE-NEXT-READ-PKT (NO-HANG-P FOR-SYNC-MARK-P)
  (DECLARE (:SELF-FLAVOR QFILE-INPUT-STREAM-MIXIN))
  (CASE (IF FOR-SYNC-MARK-P :EOF STATUS)
    ((:OPEN :EOF)
     (LET ((PKT (CHAOS:GET-NEXT-PKT CHAOS:CONNECTION NO-HANG-P "File Input")))
       (WHEN PKT
         (SELECT (CHAOS:PKT-OPCODE PKT)
           ;; Received some sort of data, return it
           ((%QFILE-BINARY-OPCODE %QFILE-CHARACTER-OPCODE)
            PKT)

           ;; No data, but a synchronous mark
           (%QFILE-SYNCHRONOUS-MARK-OPCODE
            (SETQ STATUS :SYNC-MARKED)
            (CHAOS:RETURN-PKT PKT)
            NIL)

           ;; Received an asynchronous mark, meaning some sort of error condition
           (%QFILE-ASYNCHRONOUS-MARK-OPCODE
            (SETQ STATUS :ASYNC-MARKED)
            (OR FOR-SYNC-MARK-P (QFILE-PROCESS-ASYNC-MARK PKT))
            (CHAOS:RETURN-PKT PKT)
            NIL)

           ;; EOF received, change channel state and return
           (%QFILE-EOF-OPCODE
            (SETQ STATUS :EOF)
            (CHAOS:RETURN-PKT PKT)
            NIL)

           ;; Connection closed or broken with message
           ((CHAOS:CLS-OP CHAOS:LOS-OP)
            (CHAOS:REPORT-BAD-CONNECTION-STATE CHAOS:CONNECTION "read file data from"))

           ;; Not a recognized opcode, huh?
           (OTHERWISE
            (FERROR NIL "Receieved data packet (~S) with illegal opcode for ~S."
                    PKT SELF))))))
    (:CLOSED
     (FERROR 'SYS:STREAM-CLOSED "Attempt to read from ~S, which is closed." SELF))
    ((:ASYNC-MARKED :SYNC-MARKED)
     (FERROR 'SYS:STREAM-INVALID
             "Attempt to read from ~S, which is in a marked state." SELF))
    (OTHERWISE
     (FERROR 'SYS:STREAM-INVALID
             "Attempt to read from ~S, which is in illegal state ~S." SELF STATUS))))

(DEFFLAVOR QFILE-OUTPUT-STREAM-MIXIN
        ()
        (QFILE-DATA-STREAM-MIXIN)
  (:REQUIRED-METHODS :SEND-PKT-BUFFER)
  (:INCLUDED-FLAVORS SI:OUTPUT-FILE-STREAM-MIXIN))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :BEFORE :INIT) (IGNORE)
  (LET ((DIRECTION (SEND SELF :DIRECTION)))
    (SETF (DATA-STREAM DATA-CONNECTION DIRECTION) SELF)
    (SETQ FILE-HANDLE (DATA-HANDLE DATA-CONNECTION DIRECTION)
          CHAOS:CONNECTION (DATA-CONNECTION DATA-CONNECTION))))

;;; Stream version of host unit :COMMAND, supplies file handle itself.
;;; MARK-P is just T or NIL.
(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :COMMAND) (MARK-P WHOSTATE COM &REST STRINGS)
  (DECLARE (VALUES STRING SUCCESS))
  (MULTIPLE-VALUE-BIND (PKT SUCCESS STRING)
      (LEXPR-SEND HOST-UNIT :COMMAND MARK-P SELF NIL WHOSTATE COM STRINGS)
    (LET ((DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
      (SETQ STRING (COPY-SEQ STRING)))
    (AND PKT (CHAOS:RETURN-PKT PKT))
    (VALUES STRING SUCCESS)))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :SAFE-TO-USE-P) ()
  (IF (NEQ (CHAOS:STATE (QFILE-HOST-UNIT-CONTROL-CONNECTION HOST-UNIT)) 'CHAOS:OPEN-STATE)
      (SETQ STATUS :CLOSED))                    ; ????
  (EQ STATUS :OPEN))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :CLOSE) (&OPTIONAL ABORTP)
  (COND ((EQ STATUS :CLOSED) NIL)
        ((OR (NULL (QFILE-HOST-UNIT-CONTROL-CONNECTION HOST-UNIT))
             (NEQ (CHAOS:STATE (QFILE-HOST-UNIT-CONTROL-CONNECTION HOST-UNIT))
                  'CHAOS:OPEN-STATE))
         (SETQ STATUS :CLOSED)
         T)
        (T
         (SEND SELF :REAL-CLOSE ABORTP))))

(DEFMETHOD (QFILE-INPUT-STREAM-MIXIN :REAL-CLOSE) (ABORTP &AUX SUCCESS STRING)
  (DECLARE (IGNORE ABORTP))
  (IF (NEQ STATUS :EOF)
      (MULTIPLE-VALUE (STRING SUCCESS)
        (SEND SELF :COMMAND T "Close" "CLOSE"))
    (SEND HOST-UNIT :COMMAND T SELF T "Close" "CLOSE")
    (SETQ SUCCESS T))
  (SEND HOST-UNIT :FREE-DATA-CONNECTION DATA-CONNECTION :INPUT)
  (SETQ STATUS :CLOSED)
  (COND (SUCCESS
         ;; For sake of Twenex, look for new truename after a rename.
         (IF (AND STRING (STRING-SEARCH-CHAR #/NEWLINE STRING))
             (SETQ SI:PROPERTY-LIST
                   (NCONC (READ-FILE-PROPERTY-LIST-STRING STRING "CLOSE" PATHNAME)
                          SI:PROPERTY-LIST)))
         T)
        (STRING
         (QFILE-PROCESS-ERROR-NEW STRING SELF '(:NO-ACTION) NIL :CLOSE))))

(DEFMETHOD (QFILE-OUTPUT-STREAM-MIXIN :SEND-OUTPUT-BUFFER) (&REST ARGS)
  (LOOP DOING
    (CASE STATUS
      ((:OPEN :EOF)
       (PROCESS-WAIT "File Output"
                     #'(LAMBDA (STAT CONNECTION)
                         (OR (EQ (CAR STAT) :ASYNC-MARKED)
                             (CHAOS:MAY-TRANSMIT CONNECTION)
                             (NEQ (CHAOS:STATE CONNECTION) 'CHAOS:OPEN-STATE)))
                     (LOCATE-IN-INSTANCE SELF 'STATUS) CHAOS:CONNECTION)
       (AND (NEQ (CHAOS:STATE CHAOS:CONNECTION) 'CHAOS:OPEN-STATE)
            (CHAOS:REPORT-BAD-CONNECTION-STATE CHAOS:CONNECTION
                                               "output to file data connection"))
       (AND (NEQ STATUS :ASYNC-MARKED)
            (RETURN (LEXPR-SEND SELF :SEND-PKT-BUFFER ARGS))))
      (:ASYNC-MARKED
       (QFILE-PROCESS-OUTPUT-ASYNC-MARK))
      (:CLOSED
       (FERROR 'SYS:STREAM-CLOSED
               "Attempt to output to ~S, which is closed." SELF))
      (OTHERWISE
       (FERROR 'SYS:STREAM-INVALID
               "Attempt to output to ~S, which is in illegal state ~S." SELF STATUS)))))

;;; Sent from inside the interrupt function, change our status and remember error message.
(DEFMETHOD (QFILE-OUTPUT-STREAM-MIXIN :ASYNC-MARK) (PKT)
  (SETF (GETF SI:PROPERTY-LIST 'ASYNC-MARK-PKT) PKT)
  (SETQ STATUS :ASYNC-MARKED))

(DEFMETHOD (QFILE-INPUT-STREAM-MIXIN :READ-UNTIL-SYNCHRONOUS-MARK) ()
  (LOOP UNTIL (EQ STATUS :SYNC-MARKED)
        AS PKT = (QFILE-NEXT-READ-PKT NIL T)
        WHEN PKT DO (CHAOS:RETURN-PKT PKT)
        FINALLY (SETQ STATUS :OPEN)))

(DEFMETHOD (QFILE-INPUT-STREAM-MIXIN :GET-NEXT-INPUT-PKT) (&OPTIONAL NO-HANG-P)
  (LOOP WHEN (EQ STATUS :EOF) RETURN NIL
        THEREIS (SETQ CHAOS:INPUT-PACKET (QFILE-NEXT-READ-PKT NO-HANG-P NIL))))

(DEFMETHOD (QFILE-OUTPUT-STREAM-MIXIN :WRITE-SYNCHRONOUS-MARK) ()
  (LET-GLOBALLY ((STATUS :EOF))         ;In case :ASYNC-MARK now
    (SEND SELF :FORCE-OUTPUT))          ;Send any partial buffer
  (CHAOS:SEND-PKT CHAOS:CONNECTION (CHAOS:GET-PKT) %QFILE-SYNCHRONOUS-MARK-OPCODE))

(DEFMETHOD (QFILE-OUTPUT-STREAM-MIXIN :REAL-CLOSE) (ABORTP &AUX SUCCESS STRING)
  ;; Closing an open output channel.  Finish sending the data.
  (AND (EQ STATUS :OPEN) (SEND SELF :EOF))
  ;; If aborting out of a file-writing operation before normal :CLOSE,
  ;; delete the incomplete file.  Don't worry if it gets an error.
  (AND (EQ ABORTP :ABORT)
       (SEND SELF :COMMAND NIL "Delete" "DELETE"))
  (MULTIPLE-VALUE (STRING SUCCESS)
    (SEND SELF :COMMAND T "Close" "CLOSE"))
  (SEND HOST-UNIT :FREE-DATA-CONNECTION DATA-CONNECTION :OUTPUT)
  (SETQ STATUS :CLOSED)
  (COND (SUCCESS
         (SETQ SI:PROPERTY-LIST
               (NCONC (READ-FILE-PROPERTY-LIST-STRING STRING "CLOSE" PATHNAME
                                                      '((:CREATION-DATE) (:CREATION-TIME)
                                                        (:LENGTH T) (:LISPM-LENGTH T)))
                      SI:PROPERTY-LIST))
         T)
        (T
         (QFILE-PROCESS-ERROR-NEW STRING SELF '(:NO-ACTION) NIL :CLOSE))))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :DELETE) (&OPTIONAL (ERROR-P T) &AUX SUCCESS STRING)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (CASE STATUS
      ((:OPEN :EOF :SYNC-MARKED :ASYNC-MARKED)
       (MULTIPLE-VALUE (STRING SUCCESS)
         (SEND SELF :COMMAND NIL "Delete" "DELETE"))
       (OR SUCCESS
           (QFILE-PROCESS-ERROR-NEW STRING SELF NIL (NOT ERROR-P) :DELETE)))
      (OTHERWISE (FERROR NIL "~S in illegal state for delete." SELF)))))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :RENAME) (NEW-NAME &OPTIONAL (ERROR-P T)
                                             &AUX SUCCESS STRING)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (CASE STATUS
      ((:OPEN :EOF :SYNC-MARKED :ASYNC-MARKED)
       (MULTIPLE-VALUE (STRING SUCCESS)
         (SEND SELF :COMMAND NIL "Rename"
                                 "RENAME" #/NEWLINE
                                  (FILE-PRINT-PATHNAME NEW-NAME) #/NEWLINE))
       (COND (SUCCESS
              ;; If there is a second line coming from the file server,
              ;; it is the new truename.
              (LET* ((FROM (STRING-SEARCH #/NEWLINE STRING)))
                (WHEN FROM
                  (send self :putprop (fs:parse-pathname
                                        string
                                        (send (get self :truename) :host) nil
                                        (1+ from) (string-search #/newline string (1+ from)))
                        :truename)))
              (SETQ PATHNAME NEW-NAME)
              (SEND TV::WHO-LINE-FILE-STATE-SHEET :CLOBBERED)
              T)
             (T (QFILE-PROCESS-ERROR-NEW STRING SELF NIL (NOT ERROR-P) :RENAME))))
      (OTHERWISE (FERROR NIL "~S in illegal state for rename." SELF)))))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :CHANGE-PROPERTIES)
           (ERROR-P &REST PROPERTIES &AUX SUCCESS STRING)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (CASE STATUS
      ((:OPEN :EOF :SYNC-MARKED :ASYNC-MARKED)
       (MULTIPLE-VALUE (STRING SUCCESS)
         (SEND SELF :COMMAND NIL "Change Properties"
                              (CHANGE-PROPERTIES-STRING PROPERTIES)))
       (OR SUCCESS
           (QFILE-PROCESS-ERROR-NEW STRING SELF NIL (NOT ERROR-P) :CHANGE-PROPERTIES)))
      (OTHERWISE (FERROR NIL "~S in illegal state for change properties." SELF)))))

(DEFMETHOD (QFILE-DATA-STREAM-MIXIN :CONTINUE) (&AUX SUCCESS STRING)
  (COND ((EQ STATUS :ASYNC-MARKED)
         (SETF STATUS :OPEN)
         (MULTIPLE-VALUE (STRING SUCCESS)
           (SEND SELF :COMMAND NIL "File Continue" "CONTINUE"))
         (COND ((NULL SUCCESS)
                (SETQ STATUS :ASYNC-MARKED)
                (QFILE-PROCESS-ERROR-NEW STRING SELF))))))      ;not proceedable

(DEFMETHOD (QFILE-INPUT-STREAM-MIXIN :SET-BUFFER-POINTER) (NEW-POINTER &AUX STRING SUCCESS)
  (CASE STATUS
    ((:OPEN :EOF)
     (AND (EQ STATUS :EOF) (SETQ STATUS :OPEN))
     (MULTIPLE-VALUE (STRING SUCCESS)
       (SEND SELF :COMMAND T "File Position"
                            "FILEPOS " (FORMAT NIL "~D" NEW-POINTER)))
     (OR SUCCESS (QFILE-PROCESS-ERROR-NEW STRING SELF NIL NIL :SET-POINTER))    ;Cannot proceed
     NEW-POINTER)
    (OTHERWISE
     (FERROR NIL ":SET-POINTER attempted on ~S which is in state ~S." SELF STATUS))))

(DEFMETHOD (QFILE-OUTPUT-STREAM-MIXIN :FINISH) ()
  (DO () ((CHAOS:CONN-FINISHED-P CHAOS:CONNECTION))
    (PROCESS-WAIT "File Finish"
                  #'(LAMBDA (CONN STAT)
                      (OR (CHAOS:CONN-FINISHED-P CONN)
                          (EQ (CAR STAT) :ASYNC-MARKED)))
                  CHAOS:CONNECTION (LOCATE-IN-INSTANCE SELF 'STATUS))
    (AND (EQ STATUS :ASYNC-MARKED) (QFILE-PROCESS-OUTPUT-ASYNC-MARK))))

(DEFMETHOD (QFILE-OUTPUT-STREAM-MIXIN :EOF) ()
  (SEND SELF :FORCE-OUTPUT)
  (CHAOS:SEND-PKT CHAOS:CONNECTION (CHAOS:GET-PKT) CHAOS:EOF-OP)
  (SETQ STATUS :EOF)
  (SEND SELF :FINISH))

(DEFFLAVOR QFILE-CHARACTER-STREAM-MIXIN () (QFILE-DATA-STREAM-MIXIN))

(DEFMETHOD (QFILE-CHARACTER-STREAM-MIXIN :ELEMENT-TYPE) ()
  'STRING-CHAR)

(DEFFLAVOR QFILE-BINARY-STREAM-MIXIN (CURRENT-BYTE-SIZE) (QFILE-DATA-STREAM-MIXIN))

(DEFMETHOD (QFILE-BINARY-STREAM-MIXIN :AFTER :INIT) (IGNORE)
  (SETQ CURRENT-BYTE-SIZE (GETF SI:PROPERTY-LIST :BYTE-SIZE :DEFAULT)))

(DEFMETHOD (QFILE-BINARY-STREAM-MIXIN :SET-BYTE-SIZE) (NEW-BYTE-SIZE)
  (CHECK-TYPE NEW-BYTE-SIZE (INTEGER 1 16.))
  (SEND SELF :COMMAND T "Set Byte Size"
                       "SET-BYTE-SIZE "
                       (FORMAT NIL "~D ~D" NEW-BYTE-SIZE (SEND SELF :READ-POINTER)))
  (SETQ CURRENT-BYTE-SIZE NEW-BYTE-SIZE)
  NEW-BYTE-SIZE)

;; overridden by signed-byte-stream-mixin if necessary
(DEFMETHOD (QFILE-BINARY-STREAM-MIXIN :ELEMENT-TYPE) ()
  (IF (EQ CURRENT-BYTE-SIZE :DEFAULT)
      'UNSIGNED-BYTE
    `(UNSIGNED-BYTE ,CURRENT-BYTE-SIZE)))

(DEFFLAVOR QFILE-INPUT-CHARACTER-STREAM-MIXIN ()
           (QFILE-INPUT-STREAM-MIXIN QFILE-CHARACTER-STREAM-MIXIN))

(DEFFLAVOR QFILE-INPUT-BINARY-STREAM-MIXIN ()
           (QFILE-INPUT-STREAM-MIXIN QFILE-BINARY-STREAM-MIXIN))

(DEFFLAVOR QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN ()
           (QFILE-INPUT-STREAM-MIXIN QFILE-BINARY-STREAM-MIXIN)
  (:REQUIRED-FLAVORS SI:BASIC-BUFFERED-INPUT-STREAM)
  :INITTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN :ELEMENT-TYPE) ()
  `(SIGNED-BYTE ,CURRENT-BYTE-SIZE))

(DEFMETHOD (QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN :AROUND :TYI) (CONT MT ARGS &REST IGNORE)
  (LET ((BYTE (AROUND-METHOD-CONTINUE CONT MT ARGS)))
    (WHEN BYTE
      (IF (LDB-TEST (BYTE 1 (1- CURRENT-BYTE-SIZE)) BYTE)
          (- BYTE (LSH 1 CURRENT-BYTE-SIZE))
        BYTE))))

(DEFMETHOD (QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN :STRING-IN)
           (EOF STRING &OPTIONAL (START 0) END)
  (OR END (SETQ END (ARRAY-LENGTH STRING)))
  (LOOP WHILE (< START END)
        WHILE (LOOP UNTIL (AND SI:STREAM-INPUT-BUFFER
                               (< SI:STREAM-INPUT-INDEX SI:STREAM-INPUT-LIMIT))
                    ;; Out of input, get some more
                    UNTIL (SEND SELF :SETUP-NEXT-INPUT-BUFFER)
                    DO (AND EOF (FERROR 'SYS:END-OF-FILE-1 "End of file on ~S." SELF))
                    RETURN NIL
                    FINALLY (RETURN T))
        AS AMT = (MIN (- END START) (- SI:STREAM-INPUT-LIMIT SI:STREAM-INPUT-INDEX))
        DO (COPY-ARRAY-PORTION SI:STREAM-INPUT-BUFFER SI:STREAM-INPUT-INDEX
                               (SETQ SI:STREAM-INPUT-INDEX (+ SI:STREAM-INPUT-INDEX AMT))
                               STRING START (SETQ START (+ START AMT)))
        ;; Sign-extend each byte.
        (DO ((I START (1+ I))
             (END1 (+ START AMT)))
            ((= I END1))
          (LET ((BYTE (AREF STRING I)))
            (IF (LDB-TEST (BYTE 1 (1- CURRENT-BYTE-SIZE)) BYTE)
                (SETF (AREF STRING I) (- BYTE (LSH 1 CURRENT-BYTE-SIZE))))))
        FINALLY (AND (ARRAY-HAS-LEADER-P STRING)
                     (SETF (FILL-POINTER STRING) START))
                (RETURN (VALUES START (NULL SI:STREAM-INPUT-BUFFER)))))

(DEFFLAVOR QFILE-INPUT-PHONY-CHARACTER-STREAM-MIXIN ()
           (QFILE-INPUT-STREAM-MIXIN QFILE-CHARACTER-STREAM-MIXIN))

(DEFMETHOD (QFILE-INPUT-PHONY-CHARACTER-STREAM-MIXIN :ELEMENT-TYPE) ()
  'CHARACTER)

(DEFMETHOD (QFILE-INPUT-PHONY-CHARACTER-STREAM-MIXIN :AROUND :TYI) (CONT MT ARGS &REST IGNORE)
  (LET ((CH1 (AROUND-METHOD-CONTINUE CONT MT ARGS))
        (CH2 (AROUND-METHOD-CONTINUE CONT MT ARGS))
        (CH3 (AROUND-METHOD-CONTINUE CONT MT ARGS))
        (CH4 (AROUND-METHOD-CONTINUE CONT MT ARGS)))
    (DPB CH4 #o3010 (DPB CH3 #o2010 (DPB CH2 #o1010 CH1)))))

(DEFFLAVOR QFILE-OUTPUT-CHARACTER-STREAM-MIXIN ()
           (QFILE-OUTPUT-STREAM-MIXIN QFILE-CHARACTER-STREAM-MIXIN))

(DEFFLAVOR QFILE-OUTPUT-PHONY-CHARACTER-STREAM-MIXIN ()
           (QFILE-OUTPUT-BINARY-STREAM-MIXIN))

(DEFMETHOD (QFILE-OUTPUT-PHONY-CHARACTER-STREAM-MIXIN :ELEMENT-TYPE) ()
  'CHARACTER)

(DEFMETHOD (QFILE-OUTPUT-PHONY-CHARACTER-STREAM-MIXIN :AROUND :TYO) (CONT MT ARGS CHAR)
  ARGS
  (FUNCALL-WITH-MAPPING-TABLE CONT MT :TYO (LDB #o0010 CHAR))
  (FUNCALL-WITH-MAPPING-TABLE CONT MT :TYO (LDB #o1010 CHAR))
  (FUNCALL-WITH-MAPPING-TABLE CONT MT :TYO (LDB #o2010 CHAR))
  (FUNCALL-WITH-MAPPING-TABLE CONT MT :TYO (LDB #o3010 CHAR)))

(DEFFLAVOR QFILE-OUTPUT-BINARY-STREAM-MIXIN ()
           (QFILE-OUTPUT-STREAM-MIXIN QFILE-BINARY-STREAM-MIXIN))

(DEFMETHOD (QFILE-OUTPUT-CHARACTER-STREAM-MIXIN :SEND-PKT-BUFFER) CHAOS:SEND-CHARACTER-PKT)

(DEFMETHOD (QFILE-OUTPUT-BINARY-STREAM-MIXIN :SEND-PKT-BUFFER) CHAOS:SEND-BINARY-PKT-ANY)

(DEFFLAVOR QFILE-INPUT-CHARACTER-STREAM
        ()
        (QFILE-INPUT-CHARACTER-STREAM-MIXIN SI:INPUT-FILE-STREAM-MIXIN
         CHAOS:CHARACTER-INPUT-STREAM-MIXIN SI:BUFFERED-INPUT-CHARACTER-STREAM))

(DEFFLAVOR QFILE-INPUT-PHONY-CHARACTER-STREAM
           ()
           (QFILE-INPUT-PHONY-CHARACTER-STREAM-MIXIN SI:INPUT-FILE-STREAM-MIXIN
            CHAOS:BINARY-INPUT-STREAM-MIXIN SI:BUFFERED-TYI-INPUT-STREAM))

(DEFFLAVOR QFILE-OUTPUT-CHARACTER-STREAM
        ()
        (QFILE-OUTPUT-CHARACTER-STREAM-MIXIN SI:OUTPUT-FILE-STREAM-MIXIN
         CHAOS:CHARACTER-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(DEFFLAVOR QFILE-OUTPUT-PHONY-CHARACTER-STREAM
        ()
        (QFILE-OUTPUT-PHONY-CHARACTER-STREAM-MIXIN SI:OUTPUT-FILE-STREAM-MIXIN
         CHAOS:BINARY-OUTPUT-STREAM-MIXIN SI:BUFFERED-TYO-OUTPUT-STREAM))

(DEFFLAVOR QFILE-INPUT-BINARY-STREAM
        ()
        (QFILE-INPUT-BINARY-STREAM-MIXIN SI:INPUT-FILE-STREAM-MIXIN
         CHAOS:BINARY-INPUT-STREAM-MIXIN SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR QFILE-INPUT-SIGNED-BINARY-STREAM
        ()
        (QFILE-INPUT-SIGNED-BINARY-STREAM-MIXIN SI:INPUT-FILE-STREAM-MIXIN
         CHAOS:BINARY-INPUT-STREAM-MIXIN SI:BASIC-BUFFERED-INPUT-STREAM))

(DEFFLAVOR QFILE-OUTPUT-BINARY-STREAM
        ()
        (QFILE-OUTPUT-BINARY-STREAM-MIXIN SI:OUTPUT-FILE-STREAM-MIXIN
         CHAOS:BINARY-OUTPUT-STREAM-MIXIN SI:BUFFERED-OUTPUT-STREAM))

(DEFFLAVOR QFILE-PROBE-STREAM
        ((STATUS :CLOSED))
        (QFILE-STREAM-MIXIN SI:STREAM)
  (:GETTABLE-INSTANCE-VARIABLES STATUS)
  (:INIT-KEYWORDS :DATA-CONNECTION))            ;Will be NIL, but makes life easier

(DEFMETHOD (QFILE-PROBE-STREAM :DIRECTION) () NIL)

(DEFFLAVOR QFILE-DIRECTORY-STREAM () (QFILE-INPUT-CHARACTER-STREAM))

(COMPILE-FLAVOR-METHODS QFILE-INPUT-CHARACTER-STREAM QFILE-INPUT-BINARY-STREAM
                        QFILE-INPUT-SIGNED-BINARY-STREAM QFILE-INPUT-PHONY-CHARACTER-STREAM
                        QFILE-OUTPUT-CHARACTER-STREAM QFILE-OUTPUT-BINARY-STREAM
                        QFILE-OUTPUT-PHONY-CHARACTER-STREAM
                        QFILE-PROBE-STREAM QFILE-DIRECTORY-STREAM)

;;;; Access interface

(DEFMETHOD (BASIC-QFILE-ACCESS :DIRECTORY-STREAM-DEFAULT-PARSER) () #'SUBSTRING)

(DEFMETHOD (BASIC-QFILE-ACCESS :READ-DIRECTORY-STREAM-ENTRY) (STREAM DEFAULT-PATHNAME &optional options)
  (FS:READ-DIRECTORY-STREAM-ENTRY STREAM DEFAULT-PATHNAME options))

;;; PATHNAME is supplied as an argument here so that the :PATHNAME message to the stream
;;; will return a logical pathname, if that is what was OPEN'ed.
(DEFMETHOD (BASIC-QFILE-ACCESS :OPEN) (FILE PATHNAME &REST OPTIONS)
  (APPLY 'OPEN-CHAOS SELF FILE PATHNAME OPTIONS))

(DEFMETHOD (BASIC-QFILE-ACCESS :RENAME) (FILE NEW-PATHNAME ERROR-P)
  (RENAME-CHAOS SELF FILE NEW-PATHNAME ERROR-P))

(DEFMETHOD (BASIC-QFILE-ACCESS :DELETE) (FILE ERROR-P)
  (DELETE-CHAOS SELF FILE ERROR-P))

(DEFMETHOD (BASIC-QFILE-ACCESS :COMPLETE-STRING) (FILE STRING OPTIONS &AUX SUCCESS)
  (MULTIPLE-VALUE (STRING SUCCESS)
    (COMPLETE-CHAOS SELF FILE STRING OPTIONS))
  (LET ((DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
    (VALUES (STRING-APPEND (SEND (SEND SELF :HOST) :NAME-AS-FILE-COMPUTER) ": " STRING)
            SUCCESS)))

(DEFMETHOD (BASIC-QFILE-ACCESS :CHANGE-PROPERTIES) (FILE ERROR-P &REST PROPERTIES)
  (CHANGE-PROPERTIES-CHAOS SELF FILE ERROR-P PROPERTIES))

(DEFMETHOD (BASIC-QFILE-ACCESS :DIRECTORY-STREAM) (FILE OPTIONS)
  (DIRECTORY-CHAOS SELF FILE OPTIONS))

(DEFMETHOD (BASIC-QFILE-ACCESS :HOMEDIR) (USER)
  USER
  (HOMEDIR-CHAOS SELF))

(DEFMETHOD (BASIC-QFILE-ACCESS :CREATE-LINK) (FILE LINK-TO ERROR)
  (CREATE-LINK-CHAOS SELF FILE LINK-TO ERROR))

(DEFMETHOD (BASIC-QFILE-ACCESS :EXPUNGE) (FILE ERROR)
  (DIRECTORY-OPERATION-CHAOS :EXPUNGE SELF FILE ERROR "Expunge Directory"))

(DEFMETHOD (BASIC-QFILE-ACCESS :REMOTE-CONNECT) (&REST ARGS)
  (DECLARE (ARGLIST FILE ERROR ACCESS-MODE &OPTIONAL UNIT))
  (APPLY 'CWD-CHAOS SELF ARGS))

(DEFMETHOD (BASIC-QFILE-ACCESS :CREATE-DIRECTORY) (FILE ERROR)
  (DIRECTORY-OPERATION-CHAOS :CREATE-DIRECTORY SELF FILE ERROR "Create Directory"))

(DEFMETHOD (BASIC-QFILE-ACCESS :MULTIPLE-FILE-PLISTS) (FILES OPTIONS)
  (HANDLING-FILE-ERRORS (T)
    (MULTIPLE-PLISTS-CHAOS SELF FILES OPTIONS)))

;;;; Fascist caste system support. Boo Hiss etc
(DEFMETHOD (BASIC-QFILE-ACCESS :ENABLE-CAPABILITIES) (CAPABILITIES &OPTIONAL UNIT)
  (SEND SELF :CHANGE-CAPABILITIES T CAPABILITIES UNIT))

(DEFMETHOD (BASIC-QFILE-ACCESS :DISABLE-CAPABILITIES) (CAPABILITIES &OPTIONAL UNIT)
  (SEND SELF :CHANGE-CAPABILITIES NIL CAPABILITIES UNIT))

;;; Close any data connections that have not been used for a while,
;;; exempting only the first data connection on any host unit.
(DEFMETHOD (QFILE-HOST-UNIT :CLOSE-DORMANT-DATA-CONNECTIONS) ()
  (LOCK-HOST-UNIT (SELF)
    (DOLIST (DATA-CONNECTION (CDR DATA-CONNECTIONS))
      (WHEN (DATA-CONNECTION-DORMANT DATA-CONNECTION)
        (let ((pkt (SEND SELF :COMMAND NIL (DATA-INPUT-HANDLE DATA-CONNECTION) NIL "Undata"
                         "UNDATA-CONNECTION")))
            (and pkt (chaos:return-pkt pkt)))
        (LET ((CONN (DATA-CONNECTION DATA-CONNECTION)))
          (CHAOS:CLOSE-CONN CONN "Done")
          (CHAOS:REMOVE-CONN CONN))
        (SETQ DATA-CONNECTIONS (DELQ DATA-CONNECTION DATA-CONNECTIONS))))))

(COMPILE-FLAVOR-METHODS QFILE-HOST-UNIT QFILE-ACCESS lispm-qfile-access)

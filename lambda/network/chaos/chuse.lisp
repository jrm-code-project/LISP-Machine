;;; -*- Mode:LISP; Package:CHAOS; Base:10; Readtable:CL -*-
;;;     ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;;
;;; This is SYS: NETWORK; CHAOS; CHUSE
;;; Very high-level CHAOSnet functions.
;;; The NCP and low level functions in SYS: NETWORK; CHAOS; CHSNCP

;;; This returns a connection to frob, and the host name
(defun establish-connection (real-address contact-name timeout window-size &aux conn host-name)
  (assure-enabled)
  (setq conn (open-connection real-address contact-name window-size))
  (setq host-name
        (or (si:get-host-from-address real-address :chaos)
            real-address))
  (let ((wait-completed nil))
    (unwind-protect
        (progn
          (wait conn 'rfc-sent-state timeout
                (format nil "Net Connect: ~A"
                        (if (typep host-name 'instance)
                            (send host-name :short-name)
                          host-name)))
          (setq wait-completed t))
      (unless wait-completed
        (remove-conn conn)))
    (values (and wait-completed conn) host-name)))

;;; This does a full "ICP": it sends an RFC, waits for a reply or timeout,
;;; and returns a string to get an error, or else the CONN to indicate that
;;; the foreign host sent an OPN and we are connected.
;;; The first argument gets parsed as an address.
(DEFUN CONNECT (ADDRESS CONTACT-NAME &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
                                               (TIMEOUT (* 10. 60.))
                              &AUX CONN REAL-ADDRESS HOST-NAME)
  "Establish a chaosnet connection and return the connection object.
ADDRESS is a host name or number.  CONTACT-NAME is a string containing
the contact name and optional additional data for the other host.
WINDOW-SIZE is the number of packets that can be in transit from
the other side, on this connection.
TIMEOUT is how long to wait before giving up (in 60'ths of a second).

If the connection fails, an error is signaled."
  (DO-FOREVER
    (CATCH-ERROR-RESTART-EXPLICIT-IF T (SYS:REMOTE-NETWORK-ERROR
                                         :RETRY-CONNECTION "Try the connection again.")
      (COND ((NULL (SETQ REAL-ADDRESS (ADDRESS-PARSE ADDRESS)))
             (FERROR 'SYS:UNKNOWN-ADDRESS "~S is not a valid Chaosnet address." ADDRESS))
            (T
             (multiple-value (conn host-name)
               (establish-connection real-address contact-name timeout window-size))
             (CASE (STATE CONN)
               (OPEN-STATE (RETURN CONN))
               (RFC-SENT-STATE (CLOSE-CONN CONN)
                               (funcall *chaos-stream* :flush-address (conn-foreign-address conn))
                               (FERROR 'SYS:HOST-NOT-RESPONDING-DURING-CONNECTION
                                       "Host ~1@*~A not responding."
                                       CONN HOST-NAME))
               (ANSWERED-STATE (CLOSE-CONN CONN)
                               (FERROR 'SYS:CONNECTION-ERROR-1
                                       "Received an ANS instead of an OPN."
                                       CONN))
               (CLS-RECEIVED-STATE
                (LET* ((PKT (GET-NEXT-PKT CONN))
                       (STRING (STRING-APPEND (PKT-STRING PKT))))
                  (RETURN-PKT PKT)
                  (CLOSE-CONN CONN)
                  (IF (EQUAL STRING "")
                      (FERROR 'SYS:CONNECTION-REFUSED
                              "Connection to ~1@*~A rejected without explanation."
                              CONN HOST-NAME)
                    (FERROR 'SYS:CONNECTION-REFUSED
                            "Connection to ~1@*~A refused: ~A."
                            CONN HOST-NAME STRING))))
               (OTHERWISE (UNWIND-PROTECT
                              (FERROR 'SYS:CONNECTION-ERROR-1
                                      "Bad state in ~S: ~A~@[, ~A~]"
                                      CONN
                                      (STATE CONN)
                                      (AND (READ-PKTS CONN) (PKT-STRING (READ-PKTS CONN))))
                            (REMOVE-CONN CONN)))))))
    ;; The second time, wait a long time.
    (SETQ TIMEOUT (* 2 TIMEOUT))))

;;; Takes anything anyone might use as a ChaosNet address, and tries to return
;;; the corresponding host number.  If it fails, returns NIL.
(DEFUN ADDRESS-PARSE (ADDRESS &AUX HOST)
  "Coerce the argument to a chaosnet address.
The argument can be a host name or host object, or an address."
  (DECLARE (VALUES ADDRESS HOST-OBJECT))
  (CONDITION-CASE (ERROR)
      (LET ((ADDRESS (COND ((INTEGERP ADDRESS)
                            ADDRESS)
                           ((AND (TYPEP ADDRESS 'INSTANCE)
                                 (SEND (SETQ HOST ADDRESS) :SEND-IF-HANDLES :network-ADDRESS :chaos)))
                           ((AND (SETQ HOST (SI:PARSE-HOST ADDRESS T))
                                 (SEND HOST :network-address :CHAOS)))
                           ((AND (STRINGP ADDRESS)
                                 (PARSE-NUMBER ADDRESS 0 NIL 8))))))
        (IF ADDRESS (VALUES ADDRESS (OR HOST (SI:GET-HOST-FROM-ADDRESS ADDRESS :CHAOS)))))
    (SYS:UNCLAIMED-MESSAGE NIL)))

;;; This is used to perform a "simple connection".  An RFC is sent to the
;;; specified address, expecting an ANS.  Returns a string if there was an
;;; error, in which case the string is an ASCII explanation.  Otherwise
;;; returns the ANS.  When you are done perusing the ANS, RETURN-PKT the PKT.
(DEFUN SIMPLE (ADDRESS CONTACT-NAME &OPTIONAL (TIMEOUT (* 10. 60.))
               &AUX CONN REAL-ADDRESS HOST-NAME)
  "Send a message to CONTACT-NAME at ADDRESS, expecting one ANS packet in return.
No connection is established; if the other host tries to create a connection,
it is considered an error.
If successful, the ANS packet object is returned.
Otherwise, a string describing the reasons for failure is returned.
TIMEOUT is how long to wait before giving up, in 60'ths of a second."
  (DO-FOREVER
    (CATCH-ERROR-RESTART-EXPLICIT-IF T (SYS:REMOTE-NETWORK-ERROR :RETRY-CONNECTION
                                         "Try the transaction again.")
      (COND ((NULL (SETQ REAL-ADDRESS (ADDRESS-PARSE ADDRESS)))
             (FERROR 'SYS:UNKNOWN-ADDRESS "~S is not a valid Chaosnet address." ADDRESS))
            (T (multiple-value (conn host-name)
                 (establish-connection real-address contact-name timeout 5))
               (CASE (STATE CONN)
                 (RFC-SENT-STATE
                  (REMOVE-CONN CONN)
                  (funcall *chaos-stream* :flush-address (conn-foreign-address conn))
                  (FERROR 'SYS:HOST-NOT-RESPONDING-DURING-CONNECTION
                          "Host ~1@*~A not responding."
                          CONN HOST-NAME))
                 (CLS-RECEIVED-STATE
                  (LET* ((PKT (GET-NEXT-PKT CONN))
                         (STRING (STRING-APPEND (PKT-STRING PKT))))
                    (RETURN-PKT PKT)
                    (REMOVE-CONN CONN)
                    (IF (EQUAL STRING "")
                        (FERROR 'SYS:CONNECTION-REFUSED
                                "Simple transaction to ~1@*~S rejected without explanation."
                                CONN HOST-NAME)
                        (FERROR 'SYS:CONNECTION-REFUSED
                                "Simple transaction to ~1@*~S refused: ~A."
                                CONN HOST-NAME STRING))))
                 (OPEN-STATE
                  (CLOSE-CONN CONN "I expected an ANS, not an OPN.")
                  (FERROR 'SYS:CONNECTION-ERROR-1
                          "Received an OPN instead of an ANS."
                          CONN))
                 (ANSWERED-STATE
                  (RETURN (PROG1 (GET-NEXT-PKT CONN)
                                 (CLOSE-CONN CONN))))
                 (OTHERWISE (UNWIND-PROTECT
                              (FERROR 'SYS:CONNECTION-ERROR-1
                                      "Bad state in ~S: ~A~@[, ~A~]"
                                      CONN
                                      (STATE CONN)
                                      (AND (READ-PKTS CONN) (PKT-STRING (READ-PKTS CONN))))
                              (REMOVE-CONN CONN)))))))
    (SETQ TIMEOUT (* 2 TIMEOUT))))

(DEFMACRO VALID-ADDRESS? (ADDRESS)
  `(TYPEP ,ADDRESS '(INTEGER 0 #o177777)))

;;;; USER FUNCTIONS: Functions for the user side of a connection.

;;; This is called as the first step in opening a connection.  Note the
;;; CONNECT function, which is a higher-level frob (like NETWRK's ICP routine)
;;; which you may want to use instead.
;;;   The first arg is the address of the foreign host.  Next is the contact name.
;;; Optionally following are the one-way flag and window size.
(DEFUN OPEN-CONNECTION (ADDRESS CONTACT-NAME &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
                      &AUX PKT CONN)
    (CHECK-TYPE ADDRESS (SATISFIES VALID-ADDRESS?) "an address")
    (CHECK-ARG CONTACT-NAME
               (AND (STRINGP CONTACT-NAME) ( (LENGTH CONTACT-NAME) MAX-DATA-BYTES-PER-PKT))
               "a string")
    (CHECK-TYPE WINDOW-SIZE NUMBER "a number")
    (SETQ CONN (MAKE-CONNECTION))
    (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN WINDOW-SIZE MAXIMUM-WINDOW-SIZE)))
    (SETF (FOREIGN-ADDRESS CONN) ADDRESS)
    (SETF (GETF (CONN-PLIST CONN) 'RFC-CONTACT-NAME) CONTACT-NAME)

    (UNWIND-PROTECT
      (PROGN
        (SETQ PKT (ALLOCATE-PKT))
        (SETF (PKT-OPCODE PKT) RFC-OP)
        (SET-PKT-STRING PKT CONTACT-NAME)
        (SETF (PKT-LINK PKT) NIL)
        (WITHOUT-INTERRUPTS
          (SETF (WINDOW-AVAILABLE CONN) 1)
          (SETF (TIME-LAST-RECEIVED CONN) (zl:TIME))
          (SETF (STATE CONN) 'RFC-SENT-STATE))
        (TRANSMIT-NORMAL-PKT CONN PKT (PKT-NUM-SENT CONN))
        ;; Must not put on lists before calling TRANSMIT-NORMAL-PKT, which fills in
        ;; important information
        (WITHOUT-INTERRUPTS
          (SETF (SEND-PKTS CONN) PKT)
          (SETF (SEND-PKTS-LAST CONN) PKT)
          (SETF (SEND-PKTS-LENGTH CONN) 1)
          (SETQ RETRANSMISSION-NEEDED T)
          (SETQ PKT NIL)))
      (AND PKT (FREE-PKT PKT)))
    CONN)

#| This stuff can't work yet -- RpK
;;; Open up a connection for use with foreign protocols
(DEFUN OPEN-FOREIGN-CONNECTION (FOREIGN-HOST FOREIGN-INDEX
                                &OPTIONAL (PKT-ALLOCATION 10.) DISTINGUISHED-PORT
                                &AUX CONN)
  (CHECK-TYPE FOREIGN-HOST (SATISFIES VALID-ADDRESS?) "an address")
  (SETQ CONN (MAKE-CONNECTION))
  (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN PKT-ALLOCATION MAXIMUM-WINDOW-SIZE)))
  (SETF (FOREIGN-ADDRESS CONN) FOREIGN-HOST)
  (SETF (FOREIGN-INDEX-NUM CONN) FOREIGN-INDEX)
  (SETF (STATE CONN) 'FOREIGN-STATE)
  (WHEN DISTINGUISHED-PORT
    (SETF (AREF INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (LOCAL-INDEX-NUM CONN))) NIL)
    (SETF (LOCAL-INDEX-NUM CONN) DISTINGUISHED-PORT)
    (PUSH (CONS DISTINGUISHED-PORT CONN) DISTINGUISHED-PORT-CONN-TABLE))
  CONN)

(DEFVAR *ALL-SUBNET-BIT-MAP* (MAKE-ARRAY 32. :element-type '(mod #o400) :INITIAL-ELEMENT #o377))

(DEFUN SUBNET-BIT-MAP (SUBNETS)
  "SUBNETS may be a list of subnet numbers, or the symbol :ALL"
  (DECLARE (VALUES BIT-MAP BIT-MAP-LENGTH))
  (COND ((EQ SUBNETS :ALL) (VALUES *ALL-SUBNET-BIT-MAP* 32.))
        ((NULL SUBNETS) (VALUES (MAKE-ARRAY 4. :ELEMENT-TYPE 'string-char :INITIAL-ELEMENT 0) 4))
        (T
         (LET* ((BIT-MAP-LENGTH (* (CEILING (CEILING (1+ (APPLY #'MAX SUBNETS)) 8.) 4) 4))
                (BIT-MAP (MAKE-ARRAY BIT-MAP-LENGTH :element-TYPE '(mod #o400) :INITIAL-ELEMENT 0)))
           (DOLIST (SUBNET SUBNETS)
             (MULTIPLE-VALUE-BIND (BYTE BIT) (TRUNCATE SUBNET 8)
               (SETF (AREF BIT-MAP BYTE) (LOGIOR (AREF BIT-MAP BYTE) (LSH 1 BIT)))))
           (VALUES BIT-MAP BIT-MAP-LENGTH)))))

(DEFUN OPEN-BROADCAST-CONNECTION (SUBNETS CONTACT-NAME &OPTIONAL (PKT-ALLOCATION 10.)
                                  &AUX SUBNET-BIT-MAP SUBNET-BIT-MAP-LENGTH)
  "Broadcast a service request from CONTACT-NAME over certain subnets.
PKT-ALLOCATION is the buffering size for unread requests as they come over the net.
The connection returned is in the CHAOS:BROADCAST-SENT-STATE."
  (MULTIPLE-VALUE (SUBNET-BIT-MAP SUBNET-BIT-MAP-LENGTH) (SUBNET-BIT-MAP SUBNETS))
  (LET ((CONN (MAKE-CONNECTION)))
    (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN PKT-ALLOCATION MAXIMUM-WINDOW-SIZE)))
    (SETF (FOREIGN-ADDRESS CONN) 0) ; seems ok
    ; (SETF (FOREIGN-INDEX-NUM CONN) FOREIGN-INDEX) ; not sure about this
    (LET ((PKT NIL))
      (UNWIND-PROTECT
          (PROGN
            (SETQ PKT (ALLOCATE-PKT))
            (SETF (PKT-ACK-NUM PKT) SUBNET-BIT-MAP-LENGTH)
            (SETF (PKT-OPCODE PKT) BRD-OP)
            (SETF (PKT-LINK PKT) NIL)
            (SETF (PKT-DEST-ADDRESS PKT) 0)
            (SETF (PKT-DEST-INDEX-NUM PKT) 0)
            (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
            (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
            (SETF (GETF (CONN-PLIST CONN) 'BROADCAST-CONNECTION) T)
            (SETF (GETF (CONN-PLIST CONN) 'SUBNET-BIT-MAP) SUBNET-BIT-MAP)
            (SETF (GETF (CONN-PLIST CONN) 'SUBNET-BIT-MAP-LENGTH) SUBNET-BIT-MAP-LENGTH)
            (SETF (GETF (CONN-PLIST CONN) 'CONTACT-NAME) CONTACT-NAME)
            (SET-PKT-STRING PKT SUBNET-BIT-MAP CONTACT-NAME)
            (WITHOUT-INTERRUPTS
              (SETF (WINDOW-AVAILABLE CONN) 1)
              (SETF (TIME-LAST-RECEIVED CONN) (zl:TIME))
              (SETF (STATE CONN) 'BROADCAST-SENT-STATE))
            (TRANSMIT-PKT PKT ()))
        (AND PKT (FREE-PKT PKT)))
    CONN)))

(DEFMACRO ASSURE-BROADCAST-CONNECTION (CONN)
  `(IF (NOT (GETF (CONN-PLIST ,CONN) 'BROADCAST-CONNECTION))
       (FERROR 'SYS:LOCAL-NETWORK-ERROR
               :FORMAT-STRING "~A was not opened in broadcast mode"
               :FORMAT-ARGS (LIST ,CONN))))

(DEFUN RETRANSMIT-BRD-PACKET (CONN)
  "Send out another request for service, if CONN was opened in broadcast mode."
  (ASSURE-BROADCAST-CONNECTION CONN)
  (LET ((PKT (ALLOCATE-PKT))
        (SUBNET-BIT-MAP-LENGTH (GET (LOCF (CONN-PLIST CONN)) 'SUBNET-BIT-MAP-LENGTH)))
    (UNWIND-PROTECT
        (PROGN
          (SETF (PKT-ACK-NUM PKT) SUBNET-BIT-MAP-LENGTH)
          (SETF (PKT-OPCODE PKT) BRD-OP)
          (SETF (PKT-DEST-ADDRESS PKT) 0)
          (SETF (PKT-DEST-INDEX-NUM PKT) 0)
          (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
          (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
          (SET-PKT-STRING PKT (GET (LOCF (CONN-PLIST CONN)) 'SUBNET-BIT-MAP)
                          (GET (LOCF (CONN-PLIST CONN)) 'CONTACT-NAME))
          (TRANSMIT-PKT PKT ()))
      (FREE-PKT PKT))))

(DEFUN READ-BROADCAST-PKT (CONN &KEY NO-HANG-P (RESET-STATE-P :ANS) (WHOSTATE "BRD In"))
  "Returns a PKT or NIL, like GET-NEXT-PKT.
This function will do nasty things if not called on a broadcast CONN.
RESET-STATE-P can be
 :ANS only if an ANS was received
 :ALWAYS if any type of packet was received
 NIL never"
  (ASSURE-BROADCAST-CONNECTION CONN)
  (LET ((PKT (GET-NEXT-PKT CONN NO-HANG-P WHOSTATE)))
    (UNLESS (NULL PKT)
      (IF (OR (EQ RESET-STATE-P :ALWAYS)
              (AND (EQ (STATE CONN) 'ANSWERED-STATE) (EQ RESET-STATE-P :ANS)))
          (SETF (STATE CONN) 'BROADCAST-SEND-STATE))
      PKT)))
|#

;;;; SERVER FUNCTIONS: Functions used by the server side of a connection only.

(DEFUN LISTEN (CONTACT-NAME &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE) (WAIT-FOR-RFC T)
                     &AUX CONN)
    "Listen for an incoming RFC to CONTACT-NAME.
Returns the connection-object, ready to have CHAOS:ACCEPT,
CHAOS:REJECT, CHAOS:ANSWER, or CHAOS:FORWARD done to it.
A server function on SERVER-ALIST can call LISTEN to respond to
the request which caused the server to be run.
If WAIT-FOR-RFC is NIL, doesn't wait for the RFC to arrive, just sets up a queue.
WINDOW-SIZE specifies how many packets can be in transit at once from the
other side of the connection to this one, once the connection is established."
    (CHECK-TYPE CONTACT-NAME STRING)
    (CHECK-TYPE WINDOW-SIZE NUMBER)
    ;; Make a connection.  If table full, wait a little while and try again.
    (DO-FOREVER
      (CONDITION-CASE ()
          (SETQ CONN (MAKE-CONNECTION))
        (SYS:NETWORK-RESOURCES-EXHAUSTED
          (PROCESS-SLEEP 30.))
        (:NO-ERROR (RETURN))))
    (SETF (GETF (CONN-PLIST CONN) 'LISTEN-CONTACT-NAME) CONTACT-NAME)
    (SETF (LOCAL-WINDOW-SIZE CONN) (MAX 1 (MIN WINDOW-SIZE MAXIMUM-WINDOW-SIZE)))
    (PROG LISTEN ()
      (WITHOUT-INTERRUPTS                       ;First try to pick up a pending RFC
        (DO ((PKT PENDING-RFC-PKTS (PKT-LINK PKT))
             (PREV NIL PKT))
            ((NULL PKT))
          (COND ((STRING-EQUAL (CONTACT-NAME-FROM-RFC PKT) CONTACT-NAME)
                 (COND ((NULL PREV) (SETQ PENDING-RFC-PKTS (PKT-LINK PKT)))
                       (T (SETF (PKT-LINK PREV) (PKT-LINK PKT))))
                 (RFC-MEETS-LSN CONN PKT)
                 (RETURN-FROM LISTEN CONN))))
        (SETF (STATE CONN) 'LISTENING-STATE)    ;No RFC, let listen pend
        (PUSH (CONS CONTACT-NAME CONN) PENDING-LISTENS))
      (COND (WAIT-FOR-RFC
             (PROCESS-WAIT "Net Listen"
                           #'(LAMBDA (CONN) (NEQ (STATE CONN) 'LISTENING-STATE))
                           CONN)
             (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
                 (FERROR 'SYS:BAD-CONNECTION-STATE-1
                         "Listening connection ~S entered bad state ~S"
                         CONN (STATE CONN)))))
      (RETURN CONN)))

;;; If you have done a LISTEN and the state has changed to RFC-RECEIVED, you
;;; call one of the following four functions.

;;; Send an OPN, and leave conn in OPEN-STATE.
;;; Note that when this returns the other end has not yet acknowledged
;;; the OPN, and the window size is still 0.  Transmitting the first packet
;;; will wait.
(DEFUN ACCEPT (CONN &AUX PKT)
  "Accept a request for a connection, received on connection-object CONN.
CONN should have been returned by a previous call to LISTEN.
Note that the connection is not completely established
until the other side replies to the packet we send."
    (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
        (FERROR 'SYS:BAD-CONNECTION-STATE-1
                "Attempt to accept ~S, which was in ~A, not RFC-RECEIVED-STATE"
                CONN (STATE CONN)))
    (SETQ PKT (READ-PKTS CONN))
    (COND (PKT                                  ;In case the user has not read the RFC
           (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM PKT))
           (SETF (READ-PKTS CONN) (PKT-LINK PKT))
           (OR (READ-PKTS CONN)
               (SETF (READ-PKTS-LAST CONN) NIL))
           (FREE-PKT PKT)))
    (SETQ PKT (ALLOCATE-PKT))
    (SETF (PKT-OPCODE PKT) OPN-OP)
    (SETF (PKT-NBYTES-on-write PKT) 4)
    (SETF (PKT-SECOND-DATA-WORD PKT) (LOCAL-WINDOW-SIZE CONN))
    (SETF (PKT-FIRST-DATA-WORD PKT) (PKT-NUM-READ CONN))
    (WITHOUT-INTERRUPTS
      (SETF (PKT-LINK PKT) NIL)
      (SETF (WINDOW-AVAILABLE CONN) 0)
      (SETF (TIME-LAST-RECEIVED CONN) (zl:TIME))
      (SETF (STATE CONN) 'OPEN-STATE))  ;Set this -before- telling other end it's open!
    (TRANSMIT-NORMAL-PKT CONN PKT T)
    (WITHOUT-INTERRUPTS
      ;; TRANSMIT-NORMAL-PKT fills in fields that must be filled before packet
      ;; can be put on transmit list
      (SETF (SEND-PKTS CONN) PKT)
      (SETF (SEND-PKTS-LAST CONN) PKT)
      (SETF (SEND-PKTS-LENGTH CONN) 1)
      (SETQ RETRANSMISSION-NEEDED T))
    T)

;;; Send a CLS and leave conn INACTIVE.
(DEFUN REJECT (CONN REASON)
  "Reject a request for a connection, received on connection-object CONN.
CONN should have been returned by a previous call to LISTEN.
REASON is a string to be sent to the requestor and returned from
his call to CONNECT."
    (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
        (FERROR 'SYS:BAD-CONNECTION-STATE-1
                "Attempt to reject ~S, which was in ~A, not RFC-RECEIVED-STATE"
                CONN (STATE CONN)))
    (CLOSE-CONN CONN REASON)
    T)

;; Send an ANS, and leave conn INACTIVE.
;; The caller passes in a PKT with data and NBYTES set up.
(DEFUN ANSWER (CONN PKT)
  "Reply to a simple transaction received on connection-object CONN.
PKT should be a packet with ANS as its opcode and the data and nbytes fields set up.
This is the proper way to answer when the requestor has used the function CHAOS:SIMPLE.
Note that there is no guarantee that the requestor will receive the answer;
he will just repeat the request if he does not.
See also CHAOS:ANSWER-STRING."
  (WHEN (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
    (SETF (PKT-OPCODE PKT) ANS-OP)
    (TRANSMIT-NORMAL-PKT CONN PKT))
  (RETURN-PKT PKT)
  (REMOVE-CONN CONN)
  T)

(DEFUN ANSWER-STRING (CONN STRING)
  "Reply to a simple transaction received on connection-object CONN.
STRING specifies the answer to send.
This is the proper way to answer when the requestor has used the function CHAOS:SIMPLE.
Note that there is no guarantee that the requestor will receive the answer;
he will just repeat the request if he does not.
See also CHAOS:ANSWER, a lower level way of answering."
  (LET ((PKT (GET-PKT)))
    (SETF (PKT-NBYTES-on-write PKT) (MIN (STRING-LENGTH STRING) MAX-DATA-BYTES-PER-PKT))
    (COPY-ARRAY-CONTENTS STRING (PKT-STRING PKT))
    (ANSWER CONN PKT)))

;;; Minimal-consing simple-transaction answerer.
;;; Returns T if succeeds, NIL if fails, although you probably don't care, since
;;; a value of T does not assure that the ANS really reached the requestor.
(DEFUN FAST-ANSWER-STRING (CONTACT-NAME STRING)
  "Reply to a simple transaction requested on CONTACT-NAME, with answer STRING.
This is like (ANSWER-STRING (LISTEN contact-name) string) but conses less."
  (PROG ((PREV NIL) RFC PKT PSTR)
    (WITHOUT-INTERRUPTS
      (SETQ RFC (DO PKT PENDING-RFC-PKTS (PKT-LINK PKT) (NULL PKT)
                    (AND (STRING-EQUAL (CONTACT-NAME-FROM-RFC PKT) CONTACT-NAME)
                         (RETURN PKT))
                    (SETQ PREV PKT)))
      (IF (NULL RFC) (RETURN NIL)
          (IF (NULL PREV) (SETQ PENDING-RFC-PKTS (PKT-LINK RFC))
              (SETF (PKT-LINK PREV) (PKT-LINK RFC)))))
    (setq pkt (net:allocate-packet))
    (SETF (PKT-NBYTES-on-write PKT) (MIN (STRING-LENGTH STRING) MAX-DATA-BYTES-PER-PKT))
    (SETQ PSTR       ;Create indirect array to reference as a string
          (MAKE-STRING MAX-DATA-BYTES-PER-PKT :FILL-POINTER 0
                                              :DISPLACED-TO PKT
                                              :DISPLACED-INDEX-OFFSET 16.))
    (COPY-ARRAY-CONTENTS STRING PSTR)
    ;(RETURN-ARRAY (PROG1 PSTR (SETQ PSTR NIL)))
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) 0)
    (SETF (PKT-DEST-ADDRESS PKT) (PKT-SOURCE-ADDRESS RFC))
    (SETF (PKT-DEST-INDEX-NUM PKT) (PKT-SOURCE-INDEX-NUM RFC))
    (SETF (PKT-OPCODE PKT) ANS-OP)
    (SETF (PKT-NUM PKT) 0)
    (SETF (PKT-ACK-NUM PKT) 0)
    (TRANSMIT-INT-PKT PKT)
    (SETF (PKT-STATUS RFC) NIL)
    (FREE-PKT RFC)
    (RETURN T)))

(DEFUN FORWARD (CONN PKT HOST)
  "Forward a request for a connection to some other host and//or contact name.
CONN should be a connection object returned by LISTEN on which a
request has been received.  PKT should have opcode CHAOS:FWD-OP and its
data (and PKT-NBYTES) set to the new contact name to forward to.
HOST should specify the host to forward to."
  (OR (EQ (STATE CONN) 'RFC-RECEIVED-STATE)
      (FERROR 'SYS:BAD-CONNECTION-STATE-1
              "Attempt to forward ~S, which was in ~A, not RFC-RECEIVED-STATE"
              CONN (STATE CONN)))
  (SETF (PKT-OPCODE PKT) FWD-OP)
  (TRANSMIT-NORMAL-PKT CONN PKT 0 HOST)
  (RETURN-PKT PKT)
  (REMOVE-CONN CONN)
  T)

(DEFUN FORWARD-ALL (CONTACT-NAME HOST)
  "Tell all requests for chaosnet connections to CONTACT-NAME to try host HOST instead."
  (SETQ HOST (ADDRESS-PARSE HOST))
  (PUSH (LIST CONTACT-NAME
              `(PROG (CONN)
                     (SETQ CONN (LISTEN ,CONTACT-NAME))
                     (FORWARD CONN (GET-NEXT-PKT CONN) ,HOST)))
        SERVER-ALIST)
  NIL)


;;;; Control operations used by both users and servers.

;;; If CONN has received a close, free it up.
;;; If CONN is inactive, do nothing.
;;; If CONN is open, send a CLS containing the reason, leaving CONN inactive.
(DEFUN CLOSE-CONN (CONN &OPTIONAL (REASON "") &AUX PKT)
  "Close a chaosnet connection, given connection-object CONN.
REASON is a string telling the other side why; but don't rely
on its being received."
    (CASE (STATE CONN)
      ((CLS-RECEIVED-STATE ANSWERED-STATE)
       (REMOVE-CONN CONN)
       NIL)
      (INACTIVE-STATE
       (SETQ CONN-LIST (DELQ CONN CONN-LIST))
       NIL)
      ((OPEN-STATE RFC-RECEIVED-STATE)
       (SETQ PKT (ALLOCATE-PKT))
       (SETF (PKT-OPCODE PKT) CLS-OP)
       (SET-PKT-STRING PKT REASON)
       (TRANSMIT-NORMAL-PKT CONN PKT)
       (FREE-PKT PKT)
       (REMOVE-CONN CONN)
       NIL)
      ((LOS-RECEIVED-STATE HOST-DOWN-STATE LISTENING-STATE RFC-SENT-STATE)
       (REMOVE-CONN CONN)
       NIL)
      (OTHERWISE
       (FERROR 'SYS:BAD-CONNECTION-STATE-1
               "Attempt to close ~S, which was in ~S, not an acceptable state"
               CONN (STATE CONN)))))

(DEFF CLOSE 'CLOSE-CONN)
(DEFF CHAOS-CLOSE 'CLOSE-CONN)
(MAKE-OBSOLETE CLOSE "use CHAOS:CLOSE-CONN")
(MAKE-OBSOLETE CHAOS-CLOSE "use CHAOS:CLOSE-CONN")

;;; Wait until either:
;;;  the state of CONN is not STATE  (return T), or
;;;  over TIMEOUT 60ths of a second happen (return NIL).
(DEFUN WAIT (CONN STATE TIMEOUT &OPTIONAL (WHOSTATE "Chaosnet Wait") &AUX START-TIME)
  "Wait for chaosnet connection CONN to be in a state other than STATE.
Alternatively, waiting ends after TIMEOUT time (measured in 60'ths).
Returns non-NIL iff the connection's state has changed.
WHOSTATE is a string to tell the user what you are waiting for."
   (SETQ START-TIME (zl:TIME))
   (DO () (NIL)
     (OR (EQ STATE (STATE CONN))
         (RETURN T))
     (OR (< (TIME-DIFFERENCE (zl:TIME) START-TIME) TIMEOUT)
         (RETURN NIL))
     (PROCESS-WAIT WHOSTATE
                   (FUNCTION (LAMBDA (CONN STATE START-TIME TIMEOUT)
                                     (OR (NEQ (STATE CONN) STATE)
                                         ( (TIME-DIFFERENCE (zl:TIME) START-TIME) TIMEOUT))))
                   CONN
                   STATE
                   START-TIME
                   TIMEOUT)))

;;; Send the specied format string, and eof and close
(DEFUN FORMAT-AND-EOF (CONN &REST FORMAT-ARGS)
  (CONDITION-CASE ()
      (PROGN
        (ACCEPT CONN)
        (WITH-OPEN-STREAM (STREAM (MAKE-STREAM CONN))
          (APPLY #'FORMAT STREAM FORMAT-ARGS)))
    (SYS:REMOTE-NETWORK-ERROR NIL)))


;;;; Streams
;;; This is included in all chaosnet streams, input or output
(DEFFLAVOR BASIC-STREAM
        ((CONNECTION NIL))
        ()
  (:INCLUDED-FLAVORS SI:STREAM)
  (:INITABLE-INSTANCE-VARIABLES CONNECTION)
  (:GETTABLE-INSTANCE-VARIABLES CONNECTION))


;;; To find out what chaos host a stream is open to.
(DEFMETHOD (BASIC-STREAM :FOREIGN-HOST) ()
  (SI:GET-HOST-FROM-ADDRESS (FOREIGN-ADDRESS CONNECTION) :CHAOS))

(DEFMETHOD (BASIC-STREAM :CONTACT-NAME) () (CONTACT-NAME CONNECTION))

(DEFMETHOD (BASIC-STREAM :CLOSE) (&OPTIONAL ABORT-P)
  (WHEN CONNECTION                              ;Allowed to keep doing this
    (CLOSE-CONN CONNECTION (IF ABORT-P "Aborted" ""))
    (REMOVE-CONN (PROG1 CONNECTION (SETQ CONNECTION NIL)))))

(DEFMETHOD (BASIC-STREAM :ACCEPT) ()
  (ACCEPT CONNECTION))

(DEFMETHOD (BASIC-STREAM :REJECT) (&OPTIONAL REASON)
  (REJECT CONNECTION (OR REASON "")))

;;; These are new operations for the coming network system
(DEFMETHOD (BASIC-STREAM :ADD-AS-SERVER) (NAME &OPTIONAL (PROCESS CURRENT-PROCESS))
  (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONNECTION NAME PROCESS))

(DEFMETHOD (BASIC-STREAM :DELETE-AS-SERVER) ()
  (SEND TV:WHO-LINE-FILE-STATE-SHEET :DELETE-SERVER CONNECTION))

(DEFMETHOD (BASIC-STREAM :NETWORK) () :CHAOS)

(DEFVAR *SECURITY-FUNCTION* NIL "If T, a predicate called with the chaos address")
(DEFUN SECURE-P-INTERNAL (CONNECTION)
  (IF *SECURITY-FUNCTION* (FUNCALL *SECURITY-FUNCTION* (FOREIGN-ADDRESS CONNECTION)) T))

(DEFMETHOD (BASIC-STREAM :SECURE-P) () (SECURE-P-INTERNAL CONNECTION))

;;; This is included in all chaosnet input streams, character and binary
(DEFFLAVOR INPUT-STREAM-MIXIN
        ((INPUT-PACKET nil))
        ()
  (:INCLUDED-FLAVORS SI:BASIC-BUFFERED-INPUT-STREAM))

(DEFMETHOD (INPUT-STREAM-MIXIN :DISCARD-INPUT-BUFFER) (IGNORE)
  (when input-packet
    (RETURN-PKT INPUT-PACKET)
    (setq input-packet nil)))

(defmethod (input-stream-mixin :before :close) (&rest ignore)
  (when input-packet
    (return-pkt input-packet)
    (setq input-packet nil)))

;;; This is included in all chaosnet output streams, character and binary
(DEFFLAVOR OUTPUT-STREAM-MIXIN
        (OUTPUT-PACKET)
        ()
  (:INCLUDED-FLAVORS SI:BASIC-BUFFERED-OUTPUT-STREAM))

(DEFMETHOD (OUTPUT-STREAM-MIXIN :DISCARD-OUTPUT-BUFFER) (IGNORE)
  (RETURN-PKT OUTPUT-PACKET)
  (SETQ OUTPUT-PACKET NIL))

;;; This is included in simple chaosnet input streams, but not file streams, where certain
;;; opcodes have special meaning.
(DEFFLAVOR BASIC-INPUT-STREAM
        ((INPUT-PACKET NIL))
        (INPUT-STREAM-MIXIN BASIC-STREAM))

(DEFMETHOD (BASIC-INPUT-STREAM :GET-NEXT-INPUT-PKT) (NO-HANG-P &AUX OP)
  (COND ((AND INPUT-PACKET
              (OR (= (SETQ OP (PKT-OPCODE INPUT-PACKET)) EOF-OP)
                  (= OP CLS-OP)))
         NIL)
        ((NULL (SETQ INPUT-PACKET (GET-NEXT-PKT CONNECTION NO-HANG-P "Chaosnet Input" T)))
         NIL)
        ((OR (= (SETQ OP (PKT-OPCODE INPUT-PACKET)) EOF-OP)
             (= OP CLS-OP))
         NIL)
        (( OP DAT-OP)
         T)
        (T
         (FERROR NIL "Unknown opcode ~O in packet ~S received from connection ~S"
                 OP INPUT-PACKET CONNECTION))))

(DEFMETHOD (BASIC-INPUT-STREAM :CLEAR-EOF) ()
  (COND ((AND INPUT-PACKET (= (PKT-OPCODE INPUT-PACKET) EOF-OP))
         (RETURN-PKT INPUT-PACKET)
         (SETQ INPUT-PACKET NIL))))

;;; This is included in simple chaosnet output streams, but not file streams, where a
;;; connection is maintained for longer.
(DEFFLAVOR BASIC-OUTPUT-STREAM
        ()
        (OUTPUT-STREAM-MIXIN BASIC-STREAM)
  (:INCLUDED-FLAVORS SI:BASIC-BUFFERED-OUTPUT-STREAM))

(DEFMETHOD (BASIC-OUTPUT-STREAM :EOF) ()
  (SEND SELF :FORCE-OUTPUT)
  (SEND-PKT CONNECTION (GET-PKT) EOF-OP)
  (FINISH-CONN CONNECTION))

(DEFMETHOD (BASIC-OUTPUT-STREAM :FINISH) ()
  (FINISH-CONN CONNECTION))

(DEFMETHOD (BASIC-OUTPUT-STREAM :BEFORE :CLOSE) (&OPTIONAL ABORT-P)
  (AND CONNECTION (NOT ABORT-P)
       (EQ (STATE CONNECTION) 'OPEN-STATE)
       (SEND SELF :EOF)))

(DEFFLAVOR CHARACTER-INPUT-STREAM-MIXIN
        (INPUT-PACKET)
        (INPUT-STREAM-MIXIN)
  (:INCLUDED-FLAVORS BASIC-STREAM SI:BASIC-BUFFERED-INPUT-STREAM)
  ;; :GET-NEXT-INPUT-PKT returns T if INPUT-PACKET is a valid packet
  (:REQUIRED-METHODS :GET-NEXT-INPUT-PKT))

(DEFMETHOD (CHARACTER-INPUT-STREAM-MIXIN :ELEMENT-TYPE) () 'STRING-CHAR)

(DEFMETHOD (CHARACTER-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (&OPTIONAL NO-HANG-P)
  (AND (SEND SELF :GET-NEXT-INPUT-PKT NO-HANG-P)
       (VALUES (PKT-STRING INPUT-PACKET)
               0
               (PKT-NBYTES INPUT-PACKET))))

(DEFFLAVOR BINARY-INPUT-STREAM-MIXIN
        (INPUT-PACKET)
        (INPUT-STREAM-MIXIN)
  (:INCLUDED-FLAVORS BASIC-STREAM SI:BASIC-BUFFERED-INPUT-STREAM)
  (:REQUIRED-METHODS :GET-NEXT-INPUT-PKT))

(DEFMETHOD (BINARY-INPUT-STREAM-MIXIN :ELEMENT-TYPE) () '(UNSIGNED-BYTE 8))

(DEFMETHOD (BINARY-INPUT-STREAM-MIXIN :NEXT-INPUT-BUFFER) (&OPTIONAL NO-HANG-P)
  (AND (SEND SELF :GET-NEXT-INPUT-PKT NO-HANG-P)
       (LET ((ET (SEND-IF-HANDLES SELF :ELEMENT-TYPE)))
         (COND ((AND (CONSP ET)
                     (MEMQ (CAR ET) '(UNSIGNED-BYTE SIGNED-BYTE))
                     (EQ 8 (CADR ET)))
                (VALUES (PKT-STRING INPUT-PACKET)
                        0
                        (PKT-NBYTES INPUT-PACKET)))
               ('ELSE
                (VALUES INPUT-PACKET
                        FIRST-DATA-WORD-IN-PKT
                        (+ FIRST-DATA-WORD-IN-PKT (TRUNCATE (PKT-NBYTES INPUT-PACKET) 2))))))))

(DEFFLAVOR CHARACTER-OUTPUT-STREAM-MIXIN
        (OUTPUT-PACKET)
        (OUTPUT-STREAM-MIXIN)
  (:INCLUDED-FLAVORS BASIC-STREAM SI:BASIC-BUFFERED-OUTPUT-STREAM))

(DEFMETHOD (CHARACTER-OUTPUT-STREAM-MIXIN :ELEMENT-TYPE) () 'STRING-CHAR)

(DEFMETHOD (CHARACTER-OUTPUT-STREAM-MIXIN :NEW-OUTPUT-BUFFER) ()
  (SETQ OUTPUT-PACKET (GET-PKT))
  (VALUES (PKT-STRING OUTPUT-PACKET) 0 MAX-DATA-BYTES-PER-PKT))

(DEFMETHOD (CHARACTER-OUTPUT-STREAM-MIXIN :SEND-OUTPUT-BUFFER) SEND-CHARACTER-PKT)

(DECLARE-FLAVOR-INSTANCE-VARIABLES (CHARACTER-OUTPUT-STREAM-MIXIN)
(DEFUN SEND-CHARACTER-PKT (IGNORE IGNORE LENGTH)
  (SETF (PKT-NBYTES-on-write OUTPUT-PACKET) LENGTH)
  (SEND-PKT CONNECTION OUTPUT-PACKET)
  (SETQ OUTPUT-PACKET NIL)))

(DEFFLAVOR BINARY-OUTPUT-STREAM-MIXIN
        (OUTPUT-PACKET)
        (OUTPUT-STREAM-MIXIN)
  (:INCLUDED-FLAVORS BASIC-STREAM SI:BASIC-BUFFERED-OUTPUT-STREAM))

;due to unfortunate history, binary implies a default byte size of 16.
(DEFMETHOD (BINARY-OUTPUT-STREAM-MIXIN :ELEMENT-TYPE) () '(UNSIGNED-BYTE 16))

(DEFMETHOD (BINARY-OUTPUT-STREAM-MIXIN :NEW-OUTPUT-BUFFER) ()
  (SETQ OUTPUT-PACKET (GET-PKT))
  (LET ((ET (SEND-IF-HANDLES SELF :ELEMENT-TYPE)))
    (COND ((AND (CONSP ET)
                (MEMQ (CAR ET) '(UNSIGNED-BYTE SIGNED-BYTE))
                (EQ 8 (CADR ET)))
           (VALUES (PKT-STRING OUTPUT-PACKET)
                   0
                   MAX-DATA-BYTES-PER-PKT))
          ('ELSE
           (VALUES OUTPUT-PACKET
                   FIRST-DATA-WORD-IN-PKT
                   (+ FIRST-DATA-WORD-IN-PKT (TRUNCATE MAX-DATA-BYTES-PER-PKT 2)))))))

(DEFMETHOD (BINARY-OUTPUT-STREAM-MIXIN :SEND-OUTPUT-BUFFER) (ARRAY END)
  (COND ((EQ ARRAY OUTPUT-PACKET)
         (SEND-BINARY-PKT NIL NIL END))
        ('ELSE
         (SETF (PKT-NBYTES-ON-WRITE OUTPUT-PACKET) END)
         (SEND-PKT CONNECTION OUTPUT-PACKET #o300)
         (SETQ OUTPUT-PACKET NIL))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (BINARY-OUTPUT-STREAM-MIXIN)
(DEFUN SEND-BINARY-PKT (IGNORE IGNORE LENGTH)
  (SETF (PKT-NBYTES-on-write OUTPUT-PACKET) (* (- LENGTH FIRST-DATA-WORD-IN-PKT) 2))
  (SEND-PKT CONNECTION OUTPUT-PACKET #o300)
  (SETQ OUTPUT-PACKET NIL)))


(DECLARE-FLAVOR-INSTANCE-VARIABLES (BINARY-OUTPUT-STREAM-MIXIN)
(DEFUN SEND-BINARY-PKT-8 (IGNORE IGNORE LENGTH)
  (SETF (PKT-NBYTES-ON-WRITE OUTPUT-PACKET) LENGTH)
  (SEND-PKT CONNECTION OUTPUT-PACKET #o300)
  (SETQ OUTPUT-PACKET NIL)))


(DECLARE-FLAVOR-INSTANCE-VARIABLES (BINARY-OUTPUT-STREAM-MIXIN)
(DEFUN SEND-BINARY-PKT-ANY (IGNORE ARRAY LENGTH)
  (COND ((EQ ARRAY CHAOS:OUTPUT-PACKET)
         (CHAOS:SEND-BINARY-PKT NIL NIL LENGTH))
        ('ELSE
         (CHAOS:SEND-BINARY-PKT-8 NIL NIL LENGTH)))))




;;; Now the instantiatable flavors
(DEFFLAVOR INPUT-CHARACTER-STREAM
        ()
        (CHARACTER-INPUT-STREAM-MIXIN BASIC-INPUT-STREAM SI:BUFFERED-INPUT-CHARACTER-STREAM))

(DEFFLAVOR OUTPUT-CHARACTER-STREAM
        ()
        (CHARACTER-OUTPUT-STREAM-MIXIN BASIC-OUTPUT-STREAM
         SI:BUFFERED-OUTPUT-CHARACTER-STREAM))

(DEFFLAVOR CHARACTER-STREAM
        ()
        (CHARACTER-INPUT-STREAM-MIXIN CHARACTER-OUTPUT-STREAM-MIXIN
         BASIC-INPUT-STREAM BASIC-OUTPUT-STREAM SI:BUFFERED-CHARACTER-STREAM))

;;; This is to make the EVAL server work
(DEFMETHOD (CHARACTER-STREAM :BEEP) (&OPTIONAL IGNORE)
  )

(COMPILE-FLAVOR-METHODS INPUT-CHARACTER-STREAM OUTPUT-CHARACTER-STREAM CHARACTER-STREAM )

(DEFFLAVOR INPUT-BINARY-STREAM
        ()
        (BINARY-INPUT-STREAM-MIXIN BASIC-INPUT-STREAM
         SI:BUFFERED-INPUT-STREAM))

(DEFFLAVOR OUTPUT-BINARY-STREAM
        ()
        (BINARY-OUTPUT-STREAM-MIXIN BASIC-OUTPUT-STREAM
         SI:BUFFERED-OUTPUT-STREAM))

(DEFFLAVOR BINARY-STREAM
        ()
        (BINARY-INPUT-STREAM-MIXIN BINARY-OUTPUT-STREAM-MIXIN
         BASIC-INPUT-STREAM BASIC-OUTPUT-STREAM SI:BUFFERED-STREAM))

(COMPILE-FLAVOR-METHODS INPUT-BINARY-STREAM OUTPUT-BINARY-STREAM BINARY-STREAM)

(DEFFLAVOR ASCII-TRANSLATING-INPUT-CHARACTER-STREAM
        ()
        (SI:ASCII-TRANSLATING-INPUT-STREAM-MIXIN
         CHARACTER-INPUT-STREAM-MIXIN BASIC-INPUT-STREAM
         SI:BUFFERED-TYI-INPUT-STREAM))

(DEFFLAVOR ASCII-TRANSLATING-OUTPUT-CHARACTER-STREAM
        ()
        (SI:ASCII-TRANSLATING-OUTPUT-STREAM-MIXIN
         CHARACTER-OUTPUT-STREAM-MIXIN BASIC-OUTPUT-STREAM
         SI:BUFFERED-TYO-OUTPUT-STREAM))

(DEFFLAVOR ASCII-TRANSLATING-CHARACTER-STREAM
        ()
        (SI:ASCII-TRANSLATING-INPUT-STREAM-MIXIN SI:ASCII-TRANSLATING-OUTPUT-STREAM-MIXIN
         CHARACTER-INPUT-STREAM-MIXIN CHARACTER-OUTPUT-STREAM-MIXIN
         BASIC-INPUT-STREAM BASIC-OUTPUT-STREAM SI:BUFFERED-TYI-TYO-STREAM))

(COMPILE-FLAVOR-METHODS ASCII-TRANSLATING-INPUT-CHARACTER-STREAM
                        ASCII-TRANSLATING-OUTPUT-CHARACTER-STREAM
                        ASCII-TRANSLATING-CHARACTER-STREAM)

(DEFUN OPEN-STREAM (HOST CONTACT-NAME &KEY &OPTIONAL (WINDOW-SIZE DEFAULT-WINDOW-SIZE)
                                                     (TIMEOUT (* 10. 60.))
                                                     (DIRECTION :BIDIRECTIONAL)
                                                     (ERROR T)
                                                     (CHARACTERS T)
                                                     (ASCII-TRANSLATION NIL)
                                      &AUX CONN)
  "Open a chaosnet connection and return a stream that does i//o to it.
HOST is the host to connect to; CONTACT-NAME is the contact name at that host.
The keyword arguments are:
:WINDOW-SIZE - number of packets to allow in transit to this host over the connection.
:TIMEOUT - how long to wait before assuming the host is down.
:ASCII-TRANSLATION - if non-NIL, assume the data on the connection is in ASCII
 and translate to and from the Lisp machine character set as appropriate.
:DIRECTION, :CHARACTERS, :ERROR - as in OPEN.  :DIRECTION defaults to :BIDIRECTIONAL."
  (CONDITION-CASE-IF (NOT ERROR) (ERROR-OBJECT)
        (SETQ CONN (IF HOST
                       (CONNECT HOST CONTACT-NAME WINDOW-SIZE TIMEOUT)
                       (LISTEN CONTACT-NAME WINDOW-SIZE)))
    (SYS:REMOTE-NETWORK-ERROR ERROR-OBJECT)
    (:NO-ERROR
      (MAKE-STREAM CONN :DIRECTION DIRECTION
                        :CHARACTERS CHARACTERS
                        :ASCII-TRANSLATION ASCII-TRANSLATION))))

(DEFUN MAKE-STREAM (CONNECTION &KEY &OPTIONAL (DIRECTION :BIDIRECTIONAL)
                                              (CHARACTERS T)
                                              (ASCII-TRANSLATION NIL))
  "Return a stream that does I//O to an already established chaos connection.
:ASCII-TRANSLATION - if non-NIL, assume the data on the connection is in ASCII
 and translate to and from the Lisp machine character set as appropriate.
:DIRECTION, :CHARACTERS - as in OPEN.  :DIRECTION defaults to :BIDIRECTIONAL."
  (MAKE-INSTANCE (CASE DIRECTION
                   (:INPUT
                    (COND (ASCII-TRANSLATION 'ASCII-TRANSLATING-INPUT-CHARACTER-STREAM)
                          (CHARACTERS 'INPUT-CHARACTER-STREAM)
                          (T 'INPUT-BINARY-STREAM)))
                   (:OUTPUT
                    (COND (ASCII-TRANSLATION 'ASCII-TRANSLATING-OUTPUT-CHARACTER-STREAM)
                          (CHARACTERS 'OUTPUT-CHARACTER-STREAM)
                          (T 'OUTPUT-BINARY-STREAM)))
                   (:BIDIRECTIONAL
                    (COND (ASCII-TRANSLATION 'ASCII-TRANSLATING-CHARACTER-STREAM)
                          (CHARACTERS 'CHARACTER-STREAM)
                          (T 'BINARY-STREAM))))
                 :CONNECTION CONNECTION))

(DEFF STREAM 'MAKE-STREAM)
(MAKE-OBSOLETE STREAM "use MAKE-STREAM")

;;;; Useful information gatherers

;;; HOST-DATA: returns information about a specified host.  Currently,
;;; returns name of machine as primary value and host number as second value
(DEFUN HOST-DATA (&OPTIONAL (HOST MY-ADDRESS) &AUX HOST-ADDRESS HOST-NAME TEM)
  "Return the long name and chaos address of a host."
  (DECLARE (RETURN-LIST HOST-NAME HOST-ADDRESS))
  (OR (SETQ HOST-ADDRESS (ADDRESS-PARSE HOST))
      (FERROR NIL "~S is an illegal host specification" HOST))
  (IF (AND (SETQ HOST-NAME (SI:GET-HOST-FROM-ADDRESS HOST-ADDRESS :CHAOS))
           (SETQ HOST-NAME (SEND HOST-NAME :NAME)))
      (AND (SETQ TEM (ASSOC-EQUALP HOST-NAME SI:MACHINE-LOCATION-ALIST))
           (SETQ HOST-NAME (SECOND TEM)))
    (IF (SETQ TEM (GET-HOST-STATUS-PACKET HOST-ADDRESS))
        (LET ((STRING (PKT-STRING TEM)))
          (SETQ HOST-NAME (SUBSTRING STRING 0
                                     (MIN (PKT-NBYTES TEM) 32.
                                          (OR (STRING-SEARCH-CHAR 0 STRING) 32.)))))
      (SETQ HOST-NAME "Unknown")))
  (VALUES HOST-NAME HOST-ADDRESS))

;;; If given a number, this always returns something that ADDRESS-PARSE would make into that
;;; number.
(DEFUN HOST-SHORT-NAME (HOST &AUX HOST1)
  "Return a brief name for the specified host."
  (COND ((NOT (NUMBERP HOST))
         (SEND (SI:PARSE-HOST HOST) :SHORT-NAME))
        ((SETQ HOST1 (SI:GET-HOST-FROM-ADDRESS HOST :CHAOS))
         (SEND HOST1 :SHORT-NAME))
        (T (FORMAT NIL "~O" HOST))))

;(FSET 'HOST-SYSTEM-TYPE 'SI:HOST-SYSTEM-TYPE)

(DEFUN GET-HOST-STATUS-PACKET (HOST &AUX CONNECTION PKT ADR)
  "Returns a STATUS packet from the specified host or NIL if couldn't get the packet"
  (ASSURE-ENABLED)
  (SETQ ADR (OR (ADDRESS-PARSE HOST)
                (FERROR NIL "Not a known Chaos address: ~S" HOST)))
  (SETQ CONNECTION (OPEN-CONNECTION ADR "STATUS" 1))
  (DO () ((NULL CONNECTION))
      (PROCESS-SLEEP 10.)                       ;Take a few chaos net interrupts
      (CASE (STATE CONNECTION)
        (RFC-SENT-STATE
          (COND (( (TIME-DIFFERENCE (zl:TIME) (TIME-LAST-RECEIVED CONNECTION))
                    300.)               ;5-second timeout
                 (REMOVE-CONN CONNECTION)
                 (RETURN NIL))))
        (ANSWERED-STATE                         ;This is what we want
          (SETQ PKT (GET-NEXT-PKT CONNECTION))
          (CLOSE-CONN CONNECTION)
          (RETURN PKT))
        (CLS-RECEIVED-STATE (CLOSE-CONN CONNECTION) (RETURN NIL))
        (OPEN-STATE
          (CLOSE-CONN CONNECTION "I expected an ANS, not an OPN.")
          (RETURN NIL))
        (LOS-RECEIVED-STATE
          (CLOSE-CONN CONNECTION)
          (RETURN NIL))
        (OTHERWISE
          (CLOSE-CONN CONNECTION)
          (RETURN NIL)))))

(DEFUN ON-CHAOSNET-P (HOST)
  "Return T if HOST has a chaosnet address."
  (SEND (SI:PARSE-HOST HOST) :NETWORK-TYPEP :CHAOS))

(DEFINE-SITE-VARIABLE USUAL-LM-NAME-PREFIX :USUAL-LM-NAME-PREFIX)

(DEFUN GET-SHORT-LM-NAME (LM &AUX (PL (STRING-LENGTH USUAL-LM-NAME-PREFIX)))
  (IF USUAL-LM-NAME-PREFIX
      (DO ((L (SEND LM :HOST-NAMES) (CDR L)))
          ((NULL L) (SEND LM :SHORT-NAME))
        (AND (STRING-EQUAL USUAL-LM-NAME-PREFIX (CAR L) :END1 PL :END2 PL)
             (RETURN (CAR L))))
    (SEND LM :SHORT-NAME)))


;;; This isn't DEFINE-SITE-HOST-LIST because this file is loaded too early,
;;; as is the SITE file itself.
(DEFINE-SITE-VARIABLE TIME-SERVER-HOSTS :CHAOS-TIME-SERVER-HOSTS)

(SETQ TIME:*NETWORK-TIME-FUNCTION* 'HOST-TIME)

;;; Returns universal time from host over the net, as a 32-bit number
;;; or if it can't get the time, returns a string which is the reason why not.
;;; This applies each host for the time at a rate of one per second.
;;; As soon as one of them replies, it returns the time that host gave.
;;; 2nd value is host from which time was gotten.
(DEFUN HOST-TIME (&OPTIONAL (HOSTS TIME-SERVER-HOSTS) &AUX CONNECTIONS LAST-HOST)
  (ASSURE-ENABLED)
  (AND (NLISTP HOSTS)
       (NOT (NULL HOSTS))
       (SETQ HOSTS (LIST HOSTS)))
  (SETQ LAST-HOST (CAR (LAST HOSTS)))
  (UNWIND-PROTECT
    (LOOP NAMED HOST-TIME
          FOR HOST IN HOSTS
          AS ADDRESS = (ADDRESS-PARSE HOST)
          WHEN (AND ADDRESS ( ADDRESS MY-ADDRESS))
            DO (PUSH (OPEN-CONNECTION (ADDRESS-PARSE HOST) "TIME" 5) CONNECTIONS)
               (COND ((PROCESS-WAIT-WITH-TIMEOUT "Ask the Time"
                        (IF (EQ HOST LAST-HOST) 300. 60.)
                        #'(LAMBDA (CONNECTIONS)
                            (LOOP FOR CONNECTION IN CONNECTIONS
                                  WHEN (EQ (STATE CONNECTION) 'ANSWERED-STATE)
                                  RETURN T))
                        CONNECTIONS)
                      (LOOP WITH PKT
                            FOR CONNECTION IN CONNECTIONS
                            WHEN (EQ (STATE CONNECTION) 'ANSWERED-STATE)
                            DO (RETURN-FROM HOST-TIME
                                 (VALUES
                                   (PROG2 (SETQ PKT (GET-NEXT-PKT CONNECTION))
                                          (DECODE-CANONICAL-TIME-PACKET PKT)
                                          (RETURN-PKT PKT))
                                   (SI:GET-HOST-FROM-ADDRESS
                                     (FOREIGN-ADDRESS CONNECTION) :CHAOS))))))
          ELSE UNLESS ADDRESS DO
          (FORMAT *ERROR-OUTPUT* "~&Invalid host given to HOST-TIME by ~S" HOST)
          FINALLY (RETURN "No hosts responded."))
    (MAPC 'CLOSE-CONN CONNECTIONS)))


;; Copied from LAD: RELEASE-3.NETWORK.CHAOS; CHUSE.LISP#28 on 2-Oct-86 17:22:45
(network:define-network-function (network:get-host-time :chaos) (host)
  (multiple-value-bind (time ahost)
      (host-time (list host))
    (if ahost time)))


(DEFUN CHAOS-UNKNOWN-HOST-FUNCTION (NAME)
  (DOLIST (HOST (SI:GET-SITE-OPTION :CHAOS-HOST-TABLE-SERVER-HOSTS))
    (AND (SI:PARSE-HOST HOST T ())              ; prevent infinite recursion
         (WITH-OPEN-STREAM (STREAM (OPEN-STREAM HOST "HOSTAB" :ERROR NIL))
           (SETQ NAME (STRING NAME))
           (UNLESS (ERRORP STREAM)
             (SEND STREAM :LINE-OUT NAME)
             (SEND STREAM :FORCE-OUTPUT)
             (DO ((LIST NIL) (RESULT) (DONE)
                  (LINE) (EOF)
                  (LEN) (SP) (PROP))
                 (DONE RESULT)
               (MULTIPLE-VALUE (LINE EOF) (SEND STREAM :LINE-IN))
               (cond (EOF
                      (SETQ RESULT (WHEN LIST
                                     (PUTPROP LIST (STABLE-SORT (GET LIST :HOST-NAMES)
                                                                #'(LAMBDA (X Y)
                                                                    (< (STRING-LENGTH X)
                                                                       (STRING-LENGTH Y))))
                                              :HOST-NAMES)
                                     (APPLY #'SI:DEFINE-HOST LIST))
                            DONE T))
                     (t
                      (SETQ LEN (STRING-LENGTH LINE)
                            SP (STRING-SEARCH-CHAR #\SP LINE 0 LEN))
                      (SETQ PROP (INTERN (SUBSTRING LINE 0 SP) ""))
                      (INCF SP)
                      (CASE PROP
                        (:ERROR (SETQ DONE T))
                        (:NAME
                         (LET ((NAME (SUBSTRING LINE SP LEN)))
                           (OR LIST (SETQ LIST (NCONS NAME)))
                           (PUSH NAME (GET LIST :HOST-NAMES))))
                        ((:SYSTEM-TYPE :MACHINE-TYPE)
                         (PUTPROP LIST (INTERN (SUBSTRING LINE SP LEN) "") PROP))
                        (OTHERWISE
                         (LET ((FUNCTION (GET PROP 'NET:ADDRESS-PARSER)))
                           (OR FUNCTION (SETQ FUNCTION (GET :CHAOS 'NET:ADDRESS-PARSER)))
                           (PUSH (FUNCALL FUNCTION PROP LINE SP LEN)
                                 (GET LIST PROP))))))))
             (RETURN T))))))

(SETQ SI:UNKNOWN-HOST-FUNCTION 'CHAOS-UNKNOWN-HOST-FUNCTION)

(DEFUN NEW-HOST-VALIDATION-FUNCTION (HOST SYSTEM-TYPE ADDRESS)
  (COND ((NOT (STRINGP HOST))
         (AND ADDRESS
              (NOT (MEMQ ADDRESS (SEND HOST :CHAOS-ADDRESSES)))
              (FERROR NIL "~O is not a valid chaosnet address for ~A" ADDRESS HOST))
         HOST)
        (T
         (LET ((STATUS-PKT (GET-HOST-STATUS-PACKET ADDRESS)))
           (OR STATUS-PKT (FERROR NIL "Cannot connect to ~A at ~O" HOST ADDRESS))
           (LET ((STRING (PKT-STRING STATUS-PKT)))
             (OR (FQUERY NIL "Host is ~A, ok? "
                         (SUBSTRING STRING 0 (MIN (STRING-LENGTH STRING) 32.
                                                  (OR (STRING-SEARCH-SET '(#o200 0) STRING)
                                                      32.))))
                 (FERROR NIL "Incorrect host specified"))))
         (SI:DEFINE-HOST HOST :HOST-NAMES `(,HOST)
                              :SYSTEM-TYPE SYSTEM-TYPE
                              :CHAOS `(,ADDRESS))
         (SETQ HOST (SI:PARSE-HOST HOST))
         (AND (EQ CHAOS:MY-ADDRESS ADDRESS) (SETQ SI:LOCAL-HOST HOST))
         HOST)))

(setf (get 'si:new-host-validation-function :chaos) 'chaos:new-host-validation-function)

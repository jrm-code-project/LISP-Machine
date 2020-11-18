;;; -*- Mode:LISP; Package:CHAOS; Base:10; Readtable:CL -*-
;;; This is SYS: NETWORK; CHAOS; CHSNCP
;;;
;;;     ** (c) Copyright 1980, 1984 Massachusetts Institute of Technology **
;;;
;;; Lisp Machine package for using the ChaosNet.  Does not contain Ethernet II interface.
;;; New Protocol of May, 1978
;;;
;;; Software details for general use and the Lisp Machine are documented in the
;;; chapter ``The Chaosnet'' in the Lisp Machine manual.

(net:defstream chaos-network-protocol
               (net:network-protocol)
               "CHAOS-"
  )

(defvar *chaos-stream* nil "chaos-network-protocol for Chaos")

(defun chaos-special-addresses (address interface)
  (and (zerop address) interface (net:ni-broadcast-address interface)))

(defmacro chaos-subnet-number-from-address (address)
  `(ldb (byte 8 8) ,address))

(defun chaos-packet-length (int-pkt offset)
  (+ 16 (ldb (byte 12 0) (aref int-pkt (1+ offset)))))

(defun setup-chaos (&aux (my-subnet (chaos-subnet-number-from-address my-address)))
  (if background
      (send background :flush)
    (setq background (make-process "Chaos Background" :warm-boot-action nil :priority 25.)))

  (setq *chaos-stream* (make-chaos-network-protocol :keyword :chaos
                                                    :address-length 2
                                                    :big-endian-address-p nil
                                                    :disable-function 'disable
                                                    :enable-function 'enable
                                                    :interrupt-function 'receive-chaos-pkt
                                                    :special-address-function 'chaos-special-addresses
                                                    :packet-length-function 'chaos-packet-length
                                                    :gauge-name "Chaos"
                                                    ))
  (setup-my-address)
  (send *chaos-stream* :open my-address my-subnet)
  (send *chaos-stream* :enable)
  )

;;; TO BE FIXED!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; **** Fix the packet recording stuff****
;;; **** Per-connection retransmission timeouts/intervals  (Yow !  Do we have land lines
;;;      yet ?)

;;; This file contains the CHAOS net software from the packet level down

;;;Some standard abbreviations and mnemonic indicators:
;;;Items in this list with no "-" at either end signify abbreviations
;;;     which may occur in any context, those with one or more "-"'s
;;;     only occur in a context in which there are "-"'s in exactly
;;;     those places.
;;; PKT         Abbreviation for PACKET.  see the DEFSTRUCT
;;; CONN                Abbreviation for CONNECTION.  see the DEFSTRUCT
;;; SEND-               Cause a packet to be "sent" with retry etc. if applicable.
;;; TRANSMIT-   Cause a packet to be "transmitted" (i.e. placed on the net).
;;;                     This is done by all the SEND- routines by calling
;;;                     an appropriate TRANSMIT- routine to put the packet
;;;                     on a list of packets to be put on the net.
;;; -STATE      A symbol representing a state that a connection might be in.
;;; -OP         A system constant whose value represents that of a packet op code.

;;; The file is divided into more-or-less localized units.  These units
;;;     and which of the contained functions are intended for outside
;;;     use are listed here in the order in which they appear in the file.

;;; Definitions for high-level structures (with functions to print them textually)
;;;     and various macros for the specialized formulas of the protocol
;;;   (Everything here is used everywhere)

;;; Low-level PKT management.
;;; functions referenced elsewhere:
;;;   ALLOCATE-PKT, FREE-PKT, SET-PKT-STRING

;;; Low-level CONN management.
;;; functions referenced elsewhere:
;;;   MAKE-CONNECTION, REMOVE-CONN

;;; High level Transmission routines
;;; functions referenced elsewhere:
;;;   TRANSMIT-LOS-INT-PKT, TRANSMIT-STS, TRANSMIT-NORMAL-PKT

;;;; Opcodes of packets
(DEFCONSTANT RFC-OP #o1 "Opcode value for RFC packets.")
(DEFCONSTANT OPN-OP #o2 "Opcode value for OPN packets.")
(DEFCONSTANT CLS-OP #o3 "Opcode value for CLS packets.")
(DEFCONSTANT FWD-OP #o4 "Opcode value for FWD packets.")
(DEFCONSTANT ANS-OP #o5 "Opcode value for ANS packets.")
(DEFCONSTANT SNS-OP #o6 "Opcode value for SNS packets.")
(DEFCONSTANT STS-OP #o7 "Opcode value for STS packets.")
(DEFCONSTANT RUT-OP #o10 "Opcode value for RUT packets.")
(DEFCONSTANT LOS-OP #o11 "Opcode value for LOS packets.")
(DEFCONSTANT LSN-OP #o12 "Opcode value for LSN packets.")
(DEFCONSTANT MNT-OP #o13 "Opcode value for MNT packets.")
(DEFCONSTANT EOF-OP #o14 "Opcode value for EOF packets.")
(DEFCONSTANT UNC-OP #o15 "Opcode value for UNC packets.")
(DEFCONSTANT BRD-OP #o16 "Opcode value for BRD packets.")
(DEFCONSTANT DAT-OP #o200 "Default data opcode for 8-bit byte data.")

;;; This is for printing out packets nicely.
(DEFPARAMETER OPCODE-LIST '(ZERO? RFC OPN CLS FWD ANS SNS STS RUT LOS LSN MNT EOF UNC BRD)
  "List of system packet opcode names, indexed by opcode value.")

;;; Size-of-packet symbols.  These symbols define the protocol specified sizes for standard
;;;  chaos protocol.
(DEFCONSTANT MAX-WORDS-PER-PKT 252.
  "Largest packet possible in 16-bit words, including header.")
(DEFCONSTANT MAX-DATA-WORDS-PER-PKT 244. "Number of 16-bit data words in largest packet.")
(DEFCONSTANT MAX-DATA-BYTES-PER-PKT 488. "Number of 8-bit data bytes in largest packet.")
(DEFCONSTANT FIRST-DATA-WORD-IN-PKT 8.
  "Offset to first data word in packet (or, number of bytes in header).
Note that the contents of the packet, as an array, includes its header.")

(DEFVAR PKT-LEADER-SIZE :UNBOUND "Number of elements in array leader of a PKT.")

;;; Clock constants
(DEFPARAMETER RETRANSMISSION-INTERVAL 30.)      ;  1/2 second
(DEFPARAMETER PROBE-INTERVAL (* 60. 10.))       ; 10 seconds
(DEFPARAMETER LONG-PROBE-INTERVAL (* 60. 60.))  ;  1 minute
(DEFPARAMETER HOST-DOWN-INTERVAL (* 60. 60. 5)) ;  5 minutes

(DEFVAR BACKGROUND-REQUESTS NIL "List of requests to the background ChaosNet process.")
(DEFVAR RETRANSMISSION-NEEDED T
  "T if retransmission of packets may be needed.
Enables background process to wake up on clock.
Set this whenever you put something on SEND-PKTS of a CONN.")

(DEFVAR MORE-RETRANSMISSION-NEEDED)

(DEFPARAMETER DEFAULT-WINDOW-SIZE #o15 "This is the default size of the window for a CONN.")
(DEFPARAMETER MAXIMUM-WINDOW-SIZE #o200 "This is the maximum size of the window for a CONN.")

(DEFPARAMETER MAXIMUM-INDEX #o200
  "Length of INDEX-CONN; number of distinct connection-indices.")
(DEFPARAMETER MAXIMUM-INDEX-LOG-2-MINUS-1 (1- (HAULONG MAXIMUM-INDEX))
  "Number of bits in a connection index.")

;;; This array holds the CONN for the given index number.
;;; It is big enough that no uniquizing is needed, since it is
;;; used in circular fashion.
(DEFVAR INDEX-CONN (zl:MAKE-ARRAY MAXIMUM-INDEX :AREA PERMANENT-STORAGE-AREA)
  "Array holding the connection for each index number.
No two connections at any one time can have the same index number.
Index numbers go in packets to identify which connection they are for.")
(DEFVAR INDEX-CONN-FREE-POINTER 1
  "Next connection index to consider using for a new connection.")

;;; This array holds the uniquizer for the current (or last) connection for a given index
(DEFVAR UNIQUIZER-TABLE (zl:MAKE-ARRAY MAXIMUM-INDEX :TYPE 'ART-16B
                                                  :AREA PERMANENT-STORAGE-AREA)
  "For each connection index, holds last uniquizer value.
The uniquizer is incremented each time a new connection is made
for the given index, and it is used together with the index
in identifying the connection a packet is intended for.")

(DEFVAR BACKGROUND nil
  "This process runs all chaosnet time response actions such as PROBEs and Retransmission,
and some other things for the net.")

(DEFVAR ENABLE NIL
  "T if chaosnet is enabled (turned on for use).")

;;; The following are used for negotiating the initial connection between a host and a server.
(DEFVAR PENDING-RFC-PKTS NIL
  "Incoming RFC packets not yet LISTENed for, linked through the PKT-LINK.")
(DEFVAR PENDING-LISTENS NIL
  "List of (CONTACT-NAME . CONN) for pending listens.")

;;; BRD in direction meters
(DEFVAR *BRD-HISTORY* ()
  "A list describing answered BRDs (contact-name host-address)")
(DEFVAR *BRD-PKTS-IN* 0 "Number of answered BRD packets")

;;; This is NIL at first to leave the machine in peace during the cold load
(DEFVAR *RECEIVE-BROADCAST-PACKETS-P* () "BRD packets are responded to if this is T.")

;;; BRD out direction meters
(DEFVAR *BRD-REPLIES-IN* 0 "Number of replies from opening a broadcast connection")
(DEFVAR *BRD-PKTS-OUT* 0 "Number of BRDs we have transmitted")

(DEFVAR SERVER-ALIST NIL
  "Alist of (CONTACT-NAME FORM-TO-EVALUATE) for creating chaos servers.
Entries are put on with ADD-INITIALIZATION.
The form is evaluated in the background task when an incoming RFC
matches the contact name and there is no pending listen for it.")

;;; Packet lists and other pointers
(DEFVAR FREE-PKTS NIL
  "Chain of free packets, linked through the PKT-LINK field.")
(DEFVAR MADE-PKTS NIL
  "Chain of all packets constructed, linked through the PKT-MADE-LINK field.")

;;; Connection list
(DEFVAR CONN-LIST NIL
  "List of existing CONNs.")
(DEFVAR FREE-CONN-LIST NIL "List of free CONN structures (save consing).")
(DEFVAR PROTOTYPE-CONN :UNBOUND
  "A CONN object used for initializing other CONNs when they are made.
The prototype is simply copied into the new CONN.")
(DEFVAR DISTINGUISHED-PORT-CONN-TABLE)  ;Assq list of special port numbers and conns
                                        ;This is because of EFTP


;;;; Meters
(DEFVAR PKTS-FORWARDED :UNBOUND
  "Incremented when we forward a PKT to someone.")
(DEFVAR PKTS-OVER-FORWARDED :UNBOUND
  "Incremented when we forward a PKT for the nth time and discard it as a consequence.")
(DEFVAR PKTS-MADE :UNBOUND
  "Number of PKTs ever created.")
(DEFVAR PKTS-RECEIVED :UNBOUND
  "Number of packets received from the chaosnet.")
(DEFVAR PKTS-TRANSMITTED :UNBOUND
  "Number of packets transmitted to the chaosnet.")
(DEFVAR PKTS-OTHER-DISCARDED :UNBOUND
  "Incremented when we discard a packet for misc other reasons.
Such as, too small to contain a protocol packet.")
(DEFVAR LOS-PKT-COUNT :UNBOUND
  "Number of all LOS packets ever received.")
(DEFVAR CURRENT-LOS-PKT-COUNT :UNBOUND
  "Number of packets currently on LOS-PKTS.")
(DEFVAR PKTS-RETRANSMITTED :UNBOUND
  "Number of packets retransmitted.")
(DEFVAR PKTS-DUPLICATED :UNBOUND
  "Number of duplicate packets received.")
(DEFVAR DATA-PKTS-IN :UNBOUND
  "Number of data packets received.")
(DEFVAR DATA-PKTS-OUT :UNBOUND
  "Number of data packets transmitted.")
  ;also SI:%COUNT-CHAOS-TRANSMIT-ABORTS which is maintained by the microcode
  ;Reference this with READ-METER, WRITE-METER

;;;; Debugging aids which keep records into the past (a short way).
(DEFVAR BAD-PKT-LIST NIL
  "List of strings describing packets received in error.")
(DEFVAR PKTS-BAD-CRC-SAVE-P NIL
  "Non-NIL means save all packets with bad crc.")
(DEFVAR LOS-PKTS NIL
  "Chain of recent LOS PKTs received from the network, linked by PKT-LINK.")
(DEFVAR MAX-LOS-PKTS-TO-KEEP 16.
  "Maximum number of LOS packets to keep on LOS-PKTS.
There may actually be more but they will be used by allocator.")
(DEFVAR RECENT-HEADERS :UNBOUND
  "Array of #o200 most recent packet transactions' headers.
Each row of the array contains the eight header words of the packet
and the time at which the record was made.")
(DEFVAR RECENT-HEADERS-POINTER :UNBOUND
  "Next index to use in storing in RECENT-HEADERS.")

(DEFVAR MY-ADDRESS :UNBOUND "This machine's chaosnet address.")
(DEFVAR MY-SUBNET :UNBOUND "This machine's chaosnet subnet number.")

;;; Will be used in the future by bridging machines
(DEFVAR MY-OTHER-ADDRESSES NIL "Secondary chaos addresses.")
(DEFVAR MY-OTHER-SUBNETS NIL "Secondary chaos subnet numbers.")

;;; This array is the routing table:  if we want to send a message to a given
;;; subnet, where should I forward it?  If the subnet # is greater than the
;;; length of the array, use the contents of array element zero.
;;; The contents of the array are the host number on our subnet who knows
;;; how to handle this packet.  NOTE that for now we can only be on one subnet.
;;; Don't use this table unless you are sure that the packet is not going to
;;; a host on THIS subnet!
;;; These tables are filled in by code in INITIALIZE-NCP-ONCE.
(DEFPARAMETER ROUTING-TABLE-SIZE #o400 "the number of subnets in the routing table")
(DEFVAR ROUTING-TABLE nil)
(DEFVAR ROUTING-TABLE-COST nil)
(DEFVAR ROUTING-TABLE-TYPE nil "the type of the subnet")
(defvar default-routing-cost 10. "The default routing cost")
(DEFVAR MAXIMUM-ROUTING-COST 1024. "The maximum value in the routing table that is real.")

(defun list-route-table ()
  (format t "~&Subnet~16TGateway~32TCost~48TType")
  (dotimes (i routing-table-size)
    (unless (zerop (aref routing-table i))
      (format t "~&~A~16T~O~32T~S~48T~S"
                (if (zerop i) "DEFAULT" i)
                (aref routing-table i)
                (aref routing-table-cost i)
                (aref routing-table-type i))))
  )

(defun add-gateway (subnet gateway &optional (cost default-routing-cost) (type :ethernet) &aux parsed-gateway)
  (check-type subnet (integer 0 (#.routing-table-size)))
  (assert (setq parsed-gateway (address-parse gateway))
          (gateway)
          "Bad Chaos address specified for GATEWAY:~A"
          gateway)
  (setf (aref routing-table subnet) parsed-gateway)
  (setf (aref routing-table-cost subnet) cost)
  (setf (aref routing-table-type subnet) type)
  t)

(defun remove-gateway (subnet)
  (check-type subnet (integer 0 (#.routing-table-size)))
  (setf (aref routing-table subnet) 0)
  (setf (aref routing-table-cost subnet) maximum-routing-cost)
  (setf (aref routing-table-type subnet) nil))

;;; These are interesting meters
(DEFPARAMETER PEEK-A-BOO-LIST
          '(PKTS-FORWARDED PKTS-OVER-FORWARDED PKTS-BAD-BIT-COUNT PKTS-BAD-DEST
            PKTS-BAD-CRC-1 PKTS-BAD-CRC-2 PKTS-LOST PKTS-MADE PKTS-RECEIVED
            PKTS-TRANSMITTED PKTS-OTHER-DISCARDED LOS-PKT-COUNT
            CURRENT-LOS-PKT-COUNT PKTS-RETRANSMITTED PKTS-DUPLICATED DATA-PKTS-IN
            DATA-PKTS-OUT))

(DEFUN RESET-METERS ()
  (DOLIST (METER PEEK-A-BOO-LIST)
    (SET METER 0))
  (WRITE-METER 'SYS:%COUNT-CHAOS-TRANSMIT-ABORTS 0))

;;; avoid unbound-symbol errors if new things have been added to peek-a-boo-list.
;;; These can be very embarrassing.
(RESET-METERS)

(DEFUN RESET-ROUTING-TABLE ()
  "Flush out old routing data."
  (SETQ MY-OTHER-SUBNETS NIL)
  (DOTIMES (I (ARRAY-LENGTH ROUTING-TABLE))     ;Clear out the routing table
    (SETF (AREF ROUTING-TABLE I) 0)
    (SETF (AREF ROUTING-TABLE-COST I) MAXIMUM-ROUTING-COST)
    (SETF (AREF ROUTING-TABLE-TYPE I) NIL))
  (SETF (AREF ROUTING-TABLE MY-SUBNET) MY-ADDRESS)
  (SETF (AREF ROUTING-TABLE-COST MY-SUBNET) default-routing-cost)
  ;; RPK please check this
  ;; LMI-specific numeric host numbers garbage flushed from this function by RMS, 8/28/84
  ;; Regretfully reinstated by KHS, 9/17/84
  ;; at least modularized and made site independant due to a function by MWT, 2/05/85 10:09:21
  (cond ((null (get-site-option :lmi-bridge-kludge))
         (setf (aref routing-table-type my-subnet)
               (select-processor
                 (:cadr :chaos)
                 ((:lambda :explorer :falcon) :ethernet))))
        (t
         ;; any site which has bridges which do not send routing packets will need to run
         ;; this code. One example is a site which has given different sets of machines
         ;; on the same physical ether different subnet addresses. A "feature" is that
         ;; these machines won't see each other unless :LMI-BRIDGE-KLUDGE is on.
         ;; One use of this has been to isolate "EDUCATION" and "DEVELOPEMENT" use machines.
         ;; Ah, sometimes turning a kludge into a feature is a hackers only reward.
         (set-routing-table-from-host-table))))


(defun set-routing-table-from-host-table ()
  ;; more general code
    ;;; Initialize table entries for any other subnets used at this site.
  (let (host-addresses-all temp address-of-bridge-on-my-subnet)
    (dolist (host-alist-elem si:host-alist)
      (cond ((and (eq si:site-name (si:host-site-name host-alist-elem))
                  (setq temp (memq :CHAOS (si:host-addresses host-alist-elem))))
             (setq host-addresses-all (cadr temp))
             (dolist (addr host-addresses-all)
               (or (aref routing-table-type (chaos-subnet-number-from-address addr))
                   (setf (aref routing-table-type (chaos-subnet-number-from-address addr))
                         :ETHERNET)))
             (cond ((cdr host-addresses-all)    ; Is it a bridge?

                    (setq address-of-bridge-on-my-subnet
                          (car (mem #'(lambda (subnet addr) (= subnet (ldb #o1010 addr)))
                                    my-subnet host-addresses-all)))

                    (and address-of-bridge-on-my-subnet
                         (dolist (addr host-addresses-all)
                           (and (zerop (aref routing-table (chaos-subnet-number-from-address addr)))
                                (setf (aref routing-table (chaos-subnet-number-from-address addr))
                                      address-of-bridge-on-my-subnet)))))))))))



;;;; High Level Structures

;;; Definitions for high level structures and various macros for the
;;;     specialized formulas of the protocol

;;; STRUCTURE DEFINITIONS: Connections (CONNs) and Packets (PKTs)

(DEFSUBST CONN-STATE (CONN)
  "The current state of CONN.
Possible states are CHAOS:INACTIVE-STATE, CHAOS:ANSWERED-STATE,
CHAOS:CLS-RECEIVED-STATE, CHAOS:LISTENING-STATE, CHAOS:RFC-RECEIVED-STATE,
CHAOS:RFC-SENT-STATE, CHAOS:OPEN-STATE, CHAOS:LOS-RECEIVED-STATE,
CHAOS:HOST-DOWN-STATE, CHAOS:FOREIGN-STATE, CHAOS:BROADCAST-SENT-STATE"
  (STATE CONN))

(zl:DEFSTRUCT (CONN :ARRAY :NAMED (:ALTERANT NIL))
  "This structure is a connection, abbreviated CONN."
  (LOCAL-WINDOW-SIZE NIL :DOCUMENTATION "Window size for receiving")
  (FOREIGN-WINDOW-SIZE NIL :DOCUMENTATION "Window size for transmitting")
  (STATE 'INACTIVE-STATE :DOCUMENTATION "State of this connection")
;;; States in which a connection may be.

;;;     INACTIVE-STATE
;;; This state indicates the CONN is not currently associated with any CHAOS channel.

;;;     ANSWERED-STATE
;;; This state indicates the CONN has received an ANS from the other end of the channel;
;;;       it is waiting to be read.
;;;   No I/O is allowed except reading the ANS packet already on the READ-PKTS chain.

;;;     CLS-RECEIVED-STATE
;;; This state indicates the CONN has received a CLS from the other end of the channel;
;;;       any data packets that were received in order before the CLS are waiting
;;;       to be read, followed by the CLS packet.
;;;   No I/O is allowed except reading the packets already on th READ-PKTS chain.

;;;     LISTENING-STATE
;;; This state is given to a CONN on which a LSN has been sent while it is awaiting the RFC.

;;;     RFC-RECEIVED-STATE
;;; This state indicates that an RFC has been received on this CONN but that no response
;;;     has yet been given (such as done by ACCEPT and REJECT).

;;;     RFC-SENT-STATE
;;; This state indicates that there has been an RFC sent on this CONN but that no response
;;;     has yet been received from the foreign host.

;;;     OPEN-STATE
;;; This state is the normal state for an open connection

;;;     LOS-RECEIVED-STATE
;;; This state indicates the CONN has received a LOS from the other end of the channel;
;;;     the LOS packet will be the data packet waiting to be read; any READ-PKTS
;;;     or SEND-PKTS are discarded
;;;   No I/O is allowed except reading the packets already on th READ-PKTS chain.

;;;     HOST-DOWN-STATE
;;; This state is entered when it is determined that the foreign
;;;     host is down (or something). This is done in PROBE-CONN.
;;;   No I/O is allowed except reading the packets already on th READ-PKTS chain.

;;;     FOREIGN-STATE
;;; Allows UNC packets to go in and out, for implementing non-Chaosnet protocols

;;;     BROADCAST-SENT-STATE
;;; We have transmitted a broadcast packet.  The following actions are possible:
;;;     1. The user can send more BRD packets.
;;;     2. The user can read all packets except OPNs (until buffering is exceeded)
;;;     3. OPNs, when there are no queued input packets, will cause the connection
;;;        to enter the OPEN state.  Any other packets received from other hosts after
;;;        this OPEN state is reached will evoke a LOS packet.

  (FOREIGN-ADDRESS 0 :DOCUMENTATION "Address <his> for the other end of this CONN")
  (FOREIGN-INDEX-NUM 0 :DOCUMENTATION "Index number <his> for the other end of this CONN")
  ;; LOCAL-ADDRESS is a constant and therefore not needed or included
  (LOCAL-INDEX-NUM 0 :DOCUMENTATION "Index number <mine> for this end of the CONN")
  (READ-PKTS NIL :DOCUMENTATION "Packets which have been read from the net and are in order")
  (READ-PKTS-LAST NIL :DOCUMENTATION "Last packet in READ-PKTS of the CONN")
  (RECEIVED-PKTS NIL :DOCUMENTATION "Packets which have been received but are not in order")

  (PKT-NUM-READ -1 :DOCUMENTATION "The <his> highest packet number given to user.")
  (PKT-NUM-RECEIVED -1 :DOCUMENTATION
                    "The <his> highest packet number in ordered list (READ-PKTS)")
  (PKT-NUM-ACKED -1 :DOCUMENTATION "The level of acknowledgement we have sent out to date")
  (TIME-LAST-RECEIVED NIL :DOCUMENTATION "Time of last input from net.")

  (SEND-PKTS NIL :DOCUMENTATION "List of packets which we must send.")
  (SEND-PKTS-LAST NIL :DOCUMENTATION "Last PT on SEND-PKTS")
  (SEND-PKTS-LENGTH 0 :DOCUMENTATION "Length of SEND-PKTS chain")
  (PKT-NUM-SENT 0 :DOCUMENTATION "Highest <our> packet number assigned.")
  (SEND-PKT-ACKED 0 :DOCUMENTATION
                  "The last packet number for which we received acknowledgement")
  (WINDOW-AVAILABLE 0 :DOCUMENTATION "Space in window not occupied by unacknowledged packets")
  (RETRANSMISSION-INTERVAL 30.
                           :DOCUMENTATION "Retransmission interval for this CONN in 60ths.")

  (INTERRUPT-FUNCTION NIL :DOCUMENTATION
                      "Function to be called in when a new packet arrives at the head of READ-PKTS")
  (CONN-PLIST NIL :DOCUMENTATION
              "Properties include RFC-CONTACT-NAME and LISTEN-CONTACT-NAME,
which record the contact names used in the two directions."))


(DEFSUBST CONN-LOCAL-WINDOW-SIZE (CONN) (LOCAL-WINDOW-SIZE CONN))
(DEFSUBST CONN-FOREIGN-WINDOW-SIZE (CONN) (FOREIGN-WINDOW-SIZE CONN))

(DEFSUBST CONN-FOREIGN-ADDRESS (CONN)
  "Address of host at other end of CONN."
  (FOREIGN-ADDRESS CONN))

(DEFSUBST CONN-FOREIGN-INDEX-NUM (CONN) (FOREIGN-INDEX-NUM CONN))
(DEFSUBST CONN-LOCAL-INDEX-NUM (CONN) (LOCAL-INDEX-NUM CONN))
(DEFSUBST CONN-READ-PKTS (CONN)
  "Chain of sequential packets available for reading from CONN by user."
  (READ-PKTS CONN))
(DEFSUBST CONN-READ-PKTS-LAST (CONN) (READ-PKTS-LAST CONN))
(DEFSUBST CONN-RECEIVED-PKTS (CONN) (RECEIVED-PKTS CONN))
(DEFSUBST CONN-PKT-NUM-READ (CONN) (PKT-NUM-READ CONN))
(DEFSUBST CONN-PKT-NUM-RECEIVED (CONN) (PKT-NUM-RECEIVED CONN))
(DEFSUBST CONN-PKT-NUM-ACKED (CONN) (PKT-NUM-ACKED CONN))
(DEFSUBST CONN-TIME-LAST-RECEIVED (CONN) (TIME-LAST-RECEIVED CONN))
(DEFSUBST CONN-SEND-PKTS (CONN) (SEND-PKTS CONN))
(DEFSUBST CONN-SEND-PKTS-LAST (CONN) (SEND-PKTS-LAST CONN))
(DEFSUBST CONN-SEND-PKTS-LENGTH (CONN) (SEND-PKTS-LENGTH CONN))
(DEFSUBST CONN-PKT-NUM-SENT (CONN) (PKT-NUM-SENT CONN))
(DEFSUBST CONN-SEND-PKT-ACKED (CONN) (SEND-PKT-ACKED CONN))
(DEFSUBST CONN-RETRANSMISSION-INTERVAL (CONN) (RETRANSMISSION-INTERVAL CONN))

(DEFSUBST CONN-WINDOW-AVAILABLE (CONN)
  "Number of packets that may be sent on CONN before outgoing window is full."
  (WINDOW-AVAILABLE CONN))

(DEFSUBST CONN-INTERRUPT-FUNCTION (CONN)
  "Function to be called when a new packet arrives for CONN."
  (INTERRUPT-FUNCTION CONN))

(DEFUN CONTACT-NAME (CONN)
  "Return the contact name with which connection CONN was created, or NIL if none.
Can be NIL if the connection is in a weird state."
  (OR (GET (LOCF (CONN-PLIST CONN)) 'RFC-CONTACT-NAME)
      (GET (LOCF (CONN-PLIST CONN)) 'LISTEN-CONTACT-NAME)))

(DEFSELECT ((:PROPERTY CONN NAMED-STRUCTURE-INVOKE))
  (:DESCRIBE (CONN)
    (PRINT-CONN CONN)
    (SI:DESCRIBE-DEFSTRUCT CONN 'CONN))
  (:PRINT-SELF (CONN STREAM IGNORE &OPTIONAL IGNORE)
    (SYS:PRINTING-RANDOM-OBJECT (CONN STREAM)
      (SEND STREAM :STRING-OUT "CHAOS Connection")
      (LET ((FHOST (SI:GET-HOST-FROM-ADDRESS (FOREIGN-ADDRESS CONN) :CHAOS))
            CONTACT)
        (COND ((SETQ CONTACT (GETF (CONN-PLIST CONN) 'RFC-CONTACT-NAME))
               (FORMAT STREAM " to ~A ~A" FHOST CONTACT))
              ((SETQ CONTACT (GETF (CONN-PLIST CONN) 'SERVER-CONTACT-NAME))
               (FORMAT STREAM " from ~A to ~A server" FHOST CONTACT))))))
  ;; These are here for the benefit of NET:DEFINE-SERVER and more generic network stuff
  (:ANSWER-STRING (CONN STRING) (ANSWER-STRING CONN STRING))
  (:ANSWER (CONN PKT HI)
    (SETF (PKT-NBYTES-on-write PKT) (- HI FIRST-DATA-WORD-IN-PKT))
    (ANSWER CONN PKT))
  (:GET-DATAGRAM (IGNORE) (VALUES (GET-PKT) FIRST-DATA-WORD-IN-PKT MAX-DATA-BYTES-PER-PKT))
  (:RETURN-DATAGRAM (IGNORE PKT) (RETURN-PKT PKT))
  (:REJECT (CONN &OPTIONAL (REASON "")) (REJECT CONN REASON))
  (:SECURE-P (CONN) (SECURE-P-INTERNAL CONN))
  (:FOREIGN-HOST (CONN) (SI:GET-HOST-FROM-ADDRESS (FOREIGN-ADDRESS CONN) :CHAOS))
  (:NETWORK (CONN) CONN :CHAOS)
  (:ADD-AS-SERVER (CONN NAME &OPTIONAL (PROCESS CURRENT-PROCESS))
    (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONN NAME PROCESS))
  (:DELETE-AS-SERVER (CONN)
    (SEND TV:WHO-LINE-FILE-STATE-SHEET :DELETE-SERVER CONN))
  (:REMOVE (CONN) (REMOVE-CONN CONN)))


;;;; Packets.
;;; ***THESE DEFSTRUCTS USED BY QFILE!  RECOMPILE IT IF THEY CHANGE!***
(zl:DEFSTRUCT (PKT-LEADER :ARRAY-LEADER (:CONSTRUCTOR NIL) (:ALTERANT NIL)
                          (:SIZE-SYMBOL PKT-LEADER-SIZE))
  "This structure is a packet, abbreviated PKT.
The elements of the array are the actual bits of the packet, whereas
the elements of the leader are internal information not transmitted."
  PKT-ACTIVE-LENGTH                             ;Not used
  (PKT-NAMED-STRUCTURE-SYMBOL PKT)              ;Note PKT not PKT-LEADER;
                                                ; 2 defstructs for 1 object!
  (PKT-TIME-TRANSMITTED nil :documentation
                        "Time this PKT last transmitted")
  (PKT-TIMES-TRANSMITTED nil :documentation
                         "Number of times this PKT has been transmitted")
  (PKT-STRING nil :documentation
              "A string which is the bytes of the PKT")
  (PKT-LINK nil :documentation
            "Links PKTs in the chain that describes them")
  (PKT-MADE-LINK nil :documentation
                 "Links all packets ever made")
  ;; for all three -LINKs NIL = Last in chain, T = Not on chain at all.
  ;; PKT-LINK is T only if the PKT was freed but was on transmit list
  ;; and so is temporarily kept.
  (PKT-BEING-RETRANSMITTED nil :documentation
                           "T if the packet is being retransmitted and so cannot be really freed.
If this is the case, it is bashed to be FREE so that the retransmitter
will know to free it up")
  (PKT-STATUS nil :documentation
              "Status of the packet
The status slot is used by the NCP to remember a small amount of info about the packet:
  NIL      Normal packet, in use by the NCP
  RELEASED Packet has been given to the user"))


(zl:DEFSTRUCT (PKT :ARRAY (:CONSTRUCTOR NIL) (:ALTERANT NIL))
  "For a description of the fields in a PKT see the documentation on the CHAOS Net"
  ((PKT-OPCODE-LEFT-JUSTIFIED NIL)
   (PKT-OPCODE #o1010))
  ((pkt-nbytes-on-write nil)                    ;on write, also store fwd-count zero.
   (PKT-NBYTES #o0014)
   (PKT-FWD-COUNT #o1404))
  ((PKT-DEST-ADDRESS NIL)
   (PKT-DEST-HOST-NUM #o0010)
   (PKT-DEST-SUBNET #o1010))
  PKT-DEST-INDEX-NUM
  ((PKT-SOURCE-ADDRESS NIL)
   (PKT-SOURCE-HOST-NUM #o0010)
   (PKT-SOURCE-SUBNET #o1010))
  PKT-SOURCE-INDEX-NUM
  PKT-NUM
  PKT-ACK-NUM
  PKT-FIRST-DATA-WORD
  PKT-SECOND-DATA-WORD)

(DEFMACRO PKT-NWORDS (PKT)
  `(+ FIRST-DATA-WORD-IN-PKT (LSH (1+ (PKT-NBYTES ,PKT)) -1)))

(DEFMACRO PKT-DEST-CONN (PKT)
  `(AREF INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (PKT-DEST-INDEX-NUM ,PKT))))

(DEFMACRO PKT-SOURCE-CONN (PKT)
  `(AREF INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (PKT-SOURCE-INDEX-NUM ,PKT))))

(DEFSELECT ((:PROPERTY PKT NAMED-STRUCTURE-INVOKE) IGNORE)
  (:PRINT-SELF (PKT STREAM IGNORE &OPTIONAL IGNORE)
    (SYS:PRINTING-RANDOM-OBJECT (PKT STREAM)
      (FORMAT STREAM "CHAOS packet :STRING ~S :STATUS ~S"
              (PKT-STRING PKT) (PKT-STATUS PKT))))
  (:DESCRIBE (PKT)
    (DESCRIBE-DEFSTRUCT PKT 'PKT)
    (DESCRIBE-DEFSTRUCT PKT 'PKT-LEADER)
    (PRINT-PKT PKT)))

;;;; The following macros are for accessing the elements of RECENT-HEADERS
(DEFMACRO RCNT-OPCODE (INDEX) `(LDB #o1010 (AREF RECENT-HEADERS ,INDEX 0)))

(DEFMACRO RCNT-NBYTES (INDEX) `(LDB #o0014 (AREF RECENT-HEADERS ,INDEX 1)))

(DEFMACRO RCNT-FWD-COUNT (INDEX) `(LDB #o1404 (AREF RECENT-HEADERS ,INDEX 1)))

(DEFMACRO RCNT-DEST-ADDRESS (INDEX) `(AREF RECENT-HEADERS ,INDEX 2))

(DEFMACRO RCNT-DEST-INDEX (INDEX) `(AREF RECENT-HEADERS ,INDEX 3))

(DEFMACRO RCNT-SOURCE-ADDRESS (INDEX) `(AREF RECENT-HEADERS ,INDEX 4))

(DEFMACRO RCNT-SOURCE-INDEX (INDEX) `(AREF RECENT-HEADERS ,INDEX 5))

(DEFMACRO RCNT-PKT-NUM (INDEX) `(AREF RECENT-HEADERS ,INDEX 6))

(DEFMACRO RCNT-ACK-NUM (INDEX) `(AREF RECENT-HEADERS ,INDEX 7))

(DEFMACRO RCNT-TIME-RECORDED (INDEX) `(AREF RECENT-HEADERS ,INDEX 8))

;;;; These are routines to print out the preceding structures in a readable form

(DEFUN PRINT-CONN (CONN &OPTIONAL (SHORT-PKT-DISPLAY T) &AUX (LAST NIL))
  (FORMAT T
          "~%Chn: ~O (~O): Contact: ~S State: ~S From: ~O-~O to ~O-~O .~%"
          (LOCAL-INDEX-NUM CONN) (%POINTER CONN)
          (OR (GET (LOCF (CONN-PLIST CONN)) 'RFC-CONTACT-NAME)
              (GET (LOCF (CONN-PLIST CONN)) 'LISTEN-CONTACT-NAME))
          (STATE CONN)
          MY-ADDRESS (LOCAL-INDEX-NUM CONN)
          (FOREIGN-ADDRESS CONN) (FOREIGN-INDEX-NUM CONN))
  (FORMAT T
          " Rcvd #~O, Read #~O, Acked #~O; Sent #~O, Acked #~O.  Windows: ~O, ~O (~O available).~%"
          (PKT-NUM-RECEIVED CONN) (PKT-NUM-READ CONN) (PKT-NUM-ACKED CONN)
          (PKT-NUM-SENT CONN) (SEND-PKT-ACKED CONN)
          (LOCAL-WINDOW-SIZE CONN) (FOREIGN-WINDOW-SIZE CONN) (WINDOW-AVAILABLE CONN))

  (WHEN (SEND-PKTS CONN)
    (FORMAT T " Send pkts:")
    (DO ((PKT (SEND-PKTS CONN) (PKT-LINK PKT))
         (LAST NIL PKT)
         (LEN 0 (1+ LEN)))
        ((NULL PKT)
         (OR (EQ LAST (SEND-PKTS-LAST CONN))
             (FORMAT T
                     "==> SEND-PKTS-LAST IS SCREWED! <==~%"))
         (OR (= LEN (SEND-PKTS-LENGTH CONN))
             (FORMAT T "==> SEND-PKTS-LENGTH IS SCREWED! <==~%")))
      (unless (EQ CONN (PKT-SOURCE-CONN PKT))
        (FORMAT T "~Following PKT has bad PKT-SOURCE-CONN PKT = ~S"
                (PKT-SOURCE-CONN PKT)))
      (PRINT-PKT PKT SHORT-PKT-DISPLAY)))
  (SETQ LAST NIL)
  (WHEN (READ-PKTS CONN)
    (FORMAT T " Read pkts:")
    (DO ((PKT (READ-PKTS CONN) (PKT-LINK PKT))
         (LAST NIL PKT)
         (LEN 0 (1+ LEN)))
        ((NULL PKT)
         (OR (EQ LAST (READ-PKTS-LAST CONN))
             (FORMAT T
                     "==> READ-PKTS-LAST IS SCREWED! <==~%")))
      (PRINT-PKT PKT SHORT-PKT-DISPLAY)))

  (WHEN (RECEIVED-PKTS CONN)
    (FORMAT T " Received pkts:")
    (DO ((PKT (RECEIVED-PKTS CONN) (PKT-LINK PKT)))
        ((NULL PKT))
      (SETQ LAST PKT)
      (PRINT-PKT PKT SHORT-PKT-DISPLAY))))

;;; Print out a packet, if SHORT-DISPLAY is T only 1 line is printed.
(DEFUN PRINT-PKT (PKT &OPTIONAL (SHORT-DISPLAY NIL) INT-PKT-P)
  (TERPRI)
  (AND SHORT-DISPLAY (FORMAT T "   "))
  (FORMAT T "Number: #o~O (#o~O)  Opcode: #o~O (~A).  Number of bytes = #o~O ."
          (PKT-NUM PKT)
          (%POINTER PKT)
          (PKT-OPCODE PKT)
          (COND ((< (PKT-OPCODE PKT) (LENGTH OPCODE-LIST))
                 (NTH (PKT-OPCODE PKT) OPCODE-LIST))
                (( (PKT-OPCODE PKT) DAT-OP) 'DAT)
                (T (FORMAT NIL "==> #o~O <==" (PKT-OPCODE PKT))))
          (PKT-NBYTES PKT))
  (UNLESS SHORT-DISPLAY
    (let* ((from-chaos (PKT-SOURCE-ADDRESS PKT))
           (to-chaos (PKT-DEST-ADDRESS PKT))
           (from-host (cond ((si:get-host-from-address from-chaos :chaos)) (t from-chaos)))
           (to-host (cond ((si:get-host-from-address to-chaos :chaos)) (t to-chaos))))
      (if (instancep from-host) (setq from-host (send from-host :short-name)))
      (if (instancep to-host) (setq to-host (send to-host :short-name)))
      (if (and (stringp to-host) (stringp from-host))   ;hack #o s a bit.
          (FORMAT T "~%From ~O-#o~O to ~O-#o~O .~%"
                  from-host (PKT-SOURCE-INDEX-NUM PKT)
                  to-host (PKT-DEST-INDEX-NUM PKT))
        (FORMAT T "~%From #o~O-~O to #o~O-~O .~%"
                from-host (PKT-SOURCE-INDEX-NUM PKT)
                to-host (PKT-DEST-INDEX-NUM PKT))))
    ;        (FORMAT T "Contents:~S~%   " (PKT-STRING PKT))
    (LET ((MIN-WORDS (MIN 8. (PKT-NWORDS PKT))))
      (DO ((I 0 (1+ I))) (( I min-words))
        (FORMAT T "#o~6,48O~:[,~;~%~]" (AREF PKT I) (= (1+ I) MIN-WORDS))))
    (FORMAT T "Pkt number = #o~O, Ack number = #o~O, Forwarded #o~O times.~%"
            (PKT-NUM PKT) (PKT-ACK-NUM PKT) (PKT-FWD-COUNT PKT))
    (UNLESS INT-PKT-P
      (FORMAT T "Retransmitted ~O times, last at ~S.~%Link = ~S~%"
              (PKT-TIMES-TRANSMITTED PKT) (PKT-TIME-TRANSMITTED PKT)
              (PKT-LINK PKT))))
  NIL)

(DEFUN PRINT-ALL-PKTS (CHAIN &OPTIONAL (SHORT-DISPLAY T))
  (DO ((PKT CHAIN (PKT-LINK PKT))) ((NULL PKT))
    (PRINT-PKT PKT SHORT-DISPLAY)))

;;; MACROS: for various random things

(DEFMACRO PKTNUM-< (A B)
  "Compare two packet numbers, taking wrap-around into account."
   `(BIT-TEST #o100000 (- ,A ,B)))

(DEFMACRO PKTNUM-1+ (A)
  "Increment a packet number, with wrap-around."
   `(LOGAND #o177777 (1+ ,A)))

(DEFUN PKTNUM-- (A B &AUX TEM)
  "Subtract one packet number from another, with wrap-around."
    (SETQ TEM (- A B))
    (IF (< TEM 0)
        (+ TEM #o200000)
        TEM))

;;; Adds a new background task to the queue:  these tasks are ORDERED on a fifo bases
(DEFMACRO BACKGROUND-TASK (TASK)
  `(WITHOUT-INTERRUPTS
     (PUSH ,TASK BACKGROUND-REQUESTS)))

;;;; Initialize all of the data of the NCP routines

;;; Once-only initialization stuff
(DEFUN INITIALIZE-NCP-ONCE ()
  (SETQ *RECEIVE-BROADCAST-PACKETS-P* NIL)
  (SETQ
    ;; Connection list
    PROTOTYPE-CONN (MAKE-CONN)
    free-conn-list nil

    ;; Recent headers
    ;;  Array of #o200 most recent packet transactions each row
    ;;  containing the eight header words of the packet and the
    ;;  time at which the record was made.
    RECENT-HEADERS (zl:MAKE-ARRAY '(#o200 9.) :TYPE 'ART-16B :AREA PERMANENT-STORAGE-AREA)

    ;; Routing table
    ROUTING-TABLE (zl:MAKE-ARRAY ROUTING-TABLE-SIZE :TYPE 'ART-16B :AREA PERMANENT-STORAGE-AREA)
    ROUTING-TABLE-COST (zl:MAKE-ARRAY ROUTING-TABLE-SIZE :TYPE 'ART-16B :AREA PERMANENT-STORAGE-AREA)
    ROUTING-TABLE-TYPE (zl:MAKE-ARRAY routing-table-size :TYPE ART-Q :AREA PERMANENT-STORAGE-AREA)
    )
  ;; Make 8 connections now so they're all on the same page.
  (DOTIMES (I 8)
    (PUSH (MAKE-CONN) FREE-CONN-LIST))
  )

(defconstant amnesia-address #o3412 "Chaosnet address for local host that can't find its own address")

(defun amnesia-p ()
  (or (not (boundp 'my-address))
      (null my-address)
      (= my-address amnesia-address)))

(defun setup-my-address ()
  (let ((old-address (and (boundp 'my-address) my-address)))
    (select-processor
      (:cadr
        (setq my-address (%unibus-read my-number-register)))    ;Full address of this host.
      ((:lambda :explorer)
       (let ((names-for-this-machine (multiple-value-list (si:get-pack-name)))
             my-name)
         (cond ((and (= si:processor-type-code si:lambda-type-code)
                     (variable-boundp si:*my-proc-number*))
                (setq my-name (nth si:*my-proc-number* names-for-this-machine)))
               (t
                (setq my-name (car names-for-this-machine))))
         (without-interrupts
           (setq my-address amnesia-address)    ; Flag that address is bad
           (when (stringp my-name)
             (let ((host (si:parse-host my-name t nil)))
               (when host
                 (setq my-address (send host :network-address :chaos))))))
         )))
    (cond ((eql old-address my-address))        ;Chaos address unchanged
          ((null old-address))                  ;Didn't used to have address but now we do
          ((null my-address)                    ;Used to have an Chaos address but now we don't -- disable Chaos
           (when *chaos-stream*
             (send *chaos-stream* :close)))
          (t
           ;;We used to have an Chaos address and we still do -- but it has changed.  We must change the
           ;;addresses in the Network Interfaces so that ARP will work right
           (dolist (ni net:*network-interfaces*)
             (net:delete-from-alist :chaos (net:ni-address-alist ni))
             (push (list :chaos my-address) (net:ni-address-alist ni)))
           (when *chaos-stream*
             (setf (chaos-addresses *chaos-stream*)
                   (substitute my-address old-address (chaos-addresses *chaos-stream*)))))
          ))
  (setq my-subnet (chaos-subnet-number-from-address my-address))        ;Subnet of this host.
  (let ((existing-host (si:get-host-from-address my-address :chaos)))
    (setq si:local-host
          (if (and existing-host (eq (send existing-host :system-type) :lispm))
              existing-host
            (si:make-unnamed-host :lispm `(:chaos (,my-address))))))
  (when (and (not si:*in-cold-load-p*) (amnesia-p))
    (tv:careful-notify nil t
     "This host does not know its Chaos address; it needs updated site information.~
      ~:[~%Try:  (UPDATE-SITE-CONFIGURATION-INFO).~;~
      ~%But loading site files from the system host will not work, because~
      ~%this host does not control an ethernet interface.~
      ~%Look at the documentation for (SI:SET-PROCESSOR-OWNING-ETHERNET).~]"
      (neq si:*my-op* si:*ethernet-hardware-controller*)))
  si:local-host)

;;; Cold-boot initialization stuff
(DEFUN INITIALIZE-NCP-COLD ()
  ;; Debugging aids which keep records into the past (a short way).
  (SETQ BAD-PKT-LIST NIL        ;List of strings describing packets received in error
        PKTS-BAD-CRC-SAVE-P NIL ;Don't defaultly save packets with bad CRC
        LOS-PKTS NIL            ;LOS PKTs received from the network linked through PKT-LINK.
        MAX-LOS-PKTS-TO-KEEP 16. ;Maximum number of LOS packets to keep on LOS-PKTS
                                ;There may actually be more but they will be used by allocator
        RECENT-HEADERS-POINTER 0
        *RECEIVE-BROADCAST-PACKETS-P* NIL
        *BRD-PKTS-IN* 0
        *BRD-PKTS-OUT* 0
        *BRD-HISTORY* NIL
        *BRD-REPLIES-IN* 0)
  (RESET-METERS)
  (reset-pkts)                                  ;(reset-meters) clobbers pkts-made.
  (SETUP-MY-ADDRESS)
  (RESET-ROUTING-TABLE))

;;;; Low Level PKT Management

;;; PKT MANAGEMENT.

;;; Creates a new pkt.  Only allocates the storage, doesn't initialize anything.
;;; This should only be called by allocate and with interrupts inhibited
;;; Make sure it doesnt happen in a temporary area.
(DEFUN MAKE-PKT (&AUX PKT (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  (SETQ PKT (ZL:MAKE-ARRAY NET:MAX-WORDS-PER-PKT :TYPE 'ART-16B
                           :LEADER-LENGTH PKT-LEADER-SIZE
                           :NAMED-STRUCTURE-SYMBOL 'PKT))
  (SETF (PKT-STRING PKT)                        ;Create indirect array to reference as a string
        (zl:MAKE-ARRAY NET:MAX-DATA-BYTES-PER-PKT
                       ;;:ELEMENT-TYPE 'STRING-CHAR
                       :type 'art-string
                       :FILL-POINTER 0
                       :DISPLACED-TO PKT
                       :DISPLACED-INDEX-OFFSET 16.))
  (SETF (PKT-MADE-LINK PKT) MADE-PKTS)
  (SETQ MADE-PKTS PKT)
  PKT)

(DEFUN ALLOCATE-PKT (&AUX PKT)
  "Allocate, initialize and return a packet, reusing one if possible."
  (WITHOUT-INTERRUPTS
    (SETQ PKT (COND (FREE-PKTS
                      (PROG1 FREE-PKTS
                             (SETQ FREE-PKTS (PKT-LINK FREE-PKTS))))
                    ((> CURRENT-LOS-PKT-COUNT MAX-LOS-PKTS-TO-KEEP)
                     (PROG1 LOS-PKTS
                            (SETQ LOS-PKTS (PKT-LINK LOS-PKTS))
                            (SETQ CURRENT-LOS-PKT-COUNT (1- CURRENT-LOS-PKT-COUNT))))
                    (T (SETQ PKTS-MADE (1+ PKTS-MADE))
                       (MAKE-PKT))))
    (SETF (PKT-TIME-TRANSMITTED PKT) 0)
    (SETF (PKT-TIMES-TRANSMITTED PKT) 0)
    (setf (fill-pointer (PKT-STRING PKT)) 0)
    (SETF (PKT-LINK PKT) T)
    (SETF (PKT-OPCODE-left-justified PKT) 0)
    (SETF (PKT-NBYTES-on-write PKT) 0)
  ; (SETF (PKT-FWD-COUNT PKT) 0)   ;included in above field.
    PKT))

(DEFUN FREE-PKT (PKT)
  "Release the packet PKT so that ALLOCATE-PKT can reuse it.
It is ok to call this while PKT is still awaiting transmission
at interrupt level; it will not really be reused until it has been sent.
NOTE: This is for internal use by the chaosnet ncp ONLY.
User programs should use RETURN-PKT to free packets obtained with
GET-PKT or GET-NEXT-PKT."
  (WITHOUT-INTERRUPTS
   (COND ((NULL (PKT-BEING-RETRANSMITTED PKT))
          (SETF (PKT-LINK PKT) FREE-PKTS)
          (SETQ FREE-PKTS PKT))
         (T (SETF (PKT-BEING-RETRANSMITTED PKT) 'FREE)))))

(DEFUN SET-PKT-STRING (PKT STRING &REST OTHER-STRINGS &AUX LEN)
  "Store data into packet PKT from STRING and OTHER-STRINGS concatenated.
The PKT-NBYTES field is updated."
    (COPY-ARRAY-PORTION (SETQ STRING (STRING STRING)) 0 (ARRAY-ACTIVE-LENGTH STRING)
                        (PKT-STRING PKT) 0 (ARRAY-ACTIVE-LENGTH STRING))
    (SETQ LEN (ARRAY-ACTIVE-LENGTH STRING))
    (when OTHER-STRINGS
      (DO ((STRINGS OTHER-STRINGS (CDR STRINGS))
           (PKT-STRING (PKT-STRING PKT)))
          ((OR (NULL STRINGS) ( LEN MAX-DATA-BYTES-PER-PKT)))
        (DO ((IDX 0 (1+ IDX))
             (STR (STRING (CAR STRINGS)))
             (STR-LEN (STRING-LENGTH (STRING (CAR STRINGS)))))
            ((OR ( IDX STR-LEN) ( LEN MAX-DATA-BYTES-PER-PKT)))
          (ASET (AREF STR IDX) PKT-STRING LEN)
          (SETQ LEN (1+ LEN)))))
    (SETQ LEN (MIN MAX-DATA-BYTES-PER-PKT LEN))
    (SETF (PKT-NBYTES-on-write PKT) LEN)
    (SETF (FILL-POINTER (PKT-STRING PKT)) LEN))

;;Debugging functions for pkts

(defun count-free-pkts ()
  (do ((pkt free-pkts)
       (i 0 (1+ i)))
      ((null pkt)
       (values i pkts-made))
    (setq pkt (pkt-link pkt))))

(defun find-lost-pkts ()
  (do ((pkt made-pkts)
       (result nil))
      ((null pkt) result)
    (do ((free-pkt free-pkts))
        ((null free-pkt)
         (push pkt result))
      (if (eq pkt free-pkt)
          (return)
        (setq free-pkt (pkt-link free-pkt))))
    (setq pkt (pkt-made-link pkt))))

(defun reset-pkts ()
  (setq current-los-pkt-count 0)
  (setq los-pkts nil)
  (setq pkts-made (do ((pkt made-pkts)
                       (i 0 (1+ i)))
                      ((null pkt) i)
                    (setf (pkt-link pkt) (pkt-made-link pkt))
                    (setf (pkt-status pkt) nil)
                    (setf (pkt-being-retransmitted pkt) nil)
                    (setq pkt (pkt-made-link pkt))))
  (setq free-pkts made-pkts))

(defun fixup-pkts ()
  (reset nil)
  (reset-pkts)
  (reset t)
  t)

;;;; INT-PKT management routines

(defun convert-to-pkt (int-pkt &optional (free-pkt-flag t)
                       &aux (pkt (allocate-pkt)) (nw (pkt-nwords int-pkt)))
  "Allocates a new packet, copies the INT-PKT to it, and then deallocates the INT-PKT"
  (select-processor
    ((:lambda :explorer :cadr)
     (without-interrupts
       (%blt (%make-pointer-offset dtp-fix int-pkt (si:array-data-offset int-pkt))
             (%make-pointer-offset dtp-fix pkt (si:array-data-offset pkt))
             (ceiling nw 2)
             1)))
    (:falcon
      (copy-array-portion int-pkt 0 nw pkt 0 nw)))
  (setf (fill-pointer (pkt-string pkt)) (pkt-nbytes int-pkt))
  (and free-pkt-flag (net:free-packet int-pkt))
  pkt)

(defun convert-to-int-pkt (pkt &optional (int-pkt (net:allocate-packet))
                           &aux (nw (pkt-nwords pkt)))
  (select-processor
    ((:lambda :explorer :cadr)
     (without-interrupts
       (%blt (%make-pointer-offset dtp-fix pkt (si:array-data-offset pkt))
             (%make-pointer-offset dtp-fix int-pkt (si:array-data-offset int-pkt))
             (ceiling nw 2)
             1)))
    (:falcon
      (copy-array-portion pkt 0 nw int-pkt 0 nw)))
  (setf (fill-pointer int-pkt) nw)
  int-pkt)

;;;; Low Level CONN Management

;;; CONN MANAGEMENT.

;;; Create a connection.  Returns the connection.
(DEFUN MAKE-CONNECTION ( &OPTIONAL CONN &AUX CONS)
  (WITHOUT-INTERRUPTS
    (COND (CONN)                        ;Caller supplying CONN to be recycled
          ((SETQ CONS FREE-CONN-LIST)   ;Recycle one
           (SETQ FREE-CONN-LIST (CDR CONS)
                 CONN (CAR CONS))
           (COPY-ARRAY-CONTENTS PROTOTYPE-CONN CONN))
          ((SETQ CONN (MAKE-CONN)))))
  (OR (EQ (STATE CONN) 'INACTIVE-STATE)
      (FERROR "Attempt to reuse ~S, which is in the ~A, not INACTIVE-STATE"
              CONN (STATE CONN)))
  (AND (MEMQ CONN FREE-CONN-LIST)
       (FERROR "Attempt to reuse ~S, which was on FREE-CONN-LIST twice (now only once)."
               CONN))
  (AND (MEMQ CONN CONN-LIST)
       (FERROR "Attempt to reuse ~S, which is already in use." CONN))
  (DO ((FP (rem (1+ INDEX-CONN-FREE-POINTER) MAXIMUM-INDEX) (rem (1+ FP) MAXIMUM-INDEX))
       (COUNTER MAXIMUM-INDEX (1- COUNTER)))
      ((%STORE-CONDITIONAL (AP-1 INDEX-CONN (IF (= FP 0) (SETQ FP 1) FP))
                           NIL
                           CONN)
       (SETQ INDEX-CONN-FREE-POINTER FP)
       (SETF (LOCAL-INDEX-NUM CONN) (DPB (ASET (1+ (AR-1 UNIQUIZER-TABLE FP))
                                               UNIQUIZER-TABLE FP)
                                         (DPB MAXIMUM-INDEX-LOG-2-MINUS-1
                                              #o0606
                                              (- #o20 MAXIMUM-INDEX-LOG-2-MINUS-1))
                                         FP))
       (SETF (CONN-RETRANSMISSION-INTERVAL CONN) RETRANSMISSION-INTERVAL)
       (WITHOUT-INTERRUPTS
         (SETQ CONN-LIST (RPLACD (OR CONS (NCONS CONN)) CONN-LIST)))
         CONN)
    (AND (MINUSP COUNTER)
         (FERROR 'SYS:NETWORK-RESOURCES-EXHAUSTED "Connection table full"))))

(DEFUN REMOVE-CONN (CONN)
  "Remove connection-object CONN from the connection tables and free its packets."
  (WITHOUT-INTERRUPTS
    (FREE-ALL-READ-PKTS CONN)
    (FREE-ALL-RECEIVED-PKTS CONN)
    (FREE-ALL-SEND-PKTS CONN)
    (SETF (STATE CONN) 'INACTIVE-STATE)
    (AS-1 NIL INDEX-CONN (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (LOCAL-INDEX-NUM CONN)))
    (SETQ DISTINGUISHED-PORT-CONN-TABLE
          (DELQ (RASSQ CONN DISTINGUISHED-PORT-CONN-TABLE) DISTINGUISHED-PORT-CONN-TABLE))
    (LET ((CONS (MEMQ CONN CONN-LIST)))
      (SETQ CONN-LIST (DELQ CONN CONN-LIST))
      (OR (MEMQ CONN FREE-CONN-LIST)
          (SETQ FREE-CONN-LIST (RPLACD (OR CONS (NCONS CONN)) FREE-CONN-LIST))))
    (DOLIST (X PENDING-LISTENS)
      (AND (EQ (CDR X) CONN) (SETQ PENDING-LISTENS (DELQ X PENDING-LISTENS))))
    NIL))

;;; Must be called with interrupts off.
(DEFUN FREE-ALL-READ-PKTS (CONN)
    (DO ((PKT (READ-PKTS CONN) (PKT-LINK PKT))
         (PREV NIL PKT))
        (NIL)
      (AND PREV (FREE-PKT PREV))
      (OR PKT (RETURN NIL)))
    (SETF (READ-PKTS CONN) NIL)
    (SETF (READ-PKTS-LAST CONN) NIL))

;;; Must be called with interrupts off.
(DEFUN FREE-ALL-RECEIVED-PKTS (CONN)
    (DO ((PKT (RECEIVED-PKTS CONN) (PKT-LINK PKT))
         (PREV NIL PKT))
        (NIL)
      (AND PREV (FREE-PKT PREV))
      (OR PKT (RETURN NIL)))
    (SETF (RECEIVED-PKTS CONN) NIL))

;;; Must be called with interrupts off.
(DEFUN FREE-ALL-SEND-PKTS (CONN)
  (DO ((PKT (SEND-PKTS CONN) (PKT-LINK PKT))
       (PREV NIL PKT))
      (NIL)
    (AND PREV (FREE-PKT PREV))                  ;This offseting so doesn't rely on PKT-LINK of
    (OR PKT (RETURN NIL)))                      ; a PKT it has freed.
  (SETF (SEND-PKTS CONN) NIL)
  (SETF (SEND-PKTS-LAST CONN) NIL)
  (SETF (SEND-PKTS-LENGTH CONN) 0))

(DEFUN INTERRUPT-CONN (REASON CONN &REST ARGS &AUX (IFUN (INTERRUPT-FUNCTION CONN)))
  "Causes the CONN's INTERRUPT-FUNCTION to be run in the background process with the
specified reason and arguments
 Reasons are:
        :INPUT                  input has arrived
        :OUTPUT                 the window, which was full, now has room in it
        :CHANGE-OF-STATE        the state of the connection has just changed"
  (AND IFUN
       (BACKGROUND-TASK `(INTERRUPT-CONN-INTERNAL ',IFUN ',REASON ',CONN
                                                  ',(APPEND ARGS NIL)))))

;;; If while the request was on the queue, the connection was flushed, get rid
;;; of the interrupt.  Because of connection reusing, this is somewhat heuristic.
(DEFUN INTERRUPT-CONN-INTERNAL (IFUN REASON CONN ARGS)
  (OR (EQ (STATE CONN) 'INACTIVE-STATE)
      (NEQ (INTERRUPT-FUNCTION CONN) IFUN)
      (LEXPR-FUNCALL IFUN REASON CONN ARGS)))

;;;; High Level Transmission Routines

;;; These are the routines which cause a packet to be queued for transmission.

(DEFUN TRANSMIT-PKT (PKT &OPTIONAL ACK-P)
  "Put the pkt on the transmit list, and create a phony transmitter interrupt
  if needed so that the interrupt level will start sending.
If the second arg is T, put an ACK aboard this PKT.
This is a very low level function, called mainly by the following 2 functions.
/(Also called by the retransmitter and forwarder.)"
    (AND (> (PKT-NBYTES PKT) MAX-DATA-BYTES-PER-PKT)
         (FERROR "Attempt to transmit an invalid packet (~S).~%   ~
                        The length ~O is greater than the maximum packet size (~O)."
                 PKT (PKT-NBYTES PKT) MAX-DATA-BYTES-PER-PKT))
    (WHEN (AND ACK-P (NOT (= (PKT-OPCODE PKT) RFC-OP)))
      (WITHOUT-INTERRUPTS
        (LET ((CONN (PKT-SOURCE-CONN PKT)))
          (OR CONN (FERROR "~S has null connection." PKT))
          (LET ((ACKN (PKT-NUM-READ CONN)))
            (SETF (PKT-ACK-NUM PKT) ACKN)
            (SETF (PKT-NUM-ACKED CONN) ACKN)))))
    (SETF (PKT-TIME-TRANSMITTED PKT) (zl:TIME))
    (SETF (PKT-TIMES-TRANSMITTED PKT) (1+ (PKT-TIMES-TRANSMITTED PKT)))
    (TRANSMIT-INT-PKT (CONVERT-TO-INT-PKT PKT)) )

(DEFUN TRANSMIT-INT-PKT-FOR-CONN (CONN PKT)
    (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
    (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
    (SETF (PKT-DEST-ADDRESS PKT) (FOREIGN-ADDRESS CONN))
    (SETF (PKT-DEST-INDEX-NUM PKT) (LDB #o0020 (FOREIGN-INDEX-NUM CONN)))
    (WITHOUT-INTERRUPTS
      (LET ((ACKN (PKT-NUM-READ CONN)))
        (SETF (PKT-ACK-NUM PKT) ACKN)
        (SETF (PKT-NUM-ACKED CONN) ACKN)))
    (TRANSMIT-INT-PKT PKT))

(DEFUN TRANSMIT-LOS-INT-PKT (INT-PKT OP &OPTIONAL REASON &AUX DH DI LEN)
  "Given a losing pkt or an RFC we want to reject, shuffle the
pkt and return it.  Caller must specify opcode, either LOS or CLS.
If the OP is CLS, include a string which is the reason the RFC was
rejected.  Note that the very same pkt is used, so when this is called
the pkt had better not be on any lists or anything."
  (SETF (PKT-OPCODE INT-PKT) OP)
  (COND (REASON
          (SETF (PKT-NBYTES-on-write INT-PKT) (SETQ LEN (ARRAY-ACTIVE-LENGTH REASON)))
          (DO ((SIDX 0 (+ SIDX 2))
               (WIDX FIRST-DATA-WORD-IN-PKT (1+ WIDX)))
              (( SIDX LEN))
            (ASET (IF (= (1+ SIDX) LEN)
                      (AREF REASON SIDX)
                      (DPB (AREF REASON (1+ SIDX)) (byte 8 8) (AREF REASON SIDX)))
                  INT-PKT WIDX))))
  (SETQ DH (PKT-DEST-ADDRESS INT-PKT)
        DI (PKT-DEST-INDEX-NUM INT-PKT))
  (SETF (PKT-DEST-ADDRESS INT-PKT) (PKT-SOURCE-ADDRESS INT-PKT))
  (SETF (PKT-DEST-INDEX-NUM INT-PKT) (PKT-SOURCE-INDEX-NUM INT-PKT))
  (SETF (PKT-SOURCE-ADDRESS INT-PKT) DH)
  (SETF (PKT-SOURCE-INDEX-NUM INT-PKT) DI)
  (TRANSMIT-INT-PKT INT-PKT))

(DEFUN TRANSMIT-NORMAL-PKT (CONN PKT &OPTIONAL (PKTN 0) (ACK-PKTN 0) &AUX ACK-P)
  "Send a normal pkt (i.e., not LOS nor DATA)
Caller must allocate the pkt, and fill in the opcode, nbytes, and data parts.
The PKT and ACK-PKT numbers to place in the packet are optional arguments,
they default to 0.  If T is provided for either, the usual thing happens."
  (WHEN (EQ PKTN T)
    (SETQ PKTN (PKTNUM-1+ (PKT-NUM-SENT CONN)))
    (SETF (PKT-NUM-SENT CONN) PKTN))
  (SETF (PKT-NUM PKT) PKTN)
  (COND ((EQ ACK-PKTN T) (SETQ ACK-P T))
        (T (SETF (PKT-ACK-NUM PKT) ACK-PKTN)))
  (SETF (PKT-SOURCE-ADDRESS PKT) MY-ADDRESS)
  (SETF (PKT-SOURCE-INDEX-NUM PKT) (LOCAL-INDEX-NUM CONN))
  (SETF (PKT-DEST-ADDRESS PKT) (FOREIGN-ADDRESS CONN))
  (SETF (PKT-DEST-INDEX-NUM PKT) (LDB (byte 16. 0) (FOREIGN-INDEX-NUM CONN)))
  (TRANSMIT-PKT PKT ACK-P))

(DEFVAR STS-WHY-ARRAY (zl:MAKE-ARRAY #o100 :LEADER-LIST '(#o100 0)))

(DEFUN PRINT-STS-WHY ()
  (LET ((N (ARRAY-LEADER STS-WHY-ARRAY 1)))
    (DO ((I (rem (1+ N) #o100) (rem (1+ I) #o100)))
        (NIL)
      (PRINT (AREF STS-WHY-ARRAY I))
      (AND (= I N) (RETURN NIL)))))

;;; Internal routine to send a status packet to a connection.
(DEFUN TRANSMIT-STS (CONN WHY &OPTIONAL PKT)
  "transmit a STS message.  If passed PKT argument, it is an int-pkt to use"
  (SETF (AREF STS-WHY-ARRAY (ARRAY-LEADER STS-WHY-ARRAY 1)) WHY)
  (SETF (ARRAY-LEADER STS-WHY-ARRAY 1) (REM (1+ (ARRAY-LEADER STS-WHY-ARRAY 1)) #O100))
  (unless pkt
    (setq pkt (net:allocate-packet)))
  (SETF (PKT-OPCODE PKT) STS-OP)
  (SETF (PKT-NBYTES-ON-WRITE PKT) 4)
  (SETF (PKT-FIRST-DATA-WORD PKT) (PKT-NUM-RECEIVED CONN))
  (SETF (PKT-SECOND-DATA-WORD PKT) (LOCAL-WINDOW-SIZE CONN))
  (TRANSMIT-INT-PKT-FOR-CONN CONN PKT))

;;;; Output-Main Program level

(DEFUN RELEASE-PKT (PKT)
  "Release a packet to a routine outside the NCP.
This routine should be called whenever returning a packet as a value
to a caller which is outside the NCP"
  (COND ((NULL (PKT-STATUS PKT))
         (SETF (PKT-STATUS PKT) 'RELEASED)
         (SETF (PKT-LINK PKT) NIL))
        (T (FERROR "Attempt to release ~S, which is already released." PKT))))

;;; To send a PKT, first call GET-PKT to give you a pkt, fill it full of
;;; cruft and set its NBYTES, and then call SEND-PKT on it.

(DEFUN GET-PKT ( &AUX PKT)
  "Allocate and return a \"released\" packet.
A released packet is one which can be in use outside the chaosnet ncp itself.
This is the proper way for a progam which uses the chaosnet to get a packet.
When you are finished with it, call RETURN-PKT to allow it to be reused."
  (SETQ PKT (ALLOCATE-PKT))
  (RELEASE-PKT PKT)
  PKT)

;;; CONN must be in OPEN-STATE, and the OPCODE must be a DAT opcode.
(DEFUN SEND-PKT (CONN PKT &OPTIONAL (OPCODE DAT-OP))
  "Send the data packet PKT to connection CONN.  OPCODE specifies the type of packet;
The default is DAT (opcode 200).  PKT is returned to the chaosnet ncp
and should not be used further by the caller.
The value is the packet number assigned to the packet."
  (SELECTQ (STATE CONN)
    (OPEN-STATE
      (OR (BIT-TEST DAT-OP OPCODE) (= EOF-OP OPCODE)
          (FERROR "~O is not a legal opcode." OPCODE))
      (PROCESS-WAIT "Chaosnet Output"
                    (FUNCTION (LAMBDA (X) (OR (MAY-TRANSMIT X)
                                              (NEQ (STATE X) 'OPEN-STATE))))
                    CONN)
      (COND ((EQ (STATE CONN) 'OPEN-STATE)
             (OR (EQ (PKT-STATUS PKT) 'RELEASED)
                 (FERROR "Attempt to transmit ~S, which is not released." PKT))
             (SETF (PKT-STATUS PKT) NIL)
             (SETF (PKT-OPCODE PKT) OPCODE)
             (SETF (WINDOW-AVAILABLE CONN) (1- (WINDOW-AVAILABLE CONN)))
             (TRANSMIT-NORMAL-PKT CONN PKT T T)   ;And send it for the first time.
             (WITHOUT-INTERRUPTS                  ;Must do the transmit before putting it
               (LET ((LAST (SEND-PKTS-LAST CONN)))  ;in SEND-PKTS because TRANSMIT-NORMAL-PKT
                 (COND (LAST (SETF (PKT-LINK LAST) PKT)) ;fills in lots of fields.
                       (T (SETF (SEND-PKTS CONN) PKT)))
                 (SETF (SEND-PKTS-LAST CONN) PKT)
                 (SETF (PKT-LINK PKT) NIL)
                 (SETF (SEND-PKTS-LENGTH CONN) (1+ (SEND-PKTS-LENGTH CONN)))
                 (SETQ RETRANSMISSION-NEEDED T))
               (PKT-NUM PKT)))
            (T (REPORT-BAD-CONNECTION-STATE CONN "transmit on"))))
    (T (REPORT-BAD-CONNECTION-STATE CONN "transmit on"))))

(DEFUN SEND-STRING (CONN &REST STRINGS &AUX PKT)
  "Send data made by concatenating STRINGS down connection CONN."
  (SETQ PKT (GET-PKT))
  (APPLY 'SET-PKT-STRING PKT STRINGS)
  (SEND-PKT CONN PKT))

;;; User level routine to transmit an uncontrolled packet.
(DEFUN SEND-UNC-PKT (CONN PKT
                     &OPTIONAL (PKTN-FIELD (PKT-NUM PKT)) (ACK-FIELD (PKT-ACK-NUM PKT)))
  "Send a user-handled uncontrolled packet PKT down connection CONN.
The opcode field of PKT should be set up by the caller."
  (SETF (PKT-OPCODE PKT) UNC-OP)
  (TRANSMIT-NORMAL-PKT CONN PKT PKTN-FIELD ACK-FIELD))

(DEFUN MAY-TRANSMIT (CONN)
  "T if more packets can be sent down CONN without filling up the transmit window.
Packets awaiting transmission count against the limits."
  (> (WINDOW-AVAILABLE CONN) 0))

(DEFUN DATA-AVAILABLE (CONN)
  "T if input data is available on connection CONN."
  (NOT (NULL (READ-PKTS CONN))))

(DEFUN FINISH-CONN (CONN &OPTIONAL (WHOSTATE "Chaosnet Finish"))
  "Wait until all packets awaiting transmission on CONN have been sent and acknowledged.
Also returns if the connection is closed.
Returns T if the connection is still open."
  (PROCESS-WAIT WHOSTATE 'CONN-FINISHED-P CONN)
  (EQ (STATE CONN) 'OPEN-STATE))
(DEFF FINISH 'FINISH-CONN)
(MAKE-OBSOLETE FINISH "use CHAOS:FINISH-CONN")

(DEFUN CONN-FINISHED-P (CONN)
  "T unless connection is open but not all our transmissions have been acknowledged."
  (OR (null (send-pkts conn))
     ;( (WINDOW-AVAILABLE CONN) (FOREIGN-WINDOW-SIZE CONN))
      (NEQ (STATE CONN) 'OPEN-STATE)))
(DEFF FINISHED-P 'CONN-FINISHED-P)
(MAKE-OBSOLETE FINISHED-P "use CHAOS:CONN-FINISHED-P")

;;;; Input-Main Program level

(DEFUN GET-NEXT-PKT (CONN &OPTIONAL (NO-HANG-P NIL) (WHOSTATE "Chaosnet Input")
                     (CHECK-CONN-STATE (NOT NO-HANG-P))
                     &AUX PKT)
  "Return the next input packet from connection CONN.
The packet may contain data, or it may be a CLS, ANS or UNC.
If the next input packet is not are available,
 either wait or return NIL according to NO-HANG-P.
WHOSTATE is what to put in the who-line while we wait, if we wait.
CHECK-CONN-STATE non-NIL says get an error now if connection is in
 an invalid state.  Default is T unless NO-HANG-P.
When you are finished with the data in the packet, use RETURN-PKT
to allow the chaosnet ncp to reuse the packet."
  ;; Loop until we get a packet, decide not to hang, or error out
  (DO-FOREVER
    ;; Check for connection in an erroneous state
    (AND CHECK-CONN-STATE
         (OR (MEMQ (STATE CONN) '(OPEN-STATE RFC-RECEIVED-STATE ANSWERED-STATE FOREIGN-STATE))
             (IF (EQ (STATE CONN) 'CLS-RECEIVED-STATE)
                 (UNLESS (READ-PKTS CONN)
                   (FERROR 'SYS:CONNECTION-NO-MORE-DATA
                           "~Attempt to receive from ~S,~%~
                                a connection which has been closed by foreign host.~"
                           CONN))
               (REPORT-BAD-CONNECTION-STATE CONN "receive from"))))
    ;; Now see if there are any packets we can have
    (WITHOUT-INTERRUPTS
      (SETQ PKT (READ-PKTS CONN))
      (COND (PKT                                ;Got packet, take off of read list
             (AND ( UNC-OP (PKT-OPCODE PKT))
                  (SETF (PKT-NUM-READ CONN) (PKT-NUM PKT)))
             (SETF (READ-PKTS CONN) (PKT-LINK PKT))
             (COND ((NULL (READ-PKTS CONN))
                    (SETF (READ-PKTS-LAST CONN) NIL))))))
    (AND (NOT (NULL PKT))                       ;Got packet, acknowledge if necessary
         (EQ (STATE CONN) 'OPEN-STATE)
         ( (* 3 (PKTNUM-- (PKT-NUM PKT) (PKT-NUM-ACKED CONN))) (LOCAL-WINDOW-SIZE CONN))
         (TRANSMIT-STS CONN 'WINDOW-FULL))
    (AND PKT                                    ;Got packet, release from NCP
         (RELEASE-PKT PKT))
    (AND (OR PKT NO-HANG-P) (RETURN PKT))       ;If satisfied, return
    ;; Not satisfied, wait for something interesting to happen
    (PROCESS-WAIT WHOSTATE
                  #'(LAMBDA (X) (OR (READ-PKTS X)
                                    (NOT (MEMQ (STATE X) '(OPEN-STATE FOREIGN-STATE)))))
                  CONN)))

(DEFUN REPORT-BAD-CONNECTION-STATE (CONN OPERATION-STRING)
  (declare (dbg:error-reporter))
  (COND ((EQ (STATE CONN) 'HOST-DOWN-STATE)
         (FERROR 'SYS:HOST-STOPPED-RESPONDING
                 "Attempt to ~1@*~A ~0@*~S,
a connection whose foreign host died."
                 CONN OPERATION-STRING))
        ((EQ (STATE CONN) 'LOS-RECEIVED-STATE)
         (FERROR 'SYS:CONNECTION-LOST
                 "Attempt to ~2@*~A ~0@*~S,
which got a LOS: ~A"
                 CONN
                 (IF (READ-PKTS-LAST CONN)
                     (PKT-STRING (READ-PKTS-LAST CONN))
                   "??")
                 OPERATION-STRING))
        ((EQ (STATE CONN) 'CLS-RECEIVED-STATE)
         (FERROR 'SYS:CONNECTION-CLOSED
                 "Attempt to ~1@*~A ~0@*~S,
a connection which has been closed by foreign host."
                 CONN OPERATION-STRING))
        (T
         (FERROR 'SYS:BAD-CONNECTION-STATE-1
                 "Attempt to ~2@*~A ~0@*~S,
which is in ~S, not a valid state"
                 CONN (STATE CONN) OPERATION-STRING))))

(DEFUN RETURN-PKT (PKT)
  "Tell the chaosnet ncp you are finished with packet PKT.
PKT should be a \"released\" packet, obtained with GET-PKT or GET-NEXT-PKT."
  (CASE (PKT-STATUS PKT)
    (RELEASED
     (SETF (PKT-STATUS PKT) NIL)
     (FREE-PKT PKT))
    (T
     (FERROR "Attempt to return unreleased packet (~S) to the NCP." PKT))))

;;;; Receiver Interrupt level

;;; RECEIVER FUNCTIONS:  These run at receiver interrupt level.

;;; This function is called on an INT-PKT which has just come in from the net.
;;; It is mostly a transfer vector to more specialized functions, but it also
;;; does error checking.
;;; Note: Functions reached from here must not, in any case, go blocked.
;;;  A common case is to call functions which ordinarily could go blocked
;;;  waiting for a INT-PKT.  Routines TRANSMIT-STS and CONVERT-TO-INT-PKT
;;;  have optional arguments to allow you to use the INT-PKT just received.

(defun set-word-count (int-pkt)
  (let ((proposed-word-count (pkt-nwords int-pkt)))
    (cond ((< proposed-word-count first-data-word-in-pkt)
           nil)
          ((> proposed-word-count net:max-words-per-pkt)
           nil)
          (t
           (setf (fill-pointer int-pkt) proposed-word-count)
           t))))

(defun receive-chaos-pkt (int-pkt interface stream source destination broadcast-p)
  (declare (ignore stream))
  (declare (ignore destination))
  (incf pkts-received)                          ;Increment counter looked at by Hostat
  (let* ((packet (net:original-array int-pkt))
         (header (net:ni-rcvd-header-length interface))
         (trailer (net:ni-rcvd-trailer-length interface))
         (total-words (/ (+ header trailer) 2)))
    (unless (zerop header)
      (setq int-pkt (make-array (- (array-length packet) total-words)
                                :element-type '(unsigned-byte 16)
                                :displaced-to packet
                                :displaced-index-offset (/ header 2)
                                :fill-pointer (- (fill-pointer int-pkt) total-words)))))
  (cond ((set-word-count int-pkt)
         (and (/= (pkt-dest-address int-pkt) my-address)        ;Packet to be forwarded
              (eq interface si:share-interface)                 ;And came from other processor
              (let ((elt (assoc source net:*processor-forwarding-alist*)))
                (when elt
                  (setf (cdr elt) t))))         ;Note that we are that processor's gateway
         (without-interrupts
           (receive-int-pkt int-pkt broadcast-p)))
        (t
         ;;packet appears to be a loser.
         (net:free-packet int-pkt))))

(defvar *header-version-action* :RESPECT
  "header version feature was never used, so it is sometimes good to ignore it
when trying to talk to machines with slightly broken chaos software.
Value can be :IGNORE, :NOTIFY, :RESPECT")

(DEFUN RECEIVE-INT-PKT (INT-PKT &optional broadcast-p &AUX (OP (PKT-OPCODE INT-PKT)) CONN ACKN)
  (COND ((and (NOT (ZEROP (LDB (byte 8 0) (AREF INT-PKT 0))))
              (memq *header-version-action* '(:NOTIFY :RESPECT)))
         (if (eq *header-version-action* :notify)
             (tv:notify nil "Got non-zero header version from address: #o~O"
                        (pkt-source-address int-pkt)))
         (NET:FREE-PACKET INT-PKT))
        ((= OP RUT-OP)
         (DO ((I FIRST-DATA-WORD-IN-PKT (+ I 2))
              (N (floor (PKT-NBYTES INT-PKT) 4) (1- N))
              (GATEWAY (PKT-SOURCE-ADDRESS INT-PKT))
              (N-SUBNETS (ARRAY-LENGTH ROUTING-TABLE))
              (SUBNET) (COST))
             ((ZEROP N) (NET:FREE-PACKET INT-PKT))
           (SETQ SUBNET (AREF INT-PKT I) COST (AREF INT-PKT (1+ I)))
           (WHEN (AND (< SUBNET N-SUBNETS)
                      ( COST (AREF ROUTING-TABLE-COST SUBNET))
                      (NULL (AREF ROUTING-TABLE-TYPE SUBNET)))
             (SETF (AREF ROUTING-TABLE SUBNET) GATEWAY)
             (SETF (AREF ROUTING-TABLE-COST SUBNET) COST))))
        ((AND (= OP BRD-OP) (ZEROP (PKT-DEST-ADDRESS INT-PKT)))
         (RECEIVE-BRD INT-PKT))
        ((/= (PKT-DEST-ADDRESS INT-PKT) MY-ADDRESS)     ;Packet to be forwarded
         (COND ((neq si:*ethernet-hardware-controller* si:*my-op*)
                ;;If we don't talk directly to network, don't forward.  If we are slot 4 and don't yet
                ;;know who we are, whereas slot 0 does know, we will receive packets that we (improperly)
                ;;believe are not for us.  Drop them, don't bounce them back to slot 0.
                (net:free-packet int-pkt))
               (broadcast-p
                (net:free-packet int-pkt))
               ((OR (= (PKT-FWD-COUNT INT-PKT) 17)
                    (> (PKT-NBYTES INT-PKT) MAX-DATA-BYTES-PER-PKT))
                (NET:FREE-PACKET INT-PKT)
                (INCF PKTS-OVER-FORWARDED))
               (T (SETF (PKT-FWD-COUNT INT-PKT) (1+ (PKT-FWD-COUNT INT-PKT)))
                  (INCF PKTS-FORWARDED)
                  (TRANSMIT-INT-PKT INT-PKT nil nil nil))))
        (T (RECORD-INT-PKT-HEADER INT-PKT)
           (AND (BIT-TEST #o200 OP) (INCF DATA-PKTS-IN))
           (COND
             ((= OP RFC-OP) (RECEIVE-RFC INT-PKT))
             ((= OP LOS-OP) (RECEIVE-LOS INT-PKT))
             ((= OP CLS-OP) (RECEIVE-CLS INT-PKT))
             ((= OP MNT-OP) (NET:FREE-PACKET INT-PKT))
             ((AND (OR (NULL (SETQ CONN (PKT-DEST-CONN INT-PKT)))
                       (/= (PKT-DEST-INDEX-NUM INT-PKT) (LOCAL-INDEX-NUM CONN))
                       (/= (PKT-SOURCE-ADDRESS INT-PKT) (FOREIGN-ADDRESS CONN)))
                   (NOT (SETQ CONN (CDR (ASSQ (PKT-DEST-INDEX-NUM INT-PKT)
                                              DISTINGUISHED-PORT-CONN-TABLE)))))
              (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP
                                    (IF CONN "You are not connected to this index"
                                      "No such index exists")))
             ((PROG2 (SETF (TIME-LAST-RECEIVED CONN) (ZL:TIME))
                     (= OP OPN-OP))
              (RECEIVE-OPN CONN INT-PKT))
             ((= OP FWD-OP) (RECEIVE-FWD CONN INT-PKT))
             ((= OP ANS-OP) (RECEIVE-ANS CONN INT-PKT))
             ((= OP UNC-OP) (RECEIVE-UNC CONN INT-PKT))
             ((NOT (OR (= OP SNS-OP) (= OP STS-OP)
                       (= OP EOF-OP) ( OP DAT-OP)))
              (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Illegal opcode"))
             ((NOT (= (PKT-SOURCE-INDEX-NUM INT-PKT) (FOREIGN-INDEX-NUM CONN)))
              (IF (= OP SNS-OP) (NET:FREE-PACKET INT-PKT)       ;Ignore SNS if not open
                (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP
                                      "That is not your index number for this connection")))
             ;; Below here can be SNS, STS, EOF, or DAT, all packets having ack fields.
             ((NOT (EQ (STATE CONN) 'OPEN-STATE))
              (IF (= OP SNS-OP) (NET:FREE-PACKET INT-PKT)       ;Ignore SNS if not open
                (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Connection not open")))
             (T
              ;; Below here, this INT-PKT contains a normal acknowledgement field.
              (SETQ ACKN (PKT-ACK-NUM INT-PKT)) ;Acknowledgement field
              (RECEIPT CONN ACKN)               ;Clear receipted packets from send list
              (COND ((OR (>= OP DAT-OP) (= OP EOF-OP))
                     (RECEIVE-EOF-OR-DAT CONN INT-PKT))
                    ((= OP SNS-OP) (RECEIVE-SNS CONN INT-PKT))
                    ((= OP STS-OP) (RECEIVE-STS CONN INT-PKT))
                    (T (FERROR NIL "should not get here"))))))))

(DEFUN RECORD-INT-PKT-HEADER (INT-PKT)
  (DO ((I 0 (1+ I))) ((= I 8.))
    (ASET (AR-1 INT-PKT I) RECENT-HEADERS RECENT-HEADERS-POINTER I))
  (ASET (zl:TIME) RECENT-HEADERS RECENT-HEADERS-POINTER 8.)
  (SETQ RECENT-HEADERS-POINTER (rem (1+ RECENT-HEADERS-POINTER) #o200)))

;;; Discard packets from send-list which have been receipted by other end
(DEFUN RECEIPT (CONN ACK-LEV &optional (REC-LEV ACK-LEV))
  (WITHOUT-INTERRUPTS
    (LET ((SENDS (SEND-PKTS CONN))              ;(Save array references...)
          (LENGTH (SEND-PKTS-LENGTH CONN)))
      (DO ((PKT SENDS SENDS))                   ;For each PKT the destination has received...
          ((OR (NULL PKT) (PKTNUM-< REC-LEV (PKT-NUM PKT))))
        (SETQ SENDS (PKT-LINK PKT))
        (FREE-PKT PKT)
        (DECF LENGTH))

      (SETF (SEND-PKTS CONN) SENDS)
      (SETF (SEND-PKTS-LENGTH CONN) LENGTH)
      (UNLESS SENDS
        (SETF (SEND-PKTS-LAST CONN) NIL)))
    (WHEN (PKTNUM-< (SEND-PKT-ACKED CONN) ACK-LEV)
      (SETF (SEND-PKT-ACKED CONN) ACK-LEV))
    (UPDATE-WINDOW-AVAILABLE CONN)))

;;; A new ack has come in, so adjust the amount left in the window.  If the window was
;;; full, and has now become "un-full", cause an output interrupt
(DEFUN UPDATE-WINDOW-AVAILABLE (CONN &AUX (AVAILABLE (WINDOW-AVAILABLE CONN)))
  (SETF (WINDOW-AVAILABLE CONN)
        (MAX AVAILABLE                  ;in case rcvd out of order
             (- (FOREIGN-WINDOW-SIZE CONN)
                (PKTNUM-- (PKT-NUM-SENT CONN) (SEND-PKT-ACKED CONN)))))
  (AND (ZEROP AVAILABLE) (NOT (ZEROP (WINDOW-AVAILABLE CONN)))
       (INTERRUPT-CONN :OUTPUT CONN)))

;;; The following functions are called to process the receipt of a particular kind of packet.

(DEFUN RECEIVE-STS (CONN INT-PKT)
  (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-SECOND-DATA-WORD INT-PKT))
  (RECEIPT CONN (PKT-ACK-NUM INT-PKT) (PKT-FIRST-DATA-WORD INT-PKT))
  (NET:FREE-PACKET INT-PKT))

;;; When this is called, CONN is known to be in OPEN-STATE.
(DEFUN RECEIVE-SNS (CONN INT-PKT)
  (TRANSMIT-STS CONN 'SNS int-pkt))

;;; This uses the PKT-NUM to correctly order the PKTs.
;;;     If the PKT-NUM is less than or equal to the highest we received
;;;          ignore it and send a receipt
;;;     If one higher then this one is added to the end of the successfully recieved PKTs.
;;;          Then the out of sequence list is appended to the insequence list and the
;;;             point of break in sequence is found whereupon the list is broken
;;;             and all the appropriate pointers are set up.
;;;     If more than one larger ry locating it's position in the out of order list
;;; When this is called, CONN is known to be in OPEN-STATE.
(DEFUN RECEIVE-EOF-OR-DAT (CONN INT-PKT &AUX PKT PKT-NUM PKTL-NUM PREV)
  (SETQ PKT-NUM (PKT-NUM INT-PKT))
  (COND ((NOT (PKTNUM-< (PKT-NUM-RECEIVED CONN) PKT-NUM))
         (SETQ PKTS-DUPLICATED (1+ PKTS-DUPLICATED))
         (TRANSMIT-STS CONN '<-NUM-RCVD int-pkt))       ;This is a duplicate, receipt and ignore
        ((= PKT-NUM (PKTNUM-1+ (PKT-NUM-RECEIVED CONN)))
         ;; This is the one we were waiting for add it to READ-PKTS
         (SETQ PKT (CONVERT-TO-PKT INT-PKT))
         (AND (NULL (READ-PKTS CONN))
              (INTERRUPT-CONN :INPUT CONN))
         (WITHOUT-INTERRUPTS
           (SETF (PKT-LINK PKT) (RECEIVED-PKTS CONN))   ;Link the two lists together
           (SETF (RECEIVED-PKTS CONN) NIL)
           (COND ((NULL (READ-PKTS-LAST CONN))
                  (SETF (READ-PKTS CONN) PKT))
                 (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
           (DO ((PKTL-NUM (PKT-NUM PKT)
                          (PKTNUM-1+ PKTL-NUM)))
               ((OR (NULL PKT)
                    ( PKTL-NUM (PKT-NUM PKT)))
                (SETF (PKT-NUM-RECEIVED CONN) (PKTNUM-- PKTL-NUM 1))
                (SETF (RECEIVED-PKTS CONN) PKT)
                (AND PREV (SETF (PKT-LINK PREV) NIL))
                (SETF (READ-PKTS-LAST CONN) PREV))
             (SETQ PREV PKT)
             (SETQ PKT (PKT-LINK PKT)))))
        (T (WITHOUT-INTERRUPTS
             (DO ((PKTL (RECEIVED-PKTS CONN) (PKT-LINK PKTL))
                  (PREV NIL PKTL))
                 ((NULL PKTL)
                  (SETQ PKT (CONVERT-TO-PKT INT-PKT))
                  (COND ((NULL PREV) (SETF (RECEIVED-PKTS CONN) PKT))
                        (T (SETF (PKT-LINK PREV) PKT)))
                  (SETF (PKT-LINK PKT) NIL))
               (SETQ PKTL-NUM (PKT-NUM PKTL))
               (COND ((= PKT-NUM PKTL-NUM)      ;Same as existing one, forget about it.
                      (TRANSMIT-STS CONN 'ALREADY-ON-RCVD-PKTS int-pkt) ;Send a receipt
                      (RETURN NIL))
                     ((PKTNUM-< PKT-NUM PKTL-NUM)       ;This is the place!
                      (SETQ PKT (CONVERT-TO-PKT INT-PKT))
                      (COND ((NULL PREV)
                             (SETF (PKT-LINK PKT) (RECEIVED-PKTS CONN))
                             (SETF (RECEIVED-PKTS CONN) PKT))
                            (T
                             (SETF (PKT-LINK PKT) (PKT-LINK PREV))
                             (SETF (PKT-LINK PREV) PKT)))
                      (RETURN NIL))))))))

(DEFUN RECEIVE-UNC (CONN INT-PKT &AUX PKT)
  (PROG ()
    (COND ((EQ (STATE CONN) 'FOREIGN-STATE))    ;Foreign-protocol state--no checks
          ((NEQ (STATE CONN) 'OPEN-STATE)
           (RETURN (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Connection not open")))
          ((NOT (= (PKT-SOURCE-INDEX-NUM INT-PKT) (FOREIGN-INDEX-NUM CONN)))
           (RETURN (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP
                          "That is not your index number for this connection"))))
    (COND (( (LOOP FOR X = (READ-PKTS CONN) THEN (PKT-LINK X)
                    WHILE X
                    COUNT T)
              (LOCAL-WINDOW-SIZE CONN))
           ;; There are more packets on the list than the window size.  Discard
           ;; this packet.  This is so that we do not allocate infinite packet
           ;; buffers if someone throws lots of UNC packets at us.
           (NET:FREE-PACKET INT-PKT))
          (T
           ;; Convert to regular packet, do INTERRUPT-CONN, and thread it on.
           (SETQ PKT (CONVERT-TO-PKT INT-PKT))
           (AND (NULL (READ-PKTS CONN))
                (INTERRUPT-CONN :INPUT CONN))
           (WITHOUT-INTERRUPTS
             (SETF (PKT-LINK PKT) (READ-PKTS CONN))
             (SETF (READ-PKTS CONN) PKT)
             (AND (NULL (READ-PKTS-LAST CONN))
                  (SETF (READ-PKTS-LAST CONN) PKT)))))))

;;; Random servers are not likely to work during the cold load, leave them turned off until
;;; the first disk save.
(DEFVAR CHAOS-SERVERS-ENABLED NIL "T enables RFCs to create server processes.")

(ADD-INITIALIZATION "Allow CHAOS servers" '(SETQ CHAOS-SERVERS-ENABLED T) '(:ENABLE-SERVICES))
(ADD-INITIALIZATION "Disallow CHAOS servers"
                    '(SETQ CHAOS-SERVERS-ENABLED NIL
                           *RECEIVE-BROADCAST-PACKETS-P* NIL)
                    '(:DISABLE-SERVICES))
(ADD-INITIALIZATION "Allow CHAOS BRD Reception"
                    '(SETQ *RECEIVE-BROADCAST-PACKETS-P* T) '(:BEFORE-COLD))

(DEFUN DUPLICATE-RFC-INT-PKT-P (INT-PKT &AUX CONN)
  (OR (DO ((TST-PKT PENDING-RFC-PKTS (PKT-LINK TST-PKT)))
          ((NULL TST-PKT) NIL)
        (AND (= (PKT-SOURCE-ADDRESS INT-PKT) (PKT-SOURCE-ADDRESS TST-PKT))
             (= (PKT-SOURCE-INDEX-NUM INT-PKT) (PKT-SOURCE-INDEX-NUM TST-PKT))
             (RETURN T)))
      (DO ((I 1 (1+ I)))
          (( I MAXIMUM-INDEX) NIL)
        (AND (SETQ CONN (AR-1 INDEX-CONN I))
             (= (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS INT-PKT))
             (= (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM INT-PKT))
             (RETURN T)))))

;;; If RFC matches a pending LSN, call RFC-MEETS-LSN, else if there is a server,
;;; add to pending list and start up a server.
;;; (So far all we have done is verified PKT-DEST-ADDRESS.)
;;; Note that because of RFC-ANS stuff, the contact "name" is not the
;;; whole string, so we must do a simple parse.
(DEFUN RECEIVE-RFC (INT-PKT)
  (IF (DUPLICATE-RFC-INT-PKT-P INT-PKT)
      (NET:FREE-PACKET INT-PKT)
    (HANDLE-RFC-PKT (CONVERT-TO-PKT int-pkt nil) int-pkt T)))

(DEFUN HANDLE-RFC-PKT (PKT int-pkt CLS-ON-ERROR-P &AUX LSN SERVER CONTACT-NAME)
  "Handle an RFC packet.  If CLS-ON-ERROR-P is T, send a CLS if there's no server.
The associated INT-PKT should be passed (from the function CONVERT-TO-PKT)."
  (SETQ CONTACT-NAME (CONTACT-NAME-FROM-RFC PKT))
  (COND ((SETQ LSN (ASS 'EQUALP CONTACT-NAME PENDING-LISTENS))
         (SETQ PENDING-LISTENS (DELQ LSN PENDING-LISTENS))
         (NET:FREE-PACKET INT-PKT)
         (RFC-MEETS-LSN (CDR LSN) PKT))
        ((AND (OR (EQUALP CONTACT-NAME "STATUS") CHAOS-SERVERS-ENABLED)
              (SETQ SERVER (ASS 'EQUALP CONTACT-NAME SERVER-ALIST)))
         (WITHOUT-INTERRUPTS    ;seems like a good idea, altho probably not necessary
           (SETF (PKT-LINK PKT) PENDING-RFC-PKTS)
           (SETQ PENDING-RFC-PKTS PKT))
         (NET:FREE-PACKET INT-PKT)
         ;; This assumes that the name is in the CAR of an init list entry
         ;; was just EVAL
         (BACKGROUND-TASK (SI:INIT-FORM SERVER)))
        (CLS-ON-ERROR-P
         (TRANSMIT-LOS-INT-PKT (CONVERT-TO-INT-PKT PKT INT-PKT) CLS-OP
                                   "No server for this contact name [LISPM]")
         (FREE-PKT PKT))
        (T
         (FREE-PKT PKT)
         (NET:FREE-PACKET INT-PKT))))

(DEFUN CONTACT-NAME-FROM-RFC (PKT &AUX CONTACT-STRING TEM)
    (SETQ CONTACT-STRING (PKT-STRING PKT))
    (COND ((SETQ TEM (STRING-SEARCH-CHAR #o40 CONTACT-STRING))
           (NSUBSTRING CONTACT-STRING 0 TEM))
          (T CONTACT-STRING)))

;;; Receving a BRD is like receiving an RFC
(DEFUN RECEIVE-BRD (INT-PKT &AUX BIT-MAP-LENGTH BYTE-IN-MAP PKT)
  (COND ((AND *RECEIVE-BROADCAST-PACKETS-P*
              ;; Check the subnet bit map : length in bytes multiple of 4 ?
              (ZEROP (MOD (SETQ BIT-MAP-LENGTH (PKT-ACK-NUM INT-PKT)) 4))
              (> BIT-MAP-LENGTH (SETQ BYTE-IN-MAP (TRUNCATE MY-SUBNET 8)))) ; big enuf ?
         ;; Massage so it looks like an RFC: Delete bit map, decrease byte count
         (SETF (PKT-ACK-NUM INT-PKT) 0)
         (INCF *BRD-PKTS-IN*)
         (SETQ PKT (CONVERT-TO-PKT INT-PKT nil))
         (SET-PKT-STRING PKT (SUBSTRING (PKT-STRING PKT) BIT-MAP-LENGTH))
         (HANDLE-RFC-PKT PKT int-pkt NIL))
        (T
         (NET:FREE-PACKET INT-PKT))))

;;; This is called when we have a LSN matching an RFC.  It can be called when we do
;;; a LSN (m.p. level) or when an RFC gets here (p.i. level).
;;; Here LISTEN has filled in some of the fields of the CONN, we must
;;; fill in the rest.
(DEFUN RFC-MEETS-LSN (CONN PKT)
  (SETF (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS PKT))
  (SETF (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM PKT))
  (SETF (FOREIGN-WINDOW-SIZE CONN) (IF (> (PKT-ACK-NUM PKT) MAXIMUM-WINDOW-SIZE)
                                       DEFAULT-WINDOW-SIZE
                                     (PKT-ACK-NUM PKT)))
  (SETF (PKT-NUM-READ CONN) (PKT-NUM PKT))
  (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM PKT))
  (SETF (PKT-NUM-ACKED CONN) (PKT-NUM PKT))
  (SETF (STATE CONN) 'RFC-RECEIVED-STATE)
  (SETF (READ-PKTS CONN) PKT)
  (SETF (READ-PKTS-LAST CONN) PKT)
  (SETF (PKT-LINK PKT) NIL)
  (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'RFC-RECEIVED-STATE)
  (INTERRUPT-CONN :INPUT CONN))

;;; So far both host and both index numbers have been verified.
(DEFUN RECEIVE-OPN (CONN INT-PKT)
  (SELECTQ (STATE CONN)
    (RFC-SENT-STATE (SETF (FOREIGN-INDEX-NUM CONN) (PKT-SOURCE-INDEX-NUM INT-PKT))
                    (SETF (FOREIGN-WINDOW-SIZE CONN) (PKT-SECOND-DATA-WORD INT-PKT))
                    (SETF (PKT-NUM-READ CONN) (PKT-NUM INT-PKT))
                    (SETF (PKT-NUM-RECEIVED CONN) (PKT-NUM INT-PKT))
                    (SETF (PKT-NUM-ACKED CONN) (PKT-NUM INT-PKT))
                    (SETF (TIME-LAST-RECEIVED CONN) (zl:TIME))
                    (RECEIPT CONN (PKT-ACK-NUM INT-PKT))
                    (SETF (STATE CONN) 'OPEN-STATE)
                    (TRANSMIT-STS CONN 'OPN int-pkt)
                    (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'OPEN-STATE))
    (OPEN-STATE (COND ((AND (= (FOREIGN-ADDRESS CONN) (PKT-SOURCE-ADDRESS INT-PKT))
                            (= (FOREIGN-INDEX-NUM CONN)
                               (PKT-SOURCE-INDEX-NUM INT-PKT))
                            (= MY-ADDRESS (PKT-DEST-ADDRESS INT-PKT))
                            (= (LOCAL-INDEX-NUM CONN) (PKT-DEST-INDEX-NUM INT-PKT)))
                       (TRANSMIT-STS CONN 'OPN int-pkt))
                      (T (TRANSMIT-LOS-INT-PKT INT-PKT
                                               LOS-OP
                                               "You didn't open this connection"))))
    (OTHERWISE (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "Bad state for OPN"))))

;;; We have received a CLS.  If the connection which he is closing is still open,
;;; put it in closed state, free up all pending SEND-PKTs, and put the CLS packet
;;; on the READ-PKTS list so that MP level can see it.
;;;      If the connection does not exist, this is NOT an error, because two
;;; CLSs might have passed each other.  So just free the PKT.
(DEFUN RECEIVE-CLS (INT-PKT &AUX PKT (INT-FLAG NIL))
  (LET ((CONN (PKT-DEST-CONN INT-PKT)))
    (COND ((NULL CONN)
           (NET:FREE-PACKET INT-PKT))
          ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
           (SETQ PKT (CONVERT-TO-PKT INT-PKT))
           (WITHOUT-INTERRUPTS
             (FREE-ALL-SEND-PKTS CONN)
             (FREE-ALL-RECEIVED-PKTS CONN)
             (SETF (STATE CONN) 'CLS-RECEIVED-STATE)
             (COND ((NULL (READ-PKTS-LAST CONN))
                    (SETF (READ-PKTS CONN) PKT)
                    (SETQ INT-FLAG T))
                   (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
             (SETF (READ-PKTS-LAST CONN) PKT)
             (SETF (PKT-LINK PKT) NIL))
           (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'CLS-RECEIVED-STATE)
           (AND INT-FLAG (INTERRUPT-CONN :INPUT CONN)))
          (T (TRANSMIT-LOS-INT-PKT INT-PKT
                                   LOS-OP
                                   "You sent a CLS to the wrong kind of connection.")))))

(DEFUN RECEIVE-LOS (INT-PKT &AUX (PKT (CONVERT-TO-PKT INT-PKT)) MY-INDEX CONN (INT-FLAG NIL))
  (SETQ MY-INDEX (LDB MAXIMUM-INDEX-LOG-2-MINUS-1 (PKT-DEST-INDEX-NUM PKT)))
  (COND ((AND (< MY-INDEX MAXIMUM-INDEX)
              (SETQ CONN (AREF INDEX-CONN MY-INDEX))
              (EQ (STATE CONN) 'OPEN-STATE)
              (= (PKT-SOURCE-ADDRESS INT-PKT) (FOREIGN-ADDRESS CONN))
              (= (PKT-SOURCE-INDEX-NUM INT-PKT) (FOREIGN-INDEX-NUM CONN)))
         (WITHOUT-INTERRUPTS
           (FREE-ALL-SEND-PKTS CONN)
           (FREE-ALL-RECEIVED-PKTS CONN)
           (SETF (STATE CONN) 'LOS-RECEIVED-STATE)
           (COND ((NULL (READ-PKTS-LAST CONN))
                  (SETF (READ-PKTS CONN) PKT)
                  (SETQ INT-FLAG T))
                 (T (SETF (PKT-LINK (READ-PKTS-LAST CONN)) PKT)))
           (SETF (READ-PKTS-LAST CONN) PKT)
           (SETF (PKT-LINK PKT) NIL))
         (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'LOS-RECEIVED-STATE)
         (AND INT-FLAG (INTERRUPT-CONN :INPUT CONN)))
        (T (SETF (PKT-LINK PKT) LOS-PKTS)
           (SETQ LOS-PKTS PKT)
           (SETQ CURRENT-LOS-PKT-COUNT (1+ CURRENT-LOS-PKT-COUNT)))))

(DEFUN RECEIVE-FWD (CONN INT-PKT &AUX PKT)
  (COND ((NEQ (STATE CONN) 'RFC-SENT-STATE)
         (TRANSMIT-LOS-INT-PKT INT-PKT LOS-OP "An FWD was sent to a non-RFC-SENT index."))
        (T (SETQ PKT (CONVERT-TO-PKT INT-PKT))
           (SETF (FOREIGN-ADDRESS CONN) (PKT-ACK-NUM PKT))
           (SETF (PKT-OPCODE PKT) RFC-OP)
           (TRANSMIT-NORMAL-PKT CONN PKT (PKT-NUM-SENT CONN) (LOCAL-WINDOW-SIZE CONN)))))

(DEFUN RECEIVE-ANS (CONN INT-PKT &AUX PKT)
    (COND ((NOT (MEMQ (STATE CONN) '(RFC-SENT-STATE BROADCAST-SENT-STATE)))
           (NET:FREE-PACKET INT-PKT))
          (T (SETQ PKT (CONVERT-TO-PKT INT-PKT))
             (SETF (STATE CONN) 'ANSWERED-STATE)
             (SETF (READ-PKTS CONN) PKT)
             (SETF (PKT-LINK PKT) NIL)
             (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'ANSWERED-STATE)
             (INTERRUPT-CONN :INPUT CONN))))

;;;; Timed Responses and background tasks

(DEFUN BACKGROUND (&AUX LAST-WAKEUP-TIME (LAST-PROBE-TIME (zl:TIME)) TASKS TIME)
  (RESET-ROUTING-TABLE)
  (DO () (NIL)
    (SETQ TIME (zl:TIME))
    (SETQ LAST-WAKEUP-TIME TIME)
    (DO () ((OR ( (TIME-DIFFERENCE TIME LAST-WAKEUP-TIME) RETRANSMISSION-INTERVAL)
                ( (TIME-DIFFERENCE TIME LAST-PROBE-TIME) PROBE-INTERVAL)))
      (PROCESS-WAIT "Background Task"
                    #'(LAMBDA (LAST-WAKEUP-TIME LAST-PROBE-TIME &AUX (TIME (zl:TIME)))
                        (OR ( (TIME-DIFFERENCE TIME LAST-PROBE-TIME) PROBE-INTERVAL)
                            (AND RETRANSMISSION-NEEDED
                                 ( (TIME-DIFFERENCE TIME LAST-WAKEUP-TIME)
                                    RETRANSMISSION-INTERVAL))
                            BACKGROUND-REQUESTS))
                    LAST-WAKEUP-TIME LAST-PROBE-TIME)
      (WITHOUT-INTERRUPTS
        (SETQ TASKS (NREVERSE BACKGROUND-REQUESTS))
        (SETQ BACKGROUND-REQUESTS NIL))
      (DO () ((NULL TASKS))
        (EVAL (CAR TASKS))
        (SETQ TASKS (CDR TASKS)))
      (SETQ TIME (zl:TIME)))
    (COND (RETRANSMISSION-NEEDED
           (SETQ RETRANSMISSION-NEEDED NIL
                 MORE-RETRANSMISSION-NEEDED NIL)
           (MAPC 'RETRANSMISSION CONN-LIST)     ;Retransmit on all connections
           (WITHOUT-INTERRUPTS
             (SETQ RETRANSMISSION-NEEDED
                   (OR RETRANSMISSION-NEEDED MORE-RETRANSMISSION-NEEDED)))))
    (COND (( (TIME-DIFFERENCE TIME LAST-PROBE-TIME) PROBE-INTERVAL)
           (SETQ LAST-PROBE-TIME TIME)
           (DOTIMES (I (ARRAY-LENGTH ROUTING-TABLE-COST))
             (UNLESS (AREF ROUTING-TABLE-TYPE I)
               ;; If we have a direct connection of known type,
               ;; don't do the processing for indirect links.
               (ASET (MIN (+ (AREF ROUTING-TABLE-COST I) 2) #o1000)
                     ROUTING-TABLE-COST I)))
           (MAPC 'PROBE-CONN CONN-LIST)))))     ;Do probes and timeouts

;;; Retransmit all unreceipted packets not recently sent
(DEFUN RETRANSMISSION (CONN &AUX TIME (INHIBIT-SCHEDULING-FLAG T))
  (when (MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
    ;;Only if it is open or awaiting a response from RFC
    (when (null (send-pkts conn))
      (when (pktnum-< (send-pkt-acked conn) (pkt-num-sent conn))
        ;;Need to retransmit but no packets on send-list.  Remote end has received all of
        ;;our packets but has not acknowledged them all.  Send a SNS to elicit a STS
        (setq inhibit-scheduling-flag nil)
        (let ((pkt (net:allocate-packet)))
          (setf (pkt-opcode pkt) sns-op)
          (setf (pkt-nbytes-on-write pkt) 0)
          (transmit-int-pkt-for-conn conn pkt))
        (setq more-retransmission-needed t))    ;Indicate that pkts remain unacknowledged
      (return-from retransmission t))           ;And return from this function
    ;; Doing this outside the loop can lose
    ;; because then TIME can be less than (PKT-TIME-TRANSMITTED PKT).
    (SETQ TIME (zl:TIME))                       ;On the other hand, doing it inside the loop loses
    ;;because if there are enough PKTs pending for a particular CONN, it can
    ;;hang because every time it sends one it has to restart from the beginning
    ;;of the list.  So now we deal with the case mentioned above explicitly.
    (DO-NAMED CONN-DONE
              () (NIL)
      (LET ((INHIBIT-SCHEDULING-FLAG T))
        (DO* ((PKT (SEND-PKTS CONN) (PKT-LINK PKT))
              (first-pkt-num (and pkt (pkt-num pkt))))
             ((NULL PKT) (RETURN-FROM CONN-DONE NIL))
          (COND ((NOT (EQ CONN (PKT-SOURCE-CONN PKT)))
                 (FERROR "~S in SEND-PKTS list for incorrect CONN:
CONN ~S, (PKT-SOURCE-CONN PKT) ~S." PKT CONN (PKT-SOURCE-CONN PKT))))
          (SETQ MORE-RETRANSMISSION-NEEDED T)
          (COND ((TIME-LESSP TIME (PKT-TIME-TRANSMITTED PKT)))  ;Dont do this one again.
                (( (TIME-DIFFERENCE TIME (PKT-TIME-TRANSMITTED PKT))
                    (LSH (CONN-RETRANSMISSION-INTERVAL CONN)
                         ;; Retransmit the lowest numbered packet most often
                         (MAX 0 (MIN 5 (1- (pktnum-- (PKT-NUM PKT) first-pkt-num))))))
                 (SETF (PKT-BEING-RETRANSMITTED PKT) T)
                 (SETQ INHIBIT-SCHEDULING-FLAG NIL)
                 (TRANSMIT-PKT PKT T)
                 (INCF PKTS-RETRANSMITTED)
                 (SETQ INHIBIT-SCHEDULING-FLAG T)
                 (COND ((EQ (PKT-BEING-RETRANSMITTED PKT) 'FREE)
                        (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)
                        (FREE-PKT PKT))
                       (T (SETF (PKT-BEING-RETRANSMITTED PKT) NIL)))
                 (RETURN NIL)))))               ;Must always start from beginning of chain if
                                                ; turned on scheduling, since chain could be invalid
      (PROCESS-ALLOW-SCHEDULE))))

;;; Send a SNS on this conn if necessary.  Decide whether foreign host is down.
;;; This gets called every PROBE-INTERVAL.
(DEFUN PROBE-CONN (CONN &AUX DELTA-TIME)
  (COND ((MEMQ (STATE CONN) '(OPEN-STATE RFC-SENT-STATE))
         ;Only if it is open or awaiting a response from RFC
         (SETQ DELTA-TIME (TIME-DIFFERENCE (zl:TIME) (TIME-LAST-RECEIVED CONN)))
         (COND ((> DELTA-TIME HOST-DOWN-INTERVAL)
                (WITHOUT-INTERRUPTS
                  (FREE-ALL-SEND-PKTS CONN)
                  (FREE-ALL-RECEIVED-PKTS CONN)
                  (SETF (STATE CONN) 'HOST-DOWN-STATE))
                (INTERRUPT-CONN :CHANGE-OF-STATE CONN 'HOST-DOWN-STATE))
               ((AND (EQ (STATE CONN) 'OPEN-STATE)      ;Send SNS only on open connections
                     (OR (< (WINDOW-AVAILABLE CONN) (FOREIGN-WINDOW-SIZE CONN))
                         (> DELTA-TIME LONG-PROBE-INTERVAL)))
                (LET ((PKT (NET:ALLOCATE-PACKET)))
                  (SETF (PKT-OPCODE PKT) SNS-OP)
                  (SETF (PKT-NBYTES-on-write PKT) 0)
                  (TRANSMIT-INT-PKT-FOR-CONN CONN PKT)))))))

(defun transmit-int-pkt (int-pkt &optional host subnet (broadcast-if-necessary t)
                         &aux byte-count address interface on-behalf-of)
  (unless (net:check-packet (net:original-array int-pkt))
    (error "Attempt to transmit non-interrupt packet ~A." int-pkt))

  (when (null host)
    (setq host (pkt-dest-address int-pkt)))
  (when (null subnet)
    (setq subnet (pkt-dest-subnet int-pkt)))

  (unless (= my-address (pkt-source-address int-pkt))
    (setq on-behalf-of (pkt-source-address int-pkt)))

  (unless (or (= subnet my-subnet) (member subnet my-other-subnets))
    (when (>= subnet (array-length routing-table))
      (setq subnet 0))
    (setq host (aref routing-table subnet)))

  (setq byte-count (* 2 (pkt-nwords int-pkt)))

  (when (bit-test #o200 (pkt-opcode int-pkt))
    (incf data-pkts-out))

  (setf (ldb (byte 8 0) (aref int-pkt 0)) 0)    ;Set header version to 0

  (cond ((null *chaos-stream*)                  ;Chaos not enabled
         (net:free-packet int-pkt))

        ((and broadcast-if-necessary
              (zerop host)
              (zerop (pkt-dest-address int-pkt)))
         ;;Here if this is a real broadcast packet
         (incf pkts-transmitted)                ;Increment counter looked at by Hostat
         (send *chaos-stream*
               :broadcast
               int-pkt
               byte-count))

        ((= host my-address)
         ;;Here if packet is for this host
         (incf pkts-transmitted)                ;Increment counter looked at by Hostat
         (send *chaos-stream*
               :send
               int-pkt
               byte-count
               net:*loopback-interface*
               0))

        ((and si:share-interface
              (not (zerop host))
              (multiple-value-setq (address interface)
                (send *chaos-stream*
                      :translate-address
                      host
                      si:share-interface
                      on-behalf-of)))
         ;;Here if to a processor on same bus
         (incf pkts-transmitted)                ;Increment counter looked at by Hostat
         (send *chaos-stream*
               :send
               int-pkt
               byte-count
               interface
               address))

        ;;Maybe this case should be handled by some sort of "default gateway" mechanism.
        ((and si:*ethernet-hardware-controller*
              (not (eq si:*ethernet-hardware-controller* si:*my-op*))
              (si:share-mode-active-p))
         ;;Here if there is Ethernet hardware and it is owned by another processor
         (incf pkts-transmitted)                ;Increment counter looked at by Hostat
         (send *chaos-stream*
               :send
               int-pkt
               byte-count
               si:share-interface
               si:*ethernet-hardware-controller*))

        ((multiple-value-setq (address interface)
           (send *chaos-stream*
                 :translate-address
                 ;;If host is 0 and not a broadcast packet, routing failed.  Still, the unknown
                 ;;Chaos subnet MIGHT be on our physical network, so do address translation...
                 (if (zerop host) (pkt-dest-address int-pkt) host)
                 nil
                 on-behalf-of))
         ;;Here if there is Ethernet hardware and we own it and packet goes out of this rack
         (incf pkts-transmitted)                ;Increment counter looked at by Hostat
         (send *chaos-stream*
               :send
               int-pkt
               byte-count
               interface
               address))

        (t
         (incf (chaos-packets-sent-discarded *chaos-stream*))
         (incf (chaos-bytes-sent-discarded *chaos-stream*) byte-count)
         (net:free-packet int-pkt)))
  )

;;;; KLUDGES and assorted random functions

(DEFUN ASSURE-ENABLED ()
  "Make sure the chaosnet NCP is operating.
User programs may call this before attempting to use the chaosnet."
  (OR ENABLE (ENABLE)))

(defun enable (&optional stream)
  (declare (ignore stream))
  (send background :revoke-run-reason)
  (send background :preset 'background)
  (send background :run-reason)
  (setq enable t))

(defun disable (&optional stream)
  (declare (ignore stream))
  (setq enable nil)
  (when background
    (send background :revoke-run-reason))
  nil)

(defun reset (&optional enable-p)
  "Turn off and reinitialize the chaosnet software.
This may unwedge it if it is not working properly.
This will cause all of your currently open connections to lose.
You must call CHAOS:ENABLE to turn the chaosnet ncp on again
before you can use the chaosnet; but many user-level functions
that use the net will do that for you.
Calling this function with ENABLE-P of T will have the same effect."
  (disable)
  (without-interrupts
    (setq background-requests nil)              ;Get rid of requests for connections flushing
    (setq retransmission-needed t)
    (do ((cl conn-list (cdr cl)))
        ((null cl))
      (free-all-read-pkts (car cl))
      (free-all-received-pkts (car cl))
      (free-all-send-pkts (car cl))
      (setf (state (car cl)) 'inactive-state))
    (do ((i 1 (1+ i)))
        ((= i maximum-index))
      (aset nil index-conn i))
    (setq distinguished-port-conn-table nil)
    (setq conn-list nil)
    (or (and (fixp index-conn-free-pointer)
             (> maximum-index index-conn-free-pointer -1))
        (setq index-conn-free-pointer 1))

    ;; The initialization is needed because if the LISP Machine has an open connection,
    ;; it gets reloaded, and the connection is established on the same index before the
    ;; other end has gone into INCXMT state, then the RFC will look like a duplicate.
    ;; Though this may sound like a rare event, it is exactly what happens with the
    ;; file job connection!!
    (do ((index 0 (1+ index)))
        ((>= index maximum-index))
      (aset (+ (zl:time) index) uniquizer-table index))

    ;; Should actually try and free up these
    (setq pending-listens nil)
    (setq pending-rfc-pkts nil)
    (setq *brd-pkts-in* 0
          *brd-pkts-out* 0
          *brd-history* nil
          *brd-replies-in* 0
          *receive-broadcast-packets-p* nil))
  (when enable-p
    (enable))
  (format nil "Reset and ~:[dis~;en~]abled -- perhaps you need to do (NET:CONFIGURE)" enable))

(DEFUN PKT-ADD-32 (PKT COUNT)
  (LET ((IDX (+ FIRST-DATA-WORD-IN-PKT (TRUNCATE (PKT-NBYTES PKT) 2))))
    (ASET (LDB (byte 16 0) COUNT) PKT IDX)
    (ASET (LDB (byte 16 16) COUNT) PKT (1+ IDX))
    (SETF (PKT-NBYTES-on-write PKT) (+ (PKT-NBYTES PKT) 4))))

(DEFUN SEND-STATUS (&AUX CONN PKT STRING)
  (SETQ CONN (LISTEN "STATUS"))
  (SETQ PKT (GET-PKT))
  (SET-PKT-STRING PKT (HOST-DATA MY-ADDRESS))
  (DO I (ARRAY-ACTIVE-LENGTH (SETQ STRING (PKT-STRING PKT))) (1+ I) ( I 32.)
    (ARRAY-PUSH STRING 0))
  (SETF (PKT-NBYTES-on-write PKT) 32.)
  (PKT-ADD-32 PKT (DPB 16. (byte 16 16) (+ (chaos-subnet-number-from-address my-address) #o400)))
  (PKT-ADD-32 PKT PKTS-RECEIVED)
  (PKT-ADD-32 PKT PKTS-TRANSMITTED)
  (PKT-ADD-32 PKT (READ-METER '%COUNT-CHAOS-TRANSMIT-ABORTS))
  (PKT-ADD-32 PKT PKTS-LOST)
  (PKT-ADD-32 PKT PKTS-BAD-CRC-1)
  (PKT-ADD-32 PKT PKTS-BAD-CRC-2)
  (PKT-ADD-32 PKT PKTS-BAD-BIT-COUNT)
  (PKT-ADD-32 PKT PKTS-OTHER-DISCARDED)
  (ANSWER CONN PKT))

(ADD-INITIALIZATION "STATUS" '(SEND-STATUS) NIL 'SERVER-ALIST)

;;; routing packet transmitter

(DEFVAR RUT-TRANSMITTER-PROCESS NIL)

(DEFUN START-RUT-TRANSMITTER-PROCESS ()
  (IF (NULL RUT-TRANSMITTER-PROCESS)
      (SETQ RUT-TRANSMITTER-PROCESS
            (MAKE-PROCESS "Chaos RUT transmitter" ':WARM-BOOT-ACTION NIL ':PRIORITY 25.)))
  (COND ((NULL MY-OTHER-SUBNETS)
         (PROCESS-DISABLE RUT-TRANSMITTER-PROCESS))
        (T
         (GLOBAL:SEND RUT-TRANSMITTER-PROCESS ':PRESET 'RUT-TRANSMITTER-TOP-LEVEL)
         (PROCESS-RESET-AND-ENABLE RUT-TRANSMITTER-PROCESS))))

(DEFVAR *NUMBER-OF-ROUTING-PACKETS* 0)

(DEFUN RUT-TRANSMITTER-TOP-LEVEL ()
  (LET ((SUBNET-LIST (CONS MY-SUBNET MY-OTHER-SUBNETS))
        (ADDRESS-LIST (CONS MY-ADDRESS MY-OTHER-ADDRESSES)))
    (DO-FOREVER
      (DO ((SUB SUBNET-LIST (CDR SUB))
           (ADDR ADDRESS-LIST (CDR ADDR))
           (INT-PKT (NET:ALLOCATE-PACKET) (NET:ALLOCATE-PACKET)))
          ((NULL SUB) (NET:FREE-PACKET INT-PKT))
        (DO ((SUBNET 1 (1+ SUBNET))
             (I FIRST-DATA-WORD-IN-PKT)
             (N 0)
             )
            ((= SUBNET (ARRAY-LENGTH ROUTING-TABLE))
             (SETF (PKT-NBYTES-on-write INT-PKT) (* N 4)))
          (COND ((< (AREF ROUTING-TABLE-COST SUBNET) MAXIMUM-ROUTING-COST)
                 (ASET SUBNET INT-PKT I)
                 (ASET (AREF ROUTING-TABLE-COST SUBNET) INT-PKT (1+ I))
                 (SETQ I (+ I 2))
                 (SETQ N (+ N 1)))))
        (SETF (PKT-OPCODE INT-PKT) RUT-OP)
        (SETF (PKT-SOURCE-ADDRESS INT-PKT) (CAR ADDR))
        (SETF (PKT-SOURCE-INDEX-NUM INT-PKT) 0)
        (SETF (PKT-DEST-ADDRESS INT-PKT) 0)
        (SETF (PKT-DEST-INDEX-NUM INT-PKT) 0)
        (TRANSMIT-INT-PKT INT-PKT 0 (CAR SUB)) ; frees int-pkt
        (INCF *NUMBER-OF-ROUTING-PACKETS*))
      (PROCESS-SLEEP (* 60. 10.)))))

(ADD-INITIALIZATION "CHAOS-NCP" '(INITIALIZE-NCP-ONCE) '(ONCE))
;reset chaos net before disk-save, and early on in every cold or warm boot, and now if just adding
(ADD-INITIALIZATION "Reset Chaosnet" '(RESET) '(:BEFORE-COLD))
(ADD-INITIALIZATION "Reset Chaosnet" '(RESET) '(:system :first))
(ADD-INITIALIZATION "CHAOS-NCP" '(INITIALIZE-NCP-COLD) '(COLD :first))
;; NORMAL below prevents this from losing while reloading the system.
(add-initialization "Setup my address" '(setup-my-address) '(site normal))

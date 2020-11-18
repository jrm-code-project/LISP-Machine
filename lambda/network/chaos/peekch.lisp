;;; -*- Mode: LISP;  Package: CHAOS;  Base: 8 -*-
;;;     ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;; Chaosnet peek functions

(TV:DEFINE-PEEK-MODE PEEK-HOSTAT #/H "Hostat" T
                     "Print chaosnet statistics of all known hosts.")

(TV:DEFINE-PEEK-MODE PEEK-CHAOS #/C "Chaosnet" NIL
                     "Display useful information about all chaosnet connections other chaos related information.")

(DEFUN PEEK-HOSTAT (&REST IGNORE)
  (HOSTAT))

(DEFUN PEEK-CHAOS-PACKET-ITEM (PKT &OPTIONAL (INDENT 0))
  "Returns an item that describes a chaosnet packet.  Mouseable subfields are:
   The host:  Left: Causes info about the host to displayed inferior to the packet.
              Middle: Causes a static hostat to be displayed inferior to the packet.
              Right (menu): Typeout Hostat, Supdup, Telnet, Qsend

Sample output:
Pkt [to ! from] <name> (number){, transmitted <n> times (at <time>)}{, being retransmitted}{, released}{, fowarded <n> times}
    <op> (<number>), <n> bytes, number <n>, acking <n>, source idx <n>, dest idx <n>
    Words from <n>: <wordn> ... <wordn+m>
    String: <string>

Packet: to AI (2026), transmitted 27 times (at 1231232), being retransmitted
 CLS (11), 432 bytes, number 3422, acking 3221, source idx 177777, dest idx 177777
 Words from 0: 123123 12371 1227 272727 272626
 String: /"Now is the time for all good men/"

Packet: from MC (1440), released, forwarded 17 times
 DAT (201), 100 bytes, number 432, acking 102, source idx 123451, dest idx 123441
 Words from 0: 123123 64532
 String: /"FUKT!/"

"
  (LET ((TO-US (AND (ZEROP (PKT-TIMES-TRANSMITTED PKT))
                    (= (PKT-DEST-ADDRESS PKT) MY-ADDRESS)))
        (OTHER-HOST))
    (SETQ OTHER-HOST (IF TO-US
                         (PKT-SOURCE-ADDRESS PKT)
                         (PKT-DEST-ADDRESS PKT)))
    (LIST ()
      (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-PACKET-INSERT-HOSTAT)
        (TV:SCROLL-PARSE-ITEM
          ':LEADER 4
          `(:MOUSE-ITEM (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',OTHER-HOST 'TV:ITEM 0 ,INDENT)
                             :DOCUMENTATION "Menu of useful things to do to this host.")
            :STRING ,(FORMAT NIL "~V@TPacket ~:[to~;from~] ~@[~A ~](~O)"
                             INDENT TO-US
                             (SI:GET-HOST-FROM-ADDRESS OTHER-HOST ':CHAOS) OTHER-HOST))
          (AND (NOT TO-US)
               `(:FUNCTION ,#'PKT-TIMES-TRANSMITTED (,PKT)
                           NIL (", transmitted ~D times")))
          (AND (NOT TO-US)
               `(:FUNCTION ,#'PKT-TIME-TRANSMITTED (,PKT) NIL (" (at ~O)")))
          (AND (NOT TO-US)
               `(:FUNCTION ,#'PKT-BEING-RETRANSMITTED (,PKT)
                           NIL ("~:[, being retransmitted~;~]")))
          `(:FUNCTION ,#'PKT-STATUS (,PKT) NIL ("~:[~;, Status: ~@*G~A~]"))
          (AND TO-US
               (FORMAT NIL ", fowarded ~D time~:P" (PKT-FWD-COUNT PKT)))))

      ;; Second line
      (LET ((OP (PKT-OPCODE PKT)))
       (TV:SCROLL-PARSE-ITEM
         (FORMAT NIL
                 "~V@T~A (~O), ~O bytes, number ~O, acking ~O, source idx ~O, dest idx ~O"
                 INDENT
                 (IF ( OP DAT-OP)
                     "Data"
                   (NTH OP OPCODE-LIST))
                 OP
                 (PKT-NBYTES PKT)
                 (PKT-NUM PKT) (PKT-ACK-NUM PKT)
                 (PKT-SOURCE-INDEX-NUM PKT) (PKT-DEST-INDEX-NUM PKT))))
      (TV:SCROLL-PARSE-ITEM (FORMAT NIL "~V@T" INDENT) (PEEK-CHAOS-PKT-WORDS PKT 0 6))
      (TV:SCROLL-PARSE-ITEM (FORMAT NIL "~V@TString: " INDENT) (PEEK-CHAOS-PKT-STRING PKT)))))

(DEFUN PEEK-CHAOS-PKT-WORDS (PKT START NUMBER &AUX STRING)
  "Returns a string consisting of words from the packet."
  (SETQ STRING (FORMAT NIL "Words from ~O: " START))
  (DO ((I START (1+ I))
       (LEN (ARRAY-LENGTH PKT)))
      ((OR ( I (+ START NUMBER)) ( I LEN))
       STRING)
    (SETQ STRING
          (STRING-APPEND STRING
                         (FORMAT NIL "~6O" (AREF PKT (+ FIRST-DATA-WORD-IN-PKT I)))
                         " "))))

;;; Boy, is this piece of shit ad hoc!!
(DEFUN PEEK-CHAOS-PKT-STRING (PKT &OPTIONAL COUNT)
  "Returns a 'safe' string as far as the scrolling stuff is concerned"
  (DO ((STRING (MAKE-STRING #o100 :FILL-POINTER 0))
       (PKT-STRING (PKT-STRING PKT))
       (CHAR)
       (I 0 (1+ I))
       (LEN (STRING-LENGTH (PKT-STRING PKT))))
      ((OR ( I LEN) (AND COUNT ( I COUNT)))
       STRING)
    (SETQ CHAR (AREF PKT-STRING I))
    (IF (AND (< CHAR #o200) ( CHAR #/))
        (VECTOR-PUSH-EXTEND CHAR STRING)
      (VECTOR-PUSH-EXTEND #/ STRING)
      (IF ( CHAR #/)
          (ARRAY-PUSH-EXTEND STRING (LOGIOR #o100 (LOGAND CHAR #o77)))
          (ARRAY-PUSH-EXTEND STRING #/)))))

(DEFUN PEEK-CHAOS-CONN (CONN)
  "Format is:

Connection to <contact> at/from Host <name> (<number>),
<state>, local idx <n>, foreign idx <n>
Windows: local <n>, foreign <n> (<n> available)
Received: pkt <n> (time <n>), read pkt <n>, ack pkt <n>, <n> queued
Sent: pkt <n>, ack for pkt <n>, <n> queued
"
  (LIST ()
    (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-INSERT-HOSTAT)
          (TV:SCROLL-PARSE-ITEM
            ':LEADER 3
            `(:MOUSE-ITEM
               (NIL :MENU-CHOOSE
                    ("Connection Operations"
                     ("Close" :EVAL (CHAOS:CLOSE-CONN CONN)
                      :DOCUMENTATION
                      "Click left to close this connection")
                     ("Inspect" :EVAL
                      (LET ((*TERMINAL-IO* TYPWIN))
                        (INSPECT CONN))
                      :DOCUMENTATION
                      "Click left to INSPECT this connection.")
                     ("Describe" :EVAL
                      (LET ((*TERMINAL-IO* TYPWIN))
                        (DESCRIBE CONN))
                      :DOCUMENTATION
                      "Click left to DESCRIBE this connection."))
                    :DOCUMENTATION
                    "Menu of things to do to this connection."
                    :BINDINGS
                    ((CONN ',CONN)
                     (TYPWIN ',(SEND SELF ':TYPEOUT-WINDOW))))
               :FUNCTION CONTACT-NAME (,CONN) NIL ("~@[Connection to ~A~]"))
            `(:FUNCTION ,#'(LAMBDA (CONN)
                             (COND ((GETF (CONN-PLIST CONN) 'RFC-CONTACT-NAME)
                                    " at host")
                                   ((GETF (CONN-PLIST CONN) 'LISTEN-CONTACT-NAME)
                                    " from host")
                                   (T "Host")))
                        (,CONN)
                        NIL ("~A "))
            ;; The following code returns a list which specifies the entry
            ;; that prints the name of the connection's host.
            ;; The hair is because we create a closure to put in the entry specification.
            (LET ((PEEK-CHAOS-HOST (CONS -1 NIL)))
              (DECLARE (SPECIAL PEEK-CHAOS-HOST))
              `(:MOUSE-ITEM
                 (NIL :EVAL (PEEK-CHAOS-HOST-MENU (CAR ',(LOCF (CAR PEEK-CHAOS-HOST)))
                                                  'TV:ITEM 0)
                      :DOCUMENTATION "Menu of useful things to do to this host.")
                 :FUNCTION ,(CLOSURE '(PEEK-CHAOS-HOST)
                                     #'(LAMBDA (CONN)
                                         (AND ( (CAR PEEK-CHAOS-HOST)
                                                 (PROG2 (RPLACA PEEK-CHAOS-HOST
                                                                (FOREIGN-ADDRESS CONN))
                                                        (CAR PEEK-CHAOS-HOST)))
                                              (RPLACD PEEK-CHAOS-HOST
                                                      (FORMAT NIL "~@[~A ~](~O), "
                                                              (SI:GET-HOST-FROM-ADDRESS
                                                                (CAR PEEK-CHAOS-HOST) ':CHAOS)
                                                              (CAR PEEK-CHAOS-HOST))))
                                         (CDR PEEK-CHAOS-HOST)))
                 (,CONN) NIL))))
    (TV:SCROLL-PARSE-ITEM
      `(:FUNCTION STATE (,CONN) NIL)
      `(:FUNCTION LOCAL-INDEX-NUM (,CONN) NIL (", local idx ~O, "))
      `(:FUNCTION FOREIGN-INDEX-NUM (,CONN) NIL ("foreign idx ~O")))
    (TV:SCROLL-PARSE-ITEM
      `(:FUNCTION ,#'LOCAL-WINDOW-SIZE (,CONN) NIL ("Windows: local ~D, "))
      `(:FUNCTION ,#'FOREIGN-WINDOW-SIZE (,CONN) NIL ("foreign ~D, "))
      `(:FUNCTION ,#'WINDOW-AVAILABLE (,CONN) NIL ("(~D available)")))
    (LIST `(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-RECEIVED-PKTS :CONNECTION ,CONN)
      (TV:SCROLL-PARSE-ITEM
        ':LEADER 1
        ':MOUSE-SELF '(NIL :EVAL (TV:PEEK-MOUSE-CLICK 'SELF 0)
                           :DOCUMENTATION
                           "Insert//remove display of packets on receive list.")
        `(:FUNCTION ,#'PKT-NUM-RECEIVED (,CONN) NIL ("Received: pkt ~O"))
        `(:FUNCTION ,#'TIME-LAST-RECEIVED (,CONN) NIL (" (time ~O), "))
        `(:FUNCTION ,#'PKT-NUM-READ (,CONN) NIL ("read pkt ~O, "))
        `(:FUNCTION ,#'PKT-NUM-ACKED (,CONN) NIL ("ack pkt ~O, "))
        `(:FUNCTION ,#'(LAMBDA (CONN)
                         (- (PKT-NUM-RECEIVED CONN) (PKT-NUM-READ CONN)))
                    (,CONN) NIL ("~D queued"))))
    (LIST `(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-SEND-PKTS :CONNECTION ,CONN)
      (TV:SCROLL-PARSE-ITEM
        ':LEADER 1
        ':MOUSE-SELF '(NIL :EVAL (TV:PEEK-MOUSE-CLICK 'SELF 0)
                           :DOCUMENTATION
                           "Insert//remove display of packets on transmit list.")
        `(:FUNCTION ,#'PKT-NUM-SENT (,CONN) NIL ("Sent: pkt ~O, "))
        `(:FUNCTION ,#'SEND-PKT-ACKED (,CONN) NIL ("ack for pkt ~O, "))
        `(:FUNCTION ,#'SEND-PKTS-LENGTH (,CONN) NIL ("~D queued"))))
    (TV:SCROLL-PARSE-ITEM "")))

(DEFUN PEEK-CHAOS-SUBNET-NAME (SUBNET)
  (LET ((STRING
          (FORMAT NIL "~:[Direct~;~A~]"
                  ( SUBNET MY-SUBNET)
                  ;; this could really be clever and try to save away the info somewhere
                  (LET ((BRIDGE (AREF ROUTING-TABLE SUBNET)))
                    (COND ((AND BRIDGE (NOT (ZEROP BRIDGE)))
                           (HOST-DATA BRIDGE))
                          (T "No Connection"))))))
    (SUBSTRING STRING 0 (MIN 18. (STRING-LENGTH STRING)))))

(DEFUN PEEK-CHAOS-ROUTING-COST (SUBNET)
  (FORMAT NIL "~:[~4D.~]"
          (= SUBNET MY-SUBNET)
          (AREF ROUTING-TABLE-COST SUBNET)))

(DEFUN PEEK-CHAOS (IGNORE)
  "Displays state of all chaos net connections, meters, and routing table"
  (LIST NIL
        (TV:SCROLL-PARSE-ITEM "Chaos connections at "
                              `(:FUNCTION ,#'TIME () NIL ("~O")))
        (TV:SCROLL-PARSE-ITEM "")
        (TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () CONN-LIST)
                                 #'PEEK-CHAOS-CONN)
        (TV:SCROLL-PARSE-ITEM "Interesting meters")
        (TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () PEEK-A-BOO-LIST)
                                 #'(LAMBDA (COUNTER)
                                     (TV:SCROLL-PARSE-ITEM
                                       `(:STRING ,(STRING COUNTER) 35.)
                                       `(:FUNCTION SYMEVAL (,COUNTER) NIL ("~@15A" 10. T)))))
        (TV:SCROLL-PARSE-ITEM '(:STRING "%COUNT-CHAOS-TRANSMIT-ABORTS" 35.)
                              '(:FUNCTION READ-METER (%COUNT-CHAOS-TRANSMIT-ABORTS) NIL
                                          ("~@15A" 10. T)))
        (TV:SCROLL-PARSE-ITEM "")
        (TV:SCROLL-PARSE-ITEM "Subnet  Gateway            Cost")
        (TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () 1)
                                 #'(LAMBDA (SUBNET)
                                     (TV:SCROLL-PARSE-ITEM
                                       `(:STRING ,(FORMAT NIL "~O" SUBNET) 8.)
                                       `(:FUNCTION PEEK-CHAOS-SUBNET-NAME (,SUBNET) 16.)
                                       `(:FUNCTION PEEK-CHAOS-ROUTING-COST (,SUBNET) 12.)))
                                 NIL
                                 #'(LAMBDA (SUBNET)
                                     (LET ((NEW-SUBNET
                                             (POSITION-IF-NOT #'ZEROP
                                                              ROUTING-TABLE :START SUBNET)))
                                       (VALUES NEW-SUBNET
                                               (AND NEW-SUBNET (+ NEW-SUBNET 1))
                                               (NOT NEW-SUBNET)))))))

(DEFUN PEEK-CHAOS-HOST-MENU (&REST ARGS)
  (APPLY 'PROCESS-RUN-FUNCTION "Peek Chaos Menu" SELF ':PEEK-CHAOS-HOST-MENU ARGS))

(DEFMETHOD (TV:BASIC-PEEK :PEEK-CHAOS-HOST-MENU)
           (HOST ITEM &OPTIONAL (OFFSET 0) &REST ADDITIONAL-STUFF)
  "Menu for interesting operations on hosts in a PEEK display.
HOST may be a host object or chaosnet address.
ITEM is the item containing the sensitive area clicked on to get this menu.
OFFSET and ADDITIONAL-STUFF are hairy, for the INSERT-HOSTAT choice."
  (WHEN (NUMBERP HOST)
    (SETQ HOST (OR (SI:GET-HOST-FROM-ADDRESS HOST ':CHAOS) HOST)))
  (LET ((CHOICE (TV:MENU-CHOOSE
                  '(("Hostat One" :VALUE HOSTAT-ONE
                     :DOCUMENTATION "Show Hostat for selected host in typeout window.")
                    ("Hostat All" :VALUE HOSTAT-ALL
                     :DOCUMENTATION "Show Hostat for all hosts in typeout window.")
                    ("Insert Hostat" :VALUE HOSTAT-INSERT
                     :DOCUMENTATION "Insert static Hostat for selected host in the display.")
                    ("Remove Hostat" :VALUE HOSTAT-REMOVE
                     :DOCUMENTATION "Remove static Hostat.")
                    ("Supdup" :VALUE HOSTAT-SUPDUP :DOCUMENTATION "SUPDUP to selected host.")
                    ("Telnet" :VALUE HOSTAT-TELNET :DOCUMENTATION "TELNET to selected host.")
                    ("Qsend" :VALUE HOSTAT-QSEND
                     :DOCUMENTATION "Send a message to user on selected host.")
                    ("Inspect" :VALUE HOSTAT-INSPECT
                     :DOCUMENTATION "Look at host object in inspector."))
                  (STRING-APPEND "HOST "
                                 (IF (NUMBERP HOST) (FORMAT NIL "~O" HOST)
                                   (SEND HOST ':NAME)))))
        (*TERMINAL-IO* TV:TYPEOUT-WINDOW))
    (SELECTQ CHOICE
      (HOSTAT-ONE (HOSTAT HOST))
      (HOSTAT-ALL (HOSTAT))
      ((HOSTAT-INSERT HOSTAT-REMOVE)
       (SETF (ARRAY-LEADER ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET OFFSET))
             (EQ CHOICE 'HOSTAT-INSERT))
       (SETF (ARRAY-LEADER ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET OFFSET 1)) HOST)
       (DOTIMES (I (LENGTH ADDITIONAL-STUFF))
         (SETF (ARRAY-LEADER ITEM (+ TV:SCROLL-ITEM-LEADER-OFFSET OFFSET I 2))
               (NTH I ADDITIONAL-STUFF)))
       (SETQ TV:NEEDS-REDISPLAY T))
      (HOSTAT-SUPDUP (SEND SELF ':FORCE-KBD-INPUT `(SUPDUP ,HOST)))
      (HOSTAT-TELNET (SEND SELF ':FORCE-KBD-INPUT `(TELNET ,HOST)))
      (HOSTAT-QSEND (SEND SELF ':FORCE-KBD-INPUT `(QSEND ,HOST)))
      (HOSTAT-INSPECT (SEND SELF ':FORCE-KBD-INPUT `(INSPECT ,HOST)))
      (NIL)
      (OTHERWISE (BEEP)))))

(DEFUN PEEK-CHAOS-CONN-INSERT-HOSTAT (ITEM &AUX HOST)
  "A pre-process function to insert/remove a hostat from the display."
  (COND ((ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET)
         ;; Want a hostat, make sure it's there and for the right host
         (IF (AND (EQ (SETQ HOST (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
                                               (1+ TV:SCROLL-ITEM-LEADER-OFFSET)))
                      (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
                                    (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)))
                  (CDDR ITEM))
             NIL
             (RPLACD (CDR ITEM)
                     (PEEK-CHAOS-HOSTAT HOST 1))
             (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
                                 (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)) HOST)))
        (T (RPLACD (CDR ITEM) NIL)
           (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM))
                               (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)) NIL))))

(DEFUN PEEK-CHAOS-PACKET-INSERT-HOSTAT (ITEM &AUX HOST SI)
  "A pre-process function to insert/remove a hostat from the display."
  (COND ((ARRAY-LEADER (SETQ SI (FIRST (TV:SCROLL-ITEMS ITEM))) TV:SCROLL-ITEM-LEADER-OFFSET)
         ;; Want a hostat, make sure it's there and for the right host
         (IF (AND (EQ (SETQ HOST (ARRAY-LEADER SI (1+ TV:SCROLL-ITEM-LEADER-OFFSET)))
                      (ARRAY-LEADER SI (+ TV:SCROLL-ITEM-LEADER-OFFSET 3)))
                  (CDDR ITEM))
             NIL
             (RPLACD (CDR ITEM)
                     (PEEK-CHAOS-HOSTAT HOST
                                        (1+ (ARRAY-LEADER SI
                                                          (+ TV:SCROLL-ITEM-LEADER-OFFSET 2)))))
             (SETF (ARRAY-LEADER SI (+ TV:SCROLL-ITEM-LEADER-OFFSET 3)) HOST)))
        (T (RPLACD (CDR ITEM) NIL)
           (SETF (ARRAY-LEADER SI (+ TV:SCROLL-ITEM-LEADER-OFFSET 3)) NIL))))

(DEFVAR *PEEK-HOSTAT-LIST*)
(DEFVAR *PEEK-HOSTAT-STRING*)
(DEFVAR *PEEK-HOSTAT-INDENT*)

(DEFUN PEEK-CHAOS-HOSTAT (HOST *PEEK-HOSTAT-INDENT* &OPTIONAL PKT
                          &AUX (*PEEK-HOSTAT-LIST* NIL) (*PEEK-HOSTAT-STRING* NIL))
  (COND ((OR PKT (SETQ PKT (GET-HOST-STATUS-PACKET HOST)))
         (PEEK-HOSTAT-STREAM ':TYO #/CR)
         (HOSTAT-HEADING 'PEEK-HOSTAT-STREAM NIL)
         (HOSTAT-FORMAT-ANS  'PEEK-HOSTAT-STREAM (PKT-SOURCE-ADDRESS PKT) PKT)
         ;; Parse the strings into scroll items, removing any blank lines
         (SETQ *PEEK-HOSTAT-LIST* (NREVERSE *PEEK-HOSTAT-LIST*))
         (DO ((L *PEEK-HOSTAT-LIST* (CDR L)))
             ((NULL L) (LIST* () *PEEK-HOSTAT-LIST*))
           (IF (STRING-SEARCH-NOT-CHAR #/  (CAR L))
               (RPLACA L (TV:SCROLL-PARSE-ITEM (CAR L)))
               (SETQ *PEEK-HOSTAT-LIST* (DELQ (CAR L) *PEEK-HOSTAT-LIST*)))))
        (T (NCONS (TV:SCROLL-PARSE-ITEM "Host data unavailable")))))

(DEFUN PEEK-HOSTAT-STREAM (OP &OPTIONAL ARG1 &REST REST)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYO :READ-CURSORPOS :SET-CURSORPOS))
    (:TYO
     (COND ((= ARG1 #/CR)
            (AND *PEEK-HOSTAT-STRING*
                 (PUSH *PEEK-HOSTAT-STRING* *PEEK-HOSTAT-LIST*))
            (SETQ *PEEK-HOSTAT-STRING* (MAKE-STRING 50. :FILL-POINTER 0))
            (PEEK-HOSTAT-STREAM ':SET-CURSORPOS *PEEK-HOSTAT-INDENT*))
           (T
            (ARRAY-PUSH-EXTEND *PEEK-HOSTAT-STRING* ARG1))))
    (:READ-CURSORPOS (STRING-LENGTH *PEEK-HOSTAT-STRING*))
    (:SET-CURSORPOS
     (LET ((SPACES (- ARG1 (STRING-LENGTH *PEEK-HOSTAT-STRING*))))
       (AND (> SPACES 0)
            (DOTIMES (I SPACES) (PEEK-HOSTAT-STREAM ':TYO #/SPACE)))))
    (T (STREAM-DEFAULT-HANDLER 'PEEK-HOSTAT-STREAM OP ARG1 REST))))

(DEFUN PEEK-CHAOS-CONN-RECEIVED-PKTS (ITEM &OPTIONAL (INDENT 0) &AUX CONN)
  "Show//unshow the received pkts of the connection"
  (OR (SETQ CONN (GETF (TV:SCROLL-FLAGS ITEM) ':CONNECTION))
      (FERROR NIL "~S has no associated connection, can't display packets." ITEM))
  (COND ((NOT (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET))
         ;; Want to leave state alone
         )
        ((CDR (TV:SCROLL-ITEMS ITEM))
         ;; Remove display
         (RPLACD (TV:SCROLL-ITEMS ITEM) NIL))
        (T
         ;; Add display
         (RPLACD (TV:SCROLL-ITEMS ITEM)
                 (NCONS
                   (TV:SCROLL-MAINTAIN-LIST `(LAMBDA () (READ-PKTS ',CONN))
                                            `(LAMBDA (X)
                                               (PEEK-CHAOS-PACKET-ITEM X ,(+ INDENT 2)))
                                            NIL
                                            #'(LAMBDA (STATE)
                                                (PROG ()
                                                      (RETURN (VALUES STATE
                                                                      (PKT-LINK STATE)
                                                                      (NULL (PKT-LINK STATE)))))))))))
  (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET) NIL))

(DEFUN PEEK-CHAOS-CONN-SEND-PKTS (ITEM &OPTIONAL (INDENT 0) &AUX CONN)
  "Show//unshow the send pkts of the connection"
  (OR (SETQ CONN (GETF (TV:SCROLL-FLAGS ITEM) ':CONNECTION))
      (FERROR NIL "~S has no associated connection, can't display packets." ITEM))
  (COND ((NOT (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET))
         ;; Want to leave state alone
         )
        ((CDR (TV:SCROLL-ITEMS ITEM))
         ;; Remove display
         (RPLACD (TV:SCROLL-ITEMS ITEM) NIL))
        (T
         ;; Add display
         (RPLACD (TV:SCROLL-ITEMS ITEM)
                 (NCONS
                   (TV:SCROLL-MAINTAIN-LIST `(LAMBDA () (SEND-PKTS ',CONN))
                                            `(LAMBDA (X)
                                               (PEEK-CHAOS-PACKET-ITEM X ,(+ INDENT 2)))
                                            NIL
                                            #'(LAMBDA (STATE)
                                                (PROG ()
                                                      (RETURN (VALUES STATE (PKT-LINK STATE)
                                                                      (NULL (PKT-LINK STATE)))))))))))
    (SETF (ARRAY-LEADER (FIRST (TV:SCROLL-ITEMS ITEM)) TV:SCROLL-ITEM-LEADER-OFFSET) NIL))

(DEFUN HOST-CHAOS-PEEK-FILE-SYSTEM-HEADER (ACCESS HOST)
  (LIST '(:PRE-PROCESS-FUNCTION PEEK-CHAOS-CONN-INSERT-HOSTAT)
        (TV:SCROLL-PARSE-ITEM
          ':LEADER 3
          `(:MOUSE-ITEM
             (NIL :EVAL (PEEK-CHAOS-HOST-MENU ',HOST 'TV:ITEM 0)
                  :DOCUMENTATION "Menu of useful things to do to this host.")
             :STRING ,(FORMAT NIL "~A" ACCESS)))))

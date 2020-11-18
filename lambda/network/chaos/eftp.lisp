;-*- Mode:LISP; Package:CHAOS; Base:8; readtable: ZL -*-

;;; This file implements EFTP on the Lisp machine,
;;; with the cooperation of CHSNCP, using the Chaosnet foreign-protocol protocol.

(DEFCONST PUP-NON-DATA-BYTES 22.)       ;10. words of header and a checksum
(DEFCONST MAX-PUP-DATA-BYTES (- MAX-DATA-BYTES-PER-PKT PUP-NON-DATA-BYTES))
(DEFCONST PUP-PROTOCOL-ID 100001)

;;; Structure of a PUP in a Chaosnet packet
;; Cannot use (:INCLUDE PKT) because PKT defstruct has some garbage at the end
(DEFSTRUCT (PUP :ARRAY (:CONSTRUCTOR NIL) (:ALTERANT NIL)
                (:INITIAL-OFFSET #.FIRST-DATA-WORD-IN-PKT) (:SIZE-SYMBOL PUP-FIRST-DATA-WORD))
  (PUP-OVERALL-LENGTH)
  ((PUP-TYPE #o0010) (PUP-TRANSPORT #o1010))
  (PUP-ID-HIGH)
  (PUP-ID-LOW)
  (PUP-DEST-HOST)
  (PUP-DEST-PORT-HIGH)
  (PUP-DEST-PORT-LOW)
  (PUP-SOURCE-HOST)
  (PUP-SOURCE-PORT-HIGH)
  (PUP-SOURCE-PORT-LOW))        ;Data follow, then checksum

;;; Get a PUP buffer which can be filled in then transmitted via TRANSMIT-PUP
(DEFUN GET-PUP (CONN PUP-TYPE PUP-ID
                &AUX (PKT (GET-PKT)))
  (COPY-ARRAY-PORTION PKT 0 0 PKT 0 (ARRAY-LENGTH PKT)) ;Clear to zero
  (SETF (PUP-TYPE PKT) PUP-TYPE)
  (SETF (PUP-ID-HIGH PKT) (LDB 2020 PUP-ID))
  (SETF (PUP-ID-LOW PKT) (LDB 0020 PUP-ID))
  (SETF (PUP-DEST-HOST PKT) (FOREIGN-ADDRESS CONN))
  (SETF (PUP-DEST-PORT-HIGH PKT) (LDB 2020 (FOREIGN-INDEX-NUM CONN)))
  (SETF (PUP-DEST-PORT-LOW PKT) (LDB 0020 (FOREIGN-INDEX-NUM CONN)))
  (SETF (PUP-SOURCE-HOST PKT) MY-ADDRESS)
  (SETF (PUP-SOURCE-PORT-LOW PKT) (LOCAL-INDEX-NUM CONN))
  PKT)

;;; The header of a PUP is words and the data portion is bytes.
;;; The bytes are already in Lisp machine order, but the header needs to be fixed.
(DEFUN SWAB-PUP (PUP)
  (LOOP FOR I FROM FIRST-DATA-WORD-IN-PKT BELOW PUP-FIRST-DATA-WORD
        AS WD = (AREF PUP I)
        DO (ASET (DPB WD 1010 (LDB 1010 WD)) PUP I))
  PUP)

;;; Accessor for binary data in a PUP
(DEFUN PUP-WORD (PUP I)
  (LET ((WD (AREF PUP (+ PUP-FIRST-DATA-WORD I))))
    (DPB WD 1010 (LDB 1010 WD))))

(defsetf pup-word pup-store-word)
;(DEFPROP PUP-WORD ((PUP-WORD PUP I) . (PUP-STORE-WORD PUP I SI:VAL)) SETF)
(DEFUN PUP-STORE-WORD (PUP I WD)
  (ASET (DPB WD 1010 (LDB 1010 WD)) PUP (+ PUP-FIRST-DATA-WORD I)))

;;; Compute the checksum of a PUP
(DEFUN CHECKSUM-PUP (PKT)
  (DO ((I -10. (1+ I))
       (CK 0)
       (N (LSH (1- (PKT-NBYTES PKT)) -1) (1- N)))
      ((ZEROP N)
       (AND (= CK 177777) (SETQ CK 0))          ;Gronk minus zero
       (RETURN (values CK I)))                          ;Return checksum and index in PUP of cksm
    (SETQ CK (+ CK (PUP-WORD PKT I)))           ;1's complement add
    (AND (BIT-TEST 200000 CK) (SETQ CK (LDB 0020 (1+ CK))))
    (SETQ CK (DPB CK 0117 (LDB 1701 CK)))))     ;16-bit left rotate

;;; Fire off a PUP previously gotten from GET-PUP
(DEFUN TRANSMIT-PUP (CONN PKT N-BYTES)
  (SETF (PKT-NBYTES-on-write PKT) (+ PUP-NON-DATA-BYTES N-BYTES))
  (SETF (PUP-OVERALL-LENGTH PKT) (+ PUP-NON-DATA-BYTES N-BYTES))
  (SETF (PKT-ACK-NUM PKT) PUP-PROTOCOL-ID)
  (SWAB-PUP PKT)
  (MULTIPLE-VALUE-BIND (CKSM CKSMX) (CHECKSUM-PUP PKT)
    (SETF (PUP-WORD PKT CKSMX) CKSM))
  (SEND-UNC-PKT CONN PKT)
  (SWAB-PUP PKT))       ;Put back in case caller retransmits it

;;; Internal routine to get back a PUP on a specified port, with timeout
;;; Returns PKT or NIL.
(DEFUN RECEIVE-PUP (CONN &OPTIONAL (TIMEOUT 60.))
  (LOOP WITH START-TIME = (TIME)
        AS PUP = (GET-NEXT-PKT CONN T)
        WHEN PUP
          IF (AND (= (PKT-OPCODE PUP) UNC-OP)
                  (= (PKT-ACK-NUM PUP) PUP-PROTOCOL-ID)
                  (MULTIPLE-VALUE-BIND (CKSM CKSMX) (CHECKSUM-PUP PUP)
                    (LET ((CK (PUP-WORD PUP CKSMX)))
                      (OR (= CK 177777) (= CK CKSM)))))
          RETURN (SWAB-PUP PUP)
          ELSE DO (RETURN-PKT PUP)
        DO (PROCESS-WAIT "PUP in"
                         #'(LAMBDA (CONN START-TIME TIMEOUT)
                             (OR (READ-PKTS CONN)
                                 (> (TIME-DIFFERENCE (TIME) START-TIME) TIMEOUT)))
                         CONN START-TIME TIMEOUT)
        UNTIL (> (TIME-DIFFERENCE (TIME) START-TIME) TIMEOUT)))

;Cons a string containing characters taken from a PUP
(DEFUN PUP-STRING (PUP &OPTIONAL (FROM 0) (TO (- (PUP-OVERALL-LENGTH PUP)
                                                 PUP-NON-DATA-BYTES)))
  (SUBSTRING (PKT-STRING PUP) (+ 20. FROM) (+ 20. TO))) ;20. is bytes in pup header

;Complain about random PUP we may have received, and free the PKT
;Put a trace breakpoint on this if you are trying to figure out what's going on.
(DEFUN RECEIVED-RANDOM-PUP (PUP)
  #-REL5
  (FORMAT ERROR-OUTPUT
          "~&[Random PUP type ~O received from ~O#~O#~O~:[~;, code ~D. <~O>, ~A~]]~%"
          (PUP-TYPE PUP)
          (LDB 1010 (PUP-SOURCE-HOST PUP))
          (LDB 0010 (PUP-SOURCE-HOST PUP))
          (DPB (PUP-SOURCE-PORT-HIGH PUP) 2020 (PUP-SOURCE-PORT-LOW PUP))
          (= (PUP-TYPE PUP) 4)          ;Error
          (PUP-WORD PUP 10.)            ;Standard code
          (PUP-WORD PUP 11.)            ;Misc argument to it
          (PUP-STRING PUP 24.))         ;Human readable text
  (RETURN-PKT PUP))

;EFTP-write stream.
(DEFVAR EFTP-NEXT-PUP-ID)
(DEFVAR EFTP-CONN)
(DEFVAR EFTP-BINARY-P)
(DEFVAR EFTP-BUFFER)

(DEFUN MAKE-EFTP-WRITE-STREAM (FOREIGN-HOST
                               &OPTIONAL (EFTP-BINARY-P NIL) (FOREIGN-PORT 20))
  "Return a stream which sends output to ethernet host FOREIGN-HOST via EFTP.
EFTP-BINARY-P non-NIL means sending 16-bit data, otherwise 8-bit.
FOREIGN-PORT is the ethernet port to contact on that machine."
  (LET ((EFTP-NEXT-PUP-ID 0)
        (EFTP-BUFFER (MAKE-ARRAY MAX-PUP-DATA-BYTES ':TYPE 'ART-8B ':LEADER-LIST '(0)))
        (EFTP-CONN (OPEN-FOREIGN-CONNECTION FOREIGN-HOST FOREIGN-PORT)))
    (CLOSURE '(EFTP-NEXT-PUP-ID EFTP-CONN EFTP-BINARY-P EFTP-BUFFER)
             'EFTP-WRITE-STREAM)))

(DEFUN EFTP-WRITE-STREAM (OP &OPTIONAL ARG1 &REST ARGS)
  (SELECTQ OP
    (:WHICH-OPERATIONS (IF EFTP-BINARY-P
                           '(:TYO :STRING-OUT :FORCE-OUTPUT :CLOSE)
                           '(:TYO :STRING-OUT :LINE-OUT :FORCE-OUTPUT :CLOSE)))
    (:TYO (ARRAY-PUSH EFTP-BUFFER ARG1)
          (AND (= (ARRAY-ACTIVE-LENGTH EFTP-BUFFER) (ARRAY-LENGTH EFTP-BUFFER))
               (EFTP-FORCE-OUTPUT)))
    (:LINE-OUT (LEXPR-FUNCALL #'EFTP-WRITE-STREAM ':STRING-OUT ARG1 ARGS)
               (EFTP-WRITE-STREAM ':TYO 15)
               (EFTP-WRITE-STREAM ':TYO 12))
    (:STRING-OUT        ;Could be coded more efficiently, but why bother?
      (LET ((FROM (OR (CAR ARGS) 0))
            (TO (OR (CADR ARGS) (ARRAY-ACTIVE-LENGTH ARG1))))
        (DO ((I FROM (1+ I))
             (CH))
            (( I TO))
          (SETQ CH (AREF ARG1 I))
          (COND (EFTP-BINARY-P)
                ((= CH #\TAB) (SETQ CH 11))
                ((= CH #\CR)
                 (EFTP-WRITE-STREAM ':TYO 15)
                 (SETQ CH 12)))
          (ARRAY-PUSH EFTP-BUFFER CH)
          (AND (= (ARRAY-ACTIVE-LENGTH EFTP-BUFFER) (ARRAY-LENGTH EFTP-BUFFER))
               (EFTP-FORCE-OUTPUT)))))
    (:FORCE-OUTPUT (EFTP-FORCE-OUTPUT))
    (:CLOSE (EFTP-FORCE-OUTPUT)
            (DO ((ID (1- (SETQ EFTP-NEXT-PUP-ID (1+ EFTP-NEXT-PUP-ID))))
                 (N-RETRANSMISSIONS 1 (1+ N-RETRANSMISSIONS))
                 (PUP))
                (NIL)
              (SETQ PUP (GET-PUP EFTP-CONN 32 ID))
              (TRANSMIT-PUP EFTP-CONN PUP 0)
              (COND ((NULL (SETQ PUP (RECEIVE-PUP EFTP-CONN)))
                     (AND (ZEROP (\ N-RETRANSMISSIONS 10.))
                          (FORMAT ERROR-OUTPUT
                                  "~&[Host not responding to EFTP_End, still trying...]~%")))
                    ((= (PUP-TYPE PUP) 33)
                     (FORMAT ERROR-OUTPUT "~&EFTP Abort in EFTP_End, code ~D, ~A~%"
                             (PUP-WORD PUP 0) (PUP-STRING PUP 2))
                     (RETURN-PKT PUP)
                     (BREAK 'EFTP-ABORT))
                    ((NOT (= (PUP-TYPE PUP) 31))
                     (RECEIVED-RANDOM-PUP PUP))
                    ((NOT (= (DPB (PUP-ID-HIGH PUP) 2020 (PUP-ID-LOW PUP)) ID))
                     (RETURN-PKT PUP))          ;Ignore random old acks
                    (T (RETURN-PKT PUP)         ;Good ack
                       (RETURN NIL))))
            (TRANSMIT-PUP EFTP-CONN (GET-PUP EFTP-CONN 32 EFTP-NEXT-PUP-ID) 0)
            (REMOVE-CONN EFTP-CONN))
    (OTHERWISE (STREAM-DEFAULT-HANDLER #'EFTP-WRITE-STREAM OP ARG1 ARGS))))

(DEFUN EFTP-FORCE-OUTPUT ()
  (AND (NOT (ZEROP (ARRAY-ACTIVE-LENGTH EFTP-BUFFER)))
       (DO ((ID (1- (SETQ EFTP-NEXT-PUP-ID (1+ EFTP-NEXT-PUP-ID))))
            (N-RETRANSMISSIONS 1 (1+ N-RETRANSMISSIONS))
            (PUP))
           (NIL)
         (SETQ PUP (GET-PUP EFTP-CONN 30 ID))
         (DOTIMES (I (TRUNCATE (1+ (ARRAY-ACTIVE-LENGTH EFTP-BUFFER)) 2))
           (ASET (DPB (AREF EFTP-BUFFER (1+ (* I 2))) 1010 (AREF EFTP-BUFFER (* I 2)))
                 PUP (+ I PUP-FIRST-DATA-WORD)))
         (TRANSMIT-PUP EFTP-CONN PUP (ARRAY-ACTIVE-LENGTH EFTP-BUFFER))
         (COND ((NULL (SETQ PUP (RECEIVE-PUP EFTP-CONN)))
                (AND (ZEROP (\ N-RETRANSMISSIONS 10.))
                     (FORMAT ERROR-OUTPUT "~&[Host not responding, still trying...]~%")))
               ((= (PUP-TYPE PUP) 33)
                (FORMAT ERROR-OUTPUT "~&EFTP Abort code ~D, ~A~%"
                        (PUP-WORD PUP 0) (PUP-STRING PUP 2))
                (RETURN-PKT PUP)
                (BREAK 'EFTP-ABORT))
               ((NOT (= (PUP-TYPE PUP) 31))
                (RECEIVED-RANDOM-PUP PUP))
               ((NOT (= (DPB (PUP-ID-HIGH PUP) 2020 (PUP-ID-LOW PUP)) ID))
                (RETURN-PKT PUP))               ;Ignore random old acks
               (T (RETURN-PKT PUP)              ;Good ack
                  (RETURN NIL)))))              ;Bingo!
  (STORE-ARRAY-LEADER 0 EFTP-BUFFER 0)
  T)

(DEFVAR EFTP-UNRCHF)

(DEFUN MAKE-EFTP-READ-STREAM (FOREIGN-HOST
                               &OPTIONAL (EFTP-BINARY-P NIL) #-REL5 (LOCAL-PORT 20))
  "Return a stream which reads input from ethernet host FOREIGN-HOST via EFTP.
EFTP-BINARY-P non-NIL means receiving 16-bit data, otherwise 8-bit.
LOCAL-PORT is the ethernet port to use on this machine."
  (LET ((EFTP-NEXT-PUP-ID 0)
        (EFTP-CONN (OPEN-FOREIGN-CONNECTION FOREIGN-HOST 0 #-REL5 10. #-REL5 LOCAL-PORT))
        (EFTP-UNRCHF NIL)
        (EFTP-BUFFER (MAKE-ARRAY MAX-PUP-DATA-BYTES ':TYPE 'ART-8B ':LEADER-LIST '(0 0))))
    (CLOSURE '(EFTP-CONN EFTP-UNRCHF EFTP-NEXT-PUP-ID EFTP-BINARY-P EFTP-BUFFER)
             'EFTP-READ-STREAM)))

(DEFUN EFTP-READ-STREAM (OP &OPTIONAL ARG1 &REST ARGS)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:TYI :UNTYI :CLOSE))
    (:TYI (COND (EFTP-UNRCHF
                  (PROG1 EFTP-UNRCHF (SETQ EFTP-UNRCHF NIL)))
                ((< (ARRAY-LEADER EFTP-BUFFER 1) (ARRAY-LEADER EFTP-BUFFER 0))
                  (SETF (ARRAY-LEADER EFTP-BUFFER 1) (1+ (ARRAY-LEADER EFTP-BUFFER 1)))
                  (LET ((CH (AREF EFTP-BUFFER (1- (ARRAY-LEADER EFTP-BUFFER 1)))))
                    (COND ((NOT EFTP-BINARY-P)
                           (COND ((MEMQ CH '(11 14 15))
                                  (SETQ CH (+ CH 200)))
                                 ((= CH 12)
                                  (SETQ CH (EFTP-READ-STREAM OP ARG1))))))
                    CH))
                ((AND EFTP-CONN (EFTP-READ-NEXT-PUP))
                  (EFTP-READ-STREAM OP ARG1))
                (T ;Eof
                  (REMOVE-CONN EFTP-CONN)
                  (SETQ EFTP-CONN NIL)          ;Flag as eof
                  (AND ARG1 (FERROR #+CADR 'SYS:END-OF-FILE-1
                                    #+3600 'SI:END-OF-FILE-1 "End of file on ~S."
                                    'EFTP-READ-STREAM)))))
    (:UNTYI (SETQ EFTP-UNRCHF ARG1))
    (:CLOSE (REMOVE-CONN EFTP-CONN))
    (OTHERWISE (STREAM-DEFAULT-HANDLER #'EFTP-READ-STREAM OP ARG1 ARGS))))

(DEFUN EFTP-READ-NEXT-PUP ()
  "Returns NIL at eof, else sets up buffer"
  ;; EFTP-NEXT-PUP-ID has the number of the packet we are expecting to receive here
  (AND (PLUSP EFTP-NEXT-PUP-ID)         ;Not first time, acknowledge previous packet
       (TRANSMIT-PUP EFTP-CONN (GET-PUP EFTP-CONN 31 (1- EFTP-NEXT-PUP-ID)) 0))
  (DO ((N-TIMEOUTS 1 (1+ N-TIMEOUTS))
       (EOF-SEQUENCE-P NIL)
       (PUP))
      (NIL)                             ;Loop until receive data
    (COND ((NULL (SETQ PUP (RECEIVE-PUP EFTP-CONN)))
           (COND ((ZEROP (\ N-TIMEOUTS 10.))
                  (AND EOF-SEQUENCE-P (RETURN NIL))     ;Done with dally timeout
                  (FORMAT ERROR-OUTPUT
                          (IF (PLUSP EFTP-NEXT-PUP-ID)
                              "~&[Host has stopped sending, still trying...]~%"
                              "~&[Host has not started sending, still trying...]~%")))))
          ((NOT (AND (OR (= (PUP-TYPE PUP) 30) (= (PUP-TYPE PUP) 32) (= (PUP-TYPE PUP) 33))
                     (= (PUP-SOURCE-HOST PUP) (FOREIGN-ADDRESS EFTP-CONN))
                     (OR (ZEROP EFTP-NEXT-PUP-ID)
                         (= (DPB (PUP-SOURCE-PORT-HIGH PUP) 2020 (PUP-SOURCE-PORT-LOW PUP))
                            (FOREIGN-INDEX-NUM EFTP-CONN)))))
           (RECEIVED-RANDOM-PUP PUP))
          ((= (PUP-TYPE PUP) 33)
           (FORMAT ERROR-OUTPUT "~&EFTP Abort~:[~; in eof sequence~], code ~D, ~A~%"
                   EOF-SEQUENCE-P (PUP-WORD PUP 0) (PUP-STRING PUP 2))
           (RETURN-PKT PUP)
           (BREAK 'EFTP-ABORT))
          ((NOT (= (DPB (PUP-ID-HIGH PUP) 2020 (PUP-ID-LOW PUP))
                   EFTP-NEXT-PUP-ID))
           (RETURN-PKT PUP)             ;Ignore random old data
           (AND (PLUSP EFTP-NEXT-PUP-ID);Except repeat acknowledgement
                (TRANSMIT-PUP EFTP-CONN (GET-PUP EFTP-CONN 31 (1- EFTP-NEXT-PUP-ID)) 0)))
          ((= (PUP-TYPE PUP) 32)        ;Eof
           (RETURN-PKT PUP)
           (AND EOF-SEQUENCE-P (RETURN NIL))    ;Done dallying
           (SETQ EOF-SEQUENCE-P T)      ;Ack the EFTP-END packet
           (TRANSMIT-PUP EFTP-CONN (GET-PUP EFTP-CONN 31 EFTP-NEXT-PUP-ID) 0)
           (SETQ EFTP-NEXT-PUP-ID (1+ EFTP-NEXT-PUP-ID)))
          (T                            ;Incoming data
           (AND (> N-TIMEOUTS 9)
                (FORMAT ERROR-OUTPUT "~&[Host has commenced transmission]~%"))
           (AND (ZEROP EFTP-NEXT-PUP-ID)
                (SETF (FOREIGN-INDEX-NUM EFTP-CONN)
                      (DPB (PUP-SOURCE-PORT-HIGH PUP) 2020 (PUP-SOURCE-PORT-LOW PUP))))
           (SETF (ARRAY-LEADER EFTP-BUFFER 1) 0)
           (SETF (ARRAY-LEADER EFTP-BUFFER 0) (- (PUP-OVERALL-LENGTH PUP) PUP-NON-DATA-BYTES))
           (DOTIMES (I (TRUNCATE (1+ (ARRAY-ACTIVE-LENGTH EFTP-BUFFER)) 2))
             (LET ((WD (AREF PUP (+ I PUP-FIRST-DATA-WORD))))
               (ASET (LDB 0010 WD) EFTP-BUFFER (* I 2))
               (ASET (LDB 1010 WD) EFTP-BUFFER (1+ (* I 2)))))
           (RETURN-PKT PUP)
           (SETQ EFTP-NEXT-PUP-ID (1+ EFTP-NEXT-PUP-ID))
           (RETURN T)))))

(DEFUN EFTP-BINARY-FILE-TO-ALTO (FILENAME ALTO-ADDRESS)
  "Send binary file FILENAME to Alto with host-number ALTO-ADDRESS.
You must run the EFTP program on that Alto and tell it this
Lisp machine's chaosnet address, and where to write the data it receives."
  (WITH-OPEN-FILE (IN FILENAME '(:READ :FIXNUM :BYTE-SIZE 8))
    (LET ((OUT (MAKE-EFTP-WRITE-STREAM ALTO-ADDRESS T)))
      (STREAM-COPY-UNTIL-EOF IN OUT)
      (FUNCALL OUT ':CLOSE))))

(DEFUN EFTP-BINARY-FILE-FROM-ALTO (FILENAME ALTO-ADDRESS)
  "Receive binary file FILENAME to Alto with host-number ALTO-ADDRESS.
You must run the EFTP program on that Alto and tell it this
Lisp machine's chaosnet address, and what local file to transmit."
  (WITH-OPEN-FILE (OUT FILENAME '(:WRITE :FIXNUM :BYTE-SIZE 8))
    (LET ((IN (MAKE-EFTP-READ-STREAM ALTO-ADDRESS T)))
      (STREAM-COPY-UNTIL-EOF IN OUT))))

(DEFUN EFTP-TEXT-FILE-FROM-ALTO (FILENAME ALTO-ADDRESS)
  "Receive text file FILENAME from Alto with host-number ALTO-ADDRESS.
You must run the EFTP program on that Alto and tell it this
Lisp machine's chaosnet address, and what local file to transmit."
  (WITH-OPEN-FILE (OUT FILENAME '(:WRITE))
    (LET ((IN (MAKE-EFTP-READ-STREAM ALTO-ADDRESS)))
      (STREAM-COPY-UNTIL-EOF IN OUT))))

(DEFUN EFTP-TEXT-FILE-TO-ALTO (FILENAME ALTO-ADDRESS)
  "Send text file FILENAME to Alto with host-number ALTO-ADDRESS.
You must run the EFTP program on that Alto and tell it this
Lisp machine's chaosnet address, and where to write the data it receives."
  (WITH-OPEN-FILE (IN FILENAME '(:READ))
    (LET ((OUT (MAKE-EFTP-WRITE-STREAM ALTO-ADDRESS)))
      (STREAM-COPY-UNTIL-EOF IN OUT)
      (FUNCALL OUT ':CLOSE))))

;; -*- Mode:LISP; Package:CHAOS; Base:8; Readtable:ZL -*-

;;; TESTING FUNCTIONS:

;;;   SET-BASE-ADDRESS - sets the base unibus address for the network interface
;;;                      (defaults to 764140)
;;;   CHATST         - send to self with and without loopback printing the results,
;;;                    using pattern set by SET-PATTERN
;;;   CHATST-LOOP    - send packets to another host (defaults to MC) which will echo it back
;;;                    (useful for scope loops)
;;;   CHATST-MONITOR - looks at everying flowing on the network
;;;   CHATST-STATUS  - prints the status of the network interface, interpreting the CSR
;;;   CHATST-RESET   - resets the network interface
;;;   CHATST-ECHO    -
;;;   CHATST-ECHO-ONCE
;;;   CHATST-SOAK
;;;   SET-NCP-BASE-ADDRESS - Sets the device address used by the NCP so that the
;;;                          interface to be tested can be tried in full service.
;;;                          NOTE!!!! A bus grant jumper must be run to the board you are
;;;                          debugging in order for interrupts to work!

(DEFVAR CHATST-USE-DEBUG NIL)   ;if T, everything refers to machine on debug interface
(DEFVAR CHATST-PACKET-LENGTH 20)        ;Packet length chatst sends

;;;  **** NOTE *****
;;;  Here are some typical screws encountered in testing chaos boards:
;;;  If you get a CRC error, but the contents of the packet is NOT printed out,
;;;  this means the data came back correctly, but a CRC error was indicated
;;;  (often implying a bad CRC generator chip).
;;;  MAKE SURE AND TEST WITH SEVERAL PATTERNS!!  Certain patterns (e.g all zeros)
;;;  will not show certain errors.

(DECLARE (SPECIAL
   ;;; hardware related specials
        CONTROL-STATUS-REGISTER-TEST    ;the control-status register
        MY-NUMBER-REGISTER-TEST ;the cable address register
        WRITE-BUFFER-REGISTER-TEST      ;the write-data register
        READ-BUFFER-REGISTER-TEST       ;the read-data register
        BIT-COUNT-REGISTER-TEST ;the bit count register
        INITIATE-TRANSFER-REGISTER-TEST ;the start transfer register
        INTERVAL-TIMER-REGISTER-TEST    ;start the interval timer
        CHATST-PATTERN
        CHATST-ADDRESS                  ;Host address of interface we're testing
))


;;;Format of control register
;;; 1           ;XMT BUSY
;;; 2           ;LOOP BACK
;;; 4           ;RECEIVE-ALL
;;; 10          ;RESET-RECEIVE
;;; 20          ;RCV INTERRUPT ENABLE
;;; 40          ;TRANSMIT INTERRUPT ENABLE
;;; 100         ;TRANSMIT ABORT
;;; 200         ;TRANSMIT DONE
;;; 400         ;TRANSMIT RESET
;;; 17000       ;LOST COUNT
;;; 20000       ;IO RESET
;;; 40000       ;CRC ERROR
;;; 10000       ;RCV DONE


(DEFMACRO INITIATE-PACKET-TRANSMISSION ()
  '(%U-READ INITIATE-TRANSFER-REGISTER-TEST))

(DEFUN SET-BASE-ADDRESS (&OPTIONAL (BASE-ADDRESS 764140))
    "Set the base UNIBUS address for the Chaos net device.
Argument is optional and defaults to 764140.  Defines various
special variables and read and prints the host address of
the device at the specified address."

    (SETQ CONTROL-STATUS-REGISTER-TEST BASE-ADDRESS
          MY-NUMBER-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-MY-NUMBER-OFFSET 1))
          WRITE-BUFFER-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-WRITE-BUFFER-OFFSET 1))
          READ-BUFFER-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-READ-BUFFER-OFFSET 1))
          BIT-COUNT-REGISTER-TEST (+ BASE-ADDRESS (LSH %CHAOS-BIT-COUNT-OFFSET 1))
          INITIATE-TRANSFER-REGISTER-TEST
          (+ BASE-ADDRESS (LSH %CHAOS-START-TRANSMIT-OFFSET 1))
          INTERVAL-TIMER-REGISTER-TEST
          (+ BASE-ADDRESS 20))
    (FORMAT T "~%My number: ~O" (setq chatst-address (%u-read MY-NUMBER-REGISTER-TEST))))

(DEFVAR CHATST-PATTERN (MAKE-ARRAY 256. ':TYPE 'ART-16B))

(DEFVAR CHATST-PATTERN-TYPE 0)

(DEFUN SET-PATTERN (PAT)
  (SETQ CHATST-PATTERN-TYPE PAT)
  (DO I 0 (1+ I) (= I CHATST-PACKET-LENGTH)
    (AS-1 (COND ((EQ PAT 'FLOATING-ONE) (LSH 1 I))
                ((EQ PAT 'FLOATING-ZERO) (LOGXOR (LSH 1 I) -1))
                ((EQ PAT 'ADDRESS) I)
                ((NUMBERP PAT) PAT)
                ((ERROR "BAD PATTERN" I)))
          CHATST-PATTERN
          I)))

(SET-PATTERN 'FLOATING-ONE)                     ;REASONABLE DEFAULT

(DEFVAR CHATST-USE-RECEIVE-ALL T)               ;reasonable???

(DEFUN CHATST (&OPTIONAL (LOOPBACK-COUNT 4) (CABLE-COUNT 4))
    "Standard test function for the chaos network interface.
If it passes this test, sending and receiving packets from the network
probably works.  Use SET-NCP-BASE-ADDRESS to give it a full test.
Things not tested by this function include UNIBUS interrupts, bus grant
logic, etc.  This function cycles through several bit patterns, sending
4 packets with each pattern, both in loopback and out on the cable.
It does not send a properly formated packet with a header, but just
a packet of raw bits."
    (IF CHATST-USE-DEBUG (FORMAT T "~%Using debug interface"))
    (CHATST-RESET)
    (DOLIST (PAT '(FLOATING-ONE FLOATING-ZERO ADDRESS 52525 0 177777))
      (FORMAT T "~%Pattern:  ~A ~%Using Loopback ~%" PAT)
      (SET-PATTERN PAT)
      (LET ((CHATST-USE-RECEIVE-ALL T))
        (DOTIMES (I LOOPBACK-COUNT)
          (CHATST-PREP T) (CHATST-XMT) (CHATST-RCV)))
      (FORMAT T "~%Using the cable ~%")
      (LET ((CHATST-USE-RECEIVE-ALL NIL))
        (DOTIMES (I CABLE-COUNT)
          (CHATST-PREP NIL) (CHATST-XMT) (CHATST-RCV T)))))

(DEFUN CHATST-ONCE (&OPTIONAL (LOOPBACK NIL) (CHATST-USE-RECEIVE-ALL LOOPBACK))
  "Like CHATST, but only tries the currently defined pattern.  Call SET-PATTERN
to change the pattern."
  (CHATST-RESET)
  (FORMAT T "~%Loopback: ~A,  Pattern:  ~A" LOOPBACK CHATST-PATTERN-TYPE)
  (DO ((I 0 (1+ I)))
      ((= I 4))
    (CHATST-PREP NIL)
    (CHATST-XMT)
    (CHATST-RCV T)))

(DEFUN CHATST-TR-LOOP (&OPTIONAL LOOPBACK &AUX (CHATST-USE-RECEIVE-ALL LOOPBACK))
  (CHATST-RESET)
  (DO () ((SEND *TERMINAL-IO* :TYI-NO-HANG)) (CHATST-PREP LOOPBACK)
      (CHATST-XMT) (CHATST-RCV T)))

(DEFUN CHATST-XMT ()
  "Send a packet consisting of 16 words of selected pattern and my address."
  (DO ((I 0 (1+ I)))
      ((= I CHATST-PACKET-LENGTH))
    (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
  (%u-write WRITE-BUFFER-REGISTER-TEST (%U-READ MY-NUMBER-REGISTER-TEST))
  (%u-write CONTROL-STATUS-REGISTER-TEST        ;improve chances of avoiding an abort
            (LOGIOR 10 (%U-READ CONTROL-STATUS-REGISTER-TEST)))
  (INITIATE-PACKET-TRANSMISSION))

(DEFUN CHATST-PACKET (&OPTIONAL (CABLE-DEST 3040))      ;MC-11
    "Send a packet to some host (defaults to MC) which it will echo back."
  (DO () ((bit-test 200 (%U-READ CONTROL-STATUS-REGISTER-TEST)))) ;AWAIT TDONE
  (%u-write WRITE-BUFFER-REGISTER-TEST 100000)  ;DATA
  (%u-write WRITE-BUFFER-REGISTER-TEST 40)      ;NBYTES
  (%u-write WRITE-BUFFER-REGISTER-TEST 1440)    ;MC
  (%u-write WRITE-BUFFER-REGISTER-TEST 0)
  (%u-write WRITE-BUFFER-REGISTER-TEST chatst-address)  ;LISPM
  (DO ((I 0 (1+ I)))
      ((= I 3))                                 ;SEND THE PATTERN AS IDX, PKT, ACK
    (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
  (DO ((I 0 (1+ I)))
      ((= I CHATST-PACKET-LENGTH))              ;SEND THE PATTERN AS 40 BYTES OF DATA
    (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
  (%u-write WRITE-BUFFER-REGISTER-TEST CABLE-DEST)
  (INITIATE-PACKET-TRANSMISSION))


(DEFUN CHATST-LOOP (&OPTIONAL (CABLE-DEST 3040) (LOOP-BACK-P NIL))      ;MC-11, NO LOOPBACK
  "Scope loop, ignore what is received (defaults to mc)"
  (DO () ((SEND *TERMINAL-IO* :TYI-NO-HANG))
    (CHATST-PREP LOOP-BACK-P)
    (CHATST-PACKET CABLE-DEST)))

;;; Prepare the interface to receive.
(DEFUN CHATST-PREP (LOOPBACK-P)
  (%u-write CONTROL-STATUS-REGISTER-TEST
                 (+ (COND ((NOT LOOPBACK-P) 10) (T 12))
                    (COND ((NOT CHATST-USE-RECEIVE-ALL) 0) (T 4)))))

(DEFUN CHATST-RESET ()
  (%u-write CONTROL-STATUS-REGISTER-TEST 20000))

(SETQ INBUF (MAKE-ARRAY 256 ':TYPE 'ART-16B))
(DECLARE (SPECIAL INBUF))

;;; Look for a received packet, and complain in various ways.
(DEFUN CHATST-RCV ( &OPTIONAL BUSY-WAIT (CNT CHATST-PACKET-LENGTH) &AUX CSR TEM ME LOSE)
  (IF BUSY-WAIT
      (DO () ((LDB-TEST %%CHAOS-CSR-RECEIVE-DONE
                        (%U-READ CONTROL-STATUS-REGISTER-TEST))))
    (PROCESS-SLEEP 1.))  ;Give it time to arrive
  (SETQ CSR (%U-READ CONTROL-STATUS-REGISTER-TEST))
  (SETQ ME (%U-READ MY-NUMBER-REGISTER-TEST))
  (IF (LDB-TEST %%CHAOS-CSR-TRANSMIT-ABORT CSR)
      (FORMAT t "~%Transmit aborted, then~%"))
  (COND ((NOT (LDB-TEST %%CHAOS-CSR-RECEIVE-DONE CSR))
         (SETQ LOSE T) (PRINT 'NO-RECEIVE))
        (T (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR CSR)
                (PROGN (SETQ LOSE 'CRC)
                       (PRINT '"CRC Error indicated (check the data)")))
           (OR (= (%U-READ BIT-COUNT-REGISTER-TEST) (1- (* 16. (+ 3 CNT))))
               (PROGN (SETQ LOSE T)
                      (PRINT (LIST (%U-READ BIT-COUNT-REGISTER-TEST) 'BAD-BIT-COUNT))))
           (DO ((I 0 (1+ I)))
               ((= I CNT))
             (AS-1 (%U-READ READ-BUFFER-REGISTER-TEST) INBUF I))
           (IF ( (SETQ TEM (%U-READ READ-BUFFER-REGISTER-TEST)) ME)
               (PROGN (SETQ LOSE T)
                      (FORMAT T "~% DEST=~O SHOULD=~O" TEM ME)))
           (IF ( (SETQ TEM (%U-READ READ-BUFFER-REGISTER-TEST)) ME)
               (PROGN (SETQ LOSE T)
                      (FORMAT T "~% SOURCE=~O SHOULD=~O" TEM ME)))
           (DO ((I 0 (1+ I))
                (K))
               ((= I CNT) (IF LOSE (PRINT "Data returned was correct")))
             (SETQ K (AR-1 CHATST-PATTERN I))
             (COND (( K (AR-1 INBUF I))
                    (SETQ LOSE T)
                    (TERPRI) (PRINC "LOC    GOOD   BAD")
                    (DO I 0 (1+ I) (= I (min 10. CNT))
                      (FORMAT T "~%~2O  ~6O ~6O" I (AR-1 CHATST-PATTERN I) (AR-1 INBUF I)))
                    (RETURN NIL))))
           (%U-READ READ-BUFFER-REGISTER-TEST)  ;gobble the CRC word
           (IF (AND (NOT (EQ LOSE 'CRC))        ;don't bother pointing this out, if already
                    (LDB-TEST %%CHAOS-CSR-CRC-ERROR (%U-READ CONTROL-STATUS-REGISTER-TEST)))
               (PROGN (FORMAT T "~%CRC error indicated after data readout~:[ even though data is correct~]."
                              LOSE)
                      (SETQ LOSE T)))
           ))
  (OR LOSE (FORMAT T "~&WIN")))

;;; Monitor the Net for traffic

(DEFUN CHATST-MONITOR (&OPTIONAL (SHORT-P T) &AUX BITS cnt)
 "Monitor all network traffic.  This will often tell you if your interface or
  transceiver has trouble receiving packets from a particular host.  It all
  may tell you if something strange is happening on the network, such as
  a random host sending garbage packets, etc."
  (CHATST-RESET)
  (%u-write CONTROL-STATUS-REGISTER-TEST 14)        ;reset rcvr, RCV ALL
  (DO () ((SEND *TERMINAL-IO* :LISTEN) (SEND *TERMINAL-IO* :TYI-NO-HANG))
    (DO ((i 0 (1+ i)))
        ((> I 50.) (FORMAT T "."))
      (COND ((bit-test 100000 (%U-READ CONTROL-STATUS-REGISTER-TEST))
             (FORMAT T "~%---------------------~%")
             (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR (%U-READ CONTROL-STATUS-REGISTER-TEST))
                  (FORMAT T "CRC-Error "))
             (SETQ BITS (1+ (%U-READ BIT-COUNT-REGISTER-TEST))
                   CNT (FLOOR BITS 16.))
             (OR (ZEROP (\  BITS 16.))
                 (FORMAT T "Bad bit count, is ~O" BITS))
             (COND ((AND SHORT-P (> CNT 8))
                    (DO I 0 (1+ I) (= I 5)
                        (FORMAT T "~&~O   ~O" I (%U-READ READ-BUFFER-REGISTER-TEST)))
                    (FORMAT T "~%     ...")
                    (DO I 0 (1+ I) ( I (- CNT 8))(%U-READ READ-BUFFER-REGISTER-TEST))
                    (LET ((D (%U-READ READ-BUFFER-REGISTER-TEST))
                          (S (%U-READ READ-BUFFER-REGISTER-TEST))
                          (CRC  (%U-READ READ-BUFFER-REGISTER-TEST)))
                      (FORMAT T "~% dest ~O  ~A" D (SI:GET-HOST-FROM-ADDRESS D ':CHAOS))
                      (FORMAT T "~% src  ~O  ~A" S (SI:GET-HOST-FROM-ADDRESS S ':CHAOS))
                      (FORMAT T "~% CRC ~O" CRC)))
                   (T (DO I 0 (1+ I) (= I CNT)
                          (FORMAT T "~&~O   ~O" I (%U-READ READ-BUFFER-REGISTER-TEST)))))
             (%u-write CONTROL-STATUS-REGISTER-TEST 14)        ;reset rcvr, RCV ALL
             (RETURN NIL)))))
  (CHATST-RESET))


(DECLARE (SPECIAL CHATST-HEADER))
(SETQ CHATST-HEADER (MAKE-ARRAY 8 ':TYPE 'ART-16B))

(DEFUN CHATST-SET-HEADER NIL
   (AS-1 100000 CHATST-HEADER 0)                   ;OPCODE (DATA)
   (AS-1 0 CHATST-HEADER 1)                        ;LENGTH IN BYTES
   (AS-1 chatst-address CHATST-HEADER 2)           ;DESTINATION (CAUSE FORWARDING)
   (AS-1 0 CHATST-HEADER 3)
   (AS-1 chatst-address CHATST-HEADER 4)           ;SOURCE
   (DO I 0 (1+ I) (= I 3)                          ;SRC-IDX, PK#, ACK#
       (AS-1 (AR-1 CHATST-PATTERN I) CHATST-HEADER (+ I 5))))

(DEFUN CHATST-ECHO (&OPTIONAL (DEST 3040)  (LEN CHATST-PACKET-LENGTH))
  (CHATST-RESET)
  (CHATST-SET-HEADER)                           ;Setup an echo header
  (SETQ LEN (MIN LEN 248.))                     ;4096.-header
  (AS-1 (* LEN 2) CHATST-HEADER 1)
  (DO ((pat1 0 (1+ pat1))
       (pat2 (random) (random)))
      ((SEND *TERMINAL-IO* :TYI-NO-HANG))
    (%u-write CONTROL-STATUS-REGISTER-TEST 10)  ;reset rcvr
    (do i 0 (+ i 2) ( i len)
        (as-1 pat1 chatst-pattern i)
        (as-1 pat2 chatst-pattern (1+ i)))
    (format t "~%Patterns ~O, ~O" pat1 pat2)
    ;;Try this pattern 10. times
    (do ((j 0 (1+ j))) ((= j 10.))
      (DO ((i 0 (1+ i)))
          ((bit-test 200 (%U-READ CONTROL-STATUS-REGISTER-TEST)))       ;AWAIT TDONE
        (COND ((> i 50.)
               (FORMAT T "~% TDONE timeout")
               (RETURN NIL))))
      (DO ((I 0 (1+ I)))
          ((= I 8))                             ;Fill in IDX, PKT, ACK with pattern
        (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-HEADER I)))
      (DO ((I 0 (1+ I)))
          ((= I LEN))
        (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
      (%u-write CONTROL-STATUS-REGISTER-TEST 10)        ;reset rcvr
      (%u-write WRITE-BUFFER-REGISTER-TEST DEST)
      (INITIATE-PACKET-TRANSMISSION)
      (DO ((i 0 (1+ i)))
          ((OR (bit-test 100 (%U-READ CONTROL-STATUS-REGISTER-TEST))
               (> I 1000.))
           (IF (bit-test 100 (%U-READ CONTROL-STATUS-REGISTER-TEST))
               (FORMAT T "~%Transmit aborted.")
               (FORMAT T "~% Rcv-done timeout")))
        (COND ((BIT-TEST 100000 (%U-READ CONTROL-STATUS-REGISTER-TEST))
               (CHATST-CHECK-PK DEST LEN T)
               (RETURN NIL)))) )))

;;Scope trace - echo from some host

(DEFUN CHATST-BUZZ (&OPTIONAL (DEST 3040) (LEN CHATST-PACKET-LENGTH))
  (CHATST-RESET)
  (SETQ LEN (MIN LEN 248.))                     ;4096.-header
  (AS-1 (* LEN 2) CHATST-HEADER 1)
  (DO ()
      ((SEND *TERMINAL-IO* :TYI-NO-HANG)
       (CHATST-PRINT-STATUS DEST LEN))
    (as-1 (1+ (ar-1 chatst-pattern 0)) chatst-pattern 0)
    ;;Try this pattern 10. times
    (do ((j 0 (1+ j))) ((= j 10.))
      ;;Wait for Transmit side idle
      (DO ((i 0 (1+ i)))
          ((bit-test 200 (%U-READ CONTROL-STATUS-REGISTER-TEST)))
        (COND ((> i 50.)
               (FORMAT T "~% TDONE timeout")
               (RETURN NIL))))
      ;;Fill in header, data with pattern
      (DO ((I 0 (1+ I)))
          ((= I 8))
        (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-HEADER I)))
      (DO ((I 0 (1+ I)))
          ((= I LEN))
        (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
      (%u-write WRITE-BUFFER-REGISTER-TEST DEST)
      ;;Now wait for echoed packet
      (DO ((i 0 (1+ i)))
          ((> I 50.))
        (COND ((bit-test 100000 (%U-READ CONTROL-STATUS-REGISTER-TEST))
               (RETURN NIL))))
      (%u-write CONTROL-STATUS-REGISTER-TEST (IF (= DEST 0) 12 10))
      (INITIATE-PACKET-TRANSMISSION))))

(DEFUN CHATST-PRINT-STATUS ( &OPTIONAL (DEST 100) (LEN CHATST-PACKET-LENGTH))
  (TERPRI)
  (PROCESS-SLEEP 30.)  ;Give it time to arrive
  (CHATST-STATUS)                                           ;Decode status
  (CHATST-CHECK-PK DEST LEN)                             ;Check if any errors in PK
)


(DEFUN CHATST-CHECK-PK (&OPTIONAL (DEST-HOST 100) (CNT CHATST-PACKET-LENGTH) IGNORE-DEST-0
                        &AUX (CSR (%U-READ CONTROL-STATUS-REGISTER-TEST))
                             ME BITS BITS1 DEST SRC1 DEST1)
  (SETQ ME (%U-READ MY-NUMBER-REGISTER-TEST)
        BITS (1- (* 16. (+ 11. CNT)))
        BITS1 (%U-READ BIT-COUNT-REGISTER-TEST)
        DEST DEST-HOST
        SRC1 (%U-READ READ-BUFFER-REGISTER-TEST)
        DEST1 (%U-READ READ-BUFFER-REGISTER-TEST))
  (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR CSR)
       (PRINT 'CRC-ERROR))
  (DO I 0 (1+ I) (= I (+ 8 CNT))                ;skip first words of header
      (AS-1 (%U-READ READ-BUFFER-REGISTER-TEST) INBUF I))
  (COND ((AND IGNORE-DEST-0 (= DEST1 0)))
        (T (OR (= BITS1 BITS)
               (FORMAT T "~%Bad bit count, is ~O, should be ~O" BITS1 BITS))
           (OR (= DEST1 ME) (FORMAT T "~% DEST=~O, should be ~O"  DEST1 ME))
           (OR (= SRC1 DEST) (FORMAT T "~% SOURCE=~O, should be ~O"  SRC1 DEST))
           (AS-1 (LOGAND (AR-1 INBUF 1) 7777) INBUF 1)  ;FLUSH FORWARDING COUNT
           (DO I 0 (1+ I) (= I 8)
               (COND (( (AR-1 CHATST-HEADER I) (AR-1 INBUF I))
                      (TERPRI) (PRINC "HEADER  SENT    RCVD")
                      (DO I 0 (1+ I) (= I 8)
                          (FORMAT T "~%~2O  ~6O ~6O" I (AR-1 CHATST-HEADER I) (AR-1 INBUF I)))
                      (RETURN NIL))))
           (DO ((I 0 (1+ I)) (J 8 (1+ J))) ((= I CNT))
             (COND (( (AR-1 CHATST-PATTERN I) (AR-1 INBUF J))
                    (TERPRI) (PRINC "LOC    SENT    RCVD")
                    (DO ((I 0 (1+ I))(J 8 (1+ J))) ((= I CNT))
                      (FORMAT T "~%~2O  ~6O ~6O" I (AR-1 CHATST-PATTERN I) (AR-1 INBUF J)))
                    (RETURN NIL))))))
  (%u-write CONTROL-STATUS-REGISTER-TEST 10)    ;reset rcvr
           )

(DEFUN CHATST-ECHO-ONCE (&OPTIONAL (DEST 500) (LEN CHATST-PACKET-LENGTH))
  (DO ()((bit-test 200 (%U-READ CONTROL-STATUS-REGISTER-TEST))))
  (DO I 0 (1+ I) (= I LEN)                      ;SEND THE PATTERN AS 40 BYTES OF DATA
      (%u-write WRITE-BUFFER-REGISTER-TEST (AR-1 CHATST-PATTERN I)))
  (%u-write WRITE-BUFFER-REGISTER-TEST DEST)
  (%u-write CONTROL-STATUS-REGISTER-TEST 10)    ;reset rcvr
  (INITIATE-PACKET-TRANSMISSION)
  (DO ((i 0 (1+ i)))
      ((or (bit-test 200 (%U-READ CONTROL-STATUS-REGISTER-TEST))
           (> i 50.))))                         ;AWAIT TDONE
  (%u-write CONTROL-STATUS-REGISTER-TEST 14)    ;RCV ALL
  (CHATST-PRINT-STATUS DEST LEN))

(DEFUN CHATST-STATUS ( &AUX CSR LC)
  "Describes the bits currently on in the control status register for the
board being tested."
  (SETQ CSR (%U-READ CONTROL-STATUS-REGISTER-TEST))
  (FORMAT T "~2%CSR = ~O~%" CSR)
  (AND (LDB-TEST %%CHAOS-CSR-TIMER-INTERRUPT-ENABLE CSR)
       (FORMAT T "Timer interrupt enable. ?? ~%"))      ;This bit doesnt seem to do anything.
;    (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-BUSY CSR)
;        (FORMAT T "Transmit busy.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-LOOP-BACK CSR)
       (FORMAT T "Loopback.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-RECEIVE-ALL CSR)
       (FORMAT T "Receive all messages mode is on.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-RECEIVE-ENABLE CSR)
       (FORMAT T "Receiver interrupt enabled.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-ENABLE CSR)
       (FORMAT T "Transmit interrupt enabled.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-ABORT CSR)
       (FORMAT T "Transmit aborted by collision.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-TRANSMIT-DONE CSR)
       (FORMAT T "Transmit done.~%"))
  (OR  (ZEROP (SETQ LC (LDB %%CHAOS-CSR-LOST-COUNT CSR)))
       (FORMAT T "Lost count = ~O~%" LC))
  (AND (LDB-TEST %%CHAOS-CSR-RESET CSR)
       (FORMAT T "I//O reset.~%"))
  (AND (LDB-TEST %%CHAOS-CSR-CRC-ERROR CSR)
       (FORMAT T "==> CRC ERROR!!! <==~%"))
  (AND (LDB-TEST %%CHAOS-CSR-RECEIVE-DONE CSR)
       (FORMAT T "Receive done.~%"))
  (FORMAT T "Bit count: ~O~%" (%U-READ BIT-COUNT-REGISTER-TEST))
  NIL)

(DEFUN CHATST-SOAK (&AUX (M-ONES 0) (OTHERS 0))
  (%u-write control-status-register-test 14)
  (DO () ((SEND *TERMINAL-IO* :TYI-NO-HANG)
          (FORMAT T "~%-1 length packets ~O, others ~O" m-ones others))
    (COND ((bit-test 100000 (%U-READ CONTROL-STATUS-REGISTER-TEST))
;          (DO ((I 0 (1+ I))) ((> I 10.))
;            (FORMAT T "~%~O" (%U-READ CONTROL-STATUS-REGISTER-TEST)))
           (let ((tem (%u-read bit-count-register-test)))
             (if (= tem 7777)                   ;Null packet "received"
                 (setq m-ones (1+ m-ones))
                 (setq others (1+ others))))
               (%u-write control-status-register-test 14)))))


(DEFUN SET-NCP-BASE-ADDRESS (ADDR &AUX (OLD-CSR CONTROL-STATUS-REGISTER))
 "Set the base address that the NCP uses for all Chaos net functions.
NOTE!!!! A bus grant jumper must be run to the board you are debugging in
order for interrupts to work!  This function makes the board you are debugging
used for everything, rather than the default."
  (SET-BASE-ADDRESS ADDR)
  (SETQ BASE-ADDRESS ADDR
        CONTROL-STATUS-REGISTER BASE-ADDRESS
        MY-NUMBER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-MY-NUMBER-OFFSET 1))
        WRITE-BUFFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-WRITE-BUFFER-OFFSET 1))
        READ-BUFFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-READ-BUFFER-OFFSET 1))
        BIT-COUNT-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-BIT-COUNT-OFFSET 1))
        INITIATE-TRANSFER-REGISTER (+ BASE-ADDRESS (LSH %CHAOS-START-TRANSMIT-OFFSET 1)))

  (SETQ SI:%CHAOS-CSR-ADDRESS
        (SI:%MAKE-POINTER-UNSIGNED (+ 77400000 (LSH ADDR -1))))  ; SET THE A MEMORY LOCATION
  (INITIALIZE-NCP-SYSTEM)
  (%u-write OLD-CSR 20010)                      ;avoid interrupt hang screw
  (%u-write CONTROL-STATUS-REGISTER 20010)
  (FORMAT NIL "NCP now using ~6O as the network interface base address." ADDR))


(DEFUN TIMER-LOOP (&OPTIONAL (COUNT 511.) (SLEEP-TIME 1))
  "Scope loop for looking at the interval timer."
  (DO NIL ((SEND *TERMINAL-IO* :TYI-NO-HANG))
    (%u-write INTERVAL-TIMER-REGISTER-TEST COUNT)
    (PROCESS-SLEEP SLEEP-TIME)))

(DEFUN %U-READ (ADR)
  (IF CHATST-USE-DEBUG
      (CADR:DBG-READ ADR)
      (%UNIBUS-READ ADR)))

(DEFUN %U-WRITE (ADR DATA)
  (IF CHATST-USE-DEBUG
      (CADR:DBG-WRITE ADR DATA)
      (%UNIBUS-WRITE ADR DATA)))

(AND (= PROCESSOR-TYPE-CODE SI:CADR-TYPE-CODE)
     (SET-BASE-ADDRESS))

(defun chatst-continuous-test (&optional (file)
                                         (record-errors-this-many-packets (* 10. 60. 5))
                                         (chatst-packet-length 310)
                                         (punt-error-percentage 10.))
  "Test chaosnet continuously, recording errors in FILE."
  (and file
       (not (probef file))
       (with-open-file (stream file ':direction ':out)
         (format stream "Chaosnet error log file created ")
         (time:print-current-date stream)
         (terpri stream)
         (terpri stream)))
  (do-named punt
            ((error-log nil nil)
             (started (time:get-universal-time) nil)
             (total-packets 0)
             (errors-before-punting
               (floor (* record-errors-this-many-packets punt-error-percentage) 100.))
             (errors-this-time 0 0)
             (error-count 0))
            (())
    (unwind-protect
      (progn
        (dotimes (i record-errors-this-many-packets)
          (set-pattern (selectq (\ i 4)
                         (0 'floating-one)
                         (1 'floating-zero)
                         (2 'address)
                         (otherwise (random 177777))))
          (chatst-prep nil)
          (chatst-xmt)
          (let ((error (chatst-rcv-with-errors)))
            (incf total-packets)
            (cond (error
                   (push error error-log)
                   (incf errors-this-time)
                   (and (> errors-this-time errors-before-punting)
                        (return-from punt "Too many errors")))))))
      (with-open-stream (out-stream
                          (if file
                              (global:open file ':direction ':append)
                            standard-output))
        (cond (started
               (format out-stream "~&~%")
               (time:print-universal-time started out-stream)
               (format out-stream "  Test started")))
        (format out-stream "~&")
        (incf error-count errors-this-time)
        (if ( errors-this-time errors-before-punting)
            (dolist (e (nreverse error-log))
              (format out-stream "~&")
              (time:print-universal-time (first e) out-stream)
              (format out-stream "  ~A, Pat ~A" (third e) (second e))
              (dolist (data (cdddr e))
                (format out-stream ", ~O" data)))
          (format out-stream "~&")
          (time:print-current-time out-stream)
          (format out-stream "  Too many errors, punting test"))
        (format out-stream "~&")
        (time:print-current-time out-stream)
        (format out-stream "  ~D packets transmitted with ~D total errors"
                total-packets error-count)))))

(defun chatst-rcv-with-errors (&aux (cnt chatst-packet-length) csr tem me crc-1-error bbc k)
  (process-wait-with-timeout "Chaos Receive" 2
                             #'(lambda ()
                                 (ldb-test %%chaos-csr-receive-done
                                           (%u-read control-status-register-test))))
  (setq csr (%u-read control-status-register-test))
  (setq me (%u-read my-number-register-test))
  (catch 'chatst-error
    (and (ldb-test %%chaos-csr-transmit-abort csr)
         (chatst-error-log nil "Transmit aborted"))
    (or (ldb-test %%chaos-csr-receive-done csr)
        (chatst-error-log nil "No receive"))
    (setq crc-1-error (ldb-test %%chaos-csr-crc-error csr))
    (setq bbc ( (%u-read bit-count-register-test) (1- (* 16. (+ 3 cnt)))))
    (dotimes (i cnt)
      (as-1 (%u-read read-buffer-register-test) inbuf i))
    (and ( (setq tem (%u-read read-buffer-register-test)) me)
         (chatst-error-log crc-1-error "Destination not me (me, dest)" me tem))
    (and ( (setq tem (%u-read read-buffer-register-test)) me)
         (chatst-error-log crc-1-error "Source not me (me, source)" me tem))
    (dotimes (i cnt)
      (setq k (ar-1 chatst-pattern i))
      (and ( k (ar-1 inbuf i))
           (chatst-error-log crc-1-error "Adr, good, bad data" i k (ar-1 inbuf i))))
    (and crc-1-error (chatst-error-log nil "CRC error"))))

(defun chatst-error-log (crc-error-p string &rest data)
  (throw 'chatst-error (list* (time:get-universal-time) chatst-pattern-type
                              (if crc-error-p
                                  (format nil "~A [CRC]" string)
                                string)
                              (copylist data))))

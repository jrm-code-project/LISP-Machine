;;; -*- Mode:Lisp; Package:System-Internals; Base:8 -*-
;;; ** (C) Copyright 1980, Massachusetts Institute of Technology
;;;    Enhancements (C) Copyright 1981, Symbolics, Inc.
;;; The Massachusetts Institute of Technology has acquired the rights from Symbolics
;;; to include the Software covered by the foregoing notice of copyright with its
;;; licenses of the Lisp Machine System **

;;; Handler for the serial I/O interface

(DEFUN MAKE-SERIAL-STREAM (&REST KEYWORDS)
  "Make a stream that talks over the serial i//o interface.
Keywords are:
:ASCII-CHARACTERS -- T enables translation between Ascii and Lisp machine character sets
:ASCII-PROTOCOL -- obsolete name for :XON-XOFF-PROTOCOL
:BAUD -- baud rate.  Default is 300.
:BUFFERING-CAPACITY -- number of characters output buffer, over 2.  Only if ASCII-PROTOCOL
:CHECK-FRAMING-ERRORS -- T enables error if break received
:CHECK-OVER-RUN-ERRORS -- T enables error if input characters lost in UART
    (input characters lost due to buffer overflow not detected).
:CHECK-PARITY-ERRORS -- T enables error if wrong parity
:DATA-TERMINAL-READY -- NIL disables DTR output
:DLE-CHARACTER -- data link escape character for synchronous mode
:FORCE-OUTPUT -- NIL means you must :FORCE-OUTPUT to cause transmission,
    T (the default) means an automatic :FORCE-OUTPUT occurs after :TYO/:STRING-OUT
    Using NIL gives faster output.
:INPUT-BUFFER-SIZE -- approximate size to make the input ring buffer, in characters.
:NUMBER-OF-DATA-BITS -- default 7  (doesn't include parity bit)
:NUMBER-OF-STOP-BITS -- default 1
:OUTPUT-BUFFER-SIZE -- approximate size to make the output ring buffer, in characters.
:PARITY -- default :EVEN
:REQUEST-TO-SEND -- NIL disables RTS output
:SYNCHRONOUS-MODE -- T enables synchronous protocol
:SYN1-CHARACTER -- first sync character for synchronous mode
:SYN2-CHARACTER -- second sync character for synchronous mode
:XON-XOFF-PROTOCOL -- T enables XON/XOFF (^S/^Q) control of output"
  (DECLARE (ARGLIST &KEY &OPTIONAL ASCII-CHARACTERS ASCII-PROTOCOL BAUD BUFFERING-CAPACITY
                    CHECK-FRAMING-ERRORS CHECK-OVER-RUN-ERRORS CHECK-PARITY-ERRORS
                    DATA-TERMINAL-READY DLE-CHARACTER FORCE-OUTPUT
                    NUMBER-OF-DATA-BITS NUMBER-OF-STOP-BITS PARITY
                    REQUEST-TO-SEND SYNCHRONOUS-MODE SYN1-CHARACTER SYN2-CHARACTER
                    XON-XOFF-PROTOCOL))
  (INSTANTIATE-FLAVOR (IF (GETL (LOCF KEYWORDS) '(:XON-XOFF-PROTOCOL :ASCII-PROTOCOL))
                          (IF (GET (LOCF KEYWORDS) ':ASCII-CHARACTERS)
                              'SERIAL-ASCII-XON-XOFF-STREAM
                            'SERIAL-XON-XOFF-STREAM)
                        (IF (GET (LOCF KEYWORDS) ':ASCII-CHARACTERS)
                            'SERIAL-ASCII-STREAM
                          'SERIAL-STREAM))
                      (LOCF KEYWORDS)
                      T))

(DEFUN SERIAL-STATUS ()
  "Print out the status of the serial I//O interface."
  (LET ((MODE (SERIAL-READ-MODE))
        (STATUS (%UNIBUS-READ 764162))
        (COMMAND (%UNIBUS-READ 764166))
        (SYNCHRONOUS-P NIL))
    (FORMAT T "~%STATUS ~O, COMMAND ~O~%" STATUS COMMAND)
    (AND (ZEROP (LDB 1002 MODE)) (SETQ SYNCHRONOUS-P T))
    (SELECTQ (LDB 0602 COMMAND)
      (0 )
      (1 (PRINC (IF SYNCHRONOUS-P "syn//dle-stripping " "auto-echo ")))
      (2 (PRINC "local-loop-back "))
      (3 (PRINC "remote-loop-back ")))
    (AND (BIT-TEST 40 COMMAND) (PRINC "request-to-send "))
    (AND (BIT-TEST 2 COMMAND) (PRINC "data-terminal-ready "))
    (AND (BIT-TEST 10 COMMAND) (PRINC (IF SYNCHRONOUS-P "send-dle" "send-break")))
    (FORMAT T "receiver-~[off~;on~] " (LDB 0201 COMMAND))
    (FORMAT T "transmitter-~[off~;on~]~%" (LDB 0001 COMMAND))
    (FORMAT T "interrupt-~[dis~;en~]able " (LDB 0701 (%UNIBUS-READ 764112)))
    (AND (BIT-TEST 200 STATUS) (PRINC "data-set-ready "))
    (AND (BIT-TEST 100 STATUS) (PRINC "carrier-detect "))
    (AND (BIT-TEST 40 STATUS) (PRINC (IF SYNCHRONOUS-P "sync-detect " "break-received ")))
    (AND (BIT-TEST 20 STATUS) (PRINC "receive-overrun "))
    (AND (BIT-TEST 10 STATUS) (PRINC (IF SYNCHRONOUS-P "par-err-or-dle " "parity-error ")))
    (AND (BIT-TEST 4 STATUS) (PRINC "idle//data-set-change "))
    (AND (BIT-TEST 2 STATUS) (PRINC "receive-ready "))
    (AND (BIT-TEST 1 STATUS) (PRINC "transmit-ready "))
    (TERPRI)
    (FORMAT T "~[ILLEGAL~;1~;1.5~;2~] stop bits, " (LDB 1602 MODE))
    (FORMAT T "~[no~;odd~;no~;even~] parity, " (LDB 1402 MODE))
    (FORMAT T "~D data bits, " (+ 5 (LDB 1202 MODE)))
    (FORMAT T "~[synchronous~;asynchronous~;asynchronous//16~;asynchronous//64~]~%"
              (LDB 1002 MODE))
    (FORMAT T "~[external~;internal~] transmit clock, " (LDB 0501 MODE))
    (FORMAT T "~[external~;internal~] receive clock, " (LDB 0401 MODE))
    (FORMAT T "~D baud" (NTH (LDB 0004 MODE) '(50. 75. 110. 134. 150. 300. 600.
                                               1200. 1800. 2000. 2400. 3600.
                                               4800. 7200. 9600. 19200.)))))

(DEFFLAVOR SERIAL-STREAM-MIXIN
           (INPUT-UNIBUS-CHANNEL
            OUTPUT-UNIBUS-CHANNEL
            RANDOM-UNIBUS-CHANNEL
            ;; RANDOM-UNIBUS-CHANNEL is for some serial pci chips which seem to cause
            ;; interrupts on modem transitions (bit 4). This absorbs these interrupts
            ;; to prevent the microcode from bombing because it can't find anyone to
            ;; give the interrupt to.
            (INPUT-BUFFER-SIZE PAGE-SIZE)
            (OUTPUT-BUFFER-SIZE PAGE-SIZE)
            (UART-COMMAND 5)                    ;receive, transmit enable
            (UART-MODES 0)
            (FORCE-OUTPUT T)
            (DLE-CHARACTER 0)
            (SYN1-CHARACTER 0)
            (SYN2-CHARACTER 0)
            (SERIAL-UNRCHF NIL)
            (SERIAL-ERROR-MASK 0))
           (SI:LINE-OUTPUT-STREAM-MIXIN)
  (:REQUIRED-FLAVORS SI:BIDIRECTIONAL-STREAM SI:CHARACTER-STREAM
                     SI:UNBUFFERED-LINE-INPUT-STREAM
                     SI:BASIC-BUFFERED-OUTPUT-STREAM)
  (:INITABLE-INSTANCE-VARIABLES FORCE-OUTPUT DLE-CHARACTER SYN1-CHARACTER SYN2-CHARACTER
                                INPUT-BUFFER-SIZE OUTPUT-BUFFER-SIZE)
  (:INIT-KEYWORDS :CHECK-PARITY-ERRORS :CHECK-OVER-RUN-ERRORS :CHECK-FRAMING-ERRORS
                  :REQUEST-TO-SEND :DATA-TERMINAL-READY :SYNCHRONOUS-MODE
                  :NUMBER-OF-STOP-BITS :PARITY :NUMBER-OF-DATA-BITS :BAUD
                  :XON-XOFF-PROTOCOL :ASCII-PROTOCOL :ASCII-CHARACTERS)
  (:DOCUMENTATION :MIXIN "The guts of RS232 Serial I//O Streams"))

(DEFFLAVOR SERIAL-STREAM () (SERIAL-STREAM-MIXIN
                             SI:BIDIRECTIONAL-STREAM SI:CHARACTER-STREAM
                             SI:UNBUFFERED-LINE-INPUT-STREAM
                             SI:BUFFERED-OUTPUT-STREAM)
  (:DOCUMENTATION :COMBINATION "RS232 Serial I//O Stream, no character-set translation"))

(DEFFLAVOR SERIAL-ASCII-STREAM ()
           (SI:ASCII-TRANSLATING-OUTPUT-STREAM-MIXIN SI:ASCII-TRANSLATING-INPUT-STREAM-MIXIN
            SERIAL-STREAM-MIXIN
            SI:BIDIRECTIONAL-STREAM SI:CHARACTER-STREAM
            SI:UNBUFFERED-LINE-INPUT-STREAM
            SI:BASIC-BUFFERED-OUTPUT-STREAM)
  (:DOCUMENTATION :COMBINATION "RS232 Serial I//O Stream, Ascii character translation"))

;;; Housekeeping methods
(DEFMETHOD (SERIAL-STREAM-MIXIN :INIT) (INIT-PLIST)
  (SERIAL-CHECK-EXISTENCE)                      ;Barf if machine doesn't have serial I/O port
  (LET ((REQUEST-TO-SEND T)                     ;Parse options
        (DATA-TERMINAL-READY T)
        (SYNCHRONOUS-MODE NIL))
    (LOOP FOR (KWD VAL) ON (CDR INIT-PLIST) BY 'CDDR DO
      (SELECTQ KWD
        (:REQUEST-TO-SEND (SETQ REQUEST-TO-SEND VAL))
        (:DATA-TERMINAL-READY (SETQ DATA-TERMINAL-READY VAL))
        (:SYNCHRONOUS-MODE (SETQ SYNCHRONOUS-MODE VAL))))
    (%UNIBUS-WRITE 764166 20)                   ;Reset
    (SERIAL-WRITE-COMMAND UART-COMMAND)         ;Unreset, enable receiver and transmitter
    (SERIAL-WRITE-MODE 60)                      ;Reset modes, set to internal clocks
    ;; Set up modes which want to be specified first
    (SEND SELF ':PUT ':SYNCHRONOUS-MODE SYNCHRONOUS-MODE)
    (SEND SELF ':PUT ':REQUEST-TO-SEND REQUEST-TO-SEND)
    (SEND SELF ':PUT ':DATA-TERMINAL-READY DATA-TERMINAL-READY)
    ;; Default to even parity and 7 data bits,
    ;; but don't check received parity.  This causes the
    ;; input stream to return 7-bit characters, to avoid
    ;; faking out Lisp-machine-oriented programs.
    (SEND SELF ':PUT ':NUMBER-OF-STOP-BITS 1)
    (SEND SELF ':PUT ':PARITY ':EVEN)
    (SEND SELF ':PUT ':NUMBER-OF-DATA-BITS 7)
    (SEND SELF ':PUT ':BAUD 300.)
    ;; Now do :PUT for any necessary options
    (LOOP FOR (KWD VAL) ON (CDR INIT-PLIST) BY 'CDDR
          UNLESS (MEMQ KWD '(:REQUEST-TO-SEND :DATA-TERMINAL-READY :SYNCHRONOUS-MODE
                             :XON-XOFF-PROTOCOL :ASCII-PROTOCOL :BUFFERING-CAPACITY
                             :ASCII-CHARACTERS :FORCE-OUTPUT
                             :DLE-CHARACTER :SYN1-CHARACTER :SYN2-CHARACTER))
            DO (ERRSET (SEND SELF ':PUT KWD VAL) NIL))
    (SEND SELF ':RESET)))                       ;Sets up the Unibus channels

;;; Test existence of device.  If IOB not wired for it, will read back
;;; all zero.  If PCI not plugged in, will read back all ones.
(DEFUN SERIAL-CHECK-EXISTENCE (&AUX ZEROS ONES)
  (%UNIBUS-WRITE 764166 0)
  (SETQ ZEROS (LDB 0010 (%UNIBUS-READ 764166)))
  (%UNIBUS-WRITE 764166 100)
  (SETQ ONES (LDB 0010 (%UNIBUS-READ 764166)))
  (COND ((ZEROP ONES) (FERROR NIL "This IOB does not have serial I/O"))
        ((= ZEROS 377) (FERROR NIL "This IOB does not contain a PCI"))))

;;; Get the various modes, hardware status, etc.
(DEFMETHOD (SERIAL-STREAM-MIXIN :GET) (PROP)
  (LET ((MODE (SERIAL-READ-MODE))
        (STATUS (%UNIBUS-READ 764162))
        (COMMAND (%UNIBUS-READ 764166))
        (SYNCHRONOUS-P NIL))
    (AND (ZEROP (LDB 1002 MODE)) (SETQ SYNCHRONOUS-P T))
    (SELECTQ PROP
      (:DATA-SET-READY (BIT-TEST 200 STATUS))   ;This might want to be wired to CTS?
      (:CARRIER-DETECT (BIT-TEST 100 STATUS))
      (:REQUEST-TO-SEND (BIT-TEST 40 COMMAND))
      (:DATA-TERMINAL-READY (BIT-TEST 2 COMMAND))
      (:NUMBER-OF-STOP-BITS (IF SYNCHRONOUS-P
                                0
                              (NTH (LDB 1602 MODE) '(NIL 1 1.5 2))))
      (:PARITY (NTH (LDB 1402 MODE) '(NIL :ODD NIL :EVEN)))
      (:NUMBER-OF-DATA-BITS (+ 5 (LDB 1202 MODE)))
      (:BAUD (NTH (LDB 0004 MODE) '(50. 75. 110. 134. 150. 300. 600.
                                        1200. 1800. 2000. 2400. 3600.
                                        4800. 7200. 9600. 19200.)))
      (:INPUT-BUFFER-SIZE INPUT-BUFFER-SIZE)
      (:OUTPUT-BUFFER-SIZE INPUT-BUFFER-SIZE)
      (:SYNCHRONOUS-MODE SYNCHRONOUS-P)
      (:SYN1-CHARACTER SYN1-CHARACTER)
      (:SYN2-CHARACTER SYN2-CHARACTER)
      (:DLE-CHARACTER DLE-CHARACTER)
      (:SINGLE-SYNC-CHAR-MODE (AND SYNCHRONOUS-P (BIT-TEST 100000 MODE)))
      (:SYNC-TRANSPARENT-MODE (AND SYNCHRONOUS-P (BIT-TEST 40000 MODE)))
      (:AUTOMATIC-ECHO-MODE (AND (NOT SYNCHRONOUS-P) (= (LDB 0602 COMMAND) 1)))
      (:SYNC-DLE-STRIPPING-MODE (AND SYNCHRONOUS-P (= (LDB 0602 COMMAND) 1)))
      (:LOCAL-LOOP-BACK (= (LDB 0602 COMMAND) 2))
      (:REMOTE-LOOP-BACK (= (LDB 0602 COMMAND) 3))
      (:RECEIVE-ENABLE (BIT-TEST 4 COMMAND))
      (:TRANSMIT-ENABLE (BIT-TEST 1 COMMAND))
      (:CHECK-PARITY-ERRORS (BIT-TEST 10 SERIAL-ERROR-MASK))
      (:CHECK-OVER-RUN-ERRORS (BIT-TEST 20 SERIAL-ERROR-MASK))
      (:CHECK-FRAMING-ERRORS (BIT-TEST 40 SERIAL-ERROR-MASK))
      (OTHERWISE (FERROR NIL "~S not a valid property name" PROP)))))

(DEFUN SERIAL-READ-MODE ()
  (%UNIBUS-READ 764166)                         ;reset MODE1/MODE2 phase
  (DPB (%UNIBUS-READ 764164) 1010 (%UNIBUS-READ 764164)))

;;; Change modes, hardware status, etc.
(DEFMETHOD (SERIAL-STREAM-MIXIN :PUT) (PROP VAL)
  (LET ((MODE (SERIAL-READ-MODE))
        (SYNCHRONOUS-P NIL))
    (AND (ZEROP (LDB 1002 MODE)) (SETQ SYNCHRONOUS-P T))
    (SELECTQ PROP
      (:REQUEST-TO-SEND (SERIAL-WRITE-COMMAND (LOGIOR (IF VAL 40 0) UART-COMMAND)))
      (:DATA-TERMINAL-READY (SERIAL-WRITE-COMMAND (LOGIOR (IF VAL 2 0) UART-COMMAND)))
      (:NUMBER-OF-STOP-BITS
       (SETQ VAL (SELECT VAL
                   (1 1)
                   (1.5 2)
                   (2 3)
                   (T (FERROR NIL ":NUMBER-OF-STOP-BITS is ~S, not 1, 1.5, or 2." VAL))))
       (SERIAL-WRITE-MODE (DPB VAL 1602 MODE)))
      (:PARITY
       (SETQ VAL (SELECTQ VAL
                   (NIL 0)
                   (:ODD 1)
                   (:EVEN 3)
                   (T (FERROR NIL ":PARITY is ~S, not NIL, :ODD, or :EVEN." VAL))))
       (SERIAL-WRITE-MODE (DPB VAL 1402 MODE)))
      (:NUMBER-OF-DATA-BITS
       (SETQ VAL (IF ( 5 VAL 8)
                     (- VAL 5)
                   (FERROR NIL ":NUMBER-OF-DATA-BITS is ~D, not 5, 6, 7, or 8." VAL)))
       (SERIAL-WRITE-MODE (DPB VAL 1202 MODE)))
      (:BAUD
       (SETQ VAL (FIND-POSITION-IN-LIST VAL
                                        '(50.   75.   110.  134.  150.  300.  600.  1200.
                                          1800. 2000. 2400. 3600. 4800. 7200. 9600. 19200.)))
       (OR VAL (FERROR NIL "Invalid serial stream baud rate, ~D." VAL))
       (SERIAL-WRITE-MODE (DPB VAL 0004 MODE)))
      (:INPUT-BUFFER-SIZE (SETQ INPUT-BUFFER-SIZE VAL))
      (:OUTPUT-BUFFER-SIZE (SETQ INPUT-BUFFER-SIZE VAL))
      (:SYNCHRONOUS-MODE
       (AND VAL (SERIAL-WRITE-SYNC-CHARS SYN1-CHARACTER SYN2-CHARACTER DLE-CHARACTER))
       (SERIAL-WRITE-MODE (DPB (IF VAL 0 1) 1002 MODE)))
      (:SYN1-CHARACTER
       (SETQ SYN1-CHARACTER VAL)
       (SERIAL-WRITE-SYNC-CHARS SYN1-CHARACTER SYN2-CHARACTER DLE-CHARACTER))
      (:SYN2-CHARACTER
       (SETQ SYN2-CHARACTER VAL)
       (SERIAL-WRITE-SYNC-CHARS SYN1-CHARACTER SYN2-CHARACTER DLE-CHARACTER))
      (:DLE-CHARACTER
       (SETQ DLE-CHARACTER VAL)
       (SERIAL-WRITE-SYNC-CHARS SYN1-CHARACTER SYN2-CHARACTER DLE-CHARACTER))
      (:SINGLE-SYNC-CHAR-MODE
       (SERIAL-WRITE-MODE (DPB (IF VAL 1 0) 1701 MODE)))
      (:SYNC-TRANSPARENT-MODE
       (SERIAL-WRITE-MODE (DPB (IF VAL 1 0) 1601 MODE)))
      (:AUTOMATIC-ECHO-MODE
       (AND SYNCHRONOUS-P (FERROR NIL "does not apply in synchronous mode"))
       (SERIAL-WRITE-COMMAND (DPB (IF VAL 1 0) 0602 UART-COMMAND)))
      (:SYNC-DLE-STRIPPING-MODE
       (AND (NOT SYNCHRONOUS-P) (FERROR NIL "does not apply in asynchronous mode"))
       (SERIAL-WRITE-COMMAND (DPB (IF VAL 1 0) 0602 UART-COMMAND)))
      (:LOCAL-LOOP-BACK
       (SERIAL-WRITE-COMMAND (DPB (IF VAL 2 0) 0602 UART-COMMAND)))
      (:REMOTE-LOOP-BACK
       (SERIAL-WRITE-COMMAND (DPB (IF VAL 3 0) 0602 UART-COMMAND)))
      (:RECEIVE-ENABLE
       (SERIAL-WRITE-COMMAND (DPB (IF VAL 1 0) 0201 UART-COMMAND)))
      (:TRANSMIT-ENABLE
       (SERIAL-WRITE-COMMAND (DPB (IF VAL 1 0) 0001 UART-COMMAND)))
      (:CHECK-PARITY-ERRORS
       (SETQ SERIAL-ERROR-MASK (DPB (IF VAL 1 0) 0301 SERIAL-ERROR-MASK)))
      (:CHECK-OVER-RUN-ERRORS
       (SETQ SERIAL-ERROR-MASK (DPB (IF VAL 1 0) 0401 SERIAL-ERROR-MASK)))
      (:CHECK-FRAMING-ERRORS
       (SETQ SERIAL-ERROR-MASK (DPB (IF VAL 1 0) 0501 SERIAL-ERROR-MASK)))
      (OTHERWISE (FERROR NIL "~S not a valid property name" PROP)))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SERIAL-STREAM-MIXIN)
(DEFUN SERIAL-WRITE-COMMAND (CMD)
  (SETQ UART-COMMAND CMD)                       ;remember it
  (IF (VARIABLE-BOUNDP OUTPUT-UNIBUS-CHANNEL)
      (%P-DPB-OFFSET (LOGAND CMD 376) %%Q-POINTER OUTPUT-UNIBUS-CHANNEL
                     %UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS))
  (%UNIBUS-WRITE 764166 CMD)))

(DEFUN SERIAL-WRITE-SYNC-CHARS (SYN1 SYN2 DLE)
  (%UNIBUS-READ 764166)                         ;reset MODE1/MODE2 phase
  (%UNIBUS-WRITE 764162 SYN1)
  (%UNIBUS-WRITE 764162 SYN2)
  (%UNIBUS-WRITE 764162 DLE))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SERIAL-STREAM-MIXIN)
(DEFUN SERIAL-WRITE-MODE (MODE)
  (SETQ UART-MODES MODE)                        ;remember it
  (%UNIBUS-READ 764166)                         ;reset MODE1/MODE2 phase
  (%UNIBUS-WRITE 764164 (LDB 1010 MODE))
  (%UNIBUS-WRITE 764164 MODE)))

(DEFMETHOD (SERIAL-STREAM-MIXIN :CLOSE) (&OPTIONAL ABORT-P)
  (IF (AND (NOT ABORT-P) (VARIABLE-BOUNDP OUTPUT-UNIBUS-CHANNEL) OUTPUT-UNIBUS-CHANNEL)
      (SEND SELF ':FINISH))
  (%UNIBUS-WRITE 764112 (DPB 0 0701 (%UNIBUS-READ 764112)))     ;Turn off interrupt
  (WHEN (VARIABLE-BOUNDP INPUT-UNIBUS-CHANNEL)
    (RETURN-UNIBUS-CHANNEL (PROG1 INPUT-UNIBUS-CHANNEL
                                  (VARIABLE-MAKUNBOUND INPUT-UNIBUS-CHANNEL))))
  (WHEN (VARIABLE-BOUNDP OUTPUT-UNIBUS-CHANNEL)
    (RETURN-UNIBUS-CHANNEL (PROG1 OUTPUT-UNIBUS-CHANNEL
                                  (VARIABLE-MAKUNBOUND OUTPUT-UNIBUS-CHANNEL))))
  (WHEN (VARIABLE-BOUNDP RANDOM-UNIBUS-CHANNEL)
    (RETURN-UNIBUS-CHANNEL (PROG1 RANDOM-UNIBUS-CHANNEL
                                  (VARIABLE-MAKUNBOUND RANDOM-UNIBUS-CHANNEL)))))

(DEFMETHOD (SERIAL-STREAM-MIXIN :FINISH) ()
  (PROCESS-WAIT "Serial Finish"
                #'(LAMBDA (CHAN) (NOT (UNIBUS-CHANNEL-NOT-EMPTY CHAN)))
                OUTPUT-UNIBUS-CHANNEL))

;This fully resets the hardware.
(DEFMETHOD (SERIAL-STREAM-MIXIN :CLEAR-INPUT) ()
  (SEND SELF ':RESET NIL))

(DEFMETHOD (SERIAL-STREAM-MIXIN :RESET) (&OPTIONAL (ABORT-OUTPUT :ABORT))
    (SEND SELF :CLOSE ABORT-OUTPUT)             ;Get rid of unibus channels if any
    ;; Reset the stupid error flags
    (%UNIBUS-WRITE 764166 (LOGIOR 20 UART-COMMAND))
    (%UNIBUS-WRITE 764166 UART-COMMAND)
    (SERIAL-WRITE-MODE UART-MODES)              ;Restore mode registers
    (%UNIBUS-READ 764160)                       ;Flush buffered input character if any
    (SETQ INPUT-UNIBUS-CHANNEL
          (GET-UNIBUS-CHANNEL 264 764162 2 764160 2
                              (CEILING INPUT-BUFFER-SIZE PAGE-SIZE))
          OUTPUT-UNIBUS-CHANNEL
          (GET-UNIBUS-CHANNEL 264 764162 1 764160 1
                              (CEILING OUTPUT-BUFFER-SIZE PAGE-SIZE)
                              764166 (LOGAND UART-COMMAND 376))
          ;;Some UARTS cause interrupts on modem changes.  RANDOM-UNIBUS-CHANNEL absorbs
          ;; these to prevent microcode from bombing because it cant find anyone to give
          ;; interrupt to.  "DATA" is CSR itself.
          RANDOM-UNIBUS-CHANNEL
          (GET-UNIBUS-CHANNEL 264 764162 4 764162 1 1)) ;; PAGE-SIZE
    (%UNIBUS-WRITE 764112 (DPB 1 0701 (%UNIBUS-READ 764112))))  ;Turn on interrupt

;;; Stream Input Protocol
(DEFMETHOD (SERIAL-STREAM-MIXIN :TYI) (&OPTIONAL IGNORE)
  (COND ((NULL SERIAL-UNRCHF)
         (PROCESS-WAIT "Serial TYI" #'UNIBUS-CHANNEL-NOT-EMPTY INPUT-UNIBUS-CHANNEL)
         (MULTIPLE-VALUE-BIND (CH STATUS) (READ-UNIBUS-CHANNEL INPUT-UNIBUS-CHANNEL)
           (SETQ CH (LOGAND 377 CH))
           (COND ((BIT-TEST SERIAL-ERROR-MASK STATUS)
                  ;; Reset the stupid error flags
                  (%UNIBUS-WRITE 764166 (LOGIOR 20 UART-COMMAND))
                  (%UNIBUS-WRITE 764166 UART-COMMAND)
                  (CERROR T NIL NIL
                          "Serial input ~:[framing ~]~:[over-run ~]~:[parity ~]error: ~O"
                          (ZEROP (LOGAND 40 STATUS SERIAL-ERROR-MASK))
                          (ZEROP (LOGAND 20 STATUS SERIAL-ERROR-MASK))
                          (ZEROP (LOGAND 10 STATUS SERIAL-ERROR-MASK))
                          CH)))
           CH))
        (T (PROG1 SERIAL-UNRCHF (SETQ SERIAL-UNRCHF NIL)))))

(DEFMETHOD (SERIAL-STREAM-MIXIN :UNTYI) (CHAR)
  (SETQ SERIAL-UNRCHF CHAR))

(DEFMETHOD (SERIAL-STREAM-MIXIN :LISTEN) ()
  (OR (NOT (NULL SERIAL-UNRCHF))
      (UNIBUS-CHANNEL-NOT-EMPTY INPUT-UNIBUS-CHANNEL)))

(DEFMETHOD (SERIAL-STREAM-MIXIN :TYI-NO-HANG) ()
  (AND (UNIBUS-CHANNEL-NOT-EMPTY INPUT-UNIBUS-CHANNEL)
       (SEND SELF ':TYI)))

;;; Next three methods implement buffered output stream protocol
(DEFMETHOD (SERIAL-STREAM-MIXIN :NEW-OUTPUT-BUFFER) ()
  (PROCESS-WAIT "Serial TYO" #'UNIBUS-CHANNEL-NOT-FULL OUTPUT-UNIBUS-CHANNEL)
  (MULTIPLE-VALUE-BIND (START END) (UNIBUS-CHANNEL-SPACE-AVAILABLE OUTPUT-UNIBUS-CHANNEL)
    (VALUES OUTPUT-UNIBUS-CHANNEL START END)))

(DEFMETHOD (SERIAL-STREAM-MIXIN :SEND-OUTPUT-BUFFER) (IGNORE NEW-INDEX)
  (UNIBUS-CHANNEL-ADVANCE OUTPUT-UNIBUS-CHANNEL NEW-INDEX)
  (%UNIBUS-WRITE 764166 UART-COMMAND))  ;Enables the transmitter

(DEFMETHOD (SERIAL-STREAM-MIXIN :DISCARD-OUTPUT-BUFFER) (IGNORE)
  NIL)

;;; The inherited method for this (from BUFFERED-OUTPUT-STREAM) does not work
;;; because we have a single circular buffer, so just punt.
;;; This is used by :FRESH-LINE.
(DEFMETHOD (SERIAL-STREAM-MIXIN :LAST-CHAR-OUTPUT) () NIL)

;;; For compatibility with the old unbuffered stream, we provide a mode where
;;; you don't have to do :FORCE-OUTPUT manually, and even make it the default.
;;; This should be a mixin, but it was too inconvenient to have so many flavors
;;; for such a trivial thing.
(DEFMETHOD (SERIAL-STREAM-MIXIN :AFTER :TYO) (IGNORE)
  (AND FORCE-OUTPUT (SEND SELF ':FORCE-OUTPUT)))

(DEFMETHOD (SERIAL-STREAM-MIXIN :AFTER :STRING-OUT) (&REST IGNORE)
  (AND FORCE-OUTPUT (SEND SELF ':FORCE-OUTPUT)))

(DEFFLAVOR SERIAL-XON-XOFF-MIXIN ((BUFFERING-CAPACITY 10.)) ()
  (:INITABLE-INSTANCE-VARIABLES BUFFERING-CAPACITY)
  (:REQUIRED-FLAVORS SERIAL-STREAM-MIXIN)
  (:DOCUMENTATION :MIXIN "Serial output controlled by XON and XOFF characters.
   BUFFERING-CAPACITY is one half the allowed number of characters between XOFF checks."))

(DEFFLAVOR SERIAL-XON-XOFF-STREAM () (SERIAL-XON-XOFF-MIXIN SERIAL-STREAM)
  (:DOCUMENTATION :COMBINATION
                  "RS232 Serial I//O Stream, XON//XOFF, no character translation"))

(DEFFLAVOR SERIAL-ASCII-XON-XOFF-STREAM () (SERIAL-XON-XOFF-MIXIN SERIAL-ASCII-STREAM)
  (:DOCUMENTATION :COMBINATION
                  "RS232 Serial I//O Stream, XON//XOFF, ascii character translation"))

(DEFMETHOD (SERIAL-XON-XOFF-MIXIN :NEW-OUTPUT-BUFFER) ()
  (PROCESS-WAIT "Serial TYO"
                #'UNIBUS-CHANNEL-NOT-FULL OUTPUT-UNIBUS-CHANNEL BUFFERING-CAPACITY)
  (LET ((CH (SEND SELF ':TYI-NO-HANG)))
    (SELECTQ CH
      (21 NIL)                                  ;Spurious ^Q - ignore.
      (23                                       ;XOFF (^S)
        (LOOP DO (PROCESS-WAIT "Serial XON" #'UNIBUS-CHANNEL-NOT-EMPTY INPUT-UNIBUS-CHANNEL)
              UNTIL (= (SEND SELF ':TYI) 21)))  ;XON (^Q)
      (NIL )
      (OTHERWISE (FERROR NIL "Unexpected character ~O received" CH))))
  (MULTIPLE-VALUE-BIND (START END) (UNIBUS-CHANNEL-SPACE-AVAILABLE OUTPUT-UNIBUS-CHANNEL)
    (IF (> (- END START) BUFFERING-CAPACITY)
        (SETQ END (+ START BUFFERING-CAPACITY)))
    (VALUES OUTPUT-UNIBUS-CHANNEL START END)))

(COMPILE-FLAVOR-METHODS SERIAL-STREAM SERIAL-ASCII-STREAM
                        SERIAL-XON-XOFF-STREAM SERIAL-ASCII-XON-XOFF-STREAM)

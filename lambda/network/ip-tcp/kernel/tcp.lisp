;;; -*- Mode:LISP; Package:TCP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *tcp-stream*
           setup-tcp
           wait-for-tcp-enabled
           describe-tcp
           make-tcp-socket
           using-tcp-socket
           multiple-connection-listen
           multiple-connection-kill-gauges))

;;;A structure full of statistics inspired by 4BSD
(eval-when (compile load eval)
  (defvar *all-tcp-statistics* (make-fifo)))

(defmacro tcp-statistics (&rest stats)
  (let ((index 0)
        (forms nil))
    (dolist (stat stats)
      (let* ((symbol (first stat))
             (documentation (second stat))
             (name (intern (string-append "TS-" symbol))))
        (incf index)
        (push `(eval-when (compile load eval)
                 (pushnew-fifo ',symbol *all-tcp-statistics*))
              forms)
        (push `(defconstant ,symbol ,index ,(or documentation "TCP statistic")) forms)
        (push `(defsubst ,name (array)
                 (aref array ,index))
              forms)))
    (push `(defmacro make-tcp-statistics ()
             '(si:make-array-into-named-structure
                (make-array ,(1+ index) :initial-element 0)
                'tcp-statistics))
          forms)
    `(progn ,@(nreverse forms))))

(tcp-statistics
  (data-packets-sent "packets sent containing data")
  (data-bytes-sent "User data bytes sent")
  (ack-packets-requested "ACK packets requested (not all will be sent....)")
  (ack-packets-sent "packets actually sent containing only ACKs")
  (ack-packets-delayed "ACK packets delayed before being sent (not all were sent....)")
  (urg-packets-sent "ACK packets sent to communicate URG when SND.WND shut")
  (window-probes-sent "zero-window probes")
  (saved-bytes "Bytes saved beyond user receive buffers")
  (retrieved-bytes "Saved bytes that were retrieved")
  (dropped-bytes "Bytes that couldn't be saved")
  (rst-packets-sent "RST packets")
  ;;Here follow statistics for packets RECEIVED...
  (ack-packets-received "packets received with an acknowledgement")
  (ack-bytes-received "number of unduplicate bytes acknowledged")
  (duplicate-ack-packets-received "packets received with only a duplicate ACK")
  (unsent-data-acks "packets acknowledging data never sent")
  (in-sequence-packets "packets received in expected order")
  (in-sequence-bytes "bytes received in expected order")
  (total-duplicate-packets "packets that are complete duplicates")
  (total-duplicate-bytes "bytes that are complete duplicates")
  (partially-duplicate-packets "packets that contain some duplicate data")
  (partially-duplicate-bytes "duplicate bytes from partial dup packets")
  (out-of-order-packets "packets received out of order")
  (out-of-order-bytes "bytes received out of order")
  (out-of-window-packets "packets with data after window")
  (out-of-window-bytes "bytes received out of window")
  (urg-packets-received "ACK packets received that update RCV.UP when RCV.WND shut")
  (window-probes-received "received probes of zero-window")
  (window-updates-received "received ACKS with window update")
  (shrinking-window-updates "Window updates that shrank the receive window")
  (useless-ack-packets-received "ACK packets with no discernible purpose")
  (checksum-failures "bad checksum packets")
  (bad-header-offset "packets with bad header offset field")
  (too-short "packets less than minimum TCP header length")
  ;;Here follow statistics on connections
  (out-connection-requests "Outgoing connection requests")
  (out-connection-accepts "Successful outgoing connection requests")
  (out-connection-refused "Refused outgoing connection requests")
  (out-connection-timeouts "Timeouts for outgoing connection requests")
  (in-connection-requests "Incoming connection requests")
  (in-connection-accepts "Successful incoming connection requests")
  (in-connection-refused "Refused incoming connection requests")
  (in-connection-timeouts "Timeouts for incoming connection requests")
  (rtt-attempts "Segments sent that attempted to measure round-trip-time")
  (rtt-updates "Segments received that updated a measured round-trip-time")
  (retransmission-timeouts "Total retransmission timeouts")
  (user-timeouts "Times user specified timeout exceeded")
  (data-bytes-received "Total data bytes received")
  (packet-allocation-delays "Times that an int-pkt was not available when needed")
  (bad-sequence-packets "Packets that failed CHECK-PACKET-SEQUENCE-SPACE")
  (acks-sent-due-to-timer "Number of ACKs sent because delayed-ack timer expired")
  (packets-while-ack-delayed "Number of packets that arrived while delayed-ack timer active")
  (out-connection-aborts "Outgoing connections aborted before opened")
  (in-connection-aborts "Incoming connections aborted before opened")
  (failed-sends "Times that IP refused to send a packet for us (DF, or interface rejected it)")
  (failed-send-timeouts "Timeouts to retransmit a packet that the interface rejected"))

(defselect ((:property tcp-statistics named-structure-invoke))
  (:print-self (array stream ignore ignore)
    (format stream "#S(~S" 'tcp-statistics)
    (dolist (x (fifo-as-list *all-tcp-statistics*))
      (let ((value (aref array (symbol-value x))))
        (unless (zerop value)
          (format stream " ~A ~D" x value))))
    (format stream ")")))

;;;TCP's interface to IP
(defstream tcp-ip-transport-protocol
           (ip-transport-protocol)
           "TCP-"
  (user-socket-alist nil)                       ;user sockets
  (next-local-port 256)                         ;next local port to assign
  (stats (make-tcp-statistics))                 ;Big pile of statistics
  (local-checksums nil)                         ;T if calculate checksums for looped-back packets
  )

(defun tcp-checksum-failures (stream)
  (ts-checksum-failures (tcp-stats stream)))

(defop (tcp-ip-transport-protocol :peek-normal-fields) (tp)
  (ncons (tv:scroll-parse-item
           `(:function tcp-next-local-port (,tp) NIL ("  Next local port assigned = ~D"))
           `(:function tcp-checksum-failures (,tp) NIL ("  Checksum failures ~D"))
           `(:mouse-item
              (nil :buttons
                   ,(make-list 3 :initial-element
                               `(nil :eval (setf (tcp-local-checksums tp)
                                                 (not (tcp-local-checksums tp)))
                                     :bindings ((tp ',tp))))
                   :DOCUMENTATION
                   "Click to toggle local checksums"
                   :BINDINGS
                   ((tp ',tp)))
              :function tcp-local-checksums (,tp) NIL (" Local checksums: ~A")))))

(defop (tcp-ip-transport-protocol :peek-verbose-fields) (tp)
  (let ((stats (tcp-stats tp)))
    (list
      (tv:scroll-parse-item
        " "
        `(:function ts-out-connection-requests (,stats) NIL (" INIT: requests = ~D"))
        `(:function ts-out-connection-accepts (,stats) NIL (" accepts = ~D"))
        `(:function ts-out-connection-refused (,stats) NIL (" refused = ~D"))
        `(:function ts-out-connection-timeouts (,stats) NIL (" timeout = ~D"))
        `(:function ts-out-connection-aborts (,stats) NIL (" aborted = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-in-connection-requests (,stats) NIL (" LISTEN: requests = ~D"))
        `(:function ts-in-connection-accepts (,stats) NIL (" accepts = ~D"))
        `(:function ts-in-connection-refused (,stats) NIL (" refused = ~D"))
        `(:function ts-in-connection-timeouts (,stats) NIL (" timeouts = ~D"))
        `(:function ts-in-connection-aborts (,stats) NIL (" aborted = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-data-packets-sent (,stats) NIL (" Data sent: packets = ~D"))
        `(:function ts-data-bytes-sent (,stats) NIL (", bytes = ~D"))
        `(:function ts-ack-bytes-received (,stats) NIL (", ACKed = ~D"))
        `(:function ts-data-bytes-received (,stats) NIL ("   Data received: bytes = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-ack-packets-requested (,stats) NIL (" ACK packets sent: req = ~D"))
        `(:function ts-ack-packets-sent (,stats) NIL (" sent = ~D"))
        `(:function ts-ack-packets-delayed (,stats) NIL (" delay = ~D"))
        `(:function ts-urg-packets-sent (,stats) NIL (" urg = ~D"))
        `(:function ts-window-probes-sent (,stats) NIL (" probe = ~D"))
        `(:function ts-rst-packets-sent (,stats) NIL (" rst = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-ack-packets-received (,stats) NIL (" ACK packets rcvd = ~D"))
        `(:function ts-duplicate-ack-packets-received (,stats) NIL (" dup = ~D"))
        `(:function ts-unsent-data-acks (,stats) NIL (" unsent = ~D"))
        `(:function ts-useless-ack-packets-received (,stats) NIL (" useless = ~D"))
        `(:function ts-urg-packets-received (,stats) NIL (" URG = ~D"))
        `(:function ts-window-probes-received (,stats) NIL (" probe = ~D"))
        `(:function ts-window-updates-received (,stats) NIL (" update = ~D"))
        `(:function ts-shrinking-window-updates (,stats) NIL (" shrink = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-in-sequence-packets (,stats) NIL (" Data packets rcvd: in-seq = ~D"))
        `(:function ts-total-duplicate-packets (,stats) NIL (" total = ~D"))
        `(:function ts-partially-duplicate-packets (,stats) NIL (" partial = ~D"))
        `(:function ts-out-of-order-packets (,stats) NIL (" order = ~D"))
        `(:function ts-out-of-window-packets (,stats) NIL (" window = ~D"))
        `(:function ts-bad-sequence-packets (,stats) NIL (" bad-seq = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-in-sequence-bytes (,stats) NIL (" Data bytes rcvd: in-seq = ~D"))
        `(:function ts-total-duplicate-bytes (,stats) NIL (" total = ~D"))
        `(:function ts-partially-duplicate-bytes (,stats) NIL (" partial = ~D"))
        `(:function ts-out-of-order-bytes (,stats) NIL (" order = ~D"))
        `(:function ts-out-of-window-bytes (,stats) NIL (" window = ~D"))
        `(:function ts-failed-sends (,stats) NIL ("   Failed sends = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-saved-bytes (,stats) NIL (" Bytes saved = ~D"))
        `(:function ts-retrieved-bytes (,stats) NIL (" retrieved = ~D"))
        `(:function ts-dropped-bytes (,stats) NIL (" dropped = ~D"))
        `(:function ts-packet-allocation-delays (,stats) NIL ("   Alloc delays = ~D"))
        `(:function ts-bad-header-offset (,stats) NIL ("  Header bad = ~D"))
        `(:function ts-too-short (,stats) NIL (" short = ~D"))
        )
      (tv:scroll-parse-item
        " "
        `(:function ts-rtt-attempts (,stats) NIL (" RTT: attempts = ~D"))
        `(:function ts-rtt-updates (,stats) NIL (" updates = ~D"))
        `(:function ts-retransmission-timeouts (,stats) NIL ("   Timeouts: RTO = ~D"))
        `(:function ts-user-timeouts (,stats) NIL (" user = ~D"))
        `(:function ts-acks-sent-due-to-timer (,stats) NIL (" ACK = ~D"))
        `(:function ts-packets-while-ack-delayed (,stats) NIL (" (useful = ~D)"))
        `(:function ts-failed-send-timeouts (,stats) NIL (" failed send = ~D"))
        )
      ))
  )
(defop (tcp-ip-transport-protocol :peek-final-fields) (tp)
  (ncons (TV:SCROLL-MAINTAIN-LIST
           #'(LAMBDA () (tcp-user-socket-alist tp))
           'peek-tcp-socket)))

(defun peek-tcp-socket (elt)
  (let ((socket (cdr elt)))
    (send socket :peek-item elt)))

(defvar *tcp-stream* nil "The ip-transport-protocol for TCP")
(defvar *tcp-receives-out* 8 "Number of outstanding receives")
(defvar *tcp-logging* nil "If T, save headers on *tcp-log*")
(defvar *tcp-log-keywords* nil "Unless NIL, a list of keywords that get logged")
(defvar *tcp-log-level* 0 "Greater number means more logging for a given keyword")

(defmacro tcp-log (keyword socket &rest args)
  `(when *tcp-logging*
     (tcp-log-internal ,keyword ,socket ,@args)))

(defconstant tcp-protocol 6 "IP protocol type field for TCP")

(defun setup-tcp ()
  (setq *tcp-stream* (make-tcp-ip-transport-protocol :keyword :tcp
                                                     :type tcp-protocol
                                                     :gauge-name "TCP"
                                                     :close-function 'tcp-close
                                                     :reset-function 'tcp-reset
                                                     :receive-function 'receive-tcp-packet
                                                     :icmp-notification-function 'tcp-icmp
                                                     :broadcast-allowed-p nil
                                                     ))

  (initialize-tcp-background-process)
  (send *tcp-stream* :open)
  (send *tcp-stream* :enable)
  (dotimes (i *tcp-receives-out*)               ;and give IP some read buffers
    (send *tcp-stream* :receive (get-tcp-buffer)))
  t)

(defparameter *tcp-buffer-data-size* 1460 "Amount of user data in a tcp-buffer")

(defvar *free-tcp-buffers* nil "List of free TCP message buffers")

(defun get-tcp-buffer ()
  "returns an array to hold a TCP message."
  (do ((buffer nil))
      (buffer buffer)
    (if *free-tcp-buffers*
        (without-interrupts
          (setq buffer (pop *free-tcp-buffers*)))
      (setq buffer (zl:make-array (+ *tcp-buffer-data-size* 20) ; + 20 IP = 1500 bytes
                                  :element-type '(unsigned-byte 8)
                                  :fill-pointer 0
                                  :leader-length 2
                                  :named-structure-symbol 'tcp-buffer)))))

(defun free-tcp-buffer (tcp)
  (without-interrupts
    (push tcp *free-tcp-buffers*))
  nil)

(defun tcp-buffer-p (buffer)
  (eq (named-structure-p buffer) 'tcp-buffer))

(defvar *free-tcp-headers* nil "List of free TCP headers")

(defun get-tcp-header ()
  "returns an array to hold a TCP header."
  (do ((header nil))
      (header header)
    (if *free-tcp-headers*
        (without-interrupts
          (setq header (pop *free-tcp-headers*)))
      (setq header (make-array 24
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0)))))

(defun free-tcp-header (tcp)
  (without-interrupts
    (push tcp *free-tcp-headers*))
  nil)

;;;Timeout constants
(defparameter *rto-alpha* 0.9 "Smoothing factor for round-trip-time calculation")
(defparameter *rto-beta* 3.0 "Multiple of smoothed-round-trip-time that is retransmission timeout")
(defparameter *rto-lbound* 1 "Lower bound on retransmission timeout in seconds")
(defparameter *rto-ubound* 60 "Upper bound on retransmission timeout in seconds")

(defparameter *msl* 60 "Maximum Segment Lifetime")

(defparameter *ack-delay* 0.30 "Delay in seconds for ACK segment")
(defparameter *delayed-ack-sample* 10 "Minimum number of delayed ACKs to determine if they are doing good")
(defparameter *delayed-ack-proportion* 0.40 "Proportion of delayed ACKs which must be useful")
(defparameter *window-probe-interval* 60 "Delay in seconds between zero window probes")

;;;TCP header definitions

(defsubst tcp-source-port (tcp)
  (dpb (aref tcp 0) (byte 8 8) (aref tcp 1)))
(defun set-tcp-source-port (tcp val)
  (setf (aref tcp 0) (ldb (byte 8 8) val))
  (setf (aref tcp 1) (ldb (byte 8 0) val))
  val)
(defsetf tcp-source-port set-tcp-source-port)

(defsubst tcp-destination-port (tcp)
  (dpb (aref tcp 2) (byte 8 8) (aref tcp 3)))
(defun set-tcp-destination-port (tcp val)
  (setf (aref tcp 2) (ldb (byte 8 8) val))
  (setf (aref tcp 3) (ldb (byte 8 0) val))
  val)
(defsetf tcp-destination-port set-tcp-destination-port)

(defsubst tcp-sequence-number (tcp)
  (dpb (aref tcp 4) (byte 8 24)
       (dpb (aref tcp 5) (byte 8 16)
            (dpb (aref tcp 6) (byte 8 8)
                 (aref tcp 7)))))
(defun set-tcp-sequence-number (tcp val)
  (setf (aref tcp 4) (ldb (byte 8 24) val))
  (setf (aref tcp 5) (ldb (byte 8 16) val))
  (setf (aref tcp 6) (ldb (byte 8 8) val))
  (setf (aref tcp 7) (ldb (byte 8 0) val))
  val)
(defsetf tcp-sequence-number set-tcp-sequence-number)

(defsubst tcp-ack-number (tcp)
  (dpb (aref tcp 8) (byte 8 24)
       (dpb (aref tcp 9) (byte 8 16)
            (dpb (aref tcp 10) (byte 8 8)
                 (aref tcp 11)))))
(defun set-tcp-ack-number (tcp val)
  (setf (aref tcp 8) (ldb (byte 8 24) val))
  (setf (aref tcp 9) (ldb (byte 8 16) val))
  (setf (aref tcp 10) (ldb (byte 8 8) val))
  (setf (aref tcp 11) (ldb (byte 8 0) val))
  val)
(defsetf tcp-ack-number set-tcp-ack-number)

(defsubst tcp-data-offset (tcp)
  "In 32 bit words"
  (ldb (byte 4 4) (aref tcp 12)))
(defsubst tcp-first-data-byte (tcp)
  (* 4 (tcp-data-offset tcp)))

(defsubst tcp-reserved-1 (tcp)
  (ldb (byte 4 0) (aref tcp 12)))
(defsubst tcp-reserved-2 (tcp)
  (ldb (byte 2 6) (aref tcp 13)))

(defsubst tcp-flags (tcp)
  (ldb (byte 6 0) (aref tcp 13)))
(defconstant URG #o40 "Urgent bit")
(defconstant ACK #o20 "Ack bit")
(defconstant PUSH #o10 "Push bit")
(defconstant RST #o04 "Reset bit")
(defconstant SYN #o02 "SYN bit")
(defconstant FIN #o01 "FIN bit")

(defsubst tcp-urg-bit (tcp)
  (ldb (byte 1 5) (aref tcp 13)))
(defsubst tcp-urg-bit-p (tcp)
  (ldb-test (byte 1 5) (aref tcp 13)))

(defsubst tcp-ack-bit (tcp)
  (ldb (byte 1 4) (aref tcp 13)))
(defsubst tcp-ack-bit-p (tcp)
  (ldb-test (byte 1 4) (aref tcp 13)))

(defsubst tcp-psh-bit (tcp)
  (ldb (byte 1 3) (aref tcp 13)))
(defsubst tcp-psh-bit-p (tcp)
  (ldb-test (byte 1 3) (aref tcp 13)))

(defsubst tcp-rst-bit (tcp)
  (ldb (byte 1 2) (aref tcp 13)))
(defsubst tcp-rst-bit-p (tcp)
  (ldb-test (byte 1 2) (aref tcp 13)))

(defsubst tcp-syn-bit (tcp)
  (ldb (byte 1 1) (aref tcp 13)))
(defsubst tcp-syn-bit-p (tcp)
  (ldb-test (byte 1 1) (aref tcp 13)))

(defsubst tcp-fin-bit (tcp)
  (ldb (byte 1 0) (aref tcp 13)))
(defsubst tcp-fin-bit-p (tcp)
  (ldb-test (byte 1 0) (aref tcp 13)))

(defsubst tcp-window (tcp)
  (dpb (aref tcp 14) (byte 8 8) (aref tcp 15)))
(defun set-tcp-window (tcp val)
  (setf (aref tcp 14) (ldb (byte 8 8) val))
  (setf (aref tcp 15) (ldb (byte 8 0) val))
  val)
(defsetf tcp-window set-tcp-window)

(defsubst tcp-checksum (tcp)
  (dpb (aref tcp 16) (byte 8 8) (aref tcp 17)))
(defun set-tcp-checksum (tcp val)
  (setf (aref tcp 16) (ldb (byte 8 8) val))
  (setf (aref tcp 17) (ldb (byte 8 0) val))
  val)
(defsetf tcp-checksum set-tcp-checksum)

(defsubst tcp-urgent-pointer (tcp)
  (dpb (aref tcp 18) (byte 8 8) (aref tcp 19)))
(defun set-tcp-urgent-pointer (tcp val)
  (setf (aref tcp 18) (ldb (byte 8 8) val))
  (setf (aref tcp 19) (ldb (byte 8 0) val))
  val)
(defsetf tcp-urgent-pointer set-tcp-urgent-pointer)

;;;TCP Options

(defmacro define-tcp-option (name number)
  (let ((nn (intern (string-append "TCP-OPT-" name))))
    `(eval-when (load compile eval)
       (defconstant ,nn ,number))))

(define-tcp-option end 0)
(define-tcp-option nop 1)
(define-tcp-option mss 2)

(defun describe-tcp (tcp &optional display-data-p)
  "Given an art-8b array, displays the TCP message in it"
  (format t "~&Source-port: ~20t~D" (tcp-source-port tcp))
  (format t "~&Destination-port: ~20t~D" (tcp-destination-port tcp))
  (format t "~&Sequence number: ~20t~X" (tcp-sequence-number tcp))
  (format t "~&Ack number: ~20t~X" (tcp-ack-number tcp))
  (format t "~&Header length: ~20t~D" (* 4 (tcp-data-offset tcp)))
  (format t "~&Flags: ~20t~O" (tcp-flags tcp))
  (describe-flags (tcp-flags tcp))
  (format t "~&Window: ~20t~D" (tcp-window tcp))
  (format t "~&Checksum: ~20t~X" (tcp-checksum tcp))
  (format t "~&Urgent-pointer: ~20t~D" (tcp-urgent-pointer tcp))
  (when (> (tcp-data-offset tcp) 5)
    (describe-tcp-options tcp))

  (when (and display-data-p (> (length tcp) (tcp-first-data-byte tcp)))
    (if (numberp display-data-p)
        (format t "~&~D bytes of data:" display-data-p)
      (format t "~&rest of data:"))
    (do* ((offset (tcp-first-data-byte tcp) (1+ offset))
          (length (- (length tcp) offset))
          (count (if (numberp display-data-p) (min length display-data-p) length))
          (index 0 (1+ index)))
         ((= index count) nil)
      (if (zerop (mod index 32))
          (format t "~&"))
      (format t "~16,2,'0r " (aref tcp offset))))

  tcp)

(defun describe-tcp-options (tcp)
  (let ((data-offset (tcp-first-data-byte tcp)))
    (when (> data-offset 20)
      (format t "~&TCP Options: ~19t")
      (do* ((offset 20))
           ((>= offset data-offset))
        (case (aref tcp offset)
          (#.tcp-opt-end
           (format t " END")
           (incf offset))
          (#.tcp-opt-nop
           (format t " NOP")
           (incf offset))
          (#.tcp-opt-mss
           (format t
                   " MSS(~D) = ~D"
                   (aref tcp (incf offset))
                   (+ (ash (aref tcp (incf offset)) 8)
                      (aref tcp (incf offset))))
           (incf offset))
          (otherwise
           (format t " UNK(~D,~D)" (aref tcp offset) (aref tcp (1+ offset)))
           (incf offset (aref tcp (1+ offset)))))))))

(defun describe-flags (flags)
  (format t "~:[ URG~;~]~:[ ACK~;~]~:[ PUSH~;~]~:[ RST~;~]~:[ SYN~;~]~:[ FIN~;~]"
          (zerop (logand flags URG))
          (zerop (logand flags ACK))
          (zerop (logand flags PUSH))
          (zerop (logand flags RST))
          (zerop (logand flags SYN))
          (zerop (logand flags FIN))))

(defun copy-tcp-header (from to)
  (let ((length (min 24 (tcp-first-data-byte from))))
    (setf (fill-pointer to) length)
    (copy-array-portion from 0 length to 0 length))
  to)

(defun check-tcp-checksum (tcp ip local-p)
  (when (and (not (tcp-local-checksums *tcp-stream*)) local-p)
    (return-from check-tcp-checksum t))
  (let* ((count (length tcp))
         (sum (checksum-1 tcp (pseudo-header-checksum ip count) count)))
    (values (zerop sum) sum)))

(defun store-tcp-checksum (buffers ip local-p)
  (unless (consp buffers)
    (setq buffers (ncons buffers)))
  (setf (tcp-checksum (first buffers)) 0)
  (when (and (not (tcp-local-checksums *tcp-stream*)) local-p)
    (return-from store-tcp-checksum t))
  (let ((sum (pseudo-header-checksum ip (apply '+ (mapcar 'length buffers)))))
    (setf (tcp-checksum (first buffers)) (checksum buffers sum))))

;;;User interface to TCP

(defstream tcp-socket
           ()
           "TCP-USER-"
  (keyword nil)                                 ; Keyword to identify this TCP user
  (local-port nil)                              ; Local Port number of this TCP user
  (remote-port nil)                             ; Remote Port number, if specified
  (remote-address nil)                          ; Remote Address, if specified
  (SND.UNA 0)                                   ; Sequence number of first unacknowledged octet
  (SND.NXT 0)                                   ; Sequence number of next octet to send
  (SND.WND 0)                                   ; Send Window
  (SND.UP nil)                                  ; Send Urgent Pointer
  (SND.WL1 0)                                   ; Sequence number used for last window update
  (SND.WL2 0)                                   ; Ack number used for last window update
  (ISS 0)                                       ; Initial Send Sequence number
  (RCV.NXT 0)                                   ; Next expected sequence number to receive
  (RCV.WND 0)                                   ; Receive Window
  (RCV.UP nil)                                  ; Receive Urgent Pointer
  (IRS 0)                                       ; Initial Receive Sequence number
  (state nil)                                   ; Connection state
  (active nil)                                  ; T if user, NIL if server
  (fully-specified nil)                         ; T if remote-port and remote-address specified
  (saved-packets nil)                           ; list of packets saved without user receive buffers
  (packet-list nil)                             ; list of received packets and asynchronous items
  (statistics-block (make-statistics-block))    ; Network statistics block maintained at clock level
  (active-gauges nil)                           ; list of gauges in control panel
  (inactive-gauges nil)                         ; list of gauges not currently in control panel
  (gauge-name nil)                              ; the name of this protocol for its gauges
  (remote-mss nil)                              ; The MSS specified by remote host
  (interface-mss nil)                           ; The MSS implied by our local network
  (local-host-p nil)                            ; T if to a local host, i.e. self or shared backplane
  (ip-header nil)                               ; The prototype IP header for this connection
  (precedence nil)                              ; The precedence of this connection (from IP header)
  (security-options nil)                        ; Security options of this connection (from IP header)
  (open-timeout nil)                            ; User specified open timeout
  (send-timeout nil)                            ; User specified send timeout
  (smoothed-round-trip-time nil)                ; Measured Round trip time
  (retransmission-timeout nil)                  ; retransmission timeout, based on srtt
  (last-time nil)                               ; the time when we started the last timeout
  (timeout-alist nil)                           ; List of timeouts: (function . time)
  (cumulative-timeout nil)                      ; Current total time without acknowledgement
  (send-data nil)                               ; List of buffers to be sent
  (send-data-length 0)                          ; Amount of data in send-data
  (send-data-offset 0)                          ; Byte offset into buffers of SND.NXT
  (receive-data nil)                            ; List of buffers to be filled with received data
  (receive-data-length 0)                       ; Amount of space in receive-data
  (receive-data-offset 0)                       ; Byte offset into buffers of RCV.NXT
  (closed-p nil)                                ; T if user has closed connection
  (local-fin-sequence-number nil)               ; sequence number of our FIN
  (remote-fin-sequence-number nil)              ; sequence number of remote FIN
  (rtt-sequence-time-alist nil)                 ; alist of sequence numbers / times sent for RTT calculation
  (stats (make-tcp-statistics))                 ; Big pile of statistics
  (auto-push nil)                               ; If T, generate a PUSH for each sequence of data
  (push-sequence-number nil)                    ; Sequence number user specified got PUSHed
  (SND.LAST 0)                                  ; The highest sequence number we've sent
  (RCV.LAST 0)                                  ; The highest valid sequence we've received
  (discard-p nil)                               ; T if we are discarding data after CLOSE
  (lock nil)                                    ; Interlock on SND.NXT, send-data, RCV.NXT, receive-data, etc.
  (optimistic-window-p nil)                     ; T if optimistic window desired
  (ack-needed-p nil)                            ; T if should send an ACK
  (ack-delay nil)                               ; The ACK delay for this connection
  (fin-needed-p nil)                            ; T if need to send a FIN
  )

;;Security options
(defsubst tcp-user-security (tp)
  (first (tcp-user-security-options tp)))
(defsubst tcp-user-compartment (tp)
  (second (tcp-user-security-options tp)))
(defsubst tcp-user-handling (tp)
  (third (tcp-user-security-options tp)))
(defsubst tcp-user-tcc (tp)
  (fourth (tcp-user-security-options tp)))
(defsubst security (options)
  (first options))
(defsubst compartment (options)
  (second options))
(defsubst handling (options)
  (third options))
(defsubst tcc (options)
  (fourth options))

(defsubst SND.UNA (tp)
  (tcp-user-SND.UNA tp))
(defsubst SND.NXT (tp)
  (tcp-user-SND.NXT tp))
(defsubst SND.WND (tp)
  (tcp-user-SND.WND tp))
(defsubst SND.UP (tp)
  (tcp-user-SND.UP tp))
(defsubst SND.WL1 (tp)
  (tcp-user-SND.WL1 tp))
(defsubst SND.WL2 (tp)
  (tcp-user-SND.WL2 tp))
(defsubst SND.LAST (tp)
  (tcp-user-SND.LAST tp))
(defsubst ISS (tp)
  (tcp-user-ISS tp))
(defsubst RCV.NXT (tp)
  (tcp-user-RCV.NXT tp))
(defsubst RCV.WND (tp)
  (tcp-user-RCV.WND tp))
(defsubst RCV.UP (tp)
  (tcp-user-RCV.UP tp))
(defsubst RCV.LAST (tp)
  (tcp-user-RCV.LAST tp))
(defsubst IRS (tp)
  (tcp-user-IRS tp))

(defsubst tcp-user-packets-sent (tu)
  (aref (tcp-user-statistics-block tu) STAT-PS STAT-CURR))
(defsubst tcp-user-packets-received (tu)
  (aref (tcp-user-statistics-block tu) STAT-PR STAT-CURR))
(defsubst tcp-user-bytes-sent (tu)
  (aref (tcp-user-statistics-block tu) STAT-BS STAT-CURR))
(defsubst tcp-user-bytes-received (tu)
  (aref (tcp-user-statistics-block tu) STAT-BR STAT-CURR))

(defsubst tcp-user-enabled (socket)
  (rassoc socket (tcp-user-socket-alist *tcp-stream*) :test #'eq))

(defmacro change-state (tp new-state)
  `(progn
     (tcp-log :state ,tp (tcp-user-state ,tp) ,new-state)
     (setf (tcp-user-state ,tp) ,new-state)))

(defun tcp-stat-incf (socket stat &optional (value 1))
  (when socket
    (incf (aref (tcp-user-stats socket) stat) value))
  (incf (aref (tcp-stats *tcp-stream*) stat) value))

;;;The element stored on the receive-data and saved-packets fifos
(defstruct (rcv-data
             (:conc-name "RCV-")
             (:type :list))
  buffer                                        ;The buffer supplied by the user
  holes                                         ;The list of unfilled-in data
  push-offset                                   ;If PUSHed, last offset stored in this buffer
  )

;;;a hole is just a dotted pair -- start and end offsets
(defmacro hole-start (hole)
  `(car ,hole))

(defmacro hole-end (hole)
  `(cdr ,hole))

;;;Peek item for a TCP socket

(defop (tcp-socket :peek-item) (elt)
  (let ((socket (cdr elt)))
    (list `(:PRE-PROCESS-FUNCTION tcp-socket-insert-special-fields :socket ,socket)
          (tv:scroll-parse-item
            :leader 2
            `(:MOUSE-ITEM
               (NIL :MENU-CHOOSE
                    ("TCP socket operations"
                     ("Close" :EVAL  (when (tv:mouse-y-or-n-p "Close this TCP socket")
                                       (funcall socket :close))
                      :DOCUMENTATION
                      "Click left to close this TCP socket.")
                     ("Abort" :EVAL  (when (tv:mouse-y-or-n-p "Abort this TCP socket")
                                       (funcall socket :abort))
                      :DOCUMENTATION
                      "Click left to abort this TCP socket.")
                     ("Inspect" :EVAL (send tv:selected-window :force-kbd-input `(inspect ,socket))
                      :DOCUMENTATION
                      "Click left to INSPECT this TCP socket.")
                     ("Describe" :EVAL (send tv:selected-window :force-kbd-input `(describe ,socket))
                      :DOCUMENTATION
                      "Click left to DESCRIBE this TCP socket.")
                     ("Brief" :eval (tcp-socket-level tv:item :brief)
                      :documentation
                      "Click left to choose brief display for this TCP socket.")
                     ("Normal" :eval (tcp-socket-level tv:item :normal)
                      :documentation
                      "Click left to choose normal display for this TCP socket.")
                     ("Verbose" :eval (tcp-socket-level tv:item :verbose)
                      :documentation
                      "Click left to choose verbose display for this TCP socket."))
                    :DOCUMENTATION
                    "Menu of things to do to this TCP socket."
                    :BINDINGS
                    ((socket ',socket)))
               :FUNCTION tcp-user-keyword (,socket) NIL ("   TCP socket ~D"))
            `(:function tcp-user-active (,socket) NIL (" is ~:[passive~;active~]"))
            `(:function tcp-user-state (,socket) NIL (" in state ~A"))
            `(:function tcp-user-local-port (,socket) NIL ("  ~D"))
            `(:function tcp-user-remote-port (,socket) NIL (" -> ~D"))
            `(:function ,#'(lambda (socket)
                             (if (tcp-user-remote-address socket)
                                 (ip:canonical-ip (tcp-user-remote-address socket))
                               nil)) (,socket) NIL (" [~A] "))
            `(:mouse-item
               (nil :buttons
                    ((nil :eval (funcall socket (if (tcp-user-active-gauges socket) :kill-gauges :make-gauges))
                          :bindings ((socket ',socket)))
                     (nil :eval (ignore))
                     (nil :eval (net:gauge-menu socket (tcp-user-active-gauges socket))
                          :bindings ((socket ',socket))))
                    :documentation
                    "Click left for default, right for menu"
                    ((socket ',socket)))
               :function ,#'(lambda (n) (mapcar 'car (tcp-user-active-gauges n))) (,socket) NIL ("Gauges: ~A"))))))

(defun tcp-socket-level (item level)
  (when (null (array-leader item (1+ tv:scroll-item-leader-offset)))
    (setf (array-leader item (1+ tv:scroll-item-leader-offset)) :brief))
  (unless (eq level (array-leader item (1+ tv:scroll-item-leader-offset)))
    (setf (array-leader item tv:scroll-item-leader-offset) nil)
    (setf (array-leader item (1+ tv:scroll-item-leader-offset)) level)))

(defun tcp-socket-insert-special-fields (item &aux socket level)
  "A pre-process function to insert tcp-socket specific fields in the display"
  (let ((first-item (first (tv:scroll-item-component-items item))))
    (unless (array-leader first-item tv:scroll-item-leader-offset)
      (setq level (array-leader first-item (1+ tv:scroll-item-leader-offset)))
      (setq socket (getf (tv:scroll-item-plist item) :socket))
      (setf (tv:scroll-item-component-items item)
            (nconc (list (first (tv:scroll-item-component-items item)))
                   (if (member level '(:normal :verbose)) (tcp-socket-normal-fields socket))
                   (if (member level '(:verbose)) (tcp-socket-verbose-fields socket))))
      (setf (array-leader first-item tv:scroll-item-leader-offset) t))))

(defun tcp-socket-normal-fields (socket)
  (list
    (net:sent-statistics (tcp-user-statistics-block socket) "    ")
    (net:rcvd-statistics (tcp-user-statistics-block socket) "    ")
    (tv:scroll-parse-item
      `(:function ,#'(lambda (n) (or (ISS n) 0)) (,socket) NIL ("    ISS = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (SND.WND n) 0)) (,socket) NIL (" SND.WND = ~5D"))
      `(:function ,#'(lambda (n) (or (SND.NXT n) 0)) (,socket) NIL (" SND.NXT = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (SND.UP n) 0)) (,socket) NIL (" SND.UP = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (SND.WL2 n) 0)) (,socket) NIL (" SND.WL2 = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (SND.UNA n) 0)) (,socket) NIL (" SND.UNA = ~16,8,'0R")))
    (tv:scroll-parse-item
      `(:function ,#'(lambda (n) (or (IRS n) 0)) (,socket) NIL ("    IRS = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (RCV.WND n) 0)) (,socket) NIL (" RCV.WND = ~5D"))
      `(:function ,#'(lambda (n) (or (RCV.NXT n) 0)) (,socket) NIL (" RCV.NXT = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (RCV.UP n) 0)) (,socket) NIL (" RCV.UP = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (SND.WL1 n) 0)) (,socket) NIL (" SND.WL1 = ~16,8,'0R"))
      `(:function ,#'(lambda (n) (or (tcp-user-remote-fin-sequence-number n) 0))
                  (,socket) NIL ("     FIN = ~16,8,'0R")))
    (tv:scroll-parse-item
      `(:function tcp-user-remote-mss (,socket) NIL ("    Remote MSS = ~D"))
      `(:function tcp-user-interface-mss (,socket) NIL (" Local MSS = ~D"))
      `(:function tcp-user-precedence (,socket) NIL (" Precedence = ~D"))
      `(:function tcp-user-security (,socket) NIL (" Security = ~D"))
      `(:function tcp-user-compartment (,socket) NIL (" Compartment = ~D"))
      `(:function tcp-user-handling (,socket) NIL (" Handling = ~D"))
      `(:function tcp-user-tcc (,socket) NIL (" TCC = ~D")))
    (tv:scroll-parse-item
      `(:function tcp-user-open-timeout (,socket) NIL ("    Timeouts: Open = ~D"))
      `(:function tcp-user-send-timeout (,socket) NIL (" Send = ~D"))
      `(:function tcp-user-cumulative-timeout (,socket) NIL (" Cumulative = ~D"))
      `(:function tcp-user-ack-delay (,socket) NIL (" ACK = ~D"))
      `(:function tcp-user-smoothed-round-trip-time (,socket) NIL (" SRTT = ~4F"))
      `(:function tcp-user-retransmission-timeout (,socket) NIL (" RTO = ~4F"))
      `(:function tcp-user-closed-p (,socket) NIL (" Closed = ~D")))
    (tv:scroll-parse-item
      `(:function tcp-user-send-data-length (,socket) NIL ("    Send Length = ~D"))
      `(:function tcp-user-send-data-offset (,socket) NIL (" Sent Unacknowledged = ~D"))
      `(:function ,#'(lambda (n) (- (tcp-user-send-data-length n)
                                    (tcp-user-send-data-offset n)))
                  (,socket) NIL (" Unsent = ~D"))
      `(:function tcp-user-receive-data-length (,socket) NIL (" Receive Length = ~D"))
      `(:function ,#'(lambda (n) (- (tcp-user-receive-data-length n)
                                    (tcp-user-receive-data-offset n)))
                  (,socket) NIL (" Receive Free = ~D")))
    ))

(defun tcp-socket-verbose-fields (socket)
  (let ((stats (tcp-user-stats socket)))
    (list
      (tv:scroll-parse-item
        "   "
        `(:function ts-data-packets-sent (,stats) NIL (" Data sent: packets = ~D"))
        `(:function ts-data-bytes-sent (,stats) NIL (", bytes = ~D"))
        `(:function ts-ack-bytes-received (,stats) NIL (", ACKed = ~D"))
        `(:function ts-data-bytes-received (,stats) NIL ("   Data received: bytes = ~D"))
        )
      (tv:scroll-parse-item
        "   "
        `(:function ts-ack-packets-requested (,stats) NIL (" ACK packets sent: req = ~D"))
        `(:function ts-ack-packets-sent (,stats) NIL (" sent = ~D"))
        `(:function ts-ack-packets-delayed (,stats) NIL (" delay = ~D"))
        `(:function ts-urg-packets-sent (,stats) NIL (" urg = ~D"))
        `(:function ts-window-probes-sent (,stats) NIL (" probe = ~D"))
        `(:function ts-rst-packets-sent (,stats) NIL (" rst = ~D"))
        )
      (tv:scroll-parse-item
        "   "
        `(:function ts-ack-packets-received (,stats) NIL (" ACK packets rcvd = ~D"))
        `(:function ts-duplicate-ack-packets-received (,stats) NIL (" dup = ~D"))
        `(:function ts-unsent-data-acks (,stats) NIL (" unsent = ~D"))
        `(:function ts-useless-ack-packets-received (,stats) NIL (" useless = ~D"))
        `(:function ts-urg-packets-received (,stats) NIL (" URG = ~D"))
        `(:function ts-window-probes-received (,stats) NIL (" probe = ~D"))
        `(:function ts-window-updates-received (,stats) NIL (" update = ~D"))
        `(:function ts-shrinking-window-updates (,stats) NIL (" shrink = ~D"))
        )
      (tv:scroll-parse-item
        "   "
        `(:function ts-in-sequence-packets (,stats) NIL (" Data packets rcvd: in-seq = ~D"))
        `(:function ts-total-duplicate-packets (,stats) NIL (" total = ~D"))
        `(:function ts-partially-duplicate-packets (,stats) NIL (" partial = ~D"))
        `(:function ts-out-of-order-packets (,stats) NIL (" order = ~D"))
        `(:function ts-out-of-window-packets (,stats) NIL (" window = ~D"))
        `(:function ts-bad-sequence-packets (,stats) NIL (" bad-seq = ~D"))
        )
      (tv:scroll-parse-item
        "   "
        `(:function ts-in-sequence-bytes (,stats) NIL (" Data bytes rcvd: in-seq = ~D"))
        `(:function ts-total-duplicate-bytes (,stats) NIL (" total = ~D"))
        `(:function ts-partially-duplicate-bytes (,stats) NIL (" partial = ~D"))
        `(:function ts-out-of-order-bytes (,stats) NIL (" order = ~D"))
        `(:function ts-out-of-window-bytes (,stats) NIL (" window = ~D"))
        )
      (tv:scroll-parse-item
        "   "
        `(:function ts-saved-bytes (,stats) NIL (" Bytes saved = ~D"))
        `(:function ts-retrieved-bytes (,stats) NIL (" retrieved = ~D"))
        `(:function ts-dropped-bytes (,stats) NIL (" dropped = ~D"))
        `(:function ts-packet-allocation-delays (,stats) NIL ("   Alloc delays = ~D"))
        `(:function ts-failed-sends (,stats) NIL ("   Failed sends = ~D"))
        )
      (tv:scroll-parse-item
        "   "
        `(:function ts-rtt-attempts (,stats) NIL (" RTT: attempts = ~D"))
        `(:function ts-rtt-updates (,stats) NIL (" updates = ~D"))
        `(:function ts-retransmission-timeouts (,stats) NIL ("   Timeouts: RTO = ~D"))
        `(:function ts-user-timeouts (,stats) NIL (" user = ~D"))
        `(:function ts-acks-sent-due-to-timer (,stats) NIL (" ACK = ~D"))
        `(:function ts-packets-while-ack-delayed (,stats) NIL (" (useful = ~D)"))
        `(:function ts-failed-send-timeouts (,stats) NIL (" failed send = ~D"))
        )
      )))

;;;Sequence number manipulation functions, courtesy of Pace Willison

(defsubst 32-bit-< (a b)
  (ldb-test (byte 1 31.) (- a b)))

(defsubst 32-bit-<= (a b)
  (or (= a b)
      (ldb-test (byte 1 31.) (- a b))))

(defsubst 32-bit->= (a b)
  (not (ldb-test (byte 1 31.) (- a b))))

(defsubst 32-bit-> (a b)
  (and (not (= a b))
       (not (ldb-test (byte 1 31.) (- a b)))))

(defsubst 32-bit-plus (a b)
  (logand #o37777777777 (+ a b)))

(defsubst 32-bit-minus (a b)
  (logand #o37777777777
          (- (+ a #o40000000000) b)))

(defsubst 32-bit-max (a b)
  (if (32-bit-< b a) a b))

(defsubst 32-bit-min (a b)
  (if (32-bit-< b a) b a))

;;;User Interface methods

(defop (tcp-socket :open) (&key active local-port remote-port remote-address auto-push (optimistic t)
                                open-timeout send-timeout ip-header-options
                                (initial-gauges nil ig-p) gauge-name
                                &aux header parsed-remote-address)
  (unless (tcp-user-enabled self)
    (when (tcp-enabled *tcp-stream*)
      (assert (or (setq parsed-remote-address nil)      ;This branch always fails, but reinitializes local
                  (cond ((null remote-port)
                         ;;If no remote-port, must be inactive and have no remote-address
                         (and (not active) (null remote-address)))
                        ((setq parsed-remote-address (parse-internet-address remote-address))
                         ;;If have a remote-port, remote-address must be valid AND remote-port must be valid
                         (typep remote-port '(unsigned-byte 16)))
                        (t
                         ;;If remote-port and remote-address invalid, error
                         nil)))
              (remote-port remote-address)
              "~[Active connection must specify both REMOTE-PORT and REMOTE-ADDRESS~;REMOTE-PORT ~D out of range~;REMOTE-ADDRESS ~S unknown~;REMOTE-PORT and REMOTE-ADDRESS must be both nil or both non-nil~]"
              (cond ((null remote-port) (if active 0 3))
                    (parsed-remote-address 1)
                    (t 2))
              (if parsed-remote-address remote-port remote-address))
      (if local-port
          (assert (and (typep local-port '(unsigned-byte 16))
                       (unused-local-port local-port remote-port remote-address))
                  (local-port)
                  "LOCAL-PORT ~D is ~:[out of range~;already in use~]"
                  local-port
                  (typep local-port '(unsigned-byte 16)))
        (setq local-port (assign-local-port remote-port remote-address)))
      (setf (SND.UNA self) 0)
      (setf (SND.NXT self) 0)
      (setf (SND.WND self) 0)
      (setf (SND.UP self) nil)
      (setf (SND.WL1 self) 0)
      (setf (SND.WL2 self) 0)
      (setf (ISS self) 0)
      (setf (RCV.NXT self) 0)
      (setf (RCV.WND self) 0)
      (setf (RCV.UP self) nil)
      (setf (IRS self) 0)
      (setf (tcp-user-active self) active)
      (setf (tcp-user-fully-specified self) (not (null remote-port)))
      (setf (tcp-user-local-port self) local-port)
      (setf (tcp-user-remote-port self) remote-port)
      (setf (tcp-user-remote-address self) parsed-remote-address)
      (setf (tcp-user-packet-list self) (make-fifo))
      (setf (tcp-user-saved-packets self) (make-fifo))
      (let ((buffer (get-tcp-buffer)))
        (push-fifo (make-rcv-data :buffer buffer :holes (ncons (cons 0 (1- (array-length buffer)))))
                   (tcp-user-saved-packets self)))
      (setf (tcp-user-smoothed-round-trip-time self) nil)
      (setf (tcp-user-retransmission-timeout self) *rto-lbound*)
      (setf (tcp-user-send-data self) (make-fifo))
      (setf (tcp-user-send-data-length self) 0)
      (setf (tcp-user-send-data-offset self) 0)
      (setf (tcp-user-receive-data self) (make-fifo))
      (setf (tcp-user-receive-data-length self) 0)
      (setf (tcp-user-receive-data-offset self) 0)
      (setf (tcp-user-auto-push self) auto-push)
      (setf (tcp-user-push-sequence-number self) NIL)
      (setf (SND.LAST self) 0)
      (setf (RCV.LAST self) 0)
      (setf (tcp-user-gauge-name self) (or gauge-name (format nil "TCP ~D" (tcp-user-local-port self))))
      (array-initialize (tcp-user-statistics-block self) 0)
      (array-initialize (tcp-user-stats self) 0 1)
      (dolist (g (tcp-user-inactive-gauges self))
        (send (cdr g) :set-margin-name (tcp-user-gauge-name self)))
      (when ig-p
        ;;If initial-gauges specified, NIL means no gauges, T means default, anything else specifies gauges
        ;;The reason to add no gauges is to have the statistics block looked at by the clock function
        (if (eq initial-gauges t)
            (setq initial-gauges '(:apr :aps :abr :abs)))
        (set-gauges (tcp-user-statistics-block self)
                    (locf (tcp-user-active-gauges self))
                    (locf (tcp-user-inactive-gauges self))
                    (tcp-user-gauge-name self)
                    initial-gauges)
        (add-network-statistics-block (tcp-user-statistics-block self)))
      (setf (tcp-user-remote-mss self) 536)     ;The default MSS
      (setq header (apply 'make-ip-header (append ip-header-options
                                                  (if parsed-remote-address `(:destination ,parsed-remote-address))
                                                  '(:ttl 60)    ;drop segments after 60 seconds
                                                  `(:protocol ,tcp-protocol))))
      (setf (tcp-user-ip-header self) header)
      (multiple-value-bind (precedence security compartment handling tcc)
          (find-security-and-precedence header)
        (setf (tcp-user-precedence self) precedence)
        (setf (tcp-user-security-options self) (list security compartment handling tcc)))
      (setf (tcp-user-open-timeout self) open-timeout)
      (setf (tcp-user-send-timeout self) send-timeout)
      (setf (tcp-user-cumulative-timeout self) 0)
      (setf (tcp-user-last-time self) nil)
      (setf (tcp-user-timeout-alist self) nil)
      (setf (tcp-user-rtt-sequence-time-alist self) nil)
      (setf (tcp-user-closed-p self) nil)
      (setf (tcp-user-local-fin-sequence-number self) nil)
      (setf (tcp-user-remote-fin-sequence-number self) nil)
      (setf (tcp-user-lock self) nil)
      (setf (tcp-user-discard-p self) nil)
      (setf (tcp-user-optimistic-window-p self) optimistic)
      (setf (tcp-user-ack-needed-p self) nil)
      (setf (tcp-user-ack-delay self) *ack-delay*)
      (setf (tcp-user-fin-needed-p self) nil)
      (push (cons local-port self) (tcp-user-socket-alist *tcp-stream*))
      (tcp-log :open self)
      (cond (active
             (tcp-stat-incf nil out-connection-requests)
             (set-interface-mss self parsed-remote-address header)
             (setf (RCV.WND self) (if optimistic        ;Initial window includes one max packet
                                      (tcp-user-interface-mss self)
                                    0))
             (change-state self :SYN-SENT)
             (setf (ISS self) (choose-iss))
             (setf (SND.UNA self) (ISS self))
             (setf (SND.NXT self) (ISS self))
             (setf (SND.LAST self) (ISS self))
             (send-tcp-packets self))
            (t
             (tcp-stat-incf nil in-connection-requests)
             (change-state self :LISTEN)
             (when open-timeout
               (start-timeout open-timeout 'listen-timeout self))))
      local-port)))

(defvar *transmission-time-multiple* 30.
  "Default TCP user timeout = this number times seconds to transmit one max packet over the network-interface")
(defvar *minimum-user-timeout* 30. "Minimum number of seconds for a TCP socket user timeout.")

(defun set-interface-mss (socket remote-address header)
  (multiple-value-bind (mss local-p transmission-time)
      (mss remote-address)
    (setf (tcp-user-interface-mss socket) (- mss (length header) 20))
    (setf (tcp-user-local-host-p socket) local-p)
    (unless (integerp (tcp-user-send-timeout socket))
      ;;Unless the user has specified a user timeout in 60'ths of seconds, choose one based on time
      ;;to transmit a maximum packet over the interface.  (mss) returns this time in seconds;
      ;;minimum user timeout is 30 times that, minimum of *minimum-user-timeout* seconds.
      (setf (tcp-user-send-timeout socket)
            (max *minimum-user-timeout*
                 (ceiling (* (or transmission-time 1.)
                             60.
                             *transmission-time-multiple*)))))))

(defun choose-iss ()
  "Choose an Initial Send Sequence Number"
  (logand #xffffffff (time:microsecond-time)))

(defun unused-local-port (local-port remote-port remote-address)
  (do ((list (tcp-user-socket-alist *tcp-stream*)))
      ((null list) t)
    (setq list (member local-port list :key #'car :test #'=))
    (and list
         (eql remote-port (tcp-user-remote-port (cdar list)))
         (eql remote-address (tcp-user-remote-address (cdar list)))
         (return nil))
    (setq list (cdr list))))

(defun assign-local-port (remote-port remote-address)
  (without-interrupts
    (do ((port (tcp-next-local-port *tcp-stream*) (max 256 (mod (1+ port) 65536))))
        ((unused-local-port port remote-port remote-address)
         (setf (tcp-next-local-port *tcp-stream*) (max 256 (mod (1+ port) 65536)))
         port))))

(defun close-tcp-socket (socket)
  (delete-network-statistics-block (tcp-user-statistics-block socket))
  (send socket :kill-gauges)
  (without-interrupts
    (let ((elt (rassoc socket (tcp-user-socket-alist *tcp-stream*) :test #'eq)))
      (setf (tcp-user-socket-alist *tcp-stream*) (delete elt (tcp-user-socket-alist *tcp-stream*)))
      (change-state socket :CLOSED))))

(defun flush-tcp-socket (socket &optional ignore)
  (close-tcp-socket socket)
  (free-tcp-user-resources socket))

(defun reset-connection (socket send-reset-p &optional reason)
  "Push a :RESET message on the packet list, returning all SENDs and RECEIVEs"
  (with-lock ((tcp-user-lock socket))
    (unless (eq (tcp-user-state socket) :closed)
      (close-tcp-socket socket)
      (cancel-timeout socket)
      (if send-reset-p
          (send-rst-for-connection socket reason)
        (tcp-log :reset-received socket reason))
      (free-tcp-user-resources socket)
      (let ((sends (fifo-as-list (tcp-user-send-data socket)))
            (receives (fifo-as-list (tcp-user-receive-data socket))))
        (setf (tcp-user-send-data socket) nil)
        (setf (tcp-user-receive-data socket) nil)
        (push-fifo `(:reset ,(mapcar #'rcv-buffer receives) ,sends)
                   (tcp-user-packet-list socket))))))

(defop (tcp-socket :abort) ()
  (tcp-log :abort self)
  (ecase (tcp-user-state self)
    (:CLOSED
     (close-tcp-socket self)
     ;;(error "connection does not exist")
     nil)
    ((:LISTEN :SYN-SENT :CLOSING :LAST-ACK :TIME-WAIT)
     (case (tcp-user-state self)
       (:LISTEN
        (tcp-stat-incf self in-connection-aborts))
       (:SYN-SENT
        (tcp-stat-incf self out-connection-aborts)))
     (reset-connection self nil "User aborted")
     t)
    ((:SYN-RECEIVED :ESTABLISHED :FIN-WAIT-1 :FIN-WAIT-2 :CLOSE-WAIT)
     (reset-connection self t "User aborted")
     t)))

(defop (tcp-socket :close) (&optional (mode :discard))
  ;;mode = nil or :discard -- throw away incoming data until FIN
  ;;mode = :normal -- allow further :receive's
  ;;mode = :abort or t -- abort connection
  (ecase mode
    ((:discard nil) (setq mode :discard))
    ((:abort t) (setq mode :abort))
    (:normal))
  (cond ((null (tcp-user-state self)))
        ((eq (tcp-user-state self) :closed))
        ((eq mode :abort)
         (send self :abort))
        (t
         ;;Normal graceful close
         (with-lock ((tcp-user-lock self))
           (tcp-log :close self)
           (cond ((setf (tcp-user-discard-p self) (eq mode :discard))
                  ;;Max out the window.  Some (broken) implementations treat the window as a SIGNED number...
                  (setf (RCV.WND self) (floor 65535 2))
                  (let ((saved-data (saved-but-unacknowledged-data self)))
                    (when (plusp saved-data)
                      ;;We have data saved but not given to user.  Acknowledge it all.
                      (setf (RCV.NXT self) (32-bit-plus (RCV.NXT self) saved-data))))
                  (do ((elt (pop-fifo (tcp-user-saved-packets self))
                            (pop-fifo (tcp-user-saved-packets self))))
                      ((null elt))
                    (free-tcp-buffer (rcv-buffer elt)))
                  (let ((receives (fifo-as-list (tcp-user-receive-data self))))
                    (setf (tcp-user-receive-data self) nil)
                    (setf (tcp-user-receive-data-length self) 0)
                    (setf (tcp-user-receive-data-offset self) 0)
                    (push-fifo `(:close ,(mapcar #'rcv-buffer receives)) (tcp-user-packet-list self))))
                 (t
                  (push-fifo '(:close) (tcp-user-packet-list self))))
           (ecase (tcp-user-state self)
             (:CLOSED
              ;;(error "connection does not exist")
              (push-fifo '(:closed) (tcp-user-packet-list self))
              nil)
             ((:LISTEN :SYN-SENT)
              (tcp-stat-incf self (if (eq (tcp-user-state self) :listen)
                                      in-connection-aborts
                                    out-connection-aborts))
              (flush-tcp-socket self)
              (push-fifo '(:closed) (tcp-user-packet-list self))
              t)
             (:SYN-RECEIVED
              (setf (tcp-user-closed-p self) t)
              (setf (tcp-user-fin-needed-p self) t)
              (when (fifo-empty-p (tcp-user-send-data self))
                (change-state self :FIN-WAIT-1)
                (send-tcp-packets self))
              t)
             (:ESTABLISHED
              (setf (tcp-user-closed-p self) t)
              (setf (tcp-user-fin-needed-p self) t)
              (change-state self :FIN-WAIT-1)
              (send-tcp-packets self)
              t)
             (:CLOSE-WAIT
              (setf (tcp-user-closed-p self) t)
              (setf (tcp-user-fin-needed-p self) t)
              (change-state self :CLOSING)
              (send-tcp-packets self)
              t)
             ((:FIN-WAIT-1 :FIN-WAIT-2 :CLOSING :LAST-ACK :TIME-WAIT)
              ;;(error "connection closing")
              nil))))))

(defun free-tcp-user-resources (tp)
  (do ((elt (pop-fifo (tcp-user-saved-packets tp))
            (pop-fifo (tcp-user-saved-packets tp))))
      ((null elt))
    (free-tcp-buffer (rcv-buffer elt)))
  (free-ip-header (tcp-user-ip-header tp))
  (setf (tcp-user-ip-header tp) nil))

(defop (tcp-socket :mss) ()
  (values (tcp-user-remote-mss self)
          (tcp-user-interface-mss self)))

(defop (tcp-socket :write-data) (buffer &key pushed urgent send-timeout)
  (check-type buffer (satisfies byte-array-or-string-p))
  (tcp-log :write self (length buffer))
  (with-lock ((tcp-user-lock self))
    (let ((last-data-sequence (32-bit-plus (SND.NXT self) (- (+ (tcp-user-send-data-length self)
                                                                (length buffer))
                                                             (tcp-user-send-data-offset self)
                                                             1))))
      (when pushed
        (setf (tcp-user-push-sequence-number self) last-data-sequence))
      (when urgent
        (setf (SND.UP self) last-data-sequence))))
  (when send-timeout
    (setf (tcp-user-send-timeout self) send-timeout))
  (ecase (tcp-user-state self)
    ((nil :CLOSED)
     ;;(error "connection does not exist"))
     nil)
    (:LISTEN
     (unless (tcp-user-remote-address self)
       (error "foreign socket unspecified"))
     (setf (tcp-user-active self) t)
     (set-interface-mss self (tcp-user-remote-address self) (tcp-user-ip-header self))
     (when (tcp-user-optimistic-window-p self)  ;Initial window includes one max packet
       (incf (RCV.WND self) (tcp-user-interface-mss self)))
     (change-state self :SYN-SENT)
     (setf (ISS self) (choose-iss))
     (setf (SND.UNA self) (ISS self))
     (setf (SND.NXT self) (ISS self))
     (setf (SND.LAST self) (ISS self))
     (incf (tcp-user-send-data-length self) (length buffer))
     (push-fifo buffer (tcp-user-send-data self))
     (send-tcp-packets self)
     t)
    ((:SYN-SENT :SYN-RECEIVED)
     (with-lock ((tcp-user-lock self))
       (incf (tcp-user-send-data-length self) (length buffer))
       (push-fifo buffer (tcp-user-send-data self)))
     t)
    ((:ESTABLISHED :CLOSE-WAIT)
     (with-lock ((tcp-user-lock self))
       (incf (tcp-user-send-data-length self) (length buffer))
       (push-fifo buffer (tcp-user-send-data self))
       (if (zerop (SND.WND self))
           (if urgent
               (send-urg-packet self)
             (zero-window-probe self))
         (if (zerop (tcp-user-cumulative-timeout self)) ;Only send a packet if not currently retransmitting.
             (send-tcp-packets self))))
     t)
    ((:FIN-WAIT-1 :FIN-WAIT-2 :CLOSING :LAST-ACK :TIME-WAIT)
     ;;(error "connection closing")
     nil)))

(defop (tcp-socket :reset-timeout) (&optional send-timeout)
  "Reset the timeout for a TCP socket.  If a connection timed out, this will restart transmission"
  (with-lock ((tcp-user-lock self))
    (when send-timeout
      (setf (tcp-user-send-timeout self) send-timeout))
    (setf (tcp-user-cumulative-timeout self) 0)
    (send-tcp-packets self t)))

(defop (tcp-socket :receive) (buffer)
  (assert (and (array-has-fill-pointer-p buffer) (byte-array-or-string-p buffer))
          (buffer)
          "The array must have a fill pointer and have elements of type (unsigned-byte 8)")
  (setf (fill-pointer buffer) (array-length buffer))
  (with-lock ((tcp-user-lock self))
    (let* ((length (length buffer))
           (elt (make-rcv-data :buffer buffer :holes (ncons (cons 0 (1- length))))))
      (tcp-log :read self length)
      (case (tcp-user-state self)
        (:CLOSED
         ;;(error "connection does not exist")
         nil)
        ((:LISTEN :SYN-SENT :SYN-RECEIVED)
         ;;Queue for processing after entering ESTABLISHED state...
         (copy-saved-data-to-user self elt)     ;won't reply to reads until ESTABLISHED
         t)
        ((:ESTABLISHED :FIN-WAIT-1 :FIN-WAIT-2)
         (unless (and (tcp-user-closed-p self) (tcp-user-discard-p self))
           (copy-saved-data-to-user self elt)
           (send-tcp-packets self)
           t))
        (:CLOSE-WAIT
         ;;Satisfy with data already present.  If none, "error:connection is closing"
         (cond ((32-bit-< (RCV.NXT self) (tcp-user-remote-fin-sequence-number self))
                (copy-saved-data-to-user self elt)
                (send-tcp-packets self))
               (t
                ;;(error "connection closing")
                ))
         nil)
        ((:CLOSING :LAST-ACK :TIME-WAIT)
         ;;(error "connection closing")
         nil)))))

(defop (tcp-socket :read-data) ()
  (pop-fifo (tcp-user-packet-list self)))

(defop (tcp-socket :listen) ()
  (not (fifo-empty-p (tcp-user-packet-list self))))

(defop (tcp-socket :remote-port) ()
  (and (tcp-enabled *tcp-stream*)
       (tcp-user-enabled self)
       (tcp-user-remote-port self)))

(defop (tcp-socket :remote-address) ()
  (and (tcp-enabled *tcp-stream*)
       (tcp-user-enabled self)
       (tcp-user-remote-address self)))

(defop (tcp-socket :local-port) ()
  (and (tcp-enabled *tcp-stream*)
       (tcp-user-enabled self)
       (tcp-user-local-port self)))

(defop (tcp-socket :local-address) ()
  (and (tcp-enabled *tcp-stream*)
       (tcp-user-enabled self)
       (tcp-user-ip-header self)
       (ip:ih-source-address (tcp-user-ip-header self))))

(defun multiple-connection-listen (list)
  "Given a LIST of sockets, return the first socket that has data waiting"
  (dolist (socket list)
    (when (not (fifo-empty-p (tcp-user-packet-list socket)))
      (return socket))))

(defun multiple-connection-kill-gauges (list)
  (let ((gauges nil))
    (without-interrupts
      (dolist (socket list)
        (do ((gauge (pop (tcp-user-active-gauges socket)) (pop (tcp-user-active-gauges socket))))
            ((null gauge))
          (push (cdr gauge) gauges)
          (push gauge (tcp-user-inactive-gauges socket)))))
    (when gauges
      (apply 'tv:delete-network-gauges gauges))))

(defop (tcp-socket :print-self) (stream &optional ignore ignore)
  (si:printing-random-object (self stream :type :no-pointer)
    (if (or (null (tcp-user-state self))
            (eq (tcp-user-state self) :CLOSED))
        (format stream "(CLOSED)")
      (format stream
              "(~A) ~D~:[~; -> ~:*~D [~A]~]"
              (tcp-user-state self)
              (tcp-user-local-port self)
              (tcp-user-remote-port self)
              (if (tcp-user-remote-address self)
                  (canonical-ip (tcp-user-remote-address self)))))))

(defop (tcp-socket :set-gauges) (gauge-list)
  "Set the active gauges for this tcp user socket"
  (when (tcp-user-enabled self)
    (add-network-statistics-block (tcp-user-statistics-block self))
    (set-gauges (tcp-user-statistics-block self)
                (locf (tcp-user-active-gauges self))
                (locf (tcp-user-inactive-gauges self))
                (tcp-user-gauge-name self)
                gauge-list)))

(defop (tcp-socket :make-gauges) (&optional (gauge-list '(:apr :aps :abr :abs)))
  "Add selected gauges for this tcp user socket"
  (when (tcp-user-enabled self)
    (add-network-statistics-block (tcp-user-statistics-block self))
    (add-gauges (tcp-user-statistics-block self)
                (locf (tcp-user-active-gauges self))
                (locf (tcp-user-inactive-gauges self))
                (tcp-user-gauge-name self)
                gauge-list)))

(defop (tcp-socket :kill-gauges) (&optional (gauge-list '(:ipr :apr :ips :aps :ibr :abr :ibs :abs)))
  "Delete selected gauges from a tcp user socket.  Default is to delete all gauges"
  (delete-gauges (locf (tcp-user-active-gauges self))
                 (locf (tcp-user-inactive-gauges self))
                 gauge-list))

(defun using-tcp-socket (host port keyword active-p receiver &optional (couldnt-open #'false) &aux header)
  (declare (zwei:indentation 4 2))
  (if (ip-header-p host)
      (setq host (ip:ih-dest-address (setq header host)))
    (let ((original-host host))
      (assert (numberp (setq host (parse-internet-address (setq original-host host))))
              (host)
              "~S is not a valid Internet host specification"
              original-host)))
  (let ((user-socket (make-tcp-socket :keyword keyword))
        (opened nil))
    (unwind-protect
        (if (setq opened (send user-socket :open :remote-port port :remote-address host :active active-p))
            (funcall receiver
                     user-socket
                     (or header (make-ip-header :destination host)))
          (funcall couldnt-open))
      (when opened
        (send user-socket :close)))))

(defun wait-for-tcp-enabled (interval)
  (process-wait-with-timeout "TCP Enabled"
                             interval
                             #'(lambda ()
                                 (and (boundp 'tcp:*tcp-stream*)
                                      tcp:*tcp-stream*
                                      (tcp-enabled tcp:*tcp-stream*))))
  (and (boundp 'tcp:*tcp-stream*)
       (tcp:tcp-enabled tcp:*tcp-stream*)))

;;;Sending TCP packets

(defvar *send-blocked-sockets* (make-fifo))
(defvar *send-blocked-control-packets* (make-fifo))

(defun send-tcp-packets (tp &optional first-only int-pkt)
  (declare (values number-of-packets-sent))
  (unwind-protect
      (with-lock ((tcp-user-lock tp))
        (unless (member (tcp-user-state tp) '(:closed :listen))
          ;;socket may have aborted while waiting for lock
          (do ((count 0 (1+ count))
               (source (ip::ih-source-address (tcp-user-ip-header tp)))
               (destination (tcp-user-remote-address tp)))
              ((or (not (ok-to-send tp))
                   (and first-only (plusp count)))
               (remove-from-fifo tp *send-blocked-sockets*)
               (when (tcp-user-ack-needed-p tp)
                 (send-ack-for-connection tp))
               count)
            (if (or int-pkt (setq int-pkt (net:allocate-packet nil)))
                ;;If we have an int-pkt to give to IP, build and send a TCP packet
                (multiple-value-bind (buffer-list byte-count)
                    (build-tcp-packet tp)
                  (unwind-protect
                      (multiple-value-bind (result reason)
                          (send *tcp-stream* :send
                                             buffer-list
                                             (tcp-user-ip-header tp)
                                             (prog1 int-pkt (setq int-pkt nil)))
                        (cond ((null result)    ;Returned NIL -- couldn't route.
                               (case reason
                                 (:header       ;Bad IP header
                                  (error "Bad IP header"))
                                 (:route        ;IP routing failed
                                  (push-fifo '(:host-unreachable) (tcp-user-packet-list tp)))
                                 (:arp          ;Routing succeeded, no address translation yet
                                  (start-retransmission-timeout tp (first buffer-list))))
                               (return nil))
                              ((plusp result)
                               ;;the send succeeded
                               (update-tcp-variables tp (first buffer-list) byte-count)
                               (incf (tcp-user-packets-sent tp))
                               (incf (tcp-user-bytes-sent tp) byte-count)
                               (tcp-log :send nil
                                        (copy-tcp-header (first buffer-list) (get-tcp-header))
                                        byte-count
                                        source
                                        destination
                                        (when (and (plusp *tcp-log-level*)
                                                   (second buffer-list))
                                          (string (second buffer-list))))
                               )
                              (t
                               ;;The send failed.  Try again later...
                               (start-failed-send-timeout tp)
                               (return count))))
                    (free-tcp-header (first buffer-list))))
              (progn
                ;;Else, note that this connection needs a packet sent
                (tcp-log :block tp)
                (tcp-stat-incf tp packet-allocation-delays)
                (priority-insert-fifo tp #'tcp-user-precedence *send-blocked-sockets*)
                (return count))))))
    (when int-pkt
      ;;Shouldn't get here, but just in case -- free the unused int-pkt
      (net:free-packet int-pkt))))

(defvar *failed-send-retry-interval* 0.5 "Interval between sends when interface rejected a send")

(defun start-failed-send-timeout (tp)
  (tcp-log :failed-send tp)
  (tcp-stat-incf tp failed-sends)
  (start-timeout *failed-send-retry-interval* 'failed-send-timeout tp))

(defun failed-send-timeout (tp ignore)
  (tcp-log :failed-send-timeout tp)
  (tcp-stat-incf tp failed-send-timeouts)
  (send-tcp-packets tp t))

(defun start-failed-control-packet-send-timeout (header ip-header tp)
  (tcp-log :failed-send tp)
  (tcp-stat-incf tp failed-sends)
  (start-global-timeout *failed-send-retry-interval*
                        'failed-control-packet-send-timeout
                        ;;Note that we don't save tp -- the connection could be aborting
                        (list header ip-header nil)))

(defun failed-control-packet-send-timeout (list ignore)
  (tcp-log :failed-send-timeout nil)
  (tcp-stat-incf nil failed-send-timeouts)
  (push-fifo list *send-blocked-control-packets*))

(defun ok-to-send (tp)
  (let ((SND.NXT (SND.NXT tp))
        (SND.WND (SND.WND tp))
        (state (tcp-user-state tp)))
    (cond ((or (null state) (eq state :closed))
           nil)                                 ;Connection not open
          ((= (ISS tp) SND.NXT)
           t)                                   ;Need to send a SYN
          ((= (tcp-user-send-data-length tp) (tcp-user-send-data-offset tp))
           (tcp-user-fin-needed-p tp))          ;All data send; need to send a FIN?
          ((zerop SND.WND)                      ;Not all data has been sent but SND.WND is shut
           (zero-window-probe tp)               ;Send a zero window probe
           nil)                                 ;...and return nil
          (t                                    ;Not all data has been sent, window not completely shut
           (let* ((useable-window (- SND.WND (32-bit-minus SND.NXT (SND.UNA tp))))
                  (remote-mss (tcp-user-remote-mss tp))
                  (push-seq (tcp-user-push-sequence-number tp))
                  (data-to-push-point (if (and push-seq (32-bit-> push-seq SND.NXT))
                                          (32-bit-minus push-seq SND.NXT)
                                        remote-mss)))
             (and (plusp useable-window)                        ;And the window is open enough ...
                  (or (>= useable-window remote-mss )           ;to send a Maximum Segment,
                      (>= useable-window data-to-push-point)    ;all the data to the next push point,
                      (> (* 4 useable-window) SND.WND))))))))   ;or at least 25% of offered

(defun build-tcp-packet (tp)
  "Builds and returns a TCP packet for TCP connection tp"
  (declare (values buffer-list byte-count))
  (let* ((header (get-tcp-header))
         (buffers (ncons header))
         (data-to-send 0)
         (ISS (ISS tp))
         (SND.UNA (SND.UNA tp))
         (SND.NXT (SND.NXT tp))
         (SND.UP (SND.UP tp)))
    (setf (tcp-source-port header) (tcp-user-local-port tp))
    (setf (tcp-destination-port header) (tcp-user-remote-port tp))
    (setf (tcp-sequence-number header) SND.NXT)
    (setf (tcp-ack-number header) (RCV.NXT tp))
    (setf (tcp-reserved-1 header) 0)
    (setf (tcp-reserved-2 header) 0)
    (setf (tcp-flags header) 0)
    (cond ((= ISS SND.NXT)
           (setf (tcp-data-offset header) 6)
           (setf (fill-pointer header) 24)
           (setf (tcp-syn-bit header) 1)
           (setf (aref header 20) tcp-opt-mss)
           (setf (aref header 21) 4)
           (let ((mss (tcp-user-interface-mss tp)))
             (setf (aref header 22) (ldb (byte 8 8) mss))
             (setf (aref header 23) (ldb (byte 8 0) mss))))
          (t
           (setf (tcp-data-offset header) 5)
           (setf (fill-pointer header) 20)))

    (unless (member (tcp-user-state tp) '(:CLOSED :LISTEN :SYN-SENT))
      (setf (tcp-ack-bit header) 1))

    (setf (tcp-window header) (RCV.WND tp))

    (cond ((and SND.UP (32-bit->= SND.UP SND.NXT))
           (setf (tcp-urg-bit header) 1)
           (setf (tcp-urgent-pointer header) (32-bit-minus SND.UP SND.NXT)))
          (t
           (setf (tcp-urgent-pointer header) 0)))

    ;; Add user data to list of buffers
    (setq data-to-send (min (- (tcp-user-send-data-length tp) (tcp-user-send-data-offset tp))   ;user data
                            (tcp-user-remote-mss tp)    ;remote end's max segment
                            (tcp-user-interface-mss tp)))       ;max data in int-pkt
    (unless (eq (tcp-user-state tp) :SYN-SENT)
      (setq data-to-send (min (- (SND.WND tp)
                                 (32-bit-minus SND.NXT SND.UNA))
                              data-to-send)))
    (when (plusp data-to-send)
      (do ((left data-to-send)
           (current-offset 0)
           (start-offset (- (tcp-user-send-data-offset tp)      ;sent but unACKed
                            (if (= ISS SND.UNA) 1 0)))          ;but don't count SYN
           (list (fifo-as-list (tcp-user-send-data tp)) (cdr list)))
          ((or (null list) (not (plusp left)))
           (unless (zerop left)
             (cerror "continue" "Not enough data in send list")))
        (let ((length (length (first list))))
          (cond ((> start-offset current-offset)
                 ;;At least front of this buffer gets skipped...
                 (let ((skip (- start-offset current-offset)))
                   (unless (>= skip length)
                     (push (make-array (min (- length skip) left)
                                       :element-type '(unsigned-byte 8)
                                       :displaced-to (first list)
                                       :displaced-index-offset skip)
                           buffers)
                     (decf left (length (first buffers))))))
                ((> length left)
                 ;;Tail of this buffer is skipped...
                 (push (make-array left
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to (first list))
                       buffers)
                 (return))
                (t
                 ;;Entire buffer included
                 (push (first list) buffers)
                 (decf left length)))
          (incf current-offset length)))
      (setq buffers (nreverse buffers))

      ;;If this buffer includes PUSHed data, set the PUSH bit
      (when (and (tcp-user-push-sequence-number tp)
                 (32-bit-< (tcp-user-push-sequence-number tp)
                           (32-bit-plus SND.NXT data-to-send)))
        (setf (tcp-psh-bit header) 1)))

    ;;Have we included the last data byte the user's given us?
    (when (= (+ data-to-send (tcp-user-send-data-offset tp))
             (tcp-user-send-data-length tp))
      (when (tcp-user-auto-push tp)
        ;; In auto-push mode, set the PUSH bit
        (setf (tcp-psh-bit header) 1))
      ;; Set the FIN bit too if the user has closed the connection
      (when (tcp-user-fin-needed-p tp)
        (setf (tcp-fin-bit header) 1)))

    ;; Calculate and store the TCP checksum
    (store-tcp-checksum buffers (tcp-user-ip-header tp) (tcp-user-local-host-p tp))

    ;; Return the list of buffers we built
    (values buffers (max data-to-send 0))))

(defun update-tcp-variables (tp header count)
  ;;Cancel the failed-send timeout
  (cancel-timeout tp 'failed-send-timeout)

  ;;If we are sending an ACK, cancel ACK timeout
  (when (tcp-ack-bit-p header)
    (setf (tcp-user-ack-needed-p tp) nil)
    (cancel-timeout tp 'ack-delay-timeout))

  ;;If we are sending SYN, increment SND.NXT
  (when (tcp-syn-bit-p header)
    (setf (SND.NXT tp) (32-bit-plus (SND.NXT tp) 1)))

  ;;If we are sending user data, increment statistics and SND.NXT
  (when (plusp count)
    (tcp-stat-incf tp data-packets-sent)
    (let* ((new-seq (32-bit-plus (SND.NXT tp) count))
           (new-data (if (32-bit-> new-seq (SND.LAST tp))
                         (32-bit-minus new-seq (SND.LAST tp))
                       0)))
      (tcp-stat-incf tp data-bytes-sent new-data))
    (incf (tcp-user-send-data-offset tp) count)
    (setf (SND.NXT tp) (32-bit-plus (SND.NXT tp) count)))

  ;;If we sent a FIN, note that we've done so
  (when (tcp-fin-bit-p header)
    (setf (tcp-user-fin-needed-p tp) nil)
    (unless (tcp-user-local-fin-sequence-number tp)
      (setf (tcp-user-local-fin-sequence-number tp) (SND.NXT tp)))
    (setf (SND.NXT tp) (32-bit-plus (tcp-user-local-fin-sequence-number tp) 1)))

  ;;Remember the highest sequence number we've sent for this connection
  (when (32-bit-< (SND.LAST tp) (SND.NXT tp))
    (setf (SND.LAST tp) (SND.NXT tp)))

  ;; If sending SYN, FIN, or user data, start timeout
  (when (or (plusp count)
            (not (zerop (logand (tcp-flags header) (logior FIN SYN)))))
    (let ((now (zl:time)))
      (without-interrupts
        (push (cons (tcp-sequence-number header) now) (tcp-user-rtt-sequence-time-alist tp)))
      (tcp-stat-incf tp rtt-attempts)
      (start-retransmission-timeout tp header now)))    ;Start timeout...
  )

(defun start-retransmission-timeout (tp header &optional (now (zl:time)))
  (setf (tcp-user-last-time tp) now)
  (let ((timeout (tcp-user-retransmission-timeout tp)))
    (when (tcp-syn-bit-p header)
      ;;Exponential Backoff for retransmission of SYN
      (setq timeout (max timeout (/ (float (tcp-user-cumulative-timeout tp)) 60.0))))
    (start-timeout (min timeout
                        (/ (float (- (tcp-user-send-timeout tp)
                                     (tcp-user-cumulative-timeout tp)))
                           60.0))
                   'retransmission-timeout
                   tp)))


(defun send-rst-packet (buffer ip-header &optional reason)
  "Builds and sends a RST packet in response to a received packet"
  (tcp-stat-incf nil rst-packets-sent)
  (let ((header (get-tcp-header))
        (sender (ip:ih-source-address ip-header)))
    (reverse-route ip-header)
    (setf (tcp-source-port header) (tcp-destination-port buffer))
    (setf (tcp-destination-port header) (tcp-source-port buffer))
    (setf (tcp-reserved-1 header) 0)
    (setf (tcp-reserved-2 header) 0)
    (setf (tcp-flags header) RST)
    (cond ((tcp-ack-bit-p buffer)
           (setf (tcp-sequence-number header) (tcp-ack-number buffer))
           (setf (tcp-ack-number header) 0))
          (t
           (setf (tcp-sequence-number header) 0)
           (setf (tcp-ack-number header) (32-bit-plus (tcp-sequence-number buffer)
                                                      (compute-seg.len buffer)))
           (setf (tcp-ack-bit header) 1)))
    (setf (tcp-data-offset header) 5)
    (setf (tcp-window header) 0)
    (setf (tcp-urgent-pointer header) 0)
    (setf (fill-pointer header) 20)
    (tcp-log :reset-packet nil (copy-tcp-header buffer (get-tcp-header)) reason sender)
    (send-control-packet header ip-header nil)))


(defun send-rst-for-connection (tp &optional reason)
  "Builds and sends a RST packet for a specific TCP connection"
  (tcp-stat-incf tp rst-packets-sent)
  (multiple-value-bind (header ip-header)
      (build-rst-packet tp)
    (tcp-log :reset-sent tp (copy-tcp-header header (get-tcp-header)) reason)
    (send-control-packet header ip-header tp)))

(defun build-rst-packet (tp)
  (declare (values tcp-header ip-header))
  (with-lock ((tcp-user-lock tp))
    (let* ((header (get-tcp-header))
           (ip-header (tcp-user-ip-header tp)))
      (setf (tcp-source-port header) (tcp-user-local-port tp))
      (setf (tcp-destination-port header) (tcp-user-remote-port tp))
      (setf (tcp-sequence-number header) (SND.LAST tp))
      (setf (tcp-ack-number header) 0)
      (setf (tcp-reserved-1 header) 0)
      (setf (tcp-reserved-2 header) 0)
      (setf (tcp-flags header) RST)
      (setf (tcp-data-offset header) 5)
      (setf (tcp-window header) 0)
      (setf (tcp-urgent-pointer header) 0)
      (setf (fill-pointer header) 20)
      (values header ip-header))))

(defun build-ack-packet (tp)
  (declare (values tcp-header ip-header))
  (with-lock ((tcp-user-lock tp))
    (let* ((header (get-tcp-header))
           (ip-header (tcp-user-ip-header tp)))
      (setf (tcp-source-port header) (tcp-user-local-port tp))
      (setf (tcp-destination-port header) (tcp-user-remote-port tp))
      (setf (tcp-sequence-number header) (SND.LAST tp))
      (setf (tcp-ack-number header) (RCV.NXT tp))
      (setf (tcp-reserved-1 header) 0)
      (setf (tcp-reserved-2 header) 0)
      (setf (tcp-flags header) ACK)
      (setf (tcp-urgent-pointer header) 0)
      (when (and (SND.UP tp) (32-bit-> (SND.UP tp) (tcp-sequence-number header)))
        (setf (tcp-urg-bit header) 1)
        (setf (tcp-urgent-pointer header) (32-bit-minus (SND.UP tp) (tcp-sequence-number header))))
      (setf (tcp-window header) (RCV.WND tp))
      (setf (tcp-data-offset header) 5)
      (setf (fill-pointer header) 20)
      (setf (tcp-user-ack-needed-p tp) nil)
      (values header ip-header))))

(defun send-control-packet (header ip-header tp &optional int-pkt)
  (let* ((source (ip::ih-source-address ip-header))
         (destination (if tp
                          (tcp-user-remote-address tp)
                        (ip::ih-dest-address ip-header))))
    (unless int-pkt
      (store-tcp-checksum header ip-header (and tp (tcp-user-local-host-p tp)))
      (setq int-pkt (net:allocate-packet nil)))
    (cond (int-pkt
           (let ((result (send *tcp-stream* :send header ip-header int-pkt)))
             (cond ((null result))              ;TCP is down.
                   ((plusp result)
                    ;;Send succeeded.  Increment statistics and do logging.
                    (when tp
                      (incf (tcp-user-packets-sent tp))
                      (incf (tcp-user-bytes-sent tp) (- (length header) 20)))
                    (tcp-log :send
                             nil
                             (copy-tcp-header header (get-tcp-header))
                             (- (fill-pointer header) 20)
                             source
                             destination)
                    (free-tcp-header header))
                   (tp
                    ;;Send failed when sending a RST or ACK packet for a socket.
                    (cond ((tcp-rst-bit-p header)
                           (start-failed-control-packet-send-timeout header ip-header tp))
                          (t
                           ;;This was an ACK packet.  Note that we need an ACK and start failed-send-timeout
                           (setf (tcp-user-ack-needed-p tp) t)
                           (start-failed-send-timeout tp)
                           (free-tcp-header header))))
                   (t
                    ;;Send failed when sending a RST in response to bogus packet.  Ignore problem.
                    (start-failed-control-packet-send-timeout header ip-header nil))))
           )
          (t
           (push-fifo (list header ip-header tp) *send-blocked-control-packets*)
           (tcp-stat-incf tp packet-allocation-delays)))))

(defun send-ack-for-connection (tp)
  "Indicates that a delayed ACK should be queued for this connection"
  (tcp-stat-incf tp ack-packets-requested)
  (cond ((tcp-user-closed-p tp)
         ;;User has closed -- no data to piggyback ACK onto
         (send-immediate-ack tp))
        ((zerop (tcp-user-ack-delay tp))
         ;;We've shut off delayed ACKs for this connection
         (send-immediate-ack tp))
        ((assoc 'ack-delay-timeout (tcp-user-timeout-alist tp) :test #'eq)
         ;;Already delaying an ACK -- do nothing
         (tcp-log :skip tp))
;;; The following seems like a good idea but works poorly when both ends keep many more sends out
;;; than receives.  Perhaps should arm a longer delayed-ack timeout?
;;;     ((> (tcp-user-send-data-length tp) (tcp-user-send-data-offset tp))
;;;      ;;User has unsent data -- send ACK with new data
;;;      (tcp-log :wait tp))
        (t
         ;;start delayed ACK timeout
         (tcp-stat-incf tp ack-packets-delayed)
         (tcp-log :delay tp)
         (start-timeout (tcp-user-ack-delay tp) 'ack-delay-timeout tp))))

(defun ack-delay-timeout (tp &optional ignore)
  "Handles a delayed ACK timeout"
  (unless (member tp (fifo-as-list *send-blocked-sockets*) :test #'eq)
    (tcp-stat-incf tp acks-sent-due-to-timer)
    (let ((delayed-acks (ts-acks-sent-due-to-timer (tcp-user-stats tp)))
          (good-packets (ts-packets-while-ack-delayed (tcp-user-stats tp))))
      ;;If big enough sample, but not enough delayed ACKs were useful
      (when (and (>= delayed-acks *delayed-ack-sample*)
                 (< good-packets (* delayed-acks *delayed-ack-proportion*)))
        (setf (tcp-user-ack-delay tp) 0)))      ;then shut off delaying ACKs for this connection
    (send-immediate-ack tp)))

(defun send-immediate-ack (tp)
  "Builds and sends a ACK packet for a specific TCP connection"
  (with-lock ((tcp-user-lock tp))
    (unless (eq (tcp-user-state tp) :closed)
      (tcp-stat-incf tp ack-packets-sent)
      (tcp-log :send-ack tp (RCV.NXT tp))
      (setf (tcp-user-ack-needed-p tp) nil)
      (cancel-timeout tp 'ack-delay-timeout)
      (multiple-value-bind (header ip-header)
          (build-ack-packet tp)
        (send-control-packet header ip-header tp)))))

(defun send-urg-packet (tp)
  (tcp-stat-incf tp urg-packets-sent)
  (multiple-value-bind (header ip-header)
      (build-ack-packet tp)
    (send-control-packet header ip-header tp)))

(defun zero-window-probe (tp &optional ignore &aux byte data-seq)
  (with-lock ((tcp-user-lock tp))
    (unless (assoc 'zero-window-probe (tcp-user-timeout-alist tp) :test #'eq)
      (setq data-seq (32-bit-plus (SND.NXT tp) 1))
      (setq byte (find-byte-at-SND.NXT tp))
      (when (and (> (tcp-user-send-data-length tp) (tcp-user-send-data-offset tp))
                 (zerop (SND.WND tp))
                 byte)
        (tcp-stat-incf tp window-probes-sent)
        (multiple-value-bind (header ip-header)
            (build-ack-packet tp)
          ;;(build-ack-packet) uses SND.LAST.  This routine increments SND.LAST.  If it is called
          ;;more than once, we will be sending TWO beyond SND.NXT.
          (setf (tcp-sequence-number header) (SND.NXT tp))
          (setf (aref header 20) byte)
          (setf (fill-pointer header) 21)
          (when (32-bit-< (SND.LAST tp) data-seq)
            (tcp-stat-incf tp data-bytes-sent 1)
            (setf (SND.LAST tp) data-seq))
          (send-control-packet header ip-header tp))
        (start-timeout *window-probe-interval* 'zero-window-probe tp)))))

(defun find-byte-at-SND.NXT (tp)
  "Return the first byte of user data at SND.NXT"
  (do* ((current-offset 0)
        (start-offset (- (tcp-user-send-data-offset tp) ;sent but unACKed
                         (if (= (ISS tp) (SND.UNA tp)) 1 0)))   ;but don't count SYN
        (list (fifo-as-list (tcp-user-send-data tp)) (cdr list))
        (buffer (first list) (first list)))
       ((null list) nil)
    (let ((length (length buffer)))
      (cond ((> start-offset current-offset)
             ;;Front of this buffer gets skipped...
             (let ((skip (- start-offset current-offset)))
               (unless (>= skip length)
                 (return (aref buffer skip)))))
            ((> length 1)
             ;;First byte of this buffer is included
             (return (aref buffer 0))))
      (incf current-offset length))))

;;;Receiving TCP packets....

(defmacro tcp-data-bytes (packet)
  `(- (length ,packet) (tcp-first-data-byte ,packet)))

(defun compute-seg.len (packet)
  "Calculates SEG.LEN (sequence number space consumed by packet)"
  (+ (tcp-data-bytes packet)
     (tcp-syn-bit packet)
     (tcp-fin-bit packet)))

(defmacro SEG.ACK (packet)
  `(tcp-ack-number ,packet))

(defmacro SEG.LEN (packet)
  `(compute-seg.len ,packet))

(defmacro SEG.PRC (ip-header)
  `(ip::ih-precedence ,ip-header))

(defmacro SEG.SEQ (packet)
  `(tcp-sequence-number ,packet))

(defmacro SEG.UP (packet)
  `(tcp-urgent-pointer ,packet))

(defmacro SEG.WND (packet)
  `(tcp-window ,packet))

(defun get-remote-mss (socket packet)
  "If the packet is a SYN packet with Maximum Segment Size option, update socket's instance variable"
  (when (tcp-syn-bit-p packet)
    (do* ((offset 20)
          (data-offset (tcp-first-data-byte packet)))
         ((>= offset data-offset))
      (case (aref packet offset)
        ((#.tcp-opt-end #.tcp-opt-nop)
         (incf offset))
        (#.tcp-opt-mss
         (setf (tcp-user-remote-mss socket)
               (+ (ash (aref packet (+ offset 2)) 8)
                  (aref packet (+ offset 3))))
         (incf offset 4))
        (otherwise
         (incf offset (aref packet (1+ offset))))))))

(defun process-ACK (socket packet)
  "Processes the ACK field in a packet -- updates SND.UNA, removes completely ACKed buffers, etc."
  (when (tcp-ack-bit-p packet)
    (let ((SEG.ACK (SEG.ACK packet))
          (SEG.WND (SEG.WND packet))
          (SEG.SEQ (SEG.SEQ packet)))
      (unless (32-bit->= SEG.ACK (SND.UNA socket))
        (when (zerop (tcp-data-bytes packet))
          (tcp-stat-incf socket duplicate-ack-packets-received))
        (return-from process-ACK))

      (when (32-bit-> SEG.ACK (SND.LAST socket))
        ;;ACKing unsent data -- send an ACK packet and return
        (tcp-stat-incf socket unsent-data-acks)
        (setf (tcp-user-ack-needed-p socket) t)
        (return-from process-ACK))

      (when (zerop (tcp-data-bytes packet))
        (cond ((or (32-bit-> SEG.ACK (SND.UNA socket))
                   (tcp-fin-bit-p packet))
               ;;If ACKing new data, or merely transmitting a FIN, count as real ACK packet
               (tcp-stat-incf socket ack-packets-received))
              ((/= (32-bit-plus SEG.ACK SEG.WND)
                   (32-bit-plus (SND.UNA socket) (SND.WND socket)))
               ;;Else, if changing window, count as window update
               (tcp-stat-incf socket window-updates-received))
              ((tcp-urg-bit-p packet)
               ;;Else, if URG bit set, count as Urgent data indication
               (tcp-stat-incf socket urg-packets-received))
              (t
               ;;Else, why was it sent??
               (tcp-stat-incf socket useless-ack-packets-received))))

      ;;No.  SYN acknowledged?
      (when (= (SND.UNA socket) (ISS socket))
        ;;SYN acknowledged -- remember it
        (setf (SND.UNA socket) (32-bit-plus (SND.UNA socket) 1))
        (when (= (SND.NXT socket) (ISS socket))
          ;;SYN was to be retransmitted but hasn't been yet...
          (setf (SND.NXT socket) (SND.UNA socket))))

      ;;User data acknowledged?
      (when (32-bit-> SEG.ACK (SND.NXT socket))
        (let* ((bytes-acked (32-bit-minus SEG.ACK (SND.NXT socket)))
               (local-fin (tcp-user-local-fin-sequence-number socket))
               (fin-acked (and local-fin (32-bit-> SEG.ACK local-fin))))
          ;;This can happen if not all of our once-sent data has been retransmitted
          (incf (tcp-user-send-data-offset socket) (- bytes-acked (if fin-acked 1 0)))
          (setf (SND.NXT socket) SEG.ACK)))

      (let ((buffer-seq (32-bit-minus (SND.NXT socket) (tcp-user-send-data-offset socket))))
        (dolist (buffer (fifo-as-list (tcp-user-send-data socket)))
          (let ((length (length buffer)))
            (if (32-bit-> SEG.ACK buffer-seq)
                (let ((acked-bytes (32-bit-minus SEG.ACK buffer-seq)))
                  (When (>= acked-bytes length)
                    ;;This entire buffer is acknowledged
                    (tcp-log :write-reply socket length)
                    (push-fifo `(:write-reply ,(pop-fifo (tcp-user-send-data socket)))
                               (tcp-user-packet-list socket))
                    (decf (tcp-user-send-data-length socket) length)
                    (decf (tcp-user-send-data-offset socket) length)))
              (return))
            (setq buffer-seq (32-bit-plus buffer-seq length)))))

      ;;Update send window
      (when (or (32-bit-< (SND.WL1 socket) SEG.SEQ)
                (and (= (SND.WL1 socket) SEG.SEQ)
                     (32-bit-<= (SND.WL2 socket) SEG.ACK)))
        ;;Check for remote end shrinking window on us.
        ;;TCP spec strongly advises against it, but 4.2BSD does it...
        (when (32-bit-< (32-bit-plus SEG.ACK SEG.WND)
                        (32-bit-plus (SND.UNA socket) (SND.WND socket)))
          (tcp-stat-incf socket shrinking-window-updates))
        (setf (SND.WND socket) SEG.WND)
        (setf (SND.WL1 socket) SEG.SEQ)
        (setf (SND.WL2 socket) SEG.ACK))

      ;;Note that some sequence space has been acknowledged
      (let ((local-fin (tcp-user-local-fin-sequence-number socket)))
        (tcp-stat-incf socket
                       ack-bytes-received
                       (- (32-bit-minus SEG.ACK (SND.UNA socket))
                          ;;sent a FIN, it wasn't acknowledged, but now it is, don't count as data byte
                          (if (and local-fin (32-bit-> SEG.ACK local-fin)) 1 0))))
      (setf (SND.UNA socket) SEG.ACK)

      ;;If our PUSHed buffer is acknowledged, forget we were PUSHing it
      (when (and (tcp-user-push-sequence-number socket)
                 (32-bit-> SEG.ACK (tcp-user-push-sequence-number socket)))
        (setf (tcp-user-push-sequence-number socket) nil))

      ;;If our Urgent data is acknowledged, leave Urgent mode for sending
      (when (and (SND.UP socket) (32-bit-> SEG.ACK (SND.UP socket)))
        (setf (SND.UP socket) nil))

      ;;Deal with timeouts, etc.  If timing round-trip time, have our bytes been acknowledged?
      (when (tcp-user-rtt-sequence-time-alist socket)
        (let ((now (zl:time))
              (passed nil))
          (without-interrupts
            (when (setq passed (nreverse (mapcan #'(lambda (x) (if (32-bit-<= (car x) SEG.ACK) (ncons x)))
                                                 (tcp-user-rtt-sequence-time-alist socket))))
              (setf (tcp-user-rtt-sequence-time-alist socket)
                    (nset-difference (tcp-user-rtt-sequence-time-alist socket) passed))))
          (when passed
            ;;(tcp-log :initial-rtt socket (tcp-user-smoothed-round-trip-time socket))
            (tcp-stat-incf socket rtt-updates)
            (dolist (seq-num passed)
              ;;calculate round-trip-time of acknowledged packet
              (let ((rtt (/ (float (time-difference now (cdr seq-num))) 60.0)))
                (setf (tcp-user-smoothed-round-trip-time socket)
                      (cond ((not (tcp-user-smoothed-round-trip-time socket))
                             ;;If no previous measurement, store this one
                             rtt)
                            (t
                             (if (= (car seq-num) (ISS socket))
                                 ;;If previous measurement for SYN, and this is SYN, average it in...
                                 (/ (+ (tcp-user-smoothed-round-trip-time socket) rtt) 2.0)
                               ;;If not measuring SYN, apply smoothing formula
                               (+ (* *rto-alpha* (tcp-user-smoothed-round-trip-time socket))
                                  (* (- 1.0 *rto-alpha*) rtt))))))
                ;;(tcp-log :rtt socket rtt (tcp-user-smoothed-round-trip-time socket))
                ))
            (setf (tcp-user-retransmission-timeout socket)
                  (min *rto-ubound*
                       (max *rto-lbound*
                            (* *rto-beta* (tcp-user-smoothed-round-trip-time socket))))))))

      (setf (tcp-user-cumulative-timeout socket) 0)     ;Always reset cumulative time when some data is ACKed
      (cond ((= (SND.UNA socket) (SND.NXT socket))
             ;;All data has been acknowledged -- cancel the retransmission timeout
             (cancel-timeout socket 'retransmission-timeout)
             (setf (tcp-user-last-time socket) nil))
            (t
             ;;Some data has been acknowledged -- restart the retransmission timeout
             (start-timeout (tcp-user-retransmission-timeout socket) 'retransmission-timeout socket)))
      )))

(defun process-URG (socket packet)
  (when (tcp-urg-bit-p packet)
    (let ((urg-seq (32-bit-plus (SEG.SEQ packet) (SEG.UP packet))))
      (cond ((null (RCV.UP socket))
             (push-fifo '(:urgent) (tcp-user-packet-list socket))
             (setf (RCV.UP socket) urg-seq))
            ((32-bit-> urg-seq (RCV.UP socket))
             (setf (RCV.UP socket) urg-seq))))))

(defun process-data (socket packet)
  "Copies data into appropriate spots of receive buffers"
  (let* ((packet-offset (tcp-first-data-byte packet))   ;offset in packet of first new data byte
         (packet-seq (32-bit-plus (SEG.SEQ packet) (tcp-syn-bit packet)))       ;seq num of 1st new data byte
         (packet-bytes (- (length packet) packet-offset))       ;number of new data bytes in packet
         (packet-edge (32-bit-plus packet-seq packet-bytes))    ;1+ last seq in packet
         (push-offset (if (or (tcp-psh-bit-p packet) (tcp-fin-bit-p packet)) packet-bytes))
         (old-RCV.NXT (RCV.NXT socket))         ;Remember current version of RCV.NXT
         new-buffer-seq)

    (when (32-bit-> (RCV.NXT socket) packet-seq)
      (let ((dup (32-bit-minus (RCV.NXT socket) packet-seq)))   ;bytes of duplicate data
        (incf packet-offset dup)
        (setq packet-seq (32-bit-plus packet-seq dup))
        (decf packet-bytes dup)
        (cond ((null push-offset))
              ((>= dup push-offset)
               (setq push-offset nil))
              (t
               (decf push-offset dup)))))

    (when (32-bit-> packet-edge (RCV.LAST socket))
      (setf (RCV.LAST socket) packet-edge))

    ;;If the FIN bit is set, remember its sequence number.
    (when (tcp-fin-bit-p packet)
      (setf (tcp-user-remote-fin-sequence-number socket) packet-edge)
      (setf (RCV.LAST socket) (32-bit-plus packet-edge 1)))


    (when (plusp packet-bytes)
      ;;Copy as much data as possible to user receive buffers
      (multiple-value-setq (packet-bytes packet-offset packet-seq push-offset new-buffer-seq)
        (copy-packet-data-to-receive-data socket
                                          (32-bit-minus (RCV.NXT socket) (tcp-user-receive-data-offset socket))
                                          packet
                                          packet-bytes
                                          packet-offset
                                          packet-seq
                                          push-offset
                                          :data))

      ;;Reply to filled receive buffers
      (reply-to-completed-receives socket)

      (when (plusp packet-bytes)
        (if (and (tcp-user-closed-p socket)
                 (tcp-user-discard-p socket)
                 (fifo-empty-p (tcp-user-receive-data socket)))
            ;;User has closed, said we should discard excess data, and no buffers remain
            (setf (RCV.NXT socket) packet-edge)
          ;;Save as much as possible in existing saved-packet list
          (progn
            (multiple-value-bind (new-packet-bytes new-packet-offset)
                (copy-packet-data-to-saved-data socket
                                                new-buffer-seq
                                                packet
                                                packet-bytes
                                                packet-offset
                                                packet-seq
                                                push-offset)
              (when (plusp new-packet-bytes)
                ;;Some data still remains uncopied.  Drop it.
                (tcp-log :drop socket new-packet-bytes new-packet-offset)
                (tcp-stat-incf socket dropped-bytes new-packet-bytes)))))))

    ;;If all data up to FIN has been processed, signal user
    (check-if-fin-processed socket)

    ;;Finally, send an ACK if any in-sequence data has arrived
    (unless (= old-RCV.NXT (RCV.NXT socket))
      (when (assoc 'ack-delay-timeout (tcp-user-timeout-alist socket) :test #'eq)
        (tcp-stat-incf socket packets-while-ack-delayed))
      (setf (tcp-user-ack-needed-p socket) t))))

(defun copy-packet-data-to-receive-data (socket buffer-seq packet packet-bytes packet-offset packet-seq
                                         push-offset keyword)
  "Copy data from a received packet into user receive buffers.  Return amount of uncopied data"
  (declare (values bytes-not-copied new-offset new-sequence new-push-offset new-buffer-seq))
  (do ((list (fifo-as-list (tcp-user-receive-data socket)) (cdr list))
       (packet-edge (32-bit-plus packet-seq packet-bytes)))     ;sequence number of end of packet
      ((null list))                             ;Quit when we've run out of buffers
    (let* ((elt (car list))
           (length (array-length (rcv-buffer elt)))
           (buffer-edge (32-bit-plus buffer-seq length))
           (buffer-offset 0)
           (copy-bytes 0))
      ;;Sequence numbers of buffer: [buffer-seq,buffer-edge)
      (cond ((32-bit-< buffer-seq packet-seq)
             ;;Packet starts after left edge of buffer
             (unless (32-bit-<= buffer-edge packet-seq)
               ;;Packet's left edge is in this buffer
               (setq buffer-offset (32-bit-minus packet-seq buffer-seq))
               (setq copy-bytes (min (- length buffer-offset) packet-bytes))))
            ((32-bit->= packet-edge buffer-seq)
             ;;Packet's right edge is in this buffer
             (setq buffer-offset 0)
             (setq copy-bytes (min length packet-bytes))))
      (when (plusp copy-bytes)
        (copy-tcp-data packet packet-offset elt buffer-offset copy-bytes socket keyword)
        (incf packet-offset copy-bytes)
        (setq packet-seq (32-bit-plus packet-seq copy-bytes))
        (decf packet-bytes copy-bytes)
        (when push-offset
          (decf push-offset copy-bytes)))
      (cond ((plusp packet-bytes)               ;Entire packet doesn't fit in this buffer
             (when push-offset
               (setf (rcv-push-offset elt) (array-length (rcv-buffer elt)))))
            ((zerop packet-bytes)
             (unless (32-bit-<= buffer-edge packet-seq)
               ;;Unless this buffer precedes the packet....
               (when (32-bit-> packet-seq buffer-seq)
                 ;;If part of packet went into this buffer...
                 (adjust-push-offset elt (and push-offset (+ buffer-offset push-offset copy-bytes))))
               (reset-holes socket)
               (return)))
            (t
             (error "packet-bytes < 0")))
      (setq buffer-seq buffer-edge)))
  (values packet-bytes packet-offset packet-seq push-offset buffer-seq))

(defun adjust-push-offset (elt push-offset)
  (let* ((length (array-length (rcv-buffer elt)))
         (last-hole (and (rcv-holes elt) (car (last (rcv-holes elt)))))
         (hole-offset (if (and last-hole (= (hole-end last-hole) (1- length)))
                          (hole-start last-hole)
                        length))
         (rcv-offset (or (rcv-push-offset elt) 0)))
    (cond (push-offset                          ;This packet was pushed
           (cond ((> push-offset length)
                  ;;Push offset beyond end of this buffer -- set to end
                  (setf (rcv-push-offset elt) length))
                 ((<= hole-offset push-offset)
                  ;;A hole at or before push offset of this packet --  max of current and previous offsets
                  (setf (rcv-push-offset elt) (max push-offset rcv-offset)))
                 (t                             ;(> hole-offset push-offset)
                  ;;This buffer has data following this push offset -- push offset at end of that data
                  (setf (rcv-push-offset elt) (max hole-offset rcv-offset)))))
          ((rcv-push-offset elt)                ;This packet was not pushed, but a previous one was
           (setf (rcv-push-offset elt) (max rcv-offset hole-offset))))))

(defun reset-holes (socket)
  (do* ((list (fifo-as-list (tcp-user-receive-data socket)) (cdr list))
        (elt (car list) (car list)))
       ((null elt))
    (cond ((null (rcv-holes elt)))              ;If buffer is full, skip it
          ((null (rcv-push-offset elt))         ;If not-full buffer not pushed, break out of loop
           (return))
          ((> (rcv-push-offset elt) (hole-start (first (rcv-holes elt))))
           (return))                            ;Buffer pushed, but hole before push offset -- break
          ((= (rcv-push-offset elt) (hole-start (first (rcv-holes elt))))
           (setf (rcv-holes elt) nil)           ;This buffer and all prior ones full & pushed -- trim holes
           (dolist (e (cdr list))
             (setf (rcv-holes e) (list (cons 0 (1- (array-length (rcv-buffer e)))))))
           (dolist (e (fifo-as-list (tcp-user-saved-packets socket)))
             (setf (rcv-holes e) (list (cons 0 (1- (array-length (rcv-buffer e)))))))
           (return))
          (t                                    ;push offset before hole -- should have been fixed above
           (error "Buffer has push offset < start of first hole")))))

(defun copy-packet-data-to-saved-data (socket fifo-seq packet packet-bytes packet-offset packet-seq push-offset)
  "Copy data from a received packet into saved data buffer.  Return amount of uncopied data"
  (declare (values bytes-not-copied new-offset))
  (let* ((list (fifo-as-list (tcp-user-saved-packets socket)))
         (packet-edge (32-bit-plus packet-seq packet-bytes))    ;sequence number of end of packet
         (elt (first list))
         (length (array-length (rcv-buffer elt)))
         (buffer-edge (32-bit-plus fifo-seq length))
         (buffer-offset 0)
         (copy-bytes 0))
    ;;Sequence numbers of buffer: [fifo-seq,buffer-edge)
    (cond ((32-bit-< fifo-seq packet-seq)
           ;;Packet starts after left edge of buffer
           (unless (32-bit-<= buffer-edge packet-seq)
             ;;Packet's left edge is in this buffer
             (setq buffer-offset (32-bit-minus packet-seq fifo-seq))
             (setq copy-bytes (min (- length buffer-offset) packet-bytes))))
          ((not (32-bit->= fifo-seq packet-edge))
           ;;Packet's right edge is in this buffer
           (setq copy-bytes (min length packet-bytes))))
    (when (plusp copy-bytes)
      (copy-tcp-data packet packet-offset elt buffer-offset copy-bytes socket :save))
    (adjust-push-offset elt push-offset)
    (values (- packet-bytes copy-bytes) (+ packet-offset copy-bytes))))

(defun copy-tcp-data (packet packet-offset rcv-data buffer-offset copy-bytes socket keyword)
  "Copy data from a TCP packet into a user buffer.  Adjust hole list pertaining to this buffer."
  ;;Algorithm copied from IP's fragment reassembly -- (ip:copy-fragment-data)
  (let* ((start buffer-offset)
         (length copy-bytes)
         (end (1- (+ start length))))
    (do* ((result nil)
          (holes (rcv-holes rcv-data) (cdr holes))
          (hole (car holes) (car holes))
          (buffer (rcv-buffer rcv-data)))
         ((null holes) (setf (rcv-holes rcv-data) (nreverse result)))
      (if (and (<= (hole-start hole) end)
               (>= (hole-end hole) start))
          ;;fragment overlaps this hole
          (let* ((first (max (hole-start hole) start))
                 (count (- (1+ (min (hole-end hole) end)) first))
                 (source-first (+ packet-offset (- first start))))
            (ecase keyword
              (:data
               (tcp-log :copy socket count source-first first)
               (tcp-stat-incf socket data-bytes-received count))
              (:save
               (tcp-log :save socket count source-first first)
               (tcp-stat-incf socket saved-bytes count))
              (:retrieve
               (tcp-log :retrieve socket count source-first first)
               (tcp-stat-incf socket retrieved-bytes count)
               (tcp-stat-incf socket data-bytes-received count)))
            (copy-array-portion packet source-first (+ source-first count)
                                buffer first (+ first count))
            (if (<= start (hole-start hole))
                ;;fragment overlaps front of hole -- truncate front of hole
                (when (< end (hole-end hole))   ;unless overlaps end
                  (setf (hole-start hole) (1+ end))     ;just truncate front
                  (push hole result))
              ;;else, doesn't overlap front
              (let (new-hole)
                (when (< end (hole-end hole))
                  (setq new-hole (cons (1+ end) (hole-end hole))))
                (setf (hole-end hole) (1- start))       ;trim first hole
                (push hole result)              ;save it
                (and new-hole (push new-hole result)))))
        (push hole result)))))

(defun reply-to-completed-receives (socket)
  (do ((list (fifo-as-list (tcp-user-receive-data socket)) (cdr list))
       (offset (tcp-user-receive-data-offset socket) 0)
       (new-data nil))
      ((or (null list) (and new-data (zerop new-data))))
    (let* ((elt (car list))
           (buffer (rcv-buffer elt))
           (length (if (null (rcv-holes elt))
                       (or (rcv-push-offset elt) (array-length buffer))
                     (hole-start (first (rcv-holes elt)))))
           (urgent-offset nil))
      (unless (<= offset length)
        (error "receive-data-offset not < length of first buffer"))
      (setq new-data (- length offset))
      (when (plusp new-data)
        (incf (tcp-user-receive-data-offset socket) new-data)
        (setf (RCV.NXT socket) (32-bit-plus (RCV.NXT socket) new-data))
        (decf (RCV.WND socket) new-data))
      (if (null (rcv-holes elt))
          (unless (or (zerop length)            ;...If FIN occurred at beginning of this buffer
                      (member (tcp-user-state socket) '(:listen :syn-sent :syn-received)))
            ;;We can have data queued up waiting for user connection to become established...
            (when (and (RCV.UP socket) (32-bit-> (RCV.NXT socket) (RCV.UP socket)))
              (setq urgent-offset (- length (32-bit-minus (RCV.NXT socket) (RCV.UP socket))))
              (setf (RCV.UP socket) nil))
            (setf (fill-pointer buffer) length)
            (pop-fifo (tcp-user-receive-data socket))
            (let ((push-indicator (cond ((eql (RCV.NXT socket) (tcp-user-remote-fin-sequence-number socket))
                                         :EOF)
                                        ((rcv-push-offset elt)
                                         :PUSH))))
              (push-fifo `(:data ,buffer ,push-indicator ,urgent-offset) (tcp-user-packet-list socket))
              (tcp-log :read-reply socket length push-indicator urgent-offset))
            (decf (tcp-user-receive-data-length socket) (array-length buffer))
            (setf (tcp-user-receive-data-offset socket) 0))
        (return)))))

(defun check-if-fin-processed (socket)
  (when (eql (tcp-user-remote-fin-sequence-number socket) (RCV.NXT socket))
    (let ((list (tcp-user-receive-data socket)))
      (setf (tcp-user-receive-data socket) nil)
      (setf (tcp-user-receive-data-length socket) 0)
      (setf (tcp-user-receive-data-offset socket) 0)
      (push-fifo `(:closing ,(mapcar #'rcv-buffer (fifo-as-list list)))
                 (tcp-user-packet-list socket)))
    (setf (RCV.NXT socket) (32-bit-plus (RCV.NXT socket) 1))
    (case (tcp-user-state socket)
      (:established
       (change-state socket :CLOSE-WAIT))
      (:fin-wait-1
       (change-state socket :CLOSING))
      (:fin-wait-2
       (enter-time-wait socket)))))

(defun saved-but-unacknowledged-data (socket)
  "Returns number of bytes of data saved past RCV.NXT"
  (do* ((list (append (fifo-as-list (tcp-user-receive-data socket))
                      (fifo-as-list (tcp-user-saved-packets socket)))
              (cdr list))
        (total-length 0)
        (receive-offset (tcp-user-receive-data-offset socket))
        (offset receive-offset))
      ((null list)
       (- offset receive-offset))
    (let* ((elt (first list))
           (length (array-length (rcv-buffer elt)))
           (holes (rcv-holes elt))
           (last-hole (and holes (car (last holes))))
           (hole-offset (if (and last-hole (= (hole-end last-hole) (1- length)))
                            (hole-start last-hole)
                          length)))
      (when (plusp hole-offset)
        (setq offset (+ total-length hole-offset)))
      (incf total-length length))))

;;;   Saved Packet processing.  To simplify (!) the following algorithm, we
;;;   save exactly ONE buffer on the saved-packet-list

(defun copy-saved-data-to-user (socket elt)
  ;;Save the read on the receive-data-list
  (incf (tcp-user-receive-data-length socket) (array-length (rcv-buffer elt)))
  (push-fifo elt (tcp-user-receive-data socket))
  ;;Copy data from the Saved Packet buffer
  (let* ((receive-buffer-seq (32-bit-minus (RCV.NXT socket) (tcp-user-receive-data-offset socket)))
         (saved-buffer-seq (32-bit-minus (32-bit-plus receive-buffer-seq
                                                      (tcp-user-receive-data-length socket))
                                         (array-length (rcv-buffer elt))))
         (p (first (fifo-as-list (tcp-user-saved-packets socket))))
         (not-holes (make-not-hole-list (rcv-holes p) (array-length (rcv-buffer p)))))
    (dolist (nh not-holes)
      (let* ((start-offset (hole-start nh))
             (length (1+ (- (hole-end nh) start-offset))))
        (unless (zerop (copy-packet-data-to-receive-data socket
                                                         receive-buffer-seq
                                                         (rcv-buffer p)
                                                         length
                                                         start-offset
                                                         (32-bit-plus saved-buffer-seq start-offset)
                                                         (and (rcv-push-offset p)
                                                              (- (rcv-push-offset p) start-offset))
                                                         :retrieve))
          (return)))))                          ;If entire buffer not copied, stop copying now

  ;;Drop any data past total receive length
  (reset-saved-packet-data (tcp-user-saved-packets socket) (array-length (rcv-buffer elt)))

  ;;Give back any reads now completed
  (reply-to-completed-receives socket)

  ;;If all data up to FIN has been processed, signal user
  (check-if-fin-processed socket)

  ;;Update receive window
  (let* ((saved-elt (first (fifo-as-list (tcp-user-saved-packets socket))))
         (saved-data (if (rcv-holes saved-elt)
                         (hole-start (first (rcv-holes saved-elt)))
                       *tcp-buffer-data-size*)))
    (setf (RCV.WND socket) (- (+ (tcp-user-receive-data-length socket)
                                 (if (tcp-user-optimistic-window-p socket)
                                     (or (tcp-user-interface-mss socket)
                                         0)
                                   0))
                              (tcp-user-receive-data-offset socket)
                              saved-data)))

  (unless (member (tcp-user-state socket) '(:listen :syn-sent :syn-received))
    (setf (tcp-user-ack-needed-p socket) t))
  )

(defun make-not-hole-list (hole-list total-length)
  (do* ((start 0)
        (list hole-list (cdr list))
        (hole (car list) (car list))
        (result nil))
       ((null list)
        (if (< start total-length)
            (push (cons start (1- total-length)) result))
        (nreverse result))
    (if (< start (hole-start hole))
        (push (cons start (1- (hole-start hole))) result))
    (setq start (1+ (hole-end hole)))))

(defun dropped-holes (hole-list trim-offset)
  (do* ((list hole-list (cdr list))
        (hole (car list) (car list))
        (result nil))
       ((null list) (nreverse result))
    (let ((start (hole-start hole))
          (end (hole-end hole)))
      (if (> trim-offset start)
          (if (>= trim-offset end)
              (push (cons start end) result)
            (push (cons start (min (1- trim-offset) end)) result))))))

(defun trim-hole-list (hole-list trim-offset)
  (do* ((list hole-list (cdr list))
        (hole (car list) (car list))
        (result nil))
       ((null list) (nreverse result))
    (let ((start (hole-start hole))
          (end (hole-end hole)))
      (if (< trim-offset end)
          (if (< trim-offset start)
              (push (cons start end) result)
            (push (cons trim-offset end) result))))))

(defun displace-hole-list (hole-list trim-offset)
  (do* ((list hole-list (cdr list))
        (hole (car list) (car list))
        (result nil))
       ((null list) (nreverse result))
    (let ((start (hole-start hole))
          (end (hole-end hole)))
      (if (< trim-offset end)
          (if (< trim-offset start)
              (push (cons (- start trim-offset) (- end trim-offset)) result)
            (push (cons 0 (- end trim-offset)) result))))))

(defun reset-saved-packet-data (fifo length)
  ;;Saved-packet fifo assumed to have exactly ONE element in it.
  (let* ((current (first (fifo-as-list fifo)))
         (buffer (rcv-buffer current))
         (not-holes (make-not-hole-list (rcv-holes current) (array-length buffer))))
    (do* ((source (trim-hole-list not-holes length) (cdr source))
          (dest (displace-hole-list source length) (cdr dest))
          (result dest))
         ((null source)
          (setf (rcv-push-offset current)
                (and result (rcv-push-offset current) (- (rcv-push-offset current) length)))
          (setf (rcv-holes current) (make-not-hole-list result (array-length buffer))))
      (copy-array-portion buffer (hole-start (first source)) (1+ (hole-end (first source)))
                          buffer (hole-start (first dest)) (1+ (hole-end (first dest)))))))

;;;   End of saved packet processing

(defun enter-time-wait (socket)
  (tcp-log :close-reply socket)
  (push-fifo '(:closed) (tcp-user-packet-list socket))
  (change-state socket :TIME-WAIT)
  (start-timeout (* 2 *msl*) 'flush-tcp-socket socket))

(defun process-received-packet (buffer header socket)
  "Process a TCP packet received for a particular tcp-socket"
  (with-lock ((tcp-user-lock socket))
    (unless (eq (tcp-user-state socket) :CLOSED)
      (funcall (ecase (tcp-user-state socket)
                 (:LISTEN 'process-packet-for-LISTEN)
                 (:SYN-SENT 'process-packet-for-SYN-SENT)
                 (:SYN-RECEIVED 'process-packet-for-SYN-RECEIVED)
                 (:ESTABLISHED 'process-packet-for-ESTABLISHED)
                 (:FIN-WAIT-1 'process-packet-for-FIN-WAIT-1)
                 (:FIN-WAIT-2 'process-packet-for-FIN-WAIT-2)
                 (:CLOSE-WAIT 'process-packet-for-CLOSE-WAIT)
                 (:CLOSING 'process-packet-for-CLOSING)
                 (:LAST-ACK 'process-packet-for-LAST-ACK)
                 (:TIME-WAIT 'process-packet-for-TIME-WAIT))
               buffer
               header
               socket)
      (unless (member (tcp-user-state socket) '(:CLOSED :LISTEN))
        ;;Send more data if the window has opened up.
        (send-tcp-packets socket)))))

(defun process-packet-for-LISTEN (buffer header socket)
  (unless (tcp-rst-bit-p buffer)
    (if (tcp-ack-bit-p buffer)
        (send-rst-packet buffer header "ACK bit on in LISTEN")
      (multiple-value-bind (precedence security compartment handling tcc)
          (find-security-and-precedence header)
        (let ((security-options (tcp-user-security-options socket)))
          (if (or (/= security (security security-options))
                  (/= compartment (compartment security-options))
                  (/= handling (handling security-options))
                  (/= tcc (tcc security-options)))
              (send-rst-packet buffer header "Bad security in LISTEN")
            (progn
              (tcp-stat-incf nil in-connection-accepts)
              (cancel-timeout socket 'listen-timeout)
              (when (> precedence (tcp-user-precedence socket))
                (setf (tcp-user-precedence socket) precedence)
                (set-precedence (tcp-user-ip-header socket) precedence))
              (get-remote-mss socket buffer)
              (let* ((ISS (choose-iss))
                     (IRS (SEG.SEQ buffer))
                     (RCV.NXT (32-bit-plus IRS 1)))
                (setf (IRS socket) IRS)
                (setf (RCV.NXT socket) RCV.NXT)
                (setf (RCV.LAST socket) RCV.NXT)
                (setf (ISS socket) ISS)
                (setf (SND.UNA socket) ISS)
                (setf (SND.NXT socket) ISS)
                (setf (SND.LAST socket) ISS)
                (setf (SND.WL1 socket) IRS)
                (setf (SND.WL2 socket) ISS))
              (change-state socket :SYN-RECEIVED)
              (setf (tcp-user-remote-port socket) (tcp-source-port buffer))
              (let ((remote-address (ip::ih-source-address header)))
                (setf (tcp-user-remote-address socket) remote-address)
                (copy-ip-header (reverse-route header) (tcp-user-ip-header socket) t)
                (set-interface-mss socket remote-address header)
                (incf (RCV.WND socket) (if (tcp-user-optimistic-window-p socket)
                                           (tcp-user-interface-mss socket)
                                         0)))
              (process-URG socket buffer)       ;Save other data and controls...
              (process-data socket buffer)
              )))))))

(defun process-packet-for-SYN-SENT (buffer header socket &aux (SEG.ACK (SEG.ACK buffer)))
  (when (tcp-ack-bit-p buffer)
    (when (or (32-bit-<= SEG.ACK (ISS socket))
              (32-bit-> SEG.ACK (SND.LAST socket)))
      (unless (tcp-rst-bit-p buffer)
        (send-rst-packet buffer header "Bad ACK in SYN-SENT"))
      (return-from process-packet-for-SYN-SENT)))
  (when (tcp-rst-bit-p buffer)
    (when (tcp-ack-bit-p buffer)
      (tcp-stat-incf nil out-connection-refused)
      (reset-connection socket nil "Received RST in SYN-SENT"))
    (return-from process-packet-for-SYN-SENT))
  (multiple-value-bind (precedence security compartment handling tcc)
      (find-security-and-precedence header)
    (let ((security-options (tcp-user-security-options socket)))
      (when (or (/= security (security security-options))
                (/= compartment (compartment security-options))
                (/= handling (handling security-options))
                (/= tcc (tcc security-options)))
        (send-rst-packet buffer header "Bad security in SYN-SENT")
        (return-from process-packet-for-SYN-SENT)))
    (if (tcp-ack-bit-p buffer)
        (unless (= precedence (tcp-user-precedence socket))
          (send-rst-packet buffer header "Bad precedence in SYN-SENT")
          (return-from process-packet-for-SYN-SENT))
      (when (> precedence (tcp-user-precedence socket))
        (setf (tcp-user-precedence socket) precedence)
        (set-precedence (tcp-user-ip-header socket) precedence))))
  (when (tcp-syn-bit-p buffer)
    (tcp-stat-incf nil out-connection-accepts)
    (get-remote-mss socket buffer)
    (setf (IRS socket) (SEG.SEQ buffer))
    (setf (RCV.NXT socket) (32-bit-plus (IRS socket) 1))
    (setf (RCV.LAST socket) (RCV.NXT socket))
    (setf (SND.WL1 socket) (IRS socket))
    (setf (SND.WL2 socket) (ISS socket))
    (cond ((32-bit-> SEG.ACK (ISS socket))
           (tcp-log :open-reply socket)
           (push-fifo '(:open) (tcp-user-packet-list socket))
           (change-state socket :ESTABLISHED)
           (process-ACK socket buffer)
           (process-URG socket buffer)
           (process-data socket buffer)
           (send-immediate-ack socket))
          (t                                    ;SYN not ACKed yet
           (change-state socket :SYN-RECEIVED)
           (setf (SND.NXT socket) (ISS socket)) ;back up & retransmit SYN with an ACK
           (process-ACK socket buffer)
           (process-URG socket buffer)          ;Save other data and controls...
           (process-data socket buffer)))
    ))

(defun check-packet-sequence-space (socket packet)
  "Function to check that sequence space consumed by a packet is within window"
  (let* ((packet-seq (SEG.SEQ packet))
         (packet-bytes (tcp-data-bytes packet))
         (SEG.LEN (- (compute-SEG.LEN packet) (tcp-fin-bit packet)))    ;Don't count FIN for window
         (packet-edge (32-bit-plus packet-seq (1- SEG.LEN)))
         (expected-seq (RCV.NXT socket))
         (last-seq (RCV.LAST socket))
         (window (RCV.WND socket))
         (window-edge (32-bit-plus expected-seq window)))
    (when (plusp packet-bytes)
      (cond ((32-bit-< packet-edge last-seq)
             (tcp-stat-incf socket total-duplicate-packets)
             (tcp-stat-incf socket total-duplicate-bytes packet-bytes))
            ((32-bit-< packet-seq last-seq)
             (let ((dup (32-bit-minus last-seq packet-seq)))
               (tcp-stat-incf socket partially-duplicate-packets)
               (tcp-stat-incf socket partially-duplicate-bytes dup)
               (tcp-stat-incf socket in-sequence-bytes (- packet-bytes dup))))
            ((= last-seq packet-seq)
             (tcp-stat-incf socket in-sequence-packets)
             (tcp-stat-incf socket in-sequence-bytes packet-bytes))
            (t                                  ;Out of order
             (tcp-stat-incf socket out-of-order-packets)
             (tcp-stat-incf socket out-of-order-bytes packet-bytes))))
    (when (32-bit->= packet-edge window-edge)
      (cond ((and (zerop (RCV.WND socket)) (= packet-seq (RCV.NXT socket)))
             (tcp-stat-incf socket window-probes-received))
            (t
             (tcp-stat-incf socket out-of-window-packets)))
      (tcp-stat-incf socket out-of-window-bytes (32-bit-plus (32-bit-minus packet-edge window-edge) 1)))

    (let ((result (if (zerop window)
                      ;;If window is closed, must be at expected spot in sequence space.  Note: there should
                      ;;not be data when our window is closed -- but ACK, RST, and URG must be processed.
                      (32-bit-<= packet-seq last-seq)
                    (or (and (32-bit-<= expected-seq packet-seq)        ;else, packet start must be in window
                             (if (zerop SEG.LEN)
                                 (32-bit-<= packet-seq window-edge)     ; (zero-length packet can be AT edge)
                               (32-bit-< packet-seq window-edge)))
                        (and (plusp SEG.LEN)    ;   or end of packet must be in window.
                             (32-bit-<= expected-seq packet-edge)
                             (32-bit-< packet-edge window-edge))))))
      (unless result
        (tcp-stat-incf socket bad-sequence-packets))
      result)))

(defun process-packet-for-SYN-RECEIVED (buffer header socket)
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-SYN-RECEIVED))
  (when (tcp-rst-bit-p buffer)
    (if (tcp-user-active socket)
        (reset-connection socket nil "Received RST in SYN-RECEIVED")
      (progn
        (unless (tcp-user-fully-specified socket)
          (setf (tcp-user-remote-port socket) nil)
          (setf (tcp-user-remote-address socket) nil))
        (cancel-timeout socket)
        (setf (tcp-user-last-time socket) nil)
        (setf (tcp-user-cumulative-timeout socket) 0)
        (setf (RCV.WND socket) 0)
        (change-state socket :LISTEN)
        (when (tcp-user-open-timeout socket)
          (start-timeout (tcp-user-open-timeout socket) 'listen-timeout socket))))
    (return-from process-packet-for-SYN-RECEIVED))
  (multiple-value-bind (precedence security compartment handling tcc)
      (find-security-and-precedence header)
    (let ((security-options (tcp-user-security-options socket)))
      (when (or (/= precedence (tcp-user-precedence socket))
                (/= security (security security-options))
                (/= compartment (compartment security-options))
                (/= handling (handling security-options))
                (/= tcc (tcc security-options)))
      (send-rst-packet buffer header "Bad security or precedence in SYN-RECEIVED")
      (return-from process-packet-for-SYN-RECEIVED))))
  (when (tcp-syn-bit-p buffer)
    (unless (= (SEG.SEQ buffer) (IRS socket))
      (reset-connection socket t "Bad SYN in SYN-RECEIVED")
      (return-from process-packet-for-SYN-RECEIVED)))
  (when (tcp-ack-bit-p buffer)
    (let ((SEG.ACK (SEG.ACK buffer)))
      (unless (and (32-bit-<= (SND.UNA socket) SEG.ACK)
                   (32-bit-<= SEG.ACK (SND.LAST socket)))
        (send-rst-packet buffer header "Bad ACK in SYN-RECEIVED")
        (return-from process-packet-for-SYN-RECEIVED))
      (tcp-log :open-reply socket)
      (push-fifo '(:open) (tcp-user-packet-list socket))
      (change-state socket :ESTABLISHED)
      (process-ACK socket buffer)
      (process-URG socket buffer)
      (process-data socket buffer))
    ))

(defun process-packet-for-ESTABLISHED (buffer header socket)
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-ESTABLISHED))
  (when (tcp-rst-bit-p buffer)
    (reset-connection socket nil "Received RST in ESTABLISHED")
    (return-from process-packet-for-ESTABLISHED))
  (multiple-value-bind (precedence security compartment handling tcc)
      (find-security-and-precedence header)
    (let ((security-options (tcp-user-security-options socket)))
      (cond ((/= precedence (tcp-user-precedence socket))
             (reset-connection socket t "Bad precedence in ESTABLISHED")
             (return-from process-packet-for-ESTABLISHED))
            ((or (/= security (security security-options))
                 (/= compartment (compartment security-options))
                 (/= handling (handling security-options))
                 (/= tcc (tcc security-options)))
             (reset-connection socket t "Bad security in ESTABLISHED")
             (return-from process-packet-for-ESTABLISHED))
            ((and (tcp-syn-bit-p buffer) (/= (SEG.SEQ buffer) (IRS socket)))
             (reset-connection socket t "Bad SYN in ESTABLISHED")
             (return-from process-packet-for-ESTABLISHED)))))
  (when (tcp-ack-bit-p buffer)
    (process-ACK socket buffer)
    (process-URG socket buffer)
    (process-data socket buffer))
  )

(defun process-packet-for-FIN-WAIT-1 (buffer header socket)
  (declare (ignore header))
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-FIN-WAIT-1))
  (when (tcp-rst-bit-p buffer)
    (reset-connection socket nil "Received RST in FIN-WAIT-1")
    (return-from process-packet-for-FIN-WAIT-1))
  (when (and (tcp-syn-bit-p buffer)
             (/= (SEG.SEQ buffer) (IRS socket)))
    (reset-connection socket t "Bad SYN in FIN-WAIT-1")
    (return-from process-packet-for-FIN-WAIT-1))
  (when (tcp-ack-bit-p buffer)
    (process-ACK socket buffer)
    (let ((local-fin (tcp-user-local-fin-sequence-number socket)))
      (when (and local-fin (32-bit-> (SND.UNA socket) local-fin))
        (change-state socket :FIN-WAIT-2)))
    (process-URG socket buffer)
    (process-data socket buffer))
  )

(defun process-packet-for-FIN-WAIT-2 (buffer header socket)
  (declare (ignore header))
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-FIN-WAIT-2))
  (when (tcp-rst-bit-p buffer)
    (reset-connection socket nil "Received RST in FIN-WAIT-2")
    (return-from process-packet-for-FIN-WAIT-2))
  (when (and (tcp-syn-bit-p buffer)
             (/= (SEG.SEQ buffer) (IRS socket)))
    (reset-connection socket t "Bad SYN in FIN-WAIT-2")
    (return-from process-packet-for-FIN-WAIT-2))
  (when (tcp-ack-bit-p buffer)
    (process-ACK socket buffer)
    (process-URG socket buffer)
    (process-data socket buffer))
  )

(defun process-packet-for-CLOSING (buffer header socket)
  (declare (ignore header))
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-CLOSING))
  (when (tcp-rst-bit-p buffer)
    (flush-tcp-socket socket)
    (return-from process-packet-for-CLOSING))
  (when (and (tcp-syn-bit-p buffer)
             (/= (SEG.SEQ buffer) (IRS socket)))
    (reset-connection socket t "Bad SYN in CLOSING")
    (return-from process-packet-for-CLOSING))
  (when (tcp-ack-bit-p buffer)
    (process-ACK socket buffer)
    (let ((local-fin (tcp-user-local-fin-sequence-number socket)))
      (when (and local-fin (32-bit-> (SND.UNA socket) local-fin))
        (enter-time-wait socket))))
  )

(defun process-packet-for-CLOSE-WAIT (buffer header socket)
  (declare (ignore header))
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-CLOSE-WAIT))
  (when (tcp-rst-bit-p buffer)
    (reset-connection socket nil "Received RST in CLOSE-WAIT")
    (return-from process-packet-for-CLOSE-WAIT))
  (when (and (tcp-syn-bit-p buffer)
             (/= (SEG.SEQ buffer) (IRS socket)))
    (reset-connection socket t "Bad SYN in CLOSE-WAIT")
    (return-from process-packet-for-CLOSE-WAIT))
  (when (tcp-ack-bit-p buffer)
    (process-ACK socket buffer))
  )

(defun process-packet-for-LAST-ACK (buffer header socket)
  (declare (ignore header))
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-LAST-ACK))
  (when (tcp-rst-bit-p buffer)
    (reset-connection socket nil "Received RST in LAST-ACK")
    (return-from process-packet-for-LAST-ACK))
  (when (and (tcp-syn-bit-p buffer)
             (/= (SEG.SEQ buffer) (IRS socket)))
    (reset-connection socket t "Bad SYN in LAST-ACK")
    (return-from process-packet-for-LAST-ACK))
  (when (tcp-ack-bit-p buffer)
    (process-ACK socket buffer)
    (let ((local-fin (tcp-user-local-fin-sequence-number socket)))
      (when (and local-fin (32-bit-> (SND.UNA socket) local-fin))
        (push-fifo '(:closed) (tcp-user-packet-list socket))
        (flush-tcp-socket socket))))
  )

(defun process-packet-for-TIME-WAIT (buffer header socket)
  (declare (ignore header))
  (unless (check-packet-sequence-space socket buffer)
    (unless (tcp-rst-bit-p buffer)
      (setf (tcp-user-ack-needed-p socket) t))
    (return-from process-packet-for-TIME-WAIT))
  (when (tcp-rst-bit-p buffer)
    (flush-tcp-socket socket)
    (return-from process-packet-for-TIME-WAIT))
  (when (and (tcp-syn-bit-p buffer)
             (/= (SEG.SEQ buffer) (IRS socket)))
    (reset-connection socket t "Bad SYN in TIME-WAIT")
    (return-from process-packet-for-TIME-WAIT))
  (when (and (tcp-ack-bit-p buffer)
             (tcp-fin-bit-p buffer))
    (send-immediate-ack socket)
    (start-timeout (* 2 *msl*) 'flush-tcp-socket socket)))

;;;Background process -- timeouts, etc.

(defvar *background-wakeup-time* nil
  "If non-NIL, the time when the background process should next wake up")

(defvar *global-timeouts* nil
  "List of timeouts (time function arg) not associated with a socket")

(defvar *tcp-background-process* (make-process "TCP Background Process"
                                               :warm-boot-action 'ignore
                                               :priority 25
                                               :arrest-reasons '(:not-running)))

(defun initialize-tcp-background-process ()
  (setq *send-blocked-sockets* (make-fifo))
  (setq *send-blocked-control-packets* (make-fifo))
  (setq *global-timeouts* nil)
  (send *tcp-background-process* :preset 'tcp-background-process)
  (send *tcp-background-process* :reset)
  (send *tcp-background-process* :run-reason :enable)
  (send *tcp-background-process* :revoke-arrest-reason :not-running))

(defun start-timeout (interval function socket)
  (without-interrupts
    (let* ((now (zl:time))
           (time (time-increment now (floor (* 60 interval))))
           (elt (assoc function (tcp-user-timeout-alist socket) :test #'eq)))
      (if elt
          (setf (cdr elt) time)
        (push (cons function time) (tcp-user-timeout-alist socket)))
      (if (or (null *background-wakeup-time*)
              (time-lessp time *background-wakeup-time*))
          (setq *background-wakeup-time* time)))))

(defun start-global-timeout (interval function arg)
  (without-interrupts
    (let ((time (time-increment (zl:time) (floor (* 60 interval)))))
      (push (list time function arg) *global-timeouts*)
      (if (or (null *background-wakeup-time*)
              (time-lessp time *background-wakeup-time*))
          (setq *background-wakeup-time* time)))))

(defun cancel-timeout (socket &optional function)
  (without-interrupts
    (cond (function
           ;;Cancelling a specific timeout
           (let ((elt (assoc function (tcp-user-timeout-alist socket) :test #'eq)))
             (when elt
               (tcp-log :cancel socket function)
               (setf (tcp-user-timeout-alist socket) (delete elt (tcp-user-timeout-alist socket))))))
          ((tcp-user-timeout-alist socket)
           ;;Cancelling all timeouts and some are set
           (tcp-log :cancel socket nil)
           (setf (tcp-user-timeout-alist socket) nil))
          (t
           ;;timeout not set...
           nil))))

(defun tcp-background-process-wait (int-pkt now)
 (and (tcp-enabled *tcp-stream*)
      (or (and (or (not (fifo-empty-p *send-blocked-control-packets*))
                   (not (fifo-empty-p *send-blocked-sockets*)))
               (or (cdr int-pkt)
                   (setf (cdr int-pkt) (net:allocate-packet nil))))
          (and *background-wakeup-time*
               (not (time-lessp (setf (cdr now) (zl:time))
                                *background-wakeup-time*))))))

(defun tcp-background-process ()
  (loop
    (let (int-pkt now)
      (process-wait "Timeout" 'tcp-background-process-wait (locf int-pkt) (locf now))
      (tcp-background-process-1 int-pkt now))))

(defun tcp-background-process-1 (int-pkt now)
  (let (elt socket)
    (cond ((not (tcp-enabled *tcp-stream*)))    ;tcp disabled -- do nothing
          (int-pkt                              ;send blocked stream or packet -- send it
           (cond ((setq elt (pop-fifo *send-blocked-control-packets*))
                  (apply 'send-control-packet (nconc elt (ncons int-pkt))))
                 ((setq socket (pop-fifo *send-blocked-sockets*))
                  (tcp-log :unblock socket)
                  (send-tcp-packets socket t int-pkt))
                 (t
                  (net:free-packet int-pkt))))
          (t                                    ;generic timeout
           ;;Do the global timeouts
           (do ((list *global-timeouts* (cdr list))
                (result nil))
               ((null list)
                (setq *global-timeouts* result))
             (let* ((elt (car list))
                    (time (first elt)))
               (if (time-lessp time now)
                   (funcall (second elt) (third elt) now)
                 (push elt result))))

           ;;And the socket specific timeouts
           (do ((list (tcp-user-socket-alist *tcp-stream*) (cdr list)))
               ((null list))
             (let* ((socket (cdar list))
                    (passed (mapcan #'(lambda (x) (if (time-lessp (cdr x) now) (ncons x)))
                                    (tcp-user-timeout-alist socket))))
               (when passed
                 (setf (tcp-user-timeout-alist socket)
                       (nset-difference (tcp-user-timeout-alist socket) passed))
                 (dolist (x passed)
                   (funcall (car x) socket now)))))

           ;;Reset next wakeup time
           (without-interrupts
             (let ((next-wakeup-time nil))
               (dolist (x *global-timeouts*)
                 (when (or (null next-wakeup-time)
                           (time-lessp (first x) next-wakeup-time))
                   (setq next-wakeup-time (first x))))
               (dolist (elt (tcp-user-socket-alist *tcp-stream*))
                 (dolist (x (tcp-user-timeout-alist (cdr elt)))
                   (when (or (null next-wakeup-time)
                             (time-lessp (cdr x) next-wakeup-time))
                     (setq next-wakeup-time (cdr x)))))
               (setq *background-wakeup-time* next-wakeup-time)))))))

(defun listen-timeout (socket ignore)
  (with-lock ((tcp-user-lock socket))
    (when (eq (tcp-user-state socket) :listen)
      (tcp-log :user-timeout socket)
      (flush-tcp-socket socket)
      (push-fifo '(:timeout) (tcp-user-packet-list socket))
      (tcp-stat-incf socket in-connection-timeouts))))

(defun retransmission-timeout (socket now)
  (tcp-log :timeout socket)
  (tcp-stat-incf socket retransmission-timeouts)
  (with-lock ((tcp-user-lock socket))
    (let ((last-time (tcp-user-last-time socket))
          (send-timeout (tcp-user-send-timeout socket))
          (SND.NXT (SND.NXT socket))
          (SND.UNA (SND.UNA socket))
          (ISS (ISS socket))
          (local-fin (tcp-user-local-fin-sequence-number socket)))
      (when (and (not (eq (tcp-user-state socket) :closed))     ;Unless socket aborted while waiting for lock,
                 last-time                                      ;the timeout was cancelled,
                 (time-lessp last-time now)                     ;or timeout moved
                 (< (tcp-user-cumulative-timeout socket) send-timeout)) ;and timeout not-yet-signalled,
        ;;Reset Data sending variables
        (decf (tcp-user-send-data-offset socket)
              (- (32-bit-minus SND.NXT SND.UNA)
                 (cond ((= SND.NXT ISS) 0)      ;Couldn't route
                       ((= SND.UNA ISS) 1)      ;SYN has been sent but not ACKed
                       (t 0))
                 (cond ((null local-fin) 0)
                       ((32-bit-> SND.NXT local-fin) 1)
                       (t 0))))
        (setf (SND.NXT socket) (SND.UNA socket))
        (when local-fin
          (setf (tcp-user-fin-needed-p socket) t))
        (incf (tcp-user-cumulative-timeout socket) (time-difference now last-time))
        (cond ((< (tcp-user-cumulative-timeout socket) send-timeout)
               (send-tcp-packets socket t))
              (t
               ;;Send timeout has expired -- signal user
               (tcp-stat-incf socket (if (= (SND.UNA socket) (ISS socket))
                                         out-connection-timeouts
                                       user-timeouts))
               (tcp-log :user-timeout socket (tcp-user-cumulative-timeout socket) send-timeout)
               (push-fifo '(:timeout) (tcp-user-packet-list socket))))))))

;;;IP interface

(defun tcp-close (stream &optional (abort t))
  (do ()
      ((null (tcp-receive-buffers stream)))
    (free-tcp-buffer (pop (tcp-receive-buffers stream))))
  (close-tcp-sockets stream abort))

(defun tcp-reset (stream)
  (close-tcp-sockets stream :abort))

(defun close-tcp-sockets (stream abort)
  (dolist (user (tcp-user-socket-alist stream))
    (send (cdr user) :close abort)))

(defun match-tcp-msg-with-socket (local-port remote-port remote-address)
  (do ((list (tcp-user-socket-alist *tcp-stream*) (cdr list))
       (generic-handler nil))
      ((null list) generic-handler)
    (when (= (caar list) local-port)
      (let* ((match (cdar list))
             (rport (tcp-user-remote-port match)))
        (if (null rport)
            (setq generic-handler match)
          (and (= remote-port rport)
               (= remote-address (tcp-user-remote-address match))
               (return match)))))))

(defun tcp-icmp (type header buffer)
  (let ((user-socket (match-tcp-msg-with-socket (tcp-source-port buffer)
                                                (tcp-destination-port buffer)
                                                (ip:ih-dest-address header))))
    (if user-socket
        (push-fifo (list type header buffer) (tcp-user-packet-list user-socket))
      (free-ip-header header))))

(defun receive-tcp-packet (stream buffer header local-p broadcast-p interface)
  (declare (ignore broadcast-p interface))
  (let ((source (ip::ih-source-address header))
        (destination (ip::ih-dest-address header))
        (user-socket nil))
    (tcp-log :receive nil
             (copy-tcp-header buffer (get-tcp-header))
             (tcp-data-bytes buffer)
             source
             destination
             (when (plusp *tcp-log-level*)
               (string (make-array (tcp-data-bytes buffer)
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to buffer
                                   :displaced-index-offset (tcp-first-data-byte buffer)))))

    (unwind-protect
        (cond ((not (check-tcp-checksum buffer header local-p))
               (tcp-stat-incf nil checksum-failures))
              ((< (length buffer) 20)
               (tcp-stat-incf nil too-short))
              ((< (tcp-data-offset buffer) 5)
               (tcp-stat-incf nil bad-header-offset))
              ((setq user-socket (match-tcp-msg-with-socket (tcp-destination-port buffer)
                                                            (tcp-source-port buffer)
                                                            source))
               (incf (tcp-user-packets-received user-socket))
               (incf (tcp-user-bytes-received user-socket) (tcp-data-bytes buffer))
               (process-received-packet buffer header user-socket))
              (t                                ;(null user-socket)
               (unless (tcp-rst-bit-p buffer)
                 (when (tcp-syn-bit-p buffer)
                   (tcp-stat-incf nil in-connection-refused))
                 (send-rst-packet buffer header "No connection"))
               (incf (net:tp-packets-received-discarded stream))
               (incf (net:tp-bytes-received-discarded stream) (length buffer))))
      (free-ip-header header)
      (send stream :receive buffer))))

;;;TCP logging
(defvar *tcp-log* (make-fifo) "FIFO of TCP headers sent and received")

(defun initialize-tcp-log ()
  (let ((*tcp-logging* nil))
    (declare (special *tcp-logging*))
    (do ()
        ((fifo-empty-p *tcp-log*))
      (let ((elt (pop-fifo *tcp-log*)))
        (when (member (first elt) '(:send :receive))
          (free-tcp-header (second elt)))))))

(defun tcp-logging (&optional (arg t arg-p) (initialize t))
  "Turn on and off tcp logging.  With ARG = T, turns on and, by default, initializes the TCP log.
With ARG = NIL, turns off logging.  With ARG unspecified, merely reports state of logging."
  (when arg-p
    (when (and arg initialize)
      (initialize-tcp-log))
    (setq *tcp-logging* arg))
  (if *tcp-logging* "Enabled" "Disabled"))

(defun tcp-receive-logging (enable)
  "Turns on TCP logging for data reception"
  (setq *tcp-logging* nil)
  (setq *tcp-log-keywords* nil)
  (initialize-tcp-log)
  (when enable
    (setq *tcp-log-keywords* '(:open :close :read :receive :copy :save :retrieve :drop :read-reply :send-ack))
    (setq *tcp-logging* t)))

(defun tcp-log-internal (keyword socket &rest args)
  (when (or (null *tcp-log-keywords*)
            (member keyword *tcp-log-keywords* :test #'eq))
    (push-fifo (nconc (ncons keyword)
                      (if socket (ncons (tcp-user-local-port socket)))
                      (if socket (ncons (tcp-user-remote-port socket)))
                      (copy-list args))
               *tcp-log*)))

(defun print-tcp-log (&optional (stream *standard-output*))
  (unless (fifo-empty-p *tcp-log*)
    (format stream "~&S/R sport dport      SEQ      ACK  flags   WND    UP  data")
    (dolist (elt (fifo-as-list *tcp-log*))
      (let ((keyword (case (car elt)
                       (:copy :CPY)
                       (:save :SAV)
                       (:retrieve :RET)
                       (:drop :DRP)
                       (:delay :DLY)
                       (:wait :WAI)
                       (:skip :SKP)
                       (:send-ack :ACK)
                       (:timeout :TMO)
                       (:user-timeout :UTM)
                       (:cancel :CAN)
                       (:open :OP)
                       (:open-reply :OPR)
                       (:close :CL)
                       (:close-reply :CLR)
                       (:abort :ABO)
                       (:read :RD)
                       (:read-reply :RDR)
                       (:write :WR)
                       (:write-reply :WRR)
                       (:reset-received :RSR)
                       (:reset-packet :RSP)
                       (:reset-sent :RSS)
                       (:receive :RCV)
                       (:send :SND)
                       (:failed-send :FAI)
                       (:failed-send-timeout :FST)
                       (:block :BLK)
                       (:unblock :UBL)
                       (:initial-rtt :IRT)
                       (:rtt :RTT)))
            (args (cdr elt)))
        (case (car elt)
          ((:copy :save :retrieve :drop)
           (format stream "~&~A ~5D ~5D ~5D bytes from ~D -> ~D"
                   keyword
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)
                   (fifth args)))
          (:user-timeout
           (format stream "~&~3A ~5D ~5D cum = ~D tmo = ~D"
                   keyword
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)))
          (:initial-rtt
           (format stream "~&~3A ~5D ~5D ~,4F"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          (:rtt
           (format stream "~&~3A ~5D ~5D ~,4F -> ~,4F"
                   keyword
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)))
          (:send-ack
           (format stream "~&~3A ~5D ~5D ~16,8,'0R"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          ((:delay :wait :skip :timeout :cancel :open :open-reply :close :close-reply :abort
                   :failed-send :failed-send-timeout :block :unblock)
           (format stream "~&~3A ~5D ~5D"
                   keyword
                   (first args)
                   (second args)))
          (:read-reply
           (format stream "~&~3A ~5D ~5D ~5D bytes~@[ ~D~]~@[ URG = ~D~]"
                   keyword
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)
                   (fifth args)))
          ((:read :write :write-reply)
           (format stream "~&~3A ~5D ~5D ~5D bytes"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          (:state
           (format stream "~&STC ~5D ~5D ~A -> ~A"
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)))
          (:reset-received
           (format stream "~&~3A ~5D ~5D ~S"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          (:reset-sent
           (format stream "~&~3A ~5D ~5D ~S"
                   keyword
                   (first args)
                   (second args)
                   (fourth args)))
          (:reset-packet
           (let* ((pkt (first args))
                  (source (tcp-source-port pkt))
                  (dest (tcp-destination-port pkt)))
             (format stream "~&~3A (~D[~A]->~D) ~S"
                     keyword
                     source
                     (canonical-ip (third args))
                     dest
                     (second args))))
          ((:send :receive)
           (let ((pkt (first args))
                 (data-bytes (second args))
                 (source (third args))
                 (destination (fourth args)))
             (format stream
                     "~&~3A ~5D ~5D ~16,8,'0R ~16,8,'0R ~:[ ~;U~]~:[ ~;A~]~:[ ~;P~]~:[ ~;R~]~:[ ~;S~]~:[ ~;F~] ~5D ~5D ~5D ~A -> ~A"
                     keyword
                     (tcp-source-port pkt)
                     (tcp-destination-port pkt)
                     (tcp-sequence-number pkt)
                     (tcp-ack-number pkt)
                     (tcp-urg-bit-p pkt)
                     (tcp-ack-bit-p pkt)
                     (tcp-psh-bit-p pkt)
                     (tcp-rst-bit-p pkt)
                     (tcp-syn-bit-p pkt)
                     (tcp-fin-bit-p pkt)
                     (tcp-window pkt)
                     (tcp-urgent-pointer pkt)
                     data-bytes
                     (canonical-ip source)
                     (canonical-ip destination))
             (when (fifth args)
               (format stream "~&~S" (fifth args))))))))))

(defun print-tcp-receive-log (&optional (stream *standard-output*))
  (unless (fifo-empty-p *tcp-log*)
    (format stream "~&S/R sport dport      SEQ      flags   WND    UP  data")
    (dolist (elt (fifo-as-list *tcp-log*))
      (let ((keyword (case (car elt)
                       (:copy :CPY)
                       (:save :SAV)
                       (:retrieve :RET)
                       (:drop :DRP)
                       (:send-ack :ACK)
                       (:open :OP)
                       (:close :CL)
                       (:read :RD)
                       (:read-reply :RDR)
                       (:receive :RCV)))
            (args (cdr elt)))
        (case (car elt)
          ((:copy :save :retrieve :drop)
           (format stream "~&~A ~5D ~5D ~5D bytes from ~D -> ~D"
                   keyword
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)
                   (fifth args)))
          (:send-ack
           (format stream "~&~3A ~5D ~5D ~D"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          ((:open :close)
           (format stream "~&~3A ~5D ~5D"
                   keyword
                   (first args)
                   (second args)))
          (:read
           (format stream "~&~3A ~5D ~5D ~5D bytes"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          (:read-reply
           (format stream "~&~3A ~5D ~5D ~5D bytes~@[ ~D~]~@[ URG = ~D~]"
                   keyword
                   (first args)
                   (second args)
                   (third args)
                   (fourth args)
                   (fifth args)))
          ((:read :write :write-reply)
           (format stream "~&~3A ~5D ~5D ~5D bytes"
                   keyword
                   (first args)
                   (second args)
                   (third args)))
          (:receive
           (let ((pkt (first args))
                 (data-bytes (second args)))
             (format stream
                     "~&~3A ~5D ~5D ~D ~:[ ~;U~]~:[ ~;A~]~:[ ~;P~]~:[ ~;R~]~:[ ~;S~]~:[ ~;F~] ~5D ~5D ~5D"
                     keyword
                     (tcp-source-port pkt)
                     (tcp-destination-port pkt)
                     (tcp-sequence-number pkt)
                     (tcp-urg-bit-p pkt)
                     (tcp-ack-bit-p pkt)
                     (tcp-psh-bit-p pkt)
                     (tcp-rst-bit-p pkt)
                     (tcp-syn-bit-p pkt)
                     (tcp-fin-bit-p pkt)
                     (tcp-window pkt)
                     (tcp-urgent-pointer pkt)
                     data-bytes)
             (when (fifth args)
               (format stream "~&~S" (fifth args))))))))))

(defun setup-tcp-logging ()
  (tv:add-escape-key #\network
                     #'(lambda (x)
                         (case x
                           (nil (tcp-logging (not *tcp-logging*) nil))
                           (0 (tcp-logging nil nil))
                           (1 (tcp-logging t nil))
                           (2 (initialize-tcp-log))))
                     "Toggle TCP logging, or prefix with 0: disable, 1: enable, 2: initialize"
                     :keyboard-process))

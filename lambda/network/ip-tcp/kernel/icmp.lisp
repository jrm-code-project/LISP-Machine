;;; -*- Mode:LISP; Package:ICMP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *icmp-stream*
           setup-icmp
           describe-icmp
           icmp
           with-icmp-socket
           ping))

;;;ICMP interface to IP
(defstream icmp-ip-transport-protocol
           (ip-transport-protocol)
           "ICMP-"
  (user-socket-alist nil)                       ;user sockets
  (next-identifier 0)                           ;next user socket identifier to assign
  (echo-sent 0)                                 ;echo messages sent
  (echo-received 0)                             ;echo messages received
  (echo-reply-sent 0)                           ;echo-reply messages sent
  (echo-reply-received 0)                       ;echo-reply messages received
  (info-sent 0)                                 ;info messages sent
  (info-received 0)                             ;info messages received
  (info-reply-sent 0)                           ;info-reply messages sent
  (info-reply-received 0)                       ;info-reply messages received
  (timestamp-sent 0)                            ;timestamp messages sent
  (timestamp-received 0)                        ;timestamp messages received
  (timestamp-reply-sent 0)                      ;timestamp-reply messages sent
  (timestamp-reply-received 0)                  ;timestamp-reply messages received
  (address-mask-sent 0)                         ;address-mask messages sent
  (address-mask-received 0)                     ;address-mask messages received
  (address-mask-reply-sent 0)                   ;address-mask-reply messages sent
  (address-mask-reply-received 0)               ;address-mask-reply messages received
  (destination-unreachable-sent 0)              ;destination-unreachable messages sent
  (destination-unreachable-received 0)          ;destination-unreachable messages received
  (source-quench-sent 0)                        ;source-quench messages sent
  (source-quench-received 0)                    ;source-quench messages received
  (redirect-sent 0)                             ;redirect messages sent
  (redirect-received 0)                         ;redirect messages received
  (parameter-problem-sent 0)                    ;parameter-problem messages sent
  (parameter-problem-received 0)                ;parameter-problem messages received
  (time-exceeded-sent 0)                        ;time-exceeded messages sent
  (time-exceeded-received 0)                    ;time-exceeded messages received
  (local-checksums nil)                         ;T if calculate checksums for looped-back packets
  (checksum-failures 0)                         ;number of packets with bad ICMP checksum
  )

(defop (icmp-ip-transport-protocol :peek-normal-fields) (tp)
  (ncons (tv:scroll-parse-item
           `(:function icmp-next-identifier (,tp) NIL ("  Next identifier assigned = ~D"))
           `(:function icmp-checksum-failures (,tp) NIL ("  Checksum failures ~D"))
           `(:mouse-item
              (nil :buttons
                   ,(make-list 3 :initial-element
                               `(nil :eval (setf (icmp-local-checksums tp)
                                                 (not (icmp-local-checksums tp)))
                                     :bindings ((tp ',tp))))
                   :DOCUMENTATION
                   "Click to toggle local checksums"
                   :BINDINGS
                   ((tp ',tp)))
              :function icmp-local-checksums (,tp) NIL (" Local checksums: ~A")))))

(defop (icmp-ip-transport-protocol :peek-verbose-fields) (tp)
  (list (tv:scroll-parse-item
          "  ****** Echo/Rply Info/Rply Time/Rply Mask/Rply RDIR QNCH TEXC UNRC PARM")
        (tv:scroll-parse-item
          "  Sent: "
          `(:function icmp-echo-sent (,tp) NIL ("~5d"))
          `(:function icmp-echo-reply-sent (,tp) NIL ("~5d"))
          `(:function icmp-info-sent (,tp) NIL ("~5d"))
          `(:function icmp-info-reply-sent (,tp) NIL ("~5d"))
          `(:function icmp-timestamp-sent (,tp) NIL ("~5d"))
          `(:function icmp-timestamp-reply-sent (,tp) NIL ("~5d"))
          `(:function icmp-address-mask-sent (,tp) NIL ("~5d"))
          `(:function icmp-address-mask-reply-sent (,tp) NIL ("~5d"))
          `(:function icmp-redirect-sent (,tp) NIL ("~5d"))
          `(:function icmp-source-quench-sent (,tp) NIL ("~5d"))
          `(:function icmp-time-exceeded-sent (,tp) NIL ("~5d"))
          `(:function icmp-destination-unreachable-sent (,tp) NIL ("~5d"))
          `(:function icmp-parameter-problem-sent (,tp) NIL ("~5d"))
          )
        (tv:scroll-parse-item
          "  Rcvd: "
          `(:function icmp-echo-received (,tp) NIL ("~5d"))
          `(:function icmp-echo-reply-received (,tp) NIL ("~5d"))
          `(:function icmp-info-received (,tp) NIL ("~5d"))
          `(:function icmp-info-reply-received (,tp) NIL ("~5d"))
          `(:function icmp-timestamp-received (,tp) NIL ("~5d"))
          `(:function icmp-timestamp-reply-received (,tp) NIL ("~5d"))
          `(:function icmp-address-mask-received (,tp) NIL ("~5d"))
          `(:function icmp-address-mask-reply-received (,tp) NIL ("~5d"))
          `(:function icmp-redirect-received (,tp) NIL ("~5d"))
          `(:function icmp-source-quench-received (,tp) NIL ("~5d"))
          `(:function icmp-time-exceeded-received (,tp) NIL ("~5d"))
          `(:function icmp-destination-unreachable-received (,tp) NIL ("~5d"))
          `(:function icmp-parameter-problem-received (,tp) NIL ("~5d"))
          )))

(defop (icmp-ip-transport-protocol :peek-final-fields) (tp)
  (ncons (TV:SCROLL-MAINTAIN-LIST
           #'(LAMBDA () (icmp-user-socket-alist tp))
           'peek-icmp-socket)))

(defun peek-icmp-socket (elt)
  (let ((socket (cdr elt)))
    (send socket :peek-item elt)))

(defvar *icmp-stream* nil "The ip-transport-protocol for ICMP")
(defvar *icmp-receives-out* 4 "Number of outstanding receives")

(defun setup-icmp ()
  (setq *icmp-stream* (make-icmp-ip-transport-protocol :keyword :icmp
                                                       :receive-function 'receive-icmp-packet
                                                       :reset-function 'icmp-reset
                                                       :close-function 'icmp-close
                                                       :type icmp-protocol
                                                       :gauge-name "ICMP"
                                                       :broadcast-allowed-p t
                                                       ))

  (send *icmp-stream* :open)                    ;let IP know about us
  (send *icmp-stream* :enable)          ;set up for business
  (dotimes (i *icmp-receives-out*)              ;and give IP some read buffers
    (send *icmp-stream* :receive (get-icmp-message)))
  t)

(defun icmp-close (stream &optional abort)
  (declare (ignore abort))
  (do ()
      ((null (icmp-receive-buffers stream)))
    (free-icmp-message (pop (icmp-receive-buffers stream))))
  (close-icmp-sockets stream))

(defun icmp-reset (stream)
  (close-icmp-sockets stream))

(defun close-icmp-sockets (stream)
  (dolist (user (icmp-user-socket-alist stream))
    (send (cdr user) :close)))

;;;ICMP header definitions

(defmacro icmp-type (icmp)
  `(aref ,icmp 0))

(defmacro icmp-code (icmp)
  `(aref ,icmp 1))

(defmacro icmp-checksum (icmp)
  `(dpb (aref ,icmp 2) (byte 8 8) (aref ,icmp 3)))
(defun set-icmp-checksum (icmp val)
  (setf (aref icmp 2) (ldb (byte 8 8) val))
  (setf (aref icmp 3) (ldb (byte 8 0) val))
  val)
(defsetf icmp-checksum set-icmp-checksum)

;;;Here follow the message-specific bytes.

(defmacro icmp-byte-1 (icmp)
  `(aref ,icmp 4))

(defmacro icmp-byte-2 (icmp)
  `(aref ,icmp 5))

(defmacro icmp-byte-3 (icmp)
  `(aref ,icmp 6))

(defmacro icmp-byte-4 (icmp)
  `(aref ,icmp 7))

;;;Redirect has a 4 byte gateway address
(defsubst icmp-gateway (icmp)
  (dpb (aref icmp 4) (byte 8 24.)
       (dpb (aref icmp 5) (byte 8 16.)
            (dpb (aref icmp 6) (byte 8 8)
                 (aref icmp 7)))))
(defun set-icmp-gateway (icmp val)
  (setf (aref icmp 4) (ldb (byte 8 24.) val))
  (setf (aref icmp 5) (ldb (byte 8 16.) val))
  (setf (aref icmp 6) (ldb (byte 8 8) val))
  (setf (aref icmp 7) (ldb (byte 8 0) val))
  val)
(defsetf icmp-gateway set-icmp-gateway)

;;;Parameter problem has a 1 byte pointer
(defmacro icmp-parameter-problem-pointer (icmp)
  `(aref ,icmp 4))

;;;Echo, Timestamp, Info, and Address Mask use sequence and identifier
(defmacro icmp-identifier (icmp)
  `(dpb (aref ,icmp 4) (byte 8 8) (aref ,icmp 5)))
(defun set-icmp-identifier (icmp val)
  (setf (aref icmp 4) (ldb (byte 8 8) val))
  (setf (aref icmp 5) (ldb (byte 8 0) val))
  val)
(defsetf icmp-identifier set-icmp-identifier)

(defmacro icmp-sequence (icmp)
  `(dpb (aref ,icmp 6) (byte 8 8) (aref ,icmp 7)))
(defun set-icmp-sequence (icmp val)
  (setf (aref icmp 6) (ldb (byte 8 8) val))
  (setf (aref icmp 7) (ldb (byte 8 0) val))
  val)
(defsetf icmp-sequence set-icmp-sequence)

;;;Timestamp uses 3 4-byte timestamps
(defsubst icmp-originate-timestamp (icmp)
  (dpb (aref icmp 8) (byte 8 24.)
       (dpb (aref icmp 9) (byte 8 16.)
            (dpb (aref icmp 10) (byte 8 8)
                 (aref icmp 11)))))
(defun set-icmp-originate-timestamp (icmp val)
  (setf (aref icmp 8) (ldb (byte 8 24.) val))
  (setf (aref icmp 9) (ldb (byte 8 16.) val))
  (setf (aref icmp 10) (ldb (byte 8 8) val))
  (setf (aref icmp 11) (ldb (byte 8 0) val))
  val)
(defsetf icmp-originate-timestamp set-icmp-originate-timestamp)

(defsubst icmp-receive-timestamp (icmp)
  (dpb (aref icmp 12) (byte 8 24.)
       (dpb (aref icmp 13) (byte 8 16.)
            (dpb (aref icmp 14) (byte 8 8)
                 (aref icmp 15)))))
(defun set-icmp-receive-timestamp (icmp val)
  (setf (aref icmp 12) (ldb (byte 8 24.) val))
  (setf (aref icmp 13) (ldb (byte 8 16.) val))
  (setf (aref icmp 14) (ldb (byte 8 8) val))
  (setf (aref icmp 15) (ldb (byte 8 0) val))
  val)
(defsetf icmp-receive-timestamp set-icmp-receive-timestamp)

(defsubst icmp-transmit-timestamp (icmp)
  (dpb (aref icmp 16) (byte 8 24.)
       (dpb (aref icmp 17) (byte 8 16.)
            (dpb (aref icmp 18) (byte 8 8)
                 (aref icmp 19)))))
(defun set-icmp-transmit-timestamp (icmp val)
  (setf (aref icmp 16) (ldb (byte 8 24.) val))
  (setf (aref icmp 17) (ldb (byte 8 16.) val))
  (setf (aref icmp 18) (ldb (byte 8 8) val))
  (setf (aref icmp 19) (ldb (byte 8 0) val))
  val)
(defsetf icmp-transmit-timestamp set-icmp-transmit-timestamp)

;;;Address Mask uses a 4-byte mask
(defsubst icmp-address-mask (icmp)
  (dpb (aref icmp 8) (byte 8 24.)
       (dpb (aref icmp 9) (byte 8 16.)
            (dpb (aref icmp 10) (byte 8 8)
                 (aref icmp 11)))))
(defsetf icmp-address-mask set-icmp-originate-timestamp)

(eval-when (load compile eval)
  (defvar *icmp-message-list* nil "Valid user-sendable ICMP message types")
  (defvar *icmp-type-alist* nil "Association list between keyword and code")
  (defvar *icmp-int-pkt-messages* nil "List of messages with int-pkt as parameter"))

(defmacro define-icmp-message (keyword type &optional user-sendable-p int-pkt-p)
  (let ((name (intern (string-append "ICMP-" keyword))))
    `(eval-when (load compile eval)
       (defconstant ,name ,type)
       (when ,int-pkt-p
         (pushnew ,keyword *icmp-int-pkt-messages*))
       (unless (assoc ,keyword *icmp-type-alist*)
         (push (cons ,keyword ,type) *icmp-type-alist*))
       (when ,user-sendable-p
         (pushnew ,keyword *icmp-message-list*)))))

(define-icmp-message :echo-reply 0)
(define-icmp-message :destination-unreachable 3 t t)
(define-icmp-message :source-quench 4 t t)
(define-icmp-message :redirect 5 t t)
(define-icmp-message :echo 8 t)
(define-icmp-message :time-exceeded 11 t t)
(define-icmp-message :parameter-problem 12 t t)
(define-icmp-message :timestamp 13 t)
(define-icmp-message :timestamp-reply 14)
(define-icmp-message :information-request 15 t)
(define-icmp-message :information-reply 16)
(define-icmp-message :address-mask-request 17 t)
(define-icmp-message :address-mask-reply 18)

(defvar *free-icmp-messages* nil "List of free ICMP message buffers")

(defun get-icmp-message ()
  "returns an array to hold an ICMP message."
  (if *free-icmp-messages*
      (pop *free-icmp-messages*)
    (zl:make-array 1480                         ; + 20 IP = 1500 bytes
                   :element-type '(unsigned-byte 8)
                   :fill-pointer 0
                   :leader-length 2
                   :named-structure-symbol 'icmp-message)))

(defun free-icmp-message (icmp)
  (push icmp *free-icmp-messages*)
  nil)

(defun icmp-message-p (icmp)
  (eq (named-structure-p icmp) 'icmp-message))

(defselect ((:property icmp-message named-structure-invoke))
  (:describe (message)
    (describe-icmp message))
  (:print-self (message stream ignore ignore)
    (si:printing-random-object
      (message stream :type :no-pointer)
      (format stream "~A"
              (car (rassoc (icmp-type message) *icmp-type-alist*))))))

(defun describe-icmp (icmp &aux type-name ip-header)
  "Given an art-8b array, displays the ICMP message in it"
  (setq type-name (car (rassoc (icmp-type icmp) *icmp-type-alist*)))
  (format t "~&message type: ~20t~A (~D)" type-name (icmp-type icmp))
  (format t "~&checksum: ~20t~D" (icmp-checksum icmp))
  (when (member type-name *icmp-int-pkt-messages*)
    (setq ip-header (make-array 60
                                :element-type '(unsigned-byte 8)
                                :displaced-to icmp
                                :displaced-index-offset 8)))
  (case (icmp-type icmp)
    ((#.icmp-echo #.icmp-echo-reply)
     (format t "~&Identifier: ~20t~D" (icmp-identifier icmp))
     (format t "~&Sequence number: ~20t~D" (icmp-sequence icmp))
     (format t "~&Echo data:")
     (do* ((offset 8 (1+ offset))
           (count (- (fill-pointer icmp) 8))
           (index 0 (1+ index)))
          ((= index count) nil)
       (if (zerop (mod index 16))
           (format t "~&"))
       (format t "~16,2,'0r " (aref icmp offset))))
    ((#.icmp-timestamp #.icmp-timestamp-reply)
     (format t "~&Identifier: ~20t~D" (icmp-identifier icmp))
     (format t "~&Sequence number: ~20t~D" (icmp-sequence icmp))
     (format t "~&Originate timestamp: ~20t~D" (icmp-originate-timestamp icmp))
     (format t "~&Receive timestamp: ~20t~D" (icmp-receive-timestamp icmp))
     (format t "~&Transmit timestamp: ~20t~D" (icmp-transmit-timestamp icmp)))
    ((#.icmp-information-request #.icmp-information-reply)
     (format t "~&Identifier: ~20t~D" (icmp-identifier icmp))
     (format t "~&Sequence number: ~20t~D" (icmp-sequence icmp)))
    ((#.icmp-address-mask-request #.icmp-address-mask-reply)
     (format t "~&Identifier: ~20t~D" (icmp-identifier icmp))
     (format t "~&Sequence number: ~20t~D" (icmp-sequence icmp))
     (format t "~&Address Mask: ~20t~16,8,'0r" (icmp-address-mask icmp)))
    (#.icmp-destination-unreachable
     (format t "~&code: ~20t~D" (icmp-code icmp))
     (format t "~&In response to IP header:")
     (describe-ih ip-header 8))
    (#.icmp-source-quench
     (format t "~&In response to IP header:")
     (describe-ih ip-header 8))
    (#.icmp-redirect
     (format t "~&code: ~20t~D" (icmp-code icmp))
     (format t "~&Gateway:~20t~A" (canonical-ip (icmp-gateway icmp)))
     (format t "~&In response to IP header:")
     (describe-ih ip-header 8))
    (#.icmp-time-exceeded
     (format t "~&code: ~20t~D" (icmp-code icmp))
     (format t "~&In response to IP header:")
     (describe-ih ip-header 8))
    (#.icmp-parameter-problem
     (format t "~&In response to byte ~D of IP header:" (icmp-parameter-problem-pointer icmp))
     (describe-ih ip-header 8)))
  icmp)

;;;ICMP message sending -- interface to IP

(defun icmp (message buffers &rest args)
  "Send an ICMP message if the icmp stream is operational"
  (if (and *icmp-stream*                                ;first, make sure icmp is operational
           (ip:tp-enabled *icmp-stream*)
           (member message *icmp-message-list*))
      ;;ICMP is up.  See if this message is in response to a received IP message
      (if (member message *icmp-int-pkt-messages*)
          (let* ((packet-list (if (consp buffers) buffers (ncons buffers)))
                 (header (first packet-list))
                 (address (ip:ih-dest-address header)))
            ;;It is.  Send ICMP message only if first fragment & not for ICMP & not broadcast packet
            (when (and (zerop (ip:ih-fragment-offset header))
                       (/= (ip:ih-protocol header) icmp-protocol)
                       (not (ip:ip-broadcast-address-p address))
                       (not (ip:ip-self-address-p address)))
              ;;OK to send -- call ICMP
              (apply *icmp-stream* message packet-list args)))
        ;;Message not in response to received IP packet
        (apply *icmp-stream* message args))))

(defop (icmp-ip-transport-protocol :destination-unreachable) (buffers code)
  (when (icmp-send-int-pkt-message icmp-destination-unreachable buffers code)
    (incf (icmp-destination-unreachable-sent *icmp-stream*))))

(defop (icmp-ip-transport-protocol :source-quench) (buffers)
  (when (icmp-send-int-pkt-message icmp-source-quench buffers 0)
    (incf (icmp-source-quench-sent *icmp-stream*))))

(defop (icmp-ip-transport-protocol :redirect) (buffers code gateway)
  (when (icmp-send-int-pkt-message icmp-redirect buffers code gateway)
    (incf (icmp-redirect-sent *icmp-stream*))))

(defop (icmp-ip-transport-protocol :time-exceeded) (buffers code)
  (when (icmp-send-int-pkt-message icmp-time-exceeded buffers code)
    (incf (icmp-time-exceeded-sent *icmp-stream*))))

(defop (icmp-ip-transport-protocol :parameter-problem) (buffers pointer)
  (when (icmp-send-int-pkt-message icmp-parameter-problem buffers 0 (dpb pointer (byte 8 24) 0))
    (incf (icmp-parameter-problem-sent *icmp-stream*))))

(defun icmp-send-int-pkt-message (type buffers code &optional (4-bytes 0) &aux packet)
  (when (setq packet (allocate-packet nil))
    (let ((message (get-icmp-message))          ;icmp message we are building
          header)                               ;IP header to send it with
      (setf (icmp-type message) type)
      (setf (icmp-code message) code)
      (setf (icmp-byte-1 message) (ldb (byte 8 24) 4-bytes))
      (setf (icmp-byte-2 message) (ldb (byte 8 16) 4-bytes))
      (setf (icmp-byte-3 message) (ldb (byte 8 8) 4-bytes))
      (setf (icmp-byte-4 message) (ldb (byte 8 0) 4-bytes))
      (unless (consp buffers)
        (setq buffers (ncons buffers)))
      (let ((count (+ 8 (ip:ih-ihl-bytes (first buffers)))))    ;total bytes to copy
        (setf (fill-pointer message) (+ 8 count))
        (do ((b buffers (cdr b))                ;buffers remaining to look at
             (doff 8 (+ doff length))           ;offset in ICMP message
             (length 0))                        ;length to copy from current buffer
            ((or (null b) (zerop count)))
          (setq length (min count (length (car b))))
          (copy-array-portion (car b) 0 length message doff (+ doff length))
          (decf count length)))
      (let ((destination (ip:ih-source-address (first buffers))))
        (setq header (make-ip-header :destination destination))
        (store-icmp-checksum message (local-host-p destination)))
      (multiple-value-prog1
        (send *icmp-stream* :send message header packet)
        (free-icmp-message message)
        (free-ip-header header)))))

(defun store-icmp-checksum (buffers local-p)
  (unless (consp buffers)
    (setq buffers (ncons buffers)))
  (setf (icmp-checksum (first buffers)) 0)
  (when (and (not (icmp-local-checksums *icmp-stream*)) local-p)
    (return-from store-icmp-checksum t))
  (setf (icmp-checksum (first buffers)) (checksum buffers 0)))

;;;ICMP message receiving

(defun check-icmp-checksum (icmp local-p)
  (when (and (not (icmp-local-checksums *icmp-stream*)) local-p)
    (return-from check-icmp-checksum t))
  (let ((sum (checksum-1 icmp 0 (length icmp))))
    (values (zerop sum) sum)))

(defun pass-up-icmp-message (buffer)
  "Given Destination Unreachable, Time Exceeded, or Source Quench ICMP message,
inform the protocol that originated the problem packet"
  (let* ((header (make-array 60
                             :element-type '(unsigned-byte 8)
                             :displaced-to buffer
                             :displaced-index-offset 8))
         (stream (cdr (assoc (ip:ih-protocol header) (ip:ip-protocols *ip-stream*)))))
    (when (and stream (ip:tp-icmp-notification-function stream))
      (let* ((offset (+ 8 (ip:ih-ihl-bytes header)))
             (data (make-array 8 :element-type '(unsigned-byte 8))))
        (copy-array-portion buffer offset (+ offset 8) data 0 8)
        (funcall (ip:tp-icmp-notification-function stream)
                 (ecase (icmp-type buffer)
                   (#.icmp-destination-unreachable
                    (case (icmp-code buffer)
                      (#.icmp-network-unreachable :network-unreachable)
                      (#.icmp-host-unreachable :host-unreachable)
                      (#.icmp-protocol-unreachable :protocol-unreachable)
                      (#.icmp-port-unreachable :port-unreachable)
                      (#.icmp-couldnt-fragment :couldnt-fragment)
                      (#.icmp-source-route-failed :source-route-failed)
                      (otherwise :unknown-unreachable)))
                   (#.icmp-source-quench :source-quench)
                   (#.icmp-time-exceeded
                    (case (icmp-code buffer)
                      (#.icmp-ttl-exceeded :time-to-live-expired)
                      (#.icmp-fragment-timeout :fragment-reassembly-timeout)
                      (otherwise :unknown-time-exceeded))))
                 (copy-ip-header header (get-ip-header) t)
                 data)))))

;;;User interface to ICMP

(defstream icmp-socket
           ()
           "ICMP-USER-"
  (sequence 0)                                  ;sequence number for next request
  (request-list nil)                            ;outstanding requests
  (reply-list nil)                              ;pending replies
  (echo-sent 0)                                 ;echo requests sent
  (echo-received 0)                             ;echo replies received
  (info-sent 0)                                 ;info requests sent
  (info-received 0)                             ;info replies received
  (timestamp-sent 0)                            ;timestamp requests sent
  (timestamp-received 0)                        ;timestamp replies received
  (address-mask-sent 0)                         ;address-mask requests sent
  (address-mask-received 0)                     ;address-mask replies received
  )

(defop (icmp-socket :peek-item) (elt)
  (let ((socket (cdr elt)))
    (list nil
          (tv:scroll-parse-item
            `(:MOUSE-ITEM
               (NIL :MENU-CHOOSE
                    ("ICMP socket operations"
                     ("Close" :EVAL  (when (tv:mouse-y-or-n-p "Close this ICMP socket")
                                       (funcall socket :close))
                      :DOCUMENTATION
                      "Click left to close this ICMP socket.")
                     ("Inspect" :EVAL (send tv:selected-window :force-kbd-input `(inspect ,socket))
                      :DOCUMENTATION
                      "Click left to INSPECT this ICMP socket.")
                     ("Describe" :EVAL (send tv:selected-window :force-kbd-input `(describe ,socket))
                      :DOCUMENTATION
                      "Click left to DESCRIBE this ICMP socket."))
                    :DOCUMENTATION
                    "Menu of things to do to this ICMP socket."
                    :BINDINGS
                    ((socket ',socket)))
               :FUNCTION car (,elt) NIL ("   ICMP socket ~D:"))
            `(:function icmp-user-sequence (,socket) NIL (" Next sequence = ~D"))
            `(:function icmp-user-echo-sent (,socket) NIL ("  Echo S/R: ~D"))
            `(:function icmp-user-echo-received (,socket) NIL ("/~D"))
            `(:function icmp-user-info-sent (,socket) NIL (" Info S/R: ~D"))
            `(:function icmp-user-info-received (,socket) NIL ("/~D"))
            `(:function icmp-user-timestamp-sent (,socket) NIL (" Timestamp S/R: ~D"))
            `(:function icmp-user-timestamp-received (,socket) NIL ("/~D"))
            `(:function icmp-user-address-mask-sent (,socket) NIL (" Mask S/R: ~D"))
            `(:function icmp-user-address-mask-received (,socket) NIL ("/~D"))))))

(defop (icmp-socket :open) ()
  (unless (rassoc self (icmp-user-socket-alist *icmp-stream*))
    (when (icmp-enabled *icmp-stream*)
      (do ((id (icmp-next-identifier *icmp-stream*) (1+ id)))
          ((null (assoc id (icmp-user-socket-alist *icmp-stream*)))
           (push (cons id self) (icmp-user-socket-alist *icmp-stream*))
           (setf (icmp-next-identifier *icmp-stream*) (1+ id))
           t)))))

(defop (icmp-socket :close) ()
  (let ((item (rassoc self (icmp-user-socket-alist *icmp-stream*))))
    (when item
      (setf (icmp-user-socket-alist *icmp-stream*) (remove item (icmp-user-socket-alist *icmp-stream*)))
      t)))

(defop (icmp-socket :print-self) (stream &optional ignore ignore)
  (si:printing-random-object (self stream :type :no-pointer)
    (format stream
            "~:[closed~;open~]"
            (and *icmp-stream*
                (rassoc self (icmp-user-socket-alist *icmp-stream*))))))

(defop (icmp-socket :echo) (data sequence-number &rest header-args)
  (icmp-user-message self :echo data sequence-number header-args))

(defop (icmp-socket :info) (ignore sequence-number &rest header-args)
  (icmp-user-message self :info nil sequence-number header-args))

(defop (icmp-socket :timestamp) (ignore sequence-number &rest header-args)
  (icmp-user-message self :timestamp nil sequence-number header-args))

(defop (icmp-socket :address-mask) (network sequence-number &rest header-args)
  (icmp-user-message self :address-mask network sequence-number header-args))

(defun icmp-user-message (socket operation data sequence header-args)
  (let ((header (apply 'make-ip-header header-args))
        (message (get-icmp-message)))
    (unwind-protect
        (let ((buffers (ncons message))
              (result nil))
          (unless sequence
            (setq sequence (icmp-user-sequence socket))
            (incf (icmp-user-sequence socket)))
          (setf (icmp-code message) 0)
          (setf (icmp-identifier message) (car (rassoc socket (icmp-user-socket-alist *icmp-stream*))))
          (setf (icmp-sequence message) sequence)
          (case operation
            (:echo
             (setf (icmp-type message) icmp-echo)
             (setf (fill-pointer message) 8)
             (check-type data (satisfies byte-array-or-string-p))
             (setq buffers (nconc buffers (ncons data))))
            (:info
             (setf (icmp-type message) icmp-information-request)
             (setf (fill-pointer message) 8))
            (:timestamp
             (setf (icmp-type message) icmp-timestamp)
             (setf (icmp-originate-timestamp message) (milliseconds-since-midnight-gmt))
             (setf (icmp-receive-timestamp message) 0)
             (setf (icmp-transmit-timestamp message) 0)
             (setf (fill-pointer message) 20))
            (:address-mask
             (setf (icmp-type message) icmp-address-mask-request)
             (setf (icmp-address-mask message) (or data 0))
             (setf (fill-pointer message) 16)))
          (store-icmp-checksum buffers (local-host-p (ip::ih-dest-address header)))
          (push (cons sequence nil) (icmp-user-request-list socket))
          (if (eq operation :address-mask)
              (setq result (send *icmp-stream* :broadcast buffers header data))
            (setq result (send *icmp-stream* :send buffers header)))
          (cond ((or (null result) (zerop result))
                 ;;Couldn't route or send on interface
                 (delete-from-alist sequence (icmp-user-request-list socket))
                 nil)
                (t
                 (case operation
                   (:echo
                    (incf (icmp-user-echo-sent socket))
                    (incf (icmp-echo-sent *icmp-stream*)))
                   (:info
                    (incf (icmp-user-info-sent socket))
                    (incf (icmp-info-sent *icmp-stream*)))
                   (:timestamp
                    (incf (icmp-user-timestamp-sent socket))
                    (incf (icmp-timestamp-sent *icmp-stream*)))
                   (:address-mask
                    (incf (icmp-user-address-mask-sent socket))
                    (incf (icmp-address-mask-sent *icmp-stream*))))
                 sequence)))
      (free-ip-header header)
      (free-icmp-message message))))

(defmacro with-icmp-socket ((socket) &body body)
  (let ((s (gensym)))
    `(let ((,s (make-icmp-socket)))
       (unwind-protect
           (let ((,socket ,s))
             (when (send ,s :open)
               ,@body))
         (send ,s :close)))))

(defun ping (host &optional (operation :echo) (data nil data-p) &rest ip-header-args)
  "Do an ICMP Echo, Timestamp, Information Request, or Address Mask"
  (let ((original-host host))
    (assert (numberp (setq host (parse-internet-address (setq original-host host))))
            (host)
            "~S is not a valid Internet host specification"
            original-host))
  (check-type operation (member :echo :info :address-mask :timestamp))
  (unless data-p
    (case operation
      (:echo                                    ;Default data for :echo is a string...
       (setq data "Ping"))
      (:address-mask
       ;;Default data is remote-address -- which implies a specific network interface
       (setq data host))))
  (with-icmp-socket (s)
    (let ((start (zl:time))
          (sequence (apply s operation data nil :destination host ip-header-args)))
      (when sequence
        (process-wait-with-timeout "Ping Reply" 300
                                   #'(lambda (socket sequence)
                                       (assoc sequence (icmp-user-reply-list socket)))
                                   s sequence)
        (let ((end (zl:time))
              (result (assoc sequence (icmp-user-reply-list s))))
          (when result
            (values (time-difference end start) (cddr result))))))))

;;;Top level ICMP packet reception

(defun receive-icmp-packet (stream buffer header local-p broadcast-p interface)
  (declare (ignore broadcast-p))
  (unwind-protect
      (if (check-icmp-checksum buffer local-p)
          (case (icmp-type buffer)
            (#.icmp-echo
             (receive-icmp-echo stream buffer header local-p))
            (#.icmp-information-request
             (receive-icmp-info stream buffer header local-p))
            (#.icmp-timestamp
             (receive-icmp-timestamp stream buffer header local-p))
            (#.icmp-address-mask-request
             (receive-icmp-address-mask stream buffer header local-p interface))
            (#.icmp-echo-reply
             (receive-icmp-echo-reply stream buffer))
            (#.icmp-information-reply
             (receive-icmp-info-reply stream buffer))
            (#.icmp-timestamp-reply
             (receive-icmp-timestamp-reply stream buffer))
            (#.icmp-address-mask-reply
             (receive-icmp-address-mask-reply stream buffer))
            (#.icmp-redirect
             (receive-icmp-redirect stream buffer))
            (#.icmp-destination-unreachable
             (receive-icmp-unreachable stream buffer))
            (#.icmp-source-quench
             (receive-icmp-source-quench stream buffer))
            (#.icmp-time-exceeded
             (receive-icmp-time-exceeded stream buffer))
            (#.icmp-parameter-problem
             (receive-icmp-parameter-problem stream buffer)))
        (incf (icmp-checksum-failures stream)))
    (free-ip-header header)
    (send stream :receive buffer)))

(defun receive-icmp-echo (stream buffer header local-p)
  (incf (icmp-echo-received stream))
  (reverse-route header)
  (setf (icmp-type buffer) icmp-echo-reply)
  (store-icmp-checksum buffer local-p)
  (let ((result (send stream :send buffer header)))
    (and result
         (plusp result)
         (incf (icmp-echo-reply-sent stream)))))

(defun receive-icmp-info (stream buffer header local-p)
  (incf (icmp-info-received stream))
  (reverse-route header)
  (setf (icmp-type buffer) icmp-information-reply)
  (store-icmp-checksum buffer local-p)
  (let ((result (send stream :send buffer header)))
    (and result
         (plusp result)
         (incf (icmp-info-reply-sent stream)))))

(defun receive-icmp-timestamp (stream buffer header local-p)
  (setf (icmp-receive-timestamp buffer) (milliseconds-since-midnight-gmt))
  (incf (icmp-timestamp-received stream))
  (reverse-route header)
  (setf (icmp-type buffer) icmp-timestamp-reply)
  (setf (icmp-transmit-timestamp buffer) (milliseconds-since-midnight-gmt))
  (store-icmp-checksum buffer local-p)
  (let ((result (send stream :send buffer header)))
    (and result
         (plusp result)
         (incf (icmp-timestamp-reply-sent stream)))))

(defun receive-icmp-address-mask (stream buffer header local-p interface)
  (incf (icmp-address-mask-received stream))
  (when (ip:ip-gateway *ip-stream*)             ;Only a gateway responds to Address Mask request
    (reverse-route header)
    (setf (icmp-type buffer) icmp-address-mask-reply)
    (setf (ip:ih-source-address header) (second (assoc :internet (net:ni-address-alist interface))))
    (setf (icmp-address-mask buffer) (third (assoc :internet (net:ni-network-alist interface))))
    (store-icmp-checksum buffer local-p)
    (let ((result (if (zerop (ip:ih-dest-address header))
                      (send stream :broadcast buffer header (ip:ih-source-address header))
                    (send stream :send buffer header))))
      (and result
           (plusp result)
           (incf (icmp-address-mask-reply-sent stream))))))

(defun receive-icmp-echo-reply (stream buffer)
  (incf (icmp-echo-reply-received stream))
  (let ((user (assoc (icmp-identifier buffer) (icmp-user-socket-alist stream)))
        (data-length (- (length buffer) 8))
        (string nil))
    (when (and user (assoc (icmp-sequence buffer) (icmp-user-request-list (cdr user))))
      (delete-from-alist (icmp-sequence buffer) (icmp-user-request-list (cdr user)))
      (copy-array-portion buffer
                          8
                          (length buffer)
                          (setq string (make-string data-length))
                          0
                          data-length)
      (push (list (icmp-sequence buffer)
                  (zl:time)
                  string)
            (icmp-user-reply-list (cdr user)))
      (incf (icmp-user-echo-received (cdr user))))))

(defun receive-icmp-info-reply (stream buffer)
  (incf (icmp-info-reply-received stream))
  (let ((user (assoc (icmp-identifier buffer) (icmp-user-socket-alist stream))))
    (when (and user (assoc (icmp-sequence buffer) (icmp-user-request-list (cdr user))))
      (delete-from-alist (icmp-sequence buffer) (icmp-user-request-list (cdr user)))
      (push (list (icmp-sequence buffer) (zl:time))
            (icmp-user-reply-list (cdr user)))
      (incf (icmp-user-info-received (cdr user))))))

(defun receive-icmp-timestamp-reply (stream buffer)
  (incf (icmp-timestamp-reply-received stream))
  (let ((user (assoc (icmp-identifier buffer) (icmp-user-socket-alist stream))))
    (when (and user (assoc (icmp-sequence buffer) (icmp-user-request-list (cdr user))))
      (delete-from-alist (icmp-sequence buffer) (icmp-user-request-list (cdr user)))
      (push (list (icmp-sequence buffer)
                  (zl:time)
                  (icmp-originate-timestamp buffer)
                  (icmp-receive-timestamp buffer)
                  (icmp-transmit-timestamp buffer))
            (icmp-user-reply-list (cdr user)))
      (incf (icmp-user-timestamp-received (cdr user))))))

(defun receive-icmp-address-mask-reply (stream buffer)
  (incf (icmp-address-mask-reply-received stream))
  (let ((user (assoc (icmp-identifier buffer) (icmp-user-socket-alist stream))))
    (when (and user (assoc (icmp-sequence buffer) (icmp-user-request-list (cdr user))))
      (delete-from-alist (icmp-sequence buffer) (icmp-user-request-list (cdr user)))
      (push (list (icmp-sequence buffer)
                  (zl:time)
                  (icmp-address-mask buffer))
            (icmp-user-reply-list (cdr user)))
      (incf (icmp-user-address-mask-received (cdr user))))))

(defun receive-icmp-redirect (stream buffer)
  (incf (icmp-redirect-received stream))
  (let* ((header (make-array 60
                             :element-type '(unsigned-byte 8)
                             :displaced-to buffer
                             :displaced-index-offset 8))
         (address (ip:ih-dest-address header))
         (gateway (icmp-gateway buffer))
         network tos)
    (case (icmp-code buffer)
      (#.icmp-redirect-network
       (setq network (ip:ip-network-number-from-address address)))
      (#.icmp-redirect-host
       (setq network address))
      (#.icmp-redirect-network-and-tos
       (setq network (ip:ip-network-number-from-address address))
       (setq tos (ip:ih-tos header)))
      (#.icmp-redirect-host-and-tos
       (setq network address)
       (setq tos (ip:ih-tos header))))
    (when network                               ;Valid code...
      (multiple-value-bind (found interface ignore)
          (ip:route gateway)
        (when (eql found gateway)               ;And we know how to get to the gateway directly
          (ip:add-gateway network gateway interface tos *icmp-redirect-expiration*))))))

(defun receive-icmp-unreachable (stream buffer)
  (incf (icmp-destination-unreachable-received stream))
  ;;****change routing table?
  (pass-up-icmp-message buffer))

(defun receive-icmp-source-quench (stream buffer)
  (incf (icmp-source-quench-received stream))
  (pass-up-icmp-message buffer))

(defun receive-icmp-time-exceeded (stream buffer)
  (incf (icmp-time-exceeded-received stream))
  (pass-up-icmp-message buffer))

(defun receive-icmp-parameter-problem (stream buffer)
  (declare (ignore buffer))
  (incf (icmp-parameter-problem-received stream))
  ;;****Log this somehow -- bug in our IP??!!
  )

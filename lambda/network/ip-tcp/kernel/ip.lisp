;;; -*- Mode:LISP; Package:INTERNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *ip-stream*
           ip-transport-protocol
           make-ip-header
           copy-ip-header
           reverse-route
           set-destination-address
           set-precedence
           get-ip-header
           free-ip-header
           ip-header-p
           parse-internet-address
           canonical-ip
           describe-ih
           mss
           local-host-p
           find-security-and-precedence
           checksum
           checksum-1
           pseudo-header-checksum
           setup-ip))

;;; This is the network-protocol structure for IP -- the interface between IP
;;; and the link-level network interfaces

(defstream ip-network-protocol
           (network-protocol)
           "IP-"
  (gateway nil)                                 ; t if act as IP gateway and forward packets
                                                ; NOTE: will always act as gateway for share device
  (checksum-failures 0)                         ; packets received with bad checksum
  (bad-header 0)                                ; packets dropped because of bad header
  (bad-options 0)                               ; packets dropped because of bad options
  (ttl-expired 0)                               ; packets dropped because ttl expired
  (unknown-protocol-packets 0)                  ; packets dropped due to unregistered transport protocol
  (fragment-timeouts 0)                         ; packets dropped because fragments timed-out
  (self-address-packets 0)                      ; packets received with the IP "self" address
  (local-checksums nil)                         ; T if calculate checksums for looped-back packets
  (hardware-broadcast-packets 0)                ; packets received on a hardware broadcast address
  (ip-broadcast-packets 0)                      ; packets received with IP broadcast address
  (not-for-me 0)                                ; (if gateway nil) packets dropped: not for this address
  (no-forwarding-address 0)                     ; packets dropped because no link-level address translation
  (cant-forward 0)                              ; packets dropped because too many already queued
  (packets-forwarded 0)                         ; packets successfully forwarded
  (default-address 0)                           ; default IP address
  (packets-queued 0)                            ; packets queued for forwarding by the background process
  )

(defop (ip-network-protocol :peek-special-fields) (np)
  (list (tv:scroll-parse-item
          `(:function ip-checksum-failures (,np) NIL ("Bad checksum ~D"))
          `(:function ip-bad-header (,np) NIL (" bad header ~D"))
          `(:function ip-bad-options (,np) NIL (" bad options ~D"))
          `(:function ip-unknown-protocol-packets (,np) NIL (" bad protocol ~D"))
          `(:function ip-fragment-timeouts (,np) NIL (" frag timeouts ~D")))
        (tv:scroll-parse-item
          `(:function ip-hardware-broadcast-packets (,np) NIL ("Hardware Broadcast packets = ~D"))
          `(:function ip-ip-broadcast-packets (,np) NIL ("  IP Broadcast packets ~D"))
          `(:function ip-self-address-packets (,np) NIL ("  Self Address packets ~D"))
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (setf (ip-local-checksums np)
                                                (not (ip-local-checksums np)))
                                    :bindings ((np ',np))))
                  :DOCUMENTATION
                  "Click to toggle local checksums"
                  :BINDINGS
                  ((np ',np)))
             :function ip-local-checksums (,np) NIL (" Local checksums: ~A")))
        (tv:scroll-parse-item
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (setf (ip-gateway np) (not (ip-gateway np)))
                                    :bindings ((np ',np))))
                  :DOCUMENTATION
                  "Click to toggle IP gateway functions"
                  :BINDINGS
                  ((np ',np)))
             :function ip-gateway (,np) NIL ("IP gateway: ~A"))
          `(:function ip-packets-forwarded (,np) NIL (" forwarded ~D"))
          `(:function ip-packets-queued (,np) NIL (" queued ~D"))
          `(:function ip-not-for-me (,np) NIL (" not for me ~D"))
          `(:function ip-no-forwarding-address (,np) NIL (" no address ~D"))
          `(:function ip-cant-forward (,np) NIL (" queue full ~D"))
          `(:function ip-ttl-expired (,np) NIL (" TTL expired ~D")))
        (TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () (ip-protocols np))
                                 'net:peek-transport-protocol)))

(defvar *ip-stream* nil "ip-network-protocol for DoD IP")

;;; This is the transport-protocol structure for protocols above IP
(defstream ip-transport-protocol
           (transport-protocol
             (network-protocol *ip-stream*))
           "TP-"
  (next-identification 0)                       ; Next datagram identification
  (fragment-list nil)                           ; list of receive buffers being used for fragment reassembly
  (icmp-notification-function nil)              ; function to receive ICMP notifications:
                                                ; (:unreachable :source-quench :time-exceeded)
  (broadcast-allowed-p nil)                     ; T if broadcast packets allowed
  )

;;; Stuff to deal with IP headers and options.

(defvar *free-ip-headers* nil "List of free IP header arrays")

(defun get-ip-header ()
  "returns an array to hold an IP header."
  (do ((header nil))
      (header header)
    (if *free-ip-headers*
        (without-interrupts
          (setq header (pop *free-ip-headers*)))
      (setq header (zl:make-array 60
                                  :element-type '(unsigned-byte 8)
                                  :fill-pointer 0
                                  :leader-length 2
                                  :named-structure-symbol 'ip-header)))))

(defun free-ip-header (ih)
  (without-interrupts
    (push ih *free-ip-headers*))
  nil)

(defun ip-header-p (ih)
  (eq (named-structure-p ih) 'ip-header))

;;; IP HEADER FIELDS

(defconstant ip-version-number 4)               ; This version of DoD IP
(defmacro ih-version (ip)
  "IP version number"
  `(ldb (byte 4 4) (aref ,ip 0)))

(defmacro ih-ihl (ip)
  "IP header length in 32 bit units"
  `(ldb (byte 4 0) (aref ,ip 0)))
(defmacro ih-ihl-words (ip)
  "IP header length in 16 bit units"
  `(* 2 (ldb (byte 4 0) (aref ,ip 0))))
(defmacro ih-ihl-bytes (ip)
  "IP header length in 8 bit units"
  `(* 4 (ldb (byte 4 0) (aref ,ip 0))))

(defmacro ih-tos (ip)
  "Precedence and type of service"
  `(aref ,ip 1))

(defmacro ih-precedence (ip)
  "Precedence"
  `(ldb (byte 3 5) (aref ,ip 1)))

(eval-when (load compile eval)
  (defvar *ih-precedence-alist* nil "Valid Internet header precedence values"))

(defmacro define-ih-precedence (name value)
  `(eval-when (load compile eval)
     (unless (assoc ,name *ih-precedence-alist*)
       (push (cons ,name ,value) *ih-precedence-alist*))))

(define-ih-precedence :network-control 7)
(define-ih-precedence :internetwork-control 6)
(define-ih-precedence :critic/ecp 5)
(define-ih-precedence :flash-override 4)
(define-ih-precedence :flash 3)
(define-ih-precedence :immediate 2)
(define-ih-precedence :priority 1)
(define-ih-precedence :routine 0)

(defmacro ih-delay (ip)
  "Delay"
  `(ldb (byte 1 4) (aref ,ip 1)))

(eval-when (load compile eval)
  (defvar *ih-delay-alist* nil "Valid Internet header delay values"))

(defmacro define-ih-delay (name value)
  `(eval-when (load compile eval)
     (unless (assoc ,name *ih-delay-alist*)
       (push (cons ,name ,value) *ih-delay-alist*))))

(define-ih-delay :low 1)
(define-ih-delay :normal 0)

(defmacro ih-throughput (ip)
  "Throughput"
  `(ldb (byte 1 3) (aref ,ip 1)))

(eval-when (load compile eval)
  (defvar *ih-throughput-alist* nil "Valid Internet header throughput values"))

(defmacro define-ih-throughput (name value)
  `(eval-when (load compile eval)
     (unless (assoc ,name *ih-throughput-alist*)
       (push (cons ,name ,value) *ih-throughput-alist*))))

(define-ih-throughput :high 1)
(define-ih-throughput :normal 0)

(defmacro ih-reliability (ip)
  "Reliability"
  `(ldb (byte 1 2) (aref ,ip 1)))

(eval-when (load compile eval)
  (defvar *ih-reliability-alist* nil "Valid Internet header reliability values"))

(defmacro define-ih-reliability (name value)
  `(eval-when (load compile eval)
     (unless (assoc ,name *ih-reliability-alist*)
       (push (cons ,name ,value) *ih-reliability-alist*))))

(define-ih-reliability :high 1)
(define-ih-reliability :normal 0)

(defsubst ih-length (ip)
  "Total packet length in bytes"
  (dpb (aref ip 2) (byte 8 8) (aref ip 3)))
(defun set-ih-length (ip val)
  (setf (aref ip 2) (ldb (byte 8 8) val))
  (setf (aref ip 3) (ldb (byte 8 0) val))
  val)
(defsetf ih-length set-ih-length)

(defsubst ih-identification (ip)
  (dpb (aref ip 4) (byte 8 8) (aref ip 5)))
(defun set-ih-identification (ip val)
  (setf (aref ip 4) (ldb (byte 8 8) val))
  (setf (aref ip 5) (ldb (byte 8 0) val))
  val)
(defsetf ih-identification set-ih-identification)

(defmacro ih-flags (ip)
  `(ldb (byte 3 5) (aref ,ip 6)))
(defconstant mf-flag 1)
(defconstant df-flag 2)

(defmacro ih-flags-mf (ip)
  `(ldb (byte 1 5) (aref ,ip 6)))
(defmacro ih-flags-df (ip)
  `(ldb (byte 1 6) (aref ,ip 6)))

(defmacro ih-fragment-offset (ip)
  `(dpb (ldb (byte 5 0) (aref ,ip 6)) (byte 5 8) (aref ,ip 7)))
(defun set-ih-fragment-offset (ip val)
  (setf (ldb (byte 5 0) (aref ip 6)) (ldb (byte 5 0) val))
  (setf (aref ip 7) (ldb (byte 8 0) val)))
(defsetf ih-fragment-offset set-ih-fragment-offset)

(defmacro ih-ttl (ip)
  `(aref ,ip 8))

(defmacro ih-protocol (ip)
  `(aref ,ip 9))

(defmacro ih-checksum (ip)
  `(dpb (aref ,ip 10) (byte 8 8) (aref ,ip 11)))
(defun set-ih-checksum (ip val)
  (setf (aref ip 10) (ldb (byte 8 8) val))
  (setf (aref ip 11) (ldb (byte 8 0) val)))
(defsetf ih-checksum set-ih-checksum)

(defsubst ih-source-address (ip)
  (dpb (aref ip 12) (byte 8 24.)
       (dpb (aref ip 13) (byte 8 16.)
            (dpb (aref ip 14) (byte 8 8)
                 (aref ip 15)))))
(defun set-ih-source-address (ip val)
  (setf (aref ip 12) (ldb (byte 8 24) val))
  (setf (aref ip 13) (ldb (byte 8 16) val))
  (setf (aref ip 14) (ldb (byte 8 8) val))
  (setf (aref ip 15) (ldb (byte 8 0) val))
  val)
(defsetf ih-source-address set-ih-source-address)

(defsubst ih-dest-address (ip)
  (dpb (aref ip 16) (byte 8 24.)
       (dpb (aref ip 17) (byte 8 16.)
            (dpb (aref ip 18) (byte 8 8)
                 (aref ip 19)))))
(defun set-ih-dest-address (ip val)
  (setf (aref ip 16) (ldb (byte 8 24) val))
  (setf (aref ip 17) (ldb (byte 8 16) val))
  (setf (aref ip 18) (ldb (byte 8 8) val))
  (setf (aref ip 19) (ldb (byte 8 0) val))
  val)
(defsetf ih-dest-address set-ih-dest-address)

(defselect ((:property ip-header named-structure-invoke))
  (:describe (header)
    (describe-ih header))
  (:print-self (header stream ignore ignore)
    (si:printing-random-object
      (header stream :type :no-pointer)
      (format stream "(~D) ~A -> ~A"
              (ih-protocol header)
              (canonical-ip (ih-source-address header))
              (canonical-ip (ih-dest-address header))))))

;;;  IP header option definitions

(defconstant ih-option-offset 20)               ;byte-offset of start of options
(defconstant max-option-length 40)              ;maximum length of options in bytes

(defmacro define-ih-option (name copy class number)
  (let ((nn (intern (string-append "IH-OPT-" name)))
        (copy-number (case copy
                       (copied #o200)
                       (not-copied 0)))
        (class-number (case class
                        (control 0)
                        (debug #o100))))
    `(eval-when (load compile eval)
       (defconstant ,nn (+ ,copy-number ,class-number ,number)))))

(define-ih-option end not-copied control 0)
(define-ih-option nop not-copied control 1)
(define-ih-option sec copied control 2)
(define-ih-option lsrr copied control 3)
(define-ih-option ssrr copied control 9)
(define-ih-option rr not-copied control 7)
(define-ih-option stream copied control 8)
(define-ih-option ts not-copied debug 4)

(eval-when (load compile eval)
  (defvar *ih-security-alist* nil "Valid IP Security option security types"))

(defmacro define-ih-security-type (name value)
  `(eval-when (load compile eval)
     (unless (assoc ,name *ih-security-alist*)
       (push (cons ,name ,value) *ih-security-alist*))))

(define-ih-security-type :unclassified #b0000000000000000)
(define-ih-security-type :confidential #b1111000100110101)
(define-ih-security-type :efto         #b0111100010011010)
(define-ih-security-type :mmmm         #b1011110001001101)
(define-ih-security-type :prog         #b0101111000100110)
(define-ih-security-type :restricted   #b1010111100010011)
(define-ih-security-type :secret       #b1101011110001000)
(define-ih-security-type :top-secret   #b0110101111000101)
(define-ih-security-type :reserved-1   #b0011010111100010)
(define-ih-security-type :reserved-2   #b1001101011110001)
(define-ih-security-type :reserved-3   #b0100110101111000)
(define-ih-security-type :reserved-4   #b0010010010111101)
(define-ih-security-type :reserved-5   #b0001001101011110)
(define-ih-security-type :reserved-6   #b1000100110101111)
(define-ih-security-type :reserved-7   #b1100010011010110)
(define-ih-security-type :reserved-8   #b1110001001101011)

(eval-when (load compile eval)
  (defvar *ih-timestamp-type-alist* nil "Valid Internet header timestamp types"))

(defmacro define-ih-timestamp-type (name value)
  `(eval-when (load compile eval)
     (unless (assoc ,name *ih-timestamp-type-alist*)
       (push (cons ,name ,value) *ih-timestamp-type-alist*))))

(define-ih-timestamp-type :ts-only 0)
(define-ih-timestamp-type :ts-address 1)
(define-ih-timestamp-type :ts-specified-address 3)

(defmacro ih-sub-header-offset (ip)
  `(ih-ihl-bytes ,ip))

;;;ICMP definitions
(defconstant icmp:icmp-protocol 1 "IP protocol number of ICMP")

;;;destination-unreachable codes
(defconstant icmp:icmp-network-unreachable 0)
(defconstant icmp:icmp-host-unreachable 1)
(defconstant icmp:icmp-protocol-unreachable 2)
(defconstant icmp:icmp-port-unreachable 3)
(defconstant icmp:icmp-couldnt-fragment 4)
(defconstant icmp:icmp-source-route-failed 5)

;;;time-exceeded codes
(defconstant icmp:icmp-ttl-exceeded 0)
(defconstant icmp:icmp-fragment-timeout 1)

;;;redirect codes
(defconstant icmp:icmp-redirect-network 0)
(defconstant icmp:icmp-redirect-host 1)
(defconstant icmp:icmp-redirect-network-and-tos 2)
(defconstant icmp:icmp-redirect-host-and-tos 3)

;;; IP header display routines

(defun describe-ih (ih &optional display-data-p)
  "Given an art-8b array, displays the IP header at the head of it"
  (format t "~&version: ~20t~s" (ih-version ih))
  (format t "~&header-length: ~20t~s" (ih-ihl ih))
  (format t "~&tos: ~20t~s" (ih-tos ih))
  (describe-tos (ih-tos ih))
  (format t "~&length: ~20t~s" (ih-length ih))
  (format t "~&identification: ~20t~s" (ih-identification ih))
  (format t "~&flags: ~20t~s" (ih-flags ih))
  (describe-flags (ih-flags ih))
  (format t "~&fragment-offset: ~20t~s" (ih-fragment-offset ih))
  (format t "~&time-to-live: ~20t~s" (ih-ttl ih))
  (format t "~&protocol: ~20t~s" (ih-protocol ih))
  (format t "~&header-checksum: ~20t~s" (ih-checksum ih))
  (format t "~&source-address: ~20t~a ~s" (canonical-ip (ih-source-address ih))
          (si:get-host-from-address (ih-source-address ih) :internet))
  (format t "~&dest-address: ~20t~a ~s" (canonical-ip (ih-dest-address ih))
          (si:get-host-from-address (ih-dest-address ih) :internet))
  (describe-ih-options ih)

  (when (and display-data-p
             (> (ih-length ih) (ih-ihl-bytes ih)))
    (if (numberp display-data-p)
        (format t "~&~D bytes of data:" display-data-p)
      (format t "~&rest of data:"))
    (do* ((offset (ih-sub-header-offset ih) (1+ offset))
          (length (- (ih-length ih) offset))
          (count (if (numberp display-data-p) (min length display-data-p) length))
          (index 0 (1+ index)))
         ((= index count) nil)
      (if (zerop (mod index 32))
          (format t "~&"))
      (format t "~16,2,'0r " (aref ih offset))))

  ih)

(defun describe-tos (tos)
  (format t " prec = ~a delay = ~a throughput = ~a reliability = ~a"
          (car (rassoc (ldb (byte 3 5) tos) *ih-precedence-alist*))
          (car (rassoc (ldb (byte 1 4) tos) *ih-delay-alist*))
          (car (rassoc (ldb (byte 1 3) tos) *ih-throughput-alist*))
          (car (rassoc (ldb (byte 1 2) tos) *ih-reliability-alist*))))

(defun describe-flags (flags)
  (format t "~:[ DF~;~]~:[ MF~;~]"
          (zerop (logand flags df-flag))
          (zerop (logand flags mf-flag))))

(defun describe-ih-options (ih)
  "Describes all the options in an IP header"
  (let ((length (ih-ihl-bytes ih)))
    (when (> length ih-option-offset)
      (format t "~&Options:")
      (do ((offset ih-option-offset))
          ((>= offset length))
        (incf offset (describe-ih-option ih offset))))))

(defun get-two-bytes-from-option (opt offset)
  (+ (dpb (aref opt offset) (byte 8 8) 0)
     (aref opt (incf offset))))

(defun get-three-bytes-from-option (opt offset)
  (+ (dpb (aref opt offset) (byte 8 16) 0)
     (dpb (aref opt (incf offset)) (byte 8 8) 0)
     (aref opt (incf offset))))

(defun get-four-bytes-from-option (opt offset)
  (+ (dpb (aref opt offset) (byte 8 24) 0)
     (dpb (aref opt (incf offset)) (byte 8 16) 0)
     (dpb (aref opt (incf offset)) (byte 8 8) 0)
     (aref opt (incf offset))))

(defun describe-ih-option (opt offset)
  "Describes an IP header option and returns the length of the option"
  (case (aref opt offset)
    (#.ih-opt-end
     (format t " END(1)")
     1)
    (#.ih-opt-nop
     (format t " NOP(1)")
     1)
    (#.ih-opt-sec
     (format t " SEC(11)")
     (describe-ih-security-option opt offset))
    (#.ih-opt-lsrr
     (format t " LSRR")
     (describe-ih-route opt offset))
    (#.ih-opt-ssrr
     (format t " SSRR")
     (describe-ih-route opt offset))
    (#.ih-opt-rr
     (format t " RR")
     (describe-ih-route opt offset))
    (#.ih-opt-stream
     (format t " STREAM(4)~D"
             (get-two-bytes-from-option opt (+ 2 offset)))
     4)
    (#.ih-opt-ts
     (format t " TIME")
     (describe-ih-timestamp opt offset))
    (otherwise
     (format t " UNKNOWN:~D(~D)"
             (aref opt offset)
             (aref opt (1+ offset)))
     (aref opt (1+ offset)))))

(defun describe-ih-security-option (opt offset)
  "Describes an IP Security option and returns its length"
  (format t "SEC=~A,COMP=~16,4,'0r,HAND=~16,4,'0r,TCC=~16,6,'0r"
          (car (rassoc (get-two-bytes-from-option opt (+ 2 offset))
                       *ih-security-alist*))
          (get-two-bytes-from-option opt (+ 4 offset))
          (get-two-bytes-from-option opt (+ 6 offset))
          (get-three-bytes-from-option opt (+ 8 offset)))
  11)

(defun describe-ih-route (opt offset &aux address)
  "Describes an IP routing option and returns its length"
  (let* ((length (aref opt (incf offset)))
         (pointer (aref opt (incf offset)))
         (index (1- (truncate pointer 4))))
    (format t "(~D)" length)
    (incf offset)
    (dotimes (i (truncate (- length 3) 4))
      (setq address (get-four-bytes-from-option opt offset))
      (incf offset 4)
      (format t "~:[~;,~]~:[~;*~]~A"
              (plusp i)
              (= index i)
              (canonical-ip address)))
    (if (> pointer length)
        (format t "*"))
    length))

(defun describe-ih-timestamp (opt offset &aux timestamp address index)
  "Describes an IP Timestamp option and returns its length"
  (let* ((length (aref opt (incf offset)))
         (pointer (aref opt (incf offset)))
         (ovf-and-type (aref opt (incf offset)))
         (overflow (ldb (byte 4 4) ovf-and-type))
         (type (ldb (byte 4 0) ovf-and-type)))
    (format t "(~D,~D)" length overflow)
    (incf offset)
    (case (car (rassoc type *ih-timestamp-type-alist*))
      (:ts-only
       (setq index (truncate (- pointer 5) 4))
       (dotimes (i (truncate (- length 4) 4))
         (setq timestamp (get-four-bytes-from-option opt offset))
         (incf offset 4)
         (format t "~:[~;,~]~:[~;*~]~D"
                 (plusp i)
                 (= index i)
                 timestamp))
       (if (> pointer length)
           (format t "*")))
      ((:ts-address :ts-specified-address)
       (setq index (truncate (- pointer 5) 8))
       (dotimes (i (truncate (- length 4) 8))
         (setq address (get-four-bytes-from-option opt offset))
         (incf offset 4)
         (setq timestamp (get-four-bytes-from-option opt offset))
         (incf offset 4)
         (format t "~:[~;,~]~:[~;*~]~A:~D"
                 (plusp i)
                 (= index i)
                 (canonical-ip address)
                 timestamp))
       (if (> pointer length)
           (format t "*"))))
    length))

;;;Checksum routines: used by IP, ICMP, UDP, and TCP

(defsubst build-checksum-from-bytes (low-sum high-sum)
  "%ip-checksum and pseudo-header-checksum both accumulate low-sum and high-sum separately.
This routine adds the low-sum carries into the high-sum and the high-sum carries into the low-sum
and returns the 16 bit result.  The miracles of ones-complement..."
  (incf high-sum (ash low-sum -8))
  (do ((sum (dpb high-sum (byte 24 8) low-sum)))
      ((<= sum #xffff) sum)
    (setq sum (+ (ldb (byte 16 0) sum)
                 (ldb (byte 16 16) sum)))))

(defun %ip-checksum (array sum count odd-flag)
  "The primitive checksum function."
  (check-type array (satisfies byte-array-or-string-p))
  (let ((high-sum (logand #xff (ash sum -8)))
        (low-sum (logand #xff sum))
        (offset 0))
    (unless (zerop count)
      (tagbody
          (when odd-flag
            (go get-low-byte))
       top
          (incf high-sum (aref array offset))
          (incf offset)
       get-low-byte
          (unless (= offset count)
            (incf low-sum (aref array offset))
            (incf offset)
            (unless (= offset count)
              (go top)))))
    (build-checksum-from-bytes low-sum high-sum)))

(defun checksum (buffers sum)
  "Calculate the checksum used by IP, ICMP, UDP, and TCP:
The ones-complement of the the ones-complement sum of the data taken 16 bits at a time.
This routine is optimized to checksum a list of BUFFERS; use checksum-1 to checksum a single buffer.
SUM is the initial sum passed in.  For TCP, UDP, use psuedo-header-checksum to calculate this."
  (let ((odd-flag nil))
    (dolist (array buffers)
      (let* ((count (length array))
             (odd (oddp count)))
        (setq sum (select-processor
                    (:lambda                    ;Microcode!
                      (compiler:%ip-checksum array sum count odd-flag))
                    ((:falcon :explorer :cadr)
                     (%ip-checksum array sum count odd-flag)))
                  )
        (setq odd-flag (if odd-flag (not odd) odd))))
    (logxor #xffff sum)))

(defsubst checksum-1 (array sum count)
  (logxor #xffff (select-processor
                   (:lambda                     ;Microcode!
                     (compiler:%ip-checksum array sum count nil))
                   ((:falcon :explorer :cadr)
                    (%ip-checksum array sum count nil)))
                 ))

(defun pseudo-header-checksum (ip length)
  "Calculate the pseudo-header checksum for UDP and TCP based on IP source and destination addresses,
IP protocol number, and total data length."
  (let ((high-sum (+ (aref ip 12) (aref ip 14)  ;high bytes of source address...
                     (aref ip 16) (aref ip 18)  ;high bytes of destination address...
                     (ash length -8)))          ;and high byte of length
        (low-sum (+ (aref ip 13) (aref ip 15)   ;low bytes of source address
                    (aref ip 17) (aref ip 19)   ;low bytes of destination address
                    (aref ip 9)                 ;protocol number...
                    (logand length #xff))))     ;and low byte of length
    (build-checksum-from-bytes low-sum high-sum)))

;;; IP header building functions

(defun make-ip-header (&key (ttl 60) (may-fragment t) (protocol 0)
                       source destination
                       (precedence :routine) (delay :normal)
                       (throughput :normal) (reliability :normal)
                       security route stream timestamp
                       &aux option-length length ih)
  (declare (special security route stream timestamp))
  (when precedence
    (check-type precedence #.(make-key-list *ih-precedence-alist*)))
  (when delay
    (check-type delay #.(make-key-list *ih-delay-alist*)))
  (when throughput
    (check-type throughput #.(make-key-list *ih-throughput-alist*)))
  (when reliability
    (check-type reliability #.(make-key-list *ih-reliability-alist*)))

  (assert (<= (setq option-length (find-option-length)) max-option-length)
          (security stream route timestamp)
          "Total option length > ~D: security = ~D stream = ~D route = ~D timestamp = ~D"
          max-option-length
          (security-option-length security)
          (stream-option-length stream)
          (route-option-length route)
          (timestamp-option-length timestamp))

  (setq length (+ ih-option-offset option-length))

  (if source
      (setq source (validate-address source))
    (setq source (ip-default-address *ip-stream*)))

  (when destination
    (setq destination (validate-address destination)))

  ;; All is well.  Adjust the route by putting ultimate destination at end.
  (cond ((member (car route) '(:loose :strict))
         ;;A route is specified
         (unless destination
           ;;If no destination, we will remove an address from the route
           (decf option-length 4)
           (decf length 4))
         (psetq destination (second route)
                route (append (list (first route))
                              (cddr route)
                              (and destination (list destination)))))
        ((null destination)
         ;;No route and no destination -- assume will be set later with set-destination-address
         (setq destination 0)))

  ;;Now that we've verified all the parameters, build the header
  (setq ih (get-ip-header))
  (setf (fill-pointer ih) length)
  (setf (ih-version ih) ip-version-number)
  (setf (ih-ihl ih) (ceiling length 4))
  (setf (ih-tos ih) 0)
  (setf (ih-precedence ih) (cdr (assoc precedence *ih-precedence-alist* :test #'eq)))
  (setf (ih-delay ih) (cdr (assoc delay *ih-delay-alist* :test #'eq)))
  (setf (ih-throughput ih) (cdr (assoc throughput *ih-throughput-alist* :test #'eq)))
  (setf (ih-reliability ih) (cdr (assoc reliability *ih-reliability-alist* :test #'eq)))
  (setf (ih-length ih) length)
  (setf (ih-identification ih) 0)
  (setf (ih-flags ih) (if may-fragment 0 df-flag))
  (setf (ih-fragment-offset ih) 0)
  (setf (ih-ttl ih) ttl)
  (setf (ih-protocol ih) protocol)
  (setf (ih-checksum ih) 0)
  (setf (ih-source-address ih) source)
  (setf (ih-dest-address ih) destination)
  (when (plusp option-length)
    (make-ip-options ih :security security :stream stream
                        :route route :timestamp timestamp))

  ih)

(defun validate-address (address)
  "Given something that parses into an Internet address, return the address.
Keeps prompting user until a valid Internet address is found"
  (let ((parsed-address nil))
    (assert (setq parsed-address (parse-internet-address address))
            (address)
            "~S is not a valid Internet address specification"
            address)
    parsed-address))

(defun find-option-length ()
  (declare (special security stream route timestamp))
  (when security
    (check-ih-security))
  (when route
    (check-ih-route))
  (when stream
    (check-type stream (unsigned-byte 16)))
  (when timestamp
    (check-ih-timestamp))
  (* (ceiling  (+ (security-option-length security)
                  (stream-option-length stream)
                  (route-option-length route)
                  (timestamp-option-length timestamp))
               4)
     4))

(defun security-option-length (security)
  (if security 11 0))

(defun stream-option-length (stream)
  (if stream 4 0))

(defun route-option-length (route)
  (if route
      (case (car route)
        ((:loose :strict)
         (+ 3 (* 4 (length (cdr route)))))
        (:record
         (+ 3 (* 4 (cadr route)))))
    0))

(defun timestamp-option-length (timestamp)
  (if timestamp
      (case (car timestamp)
        (:ts-only
         (+ 4 (* 4 (cadr timestamp))))
        (:ts-address
         (+ 4 (* 8 (cadr timestamp))))
        (:ts-specified-address
         (+ 4 (* 8 (length (cdr timestamp))))))
    0))

(defun check-ih-security ()
  (declare (special security))
  (check-type (first security) #.(make-key-list *ih-security-alist*))
  (check-type (second security) (unsigned-byte 16))
  (check-type (third security) (unsigned-byte 16))
  (check-type (fourth security) (unsigned-byte 24))
  t)

(defun check-ih-route ()
  (declare (special route))
  (cond ((null route) t)
        ((listp route)
         (check-type (first route) (member :loose :strict :record))
         (case (first route)
           ((:loose :strict)
            (assert (<= (length (cdr route)) 9)
                    ((cdr route))
                    "More than 9 addresses in route")
            (do* ((route-list (copy-list (cdr route)))
                  (rest route-list (cdr rest)))
                 ((null rest)
                  (setf (cdr route) route-list))
              (setf (car rest) (validate-address (car rest)))))
           (:record
            (check-type (second route) (integer 1 9))))
         t)
        (t nil)))

(defun check-ih-timestamp ()
  (declare (special timestamp))
  (cond ((null timestamp) t)
        ((listp timestamp)
         (check-type (first timestamp) #.(make-key-list *ih-timestamp-type-alist*))
         (case (first timestamp)
           (:ts-only
            (check-type (second timestamp) (integer 1 9)))
           (:ts-address
            (check-type (second timestamp) (integer 1 4)))
           (:ts-specified-address
            (assert (<= (length (cdr timestamp)) 4)
                    ((cdr timestamp))
                    "More than 4 addresses in timestamp")
            (do* ((timestamp-list (copy-list (cdr timestamp)))
                  (rest timestamp-list (cdr rest)))
                 ((null rest)
                  (setf (cdr timestamp) timestamp-list))
              (setf (car rest) (validate-address (car rest))))))
         t)
        (t nil)))

(defun make-ip-options (ih &key security stream route timestamp)
  (when (or security stream route timestamp)
    (let ((offset ih-option-offset))
      (when security
        (setq offset (make-security-option ih security offset)))
      (when stream
        (setq offset (make-stream-option ih stream offset)))
      (when route
        (setq offset (make-route-option ih route offset)))
      (when timestamp
        (setq offset (make-timestamp-option ih timestamp offset)))
      (do ()
          ((zerop (rem offset 4)))
        (incf offset (store-one-byte-in-option ih-opt-end ih offset))))))

(defun make-security-option (bytes security offset)
  (incf offset (store-one-byte-in-option ih-opt-sec bytes offset))
  (incf offset (store-one-byte-in-option 11 bytes offset))
  (incf offset (store-two-bytes-in-option (cdr (assoc (first security) *ih-security-alist* :test #'eq))
                                          bytes offset))
  (incf offset (store-two-bytes-in-option (second security) bytes offset))
  (incf offset (store-two-bytes-in-option (third security) bytes offset))
  (incf offset (store-three-bytes-in-option (fourth security) bytes offset))
  offset)

(defun make-stream-option (bytes stream offset)
  (incf offset (store-one-byte-in-option ih-opt-stream bytes offset))
  (incf offset (store-one-byte-in-option 4 bytes offset))
  (incf offset (store-two-bytes-in-option stream bytes offset))
  offset)

(defun make-route-option (bytes route offset &aux type length)
  (case (car route)
    (:loose
     (setq type ih-opt-lsrr length (+ 3 (* 4 (length (cdr route))))))
    (:strict
     (setq type ih-opt-ssrr length (+ 3 (* 4 (length (cdr route))))))
    (:record
     (setq type ih-opt-rr length (+ 3 (* 4 (cadr route))))))
  (incf offset (store-one-byte-in-option type bytes offset))
  (incf offset (store-one-byte-in-option length bytes offset))
  (incf offset (store-one-byte-in-option 4 bytes offset))
  (case (car route)
    ((:loose :strict)
     (dolist (address (cdr route))
       (incf offset (store-four-bytes-in-option address bytes offset))))
    (:record
     (dotimes (i (* 4 (cadr route)))
       (incf offset (store-one-byte-in-option 0 bytes offset)))))
  offset)

(defun make-timestamp-option (bytes timestamp offset &aux length)
  (case (car timestamp)
    (:ts-only
     (setq length (+ 4 (* 4 (cadr timestamp)))))
    (:ts-address
     (setq length (+ 4 (* 8 (cadr timestamp)))))
    (:ts-specified-address
     (setq length (+ 4 (* 8 (length (cdr timestamp)))))))
  (incf offset (store-one-byte-in-option ih-opt-ts bytes offset))
  (incf offset (store-one-byte-in-option length bytes offset))
  (incf offset (store-one-byte-in-option 5 bytes offset))
  (incf offset (store-one-byte-in-option (cdr (assoc (car timestamp) *ih-timestamp-type-alist* :test #'eq))
                                         bytes offset))
  (case (car timestamp)
    ((:ts-only :ts-address)
     (dotimes (i (- length 4))
       (incf offset (store-one-byte-in-option 0 bytes offset))))
    (:ts-specified-address
     (dolist (address (cdr timestamp))
       (incf offset (store-four-bytes-in-option address bytes offset))
       (dotimes (i 4)
         (incf offset (store-one-byte-in-option 0 bytes offset))))))
  offset)

(defun store-one-byte-in-option (byte array index)
  (setf (aref array index) byte)
  1)

(defun store-two-bytes-in-option (val array index)
  (store-one-byte-in-option (ldb (byte 8 8) val) array index)
  (store-one-byte-in-option (ldb (byte 8 0) val) array (1+ index))
  2)

(defun store-three-bytes-in-option (val array index)
  (store-one-byte-in-option (ldb (byte 8 16) val) array index)
  (store-one-byte-in-option (ldb (byte 8 8) val) array (1+ index))
  (store-one-byte-in-option (ldb (byte 8 0) val) array (+ index 2))
  3)

(defun store-four-bytes-in-option (val array index)
  (store-one-byte-in-option (ldb (byte 8 24) val) array index)
  (store-one-byte-in-option (ldb (byte 8 16) val) array (1+ index))
  (store-one-byte-in-option (ldb (byte 8 8) val) array (+ index 2))
  (store-one-byte-in-option (ldb (byte 8 0) val) array (+ index 3))
  4)

;;; User interface to send packets

(defun send-ip-packet (stream buffers header &optional int-pkt identifier
                       &aux gateway interface address (length 0))
  "Send IP packet(s) on behalf of a transport protocol given an IP header and a list of buffers"
  (declare (values fragments-sent identifier))
  (check-type stream ip-transport-protocol)
  (when (null buffers)
    (error "No buffers"))
  (unless (listp buffers)
    (setq buffers (ncons buffers)))
  (check-type header (satisfies ip-header-p))
  (dolist (b buffers)
    (check-type b (satisfies byte-array-or-string-p))
    (incf length (length b)))
  (cond (identifier
         (check-type identifier (unsigned-byte 16)))
        (t
         (setq identifier (tp-next-identification stream))
         (setf (tp-next-identification stream) (mod (1+ identifier) #xffff))))
  ;;first, set the fields the user has no control over
  (setf (ih-version header) ip-version-number)
  (setf (ih-length header) (ih-ihl-bytes header))
  (setf (ih-flags header) (logand (ih-flags header) df-flag))
  (setf (ih-fragment-offset header) 0)
  (setf (ih-protocol header) (tp-type stream))
  (setf (ih-checksum header) 0)
  (unwind-protect
      (cond ((not (check-user-header header))
             (values nil :header))
            ((not (multiple-value-setq (gateway interface)
                    (route (ih-dest-address header))))
             (values nil :route))
            ((not (multiple-value-setq (address interface)
                    (send *ip-stream* :translate-address gateway interface)))
             (values nil :arp))
            (t
             (send-ip-packet-on-interface header
                                          buffers
                                          interface
                                          (prog1 int-pkt (setq int-pkt nil))
                                          identifier
                                          length
                                          address)))
    (when int-pkt
      (free-packet int-pkt))))

(defun broadcast-ip-packet (stream buffers header &optional remote-network function
                            &aux identifier gateway interface (length 0) (sent 0))
  "Send IP packet(s) on behalf of a transport protocol given an IP header and a list of buffers"
  (declare (values fragments-sent identifier))
  (check-type stream ip-transport-protocol)
  (when (null buffers)
    (error "No buffers"))
  (unless (listp buffers)
    (setq buffers (ncons buffers)))
  (check-type header (satisfies ip-header-p))
  (dolist (b buffers)
    (check-type b (satisfies byte-array-or-string-p))
    (incf length (length b)))
  (setq identifier (tp-next-identification stream))
  (setf (tp-next-identification stream) (mod (1+ identifier) #xffff))
  ;;first, set the fields the user has no control over
  (setf (ih-version header) ip-version-number)
  (setf (ih-length header) (ih-ihl-bytes header))
  (setf (ih-flags header) (logand (ih-flags header) df-flag))
  (setf (ih-fragment-offset header) 0)
  (setf (ih-protocol header) (tp-type stream))
  (setf (ih-checksum header) 0)
  (when (check-user-header header)
    (if remote-network
        (when (multiple-value-setq (gateway interface)
                (route (ip-broadcast-address-from-address remote-network)))
          (setq sent (or (broadcast-ip-packet-on-interface header buffers interface identifier length function) 0)))
      (dolist (interface net:*network-interfaces*)
        (incf sent (or (broadcast-ip-packet-on-interface header buffers interface identifier length function) 0)))))
  (values sent identifier))

(defun broadcast-ip-packet-on-interface (header buffers interface identifier length &optional function &aux (frags 0))
  (cond ((net:ni-broadcast-address interface)
         (let ((remote-network (cadr (assoc :internet (net:ni-address-alist interface)))))
           (when remote-network
             (set-destination-address header (ip-broadcast-address-from-address remote-network))
             (when function
               (funcall function header buffers))
             (send-ip-packet-on-interface header
                                          buffers
                                          interface
                                          nil
                                          identifier
                                          length
                                          (net:ni-broadcast-address interface)))))
        (t
         (dolist (at (net:ni-address-translations interface))
           (when (eq (net:at-protocol at) :internet)
             (set-destination-address header (ip-broadcast-address-from-address (net:at-protocol-address at)))
             (when function
               (funcall function header buffers))
             (incf frags (send-ip-packet-on-interface header
                                                      buffers
                                                      interface
                                                      nil
                                                      identifier
                                                      length
                                                      (net:at-hardware-address at)))))
         frags)))

(defun send-ip-packet-on-interface (header buffers interface int-pkt identifier length address)
  (declare (values fragments-sent identifier))
  (do* ((max-data (net:ni-maximum-data-length interface))
        (data-left length (- data-left count))
        (header-length (header-length header t))
        (offset 0 (+ offset count))
        (count (min (- max-data header-length) data-left)
               (min (- max-data header-length) data-left))
        (fragments-sent 0 (1+ fragments-sent)))
       ((zerop data-left) (values fragments-sent identifier))
    (when (< count data-left)                   ;fragmentation needed
      (unless (zerop (ih-flags-df header))      ; DF set?
        (return (values 0 nil)))                ; yes -- can't send
      (setq count (* 8 (truncate count 8))))    ;fragment -- 8 byte chunks
    (unless (send *ip-stream*
                  :send
                  (build-ip-fragment header
                                     buffers
                                     offset
                                     count
                                     interface
                                     (prog1 int-pkt (setq int-pkt nil))
                                     identifier)
                  (+ header-length count)
                  interface
                  address)
      ;;Send failed -- break out of loop
      (return (values fragments-sent identifier)))
    (when (and (> data-left count)
               (zerop fragments-sent))
      (setq header-length (header-length header nil)))))

(defun build-ip-fragment (header buffers offset count interface &optional int-pkt identifier &aux packet)
  "Creates an IP fragment from user HEADER and data BUFFERS.  OFFSET and COUNT
determine the portion of the buffer included in this fragment.  If OFFSET is 0
and COUNT equals the buffer's fill-pointer, the whole buffer fits."
  (setq int-pkt (if int-pkt (original-array int-pkt) (allocate-packet)))
  (let* ((header (net:ni-sent-header-length interface))
         (trailer (net:ni-sent-trailer-length interface))
         (total-bytes (- (* 2 (array-length int-pkt)) header trailer)))
    (setq packet (make-array total-bytes
                             :element-type '(unsigned-byte 8)
                             :displaced-to int-pkt
                             :displaced-index-offset header
                             :fill-pointer 0)))

  (setq offset (* 8 (truncate offset 8)))       ;align to 8 byte boundary
  ;;First, copy in the IP header
  (copy-ip-header header                        ;copy from user header
                  packet                        ;to packet
                  (zerop offset))               ;first-fragment-p
  (when identifier
    (setf (ih-identification packet) identifier))
  (setf (ih-fragment-offset packet) (truncate offset 8))   ;8-byte boundary
  (unless (= (+ offset count) (apply '+ (mapcar 'length buffers)))
    (setf (ih-flags-mf packet) 1))
  (setf (ih-length packet) (+ (ih-ihl-bytes packet) count))
  (store-header-checksum packet)

  ;;set the fill pointer of the packet to reflect size of ip packet
  (setf (fill-pointer packet) (+ (ih-ihl-bytes packet) count))
  (setf (fill-pointer int-pkt) (ceiling (fill-pointer packet) 2))

  ;;Copy proper portion of user's data buffers into packet.
  (do* ((b buffers (cdr b))                     ;(car b) is buffer we are looking at
        (doff (ih-ihl-bytes packet))            ;destination (packet) offset in bytes
        (start 0 (+ start length))              ;offset of start of current buffer
        (length 0))                             ;length of current buffer
       ((or (null b) (zerop count)))            ;quit when run out of buffers or data left to copy
    (setq length (length (car b)))
    (when (and (<= start offset)
               (<= offset (+ start length)))
      (let* ((off (- offset start))
             (len (min count (- length off))))
        (copy-array-portion (car b) off (+ off len) packet doff (+ doff len))
        (decf count len)
        (incf offset len)
        (incf doff len))))

  (unless (zerop count)
    (cerror "continue" "Couldn't copy enough data from supplied buffers"))

  ;;return the int-pkt we built a packet in
  int-pkt)

(defun copy-ip-header (from to first-fragment-p)
  (let* ((length (ih-ihl-bytes from)))          ;length in bytes
    (cond ((or first-fragment-p (= length ih-option-offset))
           (copy-array-portion from 0 length to 0 length))
          (t
           (copy-array-portion from 0 ih-option-offset to 0 ih-option-offset)
           (do* ((source-offset ih-option-offset)
                 (dest-offset ih-option-offset)
                 olength
                 option)
                ((>= source-offset length)
                 (setf (ih-ihl to) (ceiling dest-offset 4))
                 (do ()
                     ((zerop (rem dest-offset 4)))
                   (setf (aref to dest-offset) ih-opt-end)
                   (incf dest-offset)))
             (case (setq option (aref from source-offset))
               ((#.ih-opt-end #.ih-opt-nop)
                (incf source-offset 1))
               (otherwise
                (setq olength (aref from (1+ source-offset)))
                (unless (zerop (logand option #o200))
                  (copy-array-portion from
                                      source-offset (+ source-offset olength)
                                      to
                                      dest-offset (+ dest-offset olength))
                  (incf dest-offset olength))
                (incf source-offset olength)))))))
  to)

(defun set-destination-address (header address)
  "Sets the ultimate destination address in the supplied IP header"
  (let* ((length (ih-ihl-bytes header)))
    (setq address (validate-address address))
    (when (> length ih-option-offset)
      (do ((offset ih-option-offset))
          ((>= offset length))
        (case (aref header offset)
          ((#.ih-opt-end #.ih-opt-nop)
           (incf offset 1))
          ((#.ih-opt-ssrr #.ih-opt-lsrr)
           (let* ((length (aref header (1+ offset)))
                  (count (truncate length 4)))
             (store-four-bytes-in-option address header (+ offset 3 (* 4 (1- count))))
             (return-from set-destination-address header)))
          (otherwise
           (incf offset (aref header (1+ offset)))))))
    ;;No options, or no source route
    (setf (ih-dest-address header) address)
    header))

(defun set-precedence (header precedence)
  "Sets the precedence in an IP header"
  (setf (ih-precedence header) precedence))

(defun ip-route-option-present (header)
  "Return T if an IP routing option appears in the header"
  "Sets the ultimate destination address in the supplied IP header"
  (do ((length (ih-ihl-bytes header))
       (offset ih-option-offset))
      ((>= offset length)
       nil)
    (case (aref header offset)
      ((#.ih-opt-end #.ih-opt-nop)
       (incf offset 1))
      ((#.ih-opt-ssrr #.ih-opt-lsrr)
       (return t))
      (otherwise
       (incf offset (aref header (1+ offset)))))))

(defun reverse-route (header)
  "Given an IP header with (possibly) a recorded route, reverse the route, allowing
the header to be used to reach the original sender"
  (let* ((source (ih-source-address header))
         (destination (ih-dest-address header))
         (length (ih-ihl-bytes header)))
    (when (> length ih-option-offset)
      (do ((offset ih-option-offset))
          ((>= offset length))
        (case (aref header offset)
          ((#.ih-opt-end #.ih-opt-nop)
           (incf offset 1))
          ((#.ih-opt-rr #.ih-opt-ssrr #.ih-opt-lsrr)
           (let* ((length (aref header (1+ offset)))
                  (count (truncate length 4))
                  route)
             (push source route)
             (do ((i count (1- i))
                  (off (+ offset 3) (+ 4 off)))
                 ((zerop i))
               (push (get-four-bytes-from-option header off) route))
             (incf offset 2)
             (incf offset (store-one-byte-in-option 4 header offset))
             (setq source (pop route))
             (do ((i count (1- i)))
                 ((zerop i))
               (incf offset (store-four-bytes-in-option (pop route) header offset)))))
          (otherwise
           (incf offset (aref header (1+ offset)))))))
    (setf (ih-source-address header) destination)
    (setf (ih-dest-address header) source))
  header)

(defun find-security-and-precedence (header)
  "Given an IP header, return the security and precedence options from it"
  (declare (values precedence security compartment handling tcc))
  (let ((precedence (ih-precedence header))
        (security 0)
        (compartment 0)
        (handling 0)
        (tcc 0)
        (length (ih-ihl-bytes header)))
    (when (> length ih-option-offset)
      (do ((offset ih-option-offset))
          ((>= offset length))
        (case (aref header offset)
          ((#.ih-opt-end #.ih-opt-nop)
           (incf offset 1))
          (#.ih-opt-sec
           (setq security (get-two-bytes-from-option header (+ 2 offset)))
           (setq compartment (get-two-bytes-from-option header (+ 4 offset)))
           (setq handling (get-two-bytes-from-option header (+ 6 offset)))
           (setq tcc (get-three-bytes-from-option header (+ 8 offset)))
           (return t))
          (otherwise
           (incf offset (aref header (1+ offset)))))))
    (values precedence security compartment handling tcc)))

(defun header-length (header first-fragment-p)
  "Given an IP header, return header length in bytes.  first-fragment-p is used
in calculating length of options -- not all are copied in subsequent fragments"
  (let ((length (ih-ihl-bytes header)))
    (if (or first-fragment-p (= length ih-option-offset))
        length
      (do* ((result ih-option-offset)
            (offset ih-option-offset)
            option)
           ((>= offset length)
            (* 4 (ceiling result 4)))
        (case (setq option (aref header offset))
          ((#.ih-opt-end #.ih-opt-nop)
           (incf offset 1))
          (otherwise
           (unless (zerop (logand option #o200))
             (incf result (aref header (1+ offset))))
           (incf offset (aref header (1+ offset)))))))))

(defun check-user-header (ih)
  (and (>= (ih-ihl ih) 5)
       (member (ih-source-address ih) (ip-addresses *ip-stream*))
       (or (= (ih-ihl-bytes ih) ih-option-offset)
           (check-user-options ih))))

(defun check-user-options (ih)
  "Checks the options ina user-supplied IP header and returns t if all is well"
  (let ((length (ih-ihl-bytes ih)))
    (or (= length ih-option-offset)
        (do ((offset ih-option-offset)
             (ok t)
             olength)
            ((or (not ok) (>= offset length))
             ok)
          (multiple-value-setq (olength ok)
            (check-user-option ih offset))
          (if ok (incf offset olength))))))

(defun check-user-option (opt offset)
  "Checks an IP header option and returns two values: option length, option-ok"
  (declare (values length option-ok-p))
  (case (aref opt offset)
    (#.ih-opt-end
     (values 1 t))
    (#.ih-opt-nop
     (values 1 t))
    (#.ih-opt-sec
     (check-user-security opt offset))
    (#.ih-opt-stream
     (values 4 t))
    (#.ih-opt-ts
     (check-user-timestamp opt offset))
    ((#.ih-opt-lsrr #.ih-opt-ssrr #.ih-opt-rr)
     (check-user-route opt offset))
    (otherwise
     (values (aref opt (1+ offset)) nil))))

(defun check-user-security (opt offset)
  (let ((length (aref opt (1+ offset))))
    (values length
            (and (= length 11)
                 (rassoc (get-two-bytes-from-option opt (+ offset 2))
                         *ih-security-alist*)))))

(defun check-user-timestamp (opt offset)
  (let ((length (aref opt (1+ offset)))
        (pointer (aref opt (+ offset 2))))
    (values length
            (and (<= length (- (array-length opt) offset))
                 (rassoc (ldb (byte 4 0) (aref opt (+ offset 3)))
                         *ih-timestamp-type-alist*)
                 ;;make sure pointer in valid range.
                 ;;Could insist it be 5, i.e. at start of route
                 (<= 5 pointer (1+ length))))))

(defun check-user-route (opt offset)
  (let ((length (aref opt (1+ offset)))
        (pointer (aref opt (+ offset 2))))
    (values length
            (and (<= length (- (array-length opt) offset))
                 ;;make sure pointer in valid range.
                 ;;Could insist it be 4, i.e. at start of route
                 (<= 4 pointer (1+ length))
                 (zerop (rem pointer 4))))))

(defun store-header-checksum (ip &optional force)
  (setf (ih-checksum ip) 0)
  (and (not force)                              ;If we are not forcing checksum to be calculated,
       (not (ip-local-checksums *ip-stream*))   ;  local checksums are disabled
       (local-host-p (ih-dest-address ip))      ;  and this is a local packet
       (return-from store-header-checksum t))   ;  Then don't bother with checksum
  (setf (ih-checksum ip) (checksum-1 ip 0 (ih-ihl-bytes ip))))

;;; IP received packet handling

(defun check-header-checksum (ip)
  (when (and (not (ip-local-checksums *ip-stream*))
             (local-host-p (ih-source-address ip)))
    (return-from check-header-checksum t))
  (let ((sum (checksum-1 ip 0 (ih-ihl-bytes ip))))
    (values (zerop sum) sum)))

(defun check-net-header (int-pkt)
  "Check the IP header and return t if bad, with offset of badness"
  (declare (values not-ok pointer))
  (cond ((/= (ih-version int-pkt) ip-version-number)
         (values t 0))
        ((< (ih-ihl int-pkt) 5)
         (values t 0))
        ((< (ih-length int-pkt) (ih-ihl-bytes int-pkt))
         (values t 3))
        (t nil)))

(defun process-packet-options (packet)
  "Process the IP header options.  Return t and pointer if bad option encountered"
  (declare (values not-ok pointer))
  (let ((length (ih-ihl-bytes packet)))
    (when (> length ih-option-offset)
      (do ((offset ih-option-offset)
           (ok t)
           olength
           pointer)
          ((or (not ok) (>= offset length))
           (values (not ok) pointer))
        (multiple-value-setq (olength ok pointer)
          (process-ih-option packet offset))
        (if ok (incf offset olength))))))

(defun process-ih-option (opt offset)
  "Process one IP header option and return three values: length, option-ok-p, pointer to badness"
  (declare (values length option-ok-p pointer))
  (case (aref opt offset)
    (#.ih-opt-end
     (values 1 t nil))
    (#.ih-opt-nop
     (values 1 t nil))
    (#.ih-opt-sec
     (values 11 t nil))
    (#.ih-opt-stream
     (values 4 t nil))
    (#.ih-opt-ts
     (process-ih-timestamp opt offset))
    ((#.ih-opt-lsrr #.ih-opt-ssrr #.ih-opt-rr)
     (process-ih-route opt offset))
    (otherwise
     (values (aref opt (1+ offset)) nil offset))))

(defun process-ih-timestamp (opt offset)
  "Process an IP Timestamp option and return two values: length, option-ok-p"
  (declare (values length option-ok-p))
  (let* ((length (aref opt (1+ offset)))
         (pointer (aref opt (+ 2 offset)))
         (index (1- pointer))
         (flags (aref opt (+ 3 offset))))
    (when (> pointer length)
      (incf (ldb (byte 4 4) (aref opt (+ 3 offset))))   ;increment overflow byte
      (return-from process-ih-timestamp (values length t)))
    (case (car (rassoc (ldb (byte 4 0) flags) *ih-timestamp-type-alist*))
      (:ts-only
       (when (< (- length index) 4)             ;error -- not enough room
         (return-from process-ih-timestamp (values length nil (1+ offset))))
       (store-four-bytes-in-option (milliseconds-since-midnight-gmt) opt (+ offset index))
       (incf (aref opt (+ 2 offset)) 4))
      (:ts-address
       (when (< (- length index) 8)             ;error -- not enough room
         (return-from process-ih-timestamp (values length nil (1+ offset))))
       (store-four-bytes-in-option (ip-default-address *ip-stream*) opt (+ offset index))
       (store-four-bytes-in-option (milliseconds-since-midnight-gmt) opt (+ 4 offset index))
       (incf (aref opt (+ 2 offset)) 8))
      (:ts-specified-address
       (when (member (get-four-bytes-from-option opt index)
                     (ip-addresses *ip-stream*))
         (incf index 4)
         (when (< (- length index) 4)           ;error -- not enough room
           (return-from process-ih-timestamp (values length nil (1+ offset))))
         (store-four-bytes-in-option (milliseconds-since-midnight-gmt) opt (+ offset index))
         (incf (aref opt (+ 2 offset)) 8)))
      (otherwise
       (return-from process-ih-timestamp (values length nil (+ 3 offset)))))
    (values length t)))

(defun process-ih-route (opt offset &aux address)
  "Process an IP routing option and return two values: length, option-ok-p"
  (declare (values length option-ok-p))
  (let* ((option (aref opt offset))
         (length (aref opt (1+ offset)))
         (pointer (aref opt (+ 2 offset)))
         (index (+ offset (1- pointer))))
    (when (and (= option ih-opt-rr)
               (< (- length pointer) 3))
      ;;No room to record address (exhausted, or < 4 bytes)
      (return-from process-ih-route (values length nil (1+ offset))))
    (when (> pointer length)                    ;source route exhausted?
      ;;If the address in the destination field has been reached
      (when (or (= option ih-opt-rr)
                (member (ih-dest-address opt) (ip-addresses *ip-stream*)))
        ;;Replace the destination address with the next source address
        (unless (= option ih-opt-rr)
          (setf (ih-dest-address opt) (get-four-bytes-from-option opt index)))
        ;;Find my internet address on the packet's next network
        (multiple-value-bind (ignore interface)
            (route (ih-dest-address opt))
          (unless interface
            (icmp:icmp :destination-unreachable opt icmp:icmp-source-route-failed)
            (return-from process-ih-route (values length t)))
          (setq address (cadr (assoc :internet (net:ni-address-alist interface)))))
        ;;And store our address in the recorded route
        (store-four-bytes-in-option address opt index))
      ;;Point to next address in source route
      (incf (aref opt (+ 2 offset)) 4))
    (values length t)))

;;; FRAGMENT REASSEMBLY

(defun pass-up-packet (int-pkt broadcast-p interface &aux buffer header local-p)
  (let ((stream (cdr (assoc (ih-protocol int-pkt) (ip-protocols *ip-stream*) :test #'eq))))
    (when (and stream (tp-enabled stream))
      (multiple-value-setq (buffer header)
        (bind-fragment int-pkt stream))
      (setq local-p (local-host-p (ih-source-address header)))
      (when buffer
        (when broadcast-p
          (incf (tp-broadcast-packets-received stream))
          (unless (tp-broadcast-allowed-p stream)
            (return-from pass-up-packet t)))
        (send stream :packet-arrival buffer header local-p broadcast-p interface))
      t)))

(defun bind-fragment (int-pkt stream &aux count buffer header)
  (declare (values buffer header))
  (setq count (- (ih-length int-pkt) (ih-ihl-bytes int-pkt)))
  (cond ((or (not (zerop (ih-fragment-offset int-pkt)))
             (not (zerop (ih-flags-mf int-pkt))))
         (reassemble-fragments int-pkt stream count))
        ((setq buffer (find-receive-buffer stream int-pkt))
         (setq header (copy-ip-header int-pkt (get-ip-header) t))
         (setf (fill-pointer header) (ih-ihl-bytes int-pkt))
         (copy-array-portion int-pkt (ih-ihl-bytes int-pkt) (ih-length int-pkt) buffer 0 count)
         (setf (fill-pointer buffer) count)
         (values buffer header))
        (t
         (incf (tp-packets-received-discarded stream))
         (incf (tp-bytes-received-discarded stream) count)
         (icmp:icmp :source-quench int-pkt)
         nil)))

(defstruct (fragment
             (:conc-name "FRAG-")
             (:print-function (lambda (frag stream ignore)
                                (sys:printing-random-object
                                  (frag stream :type :no-pointer)
                                  (format stream "~A(~X)"
                                          (and (frag-source frag) (canonical-ip (frag-source frag)))
                                          (frag-id frag))))))
  id                                            ;fragment id
  source                                        ;source address
  dest                                          ;destination address
  buffer                                        ;reassembly buffer
  header                                        ;IP header
  time                                          ;fragment expiration
  holes                                         ;hole list
  )

;;;a hole is just a dotted pair -- start and end offsets
(defmacro hole-start (hole)
  `(car ,hole))

(defmacro hole-end (hole)
  `(cdr ,hole))

(defun find-receive-buffer (stream int-pkt &aux count)
  "Free expired fragments and find a receive buffer to hold datagram in int-pkt"
  (setq count (- (ih-length int-pkt) (ih-ihl-bytes int-pkt)))
  ;; move expired fragments to buffer list
  (when (tp-fragment-list stream)
    (let ((now (global:time))
          (id (ih-identification int-pkt))
          (source (ih-source-address int-pkt))
          (dest (ih-dest-address int-pkt)))
      (dolist (frag (tp-fragment-list stream))
        (cond ((and (= id (frag-id frag))
                    (= source (frag-source frag))
                    (= dest (frag-dest frag)))
               (delete-fragment frag stream))
              ((time-lessp (frag-time frag) now)
               (incf (ip-fragment-timeouts *ip-stream*))
               (when (frag-header frag)
                 (icmp:icmp :time-exceeded (frag-header frag) icmp:icmp-fragment-timeout))
               (delete-fragment frag stream))))))
  ;; find first buffer with at least COUNT bytes
  (dolist (b (tp-receive-buffers stream))
    (when (>= (array-length b) count)
      (setf (tp-receive-buffers stream) (delete b (tp-receive-buffers stream) :test #'eq))
      (return b))))

(defun delete-fragment (frag stream)
  (setf (tp-fragment-list stream) (delete frag (tp-fragment-list stream) :test #'eq))
  (when (frag-header frag)
    (free-ip-header (frag-header frag)))
  (push (frag-buffer frag) (tp-receive-buffers stream)))

(defun reassemble-fragments (int-pkt stream count &aux frag buffer)
  (declare (values buffer header))
  (let* ((id (ih-identification int-pkt))
         (source (ih-source-address int-pkt))
         (dest (ih-dest-address int-pkt))
         (expiration (time-increment (global:time) (* 60 (ih-ttl int-pkt)))))

    ;;find matching fragment; if none, create one and initialize it
    (unless (setq frag (dolist (f (tp-fragment-list stream))
                         (and (= id (frag-id f))
                              (= source (frag-source f))
                              (= dest (frag-dest f))
                              (return f))))
      ;; count is length of current fragment.  Want a buffer big enough to hold
      ;; entire datagram, but total length not known until last fragment.  Sigh.
      (unless (setq buffer (find-receive-buffer stream int-pkt))
        (incf (tp-packets-received-discarded stream))
        (incf (tp-bytes-received-discarded stream) count)
        (return-from reassemble-fragments nil))
      (setq frag (make-fragment :id id :source source :dest dest
                                :buffer buffer :time expiration))
      (push (cons 0 (1- (array-length buffer))) (frag-holes frag))
      (push frag (tp-fragment-list stream)))

    ;;copy in data, clearing holes.
    (unless (copy-fragment-data int-pkt frag)
      (delete-fragment frag stream)
      (incf (tp-packets-received-discarded stream))
      (incf (tp-bytes-received-discarded stream) count)
      (return-from reassemble-fragments nil))

    ;;update fragment lifetime
    (if (time-lessp (frag-time frag) expiration)
        (setf (frag-time frag) expiration))

    ;;If first fragment, save IP header
    (when (zerop (ih-fragment-offset int-pkt))
      (let ((header (copy-ip-header int-pkt (get-ip-header) t)))
        (setf (fill-pointer header) (ih-ihl-bytes header))
        (setf (frag-header frag) header)))

    ;;If no more holes, pass up complete datagram
    (unless (frag-holes frag)
      (let ((header (frag-header frag))
            (buffer (frag-buffer frag)))
        (setf (tp-fragment-list stream) (delete frag (tp-fragment-list stream) :test #'eq))
        (setf (ih-length header) (+ (ih-ihl-bytes header) (fill-pointer buffer)))
        (setf (ih-flags-mf header) 0)
        (values buffer header)))))

(defun copy-fragment-data (int-pkt frag)
  "Copy data from INT-PKT to correct part of buffer in FRAG.  Adjust the holes
in FRAG and set the fill-pointer of the buffer if total data length now known.
Return t if whole fragment fit, nil if not copied because would be truncated."
  (let* ((start (* 8 (ih-fragment-offset int-pkt)))
         (length (- (ih-length int-pkt) (ih-ihl-bytes int-pkt)))
         (end (1- (+ start length))))
    (when (< end (array-length (frag-buffer frag)))
      ;;For all the holes, copy in portion of buffer that is in hole
      (do* ((result nil)
            (holes (frag-holes frag) (cdr holes))
            (hole (car holes) (car holes)))
           ((null holes)
            (setf (frag-holes frag) (nreverse result)))
        (if (and (<= (hole-start hole) end)
                 (>= (hole-end hole) start))
            ;;fragment overlaps this hole
            (let* ((first (max (hole-start hole) start))
                   (count (- (1+ (min (hole-end hole) end)) first))
                   (source-first (+ (ih-ihl-bytes int-pkt) (- first start))))
              (copy-array-portion int-pkt source-first (+ source-first count)
                                  (frag-buffer frag) first (+ first count))
              (if (<= start (hole-start hole))
                  ;;fragment overlaps front of hole -- truncate front of hole
                  (if (zerop (ih-flags-mf int-pkt))     ;if last fragment,
                      (setq holes nil)          ;forget this and remaining holes
                    (when (< end (hole-end hole))       ;unless overlaps end
                      (setf (hole-start hole) (1+ end)) ;just truncate front
                      (push hole result)))
                ;;else, doesn't overlap front
                (let (new-hole)
                  (if (< end (hole-end hole))
                      (if (zerop (ih-flags-mf int-pkt)) ;doesn't overlap end...
                          (setq holes nil)      ;but last fragment; forget rest
                        ;;not last fragment, so split hole in two
                        (setq new-hole (cons (1+ end) (hole-end hole)))))
                  (setf (hole-end hole) (1- start))     ;trim first hole
                  (push hole result)            ;save it
                  (and new-hole (push new-hole result)))))
          (push hole result)))

      ;;If last fragment, set fill pointer to total data length
      (when (zerop (ih-flags-mf int-pkt))
        (setf (fill-pointer (frag-buffer frag)) (1+ end)))

      ;;And return t because entire fragment fit into buffer
      t)))

;;; IP routing table

(defstruct (route-entry
             (:conc-name "RE-")
             (:type :list))
  network                                       ;network or host
  gateway                                       ;gateway to get to it
  interface                                     ;network interface to get to that gateway
  tos                                           ;type-of-service
  age                                           ;expiration time for this entry
  )

(defvar *route-table* nil "List mapping network numbers to (gateway interface)")
(defvar *default-network-numbers* nil "The network numbers of directly connected networks")
(defvar *default-gateway* nil "The default gateway")
(defvar *default-interface* nil "The interface to reach the default gateway")
(defvar *network-list* nil "Association List of Networks and Subnet Masks")

(defun list-route-table ()
  (format t "~&Known Network~16TSubnet Mask")
  (dolist (elem *network-list*)
    (format t "~&~A~16T~A"
            (canonical-ip (car elem))
            (canonical-ip (cdr elem))))
  (format t "~&~%Network~16TGateway~32TTOS~40TExpires~50TInterface")
  (let ((now (zl:time)))
    (dolist (elem *route-table*)
      (let ((net (re-network elem))
            (ni (re-interface elem)))
        (format t "~&~A~16T~A~32T~A~40T~A~50T~A ~A ~S"
                (if (zerop net)
                    "DEFAULT"
                  (canonical-ip net))
                (canonical-ip (re-gateway elem))
                (re-tos elem)
                (and (re-age elem) (time-difference (re-age elem) now))
                (net:ni-interface ni)
                (net:ni-keyword ni)
                (net:ni-tag ni))))))

(defvar icmp:*icmp-redirect-expiration* (* 5 60 60)
  "Interval in ticks for ICMP redirections to expire.  If NIL, they never expire.")

(defun add-gateway (network gateway &optional interface tos expires &aux parsed-network parsed-gateway)
  (and (null interface)
       (null (setq interface *default-interface*))
       (error "No interface specified in ADD-GATEWAY and no default interface is known."))
  (assert (setq parsed-network (parse-internet-address network))
          (network)
          "Bad internet address specified for NETWORK:~A"
          network)
  (assert (setq parsed-gateway (parse-internet-address gateway))
          (gateway)
          "Bad internet address specified for GATEWAY: ~A"
          gateway)
  (without-interrupts
    (remove-gateway parsed-network tos)
    (when (zerop parsed-network)
      (setq *default-gateway* parsed-gateway)
      (setq *default-interface* interface))
    (push (make-route-entry :network parsed-network
                            :gateway parsed-gateway
                            :interface interface
                            :tos tos
                            :age (and expires (time-increment (zl:time) expires)))
          *route-table*)
    (when expires
      (reset-ip-background-wakeup-time))))

(defun remove-gateway (network &optional tos &aux parsed-network)
  (assert (setq parsed-network (parse-internet-address network))
          (network)
          "Bad internet address specified for NETWORK: ~A"
          network)
  (without-interrupts
    (when (and (zerop parsed-network) (null tos))
      (setq *default-gateway* nil)
      (setq *default-interface* nil))
    (do* ((list *route-table* (cdr list))
          (item (car list) (car list)))
         ((null list))
      (when (and (= parsed-network (re-network item))
                 (eql tos (re-tos item)))
        (setq *route-table* (delete item *route-table*))
        (when (re-age item)
          (reset-ip-background-wakeup-time))
        (return)))))

;;; The (:DEFAULT-INTERNET-ROUTING <value>) site option.
;;; If <value> is an atom then it is the default gateway. Probably a very common
;;; case is that there is only one gateway, and this will suffice.
;;; Otherwise the <value> is a list, of (<network-or-host> <gateway>).
;;; Hosts and gateways may be given with symbolic names (e.g. 'ALADDIN or "ALADDIN"),
;;; as an integer, or as a dotted decimal address (e.g. '192.25.3.4 or "192.25.3.4".)
;;; The <network-or-host> may also be a <network> address, which is like a host address
;;; except that the lower bits are zero. e.g. "192.25.3.0" (for a class-C address).
;;; <network-or-host> = 0 means the default gateway.

(defun initialize-route-table (default-internet-address)
  (when default-internet-address
    (setq *default-gateway* nil)
    (setq *default-interface* nil)
    (when (consp default-internet-address)
      ;;If given a list of addresses, assume the first is the default
      (setq default-internet-address (first default-internet-address)))
    (let ((r (global:get-site-option :default-internet-routing))
          (default-interface nil))
      (setq *route-table* nil)
      ;;Add entries for directly connected networks
      (setq *default-network-numbers* nil)
      (dolist (ni *network-interfaces*)
        (let ((my-address (second (assoc :internet (net:ni-address-alist ni) :test #'eq))))
          (when my-address                      ;If we have an Internet address on this network...
            (let* ((ip-on-ni (assoc :internet (net:ni-network-alist ni) :test #'eq))
                   (my-network (second ip-on-ni))
                   (mask (third ip-on-ni)))
              (when my-network                  ;If this directly connects us to another network
                (when (= my-address default-internet-address)   ;If this interface has my default address...
                  (setq default-interface ni))                  ;make it my default interface
                (let ((network-number (logand mask my-network)))
                  (push (cons network-number mask) *default-network-numbers*)
                  (add-gateway network-number my-address ni)))))))
      ;;Add entries for gateways listed in site files
      (when default-interface                   ;If we are not currently enabled for IP, nothing to do...
        (cond ((null r))
              ((atom r)
               (let ((parsed-address (parse-internet-address r)))
                 (if parsed-address
                     (add-gateway 0 parsed-address default-interface)
                   (tv:careful-notify nil t "Bad Internet Address in :default-internet-routing site option: ~S" r))))
              ((listp r)
               (dolist (elem r)
                 (let ((parsed-network (parse-internet-address (first elem)))
                       (parsed-gateway (parse-internet-address (second elem))))
                   (if (and parsed-network parsed-gateway)
                       (add-gateway parsed-network parsed-gateway default-interface)
                     (tv:careful-notify nil t "Bad entry in :default-internet-routing site option: ~S" elem)))))
              (t nil))))
    (let ((op-address nil))
      (cond ((null si:*ethernet-hardware-controller*))  ;No network interface?  Too bad...
            ((eq si:*ethernet-hardware-controller* si:*my-op*)) ;We talk directly to ethernet?  Good...
            ((setq op-address (net:find-network-address-for-other-processor si:*ethernet-hardware-controller* :internet))
             (add-gateway 0 op-address si:share-interface))
            (t
             (tv:careful-notify nil t "The si:*ethernet-hardware-controller* has no Internet Address"))))))

(defun setup-my-internet-address ()
  "Reads :network-names site option and sets up *network-list*.
Return two values: a list of our Internet addresses and a list of corresponding Subnet Masks"
  (declare (values address-list subnet-mask-list))
  (flet ((find-subnet-mask (address)
           ;;given address and *network-list*, return subnet-mask
           (dolist (elt *network-list* (ip-subnet-mask address))
             (let ((mask (cdr elt)))
               (when (= (logand (car elt) mask) (logand address mask))
                 (return mask))))))
    (let* ((network-addresses (send si:local-host :network-addresses))
           (internet-addresses (getf network-addresses :internet))
           (network-names (global:get-site-option :network-names))
           (subnet-masks nil))
      (setq *network-list* nil)
      (dolist (network network-names)
        (dolist (domain (second network))
          (when (eq (first domain) :internet)
            (let* ((network-number (parse-internet-address (second domain)))
                   (subnet-mask (if (third domain)
                                    (parse-internet-address (third domain))
                                  (ip-subnet-mask network-number))))
              (push (cons network-number subnet-mask) *network-list*)))))
      (setq subnet-masks (mapcar #'find-subnet-mask internet-addresses))
      (let ((old-internet-address (and *ip-stream* (ip-enabled *ip-stream*) (first (ip-addresses *ip-stream*))))
            (new-internet-address (first internet-addresses)))
        (cond ((eql old-internet-address new-internet-address)
               ;;No change (no IP before and none now, or had same IP address)  -- nothing to do
               )
              ((null old-internet-address)
               ;;Didn't used to have an IP address but now we do.  Can't do anything here, as this function
               ;;can be called either from (net:configure) or the site initialization list.  In the first
               ;;case, IP will be started.  In the second, the user must do a (net:configure).
               )
              ((null new-internet-address)
               ;;Used to have an IP address but now we don't -- disable IP
               (send *ip-stream* :close))
              (t
               ;;We used to have an IP address and we still do -- but it has changed.  We must change the
               ;;addresses in the Network Interfaces so that ARP will work right
               (dolist (ni net:*network-interfaces*)
                 (unless (eq (net:ni-interface ni) :loopback)
                   (delete-from-alist :internet (net:ni-address-alist ni))
                   (push (list :internet new-internet-address) (net:ni-address-alist ni))))
               (setf (ip-addresses *ip-stream*)
                     (substitute new-internet-address old-internet-address (ip-addresses *ip-stream*)))
               ;;Must also reset all the transport protocols -- existing connections are to the old address
               (dolist (tp (ip-protocols *ip-stream*))
                 (send (cdr tp) :reset)))))
      (values internet-addresses subnet-masks))))

(add-initialization "Initialize IP routing table"
                    '(initialize-route-table (setup-my-internet-address))
                    '(:site :normal))

(defun route (address &optional tos)
  "Given an internet address, choose the gateway to send a packet to"
  (declare (values gateway interface local-p))
  ;;If this is one of our addresses, return loopback interface
  (when (member address (ip-addresses *ip-stream*))
    (return-from route (values address *loopback-interface* t)))
  ;;If this is the address of host on same backplane, return share interface
  (and si:share-interface
       (arp:get-address-info address :internet nil si:share-interface nil)
       (return-from route (values address si:share-interface t)))
  ;;If address on directly connected network, send directly
  (dolist (interface *network-interfaces*)
    (let* ((ip-on-ni (assoc :internet (net:ni-network-alist interface) :test #'eq))
           (my-network (second ip-on-ni))
           (mask (third ip-on-ni)))
      (when ip-on-ni
        (cond ((net:ni-point-to-point interface)
               ;;search address cache
               (when (arp:get-address-info address :internet nil interface nil)
                 ;;The address is accessible on point-to-point link
                 (return-from route (values address interface nil))))
              ((and mask (= (logand mask address) (logand mask my-network)))
               ;;We are directly connected to that network.  Send the packet directly
               (return-from route (values address interface nil)))))))
  ;;Else, search routing table
  (let ((network-number (ip-network-number-from-address address)))
    (dolist (x *network-list*)
      ;;For all known networks....
      (when (= (car x) (logand (cdr x) address))
        ;;If this is one of them, pick out the real network number under subnet mask
        (return (setq network-number (car x)))))
    (do* ((list *route-table* (cdr list))
          (item (car list) (car list))
          (host-entry nil)                      ;Entry found for this host and any TOS
          (network-entry)                       ;Entry found for this network and this TOS
          (default-entry nil))                  ;Entry found for this network and any TOS
         ((null list)
          (let ((win (or host-entry network-entry default-entry)))
            (when win
              (return-from route (values (re-gateway win) (re-interface win) nil)))))
      (cond ((= (re-network item) address)      ;Host specific entry matches
             (cond ((eql tos (re-tos item))     ;TOS matches
                    (return-from route (values (re-gateway item) (re-interface item) nil)))
                   ((null (re-tos item))        ;TOS doesn't match, but this is generic
                    (setq host-entry item))))
            ((= (re-network item) network-number)       ;Network entry matches
             (cond ((eql tos (re-tos item))     ;TOS matches
                    (setq network-entry item))
                   ((null (re-tos item))        ;TOS doesn't match but this is generic
                    (setq default-entry item)))))))
  ;;If all else fails, return default gateway
  (values *default-gateway* *default-interface* nil))

;;;Background process -- route table update, packet forwarding, etc.

(defvar *background-wakeup-time* nil
  "If non-NIL, the time when the background process should next wake up")

(defvar *ip-background-process* (make-process "IP Background Process"
                                              :warm-boot-action 'ignore
                                              :priority 25
                                              :arrest-reasons '(:not-running)))

(defvar *maximum-queued-packets* 10 "Max number of packets queued for forwarding")
(defvar *current-queued-packets* 0 "Current number of packets queued for forwarding")
(defvar *queued-packet-fifo* (make-fifo) "Fifo of packets queued for forwarding")

(defun initialize-ip-background-process ()
  (without-interrupts
    (do ((elt (pop-fifo *queued-packet-fifo*)
              (pop-fifo *queued-packet-fifo*)))
        ((null elt))
      (free-packet (first elt))))
  (send *ip-background-process* :preset 'ip-background-process)
  (send *ip-background-process* :reset)
  (send *ip-background-process* :run-reason :enable)
  (send *ip-background-process* :revoke-arrest-reason :not-running))

(defun reset-ip-background-wakeup-time ()
  (let ((wakeup-time nil))
    (without-interrupts
      (dolist (item *route-table*)
        (let ((age (re-age item)))
          (when (and age (or (null wakeup-time) (time-lessp age wakeup-time)))
            (setq wakeup-time age))))
      (setq *background-wakeup-time* wakeup-time))))

(defun queue-packet-for-forwarding (int-pkt interface destination-list)
  ;;Packets can be queued for two reasons:
  ;;   They require fragmentation
  ;;   They are being broadcast to more than one destination
  ;;Either case could block the network receiver waiting for an int-pkt, so the
  ;;background process (which doesn't mind being blocked) does the forwarding.
  (without-interrupts
    (cond ((>= *current-queued-packets* *maximum-queued-packets*)
           ;;Simple-minded criterion to avoid using up all int-pkts.
           (incf (ip-cant-forward *ip-stream*)))
          (t
           (incf (ip-packets-queued *ip-stream*))
           (incf *current-queued-packets*)
           (push-fifo (list int-pkt interface destination-list) *queued-packet-fifo*)))))

(defun ip-background-process ()
  (do ((now nil nil))
      (nil)
    (process-wait "Wakeup Time"
                  #'(lambda () (and (ip-enabled *ip-stream*)
                                    (or (not (fifo-empty-p *queued-packet-fifo*))
                                        (and *background-wakeup-time*
                                             (not (time-lessp (setq now (zl:time))
                                                              *background-wakeup-time*)))))))
    ;;Forward any queued packets.
    (do ((elt (pop-fifo *queued-packet-fifo*)
              (pop-fifo *queued-packet-fifo*)))
        ((null elt))
      (let* ((int-pkt (first elt))
             (interface (second elt))
             (destination-list (third elt))
             (header (make-array 60 :element-type '(unsigned-byte 8) :displaced-to int-pkt))
             (header-length (ih-ihl-bytes header))
             (length (- (ih-length header) header-length))
             (buffer (make-array length
                                 :element-type '(unsigned-byte 8)
                                 :displaced-to int-pkt
                                 :displaced-index-offset header-length)))
        (decf *current-queued-packets*)
        (incf (ip-packets-forwarded *ip-stream*))
        (dolist (address destination-list)
          (send-ip-packet-on-interface header
                                       (ncons buffer)
                                       interface
                                       nil
                                       (ih-identification header)
                                       length
                                       address))
        (free-packet int-pkt)))

    ;;Discard expired route entries
    (without-interrupts
      (do* ((result nil)
            (list *route-table* (cdr list))
            (item (car list) (car list)))
           ((null list)
            (setq *route-table* (nreverse result)))
        (let ((age (re-age item)))
          (when (or (null age)                  ;Permanent entry
                    (time-lessp now age))       ;Transient entry but not expired
            (push item result)))))
    (reset-ip-background-wakeup-time)))

(defun forward-packet (int-pkt interface source &aux destination gateway dest-interface)
  "Forward a packet.  Returns NIL if succeeded, the packet if failed.
It is the caller's responsibility to free the packet in the latter case"
  (cond ((not (plusp (decf (ih-ttl int-pkt))))
         ;;Time-To-Live expired -- send ICMP message and drop packet
         (icmp:icmp :time-exceeded int-pkt icmp:icmp-ttl-exceeded)
         (free-packet int-pkt))
        ((and (multiple-value-setq (gateway dest-interface)
                (route (ih-dest-address int-pkt)))
              (setq destination
                    (send *ip-stream* :translate-address gateway dest-interface (ih-source-address int-pkt))))
         (cond ((and (not (ip-gateway *ip-stream*))                     ;We are not IP gateway
                     (not (eq interface si:share-interface))            ; and not FROM share interface
                     (not (eq dest-interface si:share-interface)))      ; and not TO share interface
                ;;Destination is a processor not on this backplane and we are not being a gateway
                (incf (ip-not-for-me *ip-stream*))
                (icmp:icmp :destination-unreachable int-pkt icmp:icmp-host-unreachable)
                (free-packet int-pkt))
               (t
                ;;Successful routing and address translation -- send to the gateway
                (unless (ip-route-option-present int-pkt)
                  (when (eq interface si:share-interface)
                    ;;If packet is FROM share-interface, conclude that we are that processor's gateway
                    ;;and set *processor-forwarding-alist* to indicate that.
                    (let ((elt (assoc source net:*processor-forwarding-alist*)))
                      (when elt
                        (setf (cdr elt) t))))
                  (when (eq interface dest-interface)
                    (icmp:icmp :redirect
                               int-pkt
                               (if (eq interface si:share-interface)
                                   icmp:icmp-redirect-host
                                 icmp:icmp-redirect-network)
                               gateway)))
                (let ((count (ih-length int-pkt)))
                  (if (> count (net:ni-maximum-data-length dest-interface))
                      (queue-packet-for-forwarding int-pkt dest-interface (ncons destination))
                    (progn
                      (incf (ip-packets-forwarded *ip-stream*))
                      (store-header-checksum int-pkt t)
                      (send *ip-stream* :send int-pkt count dest-interface destination)))))))
        (t
         ;;Either couldn't route to a gateway, or couldn't find gateway's link-level address
         (incf (ip-no-forwarding-address *ip-stream*))
         (when (null gateway)                   ;We couldn't route this
           (icmp:icmp :destination-unreachable int-pkt icmp:icmp-host-unreachable))
         (free-packet int-pkt))))

(defun pass-up-broadcast-packet (stream int-pkt interface source)
  "Deals with a broadcast packet from ethernet or from share interface."
  (cond ((ip-broadcast-address-p (ih-dest-address int-pkt))
         (incf (ip-ip-broadcast-packets stream))
         (pass-up-packet int-pkt t interface)
         (pass-broadcast-packet-to-other-processors int-pkt interface source))
        ((ip-self-address-p (ih-dest-address int-pkt))
         ;;Bug -- Berkeley 4.2 uses self-address as broadcast-address
         (incf (ip-self-address-packets stream))
         (pass-up-packet int-pkt t interface)
         (pass-broadcast-packet-to-other-processors int-pkt interface source))
        (t
         ;;Not really a broadcast packet.
         (free-packet int-pkt))))

(defun pass-broadcast-packet-to-other-processors (int-pkt interface source)
  (unwind-protect
      (when (eq si:*my-op* si:*ethernet-hardware-controller*)   ;If I own the Ethernet hardware...
        (cond ((eq interface net:*loopback-interface*)) ;Don't forward looped-back packets
              ((null si:*other-processors*))            ;If there are no other processors, ignore
              ((eq interface si:share-interface)        ;Packets from other processors go onto ethernet
               ;;If packet came from Share interface, forward out Ethernet interface
               (when (cdr (assoc source net:*processor-forwarding-alist*))
                 ;;Came from a processor we are being a gateway for
                 (forward-broadcast-packet (prog1 int-pkt (setq int-pkt nil)) (find-ethernet-interface))))
              (t                                        ;Packet from Ethernet
               ;;If packet came from Ethernet, forward out Share interface
               (forward-broadcast-packet (prog1 int-pkt (setq int-pkt nil)) si:share-interface))))
    (and int-pkt (free-packet int-pkt))))

(defun find-ethernet-interface ()
  (let* ((address (second (assoc :internet (net:ni-address-alist si:share-interface))))
         (network (ip-network-number-from-address address)))
    (dolist (ni net:*network-interfaces*)
      (unless (eq ni si:share-interface)
        (when (eql network
                   (ip-network-number-from-address (second (assoc :internet (net:ni-network-alist ni)))))
          (return ni))))))

(defun forward-broadcast-packet (int-pkt interface &aux destination)
  (cond ((not (plusp (decf (ih-ttl int-pkt))))
         ;;TTL expired.  No ICMP message sent for broadcast packets...
         (free-packet int-pkt))
        ((eq interface si:share-interface)
         ;;Give copy to each processor on backplane with internet address
         (let ((destination-list nil)
               (max-data (net:ni-maximum-data-length interface))
               (count (ih-length int-pkt)))
           (dolist (at (net:ni-address-translations si:share-interface))
             (let ((op (net:at-hardware-address at)))
               (when (and (eq :internet (net:at-protocol at))
                          (cdr (assoc op net:*processor-forwarding-alist*)))
                 ;;If processor has an Internet address AND we are its gateway, save address
                 (push op destination-list))))
           (cond ((null destination-list)       ;Nobody on backplane with Internet address
                  (free-packet int-pkt))
                 ((> count max-data)            ;Fragmentation needed
                  (queue-packet-for-forwarding int-pkt interface destination-list))
                 ((null (cdr destination-list)) ;Single other processor with Internet address
                  (incf (ip-packets-forwarded *ip-stream*))
                  (store-header-checksum int-pkt)
                  (send *ip-stream* :send int-pkt count interface (car destination-list)))
                 (t                             ;More than one process with Internet address
                  (queue-packet-for-forwarding int-pkt interface destination-list)))))
        ((setq destination (net:ni-broadcast-address interface))
         ;;Network does have a broadcast address...
         (let ((max-data (net:ni-maximum-data-length interface))
               (count (ih-length int-pkt)))
           (cond ((> count max-data)
                  (queue-packet-for-forwarding int-pkt interface (ncons destination)))
                 (t
                  (incf (ip-packets-forwarded *ip-stream*))
                  (store-header-checksum int-pkt t)
                  (send *ip-stream* :send int-pkt count interface destination)))))
        (t
         (free-packet int-pkt))))

;;; IP UTILITY FUNCTIONS

(defun local-host-p (address)
  (values (or (member address (ip-addresses *ip-stream*))
              (and si:share-interface
                   (arp:get-address-info address :internet nil si:share-interface nil)))))

(defun mss (address)
  "Given an internet address, find the network interface packets will be sent on and return its MSS"
  (declare (values mss local-p transmission-time))
  (multiple-value-bind (ignore interface local-p)
      (route address)
    (if interface
        (let ((mss (net:ni-maximum-data-length interface)))
          (values mss
                  local-p
                  (send interface :send-if-handles :time-to-transmit mss)))
      (values 0 nil 0))))

(defun ip-network-number-from-address (adr)
  "Given an IP address, return the network number"
  (cond ((= (ldb (byte 1 31) adr) 0)
         (dpb (ldb (byte 8 24) adr) (byte 8 24) 0))
        ((= (ldb (byte 2 30) adr) 2)
         (dpb (ldb (byte 16 16) adr) (byte 16 16) 0))
        ((= (ldb (byte 3 29) adr) 6)
         (dpb (ldb (byte 24 8) adr) (byte 24 8) 0))
        (t adr)))

(global:define-site-variable *broken-berkeley-unix-broadcast-address-p*
                             :broken-berkeley-unix-broadcast-address-p
  "T to use broken broadcast address used by Berkeley 4.2 systems")

(defun ip-broadcast-address-from-address (adr)
  "Given an IP address, return the network number"
  (let ((host (if *broken-berkeley-unix-broadcast-address-p* 0 #xffffff)))
    (cond ((= (ldb (byte 1 31) adr) 0)
           (dpb (ldb (byte 8 24) adr) (byte 8 24) host))
          ((= (ldb (byte 2 30) adr) 2)
           (dpb (ldb (byte 16 16) adr) (byte 16 16) host))
          ((= (ldb (byte 3 29) adr) 6)
           (dpb (ldb (byte 24 8) adr) (byte 24 8) host))
          (t adr))))

(defun ip-network-number-and-mask (adr)
  "Given an IP address, return the network number and mask assuming no subnets"
  (declare (values network mask))
  (cond ((= (ldb (byte 1 31) adr) 0)
         (values (dpb (ldb (byte 8 24) adr) (byte 8 24) 0)
                 (dpb -1 (byte 8 24) 0)))
        ((= (ldb (byte 2 30) adr) 2)
         (values (dpb (ldb (byte 16 16) adr) (byte 16 16) 0)
                 (dpb -1 (byte 16 16) 0)))
        ((= (ldb (byte 3 29) adr) 6)
         (values (dpb (ldb (byte 24 8) adr) (byte 24 8) 0)
                 (dpb -1 (byte 24 8) 0)))
        (t
         (values adr #xffffffff))))

(defun ip-subnet-mask (adr)
  "Given an IP address, return the mask assuming no subnets"
  (cond ((= (ldb (byte 1 31) adr) 0)
         (dpb -1 (byte 8 24) 0))
        ((= (ldb (byte 2 30) adr) 2)
         (dpb -1 (byte 16 16) 0))
        ((= (ldb (byte 3 29) adr) 6)
         (dpb -1 (byte 24 8) 0))
        (t #xffffffff)))

(defun ip-host-number-from-address (adr)
  "Given an IP address, return the host number"
  (cond ((= (ldb (byte 1 31) adr) 0)
         (ldb (byte 24 0) adr))
        ((= (ldb (byte 2 30) adr) 2)
         (ldb (byte 16 0) adr))
        ((= (ldb (byte 3 29) adr) 6)
         (ldb (byte 8 0) adr))
        (t adr)))

(defun ip-broadcast-address-p (adr)
  "Return T if ADR is an IP broadcast address (all ones, or all ones in host)"
  (or (= adr #xffffffff)
      (dolist (ni *network-interfaces*)
        (let* ((elt (assoc :internet (net:ni-network-alist ni) :test #'eq))
               (mask (third elt)))
          (and elt                              ;if :internet enabled
               mask                             ;and this interface has a network number
               (= (logand mask adr) (logand mask (second elt))) ;and address on this network
               (= (logandc1 mask adr) (logandc1 mask #xffffffff))
               (return t))))
      (multiple-value-bind (ignore mask)
          (ip-network-number-and-mask adr)
        (= (logandc1 mask adr) (logandc1 mask #xffffffff)))))

(defun ip-self-address-p (adr)
  "Return T if ADR is IP 'self' address (all zeros, or all zeros in host)"
  (or (zerop adr)
      (zerop (ip-host-number-from-address adr))))

(defun ip-null-network-p (adr)
  "Return T if network field of ADR is all zeros"
  (not (ldb-test (byte 8 24) adr)))

(defun ip-address-from-network-and-host (network host)
  (+ network
     (if (= (ldb (byte 1 31) network) 0)
         (ldb (byte 24 0) host)
       (if (= (ldb (byte 2 30) network) 2)
           (ldb (byte 16 0) host)
         (if (= (ldb (byte 3 29) network) 6)
             (ldb (byte 8 0) host)
           0)))))

(defun parse-internet-address (address &aux host name)
  (declare (values address host-object))
  (labels ((glom-address (string)
             (let* ((first (string-search-char #\. string))
                    (second (and first (string-search-char #\. string (1+ first))))
                    (third (and second (string-search-char #\. string (1+ second)))))
               (if first                        ;If at least one dot, must be three of them...
                   (when (and second third)
                     (let ((one (parse-integer string :start 0 :end first :junk-allowed t))
                           (two (parse-integer string :start (1+ first) :end second :junk-allowed t))
                           (three (parse-integer string :start (1+ second) :end third :junk-allowed t))
                           (four (parse-integer string :start (1+ third) :junk-allowed t)))
                       (when (and one (not (minusp one)) (< one 256)
                                  two (not (minusp two)) (< two 256)
                                  three (not (minusp three)) (< three 256)
                                  four (not (minusp four)) (< four 256))
                         (dpb one (byte 8 24)
                              (dpb two (byte 8 16)
                                   (dpb three (byte 8 8)
                                        four))))))
                 (parse-integer string :junk-allowed t)))))     ;If no dots, address given as an integer
    (let ((parsed-address (cond ((null address) nil)
                                ((numberp address) address)
                                ((or (and (symbolp address)
                                          (setq name (symbol-name address)))
                                     (and (stringp address)
                                          (setq name address)))
                                 (setq host (si:parse-host name t nil))
                                 (if host
                                     (send host :network-address :internet t)
                                   (glom-address name)))
                                ((typep address 'si:host)
                                 (setq host address)
                                 (send address :network-address :internet t))
                                (t nil))))
      (when parsed-address
        (values parsed-address (or host (si:get-host-from-address parsed-address :internet)))))))


;;; If given a number, this always returns something that PARSE-INTERNET-ADDRESS would make into that number.
(defun host-short-name (host)
  "Return a brief name for the specified host."
  (multiple-value-bind (number object)
      (parse-internet-address host)
    (cond (object
           (send object :short-name))
          (number
           (canonical-ip number))
          (t
           host))))

(defun (:property :internet si:smart-address-chooser) (addresses)
  (or (dolist (x addresses)
        (when (member x *default-network-numbers*
                        :test #'(lambda (a b)
                                  (= (logand a (cdr b)) (car b))))
          (return x)))
      (first addresses)))

(defun new-host-validation-function (host system-type address)
  (let ((parsed-address (parse-internet-address address)))
    (cond ((stringp host)
           ;;Try an ICMP Echo 6 times -- allow for ARP failures, lost packets, etc...
           (do ((tries 0 (1+ tries)))
               ((= tries 6)
                (error "No response from address ~A" (canonical-ip parsed-address)))
             (when (icmp:ping parsed-address)
               (return)))
           (si:define-host host :host-names `(,host)
                                :system-type system-type
                                :internet `(,parsed-address))
           (setq host (si:parse-host host))
           (when (member parsed-address (ip-addresses *ip-stream*))
             (setq si:local-host host))
           host)
          (t
           (when address
             (unless (member parsed-address (send host :internet-addresses))
               (error "~A is not a valid Internet address for ~A" address host)))
           host))))

(setf (get 'si:new-host-validation-function :internet) 'ip:new-host-validation-function)

;;; network-protocol interface

(defun ip-length (int-pkt offset)
  (swap-two-bytes (aref int-pkt (+ offset 1))))

(defun canonical-ip (ip-addr &optional (delimiter #\.))
  "Return canonical string for numeric Internet Address."
  (check-type ip-addr (unsigned-byte 32))
  (format nil "~D~C~D~C~D~C~D"
          (ldb (byte 8 24) ip-addr)
          delimiter
          (ldb (byte 8 16) ip-addr)
          delimiter
          (ldb (byte 8 8) ip-addr)
          delimiter
          (ldb (byte 8 0) ip-addr)
          ))

(defun ip-special-addresses (address interface)
  "Special address translations for IP.  All zeros is SELF, all ones is BROADCAST.
This applies to either entire address, or host-number (under subnet mask) -- see RFC 950"
  (let ((network-mask (third (assoc :internet (net:ni-network-alist interface) :test #'eq))))
    (cond ((or (zerop address)
               (and network-mask
                    (zerop (logandc1 network-mask address))))
           (net:ni-address interface))
          ((or (= address #xffffffff)
               (and network-mask
                    (= (logandc1 network-mask address) (logandc1 network-mask #xffffffff))))
           (net:ni-broadcast-address interface)))))

(defun receive-ip-packet (int-pkt interface stream source destination broadcast-p &aux not-ok pointer)
  (declare (ignore destination))

  (let* ((packet (original-array int-pkt))
         (header (net:ni-rcvd-header-length interface))
         (trailer (net:ni-rcvd-trailer-length interface))
         (total-bytes (+ header trailer)))
    (setq int-pkt (make-array (- (* 2 (array-length packet)) total-bytes)
                              :element-type '(unsigned-byte 8)
                              :displaced-to packet
                              :displaced-index-offset header
                              :fill-pointer (- (* 2 (fill-pointer packet)) total-bytes))))
  (unwind-protect
      (cond ((not (check-header-checksum int-pkt))
             (incf (ip-checksum-failures stream)))
            ((multiple-value-setq (not-ok pointer)
                 (check-net-header int-pkt))
             (incf (ip-bad-header stream))
             (unless broadcast-p
               (icmp:icmp :parameter-problem int-pkt pointer)))
            ((multiple-value-setq (not-ok pointer)
                 (process-packet-options int-pkt))
             (incf (ip-bad-options stream))
             (unless broadcast-p
               (icmp:icmp :parameter-problem int-pkt pointer)))
            ((zerop (ih-ttl int-pkt))
             (incf (ip-ttl-expired stream))
             (unless broadcast-p
               (icmp:icmp :time-exceeded int-pkt icmp:icmp-ttl-exceeded)))
            (broadcast-p
             ;;Hardware broadcast packet.  Don't forward, but pass up if protocol is enabled for such.
             ;;I.e. UDP, ICMP: yes, TCP: no.
             (incf (ip-hardware-broadcast-packets stream))
             (pass-up-broadcast-packet stream (prog1 int-pkt (setq int-pkt nil)) interface source))
            ((ip-broadcast-address-p (ih-dest-address int-pkt))
             ;;IP broadcast, but not hardware broadcast -- presumably came over point-to-point link
             (pass-up-broadcast-packet stream (prog1 int-pkt (setq int-pkt nil)) interface source))
            ((ip-self-address-p (ih-dest-address int-pkt))
             ;;Self-address packet, not broadcast.  Broken Berkeley 4.2 forces us to treat this like
             ;;a broadcast packet.  Someday, when 4.2 is completely dead, do REAL self-address stuff
             (pass-up-broadcast-packet stream (prog1 int-pkt (setq int-pkt nil)) interface source))
            ((not (member (ih-dest-address int-pkt) (ip-addresses stream)))
             (forward-packet (prog1 int-pkt (setq int-pkt nil)) interface source))
            ((pass-up-packet int-pkt nil interface))
            (t
             (icmp:icmp :destination-unreachable int-pkt icmp:icmp-protocol-unreachable)
             (pushnew (ih-protocol int-pkt) (ip-protocols-not-understood stream))
             (incf (ip-unknown-protocol-packets stream))))
    (and int-pkt (free-packet int-pkt))))

;;;CONFIGURATION

(defun setup-ip ()
  (multiple-value-bind (internet-address-list subnet-mask-list)
      (setup-my-internet-address)
    (when internet-address-list
      (let ((my-internet-address (first internet-address-list))
            (my-subnet-mask (first subnet-mask-list)))
        ;;;***Ideally, there should be a way to match multiple internet addresses with
        ;;;***multiple network interfaces -- Internet Gateway functions!
        (setq *ip-stream*
              (make-ip-network-protocol :keyword :internet
                                        :address-length 4
                                        :big-endian-address-p t
                                        :interrupt-function 'receive-ip-packet
                                        :address-printer 'canonical-ip
                                        :get-header-function 'make-ip-header
                                        :free-header-function 'free-ip-header
                                        :send-packet-function 'send-ip-packet
                                        :broadcast-packet-function 'broadcast-ip-packet
                                        :special-address-function 'ip-special-addresses
                                        :packet-length-function 'ip-length
                                        :gauge-name "IP"
                                        :default-address my-internet-address
                                        ))
        (send *ip-stream* :open my-internet-address my-subnet-mask))
      (initialize-route-table internet-address-list)
      (initialize-ip-background-process)
      (send *ip-stream* :enable)
      t)))

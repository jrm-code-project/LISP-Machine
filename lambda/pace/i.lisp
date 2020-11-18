;;; -*- Mode:LISP; Package:I; Base:10; Readtable:CL -*-

;;;to install
;;; chaos:create-bigger-chaosnet-buffers must set array-leader 1 of int-pkts to NIL


; ih-version
; ih-internet-header-length
; ih-type-of-service
; ih-length
; ih-identification
; ih-flags
; ih-fragment-offset
; ih-time-to-live
; ih-protocol
; ih-header-checksum
; ih-source-address
; ih-dest-address

(defmacro ih-version (ip)
  `(ldb (byte 4 4) (aref ,ip 0)))

(defmacro ih-internet-header-length (ip)
  `(ldb (byte 4 0) (aref ,ip 0)))

(defmacro ih-type-of-service (ip)
  `(ldb (byte 8 8) (aref ,ip 0)))

(defun ih-length (ip)
  (dpb (ldb (byte 8 0) (aref ip 1))
       (byte 8 8)
       (ldb (byte 8 8) (aref ip 1))))
(defun set-ih-length (ip val)
  (aset (dpb (ldb (byte 8 0) val)
             (byte 8 8)
             (ldb (byte 8 8) val))
        ip 1))
(defsetf ih-length set-ih-length)

(defmacro ih-identification (ip)
  `(aref ,ip 2))

(defmacro ih-flags (ip)
  `(ldb (byte 3 13.) (aref ,ip 3)))

(defmacro ih-fragment-offset (ip)
  `(ldb (byte 13. 0) (aref ,ip 3)))

(defmacro ih-time-to-live (ip)
  `(ldb (byte 8 0) (aref ,ip 4)))

(defmacro ih-protocol (ip)
  `(ldb (byte 8 8) (aref ,ip 4)))

(defmacro ih-header-checksum (ip)
  `(aref ,ip 5))

(defun ih-source-address (ip)
  (+ (dpb (ldb (byte 8 0) (aref ip 6)) (byte 8 24.) 0)
     (dpb (ldb (byte 8 8) (aref ip 6)) (byte 8 16.) 0)
     (dpb (ldb (byte 8 0) (aref ip 7)) (byte 8 8) 0)
     (dpb (ldb (byte 8 8) (aref ip 7)) (byte 8 0) 0)))
(defun set-ih-source-address (ip val)
  (aset (dpb (ldb (byte 8 16.) val)
             (byte 8 8)
             (ldb (byte 8 24.) val))
        ip 6)
  (aset (dpb (ldb (byte 8 0) val)
             (byte 8 8)
             (ldb (byte 8 8) val))
        ip 7))
(defsetf ih-source-address set-ih-source-address)

(defun ih-dest-address (ip)
  (+ (dpb (ldb (byte 8 0) (aref ip 8)) (byte 8 24.) 0)
     (dpb (ldb (byte 8 8) (aref ip 8)) (byte 8 16.) 0)
     (dpb (ldb (byte 8 0) (aref ip 9)) (byte 8 8) 0)
     (dpb (ldb (byte 8 8) (aref ip 9)) (byte 8 0) 0)))
(defun set-ih-dest-address (ip val)
  (aset (dpb (ldb (byte 8 16.) val)
             (byte 8 8)
             (ldb (byte 8 24.) val))
        ip 8)
  (aset (dpb (ldb (byte 8 0) val)
             (byte 8 8)
             (ldb (byte 8 8) val))
        ip 9))
(defsetf ih-dest-address set-ih-dest-address)

(defun describe-ih (ih)
  (format t "~&version: ~s" (ih-version ih))
  (format t "~&header-length: ~s" (ih-internet-header-length ih))
  (format t "~&type-of-service: ~s" (ih-type-of-service ih))
  (format t "~&length: ~s" (ih-length ih))
  (format t "~&identification: ~s" (ih-identification ih))
  (format t "~&flags: ~s" (ih-flags ih))
  (format t "~&fragment-offset: ~s" (ih-fragment-offset ih))
  (format t "~&time-to-live: ~s" (ih-time-to-live ih))
  (format t "~&protocol: ~s" (ih-protocol ih))
  (format t "~&header-checksum: ~s" (ih-header-checksum ih))
  (format t "~&source-address: ~16r ~s" (ih-source-address ih)
          (or (si:get-host-from-address (ih-source-address ih) :internet)
              (si:get-host-from-address (reverse-internet-address (ih-source-address ih)) :internet)))
  (format t "~&dest-address: ~16r ~s" (ih-dest-address ih)
          (or (si:get-host-from-address (ih-dest-address ih) :internet)
              (si:get-host-from-address (reverse-internet-address (ih-dest-address ih)) :internet)))
  (format t "~&rest of data:")
  (dotimes (i (- (ceiling (ih-length ih) 2) (* (ih-internet-header-length ih) 2)))
    (if (zerop (ldb (byte 3 0) i))
        (format t "~&"))
    (format t "~16,4,'0r " (aref ih (+ i (* (ih-internet-header-length ih) 2)))))
  )

(defun reverse-internet-address (adr)
  (+ (dpb (ldb (byte 8 0) adr) (byte 8 24.) 0)
     (dpb (ldb (byte 8 8) adr) (byte 8 16.) 0)
     (dpb (ldb (byte 8 16.) adr) (byte 8 8) 0)
     (dpb (ldb (byte 8 24.) adr) (byte 8 0) 0)))

(defun setup-my-internet-address ()
  (when (null (send si:local-host :network-address :internet))
    (let ((proposed-address #x64000034))
      (loop until (null (si:get-host-from-address proposed-address :internet))
            do (incf proposed-address))
      (nconc (symeval-in-instance si:local-host 'si:alist-elem)
             (list :internet
                   (list proposed-address))))))

(setup-my-internet-address)

(defconst my-internet-address (send si:local-host :network-address :internet))

(defun send-echo-request (dest-adr &optional (send-it nil))
  (if (not (numberp dest-adr))
      (setq dest-adr (send (si:parse-host dest-adr) :network-address :internet)))
  (let ((ip (chaos:allocate-int-pkt)))
    (setf (ih-version ip) 4)
    (setf (ih-internet-header-length ip) 5)
    (setf (ih-type-of-service ip) 0)
    (setf (ih-length ip) 60.)
    (setf (ih-identification ip) 1234)
    (setf (ih-flags ip) 0)
    (setf (ih-fragment-offset ip) 0)
    (setf (ih-time-to-live ip) 60.)
    (setf (ih-protocol ip) 1)
    (setf (ih-header-checksum ip) -1)
    (setf (ih-source-address ip) my-internet-address)
    (setf (ih-dest-address ip) dest-adr)
    (let ((offset (* (ih-internet-header-length ip) 2)))
      (setf (ldb (byte 8 0) (aref ip offset)) 8)
      (setf (ldb (byte 8 8) (aref ip offset)) 0)
      (aset -1 ip (+ offset 1))                 ;checksum
      (aset 0 ip (+ offset 2))                  ;identifier
      (aset 0 ip (+ offset 3))                  ;sequence number

      (set-header-checksum ip)
      (set-icmp-checksum ip)

      )
    (let ((dest-ether-address (ethernet:get-ethernet-address (reverse-internet-address dest-adr) :ip)))
      (cond ((null dest-ether-address)
             (format t "no ether address"))
            (send-it
             (ethernet:send-int-pkt-via-ethernet
               ip
               ethernet:my-ethernet-address
               dest-ether-address
               ethernet:ip-ethernet-type
               (ceiling (ih-length ip) 2)))))
    ip))


(defun set-header-checksum (ip &aux (sum 0))
  (setf (ih-header-checksum ip) 0)
  (dotimes (i (* 2 (ih-internet-header-length ip)))
    (incf sum (aref ip i))
    (if (ldb-test (byte 1 16.) sum)
        (setq sum (1+ (ldb (byte 16. 0) sum)))))
  (setf (ih-header-checksum ip) (logxor #o177777 sum)))

(defun check-header-checksum (ip &aux (sum 0))
  (dotimes (i (* 2 (ih-internet-header-length ip)))
    (incf sum (aref ip i))
    (if (ldb-test (byte 1 16.) sum)
        (setq sum (1+ (ldb (byte 16. 0) sum)))))
  (values (= sum 177777) sum))

(defun set-icmp-checksum (ip length-in-halfwords &aux (sum 0))
  (let ((offset (* 2 (ih-internet-header-length ip))))
    (aset 0 ip (+ offset 1))
    (dotimes (i length-in-halfwords)
      (incf sum (aref ip (+ i offset)))
      (if (ldb-test (byte 1 16.) sum)
          (setq sum (1+ (ldb (byte 16. 0) sum)))))
    (aset (logxor #o177777 sum) ip (+ offset 1))))

(defun check-icmp-checksum (ip &aux (sum 0))
  (let ((offset (* 2 (ih-internet-header-length ip))))
    (dotimes (i (- (ceiling (ih-length ip) 2) offset))
      (incf sum (aref ip (+ i offset)))
      (if (ldb-test (byte 1 16.) sum)
          (setq sum (1+ (ldb (byte 16. 0) sum)))))
    (values (= sum 177777) sum)))




(defun install-ip-receiver ()
  (setq chaos:ethernet-pkt-handler-alist
        (delq (assq ethernet:ip-ethernet-type
                    chaos:ethernet-pkt-handler-alist)
              chaos:ethernet-pkt-handler-alist))
  (push (cons ethernet:ip-ethernet-type 'receive-ip-ethernet-type)
        chaos:ethernet-pkt-handler-alist))

(defvar *received-ip-pkts* nil)

(defun reset ()
  (setq *received-ip-pkts* nil)
  (chaos:reset t))


(defun receive-ip-ethernet-type (int-pkt)
  (when (or (not (= (%data-type int-pkt) dtp-array-pointer))
            (not (= (%p-data-type int-pkt) dtp-array-header)))
    (ferror nil "int-pkt must be an un-forwarded array"))
  (setf (array-leader int-pkt 1) 'internet-header)
  (setf (%p-ldb si:%%ARRAY-NAMED-STRUCTURE-FLAG int-pkt) 1)
  (setq *received-ip-pkts*
        (append *received-ip-pkts*
                (list int-pkt))))

(defselect ((internet-header si:named-structure-invoke))
  (:describe (pkt)
    (format t "~&~S:" pkt)
    (describe-ih pkt)
    )
  (:print-self (pkt stream ignore ignore)
    (si:printing-random-object (pkt stream :typep)
      (case (ih-protocol pkt)
        (1 (format stream "ICMP"))
        (t (format stream "Protocol ~d" (ih-protocol pkt))))))
  (:which-operations (ignore)
    '(:describe :print-self :which-operations)))


;(install-ip-receiver)

(defstruct (ip-pkt (:type :named-array-leader)
                   (:print (format nil "#<~s ~s>" (type-of ip-pkt) (%pointer ip-pkt)))
                   (:size-symbol)
                   )
  fill-pointer
  sub-header                            ;indirect array to TCP or UDP header
  data                                  ;indirect array to data
  data-string
  )

(defun make-empty-ip-pkt (&optional
                          (internet-header-length 5)
                          (sub-header-size 2)
                          (total-length-in-bytes 30.) &aux pkt)
  (setq pkt (make-array chaos:expanded-max-words-per-pkt
                        :type art-16b
                        :leader-length ip-pkt-size))
  (setf (array-leader pkt 1) 'ip-pkt)

  (setf (ip-pkt-sub-header pkt)
        (make-array sub-header-size
                    :type art-16b
                    :displaced-to pkt
                    :displaced-index-offset (* 2 internet-header-length)))
  (setf (ip-pkt-data pkt)
        (make-array (- (ceiling total-length-in-bytes 2)
                       (* 2 internet-header-length)
                       sub-header-size)
                    :type art-16b
                    :displaced-to pkt
                    :displaced-index-offset (+ (* 2 internet-header-length)
                                               sub-header-size)))
  (setf (ip-pkt-data-string pkt)
        (make-array (* 2 (- (ceiling total-length-in-bytes 2)
                            (* 2 internet-header-length)
                            sub-header-size))
                    :type art-string
                    :displaced-to pkt
                    :displaced-index-offset (* 2 (+ (* 2 internet-header-length)
                                                    sub-header-size))))
  pkt)

(defun change-ip-pkt (pkt internet-header-length sub-header-size total-length-in-bytes)
  (let ((start-of-sub-header (* internet-header-length 2)))
    (si:change-indirect-array (ip-pkt-sub-header pkt)
                              art-16b
                              sub-header-size
                              pkt
                              start-of-sub-header)
    (si:change-indirect-array (ip-pkt-data pkt)
                              art-16b
                              (- (ceiling total-length-in-bytes 2)
                                 start-of-sub-header
                                 sub-header-size)
                              pkt
                              (+ start-of-sub-header
                                 sub-header-size))
    (si:change-indirect-array (ip-pkt-data-string pkt)
                              art-string
                              (* 2 (- (ceiling total-length-in-bytes 2)
                                      start-of-sub-header
                                      sub-header-size))
                              pkt
                              (* 2 (+ start-of-sub-header
                                      sub-header-size))))
  pkt)

(defun send-ip-pkt (ip-pkt dest-adr length-in-bytes
                    identification protocol)
  (let ((ip (chaos:allocate-int-pkt)))
    (copy-array-contents ip-pkt ip)
    (setf (ih-version ip) 4)
    (setf (ih-internet-header-length ip) 5)
    (setf (ih-type-of-service ip) 0)
    (setf (ih-length ip) (+ (* (ih-internet-header-length ip) 2)
                            length-in-bytes))
    (setf (ih-identification ip) identification)
    (setf (ih-flags ip) 0)
    (setf (ih-fragment-offset ip) 0)
    (setf (ih-time-to-live ip) 60.)
    (setf (ih-protocol ip) protocol)
    (setf (ih-header-checksum ip) -1)
    (setf (ih-source-address ip) my-internet-address)
    (setf (ih-dest-address ip) dest-adr)
    (set-header-checksum ip)

 (cerror :no-action nil nil "foo")

    (let ((dest-ether-address (ethernet:get-ethernet-address (reverse-internet-address dest-adr) :ip)))
      (cond ((null dest-ether-address)
             (format t "no ether address"))
            (t
             ;this frees it
             (ethernet:send-int-pkt-via-ethernet
               ip
               ethernet:my-ethernet-address
               dest-ether-address
               ethernet:ip-ethernet-type
               (ceiling (ih-length ip) 2)))))
    ip))


(defun convert-to-icmp-ip-pkt (int-pkt &aux pkt)
  (setq pkt (make-empty-ip-pkt))
  (copy-array-contents int-pkt pkt)
  (setf (array-leader int-pkt 1) nil)
  (chaos:free-int-pkt int-pkt)
  (change-ip-pkt pkt 5 4 (ih-length pkt))
  (setf (array-leader pkt 1) 'icmp-pkt)
  pkt)

(defmacro icmp-type (ip-pkt)
  `(ldb (byte 8 0) (aref (ip-pkt-sub-header ,ip-pkt) 0)))

(defmacro icmp-code (ip-pkt)
  `(ldb (byte 8 8) (aref (ip-pkt-sub-header ,ip-pkt) 0)))

(defmacro icmp-checksum (ip-pkt)
  `(aref (ip-pkt-sub-header ,ip-pkt) 1))


(defselect ((icmp-pkt si:named-structure-invoke))
  (:describe (pkt)
    (format t "~&~S:" pkt)
    (describe-ih (ip-pkt-whole-thing pkt))
    )
  (:print-self (pkt stream ignore ignore)
    (si:printing-random-object (pkt stream :typep)
      (case (icmp-type pkt)
        (3
         (format stream "Dest-Unreachable: ~a"
                 (case (icmp-code pkt)
                   (0 "Net Unreachable")
                   (1 "Host Unreachable")
                   (2 "Protocol Unreachable")
                   (3 "Port Unreachable")
                   (4 "Fragmentation needed and DF set")
                   (5 "Source route failed")
                   (t "Unknwon Code"))))
        (11.
         (format stream "Time Exceeded: ~a"
                 (case (icmp-code pkt)
                   (0 "Time to Live")
                   (1 "Fragment reassembly")
                   (t "Unknown Code"))))
        (12.
         (format stream "Parameter Problem: ~a"
                 (case (icmp-code pkt)
                   (0 "Pointer indicates error")
                   (1 "unknown code"))))
        (4
         (format stream "Source Quench"))
        (5
         (format stream "Redirect: ~a"
                 (case (icmp-code pkt)
                   (0 "Network")
                   (1 "Host")
                   (2 "Type of Service and Network")
                   (3 "Type of Service and Host"))))
        (8
         (format stream "Echo Request"))
        (0
         (format stream "Echo Reply"))
        (13.
         (format stream "Timestamp Request"))
        (14.
         (format stream "Timestamp Reply"))
        (15.
         (format stream "Information Request"))
        (16.
         (format stream "Information Reply")))))
  (:which-operations (ignore)
    '(:describe :print-self :which-operations)))


(defun send-echo-request (dest-adr)
  (if (not (numberp dest-adr))
      (setq dest-adr (send (si:parse-host dest-adr) :network-address :internet)))
  (let ((ip (make-empty-ip-pkt)))
    (setf (icmp-type ip) 8)
    (setf (icmp-code ip) 0)
    (set-icmp-checksum ip 4)
    (send-ip-pkt ip dest-adr 4 1234 1)))


(defun convert-to-tcp-ip-pkt (int-pkt &aux pkt)
  (without-interrupts
    (cond ((not (null ip-pkt-free-head))
           (setq pkt ip-pkt-free-head)
           (setq ip-pkt-free-head (ip-pkt-thread pkt))
           (setf (ip-pkt-thread pkt) nil))))
  (if (null pkt)
      (setq pkt (make-empty-ip-pkt)))
  (copy-array-contents int-pkt (ip-pkt-whole-thing pkt))
  (setf (ip-pkt-int-pkt pkt) int-pkt)
  (let* ((start-of-sub-header (* 2 (ih-internet-header-length int-pkt)))
         (tcp-header-length (* 2 (ldb (byte 4. 4.) (aref int-pkt (+ start-of-sub-header 6)))))
         (start-of-data (+ start-of-sub-header tcp-header-length)))
    (si:change-indirect-array (ip-pkt-sub-header pkt)
                              art-16b
                              tcp-header-length
                              int-pkt
                              start-of-sub-header)
    (si:change-indirect-array (ip-pkt-data pkt)
                              art-16b
                              (- (// (ih-length int-pkt) 2)
                                 start-of-sub-header
                                 tcp-header-length)
                              int-pkt
                              start-of-data)
    (si:change-indirect-array (ip-pkt-data-string pkt)
                              art-string
                              (* 2 (- (// (ih-length int-pkt) 2)
                                      start-of-sub-header
                                      tcp-header-length))
                              int-pkt
                              start-of-data)
    )
  (chaos:free-int-pkt int-pkt)
  pkt)

(defun convert-to-udp-ip-pkt (int-pkt &aux pkt)
  (without-interrupts
    (cond ((not (null ip-pkt-free-head))
           (setq pkt ip-pkt-free-head)
           (setq ip-pkt-free-head (ip-pkt-thread pkt))
           (setf (ip-pkt-thread pkt) nil))))
  (if (null pkt)
      (setq pkt (make-empty-ip-pkt)))
  (copy-array-contents int-pkt (ip-pkt-whole-thing pkt))
  (let* ((start-of-sub-header (* 2 (ih-internet-header-length int-pkt)))
         (start-of-data (+ start-of-sub-header 4)))
    (si:change-indirect-array (ip-pkt-sub-header pkt)
                              art-16b
                              4
                              int-pkt
                              start-of-sub-header)
    (si:change-indirect-array (ip-pkt-data pkt)
                              art-16b
                              (- (// (ih-length int-pkt) 2)
                                 start-of-sub-header
                                 4)
                              int-pkt
                              start-of-data)
    (si:change-indirect-array (ip-pkt-data-string pkt)
                              art-string
                              (* 2 (- (// (ih-length int-pkt) 2)
                                      start-of-sub-header
                                      4))
                              int-pkt
                              start-of-data)
    )
  (chaos:free-int-pkt int-pkt)
  pkt)

(defun free-ip-pkt (pkt)
  (without-interrupts
    (do ((p ip-pkt-free-head (ip-pkt-thread p)))
        ((null p))
      (if (eq p pkt)
          (ferror nil "already on free list")))
    (setf (ip-pkt-thread pkt) ip-pkt-free-head)
    (setq ip-pkt-free-head pkt)))

(defun swap-16-bits (d)
  (dpb (ldb (byte 8 0) d)
       (byte 8 8)
       (ldb (byte 8 8) d)))

(defun tcp-source-port (pkt)
  (swap-16-bits (aref (ip-pkt-sub-header pkt) 0)))

(defun tcp-dest-port (pkt)
  (swap-16-bits (aref (ip-pkt-sub-header pkt) 1)))

(defun tcp-sequence-number (pkt)
  (dpb (swap-16-bits (aref (ip-pkt-sub-header pkt) 2))
       (byte 16. 16.)
       (swap-16-bits (aref (ip-pkt-sub-header pkt) 3))))

(defun tcp-ack-number (pkt)
  (dpb (swap-16-bits (aref (ip-pkt-sub-header pkt) 4))
       (byte 16. 16.)
       (swap-16-bits (aref (ip-pkt-sub-header pkt) 5))))

(defun tcp-data-offset (pkt)
  (ldb (byte 4. 4.) (aref (ip-pkt-sub-header pkt) 6)))

(defun tcp-flags (pkt)
  (dpb (ldb (byte 4 0) (aref (ip-pkt-sub-header pkt) 6))
       (byte 8 8)
       (ldb (byte 8 8) (aref (ip-pkt-sub-header pkt) 6))))

(defun tcp-window (pkt)
  (aref (ip-pkt-sub-header pkt) 7))

(defun tcp-checksum (pkt)
  (aref (ip-pkt-sub-header pkt) 8))

(defun tcp-urgent-pointer (pkt)
  (swap-16-bits (aref (ip-pkt-sub-header pkt) 9)))

; tcp-source-port
; tcp-dest-port
; tcp-sequence-number
; tcp-ack-number
; tcp-data-offset
; tcp-flags
; tcp-window
; tcp-checksum
; tcp-urgent-pointer

(defun tcp-receiver (int-pkt)
  (let ((pkt (convert-to-tcp-ip-pkt int-pkt)))
    (format t "~&**** TCP ****~&")
    (describe-ih int-pkt)
    (format t "~&tcp-source-port: ~16r" (tcp-source-port pkt))
    (format t "~&tcp-dest-port: ~16r" (tcp-dest-port pkt))
    (format t "~&tcp-sequence-number: ~16r" (tcp-sequence-number pkt))
    (format t "~&tcp-ack-number: ~16r" (tcp-ack-number pkt))
    (format t "~&tcp-data-offset: ~16r" (tcp-data-offset pkt))
    (format t "~&tcp-flags: ~16r" (tcp-flags pkt))
    (format t "~&tcp-window: ~16r" (tcp-window pkt))
    (format t "~&tcp-checksum: ~16r" (tcp-checksum pkt))
    (format t "~&tcp-urgent-pointer: ~16r" (tcp-urgent-pointer pkt))
    (format t "~&other words:")
    (dotimes (i (- (* 2 (tcp-data-offset pkt)) 10.))
      (format t "~16,4,'0r " (aref (ip-pkt-sub-header pkt) (+ i 10.))))
    (format t "~&data: ~s" (ip-pkt-data-string pkt))
    (free-ip-pkt pkt)
    (chaos:free-int-pkt int-pkt)))

(push '(6 . tcp-receiver) datagram-handler-alist)

(defun udp-source-port (pkt)
  (swap-16-bits (aref (ip-pkt-sub-header pkt) 0)))

(defun udp-dest-port (pkt)
  (swap-16-bits (aref (ip-pkt-sub-header pkt) 1)))

(defun udp-length (pkt)
  (swap-16-bits (aref (ip-pkt-sub-header pkt) 2)))

(defun udp-checksum (pkt)
  (aref (ip-pkt-sub-header pkt) 3))

(defstruct (udp-conn (:type :named-array)
                     (:print (format nil "#<~s ~s>" (typep udp-conn) (%pointer udp-conn))))
  udp-conn-local-port
  udp-conn-foreign-port
  udp-conn-received-list
  )


(defun udp-receiver (int-pkt)
  (let ((pkt (convert-to-udp-ip-pkt int-pkt)))
    (dolist (conn udp-conn-list)
      (cond ((and (numberp (udp-conn-local-port conn))
                  (= (udp-dest-port pkt) (udp-conn-local-port conn))
                  (or (eq (udp-conn-foreign-port conn) t)
                      (and (numberp (udp-conn-foreign-port conn))
                           (= (udp-source-port pkt) (udp-conn-foreign-port conn)))))
             (setf (udp-conn-foreign-port conn) (udp-source-port pkt))
             (without-interrupts
               (setf (ip-pkt-thread pkt) (udp-conn-received-list conn))
               (setf (udp-conn-received-list conn) pkt))
             (return-from udp-receiver nil))))
    (print-udp-pkt pkt)
    (chaos:free-int-pkt int-pkt)
    (free-ip-pkt pkt)))

(defun print-udp-pkt (pkt)
  (format t "~&**** UDP ****~&")
  (describe-ih (ip-pkt-int-pkt pkt))
  (format t "~&udp-source-port: ~16r" (udp-source-port pkt))
  (format t "~&udp-dest-port: ~16r" (udp-dest-port pkt))
  (format t "~&udp-length: ~d." (udp-length pkt))
  (format t "~&udp-checksum: ~16r" (udp-checksum pkt))
  (format t "~&data: ~s" (ip-pkt-data-string pkt))
  )

(push '(21 . udp-receiver) datagram-handler-alist)


(defvar udp-conn-list nil)

(defun udp-make-conn ()
  (let ((conn (make-udp-conn)))
    (setf (udp-conn-local-port conn) nil)
    (setf (udp-conn-foreign-port conn) nil)
    (setf (udp-conn-received-list conn) nil)
    (push conn udp-conn-list)))


(defun udp-listen (conn port-number &optional (wait t))
  (setf (udp-conn-local-port conn) port-number)
  (setf (udp-conn-foreign-port conn) t)
  (if wait
      (process-wait "UDP Listen" #'(lambda (conn) (udp-conn-received-list conn)) conn))
  conn)

(defun udp-transmit (conn foreign-port-number &optional local-port-number)
  (if local-port-number
      (setf (udp-conn-local-port conn) local-port-number))
  (setf (udp-conn-foreign-port-number conn) foreign-port-number)
  )

(defun udp-close (conn)
  (setq udp-conn-list (delq conn udp-conn-list))
  (setf (udp-conn-local-port conn) nil)
  (setf (udp-conn-foreign-port conn) nil)
  (do ((p (udp-conn-received-list conn) (ip-pkt-thread p)))
      ((null p))
    (chaos:free-int-pkt (ip-pkt-int-pkt p))
    (free-ip-pkt p)))

(defun print-ip-pkts (start)
  (do ((p start (ip-pkt-thread p)))
      ((null p))
    (print p)))

(defun reset ()
  (without-interrupts
    (setq udp-conn-list nil)
    (setq ip-pkt-free-head  nil)
    (chaos:reset t)
    (get-ethernet-address (address-list-to-number (address-parse "angel-ec")) :ip)))


(defvar datagram-handler-alist ())

(defvar internet-receiver nil)

(defun start-internet-receiver ()
  (if (null internet-receiver)
      (setq internet-receiver (make-process "Internet Reciever")))
  (send internet-receiver :preset 'internet-receiver-top-level)
  (process-enable internet-receiver))


(defun internet-receiver-top-level ()
  (pkg-bind 'internet
    (do-forever
      (process-wait "Internet Pkt" #'(lambda () (and chaos:enable
                                                     internet-received-pkts)))
      (internet-receiver-top-level-1))))

(defun internet-receiver-top-level-1 (&aux pkt)
  (block nil
    (without-interrupts
      (if internet-received-pkts
          (setq pkt (pop internet-received-pkts))
        (setq pkt nil)))
    (cond ((not (null pkt))
           (cond ((null (check-header-checksum pkt))
                  (format t "~&bad checksum")
                  (chaos:free-int-pkt pkt)
                  (return nil)))
           (let ((func (cdr (assq (ih-protocol pkt) datagram-handler-alist))))
             (cond ((null func)
                    (format t "~&****~&")
                    (describe-ih pkt)
                    (chaos:free-int-pkt pkt))
                   (t
                    (funcall func pkt))))))))

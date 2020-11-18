;;; -*- Mode:LISP; Package:CHAOS; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defvar cadr-network-interface nil "The CADR Chaosnet interface")

;;; hardware related specials
(defvar base-address #o764140 "Base address of CADR chaosnet hardware registers")
(defvar control-status-register
        base-address "control-status register")
(defvar my-number-register
        (+ base-address (lsh %chaos-my-number-offset 1)) "cable address register")
(defvar write-buffer-register
        (+ base-address (lsh %chaos-write-buffer-offset 1)) "write-data register")
(defvar read-buffer-register
        (+ base-address (lsh %chaos-read-buffer-offset 1)) "read-data register")
(defvar bit-count-register
        (+ base-address (lsh %chaos-bit-count-offset 1)) "bit count register")
(defvar initiate-transfer-register
        (+ base-address (lsh %chaos-start-transmit-offset 1)) "start transfer register")

(defvar pkts-bad-bit-count 0
  "Incremented when a packet is discarded because of a bad bit count:
Bit count less than Dest, Source, and CRC words,
or not mod 16., or doesn't agree with software packet length.")
(defvar pkts-bad-dest 0
  "Incremented when we discard a packet because hardware dest wasn't M-ADDRESS.")
(defvar pkts-bad-crc-1 0
  "Incremented when a packet's CRC was bad on receive.")
(defvar pkts-bad-crc-2 0
  "Incremented when a packet's CRC was bad after readout.")
(defvar pkts-lost 0
  "Incremented when hardware says it lost a packet.")

;;; Also, at the end of an INT-PKT are the source address, destination address, and CRC
(defmacro int-pkt-hardware-dest (int-pkt)
  `(aref ,int-pkt (- (int-pkt-word-count ,int-pkt) 3)))

(defmacro int-pkt-hardware-source (int-pkt)
  `(aref ,int-pkt (- (int-pkt-word-count ,int-pkt) 2)))

(defmacro int-pkt-crc (int-pkt)
  `(aref ,int-pkt (1- (int-pkt-word-count ,int-pkt))))

(defsubst cadr-reset (stream)
  (declare (ignore stream))
  (%unibus-write control-status-register
                 (dpb -1 %%chaos-csr-reset 0)))

(defsubst cadr-enable (stream)
  (declare (ignore stream))
  (cadr-reset stream)
  (%unibus-write control-status-register
                 (dpb -1 %%chaos-csr-interrupt-enables 0))
  (setf (net:ni-address cadr-network-interface) (%unibus-read my-number-register)))

(defun cadr-send-int-pkt (int-pkt source dest packet-type n-16-bit-words)
  (declare (ignore packet-type))
  (declare (ignore n-16-bit-words))
  (assure-safe-int-pkt int-pkt)
  (setf (int-pkt-hardware-source int-pkt) source)
  (setf (int-pkt-hardware-dest int-pkt) dest)
  (without-interrupts
    (prog (old-transmit-list)
       loop
          (setq old-transmit-list (net:int-transmit-list))
          (setf (net:int-pkt-thread int-pkt) old-transmit-list)
          (or (%store-conditional net:int-transmit-list-pointer old-transmit-list int-pkt)
              (go loop))
          (%chaos-wakeup)
          t)))

(defun cadr-packet-ready ()
  (net:int-receive-list))

(defun cadr-get-next-packet (&aux int-pkt bits dest)
  (declare (values packet type source destination broadcast-p))

  (do-forever
    (let ((old-receive-list (net:int-receive-list)))
      (when (%store-conditional-double net:int-receive-list-pointer old-receive-list
                                       (locf (net:int-pkt-thread old-receive-list))
                                       (net:int-pkt-thread old-receive-list))
        (return (setq int-pkt old-receive-list)))))

  (setf (net:int-pkt-thread int-pkt) nil)

  (setq dest (int-pkt-hardware-dest int-pkt)
        bits (int-pkt-bit-count int-pkt))

  (incf pkts-lost (ldb %%chaos-csr-lost-count (int-pkt-csr-2 int-pkt)))

  (cond ((or (< bits 48.) (bit-test 17 bits))
         (incf pkts-bad-bit-count)
         (net:free-packet int-pkt)
         (setq int-pkt nil))
        ((ldb-test %%chaos-csr-crc-error (int-pkt-csr-1 int-pkt))
         (incf pkts-bad-crc-1)
         (net:free-packet int-pkt)
         (setq int-pkt nil))
        ((ldb-test %%chaos-csr-crc-error (int-pkt-csr-2 int-pkt))
         (incf pkts-bad-crc-2)
         (net:free-packet int-pkt)
         (setq int-pkt nil))
        ((and ( dest 0) ( dest (net:ni-address cadr-network-interface)))
         (incf pkts-bad-dest)
         (net:free-packet int-pkt)
         (setq int-pkt nil)))

  (values int-pkt
          nil               ;hardware type
          (int-pkt-hardware-source int-pkt)
          (int-pkt-hardware-dest int-pkt)
          nil)
  )

(defun status (&aux csr lc)
  "Print out contents of hardware registers (CADR Only)"
  (unless (eq si:processor-type-code si:cadr-type-code)
     (ferror "Only CADRs have chaosnet boards"))

  (setq csr (%unibus-read control-status-register))
  (terpri) (terpri)
  (and (ldb-test %%chaos-csr-timer-interrupt-enable csr)
       (format t "Timer interrupt enable or maybe transmit busy.~%"))
  (and (ldb-test %%chaos-csr-loop-back csr)
       (format t "Loopback.~%"))
  (and (ldb-test %%chaos-csr-receive-all csr)
       (format t "Receive all messages mode is on.~%"))
  (and (ldb-test %%chaos-csr-receive-enable csr)
       (format t "Receiver interrupt enabled.~%"))
  (and (ldb-test %%chaos-csr-transmit-enable csr)
       (format t "Transmit interrupt enabled.~%"))
  (and (ldb-test %%chaos-csr-transmit-abort csr)
       (format t "Transmit aborted by collision.~%"))
  (and (ldb-test %%chaos-csr-transmit-done csr)
       (format t "Transmit done.~%"))
  (or  (zerop (setq lc (ldb %%chaos-csr-lost-count csr)))
       (format t "Lost count = ~O~%" lc))
  (and (ldb-test %%chaos-csr-reset csr)
       (format t "I//O reset.~%"))
  (and (ldb-test %%chaos-csr-crc-error csr)
       (format t "-- CRC ERROR!!! --~%"))
  (and (ldb-test %%chaos-csr-receive-done csr)
       (format t "Receive done.~%"))
  (format t "Bit count: ~O~%" (%unibus-read bit-count-register))
  nil)

(defun setup-cadr-network (tag &aux alist)
  (when (eq si:processor-type-code si:cadr-type-code)

    (when cadr-network-interface
      (setq alist (net:ni-protocol-alist cadr-network-interface))
      (funcall cadr-network-interface :close))

    (setq cadr-network-interface
          (net:make-network-interface :tag tag
                                      :interface :cadr
                                      :keyword :chaos
                                      :hardware-type net:arp-chaos-hardware
                                      :address-length 2
                                      :address 0
                                      :broadcast-address 0
                                      :minimum-data-length 0
                                      :maximum-data-length 488.
                                      :sent-header-length 0
                                      :rcvd-header-length 0
                                      :sent-trailer-length 0
                                      :rcvd-trailer-length 0
                                      :loopback t
                                      :protocol-alist alist
                                      :reset-function 'cadr-reset
                                      :disable-function 'cadr-reset
                                      :enable-function 'cadr-enable
                                      :packet-ready-function 'cadr-packet-ready
                                      :get-next-packet-function 'cadr-get-next-packet
                                      :send-function  'cadr-send-int-pkt
                                      :gauge-name "CADR"
                                      ))

    (funcall cadr-network-interface :open)
    (funcall cadr-network-interface :enable)))

(add-initialization "Start CADR interface" '(setup-cadr-network "ONE") :network-driver)

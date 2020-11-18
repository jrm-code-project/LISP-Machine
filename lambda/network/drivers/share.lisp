;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

#|
This is the implementation of the "Shared Chaos" interface for the new network
system.  I attempted to set it up as "just another device", but didn't entirely
succeed;  there are a few places where there is explicit knowledge of this device
as the means of communications with processors on our own backplane.

The following kludges exist:

ARP.LISP (processor-without-network-interface) looks at addresses of processors on share interface
and net:*processor-forwarding-alist* to decide if should do ARP for them.

CHSNCP.LISP (receive-chaos-pkt) and IP.LISP (forward-packet) know that other processors access
the network through our hardware via Share device, and modify net:*processor-forwarding-alist*

IP.LISP (forward-broadcast-packet & pass-broadcast-packet-to-other-processors) look at the
net:*processor-forwarding-alist* to decide if IP broadcast packet should be forwarded.

CHSNCP.LISP (transmit-int-pkt) and IP.LISP (initialize-route-table & route) contain special knowledge
of the Share device as the way to get to other processors.

CONFIGURE.LISP (configure & cold-load-configure) contains code to initialize the address-translation
cache of the share interface.
|#

(net:defstream share-interface
               (net:network-interface)
               "SHARE-"
  (transmit-buffer-not-available-timeouts 0)    ;statistic: how many times send failed because buffer busy
  (transmit-buffer-timeout-period 100000.)      ;# of microseconds we will loop waiting for send buffer
  (n-pkts-not-transmitted-in-a-row 0)           ;number of successive failed packets.
  (lock nil)                                    ;interlock on sending
  )

(net:defop (share-interface :peek-special-fields) (ni)
  (list (tv:scroll-parse-item
          `(:function share-transmit-buffer-timeout-period (,ni) NIL ("Timeout period: ~D"))
          `(:function share-transmit-buffer-not-available-timeouts (,ni) NIL (" time outs: ~D")))))

(defvar share-interface nil "The Lambda Share interface")

(defun get-share-chaos-addresses-for-processor (op)
  (declare (values xmit-adr rcv-adr))
  (let ((xmit-phys (processor-configuration
                     (op-proc-conf op)
                     (+ %processor-conf-chaos-share-0 (op-proc-number *my-op*))))
        (rcv-phys (processor-configuration
                    (op-proc-conf *my-op*)
                    (+ %processor-conf-chaos-share-0 (op-proc-number op)))))
    (values (sdu-phys-to-virtual xmit-phys)
            (sdu-phys-to-virtual rcv-phys))))

(defun set-up-chaos-channels ()
  (setf (%processor-conf-chaos-address *my-proc-conf*) chaos:my-address)
  (dolist (op *other-processors*)
    (setf (op-chaos-xmit-ctl op) nil)
    (setf (op-chaos-xmit-pkt op) nil)
    (setf (op-chaos-rcv-ctl op) nil)
    (setf (op-chaos-rcv-pkt op) nil)

    (multiple-value-bind (xmit-adr rcv-adr)
        (get-share-chaos-addresses-for-processor op)

      (cond ((not (null xmit-adr))
             (setf (op-chaos-rcv-ctl op) (make-chaos-share-virtual rcv-adr))
             (fillarray (op-chaos-rcv-ctl op) nil)

             (let* ((ctl-size (length chaos-share-dev-qs))      ; in words
                    (data-size (- page-size ctl-size)))

               (setf (%chaos-share-size (op-chaos-rcv-ctl op)) ctl-size)
               (setf (%chaos-share-buf-size (op-chaos-rcv-ctl op)) (* 4 data-size))
               (setf (%chaos-share-intr-addr (op-chaos-rcv-ctl op)) 0)
               (setf (op-chaos-rcv-pkt op)
                     (zl:make-array (* data-size 2)
                                    :type 'art-16b
                                    :displaced-to (+ rcv-adr ctl-size)))

               (setf (op-chaos-xmit-ctl op) (make-chaos-share-virtual xmit-adr))
               (setf (op-chaos-xmit-pkt op)
                     (zl:make-array (* data-size 2)
                                    :type 'art-16b
                                    :displaced-to (+ xmit-adr ctl-size)))))))))

(defun share-chaos-interrupt (op)
  (when (share-mode-active-p)
    (let* ((cs (op-chaos-xmit-ctl op))
           (intr-adr (%chaos-share-intr-addr cs)))
      (cond ((zerop intr-adr))
            ((let ((code (ldb (byte 4 28.) intr-adr)))
               (or (= code #xf) (= code #xe)))
             (%nubus-write (ldb (byte 8 24.) intr-adr)
                           (logand #o77777777 intr-adr)
                           1))
            (t
             (ferror nil "bad interrupt addr"))))))

(defun share-reset (stream)
  (declare (ignore stream))
  (when *share-code-ready*
    (set-up-chaos-channels))
  nil)

(defun share-send-int-pkt (int-pkt source op type n-16-bit-words)
  (declare (ignore source))
  (with-lock ((share-lock share-interface))
    (unwind-protect
        (when (and (share-mode-active-p)
                   (not (null (op-chaos-xmit-ctl op))))
          (do ((cs-xmit (op-chaos-xmit-ctl op))
               (transmit-start-time (%fixnum-microsecond-time)))
              ((> (time-difference (%fixnum-microsecond-time) transmit-start-time)
                  (share-transmit-buffer-timeout-period share-interface))
               (incf (share-transmit-buffer-not-available-timeouts share-interface))
               (incf (share-n-pkts-not-transmitted-in-a-row share-interface))
               nil)
            (unless (ldb-test %%chaos-share-dev-valid-bit (%chaos-share-csr cs-xmit))
              (setf (share-n-pkts-not-transmitted-in-a-row share-interface) 0)
              (copy-array-portion int-pkt 0 n-16-bit-words (op-chaos-xmit-pkt op) 0 n-16-bit-words)
              (setf (%chaos-share-pkt-length cs-xmit)
                    (dpb type (byte 16 16) (* 2 n-16-bit-words)))
              (setf (ldb %%chaos-share-dev-valid-bit (%chaos-share-csr cs-xmit)) 1)
              (share-chaos-interrupt op)
              (return t))))
      (net:free-packet int-pkt))))

(defun share-packet-ready ()
  (when (share-mode-active-p)
    (dolist (op *other-processors*)
      (let ((rcv-ctl (op-chaos-rcv-ctl op)))
        (if (and rcv-ctl (ldb-test %%chaos-share-dev-valid-bit (%chaos-share-csr rcv-ctl)))
            (return op))))))

(defun share-get-next-packet (&aux op int-pkt)
  (declare (values packet type source destination broadcast-p))
  (when (and (setq op (share-packet-ready))
             (setq int-pkt (net:allocate-packet-for-receive share-interface)))
    (let* ((cs-rcv (op-chaos-rcv-ctl op))
           (source (op-chaos-rcv-pkt op))
           (type-and-length (%chaos-share-pkt-length cs-rcv))
           (type (ldb (byte 16 16) type-and-length))
           (length (ceiling (ldb (byte 16 0) type-and-length) 2)))
      (setf (fill-pointer int-pkt) length)
      (copy-array-portion source 0 length int-pkt 0 length)
      (setf (ldb %%chaos-share-dev-valid-bit (%chaos-share-csr cs-rcv)) 0)
      (share-chaos-interrupt op)
      ;; Rotate the *other-processors* list so that next time we look
      ;; for a packet, we will look at the other processor first.
      (without-interrupts
        (setq *other-processors* (net:nrotate *other-processors*)))
      (values int-pkt                           ; the packet
              type                              ; the type code
              op                                ; the sender
              nil                               ; the receiver
              nil                               ; not broadcast
              ))))

(defun setup-share-interface (tag &aux alist)
  (when (and si:*other-processors*
             *share-code-ready*)

    (when share-interface
      (setq alist (net:ni-protocol-alist share-interface))
      (send share-interface :close))

    (setq share-interface
          (make-share-interface :tag tag
                                :interface :share
                                :keyword :backplane
                                :address *my-op*
                                :point-to-point t
                                :minimum-data-length 0
                                :maximum-data-length
                                    (* 4 (- page-size (length chaos-share-dev-qs)))
                                :sent-header-length 0
                                :rcvd-header-length 0
                                :sent-trailer-length 0
                                :rcvd-trailer-length 0
                                :protocol-alist alist
                                :reset-function 'share-reset
                                :packet-ready-function 'share-packet-ready
                                :get-next-packet-function 'share-get-next-packet
                                :send-function  'share-send-int-pkt
                                :gauge-name "Share"
                                ))

    (send share-interface :open)
    (send share-interface :enable)))

(add-initialization "Start Share interface" '(setup-share-interface "SHARE") :network-driver)

(when (fboundp 'setf)
  (add-initialization "Plug my chaos address into proc-conf"
                      '(setf (%processor-conf-chaos-address *my-proc-conf*) chaos:my-address)
                      '(:site :normal)))

;some debugging functions
(defvar all-cs nil)

(defun make-all-cs ()
  (setq all-cs nil)
  (let (lowest)
    (dolist (op (cons *my-op* *other-processors*))
      (cond ((= (op-proc-number op) 0)
             (setq lowest (%processor-conf-chaos-share-1 (op-proc-conf op))))))
    (dotimes (i (nth (length *other-processors*) '(2 2 6 10. 20.)))
      (push (make-chaos-share-physical (+ lowest (* 1024. i)))
            all-cs)))
  (setq all-cs (reverse all-cs)))


(defun cs-status ()
  (dolist (op *other-processors*)
    (let ((chaos-adr (%processor-conf-chaos-address (op-proc-conf op))))
      (format t "~&Processor ~s chaos-adr ~o " op chaos-adr)
      (if (not (zerop chaos-adr))
          (format t "~a" (get-host-from-address chaos-adr ':chaos))))
    (describe op)
    (cond ((not (null (op-chaos-xmit-ctl op)))
           (format t "~&My transmit")
           (describe (op-chaos-xmit-ctl op))))
    (cond ((not (null (op-chaos-rcv-ctl op)))
           (format t "~&My receive")
           (describe (op-chaos-rcv-ctl op))))))

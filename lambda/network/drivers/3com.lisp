;;; -*- Mode:LISP; Package:ETHERNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( setup-3com
           *3com-ethernet-interface*
           *3com-owner*))

(defstream 3com-interface
           (network-interface)
           "3COM-"
  (microcode-enabled nil)                         ; T if use lambda microcode
  (transmit-time-outs 0)
  (buffer-not-available 0)
  (jam-count 0)
  (fcs-errors 0)
  (hdr-errors 0)                                  ;random bogus header errors
  )

(defop (3com-interface :peek-special-fields) (ni)
  (list (tv:scroll-parse-item
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (let ((enabled (3com-microcode-enabled ni)))
                                            (funcall ni :disable)
                                            (funcall ni :enable :microcode (not enabled)))
                                    :bindings ((ni ',ni))))
                  :DOCUMENTATION
                  "Click to toggle usage of microcode"
                  :BINDINGS
                  ((ni ',ni)))
             :function 3com-microcode-enabled (,ni) NIL ("Microcode enabled: ~A"))
          `(:function 3com-transmit-time-outs (,ni) NIL (" time outs ~D"))
          `(:function 3com-buffer-not-available (,ni) NIL (" no buffer ~D"))
          `(:function 3com-jam-count (,ni) NIL (" jam count ~D"))
          `(:function 3com-fcs-errors (,ni) NIL (" fcs error ~D"))
          `(:function 3com-hdr-errors (,ni) NIL (" hdr error ~D")))))

(defvar *3com-ethernet-interface* nil "The Lambda 3com Ethernet interface")
(defsubst i-own-3com ()
  (eq *3com-owner* si:*my-op*))

;;; This file contains routines pertaining specifically to the 3Com Ethernet interface

(defparameter 3com-mebase #x30000 "address of 3com ethernet controller")
(defparameter 3com-mecsr 3com-mebase "control/status register for 3com interface")
(defparameter 3com-meback (+ 2 3com-mecsr) "jam backoff counter for 3com interface")
(defparameter 3com-address-rom (+ 3com-mebase #x400) "3com ethernet address ROM")
(defparameter 3com-address-ram (+ 3com-mebase #x600) "3com ethernet address RAM")
(defparameter 3com-transmit-buffer (+ 3com-mebase #x800) "3com transmit buffer")
(defparameter 3com-buffer-a (+ 3com-mebase #x1000) "3com receive buffer A")
(defparameter 3com-buffer-b (+ 3com-mebase #x1800) "3com receive buffer B")
(defparameter 3com-me-buffer-size 2048. "number of bytes in a buffer")

;Note: this code operates with the byte-ordering switch ON on the 3-COM board.
;  This sets "low byte first" mode, like the 8086 and unlike the 68000.
;  Setting it this way means data CAN be read from packet buffers with 32 bit transfers.
;  This is NOT the way the board was shipped by 3-COM.
;  This means the pictures in the manual are byte reversed!
;  In particular, the byte offset words in the buffer headers are byte reversed!!!

(defconstant reset (byte 1 0))                  ;reset the controller.
;;; bit 1 not used
(defconstant rbba (byte 1 2))                   ;A/B receive buffer ordering.
(defconstant amsw (byte 1 3))                   ;address in RAM is valid
(defconstant jam (byte 1 4))                    ;writing 1 clears jam.
(defconstant tbsw (byte 1 5))                   ;set if transmit buffer belongs to ether.
 (defconstant tbsw+jam (byte 2 4))
(defconstant absw (byte 1 6))                   ;set if buffer A belongs to ether.
(defconstant bbsw (byte 1 7))                   ;set if buffer B belongs to ether.
 (defconstant a+b-bsw (byte 2 6))               ; both of the above.
(defconstant pa (byte 4 8))                     ;which frame addresses to accept
(defconstant jinten (byte 1 12))                ;enable interrupts on jam.
(defconstant tinten (byte 1 13))                ;enable interrupts on transmit buffer.
(defconstant ainten (byte 1 14))                ;enable interrupts on buffer A.
(defconstant binten (byte 1 15))                ;enable interrupts on buffer B.

;;;packet class codes
(defconstant pcc-all 0)                         ;receive all packets
(defconstant pcc-all-errors 1)                  ;receive all packets except runts, frame, fcs errors
(defconstant pcc-all-fcs-frame 2)               ;receive all packets except frame & fcs errors
(defconstant pcc-mine+multi 3)                  ;receive this station + multicast packets
(defconstant pcc-mine+multi-errors 4)           ;receive this station + multicast except errors
(defconstant pcc-mine+multi-fcs-frame 5)        ;receive this station + multicast except frame and fcs errors
(defconstant pcc-mine+broad 6)                  ;receive this station + broadcast packets
(defconstant pcc-mine+broad-errors 7)           ;receive this station + broadcast except errors
(defconstant pcc-mine+broad-fcs-frame 8)        ;receive this station + broadcast except frame and fcs errors

(defconstant 3com-csr-background-bits (logior (dpb pcc-mine+broad-errors pa 0) (dpb 1 amsw 0)))

(defun write-3com-csr (new-csr)
  (when (i-own-3com)
    (sys:%nubus-write si:sdu-quad-slot 3com-mecsr new-csr)
    ;; This also writes the jam-backoff counter, which I guess doesn't hurt.
    ;; Writing it in two pieces also has potential lossage.
    ))

(defun read-3com-csr ()
  (if (i-own-3com)
      (ldb (byte 16 0) (sys:%nubus-read si:sdu-quad-slot 3com-mecsr))
    0))

;interface to LAMBDA ethernet microcode

(defun enable-lambda-ucode ()
  (sys:%processor-switches (dpb 1 (byte 1 22.) (sys:%processor-switches nil))))

(defun disable-lambda-ucode ()
  (sys:%processor-switches (dpb 0 (byte 1 22.) (sys:%processor-switches nil))))

(defsubst lambda-ucode-enabled-p ()
  (ldb-test (byte 1 22.) (sys:%processor-switches nil)))

(defun assure-good-int-list-pointer (pointer-to-list)
  (or (eq pointer-to-list net:int-free-list-pointer)
      (eq pointer-to-list net:int-receive-list-pointer)
      (eq pointer-to-list net:int-transmit-list-pointer)))

(defun int-pkt-list-put (int-pkt pointer-to-list)
  (net:assure-safe-int-pkt int-pkt)
  (assure-good-int-list-pointer pointer-to-list)
  (zl:loop for old = (sys:%p-contents-offset pointer-to-list 0)
           do (setf (net:int-pkt-thread int-pkt) old)
           until (sys:%store-conditional pointer-to-list old int-pkt)))

(defun int-pkt-list-get (pointer-to-list)
  (assure-good-int-list-pointer pointer-to-list)
  (zl:loop for old = (sys:%p-contents-offset pointer-to-list 0)
           if (null old) do (return nil)
           until (sys:%store-conditional-double pointer-to-list old
                                                (locf (net:int-pkt-thread old))
                                                (net:int-pkt-thread old))
           finally (setf (net:int-pkt-thread old) nil)
           (return old)))

(defun 3com-reset (stream)
  (when (i-own-3com)
    (without-interrupts
      (disable-lambda-ucode)
      (write-3com-csr (dpb 1 reset 0))  ;don't used background bits here
      (setf (3com-address stream) 0)
      (dotimes (i 6)
        (let ((next-byte (sys:%multibus-read-8 (+ 3com-address-rom i))))
          (sys:%multibus-write-8 (+ 3com-address-ram i) next-byte)
          (setf (3com-address stream)
                (dpb next-byte (byte 8 0) (ash (3com-address stream) 8.)))))
      ;;set up normal csr - address RAM valid, receive MINE + Broadcast packets
      (write-3com-csr 3com-csr-background-bits))
    t))

(defun 3com-disable (stream)
  (when (3com-microcode-enabled stream)
    (disable-lambda-ucode)
    (setf (3com-microcode-enabled stream) nil)
    (without-interrupts     ;free all packets on receive list
      (do ((int-pkt (int-pkt-list-get net:int-receive-list-pointer)
                    (int-pkt-list-get net:int-receive-list-pointer)))
          ((null int-pkt))
        (setf (net:int-pkt-thread int-pkt) nil)
        (free-packet int-pkt)))))

(defun 3com-enable (stream &optional &key (microcode t))
  (when (i-own-3com)
    (setf (3com-microcode-enabled stream) microcode)
    (cond (microcode
           (enable-lambda-ucode))
          (t
           (arm-3com-receive-buffer ABSW)
           (arm-3com-receive-buffer BBSW)))
    t))

(defun reset-jam ()
  (write-3com-csr (dpb 1 reset 0))
  (write-3com-csr 3com-csr-background-bits)
  (incf (3com-jam-count *3com-ethernet-interface*)))

(defun arm-3com-receive-buffer (byte-ptr)
  (tagbody
   l  (when (ldb-test tbsw (read-3com-csr))
        (when (ldb-test jam (read-3com-csr))
          (reset-jam))
        (go l))
      (without-interrupts
        (when (ldb-test tbsw (read-3com-csr))
          (go l))
        (write-3com-csr (dpb 1 byte-ptr 3com-csr-background-bits)))))

(defun write-lambda-3com-frame-header (buffer-base offset source destination type)
  (when (i-own-3com)
    (decf offset 14.)

    (sys:%multibus-write-8 (1+ buffer-base) (ldb (byte 8 0) offset))    ;offset reg is reversed!!!
    (sys:%multibus-write-8  buffer-base (ldb (byte 8 8) offset))

    (dotimes (i 6)
      (let ((next-byte (ldb (byte 8 (* i 8)) destination)))
        (sys:%multibus-write-8 (- (+ buffer-base offset 5) i) next-byte)))

    (dotimes (i 6)
      (let ((next-byte (ldb (byte 8 (* i 8)) source)))
        (sys:%multibus-write-8 (- (+ buffer-base offset 11) i) next-byte)))

    (sys:%multibus-write-8 (+ buffer-base offset 12) (ldb (byte 8 8) type))
    (sys:%multibus-write-8 (+ buffer-base offset 13) (ldb (byte 8 0) type))))

;;; LAMBDA's 3com receive buffer contains
;;; buffer-base:    meahdr (note byte reversed!!)
;;;          +2:    destination
;;;          +8:    source
;;;         +14:    type
;;;         +16:    data

(defconstant %%3com-buffer-header-nbytes (byte 11 0))
(defconstant %%3com-buffer-header-framing-error (byte 1 11))
(defconstant %%3com-buffer-header-address-match (byte 1 12))
(defconstant %%3com-buffer-header-range-error (byte 1 13))
(defconstant %%3com-buffer-header-broadcast (byte 1 14))
(defconstant %%3com-buffer-header-fcs-error (byte 1 15))
(defconstant %%3com-buffer-header-error #o124000)       ;framing error + range error + fcs error

(defun read-3com-buffer-header (buffer-base)
  (dpb (sys:%multibus-read-8 buffer-base)
       (byte 8 8)
       (sys:%multibus-read-8 (1+ buffer-base))))

(defun read-frame-header-destination (buffer-base &aux (result 0))
  "returns the destination address of the packet in the hardware buffer"
  (dotimes (i 6)
    (let ((next-byte (sys:%multibus-read-8 (+ buffer-base 2 i))))
      (setf result (dpb next-byte (byte 8 0) (ash result 8.)))))
  result)

(defun read-frame-header-source (buffer-base &aux (result 0))
  "returns the source address of the packet in the hardware buffer"
  (dotimes (i 6)
    (let ((next-byte (sys:%multibus-read-8 (+ buffer-base 8 i))))
      (setf result (dpb next-byte (byte 8 0) (ash result 8.)))))
  result)

(defun read-frame-header-type (buffer-base)
  "returns the type of the packet in the hardware buffer"
  (dpb (sys:%multibus-read-8 (+ buffer-base 14.))
       (byte 8 8)
       (sys:%multibus-read-8 (+ buffer-base 15.))))

(defun 3com-send-int-pkt (int-pkt e-source e-dest e-type n-16-bit-words)
  (net:assure-safe-int-pkt int-pkt)

  (cond ((lambda-ucode-enabled-p)
         (setf (fill-pointer int-pkt) (max (fill-pointer int-pkt) 30.))
         ;the microcode stores the address "right", we should fix the
         ;macrocode conventions someday
         (setf (array-leader int-pkt sys:%chaos-leader-csr-1)
               (+ (ash (ldb (byte 8 24.) e-dest) 16.)
                  (ash (ldb (byte 8 32.) e-dest)  8.)
                  (ldb (byte 8 40.) e-dest)))
         (setf (array-leader int-pkt sys:%chaos-leader-csr-2)
               (+ (ash (ldb (byte 8  0.) e-dest) 16.)
                  (ash (ldb (byte 8  8.) e-dest)  8.)
                  (ldb (byte 8 16.) e-dest)))
         (setf (array-leader int-pkt sys:%chaos-leader-bit-count) (swap-two-bytes e-type))
         (int-pkt-list-put int-pkt net:int-transmit-list-pointer)
         (sys:%chaos-wakeup)
         t)
        (t
         (3com-send-int-pkt-via-multibus int-pkt e-source e-dest e-type n-16-bit-words))))

(defun 3com-send-int-pkt-via-multibus (int-pkt e-source e-dest e-type n-16-bit-words
                                       &aux csr start-time)
  (declare (fixnum csr))
  (unwind-protect
      (let* ((physical-size (max (* n-16-bit-words 2) 60.))     ;don't send runts
             (offset (the fixnum (- 3com-me-buffer-size physical-size)))
             (beginning-address (the fixnum (+ 3com-transmit-buffer offset))))
        (tagbody
            (setq start-time (sys:%fixnum-microsecond-time))
         l  (setq csr (read-3com-csr))
            (cond ((ldb-test jam csr)
                   (reset-jam)
                   (go l))
                  ((ldb-test tbsw csr)
                   (cond ((> (time-difference (sys:%fixnum-microsecond-time)
                                              start-time)
                             100000.)
                          (incf (3com-transmit-time-outs *3com-ethernet-interface*))
                          (return-from 3com-send-int-pkt-via-multibus nil))     ;give up.
                         (t (go l))))))
        (without-interrupts
          (cond ((ldb-test tbsw csr)
                 (incf (3com-buffer-not-available *3com-ethernet-interface*)))
                (t
                 (do ((adr beginning-address (the fixnum (+ adr 2)))
                      (from-index 0 (the fixnum (1+ from-index)))
                      (n n-16-bit-words (the fixnum (1- n))))
                     ((zerop n))
                   (declare (fixnum adr from-index n))
                   (let ((data (aref int-pkt from-index)))
                     (declare (fixnum data))
                     (sys:%multibus-write-8 adr (ldb (byte 8 0) data))
                     (sys:%multibus-write-8 (the fixnum (1+ adr)) (ldb (byte 8 8) data))))
                 (write-lambda-3com-frame-header 3com-transmit-buffer
                                                 offset
                                                 e-source
                                                 e-dest
                                                 e-type)
                 (write-3com-csr (dpb 1 tbsw 3com-csr-background-bits)))))
        t)
    (free-packet int-pkt)))

(defun 3com-packet-ready ()
  (and (i-own-3com)
       (if (lambda-ucode-enabled-p)
           (net:int-receive-list)
         (not (= 3 (ldb a+b-bsw (the fixnum (read-3com-csr))))))))

(defun 3com-get-next-packet ()
  (declare (values packet type source destination broadcast-p))
  (and (i-own-3com)
       (if (lambda-ucode-enabled-p)
           (3com-get-next-pkt-via-ucode)
         (3com-get-next-pkt-via-multibus))))

(defun 3com-get-next-pkt-via-ucode (&aux int-pkt byte-count)
  (declare (values packet type source destination broadcast-p))
  (when (setq int-pkt (int-pkt-list-get net:int-receive-list-pointer))
    (setf (net:int-pkt-thread int-pkt) nil)
    (let* (;;The int-pkt-csr2 is a fixnum that has the buffer-header as the low 16 bits, and the first
           ;;9 bits of the destination address above them.  The first address bit is the multicast bit.
           (int-csr2 (net:int-pkt-csr-2 int-pkt))
           (csr2 (swap-two-bytes int-csr2))
           (type (swap-two-bytes (net:int-pkt-bit-count int-pkt)))
           (multicast-p (ldb-test (byte 1 16) int-csr2)))
      (unless (zerop (logand %%3com-buffer-header-error csr2))
        (incf (3com-fcs-errors *3com-ethernet-interface*))
        (return-from 3com-get-next-pkt-via-ucode nil))
      (setf (net:int-pkt-csr-2 int-pkt) csr2)
      (setf (net:int-pkt-bit-count int-pkt) type)
      ;;Header: 2 bytes buffer header 14 bytes ethernet header.  Trailer: 4 bytes FCS
      (setq byte-count (- (ldb %%3com-buffer-header-nbytes csr2) 20.))
      (cond
        ((not (plusp byte-count))
         (incf (3com-hdr-errors *3com-ethernet-interface*))
         (return-from 3com-get-next-pkt-via-ucode nil))
        (t
         (setf (fill-pointer int-pkt) (ceiling byte-count 2))
         (values int-pkt                          ; the packet
                 (net:int-pkt-bit-count int-pkt)  ; the type code
                 0                                ; the sender
                 0                                ; the receiver
                 multicast-p                      ; only multicast address we're enabled on is broadcast address
                 ))))))

(defun 3com-get-next-pkt-via-multibus (&aux (csr (read-3com-csr)))
  (declare (values packet type source destination broadcast-p))
  (multiple-value-bind (buffer BSW)
      (if (ldb-test rbba csr)
          (cond ((zerop (ldb absw csr))
                 (values 3com-buffer-a ABSW))
                ((zerop (ldb bbsw csr))
                 (values 3com-buffer-b BBSW))
                (t (return-from 3com-get-next-pkt-via-multibus nil)))
        (cond ((zerop (ldb bbsw csr))
               (values 3com-buffer-b BBSW))
              ((zerop (ldb absw csr))
               (values 3com-buffer-a ABSW))
              (t (return-from 3com-get-next-pkt-via-multibus nil))))
    (multiple-value-bind (int-pkt type source dest broadcast-p)
        (multibus-ethernet-receive-buffer buffer)
      (arm-3com-receive-buffer BSW)
      (values int-pkt
              type
              source
              dest
              broadcast-p
              ))))

(defun multibus-ethernet-receive-buffer (buffer-base &aux int-pkt)
  (declare (values packet type source destination broadcast-p))
  (let* ((buffer-header (read-3com-buffer-header buffer-base))
         ;;Header: 2 bytes buffer header 14 bytes ethernet header.  Trailer: 4 bytes FCS
         (nbytes (- (ldb %%3com-buffer-header-nbytes buffer-header) 20.))
         (type (read-frame-header-type buffer-base))
         (source (read-frame-header-source buffer-base))
         (dest (read-frame-header-destination buffer-base)))
    (unless (zerop (logand %%3com-buffer-header-error buffer-header))
      (incf (3com-fcs-errors *3com-ethernet-interface*))
      (return-from multibus-ethernet-receive-buffer nil))
    (unless (plusp nbytes)
      (incf (3com-hdr-errors *3com-ethernet-interface*))
      (return-from multibus-ethernet-receive-buffer nil))
    (when (setq int-pkt (allocate-packet-for-receive *3com-ethernet-interface*))
      (do ((adr (the fixnum (+ buffer-base 16.))
                (the fixnum (+ adr 2)))
           (nwords (ceiling nbytes 2))
           (wd-count 0 (the fixnum (1+ wd-count))))
          ((>= wd-count nwords))
        (declare (fixnum adr wd-count))
        (setf (aref int-pkt wd-count) (dpb (sys:%multibus-read-8 (the fixnum (+ adr 1)))
                                           (byte 8 8)
                                           (sys:%multibus-read-8 adr))))
      (setf (fill-pointer int-pkt) (ceiling nbytes 2))
      (values int-pkt
              type
              source
              dest
              ;;The following SHOULD work, but the 3com board is broken...
              ;;   (ldb-test %%3com-buffer-header-broadcast buffer-header)
              (= dest *ethernet-broadcast-address*)
              ))))

(defun setup-3com (tag &aux alist)
  (when (and (eq si:processor-type-code si:lambda-type-code)
             (i-own-3com))

    (when *3com-ethernet-interface*
      (setq alist (3com-protocol-alist *3com-ethernet-interface*))
      (funcall *3com-ethernet-interface* :close))

    (setq *3com-ethernet-interface*
          (make-3com-interface :tag tag
                               :interface :3com
                               :keyword :ethernet
                               :hardware-type net:arp-ethernet-hardware
                               :address-length 6
                               :address 0
                               :broadcast-address *ethernet-broadcast-address*
                               :minimum-data-length 46.
                               :maximum-data-length 1500.
                               :sent-header-length 0
                               :rcvd-header-length 0
                               :sent-trailer-length 0
                               :rcvd-trailer-length 0
                               :loopback t
                               :protocol-alist alist
                               :reset-function '3com-reset
                               :enable-function '3com-enable
                               :disable-function '3com-disable
                               :packet-ready-function '3com-packet-ready
                               :get-next-packet-function '3com-get-next-packet
                               :send-function  '3com-send-int-pkt
                               :gauge-name "3Com"
                               ))

    (funcall *3com-ethernet-interface* :open)
    (funcall *3com-ethernet-interface* :enable)))

(defun print-3com-csr ()
  (let ((csr (read-3com-csr)))
    (format t "~%Buf B belongs to ether: ~40t~d" (ldb bbsw csr))
    (format t "~%Buf A belongs to ether: ~40t~d" (ldb absw csr))
    (format t "~%Transmit buf belongs to ether: ~40t~d" (ldb tbsw csr))
    (format t "~%Jam: ~40t~d" (ldb jam csr))
    (format t "~%RAM valid: ~40t~d" (ldb amsw csr))
    (format t "~%A/B buffer ordering: ~40t~d" (ldb rbba csr))
    (format t "~%reset: ~40t~d" (ldb reset csr))        ;probably write only
    (format t "~%Enable interrupts on B buffer: ~40t~d" (ldb binten csr))
    (format t "~%Enable interrupts on A buffer: ~40t~d" (ldb ainten csr))
    (format t "~%Enable interrupts on transmit buffer: ~40t~d" (ldb tinten csr))
    (format t "~%Enable interrupts on JAM: ~40t~d" (ldb jinten csr))
    (format t "~%pa: ~40t~d" (ldb pa csr))))

(defun print-3com-address-ram ()
  (unless (i-own-3com)
    (error "no ethernet present"))
  (dotimes (i 6)
    (format t "~16r " (sys:%multibus-read-8 (+ 3com-address-ram i)))))

(defun print-3com-address-rom ()
  (unless (i-own-3com)
    (error "no ethernet present"))
  (dotimes (i 6)
    (format t "~16r " (sys:%multibus-read-8 (+ 3com-address-rom i)))))

(defun print-rcv-buffer (buf)
  (let ((buf-base (case buf
                    ((a 0) 3com-buffer-a)
                    ((b 1) 3com-buffer-b))))
    (dotimes (i 24)
      (format t "~16r " (sys:%multibus-read-8 (+ buf-base i))))))

(defun print-xmit-buffer ()
  (let ((offset (dpb (sys:%multibus-read-8  3com-transmit-buffer)
                     (byte 8 8)
                     (sys:%multibus-read-8 (1+ 3com-transmit-buffer)))))
    (format t "offset = ~s ~:* #x~16r; " offset)
    (dotimes (i 24)
      (format t "~16r " (sys:%multibus-read-8 (+ 3com-transmit-buffer offset i))))))

;;; -*- Mode:LISP; Package:ETHERNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

;;; This file contains routines pertaining specifically to the Explorer NuBus Ethernet interface

(defvar explorer-ethernet-interface nil "The Explorer Ethernet interface")

(defun explorer-send-int-pkt (int-pkt e-source e-dest e-type n-16-bit-words)
  (net:assure-safe-int-pkt int-pkt)
  (unwind-protect
      (si:nubus-transmit-ethernet-16b-array e-source
                                      e-dest
                                      int-pkt
                                      (max n-16-bit-words 64.)
                                      (swap-two-bytes e-type))
    (free-packet int-pkt)))

(defun explorer-get-next-packet (&aux int-pkt)
  (declare (values packet type source destination))
  (declare (special si::controller))
  (without-interrupts
    (when (setq int-pkt (allocate-packet-for-receive explorer-ethernet-interface))
      (multiple-value-bind (destination source type ignore byte-count)
          (si:receive-frame si:controller int-pkt)
        (setf (fill-pointer int-pkt) (ceiling byte-count 2))
        (values int-pkt                         ; the packet
                (swap-two-bytes type)           ; the type code
                destination                     ; the sender
                source                          ; the receiver
                (= destination *ethernet-broadcast-address*)
                )))))

(defun explorer-reset (stream)
  (declare (ignore stream))
  (si:nubus-ethernet-reset))

(defun setup-explorer-ethernet (tag &aux alist)
  (when (eq si:processor-type-code si:explorer-type-code)

    (when explorer-ethernet-interface
      (setq alist (net:ni-protocol-alist explorer-ethernet-interface))
      (funcall explorer-ethernet-interface :close))

    (setq explorer-ethernet-interface
          (net:make-network-interface :tag tag
                                      :interface :explorer
                                      :keyword :ethernet
                                      :hardware-type net:arp-ethernet-hardware
                                      :address-length 6
                                      :address si:my-ethernet-address
                                      :broadcast-address *ethernet-broadcast-address*
                                      :minimum-data-length 46.
                                      :maximum-data-length 1500.
                                      :sent-header-length 0
                                      :rcvd-header-length 0
                                      :sent-trailer-length 0
                                      :rcvd-trailer-length 0
                                      :loopback t
                                      :protocol-alist alist
                                      :reset-function 'explorer-reset
                                      :packet-ready-function 'si:NEC-Pkt-Available
                                      :get-next-packet-function 'explorer-get-next-packet
                                      :send-function  'explorer-send-int-pkt
                                      :gauge-name "Explorer"
                                      ))

    (funcall explorer-ethernet-interface :open)
    (funcall explorer-ethernet-interface :enable)))

(add-initialization "Start EXPLORER interface" '(setup-explorer-ethernet "ONE") :network-driver)

;;; -*- Mode:LISP; Package:NETWORK; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *network-interfaces*
           network-interface
           add-address-info))

(defstream network-interface
           ()
           "NI-"
  ;The structure describing a particular network interface
  (tag nil)                                     ; A tag to identify this particular network interface
  (interface nil)                               ; A keyword to identify the type of interface
  (keyword nil)                                 ; A keyword to identify the type of network
  (opened nil)                                  ; The interface has been initialized and is in global list
  (enabled nil)                                 ; The interface is available for use
  (hardware-type nil)                           ; A hardware type code, as used by ARP
  (address-length nil)                          ; The length in bytes of the address of this interface
  (address nil)                                 ; The hardware address of this interface on the network
  (broadcast-address nil)                       ; The hardware address used for broadcast.  NIL if can't
  (sent-header-length 0)                        ; Space reserved for the link level header in sent buffers
  (rcvd-header-length 0)                        ; Space taken by the link level header in received buffers
  (sent-trailer-length 0)                       ; Space reserved for the link level trailer in sent buffers
  (rcvd-trailer-length 0)                       ; Space taken by the link level trailer in received buffers
  (minimum-data-length 0)                       ; minimum packet length on this network
  (maximum-data-length 0)                       ; maximum packet length on this network
  (protocol-alist nil)                          ; Association list: (protocol-keyword . receive-function)
  (network-alist nil)                           ; Association list: (protocol-keyword . network-number)
  (address-alist nil)                           ; Association list: (protocol-keyword . (addresses))
  (address-translations nil)                    ; ARP table for this network interface
  (loopback nil)                                ; T if software loopback needed
  (point-to-point nil)                          ; T if point-to-point: must use address-translation-list
  (reset-function nil)                          ; function to reset interface
  (enable-function nil)                         ; function to enable interface
  (disable-function nil)                        ; function to disable interface
  (packet-ready-function nil)                   ; function that returns t if packet available
  (get-next-packet-function nil)                ; function that gets next packet
  (send-function nil)                           ; function to send a packet onto the network
  (need-receive-buffer nil)                     ; t if non-blocking allocate failed
  (reserved-receive-buffer nil)                 ; the buffer allocated by scheduler for receive
  (allocation-failures 0)                       ; statistic: how many times int-pkts not available
  (statistics-block (make-statistics-block))    ; Network statistics block maintained at clock level
  (packets-sent-discarded 0)                    ; statistic: sent packets discarded
  (packets-received-discarded 0)                ; statistic: received packets discarded
  (bytes-sent-discarded 0)                      ; statistic: sent bytes discarded
  (bytes-received-discarded 0)                  ; statistic: received bytes discarded
  (protocols-not-understood nil)                ; list of unhandled protocols received from network
  (active-gauges nil)                           ; list of gauges in control panel
  (inactive-gauges nil)                         ; list of gauges not currently in control panel
  (gauge-name nil)                              ; the name of this interface for its gauges
  )

(defsubst ni-packets-sent (ni)
  (aref (ni-statistics-block ni) STAT-PS STAT-CURR))
(defsubst ni-packets-received (ni)
  (aref (ni-statistics-block ni) STAT-PR STAT-CURR))
(defsubst ni-bytes-sent (ni)
  (aref (ni-statistics-block ni) STAT-BS STAT-CURR))
(defsubst ni-bytes-received (ni)
  (aref (ni-statistics-block ni) STAT-BR STAT-CURR))

(defvar *network-interfaces* nil
  "The global list of network interfaces known to this machine")

(defvar *network-interface-fifo* (make-fifo)
  "A copy of *network-interfaces* rotated by the network receiver process")

(defvar network-driver-initialization-list nil "The initialization list for network drivers")

(defop (network-interface :open) nil
  (unless (ni-opened self)
    (dolist (ni *network-interfaces*)
      (when (equal (ni-tag self) (ni-tag ni))
        (send ni :close)
        (return t)))
    (without-interrupts
      (push self *network-interfaces*)
      (push-fifo self *network-interface-fifo*))
    (when (ni-reset-function self)
      (funcall (ni-reset-function self) self))
    (add-network-statistics-block (ni-statistics-block self))
    (setf (ni-enabled self) nil)
    (setf (ni-opened self) t)))

(defop (network-interface :close) nil
  (when (ni-opened self)
    (when (ni-enabled self)
      (send self :disable))
    (delete-network-statistics-block (ni-statistics-block self))
    (setf (ni-opened self) nil)
    (send self :kill-gauges)
    (without-interrupts
      (setq *network-interfaces* (delete self *network-interfaces*))
      (remove-from-fifo self *network-interface-fifo*)))
  t)

(defop (network-interface :reset) nil
  (when (ni-opened self)
    (when (ni-reset-function self)
      (funcall (ni-reset-function self) self))))

(defop (network-interface :enable) (&rest arguments)
  (when (ni-opened self)
    (cond ((ni-enabled self))
          ((ni-enable-function self)
           (and (apply (ni-enable-function self) self arguments)
                (setf (ni-enabled self) t)))
          (t
           (setf (ni-enabled self) t)))))

(defop (network-interface :disable) (&rest arguments)
  (when (ni-enabled self)
    (setf (ni-enabled self) nil)
    (setf (ni-need-receive-buffer self) nil)
    (when (ni-reserved-receive-buffer self)
      (free-packet (ni-reserved-receive-buffer self))
      (setf (ni-reserved-receive-buffer self) nil))
    (when (ni-disable-function self)
      (apply (ni-disable-function self) self arguments))))

(defop (network-interface :print-self) (stream &optional ignore ignore)
  (system:printing-random-object (self stream :type :no-pointer)
    (format stream "~S (~:[closed~;open~] ~:[dis~;en~]abled) ~A ~A address ~X"
            (ni-tag self)
            (ni-opened self)
            (ni-enabled self)
            (ni-interface self)
            (ni-keyword self)
            (ni-address self))))

(defop (network-interface :set-gauges) (gauge-list)
  "Set the active network gauges for this network interface"
  (set-gauges (ni-statistics-block self)
              (locf (ni-active-gauges self))
              (locf (ni-inactive-gauges self))
              (ni-gauge-name self)
              gauge-list))

(defop (network-interface :make-gauges) (&optional (gauge-list '(:apr :aps :abr :abs)))
  "Add selected gauges for this network interface"
  (add-gauges (ni-statistics-block self)
              (locf (ni-active-gauges self))
              (locf (ni-inactive-gauges self))
              (ni-gauge-name self)
              gauge-list))

(defop (network-interface :kill-gauges) (&optional (gauge-list '(:ipr :apr :ips :aps :ibr :abr :ibs :abs)))
  "Delete selected gauges from a network interface.  Default is to delete all gauges"
  (delete-gauges (locf (ni-active-gauges self))
                 (locf (ni-inactive-gauges self))
                 gauge-list))

;;; the structure of one entry in the address-translations list
(defstruct (address-translations
             (:type :list)
             (:conc-name "AT-"))
  protocol-address                              ;The Network Protocol address (e.g. Chaos or Internet)
  hardware-address                              ;The Hardware Address (e.g. Ethernet)
  protocol                                      ;The Keyword for the Network Protocol
  on-behalf-of)                                 ;A list of addresses that this info is valid for

(defun add-address-info (address protocol interface hardware-address &optional on-behalf-of &aux adr-info)
  (when (assoc protocol (ni-address-alist interface) :test #'eq)
    (setq adr-info (without-interrupts
                     (dolist (at (ni-address-translations interface))
                       (when (and (= address (at-protocol-address at))
                                  (eq protocol (at-protocol at)))
                         (return at)))))
    (unless adr-info
      (setq adr-info (make-address-translations))
      (push adr-info (ni-address-translations interface)))
    (setf (at-protocol-address adr-info) address)
    (setf (at-hardware-address adr-info) hardware-address)
    (setf (at-protocol adr-info) protocol)
    (when on-behalf-of
      (pushnew on-behalf-of (at-on-behalf-of adr-info)))))

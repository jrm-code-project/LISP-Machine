;;; -*- Mode:LISP; Package:NETWORK; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *network-protocol-streams*
           network-protocol
           make-network-protocol
           enable-network-protocols
           disable-network-protocols))

(defstream network-protocol
           ()
           "NP-"
  (keyword nil)                                 ; keyword for this protocol family
  (opened nil)                                  ; t if stream ready for business
  (enabled nil)                                 ; t if processing enabled
  (protocol nil)                                ; protocol types for ARP
  (addresses nil)                               ; network address list.  If NIL, no ARP
  (address-length 0)                            ; length of a network address
  (big-endian-address-p nil)                    ; t if address extracted low-byte first
  (reset-function nil)                          ; function to reset processing for this protocol
  (close-function nil)                          ; function to call when this protocol is being closed
  (enable-function nil)                         ; function to enable processing for this protocol
  (disable-function nil)                        ; function to disable processing for this protocol
  (interrupt-function nil)                      ; function called at interrupt level when packet received
                                                ; if NIL, packets saved on packet-list
  (packet-list nil)                             ; list of packets awaiting :receive
  (special-address-function nil)                ; a function to call to check if an address is special
  (address-printer nil)                         ; a function to call to return string with canonical address
  (get-header-function nil)                     ; a function to call to get a packet header
  (free-header-function nil)                    ; a function to call to free a packet header
  (send-packet-function nil)                    ; a function to call to send a packet with header
  (broadcast-packet-function nil)               ; a function to call to broadcast a packet with header
  (packet-length-function nil)                  ; a function to call to figure packet length (for stats)
  (statistics-block (make-statistics-block))    ; Network statistics block maintained at clock level
  (packets-sent-discarded 0)                    ; statistic: sent packets discarded
  (packets-received-discarded 0)                ; statistic: received packets discarded
  (bytes-sent-discarded 0)                      ; statistic: sent bytes discarded
  (bytes-received-discarded 0)                  ; statistic: received bytes discarded
  (protocols nil)                               ; alist of transport protocols above this network protocol
  (protocols-not-understood nil)                ; list of unrecognized protocols
  (active-gauges nil)                           ; list of gauges in control panel
  (inactive-gauges nil)                         ; list of gauges not currently in control panel
  (gauge-name nil)                              ; the name of this protocol for its gauges
  )

(defsubst np-packets-sent (np)
  (aref (np-statistics-block np) STAT-PS STAT-CURR))
(defsubst np-packets-received (np)
  (aref (np-statistics-block np) STAT-PR STAT-CURR))
(defsubst np-bytes-sent (np)
  (aref (np-statistics-block np) STAT-BS STAT-CURR))
(defsubst np-bytes-received (np)
  (aref (np-statistics-block np) STAT-BR STAT-CURR))

(defvar *network-protocol-streams* nil "List of open network-protocols")

(defop (network-protocol :update-methods) nil
  (create-included-methods (named-structure-p self)))

(defop (network-protocol :open) (address subnet)
  "open a network-protocol."
  (unless (np-opened self)
    (dolist (stream *network-protocol-streams*)
      (when (eq (np-keyword stream) (np-keyword self))
        (send stream :close)
        (return t)))
    (push self *network-protocol-streams*)
    (let ((func (let ((stream self))
                  #'(lambda (int-pkt interface sender receiver broadcast-p)
                      (network-protocol-packet-receiver int-pkt interface stream sender receiver broadcast-p))))
          list
          result)
      (labels ((make-address-form (l)
                 (cond ((consp l)
                        (cons (make-address-form (car l))
                              (make-address-form (cdr l))))
                       ((numberp l) l)
                       ((eq l :address) address)
                       ((eq l :subnet) subnet)
                       (t nil))))
        (dolist (ni *network-interfaces*)
          (dolist (item *protocol-configuration*)
            (when (and (eq (ni-keyword ni) (first item))
                       (setq list (assoc (np-keyword self) (cdr item))))
              (pushnew (cons (first item) (second list)) (np-protocol self) :test #'equal)
              (delete-from-alist (np-keyword self) (ni-network-alist ni))
              (push (cons (np-keyword self) (make-address-form (third list))) (ni-network-alist ni))
              (delete-from-alist (np-keyword self) (ni-address-alist ni))
              (let ((address (make-address-form (fourth list))))
                (push (cons (np-keyword self) (ncons address)) (ni-address-alist ni))
                (setq result (nconc result (ncons address))))
              (delete-from-alist (second list) (ni-protocol-alist ni))
              (push (cons (second list) func) (ni-protocol-alist ni))
              (return t)))))
      (setf (np-addresses self) result))
    (when (np-addresses self)
      (setf (get (np-keyword self) 'si::address-unparser) (np-address-printer self)))
    (add-network-statistics-block (np-statistics-block self))
    (setf (np-opened self) t)))

(defop (network-protocol :close) (&optional abort)
  (when (np-opened self)
    (when (np-enabled self)
      (send self :disable))
    (when (np-close-function self)
      (funcall (np-close-function self) self abort))
    (dolist (prot (np-protocols self))
      (send (cdr prot) :close))
    (setf (np-opened self) nil)
    (send self :kill-gauges)
    (delete-network-statistics-block (np-statistics-block self))
    (without-interrupts
      (setq *network-protocol-streams* (delete self *network-protocol-streams*)))
    (dolist (ni *network-interfaces*)
      (delete-from-alist (np-keyword self) (ni-network-alist ni))
      (delete-from-alist (np-keyword self) (ni-address-alist ni))
      (let ((type (cdr (assoc (ni-keyword ni) (np-protocol self) :test #'eq))))
        (when type
          (delete-from-alist type (ni-protocol-alist ni)))))))

(defop (network-protocol :reset) nil
  (when (np-opened self)
    (and (np-reset-function self)
         (funcall (np-reset-function self) self))))

(defop (network-protocol :enable) nil
  (when (np-opened self)
    (unless (np-enabled self)
      (and (np-enable-function self)
           (funcall (np-enable-function self) self))
      (setf (np-enabled self) t))))

(defop (network-protocol :disable) nil
  (when (np-enabled self)
    (setf (np-enabled self) nil)
    (dolist (x (np-packet-list self))
      (free-packet (car x)))
    (setf (np-packet-list self) nil)
    (and (np-disable-function self)
         (funcall (np-disable-function self) self))))

(defop (network-protocol :send) (packet length interface address &optional hang-p)
  (declare (ignore hang-p))
  (when (and (ni-enabled interface) (ni-loopback interface) (eql address (ni-address interface)))
    (setq interface *loopback-interface*))
  (let* ((int-pkt (original-array packet))
         (header (ni-sent-header-length interface))
         (trailer (ni-sent-trailer-length interface))
         (total-length (+ length header trailer)))
    (cond ((and (np-enabled self) (ni-enabled interface))
           (incf (np-packets-sent self))
           (incf (np-bytes-sent self) length)
           (setf (fill-pointer int-pkt) (ceiling total-length 2))
           (incf (ni-packets-sent interface))
           (incf (ni-bytes-sent interface) total-length)
           (let ((type (cdr (assoc (ni-keyword interface) (np-protocol self) :test #'eq))))
             ;;***for print-int-pkt-status...
             (setf (int-pkt-bit-count int-pkt) (swap-two-bytes type))
             (funcall (ni-send-function interface)
                      int-pkt
                      (ni-address interface)
                      address
                      type
                      (ceiling total-length 2))))
          (t
           (incf (np-packets-sent-discarded self))
           (incf (np-bytes-sent-discarded self) length)
           (incf (ni-packets-sent-discarded interface))
           (incf (ni-bytes-sent-discarded interface) length)
           (free-packet packet)
           nil))                                ;Didn't send it
    ))

(defop (network-protocol :receive) (&optional no-hang-p)
  (declare (values packet interface))
  (cond ((np-enabled self)
         (or (np-packet-list self)              ; packets waiting...
             no-hang-p                          ; or don't care...
             (np-interrupt-function self)       ; or interrupt function (NEVER packets waiting!)
             (process-wait "Link Level Packet" #'(lambda (x) (np-packet-list x)) self))
         (without-interrupts
           (let ((pair (pop (np-packet-list self))))
             (values (car pair) (cdr pair)))))))

(defop (network-protocol :print-self) (stream &optional ignore ignore)
  (sys:printing-random-object (self stream :type :no-pointer)
    (format stream
            "~A (~:[closed~;open~] ~:[dis~;en~]abled)"
            (np-keyword self)
            (np-opened self)
            (np-enabled self))))

(defop (network-protocol :set-gauges) (gauge-list)
  "Set the active network gauges for this network protocol"
  (set-gauges (np-statistics-block self)
              (locf (np-active-gauges self))
              (locf (np-inactive-gauges self))
              (np-gauge-name self)
              gauge-list))

(defop (network-protocol :make-gauges) (&optional (gauge-list '(:apr :aps :abr :abs)))
  "Add selected gauges for this network protocol"
  (add-gauges (np-statistics-block self)
              (locf (np-active-gauges self))
              (locf (np-inactive-gauges self))
              (np-gauge-name self)
              gauge-list))

(defop (network-protocol :kill-gauges) (&optional (gauge-list '(:ipr :apr :ips :aps :ibr :abr :ibs :abs)))
  "Delete selected gauges from a network protocol.  Default is to delete all gauges"
  (delete-gauges (locf (np-active-gauges self))
                 (locf (np-inactive-gauges self))
                 gauge-list))

(defop (network-protocol :reset-stats) ()
  (without-interrupts
    (setf (np-bytes-sent self) 0)
    (setf (np-bytes-received self) 0)
    (setf (np-packets-sent self) 0)
    (setf (np-packets-received self) 0)
    ))

(defun network-protocol-packet-receiver (packet interface stream sender receiver broadcast-p)
  (let ((bytes (if (np-packet-length-function stream)
                   (funcall (np-packet-length-function stream) packet (/ (ni-rcvd-header-length interface) 2))
                 (* 2 (fill-pointer packet)))))
    (cond ((np-enabled stream)
           (incf (np-packets-received stream))
           (incf (np-bytes-received stream) bytes)
           (if (np-interrupt-function stream)
               (funcall (np-interrupt-function stream) packet interface stream sender receiver broadcast-p)
             (without-interrupts
               (setf (np-packet-list stream)
                     (nconc (np-packet-list stream)
                            (ncons (list packet interface sender receiver)))))))
          (t
           (incf (np-packets-received-discarded stream))
           (incf (np-bytes-received-discarded stream) bytes)
           (incf (ni-packets-received-discarded interface))
           (incf (ni-bytes-received-discarded interface) bytes)
           (free-packet packet)))
    ))

(defun disable-network-protocols ()
  (dolist (np *network-protocol-streams*)
    (send np :disable)))

(defun enable-network-protocols ()
  (dolist (np *network-protocol-streams*)
    (send np :enable)))

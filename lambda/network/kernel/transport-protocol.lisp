;;; -*- Mode:LISP; Package:NETWORK; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( transport-protocol))

(defstream transport-protocol
           ()
           "TP-"
  (network-protocol nil)                        ; the link-level-stream of the network protocol owning this
  (keyword nil)                                 ; keyword identifying this transport protocol
  (type nil)                                    ; network protocol's type code for this transport protocol
  (opened nil)                                  ; t if stream ready for business
  (enabled nil)                                 ; t if processing enabled
  (reset-function nil)                          ; function to reset processing for this protocol
  (close-function nil)                          ; function to call when this protocol is being closed
  (enable-function nil)                         ; function to enable processing for this protocol
  (disable-function nil)                        ; function to disable processing for this protocol
  (receive-function nil)                        ; if non-NIL, function called when a packet arrives
  (packet-list (make-fifo))                     ; else, packets pushed onto this list
  (receive-buffers nil)                         ; list of buffers to receive network packets
  (statistics-block (make-statistics-block))    ; Network statistics block maintained at clock level
  (packets-sent-discarded 0)                    ; statistic: sent packets discarded (couldn't route)
  (packets-received-discarded 0)                ; statistic: received packets discarded (no rcv buffer)
  (bytes-sent-discarded 0)                      ; statistic: sent bytes discarded
  (bytes-received-discarded 0)                  ; statistic: received bytes discarded
  (broadcast-packets-sent 0)                    ; Number of broadcast packets sent
  (broadcast-packets-received 0)                ; Number of broadcast packets received
  (active-gauges nil)                           ; list of gauges in control panel
  (inactive-gauges nil)                         ; list of gauges not currently in control panel
  (gauge-name nil)                              ; the name of this protocol for its gauges
  )

(defsubst tp-packets-sent (tp)
  (aref (tp-statistics-block tp) STAT-PS STAT-CURR))
(defsubst tp-packets-received (tp)
  (aref (tp-statistics-block tp) STAT-PR STAT-CURR))
(defsubst tp-bytes-sent (tp)
  (aref (tp-statistics-block tp) STAT-BS STAT-CURR))
(defsubst tp-bytes-received (tp)
  (aref (tp-statistics-block tp) STAT-BR STAT-CURR))

(defop (transport-protocol :update-methods) nil
  (create-included-methods (named-structure-p self)))

(defop (transport-protocol :open) nil
  "open a transport-protocol."
  (unless (tp-opened self)
    (when (tp-network-protocol self)
      (let ((stream (cdr (assoc (tp-type self) (np-protocols (tp-network-protocol self))))))
        (when stream
          (send stream :close)))
      (push (cons (tp-type self) self) (np-protocols (tp-network-protocol self))))
    (add-network-statistics-block (tp-statistics-block self))
    (setf (tp-opened self) t)))

(defop (transport-protocol :close) (&optional args)
  (when (tp-opened self)
    (when (tp-enabled self)
      (send self :disable))
    (when (tp-close-function self)
      (apply (tp-close-function self) self args))
    (send self :kill-gauges)
    (delete-network-statistics-block (tp-statistics-block self))
    (setf (tp-opened self) nil)
    (when (tp-network-protocol self)
      (without-interrupts
        (delete-from-alist (tp-type self) (np-protocols (tp-network-protocol self)))))
    t))

(defop (transport-protocol :reset) nil
  (when (tp-opened self)
    (and (tp-reset-function self)
         (funcall (tp-reset-function self) self))))

(defop (transport-protocol :enable) nil
  (when (tp-opened self)
    (unless (tp-enabled self)
      (and (tp-enable-function self)
           (funcall (tp-enable-function self) self))
      (setf (tp-enabled self) t))))

(defop (transport-protocol :disable) nil
  (when (tp-enabled self)
    (setf (tp-enabled self) nil)
    (and (tp-disable-function self)
         (funcall (tp-disable-function self) self))))

(defop (transport-protocol :print-self) (stream &optional ignore ignore)
  (si:printing-random-object (self stream :type :no-pointer)
    (format stream
            "~A (~:[closed~;open~] ~:[dis~;en~]abled)"
            (tp-keyword self)
            (tp-opened self)
            (tp-enabled self))))

(defop (transport-protocol :set-gauges) (gauge-list)
  "Set the active transport gauges for this transport protocol"
  (set-gauges (tp-statistics-block self)
              (locf (tp-active-gauges self))
              (locf (tp-inactive-gauges self))
              (tp-gauge-name self)
              gauge-list))

(defop (transport-protocol :make-gauges) (&optional (gauge-list '(:apr :aps :abr :abs)))
  "Add selected gauges for this transport protocol"
  (add-gauges (tp-statistics-block self)
              (locf (tp-active-gauges self))
              (locf (tp-inactive-gauges self))
              (tp-gauge-name self)
              gauge-list))

(defop (transport-protocol :kill-gauges) (&optional (gauge-list '(:ipr :apr :ips :aps :ibr :abr :ibs :abs)))
  "Delete selected gauges from a transport protocol.  Default is to delete all gauges"
  (delete-gauges (locf (tp-active-gauges self))
                 (locf (tp-inactive-gauges self))
                 gauge-list))

(defop (transport-protocol :send) (buffers &rest args)
  (when (tp-enabled self)
    (let ((result (multiple-value-list
                    (apply (np-send-packet-function (tp-network-protocol self)) self buffers args)))
          (length (if (consp buffers)
                      (apply '+ (mapcar 'length buffers))
                    (length buffers))))
      (cond ((car result)
             (incf (tp-packets-sent self))
             (incf (tp-bytes-sent self) length))
            (t
             (incf (tp-packets-sent-discarded self))
             (incf (tp-bytes-sent-discarded self) length)))
      (values-list result))))

(defop (transport-protocol :broadcast) (buffers &rest args)
  (when (tp-enabled self)
    (let* ((function (np-broadcast-packet-function (tp-network-protocol self)))
           (result (when function
                     (multiple-value-list (apply function self buffers args))))
           (length (if (consp buffers)
                       (apply '+ (mapcar 'length buffers))
                     (length buffers))))
      (cond ((car result)
             (incf (tp-packets-sent self))
             (incf (tp-bytes-sent self) length)
             (incf (tp-broadcast-packets-sent self)))
            (t
             (incf (tp-packets-sent-discarded self))
             (incf (tp-bytes-sent-discarded self) length)))
      (values-list result))))

(defop (transport-protocol :receive) (buffer)
  (when (tp-enabled self)
    (push buffer (tp-receive-buffers self))
    t))

(defop (transport-protocol :packet-arrival) (buffer header local-p broadcast-p interface)
  (incf (tp-packets-received self))
  (incf (tp-bytes-received self) (fill-pointer buffer))
  (if (tp-receive-function self)
      (funcall (tp-receive-function self) self buffer header local-p broadcast-p interface)
    (push-fifo (list buffer header local-p broadcast-p interface) (tp-packet-list self))))

(defop (transport-protocol :get-header) (&rest args &aux func)
  (and (setq func (np-get-header-function (tp-network-protocol self)))
       (apply func args)))

(defop (transport-protocol :free-header) (&rest args &aux func)
  (and (setq func (np-free-header-function (tp-network-protocol self)))
       (apply func args)))

;;; -*- Mode:LISP; Package:NETWORK; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

;This file contains the Network Packet Receiver Process.
;It receives packets for all interfaces on net:*network-interfaces*

(defun get-next-packet-wait-function ()
  (dolist (ni (car *network-interface-fifo*))
    (and (ni-enabled ni)
         (cond ((ni-reserved-receive-buffer ni)
                ;;If we have a buffer, cool
                t)
               ((ni-need-receive-buffer ni)
                ;;If we need a buffer, it is only cool if we can now get one
                (when (setf (ni-reserved-receive-buffer ni) (allocate-packet nil))
                  (setf (ni-need-receive-buffer ni) nil)
                  t))
               ((ni-packet-ready-function ni)
                ;;Otherwise, call the packet ready function, if any
                (funcall (ni-packet-ready-function ni))))
         (return ni))))

(defun packet-receiver (&aux int-pkt func source destination broadcast-p type interface bytes)
  (loop
    ;; Sleep until a packet is available.  When one arrives, rotate interface list.
    (do ((ni (get-next-packet-wait-function)))
        (ni (rotate-fifo *network-interface-fifo*)
            (setq interface ni))
      (process-wait "Network Packet"
                    #'(lambda ()
                        (or ni (setq ni (get-next-packet-wait-function))))))

    (multiple-value-setq (int-pkt type source destination broadcast-p)
      (funcall (ni-get-next-packet-function interface)))

    (when int-pkt
      (setf (int-pkt-bit-count int-pkt) (swap-two-bytes type))  ;***for print-int-pkt-status...
      (setq bytes (* 2 (fill-pointer int-pkt)))
      (incf (ni-packets-received interface))
      (incf (ni-bytes-received interface) bytes)

      ;;***Assuming type codes are comparable with EQ (i.e. fixnums)...
      (setq func (cdr (assoc type (ni-protocol-alist interface) :test #'eq)))

      (cond (func
             (funcall func int-pkt interface source destination broadcast-p))
            (t
             (pushnew type (ni-protocols-not-understood interface))
             (incf (ni-packets-received-discarded interface))
             (incf (ni-bytes-received-discarded interface) bytes)
             (free-packet int-pkt)))
      (setq int-pkt nil))))

(defvar *receiver* nil "The network receiver process")

(defun start-receiver ()
  (if *receiver*
      (send *receiver* :flush)
    (setq *receiver* (make-process "Network Receiver"
                                   :simple-p nil
                                   :warm-boot-action nil
                                   :priority 25.)))
  (send *receiver* :preset 'packet-receiver)
  (send *receiver* :run-reason)
  *receiver*)

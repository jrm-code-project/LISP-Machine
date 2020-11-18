;;; -*- Mode:LISP; Package:TCP; Base:10 -*-
;;;
;;; Look at all ethernet packets with the Excelan board.
;;;

(defun netspy ()
  "Look at all ethernet packets with the Excelan board."
  (tcp-disable)
  (setq *netload-debug-p* nil)
  (start :enable-tcp nil)
  (LINK-NET-MODE :read-p t
                 :write-p t
                 :mode 'nmode-connect-promiscuous)
  (exos-stats)
  (netspy-loop))

(defun netspy-loop ()
  (terpri)
  (DO-FOREVER
    (when (send terminal-io :listen)
      (let ((c (send terminal-io :tyi)))
        (exos-stats :reset-p (char-equal c #\r))))
    (WITH-DMABUF (BUF)
      (NETSPY-RECEIVE BUF (LINK-RECV BUF)))))

(defvar *pkts* nil)
(defvar *pktn* 0)

(defun netspy-loop-2 ()
  (terpri)
  (WITH-DMABUF (BUF)
    (DO-FOREVER
      (when (send terminal-io :listen)
        (let ((c (send terminal-io :tyi)))
          (exos-stats :reset-p (char-equal c #\r))))
      (let ((n (LINK-RECV BUF)))
        (incf *pktn* n)
        (incf *pkts*)
        (princ ".")))))

(defvar *pkt-peek* nil
  "Controls level of peeking at packets.
  NIL just shows type and length.
  T or n also shows destination and source.
  n also shows n data characters both as hex and as chars.")

(defun netspy-receive (buffer nbytes)
  (INCF *EXOS-ETHERNET-PACKETS-RECEIVED*)
  (LET ((TYPE (GRATUITOUS-BYTESWAP-2 (READ-SHORT BUFFER 0 'ETHER-TYPE))))
    (SELECT TYPE
      (ETHERNET:CHAOS-ETHERNET-TYPE
       (INCF *EXOS-ETHERNET-CHAOS-PKTS-RECEIVED*)
       (format t "**** Got a chaos pkt ~D bytes long~%" nbytes))
      (ETHERNET:ADDRESS-RESOLUTION-TYPE
       (format t "**** Got an address resolution pkt ~D bytes long~%" nbytes))
      (ethernet:ip-ethernet-type
       (format t "**** Got an internet pkt ~D bytes long~%" nbytes))
      (OTHERWISE
       (format t "**** Got an unknown pkt ~D bytes long~%" nbytes)
       (INCF *EXOS-ETHERNET-UNKNOWN-PROTOCOL-PKTS-RECEIVED*)
       (SETQ *EXOS-ETHERNET-LAST-UNKNOWN-PROTOCOL-TYPE* TYPE)
       (SETF (FILL-POINTER *LAST-BOGUS-PACKET*) NBYTES)
       (COPY-ARRAY-PORTION BUFFER 0 NBYTES *LAST-BOGUS-PACKET* 0 NBYTES))))
  (when *pkt-peek*
    (pkt-peek buffer nbytes)))

(defun pkt-peek (buffer nbytes)
  "Show further information about packet.
  Variable *pkt-peek* controls level of peeking at packets.
  If it is non-NIL, this routine is called.
  If it is a number, show n data characters both as hex and as chars."
  (flet ((hexbytes (start length &optional terpri)
                   (dotimes (j length)
                     (and terpri (zerop (mod j terpri)) (terpri))
                     (format t "~16,2,'0R " (aref buffer (+ start j))))))
    (format t "ETHER DST: ")
    (hexbytes 0 6)
    (format t "~%ETHER SRC: ")
    (hexbytes 6 6)
    (format t "~%TYPE     : ")
    (hexbytes 12 2)
    (when (and (numberp *pkt-peek*) (> *pkt-peek* 0))
      (let ((nlook (min (- nbytes 14) *pkt-peek*)))
        (format t "~%~D bytes of data in hex:" nlook)
        (hexbytes 14 nlook 32)
        (format t "~%Packet data as characters:")
        (dotimes (j nlook)
          (and (zerop (mod j 64)) (terpri))
          (let ((c (aref buffer (+ 14 j))))
            (cond ((graphic-char-p c)
                   (tyo c))
                  ('else
                   (tyo #\_))))))))
  (terpri))


(defun dump-trans ()
  ;; this only works if you also have a 3com board.
  (format t "~&Ethernet Address ~,8@TType ~,8@TAddress ~12,8@THost~%")
  (dolist (e ethernet:ether-address-translations)
    (ecase (cadddr e)
      (:chaos
       (format t
              "~16R ~6,8@T~A ~,8T#o~O ~12,8@T~A~%"
              (cadr e)
              (cadddr e)
              (car e)
              (si:get-host-from-address (car e) :chaos)))
      (:ip
       (let ((n (car e)))
         (swapf (ldb (byte 8 0) n) (ldb (byte 8 24) n))
         (swapf (ldb (byte 8 16) n) (ldb (byte 8 8) n))
         (format t
              "~16R ~6,8@T~A ~,8@T~A ~,8@T~A~%"
              (cadr e)
              (cadddr e)
              (canonical-ip n)
              (si:get-host-from-address n :internet))))
      )))

(defun canonical-ip (ip-addr)
  "Return canonical string for numeric Internet Address."
  (check-type ip-addr number)
  (format nil "~3,48D.~3,48D.~3,48D.~3,48D"
          (ldb (byte 8 24) ip-addr)
          (ldb (byte 8 16) ip-addr)
          (ldb (byte 8 8) ip-addr)
          (ldb (byte 8 0) ip-addr)
          ))

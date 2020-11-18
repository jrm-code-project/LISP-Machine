;;; -*- Mode:LISP; Package:ARP; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

;;; &&& Put functions in order, avoiding forward references, to avoid
;;; mysterious cold load lossage (???) <05-Nov-88 keith>

(export '( get-hardware-address
           flush-hardware-address
           describe-arp-packet
           setup-arp))

(defvar *arp-stream* nil "arp-network-protocol structure for ARP protocol")

(eval-when (compile load eval)
  (defconstant arp-request 1)
  (defconstant arp-reply 2))

(defparameter arp-max-length 50. "ARP throws out packets that claim to be bigger than this")

(defun get-rcvd-arp-pkt (int-pkt interface)
  (let* ((header (net:ni-rcvd-header-length interface))
         (trailer (net:ni-rcvd-trailer-length interface))
         (total-words (/ (+ header trailer) 2)))
    (make-array (* 2 (- max-words-per-pkt total-words))
                :element-type '(unsigned-byte 8)
                :displaced-to (original-array int-pkt)
                :displaced-index-offset header
                :fill-pointer (* 2 (- (fill-pointer int-pkt) total-words)))))

(defun get-sent-arp-pkt (int-pkt interface)
  (let* ((header (net:ni-sent-header-length interface))
         (trailer (net:ni-sent-trailer-length interface))
         (total-words (/ (+ header trailer) 2)))
    (make-array (* 2 (- max-words-per-pkt total-words))
                :element-type '(unsigned-byte 8)
                :displaced-to (original-array int-pkt)
                :displaced-index-offset header
                :fill-pointer 0)))

(defun get-address-from-pkt-high-byte-first (array-8b begin nbytes)
  (let ((val 0))
    (dotimes (count nbytes)
      (setq val (dpb (aref array-8b (+ count begin)) (byte 8 (* 8 (- nbytes count 1))) val)))
    val))

(defun set-address-to-pkt-high-byte-first (address array-8b begin nbytes)
  (dotimes (count nbytes)
    (setf (aref array-8b (+ count begin)) (ldb (byte 8 (* 8 (- nbytes count 1))) address)))
  address)

(defun get-address-from-pkt-low-byte-first (array-8b begin nbytes)
  (let ((val 0))
    (dotimes (count nbytes)
      (setq val (dpb (aref array-8b (+ count begin)) (byte 8 (* 8 count)) val)))
    val))

(defun set-address-to-pkt-low-byte-first (address array-8b begin nbytes)
  (dotimes (count nbytes)
    (setf (aref array-8b (+ count begin)) (ldb (byte 8 (* count 8)) address)))
  address)

;the following macros make an ar structure that looks like a defstruct with slots:
;
;    ar-hardware                        2 bytes
;    ar-protocol                        2 bytes
;    ar-hardware-adr-length             1 byte
;    ar-protocol-adr-length             1 byte
;    ar-opcode                          2 bytes
;    ar-hardware-sender                 n bytes
;    ar-protocol-sender                 n bytes
;    ar-hardware-target                 n bytes
;    ar-protocol-target                 n bytes
;
; ar-hardware-sender, ar-protocol-sender, ar-hardware-target, and ar-protocol-target
; can only be referenced or set after the two length slots are set

(defmacro ar-hardware (ar-pkt)
  `(dpb (aref ,ar-pkt 0) (byte 8 8) (aref ,ar-pkt 1)))
(defun set-ar-hardware (ar-pkt val)
  (setf (aref ar-pkt 1) (ldb (byte 8 0) val))
  (setf (aref ar-pkt 0) (ldb (byte 8 8) val)))
(defsetf ar-hardware set-ar-hardware)

(defmacro ar-protocol (ar-pkt)
  `(dpb (aref ,ar-pkt 2) (byte 8 8) (aref ,ar-pkt 3)))
(defun set-ar-protocol (ar-pkt val)
  (setf (aref ar-pkt 3) (ldb (byte 8 0) val))
  (setf (aref ar-pkt 2) (ldb (byte 8 8) val)))
(defsetf ar-protocol set-ar-protocol)

(defmacro ar-hardware-adr-length (ar-pkt)
  `(aref ,ar-pkt 4))

(defmacro ar-protocol-adr-length (ar-pkt)
  `(aref ,ar-pkt 5))

(defmacro ar-opcode (ar-pkt)
  `(dpb (aref ,ar-pkt 6) (byte 8 8) (aref ,ar-pkt 7)))
(defun set-ar-opcode (ar-pkt val)
  (setf (aref ar-pkt 7) (ldb (byte 8 0) val))
  (setf (aref ar-pkt 6) (ldb (byte 8 8) val)))
(defsetf ar-opcode set-ar-opcode)

(defsubst ar-hardware-sender (ar-pkt)
  (get-address-from-pkt-high-byte-first ar-pkt 8 (ar-hardware-adr-length ar-pkt)))
(defsubst set-ar-hardware-sender (ar-pkt val)
  (set-address-to-pkt-high-byte-first val ar-pkt 8 (ar-hardware-adr-length ar-pkt)))
(defsetf ar-hardware-sender set-ar-hardware-sender)

(defsubst ar-hardware-sender-rev (ar-pkt)
  (get-address-from-pkt-low-byte-first ar-pkt 8 (ar-hardware-adr-length ar-pkt)))
(defsubst set-ar-hardware-sender-rev (ar-pkt val)
  (set-address-to-pkt-low-byte-first val ar-pkt 8 (ar-hardware-adr-length ar-pkt)))
(defsetf ar-hardware-sender-rev set-ar-hardware-sender-rev)

(defsubst ar-protocol-sender (ar-pkt)
  (get-address-from-pkt-low-byte-first ar-pkt
                                       (+ 8 (ar-hardware-adr-length ar-pkt))
                                       (ar-protocol-adr-length ar-pkt)))
(defsubst set-ar-protocol-sender (ar-pkt val)
  (set-address-to-pkt-low-byte-first val ar-pkt (+ 8 (ar-hardware-adr-length ar-pkt))
                                     (ar-protocol-adr-length ar-pkt)))
(defsetf ar-protocol-sender set-ar-protocol-sender)

(defsubst ar-protocol-sender-rev (ar-pkt)
  (get-address-from-pkt-high-byte-first ar-pkt (+ 8 (ar-hardware-adr-length ar-pkt))
                                        (ar-protocol-adr-length ar-pkt)))
(defsubst set-ar-protocol-sender-rev (ar-pkt val)
  (set-address-to-pkt-high-byte-first val ar-pkt (+ 8 (ar-hardware-adr-length ar-pkt))
                                      (ar-protocol-adr-length ar-pkt)))
(defsetf ar-protocol-sender-rev set-ar-protocol-sender-rev)

(defsubst ar-hardware-target (ar-pkt)
  (get-address-from-pkt-high-byte-first ar-pkt
                                        (+ 8
                                           (ar-hardware-adr-length ar-pkt)
                                           (ar-protocol-adr-length ar-pkt))
                                        (ar-hardware-adr-length ar-pkt)))
(defsubst set-ar-hardware-target (ar-pkt val)
  (set-address-to-pkt-high-byte-first val ar-pkt (+ 8
                                                    (ar-hardware-adr-length ar-pkt)
                                                    (ar-protocol-adr-length ar-pkt))
                                      (ar-hardware-adr-length ar-pkt)))
(defsetf ar-hardware-target set-ar-hardware-target)

(defsubst ar-hardware-target-rev (ar-pkt)
  (get-address-from-pkt-low-byte-first ar-pkt (+ 8
                                                 (ar-hardware-adr-length ar-pkt)
                                                 (ar-protocol-adr-length ar-pkt))
                                                 (ar-hardware-adr-length ar-pkt)))
(defsubst set-ar-hardware-target-rev (ar-pkt val)
  (set-address-to-pkt-low-byte-first val ar-pkt (+ 8
                                                   (ar-hardware-adr-length ar-pkt)
                                                   (ar-protocol-adr-length ar-pkt))
                                     (ar-hardware-adr-length ar-pkt)))
(defsetf ar-hardware-target-rev set-ar-hardware-target-rev)

(defsubst ar-protocol-target (ar-pkt)
  (get-address-from-pkt-low-byte-first ar-pkt (+ 8
                                                 (* 2 (ar-hardware-adr-length ar-pkt))
                                                 (ar-protocol-adr-length ar-pkt))
                                       (ar-protocol-adr-length ar-pkt)))
(defsubst set-ar-protocol-target (ar-pkt val)
  (set-address-to-pkt-low-byte-first val ar-pkt (+ 8
                                                   (* 2 (ar-hardware-adr-length ar-pkt))
                                                   (ar-protocol-adr-length ar-pkt))
                                     (ar-protocol-adr-length ar-pkt)))
(defsetf ar-protocol-target set-ar-protocol-target)

(defsubst ar-protocol-target-rev (ar-pkt)
  (get-address-from-pkt-high-byte-first ar-pkt (+ 8
                                                  (* 2 (ar-hardware-adr-length ar-pkt))
                                                  (ar-protocol-adr-length ar-pkt))
                                        (ar-protocol-adr-length ar-pkt)))
(defsubst set-ar-protocol-target-rev (ar-pkt val)
  (set-address-to-pkt-high-byte-first val ar-pkt (+ 8
                                                    (* 2 (ar-hardware-adr-length ar-pkt))
                                                    (ar-protocol-adr-length ar-pkt))
                                      (ar-protocol-adr-length ar-pkt)))
(defsetf ar-protocol-target-rev set-ar-protocol-target-rev)

(defsubst ar-pkt-length (ar-pkt)
  (+ 8
     (* 2 (ar-hardware-adr-length ar-pkt))
     (* 2 (ar-protocol-adr-length ar-pkt))))

(defsubst arp-int-pkt-length (int-pkt offset)
  (let ((length-bytes (aref int-pkt (+ offset 2))))
    (+ 8
       (* 2 (ldb (byte 8 0) length-bytes))
       (* 2 (ldb (byte 8 8) length-bytes)))))

(defun describe-arp-packet (pkt)
  (format t "~&~S is an ~S" pkt 'addr-res-pkt)
  (format t "~&   ~s~30t~o~40t~a"
          'ar-hardware
          (ar-hardware pkt)
          (case (ar-hardware pkt)
            (#.arp-ethernet-hardware "ETHERNET hardware")
            (otherwise "Unknown hardware type")))
  (let ((protocol-keyword :unknown))
    (format t "~&   ~s~30t~o~40t~a"
            'ar-protocol
            (ar-protocol pkt)
            (case (ar-protocol pkt)
              (#.chaos-ethernet-type
               (setq protocol-keyword :chaos)
               "CHAOS protocol")
              (#.ip-ethernet-type
               (setq protocol-keyword :ip)
               "INTERNET protocol")
              (#.arp-ethernet-type "ADDRESS RES protocol (this doesn't make sense)")
              (otherwise "Unknown protocol type")))
    (format t "~&   ~s~30t~o (~:*~d.)"
            'ar-hardware-adr-length
            (ar-hardware-adr-length pkt))
    (format t "~&   ~s~30t~o (~:*~d.)"
            'ar-protocol-adr-length
            (ar-protocol-adr-length pkt))
    (format t "~&   ~s~30t~o~40t~a"
            'ar-opcode
            (ar-opcode pkt)
            (case (ar-opcode pkt)
              (#.arp-request "REQUEST")
              (#.arp-reply "REPLY")))
    (format t "~&   ~s~30t~o~40t~:*#x~16r"
            'ar-hardware-sender
            (ar-hardware-sender pkt))
;      (dolist (interface *network-interfaces*)
;       (dolist (at (net:ni-address-translations interface))
;         (cond ((= (ar-hardware-sender pkt) (net:at-hardware-address at))
;                (format t ", ~S #o~o" (net:at-protocol at) (net:at-protocol-address at))))))
    (format t "~&   ~s~30t~o~40t~:*#x~16r"
            'ar-protocol-sender
            (ar-protocol-sender pkt))
    (let ((from-host (si:get-host-from-address (ar-protocol-sender pkt) protocol-keyword)))
      (if from-host
          (format t " ~s" (send from-host :short-name))))
    (format t "~&   ~s~30t~o~40t~:*#x~16r"
            'ar-hardware-target
            (ar-hardware-target pkt))
;      (dolist (interface *network-interfaces*)
;       (dolist (at (net:ni-address-translations interface))
;         (cond ((= (ar-hardware-target pkt) (net:at-hardware-address at))
;                (format t ", ~S #o~o" (net:at-protocol at) (net:at-protocol-address at))))))
    (format t "~&   ~s~30t~o~40t~:*#x~16r"
            'ar-protocol-target
            (ar-protocol-target pkt))
    (let ((to-host (si:get-host-from-address (ar-protocol-target pkt) protocol-keyword)))
      (if to-host
          (format t " ~s" (send to-host :short-name))))))

(defun send-addr-pkt (address stream interface
                      &optional his-hardware-address on-behalf-of int-pkt hang-p
                      &aux pkt)
  (unless on-behalf-of
    (setq on-behalf-of (cadr (assoc (net:np-keyword stream) (net:ni-address-alist interface) :test #'eq))))
  (unwind-protect
      (when (and *arp-stream*
                 (net:np-opened *arp-stream*)
                 (net:ni-hardware-type interface)
                 (net:ni-address interface)
                 (net:ni-broadcast-address interface)
                 on-behalf-of
                 (or int-pkt (setq int-pkt (allocate-packet hang-p))))
        (setq pkt (get-sent-arp-pkt int-pkt interface))
        (setf (ar-hardware pkt) (net:ni-hardware-type interface))
        (setf (ar-hardware-adr-length pkt) (net:ni-address-length interface))
        (setf (ar-opcode pkt) (if his-hardware-address arp-reply arp-request))
        (setf (ar-hardware-sender pkt) (net:ni-address interface))
        (setf (ar-hardware-target pkt) (or his-hardware-address 0))
        (setf (ar-protocol pkt) (cdr (assoc (net:ni-keyword interface) (net:np-protocol stream) :test #'eq)))
        (setf (ar-protocol-adr-length pkt) (net:np-address-length stream))
        (if (net:np-big-endian-address-p stream)
            (setf (ar-protocol-target-rev pkt) address)
          (setf (ar-protocol-target pkt) address))
        (if (net:np-big-endian-address-p stream)
            (setf (ar-protocol-sender-rev pkt) on-behalf-of)
          (setf (ar-protocol-sender pkt) on-behalf-of))
        (unwind-protect
            (send *arp-stream*
                  :send
                  int-pkt
                  (ar-pkt-length pkt)
                  interface
                  (or his-hardware-address (net:ni-broadcast-address interface))
                  hang-p)
          (setq int-pkt nil))
        (return-from send-addr-pkt t))
    (when int-pkt (free-packet int-pkt)))
  nil)

(defun processor-without-network-interface (address domain)
  "Return T if the specified ADDRESS in the specified address DOMAIN is on our bus and does not
control a network interface.  I.E. if we should accept packets for it."
  (and si:*share-code-ready*
       (boundp 'si:share-interface)
       si:share-interface
       (net:ni-enabled si:share-interface)
       (dolist (x (net:ni-address-translations si:share-interface))
         (when (and (eq (net:at-protocol x) domain)
                    (= (net:at-protocol-address x) address))
           (return (cdr (assoc (net:at-hardware-address x) net:*processor-forwarding-alist*)))))))

(defun get-address-info (address protocol ask-if-necessary interface &optional hang-p on-behalf-of)
  "Search the address-translation tables for the address info for a specific
protocol address.  ask-if-necessary determines whether ARP requests should
be generated if the info is not found.  interface optionally specifies a
particular network interface to restrict the search to"
  (declare (values address-info interface))
  (when on-behalf-of
    (dolist (stream *network-protocol-streams*)
      (when (eq protocol (net:np-keyword stream))
        (unless (processor-without-network-interface on-behalf-of (net:np-keyword stream))
          ;;We don't masquerade as processors not on our own bus
          (setq on-behalf-of nil)))))
  (dolist (i *network-interfaces*)
    (when (or (null interface) (eq interface i))
      (dolist (at (net:ni-address-translations i))
        (when (and (= address (net:at-protocol-address at))
                   (eq protocol (net:at-protocol at))
                   (or (net:ni-point-to-point i)
                       (null on-behalf-of)
                       (member on-behalf-of (net:at-on-behalf-of at))))
          (return-from get-address-info (values at i))))))
  (when ask-if-necessary
    (dolist (stream *network-protocol-streams*)
      (when (eq protocol (net:np-keyword stream))
        (dolist (i *network-interfaces*)
          (when (or (null interface) (eq interface i))
            (send-addr-pkt address stream i nil on-behalf-of nil hang-p)))
        (return t))))
  (values nil nil))

(defun special-translation (address protocol interface)
  (dolist (ni *network-interfaces*)
    (when (or (null interface) (eq ni interface))
      (dolist (elt (net:ni-address-alist ni))
        (when (and (eq (car elt) protocol) (member address (cdr elt)))
          (return-from special-translation (net:ni-address ni))))))
  (dolist (s *network-protocol-streams*)
    (when (eq protocol (net:np-keyword s))
      (return (and (net:np-special-address-function s)
                   (funcall (net:np-special-address-function s) address interface))))))

(defun get-hardware-address (address protocol &optional interface hang-p on-behalf-of
                             &aux addr at)
  "Given a protocol address and a protocol keyword, find the hardware address
on a directly connected network interface.  If address info not currently
available returns NIL, else returns the address and interface."
  (declare (values hardware-address interface))
  (dolist (ni *network-interfaces*)
    (when (or (null interface) (eq ni interface))
      ;; If you have an :ethernet address and there is a network interface that uses
      ;; :ethernet addresses directly, the following clause returns that network interface.
      ;; **** The question: what to do if you have more than one such interface?
      ;; The address is likely only valid on one network.
      (when (eq (net:ni-keyword ni) protocol)
        (return-from get-hardware-address (values address ni)))
      (when (setq addr (special-translation address protocol ni))
        (return-from get-hardware-address (values addr ni)))
      (dolist (elt (net:ni-address-alist ni))
        (when (and (eq (car elt) protocol) (member address (cdr elt)))
          (return-from get-hardware-address (values (net:ni-address ni) ni))))))
  (multiple-value-setq (at interface)
    (get-address-info address protocol t interface hang-p on-behalf-of))
  (values (and at (net:at-hardware-address at)) interface))

(defop (network-protocol :translate-address) (address &optional interface on-behalf-of)
  "The network-protocol interface to the internal ARP function to retrieve
an entry from the address-translation tables"
  (declare (values hardware-address interface))
  (when (net:np-enabled self)
    (if (member address (net:np-addresses self))
        (values 0 *loopback-interface*)
      (get-hardware-address address (net:np-keyword self) interface nil on-behalf-of))))

(defun get-protocol-address (hardware-address protocol &optional interface)
  "Given a hardware address, a network protocol keyword, and
a network interface, search the address translation tables
and return the network address and network interface"
  (declare (values protocol-address interface))
  (dolist (i *network-interfaces*)
    (when (or (null interface) (eq interface i))
      (dolist (at (net:ni-address-translations i))
        (when (and (= hardware-address (net:at-hardware-address at))
                   (or (null protocol) (eq protocol (net:at-protocol at))))
          (return-from get-protocol-address (values (net:at-protocol-address at) i)))))))

(defun flush-hardware-address (address protocol &optional interface)
  "The network-protocol interface to the internal ARP function to remove
an entry from the address-translation tables"
  (cond ((special-translation address protocol interface))
        (t
         (without-interrupts
           (dolist (ni *network-interfaces*)
             (when (or (null interface) (eq interface ni))
               (dolist (at (net:ni-address-translations ni))
                 (when (and (= address (net:at-protocol-address at))
                            (eq protocol (net:at-protocol at)))
                   (setf (net:ni-address-translations ni)
                         (delete at (net:ni-address-translations ni)))))))
           (get-hardware-address address protocol)))))

(defop (network-protocol :flush-address) (address &optional interface)
  (when (net:np-enabled self)
    (flush-hardware-address address (net:np-keyword self) interface)))

(defun addr-res-pkt-for-me-p (stream target-address interface)
  (let ((my-address (cadr (assoc (net:np-keyword stream) (net:ni-address-alist interface) :test #'eq))))
    (cond ((null my-address)                    ;Not for me if we don't do this protocol
           nil)
          ((= my-address target-address)        ;For me if my specific address
           t)
          ((processor-without-network-interface target-address (net:np-keyword stream))
           ;For me, if for processor on my bus that doesn't own hardware
           t)
          ((eq (net:np-keyword stream) :chaos)
           ;;BEGIN KLUDGE
           (and (= chaos:my-address #o3140)
                (= target-address #o3540))      ;hack for cadr2
           ;;END KLUDGE
           )
          (t
           nil))))

(defun receive-arp-pkt (int-pkt interface stream source destination broadcast-p
                         &aux pkt protocol sender-address target-address)
  (declare (ignore source))
  (declare (ignore destination))
  (declare (ignore broadcast-p))
  (when (setq pkt (get-rcvd-arp-pkt int-pkt interface))
    (dolist (ll *network-protocol-streams*)
      (when (eq (ar-protocol pkt) (cdr (assoc (net:ni-keyword interface) (net:np-protocol ll) :test #'eq)))
        (setq protocol ll)
        (setq sender-address (if (net:np-big-endian-address-p protocol)
                                 (ar-protocol-sender-rev pkt)
                               (ar-protocol-sender pkt)))
        (setq target-address (if (net:np-big-endian-address-p protocol)
                                 (ar-protocol-target-rev pkt)
                               (ar-protocol-target pkt)))
        (return t)))
    (cond ((null protocol)
           (pushnew (ar-protocol pkt) (net:np-protocols-not-understood stream)))
          (t
           (when (< (ar-pkt-length pkt) arp-max-length)
             (add-address-info sender-address
                               (net:np-keyword protocol)
                               interface
                               (ar-hardware-sender pkt)
                               target-address))
           (when (and (= (ar-opcode pkt) arp-request)
                      (addr-res-pkt-for-me-p protocol target-address interface))
             (unwind-protect
                 (send-addr-pkt sender-address
                                protocol
                                interface
                                (ar-hardware-sender pkt)
                                target-address
                                int-pkt)
               (setq int-pkt nil))))))
  (when int-pkt (free-packet int-pkt)))

(defun addr-stat (&rest interfaces)
  (unless interfaces
    (when *arp-stream*
      (format t "~@[~&Protocols not understood: ~s~]"
              (net:np-protocols-not-understood *arp-stream*))
      (format t "~&ARP packets received ~d transmitted ~d"
              (net:np-packets-received *arp-stream*) (net:np-packets-sent *arp-stream*))))
  (dolist (ni *network-interfaces*)
    (when (or (null interfaces)
              (member ni interfaces))
      (format t "~2&Address translations for ~A address ~X"
              (net:ni-keyword ni)
              (net:ni-address ni))
      (dolist (x (net:ni-address-translations ni))
        (format t "~&~A~9t~a~25t~a~@[~50t~16,12,'0r~]"
                (net:at-protocol x)
                (net:unparse-address (net:at-protocol-address x) (net:at-protocol x))
                (si:get-host-from-address (net:at-protocol-address x) (net:at-protocol x))
                (net:at-hardware-address x))))))

(defun setup-arp ()
  (setq *arp-stream* (make-network-protocol :keyword :arp
                                            :addresses nil
                                            :packet-length-function 'arp-int-pkt-length
                                            :interrupt-function 'receive-arp-pkt
                                            :gauge-name "ARP"
                                            ))

  (send *arp-stream* :open nil nil)
  (send *arp-stream* :enable))

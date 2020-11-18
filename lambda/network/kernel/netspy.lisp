;;; -*- Mode:LISP; Package:ETHERNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

;;;
;;; Look at all ethernet packets with the Excelan board.
;;;

(defvar *netspy-pkts* 0 "Number of packets seen.")
(defvar *netspy-pkt-bytes* 0 "Number of bytes seen.")

(defun netspy (&key (format-stream *standard-output*)
               peek-level types not-types sources destinations s-or-d)
  "Look at all ethernet packets seen by the Excelan board.
  Output is directed to FORMAT-STREAM unless it is NIL
    in which case a dot is PRINCed for each packet;
    use 'si:null-stream for no output except EXOS-STATS.
  PEEK-LEVEL controls level of peeking at packets;
    If peek-level is NIL, just show packet type and length;
    if non-NIL, also show destination and source;
    if n also show n data characters both as hex and as chars;
    if a list, (e.g. ip, udp, tcp, data n), interpret selected headers and n data characters.

  A packet will be selected if it satisfies the AND of the following specifications.
  TYPES is a list of ethernet types to be accepted.
  NOT-TYPES is a list of ethernet types to be rejected.
  SOURCES is a list of ethernet source addresses to be accepted.
  DESTINATIONS is a list of ethernet destination addresses to be accepted.
  S-OR-D is a list of ethernet addresses to be accepted either as source or destination.

  NETSPY runs till ABORT.
  Type any character to see EXOS-STATS, type character r to reset EXOS-STATS."

  (declare (special format-stream peek-level types not-types sources destinations s-or-d))

  (when (and (si:set-processor-owning-ethernet :find :excelan)
             (i-own-excelan))
    (unless *excelan-ethernet-interface*
      (setup-excelan "EXCELAN" nil))
    (let ((excelan-enabled (exc-enabled *excelan-ethernet-interface*)))
      (unwind-protect
          (progn
            (when excelan-enabled
              (send *excelan-ethernet-interface* :disable nil))
            (setq *netspy-pkts* 0 *netspy-pkt-bytes* 0)
            (link-net-mode :read-p nil :write-p t :mode 'nmode-connect-promiscuous)
            (exos-stats :force-p t)
            (netspy-loop))
        (send *excelan-ethernet-interface* :reset)
        (when excelan-enabled
          (send *excelan-ethernet-interface* :enable))))))

(defun netspy-loop ()
  (declare (special format-stream peek-level types not-types sources destinations s-or-d))
  (terpri format-stream)
  (with-dmabuf (buf)
    (loop
      (when (listen *terminal-io*)
        (let ((c (read-char *terminal-io*)))
          (exos-stats :reset-p (char-equal c #\r) :force-p t)))
      (let* ((n (- (link-recv buf) 4))
             (type (read-short buf 0 'ether-type))
             (source (read-48bits buf 0 'ether-shost))
             (destination (read-48bits buf 0 'ether-dhost)))
        (incf *netspy-pkt-bytes* n)
        (incf *netspy-pkts*)
        (when (and (or (not types) (member type types))
                   (or (not not-types) (not (member type not-types)))
                   (or (not sources) (member source sources))
                   (or (not destinations) (member destination destinations))
                   (or (not s-or-d) (member source s-or-d) (member destination s-or-d)))
          (cond (format-stream
                 (format format-stream "***At ~D got ~4D byte pkt" (get-universal-time) n)
                 (case type
                   (#.chaos-ethernet-type
                    (format format-stream ", type CHAOS"))
                   (#.arp-ethernet-type
                    (format format-stream ", type ARP"))
                   (#.ip-ethernet-type
                    (format format-stream ", type IP"))
                   (otherwise
                    (format format-stream ", type UNKNOWN (~X)" type)))
                 (terpri format-stream)
                 (when peek-level
                   (pkt-peek buf n type)))
                (t
                 (princ "."))))))))


(defun pkt-peek (buffer nbytes type)
  "Show further information about packet.
  Variable PEEK-LEVEL controls level of peeking at packets.
  If it is non-NIL, this routine is called; show destination, source and type;
  if it is n also show n data characters both as hex and as chars;
  if a list, (e.g. ip, udp, tcp, data n), interpret selected headers and n data characters."
  (declare (special format-stream peek-level))
  (format format-stream "Ethernet header: DST:")
  (hexbytes buffer 0 6)
  (format format-stream ", SRC:")
  (hexbytes buffer 6 6)
  (format format-stream ", TYPE:")
  (hexbytes buffer 12 2)
  (let  ((data-offset 14)
         (level (if (and (consp peek-level) (member :data peek-level :test #'eq))
                    (or (cadr (member :data peek-level :test #'eq)) nbytes)
                  peek-level)))
    (When (= type ip-ethernet-type)
      (let* ((ip-start data-offset)
             (ip-ihl (* 4 (ldb (byte 4 0) (read-uchar buffer ip-start 'ip-vers-ihl)))))
        (incf data-offset ip-ihl)
        (when (consp peek-level)
          (when (member :ip peek-level :test #'eq)
            (pkt-ip-header buffer ip-start))
          (case (read-uchar buffer ip-start 'ip-protocol)
            (17
             (when (member :udp peek-level :test #'eq)
               (incf data-offset (pkt-udp-header buffer data-offset))))
            (6
             (when (member :tcp peek-level :test #'eq)
               (incf data-offset (pkt-tcp-header buffer data-offset))))))))
    (and (numberp level)
         (> level 0)
         (pkt-data buffer data-offset nbytes level)))
  (terpri format-stream))

(defun hexbytes (buffer start length &optional line-length)
  (declare (special format-stream))
  (dotimes (j length)
    (and line-length
         (zerop (mod j line-length))
         (terpri format-stream))
    (format format-stream " ~16,2,'0R" (aref buffer (+ start j)))))

; Protocol header definitions
(defconstant ip-vers-ihl 0)
(setf (get 'ip-vers-ihl 'size-in-bytes) 1)
(defconstant ip-total-length 2)
(setf (get 'ip-total-length 'size-in-bytes) 2)
(defconstant ip-protocol 9)
(setf (get 'ip-protocol 'size-in-bytes) 1)
(defconstant ip-source 12)
(setf (get 'ip-source 'size-in-bytes) 4)
(defconstant ip-destination 16)
(setf (get 'ip-destination 'size-in-bytes) 4)
(defconstant udp-source 0)
(setf (get 'udp-source 'size-in-bytes) 2)
(defconstant udp-destination 2)
(setf (get 'udp-destination 'size-in-bytes) 2)
(defconstant tcp-source 0)
(setf (get 'tcp-source 'size-in-bytes) 2)
(defconstant tcp-destination 2)
(setf (get 'tcp-destination 'size-in-bytes) 2)
(defconstant tcp-sequence 4)
(setf (get 'tcp-sequence 'size-in-bytes) 4)
(defconstant tcp-flags 13)
(setf (get 'tcp-flags 'size-in-bytes) 1)


(defun pkt-ip-header (buffer start)
  (declare (special format-stream))
  (let ((ip-ihl (* 4 (ldb (byte 4 0) (read-uchar buffer start 'ip-vers-ihl))))
        (ip-prot (read-uchar buffer start 'ip-protocol)))
    (format format-stream "~%IP header:")
    (format format-stream " prot ~D = ~A" ip-prot
            (get '(ip-portocols
                    1 icmp 3 ggp 6 tcp 8 egp 9 igp 16 chaos 17 udp 27 rdp 62 cftp)
                  ip-prot 'unknown))
    (format format-stream ", src ~S" (ip:canonical-ip (read-long buffer start 'ip-source)))
    (format format-stream ", dst ~S" (ip:canonical-ip (read-long buffer start 'ip-destination)))
;  (format format-stream "~%IP header in hex: ")
;  (hexbytes buffer start ip-ihl)
    ip-ihl))

(defun pkt-udp-header (buffer start)
  (declare (special format-stream))
  (format format-stream "~%UDP header:")
  (decode-tcp-port " src" (read-short buffer start 'udp-source))
  (decode-tcp-port ", dst" (read-short buffer start 'udp-destination))
;  (format format-stream "~%UDP header in hex: ")
;  (hexbytes buffer start 8)
  8)

(defun pkt-tcp-header (buffer start)
  (declare (special format-stream))
  (format format-stream "~%TCP header:")
  (decode-tcp-port " src" (read-short buffer start 'tcp-source))
  (decode-tcp-port ", dst" (read-short buffer start 'tcp-destination))
  (format format-stream "~% seq ~D" (read-long buffer start 'tcp-sequence))
  (let ((flags (read-uchar buffer start 'tcp-flags)))
    (format format-stream ", flags")
    (loop for position from 5 downto 0
          for flag-name in '(urg ack psh rst syn fin)
          do (when (logbitp position flags)
               (format format-stream " ~A" flag-name))))
;  (format format-stream "~%TCP header in hex: ")
;  (hexbytes buffer start 20)
  20)

(defun decode-tcp-port (which-port port-number)
  (declare (special format-stream))
  (format format-stream "~A port ~D = ~A" which-port port-number
          (get '(tcp-ports
                  5 rje 7 echo 9 discard 11 users 13 daytime 15 netstat 17 quote
                  20 ftp-data 21 ftp 23 telnet 25 smtp 35 printer 37 time
                  42 nameserver 43 nicname 49 login 53 domain 77 private-rje
                  79 finger 95 supdup 101 hostname 107 rtelnet 111 sunrpc
                  115 sftp 117 uucp-path 119 untp 123 ntp)
                port-number 'unknown)))

(defun pkt-data (buffer start nbytes level)
  (declare (special format-stream))
  (let ((nlook (min (- nbytes start) level)))
        (format format-stream "~%~D bytes of data as chars and in hex: " nlook)
        (dotimes (j nlook)
;         (and (zerop (mod j 64)) (terpri format-stream))
          (let ((c (aref buffer (+ start j))))
            (cond ((graphic-char-p c)
                   (write-char c))
                  ('else
                   (write-char #\_)))))
        (hexbytes buffer start nlook 32)))

(defun netblat (&key (string "Blat Hello") (destination (net:ni-address *excelan-ethernet-interface*))
                (type 0) interval count (hash t))
  "Send ethernet packets to destination every interval seconds.
  If destination is NIL, assume packet contains ethernet header."

  (unwind-protect
      (progn
        (send *excelan-ethernet-interface* :disable nil)
        (link-net-mode :read-p nil :write-p t :mode 'nmode-connect-hardware-filter)
        (link-net-recv :read-p nil :write-p t :enable-p nil :slot *link-ethernet-slot*)
        (link-net-recv :read-p nil :write-p t :enable-p nil :slot *link-broadcast-slot*)
        (with-dmabuf (buf)
          (let ((size (length string)))
            (cond ((not destination)
                   (copy-array-portion string 0 size buf 0 size))
                  ('ELSE
                   (write-48bits destination buf 0 'ether-dhost)
                   (write-48bits (net:ni-address *excelan-ethernet-interface*) buf 0 'ether-shost)
                   (write-short type buf 0 'ether-type)
                   (copy-array-portion string 0 size buf 14 (+ size 14))
                   (incf size 14)))
            (do ((i 1 (1+ i)))
                ((and count (> i count)))
              (if hash (princ "*"))
              (link-xmit buf (min 1514 (max 60 size)))  ; Exos 201 won't xmit <60 or >1514.
              (if interval (sleep interval))))))
    (send *excelan-ethernet-interface* :reset)
    (send *excelan-ethernet-interface* :enable)))

(defun lookup-enet (host type)
  (if (numberp type)
      (setq type
            (or (cadr (assoc type
                            `((,ip-ethernet-type :internet)
                              (,chaos-ethernet-type :chaos)
                              (,arp-ethernet-type :internet :chaos)) :test #'eq))
                type)))
  (let* ((host-address-list (send (si:parse-host host) :network-addresses))
         (address (getf host-address-list type)))
    (when address
      (arp:get-hardware-address (car address) type))))

(defun spy-host-pair (h1 h2 &key (proto ip-ethernet-type))
  "Spy on traffic between hosts h1 and h2."
  (let ((e1 (if (numberp h1) h1 (lookup-enet h1 proto)))
        (e2 (if (numberp h2) h2 (lookup-enet h2 proto))))
    (format T "~&~16,12,'0R = ~A~%~16,12,'0R = ~A~%"
            e1 h1
            e2 h2)
    (netspy :peek-level t :types (list proto)
            :sources (list e1 e2)
            :destinations (list e2 e1))))

;;; -*- Mode:LISP; Package:UDP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *udp-stream*
           setup-udp
           describe-udp
           make-udp-socket
           using-udp-socket
           multiple-connection-listen
           multiple-connection-kill-gauges))

;;;UDP's interface to IP
(defstream udp-ip-transport-protocol
           (ip-transport-protocol)
           "UDP-"
  (user-socket-alist nil)                       ;user sockets
  (next-local-port 256)                         ;next local port to assign
  (checksum-failures 0)                         ;bad checksum packets
  (local-checksums nil)                         ;T if calculate checksums for looped-back packets
  )

(defop (udp-ip-transport-protocol :peek-normal-fields) (tp)
  (ncons (tv:scroll-parse-item
           `(:function udp-next-local-port (,tp) NIL ("  Next local port assigned = ~D"))
           `(:function udp-checksum-failures (,tp) NIL ("  Checksum failures ~D"))
           `(:mouse-item
              (nil :buttons
                   ,(make-list 3 :initial-element
                               `(nil :eval (setf (udp-local-checksums tp)
                                                 (not (udp-local-checksums tp)))
                                     :bindings ((tp ',tp))))
                   :DOCUMENTATION
                   "Click to toggle local checksums"
                   :BINDINGS
                   ((tp ',tp)))
              :function udp-local-checksums (,tp) NIL (" Local checksums: ~A")))))

(defop (udp-ip-transport-protocol :peek-final-fields) (tp)
  (ncons (TV:SCROLL-MAINTAIN-LIST
           #'(LAMBDA () (udp-user-socket-alist tp))
           'peek-udp-socket)))

(defun peek-udp-socket (elt)
  (let ((socket (cdr elt)))
    (send socket :peek-item elt)))

(defvar *udp-stream* nil "The ip-transport-protocol for UDP")
(defvar *udp-receives-out* 8 "Number of outstanding receives")

(defconstant udp-protocol 17 "IP protocol type field for UDP")

(defun setup-udp ()
  (setq *udp-stream* (make-udp-ip-transport-protocol :keyword :udp
                                                     :type udp-protocol
                                                     :gauge-name "UDP"
                                                     :reset-function 'udp-reset
                                                     :close-function 'udp-close
                                                     :receive-function 'receive-udp-packet
                                                     :icmp-notification-function 'udp-icmp
                                                     :broadcast-allowed-p t
                                                     ))

  (send *udp-stream* :open)
  (send *udp-stream* :enable)
  (dotimes (i *udp-receives-out*)               ;and give IP some read buffers
    (send *udp-stream* :receive (get-udp-buffer)))
  t)

(defvar *free-udp-buffers* nil "List of free UDP message buffers")

(defun get-udp-buffer ()
  "returns an array to hold a UDP message."
  (if *free-udp-buffers*
      (pop *free-udp-buffers*)
    (zl:make-array 1480                         ; + 20 IP = 1500 bytes
                   :element-type '(unsigned-byte 8)
                   :fill-pointer 0
                   :leader-length 2
                   :named-structure-symbol 'udp-buffer)))

(defun free-udp-buffer (udp)
  (push (original-array udp) *free-udp-buffers*)
  nil)

(defun udp-buffer-p (buffer)
  (eq (named-structure-p buffer) 'udp-buffer))

(defvar *free-udp-headers* nil "List of free UDP headers")

(defun get-udp-header ()
  "returns an array to hold a UDP header."
  (if *free-udp-headers*
      (pop *free-udp-headers*)
    (make-array 8 :element-type '(unsigned-byte 8))))

(defun free-udp-header (udp)
  (push udp *free-udp-headers*)
  nil)

;;;UDP Header Definitions

(defmacro udp-source-port (udp)
  `(dpb (aref ,udp 0) (byte 8 8) (aref ,udp 1)))
(defun set-udp-source-port (udp val)
  (setf (aref udp 0) (ldb (byte 8 8) val))
  (setf (aref udp 1) (ldb (byte 8 0) val))
  val)
(defsetf udp-source-port set-udp-source-port)

(defmacro udp-destination-port (udp)
  `(dpb (aref ,udp 2) (byte 8 8) (aref ,udp 3)))
(defun set-udp-destination-port (udp val)
  (setf (aref udp 2) (ldb (byte 8 8) val))
  (setf (aref udp 3) (ldb (byte 8 0) val))
  val)
(defsetf udp-destination-port set-udp-destination-port)

(defmacro udp-length (udp)
  `(dpb (aref ,udp 4) (byte 8 8) (aref ,udp 5)))
(defun set-udp-length (udp val)
  (setf (aref udp 4) (ldb (byte 8 8) val))
  (setf (aref udp 5) (ldb (byte 8 0) val))
  val)
(defsetf udp-length set-udp-length)

(defmacro udp-checksum (udp)
  `(dpb (aref ,udp 6) (byte 8 8) (aref ,udp 7)))
(defun set-udp-checksum (udp val)
  (setf (aref udp 6) (ldb (byte 8 8) val))
  (setf (aref udp 7) (ldb (byte 8 0) val))
  val)
(defsetf udp-checksum set-udp-checksum)

(defconstant udp-first-data-byte 8 "Offset of first data byte in udp buffer")

(defun describe-udp (udp &optional display-data-p)
  "Given an art-8b array, displays the UDP message in it"
  (format t "~&source-port: ~20t~D" (udp-source-port udp))
  (format t "~&destination-port: ~20t~D" (udp-destination-port udp))
  (format t "~&checksum: ~20t~D" (udp-checksum udp))
  (format t "~&length: ~20t~D" (udp-length udp))

  (when (and display-data-p (> (udp-length udp) 8))
    (if (numberp display-data-p)
        (format t "~&~D bytes of data:" display-data-p)
      (format t "~&rest of data:"))
    (do* ((offset 8 (1+ offset))
          (length (- (udp-length udp) offset))
          (count (if (numberp display-data-p) (min length display-data-p) length))
          (index 0 (1+ index)))
         ((= index count) nil)
      (if (zerop (mod index 32))
          (format t "~&"))
      (format t "~16,2,'0r " (aref udp offset))))

  udp)

(defun check-udp-checksum (udp ip local-p)
  ;;"An all-zero transmitted checksum value means that the transmitter generated no checksum" -- RFC768 p2
  (when (or (zerop (udp-checksum udp))
            (and (not (udp-local-checksums *udp-stream*)) local-p))
    (return-from check-udp-checksum (values t 0)))
  (let* ((count (udp-length udp))
         (sum (checksum-1 udp (pseudo-header-checksum ip count) count)))
    (values (zerop sum) sum)))

(defun store-udp-checksum (buffers ip local-p)
  (unless (consp buffers)
    (setq buffers (ncons buffers)))
  (setf (udp-checksum (first buffers)) 0)
  (when (and (not (udp-local-checksums *udp-stream*)) local-p)
    (return-from store-udp-checksum t))
  (let ((sum (checksum buffers (pseudo-header-checksum ip (udp-length (first buffers))))))
    ;;"If the computed checksum is zero, it is transmitted as all ones" -- RFC768 p2
    (setf (udp-checksum (first buffers)) (if (zerop sum) #xffff sum))))

;;;User interface to UDP

(defstream udp-socket
           ()
           "UDP-USER-"
  (keyword nil)                                 ; Keyword to identify this UDP user
  (local-port nil)                              ; Local Port number of this UDP user
  (remote-port nil)                             ; Remote Port number, if specified
  (remote-address nil)                          ; Remote Address, if specified
  (receive-buffers nil)                         ; list of buffers to receive network packets
  (packet-list nil)                             ; list of received packets
  (statistics-block (make-statistics-block))    ; Network statistics block maintained at clock level
  (packets-sent-discarded 0)                    ; statistic: sent packets discarded (couldn't route)
  (packets-received-discarded 0)                ; statistic: received packets discarded (no rcv buffer)
  (bytes-sent-discarded 0)                      ; statistic: sent bytes discarded
  (bytes-received-discarded 0)                  ; statistic: received bytes discarded
  (active-gauges nil)                           ; list of gauges in control panel
  (inactive-gauges nil)                         ; list of gauges not currently in control panel
  (gauge-name nil)                              ; the name of this protocol for its gauges
  )

(defsubst udp-user-packets-sent (uu)
  (aref (udp-user-statistics-block uu) STAT-PS STAT-CURR))
(defsubst udp-user-packets-received (uu)
  (aref (udp-user-statistics-block uu) STAT-PR STAT-CURR))
(defsubst udp-user-bytes-sent (uu)
  (aref (udp-user-statistics-block uu) STAT-BS STAT-CURR))
(defsubst udp-user-bytes-received (uu)
  (aref (udp-user-statistics-block uu) STAT-BR STAT-CURR))

(defsubst udp-user-enabled (socket)
  (rassoc socket (udp-user-socket-alist *udp-stream*)))

;;;Peek item for a TCP socket

(defop (udp-socket :peek-item) (elt)
  (let ((socket (cdr elt)))
    (list `(:PRE-PROCESS-FUNCTION udp-socket-insert-special-fields :socket ,socket)
          (tv:scroll-parse-item
            :leader 2
            `(:MOUSE-ITEM
               (NIL :MENU-CHOOSE
                    ("UDP socket operations"
                     ("Close" :EVAL  (when (tv:mouse-y-or-n-p "Close this UDP socket")
                                       (funcall socket :close))
                      :DOCUMENTATION
                      "Click left to close this UDP socket.")
                     ("Inspect" :EVAL (send tv:selected-window :force-kbd-input `(inspect ,socket))
                      :DOCUMENTATION
                      "Click left to INSPECT this UDP socket.")
                     ("Describe" :EVAL (send tv:selected-window :force-kbd-input `(describe ,socket))
                      :DOCUMENTATION
                      "Click left to DESCRIBE this UDP socket.")
                     ("Brief" :eval (udp-socket-level tv:item :brief)
                      :documentation
                      "Click left to choose brief display for this UDP socket.")
                     ("Normal" :eval (udp-socket-level tv:item :normal)
                      :documentation
                      "Click left to choose normal display for this UDP socket."))
                    :DOCUMENTATION
                    "Menu of things to do to this UDP socket."
                    :BINDINGS
                    ((socket ',socket)))
               :FUNCTION udp-user-keyword (,socket) NIL ("   UDP socket ~D:"))
            `(:function udp-user-local-port (,socket) NIL (" ~D"))
            `(:function udp-user-remote-port (,socket) NIL (" -> ~D"))
            `(:function ,#'(lambda (socket)
                             (if (udp-user-remote-address socket)
                                 (ip:canonical-ip (udp-user-remote-address socket))
                               nil)) (,socket) NIL (" [~A] "))
            `(:mouse-item
               (nil :buttons
                    ((nil :eval (funcall socket
                                         (if (udp-user-active-gauges socket) :kill-gauges :make-gauges))
                          :bindings ((socket ',socket)))
                     (nil :eval (ignore))
                     (nil :eval (net:gauge-menu socket (udp-user-active-gauges socket))
                          :bindings ((socket ',socket))))
                    :documentation
                    "Click left for default, right for menu"
                    ((socket ',socket)))
               :function ,#'(lambda (n) (mapcar 'car (udp-user-active-gauges n))) (,socket) NIL ("Gauges: ~A"))))))

(defun udp-socket-level (item level)
  (when (null (array-leader item (1+ tv:scroll-item-leader-offset)))
    (setf (array-leader item (1+ tv:scroll-item-leader-offset)) :brief))
  (unless (eq level (array-leader item (1+ tv:scroll-item-leader-offset)))
    (setf (array-leader item tv:scroll-item-leader-offset) nil)
    (setf (array-leader item (1+ tv:scroll-item-leader-offset)) level)))

(defun udp-socket-insert-special-fields (item &aux socket level)
  "A pre-process function to insert udp-socket specific fields in the display"
  (let ((first-item (first (tv:scroll-item-component-items item))))
    (unless (array-leader first-item tv:scroll-item-leader-offset)
      (setq level (array-leader first-item (1+ tv:scroll-item-leader-offset)))
      (setq socket (getf (tv:scroll-item-plist item) :socket))
      (setf (tv:scroll-item-component-items item)
            (nconc (list (first (tv:scroll-item-component-items item)))
                   (if (member level '(:normal)) (udp-socket-normal-fields socket))))
      (setf (array-leader first-item tv:scroll-item-leader-offset) t))))

(defun udp-socket-normal-fields (socket)
  (list
    (net:sent-statistics (udp-user-statistics-block socket) "    ")
    (net:rcvd-statistics (udp-user-statistics-block socket) "    ")
    (TV:SCROLL-PARSE-ITEM
      `(:function udp-user-packets-sent-discarded (,socket) NIL ("    Packets sent/disc ~D"))
      `(:function udp-user-packets-received-discarded (,socket) NIL (" rcvd/disc ~D"))
      `(:function udp-user-bytes-sent-discarded (,socket) NIL (" Bytes sent/disc ~D"))
      `(:function udp-user-bytes-received-discarded (,socket) NIL (" rcvd/disc ~D")))
    ))

(defop (udp-socket :open) (&key local-port remote-port remote-address (initial-gauges nil ig-p) gauge-name
                                &aux parsed-remote-address)
  (unless (udp-user-enabled self)
    (when (udp-enabled *udp-stream*)
      (assert (or (setq parsed-remote-address nil)      ;This branch always fails, but reinitializes local
                  (cond ((null remote-port)
                         ;;If no remote-port, must be no remote-address
                         (null remote-address))
                        ((setq parsed-remote-address (parse-internet-address remote-address))
                         ;;If have a remote-port, remote-address must be valid AND remote-port must be valid
                         (typep remote-port '(unsigned-byte 16)))
                        (t
                         ;;If have a remote-port and remote-address invalid, error
                         nil)))
              (remote-port remote-address)
              "~[REMOTE-PORT ~D out of range~;REMOTE-ADDRESS ~S unknown~;REMOTE-PORT and REMOTE-ADDRESS must be both nil or both non-nil~]"
              (cond ((null remote-port) 2)
                    (parsed-remote-address 0)
                    (t 1))
              (if parsed-remote-address remote-port remote-address))
      (if local-port
          (assert (and (typep local-port '(unsigned-byte 16))
                       (unused-local-port local-port remote-port))
                  (local-port)
                  "LOCAL-PORT ~D is ~:[out of range~;already in use~]"
                  local-port
                  (typep local-port '(unsigned-byte 16)))
        (setq local-port (assign-local-port)))
      (setf (udp-user-local-port self) local-port)
      (setf (udp-user-remote-port self) remote-port)
      (setf (udp-user-remote-address self) parsed-remote-address)
      (setf (udp-user-packet-list self) (make-fifo))
      (array-initialize (udp-user-statistics-block self) 0)
      (setf (udp-user-gauge-name self) (or gauge-name (format nil "UDP ~D" (udp-user-local-port self))))
      (dolist (g (udp-user-inactive-gauges self))
        (send (cdr g) :set-margin-name (udp-user-gauge-name self)))
      (when ig-p
        ;;If initial-gauges specified, NIL means no gauges, T means default, anything else specifies gauges
        ;;The reason to add no gauges is to have the statistics block looked at by the clock function
        (if (eq initial-gauges t)
            (setq initial-gauges '(:apr :aps :abr :abs)))
        (set-gauges (udp-user-statistics-block self)
                    (locf (udp-user-active-gauges self))
                    (locf (udp-user-inactive-gauges self))
                    (udp-user-gauge-name self)
                    initial-gauges)
        (add-network-statistics-block (udp-user-statistics-block self)))
      (do ((blip (pop-fifo (udp-user-packet-list self))
                 (pop-fifo (udp-user-packet-list self))))
          ((null blip))
        (unless (eq (first blip) :close)
          (free-ip-header (second blip)))
        (if (eq (first blip) :data)
            (free-udp-buffer (third blip))))
      (push (cons local-port self) (udp-user-socket-alist *udp-stream*))
      local-port)))

(defop (udp-socket :bind) (&optional remote-port remote-address &aux parsed-remote-address)
  (when (and (udp-enabled *udp-stream*) (udp-user-enabled self))
    (assert (or (setq parsed-remote-address nil)        ;This branch always fails, but reinitializes local
                (cond ((null remote-port)
                       ;;If no remote-port, must be no remote-address
                       (null remote-address))
                      ((setq parsed-remote-address (parse-internet-address remote-address))
                       ;;If have a remote-port, remote-address must be valid AND remote-port must be valid
                       (typep remote-port '(unsigned-byte 16)))
                      (t
                       ;;If have a remote-port and remote-address invalid, error
                       nil)))
            (remote-port remote-address)
            "~[REMOTE-PORT ~D out of range~;REMOTE-ADDRESS ~S unknown~;REMOTE-PORT and REMOTE-ADDRESS must be both nil or both non-nil~]"
            (cond ((null remote-port) 2)
                  (parsed-remote-address 0)
                  (t 1))
            (if parsed-remote-address remote-port remote-address))
    (when (and (null remote-port) (null remote-address))
      (unless (fifo-empty-p (udp-user-packet-list self))
        (let ((blip (first (fifo-as-list (udp-user-packet-list self)))))
          ;;Look at packet at head of packet-list to find remote address and remote port
          (when (eq (first blip) :data)
            (setq remote-port (fourth blip))
            (setq remote-address (ip:ih-source-address (second blip)))))))
    (setf (udp-user-remote-port self) remote-port)
    (setf (udp-user-remote-address self) remote-address)
    t))

(defop (udp-socket :remote-port) ()
  (and (udp-enabled *udp-stream*)
       (udp-user-enabled self)
       (udp-user-remote-port self)))

(defop (udp-socket :remote-address) ()
  (and (udp-enabled *udp-stream*)
       (udp-user-enabled self)
       (udp-user-remote-address self)))

(defop (udp-socket :local-port) ()
  (and (udp-enabled *udp-stream*)
       (udp-user-enabled self)
       (udp-user-local-port self)))

(defun unused-local-port (local-port remote-port)
  (do ((list (udp-user-socket-alist *udp-stream*)))
      ((null list) t)
    (setq list (member local-port list :key #'car))
    (if (and list (eql remote-port (udp-user-remote-port (cdar list))))
        (return nil)
      (setq list (cdr list)))))

(defun assign-local-port ()
  (do ((port (udp-next-local-port *udp-stream*) (1+ port)))
      ((unused-local-port port nil)
       (setf (udp-next-local-port *udp-stream*) (1+ port))
       (if (= (udp-next-local-port *udp-stream*) 65536)
           (setf (udp-next-local-port *udp-stream*) 256))
       port)))

(defop (udp-socket :close) (&optional abort)
  (let ((item (udp-user-enabled self)))
    (when item
      (setf (udp-user-socket-alist *udp-stream*) (remove item (udp-user-socket-alist *udp-stream*)))
      (delete-network-statistics-block (udp-user-statistics-block self))
      (send self :kill-gauges)
      (do ((buffer (pop (udp-user-receive-buffers self))
                   (pop (udp-user-receive-buffers self))))
          ((null buffer))
        (free-udp-buffer buffer))
      (if abort
          (do ((blip (pop-fifo (udp-user-packet-list self))
                     (pop-fifo (udp-user-packet-list self))))
              ((null blip))
            (free-ip-header (second blip))
            (if (eq (first blip) :data)
                (free-udp-buffer (original-array (third blip))))))
      (push-fifo (ncons :close) (udp-user-packet-list self))
      t)))

(defop (udp-socket :receive) (&optional buffer)
  (when (udp-user-enabled self)
    (when buffer
      (setq buffer (original-array buffer)))
    (if buffer
        (check-type buffer (satisfies udp-buffer-p))
      (setq buffer (get-udp-buffer)))
    (push buffer (udp-user-receive-buffers self))
    t))

(defop (udp-socket :allocate) ()
  (get-udp-buffer))

(defop (udp-socket :free) (buffer)
  (setq buffer (original-array buffer))
  (check-type buffer (satisfies udp-buffer-p))
  (free-udp-buffer buffer))

(defop (udp-socket :write-packet) (buffers ip-header &optional remote-port (checksum t))
  (unless (consp buffers)
    (setq buffers (ncons buffers)))
  (dolist (b buffers)
    (check-type b (satisfies byte-array-or-string-p)))
  (if (udp-user-remote-port self)
      (setq remote-port (udp-user-remote-port self)))
  (check-type ip-header (satisfies ip-header-p))
  (check-type remote-port (unsigned-byte 16))
  (if (udp-user-remote-address self)
      (set-destination-address ip-header (udp-user-remote-address self)))
  (setf (ip:ih-protocol ip-header) udp-protocol)
  (let ((udp-header (get-udp-header)))
    (unwind-protect
        (let ((result nil)
              (buffer-length (apply '+ (mapcar 'length buffers))))
          (push udp-header buffers)
          (setf (udp-source-port udp-header) (udp-user-local-port self))
          (setf (udp-destination-port udp-header) remote-port)
          (setf (udp-length udp-header) (+ 8 buffer-length))
          (if checksum
              (store-udp-checksum buffers ip-header (local-host-p (ip::ih-dest-address ip-header)))
            (setf (udp-checksum udp-header) 0))
          (setq result (multiple-value-list (send *udp-stream* :send buffers ip-header)))
          (cond ((or (null (car result)) (zerop (car result)))
                 (incf (udp-user-packets-sent-discarded self))
                 (incf (udp-user-bytes-sent-discarded self) buffer-length))
                (t
                 (incf (udp-user-packets-sent self))
                 (incf (udp-user-bytes-sent self) buffer-length)))
          (values-list result))
      (free-udp-header udp-header))))

(defop (udp-socket :broadcast-packet) (buffers ip-header remote-port &optional remote-network (checksum t))
  (when (udp-user-remote-address self)
    (error "Can't broadcast from a fully specified socket"))
  (unless (consp buffers)
    (setq buffers (ncons buffers)))
  (dolist (b buffers)
    (check-type b (satisfies byte-array-or-string-p)))
  (check-type ip-header (satisfies ip-header-p))
  (check-type remote-port (unsigned-byte 16))
  (when remote-network
    (check-type remote-network (unsigned-byte 32)))
  (setf (ip:ih-protocol ip-header) udp-protocol)
  (let ((udp-header (get-udp-header)))
    (unwind-protect
        (let ((result nil)
              (buffer-length (apply '+ (mapcar 'length buffers))))
          (push udp-header buffers)
          (setf (udp-source-port udp-header) (udp-user-local-port self))
          (setf (udp-destination-port udp-header) remote-port)
          (setf (udp-length udp-header) (+ 8 buffer-length))
          (setq result
                (multiple-value-list
                  (send *udp-stream* :broadcast buffers ip-header remote-network
                                     #'(lambda (header buffers)
                                         (if checksum
                                             (store-udp-checksum buffers header (local-host-p (ip:ih-dest-address header)))
                                           (setf (udp-checksum udp-header) 0))))))
          (cond ((or (null (car result)) (zerop (car result)))
                 (incf (udp-user-packets-sent-discarded self))
                 (incf (udp-user-bytes-sent-discarded self) buffer-length))
                (t
                 (incf (udp-user-packets-sent self))
                 (incf (udp-user-bytes-sent self) buffer-length)))
          (values-list result))
      (free-udp-header udp-header))))

(defop (udp-socket :listen) ()
  (not (fifo-empty-p (udp-user-packet-list self))))

(defun multiple-connection-listen (list)
  "Given a LIST of sockets, return the first stream that has data waiting"
  (dolist (socket list)
    (when (not (fifo-empty-p (udp-user-packet-list socket)))
      (return socket))))

(defun multiple-connection-kill-gauges (list)
  (let ((gauges nil))
    (without-interrupts
      (dolist (socket list)
        (do ((gauge (pop (udp-user-active-gauges socket)) (pop (udp-user-active-gauges socket))))
            ((null gauge))
          (push (cdr gauge) gauges)
          (push gauge (udp-user-inactive-gauges socket)))))
    (when gauges
      (apply 'tv:delete-network-gauges gauges))))

(defop (udp-socket :read-packet) ()
  (pop-fifo (udp-user-packet-list self)))

(defop (udp-socket :print-self) (stream &optional ignore ignore)
  (si:printing-random-object (self stream :type :no-pointer)
    (format stream
            "(~:[closed~;open~]) ~D~:[~; -> ~:*~D [~A]~]"
            (and *udp-stream*
                (rassoc self (udp-user-socket-alist *udp-stream*)))
            (udp-user-local-port self)
            (udp-user-remote-port self)
            (if (udp-user-remote-address self)
                (canonical-ip (udp-user-remote-address self))))))

(defop (udp-socket :set-gauges) (gauge-list)
  "Set the active gauges for this udp user socket"
  (when (udp-user-enabled self)
    (add-network-statistics-block (udp-user-statistics-block self))
    (set-gauges (udp-user-statistics-block self)
                (locf (udp-user-active-gauges self))
                (locf (udp-user-inactive-gauges self))
                (udp-user-gauge-name self)
                gauge-list)))

(defop (udp-socket :make-gauges) (&optional (gauge-list '(:apr :aps :abr :abs)))
  "Add selected gauges for this udp user socket"
  (when (udp-user-enabled self)
    (add-network-statistics-block (udp-user-statistics-block self))
    (add-gauges (udp-user-statistics-block self)
                (locf (udp-user-active-gauges self))
                (locf (udp-user-inactive-gauges self))
                (udp-user-gauge-name self)
                gauge-list)))

(defop (udp-socket :kill-gauges) (&optional (gauge-list '(:ipr :apr :ips :aps :ibr :abr :ibs :abs)))
  "Delete selected gauges from a udp user socket.  Default is to delete all gauges"
  (delete-gauges (locf (udp-user-active-gauges self))
                 (locf (udp-user-inactive-gauges self))
                 gauge-list))

(defun using-udp-socket (host port keyword receiver &optional (couldnt-open #'false) local-port &aux header)
  (declare (zwei:indentation 3 2))
  (if (ip-header-p host)
      (setq host (ip:ih-dest-address (setq header host)))
    (let ((original-host host))
      (assert (numberp (setq host (parse-internet-address (setq original-host host))))
              (host)
              "~S is not a valid Internet host specification"
              original-host)))
  (let ((user-socket (make-udp-socket :keyword keyword)))
    (unwind-protect
        (if (send user-socket :open :remote-port port :remote-address host :local-port local-port)
            (funcall receiver
                     user-socket
                     (or header (make-ip-header :destination host)))
          (funcall couldnt-open))
      (send user-socket :close))))

;;;IP interface

(defun udp-close (stream &optional (abort t))
  (do ()
      ((null (udp-receive-buffers stream)))
    (free-udp-buffer (pop (udp-receive-buffers stream))))
  (close-udp-sockets stream abort))

(defun udp-reset (stream)
  (close-udp-sockets stream :abort))

(defun close-udp-sockets (stream abort)
  (dolist (user (udp-user-socket-alist stream))
    (send (cdr user) :close abort)))

(defun match-udp-msg-with-socket (local-port remote-port remote-address)
  (do ((list (udp-user-socket-alist *udp-stream*) (cdr list))
       (generic-handler nil))
      ((null list) generic-handler)
    (when (= (caar list) local-port)
      (let* ((match (cdar list))
             (rport (udp-user-remote-port match)))
        (if (null rport)
            (setq generic-handler match)
          (and (= remote-port rport)
               (= remote-address (udp-user-remote-address match))
               (return match)))))))

(defun udp-icmp (type header buffer)
  (let ((user-socket (match-udp-msg-with-socket (udp-source-port buffer)
                                                (udp-destination-port buffer)
                                                (ip:ih-dest-address header))))
    (if user-socket
        (push-fifo (list type header buffer) (udp-user-packet-list user-socket))
      (free-ip-header header))))

(defun receive-udp-packet (stream buffer header local-p broadcast-p interface &aux user-socket)
  (declare (ignore interface))
  (unwind-protect
      (cond ((not (check-udp-checksum buffer header local-p))
             (incf (udp-checksum-failures stream))
             (free-ip-header header))
            ((setq user-socket (match-udp-msg-with-socket (udp-destination-port buffer)
                                                          (udp-source-port buffer)
                                                          (ip:ih-source-address header)))
             (cond ((null (udp-user-receive-buffers user-socket))
                    (incf (udp-user-packets-received-discarded user-socket))
                    (incf (udp-user-bytes-received-discarded user-socket) (- (length buffer) 8))
                    (unless broadcast-p
                      (icmp:icmp :source-quench (list header buffer)))
                    (free-ip-header header))
                   (t
                    (push-fifo (list :data
                                     header
                                     (let ((length (- (fill-pointer buffer) udp-first-data-byte)))
                                       (make-array length
                                                   :element-type '(unsigned-byte 8)
                                                   :displaced-to buffer
                                                   :displaced-index-offset udp-first-data-byte))
                                     (udp-source-port buffer)
                                     broadcast-p)
                               (udp-user-packet-list user-socket))
                    (incf (udp-user-packets-received user-socket))
                    (incf (udp-user-bytes-received user-socket) (- (length buffer) 8))
                    (setq buffer (pop (udp-user-receive-buffers user-socket))))))
            (t                                  ;(null user-socket)
             (incf (net:tp-packets-received-discarded stream))
             (incf (net:tp-bytes-received-discarded stream) (fill-pointer buffer))
             (unless broadcast-p
               (icmp:icmp :destination-unreachable (list header buffer) icmp:icmp-port-unreachable))
             (free-ip-header header)))
    (send stream :receive buffer)))

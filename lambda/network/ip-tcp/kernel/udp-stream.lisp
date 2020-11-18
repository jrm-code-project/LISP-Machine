;;; -*- Mode:LISP; Package:UDP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '(udp-stream
          udp-buffered-stream
          udp-unbuffered-stream
          *udp-stream-whostate*
          ))

;;;***UDP-STREAM

(defflavor udp-stream
         ((socket nil)                                                  ;The udp-socket
          (open nil)                                                    ;T if user has issued open
          (packet-list (make-fifo))                                     ;List of packets
          (receives-out 4)                                              ;Number of receives to keep out
          (ip-header nil)                                               ;The IP header for :write-packet
          )
         ()
  (:gettable-instance-variables socket)
  (:inittable-instance-variables socket receives-out)
  )

(defmethod (udp-stream :open) (&optional keyword &rest args)
  (setq args (copy-list args))
  (let ((open-socket (null args))
        host)
    (setq ip-header (make-ip-header))
    (when (null socket)
      (setq socket (make-udp-socket :keyword keyword)))
    (setq open (or open-socket (apply socket :open args)))
    (when (setq host (send socket :remote-address))
      (set-destination-address ip-header host))
    (dotimes (i receives-out)
      (send socket :receive)))
  self)

(defmethod (udp-stream :remote-port) ()
  (send socket :remote-port))

(defmethod (udp-stream :remote-address) ()
  (send socket :remote-address))

(defmethod (udp-stream :local-port) ()
  (send socket :local-port))

(defmethod (udp-stream :local-address) ()
  (and ip-header (ip:ih-source-address ip-header)))

(defmethod (udp-stream :close) (&optional mode)
  (when open
    (setq open nil)
    (apply socket :close (ncons mode)))
  (setq socket nil))

(defmethod (udp-stream :handle-replies) (&optional no-hang-p)
  (loop
    (cond ((send socket :listen)                                        ;Activity on the socket
           (let ((item (send socket :read-packet)))
             (case (first item)
               (:data
                (send self :read-reply
                           (third item)
                           (ip:ih-source-address (second item))
                           (fourth item))
                (return :read-reply))
               (:close                                                  ;Socket closed out from under us
                (setq open nil)
                (return :local-close))
               ((:network-unreachable :host-unreachable :protocol-unreachable :port-unreachable)
                (return :unreachable))
               (otherwise
                ;;Ignore it
                ))))
          (no-hang-p                                                    ;No activity and no-hang
           (return nil))
          (t                                                            ;No activity -- wait
           (wait-for-reply socket)))))

(defvar *udp-stream-whostate* "UDP socket I/O" "The wait state for the wholine")

(defun wait-for-reply (socket)
  (process-wait *udp-stream-whostate*
                #'(lambda (udp-socket)
                    (send udp-socket :listen))
                socket))

(defmethod (udp-stream :handle-all-replies) ()
  (do ((count 0 (1+ count))
       (event (send self :handle-replies t)
              (send self :handle-replies t)))
      ((null event)
       (plusp count))))

(defmethod (udp-stream :read-reply) (packet source-address source-port)
  (push-fifo (list packet source-address source-port) packet-list))

(defmethod (udp-stream :read-packet) (&optional pkt (start 0) end)
  (declare (values packet length remote-port remote-address))
  (loop
    (unless open
      (return nil))
    (let ((blip (pop-fifo packet-list)))
      (when blip
        (let* ((data (first blip))
               (data-length (length data)))
          (cond (pkt                            ;User gave us a packet
                 (unless end
                   (setq end (array-length pkt)))
                 (setq data-length (min data-length (- end start)))
                 (copy-array-portion data 0 data-length pkt start end)
                 (send socket :receive data))
                (t                              ;User wants the real packet we received
                 (send socket :receive (get-udp-buffer))))
          (return (values (or pkt data) data-length (third blip) (second blip))))))
    (send self :handle-replies)))

(defmethod (udp-stream :write-packet) (pkt &optional (start 0) (end (length pkt)) remote-port remote-address)
  (unless (and (= start 0) (= end (length pkt)))
    (setq pkt (make-array (- end start)
                          :element-type '(unsigned-byte 8)
                          :displaced-to pkt
                          :displaced-index-offset start)))
  (when remote-address
    (set-destination-address ip-header (parse-internet-address remote-address)))
  (lexpr-send socket :write-packet pkt ip-header (ncons remote-port))
  t)

(defmethod (udp-stream :broadcast-packet) (pkt remote-port &optional remote-network)
  (send socket :broadcast-packet pkt ip-header remote-port remote-network))

(defmethod (udp-stream :bind) (remote-port remote-address)
  (send socket :bind remote-port remote-address))

(defmethod (udp-stream :listen) ()
  (or (not open)
      (progn (send self :handle-all-replies)
             (not (fifo-empty-p packet-list)))))

(defmethod (udp-stream :before :force-output) ()
  ;;***This is just here to make sure that a :force-output method exists
  )

(compile-flavor-methods udp-stream)

;;;***UDP-BUFFERED-STREAM

(defflavor udp-buffered-stream
         ()
         (udp-stream si:buffered-stream))

(defmethod (udp-buffered-stream :next-input-buffer) (&optional no-hang-p)
  (declare (values buffer start end))
  (loop
    (cond ((not open)
           (return nil))
          ((not (fifo-empty-p packet-list))
           (let* ((blip (pop-fifo packet-list))
                  (buffer (first blip)))
             (return (values buffer 0 (length buffer)))))
          ((send self :handle-all-replies))
          (no-hang-p
           (return nil))
          (t
           (wait-for-reply socket)))))

(defmethod (udp-buffered-stream :discard-input-buffer) (buffer)
  (funcall socket :receive buffer))

(defmethod (udp-buffered-stream :new-output-buffer) ()
  (declare (values buffer start end))
  (let ((buffer (send socket :allocate)))
    (values buffer 0 (array-length buffer))))

(defmethod (udp-buffered-stream :send-output-buffer) (buffer count)
  (setf (fill-pointer buffer) count)
  (send socket :write-packet buffer ip-header))

(defmethod (udp-buffered-stream :discard-output-buffer) (buffer)
  (send socket :free buffer))

(compile-flavor-methods udp-buffered-stream)

;;;***UDP-UNBUFFERED-STREAM

(defflavor udp-unbuffered-stream
         ((tyi-buffer nil)
          (tyo-buffer (make-array 1 :element-type '(unsigned-byte 8) :fill-pointer 1))
          (untyi-char nil)
          )
         (udp-stream si:bidirectional-stream))

(defmethod (udp-unbuffered-stream :tyi) (&optional eof)
  (loop
    (cond (untyi-char
           (return (prog1 untyi-char (setq untyi-char nil))))
          (tyi-buffer
           (let* ((blip tyi-buffer)
                  (buffer (first blip))
                  (byte (aref buffer (second blip))))
             (when (= (incf (second blip)) (length buffer))
               (setq tyi-buffer nil)
               (send socket :receive buffer))
             (return byte)))
          ((not (fifo-empty-p packet-list))
           (let ((blip (pop-fifo packet-list)))
             (setq tyi-buffer (list (first blip) 0))))
          ((not open)
           (if eof
               (global:signal 'sys:end-of-file :format-string "End of File on UDP stream")
             (return nil)))
          ((send self :handle-all-replies))
          (t
           (wait-for-reply socket)))))

(defmethod (udp-unbuffered-stream :untyi) (byte)
  (when untyi-char
    (error "Can't UNTYI more than once"))
  (setq untyi-char byte))

(defmethod (udp-unbuffered-stream :listen) ()
  (loop
    (cond (untyi-char
           (return t))
          (tyi-buffer
           (return t))
          ((not (fifo-empty-p packet-list))
           (return t))
          ((not open)
           (return t))
          ((send self :handle-all-replies))
          (t
           (return nil)))))

(defmethod (udp-unbuffered-stream :tyi-no-hang) (&optional eof)
  (loop
    (cond (untyi-char
           (return (prog1 untyi-char (setq untyi-char nil))))
          (tyi-buffer
           (let* ((blip tyi-buffer)
                  (buffer (first blip))
                  (byte (aref buffer (second blip))))
             (when (= (incf (second blip)) (length buffer))
               (setq tyi-buffer nil)
               (send socket :receive buffer))
             (return byte)))
          ((not (fifo-empty-p packet-list))
           (let ((blip (pop-fifo packet-list)))
             (setq tyi-buffer (list (first blip) 0))))
          ((not open)
           (if eof
               (global:signal 'sys:end-of-file :format-string "End of File on UDP stream")
             (return nil)))
          ((send self :handle-all-replies))
          (t
           (return nil)))))

(defmethod (udp-unbuffered-stream :tyo) (byte)
  (when open
    (setf (aref tyo-buffer 0) byte)
    (setf (fill-pointer tyo-buffer) 1)
    (send socket :write-packet tyo-buffer ip-header)
    t))

(defmethod (udp-unbuffered-stream :string-out) (string)
  (when open
    (send socket :write-packet string ip-header)
    t))

(compile-flavor-methods udp-unbuffered-stream)

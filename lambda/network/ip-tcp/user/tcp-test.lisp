;;; -*- Mode:LISP; Package:TCP; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defvar *tcp-test-socket-list* nil)
(defvar *tcp-test-process-count* 0)

(defun multi-test (processes connections-per-process &rest arguments &key (quantum 60) &allow-other-keys)
  (setq *tcp-test-socket-list* nil)
  (setq *tcp-test-process-count* 0)
  (let ((process-list nil))
    (unwind-protect
        (progn
          (dotimes (i processes)
            (let ((process (make-process (format nil "TCP TEST ~D" i)
                                         :quantum quantum
                                         :warm-boot-action 'si:process-warm-boot-reset
                                         :arrest-reasons '(:not-running))))
              (send process :preset #'(lambda (number)
                                        (apply #'tcp:test
                                               :print-statistics-p nil
                                               :connection-number number
                                               :connections connections-per-process
                                               arguments))
                                    (* i connections-per-process))
              (send process :reset)
              (send process :run-reason :enable)
              (incf *tcp-test-process-count*)
              (push process process-list)))
          (mapcar #'(lambda (x) (send x :revoke-arrest-reason :not-running)) process-list)
          (process-wait "Completion" #'(lambda () (zerop *tcp-test-process-count*))))
      (mapcar #'(lambda (x)
                  (send x :revoke-arrest-reason :not-running)
                  (send x :kill))
              process-list)
      (when *tcp-test-socket-list*
        (print-statistics (apply #'append *tcp-test-socket-list*)))))
  t)

(defun test (&key total-sends send-size sends-out receive-size receives-out (check-data-p nil)
             (connections 1) (open :both) (sender :init) (receiver :listen) auto-push (optimistic t)
             listen-port init-port host (send-gauges nil) (receive-gauges nil)
             (print-statistics-p t) (connection-number 0) &allow-other-keys
             &aux listen-gauges init-gauges sender-sockets receiver-sockets
             (my-address (parse-internet-address si:local-host)))
  (declare (special total-sends send-size sends-out receive-size receives-out check-data-p))
  (check-type open (member :init :listen :both))
  (setq listen-gauges (and (member open '(:listen :both))
                           (append (if (member sender '(:listen :both)) send-gauges)
                                   (if (member receiver '(:listen :both)) receive-gauges))))
  (setq init-gauges (and (member open '(:init :both))
                         (append (if (member sender '(:init :both)) send-gauges)
                                 (if (member receiver '(:init :both)) receive-gauges))))
  (setq host (parse-internet-address host))
  (case open
    (:listen
     (check-type listen-port (or null (unsigned-byte 16)))
     (if host
         (check-type init-port (unsigned-byte 16))
       (check-type init-port null))
     (check-type sender (member nil :listen))
     (check-type receiver (member nil :listen)))
    (:init
     (check-type init-port (or null (unsigned-byte 16)))
     (check-type listen-port (unsigned-byte 16))
     (unless host
       (setq host my-address))
     (check-type sender (member nil :init))
     (check-type receiver (member nil :init)))
    (:both
     (check-type init-port (or null (unsigned-byte 16)))
     (check-type listen-port (or null (unsigned-byte 16)))
     (unless host
       (setq host my-address))
     (check-type sender (member nil :listen :init :both))
     (check-type receiver (member nil :listen :init :both))))
  (when sender
    (unless total-sends (setq total-sends 100))
    (unless sends-out (setq sends-out 1))
    (check-type total-sends (and integer (satisfies plusp)))
    (assert (or (null send-size)
                (and (consp send-size) (integerp (first send-size)) (plusp (first send-size)))
                (and (integerp send-size) (plusp send-size)))
            (send-size)
            "Send-size, ~A, is not a positive integer, a list containing a positive integer, or NIL"
            send-size)
    (check-type sends-out (and integer (satisfies plusp))))
  (when receiver
    (unless receives-out (setq receives-out 1))
    (assert (or (null receive-size)
                (and (consp receive-size) (integerp (first receive-size)) (plusp (first receive-size)))
                (and (integerp receive-size) (plusp receive-size)))
            (receive-size)
            "Receive-size, ~A, is not a positive integer, a list containing a positive integer, or NIL"
            receive-size)
    (check-type receives-out (and integer (satisfies plusp))))
  (dotimes (i connections)
    (let (listen-socket listen-keywords init-socket init-keywords)
      (when (member open '(:listen :both))
        (setq listen-keywords (append `(:open
                                         :send-timeout ,(* 60 60)
                                         :gauge-name ,(format nil "LSTN ~D" (+ connection-number i))
                                         :auto-push ,auto-push
                                         :optimistic ,optimistic)
                                      (if listen-port `(:local-port ,listen-port))
                                      (if init-port `(:remote-port ,init-port :remote-address ,host))
                                      (if listen-gauges `(:initial-gauges ,listen-gauges))))
        (setq listen-socket (make-tcp-socket :keyword (format nil "LSTN ~D" (+ connection-number i))))
        (setq listen-port (apply listen-socket listen-keywords))
        (when (member sender '(:listen :both))
          (push listen-socket sender-sockets))
        (when (member receiver '(:listen :both))
          (push listen-socket receiver-sockets)))
      (when (member open '(:init :both))
        (setq init-keywords (append `(:open
                                       :send-timeout ,(* 60 60)
                                       :gauge-name ,(format nil "INIT ~D" (+ connection-number i))
                                       :remote-port ,listen-port
                                       :remote-address ,host
                                       :active t
                                       :auto-push ,auto-push
                                       :optimistic ,optimistic)
                                    (if init-port `(:local-port ,init-port))
                                    (if init-gauges `(:initial-gauges ,init-gauges))))
        (setq init-socket (make-tcp-socket :keyword (format nil "INIT ~D" (+ connection-number i))))
        (apply init-socket init-keywords))
      (when (member sender '(:init :both))
        (push init-socket sender-sockets))
      (when (member receiver '(:init :both))
        (push init-socket receiver-sockets))
      (incf listen-port)))
  (let ((final-sockets nil))
    (unwind-protect
        (setq final-sockets
              (handle-sockets sender-sockets receiver-sockets print-statistics-p (eql my-address host)))
      (if print-statistics-p
          (setq *tcp-test-socket-list* final-sockets)
        (without-interrupts
          (push final-sockets *tcp-test-socket-list*)
          (decf *tcp-test-process-count*)))))
  t)

(defstruct (socket-statistics
             (:conc-name "SS-"))
  (open-reply-time nil)
  (close-reply-time nil)
  (closed nil)
  (total-sends 0)
  (sends-out 0)
  (last-send-byte 0)
  (total-bytes-sent 0)
  (total-receives 0)
  (receives-out 0)
  (last-receive-byte 0)
  (total-bytes-received 0))

(defun handle-sockets (send-list receive-list print-statistics-p loopback-p
                       &aux (socket-fifo (make-fifo)) socket-list opens-out)
  (declare (special total-sends send-size sends-out receive-size receives-out check-data-p))
  (declare (special socket-list))
  ;;Build a fifo containing one copy of each socket
  (dolist (socket send-list)
    (move-to-end-of-fifo socket socket-fifo))
  (dolist (socket receive-list)
    (move-to-end-of-fifo socket socket-fifo))
  (setq opens-out (length (fifo-as-list socket-fifo)))
  (setq socket-list (nreverse (mapcar #'(lambda (x) (cons x (make-socket-statistics)))
                                      (fifo-as-list socket-fifo))))
  (unwind-protect
      (do ()
          ((fifo-empty-p socket-fifo))
        (let ((socket nil)                      ;The socket we are currently looking at
              (elt nil))                        ;The read-data element
          (process-wait "Data"
                        #'(lambda () (or socket
                                         (setq socket
                                               (multiple-connection-listen (fifo-as-list socket-fifo))))))
          ;;Found a socket; look at another one first next time
          (move-to-end-of-fifo socket socket-fifo)
          (setq elt (send socket :read-data))
          (case (first elt)
            (:open
             (handle-open-reply socket)
             (decf opens-out)
             (when (zerop opens-out)
               (startup-io receive-list send-list)))
            (:data
             (handle-read-reply socket elt))
            (:write-reply
             (handle-write-reply socket elt))
            (:closing
             (handle-closing socket elt)
             (unless (member socket send-list)
               (send socket :close :normal)))
            ((:reset :closed)
             (let ((stats (cdr (assoc socket socket-list :test #'eq)))
                   (now (zl:time)))
               (unless (ss-open-reply-time stats)
                 (setf (ss-open-reply-time stats) now))
               (setf (ss-close-reply-time stats) now)
               (remove-from-fifo socket socket-fifo)))
            (:timeout
             (if loopback-p
                 (send socket :reset-timeout)
               (let ((stats (cdr (assoc socket socket-list :test #'eq)))
                     (now (zl:time)))
                 (cond ((ss-open-reply-time stats)
                        (setf (ss-closed stats) t)
                        (send socket :abort))
                       (t
                        (setf (ss-open-reply-time stats) now)
                        (setf (ss-close-reply-time stats) now)
                        (remove-from-fifo socket socket-fifo)
                        (decf opens-out)
                        (when (zerop opens-out)
                          (startup-io receive-list send-list))))))))))
    (multiple-connection-kill-gauges (mapcar #'car socket-list))
    (let ((now (zl:time)))
      (dolist (socket (fifo-as-list socket-fifo))
        (let ((stats (cdr (assoc socket socket-list :test #'eq))))
          (unless (ss-open-reply-time stats)
            (setf (ss-open-reply-time stats) now))
          (setf (ss-close-reply-time stats) now)
          (send socket :abort))))
    (when print-statistics-p
      (print-statistics socket-list)))
  socket-list)

(defun handle-open-reply (socket)
  (declare (special socket-list))
  (let ((stats (cdr (assoc socket socket-list :test #'eq))))
    (setf (ss-open-reply-time stats) (zl:time))))

(defun startup-io (receive-list send-list)
  (declare (special total-sends send-size sends-out receive-size receives-out check-data-p))
  (declare (special socket-list))
  (dolist (socket receive-list)
    (let ((stats (cdr (assoc socket socket-list :test #'eq)))
          (size))
      (multiple-value-bind (default-receive-size ignore)
          (send socket :mss)
        (setq size (cond ((null receive-size)
                          default-receive-size)
                         ((consp receive-size)
                          (* (car receive-size) default-receive-size))
                         (t
                          receive-size))))
      (dotimes (i receives-out)
        (send socket :receive (make-array size
                                          :element-type '(unsigned-byte 8)
                                          :fill-pointer 0))
        (incf (ss-receives-out stats)))))
  (dolist (socket send-list)
    (let ((stats (cdr (assoc socket socket-list :test #'eq)))
          (size))
      (multiple-value-bind (ignore default-send-size)
          (send socket :mss)
        (setq size (cond ((null send-size)
                          default-send-size)
                         ((consp send-size)
                          (* (car send-size) default-send-size))
                         (t
                          send-size))))
      (dotimes (i (min sends-out total-sends))
        (let ((buffer (make-array size
                                  :element-type '(unsigned-byte 8)
                                  :fill-pointer size)))
          (when check-data-p
            (do ((i 0 (1+ i))
                 (byte (ss-last-send-byte stats) (1+ byte)))
                ((= i size)
                 (setf (ss-last-send-byte stats) (logand byte #xff)))
              (setf (aref buffer i) byte)))
          (send socket :write-data buffer))
        (incf (ss-sends-out stats))))))

(defun handle-read-reply (socket elt)
  (declare (special total-sends send-size sends-out receive-size receives-out check-data-p))
  (declare (special socket-list))
  (let* ((stats (cdr (assoc socket socket-list :test #'eq)))
         (buffer (second elt))
         (size (length buffer)))
    (decf (ss-receives-out stats))
    (incf (ss-total-receives stats))
    (incf (ss-total-bytes-received stats) size)
    (when check-data-p
      (do ((i 0 (1+ i))
           (byte (ss-last-receive-byte stats) (logand (1+ byte) #xff)))
          ((= i size)
           (setf (ss-last-receive-byte stats) (logand byte #xff)))
        (unless (= (aref buffer i) byte)
          (error "data error!"))))
    (unless (or (ss-closed stats) (eq (third elt) :eof))
      (send socket :receive buffer)
      (incf (ss-receives-out stats)))))

(defun handle-write-reply (socket elt)
  (declare (special total-sends send-size sends-out receive-size receives-out check-data-p))
  (declare (special socket-list))
  (let* ((stats (cdr (assoc socket socket-list :test #'eq)))
         (buffer (second elt))
         (count (1+ (ss-total-sends stats))))
    (decf (ss-sends-out stats))
    (incf (ss-total-sends stats))
    (incf (ss-total-bytes-sent stats) (length buffer))
    (cond ((ss-closed stats))
          ((<= count (- total-sends sends-out))
           (when check-data-p
             (do ((i 0 (1+ i))
                  (size (length buffer))
                  (byte (ss-last-send-byte stats) (1+ byte)))
                 ((= i size)
                  (setf (ss-last-send-byte stats) (logand byte #xff)))
               (setf (aref buffer i) byte)))
           (send socket :write-data buffer)
           (incf (ss-sends-out stats)))
          ((< count total-sends))
          ((= count total-sends)
           (send socket :close :normal))
          (t
           (error "too many sends?")))))

(defun handle-closing (socket elt)
  (declare (special socket-list))
  (let* ((stats (cdr (assoc socket socket-list :test #'eq))))
    (decf (ss-receives-out stats) (length (second elt)))))

(defun print-statistics (socket-list)
  (let ((total-bytes-sent 0)
        (total-sends 0)
        (total-bytes-received 0)
        (total-receives 0)
        (earliest-start nil)
        (latest-end nil))
    (dolist (x socket-list)
      (let* ((socket (car x))
             (stats (cdr x))
             (interval (/ (float (time-difference (ss-close-reply-time stats)
                                                  (ss-open-reply-time stats)))
                          60.0))
             (sends (ss-total-sends stats))
             (receives (ss-total-receives stats))
             (bytes-sent (ss-total-bytes-sent stats))
             (bytes-received (ss-total-bytes-received stats)))
        (when (or (null earliest-start)
                  (time-lessp (ss-open-reply-time stats) earliest-start))
          (setq earliest-start (ss-open-reply-time stats)))
        (when (or (null latest-end)
                  (time-lessp latest-end (ss-close-reply-time stats)))
          (setq latest-end (ss-close-reply-time stats)))
        (incf total-sends sends)
        (incf total-receives receives)
        (incf total-bytes-sent bytes-sent)
        (incf total-bytes-received bytes-received)
        (format t "~& ~A: ~10T~,2F seconds;~27T~9D bytes sent in ~4D requests: ~,2F Bytes/Second (~,4F kbs)"
                (tcp-user-gauge-name socket)
                interval
                bytes-sent
                sends
                (if (zerop interval) "****" (/ bytes-sent interval))
                (if (zerop interval) "****" (/ (* bytes-sent 8.0) interval 1000.0)))
        (format t "~&~27T~9D bytes rcvd in ~4D requests: ~,2F Bytes/Second (~,4F kbs)"
                bytes-received
                receives
                (if (zerop interval) "****" (/ bytes-received interval))
                (if (zerop interval) "****" (/ (* bytes-received 8.0) interval 1000.0)))))
    (let ((max-interval (/ (float (time-difference latest-end earliest-start)) 60.0)))
      (format t "~&~%~A: ~10T~,2F seconds;~27T~9D bytes sent in ~4D requests: ~,2F Bytes/Second (~,4F kbs)"
              "TOTALS"
              max-interval
              total-bytes-sent
              total-sends
              (if (zerop max-interval) "****" (/ total-bytes-sent max-interval))
              (if (zerop max-interval) "****" (/ (* total-bytes-sent 8.0) max-interval 1000.0)))
      (format t "~&~27T~9D bytes rcvd in ~4D requests: ~,2F Bytes/Second (~,4F kbs)"
              total-bytes-received
              total-receives
              (if (zerop max-interval) "****" (/ total-bytes-received max-interval))
              (if (zerop max-interval) "****" (/ (* total-bytes-received 8.0) max-interval 1000.0))))))

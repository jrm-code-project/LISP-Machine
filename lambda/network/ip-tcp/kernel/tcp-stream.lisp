;;; -*- Mode:LISP; Package:TCP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '(tcp-buffered-stream
          tcp-auto-buffered-stream
          tcp-unbuffered-stream
          *tcp-stream-whostate*
          ))

(defresource simple-art-8b-buffer (size)
  :constructor (make-array size :element-type '(unsigned-byte 8.) :fill-pointer 0)
  :matcher (= size (array-length object)))

;;;***TCP-STREAM-MIXIN

(defflavor tcp-stream-mixin
         ((socket nil)                          ;The tcp-socket
          (open nil)                            ;T if user has issued open
          (closing nil)                         ;T if remote side has closed
          (auto-push nil)                       ;T if open with auto-push
          (bytes-read 0)                        ;total bytes read on this socket
          (bytes-written 0)                     ;total bytes written on this socket
          (timeout nil)                         ;T if send timed out
          (urgent-output nil)                   ;T if in Urgent mode for output
          (urgent-input nil)                    ;T if in Urgent mode for input
          (urgent-offset nil)                   ;offset of urgent data
          )
         ()
  (:method-combination (:daemon-with-or :base-flavor-last :listen))
  (:gettable-instance-variables socket urgent-output urgent-input)
  (:settable-instance-variables urgent-output)
  (:inittable-instance-variables socket auto-push)
  )

(defmethod (tcp-stream-mixin :open) (&optional keyword &rest args)
  (setq args (copy-list args))
  (let* ((host-list (member :remote-address args :test #'eq))
         (host (second host-list))
         (user-timeout (member :send-timeout args :test #'eq))
         (current-timeout (* 30 60))
         (open-socket (null args)))
    (setq args (append args `(:auto-push ,auto-push)))
    (if user-timeout
        (setq current-timeout (second user-timeout))
      (setq args (append args `(:send-timeout ,current-timeout))))
    (when (null socket)
      (setq socket (make-tcp-socket :keyword keyword)))
    (setq closing nil)
    (cond (open-socket                          ;Is socket already open?
           (setq open t)
           (send self :build-buffers)
           self)
          (t                                    ;If user specified open keywords, open socket
           (flet ((host-name (host)
                    (and (stringp host)
                         (not (global:string-search-not-set "0123456789" host))
                         (setq host (global:parse-number host)))
                    (cond ((numberp host)
                           (let ((object (si:get-host-from-address host :internet)))
                             (if object
                                 (send object :name)
                               (canonical-ip host))))
                          ((typep host 'si:host)
                           (send host :name))
                          (t
                           host))))
             (loop
               (setq open (apply socket :open args))
               (if (tcp:tcp-user-active socket)
                   (ecase (send self :handle-replies)
                     (:open                     ;All is well!
                      (return self))
                     (:unreachable
                      (cerror "Try again." "Remote Host ~S is unreachable" (host-name host)))
                     (:reset
                      (cerror "Try again." "Connection refused by ~S" (host-name host)))
                     (:timeout
                      (setq open nil)
                      (send socket :abort)
                      (incf current-timeout current-timeout)
                      (cerror (format nil "Try again with timeout of ~D ticks." current-timeout) "Connection timed out")
                      (let ((user-timeout (member :send-timeout args :test #'eq)))
                        (setf (second user-timeout) current-timeout))))
                 (return self))))))))

(defmethod (tcp-stream-mixin :remote-port) ()
  (send socket :remote-port))

(defmethod (tcp-stream-mixin :remote-address) ()
  (send socket :remote-address))

(defmethod (tcp-stream-mixin :local-port) ()
  (send socket :local-port))

(defmethod (tcp-stream-mixin :local-address) ()
  (send socket :local-address))

(defmethod (tcp-stream-mixin :accept) ()
  (loop
    (ecase (send self :handle-replies)
      (:open                                    ;All is well!
       (return self))
      (:timeout
       (setq open nil)
       (send socket :abort)
       (error "Listen timed out")
       (return nil)))))

(defmethod (tcp-stream-mixin :close) (&optional mode)
  (when open
    (send self :force-output)
    (setq open nil)
    (apply socket :close (ncons mode))
    (unless (eq mode :normal)
      (loop
        (case (send self :handle-replies)
          ((:reset :unreachable :closed)
           (return))
          (:timeout
           (send self :send-timeout)))))))

(defmethod (tcp-stream-mixin :abort) ()
  (setq open nil)
  (when (send socket :abort)
    (do ()
        ((eq (send self :handle-replies) :reset)))))

(defmethod (tcp-stream-mixin :handle-replies) (&optional no-hang-p)
  (loop
    (cond ((send socket :listen)                                        ;Activity on the socket
           (let ((item (send socket :read-data)))
             (case (first item)
               (:open
                (send self :build-buffers)
                (return :open))
               (:write-reply
                (incf bytes-written (fill-pointer (second item)))
                (send self :write-reply (second item))
                (return :write-reply))
               (:data
                (let ((length (fill-pointer (second item)))
                      (offset (fourth item)))
                  (when offset                  ;Remember last known offset of urgent data
                    (setq urgent-offset (+ bytes-read offset 1))
                    (setq urgent-input t))
                  (incf bytes-read length)
                  (when (eq (third item) :eof)
                    (setq closing t))
                  (send self :read-reply (second item) urgent-offset))
                (return :read-reply))
               (:urgent                         ;should signal this somehow...
                (setq urgent-input t))
               (:closing                                                ;Remote side has closed
                (setq closing t)
                (dolist (x (second item))
                  (send self :discard-buffer x))
                (return :remote-close))
               (:reset
                (setq closing t)
                (setq open nil)
                (dolist (x (second item))
                  (send self :discard-buffer x))
                (dolist (b (third item))
                  (send self :write-reply b))
                ;;(cerror "Continue, treating as end-of-file" "Connection reset remotely")
                (return :reset))
               (:close                                                  ;Socket closed out from under us
                (setq closing t)
                (setq open nil)
                (dolist (x (second item))
                  (send self :discard-buffer x))
                ;;(cerror "Continue, treating as end-of-file" "Connection reset locally")
                (return :local-close))
               ((:network-unreachable :host-unreachable :protocol-unreachable :port-unreachable)
                (setq closing t)
                (setq open nil)
                (send socket :abort)
                (return :unreachable))
               (:timeout
                (setq timeout t)
                (return :timeout))
               (:closed
                (return :closed))
               (otherwise
                ;;Ignore it
                ))))
          (no-hang-p                                                    ;No activity and no-hang
           (return nil))
          (t                                                            ;No activity -- wait
           (send self :wait-for-reply)))))

(defvar *tcp-stream-whostate* "TCP socket I/O" "The wait state for the wholine")

(defmethod (tcp-stream-mixin :wait-for-reply) (&optional function &rest args)
  (process-wait *tcp-stream-whostate*
                #'(lambda (tcp-socket func arg-list)
                    (or (send tcp-socket :listen)
                        (and func (apply func arg-list))))
                socket function args))

(defmethod (tcp-stream-mixin :send-timeout) (&aux ok)
  (unwind-protect
      (progn
        (cerror "Reset timeout and continue" "Send timed out")
        (send socket :reset-timeout)
        (setq timeout nil)
        (setq ok t))
    (unless ok
      (send self :abort))))

(defmethod (tcp-stream-mixin :handle-all-replies) ()
  (do ((count 0 (1+ count))
       (event (send self :handle-replies t)
              (send self :handle-replies t)))
      ((null event)
       (plusp count))))

(defmethod (tcp-stream-mixin :or :listen) ()
  (or timeout closing))

;;;***TCP-BUFFERED-STREAM

(defflavor tcp-buffered-stream
         ((input-buffer-size 0)                 ;Local MSS
          (input-buffer-fifo nil)               ;List of full input buffers
          (input-buffer-limit 4)                ;Maximum number of input buffers
          (output-buffer-size 0)                ;Remote MSS
          (output-buffer-list nil)              ;Free list of output buffers
          (output-buffer-limit 4)               ;Maximum number of output buffers
          )
           (tcp-stream-mixin si:buffered-stream)
  (:settable-instance-variables input-buffer-limit output-buffer-limit)
  (:inittable-instance-variables input-buffer-limit output-buffer-limit)
  )

(defmethod (tcp-buffered-stream :before :open)  (&rest ignore)
  (setq input-buffer-fifo (make-fifo))
  (setq output-buffer-list nil)
  )

(defmethod (tcp-buffered-stream :build-buffers) ()
  (multiple-value-bind (send receive)
      (send socket :mss)
    (setq input-buffer-size receive)
    (setq output-buffer-size send))
  (dotimes (i input-buffer-limit)
    (let ((buffer (allocate-resource 'simple-art-8b-buffer input-buffer-size)))
      (unless (send socket :receive buffer)
        (deallocate-resource 'simple-art-8b-buffer buffer))))
  (dotimes (i output-buffer-limit)
    (let ((buffer (allocate-resource 'simple-art-8b-buffer output-buffer-size)))
      (push buffer output-buffer-list))))

(defmethod (tcp-buffered-stream :build-more-buffers) (&optional (new-input-limit input-buffer-limit)
                                                                (new-output-limit output-buffer-limit))
  (when (> new-input-limit input-buffer-limit)
    (dotimes (i (- new-input-limit input-buffer-limit))
      (let ((buffer (allocate-resource 'simple-art-8b-buffer input-buffer-size)))
        (unless (send socket :receive buffer)
          (deallocate-resource 'simple-art-8b-buffer buffer))))
    (setq input-buffer-limit new-input-limit))
  (when (> new-output-limit output-buffer-limit)
    (dotimes (i (- new-output-limit output-buffer-limit))
      (let ((buffer (allocate-resource 'simple-art-8b-buffer output-buffer-size)))
        (push buffer output-buffer-list)))
    (setq output-buffer-limit new-output-limit)))

(defmethod (tcp-buffered-stream :discard-buffer) (buffer)
  (deallocate-resource 'simple-art-8b-buffer buffer))

(defmethod (tcp-buffered-stream :write-reply) (buffer)
  (push buffer output-buffer-list))

(defmethod (tcp-buffered-stream :read-reply) (buffer offset)
  (push-fifo (cons buffer offset) input-buffer-fifo))

(defmethod (tcp-buffered-stream :next-input-buffer) (&optional no-hang-p)
  (declare (values buffer start end))
  (loop
    (cond ((send self :handle-all-replies))
          ((not (fifo-empty-p input-buffer-fifo))
           (let* ((elt (pop-fifo input-buffer-fifo))
                  (buffer (car elt))
                  (offset (cdr elt)))
             (setq urgent-input (not (null offset)))
             (return (values buffer 0 (fill-pointer buffer)))))
          ((not open)
           (return nil))
          (closing
           (return nil))
          (no-hang-p
           (setq urgent-input (and urgent-offset (> urgent-offset bytes-read)))
           (return nil))
          (timeout
           (send self :send-timeout))
          (t
           (send self :wait-for-reply
                      #'(lambda (b o c to)
                          (or (not (fifo-empty-p (cdr b)))
                              (not (cdr o))
                              (cdr c)
                              (cdr to)))
                      (locf input-buffer-fifo)
                      (locf open)
                      (locf closing)
                      (locf timeout))))))

(defmethod (tcp-buffered-stream :discard-input-buffer) (buffer)
  (cond (closing
         (send self :discard-buffer buffer))
        ((send socket :receive buffer))
        (t
         (send self :discard-buffer buffer))))

(defmethod (tcp-buffered-stream :new-output-buffer) ()
  (declare (values buffer start end))
  (loop
    (cond ((send self :handle-all-replies))
          ((not open)
           (global:ferror 'sys:connection-closed "Connection closed" socket))
          (output-buffer-list
           (return (values (pop output-buffer-list) 0 output-buffer-size)))
          (timeout
           (send self :send-timeout))
          (t
           (send self :wait-for-reply
                 #'(lambda (x y)
                     (or (cdr x) (cdr y)))
                 (locf output-buffer-list)
                 (locf timeout))))))

(defmethod (tcp-buffered-stream :send-output-buffer) (buffer count)
  (setf (fill-pointer buffer) count)
  (unless (send socket :write-data buffer :pushed (< count output-buffer-size) :urgent urgent-output)
    (send self :discard-buffer buffer)
    (global:ferror 'sys:connection-closed "Connection closed" socket)))

(defmethod (tcp-buffered-stream :discard-output-buffer) (buffer)
  (push buffer output-buffer-list))

(defwrapper (tcp-buffered-stream :close) (ignore . body)
  `(unwind-protect
       (progn ,@body)
     (send self :discard-all-buffers)))

(defwrapper (tcp-buffered-stream :abort) (ignore . body)
  `(unwind-protect
       (progn ,@body)
     (send self :discard-all-buffers)))

(defmethod (tcp-buffered-stream :discard-all-buffers) ()
  (let ((input-buffers (fifo-as-list input-buffer-fifo))
        (output-buffers output-buffer-list))
    (setq input-buffer-fifo nil)
    (setq output-buffer-list nil)
    (dolist (x input-buffers)
      (send self :discard-buffer (car x)))
    (dolist (x output-buffers)
      (send self :discard-buffer x))))

(compile-flavor-methods tcp-buffered-stream)

;;;***TCP-AUTO-BUFFERED-STREAM

(defflavor tcp-auto-buffered-stream
          ((force-output-p t))                  ;T if force-output after each write
          (tcp-buffered-stream)
  (:default-init-plist :auto-push t)
  (:settable-instance-variables force-output-p))

(defmethod (tcp-auto-buffered-stream :after :tyo) (&rest ignore)
  (when force-output-p
    (send self :force-output)))

(defmethod (tcp-auto-buffered-stream :after :string-out) (&rest ignore)
  (when force-output-p
    (send self :force-output)))

(compile-flavor-methods tcp-auto-buffered-stream)

;;;***TCP-UNBUFFERED-STREAM

(defflavor tcp-unbuffered-stream
         ((tyi-buffer nil)
          (tyo-buffer nil)
          (untyi-char nil)
          )
         (tcp-stream-mixin si:bidirectional-stream))

(defmethod (tcp-unbuffered-stream :build-buffers) ()
  (setq tyi-buffer nil)
  (send socket :receive (make-array 1 :element-type '(unsigned-byte 8) :fill-pointer 0))
  (setq tyo-buffer (make-array 1 :element-type '(unsigned-byte 8) :fill-pointer 1))
  (setq untyi-char nil))

(defmethod (tcp-unbuffered-stream :discard-buffer) (ignore)
  )

(defmethod (tcp-unbuffered-stream :write-reply) (buffer)
  (setq tyo-buffer buffer))

(defmethod (tcp-unbuffered-stream :read-reply) (buffer offset)
  (declare (ignore offset))
  (setq tyi-buffer buffer))

(defmethod (tcp-unbuffered-stream :tyi) (&optional eof)
  (loop
    (cond ((send self :handle-all-replies))
          (untyi-char
           (return (prog1 untyi-char (setq untyi-char nil))))
          (tyi-buffer
           (let* ((buffer tyi-buffer)
                  (byte (aref buffer 0)))
             (setq tyi-buffer nil)
             (send socket :receive buffer)
             (setq urgent-input (and urgent-offset (>= urgent-offset bytes-read)))
             (return byte)))
          ((or closing (not open))
           (if eof
               (global:signal 'sys:end-of-file :format-string "End of File on TCP stream")
             (return nil)))
          (timeout
           (send self :send-timeout))
          (t
           (send self :wait-for-reply
                      #'(lambda (u b o c to)
                          (or (cdr u) (cdr b) (not (cdr o)) (cdr c) (cdr to)))
                      (locf untyi-char)
                      (locf tyi-buffer)
                      (locf open)
                      (locf closing)
                      (locf timeout))))))

(defmethod (tcp-unbuffered-stream :untyi) (byte)
  (when untyi-char
    (error "Can't UNTYI more than once"))
  (setq untyi-char byte))

(defmethod (tcp-unbuffered-stream :listen) ()
  (loop
    (unwind-protect
        (cond ((send self :handle-all-replies))
              (untyi-char
               (return t))
              (tyi-buffer
               (return t))
              ((or closing (not open))
               (return t))
              (timeout
               (return t))
              (t
               (return nil)))
      (setq urgent-input (and urgent-offset (>= urgent-offset bytes-read))))))

(defmethod (tcp-unbuffered-stream :tyi-no-hang) (&optional eof)
  (loop
    (cond ((send self :handle-all-replies))
          (untyi-char
           (return (prog1 untyi-char (setq untyi-char nil))))
          (tyi-buffer
           (let* ((buffer tyi-buffer)
                  (byte (aref buffer 0)))
             (setq tyi-buffer nil)
             (send socket :receive buffer)
             (setq urgent-input (and urgent-offset (>= urgent-offset bytes-read)))
             (return byte)))
          ((or closing (not open))
           (if eof
               (global:signal 'sys:end-of-file :format-string "End of File on TCP stream")
             (return nil)))
          (timeout
           (send self :send-timeout))
          (t
           (return nil)))))

(defmethod (tcp-unbuffered-stream :tyo) (byte)
  (loop
    (cond ((send self :handle-all-replies))
          ((not open)
           (return nil))
          (tyo-buffer
           (let ((buffer tyo-buffer))
             (setq tyo-buffer nil)
             (setf (aref buffer 0) byte)
             (setf (fill-pointer buffer) 1)
             (send socket :write-data buffer :urgent urgent-output)
             (return t)))
          (timeout
           (send self :send-timeout))
          (t
           (send self :wait-for-reply
                      #'(lambda (b o to)
                          (or (cdr b) (not (cdr o)) (cdr to)))
                      (locf tyo-buffer)
                      (locf open)
                      (locf timeout))))))

(compile-flavor-methods tcp-unbuffered-stream)

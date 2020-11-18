;;; -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

This random access disk protocol gives us about 20Kbytes/second. About 17 minutes to transfer a band
of 20Kblocks using SI:COPY-DISK-PARTITION. We could do twice as well with a simple disk partition
stream and stream-copy-until-eof.

|#

(define-network-service *tcp-disk-service* :disk :tcp "Read and write disk blocks"
  :toplevel-function 'serial-stream-disk-server
  :listen-port (sym ipport-lmidisk)
  :auto-enable? t)

(define-network-function (net:get-remote-disk-unit :internet) (host unit usage &optional initp writep)
  (declare (ignore initp))
  (let ((machine (send host :name))
        (stream)
        (normalp))
    (unwind-protect
        (let ((u (make-instance 'serial-stream-disk-unit
                                :stream (setq stream (open-easy-tcp-stream
                                                           machine
                                                           (sym-value 'ipport-lmidisk)
                                                           nil
                                                           :keyword "Remote DISK access"
                                                           :input-buffers (if writep 1 16.)
                                                           :output-buffers (if writep 16. 1)))
                                :unit-number unit
                                :machine-name machine)))
          (send u :notify (format nil "Disk being hacked remotely by ~A@~A -- ~A"
                                  si:user-id (send si:local-host :name)
                                  usage))
          (prog1 u
                 (setq normalp t)))
      (or normalp (not stream) (close stream :abort t)))))

(defflavor serial-stream-disk-unit
           ((unit-number 0)
            (stream nil)
            (machine-name nil)
            (anticipate-operation nil)
            anticipate-address
            anticipated-operations
            anticipate-rqb-npages)
           ()
  :initable-instance-variables
  :gettable-instance-variables)

(defun transmit-32b (word stream)
  (do ((j 0 (+ j 8)))
      ((= j 32))
    (send stream :tyo (ldb (byte 8 j) word))))

(defun receive-32b (stream)
  (do ((j 0 (+ j 8))
       (word 0 (dpb (send stream :tyi) (byte 8 j) word)))
      ((= j 32) word)))

(defun transmit-string (string stream)
  (transmit-32b (length string) stream)
  (send stream :string-out string))

(defun receive-string (stream &optional to-string)
  (let* ((length (receive-32b stream))
         (string (or to-string (make-string length))))
    (send stream :string-in nil string)
    string))

(defmethod (serial-stream-disk-unit :notify) (string)
  (send stream :tyo #\N)
  (transmit-string string stream)
  (send stream :force-output)
  (let ((r (int-char-if-any (send stream :tyi))))
    (cond ((eq r #\R))
          (t
           (send self :remote-error r)))))

(defmethod (serial-stream-disk-unit :read) (rqb address)
  (cond ((not anticipate-operation)
         (send stream :tyo #\R)
         (transmit-32b unit-number stream)
         (transmit-32b (si:rqb-npages rqb) stream)
         (transmit-32b address stream)
         (send stream :force-output)
         (let ((r (int-char-if-any (send stream :tyi))))
           (cond ((eq r #\R)
                  (receive-string stream (si:rqb-8-bit-buffer rqb)))
                 (t
                  (send self :remote-error r)))))
        ((and (eq anticipate-operation :read)
              (= anticipate-address address)
              (= anticipate-rqb-npages (si:rqb-npages rqb)))
         (receive-string stream (si:rqb-8-bit-buffer rqb))
         (decf anticipated-operations)
         (incf anticipate-address anticipate-rqb-npages)
         (when (zerop anticipated-operations)
           (setq anticipate-operation nil)))
        (t
         (send self :anticipation-protocol-violation :read rqb address)))
  rqb)


(defmethod (serial-stream-disk-unit :write) (rqb address)
  (cond ((not anticipate-operation)
         (send stream :tyo #\W)
         (transmit-32b unit-number stream)
         (transmit-32b (si:rqb-npages rqb) stream)
         (transmit-32b address stream)
         (transmit-string (si:rqb-8-bit-buffer rqb) stream)
         (send stream :force-output)
         (let ((r (int-char-if-any (send stream :tyi))))
           (cond ((eq r #\R))
                 (t
                  (send self :remote-error r)))))
        ((and (eq anticipate-operation :write)
              (= anticipate-address address)
              (= anticipate-rqb-npages (si:rqb-npages rqb)))
         (transmit-string (si:rqb-8-bit-buffer rqb) stream)
         (decf anticipated-operations)
         (incf anticipate-address anticipate-rqb-npages)
         (when (zerop anticipated-operations)
           (setq anticipate-operation nil)
           (send stream :force-output)))
        (t
         (send self :anticipation-protocol-violation :write rqb address)))
  rqb)


(defmethod (serial-stream-disk-unit :anticipate-operations) (kind start-address n-operations rqb-npages)
  (setq anticipate-operation kind)
  (setq anticipate-address start-address)
  (setq anticipated-operations n-operations)
  (setq anticipate-rqb-npages rqb-npages)
  (send stream :tyo #\A)
  (send stream :tyo (ecase kind (:read #\R) (:write #\W)))
  (transmit-32b unit-number stream)
  (transmit-32b rqb-npages stream)
  (transmit-32b start-address stream)
  (transmit-32b n-operations stream)
  (send stream :force-output))

(defmethod (serial-stream-disk-unit :remote-error) (code)
  (cond ((null code)
         (error "serial disk unit end of file"))
        ((eq code #\E)
         (error "serial disk unit error: ~A"
                 (receive-string stream)))
        (t
         (error "internal bug, unknown error code serial disk unit: ~D" code))))


(defmethod (serial-stream-disk-unit :anticipation-protocol-violation) (op rqb address)
  (error "Protocol violation: Mismatch in anticipated operation, expected ~S of ~D pages at ~D~
              ~%But was requested to ~S of ~D pages at ~D"
          anticipate-operation
          anticipate-rqb-npages
          anticipate-address
          op
          (si:rqb-npages rqb)
          address))

(defmethod (serial-stream-disk-unit :dispose) ()
  (close stream :abort t))

(defun serial-stream-disk-server (stream)
  ;; This allows someone to set up the password database to allow disk transfers
  ;; only from certain hosts.  Add an initial password negotiation in future.
  (or (validate-network-server-password "FOO" "BAR" si:local-host)
      (return-from serial-stream-disk-server nil))
  (serial-stream-disk-server-internal stream))

(defun serial-stream-disk-server-internal (stream &aux rqb)
  (unwind-protect
      (do ((opcode))
          ((null (setq opcode (int-char-if-any (send stream :tyi)))))
        (ecase opcode
          ((#\R #\W)
           (let ((unit (receive-32b stream))
                 (n-pages (receive-32b stream))
                 (address (receive-32b stream)))
             (cond ((not rqb)
                    (setq rqb (si:get-disk-rqb n-pages)))
                   ((not (= n-pages (si:rqb-npages rqb)))
                    (si:return-disk-rqb (prog1 rqb (setq rqb nil)))
                    (setq rqb (si:get-disk-rqb n-pages))))
             (ecase opcode
               (#\R
                (si:disk-read rqb unit address)
                (send stream :tyo #\R)
                (transmit-string (si:rqb-8-bit-buffer rqb)
                                 stream))
               (#\W
                (receive-string stream (si:rqb-8-bit-buffer rqb))
                (si:disk-write rqb unit address)
                (send stream :tyo #\R)))))
          (#\A
           (let ((op (int-char-if-any (send stream :tyi)))
                 (unit (receive-32b stream))
                 (n-pages (receive-32b stream))
                 (address (receive-32b stream))
                 (n-operations (receive-32b stream)))
             (cond ((not rqb)
                    (setq rqb (si:get-disk-rqb n-pages)))
                   ((not (= n-pages (si:rqb-npages rqb)))
                    (si:return-disk-rqb (prog1 rqb (setq rqb nil)))
                    (setq rqb (si:get-disk-rqb n-pages))))
             (send stream :build-more-buffers
                          (if (eq op #\W) 16. 0)        ;Network input buffers
                          (if (eq op #\R) 16. 0))       ;Network output buffers
             (dotimes (j n-operations)
               (ecase op
                 (#\R
                  (si:disk-read rqb unit address)
                  (incf address n-pages)
                  (transmit-string (si:rqb-8-bit-buffer rqb) stream))
                 (#\W
                  (receive-string stream (si:rqb-8-bit-buffer rqb))
                  (si:disk-write rqb unit address)
                  (incf address n-pages))))))
          (#\N
           (tv:notify nil "~A" (receive-string stream))
           (send stream :tyo #\R)))
        (send stream :force-output))
    (and rqb (si:return-disk-rqb (prog1 rqb (setq rqb nil))))))

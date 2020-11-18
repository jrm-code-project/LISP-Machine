;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-

(defvar nupi-tape-physical-unit #x18)

(defun nupi-tape-rewind ()
  (let ((rqb (get-disk-rqb)))
    (nupi-disk-command rqb #x20 nupi-tape-physical-unit 0 0 nil)
    (return-disk-rqb rqb)))

(defun nupi-tape-write-filemark ()
  (let ((rqb (get-disk-rqb)))
    (nupi-disk-command rqb #x25 nupi-tape-physical-unit 0 0 nil)))

(defun nupi-tape-read (rqb n-bytes)
  (if (> n-bytes (* 1024. (rqb-npages rqb))) (ferror nil "too big"))
  (nupi-tape-command rqb #x12 nupi-tape-physical-unit 0 n-bytes)
  (nupi-transfer-count rqb)
  )

(defun nupi-tape-write (rqb n-bytes)
  (if (> n-bytes (* 1024. (rqb-npages rqb))) (ferror nil "too big"))
  (nupi-tape-command rqb #x13 nupi-tape-physical-unit 0 n-bytes)
  (nupi-transfer-count rqb))

(defflavor nupi-simple-tape-input-stream
           (rqb
            (n-bytes-per-operation 10240.)
            (eof-p nil)
            buffer-8b
            )
           (si:buffered-input-stream)
  :settable-instance-variables
  )

(defmethod (nupi-simple-tape-input-stream :after :init) (ignore)
  (if (not (zerop (ldb (byte 10. 0) n-bytes-per-operation)))
      (ferror nil "n-bytes-per-operation must be multiple of 1024."))
  (setq rqb (get-disk-rqb (floor n-bytes-per-operation 1024.)))
  (setq buffer-8b (make-array n-bytes-per-operation
                              :type :art-8b
                              :displaced-to (rqb-8-bit-buffer rqb)
                              ))
  )

(defmethod (nupi-simple-tape-input-stream :next-input-buffer) (no-hang-p)
  no-hang-p
  (cond ((null eof-p)
         (let* ((n-bytes (nupi-tape-read rqb n-bytes-per-operation))
                (status (ldb (byte 8 8) (nupi-disk-status rqb))))
           (format t "~&nupi tape status ~d. xfer ~d. " status n-bytes)
           (cond ((or (= status 114.) (= status 76.))
                  ;;got file mark before finishing transfer, or something like that
                  (setq eof-p t)
                  (values buffer-8b 0 n-bytes))
                 ((zerop status)
                  (values buffer-8b 0 n-bytes))
                 (t
                  (ferror nil "unknown status ~d." status)))))
        (t
         (setq eof-p nil)
         nil)))

(defmethod (nupi-simple-tape-input-stream :discard-input-buffer) (ignore)
  nil)

(defmethod (nupi-simple-tape-input-stream :after :close) (&rest ignore)
  (return-disk-rqb rqb))

(defflavor nupi-simple-tape-output-stream
           (rqb
            (n-bytes-per-operation 10240.)
            (done-a-write-p nil)
            )
           (si:buffered-output-stream)
  )

(defmethod (nupi-simple-tape-output-stream :after :init) (ignore)
  (if (not (zerop (ldb (byte 10. 0) n-bytes-per-operation)))
      (ferror nil "n-bytes-per-operation must be multiple of 1024."))
  (setq rqb (get-disk-rqb (floor n-bytes-per-operation 1024.))))

(defmethod (nupi-simple-tape-output-stream :new-output-buffer) ()
  (values (si:rqb-8-bit-buffer rqb) 0 n-bytes-per-operation))

(defmethod (nupi-simple-tape-output-stream :send-output-buffer) (buffer ending-index)
  (if (not (zerop (ldb (byte 10. 0) ending-index)))
      (ferror nil "can't flush the buffer except on 1K boundaries"))
  (if (not (eq buffer (rqb-8-bit-buffer rqb)))
      (ferror nil "trying to write wrong buffer"))
  (setq done-a-write-p t)
  (nupi-tape-write rqb ending-index)
  )

(defmethod (nupi-simple-tape-output-stream :discard-output-buffer) (buffer)
  buffer
  )

(defmethod (nupi-simple-tape-output-stream :after :close) (&rest ignore)
  (if done-a-write-p
      (nupi-tape-write-filemark))
  (return-disk-rqb rqb))

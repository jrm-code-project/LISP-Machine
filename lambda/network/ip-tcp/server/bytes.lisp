;;; -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Readtable:CL -*-

#||

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

This service gets you N bytes. Note: it isnt a good benchmark for some reason.

||#

(define-network-service *tcp-bytes-service* :bytes :tcp "Source of N bytes"
  :listen-port 500
  :toplevel-function 'bytes-source-server)

(defun bytes-source-server (stream)
  (let ((input (telnet:make-eof-throwing-stream stream)))
    (catch 'eof
      (do ((j 0 (1+ j))
           (string (receive-string input))
           (count (receive-32b input))
           (force-p (= 1 (send input :tyi))))
          ((= j count))
        (send stream :string-out string)
        (if force-p (send stream :force-output))))))

(defun request-bytes (stream buffer-size n-buffers force-p &optional (character #\$))
  (check-type buffer-size (integer 1 #.(expt 2 16)))
  (transmit-32b buffer-size stream)
  (dotimes (j buffer-size)
    (send stream :tyo character))
  (transmit-32b n-buffers stream)
  (transmit-32b (if force-p 1 0) stream)
  (send stream :force-output))

(defun get-bytes-test (host &optional &key (buffer-size 2048) (n-buffers 100) to-file (force-p t))
  (check-type host string)
  (with-open-stream (source-stream (open (string-append "TCP-HOST:" host ".500") :keyword "User BYTES"))
    (with-open-stream (sink-stream (if to-file (open to-file :direction :output) 'si:null-stream))
      (request-bytes source-stream buffer-size n-buffers force-p)
      (or force-p (sleep 10))
      (let ((time (zl:time)))
        (do ((buf) (offset) (limit))
            (())
          (multiple-value-setq (buf offset limit)
            (send source-stream :read-input-buffer))
          (cond ((null buf) (return nil)))
          (send sink-stream :string-out buf offset limit)
          (send source-stream :advance-input-buffer))
        (setq time (/ (time-difference (zl:time) time) 60.0))
        (format t "~&~D bytes in ~$ seconds, ~$ kbytes per second~%"
                (* buffer-size n-buffers)
                time
                (/ (* buffer-size n-buffers) time 1000))))))

;;; -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(define-network-service *tcp-time-service* :time :tcp "RFC 868"
  :listen-port (sym ipport-timeserver)
  :toplevel-function 'time-server-function
  :auto-enable? t)

(define-network-service *udp-time-service* :time :udp "RFC 868"
  :listen-port (sym ipport-timeserver)
  :toplevel-function 'udp-time-server-function
  :auto-enable? t)

(defun time-user-function (host-name)
  (check-type host-name string)
  (let ((tcp:*tcp-stream-whostate* "Close Connection"))
    (with-open-stream (stream (let ((tcp:*tcp-stream-whostate* "Open Connection"))
                                (open-easy-tcp-stream host-name
                                                      (sym-value 'ipport-timeserver)
                                                      nil
                                                      :input-buffers 1
                                                      :output-buffers 0
                                                      :keyword "TIME User")))
      (let ((tcp:*tcp-stream-whostate* "Time"))
        (receive-32b-hbf stream)))))

(defun time-server-function (stream)
  (transmit-32b-hbf (get-universal-time) stream))

(defun udp-time-server-function (stream)
  (let ((word (make-string 4))
        (time (get-universal-time)))
    (dotimes (i 4)
      (setf (aref word i) (ldb (byte 8 (- 24 (* i 8))) time)))
    (send stream :write-packet word)))

(defun transmit-32b-hbf (word stream)
  (do ((j 0 (+ j 8)))
      ((= j 32))
    (send stream :tyo (ldb (byte 8 (- 24 j)) word))))

(defun receive-32b-hbf (stream)
  (do ((j 0 (+ j 8))
       (word 0 (dpb (or (send stream :tyi)
                        (return-from receive-32b-hbf nil))
                    (byte 8 (- 24 j)) word)))
      ((= j 32) word)))

(defun time-user-function-udp (host-name)
  (check-type host-name string)
  (with-open-stream (stream (open-easy-udp-stream host-name
                                                  (sym-value 'ipport-timeserver)
                                                  nil
                                                  :keyword "TIME User"))
    (let ((pkt (make-string 4)))
      (send stream :write-packet pkt 0 4)
      (dotimes (j 10.)
        (if (send stream :listen) (return nil))
        (global:process-sleep 60. "UDP Time Reply"))
      (and (send stream :listen)
           (send stream :read-packet pkt)
           (with-input-from-string (s pkt)
             (receive-32b-hbf s))))))

(define-network-function (net:get-host-time :internet) (host)
  (when (wait-for-tcp-enabled (* 60. 30.))
    (time-user-function (send host :name))))

;;; -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(define-network-service *tcp-finger-service* :finger :tcp "RFC 742"
  :listen-port (sym ipport-finger)
  :toplevel-function 'finger-server-function
  :auto-enable? t)

(defun finger-server-function (stream)
  (let ((arg (make-array 80 :element-type 'string-char :fill-pointer 0))
        (ascii-output (ftp:make-ascii-translating-output-stream stream nil)))
    (do ((j 0 (1+ j))
         (c))
        ((or (= j 80)
             (null (setq c (send stream :tyi)))
             (= c #o15)))
      (vector-push-extend c arg))
    (cond ((or (zerop (length arg))
               (string-equal arg si:user-id))
           (finger-server-info ascii-output)
           (terpri ascii-output)
           (send stream :force-output))
          (t
           (format ascii-output "?????????~%")))))


(defun finger-server-info (&optional (stream *standard-output*))
  (let ((idle (floor (time-difference (zl:time) tv:kbd-last-activity-time) 3600)))
    (format stream
            "~6A ~C ~22A ~6A ~:[    ~3*~;~:[~D:~2,48D~;  ~*~D~]~]     ~A"
            si:user-id
            fs:user-group-affiliation
            fs:user-personal-name-first-name-first
            (chaos:user-activity-string)
            (not (zerop idle))
            (zerop (floor idle 60.))
            (floor idle 60.)
            (rem idle 60.)
            si:local-finger-location)))

(define-network-function (net:finger-host :internet) (host user stream style)
  (finger-internet-host host user stream style))


(defun finger-internet-host (host user &optional (ostream *standard-output*) style)
  (let ((tcp:*tcp-stream-whostate* "Close Connection"))
    (with-open-stream (stream (let ((tcp:*tcp-stream-whostate* "Open Connection"))
                                (open-easy-tcp-stream (send (si:parse-host host) :name)
                                                      (sym-value 'ipport-finger)
                                                      nil
                                                      :keyword "FINGER User"
                                                      :input-buffers 1
                                                      :output-buffers 1)))
      (let ((ascii-output (ftp:make-ascii-translating-output-stream stream nil))
            (ascii-input (ftp:make-ascii-translating-input-stream stream nil))
            (tcp:*tcp-stream-whostate* "Finger"))
        (when (and user (> (string-length user) 0))
          (princ user ascii-output))
        (terpri ascii-output)
        (send stream :force-output)
        (format ostream "~2&")
        (when (eq style :brackets)
          (format ostream "[~A]~%" (send (si:parse-host host) :name)))
        (global:stream-copy-until-eof ascii-input ostream)))))

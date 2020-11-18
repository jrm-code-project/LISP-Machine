;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-

(defun receive-band-from-unix (from-machine from-dev to-unit to-part)
  (with-open-stream (out-stream (make-partition-output-stream :partition-name to-part
                                                          :unit to-unit
                                                          :byte-size 8.))
    (with-open-stream (in-stream (chaos:open-stream from-machine
                                                    (format nil "SENDBAND ~a 0 0" from-dev)))
      (stream-copy-until-eof in-stream out-stream))))

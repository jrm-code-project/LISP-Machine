;;; -*- Mode:LISP; Package:BT; Base:10; Readtable:CL -*-


(defvar *bt-block-list*)
(defconst *bt-ethernet-type* 9.)
(defconst *bt-data-offset* 32.)
(defvar *bt-server-needed* nil)
(defstruct (bt-pkt (:type :array))
  block-number-lo
  block-number-hi)
(defvar *fake-pkt* nil)

(defun bt-pkt-receiver (unit-name part n-blocks &aux rqb)
  (when (assq *bt-ethernet-type* chaos:ethernet-pkt-handler-alist)
    (ferror nil "already have handler for ~s" *bt-ethernet-type*))
  ;;don't want to use make-list, since that would be cdr coded
  (setq *bt-block-list* (do ((i 0 (1+ i))
                             (l nil (cons nil l)))
                            ((= i n-blocks) l)))
  (si:with-decoded-disk-unit (unit unit-name "Write" nil t)
    (multiple-value-bind (first-block n-blocks)
        (si:find-disk-partition part nil unit)
      (when (null first-block)
        (ferror nil "can't find partition ~s" part))
      (setq rqb (si:get-disk-rqb))
      (unwind-protect
          (let ((func #'(lambda (int-pkt)
                          (copy-array-portion int-pkt *bt-data-offset* (+ *bt-data-offset* (floor 1024. 2))
                                              (si:rqb-buffer rqb) 0 (floor 1024. 2))
                          (let ((block-number (dpb (bt-pkt-block-number-hi int-pkt)
                                                   (byte 16. 16.)
                                                   (bt-pkt-block-number-lo int-pkt))))
                            (when (or (< block-number 0)
                                      (>= block-number n-blocks))
                              (ferror nil "bad block number"))
                            (si:disk-write rqb unit (+ first-block block-number))
                            (setq *bt-block-list* (delq block-number *bt-block-list* 1))
                            (chaos:free-int-pkt int-pkt)))))
;           (push (cons *bt-ethernet-type* func) chaos:ethernet-pkt-handler-alist)
;           (process-wait "Band Transfer"
;                         #'(lambda () (not *bt-server-needed*)))
            (do-forever
              (process-wait "Fake Pkt" #'(lambda () *fake-pkt*))
              (funcall func *fake-pkt*)
              (setq *fake-pkt* nil)
              (if (null *bt-server-needed*) (return nil)))
            )
        (setq chaos:ethernet-pkt-handler-alist
              (delq (assq *bt-ethernet-type* chaos:ethernet-pkt-handler-alist)
                    chaos:ethernet-pkt-handler-alist))))))

(defun bt-server-function (&aux conn)
  (setq conn (chaos:listen "FAST-BAND-TRANSFER"))
;  (when (null ethernet:*lambda-has-ethernet*)
;    (chaos:reject conn "I don't own the etherent")
;    (return-from bt-server-function nil))
  (chaos:accept conn)
  (send tv:who-line-file-state-sheet :add-server conn "FAST-BAND-TRANSFER")
  (with-open-stream (stream (chaos:make-stream conn))
    (let ((*readtable* (si:find-readtable-named "CL"))
          (*read-base* 10.)
          (*print-base* 10.)
          (*package* (find-package "USER")))
      (let ((command-plist (read stream)))
        (let ((local-unit (getf command-plist :unit))
              (part (getf command-plist :part))
              (n-blocks (getf command-plist :n-blocks))
              (direction (getf command-plist :direction)))
          (ecase direction
            (:write
             (setq *bt-server-needed* t)
             (process-run-function "BT Background" #'bt-pkt-receiver local-unit part n-blocks)
             (print `(:ready t
                             ;:ethernet-address ,ethernet:my-ethernet-address
                             ) stream)
             (send stream :force-output)
             (read stream)
             (setq *bt-server-needed* nil))))))))

(add-initialization "FAST-BAND-TRANSFER"
                    '(process-run-function "BT Server" 'bt-server-function)
                    NIL
                    'chaos:server-alist)

(defun bt-send-band (local-unit-name local-part to-machine to-unit to-part &aux rqb)
  (when (not (zerop to-unit))
    (ferror nil "can't handle this yet"))
  (si:with-decoded-disk-unit (unit to-machine "Write" nil t)
    (when (null (si:find-disk-partition-for-write to-part nil unit))
      (ferror nil "can't find TO-PART: ~s" to-part)))
  (si:with-decoded-disk-unit (local-unit local-unit-name "Read")
    (multiple-value-bind (first-block n-blocks)
        (si:find-disk-partition-for-read local-part)
      (setq n-blocks (min n-blocks (si:measured-size-of-partition local-part local-unit)))
      (when (null first-block)
        (ferror nil "can't find ~s" local-part))
      (let ((*readtable* (si:find-readtable-named "CL"))
            (*read-base* 10.)
            (*print-base* 10.)
            (*package* (find-package "USER")))
        (with-open-stream (stream (chaos:open-stream to-machine "FAST-BAND-TRANSFER"))
          (print `(:direction :write
                              :unit ,to-unit
                              :part ,to-part
                              :n-blocks ,n-blocks)
                 stream)
          (send stream :force-output)
          (let ((response (read stream)))
            (let ((dest-adr (getf response :ethernet-address)))
              (setq rqb (si:get-disk-rqb))
              (dotimes (i n-blocks)
                (when (zerop (remainder i 100.))
                  (format t "~d " (floor i 100.)))
                (si:disk-read rqb local-unit (+ first-block i))
                (let ((int-pkt (chaos:allocate-int-pkt)))
                  (copy-array-portion (si:rqb-buffer rqb) 0 (floor 1024. 2)
                                      int-pkt *bt-data-offset* (+ *bt-data-offset* (floor 1024. 2)))
                  (setf (bt-pkt-block-number-hi int-pkt) (ldb (byte 16. 16.) i))
                  (setf (bt-pkt-block-number-lo int-pkt) (ldb (byte 16. 0) i))
;                 (ethernet:send-int-pkt-via-ethernet
;                   int-pkt
;                   ethernet:my-ethernet-address
;                   dest-adr
;                   *bt-ethernet-type*
;                   (floor 1024. 2))
                  (setq *fake-pkt* int-pkt)
                  (process-wait "Fake Pkt" #'(lambda () (null *fake-pkt*)))
                  ))))
          (print "T " stream)
          (send stream :force-output)
          (send stream :eof)
          )))))

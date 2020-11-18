;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(defmacro dpb-multiple (&rest fields)
  (labels ((expander (fields)
             (cond ((null fields) (ferror nil "Even number of arguments to ~s" 'dpb-multiple))
                   ((null (rest fields)) (first fields))
                   (t `(DPB
                         ,(first fields) ,(second fields)
                         ,(expander (rest (rest fields))))))))
    (expander fields)))

(defvar *my-debug-board* nil "Slot number of the debug board I am using.")

(defmacro defregister (name board offset type)
  (let* ((readable? (memq type '(:read-only  :read-write)))
         (writable? (memq type '(:write-only :read-write)))
         (read-function `(DEFUN ,name ()
                           ,(if readable?
                                `(%NUBUS-READ ,board ,offset)
                                `(FERROR NIL "Cannot read register ~S." (QUOTE ,name)))))
         (write-name (intern (string-append "WRITE-" name)))
         (write-function `(DEFUN ,write-name (VALUE)
                            ,@(if writable?
                                 `((%NUBUS-WRITE ,board ,offset VALUE))
                                 `((DECLARE (IGNORE VALUE))
                                   (FERROR NIL "Cannot write register ~S." (QUOTE ,name)))))))
    `(PROGN ,read-function
            ,write-function
            (DEFSETF ,name ,write-name))))

(defconstant debug-config-prom        #xFFF800)
(defconstant debug-config-prom-size   25.)
(defconstant debug-config-prom-string "LMI DEBUG BOARD")

(defregister debug-mode-register              *my-debug-board* #xFFF7FC :read-write)
(defregister debug-address-register           *my-debug-board* #xFFF7F8 :write-only)
(defregister debug-data-start-transfer        *my-debug-board* #xFFF7F4 :write-only)
(defregister debug-response-register          *my-debug-board* #xFFF7F4 :read-only)
(defregister debug-control-start-transfer     *my-debug-board* #xFFF7F0 :write-only)
(defregister debug-status-response            *my-debug-board* #xFFF7F0 :read-only)
(defregister debug-nubus-analyzer-ram-pointer *my-debug-board* #xFFF7EC :read-write)
(defregister debug-nubus-analyzer-ram-data    *my-debug-board* #xFFF7E8 :read-write)
(defregister debug-nubus-analyzer-ram-control *my-debug-board* #xFFF7E4 :read-write)
(defregister debug-nubus-analyzer-function    *my-debug-board* #xFFF7E0 :write-only)
(defregister debug-test-register              *my-debug-board* #xFFF7C4 :read-write)

(defconstant %%mode-init                 (byte 1. 0.))
(defconstant %%mode-master               (byte 1. 1.))
(defconstant %%mode-led                  (byte 1. 2.))
(defconstant %%debug-mode-loopback       (byte 1. 3.))
(defconstant %%debug-mode-speed          (byte 1. 4.))
(defconstant %%debug-mode-txwait         (byte 1. 6.))
(defconstant %%debug-mode-response-ready (byte 1. 7.))

(defconstant $$init 1.)

(defconstant $$disable-mastership 0)
(defconstant $$enable-mastership  1)

(defconstant $$led-off 0)
(defconstant $$led-on  1)

(defconstant $$disable-loopback 0)
(defconstant $$enable-loopback  1)

(defconstant $$slow-transfers 0)
(defconstant $$fast-transfers 1)

(defconstant $$transmitter-idle 0)
(defconstant $$transmitter-busy 1)

(defconstant $$no-response    0)
(defconstant $$response-ready 1)

(defconstant %%nubus-analyzer-slave-id       (byte 4.  0.))
(defconstant %%nubus-analyzer-master-id      (byte 4.  4.))
(defconstant %%nubus-analyzer-all-slaves     (byte 1.  8.))
(defconstant %%nubus-analyzer-all-masters    (byte 1.  9.))
(defconstant %%nubus-analyzer-enable         (byte 1. 10.))
(defconstant %%nubus-analyzer-stop-when-full (byte 1. 11.))

(defconstant $$individual 0)
(defconstant $$all        1)

(defconstant $$disable-analyzer 0)
(defconstant $$enable-analyzer  1)

(defconstant $$wrap-if-full 0)
(defconstant $$stop-if-full 1)

(defconstant %%debug-control-start-bit (byte 1. 0.))
(defconstant %%debug-control-ack-bit   (byte 1. 1.))
(defconstant %%debug-control-tm-bits   (byte 2. 2.))
(defconstant %%debug-control-axr-bit   (byte 1. 4.))

(defconstant %%debug-response-start-bit (byte 1. 0.))
(defconstant %%debug-response-ack-bit   (byte 1. 1.))
(defconstant %%debug-response-tm-bits   (byte 2. 2.))
(defconstant %%debug-response-axr-bit   (byte 1. 4.))

(defconstant $$no-start 0)
(defconstant $$start    1)

(defconstant $$no-ack 0)
(defconstant $$ack    1)

(defconstant $$tm-start-read       0.)
(defconstant $$tm-start-byte-read  1.)
(defconstant $$tm-start-write      2.)
(defconstant $$tm-start-byte-write 3.)

(defconstant $$tm-try-again-later 0.)
(defconstant $$tm-nubus-timeout   1.)
(defconstant $$tm-nubus-error     2.)
(defconstant $$tm-good-response   3.)

(defconstant $$repeat-off 0)
(defconstant $$repeat-on  1)

(defconstant fake-ack
             (dpb-multiple
               $$tm-good-response %%debug-control-tm-bits
               $$ack              %%debug-control-ack-bit
               $$no-start         %%debug-control-start-bit
               0))

(defun find-a-debug-board ()
  (let ((this-lambda-processor-slot (si::%processor-conf-slot-number si::*my-proc-conf*))
        (first-debug-board          (si::%system-configuration-lmi-debug-board-owner si::*sys-conf*))
        (second-debug-board         (si::%system-configuration-second-lmi-debug-board-owner si::*sys-conf*)))
    (let ((first-board-exists?  (not (= (ldb (byte 8. 0.) first-debug-board)  (ldb (byte 8. 0.) -1.))))
          (second-board-exists? (not (= (ldb (byte 8. 0.) second-debug-board) (ldb (byte 8. 0.) -1.)))))
      (if (not first-board-exists?)
          (ferror nil "I can't find a debug board, get one and run CONFIG.")
          ;; We have at least one board, do we own it?
          (cond ((= this-lambda-processor-slot first-debug-board)
                 (si::%system-configuration-lmi-debug-board-slot si::*sys-conf*))
                ((= this-lambda-processor-slot second-debug-board)
                 (si::%system-configuration-second-lmi-debug-board-slot si::*sys-conf*))
                ;; I don't own either, offer to take one.
                (t (cerror "Take one." "I don't own a debug board, but there is at least one here.~
                                        You can take it.")
                   (if (not second-board-exists?)
                       (fquery
                         (list ':choices
                               `(((1 "First board")
                                  ,(si::%system-configuration-lmi-debug-board-slot si::*sys-conf*))
                                 ((2 "Second board")
                                  ,(si::%system-configuration-second-lmi-debug-board-slot si::*sys-conf*)))
                               ':fresh-line 't)
                         "Take which board?"))))))))

(defun assure-debug-board ()
  (when (null *my-debug-board*)
    (let ((slot (find-a-debug-board)))
      (setq *my-debug-board* (logior #xF0 slot)))))     ;put it in slot space

(defun read-debug-board (address)
  (%nubus-read *my-debug-board* address))

(defun write-debug-board (address data)
  (%nubus-write *my-debug-board* address data))

(defun read-debug-config-prom ()
  (assure-debug-board)
  (let ((string (make-string debug-config-prom-size)))
    (dotimes (index debug-config-prom-size)
      (setf (schar string index)
            (logand #xFF (read-debug-board (+ debug-config-prom (* index 4))))))
    string))

(defun verify-debug-board ()
  (let ((config (read-debug-config-prom))
        (length (length debug-config-prom-string)))
    (when (not (string-equal
                 debug-config-prom-string config
                 :end1 length
                 :end2 length))
      (ferror nil "I found ~S, which I don't think is a debug board." config))))

(defun setup-debug-board ()
  (find-a-debug-board)
  (verify-debug-board))

(defun debug-read-write-test ()
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (setf (debug-test-register) (ash 1. i))
      (let ((result (debug-test-register)))
        (when (not (= result (ash 1. i)))
          (format t "~%Error --- wrote ~X --- read ~X" (ash 1 i) result))))))

(defun reset-debug-board ()
  (setf (debug-mode-register) (dpb $$init %%mode-init 0)))

(defun blink-debug-light ()
  (loop
    (sleep .5)
    (setf (debug-mode-register)
          (dpb $$led-on %%mode-led (debug-mode-register)))
    (sleep .5)
    (setf (debug-mode-register)
          (dpb $$led-off %%mode-led (debug-mode-register)))))

(defun reset-nubus-analyzer ()
  (setf (debug-nubus-analyzer-function)
        (dpb $$disable-analyzer %%nubus-analyzer-enable 0)))

(defun board-idle? ()
  (= $$transmitter-idle (ldb %%debug-mode-txwait (debug-mode-register))))

(defun idle-board-down ()
  (do () ((board-idle?) (sleep .1))))

(defun response-ready? ()
  (= $$response-ready (ldb %%debug-mode-response-ready (debug-mode-register))))

(defun check-for-valid-response (if-try-again-later if-nubus-timeout if-nubus-error if-valid)
  (let ((status (ldb %%debug-response-tm-bits (debug-status-response))))
    (cond ((= status $$tm-good-response)   (funcall if-valid))
          ((= status $$tm-try-again-later) (funcall if-try-again-later))
          ((= status $$tm-nubus-timeout)   (funcall if-nubus-timeout))
          ((= status $$tm-nubus-error)     (funcall if-nubus-error))
          )))

(defun wait-for-debug-response (if-timeout
                                if-try-again-later
                                if-nubus-timeout
                                if-nubus-error
                                if-valid)
  (do ((count 0 (1+ count)))
      ((response-ready?)
       (check-for-valid-response if-try-again-later if-nubus-timeout if-nubus-error if-valid))
    (when (> count 1000.)
      (return-from wait-for-debug-response (funcall if-timeout)))))

(defun make-response-error (type)
  #'(lambda () (ferror nil "~%Response error: ~A" type)))

(deff error-if-no-response     (make-response-error "No response"))
(deff error-if-try-again-later (make-response-error "Try again later"))
(deff error-if-nubus-timeout   (make-response-error "Nubus timeout"))
(deff error-if-nubus-error     (make-response-error "Nubus error"))

(defun wait-for-valid-debug-response ()
  (wait-for-debug-response
    #'error-if-no-response
    #'error-if-try-again-later
    #'error-if-nubus-timeout
    #'error-if-nubus-error
    #'ignore))

(defun flush-status-response ()
  ;; Reading will flush it.
  (debug-status-response))

(defun shutdown-nubus-analyzer ()
  ;; Reading it shuts it down.
  (debug-nubus-analyzer-ram-data))

(defun setup-debug-board-for-loopback ()
  (reset-debug-board)
  (setf (debug-mode-register)
        (dpb-multiple
          $$fast-transfers    %%debug-mode-speed
          $$enable-loopback   %%debug-mode-loopback
          $$enable-mastership %%mode-master
          0))
  (idle-board-down)
  (flush-status-response)
  (reset-nubus-analyzer)
  (shutdown-nubus-analyzer)
  t)

(defun test (chain tester)
  (map-chain


  (do ((rest chain (tail chain)))
      ((empty-chain
        (let ((result (debug-response-register)))
          (when (not (= data result))
            (format t "~%Error --- wrote ~x --- read ~x" data result))))

(defun debug-serial-loopback-test ()
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (test (ash 1. i)
            #'(lambda (data)
                (setf (debug-address-register) data)
                (setf (debug-control-start-transfer) fake-ack)  ;fake out debug board
                (wait-for-valid-debug-response)
                (debug-response-register)))
      (let ((data (logand #xFFFFFFFF (lognot (ash 1. i)))))
        (setf (debug-address-register) data)
        (setf (debug-control-start-transfer) fake-ack)  ;fake out debug board
        (wait-for-valid-debug-response)
        (let ((result (debug-response-register)))
          (when (not (= data result))
            (format t "~%Error --- wrote ~x --- read ~x" data result)))))))

;;; -*- Mode:LISP; Package:NETWORK; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( *loopback-interface*))

(defstream loopback-interface
           (network-interface)
           "LOOP-"
  )

(defvar *loopback-interface* nil "The Loopback network interface")
(defvar *loopback-receive-list* (make-fifo) "List of packets waiting to be received")

;;;Debugging parameters
(defvar *damage-percentage* 0)
(defvar *delay-percentage* 0)
(defvar *delay-maximum-interval* 2)
(defvar *drop-percentage* 0)
(defvar *duplicate-percentage* 0)

(defop (loopback-interface :peek-special-fields) (ni)
  (declare (ignore ni))
  (list (tv:scroll-parse-item
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (process-run-function "Set Loopback Debug Variables"
                                                                #'set-loopback-debugging-variables)))
                  :documentation
                  "Click to set Loopback Debugging Variables")
             :string "Debugging Variables:")
          `(:symeval net:*damage-percentage* nil ("  Damage ~A%"))
          `(:symeval net:*delay-percentage* nil ("  Delay ~A%"))
          `(:symeval net:*delay-maximum-interval* nil (" up to ~A seconds"))
          `(:symeval net:*drop-percentage* nil ("  Drop ~A%"))
          `(:symeval net:*duplicate-percentage* nil ("  Duplicate ~A%")))))

(defun set-loopback-debugging-variables ()
  (tv:choose-variable-values '((*damage-percentage* "Damage %" :number-or-nil)
                               (*delay-percentage* "Delay %" :number-or-nil)
                               (*delay-maximum-interval* "Delay max" :number-or-nil)
                               (*drop-percentage* "Drop %" :number-or-nil)
                               (*duplicate-percentage* "Duplicate %" :number or-nil))
                             :label "Loopback Debugging Parameters"
                             :function #'(lambda (ignore variable old-value new-value)
                                           (if new-value
                                               (case variable
                                                 ((*damage-percentage* *delay-percentage*
                                                   *drop-percentage* *duplicate-percentage*)
                                                  (unless (<= 0 new-value 100)
                                                    (set variable old-value)))
                                                 (*delay-maximum-interval*
                                                  (unless (<= 0 new-value 60)
                                                    (set variable old-value))))
                                             (set variable old-value))
                                           nil)))

(defmacro loop-pkt (elt)
  `(car ,elt))

(defmacro loop-type (elt)
  `(cdr ,elt))

(defun loopback-reset (stream)
  (declare (ignore stream))
  (do ((elt (pop-fifo *loopback-receive-list*)
            (pop-fifo *loopback-receive-list*)))
      ((null elt))
    (free-packet (loop-pkt elt))))

(defun loopback-send-int-pkt (int-pkt source dest type n-16-bit-words)
  (declare (ignore source))
  (declare (ignore dest))
  (setf (fill-pointer int-pkt) n-16-bit-words)
  (push-fifo (cons int-pkt type) *loopback-receive-list*)
  t)

(defun loopback-packet-ready ()
  (not (fifo-empty-p *loopback-receive-list*)))

(defun loopback-get-next-packet ()
  (declare (values packet type source destination))
  (let ((elt (pop-fifo *loopback-receive-list*)))
    (when elt
      (values (loop-pkt elt)                    ;the packet
              (loop-type elt)                   ;type code
              0                                 ;sender
              0                                 ;receiver
              nil                               ;broadcast-p
              ))))

(defun setup-loopback (tag &aux alist)
  (when *loopback-interface*
    (setq alist (ni-protocol-alist *loopback-interface*))
    (funcall *loopback-interface* :close))

  (setq *loopback-receive-list* (make-fifo))

  (setq *loopback-interface*
        (make-loopback-interface :tag tag
                                 :interface :loopback
                                 :keyword :loopback
                                 :minimum-data-length 0
                                 :maximum-data-length 1500.
                                 :sent-header-length 0
                                 :rcvd-header-length 0
                                 :sent-trailer-length 0
                                 :rcvd-trailer-length 0
                                 :protocol-alist alist
                                 :disable-function 'loopback-reset
                                 :reset-function 'loopback-reset
                                 :packet-ready-function 'loopback-packet-ready
                                 :get-next-packet-function 'loopback-get-next-packet
                                 :send-function  'loopback-send-int-pkt
                                 :gauge-name "LOOP"
                                 ))

  (funcall *loopback-interface* :open)
  (funcall *loopback-interface* :enable))

(add-initialization "Start Loopback interface" '(setup-loopback "LOOPBACK") :network-driver)

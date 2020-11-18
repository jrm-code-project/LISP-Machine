;;; -*- Mode:LISP; Package:SERIAL-IP; Readtable:ZL; Base:10 -*-

;;;TEST CODE for SERIAL-PROTOCOL:

(defvar s)
(defvar pkt)
(defvar s2)

(defun sinit()
  (setq s
      (open "sdu-serial-b:"
            :flavor-and-init-options
            '(si:sdu-serial-stream
               :baud-rate 9600 :parity :even
               :char-length :8bits)))
  (send s :reset))

(defun ininit()
  (sinit)
  (setq pkt (new-serial-proto-packet))
  (setq s2 (make-serial-proto-input-stream s pkt)))

(defun in(again)
  (or again (setf (fill-pointer pkt) 0))
  (serial-proto-receive s2)
  pkt)

(defun outinit(&rest strings &aux str)
  (sinit)
  (setq str (apply #'string-append strings))
  (setq pkt (new-serial-proto-packet))
  (copy-array-portion str 0 (string-length str)
                      pkt 0 (string-length str))
  (setf (fill-pointer pkt) (string-length str))
  (serial-proto-send str s2))

#|
;;Old code.
;;Here's a SELECTQ version of SERIAL-PROTO-IO-STREAM.

(defun serial-proto-io-stream(op &rest rest)
  "This is the driving function for a SERIAL-PROTO-IO-STREAM."
  (declare(special stream packet escaping closed status))
  (if (not (member op *serial-proto-ops*))
      (apply stream op rest)
    (selectq op
      ;;good housekeeping
    (:closed closed)
    (:status status)
    (:packet packet)
    (:full-input-packet
     (when (>= (fill-pointer packet) (array-length packet))
       (setq closed t)
       (setq status :full)))
    ;;Output
    (:tyo
     (unless closed
       (let((ch (car rest)))
         (cond
           ((null ch) nil)
           ((char= ch *sp-frame-escape*)
            (funcall stream :tyo *sp-frame-escape*)
            (funcall stream :tyo *sp-frame-xescape*))
           ((char= ch *sp-frame-end*)
            (funcall stream :tyo *sp-frame-escape*)
            (funcall stream :tyo *sp-frame-xend*))
           (t
            (funcall stream :tyo ch))))))
    (:new-packet
     (setq packet (car rest))
     (setq closed nil)
     (setq status :open))
    (:flush-output
     (unless closed
       (funcall stream :tyo *sp-frame-end*)
       ;;For the Lambda's sake we do :FORCE-OUTPUT to the serial stream when
       ;;we're done outputting a packet.  This yields a minor improvement over
       ;;the default, where characters are forced out by :TYO and :STRING-OUT.
       (funcall stream :force-output)))
    (:send-packet
     (do*((indx 0 (1+ indx))
          (ch (aref packet indx) (aref packet indx)))
         ((>= indx (fill-pointer packet))
          (serial-proto-io-stream :flush-output))
       (serial-proto-io-stream :tyo ch)))
    ;;Input
    (:tyi-no-hang
     (serial-proto-io-stream :full-input-packet)
     (serial-proto-io-stream :receive-char (funcall stream :tyi-no-hang)))
    (:tyi
     (serial-proto-io-stream :full-input-packet)
     (serial-proto-io-stream :receive-char (funcall stream :tyi)))
    (:receive-char
     (unless closed
       (let((ch (car rest)))
       (and ch
            (setq ch
                  (if (not escaping)
                      (cond
                        ((char= ch *sp-frame-end*)
                         (setq closed(setq status :done))
                         nil)
                        ((char= ch *sp-frame-escape*)
                         (print 'escaping)
                         (setq escaping t)
                         nil)
                        (t ch))
                    (progn
                      (setq escaping nil)
                      (cond
                        ((char= ch *sp-frame-xend*)
                         *sp-frame-end*)
                        ((char= ch *sp-frame-xescape*)
                         *sp-frame-escape*)
                        (t ch)))))
            (progn
              (setf (aref packet (fill-pointer packet)) ch)
              (incf (fill-pointer packet))
              ch)))))
    (:which-operations
     (append valid-ops
             (funcall stream :which-operations))))))

|#

;; -*- Mode:LISP; Package:TCP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defmacro without-more-processing (the-window &body body)
  "execute body with more processing disabled"
  (let ((more-p (gensym))
        (window (gensym)))
    `(let* ((,window ,the-window)
            (,more-p (send ,window :more-p)))
       (unwind-protect
           (progn (send ,window :set-more-p nil)
                  ,@body)
         (send ,window ':set-more-p ,more-p)))))

(defvar *telnet-logging* nil)
(defvar *telnet-data-log* nil)
(defvar *telnet-final-stats-block* nil)
(defvar *telnet-final-tcp-stats-block* nil)

(defun initialize-data-log ()
  (setq *telnet-data-log* nil))

(defun log-data (buffer)
  (when *telnet-logging*
    (push (string buffer) *telnet-data-log*)))

(defparameter *telnet-receive-buffers* 8)
(defparameter *telnet-receive-buffer-size* 256)

(defun telnet (host)
  (if (ip-header-p host)
      (setq host (ip:ih-dest-address host))
    (let ((original-host host))
      (assert (numberp (setq host (parse-internet-address (setq original-host host))))
              (host)
              "~S is not a valid Internet host specification"
              original-host)))
  (let ((socket (make-tcp-socket :keyword "User Telnet"))
        (opened nil)
        (open nil)
        (term-in-buffer nil)
        (term-out-buffer nil))
    (without-more-processing *terminal-io*
      (initialize-data-log)
      (unwind-protect
          (cond ((setq opened (send socket :open
                                    :remote-port 23
                                    :remote-address host
                                    :active t
                                    :auto-push t
                                    :optimistic t))
                 (setq term-in-buffer (make-array 256 :fill-pointer 0 :element-type '(unsigned-byte 8)))
                 (setq term-out-buffer (make-string 256))
                 (loop
                   (process-wait "Terminal or Network"
                                 #'(lambda (term)
                                     (or (send socket :listen)
                                         (and open (send term :listen))))
                                 *terminal-io*)
                   (cond ((send *terminal-io* :listen)
                          (get-terminal-data term-in-buffer)
                          (copy-data-to-net term-in-buffer socket))
                         ((send socket :listen)
                          (let ((item (send socket :read-data)))
                            (case (first item)
                              (:open
                               (dotimes (i *telnet-receive-buffers*)
                                 (send socket
                                       :receive
                                       (make-array *telnet-receive-buffer-size*
                                                   :fill-pointer 0
                                                   :element-type '(unsigned-byte 8))))
                               (setq open t))
                              (:data            ;Data from remote host
                               (log-data (second item))
                               (copy-data-to-terminal (second item) term-out-buffer)
                               (unless (eq (third item) :eof)
                                 (send socket :receive (second item))))
                              (:closing         ;Remote side has closed
                               (return "Closed by remote end"))
                              (:reset
                               (setq opened open)
                               (return (if open "Connection reset" "Connection refused")))
                              (:close           ;Socket closed out from under us
                               (setq opened nil)
                               (return "Closed locally"))
                              ((:network-unreachable :host-unreachable :protocol-unreachable :port-unreachable)
                               (return "Unreachable"))
                              (:timeout
                               (send socket :abort)
                               (setq opened nil)
                               (return "Timed out"))
                              (otherwise
                               ;;Ignore it
                               )))))))
                (t
                 "TCP not running"))
        (when opened
          (send socket :close))
        (setq *telnet-final-stats-block* (tcp-user-statistics-block socket))
        (setq *telnet-final-tcp-stats-block* (tcp-user-stats socket))
        (setq *telnet-data-log* (nreverse *telnet-data-log*))))))

(defun playback ()
  (let ((term-out-buffer (make-string 256)))
    (dolist (x *telnet-data-log*)
      (copy-data-to-terminal x term-out-buffer))))

(defun get-terminal-data (buffer)
  (do ((limit (array-length buffer))
       (index 0))
      ((or (not (send *terminal-io* :listen))
           (>= index limit))
       (setf (fill-pointer buffer) index)
       buffer)
    (let ((char (char-to-ascii (send *terminal-io* :tyi))))
      (when char
        ;;(send *terminal-io* :tyo char)
        (setf (aref buffer index) char)
        (incf index)
        (when (eq char #o15)
          (setf (aref buffer index) #o12)
          (incf index))))))

(defun copy-data-to-net (buffer socket)
  (when (plusp (length buffer))
    (send socket :write-data (string buffer))))

(defun copy-data-to-terminal (in-buffer out-buffer)
  (do ((index 0 (1+ index))
       (count 0)
       (limit (length in-buffer)))
      ((>= index limit)
       (when (plusp count)
         (send *terminal-io* :string-out out-buffer 0 count)))
    (let ((char (ascii-to-char (aref in-buffer index))))
      (cond ((eq char #o210)
             (when (plusp count)
               (send *terminal-io* :string-out out-buffer 0 count)
               (setq count 0))
             (send *terminal-io* :tyo char))
            (char
             (setf (char out-buffer count) char)
             (incf count))))))

(defun char-to-ascii (lispm-char)
  (cond ((char-bit lispm-char :control)
         (logxor #o100 (char-upcase (char-to-ascii (char-code lispm-char)))))
        ((not (zerop (char-bits lispm-char)))
         nil)
        ((member lispm-char `(,(char-int #\return) ,(char-int #\line) ,(char-int #\tab) ,(char-int #\form)))
         (- lispm-char #o200))
        ((= lispm-char (char-int #\Rubout))
         #o177)
        ((graphic-char-p lispm-char)
         (char-code lispm-char))))

(defun ascii-to-char (ascii-char)
  (cond ((> ascii-char #o177)
         ;;high bit set
         nil)
        ((= ascii-char #o15)
         ;;Carriage Return -- ignore it
         nil)
        ((= ascii-char #o12)
         ;;Line Feed -- convert to RETURN
         #\return)
        ((member ascii-char '(#o11 #o14 #o10))
         ;; Tab, Form Feed, Backspace
         (+ ascii-char #o200))
        ((< ascii-char #o40)
         ;;Other control character
         nil)
        ('else
         ;;Normal printing character
         ascii-char)))

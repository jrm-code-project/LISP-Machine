;;; -*- Mode:LISP; Package:TCP-APPLICATION; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defmacro rwho-vers (packet)
  `(aref ,packet 0))
(defconstant rwho-version-number 1)

(defmacro rwho-type (packet)
  `(aref ,packet 1))
(defconstant rwho-status-type 1)

(defsubst rwho-pad (packet)
  (dpb (aref packet 2) (byte 8 8)
       (aref packet 3)))
(defun set-rwho-pad (packet val)
  (setf (aref packet 2) (ldb (byte 8 8) val))
  (setf (aref packet 3) (ldb (byte 8 0) val))
  val)
(defsetf rwho-pad set-rwho-pad)

(defsubst rwho-send-time (packet)
  (dpb (aref packet 4) (byte 8 24.)
       (dpb (aref packet 5) (byte 8 16.)
            (dpb (aref packet 6) (byte 8 8)
                 (aref packet 7)))))
(defun set-rwho-send-time (packet val)
  (setf (aref packet 4) (ldb (byte 8 24.) val))
  (setf (aref packet 5) (ldb (byte 8 16.) val))
  (setf (aref packet 6) (ldb (byte 8 8) val))
  (setf (aref packet 7) (ldb (byte 8 0) val))
  val)
(defsetf rwho-send-time set-rwho-send-time)

(defsubst rwho-recv-time (packet)
  (dpb (aref packet 8) (byte 8 24.)
       (dpb (aref packet 9) (byte 8 16.)
            (dpb (aref packet 10) (byte 8 8)
                 (aref packet 11)))))
(defun set-rwho-recv-time (packet val)
  (setf (aref packet 8) (ldb (byte 8 24.) val))
  (setf (aref packet 9) (ldb (byte 8 16.) val))
  (setf (aref packet 10) (ldb (byte 8 8) val))
  (setf (aref packet 11) (ldb (byte 8 0) val))
  val)
(defsetf rwho-recv-time set-rwho-recv-time)

(defun rwho-hostname (packet)
  (get-asciz-substring packet 12 (+ 12 32) :lower))
(defun set-rwho-hostname (packet val)
  (set-asciz-substring packet 12 (+ 12 32) val :lower))
(defsetf rwho-hostname set-rwho-hostname)

(defun rwho-load-average (packet index)
  (dpb (aref packet (+ 44 (* index 4))) (byte 8 24.)
       (dpb (aref packet (+ 45 (* index 4))) (byte 8 16.)
            (dpb (aref packet (+ 46 (* index 4))) (byte 8 8)
                 (aref packet (+ 47 (* index 4)))))))
(defun set-rwho-load-average (packet index val)
  (setf (aref packet (+ 44 (* index 4))) (ldb (byte 8 24.) val))
  (setf (aref packet (+ 45 (* index 4))) (ldb (byte 8 16.) val))
  (setf (aref packet (+ 46 (* index 4))) (ldb (byte 8 8) val))
  (setf (aref packet (+ 47 (* index 4))) (ldb (byte 8 0) val))
  val)

(defsubst rwho-boot-time (packet)
  (dpb (aref packet 56) (byte 8 24.)
       (dpb (aref packet 57) (byte 8 16.)
            (dpb (aref packet 58) (byte 8 8)
                 (aref packet 59)))))
(defun set-rwho-boot-time (packet val)
  (setf (aref packet 56) (ldb (byte 8 24.) val))
  (setf (aref packet 57) (ldb (byte 8 16.) val))
  (setf (aref packet 58) (ldb (byte 8 8) val))
  (setf (aref packet 59) (ldb (byte 8 0) val))
  val)
(defsetf rwho-boot-time set-rwho-boot-time)

(defconstant rwho-utmp-offset 60)
(defconstant rwho-utmp-entry-length 24)

(defun rwho-utmp-ttyname (packet offset)
  (get-asciz-substring packet offset (+ offset 8) :lower))
(defun set-rwho-utmp-ttyname (packet offset val)
  (set-asciz-substring packet offset (+ offset 8) val :lower))

(defun rwho-utmp-userid (packet offset)
  (get-asciz-substring packet (+ offset 8) (+ offset 16) nil))
(defun set-rwho-utmp-userid (packet offset val)
  (set-asciz-substring packet (+ offset 8) (+ offset 16) val nil))

(defun rwho-utmp-time-logged-on (packet offset)
  (dpb (aref packet (+ offset 16)) (byte 8 24.)
       (dpb (aref packet (+ offset 17)) (byte 8 16.)
            (dpb (aref packet (+ offset 18)) (byte 8 8)
                 (aref packet (+ offset 19))))))
(defun set-rwho-utmp-time-logged-in (packet offset val)
  (setf (aref packet (+ offset 16)) (ldb (byte 8 24.) val))
  (setf (aref packet (+ offset 17)) (ldb (byte 8 16.) val))
  (setf (aref packet (+ offset 18)) (ldb (byte 8 8) val))
  (setf (aref packet (+ offset 19)) (ldb (byte 8 0) val))
  val)

(defun rwho-utmp-idle-time (packet offset)
  (dpb (aref packet (+ offset 20)) (byte 8 24.)
       (dpb (aref packet (+ offset 21)) (byte 8 16.)
            (dpb (aref packet (+ offset 22)) (byte 8 8)
                 (aref packet (+ offset 23))))))
(defun set-rwho-utmp-idle-time (packet offset val)
  (setf (aref packet (+ offset 20)) (ldb (byte 8 24.) val))
  (setf (aref packet (+ offset 21)) (ldb (byte 8 16.) val))
  (setf (aref packet (+ offset 22)) (ldb (byte 8 8) val))
  (setf (aref packet (+ offset 23)) (ldb (byte 8 0) val))
  val)

(defun get-asciz-substring (buffer start end &optional (case :lower))
  (let ((string nil))
    (do ((i start (1+ i)))
        ((or (= i end)
             (zerop (aref buffer i)))
         (setq string (make-string (- i start)))))
    (dotimes (i (length string))
      (let ((char (aref buffer (+ start i))))
        (setf (char string i) (case case
                                (:lower (char-downcase char))
                                (:upper (char-upcase char))
                                (t char)))))
    string))

(defun set-asciz-substring (buffer start end string &optional (case :lower))
  (do ((dest start (1+ dest))
       (source 0 (1+ source))
       (length (length string)))
      ((= dest end))
    (setf (aref buffer dest) (if (< source length)
                                 (let ((char (char string source)))
                                   (case case
                                     (:lower (char-downcase char))
                                     (:upper (char-upcase char))
                                     (t char)))
                               0))))

;;;Unix Universal time is calculated from midnight on Jan 1 1970
;;;Lispm Universal time is calculated from midnight on Jan 1 1900
;;;There are 70 years (including 17 leap years) in this interval

(defconstant *rwho-unix-ut-fudge* (* (+ (* 70 365) 17) 24 3600)
  "Fudge factor to convert between Unix and LISPM Universal Times")

(defmacro fudge-ut-to-unix (time)
  `(- ,time *rwho-unix-ut-fudge*))

(defmacro fudge-ut-from-unix (time)
  `(+ ,time *rwho-unix-ut-fudge*))

(defun rwho-print-interval (interval)
  (let ((seconds (mod interval 60))
        (minutes (mod (truncate interval 60) 60))
        (hours (mod (truncate interval 3600) 24))
        (days (truncate interval (* 24 3600))))
    (format nil
            "~:[~DD ~;~*~]~:[~2,'0D:~;~*~]~2,'0D:~2,'0D"
            (zerop days)
            days
            (zerop hours)
            hours
            minutes
            seconds)))

(defun describe-rwho (packet &optional (length (length packet)))
  (labels ((print-non-zero-universal-time (time)
             (if (zerop time)
                 "---"
               (with-output-to-string (*standard-output*)
                 (time:print-universal-time (fudge-ut-from-unix time))))))
    (format t "~&version: ~20t~s" (rwho-vers packet))
    (format t "~&type: ~20t~s" (rwho-type packet))
    (format t "~&pad: ~20t~s" (rwho-pad packet))
    (format t "~&send time: ~20t~s" (print-non-zero-universal-time (rwho-send-time packet)))
    (format t "~&recv time: ~20t~s" (print-non-zero-universal-time (rwho-recv-time packet)))
    (format t "~&hostname: ~20t~s" (rwho-hostname packet))
    (format t "~&load average 0: ~20t~s" (rwho-load-average packet 0))
    (format t "~&load average 1: ~20t~s" (rwho-load-average packet 1))
    (format t "~&load average 2: ~20t~s" (rwho-load-average packet 2))
    (format t "~&boot time: ~20t~s" (print-non-zero-universal-time (rwho-boot-time packet)))
    (do ((offset rwho-utmp-offset (+ offset rwho-utmp-entry-length)))
        ((>= offset length))
      (format t "~&User: tty: ~20t~s" (rwho-utmp-ttyname packet offset))
      (format t "~&      userid: ~20t~s" (rwho-utmp-userid packet offset))
      (format t "~&      logged on: ~20t~s" (print-non-zero-universal-time (rwho-utmp-time-logged-on packet offset)))
      (format t "~&      idle: ~20t~s" (with-output-to-string (*standard-output*)
                                         (time:print-interval-or-never (rwho-utmp-idle-time packet offset))))
      )))

(defun build-rwho-packet (packet)
  (setf (rwho-vers packet) RWHO-VERSION-NUMBER)
  (setf (rwho-type packet) RWHO-STATUS-TYPE)
  (setf (rwho-pad packet) 0)
  (setf (rwho-send-time packet) (fudge-ut-to-unix (get-universal-time)))
  (setf (rwho-recv-time packet) 0)
  (setf (rwho-hostname packet) (send si:local-host :name))
  (set-rwho-load-average packet 0 0)
  (set-rwho-load-average packet 1 0)
  (set-rwho-load-average packet 2 0)
  (setf (rwho-boot-time packet) (fudge-ut-to-unix time:*ut-at-boot-time*))
  (cond ((zerop (length si:user-id))            ;not logged in
         (setf (fill-pointer packet) rwho-utmp-offset))
        (t
         (set-rwho-utmp-ttyname packet rwho-utmp-offset "LISPM")
         (set-rwho-utmp-userid packet rwho-utmp-offset si:user-id)
         (let ((login-time (fourth (first si:login-history))))
           (set-rwho-utmp-time-logged-in packet
                                         rwho-utmp-offset
                                         (if login-time
                                             (fudge-ut-to-unix (time:parse-universal-time login-time))
                                           (rwho-send-time packet))))
         (let ((idle (floor (time-difference (zl:time) tv:kbd-last-activity-time) 60)))
           (set-rwho-utmp-idle-time packet rwho-utmp-offset idle))
         (setf (fill-pointer packet) (+ rwho-utmp-offset rwho-utmp-entry-length))))
  packet)

(defvar *udp-rwho-server-broadcast-interval* 3600 "60ths of a second between broadcasts")
(defvar *udp-rwho-server-next-broadcast-time* nil)
(defvar *udp-rwho-server-packets* nil "Saved packets received by RWHO Server")
(defvar *udp-rwho-server-enabled* t "Determines whether this machine broadcasts RWHO packets")

(defvar *udp-rwho-server-process* (make-process "UDP RWHO Server"
                                                :warm-boot-action 'ignore
                                                :arrest-reasons '(:not-running)))

(defun initialize-udp-rwho-server-process ()
  (setq *udp-rwho-server-packets* nil)
  (send *udp-rwho-server-process* :preset 'udp-rwho-server-process)
  (send *udp-rwho-server-process* :reset)
  (send *udp-rwho-server-process* :run-reason :enable)
  (send *udp-rwho-server-process* :revoke-arrest-reason :not-running)
  t)

(defun broadcast-rwho-packet (stream)
  (unless time:*ut-at-boot-time*
    (process-wait "Universal Time" #'(lambda () time:*ut-at-boot-time*)))
  (when *udp-rwho-server-enabled*
    (send stream :broadcast-packet
          (build-rwho-packet (udp:get-udp-buffer))
          (send stream :local-port)))
  (setq *udp-rwho-server-next-broadcast-time*
        (time-increment (zl:time) *udp-rwho-server-broadcast-interval*))
  )

(defun insert-rwho-packet (remote-address buffer)
  (let ((elt (assoc remote-address *udp-rwho-server-packets*)))
    (cond (elt
           (udp:free-udp-buffer (cdr elt))
           (setf (cdr elt) buffer))
          (t
           (push (cons remote-address buffer) *udp-rwho-server-packets*))))
  (sort *udp-rwho-server-packets* #'string-lessp :key #'(lambda (elt)
                                                          (rwho-hostname (cdr elt)))))

(defun udp-rwho-server-process ()
  (loop
    (process-wait "UDP Enable" #'(lambda ()
                                   (and udp:*udp-stream* (udp:udp-enabled udp:*udp-stream*))))
    (with-open-stream (stream (send (make-instance 'udp:udp-stream)
                                    :open "RWHO Server" :local-port (sym ipport-whoserver)))
      (broadcast-rwho-packet stream)
      (loop
        (unless (time-lessp (zl:time) *udp-rwho-server-next-broadcast-time*)
          (broadcast-rwho-packet stream))
        (when (send stream :listen)
          (multiple-value-bind (packet length remote-port remote-address)
              (send stream :read-packet)
            (declare (ignore length))
            (declare (ignore remote-port))
            (unless packet
              ;;socket closed, UDP shut down, whatever...
              (return nil))
            (setf (rwho-recv-time packet) (fudge-ut-to-unix (get-universal-time)))
            (insert-rwho-packet remote-address packet)))
        (process-wait "RWHO Packet Send/Recv"
                      #'(lambda (s)
                          (or (send s :listen)
                              (not (time-lessp (zl:time) *udp-rwho-server-next-broadcast-time*))))
                      stream)))))

(defun rwho ()
  (when *udp-rwho-server-packets*
    (let ((fudged-time (fudge-ut-to-unix (get-universal-time)))
          (format-string "~&~A~20T~@9A~30T~@15A~46T~@13A~60T~A"))
      (format t
              format-string
              "Host Name"
              "Terminal"
              "On Since"
              "Idle"
              "User")
      (dolist (x *udp-rwho-server-packets*)
        (let* ((packet (cdr x))
               (hostname (rwho-hostname packet))
               (length (length packet)))
          (cond ((>= (- fudged-time (rwho-recv-time packet)) 180)
                 ;;Packet came 3 minutes ago -- assume host died
                 (format t "~&~16a no response for ~A"
                         hostname
                         (with-output-to-string (*standard-output*)
                           (time:print-interval-or-never (- fudged-time (rwho-recv-time packet))))))
                (t
                 (do ((offset rwho-utmp-offset (+ offset rwho-utmp-entry-length)))
                     ((>= offset length))
                   (format t
                           format-string
                           hostname
                           (rwho-utmp-ttyname packet offset)
                           (with-output-to-string (*standard-output*)
                             (time:print-brief-universal-time
                               (fudge-ut-from-unix (rwho-utmp-time-logged-on packet offset))))
                           (rwho-print-interval (rwho-utmp-idle-time packet offset))
                           (rwho-utmp-userid packet offset))))))))))

(defun ruptime ()
  (when *udp-rwho-server-packets*
    (let ((fudged-time (fudge-ut-to-unix (get-universal-time))))
      (dolist (x *udp-rwho-server-packets*)
        (let* ((packet (cdr x))
               (hostname (rwho-hostname packet)))
          (cond ((>= (- fudged-time (rwho-recv-time packet)) 180)
                 ;;Packet came 3 minutes ago -- assume host died
                 (format t "~&~20a no response for ~A"
                         hostname
                         (with-output-to-string (*standard-output*)
                           (time:print-interval-or-never (- fudged-time (rwho-recv-time packet))))))
                (t
                 (let ((lav0 (rwho-load-average packet 0))
                       (lav1 (rwho-load-average packet 1))
                       (lav2 (rwho-load-average packet 2))
                       (users (truncate (- (length packet) rwho-utmp-offset) rwho-utmp-entry-length)))
                   (format t
                           "~&~20a Up ~12a ~2d Users, Load = ~d.~2,'0d ~d.~2,'0d ~d.~2,'0d"
                           hostname
                           (rwho-print-interval (- fudged-time (rwho-boot-time packet)))
                           users
                           (truncate lav0 100)
                           (mod lav0 100)
                           (truncate lav1 100)
                           (mod lav1 100)
                           (truncate lav2 100)
                           (mod lav2 100))))))))))

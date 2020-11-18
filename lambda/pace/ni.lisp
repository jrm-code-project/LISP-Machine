;;; -*- Mode:LISP; Package:I; Base:10; Readtable:CL -*-


(defstruct (pkt-history (:type :list))
  pkt
  direction
  time
  )
(defvar *pkt-history* nil)
(defvar *enable-pkt-history* nil)

(defun record (pkt direction)
  (when *enable-pkt-history*
    (let ((save-pkt (get-pkt)))
      (copy-array-contents pkt save-pkt)
      (let ((struct (make-pkt-history
                      :pkt save-pkt
                      :direction direction
                      :time (time))))
        (without-interrupts
          (push struct *pkt-history*))))))

(defun print-history (&optional (max 50))
  (let ((history (without-interrupts (reverse *pkt-history*))))
    (do ((tail history (cdr tail))
         hist
         pkt
         (iss (send conn :iss))
         (irs (send conn :irs))
         )
        ((null tail))
      (setq hist (car tail))
      (setq pkt (pkt-history-pkt hist))
      (format t "~&")
      (ecase (pkt-history-direction hist)
        (:received
         (format t "R ")
         (format t "~5d " (32-bit-minus (tcp-sequence-number pkt) irs))
         )
        (:transmitted (format t "~40tT ")))
      )))


(defun initialize ()
  (setq *tcp-conns* nil)
  (setq *ip-pkts* nil)
  (setq *pkts* nil)
  (install-ip-receiver)
  (start-ip-receiver)
  (start-ip-background)
  (reset-stats)
  )

(defvar *pkts* nil)

(defun get-pkt ()
  (let ((pkt (without-interrupts
               (cond ((null *pkts*)
                      (make-array 762.
                                  :type :art-16b
                                  :leader-length 2
                                  :named-structure-symbol 'pkt))
                     (t
                      (pop *pkts*))))))
    (array-initialize pkt 0)))

(defun free-pkt (pkt)
  (when (or (not (eq (array-type pkt) 'art-16b))
            (not (eq (array-leader pkt 1) 'pkt)))
    (ferror nil "trying to free bad pkt ~s" pkt))
  (when (memq pkt *pkts*)
    (ferror nil "pkt already freed"))
  (without-interrupts
    (push pkt *pkts*))
  nil)

(defun convert-int-pkt-and-free (int-pkt &optional n-half-words)
  "Convert to regular pkt, and free INT-PKT"
  (let ((pkt (get-pkt)))
    (cond ((null n-half-words)
           (copy-array-contents int-pkt pkt))
          (t
           (copy-array-portion int-pkt 0 n-half-words pkt 0 n-half-words)))
    (chaos:free-int-pkt int-pkt)
    pkt))

(defun copy-to-int-pkt (pkt int-pkt &optional n-half-words)
  (cond ((null n-half-words)
         (copy-array-contents pkt int-pkt))
        (t
         (copy-array-portion pkt 0 n-half-words int-pkt 0 n-half-words)))
  int-pkt)

(defvar *ip-stats* nil)

(defmacro defstat (name initial-value)
  `(progn (defvar ,name ,initial-value)
          (when (not (assq ',name *ip-stats*))
            (setq *ip-stats* (append *ip-stats* (list (cons ',name ',initial-value)))))))

(defun stats ()
  (dolist (s *ip-stats*)
    (when (numberp (symbol-value (car s)))
      (format t "~&~10d ~s" (symbol-value (car s)) (car s))))
  (dolist (s *ip-stats*)
    (when (not (numberp (symbol-value (car s))))
      (format t "~&~s ~s" (car s) (symbol-value (car s))))))

(defun reset-stats ()
  (dolist (s *ip-stats*)
    (set (car s) (cdr s))))

(defun reverse-internet-address (adr)
  (+ (dpb (ldb (byte 8 0) adr) (byte 8 24.) 0)
     (dpb (ldb (byte 8 8) adr) (byte 8 16.) 0)
     (dpb (ldb (byte 8 16.) adr) (byte 8 8) 0)
     (dpb (ldb (byte 8 24.) adr) (byte 8 0) 0)))

(defun setup-my-internet-address ()
  (when (null (send si:local-host :network-address :internet))
    (let ((proposed-address #x64000044))
      (loop until (null (si:get-host-from-address proposed-address :internet))
            do (incf proposed-address))
      (nconc (symeval-in-instance si:local-host 'si:alist-elem)
             (list :internet
                   (list proposed-address))))))

(setup-my-internet-address)

(defconst my-internet-address (send si:local-host :network-address :internet))

(defmacro ih-version (ip)
  `(ldb (byte 4 4) (aref ,ip 0)))

(defmacro ih-internet-header-length (ip)
  `(ldb (byte 4 0) (aref ,ip 0)))

(defmacro ih-type-of-service (ip)
  `(ldb (byte 8 8) (aref ,ip 0)))

(defun ih-length (ip)
  "In bytes"
  (dpb (ldb (byte 8 0) (aref ip 1))
       (byte 8 8)
       (ldb (byte 8 8) (aref ip 1))))
(defun set-ih-length (ip val)
  (aset (dpb (ldb (byte 8 0) val)
             (byte 8 8)
             (ldb (byte 8 8) val))
        ip 1)
  val)
(defsetf ih-length set-ih-length)

(defmacro ih-identification (ip)
  `(aref ,ip 2))

(defmacro ih-flags (ip)
  `(ldb (byte 3 13.) (aref ,ip 3)))

(defmacro ih-fragment-offset (ip)
  `(ldb (byte 13. 0) (aref ,ip 3)))

(defmacro ih-time-to-live (ip)
  `(ldb (byte 8 0) (aref ,ip 4)))

(defmacro ih-protocol (ip)
  `(ldb (byte 8 8) (aref ,ip 4)))

(defmacro ih-header-checksum (ip)
  `(aref ,ip 5))

(defun ih-source-address (ip)
  (+ (dpb (ldb (byte 8 0) (aref ip 6)) (byte 8 24.) 0)
     (dpb (ldb (byte 8 8) (aref ip 6)) (byte 8 16.) 0)
     (dpb (ldb (byte 8 0) (aref ip 7)) (byte 8 8) 0)
     (dpb (ldb (byte 8 8) (aref ip 7)) (byte 8 0) 0)))
(defun set-ih-source-address (ip val)
  (aset (dpb (ldb (byte 8 16.) val)
             (byte 8 8)
             (ldb (byte 8 24.) val))
        ip 6)
  (aset (dpb (ldb (byte 8 0) val)
             (byte 8 8)
             (ldb (byte 8 8) val))
        ip 7)
  val)
(defsetf ih-source-address set-ih-source-address)

(defun ih-dest-address (ip)
  (+ (dpb (ldb (byte 8 0) (aref ip 8)) (byte 8 24.) 0)
     (dpb (ldb (byte 8 8) (aref ip 8)) (byte 8 16.) 0)
     (dpb (ldb (byte 8 0) (aref ip 9)) (byte 8 8) 0)
     (dpb (ldb (byte 8 8) (aref ip 9)) (byte 8 0) 0)))
(defun set-ih-dest-address (ip val)
  (aset (dpb (ldb (byte 8 16.) val)
             (byte 8 8)
             (ldb (byte 8 24.) val))
        ip 8)
  (aset (dpb (ldb (byte 8 0) val)
             (byte 8 8)
             (ldb (byte 8 8) val))
        ip 9)
  val)
(defsetf ih-dest-address set-ih-dest-address)

(defmacro ih-sub-header-offset (ip)
  `(* (ih-internet-header-length ,ip) 2))

(defun describe-ih (ih)
  (format t "~&version: ~s" (ih-version ih))
  (format t "~&header-length: ~s" (ih-internet-header-length ih))
  (format t "~&type-of-service: ~s" (ih-type-of-service ih))
  (format t "~&length: ~s" (ih-length ih))
  (format t "~&identification: ~s" (ih-identification ih))
  (format t "~&flags: ~s" (ih-flags ih))
  (format t "~&fragment-offset: ~s" (ih-fragment-offset ih))
  (format t "~&time-to-live: ~s" (ih-time-to-live ih))
  (format t "~&protocol: ~s" (ih-protocol ih))
  (format t "~&header-checksum: ~s" (ih-header-checksum ih))
  (format t "~&source-address: ~16r ~s" (ih-source-address ih)
          (si:get-host-from-address (ih-source-address ih) :internet))
  (format t "~&dest-address: ~16r ~s" (ih-dest-address ih)
          (si:get-host-from-address (ih-dest-address ih) :internet))
  (cond ((= (ih-protocol ih) 6)
         (describe-tcp ih))
        (t
         (format t "~&rest of data:")
         (dotimes (i (- (ceiling (ih-length ih) 2) (* (ih-internet-header-length ih) 2)))
           (if (zerop (ldb (byte 3 0) i))
               (format t "~&"))
           (format t "~16,4,'0r " (aref ih (+ i (* (ih-internet-header-length ih) 2)))))))
  )

(defmacro icmp-type (ip)
  `(ldb (byte 8 0) (aref ,ip (* (ih-internet-header-length ,ip) 2))))

(defmacro icmp-code (ip)
  `(ldb (byte 8 8) (aref ,ip (* (ih-internet-header-length ,ip) 2))))

(defmacro icmp-checksum (ip)
  `(aref ,ip (+ (* (ih-internet-header-length ,ip) 2) 1)))

(defmacro icmp-echo-identifier (ip)
  `(aref ,ip (+ (* (ih-internet-header-length ,ip) 2) 2)))

(defmacro icmp-echo-sequence-number (ip)
  `(aref ,ip (+ (* (ih-internet-header-length ,ip) 2) 3)))

(defun icmp-echo-data-offset-in-halfwords (ip)
  (+ (* (ih-internet-header-length ip) 2) 4))

(defun icmp-echo-data-offset-in-bytes (ip)
  (+ (* (ih-internet-header-length ip) 4) 8))

(defun icmp-echo-data-length (ip)
  (- (ih-length ip)
     (icmp-echo-data-offset-in-bytes ip)))

(defun icmp-echo-data-string (ip)
  (make-array (icmp-echo-data-length ip)
              :type art-string
              :displaced-to ip
              :displaced-index-offset (icmp-echo-data-offset-in-bytes ip)))

(defmacro swap-two-bytes (w)
  `(dpb (ldb (byte 8 0) ,w) (byte 8 8) (ldb (byte 8 8) ,w)))

(defmacro tcp-source-port (ip)
  `(swap-two-bytes (aref ,ip (ih-sub-header-offset ,ip))))
(defsetf tcp-source-port (ip) (val)
  `(aset (swap-two-bytes ,val) ,ip (ih-sub-header-offset ,ip)))

(defmacro tcp-dest-port (ip)
  `(swap-two-bytes (aref ,ip (+ (ih-sub-header-offset ,ip) 1))))
(defsetf tcp-dest-port (ip) (val)
  `(aset (swap-two-bytes ,val) ,ip (+ (ih-sub-header-offset ,ip) 1)))

(defun tcp-sequence-number (ip)
  (let ((offset (ih-sub-header-offset ip)))
    (+ (dpb (ldb (byte 8 0) (aref ip (+ offset 2))) (byte 8 24.) 0)
       (dpb (ldb (byte 8 8) (aref ip (+ offset 2))) (byte 8 16.) 0)
       (dpb (ldb (byte 8 0) (aref ip (+ offset 3))) (byte 8 8) 0)
       (dpb (ldb (byte 8 8) (aref ip (+ offset 3))) (byte 8 0) 0))))
(defun set-tcp-sequence-number (ip val)
  (let ((offset (ih-sub-header-offset ip)))
    (aset (dpb (ldb (byte 8 16.) val)
               (byte 8 8)
               (ldb (byte 8 24.) val))
          ip (+ offset 2))
    (aset (dpb (ldb (byte 8 0) val)
               (byte 8 8)
               (ldb (byte 8 8) val))
          ip (+ offset 3)))
  val)
(defsetf tcp-sequence-number set-tcp-sequence-number)

(defun tcp-ack-number (ip)
  (let ((offset (ih-sub-header-offset ip)))
    (+ (dpb (ldb (byte 8 0) (aref ip (+ offset 4))) (byte 8 24.) 0)
       (dpb (ldb (byte 8 8) (aref ip (+ offset 4))) (byte 8 16.) 0)
       (dpb (ldb (byte 8 0) (aref ip (+ offset 5))) (byte 8 8) 0)
       (dpb (ldb (byte 8 8) (aref ip (+ offset 5))) (byte 8 0) 0))))
(defun set-tcp-ack-number (ip val)
  (let ((offset (ih-sub-header-offset ip)))
    (aset (dpb (ldb (byte 8 16.) val)
               (byte 8 8)
               (ldb (byte 8 24.) val))
          ip (+ offset 4))
    (aset (dpb (ldb (byte 8 0) val)
               (byte 8 8)
               (ldb (byte 8 8) val))
          ip (+ offset 5)))
  val)
(defsetf tcp-ack-number set-tcp-ack-number)

(defmacro tcp-data-offset (ip)
  "In 32 bit words"
  `(ldb (byte 4 4) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))

(defmacro tcp-urg-bit (ip)
  `(ldb (byte 1 13.) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))
(defmacro tcp-urg-bit-p (ip)
  `(not (zerop (tcp-urg-bit ,ip))))

(defmacro tcp-ack-bit (ip)
  `(ldb (byte 1 12.) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))
(defmacro tcp-ack-bit-p (ip)
  `(not (zerop (tcp-ack-bit ,ip))))

(defmacro tcp-psh-bit (ip)
  `(ldb (byte 1 11.) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))
(defmacro tcp-psh-bit-p (ip)
  `(not (zerop (tcp-psh-bit ,ip))))

(defmacro tcp-rst-bit (ip)
  `(ldb (byte 1 10.) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))
(defmacro tcp-rst-bit-p (ip)
  `(not (zerop (tcp-rst-bit ,ip))))

(defmacro tcp-syn-bit (ip)
  `(ldb (byte 1 9) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))
(defmacro tcp-syn-bit-p (ip)
  `(not (zerop (tcp-syn-bit ,ip))))

(defmacro tcp-fin-bit (ip)
  `(ldb (byte 1 8) (aref ,ip (+ (ih-sub-header-offset ,ip) 6))))
(defmacro tcp-fin-bit-p (ip)
  `(not (zerop (tcp-fin-bit ,ip))))

(defmacro tcp-window (ip)
  `(swap-two-bytes (aref ,ip (+ (ih-sub-header-offset ,ip) 7))))
(defsetf tcp-window (ip) (val)
  `(aset (swap-two-bytes ,val) ,ip (+ (ih-sub-header-offset ,ip) 7)))

(defmacro tcp-checksum (ip)
  `(aref ,ip (+ (ih-sub-header-offset ,ip) 8)))

(defmacro tcp-urgent-pointer (ip)
  `(swap-two-bytes (aref ,ip (+ (ih-sub-header-offset ,ip) 9))))
(defsetf tcp-urgent-pointer (ip) (val)
  `(aset (swap-two-bytes ,val) ,ip (+ (ih-sub-header-offset ,ip) 9)))

(defun tcp-data-bytes (ip)
  (- (ih-length ip)
     (* 4 (ih-internet-header-length ip))
     (* 4 (tcp-data-offset ip))))

(defselect ((pkt si:named-structure-invoke))
  (:print-self (pkt stream ignore ignore)
    (si:printing-random-object (pkt stream :type)
      (case (ih-protocol pkt)
        (6
         (format stream "TCP Ack ~d Wnd ~d Seq [~d,~d) Len ~d"
                 (tcp-ack-number pkt)
                 (tcp-window pkt)
                 (tcp-sequence-number pkt)
                 (+ (tcp-sequence-number pkt) (compute-seg.len pkt))
                 (compute-seg.len pkt)))
        (t
         (format stream "Protocol ~d" (ih-protocol pkt)))
        )))
  (:which-operations (ignore)
    `(:print-self)))

(defun store-header-checksum (ip &aux (sum 0))
  (setf (ih-header-checksum ip) 0)
  (dotimes (i (* 2 (ih-internet-header-length ip)))
    (incf sum (aref ip i))
    (if (ldb-test (byte 1 16.) sum)
        (setq sum (1+ (ldb (byte 16. 0) sum)))))
  (setf (ih-header-checksum ip) (logxor #o177777 sum)))

(defun check-header-checksum (ip &aux (sum 0))
  (dotimes (i (* 2 (ih-internet-header-length ip)))
    (incf sum (aref ip i))
    (if (ldb-test (byte 1 16.) sum)
        (setq sum (1+ (ldb (byte 16. 0) sum)))))
  (values (= sum 177777) sum))

(defun store-icmp-checksum (ip &aux (sum 0))
  (macrolet ((add-word (word)
                       `(progn
                          (incf sum ,word)
                          (if (ldb-test (byte 1 16.) sum)
                              (setq sum (1+ (ldb (byte 16. 0) sum)))))))
    (when (oddp (ih-length ip))
      (setf (ldb (byte 8 0) (aref ip (floor (ih-length ip) 2))) 0))
    (setf (icmp-checksum ip) 0)
    (let ((offset (* 2 (ih-internet-header-length ip))))
      (dotimes (i (- (ceiling (ih-length ip) 2) offset))
        (add-word (aref ip (+ i offset)))))
    (setf (icmp-checksum ip) (logxor #o177777 sum))))

(defun check-icmp-checksum (ip &aux (sum 0))
  (let ((offset (* 2 (ih-internet-header-length ip))))
    (dotimes (i (- (ceiling (ih-length ip) 2) offset))
      (incf sum (aref ip (+ i offset)))
      (if (ldb-test (byte 1 16.) sum)
          (setq sum (1+ (ldb (byte 16. 0) sum)))))
    (values (= sum 177777) sum)))

(defun store-tcp-checksum (ip &aux (sum 0))
  (macrolet ((add-word (word)
                       `(progn
                          (incf sum ,word)
                          (if (ldb-test (byte 1 16.) sum)
                              (setq sum (1+ (ldb (byte 16. 0) sum)))))))
    (setf (tcp-checksum ip) 0)
    (add-word (aref ip 6))                      ;ih-source-address
    (add-word (aref ip 7))                      ;ih-source-address
    (add-word (aref ip 8))                      ;ih-dest-address
    (add-word (aref ip 9))                      ;ih-dest-address
    (add-word (dpb (ih-protocol ip) (byte 8 8) 0))
    (let ((len (- (ih-length ip) (* 4 (ih-internet-header-length ip)))))
      (add-word (dpb (ldb (byte 8 0) len) (byte 8 8) (ldb (byte 8 8) len))))
    (let ((offset (* 2 (ih-internet-header-length ip))))
      (dotimes (i (- (ceiling (ih-length ip) 2) offset))
        (add-word (aref ip (+ i offset)))))
    (setf (tcp-checksum ip) (logxor #o177777 sum))))

(defun check-tcp-checksum (ip &aux (sum 0))
  (labels ((add-word (word)
                     (incf sum word)
                     (if (ldb-test (byte 1 16.) sum)
                         (setq sum (1+ (ldb (byte 16. 0) sum))))))
    (add-word (ldb (byte 16. 0) (ih-source-address ip)))
    (add-word (ldb (byte 16. 16.) (ih-source-address ip)))
    (add-word (ldb (byte 16. 0) (ih-dest-address ip)))
    (add-word (ldb (byte 16. 16.) (ih-dest-address ip)))
    (add-word (- (ih-length ip) (* 4 (ih-internet-header-length ip))))
    (add-word (ih-protocol ip))
    (let ((offset (* 2 (ih-internet-header-length ip))))
      (dotimes (i (- (ceiling (ih-length ip) 2) offset))
        (add-word (aref ip (+ i offset)))))
    (cond ((zerop (logxor #o177777 sum))
           "Good")
          (t
           "Bad"))))

(defvar *ip-pkts* nil)

(defstat *ip-received-ip-pkts* 0)

(defun receive-ip-pkt (int-pkt)
  (incf *ip-received-ip-pkts*)
  (when (< (length *ip-pkts*) 10.)
    (let ((pkt (get-pkt)))
      (copy-array-contents int-pkt pkt)
      (setq *ip-pkts* (nconc *ip-pkts* (list pkt)))))
  (chaos:free-int-pkt int-pkt))


(defun install-ip-receiver ()
  (setq chaos:ethernet-pkt-handler-alist
        (delq (assq ethernet:ip-ethernet-type
                    chaos:ethernet-pkt-handler-alist)
              chaos:ethernet-pkt-handler-alist))
  (push (cons ethernet:ip-ethernet-type 'receive-ip-pkt)
        chaos:ethernet-pkt-handler-alist))

(defvar *ip-protocols* '((1 . ip-receive-icmp)
                         (6 . ip-receive-tcp)))

(defstat *ip-unknown-protocols* nil)
(defstat *ip-received-pkts* 0)


(defun ip-receive-process-top-level ()
  (let ((*package* (symbol-package 'foo)))
    (do-forever
      (process-wait "IP Pkt" #'(lambda () *ip-pkts*))
      (let (ip)
        (without-interrupts
          (setq ip (pop *ip-pkts*)))
        (when ip
          ;(record ip :received)
          (incf *ip-received-pkts*)
          (let ((receive-function (assq (ih-protocol ip) *ip-protocols*)))
            (cond ((null receive-function)
                   (when (not (memq (ih-protocol ip) *ip-unknown-protocols*))
                     (push (ih-protocol ip) *ip-unknown-protocols*))
                   (free-pkt ip)
                   )
                  (t
                   (funcall (cdr receive-function) ip)))))))))

(defvar *ip-receiver* nil)

(defun start-ip-receiver ()
  (when (null *ip-receiver*)
    (setq *ip-receiver* (make-process "IP Receiver")))
  (send *ip-receiver* :preset 'ip-receive-process-top-level)
  (process-reset-and-enable *ip-receiver*))

(defconst *tcp-background-quantum* 30.)
(defun ip-background-top-level ()
  (let ((*package* (symbol-package 'foo))
        time)
    (do-forever
      (setq time (time))
      (dolist (c *tcp-conns*)
        (send c :retransmit)
        (send c :send-ack-if-needed time))
      (let ((next-time (time-increment time *tcp-background-quantum*)))
        (process-wait "IP Background" #'(lambda () (time-lessp next-time (time))))))))

(defvar *ip-background* nil)

(defun start-ip-background ()
  (when (null *ip-background*)
    (setq *ip-background* (make-process "IP Background")))
  (send *ip-background* :preset 'ip-background-top-level)
  (process-reset-and-enable *ip-background*))


(defstruct (ip-icmp-request (:type :named-array))
  type
  code
  identifier
  sequence-number
  pkt)

(defvar *ip-icmp-requests* nil)
(defstat *ip-icmp-random-replies* 0)

(defun ip-receive-icmp (ip)
  (case (icmp-type ip)
    ((0 8 13. 14. 15. 16.)
     (dolist (request *ip-icmp-requests*)
       (when (and (= (icmp-type ip) (ip-icmp-request-type request))
                  (= (icmp-echo-identifier ip) (ip-icmp-request-identifier request))
                  (= (icmp-echo-sequence-number ip) (ip-icmp-request-sequence-number request)))
         (setf (ip-icmp-request-pkt request) ip)
         (setq *ip-icmp-requests* (delq request *ip-icmp-requests*))
         (return-from ip-receive-icmp nil)))
     (free-pkt ip)
     (incf *ip-icmp-random-replies*))
    (t
     (free-pkt ip)
     (incf *ip-icmp-random-replies*))))

(defvar *default-identification* 0)

(defun get-ip-int-pkt ()
  (let ((int-pkt (chaos:allocate-int-pkt)))
    (setf (ih-version int-pkt) 4)
    (setf (ih-internet-header-length int-pkt) 5)
    (setf (ih-length int-pkt) 10.)
    (setf (ih-type-of-service int-pkt) 0)
    (setf (ih-identification int-pkt) (incf *default-identification*))
    (setf (ih-flags int-pkt) 0)
    (setf (ih-fragment-offset int-pkt) 0)
    (setf (ih-time-to-live int-pkt) 60.)
    (setf (ih-source-address int-pkt) my-internet-address)
    int-pkt))

(defun get-ip-pkt ()
  (let ((ip (get-pkt)))
    (setf (ih-version ip) 4)
    (setf (ih-internet-header-length ip) 5)
    (setf (ih-length ip) 10.)
    (setf (ih-type-of-service ip) 0)
    (setf (ih-identification ip) (incf *default-identification*))
    (setf (ih-flags ip) 0)
    (setf (ih-fragment-offset ip) 0)
    (setf (ih-time-to-live ip) 60.)
    (setf (ih-source-address ip) my-internet-address)
    ip))

(defvar *max-ack* 0)

(defun send-ip-int-pkt (int-pkt)
  ;(recored int-pkt :transmited)

  (let ((dest-ether-address (ethernet:get-ethernet-address (reverse-internet-address
                                                             (ih-dest-address int-pkt)) :ip)))
    (cond ((null dest-ether-address)
           (chaos:free-int-pkt int-pkt))
          (t
           (setq *max-ack* (max *max-ack* (tcp-ack-number int-pkt)))
           (ethernet:send-int-pkt-via-ethernet
             int-pkt
             ethernet:my-ethernet-address
             dest-ether-address
             ethernet:ip-ethernet-type
             (max (ceiling (ih-length int-pkt) 2)
                  100.))))))

(defstat *tcp-pkts-sent* 0)


(defun send-ip-pkt (ip)

  (incf *tcp-pkts-sent*)
  (send-ip-int-pkt (copy-to-int-pkt ip (chaos:allocate-int-pkt))))

(defun dot-to-address (string)
  (setq string (string string))
  (let* ((dot1 (string-search-char #\. string 0))
         (dot2 (string-search-char #\. string dot1))
         (dot3 (string-search-char #\. string dot2)))
    (when (or (null dot1) (null dot2) (null dot3))
      (ferror nil "bad address"))
    (+ (dpb (parse-integer string :start 0 :end dot1 :radix 10.) (byte 8 24.) 0)
       (dpb (parse-integer string :start (1+ dot1) :end dot2 :radix 10. :junk-allowed t) (byte 8 16.) 0)
       (dpb (parse-integer string :start (1+ dot2) :end dot3 :radix 10. :junk-allowed t) (byte 8 8.) 0)
       (dpb (parse-integer string :start (1+ dot3) :radix 10. :junk-allowed t) (byte 8 0.) 0))))

(defun make-icmp-echo-request (dest string identifier sequence-number)
  (when (not (numberp dest))
    (setq dest (send (si:parse-host dest) :network-address :internet)))
  (let ((ip (get-ip-pkt)))
    (setf (ih-length ip) (+ (* 4 (ih-internet-header-length ip))
                            8
                            (string-length string)))
    (setf (ih-protocol ip) 1)
    (setf (ih-dest-address ip) dest)
    (setf (icmp-type ip) 8)
    (setf (icmp-code ip) 0)
    (setf (icmp-echo-identifier ip) identifier)
    (setf (icmp-echo-sequence-number ip) sequence-number)
    (copy-array-contents string (make-array (string-length string)
                                            :type :art-8b
                                            :displaced-to ip
                                            :displaced-index-offset (icmp-echo-data-offset-in-bytes ip)))
    (store-icmp-checksum ip)
    (store-header-checksum ip)
    ip))

(defun icmp-map-request-type-to-reply-type (code)
  (ecase code
    (8 0)
    (13. 14.)
    (15. 16.)
    ))

(defun icmp-request (ip &optional (timeout-in-60ths 60.))
  "returns NIL if no response within timeout"
  (let ((request (make-ip-icmp-request)))
    (unwind-protect
        (progn
          (setf (ip-icmp-request-type request) (icmp-map-request-type-to-reply-type (icmp-type ip)))
          (setf (ip-icmp-request-code request) (icmp-code ip))
          (setf (ip-icmp-request-identifier request) (icmp-echo-identifier ip))
          (setf (ip-icmp-request-sequence-number request) (icmp-echo-sequence-number ip))
          (setf (ip-icmp-request-pkt request) nil)
          (without-interrupts
            (push request *ip-icmp-requests*))
          (send-ip-pkt ip)
          (process-wait-with-timeout "ICMP Reply" timeout-in-60ths #'(lambda () (ip-icmp-request-pkt request)))
          (ip-icmp-request-pkt request))
      (without-interrupts
        (setq *ip-icmp-requests*
              (delq request *ip-icmp-requests*))))))

(defun send-echo-request (dest string)
  (let ((ip (make-icmp-echo-request dest string 123 321)))
    (dotimes (retries 5)
      (let ((response (icmp-request ip)))
        (when response
          (free-pkt ip)
          (return-from send-echo-request response))
        (ethernet:flush-ethernet-address (reverse-internet-address
                                           (ih-dest-address ip)) :ip)))
    (free-pkt ip)
    (ferror nil "no response")))


(defun ping (host)
  (let ((pkt (send-echo-request host "ping")))
    (cond ((null pkt)
           "No response")
          (t
           (free-pkt pkt)
           t))))

(defun 32-bit-< (a b)
  (ldb-test (byte 1 31.) (- a b)))

(defun 32-bit-<= (a b)
  (or (= a b)
      (ldb-test (byte 1 31.) (- a b))))

(defun 32-bit->= (a b)
  (not (ldb-test (byte 1 31.) (- a b))))

(defun 32-bit-> (a b)
  (and (not (= a b))
       (not (ldb-test (byte 1 31.) (- a b)))))

(defun 32-bit-plus (a b)
  (logand #o37777777777 (+ a b)))

(defun 32-bit-minus (a b)
  (logand #o37777777777
          (- (+ a #o40000000000) b)))

(defun 32-bit-max (a b)
  (if (32-bit-> a b) a b))

(defun 32-bit-min (a b)
  (if (32-bit-< a b) a b))

(defflavor tcp-conn
         ((lock nil)
          passive-open-p
          state
          source-port
          dest-port
          snd.una
          snd.nxt
          snd.wnd
          snd.up
          snd.wl1
          snd.wl2
          iss
          rcv.nxt
          rcv.wnd
          (rcv.pre.wnd 0)
          rcv.initial.wnd
          rcv.up
          irs
          transmit-list
          receive-list
          dest-address
          urgent-mode
          last-ack-sent
          time-last-ack-sent
          )
         ()
  :settable-instance-variables)

(defvar *tcp-conns* nil)
(defstat *unmatched-tcp-pkts* 0)

(defun ip-receive-tcp (new-pkt)
  (dolist (conn *tcp-conns*)
    (when (and (= (tcp-source-port new-pkt) (send conn :dest-port))
               (= (tcp-dest-port new-pkt) (send conn :source-port)))
      (send conn :receive-pkt new-pkt)
      (return-from ip-receive-tcp nil)))
  (incf *unmatched-tcp-pkts*)
  (let ((ip (chaos:allocate-int-pkt)))
    (setf (ih-protocol ip) 6)
    (setf (ih-dest-address ip) (ih-source-address new-pkt))
    (array-initialize ip 0 (ih-sub-header-offset ip)
                      (+ (ih-sub-header-offset ip) 10.))
    (setf (tcp-source-port ip) (tcp-dest-port new-pkt))
    (setf (tcp-dest-port ip) (tcp-source-port new-pkt))
    (setf (tcp-data-offset ip) 5)
    (setf (tcp-sequence-number ip) (tcp-ack-number new-pkt))
    (setf (tcp-ack-number ip) (tcp-sequence-number new-pkt))
    (setf (tcp-ack-bit ip) 1)
    (setf (tcp-rst-bit ip) 1)
    (setf (ih-length ip) (+ (* 4 (ih-internet-header-length ip))
                            (* 4 (tcp-data-offset ip))))
    (store-tcp-checksum ip)
    (store-header-checksum ip)
    (send-ip-int-pkt ip)
    (free-pkt new-pkt)))

(defmethod (tcp-conn :pkt-in-receive-window-p) (ip)
  (let* ((seg.seq (tcp-sequence-number ip))
         (seg.len (compute-seg.len ip))
         (seg.seq+seg.len-1 (32-bit-minus (32-bit-plus seg.seq seg.len) 1))
         (rcv.nxt+rcv.wnd (32-bit-plus rcv.nxt rcv.wnd)))
    (when (< seg.len 0)
      (ferror nil "negative segment length"))
    (cond ((= seg.len 0)
           (cond ((= rcv.wnd 0)
                  (= seg.seq rcv.nxt))
                 (t
                  (and (32-bit-<= rcv.nxt seg.seq)
                       (32-bit-< seg.seq rcv.nxt+rcv.wnd)))))
          (t
           (cond ((= rcv.wnd 0)
                  nil)
                 (t
                  (or (and (32-bit-<= rcv.nxt seg.seq)
                           (32-bit-< seg.seq rcv.nxt+rcv.wnd))
                      (and (32-bit-<= rcv.nxt seg.seq+seg.len-1)
                           (32-bit-< seg.seq+seg.len-1 rcv.nxt+rcv.wnd)))))))))

(defun compute-seg.len (ip)
  (+ (tcp-data-bytes ip)
     (tcp-syn-bit ip)
     (tcp-fin-bit ip)))

(defmethod (tcp-conn :check-receive-list) ()
  (let ((real-list (sort (copylist receive-list) #'(lambda (x y)
                                                     (< (tcp-sequence-number x)
                                                        (tcp-sequence-number y))))))
    (when (not (equal receive-list real-list))
      (ferror nil "foo"))))

(defmethod (tcp-conn :insert-pkt) (ip)
  ;;the receive-list is sorted by increasing seq, no dup seqs
  (when (not (eq lock current-process))
    (ferror nil "must have lock"))
  (let ((seg.seq (tcp-sequence-number ip))
        (seg.len (compute-seg.len ip)))
    (cond ((null receive-list)
           (setq receive-list (list ip)))
          ((32-bit-< seg.seq (tcp-sequence-number (car receive-list)))
           (setq receive-list (cons ip receive-list)))
          ((= seg.seq (tcp-sequence-number (car receive-list)))
           (cond ((32-bit-<= seg.len (compute-seg.len (car receive-list)))
                  (free-pkt ip))
                 (t
                  (free-pkt (car receive-list))
                  (setf (car receive-list) ip))))
          ((or (null (cdr receive-list))
               (32-bit-< seg.seq (tcp-sequence-number (cadr receive-list))))
           (setf (cdr receive-list) (cons ip (cdr receive-list))))
          ((= seg.seq (tcp-sequence-number (cadr receive-list)))
           (cond ((32-bit-<= seg.len (compute-seg.len (cadr receive-list)))
                  (free-pkt ip))
                 (t
                  (free-pkt (cadr receive-list))
                  (setf (cadr receive-list) ip))))
          (t
           (do ((pkt0 receive-list (cdr pkt0))
                (pkt1 (cdr receive-list) (cdr pkt1))
                (pkt2 (cddr receive-list) (cdr pkt2)))
               (())
             (cond ((or (null pkt2)
                        (32-bit-< seg.seq (tcp-sequence-number (car pkt2))))
                    (cond ((= seg.seq (tcp-sequence-number (car pkt1)))
                           (cond ((32-bit-<= seg.len (compute-seg.len (car pkt1)))
                                  (free-pkt ip)
                                  (return nil))
                                 (t
                                  (free-pkt (car pkt1))
                                  (setf (car pkt1) ip)
                                  (return nil))))
                          (t
                           (setf (cdr pkt1) (cons ip pkt2))
                           (return nil))))))))))

(defmethod (tcp-conn :insert-pkt) (ip)
  ;;the receive-list is sorted by increasing seq, no dup seqs
  (when (not (eq lock current-process))
    (ferror nil "must have lock"))
  (let ((seg.seq (tcp-sequence-number ip))
        (seg.len (compute-seg.len ip)))
    (setq receive-list
          (do ((pkts receive-list (cdr pkts))
               result
               )
              ((null pkts)
               (nconc (nreverse result) (list ip)))
            (let ((this-seq (tcp-sequence-number (car pkts))))
              (cond ((32-bit-< this-seq seg.seq)
                     (push (car pkts) result))
                    ((= this-seq seg.seq)
                     (cond ((32-bit-< seg.len (compute-seg.len (car pkts)))
                            (free-pkt ip)
                            (return receive-list))
                           (t
                            (free-pkt (car pkts))
                            (return (nconc (nreverse result) (list ip) (cdr pkts))))))
                    (t
                     (return (nconc (nreverse result) (list ip (car pkts)) (cdr pkts))))))))))


(defmethod (tcp-conn :reset-rcv.nxt-and-rcv.wnd) ()
  (when (not (eq lock current-process))
    (ferror nil "must have lock"))
  (do ((pkts receive-list (cdr pkts)))
      ((null pkts))
    (let* ((seg.seq (tcp-sequence-number (car pkts)))
           (seg.len (compute-seg.len (car pkts)))
           (seg.end (32-bit-plus seg.seq seg.len)))
      (cond ((32-bit-<= seg.seq rcv.nxt)
             (when (32-bit-< rcv.nxt seg.end)
               (let ((delta (32-bit-min (32-bit-minus seg.end rcv.nxt)
                                        rcv.wnd)))
                 (setq rcv.wnd (32-bit-minus rcv.wnd delta))
                 (let ((old rcv.nxt))
                   (setq rcv.nxt (32-bit-plus rcv.nxt delta))
                   (when (32-bit-< rcv.nxt old)
                     (ferror nil "rcv.nxt backed up"))
                   (when (> (32-bit-minus rcv.nxt old) seg.len)
                     (ferror nil "rcv.nxt skipped too much")))
                   )))
            (t
             (return nil))))))

(defmethod (tcp-conn :receive-pkt) (ip)
  (let ((chaos:reserved-int-pkt nil))
    (with-lock (lock)
      (case state
        (:listen (send self :receive-pkt-listen ip))
        (:syn-sent (send self :receive-pkt-syn-sent ip))
        (t (send self :receive-pkt-established ip)))))
  )

(defvar *seq-offset* 0)

(defstat *tcp-rst-pkts-received-in-listen* 0)
(defstat *tcp-ack-pkts-received-in-listen* 0)
(defstat *tcp-non-syn-non-ack-pkts-recevied-in-listen* 0)

(defmethod (tcp-conn :receive-pkt-listen) (ip)
  (block nil
    (let ((seg.seq (tcp-sequence-number ip))
          (seg.ack (tcp-ack-number ip)))
      (cond ((tcp-rst-bit-p ip)
             (incf *tcp-rst-pkts-received-in-listen*)
             (free-pkt ip)
             (return nil))
            ((tcp-ack-bit-p ip)
             (send self :send-rst seg.ack 0)
             (incf *tcp-ack-pkts-received-in-listen*)
             (free-pkt ip)
             (return nil))
            ((tcp-syn-bit-p ip)
             (setq dest-port (tcp-source-port ip))
             (setq dest-address (ih-source-address ip))
             (setq state :syn-received)
             (setq irs seg.seq)
             (setq snd.wl1 seg.seq)
             (setq rcv.nxt irs)
             (setq receive-list (list ip))
             (incf rcv.wnd)                     ;hack ... syn uses one forever
             (send self :reset-rcv.nxt-and-rcv.wnd)
             (setq iss (ldb (byte 24. 0) (time)))
             (setq snd.nxt (32-bit-plus iss 1))
             (setq snd.una iss)
             (send self :send-ack)
             (return nil))
            (t
             ;;"can't get here" since ACK should have been on
             (incf *tcp-non-syn-non-ack-pkts-recevied-in-listen*)
             (free-pkt ip)
             (return nil))))))

(defstat *tcp-bad-ack-received-in-syn-sent* 0)
(defstat *tcp-rst-received-in-syn-sent* 0)
(defstat *tcp-non-syn-received-in-syn-sent* 0)

(defmethod (tcp-conn :receive-pkt-syn-sent) (ip)
  (block nil
    (let ((seg.ack (tcp-ack-number ip))
          (seg.seq (tcp-sequence-number ip))
          )
      (cond ((and (tcp-ack-bit-p ip)
                  (or (32-bit-<= seg.ack iss)
                      (32-bit-> seg.ack snd.nxt)))
             (when (not (tcp-rst-bit-p ip))
               (send self :send-rst seg.ack 0))
             (incf *tcp-bad-ack-received-in-syn-sent*)
             (free-pkt ip)
             (return nil))
            ((tcp-rst-bit-p ip)
             (when (tcp-ack-bit-p ip)
               (setq state :closed))
             (incf *tcp-rst-received-in-syn-sent*)
             (free-pkt ip)
             (return nil))
            ((tcp-syn-bit-p ip)
             (setq irs seg.seq)
             (setq snd.wl1 seg.seq)
             (setq rcv.nxt irs)
             (setq receive-list (list ip))
             (incf rcv.wnd)                     ;hack ... syn uses up one forever
             (send self :reset-rcv.nxt-and-rcv.wnd)
             (when (tcp-ack-bit-p ip)
               (setq snd.una seg.ack)
               (send self :trim-transmit-list))
             (cond ((32-bit-> snd.una iss)
                    (setq state :established))
                   (t
                    (setq state :syn-received)))
             (send self :send-ack))
            (t
             (incf *tcp-non-syn-received-in-syn-sent*)
             (free-pkt ip)
             (return nil))))))


;;;when the user closes, you send FIN, and go to fin-wait-1
;;;when the ACK for the FIN comes back, change to FIN-WAIT-2
;;; if you receive FIN before ack of FIN, go to CLOSING
;;;when you receive a FIN, change to TIME-WAIT


(defstat *tcp-pkts-not-in-receive-window* 0)
(defstat *tcp-rst-received* 0)
(defstat *tcp-syn-received-when-established* 0)
(defstat *tcp-pkt-without-ack* 0)
(defstat *tcp-out-of-range-pkt-in-syn-received* 0)
(defstat *tcp-ack-too-high* 0)

(defvar *pkt* nil)

(defvar *tcp-sequence-numbers* nil)

(defmethod (tcp-conn :receive-pkt-established) (ip &aux dont-free)
  (block nil
    (let ((seg.seq (tcp-sequence-number ip))
          (seg.ack (tcp-ack-number ip))
          (seg.wnd (tcp-window ip))
          (seg.up (tcp-urgent-pointer ip))
          )
      (pushnew seg.seq *tcp-sequence-numbers*)
      (labels ((free-pkt-and-return ()
                                    (if (null dont-free) (free-pkt ip))
                                    (return nil)))
        (when (not (send self :pkt-in-receive-window-p ip))
          (send self :send-ack)
          (incf *tcp-pkts-not-in-receive-window*)
          (when *pkt*
            (copy-array-contents ip *pkt*)
            (setq *pkt* nil))
          (free-pkt-and-return))
        (when (tcp-rst-bit-p ip)
          (incf *tcp-rst-received*)
          (ecase state
            (:syn-received
             (setq state (if passive-open-p :listen :closed))
             (free-pkt-and-return))
            ((:established :fin-wait-1 :fin-wait-2 :close-wait
                           :closing :last-ack :time-wait :closed)
             (setq state :closed)
             (free-pkt-and-return))))
        (when (tcp-syn-bit-p ip)
          (incf *tcp-syn-received-when-established*)
          (send self :send-rst seg.ack 0)
          (setq state :closed)
          (free-pkt-and-return))
        (when (not (tcp-ack-bit-p ip))
          (incf *tcp-pkt-without-ack*)
          (free-pkt-and-return))
        (when (eq state :syn-received)
          (cond ((and (32-bit-<= snd.una seg.ack)
                      (32-bit-<= seg.ack snd.nxt))
                 (setq state :established))
                (t
                 (incf *tcp-out-of-range-pkt-in-syn-received*)
                 (free-pkt-and-return))))
        ;;update transmit-list and window based on SEG.ACK
        (cond ((32-bit-<= seg.ack snd.una)
               ;;ack too low, but be duplicate ... ignore
               )
              ((32-bit-<= seg.ack snd.nxt)
               (setq snd.una seg.ack)
               (send self :trim-transmit-list)
               (when (or (32-bit-< snd.wl1 seg.seq)
                         (and (= snd.wl1 seg.seq)
                              (32-bit-<= snd.wl2 seg.ack)))
                 (setq snd.wnd seg.wnd)
                 (setq snd.wl1 seg.seq)
                 (setq snd.wl2 seg.ack)))
              (t
               (send self :send-ack)
               (incf *tcp-ack-too-high*)
               (free-pkt-and-return)))
        ;;change state based on SEG.ACK
        (when (memq state '(:closing :last-ack :time-wait))
          (case state
            (:closing
             (when (= snd.nxt seg.ack)
               (setq state :time-wait)))
            (:last-ack
             (when (= snd.nxt seg.ack)
               (setq state :closed)))
            (:time-wait
             (send self :send-ack)
             ;;restart 2MSL timeout
             ))
          (free-pkt-and-return))
        (when (and (eq state :fin-wait-1)
                   (= snd.nxt seg.ack))
          (setq state :fin-wait-2))             ;he acks my FIN, but I haven't seen his yet
        (when (tcp-urg-bit-p ip)
          (setq rcv.up (32-bit-max rcv.up (32-bit-plus seg.seq seg.up)))
          (setq urgent-mode t))
        (when (memq state '(:established :fin-wait-1 :fin-wait-2))
          (setq dont-free t)
          (send self :insert-pkt ip)
          (send self :check-receive-list)
          (send self :reset-rcv.nxt-and-rcv.wnd)
          ;;later, delay this
          ;(send self :send-ack)
          )
        (when (tcp-fin-bit-p ip)
          (ecase state
            ((:closed :listen :syn-sent))
            ((:syn-received :established)
             (setq state :close-wait))
            (:fin-wait-1
             (setq state :closing))
            (:fin-wait-2
             (setq state :time-wait)
             ;;start timer
             )
            ((:close-wait :closing :last-ack))
            (:time-wait
             ;;restart timer
             )))
        (free-pkt-and-return)))))

(defmethod (tcp-conn :trim-transmit-list) ()
  (do ()
      ((or (null transmit-list)
           (32-bit-> (32-bit-plus (tcp-sequence-number (car transmit-list))
                                  (compute-seg.len (car transmit-list)))
                     snd.una)))
    (free-pkt (pop transmit-list))))

(defmethod (tcp-conn :make-pkt) ()
  (let ((ip (get-ip-pkt)))
    (setf (ih-protocol ip) 6)
    (setf (ih-dest-address ip) dest-address)
    (array-initialize ip 0 (ih-sub-header-offset ip)
                      (+ (ih-sub-header-offset ip) 10.))
    (setf (tcp-source-port ip) source-port)
    (setf (tcp-dest-port ip) dest-port)
    (setf (tcp-data-offset ip) 5)
    ip))

(defstat *tcp-acks-sent* 0)
(defmethod (tcp-conn :send-ack) ()
  (with-lock (lock)
    (incf *tcp-acks-sent*)
    (let ((ip (send self :make-pkt)))
      (cond ((eq state :syn-sent)
             (setf (tcp-sequence-number ip) iss)
             (setf (tcp-syn-bit ip) 1))
            (t
             (setf (tcp-sequence-number ip) snd.nxt)))
      (setf (tcp-ack-number ip) rcv.nxt)
      (setf (tcp-ack-bit ip) 1)
      (setf (tcp-window ip) rcv.wnd)
      (setf (ih-length ip) (+ (* 4 (ih-internet-header-length ip))
                              (* 4 (tcp-data-offset ip))))
      (store-tcp-checksum ip)
      (store-header-checksum ip)
      (send-ip-pkt ip)
      (free-pkt ip)
      (setq last-ack-sent rcv.nxt)
      (setq time-last-ack-sent (time))
      )))

(defconst *time-between-acks-on-idle-conn* (* 60. 10.))
(defconst *time-between-acks-on-active-conn* (* 60. 1/2))

(defmethod (tcp-conn :send-ack-if-needed) (current-time)
  (cond ((= last-ack-sent rcv.nxt)
         (when (time-lessp (time-increment time-last-ack-sent
                                           *time-between-acks-on-idle-conn*)
                           current-time)
           (send self :send-ack)))
        (t
         (when (time-lessp (time-increment time-last-ack-sent
                                           *time-between-acks-on-active-conn*)
                           current-time)
           (send self :send-ack)))))


(defstat *tcp-rsts-sent* 0)
(defmethod (tcp-conn :send-rst) (seq ack)
  (incf *tcp-rsts-sent*)
  (let ((ip (send self :make-pkt)))
    (setf (tcp-sequence-number ip) seq)
    (setf (tcp-ack-number ip) ack)
    (setf (tcp-ack-bit ip) 1)
    (setf (tcp-rst-bit ip) 1)
    (setf (ih-length ip) (+ (* 4 (ih-internet-header-length ip))
                            (* 4 (tcp-data-offset ip))))
    (store-tcp-checksum ip)
    (store-header-checksum ip)
    (send-ip-pkt ip)
    (free-pkt ip)))

(defmethod (tcp-conn :send-pkt) (ip nbytes)
  (with-lock (lock)
    (setf (tcp-sequence-number ip) snd.nxt)
    (setf (tcp-ack-number ip) rcv.nxt)
    (setf (tcp-ack-bit ip) 1)
    (setf (tcp-window ip) rcv.wnd)
    (setf (ih-length ip) (+ (* 4 (ih-internet-header-length ip))
                            (* 4 (tcp-data-offset ip))
                            nbytes))
    (store-tcp-checksum ip)
    (store-header-checksum ip)
    (setq transmit-list (nconc transmit-list (list ip)))
    (incf snd.nxt (compute-seg.len ip))))

(defmethod (tcp-conn :make-syn-pkt) ()
  (let ((ip (send self :make-pkt)))
    (setf (tcp-sequence-number ip) iss)
    (setf (tcp-ack-number ip) 0)
    (setf (tcp-ack-bit ip) 0)
    (setf (tcp-window ip) rcv.wnd)
    (setf (tcp-syn-bit ip) 1)
    (setf (ih-length ip) (+ (* 4 (ih-internet-header-length ip))
                            (* 4 (tcp-data-offset ip))))
    (store-tcp-checksum ip)
    (store-header-checksum ip)
    (setq transmit-list (nconc transmit-list (list ip)))))

(defmethod (tcp-conn :retransmit) ()
  (when (not (memq state '(:closed)))
    (with-lock (lock)
      (dolist (ip transmit-list)
        (let ((int-pkt (chaos:allocate-int-pkt nil)))
          (cond ((null int-pkt)
                 (return nil))
                (t
                 (incf *tcp-pkts-sent*)
                 (send-ip-int-pkt (copy-to-int-pkt ip int-pkt)))))))))

(defun describe-tcp (ip)
  (format t "~&TCP")
  (format t "~&Source port ~d." (tcp-source-port ip))
  (format t "~&Dest port ~d." (tcp-dest-port ip))
  (format t "~&Seq ~d." (tcp-sequence-number ip))
  (format t "~&Ack ~d." (tcp-ack-number ip))
  (format t "~&Data offset ~d." (tcp-data-offset ip))
  (format t "~&Urg ~d Ack ~d Psh ~d Rst ~d Syn ~d Fin ~d"
          (tcp-urg-bit ip)
          (tcp-ack-bit ip)
          (tcp-psh-bit ip)
          (tcp-rst-bit ip)
          (tcp-syn-bit ip)
          (tcp-fin-bit ip))
  (format t "~&Wnd ~d" (tcp-window ip))
  (format t "~&Checksum ~d" (tcp-checksum ip))
  (format t "~&Urg ptr ~d" (tcp-urgent-pointer ip)))

(defun reset ()
  (without-interrupts
    (setq *pkts* nil)
    (setq *tcp-conns* nil)
    (initialize)))

ethernet:
(defun send-addr-pkt (address protocol
                      &optional his-ether-address (on-behalf-of chaos:my-address))
  (incf addr-pkts-transmitted)
  (let* ((chaos:reserved-int-pkt nil) ;don't allocate this on if coming from scheduler
         (int-pkt (chaos:allocate-int-pkt nil))
         pkt)
    (cond ((not (null int-pkt))
           (setq pkt (get-addr-pkt int-pkt))
           (selectq protocol
             (:chaos
              (setf (ar-hardware pkt) arp-ethernet-hardware)
              (setf (ar-protocol pkt) chaos-ethernet-type)
              (setf (ar-hardware-adr-length pkt) 6)
              (setf (ar-protocol-adr-length pkt) 2)
              (setf (ar-opcode pkt) (if his-ether-address arp-reply arp-request))
              (setf (ar-hardware-sender pkt) my-ethernet-address)
              (setf (ar-protocol-sender pkt) on-behalf-of)
              (setf (ar-hardware-target pkt) (or his-ether-address 0))
              (setf (ar-protocol-target pkt) address))
             (:ip
              (setf (ar-hardware pkt) arp-ethernet-hardware)
              (setf (ar-protocol pkt) ip-ethernet-type)
              (setf (ar-hardware-adr-length pkt) 6)
              (setf (ar-protocol-adr-length pkt) 4)
              (setf (ar-opcode pkt) (if his-ether-address arp-reply arp-request))
              (setf (ar-hardware-sender pkt) my-ethernet-address)
              (setf (ar-protocol-sender pkt) ;(logior on-behalf-of #x59000000)
                    i:(reverse-internet-address my-internet-address) ;#x34000064
                    ;#x64000033
                    )
              (setf (ar-hardware-target pkt) (or his-ether-address 0))
              (setf (ar-protocol-target pkt) address))
             (t
              (ferror nil "unknown protocol")))

           (send-int-pkt-via-ethernet (ar-int-pkt pkt)
                                      my-ethernet-address
                                      (or his-ether-address -1)
                                      address-resolution-type
                                      (floor (ar-pkt-length pkt) 2))
           (free-addr-pkt pkt)))))

ethernet:
(defun addr-res-pkt-for-me-p (pkt)
  (or (= (ar-protocol-target pkt) chaos:my-address)
      (= (ar-protocol-target pkt) i:(reverse-internet-address my-internet-address))
      (and (= chaos:my-address 3140)
           (= (ar-protocol-target pkt) 3540))   ;hack for cadr2
      (and si:*share-code-ready*
           (si:processor-for-host-if-on-my-nubus (ar-protocol-target pkt)))))


(defun pkt->string (pkt)
  (make-array (* 2 (array-length pkt))
              :type :art-string
              :displaced-to pkt))

(defun data->string (pkt)
  (make-array (tcp-data-bytes pkt)
              :type :art-string
              :displaced-to pkt
              :displaced-index-offset (+ (* 4 (ih-internet-header-length pkt))
                                         (* 4 (tcp-data-offset pkt)))))

(defvar *next-iss* 1000.)
(defvar *next-local-port* 1.)

;79 finger
;23 telnet
;21 ftp
(defun tcp-test (&optional (port 23.))
  (declare (special conn s))
  (when (and (boundp 'conn) conn)
    (tcp-abort conn))
  (setq conn nil)
  (reset-stats)
  (setq conn (tcp-open "angel" port))
;  (setq conn (tcp-open "sol" port))
;  (setq conn (tcp-open "lmivax" 23.))
  (process-wait "Establish" #'(lambda (c) (eq (send c :state) :established)) conn)
  (setq s (make-tcp-stream conn))
  )

(defun tcp-test1 ()
  (let ((stream (tcp-test)))
    (process-sleep (* 60. 5))
    (send stream :string-out "pace")
    (send stream :force-output)
    (process-sleep (* 60. 10.))
    (soak stream)
    (process-sleep (* 60. 10.))
    (soak stream)
    (setq *tcp-sequence-numbers* nil)
    (setq *max-ack* 0)
    (send stream :string-out "cat /lmi/pace/foo")
    (send stream :force-output)
    (process-sleep (* 60. 3))
    (setq *seq-offset* (32-bit-plus (send conn :rcv.nxt) 2))
    ))



(defconst *tcp-default-receive-window* 401.)

(defun tcp-open (host remote-port)
  (let* ((adr (cond ((numberp host)
                     host)
                    (t
                     (send (si:parse-host host) :network-address :internet))))
         (conn (make-instance 'tcp-conn
                              :state :syn-sent
                              :source-port (incf *next-local-port*)
                              :dest-port remote-port
                              :snd.una *next-iss*
                              :snd.nxt (32-bit-plus *next-iss* 1)
                              :snd.wnd 128.
                              :snd.up 0
                              :snd.wl1 nil
                              :snd.wl2 *next-iss*
                              :iss *next-iss*
                              :rcv.nxt 0
                              :rcv.wnd *tcp-default-receive-window*
                              :rcv.pre.wnd 0
                              :rcv.initial.wnd *tcp-default-receive-window*
                              :rcv.up 0
                              :irs 0
                              :transmit-list nil
                              :receive-list nil
                              :dest-address adr
                              :last-ack-sent 0
                              :time-last-ack-sent 0
                              )))
    (incf *next-iss* 1000.)
    (push conn *tcp-conns*)
    (send conn :make-syn-pkt)
    (send conn :retransmit)
    conn))

(defun tcp-abort (conn)
  (send conn :send-rst
        (send conn :snd.nxt)
        (send conn :rcv.nxt))
  (setq *tcp-conns* (delq conn *tcp-conns*)))

(defmethod (tcp-conn :data-available-after-seq) (seq)
  (cond ((eq current-stack-group si:scheduler-stack-group)
         (and receive-list
              (32-bit-> rcv.nxt seq)))
        (t
         (with-lock (lock)
           (and receive-list
                (32-bit-> rcv.nxt seq))))))

(defmethod (tcp-conn :read-data) (after-seq no-hang-p)
  (block nil
    (when (not (eq state :established))
      (ferror nil "bad connection state"))
    (let ((initial-data-avail (send self :data-available-after-seq after-seq))
          ip
          )
      (when (and (null initial-data-avail)
                 no-hang-p)
        (return nil))
      (when (null initial-data-avail)
        (process-wait "Net Input" #'(lambda (conn)
                                      (or (not (eq (send conn :state) :established))
                                          (send conn :data-available-after-seq after-seq))) self))
      (with-lock (lock)
        (when (eq state :established)
          (setq ip (pop receive-list))))
      (when (null ip)
        (ferror nil "bad connection state"))
      ip)))

(defmethod (tcp-conn :advance-receive-window) (nbytes)
  (with-lock (lock)
    (incf rcv.pre.wnd nbytes)
    (when (> rcv.pre.wnd (* 1/2 rcv.initial.wnd))
      (incf rcv.wnd rcv.pre.wnd)
      (setq rcv.pre.wnd 0))
    ))


(defflavor tcp-stream
         (conn
          next-seq-to-read
          (last-pkt-read nil)
          read-string-array
          write-string-array
          write-pkt
          )
         (si:buffered-stream)
  :settable-instance-variables)

(defun make-tcp-stream (conn)
  (process-wait "Conn Status"
                #'(lambda () (eq (send conn :state) :established)))
  (make-instance 'tcp-stream
                 :conn conn))


(defmethod (tcp-stream :after :init) (ignore)
  (when (not (eq (send conn :state) :established))
    (ferror nil "connection must be established first"))
  (setq next-seq-to-read (32-bit-plus (send conn :irs) 1))
  (setq read-string-array (make-array 1
                                      :type :art-string
                                      :displaced-to "foo"
                                      :displaced-index-offset 1))
  (setq write-string-array (make-array 1
                                       :type :art-string
                                       :displaced-to "foo"
                                       :displaced-index-offset 1))
  )

(defmethod (tcp-stream :next-input-buffer) (&optional no-hang-p)
  (block top
    (let (ip seg.seq seg.len)
      ;;skip over pkts with too low a seq
      (do ()
          (())
        (setq ip (send conn :read-data next-seq-to-read no-hang-p))
        (when (null ip)
          (return-from top nil))
        (setq seg.seq (tcp-sequence-number ip))
        (setq seg.len (compute-seg.len ip))
        (when (and (not (zerop seg.len))
                   (32-bit-> (32-bit-plus seg.seq seg.len) next-seq-to-read))
          (return nil))
        (free-pkt ip))

      (when (32-bit-< next-seq-to-read seg.seq)
        ;;we want data before the beginning of this pkt
        (ferror nil "wrong data"))

      (when (tcp-syn-bit-p ip)
        (incf seg.seq)
        (decf seg.len))

      (when (not (null last-pkt-read))
        (ferror nil "haven't freed last pkt"))
      (setq last-pkt-read ip)
      (let ((start (+ (* 4 (ih-internet-header-length ip))
                      (* 4 (tcp-data-offset ip))
                      (32-bit-minus next-seq-to-read seg.seq)))
            (end (ih-length ip)))
        (let ((length (- end start)))
          (when (<= length 0)
            (ferror nil "??"))
          (si:change-indirect-array read-string-array
                                    art-string
                                    length
                                    ip
                                    start)
          (setq next-seq-to-read (32-bit-plus next-seq-to-read length))
          (send conn :advance-receive-window length)
          (values read-string-array 0 length))))))

(defmethod (tcp-stream :discard-input-buffer) (string)
  (when (not (eq string read-string-array))
    (ferror nil "bad buffer returned"))
  (free-pkt last-pkt-read)
  (setq last-pkt-read nil))

(defmethod (tcp-stream :new-output-buffer) ()
  (setq write-pkt (send conn :make-pkt))
  (si:change-indirect-array write-string-array
                            art-string
                            1024.
                            write-pkt
                            (+ (* 4 (ih-internet-header-length write-pkt))
                               (* 4 (tcp-data-offset write-pkt))))
  (values write-string-array 0 1024.))

(defmethod (tcp-stream :send-output-buffer) (string end)
  (when (not (eq string write-string-array))
    (ferror nil "bad buffer"))
  (send conn :send-pkt write-pkt end)
  (setq write-pkt nil)
  (send conn :retransmit)
  )

(defmethod (tcp-stream :discard-output-buffer) (string)
  (when (not (eq string write-string-array))
    (ferror nil "bad buffer"))
  (free-pkt write-pkt))

(defun soak (s &optional hang)
  (send standard-output :tyo #\)
  (let ((message (if hang :tyi :tyi-no-hang)))
    (do ((c (send s message) (send s message)))
        ((null c))
      (cond ((char= c #\
)
             (send standard-output :tyo #\newline)
             ;(tyi)
             )
            (t
             (send standard-output :tyo c))))
    (send standard-output :tyo #\)
    nil
    ))

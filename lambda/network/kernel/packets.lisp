;;; -*- Mode:LISP; Package:NETWORK; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( allocate-packet
           allocate-packet-for-receive
           free-packet
           copy-packet
           max-words-per-pkt
           max-data-words-per-pkt
           max-data-bytes-per-pkt))

;;; Here follow routines pertaining to network packets

(defparameter max-words-per-pkt (/ 1518. 2)
  "Largest packet possible in 16-bit words, including header.")
(defparameter max-data-words-per-pkt (/ 1502. 2)
  "Number of 16-bit data words in largest packet.")
(defparameter max-data-bytes-per-pkt 1502.
  "Number of 8-bit data bytes in largest packet.")

(defsubst int-pkt-word-count (int-pkt) (array-leader int-pkt sys:%chaos-leader-word-count))
(defsubst int-pkt-thread (int-pkt) (array-leader int-pkt sys:%chaos-leader-thread))
(defsubst int-pkt-csr-1 (int-pkt) (array-leader int-pkt sys:%chaos-leader-csr-1))
(defsubst int-pkt-csr-2 (int-pkt) (array-leader int-pkt sys:%chaos-leader-csr-2))
(defsubst int-pkt-bit-count (int-pkt) (array-leader int-pkt sys:%chaos-leader-bit-count))

(defvar network-buffer-list nil "the list of all network buffers")
(defvar network-buffer-area)

(defun create-network-buffers (&optional (n-buffers 0))
  (unless (boundp 'network-buffer-area)
    (let* ((leader-length (length sys:chaos-buffer-leader-qs))  ;number of elements in array leader
           (buffer-size (+ 3                                    ;leader header, leader length, array header
                           leader-length                        ;the array-leader
                           (ceiling max-words-per-pkt 2)))      ;the array-elements
           (area-size (max #o40000                              ;size of area in q's:  minimum area size
                           (* (ceiling (* n-buffers buffer-size)
                                       sys:page-size)
                              sys:page-size))))

      (sys:make-area :name 'network-buffer-area
                     :region-size area-size
                     :size area-size
                     :gc :static)

      (setq n-buffers (floor area-size buffer-size))    ;make as many buffers as will fit in area

      (dotimes (i n-buffers)
        (let ((int-pkt (zl:make-array max-words-per-pkt
                                      :type 'sys:art-16b
                                      :area network-buffer-area
                                      :leader-length leader-length)))
          (setf (fill-pointer int-pkt) 0)
          (push int-pkt network-buffer-list)))))

  (si::%wire-area network-buffer-area t)

  (do ((tail network-buffer-list (cdr tail)))
      ((null tail))
    (setf (array-leader (car tail) sys:%chaos-leader-thread) (cadr tail)))

  (setf (aref #'sys:system-communication-area sys:%sys-com-chaos-transmit-list) nil)
  (setf (aref #'sys:system-communication-area sys:%sys-com-chaos-receive-list) nil)
  (setf (aref #'sys:system-communication-area sys:%sys-com-chaos-free-list) (car network-buffer-list))
  )

;;; Interrupt (microcode) related specials
(defvar int-free-list-pointer)                  ;Freelist used by microcode
(defvar int-receive-list-pointer)               ;Packets received at interrrupt level
(defvar int-transmit-list-pointer)              ;Packets to be transmitted at interrupt level

(defsubst int-free-list () (sys:%p-contents-offset int-free-list-pointer 0))
(defsubst int-receive-list () (sys:%p-contents-offset int-receive-list-pointer 0))
(defsubst int-transmit-list () (sys:%p-contents-offset int-transmit-list-pointer 0))

(defun initialize-network-buffers (&optional (n-buffers 0))
  (without-interrupts
    (setq int-free-list-pointer (locf (aref #'sys:system-communication-area sys:%sys-com-chaos-free-list))
          int-receive-list-pointer (locf (aref #'sys:system-communication-area sys:%sys-com-chaos-receive-list))
          int-transmit-list-pointer (locf (aref #'sys:system-communication-area sys:%sys-com-chaos-transmit-list)))
    (create-network-buffers n-buffers)
    ))

(defsubst check-packet (int-pkt)
  (= (sys:%area-number int-pkt) network-buffer-area))

(defun assure-safe-int-pkt (int-pkt)
  (cond ((not (and (arrayp int-pkt)
                   (eq (global:array-type int-pkt) 'global:art-16b)
                   (net:check-packet int-pkt)))
         (error "~s is not a valid int-pkt" int-pkt))))

(defun allocate-packet (&optional (wait-if-necessary t) &aux int-pkt free-list)
  "Allocates a new INT-PKT.  May have to wait for one, so be careful
that it is ok that the process this gets called from can be safely suspended"
  (loop
    (setq free-list (int-free-list))
    (cond ((null free-list)
           (if wait-if-necessary
               (process-wait "Network buffer" #'(lambda () (int-free-list)))
             (return nil)))
          ((sys:%store-conditional-double int-free-list-pointer
                                          free-list
                                          (locf (int-pkt-thread free-list))
                                          (int-pkt-thread free-list))
           (return (setq int-pkt free-list)))))
  (and int-pkt (setf (int-pkt-thread int-pkt) nil))
  int-pkt)

(defun allocate-packet-for-receive (interface)
  "Allocates an int-pkt non-blockingly.  If none are available, notes that one is needed
in the interface structure and returns NIL"
  (cond ((ni-reserved-receive-buffer interface) ;if buffer reserved, return it and forget it
         (prog1 (ni-reserved-receive-buffer interface)
                (setf (ni-reserved-receive-buffer interface) nil)))
        ((allocate-packet nil))                 ;else, if can allocate a buffer, do so
        (t                                      ;else, count failure and note that we need a buffer
         (incf (ni-allocation-failures interface))
         (setf (ni-need-receive-buffer interface) t)
         nil)))

(defun free-packet (int-pkt)
  "Returns an INT-PKT to the free list"
  ;;Allow a displaced array to be freed (presumeably skipping link-level headers, etc.)
  (setq int-pkt (original-array int-pkt))
  (unless (check-packet int-pkt)
    (error "Attempt to free non-interrupt packet ~A" int-pkt))
  (unless (null (int-pkt-thread int-pkt))
    (error "Attempt to free an already-freed int-pkt"))
  (prog (old-free-list)
     loop
        (setq old-free-list (int-free-list))
        (setf (int-pkt-thread int-pkt) old-free-list)
        (or (sys:%store-conditional int-free-list-pointer old-free-list int-pkt)
            (go loop))
        (sys:%chaos-wakeup)))

(defun count-packets ()
  (int-pkt-list-length (int-free-list)))

(defun copy-packet (packet interface &optional no-hang-p)
  (declare (ignore interface))
  (let ((int-pkt (allocate-packet no-hang-p))
        (nw (fill-pointer packet)))
    (when int-pkt
      (copy-array-portion packet 0 nw int-pkt 0 nw)
      (setf (fill-pointer int-pkt) nw))
    int-pkt))

(DEFUN PRINT-INT-PKT (INT-PKT)
  (DO ((I 0 (1+ I)))
      ((>= I  (INT-PKT-WORD-COUNT INT-PKT)))
    (FORMAT T "~%Word ~O, data ~O" I (AREF INT-PKT I))))

(DEFUN PRINT-INT-PKT-STATUS (&OPTIONAL PRINT-ALL)
  (FORMAT T "~%Free list ~d, transmit-list ~d, receive-list ~d"
          (INT-PKT-LIST-LENGTH (sys:SYSTEM-COMMUNICATION-AREA sys:%SYS-COM-CHAOS-FREE-LIST))
          (INT-PKT-LIST-LENGTH (sys:SYSTEM-COMMUNICATION-AREA sys:%SYS-COM-CHAOS-TRANSMIT-LIST))
          (INT-PKT-LIST-LENGTH (sys:SYSTEM-COMMUNICATION-AREA sys:%SYS-COM-CHAOS-RECEIVE-LIST)))

  (WHEN PRINT-ALL
    (let* ((area network-buffer-area)
           (region (si:%area-region-list area))
           (origin (si:%region-origin region))
           (free-pointer (sys:%pointer-plus (si:%region-free-pointer region) origin)))
      (do ((l (sys:%make-pointer sys:dtp-locative origin)
              (sys:%make-pointer-offset sys:dtp-locative l (sys:%structure-total-size l)))
           int-pkt)
          ((>= (sys:%pointer-difference l free-pointer) 0))
        (when (or (not (= (sys:%p-data-type l) sys:dtp-header))
                  (not (= (sys:%p-ldb sys:%%header-type-field l) sys:%header-type-array-leader)))
          (error "bad structure in ~s" (sys:area-name area)))
        (setq int-pkt (sys:%make-pointer-offset sys:dtp-array-pointer
                                            l
                                            (sys:%p-ldb sys:%%array-leader-length l)))
        (let* ((x (int-pkt-bit-count int-pkt))
               (ethernet-type-number (if x (swap-two-bytes x))))
          (format t
                  "~%Ethernet type ~a on ~a"
                  (case ethernet-type-number
                   (#.chaos-ethernet-type 'chaos)
                   (#.arp-ethernet-type 'addr-res)
                   (#.xns-ethernet-type 'xns)
                   (#.ip-ethernet-type 'ip)
                   (otherwise ethernet-type-number))
                  (int-pkt-on-any-list int-pkt)
                  ))))))

(DEFUN INT-PKT-LIST-LENGTH (HEAD)
  (WITHOUT-INTERRUPTS
    (DO ((C 0 (1+ C))
         (PKT HEAD (INT-PKT-THREAD PKT)))
        ((or (NULL PKT)
             (> c 1000.))
         C))))

(DEFUN INT-PKT-LIST-MEMQ (ITEM INT-PKT-LIST)
  (WITHOUT-INTERRUPTS
    (DO ((PKT INT-PKT-LIST (INT-PKT-THREAD PKT))
         (c 0 (1+ c)))
        ((or (NULL PKT)
             (> c 1000.))
         nil)
      (IF (EQ PKT ITEM) (RETURN T)))))

(defun int-pkt-on-any-list (item)
  (without-interrupts
    (cond ((int-pkt-list-memq item (int-free-list)) 'free-list)
          ((int-pkt-list-memq item (int-receive-list)) 'receive-list)
          ((int-pkt-list-memq item (int-transmit-list)) 'transmit-list))))

(add-initialization "Initialize Network Buffers" '(initialize-network-buffers) '(:once))

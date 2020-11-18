;;-*- Mode:LISP; Package:ETHERNET; Base:8. -*-


;simple ethernet hacker.  This file is self contained except for change to
; CHAOS:TRANSMIT-INT-PKT!!!

(defvar *lambda-has-ethernet* t)

(defvar *ether-meters* nil)

(defmacro def-ether-meter (x)
  `(progn 'compile
          (defvar ,x 0)
          (or (memq ',x *ether-meters*)
              (setq *ether-meters* (nconc *ether-meters* (list ',x))))))

(defun reset-stats ()
  (mapcar #'(lambda (x) (set x 0)) *ether-meters*)
  nil)

(defun print-stats ()
  (mapcar #'(lambda (x)
              (format t "~&~s:~50t~8d." x (symeval x)))
          *ether-meters*)
  nil)

(def-ether-meter *ethernet-packets-received*)
(def-ether-meter *ethernet-packets-transmitted*)
(def-ether-meter *ethernet-packets-not-transmitted-xmit-buffer-not-available*)
(def-ether-meter *ethernet-chaos-pkts-transmitted*)     ;incremented by TRANSMIT-INT-PKT
(def-ether-meter *ethernet-chaos-pkts-not-transmitted-lacking-ethernet-address*)  ;ditto
(def-ether-meter *ethernet-chaos-pkts-received*)
(def-ether-meter *ethernet-chaos-pkts-too-big-to-unwrap*)
(def-ether-meter *ethernet-address-resolution-pkts-received*)
(def-ether-meter *ethernet-echo-server-requests*)
(def-ether-meter *ethernet-unknown-protocol-pkts-received*)
(def-ether-meter *ethernet-last-unknown-protocol-type*)
(def-ether-meter *ethernet-transmit-time-outs*)
(def-ether-meter *ethernet-jam-count*)
(def-ether-meter *ethernet-fcs-errors*)
(def-ether-meter *ethernet-no-address-match*)

;These counts record errors detected while examining the unwrapped ether packet.
; the have the same meanings as the chaos counters without the "ETHER-CHAOS".
(DEF-ether-meter ETHER-CHAOS-PKTS-OTHER-DISCARDED)
(DEF-ether-meter ETHER-CHAOS-PKTS-BAD-BIT-COUNT)
(DEF-ether-meter ETHER-CHAOS-PKTS-BAD-DEST)

;these probably used to be defined elsewhere
(def-ether-meter *ethernet-address-resolution-pkts-received*)
(def-ether-meter *ethernet-echo-server-requests*)

(defconst chaos-ethernet-type #x408 "ethernet type code for chaos protocol") ; =2010
(defconst address-resolution-type #x608 "ethernet type for address resolution") ; 3010

(DEFVAR ETHERNET-RECENT-HEADERS NIL
  "Array of 200 most recent packet transactions' headers.
Each row of the array contains the eight header words of the packet
and the time at which the record was made.")
(DEFVAR ETHERNET-RECENT-HEADERS-POINTER NIL
  "Next index to use in storing in ETHERNET-RECENT-HEADERS.")


; for the time being, these symbols are also in the si package associated with MINI.
(defconst 3com-mebase #x30000 "address of 3com ethernet controller")
(defconst 3com-mecsr 3com-mebase "control/status register for 3com interface")
(defconst 3com-meback (+ 2 3com-mecsr) "jam backoff counter for 3com interface")
(defconst 3com-address-rom (+ 3com-mebase #x400) "3com ethernet address ROM")
(defconst 3com-address-ram (+ 3com-mebase #x600) "3com ethernet address RAM")
(defconst 3com-transmit-buffer (+ 3com-mebase #x800) "3com transmit buffer")
(defconst 3com-buffer-a (+ 3com-mebase #x1000) "3com receive buffer A")
(defconst 3com-buffer-b (+ 3com-mebase #x1800) "3com receive buffer B")
(defconst 3com-me-buffer-size 2048. "number of bytes in a buffer")

;Note: this code operates with the byte-ordering switch ON on the 3-COM board.
;  This sets "low byte first" mode, like the 8086 and unlike the 68000.
;  Setting it this way means data CAN be read from packet buffers with 32 bit transfers.
;  This is NOT the way the board was shipped by 3-COM.
;  This means the pictures in the manual are byte reversed!
;  In particular, the byte offset words in the buffer headers are byte reversed!!!

;the function BYTE doesn't exist when this file is loaded
(defconstant bbsw #o0701)               ;set if buffer B belongs to ether.
(defconstant absw #o0601)               ;set if buffer A belongs to ether.
 (defconstant a+b-bsw #o0602)   ; both of the above.
(defconstant tbsw #o0501)               ;set if transmit buffer belongs to ether.
(defconstant jam #o0401)                ;writing 1 clears jam.
 (defconstant tbsw+jam #o0402)
(defconstant amsw #o0301)               ;address in RAM is valid
(defconstant rbba #o0201)               ;A/B receive buffer ordering.
   ; bit 1 not used
(defconstant reset #o0001)              ;reset the controller.
(defconstant binten #o1701)     ;enable interrupts on buffer B.
(defconstant ainten #o1601)     ;enable interrupts on buffer A.
(defconstant tinten #o1501)     ;enable interrupts on transmit buffer.
(defconstant jinten #o1401)     ;enable interrupts on jam.
(defconstant pa #o1004)         ;which frame addresses to accept


(defconst 3com-csr-background-bits (logior (dpb 0 pa 0)
                                           (dpb 1 amsw 0)))

(defvar my-ethernet-address :unbound)


(defun write-3com-csr (new-csr)
  (when *lambda-has-ethernet*
    (%nubus-write si:sdu-quad-slot 3com-mecsr new-csr)
    ;this also writes the jam-backoff counter, which
    ;I guess doesnt hurt.  Writing it in two pieces also has potential lossage.
    ))


(defun read-3com-csr ()
  (if *lambda-has-ethernet*
      (ldb 0020 (%nubus-read si:sdu-quad-slot 3com-mecsr))
    0))

(defvar multibus-ethernet-process nil)

;this is called by chaos:initialize-ncp-system, which is early on the system initialization list
(defun lambda-ether-init ()
  (lambda-3com-reset)
  (when *lambda-has-ethernet*
    (COND ((NULL ETHERNET-RECENT-HEADERS)
           ;;Array of 200 most recent packet transactions each row
           ;;containing the eight header words of the packet and the
           ;;time at which the record was made.
           (SETQ ETHERNET-RECENT-HEADERS (MAKE-ARRAY '(200 22) ':TYPE 'ART-8B
                                                     ':AREA PERMANENT-STORAGE-AREA))))
    (SETQ ETHERNET-RECENT-HEADERS-POINTER 0)
    (cond ((not (chaos:lambda-ucode-enabled-p))
           (process-disable multibus-ethernet-process)
           (send multibus-ethernet-process ':preset 'multibus-ethernet-receiver)
           (process-enable multibus-ethernet-process)))))

(defun lambda-3com-reset ()
  (if (null multibus-ethernet-process)
      (setq multibus-ethernet-process
            (make-process "Multibus Ethernet Receiver"
                          ':warm-boot-action nil ':priority 25.)))

  (send multibus-ethernet-process :flush)

  (when *lambda-has-ethernet*
    (write-3com-csr (dpb 1 reset 0))            ;don't used background bits here
    (setq my-ethernet-address 0)
    (dotimes (i 6)
      (let ((next-byte (%multibus-read-8 (+ 3com-address-rom i))))
        (%multibus-write-8 (+ 3com-address-ram i) next-byte)
        (setq my-ethernet-address (dpb next-byte 0010 (ash my-ethernet-address 8.)))))
    ;;set up normal csr - address RAM valid, receive MINE + Broadcast packets
    (write-3com-csr 3com-csr-background-bits)))



(defun reset-jam ()
  (write-3com-csr (dpb 1 reset 0))
  (write-3com-csr 3com-csr-background-bits)
  (incf *ethernet-jam-count*))

(DEFUN ARM-3COM-RECEIVE-BUFFER (BYTE-PTR)
  (PROG ()
    L   (COND ((NOT (ZEROP (LDB TBSW (READ-3COM-CSR))))
               (cond ((not (zerop (ldb jam (read-3com-csr))))
                      (reset-jam)))
               (GO L)))
        (WITHOUT-INTERRUPTS
          (COND ((NOT (ZEROP (LDB TBSW (READ-3COM-CSR))))
                 (GO L)))
          (write-3com-csr (dpb 1 BYTE-PTR 3com-csr-background-bits)))))

;;; LAMBDA's 3com receive buffer contains
;;; buffer-base:    meahdr (note byte reversed!!)
;;;          +2:    destination
;;;          +8:    source
;;;         +14:    type
;;;         +16:    data

(defun read-lambda-3com-buffer-header (buffer-base)
  "returns the header word of a 3com receive buffer decoded as follows:
  first value:   the number of bytes, computed from the offset field;
  second value:  T if there was a framing error, otherwise nil;
  third value:   T if address matches our own;
  fourth value:  T if frame length is out of bounds;
  fifth value:   T if it was a broadcast frame;
  sixth value:   T if the frame has a checksum error"
  (when *lambda-has-ethernet*
    (let* ((header (dpb (%multibus-read-8 buffer-base)  ;note byte-reversed.
                        1010
                        (%multibus-read-8 (1+ buffer-base))))
           (n-bytes (ldb 0013 header))
           (framing-error (= 1 (ldb 1301 header)))
           (address-match (= 1 (ldb 1401 header)))
           (range-error (= 1 (ldb 1501 header)))
           (broadcast (= 1 (ldb 1601 header)))
           (fcs-error (= 1 (ldb 1701 header))))
      (values n-bytes framing-error address-match range-error broadcast fcs-error))))

(defconst %%3com-buffer-header-nbytes 0013)
(defconst %%3com-buffer-header-framing-error 1301)
(defconst %%3com-buffer-header-address-match 1401)
(defconst %%3com-buffer-header-range-error 1501)
(defconst %%3com-buffer-header-broadcast 1601)
(defconst %%3com-buffer-header-fcs-error 1701)

(defun read-3com-buffer-header (buffer-base)
  (dpb (%multibus-read-8 buffer-base)
       1010
       (%multibus-read-8 (1+ buffer-base))))

(defun write-lambda-3com-frame-header (buffer-base offset source destination type)
  (when *lambda-has-ethernet*
    (setq offset (- offset 14.))
    (%multibus-write-8 (1+ buffer-base) (ldb 0010 offset))      ;offset reg is reversed!!!
    (%multibus-write-8  buffer-base (ldb 1010 offset))
    (%multibus-write-8 (+ buffer-base offset ) (ldb 5010 destination))
    (%multibus-write-8 (+ buffer-base offset 1) (ldb 4010 destination))
    (%multibus-write-8 (+ buffer-base offset 2) (ldb 3010 destination))
    (%multibus-write-8 (+ buffer-base offset 3) (ldb 2010 destination))
    (%multibus-write-8 (+ buffer-base offset 4) (ldb 1010 destination))
    (%multibus-write-8 (+ buffer-base offset 5) (ldb 0010 destination))

    (%multibus-write-8 (+ buffer-base offset 6) (ldb 5010 source))
    (%multibus-write-8 (+ buffer-base offset 7) (ldb 4010 source))
    (%multibus-write-8 (+ buffer-base offset 10) (ldb 3010 source))
    (%multibus-write-8 (+ buffer-base offset 11) (ldb 2010 source))
    (%multibus-write-8 (+ buffer-base offset 12) (ldb 1010 source))
    (%multibus-write-8 (+ buffer-base offset 13) (ldb 0010 source))

    (%multibus-write-8 (+ buffer-base offset 14) (ldb 0010 type))
    (%multibus-write-8 (+ buffer-base offset 15) (ldb 1010 type))))


(defun read-lambda-3com-frame-header (buffer-base)
  "returns the type of the packet in the hardware buffer"
  (when *lambda-has-ethernet*
    (dpb (%multibus-read-8 (+ buffer-base 17))
         1010
         (%multibus-read-8 (+ buffer-base 16)))))

(defun print-3com-csr ()
  (let ((csr (read-3com-csr)))
    (format t "~%Buf B belongs to ether: ~40t~d" (ldb bbsw csr))
    (format t "~%Buf A belongs to ether: ~40t~d" (ldb absw csr))
    (format t "~%Transmit buf belongs to ether: ~40t~d" (ldb tbsw csr))
    (format t "~%Jam: ~40t~d" (ldb jam csr))
    (format t "~%RAM valid: ~40t~d" (ldb amsw csr))
    (format t "~%A/B buffer ordering: ~40t~d" (ldb rbba csr))
    (format t "~%reset: ~40t~d" (ldb reset csr))        ;probably write only
    (format t "~%Enable interrupts on B buffer: ~40t~d" (ldb binten csr))
    (format t "~%Enable interrupts on A buffer: ~40t~d" (ldb ainten csr))
    (format t "~%Enable interrupts on transmit buffer: ~40t~d" (ldb tinten csr))
    (format t "~%Enable interrupts on JAM: ~40t~d" (ldb jinten csr))
    (format t "~%pa: ~40t~d" (ldb pa csr))))


;**this seems to have a timing error!  It can get here with interrupts enabled.
; Could happen in more than one process at once!!
(defun send-int-pkt-via-multibus-ethernet (int-pkt e-source e-destination e-type
                                           &optional (n-16-bit-words
                                                       (ceiling
                                                         (the fixnum
                                                              (+ (chaos:pkt-nbytes int-pkt)
                                                                 16.))
                                                         2)))
  (if (chaos:lambda-ucode-enabled-p)
      (ferror nil "can't call this function if ucode enabled"))

  (when *lambda-has-ethernet*
    (let* ((physical-size (max (* n-16-bit-words 2) 60.))       ;don't send runts
           (offset (the fixnum (- 3com-me-buffer-size physical-size)))
           (beginning-address (the fixnum (+ 3com-transmit-buffer offset))))
      (prog (csr start-time)
            (declare (fixnum csr))
            (setq start-time (%fixnum-microsecond-time))
         l  (setq csr (read-3com-csr))
            (cond ((not (zerop (ldb jam csr)))
                   (reset-jam)
                   (go l))
                  ((not (zerop (ldb tbsw csr)))
                   (cond ((> (time-difference (%fixnum-microsecond-time)
                                              start-time)
                             100000.)
                          (setq *ethernet-transmit-time-outs*
                                (1+ *ethernet-transmit-time-outs*))
                          (return nil))         ;give up.
                         (t (go l)))))
            (incf *ethernet-packets-transmitted*)
            (WITHOUT-INTERRUPTS
              (COND ((not (zerop (ldb tbsw csr)))
                     (incf *ethernet-packets-not-transmitted-xmit-buffer-not-available*))
                    (T
                     (do ((adr beginning-address (the fixnum (+ adr 2)))
                          (from-index 0 (the fixnum (1+ from-index)))
                          (n n-16-bit-words (the fixnum (1- n))))
                         ((zerop n))
                       (declare (fixnum adr from-index n))
                       (let ((data (aref int-pkt from-index)))
                         (declare (fixnum data))
                         (%multibus-write-8 adr (ldb 0010 data))
                         (%multibus-write-8 (the fixnum (1+ adr)) (ldb 1010 data))))
                     (write-lambda-3com-frame-header 3com-transmit-buffer
                                                     offset
                                                     e-source
                                                     e-destination
                                                     e-type)
                     (write-3com-csr (dpb 1 tbsw 3com-csr-background-bits)))))
            ))))

(defun multibus-3com-process-wait-function ()
  (and chaos:enable
       (not (= 3 (ldb a+b-bsw (the fixnum (read-3com-csr))))))) ;either buffer available

(defun multibus-ethernet-receiver ()
  (*catch 'no-more-ethernet
    (when *lambda-has-ethernet*
      (arm-3com-receive-buffer ABSW)
      (arm-3com-receive-buffer BBSW)
      (error-restart-loop ((error sys:abort) "Wait for another ethernet packet")
        (process-wait "Await ether pkt or enable" 'multibus-3com-process-wait-function)
        (if (null *lambda-has-ethernet*)
            (*throw 'no-more-ethernet nil))
        (let ((csr (read-3com-csr)))
          (cond ((zerop (ldb absw csr))
                 (multibus-ethernet-receive-buffer 3com-buffer-a)
                 (arm-3com-receive-buffer ABSW))
                ((zerop (ldb bbsw csr))
                 (multibus-ethernet-receive-buffer 3com-buffer-b)
                 (arm-3com-receive-buffer BBSW))))))))

(defvar *ethernet-random-packet-alist* '((6 handle-xns-packet)
                                         (#.ip-ethernet-type handle-ip-packet)
                                         ))

(defvar *ip-pkt* nil)

(defun handle-ip-packet (buffer-base)
  (when (null *ip-pkt*)
    (let ((pkt (chaos:get-pkt)) (e-dest 0))     ;OK to go blocked
      ;;copy received data into pkt
      (setf (ldb 5010 e-dest) (%multibus-read-8 (+ buffer-base 10)))
      (setf (ldb 4010 e-dest) (%multibus-read-8 (+ buffer-base 11)))
      (setf (ldb 3010 e-dest) (%multibus-read-8 (+ buffer-base 12)))
      (setf (ldb 2010 e-dest) (%multibus-read-8 (+ buffer-base 13)))
      (setf (ldb 1010 e-dest) (%multibus-read-8 (+ buffer-base 14)))
      (setf (ldb 0010 e-dest) (%multibus-read-8 (+ buffer-base 15)))
      (do ((adr (+ buffer-base 20) (+ adr 2))
           (wd-count 0 (1+ wd-count)))
          ((>= wd-count chaos:max-words-per-pkt))
        (aset (dpb (%multibus-read-8 (+ adr 1))
                   1010
                   (%multibus-read-8 adr))
              pkt
              wd-count))
      (setf (aref pkt 5.) (aref pkt 11.))
      (setf (aref pkt 6.) (aref pkt 12.))
      (setf (aref pkt 7.) (aref pkt 13.))
      (setq *ip-pkt* pkt))))

(defun multibus-ethernet-receive-buffer (buffer-base)
  (when *lambda-has-ethernet*
    (incf *ethernet-packets-received*)
    (ethernet-record-pkt-header buffer-base)
    (let ((buffer-header (read-3com-buffer-header buffer-base)))
      (cond ((or (ldb-test %%3com-buffer-header-framing-error buffer-header)
                 (ldb-test %%3com-buffer-header-range-error buffer-header)
                 (ldb-test %%3com-buffer-header-fcs-error buffer-header))
             (incf *ethernet-fcs-errors*))
;           ((not (ldb-test %%3com-buffer-header-address-match buffer-header))
;            (incf *ethernet-no-address-match*))
            (t
             (let ((type (read-lambda-3com-frame-header buffer-base)))
               (select type
                 (chaos-ethernet-type
                  (setq *ethernet-chaos-pkts-received*
                        (1+ *ethernet-chaos-pkts-received*))
                  (let ((int-pkt (chaos:allocate-int-pkt)))     ;OK to go blocked if necessary
                    (do ((adr (the fixnum (+ buffer-base 16.))
                              (the fixnum (+ adr 2)))
                         (wd-count 0 (the fixnum (1+ wd-count))))
                        ((>= wd-count chaos:max-words-per-pkt))
                      (declare (fixnum adr wd-count))
                      (aset (dpb (%multibus-read-8 (the fixnum (+ adr 1)))
                                 1010
                                 (%multibus-read-8 adr))
                            int-pkt
                            wd-count))
                    (let ((n-16-bit-words
                            (the fixnum (+ (ceiling (the fixnum
                                                         (+ (chaos:pkt-nbytes int-pkt) 16.))
                                                    2)
                                           3))))        ; 3 for the chaos hardware source, dest, and crc
                      (cond ((<= (the fixnum n-16-bit-words)
                                 (the fixnum (+ 3 chaos:max-words-per-pkt)))
                             (setf (chaos:int-pkt-word-count int-pkt)
                                   (the fixnum (1+ n-16-bit-words)))
                             (setf (chaos:int-pkt-csr-1 int-pkt) 0)     ; say no CRC-1 error
                             (setf (chaos:int-pkt-csr-2 int-pkt) 0)     ; say no CRC-2 error
                             (setf (chaos:int-pkt-bit-count int-pkt)
                                   (* (the fixnum (- n-16-bit-words 3)) 20))    ; fill in bit count
                             (aset chaos:my-address     ; set the hardware destination
                                   int-pkt
                                   (the fixnum (- (chaos:int-pkt-word-count int-pkt) 3)))
                             (check-and-receive-chaos-int-pkt int-pkt))
                            (t

                             (incf chaos:pkts-other-discarded)
                             (incf *ethernet-chaos-pkts-too-big-to-unwrap*)
                             (chaos:free-int-pkt int-pkt))))))
                 (address-resolution-type
                  (setq *ethernet-address-resolution-pkts-received*
                        (1+ *ethernet-address-resolution-pkts-received*))
                  (let ((pkt (chaos:get-pkt)))  ;OK to go blocked
                    ;;copy received data into pkt
                    (do ((adr (+ buffer-base 16.) (+ adr 2))
                         (wd-count 0 (1+ wd-count)))
                        ((>= wd-count chaos:max-words-per-pkt))
                      (aset (dpb (%multibus-read-8 (+ adr 1))
                                 1010
                                 (%multibus-read-8 adr))
                            pkt
                            wd-count))
                    (receive-addr-pkt pkt)      ;record address; maybe send reply
                    (chaos:return-pkt pkt)))
                 (otherwise
                  (if (assoc type *ethernet-random-packet-alist*)
                      (funcall (cadr (assoc type *ethernet-random-packet-alist*)) buffer-base)
                    (setq *ethernet-unknown-protocol-pkts-received*
                          (1+ *ethernet-unknown-protocol-pkts-received*)
                          *ethernet-last-unknown-protocol-type* type))))))))))

(defun handle-xns-packet (buffer-base)
  (let ((pkt (chaos:get-pkt)) (e-dest 0))       ;OK to go blocked
    ;;copy received data into pkt
    (setf (ldb 5010 e-dest) (%multibus-read-8 (+ buffer-base 10)))
    (setf (ldb 4010 e-dest) (%multibus-read-8 (+ buffer-base 11)))
    (setf (ldb 3010 e-dest) (%multibus-read-8 (+ buffer-base 12)))
    (setf (ldb 2010 e-dest) (%multibus-read-8 (+ buffer-base 13)))
    (setf (ldb 1010 e-dest) (%multibus-read-8 (+ buffer-base 14)))
    (setf (ldb 0010 e-dest) (%multibus-read-8 (+ buffer-base 15)))
    (do ((adr (+ buffer-base 20) (+ adr 2))
         (wd-count 0 (1+ wd-count)))
        ((>= wd-count chaos:max-words-per-pkt))
      (aset (dpb (%multibus-read-8 (+ adr 1))
                 1010
                 (%multibus-read-8 adr))
            pkt
            wd-count))
    (setf (aref pkt 5.) (aref pkt 11.))
    (setf (aref pkt 6.) (aref pkt 12.))
    (setf (aref pkt 7.) (aref pkt 13.))
    (if (= (aref pkt 15.) 400)
        (progn (setf (aref pkt 15.) 1000)
               (setq *ethernet-echo-server-requests* (1+ *ethernet-echo-server-requests*))
               (send-int-pkt-via-multibus-ethernet pkt my-ethernet-address e-dest 6
                                                   chaos:max-words-per-pkt)))
    (chaos:return-pkt pkt)))


(defun check-and-receive-chaos-int-pkt (int-pkt)
  (cond ((check-over-chaos-int-pkt int-pkt)
         (without-interrupts
           (let ((chaos:reserved-int-pkt nil))

             (incf chaos:PKTS-RECEIVED)
             (chaos:receive-int-pkt int-pkt)
             (cond (chaos:reserved-int-pkt
                    (FERROR NIL "Int PKT about to be lost in ethernet stuff!"))))))))

;This checking is normally done in RECEIVE-PROCESS-NEXT-INT-PKT, which unfortunately
; is not accessible for current purposes.
(DEFUN CHECK-OVER-CHAOS-INT-PKT (INT-PKT)
  (PROG (BITS DEST)
      (SETF (CHAOS:INT-PKT-THREAD INT-PKT) NIL)
      (COND ((< (CHAOS:INT-PKT-WORD-COUNT INT-PKT)
                (+ CHAOS:FIRST-DATA-WORD-IN-PKT 3))
             ;; Less than the minimum size that can exist in the current protocol?
             (SETQ ETHER-CHAOS-PKTS-OTHER-DISCARDED
                   (1+ ETHER-CHAOS-PKTS-OTHER-DISCARDED))
             (CHAOS:FREE-INT-PKT INT-PKT)
             (RETURN NIL)))
      (SETQ DEST (CHAOS:INT-PKT-HARDWARE-DEST INT-PKT)
            BITS (CHAOS:INT-PKT-BIT-COUNT INT-PKT))
      (COND
        ;the following is a useless test for now.. also it seems to have a bug.
  ;         ((OR (< BITS 48.)
  ;              (BIT-TEST 17 BITS)
  ;              (AND (ZEROP (LOGAND 377 (AREF INT-PKT 0)))     ;Header version 0
  ;                   ( (TRUNCATE BITS 20) (+ (CHAOS:PKT-NWORDS INT-PKT) 3))))
  ;             (SETQ ETHER-CHAOS-PKTS-BAD-BIT-COUNT (1+ ETHER-CHAOS-PKTS-BAD-BIT-COUNT))
  ;             (CHAOS:FREE-INT-PKT INT-PKT))
            ((AND ( DEST 0) ( DEST CHAOS:MY-ADDRESS))
             (SETQ ETHER-CHAOS-PKTS-BAD-DEST (1+ ETHER-CHAOS-PKTS-BAD-DEST))
             (CHAOS:FREE-INT-PKT INT-PKT)
             (RETURN NIL))
            (T (RETURN INT-PKT)))
      ))

(DEFUN ETHERNET-RECORD-PKT-HEADER (BUFFER-BASE)
  ;THIS SAVES BUFFER HEADER (WITH ERROR CODE), DESTINATION, SOURCE, AND PACKET TYPE.
  ;NOTE IT IS NOT BYTE REVERSED, SO THAT HAS TO BE DONE ON READOUT.
  (COND ((AND ETHERNET-RECENT-HEADERS ETHERNET-RECENT-HEADERS-POINTER)
          ;FIRST 20 BYTES HAS DESTINATION (5), SOURCE (5), AND PACKET TYPE (2).
          ;NOTE NO BYTE REVERSING IS DONE.
         (DO I 0 (1+ I) (= I 20)
             (AS-2 (%MULTIBUS-READ-8 (+ BUFFER-BASE I))
                   ETHERNET-RECENT-HEADERS
                   ETHERNET-RECENT-HEADERS-POINTER
                   I))
         (LET ((TIME (TIME)))
           (AS-2 (LDB 0010 TIME) ETHERNET-RECENT-HEADERS ETHERNET-RECENT-HEADERS-POINTER 20)
           (AS-2 (LDB 1010 TIME) ETHERNET-RECENT-HEADERS ETHERNET-RECENT-HEADERS-POINTER 21))
         (SETQ ETHERNET-RECENT-HEADERS-POINTER (\ (1+ ETHERNET-RECENT-HEADERS-POINTER) 200)))))


(DEFUN WIPE-RECENT-HEADERS (&OPTIONAL (NBR 200))
  (DO ((I (\ (+ 177 ETHERNET-RECENT-HEADERS-POINTER) 200) (COND ((ZEROP I) 177) (T (1- I))))
        (COUNT NBR (1- COUNT)))
       ((ZEROP COUNT))
    (DOTIMES (C 22)
      (AS-2 0 ETHERNET-RECENT-HEADERS I C))))

(DEFUN ETHERNET-PRINT-RECENT-HEADERS ( &OPTIONAL (NBR 200))
   (DO ((I (\ (+ 177 ETHERNET-RECENT-HEADERS-POINTER) 200) (COND ((ZEROP I) 177) (T (1- I))))
        (COUNT NBR (1- COUNT)))
       ((ZEROP COUNT))
     (let* ((header (dpb (erp-ref i 0) 1010 (erp-ref i 1)))
            (n-bytes (ldb 0013 header))
            (framing-error (= 1 (ldb 1301 header)))
            (address-match (= 1 (ldb 1401 header)))
            (range-error (= 1 (ldb 1501 header)))
            (broadcast (= 1 (ldb 1601 header)))
            (fcs-error (= 1 (ldb 1701 header)))
            (TO (DPB (ERP-REF I 2) 5010
                  (DPB (ERP-REF I 3) 4010
                       (DPB (ERP-REF I 4) 3010
                            (DPB (ERP-REF I 5) 2010 (DPB (ERP-REF I 6) 1010 (ERP-REF I 7)))))))
            (FROM
              (DPB (ERP-REF I 10) 5010
                  (DPB (ERP-REF I 11) 4010
                       (DPB (ERP-REF I 12) 3010
                            (DPB (ERP-REF I 13) 2010
                                 (DPB (ERP-REF I 14) 1010 (ERP-REF I 15)))))))
            (TO-CHAOS (if (= to my-ethernet-address)
                          chaos:my-address
                        (GET-PROTOCOL-ADDRESS TO :CHAOS)))
            (FROM-CHAOS (GET-PROTOCOL-ADDRESS FROM :CHAOS))
            (TO-HOST (IF TO-CHAOS (SI:GET-HOST-FROM-ADDRESS TO-CHAOS :CHAOS)))
            (FROM-HOST (IF FROM-CHAOS (SI:GET-HOST-FROM-ADDRESS FROM-CHAOS :CHAOS)))
            (TO-HOST-NAME (IF TO-HOST (FUNCALL TO-HOST :SHORT-NAME)))
            (FROM-HOST-NAME (IF FROM-HOST (FUNCALL FROM-HOST :SHORT-NAME)))
            (TYPE (DPB (ERP-REF I 17) 1010 (ERP-REF I 16)))
            (TYPE-NAME (COND ((= TYPE CHAOS-ETHERNET-TYPE) :CHAOS)
                             ((= TYPE ADDRESS-RESOLUTION-TYPE) :ADDRESS-RESOLUTION)
                             (T (CADR (ASSOC TYPE *ETHERNET-RANDOM-PACKET-ALIST*))))))
       (FORMAT T "~%fcs-errror ~s, broadcast ~s, range-error ~s, adr match ~s, frame-error ~s, nbytes ~s, "
             fcs-error broadcast range-error address-match framing-error n-bytes)
       (format t "~%     TO: ~S(~16r) ~S,~%     FROM: ~S(~16r) ~S, TYPE ~S (~S)"
               to to TO-HOST-NAME
               from from FROM-HOST-NAME
             TYPE TYPE-NAME))))

(DEFUN ERP-REF (I N)
  (AREF ETHERNET-RECENT-HEADERS I N))


(defun print-3com-address-ram ()
  (if (null *lambda-has-ethernet*) (ferror nil "no ethernet present"))
  (dotimes (i 6)
    (format t "~16r " (%multibus-read-8 (+ 3com-address-ram i)))))

(defun print-3com-address-rom ()
  (if (null *lambda-has-ethernet*) (ferror nil "no ethernet present"))
  (dotimes (i 6)
    (format t "~16r " (%multibus-read-8 (+ 3com-address-rom i)))))

(defun print-rcv-buffer (buf)
  (let ((buf-base (selectq buf
                    ((a 0) 3com-buffer-a)
                    ((b 1) 3com-buffer-b))))
    (dotimes (i 30)
      (format t "~16r " (%multibus-read-8 (+ buf-base i))))))

(defun print-xmit-buffer ()
  (let ((offset (dpb (%multibus-read-8  3com-transmit-buffer)
                     1010
                     (%multibus-read-8 (1+ 3com-transmit-buffer)))))
    (format t "offset = ~s ~:* #x~16r; " offset)
    (dotimes (i 30)
      (format t "~16r " (%multibus-read-8 (+ 3com-transmit-buffer offset i))))))

;;;-*- Mode:LISP; Package:LAMBDA; Base:10 -*-

;bobp, 4/86

(defvar lmi-debug-slot)
(defvar lmi-debug-addr)
(defvar *remote-debug-board-address*)

;should allocate board instead of just checking for presence
(defun select-primary-lmi-debug-board ()
  (if (= (si:%system-configuration-lmi-debug-board-owner si:*sys-conf*)
         #xffffffff)
      (ferror nil "debug board not present"))
  (setq lmi-debug-slot (dpb (si:%system-configuration-lmi-debug-board-slot si:*sys-conf*)
                            (byte 4 0)
                            #xf0))
  (setq lmi-debug-addr (ash lmi-debug-slot 24.)))

(defun select-second-lmi-debug-board ()
  (if (= (si:%system-configuration-second-lmi-debug-board-owner si:*sys-conf*)
         #xffffffff)
      (ferror nil "second debug board not present"))
  (setq lmi-debug-slot (dpb (si:%system-configuration-second-lmi-debug-board-slot si:*sys-conf*)
                            (byte 4 0)
                            #xf0))
  (setq lmi-debug-addr (ash lmi-debug-slot 24.)))

;;;;;;;;;;;;;;;;

(defmacro cbf (&rest fields)
  "combine-bit-fields"
  (let ((sum 0))
      (dolist (f fields)
        (if (not (numberp f))
            (setq f (symeval f)))
        (setq sum (dpb #xffffff f sum)))
      sum))

;;;;;;;;;;;;;;;;

;mode register
(defconst lmi-debug-mode-reg     #xfff7fc)      ;r/w canonical nubus mode register

(defconst mode-reg-bits
          `(
            ldb-mode-init ,(byte 1 0)           ;canonical nubus init
            ldb-mode-master ,(byte 1 1)         ;enable local nubus mastership; set on reset
            ldb-mode-led ,(byte 1 2)            ;turn on LED
            ldb-mode-loopback ,(byte 1 3)       ;enable local loopback
            ldb-mode-speed ,(byte 1 4)          ;set is faster speed; reset on reset
            ldb-mode-txsync ,(byte 1 5)         ;purge communications registers
            ldb-mode-txwait ,(byte 1 6)         ;set when tx is busy sending
            ldb-mode-resprdy ,(byte 1 7)))      ;set when response reg has valid data
(assign-alternate mode-reg-bits)

(defconst lmi-debug-address-reg  #xfff7f8)      ;write address
(defconst lmi-debug-data-reg     #xfff7f4)      ;write data and start transfer
(defconst lmi-debug-response-reg #xfff7f4)      ;read response data

;write control register and
;read response-control register
(defconst lmi-debug-control-reg  #xfff7f0)      ;write control reg and start control/addr transfer
(defconst lmi-debug-response-control-reg #xfff7f0) ;read response control bits

(defconst response-control-reg-bits
          `(
            ldb-start-bit ,(byte 1 0)           ;start remote xfer
            ldb-ack-bit ,(byte 1 1)
            ldb-tm-bits ,(byte 2 2)
            ldb-axr-bit ,(byte 1 4)             ;force remote to loop xfer forever
            ldb-sp30-bit ,(byte 1 5)))
(assign-alternate response-control-reg-bits)

;;;;;;;;;;;;;;;;

;nubus analyzer ram pointer
(defconst lmi-debug-nubus-analyzer-ram-pointer  #xfff7ec)       ;r/w

(defconst nubus-analyzer-ram-pointer-bits
          `(
            ldb-analyzer-pointer ,(byte 12. 0)
            ldb-analyzer-running ,(byte 1 15.)))
(assign-alternate nubus-analyzer-ram-pointer-bits)

;nubus analyzer ram data
(defconst lmi-debug-nubus-analyzer-ram-data     #xfff7e8)       ;r/w

;nubus analyzer ram control bits
(defconst lmi-debug-nubus-analyzer-control      #xfff7e4)       ;r/w

(defconst nubus-analyzer-control-bits
          `(
            ldb-analyzer-start-bit ,(byte 1 0)  ;start remote xfer
            ldb-analyzer-ack-bit ,(byte 1 1)
            ldb-analyzer-tm-bits ,(byte 2 2)
            ldb-analyzer-arb-bits ,(byte 4 4)))
(assign-alternate nubus-analyzer-control-bits)

;nubus analyzer function reg
(defconst lmi-debug-nubus-analyzer-function-reg #xfff7e0)       ;write

(defconst nubus-analyzer-function-bits
          `(
            ldb-slave-id ,(byte 4 0)
            ldb-master-id ,(byte 4 4)
            ldb-ignore-slave-id ,(byte 1 8)
            ldb-ignore-master-id ,(byte 1 9)
            ldb-enable-analyzer ,(byte 1 10.)
            ldb-stop-when-buffer-full ,(byte 1 11.)))
(assign-alternate nubus-analyzer-function-bits)

;;;;;;;;;;;;;;;;

;explorer PC ram data
; read/write one of 4K as addressed by pc-ram-ptr
(defconst lmi-debug-explorer-pc-ram-data        #xfff7cc)       ;r/w

(defconst explorer-pc-ram-data-bits
          `(
            ldb-exp-micro-pc ,(byte 14. 0)
            ldb-exp-jumptlast ,(byte 1 14.)
            ldb-exp-bnop ,(byte 1 15.)))
(assign-alternate explorer-pc-ram-data-bits)

;explorer PC ram pointer
(defconst lmi-debug-explorer-pc-ram-pointer  #xfff7c8)  ;r/w

(defconst explorer-pc-ram-pointer-bits
          `(
            ldb-exp-pc-ram-ptr ,(byte 12. 0)))  ;ptr to address PC ram data
(assign-alternate explorer-pc-ram-pointer-bits)

;explorer interface control bits
;all but expc-e-trace are wires on the explorer processor debug connector
(defconst lmi-debug-explorer-control #xfff7c4)  ;r/w

(defconst explorer-control-bits
          `(
            expc-e-trace       ,(byte 1 0)      ;inv  NOT directly connected to explorer
            expc-e-reset       ,(byte 1 1)      ;inv
            expc-e-forcenop    ,(byte 1 2)      ;inv
            expc-e-norun       ,(byte 1 3)      ;inv
            expc-e-stepreq     ,(byte 1 4)      ;inv    CURRENTLY NOT CONNECTED; typo E_STPREQ
            expc-e-csramdis    ,(byte 1 5)      ;inv
            expc-e-xiramwe     ,(byte 1 6)      ;inv
            expc-e-irs0        ,(byte 1 7)

            expc-e-irs1        ,(byte 1 8)
            expc-e-irshft0     ,(byte 1 9)      ;e-irsin0       shift-in-0
            expc-e-irshftclk   ,(byte 1 10.)    ;e-irsclk
            expc-e-irsshften   ,(byte 1 11.)    ;inv, e-irsen   shift-enable
            expc-e-obs0        ,(byte 1 12.)
            expc-e-obs1        ,(byte 1 13.)
            expc-e-obshft0     ,(byte 1 14.)    ;e-obsin0
            expc-e-obshften    ,(byte 1 15.)    ;inv, e-obsen

            expc-e-obshftclk   ,(byte 1 16.)    ;e-obsclk
            expc-e-pcs0        ,(byte 1 17.)
            expc-e-pcshfti     ,(byte 1 18.)    ;e-pcsin0
            expc-e-pcshftclk   ,(byte 1 19.)    ;e-pcsclk
            expc-e-pcforce     ,(byte 1 20.)    ;inv
            expc-e-relhalt     ,(byte 1 21.)    ;inv
            expc-e-pardis      ,(byte 1 22.)    ;inv
            expc-e-longclk     ,(byte 1 23.)    ;inv

            expc-e-mdld        ,(byte 1 24.)    ;inv
            expc-e-setvma      ,(byte 1 25.)    ;inv
            expc-e-setmd       ,(byte 1 26.)    ;inv
            expc-e-runen       ,(byte 1 27.)))  ;inv, NOT CONNECTED; CURRENTLY E_SPARE_27
(assign-alternate explorer-control-bits)

;explorer status bits
;all but addr-rreq and data-rreq are wires on explorer processor debug connector
(defconst lmi-debug-explorer-status #xfff7c0)   ;read only

(defconst explorer-status-bits
          `(
            exps-e-promen      ,(byte 1 0)
            exps-e-irshft55    ,(byte 1 1)      ;e-irsout55
            exps-e-obshft31    ,(byte 1 2)      ;e-obsout31
            exps-e-pcshft15    ,(byte 1 3)      ;e-pcsout15
            exps-e-lowreqb     ,(byte 1 4)
            exps-e-imodlow     ,(byte 1 5)
            exps-e-imodhigh    ,(byte 1 6)
            exps-e-pagefault   ,(byte 1 7)      ;inv

            exps-e-buserr      ,(byte 1 8)      ;inv
            exps-e-halted      ,(byte 1 9)      ;inv
            exps-e-csperrq     ,(byte 1 10.)    ;inv
            exps-e-vmaen       ,(byte 1 11.)    ;inv
            exps-e-mdren       ,(byte 1 12.)    ;inv
            exps-e-SPARE13     ,(byte 1 13.)
            exps-addr-rreq     ,(byte 1 14.)    ;??
            exps-data-rreq     ,(byte 1 15.)))  ;??
(assign-alternate explorer-status-bits)

;;;;;;;;;;;;;;;;

; nubus-specific stuff

;tm bits as asserted by master; AD[01]=0
(defconst tm-read-word 0)
(defconst tm-read-byte 1)
(defconst tm-write-word 2)
(defconst tm-write-byte 3)

;tm bits as asserted during acknowledgement
(defconst tm-ack-try-again-later 0)
(defconst tm-ack-bus-timeout 1)
(defconst tm-ack-error 2)
(defconst tm-ack-complete 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst read-lmi-debug-mode ()
  (si:%nubus-read lmi-debug-slot lmi-debug-mode-reg))

(defsubst remote-read-lmi-debug-mode ()
  (debug-read-word (+ *remote-debug-board-addr* l-debug-mode-reg)))

(defsubst write-lmi-debug-mode (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-mode-reg data))

;only useful for causing the <remote> board to initiate bus cycles ...
(defsubst remote-write-lmi-debug-mode (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-mode-reg)
                    data))

(defsubst write-lmi-debug-addr (addr)
  (si:%nubus-write lmi-debug-slot lmi-debug-address-reg addr))

(defsubst remote-write-lmi-debug-addr (addr)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-address-reg)
                    addr))

(defsubst write-lmi-debug-data-start-xfer (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-data-reg data))

(defsubst remote-write-lmi-debug-data-start-xfer (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-data-reg)
                    data))

(defsubst read-lmi-debug-response ()
  (si:%nubus-read lmi-debug-slot lmi-debug-response-reg))

(defsubst remote-read-lmi-debug-response ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-response-reg)))

(defsubst write-lmi-debug-control (ctl)
  (si:%nubus-write lmi-debug-slot lmi-debug-control-reg ctl))

(defsubst remote-write-lmi-debug-control (ctl)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-control-reg)
                    ctl))

(defsubst read-debug-response-control ()
  (si:%nubus-read lmi-debug-slot lmi-debug-response-control-reg))

(defsubst remote-read-debug-response-control ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-response-control-reg)))

(defsubst read-debug-analyzer-pointer ()
  (si:%nubus-read lmi-debug-slot lmi-debug-nubus-analyzer-ram-pointer))

(defsubst remote-read-debug-analyzer-pointer ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-ram-pointer)))

(defsubst write-debug-analyzer-pointer (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-nubus-analyzer-ram-pointer data))

(defsubst remote-write-debug-analyzer-pointer (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-ram-pointer)
                    data))

(defsubst read-debug-analyzer-data ()
  (si:%nubus-read lmi-debug-slot lmi-debug-nubus-analyzer-ram-data))

(defsubst remote-read-debug-analyzer-data ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-ram-data)))

(defsubst write-debug-analyzer-data (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-nubus-analyzer-ram-data data))

(defsubst remote-write-debug-analyzer-data (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-ram-data)
                    data))

(defsubst read-debug-analyzer-control ()
  (si:%nubus-read lmi-debug-slot lmi-debug-nubus-analyzer-control-bits))

(defsubst remote-read-debug-analyzer-control ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-control-bits)))

(defsubst write-debug-analyzer-control (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-nubus-analyzer-control-bits data))

(defsubst remote-write-debug-analyzer-control (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-control-bits)
                    data))

(defsubst write-debug-analyzer-function (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-nubus-analyzer-function-reg data))

(defsubst remote-write-debug-analyzer-function (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-nubus-analyzer-function-reg)
                    data))

(defsubst read-debug-explorer-ram ()
  (si:%nubus-read lmi-debug-slot lmi-debug-explorer-pc-ram-data))

(defsubst remote-read-debug-explorer-ram ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-explorer-pc-ram-data)))

(defsubst write-debug-explorer-ram (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-explorer-pc-ram-data data))

(defsubst remote-write-debug-explorer-ram (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-explorer-pc-ram-data)
                    data))

(defsubst read-debug-explorer-pointer ()
  (si:%nubus-read lmi-debug-slot lmi-debug-explorer-pc-ram-pointer))

(defsubst remote-read-debug-explorer-pointer ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-explorer-pc-ram-pointer)))

(defsubst write-debug-explorer-pointer (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-explorer-pc-ram-pointer data))

(defsubst remote-write-debug-explorer-pointer (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-explorer-pc-ram-pointer)
                    data))

(defsubst read-debug-explorer-control ()
  (si:%nubus-read lmi-debug-slot lmi-debug-explorer-control-bits))

(defsubst remote-read-debug-explorer-control ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-explorer-control-bits)))

(defsubst write-debug-explorer-control (data)
  (si:%nubus-write lmi-debug-slot lmi-debug-explorer-control-bits data))

(defsubst remote-write-debug-explorer-control (data)
  (debug-write-word (+ *remote-debug-board-addr* lmi-debug-explorer-control-bits)
                    data))

(defsubst read-debug-explorer-status ()
  (si:%nubus-read lmi-debug-slot lmi-debug-explorer-status-bits))

(defsubst remote-read-debug-explorer-status ()
  (debug-read-word (+ *remote-debug-board-addr* lmi-debug-explorer-status-bits)))

;;;;;;;;;;;;;;;;

(defun print-lmi-debug ()
  (ldb-print-bit-list "mode"
                      (read-lmi-debug-mode) mode-reg-bits)
  (format t "~&response: ~16r" (read-lmi-debug-response))
  (ldb-print-bit-list "response control"
                      (read-debug-response-control) response-control-reg-bits)

  (ldb-print-bit-list "analyzer ram pointer"
                      (read-debug-analyzer-pointer) nubus-analyzer-ram-pointer-bits)
  (format t "~&analyzer ram data: ~16r" (read-debug-analyzer-data))
  (ldb-print-bit-list "analyzer control"
                      (read-debug-analyzer-control) nubus-analyzer-control-bits)

  (ldb-print-bit-list "explorer PC ram data"
                      (read-debug-explorer-ram) explorer-pc-ram-data-bits)
  (ldb-print-bit-list "explorer PC ram pointer"
                      (read-debug-explorer-pointer) explorer-pc-ram-pointer-bits)
  (ldb-print-bit-list "explorer interface control bits"
                      (read-debug-explorer-control) explorer-control-bits)
  (ldb-print-bit-list "explorer status bits"
                      (read-debug-explorer-status) explorer-status-bits)
  )

(defun ldb-print-bit-list (name word l)
  (format t "~&~a: ~16r" name word)
  (dolist (b l)
    (when (symbolp b)
      (format t "~&~4t~16,4r: ~a" (ldb (symeval b) word) b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-debug-board-for-loopback ()
  (write-lmi-debug-mode (cbf ldb-mode-init))    ;reset the board
  (write-lmi-debug-mode (cbf ldb-mode-master
                             ldb-mode-loopback
                             ldb-mode-speed 0))
  (process-sleep 2)
  (read-debug-response-control)
  (write-debug-analyzer-function 0)
  (read-debug-analyzer-data)
  nil)

(defun compare-sdu-ram ()
  (setup-debug-board-for-loopback)
  (dotimes (i 10000.)
    (let ((direct (si:%nubus-read #xff (* i 4)))
          (indir (debug-read-word (+ #xff000000 (* i 4)))))
      (if (null indir)
          (format t "~&~16,8r: read failed" (+ #xff000000 (* i 4)))
        (if (not (= direct indir))
            (format t "~&~16,8r: is ~16r; got ~16r"
                    (+ #xff000000 (* i 4))
                    direct
                    indir))))))

(defun compare-shared-mem ()
  "compare local shared memory as read direct and by debug board"
  (setup-debug-board-for-loopback)
  (let* ((size (ash (si:%system-configuration-global-shared-size si:*sys-conf*) -2))
         (base (si:%system-configuration-global-shared-base si:*sys-conf*))
         (slot (ash base -24))
         (offs (logand base #xffffff)))
    (dotimes (i size)
      (let ((direct (si:%nubus-read slot (+ offs (* i 4))))
            (indir (debug-read-word (+ base (* i 4)))))
        (if (null indir)
            (format t "~&~16,8r: read failed" (+ base (* i 4)))
        (if (not (= direct indir))
            (format t "~&~16,8r: is ~16r; got ~16r"
                    (+ base (* i 4))
                    direct
                    indir)))))))

(defun test-shared-mem ()
  "write local shared memory by debug board; compare by direct read"
  (setup-debug-board-for-loopback)
  (let* ((size (ash (si:%system-configuration-global-shared-size si:*sys-conf*) -2))
         (base (si:%system-configuration-global-shared-base si:*sys-conf*))
         (slot (ash base -24))
         (offs (logand base #xffffff)))
    (dotimes (i size)
      (let ((data (* i 4)))
        (debug-write-word (+ base (* i 4)) data)
        (let ((r (si:%nubus-read slot (+ base (* i 4)))))
          (if (not (= data r))
              (format t "~&~16,8r: is ~16r; got ~16r"
                      (+ base (* i 4))
                      data
                      r)))))))

(defun print-shared-mem ()
  (let* ( ;(size (ash (si:%system-configuration-global-shared-size si:*sys-conf*) -2))
         (base (si:%system-configuration-global-shared-base si:*sys-conf*))
         (slot (ash base -24))
         (offs (logand base #xffffff)))
    (dotimes (i 20.)
      (format t "~&~16r: ~16r"
              (+ base (* i 4))
              (si:%nubus-read slot (+ offs (* i 4)))))))

;;;;;;;;;;;;;;;;

(defun wait-for-debug-xmit ()
  (dotimes (i 1000.)
    (if (zerop (ldb ldb-mode-txwait (read-lmi-debug-mode)))
        (return t))))

(defun remote-wait-for-debug-xmit ()
  (dotimes (i 1000.)
    (if (zerop (ldb ldb-mode-txwait (remote-read-lmi-debug-mode)))
        (return t))))

(defun wait-for-debug-response ()
  "wait for completion; return response code; doesn't signal errors"
  (do ((i 0 (1+ i)))
      ((= i 1000.) 'no-response)
    (when (ldb-test ldb-mode-resprdy (read-lmi-debug-mode))
      (let ((resp-ctl (read-debug-response-control)))
        (if (and (= 0 (ldb ldb-start-bit resp-ctl))
                 (= 1 (ldb ldb-ack-bit resp-ctl)))
            (return
              (selectq (ldb ldb-tm-bits resp-ctl)
                (0 'try-again-later)
                (1 'bus-timeout)
                (2 'error)
                (3 (read-lmi-debug-response)))))
        ))))

;uggh, what's the difference between local and remote errors?
(defun remote-wait-for-debug-response ()
  "wait for completion; return response code; doesn't signal errors"
  (do ((i 0 (1+ i)))
      ((= i 1000.) 'no-response)
    (when (ldb-test ldb-mode-resprdy (remote-read-lmi-debug-mode))
      (let ((resp-ctl (remote-read-debug-response-control)))
        (if (and (= 0 (ldb ldb-start-bit resp-ctl))
                 (= 1 (ldb ldb-ack-bit resp-ctl)))
            (return
              (selectq (ldb ldb-tm-bits resp-ctl)
                (0 'remote-try-again-later)
                (1 'remote-bus-timeout)
                (2 'remote-error)
                (3 (remote-read-lmi-debug-response)))))
        ))))

(defun debug-read-word (addr)
  "read word; loop on try-again-laters; signals other errors"
  (do ((i 0 (1+ i)))
      ((= i 10) (ferror nil "too many try-again-laters"))
    (let ((resp (debug-read-word-raw addr)))
      (selectq resp
        ('bus-timeout
         (ferror nil "bus timeout"))
        ('error
         (ferror nil "nubus error"))
        ('try-again-later)
        (t
         (return resp))))))

(defun debug-read-word-raw (addr &optional internal)
  "read word; return 32-bit value or response code; doesn't signal errors"
  (write-lmi-debug-addr addr)
  (let ((ctl (dpb 1 ldb-start-bit 0)))
    (if internal
        (dpb 1 ldb-sp30-bit ctl))
    (write-lmi-debug-control ctl))
  (wait-for-debug-response))

(defun debug-write-word (addr data)
  "read word; loop on try-again-laters; signals other errors"
  (do ((i 0 (1+ i)))
      ((= i 10) (ferror nil "too many try-again-laters"))
    (let ((resp (debug-write-word-raw addr data)))
      (selectq resp
        ('bus-timeout
         (ferror nil "bus timeout"))
        ('error
         (ferror nil "nubus error"))
        ('try-again-later)
        (t
         (return resp))))))

(defun debug-write-word-raw (addr data &optional internal)
  "write word; return response code; doesn't signal errors"
  (let ((ctl (dpb tm-write-word ldb-tm-bits 0)))
    (write-lmi-debug-addr data)
    (write-lmi-debug-control ctl)
    (wait-for-debug-xmit)
    (write-lmi-debug-addr addr)
    (if internal
        (dpb 1 ldb-sp30-bit ctl))
    (write-lmi-debug-control (dpb 1 ldb-start-bit ctl))
    (wait-for-debug-response)))


(defun remote-debug-read-word (addr)
  (remote-write-lmi-debug-addr addr)
  (remote-write-lmi-debug-control (dpb 1 ldb-start-bit 0)))
  (remote-wait-for-debug-response))

(defun remote-debug-write-word (addr data)
  (let ((ctl (dpb tm-write-word ldb-tm-bits 0)))
    (remote-write-lmi-debug-addr addr)
    (remote-write-lmi-debug-control ctl)
    (remote-wait-for-debug-xmit)
    (remote-write-lmi-debug-addr addr)
    (remote-write-lmi-debug-control (dpb 1 ldb-start-bit ctl))
    (remote-wait-for-debug-response)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debug-rw-test (&aux temp)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (write-debug #xfff7c4 (ash 1 i))
      (setq temp (read-debug #xfff7c4))
      (when (not (equal temp (ash 1 i)))
        (format "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))

(defun debug-iloop-test (&aux temp)
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (write-debug-explorer-control (ash 1 i))
      (setq temp (debug-read-word #Xf0fff7c4 t))
      (when (not (equal temp (ash 1 i)))
        (format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp))))
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (debug-write-word #xf0fff7c4 (ash 1 i) t)
      (setq temp (read-debug-explorer-control))
      (when (not (equal temp (ash 1 i)))
        (format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))

(defun debug-iloop ()
  (setup-debug-board-for-loopback)
  (do-forever
    (write-debug-explorer-control 0)
    (when (not(equal 0 (debug-read-word #xf0fff7c4)))
      (tyo #/*))
    (write-debug-explorer-control #xffffffff)
    (when (not(equal #xffffffff (debug-read-word #xf0fff7c4)))
      (tyo #/#))))

(defun debug-serial-loop-test (&aux data temp)
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (setq data (ash 1 i))
      (write-debug-addr data)
      (write-debug-control #x0e)
      (wait-for-debug-response)
      (when (not(equal data (setq temp (read-debug-response-data))))
        (format t "Error --- wrote ~A --- read ~A~%" data temp))
      (setq data (logand #xffffffff (lognot (ash 1 i))))
      (write-debug-addr data)
      (write-debug-control #x0e)
      (wait-for-debug-response)
      (when (not(equal data (setq temp (read-debug-response-data))))
        (format t "Error --- wrote ~A --- read ~A~%" data temp)))))

(defun test-debug-analyzer-ram (&aux temp data master-data-list data-list)
  (setup-debug-board-for-loopback)
  (setq master-data-list (copylist '(#x01010101 #x02020202 #x04040404 #x08080808
                   #x10101010 #x20202020 #x40404040 #x80808080 #x55555555)))
  (rplacd (last master-data-list) master-data-list)
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (write-debug-analyzer-data (car data-list))
    (write-debug-analyzer-control (car data-list))
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data (car data-list))
    (when (not(equal data (setq temp (read-debug-analyzer-data))))
      (format t "Error data --- addr ~A --- wrote ~A --- read ~A~%" i data temp))
    (when (not(equal (logand #xff data) (setq temp (read-debug-analyzer-control))))
      (format t "Error control --- addr ~A --- wrote ~A --- read ~A~%" i (logand #xff data) temp))
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data  (logand #xffffffff (lognot (car data-list))))
    (write-debug-analyzer-data data)
    (write-debug-analyzer-control data)
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data (logand #xffffffff (lognot (car data-list))))
    (when (not(equal data (setq temp (read-debug-analyzer-data))))
      (format t "Error data --- addr ~A --- wrote ~A --- read ~A~%" i data temp))
    (when (not(equal (logand #xff data) (setq temp (read-debug-analyzer-control))))
      (format t "Error control --- addr ~A --- wrote ~A --- read ~A~%" i (logand #xff data) temp))
    (setq data-list (cdr data-list))))

(defconst analyzer-codes (make-array 16. :initial-contents
   '("Null cycle 0"     "Start read"      "ACK Try Again Later"     "Idle Cycle"
     "Intermediate ACK" "Start byte read" "ACK Parity Error"        "Bad Idle cycle 1"
     "Null cycle 2"     "Start write"     "ACK NUBUS Timeout"       "Bad Idle cycle 2"
     "Null cycle 3"     "Start byte write" "ACK OK"                 "Bad Idle cycle 3")))


(defun print-debug-analyzer (&optional (count 30.) &aux ptr data ctl master code-string)
  (read-debug-analyzer-data)
  (setq ptr (logand #xfff (read-debug-analyzer-pointer)))
  (format t "~%SEQ#~8TADDRESS~18TDATA~28TMaster~36TINFO~%")
  (dotimes (i count)
    (write-debug-analyzer-pointer (- ptr 2 i))
    (setq data (read-debug-analyzer-data))
    (setq ctl (read-debug-analyzer-control))
    (setq master (ash (logand #xf0 ctl) -4.))
    (write-debug-analyzer-pointer ptr)
    (setq code-string (aref analyzer-codes (logand ctl #x0f)))
    (cond
      ((eq 1 (logand ctl 1))
       (format t "~@4X~8T~@8X~28T~X~36T~A~%" (- ptr i) data master code-string))
      (t
       (format t "~@4X~18T~@8X~28T~X~36T~A~%" (- ptr i)  data master code-string)))))


(defun flog-debug-analyzer(&aux ptr data ctl)
  (start-debug-analyzer 0)
  (write-debug-explorer-control 0)
  (read-debug-explorer-control)
  (write-debug-explorer-control -1)
  (read-debug-explorer-control)
  (read-debug-mode)
  (read-debug-analyzer-data)
  (setq ptr (read-debug-analyzer-pointer))
  (dotimes (i 16.)
    (write-debug-analyzer-pointer (- ptr i))
    (setq data (read-debug-analyzer-data))
    (setq ctl (read-debug-analyzer-control))
    (format t "~@8X~10T~@3X~%" data ctl)))



(defun start-debug-analyzer (&optional slave master &aux temp)
  (setq temp 0)
  (write-debug-analyzer-pointer 0)
  (if slave
    (setq temp (logior temp (logand slave #x0f)))
    (setq temp (logior temp #x100)))
  (if master
    (setq temp (logior temp (ash (logand master #x0f) 4)))
    (setq temp (logior temp #x200)))
  (write-debug-analyzer-function (logior temp #x400)))

(defun debug-read-word-fast-repeat (addr &optional internal)
  (write-debug-addr addr)
  (write-debug-control #x11)
  (tyi)
  (wait-for-debug-xmit)
  (write-debug-control #x01)
  (process-sleep 2)
  (read-debug-response-control))

(defun debug-write-word-fast-repeat (addr data &optional internal)
  (write-debug-addr data)
  (write-debug-control #x08)
  (wait-for-debug-xmit)
  (write-debug-addr addr)
  (if internal
      (write-debug-control #x39)
    (write-debug-control #x19))
  (tyi)
  (wait-for-debug-xmit)
  (if internal
      (write-debug-control #x29)
    (write-debug-control #x09))
  (process-sleep 2)
  (read-debug-response-control))

;;;;;;;;;;;;;;;;

;EXPLORER

;debug board 96-pin connector connects directly to explorer processor debug connector.

;define bit names as per processor spec. and 990 assembly code.

;access local explorer interface by local nubus cycles to explorer regs
;remote explorer interface by remote cycles to ...

;copy explorer-spi, rewrite funcs to hack lmi-debug direct
;also copy regint-explorer, or just rewrite explorer-spi funcs to hack both ways?

;;;;;;;;;;;;;;;;

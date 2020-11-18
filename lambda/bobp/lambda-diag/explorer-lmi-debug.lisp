;;; -*- Mode:LISP; Package:LAMBDA; Base:8; Readtable:ZL -*-

;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.

;4/86 bobp
;serial-explorer-spi functions rewritten to use the LMI debug board.

;looks like it's really important to tri-state lmi-debug-explorer-control
;until it's been properly set up.  maybe, enable outputs on first
;write to explorer-control.

;connect missing lines as per lmi-debug-board expc- and exps- comments.
; e_runen/e_norun, e_stpreq/e_stepreq, e_promen2/e_spare27

;requires new EXPPAL to do read-straight-through of explorer-pc-ram-data

;arrange for *remote-debug-board-address* to be set.
;next rev board can supply slot number; can current board be eco'd?
;if not:
;    use config-prom-search (not likely),
; or recognize first-rev board and ask for slot, or hard-code default.

;check calls to spi-read-trace-ram-data to see if they want
;data at (adr) or data at (1- adr).

;conventions:
; at top level, all *expc* bits are set.

;test in particular:
; direction of shift-reg shift.
; shift-reg load-first, then shift.
; check all refs to trace-ram indices for 10 vs 12 bit masks.

;caveats:
; reading / writing trace ram data on ti spi increments trace ram address.
; lmi-debug trace ram address is also incremented at each trace ram data access.
;*** lmi-debug saves 4K pc's; ti saved only 1K.  10 vs. 12-bit ldb bugs?
;   some of regint-explorer masks #o1777 ...

;yes, this has nothing to do with the debug board
(defun undefine (f)
  (makunbound f)
  (fmakunbound f)
  (do ((l (plist f) (cddr l)))
      ((null l))
    (remprop f (car l))))

(defvar *lmi-debug-trace* nil)
(defvar *lmi-debug-lock* nil)

;current value in lmi-debug-explorer-control register
;all bits default to "1"
(defvar *expc*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (explorer-lmi-debug :after :init) (&rest ignored)
  (remote-setup-debug-board)
  (setq *expc* #xffffffff))

(defun error-if-trace-enabled ()
  (if (zerop (ldb expc-e-trace *expc*))
      (ferror nil "trace enabled when not expected")))

(defun error-if-running ()
  (if (= 1 (ldb expc-e-runen *expc*))
      (ferror nil "explorer running when not expected")))

;;;;;;;;;;;;;;;;

(defstruct (explorer-shift-reg)
  "control bits for explorer debug shift regs"
  (sr-nbits)
  (sr-s0)                                       ;'299 shift reg s0 and s1 control lines
  (sr-s1)
;;(sr-enable)                                   ;set to 0 to enable shift reg parallel outputs
  (sr-clock)                                    ;active high?
  (sr-data-from-exp)
  (sr-data-to-exp)
  (sr-inverted-p))                              ;if t, data-from and data-to are inverted

(defconst obus-shift-reg
          (make-explorer-shift-reg
            :sr-nbits 32.
            :sr-s0 expc-e-obs0
            :sr-s1 expc-e-obs1
            :sr-clock expc-e-obshftclk
            :sr-data-from-exp exps-e-obshft31
            :sr-data-to-exp expc-e-obshft0
            :sr-inverted-p t))

(defconst ir-shift-reg
          (make-explorer-shift-reg
            :sr-nbits 56.
            :sr-s0 expc-e-irs0
            :sr-s1 expc-e-irs1
            :sr-clock expc-e-irshftclk
            :sr-data-from-exp exps-e-irshft55
            :sr-data-to-exp expc-e-irshft0))

(defconst pc-shift-reg
          (make-explorer-shift-reg
            :sr-nbits 16.
            :sr-s0 expc-e-pcs0
            :sr-clock expc-e-pcshftclk
            :sr-data-from-exp exps-e-pcshft15
            :sr-data-to-exp expc-e-pcshfti))

; s0 s1
;  0  0  hold
;  0  1  shift left
;  1  0  shift right (A->H) (high bits pop out first?)
;  1  1  load
; s0 is wired high in the ti-spi

(defun load-explorer-shift-reg (data sr)
  (error-if-running)
  (if (sr-inverted-p sr)
      (setq data (lognot data)))
  (let ((*expc* *expc*))
    (setf (ldb (sr-s1 sr) *expc*) 0)            ;shift right (A->H), presumably A is bit 0
    (remote-write-explorer-control *expc*)
    (do ((i (1- (sr-nbits sr)) (1- i)))
        ((< i 0))
      (setf (ldb (sr-data-to-exp sr) *expc*) (ldb (byte 1 i) data))
      (remote-write-explorer-control
        (dpb 0 (sr-clock sr) *expc*))           ;clock low
      (remote-write-explorer-control *expc*))   ;clock high
      )
  (remote-write-explorer-control *expc*)        ;deassert s0, s1
  )

(defun read-explorer-shift-reg (sr &aux (data 0))
  (error-if-running)
  (if (null (sr-s1 sr))
      (ferror nil "this register can't be read -- no S1 control line"))

  ;clock once with s0=s1=1 to load shift reg with current data
  (remote-write-explorer-control
    (dpb 0 (sr-clock sr) *expc*))               ;clock low
  (remote-write-explorer-control *expc*)        ;clock high

  (let ((*expc* *expc*))
    (setf (ldb (sr-s1 sr) *expc*) 0)            ;shift right (A->H), presumably A is bit 0
    (remote-write-explorer-control *expc*)
    (do ((i (1- (sr-nbits sr)) (1- i)))
        ((< i 0))
      (let ((bit (ldb (sr-data-from-exp sr) (remote-read-explorer-status))))
        (setf (ldb (byte 1 i) data) bit)
        (setf (ldb (sr-data-to-exp sr) *expc*) bit))    ;write back so as to be non-destructive
      (remote-write-explorer-control
        (dpb 0 (sr-clock sr) *expc*))           ;clock low
      (remote-write-explorer-control *expc*))   ;clock high
    )
  (remote-write-explorer-control *expc*)
  (if (sr-inverted-p sr)
      (lognot data)
    data)
  )

;;;;;;;;;;;;;;;;

(defmethod (explorer-lmi-debug :spi-write-obus) (data)  ;W8
  "Write 32 bits of DATA to the explorer OBUS."
  (error-if-running)
  (load-explorer-shift-reg data obus-shift-reg)
  (let ((*expc* *expc*))
    (setf (ldb expc-e-obshften *expc*) 0)       ;assert obshften
    (remote-write-explorer-control *expc*)
    (remote-write-explorer-control              ;assert single-step
      (dpb 0 expc-e-stpreq *expc*))
    (remote-write-explorer-control *expc*))     ;deassert single-step
  (remote-write-explorer-control *expc*))       ;deassert obshften

;obus-shifter is apparently loaded from obus during every instruction
(defmethod (explorer-lmi-debug :spi-read-obus) ()       ;spi R8
  "Read OBUS.  Error if running."
  (error-if-running)
  (read-explorer-shift-reg obus-shift-reg))

;;;;

(defmethod (explorer-lmi-debug :spi-write-ir) (data)    ;spi W9
  "Write 56 bits to explorer IREG."
  (error-if-running)
  (load-explorer-shift-reg (compute-raven-cram-parity data) ir-shift-reg)
  (let ((*expc* *expc*))
    (setf (ldb expc-e-csramdis *expc*) 0)
    (setf (ldb expc-e-promen2 *expc*) 0)
    (remote-write-explorer-control *expc*)      ;assert csramdis and cspromdis
    (let ((*expc* *expc*))
      (setf (ldb expc-e-forcenop *expc*) 0)
      (setf (ldb expc-e-irshften *expc*) 0)
      (remote-write-explorer-control *expc*)    ;assert forcenop and irshften
      (remote-write-explorer-control            ;assert single-step
        (dpb 0 expc-e-stpreq *expc*))
      (remote-write-explorer-control *expc*))   ;deassert single-step
    (remote-write-explorer-control *expc*))     ;deassert forceno and irshften
  (remote-write-explorer-control *expc*)        ;deassert csramdis and cspromdis
  )

(defmethod (explorer-lmi-debug :spi-read-ir) () ;spi R9
  (error-if-running)
  (remote-write-explorer-control
    (dpb 0 expc-e-forcenop *expc*))             ;assert forcenop
  (prog1
    (read-explorer-shift-reg ir-shift-reg)
    (remote-write-explorer-control *expc*)      ;deassert forcenop
    ))

(defmethod (explorer-lmi-debug :spi-write-cram) (data)  ;spi WB
  "Write 56 bits into CRAM at location addressed by current PC."
  (error-if-running)
  (error-if-trace-enabled)
  (let ((*expc* *expc*))
    (setf (ldb expc-e-forcenop *expc*) 0)       ;assert forcenop
    (remote-write-explorer-control *expc*)
    (load-explorer-shift-reg (compute-raven-cram-parity data) ir-shift-reg)
    (let ((*expc* *expc*))
      (setf (ldb expc-e-csramdis *expc*) 0)     ;assert csramdis
      (setf (ldb expc-e-promen2 *expc*) 0)      ;assert promen2 (cspromdis)
      (remote-write-explorer-control *expc*)
      (let ((*expc* *expc*))
        (setf (ldb expc-e-irshften *expc*) 0)   ;assert irshften
        (remote-write-explorer-control *expc*)
        (let ((*expc* *expc*))
          (setf (ldb expc-e-xiramwe *expc*) 0)  ;assert xiramwe
          (remote-write-explorer-control *expc*)
          (remote-write-explorer-control
            (dpb 1 expc-e-csramdis *expc*))     ;deassert csramdis
          (remote-write-explorer-control *expc*)) ;assert csramdis
        (remote-write-explorer-control *expc*)) ;deassert xiramwe
      (remote-write-explorer-control *expc*))   ;deassert irshften
    (remote-write-explorer-control *expc*))     ;deassert csramdis and promen2
  (remote-write-explorer-control *expc*))       ;deassert forcenop

;;;;

(defmethod (explorer-lmi-debug :spi-write-pc) (adr)     ;spi WA
  "Write 14 bits to explorer PC."
  (error-if-running)
  (let ((*expc* *expc*))
    (setf (ldb expc-e-pcforce *expc*) 0)        ;assert pcforce
    (remote-write-explorer-control *expc*)
    (load-explorer-shift-reg adr pc-shift-reg)
    (setf (ldb expc-e-forcenop *expc*) 0)       ;assert forcenop
    (remote-write-explorer-control *expc*)
    (remote-write-explorer-control              ;assert single-step
      (dpb 0 expc-e-stpreq *expc*))
    (remote-write-explorer-control *expc*)      ;deassert single-step
    (setf (ldb expc-e-pcforce *expc*) 1)        ;deassert pcforce
    (remote-write-explorer-control *expc*)
    )
  (remote-write-explorer-control *expc*))       ;deassert forcenop

;ti-spi reads pc directly from explorer connector.
;lmi-debug can only read pc's that have been clocked into the trace ram.
;trace ram adr always points to slot after last pc that was clocked in.
;
;assumes new EXPPAL to allow read-straight-through
(defmethod (explorer-lmi-debug :spi-read-pc) () ;spi RA
  "Read 14 bits of PC."
  (error-if-running)
  (remote-write-explorer-control
    (dpb 0 expc-e-trace *expc*))                ;trace on, to enable read-straight-through
  (prog1
    (ldb expd-micro-pc (remote-read-explorer-pc-ram-data))
    (remote-write-explorer-control *expc*)))    ;trace off

;  (error-if-trace-enabled)
;  (error-if-ram-pointer-changed-since-trace-was-disabled)
;  (remote-write-explorer-pc-ram-pointer
;    (1- (remote-read-explorer-pc-ram-pointer)))        ;sloppy with extra bits
;  (ldb expd-micro-pc (remote-read-explorer-pc-ram-data)))

;;;;

(defmethod (explorer-lmi-debug :spi-write-trace-ram-adr) (adr)  ;spi W1
  (error-if-trace-enabled)
  (remote-write-explorer-pc-ram-pointer
    (ldb expp-pc-ram-ptr adr)))

(defmethod (explorer-lmi-debug :spi-read-trace-ram-adr) ()      ;spi R1
  (error-if-trace-enabled)
  (ldb expp-pc-ram-ptr (remote-read-explorer-pc-ram-pointer)))

;returns whole word including no-op and jump-taken.
;should this decrement the pointer at all?
(defmethod (explorer-lmi-debug :spi-read-trace-ram-data) ()     ;spi R2
  (error-if-trace-enabled)
  (remote-read-explorer-pc-ram-data))

;returns whole word including no-op and jump-taken.
(defmethod (explorer-lmi-debug :spi-read-nth-previous-pc) (nth) ;spi W6
  "Read the NTH PC back in the trace ram; restore the trace ram address."
  (error-if-trace-enabled)
  (let ((save-trace-adr (ldb expp-pc-ram-ptr (remote-read-explorer-pc-ram-pointer))))
    (remote-write-explorer-pc-ram-pointer
      (ldb expp-pc-ram-ptr (- save-trace-adr nth)))
    (prog1 (remote-read-explorer-pc-ram-data)
           (remote-write-explorer-pc-ram-pointer save-trace-adr))))

;;;;

(defmethod (explorer-lmi-debug :spi-write-md) (data)    ;spi WC
  "Write 32 bits into MD.  Error if running or TRACE mode."
  (error-if-running)
  (load-explorer-shift-reg data obus-shift-reg)
  (let ((*expc* *expc*))
    (setf (ldb expc-e-obshften *expc*) 0)       ;assert obshften
    (remote-write-explorer-control *expc*)
    (remote-write-explorer-control              ;assert mdld
      (dpb 0 expc-e-mdld *expc*))
    (remote-write-explorer-control *expc*))     ;deassert mdld
  (remote-write-explorer-control *expc*))       ;deassert obshften

;;;;;;;;;;;;;;;;


(defmethod (explorer-lmi-debug :spi-run) ()     ;spi S0 1
  (setf (ldb expc-e-runen *expc*) 1)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-stop) ()    ;spi S0 0
  (setf (ldb expc-e-runen *expc*) 0)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-single-step) ()     ;spi S1 0
  (remote-write-explorer-control
    (dpb 0 expc-e-stpreq *expc*))
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-release-halt) ()    ;spi S2 0
  (remote-write-explorer-control
    (dpb 0 expc-e-relhalt *expc*))
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-long-clock) ()      ;spi S3 0
  (setf (ldb expc-e-longclk *expc*) 0)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-normal-clock) ()    ;spi S3 1
  (setf (ldb expc-e-longclk *expc*) 1)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-force-noop) ()      ;spi S4 0
  (setf (ldb expc-e-forcenop *expc*) 0)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-dont-force-noop) () ;spi S4 1
  (setf (ldb expc-e-forcenop *expc*) 1)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-disable-cram-parity) ()     ;spi S5 0
  (setf (ldb expc-e-pardis *expc*) 0)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-enable-cram-parity) ()      ;spi S5 1
  (setf (ldb expc-e-pardis *expc*) 1)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-mdr-enable-mode) (n)        ;spi SA
  (setf (ldb expc-e-setmd *expc*) n)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-vma-enable-mode) (n)        ;spi SB
  (setf (ldb expc-e-setvma *expc*) n)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-force-pc) (n)       ;spi S8
  (setf (ldb expc-e-pcforce *expc*) n)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-reset) ()   ;spi SF 0
  (setf (ldb expc-e-reset *expc*) 0)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-enable) ()  ;spi SF 1
  (setf (ldb expc-e-reset *expc*) 1)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-trace-on) ()        ;spi C0 0
  (setf (ldb expc-e-trace *expc*) 0)
  (remote-write-explorer-control *expc*))

(defmethod (explorer-lmi-debug :spi-trace-off) ()       ;spi C0 1
  (setf (ldb expc-e-trace *expc*) 1)
  (remote-write-explorer-control *expc*))


(defmethod (explorer-lmi-debug :spi-enable-ir-shifter-l) (n)    ;spi S6
  (cond ((zerop n)
         (setf (ldb expc-e-csramdis *expc*) 0)  ;assert csramdis
         (setf (ldb expc-e-promen2 *expc*) 0)   ;assert cspromdis
         (remote-write-explorer-control *expc*)
         (setf (ldb expc-e-irshften *expc*) 0)  ;assert irshften
         (remote-write-explorer-control *expc*))
        (t
         (setf (ldb expc-e-irshften *expc*) 1)  ;deassert irshften
         (remote-write-explorer-control *expc*)
         (setf (ldb expc-e-csramdis *expc*) 1)  ;deassert csramdis
         (setf (ldb expc-e-promen2 *expc*) 1)   ;deassert cspromdis
         (remote-write-explorer-control *expc*))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; used in -full-restore etc.
; only seems to differ from :spi-write-ir in that this doesn't set FORCENOP
(defmethod (explorer-lmi-debug :spi-execute-then-write-ireg) (data)
;;(spi-write-ir-shifter-in-raven (compute-raven-cram-parity data))
  (load-explorer-shift-reg (compute-raven-cram-parity data) ir-shift-reg)
  (send self :spi-enable-ir-shifter-l 0)
  (send self :spi-single-step)
  (send self :spi-enable-ir-shifter-l 1)
  )

(defmethod (explorer-lmi-debug :spi-execute-ireg-at-full-speed) ()
  (load-explorer-shift-reg (raven-execute (return)
                             rav-ir-op rav-op-alu
                             rav-ir-halt 1)
                           ir-shift-reg)
;;(spi-write-ir-shifter-in-raven (raven-execute (return)
;;                                 rav-ir-op rav-op-alu
;;                                 rav-ir-halt 1))
  (send self :spi-enable-ir-shifter-l 0)
  (send self :spi-run)
  (process-sleep 1)
  (send self :spi-stop)
  (send self :spi-enable-ir-shifter-l 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (explorer-lmi-debug :spi-status-md-enable) ()
  (ldb exps-e-mdren (remote-read-explorer-status)))

(defmethod (explorer-lmi-debug :spi-status-vma-enable) ()
  (ldb exps-e-vmaen (remote-read-explorer-status)))

(defmethod (explorer-lmi-debug :spi-status-bnop) ()
  (remote-write-explorer-control
    (dpb 0 expc-e-trace *expc*))                ;trace on, to enable read-straight-through
  (prog1
    (ldb expd-bnop (remote-read-explorer-pc-ram-data))
    (remote-write-explorer-control *expc*)))    ;trace off

(defmethod (explorer-lmi-debug :spi-status-imod-hi) ()
  (ldb exps-e-imodhigh (remote-read-explorer-status)))

(defmethod (explorer-lmi-debug :spi-status-imod-low) ()
  (ldb exps-e-imodlow (remote-read-explorer-status)))

(defmethod (explorer-lmi-debug :spi-status-halted) ()
  (ldb exps-e-halted (remote-read-explorer-status)))

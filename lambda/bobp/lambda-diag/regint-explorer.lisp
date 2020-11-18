;;; -*- Mode:LISP; Package:LAMBDA; Base:8; Readtable:ZL -*-

;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.

; 4/86 bobp
; working copy for purpose of change spi- funcs to methods
; to support both the LMI debug board and explorer-spi / nu-debug.

; Assumes a remote lmi debug board is in the explorer being debugged.
; No provision now for explorer connected to local debug board.

;check calls to:
;  spi-read-pc              ;definitely return just PC bits
;  spi-read-trace-ram-data  ;definitely whole word
;  spi-read-nth-previous-pc ;not sure
;  spi-read-opc             ;not sure
;  saved-lpc                ;looks like 14 is correct, suggests read-nth should mask to 14?
;for proper bit-masks for 14 or 16 bit pc.

;should be moved to lam-regint, and replace lam-saved-explorer-md-and-vma-enable-modes
(defvar lam-saved-explorer-md-enable-mode 0)
(defvar lam-saved-explorer-vma-enable-mode 0)

;new flavors for lmi-debug:
;move to diag-defs later.
(defflavor nubus-via-lmi-debug ()
           (access-path))

(defflavor explorer-lmi-debug ()
           (access-path))

(defflavor explorer-via-remote-lmi-debug
           ()
           (nubus-via-lmi-debug
            explorer-lmi-debug
            regint-explorer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-*proc*-all-serial ()
  (setq *proc* (make-instance 'explorer-via-ti-serial
                              :proc-type :explorer
                              :memory-configuration-list
                                  `((4000 ,(ash #xf4000000 -10.))
                                    (4000 ,(ash #xf3000000 -10.))))))
(defun setup-*proc* ()
  (setq *proc* (make-instance 'explorer-via-ti-serial-with-nubus-from-burr-brown
                              :proc-type :explorer
                              :memory-configuration-list
                                  `((4000 ,(ash #xf4000000 -10.))
                                    (4000 ,(ash #xf3000000 -10.))))))

(defun setup-*proc*-lmi-debug ()
  (setq *proc* (make-instance 'explorer-via-remote-lmi-debug
                              :proc-type :explorer
                              :memory-configuration-list
                                  `((4000 ,(ash #xf4000000 -10.))
                                    (4000 ,(ash #xf3000000 -10.))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod (regint-explorer :reset) ()
  nil)

;also works to give adr with func-src indicator
(defmethod (regint-explorer :read-m-mem) (adr)
  (raven-execute (read)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src adr)
  (send self :spi-read-obus)
  )

(defmethod (regint-explorer :write-m-mem) (adr data)
  (send self :spi-write-md data)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-m-mem-dest adr))

(defmethod (regint-explorer :read-a-mem) (adr)
  (raven-execute (read)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-seta
    rav-ir-a-src adr)
  (send self :spi-read-obus))

(defmethod (regint-explorer :write-a-mem) (adr data)
  (send self :spi-write-md data)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-a-mem-dest-flag 1
    rav-ir-a-mem-dest adr))

(defmethod (regint-explorer :read-c-mem) (adr)
  (send self :write-pc-to-hardware adr)
  (send self :spi-read-ir)
  )

(defmethod (regint-explorer :write-c-mem) (adr data)
  (send self :write-pc-to-hardware adr)
  (send self :spi-write-cram data))

(defmethod (regint-explorer :read-d-mem) (adr)
  (send self :save-dc-and-q)
  (raven-execute (write)
    rav-ir-op rav-op-dispatch
    rav-ir-dispatch-addr adr
    rav-ir-read-dispatch-memory 1)
  (rav-read-func-src rav-m-src-q)
  )

(defvar lam-saved-q nil)
(defvar lam-saved-dc nil)

(defmethod (regint-explorer :save-dc-and-q) ()
  (if (null lam-saved-q)
      (setq lam-saved-q (rav-read-func-src rav-m-src-q)))
  (if (null lam-saved-dc)
      (setq lam-saved-dc (rav-read-func-src rav-m-src-disp-constant))))

(defmethod (regint-explorer :write-d-mem) (adr data)
  (send self :spi-write-md data)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-mem-dest 0
    rav-ir-m-src rav-m-src-md)
  (raven-execute (write)
    rav-ir-op rav-op-dispatch
    rav-ir-dispatch-addr adr
    rav-ir-write-dispatch-memory 1
    rav-ir-a-src 0
    )
  )

(defmethod (regint-explorer :read-q-reg) ()
  (send self :save-dc-and-q)
  lam-saved-q)

(defmethod (regint-explorer :read-q-reg-from-hardware) ()
  (rav-read-func-src rav-m-src-q))

(defmethod (regint-explorer :write-q-reg) (data)
  (send self :save-dc-and-q)
  (setq lam-saved-q data))

(defmethod (regint-explorer :write-q-reg-to-hardware) (data)
  (send self :spi-write-md data)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-q rav-q-load
    ))

(defmethod (regint-explorer :read-md) ()
  lam-saved-md)

(defmethod (regint-explorer :read-md-from-hardware) ()
  (rav-read-func-src rav-m-src-md))

(defmethod (regint-explorer :write-md) (data)
  (setq lam-saved-md data))

(defmethod (regint-explorer :write-md-to-hardware) (data)
  (send self :spi-write-md data))

(defmethod (regint-explorer :read-vma) ()
  lam-saved-vma)

(defmethod (regint-explorer :read-vma-from-hardware) ()
  (rav-read-func-src rav-m-src-vma))

(defmethod (regint-explorer :write-vma) (data)
  (setq lam-vma-changed-flag t)
  (setq lam-saved-vma data))

(defmethod (regint-explorer :write-vma-to-hardware) (data)
  (rav-write-func-dest rav-func-dest-vma data))

(defmethod (regint-explorer :read-pdl-buffer) (adr)
  (lam-save-pdl-buffer-index)
  (rav-write-func-dest rav-func-dest-pdl-buffer-index adr)
  (rav-read-func-src rav-m-src-c-pdl-buffer-index))

(defmethod (regint-explorer :write-pdl-buffer) (adr data)
  (if (null lam-saved-pdl-buffer-index)
      (lam-save-pdl-buffer-index))
  (rav-write-func-dest rav-func-dest-pdl-buffer-index adr)
  (rav-write-func-dest rav-func-dest-c-pdl-buffer-index data))

(defmethod (regint-explorer :read-pi) ()
  (or lam-saved-pdl-buffer-index
      (setq lam-saved-pdl-buffer-index (rav-read-func-src rav-m-src-pdl-buffer-index))))

(defmethod (regint-explorer :read-pi-from-hardware) ()
  (rav-read-func-src rav-m-src-pdl-buffer-index))

(defmethod (regint-explorer :write-pi) (data)
  (setq lam-saved-pdl-buffer-index data))

(defmethod (regint-explorer :write-pi-to-hardware) (data)
  (rav-write-func-dest rav-func-dest-pdl-buffer-index data))

(defmethod (regint-explorer :read-pp) ()
  (rav-read-func-src rav-m-src-pdl-buffer-pointer))

(defmethod (regint-explorer :write-pp) (data)
  (rav-write-func-dest rav-func-dest-pdl-buffer-pointer data))

(defmethod (regint-explorer :read-pc) ()
  lam-saved-pc)

;spi-read-pc may not be correct for lmi-debug explorer, see comment in EXPLORER-LMI-DEBUG
(defmethod (regint-explorer :read-pc-from-hardware) ()
  (send self :spi-read-pc))

(defmethod (regint-explorer :write-pc) (adr &optional (n-bit 1))
  (setq lam-saved-pc adr)
  (if (zerop n-bit)
      (setq lam-noop-flag nil)
    (setq lam-noop-flag t)))

(defmethod (regint-explorer :write-pc-to-hardware) (adr &optional (n-bit 1))
  (raven-execute (read)
    rav-ir-op rav-op-jump
    rav-ir-jump-cond rav-jump-cond-unc
    rav-ir-jump-addr adr
    rav-ir-n n-bit)
  (send self :spi-single-step)
  )

(defmethod (regint-explorer :read-l1-map) (adr)
  (cond ((and lam-saved-level-1-map-loc-0
              (zerop adr))
         lam-saved-level-1-map-loc-0)
        (t
         (send self :spi-write-md (dpb adr (byte 12. 13.) 0))
         (rav-read-func-src rav-m-src-l1-map))))

(defmethod (regint-explorer :save-l1-map-0) ()
  (unless lam-saved-level-1-map-loc-0
    (send self :spi-write-md 0)
    (setq lam-saved-level-1-map-loc-0 (rav-read-func-src rav-m-src-l1-map))))

(defmethod (regint-explorer :write-l1-map) (adr data)
  (lam-save-level-1-map-loc-0)
  (cond ((zerop adr)
         (setq lam-saved-level-1-map-loc-0 data))
        (t
         (rav-write-func-dest rav-func-dest-vma data)
         (send self :spi-write-md (dpb adr (byte 12. 13.) 0))
         (raven-execute (write)
           rav-ir-op rav-op-alu
           rav-ir-ob rav-ob-alu
           rav-ir-aluf rav-alu-setm
           rav-ir-m-src rav-m-src-vma
           rav-ir-func-dest rav-func-dest-vma-write-l1))))

(defmethod (regint-explorer :write-l1-map-to-hardware) (adr data)
  (rav-write-func-dest rav-func-dest-vma data)
  (send self :spi-write-md (dpb adr (byte 12. 13.) 0))
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l1))

(defmethod (regint-explorer :read-l2-map-control) (adr)
  (lam-save-level-1-map-loc-0)
  (rav-write-func-dest rav-func-dest-vma (ldb (byte 7 5) adr))
  (send self :spi-write-md 0)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l1)
  (send self :spi-write-md (dpb adr (byte 5 8) 0))
  (rav-read-func-src rav-m-src-l2-map-control))

(defmethod (regint-explorer :write-l2-map-control) (adr data)
  (lam-save-level-1-map-loc-0)
  (rav-write-func-dest rav-func-dest-vma (ldb (byte 7 5) adr))
  (send self :spi-write-md 0)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l1)
  (rav-write-func-dest rav-func-dest-vma data)
  (send self :spi-write-md (dpb adr (byte 5 8) 0))
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l2-map-control))

(defmethod (regint-explorer :read-l2-map-physical-page) (adr)
  (lam-save-level-1-map-loc-0)
  (rav-write-func-dest rav-func-dest-vma (ldb (byte 7 5) adr))
  (send self :spi-write-md 0)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l1)
  (send self :spi-write-md (dpb adr (byte 5 8) 0))
  (rav-read-func-src rav-m-src-l2-map-physical-page))



(defmethod (regint-explorer :write-l2-map-physical-page) (adr data)
  (lam-save-level-1-map-loc-0)
  (rav-write-func-dest rav-func-dest-vma (ldb (byte 7 5) adr))
  (send self :spi-write-md 0)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l1)
  (rav-write-func-dest rav-func-dest-vma data)
  (send self :spi-write-md (dpb adr (byte 5 8) 0))
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-write-l2-map-physical-page))


(defmethod (regint-explorer :read-usp) ()
  (or lam-saved-micro-stack-ptr
      (setq lam-saved-micro-stack-ptr
            (rav-read-func-src rav-m-src-micro-stack-pointer))))

(defmethod (regint-explorer :read-usp-from-hardware) ()
  (rav-read-func-src rav-m-src-micro-stack-pointer))

(defmethod (regint-explorer :write-usp) (data)
  (setq lam-saved-micro-stack-ptr data))

(defmethod (regint-explorer :write-usp-to-hardware) (data)
  (rav-write-func-dest rav-func-dest-micro-stack-pointer data))

(defmethod (regint-explorer :read-us) (adr)
  (LAM-SAVE-MICRO-STACK-PTR)
  (rav-write-func-dest rav-func-dest-micro-stack-pointer adr)
  (rav-read-func-src rav-m-src-micro-stack-data)
  )

(defmethod (regint-explorer :write-us) (adr data)
  (LAM-SAVE-MICRO-STACK-PTR)
  (rav-write-func-dest rav-func-dest-micro-stack-pointer adr)
  (rav-write-func-dest rav-func-dest-micro-stack-data data))

(defmethod (regint-explorer :read-lc) ()
  (rav-read-func-src rav-m-src-lc))

(defmethod (regint-explorer :write-lc) (data)
  (rav-write-func-dest rav-func-dest-lc data))

(defmethod (regint-explorer :read-macro-ir) ()
  (rav-read-func-src rav-m-src-macro.ir))

(defmethod (regint-explorer :write-macro-ir) (data)
  (rav-write-func-dest rav-func-dest-macro.ir data))

(defmethod (regint-explorer :read-mcr) ()
  (rav-read-func-src rav-m-src-mcr))

(defmethod (regint-explorer :write-mcr) (data)
  (rav-write-func-dest rav-func-dest-mcr data))

(defun rav-abort-and-parity-off ()
  (let ((old (send *proc* :read-mcr)))
    (send *proc* :write-mcr (dpb 0 (byte 1 13.)
                                 (dpb 0 (byte 1 12.)
                                      old))))
  )

(defun rav-nubus-reset ()
  (send *proc* :write-mcr (dpb 1 (byte 1 21.) (send *proc* :read-mcr)))
  (send *proc* :write-mcr (dpb 0 (byte 1 21.) (send *proc* :read-mcr))))


(defun print-mcr-data (data)
  (format t "~&~o" data)
  (format t "~&LED ~s" (ldb (byte 6 0) data))
  (format t "~&Self test passed ~s" (ldb (byte 1 6) data))
  (format t "~&Subsystem test passed ~s" (ldb (byte 1 7) data))
  (format t "~&Memory cycle enable ~s" (ldb (byte 1 8) data))
  (format t "~&Forced access request ~s" (ldb (byte 1 9) data))
  (format t "~&Bus lock ~s" (ldb (byte 1 10.) data))
  (format t "~&PROM disable ~s" (ldb (byte 1 11.) data))
  (format t "~&Halt on parity error ~s" (ldb (byte 1 12.) data))
  (format t "~&Abort on bus error ~s" (ldb (byte 1 13.) data))
  (format t "~&Sequence break request ~s" (ldb (byte 1 14.) data))
  (format t "~&Interrupt enable ~s" (ldb (byte 1 15.) data))
  (format t "~&Interrupt level ~s" (ldb (byte 4 16.) data))
  (format t "~&Power fail and warm boot enable ~s" (ldb (byte 1 20.) data))
  (format t "~&Nubus reset ~s" (ldb (byte 1 21.) data))
  (format t "~&Need fetch ~s" (ldb (byte 1 22.) data))
  (format t "~&Loop on self test ~s" (ldb (byte 1 23.) data))
  (format t "~&Enable MISC0 ~s" (ldb (byte 1 24.) data))
  (format t "~&Enable MISC1 ~s" (ldb (byte 1 25.) data))
  (format t "~&Macro-inst chaining enable ~s" (ldb (byte 1 26.) data))
  (format t "~&Self test busy ~s" (ldb (byte 1 27.) data))
  (format t "~&Nubus slot ~s" (ldb (byte 4 28.) data)))

(defun print-mcr ()
  (print-mcr-data (send *proc* :read-mcr)))

(defun initialize-mcr ()
  (send *proc* :write-mcr (+ (dpb 1 (byte 1 8) 0) ;memory cycle enable
                             (dpb 1 (byte 1 11.) 0) ;prom disable
                             )))


(defun check-md ()
  (send *proc* :write-md-to-hardware 123)
  (cond ((not (= (send *proc* :read-md-from-hardware) 123))
         (ferror nil "foo")))
  (send *proc* :write-md-to-hardware 321)
  (cond ((not (= (send *proc* :read-md-from-hardware) 321))
         (ferror nil "foo"))))

(defmethod (regint-explorer :read-stat-counter) ()
  0)

(defmethod (regint-explorer :write-stat-counter) (data)
  data)

(defmethod (regint-explorer :read-aux-stat-counter) ()
  0)

(defmethod (regint-explorer :write-aux-stat-counter) (data)
  data)

(defmethod (regint-explorer :read-dc) ()
  (send self :save-dc-and-q)
  lam-saved-dc)

(defmethod (regint-explorer :read-dc-from-hardware) ()
  (rav-read-func-src rav-m-src-q))

(defmethod (regint-explorer :write-dc) (data)
  (send self :save-dc-and-q)
  (setq lam-saved-dc data))

;clobbers Q
(defmethod (regint-explorer :write-dc-to-hardware) (data)
  (raven-execute (write)
    rav-ir-op rav-op-dispatch
    rav-ir-disp-const data
    rav-ir-read-dispatch-memory 1))

(defmethod (regint-explorer :read-mid) (adr)
  adr
  0)

(defmethod (regint-explorer :write-mid) (adr data)
  adr data)

(defmethod (regint-explorer :read-cam) (adr)
  adr
  0)

(defmethod (regint-explorer :write-cam) (adr data)
  adr data)

(defmethod (regint-explorer :read-ireg) ()
  lam-saved-ir)

(defmethod (regint-explorer :read-ireg-from-hardware) ()
  (send self :spi-read-ir))

(defmethod (regint-explorer :write-ireg) (data)
  (setq lam-saved-ir data))

(defmethod (regint-explorer :write-ireg-to-hardware) (data)
  (send self :spi-write-ir data))

(defmethod (regint-explorer :read-mfo) ()
  lam-saved-mfobus)

(defmethod (regint-explorer :read-nubus) (adr &optional check-for-bus-error)
  (rav-write-func-dest rav-func-dest-vma-start-unmapped-read adr)
  (cond ((null check-for-bus-error)
         (rav-read-func-src rav-m-src-md))
        (t
         (ferror nil "foo"))
        ))


;---


(defmethod (regint-explorer :stop-mach) ()
  (send self :spi-stop)
  (SETQ LAM-RUNNING 'uinst-STEP))


(defmethod (regint-explorer :start-mach) ()
  (lam-full-restore)            ;RESTORE MACHINE IF TRYING TO RUN
  (send self :spi-run)
  (SETQ LAM-RUNNING T))

(defmethod (regint-explorer :single-step) ()
  (send self :spi-single-step))

(defvar lam-last-inst-had-halt-bit nil)

;SAVE THINGS WHICH CAN BE SAVED WITHOUT MODIFYING THE STATE OF THE MACHINE
(defmethod (regint-explorer :passive-save) ()
  (cond ((not lam-passive-save-valid)
         (setq lam-saved-pdl-buffer-index nil)          ;FIRST OF ALL, CLEAR FLAGS
         (setq lam-saved-micro-stack-ptr nil)           ; WHICH MARK AUXILIARY PORTIONS
                                                        ; OF THE MACHINE NEED RESTORATION
         (setq lam-saved-level-1-map-loc-0 nil)
         (setq lam-vma-changed-flag nil)
         (setq lam-saved-opcs-valid nil)
         (setq lam-last-inst-had-halt-bit nil)
         (setq lam-saved-dc nil)
         (setq lam-saved-q nil)

         (setq lam-passive-save-valid t))))



(defvar lam-saved-lpc 0)

(defvar new-full-save t)

(defmethod (regint-explorer :full-save) (&aux (imod 0) trace-adr)
  (cond ((not lam-full-save-valid)
         (lam-stop-mach)
         (lam-passive-save)

         (if (not (zerop (send self :spi-status-imod-low)))
             (setq imod (send self :spi-read-obus)))
         (if (not (zerop (send self :spi-status-imod-hi)))
             (setq imod (ash (send self :spi-read-obus) 32.)))

         (cond ((zerop (send self :spi-status-halted))  ;halt FF
                (send self :spi-trace-off)
                (setq lam-noop-flag (zerop (send self :spi-status-noop)))
                (setq lam-saved-ir nil)
                (send self :spi-write-ir 0)
                (setq lam-saved-lpc (send self :spi-read-nth-previous-pc 2))
                (setq lam-saved-pc (ldb (byte 14. 0) (send self :spi-read-nth-previous-pc 1)))
                (send self :spi-write-trace-ram-adr (1- (send self :spi-read-trace-ram-adr)))
                (setq lam-last-inst-had-halt-bit t)
                )
               ((null new-full-save)
                (setq lam-saved-lpc (send self :spi-read-pc))
                (setq lam-saved-ir (logior imod (send self :spi-read-ir)))

                (send self :spi-trace-off)
                (send self :spi-single-step) ;execute inst in real IREG

                (setq lam-saved-pc (send self :spi-read-pc))

                (setq lam-noop-flag (zerop (send self :spi-status-noop)))

                (send self :spi-force-noop)
                (send self :spi-single-step)

                (send self :spi-force-noop)
                (send self :spi-single-step)
                )
               (t
                (setq lam-saved-lpc (send self :spi-read-pc))
                (setq lam-saved-ir (logior imod (send self :spi-read-ir)))
                (send self :spi-trace-off) ;have to turn off before reading adr
                (setq trace-adr (send self :spi-read-trace-ram-adr))
                (send self :spi-trace-on)
                (send self :spi-execute-ireg-at-full-speed)
                (send self :spi-trace-off)
                (send self :spi-write-trace-ram-adr trace-adr)
                (let ((trace-data (send self :spi-read-trace-ram-data)))
                  (setq lam-saved-pc (ldb (byte 14. 0) trace-data))
                  (setq lam-noop-flag  (not (ldb-test (byte 1 15.) trace-data))))
                (send self :spi-write-trace-ram-adr trace-adr)

                )
               )


         (lam-save-mem-status)

         (cond ((null lam-saved-ir)
                (setq lam-saved-ir (send self :read-c-mem lam-saved-lpc))))

         (setq memory-configuration-list nil)

         (when (= (ldb (byte 24. 0) (send *proc* :read-a-mem #o100))
                  (send lam-file-symbols-loaded-from :version))
           (setq memory-configuration-list (get-explorer-memory-from-a-mem)))

         (when (null memory-configuration-list)
           (setq memory-configuration-list (list (list #o4000
                                                       (ash #xf4000000 -10.)))))

         (setq lam-full-save-valid t))))

(defun get-explorer-memory-from-a-mem (&aux result)
  (do ((i 0 (1+ i)))
      (())
    (let ((a-pmo (lam-lookup-name (intern (format nil "A-PMO-~d" i) "LAM")))
          (a-pmh (lam-lookup-name (intern (format nil "A-PMH-~d" i) "LAM"))))
      (cond ((null a-pmo)
             (return))
            (t
             (setq a-pmo (- a-pmo raamo))
             (setq a-pmh (- a-pmh raamo))
             (let ((offset (send *proc* :read-a-mem a-pmo))
                   (size (send *proc* :read-a-mem a-pmh)))
               (cond ((zerop size)
                      (return))
                     (t
                      (push (list size offset) result))))))))

  (reverse result))



(defmethod (regint-explorer :full-restore) ()
  (cond (lam-full-save-valid
         (if lam-saved-micro-stack-ptr
             (lam-restore-micro-stack-ptr))
         (if lam-saved-pdl-buffer-index
             (send self :write-pi-to-hardware lam-saved-pdl-buffer-index))
         (setq lam-saved-pdl-buffer-index nil)
         (if lam-saved-dc
             (send self :write-dc-to-hardware lam-saved-dc))
         (setq lam-saved-dc nil)
         (if lam-saved-q
             (send self :write-q-to-hardware lam-saved-q))
         (setq lam-saved-q nil)
         (lam-restore-mem-status)
         (setq lam-full-save-valid nil)))
  (cond (lam-passive-save-valid
; execute exactly:
;  (jump-xct-next lam-saved-lpc)
;  (jump-xct-next lam-saved-pc) ;or just JUMP if lam-noop-flag
;  (lam-saved-ir)

         (raven-execute (read)
           rav-ir-op rav-op-jump
           rav-ir-jump-cond rav-jump-cond-unc
           rav-ir-n 0
           rav-ir-jump-addr lam-saved-lpc)
         (send self :spi-execute-then-write-ireg
           (raven-execute (return)
             rav-ir-op rav-op-jump
             rav-ir-jump-cond rav-jump-cond-unc
             rav-ir-n (if lam-noop-flag 1 0)
             rav-ir-jump-addr lam-saved-pc))
  (send self :spi-trace-on)
         (send self :spi-execute-then-write-ireg lam-saved-ir)
         ))
  (setq lam-passive-save-valid nil)
  )


(defmethod (regint-explorer :save-opcs) (&optional count)
  count
  nil)

(defmethod (regint-explorer :save-mem-status) ()
  (setq lam-saved-explorer-md-enable-mode (send self :spi-status-md-enable))
  (setq lam-saved-explorer-vma-enable-mode (send self :spi-status-vma-enable))
  (SETQ LAM-SAVED-VMA (send self :read-vma-from-hardware))
  (SETQ LAM-SAVED-MD (send self :read-md-from-hardware))

  (send self :spi-mdr-enable-mode 1)
  (send self :spi-vma-enable-mode 1)

;this will reset the MDR-ENABLE bit
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-func-dest rav-func-dest-md)
  (rav-abort-and-parity-off)
  )

;hack page fault bits!
(defmethod (regint-explorer :RESTORE-MEM-STATUS) ()
  (IF LAM-SAVED-LEVEL-1-MAP-LOC-0
      (send self :WRITE-L1-MAP-to-hardware 0 LAM-SAVED-LEVEL-1-MAP-LOC-0))
  (SETQ LAM-SAVED-LEVEL-1-MAP-LOC-0 NIL)
  (send self :WRITE-VMA-to-hardware LAM-SAVED-VMA)
  (send self :spi-WRITE-MD LAM-SAVED-MD)
;  ;don't set this if user changes MD
;  (cond ((= 1 lam-saved-explorer-md-enable-mode)
;        (send self :spi-mdr-enable-mode 0)
;        (send self :spi-mdr-enable-mode 1)))
;  ;don't set this if user changes LC
;  (cond ((= 1 lam-saved-explorer-vma-enable-mode)
;        (send self :spi-vma-enable-mode 0)
;        (send self :spi-vma-enable-mode 1)))
  )


(defmethod (regint-explorer :read-opc) (adr)
  (send self :spi-read-nth-previous-pc (1+ adr)))       ;let method mask to appropriate # of bits

;taken care of by lam-full-save
(defmethod (regint-explorer :release-halt) ()
  nil)



(defun rav-print-l1-map-data (data)
  (format t "~&Index ~s" (ldb (byte 6 0) data))
  (format t "~&GC ~s" (ldb (byte 3 7) data))
  (format t "~&OLD ~s" (ldb (byte 1 10.) data))
  (format t "~&Valid ~s" (ldb (byte 1 11.) data))
  (format t "~&----")
  (format t "~&Access fault ~s" (ldb (byte 1 12.) data))
  (format t "~&Write fault ~s" (ldb (byte 1 13.) data))
  (format t "~&Not Forced cycle ~s" (ldb (byte 1 14.) data))
  (format t "~&Unmapped ~s" (ldb (byte 1 15.) data))
  (format t "~&Rest ~s" (ash data -16.)))

(defun rav-print-l2-map-control-data (data)
  (format t "~&~s" data)
  (format t "~&Meta ~s" (ldb (byte 5 0) data))
  (format t "~&Map status ~s" (ldb (byte 2 6) data))
  (format t "~&Access ~s" (ldb (byte 2 8) data))
  (format t "~&Force ~s" (ldb (byte 1 10.) data))
  (format t "~&Volatility ~s" (ldb (byte 2 11.) data))
  (format t "~&Last locked ~s" (ldb (byte 1 13.) data))
  (format t "~&Last TM1 & TM0 ~s" (ldb (byte 2 14.) data))
  (format t "~&Rest ~s" (ash data -16.)))

;--

(defun e-test ()
  (dotimes (i 30)
    (send *proc* :write-c-mem i 0))
  (uload ()
    0
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 0
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 1
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 2
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 3
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 4
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 5
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 6
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 7
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 10
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 11
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 12
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 13
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 14
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 15
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 16
          )
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr 0
          rav-ir-n 1
          )
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-m-mem-dest 20
          )

         )
  )

(defun seq-m-mem ()
  (dotimes (i 20)
    (send *proc* :write-m-mem i (+ 1000 i))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;From here to the matching comment about 6 pages down
;used only in the serial-only ti-spi setup.

;these are defined for the serial-only ti-spi setup, so that
;nubus memory access can be done at a reasonable speed.

(defun vread (adr)
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setz
    rav-ir-func-dest rav-func-dest-md)
  (send self :spi-write-md adr)
  (raven-execute (read)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-func-dest rav-func-dest-vma-start-read)
  (send self :spi-execute-then-write-ireg
    (raven-execute (return)
      rav-ir-op rav-op-alu))
  (send self :spi-execute-then-write-ireg
    (raven-execute (return)
      rav-ir-op rav-op-alu))
  (send self :spi-execute-then-write-ireg
    (raven-execute (return)
      rav-ir-op rav-op-alu
      rav-ir-ob rav-ob-alu
      rav-ir-aluf rav-alu-setm
      rav-ir-m-src rav-m-src-md
      rav-ir-m-mem-dest 2))
  (send self :spi-execute-then-write-ireg 0))

(defun vtest ()
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-func-dest rav-func-dest-md)
  (rav-write-func-dest rav-func-dest-vma 0)
  (if (not (= (rav-read-func-src rav-m-src-vma) 0))
      (ferror nil "foo"))
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-vma
    rav-ir-func-dest rav-func-dest-vma-start-read)
  )

(defun vtest3 ()
  (send self :spi-write-ir 0)

  (send self :spi-force-noop)
  (send self :spi-single-step)

  (send self :spi-force-noop)
  (send self :spi-single-step)

  (setq lam-saved-explorer-md-enable-mode (send self :spi-status-md-enable))
  (setq lam-saved-explorer-vma-enable-mode (send self :spi-status-vma-enable))

  (SETQ LAM-SAVED-VMA (send *proc* :read-vma-from-hardware))
  (SETQ LAM-SAVED-MD (send *proc* :read-md-from-hardware))

;this will reset the MDR-ENABLE bit
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-func-dest rav-func-dest-md)
  )

(defun vtest2 ()
  (raven-execute (write)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md
    rav-ir-func-dest rav-func-dest-md)
  (raven-execute (read)
    rav-ir-op rav-op-alu
    rav-ir-ob rav-ob-alu
    rav-ir-aluf rav-alu-setm
    rav-ir-m-src rav-m-src-md)
  (let ((val (send self :spi-read-obus)))
    (values val (nth (ldb (byte 5 25.) val) q-data-types)))
  )

(defun rtest ()
  (uload ()
    0
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setz
          rav-ir-func-dest rav-func-dest-vma-start-read)
         (rav-ir-op rav-op-alu)
         (rav-ir-op rav-op-alu
          rav-ir-halt 1)))

(defun rav-vread (adr &aux data)
  (rav-fast-write-vma adr)
  (send *proc* :write-pc-to-hardware 37700)
  (send self :spi-run)
  (process-sleep 2)
  (send self :spi-stop)

  (let ((stop-pc (send self :spi-read-pc)))
    (send self :spi-write-ir 0)
    (send self :spi-force-noop)
    (send self :spi-single-step)
    (send self :spi-force-noop)
    (send self :spi-single-step)

    (selectq stop-pc
      (37712
       (setq data (send *proc* :read-md-from-hardware))

       data)
      (37713
       (ferror nil "error page fault"))
      (t
       (ferror nil "unknown halt")))))

(defun rav-vwrite (adr data)
  (rav-fast-write-vma adr)
  (send *proc* :write-pc-to-hardware 37701)
  (send self :spi-write-md data)
  (send self :spi-run)
  (process-sleep 2)
  (send self :spi-stop)

  (let ((stop-pc (send self :spi-read-pc)))
    (send self :spi-write-ir 0)
    (send self :spi-force-noop)
    (send self :spi-single-step)
    (send self :spi-force-noop)
    (send self :spi-single-step)

    (selectq stop-pc
      (37712
       nil)
      (37713
       (ferror nil "error page fault"))
      (t
       (ferror nil "unknown halt")))))

(defun rav-phys-read (adr)
  (rav-fast-write-vma adr)
  (send *proc* :write-pc-to-hardware 37702)
  (send self :spi-run)
  (process-sleep 2)
  (send self :spi-stop)

  (let ((stop-pc (send self :spi-read-pc)))
    (selectq stop-pc
      (37712
       (send *proc* :read-md-from-hardware))
      (37714
;       (cerror :no-action nil nil "bus error")
       (send *proc* :read-md-from-hardware))
      (t
       (ferror nil "unknown halt ~s" stop-pc)))))

(defun rav-phys-write (adr data)
  (rav-fast-write-vma adr)
  (send *proc* :write-pc-to-hardware 37703)
  (send self :spi-write-md data)
  (send self :spi-run)
  (process-sleep 2)
  (send self :spi-stop)

  (let ((stop-pc (send self :spi-read-pc)))
    (selectq stop-pc
      (37712
       nil)
      (37714
;       (cerror :no-action nil nil "bus error")
       )
      (t
       (ferror nil "unknown halt")))))

(defun rav-setup-memory-instructions ()
  (uload ()
    37700
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr virtual-read
          rav-ir-n 1)
    37701
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr virtual-write
          rav-ir-n 1)
    37702
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr physical-read
          rav-ir-n 1)
    37703
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr physical-write
          rav-ir-n 1)
    37704
         (rav-ir-op rav-op-alu) ;physical-byte-read
    37705
         (rav-ir-op rav-op-alu) ;physical-byte-write
    37706
         (rav-ir-op rav-op-alu) ;physical-halfword-read
    37707
         (rav-ir-op rav-op-alu) ;physical-halfword-write
    37710
   memory-cycle-ok
         (rav-ir-op rav-op-alu
          rav-ir-halt 1)
    37711
   error-page-fault
         (rav-ir-op rav-op-alu
          rav-ir-halt 1)
    37712
   nubus-error
         (rav-ir-op rav-op-alu
          rav-ir-halt 1)

         ;;((VMA-START-READ) VMA)
   virtual-read
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-vma
          rav-ir-func-dest rav-func-dest-vma-start-read)
         (rav-ir-op rav-op-alu)
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-md
          rav-ir-func-dest rav-func-dest-md)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-page-fault
          rav-ir-jump-addr error-page-fault
          rav-ir-n 1)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr memory-cycle-ok
          rav-ir-n 1)
         ;;((VMA-START-WRITE) VMA)
   virtual-write
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-vma
          rav-ir-func-dest rav-func-dest-vma-start-write)
         (rav-ir-op rav-op-alu)
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-md
          rav-ir-func-dest rav-func-dest-md)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-page-fault
          rav-ir-jump-addr error-page-fault
          rav-ir-n 1)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr memory-cycle-ok
          rav-ir-n 1)
         ;;((VMA-START-UNMAPPED-READ) VMA)
   physical-read
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-vma
          rav-ir-func-dest rav-func-dest-vma-start-unmapped-read)
         (rav-ir-op rav-op-alu)
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-md
          rav-ir-func-dest rav-func-dest-md)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-nubus-error
          rav-ir-jump-addr nubus-error
          rav-ir-n 1)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr memory-cycle-ok
          rav-ir-n 1)
         ;;((VMA-START-UNMAPPED-WRITE) VMA)
   physical-write
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-vma
          rav-ir-func-dest rav-func-dest-vma-start-unmapped-write)
         (rav-ir-op rav-op-alu)
         (rav-ir-op rav-op-alu
          rav-ir-ob rav-ob-alu
          rav-ir-aluf rav-alu-setm
          rav-ir-m-src rav-m-src-md
          rav-ir-func-dest rav-func-dest-md)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-nubus-error
          rav-ir-jump-addr nubus-error
          rav-ir-n 1)
         (rav-ir-op rav-op-jump
          rav-ir-jump-cond rav-jump-cond-unc
          rav-ir-jump-addr memory-cycle-ok
          rav-ir-n 1)
         )
  )

;ibufs are "instruction buffers" in the ti-spi box; they hold
;frequently used instructions that are set up and then loaded
;into the explorer processor by a command that is faster than
;sending over the serial line again.  Not used for lmi-debug.
;  0  ((vma) md)  ;only ibuf used now?
(defun setup-ibufs ()
  (send self :spi-load-ibuf 0 (raven-execute (return)
                     rav-ir-op rav-op-alu
                     rav-ir-ob rav-ob-alu
                     rav-ir-aluf rav-alu-setm
                     rav-ir-m-src rav-m-src-md
                     rav-ir-func-dest rav-func-dest-vma))
  )

(defun rav-fast-write-vma (data)
  (send self :spi-write-md data)
  (send self :spi-load-ir-from-ibuf 0)
  (send self :spi-single-step)
  (send self :spi-force-noop)
  )

(defun explorer-spi-phys-read (adr &optional ignore-bus-errors byte-mode)
  ignore-bus-errors byte-mode
  (rav-phys-read adr))

(defun explorer-spi-phys-write (adr data &optional ignore-bus-errors byte-mode)
  ignore-bus-errors byte-mode
  (rav-phys-write adr data))

(defun spi-reload ()
  (setup-ibufs)
  (rav-setup-memory-instructions))

;end of fast memory-ref stuff for serial-only ti-spi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

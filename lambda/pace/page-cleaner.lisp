;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

(defvar *page-cleaner-ppd-index*)

(defun clean-pages ()
  (let ((ppd-base (%region-origin physical-page-data))
        (ppd-index *page-cleaner-ppd-index*)
        (pht-base (%region-origin page-table-area))
        (phys-pages (floor (aref #'sys:system-communication-area sys:%sys-com-memory-size) 256.))
        pht-adr
        (disk-page-reads-loc '%count-disk-page-reads)
        (disk-page-writes-loc '%count-disk-page-writes)
        disk-page-reads
        disk-page-writes
        (ccwp %DISK-RQ-CCW-LIST)
        (ccwstart %disk-rq-ccw-list)
        ;;(ccwend (* page-rqb-size 2))
        (ccwend (+ %disk-rq-ccw-list 2))
        vpage-start
        (n-pages 0)
        (local-page-offset page-offset)
        (region-bits-origin (%region-origin region-bits))
        )
    (coerce-to-meter-location disk-page-reads-loc)
    (coerce-to-meter-location disk-page-writes-loc)
    (setq disk-page-reads (%p-ldb (byte 16. 0) disk-page-reads-loc))
    (setq disk-page-writes (%p-ldb (byte 16. 0) disk-page-writes-loc))
    ;;find next dirty page, but only if it is one of the next N pages
    (do ((i 0 (1+ i)))
        ((= i 5)
         (setq *page-cleaner-ppd-index* ppd-index)
         (return-from clean-pages nil))
      (let ((pht-index (%p-ldb (byte 16. 0) (+ ppd-base ppd-index))))
        (when (not (= pht-index #o177777))
          (setq pht-adr (+ pht-index pht-base))
          (when (and (= (%p-ldb #.%%pht1-valid-bit pht-adr) 1)
                     (not (= (%p-ldb #.%%pht1-swap-status-code pht-adr) #.%pht-swap-status-wired))
                     (or (= (%p-ldb #.%%pht1-modified-bit pht-adr) 1)
                         (= (%p-ldb #.%%pht2-map-status-code (1+ pht-adr)) #.%pht-map-status-read-write)))
            (return nil))))
      (incf ppd-index)
      (when (= ppd-index phys-pages)
        (setq ppd-index 0)))
    ;;get here with PHT-ADR pointing to the PHT entry for first dirty page
    ;;we want to write out this, and any following dirty virtual pages
    (wire-page-rqb)
    (setq vpage-start (%p-ldb #.%%pht1-virtual-page-number pht-adr))
    (do ((vpage vpage-start (+ vpage 1))
         (vadr (lsh vpage-start 8) (%make-pointer-offset #.dtp-fix vadr #o400)))
        (())
      (let ((page-status (%page-status vadr))
            (logical-adr (%physical-address vadr)))
        (when (or (null page-status)
                  (not (= 1 (ldb #.%%pht1-modified-bit page-status)))
                  (null logical-adr))
          (return nil))
        (incf n-pages)
        (setf (aref page-rqb ccwp) (+ logical-adr 1))
        (setf (aref page-rqb (+ ccwp 1)) (lsh logical-adr -16.))
        (incf ccwp 2)
        (when (= ccwp ccwend)
          (return nil))
        ))

    (when (= ccwp ccwstart)
      (ferror nil "page got unmodified!!"))

    (setf (ldb (byte 1 0) (aref page-rqb (- ccwp 2))) 0)

    (when (or (not (= disk-page-reads (%p-ldb (byte 16. 0) disk-page-reads-loc)))
              (not (= disk-page-writes (%p-ldb (byte 16. 0) disk-page-writes-loc))))
      (ferror nil "did some paging at a bad time"))

    (disk-write-wired page-rqb 0 (+ vpage-start local-page-offset))

    (when (or (not (= disk-page-reads (%p-ldb (byte 16. 0) disk-page-reads-loc)))
              (not (= disk-page-writes (%p-ldb (byte 16. 0) disk-page-writes-loc))))
      (ferror nil "did some paging at a terrible time"))

    (do ((vadr (lsh page-start 8) (%make-pointer-offset #.dtp-fix vadr #o400))
         (i 0 (1+ i)))
        ((= i n-pages))
      (let ((page-status (%page-status vadr)))
        (when (null page-status)
          (ferror nil "page slipped away!!"))
        (let ((swap-status (ldb #.%%pht1-swap-status-code page-status)))
          (%change-page-status vadr
                               (+ 1_23. swap-status)
                               (%p-ldb #.%%region-map-bits (+ region-bits-origin
                                                              (%region-number vadr)))))))))

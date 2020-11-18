;;; -*- Mode:LISP; Package:MAP-FAULT; Base:10; Readtable:CL -*-

(export '(
          call-while-allowing-write-in-read-only
          write-fault-handler
          read-fault-handler
          )
        )

(defconstant *page-fault-code-idle*        0)
(defconstant *page-fault-code-write-fault* 1)
(defconstant *page-fault-code-read-fault*  2)
(defconstant *page-fault-code-icache-fault* 3)

(defun read-fault-handler (vma vma-boxed md md-boxed map-bits transp)
;  (trap::tail-illop "Entered read-fault-handler")
;  (when (not (= gr::*page-fault-code* *page-fault-code-idle*))
;    (trap::tail-illop "Recursively entered read page fault handler."))
  (setq gr::*page-fault-code* *page-fault-code-read-fault*)
  ;; Don't need to know if they were off because we are called only from
  ;; the trap handler (so far).
  (trap::trap-on)
  (dispatch (byte 4. 0.) (extract-map-status map-bits)
    ($$map-status-read-mar       (read-read-mar      vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-read-mar-aged  (read-aged          vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-read-only      (read-read-only     vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-read-only-aged (read-aged          vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-swapped-out    (read-swapped-out   vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-direct-mapped  (read-direct-mapped vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-normal-aged    (read-aged          vma vma-boxed md md-boxed map-bits transp))
    ($$map-status-normal         (read-normal        vma vma-boxed md md-boxed map-bits transp))
    (t                           (read-illegal       vma vma-boxed md md-boxed map-bits transp))))

(defun read-illegal (vma vma-boxed md md-boxed map-bits transport-type)
  (trap::tail-illop "Read-illegal called."))

(defun read-read-mar (vma vma-boxed md md-boxed map-bits transport-type)
  (trap::tail-illop "Read-read-mar called."))

(defun read-aged (vma vma-boxed md md-boxed map-bits transport-type)
  (hw:trap-off)
  (hw:write-vma vma)
  (map:touch-aged map-bits)
;  (pcd:age-cluster (map-on-board-address map-bits) pcd:$$status-normal)
  (pcd:rejuvenate-cluster (map-on-board-address map-bits))
  (setq gr::*page-fault-code* *page-fault-code-idle*)
  (vma-start-read-generic vma-boxed md-boxed 0 transport-type vma)
  )

(defun read-read-only (vma vma-boxed md md-boxed map-bits transport-type)
  (trap::tail-illop "Read-read-only called."))

(defun read-swapped-out (vma vma-boxed md md-boxed map-bits transport-type)
  ;(trap::tail-illop "Got to read-swapped-out")
  (swap-in (cluster-number vma))
  (hw:trap-off)
  (setq gr::*page-fault-code* *page-fault-code-idle*)
  (vma-start-read-generic vma-boxed md-boxed 0 transport-type vma))

(defun read-direct-mapped (vma vma-boxed md md-boxed map-bits transport-type)
  (trap::tail-illop "Read-direct-mapped called."))

(defun read-normal (vma vma-boxed md md-boxed map-bits transport-type)
  (trap::tail-illop "Read-normal called."))


(defun write-fault-handler (vma vma-boxed md md-boxed map-bits gc-trap)
;  (trap::tail-illop "Entered write-fault-handler")
;  (when (not (= gr::*page-fault-code* *page-fault-code-idle*))
;    (trap::tail-illop "Recursively entered write page fault handler."))
  (when (= (cluster-number vma) 4)
    (trap::tail-illop "Got the stray writing of cluster 4."))
  (setq gr::*page-fault-code* *page-fault-code-write-fault*)
  ;; Don't need to know if they were off because we are called only from
  ;; the trap handler (so far).
  (trap::trap-on)
  (dispatch (byte 4. 0.) (extract-map-status map-bits)
    ($$map-status-read-mar       (write-read-mar      vma vma-boxed md md-boxed map-bits gc-trap))
    #+Removed
    ($$map-status-read-mar-aged  (write-aged          vma vma-boxed md md-boxed map-bits gc-trap))
    ($$map-status-read-only      (write-read-only     vma vma-boxed md md-boxed map-bits gc-trap))
    #+Removed
    ($$map-status-read-only-aged (write-aged          vma vma-boxed md md-boxed map-bits gc-trap))
    ($$map-status-swapped-out    (write-swapped-out   vma vma-boxed md md-boxed map-bits gc-trap))
    ($$map-status-direct-mapped  (write-direct-mapped vma vma-boxed md md-boxed map-bits gc-trap))
    #+Removed
    ($$map-status-normal-aged    (write-aged          vma vma-boxed md md-boxed map-bits gc-trap))
    ($$map-status-normal         (write-normal        vma vma-boxed md md-boxed map-bits gc-trap))
    (t                           (write-illegal       vma vma-boxed md md-boxed map-bits gc-trap))))

(defun write-illegal (vma vma-boxed md md-boxed map-bits gc-trap)
  (trap::tail-illop "Write-illegal called."))

(defun write-read-mar (vma vma-boxed md md-boxed map-bits gc-trap)
  (trap::tail-illop "Write-read-mar called."))

;;; $$$ Removed following function since not currently called <17-Nov-88 WKF>
;;; @@@ This function does not currently ever get called. <17-Nov-88 wkf>
#+Removed
 (defun write-aged (vma vma-boxed md md-boxed map-bits gc-trap)
  (hw:trap-off)
  (hw:write-vma vma)
  (map:touch-aged map-bits)
;  (pcd:age-cluster (map-on-board-address map-bits) pcd:$$status-normal)
  (pcd:rejuvenate-cluster (map-on-board-address map-bits))
  (setq gr::*page-fault-code* *page-fault-code-idle*)
  (write-vma-generic vma vma-boxed)
  (md-start-write-generic md md-boxed gc-trap)  ; $$$ Changed gc-write-test to gc-trap. <17-Nov-88 wkf>
  )

(defun write-direct-mapped (vma vma-boxed md md-boxed map-bits gc-trap)
  (trap::tail-illop "Write-direct-mapped called."))

(defun write-normal (vma vma-boxed md md-boxed map-bits gc-trap)
  (trap::tail-illop "Write-normal called."))

(defun write-read-only (vma vma-boxed md md-boxed map-bits gc-trap)
  (if t         ;(map-local-memory? map-bits)   ** bombs in nubus-memory-mode  -rg 11/06/88
      (let* ((physical-cluster (map-on-board-address map-bits))
             (pcd-bits         (read-pcd physical-cluster)))
        (dispatch %%pcd-write-bits pcd-bits
          ($$write-normal              (trap::tail-illop "Huh? PCD is broken."));; Code region???
          ($$write-mar                 (trap::tail-illop "Write mar."))
          ($$write-read-only           (write-really-read-only vma vma-boxed md md-boxed map-bits gc-trap
                                                               physical-cluster))
          ($$write-read-only-mar       (trap::tail-illop "Write read-only mar."))
          ($$write-clean
            (write-clean-cluster vma vma-boxed md md-boxed map-bits gc-trap physical-cluster))
          ($$write-clean-mar           (trap::tail-illop "Write clean mar."))
          ($$write-clean-read-only     (write-really-read-only-clean
                                         vma vma-boxed md md-boxed map-bits gc-trap physical-cluster))
          ($$write-clean-read-only-mar (trap::tail-illop "Write clean read-only mar."))))
      (trap::tail-illop "Write to read-only NUBUS space!")))

(defun write-clean-cluster (vma vma-boxed md md-boxed map-bits gc-write-test physical-cluster)
  ;; This can take a GC trap as it exits the trap handler.
  ;; Ok to forget if it was off, we are leaving the trap handler.
  (hw:trap-off)
;  (trap::tail-illop "Writing clean cluster.")
  (setq gr::*page-fault-code* *page-fault-code-idle*)
  (mark-modified-in-pcd physical-cluster)
  (write-vma-generic vma vma-boxed)     ;**winds up with three insts for pipe time and map set up.
  (hw:write-map (inject-map-status map-bits $$map-status-normal))
  (md-start-write-generic md md-boxed gc-write-test))   ;** this fortunately does not cause an MMFIO collision.

(defsubst call-while-allowing-write-in-read-only (thunk)
  (let ((old-value gr:*allow-write-in-read-only*))
    (setq gr:*allow-write-in-read-only* t)
    (prog1 (funcall thunk)
           (hw:memory-wait)
           (hw:read-md)
           (setq gr:*allow-write-in-read-only* old-value))))

(defun write-really-read-only (vma vma-boxed md md-boxed map-bits gc-trap physical-cluster)
  (if gr:*allow-write-in-read-only*
      (progn (setq gr::*page-fault-code* *page-fault-code-idle*)
             (associate-temporary (cluster-number vma)
                                  physical-cluster
                                  (map-cluster-volatility map-bits))
             (write-vma-generic (hw:dpb vmem::*temporary-map-entry*
                                        vinc::%%cluster-number vma) vma-boxed)
             (md-start-write-generic md md-boxed gc-trap)
             (deassociate-temporary)
             (hw:trap-off)
             (write-vma-generic vma vma-boxed)
             ;; We can only modify instruction space by coming through this
             ;; code.  We flush the icache here because we may have written
             ;; instruction space.
             (flush-icache)
             )
      (trap::tail-illop "Writing in read only space.")))

(defun write-really-read-only-clean (vma vma-boxed md md-boxed map-bits gc-trap physical-cluster)
  (if gr:*allow-write-in-read-only*
      (progn (mark-modified-in-pcd physical-cluster)
             (write-really-read-only vma vma-boxed md md-boxed map-bits gc-trap physical-cluster))
      (trap::tail-illop "Writing in clean read only space.")))

;(defun mark-modified (vma md vma-boxed-bit md-boxed-bit gc-mode physical-cluster)
;  (mark-modified-in-pcd physical-cluster)
;  (if (= vma-boxed-bit hw:$$boxed)
;      (hw:write-vma-boxed   vma)
;      (hw:write-vma-unboxed vma))
; ** this would lose on map timing **
;  (hw:write-map (inject-map-status (hw:read-map) $$map-status-normal))
;  (trap::enable-traps)
;  (md-start-write-generic md md-boxed-bit gc-mode))

(defun write-swapped-out (vma vma-boxed md md-boxed map-bits gc-trap)
  (swap-in (cluster-number vma))
  (hw:trap-off)
  (setq gr::*page-fault-code* *page-fault-code-idle*)
  (write-vma-generic vma vma-boxed)
  (md-start-write-generic md md-boxed gc-trap))


;;; @@@ This function probably does not currently ever get called. <17-Nov-88 wkf>
 (defun swap-in (virtual-cluster)
  (if (fresh-cluster? virtual-cluster)
      (progn
;         (trap::tail-illop "Swapping in fresh cluster.")
        ;; Fault on a fresh cluster, don't swap too much out.
        (assure-free-physical-clusters 1.)
;         (trap::tail-illop "assured a free cluster.")
        (let ((region-bits      (region-bits:cluster-region-bits virtual-cluster)); $$$ Moved region-bits down <17-Nov-88 wkf>
              (physical-cluster (allocate-physical-cluster)))
;           (trap::tail-illop "allocated that cluster")
          (associate-cluster physical-cluster virtual-cluster $$status-normal
                             (if (or (region-bits:region-read-only? region-bits)
                                     (= (region-bits:region-space-type region-bits)
                                        region-bits:$$region-space-code))
                                 (progn
                                   (when (< virtual-cluster 32.)
                                     (trap:illop "Got Bad readonly cluster < 32."))
                                   $$cluster-read-only)
                               $$cluster-read-write)
                             $$cluster-no-read-mar
                             $$cluster-no-write-mar)
;           (trap::tail-illop "associated that cluster")
          (region-bits:initialize-fresh-cluster physical-cluster virtual-cluster region-bits)
;           (trap::tail-illop "Initialized the fresh cluster.")
          ))
    (trap::tail-illop "Gak! I really have to swap something in!")
                                                ;    (swap-in-internal virtual-cluster)
    ))

;;; &&& REMOVED FOLLOWING THREE FUNCTIONS SINCE NOT EVER CALLED <17-Nov-88 wkf>

;;; Paging stuff added 12/10/87.

;;; +++ These variables are NOT set up any where!  The code which uses them apparently never runs. <17-Nov-88 WKF>
;(defvar li:*paging-devices* nil)
;(defvar li:*boot-band-paging-device-id* nil)
;(defvar li:*page-band-paging-device-id* nil)

;;; @@@ This function does not currently ever get called. <17-Nov-88 wkf>
#+Removed
 (defun swap-in-internal (virtual-cluster)
  (let* ((virtual-quantum (vinc:cluster-quantum virtual-cluster))
         (quantum-map-entry (quantum-map:read-quantum-map virtual-quantum))
         (status-bits (quantum-map:quantum-status-bits quantum-map-entry))
         (paging-device (array:svref li:*paging-devices* (quantum-map:quantum-device quantum-map-entry)))
         (quantum-dqin (quantum-map:quantum-dqin quantum-map-entry))
         (region (quantum-map:region-origin virtual-quantum))
         physical-cluster
         )
    ;; Error checking first.
    ;; To swap in a cluster, its associated quantum must have a valid entry in the quantum map table, i.e.,
    ;; it had to have been written at some point in the past. The entry in the map must be have both the valid
    ;; and mapped bit on at the same time, otherwise it is an error.
    (and (quantum-map:quantum-valid? status-bits)
         (li:error "quantum ~D has not been allocated yet." virtual-quantum status-bits))
    (and (not (= status-bits quantum-map:$$quantum-mapped))
         (li:error "Quantum ~D is not mapped to any paging device" virtual-quantum status-bits))
    (when (not paging-device)
      (li:error "Quantum ~D is not associated with any paging device." virtual-quantum))
    ;; get a physical cluster to write to.
    (setq physical-cluster (pcd:allocate-physical-cluster))
    (li:begin-read paging-device quantum-dqin (hw:ldb virtual-cluster (byte 4 0) 0))
    (li:next-read-cluster paging-device physical-cluster)
    (li:do-the-read paging-device)
    ;; map it to virtual-cluster
    (pcd:associate-cluster
      physical-cluster
      virtual-cluster
      pcd:$$status-normal
      (if (let ((region-bits (region-bits:cluster-region-bits virtual-cluster)))
            ;; $$$ Added region-bits definition by copying above method in SWAP-IN. <17-Nov-88 WKF>
            (or (region-bits:region-read-only? region-bits)
                (= (region-bits:region-space-type region-bits)
                   region-bits:$$region-space-code)))
          (progn
            (when (< virtual-cluster 32.)
              (trap:illop "Got Bad readonly cluster < 32."))
            pcd:$$cluster-read-only)
        pcd:$$cluster-read-write)
      pcd:$$cluster-no-read-mar
      pcd:$$cluster-no-write-mar)
    )
  )

;;; @@@ This function does not currently ever get called. <17-Nov-88 wkf>
#+Removed
 (defun page-out-page-and-change-device-for-quantum (virtual-quantum old-dqin region cluster-to-write physical-cluster
                                                    &optional (from-paging-device-id li:*boot-band-paging-device-id*)
                                                    (To-paging-device-id li:*page-band-paging-device-id*))
  ;; should check for valid paging device ids.
  (let ((from-paging-device (array:svref li:*paging-devices* from-paging-device-id))
        (To-paging-device (array:svref li:*paging-devices* To-paging-device-id))
        (cluster (vinc:quantum->cluster virtual-quantum))
        new-dqin map-entry)
    (or from-paging-device
        (li:error "No source paging device is specified for quantum ~S" virtual-quantum))
    (or To-paging-device
        (li:error "No destination paging device is specified for quantum ~S" virtual-quantum))
    (setq new-dqin (li:allocate-quantum to-paging-device))
    ;; write out the cluster to be swapped out any way.
    (li:begin-write To-paging-device new-dqin (hw:ldb cluster (byte 4 0) 0))
    (li:next-write-cluster to-paging-device physical-cluster)
    (li:do-the-write to-paging-device)
    ;; now make sure that all the pages still in boot device are copied to paging band device.
    ;; use cluster physical-cluster-to-write for the transfer.
    (dotimes (virtual-cluster-offset 16.)
      (setq cluster (hw:dpb virtual-cluster-offset (byte 4. 10.) cluster))
      (unless (= cluster cluster-to-write)
        (setq map-entry (map:read-map cluster))
        (if (= (map:map-lisp-valid-bit map-entry) 1)
            ;; already in core. Dirty it and let go.
            (map:write-map cluster (hw:dpb 1 hw:%%map-lisp-write-enable-bit map-entry))
          ;; otherwise page in and out to new device.
          (progn
            (pcd:associate-cluster              ;not map:associate-cluster
              physical-cluster
              cluster
              pcd:$$status-normal
              (region-bits:region-read-only region) nil nil)
            (li:begin-read from-paging-device old-dqin (hw:ldb cluster (byte 4 0) 0))
            (li:next-read-cluster from-paging-device physical-cluster)
            (li:do-the-read from-paging-device)
            (li:begin-write To-paging-device new-dqin (hw:ldb cluster (byte 4 0) 0))
            (li:next-write-cluster to-paging-device physical-cluster)
            (li:do-the-write to-paging-device))
          )
        )
      )
    ;; update quantum map.
    (quantum-map:modify-quantum-map virtual-quantum
                                    #'(lambda (To)
                                        (hw:dpb
                                          new-dqin
                                          quantum-map:%%quantum-map-dqin
                                          (hw:dpb
                                            to-paging-device-id
                                            quantum-map:%%quantum-map-device
                                            (hw:dpb 1 quantum-map:%%quantum-map-mapped-bit To)))))
    )
  )

;;; @@@ This function probably (since li:*paging-devices* is not setup) does not currently ever get called. <17-Nov-88 wkf>
#+Removed
 (defun swap-out-internal (physical-cluster)
  (let* ((pcd-data (pcd:read-pcd physical-cluster))
         (virtual-cluster (pcd:pcd-virtual-cluster-number pcd-data)))
    (when (not (pcd:clean-cluster? pcd-data))
      (let* ((virtual-quantum (quantum-map:cluster-quantum virtual-cluster))
             (quantum-map-entry (quantum-map:read-quantum-map virtual-quantum))
             (status-bits (quantum-map:quantum-status-bits quantum-map-entry))
             (paging-device-id (quantum-map:quantum-device quantum-map-entry))
             (paging-device (array:svref li:*paging-devices* paging-device-id))
             (quantum-dqin (quantum-map:quantum-dqin quantum-map-entry))
             (region (quantum-map:region-origin virtual-quantum))
             )
        ;; Error checking first.
        (dispatch (byte 2 0) status-bits
                  (quantum-map:$$quantum-allocated
                    (setq quantum-dqin (li:allocate-quantum paging-device))
                    (quantum-map:modify-quantum-map
                      virtual-quantum
                      #'(lambda (q) (vinc:dpb-multiple-unboxed
                                      quantum-dqin                 quantum-map:%%quantum-map-dqin
                                      quantum-map:$$quantum-mapped quantum-map:%%quantum-map-status
                                      q))))
                  (quantum-map:$$quantum-mapped nil)
                  (t
                    (li:error "quantum ~D has not been allocated yet." virtual-quantum status-bits)))
        (if (= paging-device-id li:*boot-band-paging-device-id*)
            (page-out-page-and-change-device-for-quantum
              virtual-quantum
              quantum-dqin
              virtual-cluster
              physical-cluster
              li:*boot-band-paging-device-id*
              li:*page-band-paging-device-id*)
          ;; just write out the page. it is on the paging band.
          (progn
            (li:begin-write paging-device quantum-dqin (hw:ldb virtual-cluster (byte 4 0) 0))
            (li:next-write-cluster paging-device physical-cluster)
            (li:do-the-write paging-device)))))
    (map:free-swapped-out-virtual-cluster virtual-cluster)
    (pcd:free-physical-cluster physical-cluster)
    )
  )

;;; &&& REMOVED PREVIOUS THREE FUNCTIONS SINCE NOT EVER CALLED <17-Nov-88 wkf>

(defun swapin-cluster-and-wire-it (virtual-cluster)
  (do ((success nil)
       (address (vinc:cluster->address virtual-cluster))
       pcd)
      (success pcd)
    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed address)
    (hw:read-md)
    (hw:nop)
    (hw:nop)
    (trap:without-traps
      #'(lambda ()
          (let ((map-data (map:hw-read-map-safe)))      ; $$$ Changed to call hw-read-map-safe <15-Nov-88 JIM>
            (when (= (map:map-lisp-valid-bit map-data) hw:$$map-valid)
              (setq pcd (map:map-on-board-address map-data))
              (pcd:modify-pcd pcd
                              #'(lambda (pcd-data)
                                  (setf (pcd:pcd-status pcd-data) pcd:$$status-wired)))
              (setq success t))))))
  )

(defun icache-map-fault-handler (pc+)
;  (when (not (= gr::*page-fault-code* *page-fault-code-idle*))
;    (trap::tail-illop "Recursively entered icache page fault handler."))
  (setq gr::*page-fault-code* *page-fault-code-icache-fault*)
  (let* ((address (k2:pc->addr pc+))
         (map-bits (map:read-map (vinc:cluster-number address))))
    (vinc:flush-icache)
    ;; Don't need to know if they were off because we are called only from
    ;; the trap handler (so far).
    (trap::trap-on)
    (dispatch (byte 4. 0.) (extract-map-status map-bits)
              ($$map-status-read-only-aged (icache-aged        address map-bits))
              ($$map-status-swapped-out    (icache-swapped-out address map-bits))
              (t                           (icache-error       address map-bits pc+)))))

(defun icache-error (address map-bits pc+)
  (trap::tail-illop "Icache-error called."))

(defun icache-aged (vma map-bits)
  (hw:trap-off)
  (hw:write-vma vma)
  (map:touch-aged map-bits)
  (pcd:rejuvenate-cluster (map-on-board-address map-bits))
  (setq gr::*page-fault-code* *page-fault-code-idle*)
  )

(defun icache-swapped-out (vma map-bits)
  (let ((virtual-cluster (vinc:cluster-number vma)))
    (if (fresh-cluster? virtual-cluster)
        (trap::tail-illop "Icache fault on fresh cluster.")
      (swap-in-internal virtual-cluster)))
  (hw:trap-off)
  (setq gr::*page-fault-code* *page-fault-code-idle*))

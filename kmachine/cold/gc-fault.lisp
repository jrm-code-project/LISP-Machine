;;; -*- Mode:LISP; Package:GC-FAULT; Base:10; Readtable:CL -*-


;;; This file handles volatility traps and box errors.

;;; This code runs without interrupts because saving the state, etc
;;; would just take too long.

;;; We expect the VMA and the MD to have the appropriate fault data in
;;; them.  We are expected to not modify either.

;;; Extra hair is introduced here because we may be taking a volatility trap on the
;;; temporary map entry.  In this case, we fake up the VMA to what it should be and
;;; process the trap.

(defun gc-fault-handler ()
  (let* ((real-vma (hw:read-vma))
         (vma-boxed (hw:ldb-not (hw:read-memory-status) hw:%%memory-status-vma-not-boxed-bit 0.)))
    (when (= (cluster-number real-vma) vmem::*temporary-map-entry*)
        (let ((deduced-vma (hw:dpb gr::*temporary-map-entry-virtual-cluster*
                                   vinc:%%cluster-number
                                   real-vma)))
        (vmem:write-vma-generic deduced-vma vma-boxed)
        (dispatch hw:%%memory-control-transporter-mode (hw:read-memory-control)
                  (vinc:$$transporter-mode-normal    (handle-normal-gc-fault))
                  (t (trap::illop "GC fault with weird transporter mode.")))))
    (vmem:write-vma-generic real-vma vma-boxed)
    (dispatch hw:%%memory-control-transporter-mode (hw:read-memory-control)
              (vinc:$$transporter-mode-normal    (handle-normal-gc-fault))
              (t (trap::illop "GC fault with weird transporter mode.")))))


(defun handle-normal-gc-fault ()
  (let ((gc-ram-bits (hw:read-gc-ram)))
    (if (= hw:$$box-error (hw:ldb gc-ram-bits hw:%%transporter-ram-box-error 0.))
        (trap::illop "Box error.")
        ;; It is a volatility trap.
        (let ((region-volatility (hw:ldb gc-ram-bits hw:%%gc-ram-quantum-volatility 0.))
              (map-bits          (map:hw-read-map-safe)))       ; $$$ changed to call hw-read-map-safe <15-Nov-88 JIM>
          (let ((map-status (extract-map-status map-bits)))
            (unless (or (= map-status $$map-status-normal)
                        (and (= map-status $$map-status-read-only)
                             gr:*allow-write-in-read-only*))
              (trap::illop "GC fault on non-NORMAL cluster.")))
          (hw:write-map (hw:dpb region-volatility hw:%%map-volatility map-bits))
;         (trap::illop "Exiting GC fault.")
          (hw:nop)))))

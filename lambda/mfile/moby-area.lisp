;;; -*- Mode: Lisp; Base: 8; Package: moby-file-system; Readtable: ZL -*-


(defun %free-area (area)
  (si:for-every-region-in-area (reg area)
    (%free-region reg))
  ;flush from current area list
  ;make area symbol unbound
  ;put area on %sys-com-free-area#-list
  ;hack area-name art-q-list
)

(defun %free-region (region)
  "Removes all trace of REGION from the area, virtual memory, and GC tables."
  (unless inhibit-scheduling-flag
    (ferror "This function must be called with scheduling inhibited."))
  (let ((area (%region-area region))
        (area-region-list-base (%region-origin sys:area-region-list))
        (region-list-thread-base (%region-origin sys:region-list-thread)))
    ;; This function needs to be pretty fast, to keep GC:RECLAIM-OLDSPACE from
    ;; consuming too much time without interrupts.  Define some magic accessors
    ;; for the relevant region tables.  (Note the local variables above.)
    (macrolet ((%area-region-list (area)
                 `(%p-pointer (+ area-region-list-base ,area)))
               (%region-list-thread (region)
                 `(%p-pointer (+ region-list-thread-base ,region))))
      ;; If it's the first region in the area, delete from the start of the thread.
      (if (eq region (%area-region-list area))
          (setf (%area-region-list area) (%region-list-thread region))
        ;; Otherwise search for the region and snap it out of the thread.
        (loop with this = (%area-region-list area)
              for next = (%region-list-thread this)
              until (eq next region)
              do (setq this next)
              finally (setf (%region-list-thread this) (%region-list-thread next)))))
    (%gc-free-region region)))

;;; -*- Mode:LISP; Package:REGION-DATA; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;
;;; Region data
;;;;;;;;;;;;;;;;

(define-global-frame memory-management)

(define-global-variable memory-management *region-free-pointer*)
(define-global-variable memory-management *region-allocation-status*)
(define-global-variable memory-management *region-end*)
(define-global-variable memory-management *region-gc-pointer*)

(define-global-variable memory-management *area-region-data*)
(define-global-variable memory-management *area-region-size*)
(define-global-variable memory-management *area-region-list*)
(define-global-variable memory-management *region-list-thread*)
(define-global-variable memory-management *area-region-bits*)

(defsubst region-table-ref (table region)
  (%vma-start-read-unboxed-md-unboxed (+ region table))
  (%read-md))

(defsubst region-table-store (table region data)
  (%write-md-unboxed data)
  (%vma-start-write-unboxed (+ region table)))

(defsetf region-table-ref region-table-store)

(defsubst region-free-pointer (region)
  (region-table-ref *region-free-pointer* region))

(defsubst region-allocation-status (region)
  (region-table-ref *region-allocation-status* region))

(defsubst region-end (region)
  (region-table-ref *region-end* region))

(defsubst region-gc-pointer (region)
  (region-table-ref *region-gc-pointer* region))

(defun create-region-data-for-cold-load ()
  (let ((region-data (allocate-region (ceiling (* *number-of-regions* 4.) *qs-in-quantum*)
                                      (parameters->region-bits
                                        $$region-space-fixed
                                        $$scavenge-disabled
                                        $$region-read-write
                                        31.)
                                      0.)))     ;Swapin entire thing.

    (setq *region-free-pointer*      (quantum->address region-data))
    (setq *region-allocation-status* (+ *region-free-pointer*      *number-of-regions*))
    (setq *region-end*               (+ *region-allocation-status* *number-of-regions*))
    (setq *region-gc-pointer*        (+ *region-end*               *number-of-regions*))

    ;; Bash data for initial regions.  Do this right someday.
    (setf (region-free-pointer      0) (%dpb 1. %%k-quantum-number 0.))
    (setf (region-allocation-status 0) 0)
    (setf (region-end               0) (%dpb 1. %%k-quantum-number 0.))
    (setf (region-gc-pointer        0) (%dpb 1. %%k-quantum-number 0.))

    (setf (region-free-pointer      1) (%dpb 2. %%k-quantum-number 0.))
    (setf (region-allocation-status 1) 0)
    (setf (region-end               1) (%dpb 2. %%k-quantum-number 0.))
    (setf (region-gc-pointer        1) (%dpb 2. %%k-quantum-number 0.))

    ;; We ourselves is full.
    (setf (region-free-pointer region-data) (+ *region-gc-pointer* *number-of-regions*))
    (setf (region-free-pointer region-data) 0)
    (setf (region-free-pointer region-data) (+ *region-gc-pointer* *number-of-regions*))
    (setf (region-free-pointer region-data) (+ *region-gc-pointer* *number-of-regions*))

    region-data
    ))

(defun show-region (region stream)
  (let* ((quantum-bits (%read-quantum-map region))
         (origin (if (quantum-empty? quantum-bits)
                     (progn (format stream "~&Region ~D is not in the quantum map." region)
                            region)
                     (let ((origin (region-origin quantum-bits)))
                       (when (not (= origin region))
                         (format stream "~&Region ~D seems to begin at ~D" region origin))
                       origin))))
    (show-region-bits origin stream)
    (format stream "~&Origin: ~8D, Free Pointer: ~8D, GC pointer: ~8D End: ~8D"
            (quantum->address    origin)
            (region-free-pointer origin)
            (region-gc-pointer   origin)
            (region-end          origin)
            )))

(defun create-region (size region-bits volatility)
  (let* ((region (allocate-region size region-bits volatility))
         (origin (quantum->address region)))
    ;; Regions start out empty.
    (setf (region-free-pointer      region) origin)
    (setf (region-allocation-status region) 0)
    (setf (region-end               region) (+ origin (quantum->address size)))
    (setf (region-gc-pointer        region) origin)
    region))

(defun advance-free-pointer (region how-far)
  (let ((free-pointer (region-free-pointer region))
        (end          (region-end          region)))
    (let ((new-pointer (+ how-far free-pointer)))
      (if (> new-pointer end)
          (illop "Advanced free pointer beyond end of region.")
          (setf (region-free-pointer region) new-pointer)))))

;;; -*- Mode:LISP; Package:MEMORY-MANAGEMENT; Base:10; Readtable:CL -*-

(export '(
          *number-of-areas*
          *number-of-regions*

          free-cluster
          region-number
          region-origin
          region-table-ref
          region-table-store
          ))

(defconstant *number-of-regions* vinc:*number-of-quanta*)       ;for convenience
(defconstant *number-of-areas*                      256.)       ;by fiat.

;;;;;;;;;;;;;;;;
;;; Region data
;;;;;;;;;;;;;;;;

;;; Region tables cannot have boxed storage because
;;; a) they cannot have pointers.
;;; b) they address something that won't fit in a fixnum.

;;; The Table argument here is unboxed, but the region argument is a fixnum.
;;; We do a hw:24+ to avoid getting box errors from the datatype ram.

(defsubst region-table-ref (table region)
  (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:24+ region table))
  (hw:read-md))

(defsubst region-table-store (table region data)
  (hw:write-md-unboxed data)
  (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ region table))
  data)

;(defsetf region-table-ref region-table-store)

(defun free-cluster (virtual-cluster)
  ;this hacks the physical map, and also frees any on-board physical cluster associated in the hardware map.
  (let ((map-bits (map::read-map virtual-cluster)))
    (unless (= (map:extract-map-status map-bits) map:$$map-status-swapped-out)
      (if (map:map-local-memory? map-bits)
          (pcd:free-physical-cluster (map:map-on-board-address map-bits))
          (trap::illop "Freeing nubus space")))
    (map:free-virtual-cluster virtual-cluster)))

(defsubst region-origin (region)
  (quantum->address region))

(defsubst region-number (pointer)
  (quantum-number pointer))

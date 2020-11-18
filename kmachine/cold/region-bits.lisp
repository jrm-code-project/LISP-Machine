;;; -*- Mode:LISP; Package:REGION-BITS; Base:10; Readtable:CL -*-

(export '(
          $$scavenge-enabled
          $$scavenge-disabled
          $$region-copy-space
          $$region-external-bus
          $$region-fixed
          $$region-flippable
          $$region-internal-memory
          $$region-new-space
          $$region-read-only
          $$region-read-write
          $$region-space-code
          $$region-space-cons
          $$region-space-free
          $$region-space-invalid
          $$region-space-structure
          $$region-space-unboxed

          cluster-region-bits
          encode-region-bits
          free-region
          initialize-fresh-cluster
          make-region
          read-region-bits
          region-copy-space
          region-external-bus
          region-external-bus?
          region-flippable
          region-read-only
          region-read-only?
          region-scavenge-enable
          region-scavenge-enable?
          region-space-type
          region-swapin-quantum
          ))

;;; Each entry in the region bits table is a Q with the bits encoded
;;; as follows.  NOTE:  The datatype is reserved in this table even
;;; though it is not used.  This table should be considered unboxed
;;; storage.  This is because it shares a region with the quantum map
;;; which is unboxed.

;;; Regions are numbered according to the cluster they begin in.
;;; Regions occupy a contiguous chunk of address space.  They occupy
;;; an integral number of quanta.  No quantum contains more than one
;;; region.  Because there are sufficient entries in the region tables
;;; to describe all the quanta, some of the information kept there
;;; is not valid (for instance, region 2 is 7 quanta long, so there
;;; will not be a region 3, 4, 5, etc.)

;;; The region bits keep track of the following information.
;;; SWAPIN-QUANTUM is how many clusters to page in whenever a page
;;;   fault is taken in this region.
;;; SCAVENGE-BIT allows the scavenger to look at this region.
;;;   It should only be on for boxed storage, and then, only
;;;   for copyspace regions.  It could be deduced from these
;;;   constraints, but this is an optimization.
;;; READ-ONLY makes all clusters in this region read-only.
;;;   Writing and consing are not permitted.
;;; SPACE-TYPE describes the format of the storage in this
;;;   region.
;;;   FREE       this entry does not belong to any region (denotes free space)
;;;   INVALID    this entry corresponds to a region number that is
;;;              covered by another region.  The code should never use
;;;              this kind of entry.
;;;  These entries denote valid storage, each is scavenged differently.
;;;   UNBOXED    This region contains unboxed storage.  It cannot be scavenged
;;;              or flipped, so those bits will not be set.
;;;   CONS       This region contains only boxed storage.  The scavenge bit
;;;              should be on.  In addition, the transporter will treat this
;;;              region as cons cells and keep pairs together.  If the flip bit
;;;              is on, you should only put cons cells here.  Structure handles
;;;              are not kept for clusters in this region.
;;;   STRUCTURE  This region contains boxed and unboxed storage.  Every chunk
;;;              of storage here will have a header.  Structure handles are kept
;;;              for clusters in this region.
;;;   CODE       This region contains executable code.  The clusters in this region
;;;              must be marked as read-only.  If the scavenge bit is on, the scavenger
;;;              will examine all CALL instructions and "move immediate 32bits boxed"
;;;              instructions.
;;;
;;; NEW-SPACE prevents the transporter from consing in this region.  Typically,
;;;   the user will cons in NEW-SPACE and the transporter in COPYSPACE.
;;; FLIPPABLE allows the garbage collector to reclaim this region.  Don't turn
;;; it on for UNBOXED regions.

(defconstant %%region-bits                (byte 12. 0.))
(defconstant %%region-bits-swapin-quantum (byte 4.  0.))
(defconstant %%region-bits-scavenge-bit   (byte 1.  4.))
(defconstant %%region-bits-read-only      (byte 1.  5.))
(defconstant %%region-bits-space-type     (byte 3.  6.))
(defconstant %%region-bits-new-space      (byte 1.  9.))
(defconstant %%region-bits-flippable      (byte 1. 10.))
(defconstant %%region-bits-external-bus   (byte 1. 11.))

;; reserved (byte 3. 12.)  (see area region bits)
;; unused (byte 10. 15.)
;; reserved %%k-data-type

(defconstant $$region-internal-memory 0)
(defconstant $$region-external-bus    1)

(vinc::defextractor region-external-bus       %%region-bits-external-bus)
(vinc::defflag-extractor region-external-bus? %%region-bits-external-bus $$region-external-bus)

(vinc::defextractor region-swapin-quantum %%region-bits-swapin-quantum)

(defconstant $$scavenge-disabled 0.)
(defconstant $$scavenge-enabled  1.)

(vinc::defextractor      region-scavenge-enable  %%region-bits-scavenge-bit)
(vinc::defflag-extractor region-scavenge-enable? %%region-bits-scavenge-bit $$scavenge-enabled)

(defconstant $$region-read-write 0.)
(defconstant $$region-read-only  1.)

(vinc::defextractor      region-read-only  %%region-bits-read-only)
(vinc::defflag-extractor region-read-only? %%region-bits-read-only $$region-read-only)

(vinc::defextractor region-space-type %%region-bits-space-type)

(defconstant $$region-space-free      #b000)
(defconstant $$region-space-invalid   #b001)
(defconstant $$region-space-unboxed   #b010)
(defconstant $$region-space-cons      #b011)
(defconstant $$region-space-structure #b100)
(defconstant $$region-space-code      #b101)
;; unused 6, 7

(defconstant $$region-copy-space 0)
(defconstant $$region-new-space  1)

(vinc::defextractor      region-copy-space  %%region-bits-new-space)
(vinc::defflag-extractor region-copy-space? %%region-bits-new-space $$region-copy-space)

(defconstant $$region-fixed     0)
(defconstant $$region-flippable 1)

(vinc::defextractor region-flippable %%region-bits-flippable)
(vinc::defflag-extractor region-flippable? %%region-bits-flippable $$region-flippable)

(defun verify-region (region)
  (when (not
          (and (quantum-map:valid-quantum? region)
               (= (quantum-map:quantum-region-origin region) region)))
    (trap::tail-illop "Found an invalid region.")))



(defsubst read-region-bits (region)
  (system-table-ref gr::*region-bits* region))

(defsubst write-region-bits (region value)
  (system-table-store gr::*region-bits* region value))

(defsubst region-valid? (region)
  (not (= (region-space-type (read-region-bits region))
          $$region-space-invalid)))

(defsubst region-free? (region)
  (= (region-space-type (read-region-bits region))
          $$region-space-free))


(defsubst encode-region-bits (flippable new-space space-type read-only scavenge-enable external-bus swapin-quantum)
  (vinc::dpb-multiple-boxed
    flippable       %%region-bits-flippable
    new-space       %%region-bits-new-space
    space-type      %%region-bits-space-type
    read-only       %%region-bits-read-only
    scavenge-enable %%region-bits-scavenge-bit
    external-bus    %%region-bits-external-bus
    swapin-quantum  %%region-bits-swapin-quantum
    0.))

(defun make-region (size-in-quanta region-bits volatility)
 ;this does not hack the physical map, altho it does verify that map show "fresh" for clusters in the region.
 ;the region bits are stored in the gr::*region-bits* array.
  (let ((quantum-origin
          (quantum-map:allocate-quanta size-in-quanta
                                       (= $$region-space-code
                                          (region-space-type region-bits))
                                       volatility)))
;    (trap::tail-illop "Making region.")
    (labels ((verify-fresh-quantum (quantum cluster)
               (cond ((= cluster vinc::*clusters-in-quantum*) '())
                     ((not (map::fresh-cluster?
                                (hw:dpb quantum
                                        %%quantum-number-in-cluster
                                        cluster)))
                      (trap::tail-illop "Make region found some unfresh clusters."))
                     (t (verify-fresh-quantum quantum (1+ cluster)))))

             (verify-fresh-quanta (count quantum)
               (if (zerop count)
                   '()
                   (progn (verify-fresh-quantum quantum 0)
                          (verify-fresh-quanta (1- count) (1+ quantum))))))

      ;; Verify that the clusters are fresh in the physical map.
      (verify-fresh-quanta size-in-quanta quantum-origin))

;    (trap::tail-illop "Verified freshness.")

    ;; Setup the region bits in gr:*region-bits*
    (labels ((invalidate-region-bits (start how-many)
               (if (zerop how-many)
                   '()
                   (progn (write-region-bits start
                            (hw:dpb $$region-space-invalid %%region-bits-space-type region-bits))
                          (invalidate-region-bits (1+ start) (1- how-many))))))
      (invalidate-region-bits quantum-origin size-in-quanta))

;    (trap::tail-illop "Invalidated region bits.")

    (write-region-bits quantum-origin (hw:ldb region-bits %%region-bits 0.))

    quantum-origin))

(defun free-clusters-in-region (region cluster)
  (if (= cluster vinc:*clusters-in-quantum*)
      '()
      (progn (free-cluster (hw:dpb region
                                   %%quantum-number-in-cluster
                                   cluster))
             (free-clusters-in-region region (1+ cluster)))))

(defun zap-region-bits (region)
  (write-region-bits region (encode-region-bits
                              $$region-fixed
                              $$region-copy-space
                              $$region-space-free
                              $$region-read-only
                              $$scavenge-disabled
                              $$region-internal-memory
                              0.)))

(defun free-region (n)
  ;; N is the region number to free.  We scan over the quanta in this
  ;; region using the fact that the region number and the initial quantum
  ;; is the same.
  (verify-region n)
  (labels ((zap-region (n)
             (if (= n *number-of-regions*)
                 '()
                 (let ((bits (read-region-bits n)))
                   (when (= (region-space-type bits) $$region-space-invalid)
                     (zap-region-bits n)
                     (free-clusters-in-region n 0)
                     (zap-region (1+ n)))))))

    (zap-region-bits n)
    (free-clusters-in-region n 0)
    (zap-region (1+ n)))
  ;; Now toss out the quanta associated with this region.
  (quantum-map:deallocate-quanta n))

(defun cluster-region-bits (cluster)
  (read-region-bits (quantum-map:cluster-region cluster)))

(defun initialize-free-cluster (va)
  va
  (trap::tail-illop "Initialize-fresh-cluster called on cluster in free-space."))

(defun initialize-invalid-cluster (va)
  va
  (trap::tail-illop "Initialize-fresh-cluster called on invalid region."))

(defun initialize-unboxed-cluster (va)
  (hw:write-md-unboxed (hw:unboxed-constant 0))
  (do ((ptr (cluster->address *temporary-map-entry*) (hw:24+ 4 ptr))
       (i (hw:ldb vinc:*qs-in-cluster* (byte 22. 2.) 0) (1- i)))
      ((zerop i) nil)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 0 ptr))
    (hw:nop)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 1 ptr))
    (hw:nop)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 2 ptr))
    (hw:nop)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 3 ptr))))

(defun initialize-cons-cluster (va)
  ())

(defun initialize-structure-cluster (va)
  (hw:write-md-boxed nil)
  (do ((ptr (cluster->address *temporary-map-entry*) (hw:24+ 4 ptr))
       (i (hw:ldb vinc:*qs-in-cluster* (byte 22. 2.) 0) (1- i)))
      ((zerop i) nil)
    (hw:vma-start-write-unboxed (hw:24+ 0 ptr))
    (hw:nop)
    (hw:vma-start-write-unboxed (hw:24+ 1 ptr))
    (hw:nop)
    (hw:vma-start-write-unboxed (hw:24+ 2 ptr))
    (hw:nop)
    (hw:vma-start-write-unboxed (hw:24+ 3 ptr))))

(defun initialize-code-cluster (va)
  (hw:write-md-unboxed (hw:unboxed-constant 0))
  (do ((ptr (cluster->address *temporary-map-entry*) (hw:24+ 4 ptr))
       (i (hw:ldb vinc:*qs-in-cluster* (byte 22. 2.) 0) (1- i)))
      ((zerop i) nil)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 0 ptr))
    (hw:nop)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 1 ptr))
    (hw:nop)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 2 ptr))
    (hw:nop)
    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 3 ptr))))

(defun initialize-unknown-cluster (va)
  (trap::tail-illop "Initialize-fresh-cluster called on unknown data."))

(defun initialize-fresh-cluster (physical-cluster virtual-cluster region-bits)
  (associate-temporary virtual-cluster physical-cluster 0)
  (dispatch %%region-bits-space-type region-bits
    ($$region-space-free      (initialize-free-cluster      virtual-cluster))
    ($$region-space-invalid   (initialize-invalid-cluster   virtual-cluster))
    ($$region-space-unboxed   (initialize-unboxed-cluster   virtual-cluster))
    ($$region-space-cons      (initialize-cons-cluster      virtual-cluster))
    ($$region-space-structure (initialize-structure-cluster virtual-cluster))
    ($$region-space-code      (initialize-code-cluster      virtual-cluster))
    (t (initialize-unknown-cluster virtual-cluster)))
  (deassociate-temporary)
  )

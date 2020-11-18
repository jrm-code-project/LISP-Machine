;;; -*- Mode:LISP; Base:10 -*-
(defconstant $$region-fixed     0)
(defconstant $$region-new-space  1)
(defconstant $$region-space-unboxed   #b010)
(defconstant $$region-space-code      #b101)
(defconstant $$region-read-write 0.)
(defconstant $$scavenge-disabled 0.)
(defconstant $$region-internal-memory 0)
(defconstant $$cluster-fresh     1)

(defun field-maximum (byte-specifier)
  (lisp::expt 2. (byte-size byte-specifier)))

(defconstant %%gc-ram-md-byte    (byte 12. 14.))
(defconstant %%mapped-vma-byte   (byte 16. 10.))
(defconstant %%quantum-number    %%gc-ram-md-byte)
(defconstant %%cluster-number    %%mapped-vma-byte)
(defconstant *number-of-quanta*  (field-maximum %%quantum-number))
(defconstant *number-of-regions* *number-of-quanta*)
(defconstant %%map-fresh-cluster (byte 1. 10.))
(defconstant %%quantum-number-in-cluster (byte (byte-size %%quantum-number)
                                               (- (byte-position %%quantum-number)
                                                  (byte-position %%cluster-number))))
(defvar gr::*region-free-pointer*)
(defvar gr::*region-end*)
(defvar gr::*region-gc-pointer*)

(defconstant *clusters-in-quantum* (lisp::expt 2. (lisp::- (byte-position %%quantum-number)
                                                           (byte-position %%cluster-number))))

(defsubst quantum->address (quantum)
  (hw:dpb-unboxed quantum %%quantum-number (hw:unboxed-constant 0.)))

(defsubst region-origin (region)
  (quantum->address region))

(defun synthesize-region-data ()
  ;; 4096 regions, 4 tables = 16 clusters = 1 quantum
  (let* ((region-data (make-region 1.
                       (encode-region-bits
                         $$region-fixed
                         $$region-new-space
                         $$region-space-unboxed
                         $$region-read-write
                         $$scavenge-disabled
                         $$region-internal-memory
                         0.)
                       0.))
         (origin (memory-management::region-origin region-data)))
;    (trap::illop "Made region data region.")
    (setq gr::*region-free-pointer* origin)
    (setq gr::*region-end*          (hw:32+ gr::*region-free-pointer* *number-of-regions*))
    (setq gr::*region-gc-pointer*   (hw:32+ gr::*region-end*          *number-of-regions*))
    (initialize-region-data)))

(defconstant %%region-bits-swapin-quantum (byte 4.  0.))
(defconstant %%region-bits-scavenge-bit   (byte 1.  4.))
(defconstant %%region-bits-read-only      (byte 1.  5.))
(defconstant %%region-bits-space-type     (byte 3.  6.))
(defconstant %%region-bits-new-space      (byte 1.  9.))
(defconstant %%region-bits-flippable      (byte 1. 10.))
(defconstant %%region-bits-external-bus   (byte 1. 11.))

(prims:defmacro vinc::defextractor (name field)
  `(progn
     (PRIMS::DEFSUBST ,name (PRIMS::Q)
       (HW::LDB PRIMS::Q ,field 0.))
     (PRIMS::DEFSETF ,name (PRIMS::Q) (PRIMS::VALUE)
       `(HW:DPB ,prims::value ,',field ,prims::q))))

(prims:defmacro vinc::defflag-extractor (name field value)
  `(PRIMS::DEFSUBST ,name (PRIMS::Q)
     (HW::32= (HW::LDB PRIMS::Q ,field 0.) ,value)))

(vinc::defextractor region-space-type %%region-bits-space-type)
(vinc::defflag-extractor cluster-is-fresh?            %%map-fresh-cluster $$cluster-fresh)

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
  (let ((quantum-origin
          (quantum-map:allocate-quanta size-in-quanta
                                       (= $$region-space-code
                                          (region-space-type region-bits))
                                       volatility)))
;    (trap::illop "Making region.")
    (labels ((verify-fresh-quantum (quantum cluster)
               (cond ((= cluster *clusters-in-quantum*) '())
                     ((not (fresh-cluster?
                                (hw:dpb quantum
                                        %%quantum-number-in-cluster
                                        cluster)))
                      (trap::illop "Make region found some unfresh clusters."))
                     (t (verify-fresh-quantum quantum (1+ cluster)))))

             (verify-fresh-quanta (count quantum)
               (if (zerop count)
                   '()
                   (progn (verify-fresh-quantum quantum 0)
                          (verify-fresh-quanta (1- count) (1+ quantum))))))

      ;; Verify that the clusters are fresh.
      (verify-fresh-quanta size-in-quanta quantum-origin))
    ))

(defun fresh-cluster? (cluster)
  (cluster-is-fresh? (read-map cluster)))

(defsubst cluster->address (cluster)
  (hw:dpb cluster %%cluster-number 0.))

(defsubst address-map (virtual-cluster)
  (hw:write-vma-unboxed (cluster->address virtual-cluster))
  ;; Wait for the VMA to load.
  (hw:nop)
  (hw:nop)
  nil)

(defun read-map (virtual-cluster)
  (address-map virtual-cluster)
  (hw:read-map))

***************************************************************************************************

;;;;All references up to here are local to this file.

(defmacro locking-quantum-map (thunk)
  `(LET ((THUNK ,thunk))
     (IF (ZEROP (INCF GR::*QUANTUM-MAP-SEMAPHORE*))
         (PROG1 (FUNCALL THUNK)
                (SETQ GR::*QUANTUM-MAP-SEMAPHORE* -1))
         (TRAP::ILLOP "Quantum map being hacked recursively."))))

(defun allocate-quanta (how-many instruction-space? volatility)
  (locking-quantum-map
    #'(lambda ()
        (let ((origin (locate-contiguous-quanta how-many instruction-space?)))
;         (trap::illop "Found quanta.")
          (labels ((initialize-quanta-volatility (quantum count)
                     (if (zerop count)
                         nil
                         (progn (gc-ram:initialize-quantum quantum volatility)
                                (modify-quantum-map-prelocked quantum
                                                    #'(lambda (old)
                                                        (vinc::dpb-multiple-unboxed
                                                          $$quantum-allocated %%quantum-map-status
                                                          origin              %%quantum-map-region-origin
                                                          old)))
                                (initialize-quanta-volatility (1+ quantum) (1- count))))))
            (initialize-quanta-volatility origin how-many)
            origin)))))



(defun initialize-region-data ()
  (zap-all-regions 0)
  (find-region 0)
  nil)

(defun zap-all-regions (count)
  (if (= count *number-of-regions*)
      '()
      (progn (setf (region-free-pointer count) (hw:unboxed-constant 0))
             (setf (region-gc-pointer   count) (hw:unboxed-constant 0))
             (setf (region-end          count) (hw:unboxed-constant 0))
             (zap-all-regions (1+ count)))))

(defun find-region (scan)
  (cond ((= scan *number-of-regions*) '())
        ((not (valid-quantum? scan)) (find-region (1+ scan)))
        (t  (accumulate-region-data scan (1+ scan)))))

(defun valid-quantum? (quantum)
  (quantum-valid? (read-quantum-map quantum)))

(defsubst read-quantum-map (quantum)
  (system-table-ref gr::*quantum-map* quantum))

(defun system-table-ref (table index)
  (hw:vma-start-read-no-transport (hw:24+ table index) :unboxed :unboxed)
  (hw:read-md))

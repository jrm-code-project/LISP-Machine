;;; -*- Mode:LISP; Package:QUANTUM-MAP; Base:10; Readtable:CL -*-

(export '(
          allocate-quanta
          cluster-region
          deallocate-quanta
          quantum-region
          quantum-region-origin
          valid-quantum?))

;;; Fields in the quantum map.

(defconstant %%quantum-map-dqin          (byte 12.  0.))        ;Any device can map all quanta
(defconstant %%quantum-map-device        (byte  4. 12.))        ;Up to 16 I/O or paging devices
(defconstant %%quantum-map-region-origin (byte 12. 16.))      ;Same as region number
(defconstant %%quantum-map-status        (byte  2. 28.))
(defconstant %%quantum-map-valid-bit     (byte  1. 28.))
(defconstant %%quantum-map-mapped-bit    (byte  1. 29.))
;(defconstant %%quantum-map
;unused (byte 2. 30.)

(defconstant $$quantum-invalid 0)
(defconstant $$quantum-valid   1)

(defconstant $$quantum-map-bit-unmapped 0)
(defconstant $$quantum-map-bit-mapped   1)

(defconstant $$quantum-empty     #b00)
(defconstant $$quantum-allocated #b01)
(defconstant $$quantum-error     #b10)
(defconstant $$quantum-mapped    #b11)

(vinc::defextractor      quantum-dqin        %%quantum-map-dqin)
(vinc::defextractor      quantum-device      %%quantum-map-device)
(vinc::defflag-extractor quantum-valid?      %%quantum-map-valid-bit $$quantum-valid)
(vinc::defextractor      quantum-status-bits %%quantum-map-status)
(vinc::defflag-extractor quantum-empty?      %%quantum-map-status $$quantum-empty)
(vinc::defextractor      region-origin       %%quantum-map-region-origin)

;;; The invariant for the quantum map is that all chunks for allocated quanta
;;; have the same origin, and that empty chunks have zero for the origin.

(defmacro locking-quantum-map (thunk)
  `(LET ((THUNK ,thunk))
     (IF (ZEROP (INCF GR::*QUANTUM-MAP-SEMAPHORE*))
         (PROG1 (FUNCALL THUNK)
                (SETQ GR::*QUANTUM-MAP-SEMAPHORE* -1))
         (TRAP::ILLOP "Quantum map being hacked recursively."))))

(defsubst read-quantum-map (quantum)
  (system-table-ref gr::*quantum-map* quantum))

(defsubst write-quantum-map (quantum value)
  (system-table-store gr::*quantum-map* quantum value))

(defmacro modify-quantum-map (quantum modifier)
  `(LET ((MODIFIER ,modifier)
         (QUANTUM  ,quantum))
     (LOCKING-QUANTUM-MAP
       #'(LAMBDA ()
           (WRITE-QUANTUM-MAP QUANTUM
             (FUNCALL MODIFIER (READ-QUANTUM-MAP QUANTUM)))))))

(defmacro modify-quantum-map-prelocked (quantum modifier)
  `(LET ((MODIFIER ,modifier)
         (QUANTUM  ,quantum))
     (WRITE-QUANTUM-MAP QUANTUM (FUNCALL MODIFIER (READ-QUANTUM-MAP QUANTUM)))))

;;; In the interest of making 26 bit pointer arithmetic possible
;;; on a machine that primarily supports 24 bit fixnums, we
;;; divide the quantum map up into chunks of 2^23 qs.  We never
;;; allocate across one of these boundaries.

;;; There are 2^12 quanta.  There are 2^3 zones.  We cannot
;;; group quanta across zone boundaries. (see zoning restrictions)

;;; Also, the PC on this machine can only address the top half of the
;;; virtual memory.  In order to hack this, there is an additional
;;; argument to the allocator that makes it look in instruction space.

;(defconstant *number-of-zones* (lisp::expt 2. (- (byte-size vinc:%%pointer)
;                                                (- (byte-size vinc:%%fixnum-field)
;                                                   1))))

;(defconstant *quanta-per-zone* (lisp::floor vinc:*number-of-quanta* *number-of-zones*))
;(defconstant *first-instruction-zone* (lisp::floor *quanta-per-zone* 2.))

(defconstant *number-of-zones* 8.)
(defconstant *zone-within-quantum* (byte 4. 9.))
(defconstant *first-instruction-zone* 4.)

(defsubst zone-origin-in-quanta (zone)
  (hw:dpb zone *zone-within-quantum* 0))

(defun locate-contiguous-quanta (how-many for-instruction-space?)
  ;; Does a first fit search for contiguous quanta in the
  ;; map.
  ;; See Knuth's Fundamental Algorithms Vol I pg 437. before you go
  ;; changing this to best fit.
  (labels ((scan-zones (zone)
             (if (= zone *number-of-zones*)
                 (trap::illop "Could not allocate requested contiguous quanta.")
                 (let ((origin (find-fit-in-zone zone)))
                   (if (null origin)
                       (scan-zones (1+ zone))
                       origin))))

           (find-fit-in-zone (zone)
             (let ((origin (zone-origin-in-quanta zone))
                   (limit  (zone-origin-in-quanta (1+ zone))))
               (find-fit origin limit)))

           (find-fit (scan limit)
             (if (= scan limit)
                 '()
                 (if (quantum-empty? (read-quantum-map scan))
                     (let ((size (chunk-acceptable scan limit)))
                       (if (= size how-many)
                           scan
                           (find-fit (+ scan size) limit)))
                     (find-fit (1+ scan) limit))))

           (chunk-acceptable (scan limit)
             (scan-chunk scan limit 0))

           (scan-chunk (scan limit size)
             (cond ((= size how-many) size)
                   ((= scan limit)    size)
                   ((quantum-empty? (read-quantum-map scan))
                    (scan-chunk (1+ scan) limit (1+ size)))
                   (t size))))

    (scan-zones (if for-instruction-space?
                    *first-instruction-zone*
                    0))))

;(defun find-contiguous-quanta (how-many)
;  (let ((best-origin)
;       (best-size))
;    (do ((scan 0 (1+ scan)))
;       ((>= scan vinc:*number-of-quanta*)
;        (if (null best-size)
;            (values best-origin best-size)))
;      (let ((quantum-map-entry (read-quantum-map scan)))
;       (when (quantum-empty? quantum-map-entry)
;         ;; Found a free chunk, what is the size?
;         (block find-size
;           (do ((scan-ahead scan (1+ scan-ahead))
;                (size       0    (1+ size)))
;               (())
;             (let ((quantum-map-entry (read-quantum-map scan-ahead)))
;               (when (or (not (quantum-empty? quantum-map-entry))
;                         (zerop (mod scan-ahead *quanta-per-zone*)))
;                 (when (and (>= size how-many)
;                            (or (null best-size)
;                                (< size best-size)))
;                   (setq best-origin scan)
;                   (setq best-size   size))
;                 (setq scan scan-ahead)
;                 (return-from find-size))))))))))

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

(defun deallocate-quanta (origin) ;;;+++ I am suspicious of this code 10/20/88 --wkf
  (let ((bits (read-quantum-map origin)))
    (when (not (= origin (region-origin bits)))
      (trap::illop "Invalid deallocation:  Didn't deallocate from start."))
    (when (not (quantum-valid? bits))
      (trap::illop "Invalid deallocation:  Chunk is not active.")))
  (labels ((deallocate-quantum (n)
             (if (= n vinc:*number-of-quanta*)
                 '()
                 (let ((bits (read-quantum-map n)))
                   (when (= origin (region-origin bits)) ;;+++ Why not an if??? 10/20/88 --wkf
                     (dispatch %%quantum-map-status bits
                       ($$quantum-empty     (trap::illop "Quantum map inconsistant."))
                       ($$quantum-allocated)
                       ($$quantum-error     (trap::illop "Quantum map inconsistant."))
                       ($$quantum-mapped    (trap::illop "Deallocate mapped quantum!")))
                     ;      (paging-devices:deallocate-quantum (sim-debug::get-quantum-device bits)
                     ;                          (quantum-dqin bits))))
                     (modify-quantum-map n
                                         #'(lambda (old)
                                             (hw:dpb $$quantum-empty %%quantum-map-status old)))
                     (deallocate-quantum (1+ n)))))))
    (deallocate-quantum origin)))

(defun quantum-region-origin (quantum)
  (region-origin (read-quantum-map quantum)))

(defun valid-quantum? (quantum)
  (quantum-valid? (read-quantum-map quantum)))

(defun quantum-region (quantum)
  (when (not (valid-quantum? quantum))
    (li:error "Quantum not in any region." quantum))
  (quantum-region-origin quantum))

(defun cluster-region (cluster)
  (quantum-region (cluster-quantum cluster)))

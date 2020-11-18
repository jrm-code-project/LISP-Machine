;;; -*- Mode:LISP; Package:AREA-DATA; Base:10; Readtable:CL -*-

(export '(
          get-active-region
          initialize-area-data
          make-area
          make-area-fixed
          make-region-in-area
          reset-temporary-area
          ))

;(in-package 'area-data)

;;; The area tables have boxed storage in them, but no headers.

;;; The region list thread connects all the regions in an area into a list.
;;; The last region in the list points back to the area.

(region-data::def-region-accessor region-list-thread gr::*region-list-thread*)

(defconstant %%region-list-thread-end-flag (byte 1. 31.))
(defconstant %%region-list-thread-next-region (byte (byte-size hw:%%gc-ram-md-byte) 0.))

(defconstant $$thread-continues 0.)
(defconstant $$thread-ends      1.)

(vinc::defflag-extractor thread-continues? %%region-list-thread-end-flag $$thread-continues)

;;; The area tables are boxed space marked as cons space.

(defsubst area-table-ref (table area)
  (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ table area))
  (hw:read-md))

(defsubst area-table-store (table area new-value)
  (hw:write-vma-boxed (hw:24+ table area))
  (hw:md-start-write-boxed new-value)
  new-value)

(defsetf area-table-ref area-table-store)

(defmacro def-area-accessor (name table)
  `(PROGN
     (DEFSUBST ,name (AREA)
       (AREA-TABLE-REF ,table AREA))
     (DEFSETF ,name (AREA) (VALUE)
       `(AREA-TABLE-STORE ,',table ,area ,value))))

(def-area-accessor area-region-data gr::*area-region-data*)

(defconstant %%area-data                  (byte
                                              (+ (byte-size quantum-map::%%quantum-map-region-origin) 2.)
                                              0.))
(defconstant %%area-data-region-thread    (byte (byte-size quantum-map::%%quantum-map-region-origin) 0.))
(defconstant %%area-data-area-status      (byte 2. (byte-size quantum-map::%%quantum-map-region-origin)))
(defconstant %%area-data-area-has-regions (byte
                                              1.
                                              (1+ (byte-size quantum-map::%%quantum-map-region-origin))))
;;; unused currently 10 bits.
;;; reserved %%k-data-type

(defconstant $$area-free                 #b00)
(defconstant $$area-allocated-no-regions #b01)
(defconstant $$area-allocated            #b10)
(defconstant $$area-fixed                #b11)

(defconstant $$area-is-empty    0)
(defconstant $$area-has-regions 1)

(vinc::defextractor      area-data-region-thread  %%area-data-region-thread)
(vinc::defextractor      area-data-status         %%area-data-area-status)
(vinc::defflag-extractor area-free?               %%area-data-area-status $$area-free)
(vinc::defflag-extractor area-has-regions?        %%area-data-area-has-regions $$area-has-regions)

(def-area-accessor area-region-bits gr::*area-region-bits*)


(defconstant %%area-region-bits             (byte (+ (byte-size region-bits::%%region-bits) 3.) 0.))
(defconstant %%area-region-bits-the-bits    (byte (byte-size region-bits::%%region-bits) 0.))
(defconstant %%area-region-bits-volatility  (byte 3. (byte-size region-bits::%%region-bits)))
;;; reserved vinc::%%data-type

(vinc::defextractor area-volatility %%area-region-bits-volatility)

(def-area-accessor area-region-size gr::*area-region-size*)

;;; This would be amenable to a binary search, but it probably isn't worth it.
(defun find-free-area ()
  (dotimes (candidate *number-of-areas*)
    (when (area-free? (area-region-data candidate))
      (return-from find-free-area candidate))))

;(defsubst change-area (area thunk)
;  (let ((area-data (area-region-data area))
;       (area-bits (area-region-bits area)))
;    (lisp::macrolet ((md (field var)
;                `(FUNCTION (LAMBDA (MOD)
;                             (SETQ ,var (HW:DPB (FUNCALL MOD (HW:LDB ,var ,field 0)) ,field ,var))))))
;    (funcall thunk
;            (md %%area-data-region-thread area-data)
;            (md %%area-data-area-status   area-data)
;            (md %%area-region-bits-the-bits area-bits)
;            (md %%area-region-bits-volatility area-bits))
;    (setf (area-region-data area) area-data)
;    (setf (area-region-bits area) area-bits))))


;(defun make-area (volatility region-bits recommended-region-size-in-quanta)
;  (let ((area (find-free-area)))
;    (if (null area)
;       (trap::tail-illop "Ran out of areas")
;       (progn
;       (change-area area
;         #'(lambda (thread m-status m-region-bits m-volatility)
;             (funcall m-status #'(lambda (status) $$area-allocated-no-regions))
;             (funcall m-region-bits #'(lambda (bits) region-bits))
;             (funcall m-volatility #'(lambda (vol)  volatility))))
;         (setf (area-region-size area)
;               recommended-region-size-in-quanta)
;         area))))

(defun make-area (volatility region-bits recommended-region-size-in-quanta)
  (let ((area (find-free-area)))
    (if (null area)
        (trap::tail-illop "Ran out of areas")
        (progn
          (setf (area-region-data area)
                (hw:dpb $$area-allocated-no-regions %%area-data-area-status 0.))
          (setf (area-region-bits area)
                (vinc::dpb-multiple-boxed
                  volatility   %%area-region-bits-volatility
                  region-bits  %%area-region-bits-the-bits
                  0))
          (setf (area-region-size area)
                recommended-region-size-in-quanta)
          area))))

(defun make-area-fixed (area)
  (setf (area-region-data area)
        (hw:dpb $$area-fixed %%area-data-area-status (area-region-data area))))

(defun place-region-in-area (region area)
  ;; Put the new region on the area-region-list
  (let ((data (area-region-data area)))
    (dispatch %%area-data-area-status data
      ($$area-free                 (trap::tail-illop "Don't place regions in free areas"))
      ($$area-allocated-no-regions
        (setf (region-list-thread region)
              (hw:dpb $$thread-ends %%region-list-thread-end-flag area))
        (setf (area-region-data area)
              (vinc::dpb-multiple-boxed
                $$area-allocated %%area-data-area-status
                region           %%area-data-region-thread
                data)))
      (($$area-allocated $$area-fixed)
        (setf (region-list-thread region)
              (vinc::dpb-multiple-unboxed
                (area-data-region-thread data)
                       (byte (byte-size quantum-map::%%quantum-map-region-origin) 0)
                $$thread-continues %%region-list-thread-end-flag
                0))
        (setf (area-region-data area)
              (hw:dpb region %%area-data-region-thread data))))
    region))

(defun make-region-in-area
       (area size volatility flippable new-space space-type read-only scavenge-enable swapin-quantum external-bus)
  (let ((region (region-data:make-region size
                                          (region-bits:encode-region-bits
                                            flippable
                                            new-space
                                            space-type
                                            read-only
                                            scavenge-enable
                                            swapin-quantum
                                            external-bus)
                                          volatility)))
    (place-region-in-area region area)))


(defun poor-mans-ceiling (dividend divisor)
  ;; YOU do it right.
  (labels ((pmc-internal (counter answer)
             (if (<= counter 0)
                 answer
                 (pmc-internal (- counter divisor) (1+ answer)))))
    (pmc-internal dividend 0)))

(defun volatility-acceptable? (region-volatility target-volatility new-or-copy)
  ;; Here it is!  This decides what volatility to move old stuff to.
  ;; The region-volatility is the volatility of the region we are considereing
  ;; consing in.  The target volatility is the volatility of the place we are
  ;; coming from.  If we are consing in newspace, they must match, if copyspace,
  ;; then the new volatility must be 1 less than the old unless it is 1.

  (if (= new-or-copy region-bits:$$region-new-space)
      (= region-volatility target-volatility)
      (or (and (= target-volatility 1.)
               (= region-volatility 1.))
          (= (- target-volatility 1.) region-volatility))))

(defun region-acceptable? (candidate-region volatility-we-want space-type new-or-copy words-needed)
  (let* ((region-bits  (region-bits:read-region-bits candidate-region))
         (region-volatility-and-oldspace
           (gc-ram:quantum-volatility-and-oldspace candidate-region))
         (region-volatility (hw:ldb region-volatility-and-oldspace
                                    hw:%%gc-ram-quantum-volatility 0.))
         (region-oldspace   (hw:ldb region-volatility-and-oldspace
                                    hw:%%gc-ram-quantum-oldspace 0.)))
    (and (not (= region-oldspace hw:$$oldspace))
         (= new-or-copy (region-bits:region-copy-space region-bits))
         (= space-type (region-bits:region-space-type region-bits))
         (volatility-acceptable? region-volatility volatility-we-want new-or-copy)
         (not (region-bits:region-read-only?  region-bits))
         (hw:32>
           (hw:32-
             (hw:dpb-unboxed (region-data:region-end candidate-region)
                             vinc:%%pointer (hw:unboxed-constant 0))
             (hw:dpb-unboxed (region-data:region-free-pointer candidate-region)
                             vinc:%%pointer (hw:unboxed-constant 0)))
           (hw:dpb-unboxed words-needed vinc:%%fixnum-field (hw:unboxed-constant 0))))))

(defun get-active-region (area space-type new-or-copy volatility words-needed)
  (let* ((area-data          (area-region-data area))
         (area-status        (area-data-status area-data))
         (area-bits          (area-region-bits area))
         (area-volatility    (area-volatility area-bits))
         (quanta-needed      (poor-mans-ceiling words-needed vinc:*qs-in-quantum*))
         (volatility-we-want (if (= new-or-copy region-bits:$$region-new-space)
                                 area-volatility
                                 volatility))
         (the-region
           (labels ((maybe-make-region ()
                      (if (or (= $$area-free  area-status)
                              (= $$area-fixed area-status))
                          ;; Can't make a region here, use desparation area,
                          ;; unless we are failing on that already.
                          (if (= area gr::*desperate-consing-area*)
                              (trap::tail-illop "Desperate consing area broken.")
                              (get-active-region gr::*desperate-consing-area*
                                                 space-type new-or-copy volatility words-needed))
                          (let* ((default-region-bits (area-region-bits       area))
                                 (size                (area-region-size       area)))
                            (make-region-in-area
                              area
                              (if (> quanta-needed size) quanta-needed size)
                              volatility-we-want
                              (region-bits:region-flippable default-region-bits)
                              new-or-copy
                              space-type
                              region-bits:$$region-read-write
                              (region-bits:region-scavenge-enable default-region-bits)
                              (region-bits:region-swapin-quantum  default-region-bits)
                              (region-bits:region-external-bus    default-region-bits))))))
             (if (area-has-regions? area-data)
                 (labels ((find-active-region (candidate-region)
                            (if (region-acceptable? candidate-region volatility-we-want space-type
                                                    new-or-copy words-needed)
                                candidate-region
                                (let* ((thread (region-list-thread candidate-region))
                                       (ends   (hw:ldb thread
                                                       %%region-list-thread-end-flag 0))
                                       (next   (hw:ldb thread
                                                       %%region-list-thread-next-region 0)))
                                  (if (= $$thread-ends ends)
                                      ;; we ran out of regions in this area
                                      (progn
                                        ;; check for lossage
                                        (when (not (= next area))
                                          (trap::tail-illop "Region list thread corrupted."))
                                        (maybe-make-region))
                                      ;; Try the next one
                                      (find-active-region next))))))
                   (find-active-region (area-data-region-thread area-data)))
                 (maybe-make-region)))))
         (maybe-load-cons-cache area the-region new-or-copy space-type)
         the-region))

(defun maybe-load-cons-cache (area region new-or-copy space-type)
  (when (= new-or-copy region-bits:$$region-new-space)
    (cond ((= space-type region-bits:$$region-space-cons)
           (if (= gr::*cons-cache-region* region)
               (trap::tail-illop "Error in cons cache")
             (progn
               ;; Flush the cache
               (setf (region-data:unsafe-region-free-pointer gr::*cons-cache-region*)
                     gr::*cons-cache-free*)
               ;; Load up the cache
               (setq gr::*cons-cache-area* area)
               (setq gr::*cons-cache-region* region)
               (setq gr::*cons-cache-free*
                     (hw:dpb-boxed (region-data:unsafe-region-free-pointer region)
                                   vinc:%%pointer gr:*dtp-locative*))
               (setq gr::*cons-cache-limit* (region-data:region-end region)))))
          ((= space-type region-bits:$$region-space-structure)
           (if (= gr::*structure-cons-cache-region* region)
               (trap::tail-illop "Error in the cons cache.")
             (progn
               ;; Flush the cache
               (setf (region-data:unsafe-region-free-pointer gr::*structure-cons-cache-region*)
                     gr::*structure-cons-cache-free*)
               ;; Load up the cache
               (setq gr::*structure-cons-cache-area* area)
               (setq gr::*structure-cons-cache-region* region)
               (setq gr::*structure-cons-cache-free*
                         (hw:dpb-boxed (region-data:unsafe-region-free-pointer region)
                                       vinc:%%pointer gr:*dtp-locative*))
               (setq gr::*structure-cons-cache-limit* (region-data:region-end region))))))))



;(defun make-region-in-area (area space-type size-in-quanta volatility
;                            scavenge-enable read-only swapin-quantum)
;  (let ((suggested-bits (area-region-bits area)))
;    (let ((real-bits
;           (region-bits::parameters->region-bits
;             (if (= space-type region-bits:$$region-space-free)
;                 (illop "Don't make new regions be type free.")
;                 space-type)
;             (or scavenge-enable (region-bits:region-scavenge-enable suggested-bits))
;             (or read-only       (region-bits:region-read-only       suggested-bits))
;             (or swapin-quantum  (region-bits:region-swapin-quantum  suggested-bits))))
;         (volatility (or volatility (hw:ldb suggested-bits %%area-region-bits-volatility 0.)))
;         (size       (or size-in-quanta (area-region-size area))))
;      (let ((region (region-data:make-region size real-bits volatility)))
;       (place-region-in-area region area)))))

;;;; Find a region in AREA which contains enough space to allocate WORDS-NEEDED
;;;; creating one if none exists
;(defun get-active-region (area words-needed)
;  (let ((area-data (area-region-data area)))
;    (if (= $$area-allocated (hw:ldb area-data %%area-data-area-status 0))
;       (do ((region (hw:ldb area-data %%area-region-data-thread 0)
;                    (region-list-thread region)))
;           ((hw:32logbitp (byte-position %%region-list-thread-end-flag) region)
;            (make-region-in-area area ?? ?? ?? ?? ?? ??))
;         (when (hw:32>= (hw:32- (region-end region) (region-free-pointer region))
;                        words-needed)
;           (return region)))
;      (make-region-in-area area ?? ?? ?? ?? ?? ??))))

(defun zap-all-areas ()
  (dotimes (area *number-of-areas*)
    (setf (area-region-data area)
          (vinc::dpb-multiple-boxed
            0.           %%area-data-region-thread
            $$area-free  %%area-data-area-status
            0.))
    ;; The following are meaningless.
    (setf (area-region-size area) 0.)
    (setf (area-region-bits area) 0.)))


(defun initialize-area (region region-bits)
  (let ((area-number (make-area 0. region-bits 1.)))
    (place-region-in-area region area-number)
    (setf (area-region-data area-number)
          (hw:dpb $$area-fixed %%area-data-area-status
                  (area-region-data area-number)))))

(defun initialize-area-data ()
  ;; Grovel throught the region tables making an area around each region found.
  (zap-all-areas)
  (dotimes (region *number-of-regions*)
    (let* ((region-bits (region-bits:read-region-bits region))
           (space-type (region-bits:region-space-type region-bits)))
      (when (not (or (= space-type region-bits:$$region-space-free)
                     (= space-type region-bits:$$region-space-invalid)))
        (initialize-area region region-bits)))))

(defun reset-temporary-area (area)
  (let ((data (area-region-data area)))
    (when (area-has-regions? data)
      (labels ((toss-out-region (region)
                 (region-data::free-region region)
                 (let ((thread (region-list-thread region)))
                   (if (thread-continues? thread)
                       (toss-out-region (hw:ldb thread %%region-list-thread-next-region 0))
                       nil))))
        (toss-out-region (hw:ldb data %%area-data-region-thread 0)))
      (setf (area-region-data area) (hw:dpb $$area-allocated-no-regions
                                            %%area-data-area-status
                                            data)))))

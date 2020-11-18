;;; -*- Mode:LISP; Package:REGION-DATA; Base:10.; Readtable:CL -*-

;(in-package 'region-data)

(export '(
          make-region
          def-region-accessor
          free-region
          initialize-region-data
          region-allocation-status
          region-end
          region-free-pointer
          region-gc-pointer
          region-origin
          unsafe-region-free-pointer
          ))

;(defsubst region-free-pointer-unsafe (region)
;  (region-table-ref gr::*region-free-pointer* region))

(defmacro def-region-accessor (name table)
  `(PROGN
     ;; This should be a defsubst, but macroexpansion and defsetf happen in
     ;; a bad order.  This causes us to try to setf REGION-TABLE-REF which
     ;; has no setf method.
     (DEFUN ,name (REGION)
       (REGION-TABLE-REF ,table REGION))
     (DEFSETF ,name (REGION) (VALUE)
       `(REGION-TABLE-STORE ,',table ,region ,value))))

(def-region-accessor unsafe-region-free-pointer gr::*region-free-pointer*)
(def-region-accessor region-end                 gr::*region-end*)
(def-region-accessor region-gc-pointer          gr::*region-gc-pointer*)

(defun region-free-pointer (region)
  (cond ((= region gr::*cons-cache-region*)                     gr::*cons-cache-free*)
        ((= region gr::*structure-cons-cache-region*)           gr::*structure-cons-cache-free*)
;       ((= region gr::*copy-cons-cache-region*)                gr::*copy-cons-cache-free*)
;       ((= region gr::*copy-structure-cons-cache-region*)      gr::*copy-structure-cons-cache-free*)
        (t (unsafe-region-free-pointer region))))

(defun set-region-free-pointer (region value)
  (cond ((= region gr::*cons-cache-region*)
         (setq gr::*cons-cache-free*         value))
        ((= region gr::*structure-cons-cache-region*)
         (setq gr::*structure-cons-cache-free* value))
;       ((= region gr::*copy-cons-cache-region*)
;        (setq gr::*copy-cons-cache-free* value))
;       ((= region gr::*copy-structure-cons-cache-region*)
;        (setq gr::*copy-structure-cons-cache-free* value))
        )
  (setf (unsafe-region-free-pointer region) value))

(defsetf region-free-pointer set-region-free-pointer)


(defun cons-cache-invalid? ()
  (= gr:*cons-cache-region* -1))

(defun structure-cons-cache-invalid? ()
  (= gr:*structure-cons-cache-region* -1))

(defun invalidate-cons-cache ()
  (unless (cons-cache-invalid?)
    (setf (unsafe-region-free-pointer gr:*cons-cache-region*) gr:*cons-cache-free*))
  (setq gr:*cons-cache-region* -1)
  (setq gr:*cons-cache-free*  (hw:32- trap:*magic-garbage-location* 1))
;  (setq gr:*cons-cache-limit*   -1)
  )

(defun invalidate-structure-cons-cache ()
  (unless (structure-cons-cache-invalid?)
    (setf (unsafe-region-free-pointer gr:*structure-cons-cache-region*) gr:*structure-cons-cache-free*))
  (setq gr:*structure-cons-cache-region* -1)
  (setq gr:*structure-cons-cache-free*  (hw:32- trap:*magic-garbage-location* 1))
  (setq gr:*structure-cons-cache-limit* gr:*structure-cons-cache-free*))

(defun make-region (size region-bits volatility)
  (let* ((region (region-bits:make-region size region-bits volatility))
         (origin (region-origin region)))
    ;; Regions start out empty.
    (setf (region-free-pointer      region) origin)
    (setf (region-end               region) (hw:24+ (quantum->address size) origin))
    (setf (region-gc-pointer        region) origin)
    region))

(defun free-region (region)
;  (trap::illop "freeing region")
  (region-bits:free-region region)
  (when (= region gr:*cons-cache-region*)
      (invalidate-cons-cache))
  (let ((region-start (region-origin region)))
    (setf (region-free-pointer region)        region-start)
    (setf (region-end region)                 region-start)
    (setf (region-gc-pointer region)          region-start)))

(defun advance-free-pointer (region how-far)
  (let ((free-pointer (region-free-pointer region))
        (end          (region-end          region)))
    (let ((new-pointer (hw:32+ (hw:ldb how-far vinc:%%pointer (hw:unboxed-constant 0)) free-pointer)))
      (if (hw:32>
            (hw:ldb new-pointer vinc:%%pointer (hw:unboxed-constant 0))
            (hw:ldb end         vinc:%%pointer (hw:unboxed-constant 0)))
          (trap::illop "Advanced free pointer beyond end of region.")
          (setf (region-free-pointer region) new-pointer)))))

(defun zap-all-regions (count)
  (if (= count *number-of-regions*)
      '()
      (progn (setf (region-free-pointer count) (hw:unboxed-constant 0))
             (setf (region-gc-pointer   count) (hw:unboxed-constant 0))
             (setf (region-end          count) (hw:unboxed-constant 0))
             (zap-all-regions (1+ count)))))

(defun find-region (scan)
  (cond ((= scan *number-of-regions*) '())
        ((not (quantum-map:valid-quantum? scan)) (find-region (1+ scan)))
        (t  (accumulate-region-data scan (1+ scan)))))

(defun accumulate-region-data (start scan)
  (if (= scan *number-of-regions*)
      (record-initial-region-data start scan)
      (let ((space-type (region-bits:region-space-type (region-bits:read-region-bits scan))))
        (if (= space-type region-bits:$$region-space-invalid)
            (accumulate-region-data start (1+ scan))
            (progn (record-initial-region-data start scan)
                   (if (= space-type region-bits:$$region-space-free)
                       (find-region (1+ scan))
                       (accumulate-region-data scan (1+ scan))))))))

(defun record-initial-region-data (begin end)
  (setf (region-free-pointer begin) (quantum->address end))
  (setf (region-gc-pointer   begin) (quantum->address begin))
  (setf (region-end          begin) (quantum->address end)))

(defun initialize-region-data ()
  (zap-all-regions 0)
  (find-region 0)
  nil)


;;; This is what I would like to use, but the compiler can't hack it yet.
;(defun initialize-region-data ()
;  ;; We scan the region bits table to find regions.  Each region
;  ;; is presumed to be full.
;  (labels ((zap-all-regions (count)
;            (if (= count *number-of-regions*)
;                '()
;                (progn (setf (region-free-pointer count) (hw:unboxed-constant 0))
;                       (setf (region-gc-pointer   count) (hw:unboxed-constant 0))
;                       (setf (region-end          count) (hw:unboxed-constant 0))
;                       (zap-all-regions (1+ count)))))

;          (find-region (scan)
;            (cond ((= scan *number-of-regions*) '())
;                  ((not (quantum-map:valid-quantum? scan)) (find-region (1+ scan)))
;                  (t  (accumulate-region-data scan (1+ scan)))))

;          (accumulate-region-data (start scan)
;            (if (= scan *number-of-regions*)
;                (record-initial-region-data start scan)
;                (let ((space-type (region-bits:region-space-type (region-bits:read-region-bits scan))))
;                  (if (= space-type region-bits:$$region-space-invalid)
;                      (accumulate-region-data start (1+ scan))
;                      (progn (record-initial-region-data start scan)
;                             (if (= space-type region-bits:$$region-space-free)
;                                 (find-region (1+ scan))
;                                 (accumulate-region-data scan (1+ scan))))))))

;          (record-initial-region-data (begin end)
;            (setf (region-free-pointer begin) (quantum->address end))
;            (setf (region-gc-pointer   begin) (hw:unboxed-constant 0))
;            (setf (region-end          begin) (quantum->address end)))

;          )
;    (zap-all-regions 0)
;    (find-region 0)
;    nil))
                                                                                                ;

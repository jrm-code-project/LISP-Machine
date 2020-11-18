;;; -*- Mode:LISP; Package:GC-FAULT; Readtable:CL; Base:10 -*-

;;; Atomic pointer 26 bit math

(defun ptr-inc (ptr &optional (how-much 1))
  (hw:nop)
  (trap:without-traps
    #'(lambda ()
        (hw:dpb (hw:32+ ptr how-much) vinc:%%pointer ptr))))

(defun ptr-dec (ptr &optional (how-much 1))
  (hw:nop)
  (trap:without-traps
    #'(lambda ()
        (hw:dpb (hw:32- ptr how-much) vinc:%%pointer ptr))))

(defun ptr-inc+1 (ptr &optional (how-much 1))
  (hw:nop)
  (trap:without-traps
    #'(lambda ()
        (hw:dpb (hw:left+right+1-bw32 ptr how-much) vinc:%%pointer ptr))))

;;; Atomic pointer field (26 bit) compares

(defun ptr->= (p1 p2)
  (hw:nop)
  (trap:without-traps
   #'(lambda ()
       (hw:32>=
        (hw:ldb p1 vinc:%%pointer (hw:unboxed-constant 0))
        (hw:ldb p2 vinc:%%pointer (hw:unboxed-constant 0))))))

(defun ptr-<= (p1 p2)
  (hw:nop)
  (trap:without-traps
   #'(lambda ()
       (hw:32<=
        (hw:ldb p1 vinc:%%pointer (hw:unboxed-constant 0))
        (hw:ldb p2 vinc:%%pointer (hw:unboxed-constant 0))))))

(defun ptr-< (p1 p2)
  (hw:nop)
  (trap:without-traps
   #'(lambda ()
       (hw:32<
        (hw:ldb p1 vinc:%%pointer (hw:unboxed-constant 0))
        (hw:ldb p2 vinc:%%pointer (hw:unboxed-constant 0))))))

(defun ptr-> (p1 p2)
  (hw:nop)
  (trap:without-traps
   #'(lambda ()
       (hw:32>
        (hw:ldb p1 vinc:%%pointer (hw:unboxed-constant 0))
        (hw:ldb p2 vinc:%%pointer (hw:unboxed-constant 0))))))

(defun ptr-= (p1 p2)
  (hw:nop)
  (trap:without-traps
   #'(lambda ()
       (hw:32=
        (hw:ldb p1 vinc:%%pointer (hw:unboxed-constant 0))
        (hw:ldb p2 vinc:%%pointer (hw:unboxed-constant 0))))))

(defun ptr-/= (p1 p2)
  (hw:nop)
  (trap:without-traps
   #'(lambda ()
       (not (hw:32/=
             (hw:ldb p1 vinc:%%pointer (hw:unboxed-constant 0))
             (hw:ldb p2 vinc:%%pointer (hw:unboxed-constant 0)))))))

;;; Return a pointer after taking any transporter traps that its going to take.

(defun transport-ptr (ptr)
  (hw:vma-start-read-vma-boxed-md-boxed ptr)
  (hw:nop)
  (hw:read-md)
  (hw:read-vma))

;;; Based on the kind of header that a pointer points to, change the datatype
;;; of the pointer to something appropriate.

(defun create-structure-handle (ptr header)
  (dispatch vinc:%%data-type header
    ((vinc:$$dtp-array-header-single vinc:$$dtp-array-header-multiple)
       (cons:make-pointer vinc:$$dtp-array ptr))
    (vinc:$$dtp-array-header-extension
      (cons:make-pointer vinc:$$dtp-array (hw:24+ header (transport ptr))))
    (vinc:$$dtp-unboxed-header
      (cons:make-pointer vinc:$$dtp-bignum ptr))
    (vinc:$$dtp-symbol-header
      (cons:make-pointer vinc:$$dtp-symbol ptr))
    (vinc:$$dtp-compiled-function-header
      (cons:make-pointer vinc:$$dtp-compiled-function ptr))
    (vinc:$$dtp-structure-header
      (cons:make-pointer vinc:$$dtp-structure ptr))
    (vinc:$$dtp-hash-table-header
      (cons:make-pointer vinc:$$dtp-hash-table ptr))
    (t (error "Can't create a structure handle for this." ptr header))))

;;; Given a pointer to a structure handle in structure space, return a pointer to the
;;; next structure in structure space

(defun advance-to-next-structure (ptr)
  (setq ptr (cons:make-pointer vinc:$$dtp-locative ptr))
  (let ((header (cons:contents ptr)))
    (dispatch vinc:%%data-type header
      ((vinc:$$dtp-unboxed-structure vinc:$$dtp-structure-header vinc:$$dtp-hash-table-header)
       (ptr-inc+1 ptr header))
      (vinc:$$dtp-symbol-header (ptr-inc ptr symbol:*symbol-size*))
      (vinc:$$dtp-array-header-extension
        (let ((hptr (hw:24+ header ptr)))
          (ptr-inc+1 hptr (array:array-local-data-size hptr))))
      ((vinc:$$dtp-array-header-single vinc:$$dtp-array-header-multiple)
       (ptr-inc+1 ptr (array:array-data-local-size ptr)))
      (t (li:error "Can't advance to next structure from this pointer" ptr)))))

(defun find-handle-using-table (ptr)
  (macrolet ((gen-ptr (cluster offset)
                      (cons:make-pointer vinc:$$dtp-locative
                                         (hw:dpb cluster vinc:%%cluster-number
                                                 (hw:ldb offset vinc:%%offset-in-cluster (hw:unboxed-constant 0))))))
    (setq ptr (hw:ldb ptr vinc:%%pointer (hw:unboxed-constant 0)))
    (let* ((ptr-cluster-number (hw:ldb ptr vinc:%%cluster-number    0))
           (ptr-cluster-offset (hw:ldb ptr vinc:%%offset-in-cluster 0))
           (prev-cluster-number ptr-cluster-number)
           (prev-cluster-offset (cons:structure-handle-first-cluster prev-cluster-number))
           (do () ((or (= prev-cluster-offset cons:*no-first-header-code*)
                       (ptr->= ptr (gen-ptr prev-cluster-number prev-cluster-offset))))
             (setq prev-cluster-number (1- prev-cluster-number))
             (setq prev-cluster-offset (cons:structure-handle-first-cluster prev-cluster-number)))
           (do* ((prev-ptr (gen-ptr prev-cluster-number pref-cluster-offset) next-ptr)
                 (next-ptr (advance-to-next-structure prev-ptr) (advance-to-next-structure next-ptr)))
                ((and (ptr-<= prev-ptr ptr) (ptr-<= ptr next-ptr)) (create-structure-handle prev-ptr)))
           ))))

;;; Follow a pointer to the beginning of the structure it points to.
;;; Easy for most, hairy for $$DTP-LOCATIVE and $$DTP-UNBOXED-LOCATIVE

(defun get-structure-handle-from-pointer (ptr)
  (dispatch vinc:%%data-type ptr
    (vinc:$$dtp-locative
     (let* ((ptr-cluster-number (vinc:cluster-number ptr))
            (ptr-region-bits    (region-bits:cluster-region-bits ptr-cluster-number)))
       (dispatch region-bits:%%region-bits-space-type ptr-region-bits
          (region-bits:$$region-space-structure
            (do* ((p ptr (hw:24-1- p))
                  (data (cons:contents p) (cons:contents p)))
                 ((header-data-type-p data) (create-structure-handle p data))))
          (region-bits:$$region-space-cons
            (cons:make-pointer vinc:$$dtp-cons (hw:dpb 0 (byte 1 0) ptr)))
          (t (error "Locatives not allowed to point into regions of this type" ptr ptr-region-bits)))))
    (vinc:$$dtp-unboxed-locative
      (let* ((ptr-cluster-number (vinc:cluster-number ptr))
             (ptr-region-bits    (region-bits:cluster-region-bits ptr-cluster-number)))
        (dispatch region-bits:%%region-bits-space-type ptr-region-bits
          (region-bits:$$region-space-cons
            (cons:make-pointer vinc:$$dtp-cons (hw:dpb 0 (byte 1 0) ptr)))
          (region-bits:$$region-space-structure
            (find-handle-using-table ptr))
          (region-bits:$$region-space-code
            (do ((p (hw:dpb 0 (byte 1 0) ptr) (hw:24+ -2 p)))
                ((hw:32= cons:code-header-instruction-high
                         (array:%vm-read32 p 1))
                 p)))
          (t (li:error "Unboxed locatives no allowed in this region type" ptr ptr-region-bits)))))
    (t (li:error "Can't get structure handle for this" ptr))))

;;; -*- Mode:LISP; Package:CONS; Readtable:CL; Base:10 -*-

;;;; Storage Allocation

(export '(
          allocate-code-space
          allocate-structure
          allocate-structure-in-area
          caaaar
          caaadr
          caaar
          caadar
          caaddr
          caadr
          caar
          cadaar
          cadadr
          cadar
          caddar
          cadddr
          caddr
          cadr
          car
          cdaaar
          cdaadr
          cdaar
          cdadar
          cdaddr
          cdadr
          cdar
          cddaar
          cddadr
          cddar
          cdddar
          cddddr
          cdddr
          cddr
          cdr
          cons
          cons-in-area
          contents
          contents-offset
          endp
          make-header
          make-pointer
          rplaca
          rplacd
          set-car
          set-cdr
          set-default-cons-area
          set-default-structure-cons-area
          store-contents
          store-contents-offset
          ))


;;;; Low level storage allocation and storage conventions primitives

(defmacro make-pointer (data-type pointer)
  `(hw:dpb-boxed ,data-type vinc:%%data-type ,pointer))

;;; the header can be boxed and in the registers or md
;;; headers in dumped call stacks are ignored
(defmacro make-header (header-type header-data)
  `(hw:dpb-boxed ,header-type vinc:%%data-type ,header-data))

(defmacro contents (pointer)
  `(progn
     (hw:vma-start-read-vma-boxed-md-boxed ,pointer)
     (hw:read-md)))

(defmacro contents-offset (pointer offset)
  `(progn
     (hw:vma-start-read-vma-unboxed-md-boxed (hw:24+ ,offset ,pointer))
     (hw:read-md)))

(defun store-contents (ptr value)
  (%store-contents ptr value))

(defun store-contents-offset (ptr offset value)
  (%store-contents-offset ptr offset value))

;;;; Cons Mutators

(defun set-car (list value)
  (if (vinc:consp list)
      (%set-car list value value)
    (li:error "Not a cons to set-car" list)))

(defun rplaca (list value)
  (if (vinc:consp list)
      (%set-car list value list)
    (li:error "Not a cons to rplaca" list)))

(defun set-cdr (list value)
  (if (vinc:consp list)
      (%set-cdr list value value)
    (li:error "Not a cons to set-cdr" list)))

(defun rplacd (list value)
  (if (vinc:consp list)
      (%set-cdr list value list)
    (li:error "Not a cons to rplacd" list)))

;;;; CAR and CDR

(eval-when (compile load eval)

user:(defun make-c.n.r (length &optional (suffix "") (guts 'list))
       (if (= length 0)
           `(CONS::DEFUN ,(lisp:intern (lisp:format nil "C~aR" suffix) 'CONS) (list)
              ,guts)
           `(CONS::PROGN ,(make-c.n.r (1- length) (lisp:concatenate 'lisp:string "A" suffix)
                                      `(CONS::%CAR ,guts))
                         ,(make-c.n.r (1- length) (lisp:concatenate 'lisp:string "D" suffix)
                                      `(CONS::%CDR ,guts)))))

user::(prims:defmacro cons::def-c...r (number)
  (let ((forms '()))
    (lisp:dotimes (i number)
      (lisp:push (make-c.n.r (1+ i))
                 forms))
    `(CONS::PROGN ,@(lisp:nreverse forms))))

)

;;; They're all in here:
(def-c...r 4)

(defsetf car set-car)
(defsetf cdr set-cdr)

;;;; Structure Handles

;;; The structure handles record, for each cluster:
;;;   - The number of boxed q's at the beginning of this cluster which
;;;     are left over from structures which start before this cluster.
;;;   - The offset in the cluster of the first header. ( if there is
;;;     no header in this cluster)
;;;
;;; The structure handles allow us to scavenge the cluster and find headers
;;; of pointers into the cluster without looking at (ie paging in) the
;;; previous cluster.

(defconstant %%structure-handle-first-header (byte 11.  0.))
(defconstant %%structure-handle-boxed-qs     (byte 11. 11.))

(defconstant *no-first-header-code* #b11111111111)

(defsubst write-structure-handles (cluster first-header left-over-boxed-qs)
  (hw:write-md-boxed (hw:dpb left-over-boxed-qs %%structure-handle-boxed-qs first-header))
  (hw:vma-start-write-boxed (hw:24+ gr::*structure-handles* cluster)))

(defsubst read-structure-handles (cluster thunk)
  (hw:vma-start-read (hw:24+ gr::*structure-handles* cluster))
  (let ((stuff (hw:read-md)))
    (funcall thunk (hw:ldb stuff %%structure-handle-first-header 0)
            (hw:ldb stuff     %%structure-handle-boxed-qs 0))))

(defsubst modify-structure-handles (cluster thunk)
  ;; This is dangerous because it assumes that the vma
  ;; remains unchanged for the computation of the new value.
  (read-structure-handles cluster
    #'(lambda (first-header boxed-qs)
        (funcall thunk first-header boxed-qs
                 #'(lambda (new-first-header new-boxed-qs)
                     (hw:md-start-write-boxed
                       (hw:dpb new-boxed-qs %%structure-handle-boxed-qs new-first-header)))))))

(defsubst structure-handle-first-header (cluster)
  (read-structure-handles cluster
    #'(lambda (first-header boxed-qs) boxed-qs first-header)))

(defsubst structure-handle-boxed-qs (cluster)
  (read-structure-handles cluster
    #'(lambda (first-header boxed-qs) first-header boxed-qs)))

(defsubst write-structure-handle-boxed-qs (cluster new-value)
  (modify-structure-handles cluster
    #'(lambda (first-header boxed-qs writer)
        first-header
        (funcall writer new-value boxed-qs))))

(defsubst write-structure-handle-first-header (cluster new-value)
  (modify-structure-handles cluster
    #'(lambda (first-header boxed-qs writer)
        boxed-qs
        (funcall writer first-header new-value))))

(defsetf structure-handle-first-header write-structure-handle-first-header)
(defsetf structure-handle-boxed-qs     write-structure-handle-boxed-qs)

;(defun foo (x)
;  (structure-handle-first-header x))


(defconstant *structure-handles-quanta* (lisp:ceiling vinc:*number-of-virtual-clusters*
                                                      vinc:*qs-in-quantum*))

(defvar *structure-handles-area* nil)   ;keep this around!

(defun initialize-structure-handles ()
  (let* ((structure-handles-area
           (make-area 0
                     (encode-region-bits
                       region-bits:$$region-fixed
                       region-bits:$$region-new-space
                       region-bits:$$region-space-cons
                       region-bits:$$region-read-write
                       region-bits:$$scavenge-disabled
                       region-bits:$$region-internal-memory
                       0.)
                     0))
         (structure-handles-region
           (progn
             (check-nil "Made structure-handles-area")
             (make-region-in-area
               structure-handles-area
               *structure-handles-quanta*
               0
               region-bits:$$region-fixed
               region-bits:$$region-new-space
               region-bits:$$region-space-cons
               region-bits:$$region-read-write
               region-bits:$$scavenge-disabled
               region-bits:$$region-internal-memory
               0))))
    (check-nil "Made region in structure-handles-area")
    (setq gr::*structure-handles* (quantum->address structure-handles-region))
    (check-nil "Set the GR")
    (make-area-fixed structure-handles-area)
    (check-nil "Made structure-handles-area fixed")
    (dotimes (count vinc:*number-of-virtual-clusters*)
      (write-structure-handles count 0 0)
      (check-nil "Wrote a structure handle"))
    (setq *structure-handles-area* structure-handles-area)
    (check-nil "Did a SETQ")))

;;;; ENDP
(defafun endp (l)
  (move nop a0 dt-right-list bw-32)
  (test br-zero)
  (branch end ())
  (return gr:*NIL*)
 end
  (return gr:*T*))


;;;; Cons

(defafun cons (car cdr)
  ;; Turn off sequence breaks to avoid other processes consing after we
  ;; move the free pointer.
  ;; Check for a region in the cache.
  ;; Write the cdr at one beyond the free pointer.
  ;;  Note: If you change this, you must also decide how to invalidate the cons cache.
  ;;  Look in REGION-DATA for details.
  (alu r+1 gr::*allow-sequence-break* ignore gr::*allow-sequence-break* bw-24 boxed-right)

 try-again
  (move md a1 boxed-md) ; maybe pass cdr in md
  (alu r+1 vma-start-write ignore gr::*cons-cache-free* boxed-vma)

  ;; Bump the free pointer by 2.
  (alu r+2 gr::*cons-cache-free* ignore gr::*cons-cache-free* boxed)

  ;; Check to see if the cons cache was empty.
  (alu r+1 nop ignore gr:*cons-cache-region* bw-24)

  ;; Scavenge, and check for region end at the end of each cluster.
  (alu-field field-pass nop gr::*cons-cache-free* gr::*cons-cache-free*
             #.(byte (byte-size vinc:%%offset-in-cluster)
                     (- (byte-position vinc:%%offset-in-cluster))) br-zero)


  (branch cons-cache-empty
          (alu r-2 vma ignore gr::*cons-cache-free* br-zero boxed-vma))

  (branch cluster-full (move md-start-write a0 boxed-md))

  ;; Sequence breaks come back on, and we return the cons cell apropriately typed.
  (alu r-1 gr::*allow-sequence-break* ignore gr::*allow-sequence-break* boxed-right)
  (alu-field aligned-field-pass-left return gr::*dtp-cons* vma vinc:%%data-type ch-return next-pc-return boxed)

 cluster-full
  ;; Make the cons cell and call "cons new cluster"
  (nop) ;wait for the vma to load

  (alu xor nop gr::*cons-cache-limit* gr:*cons-cache-free* bw-24 unboxed)
  (alu-field aligned-field-pass-left a2 gr::*dtp-cons* vma vinc:%%data-type boxed br-not-zero)
  (branch exit-cluster-full ())

  (open-call (cons-new-cluster 1) r0 (o0 gr::*cons-cache-area*))

 exit-cluster-full
  ;; Sequence breaks come back on.
  (alu r-1 gr::*allow-sequence-break* ignore gr::*allow-sequence-break* bw-24 boxed-right)
  (return a2 boxed)

 cons-cache-empty
  (open-call (load-cons-cache 0) r0 nil)
  (unconditional-branch try-again ())

  )

(defun load-cons-cache ()
  (load-cons-cache-for-area gr:*cons-cache-area*))

;;; Get a new cluster to cons in, check if we are at end
;;; of region and if so, get a new region and set up cons cache.
;;; Do some scavenging
;;; This should be called with sequence breaks off
(defun cons-new-cluster (area)
;  (unless (zerop gr::*scavenge-work-while-consing*)
;    (gc:scavenge-while-consing))
  (when (hw:32>=
          (hw:ldb gr::*cons-cache-free* vinc:%%pointer (hw:unboxed-constant 0))
          (hw:ldb
            (region-data:region-end gr::*cons-cache-region*) vinc:%%pointer (hw:unboxed-constant 0)))
    (load-cons-cache-for-area gr::*cons-cache-area*)))

(defun set-default-cons-area (area)
  (unless (= area gr::*cons-cache-area*)
    (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
    (setq gr::*default-consing-area* area)
    (load-cons-cache-for-area area)
    (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))))

(defun load-cons-cache-for-area (area)
  (get-active-region
    area
    region-bits:$$region-space-cons
    region-bits:$$region-new-space
    nil
    2))

(defun cons-in-area (car cdr area)
  (if (= area gr::*cons-cache-area*)
      (cons car cdr)
      (progn
        (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
        (let ((current-cache-area gr::*cons-cache-area*))
          (load-cons-cache-for-area area)
          (prog1 (cons car cdr)
                 (load-cons-cache-for-area current-cache-area)
                 (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*)))))))

;;;; Structure Consing

(defun allocate-structure (boxed-qs unboxed-qs data-type header)
  (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))

  (let ((qs-needed (hw:ldb (+ boxed-qs unboxed-qs) vinc:%%fixnum-field 0)))
    (labels ((try-again ()
               (let ((new-free (hw:32+ (hw:ldb qs-needed vinc:%%fixnum-field (hw:unboxed-constant 0.))
                                       gr::*structure-cons-cache-free*)))
                 (if (or (region-data::structure-cons-cache-invalid?)
                         (hw:32>=
                           (hw:dpb-unboxed new-free vinc:%%pointer (hw:unboxed-constant 0))
                           (hw:dpb-unboxed gr::*structure-cons-cache-limit* vinc:%%pointer
                                           (hw:unboxed-constant 0))))
                     (progn (load-structure-cons-cache-for-area
                              gr::*structure-cons-cache-area*
                              qs-needed)
                            (try-again))
                     (let ((pointer (make-pointer data-type gr::*structure-cons-cache-free*))
                           (end-cluster (cluster-number new-free)))
                       ;; Store the header.
                       (hw::write-md-unboxed header)
                       (hw::vma-start-write-no-gc-trap-unboxed pointer)
                       (let ((clusters-consed
                               (if (not (hw:field= pointer new-free vinc:%%cluster-number))
                                   ;; The cluster the object starts on is guaranteed to be set up correctly,
                                   ;; if we walk off the end of a cluster, we have to update structure
                                   ;; handles
                                   ;; accordingly
                                   (labels ((update-structure-handles (cluster-count cluster-scan
                                                                                     boxed-qs)
                                              (cond ((= cluster-scan end-cluster)
                                                     (write-structure-handles cluster-scan
                                                       (hw:ldb new-free vinc:%%offset-in-cluster 0)
                                                       boxed-qs)
                                                     cluster-count)
                                                    ((> boxed-qs vinc:*qs-in-cluster*)
                                                     (write-structure-handles cluster-scan
                                                      *no-first-header-code*
                                                      vinc:*qs-in-cluster*)
                                                     (update-structure-handles (1+ cluster-count)
                                                                               (1+ cluster-scan)
                                                                               (- boxed-qs
                                                                                  vinc:*qs-in-cluster*)))
                                                    (t (write-structure-handles cluster-scan
                                                        *no-first-header-code*
                                                        boxed-qs)
                                                       (update-structure-handles (1+ cluster-count)
                                                                                 (1+ cluster-scan)
                                                                                 0)))))
                                     (update-structure-handles 0 (cluster-number pointer) boxed-qs))
                                   0)))
                         ;; Think about scavenging here
                         )
                       (setq gr::*structure-cons-cache-free* new-free)
                       (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))
                       pointer)))))
      (try-again))))


(defun load-structure-cons-cache-for-area (area qs-needed)
  (get-active-region
    area
    region-bits:$$region-space-structure
    region-bits:$$region-new-space
    nil
    qs-needed))

(defun set-default-structure-cons-area (area)
  (unless (= area gr::*structure-cons-cache-area*)
    (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
    (setq gr::*structure-cons-cache-area* area)
    (load-structure-cons-cache-for-area area 0)
    (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*))
    ))

(defun allocate-structure-in-area (boxed-qs unboxed-qs data-type header area)
  (if (= area gr::*structure-cons-cache-area*)
      (allocate-structure boxed-qs unboxed-qs data-type header)
      (progn
        (setq gr::*allow-sequence-break* (1+ gr::*allow-sequence-break*))
        (let ((current-cache-area gr::*structure-cons-cache-area*))
          (load-structure-cons-cache-for-area area (+ boxed-qs unboxed-qs))
          (prog1 (allocate-structure boxed-qs unboxed-qs data-type header)
                 (load-structure-cons-cache-for-area current-cache-area 0)
                 (setq gr::*allow-sequence-break* (1- gr::*allow-sequence-break*)))))))

;;; This is the top half of an illegal instruction (minus the stat bit)
(defconstant code-header-instruction-high (hw:unboxed-constant #x7FFFFFFF))

;;; Allocating Code space.

(defun allocate-code-space (n-instructions associated-fef area)
  (setq gr::*allow-sequence-break* (1+ gr:*allow-sequence-break*))
  (prog1 (map-fault:call-while-allowing-write-in-read-only
           #'(lambda ()
               (let* ((qs-needed (ash (+ n-instructions 1) 1.))
                      region code-location)
                 (do () (()) ;;;; This is gross - if the code crosses a 1/2 quantum boundary, then throw
                             ;;;;      this chunk away and try again.
                      (setq region (get-active-region area
                                                      region-bits:$$region-space-code
                                                      region-bits:$$region-new-space
                                                      nil
                                                      qs-needed))
                      (setq code-location (make-pointer $$dtp-unboxed-locative
                                                   (region-data:region-free-pointer region)))
                      (let ((code-end (hw:24+ qs-needed code-location)))

                        (when (hw:field= code-location (hw:32-1- code-end) (byte 13. 13.))
                          (setf (region-data:region-free-pointer region) code-end)
                          (return))
                        (setf (region-data:region-free-pointer region) (hw:dpb 0. (byte 13. 0.) code-end))))

                 (setq gr:*allow-write-in-read-only* t)

                 ;; This one is a pointer to the fef.
                 (hw:write-md-boxed associated-fef)
                 (hw:vma-start-write code-location)

                 ;; Other word is magic marker word.
                 (hw:write-md-unboxed code-header-instruction-high)
                 (hw:vma-start-write-no-gc-trap-unboxed (hw:32-1+ code-location))

                 code-location)))

         ;; Think about scavenging here

         (setq gr::*allow-sequence-break* (1- gr:*allow-sequence-break*))))

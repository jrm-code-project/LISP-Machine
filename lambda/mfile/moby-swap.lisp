;;; -*- Mode: Lisp; Base: 8; Package: moby-file-system; Readtable: ZL -*-


;  COPYRIGHT LISP MACHINE INC 1985
;    CONFIDENTIAL
;  This file contains trade secret information of Lisp Machine Inc.
;   (However, I (RG) and LMI (to some extent) state their intention of making
;   this technique "OPEN" at an appropriate time.)

;;; The virtual memory

(defvar *n-vmem-pages* 16.)
(defvar *moby-cached-dataspace-page* -1)        ;find this guy fast.
(defvar *moby-cached-vmem-idx* -1)              ;associated vmem-idx.  Dont reuse this one.

;(i,0) is dataspace-page-number-with-offset
;(i,1) is rqb
;(i,2) is mapping-alist elem
;(i,3) is modified flag.
;(i,4) is lock count.  reuse this vmem-page entry only if 0.
;(i,5) is 16b rqb-buffer.

(defvar *vmem-pages* (make-array (list *n-vmem-pages* 6)))

(defvar *vmem-page-reuse-pointer*)

(defun vmem-initialize ()
  (setq *vmem-page-reuse-pointer* 0)
  (setq *moby-cached-dataspace-page* -1
        *moby-cached-vmem-idx* -1)
  (dotimes (i *n-vmem-pages*)
    (aset -1 *vmem-pages* i 0)
    (aset nil *vmem-pages* i 1)
    (aset nil *vmem-pages* i 2)
    (aset nil *vmem-pages* i 3)
    (aset 0 *vmem-pages* i 4)
    (aset nil *vmem-pages* i 5)))

;fields in ph-root-dataspace-map-fields:
;(defconst %%ph-root-dataspace-map-fields-number-alts (byte 6 0))

;defined mapping codes:
; 1: straight moby, 2 qs-per-q.
; 2: section relative, 1 q per q, no moby-format info.
;     interpretation of 25 bit address:
;       positive:  relative address within section
;       leading bits 10:  other section, my offset to exit structure in root-section
;       leading bits 11:  root-section
; 3: section relative with moby-format, 1 q per q, 8 bits per q moby format stored at end.

(defconst *mapping-code-description*
          #("Mapping code 0, invalid"
            "Straight moby, 2 Qs per Q"
            "Section relative, 1 Q per Q, no moby format info"
            "Section relative, 1 Q per Q, moby format stored separately at end"))
(defconst *mapping-code-qs-per-lispm-q*
          #( nil
             2.0
             1.0
             1.25))

;mapping-code vectors

;-- obsolete
;layout of of low numbered blocks of moby partition.
;  header               1 block    partition-header defstruct.
;  namespace-glob-table
;    base, high, allocating
;  dataspace-usage-count-array   as necessary to accomodate ART-4B array for each block
;  prime root-dataspace-map     namespace used  this namespace normally dataspace mapped to
;                                one of the below slots.
;  <number-root-alternates> * <alternate root dataspace map>

(defvar *root-section-region-size-in-pages*
        (ash 400000 -8.))  ;size of new regions in root-section

(defvar *vmem-moby-partition-alist* nil)   ;indexed by DATASPACE-WITH-OFFSET frobs.
(defvar *vmem-moby-dataspace-offset-allocator* 0)

(defun mpa-association-of-partition-header (partition-header)
  (dolist (e *vmem-moby-partition-alist*)
    (cond ((eq partition-header (mpa-partition-header e))
           (return e)))))

;(defun vmem-find-mapping-for-partition (part-name)
;  (dolist (e *vmem-moby-partition-alist*)
;    (cond ((equal part-name (mpa-partition-name e))
;          (return e)))))

;--not used now.  Hack for explorer.--
(defun moby-activate-partitions (&optional reboot partition-name (unit 0))
  (if (not (boundp '*vmem-page-reuse-pointer*))
      (vmem-initialize))
  (if (not (boundp '*moby-page-association*)) (moby-initialize-transforms))
  (if (null partition-name)
      (setq partition-name (nth (if (boundp 'si:*my-proc-number*)
                                    si:*my-proc-number*
                                  0)
                                '("MOBY" "MBY1" "MBY2"))))
  (let ((partition-list (si:partition-list nil unit)))
    (dolist (e partition-list)
      (cond ((equal (first e) partition-name)
             (vmem-activate-partition-explicit
               (second e) (third e) unit reboot))))
    ))

(defun vmem-initialize-partition
       (part-name partition-host-name moby-base-namespace-page moby-namespace-pages mapping-code
        &optional
        (unit 0)
        (percentage-double-mapped .25s0)
        (usage-count-array-type 'art-4b))
  (format t "~%Use this only when partition is unique!")
  (multiple-value-bind (part-base part-length)
      (si:find-disk-partition part-name nil unit)
    (if (null part-base) (ferror nil "Partition ~s not found" part-name)
      (cond ((yes-or-no-p
               "Do you really want to initialize moby partition ~s on unit ~s,
moby namespace page ~o, ~o moby namespace pages with storage code ~d,(~a)?"
               part-name unit moby-base-namespace-page moby-namespace-pages
               mapping-code (aref *mapping-code-description* mapping-code))
             (vmem-initialize-partition-explicit
               part-base part-length partition-host-name
               moby-base-namespace-page moby-namespace-pages mapping-code unit
               percentage-double-mapped
               usage-count-array-type)
             (setq *moby-local-package-host* partition-host-name))))))

(defun vmem-initialize-partition-explicit
       (part-base part-length partition-host-name
        &optional moby-base-namespace-page moby-namespace-pages mapping-code (unit 0)
        (percentage-double-mapped .25s0)
        (usage-count-array-type 'art-4b)
        (partition-header-namespace-length-in-pages 64.)
        (root-mapping-namespace-length-in-pages 100) ;pages root-mapping-section region
  ;can expand to.  Thus, root-area can grow approximately to this shifted by 16. Qs,
  ;since root-mapping area is limited to this single region.
        (size-of-root-mapping-section-map 6)
        (initial-size-of-root-section-map 4)
        &aux mpa partition-header-area partition-header root-mapping-area root-area)
 ;compute dataspace-length-in-pages
  (let* ((host-index (moby-assign-host-index (list 'initializing)))  ;reserve ...
         (partition-header-area-name
           (moby-partition-header-area-name-for-partition-host partition-host-name))
         (root-mapping-area-name
           (moby-root-mapping-area-name-for-partition-host partition-host-name))
         (root-area-name (moby-root-area-name-for-partition-host partition-host-name))
         (qs-per-q (aref *mapping-code-qs-per-lispm-q* mapping-code))
         (total-blocks (truncate part-length qs-per-q))
         (number-double-mapped-blocks
           (fix (* percentage-double-mapped
                   (truncate total-blocks (+ 1.0 percentage-double-mapped)))))
         (dataspace-length-in-pages (- total-blocks number-double-mapped-blocks))
         (usage-count-array-entry-number-values (expt 2 (cdr (assq usage-count-array-type
                                                                   array-bits-per-element))))
         (base-dataspace-page-with-offset
             *vmem-moby-dataspace-offset-allocator*)
         (high-dataspace-page-with-offset+1
             (+ base-dataspace-page-with-offset dataspace-length-in-pages))
         (fake-partition-header-map    ;just like real thing but in working-storage.
           (moby-make-fake-map partition-header-area-name
                               moby-base-namespace-page
                               1
                               partition-header-namespace-length-in-pages
                               (dpb 1 %%area-moby-option-double-mapped 0)))
         )

    (incf *vmem-moby-dataspace-offset-allocator*
          dataspace-length-in-pages)

    (setq partition-header-area
          (moby-setup-area partition-header-area-name 'partition-header nil nil host-index))
    (moby-store-map-for-area partition-header-area fake-partition-header-map 'all nil host-index)
    (if (not (boundp root-mapping-area-name))
        (moby-setup-area root-mapping-area-name 'root-mapping nil nil host-index))
    (setq root-mapping-area (symeval root-mapping-area-name))
    (if (not (boundp root-area-name))
        (moby-setup-area root-area-name 'root nil nil host-index))
    (setq root-area (symeval root-area-name))

  ;add to *vmem-moby-partition-alist*
    (push
      (setq mpa (make-mpa-element :dataspace-origin base-dataspace-page-with-offset
                                  :dataspace-highest-+1 high-dataspace-page-with-offset+1
                                  :mapping-code mapping-code
                                  :physical-unit unit
                                  :partition-origin part-base
                                  :partition-length part-length
                                  :partition-header-area partition-header-area
                                 ;:partition-header filled in below.
                                  :root-mapping-area root-mapping-area
                                  :root-area root-area
                                  :single-mapped-dataspace-usage-occurance-counts
                                   (make-array usage-count-array-entry-number-values
                                               :initial-element 0)
                                  :double-mapped-dataspace-usage-occurance-counts
                                   (make-array usage-count-array-entry-number-values
                                               :initial-element 0)
                                  :local-host-index host-index))
      *vmem-moby-partition-alist*)

  ;wipe whole partition.  This is necessary to initialize moby bits, among other things.
    (let* ((i (vmem-find-page base-dataspace-page-with-offset nil)))
      (do ((mp base-dataspace-page-with-offset (1+ mp))
           (rqb (aref *vmem-pages* i 1))
           (c 0 (1+ c)))
          ((= c dataspace-length-in-pages))
     ;should hack double-mapped area.
        (vmem-disk-io rqb mp t (aref *vmem-pages* i 2))))

    (setq partition-header
          (make-partition-header
              :MAKE-ARRAY (:AREA PARTITION-HEADER-AREA)
              :code             1
              :mapping-code     mapping-code
              :magic-number     *moby-partition-header-magic-number*
              :partition-header-low-namespace-page   moby-base-namespace-page
              :partition-header-high-namespace-page
                        (+ moby-base-namespace-page
                           partition-header-namespace-length-in-pages)
              :dataspace-length-in-pages        dataspace-length-in-pages
              :number-double-mapped-blocks      number-double-mapped-blocks
;             :root-section-options
;             (dpb 1 %%section-option-update-on-request-only
;                  (dpb 1 %%section-option-reconcile-on-swapin
;                       (dpb 1 %%section-option-self-relative 0)))
;             :root-section-namespace-length-in-pages
;             local-region-length-in-pages
;             :root-dataspace-map-length-in-pages
;             root-dataspace-map-length-in-pages
;             :root-dataspace-map-fields
;             (dpb number-root-alternates
;                  %%ph-root-dataspace-map-fields-number-alts
;                  0)
;             :root-dataspace-map-base-relative-page
;             root-dataspace-map-base-relative-page
              ))

    (setf (mpa-partition-header mpa) partition-header)

  ;now cons "real" partition-header map in partition-header-area, to replace fake one.
    (let ((real-partition-header-map
            (moby-make-section-map partition-header-area nil nil partition-header-area
                                   1 (dpb 1 %%area-moby-option-double-mapped 0))))
      (array-push real-partition-header-map
                  (let ((default-cons-area partition-header-area))
                    (si:copy-object (aref fake-partition-header-map 0))))
      (let ((real-region-map (aref real-partition-header-map 0))
            (real-region (one-and-only-region-of-area partition-header-area)))
        (setf (aref *region-to-region-map*      ;update to pointer at real thing, not fake.
                    real-region)
              real-region-map)
        (setf (aref *region-fresh-cons-boundary* real-region)
              (rm-free-pointer real-region-map))
        (setf (rm-locally-mapped-region real-region-map) real-region))
      (setf (ph-partition-header-section-map partition-header) real-partition-header-map)
      (moby-store-map-for-area partition-header-area real-partition-header-map
                               'all nil host-index))
;      ;map-moby-handle is set right by moby-make-section-map, even here.
;      (let ((msa (aref *area-to-msa* partition-header-area)))
;       (setf (msa-map-moby-handle msa)
;             (local-to-moby-corresondance real-partition-header-map)))

  ;now root-mapping-section-map can be CONSed in partition-header-area.

    (let ((root-mapping-section-map
            (moby-make-section-map partition-header-area nil nil root-mapping-area
                                   size-of-root-mapping-section-map
                                   (dpb 1 %%area-moby-option-double-mapped 0))))
      (setf (ph-root-mapping-section-map partition-header)
            root-mapping-section-map)
      (let ((region-map
              (moby-make-region-map nil
                                    partition-header-area
                                    (+ moby-base-namespace-page
                ;area where bignum cons happens doesnt matter since moby-form is immediate.
                                       partition-header-namespace-length-in-pages)
                                    root-mapping-namespace-length-in-pages)))
        (array-push root-mapping-section-map region-map))
     ;this will actually add an appropriate region to root-mapping-area.  Now it can be consed in.
      (moby-store-map-for-area root-mapping-area root-mapping-section-map
                               'all nil host-index))

    (moby-store-partition-header-for-area partition-header-area partition-header)
    (moby-store-partition-header-for-area root-mapping-area partition-header)
    (moby-store-partition-header-for-area root-area partition-header)

    (let ((namespace-allocation-structure
            (make-namespace-allocation-structure
              :make-array (:area root-mapping-area)
              :allocation-pointer (+ moby-base-namespace-page
                                     partition-header-namespace-length-in-pages
                                     root-mapping-namespace-length-in-pages)
              :allocation-limit   (+ moby-base-namespace-page
                                     moby-namespace-pages)
              :allocation-alist nil)))   ;fill in with cons from root-area
      (setf (ph-namespace-allocation-structure partition-header)
            namespace-allocation-structure))

    (let ((double-mapped-usage-count-array
            (make-usage-count-array
              :make-array (:area root-mapping-area
                           :length number-double-mapped-blocks
                           :type usage-count-array-type)
              :fill-pointer number-double-mapped-blocks
              :base-dataspace-page 0
              :ascending-count 0
              :scan-pointer 0)))
      (setf (ph-double-mapped-usage-count-array partition-header)
            double-mapped-usage-count-array)
      (setf (aref (mpa-double-mapped-dataspace-usage-occurance-counts mpa) 0)
            number-double-mapped-blocks))

    (let* ((number-single-mapped-blocks (- dataspace-length-in-pages number-double-mapped-blocks))
           (single-mapped-usage-count-array
             (make-usage-count-array
               :make-array (:area root-mapping-area
                            :length number-single-mapped-blocks
                            :type usage-count-array-type)
               :fill-pointer number-double-mapped-blocks
               :base-dataspace-page number-double-mapped-blocks
               :ascending-count 0
               :scan-pointer 0)))
      (setf (ph-single-mapped-usage-count-array partition-header)
            single-mapped-usage-count-array)
      (setf (aref (mpa-single-mapped-dataspace-usage-occurance-counts mpa) 0)
            number-single-mapped-blocks))

  ;it is critical that no dataspace has been assigned up to here.  So these go
  ;at the beginning of the partition, which is what the activating bootstrapper assumes.
    (moby-assign-dataspace-to-area partition-header-area t)  ;assign dataspace to whole thing.
    (moby-assign-dataspace-to-area root-mapping-area t)      ;likewise

  ;now assigning local-cells in region-maps can win.
    (moby-declare-local-cells-for-area partition-header-area)
    (moby-declare-local-cells-for-area root-mapping-area )

  ;cons root-section-map in root-mapping-section
    (let ((root-section-map
            (moby-make-section-map root-mapping-area nil nil root-area
                                   initial-size-of-root-section-map
                                   (dpb 1 %%area-moby-option-double-mapped 0))))
      (setf (ph-root-section-map partition-header)
            root-section-map)
      (moby-add-region-to-map-and-allocate-namespace
        root-section-map nil
        *root-section-region-size-in-pages* partition-header)
      (moby-store-map-for-area root-area root-section-map
                               'all nil host-index))

    (setf (ph-partition-host-name partition-header)
          (let ((default-cons-area partition-header-area))
            (string-append partition-host-name)))

  ;by the time we try to read root-area in the bootstrap process, the dataspace mapping
  ; mechanisms are active. So this does not necessarily have to go in consecutive dataspace
  ; following the partition-header and root-mapping-area.  However, we arrange that at least
  ; the first region does so for efficiency (or something).
    (moby-assign-dataspace-to-area root-area nil)   ;only assign dataspace to root as used.

  ;initialize section header
    (moby-cons-section-directory-header partition-host-name mpa)
  ;declare local cells in usage-count-arrays.
    (moby-declare-local-cell
      (locf (uc-lock (ph-double-mapped-usage-count-array partition-header))))
    (moby-declare-local-cell
      (locf (uc-lock (ph-single-mapped-usage-count-array partition-header))))
    (let ((partition-host (define-moby-file-system-host partition-host-name mpa host-index)))
      (format t "~%Defined host ~s" partition-host-name)
      (moby-package-root partition-host) ;assure root;package-root.mby exists.
      (writeout-moby-package-root partition-host)
      (moby-writeout-root partition-host))
    t))

(defun moby-writeout-all (&optional even-if-clean)
  (dolist (msa *moby-section-area-and-region-associations*)
    (moby-writeout-area (msa-area msa) even-if-clean)))

(defun moby-writeout-root (partition-host &optional even-if-clean)
  (let ((mpa (send partition-host :mpa-defstruct))
        (package-root-area (%area-number *moby-package-root*)))
   ;write them in this order since writing one can change the others.
    (do-forever
      (cond (*moby-package-root*
             (moby-writeout-area package-root-area)))
  ;root-area, root-maping-area and partition-header-area are hierarchial in that
  ; writing one cannot affect previous ones in that list.
      (moby-writeout-area (mpa-root-area mpa) even-if-clean)
      (moby-writeout-area (mpa-root-mapping-area mpa) even-if-clean)
      (moby-writeout-area (mpa-partition-header-area mpa) even-if-clean)
      (cond ((or (null *moby-package-root*)
                 (moby-area-clean-p package-root-area))
             (return t))))
    (vmem-force-all)))

(defun vmem-activate-partition (part-name unit)
  (multiple-value-bind (part-base part-length)
      (si:find-disk-partition part-name nil unit)
    (if part-base (vmem-activate-partition-explicit part-base part-length unit))))

;values  (partition-header root-dataspace-map section-root)
(defun vmem-activate-partition-explicit
       (part-base part-length unit &aux mpa)
  (let* ((rqb (sys:get-disk-rqb 2))
         (buf (sys:rqb-buffer rqb)))
    (sys:disk-read rqb unit part-base)
    (let* ((host-index (moby-assign-host-index (list 'activating)))
           (partition-header-area-placeholder '*moby-partition-header-area-placeholder*)
           (partition-header-area nil)  ;temporarily
           (root-mapping-area nil)
           (root-area nil)

            ;q0 is array-header
            ;unfortunately, with the array-header q in there, the hack of assuming
            ; qs-per-lispm-q is 1 initially doesnt win.. could be hacked. ***
           (code
             (vmem-get-fix-unsigned 2 buf
                                    (1+ (get-defstruct-slot-index 'ph-code))))
           (mapping-code
             (vmem-get-fix-unsigned 2 buf
                                    (1+ (get-defstruct-slot-index 'ph-mapping-code))))
           (qs-per-lispm-q (fix (aref *mapping-code-qs-per-lispm-q* mapping-code)))
           (magic-number
             (vmem-get-fix-unsigned qs-per-lispm-q buf
                                    (1+ (get-defstruct-slot-index 'ph-magic-number))))
           (partition-header-low-namespace-page
             (vmem-get-fix-unsigned qs-per-lispm-q buf
              (1+ (get-defstruct-slot-index 'ph-partition-header-low-namespace-page))))
           (partition-header-high-namespace-page
             (vmem-get-fix-unsigned qs-per-lispm-q buf
              (1+ (get-defstruct-slot-index 'ph-partition-header-high-namespace-page))))
           (partition-header-free-pointer
             (vmem-get-fix-unsigned qs-per-lispm-q buf
              (1+ (get-defstruct-slot-index 'ph-partition-header-free-pointer))))
           (partition-header-length-in-pages
             (- partition-header-high-namespace-page
                partition-header-low-namespace-page))
           (dataspace-length-in-pages
             (vmem-get-fix-unsigned qs-per-lispm-q buf
              (1+ (get-defstruct-slot-index 'ph-dataspace-length-in-pages))))
           (number-double-mapped-blocks
             (vmem-get-fix-unsigned qs-per-lispm-q buf
              (1+ (get-defstruct-slot-index 'ph-number-double-mapped-blocks))))
           (base-dataspace-page-with-offset
             *vmem-moby-dataspace-offset-allocator*)
           (high-dataspace-page-with-offset+1
             (+ base-dataspace-page-with-offset dataspace-length-in-pages))
           )
      (sys:return-disk-rqb (prog1 rqb (setq rqb nil)))

      (incf *vmem-moby-dataspace-offset-allocator*
            dataspace-length-in-pages)

   ;check header
      (if (not (= magic-number *moby-partition-header-magic-number*))
          (ferror nil "Header magic number does not check"))
      (makunbound partition-header-area-placeholder)    ;just to make sure.
      (moby-setup-area partition-header-area-placeholder
                       'partition-header nil nil host-index)
      (setq partition-header-area (symeval partition-header-area-placeholder))
   ;add to *vmem-moby-partition-alist*
      (push
        (setq mpa (make-mpa-element :dataspace-origin base-dataspace-page-with-offset
                                    :dataspace-highest-+1 high-dataspace-page-with-offset+1
                                    :mapping-code mapping-code
                                    :physical-unit unit
                                    :partition-origin part-base
                                    :partition-length part-length
                                    :partition-header-area partition-header-area
                        ;below have to be filled in later.
                                    ;:partition-header
                                    ;:root-mapping-area
                                    ;:root-area
                                    ;:single-mapped-dataspace-usage-occurance-counts
                                    ;:double-mapped-dataspace-usage-occurance-counts
                                    :local-host-index host-index
                                    ))
        *vmem-moby-partition-alist*)
      (format t"~%Activating partition header at namespace ~s, ~d moby pages,
  ~d disk-pages on unit ~s starting at ~s, partition-header-area ~s, mapping code ~d(~a)"
              partition-header-low-namespace-page dataspace-length-in-pages part-length unit
              part-base partition-header-area
              mapping-code (aref *mapping-code-description* mapping-code))

      (let ((msa (aref *area-to-msa* partition-header-area)))
        (setf (msa-mpa msa) mpa))  ;normally gotten via association on partition-header
                ;this needs to be set up earlier here for bootstrapping.

;-- new plan
;  make fake map from info bootstrapped info.
;  recover partition-host-name
;  create root-mapping-area and root-area with correct names.
;  install root-mapping-area.
;  install root-area.

;to successfully bootstrap partition-header-area
;   local-to-moby-correspondance has to work -> #'sys:region-namespace-origin set up.
;    moby-local-page-to-dataspace-page  ->  *region-to-region-map* set up.
;                                           *region-to-msa*, msa-to-mpa, mpa-dataspace-origin.

      (let* ((fake-partition-header-map
               (moby-make-fake-map partition-header-area-placeholder
                                  partition-header-low-namespace-page
                                  1
                                  partition-header-length-in-pages
                                  (dpb 1 %%area-moby-option-double-mapped 0)))
             (fake-region-map (aref fake-partition-header-map 0)))
        (setf (rm-free-pointer fake-region-map) partition-header-free-pointer)
        (dotimes (p partition-header-length-in-pages)
          (setf (aref fake-region-map p) p))  ;direct map the partition-header.
        (setf (rm-fill-pointer fake-region-map) partition-header-length-in-pages)
        (moby-store-map-for-area partition-header-area fake-partition-header-map
                                 'all nil host-index)

        (let* ((partition-header
                 (moby-typed-pointer dtp-array-pointer
                                     (ash partition-header-low-namespace-page 8)))
               (partition-host-name
                 (ph-partition-host-name partition-header)))

          (setf (mpa-partition-header mpa) partition-header)
          (moby-store-partition-header-for-area partition-header-area partition-header)

          (moby-smash-area-name partition-header-area-placeholder
                                (moby-partition-header-area-name-for-partition-host
                                  partition-host-name))
       ;touch Qs in real map now to reconcile them.  This avoids the infinite recursive
       ; loop which would happen if an unreconciled trap was taken in
       ;  moby-local-page-to-dataspace-page.  Also touch rm-fill-pointer
       ; which is touched there.  As long as we are using the fake map, there is no
       ; problem.
          (let ((real-partition-header-map (ph-partition-header-section-map partition-header)))
            (dotimes (i (array-length real-partition-header-map))
              (let ((rm (aref real-partition-header-map i)))
                (cond (rm
                       (dotimes (p (rm-fill-pointer rm))
                         (aref rm p))
                       (dotimes (p (array-leader-length rm))
                         (array-leader rm p))))))
           ;touch array leader of section map.
            (dotimes (i (array-leader-length real-partition-header-map))
              (array-leader real-partition-header-map i))
       ;store real map over fake one.
            (moby-store-map-for-area partition-header-area real-partition-header-map
                                     'all nil host-index)
;       ;correct map-moby-handle
;           (let ((msa (aref *area-to-msa* partition-header-area)))
;             (setf (msa-map-moby-handle msa)
;                   (local-to-moby-correspondance real-partition-header-map)))
            )

          (let ((root-mapping-area-name
                  (moby-root-mapping-area-name-for-partition-host partition-host-name))
                (root-area-name (moby-root-area-name-for-partition-host partition-host-name)))
            (moby-setup-area root-mapping-area-name 'root-mapping nil
                             partition-header host-index)
            (setq root-mapping-area (symeval root-mapping-area-name))
            (setf (mpa-root-mapping-area mpa) root-mapping-area)
            (moby-store-map-for-area root-mapping-area
                                     (ph-root-mapping-section-map partition-header)
                                     'all nil host-index)
            (moby-setup-area root-area-name 'root nil
                             partition-header host-index)
            (setq root-area (symeval root-area-name))
            (setf (mpa-root-area mpa) root-area)
            (moby-store-map-for-area root-area
                                     (ph-root-section-map partition-header)
                                     'all nil host-index))


        (let* ((double-uc (ph-double-mapped-usage-count-array partition-header))
               (double-ua-occur (make-array (cadr (array-element-type double-uc)))))
          (setf (mpa-double-mapped-dataspace-usage-occurance-counts mpa) double-ua-occur)
          (moby-count-occurances double-ua-occur double-uc))

        (let* ((single-uc (ph-single-mapped-usage-count-array partition-header))
               (single-ua-occur (make-array (cadr (array-element-type single-uc)))))
          (setf (mpa-single-mapped-dataspace-usage-occurance-counts mpa) single-ua-occur)
          (moby-count-occurances single-ua-occur single-uc))

        (let ((sdh (ph-root-section-directory-header partition-header)))
          (when (null sdh)
            (format t "~%The partition-host of this partition has not been initialized."))
          (define-moby-file-system-host partition-host-name mpa host-index)
          (format t "~%Defined host ~s" partition-host-name)
          (moby-walk-hierarchy-storing-moby-to-section sdh))

        (setq *moby-local-package-host* partition-host-name)
        (values partition-header))))))

(defun moby-smash-area-name (old-name-symbol new-name-symbol)
  (set new-name-symbol (symeval old-name-symbol))
  (setf (aref #'sys:area-name (symeval old-name-symbol))
        new-name-symbol)
  ;(fset new-name-symbol (fsymeval old-name-symbol))
  (do ((p (si:current-area-list) (cdr p)))
      ((null p) (ferror nil "Cant find ~s on current-area-list" old-name-symbol))
    (cond ((eq (car p) old-name-symbol)
           (rplaca p new-name-symbol)
           (return nil))))
  (do ((p si:room (cdr p)))
      ((null p))
    (cond ((eq (car p) old-name-symbol)
           (rplaca p new-name-symbol)
           (return nil)))))

(defun moby-cons-section-directory-header (name mpa)
 ;the section-directory-header is a somewhat specially distinguished section-item ...
  (let* ((partition-header (mpa-partition-header mpa))
         (root-area (mpa-root-area mpa)))
    (let ((section-directory-header
            (make-moby-file :make-array (:area root-area)
              )))
      (moby-declare-local-cell (locf (mfile-lock section-directory-header)))
      (setf (ph-root-section-directory-header partition-header) section-directory-header)
      (let* ((default-cons-area root-area)
             (moby-name (string-append name)))
          (setf (mfile-name section-directory-header) moby-name))
      section-directory-header)))

(defun lookup-section-directory-header (partition-host-name)
  (dolist (e *vmem-moby-partition-alist*)
    (let ((sdh (ph-root-section-directory-header (mpa-partition-header e))))
      (cond ((string-equal partition-host-name (mfile-name sdh))
             (return sdh))))))

(defun lookup-root-directory-files (partition-host-name)
  (let ((sdh (lookup-section-directory-header partition-host-name)))
    (if (null sdh)
        (ferror nil "Root not found")
      (mfile-files sdh))))

(defun find-root-directory (partition-host-name)
  (lookup-section-directory-header partition-host-name))

(defun root-area-of-partition-host (partition-host)
  (mpa-root-area (send partition-host :mpa-defstruct))
;  (dolist (e *vmem-moby-partition-alist*)
;    (let ((sdh (ph-root-section-directory-header (mpa-partition-header e))))
;      (cond ((string-equal partition-host-name (mfile-name sdh))
;            (return (mpa-root-area e))))))
  )

(defun mpa-of-partition-host (partition-host)
  (send partition-host :mpa-defstruct)
;  (dolist (e *vmem-moby-partition-alist*)
;    (let ((sdh (ph-root-section-directory-header (mpa-partition-header e))))
;      (cond ((string-equal partition-host-name (mfile-name sdh))
;            (return e)))))
  )

(defun mpa-of-partition-header (partition-header)
  (dolist (e *vmem-moby-partition-alist*)
    (cond ((eq partition-header (mpa-partition-header e))
           (return e)))))

(defun partition-header-of-partition-host (partition-host)
  (mpa-partition-header (send partition-host :mpa-defstruct))
;  (dolist (e *vmem-moby-partition-alist*)
;    (let ((sdh (ph-root-section-directory-header (mpa-partition-header e))))
;      (cond ((string-equal partition-host-name (mfile-name sdh))
;            (return (mpa-partition-header e))))))
  )

(defun moby-local-partition-host-name (localp)
  (let* ((region (%region-number localp))
         (msa (aref *region-to-msa* region)))
    (cond ((msa-partition-header msa)
           (ph-partition-host-name (msa-partition-header msa)))
          (t *moby-local-package-host*
             ;(ferror nil "Need partition host for non local moby area")
             ))
    ))

(defun moby-local-partition-host (localp)
  (let* ((region (%region-number localp))
         (msa (aref *region-to-msa* region)))
    (aref *moby-host-index* (msa-host-index msa))))

(defun moby-mpa-double-mapped-dataspace-usage-occurance-counts (partition-host)
  (let ((mpa (send partition-host :mpa-defstruct)))
    (mpa-double-mapped-dataspace-usage-occurance-counts mpa)))

(defun moby-mpa-single-mapped-dataspace-usage-occurance-counts (partition-host)
  (let ((mpa (send partition-host :mpa-defstruct)))
    (mpa-single-mapped-dataspace-usage-occurance-counts mpa)))

(defun moby-dataspace-offset-of-partition-header (partition-header)
  (dolist (e *vmem-moby-partition-alist* (ferror nil "partition-header not found"))
    (cond ((eq partition-header (mpa-partition-header e))
           (return (mpa-dataspace-origin e))))))

;Return a positive integer, possibly a bignum.
(defun vmem-get-fix-unsigned (qs-per-lispm-q buf idx)
  (let ((base-idx (* qs-per-lispm-q 2 idx)))
    (if (not (= dtp-fix (ldb (byte 5 9.) (aref buf (1+ base-idx)))))
        (ferror nil "Data type in section header not DTP-FIX"))
    (+ (ash (ldb (byte 9. 0) (aref buf (1+ base-idx))) 16.)
       (aref buf base-idx)
       (ash (aref buf (+ base-idx 2)) 25.)
       (ash (ldb (byte 9 0) (aref buf (+ 3 base-idx))) 41.))))

;;Return a 50. positive bignum stored as two consecutive 25. bit fixnums.
;;(each of those fixnums occupies QS-PER-LISPM-Q).  A few things are
;;stored this way in the header for the convenience of single mapped partitions.
;(defun vmem-get-double-fix-unsigned (qs-per-lispm-q buf idx)
;  (let ((base-idx (* qs-per-lispm-q 2 idx)))
;    (if (not (= dtp-fix (ldb (byte 5 9.) (aref buf (1+ base-idx)))))
;       (ferror nil "Data type in section header not DTP-FIX"))
;    (let ((w1 (+ (ash (ldb (byte 9. 0) (aref buf (1+ base-idx))) 16.)
;                (aref buf base-idx))))
;      (cond ((> qs-per-lispm-q 1)
;            (if (not (= dtp-fix (ldb (byte 5 9.) (aref buf (+ 5 base-idx)))))
;                (ferror nil "Data type in section header (high word) not DTP-FIX"))
;            (let ((w2 (+ (ash (ldb (byte 9. 0) (aref buf (+ 5 base-idx))) 16.)
;                         (aref buf (+ 4 base-idx)))))
;              (+ w1 (ash w2 25.))))
;           (t
;            (if (not (= dtp-fix (ldb (byte 5 9.) (aref buf (+ 3 base-idx)))))
;                (ferror nil "Data type in section header (high word) not DTP-FIX"))
;            (let ((w2 (+ (ash (ldb (byte 9. 0) (aref buf (+ 3 base-idx))) 16.)
;                         (aref buf (+ 2 base-idx)))))
;              (+ w1 (ash w2 25.))))))))

(defun vmem-get-mapping (dataspace-page-with-offset)
  (do ((p *vmem-moby-partition-alist* (cdr p)))
      ((null p) (ferror nil "unable to map moby page"))
    (cond ((and (not (< dataspace-page-with-offset (mpa-dataspace-origin (car p))))
                (< dataspace-page-with-offset (mpa-dataspace-highest-+1 (car p))))
           (return (car p))))))

;Write out all the buffered pages and return the rqb's
(defun vmem-finish (&optional no-write &aux rqb)
  (setq *moby-cached-dataspace-page* -1
        *moby-cached-vmem-idx* -1)
  (dotimes (i *n-vmem-pages*)
    (cond ((setq rqb (aref *vmem-pages* i 1))
           (cond ((and (null no-write)
                       (aref *vmem-pages* i 3))
                  (vmem-disk-io rqb (aref *vmem-pages* i 0) t (aref *vmem-pages* i 2))))
           (sys:return-disk-rqb rqb)
           (aset -1 *vmem-pages* i 0)
           (aset nil *vmem-pages* i 1)
           (aset nil *vmem-pages* i 2)
           (aset nil *vmem-pages* i 3)
           (aset 0 *vmem-pages* i 4)
           (aset nil *vmem-pages* i 5)))))

(defun vmem-force-all (&aux rqb)
  (dotimes (i *n-vmem-pages*)
    (cond ((setq rqb (aref *vmem-pages* i 1))
           (cond ((aref *vmem-pages* i 3)
                  (vmem-disk-io rqb (aref *vmem-pages* i 0) t (aref *vmem-pages* i 2))))
           (aset nil *vmem-pages* i 3)))))

(defun vmem-force-write (dataspace-page-with-offset)
  (dotimes (i *n-vmem-pages*)
    (cond ((= (aref *vmem-pages* i 0) dataspace-page-with-offset)
           (cond ((aref *vmem-pages* i 3)
                  (vmem-disk-io (aref *vmem-pages* i 1)
                                dataspace-page-with-offset
                                t
                                (aref *vmem-pages* i 2))
                  (setf (aref *vmem-pages* i 3) nil)))))))

(defun vmem-disk-io (rqb dataspace-page-with-offset writep mapping-elem)
  (funcall (if writep #'sys:disk-write #'sys:disk-read) rqb
           (mpa-physical-unit mapping-elem)
           (+ (mpa-partition-origin mapping-elem)
    ;below gets the main frob for section relative with moby format info mapping code.
              (* (fix (aref *mapping-code-qs-per-lispm-q*
                            (mpa-mapping-code mapping-elem)))
                 (- dataspace-page-with-offset (mpa-dataspace-origin mapping-elem))))))

;Given address returns art-16b array containing that page.  With second arg of nil
;initializes to dtp-trap instead of reading in from disk.
(defun vmem-find-page (dataspace-page-with-offset &optional (get-from-disk-p t))
  (do ((i 0 (1+ i))
       (rqb) (buf))
      (( i *n-vmem-pages*)
       (let ((mapping-elem (vmem-get-mapping dataspace-page-with-offset)))
         (do-forever
           (setq i *vmem-page-reuse-pointer*)
           (setq *vmem-page-reuse-pointer* (\ (1+ i) *n-vmem-pages*))
           (cond ((and (zerop (aref *vmem-pages* i 4))
                       (not (= i *moby-cached-vmem-idx*)))
                  (return))))
         (cond ((setq rqb (aref *vmem-pages* i 1))  ;Swap this guy out
                (cond ((aref *vmem-pages* i 3)
                       (vmem-disk-io rqb (aref *vmem-pages* i 0) t (aref *vmem-pages* i 2))
                       (setf (aref *vmem-pages* i 3) nil))))
               (t (setq rqb (sys:get-disk-rqb 2))
                  (aset rqb *vmem-pages* i 1)
                  (setf (aref *vmem-pages* i 3) nil)
                  (setf (aref *vmem-pages* i 5) (sys:rqb-buffer rqb))))
         (aset dataspace-page-with-offset *vmem-pages* i 0)
         (aset mapping-elem *vmem-pages* i 2)
         (setq buf (sys:rqb-buffer rqb))
         (cond (get-from-disk-p
                (vmem-disk-io rqb dataspace-page-with-offset nil mapping-elem)
                (setf (aref *vmem-pages* i 3) nil))
               (t (do ((j 0 (1+ j))
                       (high (dpb 1 %%moby-starts-object (ash dtp-trap (- 25. 16.))))
                       (idx-step (* 2 (fix (aref *mapping-code-qs-per-lispm-q*
                                                 (mpa-mapping-code mapping-elem)))))
                       (base-idx 0 (+ base-idx idx-step)))
                      (( j si:page-size)
                       (setf (aref *vmem-pages* i 3) t))
                    (aset 0 buf base-idx)
                    (aset high buf (1+ base-idx))
                    (cond ((> idx-step 2)
                           (aset 0 buf (+ 2 base-idx))
                           (aset 0 buf (+ 3 base-idx)))))))
         i))
    (cond ((= (aref *vmem-pages* i 0) dataspace-page-with-offset)       ;Already swapped in
           (and (= *vmem-page-reuse-pointer* i)
                (setq *vmem-page-reuse-pointer* (\ (1+ i) *n-vmem-pages*)))
           (return i)))))

(defun print-vmem-status ()
  (dotimes (i *n-vmem-pages*)
    (format t "~%Buffer index ~s: dataspace page with offset ~s, rqb ~s,
 mapping ~s, modified ~s, lock ~s, buffer ~s"
            i
            (aref *vmem-pages* i 0)
            (aref *vmem-pages* i 1)
            (aref *vmem-pages* i 2)
            (aref *vmem-pages* i 3)
            (aref *vmem-pages* i 4)
            (aref *vmem-pages* i 5))))


;like LDB, but can load fields bigger than fixnum size.
(DEFUN LDB-BIG (FLD WD)
  (PROG (ANS BITS BITS-OVER SHIFT)
        (SETQ SHIFT 0 ANS 0 BITS (LDB 0006 FLD) BITS-OVER (LDB 0620 FLD))
    L   (SETQ ANS (LOGIOR ANS (ASH (LDB (DPB BITS-OVER 0620 (MIN BITS 24.)) WD) SHIFT)))
        (IF ( (SETQ BITS (- BITS 24.)) 0) (RETURN ANS))
        (SETQ SHIFT (+ SHIFT 24.)
              BITS-OVER (+ BITS-OVER 24.))
        (GO L)))

(DEFUN DPB-BIG (QUAN FLD WD)
  (PROG (ANS BITS BITS-OVER Q)
        (SETQ ANS WD BITS (LDB 0006 FLD) BITS-OVER (LDB 0620 FLD) Q QUAN)
    L   (SETQ ANS (DPB (logand 77777777 Q) (DPB BITS-OVER 0620 (MIN BITS 24.)) ANS))
        (IF ( (SETQ BITS (- BITS 24.)) 0) (RETURN ANS))
        (SETQ Q (ASH Q -24.)
              BITS-OVER (+ BITS-OVER 24.))
        (GO L)))

(defun i () (inspect *vmem-moby-partition-alist*))
(defun e () (moby-exhibit *vmem-moby-partition-alist*))
(defun m () (inspect *moby-section-area-and-region-associations*))

(pushnew :moby *features*)

;not used currently.
(defun moby-fetch-quarters (local-address)
  (let ((idx (ash (logand local-address 377) 2))
        (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
    (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
           (setq *moby-cached-vmem-idx*
                 (vmem-find-page dataspace-page)
                 *moby-cached-dataspace-page* dataspace-page)))
    (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
      (values (aref buf idx)
              (aref buf (1+ idx))
              (aref buf (+ 2 idx))
              (aref buf (+ 3 idx))))))

(defun moby-fetch-q3 (local-address)
  (let ((idx (ash (logand local-address 377) 2))
        (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
    (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
           (setq *moby-cached-vmem-idx*
                 (vmem-find-page dataspace-page)
                 *moby-cached-dataspace-page* dataspace-page)))
    (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
      (aref buf (+ 3 idx)))))

;not used currently.
(defun moby-store-quarters (local-address q0 q1 q2 q3)
  (let ((idx (ash (logand local-address 377) 2))
        (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
    (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
           (setq *moby-cached-vmem-idx*
                 (vmem-find-page dataspace-page)
                 *moby-cached-dataspace-page* dataspace-page)))
    (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
      (setf (aref buf idx) q0)
      (setf (aref buf (1+ idx)) q1)
      (setf (aref buf (+ 2 idx)) q2)
      (setf (aref buf (+ 3 idx)) q3)
      (setf (aref *vmem-pages* *moby-cached-vmem-idx* 3) t)
      nil)))

;component formats:
;  unboxed: low 16 bits in q0, high 16 in q2.
;  pointer:  50 bit moby pointer (as a positive integer, maybe big), 7 bit cc-and-dt,
;                       5 bit moby-format.
;     used for dtp-symbol, dtp-list, dtp-array-pointer
;  direct:   25 bits for %pointer field, 7 bit cc-and-dt, 5 bit moby-format
;     used for dtp-character, dtp-header, dtp-array-header, dtp-small-flonum
;  fixnum dt:   50 bit signed integer, 7 bit cc-and-dt, 5 bit moby format.
; (values format-symbol moby-format-bits v1 v2)


;currently not used.
;(defun moby-fetch-parsed-components (local-address)
;  (declare (values moby-pointer cdr-code-and-data-type moby-format))
;  (let ((idx (ash (logand local-address 377) 2))
;       (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
;    (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
;          (setq *moby-cached-vmem-idx*
;                (vmem-find-page dataspace-page)
;                *moby-cached-dataspace-page* dataspace-page)))
;    (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
;      (moby-parse-components (aref buf idx) (aref buf (1+ idx))
;                            (aref buf (+ 2 idx)) (aref buf (+ 3 idx))))))

(defun moby-parse-components-and-moby-bits (q0 q1 q2 q3)
  (declare (values type-symbol moby-bits moby-pointer cdr-code-and-data-type))
  (let* ((moby-bits (logior %moby-region-valid (ldb %%moby-internal-bits q3)))
         (boxed-code (ldb %%%moby-boxed moby-bits)))
    (cond ((= boxed-code 2)
           (values 'local-cell moby-bits nil (ldb (byte 7 9) q1)))
          ((zerop boxed-code)
           (values 'unboxed moby-bits q0 q2))
          (t (let ((moby-dt (ldb (byte 5 9) q1))
                   (ccdt (ldb (byte 7 9) q1)))
               (cond ((%pointer-type-p moby-dt)
                      (values 'pointer
                              moby-bits
                              (dpb-big (dpb q3
                                            (byte 9 16.)
                                            q2)
                                       (byte 25. 25.)
                                       (dpb q1  ;value of this dbp is bottom 25.
                                            (byte 9 16.)
                                            q0))
                              ccdt))
                     ((= moby-dt dtp-fix)
                                                ;moby fixnum might be bignum in local form..
                      (let* ((si:NUMBER-CONS-AREA WORKING-STORAGE-AREA)
                                        ;avoid screwws due to consing number in extra-pdl-area.
                             (ans
                               (cond ((zerop (ldb (byte 1 8) q3))
                                                ;positive number
                                      (dpb q3
                                           (byte 9 41.)
                                           (dpb q2
                                                (byte 16. 25.)
                                                (dpb q1
                                                     (byte 9. 16.)
                                                     q0))))
                                     (t
                                                ;negative number
                                      (dpb q3
                                           (byte 9 41.)
                                           (dpb q2
                                                (byte 16. 25.)
                                                (dpb q1
                                                     (byte 9. 16.)
                                                     (dpb q0
                                                          (byte 16. 0)
                                                          -1))))))))
                        (cond ((fixp ans))
                              ((not (= (%area-number ans) working-storage-area))
                               (ferror nil "failed to number cons in working-storage-area"))
                              )
                        (values 'fixnum moby-bits
                                ans
                                ccdt)))
                     (t
                      (values 'direct moby-bits
                              (dpb q1 (byte 9 16.) q0)
                              ccdt))))))))

(defun moby-pointer-type-p (data-type-code)
    "T if DATA-TYPE-CODE is a the code for a data type that points to storage."
  (NOT (MEMQ DATA-TYPE-CODE
             '(#.DTP-FIX #.DTP-SMALL-FLONUM #.DTP-CHARACTER
               #.DTP-TRAP #.DTP-SELF-REF-POINTER
   ;note that DTP-INSTANCE-HEADER IS a pointer!.
               #.DTP-HEADER #.DTP-ARRAY-HEADER
   ;note that DTP-HEADER-FORWARD IS a pointer!.
               ))))

(defun moby-parse-components (q0 q1 q2 q3)
  (declare (values type-symbol moby-pointer cdr-code-and-data-type))
  (let* ((boxed-code (ldb %%moby-boxed q3)))
    (cond ((= boxed-code 2)
           (values 'local-cell nil (ldb (byte 7 9) q1)))
          ((zerop boxed-code)
           (values 'unboxed q0 q2))
          (t (let ((moby-dt (ldb (byte 5 9) q1))
                   (ccdt (ldb (byte 7 9) q1)))
               (cond ((moby-pointer-type-p moby-dt)
                      (values 'pointer
                              (dpb-big (dpb q3
                                            (byte 9 16.)
                                            q2)
                                       (byte 25. 25.)
                                       (dpb q1  ;value of this dbp is bottom 25.
                                            (byte 9 16.)
                                            q0))
                              ccdt))
                     ((= moby-dt dtp-fix)
                                                ;moby fixnum might be bignum in local form..
                      (let* ((si:NUMBER-CONS-AREA WORKING-STORAGE-AREA)
                                        ;avoid screwws due to consing number in extra-pdl-area.
                             (ans
                               (cond ((zerop (ldb (byte 1 8) q3))
                                                ;positive number
                                      (dpb q3
                                           (byte 9 41.)
                                           (dpb q2
                                                (byte 16. 25.)
                                                (dpb q1
                                                     (byte 9. 16.)
                                                     q0))))
                                     (t
                                                ;negative number
                                      (dpb q3
                                           (byte 9 41.)
                                           (dpb q2
                                                (byte 16. 25.)
                                                (dpb q1
                                                     (byte 9. 16.)
                                                     (dpb q0
                                                          (byte 16. 0)
                                                          -1))))))))
                        (cond ((fixp ans))
                              ((not (= (%area-number ans) working-storage-area))
                               (ferror nil "failed to number cons in working-storage-area"))
                              )
                        (values 'fixnum
                                ans
                                ccdt)))
                     (t
                      (values 'direct
                              (dpb q1 (byte 9 16.) q0)
                              ccdt))))))))
;not currently used.
(defun moby-fetch-components (local-address)
  (declare (values moby-pointer cdr-code-and-data-type moby-bits))
  (let ((idx (ash (logand local-address 377) 2))
        (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
    (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
           (setq *moby-cached-vmem-idx*
                 (vmem-find-page dataspace-page)
                 *moby-cached-dataspace-page* dataspace-page)))
    (let* ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5))
           (q1 (aref buf (1+ idx)))
           (q3 (aref buf (+ 3 idx))))
      (values (dpb-big (dpb q3
                            (byte 9 16.)
                            (aref buf (+ idx 2)))
                       (byte 25. 25.)
                       (dpb q1  ;value of this dbp is bottom 25.
                            (byte 9 16.)
                            (aref buf idx)))
              (ldb (byte 7 9) q1)
              (ldb %%moby-internal-bits q3))))) ;flush external bits.

(defun moby-fetch-all (local-address)
  ;this one used for debugging, not time optimized.
  (declare (values full-value moby-bits cdr-code-and-data-type moby-pointer))
  (let ((idx (ash (logand local-address 377) 2))
        (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8) t)))
    (cond ((null dataspace-page)
           (return-from moby-fetch-all nil))
          ((not (= dataspace-page *moby-cached-dataspace-page*))
           (setq *moby-cached-vmem-idx*
                 (vmem-find-page dataspace-page)
                 *moby-cached-dataspace-page* dataspace-page)))
    (let* ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
      (values (dpb (aref buf (+ idx 3))
                   6020
                   (dpb (aref buf (+ idx 2))
                        4020
                        (dpb (aref buf (+ idx 1))
                             2020
                             (aref buf idx))))
              (ldb %%moby-internal-bits (aref buf (+ idx 3)))
              (ldb (byte 7 9) (aref buf (+ idx 1)))     ;cdr-code and data-type
              (dpb-big (dpb (aref buf (+ idx 3))
                            (byte 9 16.)
                            (aref buf (+ idx 2)))
                       (byte 25. 25.)
                       (dpb (aref buf (+ idx 1))        ;value of this dbp is bottom 25.
                            (byte 9 16.)
                            (aref buf idx)))))))

;not used, currently
(defun moby-store-components (local-address moby-pointer cdr-code-and-data-type moby-bits)
  (let ((idx (ash (logand local-address 377) 2))
        (dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
    (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
           (setq *moby-cached-vmem-idx*
                 (vmem-find-page dataspace-page)
                 *moby-cached-dataspace-page* dataspace-page)))
    (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
      (aset (ldb (byte 16. 0) moby-pointer) buf idx)            ;low 16 bits.
      (aset (dpb cdr-code-and-data-type (byte 7 9) (ldb (byte 9. 16.) moby-pointer))
            buf (1+ idx))       ;next 9
      (aset (ldb (byte 16. 25.) moby-pointer) buf (+ idx 2))    ;next 16
      (aset (dpb (ldb %%moby-external-bits (aref buf (+ idx 3)))  ;preserve external bits.
                 %%moby-external-bits
                 (dpb moby-bits %%moby-internal-bits (ldb (byte 9 41.) moby-pointer)))  ;highest 9
            buf (+ idx 3))
      (setf (aref *vmem-pages* *moby-cached-vmem-idx* 3) t)
      nil)))

(defun moby-declare-local-cell (local-address)
  "Declare local-address to be a local cell.  This means it does not get dereconciled,
and always gets reconciled as NIL.  The CDR-CODE is still meaningful."
  (let* ((area-number (%area-number local-address))
         (local-p (%pointer local-address))
         (local-page (lsh local-p -8)))
    (cond ((aref *area-to-msa* area-number)  ;do nothing if local area.
           (let ((vp (aref #'system:virtual-page-data local-page)))
             (cond ((zerop (ldb si:%%virtual-page-moby-bits-complete vp))
                    ;;;do the whole thing, both from moby space and freshly consed.
                    (moby-validate-moby-bits local-p nil))))
           (let* ((region (%region-number local-p))
                  (region-moby-bits-array (aref #'sys:region-moby-bits-array region))
                  (region-origin (si:%region-origin region))
                  (region-relative-local-address
                    (%pointer-difference local-p region-origin))
                  (moby-bits (aref region-moby-bits-array region-relative-local-address))
                  (boxed-code (ldb %%%moby-boxed moby-bits)))
             (cond ((zerop (logand %moby-region-valid moby-bits))
                    (ferror nil "moby bits not valid for local cell"))
                   ((not (= boxed-code 2))
                    (setf (aref region-moby-bits-array region-relative-local-address)
                          (dpb 2 %%%moby-boxed moby-bits))
                    (setf (ldb si:%%virtual-page-clean    ;make sure page gets written next time.
                               (aref #'system:virtual-page-data local-page))
                          0)
;                   (let ((idx (ash (logand local-p #o377) 2))
;                         (dataspace-page (moby-local-page-to-dataspace-page local-page)))
;                     (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
;                            (setq *moby-cached-vmem-idx*
;                                  (vmem-find-page dataspace-page)
;                                  *moby-cached-dataspace-page* dataspace-page)))
;                     (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
;                       (aset 0 buf idx)
;                       (aset (ash (dpb (%p-ldb-offset %%q-cdr-code 0 local-p)
;                                       (byte 2 5)
;                                       dtp-fix)
;                                  9)
;                             buf
;                             (1+ idx))
;                       (aset 0 buf (+ idx 2))
;                       (aset (dpb 2 %%moby-boxed (aref buf (+ idx 3)))
;                             buf
;                             (+ idx 3))
;                       (setf (aref *vmem-pages* *moby-cached-vmem-idx* 3) t)))
                    )))))))

(defun moby-structure-handle-of-local-address (local-address)
  ;this one used for debugging, not time optimized.
  (declare (values structure-handle))
    (let ((dataspace-page (moby-local-page-to-dataspace-page (lsh local-address -8))))
      (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
             (setq *moby-cached-vmem-idx*
                   (vmem-find-page dataspace-page)
                   *moby-cached-dataspace-page* dataspace-page)))
      (let ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5)))
        (moby-read-structure-handle-from-external-bits buf))))


(defun moby-store-structure-handle-into-external-bits (sh buf)
  ;structure-handle is 18. bits.  store them in external bits of first 9 MOBY-Qs of buf.
  (if (> (ldb si:%%virtual-page-initial-qs sh) 400)
      (ferror nil "Initial Qs too big in structure handle"))
  (if (> (ldb si:%%virtual-page-first-header sh) 400)
           (ferror nil "First header too big in structure handle"))
  (do ((s sh (ash s -2))
       (c 0 (1+ c))
       (idx 3 (+ idx 4)))
      ((= c 9))
    (aset (dpb s %%moby-external-bits (aref buf idx))
          buf idx)))

(defun moby-read-structure-handle-from-external-bits (buf)
  (do ((s 0)
       (p (byte 2 0) (+ p (byte 0 2)))
       (idx 3 (+ idx 4))
       (c 0 (1+ c)))
      ((= c 9)
       (if (> (ldb si:%%virtual-page-initial-qs s) 400)
           (ferror nil "Initial Qs too big in structure handle"))
       (if (> (ldb si:%%virtual-page-first-header s) 400)
           (ferror nil "First header too big in structure handle"))
       s)
    (setq s (dpb (ldb %%moby-external-bits (aref buf idx))
                 p
                 s))))

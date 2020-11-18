 ;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Readtable:CL; Base:8 -*-

;;; (c) Copyright 1985, Lisp Machine Incorporated (LMI).

;;; This is a cold-load file.  See also STORAGE-DEFS.

; forwarded to a-mem
(defvar-resettable %inhibit-read-only nil nil
  "Bind this to T to do nasty horrible things behind the virtual memory system's back.")

(defvar *area-list* :unbound
  "Call (CURRENT-AREA-LIST) for list of active areas.")

(defun %region-free-pointer (region)
  (without-interrupts
    (compiler::invalidate-cons-caches)
    ;(%p-contents-offset (%region-origin region-free-pointer) region)
    (aref #'region-free-pointer region)
    ))

(defun set-%region-free-pointer (region value)
  (without-interrupts
    (compiler::invalidate-cons-caches)
    ;(%p-store-contents-offset value (%region-origin region-free-pointer) region)
    (setf (aref #'region-free-pointer region) value)
    ))

;(defsetf %region-free-pointer set-%region-free-pointer) -- in storage-defs


;(defun %reset-temporary-area (area &optional inhibit-error)
;  "Reclaim all storage associated with AREA.  There must not be any references to storage
;in the area.  References from unused storage are not permitted.  References from active
;stack frames are not permitted.  References from internal processor registers are not
;permitted.  References from other stack groups, including inactive ones, are not permitted.
;In short, you shouldn't be using this.  Use the garbage collector."
;  (unless (or inhibit-error (area-temporary? area))
;    (multiple-cerror () ()
;                    ("The area ~S (~S) was not created as temporary." (area-name area) area)
;      ("Don't reset this area" (return-from %reset-temporary-area nil))
;      ("Make area temporary, and the reset it" (make-area-temporary area))))
;  (without-interrupts
;    ;; We can't just iterate over the region tables here (because %free-region modifies
;    ;; them), so we build a list of the regions we want to free, then %free-region them.
;    (mapc #'%free-region
;         (loop for region = (%area-region-list area) then (%region-list-thread region)
;               until (minusp region)
;               collect region))))

(defun reset-temporary-area (area &optional inhibit) area inhibit
  ;; Let's put the fear of God into casual users of this thing.
;  (multiple-cerror () ()
;                  ("RESET-TEMPORARY-AREA is obsolete and dangerous.")
;    ("Don't reset this area." ())
;    ("Reset this area using SI::%RESET-TEMPORARY-AREA." (%reset-temporary-area area inhibit)))
  (ferror nil "RESET-TEMPORARY-AREA is no longer supported.")
  )

(make-obsolete reset-temporary-area "is no longer supported.")

;(defun %reset-region-free-pointer (region new-fp)
;  (unless inhibit-scheduling-flag
;    (ferror "This function must be called with scheduling inhibited."))
;  (let ((old-fp (%region-free-pointer region)))
;    (when (< new-fp old-fp)
;      (setf (%region-free-pointer region) new-fp)
;      ;; Reset the structure-handles in the affected area.  On the first page
;      ;; (page-number new-fp), reset the first-header iff it needs to be lower.
;      ;; For the following pages, just indicate no header and no initial qs.
;      ;; Careful about the very last page -- if it's in the next region don't
;      ;; touch it.
;      (when (= old-fp (%region-length region)) (decf old-fp))
;      (setq old-fp (%pointer-plus old-fp (%region-origin region)))
;      (setq new-fp (%pointer-plus new-fp (%region-origin region)))
;      ;; If this function ever needs to be fast, use %BLT here.
;      (loop initially
;             (when (> (page-first-header (page-number new-fp)) (page-index new-fp))
;               (setf (page-first-header (page-number new-fp)) (page-index new-fp)))
;           for page from (1+ (page-number new-fp)) to (page-number old-fp)
;           do (setf (page-first-header page) #o400)
;           do (setf (page-initial-qs page) 0))
;      (%gc-scav-reset region))))

;(make-obsolete %reset-region-free-pointer "use garbage collector")

;(defun %free-region (region)
;  "Removes all trace of REGION from the area, virtual memory, and GC tables."
;  (unless inhibit-scheduling-flag
;    (ferror "This function must be called with scheduling inhibited."))
;  (let ((area (%region-area region))
;       (area-region-list-base (%region-origin sys:area-region-list))
;       (region-list-thread-base (%region-origin sys:region-list-thread)))
;    ;; This function needs to be pretty fast, to keep GC:RECLAIM-OLDSPACE from
;    ;; consuming too much time without interrupts.  Define some magic accessors
;    ;; for the relevant region tables.  (Note the local variables above.)
;    (macrolet ((%area-region-list (area)
;                `(%p-pointer (+ area-region-list-base ,area)))
;              (%region-list-thread (region)
;                `(%p-pointer (+ region-list-thread-base ,region))))
;      ;; If it's the first region in the area, delete from the start of the thread.
;      (if (eq region (%area-region-list area))
;         (setf (%area-region-list area) (%region-list-thread region))
;       ;; Otherwise search for the region and snap it out of the thread.
;       (loop with this = (%area-region-list area)
;             for next = (%region-list-thread this)
;             until (eq next region)
;             do (setq this next)
;             finally (setf (%region-list-thread this) (%region-list-thread next)))))
;    (%gc-free-region region)))

(defun %deallocate-end-of-region (region)
  "Return unused quantums in region to free pool."
  (unless inhibit-scheduling-flag
    (ferror "This function must be called with scheduling disabled."))
  (let ((quantum-size %address-space-quantum-size)
        (origin (%region-origin region))
        (length (%region-length region))
        (free-pointer (%region-free-pointer region)))
    ;; If less than one quantum long, or if there is less than one quantum of free space,
    ;; don't do anything.  It is illegal to have regions with no quantums.
    (unless (or (<= length quantum-size)
                (<= (- length free-pointer) quantum-size))
      (loop with first-free-quantum = (ceiling free-pointer quantum-size)
            with new-length = (* first-free-quantum quantum-size)
            with origin-quantum = (truncate (si::%pointer-unsigned origin) quantum-size)
            with array = #'address-space-map
            initially (setf (%region-length region) new-length)
            for quantum from first-free-quantum below (truncate length quantum-size)
            do (setf (aref array (+ origin-quantum quantum)) 0)
            finally (%deallocate-pages (%pointer-plus origin new-length)
                                       (%pointer-plus origin length))))))

(defun %deallocate-pages (vma-start vma-bound)
  "Remove the pages between START and BOUND from the maps and the page-hash-table."
  (unless inhibit-scheduling-flag
    (ferror "This function must be called with scheduling disabled."))
  (loop with page = page-size           ;Yucky special variable.
        ;; Map gets Map Status: read/write unmodified, Map Access: no access.
        with bits = #o300
        ;; Bit 30 in swap-status argument means disconnect virtual page from page frame.
        ;; It also makes the page unmodified in the other way, the details of which
        ;; I almost understood at one point, but no longer.  Yech-oh.
        with swap-status = (%logdpb 1 (byte 1 #o30) %pht-swap-status-flushable)
        for address = vma-start then (%pointer-plus address page)
        until (= address vma-bound)
        do (%change-page-status address swap-status bits)))

(defun %invalidate-region-mapping (region)
  "Decache all information about REGION from the virtual memory maps."
  (loop with page = page-size
        with origin = (%region-origin region)
        with bound = (%pointer-plus origin (%region-length region))
        for address = origin then (%pointer-plus address page)
        until (eq address bound)
        do (%change-page-status address nil nil)))

(defun %invalidate-area-mapping (area)
  "Decache all information about AREA from the virtual memory maps."
  (for-every-region-in-area (region area)
    (%invalidate-region-mapping region)))

;;; The following functions don't work at all.

(defun %make-region-not-read-only (region)
  (let* ((old-region-bits (%region-bits region))
         (old-access-status-meta (ldb %%region-map-bits old-region-bits))
         (new-access-status-meta (%logdpb %pht-map-status-read-write-first
                                          %%region-map-status-code
                                   (%logdpb 3 %%region-map-access-code old-region-bits)))
         (new-region-bits (%logdpb new-access-status-meta
                                   %%region-map-bits
                                   old-region-bits)))
    (declare (ignore old-access-status-meta))
    (setf (%region-bits region) new-region-bits)
    (loop with page = page-size
          with origin = (%region-origin region)
          with bound = (%pointer-plus origin (%region-length region))
          for address = origin then (%pointer-plus address page)
          until (eq address bound)
          do (%change-page-status address nil new-region-bits))))

(defun %make-region-read-only (region)
  (let* ((old-region-bits (%region-bits region))
         (old-access-status-meta (ldb %%region-map-bits old-region-bits))
         (new-access-status-meta (%logdpb %pht-map-status-read-only
                                          %%region-map-status-code
                                   (%logdpb 2 %%region-map-access-code old-region-bits)))
         (new-region-bits (%logdpb new-access-status-meta
                                   %%region-map-bits
                                   old-region-bits)))
    (declare (ignore old-access-status-meta))
    (setf (%region-bits region) new-region-bits)
    (loop with page = page-size
          with origin = (%region-origin region)
          with bound = (%pointer-plus origin (%region-length region))
          for address = origin then (%pointer-plus address page)
          until (eq address bound)
          do (%change-page-status address nil new-region-bits))))

;;;

(defun current-area-list ()
 "Use this instead of the variable AREA-LIST.  That can get clobbered if someone
reads in QCOM or something."
 (if (not (boundp '*area-list*))
     (setq *area-list* (g-l-p (symbol-function 'area-name))))
 *area-list*
  ;(g-l-p (symbol-function 'area-name)) was the old thing.  As areas get deallocated,
  ;  can be completely wrong now.
  )

(defun allocated-regions ()
  (loop for region from 0 below sys:number-of-regions
        counting ( (%region-type region) %region-space-free)))

(defun allocated-areas ()
  (length (current-area-list))  ;(fill-pointer (symbol-function 'area-name))
  )



;(defvar area-temporary-flag-array :unbound
;  "Array index by area number containing 1 if area is temporary, else 0.")

(defun area-temporary-p (area) area
  "Return T if the specified area is a temporary area."
;  (not (zerop (aref area-temporary-flag-array area)))
  nil
  )

(defun area-temporary? (area) area
  "Return T if the specified area is a temporary area."
;  (not (zerop (aref area-temporary-flag-array area)))
  nil
  )

;(defun make-area-temporary (area)
;  "Mark an area (specified by number) as temporary."
;  (setf (aref area-temporary-flag-array area) 1))



(defvar *room* :unbound
  "Areas to mention when ROOM is called with no arguments.")
(forward-value-cell '*room* 'room)

(defun make-area (&key name
                       (region-size #o40000)
                       (gc :dynamic)
                       (read-only ())
                       (volatility 3 volatilityp)
                       (room ())
                       (swap-recommendations 0)
                       &allow-other-keys)
  "Create a new area, or modify an existing one.  Returns the area number.
Takes keyword argument pairs as follows:
:NAME - Symbol which provides name of area.  This symbol, which must be supplied, is
        SET to the area number.
:REGION-SIZE - size for regions, between #o40000 and #o4000000 words.
:GC - :DYNAMIC - garbage-collector treats this area normally (the default);
      :STATIC - garbage-collector ignores this area;
      :FIXED - garbage-collector ignores this area, which may not be consed in.
      :MOBY-CONSABLE - can be consed in.  Moby regions are effectively STATIC for gc purposes.
      :MOBY-FIXED - already allocated or consable on another machine. Not consable here.
:VOLATILITY - The volatility (number between 0 and 3) of newspace regions in this area.
:READ-ONLY - If T, the area may not be written or consed in.
:ROOM - if specified, push this area onto ROOM, so that (ROOM) will list it.
:SWAP-RECOMMENDATIONS - pages prefetched upon a page fault."
  (declare (unspecial room))
;  (unless (variable-boundp area-temporary-flag-array)
;    (setq area-temporary-flag-array (make-array #o400 :element-type 'bit)))
  (check-type name (and symbol (not null)))
  (check-type region-size (integer #o40000 #o4000000))
  (check-type gc (member :static :dynamic :fixed :moby-consable :moby-fixed))
  (check-type volatility (integer 0 3))
  (check-type read-only (member t nil))
  (check-type swap-recommendations (integer 0 31.))
  (and volatilityp (neq gc ':dynamic)
       (ferror "~S specified, but ~S is not ~S" :volatility :gc :dynamic))
  (let ((bits (%logdpb (case gc
                         (:static %region-space-static)
                         ;(:temporary %region-space-static)
                         (:dynamic %region-space-new)
                         (:fixed %region-space-fixed)
                         (:moby-consable %region-space-moby-new)
                         (:moby-fixed %region-space-moby-fixed))
                       %%region-space-type
               ;; I think you have to say "no scavenge" for the area so
               ;; new space regions don't get scavenged.  when the microcode
               ;; allocates a copy region, it automatically turns this on.
               ;; This is the same behavior as the cold load builder.  - Pace 17-Feb-86
               (%logdpb (ecase gc
                          (:static 1)
                          ;(:temporary 1)
                          (:dynamic 0)
                          (:fixed 1)
                          (:moby-consable 1)
                          (:moby-fixed 1))
                        %%region-scavenge-enable
                (%logdpb swap-recommendations %%region-swapin-quantum
                 (%logdpb volatility %%region-volatility
                  (%logdpb 1 %%region-oldspace-meta-bit
                   (%logdpb 1 %%region-extra-pdl-meta-bit
                    (%logdpb 2 %%region-representation-type
                     (%logdpb (if read-only
                                  %pht-map-status-read-only
                                %pht-map-status-read-write-first)
                              %%region-map-status-code
                      (%logdpb (if read-only 2 3)
                               %%region-map-access-code
                               0))))))))))
        (number))
    (without-interrupts
      (gc:without-scavenging
        (gc:without-flipping
          (cond ((memq name (current-area-list))
                 (setq number (symbol-value name)))
                (t
                 (setq number (aref #'system-communication-area %sys-com-free-area#-list))
                 (when (= number 0)
                   (ferror "Out of area numbers, cannot create ~S" name))
                 (setf (aref #'system-communication-area %sys-com-free-area#-list)
                       (%area-region-list number))
                 (setf (%area-region-list number)
                       (%logdpb 1 %%q-boxed-sign-bit number))
                                                ;(setf (array-leader #'area-name 0) number)
                                                ;(vector-push name #'area-name)
                 (setf (aref #'area-name number) name)
                 (setf (symbol-value name) number)
                 (do ((p (current-area-list) (cdr p))
                      (last-p (value-cell-location '*area-list*) p))
                     ((null p)
                      (rplacd last-p (list name)))
                   (cond ((not (< (symeval (car p)) number))
                          (return (rplacd last-p
                                          (cons name (cdr last-p)))))))))
          (setf (%area-region-size number) region-size)
          (setf (%area-region-bits number) bits)
          (when (and room (not (memq name *room*)))
            (push name *room*))
;         (when (eq gc ':temporary)
;           (make-area-temporary number))
          number)))))

(defun rename-area (old-area-name new-area-name &aux tem)
  "Change the name of an area.  This should not be done casually."
  (check-type old-area-name (and symbol (not null)))
  (check-type new-area-name (and symbol (not null)))
  (let ((number (symbol-value old-area-name)))
    (without-interrupts
      (gc:without-scavenging
        (gc:without-flipping
          (cond ((null (setq tem (memq old-area-name (current-area-list))))
                 (ferror "~S is not an active area" old-area-name))
                ((not (eq old-area-name
                          (aref #'area-name number)))
                 (ferror "Area structure for ~s inconsistant" old-area-name)))
          (setf (aref #'area-name number) new-area-name)
          (rplaca tem new-area-name)
          (makunbound old-area-name)
          (setf (symbol-value new-area-name) number)
          (cond ((setq tem (memq old-area-name *room*))
                 (rplaca tem new-area-name)))
          number)))))

(defun delete-null-areas ()
  (dolist (a-n (current-area-list))
    (let ((area-number (symbol-value a-n))
          (count 0))
      (for-every-region-in-area (region area-number)
        (incf count))
      (if (zerop count)
          (delete-area a-n)))))

(defun delete-area (a-n &aux tem)
  "Delete an area name.  Area must have 0 regions."
  (check-type a-n (and symbol (not null)))
  (let ((number (symbol-value a-n)))
    (cond ((not (eq (%area-region-list number)
                    (%logdpb 1 %%q-boxed-sign-bit number)))
           (ferror "Area to be deleted has regions.")))
    (cond ((null (setq tem (memq a-n (current-area-list))))
           (ferror "~S is not an active area" a-n))
                ((not (eq a-n
                          (aref #'area-name number)))
                 (ferror "Area structure for ~s inconsistant" a-n)))
    (without-interrupts
      (gc:without-scavenging
        (gc:without-flipping
          (setq *area-list* (delq a-n *area-list*))
          (setf (%area-region-list number)
                (aref #'system-communication-area %sys-com-free-area#-list))
          (setf (aref #'system-communication-area %sys-com-free-area#-list)
                number)
          (setf (aref #'area-name number) nil)
          (makunbound a-n)
          (setq *room* (delq a-n *room*))
          number)))))

;;; Structure-handles initialization.  This is called right at the beginning
;;; of SI::QLD.

(defun setup-structure-handles-for-region (region)
  (loop with origin = (%region-origin region)
        with object = origin
        with page = -1
        until (= object (%pointer-plus origin (%region-free-pointer region)))
        for boxed = (%structure-boxed-size object)
        for total = (%structure-total-size object)
        when ( (page-number object) page)
          do (setf (page-first-header (page-number object)) (page-index object))
        for boundary = (+ (page-index object) boxed)
        when (> boundary #o400)
          do (loop for p from (1+ (page-number object))
                   do (decf boundary #o400)
                   until (< boundary #o400)
                   do (setf (page-first-header p) #o400)
                   do (setf (page-initial-qs p) #o400)
                   finally (setf (page-initial-qs p) (page-index boundary)))
        do (setq page (page-number object))
        do (setq object (%pointer-plus object total))
        finally
          (when (= (page-first-header (page-number object)) #o400)
            (setf (page-first-header (page-number object)) (page-index object)))))

(defun setup-structure-handles-for-area (area)
  (for-every-region-in-area (region area)
    (initialize-structure-handles-for-region region)
    (unless (= (%region-representation-type region) %region-representation-type-unstructured)
      (setup-structure-handles-for-region region))))

(defun initialize-structure-handles-for-region (region)
  "Define every page in the region to contain #o400 unboxed words."
  (loop with origin = (%region-origin region)
        with bound = (%pointer-plus origin (%region-length region))
        for page from (page-number origin) below (page-number bound)
        do (setf (page-first-header page) #o400);No header on this page.
        do (setf (page-initial-qs page) 0)))    ;No initial Qs on this page.

(defvar *structure-handles-setup* nil)

(defun setup-structure-handles ()
  (loop for symbolic-area in (current-area-list)
        for area = (symbol-value symbolic-area)
        do (setup-structure-handles-for-area area)
        finally
          (setf (%region-free-pointer virtual-page-data) (%region-length virtual-page-data)))
  (setq *structure-handles-setup* t)
  (enable-structure-handles-error-checks))

(defun enable-structure-handles-error-checks ()
  (if *structure-handles-setup*
      (%p-dpb 1 %%m-flags-check-structure-handles (locf si:%mode-flags))))

(add-initialization 'enable-structure-handles-error-checks
                    '(enable-structure-handles-error-checks)
                    :system)



;;; This is a moderately critical function for the gc process, and is currently crippled by
;;; the (aref #'address-space-map ...).  This function could be made about 4 times faster by
;;; converting the address space map into a one-word-per-quantum table, which could be scanned
;;; quickly using the sort of address arithmetic used in gc::compute-storage-distribution.
(defun unallocated-space ()
  "The amount of space not allocated to any region."
  ;; LOOP can't (declare (unspecial base))
  (let ((base (truncate (%pointer-plus (%region-origin init-list-area)
                                       (%region-length init-list-area))
                        %address-space-quantum-size))
        (bound (truncate virtual-memory-size %address-space-quantum-size)))
    (declare (unspecial base))
    (* (loop for i from base below bound
          count (zerop (aref #'address-space-map i)))
       %address-space-quantum-size)))

(defun unused-space ()
  "The amount of space allocated to regions but not yet used by them."
  (with-quick-region-area-accessors
    (loop with bound = sys:number-of-regions
          with %%type = sys:%%region-space-type
          for region from 0 below bound
          when (memq (%logldb %%type (%region-bits region)) '#.(list %region-space-new
                                                                     %region-space-copy
                                                                     %region-space-static))
          sum (- (%region-length region) (%region-free-pointer region)))))

;;; To be conservative, "free space", as far as the human or the GC are concerned,
;;; is the amount of space in unallocated quantums.  Unused space in regions may or
;;; may not be usable.

(deff free-space 'unallocated-space)
(deff get-free-space-size 'unallocated-space)           ;Crufty name has proliferated.

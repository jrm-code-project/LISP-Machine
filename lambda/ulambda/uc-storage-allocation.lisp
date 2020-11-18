;;; -*- Mode:LISP; Base:8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984,1985 - Lisp Machine Incorporated.
;;;

(DEFCONST UC-STORAGE-ALLOCATION '(

;;; first a word about list-headers.  This comment used to be right after the definition of GET-COPY-REGION
;;; and just before the definitions of INSERT-LIST-HEADER and INSERT-LIST-HEADER-COPY.  These used to be called
;;; conditionally from ALLOCATE-LIST-STORAGE-UNCACHED and ALLOCATE-LIST-COPY-STORAGE-UNCACHED respectively.
;;; The insert-list-header subroutines have now been massaged into their caller functions because of a confilct
;;; between list-header consing and the initializing of memory by TOUCH-PAGES-IN-NEW-OBJECT.

;;; The REGION-ALLOCATION-STATUS array controls the insertion of list-headers to mark
;;; transitions from structure-storage to list-storage.  A list-header must be inserted
;;; whenever a list is consed immediately after a structure is consed in the same region.
;;; Also, we insert list-headers occasionally during long runs of list consing, to
;;; speed up FIND-STRUCTURE-HEADER.

;;; The elements of REGION-ALLOCATION-STATUS are small untyped integers that represent
;;; the amount of list structure consed since the last list-header.  If this gets to be
;;; larger than A-LIST-ALLOCATION-THRESHOLD, we insert a list-header and reset the count.  When
;;; a structure is consed the REGION-ALLOCATION-STATUS is set to infinity to ensure that
;;; a header will be inserted the next time through.

(locality a-mem)
a-list-allocation-threshold (20)
(locality i-mem)


;;; These are not callable with MISC instructions, only as functions.
;;; They are documented as taking an &REST argument but actually take 63 optional args.
;;; When entered, the arguments are on the stack and M-R contains the number of them.
;;; (M-AP)+1 is the first argument, (PP) is the last.

        (misc-inst-entry list)
xlist   (jump-equal m-r a-zero xfalse)
        ((m-b) q-pointer m-r)
        (call-return allocate-list-storage-default xlist0)

        (misc-inst-entry list*)
xlistr  (jump-equal m-r (a-constant 1) poptj)
        ((m-b) q-pointer m-r)
        (call-return allocate-list-storage-default xlistr0)

;;; Note that these two never pop their first argument.  This doesn't matter when
;;; calling them as functions, but if you try to make a MISC-instruction interface
;;; to these you will need to be aware of that.

        (misc-inst-entry list-in-area)
xlista  (jump-equal m-r (a-constant 1) xfalse)
        ((pdl-buffer-index) add m-ap (a-constant 1))
        ((m-s) q-typed-pointer c-pdl-buffer-index)
        ((m-b) sub m-r (a-constant 1))
        (call-return allocate-list-storage xlist0)

        (misc-inst-entry list*-in-area)
xlistra (jump-equal m-r (a-constant 2) poptj)
        ((pdl-buffer-index) add m-ap (a-constant 1))
        ((m-s) q-typed-pointer c-pdl-buffer-index)
        ((m-b) sub m-r (a-constant 1))
        (call-return allocate-list-storage xlistr0)

;;; The above scheme has been replaced with the following four misc instructions
;;; for a significant performance improvement.

;;; %INTERNAL-LIST,%INTERNAL-LIST* calling sequence:
;;; --> Number of elements
;;;     Last element
;;;     ...
;;;     First element

x-internal-list (misc-inst-entry %internal-list)
        (call-xct-next allocate-list-storage-default)
       ((m-b) q-pointer pdl-pop)                       ;Number of elements.
xlist0  ((md) ldb pdl-pop q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-nil)))
        ((vma) add m-t a-b)
        ((m-t) dpb q-pointer m-t (a-constant (byte-value q-data-type dtp-list)))
xlist1  ((vma-start-write) sub vma (a-constant 1))
        (check-page-write)
        (gc-write-test)
        ((m-b) sub m-b (a-constant 1))
xlist2  (popj-less-or-equal m-b a-zero)
        (jump-xct-next xlist1)
       ((md) ldb pdl-pop q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))

x-internal-list* (misc-inst-entry %internal-list*)
        (call-xct-next allocate-list-storage-default)
       ((m-b) q-pointer pdl-pop)                       ;Number of elements.
xlistr0 ((vma) add m-t a-b)
        ((m-t) dpb q-pointer m-t (a-constant (byte-value q-data-type dtp-list)))
        ((md) ldb pdl-pop q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-error)))
        ((vma-start-write) sub vma (a-constant 1))
        (check-page-write)
        (gc-write-test)
        ((md) ldb pdl-pop q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-normal)))
        ((vma-start-write) sub vma (a-constant 1))
        (check-page-write)
        (gc-write-test)
        (jump-xct-next xlist2)
       ((m-b) sub m-b (a-constant 2))

;;; %INTERNAL-LIST-IN-AREA,%INTERNAL-LIST*-IN-AREA calling sequence:
;;; --> Area
;;;     Number of elements
;;;     Last element
;;;     ...
;;;     First element

        (misc-inst-entry %internal-list-in-area)
x-internal-list-in-area
        ((m-s) q-typed-pointer pdl-pop)                ;Area.
        ((m-b) q-pointer pdl-pop)                      ;Number of elements.
        (call-return allocate-list-storage xlist0)

        (misc-inst-entry %internal-list*-in-area)
x-internal-list*-in-area
        ((m-s) q-typed-pointer pdl-pop)                ;Area.
        ((m-b) q-pointer pdl-pop)                      ;Number of elements.
        (call-return allocate-list-storage xlistr0)


;THIS IS THE NEW PRIMITIVE CALLED BY MAKE-LIST.  ARGUMENTS ARE
;INITIAL-VALUE, AREA, LENGTH.

(ERROR-TABLE DEFAULT-ARG-LOCATIONS %MAKE-LIST PP M-S M-B)

        (misc-inst-entry %make-list)
make-list
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 2)
        (call-if-bit-set boxed-sign-bit pdl-top trap)
     (error-table argtyp nonnegative-fixnum pp 2)
        ((m-b) q-pointer pdl-pop)               ;third arg number of qs
        ((m-s) q-typed-pointer pdl-pop)         ;second arg area
        (jump-equal m-b a-zero pop-then-xfalse) ;zero length list is nil
        (call list-of-things)                   ;takes initial-value on stack
        (popj-after-next
          (m-t) q-pointer m-t (a-constant (byte-value q-data-type dtp-list)))
       (no-op)


;;; Basic consing instructions.

        (misc-inst-entry xcons)
xxcons  (call-xct-next allocate-list-storage-default)
       ((m-b) (a-constant 2))
        (call-return exchange-top-of-stack store-values-in-cons)

        (misc-inst-entry xcons-in-area)
xxcona  ((m-s) q-typed-pointer pdl-pop)                ;Area from third argument.
        (call-xct-next allocate-list-storage)
       ((m-b) (a-constant 2))
        (call-return exchange-top-of-stack store-values-in-cons)

        (misc-inst-entry ncons-in-area)
xncona  ((m-s) q-typed-pointer pdl-pop)                ;Area from second argument.
        (jump-xct-next qcons)
       ((pdl-push) a-v-nil)

        (misc-inst-entry ncons)
xncons  ((pdl-push) a-v-nil)                           ;Fake second argument, then fall through.

        (misc-inst-entry cons)
xcons   (call-xct-next allocate-list-storage-default)
       ((m-b) (a-constant 2))
store-values-in-cons
        ((md) dpb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-error)))
        ((vma-start-write) add m-t (a-constant 1))
        (check-page-write)
        (gc-write-test)
        ((md) dpb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-normal)))
        ((vma-start-write m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-list)))
        (check-page-write)
        (gc-write-test)
        (popj)

        (misc-inst-entry cons-in-area)
xconsa  ((m-s) q-typed-pointer pdl-pop)                ;Area from third argument.
qcons   ((m-b) (a-constant 2))
        (call-return allocate-list-storage store-values-in-cons)

(begin-comment) Zwei Lossage (end-comment)

;;; There are two cons caches, one for active storage allocation, and one for the transporter.
;;; Repeated allocation in the same area will hit the cache, saving the time needed to find
;;; an appropriate region in the area to cons in.  The amount of allocation you can do without
;;; going through the slow path is limited to the current page; new pages have to be explicitly
;;; created by the slow path, and the structure handles need to get set up for objects that cross
;;; page boundaries.

;;; The cache for copy regions also records region volatility, since the transporter conses in
;;; different regions within an area depending on the volatility of the copied objects.

(locality a-mem)
a-active-cons-cache-area (-1)           ;or -1 if none.
a-active-cons-cache-region (-1)         ;or -1 if none.
  ;note: all flavors of region-origin, region-length, and free-pointer are now clean untyped
  ; quanities.
a-active-cons-cache-region-origin (0)
a-active-cons-cache-free-pointer (0)
a-active-cons-cache-free-limit (0)
a-active-cons-cache-allocation-status (0)
a-copy-cons-cache-area (-1)             ;or -1 if none.
a-copy-cons-cache-region (-1)           ;or -1 if none.
a-copy-cons-cache-region-origin (0)
a-copy-cons-cache-free-pointer (0)
a-copy-cons-cache-free-limit (0)
a-copy-cons-cache-allocation-status (0)
a-copy-cons-cache-volatility (0)
;; This is 4x page-size, so scavenger work is at least 4x cons work.
a-scavenge-work-while-consing (2000)
(locality i-mem)

     (misc-inst-entry invalidate-cons-caches)
invalidate-cons-caches
        (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
        (call-not-equal m-minus-one a-copy-cons-cache-area invalidate-copy-cons-cache)
        (popj)

invalidate-active-cons-cache
        (popj-equal a-active-cons-cache-region m-minus-one)
        ((m-3) a-active-cons-cache-region)
        ((vma) add m-3 a-v-region-free-pointer)
        ((md) a-active-cons-cache-free-pointer)
        ((md-start-write) output-selector-mask-25 sub md a-active-cons-cache-region-origin)
        (check-page-write-unboxed)
        ((a-active-cons-cache-area) m-minus-one)
        ((a-active-cons-cache-region) m-minus-one)             ;Scavenger depends on this.
        ((vma) add m-3 a-v-region-allocation-status)
        (popj-after-next
          (md-start-write) a-active-cons-cache-allocation-status)
       (check-page-write-unboxed)

invalidate-copy-cons-cache
        (popj-equal a-copy-cons-cache-region m-minus-one)
        ((m-3) a-copy-cons-cache-region)
        ((vma) add m-3 a-v-region-free-pointer)
        ((md) a-copy-cons-cache-free-pointer)
        ((md-start-write) output-selector-mask-25 sub md a-copy-cons-cache-region-origin)
        (check-page-write-unboxed)
        ((a-copy-cons-cache-area) m-minus-one)
        ((a-copy-cons-cache-region) m-minus-one)               ;Scavenger depends on this.
        ((vma) add m-3 a-v-region-allocation-status)
        (popj-after-next
          (md-start-write) a-copy-cons-cache-allocation-status)
       (check-page-write-unboxed)

;;; Storage allocation primitives.

;;; Call with area in M-S, total-size in M-B, and boxed-size in M-A (not necessary for
;;; list allocation).  Result in M-T is address (with invalid data-type!) of uninitialized
;;; block of storage.  It's up to you to make things valid post haste.

allocate-bignum-storage
     ;; Convenient entry for 1 boxed word + M-B unboxed words.  Saves M-A because the bignum
     ;; code depends on it in mysterious ways.
        ((pdl-push) m-a)
        (call-xct-next allocate-extended-number-storage)
       ((m-a) (a-constant 1))
        (popj-after-next
          (m-a) pdl-pop)
       (no-op)

allocate-extended-number-storage
        ((m-s) dpb m-zero q-all-but-typed-pointer a-number-cons-area)
allocate-structure-storage
        (jump-not-equal m-s a-v-nil allocate-structure-storage-kernel)
allocate-structure-storage-default
        ((m-s) dpb m-zero q-all-but-typed-pointer a-default-cons-area)
allocate-structure-storage-kernel
        (jump-not-equal m-s a-active-cons-cache-area allocate-structure-storage-uncached)
        ((m-3) output-selector-mask-25 add m-b a-active-cons-cache-free-pointer)
        (jump-greater-than m-3 a-active-cons-cache-free-limit allocate-structure-storage-uncached)
     ;; Set allocation-status to infinity to force list-header insertion next list-structure cons.
        ((a-active-cons-cache-allocation-status) dpb m-minus-one q-pointer a-zero)
        (popj-after-next
          (m-t) a-active-cons-cache-free-pointer)
       ((a-active-cons-cache-free-pointer) m-3)
allocate-structure-storage-uncached
     ;; Invalidate cache before we might trap out (just a good idea, probably not necessary).

;;; Can this hope to win?
;;; Yes!  And it does what you would expect it to do.
        (call-xct-next decode-area-specification)
       (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)

;;; This is what the above code equals.
;  (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
;  (call decode-area-specification)

        (call-not-equal-xct-next m-zero a-scavenge-work-while-consing scavenge-while-consing)
       ((a-scavenge-work) a-scavenge-work-while-consing)
        (call-xct-next get-active-region)
       ((vma-start-read) add m-s a-v-area-region-list)
     ;; Now M-T has address of object, M-B has total size, and M-A has boxed size.
        (call-xct-next touch-pages-in-new-object)
       ((a-active-cons-cache-allocation-status) dpb m-minus-one q-pointer a-zero)
        (jump update-structure-handles-for-object)

;These routines allocate some room in a list-structured region, creating a new region
;if necessary.  Args are M-S = typed/no cdr-code area number to allocate in,
;M-B = untyped number of q's to allocate.
;On exit
;M-T gets the first word of allocated storage.  Note:  These routines
;just bump the free pointer and put the old value in M-T, so don't expect a reasonable
;type on M-T or in the newly allocated storage.

allocate-list-storage
        (jump-not-equal m-s a-v-nil allocate-list-storage-kernel)
allocate-list-storage-default
        ((m-s) dpb m-zero q-all-but-typed-pointer a-default-cons-area)
allocate-list-storage-kernel
        (jump-not-equal m-s a-active-cons-cache-area allocate-list-storage-uncached)
        ((a-active-cons-cache-allocation-status q-r)
                add m-b a-active-cons-cache-allocation-status)
        (jump-greater-than q-r a-list-allocation-threshold
                           allocate-list-storage-cached-list-header)
        ((m-3) output-selector-mask-25 add m-b a-active-cons-cache-free-pointer)
        (jump-greater-than m-3 a-active-cons-cache-free-limit allocate-list-storage-uncached)
        (popj-after-next
          (m-t) a-active-cons-cache-free-pointer)
       ((a-active-cons-cache-free-pointer) m-3)
allocate-list-storage-uncached
        ((m-a) m-b)                             ;Legislate boxed-size  total-size.
     ;; Invalidate cache before we might trap out (just a good idea, probably not necessary).

        (call-xct-next decode-area-specification)
       (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)

;;; This does the same thing.
;      (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
;      (call decode-area-specification)

        (call-not-equal-xct-next m-zero a-scavenge-work-while-consing scavenge-while-consing)
       ((a-scavenge-work) a-scavenge-work-while-consing)
        (call-xct-next get-active-region)       ;return allocation-status in q-r
       ((vma-start-read) add m-s a-v-area-region-list)
     ;; Insert list-header if last object was structure, or if over threshold.
        (jump-greater-than q-r a-list-allocation-threshold insert-list-header-and-finish-up)
     ;; Now M-T has address of object, M-B has total size, and M-A has boxed size.
        (call-xct-next touch-pages-in-new-object)
       ((a-active-cons-cache-allocation-status) add m-b a-active-cons-cache-allocation-status)
        (jump update-structure-handles-for-object)
        ;;; we have to insert a list header.  Lie to TOUCH-PAGES-IN-NEW-OBJECT about the size
        ;;; so that it will do ehough "touching" for the list header word also.
insert-list-header-and-finish-up
        (call-xct-next touch-pages-in-new-object)
       ((m-b) m+a+1 m-b a-zero)                 ; add one word for the list header
        ((m-b) m-a-1 m-b a-zero)                ; fix m-b back to size of object being consed
    ;; Insert a list-header to indicate a transition from structure-storage to list-storage or if we havn't put one recently.
        ((a-active-cons-cache-free-pointer) m+a+1 m-zero a-active-cons-cache-free-pointer)
;       ((a-active-cons-cache-allocation-status) m-zero)
;       ((a-active-cons-cache-allocation-status) add m-b a-active-cons-cache-allocation-status)
        ((a-active-cons-cache-allocation-status) m-b)
        ((md) (a-constant (plus (byte-value q-cdr-code cdr-error)
                                (byte-value q-data-type dtp-header)
                                (byte-value q-header-type %header-type-list))))
        ((vma-start-write) m-t)
       (check-page-write-unboxed)
        ((m-t) add m-t (a-constant 1))          ;past list header
        (jump update-structure-handles-for-object)

allocate-list-storage-cached-list-header
     ;; Cons cache hit, but we need to insert a list header.  This path is a bit slower, but
     ;; not nearly as bad as the uncached case.
        ((m-3) output-selector-mask-25 m+a+1 m-b a-active-cons-cache-free-pointer)
        (jump-greater-than m-3 a-active-cons-cache-free-limit allocate-list-storage-uncached)
        ((m-t) m+a+1 m-zero a-active-cons-cache-free-pointer)
        ((a-active-cons-cache-free-pointer) m-3)
        ((a-active-cons-cache-allocation-status) m-zero)
     ;; Insert a list-header to indicate a transition from structure-storage to list-storage.
        ((md) (a-constant (plus (byte-value q-cdr-code cdr-error)
                                (byte-value q-data-type dtp-header)
                                (byte-value q-header-type %header-type-list))))
        (popj-after-next
          (vma-start-write) sub m-t (a-constant 1))
       (check-page-write-unboxed)

;;; Similar routines for consing inside transporter.  These differ in that 1) the area in M-S is
;;; known to be valid, 2) a different set of cache registers is used, 3) they choose copy regions
;;; of the desired volatility (passed untagged in M-E).

(begin-comment) Zwei Lossage (end-comment)

allocate-structure-copy-storage
        (jump-not-equal m-s a-copy-cons-cache-area allocate-structure-copy-storage-uncached)
        (jump-not-equal m-e a-copy-cons-cache-volatility allocate-structure-copy-storage-uncached)
        ((m-3) output-selector-mask-25 add m-b a-copy-cons-cache-free-pointer)
        (jump-greater-than m-3 a-copy-cons-cache-free-limit
                           allocate-structure-copy-storage-uncached)
        ((a-copy-cons-cache-allocation-status) dpb m-minus-one q-pointer a-zero)
        (popj-after-next
          (m-t) a-copy-cons-cache-free-pointer)
       ((a-copy-cons-cache-free-pointer) m-3)

allocate-structure-copy-storage-uncached
        (call-not-equal m-minus-one a-copy-cons-cache-area invalidate-copy-cons-cache)
        (call-xct-next get-copy-region)
       ((vma-start-read) add m-s a-v-area-region-list)
        (call-xct-next touch-pages-in-new-object)
       ((a-copy-cons-cache-allocation-status) dpb m-minus-one q-pointer a-zero)
        (jump update-structure-handles-for-object)

allocate-list-copy-storage
        (jump-not-equal m-s a-copy-cons-cache-area allocate-list-copy-storage-uncached)
        (jump-not-equal m-e a-copy-cons-cache-volatility allocate-list-copy-storage-uncached)
        ((a-copy-cons-cache-allocation-status q-r) add m-b a-copy-cons-cache-allocation-status)
        (jump-greater-than q-r a-list-allocation-threshold allocate-list-copy-storage-uncached)
        ((m-3) output-selector-mask-25 add m-b a-copy-cons-cache-free-pointer)
        (jump-greater-than m-3 a-copy-cons-cache-free-limit allocate-list-copy-storage-uncached)
        (popj-after-next
          (m-t) a-copy-cons-cache-free-pointer)
       ((a-copy-cons-cache-free-pointer) m-3)

allocate-list-copy-storage-uncached
        (call-not-equal m-minus-one a-copy-cons-cache-area invalidate-copy-cons-cache)
        (call-xct-next get-copy-region)         ;return allocation-status in q-r.
       ((vma-start-read) add m-s a-v-area-region-list)
        (jump-greater-than q-r a-list-allocation-threshold insert-list-header-copy-and-finish-up)
        (call-xct-next touch-pages-in-new-object)
       ((a-copy-cons-cache-allocation-status) add m-b a-copy-cons-cache-allocation-status)
        (jump update-structure-handles-for-object)
insert-list-header-copy-and-finish-up
        (call-xct-next touch-pages-in-new-object)
       ((m-b) m+a+1 m-b a-zero)
        ((m-b) m-a-1 m-b a-zero)
        ((a-copy-cons-cache-free-pointer) m+a+1 m-zero a-copy-cons-cache-free-pointer)
        ((a-copy-cons-cache-allocation-status) m-b)
  ;separate copy of code for non-copy case since transporter must do check-page-write-force to avoid
  ; false read-only traps.
        ((md) (a-constant (plus (byte-value q-cdr-code cdr-error)
                                (byte-value q-data-type dtp-header)
                                (byte-value q-header-type %header-type-list))))
        ((vma-start-write) m-t)
        (check-page-write-force)
        ((m-t) add m-t (a-constant 1))          ;should be unboxed ***
        (jump update-structure-handles-for-object)

;;; Decode area specification in M-S.  Return boxed fixnum in M-S.
decode-area-specification
        (declare (args a-s) (values a-s))
        (call-data-type-equal m-s (a-constant (byte-value q-data-type dtp-symbol)) decode-area-symbol)
        (call-less-than m-s (a-constant (byte-value q-data-type dtp-fix)) trap)
     (error-table argtyp area m-s nil)
        (call-greater-than m-s (a-constant (plus (byte-value q-data-type dtp-fix) 377)) trap)
     (error-table argtyp area m-s nil)
        (popj)
decode-area-symbol
        (declare (args a-s) (values a-s))
        ((vma-start-read) output-selector-mask-25 add m-s (a-constant 1))       ;Value cell.
        (check-page-read)
        (popj-after-next dispatch transport md)
       ((m-s) q-typed-pointer md)

;;; Dispatch table for GET-ACTIVE-REGION, below.  Dispatches on region space type to find
;;; an active region appropriate to cons in.
(locality d-mem)
(start-dispatch 4 0)
d-verify-region-type
        (p-bit n-bit trap)                      ;0 FREE
        (n-bit get-active-region-loop)          ;1 OLD (try next region)
        (p-bit r-bit)                           ;2 NEW
        (p-bit n-bit trap)                      ;3
        (p-bit n-bit trap)                      ;4
        (p-bit n-bit trap)                      ;5
        (p-bit n-bit trap)                      ;6
        (p-bit n-bit trap)                      ;7
        (p-bit n-bit trap)                      ;10
        (p-bit r-bit)                           ;11 STATIC
        (p-bit n-bit trap)                      ;12 FIXED
        (p-bit r-bit)                           ;13 EXTRA-PDL
        (n-bit get-active-region-loop)          ;14 COPY (try next region)
        (n-bit get-active-region-loop)          ;15 MOBY-FIXED (try next region)
        (p-bit r-bit)                           ;16 MOBY-NEW  try this one.
(repeat 1 (p-bit n-bit trap))                   ;  code 15 up are MOBY.
(end-dispatch)
(locality i-mem)

;;; Subroutine of allocate-storage.  Call with area number (fixnum) in M-S, total number
;;; of words needed in M-B (untyped).  Finds a NEW, STATIC, or EXTRA-PDL region in area
;;; that has enough room to hold the new object, or allocates a new region for that area.
;;; Currently this fails if the object is larger than the region size of the area.
;;; Return region-allocation-status of region found in q-r.
get-active-region-loop
        ((vma-start-read) add m-k a-v-region-list-thread)
get-active-region
        (check-page-read)
     ;No transport, region list is not in oldspace (ever) (I think). -jrm
        ((m-k) q-pointer md)
     ;; If at end of region list, allocate another region, or reset extra-pdl and retry.
        (call-if-bit-set boxed-sign-bit m-k allocate-active-region)
        ((vma-start-read) add m-k a-v-region-bits)
        (check-page-read)
     ;; Check region-space-type.  If not suitable, jump to get-active-region-loop.
        (dispatch (lisp-byte %%region-space-type) md d-verify-region-type)
     (error-table cons-in-inappropriate-region)
     ;; Fall through for legitimate active region types (new, static, or extra-pdl).
        ((vma-start-read) add m-k a-v-region-origin)
        (check-page-read)
        ((a-active-cons-cache-region-origin) q-pointer md a-zero)
        ((vma-start-read) add m-k a-v-region-length)
        (check-page-read)
        ((m-3) output-selector-mask-25 add md a-active-cons-cache-region-origin)
        ((vma-start-read) add m-k a-v-region-free-pointer)
        (check-page-read)
        ((m-t) output-selector-mask-25 add md a-active-cons-cache-region-origin)
     ;; Compare region-free-pointer against region-length to see if there is enough room
     ;; for the object.  Note that we require that there be (1+ M-B) words, because we might
     ;; have to insert a list-header later on.
        ((a-active-cons-cache-free-pointer q-r) add m-t a-b)
        (jump-greater-or-equal q-r a-3 get-active-region-loop)
     ;; Set up the free limit to word 0 on the page following the free-pointer.  However,
     ;; If the free-pointer is at word 0 of a page, set the limit to that page, so the
     ;; structure-handles get set up correctly next time around.
        ((m-tem) add q-r (a-constant 377))
        ((a-active-cons-cache-free-limit) dpb m-zero q-page-index a-tem)
        ((vma-start-read) add m-k a-v-region-allocation-status)
        (check-page-read)
     ;; Note: GET-ACTIVE-REGION must return the allocation-status in the Q register.
        ((a-active-cons-cache-allocation-status q-r) md)
        (popj-after-next
          (a-active-cons-cache-region) m-k)
       ((a-active-cons-cache-area) m-s)

get-copy-region-loop
        ((vma-start-read) add m-k a-v-region-list-thread)
get-copy-region
        (check-page-read)
     ;; MAKE-REGION clobbers M-E, so set this here for later comparison.
        ((a-copy-cons-cache-volatility) m-e)
        ((m-k) q-pointer md)
     ;; If at end of region list, allocate another region, or reset extra-pdl and retry.
        (call-if-bit-set boxed-sign-bit m-k allocate-copy-region)
        ((vma-start-read) add m-k a-v-region-bits)
        (check-page-read)
     ;; If it's not a copy region, get the next one.
        ((m-tem) (lisp-byte %%region-space-type) md)
        (jump-not-equal m-tem (a-constant (eval %region-space-copy)) get-copy-region-loop)
     ;; If it's not the right volatility, get the next one.
        ((m-tem) (lisp-byte %%region-volatility) md)
        (jump-not-equal m-tem a-copy-cons-cache-volatility get-copy-region-loop)
        ((vma-start-read) add m-k a-v-region-origin)
        (check-page-read)
        ((a-copy-cons-cache-region-origin) q-pointer md a-zero)
        ((vma-start-read) add m-k a-v-region-length)
        (check-page-read)
        ((m-3) output-selector-mask-25 add md a-copy-cons-cache-region-origin)
        ((vma-start-read) add m-k a-v-region-free-pointer)
        (check-page-read)
        ((m-t) output-selector-mask-25 add md a-copy-cons-cache-region-origin)
     ;; Compare region-free-pointer against region-length to see if there is enough room
     ;; for the object.  Note that we require that there be (1+ M-B) words, because we might
     ;; have to insert a list-header later on.
        ((a-copy-cons-cache-free-pointer q-r) add m-t a-b)
        (jump-greater-or-equal q-r a-3 get-copy-region-loop)
     ;; Set up the free limit to word 0 on the page following the free-pointer.  However,
     ;; If the free-pointer is at word 0 of a page, set the limit to that page, so the
     ;; structure-handles get set up correctly next time around.
        ((m-tem) add q-r (a-constant 377))
        ((a-copy-cons-cache-free-limit) dpb m-zero q-page-index a-tem)
        ((vma-start-read) add m-k a-v-region-allocation-status)
        (check-page-read)
     ;; Note: GET-COPY-REGION must return the allocation-status in the Q register.
        ((a-copy-cons-cache-allocation-status q-r) md)
        (popj-after-next
          (a-copy-cons-cache-region) m-k)
       ((a-copy-cons-cache-area) m-s)

;;; Subroutine of allocate-storage.  Creates physical pages for the allocated object.
;;; M-DONT-SWAP-IN is a flag argument to the page fault handler.  The start of the object is in M-T.
;;; Total size of object in M-B.  The confusion about M-4 looks like a rounding hack to do ceiling to
;;; a whole page boundary.
touch-pages-in-new-object
        ((a-cons-tem) m-flags)
        ((vma-start-read) m-t)
        ((m-4) q-pointer-within-page m-t)       ;Then touch each page of allocated stuff
        (jump-not-equal-xct-next m-4 a-zero touch-pages-1) ;Jump if first page not a fresh page
       ((m-4) add m-4 a-b)                      ;M-4 gives page count in hairy way
touch-pages-0
        ((m-dont-swap-in) dpb (m-constant -1) a-flags)  ;Create pages without disk read
touch-pages-1
        (check-page-read)                       ;now take fault for previous VMA-START-READ
        ((m-4) sub m-4 (a-constant (eval page-size)))
        (popj-less-or-equal-xct-next m-4 a-zero)
       ((m-flags) seta a-cons-tem md)   ;Restore flags, complete memory cycle
        (jump-xct-next touch-pages-0)
       ((vma-start-read) add vma (a-constant (eval page-size)))

(begin-comment) Zwei Lossage (end-comment)

;;; A convenient subroutine for %MAKE-STRUCTURE and %MAKE-ARRAY.  Pops arguments off the
;;; stack (area total-size boxed-size), allocates and initializes an appropriate structure.

allocate-block
        ((m-a) q-pointer pdl-pop)               ;Boxed size.
        ((m-b) q-pointer pdl-pop)               ;Total size.
        (call-xct-next allocate-structure-storage)
       ((m-s) q-typed-pointer pdl-pop)          ;Area number.
        (jump-xct-next initialize-storage)      ;Tail recurse.
       ((pdl-push) a-v-nil)                     ;Boxed Q for initialization.

;;; (%make-structure pointer-data-type header-data-type header second-word area total boxed)

        (misc-inst-entry %make-structure)
x-make-structure
        (call allocate-block)
        ((vma) add m-t (a-constant 1))
        ((md-start-write) c-pdl-buffer-pointer-pop)
        (check-page-write)
        (gc-write-test-volatility)      ;OK since data is boxed.
        ((m-tem1) c-pdl-buffer-pointer-pop)
        ((md) dpb c-pdl-buffer-pointer-pop q-all-but-pointer a-tem1)
        ((vma-start-write m-t) dpb c-pdl-buffer-pointer-pop q-all-but-pointer a-t)
        (check-page-write)
        (gc-write-test-volatility)      ;OK since data is boxed.
        (popj)

;;; (%make-array header-word index-length leader-length area total-size boxed-size)

        (misc-inst-entry %make-array)
x-make-array
        (call allocate-block)
        ((vma m-t) q-pointer m-t (a-constant (byte-value q-data-type dtp-array-pointer)))
        ((m-e) output-selector-mask-25 add m-t a-b)    ;Last storage location.
        ((m-c) q-pointer pdl-pop)               ;Leader length.
        ((m-b) q-pointer pdl-pop)               ;Index length.
        (call-if-bit-set-xct-next (lisp-byte %%array-leader-bit) c-pdl-buffer-pointer
            make-array-leader)
       ((m-d) dpb pdl-pop q-pointer (a-constant (byte-value q-data-type dtp-array-header)))
        ((md-start-write) m-d)                  ;Store header.
        (check-page-write-no-sequence-break)    ;storage conventions would not be consistant...
        ((m-tem) ldb (lisp-byte %%array-type-field) m-d)
        (call-equal m-tem (a-constant (eval (ldb %%array-type-field art-complex)))
            initialize-complex-array)
;;; ***** NEVER NEVER NEVER LEAVE ARRAY HEADERS IN ACCUMULATORS *******
;;; It took me a week to find this bug! -JRM
        (popj-if-bit-clear-xct-next (lisp-byte %%array-long-length-flag) m-d)
       ((m-d) a-v-nil)

        ((vma) add m-t (a-constant 1))
        ((md-start-write) dpb m-b q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (check-page-write)
        (popj)

make-array-leader
     ;; Build and store array leader header.  VMA points to leader header word.  VMA and M-T
     ;; have dtp-array-pointer throughout this code.
        ((md-start-write) add m-c
            (a-constant (plus (byte-value q-data-type dtp-header)
                              (byte-value %%header-type-field %header-type-array-leader)
                              2)))
        (check-page-write-no-sequence-break)
     ;; Word before array header, gets leader length.
        ((vma m-t) m+a+1 m-t a-c)
        ((md-start-write) dpb m-c q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (check-page-write)
     ;; Leave VMA, M-T pointing to array header.
        (popj-after-next
          (vma m-t) add m-t (a-constant 1))
       (no-op)

initialize-complex-array
     ;; VMA points to 1- initial word.  M-E points to last word.  Fill with fixnum zeros.
        ((md) (a-constant (byte-value q-data-type dtp-fix)))
        ((m-e) output-selector-mask-25 sub m-e (a-constant 1))
initialize-complex-array-loop
        ((vma-start-write) output-selector-mask-25 add vma (a-constant 1))
        (check-page-write)
        (jump-less-than vma a-e initialize-complex-array-loop)
        (popj)

;;; Subroutine of make-structure for initializing both boxed and unboxed storage.
;;; Given an address in M-T, the number of boxed words (untagged) in M-A, and the
;;; total number of words (untagged) in M-B, initialize all the boxed qs to the value
;;; popped off the stack, with cdr codes of cdr-next (last one gets cdr-nil), and all
;;; the unboxed words to zero.

initialize-storage
        ((md) ldb pdl-pop q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
        ((m-3) setm m-a)                        ;Number of boxed words to initialize.
        ((m-4) sub m-b a-3)                     ;Total number of words.
        ((vma) sub m-t (a-constant 1))
        (jump-less-or-equal m-3 (a-constant 1) initialize-boxed-storage-1)
initialize-boxed-storage-0
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write)
        (gc-write-test)                                ;850726
        (jump-greater-than-xct-next m-3 (a-constant 2) initialize-boxed-storage-0)
       ((m-3) sub m-3 (a-constant 1))
initialize-boxed-storage-1
        ((md) q-all-but-cdr-code md (a-constant (byte-value q-cdr-code cdr-nil)))
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write)
        (gc-write-test)                                ;850726
initialize-unboxed-storage
        ((md) setz)
initialize-unboxed-storage-0
        (popj-less-or-equal m-4 a-zero)
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write-unboxed)
        (jump-xct-next initialize-unboxed-storage-0)
       ((m-4) sub m-4 (a-constant 1))

list-of-nils
        ((pdl-push) a-v-nil)
list-of-things
        ((m-a) m-b)                                    ;Boxed-size  Total-size.
        (call-return allocate-list-storage initialize-storage)

(begin-comment) Zwei Lossage (end-comment)

;;; Structure handles.

;;; Structure-handles comprise 18 bits of the VIRTUAL-PAGE-DATA entry for each page.
;;; %%VIRTUAL-PAGE-FIRST-HEADER (0..400) is the first object consed on that page, ie
;;; the index in the page of the first q you can look at to determine the storage
;;; layout (400 means there is no header on the page).  %%VIRTUAL-PAGE-INITIAL-QS
;;; (0..400) is the number of boxed words at the beginning of the page (left over from
;;; an object on the previous page).

;;; One problem here is that the structure handles are meaningless during
;;; a cold load.  In this case, we turn off the error checking.

;;; Given page number in M-3, reads structure-handle for that page.
;;; Returns untagged <first-header> in M-5, untagged <initial-qs> in M-6.

read-structure-handle
        ((vma-start-read) add m-3 a-v-virtual-page-data)
        (check-page-read)
        ((m-5) (lisp-byte %%virtual-page-first-header) md)
        ((m-tem) m-flags-check-structure-handles)       ;Don't care if they're bogus
        (popj-equal-xct-next m-tem a-zero)                      ;during cold loads.
       ((m-6) (lisp-byte %%virtual-page-initial-qs) md)

        (call-greater-than m-5 (a-constant 400) illop)
        (call-greater-than m-6 (a-constant 400) illop)
        (popj)

;;; Given page number in M-3, untagged <first-header> in M-5, untagged <initial-qs> in
;;; M-6, update structure handle word for that page.

write-structure-handle
        ((vma-start-read) add m-3 a-v-virtual-page-data)
        (check-page-read)
        ((m-tem1) m-flags-check-structure-handles)
        ((m-tem) dpb m-6 (lisp-byte %%virtual-page-initial-qs) a-zero)
        (jump-equal-xct-next m-tem1 a-zero write-structure-handle-no-check)
       ((m-tem) dpb m-5 (lisp-byte %%virtual-page-first-header) a-tem)
        ;; Avoid bogus structure handles.
        (call-greater-than m-5 (a-constant 400) illop)
        (call-greater-than m-6 (a-constant 400) illop)

write-structure-handle-no-check
        ((m-tem1) md)                           ;Need this on A side, for merging.
        (popj-after-next
          (md-start-write) dpb m-tem (lisp-byte %%virtual-page-structure-handle) a-tem1)
       (check-page-write-unboxed)

;;; Given region-number (untagged) in M-K, initialize VIRTUAL-PAGE-DATA for every
;;; page in that region.  Initial value is <initial-qs> = 0, <first-header> = 400
;;; (no header on that page).  This will also initialize the volatility bits to zero.

initialize-virtual-page-data-for-region
        ((vma-start-read) add m-k a-v-region-origin)
        (check-page-read)
        ((md) q-page-number md)
        ((pdl-push) add md a-v-virtual-page-data)
        ((vma-start-read) add m-k a-v-region-length)
        (check-page-read)
        ((pdl-push) q-page-number md)
        (call-xct-next fill-memory)
       ((md) (a-constant (byte-value %%virtual-page-first-header 400)))
     ;; Fall through to do volatilities.
initialize-region-volatilities
        ((vma-start-read) add m-k a-v-region-origin)
        (check-page-read)
        ((md) (byte-field 15 14) md)                      ;(// (page-number region-origin) 16.)
        ((pdl-push) add md a-v-virtual-page-volatility)   ;first word to fill
        ((vma-start-read) add m-k a-v-region-length)
        (check-page-read)
        ((pdl-push) (byte-field 15 14) md)                ;number of words
        (jump-xct-next fill-memory)
       ((md) m-zero)

;;; Fill memory with contents of MD, stack has starting address and number of words.
fill-memory
        ((a-fill-memory-bound) sub pdl-pop (a-constant 2))
        ((vma-start-write) pdl-pop)
        (check-page-write-unboxed)
        (jump-not-equal-xct-next m-minus-one a-fill-memory-bound fill-memory-loop-entry)
       ((a-fill-memory-bound) add vma a-fill-memory-bound)
        (popj)
fill-memory-loop
        (check-page-write-unboxed)
fill-memory-loop-entry
        (jump-less-than-xct-next vma a-fill-memory-bound fill-memory-loop)
       ((vma-start-write) add vma (a-constant 1))
        (check-page-write-unboxed)
        (popj)

(begin-comment) Zwei Lossage (end-comment)

;;; Given object address in M-T, untagged boxed size in M-A, untagged total size in M-B,
;;; update the structure-handles of the pages contained in the object.

update-structure-handles-for-object
        (call-xct-next read-structure-handle)
       ((m-3) q-page-number m-t)
     ;; If there is no header on this page (<first-header> = 400), then this is
     ;; the first structure on the page, and its index becomes the <first-header>.
     ;; Note that there may be <initial-qs> on this page left from last page.
        (call-equal-xct-next m-5 (a-constant 400) write-structure-handle)
       ((m-5) q-page-index m-t)
        ((m-4) add m-5 a-a)
     ;; If there are no boxed words extending over the following pages, exit.
        (jump-less-or-equal m-4 (a-constant 400) update-structure-handles-last-page)
        ((m-4) sub m-4 (a-constant 400))
        ((m-5) (a-constant 400))                ;Indicates no header (yet) on these pages.
        ((m-6) (a-constant 400))
update-structure-handles-boxed-loop
        (jump-less-than m-4 (a-constant 400) update-structure-handles-last-boxed-page)
        (call-xct-next write-structure-handle)
       ((m-3) add m-3 (a-constant 1))
        (jump-xct-next update-structure-handles-boxed-loop)
       ((m-4) sub m-4 (a-constant 400))
update-structure-handles-last-boxed-page
        ((m-6) q-page-index m-4)
        (call-xct-next write-structure-handle)
       ((m-3) add m-3 (a-constant 1))
update-structure-handles-last-page
        ((m-4) add m-t a-b)
        ((m-3) q-page-number m-4)
        ((vma-start-read) add m-3 a-v-virtual-page-data)
        (check-page-read)
        ((m-tem) q-page-index m-4)
        ((m-5) (lisp-byte %%virtual-page-first-header) md)
        (popj-not-equal m-5 (a-constant 400))   ;First header already set, exit.
        ((a-tem1) md)                           ;Just need this on A side, for merging.
        (popj-after-next
          (md-start-write) dpb m-tem (lisp-byte %%virtual-page-first-header) a-tem1)
       (check-page-write-unboxed)

;;;

(locality d-mem)
(start-dispatch 5 0)
d-structure-info
        (p-bit n-bit illop)                     ;trap
        (p-bit r-bit)                           ;null
        (p-bit n-bit UNRECONCILED-ILLOP)        ;unreconciled
        (p-bit r-bit)                           ;symbol
        (n-bit structure-info-symbol-header)    ;symbol-header
        (p-bit r-bit)                           ;fix
        (p-bit r-bit)                           ;extended-number
        (n-bit structure-info-header)           ;header
        (p-bit r-bit)                           ;gc-forward
        (p-bit r-bit)                           ;external-value-cell-pointer
        (p-bit r-bit)                           ;one-q-forward
        (n-bit structure-info-header-forward)   ;header-forward
        (n-bit structure-info-body-forward)     ;body-forward
        (p-bit r-bit)                           ;locative
        (p-bit r-bit)                           ;list
        (p-bit r-bit)                           ;u code entry
        (p-bit r-bit)                           ;fef
        (p-bit r-bit)                           ;array-pointer
        (n-bit structure-info-array-header)     ;array-header
        (p-bit r-bit)                           ;stack-group
        (p-bit r-bit)                           ;closure
        (p-bit r-bit)                           ;indexed-forward
        (p-bit r-bit)                           ;select-method
        (p-bit r-bit)                           ;instance
        (n-bit structure-info-instance-header)  ;instance-header
        (p-bit r-bit)                           ;entity
        (p-bit n-bit illop)                     ;unused-32
        (p-bit r-bit)                           ;self-ref-pointer
        (p-bit r-bit)                           ;character
        (p-bit r-bit)                           ;rplacd-forward
        (p-bit n-bit illop)                     ;spare
        (p-bit r-bit)                           ;small-flonum
        (repeat nqzusd (p-bit n-bit illop))
(end-dispatch)
(locality i-mem)

;;; Given the address of the base of a structure, return information on its size.
;;; Note that if given the address of an array header, the leader (if any) is
;;; not counted, but if given the address of the leader, the leader is
;;; counted.  In other words, nothing lower the given address is counted.
;;; Inputs:  address in MD
;;; Outputs: M-3 number of boxed words, M-4 number of unboxed words following those,
;;;          M-K<0> clear if object is part of a list, set if it's a structure.
;;;          M-K<1> set if structure is a stack.
;;;          If object is a list, MD is left with the last element, which may be
;;;             tested for DTP-RPLACD-FORWARD.
;;; Clobbers: M-A, M-B, M-T, usual page-fault things.
;;; The type field of VMA is zero throughout this section.
;;; This routine MAY NOT call the transporter, since it is invoked by the
;;; transporter.  Otherwise the transporter's variables and flag could be
;;; clobbered, and the possibility of micro-stack overflow would arise.
;;; Note that an illegal pointer to oldspace can be left in the VMA.

(locality a-mem)
a-structure-info-scan-base (0)
(locality i-mem)

(locality d-mem)
(start-dispatch 2 0)
d-structure-info-list
        (structure-info-list-loop)              ;0 cdr-next
        (n-bit structure-info-list-exit)        ;1 cdr-error
        (structure-info-list-loop)              ;2 cdr-normal
        (n-bit structure-info-list-exit)        ;3 cdr-nil
(end-dispatch)
(locality i-mem)

structure-info
        ((m-3) setz)
        ((m-4) setz)
        ((a-structure-info-scan-base) q-pointer m-t)
        ((vma-start-read) q-pointer m-t)
        (check-page-read)
        ((m-k) a-zero)
        (dispatch q-data-type md d-structure-info)
     ;; Fall through on pointer/immediate types, which must components of a list.
structure-info-list
        (dispatch-xct-next q-cdr-code md d-structure-info-list)
       ((vma-start-read) add vma (a-constant 1))
structure-info-list-loop
        (check-page-read)
        (dispatch-xct-next q-cdr-code md d-structure-info-list)
       ((vma-start-read) add vma (a-constant 1))
structure-info-list-exit
        (popj-after-next
          (m-3) sub vma a-structure-info-scan-base)
       ((m-3) add m-3 (a-constant 1))

structure-info-symbol-header
        (popj-after-next
          (m-3) (a-constant 5))
       ((m-k) (a-constant 1))

structure-info-instance-header
     ;; Get size (all boxed) from instance descriptor.  Descriptor could be in oldspace.
        ((vma-start-read) add md (a-constant (eval %instance-descriptor-size)))
        (check-page-read)
        ((m-k) (a-constant 1))
structure-info-instance-header-transport
        (popj-data-type-not-equal-xct-next md (a-constant (byte-value q-data-type dtp-gc-forward)))
       ((m-3) q-pointer md)
        ((vma-start-read) md)
        (jump-xct-next structure-info-instance-header-transport)
       (check-page-read)

structure-info-body-forward
  ;;body-forward can be either below the dtp-header-forward (if it was originally an
  ;; array-leader or above it (in the body of the array.)
     ;; Find DTP-HEADER-FORWARD, count intervening words.
        ((m-tem) q-pointer md)                  ;Address of header.
        ((m-tem) sub vma a-tem)                 ;- # words between here and there.
        (call-greater-or-equal m-tem a-zero structure-info-body-forward-higher-than-header)
        ((m-3) sub m-3 a-tem)                   ;Account for them, drop into header case.
        ((vma) q-pointer md)

;; DTP-HEADER-FORWARD - include all DTP-BODY-FORWARD's that point here as unboxed Q's.
structure-info-header-forward
        ((m-3) add m-3 (a-constant 1))          ;1 boxed Q for the header
     ;; Note: this is a bad thing to be in M-K, it must be clobbered (see below) before SB.
        ((m-k) q-pointer vma (a-constant (byte-value q-data-type dtp-body-forward)))
        ((vma) q-pointer m-k)                   ;Address of header
structure-info-header-forward-loop
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read-no-sequence-break)
        ((m-tem) q-typed-pointer md)
     ;; Check: is this word a body-forward that points to this header-forward?
        (jump-equal-xct-next m-tem a-k structure-info-header-forward-loop)
       ((m-4) add m-4 (a-constant 1))           ;So count it and keep looping
     ;; Last word was not part of structure so uncount it.
        (popj-after-next (m-4) sub m-4 (a-constant 1))
       ((m-k) seta (a-constant 1))

structure-info-body-forward-higher-than-header
        ((m-k) q-typed-pointer md)              ;header which will continue block
        (jump-xct-next structure-info-header-forward-loop)
       ((m-4) add m-4 (a-constant 1))           ;word contining first body-forward.

(locality d-mem)
(start-dispatch 5)                              ;dispatch on header subtype
d-structure-info-header
        (p-bit n-bit illop)                     ;%header-type-error
        (structure-info-fef)                    ;%header-type-fef
        (structure-info-array-leader)           ;%header-type-array-leader
        (structure-info-list-header)            ;%header-type-list
        (structure-info-flonum)                 ;%header-type-flonum
        (structure-info-complex)                ;%header-type-complex
        (structure-info-bignum)                 ;%header-type-bignum
        (structure-info-rational)               ;%header-type-rational
        (structure-info-fef)                    ;%header-type-fast-fef-fixed-args-no-locals
        (structure-info-fef)                    ;%header-type-fast-fef-var-args-no-locals
        (structure-info-fef)                    ;%header-type-fast-fef-fixed-args-with-locals
        (structure-info-fef)                    ;%header-type-fast-fef-var-args-with-locals
        (repeat nhdusd (p-bit n-bit illop))
(end-dispatch)
(locality i-mem)

structure-info-header
        (dispatch-xct-next (lisp-byte %%header-type-field) md d-structure-info-header)
       ((m-k) seta (a-constant 1))

structure-info-fef
        ((m-3) (lisp-byte %%fefh-pc-in-words) md)       ;Number of boxed words.
        ((vma-start-read) add vma (a-constant (eval %fefhi-storage-length)))
        (check-page-read)
        (popj-after-next (m-4) q-pointer md)            ;Total number of words.
       ((m-4) sub m-4 a-3)                              ;Number of unboxed words.

structure-info-flonum
        (popj-after-next (m-3) (a-constant 2))          ;Header plus one boxed word.
       (no-op)

structure-info-complex
structure-info-rational
        (popj-after-next (m-3) (a-constant 3))          ;Headers and two number pointers.
       (no-op)

structure-info-bignum
        (popj-after-next
          (m-3) seta (a-constant 1))
       ((m-4) bignum-header-length md)

structure-info-list-header
        (popj-after-next
          (m-3) seta (a-constant 1))
       (no-op)

(locality d-mem)
(start-dispatch 5 0)
d-structure-info-array-header
        (p-bit n-bit illop)                             ;array type 0 not used
        (n-bit structure-info-1b-array)                 ;bit array
        (n-bit structure-info-2b-array)                 ;2 bit array
        (n-bit structure-info-4b-array)                 ;4 bit array
        (n-bit structure-info-8b-array)                 ;8 bit array
        (n-bit structure-info-16b-array)                ;16 bit array
        (n-bit structure-info-32b-array)                ;32 bit array
        (r-bit)                                         ;q array
        (r-bit)                                         ;list q array
        (n-bit structure-info-8b-array)                 ;string array
        (r-bit)                                         ;stack-group head
        (n-bit structure-info-special-pdl)             ;binding-pdl
        (n-bit structure-info-16b-array)                ;half-fix
        (n-bit structure-info-regular-pdl)              ;reg-pdl
        (n-bit structure-info-float-array)              ;float
        (n-bit structure-info-32b-array)                ;fps-float
        (n-bit structure-info-16b-array)                ;fat-string
        (n-bit structure-info-complex-float-array)      ;complex-float
        (n-bit structure-info-complex-array)            ;complex
        (n-bit structure-info-complex-fps-float-array)  ;complex-fps-float
        (n-bit structure-info-32b-array)                ;inum
        (repeat natusd (p-bit n-bit illop))
(end-dispatch)
(locality i-mem)

structure-info-array-leader
        ((m-3) (lisp-byte %%array-leader-length) md)    ;Add in size of leader.
        ((vma-start-read) add vma a-3)                  ;Get header.
        (check-page-read)                               ;And drop into array-header case.

structure-info-array-header
        ((m-k) seta (a-constant 1))
        ((m-a) q-pointer md)    ;Copy the array header
        ((m-t) (lisp-byte %%array-number-dimensions) m-a)
        ((m-b) (lisp-byte %%array-index-length-if-short) m-a)
        (call-equal m-t a-zero structure-info-rank-zero-array)
        (call-if-bit-set (lisp-byte %%array-long-length-flag) m-a structure-info-long-length)
     ;; M-T has #header-words, M-B index-length, VMA address of header, M-A header.
        (jump-if-bit-set-xct-next (lisp-byte %%array-displaced-bit) m-a
            structure-info-displaced-array)
       ((m-3) add m-3 a-t)                      ;Count array header, dimension words as boxed
        (dispatch (lisp-byte %%array-type-field) m-a d-structure-info-array-header)
       ((m-3) add m-3 a-b)                      ;POPJ-XCT-NEXT if Q-type array

structure-info-long-length
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        ((m-t) add m-t (a-constant 1))
        (popj-after-next
          (m-b) q-pointer md)                   ;Long index length word.
       ((vma) sub vma (a-constant 1))           ;Make VMA point to header again.

structure-info-displaced-array
        (popj-after-next (m-3) add m-3 a-b)     ;Displaced array, pretend type is Q
       (no-op)

structure-info-rank-zero-array
        (popj-after-next)
       ((m-t) add m-t (a-constant 1))

structure-info-1b-array
        (popj-after-next (m-b) add m-b (a-constant 37))
       ((m-4) (byte-field (difference q-pointer-width 5) 5) m-b)

structure-info-2b-array
        (popj-after-next (m-b) add m-b (a-constant 17))
       ((m-4) (byte-field (difference q-pointer-width 4) 4) m-b)

structure-info-4b-array
        (popj-after-next (m-b) add m-b (a-constant 7))
       ((m-4) (byte-field (difference q-pointer-width 3) 3) m-b)

structure-info-8b-array
        (popj-after-next (m-b) add m-b (a-constant 3))
       ((m-4) (byte-field (difference q-pointer-width 2) 2) m-b)

structure-info-16b-array
        (popj-after-next (m-b) add m-b (a-constant 1))
       ((m-4) (byte-field (difference q-pointer-width 1) 1) m-b)

structure-info-32b-array
        (popj-after-next no-op)
       ((m-4) m-b)

structure-info-complex-float-array              ;Four unboxed words per element.
        ((m-tem) add m-b a-b)
        (popj-after-next
          (m-tem) add m-tem a-tem)
       ((m-4) add m-4 a-tem)

structure-info-complex-array                    ;Two boxed words per element.
        (popj-after-next
          (m-3) add m-3 a-b)
       ((m-3) add m-3 a-b)

structure-info-complex-fps-float-array
structure-info-float-array                      ;Two unboxed words per element.
        (popj-after-next
          (m-4) add m-4 a-b)
       ((m-4) add m-4 a-b)

;;; Find the stack-group associated with the pdl in vma, following broken-hearts.
pdl-stack-group
        ((vma-start-read) sub vma (a-constant 2))
        (check-page-read)
pdl-stack-group-transport
        ((vma-start-read) q-typed-pointer md)
        (check-page-read)
        (jump-xct-next pdl-stack-group-transport)
       (popj-data-type-not-equal md (a-constant (byte-value q-data-type dtp-gc-forward)))

;;; PDL's have the magic feature that stuff after the pdl pointer is called unboxed.
structure-info-regular-pdl
        (call-xct-next pdl-stack-group)
       ((m-k) (a-constant 3))
        (jump-equal vma a-qcstkg structure-info-my-regular-pdl)
structure-info-not-my-regular-pdl
        ((vma-start-read) sub vma (a-constant (eval (+ 2 sg-regular-pdl-pointer))))
        (check-page-read)
structure-info-regular-pdl-exit
     ;; MD is pdl-pointer (can be -1), M-TEM gets index of lowest unboxed word.
        ((m-tem) output-selector-mask-25 add md (a-constant 1))
        (popj-after-next (m-3) add m-3 a-tem)
       ((m-4) sub m-b a-tem)
structure-info-my-regular-pdl
     ;; If in middle of switching stack groups, pdl pointers in machine are not valid.
     ;; We must have been called from the transporter, and this must be the sg we're
     ;; switching to, since the one we're switching from cannot be in oldspace.
        (jump-if-bit-set m-stack-group-switch-flag structure-info-not-my-regular-pdl)
     ;; If not switching stack groups, and this is the current stack group,
     ;; use the pdl pointers in the machine rather than those in memory.
        ((pdl-buffer-index) sub pdl-buffer-pointer a-pdl-buffer-head)  ;Modulo arithmetic.
        ((md) add pdl-buffer-index a-pdl-buffer-virtual-address)
        (jump-xct-next structure-info-regular-pdl-exit)
       ((md) sub md a-qlpdlo)                          ;Relative pdl pointer.

structure-info-special-pdl
        (call-xct-next pdl-stack-group)
       ((m-k) (a-constant 3))
        (jump-equal vma a-qcstkg structure-info-my-special-pdl)
structure-info-not-my-special-pdl
        ((vma-start-read) sub vma (a-constant (eval (+ 2 sg-special-pdl-pointer))))
        (check-page-read)
structure-info-special-pdl-exit
     ;; MD is pdl-pointer (can be -1), M-TEM gets index of lowest unboxed word.
        ((m-tem) output-selector-mask-25 add md (a-constant 1))
        (popj-after-next (m-3) add m-3 a-tem)
       ((m-4) sub m-b a-tem)
structure-info-my-special-pdl
     ;; If in middle of switching stack groups, pdl pointers in machine are not valid.
     ;; We must have been called from the transporter, and this must be the sg we're
     ;; switching to, since the one we're switching from cannot be in oldspace.
        (jump-if-bit-set m-stack-group-switch-flag structure-info-not-my-special-pdl)
     ;; If not switching stack groups, and this is the current stack group,
     ;; use the pdl pointers in the machine rather than those in memory.
        ((md) a-qlbndp)
        (jump-xct-next structure-info-special-pdl-exit)
       ((md) sub md a-qlbndo)

     (misc-inst-entry %structure-boxed-size)
structure-boxed-size
        (call-xct-next structure-info)
       ((m-t) q-pointer pdl-pop)
        (popj-after-next no-op)
       ((m-t) q-pointer m-3 (a-constant (byte-value q-data-type dtp-fix)))

     (misc-inst-entry %structure-total-size)
structure-total-size
        (call-xct-next structure-info)
       ((m-t) q-pointer pdl-pop)
        (popj-after-next (m-3) add m-3 a-4)
       ((m-t) q-pointer m-3 (a-constant (byte-value q-data-type dtp-fix)))

(locality a-mem)
a-find-structure-header-value (0)
a-find-structure-header-scan-base (0)
(locality i-mem)

(locality d-mem)
(start-dispatch 5 0)
d-find-structure-header
        (fsh-search-loop)                       ;trap (allowed in PDLs)
        (fsh-search-loop)                       ;null
        (N-BIT UNRECONCILED-TRAP)               ;unreconciled
        (fsh-search-loop)                       ;symbol
        (n-bit fsh-symbol-header)               ;symbol-header
        (fsh-search-loop)                       ;fix
        (fsh-search-loop)                       ;extended-number
        (n-bit fsh-header)                      ;header
        (n-bit fsh-list-search)                 ;gc-forward            *** document this ***
        (fsh-search-loop)                       ;external-value-cell-pointer
        (fsh-search-loop)                       ;one-q-forward
        (n-bit fsh-header-forward)              ;header-forward
        (n-bit fsh-body-forward)                ;body-forward
        (fsh-search-loop)                       ;locative
        (fsh-search-loop)                       ;list
        (fsh-search-loop)                       ;u code entry
        (fsh-search-loop)                       ;fef
        (fsh-search-loop)                       ;array-pointer
        (n-bit fsh-array-header)                ;array-header
        (fsh-search-loop)                       ;stack-group
        (fsh-search-loop)                       ;closure
        (fsh-search-loop)                       ;indexed-forward
        (fsh-search-loop)                       ;select-method
        (fsh-search-loop)                       ;instance
        (n-bit fsh-instance-header)             ;instance-header
        (fsh-search-loop)                       ;entity
        (p-bit n-bit illop)                     ;unused-32
        (fsh-search-loop)                       ;self-ref-pointer
        (fsh-search-loop)                       ;character
        (n-bit fsh-list-search)                 ;rplacd-forward
        (p-bit n-bit illop)                     ;36
        (fsh-search-loop)                       ;small-float (also small neg num allowed in PDLs)
(end-dispatch)

(start-dispatch 5)                              ;dispatch on header subtype
d-fsh-header
        (n-bit p-bit illop)                     ;%header-type-error
        (fsh-fef-header)                        ;%header-type-fef
        (n-bit fsh-array-leader)                ;%header-type-array-leader
        (n-bit fsh-list-header)                 ;%header-type-list
        (fsh-extended-number)                   ;%header-type-flonum
        (fsh-extended-number)                   ;%header-type-complex
        (fsh-extended-number)                   ;%header-type-bignum
        (fsh-extended-number)                   ;%header-type-rational
        (fsh-fef-header)                        ;%header-type-fast-fef-fixed-arg-no-locals
        (fsh-fef-header)                        ;%header-type-fast-fef-var-args-no-locals
        (fsh-fef-header)                        ;%header-type-fast-fef-fixed-args-with-locals
        (fsh-fef-header)                        ;%header-type-fast-fef-var-args-with-locals
(repeat nhdusd (n-bit p-bit illop))
(end-dispatch)
(locality i-mem)

     (misc-inst-entry %find-structure-header)
find-structure-header
        ((m-e) a-zero)
        (call-return xrgn find-structure-header-region-known)

     (misc-inst-entry %find-structure-leader)
find-structure-leader
        (call xrgn)

     ;; Special entry for transporter.
find-structure-leader-region-known
        ((m-e) (a-constant 1))
find-structure-header-region-known
     ;; This entry presumes pointer in M-A, region number in M-T.
        (call-equal m-t a-v-nil illop)          ;Halt on illegal pointers while debugging.
      (error-table crash find-structure-header of a pointer not in any region)
     ;; M-B gets the region origin, to bound the search.
        ((vma-start-read) add m-t a-v-region-origin)
        (check-page-read)
   ;*** these DTP-LOCATIVEs a bit marginal..
        ((m-b) q-pointer md (a-constant (byte-value q-data-type dtp-locative)))

        ((vma-start-read m-t) q-pointer m-a (a-constant (byte-value q-data-type dtp-locative)))
        (check-page-read)
        ((a-find-structure-header-scan-base) m-t)
        (dispatch-xct-next q-data-type md d-find-structure-header)
       (jump-equal m-t a-b fsh-list-search)

fsh-search-loop
      (error-table crash illegal structure in find-structure-header)
     ;; this pc is on the top of the stack when (perhaps amoung other
     ;; places) find-structure-header is following header forwards and
     ;; at the end of the chain finds something that is not a header
        ((vma-start-read m-t) sub m-t (a-constant 1))
        (check-page-read)
     ;; Dispatch goes to terminating routines for structure-type headers, loops for
     ;; non-header types.  Note that the xct-next cycle does the bounds check.  This is
     ;; is executed for the non-headers, in which case you know you're in a list;
     ;; the header types inhibit xct-next, since they terminate on this word anyway,
     ;; and they don't necessarily indicate that you're in a list.
        (dispatch-xct-next q-data-type md d-find-structure-header)
       (jump-equal m-t a-b fsh-list-search)

fsh-symbol-header
        (popj-after-next
          (m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-symbol)))
       (no-op)

fsh-instance-header
        (popj-after-next
          (m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-instance)))
       (no-op)

fsh-body-forward
        ((vma-start-read m-t) q-pointer md (a-constant (byte-value q-data-type dtp-locative)))
        (check-page-read)
fsh-header-forward
     ;; Header-forwards can be multiply indirected and could point to oldspace.
        (dispatch transport-header md)
     ;; Dispatch does xct-next cycle for non-headers.  A header forward should not point
     ;; to a non-header, so we illop.
        (dispatch-xct-next q-data-type md d-find-structure-header)
       (call illop)

fsh-array-leader
     ;; If we're doing find-structure-leader, just return M-T as a locative, else get header.
        (popj-not-equal-xct-next m-e a-zero)
       ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-locative)))
     ;; Return appropriate pointer to header by offsetting to header and falling through.
        ((m-b) (lisp-byte %%array-leader-length) md)
     ;; There is an important distinction between VMA and M-T here.  The VMA points to where
     ;; the leader header actually is, M-T points to the location that, transported, points
     ;; to the leader header.  The routine below needs the VMA pointing to the actual array
     ;; header, and needs M-T offset likewise, to return with the appropriate tag as the value.
     ;; This is why we add M-b to both VMA and M-T independently.
        ((vma-start-read) add vma a-b)
        (check-page-read)
        ((m-t) add m-t a-b)             ;dont depend on m-tem across check-page-read!
     ;; Temporary defense for debugging.
        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-header)) illop)
      (error-table crash inconsistant array header in find-structure-header)
     ;; Fall through.
fsh-return-array-or-stack-group
     ;; Return a pointer to the header, with data-type DTP-ARRAY-POINTER or DTP-STACK-GROUP.
        ((m-tem) (lisp-byte %%array-type-field) md)
        (popj-not-equal-xct-next
           m-tem (a-constant (eval (lsh art-stack-group-head array-type-shift))))
       ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-array-pointer)))
     ;; Header was stack-group-header, return a dtp-stack-group pointer.
        (popj-after-next
          (m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-stack-group)))
       (no-op)

fsh-array-header
     ;; If we're doing find-structure-header or the array has no leader, branch to routine
     ;; to return array or stack-group pointer to header.  Otherwise return locative to leader.
        (jump-if-bit-clear (lisp-byte %%array-leader-bit) md fsh-return-array-or-stack-group)
        (jump-equal m-e a-zero fsh-return-array-or-stack-group)
     ;; There is a leader, return a locative to the array-leader header.
        ((vma-start-read) sub vma (a-constant 1))       ;Leader length stored here.
        (check-page-read)
     ;; Leader header is at (- array-pointer leader-length 2).
        ((m-t) sub m-t (a-constant 2))
        ((m-tem) (lisp-byte %%array-leader-length) md)
        (popj-after-next
          (m-t) sub m-t a-tem)
       ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-locative)))

fsh-header
     ;; Know your control structures!  FEFs and extended-numbers xct-next here, which means
     ;; just one instruction at the dispatch target will be executed before the popj.
        (dispatch-xct-next (lisp-byte %%header-type-field) md d-fsh-header)
       (popj-xct-next)

fsh-fef-header
        ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-fef-pointer)))

fsh-extended-number
        ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-extended-number)))

fsh-list-header   ;get here if original thing a list, not contained within another structure.
fsh-list-search
     ;; Set scan pointer back to the address find-structure-header was called on, enter loop.
     ;; Note that we actually start on the word before the scan base.  This is OK.
        ((m-t) seta a-find-structure-header-scan-base)
fsh-list-loop
     ;; Stop on this word (exactly) if at known object boundary.
        (jump-equal m-t a-b fsh-list-exit)
        ((vma-start-read m-t) sub m-t (a-constant 1))
        (check-page-read)
     ;; Loop if CDR-NEXT or CDR-NORMAL, otherwise fall through.
        (jump-if-bit-clear-xct-next (byte-field 1 36) md fsh-list-loop)
       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-rplacd-forward))
            fsh-list-too-far)
fsh-list-too-far
     ;; Back up one word and return list pointer.
        (popj-after-next
          (m-t) add m-t (a-constant 1))
       ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-list)))
fsh-list-exit
     ;; Return list pointer to this word.  However, if it's a list-header (dtp-header here
     ;; must be list-header), then we've gone one too far and must back up.
        (popj-after-next
          (m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-list)))
       (call-data-type-equal md (a-constant (byte-value q-data-type dtp-header))
            fsh-list-too-far)

;;; Pointer-info: given an address, figure out if it points into valid boxed storage.

;arg: unknown pointer in m-1.  Return in m-2 pointer to beginning of an object
;  just below m-1  (It might or might not be the object containing m-1.)
compute-structure-bound
        ((pdl-push) q-pointer m-1)
compute-structure-bound-loop
        (call-xct-next read-structure-handle)
       ((m-3) q-page-number m-1)
        (jump-not-equal m-5 (a-constant 400) compute-structure-bound-exit)
compute-structure-bound-restart
        (jump-xct-next compute-structure-bound-loop)
       ((m-1) sub m-1 (a-constant 400))
compute-structure-bound-exit
        ((m-2) dpb m-5 q-page-index a-1)
        (jump-less-than pdl-top a-2 compute-structure-bound-restart)
        (popj-after-next
          (m-2) q-pointer m-2)
       ((m-1) q-pointer pdl-pop)

     (misc-inst-entry %pointer-info)
pointer-info
     ;; First see if it's in a valid region... (leaves address in M-A)
        (call xrgn)
        (jump-equal m-t a-v-nil pointer-maybe-invalid-region)
pointer-info-region-ok
        (call-xct-next compute-structure-bound)
       ((m-1) q-pointer m-a)
pointer-info-loop
        (call-xct-next structure-info)
       ((m-t) m-2)
        ((m-tem) add m-2 a-3)
        (jump-less-than m-1 a-tem pointer-boxed)        ;points to boxed area, this obj
        ((m-tem) add m-tem a-4)
        (jump-less-than m-1 a-tem pointer-unboxed)      ;points to unboxed are, this obj
        (jump-xct-next pointer-info-loop)               ;not within this obj.
       ((m-2) m-tem)

pointer-invalid-region
        (popj-after-next
          (pdl-push) a-v-nil)
       ((m-t) (a-constant (byte-value q-data-type dtp-fix)))

pointer-unboxed
        (popj-after-next
          (pdl-push) dpb m-2 q-pointer (a-constant (byte-value q-data-type dtp-fix)))
       ((m-t) (a-constant (plus (byte-value q-data-type dtp-fix) 1)))

pointer-maybe-invalid-region
     ;; If M-A points into A-memory, pretend it points into boxed storage.
        ((m-2) q-pointer m-a)
        (jump-less-than m-2 (a-constant lowest-a-mem-virtual-address) pointer-invalid-region)
        (jump-greater-or-equal m-2 (a-constant lowest-io-space-virtual-address) pointer-invalid-region)

pointer-boxed
        (popj-after-next
          (pdl-push) dpb m-2 q-pointer (a-constant (byte-value q-data-type dtp-fix)))
       ((m-t) (a-constant (plus (byte-value q-data-type dtp-fix) 2)))

;;;;

;;; Warning:  The below code is being fixed by dg...

;(locality d-mem)
;(start-dispatch 5 0)
;d-verify-accumulator
;       (n-bit verify-immediate)                ;trap
;       (n-bit verify-immediate)                ;null
;       (n-bit verify-immediate)                ;unreconciled
;       (verify-symbol)                         ;symbol
;       (verify-symbol-header)                  ;symbol-header
;       (n-bit verify-immediate)                ;fix
;       (verify-extended-number)                ;extended-number
;       (n-bit verify-immediate)                ;header
;       (n-bit p-bit illop)                     ;gc-forward
;       (verify-pointer)                        ;external-value-cell-pointer
;       (n-bit p-bit illop)                     ;one-q-forward
;       (n-bit p-bit illop)                     ;header-forward
;       (n-bit p-bit illop)                     ;body-forward
;       (verify-pointer)                        ;locative
;       (verify-pointer)                        ;list
;       (n-bit verify-immediate)                ;u code entry
;       (verify-fef-pointer)                    ;fef
;       (verify-array-pointer)                  ;array-pointer
;       (n-bit verify-immediate)                ;array-header ;gak!  this is wrong. should be illop
;       (verify-array-pointer)                  ;stack-group
;       (verify-pointer)                        ;closure
;       (n-bit verify-immediate)                ;indexed-offset
;       (verify-pointer)                        ;select-method
;       (verify-instance-pointer)               ;instance
;       (n-bit verify-immediate)                ;instance-header
;       (verify-pointer)                        ;entity
;       (verify-pointer)                        ;unused-32
;       (n-bit verify-immediate)                ;self-ref-pointer
;       (n-bit verify-immediate)                ;character
;       (verify-pointer)                        ;rplacd-forward
;       (n-bit p-bit illop)                     ;36
;       (n-bit verify-immediate)                ;small-flonum
;(end-dispatch)
;(locality a-mem)

#-lambda (begin-comment)

(locality d-mem)
(start-dispatch 5 0)

;;; this verification stuff hacked by DG to find bugs in UCODE. -5/28/86
d-verify-accumulator-simple                     ;Simple verifications of accumulator contents
                                                ;to detect dangerous types being left in accumulators

        (n-bit r-bit)                           ;trap
        (n-bit r-bit)                           ;null
        (n-bit r-bit)                           ;unreconciled
        (n-bit r-bit)                           ;symbol
        (n-bit p-bit illop)                     ;symbol-header
        (n-bit r-bit)                           ;fix
        (n-bit r-bit)                           ;extended-number
        (n-bit r-bit)                           ;header
        (n-bit p-bit illop)                     ;gc-forward
        (n-bit r-bit)                           ;external-value-cell-pointer
        (n-bit p-bit illop)                     ;one-q-forward
        (n-bit p-bit illop)                     ;header-forward
        (n-bit p-bit illop)                     ;body-forward
        (n-bit r-bit)                           ;locative
        (n-bit r-bit)                           ;list
        (n-bit r-bit)                           ;u code entry
        (n-bit r-bit)                           ;fef
        (n-bit r-bit)                           ;array-pointer
        (n-bit p-bit illop)                     ;array-header
        (n-bit r-bit)                           ;stack-group
        (n-bit r-bit)                           ;closure
        (n-bit r-bit)                           ;indexed-forward
        (n-bit r-bit)                           ;select-method
        (n-bit r-bit)                           ;instance
        (n-bit p-bit illop)                     ;instance-header
        (n-bit r-bit)                           ;entity
        (n-bit p-bit illop)                     ;unused-32
        (n-bit r-bit)                           ;self-ref-pointer
        (n-bit r-bit)                           ;character
        (n-bit p-bit illop)                     ;rplacd-forward
        (n-bit r-bit)                           ;36
        (n-bit r-bit)                           ;small-flonum
(end-dispatch)

;(locality a-mem)
;a-verify-accumulator (0)

(locality i-mem)

;verify-immediate                               ;contents ok...
;        (popj)

;boxed-pointer?
;        ((a-verify-accumulator) pdl-pop)
;        ((pdl-push) m-a)
;        ((pdl-push) m-t)
;        ((pdl-push) m-1)
;        ((pdl-push) m-2)
;        ((pdl-push) m-3)
;        ((pdl-push) m-4)
;        ((pdl-push) m-5)
;        ((pdl-push) m-6)
;        (call-xct-next pointer-info)
;       ((pdl-push) a-verify-accumulator)
;        (call-not-equal m-t (a-constant (plus (byte-value q-data-type dtp-fix) 2)) illop)
;        (pdl-pop)                                     ;Extra value from pointer-info.
;        ((m-6) pdl-pop)
;        ((m-5) pdl-pop)
;        ((m-4) pdl-pop)
;        ((m-3) pdl-pop)
;        ((m-2) pdl-pop)
;        ((m-1) pdl-pop)
;        ((m-t) pdl-pop)
;        ((m-a) pdl-pop)
;        ((pdl-push) a-verify-accumulator)
;        (popj-after-next
;          (vma-start-read) a-verify-accumulator)
;       (check-page-read)

verify-accumulator-contents
        (dispatch q-data-type pdl-top d-verify-accumulator-simple)
        (popj)
        ;(call boxed-pointer?)

;verify-symbol
;       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward)) v-header-forward-ok)
;        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-symbol-header)) illop)
;        (popj)

;verify-symbol-header
;       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward)) v-header-forward-ok)
;        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-header)) illop)
;        (popj)

;verify-extended-number
;       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward)) v-header-forward-ok)
;     ;; UNCONSing can leave bignum pointers pointing to array headers.
;        (popj-data-type-equal md (a-constant (byte-value q-data-type dtp-array-header)))
;        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-header)) illop)
;        (popj)

;verify-fef-pointer
;       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward)) v-header-forward-ok)
;        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-header)) illop)
;        (popj)

;verify-array-pointer
;       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward)) v-header-forward-ok)
;        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-header)) illop)
;        (popj)

;verify-instance-pointer
;       (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward)) v-header-forward-ok)
;        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-instance-header)) illop)
;        (popj)

;verify-pointer

;;;;

;v-header-forward-ok
;       ((vma-start-read) md)
;       (check-page-read-no-interrupt)
;       (dispatch-xct-next q-data-type pdl-top d-verify-accumulator)
;       (no-op)

verify-accumulators     ;clobbers M-1 and M-TEM (of course)

;;; why was this here??? -dg
;        ((m-tem) dpb m-zero q-all-but-typed-pointer a-inhibit-scheduling-flag)
;        (popj-not-equal m-tem a-v-nil)

        ((m-1) (a-constant (m-mem-loc m-zr)))
verify-accumulators-loop
        ((oa-reg-high) dpb m-1 oah-m-src a-zero)
        ((pdl-push) m-garbage)
        (call verify-accumulator-contents)
        (pdl-pop)
        (jump-less-than-xct-next m-1 (a-constant (m-mem-loc m-k)) verify-accumulators-loop)
       ((m-1) add m-1 (a-constant 1))
        (popj)

#-lambda (end-comment)
;;;


(assign extra-pdl-area-number
        (plus (byte-value q-data-type dtp-fix)
              (difference (a-mem-loc a-v-extra-pdl-area)
                          (a-mem-loc a-v-resident-symbol-area))
              ;(eval (find-position-in-list 'extra-pdl-area
              ;                            initial-area-list))
              ))

;;; Extra-PDL region not big enough to hold object.  If object is not too big, reset extra-pdl
;;; and retry.

extra-pdl-overflow
        ((vma-start-read) add m-s a-v-area-region-list)
        (check-page-read)
        ((m-k) q-pointer md)            ;m-k gets extra-pdl region.
        ((vma-start-read) add m-k a-v-region-length)
        (check-page-read)
        ((m-3) q-pointer md)
        (jump-less-than m-b a-3 extra-pdl-overflow-reset)
     ;; Object is too big to fit in extra-pdl even when reset, cons it in working-storage.
        ((m-s) dpb m-zero q-all-but-typed-pointer a-cnsadf)
extra-pdl-overflow-retry
     ;; in path from below, make sure consing done while reseting gets stored out.
     ;; in drop thru path, make sure right area is up.
       (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
     ;; This emulates what would happen if we could tail-recurse back into ALLOCATE-STORAGE.
        ((vma-start-read) add m-s a-v-area-region-list)
        (check-page-read)
        (popj-after-next (m-k) q-pointer md)
       (no-op)

extra-pdl-overflow-reset
        (call reset-extra-pdl)
        (jump extra-pdl-overflow-retry)

;;; Flush all pointers to EXTRA-PDL-AREA out of the machine.  Registers M-ZR thru M-J,
;;; A-VERSION thru A-END-Q-POINTERS, PDL-BUFFER.

;This called from above and also from BOOT-RESET-EXTRA-PDL.  Calling from there a
; good idea on warm boot, and also, we need to do the
; INITIALIZE-VIRTUAL-PAGE-DATA-FOR-REGION so PHT and virtual page data, etc in phase.

reset-extra-pdl
        ((m-e) (a-constant (m-mem-loc m-zr)))
reset-extra-pdl-m-loop
        ((oa-reg-high) dpb m-e oah-m-src a-zero)
        ((m-t) m-garbage)                       ;M-GARBAGE is location 0@m.
        (call extra-pdl-purge)
        ((oa-reg-low) dpb m-e oal-m-dest a-zero)
        ((m-garbage) m-t)
        (jump-not-equal-xct-next m-e (a-constant (m-mem-loc m-j)) reset-extra-pdl-m-loop)
       ((m-e) add m-e (a-constant 1))
        ((m-e) (a-constant (a-mem-loc a-version)))
reset-extra-pdl-a-loop
        ((oa-reg-high) dpb m-e oah-a-src a-zero)
        ((m-t) a-garbage)                       ;A-GARBAGE is location 0@a.
        (call extra-pdl-purge)
        ((oa-reg-low) dpb m-e oal-a-dest a-zero)
        ((a-garbage) m-t)
        (jump-not-equal-xct-next m-e (a-constant (a-mem-loc a-end-q-pointers))
            reset-extra-pdl-a-loop)
       ((m-e) add m-e (a-constant 1))
        ((pdl-buffer-index) a-pdl-buffer-head)
reset-extra-pdl-pdl-loop
        ((m-e) pdl-buffer-index)        ;Save PI
        (call-xct-next extra-pdl-purge)
       ((m-t) c-pdl-buffer-index)
        ((pdl-buffer-index) m-e)        ;Restore possibly-clobbered PI
        ((c-pdl-buffer-index) m-t)
        (jump-not-equal-xct-next a-e pdl-buffer-pointer reset-extra-pdl-pdl-loop)
       ((pdl-buffer-index) add m-e (a-constant 1))
     ;; Now reset the extra-pdl free-pointer and try again.
        ((m-s) (a-constant extra-pdl-area-number))
        ((vma-start-read) add m-s a-v-area-region-list)   ;this may not be set up from
                                ;boot-reset-extra-pdl
        (check-page-read)
        ((m-k) q-pointer md)            ;m-k gets extra-pdl region.
        ((md) a-zero)
        ((vma-start-write) add m-k a-v-region-free-pointer)
        (check-page-write)
     ;; Initialize structure-handles and volatility bits.
        (call initialize-virtual-page-data-for-region)
        (popj)

;;; If M-T points to extra-pdl, copy out what it points to and change it.
;;; Must protect all lettered registers, M-1, M-2.
extra-pdl-purge
     ;; Make sure pointer is to EXTRA-PDL-AREA.  Actually, this test
     ;;  should prevent lossage with stack-closures, below.
        ((md) q-pointer m-t (a-constant (byte-value q-data-type dtp-fix)))
        (popj-less-than md a-v-extra-pdl-area)
        (popj-greater-or-equal md a-v-micro-code-entry-area)
        ((md) m-t)              ;Get full ptr including data type
        ((vma-start-write) (a-constant (eval (+ 400 %sys-com-temporary))))
        (illop-if-page-fault)
      (error-table crash page 1 not wired while in extra-pdl-purge)
        (gc-write-test (i-arg 20))      ;Use regular GC-WRITE-TEST mechanism
                ;but dont frotz stack-closures.  Actually, should not get here
                ;on stack-closures due to test above.
                ;But having bit doesnt cost anything.
   ;No more stack closures any more.
        ((m-t) md)
        (popj)


(begin-comment) Zwei Lossage (end-comment)

;;; Region allocation.  Call ALLOCATE-ACTIVE-REGION or ALLOCATE-COPY-REGION with area in
;;; M-S, minimum size in M-B, volatility in M-E (for copy regions only, for active regions
;;; it is copied from area-region-bits).  Scavenge-enable is copied from area-region-bits,
;;; or set for copy regions.

get-area-region-bits
        ((vma-start-read) add m-s a-v-area-region-bits)
        (check-page-read)
        ((m-tem) (lisp-byte %%region-space-type) md)
        (call-equal m-tem (a-constant (eval %region-space-fixed)) trap)
     (error-table attempt-to-extend-fixed-area)
        ((m-4) ior md (a-constant (byte-mask %%region-oldspace-meta-bit)))
        (popj-after-next
          (m-tem) (a-constant (eval %region-representation-type-lisp)))
       ((m-4) dpb m-tem (lisp-byte %%region-representation-type) a-4)

allocate-copy-region
        (jump-equal m-s (a-constant extra-pdl-area-number) extra-pdl-overflow)
        (call get-area-region-bits)
     ;; Inside transporter, new regions are always scavengeable copy space.
        ((md) (a-constant (eval %region-space-copy)))
        ((m-4) dpb md (lisp-byte %%region-space-type) a-4)
        ((m-4) dpb (m-constant -1) (lisp-byte %%region-scavenge-enable) a-4)
     ;; Setup desired volatility.
        ((m-4) dpb m-e (lisp-byte %%region-volatility) a-4)
        (jump allocate-region)

allocate-active-region
        (jump-equal m-s (a-constant extra-pdl-area-number) extra-pdl-overflow)
        (call get-area-region-bits)             ;return in M-4
     ;; Volatility for active regions comes from area-region-bits.
        ((m-e) (lisp-byte %%region-volatility) m-4)
        ((m-tem) (lisp-byte %%region-space-type) m-4)
        (jump-greater-or-equal m-tem (a-constant (eval %region-space-moby-fixed))
                               allocate-moby-region)

allocate-region
        ((vma-start-read) add m-s a-v-area-region-size)
        (check-page-read)
        ((m-3) q-pointer md)                    ;Normal amount to allocate.
        (jump-greater-than m-3 a-b rcons1)
        ((m-3) add m-b (a-constant 1))          ;M-3 amount we want to allocate.
rcons1  (call make-region)              ;Allocate a region of that size (to M-K).
     ;; Update REGION-AREA-MAP to know about new region.
        ((md) dpb m-s q-pointer a-zero)
        ((vma-start-write) add m-k a-v-region-area-map)
        (check-page-write)
     ;; Insert new region at front of AREA-REGION-LIST.
        ((vma-start-read) add m-s a-v-area-region-list)
        (check-page-read)
        ((m-3) md)                              ;This becomes the next region.
        ((md-start-write) dpb m-k q-pointer a-3)
        (check-page-write)
        ((md) m-3)
        (popj-after-next
          (vma-start-write) add m-k a-v-region-list-thread)
       (check-page-write)

allocate-moby-region
        (call save-state-acs-on-pdl)
        (call p3zero)
        ((arg-call ref-support-vector) (i-arg svc-new-moby-region) i-dont-chain)
        (dispatch transport md)
        ((pdl-push) md)
  ;args are (area, size-of-object being made, proposed volatility <probably random for now>)
        ((pdl-push) dpb m-s q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) dpb m-b q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) dpb m-e q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((arg-call mmcall) (i-arg 3))
        (call restore-state-acs-from-pdl)
        ((LOCATION-COUNTER) SUB LOCATION-COUNTER (A-CONSTANT #+lambda 2 #+exp 1))
                        ;IT IS NECESSARY THAT
                        ;M-INSTRUCTION-BUFFER ACTUALLY HAVE THE LAST INSTRUCTION
                        ;EXECUTED (IE NOT SUFFICIENT MERELY THAT THE CORRECT INSTRUCTION
                        ;WILL BE FETCHED NEXT TIME AROUND THE MAIN LOOP).  THIS IS BECAUSE
                        ;THE CURRENT MACRO-INSTRUCTION, WHICH MAY BE BEING REENTERED
                        ;IN THE MIDDLE, CAN DISPATCH AGAIN ON M-INSTRUCTION-STREAM
                        ;(TO GET THE DESTINATION IN MISC, FOR EXAMPLE).  THE SIMPLEST
                        ;WAY TO ASSURE THIS IS TO BACK UP THE LOCATION COUNTER AND
                        ;RE-ADVANCE IT.
        (DISPATCH ADVANCE-INSTRUCTION-STREAM i-dont-chain)
     (error-table restart allocate-moby-region-restart)
       (no-op)  ;seems to be necessary ..
        ((vma-start-read) add m-s a-v-area-region-list)  ;restart region scan
        (check-page-read)
        ((m-k) q-pointer md)
        (popj)


;;; MAKE A REGION.
;;; M-3 HAS SIZE IN WORDS, M-4 HAS REGION-BITS
;;; SETS UP EVERYTHING ELSE EXCEPT REGION-LIST-THREAD, RETURNS REGION IN M-K, BASHES M-E, M-T
;;; PRESERVES M-3 AND M-4, EXCEPT M-3 IS ROUNDED UP TO THE NEXT QUANTUM BOUNDARY
MAKE-REGION
        ((M-3) ADD M-3 (A-CONSTANT (EVAL (1- %ADDRESS-SPACE-QUANTUM-SIZE)))) ;Round up to
        ((M-3) SELECTIVE-DEPOSIT M-3 VMA-QUANTUM-BYTE A-ZERO)                ; quantum bound
        ((A-REGION-CONS-ALARM) M+A+1 M-ZERO A-REGION-CONS-ALARM)
        ((M-TEM) VMA-PAGE-ADDR-PART M-3)        ;Length of region in pages
        ((A-PAGE-CONS-ALARM) ADD M-TEM A-PAGE-CONS-ALARM)
        ;; Search address-space-map for suitable number of consecutive zeros
        ;; M-T gets starting address here either from A-NEW-REGION-SEARCH-OFFSET
        ;; or by the usual way which is first page after fixed areas
        ((m-t) dpb m-zero q-all-but-pointer a-new-region-search-offset)
        (jump-greater-or-equal m-t a-v-first-unfixed-area make-region-continue)
        ((M-T) A-V-FIRST-UNFIXED-AREA)
make-region-continue
        ((M-TEM) A-LOWEST-DIRECT-VIRTUAL-ADDRESS)  ;Avoid losing if additional direct
        ((M-TEM) VMA-PAGE-ADDR-PART M-TEM)         ; space created or band allocated too big.
        (JUMP-LESS-THAN M-TEM a-address-space-maximum MAKE-REGION-0)
        ((M-TEM) dpb m-zero (byte 6 0) a-address-space-maximum) ;Ending address.  Must be on an even
                        ;boundary even if page partition has been allocated randomly.
MAKE-REGION-0
        ((M-K) DPB M-TEM VMA-PAGE-ADDR-PART
         (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
MAKE-REGION-1
        ((M-E) ADD M-T A-3)                     ;End of large enough region starting here
MAKE-REGION-2
        (CALL-GREATER-OR-EQUAL M-T A-K TRAP)    ;Reached end of map, with no luck
            (ERROR-TABLE VIRTUAL-MEMORY-OVERFLOW)
        (CALL ADDRESS-SPACE-MAP-LOOKUP)         ;This could be optimized to save some mem rds?
        (JUMP-NOT-EQUAL-XCT-NEXT M-TEM A-ZERO MAKE-REGION-1)
       ((M-T) ADD M-T (A-CONSTANT (EVAL %ADDRESS-SPACE-QUANTUM-SIZE)))
        (JUMP-LESS-THAN M-T A-E MAKE-REGION-2)  ;Found free space, but not big enough yet
        ((M-T) SUB M-T A-3)                     ;Base address of free space found
        ;; M-T has origin, M-3 has length, M-4 has bits.  Put region in tables.
        ((VMA-START-READ) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-FREE-REGION/#-LIST))))
        (illop-if-page-fault)
      (error-table crash page 1 not wired)
        ((M-K) Q-POINTER MD)    ;Number of new region
        (CALL-EQUAL M-K A-ZERO TRAP)            ;Out of region numbers
            (ERROR-TABLE REGION-TABLE-OVERFLOW)
        ((VMA-START-READ) ADD M-K A-V-REGION-LIST-THREAD)       ;CDR OFF OF LIST
        (CHECK-PAGE-READ-no-interrupt)
        ((MD) MD)       ;THIS ENSURES READ CYCLE FINISHES
        ((VMA-START-WRITE) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-FREE-REGION/#-LIST))))
        (illop-if-page-fault)
      (error-table crash page 1 not wired)
        ;; Proceed to initialize the various tables, except list-thread which caller does.
        ((MD) Q-POINTER M-T a-zero)
        ((VMA-START-WRITE) ADD M-K A-V-REGION-ORIGIN)
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((MD) Q-POINTER M-3 a-zero)
        ((VMA-START-WRITE) ADD M-K A-V-REGION-LENGTH)
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((MD) Q-POINTER M-4 a-zero)
        ((VMA-START-WRITE) ADD M-K A-V-REGION-BITS)
        (illop-if-page-fault)
      (error-table crash page-fault in wired area)
        ;; Set up address-space-map
        ((M-E) ADD M-T A-3)                     ;End of region
MAKE-REGION-3
        ;(call allocate-quantum-for-new-region)     ;;; we don't want to allocate page space until the last possible moment
        (CALL ADDRESS-SPACE-MAP-STORE)

        ;*** depends on L1 map block being half a quantum
        ((md) m-t)
        ((m-tem1) ldb (lisp-byte %%region-volatility) m-4)
#+lambda((m-tem) l1-map)
#+exp   ((m-tem) ldb l1-map-all-but-old-and-valid l1-map
           (a-constant (plus (byte-value l1-map-valid-exp 1)
                             (byte-value l1-map-old-exp 1))))
        ((#+lambda l1-map
          #+exp vma-write-l1-map) dpb m-tem1 map1-volatility a-tem)
        ((md) add md (a-constant (eval (* 32. 400))))
#+lambda((m-tem) l1-map)
#+exp   ((m-tem) ldb l1-map-all-but-old-and-valid l1-map
           (a-constant (plus (byte-value l1-map-valid-exp 1)
                             (byte-value l1-map-old-exp 1))))
        ((#+lambda l1-map
          #+exp vma-write-l1-map) dpb m-tem1 map1-volatility a-tem)

        ((M-T) ADD M-T (A-CONSTANT (EVAL %ADDRESS-SPACE-QUANTUM-SIZE)))
        (JUMP-LESS-THAN M-T A-E MAKE-REGION-3)
        (call initialize-virtual-page-data-for-region)
        ;; Finish setting up tables
  ;Set region-allocation-status of new region to untyped zero.  Thus, no list header
  ; gets inserted if first consing is LIST stuff. This minimizes randomness
  ; and avoids problems with MOBY (and possibly other places).
        ((md) a-zero)
        ((vma-start-write) add m-k a-v-region-allocation-status)
        (check-page-write-no-interrupt)
                 ;FREE PTR = 0
        ((VMA-START-WRITE) ADD M-K A-V-REGION-FREE-POINTER)
        (illop-if-page-fault)
      (error-table crash page-fault in wired area)
        (popj-after-next (VMA-START-WRITE) ADD M-K A-V-REGION-GC-POINTER)
       (check-page-write-no-interrupt)

(begin-comment)                                 ;don't want to allocate page space until last minute
                                                ;see comment at beginning of QUANTUM-MAP-REGION in UC-INITIALIZATION
                                                ;if you want to put this back.
;;; Here when allocating a quantum to a new region.  If the quantum map entry is valid then
;;; the quantum already has page space allocated to it and we don't need to do anything.
;;; Otherwise we must allocate page space for the quantum and set up the quantum map entry.
;;; Address is in M-T.  allowed to clobber vma, md, m-tem, m-tem1.
;;; much of this code stolen from its predecessor ALLOCATE-NEW-QUANTUM.
allocate-quantum-for-new-region
        ((vma) ldb vma-quantum-byte m-t)
        ((vma) dpb vma (byte-field 31. 1) a-zero)       ;double it
        ((vma) add vma (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((a-quantum-map-index) add vma a-v-quantum-map) ;compute quantum map location and save for later
                                                ;a-quantum-map-index also used in disk swap code.  should not collide.

        ((vma-start-read) a-quantum-map-index)  ;see if quantum already exists.  The quantum might already have page space
                                                ;allocated to it if its part of virtual memory was preveously in use but has
                                                ;been reclaimed by the garbage collector.  FREE-REGION-1 does not attempt
                                                ;to free quanta yet.
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (jump-if-bit-set (lisp-byte %%pq1-quantum-is-valid) md allocate-quantum-for-new-region-quantum-already-exists)

        (call-xct-next allocate-page-space-for-quantum)
       ((a-disk-swap-saved-m-a) m-t)            ;should not collide with disk swap code
                                                ;a-disk-swap-saved-m-a is an argument to allocate-page-space-for-quantum.
        ;;; on return from allocate-page-space-for-quantum, m-tem contains location in PAGE for this quantum

        ((md) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-tem)
                                                ;quantum valid, memory quantum, no need to copy, page offset in a-tem
        ((vma-start-write) a-quantum-map-index)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)

        ((m-tem) a-page-partition-to-use)
        ((md) dpb m-tem (lisp-byte %%pq2m-partition-number) a-zero)
        ((vma-start-write) m+a+1 m-zero a-quantum-map-index)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)

        (popj)

allocate-quantum-for-new-region-quantum-already-exists
        (popj-if-bit-clear md (lisp-byte %%pq1-quantum-is-device))
        (call illop)
     (error-table crash attempt to allocate device quantum as memory)
(end-comment)

(begin-pagable-ucode)
;;;New %MAKE region, used to allocate new regions to MOBY-AREAS.  Could conceivably
;;;   be useful in some other cases as well.  Page-structure-handles
;;;   (ie virtual-page-data) are initialized.
XMKRG  (misc-inst-entry %MAKE-REGION)      ;(%make-region area region-bits size-in-qs)
        ((m-3) q-pointer c-pdl-buffer-pointer-pop)      ;size
        ((m-4) q-pointer c-pdl-buffer-pointer-pop)      ;region-bits.
        ((m-s) q-typed-pointer c-pdl-buffer-pointer-pop)  ;area
        (call rcons1)                   ;make the region  and link it in.
        (popj-after-next
         (m-t) q-pointer m-k (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)

xafpaw (misc-inst-entry %advance-free-pointer-and-wipe)  ;(.. region data-type pointer nwords)
        (call invalidate-cons-caches)
        ((m-b) q-pointer c-pdl-buffer-pointer-pop)
        ((m-2) q-pointer c-pdl-buffer-pointer-pop)
        ((m-1) q-pointer c-pdl-buffer-pointer-pop)
        ((m-k) q-pointer c-pdl-buffer-pointer-pop)
        ((vma-start-read) add m-k a-v-region-origin)
        (illop-if-page-fault)
      (error-table crash page-fault in wired area)
        ((m-t) q-pointer md)
        ((vma-start-read) add m-k a-v-region-free-pointer)
        (illop-if-page-fault)
      (error-table crash page-fault in wired area)
        ((m-t) output-selector-mask-25 add md a-t)
        ((md-start-write) add md a-b)           ;update free pointer.
        (illop-if-page-fault)
      (error-table crash page-fault in wired area)
        (call touch-pages-in-new-object)
        ((m-a) m-b)
        ((m-enable-store-unreconciled) dpb m-minus-one a-flags)
        (call-xct-next initialize-storage)
       ((pdl-push) dpb m-1 q-data-type a-2)
        ((m-enable-store-unreconciled) dpb m-zero a-flags)
        (jump xfalse)

(end-pagable-ucode)

;need this in real memory so opcs aren't wasted paging it in...
gc-write-unreconciled           ;from d-gc-write-test
        (popj-if-bit-set (byte 1 1) read-i-arg)
        (popj-if-bit-set m-enable-store-unreconciled)
        (call illop)
      (error-table crash fatal moby error)


;below is the old form of %make-region, which has been flushed.
;;;; SUBROUTINE TO CREATE A REGION, CALLED ONLY BY AREA-CREATOR
;;;; EXISTS MAINLY BECAUSE THE MICROCODE HAS TO KNOW HOW TO DO THIS ANYWAY
;XMKRG (MISC-INST-ENTRY %MAKE-REGION)
;       ((M-3) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;SIZE
;       (CALL-XCT-NEXT MAKE-REGION)
;       ((M-4) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;BITS
;       (POPJ-AFTER-NEXT
;        (M-T) Q-POINTER M-K (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       (NO-OP)

;Given an address in M-T, look up in the address space map, return result in M-TEM
ADDRESS-SPACE-MAP-LOOKUP
        ;; Get word from ADDRESS-SPACE-MAP (assuming it starts on proper boundary!)
; Do you have any idea how long it takes to make a region?  200ns is nothing compared to
; the time I wasted building a new cold load because of this brain damage!
;       ((VMA-START-READ) ADDRESS-SPACE-MAP-WORD-INDEX-BYTE M-T A-V-ADDRESS-SPACE-MAP)
        ((vma) address-space-map-word-index-byte m-t)
        ((vma-start-read) add vma a-v-address-space-map)
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((M-TEM) ADDRESS-SPACE-MAP-BYTE-NUMBER-BYTE M-T)        ;Byte number in that word
        ((M-TEM) DPB M-TEM ADDRESS-SPACE-MAP-BYTE-MROT A-ZERO)
#+lambda(POPJ-AFTER-NEXT (OA-REG-LOW) SUB (M-CONSTANT 40) A-TEM) ;40 doesn't hurt here, IORed
#+exp   (popj-after-next (oa-reg-low) add m-tem (a-constant 1_16.)) ;rotate right
       ((M-TEM) (BYTE-FIELD (EVAL %ADDRESS-SPACE-MAP-BYTE-SIZE) 0) MD)

;Given an address in M-T, store M-K into the address space map.
ADDRESS-SPACE-MAP-STORE
        ;; Get word from ADDRESS-SPACE-MAP (assuming it starts on proper boundary!)
; Do you have any idea how long it takes to make a region?  200ns is nothing compared to
; the time I wasted building a new cold load because of this brain damage!
;       ((VMA-START-READ) ADDRESS-SPACE-MAP-WORD-INDEX-BYTE M-T A-V-ADDRESS-SPACE-MAP)
        ((vma) address-space-map-word-index-byte m-t)
        ((vma-start-read) add vma a-v-address-space-map)
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((M-TEM) ADDRESS-SPACE-MAP-BYTE-NUMBER-BYTE M-T)        ;Byte number in that word
        ((m-TEM1) MD)
        ((OA-REG-LOW) DPB M-TEM ADDRESS-SPACE-MAP-BYTE-MROT A-ZERO)
        (popj-after-next
          (MD-START-WRITE) DPB M-K (BYTE-FIELD (EVAL %ADDRESS-SPACE-MAP-BYTE-SIZE) 0) A-TEM1)
       (illop-if-page-fault)
      ;;(no error table entry because of the preceeding popj-after-next)

;       (POPJ-AFTER-NEXT (OA-REG-LOW) DPB M-TEM ADDRESS-SPACE-MAP-BYTE-MROT A-ZERO)
;       ((MD-START-WRITE) DPB M-K
;               (BYTE-FIELD (EVAL %ADDRESS-SPACE-MAP-BYTE-SIZE) 0) A-TEM1)

;;; CALL THIS ROUTINE TO FREE UP A REGION, NUMBER IN M-K (MUST BE PURE NUMBER).
;;; BASHES M-A,M-B,M-D,M-E,M-K,M-T, M-1...M-2, A-TEM1...A-TEM3

XFREE-REGION (MISC-INST-ENTRY %GC-FREE-REGION)
        ((M-K) Q-POINTER C-PDL-BUFFER-POINTER-POP)
FREE-REGION
        ((MD) a-zero)
        ((VMA-START-WRITE) ADD M-K A-V-REGION-BITS)     ;Clear the REGION-BITS, = free status
        (CHECK-PAGE-WRITE)
        ((M-D) DPB (M-CONSTANT -1) (BYTE-FIELD 1 24.)   ;Change swap-status to Flushable
                (A-CONSTANT 2))                         ; and disconnect the virtual page
        (CALL-XCT-NEXT UPDATE-REGION-PHT);Note that this sets M-1 and M-2 to the region bounds
            ;Make read-only, no access, in PHT2
       ((MD) (A-CONSTANT (BYTE-VALUE %%region-access-and-status-bits 2)))
        ;; Put region in M-K onto free region-table-entry list
        ((VMA-START-READ) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-FREE-REGION/#-LIST))))
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((m-TEM2) MD)
        ((MD-START-WRITE) Q-POINTER M-K
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((MD) A-TEM2)
        ((VMA-START-WRITE) ADD M-K A-V-REGION-LIST-THREAD)
        (CHECK-PAGE-WRITE)
        ;; Write -1 to region-area-map to signal region free.
        ((md) dpb m-minus-one q-pointer a-zero)
        ((vma-start-write) add m-k a-v-region-area-map)
        (check-page-write)
        ;; Remove from ADDRESS-SPACE-MAP
        ;; Referencing these addresses will halt in PAGE-IN-GET-MAP-BITS
        ((M-T) M-1)
FREE-REGION-1
        (CALL-XCT-NEXT ADDRESS-SPACE-MAP-STORE)
       ((M-K) A-ZERO)
        ((M-T) ADD M-T (A-CONSTANT (EVAL %ADDRESS-SPACE-QUANTUM-SIZE)))
        (JUMP-LESS-THAN M-T A-2 FREE-REGION-1)
        (POPJ-AFTER-NEXT (M-T) A-V-NIL)
       (NO-OP)

;Remove all information about the region in M-K from the page map,
;and fix the PHT entries of any swapped-in pages.
;Call with MD containing the new REGION-BITS entry for the region,
; and M-D containing A-V-NIL or the new swap-status.
;Sets M-1 and M-2 to the bounds of the region.
;Bashes M-A, M-B, M-E, M-T, tems.
; This called when flipping or flushing region.
UPDATE-REGION-PHT
        ((M-E) (LISP-BYTE %%REGION-MAP-BITS) MD)        ;Arg for XCPGS0
        ((VMA-START-READ) ADD M-K A-V-REGION-ORIGIN)    ;Find virtual address range of region
        (CHECK-PAGE-READ)
        ((M-1) Q-POINTER MD)
        ((VMA-START-READ) ADD M-K A-V-REGION-LENGTH)
        (CHECK-PAGE-READ)
        ((M-2) Q-POINTER MD)
        ((MD M-2) ADD M-1 A-2)
        ;; M-1 has lowest address in region, M-2 has highest address in region +1
        ;; Both are necessarily a multiple of the page size.
        ;; Call XCPGS0 on each page, to fix the PHT entry (if any) and the map.
UPDATE-REGION-PHT-0
        (CALL-XCT-NEXT XCPGS0)
       ((C-PDL-BUFFER-POINTER-PUSH) SUB MD (A-CONSTANT (EVAL PAGE-SIZE)))
  ;if flipping region to oldspace, change level-1 map oldspace bit on explorer.
  ; do this here rather than in XCPGS0 because this is the only place it should matter,
  ; and minimize randomness if a guy does a somewhat random %CHANGE-PAGE-STATUS, etc.
#+exp   (jump-if-bit-set map2c-oldspace-meta-bit m-e update-region-pht-1)
#+exp   ((m-tem) l1-map)
#+exp   ((vma-write-l1-map) dpb m-zero l1-map-old-exp a-tem)
update-region-pht-1
        (JUMP-GREATER-THAN MD A-1 UPDATE-REGION-PHT-0)
        (POPJ)

GET-AREA-ORIGINS
        ((VMA-START-READ) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-AREA-ORIGIN-PNTR))))
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((VMA) SUB MD (A-CONSTANT 1)) ;1- ADDR OF REGION-ORIGIN TABLE
        ((M-K) (A-CONSTANT (A-MEM-LOC A-V-RESIDENT-SYMBOL-AREA)))
BEG02   ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((OA-REG-LOW) DPB M-K OAL-A-DEST A-ZERO)        ;DESTINATION
        ((A-GARBAGE) Q-POINTER MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-K) ADD M-K (A-CONSTANT 1))
        (JUMP-NOT-EQUAL M-K (A-CONSTANT (A-MEM-LOC A-V-FIRST-UNFIXED-AREA)) BEG02)
        ;; Now find the end of the last fixed area, which is where we can start making regions
        ;; Too bad the cold-load generator didn't store this anywhere for us
        ((M-K) M-A-1 M-K (A-CONSTANT (A-MEM-LOC A-V-RESIDENT-SYMBOL-AREA)))
        ((VMA-START-READ) ADD M-K A-V-REGION-LENGTH)
        (ILLOP-IF-PAGE-FAULT)
      (error-table crash page-fault in wired area)
        ((M-K) ADD MD A-V-INIT-LIST-AREA)       ;...the last fixed area
        ;; Round up to next multiple of a quantum
        (POPJ-AFTER-NEXT (M-K) ADD M-K (A-CONSTANT (EVAL (1- %ADDRESS-SPACE-QUANTUM-SIZE))))
       ((A-V-FIRST-UNFIXED-AREA) SELECTIVE-DEPOSIT M-K
                VMA-QUANTUM-BYTE (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))


;;; UN-CONS no longer works.  For compatibility, fill the given storage with a dummy array.

;;; Why don't we just do nothing (given that my machine just crashed unconsing a 2000 word
;;; bignum... pace.  Noop, cant leave garbage there..

un-cons
        (popj-equal m-2 a-zero)
        ((m-2) sub m-2 (a-constant 1))          ;M-2 gets length of array to fill with
        (jump-greater-than m-2 (a-constant (eval %array-max-short-index-length)) un-cons-1)
        ((md) add m-2 (a-constant (plus (byte-value q-data-type dtp-array-header)
                                        (byte-value %%array-number-dimensions 1)
                                        (eval art-32b))))
        ((vma-start-write) m-1)
        (check-page-write)
        (popj)

;;this is new code - if there are syntax errors, please fix them up --pace
un-cons-1
        ((m-2) sub m-2 (a-constant 1))
        ((md) (a-constant (plus (byte-value q-data-type dtp-array-header)
                                (byte-value %%array-number-dimensions 1)
                                (byte-value %%array-long-length-flag 1)
                                (eval art-32b))))
        ((vma-start-write) m-1)
        (check-page-write-no-interrupt)
        ((md) dpb m-2 (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-write) m+a+1 m-1 a-zero)
        (check-page-write)
        (popj)



;;; FLIPPER

;%GC-FLIP.  Flips all regions with flipping enabled, converting newspace
; to oldspace.
; This used to ignore the argument but now if the argument is a non-zero fixnum
;  it is stored into A-NEW-REGION-SEARCH-OFFSET which is used by MAKE-REGION
;  to determine where to start looking for free space to fit the region.
; Then goes over everything in the machine and makes sure it
; doesn't point to old-space.
; Usually reclaim oldspace at some point before calling this function.
xflip (misc-inst-entry %gc-flip)
        ((m-tem) q-typed-pointer pdl-pop)
        (jump-data-type-equal m-tem (a-constant (byte-value q-data-type dtp-fix)) xflip-arg-ok)
        ((m-tem) m-zero)
xflip-arg-ok
        ((a-new-region-search-offset) dpb m-tem vma-page-addr-part
          (a-constant (byte-value q-data-type dtp-fix)))
        ((a-gc-flip-ready) a-v-nil)             ;Due to creation of new old-space regions.
        ((a-scavenge-state) setz)
        (call invalidate-cons-caches)
        ((a-tv-current-sheet) a-v-nil)          ;Invalidate sheet cache.
        ((m-array-pointer) m-zero)              ;Invalidate array cache.
        ((a-font-pointer) m-zero)               ;Invalidate font cache.

     ;; Save the pointers in the machine out to memory.  We will flip, and then read them
     ;; back in ensuring that they point to newspace by the normal virtual memory read.
     ;; "The machine" is M-ZR through M-K, A-VERSION through A-END-Q-POINTERS, pdl buffer,
     ;; A-PDL-BUFFER-VIRTUAL-ADDRESS, A-QLBNDO, etc.
     ;; In order to avoid bugs with storing GC-FORWARDING pointers into the pdl buffer
     ;; and the like, we use the stack-group-switch mechanism to save the state of the machine
     ;; then load it back with transporting.
        (call-xct-next sglv)                    ;Save state, don't swap variables.
       ((m-tem) dpb (m-constant -1) (byte-field 1 6) a-sg-state)

        ((vma-start-read) (a-constant (eval (+ 400 %sys-com-number-regions))))
        (illop-if-page-fault)
        ((m-k) output-selector-extend-25 sub md (a-constant 1))
flip-region-loop
        ((vma-start-read) add m-k a-v-region-bits)
        (check-page-read)
        (call-if-bit-set (lisp-byte %%region-flip-enable) md flip-region)
        (jump-greater-than-xct-next m-k a-zero flip-region-loop)
       ((m-k) sub m-k (a-constant 1))

  ;stack group reload used to be here.  Lost because, not having done below loop, A-QCSTKG
  ; can point to OLDSPACE, which caused GC-FORWARDs to appear where not expected.

     ;; Now transport the magic A-memory variables, which constitute the root of the world.
        ((vma) (a-constant (eval (+ 400 %sys-com-temporary))))  ;Pretend was read from here.
        ((m-e) (a-constant (a-mem-loc a-version)))
xflipw2 ((oa-reg-high) dpb m-e oah-a-src a-zero)
        ((md) a-garbage)                        ;A-garbage is location 0@A.
        (dispatch transport-ac md)
        ((oa-reg-low) dpb m-e oal-a-dest a-zero)
        ((a-garbage) md)
        (jump-not-equal-xct-next m-e (a-constant (a-mem-loc a-end-q-pointers)) xflipw2)
       ((m-e) add m-e (a-constant 1))

     ;; Now restore the stack-group, which got copied back there someplace.
     ;; this used to be before above loop, but that lost, see comment above.
        (call sgent)                    ;Restore state
        ((a-sg-state) dpb m-tem (lisp-byte %%sg-st-current-state) a-sg-state)
        ((m-t) a-v-nil)

;;; These 3 registers need to be transported.  They should be moved up to the "boxed Q's" block
;;; for release 2.1.
;They are moved now.  Too easy to get screwwed by read-time conditionalization with present
; setup for such things.
;#+prolog((md) a-unify-dispatch)
;#+prolog(dispatch transport-ac md)
;#+prolog((a-unify-dispatch) md)
;#+prolog((md) a-lmp-vector)
;#+prolog(dispatch transport-ac md)
;#+prolog((a-lmp-vector) md)
;#+prolog((md) a-lmp-trail)
;#+prolog(dispatch transport-ac md)
;#+prolog((a-lmp-trail) md)

flip-scavenge-special-regions
        ((vma-start-read) a-v-region-origin)
        (check-page-read)
        (call-xct-next flip-scavenge-words)
       ((m-1) (a-constant 10.))         ;scavenge T and NIL.
        ((vma-start-read) m+a+1 m-zero a-v-region-origin)
        (check-page-read)               ;scavenge SYSTEM-COMMUNICATION-AREA
        (jump-xct-next flip-scavenge-words)
       ((m-1) (a-constant (eval (length system-communication-area-qs))))

flip-scavenge-words
        ((vma) md)
flip-scavenge-words-loop
        (popj-less-or-equal m-1 a-zero)
        ((vma-start-read) q-pointer vma)
        (check-page-read-no-interrupt)
        (dispatch transport-scav md)
        ((vma-start-write) q-pointer vma)
        (check-page-write-no-interrupt)
        (gc-write-test)
        ((vma) add vma (a-constant 1))
        (jump-xct-next flip-scavenge-words-loop)
       ((m-1) sub m-1 (a-constant 1))

flip-region
     ;; Change to old-space, clear meta bit, clear scavenge-enable, clear flip-enable.
        ((m-tem) (a-constant (eval %region-space-old)))
        ((m-tem1) andca md (a-constant (plus (byte-mask %%region-oldspace-meta-bit)
                                             (byte-mask %%region-scavenge-enable)
                                             (byte-mask %%region-flip-enable))))
        ((md-start-write) dpb m-tem (lisp-byte %%region-space-type) a-tem1)
        (check-page-write)
        (jump-xct-next update-region-pht)       ;Fix map, page table.
       ((m-d) a-v-nil)                          ;Don't change swap-status

))

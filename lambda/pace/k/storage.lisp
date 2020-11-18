;;; -*- Mode:LISP; Package:SIM; Fonts:(CPTFONT HL12I); Base:10; Readtable:ZL -*-


(defreg *active-cons-cache-area*              0 0)
(defreg *active-cons-cache-region*            0 1)
(defreg *active-cons-cache-region-origin*     0 2)
(defreg *active-cons-cache-free-pointer*      0 3)
(defreg *active-cons-cache-free-limit*        0 4)
(defreg *active-cons-cache-allocation-status* 0 5)
(defreg *list-allocation-threshold*           0 6)
(defreg *default-cons-area*                   0 7)
;; This is 4x page-size, so scavenger work is at least 4x cons work.
(defreg *scavenge-work-while-consing*         0 8) ;2000



(defun test-setup ()
  (setq *scavenge-work-while-consing* 0)
  (setq *list-allocation-threshold* 16.)
  (setq *default-cons-area* ksi:working-storage-area)
  (setq *active-cons-cache-area* 0)
  (setq *active-cons-cache-allocation-status* 0)
  (setq *active-cons-cache-free-pointer* 0)
  (setq *active-cons-cache-free-limit* 10000.))

(defun l3 (a b c)
  (cons a (cons b (cons c nil))))


(DEFKFUN L3 (A B C)
  TAG::P_7
  (TAIL-OPEN)
  (KOPEN)
  (MOVE O0 A2 CH-OPEN)
  (KCALL KSI:CONS '2 O1 (O1 'NIL))
  TAG::C_11
  (KCALL KSI:CONS '2 O1 (O0 A1))
  TAG::C_14
  (TAIL-CALL KSI:CONS '2 (O0 A0)))



(defun KSI:CONS (car cdr)
  (let ((loc (allocate-list-storage 2 *default-cons-area*)))
    (%loc-store-car loc car)
    (%loc-store-cdr loc cdr)
    (%k-make-pointer #.dtp-list loc)
    ))


(DEFKFUN KSI::CONS (CAR CDR)
  TAG::P_8
     (MOVE O0 '2 CH-OPEN)
     (KCALL ALLOCATE-LIST-STORAGE '2 A2 (O1 (%REGISTER *DEFAULT-CONS-AREA* 0 7)))
  TAG::C_19
     (MOVE MD A0)
     (MOVE VMA-START-WRITE A2)
     (MOVE MD A1)
     (ALU ALU-OR VMA-START-WRITE A2 '1)
     (KDPB RETURN '14 A2 %%PTR-DATA-TYPE CH-RETURN))





(defun allocate-list-storage (length area)
  (cond ((not (= area *active-cons-cache-area*))
         (allocate-list-storage-uncached length area))
        ((> (setq *active-cons-cache-allocation-status*
                  (+ length *active-cons-cache-allocation-status*))
            *list-allocation-threshold*)
         (allocate-list-storage-cached-list-header length area))
        (t
         (let ((temp (%loc-+ *active-cons-cache-free-pointer* length)))
           (if (%loc-> temp *active-cons-cache-free-limit*)
               (allocate-list-storage-uncached length area)
             (let ((ptr *active-cons-cache-free-pointer*))   ;prog1
               (setq *active-cons-cache-free-pointer* temp)
               ptr))))))


(DEFKFUN ALLOCATE-LIST-STORAGE (LENGTH AREA)
  TAG::P_10
      (ALU L-R GARBAGE A1 (%REGISTER *ACTIVE-CONS-CACHE-AREA* 0 0))
      (TEST BR-NOT-EQUAL)
      (BRANCH TAG::C_13)
  TAG::C_16
      (ALU L+R
           (%REGISTER *ACTIVE-CONS-CACHE-ALLOCATION-STATUS* 0 5)
           A0
           (%REGISTER *ACTIVE-CONS-CACHE-ALLOCATION-STATUS* 0 5))
      (MOVE A2 (%REGISTER *ACTIVE-CONS-CACHE-ALLOCATION-STATUS* 0 5))
      (ALU L-R GARBAGE A2 (%REGISTER *LIST-ALLOCATION-THRESHOLD* 0 6))
      (TEST BR-NOT-GREATER-THAN)
      (BRANCH TAG::C_21)
  TAG::C_19
      (MOVE O0 A0 CH-TAIL-OPEN)
      (TAIL-CALL ALLOCATE-LIST-STORAGE-CACHED-LIST-HEADER '2 (O1 A1))
  TAG::C_21
      (ALU L+R A3 (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) A0)
      (ALU L-R GARBAGE A3 (%REGISTER *ACTIVE-CONS-CACHE-FREE-LIMIT* 0 4))
      (TEST BR-NOT-GREATER-THAN)
      (BRANCH TAG::C_27)
  TAG::C_25
      (MOVE O0 A0 CH-TAIL-OPEN)
      (TAIL-CALL ALLOCATE-LIST-STORAGE-UNCACHED '2 (O1 A1))
  TAG::C_27
      (MOVE A4 (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3))
      (MOVE (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) A3)
      (MOVE RETURN A4 CH-RETURN)
  TAG::C_13
      (MOVE O0 A0 CH-TAIL-OPEN)
      (TAIL-CALL ALLOCATE-LIST-STORAGE-UNCACHED '2 (O1 A1)))




;;; Cons cache hit, but we need to insert a list header
;;; (so that find-structure-header doesn't have to search
;;; too far).  This path is a bit slower, but
;;; not nearly as bad as the uncached case.
(defun allocate-list-storage-cached-list-header (length area)
  (let ((new-free-pointer (%loc-+ *active-cons-cache-free-pointer* (+ length 2))))
    (if (%loc-> new-free-pointer *active-cons-cache-free-limit*)
        (allocate-list-storage-uncached length area)
      (progn (%loc-store-car *active-cons-cache-free-pointer*
                             (%k-make-pointer #.dtp-header #.%header-type-list))
             ;; can't just store one word header now
             ;; since conses are always on even addresses
             (%loc-store-cdr *active-cons-cache-free-pointer*
                             nil)
             (let ((ptr (%loc-+ *active-cons-cache-free-pointer* 2)))
               (setq *active-cons-cache-free-pointer* new-free-pointer)
               (setq *active-cons-cache-allocation-status* 0)
               ptr)))))
;      ;; the ucode does it like this? contorted but safer??
;      (setq start-of-list (%loc-+ *active-cons-cache-free-pointer* 1)
;           *active-cons-cache-free-pointer* new-free-pointer
;           *active-cons-cache-allocation-status* 0)
;      (%loc-store (%loc--1 start-of-list)
;                 (%k-make-pointer #.dtp-header #.%header-type-list)))))


(DEFKFUN ALLOCATE-LIST-STORAGE-CACHED-LIST-HEADER (LENGTH AREA)
  TAG::P_10
     (ALU L+R A2 A0 '2)
     (ALU L+R A3 (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) A2)
     (ALU L-R GARBAGE A3 (%REGISTER *ACTIVE-CONS-CACHE-FREE-LIMIT* 0 4))
     (TEST BR-NOT-GREATER-THAN)
     (BRANCH TAG::C_16)
  TAG::C_14
     (MOVE O0 A0 CH-TAIL-OPEN)
     (TAIL-CALL ALLOCATE-LIST-STORAGE-UNCACHED '2 (O1 A1))
  TAG::C_16
     (KDPB A4 '7 '3 %%PTR-DATA-TYPE)
     (MOVE MD A4)
     (MOVE VMA-START-WRITE (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3))
     (MOVE MD 'NIL)
     (ALU ALU-OR VMA-START-WRITE (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) '1)
     (ALU L+R A5 (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) '2)
     (MOVE (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) A3)
     (MOVE (%REGISTER *ACTIVE-CONS-CACHE-ALLOCATION-STATUS* 0 5) '0)
     (MOVE RETURN A5 CH-RETURN))






;allocate-list-storage-uncached
;        ((m-a) m-b)                          ;legislate boxed-size  total-size.
;     ;; invalidate cache before we might trap out (just a good idea, probably not necessary).
;
;
;        (call-xct-next decode-area-specification)
;       (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
;
;;;; this does the same thing.
;;      (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
;;      (call decode-area-specification)  ;;returns area spec in m-s
;
;        (call-not-equal-xct-next m-zero a-scavenge-work-while-consing scavenge-while-consing)
;       ((a-scavenge-work) a-scavenge-work-while-consing)
;        (call-xct-next get-active-region)    ;return allocation-status in q-r
;       ((vma-start-read) add m-s a-v-area-region-list)
;     ;; insert list-header if last object was structure, or if over threshold.
;        (call-greater-than q-r a-list-allocation-threshold insert-list-header)
;     ;; now m-t has address of object, m-b has total size, and m-a has boxed size.
;        (call-xct-next touch-pages-in-new-object)
;       ((a-active-cons-cache-allocation-status) add m-b a-active-cons-cache-allocation-status)
;        (jump update-structure-handles-for-object)


(defun allocate-list-storage-uncached (length area)
  "The basic, slow path, for list storage allocation"
  (unless (null *active-cons-cache-area*) (invalidate-active-cons-cache))
  (setq area (decode-area-specification area))
  (unless (zerop *scavange-work-while-consing*)
    (scavenge-while-consing *scavenge-work-while-consing*))
  ;; insert list-header if last object was structure, or if over threshold.
  (if (> (get-active-region length area) *list-allocation-threshold*)
      (insert-list-header))
  (setq *active-cons-cache-allocation-status*
        (+ length *active-cons-cache-allocation-status*))
  (touch-pages-in-new-object)
  (update-structure-handles-for-object))




;invalidate-active-cons-cache
;     (popj-equal a-active-cons-cache-region m-minus-one)
;        ((m-3) a-active-cons-cache-region)
;        ((vma) add m-3 a-v-region-free-pointer)
;        ((md) a-active-cons-cache-free-pointer)
;        ((md-start-write) output-selector-mask-25 sub md a-active-cons-cache-region-origin)
;        (check-page-write-unboxed)
;        ((a-active-cons-cache-area) m-minus-one)
;        ((a-active-cons-cache-region) m-minus-one)          ;Scavenger depends on this.
;        ((vma) add m-3 a-v-region-allocation-status)
;        (popj-after-next
;          (md-start-write) a-active-cons-cache-allocation-status)
;       (check-page-write-unboxed)

(defun invalidate-active-cons-cache ()
  (unless (null *active-cons-cache-region*)
    (let ((region *active-cons-cache-region*))
      (%loc-store (%loc-+ *v-region-free-pointer* region)
                  ;; this is (- loc loc) not (- loc fix) ???
                  (%loc-- *active-cons-cache-free-pointer* *active-cons-cache-region-origin*))
      (setq *active-cons-cache-area* nil)
      (setq *active-cons-cache-region* nil)
      (%loc-store (%loc-+ *v-region-allocation-status* region)
                  *active-cons-cache-allocation-status*))))


;insert-list-header
;    ;; Insert a list-header to indicate a transition from structure-storage to list-storage.
;        ((a-active-cons-cache-free-pointer) m+a+1 m-zero a-active-cons-cache-free-pointer)
;        ((a-active-cons-cache-allocation-status) m-zero)
;        ((m-t) add m-t (a-constant 1))
;        ((md) (a-constant (plus (byte-value q-cdr-code cdr-error)
;                             (byte-value q-data-type dtp-header)
;                             (byte-value q-header-type %header-type-list))))
;        (popj-after-next
;          (vma-start-write) sub m-t (a-constant 1))
;       (check-page-write-unboxed)

(defun insert-list-header ()
  ;; ucode updates free pointer first, safer?
  (%loc-store-car *active-cons-cache-free-pointer*
                  (%k-make-pointer #.dtp-header #.%header-type-list))
  ;; can't just store one word header now
  ;; since conses are always on even addresses
  (%loc-store-cdr *active-cons-cache-free-pointer*
                  nil)
  (setq *active-cons-cache-free-pointer* (%loc-+ *active-cons-cache-free-pointer 2)
        *active-cons-cache-allocation-status* 0)
  *active-cons-cache-free-pointer*)




(defun allocate-structure-storage (length area)
  (cond ((not (= area *active-cons-cache-area*))
         (allocate-structure-storage-uncached length area))
        (t
         (let ((temp (%loc-+ *active-cons-cache-free-pointer* length)))
           (if (%loc-> temp *active-cons-cache-free-limit*)
               (allocate-structure-storage-uncached length area)
             ;; Set allocation-status to > threshold
             ;; to force list-header insertion next list-structure cons.
             (progn (setq *active-cons-cache-allocation-status* (1+ *list-allocation-threshold*))
                    (let ((ptr *active-cons-cache-free-pointer*))   ;prog1
                      (setq *active-cons-cache-free-pointer* temp)
                      ptr)))))))


(DEFKFUN ALLOCATE-STRUCTURE-STORAGE (LENGTH AREA)
  TAG::P_10
     (ALU L-R GARBAGE A1 (%REGISTER *ACTIVE-CONS-CACHE-AREA* 0 0))
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_13)
  TAG::C_16
     (ALU L+R A2 (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) A0)
     (ALU L-R GARBAGE A2 (%REGISTER *ACTIVE-CONS-CACHE-FREE-LIMIT* 0 4))
     (TEST BR-NOT-GREATER-THAN)
     (BRANCH TAG::C_22)
  TAG::C_20
     (MOVE O0 A0 CH-TAIL-OPEN)
     (TAIL-CALL ALLOCATE-STRUCTURE-STORAGE-UNCACHED '2 (O1 A1))
  TAG::C_22
     (ALU L+R+1
          (%REGISTER *ACTIVE-CONS-CACHE-ALLOCATION-STATUS* 0 5)
          (%REGISTER *LIST-ALLOCATION-THRESHOLD* 0 6)
       '0)
     (MOVE A3 (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3))
     (MOVE (%REGISTER *ACTIVE-CONS-CACHE-FREE-POINTER* 0 3) A2)
     (MOVE RETURN A3 CH-RETURN)
  TAG::C_13
     (MOVE O0 A0 CH-TAIL-OPEN)
     (TAIL-CALL ALLOCATE-STRUCTURE-STORAGE-UNCACHED '2 (O1 A1)))






;allocate-structure-storage-uncached
;     ;; Invalidate cache before we might trap out (just a good idea, probably not necessary).

;;;; Can this hope to win?
;;;; Yes!  And it does what you would expect it to do.
;           (call-xct-next decode-area-specification)
;          (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)

;;;; This is what the above code equals.
;;  (call-not-equal m-minus-one a-active-cons-cache-area invalidate-active-cons-cache)
;;  (call decode-area-specification)

;        (call-not-equal-xct-next m-zero a-scavenge-work-while-consing scavenge-while-consing)
;       ((a-scavenge-work) a-scavenge-work-while-consing)
;        (call-xct-next get-active-region)
;       ((vma-start-read) add m-s a-v-area-region-list)
;     ;; Now M-T has address of object, M-B has total size, and M-A has boxed size.
;        (call-xct-next touch-pages-in-new-object)
;       ((a-active-cons-cache-allocation-status) dpb m-minus-one q-pointer a-zero)
;        (jump update-structure-handles-for-object)


;;;; Decode area specification in M-S.  Return boxed fixnum in M-S.
;decode-area-specification
;     (declare (args a-s) (values a-s))
;        (call-data-type-equal m-s (a-constant (byte-value q-data-type dtp-symbol)) decode-area-symbol)
;        (call-less-than m-s (a-constant (byte-value q-data-type dtp-fix)) trap)
;     (error-table argtyp area m-s nil)
;        (call-greater-than m-s (a-constant (plus (byte-value q-data-type dtp-fix) 377)) trap)
;     (error-table argtyp area m-s nil)
;        (popj)
;decode-area-symbol
;     (declare (args a-s) (values a-s))
;     ((vma-start-read) output-selector-mask-25 add m-s (a-constant 1))   ;Value cell.
;     (check-page-read)
;     (popj-after-next dispatch transport md)
;       ((m-s) q-typed-pointer md)

(defun decode-area-specification (area-spec)
  "Take an area-specification and return the area number as a fixnum"
  (if (symbolp area-spec)
      (setq area-spec (symbol-value area-spec)))
  ;; ucode did pointer compare and caught non fixnums
  (if (or (< area-spec 0)
          (> area-spec #.maximum-area-number))
      (error "~a is not a valid area number." area-spec)
  area-spec))




;;; Dispatch table for GET-ACTIVE-REGION, below.  Dispatches on region space type to find
;;; an active region appropriate to cons in.
(locality d-mem)
(start-dispatch 4 0)
d-verify-region-type
        (p-bit n-bit trap)                  ;0 FREE
        (n-bit get-active-region-loop)              ;1 OLD (try next region)
        (p-bit r-bit)                               ;2 NEW
        (p-bit n-bit trap)                  ;3
        (p-bit n-bit trap)                  ;4
        (p-bit n-bit trap)                  ;5
        (p-bit n-bit trap)                  ;6
        (p-bit n-bit trap)                  ;7
        (p-bit n-bit trap)                  ;10
        (p-bit r-bit)                               ;11 STATIC
        (p-bit n-bit trap)                  ;12 FIXED
        (p-bit r-bit)                               ;13 EXTRA-PDL
        (n-bit get-active-region-loop)              ;14 COPY (try next region)
        (n-bit get-active-region-loop)              ;15 MOBY-FIXED (try next region)
        (p-bit r-bit)                               ;16 MOBY-NEW  try this one.
(repeat 1 (p-bit n-bit trap))                 ;  code 15 up are MOBY.
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


;;; temporary, really turns into a dispatch instruction
;;; similiar to dispatch but takes addresses
(defmacro kdispatch (field value &body clauses)
  `(case (ldb ,field ,value)
     ,@(mapcar #'(lambda (clause)
                   `(,(car clause) (go ,(cadr clause))))
               clauses)))

;;; Subroutine of allocate-storage.  Call with area number (fixnum) in M-S, total number
;;; of words needed in M-B (untyped).  Finds a NEW, STATIC, or EXTRA-PDL region in area
;;; that has enough room to hold the new object, or allocates a new region for that area.
;;; Currently this fails if the object is larger than the region size of the area.
;;; Return region-allocation-status of region found in q-r.

(defun get-active-region (size area-number)
  (let ((region-num (%loc-read (%loc-+ *v-area-region-list* area-number))))   ;actually this could be an array
    ;; no transport, region list is never in oldspace (jrm)
    (tagbody
        loop
           ;; If at end of region list, allocate another region, or reset extra-pdl and retry.
           (if (boxed-sign-bit??? region-num)
               (setq region-num (allocate-active-region size area-number)))
           ;; Check region-space-type.  If not suitable, jump to get-active-region-loop.
           (kdispatch %%region-space-type (%loc-read (%loc-+ *v-region-bits* region-num))   ;could be an array
              (0 bad-region)                            ;free, trap
              (1 get-active-region-loop)                ;old, try next region
              (2 ok)                                    ;new
              (11 ok)                                   ;static
              (12 bad-region)                           ;fixed, trap
              (13 ok)                                   ;extra-pdl
              (14 get-active-region-loop)               ;copy, try next region
              (15 get-active-region-loop)               ;moby, try next region
              (16 ok)                                   ;moby-new
              (otherwise bad-region))
        bad-region
           (error "Cons in inappropriate region ~a" region-num)
        get-active-region-loop
           (setq region-num (%loc-read (%loc-+ *v-region-list-thread* region-num)))
           (go loop)
        ok ;;legitimate active region types (new, static, or extra-pdl).
           (setq *active-cons-cache-region-origin*  ;type?
                 (%loc-read (%loc-+ *v-region-origin* region-num)))
           (let ((region-end (%loc-+ *active-cons-cache-region-origin*
                              (%loc-read (%loc-+ *v-region-length* region-num))))
          (region-free (%loc-+ *active-cons-cache-region-origin*
                               (%loc-read (%loc-+ *v-region-free-pointer* region-num)))))
      (setq *active-cons-cache-free-pointer* (%loc-+ size region-free))
      ;; Compare region-free-pointer against region-length to see if there is enough room
      ;; for the object.  Note that we require that there be (1+ M-B) words, because we might
      ;; have to insert a list-header later on.
      (if (%loc-< region-end *active-cons-cache-free-pointer*)
          (go get-active-region-loop)))
    ;; Set up the free limit to word 0 on the page following the free-pointer.  However,
    ;; If the free-pointer is at word 0 of a page, set the limit to that page, so the
    ;; structure-handles get set up correctly next time around.
    (setq *active-cons-cache-free-limit*
          (dpb 0 %%ptr-page-index (%loc+ *active-cons-cache-free-limit* #.page-size)))
    (setq *active-cons-cache-region* region-num
          *active-cons-cache-area* area-number
          *active-cons-cache-allocation-status*
          (%loc-read (%loc-+ *v-region-allocation-status* region-num)))))




;allocate-active-region
;       (jump-equal m-s (a-constant extra-pdl-area-number) extra-pdl-overflow)
;        (call get-area-region-bits)                ;return in M-4
;     ;; Volatility for active regions comes from area-region-bits.
;        ((m-e) (lisp-byte %%region-volatility) m-4)
;       ((m-tem) (lisp-byte %%region-space-type) m-4)
;       (jump-greater-or-equal m-tem (a-constant (eval %region-space-moby-fixed))
;                              allocate-moby-region)

(defun allocate-active-region (size area)
  (if (= area #.ksi:extra-pdl-area)
      (extra-pdl-overflow)
    (let ((region-bits (get-area-region-bits area)))
      (ldb %%region-volatility region-bits)     ;what does it do with this?
      (allocate-region size area))))



;allocate-region
;       ((vma-start-read) add m-s a-v-area-region-size)
;       (check-page-read)
;       ((m-3) q-pointer md)                        ;Normal amount to allocate.
;       (jump-greater-than m-3 a-b rcons1)
;       ((m-3) add m-b (a-constant 1))              ;M-3 amount we want to allocate.
;rcons1     (call make-region)          ;Allocate a region of that size (to M-K).
;     ;; Update REGION-AREA-MAP to know about new region.
;        ((md) dpb m-s q-pointer a-zero)
;        ((vma-start-write) add m-k a-v-region-area-map)
;        (check-page-write)
;     ;; Insert new region at front of AREA-REGION-LIST.
;       ((vma-start-read) add m-s a-v-area-region-list)
;       (check-page-read)
;       ((m-3) md)                          ;This becomes the next region.
;       ((md-start-write) dpb m-k q-pointer a-3)
;       (check-page-write)
;       ((md) m-3)
;       (popj-after-next
;          (vma-start-write) add m-k a-v-region-list-thread)
;       (check-page-write)

(defun allocate-region (size area)
  (let ((region-size (%loc-read (%loc-+ *v-area-region-size* area))))
    (unless (> region-size size)
      (setq region-size (1+ size)))
    ;;Allocate a region of that size
    (let ((region (make-region region-size)))
      ;; Update REGION-AREA-MAP to know about new region.
      (%loc-store (%loc-+ *v-region-area-map* region)
                  area)
      ;; Insert new region at front of AREA-REGION-LIST.
      ;;vma doesn't need to be written twice...
      (let ((old-first-region (%loc-read (%loc-+ *v-area-region-list* area))))
        (%loc-store (%loc-+ *v-area-region-list* area)
                    region)
        (%loc-store (%loc-+ *v-region-list-thread* region)
                    old-first-region))
      region)))



;;; A convenient subroutine for %MAKE-STRUCTURE and %MAKE-ARRAY.
;;; Allocates and initializes an appropriate structure.
(defun allocate-block (area total-size boxed-size)
  (initialize-storage
    (allocate-structure-storage total-size area)
    total-size
    boxed-size
    nil))


(DEFKFUN ALLOCATE-BLOCK (AREA TOTAL-SIZE BOXED-SIZE)
  TAG::P_7
     (TAIL-OPEN)
     (MOVE O0 A1 CH-OPEN)
     (KCALL ALLOCATE-STRUCTURE-STORAGE '2 O0 (O1 A0))
  TAG::C_10
     (MOVE O1 A1)
     (MOVE O2 A2)
     (TAIL-CALL INITIALIZE-STORAGE '4 (O3 'NIL)))


;;;; Subroutine of make-structure for initializing both boxed and unboxed storage.
;;;; Given an address in M-T, the number of boxed words (untagged) in M-A, and the
;;;; total number of words (untagged) in M-B, initialize all the boxed qs to the value
;;;; popped off the stack, with cdr codes of cdr-next (last one gets cdr-nil), and all
;;;; the unboxed words to zero.

;initialize-storage
;     ((md) ldb pdl-pop q-all-but-cdr-code (a-constant (byte-value q-cdr-code cdr-next)))
;     ((m-3) setm m-a)                    ;Number of boxed words to initialize.
;        ((m-4) sub m-b a-3)                  ;Total number of words.
;     ((vma) sub m-t (a-constant 1))
;     (jump-less-or-equal m-3 (a-constant 1) initialize-boxed-storage-1)
;initialize-boxed-storage-0
;     ((vma-start-write) add vma (a-constant 1))
;     (check-page-write)
;        (gc-write-test)                                     ;850726
;     (jump-greater-than-xct-next m-3 (a-constant 2) initialize-boxed-storage-0)
;       ((m-3) sub m-3 (a-constant 1))
;initialize-boxed-storage-1
;     ((md) q-all-but-cdr-code md (a-constant (byte-value q-cdr-code cdr-nil)))
;     ((vma-start-write) add vma (a-constant 1))
;     (check-page-write)
;        (gc-write-test)                                     ;850726
;initialize-unboxed-storage
;        ((md) setz)
;initialize-unboxed-storage-0
;        (popj-less-or-equal m-4 a-zero)
;        ((vma-start-write) add vma (a-constant 1))
;        (check-page-write-unboxed)
;        (jump-xct-next initialize-unboxed-storage-0)
;       ((m-4) sub m-4 (a-constant 1))




;;; Subroutine of make-structure for initializing both boxed and unboxed storage.
;;; Given a block, the number of boxed words, and the
;;; total number of words, initialize all the boxed qs to the value, (&cdr codes!?)
;;; and all the unboxed words to zero.
(defun initialize-storage (block total-words boxed-words value)
  (let ((unboxed-words (- total-words boxed-words)))
    (tagbody
        (setq K:VMA (%loc-1 block))
        (setq K:MD value)
     initialize-boxed-storage
        (%vma-write-next)
        (decf boxed-words)
        (if (> boxed-words 0)
            (go initialize-boxed-storage))
  ;; initialize-unboxed-storage
        (setq K:MD 0)
     initialize-unboxed-storage
        (if (<= unboxed-words 0)
            (return-from initialize-storage block))
        (%vma-write-next)                       ;check write unboxed
        (decf unboxed-words)
        (go initialize-unboxed-storage))))


(DEFKFUN INITIALIZE-STORAGE (BLOCK TOTAL-WORDS BOXED-WORDS VALUE)
  TAG::P_15
     (ALU L-R A4 A1 A2)
     (ALU L+R-1 (%REGISTER VMA NIL NIL) A0 '0)
     (MOVE (%REGISTER MD NIL NIL) A3)
  TAG::INITIALIZE-BOXED-STORAGE_47
     (ALU L+R+1 VMA-START-WRITE VMA '0)
     (ALU L-R A2 A2 '1)
     (ALU L-R GARBAGE A2 '0)
     (TEST BR-NOT-GREATER-THAN)
     (BRANCH TAG::C_59)
  TAG::C_56
     (JUMP TAG::INITIALIZE-BOXED-STORAGE_47)
  TAG::C_59
     (MOVE (%REGISTER MD NIL NIL) '0)
  TAG::INITIALIZE-UNBOXED-STORAGE_23
     (ALU L-R GARBAGE A4 '0)
     (TEST BR-NOT-LESS-THAN-OR-EQUAL)
     (BRANCH TAG::C_29)
  TAG::C_26
     (MOVE RETURN A0 CH-RETURN)
  TAG::C_29
     (ALU L+R+1 VMA-START-WRITE VMA '0)
     (ALU L-R A4 A4 '1)
     (JUMP TAG::INITIALIZE-UNBOXED-STORAGE_23))









;List-of-nils
;        ((pdl-push) a-v-nil)
;list-of-things
;        ((m-a) m-b)                                 ;Boxed-size  Total-size.
;        (call-return allocate-list-storage initialize-storage)





;;;; (%make-structure pointer-data-type header-data-type header second-word area total boxed)

;        (misc-inst-entry %make-structure)
;x-make-structure
;        (call allocate-block)
;        ((vma) add m-t (a-constant 1))
;        ((md-start-write) c-pdl-buffer-pointer-pop)
;        (check-page-write)
;     (gc-write-test-volatility)  ;OK since data is boxed.
;        ((m-tem1) c-pdl-buffer-pointer-pop)
;        ((md) dpb c-pdl-buffer-pointer-pop q-all-but-pointer a-tem1)
;        ((vma-start-write m-t) dpb c-pdl-buffer-pointer-pop q-all-but-pointer a-t)
;        (check-page-write)
;     (gc-write-test-volatility)  ;OK since data is boxed.
;        (popj)


(defun %make-structure (pointer-data-type header-data-type
                        header second-word
                        area total boxed)
  (let ((block (allocate-block area total boxed)))
    (%loc-store (%loc+1 block) second-word)
    (%loc-store (%k-make-pointer pointer-data-type block)
                (%k-make-pointer header-data-type header))))



;;;; (%make-array header-word index-length leader-length area total-size boxed-size)

;        (misc-inst-entry %make-array)
;x-make-array
;        (call allocate-block)
;        ((vma m-t) q-pointer m-t (a-constant (byte-value q-data-type dtp-array-pointer)))
;        ((m-e) output-selector-mask-25 add m-t a-b)    ;Last storage location.
;        ((m-c) q-pointer pdl-pop)            ;Leader length.
;        ((m-b) q-pointer pdl-pop)            ;Index length.
;        (call-if-bit-set-xct-next (lisp-byte %%array-leader-bit) c-pdl-buffer-pointer
;         make-array-leader)
;       ((m-d) dpb pdl-pop q-pointer (a-constant (byte-value q-data-type dtp-array-header)))
;        ((md-start-write) m-d)                       ;Store header.
;        (check-page-write-no-sequence-break) ;storage conventions would not be consistant...
;        ((m-tem) ldb (lisp-byte %%array-type-field) m-d)
;        (call-equal m-tem (a-constant (eval (ldb %%array-type-field art-complex)))
;            initialize-complex-array)
;;;; ***** NEVER NEVER NEVER LEAVE ARRAY HEADERS IN ACCUMULATORS *******
;;;; It took me a week to find this bug! -JRM
;        (popj-if-bit-clear-xct-next (lisp-byte %%array-long-length-flag) m-d)
;       ((m-d) a-v-nil)

;        ((vma) add m-t (a-constant 1))
;        ((md-start-write) dpb m-b q-pointer (a-constant (byte-value q-data-type dtp-fix)))
;        (check-page-write)
;        (popj)


(defun %make-array (header-word index-length leader-length area total-size boxed-size)
  (let ((array (%k-make-pointer #.dtp-array-pointer
                                (allocate-block area total-size boxed-size))))
    (%loc-store array (%k-make-pointer #.dtp-array-header header-word))
    (if (bit-test #.%%array-leader-bit header-word)
        (make-array-leader))
;    (if (= (ldb #.%%array-type-field header-word)
;          #.(ldb %%array-type-field art-complex))
;       (initialize-complex-array))
    (when (bit-test #.%%array-long-length-flag header-word)
      (%loc-store (%loc+1 array) index-length))
    array))


(DEFKFUN %MAKE-ARRAY (HEADER-WORD INDEX-LENGTH LEADER-LENGTH AREA TOTAL-SIZE BOXED-SIZE)
  TAG::P_12
     (MOVE O0 A3 CH-OPEN)
     (MOVE O1 A4)
     (KCALL ALLOCATE-BLOCK '3 A6 (O2 A5))
  TAG::C_64
     (KDPB A7 '17 A6 %%PTR-DATA-TYPE)
     (KDPB A8 '18 A0 %%PTR-DATA-TYPE)
     (MOVE MD A8)
     (MOVE VMA-START-WRITE A7)
     (ALU ALU-AND A9 '1089 A0)
     (ALU L-R GARBAGE A9 '0)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_20)
  TAG::C_23
     (JUMP TAG::B_59)
  TAG::C_20
     (OPEN-CALL MAKE-ARRAY-LEADER '0 IGNORE NIL)
  TAG::C_118
  TAG::B_59
     (ALU ALU-AND A9 '705 A0)
     (ALU L-R GARBAGE A9 '0)
     (TEST BR-NOT-EQUAL)
     (BRANCH TAG::C_40)
  TAG::C_45
     (MOVE RETURN A7 CH-RETURN)
  TAG::C_40
     (ALU L+R+1 A10 A7 '0)
     (MOVE MD A1)
     (MOVE VMA-START-WRITE A10)
     (MOVE RETURN A7 CH-RETURN))




(defvar art-q-1d-header (%LOGDPB 1 system:%%ARRAY-NUMBER-DIMENSIONS
                                 (%LOGDPB art-q system:%%ARRAY-TYPE-FIELD 0)))





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


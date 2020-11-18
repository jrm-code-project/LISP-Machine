
;-*- Mode:LISP; Base:8; readtable: ZL -*-

; (c) Copyright 1985, Lisp Machine Incorporated.

(DEFCONST UC-LAMBDA-ARRAY '(

; New array caching scheme.  KHS 2/6/85

;There are now 5 M/A registers associated with the array decoding process.

;    M-ARRAY-POINTER  -- A typed pointer to an array or stack group.
;    M-ARRAY-HEADER   -- The untyped word pointed to by M-ARRAY-POINTER.
;    M-ARRAY-ORIGIN   -- The data origin of the array in M-ARRAY-POINTER, untyped.
;    M-ARRAY-LENGTH   -- The total index length of the array, untyped.
;    M-ARRAY-RANK     -- The dimensionality of the array (general case only), untyped.

;The idea is, since these registers are used only by the array routines, once an array
;is decoded and information about it loaded into these registers, the decoding process
;can be eliminated for subsequent array operations on the same array.  This potentially
;doubles (or so) the speed of array operations.  Array cache hits are determined by
;comparison with M-ARRAY-POINTER.  The CDR-CODE bits provide explicit control over
;the cache, as explained below.

;There are two basic routines for decoding arrays, a general case applicable to all
;arrays, and a special, fast one-dimensional case.  These routines both setup the
;above registers (except the 1d case doesn't bother to set up M-ARRAY-RANK), but they
;may not share the cache, ie: just because you decoded an array with the 1d routine
;doesn't mean you can use the array registers to do general references, and vice
;versa.  This is primarily to ensure correct error detection, but there are some
;other subtleties involved.

;The CDR-CODE bits of M-ARRAY-POINTER are used to indicate whether the array cache
;is currently invalid, valid for 1d arrays, or valid for the general case.  The
;only critical assignment is that the 1d case be CDR-NEXT, since this allows instant
;comparison with objects coming off the stack.  There are assembler idioms called
;VALIDATE-ARRAY-CACHE (1d), VALIDATE-GENERAL-ARRAY-CACHE, and INVALIDATE-ARRAY-CACHE,
;that load the right bits in.

;Other than the proviso that the cache not be shared between the two cases, the other
;rule governing the cache is that it must be invalidated if an array is decoded in
;a non-standard manner, or if the array registers are modified.  Thus AR-1-FORCE must
;invalidate the cache, and displaced/indirect arrays may not be cached (the routines
;for decoding them take care of this).

;A typical decoding of a 1d array is

;     (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
;    ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)

;the comparison determines the cache hit (which will miss if the cdr-code is anything
;but cdr-next).  M-ARRAY-POINTER is loaded to validate the cache, which is superfluous
;if it's already valid;  this was a design decision to trade time from the cache-hit case
;to the cache-miss case.  The subroutine will trap if the array is not 1-dimensional.

;A typical decoding of a general array is

;     (dispatch-xct-next dispatch-write-vma q-data-type c-pdl-buffer-pointer
;        array-header-setup-dispatch)
;    ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)

;note that the VMA must be written, and that M-A must get a copy of the pointer with
;the general-case cdr-codes if you want to be able to take a cache hit.  (Actually M-A
;should always be loaded with something, even if it's just garbage, to prevent false
;hits.)  This subroutine doesn't care about dimensionality, it returns the rank in
;M-ARRAY-RANK for your inspection.

;You have to

;     (call-if-bit-set-xct-next (lisp-byte %%array-displaced-bit) m-array-header
;           decode-displaced-array)

;after the decoding if you need to process displaced arrays, just as before.

;Trapping out of the array routines is a bit wierd, since the array registers are not
;saved in stack groups.  Instead of going to TRAP, go to ARRAY-TRAP, which stores
;the array registers in their associated (historically) accumulators, A,B,E,S,D.
;Restarts should arrange to first go through RESTORE-ARRAY-REGISTERS, which reverses
;the process.

;-----------
(begin-pagable-ucode)

;;; Here if combined subscript is larger than M-ARRAY-LENGTH.

;;; (could this be dead code?)
array-subscript-error
        (call array-trap)
     (error-table subscript-oob m-q m-array-length (nil restore-array-registers)
                  m-a)    ;Not m-array-pointer
     ;; Microstack has address of array reference/store routine aborted out of.
     ;; Recheck subscript and continue.  Actually this should probably allow one
     ;; to specify a completely new array.
        (popj-after-next)
        (array-subscript-range-check)
(end-pagable-ucode)

;;; Actual one-dimensional array access instructions.

xar-1 (misc-inst-entry ar-1)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)
    (error-table arg-popped 1 m-q)
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
    (error-table calls-sub ar-1)
    (error-table arg-popped 0 m-array-pointer)
xar-1-x
        (dispatch-xct-next (lisp-byte %%array-type-field) m-array-header
            array-type-ref-dispatch-jump)
       (array-subscript-range-check)

xcommon-lisp-ar-1 (misc-inst-entry common-lisp-ar-1)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
xcommon-lisp-ar-1-x
        (dispatch-xct-next (lisp-byte %%array-type-field) m-array-header
            common-lisp-array-type-ref-dispatch-jump)
       (array-subscript-range-check)

xset-ar-1 (misc-inst-entry set-ar-1)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
xt-set-ar1                                      ;MC-linkage. (** obsolete? **)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
xset-ar-1-x
        (dispatch-xct-next (lisp-byte %%array-type-field) m-array-header
            array-type-store-dispatch)
       (array-subscript-range-check)

xas-1 (misc-inst-entry as-1)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
xas-1-x
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (dispatch-xct-next (lisp-byte %%array-type-field) m-array-header
            array-type-store-dispatch)
       (array-subscript-range-check)

xap-1 (misc-inst-entry ap-1)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
xap-1-x
        (dispatch (lisp-byte %%array-type-field) m-array-header skip-if-numeric-array)
        (jump xap-1-a)
        (call array-trap)
    (error-table number-array-not-allowed m-array-pointer (nil restore-array-registers))
        (jump xap-1-x)
xap-1-a
        (array-subscript-range-check)
        (popj-after-next
          (m-t) add m-array-origin a-q)
       ((m-t) dpb m-t q-pointer (a-constant (byte-value q-data-type dtp-locative)))

(begin-comment) Zwei Lossage (end-comment)
(begin-pagable-ucode)

;;; This is the remains of an immediate-array-referencing tweak that was used
;;; before 5-bit opcodes.  System 102 still contains a few references to these
;;; instructions because of a compiler bug.  They should be flushed after the next
;;; recompilation, and the misc instructions reclaimed.

obsolete-arefi (macro-ir-decode (misc * (0 1)))
        (dispatch-xct-next (byte-field 3 4) macro-ir d-obsolete-arefi)
       (no-op)

(locality d-mem)
(start-dispatch 3 0)
d-obsolete-arefi
        (obsolete-arefi-array)
        (obsolete-arefi-array-leader)
        (obsolete-arefi-instance)
        (illop)
        (obsolete-aseti-array)
        (obsolete-aseti-array-leader)
        (obsolete-aseti-instance)
        (illop)
(end-dispatch)
(locality i-mem)

obsolete-arefi-array (macro-ir-misc-decode (0 17))
        (jump-xct-next arefi-array-kernel)
       ((m-q) ldb (byte-field 4 0) macro-ir)

obsolete-arefi-array-leader (macro-ir-misc-decode (20 37))
        (jump-xct-next arefi-array-leader-kernel)
       ((m-q) ldb (byte-field 4 0) macro-ir)

obsolete-arefi-instance (macro-ir-misc-decode (40 57))
        ((m-q) ldb (byte-field 4 0) macro-ir)
        (jump-xct-next arefi-instance-kernel)
       ((m-q) m+a+1 m-q a-zero)

obsolete-aseti-array (macro-ir-misc-decode (100 117))
        ((m-q) ldb (byte-field 4 0) macro-ir)
        (call-return exchange-top-of-stack arefi-set-array-kernel)

obsolete-aseti-array-leader (macro-ir-misc-decode (120 137))
        ((m-q) ldb (byte-field 4 0) macro-ir)
        (call-return exchange-top-of-stack arefi-set-array-leader-kernel)

obsolete-aseti-instance (macro-ir-misc-decode (140 157))
        ((m-q) ldb (byte-field 4 0) macro-ir)
        ((m-q) m+a+1 m-q a-zero)
        (call-return exchange-top-of-stack arefi-set-instance-kernel)
                                                ;end of obsolete immediate-array code
(end-pagable-ucode)

exchange-top-of-stack
        ((m-3) c-pdl-buffer-pointer-pop)
        ((m-4) c-pdl-buffer-pointer-pop)
        (popj-after-next
          (c-pdl-buffer-pointer-push) m-3)
        ((c-pdl-buffer-pointer-push) m-4)

(begin-comment) Zwei Lossage (end-comment)

;;; This is the current immediate-array-referencing tweak.  Pass 2 optimizes
;;; constant-argument references to 1d arrays into these.

arefi-array (macro-ir-decode (arefi * arefi-array))
        ((m-q) macro-ir-displacement)
arefi-array-kernel
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header
            array-type-ref-dispatch)
       (array-subscript-range-check)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

arefi-set-array (macro-ir-decode (arefi * arefi-set-array))
        ((m-q) macro-ir-displacement)
arefi-set-array-kernel
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header
            array-type-store-dispatch-pushj)
       (array-subscript-range-check)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

arefi-array-leader (macro-ir-decode (arefi * arefi-array-leader))
        ((m-q) macro-ir-displacement)
arefi-array-leader-kernel
        (call find-array-leader)
        ((vma-start-read) vma)
        (check-page-read)
        (dispatch transport md)
        ((m-t) q-typed-pointer md)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

arefi-set-array-leader (macro-ir-decode (arefi * arefi-set-array-leader))
        ((m-q) macro-ir-displacement)
arefi-set-array-leader-kernel
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        (call find-array-leader)
        ((md-start-write) m-t)
        (check-page-write)
        (gc-write-test)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

arefi-instance (macro-ir-decode (arefi * arefi-instance))
        ((m-q) m+a+1 macro-ir-displacement a-zero)      ;arg is 0-origin but data is 1-origin
arefi-instance-kernel
     (error-table restart arefi-instance-restart-type)
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-instance)) trap)
     (error-table argtyp instance pp 0 arefi-instance-restart-type %instance-ref)
        ((vma-start-read) q-typed-pointer c-pdl-buffer-pointer-pop)
        (check-page-read)
        (dispatch transport-header md)
        ((m-t) vma)                                    ;Might have moved.
        ((vma-start-read) add md (a-constant (eval %instance-descriptor-size)))
        (check-page-read)
        ((m-2) q-pointer read-memory-data)             ;Size of instance.
     (error-table restart arefi-instance-restart-index)
        (call-greater-or-equal m-q a-2 trap)
     (error-table subscript-oob m-q m-2 arefi-instance-restart-index m-t)
        ((vma-start-read) add m-t a-q)
        (check-page-read)
        (dispatch transport md)
        ((m-t) q-typed-pointer md)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

arefi-set-instance (macro-ir-decode (arefi * arefi-set-instance))
        ((m-q) m+a+1 macro-ir-displacement a-zero)      ;arg is 0-origin but data is 1-origin
arefi-set-instance-kernel
        ((m-j) q-typed-pointer pdl-pop)
     (error-table restart arefi-set-instance-restart-type)
        (call-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-instance)) trap)
     (error-table argtyp instance pp 0 arefi-set-instance-restart-type %instance-set)
        ((vma-start-read) q-typed-pointer c-pdl-buffer-pointer-pop)
        (check-page-read)
        (dispatch transport-header md)
        ((m-t) vma)                                    ;Might have moved.
        ((vma-start-read) add md (a-constant (eval %instance-descriptor-size)))
        (check-page-read)
        ((m-2) q-pointer read-memory-data)             ;Size of instance.
     (error-table restart arefi-set-instance-restart-index)
        (call-greater-or-equal m-q a-2 trap)
     (error-table subscript-oob m-q m-2 arefi-set-instance-restart-index m-t)
        ((md) q-typed-pointer m-j)
        ((vma-start-write) add m-t a-q)
        (check-page-write)
        (gc-write-test)
        ((m-t) q-typed-pointer m-j)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

arefi-array-common-lisp (macro-ir-decode (arefi * arefi-array-common-lisp))
        ((m-q) macro-ir-displacement)
arefi-array-common-lisp-kernel
        (call-not-equal-xct-next c-pdl-buffer-pointer a-array-pointer decode-1d-array-uncached)
       ((m-array-pointer) validate-array-cache c-pdl-buffer-pointer-pop)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header
            common-lisp-array-type-ref-dispatch)
       (array-subscript-range-check)
        (dispatch macro-ir-dest qmdtbd)
       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

;arefi-svref (macro-ir-decode (arefi * arefi-svref))
;        ((m-q) macro-ir-displacement)
;        ((vma-start-read) m+a+1 c-pdl-buffer-pointer-pop a-q)
;        (check-page-read)
;       (dispatch transport read-memory-data)
;        ((m-t) q-typed-pointer read-memory-data)
;        (dispatch macro-ir-dest qmdtbd)
;       ((pdl-push) dpb m-t q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))

(begin-comment) Zwei Lossage (end-comment)

;;; Instructions that apply a single subscript to any array regardless of rank.

xar-1-force (misc-inst-entry ar-1-force)
        (call-return decode-1d-array-force xar-1-x)

xcommon-lisp-ar-1-force (misc-inst-entry common-lisp-ar-1-force)
        (call-return decode-1d-array-force xcommon-lisp-ar-1-x)

xas-1-force (misc-inst-entry as-1-force)
        (call-return decode-1d-array-force xas-1-x)

xset-ar-1-force (misc-inst-entry set-ar-1-force)
        ((m-t) q-typed-pointer pdl-pop)
        (call-return decode-1d-array-force xset-ar-1-x)

xap-1-force (misc-inst-entry ap-1-force)
        (call-return decode-1d-array-force xap-1-x)

;;; Decode an array and subscript, treating it as one-dimensional regardless of rank.

decode-1d-array-force
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-q) q-pointer c-pdl-buffer-pointer-pop)
        ((m-array-pointer) invalidate-array-cache c-pdl-buffer-pointer-pop)
     (error-table restart decode-1d-array-force)
decode-1d-array-force-1
        (call-data-type-not-equal m-array-pointer
                                  (a-constant (byte-value q-data-type dtp-array-pointer))
            array-trap)
     (error-table argtyp array m-array-pointer 0 (decode-1d-array-force restore-array-registers))
        ((vma-start-read) q-pointer m-array-pointer)
        (check-page-read)
        (dispatch transport-header md)
        ((m-array-rank) (lisp-byte %%array-number-dimensions) md)
        ((m-array-origin) output-selector-mask-25 add vma a-array-rank)
     ;; Mung M-ARRAY-HEADER to say "one dimensional" rather than the array's actual rank.
        ((md) andca md (a-constant (byte-mask %%array-number-dimensions)))
        (jump-xct-next decode-1d-array-force-entry)
       ((m-array-header) ior md (a-constant (byte-value %%array-number-dimensions 1)))

(begin-comment) Zwei Lossage (end-comment)

;;; Accessing two-dimensional arrays with first subscript varying fastest.

xas-2-reverse (misc-inst-entry as-2-reverse)
        (call-return decode-2d-array-reverse xas-1-x)

xar-2-reverse (misc-inst-entry ar-2-reverse)
        (call-return decode-2d-array-reverse xcommon-lisp-ar-1-x)

;;; Normal two-dimensional array access.

xset-ar-2 (misc-inst-entry set-ar-2)
        ((m-t) q-typed-pointer pdl-pop)
xt-set-ar2                                      ;MC-linkage (***obsolete?***)
        (call-return decode-2d-array xset-ar-1-x)

xas-2 (misc-inst-entry as-2)
        (call-return decode-2d-array xas-1-x)

xap-2 (misc-inst-entry ap-2)
        (call-return decode-2d-array xap-1-x)

xar-2 (misc-inst-entry ar-2)
xar2                                            ;MC-linkage (***obsolete?***)
        ((micro-stack-data-push) (a-constant (i-mem-loc xcommon-lisp-ar-1-x)))
     ;; Falls through...

;;; Decode a two-dimensional array and two subscripts, popping them and checking all errors.
;;; Unlike the 1d version of this routine, the caller does not need to pop the subscripts.

decode-2d-array
     ;; Decode with last subscript varying fastest.
        ((m-q) q-typed-pointer pdl-pop)
        ((m-j) q-typed-pointer pdl-pop)
decode-2d-array-common
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
     (error-table restart decode-2d-array-restart)
decode-2d-array-restart
        (call-not-equal m-array-rank (a-constant 2) array-trap)
     (error-table array-number-dimensions m-d 2 m-array-pointer
                  (decode-2d-array-restart gahdr restore-array-registers))
        (array-trap-unless-fixnum m-q
                                  :argument 1
                                  :restart (decode-2d-array-restart restore-array-registers))
        (array-trap-unless-fixnum m-j
                                  :argument 2
                                  :restart (decode-2d-array-restart restore-array-registers))
        ((m-1) q-pointer m-j)
     ;; Fetch first dimension length.
        ((vma-start-read) sub m-array-origin (a-constant 1))
        (check-page-read)                       ;No transport needed, just touched header.
     ;; Multiply the first subscript by the array first dimension length.
        (call-xct-next mpy)
     ;; First dimension, save in M-D for BITBLT.
       ((q-r m-d) md)
        (call-if-bit-set-xct-next (lisp-byte %%array-displaced-bit) m-array-header
            decode-displaced-array)
     ;; Add the other subscript.
       ((m-q) output-selector-mask-25 add q-r a-q)
        (popj-after-next
          (m-q) q-pointer m-q)
       (no-op)

(begin-pagable-ucode)

decode-2d-array-reverse
     ;; Decode with first subscript varying fastest.
        ((m-j) q-typed-pointer pdl-pop)
        (jump-xct-next decode-2d-array-common)
       ((m-q) q-typed-pointer pdl-pop)

(begin-comment) Zwei Lossage (end-comment)

xar-3 (misc-inst-entry ar-3)
        (call-return decode-3d-array xcommon-lisp-ar-1-x)

xas-3 (misc-inst-entry as-3)
        (call-return decode-3d-array xas-1-x)

xap-3 (misc-inst-entry ap-3)
        (call-return decode-3d-array xap-1-x)

xset-ar-3 (misc-inst-entry set-ar-3)
        ((m-t) q-typed-pointer pdl-pop)
xt-set-ar3                                      ;MC-linkage (***obsolete?***)
        (call-return decode-3d-array xset-ar-1-x)

decode-3d-array
        (call-xct-next decode-Nd-array)
       ((m-r) (a-constant 3))
        (popj-after-next
          (pdl-pointer) sub pdl-pointer (a-constant 4))  ;flush 3 dimensions plus array-pointer.
        (no-op)

(begin-comment) Zwei Lossage (end-comment)

;;; AREF is called with the CALL instruction; args are an array and subscripts.
;;; M-R is the number of args, or one more than the number of subscripts.
;;; When we return, the frame is thrown away, so we don't have to worry about stack levels.
;;; ASET and ALOC are similar; ASET's first arg is the value to store.

xaref (misc-inst-entry aref)
        (call-return decode-Nd-array-decrementing-m-r xar-1-x)

xcommon-lisp-aref (misc-inst-entry common-lisp-aref)
        (call-return decode-Nd-array-decrementing-m-r xcommon-lisp-ar-1-x)

xset-aref (misc-inst-entry set-aref)
        ((m-t) q-typed-pointer c-pdl-buffer-pointer-pop)
        ((m-r) sub m-r (a-constant 2))
        (call-return decode-Nd-array xset-ar-1-x)

xaset (misc-inst-entry aset)
        (call-xct-next decode-nd-array)
       ((m-r) sub m-r (a-constant 2))   ;There are TWO args that aren't subscripts in ASET.
        ((pdl-index) add m-ap (a-constant 1))
        ((m-t) q-typed-pointer c-pdl-buffer-index)
        (dispatch-xct-next (lisp-byte %%array-type-field) m-array-header
            array-type-store-dispatch)
       (array-subscript-range-check)

xaloc (misc-inst-entry aloc)
        (call-return decode-Nd-array-decrementing-m-r xap-1-x)

;;; Decode an array and any number of subscripts, on the stack.  ** Does not pop them **
;;; Expects number of subscripts in M-R.  Returns array in M-ARRAY-POINTER (with a
;;; general array-cache-valid tag), M-ARRAY-ORIGIN, M-ARRAY-HEADER, M-ARRAY-LENGTH as
;;; usual, ultimate index in M-Q.

decode-Nd-array-decrementing-M-R
     ;; For convenience -- most of the above AREF instructions need this.
        ((m-r) sub m-r (a-constant 1))
     (error-table restart decode-nd-array)
decode-Nd-array
     ;; Find the array on the stack and get its dimension, data start, and length.
        ((pdl-index) sub pdl-pointer a-r)
#+exp   ((vma) c-pdl-buffer-index)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-index
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-index)
        (call-not-equal m-array-rank a-r array-trap)
     (error-table array-number-dimensions m-d m-r m-array-pointer
                  (decode-nd-array restore-array-registers))
;Enter here with GAHDR called, from QARYR.
decode-Nd-array-kernel
        (jump-equal m-array-rank a-zero decode-Nd-array-rank-zero)
     ;; Now multiply subscripts by dimensions, summing.  Initialize first.
        ((m-q q-r) a-zero)
     ;; Address of last multiplier (next-to-last dimension), plus 1.
        ((vma) m-array-origin)
        ((pdl-index) sub pdl-pointer a-r)
        ((pdl-index) add pdl-index (a-constant 1))
decode-Nd-array-multiply
     ;; PDL-INDEX points to the next subscript to use in the computation.
        ((m-j) q-typed-pointer c-pdl-buffer-index)
        (array-trap-unless-fixnum m-j :argument nil :restart (nil restore-array-registers))
        ((m-1) add q-r a-j)
        (jump-equal-xct-next m-r (a-constant 1) decode-Nd-array-last-subscript)
       ((m-1) q-pointer m-1)
        ((pdl-index) add pdl-index (a-constant 1))
        ((vma-start-read) sub vma (a-constant 1))
        (check-page-read)               ;No transport since just touched header.
     ;; Now multiply the next subscript by the array next dimension length.
        (call-xct-next mpy)
       ((q-r) md)
        (jump-xct-next decode-Nd-array-multiply)
       ((m-r) sub m-r (a-constant 1))

decode-Nd-array-rank-zero
        ((m-1) a-zero)
;;; Here after adding in the last subscript (sum in M-1).
;;; The ultimate index is now complete with no final multiplication.
decode-Nd-array-last-subscript
        (call-if-bit-set-xct-next (lisp-byte %%array-displaced-bit) m-array-header
            decode-displaced-array)
       ((m-q) m-1)
        (popj)

(begin-comment) Zwei Lossage (end-comment)

;;; Bletcherous array-invocation hack from yesteryear.

qaryr (error-table restart qaryr)
#+exp   ((vma) m-a)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type m-a
            array-header-setup-dispatch)
       ((m-a) invalidate-array-cache m-a)
        (jump-if-bit-set-xct-next (lisp-byte %%array-named-structure-flag) m-array-header
            qaryr-named)
       ((m-j) m-s)       ;Save previous frame location
;A named structure with its funcall as hash table bit set
;should not simply be indexed into.  Instead, do something like
;what is done for funcalling an instance:
; hash the first arg to find the function to call.
        (call-not-equal m-array-rank a-r array-trap)
    (error-table array-number-dimensions m-d m-r m-array-pointer (qaryr restore-array-registers))
        (call decode-Nd-array-kernel)
        ((a-qlaryl) dpb m-q q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (call-xct-next xar-1-x)
       ((a-qlaryh) q-typed-pointer m-array-pointer)
     ;; Full frame leave has been done, so just return.
        (jump fast-qmddr)
(end-pagable-ucode)

QARYR-NAMED
        ((M-S) M-J)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-LEADER-BIT) M-array-header CALL-NAMED-STRUCTURE)
        ((VMA-START-READ) SUB M-Array-pointer (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-SET (LISP-BYTE %%ARRAY-LEADER-FUNCALL-AS-HASH-TABLE) MD CALL-HASH-TABLE)
CALL-NAMED-STRUCTURE
;; Make one more arg -- a copy of the structure that was called.
        ((M-R) ADD M-R (A-CONSTANT 1))
        ((c-pdl-buffer-pointer-push) m-array-pointer)
;; Now figure out the function to actually call.
        ((ARG-CALL REF-SUPPORT-VECTOR) (I-ARG SVC-NAMED-STRUCTURE-INVOKE))
        (DISPATCH-XCT-NEXT qmrcl-dispatch MD)
       ((M-A) Q-TYPED-POINTER MD)

(begin-comment) Zwei Lossage (end-comment)
(begin-pagable-ucode)
;;; Miscellaneous array-information routines.

xaixl (misc-inst-entry array-length)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
     (error-table calls-sub array-length)
     (error-table arg-popped 0 m-a)
xaixl1  (popj-if-bit-clear-xct-next (lisp-byte %%array-displaced-bit) m-array-header)
       ((m-t) dpb m-array-length q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-read) add m-array-origin (a-constant 1))
        (check-page-read)                       ;No transport, just touched header.
xaixl2  (popj-after-next)
       ((m-t) dpb md q-pointer (a-constant (byte-value q-data-type dtp-fix)))

xaaixl (misc-inst-entry array-active-length)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
     (error-table calls-sub array-active-length)
     (error-table arg-popped nil)
        (jump-if-bit-clear (lisp-byte %%array-leader-bit) m-array-header xaixl1)
     ;; Get fill pointer from leader.
        ((vma-start-read) sub m-array-pointer (a-constant 2))
        (check-page-read)
        (popj-after-next
          (m-t) dpb md q-pointer (a-constant (byte-value q-data-type dtp-fix)))
     ;; If not fixnum, don't return garbage... goto XAIXL1 to return ARRAY-LENGTH instead.
       (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-fix)) xaixl1)

xarray-rank (misc-inst-entry array-rank)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
     (error-table calls-sub array-rank)
     (error-table arg-popped nil)
        (popj-xct-next)
       ((m-t) dpb m-array-rank q-pointer (a-constant (byte-value q-data-type dtp-fix)))

xarray-leader-length (misc-inst-entry array-leader-length)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
     (error-table calls-sub array-active-length)
     (error-table arg-popped nil)
        (jump-if-bit-clear (lisp-byte %%array-leader-bit) m-array-header xfalse)
        ((vma-start-read) sub m-array-pointer (a-constant 1))
        (check-page-read)
        (popj-xct-next)
       ((m-t) q-typed-pointer md)

xarray-has-fill-pointer-p (misc-inst-entry array-has-fill-pointer-p)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
     (error-table calls-sub array-has-fill-pointer-p)
     (error-table arg-popped nil)
        (jump-if-bit-clear (lisp-byte %%array-leader-bit) m-array-header xfalse)
        (jump-not-equal m-array-rank (a-constant 1) xfalse)
        ((vma-start-read) sub m-array-pointer (a-constant 2))   ;Get fill pointer from leader.
        (check-page-read)
        (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-fix)) xtrue)
        (jump xfalse)                                           ;Garbage doesn't count.

xsimple-array-p (misc-inst-entry simple-array-p)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                                  (a-constant (byte-value q-data-type dtp-array-pointer))
            xfalse)
        (call xarray-has-leader-p) ;>>(call xarray-has-fill-pointer-p)
        (jump-equal m-t a-v-true xfalse)
        (jump-if-bit-set (lisp-byte %%array-displaced-bit) m-array-header xfalse)
        (jump xtrue)

xsimple-vector-p (misc-inst-entry simple-vector-p)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                                  (a-constant (byte-value q-data-type dtp-array-pointer))
            xfalse)
        (call xarray-has-leader-p) ;>>(call xarray-has-fill-pointer-p)
        (jump-equal m-t a-v-true xfalse)
        ((m-1) (lisp-byte %%array-number-dimensions) m-array-header)
        (jump-not-equal m-1 (a-constant 1) xfalse)
        (dispatch (lisp-byte %%array-type-field) m-array-header skip-if-numeric-array)
        (jump-if-bit-clear (lisp-byte %%array-displaced-bit) m-array-header xtrue)
        (jump xfalse)

xsimple-string-p (misc-inst-entry simple-string-p)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                                  (a-constant (byte-value q-data-type dtp-array-pointer))
            xfalse)
        (call xarray-has-leader-p) ;>>(call xarray-has-fill-pointer-p)
        (jump-equal m-t a-v-true xfalse)
        ((m-1) (lisp-byte %%array-number-dimensions) m-array-header)
        (jump-not-equal m-1 (a-constant 1) xfalse)
        ((m-1) (lisp-byte %%array-type-field) m-array-header)
        (jump-if-bit-set (lisp-byte %%array-displaced-bit) m-array-header xfalse)
        (jump-equal m-1 (a-constant (eval (lsh art-string array-type-shift))) xtrue)
        ;>>??
        (jump-equal m-1 (a-constant (eval (lsh art-fat-string array-type-shift))) xtrue)
        (jump xfalse)

xsimple-bit-vector-p (misc-inst-entry simple-bit-vector-p)
        (jump-data-type-not-equal c-pdl-buffer-pointer
                                  (a-constant (byte-value q-data-type dtp-array-pointer))
            xfalse)
        (call xarray-has-leader-p) ;>>(call xarray-has-fill-pointer-p)
        (jump-equal m-t a-v-true xfalse)
        ((m-1) (lisp-byte %%array-number-dimensions) m-array-header)
        (jump-not-equal m-1 (a-constant 1) xfalse)
        ((m-1) (lisp-byte %%array-type-field) m-array-header)
        (jump-if-bit-set (lisp-byte %%array-displaced-bit) m-array-header xfalse)
        (jump-equal m-1 (a-constant (eval (lsh art-1b array-type-shift))) xtrue)
        (jump xfalse)

xvectorp (misc-inst-entry vectorp)
        ((m-array-pointer) invalidate-array-cache c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal m-array-pointer
                (a-constant (byte-value q-data-type dtp-array-pointer)) xfalse)
        ((vma-start-read) m-array-pointer)
        (check-page-read)
        (dispatch transport-header read-memory-data)
        ((m-tem) (lisp-byte %%array-number-dimensions) read-memory-data)
        (jump-not-equal m-tem (a-constant 1) xfalse)
        (jump xtrue)

xstringp   (misc-inst-entry stringp)
     ;; A string is defined to be a one-d array of type ART-STRING or ART-FAT-STRING.
xstrnp  ((m-array-pointer) invalidate-array-cache c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal m-array-pointer
                (a-constant (byte-value q-data-type dtp-array-pointer)) xfalse)
        ((vma-start-read) m-array-pointer)
        (check-page-read)
        (dispatch transport-header read-memory-data)
        ((m-tem) (lisp-byte %%array-number-dimensions) read-memory-data)
        (jump-not-equal m-tem (a-constant 1) xfalse)
        ((m-tem) (lisp-byte %%array-type-field) read-memory-data)
        (jump-equal m-tem (a-constant (eval (lsh art-string array-type-shift))) xtrue)
        (jump-equal m-tem (a-constant (eval (lsh art-fat-string array-type-shift))) xtrue)
        (jump xfalse)

xbit-vector-p   (misc-inst-entry bit-vector-p)
        ((m-array-pointer) invalidate-array-cache c-pdl-buffer-pointer-pop)
        (jump-data-type-not-equal m-array-pointer
                (a-constant (byte-value q-data-type dtp-array-pointer)) xfalse)
        ((vma-start-read) m-array-pointer)
        (check-page-read)
        (dispatch transport-header read-memory-data)
        ((m-tem) (lisp-byte %%array-number-dimensions) read-memory-data)
        (jump-not-equal m-tem (a-constant 1) xfalse)
        ((m-tem) (lisp-byte %%array-type-field) read-memory-data)
        (jump-equal m-tem (a-constant (eval (lsh art-1b array-type-shift))) xtrue)
        (jump xfalse)

    (ERROR-TABLE RESTART XARRAY-DIMENSION)
xarray-dimension (misc-inst-entry array-dimension)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        ((m-c) q-pointer c-pdl-buffer-pointer-pop)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
    (ERROR-TABLE RESTART XARRAY-DIMENSION-1)
xarray-dimension-1
        (call-less-than m-c a-zero array-trap)
     (error-table bad-array-dimension-number m-array-pointer m-c
                  (xarray-dimension-1 restore-array-registers))
        (call-greater-or-equal m-c a-array-rank array-trap)
     (error-table bad-array-dimension-number m-array-pointer m-c
                  (xarray-dimension-1 restore-array-registers))
;; M-C has dimension number (untyped);
;; return dimension length in M-T (typed) and m-1 (untyped)
;; Array should be already decoded and M-C valid.
;; clobbers M-2, M-3, Q-R, M-TEM
array-dimension
        (jump-equal m-c a-zero xarray-implicit-dimension)
        ((vma-start-read) sub m-array-origin a-c)
        (check-page-read)
        (popj-after-next (m-1) q-pointer md)
       ((m-t) md)

xarray-implicit-dimension
        (call array-implicit-dimension)
        (popj-after-next (m-1) q-r)
       ((m-t) dpb m-1 q-pointer (a-constant (byte-value q-data-type dtp-fix)))

array-implicit-dimension
        (call xaixl1)
        ((m-t) q-pointer m-t)
        (jump-equal m-array-rank (a-constant 1) array-implicit-dimension-rank-1)
        ((vma-start-read) sub m-array-origin (a-constant 1))
        (check-page-read)
        ((m-2) q-pointer md)
        ((m-3) (a-constant 2))
array-implicit-dimension-loop
        (jump-equal m-3 a-array-rank array-implicit-dimension-divide)
        ((vma-start-read) sub vma (a-constant 1))
        (check-page-read)
        ((q-r) m-2)
        ((m-1) q-pointer md)
        (call mpy)
        ((m-2) q-r)
        (jump-xct-next array-implicit-dimension-loop)
       ((m-3) add m-3 (a-constant 1))

array-implicit-dimension-rank-1
        (popj-after-next (m-1) q-pointer m-t)
       ((q-r) m-1)

array-implicit-dimension-divide
        (popj-after-next (m-1 q-r) m-t)
       (call-not-equal m-1 a-zero div)
(end-pagable-ucode)

(begin-comment) Zwei Lossage (end-comment)

store-array-registers-in-accumulators
        ((m-a) m-array-pointer)
        ((m-b) q-pointer m-array-header) ;Don't put headers in accumulators!
        ((m-d) m-array-rank)
        (popj-after-next
          (m-e) m-array-origin)
        ((m-s) m-array-length)

    (error-table restart restore-array-registers)
restore-array-registers-from-accumulators
     ;; Array cache always invalid after a TRAP.
        ((m-array-pointer) invalidate-array-cache m-a)
        ((m-array-header) q-pointer m-b)
        ((m-array-rank) q-pointer m-d)
        (popj-after-next
          (m-array-origin) q-pointer m-e)
        ((m-array-length) q-pointer m-s)

;;; Subroutines for protecting registers during complex array store/reference routines.

save-array-accumulators
        ((pdl-push) m-b)
        ((pdl-push) m-e)
        ((pdl-push) m-i)
        ((pdl-push) m-k)
        (popj-after-next
          (pdl-push) m-s)
       ((pdl-push) dpb m-q q-pointer (a-constant (byte-value q-data-type dtp-fix)))

restore-array-accumulators
        ((m-q) ldb q-pointer pdl-pop)
        ((m-s) pdl-pop)
        ((m-k) pdl-pop)
        ((m-i) pdl-pop)
        (popj-after-next
          (m-e) pdl-pop)
       ((m-b) pdl-pop)

;;; Array reference routines.

QB1RY   ((m-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 5) 5) M-Q)    ;BIT ARRAY
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (CHECK-PAGE-READ)
        ((m-TEM2) (BYTE-FIELD 5 0) M-Q)
        ((m-j) sub (m-constant 40) a-tem2)
        ((oa-reg-low) dpb m-j (a-constant (oa-low-context (byte-inst (byte-field 1 0)))) oal-mrot)
        (popj-after-next
          byte-inst (m-t) md)
        ((m-t) ior (a-constant (byte-value q-data-type dtp-fix)) m-t)

QB2RY   ((M-J) (BYTE-FIELD 4 0) M-Q)            ;2 BIT ARRAY
        ((m-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 4) 4) M-Q)
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (CHECK-PAGE-READ)
        ((m-TEM2) DPB M-J A-ZERO (BYTE-FIELD 4 1))      ;LSH M-J 1
        ((m-j) sub (m-constant 40) a-tem2)
        ((oa-reg-low) dpb m-j (a-constant (oa-low-context (byte-inst (byte-field 2 0)))) oal-mrot)
        (popj-after-next
          byte-inst (m-t) md)
        ((m-t) ior (a-constant (byte-value q-data-type dtp-fix)) m-t)

QB4RY   ((M-J) (BYTE-FIELD 3 0) M-Q)            ;4 BIT ARRAY
        ((m-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 3) 3) M-Q)
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (CHECK-PAGE-READ)
        ((M-TEM2) DPB M-J A-ZERO (BYTE-FIELD 3 2))      ;LSH M-J 2
        ((m-j) sub (m-constant 40) a-tem2)
        ((oa-reg-low) dpb m-j (a-constant (oa-low-context (byte-inst (byte-field 4 0)))) oal-mrot)
        (popj-after-next
          byte-inst (m-t) md)
        ((m-t) ior (a-constant (byte-value q-data-type dtp-fix)) m-t)

;; 8-bit bytes (including strings)
QBARY   ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 2) 2) M-Q)
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (DISPATCH-XCT-NEXT (BYTE-FIELD 2 0) M-Q D-QBARY)
       (CHECK-PAGE-READ)

(LOCALITY D-MEM)
(START-DISPATCH 2 0)
D-QBARY (QBARY-0)
        (QBARY-1)
        (QBARY-2)
        (QBARY-3)
(END-DISPATCH)
(LOCALITY I-MEM)

QBARY-0 (POPJ-XCT-NEXT)
       ((M-T) DPB MD (BYTE-FIELD 10 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QBARY-1 (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 10 10) MD)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QBARY-2 (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 10 20) MD)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QBARY-3 (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 10 30) MD)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;Strings when accessed using COMMON-LISP-AR-1, etc.
QBARY-COMMON-LISP
        ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 2) 2) M-Q)
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (DISPATCH-XCT-NEXT (BYTE-FIELD 2 0) M-Q D-QBARY-COMMON-LISP)
       (CHECK-PAGE-READ)

(LOCALITY D-MEM)
(START-DISPATCH 2 0)
D-QBARY-COMMON-LISP
        (QBARY-0-COMMON-LISP)
        (QBARY-1-COMMON-LISP)
        (QBARY-2-COMMON-LISP)
        (QBARY-3-COMMON-LISP)
(END-DISPATCH)
(LOCALITY I-MEM)

QBARY-0-COMMON-LISP
        (POPJ-XCT-NEXT)
       ((M-T) DPB MD (BYTE-FIELD 10 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)))

QBARY-1-COMMON-LISP
        (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 10 10) MD)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)))

QBARY-2-COMMON-LISP
        (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 10 20) MD)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)))

QBARY-3-COMMON-LISP
        (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 10 30) MD)
       ((M-T) DPB M-T Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)))

QB16RY  ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 1) 1) M-Q)
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (JUMP-IF-BIT-SET-XCT-NEXT (BYTE-FIELD 1 0) M-Q QB16RY-ODD)
       (CHECK-PAGE-READ)
        (POPJ-XCT-NEXT)
       ((M-T) DPB MD (BYTE-FIELD 20 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QB16RY-ODD
        (POPJ-AFTER-NEXT (M-T) (BYTE-FIELD 20 20) MD)
       ((M-T) DPB M-T (BYTE-FIELD 20 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QB32RY  ((VMA-START-READ) ADD A-Q M-array-origin)  ;32 BIT ARRAY (really 32 data bits now)
        (CHECK-PAGE-READ)
        (jump-xct-next return-m-1-unsigned)
       ((m-1) md)

QINARY  ((VMA-START-READ) ADD A-Q M-array-origin)  ;INUM ARRAY
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT NO-OP)
       ((M-T) DPB READ-MEMORY-DATA Q-POINTER
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QB16SRY ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 1) 1) M-Q)    ;HALFWORD FIXNUM ARRAY
        ((VMA-START-READ) ADD A-TEM1 M-array-origin)
        (CHECK-PAGE-READ)
        (JUMP-IF-BIT-CLEAR-XCT-NEXT (BYTE-FIELD 1 0) M-Q A-ZERO QB16SRY-1)
       ((M-T) (BYTE-FIELD 16. 0) READ-MEMORY-DATA
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-T) (BYTE-FIELD 16. 16.) READ-MEMORY-DATA
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
QB16SRY-1
        (POPJ-AFTER-NEXT POPJ-IF-BIT-CLEAR (BYTE-FIELD 1 15.) M-T)
       ((M-T) DPB M-T (BYTE-FIELD 16. 0)  ;If number is negative, extend sign.
              (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                (BYTE-VALUE BOXED-SIGN-BIT 1)
                                (BYTE-VALUE BOXED-NUM-EXCEPT-SIGN-BIT -1))))

QQARY   ((VMA-START-READ) ADD A-Q M-array-origin)               ;Q ARRAY
        (CHECK-PAGE-READ)
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT READ-MEMORY-DATA)
       ((M-T) Q-TYPED-POINTER READ-MEMORY-DATA)

QFARY   ((M-TEM) ADD M-Q A-Q)                   ;FLOAT
        ((VMA-START-READ) ADD M-array-origin A-TEM)
        (CHECK-PAGE-READ)
        (call save-array-accumulators)
        ((M-I) READ-MEMORY-DATA)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-1) READ-MEMORY-DATA)
        (call-return flopack restore-array-accumulators)

QFFARY  ((VMA-START-READ) ADD M-Q A-array-origin)               ;FPS-FLOAT
        (CHECK-PAGE-READ)
        (call save-array-accumulators)
        ((M-TEM) (BYTE-FIELD 16. 16.) READ-MEMORY-DATA)         ;Swap halves
        ((M-TEM) DPB READ-MEMORY-DATA (BYTE-FIELD 16. 16.) A-TEM)
        ((M-1) DPB M-TEM (BYTE-FIELD 23. 7) (A-CONSTANT 1_30.)) ;Positive fraction
        ((M-I) (BYTE-FIELD 8 23.) M-TEM)        ;Excess-200 exponent
        (CALL-EQUAL-XCT-NEXT M-I A-ZERO FLZERO) ;0.0 is a special case (M-TEM OK)
       ((M-I) ADD M-I (A-CONSTANT 1600))        ;Excess-2000 exponent
        (CALL-IF-BIT-SET (BYTE-FIELD 1 31.) M-TEM FNEG1) ;If negative, negate
        (call-return flopack restore-array-accumulators)

;ART-COMPLEX.  Two words per element.
QCARY   (call-xct-next save-array-accumulators)
       ((M-TEM) ADD M-Q A-Q)
;Read the imaginary part first, and push it.
        ((VMA-START-READ) M+A+1 M-array-origin A-TEM)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
        ((PDL-PUSH) Q-TYPED-POINTER MD)
;Read the real part and push it.
        ((VMA-START-READ) SUB VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT MD)
;Do we have a real, rational number here?
        (JUMP-NOT-EQUAL-XCT-NEXT PDL-TOP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))
                QCARY-NOT-REAL)                 ;Imag part not zero, must make complexnum
       ((M-T PDL-PUSH) MD)
        ((M-1) Q-DATA-TYPE MD)
        (JUMP-EQUAL M-1 (A-CONSTANT (EVAL DTP-FIX)) QCARY-REAL)  ;Real part fixnum, return it.
        (CALL XTFLTP)                           ;M-T gets FLOATP of M-T (the real part)
        (JUMP-EQUAL M-T A-V-NIL QCARY-REAL)     ;Real part rational => return it.
QCARY-NOT-REAL
        (call-return make-complex restore-array-accumulators)

;Here if the imaginary part was 0 and real part is rational.  Just return real part.
;Currently real part is on stack, and imag part is underneath it.
;Note we only checked for fixed point 0 because that is what was
;put in the imag part if a real number is stored in the array.
QCARY-REAL
        ((M-T) PDL-POP)
        (PDL-POP)
     ;; Fall through.

;Make a complex from the real part and imag part on the stack (in that order)
;and return it in M-T.
XCOMPLEX-CONS (MISC-INST-ENTRY %COMPLEX-CONS)
        (call exchange-top-of-stack)
     ;; Fall through.

;Make a complex from the imag part and real part on the stack (in that order)
;and return it in M-T.
MAKE-COMPLEX
;Now make a rational (!) with the imag part as denominator and real part as numerator.
        (CALL MAKE-RATIONAL)
;Change the rational into a complex by altering the header type.
        ((MD-START-WRITE)
         (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-HEADER)
                           (BYTE-VALUE HEADER-TYPE-FIELD %HEADER-TYPE-COMPLEX)
                           0)))
        (CHECK-PAGE-WRITE)
        (POPJ)

;ART-COMPLEX-FLOAT.  Four words per element -- two flonums.
;Read the two flonums one by one as for ART-FLOAT,
;then make a complex from them.
QCFARY  (CALL SAVE-ARRAY-ACCUMULATORS)
        (CALL-XCT-NEXT QCFARY1)
       ((M-Q) M+A+1 M-Q A-Q)
        ((M-Q) SUB M-Q (A-CONSTANT 1))
        (CALL-XCT-NEXT QCFARY1)
       ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-T)
        (CALL-RETURN MAKE-COMPLEX RESTORE-ARRAY-ACCUMULATORS)

QCFARY1 ((M-TEM) ADD M-Q A-Q)                   ;FLOAT
        ((VMA-START-READ) ADD M-ARRAY-ORIGIN A-TEM)
        (CHECK-PAGE-READ)
        ((M-I) READ-MEMORY-DATA)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-1) READ-MEMORY-DATA)
        (JUMP FLOPACK)

;ART-COMPLEX-FPS-FLOAT.  Two words per element -- two flonums in FPS format.
;Read the two flonums one by one as for ART-FPS-FLOAT,
;then make a complex from them.
QCFFARY (CALL save-array-accumulators)
        (CALL-XCT-NEXT QFFARY)
       ((M-Q) M+A+1 M-Q A-Q)
        ((M-Q) SUB M-Q (A-CONSTANT 1))
        (CALL-XCT-NEXT QFFARY)
       ((PDL-PUSH) M-T)
        ((PDL-PUSH) M-T)
        (call-return make-complex restore-array-accumulators)

(begin-comment) Zwei Lossage (end-comment)

;;; This is disgusting.

   (MISC-INST-ENTRY GET-LIST-POINTER-INTO-ARRAY)
XGLPA   ;((m-a) invalidate-array-cache a-qlaryh)
        ((m-a) m-minus-one)
        ((pdl-push) invalidate-array-cache a-qlaryh)
#+exp   ((vma) pdl-top)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type pdl-pop
            array-header-setup-dispatch)
       ((M-R) SETA (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST))
                     C-PDL-BUFFER-POINTER-POP)  ;IGNORE ARGUMENT
                                        ;GET LIST POINTER TO LAST ARRAY ELEMENT REF ED
   (ERROR-TABLE CALLS-SUB GET-LIST-POINTER-INTO-ARRAY)
        ((M-Q) DPB M-ZERO Q-ALL-BUT-POINTER A-QLARYL)           ;ENTRY NUMBER
XGLPA1  ((M-TEM) (LISP-BYTE %%ARRAY-TYPE-FIELD) M-array-header)
        (CALL-NOT-EQUAL M-TEM (A-CONSTANT (EVAL (LSH ART-Q-LIST ARRAY-TYPE-SHIFT))) array-trap)
   (ERROR-TABLE ARGTYP ART-Q-LIST-ARRAY M-A T NIL)
XGLOP1  (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-array-header
                decode-displaced-array)
        (CALL-GREATER-OR-EQUAL M-Q A-array-length array-trap)   ;INDEX OUT OF BOUNDS *****
   (ERROR-TABLE SUBSCRIPT-OOB M-Q M-S)
        (POPJ-AFTER-NEXT
         (M-TEM3) IOR A-R M-Q)
      ((M-T) ADD A-TEM3 M-array-origin)

     (MISC-INST-ENTRY G-L-P)            ;(G-L-P <ARRAY-POINTER-TO-ART-Q-LIST-ARRAY>)
XGLPAR
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
        ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
    (error-table calls-sub g-l-p)       ;IF FILL-POINTER 0, RETURN NIL
    (ERROR-TABLE ARG-POPPED 0 M-A)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-LEADER-BIT) m-array-header XGLPA2) ; NO LEADER
        ((VMA-START-READ) SUB M-Array-pointer (A-CONSTANT 2))   ;NO TRANSPORT needed
        (CHECK-PAGE-READ)
        ((M-TEM) Q-TYPED-POINTER READ-MEMORY-DATA)
        (JUMP-EQUAL M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) XFALSE)
XGLPA2  ((M-R) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LIST)))
        (JUMP-XCT-NEXT XGLPA1)          ;RETURN POINTER TO ELEMENT NUMBER 0
       ((M-Q) A-ZERO)

(begin-comment) Zwei Lossage (end-comment)

;Storing into arrays.

;Store routines for various types of arrays, reached via ARRAY-TYPE-STORE-DISPATCH.
;M-T has data to store, M-Q subscript, M-E etc. have GAHDR data.

;NOTE REFLECTING ABOUT 40 HACK NOT NECESSARY FOR DPB
QSBARY  ((M-J) DPB M-Q (BYTE-FIELD 2 3)         ;STORE IN BYTE ARRAY (8 BIT)
                (A-CONSTANT (logand 7777        ;ASSURE OK TO BE IN TYPED REGISTER
                                    (OA-LOW-CONTEXT (BYTE-INST (BYTE-FIELD 10 0))))))
        ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 2) 2) M-Q)    ;WORD OFFSET
QSANUM  ((VMA-START-READ) ADD A-TEM1 M-array-origin)    ;COMMON STORE ROUTINE FOR NUMERIC ARRAYS
        (CHECK-PAGE-READ)
        (array-trap-unless-fixnum m-t :argument 0)
        ((M-TEM1) READ-MEMORY-DATA)
        ((OA-REG-LOW) M-J)                      ;MODIFY FOLLOWING INST FOR WRITE
       ((WRITE-MEMORY-DATA-START-WRITE) DPB M-T A-TEM1)
        (CHECK-PAGE-WRITE-unboxed)
CPOPJ   (POPJ)

QS1RY   ((M-J) DPB M-Q (BYTE-FIELD 5 0)         ;STORE IN BIT ARRAY
                (A-CONSTANT (logand 7777
                                    (OA-LOW-CONTEXT (BYTE-INST (BYTE-FIELD 1 0))))))
        (JUMP-XCT-NEXT QSANUM)
       ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 5) 5) M-Q)             ;WORD OFFSET

QS2RY   ((M-J) DPB M-Q (BYTE-FIELD 4 1)         ;STORE IN 2-BIT ARRAY
                (A-CONSTANT (logand 7777
                                    (OA-LOW-CONTEXT (BYTE-INST (BYTE-FIELD 2 0))))))
        (JUMP-XCT-NEXT QSANUM)
       ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 4) 4) M-Q)             ;WORD OFFSET

QS4RY   ((M-J) DPB M-Q (BYTE-FIELD 3 2)         ;STORE IN 4-BIT ARRAY
                (A-CONSTANT (logand 7777
                                    (OA-LOW-CONTEXT (BYTE-INST (BYTE-FIELD 4 0))))))
        (JUMP-XCT-NEXT QSANUM)
       ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 3) 3) M-Q)             ;WORD OFFSET

QS16RY  ((M-J) DPB M-Q (BYTE-FIELD 1 4)         ;STORE IN 16-BIT ARRAY
                (A-CONSTANT (logand 7777
                                    (OA-LOW-CONTEXT (BYTE-INST (BYTE-FIELD 20 0))))))
        (JUMP-XCT-NEXT QSANUM)
       ((M-TEM1) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 1) 1) M-Q)             ;WORD OFFSET

QS32RY  ((pdl-push) m-i)        ;COPY-ARRAY-CONTENTS needs M-I saved.
        (call-xct-next get-32-bits)                     ;32 BIT ARRAY (really 32 data bits now.)
       ((pdl-push) m-t)
        ((m-i) pdl-pop)
        ((vma) add a-q m-array-origin)
        ((write-memory-data-start-write) m-1)
        (check-page-write-unboxed)
        (popj)

QSINARY ((pdl-push) m-i)        ;COPY-ARRAY-CONTENTS needs M-I saved.
        (call-xct-next get-32-bits)     ;INUM array.
       ((pdl-push) m-t)
        ((m-i) pdl-pop)
        ((VMA) ADD A-Q M-array-origin)
        ((WRITE-MEMORY-DATA-START-WRITE) q-pointer m-1)
        (CHECK-PAGE-WRITE-unboxed)
        (POPJ)

QSQARY  ((VMA) ADD A-Q M-array-origin)                  ;Q ARRAY
        ((WRITE-MEMORY-DATA-START-WRITE) M-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj)

QSLQRY  ((VMA-START-READ) ADD A-Q M-array-origin)               ;Q-LIST ARRAY
        (CHECK-PAGE-READ)                       ;NO TRANSPORT SINCE STORING AND JUST
        ((WRITE-MEMORY-DATA-START-WRITE)        ;TOUCHED HEADER AND DON'T ALLOW ONE-Q-FORWARD
                SELECTIVE-DEPOSIT READ-MEMORY-DATA Q-ALL-BUT-TYPED-POINTER A-T)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj)

QSFARY  ((M-J) M-I)                             ;Save M-I
        ((C-PDL-BUFFER-POINTER-PUSH) M-T)       ;copy to return.
        (CALL-XCT-NEXT GET-FLONUM)
       ((C-PDL-BUFFER-POINTER-PUSH) M-T)        ;Value being stored
        ((M-T) C-PDL-BUFFER-POINTER-POP)                ;for the value of ASET.
        ((M-TEM) ADD M-Q A-Q)
        ((WRITE-MEMORY-DATA) M-I)
        ((VMA-START-WRITE) ADD M-ARRAY-ORIGIN A-TEM)
        (CHECK-PAGE-WRITE-unboxed)
        ((M-I) M-J)                             ;Restore M-I
        ((WRITE-MEMORY-DATA) M-1)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-unboxed)
        (POPJ)

;Store into ART-COMPLEX-FLOAT.
QSCFARY (CALL-XCT-NEXT QSCARY-DECODE)
;M-J has real part, MD has imag part.
       ((PDL-PUSH) M-T)         ;Don't clobber M-T or M-Q.
        ((PDL-PUSH) DPB M-Q Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-PUSH) M-J)        ;QSFARY clobbers M-J.
        ((M-T) MD)
        (CALL-XCT-NEXT QSFARY)
       ((M-Q) M+A+1 M-Q A-Q)    ;Store the imaginary part, ART-FLOAT style.
        ((M-T) PDL-POP)
        (CALL-XCT-NEXT QSFARY)  ;Store the real part similarly.
       ((M-Q) SUB M-Q (A-CONSTANT 1))
        (POPJ-AFTER-NEXT
         (M-Q) LDB Q-POINTER PDL-POP)           ;Restore registers.
       ((M-T) PDL-POP)

;Store into ART-COMPLEX-FPS-FLOAT.
QSCFFARY (CALL-XCT-NEXT QSCARY-DECODE)
;M-J has real part, MD has imag part.
       ((PDL-PUSH) M-T)         ;Don't clobber M-T or M-Q.
        ((PDL-PUSH) DPB M-Q Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-PUSH) M-J)        ;QSFARY clobbers M-J.
        ((M-T) MD)
        (CALL-XCT-NEXT QSFFARY)
       ((M-Q) M+A+1 M-Q A-Q)    ;Store the imaginary part, ART-FPS-FLOAT style.
        ((M-T) PDL-POP)
        (CALL-XCT-NEXT QSFFARY) ;Store the real part similarly.
       ((M-Q) SUB M-Q (A-CONSTANT 1))
        (POPJ-AFTER-NEXT
         (M-Q) LDB Q-POINTER PDL-POP)           ;Restore registers.
       ((M-T) PDL-POP)

QSCARY  (CALL QSCARY-DECODE)
;MD has the imaginary part, M-J the real part.
        ((M-TEM) ADD M-Q A-Q)
        ((VMA-START-WRITE) M+A+1 M-array-origin A-TEM)
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        ((WRITE-MEMORY-DATA) M-J)
        ((VMA-START-WRITE) SUB VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (popj)

;Decode the number in M-T: put realpart in M-J and imag part in MD.
;If the number is real, put it in M-J and put boxed zero in MD.
QSCARY-DECODE
;First, is the number we are storing actually complex?
        (JUMP-DATA-TYPE-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) QSCARY-RATIONAL)
        (JUMP-DATA-TYPE-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTENDED-NUMBER))
                QSCARY-SMALL-FLOAT)
        ((VMA-START-READ) M-T)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER MD)
        ((M-TEM) HEADER-TYPE-FIELD MD)
        (JUMP-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-RATIONAL)) QSCARY-RATIONAL)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (EVAL %HEADER-TYPE-COMPLEX)) QSCARY-FLOAT)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)
        ((M-J) Q-TYPED-POINTER MD)
        (POPJ-AFTER-NEXT
         (VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-READ)

QSCARY-RATIONAL
        (POPJ-AFTER-NEXT
         (M-J) M-T)
       ((MD) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

QSCARY-SMALL-FLOAT
        (POPJ-AFTER-NEXT
         (M-J) M-T)
       ((MD) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-SMALL-FLONUM)))

QSCARY-FLOAT
        (POPJ-AFTER-NEXT
         (M-J) M-T)
       ((MD) A-FLOATING-ZERO)

;FPS-FLOAT has less precision than Lisp machine float, so round.
QSFFARY ((M-J) M-I)                             ;Save M-I
        ((C-PDL-BUFFER-POINTER-PUSH) M-T)       ;Value being stored
        (CALL GET-FLONUM)
        ;Transfer sign bit to M-TEM and get magnitude of fraction
        (CALL-LESS-THAN-XCT-NEXT M-1 A-ZERO FNEG1)              ;M-TEM OK
       ((M-TEM) SELECTIVE-DEPOSIT M-1 (BYTE-FIELD 1 31.) A-ZERO)
        ;Round off fraction
        ((M-4) (BYTE-FIELD 7 0) M-1)            ;Discarded bits of fraction
        (CALL-EQUAL M-4 (A-CONSTANT 1_6) QSFFRY2)       ;Stable rounding
        ((M-1) ADD M-1 (A-CONSTANT 1_6))
        (CALL-IF-BIT-SET (BYTE-FIELD 1 31.) M-1 QSFFRY3)        ;Renormalize
QSFFRY0 ((M-I) SUB M-I (A-CONSTANT 1600))       ;Get excess-200 exponent
        (JUMP-LESS-OR-EQUAL M-I A-ZERO QSFFRY1) ;Underflow or zero => zero
        ;Insert relevant fraction bits
        ((M-TEM) (BYTE-FIELD 23. 7) M-1 A-TEM)
        (JUMP-LESS-THAN-XCT-NEXT M-I (A-CONSTANT 400) QSFFRY1)
       ((M-TEM) DPB M-I (BYTE-FIELD 8 23.) A-TEM)
        ((M-TEM) DPB (M-CONSTANT -1) (BYTE-FIELD 31. 0) A-TEM)  ;Overflow => infinity
QSFFRY1 ((M-1) (BYTE-FIELD 16. 16.) M-TEM)      ;Swap halves
        ((WRITE-MEMORY-DATA) DPB M-TEM (BYTE-FIELD 16. 16.) A-1)
        ((VMA-START-WRITE) ADD M-array-origin A-Q)
        (CHECK-PAGE-WRITE-unboxed)
        (POPJ-AFTER-NEXT (M-I) M-J)
       (NO-OP)

QSFFRY2 (POPJ-AFTER-NEXT POPJ-IF-BIT-SET (BYTE-FIELD 1 7) M-1)
        (JUMP QSFFRY0)                          ;If lsb 0, suppress adding 1

QSFFRY3 (POPJ-AFTER-NEXT (M-1) (BYTE-FIELD 30. 1) M-1)  ;Shift fraction right 1
       ((M-I) ADD M-I (A-CONSTANT 1))           ;And increment exponent

(begin-comment) Zwei Lossage (end-comment)

   (ERROR-TABLE DEFAULT-ARG-LOCATIONS VECTOR-PUSH M-T M-C)
XVECTOR-PUSH (MISC-INST-ENTRY VECTOR-PUSH)
        ((M-C) Q-TYPED-POINTER PDL-POP)
        (JUMP-XCT-NEXT XFARY0)
       ((M-T) Q-TYPED-POINTER PDL-POP)

   (ERROR-TABLE DEFAULT-ARG-LOCATIONS ARRAY-PUSH M-C M-T)
   (MISC-INST-ENTRY ARRAY-PUSH)
XFARY   ((M-T) Q-TYPED-POINTER PDL-POP)
        ((M-C) Q-TYPED-POINTER PDL-POP)
   (ERROR-TABLE RESTART XFARY-RESTART)
XFARY0
#+exp   ((vma) m-c)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type m-c
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache m-c)
   (ERROR-TABLE CALLS-SUB ARRAY-PUSH)
XFARY-1
        (CALL-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-LEADER-BIT) M-array-header array-trap)
   (ERROR-TABLE ARRAY-HAS-NO-LEADER M-C XFARY-RESTART)
        ((VMA-START-READ) SUB M-Array-pointer (A-CONSTANT 2))   ;REF FILL POINTER
        (CHECK-PAGE-READ)                       ;NO TRANSPORT SINCE JUST TOUCHED HEADER
        (array-trap-unless-fixnum md :argument 1)
   (ERROR-TABLE FILL-POINTER-NOT-FIXNUM M-C xfary-restart)
        ((M-Q) Q-POINTER READ-MEMORY-DATA)      ;THIS ONE GETS RELOCATED IF INDIRECT ARY
        ((A-FARY-TEM) Q-TYPED-POINTER READ-MEMORY-DATA)  ;NOT CLOBBERED BY ARY ROUTINES
                                                ;THIS COPY USED FOR INCREMENTING AND
                                                ;STORING BACK
        (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-array-header
                         decode-displaced-array)
        (JUMP-GREATER-OR-EQUAL M-Q A-array-length XFALSE) ;out of bounds, return NIL, don't store.
        ((VMA) SUB M-Array-pointer (A-CONSTANT 2))              ;KNOW WILL WIN NOW, MUNG
        ((WRITE-MEMORY-DATA-START-WRITE) ADD A-FARY-TEM M-ZERO ALU-CARRY-IN-ONE)
        (CHECK-PAGE-WRITE)
        (DISPATCH-CALL (LISP-BYTE %%ARRAY-TYPE-FIELD) M-array-header
                        ARRAY-TYPE-FILL-DISPATCH)
   (ERROR-TABLE BAD-ARRAY-TYPE m-array-header)
        (POPJ-AFTER-NEXT        ;RETURN ELEMENT NUMBER STORED INTO.
         (M-T) A-FARY-TEM)
       ((M-T) IOR (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)) M-T)

XFALAR  ((M-TEM1) M-Q)                  ;HERE FROM ARRAY-TYPE-FILL-DISPATCH FOR Q-LIST-ARRAY
        ((VMA) ADD A-TEM1 M-array-origin)               ;MUST HACK CDR CODES
        ((WRITE-MEMORY-DATA-START-WRITE)  ;NO TRANSPORTER HACKERY NEEDED SINCE ADDRESSING
             DPB M-T Q-TYPED-POINTER      ;A "FRESH" Q.
                (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NIL)))
        (CHECK-PAGE-WRITE)
        (GC-WRITE-TEST)
        (POPJ-EQUAL A-FARY-TEM M-ZERO)  ;FIRST ENTRY, DO NOTHING
        ((VMA-START-READ) SUB VMA (A-CONSTANT 1))       ;NO TRANSPORT NEEDED (JUST FIDDLING
        (CHECK-PAGE-READ)                               ;CDR CODE)
        (POPJ-AFTER-NEXT
         (WRITE-MEMORY-DATA-START-WRITE)
             DPB READ-MEMORY-DATA Q-TYPED-POINTER
                    (A-CONSTANT (BYTE-VALUE Q-CDR-CODE CDR-NEXT)))
       (CHECK-PAGE-WRITE)

   (MISC-INST-ENTRY STORE-ARRAY-LEADER)
XSALDR  (CALL XFLAD1)           ;STORE IN ARRAY LEADER
  (ERROR-TABLE CALLS-SUB STORE-ARRAY-LEADER)
;NEEDS TRANSPORTER HACKERY HERE IF ONE-Q-FORWARD S IN ARRAY-LEADERS ARE TO BE SUPPORTED.
XSALDR-AREFI
        ((M-T WRITE-MEMORY-DATA-START-WRITE)
                Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (CHECK-PAGE-WRITE)              ;SEQ BRK O.K. HERE
        (GC-WRITE-TEST)
        (popj)

XSET-ARRAY-LEADER   (MISC-INST-ENTRY SET-ARRAY-LEADER)
        ((M-T) Q-TYPED-POINTER PDL-POP)
  (ERROR-TABLE ARG-POPPED 0 M-T)
        (CALL XFLAD1)           ;STORE IN ARRAY LEADER
  (ERROR-TABLE CALLS-SUB STORE-ARRAY-LEADER)
;NEEDS TRANSPORTER HACKERY HERE IF ONE-Q-FORWARD S IN ARRAY-LEADERS ARE TO BE SUPPORTED.
XSET-ARRAY-LEADER-AREFI
        ((M-T WRITE-MEMORY-DATA-START-WRITE) M-T)
        (CHECK-PAGE-WRITE)              ;SEQ BRK O.K. HERE
        (GC-WRITE-TEST)
        (popj)

   (MISC-INST-ENTRY ARRAY-LEADER)
XFALDR  (CALL XFLAD1)                   ;FETCH ELEMENT IN ARRAY LEADER
  (ERROR-TABLE CALLS-SUB ARRAY-LEADER)
XFALDR-AREFI
        ((VMA-START-READ) VMA)
        (CHECK-PAGE-READ)               ;SEQ BRK O.K. HERE
        (POPJ-AFTER-NEXT DISPATCH TRANSPORT READ-MEMORY-DATA)
       ((M-T) Q-TYPED-POINTER READ-MEMORY-DATA)

XFLAD1-RESTART
    (ERROR-TABLE RESTART XFLAD1-RESTART)
        (CALL GAHDR)
        (JUMP XFLAD1-RESTART-1)
;Pop index and array off stack, and return in VMA the address
;of the slot in the leader specified by the index.
XFLAD1
;       (array-trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
   (ERROR-TABLE ARG-POPPED 0 PP PP)
        ((M-Q) Q-POINTER C-PDL-BUFFER-POINTER-POP)              ;OR ARRAY LEADER ELEMENT
find-array-leader
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
   (ERROR-TABLE ARG-POPPED 0 M-A M-Q)
XFLAD1-RESTART-1
        (CALL-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-LEADER-BIT) M-array-header array-trap)
   (ERROR-TABLE ARRAY-HAS-NO-LEADER M-A XFLAD1-RESTART)
   (ERROR-TABLE RESTART XFLAD1-A)
   (ERROR-TABLE ARG-POPPED 0 M-Q)
        ((VMA-START-READ) SUB m-array-pointer (A-CONSTANT 1))   ;GET LENGTH OF ARRAY LEADER
        (CHECK-PAGE-READ)       ;NO TRANSPORT SINCE JUST TOUCHED HEADER
        ((M-TEM1) (LISP-BYTE %%ARRAY-LEADER-LENGTH) READ-MEMORY-DATA)
        (CALL-GREATER-OR-EQUAL M-Q A-TEM1 array-trap)           ;SUBSCRIPT OUT OF
   (ERROR-TABLE SUBSCRIPT-OOB M-Q RMD XFLAD1-A)
   (ERROR-TABLE ARG-POPPED 0 M-A M-Q)
        (POPJ-AFTER-NEXT (M-TEM1) ADD M-Q (A-CONSTANT 2))
       ((VMA) SUB m-array-pointer A-TEM1)

xarray-has-leader-p
XAHLP  (MISC-INST-ENTRY ARRAY-HAS-LEADER-P)
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
  (ERROR-TABLE ARG-POPPED 0 M-A)
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%ARRAY-LEADER-BIT) M-array-header XFALSE)
        (JUMP XTRUE)

XAPLD (MISC-INST-ENTRY AP-LEADER)       ;RETURN LOCATIVE POINTER TO LEADER ELEMENT
        (CALL XFLAD1)                   ;SET UP VMA
  (ERROR-TABLE CALLS-SUB AP-LEADER)
XAP1B   (POPJ-AFTER-NEXT
         (M-T) DPB VMA Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-LOCATIVE)))
       (NO-OP)

(ERROR-TABLE DEFAULT-ARG-LOCATIONS COPY-ARRAY-CONTENTS-AND-LEADER M-C M-T)

XCARCL (MISC-INST-ENTRY COPY-ARRAY-CONTENTS-AND-LEADER)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)                ;TO
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)                ;FROM
        (CALL-XCT-NEXT GALPTR)
       ((m-array-pointer) invalidate-array-cache m-c)
        ((M-Q) M-array-length)                                  ;LENGTH OF FROM LEADER
        ((M-J) M-array-origin)                                  ;HIGH ADDRESS OF FROM LEADER
        (CALL-XCT-NEXT GALPTR)
       ((m-array-pointer) invalidate-array-cache m-t)
        ((M-I) A-ZERO)                                  ;CURRENT ARRAY LEADER INDEX
XCALD1  (JUMP-GREATER-OR-EQUAL M-I A-array-length XCARC0)       ;TO LEADER DONE, GO COPY DATA
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-I A-Q XCALD2)
       ((WRITE-MEMORY-DATA) A-V-NIL)                    ;IF FROM LEADER EXHAUSTED, USE NIL
        ((VMA-START-READ) M-J)                          ;GET FROM ARRAY LEADER ITEM
        (CHECK-PAGE-READ)
        ((M-J) SUB M-J (A-CONSTANT 1))
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ;((WRITE-MEMORY-DATA) READ-MEMORY-DATA)
XCALD2  ((VMA-START-WRITE) M-array-origin)      ;STORE IN TO ARRAY LEADER ITEM
        (CHECK-PAGE-WRITE)                      ;NO TRANSP HERE SINCE TOUCHED HEADER?
        (GC-WRITE-TEST)
        ((M-array-origin) SUB M-array-origin (A-CONSTANT 1))
    ((m-array-origin) q-pointer m-array-origin)
        (JUMP-XCT-NEXT XCALD1)
       ((M-I) ADD M-I (A-CONSTANT 1))

;(COPY-ARRAY-PORTION FROM-ARRAY FROM-START FROM-END TO-ARRAY TO-START TO-END)
;IF THE TO-LENGTH IS LONGER IT FILLS WITH 0 OR NIL
XCAP (MISC-INST-ENTRY COPY-ARRAY-PORTION)
        ((M-R) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;TO-END
        ((M-Q) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;TO-START
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)

  (ERROR-TABLE CALLS-SUB COPY-ARRAY-PORTION)
        ((M-R) SUB M-R A-Q)                             ;DON'T GET SCREWED BY DSP-ARRAY-SETUP
        (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-array-header decode-displaced-array)
        ((M-R) ADD M-R A-Q)
        ((M-I) M-Q)                                     ;TO-INDEX
        (CALL-GREATER-THAN M-R A-array-length TRAP)     ;TO-LENGTH IN M-R MUST BE IN-BOUNDS *****
  (ERROR-TABLE SUBSCRIPT-OOB M-R M-S)
        ((M-C) M-array-origin)                                  ;TO-ADDRESS
        ((M-K) M-array-header)                                  ;TO-ARRAY-HEADER
        ((M-T) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;FROM-END
        ((M-Q) Q-POINTER C-PDL-BUFFER-POINTER-POP)      ;FROM-START
#+exp   ((vma) c-pdl-buffer-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type c-pdl-buffer-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache c-pdl-buffer-pointer-pop)
  (ERROR-TABLE CALLS-SUB COPY-ARRAY-PORTION)
        ((M-T) SUB M-T A-Q)                             ;DON'T GET SCREWED BY DSP-ARRAY-SETUP
        (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-array-header decode-displaced-array)
        ((M-T) ADD M-T A-Q)
        (CALL-GREATER-THAN M-T A-array-length array-trap) ;FROM-LENGTH IN M-T MUST BE IN-BOUNDS *****
  (ERROR-TABLE SUBSCRIPT-OOB M-T M-S)
     ;; Since we modify M-ARRAY-LENGTH, we must invalidate the cache.
        ((m-array-pointer) invalidate-array-cache m-array-pointer)
        (JUMP-XCT-NEXT XCARC1)
       ((m-array-length) M-T)

;NOTE:  AN OPTIMIZATION TO DO IT WORD BY WORD MIGHT BE HANDY...
XCARC (MISC-INST-ENTRY COPY-ARRAY-CONTENTS)
        ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)                ;TO
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)                ;FROM
XCARC0  (CALL-XCT-NEXT GADPTR)
       ((m-array-pointer) invalidate-array-cache m-t)
  (ERROR-TABLE CALLS-SUB COPY-ARRAY-CONTENTS)
  (ERROR-TABLE ARG-POPPED 0 M-C M-T)
        ((m-array-pointer) invalidate-array-cache m-c)  ;FROM-ARRAY
        ((M-R) M-array-length)                  ;TO LENGTH
        ((m-c) q-pointer m-array-origin)               ;TO ADDRESS
        ((M-I) M-Q)                                     ;TO INITIAL INDEX
        (CALL-XCT-NEXT GADPTR)
       ((M-K) M-array-header)                                   ;TO ARRAY HEADER
  (ERROR-TABLE CALLS-SUB COPY-ARRAY-CONTENTS)
  (ERROR-TABLE ARG-POPPED 0 M-A M-T)
XCARC1  (JUMP-GREATER-OR-EQUAL M-I A-R XTRUE)           ;TO ARRAY DONE, RETURN
        (JUMP-GREATER-OR-EQUAL M-Q A-array-length XCARC3)       ;JUMP IF FROM ARRAY EXHAUSTED
        (DISPATCH-CALL-XCT-NEXT                         ;M-T := FROM ITEM, CLOBBER M-J
                (LISP-BYTE %%ARRAY-TYPE-FIELD) m-array-header ARRAY-TYPE-REF-DISPATCH)
   (ERROR-TABLE BAD-ARRAY-TYPE m-array-header)
XCARC4 ((C-PDL-BUFFER-POINTER-PUSH) Q-POINTER M-Q
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((m-d) q-pointer m-array-origin)
        ((M-Q) M-I)
        (DISPATCH-XCT-NEXT (LISP-BYTE %%ARRAY-TYPE-FIELD) M-K
                           ARRAY-TYPE-STORE-DISPATCH-PUSHJ)
       ((M-array-origin) q-pointer M-C)
XCARC5  ((M-I) ADD M-I (A-CONSTANT 1))
        ((M-Q) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-Q) ADD M-Q (A-CONSTANT 1))
        (JUMP-XCT-NEXT XCARC1)
       ((M-array-origin) q-pointer M-D)

;COMPUTE FILLER VALUE IN M-T, REENTER AT XCARC4
;THIS USED TO PAD STRINGS WITH 200, BUT THAT WAS A CROCK
XCARC3  ((M-T) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))   ;Zero for numeric array
        (DISPATCH (LISP-BYTE %%ARRAY-TYPE-FIELD) M-K SKIP-IF-NUMERIC-ARRAY)
         ((M-T) A-V-NIL)                        ;NIL for non-numeric
        (JUMP XCARC4)

;GET ADDRESS AND LENGTH OF ARRAY LEADER
GALPTR
#+exp   ((vma) m-array-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type m-array-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache m-array-pointer)
        ((m-array-origin) output-selector-mask-25 sub m-array-pointer (a-constant 2))  ;Address.
        (popj-if-bit-clear-xct-next (lisp-byte %%array-leader-bit) m-array-header)
       ((m-array-length) a-zero)                                ;LENGTH
        ((vma-start-read) sub m-array-pointer (a-constant 1))   ;no transport needed.
        (check-page-read)
        (popj-after-next
         (m-array-length) (lisp-byte %%array-leader-length) read-memory-data)
     ;; Since we clobber M-ARRAY-LENGTH, we must invalidate the cache.
       ((m-array-pointer) invalidate-array-cache m-array-pointer)

;Get address, length, and initial index of an array.
;Like GAHDR, but processes displacing and indirection of arrays.
     (error-table restart gadptr)
GADPTR
#+exp   ((vma) m-array-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type m-array-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache m-array-pointer)
        (popj-if-bit-clear-xct-next (lisp-byte %%array-displaced-bit) m-array-header)
       ((m-q) a-zero)
        (jump decode-displaced-array)

(begin-comment) Zwei Lossage (end-comment)

;; General subroutine for decoding an array's header information.
;; Dispatch on array pointer with ARRAY-HEADER-SETUP-DISPATCH, arranging for
;; M-ARRAY-POINTER to have a copy of the pointer, or call GAHDR.
;; Sets up M-ARRAY-HEADER, M-ARRAY-ORIGIN, M-ARRAY-LENGTH, M-ARRAY-RANK.

gahdr-trap
        ;; Enter here from ARRAY-HEADER-SETUP-DISPATCH if array pointer has bad data-type.
        ((m-array-pointer) invalidate-array-cache m-a)
        (call array-trap)
     (error-table argtyp array m-array-pointer 0 (gahdr restore-array-registers) gahdr)

;gahdra ((m-array-pointer) c-pdl-buffer-pointer-pop)
     (error-table restart gahdr)
gahdr
#+exp   ((vma) m-array-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type m-array-pointer
            array-header-setup-dispatch)
       ((m-a) validate-general-array-cache m-array-pointer)

gahdr-d
     ;; Enter here from ARRAY-HEADER-SETUP-DISPATCH.
        (popj-equal m-a a-array-pointer)        ;Abort if cache hits.
        ((vma-start-read) vma)
        (check-page-read)
        (dispatch transport-header md)
        ((m-array-pointer) validate-general-array-cache vma)    ;Might have moved.
        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-header))
            array-trap)
     (error-table data-type-screwup array)
        ((m-array-header) q-pointer md)
        ((m-array-rank) (lisp-byte %%array-number-dimensions) m-array-header)
        ((m-array-origin) output-selector-mask-25 add m-array-pointer a-array-rank)

        (jump-not-equal m-array-rank a-zero gahd2)
        ((m-array-origin) add m-array-origin (a-constant 1))    ;Point to the single element
gahd2
        (popj-after-next
          (m-array-length) (lisp-byte %%array-index-length-if-short) m-array-header)
       (call-if-bit-set (lisp-byte %%array-long-length-flag) m-array-header gahd3)

gahd3   ;; Long array, get index length Q and revise origin and length.
        ((vma-start-read) add m-array-pointer (a-constant 1))
        (check-page-read)                       ;No transport since just touched header.
        (popj-after-next
          (m-array-origin) add m-array-origin (a-constant 1))
       ((m-array-length) q-pointer md)

;;; Decoding one-dimensional arrays.

decode-1d-stack-group
        (jump-data-type-equal m-array-pointer
                                  (a-constant (byte-value q-data-type dtp-stack-group))
            decode-1d-array-d)
decode-1d-array-trap
        (call array-trap)
     (error-table argtyp array m-array-pointer 0 (decode-1d-array-restart restore-array-registers))
     ;; Fall through into type-checking loop.

     (error-table restart decode-1d-array-restart)
decode-1d-array-uncached
        (jump-data-type-not-equal m-array-pointer
                                  (a-constant (byte-value q-data-type dtp-array-pointer))
            decode-1d-stack-group)
decode-1d-array-d
        ((vma-start-read) m-array-pointer)
        (check-page-read)
        (dispatch transport-header md)
        ((m-array-origin) output-selector-mask-25 add vma (a-constant 1))
        ((m-array-header) md)
decode-1d-array-force-entry
        ((m-tem) and m-array-header (a-constant (plus (byte-value %%array-long-length-flag 1)
                                                      (byte-value %%array-displaced-bit 1)
                                                      (byte-value %%array-number-dimensions 7777)
                                                      (byte-value q-data-type 77777))))
        (popj-after-next
          (m-array-length) (lisp-byte %%array-index-length-if-short) m-array-header)
       (call-not-equal m-tem (a-constant (plus (byte-value %%array-number-dimensions 1)
                                               (byte-value q-data-type dtp-array-header)))
                       decode-unusual-1d-array)

;Either it is long, or displaced, or it has the wrong rank, or a bad data type.
decode-unusual-1d-array
     ;; First handle the ones that are just long.  That is most common case here.
        (jump-not-equal m-tem (a-constant (plus (byte-value %%array-number-dimensions 1)
                                                (byte-value q-data-type dtp-array-header)
                                                (byte-value %%array-long-length-flag 1)))
                decode-weird-1d-array)
     ;; Get the index length from its Q.
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        (popj-after-next
          (m-array-origin) output-selector-mask-25 add m-array-origin (a-constant 1))
       ((m-array-length) q-pointer md)

;; Either it has the wrong rank, a bad data type or it is a displaced array.
decode-weird-1d-array
        (call-data-type-not-equal m-array-header
            (a-constant (byte-value q-data-type dtp-array-header))
            array-trap)
    (error-table data-type-screwup array)
        ((m-array-length) (lisp-byte %%array-number-dimensions) m-array-header)
        (call-not-equal m-array-length (a-constant 1) array-trap)
    (error-table array-number-dimensions m-array-length 1 m-array-pointer
                 (decode-1d-array-restart restore-array-registers))
        (jump decode-displaced-array)

(begin-comment) Zwei Lossage (end-comment)

decode-displaced-array
     ;; Since we change M-ARRAY-ORIGIN, we must invalidate the cache. However, we
     ;; must invalidate it so much that array candidate, regardless of its cdr codes,
     ;; could hit.  However, some places rely on the virtual address, so we use
     ;; q-pointer.  ** 1/19/87 BUT we must maintain a pointer DTP.  Also, a 0
     ;; DTP caused the error handler to bomb in case of subscript-oob, etc.
        ((m-array-pointer) q-pointer m-array-pointer
            (a-constant (byte-value q-data-type dtp-locative)))
        ((vma-start-read) add m-array-origin (a-constant 1))
        (check-page-read)
        (dispatch transport md)
        ((m-array-length) q-pointer md)
        ((vma-start-read) m-array-origin)
        (check-page-read)
        (dispatch transport md)
        (popj-data-type-not-equal-xct-next
            md (a-constant (byte-value q-data-type dtp-array-pointer)))
       ((m-array-origin) q-pointer md)

;;Drops in if Indirect-array
;;Operation of QDACMP:
;; The word just read from memory is the array-pointer to indirect to
;; M-Q has entry number desired
;;QDACMP pushes the info relative to the indirect array (M-A, M-B, M-D).
;; M-E eventually gets the data base of the pointed-to array.
;; M-S gets MIN(M-S from indirect array + index offset, index length of pointed-to array).
;; In the process, M-Q will be adjusted if an index offset is encountered.
;; After the final data base is determined, M-A, M-B, and M-D are restored.

decode-indirect-array
        ((pdl-push) m-array-pointer)
        ((pdl-push) m-array-header)
        ((pdl-push) m-array-rank)
        ((pdl-push) m-d)
        ;; Save array type of original array for  later comparison.
        ((pdl-push) (lisp-byte %%array-type-field) m-array-header
            (a-constant (byte-value q-data-type dtp-fix)))
;Come here each time we trace through to another indirect array.
;M-B has the header of the last indirect array found, which we will now trace.
;In particular, the low bit of M-B is set if the length-field is 3 (index offset)
;and not if it is 2 (no index offset).
qdacm2  ((m-array-pointer) invalidate-array-cache md)
        (jump-if-bit-clear (byte-field 1 0) m-array-header qdacm5)      ;Jump unless index-offset.
        ((vma-start-read) add vma (a-constant 2))
        (check-page-read)
        (dispatch transport md)
        (trap-unless-fixnum md)
    (error-table data-type-screwup array)
        ((m-d) q-pointer md)                    ;Fetch index offset.
        ((m-array-length) add m-array-length a-d)       ;Adjust index limit.
        ((m-q) add m-q a-d)                             ;Adjust current index.
qdacm5
        ((m-a) invalidate-array-cache m-zero)
#+exp   ((vma) m-array-pointer)
        (dispatch-xct-next #+lambda dispatch-write-vma q-data-type m-array-pointer
            array-header-setup-dispatch)
     ;; Save index length of indirect array.
       ((pdl-push) q-pointer m-array-length (a-constant (byte-value q-data-type dtp-fix)))
     (error-table calls-sub array-indirect)
        (jump-if-bit-clear-xct-next (lisp-byte %%array-displaced-bit) m-array-header qdacmi)
     ;; Find minimum of the two lengths.
       ((m-d) q-pointer c-pdl-buffer-pointer-pop)
     ;; Double displace, get correct length.
        ((vma-start-read) add m-array-origin (a-constant 1))
        (check-page-read)
        ((m-array-length) q-pointer md)
qdacmi  ;; Same array type as original reference?
        ((m-tem) (lisp-byte %%array-type-field) m-array-header
            (a-constant (byte-value q-data-type dtp-fix)))
        (jump-not-equal c-pdl-buffer-pointer a-tem qdacm8)  ;No, original controls.
        (jump-greater-or-equal m-d a-array-length qdacm7)
qdacm8  ((m-array-length) m-d)
qdacm7  (jump-if-bit-set (lisp-byte %%array-displaced-bit) m-array-header qdacm6) ;Further indir.
qdacm1  ((m-garbage) c-pdl-buffer-pointer-pop)  ;Flush array type.
     ;; Restore indirect array info.
        ((m-d) c-pdl-buffer-pointer-pop)
        ((m-array-rank) c-pdl-buffer-pointer-pop)
        (popj-after-next
         (m-array-header) c-pdl-buffer-pointer-pop)
     ;; See comment in displaced array code about cache invalidation.
       ((m-array-pointer) q-pointer c-pdl-buffer-pointer-pop
            (a-constant (byte-value q-data-type dtp-locative)))

qdacm6  ((vma-start-read) m-array-origin)
        (check-page-read)
        (dispatch transport md)
        (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-array-pointer)) qdacm2)
        (jump-xct-next qdacm1)                  ;Just displaced.
       ((m-array-origin) q-pointer md)

))

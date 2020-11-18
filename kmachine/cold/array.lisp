;;; -*- Mode:LISP; Package:ARRAY; Base:10.; Readtable:CL -*-

(export
  '(;; Common Lisp
    ARRAY-DIMENSION-LIMIT
    ARRAY-DIMENSIONS
    ARRAY-ELEMENT-TYPE
    ARRAY-HAS-FILL-POINTER-P
    ARRAY-IN-BOUNDS-P
    ARRAY-RANK
    ARRAY-RANK-LIMIT
    ARRAY-ROW-MAJOR-INDEX
    ARRAY-TOTAL-SIZE
    ARRAY-TOTAL-SIZE-LIMIT
    SIMPLE-STRING-P
    SIMPLE-VECTOR-P
    STRINGP
    SVREF
    VECTORP

    ;; non Common Lisp
    MAKE-VECTOR
    AREF-1
    ASET-1
))


;
;*******************************************************************************
;    Common LISP required constants
;
(defconstant ARRAY-RANK-LIMIT              8.   "Arrays may have 0 to 7 dimensions.")
(defconstant ARRAY-DIMENSION-LIMIT   2097152.   "Max number of elements in any one array dimension")
(defconstant ARRAY-TOTAL-SIZE-LIMIT  2097152.   "Most pessimistic number of elements allowed in an array")


;*******************************************************************************
;
; array-header   format -->    31         26 25      21 20               0
;                             +-------------+----------+------------------+
;                               dtp-array-    acode       index 0 bounds
;                               header-1
;
; acodes are as follogs:
;      acode 0  = simple vector ART-Q array
;      acode 1  = simple vector ART-1B  unsigned array
;      acode 2  = simple vector ART-2B    signed array
;      acode 3  = simple vector ART-2B  unsigned array
;      acode 4  = simple vector ART-4B    signed array
;      acode 5  = simple vector ART-4B  unsigned array
;      acode 6  = simple vector ART-8B    signed array
;      acode 7  = simple vector ART-8B  unsigned array
;      acode 8  = simple vector ART-16B   signed array
;      acode 9  = simple vector ART-16B unsigned array
;      acode 10 = simple vector ART-32B   signed array
;      acode 11 = simple vector ART-32B unsigned array
;      acode 12 = simple vector ART-STRING
;      acode 13 = simple vector ART-FAT-STRING
;      acode 14 = simple vector ART-SINGLE-FLOAT
;      acode 15 = simple vector ART-DOUBLE-FLOAT
;      acode 16-27 = spare
;      acode 28 = ART-CONTROL-PDL
;      acode 29 = ART-EXTRANEOUS-PDL
;      acode 30 = ART-SPECIAL-PDL
;      acode 31 = hard array
;
;
;  array-header-extension format:
;
;         31          26 25     23 22 21 20 19 18 17 16  14 13        9 8 7    4 3      0
;        +--------------+---------+--+--+--+--+--+--+------+-----------+-+------+--------+
;         dtp-array-        spare   A  F NS  D  I  L   num    array     s displ   leader
;         header-extension                             dims   type      p offset  offset
;                                                                       a
;       A - adjustable array
;       F - has fill pointer
;       NS- Is a named Structure.
;       D - displaced array
;       I - not used (formerly indirect array)
;       L - has array leader
;       Array-type - same as acode, but 31 is an ART-ERROR
;       displ-offset <%%displaced-to-offset>  not currently used!
;       leader-offset <%%leader-offset>  from header to base of leader.
;
;  array-leader-header format:
;
;          31           26 25 22 21               0
;         +---------------+-----+-----------------+
;          dtp-leader-     spare  length of array
;          header
;
;
;
;  array storage in memory:
;
;     lowest addr:      array-header-size  (array-extension-header)
;                       leader(n)
;                        ...
;                       leader(1)
;                       leader(0)
;                       leader-header      (fixnum)
;                       fill-pointer       (fixnum)
;                       displaced-offset-2 (fixnum)
;                       displaced-offset-1 (fixnum)
;                       displaced-to       (unboxed-locative or array)
;                       dim(S0)            (fixnum)
;                        ...
;                       dim(S6)             (fixnum)
;                       array-second-header (fixnum)
;  array-pointer -----> array-header (S7)
;                       data(0)
;                        ...
;    highest addr:      data(m)
;

;*******************************************************************************

(defconstant ART-Q              0.)
(defconstant ART-1B             1.)
(defconstant ART-2BS            2.)
(defconstant ART-2B             3.)
(defconstant ART-4BS            4.)
(defconstant ART-4B             5.)
(defconstant ART-8BS            6.)
(defconstant ART-8B             7.)
(defconstant ART-16BS           8.)
(defconstant ART-16B            9.)
(defconstant ART-32BS          10.)
(defconstant ART-32B           11.)
(defconstant ART-STRING        12.)
(defconstant ART-FAT-STRING    13.)
(defconstant ART-SINGLE-FLOAT  14.)
(defconstant ART-DOUBLE-FLOAT  15.)

(defconstant ART-CONTROL-PDL    28.)
(defconstant ART-EXTRANEOUS-PDL 29.)
(defconstant ART-SPECIAL-PDL   30.)
(defconstant ART-HARD          31.)
(defconstant ART-ERROR         31.)

(defconstant %%SV-ART           (byte 5. 21.)   "Simple vector array type byte-spec in array header")  ;ACODE field in header
(defconstant %%BOUNDS           (byte 21. 0.)   "Byte-spec for dimensions in array header and dimension words")
;array-header-extension fields:
(defconstant %%LEADER-OFFSET    (byte 4 0.)     "Byte-spec for array leader offset in extension header")
(defconstant %%DISPLACED-TO-OFFSET (byte 4 4)   "Byte-spec for offset to displaced information")
(defconstant %%ARRAY-TYPE       (byte 5. 9.)    "Byte-spec for array type in extension header")
(defconstant %%DIMENSIONS       (byte 3. 14.)   "Byte-spec for rank of array in extension header")
(defconstant %%LEADER-P         (byte 1. 17.)   "Byte-spec for has-leader flag in extension header")
(defconstant %%INDIRECT-P       (byte 1. 18.)   "Byte-spec for indirect-array flag in extension header")
(defconstant %%DISPLACED-P      (byte 1. 19.)   "Byte-spec for displaced-array flag in extension header")
(defconstant %%NAMED-STRUCTURE-P (byte 1. 20.)  "Byte-spec for Named-Structure flag in extension header")
(defconstant %%FILL-POINTER-P   (byte 1. 21.)   "Byte-spec for fill-pointer flag in extension header")
(defconstant %%ADJUSTABLE-P     (byte 1. 22.)   "Byte-spec for adjustable flag in extension header")
;array-leader-header fields:
(defconstant %%LEADER-LENGTH    (byte 21. 0)    "Byte-spec for leader length of array")


(defsubst %VM-READ32 (pointer offset)
  (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:32+ offset pointer))
  (hw:read-md))

(defsubst %VM-READ (pointer)
  (hw:vma-start-read-vma-boxed-md-boxed pointer)
  (hw:read-md))

(defsubst %VM-WRITE32 (pointer offset data)
  (hw:write-md-unboxed data)
  (hw:vma-start-write-no-gc-trap-unboxed (hw:32+ offset pointer)))

(defsubst %VM-WRITE (pointer data)
  (hw:write-md-boxed data)
  (hw:vma-start-write-boxed pointer))

(defun ASET-1 (data array index)
  (start-array-header-reference array)  ;generates a VMA-START-READ with data-type trap
                        ;unless array.
  (aset-quick data index))

(defun AREF-1 (array index)
  (unless (vinc:%fixnump index)
    (li:tail-error "Not an fixnum to aref-1" array index)) ;;@@@ Remove this for speed.
  (start-array-header-reference array)
  (let ((ans (aref-quick index)))
;    (li:error "aref done" ans array index)
    ans))

(defun SVREF (array index)
  (when (minusp index)
    (li:error "Negative index to SVREF" array index))
  (%svref array index))

(defun %SVREF (array index)
  (start-array-header-reference array)
  (aref-quick index))

(defun SVSET (array index data)
  (when (minusp index)
    (li:error "Negative index to SVSET" array index data))
  (%svset array index data))

(defun %SVSET (array index data)
  (start-array-header-reference array)
  (aset-quick data index))

(defsetf svref svset)

;*******************************************************************************
; To call this - Start a read on the array-header and call ASET-QUICK with the index:
;      (alu setr vma-start-read gr:*random-structure* a7 dt-right-array-and-left-structure boxed-vma boxed-md)
;      (move o0 a10 ch-open) ;data
;      (call (aset-quick 1) a8 (o1 a9)) ;index
;
; vma - array pointer argument
; a0  - data
; a1  - index argument
; a2  - array pointer from vma
; a3  - bounds
; a4  - scaled index
; a5  - data temp
; a6  - bignum size
; a7  - bignum ptr
; a8  -
(defafun ASET-QUICK (data index)
  (alu-field field-extract-r nop a1 a1 (byte 3. -21.) dt-both-fixnum)
  (alu-field field-xor nop gr:*zero* md (byte 5. 21.) br-not-zero)
  (branch not-easy-index (alu l-r nop a1 md bw-24 br-not-equal))  ;** this would screw the
                ;sequence-break count if it transferred.
  (branch not-fast-q (alu load-status-r nop ignore gr:*data-type* bw-16 br-not-negative))
  (branch bounds-fail (alu r+1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break* boxed-right bw-24))
fast-q
  (alu l+r+c vma-start-read-will-write a1 vma bw-24 carry-1 unboxed-vma boxed-md)
  (nop)
  (alu setl md-start-write a0 md boxed-md)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (return a0)
not-fast-q
  (movea a3 (svset-dispatch 2))
  (alu-field field-extract-r a4 ignore md (byte 5. -21.) unboxed)
  (alu l+r nop a3 a4)
  (alu-field field-extract-r a3 ignore md %%bounds unboxed)
  (alu l-r nop a1 a3 bw-24 next-pc-dispatch)
bounds-fail
not-easy-index
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "ASET-QUICK bad subscript (vma index data)" boxed ch-open)
  (move o1 vma)
  (move o2 a1)
  (call (li:error 4) ignore (o3 a0))
  (nop)
  (return a0) ;;ignore
 )

(defafun SVSET-DISPATCH (data index)
  ;; a0 <--- data
  ;; a1 <--- index
  ;; vma <--- array-pointer
  (unconditional-branch sv-art-q                () br-greater-or-equal)
  (unconditional-branch sv-art-1b               (alu sex-r nop a0 a0 bw-24 dt-both-fixnum br-greater-or-equal))
  (unconditional-branch sv-art-2bs              (alu r+2 nop a0 a0 dt-both-fixnum-with-overflow br-greater-or-equal))
  (unconditional-branch sv-art-2b               (alu sex-r nop a0 a0 bw-24 dt-both-fixnum br-greater-or-equal))
  (unconditional-branch sv-art-4bs              () br-greater-or-equal)
  (unconditional-branch sv-art-4b               () br-greater-or-equal)
  (unconditional-branch sv-art-8bs              (alu shift-dn-0f-r a4 ignore a1 bw-24 br-greater-or-equal))
  (unconditional-branch sv-art-8b               (alu shift-dn-0f-r a4 ignore a1 bw-24 br-greater-or-equal))
  (unconditional-branch sv-art-16bs             (alu shift-dn-0f-r a4 ignore a1 bw-24 br-greater-or-equal))
  (unconditional-branch sv-art-16b              (alu shift-dn-0f-r a4 ignore a1 bw-24 br-greater-or-equal))
  (unconditional-branch sv-art-32bs             (alu aligned-field-xor nop a0 gr:*zero* pw-rr br-greater-or-equal))
  (unconditional-branch sv-art-32b              (alu aligned-field-xor nop a0 gr:*zero* pw-rr br-greater-or-equal))
  (unconditional-branch sv-art-string           (alu shift-dn-0f-r a4 ignore a1 bw-24 br-greater-or-equal))
  (unconditional-branch sv-art-fat-string       (alu aligned-field-xor nop a0 gr:*dtp-character* pw-rr br-greater-or-equal))
  (unconditional-branch sv-art-single-float     () br-greater-or-equal)
  (unconditional-branch sv-art-double-float     () br-greater-or-equal)
  (unconditional-branch broken                  ()) ;16
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ()) ;20
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ()) ;24
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch sv-art-q                () br-greater-or-equal) ;28
  (unconditional-branch sv-art-q                () br-greater-or-equal)
  (unconditional-branch sv-art-q                () br-greater-or-equal)
  (move a2 a1)                     ; 31
  (jump (aset-hard 3) (a1 vma)) ;;; (aset-hard data array index)

sv-art-q
  (branch bounds-fail (alu aligned-field-pass-right a2 gr:*dtp-locative* vma pw-rr boxed-right))
  (alu l+r+c vma-start-read-will-write a1 a2 bw-24 carry-1 boxed-vma boxed-md)
  (nop)
  (move nop md)
  (move md-start-write a0 boxed-md)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (return a0 boxed-right)

sv-art-string
  (branch bounds-fail (alu shift-dn-0f-r a4 ignore a4 bw-24))
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu aligned-field-xor nop a0 gr:*dtp-character* pw-rr)
  (alu-field field-extract-r a4 ignore a1 (byte 2. 3.) unboxed br-not-equal)
  (branch bad-data (alu load-status-r nop ignore a4 bw-8))
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 8. 0.) pw-ri unboxed-md) ;pos from status reg, width from IR.
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))


sv-art-fat-string
  (branch bounds-fail (alu shift-dn-0f-r a4 ignore a1 bw-24 br-not-zero))
  (branch bad-data (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1))
  (alu-field field-extract-r a4 ignore a1 (byte 1. 4.) unboxed)
  (alu load-status-r nop ignore a4 bw-8)
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 16. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-1b
  (branch bounds-fail (alu r-2 nop ignore a0 bw-24 br-negative))
  (branch bad-data () br-not-negative)
  (branch bad-data ())
  (alu-field field-extract-r a4 ignore a1 (byte 19. -5.) unboxed)
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a4 ignore a1 (byte 5. 0.) unboxed)
  (alu load-status-r nop ignore a4 bw-8)
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 1. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-2b
  (branch bounds-fail (alu r-4 nop ignore a0 bw-24 br-negative))
  (branch bad-data () br-not-negative)
  (branch bad-data ())
  (alu-field field-extract-r a4 ignore a1 (byte 20. -4.) unboxed)
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a4 ignore a1 (byte 4. 1.) unboxed)
  (alu load-status-r nop ignore a4 bw-8)
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 2. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-2bs
  (branch bounds-fail (alu r-2 nop ignore a0 bw-24 br-negative))
  (branch bad-data () br-not-negative)
  (branch bad-data ())
  (alu-field field-extract-r a4 ignore a1 (byte 20. -4.) unboxed)
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a4 ignore a1 (byte 4. 1.) unboxed)
  (alu load-status-r nop ignore a4 bw-8)
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 2. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-4b
  (branch bounds-fail ())
  (alu-field field-xor nop gr:*zero* a0 (byte 20. 4.) dt-both-fixnum)
  (alu-field field-extract-r a4 ignore a1 (byte 21. -3.) unboxed br-not-zero)
  (branch bad-data (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1))
  (alu-field field-extract-r a4 ignore a1 (byte 3. 2.) unboxed)
  (alu load-status-r nop ignore a4 bw-8)
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 4. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-4bs
  (branch bounds-fail ())
  (alu-field field-extract-r a4 ignore a1 (byte 21. -3.) unboxed)
  (alu-field field-xor nop gr:*zero* a0 (byte 21. 3.) dt-both-fixnum)
  (alu-field field-xor nop gr:*minus-one* a0 (byte 21. 3.) dt-both-fixnum br-not-zero)
  (branch bad-data (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1 br-not-zero))
  (branch bad-data ())
  (alu-field field-extract-r a4 ignore a1 (byte 3. 2.) unboxed)
  (alu load-status-r nop ignore a4 bw-8)
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 4. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-8b
  (branch bounds-fail (alu shift-dn-0f-r a4 ignore a4 bw-24))
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-xor nop gr:*zero* a0 (byte 16. 8.) dt-both-fixnum)
  (alu-field field-extract-r a4 ignore a1 (byte 2. 3.) unboxed br-not-zero)
  (branch bad-data (alu load-status-r nop ignore a4 bw-8))
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 8. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-8bs
  (branch bounds-fail (alu shift-dn-0f-r a4 ignore a4 bw-24))
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-xor nop gr:*zero* a0 (byte 17. 7.) dt-both-fixnum)
  (alu-field field-xor nop gr:*minus-one* a0 (byte 17. 7.) dt-both-fixnum br-not-zero)
  (alu-field field-extract-r a4 ignore a1 (byte 2. 3.) unboxed br-not-zero)
  (branch bad-data (alu load-status-r nop ignore a4 bw-8))
  (branch bad-data ())
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 8. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-16b
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-xor nop gr:*zero* a0 (byte 8. 16.) dt-both-fixnum)
  (alu-field field-extract-r a4 ignore a1 (byte 1. 4.) unboxed br-not-zero)
  (branch bad-data (alu load-status-r nop ignore a4 bw-8))
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 16. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-16bs
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a4 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-xor nop gr:*zero* a0 (byte 9. 15.) dt-both-fixnum)
  (alu-field field-xor nop gr:*minus-one* a0 (byte 9. 15.) dt-both-fixnum br-not-zero)
  (alu-field field-extract-r a4 ignore a1 (byte 1. 4.) unboxed br-not-zero)
  (branch bad-data (alu load-status-r nop ignore a4 bw-8))
  (branch bad-data ())
  (alu-field field-pass md-start-write-no-gc-trap a0 md (byte 8. 0.) pw-ri unboxed-md)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-32b
  (branch bounds-fail (alu aligned-field-xor nop a0 gr:*dtp-bignum* pw-rr br-equal))
  (branch fixnum-32b (move a2 vma br-not-equal))
  (branch bad-data ())
 bignum-32b
  (move vma-start-read a0 boxed-vma boxed-md)
  (nop)
  (alu r-1 nop ignore md bw-24)
  (alu r-2 nop ignore md bw-24 br-zero)
  (branch bignum-ok-32b (alu r+1 vma-start-read-no-transport ignore a0 unboxed-vma unboxed-md br-not-zero))
  (branch bad-data ())
  (move a5 md)
  (alu r+2 vma-start-read-no-transport ignore a0 unboxed-vma unboxed-md)
  (nop)
  (move nop md)
  (test br-zero)
  (branch store-it-32b (alu setr md ignore a5 unboxed-md))
  (unconditional-branch bad-data ())
 bignum-ok-32b
  (nop)
  (move md md)
  (test br-not-negative)
  (branch store-it-32b ())
  (unconditional-branch bad-data ())
 fixnum-32b
  (alu sex-r md a0 a0 bw-24 unboxed-md dt-both-fixnum)
  (test br-negative)
  (branch bad-data ())
 store-it-32b
  (alu l+r+c vma-start-write-no-gc-trap a1 a2  carry-1 bw-24 unboxed-vma)
 done-32b
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-32bs
  (branch bounds-fail (alu aligned-field-xor nop a0 gr:*dtp-bignum* pw-rr br-equal))
  (branch fixnum-32bs (move a2 vma br-not-equal))
  (branch bad-data ())
 bignum-32bs
  (move vma-start-read a0 boxed-vma boxed-md)
  (nop)
  (alu r-1 nop ignore md bw-24)
  (alu r+1 vma-start-read-no-transport ignore a0 unboxed-vma unboxed-md br-not-zero)
  (branch bad-data ())
  (unconditional-branch done-32bs (alu setr md ignore md unboxed-md))
 fixnum-32bs
  (alu sex-r md ignore a0 bw-24 unboxed-md)
 done-32bs
  (alu l+r+c vma-start-write-no-gc-trap a1 a2 bw-24 carry-1 unboxed-vma)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-single-float
  (branch bounds-fail ())
  (movei a6 vinc:$$dtp-single-float unboxed)
  (alu-field field-xor nop a6 a0 vinc:%%data-type)
  (test br-not-equal)
  (branch bad-data (move a2 vma))
  (alu r+1 vma-start-read-no-transport ignore a0 unboxed-vma unboxed-md)
  (nop)
  (move md md unboxed-md)
  (alu l+r+c vma-start-write-no-gc-trap a1 a2 bw-24 unboxed-vma carry-1)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

sv-art-double-float
  (branch bounds-fail (alu shift-up-0f-r a4 ignore a1 bw-24))
  (movei a6 vinc:$$dtp-double-float unboxed)
  (alu-field field-xor nop a6 a0 vinc:%%data-type)
  (test br-not-equal)
  (branch bad-data (move a2 vma))
  (alu r+1 vma-start-read-no-transport ignore a0 unboxed-vma unboxed-md)
  (nop)
  (move md md unboxed-md)
  (alu l+r+c vma-start-write-no-gc-trap a4 a2 bw-24 unboxed-vma carry-1)
  (alu r+1 a4 ignore a4 bw-24)
  (alu r+2 vma-start-read-no-transport ignore a0 unboxed-vma unboxed-md)
  (nop)
  (move md md unboxed-md)
  (alu l+r+c vma-start-write-no-gc-trap a4 a2 bw-24 unboxed-vma carry-1)
  (unconditional-branch svset-exit
     (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list))

svset-exit
  (return a0)
bounds-fail
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "SVSET-DISPATCH bad subscript (vma index data)" boxed ch-open)
  (move o1 vma)
  (move o2 a1)
  (call (li:error 4) ignore (o3 a0))
  (nop)
  (return a0) ;;ignore
bad-data
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "SVSET-DISPATCH bad data (vma index data)" boxed ch-open)
  (move o1 vma)
  (move o2 a1)
  (call (li:error 4) ignore (o3 a0))
  (nop)
  (return a0) ;;ignore
broken
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "SVSET-DISPATCH bad array type (vma index data)" boxed ch-open)
  (move o1 vma)
  (move o2 a1)
  (call (li:error 4) ignore (o3 a0))
  (nop)
  (return a0) ;;ignore
 )

;*******************************************************************************
; To call this - Start a read on the array-header and call AREF-QUICK with the index:
;      (alu setr vma-start-read gr:*random-structure* a7 dt-right-array-and-left-structure boxed-vma boxed-md)
;      (open-call (aref-quick 1) a8 (o0 a9))
;
; vma - array pointer argument
; a0  - index argument
; a1  - array pointer from vma
; a2  - bounds
; a3  - scaled index
; a4  - data temp
; a5  - bignum size
; a6  - bignum ptr

(defafun AREF-QUICK (index)
  (alu-field field-extract-r nop a0 a0 (byte 3. -21.) dt-both-fixnum)
  (alu-field field-xor nop gr:*zero* md (byte 5. 21.) br-not-zero)
  (branch not-easy-index (alu l-r nop a0 md bw-24 br-not-equal))
  (branch not-fast-q (alu load-status-r nop ignore gr:*data-type* bw-16 br-not-negative))
  (branch bounds-fail (alu aligned-field-pass-right a1 gr:*dtp-locative* vma pw-rr))
fast-q
  (alu l+r+c vma-start-read a0 a1 bw-24 carry-1 boxed-vma boxed-md)
  (nop)
  (return md boxed)
not-fast-q
  (alu r+1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break* boxed-right)
  (movea a2 (svref-dispatch 1) boxed)
  (alu-field field-extract-r a3 ignore md (byte 5. -21.) unboxed)
  (alu l+r nop a2 a3)
  (alu-field field-extract-r a2 ignore md (byte 21. 0.) unboxed)
  (alu l-r nop a0 a2 bw-24 next-pc-dispatch)
bounds-fail
not-easy-index
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "AREF-QUICK bad subscript (vma subscript)" boxed ch-open)
  (move o1 vma)
  (call (li:error 3) ignore (o2 a0))
  (nop)
  (return a0 boxed) ;;random return value.
 )


(defafun SVREF-DISPATCH (index)
  (unconditional-branch sv-art-q                () br-greater-or-equal)
  (unconditional-branch sv-art-1b               () br-greater-or-equal)
  (unconditional-branch sv-art-2bs              () br-greater-or-equal)
  (unconditional-branch sv-art-2b               () br-greater-or-equal)
  (unconditional-branch sv-art-4bs              () br-greater-or-equal)
  (unconditional-branch sv-art-4b               () br-greater-or-equal)
  (unconditional-branch sv-art-8bs              (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-8b               (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-16bs             (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-16b              (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-32bs             () br-greater-or-equal)
  (unconditional-branch sv-art-32b              () br-greater-or-equal)
  (unconditional-branch sv-art-string           (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-fat-string       (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-single-float     (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch sv-art-double-float     (alu shift-dn-0f-r a3 ignore a0 br-greater-or-equal))
  (unconditional-branch broken                  ()) ;16
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ()) ;20
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ()) ;24
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch broken                  ())
  (unconditional-branch sv-art-q                () br-greater-or-equal) ;28
  (unconditional-branch sv-art-q                () br-greater-or-equal)
  (unconditional-branch sv-art-q                () br-greater-or-equal)
  (move a1 a0)                     ;31
  (jump (aref-hard 2) (a0 vma))    ; (aref-hard array index)

sv-art-q
  (branch bounds-fail (alu aligned-field-pass-right a2 gr:*dtp-locative* vma pw-rr boxed-right))
  (alu l+r+c vma-start-read a0 a2 carry-1 bw-24 boxed-vma boxed-md)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (return md boxed)

sv-art-string
  (branch bounds-fail (alu shift-dn-0f-r a3 ignore a3 br-greater-or-equal))
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 2. 3.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 8. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*dtp-character* (byte 8. 0.) boxed ch-return next-pc-return)

sv-art-fat-string
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 1. 4.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 16. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*dtp-character* (byte 16. 0.) boxed ch-return next-pc-return)

sv-art-32b
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a0 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-extract-r nop ignore md (byte 9. -23.))
  (alu-field field-extract-r nop ignore md (byte 1. -31.) br-zero)
  (branch fixnum () br-zero)
  (branch bignum-1 (alu setr o0 ignore gr:*all-zero* unboxed ch-tail-open))
 bignum-2
  (tail-call (make-bignum-64 2) (o1 md))
 bignum-1
  (tail-call (make-bignum-32 1) (o0 md))
 fixnum
  (alu merge-r return gr:*zero* md boxed  bw-24 ch-return next-pc-return)

sv-art-32bs
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a0 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-extract-r nop ignore md (byte 9. -23.))
  (alu-field field-xor nop gr:*all-ones* md (byte 9. 23.) br-zero)
  (branch fixnum () br-zero)
  (branch fixnum ())
  (tail-open-call (make-bignum-32 1) (o0 md))

sv-art-16b
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 1. 4.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 16. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 16. 0.) boxed ch-return next-pc-return)

sv-art-16bs
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 1. 4.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 16. 0.) pw-ri)
  (alu sex-r a4 ignore a4 unboxed bw-16)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 16. 0.) boxed ch-return next-pc-return)

sv-art-8b
  (branch bounds-fail (alu shift-dn-0f-r a3 ignore a3 br-greater-or-equal))
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 2. 3.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 8. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 8. 0.) boxed ch-return next-pc-return)

sv-art-8bs
  (branch bounds-fail (alu shift-dn-0f-r a3 ignore a3 br-greater-or-equal))
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 2. 3.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 8. 0.) pw-ri)
  (alu sex-r a4 ignore a4 unboxed bw-8)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 8. 0.) boxed ch-return next-pc-return)

sv-art-4b
  (branch bounds-fail ())
  (alu-field field-extract-r a3 ignore a0 (byte 21. -3.) unboxed)
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 3. 2.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 4. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 4. 0.) boxed ch-return next-pc-return)

sv-art-4bs
  (branch bounds-fail ())
  (alu-field field-extract-r a3 ignore a0 (byte 21. -3.) unboxed)
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 3. 2.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 4. 0.) pw-ri)
  (alu-field nb-shift-ar-r a4 ignore a4 (byte 0. 28.) unboxed)
  (alu-field nb-shift-ar-r a4 ignore a4 (byte 0. -28.) unboxed)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 4. 0.) boxed ch-return next-pc-return)

sv-art-2b
  (branch bounds-fail ())
  (alu-field field-extract-r a3 ignore a0 (byte 20. -4.) unboxed)
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 4. 1.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 2. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 2. 0.) boxed ch-return next-pc-return)

sv-art-2bs
  (branch bounds-fail ())
  (alu-field field-extract-r a3 ignore a0 (byte 20. -4.) unboxed)
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 4. 1.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 2. 0.) pw-ri)
  (alu-field nb-shift-ar-r a4 ignore a4 (byte 0. 30.) unboxed)
  (alu-field nb-shift-ar-r a4 ignore a4 (byte 0. -30.) unboxed)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 2. 0.) boxed ch-return next-pc-return)

sv-art-1b
  (branch bounds-fail ())
  (alu-field field-extract-r a3 ignore a0 (byte 19. -5.) unboxed)
  (alu l+r+c vma-start-read-no-transport a3 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu-field field-extract-r a3 ignore a0 (byte 5. 0.) unboxed)
  (alu neg-r a3 ignore a3 bw-8)
  (alu load-status-r nop ignore a3 bw-8)
  (alu-field field-extract-r a4 ignore md (byte 1. 0.) pw-ri)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (alu-field field-pass return a4 gr:*zero* (byte 1. 0.) boxed ch-return next-pc-return)

sv-art-single-float
  (branch bounds-fail ())
  (alu l+r+c vma-start-read-no-transport a0 vma bw-24 unboxed-vma unboxed-md carry-1)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (tail-open-call (make-single-float 1) (o0 md))

sv-art-double-float
  (branch bounds-fail (alu shift-up-0f-r a3 ignore a0 bw-24))
  (move a1 vma)
  (alu l+r+c vma-start-read-no-transport a3 a1 bw-24 unboxed-vma unboxed-md carry-1)
  (alu r+1 a3 ignore a3 bw-24)
  (move o1 md ch-tail-open)
  (alu l+r+c vma-start-read-no-transport a3 a1 bw-24 unboxed-vma unboxed-md carry-1)
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (tail-call (make-double-float 2) (o1 md))

bounds-fail
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "SVREF-DISPATCH bad subscript (vma index)" boxed ch-open)
  (move o1 vma)
  (call (li:error 3) ignore (o2 a0))
  (nop)
  (return a0) ;;ignore
broken
  (alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  (movei o0 "SVREF-DISPATCH bad array type (vma index)" boxed ch-open)
  (move o1 vma)
  (call (li:error 3) ignore (o2 a0))
  (nop)
  (return a0) ;;ignore
 )


;*******************************************************************************

;;; The real MAKE-STRING is in "STRINGS.LISP"
(defun MAKE-STRING-NO-INIT (length)
  (if ;;0 <= length < 2^21
      (hw:field= gr:*zero* length (byte 11. 21.))
      (cons:allocate-structure
        1
        (hw:ldb (+ length 3) (byte 22. 2.) 0)  ;divide by 4
        vinc:$$dtp-array
        (vinc::dpb-multiple-boxed
          length                               %%bounds
          art-string                           %%sv-art
          vinc:$$dtp-array-header-single vinc:%%data-type
          0))
    (li:tail-error "Bad size to make string" length)))

(defun MAKE-VECTOR (length)
  (if ;;0 <= length < 2^21
      (hw:field= gr:*zero* length (byte 11. 21.))
      (cons:allocate-structure
        (1+ length)
        0
        vinc:$$dtp-array
        (vinc::dpb-multiple-boxed
          length                               %%bounds
          art-q                               %%sv-art
          vinc:$$dtp-array-header-single vinc:%%data-type
          0))
    (li:tail-error "Bad size to make vector" length)))

(defun MAKE-1D-ARRAY (length &optional (type ART-Q) (area gr:*default-consing-area*))
  (if ;;0 <= length < 2^21
      (hw:field= gr:*zero* length (byte 11. 21.))
      (cons:allocate-structure-in-area
        (1+ length)
        0
        vinc:$$dtp-array
        (vinc::dpb-multiple-boxed
          length                              %%bounds
          type                                %%sv-art
          vinc:$$dtp-array-header-single vinc:%%data-type
          0)
        area)
    (li:tail-error "Bad size to make  1D array")))

(defun VECTOR (&rest things)
  (let* ((number-of-things (length things))
          (array (make-vector number-of-things)))
    (do ((i 0 (1+ i))
          (remaining-things things (li:cdr remaining-things)))
         ((null remaining-things) array)
      (setf (svref array i) (li:car remaining-things)))))

;*******************************************************************************


(defafun READ-AND-LOCK-ARRAY-HEADER (array)
  (alu r+1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break* bw-24 boxed-right)
  (alu setr vma-start-read gr:*random-structure* a0 boxed-vma boxed-md dt-right-array-and-left-structure)
  (nop)
  (return md))

(defsubst UNLOCK-ARRAY ()
  (setq gr:*allow-sequence-break* (1- gr:*allow-sequence-break*))
 ;This should be below instruction, but unfortunately, the defsubst probably would work right.
 ;(alu l-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*request-sequence-break* bw-24 boxed-left dt-right-list)
  )

(defsubst LOCK-ARRAY ()
  (setq gr:*allow-sequence-break* (1+ gr:*allow-sequence-break*)))


;*******************************************************************************

(defun make-bignum-32 (n)
  (let ((ptr (cons:allocate-structure
               1 1 $$dtp-bignum
               (cons:make-header $$dtp-unboxed-header 1))))
    (%vm-write32 ptr 1 n)
    ptr))

(defun make-bignum-64 (n-hi n-lo)
  (let ((ptr (cons:allocate-structure
               1 2 $$dtp-bignum
               (cons:make-header $$dtp-unboxed-header 2))))
    (%vm-write32 ptr 1 n-lo)
    (%vm-write32 ptr 2 n-hi)
    ptr))

(defun make-single-float (n)
  (let ((ptr (cons:allocate-structure
               1 1 $$dtp-single-float
               (cons:make-header $$dtp-unboxed-header 1))))
    (%vm-write32 ptr 1 n)
    ptr))

(defun make-double-float (n-hi n-lo)
  (let ((ptr (cons:allocate-structure
               1 2 $$dtp-double-float
               (cons:make-header $$dtp-unboxed-header 2))))
    (%vm-write32 ptr 1 n-lo)
    (%vm-write32 ptr 2 n-hi)
    ptr))

(defun bad-subscript ()
  (li:tail-error "Bad subscript"))

(defun bad-data ()
  (li:tail-error "Incorrect data for this array type"))

;*******************************************************************************


(defsubst array-test (x thunk)
  ;; Returns NIL if X is not an array, Calls THUNK on primary header if x is an array.
  (and (arrayp x)
       (prog1
         (funcall thunk (read-and-lock-array-header x))
         (unlock-array))))

(defun stringp (x)
  (array-test x
    #'(lambda (header1)
          (or
            (hw:field=
              (hw:unboxed-constant #.(lisp:ash art-string (byte-position %%sv-art)))
              header1 %%sv-art)
            (and
              (hw:field= (hw:unboxed-constant #.(lisp:ash $$dtp-array-header-multiple 26.)) header1 vinc:%%data-type)
              (let ((header2 (progn
                               (hw:vma-start-read-vma-unboxed-md-boxed (hw:32-1- x))
                               (hw:read-md))))
                (and
                  (= 1 (hw:ldb header2 %%dimensions 0))
                  (hw:field=
                    (hw:unboxed-constant #.(lisp:ash art-string (byte-position %%array-type)))
                    header2 %%array-type))))))))


(defun vectorp (x)
  (array-test x
    #'(lambda (header1)
          (or
            (hw:field=  header1 (hw:unboxed-constant #.(lisp:ash $$dtp-array-header-single
                                                                 (byte-position vinc:%%data-type)))
                        vinc:%%data-type)
            (let ((header2 (progn
                             (hw:vma-start-read-vma-unboxed-md-boxed (hw:32-1- x))
                             (hw:read-md))))
              (= 1 (hw:ldb header2 %%dimensions 0)))))))


(defun simple-vector-p (x)
  (array-test x
    #'(lambda (header1)
        (hw:field=  header1 (hw:unboxed-constant #.(lisp:ash $$dtp-array-header-single 26.))
                    vinc:%%data-type))))


(defun simple-string-p (x)
  (array-test x
    #'(lambda (header1)
        (hw:field=
          (vinc:dpb-multiple-unboxed
              art-string                %%sv-art
              $$dtp-array-header-single vinc:%%data-type
              0)
            x (byte 11. 21.)))))

(defun %hard-header-p (locked-header) ;;@@@ Turn into a macro for speed.  --wkf
  (= art-hard (hw:ldb locked-header %%sv-art 0)))

(defun %header-bounds (locked-header) ;;@@@ Turn into a macro for speed. --wkf
  (hw:ldb locked-header %%bounds 0))

(defun %array-length (array)
  (array-test array
     #'(lambda (header1)
         (if (and (not (%hard-header-p header1))
                  (hw:field=
                    #.(hw:unboxed-constant
                        (lisp:ash $$dtp-array-header-single (byte-position vinc:%%data-type)))
                    header1 vinc:%%data-type))
             (%header-bounds header1)
           (let ((header2 (%vm-read (hw:24-1- array))))
             (unless (= 1 (hw:ldb header2 %%dimensions 0))
               (li:error "Length called on non one-dimensional array" array header1 header2))
             (if (hw:32logbitp (byte-position %%fill-pointer-p) header2)
                 (fill-pointer array)
               (%header-bounds header1)))))))

(defun %string-length (string)  ;;@@@ This can be optimized better.  --wkf
  (cond ((%array-length string))
        (t (li:tail-error "Not an array to %string-length" string))))

(defun length (seq)
  (cond
    ((%array-length seq))
    ((hw:field= gr:*dtp-cons* seq vinc:%%data-type)
     (labels
       ((list-len (list len)
                  (if list
                      (list-len (cons:cdr list) (1+ len))
                    len)))
       (list-len (cons:cdr seq) 1)))
    ((null seq) 0)
    (t (li:tail-error "Arg to LENGTH not a sequence" seq))))

(defun %string= (s1 s2)
  (if (and (stringp s1) (stringp s2))
      (let ((len (%string-length s1)))
        (and (= len (%string-length s2))
             (dotimes (i len t)
               (unless (li:%char= (svref s1 i) (svref s2 i))
                 (return-from %string= nil)))))
    (li:tail-error "Not a string to %string=" 'in_%string= s1 s2)))

;; Don't just comment these two functions out...STRING-COMPARE uses them!  --Jim
;;
(defun %fast-string= (string1 start1 string2 start2 count)
  (do ((index1 start1 (+ 4 index1))
       (index2 start2 (+ 4 index2))
       (n count (- n 4)))
      ((<= n 0) count)
    (let ((c1 (%fast-get-4-chars string1 index1 n))
          (c2 (%fast-get-4-chars string2 index2 n)))
    (cond
      ((hw:32= c1 c2))
      ((not (hw:field= c1 c2 (byte 8.  0.))) (return (- count n)))
      ((not (hw:field= c1 c2 (byte 8.  8.))) (return (+ 1 (- count n))))
      ((not (hw:field= c1 c2 (byte 8. 16.))) (return (+ 2 (- count n 2))))
      (t (return (+ 3 (- count n ))))))))

(defun %fast-get-4-chars (string index count)
  (let ((offset (1+ (hw:ldb index (byte 22. 2.) 0))))
    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:24+ offset string) 0)
    (let ((skew (hw:ldb index (byte 2. 0.) 0)))
      (if (zerop skew)
          (if (>= count 4)
              (hw:read-md)
            (hw:field-extract-64 (hw:unboxed-constant 0) (hw:read-md)
                                 (hw:dpb count (byte 2. 11.) 0))) ; (byte (* count 8) 0)
        (if (>= count 4)
            (let ((hi (hw:read-md)))
              (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:24+ (1+ offset) string) 0)
              (let ((byte-spec (hw:dpb skew (byte 2. 3.) 0)))
                (hw:field-extract-64 (hw:read-md) hi byte-spec)))
          (let ((hi (hw:read-md)))
            (if (>= (- 4 count) skew)
                (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:24+ (1+ offset) string) 0)
              (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed nil 0))
            (let ((byte-spec (hw:dpb count (byte 2. 11.)
                                     (hw:dpb skew (byte 2. 3.) 0))))
              (hw:field-extract-64 (hw:read-md) hi byte-spec))))))))

(defun control-pdl-p (x)
  (array-test x
    #'(lambda (header1)
          (or
            (hw:field=
              (hw:unboxed-constant #.(lisp:ash art-control-pdl (byte-position %%sv-art)))
              header1 %%sv-art)
            (and
              (hw:field= (hw:unboxed-constant #.(lisp:ash $$dtp-array-header-multiple 26.)) header1 vinc:%%data-type)
              (let ((header2 (progn
                               (hw:vma-start-read-vma-unboxed-md-boxed (hw:32-1- x))
                               (hw:read-md))))
                (and
                  (= 1 (hw:ldb header2 %%dimensions 0))
                  (hw:field=
                    (hw:unboxed-constant #.(lisp:ash art-control-pdl (byte-position %%array-type)))
                    header1 %%array-type))))))))

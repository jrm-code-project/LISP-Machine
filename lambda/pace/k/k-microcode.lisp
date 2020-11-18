;;; -*- Mode:LISP; Package:MICRO; Base:10; Readtable:CL -*-

eh:
(eh:def-ucode-format-error something-was-wrong
  "Something was wrong ~s"
  (and (second ete) (sg-contents sg (second ete)))
  )

(define-micro-function %u-k-open (locf-o0)
  (declare (:call-as-misc-instruction t))
  ;;(locf o0)
  ((m-a) pdl-pop)

  ;;put base of free frame into m-b

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot 'sim:*k-frame-stack-pointer*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read m-c) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  ((m-1) q-pointer md)

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot 'sim:*k-frames*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-pointer)) trap)
  (error-table something-was-wrong nil *k-frames* is not an array)

  ((vma-start-read) md)
  (check-page-read)
  (dispatch transport md)
  (call-if-bit-set (lisp-byte si:%%array-long-length-flag) md trap)
  (error-table something-was-wrong nil *k-frames* is a long array)
  ((m-tem) (lisp-byte si:%%array-index-length-if-short) md)
  (call-greater-or-equal m-1 a-tem trap)
  (error-table something-was-wrong nil k-frame overflow)
  ((vma-start-read) m+a+1 vma a-1)
  (check-page-read)
  (dispatch transport md)
  ((m-b) md)
  ((md) add m-1 (a-constant 1))
  ((md) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))
  ((vma-start-write) m-c)
  (check-page-write)
  (gc-write-test)

  ((m-k) m-a)
  (call get-pdl-buffer-index)
  ((pdl-index) m-k)

  ((vma) sub m-b (a-constant 1))
  ((m-1) (a-constant 16.))
  ((md) pdl-index-indirect)
copy-loop
  ((vma-start-write) add vma (a-constant 1))
  (check-page-write)
  ((pdl-index) add pdl-index (a-constant 1))
  ((m-1) sub m-1 (a-constant 1))
  (gc-write-test)
  (jump-not-equal-xct-next m-1 a-zero copy-loop)
 ((md) ldb q-typed-pointer pdl-index-indirect (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-index) sub pdl-index (a-constant 1))
  ((md-start-write) ldb pdl-index-indirect q-typed-pointer (a-constant (byte-value q-cdr-code cdr-nil)))
  (check-page-write)
  (gc-write-test)

  ((m-t) a-v-nil)
  (popj)
  )

(define-micro-function %u-k-call (function n-args return-dest locf-o0 locf-r0)
  (declare (:call-as-misc-instruction t))
  ((m-d) q-typed-pointer pdl-pop)
  ((m-c) q-pointer pdl-pop)
  ((m-b) q-typed-pointer pdl-pop)
  ((m-r) q-typed-pointer pdl-pop)
  ((m-a) q-typed-pointer pdl-pop)

  (call p3zero)

  ((pdl-push) m-a) ;function
  ((pdl-push) m-b) ;return destination
  ((pdl-push) m-d) ;locf-r0

  ((m-1) add pdl-pointer (a-constant 16.))
  ((pdl-index) add m-1 (a-constant 1))
  ((pdl-pointer) sub m-c a-pdl-buffer-virtual-address)
  ((pdl-pointer) add pdl-pointer a-pdl-buffer-head)
  ((pdl-pointer) add pdl-pointer (a-constant 15.))

  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-pointer) m-1)
  ((pdl-push) dpb m-r q-typed-pointer (a-constant (byte-value q-cdr-code cdr-nil)))

  ;;get saved open frame into m-a

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot 'sim:*k-frame-stack-pointer*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  ((m-1) q-pointer md)
  (call-less-or-equal m-1 a-zero trap)
  (error-table something-was-wrong nil k-frame underflow)
  ((m-1) sub m-1 (a-constant 1))
  ((md-start-write) q-pointer m-1 (a-constant (byte-value q-data-type dtp-fix)))
  (check-page-write)
  (gc-write-test)

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot 'sim:*k-frames*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-pointer)) trap)
  (error-table something-was-wrong m-t *k-frames* is not an array)

  ((vma-start-read) m+a+1 md a-1)
  (check-page-read)
  (dispatch transport md)
  ((vma) sub md (a-constant 1))

  ((pdl-index) sub m-c a-pdl-buffer-virtual-address)
  ((pdl-index) add pdl-index a-pdl-buffer-head)

  ((m-1) (a-constant 16.))
  ((pdl-index) sub pdl-index (a-constant 1))
copy-loop
  ((vma-start-read) add vma (a-constant 1))
  (check-page-read)
  ((pdl-index) add pdl-index (a-constant 1))
  ((m-1) sub m-1 (a-constant 1))
  (dispatch transport md)
  (jump-not-equal-xct-next m-1 a-zero copy-loop)
 ((pdl-index-indirect) md)

  ((arg-call mmcall) (i-arg 20.))               ;i-arg is not really used

  (popj-after-next (m-t) a-v-nil)
 (no-op)
  )

(define-micro-function %u-k-return (value act-frame-location return-frame-location return-destination)
  (declare (:call-as-misc-instruction t))
  ;;return-destination
  ((m-e) q-pointer pdl-pop)
  ;;return-frame-location
  ((m-d) q-pointer pdl-pop)
  ;;act-frame-location
  ((m-b) q-typed-pointer pdl-pop)
  ;;value
  ((m-c) q-typed-pointer pdl-pop)

  (call pdl-buffer-refill)                      ;clobbers m-1, m-2
  (jump-less-than m-d a-pdl-buffer-virtual-address not-in-pdl-buffer)

  ;;return-frame-location
  ((m-tem) sub m-d a-pdl-buffer-virtual-address)
  ((m-k) add m-tem a-pdl-buffer-head)
  ((pdl-index) add m-k (a-constant 16.))

  ((m-1) pdl-pointer)


  ;;act-frame-location
  ((m-k) m-b)
  (call get-pdl-buffer-index)
  ((pdl-pointer) add m-k (a-constant 15.))

  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-pointer) m-1)
  (jump write-return-dest)

not-in-pdl-buffer
;;copy m-b to m-d, m-b is in pdl, m-d must use vma path
  ((pdl-index) sub m-b a-pdl-buffer-virtual-address)
  ((pdl-index) add pdl-index a-pdl-buffer-head)
  ((vma) m-d)
  ((m-1) (a-constant 16.))
loop
  ((md-start-write) pdl-index-indirect)
  (check-page-write)
  (gc-write-test)
  ((vma) add vma (a-constant 1))
  ((pdl-index) add pdl-index (a-constant 1))
  ((m-1) sub m-1 (a-constant 1))
  (jump-not-equal m-1 a-zero loop)

write-return-dest
  ((md) m-c)
  (JUMP-LESS-THAN M-e A-PDL-BUFFER-VIRTUAL-ADDRESS real-write)
  ((M-TEM) SUB M-e A-PDL-BUFFER-VIRTUAL-ADDRESS)
  ((PDL-INDEX) ADD M-TEM A-PDL-BUFFER-HEAD)
  (popj-after-next (PDL-INDEX-INDIRECT) MD)
 ((m-t) a-v-nil)

real-write
   ((VMA-START-WRITE) M-e)
   (CHECK-PAGE-WRITE)
   (gc-write-test)
   (popj-after-next (m-t) a-v-nil)
  (no-op)
  )

(define-micro-function %u-k-t-call (function n-args locf-a0 locf-o0)
  (declare (:call-as-misc-instruction t))
  ((m-t) a-v-nil)
  (call-not-equal m-ap a-ipmark trap)
 (error-table something-was-wrong nil attempt to do t-call with an open frame)

  (call-data-type-equal m-fef (a-constant (byte-value q-data-type dtp-u-entry)) trap)
 (error-table something-was-wrong m-t not-dtp-u-entry)
  (call-if-bit-set (byte 1 0) m-flags trap)
 (error-table something-was-wrong m-t you have special bindings)

  ((m-tem) ldb (byte 8 24.) micro-stack-pntr-and-data)
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t i must be called d-ignore)

  ((m-c) q-typed-pointer pdl-pop) ;locf-o0
  ((m-b) q-typed-pointer pdl-pop) ;locf-a0
  ((m-r) q-typed-pointer pdl-pop) ;n-args
  ((m-a) q-typed-pointer pdl-pop) ;function


  ((pdl-index) add m-ap (a-constant (eval si:%lp-call-state)))
  ((m-t) pdl-index-indirect)
  ((m-tem) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-cls-self-map-provided 0)
                             (dpb -1 si:%%lp-cls-adi-present 0)))))
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t bad call state word)

  ((pdl-index) add m-ap (a-constant (eval si:%lp-exit-state)))
  ((m-t) pdl-index-indirect)
  ((m-tem) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-exs-micro-stack-saved 0)))))
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t bad exit state word)

  ((pdl-index) add m-ap (a-constant (eval si:%lp-entry-state)))
  ((m-t) pdl-index-indirect)
  ((m-tem) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-ens-lctyp 0)
                             ;;this is automatically set when you do (locf arg0) ...
                             ;;(dpb -1 si:%%lp-ens-unsafe-rest-arg 0)
                             (dpb -1 si:%%lp-ens-unsafe-rest-arg-1 0)
                             (dpb -1 si:%%lp-ens-environment-pointer-points-here 0)
                             ))))
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t bad entry state word)

  ((m-tem) ldb (lisp-byte si:%%lp-ens-num-args-supplied) pdl-index-indirect)
  ((m-t) pdl-index-indirect)
  (call-less-than m-tem (a-constant 19.) trap)
 (error-table something-was-wrong m-t wrong number of args)

  ((pdl-index) add m-ap (a-constant si:%lp-call-state))
  ((pdl-index-indirect) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-cls-delta-to-open-block 0)
                             (dpb -1 si:%%lp-cls-destination 0)
                             (dpb -1 si:%%lp-cls-delta-to-active-block 0)
                             (dpb -1 si:%%lp-cls-attention 0)
                             (dpb -1 si:%%lp-cls-trap-on-exit 0)
                             (dpb -1 %%q-data-type 0)
                             ))))
  ((pdl-index) add m-ap (a-constant si:%lp-exit-state))
  ((pdl-index-indirect) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-exs-pc-status 0)
                             (dpb -1 si:%%lp-exs-exit-pc 0)
                             (dpb -1 %%q-data-type 0)))))
  ((pdl-index) add m-ap (a-constant si:%lp-entry-state))
  ((pdl-index-indirect) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-ens-num-args-supplied 0)
                             (dpb -1 %%q-data-type 0)))))

  ((pdl-index) m-ap)
  ((pdl-index-indirect) m-a)
  ((m-fef) m-a)

  ;;locf-o0
  ((m-k) m-c)
  (call get-pdl-buffer-index)
  ((m-c) m-k)
  ;;locf-a0
  ((m-k) m-b)
  (call get-pdl-buffer-index)
  ((m-b) m-k)

  ((pdl-pointer) add m-c (a-constant 15.))
  ((pdl-index) add m-b (a-constant 16.))

  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-top) ldb q-typed-pointer m-r (a-constant (byte-value q-cdr-code cdr-nil)))

  ((pdl-index) sub pdl-pointer a-ap)                    ;number of args
  ((m-r) pdl-index)
  (dispatch-xct-next dispatch-write-vma q-data-type d-qmrcl m-a)
  (no-op)

  (call trap)
 (error-table something-was-wrong m-t how did you get here))

(define-micro-function %u-k-check-args (argspec)
  (declare (:call-as-misc-instruction t))
  ((m-t) a-v-nil)
  ;;get n-args
  ((pdl-index) add m-minus-one a-localp)
  ((m-1) q-pointer pdl-index-indirect)
  ((m-2) q-pointer pdl-pop)
  ((m-3) ldb (lisp-byte sim:%%k-rest-p) m-1)
  ((m-4) ldb (lisp-byte sim:%%k-rest-p) m-2)
  (call-not-equal m-3 a-4 trap)
  (error-table something-was-wrong m-t rest-arg-p disagreees)
  ((m-1) dpb m-zero (lisp-byte sim:%%k-rest-p) a-1)
  ((m-4) ldb (lisp-byte sim:%%k-max-args) m-2)
  (call-greater-than m-1 a-4 trap)
  (error-table something-was-wrong m-t too many arguments)
  ((m-4) ldb (lisp-byte sim:%%k-min-args) m-2)
  (call-less-than m-1 a-4 trap)
  (error-table something-was-wrong m-t too few arguments)
  (popj)
  )

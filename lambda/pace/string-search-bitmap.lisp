;;; -*- Mode:LISP; Package:MICRO; Base:10; Readtable:CL -*-

(eh:def-ucode-format-error eh:bad-arg "Bad Arg")


(define-micro-function string-search-bitmap (bitmap string start end)
  ((m-j) q-typed-pointer pdl-pop)               ;END, NIL means active length
  ((m-q) q-typed-pointer pdl-pop)               ;START
  (call-data-type-not-equal m-q (a-constant (byte-value q-data-type dtp-fix)) trap)
  (error-table bad-arg start is not fixnum)
  ((m-q) q-pointer m-q)
  (call xaaixl)                                 ;pop array, decode, active length in M-T
  ((m-i) q-pointer m-t)
  (jump-equal m-j a-v-nil string-search-bitmap-1)
  (call-data-type-not-equal m-j (a-constant (byte-value q-data-type dtp-fix)) trap)
  (error-table bad-arg end is not nil or fixnum)
  ((m-j) q-pointer m-j)
  (jump-greater-than m-j a-i string-search-bitmap-1)
  ((m-i) q-pointer m-j)
string-search-bitmap-1
  (call-if-bit-set (lisp-byte si:%%array-displaced-bit) m-array-header decode-displaced-array)

  ((m-a) q-typed-pointer pdl-pop)               ;BITMAP
  (call-data-type-not-equal m-a (a-constant (byte-value q-data-type dtp-array-pointer)) trap)
  (error-table bad-arg bitmap is not array)
  ((vma-start-read) m-a)
  (check-page-read)
  (dispatch transport m-a)
  ((m-tem) ldb (lisp-byte si:%%array-type-field) md)
  (call-not-equal m-tem (a-constant (eval (ash art-1b si:array-type-shift))) trap)
  (error-table bad-arg bitmap is not 1b array)
  (call-if-bit-set (lisp-byte si:%%array-displaced-bit) md trap)
  (error-table bad-arg bitmap is displaced)
  ((m-tem) ldb (lisp-byte si:%%array-number-dimensions) md)
  (call-not-equal m-tem (a-constant 1) trap)
  (error-table bad-arg bitmap is not one dimensional)
  (call-if-bit-set (lisp-byte si:%%array-long-length-flag) md trap)
  (error-table bad-arg bitmap is too long)
  ((m-tem) ldb (lisp-byte si:%%array-index-length-if-short) md)
  (call-not-equal m-tem (a-constant 256.) trap)
  (error-table bad-arg bitmap is not 256bits long)

;M-Q has index to fetch from.
;M-I has index of end
  ((m-b) a-array-length)
  (jump-greater-or-equal m-q a-array-length string-search-bitmap-done)
  (jump-greater-than m-i a-array-length string-search-bitmap-done)
string-search-bitmap-loop
  (dispatch-xct-next (lisp-byte si:%%array-type-field) m-array-header array-type-ref-dispatch)
 (no-op)
  (error-table bad-array-type m-array-header)
  ;;m-t is the next character
  ((m-tem) ldb (byte 3 5) m-t)
  ((vma-start-read) m+a+1 m-a a-tem)
  (check-page-read)
  ((m-tem) ldb (byte 5 0) m-t)
  ((m-1) (a-constant 32.))
  ((m-tem) sub m-1 a-tem)
  ((oa-reg-low) dpb m-tem oal-mrot a-zero)
  (jump-if-bit-set (byte 1 32.) md string-search-bitmap-done)
  ((m-q) add m-q (a-constant 1))
  (jump-less-than m-q a-i string-search-bitmap-loop)
  ((m-t) a-v-nil)
  (popj)
string-search-bitmap-done
  ((m-t) ldb m-q q-pointer (a-constant (byte-value q-data-type dtp-fix)))
  (popj)
  )

(defvar *all-ones* (make-array 256.
                               :type art-1b
                               :initial-element 1))

(defvar *all-zeros* (make-array 256.
                                :type art-1b
                                :initial-element 0))


(defvar *x* (let ((x (make-array 256.
                                 :type art-1b
                                 :initial-element 0)))
              (aset 1 x #\x)
              x))

(defun stest (bitmap string)
  (string-search-bitmap bitmap string 0 nil))

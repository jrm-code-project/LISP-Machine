;;; -*- Mode:LISP; Package:NEW-MATH; Base:10; Readtable:ZL -*-

(defun foo ()
  (li:break (ash-bignum #x1000000 2) #x1000000))

(defafun ash-bignum (num shift)
  (alu-field aligned-field-xor nop gr:*zero* a1 vinc:%%data-type) ;only fixnum shifts are legal
  (move vma-start-read a0 boxed-vma boxed-md br-zero)
  (branch got-shift-size (alu sex-r a1 ignore a1 bw-24 unboxed))
bad-ash
  (open-call (big-ash-illop 0) ignore ())
got-shift-size
  (alu setr nop a1 a1 bw-24 boxed dt-none br-negative) ;;zero test
  (branch shift-down (alu merge-r a3 gr:*zero* MD bw-24 boxed br-not-zero))
  (branch shift-up (alu l+r vma-start-read-no-transport a3 a0 bw-24 unboxed-vma unboxed-md)) ;read top source word of bignum
zero-shift
  (movei (register *number-of-return-values* 11 15) (quote 2) boxed)
  (alu-field set-bit-right nop ignore md (byte 1 0))                                         ;;top source word sign to status
  (alu pass-status (register *return-0* 10 0) ignore a3 bw-24 boxed)     ;; Get fixnum datatype from a3
  (move return-mv a0 boxed-right ch-return next-pc-return)
shift-up
  (movei a4 31. unboxed) ;allocate (+ old-length (ash (+ shift 31.) -5)) words for result
  (alu l+r a4 a1 a4 unboxed)
  (alu-field nb-shift-ar-r a4 ignore a4 (byte 32. -5.) unboxed)
  (alu l+r a4 a4 a3 bw-24 boxed)
  (move a6 a4)  ; result index
  (move a7 a3)  ; source index
  (move a10 md) ; top source word
  (alu sign a9 ignore ignore unboxed) ;sign extension of source
  (open-call (allocate-bignum 1) a5 (o0 a4))
;;  ||| removed 10/18/88 --wkf (open-call (zero-bignum-internal 1) ignore (o0 a5))      ;;;@@@ Do we need to zero it?
  (alu-field field-extract-r a2 ignore a1 (byte 5. 0.) unboxed) ;shift mod 32
  (movei a11 32. unboxed) ;compute extract constant
  (alu l-r a11 a11 a2 bw-8 unboxed)
  (alu neg-r a11 ignore a11 bw-8 unboxed)
  (alu load-status-r nop ignore a11 bw-16)
shift-up-loop
  (alu-field field-extract-lr md a9 a10 (byte 32. 0.) pw-ri unboxed-md)
  (alu l+r vma-start-write-no-gc-trap a6 a5 bw-24 unboxed-vma)
  (alu r-1 a7 ignore a7 bw-24)
  (alu r-1 a6 ignore a6 br-zero bw-24)
  (branch shift-up-finish (alu l+r vma-start-read-no-transport a7 a0 bw-24 unboxed-vma unboxed-md))
  (move a9 a10 unboxed)
  (unconditional-branch shift-up-loop (alu setr a10 ignore md unboxed))
shift-up-finish
  (alu-field field-extract-lr md a10 gr:*all-zero* (byte 32. 0.) pw-ri unboxed-md)
  (alu l+r vma-start-write-no-gc-trap a6 a5 bw-24 unboxed-vma)
  (tail-open-call (shrink-bignum-structure 1) (o0 a5))
shift-down
  (alu neg-r a1 ignore a1 unboxed)
  (alu-field nb-shift-ar-r a4 ignore a1 (byte 32. -5.) unboxed)
  (alu r-l a4 a4 a3 bw-24 boxed)
  (test br-greater-than)
  (branch shift-down-allocate ())
shift-down-till-nothing-left
  (alu l+r vma-start-read-no-transport a3 a0 bw-24 unboxed-vma unboxed-md)
  (movei a15 '5)
  (alu-field nb-shift-ar-r a10 ignore md (byte 32. -32.) unboxed)
  (alu pass-status a14 gr:*zero* gr:*zero* bw-24)
  (alu merge-r return gr:*zero* a10 bw-24 boxed ch-return next-pc-return)
shift-down-allocate
  (open-call (%allocate-bignum 1) a5 (o0 a4))
  (alu l+r vma-start-read-no-transport a3 a0 bw-24 unboxed-vma unboxed-md) ;read top source word
  (alu-field field-extract-r a2 ignore a1 (byte 5. 0.) unboxed) ;shift mod 32
  (alu neg-r a2 ignore a2 bw-8 br-greater-or-equal)
  (branch shift-down-more-setup ())
  (movei a2 '#x00e0)
shift-down-more-setup
  (alu load-status-r nop ignore a2 bw-16)
  (move a6 a4)  ; result index
  (move a7 a3)  ; source index
  (move a10 md) ; top source word
  (alu sign a9 ignore ignore unboxed) ;sign extension of source
shift-down-loop
  (alu-field field-extract-lr md a9 a10 (byte 32. 0.) pw-ri unboxed-md)
  (alu l+r vma-start-write-no-gc-trap a6 a5 bw-24 unboxed-vma)
  (alu r-1 a6 ignore a6 bw-24)
  (alu r-1 a7 ignore a7 br-zero bw-24)
  (branch shift-down-finish (alu l+r vma-start-read-no-transport a7 a0 bw-24 unboxed-vma unboxed-md))
  (move a9 a10 unboxed)
  (unconditional-branch shift-down-loop (alu setr a10 ignore md unboxed))
shift-down-finish
  (tail-open-call (shrink-bignum-structure 1) (o0 a5))
 )

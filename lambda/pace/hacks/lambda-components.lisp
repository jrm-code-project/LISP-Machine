;;; -*- Mode:LISP; Package:(MICRO GLOBAL); Base:8; Readtable:ZL -*-

(define-micro-component popj-after-next ()
  lam-ir-popj-after-next 1
  )

(define-micro-component no-op ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu)

(define-micro-component jump ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-unc
  lam-ir-n 1
  lam-ir-jump-addr :required
  )

(define-micro-component jump-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-unc
  lam-ir-jump-addr :required
  )

(define-micro-component call ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-unc
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  )

(define-micro-component call-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-jump-cond lam-jump-cond-unc
  lam-ir-jump-addr :required
  )

(define-micro-component popj ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-unc
  lam-ir-r 1
  lam-ir-n 1
  )

(define-micro-component popj-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-r 1
  lam-ir-jump-cond lam-jump-cond-unc
  )

(define-micro-component jump-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m=a
  lam-ir-n 1
  lam-ir-jump-addr :required
  )

(define-micro-component jump-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m=a
  lam-ir-jump-addr :required
  )

(define-micro-component call-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m=a
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  )

(define-micro-component call-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-jump-cond lam-jump-cond-m=a
  lam-ir-jump-addr :required
  )

(define-micro-component popj-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m=a
  lam-ir-r 1
  lam-ir-n 1
  )

(define-micro-component popj-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-r 1
  lam-ir-jump-cond lam-jump-cond-m=a
  )

(define-micro-component jump-not-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m-neq-a
  lam-ir-n 1
  lam-ir-jump-addr :required
  )

(define-micro-component jump-not-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m-neq-a
  lam-ir-jump-addr :required
  )

(define-micro-component call-not-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m-neq-a
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  )

(define-micro-component call-not-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-jump-cond lam-jump-cond-m-neq-a
  lam-ir-jump-addr :required
  )

(define-micro-component popj-not-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m-neq-a
  lam-ir-r 1
  lam-ir-n 1
  )

(define-micro-component popj-not-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-r 1
  lam-ir-jump-cond lam-jump-cond-m-neq-a
  )

(define-micro-component jump-less-than-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<a
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-less-or-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<=a
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-greater-than-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>a
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-greater-or-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>=a
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-less-than-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<a
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-less-or-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<=a
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-greater-than-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>a
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-greater-or-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>=a
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)


(define-micro-component popj-less-than-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<a
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-less-or-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<=a
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-greater-than-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>a
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-greater-or-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>=a
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-less-than ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<a
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-less-or-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<=a
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-greater-than ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>a
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-greater-or-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>=a
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-less-than ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<a
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-less-or-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<=a
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-greater-than ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>a
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-greater-or-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>=a
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-a-src :required
  lam-ir-m-src :required)


(define-micro-component popj-less-than ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<a
  lam-ir-n 1
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-less-or-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m<=a
  lam-ir-n 1
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-greater-than ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>a
  lam-ir-n 1
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-greater-or-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-m>=a
  lam-ir-n 1
  lam-ir-r 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-if-bit-set ()
  lam-ir-op lam-op-jump
  ;;leave jump-cond 0 to mean "if bit set"
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component jump-if-bit-set-xct-next ()
  lam-ir-op lam-op-jump
  ;;leave jump-cond 0 to mean "if bit set"
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component jump-if-bit-clear ()
  lam-ir-op lam-op-jump
  lam-ir-jump-invert-condition 1
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component jump-if-bit-clear-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-invert-condition 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component call-if-bit-set ()
  lam-ir-op lam-op-jump
  ;;leave jump-cond 0 to mean "if bit set"
  lam-ir-n 1
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component call-if-bit-set-xct-next ()
  lam-ir-op lam-op-jump
  ;;leave jump-cond 0 to mean "if bit set"
  lam-ir-p 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component call-if-bit-clear ()
  lam-ir-op lam-op-jump
  lam-ir-jump-invert-condition 1
  lam-ir-p 1
  lam-ir-n 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component call-if-bit-clear-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-jump-invert-condition 1
  lam-ir-jump-addr :required
  lam-ir-m-src :required)

(define-micro-component popj-if-bit-set ()
  lam-ir-op lam-op-jump
  ;;leave jump-cond 0 to mean "if bit set"
  lam-ir-n 1
  lam-ir-r 1
  lam-ir-m-src :required)

(define-micro-component popj-if-bit-set-xct-next ()
  lam-ir-op lam-op-jump
  ;;leave jump-cond 0 to mean "if bit set"
  lam-ir-r 1
  lam-ir-m-src :required)

(define-micro-component popj-if-bit-clear ()
  lam-ir-op lam-op-jump
  lam-ir-jump-invert-condition 1
  lam-ir-r 1
  lam-ir-n 1
  lam-ir-m-src :required)

(define-micro-component popj-if-bit-clear-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-invert-condition 1
  lam-ir-r 1
  lam-ir-m-src :required)

(define-micro-component jump-data-type-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-equal
  lam-ir-n 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-data-type-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-equal
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-data-type-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-equal
  lam-ir-p 1
  lam-ir-n 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-data-type-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-jump-cond lam-jump-cond-data-type-equal
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-data-type-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-equal
  lam-ir-r 1
  lam-ir-n 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-data-type-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-r 1
  lam-ir-jump-cond lam-jump-cond-data-type-equal
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-data-type-not-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-not-equal
  lam-ir-n 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component jump-data-type-not-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-not-equal
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-data-type-not-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-not-equal
  lam-ir-p 1
  lam-ir-n 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-data-type-not-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-jump-cond lam-jump-cond-data-type-not-equal
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-data-type-not-equal ()
  lam-ir-op lam-op-jump
  lam-ir-jump-cond lam-jump-cond-data-type-not-equal
  lam-ir-r 1
  lam-ir-n 1
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component popj-data-type-not-equal-xct-next ()
  lam-ir-op lam-op-jump
  lam-ir-r 1
  lam-ir-jump-cond lam-jump-cond-data-type-not-equal
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component call-if-page-fault-or-interrupt ()
  lam-ir-op lam-op-jump
  lam-ir-p 1
  lam-ir-n 1
  lam-ir-jump-cond lam-jump-cond-page-fault-or-interrupt
  lam-ir-jump-addr :required)

(define-micro-component vma ()
  lam-ir-m-src lam-m-src-vma
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component vma (destination)
  lam-ir-func-dest lam-func-dest-vma
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component vma-start-read (destination)
  lam-ir-func-dest lam-func-dest-vma-start-read
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component vma-start-write (destination)
  lam-ir-func-dest lam-func-dest-vma-start-write
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component md ()
  lam-ir-m-src lam-m-src-md
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component md (destination)
  lam-ir-func-dest lam-func-dest-md
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component md-start-read (destination)
  lam-ir-func-dest lam-func-dest-md-start-read
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component md-start-write (destination)
  lam-ir-func-dest lam-func-dest-md-start-write
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component pdl-pop ()
  lam-ir-m-src lam-m-src-c-pdl-buffer-pointer-pop)

(define-micro-component pdl-top ()
  lam-ir-m-src lam-m-src-c-pdl-buffer-pointer)

(define-micro-component pdl-top (destination)
  lam-ir-func-dest lam-func-dest-c-pdl-buffer-pointer)

(define-micro-component c-pdl-buffer-pointer ()
  lam-ir-m-src lam-m-src-c-pdl-buffer-pointer)

(define-micro-component pdl-pointer-indirect ()
  lam-ir-m-src lam-m-src-c-pdl-buffer-pointer)

(define-micro-component c-pdl-buffer-pointer (destination)
  lam-ir-func-dest lam-func-dest-c-pdl-buffer-pointer)

(define-micro-component pdl-pointer-indirect (destination)
  lam-ir-func-dest lam-func-dest-c-pdl-buffer-pointer)

(define-micro-component pdl-push (destination)
  lam-ir-func-dest lam-func-dest-c-pdl-buffer-pointer-push)

(define-micro-component c-pdl-buffer-index ()
  lam-ir-m-src lam-m-src-c-pdl-buffer-index)

(define-micro-component pdl-index-indirect ()
  lam-ir-m-src lam-m-src-c-pdl-buffer-index)

(define-micro-component c-pdl-buffer-index (destination)
  lam-ir-func-dest lam-func-dest-c-pdl-buffer-index)

(define-micro-component pdl-index-indirect (destination)
  lam-ir-func-dest lam-func-dest-c-pdl-buffer-index)

(define-micro-component pdl-buffer-pointer ()
  lam-ir-m-src lam-m-src-pdl-buffer-pointer)

(define-micro-component pdl-buffer-pointer (destination)
  lam-ir-func-dest lam-func-dest-pdl-buffer-pointer)

(define-micro-component pdl-buffer-index ()
  lam-ir-m-src lam-m-src-pdl-buffer-index)

(define-micro-component pdl-buffer-index (destination)
  lam-ir-func-dest lam-func-dest-pdl-buffer-index)

(define-micro-component pdl-pointer ()
  lam-ir-m-src lam-m-src-pdl-buffer-pointer)

(define-micro-component pdl-pointer (destination)
  lam-ir-func-dest lam-func-dest-pdl-buffer-pointer)

(define-micro-component pdl-index ()
  lam-ir-m-src lam-m-src-pdl-buffer-index)

(define-micro-component pdl-index (destination)
  lam-ir-func-dest lam-func-dest-pdl-buffer-index)

(define-micro-component setz ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-setz)

(define-micro-component and ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-and
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component andca ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-andca
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component setm ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-setm
  lam-ir-m-src :required)

(define-micro-component andcm ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf 4
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component seta ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-seta
  lam-ir-a-src :required)

(define-micro-component xor ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-xor
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component ior ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-ior
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component andcb ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o10
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component eqv ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o11
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component setca ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o12
  lam-ir-a-src :required)

(define-micro-component orca ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o13
  lam-ir-a-src :required)

(define-micro-component setcm ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o14
  lam-ir-m-src :required)

(define-micro-component orcm ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o15
  lam-ir-m-src :required)

(define-micro-component orcb ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf #o16
  lam-ir-a-src :required
  lam-ir-m-src :required)

(define-micro-component seto ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-seto)

(define-micro-component add ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-add
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component sub ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-sub
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component sub-m+1 ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-m-a-1
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component m+m ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-m+m
  lam-ir-m-src :required)

(define-micro-component m+1 ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-m+1
  lam-ir-m-src :required)

(define-micro-component m-a-1 ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-m-a-1
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component m+a+1 ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-m+a+1
  lam-ir-m-src :required
  lam-ir-a-src :required)

(define-micro-component m+m+1 ()
  lam-ir-op lam-op-alu
  lam-ir-ob lam-ob-alu
  lam-ir-aluf lam-alu-m+m+1
  lam-ir-m-src :required)



(define-micro-component ldb ()
  lam-ir-op lam-op-byte
  lam-ir-byte-func lam-byte-func-ldb
  lam-ir-m-src :required
  lam-byte-spec :required)

(define-micro-component dpb ()
  lam-ir-op lam-op-byte
  lam-ir-byte-func lam-byte-func-dpb
  lam-ir-m-src :required
  lam-byte-spec :required)

(define-micro-component selective-deposit ()
  lam-ir-op lam-op-byte
  lam-ir-byte-func lam-byte-func-selective-deposit
  lam-ir-m-src :required
  lam-byte-spec :required)

(define-micro-component dispatch ()
  lam-ir-op lam-op-dispatch
  lam-ir-dispatch-addr :required
  lam-byte-spec :required)

(define-micro-component gc-map-test ()
  lam-ir-slow-dest 1)

(define-micro-component clobbers-mem-subr ()
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component dispatch-write-vma ()
  lam-ir-disp-write-vma 1
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component alu-carry-in-one ()
  lam-ir-carry 1)

(define-micro-component output-selector-rightshift-1 ()
  lam-ir-ob lam-ob-alu-right-1)

(define-micro-component output-selector-leftshift-1 ()
  lam-ir-ob lam-ob-alu-left-1)

(define-micro-component shift-alu-right ()
  lam-ir-ob lam-ob-alu-right-1)

(define-micro-component shift-alu-left ()
  lam-ir-ob lam-ob-alu-left-1)

(define-micro-component output-selector-extend-25 ()
  lam-ir-ob lam-ob-alu-extend-25)

(define-micro-component output-selector-bit-mirror ()
  lam-ir-ob lam-ob-alu-mirror)

(define-micro-component output-selector-mask-25 ()
  lam-ir-ob lam-ob-alu
  lam-ir-alu-control-5 1)

(define-micro-component output-selector-mask-11 ()
  lam-ir-ob 5
  lam-ir-alu-control-5 1)

(define-micro-component shift-q-left ()
  lam-ir-q lam-q-left)

(define-micro-component shift-q-right ()
  lam-ir-q lam-q-right)

(define-micro-component halt-bit ()
  lam-ir-halt 1)

(define-micro-component i-long ()
  lam-ir-ilong 1)


(define-micro-component i-arg ()
  lam-ir-m-src lam-m-src-disp-const)

(define-micro-component micro-stack-pntr-and-data ()
  lam-ir-m-src lam-m-src-micro-stack)

(define-micro-component q-r ()
  lam-ir-m-src lam-m-src-q)

(define-micro-component q-r (destination)
  lam-ir-q lam-q-load)

(define-micro-component location-counter ()
  lam-ir-m-src lam-m-src-lc)

(define-micro-component location-counter (destination)
  lam-ir-func-dest lam-func-dest-lc)

(define-micro-component micro-stack-pntr-and-data-pop ()
  lam-ir-m-src lam-m-src-micro-stack-pop)

(defun (:property micro-stack-pointer micro-expand) (ignore)
  `(micro-stack-pntr-and-data (byte 8 24.)))

(defun (:property micro-stack-pointer micro-expand) (ignore)
  `(micro-stack-pntr-and-data-pop (byte 8 24.)))

(defun (:property micro-stack-data micro-expand) (ignore)
  `(micro-stack-pntr-and-data (byte 16. 0)))

(defun (:property micro-stack-data-pop micro-expand) (ignore)
  `(micro-stack-pntr-and-data-pop (byte 20. 0)))

(defun (:property micro-stack-pc-data micro-expand) (ignore)
  `(micro-stack-pntr-and-data (byte 16. 0)))

(defun (:property micro-stack-pc-data-pop micro-expand) (ignore)
  `(micro-stack-pntr-and-data-pop (byte 16. 0)))

(define-micro-component interrupt-pointer ()
  lam-ir-m-src lam-m-src-interrupt-pointer)

(define-micro-component macro-ir-displacement ()
  lam-ir-m-src lam-m-src-macro.ir.displacement)

(define-micro-component macro-ir ()
  lam-ir-m-src lam-m-src-macro.ir)

(define-micro-component stat-counter ()
  lam-ir-m-src lam-m-src-stat-counter)

(define-micro-component stat-counter (destination)
  lam-ir-func-dest lam-func-dest-stat-counter)

(define-micro-component stat-counter-aux ()
  lam-ir-m-src lam-m-src-stat-counter-aux)

(define-micro-component stat-counter-aux (destination)
  lam-ir-func-dest lam-func-dest-stat-counter-aux)

(define-micro-component macro-ir-decode ()
  lam-ir-m-src lam-m-src-macro.ir.decode.ram
  lam-ir-ilong 1)

(define-micro-component macro-ir-decode (destination)
  lam-ir-func-dest lam-func-dest-macro.ir.decode.ram
  lam-ir-slow-dest 1)

(define-micro-component spy-reg ()
  lam-ir-m-src lam-m-src-spy-reg)

(define-micro-component l1-map ()
  lam-ir-m-src lam-m-src-l1-map
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component l1-map (destination)
  lam-ir-func-dest lam-func-dest-l1-map
  lam-ir-slow-dest 1
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component l2-map-control ()
  lam-ir-m-src lam-m-src-l2-map-control
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component l2-map-control (destination)
  lam-ir-func-dest lam-func-dest-l2-map-control
  lam-ir-clobbers-mem-subr-bit 1
  lam-ir-slow-dest 1)

(define-micro-component l2-map-physical-page ()
  lam-ir-m-src lam-m-src-l2-map-physical-page
  lam-ir-clobbers-mem-subr-bit 1)

(define-micro-component l2-map-physical-page (destination)
  lam-ir-func-dest lam-func-dest-l2-map-physical-page
  lam-ir-clobbers-mem-subr-bit 1
  lam-ir-slow-dest 1)


(define-micro-component interrupt-clear (destination)
  lam-ir-func-dest lam-func-dest-interrupt-clear)

(define-micro-component rg-mode ()
  lam-ir-m-src lam-m-src-rg-mode)

(define-micro-component rg-mode (destination)
  lam-ir-func-dest lam-func-dest-rg-mode)

(define-micro-component dp-mode ()
  lam-ir-m-src lam-m-src-dp-mode)

(define-micro-component dp-mode (destination)
  lam-ir-func-dest lam-func-dest-dp-mode)

(define-micro-component micro-stack-data-push (destination)
  lam-ir-func-dest lam-func-dest-micro-stack-push)

(define-micro-component oa-reg-low (destination)
  lam-ir-func-dest lam-func-dest-imod-low)

(define-micro-component oa-reg-high (destination)
  lam-ir-func-dest lam-func-dest-imod-high)

(define-micro-component oa-reg-hi (destination)
  lam-ir-func-dest lam-func-dest-imod-high)

(defun (:property q-data-type micro-expand) (ignore)
  `((byte 5 25.)))

(defun (:property q-data-type-plus-one-bit micro-expand) (ignore)
  `((byte 6 24.)))

(defun (:property transport micro-expand) (ignore)
  `(d-transport i-not-linked oldspace-meta push-own-address (i-arg 1) q-data-type-plus-one-bit))


(define-micro-component i-not-linked ()
  lam-ir-this-instruction-not-linked-to-next 1)

(define-micro-component oldspace-meta ()
  lam-ir-disp-enable-oldspace-meta 1)

(define-micro-component push-own-address ()
  lam-ir-push-own-address 1)


(defun (:property check-page-read micro-macro) (form)
  form
  `((call-if-page-fault-or-interrupt pgf-r-i)))

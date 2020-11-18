;;; -*- Mode:LISP; Package:TEST; Base:10; Readtable:ZL -*-


(prims:define-global-frame trap)

(prims:define-global-variable trap save-oreg)
(prims:define-global-variable trap save-left)
(prims:define-global-variable trap save-right)
(prims:define-global-variable trap save-status)
(prims:define-global-variable trap save-jcond)
(prims:define-global-variable trap save-trap)
(prims:define-global-variable trap save-trap-pc)
(prims:define-global-variable trap save-trap-pc+)
(prims:define-global-variable trap trap-mask)
(prims:define-global-variable trap trap-temp)
(prims:define-global-constant trap trap-DTP-CODE)

(defafun trap-entry ()      ;;; at absolute location 0
  (alu setl save-oreg r0 r0 bw-32 boxed-left)                                   ;save OREG
  (alu setl save-left r0 r0 bw-32 boxed-left)                                   ;save LEFT
  (alu setr save-right r0 r0 bw-32 boxed-right)                                 ;save RIGHT
  (alu pass-status save-status r0 r0 bw-32 unboxed)                             ;save STATUS
  (alu-field extract-bit-right save-jcond r0 processor-status (byte 1. -16.) unboxed) ;save JCOND
  (alu-field field-and save-trap trap-mask trap-register (byte 31. 0.) unboxed) ;save TRAP reg
  (alu prioritize-r trap-temp r0 save-trap bw-32 unboxed)                       ;prioritize traps
  (alu-field set-bit-right trap-temp r0 trap-temp (byte 1. 5.) unboxed)         ;add 32.
  (alu merge-r save-trap-pc  trap-DTP-CODE trap-pc+ bw-24 boxed)
  (alu merge-r save-trap-pc+ trap-DTP-CODE trap-pc  bw-24 boxed next-pc-dispatch)
  (nop)
  (nop))

(defafun interrupt-exit ()   ;;; at absolute location 12.
  (alu-field field-pass processor-control save-jcond processor-control (byte 1. 4.))
  (alu load-status-r nop r0 save-status bw-32)
  (alu setl nop save-trap-pc  save-right  bw-32)
  (alu setl nop save-trap-pc+ save-right  bw-32)
  (move nop save-oreg bw-32 next-pc-dispatch br-jindir)
  (nop)
  (nop)
  (nop))

(defafun trap-exit ()         ;;; at absolute location 20.
  (alu-field field-pass processor-control save-jcond processor-control (byte 1. 4.))
  (alu load-status-r nop r0 save-status bw-32)
  (alu setl nop save-trap-pc  save-right bw-32)
  (alu setl nop save-trap-pc+ save-right bw-32)
  (move nop save-oreg bw-32 next-pc-dispatch br-jindir)
  (nop)
  (nop)
  (nop))

(defafun diag-trap-exit ()    ;;; at absolute location 28.
  (alu setl nop save-trap-pc  save-right bw-32)
  (alu setl nop save-trap-pc+ save-right bw-32)
  (move nop save-oreg bw-32 next-pc-dispatch)
  (nop))

(defafun trap-vector-table () ;;; at absolute location 32.
  (jump trap-reset ())                                  ;Bit 31 - addr 32 - Highest priority
  (jump trap-trace ())                                  ;Bit 30 - addr 33
trap-icache-parity
  (unconditional-branch trap-icache-parity ())          ;Bit 29 - addr 34
trap-icache-nubus-err
  (unconditional-branch trap-icache-nubus-err ())       ;Bit 28 - addr 35
trap-icache-nubus-timeout
  (unconditional-branch trap-icache-nubus-timeout ())   ;Bit 27 - addr 36
trap-icache-page-fault
  (unconditional-branch trap-icache-page-fault ())      ;Bit 26 - addr 37
trap-proc-mread-parity
  (unconditional-branch trap-proc-mread-parity ())      ;Bit 25 - addr 38
trap-proc-mread-nubus-err
  (unconditional-branch trap-proc-mread-nubus-err ())   ;Bit 24 - addr 39
trap-proc-mread-nubus-timeout
  (unconditional-branch trap-proc-mread-nubus-timeout ());Bit 23- addr 40
trap-proc-mread-page-fault
  (unconditional-branch trap-proc-mread-page-fault ())  ;Bit 22 - addr 41
trap-proc-mread-transporter
  (unconditional-branch trap-proc-mread-transporter ()) ;Bit 21 - addr 42
trap-proc-mwrite-nubus-err
  (unconditional-branch trap-proc-mwrite-nubus-err ())  ;Bit 20 - addr 43
trap-proc-mwrite-nubus-timeout
  (unconditional-branch trap-proc-mwrite-nubus-timeout ());Bit 19-addr 44
trap-proc-mwrite-page-fault
  (unconditional-branch trap-proc-mwrite-page-fault ()) ;Bit 18 - addr 45
trap-proc-mwrite-gc
  (unconditional-branch trap-proc-mwrite-gc ())         ;Bit 17 - addr 46
trap-floating-point
  (unconditional-branch trap-floating-point ())         ;Bit 16 - addr 47
trap-heap-empty
  (unconditional-branch trap-heap-empty ())             ;Bit 15 - addr 48
trap-spare14
  (unconditional-branch trap-spare14 ())                ;Bit 14 - addr 49
trap-datatype
  (unconditional-branch trap-datatype ())               ;Bit 13 - addr 50
trap-overflow
  (unconditional-branch trap-overflow ())               ;Bit 12 - addr 51
trap-spare11
  (unconditional-branch trap-spare11 ())                ;Bit 11 - addr 52
trap-interrupt7
  (unconditional-branch trap-interrupt7 ())             ;Bit 10 - addr 53
trap-interrupt6
  (unconditional-branch trap-interrupt6 ())             ;Bit 09 - addr 54
trap-interrupt5
  (unconditional-branch trap-interrupt5 ())             ;Bit 08 - addr 55
trap-interrupt4
  (unconditional-branch trap-interrupt4 ())             ;Bit 07 - addr 56
trap-interrupt3
  (unconditional-branch trap-interrupt3 ())             ;Bit 06 - addr 57
trap-interrupt2
  (unconditional-branch trap-interrupt2 ())             ;Bit 05 - addr 58
trap-interrupt1
  (unconditional-branch trap-interrupt1 ())             ;Bit 04 - addr 59
trap-interrupt0
  (unconditional-branch trap-interrupt0 ())             ;Bit 03 - addr 60
trap-timer-1024
  (unconditional-branch trap-timer-1024 ())             ;Bit 02 - addr 61
trap-timer-16384
  (unconditional-branch trap-timer-16384 ())            ;Bit 01 - addr 62
trap-spurious
  (unconditional-branch trap-spurious ()))              ;Bit 00 - addr 63

;-------------------------------------------------------------------

(prims:define-global-frame foobar)
(prims:define-global-variable foobar xtemp1)
(prims:define-global-variable foobar xtemp2)
(prims:define-global-variable foobar xtemp3)
(prims:define-global-variable foobar xtemp4)
(prims:define-global-variable foobar trace-handler)

;-------------------------------------------------------------------

(defafun trap-trace ()
  hang (unconditional-branch hang ()))

;-------------------------------------------------------------------

(defafun trap-reset ()
  (movei trap-mask #xffffffff)
  (movei xtemp4 0)
  (move processor-control xtemp4)
  (movei xtemp4 #x01040000)
  (move memory-control xtemp4)
  (nop)
  (nop)
  (nop)
  (nop)
  (jump ram-test ()))

;-------------------------------------------------------------------

(defafun ram-test ()
  (movei xtemp1 0)
  loop
  (alu r+1 xtemp1 xtemp1 xtemp1)
  (alu-field field-not memory-control xtemp1 memory-control hw:%%memory-control-leds)
  (movei a1 0)
  (alu-field field-pass processor-control a1 processor-control (byte 3. 0.))
  (nop)
  (nop)
  (movei a1 4)
  (alu-field field-pass processor-control a1 processor-control (byte 3. 0.))
  (open-call ram-atest 0 a0 ())

 hang
  (nop)
  (nop)
  (unconditional-branch loop ()))

;-------------------------------------------------------------------

; The funny branching is to guarantee no cache misses during the write pulse
;
(defafun write-map (page-number data)
  (alu-field field-extract-r vma r0 a0 (byte 16. 10.) unboxed-vma)
  (nop)
  (unconditional-branch wplus3 (alu setr memory-map  r0 a1))
 wplus1
  (nop)
  (return a1)
 wplus3
  (nop)
  (unconditional-branch wplus1 ()))

;-------------------------------------------------------------------

(defafun map-init () ;assumes 16Meg mem, 4 pages reserved for code
  (movei a10 #x00000085) ;code pages map constant
  (movei a11 #x0000008f) ;vma  pages map constant
  (movei a12 #x8000)     ;vma offset to PC space
  (movei a13 #x1000)     ;MAP inc for page size
  (movei a14 8192.)      ;Number of pages to map
  (movei a15 0)          ;Loop counter
 loop
  (move o0 a12 ch-open)   ;map code entries
  (call write-map 2 r0 (o1 a10))
  (move o0 a15 ch-open)  ;map vma entries
  (call write-map 2 r0 (o1 a11))
  (alu l+r a10 a10 a13)
  (alu l+r a11 a11 a13)
  (alu r+1 a15 r0 a15)
  (alu r+1 a12 r0 a12)
  (alu l-r nop-no-overflow-trap a15 a14)
  (test br-not-equal)
  (branch loop ())
  (return r0))

;-------------------------------------------------------------------

(defafun ram-atest (set)
  (open-call map-init 0 r0 ())

  (movei a5 31.)     ;rotate amount
 main-loop
  (movei a1 #x400000) ;end address
  (movei a0 #x2000) ;start address
  (alu load-status-r nop-no-overflow-trap r0 a5)
 fill-loop
  (alu rotate-r md r0 a0 pw-rr)
  (move vma-start-write a0)
  (alu r+1 a0 r0 a0)
  (alu l-r nop-no-overflow-trap a1 a0)
  (test br-not-equal)
  (branch fill-loop  ())

  (movei a1 #x400000) ;end address
  (movei a0 #x2000) ;start address
  (movei a6 32.)
  (alu l-r a6 a6 a5)
  (alu load-status-r nop-no-overflow-trap r0 a6)
 check-loop
  (move vma-start-read a0)
  (nop)
  (alu rotate-r a2 r0 md pw-rr)
  (alu l-r nop-no-overflow-trap a0 a2)
  (test br-not-equal)
  (branch fail ())
  (alu r+1 a0 r0 a0)
  (alu l-r nop-no-overflow-trap a0 a1)
  (test br-not-equal)
  (branch check-loop ())

  (alu r-1 a5 r0 a5)
  (test br-positive)
  (branch main-loop ())

  (return a0)

 fail
  (unconditional-branch fail ()))

;-------------------------------------------------------------------

(defun tmem ()
  "Test memory"
  (lam:k-setup)
  (lam:k-reset)
  (lam:k-init-virtual-memory)
  (lam:debug-write-word #xfcfff7fc 0)
  (kbug:load-fcns
    '(trap-entry interrupt-exit trap-exit diag-trap-exit trap-vector-table
      trap-trace trap-reset map-init write-map ram-atest ram-test) 0)
  (kbug:run 0))


(defafun wz ()
  (movei a0 #x100000)
  (movei md #x55555555)
  loop
   (move vma-start-write a0)
   (unconditional-branch loop ()))

;;; -*- Mode:LISP; Package:TRAP; Readtable:ZL; Base:10 -*-


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


(defafun trap-entry ()      ;;; at absolute location 0
  (alu setl save-oreg r0 r0 bw-32)                                              ;save OREG
  (alu setl save-left r0 r0 bw-32)                                              ;save LEFT
  (alu setr save-right r0 r0 bw-32)                                             ;save RIGHT
  (alu pass-status save-status r0 r0 bw-32)                                     ;save STATUS
  (alu-field extract-bit-right save-jcond r0 processor-status (byte 1. -16.))   ;save JCOND
  (alu-field field-and save-trap trap-mask trap-register (byte 31. 0.))         ;save TRAP reg
  (alu prioritize-r trap-temp r0 save-trap bw-32)                               ;prioritize traps
  (alu-field set-bit-right trap-temp r0 trap-temp (byte 1. 5.))                 ;add 32.
  (alu-field field-extract-r save-trap-pc r0 trap-pc  (byte 24. 0.))            ;save trap-pc
  (alu-field field-extract-r save-trap-pc+ r0 trap-pc+ (byte 24. 0.) next-pc-dispatch)
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
  (move nop save-oreg bw-32 next-pc-dispatch br-jindir)
  (nop))

(defafun trap-vector-table () ;;; at absolute location 32.
  (jump trap-reset ())          ;Bit 31 - Highest priority
trap-trace
  (unconditional-branch trap-trace ())                  ;Bit 30
trap-icache-parity
  (unconditional-branch trap-icache-parity ())          ;Bit 29
trap-icache-nubus-err
  (unconditional-branch trap-icache-nubus-err ())               ;Bit 28
trap-icache-nubus-timeout
  (unconditional-branch trap-icache-nubus-timeout ())   ;Bit 27
trap-icache-page-fault
  (unconditional-branch trap-icache-page-fault ())      ;Bit 26
trap-proc-mread-parity
  (unconditional-branch trap-proc-mread-parity ())      ;Bit 25
trap-proc-mread-nubus-err
  (unconditional-branch trap-proc-mread-nubus-err ())   ;Bit 24
trap-proc-mread-nubus-timeout
  (unconditional-branch trap-proc-mread-nubus-timeout ())       ;Bit 23
trap-proc-mread-page-fault
  (unconditional-branch trap-proc-mread-page-fault ())  ;Bit 22
trap-proc-mread-transporter
  (unconditional-branch trap-proc-mread-transporter ()) ;Bit 21
trap-proc-mwrite-nubus-err
  (unconditional-branch trap-proc-mwrite-nubus-err ())  ;Bit 20
trap-proc-mwrite-nubus-timeout
  (unconditional-branch trap-proc-mwrite-nubus-timeout ());Bit 19
trap-proc-mwrite-page-fault
  (unconditional-branch trap-proc-mwrite-page-fault ()) ;Bit 18
trap-proc-mwrite-gc
  (unconditional-branch trap-proc-mwrite-gc ())         ;Bit 17
trap-floating-point
  (unconditional-branch trap-floating-point ())         ;Bit 16
trap-heap-empty
  (unconditional-branch trap-heap-empty ())             ;Bit 15
trap-spare14
  (unconditional-branch trap-spare14 ())                        ;Bit 14
trap-datatype
  (unconditional-branch trap-datatype ())                       ;Bit 13
trap-overflow
  (unconditional-branch trap-overflow ())                       ;Bit 12
trap-spare11
  (unconditional-branch trap-spare11 ())                        ;Bit 11
trap-interrupt7
  (unconditional-branch trap-interrupt7 ())             ;Bit 10
trap-interrupt6
  (unconditional-branch trap-interrupt6 ())             ;Bit 9
trap-interrupt5
  (unconditional-branch trap-interrupt5 ())             ;Bit 8
trap-interrupt4
  (unconditional-branch trap-interrupt4 ())             ;Bit 7
trap-interrupt3
  (unconditional-branch trap-interrupt3 ())             ;Bit 6
trap-interrupt2
  (unconditional-branch trap-interrupt2 ())             ;Bit 5
trap-interrupt1
  (unconditional-branch trap-interrupt1 ())             ;Bit 4
trap-interrupt0
  (unconditional-branch trap-interrupt0 ())             ;Bit 3
trap-timer-1024
  (unconditional-branch trap-timer-1024 ())             ;Bit 2
trap-timer-16384
  (unconditional-branch trap-timer-16384 ())            ;Bit 1
trap-spurious
  (unconditional-branch trap-spurious ()))              ;Bit 0


(prims:define-global-variable trap trap-xtemp1)
(prims:define-global-variable trap trap-xtemp2)


(defafun trap-reset ()
 blinker
  (alu r+1 trap-xtemp1 trap-xtemp1 trap-xtemp1)
  (alu-field field-pass trap-xtemp2 trap-xtemp1 trap-xtemp2 (byte 3. -17.))
  (alu-field field-pass memory-control trap-xtemp2 memory-control hw:%%memory-control-leds)
  (nop)
  (unconditional-branch blinker ()))

(defun tt ()
  (lam:k-setup)
  (lam:k-init-virtual-memory)
  (kbug:load-fcns '(trap-entry interrupt-exit trap-exit diag-trap-exit trap-vector-table trap-reset) 0)
  (kbug:run 'trap-entry))

(defun daisy-lowcore ()
  (kbug:make-boot-prom-simulation '(trap-entry interrupt-exit trap-exit diag-trap-exit trap-vector-table trap-reset)))

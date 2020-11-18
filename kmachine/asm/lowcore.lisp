;;; -*- Mode:LISP; Package:TRAP; Base:10; Readtable:ZL -*-

;------------------------------------------------------------------------------------------------

(nc::defafun trap-entry ()      ;;; at absolute location 0
  (alu setl gr::*save-oreg* r0 r0 bw-32 boxed-left)
  (alu setl gr::*save-left* r0 r0 bw-32 boxed-left)
  (alu setr gr::*save-right* r0 r0 bw-32 boxed-right)
  (alu pass-status gr::*save-status* r0 r0 bw-32 unboxed)
  (alu-field extract-bit-right gr::*save-jcond* r0 processor-status (byte 1. -16.) unboxed)
  (alu-field field-and gr::*save-trap* gr::*trap-mask* trap-register (byte 31. 0.) unboxed)
  (alu prioritize-r gr::*trap-temp1* r0 gr::*save-trap* bw-32 unboxed)
  (alu-field set-bit-right gr::*trap-temp1* r0 gr::*trap-temp1* (byte 1. 5.) unboxed)
  (alu merge-r gr::*save-trap-pc*  gr::*trap-dtp-code* trap-pc+ bw-24 boxed)
  (alu merge-r gr::*save-trap-pc+* gr::*trap-dtp-code* trap-pc  bw-24 boxed next-pc-dispatch)
  (nop)
  (nop))

;------------------------------------------------------------------------------------------------

(nc::defafun interrupt-exit ()   ;;; at absolute location 12.
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl nop gr::*save-trap-pc*  gr::*save-right*  bw-32)
  (alu setl nop gr::*save-trap-pc+* gr::*save-right*  bw-32)
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch br-jindir)
  (nop)
  (nop)
  (nop))

;------------------------------------------------------------------------------------------------

(nc::defafun trap-exit ()         ;;; at absolute location 20.
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl nop gr::*save-trap-pc*  gr::*save-right* bw-32)
  (alu setl nop gr::*save-trap-pc+* gr::*save-right* bw-32)
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch br-jindir)
  (nop)
  (nop)
  (nop))

;------------------------------------------------------------------------------------------------

(nc::defafun diag-trap-exit ()    ;;; at absolute location 28.
  (alu setl nop gr::*save-trap-pc*  gr::*save-right* bw-32)
  (alu setl nop gr::*save-trap-pc+* gr::*save-right* bw-32)
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch)
  (nop))

;------------------------------------------------------------------------------------------------

(nc::defafun trap-vector-table () ;;; at absolute location 32.
trap-vector-reset                                       ;Bit 31 - addr 32 - Highest priority
  (jump trap-reset ())
trap-vector-trace                                       ;Bit 30 - addr 33
  (jump trap-spurious ())
trap-vector-icache-parity                               ;Bit 29 - addr 34
  (jump trap-spurious ())
trap-vector-icache-nubus-err                            ;Bit 28 - addr 35
  (jump trap-spurious ())
trap-vector-icache-nubus-timeout                        ;Bit 27 - addr 36
  (jump trap-spurious ())
trap-vector-icache-page-fault                           ;Bit 26 - addr 37
  (jump trap-spurious ())
trap-vector-proc-mread-parity                           ;Bit 25 - addr 38
  (jump trap-spurious ())
trap-vector-proc-mread-nubus-err                        ;Bit 24 - addr 39
  (jump trap-spurious ())
trap-vector-proc-mread-nubus-timeout                    ;Bit 23- addr 40
  (jump trap-spurious ())
trap-vector-proc-mread-page-fault                       ;Bit 22 - addr 41
  (jump trap-spurious ())
trap-vector-proc-mread-transporter                      ;Bit 21 - addr 42
  (jump trap-spurious ())
trap-vector-proc-mwrite-nubus-err                       ;Bit 20 - addr 43
  (jump trap-spurious ())
trap-vector-proc-mwrite-nubus-timeout                   ;Bit 19-addr 44
  (jump trap-spurious ())
trap-vector-proc-mwrite-page-fault                      ;Bit 18 - addr 45
  (jump trap-spurious ())
trap-vector-proc-mwrite-gc                              ;Bit 17 - addr 46
  (jump trap-spurious ())
trap-vector-floating-point                              ;Bit 16 - addr 47
  (jump trap-spurious ())
trap-vector-heap-empty                                  ;Bit 15 - addr 48
  (jump trap-spurious ())
trap-vector-spare14                                     ;Bit 14 - addr 49
  (jump trap-spurious ())
trap-vector-datatype                                    ;Bit 13 - addr 50
  (jump trap-spurious ())
trap-vector-overflow                                    ;Bit 12 - addr 51
  (jump trap-spurious ())
trap-vector-spare11                                     ;Bit 11 - addr 52
  (jump trap-spurious ())
trap-vector-interrupt7                                  ;Bit 10 - addr 53
  (jump trap-debugger ())
trap-vector-interrupt6                                  ;Bit 09 - addr 54
  (jump trap-spurious ())
trap-vector-interrupt5                                  ;Bit 08 - addr 55
  (jump trap-spurious ())
trap-vector-interrupt4                                  ;Bit 07 - addr 56
  (jump trap-spurious ())
trap-vector-interrupt3                                  ;Bit 06 - addr 57
  (jump trap-spurious ())
trap-vector-interrupt2                                  ;Bit 05 - addr 58
  (jump trap-spurious ())
trap-vector-interrupt1                                  ;Bit 04 - addr 59
  (jump trap-spurious ())
trap-vector-interrupt0                                  ;Bit 03 - addr 60
  (jump trap-spurious ())
trap-vector-timer-1024                                  ;Bit 02 - addr 61
  (jump trap-spurious ())
trap-vector-timer-16384                                 ;Bit 01 - addr 62
  (jump trap-spurious ())
trap-vector-spurious                                    ;Bit 00 - addr 63
  (jump trap-spurious ()))

;------------------------------------------------------------------------------------------------

(nc::defafun trap-debugger ()                           ;Absolute location 64.
  (move gr::*trap-temp11* processor-control)                                         ;save pctl
  (alu-field field-xor gr::*trap-temp12* gr::*trap-temp11* gr::*trap-temp11* (byte 3. 0.)) ;icache off
  (alu-field set-bit-right processor-control r0 gr::*trap-temp12*  (byte 1. 7.))       ;halt
  (nop)
  (nop)
  (nop)
  (nop)                                         ;;;; trap debugger should stop at 70. ;;;;;;
  (move processor-control gr::*trap-temp11*)              ;restore pctl
  (nop)
  (nop)
  (nop)
  (jump interrupt-exit ()))                               ;return

;------------------------------------------------------------------------------------------------

(nc::defafun trap-spurious ()
  (jump trap-spurious ()))

;------------------------------------------------------------------------------------------------

(nc::defafun trap-reset ()
  (movei gr::*trap-mask* #xffffffff)
  (jump trap-reset ()))

;------------------------------------------------------------------------------------------------

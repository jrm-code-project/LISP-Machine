;;; -*- Mode:LISP; Package:TRAP; Base:10; Readtable:CL -*-


;;; Note:  This file is the first to be loaded in a cold load.  The
;;; functions in here *must* load at defined locations.  We
;;; are not guaranteeing this in any way, we just know that the compiler
;;; will do the right thing if we feed this file in first.  Because of
;;; this, it is not possible to add even a single line of code to this
;;; file until we are beyond the trap vector.  DON'T DO IT.
;;; This is possibly the most critical piece of code in the system.


(defafun trap ()
  ;; The hardware depends on this loaded at location 0.

  ;; Save the oreg, source doesn't matter because pipeline is shut off.
  (alu setl gr::*save-oreg* r0 r0 bw-32 boxed-left)

  ;; Oreg clock comes on, we save the left alu input
  (alu setl gr::*save-left* r0 r0 bw-32 boxed-left)

  ;; Left clock comes on, we save the right alu input
  (alu setr gr::*save-right* r0 r0 bw-32 boxed-right)

  ;; Right clock comes on, we save the alu status
  (alu pass-status gr::*save-status* r0 r0 bw-32 unboxed)

  ;; Alu clock comes on, we save the jump condition
  ;the + 32 in the following instruction causes the bit to be read inverted!
  (alu-field extract-bit-right gr::*save-jcond* r0 processor-status (byte 1. (+ 32. 17.)) unboxed)

  ;; Jump condition clock comes on, find out which trap went off.
  (alu-field field-and gr::*save-trap* gr::*trap-mask* trap-register (byte 31. 0.) unboxed)
  (alu prioritize-r gr::*trap-temp1* r0 gr::*save-trap* bw-32 unboxed)
  (alu-field set-bit-right gr::*trap-temp1* r0 gr::*trap-temp1* (byte 1. 5.) unboxed)

  ;; Save pc
  (alu merge-r gr::*save-trap-pc*  gr::*trap-dtp-code-5* trap-pc bw-24 boxed)

  ;; Save pc + 1, dispatch to trap handler
  (alu merge-r gr::*save-trap-pc+* gr::*trap-dtp-code-5* trap-pc+  bw-24 boxed next-pc-dispatch)

  ;; Spare locations.
  (nop)
  (nop))

;------------------------------------------------------------------------------------------------

(defafun non-modifying-exit ()
  ;; The hardware depends on this loading at location 12.

  ;; Jump condition gets fed to magic flipflop.
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))

  ;; Restore status of trapped instruction, alu clock turns off.
  (alu load-status-r nop r0 gr::*save-status* bw-32)

  ;; Pipeline saved pc for returning.
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right*  bw-32 boxed-left)

  ;; Pipeline saved pc+ for restarting dispatch instructions.
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right*  bw-32 boxed-left)

  ;; Jump to trapped instruction, pipeline jump condition for trapped jumps.
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)

  (nop)
  (nop)
  (nop))

;------------------------------------------------------------------------------------------------

(defafun modifying-exit ()
  ;; The hardware depends on this loading at location 20.

  ;; Jump condition gets fed to magic flipflop.
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))

  ;; Restore status of trapped instruction, alu clock turns off.
  (alu load-status-r nop r0 gr::*save-status* bw-32)

  ;; Pipeline saved pc for returning. Right side clock shuts off, save right gets caught.
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right* bw-32 boxed-left)

  ;; Pipeline saved pc+1 for dispatches.
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right* bw-32 boxed-left)

  ;; Jump to trapped instruction, setup saved jump condition.
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)

  (nop)
  (nop)
  (nop))

;------------------------------------------------------------------------------------------------

(defafun diagnostic-trap-exit ()
  ;; The hardware depends on this assembling at location 28.
  ;; It causes a trap to happen after the instruction fetch
  ;; but before the instruction register gets loaded.  This
  ;; enables us to run icache diagnostics.


  ;; Dispatch to trap pc.
  (alu setl nop gr::*save-trap-pc*  gr::*save-right* bw-32)

  ;; This instruction can be ignored.
  (alu setl nop gr::*save-trap-pc+* gr::*save-right* bw-32)

  ;; This just does a dispatch.
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch)

  (nop))

;;; Trap dispatch table

(defafun trap-vector-table () ;;; at absolute location 32.

  ;; This "function" is actually a dispatch table.

trap-vector-reset                                       ;Bit 31 - addr 32 - Highest priority
  (jump reset-trap-handler ())

trap-vector-trace                                       ;Bit 30 - addr 33
  (jump trace-trap-handler ())

trap-vector-icache-parity                               ;Bit 29 - addr 34
  (jump icache-parity-trap-handler ())

trap-vector-icache-nubus-err                            ;Bit 28 - addr 35
  (jump icache-nubus-error-trap-handler ())

trap-vector-icache-nubus-timeout                        ;Bit 27 - addr 36
  (jump icache-nubus-timeout-trap-handler ())

trap-vector-icache-page-fault                           ;Bit 26 - addr 37
  (jump icache-map-fault-trap-handler ())

trap-vector-proc-mread-parity                           ;Bit 25 - addr 38
  (jump memory-read-parity-trap-handler ())

trap-vector-proc-mread-nubus-err                        ;Bit 24 - addr 39
  (jump memory-read-nubus-error-trap-handler ())

trap-vector-proc-mread-nubus-timeout                    ;Bit 23- addr 40
  (jump memory-read-nubus-timeout-trap-handler ())

trap-vector-proc-mread-page-fault                       ;Bit 22 - addr 41
  (jump memory-read-page-fault-trap-handler ())

trap-vector-proc-mread-transporter                      ;Bit 21 - addr 42
  (jump memory-read-transporter-trap-handler ())

trap-vector-proc-mwrite-nubus-err                       ;Bit 20 - addr 43
  (jump memory-write-nubus-error-trap-handler ())

trap-vector-proc-mwrite-nubus-timeout                   ;Bit 19-  addr 44
  (jump memory-write-nubus-timeout-trap-handler ())

trap-vector-proc-mwrite-page-fault                      ;Bit 18 - addr 45
  (jump memory-write-page-fault-trap-handler ())

trap-vector-proc-mwrite-gc                              ;Bit 17 - addr 46
  (jump memory-write-gc-trap-handler ())

trap-vector-floating-point                              ;Bit 16 - addr 47
  (jump floating-point-trap-handler ())

trap-vector-heap-empty                                  ;Bit 15 - addr 48
  (jump heap-empty-trap-handler ())

trap-vector-instruction-bit                             ;Bit 14 - addr 49
  (jump instruction-trap-handler ())

trap-vector-datatype                                    ;Bit 13 - addr 50
  (jump datatype-trap-handler ())

trap-vector-overflow                                    ;Bit 12 - addr 51
  (jump overflow-trap-handler ())

trap-vector-spare11                                     ;Bit 11 - addr 52
  (jump spare11-trap-handler ())

trap-vector-interrupt7                                  ;Bit 10 - addr 53
  (jump debugger-trap-handler ())

trap-vector-interrupt6                                  ;Bit 09 - addr 54
  (jump interrupt6-trap-handler ())

trap-vector-interrupt5                                  ;Bit 08 - addr 55
  (jump interrupt5-trap-handler ())

trap-vector-interrupt4                                  ;Bit 07 - addr 56
  (jump iop-trap-handler ())

trap-vector-interrupt3                                  ;Bit 06 - addr 57
  (jump interrupt3-trap-handler ())

trap-vector-interrupt2                                  ;Bit 05 - addr 58
  (jump interrupt2-trap-handler ())

trap-vector-interrupt1                                  ;Bit 04 - addr 59
  (jump interrupt1-trap-handler ())

trap-vector-interrupt0                                  ;Bit 03 - addr 60
  (jump interrupt0-trap-handler ())

trap-vector-timer-1024                                  ;Bit 02 - addr 61
  (jump timer-1024-trap-handler ())

trap-vector-timer-16384                                 ;Bit 01 - addr 62
  (jump timer-16384-trap-handler ())

trap-vector-spurious                                    ;Bit 00 - addr 63
  (jump spurious-trap-handler ()))

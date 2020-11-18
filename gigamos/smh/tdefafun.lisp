;;; -*- Mode:LISP; Package:TRAP; Base:10; Readtable:ZL -*-

(compiler:defafun diagnostic-trap-exit ()
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

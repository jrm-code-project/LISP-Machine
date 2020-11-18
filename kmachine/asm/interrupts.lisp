;;; -*- Mode:LISP; Package:TEST; Base:10; Readtable:ZL -*-

(defafun enable-overflow-traps (on)
  (alu-field field-extract-r a1 r0 trap-off (byte 1. 0.) unboxed)          ;traps off
  (alu-field field-pass trap-mask a0 trap-mask (byte 1. 12.) unboxed)      ;overflow mask
  (alu-field field-pass a2 a1 memory-control (byte 1. 31.) unboxed)        ;set trap enable
  (alu-field field-pass memory-control a0 a2 (byte 1. 29.) unboxed)        ;update MCTL
  (alu-field extract-bit-right a2 r0 a2 (byte 1. 29.) unboxed)             ;old enbl
  (alu merge-r return a0 a2 bw-24 boxed ch-return next-pc-return))         ;return

(defafun enable-async-traps (on)
  (alu-field extract-bit-right nop r0 a0 (byte 1. 0.))
  (alu-field field-extract-r a1 r0 trap-off (byte 1. 0.) unboxed br-zero)  ;traps off
  (branch off ())
  (movei a2 #x000007f4)                                                    ; on mask
  (branch finish (alu or trap-mask a2 trap-mask))
 off
  (movei a2 #xfffff807)                                                    ;off mask
  (alu and trap-mask a2 trap-mask)
 finish
  (alu-field field-pass a2 a1 memory-control (byte 1. 31.) unboxed)        ;set trap enable
  (alu-field field-pass memory-control a0 a2 (byte 1. 30.) unboxed)        ;update MCTL
  (alu-field extract-bit-right a2 r0 a2 (byte 1. 30.) unboxed)             ;old enbl
  (alu merge-r return a0 a2 bw-24 boxed ch-return next-pc-return))         ;return

(defafun enable-sync-traps (on)
  (alu-field extract-bit-right nop r0 a0 (byte 1. 0.))
  (alu-field field-extract-r a1 r0 trap-off (byte 1. 0.) unboxed br-zero)  ;traps off
  (branch off ())
  (movei a2 #x3ffe0000)                                                    ; on mask
  (branch finish (alu or trap-mask a2 trap-mask))
 off
  (movei a2 #xc001ffff)                                                    ;off mask
  (alu and trap-mask a2 trap-mask)
 finish
  (alu-field field-pass a2 a1 memory-control (byte 1. 31.) unboxed)        ;set trap enable
  (alu-field field-pass memory-control a0 a2 (byte 1. 27.) unboxed)        ;update MCTL
  (alu-field extract-bit-right a2 r0 a2 (byte 1. 27.) unboxed)             ;old enbl
  (alu merge-r return a0 a2 bw-24 boxed ch-return next-pc-return))         ;return

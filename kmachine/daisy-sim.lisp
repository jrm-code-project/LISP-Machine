;;; -*- Mode:LISP; Package:TRAP; Readtable:ZL; Base:10 -*-

(defafun daisy ()
loc-0
  (nop)
  (nop)
  (nop)
  (unconditional-branch loc-32 ())
loc-4
  (nop)
  (nop)
  (nop)
  (nop)
loc-8
  (nop)
  (nop)
  (nop)
  (nop)
loc-12
  (nop)
  (nop)
  (nop)
  (nop)
loc-16
  (nop)
  (nop)
  (nop)
  (nop)
loc-20
  (nop)
  (nop)
  (nop)
  (nop)
loc-24
  (nop)
  (nop)
  (nop)
  (nop)
loc-28
  (nop)
  (nop)
  (nop)
  (nop)
loc-32
  (movei processor-control 7)
  (movei bus-control #X79)
  (movei open-active-return #xffffff)
  (movei vma 0)
  (nop)
  (movei memory-map #x0f)
  (movei memory-map #x0f)
  (nop)
  (nop)
  (nop)
  (nop)
  (movei a0 0 unboxed)
hang
  (unconditional-branch hang (alu r+1 a0 a0 a0 bw-32 boxed-right))
  )

(defun make-it ()
  (kbug:make-boot-ram-simulation '(daisy)))

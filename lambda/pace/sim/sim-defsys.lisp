;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(load "dj:pace.sim;sim.translations")

(defpackage SIM)

(defsystem SIM
  (:pathname-default "sim:sim;")
  (:module defs ("sim-defs" "inst"))
  (:module main ("sim" "sim-tv" "sim-asm" "sim-sym"))
  (:compile-load defs)
  (:compile-load main (:fasload defs)))

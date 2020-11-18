;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-


(DEFSYSTEM TS
  (:PATHNAME-DEFAULT "TS:SOURCE;")
  (:MODULE DEFS "DEFS")
  (:Module main "main")
  (:module setup "setup")
  (:COMPILE-LOAD DEFS)
  (:compile-load main (:fasload defs) (:fasload defs))
  (:compile-load setup (:fasload defs) (:fasload main)))

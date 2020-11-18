;;; -*- Mode:LISP; Package:USER; Base:8; Readtable:ZL -*-

(defpackage STAT-COUNTERS)

(defsystem pace-hacks
  (:pathname-default "dj:pace.hacks;")
  (:module wholine-gc "wholine-gc")
  (:compile-load wholine-gc))

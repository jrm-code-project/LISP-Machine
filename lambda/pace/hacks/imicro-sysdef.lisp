;;; -*- Mode:LISP; Package:USER; Base:8; Readtable:ZL -*-

(defpackage MICRO
  :use (LAMBDA GLOBAL))

(defsystem imicro
  (:pathname-default "dj:pace.hacks;")
  (:module lambda-symbols ("sys:ulambda;lambda-symbols") :package "LAMBDA")
  (:module imicro ("symbol-table" "new-micro-as"))
  (:module components ("lambda-components"))
  (:readfile lambda-symbols)
  (:compile-load imicro (:readfile lambda-symbols))
  (:compile-load components (:fasload imicro)))

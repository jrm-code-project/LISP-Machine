;;; -*- Mode:LISP; Package:CLC; Readtable:CL; Base:10 -*-


(defsystem cl
  (:pathname-default "CLC:SOURCE;")
  (:module defs "defs")
  (:module main ("files" "p1"))
  (:compile-load defs)
  (:compile-load main (:fasload defs) (:fasload defs)))

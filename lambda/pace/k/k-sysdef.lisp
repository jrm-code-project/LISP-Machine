;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(when (or (null (find-package "MICRO"))
          (not (fboundp (intern-soft "DEFINE-MICRO-FUNCTION" "MICRO"))))
  (ferror nil "Do (make-system 'imicro) first."))

(defpackage "SIM"
  (:nicknames "K"))

(defsystem k
  (:pathname-default "dj:pace.k;")
  (:module defs ("k-defs"))
  (:module microcode ("k-microcode") :package micro)
  (:module main ("k-control"))                  ; "k-tests"
  (:module i-expand "i-expand")
  (:module memory "k-memory")
  (:module k-funs "k-funs")
  (:compile-load defs)
  (:compile-load microcode (:fasload defs))
  (:compile-load main (:fasload defs microcode))
  (:compile-load i-expand (:fasload main))
  (:compile-load memory (:fasload i-expand) (:fasload i-expand))
  (:compile-load k-funs (:fasload i-expand) (:fasload i-expand)))

;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(when (or (null (find-package "MICRO"))
          (not (fboundp (intern-soft "DEFINE-MICRO-FUNCTION" "MICRO"))))
  (ferror nil "Do (make-system 'imicro) first."))

(defpackage "SIM")

(defsystem k
  (:pathname-default "dj:pace;")
  (:module microcode ("k-microcode") :package micro)
  (:module main ("k-sim"))
  (:compile-load (microcode main)))

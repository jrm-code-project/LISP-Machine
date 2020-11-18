;;; -*- Mode: LISP; Syntax: Zetalisp; Package: USER; Base: 10; -*-

(defpackage DVI
  (:use global)
  #+(OR LMI TI)
  (:shadow "WRITE-BYTE" "FILE-LENGTH" "FINISH-OUTPUT"))

(defsystem dvi
  (:pathname-default "SYS:DVI;")

  #+(OR LMI TI)

  (:module expl ("dvi-explorer" #+release-2 "rel2-fixes"))
  (:module macros ("dvi-macros" ))
  (:module main ("dvi"))
  (:module press-prims ("dvipress-prims"))
  (:module translations ("dvi-im-methods"
                         "lmscreen-methods"
                         "dvipress-methods"))
  (:module toplevel ("dvicom"))
  (:module user ("dvi-user"))
  #+TI
  (:module spool ("printers"))
  (:compile-load expl)
  (:compile-load macros #+(OR LMI TI) (:fasload expl))
  (:compile-load main (:fasload macros))
  (:compile-load press-prims (:fasload main macros))
  (:compile-load translations (:fasload press-prims main macros))
  #+TI
  (:compile-load spool)
  (:compile-load toplevel (:fasload main))
  (:compile-load user (:fasload main)))

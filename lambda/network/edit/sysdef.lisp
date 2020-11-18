;;;-*-Mode:LISP; Package:USER; Readtable: CL; Base: 10-*-
;;; Copyright (c) Lisp Machine Inc., 1986.

(defpackage SITE-DATA-EDIT
  (:nicknames "SITED" "SITE-EDITOR")
  (:prefix-name "SITED")
  (:size 400)
  (:use tv global))

(defsystem site-editor
  (:name "Site Data Editor")
  (:pathname-default "SYS: NETWORK; EDIT;")
  (:patchable)
  (:warnings-pathname-default "SYS: NETWORK; EDIT; SITE-EDITOR-CWARNS")
  (:package site-data-edit)
  (:module edit ("WINDOW" "ATTRIBUTE"))
  (:module data-types ("DATA-TYPES"))
  (:module main ("MAIN"))
  (:module bootstrap ("BOOTSTRAP"))
  (:compile-load edit)
  (:compile-load bootstrap)
  (:compile-load data-types (:fasload edit))
  (:compile-load main (:fasload edit data-types)))

;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

;;; Font editor
(DEFSYSTEM font-editor
  (:name "Font Editor")
  (:short-name "FED")
  (:pathname-default "sys:window;font-editor;")
  (:warnings-pathname-default "SYS: WINDOW; font-editor; FED-CWARNS.LISP")
  (:MODULE DEFS "SYS: IO1; FNTDEF")
  (:MODULE IMAGE-TOOLS "IMAGE-TOOLS")
  (:MODULE MAIN (#|"SYS: IO1; FNTCNV"|#
                 "FED"
                 ))
  (:READFILE DEFS)
  (:COMPILE-LOAD IMAGE-TOOLS (:READFILE DEFS))
  (:COMPILE-LOAD MAIN (:FASLOAD IMAGE-TOOLS)))

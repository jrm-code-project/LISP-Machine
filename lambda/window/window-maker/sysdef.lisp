;;;-*- Mode:LISP; Package:USER; Base:8; Readtable:CL -*-
;;; Copyright C LISP MACHINE INC., 1985.
;;;

(defpackage "WINDOW-MAKER"
  (:nicknames "WM")
  (:prefix-name "WM")
  (:use global))

(defsystem window-maker
  (:NAME "Window-Maker")
  (:PATHNAME-DEFAULT "sys:window;window-maker;")
  (:patchable "sys:window;window-maker;window-maker")
  (:PACKAGE 'window-maker)
  (:module defs ("macros" "variables"))
  (:module general-functions ("slicing-procedures" "my-choose-variable-window"))
  (:module main ("new-window-maker"
                 "flavor-definitions-and-methods"
                 "code-generation-for-constraint-frames"
                 "specifying-parameters-for-constraint-frames"
                 "editing-constraint-frames"))
  (:module fonts ("sys:fonts;40vshd"))
  ;;
  (:fasload fonts)
  (:compile-load defs)
  (:compile-load general-functions
                 (:fasload defs))
  (:compile-load main
                 (:fasload defs general-functions)))

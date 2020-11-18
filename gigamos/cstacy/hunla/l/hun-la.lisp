;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(unless (find-package "HL")
  (make-package "HUNLA" :nicknames '("HL")))

(defsystem HUN-LA
  (:name "Hun La")
  (:pathname-default "HUNLA:L;")
  (:patchable "HUNLA:L;PATCH;")
  (:initial-status :experimental)
  (:maintaining-sites :GIGAMOS-CAM)

  (:module lisp-patches ("si-patches"
                         "misc-patches"))
  (:module generic-fun  ("fake-generic-functions"))
  (:module cl-zwei  ("cl-zwei"))
  (:module zwei-patches  ("zwei-patches"))
  (:module zwei ("kp-zwei"
                 "zwei-misc"
                 "edit-buffers"
                 "isearch"
                 "who-calls-zwei"))
  (:module misc-1 ("who-calls"))
  (:module misc-2 ("random" "wholine-hacks"))

  (:compile-load lisp-patches)
  (:compile-load generic-fun
                 (:fasload lisp-patches))
  (:compile-load cl-zwei
                 ((:fasload lisp-patches) (:fasload generic-fun)))
  (:compile-load zwei-patches
                 ((:fasload lisp-patches) (:fasload generic-fun)))
  (:compile-load zwei
                 ((:fasload lisp-patches) (:fasload generic-fun)
                  (:fasload cl-zwei)      (:fasload zwei-patches)))
  (:compile-load misc-1
                 ((:fasload lisp-patches) (:fasload generic-fun)))
  (:compile-load misc-2
                 ((:fasload lisp-patches) (:fasload generic-fun)
                  (:fasload misc-1))))

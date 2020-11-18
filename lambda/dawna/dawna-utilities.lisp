;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(cond ((>= (si:get-system-version) 110)
       (sstatus feature release-3))
      ('else
       (sstatus feature release-2)))

(defsystem dawna-utilities
  (:pathname-default "dj:dawna;")
  (:default-binary-file-type #+release-2 "QFASL" #+release-3 "QF3")
  (:module main ("backup"
                "dirdif"
                #+RELEASE-2 "file-utils" #+RELEASE-3 "DJ:L.EXAMPLES;FILE-UTILS"
                "angel-screen"))
  (:compile-load main))

(make-system 'dawna-utilities :no-reload-system-declaration :noconfirm)

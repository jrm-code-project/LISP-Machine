;;; -*- Mode:LISP; Package:PACKAGES; Readtable:CL; Base:10 -*-


;;; TEST-PACKAGES.LISP
;;;

(flush-package-system)

(setq lisp
      (make-package 'lisp :use NIL))

(export (list (intern "CONS" lisp)
              (intern "CAR"  lisp)
              (intern "CDR"  lisp))
        lisp)

(setq p1
      (make-package 'package-one :nicknames '(p1 package1) :use NIL))

(setq p2
      (make-package "PACKAGE-TWO" :nicknames '(p2 package2) :use NIL))

(setq p3
      (make-package "PACKAGE-THREE" :nicknames (list "P3" "PACKAGE3")))

(setq foo1sym (intern "FOO" p1))
(setq foo2sym (intern "FOO" p2))
(setq foo3sym (intern "FOO" p3))

(export foo2sym p2)
(export foo3sym p3)

(use-package p2 p1)
(use-package p3 p1)

;(use-package p2 p3)

(setq p4
      (make-package "PACKAGE-FOUR" :nicknames '(p4) :use '(p1 p2 p3)))

;; pick P2::FOO

(unintern foo2sym p4)

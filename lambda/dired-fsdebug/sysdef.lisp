;;; -*- Mode:LISP; Package:USER; Fonts:(CPTFONT); Base:10 -*-

;;; Copyright (C) Lisp Machine, Inc. 1984, 1985, 1986
;;;   See filename "Copyright" for
;;; licensing and release information.

(defflavor zwei:gateway-constraint-frame ()())

(let ((inhibit-fdefine-warnings t))
  (load "dj:l.dired-fsdebug;odm-dired-environment")
  (load "dj:l.dired-fsdebug;fs-redefs")
  (load "dj:l.dired-fsdebug;tv-redefs")
  (load "dj:l.dired-fsdebug;zwei-redefs"))
(load "dj:l.dired-fsdebug;my-callers")
(load "dj:l.dired-fsdebug;make-flavor-tree")
(load "dj:l.dired-fsdebug;dired-blink")
(load "dj:l.dired-fsdebug;new-fsdebug")
(load "dj:l.dired-fsdebug;dired-frame")

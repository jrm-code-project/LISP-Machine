;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(eval-when (load eval)
  (si::goto-package-environment "USER")
  (si::make-package-environment "COMPILER")
  (si::make-package-environment "DEBUGGER"))
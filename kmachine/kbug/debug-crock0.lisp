;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(export '(
	  defconstant
	  defmacro
	  byte
	  byte-size
	  byte-position
	  )
	(find-package "PRIMS" user:*package*))

(defun hw::unboxed-constant (n)
  n)

;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-
;;;
;;; K-PACKAGES.LISP
;;;
;;; This file creates the packages which implement Common Lisp functions.


(make-package "DEFSTRUCT"
	      :use 'k-lisp)

(make-package "LISP-IO"
	      :use 'k-lisp)

(make-package "SETF"
	      :use 'k-lisp)

(make-package "TABLE"
	      :use 'k-lisp)


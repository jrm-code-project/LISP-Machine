;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

;;; this is a cross compiling kluge

(zl:defsubst nlisp:macro-function (sym)
  (get sym 'nmacro))

(defsetf nlisp:macro-function (sym) (fcn)
  `(setf (get ,sym 'nmacro) ,fcn))

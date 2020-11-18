;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-
;;;
;;; INTERPRETER-IMPORTS.LISP
;;;
;;; There's all sorts of funny business going on with loading the interpreter.


(eval-when (load eval)
  (shadowing-import
    '(
      k-lisp:eval
      k-lisp:evalhook
      k-lisp:applyhook
      k-lisp:*evalhook*
      k-lisp:*applyhook*
      k-lisp:macroexpand
      k-lisp:macroexpand-1
      k-lisp:*macroexpand-hook*
      )
    'interpreter))

(eval-when (load eval)
  (shadowing-import
    '(
      k-lisp:defun
      k-lisp:defvar
      k-lisp:defparameter
      k-lisp:defconstant
      )
    'interpreter))

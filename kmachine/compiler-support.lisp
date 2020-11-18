;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10. -*-

;;; This file contains support for letting the compiler run
;;; on the lambda.

(prims:defmacro hw::unboxed-constant (n)
  (setq n (eval n))
  (if (or (typep n '(unsigned-byte 32.))
          (typep n '(signed-byte 32.)))
      `'(HW::UNBOXED-CONSTANT ,(lisp:logand #xffffffff n))
      (error "This is not a 32 bit number: ~s" n)))

(prims:defmacro hw::boxed-constant (n)
  (setq n (eval n))
  (if (typep n '(unsigned-byte 32.))
      `'(HW::BOXED-CONSTANT ,n)
      (error "This is not a 32 bit number: ~s" n)))

(export '(
          nc::defafun
          nc::defrewrite
          nc::def-rewrite-patterns
          )
        (find-package "PRIMS" *package*))

;(export '(
;         >
;         >=
;         <
;         +
;         -
;         =
;         1+
;         *
;         and
;         ash
;         byte
;         byte-position
;         byte-size
;         defconstant
;         defun
;         defmacro
;         defsubst
;         export
;         funcall
;         function
;         if
;         in-package
;         labels
;         lambda
;         let
;         multiple-value-bind
;         "GLOBAL:NIL"
;         not
;         null
;         or
;         prog1
;         proclaim
;         quote
;         values
;         )
;       (find-package "VINC"))


(export '(nlisp::dpb nlisp::ldb prims:byte) 'nlisp)

(zl:defsubst nlisp:dpb (value byte-spec word)
  (dpb value
       (byte (prims:byte-size byte-spec)
             (prims:byte-position byte-spec))
       word))

(zl:defsubst nlisp:ldb (byte-spec word)
  (ldb (byte (prims:byte-size byte-spec)
             (prims:byte-position byte-spec))
       word))

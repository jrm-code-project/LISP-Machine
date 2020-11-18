;;; -*- Mode:LISP; Package:HARDWARE; Readtable:CL; Base:10 -*-


(lisp::defmacro unboxed-constant (n)
  (lisp::unless (lisp::typep n '(lisp::unsigned-byte 32.))
    (lisp::error "This is not a 32 bit number: ~s" n))
  `'(unboxed-constant ,n))

(lisp::import '(
                lisp::defconstant
                lisp::byte
                ))

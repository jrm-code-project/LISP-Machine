;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


;;; hack the interface to the compiler

(advise nlisp::compile-file :around hack-illop-codes nil
  (keeping-illop-codes-consistent
    #'(lambda () (progn :do-it))))
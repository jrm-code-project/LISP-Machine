;;; -*- Mode:LISP; Package:KBUG; Readtable:CL; Base:10 -*-


(defparameter *k-warm-test-files*
  '(
;    "jb:k.math;fixnum"
;    "jb:k.math;bignum"
;    "jb:k.math;rational"
;    "jb:k.math;float"
;    "jb:k.math;complex"
;    "jb:k.math;convert"
;    "jb:k.array;string"
    "jb:k.array;character"
    "jb:k.array;character-test"
    "jb:k;lists"
    "jb:k;type-predicates"
    ))

(defun warmload-list (&optional (file-list *k-warm-test-files*))
  (dolist (f file-list)
    (kbug-fasl f))
  nil)

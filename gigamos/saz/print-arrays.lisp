;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defmacro PRINTING-ALL-ARRAYS (&body body)
  `(once-only (most-positive-fixnum)
     (let ((si:*print-array* nil)
           (si:*print-simple-vector-length* ,most-positive-fixnum)
           (si:*print-simple-byte-vector-length* ,most-positive-fixnum))
       ,(first body))))

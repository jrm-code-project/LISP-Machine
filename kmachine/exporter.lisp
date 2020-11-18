;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(eval-when (load)
  (do-local-symbols (x 'vinculum nil)
    (export x 'vinculum)))

(eval-when (load)
  (do-local-symbols (x 'hardware nil)
    (export x 'hardware)))
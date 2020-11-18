;;; -*- Mode:LISP; Package:COLD; Readtable:CL; Base:10 -*-

(eval-when (load eval)
  (setq *cold-code-pointer*     cold-code-start)
  (setq *cold-loaded-functions* '()))
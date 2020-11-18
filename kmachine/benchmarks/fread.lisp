;;; -*- Mode:LISP; Package:user; Base:10; Readtable:ZL -*-

;;; From the "Dick Gabriel" Benchmark Series.
;;; Enhancements (C) Copyright 1983, Lisp Machine, Inc.

;;;BEGIN
;;;FREAD
;;; Benchmark to read from a file.

(defun fread ()
       (let ((f (open *fprint-test-file* '(in ascii))))
            (read f)
            (close f)))

(cond ((probef *fprint-test-file*))
      (t
       (terpri)
       (princ "Define FPRINT.TST using the FPRINT benchmark!")
       (let ((f (open *fprint-test-file* '(out ascii))))
            (print bench-fprint:test-pattern f)
            (close f))))

;(include "timer.lsp")

(timer timit (fread))
;;;END


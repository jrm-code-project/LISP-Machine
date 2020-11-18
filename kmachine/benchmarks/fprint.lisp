;;; -*- Mode:LISP; Package:user; Base:10; Readtable:ZL -*-

;;; From the "Dick Gabriel" Benchmark Series.
;;; Enhancements (C) Copyright 1983, Lisp Machine, Inc.

;;;BEGIN
;;;FPRINT
;;; Benchmark to print to a file.

(defun init (m n atoms)
       (let ((atoms (subst () () atoms)))
            (do ((a atoms (cdr a)))
                ((null (cdr a)) (rplacd a atoms)))
            (init1 m n atoms)))

(defun init1 (m n atoms)
       (cond ((= m 0) (pop atoms))
             (t (do ((i n (- i 2))
                     (a ()))
                    ((< i 1) a)
                    (push (pop atoms) a)
                    (push (init1 (1- m) n atoms) a)))))

(declare (special test-atoms))

(setq test-atoms '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67
                            mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12
                            wxyzab23 xyzabc34 123456ab 234567bc 345678cd
                            456789de 567890ef 678901fg 789012gh 890123hi))

(declare (special test-pattern))

(setq test-pattern (init 6. 6. test-atoms))

(defun fprint ()
       (cond ((probef *fprint-test-file*)
              (deletef *fprint-test-file*)))
       (let ((f (open *fprint-test-file* '(out ascii))))
            (print test-pattern f)
            (close f)))

(cond ((probef *fprint-test-file*))
      (t
       (let ((f (open  *fprint-test-file* '(out ascii))))
            (print test-pattern f)
            (close f))))

;(include "timer.lsp")

(timer timit (fprint))
;;;END

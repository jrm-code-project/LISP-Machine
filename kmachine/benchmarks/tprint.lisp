;;; -*- Mode:LISP; Package:user; Base:10; Readtable:ZL -*-

;;; From the "Dick Gabriel" Benchmark Series.
;;; Enhancements (C) Copyright 1983, Lisp Machine, Inc.

;;;BEGIN
;;;TPRINT
;;; Benchmark to print and read to the terminal

#-LISPM (declare (fixsw t))

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

(setq test-atoms '(abc1 cde2 efg3 ghi4 ijk5 klm6 mno7 opq8 qrs9
                        stu0 uvw1 wxy2 xyz3 123a 234b 345c 456d
                        567d 678e 789f 890g))

(declare (special test-pattern))

(setq test-pattern (init 6. 6. test-atoms))

;(include "timer.lsp")

(timer timit (print test-pattern #+LISPM TERMINAL-IO))
;;;END

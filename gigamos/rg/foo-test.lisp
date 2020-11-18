;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:ZL; Base:10 -*-




(defun k::test (a) (print (list 'q a)))
(defun k::test (a b) (logior a b))

(defun k::test (a b c) (if a b c))                             ;wins
(defun k::test () 105)
(defun k::test (a b) (foo a b))
(defun k::test (a) (+ a 2))
(defun k::test (a b) (nlisp:if a b))
(defun k::test (a b) (if a b))
(defun k::test (a b) (ble a (foo (bar a) b)))
(defun k::test (a b) (funcall a b))
(defun k::test (a b) (print (list a b)))
(defun k::test (a b c d e) (list a b c d e))
(defun k::test (a) (print (list 'a a)))                         ;wins

(defun foo (a) (list a 2))              ;wins
(defun k::test (a &optional (b 105)) (list a b))                ;wins
(defun k::test (a &optional (b (foo a))) (list a b))            ;wins
(defun k::test (a &optional (b 105) (c 106)) (list a b c))      ;wins

(defun k::test (a)
  (list a))

(defun k::test (a b)
  (list a b))

(defun k::test (a &optional b)
  (list a b))

(defun k::test (&optional a b)
  (list a b))

;--now with special
(defun k::test ()
  (declare (global:special a))          ;global:special is necessary for EFH compiler to win.
  (list a))                             ;cross-compiler wins either with or without.

(defun k::test (a)
  (declare (global:special a))
  (setq a 105)
  (list a))

(defun k::test (a b)
  (declare (global:special a b))
  (list a b))

(defun k::test (a &optional b)
  (declare (global:special a b))
  (list a b))

(defun k::test (&optional a b)
  (declare (global:special a b))
  (list a b))

(defun k::test (&optional a b c)
  (declare (global:special a b c))
  (list a b c))

(defun k::test (&optional a b c d)
  (declare (global:special a b c d))
  (list a b c d))

;-- now the same with a rest
(defun k::test (&rest c)
  (declare (global:special a))
  (list 'foo c))

(defun k::test (a &rest c)
  (declare (global:special a))
  (list a c))

(defun k::test (a b &rest c)
  (declare (global:special a b))
  (list a b c))

(defun k::test (a &optional b &rest c)
  (declare (global:special a b))
  (list a b c))

(defun k::test (&optional a b &rest c)
  (declare (global:special a b))
  (list a b c))

;-- now the same with a special &rest
(defun k::test (&rest c)
  (declare (global:special a c))
  (list a c))

(defun k::test (a &rest c)
  (declare (global:special a c))
  (list a c))

(defun k::test (a b &rest c)
  (declare (global:special a b c))
  (list a b c))

(defun k::test (a &optional b &rest c)
  (declare (global:special a b c))
  (list a b c))

(defun k::test (&optional a b &rest c)
  (declare (global:special a b c))
  (list a b c))


;----  whole thing again with supplied-p frobs
(defun k::test (a &optional (b nil s1))                 ;EDc OK
  (list a b s1))

(defun k::test (&optional (a 1 s1) (b 2 s2))    ;EDc OK
  (list a b s1 s2))

;--now with special
(defun k::test ()
  (declare (global:special a))
  (list a))

(defun k::test (a)
  (declare (global:special a))
  (list a))

(defun k::test (a b)
  (declare (global:special a b))
  (list a b))

(defun k::test (a &optional (b nil s1))                 ;EDc loses
  (declare (global:special a b s1))
  (list a b s1))

(defun k::test (&optional (a nil s1) (b nil s2))        ;EDc loses
  (declare (global:special a b s1 s2))
  (list a b s1 s2)
  )

;-- now the same with a rest
(defun k::test (a &optional (b nil s1) &rest c)
  (declare (global:special a b s1))
  (list a b c s1))

(defun k::test (&optional (a 'a1 s1) (b 'a2 s2) &rest c)
  (declare (global:special a b s1 s2))
  (list a b c s1 s2))

;-- now the same with a special &rest
(defun k::test (a &optional (b nil s1) &rest c)
  (declare (global:special a b c s1))
  (list a b c s1))

(defun k::test (&optional (a nil s1) (b nil s2) &rest c)
  (declare (global:special a b c s1 s2))
  (list a b c s1 s2))


;----

(defun k::test (&optional (a (foo 1) a-p) (b (foo 2) b-p) (c (foo 3) c-p) (d (foo 4) d-p))
  (list a a-p b b-p c c-p d d-p))

(defun k::test (&optional (a (foo 1) a-p) (b (foo 2) b-p) (c (foo 3) c-p) (d (foo 4) d-p))
  (declare (global:special a a-p b b-p c c-p d d-p))
  (list 'v1 a a-p b b-p c c-p d d-p))

(defun k::test (a &aux (b 3) (c 4))
  (declare (global:special b c))
  (list a b c))

(defun k::test nil (foo 3))

(defun k::test (a &optional (b 105) (c 106))
  (declare (global:special b))
  (list a b c))

(defun k::test (a b)
  (declare (special a b))
  (list a b)
  (setq a 105))

(defun k::test (&rest c) (list c))
(defun k::test (a &optional (b 105) &rest c)
  (list a c b))

(defun k::test (&optional (a (print 'a)) (b (print 'b)))
  (list a b))

(defun k::test (&optional (a 105 a-p) (b 110 b-p) &rest c)
  (list a b c))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15)
  (list z0 z1 z2 z3 z4 z5 z6 z7)
  (list z8 z9 z10 z11 z12 z13 z14 z15))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  (list z0 z1 z2 z3 z4 z5 z6 z7)
  (list z8 z9 z10 z11 z12 z13 z14 z15 z16))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17)
  (list z0 z1 z2 z3 z4 z5 z6 z7)
  (list 'foo z8 z9 z10 z11 z12 z13 z14 z15 z16 z17))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  (list z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16))

(defun k::test (z0 z1 &aux z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  (setq z2 (setq z3 (setq z4 (setq z5 (setq z6 (setq z7 (setq z8 (foo))))))))
  (setq z9 (setq z10 (setq z11 (setq z12 (setq z13 (setq z14 (setq z15 (setq z16 (bar)))))))))
  (list z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  (let ((a (list z0 z1))
        (b (list z2 z16)))
    (list z0 z1 z2 (list a b z3) z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 &rest z16)
  (list z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16))

(defun k::test (&optional (z0 nil z0s) (z1 nil z1s) (z2 nil z2s) (z3 nil z3s)
                          (z4 nil z4s) (z5 nil z5s) (z6 nil z6s) (z7 nil z7s)
                          (z8 nil z8s) (z9 nil z9s) (z10 nil z10s) (z11 nil z11s)
                          (z12 nil z12s) (z13 nil z13s) (z14 nil z14s)
                          (z15 nil z15s) (z16 nil z16s))
  (list z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
        z0s z1s z2s z3s z4s z5s z6s z7s z8s z9s z10s z11s z12s z13s z14s z15s z16s))


(defun k::test (&optional (z0 1 z0s) (z1 2 z1s) (z2 3 z2s) (z3 4 z3s)
                          (z4 5 z4s) (z5 6 z5s) (z6 7 z6s) (z7 8 z7s)
                          (z8 9 z8s) (z9 10. z9s) (z10 11. z10s) (z11 12. z11s)
                          (z12 nil z12s) (z13 nil z13s) (z14 nil z14s)
                          (z15 nil z15s) (z16 nil z16s) &rest z17)
  (list z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16
        z0s z1s z2s z3s z4s z5s z6s z7s z8s z9s z10s z11s z12s z13s z14s z15s z16s z17))

(defun k::test ()
  (list 0 1 2 3 4 5 6 7 8 9 10. 11. 12. 13. 14. 15. 16. 17.))

(defun k::test (z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  (list 1 2))

(defun k::test1 (&optional z0 z1 z2 z3 z4 z5 z6)
  (k::test 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9 'a10 z0 z1 z2 z3 z4 z5 z6))

(defun k::test (&optional z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16)
  (list z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16))

(defun k::test (&key (a 105) (b 110))
  (list a b))

(defun k::test (&key (a 105) (b 110) (c 120))
  (list a b c))

(defun k::test (a)
  (catch 'foo
    (bar a)))

(defun k::test nil
  (throw 'foo 'bar))

(defun k::test (a b)
   (prog (l)
         (setq l (foo a (if b (go loss) 'loss)))
         (return l)
    loss (return 'loss)))

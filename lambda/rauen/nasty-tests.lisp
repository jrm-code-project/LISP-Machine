;;; -*- Mode:LISP; Package:INTERPRETER; Base:10; Readtable:CL -*-

(defvar *foo* 3)

((lambda (*foo* &optional (y *foo*)) y) 7)
((lambda (x &optional (y x)) (declare (special x)) y) 7)

(let ((*foo* 5) (x *foo*)) x)
(let* ((*foo* 5) (x *foo*)) x)

(eval-exp
  '(macrolet ((f (z) `(list 2 ,z 3)))
     (labels ((f (z) (* z z))
              (g (z) (f z)))
       (g 5))))

(macrolet ((f (z) `(list 2 ,z 3)))
             (flet ((f (z) (* z z))
                    (g (z) (f z)))
               (g 5)))

(progv '(a b) '(3 3) b)
(let ((b 4)) (progv '(a b) '(3 3) b))
(let ((b 4)) (declare (special b)) (progv '(a b) '(3 3) b))

(progv '(a b) '(3) b)
(let ((b 4)) (progv '(a b) '(3) b))
(let ((b 4)) (declare (special b)) (progv '(a b) '(3) b))

(funcall (block foo (tagbody bar (return-from foo #'(lambda () (go bar))))))

(catch 'foo (throw 'foo 4))
(catch 'foo (block bar (throw 'foo 5)))
(catch 'foo (tagbody bar (throw 'foo 6)))
(catch 'foo (catch 'bar (throw 'foo 7)))

(let ((x 5) (funs '()))
  (dotimes (j 10)
    (push #'(lambda (z)
              (if (null z) (setq x 0) (+ x z)))
          funs))
  funs)

(kdefun fib (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(time '(labels ((fib (n)
                     (cond ((= n 0) 1)
                           ((= n 1) 1)
                           (t (+ (fib (- n 1))
                                 (fib (- n 2)))))))
         (fib 11)))

(declare)

(funcall '(lambda (x) `(list 'foo ,(cadr x))) '(gorp 3))

(defmacro baz (x) `(list ,x ,x ,x))
(baz 5)

(kdefvar *foo*)
(defmacro decls () (declare (special w x) (special y)) (declare (special z)))
(let (a b c *foo* w x y z) 'done)

(defmacro arithmetic-if (test neg-form zero-form pos-form)
  (let ((var (gensym)))
    `(let ((,var ,test))
       (cond ((< ,var 0) ,neg-form)
             ((= ,var 0) ,zero-form)
             (t ,pos-form)))))

;;; neato bug - make-evaluation-frame had special binding proc return nil instead of
;;; env.  This trashed the frame for further binding.
(let (a b c) (declare (special a)))
(let (a b c) (declare (special b)))
(let (a b c) (declare (special c)))
(let (a b c) (declare (special a c)))

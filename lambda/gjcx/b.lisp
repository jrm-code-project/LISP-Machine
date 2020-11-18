;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-



(defun to-b (n)
  (cond ((zerop n) nil)
        ((evenp n)
         (cons 'y (to-b (floor n 2))))
        ('else
         (cons 'x (to-b (floor n 2))))))


(defun b-to (l)
  (cond ((null l) 0)
        ((eq (car l) 'x)
         (1+ (* 2 (b-to (cdr l)))))
        ((eq (car l) 'y)
         (* 2 (b-to (cdr l))))))


;; (b-diff a ()) = a
;; (b-diff (cons 'y a) (cons 'y b)) = (cons 'y (b-diff a b))
;; (b-diff (cons 'y a) (cons 'x b)) = (cons 'x (b-diff (b-diff a '(x)) b))
;; (b-diff (cons 'x a) (cons 'y b)) = (cons 'x (b-diff a b))
;; (b-diff (cons 'x a) (cons 'x b)) = (cons 'y (b-diff a b))

(defun b-diff (a b)
  (cond ((null b) a)
        ((and (eq (car a) 'y) (eq (car b) 'y))
         (cons 'y (b-diff (cdr a) (cdr b))))
        ((and (eq (car a) 'y) (eq (car b) 'x))
         (cons 'x (b-diff (b-diff (cdr a) '(x)) (cdr b))))
        ((and (eq (car a) 'x) (eq (car b) 'y))
         (cons 'x (b-diff (cdr a) (cdr b))))
        ((and (eq (car a) 'x) (eq (car b) 'x))
         (cons 'y (b-diff (cdr a) (cdr b))))))

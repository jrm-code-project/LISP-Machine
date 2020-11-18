;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;; 28-Oct-86 09:56:43 -gjc

;;; simplest possible bignum runtime support.

;;; primitives
;;; (b-radix-plus <x> <y>)  => <x+y> <carry>
;;; (b-radix-times <x> <y>) => <high> <low>
;;; (b-radix-haulong <x>)   => <n>

(defvar *b-radix* 10)

(defun b-radix-plus (x y)
  (let ((r (+ x y)))
    (if (< r *b-radix*)
        (values r 0)
      (values (- r *b-radix*) 1))))

(defun b-radix-times (x y)
  (floor (* x y) *b-radix*))

(defun b-radix-haulong (x)
  (haulong x))

(defun b-radix-lessp (a b)
  (< a b))

(defun b-radix-equal (a b)
  (= a b))

(defun b-radix-difference (x y)
  (cond ((< x y)
         (values (- (+ x *b-radix*) y) 1))
        ('else
         (values (- x y) 0))))


;;; a bignum is a list of cooefficients of (radix^0 radix^1 radix^2 ...)

(defun b-plus (a b)
  (b-plus-aux a b 0))

(defun b-plus-aux (a b c)
  (cond ((null a)
         (cond ((zerop c)
                b)
               ('else
                (b-plus-aux (list c) b 0))))
        ((null b)
         (cond ((zerop c)
                a)
               ('else
                (b-plus-aux a (list c) 0))))
        ('else
         (multiple-value-bind (sum carry)
             (b-radix-plus (car a) (car b))
           (cond ((zerop carry)
                  (cond ((zerop c))
                        ('else
                         (multiple-value-setq (sum carry) (b-radix-plus sum c)))))
                 ((zerop c))
                 ('else
                  (setq sum (b-radix-plus sum c))))
           (cons sum (b-plus-aux (cdr a) (cdr b) carry))))))

(defun b-difference (a b)
  (b-difference-aux a b 0))


(defun b-difference-aux (a b c)
  (cond ((null b)
         (cond ((zerop c)
                a)
               ('else
                (b-difference-aux a (list c) 0))))
        ('else
         (multiple-value-bind (diff borrow)
             (b-radix-difference (car a) (car b))
           (cond ((zerop borrow)
                  (cond ((zerop c))
                        ('else
                         (multiple-value-setq (diff borrow) (b-radix-difference diff c)))))
                 ((zerop c))
                 ('else
                  (setq diff (b-radix-difference diff c))))
           (cons diff (b-difference-aux (cdr a) (cdr b) borrow))))))





(defun b-times (a b)
  (b-plus-l (b-times-aux a b)))

(defun b-plus-l (l)
  (do ((sum nil (b-plus (car l) sum))
       (l l (cdr l)))
      ((null l) sum)))

(defun b-times-aux (a b)
  (cond ((null a) ())
        ((zerop (car a))
         (b-times-aux (cdr a) (cons 0 b)))
        ('else
         (cons (b-times-aux-r 0 (car a) b)
               (b-times-aux (cdr a) (cons 0 b))))))

(defun b-times-aux-r (carry d n)
  (cond ((null n)
         (cond ((zerop carry) ())
               ('else
                (list carry))))
        ('else
         (multiple-value-bind (hi low)
             (b-radix-times d (car n))
           (multiple-value-setq (low carry) (b-radix-plus low carry))
           (setq hi (b-radix-plus carry hi))
           (cons low (b-times-aux-r hi d (cdr n)))))))


(defun b-equal (a b)
  (cond ((null a) (null b))
        ((null b) nil)
        ((b-radix-equal (car a) (car b))
         (b-equal (cdr a) (cdr b)))))

(defun b-lessp (a b)
  (b-lessp-aux a b nil))

(defun b-lessp-aux (a b left)
  (cond ((null a)
         (cond ((null b) left)
               (t)))
        ((null b) nil)
        ((b-radix-lessp (car a) (car b))
         (b-lessp-aux (cdr a) (cdr b) t))
        ((b-radix-equal (car a) (car b))
         (b-lessp-aux (cdr a) (cdr b) left))
        ('else
         (b-lessp-aux (cdr a) (cdr b) nil))))


;; a simple way to deal with division is via an iterative approximation.
;; thereby avoiding the "one in a million case" and other hair.
;;

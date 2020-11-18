;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

;;; Note: This is Common-Lisp. It will not run in BuLisp without modification
;;;       or additional support code.

;;; a number N is represented in base B as a list ( D0*B^0 D1*B^1 D2*B^2 ...)
;;; For binary base let NIL = 0 and T = 1.

;;; identities for base 2.
;;; (->b2 0) = ()
;;; (->b2 (* 2 n)) = (cons () (->b2 n))
;;; (->b2 (1+ (* 2 n))) = (cons t (->b2 n))

(defun ->b2 (n)
  "Convert N to base 2"
  (cond ((zerop n)
         ())
        ((evenp n)
         (cons () (->b2 (floor n 2))))
        ('else
         (cons t (->b2 (floor n 2))))))

;;; (b2-> ()) = 0
;;; (b2-> (cons t l)) = (1+ (* 2 (b2-> l)))
;;; (b2-> (cons nil l)) = (* 2 (b2-> l))

(defun b2-> (L)
  "Convert a base to list L to an integer"
  (cond ((null l)
         0)
        ((car l)
         (1+ (* 2 (b2-> (cdr l)))))
        ('else
         (* 2 (b2-> (cdr l))))))

;;; (+b2 () a) = a
;;; (+b2 a ()) = a
;;; (+b2 (cons nil a) (cons nil b)) = (cons nil (+b2 a b))
;;; (+b2 (cons nil a) (cons t b)) = (cons t (+b2 a b))
;;; (+b2 (cons t a) (cons nil b)) = (cons t (+b2 a b))
;;; (+b2 (cons t a) (cons t b)) = (cons nil (+b2 (+b2 '(t) a) b))

(defun +b2 (a b)
  (cond ((null a)
         b)
        ((null b)
         a)
        ((null (car a))
         (cond ((null (car b))
                (cons nil (+b2 (cdr a) (cdr b))))
               ('else
                (cons t (+b2 (cdr a) (cdr b))))))
        ((null (car b))
         (cons t (+b2 (cdr a) (cdr b))))
        ('else
         (cons nil (+b2 (+b2 '(t) (cdr a)) (cdr b))))))


;;; for multiplication: sum the list of partial productions.

(defun *b2 (a b)
  (+B2-list (*b2-partial a b)))


(defun +b2-list (l)
  (cond ((null l)
         ())
        ('else
         (+b2 (car l) (+b2-list (cdr l))))))


(defun *b2-partial (a b)
  (cond ((null a)
         ())
        ((null (car a))
         (cons nil (*b2-partial (cdr a) (cons nil b))))
        ('else
         (cons b (*b2-partial (cdr a) (cons nil b))))))


;;; arbitrary base B.

(defun ->bn (b n)
  (cond ((zerop n)
         ())
        ('else
         (multiple-value-bind (quo rem)
             (floor n b)
           (cons (make-list rem) (->bn b quo))))))

;; rest of these B must be a list B long.

(defun bn-> (b l)
  (cond ((null l)
         0)
        ('else
         (+ (length (car l)) (* b (bn-> b (cdr l)))))))

(defun +bn (b l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ('else
         (let ((d (append (car l1) (cdr l2))))
           (cons (chop-d b d)
                 (+bn b (+bn b (rest-d b d) (cdr l1)) (cdr l2)))))))

(defun chop-d (b d)
  (chop-d-1 b d d))

(defun chop-d-1 (b d a)
  (cond ((null b)
         d)
        ((null d)
         a)
        ('else
         (chop-d-1 (cdr b) (cdr d) a))))

(defun rest-d (b d)
  (cond ((null b)
         d)
        ((null d)
         d)
        ('else
         (rest-d (cdr b) (cdr d)))))

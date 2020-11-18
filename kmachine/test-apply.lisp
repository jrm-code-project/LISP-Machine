;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10 -*-

(defun test-funcall ()
  (do-funcall 'symbol:symbol-value nil)
  (loop))

(defun do-funcall (f a)
  (funcall f a))


(defun test-apply ()
  (do-apply #'inc-it '(1 2 3))
  (loop))

(defun do-apply (f a)
  (apply f 4 5 a))

(defun inc-it (&rest y)
  (let ((sum 0))
    (dolist (x y sum)
      (setq sum (+ sum x)))))

(defun hang ()
  (hang))



(defun test-funcall-closure ()
  (tfc-1 3)
  (loop))

(defun tfc-1 (x)
  (tfc-2 #'(lambda (n) (+ n x))))

(defun tfc-2 (closure)
  (funcall closure 4))

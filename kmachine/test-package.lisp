;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Lowercase:T; Readtable:CL -*-


(defun test-package (x)
  (setq x (%make-package 'foo))
  (loop ))

(defun test-package2 ( x)
  (setq x (make-package 's-foo :use 'foo))
  (loop))

(defun test-use (x)
  (setq x (use-package '(foo) 's-foo))
  (loop))

(defun test-package3 (x)
  (setq x (%make-package 'loo))
  (use-package '(foo) 'loo)
  (loop))

(defun test-package4 (x)
  (setq x (%make-package 'goo :use 'foo))
  (loop))


(defun test-package4 (x)
  (setq x (%make-package 'pack-it :use 'foo))
  (loop))

(defun test-package5 (x)
  (setq x (%make-package 'xxx))
  (error "make package")
  (setq x (find-package x))
  (loop))

(defun test-package6 (x)
  (setq x (%make-package 'wew))
  (loop))

(defun test-intern (x)
  (setq *package* (find-package 'wew))
  (setq x (intern "WOW"))
  (loop))

(defun test-fp (x)
  (setq x (find-package 'wew))
  (loop))

(defun test-rehash (x)
  (setq *package* (find-package 'wew))
  (dolist (sym gr:*warm-symbols*)
           (intern sym))
  (setq x (find-package 'wew))
  (loop))


(defun put-warm-symbols ()
  (dolist (sym gr:*warm-symbols*)
    (let ((pack (or (find-package (symbol-package sym))
                    (%make-package (symbol-package sym)))))
      (setf (symbol-package sym) pack)
      (intern sym pack)))
  (setf (symbol-function 'warm-intern) #'intern)
  (loop))

(defun test-import (x)
  (setq *package* (find-package 'foo))
  (import '(loo goo hoo))
  (setq x *package*)
  (loop))

(defun *Ap* (x)
  (setq x *all-packages* )
  (loop))

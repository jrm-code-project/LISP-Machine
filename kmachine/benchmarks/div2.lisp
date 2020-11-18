;;; -*- Mode:LISP; Package:user; Base:10.; Readtable:CL -*-


(defun create-n (n)
       (do ((n n (1- n))
            (a () (push () a)))
           ((= n 0) a)))

(defun div2 (l)
       (do ((l l (cddr l))
            (a () (push (car l) a)))
           ((null l) a)))

(defun dv2 (l)
       (cond ((null l) ())
             (t (cons (car l) (dv2 (cddr l))))))

(defun test1 (l)
       (do ((i 300. (1- i)))
           ((= i 0))
           (div2 l)
           (div2 l)
           (div2 l)
           (div2 l)))

(defun test2 (l)
       (do ((i 300. (1- i)))
           ((= i 0))
           (dv2 l)
           (dv2 l)
           (dv2 l)
           (dv2 l)))



;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun div2-iterative ()
  (let ((l (create-n 200.)))
    (hw:write-microsecond-clock (hw:unboxed-constant 0))
    (li:error "DIV2-ITERATIVE complete."
           (test1 l) (hw:read-microsecond-clock))
    (loop)))

;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun div2-recursive ()
  (let ((l (create-n 200.)))
    (hw:write-microsecond-clock (hw:unboxed-constant 0))
    (li:error "DIV2-RECURSIVE complete."
           (test2 l) (hw:read-microsecond-clock))
    (loop)))

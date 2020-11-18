;;; -*- Mode:LISP; Package:user; Base:10; Readtable:CL -*-


(defun destructive (n m)
  (let ((l (do ((i 10. (1- i))
                (a () (push () a)))
               ((= i 0) a))))
    (do ((i n (1- i)))
        ((= i 0))
      (cond ((null (car l))
             (do ((l l (cdr l)))
                 ((null l))
               (or (car l)
                   (rplaca l (ncons ())))
               (nconc (car l)
                      (do ((j m (1- j))
                           (a () (push () a)))
                          ((= j 0) a)))))
            (t
             (do ((l1 l (cdr l1))
                  (l2 (cdr l) (cdr l2)))
                 ((null l2))
               (rplacd (do ((j (floor (length (car l2)) 2) (1- j))
                            (a (car l2) (cdr a)))
                           ((= j 0) a)
                         (rplaca a i))
                       (let ((n (floor (length (car l1)) 2)))
                         (cond ((= n 0) (rplaca l1 ())
                                        (car l1))
                               (t
                                (do ((j n (1- j))
                                     (a (car l1) (cdr a)))
                                    ((= j 1)
                                     (prog1 (cdr a)
                                            (rplacd a ())))
                                  (rplaca a i))))))))))))

;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun test-destructive ()
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "DESTRUCTIVE complete."
         (destructive 600. 50.)
         (hw:read-microsecond-clock))
  (loop))

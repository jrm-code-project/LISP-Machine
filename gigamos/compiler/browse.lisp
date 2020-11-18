;;; -*- Mode:LISP; Package:user; Base:10; Readtable:CL -*-

;;Hold off on this benchmark untill new compiler version is around to fix
;;compile of init.

;;COMPILE ON LAMBDA.
(defmacro char1 (x) `(char (string ,x) 0))


(defvar rand 21.)

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(defun init (n m npats ipats)
       (let ((ipats (copy-tree ipats)))
            (do ((p ipats (cdr p)))
                ((null (cdr p)) (rplacd p ipats)))
            (do ((n n (1- n))
                 (i m (cond ((= i 0) m)
                            (t (1- i))))
                 (name (intern (gensym)) (intern (gensym)))
                 (a ()))
                ((= n 0) a)
                (push name a)
                (do ((i i (1- i)))
                    ((= i 0))
                     (setf (get name (gensym)) nil))
                (setf (get name 'pattern)
                 (do ((i npats (1- i))
                      (ipats ipats (cdr ipats))
                      (a ()))
                     ((= i 0) a)
                     (push (car ipats) a)))
                (do ((j (- m i) (1- j)))
                    ((= j 0))
                    (setf (get name (gensym)) nil)))))



(defun seed () (setq rand 21.))

(defun random () (setq rand (mod (* rand 17.) 251.)))

(defun randomize (l)
       (do ((a ()))
           ((null l) a)
           (let ((n (mod (random) (length l))))
                (cond ((= n 0)
                       (push (car l) a)
                       (setq l (cdr l)))
                      (t
                       (do ((n n (1- n))
                            (x l (cdr x)))
                           ((= n 1)
                            (push (cadr x) a)
                            (rplacd x (cddr x)))))))))


(defun match (pat dat alist)
  (cond ((null pat)
         (null dat))
        ((null dat) ())
        ((or (eq (car pat) '?)
             (eq (car pat)
                 (car dat)))
         (match (cdr pat) (cdr dat) alist))
        ((eq (car pat) '*)
         (or (match (cdr pat) dat alist)
             (match (cdr pat) (cdr dat) alist)
             (match pat (cdr dat) alist)))
        (t (cond ((atom (car pat))
                  (cond
                    ((eq (char1 (car pat)) #\?)
                     (let ((val (assoc (car pat) alist)))
                       (cond (val (match (cons (cdr val)
                                               (cdr pat))
                                         dat alist))
                             (t (match (cdr pat)
                                       (cdr dat)
                                       (cons (cons (car pat)
                                                   (car dat))
                                             alist))))))
                    ((eq (char1 (car pat)) #\*)
                     (let ((val (assoc (car pat) alist)))
                       (cond (val (match (append (cdr val)
                                                 (cdr pat))
                                         dat alist))
                             (t
                              (do ((l () (nconc l (ncons (car d))))
                                   (e (cons () dat) (cdr e))
                                   (d dat (cdr d)))
                                  ((null e) ())
                                (cond ((match (cdr pat) d
                                              (cons (cons (car pat) l)
                                                    alist))
                                       (return t))))))))))
                 (t (and
                      (not (atom (car dat)))
                      (match (car pat)
                             (car dat) alist)
                      (match (cdr pat)
                             (cdr dat) alist)))))))

(defun test-browse ()
       (seed)
       (investigate (randomize
                     (init 100. 10. 4. '((a a a b b b b a a a a a b b a a a)
                                         (a a b b b b a a
                                            (a a)(b b))
                                         (a a a b (b a) b a b a))))
                    '((*a ?b *b ?b a *a a *b *a)
                      (*a *b *b *a (*a) (*b))
                      (? ? * (b a) * ? ?))))

(defun investigate (units pats)
       (do ((units units (cdr units)))
           ((null units))
           (do ((pats pats (cdr pats)))
               ((null pats))
               (do ((p (get (car units) 'pattern)
                       (cdr p)))
                   ((null p))
                   (match (car pats) (car p) ())))))


;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun browse ()
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "BROWSE complete." (test-browse) (hw:read-microsecond-clock))
  (loop))

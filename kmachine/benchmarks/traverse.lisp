;;; -*- Mode:LISP; Package:user; Base:10; Readtable:CL -*-

;;This must be compiled on the lambda.
(defstruct node
           (parents ())
           (sons ())
           (sn (snb))
           (entry1 ())
           (entry2 ())
           (entry3 ())
           (entry4 ())
           (entry5 ())
           (entry6 ())
           (mark ()))



(defvar *sn* 0)
(defvar *rand* 21.)
(defvar *count* 0)
(defvar *marker* nil)
(defvar *root*)

(setq *sn* 0)
(setq *rand* 21.)
(setq *count* 0)
(setq *marker* nil)

(defun snb () (setq *sn* (1+ *sn*)))

(defun seed () (setq *rand* 21.))

(defun random () (setq *rand* (mod (* *rand* 17.) 251.)))

(defun traverse-remove (n q)
       (cond ((eq (cdr (car q)) (car q))
              (prog1 (caar q) (rplaca q ())))
             ((= n 0)
              (prog1 (caar q)
                     (do ((p (car q) (cdr p)))
                         ((eq (cdr p) (car q))
                          (rplaca q
                                  (rplacd p (cdr (car q))))))))
             (t (do ((n n (1- n))
                     (q (car q) (cdr q))
                     (p (cdr (car q)) (cdr p)))
                    ((= n 0) (prog1 (car q) (rplacd q p)))))))

(defun traverse-select (n q)
       (do ((n n (1- n))
            (q (car q) (cdr q)))
           ((= n 0) (car q))))

(defun add (a q)
       (cond ((null q)
              (cons (let ((x (cons a nil)))
                      (rplacd x x) x)
                    nil))
;             `(,(let ((x `(,a)))
;                     (rplacd x x) x)))
             ((null (car q))
              (let ((x (cons a nil)))
                   (rplacd x x)
                   (rplaca q x)))
             (t (rplaca q
                        (rplacd (car q) (cons a (cdr (car q)))))))) ; `(,a .,(cdr (car q))))))))

(defun create-structure (n)
       (let ((a (cons (make-node) nil))) ;`(,(make-node))))
            (do ((m (1- n) (1- m))
                 (p a))
                ((= m 0) (setq a (cons (rplacd p a) nil)) ;`(,(rplacd p a)))
                         (do ((unused a)
                              (used (add (traverse-remove 0 a) ()))
                              (x) (y))
                             ((null (car unused))
                              (find-root (traverse-select 0 used) n))
                             (setq x (traverse-remove (mod (random) n) unused))
                             (setq y (traverse-select (mod (random) n) used))
                             (add x used)
                             (setf (node-sons y) (cons x (node-sons y))) ;`(,x .,(node-sons y)))
                             (setf (node-parents x) (cons y (node-parents x))) )) ;`(,y .,(node-parents x))) ))
                (push (make-node) a))))

(defun find-root (node n)
 (do ((n n (1- n)))
     ((= n 0) node)
     (cond ((null (node-parents node))
            (return node))
           (t (setq node (car (node-parents node)))))))


(defun travers (node node-mark)
       (cond ((eq (node-mark node) node-mark) ())
             (t (setf (node-mark node) node-mark)
                (setq *count* (1+ *count*))
                (setf (node-entry1 node) (not (node-entry1 node)))
                (setf (node-entry2 node) (not (node-entry1 node)))
                (setf (node-entry3 node) (not (node-entry1 node)))
                (setf (node-entry4 node) (not (node-entry1 node)))
                (setf (node-entry5 node) (not (node-entry1 node)))
                (setf (node-entry6 node) (not (node-entry1 node)))
                (do ((sons (node-sons node) (cdr sons)))
                    ((null sons) ())
                    (travers (car sons) node-mark)))))

(defun test-traverse (root)
  (let ((*count* 0))
    (travers root (setq *marker* (not *marker*)))
    *count*))


(defun traverse-loop ()
  (do ((i 50. (1- i)))
      ((= i 0))
    (test-traverse *root*)
    (test-traverse *root*)
    (test-traverse *root*)
    (test-traverse *root*)
    (test-traverse *root*)))

;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun traverse-init ()
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "TRAVERSE-INIT complete."
         (prog1 () (setq *root* (create-structure 100.)))
         (hw:read-microsecond-clock)))

(defun traverse ()

  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "TRAVERSE complete."
            (traverse-loop)
         (hw:read-microsecond-clock))
  (loop))

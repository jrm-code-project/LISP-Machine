;;; -*- Mode:LISP;  Ibase:10; Base:10 -*-

;;; Structure of the discrimination net:
;;;

(defmacro item-of-node (node)
  `(car ,node))

(defmacro a-list-of-node (node)
  `(cdr ,node))

(defmacro key-of-link (link)
  `(car ,link))

(defmacro node-of-link (link)
  `(cdr ,link))

(defvar discrim-net nil)


(defmacro add-to-a-list (exp1 exp2)
  `(cons ,exp1 ,exp2))

(defun test-index (item node)
  (index-list-of-atoms item node))

(defun index-list-of-atoms (item node)
  (let ((final-node (establish-nodes item node)))
    (print final-node)
    (cond ((item-of-node final-node))
          (t (setf (item-of-node final-node) item)
             item))))



(defun establish-nodes (item node)
  (cond ((null item) node)
        (t (establish-nodes (cdr item)
                            (establish-node (car item) node)))))




(defun establish-node (key node)
  (let ((link (assoc key (if node (a-list-of-node node) nil))))
    (cond (link (node-of-link link))
          (t (let ((next-node (ncons nil)))
               (setf (a-list-of-node node)
                     (add-to-a-list (cons key next-node)
                           (if node (a-list-of-node node))))
               next-node)))))

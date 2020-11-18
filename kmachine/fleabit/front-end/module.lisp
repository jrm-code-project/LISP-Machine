;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Dealing with the top level of a module


(defun do-exps (exps support-env shape fshape)
  (let ((*variable-id* 0)
;       (*shape* (create-shape))
        (*value-table* (make-table '*value-table*))
        (*delayed-user-messages* '()))
    (let* (
;          (shape *shape*)
;          (fshape (create-shape))
           (exps (alphatize-module exps shape fshape))
           (env (create-env *new-support* shape)))
;      (format t "~&alphatized exps: ~a" exps)
      (let ((*support* support-env)
            (*free-variables* (make-table '*free-variables*)))
        (multiple-value-bind (exps uses)
            (do-integrable-exps exps shape)
          (let ((uses uses)
                (res '()))
            (dolist (exp exps)
              (cond ((node-p exp)
                     (push exp res))
                    (t (multiple-value-bind (node u)
                           (transmogrify-exp exp shape uses)
                         (setq uses u)
                         (push node res)))))
            (values (map! #'fixup-node-tree (nreverse res))
                    env)))))))


(defun alphatize-module (exps shape fshape)
  (let ((exps (alpha-list exps shape fshape)))
     (do ((exps exps)
          (res '()))
         ((null exps) (nreverse res))
       (cond ((atom (car exps))
              (pop exps))
             ((eq (caar exps) syntax/block)
              (setq exps (append (cadar exps) (cdr exps))))
             (t
              (push (car exps) res)
              (pop exps))))))

(defun create-env (env shape)
  (let ((vars '()))                             ;what's this for?
    (dolist (def (shape-defined shape))
      (let ((name (car def)))
        (when name
          (let ((variant (cdr def))
                (support (support-table-lookup env name)))
            (if support
                (check-multiple-defs support variant)
              (let ((var (create-variable name)))
                (make-support-entry var env '() variant nil nil)
                (push var vars)))))))
    env))

(defun check-multiple-defs (support variant)
  (cond ((and (eq variant 'set)
              (or (eq (support-variant support) 'lset)
                  (eq (support-variant support) 'set))))
        ((and (eq variant 'lset)
              (eq (support-variant support) 'set))
         (setf (support-variant support) 'lset))
        ((not (eq 'multiple (support-variant support)))
         (orbit-warning "~S is multiply defined"
                        (variable-name (support-variable support)))
         (setf (support-variant support) 'multiple)))
  (undefined))

(defun do-integrable-exps (exps shape)
  (let ((count 0)
        (ints '())
        (plain '()))
    (dolist (exp exps)
      (cond ((integrable-exp? exp)
             (push (transmogrify-integrable-exp exp shape count) ints)
             (incf count))
            (t
             (incf count)
             (push (cons count exp) plain))))
    (multiple-value-bind (ints uses)
        (transmogrify-ints (sort-int-exps (nreverse ints)))
      (values (map! #'cdr (sort (nconc ints plain)
                                #'< :key #'car))
              uses))))

(defun transmogrify-ints (ints)
  (let ((uses '()))
    (dolist (int ints)
      (multiple-value-bind (node u)
          (transmogrify-node (cdr int) uses)
        node
        (setq uses u)))
    (values ints uses)))

(defvar defining-syntax
  (list)) ; syntax/define-variable-value syntax/define-constant))

(defun integrable-exp? (exp)
  (and (consp exp)
       (member (car exp) defining-syntax :test #'eq)
       (need-value? (cadr exp))))

(defun need-value? (name)
  (let ((support (if (variable-p name)
                     (variable-support name)
                     (funcall *new-support* name))))
    (and support
         (eq 'constant (support-variant support)))))

(defstruct int-def
  node
  index
  def
  uses)

(defun transmogrify-integrable-exp (exp shape index)
  (let ((node (really-transmogrify-exp exp shape))
        (new (make-int-def)))
    (setf (int-def-node new) node)
    (setf (int-def-index new) index)
    (cond ((simple-thunk? node)
           (multiple-value-bind (def uses)
               (quick-def-and-use-analyze node)
             (setf (int-def-def  new) def)
             (setf (int-def-uses new) uses)))
           (t
             (setf (int-def-def  new) nil)
             (setf (int-def-uses new) '())))
    new))

(defun sort-int-exps (ints)
  (let* ((int-table (make-table 'int-table)))
    (mapc #'(lambda (int)
              (setf (table-entry int-table (int-def-def int)) int))
          ints)
    (do ((ints ints)
         (res '()))
        ((null ints)
         (map! #'(lambda (int-def)
                   (cons (int-def-index int-def)
                         (int-def-node int-def)))
               (nreverse res)))
      (multiple-value-bind (ready unready)
          (really-sort-int-exps ints int-table)
        (mapc #'(lambda (int)
                  (setf (table-entry int-table (int-def-def int))
                        nil))
              ready)
        (setq ints unready res (nconc ready res))))))

(defun really-sort-int-exps (ints table)
  (let ((unready '())
        (ready '()))
    (dolist (int ints)
      (if (ready? int table)
          (push int ready)
        (push int unready)))
    (if ready
        (values ready unready)
      (error "integration loop ~S"
             (mapcar #'(lambda (int) (variable-name (int-def-def int)))
                     unready)))))

(defun ready? (int table)
  (null (setf (int-def-uses int)
              (delete-if-not #'(lambda (use)
                                 (table-entry table use))
                             (int-def-uses int)))))

(defun transmogrify-exp (exp shape uses)
    (debug :alpha
      (format t "~&Alphatized:~%")
      (pprint exp))
    (transmogrify-node (really-transmogrify-exp exp shape) uses))

;(defun really-transmogrify-exp (exp shape)
;  (let* ((exp `(,syntax/lambda t (nil ,(create-variable 'c))   ;t?
;                   (,exp (,syntax/quote t))))
;         (node (->value-node exp)))
;;    (return-tree-to-freelist exp)
;    (dereference-lexical-vars shape)
;    (print-all-delayed-user-messages)
;    node))

(defun really-transmogrify-exp (exp shape)
  (prog1
    (->value-node exp)
;   (return-tree-to-freelist exp)
    (dereference-lexical-vars shape)
    (print-all-delayed-user-messages)))



(defun transmogrify-node (node uses)
   (debug :unsimplified
     (format t "~&Unsimplified node:~%")
     (pp-cps node))
  (simplify-call node)
  (debug :simplified
    (format t "~&Simplified tree:~%")
    (pp-cps node))
  (multiple-value-bind (defs new-uses)
      (def-and-use-analyze node)
;    (format t "~&defs: ~a" defs)
;    (format t "~&uses: ~a" defs)
    (let ((forward-uses (check-uses defs new-uses uses)))
      (mapc #'(lambda (var)
                (noise "~S~%" (variable-name var)))
            defs)
      (values node forward-uses))))

;;; this was only called by
;;; replace-call-with-new-primop <- simplify-*primop in front;simplifiers
;(defun subexpression->code-tree (exp)
;  (let ((exp (alpha exp *syntax* *shape*)))
;    (if *front-debug*
;        (pprint exp *terminal-io*))
;    (let* ((exp `(,syntax/lambda t (nil ,(create-variable 'c)) (,exp)))
;           (node (->value-node exp)))
;;      (return-tree-to-freelist exp)
;      (dereference-lexical-vars *shape*)
;      (print-all-delayed-user-messages)
;      (if *front-debug*
;          (pp-cps node *terminal-io*))
;      node)))


;;; hacked version to not do assignment conversion
;;; since we are keeping original vars later things
;;; which use variable-flag must find it NIL
;;; (ie substitute-vars-in-node-tree) so we munge it
;;; here (all this stuff using 'lexical shape-lexical add-definition etc
;;; is now disabled ??? !)
(defun dereference-lexical-vars (shape)
  (dolist (v (shape-lexical shape))
    (setf (variable-flag v) nil)))

;;; assignment conversion here
;(defun dereference-lexical-vars (shape)
;  (setf (shape-lexical shape)
;       (remove-if-not #'(lambda (var)
;                          (cond ((variable-binder var)
;                                 (introduce-cell var)
;                                 nil)
;                                (t)))
;               (shape-lexical shape))))

(defun rebuild (nodes)
  (do ((first (car nodes) (attach first (car nodes)))
       (nodes (cdr nodes) (cdr nodes)))
      ((null nodes)
       (setf (node-parent first) empty)
       (let ((top (value-node->thunk first)))
         (setf (node-parent top) nil)
         top))))

(defun value-node->thunk (node)
  (let* ((c-var (create-variable 'k))
         (new-l (create-lambda-node 'b (list nil c-var)))
         (call (create-call-node 2 0)))
    (relate call-proc call (create-reference-node c-var))
    (relate (call-arg 1) call node)
    (relate lambda-body new-l call)
    new-l))

(defun attach (first second)
  (let ((new-l (create-lambda-node 'b (list (create-variable 'ignore))))
        (c-var (car (lambda-variables first))))
    (relate lambda-body new-l (detach (lambda-body second)))
    (case (length (variable-refs c-var))
      (0
       (bug "top level lambda ~S doesn't use its continuation" first))
      (1
       (let ((ref (car (variable-refs c-var))))
         (cond ((eq (node-role ref) call-proc)
                (replace-node (node-parent ref) (detach (lambda-body new-l)))
                (erase-all new-l))
               (t
                (replace-node ref new-l)))
         (relate lambda-body second (detach (lambda-body first)))
         (erase first)))
      (2
       (relate lambda-body
               second
               (make-call-with-exits 1 (list first new-l)))))
    second))

#|
(defun obtain-free-variable (name)
  (let ((v (funcall *base-support-env* name)))
    (cond (v
           (support-variable v))
          ((table-entry *free-variables* name))
          (t
           (let ((var (create-variable name)))
             (setf (table-entry *free-variables* name) var)
             var)))))


(defun free-refs (name var)
  (format t "~&~S: ~S~%"
          name
          (let ((l
                 (mapcar #'(lambda (ref)
                             (let ((loc (containing-definition (node-parent ref))))
                               (if loc (variable-name loc) 'top-level)))
                      (variable-refs var))))
            (if (member 'top-level l :test #'eq)
                (cons 'top-level (delete 'top-level l :test #'eq))
                l))))



|#

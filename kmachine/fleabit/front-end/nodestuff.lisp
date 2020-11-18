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

(defun used? (var)
  (and var
       (or (variable-refs var)
           (variable-special-p var))))

;;;   If NODE is a primop node or a variable node for a variable whose value
;;; is a primop, the primop is returned.

;(defun known-primop (node)
;  (cond ((primop-node? node)
;         (primop-value node))
;        ((let ((support (and (reference-node? node)
;                            (get-variable-support (reference-variable node)))))
;          (if support
;              (let* ((value (support-value support)))
;                (if (primop? value) value)))))))

(defun known-primop (node)
  (if (primop-node? node)
      (primop-value node)))

#||||||||

(defun known-object (node)
  (let ((support (and (reference-node? node)
                      (get-variable-support (reference-variable node)))))
    (if support
        (let ((value (support-value support)))
          (if (and (consp value)
                   (eq (car value) 'object))
              value)))))

(defun known-value (node)
  (let ((support (and (reference-node? node)
                      (get-variable-support (reference-variable node)))))
    (if support
        (support-value support)
      node)))

|||||||||||#

;;; If true, then the LAMBDA to which this variable is being bound
;;; can always be jumped to (although an environment adjustment may
;;; be needed).

(defun all-refs-are-calls? (var)
  (every #'(lambda (ref)
             (eq (node-role ref) call-proc))
          (variable-refs var)))

;;;  Returns T if REF is being referred to as an L-value.

(defun nonvalue-reference? (ref)
  (and (eq (node-role ref) (call-arg 2))
       (primop-node? (call-proc (node-parent ref)))
       (primop.uses-L-value? (primop-value (call-proc (node-parent ref))))))

;;; Walk (or map) a tree modifying procedure down a variable's references.

(defun walk-refs-safely (proc var)
  (let ((refs (copy-list (variable-refs var))))
    (mapc proc refs)
;    (return-list-to-freelist refs)
;    (undefined)))
    ))

(defun map-refs-safely (proc var)
  (let* ((refs (copy-list (variable-refs var)))
         (res (mapcar proc refs)))
;    (return-list-to-freelist refs)
    res))

;;; Thunk stuff

(defun thunk-value (l-node)
  (let ((refs (variable-refs (lambda-cont-var l-node))))
    (cond ((or (/= 1 (length refs))
               (not (eq call-proc (node-role (car refs)))))
           nil)
          ((/= 1 (length (call-args (node-parent (car refs)))))
           (bug "thunk returns multiple values"))
          (t
           (call-arg-n 1 (node-parent (car refs)))))))

(defun simple-thunk? (thunk)
  (let ((node (thunk-value thunk)))
    (and (node-p node)
         (eq (node-parent (node-parent node)) thunk)
         (lambda-node? node))))

;         (or (lambda-node? node)
;             (object-node? node)))))

#||||||||

;;;  Return a reference node to the variable bound to NAME in support
;;; environment ENV.
(defun create-nonlocal-reference (name env)
  (let ((support (support-lookup env name)))
    (cond (support
           (create-reference-node (support-variable support)))
          (t
           (bug "need reference to ~S in ~S and can't find it" name env)))))

;;;   Find the primop with id NAME in support environment ENV.

(defun primop-lookup (name env)
;  (let ((env (table-entry *support-tables* env-name)))
;    (if (not env)
;        (bug "need primop ~S in ~S and can't find env" name env-name)
        (let ((primop (primop-support env name)))
          (cond (primop
                 (create-primop-node primop))
                (t
                 (bug "need primop ~S in ~S and can't find it" name env)))));))

||||||||||#

;;;   Replaces the call node CALL with VALUE.
;;; (<proc> <exit> . <args>) => (<exit> <value>)

(defun replace-call-with-value (call value)
  (cond ((/= 1 (call-exits call))
         (bug "can only substitute for call with one exit ~S" call))
        (t
         (let ((cont (detach (call-arg-n 1 call))))
           (mapc #'(lambda (node)
                     (erase-all (detach node)))
                 (cdr (call-args call)))
           (replace-node (call-proc call) cont)
           (relate-new-call-args call (if value `(,value) '()))
           (setf (call-exits call) 0)))))

;;;   Substitute VAL for VAR.  If DETACH? is true then VAL should be detached
;;; and so can be used instead of a copy for the first substitution.

(defun substitute-value (var val detach?)
  (let ((refs (variable-refs var))
        (tree (debug :simp-tree (node-base val))))
    (debug :simp
      (format t "~&Substituting: ~A := ~A~%" (variable-unique-name var) (pp-cps-2 val)))
    (setf (variable-refs var) '())
    (if (and (reference-node? val)               ;Keep LET variable names
             (eq 'v (variable-name (reference-variable val))))
        (setf (variable-name (reference-variable val))
              (variable-name var)))
    (cond (refs
           (mapc #'(lambda (ref)
                     (replace-node ref (copy-node-tree val)))
                 (if detach? (cdr refs) refs))
           (if detach? (replace-node (car refs) (detach val)))
           (return-list-to-freelist refs))
          (detach?
           (erase-all (detach val))))
    (debug :simp-tree
      (pp-cps tree))))

;;;   This is SUBSTITUTE-VALUE with the guarentee that VAL will not be copied.
;;; <body> =>
;;; ((lambda (v)
;;;    <body>[VAR replaced by V])
;;;  VAL)
;;; If VAL is a reference node or VAR has only one reference then SUBSTITUTE-VALUE is
;;; called instead.

(defun replace-var (var val body detach?)
  (cond ((or (reference-node? val)
             (null (cdr (variable-refs var))))
         (substitute-value var val detach?))
        (t
         (let* ((new-var (create-variable 'v))
                (ref (create-reference-node new-var))
                (l-node (create-lambda-node 'p (list new-var)))
                (new-body (create-call-node 2 0)))
           (substitute-value var ref nil)
           (erase ref)
           (relate call-proc new-body l-node)
           (relate (call-arg 1) new-body (if detach?
                                             (detach val)
                                             (copy-node-tree val)))
           (move body
                 #'(lambda (node)
                     (relate lambda-body l-node node)
                     new-body))))))

;;;    Replace every reference of OLD-VAR in NODE with a reference to NEW-VAR.
;;; Return T if any change is made.

(defun substitute-in-node-tree (node old-var new-var)
  (let ((count (length (variable-refs new-var))))
    (substitute-vars-in-node-tree node (list old-var) (list new-var))
    (/= count (length (variable-refs new-var)))))

(defun substitute-vars-in-node-tree (node old-vars new-vars)
  (mapc #'(lambda (old new)
            (if (used? old)
                (setf (variable-flag old) new)))
        old-vars new-vars)
  (labels ((tree-walk (node)
              (cond ((lambda-node? node)
                     (mapc #'tree-walk (call-proc+args (lambda-body node))))
                    ((call-node? node)
                     (mapc #'tree-walk (call-proc+args node)))
;                   ((object-node? node)
;                    (mapc #'tree-walk (object-operations node))
;                    (mapc #'tree-walk (object-methods node))
;                    (tree-walk (object-proc node)))
                    ((let ((new (and (reference-node? node)
                                     (variable-flag (reference-variable node)))))
                       (if new
                          (replace-node node (create-reference-node new)))
                       new)))))
    (tree-walk node))
  (dolist (old old-vars)
    (if (used? old)
        (setf (variable-flag old) nil))))

;;;   NODE is the root to start from, RENAME is an a-list of variables and
;;; their replacements in the copy.

(defun copy-node-tree (node)
  (etypecase node
    (leaf-node
       (copy-leaf node))
     (lambda-node
       (copy-lambda node))
     (call-node
       (copy-call node))
;     (object-node
;       (copy-object node))
     ))

(defun copy-leaf (node)
  (ecase (leaf-variant node)
    (literal
     (create-literal-node (literal-value node)))
    (primop
     (create-primop-node (primop-value node)))
    (reference
     (let* ((var (reference-variable node))
            (v?? (and (variable-binder var)     ;       (cond ((and (variable-binder var)
                      (variable-flag var))))    ;                   (variable-flag var))
       (if v?? (create-reference-node v??)      ;              => create-reference-node)
         (create-reference-node var))))))

(defun copy-lambda (node)
  (let* ((vars (mapcar #'(lambda (var)
                           (if var
                               (setf (variable-flag var)
                                     (create-variable (variable-name var)))
                             nil))
                       (lambda-rest+variables node)))
         (new-node (create-lambda-node (variable-name (lambda-self-var node))
                                       vars)))
    (relate lambda-body new-node (copy-node-tree (lambda-body node)))
    (mapc #'(lambda (var)
              (if var (setf (variable-flag var) nil)))
          (lambda-rest+variables node))
    new-node))

(defun copy-call (node)
  (let ((new-node (create-call-node (length (call-proc+args node))
                                    (call-exits node))))
    (relate call-proc new-node (copy-node-tree (call-proc node)))
    (relate-call-args-list new-node (mapcar #'copy-node-tree (call-args node)))
    new-node))

;(defun copy-object (node)
;  (let ((new-node (create-object-node (object-operation? node)
;                                      (length (object-operations node)))))
;    (relate object-proc new-node (copy-node-tree (object-proc node)))
;    (relate-object-op new-node
;                      (mapcar #'copy-node-tree (object-operations node)))
;    (relate-object-methods new-node
;                           (mapcar #'copy-node-tree (object-methods node)))
;    new-node))

;;; NODE->VECTOR
;;;===========================================================================
;;;    Convert a node into a vector
;;;
;;;  primop        => <primop>
;;;  literal       => QUOTE <literal>
;;;  reference     => <vector index> if lexical
;;;                   <variable> otherwise
;;;  lambda        => LAMBDA #vars <variable names...> <call>
;;;  call          => <exits> <number of args> <args>
;;;  object        => (OBJECT <proc> <ops> <methods>)

(zl:defsubst add-datum (vec value)
  (setf (vref (car vec) (cdr vec)) value)
  (incf (cdr vec)))

(defun node->vector (node)
  (let* ((exp-vec (make-expanding-vector 32))
         (vec (cons exp-vec 0))
         (value (if (real-node->vector node vec)
                    (copy-node-vector exp-vec (cdr vec))
                    nil)))
    (recycle exp-vec)
    value))

(defun copy-node-vector (exp-vec size)
  (let ((first (vref exp-vec 0)))
    (cond ((or (primop? first)
               (variable-p first))
           first)
          ((eq first 'quote)
           (cons 'literal (vref exp-vec 1)))
          ((eq first 'object)
           (cons 'object (vref exp-vec 1)))
          (t
           (let ((new (make-array size)))
             (dotimes (i size)
               (setf (svref new i) (vref exp-vec i)))
             new)))))

(defun real-node->vector (node vec)
  (cond ((primop-node? node)
         (add-datum vec (primop-value node)))
        ((literal-node? node)
         (add-datum vec 'quote)
         (add-datum vec (literal-value node)))
        ((reference-node? node)
         (variable->vector (reference-variable node) vec))
        ((lambda-node? node)
         (lambda->vector node vec))
;       ((object-node? node)
;        (object->vector node vec))
        (t
         (bug "node->vector got funny node ~S" node))))

(defun lambda->vector (node vec)
  (add-datum vec 'lambda)
  (add-datum vec (variable-name (lambda-self-var node)))
  (add-datum vec (length (lambda-rest+variables node)))
  (dolist (var (lambda-rest+variables node))
    (cond (var
           (setf (variable-flag var) (cdr vec))
           (add-datum vec (variable-name var)))
          (t
           (add-datum vec nil))))
  (let ((ok? (call-node->vector (lambda-body node) vec)))
    (dolist (var (lambda-rest+variables node))
      (if var
          (setf (variable-flag var) nil)))
    ok?))

(defun variable->vector (var vec)
  (cond ((not (variable-binder var))
         (add-datum vec var)
         t)
        ((integerp (variable-flag var))
         (add-datum vec (variable-flag var))
         t)))

(defun call-node->vector (node vec)
  (add-datum vec (call-exits node))
  (node-list->vector (call-proc+args node) vec))

;(defun object->vector (node vec)
;  (add-datum vec 'object)
;  (add-datum vec (list (object-operation? node)
;                       (node->vector (object-proc node))
;                       (mapcar #'node->vector (object-operations node))
;                       (mapcar #'node->vector (object-methods node)))))

(defun node-list->vector (nodes vec)
  (add-datum vec (length nodes))
  (dolist (child nodes t)
    (unless (real-node->vector child vec)
      (return nil))))

;;; VECTOR->NODE
;;;===========================================================================
;;;    Convert a vector back into a node.

(zl:defsubst get-datum (vec)
  (incf (cdr vec))
  (vref (car vec) (cdr vec)))

(defun vector->node (vector)
  (cond ((primop? vector)
         (create-primop-node vector))
        ((variable-p vector)
         (create-reference-node vector))
        ((arrayp vector)  ;(expanding-vector-p ?    ;vector?
         (let ((node (real-vector->node (cons vector -1))))
           (clean-node-vector vector (vector-length vector))
           node))
        ((and (consp vector)
              (eq (car vector) 'literal))
         (create-literal-node (cdr vector)))
;        ((and (consp vector)
;              (eq (car vector) 'object))
;         (list->object-node (cdr vector)))
        (t
         (bug "VECTOR->NODE got funny value ~S~%" vector))))

(defun clean-node-vector (vector length)
  (dotimes (i length)
    (if (and (variable-p (vref vector i))
             (variable-binder (vref vector i)))
        (setf (vref vector i) (variable-name (vref vector i))))))

(defun real-vector->node (vec)
  (let ((exp (get-datum vec)))
    (cond ((primop? exp)
           (create-primop-node exp))
          ((variable-p exp)
           (create-reference-node exp))
          ((integerp exp)
           (create-reference-node (vref (car vec) exp)))
          ((eq exp 'lambda)
           (vector->lambda-node vec))
          ((eq exp 'quote)
           (create-literal-node (get-datum vec)))
;          ((eq exp 'object)
;           (list->object-node (get-datum vec)))
          (t
           (bug "vector->node got an unknown form ~S" exp)))))

(defun vector->lambda-node (vec)
  (let* ((self-name (get-datum vec))
         (count (get-datum vec)))
    (do ((i 0 (1+ i))
         (v '() (cons (vector->variable vec) v)))
        ((>= i count)
         (let ((node (create-lambda-node self-name (nreverse v))))
           (relate lambda-body node (vector->call-node vec))
           node)))))

(defun vector->variable (vec)
  (let ((name (get-datum vec)))
    (if name
        (setf (vref (car vec) (cdr vec)) (create-variable name)))))

(defun vector->call-node (vec)
  (let* ((exits (get-datum vec))
         (count (get-datum vec))
         (node (create-call-node count exits)))
    (dotimes (i count node)
      (relate (call-arg i) node (real-vector->node vec)))))

;;; VEC is a list in this case
;(defun list->object-node (list)
;  (destructure (((nil op? proc ops methods) list))
;    (let ((node (create-object-node op? (length ops))))
;      (relate object-proc node (vector->node proc))
;      (relate-object-ops node (mapcar #'vector->node ops))
;      (relate-object-methods node (mapcar #'vector->node methods))
;      node)))

;;; S-EXP => NODE
;;;===========================================================================
;;;    Convert an s-expression into a node.  This is a very simple version
;;; of alphatize.  Variables are all lexically bound and assumed to have unique
;;; names.
;;;
;;; There are two syntatic forms:
;;;    (LAMBDA (<self-var-name> <rest-var-name> . <var-names>)
;;;            <call-exits> . <call-expressions>)
;;;    (QUOTE <literal>)
;;;

(defun s-exp->node (exp)
  (let* ((table (make-table 's-exp->node))
         (node (real-s-exp->node exp table)))
    (recycle table)
    node))

(defun real-s-exp->node (exp table)
  (cond ((not exp) nil)
        ((primop? exp)
         (create-primop-node exp))
        ((variable-p exp)
         (create-reference-node exp))
        ((node-p exp)
         exp)
        ((let ((v (and (zl:atom exp)
                       (table-entry table exp))))
           (if v (create-reference-node v))
           v))
        ((zl:atom exp)
         (bug "s-exp->node got an unknown name ~S" exp))
        ((eq (car exp) 'lambda)
         (s-exp->lambda-node (cdr exp) table))
        ((eq (car exp) 'quote)
         (create-literal-node (cdr exp)))
        (t
         (bug "s-exp->node got an unknown form ~S" exp))))

;(define (backup-real-s-exp->node exp table)
;  (cond ((not exp) nil)
;        ((primop? exp)
;         (create-primop-node exp))
;        ((variable-p exp)
;         (create-reference-node exp))
;        ((node? exp)
;         exp)
;;        ((and (atom? exp)
;;              (table-entry table exp))
;;         => create-reference-node)
;;        ((atom? exp)
;;         (bug "s-exp->node got an unknown name ~S" exp))
;        ((atom? exp)
;         (cond ((table-entry table exp)
;                => create-reference-node)
;               (else
;                (bug "s-exp->node got an unknown name ~S" exp))))
;        ((eq? (car exp) 'lambda)
;         (s-exp->lambda-node (cdr exp) table))
;        ((eq? (car exp) 'quote)
;         (create-literal-node (cdr exp)))
;        (else
;         (bug "s-exp->node got an unknown form ~S" exp))))

(defun s-exp->lambda-node (exp table)
  (destructure (((self-var-name var-names body-exp) exp))
    (let* ((vars (mapcar #'(lambda (name)
                             (if name
                                 (setf (table-entry table name)
                                       (create-variable name))
                               nil))
                         var-names))
           (node (create-lambda-node self-var-name vars)))
      (relate lambda-body node (s-exp->call-node body-exp table))
      node)))

(defun s-exp->call-node (exp table)
  (cond ((node-p exp)
         exp)
        (t
         (destructure (((exits . exps) exp))
           (let ((node (create-call-node (length exps) exits)))
             (do ((exps exps (cdr exps))
                  (index 0 (1+ index)))
                 ((null exps) node)
               (let ((n (real-s-exp->node (car exps) table)))
                 (if n (relate (call-arg index) node n)))))))))

;;; Useful utility

(defun node-base (node)
  (do ((p node (node-parent p)))
      ((not (node-p (node-parent p)))
       p)))

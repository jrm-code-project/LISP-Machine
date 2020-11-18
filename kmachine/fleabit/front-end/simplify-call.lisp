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

;;; Simplifying call nodes

;;; Simplify the node in the car of NODE-PAIR.  Tries a series of simplification
;;; procedures, going back to the beginning whenever a change is made.  The
;;; simplifiers are only allowed to change the node and its descendents.
;;; no changes may be made to any other part of the tree.

(defun simplify-call (lambda-node)
  (let ((node (lambda-body lambda-node)))
    (if (node-simplified? node)
        node
      (do* ((node node (lambda-body lambda-node))
            (proc (call-proc node) (call-proc node))
            )
           ((not (or (and (lambda-node? proc)
                          (progn (bash-open node)
                                 (simplify-let proc node)))
;                    (and (reference-node? proc)
;                         (integrate-support proc))
                     (and (primop-node? proc)
                          (progn (bash-open node)
                                 (primop.presimplify (primop-value proc) node)))
                     (simplify-call-ignoring-exits node proc)
                     (simplify-call-using-exits node proc))))))))

;;; Simplify the non-exit arguments of NODE and NODE itself.  Returns T if any
;;; change is made.

(defun simplify-call-ignoring-exits (node proc)
  (setf (node-simplified? proc) t) ; Nothing to do here anyway
  (simplify-non-exit-args node)
  (setf (node-simplified? node) t)
  (or (simplify-call-using-proc proc node)
      (not (node-simplified? node))))

;;; Simplify the exits of NODE.  Remove it if has no side effects and its value
;;; is not used.

(defun simplify-call-using-exits (node proc)
  (simplify-exit-args node proc)
  (or (flush-unused-call node)
      (not (node-simplified? node))))

;;; Simplify the specified children.  These use the NODE-SIMPLIFIED? flag
;;; to determine if a change has been made.
(defun simplify-non-exit-args (node)
  (mapl #'simplify (nthcdr (call-exits node) (call-args node))))

;;; Simplify the exits of call-node NODE.  If the node does a test propogate
;;; results of the test down the appropriate arms.  This is a small (but
;;; helpful) bit of type inferencing.
(defun simplify-exit-args (node proc)
  (if (and (primop-node? proc)
           (eq (primop-value proc) primop/conditional))
      (progn
        (add-to-value-table node 'true)
        (simplify (call-args node))
        (add-to-value-table node 'false)
        (simplify (cdr (call-args node)))
        (add-to-value-table node nil))
    (mapl #'simplify (call-exit-args node))))

;(defun simplify-exit-args (node)
;  (case (call-exits node)
;    (1 (simplify (call-args node)))
;    (2 (add-to-value-table node 'true)
;       (simplify (call-args node))
;       (add-to-value-table node 'false)
;       (simplify (cdr (call-args node)))
;       (add-to-value-table node nil))))

;;; *VALUE-TABLE* is bound by DO-EXP
(defun add-to-value-table (call value)
  (destructure (((nil nil true? arg) (call-args call)))
    (when (and (primop-ref? true? primop/true?)
               (reference-node? arg))
      (setf (table-entry *value-table* (reference-variable arg))
            value))))

;;; Calls to literals are flushed.  Primops are simplified using their own
;;; methods.  Calls to objects are simplified (the handler is flushed).
;;; If the second argument is a reference to a known object operation dispatch
;;; will be attempted.
(defun simplify-call-using-proc (proc node)
  (unless (or (not (leaf-node? proc))
              (literal-node? proc))
    (let ((primop (known-primop proc)))
      (cond (primop
             (primop.simplify primop node))))))
;;      ((and (bound-to-operation? (call-proc node))
;;            (cdr (call-args node))
;;            (bound-to-object? (relation (call-arg 2) node)))
;;       (simplify-operation-dispatch node obj-exp))


;;; This doesn't actually detach the open since it is above us in
;;; the tree (we are simplifying the call for which this is an open)
;;; it just bashes the arg so later flush-unused-call can remove it
(defun bash-open (call)
  (let ((open-call (call-open call)))
    (when open-call
      (setf (leaf-value (call-arg-n 2 open-call)) nil)
      t)))


;;; Remove a call that has no side effects and produces no useful result.

;(defun flush-unused-call (node)
;  (when (and (not (side-effects? (call-proc node)))
;            (unused-call? node))
;    (replace-node node (detach (lambda-body (call-arg-n 1 node))))
;    t))


;;; this changed to flush calls to $OPEN-FRAME whose calls
;;; have gone away
(defun flush-unused-call (node)
  (let ((primop (known-primop (call-proc node))))
    (when (and primop
               (or (and (eq primop primop/open-frame)
                        (null (leaf-value (call-arg-n 2 node))))
                   (and (not (primop.side-effects? primop))
                        (unused-call? node)
                        ;(bash-open node)
                        )))
      (let ((tree (debug :simp-tree (node-base node))))
        (replace-node node (detach (lambda-body (call-arg-n 1 node))))
        (debug :simp
          (format *debug-stream* "~&Flushing unused call: ~a" (pp-cps-2 node))
          (debug :simp-tree
            (pp-cps tree))))
      t)))

(defun unused-call? (node)
  (and (= 1 (call-exits node))
       (leaf-node? (call-proc node))
       (lambda-node? (relation (call-arg 1) node))
       (every #'(lambda (var)
                  (or (not var)
                      (null (variable-refs var))))
              (lambda-rest+variables (call-arg-n 1 node)))))

(defun side-effects? (proc)
  (let ((primop (known-primop proc)))
    (if primop
        (primop.side-effects? primop)
      t)))

#||||

;;; OBJ is an object-lambda.  The methods are searched to see if there is
;;; one corresponding to the procedure being called.  If so, the method is
;;; integrated.

(defun simplify-operation-dispatch (call obj)
  (destructure (((nil nil nil ops methods) obj))
    (let ((call-op (reference-variable (call-proc call))))
      (mapc #'(lambda (op method) ;iterate loop ((ops ops) (methods methods))
                (if (eq op call-op)
                    (return-from simplify-operation-dispatch
                      (replace-operation-with-method call method))))
            ops methods)
      nil)))

;;;  (<op> <cont> <object> . <args>) =>
;;;  (<method> <cont> <object>  . <args>)
;;; where <method> is <object>'s method for <op>.

(defun replace-operation-with-method (call method)
  (let ((new (create-call-node (1+ (length (call-args call))) 1)))
    (relate call-proc new (vector->node method))
    (relate-call-args new `(,(detach (call-arg-n 1 call))
                            . ,(mapcar #'detach (cdr (call-args call)))))
    (replace call new)
    t))



||#

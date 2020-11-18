;;; -*- Mode:Lisp; Package:NC; Readtable:CL; Base:10 -*-

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


;;; Creating nodes and CPS conversion
;;;============================================================================

;;; Convert EXP expecting a value.

(defun ->value-node (exp)
  (let ((exp-node (->node exp)))
    (if (call-node? exp-node)
        (bug "alpha-value got a call-node ~S for ~S" exp-node exp))
    exp-node))

;;; Convert EXP expecting a call.  Create a call node if necessary.

(defun ->call-node (exp)
  (multiple-value-bind (node c-parent c-role)
      (->node exp)
    (cond ((call-node? node)
           (values node c-parent c-role))
          (t
           (let ((call (create-call-node 2 0)))
             (relate (call-arg 1) call node)
             (values call call call-proc))))))

;;;   This performs dispatch for alpha and CPS conversion.  It could be called
;;; 'COERCE-TO-NODE' since EXP may include syntax-descriptors, primops, and
;;; node and variable structures as well as lists and symbols.

(defun ->node (exp &optional called?)
  (cond
        ((variable-p exp)
         (values (create-reference-node exp) nil nil))
        ((consp exp)
         (let ((proc (table-entry *compilator-table* (car exp))))
           (cond (proc
                  (funcall proc exp))
                 ((primop? (car exp))
                  ;this might be a useful
                  ;(not (primop.needs-open? (car exp)))
                  (make-call exp))
                 (t
                  (make-opened-call exp)))))
        ((primop? exp)
         (values (create-primop-node exp) nil nil))
        ((and (self-evaluating? exp)
              (not (and called?
                        (symbolp exp))))
         (values (create-literal-node exp) nil nil))
        ((symbolp exp)
         (values (locale-variable-reference exp) nil nil))
        ((node-p exp)
         (values exp nil nil))
        (t
         (->node (bug "uncompilable object~%  ~S" exp)))))


(defun locale-variable-reference (name)
  (let (
;       (support (or (support-lookup *new-support* name)
;                    (support-lookup *support* name)
;                  ))
        var)
    (cond ;(support
          ; (create-reference-node (support-variable support)))
          ((setq var (table-entry *free-variables* name))
           (create-reference-node var))
          (t
           (let ((var (create-variable name)))
             (setf (table-entry *free-variables* name) var)
             (create-reference-node var))))))

;;; Signal an error and return a reference to PRIMOP/UNDEFINED

;(define (alpha-error . rest)
;  (apply error rest)
;  (alpha-primop-reference primop/undefined))

;;;   Creates a lambda node.  Creates a variable list which includes a
;;; continuation variable.  The continuation variable is not added to the
;;; shape since it cannot be referenced by user code.  The body is alphatized.
;;; The continuation for the body is a reference to the continuation variable.

(defun make-lambda (proc-name vars body)
  (let ((node (create-lambda-node proc-name vars)))
    (multiple-value-bind (value-node c-parent c-role)
        (make-block body)
      (relate lambda-body node value-node)
      (relate c-role c-parent (create-reference-node (cadr vars)))
      (values node nil nil))))

;;; MAKE-OPENED-CALL and MAKE-CALL turn a call into CPS format.
;;; All the work is done by CPS-ARGS.

;;; add in the open as a call to the primop OPEN-FRAME
;;; it takes the call node as arg so it can find
;;; out where its continuation wants the value...
;;; (and what kind of continuation it is)
;;;   (foo (bar 3)) =>
;;;
;;;        ($OPEN-FRAME c_3 (quote (foo c_2 v_1))) ;this crock is a *compile time* arg to $OPEN-FRAME!
;;; ((c_3) (bar c_1 3))
;;;  ((c_1 nil v_1) (foo c_2 v_1))
;;;
(defun make-opened-call (exp)
  (multiple-value-bind (top call)
      (make-call exp)
    (let ((open-call (create-call-node 3 1))
          (open-cont (create-lambda-node 'c nil)))
      (relate call-proc open-call (create-primop-node primop/open-frame))
      (relate-call-args open-call open-cont (create-literal-node call)) ;crock
      (relate lambda-body open-cont top)
      ;;save back pointer to open to flush if this node is flushed
      (setf (call-open call) open-call)
      (values open-call call (call-arg 1)))))


(defun make-call (exp)
  (let* ((call (create-call-node (1+ (length exp)) 1))
         (top (cps-args call `(,(car exp) ,empty ,@(cdr exp)))))
    (values top call (call-arg 1))))

;;; Another front for CPS-ARGS.  This is called by various compilators.

(defun make-call-with-exits (exits args)
  (cps-args (create-call-node (length args) exits) args))

;;; Structure to hold information about arguments.

(defstruct arg
  node
;  rank
  index
  c-parent
  c-role)

;(define *argument-pool*
;  (make-pool '*argument-pool* make-arg 20 arg?))

(defun create-arg (node index c-parent c-role)
  (let ((arg (make-arg)))  ;(obtain-from-pool *argument-pool*)))
    (setf (arg-node arg) node)
;    (setf (arg-rank arg) (node-rank node))
    (setf (arg-index arg) index)
    (setf (arg-c-parent arg) c-parent)
    (setf (arg-c-role arg) c-role)
    arg))

(defun return-arg-list (args)
;  (do (arg args)
;    (return-to-pool *argument-pool* arg)
)

;;;   Alphatizes ARGS and adds their values to CALL (a call-node).  A
;;; continuation is introduced for each of the arguments that is a call.
;;;
;;; ((p a1 a2) q (r (s b1 b2 b3) t))
;;; =>
;;; (#empty#  (s C_1 b1 b2 b3))              ; Call to s
;;;   (C_1 () (V_2)  (r C_3 V_2 t))          ; Call to r
;;;     (C_3 () (V_4)  (p C_5 a1 a2))        ; Call to p
;;;       (C_5 () (V_6)  (V_6 #cont# q V_4)) ; Call to result of (p a1 a2)

(defun cps-args (call args)
  (let ((arguments (make-arg-nodes args))
        (top-node call))
    (dolist (arg arguments)
      (cond ((call-node? (arg-node arg))
             (let* ((c-var (create-variable "V"))
                    (l-node (create-lambda-node
                              "C" (list nil c-var))))
               (relate lambda-body l-node top-node)
               (relate (arg-c-role arg) (arg-c-parent arg) l-node)
               (relate (call-arg (arg-index arg))
                       call
                       (create-reference-node c-var))
               (setq top-node (arg-node arg))))
            ;; here we put in a lambda binding of a temporary
            ;; variable if the argument might be setqed
            ((and (reference-node? (arg-node arg))
                  (variable-setqs (reference-variable (arg-node arg)))
                  (not (eq (car args) PRIMOP/SETQ-LEXICAL)))
             (let* ((c-var (create-variable "V"))
                    (l-node (create-lambda-node
                              "C" (list nil c-var))))
               (relate lambda-body l-node top-node)
               (relate (call-arg (arg-index arg))
                       call
                       (create-reference-node c-var))
               (setq top-node (create-call-node 2 0))
               (relate call-proc top-node l-node)
               (relate (call-arg 1) top-node (arg-node arg))))
            (t (relate (call-arg (arg-index arg)) call (arg-node arg)))))
    (return-arg-list arguments)
    top-node))

;;; Alphatizes the list of arguments in EXP.  Returns a list of ARG structures.

(defun make-arg-nodes (exp)
    (do ((i 0 (1+ i))
         (args exp (cdr args))
         (vals '() (let ((arg (car args)))
                     (if (empty? arg)
                         vals
                       (cons (if (node-p arg)
                                 (create-arg arg i nil nil)
                               (multiple-value-bind (node c-parent c-role)
                                   (->node arg (zerop i))
                                 (create-arg node i c-parent c-role)))
                             vals)))))
        ((null args) vals)))


;;;   Alphatize a block.  This is guarenteed to return a call node.  Alphatizes
;;; each expression in turn.  TOP-CALL is the root of the tree for the block.
;;; VALUE is the node for the previous expression if it didn't alphatize to a
;;; call.  C-PARENT and C-ROLE are the latest continuation for the block.  The
;;; alphatized expressions are linked using n-ary continuation lambdas whose
;;; variable is not referenced.
;;;
;;; (BLOCK (p a1 a2 ...)
;;;        (q b1 b2 ...)
;;;        (r c1 c2 ...))
;;; =>
;;; (#empty#  (p B_1 a1 a2 ...))
;;;   (B_1 IGNORE_2 ()  (q B_3 b1 b2 ...))
;;;     (B_3 IGNORE_4 ()  (r #cont# c1 c2 ...))
;;;
;;; (BLOCK (p a1 a2 ...)
;;;        i              ;;; This will disappear since the value is not used.
;;;        (q b1 b2 ...)
;;;        j)
;;; =>
;;; (#empty# (p B_1 a1 a2 ...))
;;;   (B_1 IGNORE_2 ()  (q B_3 b1 b2 ...))
;;;     (B_3 IGNORE_4 ()  (#cont# j))

(defun make-block (exp-list)
  (let ((value nil)
        (top-call nil)
        (c-parent nil)
        (c-role nil))
    (dolist (exp exp-list)
      (if value (erase-all value))
      (multiple-value-bind (node n-parent n-role) (->node exp)
        (cond ((not (call-node? node))
               (setq value node))
              ((not top-call)
               (setq value nil
                     top-call node
                     c-parent n-parent
                     c-role n-role))
              (t
               (let ((l-node (create-lambda-node
                               'b
                               (list (create-variable 'ignore)))))
                 (relate lambda-body l-node node)
                 (relate c-role c-parent l-node)
                 (setq value nil
                       c-parent n-parent
                       c-role n-role))))))
    (cond ((and top-call (not value))
           (values top-call c-parent c-role))
          (t
           (finish-block value top-call c-parent c-role)))))

;;; Create a call node if there were no calls in the block.

(defun finish-block (value top-call c-parent c-role)
  (let ((call (create-call-node 2 0))
        (value (if value value (create-literal-node nil))))   ;(create-primop-node primop/undefined))))
    (relate (call-arg 1) call value)
    (cond ((not top-call)
           (values call call call-proc))
          (t
           (let ((l-node (create-lambda-node
                          'b
                          (list (create-variable 'ignore)))))
             (relate lambda-body l-node call)
             (relate c-role c-parent l-node)
             (values top-call call call-proc))))))


(defun make-thunk (node c-parent c-role)
  (let* ((var (create-variable 'k))
         (l-node (create-lambda-node 'c (list var))))
    (cond ((call-node? node)
           (relate lambda-body l-node node)
           (relate c-role c-parent (create-reference-node var)))
          (t
           (let ((call-node (create-call-node 2 0)))
             (relate call-proc call-node (create-reference-node var))
             (relate (call-arg 1) call-node node)
             (relate lambda-body l-node call-node))))
    l-node))

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

;;;  Optimization of CPS code tree

;;; Post-CPS code has these properties:
;;;   For every LAMBDA node L:
;;;     - L's body is a call.
;;;     - L's parent is a call, or else L is the top of the tree.
;;;   For every call node N:
;;;     - N's procedure and arguments are all non-calls.
;;;     - N's parent is a LAMBDA.

;;; (SIMPLIFY node-pair)
;;;============================================================================
;;;   Post-CPS optimizer.  All simplifications are done by changing the
;;; structure of the node tree.  NODE-PAIR is a pair whose CAR contains a
;;; leaf-node or a lambda-node.
;;;
;;; There are three requirements for the simplification procedures:
;;;    1) They must return T if the tree has been changed and NIL otherwise.
;;;    2) Only the node being simplified and its descendents may be changed.
;;;    3) If a node is changed the NODE-SIMPLIFIED? flag of that node and all
;;;       its ancestors must be set to NIL.

(defun simplify (node-pair)
  (let ((node (car node-pair)))
    (unless (node-simplified? node)
      (do ((node node (car node-pair)))
          ((not (cond ((lambda-node? node) (simplify-lambda node))
;                     ((leaf-node?   node) (simplify-leaf   node))
                      ))))
      (setf (node-simplified? node) t))))



;;; (SIMPLIFY-LAMBDA node)
;;;============================================================================
;;;     Simplify a lambda node.
;;; (lambda () (x)) => x if the node is an exit.

(defun simplify-lambda (node)
  (simplify-call node)
  (when (and (or (call-exit? node)
                 (let-node? (node-parent node)))
             (not (lambda-rest-var node))
             (null (lambda-variables node))
             (null (call-args (lambda-body node)))
             (reference-node? (call-proc (lambda-body node)))
             (variable-binder
               (reference-variable (call-proc (lambda-body node)))))
    (replace-node node (detach (call-proc (lambda-body node))))
    t))

(defun let-node? (node)
  (and (call-node? node)
       (lambda-node? (call-proc node))))

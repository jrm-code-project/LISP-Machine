;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

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

;;; (lambda (x) ... x ... (set x ...) ...)
;;;   ==>  (lambda (x)
;;;          (let ((x' (make-cell x)))
;;;             ... (contents x') ... (set-contents x' ...) ...))

(defun introduce-cell (var)
  (let* ((node (variable-binder var))
         (new-v (create-variable (variable-name var)))
         (cont (create-lambda-node 'c (list nil new-v)))
         (call (create-call-node 3 1)))
    (if (not (some #'identity (map-refs-safely #'(lambda (ref)
                                                   (hack-reference ref new-v))
                                               var)))
        (delayed-user-message 'warning
                              (variable-binder var)
                              "variable ~S is set but never referenced"
                              nil
                              (variable-name var)))
    (relate call-proc call (create-primop-node primop/make-cell))
    (relate-call-args call cont (create-reference-node var))
    (insert-call call cont node)))

;;; Replace references to variables with indirections through locatives.
;;;   x              ==>  (contents x')
;;;   (set x y)      ==>  (set-contents x' y)
;;;   (locative x)   ==>  x'
;;; Returns T if the variable is being used, NIL if it is being set.

(defun hack-reference (ref new-var)
  (let* ((parent (node-parent ref))
         (proc (call-proc parent)))
    (cond ((or (not (eq (node-role ref) (call-arg 2)))
               (not (primop-node? proc)))
           (dereference parent ref new-var)
           t)
          ((eq (primop-value proc) primop/setq)             ;*set-var)
           (replace-node parent (assigner parent new-var))
           nil)
          ((eq (primop-value proc) primop/*locative)
           (replace-call-with-value parent
                                    (create-reference-node new-var))
           t)
          (t
           (dereference parent ref new-var)
           t))))

;;;  x ==> (contents-location cell-value x')

(defun dereference (call ref new-var)
  (let* ((new-v (create-variable (variable-name new-var)))
         (l-node (create-lambda-node 'c (list nil new-v)))
         (new-call (create-call-node 4 1)))
    (relate call-proc new-call (create-primop-node primop/contents-location))
    (relate-call-args new-call
                      l-node
                      (create-primop-node primop/cell-value)
                      (create-reference-node new-var))
    (insert-call new-call l-node (node-parent call))
    (replace-node ref (create-reference-node new-v))))

;;;  (set x y) ==> (set-location cell-value y x')

(defun assigner (call new-var)
  (let ((node (create-call-node 5 1)))
    (relate call-proc node (create-primop-node primop/set-location))
    (relate-call-args node
                      (detach (call-arg-n 1 call))
                      (create-primop-node primop/cell-value)
                      (detach (call-arg-n 3 call))
                      (create-reference-node new-var))
    node))

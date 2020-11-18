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

;;; Trace analysis.

;;; Trace analysis is used to help order the code blocks
;;; made by the generator.  Calls to lambda nodes that are
;;; closer are deemed more likely (or something along these
;;; lines)

;;; This code seems to work fine and can probably be safely
;;; ignored.

(defun db (depth trace) (cons depth trace))

(defun lambda-depth (lam) (car (lambda-db lam)))
(defun lambda-trace (lam) (cdr (lambda-db lam)))

(defun trace-analyze-top (top-node)
  (trace-analyze-lambda (call-arg-n 1 (lambda-body top-node)) 0 0))

(defun trace-analyze-lambda (node depth trace)
  (setf (lambda-db node) (db depth trace))
  (let ((inner-trace-number (trace-analyze-call (lambda-body node) depth trace)))
;    (mapc #'sort-by-db (if (continuation? node)
;                          (lambda-variables node)
;                          (cdr (lambda-variables node))))
    (+ inner-trace-number 1)))

(defun trace-analyze-call (node depth trace)
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (let ((value (primop-value proc)))
             (cond
               ((eq value primop/conditional)
                (trace-analyze-if node depth trace))
               ((eq value primop/Y)
                (trace-analyze-y (call-arg-n 1 node) depth trace))
;              ((eq value primop/undefined-effect)
;               trace)
               (t
                (really-trace-analyze-call (call-args node) depth trace)))))
          ((lambda-node? proc)
           (trace-analyze-let node depth trace))
          (t (really-trace-analyze-call (call-args node) depth trace)))))



(defun really-trace-analyze-call (args depth trace)
  (do ((trace trace (if (lambda-node? (first args))
                        (trace-analyze-lambda (first args) (1+ depth) trace)
                        trace))
       (args args (rest args)))
      ((null args) trace)))


(defun trace-analyze-if (node depth trace)
  (multiple-value-bind (preferred-path other-path)
      (determine-preferred-path (call-arg-n 1 node)
                                (call-arg-n 2 node))
;    (letrec ((trace-if-path (lambda (path trace)
;                             (if (lambda-node? path)
;                                 (trace-analyze-lambda path (+ depth 1) trace)
;                                 trace))))
;       (trace-if-path other-path (trace-if-path preferred-path trace)))))
    (let ((p-trace (if (lambda-node? preferred-path)
                       (trace-analyze-lambda preferred-path (1+ depth) trace)
                       trace)))
      (if (lambda-node? other-path)
          (trace-analyze-lambda other-path (1+ depth) p-trace)
          p-trace))))

(defun determine-preferred-path (th el)
  (cond ((leaf-node? th) (values el th))
        ((leaf-node? el) (values th el))
        (t
         (let ((th-body (lambda-body th))
               (el-body (lambda-body el)))
           (cond ((zerop (call-exits th-body))
                  (if (and (leaf-node? (call-proc th-body))
                           (variable-known (leaf-value (call-proc th-body))))
                      (values th el)
                      (values el th)))
                 ((zerop (call-exits el-body))
                  (if (and (leaf-node? (call-proc el-body))
                           (variable-known (leaf-value (call-proc el-body))))
                      (values el th)
                      (values th el)))
                 ((primop-node? (call-proc th-body))
                  (values th el))
                 (t
                  (values el th)))))))

(defun trace-analyze-let (let-node depth trace)
;  (if (lambda-rest-var (call-proc let-node))
;      (bug "rest arg in let is not implemented"))
  (really-trace-analyze-call (call-proc+args let-node) depth trace))

(defun trace-analyze-y (node depth trace)
  (really-trace-analyze-call (call-args (lambda-body node)) depth trace))


(defun sort-by-db (var)
  (when var
    (setf (variable-refs var)
          (sort (variable-refs var)
                #'(lambda (ref1 ref2)
                    (let ((l1 (node-parent (node-parent ref1)))
                          (l2 (node-parent (node-parent ref2))))
                      (cond ((< (lambda-trace l1) (lambda-trace l2)) t)
                            ((> (lambda-trace l1) (lambda-trace l2)) nil)
                            (t
                             (<= (lambda-depth l1) (lambda-depth l2))))))))))

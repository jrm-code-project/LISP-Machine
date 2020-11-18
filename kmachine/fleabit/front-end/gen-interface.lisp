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

;;; Interface between the front end and the code generator (in addition to the
;;; node tree).

(defun defined-variable? (var)
  (and (variable-support var)
       (eq *new-support* (support-table (variable-support var)))))

(defun defined-variable-value (var)
  (let ((support (variable-support var)))
    (if support
        (get-variable-definition var (support-variant support)))))

(defun get-variable-definition (var variant)
  (dolist (ref (variable-refs var) nil)
    (if (eq variant (supports-definition ref))
        (return (let ((val (call-arg-n 3 (node-parent ref))))
                  (if (lambda-node? val) val nil))))))


(defun defined-variable-exported? (var)
  (declare (ignore var))
  t)

(defun defined-variable-variant (var)
  (let ((support (variable-support var)))
    (cond ((not support)
           nil)
          ((eq (support-variant support) 'multiple)
           'define)
          (t
           (support-variant support)))))

(defun supported? (var)
  (variable-support var))

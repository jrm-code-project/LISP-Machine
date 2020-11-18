;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

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

;;; An extremely minimal type system
;;;
;;; A type is one of NIL, 'OBJECT, 'LITERAL '(PROC <n-ary?> <nargs>)

(defun get-support-type (value)
  (cond ((and (consp value)             ; Ugh!
              (eq (car value) 'object))
         'object)
        ((not (node-p value))
         nil)
        ((primop-node? value)
         (fix-primop-type (primop.type (primop-value value) value)))
        ((reference-node? value)
         nil)
        ((literal-node? value)
         'literal)
        ((lambda-node? value)
         `(proc ,(if (lambda-rest-var value) t nil)
                ,(length (lambda-variables value))))
        (t nil)))

;;; Primops must keep the types they have to get closed-compiled versions.

(defun fix-primop-type (old-type)
  (if (consp old-type)
      `(proc nil ,(length (cdr old-type)))
      '(proc t 0)))

(defun primop-type-check (primop call)
  (let ((type (primop.type primop call)))
    (or (not (consp type))
        (eq (length (call-args call))
            (length (cdr type))))))

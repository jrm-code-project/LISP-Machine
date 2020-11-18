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


(defstruct (expanding-vector (:print-function (lambda (struct stream depth)
                                                (format stream "#{expanding-vector ~S ~O}"
                                                        (expanding-vector-id struct)
                                                        (object-hash struct))))
                             (:constructor %make-expanding-vector (array id init-fcn vref-fcn)))
  array
  id
  init-fcn
  vref-fcn)

(zl:defsubst recycle (vect)
  (declare (ignore vect)))

(zl:defsubst vref (vect index)
  (funcall (expanding-vector-vref-fcn vect) vect index))

(defun expanding-vref (vect index)
  (aref (expanding-vector-array vect) index))

(defun infinite-vref (vect index)
  (let ((array (expanding-vector-array vect)))
    (if (>= index (length array))
        (expand-array array index (length array) (expanding-vector-init-fcn vect)))
    (aref array index)))

(defun vset (vect index value)
  (let ((array (expanding-vector-array vect)))
    (if (>= index (length array))
        (expand-array array index (length array) (expanding-vector-init-fcn vect)))
    (setf (aref array index) value)))


(defsetf vref vset)

(defun expand-array (array index size init-fcn)
  (adjust-array array (list (1+ index)))
  (do ((i size (1+ i)))
      ((> i index))
    (setf (aref array i) (if init-fcn (funcall init-fcn i) 0))))


(defun make-expanding-vector (start-size &optional id)
  (%make-expanding-vector (make-array start-size :adjustable t)
                          id nil #'expanding-vref))

(defun make-infinite-vector (start-size init-fcn &optional id)
  (let* ((array (make-array start-size :adjustable t)))
    (expand-array array (1- start-size) 0 init-fcn)
    (%make-expanding-vector array id init-fcn #'infinite-vref)))

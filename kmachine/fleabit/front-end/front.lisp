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

(defvar *support*)
(defvar *new-support* (make-empty-support-table '*new-support*))
(defvar *syntax* nil)
(defvar *shape* nil)

(defmacro front-init (&body cont)
  `(let ((*new-support* (make-empty-support-table t)))     ;'#t)))
     ,@cont))

(defun make-code-tree+support (exp support syntax)
  (multiple-value-bind (nodes support)
      (do-exps (cddr (caddr exp)) support syntax)
    (values (rebuild nodes) support))) ;(support-table->supex support))))

(defun write-support-file (supex filename)
  (noise "Writing support file~%")
  (noise "Needed support tables: ~S" supex) ; (supex-needed supex))
#|
  (with-open-streams ((output (open (support-filename filename) 'dump)))
    (write output `(,(supex-needed supex)
                    ,(if (supex-primops supex) '#t '#f)
                    ,(supex-support supex))))
  (cond ((supex-primops supex)
         (noise "Writing primop file~%")
         (with-open-streams ((output (open (primop-write-filename filename)
                                           '(out))))
           (write output
                  `(herald ,(filename-name (primop-write-filename filename))))
           (newline output)
           (walk (lambda (exp)
                   (pretty-print exp output)
                   (format output "~%"))
                 (supex-primops supex)))))
|#
  t)

#||||||
(define (support-filename filename)
  (filename-with-type filename *information-file-extension*))

(define (primop-write-filename filename)
  (make-filename (filename-fs filename)
                 (filename-dir filename)
                 (concatenate-symbol (filename-name filename) '_primops)
                 't))

(define (primop-read-filename filename)
  (make-filename (filename-fs filename)
                 (filename-dir filename)
                 (concatenate-symbol (filename-name filename) '_primops)))


;(define (write-support-env env file)
;  (with-open-streams ((output
;                       (open (support-filename (->filename file)) 'dump)))
;    (write output (map identification (subordinate-tables env)))
;    (map (lambda (table)
;           (let ((supex (support-table->supex table)))
;             (write output `(,(supex-needed supex)
;                           ,(if (supex-primops supex) '#t '#f)
;                           ,(supex-support supex)))))
;         (subordinate-tables env))))

;(define (read-support-env name file)
;  (with-open-streams ((input
;                       (open-retrieve (support-filename (->filename file)))))
;    (let ((modules (read input)))
;      (walk (lambda (name)
;              (let* ((exp (read input))
;                     (tables (map (lambda (name)
;                               `(,name . ,(get-support-table name)))
;                             (cons '() (car exp)))))
;                (expression->support-table exp name tables)))
;            modules)
;      (make-flat-support-env name (map get-support-table modules)))))

(define (orbit-vax-init . directory)
  (orbit-vax-setup (if directory (car directory) '#f))
  (orbit-init 'base
              'constants
              'primops
              'arith
              'locations
              'low
              'open
              'aliases
              'carcdr))

(define (orbit-m68-init . directory)
  (orbit-m68-setup (if directory (car directory) '#f))
  (orbit-init 'base
              'constants
              'primops
              'arith
              'locations
              'low
              'open
              'aliases
              'carcdr))

||||||#


(defvar *known-primops*
      '(*primop
        undefined
        undefined-effect
        Y
        conditional
        test
        true?
        *set-var
        *locative
        *define
        *lset
        *define-constant
        proc+handler
        contents-location
        set-location
        make-cell
        cell-value))

(defvar *unshared-primops*
      '(*primop
        Y
        conditional
        test
        *set-var
        *locative
        *define
        *lset
        *define-constant
        contents
        proc+handler
        contents-location
        set-location))

(defvar *base-support-env* (make-empty-support-env '*empty-env*))

;(load '(osys new_exports))

#|||||
(defun orbit-init (init-module &rest modules)
  (orbit-uninit)
  (let ((*noise-stream* *terminal-io*))
    (setq *initial-primop-env* (get-support-table init-module))
    (mapc #'(lambda (s)
              (setf (*value *orbit-env* (concatenate-symbol 'primop/ s))
                    (get-primop s *initial-primop-env*)))
          *known-primops*)
    (mapc #'(lambda (s)
              (setf (*initial-primop-env* s) nil))
          *unshared-primops*)
    (setq *base-support-env*
          (make-flat-support-env '*base-support-env*
                                 (mapcar #'get-support-table
                                         `(,init-module . ,modules))))
    (let ((table (make-empty-support-table '*standard-support*)))
      (mapc #'(lambda (name)
                (setf (support-lookup table name) (*base-support-env* name)))
            *t-exports*)
      (setq *standard-support-env*
            (make-support-env '*standard-support-env* (list table))))
    *standard-support-env*))

(defun get-primop (name support-env)
  (let ((support (lookup support-env name)))
    (if (and support
             (primop? (support-value support)))
        (support-value support)
        (bug "no support for ~S" name))))

(defun orbit-uninit ()
  (clean-table *support-tables*)
  (clean-table *constructed-primops*) ; Remove pointers to old nodes
  (mapc #'(lambda (s)
            (setf (*value *orbit-env* (concatenate-symbol 'primop/ s))
                  nil))     ;'#f))
        *known-primops*)
  (setq *initial-primop-env* (make-empty-support-env 'nil))
  (setq *standard-support-env* (make-empty-support-env 'nil)))

(defun clean-support-tables ()
  (table-walk *support-tables*
    #'(lambda (name val)
      (walk-support val
        #'(lambda (name support)
            (cond ((support-p support)
                   (setf (variable-refs (support-variable support))
                         '()))
                  ((variable-p support)
                   (setf (variable-refs support) '()))
                  (t
                   (bug "odd support entry ~S" support)))))))
  t)

|||||#

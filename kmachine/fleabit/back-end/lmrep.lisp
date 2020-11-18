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

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
(define (rep-analyze-top node)
  (rep-analyze ((call-arg 1) (lambda-body node)))
  (rep-analyze ((call-arg 1) (lambda-body node))))

(define (rep-analyze node)
  (cond ((lambda-node? node)
         (rep-analyze-call (lambda-body node))
         (xselect (lambda-strategy node)
           ((strategy/stack strategy/heap strategy/vframe strategy/ezclose))
           ((strategy/label strategy/open)
            (walk (lambda (var)
                    (if (eq? (variable-rep var) 'rep/pointer)
                        (set (variable-rep var) (most-important-rep var))))
                  (if (continuation? node)
                      (lambda-variables node)
                      (cdr (lambda-variables node)))))))))


(define (rep-analyze-call node)
  (let ((proc (call-proc node)))
    (cond ((lambda-node? proc)
           (walk rep-analyze (call-args node)))
          ((primop-ref? proc primop/Y)
           (destructure (((body . procs) (call-args (lambda-body ((call-arg 1) node)))))
             (walk rep-analyze procs)
             (rep-analyze body)))
          ((and (primop-ref? (call-proc node) primop/contents-location)
                (lambda-node? ((call-arg 1) node)))
           (set (variable-rep (lambda-cont-var ((call-arg 1) node)))
                (primop.rep-wants (leaf-value ((call-arg 2) node))))
           (walk rep-analyze (call-args node)))
          (else
           (walk rep-analyze (call-args node))))))



(define (most-important-rep var)
  (iterate loop ((refs (variable-refs var)))
    (cond ((null? refs)
           'rep/pointer)
          (else
           (let* ((parent (node-parent (car refs)))
                  (proc (call-proc parent))
                  (number (call-arg-number (node-role (car refs)))))
             (cond ((primop-node? proc)
                    (cond ((primop.rep-wants (primop-value proc))
                           => (lambda (reps)
                                (nth reps (fx- (fx- number
                                                    (call-exits parent))
                                               1))))
                          ((eq? (primop-value proc) primop/contents-location)
                           (if (and (fx= number 4)
                                    (neq? (rep-size (primop.rep-wants
                                             (leaf-value ((call-arg 2) parent))))
                                          size/long))
                               'rep/integer 'rep/pointer))
                          ((eq? (primop-value proc) primop/set-location)
                           (cond ((and (fx= number 5)
                                       (neq? (rep-size (primop.rep-wants
                                               (leaf-value ((call-arg 2) parent))))
                                            size/long))
                                  'rep/integer)
                                 ((fx= number 3)
                                  (primop.rep-wants
                                      (leaf-value ((call-arg 2) parent))))
                                 (else 'rep/pointer)))
                          ((eq? (primop-value proc) primop/conditional)
                           (loop (cdr refs)))
                          (else
                           (let ((cont ((call-arg 1) parent)))
                             (cond ((or (leaf-node? cont)
                                        (null? (lambda-variables cont)))
                                    (loop (cdr refs)))
                                   (else
                                    (let ((rep (variable-rep (lambda-cont-var cont))))
                                      (if (neq? rep 'rep/pointer)
                                          rep
                                          (loop (cdr refs))))))))))
                   ((variable-known (leaf-value proc))
                    => (lambda (label)
                         (cond ((lambda-rest-var label)
                                (loop (cdr refs)))
                               (else
                                (variable-rep (nth (lambda-variables label)
                                         (fx- number 1)))))))
                   (else
                    'rep/pointer)))))))



||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defconstant *reps*
             '(rep/pointer))

;                rep/char
;               rep/extend
;               rep/integer
;               rep/integer-8-s
;               rep/integer-8-u
;               rep/integer-16-s
;               rep/integer-16-u
;               rep/string
;               rep/pointer))

;(define-constant size/byte 1)
;(define-constant size/word 2)
;(define-constant size/long 4)


(defconstant *rep-converter-table* (make-table 'reps))

(dolist (rep *reps*)
  (setf (table-entry *rep-converter-table* rep)
        (make-table rep)))

(defun rep-size (rep)
  1) ;  size/long)

;  (case rep
;    ((rep/char rep/integer-8-u rep/integer-8-s) size/byte)
;    ((rep/integer-16-u rep/integer-16-s) size/word)
;    (else size/long)))



(defmacro define-rep-converter (from to proc)
  `(setf (table-entry (table-entry *rep-converter-table* ',to) ',from)
        ,proc))




;------------------------------------------


(defun rep-converter (from-rep to-rep)
  (table-entry (table-entry *rep-converter-table* to-rep) from-rep))


(defun really-rep-convert (node from from-rep to to-rep)
  (let ((converter (rep-converter from-rep to-rep)))
    (cond (converter
           (funcall converter node from to))
          ((eq to-rep 'rep/pointer)
           (generate-move from to))
          ((not (eq from to))
           (emit lm/move (m68-size to-rep) from to)))))



#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
(define (rep-push node value to-rep)
  (cond ((addressable? value)
         (select (rep-size to-rep)
           ((size/long)
            (generate-push (value-with-rep value to-rep)))
           ((size/word)
            (emit m68/pea (@r 8))
            (generate-move-word (value-with-rep value to-rep) (@r 15)))
           ((size/byte)
            (emit m68/pea (@r 8))
            (generate-move-byte (value-with-rep value to-rep) (@r 15)))))
        (else
         (let ((access (access-value node value))
               (from-rep (variable-rep value)))
           (cond ((eq? from-rep to-rep)
                  (generate-push access))
                 ((eq? to-rep 'rep/extend)
                  (emit m68/move .l access (@-r 15))
                  (emit m68/add .l (machine-num tag/extend) (@r 15)))
                 ((eq? to-rep 'rep/value)
                  (generate-push access))
                 ((neq? (rep-size to-rep) size/long)
                  (emit m68/pea (@r 8))
                  (really-rep-convert node access from-rep (@r 15) to-rep))
                 (else
                  (really-rep-convert node access from-rep (@-r 15) to-rep))))))
  (increment-stack))


(define (value-with-rep value rep)
  (xcond ((char? value)
          (xcond ((eq? rep 'rep/char)
                  (machine-num (char->ascii value)))
                 ((eq? rep 'rep/pointer)
                  (machine-num (fixnum-logior (fixnum-ashl (char->ascii value) 8)
                                              header/char)))))
         ((fixnum? value)
          (cond ((eq? rep 'rep/pointer)
                 (lit value))
                (else
                 (machine-num value))))
         ((eq? value '#T)
          (machine-num header/true))
         ((or (eq? value '#F) (eq? value '()))
          nil-reg)))


||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

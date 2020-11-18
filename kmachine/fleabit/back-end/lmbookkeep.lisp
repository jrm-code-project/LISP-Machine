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

;(define-constant *pointer-registers* 5)
;(define-constant *scratch-registers* 6)
;(define-constant *argument-registers* 3)
;(define-constant *real-registers* 11)
;(define-constant *pointer-temps* 15)
;(define-constant *no-of-registers* 31)

;;; registers < *scratch-registers* are scratch
(defconstant *scratch-registers* 0)
;;; number of argument registers
(defconstant *argument-registers* 16)
(defconstant *real-registers* 50) ;???
(defconstant *no-of-registers* 50) ;???
(defconstant *pointer-temps* 0) ;???

(defconstant *frame-size* 16)

(defconstant A0 0)
(defconstant AN (+ A0 *frame-size* -1))
(defconstant AN-1 (1- AN))
(defconstant O0 (+ A0 *frame-size*))
(defconstant ON (+ O0 *frame-size*))
(defconstant ON-1 (1- ON))
(defconstant R0 (+ O0 *frame-size*))
(defconstant RN (+ R0 *frame-size*))
(defconstant RETURN  RN)
(defconstant P       (1+ RETURN))
(defconstant TP -1)


;(define-constant S0 0)
;(define-constant S1 1)
;(define-constant S2 2)
;(define-constant S3 3)
;(define-constant S4 4)
;(define-constant S5 5)
;(define-constant NARGS 5)
;(define-constant P 6)
;(define-constant A1 7)
;(define-constant A2 8)
;(define-constant A3 9)
;(define-constant AN 10)
;(define-constant AN-1 9)
;(define-constant TP -1)
;(define-constant nil-reg -2)
;(define-constant SP -3)
;(define-constant TASK -4)
;(define-constant SCRATCH -5)


(defconstant *open-registers*
             (let ((posl ()))
               (dotimes (i *frame-size*)
                 (push (cons (+ O0 i) 'rep/pointer) posl))
               (nreverse posl)))   ; (cons (cons P 'rep/pointer)

(defconstant *return-registers*
             (let ((posl ()))
               (dotimes (i *frame-size*)
                 (push (cons (+ R0 i) 'rep/pointer) posl))
               (nreverse posl)))

(defconstant *return-dest* (list (cons RETURN 'rep/pointer)))

(defvar *pos-list1* (make-array (1+ *argument-registers*)))
(defvar *pos-list2* (make-array (+ *argument-registers* 2)))


(dotimes (i (1+ *argument-registers*))
  (let ((posl '()))
    (dotimes (r i)
      (push (cons r 'rep/pointer) posl))
    (setf (svref *pos-list1* i) (nreverse posl))))


(dotimes (i (1+ *argument-registers*))
  (let ((posl '()))
    (dotimes (r (1- i))
      (push (cons r 'rep/pointer) posl))
    (setf (svref *pos-list2* i) (if (zerop i) nil (cons (cons p 'rep/pointer) (nreverse posl))))))


(defun reg-positions (i proc?)
  (cond (proc?
         (cond ((<= i  (1+ *argument-registers*))
                (svref *pos-list2* i))
               (t
                (append (svref *pos-list2* (1+ *argument-registers*))
                        (make-num-list (- i (1+ *argument-registers*)))))))
        (t
         (cond ((<= i *argument-registers*)
                (svref *pos-list1* i))
               (t
                (append (svref *pos-list1* *argument-registers*)
                        (make-num-list (- i *argument-registers*))))))))


(defun make-num-list (amount)
  (let ((end (+ (+ *real-registers* *argument-registers*) amount)))
    (do ((i (+ *real-registers* *argument-registers*) (1+ i))
         (l '() (cons (cons i 'rep/pointer) l)))
        ((>= i end) (nreverse l)))))



(defun addressable? (val)
  (or (eq val t)
      (eq val nil)
      (eq (zl:data-type val) 'zl:dtp-fix)
      (eq (zl:data-type val) 'zl:dtp-character)))

(defun rep-size (rep)
  1)

#|||||
(define (do-trivial-lambda call-node node reg-rep)
  (let ((offset (environment-cic-offset (lambda-env node))))
    (cond ((fx= offset 0)
           (generate-move AN (car reg-rep)))
          (else
           (generate-move-address-hack (reg-offset AN offset)
                                       (car reg-rep)
                                       TP)))
    (cond ((reg-node (car reg-rep))
                => kill))
    (lock (car reg-rep))))

||||||#

;;; MAKE-HEAP-CLOSURE The first member of the closure corresponds to the
;;; template so we call %make-extend with this template and the size of the
;;; closure to be created.  Then we fill in the slots with the need variables
;;; and the addresses of templates for any closure-internal-closures.

(defun make-heap-closure (node closure)
  (if *assembly-comments?* (emit-comment "consing heap closure"))
  (let* ((members (closure-members closure))
         (template-binder (variable-binder (car members))))
    (dolist (var members)
      (lambda-queue (variable-binder var)))
    (free-register node AN)
    (let ((cl (environment-closure (lambda-env template-binder))))
      (cond ((closure-cit-offset cl)
             (let ((acc (lookup node cl nil)))
               (free-register node AN)
               (generate-move acc AN)))
            (t
             (generate-move-address (template template-binder) AN))))
    (lock AN)
    (let ((hack (generate-extend node (closure-size closure))))
      (lock hack)
      (dolist (pair (cdr (closure-env closure)))
        (let ((var (car pair))
              (offset (cdr pair)))
          (cond ((eq var *dummy-var*))
                ((member var members :test #'eq)
                 (generate-move-address-hack (template (variable-binder var))
                                             (reg-offset AN
                                                    (- offset tag/extend))
                                              hack))
                (t
                 (really-rep-convert node
                                     (access-value node var)
                                     (variable-rep var)
                                     (reg-offset AN
                                                 (- offset tag/extend))
                                     (variable-rep var))))))
      (unlock hack))
    (unlock AN)))

(defun generate-extend (node n)
  (emit lm/make-extend (machine-num (- n CELL)))        ;fix this
  (get-register 'pointer node '*))

;  (free-register node S1)
;  (free-register node S2)
;  (generate-move (machine-num (- n CELL)) S1)   ;; don't include template
;  (let ((reg (get-register 'pointer node '*)))
;    (slink-is reg)
;    (emit m68/jsr (reg-offset reg slink/make-extend))
;    reg))

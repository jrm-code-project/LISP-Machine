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

;;; Copyright (c) 1985 David Kranz

(defun generate-set-location (node)    ;; cont type-primop value . args
  (ecase (length (call-args node))
    (4 (generate-set-fixed-accessor node))
    (5 (generate-set-vector-elt node))))



(defun generate-set-fixed-accessor (node)
  (destructure (((nil type value loc) (call-args node)))
   (let ((prim (leaf-value type)))
    (cond ((lambda-node? value)
           (let ((access (access/make-closure node value)))
             (if access (protect-access access) (lock AN))
             (cond ((and (eq prim primop/cell-value)
                         (eq (variable-support (leaf-value loc)) 'one))
                    (kill (leaf-value loc)) ; force into closure
                    (emit lm/move (access-value node (leaf-value loc)) AN))
                   (t
                    (let ((reg (->register 'pointer node (leaf-value loc) '*)))
                      (generate-move (if access access AN)
                            (reg-offset reg (primop.location-specs prim))))))
             (if access (release-access access) (unlock AN))))
          (t
           (let ((access (access-with-rep node (leaf-value value) 'rep/pointer)))
             (protect-access access)
             (cond ((and (eq prim primop/cell-value)
                         (member (variable-support (leaf-value loc)) '(one nil)))  ;eq 'one
                    (let ((lc (access-value node (leaf-value loc))))
                      (generate-move access lc)
                      (let ((lc (and (register? lc) (temp-loc (leaf-value loc)))))
                        (cond (lc
                               (setf (temp-node lc) nil)
                               (setf (temp-loc (leaf-value loc)) nil))))))
                   (t
                    (let ((reg (->register 'pointer node (leaf-value loc) '*)))
                      (emit lm/move (reg-offset reg (primop.location-specs prim))
                            access))))
             (release-access access)))))))


#|||


(define (generate-set-vector-type-length node)
  (destructure (((#f vec val) (call-args node)))
    (let ((reg (->register 'pointer node (leaf-value vec) '*))
          (val (leaf-value val)))
      (lock reg)
      (let ((scratch (get-register 'scratch node '*)))
        (cond ((variable? val)
               (generate-move (access-value node val) scratch)
               (emit m68/asl .l
                     (machine-num (if (eq? (variable-rep val) 'rep/pointer) 6 8))
                     scratch))
              (else
               (emit m68/move .l (machine-num (fixnum-ashl val 8)) scratch)))
        (emit m68/move .b (reg-offset reg 1) scratch)
        (emit m68/move .l scratch (reg-offset reg -2))
        (unlock reg)))))



(define (generate-set-vector-elt node)
  (destructure (((#f type value loc idex) (call-args node)))
    (let ((idex (leaf-value idex))
          (rep (primop.rep-wants (leaf-value type))))
      (cond ((eq? rep 'rep/pointer)
             (let* ((access (if (lambda-node? value)
                                (access/make-closure node value)
                                (access-value node (leaf-value value))))
                    (value-acc (if access access AN)))
               (if access (protect-access access) (lock AN))
               (let* ((i-acc (access-with-rep node idex 'rep/pointer))
                      (i-reg (cond ((register? i-acc) i-acc)
                                   (else
                                    (emit m68/move .l i-acc SCRATCH)
                                    SCRATCH)))
                      (reg (->register 'pointer node (leaf-value loc) '*)))
                 (generate-move value-acc (indexer reg tag/extend i-reg))
                 (if access (release-access access) (unlock AN)))))
            (else
             (let* ((i-acc (access-with-rep node idex 'rep/integer))
                    (i-reg (cond ((register? i-acc) i-acc)
                                 (else
                                  (let ((i (get-register 'scratch node '*)))
                                    (emit m68/move .l i-acc i)
                                    i))))
                    (reg (->register 'pointer node (leaf-value loc) '*))
                    (value (leaf-value value)))
                 (lock i-reg)
                 (lock reg)
                 (cond ((variable? value)
                        (let ((acc (access-value node value)))
                          (protect-access acc)
                          (really-rep-convert node acc (variable-rep value)
                                   (indexer reg tag/extend i-reg)
                                   rep)
                          (release-access acc)))
                       (else
                        (really-rep-convert node (value-with-rep value rep)
                                            rep
                                            (indexer reg tag/extend i-reg)
                                            rep)))
                 (unlock i-reg)
                 (unlock reg)))))))


|||#

(defun generate-contents-location (node)
  (ecase (length (call-args node))
     (3 (generate-fixed-accessor node))
     (4 (generate-vector-elt node))))


(defun generate-fixed-accessor (node)
  (destructure (((cont type loc) (call-args node)))
   (if (or (leaf-node? cont) (used? (car (lambda-variables cont))))
       (multiple-value-bind (t-spec t-rep) (continuation-wants cont)
         (let* ((type (leaf-value type))
                (base (leaf-value loc))
                (target (get-target-register node t-spec)))
           (cond ((and (eq type primop/cell-value)
                       (member (variable-support base) '(one nil)))   ;eq 'one
                  (really-rep-convert node (access-value node base)
                                      'rep/pointer target t-rep))
                 (t
                  (let ((reg (->register 'pointer node base '*)))
                    (really-rep-convert node
                               (reg-offset reg (primop.location-specs type))
                               'rep/pointer target t-rep))))
           (let ((node (reg-node target)))
             (cond (node (setf (register-loc node) nil))))
           (mark-continuation node target))))))


#|||




(define (generate-vector-type-length node)
  (destructure (((cont vec) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((base (leaf-value vec))
             (target (get-target-register node t-spec))
             (reg (->register 'pointer node base '*))
             (temp (if (eq? (reg-type target) 'scratch) target SCRATCH)))
        (emit m68/move .l (reg-offset reg -2) temp)
        (emit m68/asr .l (machine-num 8) temp)
        (if (eq? t-rep 'rep/pointer)
            (emit m68/asl .l (machine-num 2) temp))
        (generate-move temp target)
        (cond ((reg-node target)
               => (lambda (node) (set (register-loc node) nil))))
        (mark-continuation node target)))))



(define (generate-vector-elt node)
  (destructure (((cont type loc idex) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((base (leaf-value loc))
             (rep (primop.rep-wants (leaf-value type)))
             (idex (leaf-value idex))
             (t-reg (get-target-register node t-spec))
             (reg (->register 'pointer node base '*)))
        (lock reg)
        (cond ((fixnum? idex)
               (really-rep-convert node
                        (reg-offset reg (fx+ (if (eq? rep 'rep/pointer)
                                                 (fx* idex 4)
                                                 idex)
                                              tag/extend))
                        (primop.rep-wants (leaf-value type))
                        t-reg t-rep))
              (else
               (let* ((i-acc (access-with-rep node idex
                                 (if (eq? rep 'rep/pointer)
                                     'rep/pointer
                                     'rep/integer)))
                      (i-reg (cond ((register? i-acc) i-acc)
                                   (else
                                    (let ((i (get-register 'scratch node '*)))
                                      (emit m68/move .l i-acc i)
                                      i)))))
                 (really-rep-convert node (indexer reg tag/extend i-reg)
                                     rep t-reg t-rep))))
          (unlock reg)
          (cond ((reg-node t-reg)
                 => (lambda (node) (set (register-loc node) nil))))
          (mark-continuation node t-reg)))))


(define (generate-make-pointer node)
  (destructure (((cont loc idex) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((t-reg (get-target-register node t-spec))
            (reg (->register 'pointer node (leaf-value loc) '*)))
        (lock reg)
        (let* ((i-acc (access-with-rep node (leaf-value idex) 'rep/pointer))
               (i-reg (cond ((register? i-acc) i-acc)
                            (else
                             (let ((i (get-register 'scratch node '*)))
                               (emit m68/move .l i-acc i)
                               i)))))
          (emit m68/lea (indexer reg 4 i-reg) t-reg))
        (unlock reg)
        (cond ((reg-node t-reg)
               => (lambda (node) (set (register-loc node) nil))))
        (mark-continuation node t-reg)))))


(define (generate-location-access node)
  ((xselect (length (call-args node))
     ((3) defer-fixed-accessor)
     ((4) defer-vector-elt))
   node))

(define (defer-fixed-accessor node)
  (destructure (((cont type loc) (call-args node)))
    (let* ((type (leaf-value type))
           (base (leaf-value loc))
           (reg (->register 'pointer node base '*)))
      (lock reg)
      (set (register-loc (car (lambda-variables cont)))
           (cons reg (primop.location-specs type)))
      (allocate-call (lambda-body cont)))))



(define (defer-vector-elt node)
  (destructure (((cont type loc index) (call-args node)))
    (let* ((base (leaf-value loc))
           (rep (primop.rep-wants (leaf-value type)))
           (index (leaf-value index))
           (reg (->register 'pointer node base '*)))
      (lock reg)
      (cond ((fixnum? index)
             (set (register-loc (car (lambda-variables cont)))
                  (cons reg (fx+ (if (eq? rep 'rep/pointer)
                                     (fx* 4 index)
                                     index)
                                  tag/extend))))
            (else
             (let* ((i-acc (access-with-rep node index
                                 (if (eq? rep 'rep/pointer)
                                     'rep/pointer
                                     'rep/integer)))
                    (i-reg (cond ((register? i-acc) i-acc)
                                 (else
                                  (let ((i (get-register 'scratch node '*)))
                                    (emit m68/move .l i-acc i)
                                    i)))))
               (unlock reg)
               (kill-if-dying index node)
               (lock reg)
               (lock i-reg)
               (set (register-loc (car (lambda-variables cont)))
                    (cons (cons reg i-reg) 2)))))
      (allocate-call (lambda-body cont)))))



(define (generate-%chdr node)
  (destructure (((#f vec val) (call-args node)))
    (let ((reg (->register 'pointer node (leaf-value vec) '*))
          (val (leaf-value val)))
      (lock reg)
      (cond ((fixnum? val)
             (if (fx= val 1)
                 (emit m68/add .l (machine-num 1) (reg-offset reg offset/string-base))
                 (emit m68/add .l (machine-num val)
                       (reg-offset reg offset/string-base)))
             (emit m68/sub .l (machine-num (fixnum-ashl val 8))
                   (reg-offset reg -2)))
            (else
             (let* ((n (access-with-rep node val 'rep/integer))
                    (data-reg (if (and (register? n) (dying? val node))
                             n
                             SCRATCH)))
               (generate-move n data-reg)
               (emit m68/add .l data-reg (reg-offset reg offset/string-base))
               (emit m68/asl .l (machine-num 8) data-reg)
               (emit m68/sub .l data-reg (reg-offset reg -2)))))
      (unlock reg))))


|||#

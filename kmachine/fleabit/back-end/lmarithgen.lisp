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


(defun access-with-rep (node value rep)
  (cond ((variable-p value)
         (let ((acc (access-value node value))
               (converter (rep-converter (variable-rep value) rep)))
           (cond (converter
                  (let* ((rep-type (if (eq rep 'rep/pointer) 'pointer 'scratch))
                         (reg (if (and (register? acc)
                                       (eq (reg-type acc) rep-type)
                                       (dying? value node))
                                  acc
                                (get-register rep-type node '*))))
                    (funcall converter node acc reg)
                    reg))
                 (t acc))))
        ((eq rep 'rep/pointer)
         (access-value node value))
        (t
         (value-with-rep value rep))))




(defun comparator (node inst)   ; type)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let* ((val1 (leaf-value ref2))
           (val2 (leaf-value ref1))
           (rep (cond ((and (variable-p val1)
                            (not (eq (variable-rep val1) 'rep/pointer)))
                       (variable-rep val1))
                      ((variable-p val2) (variable-rep val2))
                      (t 'rep/pointer))))
      (let ((access2 (access-with-rep node val2 rep)))
        (protect-access access2)
        (let ((access1 (access-with-rep node val1 rep)))
          (emit lm/sub garbage access1 access2)
          (emit-jump inst else then))
        (release-access access2)))))


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(define (get-jop inst rep reverse?)
  (xcase inst
    ((jneq) jump-op/jn=)
    ((jgeq)
     (case rep
       ((rep/char rep/integer-8-u rep/integer-16-u)  ;;; unsigned guys
        (if reverse? jump-op/uj<= jump-op/uj>=))
       (else
         (if reverse? jump-op/j<= jump-op/j>=))))))

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defun generate-unop (node inst)
  (destructure (((cont arg) (call-args node)))
    (multiple-value-bind (t-spec t-rep) (continuation-wants cont)
      (let* ((var (leaf-value arg))
             (acc (access-with-rep node var t-rep)))
        (cond ;; why do they prefer to leave the result in the register of the l source
              ;; rather than try for the continuation???
              ;((and (register? acc) (dying? var node))
              ; (emit inst acc acc '(constant (quote 0)))  ;???
              ; (kill var)
              ; (mark-continuation node acc))
              (t
               (protect-access acc)
               (let ((t-reg (cond ((and (register? t-spec)
                                        (not (locked? t-spec))
                                        (not (reg-node t-spec)))
                                   t-spec)
                                  (t
                                   (get-register t-spec node '*)))))
                   (release-access acc)
                   (emit inst t-reg acc '(constant (quote 0)))  ;???
                   (mark-continuation node t-reg))))))))

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
(define (generate-fixnum-binop node inst commutes? strange?)
 (case inst
   ((ashl ashr) (do-ash node inst))
   ((and or xor) (do-logical node inst))
   (else
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep node lvar t-rep)))
        (protect-access l-acc)
        (let ((r-acc (access-with-rep node rvar t-rep)))
          (release-access l-acc)
          (cond ((and (register? l-acc) (dying? lvar node) commutes?)
                 (emit (m68-op inst) (m68-size t-rep) r-acc l-acc)
                 (kill lvar)
                 (mark-continuation node l-acc))
                ((and (register? r-acc) (dying? rvar node))
                 (emit (m68-op inst) (m68-size t-rep) l-acc r-acc)
                 (kill rvar)
                 (mark-continuation node r-acc))
                (else
                 (protect-access l-acc)
                 (let ((t-reg (cond ((and (register? t-spec)
                                          (not (locked? t-spec))
                                          (not (reg-node t-spec)))
                                      t-spec)
                                    (else
                                     (get-register t-spec node '*)))))
                   (release-access l-acc)
                   (generate-move r-acc t-reg)
                   (emit (m68-op inst) (m68-size t-rep) l-acc t-reg)
                   (mark-continuation node t-reg)))))))))))


(define (do-logical node inst)
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep node lvar t-rep)))
        (protect-access l-acc)
        (let ((r-acc (access-with-rep node rvar t-rep)))
          (release-access l-acc)
          (cond ((and (register? l-acc)
                      (eq? (reg-type l-acc) 'scratch)
                      (dying? lvar node))
                 (cond ((or (and (register? r-acc)
                                 (eq? (reg-type r-acc) 'pointer))
                            (eq? inst 'xor))
                        (generate-move r-acc SCRATCH)
                        (emit (m68-op inst) (m68-size t-rep) SCRATCH l-acc))
                       (else
                        (emit (m68-op inst) (m68-size t-rep) r-acc l-acc)))
                 (kill lvar)
                 (mark-continuation node l-acc))
                ((and (register? r-acc)
                      (eq? (reg-type r-acc) 'scratch)
                      (dying? rvar node))
                 (cond ((or (and (register? l-acc)
                                 (eq? (reg-type l-acc) 'pointer))
                            (eq? inst 'xor))
                        (generate-move l-acc SCRATCH)
                        (emit (m68-op inst) (m68-size t-rep) SCRATCH r-acc))
                       (else
                        (emit (m68-op inst) (m68-size t-rep) l-acc r-acc)))
                 (kill rvar)
                 (mark-continuation node r-acc))
                (else
                 (let ((t-reg (if (and (register? t-spec)
                                       (eq? (reg-type t-spec) 'scratch)
                                       (not (reg-node t-spec)))
                                  t-spec
                                  (get-register 'scratch node '*))))
                   (generate-move r-acc t-reg)
                   (cond ((or (and (register? l-acc)
                                   (eq? (reg-type l-acc) 'pointer))
                              (eq? inst 'xor))
                          (generate-move l-acc SCRATCH)
                          (emit (m68-op inst) (m68-size t-rep) SCRATCH t-reg))
                         (else
                          (emit (m68-op inst) (m68-size t-rep) l-acc t-reg)))
                   (mark-continuation node t-reg)))))))))


(define (do-ash node inst)
  (destructure (((cont right left) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((lvar (leaf-value left))
             (rvar (leaf-value right))
             (l-acc (access-with-rep node lvar 'rep/integer)))
       (protect-access l-acc)
       (let ((r-acc (access-with-rep node rvar 'rep/integer)))
         (release-access l-acc)
         (let* ((r-reg (cond ((and (dying? rvar node) (register? r-acc))
                              (kill rvar)
                              r-acc)
                             ((and (register? t-spec)
                                   (eq? (reg-type t-spec) 'scratch)
                                   (not (reg-node t-spec)))
                              t-spec)
                             (else
                              (protect-access l-acc)
                              (protect-access r-acc)
                              (let ((r (get-register 'scratch node '*)))
                                (release-access l-acc)
                                (release-access r-acc)
                                (generate-move r-acc r)
                                r))))
                (l-reg (cond ((and (fixnum? lvar) (fx<= lvar 8) (fx>= lvar 1))
                              (machine-num lvar))
                             ((and (register? l-acc)
                                   (eq? (reg-type l-acc) 'scratch))
                              l-acc)
                             (else
                              (generate-move l-acc SCRATCH)
                              SCRATCH))))
           (emit (xcase inst ((ashl) m68/asl) ((ashr) m68/asr)) .l l-reg r-reg)
           (if (eq? t-rep 'rep/pointer) (emit m68/asl .l (machine-num 2) r-reg))
           (mark-continuation node r-reg)))))))


||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

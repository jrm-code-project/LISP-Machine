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

(defun generate-nil-test (arg)
  (emit lm/sub 'garbage arg '(constant nil)))

(defun generate-open (node)
  (let* ((call (leaf-value (call-arg-n 2 node)))
         (proc (call-proc call))
         (call-cont (call-arg-n 1 call)))
    (unless (or ;;continuation or let
                ;;these should not be generated or flushed during simplify??
              (lambda-node? proc)
              (not (reference-node? proc))              ;primop?? flushed during simplify?
              (variable-known (leaf-value proc)))   ;label procs
      (if (lambda-node? call-cont)
          (let ((wants (continuation-wants call-cont)))
;           (if (and (register? wants)
;                    (variable-p (reg-node wants))
;                    (live? (reg-node wants) call-cont))
;               (emit lm/open wants)
            (emit lm/open (get-register 'pointer call-cont (if (register? wants) wants '*))))
        (emit lm/tail-open))
      (push-o-regs))))



;;; Data manipulation
;;; ---------------------------------------------------------------------


(defun generate-define-var (node)
  (let* ((value (call-arg-n 3 node)))
    (cond ((and (lambda-node? value)
             (not (eq (primop.support-variant (leaf-value (call-proc node)))
                       'lset))
             (eq (environment-closure (lambda-env value)) *unit*))
           (lambda-queue value))
          ((primop-node? value))
          (t
           (generate-set node (call-arg-n 2 node) value)))))


;;; this is from generate-set-fixed-accessor (lmlocgen)
;;; need to call mark-continuation more
(defun generate-setq (node loc value &aux prim)   ;I hope those clauses wont get used??
  (cond ((lambda-node? value)
         (let ((access (access/make-closure node value)))
           (if access (protect-access access) (lock AN))
           (cond (;(and (eq prim primop/cell-value)
                  (member (variable-support (leaf-value loc)) '(one nil))       ;eq 'one
                  (kill (leaf-value loc))       ; force into closure
                  (generate-move (access-value node (leaf-value loc)) AN))
                 (t
                  (let ((reg (->register 'pointer node (leaf-value loc) '*)))
                    (generate-move (if access access AN)
                                   (reg-offset reg (primop.location-specs prim))))))
           (if access (release-access access) (unlock AN))))
        (t
         (let ((access (access-with-rep node (leaf-value value) 'rep/pointer)))
           (protect-access access)
           (cond (                              ;(and (eq prim primop/cell-value)
                  (member (variable-support (leaf-value loc)) '(one nil))       ;eq 'one
                  (let ((lc (access-value node (leaf-value loc))))
                    (generate-move access lc)
                    (let ((lc (and (register? lc) (temp-loc (leaf-value loc)))))
                      (cond (lc
                             (setf (temp-node lc) nil)
                             (setf (temp-loc (leaf-value loc)) nil))))
                    (mark-continuation node lc)))  ;*****
                 (t
                  (let ((reg (->register 'pointer node (leaf-value loc) '*)))
                    (generate-move (reg-offset reg (primop.location-specs prim))
                                   access)))
                 )
           (release-access access)))))


(defun generate-set (node location value)
  (cond ((lambda-node? value)
         (let ((access (access/make-closure node value)))
           (cond (access
                  (free-register node AN)
                  (generate-move access AN)))))
        (t
         (let ((access (access-value node (leaf-value value))))
           (free-register node AN)
           (generate-move access AN))))
  (lock AN)
  (let ((loc (lookup node (get-lvalue (leaf-value location)) nil)))
    (free-register node A1)
    (free-register node A2)
    (free-register node S0)
    (generate-move loc A1))
  (unlock AN)
  (slink-is A2)
  (emit m68/jsr (reg-offset A2 slink/set)))


#|||||||||

;;; Eq?
;;; ---------------------------------------------------------------------


(define (eq?-comparator node)
  (destructure (((then else () ref1 ref2) (call-args node)))
    (let ((val1 (leaf-value ref1))
          (val2 (leaf-value ref2)))
      (let ((access2 (access-with-rep node val2 'rep/pointer)))
        (protect-access access2)
        (let ((access1 (access-with-rep node val1 'rep/pointer)))
          (cond ((register? access1)
                 (emit m68/cmp .l access2 access1))
                ((register? access2)
                 (emit m68/cmp .l access1 access2))
                (else
                 (let ((reg (get-register 'pointer node '*)))
                   (emit m68/move .l access1 reg)
                   (emit m68/cmp .l access2 reg))))
          (emit-jump 'jneq else then)
          (release-access access2))))))


(define (one-arg-primitive node)
  (destructure (((cont arg) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((var (leaf-value arg))
             (dest (cond ((register? t-spec)
                          (cond ((or (not (reg-node t-spec))
                                     (dying? (reg-node t-spec) node))
                                  t-spec)
                                (else
                                 (get-register (reg-type t-spec) node '*))))
                         ((and (dying? var node) (register-loc var))
                          => (lambda (reg)
                               (if (and (register? reg) (eq? (reg-type reg) t-spec))
                                   reg
                                   (get-register t-spec node '*))))
                         (else
                          (get-register t-spec node '*)))))
        (lock dest)
        (let ((acc (access-value node var)))
          (unlock dest)
          (kill-if-dying var node)
          (values acc dest t-rep))))))


(define (generate-closure-enclosing-object node)
     (receive (source target rep) (one-arg-primitive node)
       (let ((creg (cond ((and (register? source) (neq? source target))
                          source)
                         (else
                          (lock target)
                          (block0 (get-register 'pointer node '*)
                                  (unlock target))))))
         (generate-move source creg)
         (emit m68/move .l (reg-offset creg -2) target)   ; get template
         (emit m68/move .l (machine-num 0) SCRATCH)
         (emit m68/move .w (reg-offset target -6) SCRATCH) ; offset field in bytes
         (generate-move creg target)
         (emit m68/sub .l SCRATCH target))    ; pointer and scratch adjoined
         (mark-continuation node target)))



(define (generate-throw node)
  (destructure (((#f frame val) (call-args node)))
    (->register 'pointer node (leaf-value val) A1)
    (lock A1)
    (->register 'pointer node (leaf-value frame) A2)
    (emit m68/sub .l (machine-num 2) A2)
    (emit m68/move .l A2 SP)
    (unlock A1)
    (clear-slots)
    (generate-return 1)))




(define (generate-make-vector-extend node)
  (destructure (((#f type length size) (call-args node)))
    (->register 'scratch node (leaf-value size) S1)
    (let ((acc (access-value node (leaf-value length))))
      (if  (not (dying? (leaf-value length) node))
           (free-register node AN)
           (kill (leaf-value length)))
      (emit m68/move .l acc SCRATCH)
      (emit m68/asl .l (machine-num 6) SCRATCH)
      (emit m68/move .b (machine-num (leaf-value type)) SCRATCH)
      (emit m68/move .l SCRATCH AN)
      (if (not (dying? (leaf-value size) node))
          (free-register node S1)
          (kill (leaf-value size))))
    (free-register node S2)
    (lock AN)
    (let ((reg (get-register 'pointer node '*)))
      (lock AN)
      (slink-is reg)
      (emit m68/jsr (reg-offset reg slink/make-extend)))
    (mark-continuation node AN)))


(define (generate-make-extend node)
  (destructure (((#f template size) (call-args node)))
    (->register 'scratch node (leaf-value size) S1)
    (let ((acc (access-value node (leaf-value template))))
      (if  (not (dying? (leaf-value template) node))
           (free-register node AN)
           (kill (leaf-value template)))
      (generate-move acc AN)
      (if (not (dying? (leaf-value size) node))
          (free-register node S1)
          (kill (leaf-value size))))
    (free-register node S2)
    (lock AN)
    (let ((reg (get-register 'pointer node '*)))
      (unlock AN)
      (slink-is reg)
      (emit m68/jsr (reg-offset reg slink/make-extend)))
    (mark-continuation node AN)))


(defun generate-make-cell (node)
  (destructure (((cont value) (call-args node)))
    (cond ((and (lambda-node? cont)
                (member (variable-support (car (lambda-variables cont))) '(one nil)))  ;eq 'one
           (multiple-value-bind (t-spec t-rep) (continuation-wants cont)
             (let ((dest (get-target-register node t-spec)))
               (lock dest)
               (generate-move (access-value node (leaf-value value)) dest)
               (unlock dest)
               (mark-continuation node dest))))
          (t
           (free-register node AN)
           (free-register node S1)
           (emit m68/move .l (lit 1) S1)               ; 1 slot
           (emit m68/move .l (machine-num header/cell) AN)
           (free-register node S2)
           (lock AN)
           (let ((reg (get-register 'pointer node '*)))
             (slink-is reg)
             (emit m68/jsr (reg-offset reg slink/make-extend)))
           (generate-move (access-value node (leaf-value value))
                          (reg-offset AN tag/extend))
           (unlock AN)
           (mark-continuation node AN)))))


(define (generate-make-pair node)
  (free-register node AN)
  (slink-is AN)
  (emit m68/jsr (reg-offset AN slink/make-pair))
  (mark-continuation node AN))


(define (generate-primitive-reg-ref node)
  (destructure (((cont reg arg) (call-args node)))
   (if (and (fixnum? (leaf-value reg)) (fixnum? (leaf-value arg)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((reg (leaf-value reg))
             (dest (get-target-register node t-spec)))
        (if (fx>= reg 0)
            (cond ((fx>= reg 8)
                   (emit m68/move .l (d@r reg (leaf-value arg)) dest))
                  (else
                   (let ((temp (if (eq? (reg-type dest) 'pointer)
                                   dest
                                   (get-register 'pointer node '*))))
                     (emit m68/move .l (r reg) temp)
                     (emit m68/move .l (reg-offset temp (leaf-value arg)) dest))))
            (emit m68/move .l (leaf-value arg) dest))
        (mark-continuation node dest))))))


(define (generate-set-primitive-reg-ref node)
  (destructure (((#f reg arg val) (call-args node)))
   (if (and (fixnum? (leaf-value reg)) (fixnum? (leaf-value arg)))
    (let ((reg (leaf-value reg)))
      (cond ((fx>= reg 8)
             (generate-move (access-value node (leaf-value val))
                            (d@r reg (leaf-value arg))))
            (else
             (let ((acc (access-value node (leaf-value val))))
               (protect-access acc)
               (let ((preg (get-register 'pointer node '*)))
                 (release-access acc)
                 (emit m68/move .l (r reg) preg)
                 (emit m68/move .l acc (reg-offset preg (leaf-value arg)))))))))))


(define (generate-stack-pointer node)
  (receive (t-spec t-rep) (continuation-wants ((call-arg 1) node))
    (let ((dest (get-target-register node t-spec)))
      (free-register node dest)
      (emit m68/move .l SP dest)
      (mark-continuation node dest))))


(define (generate-current-continuation node)
  (receive (t-spec t-rep) (continuation-wants ((call-arg 1) node))
    (let ((dest (get-target-register node t-spec)))
      (free-register node dest)
      (emit m68/move .l SP dest)
      (emit m68/add .l (machine-num 2) dest)
      (mark-continuation node dest))))


(define (generate-nary-setup node required)
  (if (eq? (lambda-strategy node) strategy/stack)
      (emit m68/neg .l NARGS))                           ; !!!
  (do ((i (fx+ A1 required) (fx+ i 1)))
      ((fx>= i (fx- *real-registers* 1)))
    (generate-move i (fx+ *real-registers* (fx- i A1))))
  (generate-move (machine-num required) S0)
  (slink-is AN)
  (emit m68/jsr (reg-offset AN slink/nary-setup))
  (mark (lambda-rest-var node) AN))


;;; GENERATE-HANDLER The situation is that the object is in A1 and its template
;;; is in TP.  The  operation is in P.  We must use only the register AN.

(define (generate-handler node)
  (let ((leaves (call-args (lambda-body ((call-arg 3) (lambda-body node)))))
        (methods (cdddr (call-args (lambda-body node)))))
    (cond ((null? methods)
           (emit m68/move .l nil-reg AN)
           (emit m68/rts))
          (else
      (bind ((get-register (no-op (lambda (type node where) AN))))
        (mark (lambda-self-var *lambda*) A1)
        (generate-jump (car leaves))
        (let ((last ((call-arg 3) (lambda-body node))))
          (do ((l leaves (cdr l))
               (methods methods (cdr methods)))
              ((null? l)
               (emit-tag last)
               (emit m68/move .l nil-reg AN)
               (emit m68/rts)
               (clear-slots))
            (generate-handler-test (car l)
                                   (car methods)
                                   (if (null? (cdr l)) last (cadr l))))))))))

(define (generate-handler-test leaf method next)
  (emit-tag leaf)
  (emit m68/cmp .l (access-value nil (leaf-value leaf)) P)
  (let ((el-hacko (cons nil nil)))
    (emit-jump 'jneq next el-hacko)
    (emit-tag el-hacko))
  (lambda-queue method)
  (generate-move-address (template method) AN)
  (emit m68/rts))


(define (generate-undefined-effect node)
  (generate-move (access-value node (leaf-value ((call-arg 1) node))) A1)
  (slink-is P)
  (emit m68/jmp (reg-offset P slink/undefined-effect))
  (clear-slots))



||||||||#

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

;;; Copyright (c) 1985 David Kranz

;;; RESULT-WANT-LOC Determines where to target the result of a primop.
;;; If the continuation is a variable, it is a return of one argument in
;;; A1 (no, in RETURN)  Otherwise we look at the most important use (now done non-optimally)
;;; and see where is is needed.  If it is the argument to a primop we look
;;; at the arg-specs of that primop, otherwise it is in a position for the
;;; standard calling sequence.

#|
(define-operation (foreign-name foreign) nil)
|#


(defun continuation-wants (cont)
  (cond ((lambda-node? cont)
         (cond ((n-ary? cont) (values '* 'rep/pointer))
               (t
                (let ((var (car (lambda-variables cont))))
                  (values (likely-next-reg var cont)
                          (variable-rep var))))))
        (t
         (values RETURN 'rep/pointer))))  ;A1

(defun likely-next-reg (var cont)
  (let ((spec (really-likely-next-reg var cont)))
    (if (integerp spec)
        (if (eq (reg-type spec) 'pointer)
            (if (eq (variable-rep var) 'rep/pointer) spec 'scratch)
          (if (not (eq (variable-rep var) 'rep/pointer)) spec 'pointer))
      spec)))


(defun really-likely-next-reg (var cont)
  (let ((refs (member (lambda-trace cont)
                      (variable-refs var)
                      :test #'(lambda (x ref)
                                (= (lambda-trace (node-parent (node-parent ref))) x)))))
    (dolist (ref refs (if (eq (variable-rep var) 'rep/pointer) 'pointer 'scratch))
      (let* ((parent (node-parent ref))
             (proc (call-proc parent))
             (number (call-arg-number (node-role ref))))
        (if (primop-node? proc)
            (let* ((primop (primop-value proc))
                   (specs (primop.arg-specs primop)))
              (if specs
                  (return (nth (1- (- number
                                      (call-exits parent)))
                               specs))
                ;; ****
                ;; this won't work because the caller of this will have a hard
                ;; time figuring out that the register can be munged...
                ;; (VAR can be live in CONT even though its value will not be
                ;; because of the setq)
                ;; (if (eq primop primop/setq)
                ;;     (return (register-loc (leaf-value (call-arg-n 2 parent)))))
                ;; ****
                  ))
          (let ((label (variable-known (leaf-value proc))))
            (if label
                (if (not (eq (lambda-strategy label) strategy/label))
                    (return (- (+ number *scratch-registers*)
                               (call-exits parent)))
                    (let ((args (join-point-arg-specs (lambda-env label))))
                      (if args
                          (return (car (nth (1- (- number (call-exits parent)))
                                            args)))
                        (return (if (eq (variable-rep var) 'rep/pointer)
                                    'pointer
                                  'scratch)))))
              ;;calling unknown proc, return open reg corresponding
              ;;to ref of var
              (return (- (+ (1- number) O0)    ; *scratch-registers*)
                         (call-exits parent))))))))))


#|||||||

;;; GENERATE-LET&LABELS Divide up the procedures depending on whether they
;;; need to be closed or can be jumped to.

(define (generate-let node)
  (destructure (((body . exprs) (call-proc+args node)))
    (iterate loop ((exprs exprs) (vars '()) (lambdas '()))
      (cond ((null? exprs)
             (really-generate-let node body vars lambdas))
            ((not (lambda-node? (car exprs)))
             (loop (cdr exprs) (cons (car exprs) vars) lambdas))
            ((eq? (lambda-strategy (car exprs)) strategy/label)
             (loop (cdr exprs) vars (cons (car exprs) lambdas)))
            (else
             (loop (cdr exprs) vars (cons (car exprs) lambdas)))))
    (allocate-call (lambda-body body))))

|||#

;;; this is the primop.generate function for $Y
;;; it generates the body and creates closures (?)
;;; but does not generate the label functions
;;; they are queued by set-join-state at the
;;; first call to them
(defun generate-labels (node)
  (destructure (((body . procs) (call-args (lambda-body (call-arg-n 1 node)))))
    (ecase (lambda-strategy (call-arg-n 1 node))
      ((STRATEGY/HEAP)
       (generate-heap-labels node body procs))
      ((STRATEGY/VFRAME STRATEGY/EZCLOSE)
       (generate-vframe-labels node (call-arg-n 1 node)))
      ((STRATEGY/LABEL)))
    (allocate-call (lambda-body body))))


#|||
(define (generate-heap-labels node body closures)
    (if closures
        (let ((closure (environment-closure (lambda-env (car closures)))))
          (make-heap-closure node closure)
          (lock AN)
          (walk (lambda (var)
                  (let ((reg (get-register 'pointer node '*))
                        (offset (cdr (assq var (closure-env closure)))))
                    (generate-move-address (reg-offset AN offset) reg)
                    (mark var reg)))
                (filter (lambda (closure)
                          (memq? closure (lambda-live body)))
                        (cdr (closure-members closure))))
          (unlock AN)
          (if (memq? (car (closure-members closure)) (lambda-live body))
              (mark (car (closure-members closure)) AN)))))

|||#

(defun generate-vframe-labels (node vframe)
  (cond ((null (lambda-env vframe))
         (mapc #'lambda-queue (cdr (call-args (lambda-body vframe)))))
        ((eq (lambda-strategy vframe) strategy/vframe)
         (make-vframe-closure node
                              vframe
                              (environment-closure (lambda-env vframe))
                               nil)
         (free-register node P)
         (generate-move-address (reg-offset SP 2) P)
         (mark (lambda-self-var vframe) P))
        (t
         (make-vframe-closure node
                              vframe
                              (environment-closure (lambda-env vframe))
                              t))))

#|||
;; ************** the variables of leaves MUST BE FREE. They should have been
;; substituted.  Otherwise there would be aliasing.

(define (really-generate-let node body leaves closures)
  (let ((bind-leaf
          (lambda (leaf)
            (if (variable-binder (leaf-value leaf))
                (bug "lexical variable being bound by LET ~s" (leaf-value leaf)))
            (let* ((var (nth (lambda-variables body)
                                 (fx- (call-arg-number (node-role leaf)) 1)))
                   (acc (access-value node (leaf-value leaf)))
                   (reg (get-register (if (eq? (variable-rep var) 'rep/pointer)
                                          'pointer 'scratch)
                                      node '*)))
              (really-rep-convert node acc 'rep/pointer reg (variable-rep var))
              (set (register-loc (leaf-value leaf)) nil)
              (mark var reg)))))
    (cond ((not closures)
           (walk bind-leaf leaves))
          ((eq? (lambda-strategy (car closures)) strategy/label)
           (walk bind-leaf leaves))
          ((eq? (lambda-strategy (car closures)) strategy/stack)
           (cond ((cdr closures) (bug "too many stack closures in let" node))
                 (else
                  (set (lambda-strategy body) strategy/stack)
                  (set (lambda-env body) (lambda-env (car closures)))
                  (walk bind-leaf leaves))))
          (else
           (cond ((get-member closures)
                  => (lambda (member)
           (let ((closure (environment-closure (lambda-env member))))
             (make-heap-closure node closure)
             (lock AN)
             (walk (lambda (var)
                     (let ((reg (get-register 'pointer node '*))
                           (offset (cdr (assq var (closure-env closure)))))
                       (generate-move-address (reg-offset AN offset) reg)
                       (mark var reg)))
                    (filter (lambda (closure)
                              (memq? closure (lambda-live body)))
                            (cdr (closure-members closure))))
             (walk bind-leaf leaves)
             (unlock AN)
             (mark (car (closure-members closure)) AN)))))
           (walk (lambda (closure)
                   (lambda-queue closure)
                   (mark (lambda-self-var closure)
                         (->register 'pointer node closure '*)))
                 (filter (lambda (l)
                           (eq? (environment-closure (lambda-env l))
                                *unit*))
                         closures))))))

(define (get-member closures)
  (iterate loop ((closures closures))
    (cond ((null? closures) nil)
          ((neq? (environment-closure (lambda-env (car closures)))
                 *unit*)
           (car closures))
          (else (loop (cdr closures))))))


|||#

(defun get-or-set-join-state (node lamb)
  (let ((join (lambda-env lamb)))
    (if (eq (join-point-global-registers join) 'not-yet-determined)
        (set-join-state node join lamb))
    join))

;;; SET-JOIN-STATE The first jump (compile time) is about to be made to this
;;; point.  We must set up places for the free variables to go.  For now,
;;; put one in a register and the rest in temporaries. Move them there.

(defun set-join-state (node join lamb)
  (lambda-queue lamb)
  (compute-label-arg-specs node lamb)
  (let ((args (mapcar #'car (join-point-arg-specs join)))
        (global '())
        (left '()))
    (dolist (var (join-point-env join))
      (let ((w (likely-next-reg var lamb)))
        (cond ((and (integerp w)
                    (/= w P)
                    (not (member w args)))
               (push w args)
               (push (cons w var) global))
              (t
               (let ((reg (register-loc var)))
                 (cond ((and reg
                             (var-reg-compatable? var reg)
                             (/= reg P)
                             (not (member reg args)))
                        (push reg args)
                        (push (cons reg var) global))
                       (t
                        (push var left))))))))

    (dolist (var left)
      (let ((reg (get-free-register (variable-rep var) args)))
        (push reg args)
        (push (cons reg var) global)))

;    (do ((a (join-point-arg-specs join) (cdr a)))
;        ((null? a))
;      (cond ((not (fixnum? (caar a)))
;             (let ((reg (get-free-register (cdar a) args)))
;               (push args reg)
;               (set (caar a) reg)))))
    (if (join-point-contour-needed? join)
        (push (cons P (join-point-contour join)) global))
    (setf (join-point-global-registers join) global)))

(defun var-reg-compatable? (var reg)
  (if (eq (variable-rep var) 'rep/pointer)
      (eq (reg-type reg) 'pointer)
      (eq (reg-type reg) 'scratch)))


(defun compute-label-arg-specs (node label)
  (multiple-value-bind (formals actuals)
      (if (continuation? label)
          (values (lambda-variables label)
                  (call-args node))
        (values (cdr (lambda-variables label))
                (cdr (call-args node))))
    (do ((formals formals (cdr formals))
         (actuals actuals (cdr actuals))
         (args '())
         (regs '()))
        ((null formals)
         (setf (join-point-arg-specs (lambda-env label)) (nreverse args)))
      (let* ((w (likely-next-reg (car formals) label))
             (reg (cond ((and (integerp w) (not (member w regs))) w)
                        ((let ((reg (and (reference-node? (car actuals))
                                         (register-loc (leaf-value (car actuals))))))
                           (if (and reg
                                    (/= reg P)
                                    (not (member reg regs))
                                    (var-reg-compatable? (car formals) reg))
                               reg
                             (get-free-register (variable-rep (car formals)) regs)))))))
        (push (cons reg (variable-rep (car formals))) args)
        (push reg regs)))))


(defun get-free-register (rep used)
  (do ((i A0 (1+ i)))
      ((= i AN)
       (do ((j *real-registers* (1+ j)))
           ((if (>= j (+ *real-registers* *pointer-temps*))
                (bug "ran out of registers in GET-FREE-REGISTER")
              (not (member j used)))
            j)))
    (unless (member i used)
      (return i))))

;(define (get-free-register rep used)
;  (cond ((eq? rep 'rep/pointer)
;         (iterate loop ((i A1))
;           (cond ((fx= i AN)
;                  (do ((j *real-registers* (fx+ j 1)))
;                      ((if (fx>= j (fx+ *real-registers* *pointer-temps*))
;                           (bug "ran out of registers in GET-FREE-REGISTER")
;                           (not (memq? j used)))
;                       j)))
;                 ((memq? i used) (loop (fx+ i 1)))
;                 (else i))))
;        (else
;         (iterate loop ((i 0))
;           (cond ((fx= i *scratch-registers*)
;                  (do ((j (fx+ *real-registers* *pointer-temps*) (fx+ j 1)))
;                      ((if (fx>= j *no-of-registers*)
;                           (bug "ran out of registers in GET-FREE-REGISTER")
;                           (not (memq? j used)))
;                       j)))
;                 ((memq? i used) (loop (fx+ i 1)))
;                 (else i))))))

#||||
;;; locatives


(define (generate-locative node)
  (receive (t-spec t-rep) (continuation-wants ((call-arg 1) node))
    (let* ((dest (get-target-register node t-spec))
           (acc (lookup node (get-lvalue (leaf-value ((call-arg 2) node))) nil)))
      (free-register node dest)
      (generate-move acc dest)
      (mark-continuation node dest))))





(define (get-lvalue var)
  (cond ((ass (lambda (x y)
                (and (loc-list? y)
                     (eq? x (loc-list-var y))))
              var
              (closure-env *unit*))
         => car)
        (else
         (bug "couldn't find lvalue ~s" var))))

||||#

;;; this looks sorta like continuation-wants    -efh
(defun mark-continuation (node reg)
  (let ((cont (car (call-args node))))
    (if (lambda-node? cont)
        (if (not (n-ary? cont))
            (mark (car (lambda-variables cont)) reg))
        (generate-move reg RETURN))))           ;A1

#||||
(define (get-template-definer l)
  (iterate loop ((l l))
   (let ((node (node-parent l)))
    (cond ((not node) 0)
          ((and (eq? (lambda-strategy l) strategy/heap)
                (continuation? l))
           0)
          ((or (primop-ref? (call-proc node) primop/*define)
               (primop-ref? (call-proc node) primop/*define-constant))
           (let ((offset (cdr (ass (lambda (x y)
                                 (and (loc-list? y)
                                      (eq? x (loc-list-var y))))
                              (leaf-value ((call-arg 2) node))
                              (closure-env *unit*)))))
             (fx/ offset 4)))
          (else
           (loop (node-parent node)))))))

(define (template-has-superior? l)
  (select (lambda-strategy l)
    ((strategy/ezclose strategy/vframe) 1)
    ((strategy/stack)
     (if (continuation? (node-parent (node-parent l))) 1 0))
    (else 0)))

(define-structure-type lap-template-struct
  pointer
  scratch
  nargs
  handler-tag
  strategy
  instructions)


(define (generate-lap-template node)
  (destructure (((#f i-node) (call-args node)))
    (let ((tem (make-lap-template-struct))
          (i-stream (leaf-value i-node)))
      (destructure (((pointer scratch nargs nary? strategy tag) (car i-stream)))
        (set (lap-template-struct-pointer tem) pointer)
        (set (lap-template-struct-scratch tem) scratch)
        (set (lap-template-struct-nargs tem) (cons nargs nary?))
        (set (lap-template-struct-strategy tem)
             (if (eq? strategy 'stack) 0 1))
        (set (lap-template-struct-handler-tag tem) tag)
        (set (lap-template-struct-instructions tem) (cdr i-stream))
        (lambda-queue tem)
        (free-register node AN)    ; where set (define) code expects
        (generate-move-address (template tem) AN)
        (mark-continuation node AN)))))

(define (process-lap-template tem)
  (emit-template tem (lap-template-struct-handler-tag tem))
  (set *lambda* (car (find (lambda (pair) (lambda-node? (car pair)))
                           (closure-env *unit*))))
  (lap-transduce (lap-template-struct-instructions tem))
  (process-lambda-queue))

(define (create-comex filename h unit templates thing code)
  (let ((size (fx+ (fx+ (length unit) 4) (fx* (length templates) 2))) ; hack,
        (comex (make-comex)))                                         ; template
    (receive (objects opcodes)                                        ; in both
             (create-obj-op-vectors thing unit templates size filename h)
      (set (comex-module-name comex) (filename-name filename))
      (set (comex-code comex) code)
      (set (comex-objects comex) objects)
      (set (comex-opcodes comex) opcodes)
      (set (comex-annotation comex) nil)
      comex)))

(define (create-obj-op-vectors thing unit templates size filename h)
  (let ((objects (make-vector size))
        (opcodes (make-bytev size)))
    (set (bref opcodes 0) op/literal)
    (vset objects 0 (->compiler-filename filename))
    (set (bref opcodes 1) op/literal)
    (vset objects 1 h)
    (set (bref opcodes 2) op/literal)
    (vset objects 2 'unit-env)
    (set (bref opcodes 3) op/closure)
    (vset objects 3 (code-vector-offset thing))
    (iterate loop ((a-list unit) (i 4))
      (cond ((null? a-list)
             (values objects opcodes))
            ((closure? (caar a-list))
             (vset objects i
                   (code-vector-offset (cit->lambda (caar a-list))))
             (set (bref opcodes i) op/template1)
             (set (bref opcodes (fx+ i 1)) op/template2)
             (set (bref opcodes (fx+ i 2)) op/template3)
             (loop (cdr a-list) (fx+ i 3)))
            (else
             (receive (opcode obj) (comex-decipher unit (caar a-list) filename)
               (vset objects i obj)
               (set (bref opcodes i) opcode)
               (loop (cdr a-list) (fx+ i 1))))))))


(define (->compiler-filename fn)
  (list (cond ((filename-fs fn))
              (else (fs-name (local-fs))))
        (filename-dir fn)
        (filename-name fn)
        (cond ((filename-type fn))
              (else 't))))




(define (comex-decipher unit obj name)
  (cond ((foreign-name obj)
         => (lambda (name) (values op/foreign name)))
        ((and (node? obj) (lambda-node? obj))
         (values op/closure (code-vector-offset obj)))
        ((loc-list? obj)
         (values op/location-list (variable-name (loc-list-var obj))))
        ((not (variable? obj))
         (values op/literal obj))
        ((not (defined-variable? obj))
         (values op/free (variable-name obj)))
        (else
         (xcase (defined-variable-variant obj)
           ((lset)
            (values op/lset (variable-name obj)))
           ((set)
            (values op/free (variable-name obj)))
           ((define constant)
            (decipher-definition obj))))))

(define (decipher-definition var)
  (let ((l (defined-variable-value var))
        (name (variable-name var)))
    (cond ((and l
                (let ((node ((call-arg 3) (node-parent l))))
                  (and (lambda-node? node)
                       (assq node (closure-env *unit*)))))
           => (lambda (pair)
                (values op/stored-definition (cons name (cdr pair)))))
          (else
           (values op/defined name)))))

(define (cit->lambda closure)
  (variable-binder (car (closure-members closure))))

(define (static var-name)
  (let* ((a-list (closure-env *unit*))
         (val (ass (lambda (name var)
                     (and (variable? var) (eq? name (variable-name var))))
                   var-name
                   a-list)))
    (cond (val
           (fx- (cdr val)
                (fx+ (cond ((assq *lambda* a-list)
                            => cdr)
                           (else
                            (cdr (last a-list))))
                      tag/extend)))
          (else
           (error "static value not mentioned ~s" var-name)))))


(define (template-nary l)
  (xcond ((lambda-node? l)
          (cond ((object-lambda? l)
                 (lambda-rest-var ((call-arg 2) (lambda-body l))))
                (else
                 (or (eq? (lambda-strategy l) strategy/vframe)
                     (eq? (lambda-strategy l) strategy/ezclose)
                     (lambda-rest-var l)))))
         ((lap-template-struct? l)
          (cdr (lap-template-struct-nargs l)))))


(defun get-template-annotation (l)
  (cond ((lambda-node? l)
          (+ (ash (get-template-definer l) 3)
               (+ (ash (template-has-superior? l) 2)
                    (+ (ash (if (eq (lambda-strategy l) 'strategy/stack) 0 1) 1)
                         (if (/= (environment-cic-offset (lambda-env l)) 0) 1 0)))))
         ((lap-template-struct? l)
          (fixnum-ashl (lap-template-struct-strategy l) 1))
         (t (bug "bad arg: ~s" l))))


(define (get-template-cells l)
  (xcond ((lambda-node? l)
          (let ((offset (environment-cic-offset (lambda-env l))))
            (cond ((fxn= offset 0) offset)
                  (else
                   (let ((closure (environment-closure (lambda-env l))))
                     (fx+ (fixnum-ashl (closure-pointer closure) 8)
                          (closure-scratch closure)))))))
         ((lap-template-struct? l)
          (fx+ (fixnum-ashl (lap-template-struct-pointer l) 8)
               (lap-template-struct-scratch l)))))


(define (get-template-nargs l)
  (xcond ((lambda-node? l)
          (select (lambda-strategy l)
            ((strategy/stack)
             (fx- 0 (fx+ (length (lambda-variables l)) 1)))
            ((strategy/vframe strategy/ezclose) -1)
            (else
             (cond ((object-lambda? l)
                    (let ((proc ((call-arg 2) (lambda-body l))))
                      (if (primop-ref? (call-proc (lambda-body proc))
                                       primop/undefined-effect)
                          0
                          (length (lambda-variables proc)))))
                   (else
                    (length (lambda-variables l)))))))
         ((lap-template-struct? l)
          (car (lap-template-struct-nargs l)))))

||||||#

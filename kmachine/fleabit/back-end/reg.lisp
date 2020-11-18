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

(defvar *assembly-comments?* nil)
(defvar *lambda-queue* '())         ;; queue of lambda bodies to process
(defvar *stack-pos* 0)              ;; distance of stack-pointer from "frame"
(defvar *max-temp* 0)               ;; maximum number of temporaries used
(defvar *lambda* nil)               ;; the procedure being compiled
(defvar *call-break?* nil)
;(defvar *registers* nil)

(defmacro generate-init (&body body)
  `(let ((*unit-literals* '())
         (*unit-variables* '())
         (*unit-closures* '())
         (*unit-templates* '())
         (*unit* nil)
         (*registers* (vector-fill (make-vector *no-of-registers*) nil))
         (*lambda* nil))
     (rr)
     ,@body))


(defun generate (top-node)
  (generate-code-top (car (call-args (lambda-body top-node)))))


(defun rr ()
  (setq *lambda-queue* '())
  (setq *locations* (make-table 'locations))
  (fill *registers* nil)
  *repl-wont-print*)


(defmacro ass-comment (string &rest rest)
  `(if *assembly-comments?*
       (emit-comment (format nil ,string ,@rest))))

(defstruct vframe
  lambda
  return-size)


;;; GENERATE-CODE Initialize lambda queue. Go.

(defun generate-code-top (top)
  (setq *lambda-queue* '())
  (generate-code top))

(defun generate-code (node)
  (format t "~&Generating: ~&") (pp-cps node)
  (if *debug-flag* (format t "~%~d ~s~%" (object-hash node) (lambda-name node)))
  (setq *stack-pos* 0)
  (allocate-registers node)
  (process-lambda-queue))

(defun generate-vframe (vframe)
  (let ((l-node (vframe-lambda vframe))
        (offset (vframe-return-size vframe)))
    (emit-template l-node l-node)
    (adjust-stack-pointer offset)
    (generate-return-without-nargs))
  (process-lambda-queue))

#|||||||
(defun generate-code-for-object (node)
  (if *debug-flag* (format t "~%~d ~s~%" (object-hash node) (lambda-name node)))
  (setq *stack-pos* 0)
  (setq *lambda* node)
  (let ((object-proc (call-arg-n 2 (lambda-body node))))
    (emit-template node object-proc)
    (if (closure-env (environment-closure (lambda-env node)))
        (mark (lambda-self-var node) P))
    (mark-vars-in-regs (cdr (lambda-variables object-proc)))
    (if (n-ary? object-proc)
        (n-ary-setup object-proc))
    (allocate-call (lambda-body object-proc))
    (emit-tag object-proc))
  (generate-handler node)
  (process-lambda-queue))
|||||#



(defun lambda-queue (node)
  (push node *lambda-queue*))

(defun process-lambda-queue ()
  (if *lambda-queue*
      (let ((thing (pop *lambda-queue*)))
        (cond ((object-lambda? thing)
                (generate-code-for-object thing))
              ((lambda-node? thing)
                (generate-code thing))
              ((vframe-p thing)
                (generate-vframe thing))
              ((lap-template-struct? thing)
                (process-lap-template thing))
              (t (bug "Something odd in lambda queue: ~s" thing))))))

;;; ALLOCATE-REGISTERS Sets *lambda* to be the lambda-node representing the
;;; environment the node argument is compiled in.  Generate code for the body.

(defun allocate-registers (node)
    (case (lambda-strategy node)
      ((STRATEGY/STACK STRATEGY/HEAP)
       (setq *lambda* node)
       (emit-template node node))
      ((STRATEGY/VFRAME STRATEGY/EZCLOSE)
       (setq *lambda* (node-parent (node-parent node)))
       (emit-tag node))
      (t
       (setq *lambda* (variable-binder (join-point-contour (lambda-env node))))
       (emit-tag node)))
    (initialize-registers node)
    (if (n-ary? node)
        (n-ary-setup node))
    (allocate-call (lambda-body node)))

;;; INITIALIZE-REGISTERS Here we mark the arguments of a closure as being in
;;; the argument registers.  For a heaped lambda there is also the environment
;;; in the P register.  For a join point the state is initialized.

(zl:defsubst method-lambda (node)
  (let ((p (node-parent node)))
    (if (primop-ref? (call-proc p) primop/proc+handler)
        (node-parent p)
        nil)))


(defun initialize-registers (node)
  (ecase (lambda-strategy node)
    (STRATEGY/HEAP
     (ass-comment "Procedure ~s (lambda ~s ...)"
             (lambda-name node)
             (nconc (mapcar #'variable-unique-name (lambda-variables node))
                    (let ((v (lambda-rest-var node)))
                      (if v
                          (variable-unique-name v)
                          '()))))
     (if (closure-env (environment-closure (lambda-env node)))
         (let ((obj (method-lambda node)))
           (cond (obj
                  (mark (lambda-self-var obj) P)
                  (setq *lambda* obj))
                 (t (mark (lambda-self-var node) P)))))
     (mark-vars-in-regs (cdr (lambda-variables node))))
    (STRATEGY/STACK
     (ass-comment "Continuation ~s (lambda ~s ...)"
             (lambda-name node)
             (nconc (mapcar #'variable-unique-name (lambda-variables node))
                    (let ((v (lambda-rest-var node)))
                      (if v (variable-unique-name v)
                        '()))))
     ;;why does it think that the continuation arg(s?) are in the arg regs?
     ;;is this just to mark the returned value of the unknown call this is a continuation
     ;;of as being in the first arg reg? is this always a continuation of a call that returns
     ;; in a1? and what about multiple values?
;     (mark-vars-in-regs (lambda-variables node))
     ;; if this is only a continuation to a call then open will save the dest that the
     ;;continuation wants so the continuation will get its arg (what about multiple values?) where
     ;; it wants it
     ;; also hack ignored value
     (let ((vars (lambda-variables node)))
       (when (and vars
                  (variable-refs (car vars)))
         (let ((wants (continuation-wants node)))
           (mark (car vars) (get-register 'pointer node (if (register? wants) wants '*))))))
     )
    (STRATEGY/VFRAME
     (ass-comment "Procedure ~s (lambda ~s ...)"
             (lambda-name node)
             (mapcar #'variable-unique-name (lambda-variables node)))
     (mark (lambda-self-var *lambda*) P)
     (mark-vars-in-regs (cdr (lambda-variables node))))
    (STRATEGY/EZCLOSE
     (ass-comment "Procedure ~s (lambda ~s ...)"
             (lambda-name node)
             (mapcar #'variable-unique-name (lambda-variables node)))
     (mark-vars-in-regs (cdr (lambda-variables node))))
    (STRATEGY/LABEL
     (ass-comment "Label procedure ~s (lambda ~s ...)"
             (lambda-name node)
             (mapcar #'variable-unique-name (lambda-variables node)))
     (cond ((join-point-contour-needed? (lambda-env node))
            (let ((contour (join-point-contour (lambda-env node))))
              (mark contour P)
              (if (closure-cit-offset (environment-closure
                                        (lambda-env (variable-binder contour))))
                  (generate-move (reg-offset P -2) TP)))))
     (mapc #'(lambda (var arg-spec)
               (mark var (car arg-spec)))
           (if (continuation? node)
               (lambda-variables node)
             (cdr (lambda-variables node)))
          (join-point-arg-specs (lambda-env node)))
     (dolist (pair (join-point-global-registers (lambda-env node)))
               (mark (cdr pair) (car pair))))))


;;; Mark vars as being in argument registers

(defun mark-vars-in-regs (vars)
  (do ((vars vars (cdr vars))
       (reg A0 (+ reg 1)))
      ((or (>= reg AN) (null vars))
       (do ((vars vars (cdr vars))
            (reg (+ reg (1+ *argument-registers*)) (1+ reg)))
           ((null vars))
         (cond ((and (car vars) (variable-refs (car vars)))
                (mark-temp (car vars) reg)))))
    (cond ((and (car vars) (variable-refs (car vars)))
           (mark (car vars) reg)))))

;;; A closure is n-ary if it has a non null rest arg.

(defun n-ary? (closure) (lambda-rest-var closure))

(defun n-ary-setup (node)
  (cond ((used? (lambda-rest-var node))
         (generate-nary-setup node
                              (if (eq (lambda-strategy node) strategy/stack)
                                  (length (lambda-variables node))
                                  (length (cdr (lambda-variables node))))))))



(defun allocate-primop-call (node)
  (let* ((prim (primop-value (call-proc node))))
    (cond ((primop.conditional? prim)
           (allocate-conditional-primop node prim))
          ((and (eq prim primop/contents-location)
                (not (eq (leaf-value (call-arg-n 2 node)) primop/cell-value)))
           (allocate-location node prim))
          ((primop.special? prim)
           (primop.generate prim node))
          (t
           (really-allocate-primop-call node prim)))))


;;; ALLOCATE-CONDITIONAL-PRIMOP When we come to a split we save the state of
;;; the world and traverse one arm, then restore the state and traverse the
;;; other.

(defun allocate-conditional-primop (node prim)
  (primop.generate prim node)
  (let ((then (then-cont node))
        (else (else-cont node)))
  (multiple-value-bind (then else)
;; emit-jump is for now just emiting a jump
;; it doesn't save both tags for possible
;; reordering of blocks
;      (cond ((or (leaf-node? then)
;                (leaf-node? else)
;                (< (lambda-trace then)
;                   (lambda-trace else)))
;            (values then else))
;           (t
;            (values else then)))

      (values then else)  ;efh

    (let ((*registers* (copy-registers))
          (*stack-pos* *stack-pos*)
          (*lambda* *lambda*))
      (emit-tag then)
      (cond ((lambda-node? then)
;             (dolist (n (cddr (call-args node)))
;                     (kill-if-dead n then))
             (allocate-call (lambda-body then)))
            (t
             (allocate-conditional-continuation node then)))
      (return-registers))
    (restore-slots)
    (emit-tag else)
    (cond ((lambda-node? else)
;           (dolist (n (cddr (call-args node)))
;            (kill-if-dead n else))
           (allocate-call (lambda-body else)))
          (t
           (allocate-conditional-continuation node else))))))

;; We must decide whether to try to delay dereferencing the location.
;; We do this if the value is used just once and in the next frob and
;; is an operand to a primop.




(defun allocate-location (node prim)
  (let ((c (cont node)))
    (if (and (lambda-node? c)
             (let ((refs (variable-refs (car (lambda-variables c)))))
               (and refs
                    (null (cdr refs))
                    (let ((p (node-parent (node-parent (car refs)))))
                      (or (and (eq p c)
                               (let ((proc (call-proc (lambda-body c))))
                                 (and (primop-node? proc)
                                      (not (eq (primop-value proc)
                                            primop/make-cell)))))
                          (and (eq (node-parent (node-parent p)) c)
                               (let ((proc (call-proc (node-parent (car refs)))))
                                 (and (primop-node? proc)
                                      (not (eq (primop-value proc)
                                            primop/contents-location))))
                               (let ((p (call-proc (lambda-body c))))
                                 (and (primop-node? p)
                                      (eq (primop-value p)
                                           primop/contents-location))))))
                    (reps-compatable?
                      (primop.rep-wants (leaf-value (call-arg-n 2 node)))
                      (variable-rep (car (lambda-variables c)))))))
        (generate-location-access node)
        (really-allocate-primop-call node prim))))


(defun reps-compatable? (accessor-rep use-rep)
  (and (eq (rep-size accessor-rep) (rep-size use-rep))
       (not (rep-converter accessor-rep use-rep))))

(defun really-allocate-primop-call (node prim)
  (let ((c (cont node)))
    (cond ((lambda-node? c)
           (let ((cont (call-hoisted-cont node)))
             (when cont
               (mapc #'(lambda (a-pair)
                         (or (member (car a-pair) (lambda-live c) :test #'eq)
                             (zerop (variable-number (car a-pair)))
                             (some #'(lambda (node)
                                       (and (leaf-node? node)
                                            (eq (leaf-value node) (car a-pair ))))
                                   (cdr (call-args node)))
                             (kill (car a-pair))))
                     (closure-env (environment-closure (lambda-env cont))))))
           (primop.generate prim node)
;;why?
;           (dolist (node (cdr (call-args node)))
;            (kill-if-dead node c))
           (allocate-call (lambda-body c)))
          (t
           (primop.generate prim node)
           (dolist (node (cdr (call-args node)))
             (if (leaf-node? node) (kill (leaf-value node))))
           (fetch-continuation node c)
           (clear-slots)
           (generate-return 1)))))


;;; ACCESS-VALUE This is the primary routine to get addressability to values.
;;; Just a giant case statement.

(defun access-value (node value)
 (let* ((v (and (variable-p value) (variable-known value)))
        (value (if v (lambda-self-var v) value))
        (spec (register-loc value)))
   (cond (spec
          (cond ((integerp spec))
                (t
                 (cond ((consp (car spec))
                        (unlock (caar spec))
                        (let ((var (reg-node (caar spec))))
                          (if var (kill-if-dying var node)))
                        (unlock (cdar spec)))
                       (t
                        (unlock (car spec))
                        (let ((var (reg-node (car spec))))
                          (if var (kill-if-dying var node)))))
                 (setf (register-loc value) nil)))
              spec)
        ((temp-loc value))
        ((variable-p value)
         (let ((binder (variable-binder value)))
           (cond ((not binder)
                  (lookup node value nil))
                 ((and (zerop (variable-number value))
                       (assoc binder (closure-env *unit*) :test #'eq))
                  (lookup node binder nil))
                 (t
                  (lookup node value binder)))))
        ((primop? value)
         (if (eq value primop/undefined)
             (machine-num 0)
             (lookup node value nil)))
;; ?? nil and t are addressable
;        ((eq value t)
;         (machine-num header/true))
;        ((eq value nil)
;          nil-reg)
        ((addressable? value)
         (lit value))
        (t
         (lookup node value nil)))))

;;; LOOKUP If the value is a known procedure, if it is in the unit we get it
;;; from there, otherwise we get the variable which the known procedure is
;;; bound to.

(defun lookup (node value lambda-bound?)
  (ecase (lambda-strategy *lambda*)
    ((strategy/stack strategy/ezclose)
     (fetch-from-stack node value lambda-bound?))
    ((strategy/vframe)
     (let ((contour (lambda-self-var *lambda*)))
       (->register 'pointer node contour '*)
       (fetch-from-vframe node contour value lambda-bound?)))
    ((strategy/heap)
     (let ((contour (lambda-self-var *lambda*)))
       (->register 'pointer node contour '*)
       (fetch-from-heap node contour value lambda-bound?)))))





;;; ACCESS-FROM-UNIT Get from unit when there is a closure-internal-template.
;;; If we have one, just offset from template-pointer. If we are internal to
;;; a closure which has one, get it first and then offset into unit.

#|||
(define (access-from-unit node contour var)
  (let ((closure (environment-closure (lambda-env (variable-binder contour)))))
    (cond ((closure-cit-offset closure)
           => (lambda (current-offset)
                (let ((cl? (or (and (node? var) (lambda-node? var))
                           (closure? var)))
                      (disp (fx- (cdr (assq var (closure-env *unit*)))
                                 (fx+ current-offset tag/extend))))
                  (cond ((and (eq? (lambda-strategy *lambda*) strategy/heap)
                              (eq? contour (car (closure-members closure))))
                         (if cl?
                             (list (reg-offset TP (fx+ disp tag/extend)))
                             (reg-offset TP disp)))
                        ((register-loc (variable-binder (car (closure-members closure))))
                         => (lambda (reg)
                              (if cl?
                                  (list (reg-offset reg (fx+ disp tag/extend)))
                                  (reg-offset reg disp))))
                        (else
                         (lock (register-loc contour))
                         (let ((reg (get-register 'pointer node '*)))
                           (unlock (register-loc contour))
                           (generate-move
                              (reg-offset (register-loc contour)
                                          (fx- (fx- 0 tag/extend)
                                               (cdr (assq contour
                                                    (closure-env closure)))))
                              reg)
                           (mark (variable-binder (car (closure-members closure)))
                                 reg)
                           (if cl?
                               (list (reg-offset reg (fx+ disp tag/extend)))
                               (reg-offset reg disp))))))))
          (else nil))))

|||#

(defun get-env (var)
  (lambda-env (variable-binder var)))


;;; Yukk.  Here we get a variable from a stack frame.  If it is in the frame
;;; we are OK.  Otherwise we chain down stack frames as long as they are there.
;;; These frames are all simple offsets from SP.  When we arrive at a pointer
;;; into the heap, we load that pointer into a register and go to the heap
;;; code to do the rest.

#|||
(define (fetch-from-vframe node contour value lambda-bound?)
  (iterate loop ((offset 0) (l (variable-binder contour)))
    (select (lambda-strategy l)
      ((strategy/label strategy/open)
       (loop offset (node-parent (node-parent l))))
      (else
       (cond ((not (lambda-env l))
              (loop offset (node-parent (node-parent l))))
             (else
              (let ((closure (environment-closure (lambda-env l))))
                (cond ((and lambda-bound? (assq value (closure-env closure)))
                       => (lambda (env-pair)
                            (reg-offset (register-loc contour)
                                        (+ offset
                                             (- (cdr env-pair) tag/extend)))))
                      ((closure-link closure)
                       => (lambda (link)
                       (let ((accessor (reg-offset (register-loc contour)
                                                   (- (+ offset CELL)
                                                        tag/extend))))
                         (into-register 'pointer node link accessor '*)
                         (xselect (lambda-strategy (variable-binder link))
                            ((strategy/heap)
                             (fetch-from-heap node link value lambda-bound?))
                            ((strategy/vframe)
                             (fetch-from-vframe node link value lambda-bound?))))))
                      (else
                       (loop (+ (closure-size closure) offset)
                             (node-parent (node-parent l))))))))))))
|||#



(defun fetch-from-stack (node value lambda-bound?)
  (do ((offset 0)
       (l *lambda* (node-parent (node-parent l))))
      (())
    (let ((strategy (lambda-strategy l)))
      (cond ((or (eq strategy strategy/label)
                 (eq strategy strategy/open)
                 (not (lambda-env l))))
            (t
             (let* ((closure (environment-closure (lambda-env l)))
                    (env-pair (and lambda-bound? (assoc value (closure-env closure) :test #'eq))))
               (cond (env-pair
                      (return (reg-offset SP (+ offset *stack-pos* (cdr env-pair)))))
                     (t
                      (let ((link (closure-link closure)))
                        (cond (link
                               (let ((accessor (reg-offset SP (+ *stack-pos*
                                                                 (+ offset CELL)))))
                                 (into-register 'pointer node link accessor '*)
                                 (return (ecase (lambda-strategy (variable-binder link))
                                           (STRATEGY/HEAP
                                            (fetch-from-heap node link value lambda-bound?))
                                           (STRATEGY/VFRAME
                                            (fetch-from-vframe node link value lambda-bound?))))))
                              (t
                               (incf offset (closure-size closure)))))))))))))



(defun closure-internal-closure? (value closure)
  (cond ((not (eq closure *unit*))
         (member value (closure-members closure) :test #'eq))
        (t
         (or (and (node-p value) (lambda-node? value))
             (closure-p value)))))

(defun fetch-from-heap (node contour value lambda-bound?)
  (do* ((contour contour (caadr a-list))
        (env (get-env contour) (get-env contour))
        (a-list))
       (())
    (format t "~%Contour:     ~s" contour)
    (format t "~%Env:         ~s" env)
    (format t "~%Closure-env: ~s" (closure-env (environment-closure env)))
    (setq a-list (closure-env (environment-closure env)))
    (let* ((current-offset (environment-cic-offset env))
           (pair (assoc value a-list :test #'eq)))
      (cond (pair
             (return (if (closure-internal-closure? value
                                                    (environment-closure env))
                         (list (reg-offset (register-loc contour)       ; *** hack
                                           (- (cdr pair) current-offset)))
                       (reg-offset (register-loc contour)
                                   (- (cdr pair)
                                      (+ current-offset tag/extend))))))
            (t (let ((acc (and (not lambda-bound?) (access-from-unit node contour value))))
                 (cond (acc (return acc))
                       (t  (format t "~&into-register")
                        (into-register 'pointer node (caadr a-list)
                                       (reg-offset  (register-loc contour)
                                                    (+ (- current-offset) tag/extend))
                                       '*)))))))))


(defun fetch-continuation (node leaf)
  (let* ((var (leaf-value leaf))
         (amount (fetch-continuation-from-stack node var))
         (proc (leaf-value (call-proc node))))
    (let ((n (cond ((not (eq (lambda-strategy (variable-binder var))
                             strategy/ezclose))
                    0)
                   ((eq var proc) (follow-ezclose var 0))
                   (t (let ((label (variable-known proc)))
                        (if label
                            (follow-ezclose var (lambda-depth label))
                          (follow-ezclose var 0)))))))
      (adjust-stack-pointer (+ amount n)))))


(defun follow-ezclose (var depth)
  (do ((pair (variable-type var) (caar pair))
       (i 0 (+ i (cdar pair))))
    ((or (atom pair)
         (< (lambda-depth (variable-binder (caar pair))) depth))
     i)))





;;; Code to get a continuation off the stack.
;;; Search up the tree until we find it.
;;; This relies on generating code for the body of a labels FIRST.


(defun fetch-continuation-from-stack (node var)
  (do ((offset 0)
       (l (node-parent node) (node-parent (node-parent l))))
      ((eq (variable-binder var) l)
       offset)
    (case (lambda-strategy l)
      ((STRATEGY/STACK)
;*******
       0  ;-efh
;      we don't push stuff
;       (incf offset (closure-size (environment-closure (lambda-env l))))
;*******
       )
      ((STRATEGY/VFRAME)
       (cond ((or (null (lambda-env l))
                  (vframe-p (lambda-live l))))
             (t
              (let ((vsize (closure-size (environment-closure
                                           (lambda-env l))))
                    (a (fetch-continuation-from-stack (node-parent l) var))
                    (vframe (make-vframe)))
                (setf (vframe-lambda vframe) l)
                (setf (vframe-return-size vframe) (+ vsize a))
                (setf (lambda-live l) vframe)
                (lambda-queue vframe))))
       (return offset))
      ((STRATEGY/EZCLOSE)
       (let ((frame-size
               (+ (fetch-continuation-from-stack (node-parent l) var)
                  (closure-size (environment-closure (lambda-env l))))))
         (dolist (label (cdr (call-args (lambda-body l))))
           (push (cons var frame-size)
                 (variable-type (car (lambda-variables label)))))
         (cond ((not (vframe-p (lambda-live l)))
                (let ((vframe (make-vframe)))
                  (setf (vframe-lambda vframe) l)
                  (setf (vframe-return-size vframe) frame-size)
                  (setf (lambda-live l) vframe)
                  (lambda-queue vframe)))))
       (return offset)))))

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


;;; ALLOCATE-CALL The "top".  Dispatch on the type of call.

(defun allocate-call (node)
;  (if *call-break?* (breakpoint (pp-cps node)))
  (let ((cont (call-hoisted-cont node)))
    (if cont
;don't make any stack closure
;don't push any return address
;this is not the place for the open
;it must be before the arg calculations
;(way up in the cps tree)
;; so we stuff in a call to primop/open at node conversion time
;; (but what about real stack closures???)
;       ;; make-stack-closure pushes the return address here
;        ;; this will not be called if the continuation was
;        ;; not hoisted  (hoist-continuation is called
;        ;; by live-analyze-lambda only for strategy/stack nodes)
;        (make-stack-closure node cont)))
;; but we must queue the continuation...
          (lambda-queue cont)))
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (ass-comment "~s" (pp-cps node))
           (allocate-primop-call node))
          ((lambda-node? proc)
           (generate-let node))
          (t
           (let ((proc (variable-known (leaf-value proc))))
             (cond (proc
                    (ass-comment "Call known procedure ~s"
                                 (cons (lambda-name proc) (cdr (pp-cps node))))
                    (cond ((zerop (call-exits node))
                           (allocate-known-return node proc))
                          ((= (call-exits node) 1)
                           (allocate-known-call node proc))
                          (t (error "Call exits not 0 or 1 in ALLOCATE-CALL"))))
                   ((zerop (call-exits node))
                    (ass-comment "Return from procedure ~s" (pp-cps node))
                    (allocate-return node))
                   ((= (call-exits node) 1)
                    (ass-comment "Call unknown procedure ~s" (pp-cps node))
                    (allocate-general-call node))
                   (t
                    (bug "too many exits - ~s" node))))))))

(defun allocate-known-call (node proc)
  (if (not (or (lambda-rest-var proc)
               (= (length (lambda-variables proc))
                  (length (call-args node)))))
      (warning "Wrong number of arguments in call ~s" (pp-cps node)))
  (ecase (lambda-strategy proc)
    (STRATEGY/LABEL (allocate-label-call node proc))
    (STRATEGY/HEAP (allocate-known-heap-call node proc))
    (STRATEGY/EZCLOSE (allocate-ezclose-call node proc))
    ((STRATEGY/VFRAME STRATEGY/EZCLOSE) (allocate-vframe-call node proc)))
  (if (<= (lambda-trace proc) (lambda-trace (node-parent node)))
      (generate-avoid-jump proc)
      (generate-jump proc)))


#||||
(define (allocate-known-heap-call node proc)
  (let ((cont ((call-arg 1) node)))
    (parallel-assign-general node)
    (if (leaf-node? cont) (fetch-continuation node cont)))
  (clear-slots)
  (generate-move (reg-offset P -2) TP)
  (if (n-ary? proc) (generate-move (length (call-args node)) NARGS)))

|||#

;;; ALLOCATE-LABEL-CALL If this is the path that arrives first, go to
;;; determine where the free variables of the join are to be kept.
;;; Make the state of the registers the way the join point wants it.
;;; Parallel assign and jump to the label.

(defun allocate-label-call (node proc)
  (let ((join (get-or-set-join-state node proc))
        (cont (call-arg-n 1 node)))
    (parallel-assign node
                     (cdr (call-args node))
                     (join-point-arg-specs join)
                     nil
                     (join-point-global-registers join))
    (if (leaf-node? cont) (fetch-continuation node cont))
    (let ((contour (and (join-point-contour-needed? join)
                        (join-point-contour join))))
      (if contour
          (let ((b (variable-binder contour)))
            (if (and (closure-cit-offset (environment-closure (lambda-env b)))
                     (not (eq *lambda* b)))
                (generate-move (reg-offset P -2) TP))))))
;  (clear-slots)
  )


(defun allocate-vframe-call (node proc)
  (let ((cont (call-arg-n 1 node)))
    (cond ((lambda-node? cont)
           (parallel-assign-vframe node proc))
          (t
           (parallel-assign-vframe node proc)
           (fetch-continuation node cont))))
  (clear-slots)
  (if (n-ary? proc) (generate-move (length (call-args node)) NARGS)))


(defun allocate-ezclose-call (node proc)
  (let ((cont (call-arg-n 1 node)))
    (cond ((lambda-node? cont) (bug "non-variable continuation to ezclose call"))
          (t
           (parallel-assign-known node)
           (fetch-continuation node cont))))
  (clear-slots)
  (if (n-ary? proc) (generate-move (length (call-args node)) NARGS)))


#|||
(define (allocate-known-return node proc)
  (select (lambda-strategy proc)
    ((strategy/label) (allocate-label-return node proc))
    (else
     (parallel-assign-return node)
     (fetch-continuation node (call-proc node))
     (clear-slots)
     (generate-jump proc))))




(define (allocate-label-return node proc)
  (let ((join (get-or-set-join-state node proc)))
    (cond ((n-ary? proc)
           (really-parallel-assign node '() '() (join-point-global-registers join)))
          (else
           (parallel-assign node
                            (call-args node)
                            (join-point-arg-specs join)
                            nil
                            (join-point-global-registers join)))))
  (fetch-continuation node (call-proc node))
  (clear-slots)
  (generate-jump proc))
|||#

(defun allocate-conditional-continuation (node proc-leaf)
  (let ((proc (variable-known (leaf-value proc-leaf))))
    (case (lambda-strategy proc)
      (STRATEGY/STACK)
      (t
       (let ((join (get-or-set-join-state node proc)))
         (parallel-assign node
                          '()
                          (join-point-arg-specs join)
                          nil
                        (join-point-global-registers join)))))
   (fetch-continuation node proc-leaf)
;   (clear-slots)     why??
   (generate-jump proc)))


;; much changed
;;; parallel-assign will lock the regs
;;; so pop-o-regs should unlock
(defun allocate-general-call (node)
  (let* ((cont (call-arg-n 1 node))
         (tail-p (not (lambda-node? cont))))
    (parallel-assign-general node)
    (if tail-p (fetch-continuation node cont))
    (generate-general-call (call-proc node)
                           tail-p
                         (- (length (call-args node)) 2))  ;flush cont also flush P
    (pop-o-regs)))


(defun allocate-return (node)
  (parallel-assign-return node)
  (fetch-continuation node (call-proc node))
;  (clear-slots)     why???
  (generate-return (length (call-args node))))



;;; does anyone call this except to assign to call regs?
(defun parallel-assign-general (node)
  (parallel-assign node (cdr (call-args node))   ;don't need to pass proc in P (cons (call-proc node) (cdr (call-args node)))
                        *open-registers*         ; was nil (P + arg regs)
                        t '()))

(defun parallel-assign-known (node)
  (parallel-assign node (cdr (call-args node)) nil nil '()))


(defun parallel-assign-vframe (node proc)
  (if (not (lambda-env (node-parent (node-parent proc))))
      (parallel-assign node (cdr (call-args node)) nil nil '())
      (parallel-assign node (cdr (call-args node)) nil nil
               (list (cons P (lambda-self-var (node-parent (node-parent proc))))))))

(defun parallel-assign-return (node)
  (let ((nargs (length (call-args node))))                        ;added   -efh
    (parallel-assign node (call-args node) (if (= nargs 1)        ;nil
                                               *return-dest*
                                             *return-registers*)
                     nil '())))


;;; PARALLEL-ASSIGN Cons a closure if necessary.  It is known that there
;;; will only be one that needs to be consed.

(defun parallel-assign (node args p-list proc? solve-list)
  (let* ((pos-list (if p-list p-list (reg-positions (length args) proc?)))
         (closure (get-closure args)))
    (if closure
        (make-heap-closure node closure))
    (really-parallel-assign node args pos-list solve-list)))

(defun get-closure (args)
  (some #'(lambda (arg)
            (and (lambda-node? arg)
                 (not (eq (environment-closure (lambda-env arg)) *unit*))
                 (environment-closure (lambda-env arg))))
       args))


;;; do-now - register or temp pairs (source . target)
;;; trivial - immediate or lambda
;;; do-later - environment
;;; See implementor for this stuff. Hairy!!

(defstruct arg-mover
  from
  from-rep
  to
  to-rep)

(defun mover (from from-rep to to-rep)
  (let ((a (make-arg-mover)))
    (setf (arg-mover-from a) from)
    (setf (arg-mover-from-rep a) from-rep)
    (setf (arg-mover-to a) to)
    (setf (arg-mover-to-rep a) to-rep)
    a))

(defun really-parallel-assign (node args pos-list solve-list)
  (multiple-value-bind (do-now trivial do-later) (sort-by-difficulty args pos-list)
;    (format t "~%do-now: ~s" do-now)
;    (format t "~%trivial: ~s" trivial)
;    (format t "~%do-later: ~s" do-later)
    (multiple-value-bind (do-now do-later) (add-on-free-list do-now do-later solve-list)
;      (format t "~%------------------------")
;      (format t "~%do-now: ~s" do-now)
;      (format t "~%do-later: ~s" do-later)
      (solve node do-now do-later)
      (lock AN)                     ; contains closures
      (dolist (pair trivial)
        (if (lambda-node? (car pair))
            (do-trivial-lambda node (car pair) (cdr pair))))
      (unlock AN)
      (dolist (pair do-later)
        (do-indirect node pair))
      (dolist (pair trivial)
        (if (not (lambda-node? (car pair)))
            (do-immediate node (car pair) (cdr pair)))))))


(defun add-on-free-list (do-now do-later solve-list)
  (dolist (pair solve-list)
    (let ((reg (or (register-loc (cdr pair))
                   (temp-loc (cdr pair)))))
      (cond (reg
             (push (mover reg (variable-rep (cdr pair))
                          (car pair) (variable-rep (cdr pair)))
                   do-now))
            (t
             (setq do-later
                   (if (= (car pair) P)
                       (nconc do-later (list (cons (cdr pair) P)))
                     (cons (cons (cdr pair) (car pair))
                           do-later)))))))
  (values do-now do-later))


(defun sort-by-difficulty (args pos-list)
  (let ((do-now '()) (trivial '()) (do-later '()) (pos-list pos-list))
    (dolist (arg args)
      (cond ((lambda-node? arg)
             (let ((l arg))
               (cond ((eq (environment-closure (lambda-env l)) *unit*)
                      (push (cons l (car pos-list)) do-later))
                     (t
                      (push (cons l (car pos-list)) trivial)))))
            ((addressable? (leaf-value arg))
             (push (cons arg (car pos-list)) trivial))
            (t
             (let* ((val (leaf-value arg))
                    (v (and (variable-p val)
                            (variable-known val)))
                    (value (if v (lambda-self-var v) val))
                    (reg (or (register-loc value) (temp-loc value))))
               (cond (reg
                      (push (mover reg (variable-rep value)
                                   (caar pos-list) (cdar pos-list))
                            do-now))
                     (t
                      (setq do-later
                            (if (= (caar pos-list) P)
                                (nconc do-later (list (cons value (car pos-list))))
                              (cons (cons value (car pos-list)) do-later))))))))
      (pop pos-list))
    (values do-now trivial do-later)))


(defun do-immediate (call-node node reg-rep)
  (generate-move (value-with-rep (leaf-value node) (cdr reg-rep))
                 (car reg-rep)))


(defun do-indirect (node pair)
  (cond ((and (node-p (car pair)) (lambda-node? (car pair)))
         (lambda-queue (car pair))
         (generate-move (lookup node (car pair) nil) (cadr pair))
         (let ((rn (reg-node (cadr pair))))
           (kill rn))
         (lock (cadr pair)))
        ((atom (cdr pair))                               ; free var of label
         (really-rep-convert node
                             (access-value node (car pair))
                             (variable-rep (car pair))
                             (cdr pair)
                             (variable-rep (car pair)))
         (let ((rn (reg-node (cdr pair)))) (kill rn))
         (mark (car pair) (cdr pair))
         (lock (cdr pair)))
        (t
         (let ((to-rep (cddr pair))
               (to-reg (cadr pair)))
           (really-rep-convert node
                               (access-value node (car pair))
                               (if (variable-p (car pair))
                                   (variable-rep (car pair))
                                   'rep/pointer)
                               to-reg
                               to-rep)
           (let ((var (reg-node to-reg)))
             (when var
               (setf (register-loc var) nil)
               (setf (reg-node to-reg) nil)))
           (kill (car pair))
           (mark (car pair) to-reg)
           (lock to-reg)))))


(defun solve (node movers do-later)
;  (format t "~%Solve: node:     ~s" node)
;  (format t "~%       movers:   ~s" movers)
;  (format t "~%       do-later: ~s" do-later)
  (let* ((contour (lambda-self-var *lambda*))
         (vals (mapcar #'(lambda (mover)
                           (reg-node (arg-mover-to mover)))
                       movers))
         (mover (and do-later
                     (find contour movers
                           :test #'eq
                           :key #'(lambda (mover)
                                    (reg-node (arg-mover-to mover)))))))
    (cond (mover
           (if (not (eq (arg-mover-from mover) (arg-mover-to mover)))
               (free-register node (register-loc contour)))
           (dolist (val vals)
             (unless (eq val contour) (kill val))))
          (t
           (mapc #'kill vals)))
    (dolist (mover movers)
      (lock (arg-mover-to mover)))
    (multiple-value-bind (movers self-movers) (separate-self-movers movers)
      (do ((movers movers)
           (targets (mapcar #'arg-mover-to movers))
           (temp nil))
          ((null movers))
        (dolist (candidate targets (let ((mover (car movers)))
                                     (generate-move (arg-mover-to mover)
                                                    (reg-offset TASK
                                                                (if (eq (arg-mover-to-rep mover) 'rep/pointer)
                                                                    task/extra-pointer
                                                                  task/extra-scratch)))
                                     (really-rep-convert node
                                                         (arg-mover-from mover)
                                                         (arg-mover-from-rep mover)
                                                         (arg-mover-to mover)
                                                         (arg-mover-to-rep mover))
                                     (setq movers (cdr movers))
                                     (setq targets (delete (arg-mover-to mover) targets :test #'eq))
                                     (setq temp (arg-mover-to mover))))
          (cond ((not (member candidate movers :test #'from-reg-eq?))
                 (let ((mover (car (member candidate movers :test #'to-reg-eq?))))
                   (really-rep-convert node
                                       (cond ((eq (arg-mover-from mover) temp)
                                              (if (eq (arg-mover-to-rep mover) 'rep/pointer)
                                                  (reg-offset TASK task/extra-pointer)
                                                (reg-offset TASK task/extra-scratch)))
                                             (t
                                              (arg-mover-from mover)))
                                       (arg-mover-from-rep mover)
                                       (arg-mover-to mover)
                                       (arg-mover-to-rep mover))
                   (setq movers (delete mover movers :test #'eq))
                   (setq targets (delete (arg-mover-to mover) targets :test #'eq))
                   (return t))))))
      (dolist (mover self-movers)
        (really-rep-convert node (arg-mover-from mover)
                            (arg-mover-from-rep mover)
                            (arg-mover-to mover)
                            (arg-mover-to-rep mover))))))

(defun separate-self-movers (movers)
  (let ((m '()) (s '()))
    (dolist (mover movers)
      (if (eq (arg-mover-from mover) (arg-mover-to mover))
          (push mover s)
        (push mover m)))
    (values m s)))

(defun to-reg-eq? (reg mover) (= (arg-mover-to mover) reg))
(defun from-reg-eq? (reg mover) (= (arg-mover-from mover) reg))

(defun access/make-closure (node lam)
  (let* ((closure (environment-closure (lambda-env lam))))
    (cond ((eq closure *unit*)
           (lambda-queue lam)
           (lookup node lam nil))
          (t
           (make-heap-closure node closure)
           nil))))


;;; MAKE-STACK-CLOSURE Push a continuation on the stack.  For now there are no
;;; scratch values.  When there are we will need to push zeroes for all the
;;; scratch slots and fill them in after pushing the template.  This is because
;;; the GC assumes that anything on top of the stack until the first template
;;; is a valid pointer.


(defun make-stack-closure (node cont)
  (lambda-queue cont)
  (let* ((closure (environment-closure (lambda-env cont)))
         (a-list (cdr (closure-env closure))))
    ;;; push zeros for scratch slots
    (dotimes (i (closure-scratch closure))
      (generate-push (machine-num 0)))

    ;; push pointer vars in environment of continuation
    ;; this may not be necc if they are in a regs
    ;; they will not be clobbered by the call -efh
    (dolist (pair (nthcdr (closure-scratch closure) (reverse a-list)))
      (generate-push (access-value node (car pair))))

    ;; this is where the return address gets pushed
    (generate-open (template cont)) ;efh
;    (generate-push-address (template cont))

    ;; move variables which are not pointers onto the stack
    (dolist (pair (nthcdr (closure-pointer closure) a-list))
      (really-rep-convert node
                          (access-value node (car pair))
                          (variable-rep (car pair))
                          (reg-offset SP (cdr pair))
                          (variable-rep (car pair))))))

(defun make-vframe-closure (node l closure ez?)
  (mapc #'lambda-queue (closure-vframe-lambdas closure))
  (let ((a-list (cdr (closure-env closure))))
    (dotimes (i (closure-scratch closure))
      (generate-push (machine-num 0)))
    (dolist (pair (nthcdr (closure-scratch closure) (reverse a-list)))
      (generate-push (access-value node (car pair))))
    (generate-push-address (template l))
    (dolist (pair (nthcdr (closure-pointer closure) a-list))
      (really-rep-convert node
                          (access-value node (car pair))
                          (variable-rep (car pair))
                          (reg-offset SP (cdr pair))
                          (variable-rep (car pair))))))

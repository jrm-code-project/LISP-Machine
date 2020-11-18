;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-


;;;; Strategy Analysis

;;; Determine strategy for compiling a closure

(defmacro define-lambda-strategies (&rest strategies)
  `(progn ,@(mapcar #'(lambda (strat)
                        (let ((strat (concatenate-symbol 'strategy/ strat)))
                          `(defconstant ,strat ',strat)))
                    strategies)))


(define-lambda-strategies open label proc heap)

(defun strategy-analyze-top (node)
  (setf (lambda-strategy node) strategy/heap)
  (strategy-analyze-lambda-node node)) ;(call-arg-n 1 (lambda-body node))))

(defun strategy-analyze-call-node-args (node)
  (multiple-value-bind (exit-used-variables exit-called-variables exit-closed-variables)
       (strategy-analyze-call-exits node)
     (multiple-value-bind (arg-used-variables arg-called-variables arg-closed-variables)
          (strategy-analyze-call-args node)
        (values (union exit-used-variables arg-used-variables)
                (union exit-called-variables arg-called-variables)
                (union exit-closed-variables arg-closed-variables)))))

(defun strategy-analyze-call-args (node)
  (do ((used-variables     '())
       (called-variables   '())
       (closed-variables   '())
       (args (call-non-exit-args node) (cdr args)))
      ((null args)
       (values used-variables called-variables closed-variables))
    (let ((this-arg (first args)))
      (cond ((reference-node? this-arg)
             (setq used-variables
                   (adjoin (reference-variable this-arg) used-variables)))
            ((lambda-node? this-arg)
             ;; Always heap close over random call args.
             (setf (lambda-strategy this-arg) strategy/heap)
             (multiple-value-bind (used called closed)
                 (strategy-analyze-lambda-node this-arg)
               (setq used-variables (union used-variables used)
                     closed-variables (union '() ;called  ;called not closed over?
                                             (union used
                                                    (union closed
                                                           closed-variables))))))))))


(defun strategy-analyze-call-exits (node)
  (let ((exits (call-exits node)))
    (cond ((= exits 0) (values '() '() '()))
          ((= exits 1)
           (let ((the-exit (call-arg-n 1 node)))
             ;; Continuations can always be open.
             (cond ((lambda-node? the-exit)
                    (setf (lambda-strategy the-exit) strategy/open)
                    (strategy-analyze-lambda-node the-exit))
                   (t (values '() '() '())))))
          (t
           (do ((variables-used   '())
                (variables-called '())
                (variables-closed '())
                (exits (call-exit-args node) (cdr exits)))
               ((null exits)
                (values variables-used variables-called variables-closed))
             (let ((this-exit (first exits)))
               (if (lambda-node? this-exit)
                   (multiple-value-bind (used called closed)
                       (strategy-analyze-lambda-node this-exit)
                     ;; If more than one continuation,
                     ;; they should be labels.
                     (setf (lambda-strategy this-exit) strategy/label)
                     (setq variables-used   (union used variables-used)
                           variables-called (union called variables-called)
                           variables-closed (union closed variables-closed))))))))))

(defun strategy-analyze-lambda-node (node)
  (strategy-analyze-lambda-body (lambda-body node)))

(defun strategy-analyze-lambda-body (node)
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (let ((value (primop-value proc)))
             (cond
               ((eq value primop/conditional)
                (strategy-analyze-if node))
               ((eq value primop/y)
                (strategy-analyze-y (call-arg-n 1 node)))
;              ((eq value primop/undefined-effect)
;               (values '() '() '()))
               (t (strategy-analyze-call-node-args node)))))
           ((lambda-node? proc)
            (strategy-analyze-let node))
           (t
             (multiple-value-bind (arg-used arg-called arg-closed)
                 (strategy-analyze-call-node-args node)
               (values arg-used
                       (adjoin (reference-variable proc) arg-called)
                       arg-closed))))))

(defun strategy-analyze-if (node)
  (strategy-analyze-call-node-args node))

(defun strategy-analyze-y (node)
  (let ((all-args            (call-args (lambda-body node)))
        (all-bound-variables (lambda-variables node)))
    (let ((proc (first all-args))
          (args (rest  all-args))
          (argvars (rest all-bound-variables)))
      ;; Call proc is like a continuation.
      (setf (lambda-strategy proc) strategy/open)
      (multiple-value-bind (used-vars called-vars closed-vars)
          (strategy-analyze-lambda-node proc)
        (let ((y-used-vars   '())
              (y-called-vars '())
              (y-closed-vars '()))
          (dolist (this-arg args)
            (multiple-value-bind (used called closed)
                (strategy-analyze-lambda-node this-arg)
              (setq y-used-vars   (union used y-used-vars)
                    y-called-vars (union called y-called-vars)
                    y-closed-vars (union closed y-closed-vars))))
          (let ((strategy
                  (if (or (intersection argvars used-vars)
                          (intersection argvars y-used-vars)
                          (intersection argvars closed-vars)
                          (intersection argvars y-closed-vars))
                      strategy/heap
                      strategy/label)))
            (setf (lambda-strategy node) strategy)
;           (dolist (arg args)
;             (setf (lambda-strategy arg) strategy))
;           (mapc #'(lambda (arg var)
;                     (setf (lambda-strategy arg) strategy)
;                     (setf (variable-type var) arg))
;                 args
;                 argvars)
            (do ((vars argvars (cdr vars))
                 (lambdas args (cdr lambdas)))
                ((null vars))
              (set-label-strategy (car vars) (car lambdas) strategy))
            (values (union used-vars y-used-vars)
                    (if (eq strategy strategy/heap)
                        called-vars
                        (union called-vars y-called-vars))
                    (if (eq strategy strategy/heap)
                        (union y-closed-vars
                               (union y-called-vars closed-vars))
                        (union y-closed-vars closed-vars)))))))))


(defun strategy-analyze-let (node)
  (let ((proc (call-proc node))
        (args (call-args node)))
    (let ((bound-variables (lambda-variables proc))
          (lambda-bound-vars '()))
      ;; get all the variables which are bound to lambdas
      (do ((args args (cdr args))
           (bvs bound-variables (cdr bvs)))
          ((or (null args) (null bvs)))
        (when (lambda-node? (car args))
          (push (car bvs) lambda-bound-vars)))
      ;;Call proc is like a continuation.
      (setf (lambda-strategy proc) strategy/open)
      (multiple-value-bind (used-vars called-vars closed-vars)
           (strategy-analyze-lambda-node proc)
         (let ((strategy
                 (if (or (intersection lambda-bound-vars used-vars)
                         (intersection bound-variables closed-vars))
                     strategy/heap
                     strategy/label)))
           (do ((procs args (cdr procs))
                (vars bound-variables (cdr vars)))
               ((null procs))
             (let ((proc (car procs))
                   (var (car vars)))
               (cond ((lambda-node? proc)
                      (setf (lambda-strategy proc) strategy)
                      (setf (variable-type var) proc)
                      (multiple-value-bind (used called closed)
                          (strategy-analyze-lambda-node proc)
                        (if (eq strategy strategy/heap)
                            (setq used-vars (union used used-vars)
                                  closed-vars (union called (union closed closed-vars)))
                          (setq used-vars (union used used-vars)
                                called-vars (union called-vars called)
                                closed-vars (union closed closed-vars)))))
                     ;; In T lets only bound lambda nodes
                     ;; because literals and refs were always
                     ;; substituted in.  They could get away with
                     ;; that because of assignment conversion,
                     ;; but we aren't doing that.
                     ((reference-node? proc)
                      (setq used-vars
                            (adjoin (reference-variable proc) used-vars))))))
           (values used-vars called-vars closed-vars))))))



(defun set-label-strategy (var lambda strategy)
  (ecase strategy
    (strategy/heap (setf (lambda-strategy lambda) strategy))
    (strategy/label
     (setf (lambda-strategy lambda)
           (if (truly-label-p var lambda)
               strategy/label
             strategy/proc))
     (setf (variable-type var) lambda))))


;;; return a list of all continuations
;;; to calls to label-call-var
(defun raw-label-continuations (label-call-var label-cont-var)
  (delete-if #'(lambda (c)
                 (and (reference-node? c)
                      (eq (reference-variable c) label-cont-var)))
             ;; all the continuations to calls to the label
             (mapcar #'(lambda (ref)
                         (call-arg-n 1 (node-parent ref)))
                     (variable-refs label-call-var))))



;;; attempt to return a list of all real continuations
;;; to calls to label-call-var
;;; types should probably be ref nodes not variables
(defun label-continuations (label-call-var label-cont-var)
  (let ((continuations '()))
    (dolist (cont
             (raw-label-continuations label-call-var label-cont-var))
      (cond ((lambda-node? cont)
             (push cont continuations))
            ((reference-node? cont)
             (let ((known (find-variable-known (reference-variable cont))))
               (cond ((null known)
                      (pushnew (reference-variable cont) continuations))
                     ((lambda-node? known)
                      (pushnew known continuations))
                     ((variable-p known)
                      ;; this is non-optimal
                      ;; really we should find the lambda
                      ;; this may be bound to
                      (unless (eq known label-cont-var)
                        (pushnew known continuations)))
                     ((listp known)
                      (dolist (c (delete-if #'(lambda (c)
                                                (and (reference-node? c)
                                                     (eq (reference-variable c) label-cont-var)))
                                            known))
                        (pushnew c continuations)))
                     (t (bug "bad cont from known: ~" known)))))
            (t (bug "bad cont: ~s" cont))))
    continuations))


;;; this is not so great
;;; just look one level for continuations
;;; lightweight but cheap.
(defun find-variable-known (cvar)
  ;; first see if we already got it
  (let ((type (variable-type cvar)))
    (cond ((lambda-node? type)
           type)
          ((variable-p type)
           type)
          ((reference-node? type)
           (reference-variable type))
          (t
           (let ((binder (variable-binder cvar)))
             (cond ((eq (node-role binder) call-proc)
                    (call-arg-n 1 (node-parent binder)))
                   ;; check for label callers
                   ((let ((lambda (node-parent (node-parent binder))))
                      (and (node-parent lambda)
                           (primop-node? (call-proc (node-parent lambda)))
                           (eq (primop-value (call-proc (node-parent lambda))) primop/Y)))
                    (let ((caller-label-var
                            (nth (1- (call-arg-number (node-role binder)))
                                 (lambda-variables (node-parent (node-parent binder))))))
                      (raw-label-continuations caller-label-var cvar)))))))))



(defun truly-label-p (var lam)
  (let* ((lam-cont-var (car (lambda-variables lam)))
         (continuations (label-continuations var lam-cont-var)))
    (cond ((null continuations))
          ((null (cdr continuations))
           ;; If there is one real continuation
           ;; then the continuation arg to the label proc
           ;; can be said to be bound to the cont
           (setf (variable-type lam-cont-var) (car continuations))
           t)
          (t nil))))

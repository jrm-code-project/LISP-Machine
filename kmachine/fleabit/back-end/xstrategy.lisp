;;; -*- Mode:LISP; Readtable:CL; Base:10; package: NC -*-
;;;
;;;; Strategy Analysis
;;;
;;; Determine strategy for compiling a lambda node.
;;; There are four strategies:
;;;
;;; OPEN  - This lambda is a simple continuation, it is called
;;;         from a single place and returns to a single place.
;;;         It will turn into a piece of straight line code.
;;; LABEL - This lambda may be called from many places, but only has
;;;         one continuation, ie it always returns to the same place.
;;;         It can be a piece of inline code, but will need a label
;;;         which may be branched to.
;;; PROC  - This lambda is called from more than one place with different
;;;         continuations.  It needs to be an actual subroutine.  It is
;;;         always called and never used as an argument to any call and
;;;         therefore does not actually have to have an actual function
;;;         object associated with it, or be heap closed.
;;; HEAP  - This lambda is used as an argument to a call, and therefore
;;;         may persist indefinitly and be called with FUNCALL.  A function
;;;         object needs to be created and possibly a closure consed.
;;;
;;; The strategy analysis phase also computes the set of variables used
;;; within each lambda.  The used variables are the variables referenced
;;; within the lexical scope of the lambda.  This differs from the live
;;; variables which are referenced within the dynamic extent of the lambda.
;;; The used variables are needed for closure analysis.
;;; Variables which are used in strategy HEAP lambdas also have variable-closed
;;; set to T.

;;; This seems to be about as random a place to put this.

(defun analyze (top-node)
; (trace-analyze-top    top-node)
  (strategy-analyze-top top-node)
  (live-analyze-top     top-node)
  (env-analyze-top      top-node))

(defmacro define-lambda-strategies (&rest strategies)
  `(progn ,@(mapcar #'(lambda (strat)
                        (let ((strat (concatenate-symbol 'strategy/ strat)))
                          `(defconstant ,strat ',strat)))
                    strategies)))


(define-lambda-strategies open label proc heap)

;;; Since in Common Lisp there is no way to
;;; modify the value of a lexical function name,
;;; called lexical vars are never closed over.

;;; *but* , called continuations can be closed over (go, return-from)
;;; and we need to notice that they are closed???

(defun strategy-analyze-top (node)
  (setf (lambda-strategy node) STRATEGY/HEAP)
  (strategy-analyze-lambda node)
  (debug :strategy
    (format t "~%Strategy Analysis:")
    (pp-cps node :extra #'lambda-strategy))
  (debug :used
    (format t "~%Use Analysis:")
    (pp-cps node :extra #'lambda-used)))


(defun strategy-analyze-lambda (node)
  (multiple-value-bind (used called)
      (strategy-analyze-lambda-body node)
    (values (set-difference used
                            (lambda-rest+variables node))
            (set-difference called
                            (lambda-rest+variables node)))))

(defun strategy-analyze-lambda-body (node)
  (strategy-analyze-call (lambda-body node)))

(defun strategy-analyze-call (node)
  (let ((proc (call-proc node)))
    (cond
      ((primop-node? proc)
       (let ((primop (primop-value proc)))
         (cond ((eq primop PRIMOP/Y)
                (strategy-analyze-y (call-arg-n 1 node)))
               ((eq primop PRIMOP/%GO)
                (values nil (list (reference-variable (call-arg-n 2 node)))))
               (t (strategy-analyze-call-args node)))))
      ((lambda-node? proc)
       (strategy-analyze-let node))
      ((reference-node? proc)
       (multiple-value-bind (used called)
           (strategy-analyze-call-args node)
         (values used (adjoin (reference-variable proc) called))))
      (t (bug "bad call proc in strategy analysis: ~s" proc)))))
;      (t (strategy-analyze-call-args node)))))

(defun strategy-analyze-call-args (node)
  (multiple-value-bind (exit-used exit-called)
      (strategy-analyze-call-exits node)
    (multiple-value-bind (used called)
         (strategy-analyze-non-exit-args node)
      (values (union exit-used used)
              (union exit-called called)))))


(defun strategy-analyze-call-exits (node)
  (let ((exits (call-exits node)))
    (case exits
      (0 (values '() '()))
      (1 (let ((cont (call-arg-n 1 node)))
           (cond ((lambda-node? cont)
                  (setf (lambda-strategy cont) STRATEGY/OPEN)
                  (strategy-analyze-lambda cont))
                 (t (values '() (list (reference-variable cont)))))))
      (t (let ((used '())
               (called '()))
           (dolist (exit (call-exit-args node))
             (cond ((lambda-node? exit)
                    (setf (lambda-strategy exit) STRATEGY/LABEL)
                    (multiple-value-bind (exit-used exit-called)
                        (strategy-analyze-lambda exit)
                      (setq used (union exit-used used)
                            called (union exit-called called))))
                   (t (setq called (adjoin (reference-variable exit)
                                           called)))))
           (values used called))))))

(defun strategy-analyze-non-exit-args (node)
  (let ((used '())
        (called '()))
    (dolist (arg (call-non-exit-args node))
      (cond
        ((reference-node? arg)
         (setq used (adjoin (reference-variable arg)
                            used)))
        ((lambda-node? arg)
         (setf (lambda-strategy arg) STRATEGY/HEAP)
         (multiple-value-bind (lambda-used lambda-called)
             (strategy-analyze-lambda arg)
           (set-closed lambda-used)
           (set-closed lambda-called)
           (setq used (union lambda-used used)
                 called (union lambda-called called))))))
    (values used called)))


;;; If any of the values of the let are lambdas
;;; and the variable which is bound to it is used,
;;; then the lambda is of strategy HEAP.
(defun strategy-analyze-let (node)
  (let ((proc (call-proc node))
        (args (call-args node)))
    (let ((bound-variables (lambda-variables proc))
          (lambda-bound-vars '()))
      ;; Get all the variables which are bound to lambdas
      (do ((values args (cdr values))
           (vars bound-variables (cdr vars)))
          ((or (null values) (null vars)))
        (when (lambda-node? (car values))
          (push (car vars) lambda-bound-vars)))
      ;; Call proc is like a continuation
      (setf (lambda-strategy proc) STRATEGY/OPEN)
      (multiple-value-bind (body-used body-called)
          (strategy-analyze-lambda-body proc)
         (let ((strategy
                 (if (intersection lambda-bound-vars body-used)
                     STRATEGY/HEAP
                     STRATEGY/LABEL))
               (used (set-difference body-used
                                     (lambda-rest+variables proc)))
               (called (set-difference body-called
                                       (lambda-rest+variables proc))))
           (do ((values args (cdr values))
                (vars bound-variables (cdr vars)))
               ((null values))
             (let ((value (car values))
                   (var (car vars)))
               (cond ((lambda-node? value)
                      (setf (lambda-strategy value) strategy)
                      (setf (variable-type var) value)
                      (multiple-value-bind (lambda-used lambda-called)
                          (strategy-analyze-lambda value)
                        (when (eq strategy STRATEGY/HEAP)
                          (set-closed lambda-used)
                          (set-closed lambda-called))
                        (setq used (union lambda-used used)
                              called (union lambda-called called))))
                     ((reference-node? value)
                      (setq used
                            (adjoin (reference-variable value) used))))))
         (values used called))))))

(defun strategy-analyze-y (node)
  (let ((all-args            (call-args (lambda-body node)))
        (all-bound-variables (lambda-variables node)))
    (let ((body (first all-args))
          (labels (rest  all-args))
          (argvars (rest all-bound-variables)))
      ;; Body is like a continuation.
      (setf (lambda-strategy body) STRATEGY/OPEN)
      (multiple-value-bind (used called)
          (strategy-analyze-lambda body)
        (dolist (label labels)
          (multiple-value-bind (label-used label-called)
              (strategy-analyze-lambda label)
            (setq used (union label-used used)
                  called (union label-called called))))
        (let ((used-vars (set-difference used all-bound-variables))
              (called-vars (set-difference called all-bound-variables))
              (strategy
                (if (intersection argvars used)
                    STRATEGY/HEAP
                    STRATEGY/LABEL)))
            (setf (lambda-strategy node) strategy)
            (when (eq strategy STRATEGY/HEAP)
              (set-closed used-vars)
              (set-closed called-vars))
            (do ((vars argvars (cdr vars))
                 (lambdas labels (cdr lambdas)))
                ((null vars))
              (set-label-strategy (car vars) (car lambdas) strategy))
            (setf (lambda-used node) used-vars)
            (values used-vars called-vars))))))


(defun set-closed (variables)
  (dolist (var variables)
    (when (variable-binder var)
      (setf (variable-closed var) t))))

(defun set-label-strategy (var lambda strategy)
  (ecase strategy
    (STRATEGY/HEAP (setf (lambda-strategy lambda) strategy))
    (STRATEGY/LABEL
     (setf (lambda-strategy lambda)
           (if (truly-label-p var lambda)
               STRATEGY/LABEL
             STRATEGY/HEAP)) ;PROC))
     (setf (variable-type var) lambda))))

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
                     (t (bug "bad cont from known: ~S" known)))))
            (t (bug "bad cont: ~s" cont))))
    continuations))

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

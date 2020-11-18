;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;; Simplifying Y - much like simplifying LET.  Needs to loop.
;;;  1) Move as much stuff out of the body of the Y-LAMBDA as possible.
;;;     This will be done by CSE.  Doesn't loop yet since this part isn't done.
;;;  2) Substitute for any labels values that do not need recursion.
;;;  3) If any changes, resimplify Y-LAMBDA and go to 1).
;;;  4) Remove if no label variables are left.

(defun simplify-y (call-node)
  (simplify-call (call-arg-n 1 call-node))
  (let* ((y-lambda (call-arg-n 1 call-node))
         (value-call (lambda-body y-lambda)))
;           (node-parent (car (variable-refs (lambda-cont-var y-lambda))))
    (let* ((removed (remove-loop-values y-lambda
                                        value-call
                                        #'(lambda (n) (loop-value? n y-lambda))))
           (empty? (null (cdr (lambda-variables y-lambda)))))
      (if removed
          (insert-label-lets removed (node-parent call-node)))
      (cond (empty?
             (replace-node call-node (detach (lambda-body y-lambda)))
             (replace-node value-call
                           (detach (lambda-body (car (call-args value-call)))))))
      (or removed empty?))))

(defun remove-loop-values (l-node call pred)
  (do ((vars (lambda-variables l-node))
       (args (call-args call))
       (removed '())
       (n 3))
      ((null (cdr vars))
       removed)
    (cond
      ((not (used? (cadr vars)))
       (setf (cdr vars) (cddr vars)) ; Evil
       (erase-all (cadr args))
       (setf (cdr args) (cddr args))) ; Extremely Evil
      ((not (funcall pred (cadr args)))
       (setf (node-role (cadr args)) (call-arg (1- n)))
       (let ((r (cons (cadr vars) (detach (cadr args)))))
         (setf (cdr vars) (cddr vars))          ; Evil
         (setf (cdr args) (cddr args))          ; Extremely Evil
         (push r removed)))
      (t
       (setf (variable-number (cadr vars)) n)
       (setf (node-role (cadr args)) (call-arg (1- n)))
       (pop vars)
       (pop args)
       (incf n)))))

(defun loop-value? (thunk y-lambda)
  (let ((node (thunk-value thunk)))
    (or (not node)
        (lambda-node? node)       ;;; Could check live vars...
;       (object-node? node)
        (and (reference-node? node)
             (bound-below? node y-lambda)))))

(defun bound-below? (ref top)  ; NIL if REF is bound by TOP
  (let ((binder (variable-binder (reference-variable ref))))
    (if (not binder)
        nil
        (do ((node ref (node-parent (node-parent node))))
            ((eq node top) nil)
          (if (eq node binder) (return t))))))

#|||
(define (insert-label-lets vars-and-thunks parent)
  (receive (simple complex)
           (partition-list (lambda (vt)
                             (node? (thunk-value (cdr vt))))
                           vars-and-thunks)
    (cond (simple
           (insert-let (map car simple)
                       (map (lambda (vt)
                              (thunk-value (cdr vt)))
                            simple)
                       parent)
           (walk (lambda (vt)
                   (splice-thunk (cdr vt) parent))
                 simple)))
    (walk (lambda (vt)
            (var-gets-thunk-value (car vt) (cdr vt) parent))
          complex)))
|||#

#|||
(defun var-gets-thunk-value (var thunk parent)
  (let ((new-call (create-call-node 2 1))
        (cont (create-lambda-node 'p (list nil var))))
    (relate call-proc new-call thunk)
    (relate (call-arg 1) new-call cont)
    (move (lambda-body parent)
          #'(lambda (call)
              (relate lambda-body cont call)
              new-call))))
|||#

#|||
(define (splice-thunk thunk parent)
  (move (lambda-body parent)
        (lambda (old-body)
          (replace (node-parent (car (variable-refs (lambda-cont-var thunk))))
                   old-body)
          (detach (lambda-body thunk))))
  (erase-all thunk))

(define (partition-list pred l)
  (iterate loop ((l l) (yes '()) (no '()))
    (cond ((null? l)
           (values (reverse! yes) (reverse! no)))
          ((pred (car l))
           (loop (cdr l) (cons (car l) yes) no))
          (else
           (loop (cdr l) yes (cons (car l) no))))))

|||#

;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;; Parameterizing procedures so that they can be easily copied
;;; (LET ((F (LAMBDA <vars>
;;;            <stuff>
;;;              (FOO <exit1> <exit2> ... <arg1> <arg2> ...))))
;;;    <body>)
;;;  If any of the <args> are LAMBDAs they cannot reference any of <vars>
;;;  or they must be small enough to be duplicated.
;;;  =>
;;; <stuff>
;;;   (LET ((E1 (LAMBDA <vars> <exit1>))  ; These LAMBDAs are only needed if
;;;         (E2 (LAMBDA <vars> <exit2>))  ; the exits reference <vars>.
;;;         ...
;;;         (A1 <arg1>)  ; These go here only if they are LAMBDAs that do not
;;;         (A2 <arg2>)  ; reference <vars>
;;;         ...)
;;;     (LET ((F (LAMBDA <vars>
;;;                (FOO (LAMBDA <e-vars1>
;;;                       (E1 <vars>))
;;;                     (LAMBDA <e-vars2>
;;;                       (E2 <vars>))
;;;                     ...
;;;                     A1
;;;                     A2 ...))))
;;;       <body>))


(defun parameterize (l-node call)
  (if (can-parameterize? l-node call)
      (really-parameterize l-node call)))

(defun can-parameterize? (l-node call)
  (do ((top (node-parent call) (node-parent (node-parent top)))
       (lambdas (remove-if-not #'lambda-node? (call-non-exit-args call)) ;?
                (nconc (remove-if-not #'lambda-node? (remove top (call-args (node-parent top))))
                                                    ;(call-non-exit-args call))
                       lambdas)))
      ((eq top l-node)
       (dolist (l lambdas)
         (setf (node-flag l) t))
       (mark-reference-parents l-node)
       (every #'node-flag lambdas))
    (unless (hoistable-call? (node-parent top) top)
      (return nil))))

(defun mark-reference-parents (l-node)
  (dolist (var (lambda-rest+variables l-node))
    (cond ((variable-p var)
           (dolist (ref (variable-refs var))
             (do ((n ref (node-parent (node-parent n))))
                 ((eq n l-node))
               (setf (node-flag n) nil)))))))

(defun hoistable-call? (call from)
  (let ((proc (call-proc call)))
    (cond ((lambda-node? proc)
           (and (eq proc from)
                (not (some #'global-reference? (call-args call)))))
          ((primop-node? proc)
           (and (= 1 (call-exits call))
                (eq from (car (call-args call)))
                (not (primop.side-effects? (primop-value proc)))
                (not (some #'global-reference? (call-args call)))))
          (t nil))))

(defun global-reference? (node)
  (and (reference-node? node)
       (not (variable-binder (reference-variable node)))))


(defun really-parameterize (l-node call)
;  (breakpoint `(really-parameterize0 ,l-node ,call))
  (if (not (eq (node-parent call) l-node))
      (move-block l-node call))
  (multiple-value-bind (vars vals)
      (parameterize-args call (lambda-rest+variables l-node))
    (if (not (null vars))
        (insert-let vars vals (node-parent (node-parent l-node))))
;    (breakpoint `(really-parameterize1 ,l-node ,call))
    t))


;;; Move the code between L-NODE and CALL to above L-NODE

(defun move-block (l-node call)
  (let ((new-top-call (detach (lambda-body l-node))))
    (move (node-parent l-node)
          #'(lambda (old-top-call)
              (move call
                    #'(lambda (call)
                        (relate lambda-body l-node call)
                        old-top-call))
              new-top-call))))

(defun parameterize-args (call scope-vars)
  (let ((exits (call-exits call))
        (vars '()) (vals '()))
    (dolist (arg (call-args call))
      (cond
        ((leaf-node? arg)
         (decf exits))
        ((< 0 exits)
         (let ((role (node-role arg)))
           (multiple-value-bind (new var val)
               (parameterize-exit (detach arg) scope-vars)
             (relate role call new)
             (mark-changed new)                 ; could just mark new as simplified...
             (decf exits)
             (push var vars)                    ;cons-from-freelist
             (push val vals))))
        (t
         (let ((let-var (create-variable 'c)))
           (move arg
                 #'(lambda (ignore)
                     (create-reference-node let-var)))
           (setq exits 0)
           (push let-var vars)
           (push arg vals)))))
    (values (nreverse vars) (nreverse vals))))

(defun insert-let (vars vals parent)
  (let ((new-proc (create-lambda-node 'l (cons nil vars))) ;cons-from-freelist
        (new-call (create-call-node (+ 1 (length vals)) 0)))
    (relate-call-args-list new-call vals)
    (relate call-proc new-call new-proc)
    (move (lambda-body parent)
          #'(lambda (call)
              (relate lambda-body new-proc call)
              new-call))))

;;; (LAMBDA () <cont>) + <vars>
;;; => <let-value>                   <let-var>     <new-cont>
;;;    (LAMBDA () <cont>)               v1          <v1>
;;;    (LAMBDA (vs) <cont>[vs/<vars>])  c1          (LAMBDA () (<c1> <<vars>>))

(defun parameterize-exit (node vars)
  (multiple-value-bind (new-vars old-vars)
      (replace-needed-vars node vars)
    (cond ((null new-vars)
           (let ((let-var (create-variable 'c)))
             (values (create-reference-node let-var) let-var node)))
          (t
           (let ((let-var (create-variable 'c))
                 (new-cont (create-lambda-node 'p '()))
                 (cont-call (create-call-node (+ 1 (length new-vars)) 0))
                 (let-value (create-lambda-node 'p `(() . ,new-vars))))
             (relate call-proc cont-call (create-reference-node let-var))
             (relate-call-args-list cont-call (mapcar #'create-reference-node old-vars))
             (relate lambda-body new-cont cont-call)
             (relate lambda-body let-value (detach (lambda-body node)))
             (erase-all node)
             (values new-cont let-var let-value))))))

(defun replace-needed-vars (node vars)
  (let ((new-vars (mapcar #'(lambda (var) (if (used? var) (create-variable 'w) nil))
                       vars)))
    (substitute-vars-in-node-tree node vars new-vars)
    (let ((l (delete-if-not #'used?
                      (mapcar #'cons new-vars vars)
                       :key #'car)))
      (values (mapcar #'car l)                  ;free-map
              (mapcar #'cdr l)))))

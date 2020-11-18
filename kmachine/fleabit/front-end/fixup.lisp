;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;; (lambda (k) <body>) => (lambda (v) ((lambda (k) <body>) v))

(defun fixup-node-tree (top-node)
  (fixup-call-node (lambda-body top-node))
  (setf (node-parent top-node) nil)
  top-node)

(defun fixup-value-node (node)
  (cond ((lambda-node? node)
         (fixup-call-node (lambda-body node)))
;        ((object-node? node)
;         (fixup-value-node (object-proc node))
;         (mapc #'fixup-value-node (object-operations node))
;         (mapc #'fixup-value-node (object-methods node))
;         (fix-object-node node))
        ))

(defun fixup-call-node (node)
  (mapc #'fixup-value-node (call-proc+args node))
  (let ((proc (call-proc node)))
    (cond
;;; what is this for?
;;; removed 3/25/87 because
;;; it was thunking continuation vars to label calls
;;; which caused the label calls to look like they had
;;; different continuations so the label go STRATEGY/PROC
;          ((lambda-node? proc)
;           (mapc #'(lambda (var val)
;                    (if (lambda-node? val)
;                        (check-continuation-var var val)))
;                 (lambda-variables proc)
;                 (call-args node)))
;          ((primop-ref? proc primop/undefined-effect)
;           (fixup-undefined-effect node))
          ((primop-ref? proc primop/y)
           (fixup-y node)))))

#||||

(defun check-continuation-var (var val)
  (cond ((and (lambda-rest-var val)
              (not (null (variable-refs (lambda-rest-var val)))))
         nil)
        ((some #'(lambda (ref)
                   (eq (node-role ref) call-proc))
               (variable-refs var))
         (cerror "resume" "proc refs")
         (walk-refs-safely #'(lambda (ref)
                               (if (call-exit? ref)
                                   (fix-exit-reference var ref val)))
                           var))
        ((/= 2 (variable-number var))
         (cerror "resume" "/= 2 variable-number")
         (walk-refs-safely #'(lambda (ref)
                               (if (and (call-exit? ref)
                                        (not (primop-node?
                                               (call-proc (node-parent ref)))))
                                   (fix-exit-reference var ref val)))
                           var))))

(defun fix-exit-reference (var node value)
  (if (not (eq call-proc (node-role node)))
      (real-fix-exit-reference var node value)))

(defun real-fix-exit-reference (var node value)
  (let* ((new-vars (mapcar #'(lambda (var)
                               (if var
                                   (create-variable (variable-name var))))
                             (lambda-rest+variables value)))
         (cont (create-lambda-node 'c new-vars))
         (call (create-call-node (length new-vars) 0)))
    (relate call-proc call (create-reference-node var))
    (relate-call-args-list call (mapcar #'(lambda (var)
                                            (if var
                                                (create-reference-node var)
                                              (create-literal-node nil)))       ;'#f
                                        (cdr new-vars)))
    (relate lambda-body cont call)
    (replace-node node cont)))


;;; Remove any continuation of UNDEFINED-EFFECT

(defun fixup-undefined-effect (node)
  (cond ((= (call-exits node) 1)
         (setf (call-exits node) 0)
         (erase-all (detach (call-arg-n 1 node)))
         (relate-new-call-args node (mapcar #'detach (cdr (call-args node))))
         t)
        ((and (= 1 (length (call-args node)))
              (literal-node? (call-arg-n 1 node))
              (consp (literal-value (call-arg-n 1 node))))
         (pop (literal-value (call-arg-n 1 node))))))

||||#

;;; Fixing up a call to PRIMOP/Y so that all values are dethunked lambdas.

(defun fixup-y (node)
  (let* ((y-lambda (call-arg-n 1 node))
         (vars (lambda-variables y-lambda))
         (value-call (node-parent (car (variable-refs (car vars)))))
         (removed (remove-loop-values y-lambda value-call #'simple-thunk?)))
    (if removed
        (introduce-labels-cells node value-call removed))
    (cond ((null (cdr (lambda-variables y-lambda)))
           (replace-node node (detach (lambda-body y-lambda)))
           (replace-node value-call
                    (detach (lambda-body (car (call-args value-call))))))
          (t
           (mapc #'(lambda (thunk)
                     (replace-node thunk (detach (thunk-value thunk))))
                 (cdr (call-args value-call)))))))

(defun introduce-labels-cells (node value-call removed)
  (let ((body-lambda (call-arg-n 1 value-call))
        (parent (node-parent node)))
    (dolist (r removed)
      (let* ((var (car r))
             (new-var (create-variable (variable-name var))))
        (walk-refs-safely #'(lambda (ref)
                              (hack-reference ref new-var))
                          var)
        (add-label-cell new-var parent)
        (add-label-assigner new-var (cdr r) body-lambda)))))

(defun add-label-assigner (var thunk parent)
  (let ((value (thunk-value thunk)))
    (cond (value
           (add-simple-label-assigner var (detach value) parent)
           (splice-thunk thunk parent))
          (t
           (let* ((c-var (create-variable 'k))
                  (value (create-reference-node c-var)))
             (add-simple-label-assigner var value parent)
             (var-gets-thunk-value var thunk parent))))))

(defun add-simple-label-assigner (var value parent)
  (let ((call (create-call-node 5 1))
        (cont (create-lambda-node 'c (list (create-variable 'ignore)))))
    (relate call-proc call (create-primop-node primop/set-location))
    (relate-call-args call
                      cont
                      (create-primop-node primop/cell-value)
                      value
                      (create-reference-node var))
    (insert-call call cont parent)))

(defun add-label-cell (var parent)
  (let ((call (create-call-node 3 1))
        (cont (create-lambda-node 'c (list nil var))))
    (Relate call-proc call (create-primop-node primop/make-cell))
    (relate-call-args call cont (create-literal-node 'uninitialized-labels))
    (insert-call call cont parent)))

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;;; (object <proc> (<op1> ... <opN>) (<meth1> ... <methN>))
;;; =>
;;; (lambda (V1)
;;;   (primop/proc+handler V1
;;;                        <proc>
;;;                        (lambda (V2)
;;;                          (V2 <op1> ... <opN>))
;;;                        <meth1>
;;;                        ...
;;;                        <methN>)))

(defun fix-object-node (node)
  (let* ((ops (object-operations node))
         (meths (object-methods node))
         (obj-lambda-cont (create-variable 'v))
         (obj-lambda (create-lambda-node 'c (list nil obj-lambda-cont)))
         (obj-call (create-call-node (+ 4 (length meths)) 1))
         (ops-lambda-cont (create-variable 'v))
         (ops-lambda (create-lambda-node 'c (list nil ops-lambda-cont)))
         (ops-call (create-call-node (+ 1 (length ops)) 0)))
    (relate lambda-body obj-lambda obj-call)
    (relate call-proc obj-call (create-primop-node primop/proc+handler))
    (relate-call-args obj-call
                      `(,(create-reference-node obj-lambda-cont)
                        ,(detach (object-proc node))
                        ,ops-lambda
                        . ,(mapcar #'detach meths)))
    (relate lambda-body ops-lambda ops-call)
    (relate call-proc ops-call (create-reference-node ops-lambda-cont))
    (relate-call-args ops-call (mapcar #'detach ops))
    (replace-node node obj-lambda)))

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

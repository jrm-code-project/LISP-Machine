;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-


;;; Any used variables in a STRATEGY/HEAP lambda must be closed over.
;;; If they are also used in an enclosing HEAP lambda then they are
;;; already in the environment



(defun env-analyze-top (top-node)
  (env-analyze-lambda (call-arg-n 1 (lambda-body top-node))
                      '())
  (debug :env
    (format t "~%Env Analysis:")
    (pp-cps top-node :extra #'lambda-env)))

;;; do something about Y nodes

(defun env-analyze-lambda (node env)
  (case (lambda-strategy node)
    ((STRATEGY/OPEN STRATEGY/LABEL))
    ((STRATEGY/HEAP STRATEGY/PROC)
     (when (lambda-used node)
       ;(add-env-variable node) ;???
       ;; compute NEED-CLOSING, which is the set of variables
       ;; which need to be closed and are not in any higher contour
       (let ((need-closing (remove-if-not #'(lambda (var)
                                              (and (eq 'T (variable-closed var))
                                                   (not (already-in-env-p var env))))
                                          (lambda-used node))))
         (when need-closing
           (push need-closing env))))))
  (let ((closed-setqed (remove-if-not #'(lambda (var)
                                          (and var
                                               (variable-closed var)
                                               (variable-setqs var)))
                                      (lambda-variables node))))
    (setf (lambda-env node) (cons closed-setqed env))
    (when closed-setqed
      (push closed-setqed env))
  (env-analyze-call (lambda-body node) env)))

(defun env-analyze-call (node env)
  (dolist (arg (call-proc+args node))
    (if (lambda-node? arg)
        (env-analyze-lambda arg env))))


(defun already-in-env-p (var env)
  (some #'(lambda (frame)
            (member var frame :test #'eq))
        env))

;(defun add-env-variable (lambda)
;  (push (create-variable 'env)
;       (cdr (lambda-variables lambda)))
;  (do ((vars (lambda-all-variables lambda) (cdr vars))
;       (n 0 (1+ n)))
;       ((null vars))
;    (if (car vars)
;       (setf (variable-number (car vars))
;             n))))

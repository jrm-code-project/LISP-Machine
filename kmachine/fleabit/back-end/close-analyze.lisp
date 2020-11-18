;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-


;;; Any used variables in a STRATEGY/HEAP lambda must be closed over.
;;; If they are also used in an enclosing HEAP lambda then they are
;;; already in the environment


(zl:defsubst lambda-contour (node)
  (lambda-env node))

(defun close-analyze-top (top-node)
  (close-analyze-lambda (call-arg-n 1 (lambda-body top-node)) 0))

;;; do something about Y nodes

(defun close-analyze-lambda (node contour-num)
  (case (lambda-strategy node)
    ((STRATEGY/OPEN STRATEGY/LABEL))
    ((STRATEGY/HEAP STRATEGY/PROC)
     (when (lambda-used node)
       (add-env-variable node) ;???
       ;; compute NEED-CLOSING, which is the set of variables
       ;; which need to be closed and are not in any higher contour
       (let ((need-closing (remove-if-not #'(lambda (var)
                                              (eq 'T (variable-closed var)))
                                          (lambda-used node))))
         (when need-closing
           (incf contour-num)
           (let ((offset 0))
             (dolist (var need-closing)
               (setf (variable-closed var)
                     (cons contour-num (incf offset))))))))))
  (let ((closed-setqed (remove-if-not #'(lambda (var)
                                          (and var
                                               (variable-closed var)
                                               (variable-setqs var)))
                                      (lambda-variables node))))
    (when closed-setqed
      (incf contour-num)
      (let ((offset 0))
        (dolist (var closed-setqed)
          (setf (variable-closed var)
                (cons contour-num (incf offset)))))))

  (setf (lambda-contour node) contour-num)
  (close-analyze-call (lambda-body node) contour-num))

(defun close-analyze-call (node contour-num)
  (dolist (arg (call-proc+args node))
    (if (lambda-node? arg)
        (close-analyze-lambda arg contour-num))))


(defun add-env-variable (lambda)
  (push (create-variable 'env)
        (cdr (lambda-variables lambda)))
  (do ((vars (lambda-all-variables lambda) (cdr vars))
       (n 0 (1+ n)))
       ((null vars))
    (if (car vars)
        (setf (variable-number (car vars))
              n))))

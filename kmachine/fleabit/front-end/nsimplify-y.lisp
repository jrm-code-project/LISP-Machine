;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Simplify calls to PRIMOP/Y

(defun simplify-y (call-node)
  (simplify-call (call-arg-n 1 call-node))
  (let* ((y-lambda (call-arg-n 1 call-node))
         (value-call (lambda-body y-lambda))
         (changed-p nil))
    (unless (primop-ref? (call-proc (lambda-body (call-arg-n 1 value-call)))
                         primop/optional-setup)
    (do ((vars (lambda-variables y-lambda))
         (args (call-args value-call))
         (n 3))
        ((null (cdr vars)))
      (let ((refs (variable-refs (cadr vars))))
      (cond
        ;; ------------------------------
        ;; If a label proc is not called,
        ;; remove it
        ;; ------------------------------
        ((null refs)
         (debug-msg :simp "~&Removing unused label proc: ~a"
                    (lambda-name (cadr args)))
         (setf (cdr vars) (cddr vars))          ; Evil
         (erase-all (cadr args))
         (setf (cdr args) (cddr args)))         ; Extremely Evil
        ;; ------------------------------------
        ;; If a label proc is called only once,
        ;; substitute it in.
        ;; ------------------------------------
        ((and (null (cdr refs))
              ;; we sorta depend on go's to have variable tags
              ;; particularly for dynamic state stuff
              (not (primop-ref? (call-proc (node-parent (car refs)))
                                primop/%go)))
         (debug-msg :simp "~&Substituting once called label proc: ~a"
                    (lambda-name (cadr args)))
         (replace-node (car refs)
                       (cadr args))
         (setf (cdr vars) (cddr vars))          ; Evil
         (setf (cdr args) (cddr args))          ; Extremely Evil
         (debug :simp-tree
           (pp-cps (node-base call-node)))
         (setq changed-p t))
        (t
         (setf (variable-number (cadr vars)) n)
         (setf (node-role (cadr args)) (call-arg (1- n)))
         (pop vars) (pop args) (incf n))))))
    (when (null (cdr (lambda-variables y-lambda)))
      (debug-msg :simp "~&Removing empty Y call: ~a"
                 (pp-cps-2 call-node))
      (replace-node call-node (detach (lambda-body y-lambda)))
      (replace-node value-call
                    (detach (lambda-body (car (call-args value-call)))))
      (setq changed-p t))
    changed-p))

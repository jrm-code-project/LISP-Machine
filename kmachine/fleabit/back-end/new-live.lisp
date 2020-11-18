;;; -*- Mode:LISP; Package:NC ; Base:10; Readtable:CL -*-

;;;; Live Variable Analysis

;;; This seems to be about as random a place to put this.

(defun analyze (top-node)
  (trace-analyze-top    top-node)
  (strategy-analyze-top top-node)
  (live-analyze-top     top-node)
  (close-analyze-top    top-node))

(defun live-analyze-top (node)
  (setq *unit-literals* '())
  (setq *unit-variables*
        (live-analyze-node (call-arg-n 1 (lambda-body node)))))

(defun live-analyze-node (node)
  (cond ((lambda-node? node) (live-analyze-lambda node))
        ((leaf-node?   node) (live-analyze-leaf   node))
        (t (bug "live-analyze-node called on a call-node ~s" node))))

(defun live-analyze-leaf (node)
  (flet ((add-literal ()
             (setq *unit-literals* (adjoin (leaf-value node) *unit-literals*))
             '()))
    (cond ((literal-node? node) (add-literal))
          ((primop-node? node)
           (if (foreign-name (primop-value node))
               (add-literal)
               '()))
          (t (list (leaf-value node))))))

(defun live-analyze-lambda (node)
  (let ((vars (live-analyze-call (lambda-body node))))
    (setf (lambda-live node)
          (set-difference vars (lambda-all-variables node)))))

(defun live-analyze-call (node)
  (let ((proc (call-proc node)))
    (if (primop-node? proc)
        (cond ((eq (primop-value proc) primop/y)
               (live-analyze-y (call-arg-n 1 node)))
              ((eq (primop-value proc) primop/setq-lexical)
               (delete (reference-variable (call-arg-n 2 node))
                       (live-analyze-call-args node)))
              (t (live-analyze-call-args node)))
      (live-analyze-call-args node))))

(defun live-analyze-call-args (node)
  (do ((args (call-proc+args node) (rest args))
       (live '() (union live (live-analyze-node (first args)))))
      ((null args) live)))

(defun live-analyze-y (lambda-node)
  (do ((args (call-args (lambda-body lambda-node)) (rest args))
       (live '() (union live (live-analyze-lambda (first args)))))
      ((null args)
       (let ((live (set-difference live (lambda-all-variables lambda-node))))
         (do ((args (call-args (lambda-body lambda-node)) (rest args)))
             ((null args) live)
           (setf (lambda-live (first args)) live))))))

;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-


;;; Front End

(defun make-code-tree (exp env)
  (nrebuild (do-exp exp env)))

(defun nrebuild (node)
  (setf (node-parent node) empty)
  (let ((top (value-node->thunk node)))
    (setf (node-parent top) nil)
    top))

(defun value-node->thunk (node)
  (let* ((c-var (create-variable 'k))
         (new-l (create-lambda-node 'b (list nil c-var)))
         (call (create-call-node 2 0)))
    (relate call-proc call (create-reference-node c-var))
    (relate (call-arg 1) call node)
    (relate lambda-body new-l call)
    new-l))

(defvar *free-variables*)                       ;used in node
(defvar *value-table*)                          ;used in simplify,simplify-call

(defun do-exp (exp env)
  (let ((*variable-id* 0)
        (*value-table* (make-table '*value-table*)))
    (let ((exp (alpha exp env)))
      (debug :alpha
        (format t "~&Alphatized:~%")
        (pprint exp))
      (let ((*free-variables* (make-table '*free-variables*)))
;       (fixup-node-tree
           (transmogrify-exp exp env)))))

(defun transmogrify-exp (exp env)
  (let ((node (->value-node exp)))
    (debug :unsimplified
      (format t "~&Unsimplified node:~%")
      (pp-cps node))
    (simplify-call node)
    (debug :simplified
      (format t "~&Simplified tree:~%")
      (pp-cps node))
    node))

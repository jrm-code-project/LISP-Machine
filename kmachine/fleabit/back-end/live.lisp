;;; -*- Mode:LISP; Package:NC ; Base:10; Readtable:CL -*-


;;;; Live Variable Analysis
;;;
;;;    Live variable analysis in a tree would be a simple matter of
;;; finding the live variables below a node, then removing those
;;; variables bound at the node.  However, the control structure of code
;;; trees is tangled by function calls (also meaning goto's and
;;; continuations) back up in the tree.
;;; For example in:
;;;
;;; (defun live-y (x)
;;;   (labels ((f (y)
;;;              (g y)
;;;              (print y))
;;;            (g (z)
;;;              (print z)))
;;;      (f x)))
;;;
;;; y is live in g. (Therefore z cannot share storage with y. This could
;;; happen because g wants to be substituted inline into f, in fact
;;; if the (print y) were not there it would be a good idea.) The
;;; continuation to g is known to be ((B_3 IGNORE_2) (PRINT K_0 Y_1))
;;; therefore the variable y is live in g by virtue of g referring to
;;; its continuation. What we get is a graph with a definite root and
;;; leaves but with cross links and cycles.
;;;   The algorithm for finding live variables is to make multiple passes
;;; over the graph, proceeding in a depth first manner.  Each node will be
;;; marked with the pass number. At each node we check the pass number to
;;; see if we have looped. If so, we return the nodes current list of live variables.
;;; If not, we find the live variables of the children of the node and
;;; construct the live variables for this node.  If this list is different
;;; we update the node and set a flag saying there were changes on this pass.
;;; We make passes over the graph until there are no changes.
;;;   This algorithm calculates the live variables for a node once for
;;; each pass (therefore each pass is O(n) n = number of nodes), and
;;; makes a number of passes equal to 2 + the length of the longest chain up.


(defvar *changes* nil)
(defvar *pass* 0)

(defun live-analyze-top (node)
  (do ((*changes* nil nil)
       (*pass* 0 (1+ *pass*)))
      (())
    (live-analyze-node (call-arg-n 1 (lambda-body node)))
    (when (null *changes*) (return)))
  (debug :live
    (format t "~%Live Analysis:")
    (pp-cps node :extra #'(lambda (node)
                            (let ((live (lambda-live node)))
                              (if (consp live)
                                  (mapcar #'variable-unique-name
                                          live)
                                live))))))

(defun live-analyze-node (node)
  (cond ((lambda-node? node) (live-analyze-lambda node))
        ((leaf-node?   node) (live-analyze-leaf   node))
        (t (bug "live-analyze-node called on a call-node ~s" node))))

(defun live-analyze-leaf (node)
  (cond ((literal-node? node) '())
        ((primop-node? node) '())
        (t (adjoin (leaf-value node)
                   (let ((cont (variable-known (leaf-value node))))
                     (and cont
                          (live-analyze-lambda cont)))))))

(zl:defsubst lambda-pass (lambda)
  (lambda-db lambda))

(defun lambda-seen-yet-on-this-pass? (lambda)
    (eql (lambda-pass lambda) *pass*))

(defun live-analyze-lambda (node)
    (if (lambda-seen-yet-on-this-pass? node)
        (lambda-live node)
      (progn
        (setf (lambda-pass node) *pass*)
        (let ((vars (live-analyze-call (lambda-body node))))
        (let ((live (set-difference vars (lambda-all-variables node))))
          (if (set-eq (lambda-live node) live)
              live
            (progn
;             (format t "~%**** ~a~&old: ~a~&new: ~a"
;                     (mapcar #'variable-unique-name (lambda-all-variables node))
;                     (mapcar #'variable-unique-name (lambda-live node))
;                     (mapcar #'variable-unique-name live))
              (setq *changes* t)
              (setf (lambda-live node) live))))))))



(defun live-analyze-call (node)
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (cond ((eq (primop-value proc) primop/y)
                  (live-analyze-y (call-arg-n 1 node)))
                 ((eq (primop-value proc) primop/setq-lexical)
                  (delete (reference-variable (call-arg-n 2 node))
                          (live-analyze-call-args node)))
                 (t (live-analyze-call-args node))))
          ((lambda-node? proc)
           (live-analyze-let proc node))
          (t (live-analyze-call-args node)))))


(defun live-analyze-let (proc node)
  (do ((args (call-args node) (rest args))
       (live (live-analyze-lambda proc)
             (union live (unless (lambda-node? (first args))
                           (live-analyze-node (first args))))))
      ((null args) live)))

(defun live-analyze-call-args (node)
  (do ((args (call-proc+args node) (rest args))
       (live '() (union live (live-analyze-node (first args)))))
      ((null args) live)))

;(defun live-analyze-y (lambda-node)
;  (do ((args (call-args (lambda-body lambda-node)) (rest args))
;       (live '() (union live (live-analyze-lambda (first args)))))
;      ((null args)
;       (let ((live (set-difference live (lambda-all-variables lambda-node))))
;        (do ((args (call-args (lambda-body lambda-node)) (rest args)))
;            ((null args) live)
;          (setf (lambda-live (first args)) live))))))

;;; just analyze body
;;; it will analyze label procs that it calls
(defun live-analyze-y (lambda-node)
  (let ((live (set-difference
                (live-analyze-lambda
                  (car (call-args (lambda-body lambda-node))))
                (lambda-all-variables lambda-node))))
    (setf (lambda-live lambda-node) live)))

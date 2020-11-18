;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;;  Structure definitions
;;;  Node creation; interconnect manipulation


;;; VARIABLES
;;;===========================================================================
;;; Structures to represent variables.

(defstruct (variable (:print-function (lambda (struct stream depth)
                                        depth
                                        (format stream "#{Variable ~A_~D}"
                                                (variable-name struct)
                                                (variable-id struct)))))

  name               ; Source code name for variable (for debugging only)
  id                 ; Unique numeric identifier
  (binder  nil)      ; LAMBDA node which binds this variable
  (closed  nil)      ; location in lexical env of this variable
; (support nil)      ; Support information for this variable
  number             ; K: var = (NTH (LAMBDA-ALL-VARIABLES (VARIABLE-BINDER var)) K)
  (refs '())         ; List of leaf nodes n for which (REFERENCE-VARIABLE n) = var.
  (type t)           ; The type of the variable's value at point of binding
  (special-p nil)    ; T if declared special
  (flag nil)         ; Useful slot, used by shapes, COPY-NODE, NODE->VECTOR, etc.
  (flags '())        ; For various annotations, e.g. IGNORABLE
  (loc nil)          ; Where this variable lives (<register> SPECIAL, FUNCTION)
  (setqs '())        ; call nodes that are setqs of this variable
  (optional-p ()))   ; T if an optional arg


(defvar *variable-id* 0)

(defun create-variable (name)
  (prog1 (make-variable :name name
                        :id *variable-id*)
         (incf *variable-id*)))



;;; EMPTY
;;;==========================================================================
;;; EMPTY is used to mark empty parent and child slots in nodes.

(defconstant empty "#{Empty}")

(defconstant *empty* empty) ; compatibility

(zl:defsubst empty? (obj) (eq obj empty))

(defun proclaim-empty (probe)
  (cond ((not (empty? probe))
         (bug "not empty - ~S" probe))))

(defun make-empty-node-list (size)
  (make-list size :initial-element empty))


;;; NODES
;;;============================================================================
;;; There are three node types:
;;;  - LAMBDA
;;;  - CALL
;;;  - LEAF
;;; Calls have a nonzero number of children, lambda nodes have a single child,
;;; leaf node have none.


(defstruct (node (:print-function print-node))
  (parent empty)     ; Parent node
  (role '<new>)      ; node == ((NODE-ROLE node) (NODE-PARENT node))
  (simplified? nil)  ; True if it has already been simplified.
  (flag nil))        ; Used by PARAMETERIZE

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#{NODE ~O}" (object-hash node)))



;;; RELATIONS
;;;=========================================================================
;;; A "relation" is a selector procedure - something appropriate to put in
;;; the ROLE slot of a node.


(defmacro def-relation (name accessor)
  (let ((setter-name (gensym name)))
    `(progn
       (defun ,setter-name (node value)
         (setf (,accessor node) value))
       (defvar ,name (list #',accessor #',setter-name)))))


;;; access the <relation> slot of a node
(defmacro relation (relation node)
  `(funcall (car ,relation) ,node))

;;; change the <relation> slot of a node
(defmacro set-relation (relation node value)
  `(funcall (cadr ,relation) ,node ,value))

(defsetf relation set-relation)


(defmacro make-list-relation (accessor index)
  `(list* #'(lambda (node) (nth ,index (,accessor node)))
          #'(lambda (node value) (setf (nth ,index (,accessor node))
                                       value))
          ,index))

(defmacro def-list-relation (name slot-accessor index)
  (let ((setter-name (gensym name)))
  `(progn
     (defun ,name (node)
       (nth ,index (,slot-accessor node)))
     (defun ,setter-name (node value)
       (setf (nth ,index (,slot-accessor node)) value))
     (defvar ,name (list* #',name #',setter-name ,index)))))


(zl:defsubst list-relation-index (relation)
  (cddr relation))



(defun relate-list (relations start parent list children)
  (do ((i start (1+ i))
       (l list (cdr l))
       (children children (cdr children)))
      ((null children))
    (let ((child (car children)))
      (proclaim-empty (node-parent child))     ; Could be flushed
      (proclaim-empty (car l))                 ; Could be flushed
      (setf (node-role child) (vref relations i))
      (setf (node-parent child) parent)
      (setf (car l) child))))


(defun relate (relation parent child)
  (proclaim-empty (node-parent child))
  (proclaim-empty (relation relation parent))
  (setf (relation relation parent) child)
  (setf (node-parent child) parent)
  (setf (node-role child) relation))


;;; LEAF NODES
;;;=========================================================================
;;;   There are three kinds of leaf nodes - PRIMOP, LITERAL, REFERENCE
;;;
;;; Fields:
;;;   variant  - 'literal, 'primop, or 'reference
;;;   value    - Either a primop, a variable, or a literal value
;;;   type     - the type of the object this refers to (e.g. integer)

(defstruct (leaf-node (:include node)
                      (:conc-name "LEAF-")
                      (:print-function print-leaf-node))
  value
  variant
  (type nil)
  flags)

(zl:defsubst leaf-node? (node)
  (leaf-node-p node))

(defun print-leaf-node (node stream depth)
  depth
  (format stream "#{~S ~S ~O}" (leaf-variant node) (leaf-value node) (object-hash node)))

(defun create-leaf-node (value variant)
  (make-leaf-node :value value :variant variant))


;;; PRIMOP NODES
;;;=========================================================================

(zl:defsubst primop-value (leaf) (leaf-value leaf))

(zl:defsubst create-primop-node (p)
  (create-leaf-node p 'PRIMOP))

(zl:defsubst primop-node? (node)
  (and (leaf-node-p node)
       (eq (leaf-variant node) 'PRIMOP)))

;;; Checks to see if NODE is a reference to one of the primops in PRIMOPS.
(zl:defsubst primop-ref? (node primop)
  (and (primop-node? node)
       (eq (primop-value node) primop)))


;;; LITERAL NODES
;;;=========================================================================

(zl:defsubst literal-value (leaf) (leaf-value leaf))

(zl:defsubst create-literal-node (value)
  (create-leaf-node value 'LITERAL))

(zl:defsubst literal-node? (node)
  (and (leaf-node-p node)
       (eq (leaf-variant node) 'LITERAL)))


;;; REFERENCE NODES
;;;=========================================================================

(zl:defsubst reference-variable (ref) (leaf-value ref))

(zl:defsubst reference-flags (ref) (leaf-flags ref))

(defun create-reference-node (variable)
  (let ((node (create-leaf-node variable 'REFERENCE)))
    (push node (variable-refs variable))
    node))

 (zl:defsubst reference-node? (node)
  (and (leaf-node-p node)
       (eq (leaf-variant node) 'REFERENCE)))

(zl:defsubst variable-ref? (node &rest variables)
  (and (reference-node? node)
       (member (reference-variable node) variables)))


;;; LAMBDA NODES
;;;============================================================================

(defstruct (lambda-node (:include node)
                        (:conc-name "LAMBDA-")
                        (:print-function print-lambda-node))
     (body empty)       ;the node for the body (after CPS, always a call node)
     all-variables      ;list of variables which are bound by this lambda.  The first
                        ;variable gets bound to the procedure itself; the second is a 'rest'
                        ;variable, if it is non-null the lambda is n-ary.
     (env nil)          ;list of variables live on entry to this lambda
     (strategy nil)     ;OPEN, LABEL, PROC, or HEAP
     (live '())         ;the variables live in the body of the lambda.
     db                 ;trace information.
     generated-p        ;T if this lambda has been generated
     dynamic-state      ;dynamic state on entry to this lambda
     used)              ;variables used (different from live)

(defun print-lambda-node (node stream depth)
  (declare (ignore depth))
  (format stream "#{LAMBDA-NODE ~A ~O}" (lambda-name node) (object-hash node)))

(zl:defsubst lambda-node? (x) (lambda-node-p x))

(def-relation lambda-body lambda-body)

;;; Selecting various subsets of a lambda's variables.

(zl:defsubst lambda-variables (node)
  (cddr (lambda-all-variables node)))

(zl:defsubst lambda-rest+variables (node)
  (cdr (lambda-all-variables node)))

(zl:defsubst lambda-self-var (node)
  (car (lambda-all-variables node)))

(zl:defsubst lambda-rest-var (node)
  (cadr (lambda-all-variables node)))

(zl:defsubst lambda-cont-var (node)
  (caddr (lambda-all-variables node)))

;;;    Creates a lambda node.  NAME is used as the name of the lambda node's
;;; self variable.   VARS is a list of variables.  The VARIABLE-BINDER and
;;; VARIABLE-NUMBER slots of the variables are set.

(defun create-lambda-node (name vars)
  (let* ((vars (cons (create-variable name) vars))
         (node (make-lambda-node :all-variables
                                 vars)))
    (do ((vars vars (cdr vars))
         (n 0 (1+ n)))
        ((null vars))
      (let ((var (car vars)))
        (when var
          (setf (variable-binder var) node)
          (setf (variable-number var) n))))
    node))

;;; CALL NODES
;;;==========================================================================

(defstruct (call-node (:include node)
                      (:conc-name "CALL-")
                      (:print-function print-call-node))
   proc+args           ;list of child nodes
   exits               ;the number of initial arguments that are continuations
;  complexity          ;no longer used...
;  hoisted-cont        ;continuation to be consed on stack
   (open nil))         ;the call to primop/open-frame for this call


(defun print-call-node (node stream depth)
  (declare (ignore depth))
  (format stream "#{CALL-NODE ~A ~O}" (pp-cps-2 (call-proc node)) (object-hash node)))

(zl:defsubst call-node? (x) (call-node-p x))

;;; Selecting various subsets of the children of a call node.

(zl:defsubst call-args (node)
  (cdr (call-proc+args node)))

(zl:defsubst call-arg-n (n node)
  (nth n (call-proc+args node)))

(zl:defsubst call-exit-args (node)
  (subseq (call-args node) 0 (call-exits node)))

(zl:defsubst call-non-exit-args (node)
  (nthcdr (call-exits node) (call-args node)))

;; Create a call node with N children and EXITS exits.
;; Add new argument relations if there aren't enough.
(defun create-call-node (n exits)
  (make-call-node :proc+args
                  (make-empty-node-list n)
                  :exits
                  exits))

;;; ARGUMENT RELATIONS
;;;========================================================================
;;; Argument relations are created on demand whenever a newly created
;;; call node is going to have more arguments than any previous call node
;;; has had.


(defun make-arg-relation (i)
  (make-list-relation call-proc+args i))

(zl:defsubst call-arg-number (relation)
  (list-relation-index relation))

(def-list-relation call-proc call-proc+args 0)

(defvar call-arg-relations-array
             (make-infinite-vector 10 'make-arg-relation 'call-arg-relations))

(setf (vref call-arg-relations-array 0) call-proc)
(defun call-arg-relations (n)
  (vref call-arg-relations-array n))

(zl:defsubst call-arg (i)
  (call-arg-relations i))

(zl:defsubst call-arg? (relation)
  (or (eq relation call-proc)
      (and (list-relation-index relation)
           (eq relation (call-arg (list-relation-index relation))))))

;;;  T if NODE is an exit of a call node, NIL otherwise.
(defun call-exit? (node)
  (let ((role (node-role node)))
    (when (call-arg? role)
      (cond ((lambda-node? (call-proc (node-parent node)))
             (eq role call-proc))
            ((eq role call-proc)
             (zerop (call-exits (node-parent node))))
            (t
             (<= (call-arg-number role)
                 (call-exits (node-parent node))))))))

;;; Make ARGS the arguments of call node PARENT.

(defun relate-call-args (node &rest args)
  (relate-list call-arg-relations-array 1 node (cdr (call-proc+args node)) args))

(defun relate-call-args-list (node args)
  (relate-list call-arg-relations-array 1 node (cdr (call-proc+args node)) args))


;;; Replace the arguments of call node NODE with (possibly shorter) NEW-ARGS.
(defun relate-new-call-args (node new-args)
  (let ((l (cdr (call-proc+args node))))
    (setf (cdr (call-proc+args node))
          (let ((n (- (length l) (length new-args))))
            (nthcdr n l))))        ; pairs lost...
  (relate-call-args-list node new-args))


#||||||||||||||||
;;; Replace the arguments of call node NODE with NEW-ARGS.

(define (replace-call-args node new-args)
  (walk (lambda (n)
          (if (not (empty? n))
              (erase (detach n))))
        (call-args node))
  (relate-new-call-args node new-args))


; Avoiding n-ary procedures

(define (relate-two-call-args node a1 a2)
  (let ((l (flist2 a1 a2 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))

(define (relate-three-call-args node a1 a2 a3)
  (let ((l (flist3 a1 a2 a3 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))

(define (relate-four-call-args node a1 a2 a3 a4)
  (let ((l (flist4 a1 a2 a3 a4 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))

(define (relate-five-call-args node a1 a2 a3 a4 a5)
  (let ((l (flist5 a1 a2 a3 a4 a5 '())))
    (relate-call-args node l)
    (return-list-to-freelist l)))


||||||||||||||||#

;;; UNDEFINED
;;;==========================================================================
;;; UNDEFINED is used as an initial value (ie in a lambda) when we don't want
;;; the variable initialized

(defvar undefined (gensym 'undefined))




;;; RECLAIMING NODES
;;;============================================================================
;;;     Erase node structure.  Updates the REFS slot of variables free to this
;;; node and returns the node to the pool.  There is some safety question
;;; here about erasing references to variables that have already been erased.
;;; I don't think it is a problem but it could be checked.

(defun erase (node)
  (when (not (empty? node))
    (cond ((eq (node-role node) '<erased>)
           (bug "node erased twice ~S" node))
          ((reference-node? node)
           (let ((var (reference-variable node)))
             (setf (variable-refs var)
                  (delete node (variable-refs var)))))
          ((lambda-node? node)
;                (walk (lambda (v)
;                        (if v (return-to-pool variable-pool v)))
;                      (lambda-all-variables node))
;                (return-list-to-freelist (lambda-all-variables node))
           (setf (lambda-all-variables node) '())))
    (setf (node-role node) '<erased>)
;    (incf *node-return-count*)
;         (return-to-pool node-pool node)
    ))

(defun erase-all (node)
  (unless (empty? node)
    (typecase node
      (lambda-node
       (erase-all (lambda-body node)))
      (call-node
       (mapc #'erase-all (call-proc+args node))
       ;  (return-list-to-freelist (call-proc+args node)))
       )
;      (object-node
;       (mapc #'erase-all (object-proc node))
;       ;(return-to-freelist (object-proc-pair node))
;       (mapc #'erase-all (object-operations node))
;       ;(return-list-to-freelist (object-operations node))
;       (mapc #'erase-all (object-methods node))
;       ;(return-list-to-freelist (object-methods node))
;       )
      )
    (erase node)))

;;; CONNECTING AND DISCONNECTING NODES
;;;===========================================================================

;;; Disconnect node from its parent.

(zl:defsubst detach (node)
  (setf (relation (node-role node) (node-parent node)) empty)
  (setf (node-role node) nil)
  (setf (node-parent node) empty)
  node)

;;; Replace node in tree with value of applying proc to node.
;;; Note the fact that a change has been made, for the simplifier.

(defun move (node proc)
  (let ((parent (node-parent node))
        (role (node-role node)))
    (mark-changed node)
    (detach node)
    (relate role parent (funcall proc node))))

;;; Add a new call into the node tree after lambda-node PARENT.

(defun insert-call (call cont parent)
  (move (lambda-body parent)
        #'(lambda (old-body)
            (relate lambda-body cont old-body)
            call)))

;;; Replace old-node with new-node.
;;; Note the fact that a change has been made, for the simplifier.

(defun replace-node (old-node new-node)
  (let ((role (node-role old-node))
        (parent (node-parent old-node)))
    (mark-changed old-node)
    (setf (node-parent old-node) empty)
    (erase-all old-node)
    (setf (relation role parent) new-node)
    (setf (node-simplified? new-node) nil)
    (setf (node-parent new-node) parent)
    (setf (node-role new-node) role)))

(defun mark-changed (node)
  (do ((parent (node-parent node) (node-parent parent)))
      ((or (empty? parent)
            (not (node-simplified? parent))))
    (setf (node-simplified? parent) nil)))

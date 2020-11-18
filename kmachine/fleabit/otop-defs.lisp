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

;;; Reckless (little type checking)
;;; Reclaims node and variable storage


;;; VARIABLES
;;;===========================================================================
;;; Structures to represent variables.

(defstruct (variable (:print-function (lambda (struct stream depth)
                                        depth
                                        (format stream "#{Variable ~A_~D}"
                                                (variable-name struct)
                                                (variable-id struct)))))

  name          ; Source code name for variable (temporary, for debugging only)
  id            ; Unique numeric identifier
  binder        ; LAMBDA node which binds this variable
  support       ; Support information for this variable
  number        ; K: var = (NTH (LAMBDA-ALL-VARIABLES (VARIABLE-BINDER var)) K)
  refs          ; List of leaf nodes n for which (REFERENCE-VARIABLE n) = var.
  type          ; The type of the variable's value at point of binding
  rep           ; Representation for variable's value
  flag          ; Useful slot, used by shapes, COPY-NODE, NODE->VECTOR, etc.
  flags         ; For various annotations, e.g. IGNORABLE
  loc           ; Where this variable lives (<register> SPECIAL, FUNCTION) -efh
  setqs)        ; refs that are setqs


(defvar *variable-id* 0)

;(define variable-pool
;        (make-pool 'variable-pool make-variable 20 variable?))

(defun create-variable (name)
  (let ((var (make-variable))) ; (obtain-from-pool variable-pool)))
    (setf (variable-name    var) name)
    (setf (variable-id      var) *variable-id*)
    (setf (variable-binder  var) nil)
    (setf (variable-support var) nil)
    (setf (variable-refs    var) '())
    (setf (variable-type    var) 'top?)
    (setf (variable-rep     var) 'rep/pointer)
    (setf (variable-flag    var) nil)
    (setf (variable-flags   var) '())
    (setf (variable-loc     var) nil)
    (setf (variable-setqs   var) '())
    (incf *variable-id*)
    var))

(defun used? (var)
  (and var
       (variable-refs var)))

;;; NODES
;;;============================================================================
;;; There are three node types:
;;;  - LAMBDA
;;;  - CALL
;;;  - LEAF
;;; Calls have a nonzero number of children, lambda nodes have a single child,
;;; leaf node have none.


(defstruct (node (:print-function print-node))
  variant           ; Node type, a predicate (e.g. LAMBDA-NODE?)
  parent            ; Parent node
  (role '<new>)     ; node == ((NODE-ROLE node) (NODE-PARENT node))
  simplified?       ; True if it has already been simplified.
  instructions
  stuff-0           ; Variant components
  stuff-1
  stuff-2
  stuff-3
  stuff-4
  stuff-5)

(defun print-node (node stream depth)
  (let ((variant (node-variant node)))
    (if (eq variant 'leaf-node)
        (print-leaf-node node stream depth)
      (format stream "#{~S ~A ~D}" variant (pp-cps-2 node) (object-hash node)))))

#|
(define node-pool (make-pool 'node-pool
                             (lambda ()
                               (let ((new (make-node)))
                                 (set (node-role new) '<new>)
                                 new))
                             30
                             node?))
(defvar *node-count* 0)
(defvar *node-return-count* 0)
|#

;;; EMPTY
;;;==========================================================================
;;; EMPTY is used to mark empty parent and child slots in nodes.

(defconstant empty "#{Empty}")

(defconstant *empty* empty) ; compatibility

(zl:defsubst empty? (obj) (eq obj empty))

(defun proclaim-empty (probe)
  (cond ((not (empty? probe))
         (bug "not empty - ~S" probe))))



;;; NODE VARIANTS
;;;==========================================================================
;;; A "node variant" is a predicate which answers true to nodes which
;;; belong to this variant node type.


(defmacro def-slot-alias (new-name old-name)
  `(progn (zl:putprop ',new-name (get ',old-name 'si:defstruct-slot) 'si:defstruct-slot)
          (zl:defsubst ,new-name (node) (,old-name node))
          (defsetf ,new-name (node) (value) `(setf (,',old-name ,node) ,value))))

(defmacro def-node-variant (name &rest slot-names)
  (if (> (length slot-names) 6) (error "Too many slots in node variant"))
  `(progn
     (zl:defsubst ,(intern (concatenate 'string (symbol-name name) "?")) (obj)
       (and (node-p obj) (eq (node-variant obj) ',name)))
     ,@(mapcar #'(lambda (variant-slot node-slot)
                   `(def-slot-alias ,variant-slot ,node-slot))
               slot-names
               '(node-stuff-0 node-stuff-1 node-stuff-2 node-stuff-3 node-stuff-4 node-stuff-5))))


(defun create-node (variant)
  (let ((node (make-node)))  ;(obtain-from-pool node-pool)))
    (if (not (or (eq '<erased> (node-role node))
                 (eq '<new> (node-role node))))
        (bug "new node already in use ~S" node))
;    (incf *node-count*)
    (setf (node-variant      node) variant)
    (setf (node-parent       node) empty)
    (setf (node-role         node) '<free>)
    (setf (node-simplified?  node) nil)
    (setf (node-instructions node) '())
    (setf (node-stuff-0      node) nil)
    (setf (node-stuff-1      node) nil)
    (setf (node-stuff-2      node) nil)
    (setf (node-stuff-3      node) nil)
    (setf (node-stuff-4      node) nil)
    (setf (node-stuff-5      node) nil)
    node))


(defun make-empty-node-list (size)
  (make-list size :initial-element empty))


;;; RELATIONS
;;;=========================================================================
;;; A "relation" is a selector procedure - something appropriate to put in
;;; the ROLE slot of a node.


(defmacro relation-slot-index (relation) `(car ,relation))
(defmacro relation-id (relation) `(cadr ,relation))
(defmacro relation-variant (relation) `(cddr ,relation))
(defmacro relation-index (relation) `(cdar ,relation))


(defun get-slot-index (slot-name)
  (let ((slot (get slot-name 'si:defstruct-slot)))
    (si:defstruct-slot-description-number
      (cdr (assoc (cdr slot)
                  (si:defstruct-description-slot-alist (si:get-defstruct-description (car slot))))))))

(defmacro def-relation (id variant slot)
  `(progn
     (defvar ,id `(,(get-slot-index ',slot) ,',id  . ,',variant))
     ,@(unless (eq id slot) `((def-slot-alias ,id ,slot)))))


(defun make-list-relation (id variant slot index)
  `((,(get-slot-index slot) . ,index) ,id  ,variant))


(defmacro def-list-relation (id variant slot index)
  `(progn
     (defvar ,id (make-list-relation ',id ',variant ',slot ',index))
     (zl:defsubst ,id (node) (nth ,index (,slot node)))
     (defsetf ,id (node) (value) `(setf (nth ,',index (,',slot ,node)) ,value))))


;(define (make-list-relation id variant slot index pred)
;  (object (lambda (node)
;            (nth (slot node) index))
;          ((setter self)
;           (lambda (node value) (set (nth (slot node) index) value)))
;          ((relation-index self) index)
;          ((relation-variant self) variant)
;          ((pred self) t)               ; hack
;          ((print self stream)
;           (format stream "#{Relation~_~S}" id))))


;;; access the <relation> slot of a node
(defun relation (relation node)
  (let ((index (relation-slot-index relation)))
    (if (integerp index)
        (aref node index)
      (nth (cdr index) (aref node (car index))))))

;;; change the <relation> slot of a node
(defun set-relation (relation node value)
  (let ((index (relation-slot-index relation)))
    (if (integerp index)
        (setf (aref node index) value)
      (setf (nth (cdr index) (aref node (car index))) value))))

(defsetf relation set-relation)



;(zl:defsubst relate (relation parent child)
;  (proclaim-empty (node-parent child))  ; Could be flushed
;  (proclaim-empty (relation parent))    ; Could be flushed
;  (setf (relation parent) child)
;  (setf (node-parent child) parent)
;  (setf (node-role child) relation))


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

(def-node-variant leaf-node
    leaf-value
    leaf-variant
    leaf-type
    leaf-flags)

(defun print-leaf-node (node stream depth)
  depth
  (format stream "#{~S ~S ~S}" (leaf-variant node) (leaf-value node) (object-hash node)))

(defun create-leaf-node (value variant)
  (let ((node (create-node 'leaf-node)))
    (setf (leaf-value   node) value)
    (setf (leaf-variant node) variant)
    (setf (leaf-type    node) nil)
    node))

;;; PRIMOP NODES
;;;=========================================================================


(def-slot-alias primop-value leaf-value)

(zl:defsubst create-primop-node (p)
  (create-leaf-node p 'primop))


(zl:defsubst primop-node? (node)
  (and (leaf-node? node)
       (eq (leaf-variant node) 'primop)))

;;; Checks to see if NODE is a reference to on of the primops in PRIMOPS.
(zl:defsubst primop-ref? (node primop)
  (and (primop-node? node)
       (eq (primop-value node) primop)))


;;; LITERAL NODES
;;;=========================================================================

(zl:defsubst create-literal-node (value)
  (create-leaf-node value 'literal))

(zl:defsubst literal-node? (node)
  (and (leaf-node? node)
       (eq (leaf-variant node) 'literal)))

(def-slot-alias literal-value leaf-value)

;;; REFERENCE NODES
;;;=========================================================================

(def-slot-alias reference-variable leaf-value)

(def-slot-alias reference-flags leaf-flags)

(zl:defsubst create-reference-node (variable)
  (let ((node (create-leaf-node variable 'reference)))
    (push node (variable-refs variable))
    node))

(zl:defsubst reference-node? (node)
  (and (leaf-node? node)
       (eq (leaf-variant node) 'reference)))

(zl:defsubst variable-ref? (node &rest variables)
  (and (reference-node? node)
       (member (reference-variable node) variables)))


;;; LAMBDA NODES
;;;============================================================================

(def-node-variant lambda-node
     lambda-body               ;the node for the body (after CPS, always a call node)
     lambda-all-variables      ;list of variables which are bound by this lambda.  The first
                               ;variable gets bound to the procedure itself; the second is a 'rest'
                               ;variable, if it is non-null the lambda is n-ary.
     lambda-env                ;list of variables live on entry to this lambda
     lambda-strategy           ;label, stack, or heap (where are closures over this lambda?)
     lambda-live               ;the variables live in the body of the lambda.
     lambda-db)                ;trace information.

(def-relation lambda-body lambda-node lambda-body)

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
  (let ((node (create-node 'lambda-node))
        (vars (cons (create-variable name)
                    vars)))
    (setf (lambda-all-variables node) vars)
    (setf (lambda-strategy      node) nil)
    (setf (lambda-live          node) nil)
    (setf (lambda-env           node) nil)
    (setf (lambda-body          node) empty)
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

(def-node-variant call-node
   call-proc+args              ;list of child nodes
   call-exits                  ;the number of initial arguments that are continuations
   call-complexity             ;no longer used...
   call-hoisted-cont           ;continuation to be consed on stack
   call-open)                  ;added -efh ,the call to primop/open for this call


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
;; Add new argument relations if there aren't enough."
(defun create-call-node (n exits)
  (let ((node (create-node 'call-node)))
    (setf (call-proc+args node) (make-empty-node-list n))
    (setf (call-exits node) exits)
    (setf (call-complexity node) nil)
    (setf (call-hoisted-cont node) nil)
    (setf (call-open node) nil)    ;***
    node))

;;; ARGUMENT RELATIONS
;;;========================================================================
;;; Argument relations are created on demand whenever a newly created
;;; call node is going to have more arguments than any previous call node
;;; has had.

(zl:defsubst call-arg? (relation)   ;???
  (let ((relid (relation-id relation)))
    (or (eq relid 'call-proc)
        (and (consp relid)
             (eq (car relid) 'call-arg)))))

(defun make-arg-relation (i)
  (make-list-relation `(call-arg ,i) 'call-node 'call-proc+args i))

(zl:defsubst call-arg-number (relation)
  (relation-index relation))

(def-list-relation call-proc call-node call-proc+args 0)


(defconstant call-arg-relations
             (make-infinite-vector 10 #'make-arg-relation 'call-arg-relations))
(setf (vref call-arg-relations 0) call-proc)
(defun call-arg-relations (n)
  (vref call-arg-relations n))


(zl:defsubst call-arg (i)
  (call-arg-relations i))

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
  (relate-list call-arg-relations 1 node (cdr (call-proc+args node)) args))

(defun relate-call-args-list (node args)
  (relate-list call-arg-relations 1 node (cdr (call-proc+args node)) args))



#||||||||||||||||
;;; Replace the arguments of call node NODE with NEW-ARGS.

(define (replace-call-args node new-args)
  (walk (lambda (n)
          (if (not (empty? n))
              (erase (detach n))))
        (call-args node))
  (relate-new-call-args node new-args))

;;; Replace the arguments of call node NODE with (possibly shorter) NEW-ARGS.

(define (relate-new-call-args node new-args)
  (modify (cdr (call-proc+args node))
          (lambda (l)
            (let ((n (fx- (length l) (length new-args))))
              (nthcdr l n))))        ; pairs lost...
  (relate-call-args node new-args))

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
    (case (node-variant node)
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
  (setq *tree-changed?* t)
  (do ((parent (node-parent node) (node-parent parent)))
      ((or (empty? parent)
            (not (node-simplified? parent))))
    (setf (node-simplified? parent) nil)))

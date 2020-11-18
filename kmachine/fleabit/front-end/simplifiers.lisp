;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

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

;;; Procedures to simplify calls to various primops

;;; (SIMPLIFY-TEST node)
;;;=========================================================================
;;; (PRIMOP/CONDITIONAL <exit1> <exit2> PRIMOP/TRUE? #F)
;;; => <exit2>
;;; (PRIMOP/CONDITIONAL <exit1> <exit2> PRIMOP/TRUE? not-#F)
;;; => <exit1>

(defun simplify-test (node)
  (destructure (((exit-1 exit-2 test val) (call-args node)))
    (cond ((not (primop-ref? test primop/true?))
           nil)
          (t (let ((value (and (reference-node? val)
                               (table-entry *value-table* (reference-variable val)))))
               (cond (value
                      (if (eq value 'true)
                          (replace-test node exit-1)
                        (replace-test node exit-2))
                      t)
                     ((literal-node? val)
                      (if (leaf-value val)
                          (replace-test node exit-1)
                        (replace-test node exit-2))
                      t)))))))


(defun replace-test (call-node new-node)
  (let ((new-call (create-call-node 1 0)))
    (detach new-node)
    (relate call-proc new-call new-node)
    (replace-node call-node new-call)))


;;; (SIMPLIFY-IF-CONSTANT-PREDICATE node function)
;;; ===============================================
;;; ($CONDITIONAL <then> <else> <pred> <literal1> <literal2>...)
;;; => (if (<pred> <literal1> <literal2> ...)
;;;        <then>
;;;        <else>)
(defun simplify-if-constant-predicate (node fcn)
  (let ((args (nthcdr 3 (call-args node))))
    (when (every #'literal-node? args)
      (replace-test node
                    (if (apply fcn (mapcar #'leaf-value args))
                        (call-arg-n 1 node)
                      (call-arg-n 2 node))))))


;;; (SIMPLIFY-IF-CONSTANT-EXPRESSION node function &optional (boxed t))
;;;=========================================================================
;;; ($FOO <exit> <literal1> <literal2> ...)
;;; => (<exit> <exp-literal>)
;;; <exp-literal> = (funcall function <literal1> <literal2> ...)

(defun simplify-if-constant-expression (node fcn)
  (let ((args (call-non-exit-args node)))
    (when (every #'literal-node? args)
      (replace-call-with-value node
                               (create-literal-node
                                 (apply fcn
                                        (mapcar #'convert-unboxed-constants
                                                (mapcar #'leaf-value args)))))
      t)))

;;; This is a crock.
(defun convert-unboxed-constants (value)
  (if (and (listp value)
           (symbolp (first value))
           ;; does the HW package exist here?
           (string-equal (symbol-name (first value)) "UNBOXED-CONSTANT"))
      (second value)
      value))


;;; (SIMPLIFY-FUNCALL node)
;;;=========================================================================
;;; ($FUNCALL-INTERNAL <exit> <lambda-node> . <args>)
;;; => (<lambda-node> <exit> . <args>)

;;; variable-known is not valid yet
(defun label? (var)
  (let ((parent (node-parent (variable-binder var))))
    (and (not (empty? parent))
         (primop-node? (call-proc parent))
         (eq primop/Y (primop-value (call-proc parent))))))

(defun simplify-funcall (node)
  (let ((fcn (call-arg-n 2 node)))
    (cond
      ((or (lambda-node? fcn)
               (and (reference-node? fcn)
                    (label? (reference-variable fcn))))
       (let ((args (call-args node)))
         (setf (call-proc+args node)
               (cons (pop (cdr args)) args))
         t))
      ; ((literal-node? fcn))
      )))




#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;;; (SIMPLIFY-LOCATION node)
;;;============================================================================
;;; What is going on here?
;;;
;;;    A location primop supposedly returns a locative.  This does simple
;;; representation analysis to elide the call to the location primop if the
;;; locative is not really needed.  This can go away once real representation
;;; analysis exists.
;;;
;;; (LOCATIVE (<location> . <args>)) => (LOCATIVE-LOCATION <location> . <args>)
;;; (SET-CONTENTS (<location> . <args>) <value>) =>
;;;     (SET-LOCATION <location> <value> . <args>)
;;; Otherwise (<location> . <args>) => (CONTENTS-LOCATION <location> . <args>)

(define (simplify-location node)
  (let* ((args (map detach (call-proc+args node)))
         (new-call (create-call-node (fx+ 1 (length args)) 1)))
    (relate call-proc new-call (create-primop-node primop/contents-location))
    (let ((loc (car args)))
      (set (car args) (cadr args))
      (set (cadr args) loc))
    (relate-call-args new-call args)
    (replace-node node new-call)
    t))

(define (integrate-setter? node)
  (and (eq? (node-role node) call-proc)
       (destructure (((#f cont arg . rest)
                      (call-proc+args (node-parent node))))
         (let ((p (known-primop arg)))
           (and (null? rest)
                p
                (primop.settable? p)
                (lambda-node? cont)   ;;; should do nargs check on lambda here
                (all-refs-are-calls? (car (lambda-variables cont))))))))

(define (simplify-setter node)
  (cond ((known-object ((call-arg 2) node))
         => (lambda (obj)
              (simplify-operation-dispatch node obj)))
        ((not (integrate-setter? (call-proc node)))
         nil)
        (else
         (let ((p (known-primop ((call-arg 2) node))))
           (walk-refs-safely (lambda (ref)
                               (primop.simplify-setter p (node-parent ref)))
                             (car (lambda-variables ((call-arg 1) node))))
           (replace node (detach (lambda-body ((call-arg 1) node))))
           t))))

(define (simplify-location-set location call nargs)     ; needs arg checking
  (ignore nargs)
  (let ((args (reverse! (map detach (cdr (call-args call)))))
        (new (create-call-node (fx+ 2 (length (call-args call))) 1)))
    (relate call-proc new (create-primop-node primop/set-location))
    (relate-call-args new
                      `(,(detach ((call-arg 1) call))
                        ,(create-primop-node location)
                        ,(car args)
                        . ,(reverse! (cdr args))))
      (replace call new)))


;;; CALL is a call node whose procedure is PRIMOP.  Checks to see that there
;;; are the right number of arguments and that they are all literals.  If so,
;;; the values are attached the primop and declared as support for any
;;; appropriate variables.  This works for the one necessary (simple) case.
;;; Anything fancier will probably lose.  The problem is to simplify the
;;; support for a variable without simplifying the original definition.

(define (simplify-parameterized-primop primop call)
  (cond ((or (fxn= (length (primop.formals primop)) ; Could be improper?
                   (fx+ -1 (length (call-args call))))
             (not (every? literal-node? (cdr (call-args call)))))
         nil)
        (else
         (let ((args (map literal-value (cdr (call-args call)))))
           (replace-call-with-new-primop call (construct-primop primop args))
           t))))

(define (simplify-*primop primop call)
  (ignore primop)
  (replace-call-with-new-primop call (primop-value ((call-arg 2) call)))
  t)

(define (replace-call-with-new-primop call primop)
  (let ((thunk-node (subexpression->code-tree (primop.make-closed primop))))
    (add-call-value-to-support call primop nil)
    (replace (call-proc call) thunk-node)
    (walk (lambda (n) (erase (detach n)))
          (cdr (call-args call)))
    (relate-new-call-args call (list (detach ((call-arg 1) call)))))
  (values))

(define (simplify-parameterized-structure-accessor primop call)
  (destructure (((cont stype offset slot) (call-args call)))
    (cond ((or (fxn= 4 (length (call-args call)))
               (not (reference-node? stype))
               (not (literal-node? offset))
               (not (literal-node? slot)))
           nil)
          (else
           (let ((new-primop (construct-primop primop
                                               `(,(literal-value offset)))))
             (add-call-value-to-support call new-primop t)
             (replace (call-proc call)
                      (create-reference-node
                       (obtain-free-variable 'stype-selector)))
             (walk detach (call-args call))
             (erase offset)
             (relate-new-call-args call (list cont stype slot))
             t)))))

;;; This is not good enough but what to do?
;;; Simplify our continuation ourselves?

(define (add-call-value-to-support call primop parameterized?)
  (let ((cont ((call-arg 1) call)))
    (cond ((or (not (lambda-node? cont))
               (fxn= 1 (length (lambda-variables cont))))
           (dropped-primop-warning call primop))
          (else
           (iterate loop ((refs (variable-refs (car (lambda-variables cont))))
                          (hit? nil))
             (cond ((null? refs)
                    (if (not hit?)
                        (dropped-primop-warning call primop parameterized?)))
                   ((add-primop-to-support (car refs) primop)
                    (loop (cdr refs) t))
                   (else
                    (loop (cdr refs) hit?)))))))
  (undefined))

(define (add-primop-to-support ref primop)
  (let ((parent (node-parent ref))
        (node (create-primop-node primop)))
    (cond ((or (not (call-arg? (node-role ref)))
               (not (variable-definition? parent)))
           (erase node)
           nil)
          (else
           (let ((defined-var (reference-variable ((call-arg 2) parent))))
             (and defined-var
                  (not (containing-definition parent))
                  (add-support-value defined-var node 'top)))))))

(define (dropped-primop-warning call primop parameterized?)
  (user-message 'warning
                (containing-definition call)
                (if parameterized?
                    "parameterized primop ~S not added to support"
                    "primop ~S not added to support")
                nil
                primop))

;;; (PRESIMPLIFY-VALUES node)
;;;============================================================================
;;; (values cont a b c)
;;;  ==>  (cont a b c)

(define (presimplify-values node)
  (let ((args (map detach (call-args node))))
    (set (call-exits node) 0)
    (replace-call-args node (cdr args))
    (replace (call-proc node) (car args))
    t))


;;; (PRESIMPLIFY-PREDICATE node)
;;;============================================================================
;;; (<type>? cont x) =>
;;; (primop/conditional (lambda () (cont #t))
;;;                     (lambda () (cont #f))
;;;                     primop/test
;;;                     <type>?
;;;                     x)

(defun presimplify-predicate (node)
  (destructure (((pred cont arg) (call-proc+args node)))
    (let ((primop (if (primop-node? pred)
                      pred
                      (create-primop-node (known-primop pred))))
          (test (create-primop-node primop/test)))
      (construct-conditional node test cont primop arg)
      (if (reference-node? pred) (erase pred))
      t)))

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#


;;; (PRESIMPLIFY-TO-CONDITIONAL node)
;;;============================================================================
;;; (<cond> cont arg1 arg2) =>
;;; (primop/conditional (lambda () (cont #t))
;;;                     (lambda () (cont #f))
;;;                      <cond> arg1 arg2)
;;; where <cond> is one of test, eq?, fx<, etc.

(defun presimplify-to-conditional (node)
  (apply #'construct-conditional node (call-proc+args node))
  t)

(defun construct-conditional (node pred cont &rest args)
  (let ((primop (if (primop-node? pred)
                    pred
                    (create-primop-node (known-primop pred)))))
    (mapc #'detach (call-proc+args node))
    (if (reference-node? pred) (erase pred))
    (let ((c1 (create-call-node 2 1))
          (c2 (create-call-node (+ 4 (length args)) 2))
          (c3 (create-call-node 2 0))
          (c4 (create-call-node 2 0))
          (v1 (create-variable 'c)))
      (let ((l1 (create-lambda-node 'c (list nil v1)))
            (l2 (create-lambda-node 'c '()))
            (l3 (create-lambda-node 'c '())))
        (relate call-proc c1 l1)
        (relate (call-arg 1) c1 cont)
        (relate lambda-body l1 c2)
        (relate call-proc c2 (create-primop-node primop/conditional))
        (relate-call-args-list c2 (list* l2 l3 primop args))
        (relate lambda-body l2 c3)
        (relate call-proc c3 (create-reference-node v1))
        (relate (call-arg 1) c3 (create-literal-node t))   ;'#t))
        (relate lambda-body l3 c4)
        (relate call-proc c4 (create-reference-node v1))
        (relate (call-arg 1) c4 (create-literal-node nil)) ;'#f))
        (replace-node node c1)))))


;(define (construct-conditional node pred cont arg1 arg2)
;  (walk detach (call-proc+args node))
;  (let ((call (s-exp->call-node `(0 (lambda c (#f v1)
;                                      (2 ,primop/conditional
;                                         (lambda c () (0 v1 (quote #t)))
;                                         (lambda c () (0 v1 (quote #f)))
;                                         ,pred
;                                         ,arg1
;                                         ,arg2))
;                                    ,cont))))
;    (if (reference-node? pred) (erase pred))
;    (replace node call)))

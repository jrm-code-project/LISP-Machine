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

;;; Compilators
;;;===========================================================================
;;;   The compiler knows about several special forms.  Some of them are part
;;; of Lisp, the others are internal to the compiler.
;;;   Handlers for the special forms that are of interest to the compiler
;;; are defined with the following forms.

(defvar *compilator-table*
  (make-table '*compilator-table*))

;;; Define a compilator for a special form.

(defmacro define-compilator (pattern &body body)
  (let ((name (car pattern))
        (sym (concatenate-symbol 'compilator/ (car pattern)))
        (exp (gensym 'exp)))
    `(progn
       (defun ,sym (,exp)
         (destructure ((,(cdr pattern) (cdr ,exp)))
              ,@body))
       (setf (table-entry *compilator-table* ',name) #',sym))))


;;; (QUOTE . blah)
;;;   Return a literal node.

(define-compilator (QUOTE value)
  (values (create-literal-node (copy-tree value)) nil nil))

;;;; (FUNCTION var)
;(define-compilator (FUNCTION var)
;  (make-call `(,primop/function-ref ,var)))

;;; (LAMBDA vars . body)

(define-compilator (LAMBDA name vars body)
  (let ((node (make-lambda name (copy-list vars) body)))
    (mapc #'(lambda (var)
              (if var (unused-variable-check var)))
          vars)
    (values node nil nil)))

(define-compilator (optional-init)
  (make-call `(,primop/noop)))

(define-compilator (optional-setup first-label nargs supplied-p-vars init-labels)
  (let* ((call (create-call-node (+ 5 (length init-labels)) 2))
         (top (cps-args call `(,primop/optional-setup ,empty ,first-label
                               ,nargs ,supplied-p-vars
                               ,@init-labels))))
    (values top call (call-arg 1))))


;;; make the lambdas be exit args
(define-compilator (dispatch-special-form . args)
  (let ((lambda-args (do ((n 0 (1+ n))
                          (args args (cdr args)))
                         ((not (and (consp (car args))
                                    (eq (caar args) 'LAMBDA)))
                          n))))
    ;; would be nice if make-call-with-exits was a little better
    ;; about returning top/node/role
    (let ((call (create-call-node (+ 2 (length args)) (1+ lambda-args))))
      (values
        (cps-args call `(,primop/%dispatch ,empty ,@args))
        call
        (call-arg 1)))))



(define-compilator (SETQ-LEXICAL var value)
  (multiple-value-bind (node c-parent c-role)
      (make-call `(,primop/setq-lexical ,var ,value))
    (push c-parent (variable-setqs var))
    (values node c-parent c-role)))

(define-compilator (the value-type form)
  (multiple-value-bind (node c-parent c-role) (->node form)
    (cond ((literal-node? node)
           (unless (typep (literal-value node) value-type)
             (warn "~s is not of type ~s as declared by THE." (literal-value node) value-type))
           (values node c-parent c-role))
          ((reference-node? node)
           (setf (leaf-type node) value-type)
           (values node c-parent c-role))
          ;; somehow we want to get the type of the cont-var set I think...
          (t (values node c-parent c-role)))))

;;; (PRIMOP id formals . clauses)
;;;   The special form for introducing primitive operations.  The name should
;;; be changed to stop all the warning messages for PRIMOP variables.

;(define-compilator (primop primop)
;  (make-call `(,primop/*primop ,primop)))


;;; (IF p c a)
;;;   ==>
;;; ((lambda (join)
;;;    (primop/conditional
;;;     (lambda () (join c))
;;;     (lambda () (join a))
;;;     primop/true
;;;     p))
;;;  <continuation>)

(define-compilator (IF tested con alt)
  (let* ((j-var (create-variable "J"))
         (j-lambda (create-lambda-node "P" (list nil j-var)))
         (j-call (create-call-node 2 0))
         (c-call (make-call-with-exits 2
                                       (list primop/conditional
                                             (thunkify con j-var)
                                             (thunkify alt j-var)
                                             primop/true?
                                             tested))))
    (relate call-proc j-call j-lambda)
    (relate lambda-body j-lambda c-call)
    (values j-call j-call (call-arg 1))))

(defun thunkify (exp cont-var)
  (let ((l-node (create-lambda-node "C" (list nil))))
    (multiple-value-bind (call c-parent c-role)
        (make-block (list exp))
      (relate lambda-body l-node call)
      (relate c-role c-parent (create-reference-node cont-var))
      l-node)))

;;; (LABELS ((v1 e1) (v2 e2) ... (vn en)) . body)
;;;   ==> (exits 1 primop/Y
;;;                <cont>
;;;                (lambda (c v1 ... vn)
;;;                  (exits 0 c
;;;                           (lambda (c) . body)
;;;                           (lambda () e1)
;;;                             ...
;;;                           (lambda () en))))
;;; The code generator depends on call-exits being one for PRIMOP/Y

(define-compilator (LABELS vars vals body)
  (let* ((call (create-call-node 2 1))
         (c-var (create-variable "C"))
         (b-lambda (create-lambda-node "C" (list nil '())))
         (y-lambda (create-lambda-node
                    "Y" (list* nil c-var (copy-list vars)))))
    (let ((y-call (make-call-with-exits 0
                                        `(,(create-reference-node c-var)
                                          ,b-lambda
                                          . ,vals)))) ;(mapcar #'value->thunk vals)))))
      (multiple-value-bind (b-call c-parent c-role)
          (make-block body)
        (relate call-proc call (create-primop-node primop/y))
        (relate (call-arg 1) call y-lambda)
        (relate lambda-body y-lambda y-call)
        (relate lambda-body b-lambda b-call)
        (values call c-parent c-role)))))

;(defun value->thunk (val)
;  (->value-node `(LAMBDA T (nil ,(create-variable "C")) (,val))))  ;#f


(define-compilator (PROGN forms)
  (make-block forms))


;;; BLOCK turns into a call to a lambda
;;; which binds the blocks continuation
;;; RETURN-FROM will be a call to the bound var
(define-compilator (BLOCK var forms)
  (let ((block-lambda (make-lambda 'block (list nil var) forms))
        (block-call (create-call-node 2 1)))
    (relate call-proc block-call block-lambda)
    (values block-call block-call (call-arg 1))))


;what do we do with the (ignored) continuation to return-from?
; (block xxx ... (return-from xxx v) ...)
;
; ((block_n nil xxx_n) ...
;        (xxx_n v))
;
; how about:
;        (^RF #cont#)
; ((RF IGNORE) (xxx_n v))
;
(define-compilator (RETURN-FROM block-cont value)
  ;; *** unwind the stack ***
;  (let ((call (create-call-node 2 0))
;       (value (->value-node value)))
;   (relate call-proc call (create-reference-node block-cont))
;   (relate (call-arg 1) call value)
  (multiple-value-bind (call c-node c-role)
      ;; deal with possible multiple values here
      (->node value)
    (if (call-node? call)
        (relate c-role c-node (create-reference-node block-cont))
      (progn
        (setq call (make-call-with-exits 0 (list block-cont call)))))
    (let ((crock-lambda (create-lambda-node "RF" (list (create-variable "IGNORE"))))
          (crock-call (create-call-node 2 1)))
      (relate lambda-body crock-lambda call)
      (relate call-proc crock-call crock-lambda)
      (values crock-call crock-call (call-arg 1)))))


;;; tagbody is just like labels but we also bind
;;; the continuation to the entire tagbody, go's
;;; take this as a continuation and pass it to
;;; the labels, the last label will call its continuation
(define-compilator (TAGBODY tagbody-cont . label-args)
  (let ((let (create-lambda-node "TAGBODY" (list nil tagbody-cont)))
        (call (create-call-node 2 1)))
    (multiple-value-bind (label-call c-node c-role)
        (compilator/labels (cons 'LABELS label-args))
      (relate c-role c-node (create-reference-node tagbody-cont))
      (relate lambda-body let label-call)
      (relate call-proc call let)
      (values call call (call-arg 1)))))


;;; go ignores its own contination
;;; but takes the continuation of the entire tagbody
(define-compilator (GO-INTERNAL tag tagbody-cont)
  (let ((call (make-call-with-exits 2 (list primop/%go tagbody-cont tag))))
    (let ((crock-lambda (create-lambda-node "GO"
                                            ;; ignored continuation hopefully optimized out
                                            (list (create-variable "IGNORE"))))
          (crock-call (create-call-node 2 1)))
      (relate lambda-body crock-lambda call)
      (relate call-proc crock-call crock-lambda)
      (values crock-call crock-call (call-arg 1)))))


(define-compilator (MULTIPLE-VALUE-BIND vars exp body)
  (multiple-value-bind (e-node e-parent e-role)
      (->call-node exp)
    (multiple-value-bind (b-node b-parent b-role)
        (make-block body)
      (let ((l-node (create-lambda-node "R" (cons nil (copy-list vars)))))
        (relate lambda-body l-node b-node)
        (relate e-role e-parent l-node)
        (values e-node b-parent b-role)))))


(define-compilator (VALUES vals)
  (let* ((call (create-call-node (1+ (length vals)) 0))
         (top (cps-args call `(,empty ,@vals))))
    (values top call call-proc)))

(define-compilator (%CATCH-OPEN cont tag)
  (let ((call (make-call-with-exits 2 `(,primop/%catch-open ,empty ,cont ,tag))))
    (values call call (call-arg 1))))

;;; %CATCH-BODY is wrapped around a CATCH body
;;; It inserts a call to the primop %CATCH-BODY-VALUES
;;; around every place where the body produces a value.
;;; %CATCH-BODY-VALUES will look ahead to the continuation
;;; of the body to see where the values will be needed.
(define-compilator (%CATCH-BODY body)
  (wrap-body-values primop/%catch-body-values nil body))

(define-compilator (THROW-INTERNAL tag result)
  (wrap-body-values primop/%throw-internal (list tag) result))

(defun wrap-body-values (primop extra-args body)
  (multiple-value-bind (node c-parent c-role) (->node body)
    (wrap-body-values-1 primop extra-args node c-parent c-role)))

(defun wrap-body-values-1 (primop extra-args node c-parent c-role)
  (cond ;;
        ;; '259 => (<primop> 1 <cont> '259)
        ;;
        ((or (literal-node? node)
             (reference-node? node))
         (make-call `(,primop ,@extra-args ,node)))
        ;;
        ;; (<cont> 0 '3 V_1 V_2) => (<primop> 1 <cont> '3 V_1 V_2)
        ;;
        ((eq call-proc c-role)
         (let ((args (copy-list (call-args c-parent))))
           (mapc #'detach args)
           (let ((new-call (make-call `(,primop ,@extra-args . ,args))))
             (unless (empty? (node-parent c-parent))
               (replace-node c-parent new-call))
             (values (if (eq node c-parent)
                         new-call
                       node)
                     new-call (call-arg 1)))))
        ;;
        ;; (( ... ) (^P_1 0 <cont>))
        ;;  ((P_1 NIL J_3) ...
        ;;     ...  (J_3 0 X_6)     =>  (<primop> 1 J_3 X_6)
        ;;     ...  (BAR 1 J_3 V_7) =>  ...        (BAR 1 ^C_22 V_7)
        ;;                             ((C_22 V_23) (<primop> 1 J_3 V_23))
        ;;
        ((lambda-node? (call-proc c-parent))
         (let ((jvar (car (lambda-variables (call-proc c-parent)))))
           (when jvar
             (dolist (ref (variable-refs jvar))
               (let ((ref-role (node-role ref))
                     (ref-parent (node-parent ref)))
                 (detach ref)
                 (multiple-value-bind (nil parent role)
                     (wrap-body-values-1 primop extra-args node ref-parent ref-role)
                   (relate role parent ref))))))
         (values node c-parent c-role))
        ;;
        ;; (BA:R 1 <cont> X_1) => ...         (BAR 1 ^C_22 X_1)
        ;;                       ((C_22 V_23) (<primop> 1 <cont> V_23))
        ;;
        ((and (eq c-role (call-arg 1))
              (= 1 (call-exits c-parent)))
         (values node (insert-primop-call-as-cont primop extra-args c-parent) (call-arg 1)))
        (t (bug "something funny in %values"))))


(defun insert-primop-call-as-cont (primop extra-args parent)
  (let* ((var (create-variable 'v))
         (lambda (create-lambda-node "C" (list nil var)))
         (call (make-call `(,primop ,@extra-args ,var))))
    (relate lambda-body lambda call)
    (relate (call-arg 1) parent lambda)
    call))

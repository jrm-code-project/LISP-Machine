;;; -*- Mode:Lisp; Package:NC; Readtable:CL; Base:10 -*-

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


(defvar *primop-field-table* (make-table '*primop-field-table*))

(eval-when (compile eval)

(defparameter *primop-fields-extra-defs* nil)

(defmacro primop-field (name args &body body)
  (let* ((title (concatenate-symbol 'primop. name))
         (field (concatenate-symbol 'xprimop- name))
         (keyname (intern name 'keyword))
         (primop (car args)))
    (push `(progn
             (setf (table-entry *primop-field-table* ',keyname) ',keyname)
             (defun ,title ,args
              (funcall (,field ,primop) . ,(cdr args))))
          *primop-fields-extra-defs*)
     `(function (lambda ,(cdr args) .
                   ,(if body
                        body
                      `((bug "~S has no method for ~S" ',primop ',title)))))))

(defmacro def-primop-fields ()
  `(progn ,@*primop-fields-extra-defs*))
)



(defstruct (xprimop (:print-function (lambda (struct stream depth)
                                       depth
                                       (format stream "#{Primop ~s ~o}"
                                               (xprimop-name struct) (object-hash struct)))))
  name
  bitv
  arglist
  ;; Simplification
  (simplify    (primop-field simplify (primop node) nil))
  (presimplify (primop-field presimplify (primop node) nil))
;  (integrate?  (primop-field integrate? (primop node)
;                  (and (eq (node-role node) call-proc)
;                       (primop-type-check primop (node-parent node)))))
  ;; Code generation
  (generate    (primop-field generate (primop node)))
  (type        (primop-field type (primop node) 'top?))
  source
  handler
  (arg-specs   (primop-field arg-specs (primop) nil))
  (rep-wants   (primop-field rep-wants (primop) nil))
  )

(def-primop-fields)

(zl:deff primop? 'xprimop-p)

;;; System internal fields

(zl:defsubst primop.name (primop) (xprimop-name primop))
(zl:defsubst primop.arglist (primop) (xprimop-arglist primop))
(zl:defsubst primop.source (primop) (xprimop-source primop))


(defvar *primop-predicate-table* (make-table '*primop-predicate-table*))

(eval-when (compile eval)
  (defparameter *next-primop-predicate-index* 0))

(defmacro define-primop-predicate (name)
  (let ((name (concatenate-symbol 'primop. name))
        (keyname (intern (symbol-name name) 'keyword)))
    (prog1 `(progn
              (setf (table-entry *primop-predicate-table* ',keyname) ,*next-primop-predicate-index*)
              (zl:defsubst ,name (primop)
                (logbitp ,*next-primop-predicate-index* (xprimop-bitv primop))))
           (incf *next-primop-predicate-index*))))


(define-primop-predicate constructed?)
(define-primop-predicate support?)
(define-primop-predicate location?)
(define-primop-predicate side-effects?)
(define-primop-predicate handler?)
(define-primop-predicate settable?)
(define-primop-predicate uses-L-value?)
(define-primop-predicate special?)
(define-primop-predicate defines-support?)
(define-primop-predicate type-predicate?)
(define-primop-predicate conditional?)


;;; Non generic, specific to certain types of primops

(defvar *primop-operation-table* (make-table '*primop-operation-table*))

(defmacro define-primop-operation (name args &rest default)
  (let* ((title (concatenate-symbol 'primop. name))
         (field (concatenate-symbol 'xprimop- name))
         (keyname (intern name 'keyword))
         (primop (car args)))
    `(progn (setf (table-entry *primop-operation-table* ',keyname) ',field)
            (defun ,title ,args
              (let ((.method. (table-entry (xprimop-handler ,primop) ',field)))
                (if .method.
                    (funcall .method. . ,args)
                  . ,(if default
                       default
                       `((error "~S has no method for ~S" ,primop ',title)))))))))


;;; Parameterized primops
(define-primop-operation constructor (primop))
(define-primop-operation variant-id  (primop) (primop.name primop))

;;; Creating support
(define-primop-operation support-variant (primop))

;;; Locations
(define-primop-operation location-specs (primop) nil)
(define-primop-operation simplify-setter (self call) nil)


(defvar *constructed-bit*
  (table-entry *primop-predicate-table* 'primop.constructed?))

;;; BUG: Parameterized primops cannot produce parameterized primops
;;; BUG: Parameterized primops cannot be N-ary

(defun primop-code (name arglist new? clauses)
  (multiple-value-bind (bitv fields methods)
      (parse-primop-clauses clauses)
    `(make-xprimop :name ',name
                   :bitv ,bitv
                   :arglist ',arglist
                   :source ',(if new?
                                clauses nil)
                   :handler (let ((handler-table (make-table)))
                              ,@(mapc #'(lambda (meth)
                                          `(setf (table-entry handler-table ',(car meth)) ,(cdr meth)))
                                      methods)
                              handler-table)
                   . ,fields)))



(defmacro set-bit (index word)
  `(dpb 1 (byte 1 ,index) ,word))

(defun parse-primop-clauses (clauses)
  (do ((to-do clauses (cdr to-do))
       (fields '())
       (methods '())
       (bitv 0))
      ((null to-do) (values bitv fields methods))
    (let* ((clause (car to-do))
           (name (car clause))
           (bit (table-entry *primop-predicate-table* name)))
      (if (and bit (cadr clause))
          (setq bitv (set-bit bit bitv))
        (let ((args (cadr clause))
              (body (cddr clause))
              (field (table-entry *primop-field-table* name)))
          (if field
              ;; Could do args checking
              (setq fields
                    (list* field
                           `(function (lambda ,args . ,body))
                           fields))
            (let ((operation (table-entry *primop-operation-table* name)))
              (if operation
                  (push `(,operation . (function (lambda ,args . ,body))) methods)
                (bug "unkown primop operation ~S in ~S" name (car to-do))))))))))


#||||||||
(define (make-primop-constructor name formals clauses)
  (receive (c-bitv c-fields c-methods)
           (parse-primop-clauses clauses)
    (let ((bitv (set-bit *constructed-bit*  c-bitv)))
      `(lambda ,formals
         (sub-primop-constructor ',name
                                 ,bitv
                                 (object nil
                                   ((xprimop-arglist %%handler self)
                                    (list . ,formals))
                                   . ,c-methods)
                                 . ,c-fields)))))

(define (sub-primop-constructor name bitv handler . fields)
  (lambda (parent)
    (let ((p (copy-structure parent)))
      (set (xprimop-id      p) name)
      (set (xprimop-bitv    p) (fixnum-logior bitv (xprimop-bitv parent)))
      (set (xprimop-handler p) (join handler (xprimop-handler parent)))
      (iterate loop ((fields fields))
        (cond (fields
               (set ((car fields) p) (cadr fields))
               (loop (cddr fields)))))
      p)))

(define *constructed-primops*
        (create-%table '*constructed-primops* 0 t list?
                       (lambda (l)
                         (fixnum-abs (fx+ (descriptor-hash (car l))
                                          (fx* (tree-hash (cdr l)) 2))))
                       alikev?))

(define (construct-primop base args)
  (let ((key (cons base args)))
    (cond ((table-entry *constructed-primops* key)
           => identity)
          (else
           (let ((new ((apply (primop.constructor base) args) base)))
             (set (table-entry *constructed-primops* key) new)
             new)))))

|||||#


(defvar *primop-table* (make-table))

(defmacro define-primop (name arglist &body clauses)
  (let ((vname (intern (format nil "PRIMOP/~s" name))))
   `(progn
      (defun ,vname () ,(primop-code name arglist t clauses))
      (defparameter ,vname (,vname))
      (setf (table-entry *primop-table* ',name) ,vname)
      ;; for C-Sh-A
      (setf (get ',name 'si:arglist) ',arglist))))

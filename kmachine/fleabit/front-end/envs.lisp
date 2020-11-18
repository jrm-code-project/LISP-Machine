;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-

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

;;; Early binding database for ORBIT.  DEFINE and its cousins generate early
;;; binding information, usually referred to as support, for the variable
;;; defined.  Currently the only information generated, besides the fact that
;;; the variable have been defined, is an integrable value and then only if
;;; the defining special form is of the proper type.

;;; (MAKE-SUPPORT-TABLE id primop-table)
;;;============================================================================
;;;     A support table contains the support information for a given locale.
;;;  PRIMOP-TABLE is a table containing all of the primops defined in the
;;;  locale.
;;;
;;;  (<table> <symbol>) => the support for <symbol> in the locale.  This
;;;      is settable.
;;;  (WALK-SUPPORT <table> <proc>) => (<proc> <symbol> <support>) for each
;;;      <symbol> in the table.
;;;  (PRIMOP-SUPPORT <table> <name>) => Returns a primop if there is one
;;;      named <name> in this table's primop table.

;(define (make-support-table id primop-table)
;  (let ((table (make-table id)))
;    (object (lambda (name)
;              (table-entry table name))
;      ((setter self)
;       (lambda (name value)
;         (set (table-entry table name) value)))
;      ((primop-support self name)
;       (primop-table name))
;      ((walk-support self proc)
;       (table-walk table proc))
;      ((walk-primop-table self proc)
;       (if primop-table
;           (walk-primop-table primop-table proc)))
;      ((support-table? self) t)
;      ((identification self) id)
;      ((print self stream)
;       (format stream "#{Support-table ~D ~S}" (object-hash self) id)))))

(defstruct env-table
  id
  table)

(zl:defsubst identification (env-table)
  (env-table-id env-table))


(defstruct (support-table (:include env-table)
                          (:constructor %make-support-table (id table primop-table))
                          (:print-function (lambda (struct stream depth)
                                             depth
                                             (format stream "#{Support-table ~D ~S}"
                                                     (support-table-id struct)
                                                     (object-hash struct)))))
  primop-table)

(defun make-support-table (id primop-table)
  (%make-support-table id (make-table id) primop-table))

(defun walk-support (support-table proc)
  (table-walk (support-table-table support-table) proc))

(zl:defsubst support-table-lookup (table name)
  (table-entry (support-table-table table) name))

(defun make-empty-support-table (id)
  (make-support-table id (make-primop-table id)))


;;; (MAKE-PRIMOP-TABLE id)
;;;============================================================================
;;;    The primops defined in a locale have a distinct name space.  This allows
;;; primops to be anonymous in the regular name space and still be referred to
 ;;; by name by the support tables and files.  Primop tables are essentially
;;; identical to support tables.

;(define (make-primop-table id)
;  (let ((table (make-table id)))
;    (object (lambda (name)
;              (table-entry table name))
;            ((setter self)
;             (lambda (name value)
;               (set (table-entry table name) value)))
;            ((walk-primop-table self proc)
;             (table-walk table proc))
;            ((print self stream)
;             (format stream "#{Primop-table ~D ~S}" (object-hash self) id)))))

(defstruct (primop-table (:include env-table)
                         (:constructor %make-primop-table (id table))
                         (:print-function (lambda (struct stream depth)
                                            depth
                                             (format stream "#{Primop-table ~D ~S}"
                                                     (primop-table-id struct)
                                                     (object-hash struct)))))
  )

(zl:defsubst primop-table-lookup (table name)
  (table-entry (primop-table-table table) name))

(defun make-primop-table (id)
  (%make-primop-table id (make-table id)))



;;; (MAKE-SUPPORT-ENV id support-tables)
;;;============================================================================
;;;  A support environment is made from a list of support tables.  It responds
;;; to information requests by trying each support table in turn.  The method
;;; for AUGMENT-CONTEXT is used by the HERALD special form.

;(define (make-support-env id support-tables)
;  (object (lambda (name)
;            (any (lambda (p)
;                   (p name))
;                 support-tables))
;   ((identification self) id)
;   ((primop-support self name)
;    (any (lambda (p)
;           (primop-support p name))
;         support-tables))
;   ((augment-context self . rest)
;    (get-support-environment id support-tables rest))
;   ((print self stream)
;    (format stream "#{Support-env ~D ~S}" (object-hash self) id))))


;;; identification should not properly be applied to these...
(defstruct (support-env (:constructor make-support-env (id support-tables))
                        (:print-function (lambda (struct stream depth)
                                           depth
                                           (format stream "#{Support-env ~D ~S}"
                                                   (support-env-id struct)
                                                   (object-hash struct)))))
  id
  support-tables)


(defun argument-context (support-env &rest rest)
  (error "mumble blugh"))

(defun make-empty-support-env (id)
  (make-support-env id '()))


;;; (MAKE-FLAT-SUPPORT-ENV id support-tables)
;;;============================================================================
;;;   Makes an environment that uses one table to hold all support definitions.
;;; Used for permanent environments.

;(define (make-flat-support-env id support-tables)
;  (receive (support-table primop-table)
;           (combine-tables id support-tables)
;    (object (lambda (name)
;              (table-entry support-table name))
;      ((primop-support self name)
;       (table-entry primop-table name))
;      ((walk-support self proc)
;       (table-walk support-table proc))
;      ((walk-primop-table self proc)
;       (table-walk primop-table proc))
;      ((identification self) id)
;      ((augment-context self . rest)
;       (get-support-environment id `(,self) rest))
;      ((print self stream)
;       (format stream "#{Flat-support-env ~D ~S}" (object-hash self) id)))))

(defstruct (flat-support-env (:include support-table)
                             (:constructor %make-flat-support-env (id table primop-table))
                             (:print-function (lambda (struct stream depth)
                                                depth
                                                (format stream "#{Flat-support-env ~D ~S}"
                                                        (support-env-id struct)
                                                        (object-hash struct)))))
  )

(defun make-flat-support-env (id support-tables)
  (multiple-value-bind (support-table primop-table)
      (combine-tables id support-tables)
    (%make-flat-support-env id support-table primop-table)))


(defun combine-tables (id support-tables)
  (let ((support-table (make-table id))
        (primop-table (make-table id)))
    (mapc #'(lambda (table)
              (walk-support table
                            #'(lambda (name support)
                                (setf (table-entry support-table name)
                                     support)))
              (walk-primop-table table
                                 #'(lambda (name primop)
                                     (setf (table-entry primop-table name)
                                           primop))))
          (reverse support-tables))
    (values support-table primop-table)))


(defun support-lookup (env-or-table name)
  (if (typep env-or-table 'support-env)
      (some #'(lambda (s) (support-lookup s name))
            (support-env-support-tables env-or-table))
    (support-table-lookup env-or-table name)))


; support-table
;      ((primop-support self name)
;       (primop-table name))
; flat-support-env
;      ((primop-support self name)
;       (table-entry primop-table name))
; support-env
;   ((primop-support self name)
;    (any (lambda (p)
;           (primop-support p name))
;         support-tables))
(defun primop-support (support-env-or-table name)
  (ecase (type-of support-env-or-table)
    (SUPPORT-TABLE
     (primop-table-lookup (support-table-primop-table support-env-or-table) name))
    (FLAT-SUPPORT-ENV
     (table-entry (flat-support-env-primop-table support-env-or-table) name))
    (SUPPORT-ENV
     (some #'(lambda (p)
               (primop-table-lookup (support-table-primop-table p) name))
        (support-env-support-tables support-env-or-table)))))

;support-table
;      ((walk-primop-table self proc)
;       (if primop-table
;           (walk-primop-table primop-table proc)))
;primop-table
;      ((walk-primop-table self proc)
;       (table-walk table proc))
;flat-support-env
;      ((walk-primop-table self proc)
;       (table-walk primop-table proc))
(defun walk-primop-table (table proc)
  (ecase (type-of table)
    (SUPPORT-TABLE
     (if (support-table-primop-table table)
         (table-walk (primop-table-table (support-table-primop-table table)) proc)))
    (PRIMOP-TABLE
     (table-walk (primop-table-table table) proc))
    (FLAT-SUPPORT-ENV
     (table-walk (flat-support-env-primop-table table) proc))))



;;; *PRIMITIVE-PRIMOP-TABLE*
;;; *PRIMITIVE-SUPPORT-TABLE*
;;; *PRIMITIVE-SUPPORT-ENV*
;;; *STANDARD-SUPPORT-ENV*
;;;===========================================================================
;;;   A starter set of tables and environments.

(defvar *primitive-primop-table*
  (make-primop-table '*primitive-primop-table*))

(defvar *primitive-support-table*
  (make-support-table '*primitive-support-table* *primitive-primop-table*))

(defvar *primitive-support-env*
  (make-support-env '*primitive-support-env* `(,*primitive-support-table*)))

(defvar *standard-support-env* (make-empty-support-env '*empty-env*)) ;lset

;;; SUPPORT
;;;============================================================================
;;;   Structure to hold support information for a symbol in a particular
;;; environment.

(defstruct (support (:constructor make-support (variable table data variant value type))
                    (:print-function (lambda (struct stream depth)
                                       depth
                                       (format stream "#{Support ~S}" (object-hash struct)))))
  variable  ; The variable being supported.
  table     ; The support table this support is in.
  variant   ; What kind of support this is, one of 'DEFINE etc.
  value     ; The value VARIABLE is bound to if VARIABLE is integrable.
  type      ; The type of the value VARIABLE is bound to.
  data      ; Not currently used.
  )

(defun make-support-entry (var table data variant value type)
  (let ((s (make-support var table data variant value type)))
    (setf (variable-support var) s)
    (setf (support-table-lookup table (variable-name var)) s)
    s))

;;; (CREATE-SUPPORT shape new-support support)
;;;============================================================================

;(define (create-support shape new-support support)
;  (table-walk (shape-table shape)
;              (lambda (key val)
;                (ignore key)
;                (if (or (cdr val)
;                        (variable-binder (car val)))
;                    (bug "lexical variable ~S still in shape ~S" val shape))))
;  (walk (lambda (var)
;          (find-var-support var
;                            (support (variable-name var))
;                            (new-support (variable-name var))
;                            new-support))
;        (shape-defined shape))
;  (shape-defined shape))

(defun get-variable-support (variable)
  (let ((support (variable-support variable)))
    (let ((value (and support
                      (eq 'constant (support-variant support))
                      (support-value support))))
      (if (and value (variable-p value))
          (get-variable-support value)
            support))))

;;; (SUPPORTS-DEFINITION ref)
;;;============================================================================
;;;  A predicate that determines if REF is the support value of some variable.
;;; Returns the support variant if it exists.

(defun supports-definition (ref)
  (and (call-arg? (node-role ref))
       (let ((proc (call-proc (node-parent ref))))
         (if (and (eq (call-arg 2) (node-role ref))
                  (primop-node? proc)
                  (primop.defines-support? (primop-value proc)))
             (primop.support-variant (primop-value proc))
             nil))))

;;; (FIX-MULTIPLE-DEFINITIONS var)
;;;============================================================================
;;;  Replace all more permanent definitions of VAR with DEFINE.

(defun fix-multiple-definitions (var)
  (orbit-warning "~S is multiply defined" (variable-name var))
  (dolist (ref (variable-refs var))
    (let ((variant (supports-definition ref)))
      (if (eq 'constant variant)
          (setf (primop-value (call-proc (node-parent ref)))
               primop/*define)))))

;;;

(defun weaken-support (table)
  (walk-support table
                #'(lambda (name support)
                    (ignore name)
                    (cond ((eq 'constant (support-variant support))
                           (setf (support-value support) nil)
                           (setf (support-variant support) 'define)))))
  (values))

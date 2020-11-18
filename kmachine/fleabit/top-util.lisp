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

;;; Why not GC::OBJECT-HASH? - JRM
(defun object-hash (object)
  (zl:%pointer object))

(defmacro bpt ()
  '(cerror "continue from break" "BPT"))

(defmacro make-table (&rest random-args) `(make-hash-table))

(defmacro table-entry (table key)
  `(gethash ,key ,table))

(defmacro syntax-table-entry (table key)
  `(table-entry ,table ,key))

(defun make-syntax-table (syntax-table name)
  (let ((new-table (make-table name)))
    (if syntax-table
        (maphash #'(lambda (key value)
                     (setf (gethash key new-table) value))
                 syntax-table))
    new-table))

(defmacro table-push (table key value)
  `(push ,value (table-entry ,table ,key)))

(defmacro table-pop (table key)                 ;???
  `(pop (table-entry ,table ,key)))


(defmacro table-walk (table proc)
  `(maphash ,proc ,table))

(defmacro concatenate-symbol (&rest things)
  `(intern (apply #'concatenate 'string (mapcar #'string (list . ,things)))))


(defmacro destructure (specs &body body)
  (let (let-forms once-only-forms)
    (labels ((dest (pattern value)
               (when pattern
                 (cond ((symbolp pattern)
                        (push (list pattern value) let-forms))
                       (t
                        (dest (car pattern) `(car ,value))
                        (dest (cdr pattern) `(cdr ,value)))))))
      (dolist (spec specs)
        (let ((value (cadr spec))
              var)
          (unless (zl:atom value)
            (push (list (setq var (gensym)) value) once-only-forms)
            (setq value var))
          (dest (car spec) value)))
      `(let ,(nreverse once-only-forms)
         (let ,let-forms
           ,@body)))))


(defmacro map! (function list)
  (let ((subl-var (gensym)))
    `(mapl #'(lambda (,subl-var)
               (rplaca ,subl-var (funcall ,function (car ,subl-var))))
           ,list)))



;;; Messages
;;;==========================================================================

(defvar *notes* t)

(defmacro note (string)
  `(eval-when (compile)
     (note-1 ,string)))

(defun note-1 (string)
  (terpri *notes*)
  (format *notes* "~&*** NOTE: ~a" string))

(defun bug (f &rest rest)
  (apply #'error "~@? (compiler-bug)" f rest))

(defun orbit-warning (f &rest rest)
  (apply #'warn f rest))

(defun syntax-error (f &rest rest)
  (apply #'warn f rest))

(defvar *debug-stream* t)
(defvar *debug-flag* ())
(defvar *all-debug-types* '(:alpha         ;Alphatized
                            :unsimplified  ;After node conversion
                            :simp          ;simplifications
                            :simp-tree     ;simplified tree after each simplification
                            :simplified    ;simplified node tree
                            :strategy      ;node tree with strategy annotations
                            :used          :node tree with used variables
                            :live          ;node tree with live variables
                            :regs          ;preference classes and variable assignments
                            :generate      ;generated code tree
                            :comments      ;assembly comments
                            :emit          ;emitted (non-optimized) code
                            :post          ;post processed code
                             ))

(defmacro debug (type &body body)
  (pushnew type *all-debug-types*)
  `(when (member ,type *debug-flag*)
     ,@body))

(defmacro debug-msg (type format-string &rest args)
  (pushnew type *all-debug-types*)
  `(debug ,type
     (format *debug-stream* ,format-string . ,args)))

(defun debug-on (&rest types)
  (dolist (type types)
    (case type
      (:all (setq *debug-flag* *all-debug-types*))
      (t (pushnew type *debug-flag*))))
  *debug-flag*)

(defun debug-off (&rest types)
  (dolist (type types)
    (case type
      (:all (setq *debug-flag* '()))
      (t (setq *debug-flag* (delete type *debug-flag*)))))
  *debug-flag*)

(defun orbit-debug (&rest args)
  (if *debug-flag*
      (apply #'format *terminal-io* args)))

(defun print-variable-info (vars)
  (multiple-value-bind (f e d l s u) (sort-variables vars)
    (let ((stream (if *noise-flag* *noise+terminal* *noise-stream*)))
      (format stream "~&(VARIABLES~%")
      (format stream "  FREE ~S~%" (mapc #'variable-name f))
      (format stream "  EARLY-BOUND ~S~%" (mapc #'variable-name e))
      (format stream "  DEFINED ~S~%" (mapc #'variable-name d))
      (format stream "  LSET ~S~%" (mapc #'variable-name l))
      (format stream "  SET ~S~%" (mapc #'variable-name s))
      (format stream "  UNREFERENCED ~S)~%" (mapc #'variable-name u)))))

#||||
(defun sort-variables (vars)
  (iterate loop ((v vars) (f '()) (e '()) (d '()) (l '()) (s '()) (u '()))
    (cond ((null? v)
           (values f e d l s u))
          (else
           (let ((var (car v)))
             (cond ((not (variable-support var))
                    (loop (cdr v) (cons var f) e d l s u))
                   ((not (defined-variable? var))
                    (loop (cdr v) f (cons var e) d l s u))
                   ((eq? (defined-variable-variant var) 'set)
                    (loop (cdr v) f e d l (cons var s) u))
                   (else
                    (let ((u (if (null? (cdr (variable-refs var)))
                                 (cons var u)
                                 u)))
                      (xcase (defined-variable-variant var)
                        ((lset)
                         (loop (cdr v) f e d (cons var l) s u))
                        ((define constant)
                         (loop (cdr v) f e (cons var d) l s u)))))))))))
||||#

;;; (PP-CPS node &key stream extra)
;;;===========================================================================
;;; Print CPS node tree in linear form.  Stream defaults to terminal output.

(defun pp-cps (node &key (stream t)
                         (extra #'(lambda (node)
                                    (declare (ignore node))
                                    "")))
  (pp-cps-1 node 2 stream extra))

(defun pp-cps-1 (node indent-to stream extra-fcn)
  (let ((z (pp-cps-2 node)))
    (cond ((lambda-node? node)
           (pp-cps-lambda node indent-to stream extra-fcn))
;          ((object-node? node)
;           (pp-cps-object node indent-to stream))
          )
    z))

(defun pp-cps-lambda (node indent-to stream extra-fcn)
  (let ((vars (lambda-all-variables node)))
    (format stream "~&~V,1@T(~A~VT"
            indent-to
            (mapcar #'variable-unique-name vars)
            (+ indent-to 18))
    (format stream "~A" (pp-cps-2 (lambda-body node)) stream))
    (format stream ")  ~40T~A~%" (funcall extra-fcn node))
    (pp-cps-list (call-proc+args (lambda-body node)) indent-to stream extra-fcn))

;(defun pp-cps-object (node indent-to stream)
;  (format stream "~S~VT(@~S"
;         (object-hash node)
;         indent-to
;         (object-number node)
;         (+ indent-to 18))
;  (pprint `(,(pp-cps-2 (object-proc node))
;           ,(mapc #'pp-cps-2 (object-operations node))
;           ,(mapc #'pp-cps-2 (object-methods node)))
;         stream)
;  (format stream ")~%")
;  (pp-cps-list (object-proc-pair node) indent-to stream)
;  (pp-cps-list (object-operations node) indent-to stream)
;  (pp-cps-list (object-methods node) indent-to stream))

(defun pp-cps-list (list indent-to stream extra)
  (dolist (node list)
    (pp-cps-1 node (1+ indent-to) stream extra)))

(defun pp-cps-2 (node)
  (cond ((empty? node)
         node)
        ((not (node-p node))
         `(not-a-node ,node))
        (t
         (typecase node
           (lambda-node
            (format nil "^~a" (lambda-name node)))
           (leaf-node
            (case (leaf-variant node)
                  (literal
                   (format nil "'~s" (literal-value node))
                   ; `',(literal-value node)
                   )
                  (primop
                   (if (primop? (primop-value node)) ; Hack for DK's LAP junk.
                       (concatenate-symbol
                        '$
                        (primop.variant-id (primop-value node)))
                       (concatenate-symbol '$_
                                           (object-hash (primop-value node)))))
                  (otherwise
                   (variable-unique-name (reference-variable node)))))
           (call-node
            (let ((stuff (mapcar #'pp-cps-2 (call-proc+args node))))
              `(,(car stuff) ,(call-exits node) . ,(cdr stuff))))
;           (object-node
;            (concatenate-symbol '@ (object-number node)))
           ))))

(defun lambda-name (node)
  (and (lambda-self-var node)
       (format nil "~a_~a"
               (variable-name (lambda-self-var node))
               (variable-id (lambda-self-var node)))))

;;; Returns a lexically unique name for the variable.

(defun variable-unique-name (var)
  (cond ((variable-p var)
         (let ((name (variable-name var)))
;           (cond ((variable-binder var)
                  (format nil"~a_~a" name (variable-id var)))
;                 (t
;                  name))))
         )
        ((primop? var)
         (identification var))
        (t
         var)))


;;; PRNODE

(defun prnode (node &optional (stream t))
  "Print out all information on node and subnodes."
  (format stream "~&")
  (prnode-1 node 0 stream))

(defun prnode-1 (node indent stream)
  (cond ((node-p node)
         (cond ((call-node? node)
                (format stream "~VTCALL:~%" indent)
                (format stream "~vT  PROC:" indent)
                (prnode-1 (call-proc node) (+ indent 8) stream)
                (format stream "~&")
                (format stream "~vT  ARGS:" indent)
                (mapc #'(lambda (arg)
                          (prnode-1 arg (+ indent 8) stream)
                          (format stream "~%"))
                      (call-args node)))
               ((lambda-node? node)
                (format stream "~vtLAMBDA:~%" indent)
                (format stream "~vt  STRATEGY: ~s~%" indent (lambda-strategy node))
                (format stream "~vt  LIVE:     ~s~%" indent (lambda-live node))
                (format stream "~vt  ENV:      ~s~%" indent (lambda-env node))
                (format stream "~vt  ALL-VARS:" indent)
                (mapc #'(lambda (arg)
                          (prnode-1 arg (+ indent 12) stream)
                          (format stream "~%"))
                      (lambda-all-variables node))
                (format stream "~vt  BODY:" indent)
                (prnode-1 (lambda-body node) (+ indent 12) stream))
               (t (format stream "~vT~a" indent node))))
        (t (format stream "~vT~a" indent node))))


;;; Little utilities.
;;;========================================================================

#||||||||||||||||
(define (find pred l)
  (iterate loop ((l l))
    (cond ((null? l) nil)
          ((pred (car l)) (car l))
          (else (loop (cdr l))))))

(define (filter pred l)
  (iterate loop ((l l) (r '()))
    (cond ((null? l) (reverse! r))
          ((pred (car l)) (loop (cdr l) (cons (car l) r)))
          (else (loop (cdr l) r)))))

(define (filter! pred list)
  (iterate filter! ((list list))
    (cond ((null-list? list) '())
          ((pred (car list)) (set (cdr list) (filter! (cdr list))) list)
          (else (filter! (cdr list))))))

(define (select-from-table pred table)
  (let ((res '()))
    (table-walk table
                (lambda (key entry)
                  (if (pred key entry)
                      (push res `(,key . ,entry)))))
    res))

(define (table->list table)
  (select-from-table true table))

(define (table-push table key val)
  (modify (table-entry table key)
          (lambda (old)
            (if old
                (cons val old)
                (list val))))
  val)

(define (table-pop table key)
  (pop (table-entry table key)))

(define (free-table-push table key val)
  (modify (table-entry table key)
          (lambda (old)
            (if old
                (cons-from-freelist val old)
                (cons-from-freelist val '()))))
  val)

(define (free-table-pop table key)
  (free-pop (table-entry table key)))

(define (merge-lists x y)
  (cond ((null? y) x)
        (else (do ((z x (cdr z))
                   (u y (let ((w (car z)))
                          (if (memq? w u) u (cons w u)))))
                  ((null? z) u)))))




||||||||||||||||#

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

;;; Checking for user errors and gathering information...

;;; Needs to deal with object nodes

(defvar *definitions* '())
(defvar *uses* '())

;;; Return lists of variable definitions and uses.
;;;   *DEFINITIONS* is a list of the variables defined in the given node tree.
;;;   *USES* is a list of uses in the following form:
;;;     (<variable> <location> <use>)
;;;   <location> is either 'TOP or the variable whose value contains the use.
;;;   <use> is one of 'CALL-ARG 'OPERATION, '(CALL . <number of arguments>),
;;;     or 'WEIRD.

(defun def-and-use-analyze (node)
  (let ((*definitions* '())
        (*uses* '()))
    (check-call-node (lambda-body node) 'top)
    (values *definitions* *uses*)))

;;; Check a value node - look at the body of lambdas and the support of
;;; variables.

(defun check-value-node (node loc)
  (if (lambda-node? node)
      (check-call-node (lambda-body node) loc)
    (let ((support (and (reference-node? node)
                        (variable-support (reference-variable node)))))
      (if support
          (if (eq *new-support* (support-table support))
              (push `(,(support-variable support) ,loc ,(node-use node))
                    *uses*))))))

(defun check-value-list (list loc)
  (dolist (node list)
    (check-value-node node loc)))

;;; Check calls.  Checks the procedure and then dispatches on the type of
;;; the call.

(defun check-call-node (call loc)
  (check-call-using-proc call loc)
  (cond ((variable-definition? call)
         (check-value-node (call-arg-n 1 call) loc)
         (check-value-node (call-arg-n 3 call)
                           (reference-variable (call-arg-n 2 call)))
         (add-definition-value call loc))
        ((primop-ref? (call-proc call) primop/y)
         (check-y (call-arg-n 1 call) loc))
        ((not (eq loc 'top))
         (check-value-list (call-proc+args call) loc))
        ((= 0 (call-exits call))
         (check-value-node (call-proc call) 'top)
         (check-value-list (call-args call) nil))
        ((= 1 (call-exits call))
         (check-value-node (call-proc call) nil)
         (check-value-node (call-arg-n 1 call) 'top)
         (check-value-list (cdr (call-args call)) nil))
        (t
         (check-value-list (call-proc+args call) nil)))
  (clear-check-flags call))

;;; Does CALL define a variable.

(defun variable-definition? (call)
  (let ((primop (known-primop (call-proc call))))
    (and primop
         (primop.defines-support? primop)
         (= 3 (length (call-args call)))
         (reference-node? (call-arg-n 2 call))
         (variable-support (reference-variable (call-arg-n 2 call))))))

;;; Set the support value and type of a variable.

(defun add-definition-value (call loc)
  (destructure (((proc nil ref val) (call-proc+args call)))
    (let* ((var (reference-variable ref))
           (support (variable-support var))
           (variant (primop.support-variant (known-primop proc))))
      (pushnew var *definitions* :test #'eq)
      (if (eq variant (support-variant support))
          (add-support-value var val loc))
      (if (member variant '(define constant) :test #'eq)
          (setf (support-type support)
                (get-support-type val))))))   ;(call-arg-n 3 call)))))))

(defun add-support-value (var val loc)
  (let* ((support (variable-support var))
         (val (and (eq 'constant (support-variant support))
                   (not (primop? (support-value support)))
                   (eq 'top loc)
                   (node->vector val))))
    (when val
      (setf (support-value support) val)
      t)))

;;; Type check of a call using the type of the procedure.

(defun check-call-using-proc (node loc)
  (let ((proc (call-proc node)))
    (cond ((literal-node? proc)
           (fix-call-to-literal node (literal-value proc)))
          ((reference-node? proc)
           (check-call-to-var node (reference-variable proc) loc))
          ((lambda-node? proc)
           (if (arg-check-of-lambda proc node)
               (set-check-flags (lambda-variables proc) (call-args node))
               (fix-call-to-lambda node proc))))))

;;; Special procedure for checking calls to Y.

(defun check-y (l-node loc)
  (let ((vals (mapcar #'thunk-value (cdr (call-args (lambda-body l-node))))))
    (set-check-flags (cdr (lambda-variables l-node)) vals)
    (cond ((eq loc 'top)
           (check-value-node (call-arg-n 1 (lambda-body l-node)) 'top)
           (check-value-list vals nil))
          (t
           (check-value-node (call-arg-n 1 (lambda-body l-node)) loc)
           (check-value-list vals loc)))
    (reset-check-flags (lambda-variables l-node))))

;;; Variables that have known values keep those values in the VARIABLE-FLAG
;;; field for the purposes of type checking.

(defun clear-check-flags (node)
  (if (lambda-node? (call-proc node))
      (reset-check-flags (lambda-variables (call-proc node)))))

(defun set-check-flags (vars args)
  (mapc #'(lambda (var val)
            (if (and var val (lambda-node? val))
                (setf (variable-flag var) val)))
        vars
        args))

(defun reset-check-flags (vars)
  (mapc #'(lambda (var)
            (if var (setf (variable-flag var) nil)))
        vars))

;;; Checking a call to a known variables

(defun check-call-to-var (call var loc)
  (if (variable-binder var)
      (check-call-to-lexical-var call var loc)
    (let ((support (get-variable-support var)))
      (if (and support (not (eq *new-support* (support-table support))))
          (check-call-to-bound-var call var support loc)))))

(defun check-call-to-lexical-var (call var loc)
  (let ((type (variable-flag var)))
    (cond ((not (node-p type))
           nil)
          ((lambda-node? type)
           (if (not (arg-check-of-lambda type call))
               (fix-call-to-bound-lambda call var type))))))

(defun check-call-to-bound-var (call var support loc)
  (let ((type (support-type support)))
    (cond ((eq type 'literal)
           (fix-call-to-support-literal (call-proc call))
           (replace-with-free-variable (call-proc call)))
          ((and (consp type)
                (eq (car type) 'proc)
                (not (arg-check-of-type type call)))
           (fix-call-to-support-proc (call-proc call))))))

(defun arg-check-of-lambda (proc node)
  (let ((left-over (- (length (call-args node))
                      (length (lambda-variables proc)))))
    (or (zerop left-over)
        (and (> left-over 0)
             (lambda-rest-var proc)))))

(defun arg-check-of-type (type node)
  (let ((left-over (- (length (call-args node))
                      (caddr type))))
    (or (zerop left-over)
        (and (> left-over 0)
             (cadr type)))))

;;; The way in which a node is used.

(defun node-use (node)
  (let ((role (node-role node)))
    (cond ((eq role call-proc)
           `(call . ,(length (call-args (node-parent node)))))
;          ((object-op? role) 'operation)
          ((call-arg? role) 'call-arg)      ;node???
          (t 'weird))))

(defun use-type (use)
  (support-type (variable-support (car use))))

(defun check-uses (vars new-uses old-uses)
  (let ((left (remove-if #'use-type new-uses)))
    (dolist (use old-uses)
      (let ((type (use-type use)))
        (if type
            (check-variable-use use type)
          (push use left))))
    left))

(defun check-variable-use (use var-type)
  (destructure (((var loc use-type) use))
    (cond ((or (not var-type)
               (eq use-type 'call-arg)
               (eq use-type 'weird))
           t)
          ((eq use-type 'operation)
           t)  ; Operations are not annotated yet
          ((or (not (consp use-type))
               (not (eq 'call (car use-type))))
           (bug "unknown use-type ~S in CHECK-VARIABLE-USE" use-type))
          ((eq var-type 'literal)
           (user-message 'warning
                         loc
                         "call to ~S which is bound to a literal"
                         nil
                         (variable-name var)))
          ((and (consp var-type)
                (eq (car var-type) 'proc))
           (if (not (arg-check-of-use var-type use-type))
               (user-message 'warning
                             loc
                             "wrong number of arguments in a call to ~A"
                             nil
                             (variable-name var)))))))

(defun arg-check-of-use (var-type use-type)
  (let ((left-over (- (cdr use-type)
                      (caddr var-type))))
    (or (zerop left-over)
        (and (> left-over 0)
             (cadr var-type)))))

;;; Quick version of the above.  Just finds defs and uses.  This is used on
;;; integrable definitions before they are simplified.

(defun quick-def-and-use-analyze (node)
  (let ((uses '()))
    (labels ((tree-walk (node)
               (if (lambda-node? node)
                   (mapc #'tree-walk (call-proc+args (lambda-body node)))
                 (let ((support (and (reference-node? node)
                                     (variable-support (reference-variable node)))))
                   (if (and support
                            (eq *new-support* (support-table support))
                            (not (member (support-variable support) uses :test #'eq)))
                       (push (support-variable support) uses))))))
      (tree-walk (call-arg-n 3 (lambda-body node))))
    (values (reference-variable (call-arg-n 2 (lambda-body node))) uses)))

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

;;; Simplifying LET nodes, i.e. any call with a lambda node as the procedure.
;;; Only a small percentage of these are actually created by LET.

;;; First pass at simplifying a LET node.  This does as many substitutions as
;;; possible without simplifying the body of PROC.  If any arguments survive
;;; REALLY-SIMPLIFY-LET is called.

(defun simplify-let (proc call)
  (cond ((not (arg-check-of-lambda proc call))
         (mapl #'simplify (call-args call)) ; Just looking for more errors
         (simplify-call proc)
         (warn "Wrong number of args to a lambda function")
         nil)
        ((and (null (lambda-variables proc))
              (null (lambda-rest-var proc)))
         ;; *** should this be here, or should
         ;; *** the open not be generated??
         (bash-open call)

         (replace-node call (detach (lambda-body proc)))
         t)
        (t
         (setf (node-simplified? call) t)
         (remove-unused-arguments proc call)
         (mapl #'simplify (call-args call))
         (quick-substitute-arguments proc call)
         (if (remove-unused-let call proc)
             t
             (really-simplify-let proc call)))))

;;; Simplify the body of PROC and then try substituting the arguments again.
;;; If all the arguments can be substituted the call node is removed.
;;; CHANGE? indicates that the let node has been moved in the tree and thus
;;; its parent must resimplify.

(defun really-simplify-let (proc call)
  (do ((change? nil))
      (())
    (setf (node-simplified? proc) t)
    (simplify-call proc)
    (substitute-arguments proc call)
    (cond ((substitute-join-arguments proc call)
           (setq change? t))
          ((not (node-simplified? proc)))
          (t
           (remove-unused-arguments proc call)
           (return (not (setf (node-simplified? call)
                              (not (or (remove-unused-let call proc)
                                       change?)))))))))

(defun remove-unused-let (call proc)
  (cond ((null (call-args call))
         (if (lambda-rest-var proc)
             (walk-refs-safely (lambda (ref)
                                 (replace-node ref (create-literal-node '())))
                               (lambda-rest-var proc)))
         ;; *** should this be here, or should
         ;; *** the open not be generated??
         (bash-open call)
         (replace-node call (detach (lambda-body proc)))
         t)))

;;; Removed unused arguments to a lambda node in call position.  Destructively
;;; changes the lists of arguments and variables.

(defun remove-unused-arguments (node call)
  (if (and (lambda-rest-var node)
           (not (used? (lambda-rest-var node))))
      (setf (cadr (lambda-all-variables node)) nil))
  (do ((vars (lambda-rest+variables node))
       (args (call-proc+args call))
       (exits (call-exits call) (1- exits))
       (n 2))
      ((null (cdr vars))
       (cond ((not (used? (lambda-rest-var node)))
              (setf (cadr (lambda-all-variables node)) nil)
               (mapc #'erase-all (cdr args))
               (setf (cdr args) '()))))         ; Extremely Evil
    (cond ((used? (cadr vars))
           (setf (variable-number (cadr vars)) n)
           (setf (node-role (cadr args)) (call-arg (1- n)))
           (pop vars) (pop args) (incf n))
          (t
           (debug :sub
             (format t "~%Removing: ~s" (cadr vars)))
           (setf (cdr vars) (cddr vars)) ; Evil
           (erase-all (cadr args))
           (setf (cdr args) (cddr args)) ; Extremely Evil
           (if (< 0 exits)
               (decf (call-exits call)))))))

(defun quick-substitute-arguments (node call)
  (do ((vars (lambda-rest+variables node))
       (args (call-proc+args call))
       (exits (call-exits call) (1- exits))
       (n 2))
      ((null (cdr vars))
       nil)
    (cond ((quick-substitute (cadr vars) (cadr args) n)
           (setf (cdr vars) (cddr vars)) ; Evil
           (setf (cdr args) (cddr args)) ; Extremely Evil
           (if (< 0 exits)
               (decf (call-exits call))))
          (t
           (pop vars) (pop args) (incf n)))))

;;; Set the index

(defun quick-substitute (var val n)
  (setf (variable-number var) n)
  (setf (node-role val) (call-arg (1- n)))
  (cond ((substitute? var val)
         (substitute-value var val t)
         t)
        (t nil)))

;;; Is it okay to substitute VAL for variable VAR.

(defun substitute? (var val)
  (and ;; can't substitute for setqed vars
       (null (variable-setqs var))
       ;; can't substitute for special vars
       (null (variable-special-p var))
       (or (and (reference-node? val)
                (let ((value-var (reference-variable val)))
                  (not (or
                         ;; can't substitute in a value that can change
                         (variable-setqs value-var)
                         ;(var-setqed-in-scope-of-var value-var var)
                         (variable-special-p value-var)
                         ;; Prohibit substitution of global registers for now.
                         ;; It tends to break such code as
                         ;; '(prog1 gr::foo (incf gr::foo))
                         ;; - JRM 15-Mar-87 20:56:42
                         ;; This may not be the best way to do it, but it works.
                         (variable-global-p value-var)))))
           (literal-node? val)
           (primop-node? val))))


;;; This is hard to do here, because it is not the scope of SCOPE-VAR
;;; we are worried about, but the extent.  If var is setqed within
;;; the extent, then we can't substitute.  This is fairly easy to
;;; determine if live variable analysis has been done, but not before.

;;; Return T if VAR is setqed anywhere within the scope of SCOPE-VAR
;(defun var-setqed-in-scope-of-var (var scope-var)
;  (and (variable-setqs var)
;       (let ((var-binder (variable-binder var))
;            (scope-binder (variable-binder scope-var)))
;        (dolist (setq (variable-setqs var))
;          (do ((parent (node-parent setq)
;                       (node-parent (node-parent parent))))
;              ((eq parent var-binder) nil)
;            ;; if the setq is inside a subroutine,
;            ;; wimp out and return T
;            (let ((proc (call-proc (node-parent parent))))
;              (when (or (lambda-node? proc)
;                        (primop-ref? proc primop/Y))
;                (return-from var-setqed-in-scope-of-var t)))
;            (when (eq parent scope-binder)
;              (return-from var-setqed-in-scope-of-var t)))))))

;;; Try to substitute any arguments to LAMBDA-PROC that are lambda nodes.
;;; Three different methods are tried.  SUBSTITUTE-LAMBDA? checks that there
;;; is only one reference to the variable and the reference is in call or
;;; exit position or only one call down in the tree.  PARTIAL-SUBSTITUTE?
;;; determines whether it is worth duplicating the argument to do the
;;; substitution.

(defun substitute-arguments (lambda-proc call-node)
  (mapc #'(lambda (var val)
            (cond ((not (used? var)) nil)  ; VAL may be *EMPTY* if VAR is unused
                  ((reference-node? val)
                   (partial-substitute-variable var (reference-variable val)))
                  ((not (lambda-node? val))
;                       (or (object-node? val)
;                           (lambda-node? val)))
                   nil)
                  ((substitute-lambda? var)
                   (substitute-value var val t))
                  ((and (lambda-node? val)
                        (partial-substitute-lambda? val))
                   (partial-substitute var val))))
        (lambda-variables lambda-proc)
        (call-args call-node)))

;;; Return T if there are any special bindings in effect at
;;; references to VAR that were not in effect when VAR was bound.
(defun special-bindings-in-effect? (var)
  (let ((refs (variable-refs var))
        (binder (variable-binder var)))
    (some #'(lambda (ref)
              (do ((lambda (node-parent (node-parent ref))
                           (node-parent (node-parent lambda))))
                  (())
                (when (some #'(lambda (var) (and var (variable-special-p var)))
                            (lambda-rest+variables lambda))
                  (return t))
                (when (eq lambda binder)
                  (return nil))))
          refs)))

(defun strong-substitute-lambda? (var)
  (and (null (variable-setqs var))
       (null (cdr (variable-refs var)))
       (not (special-bindings-in-effect? var))))


(defun weak-substitute-lambda? (var)
  (and (null (cdr (variable-refs var)))
       (let ((ref (car (variable-refs var))))
         (or (eq (node-role ref) call-proc)
             (call-exit? ref)
             (eq (variable-binder var) (node-parent (node-parent ref)))))))

(zl:deff substitute-lambda? #'strong-substitute-lambda?)

(defun partial-substitute-variable (var val)
  (let ((call (lambda-body (variable-binder var))))
    (walk-refs-safely #'(lambda (ref)
                          (if (eq call (node-parent ref))
                              (replace-node ref (create-reference-node val))))
                      var)))

;;; Is VAL a simple enough lambda-node to be integrated.
(note "PARTIAL-SUBSTITUTE-LAMBDA? nullified because it is
   causing conts of GO's to be different causing STRATEGY/PROC")

(defun partial-substitute-lambda? (val) nil)
;  (and (or (not (lambda-rest-var val))
;           (null (variable-refs (lambda-rest-var val))))
;       (every #'(lambda (n)
;                 (not (lambda-node? n)))
;               (call-proc+args (lambda-body val)))
;;       (let ((proc (call-proc (lambda-body val))))
;;         (not (and (reference-node? proc)
;;                   (let ((var (reference-variable proc)))
;;                     (and (variable-support var)
;;                          (eq (support-variant (variable-support var))
;;                               'constant))))))
;       ))


;;;      (eq? (variable-name var '*proclaim))
;;;      =>
;;;      (variable-binder (reference-variable (call-exit call)))
;;;        is CALL-PROC or argument to PRIMOP/Y
;;;  i.e. don't duplicate calls to unknown procedures whose continuations are
;;;  calls to known procedures

;;; Substitute VAL (a lambda-node) for VAR everywhere that it can be
;;; integrated.  References in non-call position need to be consed anyway
;;; so there is no reason not to substitute them as well.

(defun partial-substitute (var val)
  (cond ((some #'(lambda (ref)
                   (eq (node-role ref) call-proc))
               (variable-refs var))
         (substitute-value var val t))))


;;; Simplifying joins...

;;; This code is attempting, in the much more intractable CPS domain,
;;; to perform the optimization:
;;;  (IF (IF a b c) d e) => (IF a (IF b d e) (IF c d e))

(defun substitute-join-arguments (lambda-proc call)
  (do ((vars (lambda-variables lambda-proc) (cdr vars))
       (vals (call-args call) (cdr vals))
       (change? nil))
      ((null vars) change?)
    (cond ((and (used? (car vars)) ; (CAR VALS) may be *EMPTY* if VAR is unused
                (lambda-node? (car vals))
                (join-substitute (car vars) (car vals)))
           (setq change? t)))))

(defun join-substitute (var val)
  (let ((calls (mapcar #'get-simple-cond-call (lambda-variables val))))
    (dolist (ref (variable-refs var) nil)
      (let ((call (and (eq call-proc (node-role ref))
                       (call-and-literal-match calls
                                               (call-args (node-parent ref))))))
        (when (and call
                   (parameterize val call))
          (debug :simp
            (format *debug-stream* "Substituting Join: ~a := ~a"
                    (variable-unique-name var) (pp-cps-2 val)))
          (walk-refs-safely
            #'(lambda (ref)
                (if (eq call-proc (node-role ref))
                    (replace-node ref (copy-node-tree val))))
            var)
          (debug :simp-tree
            (pp-cps (node-base val)))
          (return t))))))

(defun call-and-literal-match (calls args)
  (do ((calls calls (cdr calls))
       (args args (cdr args)))
      ((or (null args)
           (and (car calls)
                (literal-node? (car args))))
       (if (null calls) nil (car calls)))))

(defun get-simple-cond-call (var)
  (and (variable-p var)
       (some #'simple-cond-ref (variable-refs var))))

(defun simple-cond-ref (ref)
  (let ((call (node-parent ref)))
    (and (= 2 (call-exits call))
         (destructure (((cond? nil nil true? t-ref)
                        (call-proc+args call)))
           (and (primop-ref? cond? primop/conditional)
                (primop-ref? true? primop/true?)
                (eq ref t-ref)
                call)))))


;;; *******************
;;; this was in analyze

(defun arg-check-of-lambda (proc node)
  (let ((left-over (- (length (call-args node))
                      (length (lambda-variables proc)))))
    (or (zerop left-over)
        (and (> left-over 0)
             (lambda-rest-var proc)))))

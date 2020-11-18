;;; -*- Mode:LISP; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Register Allocation

;;; To do:
;;; interference (conflict) graph of preference classes
;;; then do graph coloring to assign registers
;;;
;;; (define-global-variable frame *foo*)
;;; (defun foo () (setq *foo* (logand 255. (1+ *foo*))))
;;;   temps (results of 1+ and logand) should be allocated to *foo*
;;;


(defvar *preference-classes* ())
(defvar *stack-slots* 0
  "Number of stack slots used by the current HEAP or PROC lambda")

(zl:defsubst lambda-open (node)
  (lambda-db node))

(zl:defsubst lambda-stack-slots (node)
  (lambda-db node))

(defun hack-open (lambda-node open-call nested-opens)
  (setf (lambda-open lambda-node) open-call)
  (let ((call (lambda-body lambda-node)))
    (cond ((primop-node? (call-proc call))
           (let ((proc (leaf-value (call-proc call))))
             (cond
               ((eq proc primop/open-frame)
                ;; push open
                (push open-call nested-opens)
                (setq open-call (leaf-value (call-arg-n 2 call))))
               ((eq proc primop/y)
                (setq call (lambda-body (call-arg-n 1 call)))))))
          (;; primops are not opened
           (eq call open-call)
           ;; pop open
           (setq open-call (pop nested-opens))))
    (dolist (arg (call-proc+args call))
      (if (lambda-node? arg)
          (case (lambda-strategy arg)
            ((STRATEGY/HEAP STRATEGY/PROC)
             (hack-open arg nil nil))
            (t (hack-open arg open-call nested-opens)))))))


(defmacro pc-target (pc)
  `(car ,pc))

(zl:defsubst pc-vars (pc)
  (cdr pc))

(defmacro make-pc (target &rest vars)
  `(list ,target ,@vars))

(defun pref-class (var)
  (or (find var *preference-classes*
            :test #'member
            :key  #'pc-vars)
      (if (variable-loc var)
          (create-pref-class var (variable-loc var)))))


(defun create-pref-class (var target)
  (let ((pc (make-pc target var)))
    (push pc *preference-classes*)
    pc))

(defun add-to-pref-class (var pc)
  (if (var-admissible-p var pc)
     (push var (pc-vars pc))
    (create-pref-class var '*)))   ;???

(defun same-open-as-pc-p (var pc)
  (let ((open (lambda-open (variable-binder (car pc)))))
    (and (eq open (lambda-open (variable-binder var)))
         (every #'(lambda (ref) (eq open (lambda-open (node-parent ref))))
                (variable-refs var)))))

(defun var-admissible-p (var pc)
  (and (if (O-reg-p (pc-target pc))
           (same-open-as-pc-p var pc)
         t)
       (dolist (pc-var (pc-vars pc) t)
         (if (and (coextant-p var pc-var)
                  (not (can-be-same var pc-var)))
             (return nil)))))

;;; CAN-BE-SAME is true
;;;   if V1 is bound to V2
;;;      and there are no setqs of V1
;;;      V2 is not special or global
;;;      and there are no setqs of V2 when V1 is live
;;;
;;; This would be much better done by the simplification
;;; phase if SUBSTITUTE? could return T for V1 and V2.
;;; However, we need live variable analysis to determine this
;;; and analysis is difficult to make work before simplification
;;;
(defun can-be-same (v1 v2)
  (and (null (variable-setqs v1))
       (not (variable-special-p v2))
       (not (variable-global-p v2))
       (let ((binder (variable-binder v1)))
         (and (eq (node-role binder) call-proc)
              (let ((val (call-arg-n (1- (variable-number v1))
                              (node-parent binder))))
                (and (reference-node? val)
                     (eq v2 (reference-variable val))
                     (every #'(lambda (setq-call)
                                (not (member v1 (continuation-live (call-arg-n 1 setq-call)))))
                            (variable-setqs v2))))))))


(defun continuation-live (cont)
  (cond ((lambda-node? cont)
         (lambda-live cont))
        ((reference-node? cont)
         (continuation-live (variable-known (reference-variable cont))))
        (t nil)))


(defun immediately-set-p (maybe-set-var var var-binder)
  (let* ((call (lambda-body var-binder))
         (proc (call-proc call)))
    (and (primop-ref? proc primop/setq-lexical)
         (eq (reference-variable (call-arg-n 2 call))
             maybe-set-var)
         (let ((value (call-arg-n 3 call)))
           (and (reference-node? value)
                (eq (reference-variable value) var)
                (not (member var (continuation-live (cont call)))))))))



(defun coextant-p (var1 var2)
  (let ((vb1 (variable-binder var1))
        (vb2 (variable-binder var2)))
    (cond ((eq var1 var2) nil)
          ((eq vb1 vb2))
          ;; globals are always extant unless they are about to be setqed
          ;; (locals don't get on lambda-live if they are setqed)
          ((null vb1)
           (and vb2
                (not (immediately-set-p var1 var2 vb2))))
          ((null vb2)
           (and vb1
                (not (immediately-set-p var2 var1 vb1))))
          (t
           (or (live-in-extent-p var1 var2 vb2)
               (live-in-extent-p var2 var1 vb1))))))

;;; Non-null if var1 is live in the extent of var2.
;;; ie if var1 is live at a point where var2 is bound or set
(defun live-in-extent-p (var1 var2 &optional (vb2 (variable-binder var2)))
  (or (member var1 (lambda-live vb2))
      (some #'(lambda (setq)
                (let ((cont (get-continuation setq)))
                  (and cont (member var1 (lambda-live cont)))))
            (variable-setqs var2))))

(defun get-continuation (call)
  (let ((cont (cont call)))
    (cond ((lambda-node? cont) cont)
          ((reference-node? cont) (variable-known cont))
          (t (bug "bad cont: ~s" cont)))))


;;; This is not used yet
;;; It could be used to build the conflict graph
;;; It is very slow
(defun pref-classes-conflict-p (pc1 pc2)
  (dolist (var (pc-vars pc1) nil)
    (unless (var-admissible-p var pc2)
      (return t))))

(defun combine-pref-classes (pc1 pc2)
  (if (< (length pc1) (length pc2))             ;not necc
      (rotatef pc1 pc2))
  (let ((new-target (reconcile-targets (pc-target pc1) (pc-target pc2))))
    (when (and new-target
               (every #'(lambda (var)
                          (var-admissible-p var pc1))
                      (pc-vars pc2)))
      (setq *preference-classes* (delete pc2 *preference-classes*))
      (setf (pc-vars pc1) (nconc (pc-vars pc2) (pc-vars pc1)))
      (setf (pc-target pc1) new-target)
      ;;when not should we leave the classes alone
      ;;or shuffle somehow?
      )))

(defun functional-dest-p (target)
  (or (and (symbolp target)
           (get target 'functional-dest-p))
      (and (consp target)
           (get (car target) 'functional-dest-p))))


;;; RECONCILE-TARGETS
;;;
;;;           IGNORED *  A* On An
;;;
;;; IGNORED   IGNORED *  A* On An
;;;      *       *    *  A* On An
;;;     A*       A*   A* A* A* An
;;;     On       On   On A* =? An
;;;     An       An   An An An =?
(defun reconcile-targets (t1 t2)
  (cond ((eql t1 t2) t1)
        ((eq t1 IGNORED) t2)
        ((eq t2 IGNORED) t1)
        ;; (reconcile-targets R2 '*) should be *
        ((R-reg-p t1) t2)
        ((R-reg-p t2) t1)
        ((functional-dest-p t1)
         (if (functional-dest-p t2)
             '*
           t2))
        ((eq t1 '*) t2)
        ((eq t2 '*) t1)
        ((A-reg-p t1)
         (if (or (not (A-reg-p t2))
                 (= t1 t2))
           t1
           nil))
        ((A-reg-p t2)
         (if (or (not (A-reg-p t1))
                 (= t2 t1))
           t2
           nil))
        ((or (eq t1 'A*)
             (eq t2 'A*))
         'A*)
        (t (bug "different O regs") '*)))


(defun update-target (pc target)
  (let ((new-target (reconcile-targets (pc-target pc) target)))
    (if new-target
        (progn (if (and (O-reg-p new-target)
                        (not (all-same-open-p pc)))
                   (setq new-target '*))
               (setf (pc-target pc) new-target))
      (bug "irreconcilable targets"))))

(defun all-same-open-p (pc)
  (let ((open (lambda-open (variable-binder (car (pc-vars pc))))))
    (every #'(lambda (var)
               (and (eq open (lambda-open (variable-binder var)))
                    (every #'(lambda (ref)
                               (eq open (lambda-open (node-parent (node-parent ref)))))
                           (variable-refs var))))
           (pc-vars pc))))


(defun target-var-to-var (var tvar &optional
                                   (pc1 (pref-class var))
                                   (pc2 (pref-class tvar)))
  (cond ((and (null pc2)
              (null pc1))
         (add-to-pref-class
           tvar
           (create-pref-class var '*))) ;???
        ((eq pc1 pc2))
        ((null pc2)
         (add-to-pref-class tvar pc1))
        ((null pc1)
         (add-to-pref-class var pc2))
        (t (combine-pref-classes pc1 pc2))))


(defun next-reg (reg)
  (if (= reg AN)
      STACK-0
    (1+ reg)))

(defun reg-alloc-top (node)
  (hack-open node nil nil)
  (reg-alloc node))

(defun reg-alloc (node)
  (ecase (lambda-strategy node)
    ((STRATEGY/HEAP STRATEGY/PROC)
     ;; Heaped lambdas take their args in
     ;; the standard arg regs
     ;; Procs don't really have to but
     ;; it's more efficient in K machine
     (let ((*preference-classes* nil)
           (*stack-slots* 0))
       (let ((rest (lambda-rest-var node)))
         (if rest
             (target-var rest node)))
       (let ((next-reg (target-args (lambda-variables node))))
         (reg-alloc-call (lambda-body node))
         (reg-assign next-reg))
       (setf (lambda-stack-slots node) *stack-slots*)))
    (STRATEGY/OPEN
     ;; this is a continuation for a call
     ;; if it takes 1 arg as usual (call returns 1 value)
     ;; it will be in wherever it wants it because
     ;; the destination will be the cont var loc
     ;; (is this always a cont to an opened call?
     ;; most primops can also return in wherever we want
     ;; but some might not?)
     ;; (can also be con't to label call but that can
     ;; also go where it wants)
     (dolist (var (lambda-rest+variables node))
       (if var (target-var var node)))
     (reg-alloc-call (lambda-body node)))
   (STRATEGY/LABEL
     ;; Labels take their args wherever they like
    (let ((rest (lambda-rest-var node)))
      (if rest
          (target-var rest node)))
    (dolist (var (lambda-variables node))
      (target-var var node))
    (reg-alloc-call (lambda-body node)))))


(defun reg-alloc-call (call)
  (if (eq (call-proc call) primop/y)
      (setq call (lambda-body (call-arg-n 1 call))))
  (dolist (arg (call-proc+args call))
    (when (lambda-node? arg)
      (reg-alloc arg))))

(defun target-args (vars)
  ;; continuation is ignored
  ;; (setf (variable-loc (car vars)) IGNORED) would do.
  ;; vars can be NIL when the continuation to a heap lambda
  ;; is unused (dynamic return from or go)
  (when vars (create-pref-class (car vars) IGNORED))
  (do ((reg A0 (next-reg reg))
       (vars (cdr vars) (cdr vars)))
      ((null vars)
       reg)
    (create-pref-class (car vars) reg)   ; or update-target if class
    (dolist (ref (variable-refs (car vars) ))
      (let ((proc (call-proc (node-parent ref))))
        (when (lambda-node-p proc)
          (target-var-to-var (car vars)
                            (nth (1- (call-arg-number (node-role ref)))
                                 (lambda-variables proc))))))))

(defun target-var (var lambda-node)
  (let ((refs (variable-refs var)))
    (if refs
        (dolist (ref (variable-refs var))
          (let ((pc (pref-class var))
                (target (ref-target ref lambda-node)))
            (if (variable-p target)
                (target-var-to-var var target pc)
              (if pc
                  (update-target pc target)
                (create-pref-class var target)))))
      (if (and (variable-special-p var)
               ;; is this safe?
               (not (pref-class var)))
          ;; this is very temp but can't be R reg
          ;; because bind is a call...
          (create-pref-class var 'A*)
        (setf (variable-loc var) IGNORED)))))

;;; RETURN the place where the given reference
;;; would like to find its value
;;; returns another variable, an O register
;;; * for anywhere (we really don't care)
;;; A* for any A reg
;;; IGNORED for nowhere
(defun ref-target (ref lambda-node)
  (let* ((parent (node-parent ref))
         (proc (call-proc parent))
         (number (call-arg-number (node-role ref))))
    (cond ((<= number (call-exits parent))
           IGNORED)
          ((primop-node? proc)
           (let ((primop (primop-value proc)))
             (cond (;; if the reference is the value of a setq
                    ;; which is the first call
                    (and (or (eq parent (lambda-body lambda-node))
                             (null (variable-setqs (reference-variable ref))))
                         (eq primop primop/setq-lexical)
                         (= number 3))
                    ;; target the variable to the setq's var
                    (reference-variable (call-arg-n 2 parent)))
                   (;; if the reference is the value to write to a functional dest
                    (and (eq parent (lambda-body lambda-node))
                         (eq primop primop/write-functional-dest)
                         (primop-node? (call-proc (node-parent lambda-node)))
                         ;; only used once
                         (null (cdr (variable-refs (reference-variable ref)))))
                    ;; target the variable to the functional-dest
                    (leaf-value (call-arg-n 2 parent)))
;                  (;; try to put a primops arg where it's result goes??
;                   (let ((cont (call-arg-n 1 parent)))
;                     (if (lambda-node? cont)
;                         (let ((cvar (car (lambda-variables cont))))
;                           (if (and cvar
;                                    ;; no other var wants it
;                                    (every #'(lambda (arg)
;                                               (or (literal-node? arg)
;                                                   (and (reference-node? arg)
;                                                        (eq (reference-variable arg)
;                                                            (reference-variable ref)))))
;                                           (cdr (call-args parent))))
;                               cvar)))))
                   (;; only used once, immediately in primop
                    (and (eq parent (lambda-body lambda-node))
                         (null (cdr (variable-refs (reference-variable ref)))))
                    ;; very temporary, (skip R0 and R1)
                    (+ R0 (variable-number (reference-variable ref))))
                   (t '*))))
          ((lambda-node? proc)
           (nth (1- number) (lambda-variables proc)))
          (t
           (let ((label (variable-known (leaf-value proc))))
             (cond
               (label
                (cond ((eq ref proc)
                       ;; this var is bound to a proc
                       (case (lambda-strategy label)
                         (STRATEGY/LABEL
                          ;;This will be a jump, no var needed
                          IGNORED)
                         (t (bug "reference is a call-proc non LABEL"))))
                      (t
                       (ecase (lambda-strategy label)
                         ;; calling known procedure,
                         ;; put var where proc wants it
                         (STRATEGY/LABEL
                          (or (nth (1- number) (lambda-variables label))
                              (let ((rest-var (lambda-rest-var label)))
                                (cond ((null rest-var)
                                       (bug "wrong # args in reg-alloc"))
                                      ((null (variable-refs rest-var))
                                       IGNORED)
                                      (t 'A*)))))
                         (STRATEGY/OPEN
                          ;; This happens when a continuation to a label call
                          ;; is set as the known value of the labels continuation
                          ;;  (foo (do (...
                          ;; it might be more tasteful to change the open to a label
                          ;; but then allocation screws up because there is no
                          ;; continuation arg...
                          (nth (1- number) (lambda-variables label)))
                         ((STRATEGY/HEAP strategy/proc)
                          (if (eq parent (lambda-open lambda-node))
                              (- (+ (1- number) O0)
                                 (call-exits parent))
                            'A*))))))
               ;; unknown proc, return open reg corresponding
               ;; to arg position of ref
               ((eq parent (lambda-open lambda-node))
                (- (+ (1- number) O0)
                   (call-exits parent)))
               (t 'A*)))))))


(defun reg-assign (next-reg)
  (debug :pref-classes
    (format t "~%Register preference classes:")
    (dolist (pc *preference-classes*)
      (format t "~%(~a " (pc-target pc))
      (dolist (var (pc-vars pc))
        (format t " ~a" (variable-unique-name var)))
      (format t ")")))
  (debug :regs
    (format t "~%Register Assignments:"))
  (dolist (pc *preference-classes*)
    (let ((target
            (if (or (eq (pc-target pc) '*)
                    (eq (pc-target pc) 'A*))
                (prog1 (setf (pc-target pc) next-reg)
                       (setq next-reg (next-reg next-reg)))
              (pc-target pc))))
      (if (and (integerp target) (>= target STACK-0))
          (setq *stack-slots*
                (max *stack-slots* (1+ (- target STACK-0)))))
      (dolist (var (pc-vars pc))
        (debug :regs
          (format t "~%~a: ~a" (variable-unique-name var) target))
        ;; something is screwing this up
        (if (and (eq (variable-name var) 'gr:*save-trap-pc*)
                 (integerp target))
            (error "LOSE! LOSE! LOSE! setting variable-loc of GR:*SAVE-TRAP-PC*"))
        (setf (variable-loc var) target)))))


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;;; (loc . vars)
;;;
;;; loc is the most general of the hierarchy a*,*,ignore
;;; vars are preference class
;;;
;;;




;;; live...
var is live if it is used below this lambda

except vars are also live accross calls to
bound vars


extant is
   union of live
     and if bound proc
          live of continuations of calls


this is one version of live, meaning "which variables can't be clobbered by this code
because we need them later"

 v1 v2 conflict if v1 is live in binder of v2 or vice verse
     (or (member v1 (lambda-live (variable-binder v2)))
         (member v2 (lambda-live (variable-binder v1))))



another version of live is, "which variables will be used within this code"
this is the reverse, live is union of live and live of any called procedures

lambda-used-in-scope =? lambda-live-in-scope
lambda-used-in-extent
lambda-live-in-extent


B basic block
x is live at p if the value of x at p could be used
along some path in the flow graph starting at p

(in B) = set of variables live immediately before block B
(out B) = set of variables live immediately after B
(def B) = set of variables assigned values in B prior to any use in B
(use B) = set of variables whose values may be used in B prior to any definition

(in B)  = (union (use B) (set-difference (out B) (def B)))
(out B) = (apply #'union (mapcar #'in (succesors-of B)))



(defun foo ()
  (do ((a 0 b)
       (b 1 a))
      (())
    (xxx a b)))

--------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

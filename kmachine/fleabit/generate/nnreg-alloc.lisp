;;; -*- Mode:LISP; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Register Allocation

;;; To do:
;;; interference (conflict) graph of preference classes
;;; then do graph coloring to assign registers
;;;
;;; init stack slots
;;;
;;; live

(zl:defsubst lambda-open (node)
  (lambda-env node))

(defun hack-open (lambda-node open-call nested-opens)
  (setf (lambda-open lambda-node) open-call)
  (let ((call (lambda-body lambda-node)))
    (cond ((primop-node? (call-proc call))
           (let ((proc (leaf-value (call-proc call))))
             (cond
               ((eq proc primop/open)
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

(defvar *preference-classes* ())

(defmacro pc-target (pc)
  `(car ,pc))

(zl:defsubst pc-vars (pc)
  (cdr pc))

(defmacro make-pc (target &rest vars)
  `(list ,target ,@vars))

(defun pref-class (var)
  (find var *preference-classes*
        :test #'member
        :key  #'pc-vars))

(defun create-pref-class (var target)
  (let ((pc (make-pc target var)))
    (push pc *preference-classes*)
    pc))

(defun add-to-pref-class (var pc)
  (if (var-admissible-p var pc)
     (push var (pc-vars pc))
    (create-pref-class var '*)))   ;???

(defun var-admissible-p (var pc)
  (dolist (pc-var (pc-vars pc) t)
    (if (coextant-p var pc-var)
        (return nil))))

(defun coextant-p (var1 var2)
  (let ((vb1 (variable-binder var1))
        (vb2 (variable-binder var2)))
    (and (not (eq var1 var2))
         (or (eq vb1 vb2)                               ;???
             ;; this is a different lambda-live than presently exists
             (member var1 (lambda-live vb2))
             (member var2 (lambda-live vb1))))))


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


;;;          IGNORE * A* On An
;;;
;;; IGNORE   IGNORE * A* On An
;;;      *     *    * A* On An
;;;     A*     A*  A* A* A* An
;;;     On     On  On A* =? An
;;;     An     An  An An An =?
(defun reconcile-targets (t1 t2)
  (cond ((eq t1 'IGNORE) t2)
        ((eq t2 'IGNORE) t1)
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
        ((eql t1 t2) t1) ;put this first?
        (t (cerror "foo" "different O regs") '*)))

(defun A-reg-p (reg)
  (and (integerp reg)
       (>= reg A0)
       (<= reg AN)))


(defun update-target (pc target)
  (let ((new-target (reconcile-targets (pc-target pc) target)))
    (if new-target
        (setf (pc-target pc) new-target)
      (cerror "foo" "irreconcilable targets"))))



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


(defconstant STACK-0 100)

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
     (let ((*preference-classes* nil))
       (let ((next-reg (target-args (cdr (lambda-variables node)))))
         (reg-alloc-call (lambda-body node))
         (reg-assign next-reg))))
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
     (dolist (var (cdr (lambda-variables node)))
       (target-var var node))
     (reg-alloc-call (lambda-body node)))))


(defun reg-alloc-call (call)
  (if (eq (call-proc call) primop/y)
      (setq call (lambda-body (call-arg-n 1 call))))
  (dolist (arg (call-proc+args call))
    (when (lambda-node? arg)
      (reg-alloc arg))))

(defun target-args (vars)
  (do ((reg A0 (next-reg reg))
       (vars vars (cdr vars)))
      ((null vars)
       reg)
    (create-pref-class (car vars) reg)))  ; or update-target if class

(defun target-var (var lambda-node)
  (dolist (ref (variable-refs var))
    (let ((pc (pref-class var))
          (target (ref-target ref lambda-node)))
      (if (variable-p target)
          (target-var-to-var var target pc)
        (if pc
            (setf (pc-target pc) (reconcile-targets target (pc-target pc)))
          (create-pref-class var target))))))

;;; Return the place where the given reference
;;; would like to find its value
;;; returns another variable, an O register
;;; * for anywhere (we really don't care)
;;; A* for any A reg
;;; IGNORE for nowhere
(defun ref-target (ref lambda-node)
  (let* ((parent (node-parent ref))
         (proc (call-proc parent))
         (number (call-arg-number (node-role ref))))
    (cond ((<= number (call-exits parent))
           'IGNORE)
          ((primop-node? proc)
           (let ((primop (primop-value proc)))
             (cond (;; if the reference is the value of a setq
                    ;; which is the first call
                    (and (eq parent (lambda-body lambda-node))
                         (eq primop primop/setq-lexical)
                         (= number 3))
                    ;; target the variable to the setq's var
                    (reference-variable (call-arg-n 2 parent)))
                 (t ;; try to put a primops arg where it's result goes??
                  (if ;; unless result is also an arg
                    ;; check if all other args are literal nodes
                    ;; (could be cleverer)
                    (every #'(lambda (arg)
                               (or (literal-node? arg)
                                   (and (reference-node? arg)
                                        (eq (reference-variable arg)
                                            (reference-variable ref)))))
                           (cdr (call-args parent)))
                    (let ((cont (call-arg-n 1 parent)))
                      (if (lambda-node? cont)
                          (let ((cvar (car (lambda-variables cont))))
                            (or cvar '*))
                        '*))
                    '*)))))
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
                          'IGNORE)
                         (t (cerror "foo" "reference is a call-proc non LABEL"))))
                      (t
                       (ecase (lambda-strategy label)
                         ;; calling known procedure,
                         ;; put var where proc wants it
                         (STRATEGY/LABEL
                          (let ((label-var (nth (1- number) (lambda-variables label))))
                            (or label-var
                                (progn
                                  (setq label-var (lambda-rest-var label))
                                  (if (and label-var
                                           ;; ignored rest
                                           (null (variable-refs label-var)))
                                      'IGNORE
                                    (cerror "foo" "non ignored rest or bad # args in reg-alloc"))))))
                         (STRATEGY/OPEN
                          ;; This happens when a continuation to a label call
                          ;; is set as the known value of the labels continuation
                          ;;  (foo (do (...
                          ;; it might be more tasteful to change the open to a label
                          ;; but then allocation screws up because there is no
                          ;; continuation arg...
                          (nth (1- number) (lambda-variables label)))
                         (STRATEGY/PROC
                          (cerror "Foo" "allocating arg to STRATEGY/PROC call"))))))
               ;; unknown proc, return open reg corresponding
               ;; to arg position of ref
               ((eq parent (lambda-open lambda-node))
                (- (+ (1- number) O0)
                   (call-exits parent)))
               (t 'A*)))))))

(defvar *stack-slots* 0)

(defun reg-assign (next-reg)
  (debug :regs
    (format t "~%Register preference classes:")
    (dolist (pc *preference-classes*)
      (format t "~%(~a " (pc-target pc))
      (dolist (var (pc-vars pc))
        (format t " ~a" (variable-unique-name var)))
      (format t ")")))
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

(defun fact (n)
  (labels ((fact1 (m a)
             (if (= m 0) a
               (fact1 (* m a)          ;(1- m)
                      (* m a)))))
    (fact1 n 1)))


((...) (1+ ^c1 n))
 ((c1 v2) (do333 k3 v2))  ;n not live

((...) (- ^c1 m 1)
 ((c1 v2) (* ^c3 m a))   ;m,a live
  ((c3 v4) (fact1 k0 v2 v4)))








----------------

1st scheme looked one level deep
returning as target for a  reference
  i)   a real place (On)
  ii)  a specifier (* and later A*)
  iii) a variable

this loses because returning a variable does not
work if that var has been(/will be?) allocated to
someplace which conflicts with another var

as a special case of that it lost on loops
a targetted to b and b targetted to a

it then tried to pick a good place from the list of reference targets

---

2nd scheme recursed on targetting to var
this seems really wrong because we might known
nothing about other vars bound at the same time as
var...



----

preference class? interference graph?


(defun foo (n m)
  (let ((l (do ((i 10. (1- i))
                (a () (setq a (cons () a))))
               ((= i 0) a))))
    (print l)))

(defun foo (n m)
  (do ((i 0 (1+ i)))
      ((= i n))
    (print i)))


(defun foo ()
  (do ((a 0 b)
       (b 1 a))
      (())
    (xxx a b)))

--------
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

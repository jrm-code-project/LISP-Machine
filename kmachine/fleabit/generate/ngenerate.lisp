;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Code Generation

(defvar *assembly-comments?* nil)
(defvar *lambda-queue* '())         ;; queue of lambda bodies to process
;(defvar *stack-pos* 0)              ;; distance of stack-pointer from "frame"
(defvar *lambda* nil)               ;; the procedure being compiled

(zl:defsubst cont (node)
  (car (call-args node)))

(zl:defsubst then-cont (node)
  (car (call-args node)))

(zl:defsubst else-cont (node)
  (cadr (call-args node)))

(defun lambda-queue (node)
  (pushnew node *lambda-queue*))

(defun lambda-queue-at-end (node)
  (unless (member node *lambda-queue*)
    (setq *lambda-queue* (nconc *lambda-queue* (list node)))))

(defun generate (top-node)
  (let ((top (car (call-args (lambda-body top-node))))) ;??
    (reg-alloc-top top)
    (let ((*instructions* '()))
      (generate-code-top top)
      (let ((insts (post-process *instructions*)))
        (debug :post
          (format t "~&~%Post Processed:~%")
          (print-instructions insts))
        insts))))

(defvar *lambdas-generated-crock*)

(defun generate-code-top (top)
  (debug :generate
    (format t "~&Generating: ~&") (pp-cps top))
  (let ((*lambda-queue* '())
        (*lambdas-generated-crock'()))
    (generate-lambda top)
    (do ()
        ((null *lambda-queue*))
      (generate-lambda (pop *lambda-queue*)))))


(defun generate-lambda (node)
;  (setq *stack-pos* 0)
  (push node *lambdas-generated-crock*)
  (emit-tag node)   ;do something with HEAP lambdas
  (if (lambda-rest-var node)
      (generate-cons-rest node))
  (generate-call (lambda-body node)))


(defmacro ass-comment (string &rest rest)
  `(if *assembly-comments?*
       (emit-comment (format nil ,string ,@rest))))


;(defun variable-known (var)
;  (when (variable-p var)
;    (let ((type (variable-type var)))
;      (cond ((and (node-p type)
;                 (lambda-node? type))
;            ;; ((lambda (var) ...) type)  - fix later for labels
;            ;;  (eq? (node-parent (variable-binder var))
;            ;;     (node-parent type))
;            type)))))

(defun variable-known (var)
  (let ((type (variable-type var)))
    (if (node-p type)
        (cond ((lambda-node? type)
               type)
              ((reference-node? type)
               (variable-known (reference-variable type)))))))

(defun generate-call (node)
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (ass-comment "~s" (pp-cps node))
           (gen-primop-call node))
          ((lambda-node? proc)
           (generate-let node))
          (t
           (let ((proc (variable-known (leaf-value proc))))
             (cond (proc
                    (ass-comment "Call known procedure ~s"
                                 (cons (lambda-name proc) (cdr (pp-cps node))))
                    (cond ((zerop (call-exits node))
                           (generate-known-return node proc))
                          ((= (call-exits node) 1)
                           (generate-known-call node proc))
                          (t (error "Call exits not 0 or 1 in ALLOCATE-CALL"))))
                   ((zerop (call-exits node))
                    (ass-comment "Return from procedure ~s" (pp-cps node))
                    (gen-return node))
                   ((= (call-exits node) 1)
                    (ass-comment "Call unknown procedure ~s" (pp-cps node))
                    (gen-general-call node))
                   (t
                    (bug "too many exits - ~s" node))))))))


;;; This could be called with either
;;; a lambda or reference
;;; the reference can be an arg (in which case we return)
;;; or bound (we jump to value)
;;;
;;; this is sort of like allocate-conditional-continuation
;;; and allocate-label-return
;;; it also kind of takes the place of fetch-continuation...
;;;
;;; it is possible that this has to deal with join-point ... etc
;;; and with arguments to the continuation ?
(defun generate-continuation-call (cont)
  (cond ((lambda-node? cont)
         (??)) ;; these may always get hacked by hoisted cont?
        ((reference-node? cont)
         (let ((proc (variable-known (reference-variable cont))))
           (cond (proc
                  ;; do we know that it's strategy label?
                  (unless (member proc *lambdas-generated-crock*)
                    (lambda-queue proc))
                  (generate-jump proc))
                 (t (generate-return 1)))))    ; 1?
        (t (bug "Weird continuation"))))


;;; this is a lot like generate-known-call
;;; this can also be called instead of generate-known-call
;;; when calling without any cont?
;;; (ie in go)
(defun generate-known-return (node proc)
  ;; this only wants to be done once
  ;; was done by set-join-state
  ;; (which had something set in the join which was in the lambda env)
  (unless (member proc *lambdas-generated-crock*)
    (lambda-queue proc))
  (parass node (call-args node) (lambda-variables proc))
  (generate-jump proc))


(defun gen-return (node)
  (parass node (call-args node) *return-registers*)
;  (fetch-continuation node (call-proc node))   ;for popping the stack
  (generate-return (length (call-args node))))


(defun gen-general-call (node)
  (let* ((cont (call-arg-n 1 node))
         (var-cont (not (lambda-node? cont)))     ;this is all gross
         (tail-p (and var-cont
                      (not (variable-known (reference-variable cont))))))
;    (if (not var-cont) (lambda-queue cont))
    (parass node (cdr (call-args node)) *open-regs*)
    (generate-general-call (call-proc node)
                           tail-p
                           (continuation-expecting cont)
                         (- (length (call-args node)) 2))  ;flush cont also flush P
    (cond ((and var-cont (not tail-p))
           (generate-continuation-call cont))
          ((not var-cont)
           (if (cdr (lambda-variables cont))
               (generate-mv-bind cont))
           (lambda-queue cont)))))




(defun generate-known-call (node proc)
  (if (not (or (lambda-rest-var proc)
               (= (length (lambda-variables proc))
                  (length (call-args node)))))
      (warning "Wrong number of arguments in call ~s" (pp-cps node)))
  ;; hack entry points
  (ecase (lambda-strategy proc)
    (STRATEGY/LABEL (generate-label-call node proc))
    (STRATEGY/PROC (generate-proc-call node proc))
    (STRATEGY/HEAP (generate-known-heap-call node proc))))

;;; this does not queue its continuation
;;; because a label call is just a jump
;;; (all calls have the same continuation
;;; so the continuation will be generated where
;;; the proc returns)
(defun generate-label-call (node proc)
  (unless (member proc *lambdas-generated-crock*)
    (lambda-queue proc))
  (parass node (cdr (call-args node)) (cdr (lambda-variables proc)))
  (if (lambda-rest-var proc)
      ;;; not quite
      (generate-move (length (call-args node)) NARGS))
  (if (<= (lambda-trace proc) (lambda-trace (node-parent node)))
      (generate-avoid-jump proc)
      (generate-jump proc)))

;;; this is like gen-general-call
(defun generate-proc-call (node proc)
  (unless (member proc *lambdas-generated-crock*)
    (lambda-queue-at-end proc))
  (let* ((cont (call-arg-n 1 node))
         (var-cont (not (lambda-node? cont)))   ;this is all gross
         (tail-p (and var-cont
                      (not (variable-known (reference-variable cont))))))
;    (if (not var-cont) (lambda-queue cont))
    (parass node (cdr (call-args node)) *open-regs*)
    (generate-general-call (get-tag proc)
                           tail-p
                           (continuation-expecting cont)
                           (- (length (call-args node)) 2))     ;flush cont also flush P
    (cond ((and var-cont (not tail-p))
           (generate-continuation-call cont))
          ((not var-cont)
           (if (cdr (lambda-variables cont))
               (generate-mv-bind cont))
           (lambda-queue cont)))))

(defun read-acc (ref)
  (acc ref nil))

(defun write-acc (ref)
  (acc ref t))

(defun acc (ref write-p)
  (cond ((integerp ref) ref)
        ((reference-node? ref)
         (acc-var (reference-variable ref) write-p))
        ((literal-node? ref)
         (if (eq (leaf-value ref) undefined)
             undefined
           ref))
        ((variable-p ref)
         (acc-var ref write-p))
        ((or (listp ref) (symbolp ref)) ref)
        ((lambda-node? ref)
         (cerror "foo" "make closure"))
        (t (bug "Trying to access value of: ~s" ref))))


(defun acc-var (var write-p)
  (let ((loc (variable-loc var)))
    (cond ((symbolp loc) loc) ;IGNORE, globals, functional s/d
          ((variable-p loc)
           (cerror "foo" "this shouldn't happen anymore"))
          ((< loc STACK-0)
           loc)
          (t (if write-p
                 (write-stack-slot (- loc STACK-0))
               (read-stack-slot (- loc STACK-0)))))))



;;; this is not right, we need to check for conflicts
;;; and we also need to know all the registers...
(defun parass (node args registers)
;  (let ((closure (get-closure args)))
;    (if closure
;       (make-heap-closure node closure))
    ;; this of course needs to be fixed
    ;; to really parallel assign
    (do ((args args (cdr args))
         (regs registers (cdr regs)))
        ((or (null args) (null regs)))
      (let ((arg (car args)))
        (generate-move arg (car regs)))))


;(defun get-closure (args)
;  (some #'(lambda (arg)
;           (and (lambda-node? arg)
;                (not (eq (environment-closure (lambda-env arg)) *unit*))
;                (environment-closure (lambda-env arg))))
;       args))


(defun gen-primop-call (node)
  (let* ((prim (primop-value (call-proc node))))
    (cond ((primop.conditional? prim)
           (primop.generate prim node))
          ((primop.special? prim)
           (primop.generate prim node))
          (t
           (really-gen-primop-call node prim)))))

(defun really-gen-primop-call (node prim)
  (let ((c (cont node)))
    (cond ((lambda-node? c)
           ;; what is this cruft?
;           (let ((cont (call-hoisted-cont node)))
;            (when cont
;              (mapc #'(lambda (a-pair)
;                        (or (member (car a-pair) (lambda-live c) :test #'eq)
;                            (zerop (variable-number (car a-pair)))
;                            (some #'(lambda (node)
;                                      (and (leaf-node? node)
;                                           (eq (leaf-value node) (car a-pair ))))
;                                  (cdr (call-args node)))
;                            (kill (car a-pair))))
;                    (closure-env (environment-closure (lambda-env cont))))))
           (primop.generate prim node)
           (generate-call (lambda-body c)))
          (t
           (primop.generate prim node)
           (generate-continuation-call c)))))



;----------------------------------------------------------------
;;;; Labels

;;; this is the primop.generate function for $Y
;;; it generates the body and creates closures (?)
;;; but does not generate the label functions
;;; they are queued by set-join-state at the
;;; first call to them
(defun generate-labels (node)
  (destructure (((body . procs) (call-args (lambda-body (call-arg-n 1 node)))))
    (ecase (lambda-strategy (call-arg-n 1 node))
      ((STRATEGY/HEAP)
       (generate-heap-labels node body procs))
;     ((STRATEGY/VFRAME STRATEGY/EZCLOSE)
;      (generate-vframe-labels node (call-arg-n 1 node)))
      ((STRATEGY/LABEL)))
    (generate-call (lambda-body body))))

;(defun generate-vframe-labels (node vframe)
;  (cond ((null (lambda-env vframe))
;         (mapc #'lambda-queue (cdr (call-args (lambda-body vframe)))))
;        ((eq (lambda-strategy vframe) strategy/vframe)
;         (make-vframe-closure node
;                              vframe
;                              (environment-closure (lambda-env vframe))
;                               nil)
;         (free-register node P)
;         (generate-move-address (reg-offset SP 2) P)
;         (mark (lambda-self-var vframe) P))
;        (t (mapc #'lambda-queue (closure-vframe-lambdas (environment-closure (lambda-env vframe)))))))
;;           (make-vframe-closure node
;;                              vframe
;;                              (environment-closure (lambda-env vframe))
;;                              t))))



;;; Let

;;; GENERATE-LET&LABELS Divide up the procedures depending on whether they
;;; need to be closed or can be jumped to.

(defun generate-let (node)
  (destructure (((body . exprs) (call-proc+args node)))
    (let ((vars '())
          (lambdas '()))
      (dolist (expr exprs)
        (cond ((not (lambda-node? expr))
               (push expr vars))
              ((eq (lambda-strategy expr) STRATEGY/LABEL)
               (push expr lambdas))
              (t
               ;; ??? orbit lossage  -efh
               (push expr lambdas))))
      (really-generate-let node body vars lambdas))
    (generate-call (lambda-body body))))


;; ************** the variables of leaves MUST BE FREE. They should have been
;; substituted.  Otherwise there would be aliasing.
;; ??? so?  can't substitute if setqs

(defun really-generate-let (node body leaves closures)
  (labels ((bind-leaf (leaf)
;            (if (variable-binder (leaf-value leaf))
;                (bug "lexical variable being bound by LET ~s" (leaf-value leaf)))
            (let* ((var (nth (- (call-arg-number (node-role leaf)) 1)
                             (lambda-variables body))))
              (generate-move leaf var))))
    (cond ((not closures)
           (mapc #'bind-leaf leaves))
          ((eq (lambda-strategy (car closures)) STRATEGY/LABEL)
           (mapc #'bind-leaf leaves))
          ((eq (lambda-strategy (car closures)) STRATEGY/STACK)
           (cond ((cdr closures) (bug "too many stack closures in let" node))
                 (t
                  (setf (lambda-strategy body) strategy/stack)
                  (setf (lambda-env body) (lambda-env (car closures)))
                  (mapc #'bind-leaf leaves))))
          (t
           (hair)
           (cond ((get-member closures)
                  => (lambda (member)
           (let ((closure (environment-closure (lambda-env member))))
             (make-heap-closure node closure)
             (lock AN)
             (walk (lambda (var)
                     (let ((reg (get-register 'pointer node '*))
                           (offset (cdr (assq var (closure-env closure)))))
                       (generate-move-address (reg-offset AN offset) reg)
                       (mark var reg)))
                    (filter (lambda (closure)
                              (memq? closure (lambda-live body)))
                            (cdr (closure-members closure))))
             (mapc #'bind-leaf leaves)
             (unlock AN)
             (mark (car (closure-members closure)) AN)))))
           (walk (lambda (closure)
                   (lambda-queue closure)
                   (mark (lambda-self-var closure)
                         (->register 'pointer node closure '*)))
                 (filter (lambda (l)
                           (eq? (environment-closure (lambda-env l))
                                *unit*))
                         closures))))))


;----------------------------------------------------------------


(defun generate-nil-test (arg)
  (emit-alu 'K:L-R 'K:NOOP-NO-OVERFLOW-TRAP arg ''nil))

(defun read-stack-slot (slot)
  (emit-alu 'K:L+R 'K:VMA-START-READ
            'K:*STACK-POINTER*
            `',(- *stack-slots* slot))
  (emit 'K:NOOP)
  'K:READ-MD)

(defun write-stack-slot (slot)
  (emit-alu 'K:L+R 'K:WRITE-VMA-BOXED    ;?? 'K:VMA
            'K:*STACK-POINTER*
            `',(- *stack-slots* slot))
  'K:MD-START-WRITE)


;;; Primop Generation

(defun cont-var (cont)
  (or (car (lambda-variables cont))
      (lambda-rest-var cont)))


;;; Return the register in which CONT
;;; is expecting its value
(defun continuation-expecting (cont)
  (cond ((lambda-node? cont)
         (let ((cvar (cont-var cont)))
           (if cvar
               (write-acc cvar)
             'IGNORE)))   ;continuation has no vars?
        (t (let ((proc (variable-known (reference-variable cont))))
             (if proc    ;bound continuation
                 (continuation-expecting proc)
               RETURN)))))



(defun generate-binop (node op &optional (width 'K:BW-24))
  (destructure (((cont left right) (call-args node)))
    (emit-alu op (continuation-expecting cont)
              (read-acc left)                   ;this is not quite right
              (read-acc right)
              width)))

(defun generate-unop (node op  &optional (width 'K:BW-24))
  (destructure (((cont arg) (call-args node)))
    (emit-alu op (continuation-expecting cont)
          (read-acc arg)
          ''0
          width)))                                      ;???



(defun comparator (node cond &optional (width 'K:BW-24))
  (let ((ref1 (call-arg-n 4 node))
        (ref2 (call-arg-n 5 node)))
    (emit-alu 'K:L-R 'K:NOOP-NO-OVERFLOW-TRAP (read-acc ref1) (read-acc ref2) width)
    (generate-conditional node cond)))

(defun generate-conditional (node cond)
  (let ((then (then-cont node))
        (else (else-cont node)))
    (when (and (not (lambda-node? then))
               (lambda-node? else))
      (rotatef then else)
      (setq cond (inverse-cond cond)))
    (cond ((lambda-node? then)
           (emit-test cond)
           (cond ((lambda-node? else)
                  (lambda-queue else)
                  (emit-branch else))
                 (t
                  ;; can this need join-state stuff??
                  ;; this may want to be generate-continuation-call
                  (let ((else-cont (variable-known (reference-variable else))))
                    ;; maybe this should be queued by generate-let?
                    (lambda-queue else-cont)
                    (emit-branch else-cont))))
           (generate-lambda then)  ; (emit-tag then) (generate-call (lambda-body then))
           )
          (t
           ;; both continuations are bound
           ;; can this happen??
           (format t "~&Both continuations of a conditional are bound:")
           (pp-cps (node-parent node))
           (emit-test cond)
           ;; can this need join-state??
           ;; will need to queue...
           (emit-branch (variable-known (reference-variable else)))
           (generate-jump (variable-known (reference-variable then)))))))


;(defun gen-conditional-primop (node prim)
;  (primop.generate prim node)
;  (let ((then (then-cont node))
;        (else (else-cont node)))
;    (if (lambda-node? else) (lambda-queue else))
;    (cond ((lambda-node? then)
;          (emit-tag then)
;          (generate-call (lambda-body then)))
;         (t
;          (generate-conditional-continuation node then)))
;    (unless (lambda-node? else)
;      (emit-tag else)
;      (generate-conditional-continuation node else))))


;;; set-join-state queued the continuation on the first call.
;(defun generate-conditional-continuation (node proc-leaf)
;  (let ((proc (variable-known (leaf-value proc-leaf))))
;    (case (lambda-strategy proc)
;      (STRATEGY/STACK)
;      (t
;       (let ((join (get-or-set-join-state node proc)))
;         (parass node
;                (join-point-arg-specs join)
;                (join-point-global-registers join)))))
;;   (fetch-continuation node proc-leaf)
;;   (clear-slots)     why??
;   (generate-jump proc)))

;;; ???
;(defun generate-define-var (node)
;  (let* ((value (call-arg-n 3 node)))
;    (cond ((and (lambda-node? value)
;               (not (eq (primop.support-variant (leaf-value (call-proc node)))
;                        'lset))
;               (eq (environment-closure (lambda-env value)) *unit*))
;           (lambda-queue value))
;          ((primop-node? value))
;          (t
;           (generate-set node (call-arg-n 2 node) value)))))

(defun generate-define-var (node)
  (let ((value (call-arg-n 3 node)))
    (cond ((lambda-node? value)
           (lambda-queue value))
          ((primop-node? value))
          (t
           (generate-set node (call-arg-n 2 node) value)))))


;;; this is from generate-set-fixed-accessor (lmlocgen)
;;; needs to do something with the continuation
;;; (used to call mark-continuation)
;(defun generate-lexical-setq (node loc value)
;  (cond ((lambda-node? value)
;        (cerror "foo" "make closure")
;;       (let ((access (access/make-closure node value)))
;;         (if access (protect-access access) (lock AN))
;;         (cond (;(and (eq prim primop/cell-value)
;;                (member (variable-support (leaf-value loc)) '(one nil))       ;eq 'one
;;                (kill (leaf-value loc))       ; force into closure
;;                (generate-move (access-value node (leaf-value loc)) AN))
;;               (t
;;                (let ((reg (->register 'pointer node (leaf-value loc) '*)))
;;                  (generate-move (if access access AN)
;;                                 (reg-offset reg (primop.location-specs prim))))))
;;         (if access (release-access access) (unlock AN)))
;        )
;       (t
;          (cond (                              ;(and (eq prim primop/cell-value)
;                     ;; what is this cruft?
;                 t   ;;(member (variable-support (leaf-value loc)) '(one nil)) ;eq 'one
;                 ;; the heart of the matter
;                 (generate-move value loc)
;                 (generate-move loc (continuation-expecting (cont node))))
;                (t
;                 (cerror "foo" "never")
;;                (let ((reg (->register 'pointer node (leaf-value loc) '*)))
;;                  (generate-move (reg-offset reg (primop.location-specs prim))
;;                                 loc)))))
;                 )))))



(defun generate-lexical-setq (node loc value)
  (generate-move value loc)
  (generate-move loc (continuation-expecting (cont node))))


(defun generate-special-ref (node sym)
  (generate-move `(K:%VALUE-CELL ,(leaf-value sym))
                 (continuation-expecting (call-arg-n 1 node))))

(defun generate-function-ref (node sym)
  (generate-move `(K:%FUNCTION-CELL ,(leaf-value sym))
                 (continuation-expecting (call-arg-n 1 node))))

(defun generate-special-setq (node sym value)
  (cond ((lambda-node? value)
         (hair)) ;make closure
        (t (generate-move value
                          `(K:%VALUE-CELL ,(leaf-value sym))))))



;;; Returns T if VAR is not bound
;;; and is called as a function
(defun unknown-function-p (var)
  (and (null (variable-binder var))
       (some #'(lambda (ref)
                 (eq (node-role ref) call-proc))
             (variable-refs var))))


;;; Generate setup code for an optional argument
;;;
;;; (defun foo (a b &optional (x 'foo) (y 'bar y-p) (z (hair)))
;;;   (body))
;;;
;;; becomes:
;;;
;;; ((lambda (y-p)
;;;    (labels
;;;      ((2x () (optional-init x 'foo nil 3x))   ;=> ($NOOP) (setq x 'foo) (3x)
;;;       (3x () (optional-init y 'bar y-p 4x))   ;=> ($NOOP) (setq y 'bar) (setq y-p nil) (4x)
;;;       (4x () (optional-init z (hair) nil 5x)) ;=> ($NOOP) (setq z (hair)) (5x)
;;;       (5x () (body)))
;;;     (optionals-setup 2x 2 '(nil nil y-p nil) '5x '2x '3x '4x)))
;;;  '<undefined>)
;;;
;;; the entry points are fixed offsets into the code for FOO,
;;; they can be fixed because there are no function calls in
;;; the args initialization, if there are any function calls
;;; in the initializations, the initializations all come at the end
;;;
;;; FOO_16
;;; 2_987
;;;   (JUMP 2x_23)
;;; 3_988
;;;   (JUMP 3x_31)
;;; 4_989
;;;   (MOVE A5 'T)                  ;(setq y-p t)
;;;   (JUMP 4x_41)
;;; 5_990
;;;   (MOVE A5 'T)                  ;(setq y-p t)
;;; 5x_53
;;;   (TAIL-OPEN)
;;;   (TAIL-CALL BODY '0)
;;; 2x_23
;;;   (MOVE A2 'FOO)                ;(setq x 'foo)
;;; 3x_31
;;;   (MOVE A3 'BAR)                ;(setq y 'bar)
;;;   (MOVE A5 'NIL)                ;(setq y-p nil)
;;; 4x_41
;;;   (KOPEN)                       ;(setq z (hair))
;;;   (KCALL HAIR '0 A4)
;;; C_45
;;;   (JUMP 5x_53)
;;;
;;;
(defun generate-optional-setup (node)
  (let* ((args (cddr (call-args node)))
         (nargs (leaf-value (car args)))
         (supplied-p-vars (leaf-value (second args)))
         (body-label (variable-known (leaf-value (third args))))
         (inits (mapcar #'(lambda (arg)
                            (variable-known (leaf-value arg)))
                        (cdddr args)))
         (hairy-inits?
           (some #'(lambda (init)
                     (some #'unknown-function-p
                           (lambda-live (cont (lambda-body init)))))
                 inits)))
    (when hairy-inits?
      (dolist (init inits)
        (lambda-queue-at-end init))
      (lambda-queue body-label))
    (do ((nargs nargs (1+ nargs))
         (inits inits (cdr inits))
         (supplied-tail (cdr supplied-p-vars) (cdr supplied-tail))
         (supplied-p-yet? nil))
        (())
      (let ((tag (create-variable nargs))
            (init (car inits)))
        (push (cons nargs tag) *nargs-entries*)
        (emit-tag tag)
        (do ((vars supplied-p-vars (cdr vars)))
            ((eq vars supplied-tail))
          (if (car vars)
            (generate-move ''t (car vars))))
        (if (null inits) (return nil))
        (if hairy-inits?
            (generate-jump init)
          (progn
            (when (and (not supplied-p-yet?)
                       (car supplied-tail))
              (setq supplied-p-yet? t)
              (dolist (init (cdr inits))
                (push init *lambdas-generated-crock*))
              (push body-label *lambdas-generated-crock*))
            (generate-lambda init)
            (if (not supplied-p-yet?) (pop *lambda-queue*))))))
    (unless hairy-inits? (generate-lambda body-label))))



(defun generate-open (node)
  (let* ((call (leaf-value (call-arg-n 2 node)))
         (proc (call-proc call))
         (call-cont (call-arg-n 1 call)))
    (unless (or ;;continuation or let
                ;;these should not be generated or flushed during simplify??
              (lambda-node? proc)
              (not (reference-node? proc))              ;primop?? flushed during simplify?
              (variable-known (leaf-value proc)))   ;label procs
      (if (or (lambda-node? call-cont)
              (variable-known (reference-variable call-cont)))           ; bound continuation
          (emit 'K:KOPEN)
        (emit 'K:TAIL-OPEN)))))


;;; defsubst
(defun generate-move (from to)
  (setq from (read-acc from))
  (setq to (write-acc to))
  (unless (or (eql from to)
              (eq to 'IGNORE)
              (eq from undefined))
    (emit 'K:MOVE to from)))




;;; this changes if we do instruction block reordering
;;; this may not work because there might be a pending
;;;      non lambda else continuation for a conditional
(defun generate-jump (label)
  (unless (eq (car *lambda-queue*) label)
    (emit-jump label)))

;;; jump but try not to
;;; defsubst
(defun generate-avoid-jump (label)
  (generate-jump label))
;  (emit-avoid-jump 'jmp label nil))

(defun generate-general-call (name tail-p dest  n-args)
  (if tail-p
      (emit 'K:TAIL-CALL name `',(1+ n-args))   ;name was (reg-offset P -2)
    (emit 'K:KCALL name `',(1+ n-args) dest)))

(defun generate-return (n-args)
  (unless (= n-args 1)
    (emit 'K:MOVE A0 `',(+ n-args *mv-return-nargs-offset*)))
  (emit 'K:RETURN))

;;; (multiple-value-bind (x y z w) (f)  ; f will return # returned values + 3 in r0
;;;   ...
;;;
;;;   (call f a0)
;;;   (alui16 l-r garbage r0 '<n+3>)   ;(jump 1) (.+5) if f is not mv function (or remove .-.+5)
;;;   (alu l+r garbage r0 opc jcond-ge)
;;;   (branch mvbind)
;;;   (dispatch)
;;; 0 (move a0 'nil)
;;; 1 (move r1 'nil)
;;; 2 (move r2 'nil)
;;; 3 (move r3 'nil)
;;;   ...
;;; n-1
;;;   (move rn-1 'nil)
;;; mvbind
;;;   (move a1 r1)
;;;   (move a2 r2)
;;;   (move a3 r3)
;;;   ...
;;;   (move an-1 rn-1)
;;;
(defun generate-mv-bind (binder)
  (let* ((vars (lambda-variables binder))
         (nvars (length vars))
         (tag (gen-tag 'mvbind)))
    (emit 'K:ALU 'K:L-R 'K:NOOP-NO-OVERFLOW-TRAP R0 `',(+ nvars *mv-return-nargs-offset*))
    (emit 'K:ALU 'K:L+R 'K:NOOP-NO-OVERFLOW-TRAP R0 'K:OPC 'K:BRANCH-GREATER-THAN-OR-EQUAL)
    (emit 'K:BRANCH tag)
    (emit 'K:DISPATCH)
    (generate-move ''nil (car vars))
    (do ((i 1 (1+ i)))
        ((>= i nvars))
      (generate-move ''nil (+ R0 i)))
    (emit-tag-1 tag)
    (do ((i 1 (1+ i))
         (vars (cdr vars) (cdr vars)))
        ((>= i nvars))
      (generate-move (+ R0 i)
                     (write-acc (car vars))))))


;;; Cons rest argument
;;;
;;; (defun f (m n o p q r)
;;;   (g r q p o n m) ...)
;;;
;;; (defun g (a b c &rest z)
;;;   ...)
;;;
;;; f
;;;  (move O0 A5)
;;;  ...
;;;  (move O5 A0)
;;;  (move *arg-2* '5)             ;<n-args>-1
;;;  (call g)
;;;  ...
;;;
;;; g
;;;  (move *arg-1* '2)             ;<n-spread-args>-1
;;;  (jump cons-rest (*return-pc-1* opc))
;;;  (move A3 *rest*)
;;;  ...
;;;
(defun generate-cons-rest (lambda)
  (let ((acc (write-acc (lambda-rest-var lambda))))
    (unless (eq acc 'IGNORE)
      (generate-move `',(- (length (lambda-variables lambda)) 2) 'K:*ARG-1*)
      (emit 'K:JUMP 'KSI:CONS-REST (list 'K:*RETURN-PC-1* 'K:OPC))
      (generate-move 'K:*REST* acc))))

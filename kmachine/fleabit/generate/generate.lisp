;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Code Generation

(defvar *function-name* nil "Name of the current compiled function")
(defvar *lambda-queue*  '() "Queue of lambda bodies to process")
(defvar *nargs-entries* '() "Entry points in current lambda for different no. of args")
(defvar *environment*   '() "Current lexical environment")
(defvar *env-acc*       nil "Accessor for the current environment")
(defvar *dynamic-state* '() "Current dynamic state to be unwound on exit")

;;; Continuation of a call
(defsubst cont (node)
  (car (call-args node)))

;; Setting this causes the fleabit compiler to produce CERRORs when certain
;; suspicious things happen.  CERROR is used so they can't be missed.

(defvar *warn-the-luser* t)

(defun warn-the-luser (&rest args)
  (when *warn-the-luser*
    (apply #'cerror args)))

;--------------------------------------------------------------------------------
;;;; Lambda Queue

;;; A queue of lambda bodies to be generated is kept in *lambda-queue*
;;; each entry also saves the state in which the lambda is to be compiled.

(defstruct (lambda-queue-entry
             (:constructor make-lambda-queue-entry
                           (lambda dynamic-state env-acc stack-slots)))
  lambda
  dynamic-state
  env-acc
  stack-slots)

(defun lambda-queue-position (lambda)
  (position lambda *lambda-queue* :key #'lambda-queue-entry-lambda))

(defun lambda-queue (node)
  (unless (member node *lambda-queue* :key #'lambda-queue-entry-lambda :test #'eq)
    (push (make-lambda-queue-entry node *dynamic-state* *env-acc* *stack-slots*)
          *lambda-queue*)))

(defun lambda-queue-unless-generated (lambda)
  (unless (lambda-generated-p lambda)
    (lambda-queue lambda)))

(defun lambda-queue-at-end (node)
  (unless (member node *lambda-queue* :key #'lambda-queue-entry-lambda :test #'eq)
    (setq *lambda-queue* (nconc *lambda-queue*
                                (list (make-lambda-queue-entry node
                                                               *dynamic-state*
                                                               *env-acc*
                                                               *stack-slots*))))))

(defun queued-next-p (lambda)
  (and *lambda-queue*
       (eq lambda (lambda-queue-entry-lambda (car *lambda-queue*)))))

;--------------------------------------------------------------------------------
;;;; Generate

;;; GENERATE is the top level function to generate code.
;;; Given an optimized, analyzed code tree, it returns
;;; an ncompiled-function object.

(defun generate (top-node)
  (let ((top (car (call-args (lambda-body top-node))))) ;??
    (reg-alloc-top top)
    (debug :generate
      (format *debug-stream* "~&Generating:")
      (pp-cps top :extra #'lambda-strategy))
    (let ((*environment* nil)
          (*env-acc* nil))
      (generate-function (variable-name (lambda-self-var top))
                         top))))

;;; GENERATE-FUNCTION produces an ncompiled-function object from
;;; a function (heap lambda node).  It is called with the toplevel
;;; function, and also to generate any internal functions.
(defun generate-function (name lambda)
  (let ((*function-name* name)
        (*instructions* '())
        (*nargs-entries* (list (cons (1- (length (lambda-variables lambda)))
                                     (get-tag lambda))))
        (*lambda-queue* '()))
    (generate-lambda lambda name)
    (do ()
        ((null *lambda-queue*))
      (let ((queue-entry (pop *lambda-queue*)))
        (let ((*dynamic-state* (lambda-queue-entry-dynamic-state queue-entry))
              (*env-acc* (lambda-queue-entry-env-acc queue-entry))
              (*stack-slots* (lambda-queue-entry-stack-slots queue-entry)))
          (generate-lambda (lambda-queue-entry-lambda queue-entry)))))
    (debug :pre
      (format *debug-stream* "~&~%Pre Processed: entries ~s~%" *nargs-entries*)
      (print-instructions (reverse *instructions*) *debug-stream*))
    (let ((insts (post-process *instructions*)))
      (debug :post
        (format *debug-stream* "~&~%Post Processed: entries ~s~%" *nargs-entries*)
        (print-instructions insts *debug-stream*))
      (let ((nc-function-defstruct (assemble-instruction-list name insts *nargs-entries*)))
        (debug :postx
          (format *debug-stream* "~%Postprocessed instructions:~%")
          (print-instructions insts *debug-stream* (ncompiled-function-code nc-function-defstruct)))
        nc-function-defstruct))))


;;; Generate code for a lambda node
(defun generate-lambda (node &optional (name T))
  (setf (lambda-generated-p node) name)
  (case (lambda-strategy node)
    (STRATEGY/HEAP
     (emit-tag node)
     (generate-heap-lambda node))
    (STRATEGY/PROC
     (warn-the-luser "Continue as if nothing were about to happen."     ;added by smh 15aug88
             "Unbelievable fleabit attempt to generate STRATEGY/PROC lambda: ~s inside ~s"
             name *function-name*)
     (emit-tag node)
     (generate-proc-lambda node))
    (STRATEGY/LABEL
     ; (generate-label-lambda node)
     (emit-tag node)
     (generate-lambda-1 node))
    (STRATEGY/OPEN
     ;; this is neccessary, which is unfortunate
     (emit-tag node)
     (generate-lambda-1 node))
    (t (bug "Bad Strategy: ~a" (lambda-strategy node)))))



#||||
;;; This crock attempts to rotate loops so
;;; the test is at the end

(defvar *loops-crocked* '())

(defun calls-label? (cont label)
  (when (and (lambda-node? cont)
             (some #'(lambda (var)
                       (eq (variable-known var) label))
                   (lambda-live cont)))
    cont))

(defun generate-label-lambda (node &aux loop-cont)
  (let ((call (lambda-body node)))
    (let ((proc (call-proc call)))
      (if (and (primop-ref? proc primop/conditional)
               (not (member node *loops-crocked*))
               (let ((then-cont (call-arg-n 1 call))
                     (else-cont (call-arg-n 2 call)))
                 ;; do loops generally loop in the else
                 (cond ((setq loop-cont (calls-label? else-cont node)))
                       ((setq loop-cont (calls-label? then-cont node))))))
          (progn
            (setf (lambda-generated-p node) nil)
            (setf (lambda-dynamic-state node) *dynamic-state*)
            (push node *loops-crocked*)
            (emit-unconditional-branch node)
            (lambda-queue node)
            (lambda-queue loop-cont))
        (progn
          (emit-tag node)
          (generate-lambda-1 node))))))

||||#


;(defun generate-heap-lambda (node)
;  ;; this stuff all interacts badly with optional args
;  (let ((slots (lambda-stack-slots node)))
;    (let ((*stack-slots* slots))
;      (generate-alloc-stack-slots (- slots
;                                    (max 0 (- (1- (length (lambda-variables node)))
;                                              *frame-size*))))
;      (let ((*dynamic-state* (cons `(ARBITRARY-STUFF) *dynamic-state*)))
;       (setf (lambda-dynamic-state node) *dynamic-state*)
;       (unless (zerop slots)
;         (push `(SLOTS    . ,slots)
;               *dynamic-state*))
;       (generate-lambda-2 node)))))


;(defun generate-lambda-1 (node)
;  ;; save the dynamic state on entry
;  (setf (lambda-dynamic-state node) *dynamic-state*)
;  (generate-lambda-2 node))

;(defun generate-lambda-2 (node)
;  (let ((*environment* (cdr (lambda-env node))))
;    (let ((*env-acc* (if (eq (lambda-strategy node)
;                            STRATEGY/HEAP)
;                        (if *environment* AN ''NIL)
;                      *env-acc*)))
;      (let ((closed-set-vars (car (lambda-env node))))
;       (when closed-set-vars
;         (let ((new-env-acc (acc (car closed-set-vars))))
;           (generate-make-contour closed-set-vars new-env-acc)
;           (setq *env-acc* new-env-acc))
;         (push closed-set-vars *environment*)))
;      (debug-msg :env "~&Lambda: ~a~&  Environment: ~a~&  Env-acc: ~a"
;                (lambda-name node)
;                (mapcar #'(lambda (frame) (mapcar #'variable-unique-name frame))
;                        *environment*)
;                *env-acc*)
;      (let ((closed-exits (lambda-closed-exit-args node)))
;       (when closed-exits
;         (format t "~&closed-exits: ~s" closed-exits)
;         (generate-return-from-catch closed-exits)))
;      (let ((specials (lambda-special-vars node)))
;       (let ((*dynamic-state* (append (when specials
;                                        `((SPECIALS . ,(length specials))))
;                                      *dynamic-state*)))
;         (debug-msg :dynamic "~%Dynamic State ~a: ~s"
;                   (lambda-name node)
;                   *dynamic-state*)
;         (generate-special-bindings specials)
;         (generate-call (lambda-body node)))))))

;(defun generate-special-bindings (specials)
;  (dolist (var vars)
;    (generate-bind var)))

(defun generate-heap-lambda (lambda-node)
  (let ((*dynamic-state* (cons `(ARBITRARY-STUFF) *dynamic-state*))
        (*stack-slots* (lambda-stack-slots lambda-node)))
    ;; This is some installed paranoia, courtesy of smh 15aug88
    (when (> *stack-slots* 0.)
      (warn-the-luser "Continue as if the function were going to compile correctly"
              "GENERATE-HEAP-LAMBDA needs ~d stack slots for ~s inside ~s"
              *stack-slots*
              (lambda-name lambda-node)
              *function-name*))
    (generate-lambda-1 lambda-node)))


(defun generate-lambda-1 (lambda-node)
  (let ((*environment* (cdr (lambda-env lambda-node))))
    (let ((*env-acc* (if (eq (lambda-strategy lambda-node)
                             STRATEGY/HEAP)
                         (if *environment* AN ''NIL)
                       *env-acc*)))
      (setf (lambda-dynamic-state lambda-node) *dynamic-state*)
      (let ((*dynamic-state* *dynamic-state*)
            ;; this is some uninstalled generality
            ;; stack slots are now only allocated at entry to
            ;; heap lambdas
;           (*stack-slots* (+ *stack-slots* (lambda-stack-slots lambda-node)))
            )
        (unless (lambda-has-optional-args-p lambda-node)
          ;; entry stuff for optional args gets generated
          ;; by GENERATE-OPTIONAL-SETUP
          (generate-entry lambda-node))
        (generate-call (lambda-body lambda-node))))))

;;; serendipitously this will probably cause special supplied-p vars
;;; to not get bound before entries
;;; there is probably a cleaner way to do it...
(defun lambda-has-optional-args-p (lambda-node)
  (let ((proc (call-proc (lambda-body lambda-node))))
    (cond ((primop-ref? proc primop/Y)
           (primop-ref? (call-proc (lambda-body (cont (lambda-body (cont (lambda-body lambda-node))))))
                                primop/optional-setup))
          ;; supplied-p lambda
          ((lambda-node? proc)
           (lambda-has-optional-args-p proc)))))



;;; Generate various pieces of code sometimes needed on entry to a lambda
(defun generate-entry (lambda-node &optional (specials (lambda-special-vars lambda-node)))
  ;;
  ;; Cons rest arg
  ;;
  (let ((rest-var (lambda-rest-var lambda-node)))
    (when rest-var
      (generate-cons-rest rest-var))
    ;;
    ;; Allocate any stack slots
    ;;
    (when (eq (lambda-strategy lambda-node) STRATEGY/HEAP)
      (let ((slots (lambda-stack-slots lambda-node)))
        (unless (zerop slots)
          (generate-alloc-stack-slots (- slots
                                         (max 0 (- (1- (length (lambda-variables lambda-node)))
                                                   *frame-size*))))
          (push `(SLOTS    . ,slots)
                *dynamic-state*))))
    ;;
    ;; Now move rest arg
    ;;
    (when rest-var (store-rest-arg rest-var)))
  ;;
  ;; Make environments for any closed vars which are setqed
  ;;
  (let ((closed-set-vars (car (lambda-env lambda-node))))
    (when closed-set-vars
      (let ((new-env-acc (if (equal *env-acc* ''NIL)
                             (acc (car closed-set-vars))
                           *env-acc*)))
        (generate-make-contour closed-set-vars new-env-acc)
        (setq *env-acc* new-env-acc))
      (push closed-set-vars *environment*)))
  (debug-msg :env "~&Lambda: ~a~&  Environment: ~a~&  Env-acc: ~a"
             (lambda-name lambda-node)
             (mapcar #'(lambda (frame) (mapcar #'variable-unique-name frame))
                     *environment*)
             *env-acc*)
  ;;
  ;; Set up catchers for any closed exits (return-from/go out of closures)
  ;;
  (let ((closed-exits (lambda-closed-exit-args lambda-node)))
    (when closed-exits
      (format t "~&closed-exits: ~s" closed-exits)
      (generate-return-from-catch closed-exits)))
  ;;
  ;; Bind any special variables
  ;;
  (generate-special-bindings specials)
  (debug-msg :dynamic "~%Dynamic State after entry to ~a: ~s"
                 (lambda-name lambda-node)
                 *dynamic-state*)
  )




(defun generate-special-bindings (specials)
  (when specials
    (push `(SPECIALS . ,(length specials))
          *dynamic-state*))
  (dolist (var specials)
    (generate-bind var)))


;;; let nodes can't have their bodies substituted
;;; for their continuations if they bind specials.
;;; do something like this in simplify-let:
#||||
(defun strong-substitute-lambda? (var)
  (and (null (variable-setqs var))
       (null (cdr (variable-refs var)))
       (not (some #'(lambda (var) (and var (variable-special-p var)))
                  (lambda-rest+variables (variable-binder var))))))
||||#

(defun lambda-closed-exit-args (node)
  (remove-if-not #'(lambda (var)
                     (and (variable-closed var)
                          (some #'call-exit?
                                (variable-refs var))))
                 (lambda-variables node)))





(defun variable-known (var)
  (let ((type (variable-type var)))
    (if (node-p type)
        (cond ((lambda-node? type)
               type)
              ((variable-p type)
               (variable-known type))
              ((reference-node? type)
               (variable-known (reference-variable type)))))))

(defun generate-call (node)
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (emit-comment "~s" (pp-cps node))
           (generate-primop-call node))
          ((lambda-node? proc)
           (generate-let node))
          (t
           (let ((known (variable-known (reference-variable proc))))
             (cond (known
                    (emit-comment "Call known procedure ~s"
                                 (cons (lambda-name proc) (cdr (pp-cps node))))
                    (cond ((zerop (call-exits node))
                           (generate-known-return node proc known))
                          ((= (call-exits node) 1)
                           (generate-known-call node known))
                          (t (error "Call exits not 0 or 1 in GENERATE-CALL"))))
                   ((zerop (call-exits node))
                    (emit-comment "Return from procedure ~s" (pp-cps node))
                    (generate-return node proc))
                   ((= (call-exits node) 1)
                    (emit-comment "Call unknown procedure ~s" (pp-cps node))
                    (gen-general-call node proc))
                   (t
                    (bug "too many exits - ~s" node))))))))


(defun generate-go (tagbody-cont tag)
  (let ((cvar (reference-variable tag)))
    (if (throw-needed? cvar)
        ;; this can't work
        ;; the throw tag must be more unique
        (generate-throw ''.TAG. `',(variable-name (reference-variable tag)))
      (progn
        (unwind-dynamic-state cvar)
        (generate-unconditional-branch (reference-variable tag))))))


;;; this doesn't quite work
;;; where are we going to put the return values???
(defun generate-known-return (node pvar proc)
  (setq pvar (reference-variable pvar))
  (if (closed-continuation-p pvar)
         ;; wrong, need to return multiple values
         ;; also tag is not unique enough
      (generate-throw `',(get-tag pvar) (call-arg-n 1 node))
    (progn
      (if (dynamic-state? pvar)
          (unwind-dynamic-state pvar))
      (parallel-assign (call-args node) (lambda-variables proc) (lambda-rest-var proc))
      ;; nullify multiple values wanted
      (let ((extra (nthcdr (length (call-args node))
                           (lambda-variables proc))))
        (when extra
          (dolist (var extra)
            (generate-move ''NIL var))))
      (generate-unconditional-branch proc))))

#||||
(defvar *return-point* nil)

         ;; here we try to only generate the unwind and return
         ;; once, all other times we branch to it.
         (if (and (null (cdr (call-args node)))
                  *return-point*)
             (emit-unconditional-branch *return-point*)
           (progn
             (if (cdr (variable-refs proc))
                 (emit-tag (setq *return-point* (gen-tag 'return))))
             (unwind-dynamic-state proc)
             (generate-move A0 (if (cdr (call-args node))
                                   RETURN-MV
                                 RETURN))
             (emit-return)))
||||#

(defun generate-return (node proc)
  (setq proc (reference-variable proc))
  (cond ((throw-needed? proc)   ;(closed-continuation-p proc)
         ;; wrong, need to return multiple values
         ;; also tag is not unique enough
         (generate-throw `',(get-tag proc) (call-arg-n 1 node)))
        ((dynamic-state? proc)
         (generate-assign-return-values node A0)
         (unwind-dynamic-state proc)
         (generate-move A0 (if (cdr (call-args node))
                               RETURN-MV
                             RETURN))
         (emit-return))
        (t
         (generate-assign-return-values node)
         (emit-return))))

(defun closed-continuation-p (var)
  (and (variable-closed var)
       ;; this is not good enough
       ;; we must be within closure also (see acc)
       ))

;;; This is like generate-return but we know we are
;;; only returning one value, which is in A0 if
;;; there is dynamic state, and RETURN if there is not.
;;; (get-destination figured this out earlier)
;;; This might not be the right thing for PROCs??
(defun generate-primop-return (dest cvar &optional values)
  (let ((return-dest (cond ((null values) RETURN)
                           ((numberp values)
                            (if (= values 1) RETURN RETURN-MV))
                           (t RETURN-TAIL))))
    (if (dynamic-state? cvar)
        (progn (generate-move dest A0)
               (unwind-dynamic-state cvar)
               (generate-move A0 return-dest))
      (generate-move dest return-dest))
    (emit-return)))



(defun generate-return-no-values (cvar)
  (generate-move ''0 'GR:*NUMBER-OF-RETURN-VALUES*)
  (generate-primop-return ''NIL cvar 0))

;--------------------------------------------------------------------------------
;;;; Dynamic State

;;; The variable *dynamic-state* contains a representation of the
;;; current dynamic state of the code.  This includes such things as
;;; special variable bindings and allocation of stack slots.  At various
;;; points the dynamic state must be unwound back to a previous state.

(defun continuation-dynamic-state (cont-var)
  (do () (())
    (let ((type (variable-type cont-var)))
      (cond ((variable-p type)
             ;; these are set by truly-label-p
             (setq cont-var type))
            ((reference-node? type)
             (bug "ok, are known var types variables or references? lets get this right"))
            (t (return)))))
  (lambda-dynamic-state (variable-binder cont-var)))

(defun dynamic-state? (cont-var)
  (not (eq (continuation-dynamic-state cont-var)
           *dynamic-state*)))

(defun unwind-dynamic-state (cont-var)
  (let ((cont-state (continuation-dynamic-state cont-var)))
    (do ()
        ((eq *dynamic-state* cont-state))
      (let ((unwind (pop *dynamic-state*)))
        (case (car unwind)
          (SPECIALS        (generate-unbind (cdr unwind)))
          (SLOTS           (generate-dealloc-stack-slots (cdr unwind)))
          (OPEN            (emit-call 'LI:FLUSH-OPEN-FRAME 0))
          (TAIL-OPEN       (generate-flush-topen-frame))
          ;; unwind?
          (CATCH           (emit-call 'LI:FLUSH-CATCH 0))
          (ARBITRARY-STUFF (cerror "skip it" "Need to throw!"))
          (t (bug "Unknown dynamic state: ~s" unwind)))))))


(defun throw-needed? (cvar)
  (let ((cont-state (continuation-dynamic-state cvar)))
    (do ((state *dynamic-state* (cdr state)))
        ((eq state cont-state))
      (when (eq (caar state) 'ARBITRARY-STUFF)
        (return t)))))


;--------------------------------------------------------------------------------
;;;; Generation of a Function Call

(defun get-call-destination (call-node cont)
  (declare (values dest-for-call dest-for-cont))
  (if (member (call-open call-node)
              '(K:NEW-OPEN K:NEW-TAIL-OPEN))
      (let ((o-reg (continuation-expecting cont)))
        (values
          (list (call-open call-node)
                (- o-reg O0))
          o-reg))
    (let ((dest (get-destination cont)))
      (values dest dest))))


(defun lambda-special-vars (lambda)
  (remove-if-not
    #'(lambda (v)
        (and v (variable-special-p v)))
    (lambda-rest+variables lambda)))

;;; This returns T if a call with continuation CONT
;;; appears as a tail call, not counting any dynamic
;;; state which may need unwinding.
(defun wants-to-be-tail-p (cont)
  (and (reference-node? cont)
       (not (variable-known (reference-variable cont)))))

(defun tail-continuation-p (cont)
  (and (reference-node? cont)
       (not (variable-known (reference-variable cont)))
       (not (dynamic-state? (reference-variable cont)))))

;;; Generate a general call to an unknown function
(defun gen-general-call (node proc)
  (let* ((cont (call-arg-n 1 node))
         (args (cdr (call-args node)))          ;don't count cont
         (nargs (length args)))
    (parallel-assign args *open-regs* 'PUSH)
    (do ((ds (car (pop *dynamic-state*))
             (car (pop *dynamic-state*))))
        ((or (eq ds 'OPEN)
             (eq ds 'TAIL-OPEN)))
      (or (eq ds 'SLOTS)
          (bug "Something unexpected on *dynamic-state*: ~s" ds)))
    (if (wants-to-be-tail-p cont)
        (let ((cont-var (reference-variable cont)))
          (cond ((throw-needed? cont-var)
                 (generate-general-call proc nil A0 nargs)
                 (generate-throw `',(get-tag cont-var) A0))
                ((dynamic-state? cont-var)
                 (generate-general-call proc nil A0 nargs)
                 (unwind-dynamic-state cont-var)
                 (generate-move A0 RETURN-TAIL)
                 (emit-return))
                (t (generate-general-call proc t RETURN-TAIL nargs))))
        (multiple-value-bind (dest-for-call dest-for-cont)
            (get-call-destination node cont)
          (generate-general-call proc nil dest-for-call nargs)
          (when (consp dest-for-call)
            (case (car dest-for-call)
              (K:NEW-OPEN      (push (list 'OPEN) *dynamic-state*))
              (K:NEW-TAIL-OPEN (push (list 'TAIL-OPEN) *dynamic-state*))))
          (generate-continuation dest-for-cont cont)))))

;;; Generate a continuation.
;;; This could be called with either a lambda or reference
;;; A lambda has any multiple-values bound and gets generated.
;;; A reference can be a continuation arg to a heap/proc lambda (in which case we return)
;;; or bound (we branch to it)
(defun generate-continuation (dest cont &optional values)
  (cond ((lambda-node? cont)
         (generate-move dest (continuation-expecting cont))
         (assign-continuation-multiple-values cont values)
         (case (lambda-strategy cont)
           ;; no label
           (STRATEGY/OPEN (generate-call (lambda-body cont)))
           (STRATEGY/LABEL (generate-lambda cont))
           (t (bug "Odd continuation strategy: ~s" (lambda-strategy cont)))))
        ((reference-node? cont)
         (let ((proc (variable-known (reference-variable cont))))
           (cond (proc
                  (generate-move dest (continuation-expecting proc))
                  (unwind-dynamic-state (reference-variable cont))
                  (assign-continuation-multiple-values proc values)
                  (generate-unconditional-branch proc))
                 (t
                  ;; generate-general-call doesn't get here
                  ;; it just does a tail call
                  ;; what about label/proc??
                  (generate-primop-return dest (reference-variable cont) values)))))
        (t (bug "Weird continuation"))))

;;; takes a lambda continuation
;;; If it is expecting multiple values then we need
;;; to do a mv-bind unless we know how many values
;;; were returned, in which case we can just move
;;; nil to the extras
(defun assign-continuation-multiple-values (cont-lambda values)
  (when (cdr (lambda-variables cont-lambda))
    (cond ((numberp values)
           (dolist (cvar (nthcdr values (lambda-variables cont-lambda)))
             (generate-move ''NIL cvar)))
          ((null values)
           (generate-mv-bind cont-lambda)))))


(defun needs-open? (call-node)
  (let ((proc (call-proc call-node)))
    (and (reference-node? proc)                 ;primop or let? (flushed during simplify)
         (let ((known (variable-known (reference-variable proc))))
           (or (null known)
               ;; known calls need opens when PROC or HEAP
               (member (lambda-strategy known)
                       '(STRATEGY/PROC STRATEGY/HEAP)))))))

;;; Generate the OPEN or TAIL-OPEN for a call
(defun generate-open (node)
  (let ((call (leaf-value (call-arg-n 2 node))))
    (when (needs-open? call)
      (let* ((call-cont (call-arg-n 1 call))
             (tailp (tail-continuation-p call-cont))
             (cont-call (lambda-body (cont node)))
             (cont-proc (call-proc cont-call))
             next-call)
        (if ;;This open will be followed by another
          (and (primop-ref? cont-proc PRIMOP/OPEN-FRAME)
               (progn
                 (setq next-call (leaf-value (call-arg-n 2 cont-call)))
                 (and
                   (needs-open? next-call)
                   (O-reg-p (continuation-expecting (cont next-call))))))
          ;; Set the next call to do a NEW-OPEN
          (setf (call-open next-call)
                (if tailp
                    'K:NEW-TAIL-OPEN
                  'K:NEW-OPEN))
          (generate-open-1 tailp))))
    (let* ((n-args       (length (cdr (call-args call))))
           (n-stack-args (- n-args *frame-size*)))
      ;; the following warning added by smh 15aug88
      ;; Fixed boundary case of *frame-size* args to not cause error 9/30/88 --wkf |||
      (when (plusp n-stack-args)
        (warn-the-luser "Continue as if nothing were wrong."
                        "Fleabit-compiled call of ~s with ~d args in function ~s."
                        (call-proc node)
                        n-args
                        *function-name*)
        (generate-alloc-stack-slots n-stack-args)
        (incf *stack-slots* n-stack-args)
        (push `(SLOTS . ,n-stack-args) *dynamic-state*)))))

(defun generate-open-1 (tailp)
  (cond
    (tailp
     (emit 'K:TAIL-OPEN)
     (push (list 'TAIL-OPEN) *dynamic-state*))
    (t
     (emit 'K:OPEN)
     (push (list 'OPEN) *dynamic-state*))))

(defun generate-known-call (node proc)
  (if (not (or (lambda-rest-var proc)
               (= (length (lambda-variables proc))
                  (length (call-args node)))))
      (warn "Wrong number of arguments in call to ~s"
            (variable-name (reference-variable (call-proc node)))))
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
  (parallel-assign (cdr (call-args node)) (cdr (lambda-variables proc)) (lambda-rest-var proc))
;  (if (<= (lambda-trace proc) (lambda-trace (node-parent node)))
;      (generate-avoid-branch proc)
      (generate-unconditional-branch proc))

;;; this is like gen-general-call
(defun generate-proc-call (node proc)
  (unless (lambda-generated-p proc)
    (lambda-queue-at-end proc))
  (let* ((cont (call-arg-n 1 node))
         (tail-p (and (reference-node? cont)
                      (not (variable-known (reference-variable cont))))))
    (parallel-assign (cdr (call-args node)) *open-regs* 'PUSH)
    (generate-general-call (get-tag proc)
                           tail-p
                           (continuation-expecting cont)
                           (- (length (call-args node)) 2))     ;flush cont also flush P
    (unless tail-p
      (generate-continuation cont))))



(defun generate-known-heap-call (node proc)
  (gen-general-call node
                    (or (lambda-generated-p proc)
                        (generate-function `(:INTERNAL ,*function-name*
                                                       ,(variable-name (lambda-self-var proc)))
                                           proc))))




;--------------------------------------------------------------------------------
;;;; Accessors for values

;;; ACC takes a reference and returns an accessor
;;; which says where the value can be found.
(defun acc (ref)
  (cond ((integerp ref) ref)
        ((reference-node? ref)
         (acc-var (reference-variable ref)))
        ((literal-node? ref)
         (if (eq (leaf-value ref) undefined)
             undefined
           ref))
        ((variable-p ref)
         (acc-var ref))
        ((symbolp ref)
         (let ((gr (global-register ref)))
           (if gr (acc gr) ref)))
        ((listp ref) ref)
        ((lambda-node? ref)
         ref)
        (t (bug "Trying to access value of: ~s" ref))))

;;; Find out where a variable is located.
;;; This is always VARIABLE-LOC unless the reference
;;; is within a closure, then we have to get
;;; the number of frames back and the offset
;(defun acc-var (var)
;  (if (and (variable-closed var)
;          (<= (car (variable-closed var))
;              *current-contour*))
;      (let ((loc (variable-closed var)))
;       (list* 'CLOSURE-REF (- *current-contour* (car loc)) (cdr loc)))
;    (variable-loc var)))


;;; Return an accessor for a variable
(defun acc-var (var)
  (if (variable-closed var)
      (or (acc-var-in-env var)
          (variable-loc var))
    (variable-loc var)))

(defun acc-var-in-env (var)
  (let ((nback 0))
    (dolist (frame *environment*)
      (let ((offset (position var frame)))
        (when offset
          (return-from acc-var-in-env `(CLOSURE-REF ,nback . ,offset))))
      (incf nback))))


;;; Move the values in ARGS to PLACES in parallel.
;;; Any extra args go into REST
(defun parallel-assign (args places &optional rest (next-temp R2))
  (do ((args   (mapcar #'acc args)   (cdr args))
       (places (mapcar #'acc places) (cdr places)))
      ((or (null args) (null places))
       (when rest
         (let ((rest-acc (acc rest)))
           (cond
             ((eq rest-acc IGNORED))
             ((eq rest-acc 'PUSH)
              (do ((stack-args args (cdr stack-args))
                   (slot (- *stack-slots* (length args)) (1+ slot)))
                  ((null stack-args))
                (generate-move (car stack-args) (write-stack-slot slot))))
             (t
              (if (some #'O-reg-p args)
                  ;;; need to assign to r regs
                  (cerror "foo" "consing o regs")
                (if args
                    (progn
                      (emit 'K:OPEN)
                      (parallel-assign args *open-regs* 'PUSH next-temp)
                      (generate-general-call 'LI:LIST nil rest-acc (length args)))
                  (generate-move ''NIL rest-acc))))))))
    (let ((arg   (car args))
          (place (car places)))
      (when (member place (cdr args))
        (generate-move place next-temp)
        (nsubstitute next-temp place args)
        (if (= next-temp RN)
            (setq next-temp (+ STACK-0 *stack-slots*)))
        (incf next-temp))
      (generate-move arg place))))



(defun cont-var (cont)
  (or (car (lambda-variables cont))
      (lambda-rest-var cont)))


;;; Return the register in which CONT
;;; is expecting its value
(defun continuation-expecting (cont)
  (cond ((lambda-node? cont)
         (let ((cvar (cont-var cont)))
           (if cvar
               (acc cvar)
             IGNORED)))   ;continuation has no vars?
        (t (let ((proc (variable-known (reference-variable cont))))
             (if proc    ;bound continuation
                 (continuation-expecting proc)
               RETURN)))))


(defun generate-primop-call (node)
  (let ((prim (primop-value (call-proc node))))
    (let ((dest (primop.generate prim node)))
      (unless (or (primop.conditional? prim)
                  (primop.special? prim))
        (generate-continuation dest (cont node) 1)))))



;;;; Generate Let

(defun generate-let (node)
  (destructure (((body . exprs) (call-proc+args node)))
    ;; parallel-assign is overkill because we know that
    ;; none of the bound variables is an arg
    (parallel-assign exprs (lambda-variables body) (lambda-rest-var body))
    (generate-lambda body)))



;;;; Labels

;;; this is the primop.generate function for $Y
;;; it generates the body and for HEAP labels
;;; calls GENERATE-MOVE which creates closures
;;; and queues the label functions
(defun generate-labels (node)
  (let ((y-lambda (call-arg-n 1 node)))
  (destructure (((body . procs) (call-args (lambda-body y-lambda))))
    (ecase (lambda-strategy (call-arg-n 1 node))
      ((STRATEGY/LABEL)
       (maybe-generate-tagbody-catch y-lambda body))
      ((STRATEGY/PROC))
      ((STRATEGY/HEAP)
       (generate-heap-labels y-lambda procs)))
    (generate-call (lambda-body body)))))



(defun generate-heap-labels (y-lambda procs)
  (let ((vars (cdr (lambda-variables y-lambda))))
    (mapcar #'generate-move procs vars)))


(defun maybe-generate-tagbody-catch (y-lambda body)
  (let ((closed-tags (remove-if-not
                       #'(lambda (var)
                           (and (variable-closed var)
                                ;; must be a go (or return-from?)
                                (lambda-node? (variable-type var))))
                       (cdr (lambda-variables y-lambda)))))
    (if closed-tags
        (let ((loop-tag       (gen-tag 'loop))
              (catch-cont-tag (gen-tag 'catch-cont)))
          (generate-move ''LI:.TAGBODY-START. R0)
          (emit-tag loop-tag)
          ;;this tag is not good enough
          (generate-catch-1 '.TAG. catch-cont-tag)
          (setf (lambda-dynamic-state y-lambda)
                *dynamic-state*)
          (generate-tagtest 'LI:.TAGBODY-START. body)
          (dolist (closed-tag closed-tags)
            (generate-tagtest (variable-name closed-tag) (variable-known closed-tag)))
          (emit-tag catch-cont-tag)
;       (generate-move <catch-value> R0)
          (emit-unconditional-branch loop-tag)
          (emit-tag body))
      (setf (lambda-dynamic-state y-lambda)
                *dynamic-state*))))

(defun generate-tagtest (name tag)
  (generate-move `',name R1)
  (emit-alu 'K:L-R 'K:NOP R0 R1)
  (emit 'K:TEST 'K:BR-EQUAL)    ;bignums?
  (emit-branch tag))

(defun generate-catch-1 (tag cont-tag)
  (push (cons 'CATCH tag) *dynamic-state*)
  (emit 'K:OPEN)
  (generate-move ''LI:.UNWIND-MARKER. O0)
  (generate-move `',tag O1)
  (generate-move 'GR:*SPECIAL-PDL-PTR* O2)
  (generate-move 'GR:*STACK-POINTER* 'K:O3)
  (emit 'K:MOVE-PC 'K:O4 cont-tag))


(defun generate-return-from-catch (vars)
  (let ((cont (variable-known (car vars))))
    (if cont
        ;;this tag is not good enough
        (generate-catch-1 (get-tag (car vars))
                          (get-tag cont))
      (let ((catch-cont-tag (gen-tag 'catch-cont))
            (body-tag       (gen-tag 'body)))
        (generate-catch-1  (get-tag (car vars))
                           (get-tag catch-cont-tag))
        (emit-unconditional-branch body-tag)
        (emit-tag catch-cont-tag)
        (emit-call 'LI:CATCH-CONTINUE 6 RETURN t)
        (emit-tag body-tag)))))

(defun generate-throw (tag value)
  (generate-internal-call 'LI:THROW RETURN tag value))

;--------------------------------------------------------------------------------
;;;; Finding and moving values

;;; Return T if ACC is a literal
(defun literal-p (acc)
  (or (literal-node? acc)
      (and (consp acc)
           (eq (car acc) 'QUOTE))))

(defun get-literal-value (literal)
  (cond ((literal-node? literal)
         (literal-value literal))
        ((consp literal)
         (cadr literal))
        (t (bug "Bad literal: ~s" literal))))

(defun global-p (acc)
  (and (consp acc)
       (eq (car acc) 'K:REGISTER)))

(defmacro global-frame (acc)
  `(third ,acc))


;;; Return T if X and Y are both global variables
;;; but are in different register frames, (and
;;; therefore cannot be used in the same instruction).
;;; Modified to work with cross compiler reg naming convention - 14sep88 smh
(defun different-frame-globals? (x y &aux rx ry)
  (or (and (global-p x)
           (global-p y)
           (not (= (global-frame x)
                   (global-frame y))))
      (and (symbolp x)
           (symbolp y)
           (setq rx (get x ':register))
           (setq ry (get y ':register))
           (not (= (second rx)
                   (second ry))))))

(defun same-frame-globals? (x y)
  (and (global-p x)
       (global-p y)
       (= (global-frame x)
          (global-frame y))))


;;; Note that you can't use compiler-let to bind this.
(defvar *use-constant-registers* t
  "Set this to NIL to not substitute references to
constant registers for certain constants")

;;; Look for a constant register which contains
;;; the given value
(defun find-constant-register (literal)
  (when *use-constant-registers*
    (let ((probe (assoc (get-literal-value literal)
                        *global-constants*
                        :test #'equal)))
      (when probe
        (cdr probe)))))

;;; Get something into the appropriate place
;;; for being a left source.  Returns a value
;;; to be used for the left source field.
(defun get-left-operand (ref &aux probe)
  (let ((acc (acc ref)))
    (cond ((or (register-p acc)
               (global-p acc))
           acc)
          ((and (literal-p acc)
                (setq probe (find-constant-register acc)))
           probe)
          (t (generate-move acc R0)
             R0))))

;;; Get something into the appropriate place
;;; for being a right source.  Returns a value
;;; to be used for the right source field.
;;; This is like GET-LEFT-OPERAND except functional
;;; sources can be used (particularly the MD)
(defun get-right-operand (ref &aux probe)
  (let ((acc (acc ref)))
    (cond ((or (register-p acc)
               (global-p acc))
           acc)
          ((and (literal-p acc)
                (setq probe (find-constant-register acc)))
           probe)
          ((stack-slot-p acc)
           (read-stack-slot (- acc STACK-0) R1))
          (t (generate-move acc R1)
             R1))))

(defun get-operands (left-ref right-ref)
  (declare (values left right))
  (let ((left (get-left-operand left-ref))
        (right (get-right-operand right-ref)))
    (when (different-frame-globals? left right)
      (generate-move left R0)
      (setq left R0))
    (values left right)))


;;; Get the destination for an instruction
;;; (this should really check for same globals)
(defun get-destination (cont)
  (let ((acc (continuation-expecting cont)))
    (cond ((or (register-p acc)
               (eq acc IGNORED)
               (functional-dest-p acc)
               (global-p acc))
           acc)
          ((eq acc RETURN)
           (if (dynamic-state? (reference-variable cont))
               A0 RETURN))
          (t R0))))

(defun get-destination-from-place (place)
  (let ((acc (acc place)))
    (if (or (register-p acc)
            (eq acc IGNORED)
            (functional-dest-p acc)
            (global-p acc)
            (eq acc RETURN))
           acc
      R0)))

(defun get-dest-and-right-operand (cont right-ref)
  (declare (values dest right))
  (let ((dest (get-destination cont))
        (right (get-right-operand right-ref)))
    (values
      dest
      (if (different-frame-globals? dest right)
          (progn (generate-move right R1)
                 (setq right R1))
        right))))

(defun get-dest-and-operands (cont left-ref right-ref)
  (let ((dest (get-destination cont)))
    (if (not (global-p dest))
        (multiple-value-bind (l r) (get-operands left-ref right-ref)
          (values dest l r))
      (let ((left  (get-left-operand left-ref))
            (right (get-right-operand right-ref)))
        (cond ((and (not (global-p right))
                    (not (global-p left))))
              ((and (same-frame-globals? left right)
                    (not (same-frame-globals? left dest)))
               (setq dest R0))
              ((when (different-frame-globals? left dest)
                 (generate-move left R0)
                 (setq left R0)))
              ((when (different-frame-globals? right dest)
                 (generate-move right R1)
                 (setq right R1)))
              (t))
        (values dest left right)))))



;;; GENERATE-MOVE
;;;
;;; Very general procedure for moving something from anywhere
;;; to anywhere else.
;;;
(defun generate-move (from to &rest options)
  (setq from (acc from))
  (setq to (acc to))
  (setq options (copy-list options))
  (cond ((or (eql from to)
             (eql to IGNORED)
             (eq from undefined)))
        ((and (register-p from)
              (register-p to))
         (emit-move from to))
        ((stack-slot-p from)
         (if (stack-slot-p to)
             (move-stack-slot-to-stack-slot (- from STACK-0)
                                            (- to STACK-0))
           (apply #'generate-move (read-stack-slot (- from STACK-0)) to
                  options)))
        ((and (consp from)
              (eq (car from) 'CLOSURE-REF))
         (generate-closure-ref (cadr from) (cddr from) to))
        ;;; "from closure slot" needs to come before "to stack slot"
        ((stack-slot-p to)
         (when (lambda-node? from)
           (generate-make-closure from R0)
           (setq from R0))
         (generate-move from (write-stack-slot (- to STACK-0))))
        ((and (consp to)
              (eq (car to) 'CLOSURE-REF))
         (generate-closure-set (cadr to) (cddr to) from))
        ((literal-p from)
         (let ((constant (find-constant-register from)))
           (if (and constant
                    (not (different-frame-globals? constant to)))
               (emit-move constant to options)
             (emit-movei from to options))))
        ((different-frame-globals? from to)
         (emit 'K:MOVE R0 from)
         (emit 'K:MOVE to R0))
        ((lambda-node? from)
         (generate-make-closure from to))
        ((and (consp from)
              (eq (car from) 'FUNCTION))
         (apply #'emit 'K:MOVE-PC to (cadr (cadr from)) options))
        (t
         (emit-move from to options))))



(defun generate-lexical-setq (node loc value)
  (let ((loc-acc (acc loc))
        (value-acc (acc value)))
    (generate-move value-acc loc-acc)
    ;; try to get setq's return value from the easier place
    (cond ((register-p loc-acc)
           loc-acc)
          (t
           value-acc))))


;--------------------------------------------------------------------------------
;;;; Arithmetic


(defun generate-dpb (cont value byte-spec word &optional (type-check 'K:DT-NONE)
                                                         (boxed 'K:BOXED-RIGHT)
                                                         (alu-op 'K:FIELD-PASS))
  (multiple-value-bind (dest value word) (get-dest-and-operands cont value word)
    (if (literal-p byte-spec)
        (emit-alu-field alu-op dest value word byte-spec 'K:PW-II type-check boxed)
      (progn
        (emit-alu 'K:LOAD-STATUS-R 'K:NOP
                  IGNORED (get-right-operand byte-spec) '(K:BW-16))
        (emit-alu-field alu-op dest value word ''0 'K:PW-RR type-check boxed)))
    dest))

(defun generate-ldb (cont from byte-spec into &optional (type-check 'K:DT-NONE)
                                                        (boxed 'K:BOXED-RIGHT)
                                                        (alu-op 'K:FIELD-PASS))
  (multiple-value-bind (dest from into)
      (get-dest-and-operands cont from into)
    (if (literal-node? byte-spec)
        (emit-alu-field alu-op dest from into
                        (let ((byte-spec (leaf-value byte-spec)))
                          `(PRIMS:BYTE ,(prims:byte-size byte-spec)
                                       ,(- (prims:byte-position byte-spec))))
                        'K:PW-II type-check boxed)
      (progn
        (emit 'K:MOVE R1 (get-right-operand byte-spec) 'K:BW-16)
        (emit-alu 'K:NEG-R R1 IGNORED R1 '(K:BW-8))
        (emit-alu 'K:LOAD-STATUS-R 'K:NOP
                  IGNORED R1 '(K:BW-16))
        (emit-alu-field alu-op dest from into
                        ''0 'K:PW-RR type-check boxed)))
    dest))

(defun generate-binop (node op rev-op &rest options)
  (destructure (((cont left right) (call-args node)))
    (multiple-value-bind (dest left right) (get-dest-and-operands cont left right)
      (emit-alu op dest left right options)
      dest)))

;;; Generate a binop which is really a unop (because one arg is constant)
;;;  (+ x 4) => (ALU R+4 dest IGNORED x)
(defun generate-degenerate-binop (node arg op &rest options)
  (let ((cont (cont node)))
    (multiple-value-bind (dest right) (get-dest-and-right-operand cont arg)
      (emit-alu op dest IGNORED right options)
      dest)))

(defun generate-degenerate-fixnum-binop (node arg op)
  (let ((cont (cont node)))
    (multiple-value-bind (dest right) (get-dest-and-right-operand cont arg)
      (let ((left (if (or (register-p right) (global-p right)) right 'gr:*zero*))) ;still not good enough
        (emit-alu op dest left right '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
        dest))))


(defun generate-unop (node op  &rest options)
  (destructure (((cont arg) (call-args node)))
    (multiple-value-bind (dest right) (get-dest-and-right-operand cont arg)
      (emit-alu op dest IGNORED right options)
      dest)))

;;; Fixnum unary operations take the operand on the right side.
;;; They also specify DT-BOTH-FIXNUM, so there needs to be a fixnum
;;; (or another copy of the right side) on the left
(defun get-left-side-for-fixnum-unop (right)
  (if (or (register-p right) (global-p right))
      right
    ;; this is not good enough because the destination
    ;; could be another global
    'gr:*zero*))

(defun generate-fixnum-unop (node op)
  (destructure (((cont arg) (call-args node)))
    (multiple-value-bind (dest right) (get-dest-and-right-operand cont arg)
      (let ((left (get-left-side-for-fixnum-unop right)))
        (emit-alu op dest left right '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
        dest))))


(defun generate-load-sr-position (value)
  (emit-alu 'K:LOAD-STATUS-R 'K:NOP
            IGNORED (get-right-operand value) '(K:BW-8)))

(defun generate-load-sr-minus-position (value)
  (emit-alu 'K:NEG-R R1 IGNORED (get-right-operand value) '(K:BW-8))
  (emit-alu 'K:LOAD-STATUS-R 'K:NOP IGNORED R1 '(K:BW-8)))


;--------------------------------------------------------------------------------
;;;; Conditionals

(defun generate-nil-test (node)
  (generate-move (call-arg-n 4 node) 'K:NOP)
  (generate-conditional node 'K:BR-ZERO))


(defun generate-arith-predicate (node cond &rest options)
  (let ((right (get-right-operand (call-arg-n 4 node))))
    (emit-alu 'K:SETR 'K:NOP
              IGNORED right
              options)
  (generate-conditional node cond)))

(defun generate-fixnum-arith-predicate (node cond)
  (let ((right (get-right-operand (call-arg-n 4 node))))
    (emit-alu 'K:SETR 'K:NOP
              (if (or (register-p right)
                      (global-p right))
                  right 'gr:*zero*)
              right
              '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM)))
  (generate-conditional node cond))

(defun comparator (node cond &rest options)
  (let ((left (call-arg-n 4 node))
        (right (call-arg-n 5 node)))
  (multiple-value-bind (l r) (get-operands left right)
    (emit-alu 'K:L-R 'K:NOP l r options))
  (generate-conditional node cond)))


;;; Then continuation of a conditional
(defsubst then-cont (node)
  (car (call-args node)))

;;; Else continuation of a conditional
(defsubst else-cont (node)
  (cadr (call-args node)))


;;; Generate a conditional.  The code has already been generated to
;;; set the condition codes, now generate the test and flow of control
;;; to the then and else continuations.
(defun generate-conditional (node cond)
  (let ((then-cont (then-cont node))
        (else-cont (else-cont node)))
    (let ((then (tension-branch then-cont))
          (else (tension-branch else-cont)))
      ;; normally code looks like:
      ;;
      ;;   <compute condition>
      ;;   (TEST <condition>
      ;;   (BRANCH <else>)
      ;; <then>
      ;;   ...
      ;; <else>
      ;;   ...
      ;;
      ;; but in some cases we wish to invert the conditional:
      ;;   - when THEN  has already been generated
      ;;   - when THEN will probably be a continuation to ELSE

      (let ((then-pos (lambda-queue-position then))
            (else-pos (lambda-queue-position else)))
        (let ((then-before-else (and then-pos else-pos (< then-pos else-pos)))
              (else-before-then (and then-pos else-pos (> then-pos else-pos))))
          (when (or (and (lambda-node? then)
                         (lambda-generated-p then))
                    (null else)
                    else-before-then
                    ;; this is sorta heuristic
                    ;; it makes this win:
                    ;;
                    ;; (defun foo (p)
                    ;;  (if (not p)
                    ;;      (print 'x))
                    ;;  (print 'done))
                    ;;
                    ;; in cases like that the then-cont will call something
                    ;; bound to the if's continuation, the else will do some
                    ;; stuff and then call the if cont.
                    (and then
                         (not then-before-else)
                         (not (lambda-generated-p else))
                         (not (eq then then-cont))
                         (eq else else-cont)))

            ;; invert conditional
            (rotatef then else)
            (rotatef then-cont else-cont)
            (setq cond (inverse-cond cond)))))

      (emit-test cond)
      (if (lambda-node? else)
          (lambda-queue-unless-generated else))
      (if (lambda-node? then)
          (lambda-queue-unless-generated then))
      ;; it is slightly possible for generate-branch
      ;; to lose here (the inversions above try not to let it happen)
      ;; so we use emit-branch instead
      (emit-branch else)
      ;; usually this generates nothing
      ;; because THEN is on the top of the queue
      (if then
          (generate-unconditional-branch then)
        (generate-return-no-values (reference-variable then-cont))))))


(defun generate-jump (label)
  (unless (queued-next-p label)
    (emit-jump label)))

;;; jump but try not to
;;; defsubst
(defun generate-avoid-jump (label)
  (generate-jump label))
;  (emit-avoid-jump 'jmp label nil))


(NOTE "In TENSION-BRANCH, if call args to branch are located
in same place as vars the branch can be tensioned")

(defun tension-branch (label &optional sofar)
  (if (reference-node? label)
      (setq label (reference-variable label)))
  (if (variable-p label)
      (setq label (variable-known label)))
  (if (not (lambda-node? label))
      label
    ;; do branch tensioning
    (let ((call (lambda-body label))
          proc l)
      (or
        (and
          ;; tension GO's
          (primop-ref? (setq proc (call-proc call))
                       primop/%go)
          ;; unless some unwinding is needed
          (not (dynamic-state? (reference-variable (call-arg-n 2 call))))
          (let ((tag (reference-variable (call-arg-n 2 call))))
            ;; avoid losing on infinite loops
            (unless (member tag sofar)
              (tension-branch tag (cons tag sofar)))))


        (and
          ;; calling a known proc?
          (reference-node? (setq proc (call-proc call)))
          (setq l (variable-known (reference-variable proc)))
          ;; if any assignments need to be done
          ;; we can't just branch we have to do the assignments
          (or (null (call-args call))
              (not (some #'(lambda (v)
                             (and v
                                  (variable-refs v)))
                         (lambda-rest+variables l))))
          l)
        label))))

;;; this one queues before...
(defun generate-unconditional-branch (label)
  (setq label (tension-branch label))
  (if (lambda-node? label)
      (progn
        (lambda-queue-unless-generated label)
        (unless (queued-next-p label)
          (emit-unconditional-branch label)))
    (emit-unconditional-branch label)))

;;; this one does queue
(defun generate-branch (label)
  (setq label (tension-branch label))
  (if (lambda-node? label)
      (unless (queued-next-p label)
        (emit-branch label)
        (lambda-queue-unless-generated label))
    (emit-branch label)))

(defun generate-avoid-branch (label)
  (generate-branch label))



;--------------------------------------------------------------------------------
;;;; Special Variables

(defun generate-bind (var &optional (value var))
  (generate-internal-call 'LI:BIND IGNORED `',(variable-name var) value))

(defun generate-unbind (n)
  (case n
    (0)
    (1 (generate-internal-call 'LI:UNBIND-1 IGNORED))
    (t (generate-internal-call 'LI:UNBIND IGNORED `',n))))

;(defun generate-special-unbindings (vars)
;  (let ((count 0))
;    (dolist (var vars)
;      (when (and var
;                (variable-special-p var))
;       (incf count)))
;    (unless (zerop count)
;      (emit 'K:OPEN)
;      (generate-move `',count O0)
;      (generate-general-call 'LI:UNBIND nil IGNORED 1))))

;;; These are now taken care of at alphatization by substituting
;;; calls to %SYMBOL-VALUE etc.

;(defun generate-special-ref (node sym)
;  (emit-alu 'K:R+1 'K:VMA-START-READ IGNORED (get-right-operand sym))
;  (emit 'K:MEMORY-WAIT)
;  'K:MD)

;(defun generate-function-ref (node sym)
;  (emit-alu 'K:R+2 'K:VMA-START-READ IGNORED (get-right-operand sym))
;  (emit 'K:MEMORY-WAIT)
;  'K:MD)

;(defun generate-special-setq (node sym value)
;  (let ((value-acc (acc value)))
;    (generate-move value-acc 'K:MD)
;    (emit-alu 'K:R+1 'K:VMA-START-WRITE IGNORED (get-right-operand sym))
;    (cond ((register-p value-acc)
;          value-acc)
;       (t (emit 'K:MEMORY-WAIT)
;          'K:MD))))


;--------------------------------------------------------------------------------
;;; Argument Setup

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
;;;      ((2x () (optional-init x 'foo nil) (3x))   ;=> ($NOOP) (setq x 'foo) (3x)
;;;       (3x () (optional-init y 'bar y-p) (4x))   ;=> ($NOOP) (setq y 'bar) (setq y-p nil) (4x)
;;;       (4x () (optional-init z (hair) nil) (5x)) ;=> ($NOOP) (setq z (hair)) (5x)
;;;       (5x () (body)))
;;;     (optionals-setup 2x 2 '(nil nil y-p nil) '5x '2x '3x '4x)))
;;;  '<undefined>)
;;;

(defun generate-optional-setup (node)
  (let* ((args (cddr (call-args node)))
         (nargs (leaf-value (car args)))
         (supplied-p-vars (leaf-value (second args)))
         (body-label (variable-known (leaf-value (third args))))
         (inits (mapcar #'(lambda (arg)
                            (variable-known (leaf-value arg)))
                        (cdddr args)))
         (fcn (do ((lambda (node-parent node) (node-parent (node-parent lambda))))
                  ((eq (lambda-strategy lambda) STRATEGY/HEAP) lambda))))
    (setq *nargs-entries* nil)
    ;; set these so that branches will be generated
    ;; to the next init instead of queueing it
    (dolist (init (cdr inits))
      (setf (lambda-generated-p init) t))
    (setf (lambda-generated-p body-label) t)
    (do ((nargs nargs (1+ nargs))
         (inits inits (cdr inits))
         (supplied-tail (cdr supplied-p-vars) (cdr supplied-tail))
         (supplied-p-yet? nil))
        (())
      (let ((tag (create-variable nargs))  ;this is crufty
            (init (car inits)))
        (let ((dynamic-state *dynamic-state*)
              (environment *environment*)
              (env-acc *env-acc*)
              (slots *stack-slots*))
        ;; generate the entry point
        ;; and function entry code
        (push (cons nargs (get-tag tag)) *nargs-entries*)
        (emit-tag tag)
        (generate-entry fcn)  ;get specials
        (do ((vars supplied-p-vars (cdr vars)))
            ((eq vars supplied-tail))
          (when (car vars)
            ;; bind if special
            (generate-move ''T (car vars))))

        (if (null inits) (return nil))
        (generate-lambda init)
        ;; restore state for next init
        (setq *stack-slots* slots
              *env-acc* env-acc
              *environment* environment
              *dynamic-state* dynamic-state))))
    (generate-lambda body-label)))


#||||

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
;;;   (OPEN)                       ;(setq z (hair))
;;;   (CALL HAIR '0 A4)
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
           (progn (bpt)
           (some #'(lambda (init)
                     (some #'unknown-function-p
                           ;; this no longer works because free fcn refs
                           ;; are not on lambda-used
                           (lambda-used (cont (lambda-body init)))))
                 inits)))   )
    (when hairy-inits?
      (dolist (init inits)
        (lambda-queue-at-end init))
      (lambda-queue body-label))
    (setq *nargs-entries* nil)
    (do ((nargs nargs (1+ nargs))
         (inits inits (cdr inits))
         (supplied-tail (cdr supplied-p-vars) (cdr supplied-tail))
         (supplied-p-yet? nil))
        (())
      (let ((tag (create-variable nargs))  ;this is crufty
            (init (car inits)))
        (push (cons nargs (get-tag tag)) *nargs-entries*)
        (emit-tag tag)
        (do ((vars supplied-p-vars (cdr vars)))
            ((eq vars supplied-tail))
          (if (car vars)
            (generate-move ''t (car vars))))
        (if (null inits) (return nil))
        (if hairy-inits?
            (emit-unconditional-branch init)
          (progn
            (when (and (not supplied-p-yet?)
                       (car supplied-tail))
              (setq supplied-p-yet? t)
              (dolist (init (cdr inits))
                (setf (lambda-generated-p init) t))
              (setf (lambda-generated-p body-label) t))
            (generate-lambda init)
            (if (not supplied-p-yet?) (pop *lambda-queue*))))))
    (unless hairy-inits? (generate-lambda body-label))))
||||#

;;; Cons rest argument
;;;
;;; To cons rest args CONS-REST is "called" (not using the regular call mechanism
;;; because we need to access the arguments) with the number (0 based) of the
;;; first arg to cons in *arg-2* and the number of args in *arg-1*.  The consed
;;; rest args are returned in *value-1*.
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
;;;  (call xxx)
;;;  ...
;;;
;;; f is linked to g through a small 2 instruction piece of code
;;; which loads the number of args and jumps to g
;;; xxx
;;;  (movei *arg-1* '6)             ;<n-args>-1
;;;  (jump g)
;;;
;;; g
;;;  (movei *arg-2* '3)             ;<first consed>
;;;  (jump cons-rest (*return-pc-1* trap-pc+))
;;;  (move A3 *value-1*)
;;;  ...
;;;
(defun generate-cons-rest (rest-var)
  (let ((lambda   (variable-binder rest-var)))
    (if (eq (lambda-strategy lambda) STRATEGY/HEAP)
      (let ((acc (acc rest-var)))
        (setf (caar *nargs-entries*) (- (1+ (caar *nargs-entries*))))
        (if (or (eq acc IGNORED)
                (null (variable-refs rest-var)))
            (progn (emit 'K:NOP) (emit 'K:NOP))   ;apply needs this
          (progn
            (generate-move `',(- (length (lambda-variables lambda)) 1) 'GR:*ARG-2*)
            (generate-move 'K:TRAP-PC+ 'GR:*RETURN-PC-1*)
            (emit 'K:JUMP 'LI:CONS-REST))))
      rest-var)))

;;; GENERATE-CONS-REST doesn't store the rest
;;; arg because it might be a stack slot.
;;; Stack slots must be allocated after consing
;;; but before storing.  (see GENERATE-ENTRY)
;;; If the rest arg is closed over that should work.
(defun store-rest-arg (rest-var)
  (let ((lambda   (variable-binder rest-var)))
    (if (eq (lambda-strategy lambda) STRATEGY/HEAP)
        (generate-move 'gr:*value-1* rest-var))))




(defun generate-general-call (name tail-p dest n-args)
  (emit-call name n-args dest tail-p)
  ;; added by smh 15aug88
  (when (> n-args *frame-size*) ;;Changed warn to *frame-size* 9/30/88 --wkf |||
    (warn-the-luser "Continue as if nothing were wrong."
            "Fleabit-compiled general-call of ~s with ~d args in function ~s."
            name
            n-args
            *function-name*)
    (decf *stack-slots* (- n-args *frame-size*))))

;;; tail calls still don't work here
;;; generate-continuation still generates a return
(defun generate-internal-call (name dest &rest args)
  (let ((tail-p (eq dest RETURN))
        (destination (get-destination-from-place dest)))
    (emit (if tail-p 'K:TAIL-OPEN 'K:OPEN))
    (parallel-assign args *open-regs*)
    (generate-general-call name tail-p destination (length args))
    (generate-move destination dest)))

;;; This undoes a TOPEN.
;;; The net effect of TOPEN, TCALL, TOPEN-CALL is:
;;;  R <- HEAP
;;;  HEAP <- R
;;;  O <- A
;;;  A <- A
;;; It does not save R.
(defun generate-flush-topen-frame ()
  ;; HEAP(++HP) <- R
  ;; R <- A
  ;; A <- O
  (emit 'K:TAIL-CALL '(0 0) 'K:NEXT-PC-PC+1)
  ;; O,A <- R
  ;; R <- A
  (emit 'K:TAIL-OPEN-CALL '(0 0) () 'K:NEXT-PC-PC+1))



;;; This moves values to the appropriate place for
;;; an actual function return, including hacking multiple values.
;;; The primary return value goes to value1 which of course defaults
;;; to the RETURN dest, but may need to be different to be saved
;;; across dynamic unwinding code.
(defun generate-assign-return-values (node &optional first-value)
  (let* ((args (call-args node))
         (length (length args)))
    (case length
      (1 (generate-move (car args) (or first-value RETURN)))
      (0 (parallel-assign '('0 'NIL) (list 'GR:*NUMBER-OF-RETURN-VALUES* (or first-value RETURN-MV))))
      (t (parallel-assign `(',length
                            ,@(cdr args)
                            ,(car args))
                          `(GR:*NUMBER-OF-RETURN-VALUES*
                            ,@(subseq *mv-return-registers* 0 (1- length))
                            ,(or first-value RETURN-MV)))))))

;;; (multiple-value-bind (x y z w) (f)
;;; f will return # returned values + 3 in *number-of-return-values*
;;;   ...
;;;
;;;   (call f a0)
;;; (alu-field extract-bit-right nop ignore gr:*number-of-return-values*
;;;        hw:%%processor-status-return-code pw-ii)
;;; (movei r0 '<n+3> br-zero)
;;; (branch one-value (alu l-r nop gr:*number-of-return-values* r0))
;;; (alu l+r nop gr:*number-of-return-values* trap-pc br-greater-or-equal)
;;; (branch all-or-more ())
;;; (nop next-pc-dispatch)
;;; 0 (move a0 'nil)
;;; one-value
;;; 1 (move *return-0* 'nil)
;;; 2 (move *return-1* 'nil)
;;; 3 (move *return-2* 'nil)
;;;   ...
;;; n-1
;;;   (move *return-<n-1>* 'nil)
;;; mvbind
;;;   (move a1 *return-0*)
;;;   (move a2 *return-1*)
;;;   (move a3 *return-2*)
;;;   ...
;;;   (move an-1 *return-<n-1>*)
;;;
;(defun generate-mv-bind (binder)
;  (let* ((vars (lambda-variables binder))
;        (nvars (length vars))
;        (nregs (min (1- nvars) *mv-return-number-in-regs*))
;        (nstack (max 0 (- (length vars) *mv-return-number-in-regs*)))
;        (one-value-tag (gen-tag 'onevalue))
;        (bind-tag (gen-tag 'mvbind)))
;    (emit-code
;      `((K:ALU-FIELD K:EXTRACT-BIT-RIGHT K:NOP ,IGNORED K:PROCESSOR-STATUS
;                    HW:%%PROCESSOR-STATUS-RETURN-CODE K:PW-II)
;       (K:MOVEI ,R0 ',(+ nvars *mv-return-nargs-offset*) K:BR-ZERO)
;       (K:ALU K:L-R K:NOP GR:*NUMBER-OF-RETURN-VALUES* ,R0)
;        (K:BRANCH ,one-value-tag)
;        (K:ALU K:L+R K:NOP GR:*NUMBER-OF-RETURN-VALUES* K:TRAP-PC+ K:BR-GREATER-OR-EQUAL)
;        (K:ALU K:PASS-STATUS K:NOP ,IGNORED ,IGNORED)  ;to compact into branch
;        (K:BRANCH ,bind-tag)
;        (K:NOP K:NEXT-PC-DISPATCH)))
;    (generate-move ''nil (car vars))
;    (emit-tag one-value-tag)
;    (dotimes (i nregs)
;      (generate-move ''nil (nth i *mv-return-registers*)))
;    (dotimes (i nstack)
;      (emit 'K:OPEN-CALL 'LI:STACK-PUSH-NIL IGNORED '()))
;    (emit-tag bind-tag)
;    (let ((*stack-slots* (+ *stack-slots* nstack)))
;      (do ((i 0 (1+ i))
;          (vars (cdr vars) (cdr vars)))
;         ((= i nregs))
;       (generate-move (nth i *mv-return-registers*)
;                      (car vars)))
;      (let ((stack-vars (reverse (nthcdr (1+ *mv-return-number-in-regs*) vars))))
;       (dotimes (i nstack)
;         (decf *stack-slots*)
;         (let ((v (acc (pop stack-vars))))
;           (if (register-p v)
;               (emit 'K:OPEN-CALL 'LI:STACK-POP v '())
;             (progn
;               (emit 'K:OPEN-CALL 'LI:STACK-POP R0 '())
;               (generate-move R0 v)))))))))

;;; (multiple-value-bind (x y z w) (f)
;;; f will return # returned values in *number-of-return-values*
;;;   ...
;;;
;;;   (call f <dest>)
;;;   (open-call (mvbind-4 1) x (o0 <dest>))
;;;   (move y *return-0*)
;;;   (move z *return-1*)
;;;   (move w *return-2*)
;;;
(NOTE "GENERATE-MV-BIND would be better if it took the call dest")
(defun generate-mv-bind (binder)
  (let ((vars (lambda-variables binder)))
    (generate-mv-bind-1 (car vars) (car vars) (cdr vars))))



(defun generate-mv-bind-1 (first-value-in first-value-place rest-values-places)
  (let ((number-of-mvs (length rest-values-places)))
    (let ((mvbind-fcn (nth number-of-mvs '(LI:MVBIND-1 LI:MVBIND-2
                                           LI:MVBIND-3 LI:MVBIND-4
                                           LI:MVBIND-5 LI:MVBIND-6))))
      (if mvbind-fcn
          (generate-internal-call mvbind-fcn first-value-place first-value-in)
        (generate-internal-call 'LI:MVBIND-N first-value-place first-value-in `',(1+ number-of-mvs))))
    (do ((i 0 (1+ i))
         (places rest-values-places (cdr places)))
        ((= i number-of-mvs))
      (generate-move (nth i *mv-return-registers*)
                     (car places)))))




;--------------------------------------------------------------------------------
;;; CATCH

(defun generate-catch-open (node)
  (let ((cont (call-arg-n 2 node)))
    (let ((cont-lambda
            (cond ((reference-node? cont)
                   (variable-known (reference-variable cont)))
                  ((lambda-node? cont)
                   cont)
                  (t (bug "funny catch cont")))))
      (let ((tail-p (wants-to-be-tail-p (cont (lambda-body cont-lambda)))))
        (generate-open-1 tail-p)
        ;; a kluge
        (unless (eq cont cont-lambda)
          (setf (lambda-dynamic-state (variable-binder (reference-variable cont)))
                *dynamic-state*))
        (if (or tail-p (lambda-node? cont))
            (lambda-queue cont-lambda))
        (generate-move ''LI:UNWIND-MARKER     O0)
        (generate-move (call-arg-n 3 node)    O1)
        (generate-move 'GR:*SPECIAL-PDL-PTR*  O2)
        (generate-move 'GR:*STACK-POINTER* 'K:O3)
        (let ((catch-body-cont (cont (lambda-body cont-lambda))))
          (let ((catch-cont (if (lambda-node? catch-body-cont)
                                catch-body-cont
                              (variable-known catch-body-cont))))
            (if (or (null catch-cont)
                    (null (cdr (lambda-variables catch-cont))))
                (emit 'K:MOVE-PC 'K:O4 (get-tag cont-lambda))
              ;;
              ;; if the continuation wants multiple values
              ;; we have to move them from the *return registers
              ;; (where throw puts them) to the right places
              ;;
              (let ((throw-tag (gen-tag 'THROW-CONT)))
                (emit 'K:MOVE-PC 'K:O4 (get-tag throw-tag))
                (emit-unconditional-branch (call-arg-n 1 node))
                (emit-tag throw-tag)
                (generate-mv-bind-1 'K:O5 'K:O5 (cdr (lambda-variables catch-cont)))
                (emit-unconditional-branch (get-tag cont-lambda))))))
        IGNORED))))


;;; The primop CATCH-BODY-VALUES is wrapped around each place where
;;; a CATCH body produces a value.  generate-catch-body-values will look
;;; ahead to the continuation of the body and move the values to the appropriate
;;; place.  This is similiar to generate-continuation and takes the place of that.
;;; (primop/catch-body-values is :special? t)
(defun generate-catch-body-values (node)
  (destructure (((cont . values) (call-args node)))
    (let ((catch-body-cont (cont (lambda-body (variable-known (reference-variable cont))))))
      (let ((catch-cont (if (lambda-node? catch-body-cont)
                            catch-body-cont
                          (variable-known catch-body-cont))))
        (if (null catch-cont)
            (generate-catch-return-values catch-body-cont values)
          (progn
            (cond ((null values)
                   (generate-move ''NIL 'K:O5))
                  ((null (cdr values))
                   (let ((value (car values))
                         (mvars (cdr (lambda-variables catch-cont))))
                     (if (and mvars
                              (value-of-unknown-call? value))
                         (generate-mv-bind-1 value 'K:O5 mvars)
                       (progn
                         (generate-move (car values) 'K:O5)
                         (dolist (v mvars)
                           (generate-move ''NIL v))))))
                  (t ;; order?
                   (generate-move (car values) 'K:O5)
                   (parallel-assign (cdr values) (cdr (lambda-variables catch-cont)))))
            (generate-unconditional-branch cont)))))))


(defun value-of-unknown-call? (value)
  (and (reference-node? value)
       (let ((binder (variable-binder (reference-variable value))))
         (and (call-exit? binder)
              (unknown-call? (node-parent binder))))))

(defun unknown-call? (call-node)
  (let ((proc (call-proc call-node)))
    (and (reference-node? proc)
         (not (variable-known (reference-variable proc))))))


(defun generate-catch-return-values (cont values)
  (pop *dynamic-state*)
  (let ((nvalues (length values)))
    (cond
      ((= 1 nvalues)
       (let ((value (car values)))
         (generate-move value 'K:O5)
         (unwind-dynamic-state (reference-variable cont))
         (emit-call (if (value-of-unknown-call? value)
                        'LI:CATCH-CONTINUE
                      'LI:CATCH-CONTINUE-SV)
                    6 RETURN t)))
      ((= 0 nvalues)
       (generate-move ''0 'GR:*NUMBER-OF-RETURN-VALUES*)
       (generate-move ''NIL 'K:O5)
       (emit-call 'LI:CATCH-CONTINUE-MV 6 RETURN t))
      (t
       (generate-move (car values) 'K:O5)
       (parallel-assign (cdr values) *mv-return-registers*)
       (generate-move `',nvalues
                      'GR:*NUMBER-OF-RETURN-VALUES*)
       (emit-call 'LI:CATCH-CONTINUE-MV 6 RETURN t)))))

(defun generate-catch-continue (node)
     (pop *dynamic-state*)
     (let ((dest (get-destination (call-arg-n 1 node))))
       (emit-call 'LI:CATCH-CONTINUE 6 dest)
       (generate-continuation (if (eq dest RETURN) RETURN-TAIL dest) (cont node) t)))

;--------------------------------------------------------------------------------
;;;; THROW

(defun generate-throw-internal (node)
  (destructure (((cont tag . values) (call-args node)))
    (let ((length (length values)))
      (case length
        (1 (let ((value (first values)))
             (if (value-of-unknown-call? value)
                 (generate-internal-call 'LI:THROW-INTERNAL RETURN tag value)
               (generate-internal-call 'LI:THROW-SV RETURN tag value))))
        (0 (generate-move ''0 'GR:*NUMBER-OF-RETURN-VALUES*)
           (generate-internal-call 'LI:THROW-MV RETURN tag ''NIL))
        (t (generate-move `',(length values) 'GR:*NUMBER-OF-RETURN-VALUES*)
           (parallel-assign (cdr values) *mv-return-registers*)
           (generate-internal-call 'LI:THROW-MV RETURN tag (first values)))))))



;--------------------------------------------------------------------------------
;;;; Closures

(defun generate-make-contour (vars new-env-acc)
  (let ((f (nth (1- (length vars))
                '(LI:MAKE-CONTOUR-1
                  LI:MAKE-CONTOUR-2
                  LI:MAKE-CONTOUR-3
                  LI:MAKE-CONTOUR-4))))
    (if f
        (apply #'generate-internal-call f new-env-acc *env-acc* vars)
      (progn
        (generate-internal-call 'LI:MAKE-CONTOUR 'GR:*VALUE-1* *env-acc* `',(1+ (length vars)))
        (dolist (var vars)
          (generate-copy-to-new-contour var vars))
        (generate-move 'GR:*VALUE-1* new-env-acc)))))



;;; Make a closure of CLOSED-LAMBDA and move it to WHERE.
(defun generate-make-closure (closed-lambda where)
  (let ((fcn (generate-function `(:INTERNAL ,*function-name*
                                            ,(lambda-name closed-lambda))
                                closed-lambda))
        (env (cdr (lambda-env closed-lambda))))
    (cond ((null env)
           ;; When there is no environment
           ;; just move the function object.
           (emit-movei `',fcn where))
          ((equal env *environment*)            ;equal??
           ;; When we already have the environment
           ;; just make a closure with it.
           ;;
           ;;   (MOVEI O0 '<fcn> CH-OPEN)
           ;;   (MOVE O1 <env>)
           ;;   (CALL (MAKE-CLOSURE-WITH-ENV 2) <dest>)
           (generate-internal-call 'LI:MAKE-CLOSURE-WITH-ENV where
                                   `',fcn *env-acc*))
          (t
           (let ((closed-vars (car env)))       ;should make all contours from env to *environment*
             ;; must make both environment and closure
             ;; make-environment (contour?)    (separate fcns for making nested and non-nested contours?)
             ;; make-closure
             ;;
             ;;   (MOVEI O0 '<fcn> CH-OPEN)
             ;;   (MOVEI O1 '<length>)
             ;;   (CALL MAKE-CLOSURE O0)
             ;;   ... set vars in closure ...
             (let ((length (length closed-vars)))
               (let ((f (nth (1- length)
                             '(LI:MAKE-CLOSURE-1
                               LI:MAKE-CLOSURE-2
                               LI:MAKE-CLOSURE-3
                               LI:MAKE-CLOSURE-4))))
                 (if f
                     (apply #'generate-internal-call f where
                            `',fcn *env-acc* closed-vars)
                   (progn
                     (generate-internal-call 'LI:MAKE-CLOSURE where
                                             `',fcn *env-acc*
                                             `',(1+ length))
                     (dolist (v closed-vars)    ;??
                       (generate-copy-to-new-contour v closed-vars)))))))))))


;;; Generate code to copy VARIABLE to the contour
;;; contained in *VALUE-1*.
(defun generate-copy-to-new-contour (variable contour)
  (generate-internal-call 'LI:SET-IN-NEW-CONTOUR IGNORED
                          `',(1+ (position variable contour :test #'eq))
                          (variable-loc variable)))



;;; Generate code to access a closure slot
(defun generate-closure-ref (nback offset where)
  (when (null *env-acc*) (cerror "ok" "Null *env-acc*"))
  (if (zerop nback)                             ;
      (let ((f (nth offset '(LI:CLOSURE-REF-0-1
                             LI:CLOSURE-REF-0-2
                             LI:CLOSURE-REF-0-3
                             LI:CLOSURE-REF-0-4))))
        (if f
            (generate-internal-call f where *env-acc*)
          (generate-internal-call 'LI:CLOSURE-REF-0 where *env-acc* `',(1+ offset))))
      (generate-internal-call  'LI:CLOSURE-REF where *env-acc* `',nback `',(1+ offset))))

(defun generate-closure-set (nback offset value)
  (when (null *env-acc*) (cerror "ok" "Null *env-acc*"))
  (if (zerop nback)
      (let ((f (nth offset '(LI:CLOSURE-SET-0-1
                             LI:CLOSURE-SET-0-2
                             LI:CLOSURE-SET-0-3
                             LI:CLOSURE-SET-0-4))))
        (if f
            (generate-internal-call f IGNORED *env-acc* value)
          (generate-internal-call 'LI:CLOSURE-SET-0 IGNORED *env-acc* `',(1+ offset) value)))
      (generate-internal-call 'LI:CLOSURE-SET IGNORED *env-acc*
                              `',nback
                              `',(1+ offset)
                              value)))


;--------------------------------------------------------------------------------
;;;; Stack Slots

;;; When a function uses more than 16 arguments and local variables
;;; the extras are kept on an extra stack indexed by GR:*STACK-POINTER*.
;;; The number of such slots is in *stack-slots* at compile time.
;;; Accessors to such stack slots are integers of the form STACK-0+<offset>


(defun generate-incr (dest ptr n &optional (reg R0))
; this when alui-16 sign extends to 24 bits
;  (emit 'K:ALUI-16 'K:L+R dest ptr `',n)
  (case n
    (0 (generate-move ptr dest))
    (1 (emit-alu 'K:R+1 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (2 (emit-alu 'K:R+2 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (4 (emit-alu 'K:R+4 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (t (emit 'K:MOVEI reg `',n 'K:BOXED)
       (emit-alu 'K:R+L dest reg ptr '(K:BOXED-RIGHT K:BW-24)))))

(defun generate-decr (dest ptr n &optional (reg R0))
  (case n
    (0 (generate-move ptr dest))
    (1 (emit-alu 'K:R-1 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (2 (emit-alu 'K:R-2 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (4 (emit-alu 'K:R-4 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (t (emit 'K:MOVEI reg `',n 'K:BOXED)
       (emit-alu 'K:R-L dest reg ptr '(K:BOXED-RIGHT K:BW-24)))))


;;; but this shouldn't alloc for args, (interacts with optional args)
(defun generate-alloc-stack-slots (n)
  (when (plusp n)
    (generate-incr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)))

(defun generate-dealloc-stack-slots (n)
  (unless (zerop n)
    (generate-decr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)))

;(defun generate-push (arg)
;  (emit-alu 'K:L+R+1 'GR:*STACK-POINTER* IGNORED 'GR:*STACK-POINTER*)
;  (generate-move 'K:MD arg)
;  (emit 'K:MOVE 'K:VMA-START-WRITE 'GR:*STACK-POINTER*))

(defun read-stack-slot (slot &optional (reg R0))
  (generate-decr '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) 'gr:*stack-pointer*
                 (- *stack-slots* slot) reg)
  (emit 'K:MEMORY-WAIT)
  'K:MD)

(defun write-stack-slot (slot)
  ;; this uses R1 as a temp because if a stack slot is
  ;; the continuation-expecting of something, the value
  ;; will go to R0
  (generate-decr '(K:VMA K:BOXED-VMA) 'GR:*STACK-POINTER* (- *stack-slots* slot) R1)
  '(K:MD-START-WRITE K:BOXED-MD)
  )

(defun move-stack-slot-to-stack-slot (from-slot to-slot)
  (generate-decr '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) 'GR:*STACK-POINTER* (- *stack-slots* from-slot))
  (emit 'K:MEMORY-WAIT)
  (generate-decr '(K:VMA-START-WRITE K:BOXED-VMA) 'GR:*STACK-POINTER* (- *stack-slots* to-slot))
  )


;;; This generates a memory write
;;; but does invisible pointers right
(defun generate-gc-safe-memory-write (ptr new-value return-value vma-dest trap &optional offset)
  (let ((acc (acc new-value)))
    (let ((literal-p (and (literal-p acc)
                          (not (find-constant-register acc)))))
      (unless literal-p
        (setq acc (get-right-operand acc)))
      (emit-alu 'K:R+1 'GR:*ALLOW-SEQUENCE-BREAK* 'GR:*ALLOW-SEQUENCE-BREAK* 'GR:*ALLOW-SEQUENCE-BREAK*
                `(K:BW-24 K:BOXED-RIGHT K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
      (if offset
          (multiple-value-bind (left right) (get-operands offset ptr)
            (emit-alu 'K:L+R  vma-dest left right '(K:BW-24)))
        (generate-move ptr vma-dest trap))
      (if literal-p
        (progn
          (generate-move acc R1)
          (setq acc R1))
        (emit 'K:MEMORY-WAIT))
      (emit-alu 'K:SETL '(K:MD-START-WRITE K:BOXED-MD) acc 'K:MD) ;wait for read and start write
      (emit-alu 'K:L-1 'GR:*ALLOW-SEQUENCE-BREAK* 'GR:*ALLOW-SEQUENCE-BREAK* 'GR:*REQUEST-SEQUENCE-BREAK*
                `(K:BW-24 K:BOXED-LEFT K:DT-RIGHT-LIST))
      return-value)))

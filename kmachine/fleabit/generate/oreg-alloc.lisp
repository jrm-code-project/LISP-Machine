;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-


;;;; Register Allocation

(defvar *regs-used* '())

(defun find-a-reg ()
  (do ((reg A0 (1+ reg)))
      ((> reg AN) (lose))
    (unless (member reg *regs-used*)
      (push reg *regs-used*)
      (return reg))))

(defvar *open-call* nil)
(defvar *open-calls* '())

(defun push-open (call)
  (push *open-call* *open-calls*)
  (setq *open-call* call))

(defun pop-open ()
  (setq *open-call* (pop *open-calls*)))


(defun regloc (var)
  (let ((loc (variable-loc var)))
    (if (variable-p loc)
        (regloc loc)
      loc)))

(defun mark-var-in-reg (var reg)
  (debug :regs
      (format t "~&~a: ~a"
              (variable-unique-name var)
              (if (variable-p reg)
                  (variable-unique-name reg)
                reg)))
  (if (integerp reg) (push reg *regs-used*))
  (setf (variable-loc var) reg))

(defun mark-vars-in-arg-regs (vars)
  (do ((reg A0 (1+ reg))
       (vars vars (cdr vars)))
      ((or (null vars)
           (and (>= reg AN)
                (lose))))
    (mark-var-in-reg (car vars) reg)))


(defun reg-alloc-top (node)
  (setq *regs-used* ())
  (reg-alloc node))

(defun reg-alloc (node)
  (when (lambda-node? node)
    (let ((*regs-used* *regs-used*))
      (ecase (lambda-strategy node)
        ((STRATEGY/HEAP STRATEGY/PROC)
         ;; Heaped lambdas take their args in
         ;; the standard arg regs
         ;; Procs don't really have to
         ;; buts its probably more efficient in
         ;; K machine
         (mark-vars-in-arg-regs (cdr (lambda-variables node))))
        (STRATEGY/OPEN   ;(STRATEGY/STACK STRATEGY/OPEN)
         ;; this is a continuation for a call
         ;; if it takes 1 arg as usual (call returns 1 value)
         ;; it will be in wherever it wants it because
         ;; because OPEN will arrange the dest to have it there
         ;; (is this always a cont to an opened call?
         ;; most primops can also return in wherever we want
         ;; but some might not?)
         ;; (can also be con't to label call but that can
         ;; also go where it wants)
         (dolist (var (lambda-rest+variables node))
           (if var
               (mark-var-in-reg var (var-target var node)))))
        (STRATEGY/LABEL
         (let ((rest (lambda-rest-var node)))
           (if rest
               (mark-var-in-reg rest (var-target rest node))))
         (dolist (var (cdr (lambda-variables node)))
           (mark-var-in-reg var (var-target var node))))
        (STRATEGY/LABEL ???))
      (let ((call (lambda-body node)))
        ;; this is not right because
        ;; there might not really be an open??
        (cond ((primop-node? (call-proc call))
               (let ((primop (leaf-value (call-proc call))))
                 (cond
                   ((eq primop primop/open) (push-open (leaf-value (call-arg-n 2 call))))
                   ;; why does the y-lambda have same strategy as procs?
                   ((eq primop primop/y) (setq call (lambda-body (call-arg-n 1 call)))))))
              ((eq call *open-call*)
               (pop-open)))
        (dolist (arg (call-proc+args call))
          (reg-alloc arg))))))



;;; return the place where a variable
;;; would like to keep it's value
;;; either a register, IGNORE meaning the value is not used
;;; or a variable meaning in the same place as that var
(defun var-target (var lambda-node)
  (let* ((refs (variable-refs var))
;        (x (cerror "foo" "var-target"))
         (reg (cond ((null refs)                ;no refs
                     'IGNORE)
                    ((null (cdr refs))          ;one ref
                     (ref-target (car refs) lambda-node))
                    (t (let ((targets '()))
                         (dolist (ref refs)
                           (pushnew (ref-target ref lambda-node) targets))
                         (if (null (cdr targets))
                             (car targets)
                           (setq targets (delete-ignored targets))
                           (if (null (cdr targets))
                               (car targets)
                             (setq targets (delete '* targets))
                             (if (and targets
                                      (null (cdr targets)))
                                 (car targets)
                               '*))))))))   ;this could be better
    (if (eq reg '*) (find-a-reg) reg)))

(defun delete-ignored (targets)
  (delete-if #'(lambda (elt)
                 (or (eq elt 'IGNORE)
                     (and (variable-p elt)
                          (or (eq (variable-loc elt) 'IGNORE)
                              (null (variable-refs elt))))))
             targets))




;;; Return the place where the given reference
;;; would like to find its value
;;; returns a register, * for any register,
;;; or a variable meaning the same place as that var
(defun ref-target (ref lambda-node)
  (let* ((parent (node-parent ref))
         (proc (call-proc parent))
         (number (call-arg-number (node-role ref))))
    (cond ((primop-node? proc)
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
                               (or (eq arg ref)
                                   (literal-node? ref)))
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
                          (let ((var (nth (1- number) (lambda-variables label))))
                            (cond ((null var)
                                   (setq var (lambda-rest-var label))
                                   (if (and var
                                            ;; ignored rest
                                            (null (variable-refs var)))
                                       'IGNORE
                                     (cerror "foo" "non ignored rest or bad # args in reg-alloc")))
                                  (;; still problems...
                                   ;; (do ((a 0 b) (b 1 a)) (()))
                                   (or
                                     ;; this reference could want to be targeted
                                     ;; to a variable which is already targeted to it
                                     ;; (do ((a 0 (1+ a)) ...
                                     (eq (variable-loc var) (reference-variable ref))
                                     ;; this reference might want to be targeted
                                     ;; to its own variable (do ((a 0 a)) ...
                                     (eq var (reference-variable ref))
                                     ;; var will be bound to a var, but that var still needed
                                     ;; ???
                                     (member var (lambda-live lambda-node)))
                                   '*)
                                  (t var))))
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
               ((eq parent *open-call*)
                (- (+ (1- number) O0)
                   (call-exits parent)))
               (t '*)))))))



#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

(defun foo ()
  (cons '+ (let ((l))
             (bar (setq l 3))
             l)))

Unsimplified node:
7315967    ((T_5 NIL C_4) ($*DEFINE 1 ^B_20 FOO ^P_6))   NIL
7317230     ((B_20 IGNORE_19) (C_4 0 (QUOTE T)))   NIL
7316100     ((P_6 NIL K_0) ($OPEN 1 ^C_18 (QUOTE #{NC::CALL-NODE (CONS 1 K_0 (QUOTE +) V_16) 7316138})))   NIL
7317067      ((C_18)    ($OPEN 1 ^C_15 (QUOTE #{NC::CALL-NODE (^P_8 1 ^C_17 (QUOTE NIL)) 7316246})))   NIL
7316881       ((C_15)    (^P_8 1 ^C_17 (QUOTE NIL)))   NIL
7316273        ((P_8 NIL K_1 L_2) ($OPEN 1 ^C_12 (QUOTE #{NC::CALL-NODE (BAR 1 ^B_14 V_10) 7316311})))   NIL
7316625         ((C_12)    ($SETQ-LEXICAL 1 ^C_11 L_2 (QUOTE 3)))   NIL
7316546          ((C_11 NIL V_10) (BAR 1 ^B_14 V_10))   NIL
7316757           ((B_14 IGNORE_13) (K_1 0 L_2))   NIL
7316988        ((C_17 NIL V_16) (CONS 1 K_0 (QUOTE +) V_16))   NIL
Simplified tree:
7315967    ((T_5 NIL C_4) ($*DEFINE 1 ^B_20 FOO ^P_6))   NIL
7317230     ((B_20 IGNORE_19) (C_4 0 (QUOTE T)))   NIL
7316100     ((P_6 NIL K_0) ($OPEN 1 ^C_18 (QUOTE #{NC::CALL-NODE (CONS 1 K_0 (QUOTE +) L_2) 7316138})))   NIL
7317067      ((C_18)    ($OPEN 1 ^C_15 (QUOTE #{NC::CALL-NODE (^P_8 0 (QUOTE NIL)) 7316246})))   NIL
7316881       ((C_15)    (^P_8 0 (QUOTE NIL)))   NIL
7316273        ((P_8 NIL L_2) ($OPEN 1 ^C_12 (QUOTE #{NC::CALL-NODE (BAR 1 ^B_14 V_10) 7316311})))   NIL
7316625         ((C_12)    ($SETQ-LEXICAL 1 ^C_11 L_2 (QUOTE 3)))   NIL
7316546          ((C_11 NIL V_10) (BAR 1 ^B_14 V_10))   NIL
7316757           ((B_14 IGNORE_13) (CONS 1 K_0 (QUOTE +) L_2))   NIL

P_6
  (TAIL-OPEN)
  (MOVE O1 (QUOTE NIL))
  (KOPEN)
  (MOVE O1 (QUOTE 3))
  (MOVE O0 O1)
  (KCALL BAR (QUOTE 1) IGNORE)
B_14
  (MOVE O0 (QUOTE +))

It is not valid to allocate to an open reg if other references
are within the scope of another open

||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

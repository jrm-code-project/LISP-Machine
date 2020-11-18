;;; -*- Mode:LISP; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;; this is crufty, see nnreg-alloc

(defun reg-alloc-top (node)
  (hack-open node nil nil)
  (let ((*regs-used* '()))
    (reg-alloc node)))


(zl:defsubst lambda-open (node)
  (lambda-env node))

(defun hack-open (lambda-node open-call nested-opens)
  (format t "~%~a: ~a" lambda-node open-call)
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


(defvar *regs-used* '())

(defun note-used (loc)
  (if (integerp loc) (push loc *regs-used*))
  loc)

(defun find-a-reg ()
  (do ((reg A0 (1+ reg)))
      ((> reg AN) (cerror "foo" "need a local slot, alloc to stack"))
    (unless (member reg *regs-used*)
      (return reg))))

(defun mark-var-in-reg (var reg)
  (debug :regs
      (format t "~&~a: ~a"
              (variable-unique-name var)
              (if (variable-p reg)
                  (variable-unique-name reg)
                reg)))
  (note-used reg)
  (setf (variable-loc var) reg))

(defun mark-vars-in-arg-regs (vars)
  (do ((reg A0 (1+ reg))
       (vars vars (cdr vars)))
      ((or (null vars)
           (and (>= reg AN)
                (cerror "foo" "need an arg slot, alloc to stack"))))
    (mark-var-in-reg (car vars) reg)))


(defun reg-alloc (node)
  (let ((*regs-used* *regs-used*))
    (ecase (lambda-strategy node)
      ((STRATEGY/HEAP STRATEGY/PROC)
       ;; Heaped lambdas take their args in
       ;; the standard arg regs
       ;; Procs don't really have to
       ;; buts its probably more efficient in
       ;; K machine
       (mark-vars-in-arg-regs (cdr (lambda-variables node))))
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
         (if var (find-var-location var () node))))
      (STRATEGY/LABEL
       ;; Labels take their args wherever they like
       (let ((rest (lambda-rest-var node)))
         (if rest
             (find-var-location rest () node)))
       (dolist (var (cdr (lambda-variables node)))
         (find-var-location var () node))))
    (let ((call (lambda-body node)))
      (if (eq (call-proc call) primop/y)
          (setq call (lambda-body (call-arg-n 1 call))))
      (dolist (arg (call-proc+args call))
        (when (lambda-node? arg)
          (reg-alloc arg))))))



;;; return the place where a variable will be located
;;; either a register or IGNORE meaning the value is not used
;;; (or stack slot or ???)
(defun find-var-location (var finding &optional (lambda-node (variable-binder var)))
  (cond ((member var finding)
         '*)
        ((variable-loc var)
         ;; already found
         (note-used (variable-loc var)))
        (t
    (let* ((refs (variable-refs var))
;          (x (cerror "foo" "var-target"))
           (reg (cond ((null refs)              ;no refs
                       'IGNORE)
                      ((null (cdr refs))        ;one ref
                       (ref-target (car refs) var finding lambda-node))
                      (t (let ((targets '()))
                           (dolist (ref refs)
                             (pushnew (ref-target ref var finding lambda-node) targets))
                           (if (null (cdr targets))
                               (car targets)
                             (setq targets (delete-ignored targets))
                             (if (null (cdr targets))
                                 (car targets)
                               (setq targets (delete '* targets))
                               (if (and targets
                                        (null (cdr targets)))
                                   (car targets)
                                 (first-A-reg targets)) ;???
                               )))))))
      (mark-var-in-reg var
                       (if (or (eq reg 'A*)
                               (eq reg '*)
                               ;; when a var is targeted to another var which
                               ;; is allocated, then var wants to be in the same
                               ;; reg, unless it is already taken
                               ;; This is grossly non-optimal because
                               ;; some other var in the same binding list
                               ;; might have been just alloced and not really targeted
                               ;; to the same place
                               (dolist (other-var (lambda-rest+variables lambda-node))
                                 (and other-var
                                      (eql reg (variable-loc other-var))
                                      (return t))))
                           (find-a-reg)
                         reg))))))

(defun delete-ignored (targets)
  (delete-if #'(lambda (elt)
                 (or (eq elt 'IGNORE)
                     (and (variable-p elt)
                          (or (eq (variable-loc elt) 'IGNORE)
                              (null (variable-refs elt))))))
             targets))

(defun first-A-reg (targets)
  (dolist (target targets 'A*)
    (when (and (integerp target)
               (>= target A0)
               (<= target AN))
      (return target))))

;;; Return the place where the given reference
;;; would like to find its value
;;; returns a register,
;;; * for anywhere (we really don't care)
;;; A* to allocate an A reg
;;; IGNORE for nowhere
(defun ref-target (ref var finding lambda-node)
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
                    (find-var-location (reference-variable (call-arg-n 2 parent))
                                       (cons var finding)))
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
                            (if cvar
                                (find-var-location cvar (cons var finding))
                              '*))
                        '*))
                    '*)))))
          ((lambda-node? proc)
           (find-var-location (nth (1- number)
                                   (lambda-variables proc))
                              (cons var finding)))
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
                            (cond ((null label-var)
                                   (setq label-var (lambda-rest-var label))
                                   (if (and label-var
                                            ;; ignored rest
                                            (null (variable-refs label-var)))
                                       'IGNORE
                                     (cerror "foo" "non ignored rest or bad # args in reg-alloc")))
                                  (;; still problems...
                                   ;; (do ((a 0 b) (b 1 a)) (()))
                                   (or
                                     ;; this reference could want to be targeted
                                     ;; to a variable which is already targeted to it
                                     ;; (do ((a 0 (1+ a)) ...
                                     (eq (variable-loc label-var) (reference-variable ref))
                                     ;; this reference might want to be targeted
                                     ;; to its own variable (do ((a 0 a)) ...
                                     (eq label-var (reference-variable ref))
                                     ;; var will be bound to a var, but that var still needed
                                     ;; ???
                                     (member label-var (lambda-live lambda-node)))
                                   '*)
                                  (t (find-var-location label-var
                                                        (cons var finding)
                                                        label)))))
                         (STRATEGY/OPEN
                          ;; This happens when a continuation to a label call
                          ;; is set as the known value of the labels continuation
                          ;;  (foo (do (...
                          ;; it might be more tasteful to change the open to a label
                          ;; but then allocation screws up because there is no
                          ;; continuation arg...
                          (find-var-location (nth (1- number)
                                                  (lambda-variables label))
                                             (cons var finding)))
                         (STRATEGY/PROC
                          (cerror "Foo" "allocating arg to STRATEGY/PROC call"))))))
               ;; unknown proc, return open reg corresponding
               ;; to arg position of ref
               ((eq parent (lambda-open lambda-node))
                (- (+ (1- number) O0)
                   (call-exits parent)))
               (t 'A*)))))))

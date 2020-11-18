;;; -*- Mode:LISP; Readtable:CL; Base:10 -*-


;;; To do:
;;;  - put entry points into lambda node
;;;     check for known calls
;;;  - specified-p ars




;;; Take a lambda list and body, and return a modified body
;;;  and an arglist containing no lambda-list keywords.
;;; This needs to put declarations in the right places.
;;;
;;;  (defun foo (x y &optional (a (hair)) (b 'bar))
;;;    (body))
;;;
;;; becomes:
;;;
;;;  (defun foo (x y a b)
;;;    (tagbody
;;;      2
;;;       (optional-setup 2)
;;;       (setq a (hair))
;;;      3
;;;       (optional-setup 3)
;;;       (setq b 'bar)
;;;      4
;;;       (nargs-entry 4)
;;;       (progn
;;;         (body))))
;;;
;;; There is a bug with this in that the <var> should not
;;;  be lexically apparent in the init-form for <var>.
;;; Note also the following obscure bug,
;;;  maybe we need to gensym the tags after all
;;;
;;;  (defun glitch ()
;;;    (tagbody
;;;      1
;;;        (print 'out-of-f)
;;;        (labels ((f (&optional x)
;;;                   (print 'in-f)
;;;                   (go 1)))
;;;           (f))))
;;;
(defun hack-lambda-list (lambda-list body)
  (declare (values arglist body))
  (multiple-value-bind (args optionals rest keys allow-other-keys-p auxes)
      (parse-lambda-list lambda-list)
    (when auxes
      (setq body
            `((let ,auxes
                . ,body))))
    (when keys
      (unless rest (setq rest (gensym)))
      (setq body `((with-keyword-args ,rest ,keys ,allow-other-keys-p
                         . ,body))))
    (values (append args (mapcar #'car optionals) rest)
            (if optionals
                `((tagbody
                      ,@(do* ((ops optionals (cdr ops))
                              (nargs (length args) (1+ nargs))
                              (forms (list nargs)
                                     (cons nargs forms)))
                             ((null ops)
                              (nreverse (cons `(nargs-entry ,nargs) forms)))
                          (setq forms
                                (cons `(setq ,(caar ops)
                                             ,(cadar ops))
                                      (cons `(optional-setup ,nargs)
                                            forms))))
                      (progn ,@body)))
                body))))



;;; Make the variables for a lambda, add them to the shape, alphatize the
;;; body, and then remove the variables from the shape.

(defun alpha-lambda (name lambda-list body syntax shape fshape)
  (multiple-value-bind (var-names body)
      (hack-lambda-list lambda-list body)
    (let* ((vars (map! #'(lambda (name)
                           (if name (create-variable name)))
                       (fix-vars (cons 'k var-names))))
           (real-vars (cons (car vars) (cddr vars))))
      (bind-variables shape real-vars)
      (let ((exp (list syntax/lambda name vars
                       (alpha-list body syntax shape fshape))))
        (unbind-variables shape real-vars)
;      (return-to-freelist real-vars)
        exp))))


(define-constant-primop optional-setup
  (primop.generate (self node)
    self
    (generate-optional-setup node))
  (primop.side-effects? t)
  (primop.special? t))


;;; Returns T if VAR is not bound
;;; and is called as a function
(defun unknown-function-p (var)
  (and (null (variable-binder var))
       (some #'(lambda (ref)
                 (eq (node-role ref) call-proc))
             (variable-refs var))))


(defun generate-optional-setup (node)
  (generate-nargs-entry node)
  (let ((cont (call-arg-n 1 node)))
    (if (some #'unknown-function-p
                 (lambda-live cont))
        (progn
          ;; jump to setup code at end
          (generate-jump cont)
          ;; queue it also
          (setq *lambda-queue* (append *lambda-queue* (list cont)))
          ;; generate the next setup
          ;; this is quite a crock...
          (let ((entry (node-parent node)))
            (generate-lambda
              (nth (- (call-arg-number (node-role entry)) 2)
                   (call-args (node-parent entry))))))
        ;; if there are no function calls,
        ;; just generate the setup in-line
        (generate-call (lambda-body cont)))))


(define-constant-primop nargs-entry
  (primop.generate (self node)
    self
    (generate-nargs-entry node))
  (primop.side-effects? t))


(defun generate-nargs-entry (node)
   (push (cons (leaf-value (call-arg-n 2 node))
               (node-parent node))
         *nargs-entries*))


----------------------------------------------------------------


(defun foo (x y &optional (a 'foo) (b 'bar b-p) (c (hair)))
  (body))

nargs-entry-offsets ((2 . 0) (3 . 1) (4 . 4)(5 . 6))

these are fixed offsets into the code for FOO, they
can be fixed because there are no function calls in
the args initialization, all function calls come at
the end

FOO
2-ARGS
  (MOVE A2 'FOO)   ;(setq a 'foo)
3-args
  (MOVE A3 'BAR)   ;(setq b 'bar)
  (MOVE A4 'NIL)   ;(setq b-p nil)
  (JUMP 4-ARGS-1)
4-ARGS
  (MOVE A4 'T)     ;(setq b-p t)
4-ARGS-1
  (JUMP 4-ARGS-AUX)
5-ARGS
  (TAIL-OPEN)
  (TAIL-CALL BODY)
4-args-aux
  (OPEN)            ;(setq c (hair))
  (CALL HAIR 0 A5)
  (JUMP 5-ARGS)


(defun foo (x y &optional (a 'foo) (b (hair))) (body))

;;; this is not quite right
;;; because 3-args-aux is cleverly moved
;;; up to 3-args, we need to guarentee that
;;; it is at the bottom of the lambda-queue
;;; instead of (go 3-args-aux), write (jump 3-args-aux)
;;; JUMP will be like GO except it will have a continuation
;;; (what it would fall into, i.e. 4-args) which is queued before the label...
;;;
;;; (this turns into ($JUMP ^B_69 3-ARGS-AUX_88), where both args are
;;; exit args to prevent 3-args-aux from becoming STRATEGY/HEAP
;;;
;;; (but where does 3-args-aux get generated...)
(defun foo (x y a b)
  (tagbody
   2-args
      (setq a 'foo)
   3-args
      (jump 3-args-aux)
   4-args
      (return-from foo (body))
   3-args-aux
      (setq b (hair))
      (go 4-args)))



(defun foo (x y a b)
  (tagbody
    2
      (optional-setup 2)
      (setq a (hair))
    3
      (optional-setup 3)
      (setq b 'bar)
    4
      (nargs-entry 4)
      (body)))


(defun foo (x y a b)
  (tagbody
    2-args
      (optional-setup 2
        (setq a 'foo))
    3-args
      (optional-setup 3
        (setq b 'bar))
    4-args
      (nargs-entry 4)
      (body)))

we can't tell if there are function calls until after node conversion
(generation time?)

((2-args ...) ($optional-setup 1 ^b_3 2))
 ((b_3 ...) ($setq ^c_4 a_1 'foo))
  ((c_4 ...) (3-args 0 'nil))

optional-setup can take 2 args
the first is an exit arg (so it doesn't get heaped)
  which is the code to setup the arg
the second is the # of args to jump here on
  it builds the entry list from this and its node parent
optional-setup checks the setup code for function calls
if none it generates it
   else it generates a jump and queues the setup code at the end
        then somehow it must queue the next setup....


(define-compiler-syntax (JUMP tag) (syntax shape fshape)
  (let (tagvar)
    (if (variable-p tag)
        (setq tagvar tag)
      (setq tagvar (assoc tag *tags*))
      (if tagvar
          (setq tagvar (cdr tagvar))
        (if (or (symbolp tag)
                (integerp tag))
            (error "There is a JUMP to tag ~a but no such tag exists" tag)
          (error "Invalid JUMP tag: ~a, only symbols or integers allowed" tag))))
    `(,syntax/jump ,tagvar)))


(define-compilator (JUMP tag)
  ;; no unwinding here
  ;; won't work in losing cases
  (let ((node (make-call-with-exits 2 (list primop/jump empty tag))))
    (values node node (call-arg 1))))


(define-constant-primop jump
  (primop.generate (self node)
    self
    (generate-primop-jump node))
  (primop.special? t))

(defun generate-primop-jump (node)
  (let ((label (variable-known (reference-variable (call-arg-n 2 node)))))
    (generate-jump label)
    (unless (member label *lambda-queue*)
      (setq *lambda-queue* (append *lambda-queue* (list label))))
    (generate-call (lambda-body (call-arg-n 1 node)))))    ;call continuation



----------------------------------------------------------------
;;; A setup-form is a list whose car is the tag and whose cdr is a list
;;; of body forms (one unless there is a supplied-p arg)
;;;
;;; (defun foo (x y &optional (a 'foo) (b 'bar))
;;;   (body))
;;;
;;; (optional-setup
;;;   ((2-args (setq a 'foo))
;;;    (3-args (setq b 'bar)))
;;;   ((body)))
;;;
(define-compiler-syntax (optional-setup setup-forms body) (syntax shape fshape)
  (list syntax/optional-setup
        (mapcar #'(lambda (setup-form)
                    ;; no vars (no cont?)
                    (list syntax/lambda (car setup-form) (list nil (create-variable 'ignore))
                       (alpha-list (cdr setup-form) syntax shape fshape)))
                    ;(alpha-lambda (car setup-form) nil (cdr setup-form)
                    ;              syntax shape fshape))
                setup-forms)
        (alpha-list body syntax shape fshape)))


(define-compilator (optional-setup setup-forms body)
  ;; the body is made into a lambda node like make-lambda except that
  ;; instead of having the lambda take a continuation,
  ;; the continuation of optional-setup (the user lambda's cont var)
  ;; is put into the body lambda
  (let ((body-lambda (create-lambda-node 'body (list nil))))
    (multiple-value-bind (value-node c-parent c-role)
        (make-block body)
      (relate lambda-body body-lambda value-node)
      (let ((call (make-call-with-exits
                    (+ (length setup-forms) 1)  ;+ body
                    (list* primop/optional-setup body-lambda setup-forms))))
        (values call c-parent c-role)))))



(define-constant-primop optional-setup
  (primop.generate (self node)
    self
    (generate-optional-setup node)))

(defun generate-optional-setup (node)
  (let* ((args (call-args node))
         (body (first args))
         (setups (rest args)))
    (dolist (setup setups)
      (let ((setup-form (call-arg-n 3 (lambda-body setup))))
        (if (and (lambda-node? setup-form)) ;any unknown calls
            (let ((new-tag (create-variable
                             (concatenate-symbol
                               (variable-name (lambda-self-var setup-form))
                               '-aux))))
              (emit-tag setup-form)
              (setf (lambda-self-var setup-form) new-tag)
              (generate-jump setup-form)
              (push setup-form *lambda-queue*))    ;need jump back...
            (generate-lambda setup))))        ;just fall into each other
    (generate-lambda body)))

----------------------------------------------------------------
rest

----------------------------------------------------------------

(defun foo (a b c &key x (y 3) (z 'foo))
  (list a b c x y z))


The variable KSI::KEYWORD-GARBAGE is used free; assumed special
The variable KSI::KEYWORD-GARBAGE is used free; assumed special
The variable KSI::KEYWORD-GARBAGE is used free; assumed special
Alphatized:
(SYNTAX/LAMBDA
 FOO
 (#{Variable REST1411_0} #{Variable K_1} #{Variable A_2} #{Variable B_3} #{Variable C_4})
 ((SYNTAX/MULTIPLE-VALUE-BIND
   (#{Variable X_5} #{Variable Y_6} #{Variable Z_7})
   (KSI::GET-KEYWORD-ARG-VALUES #{Variable REST1411_0} (SYNTAX/QUOTE (:X :Y Z)) NIL)
   (((SYNTAX/LAMBDA P
                    (NIL #{Variable K_8})
                    ((SYNTAX/IF (EQ #{Variable X_5} (SYNTAX/SPECIAL-REF #{LITERAL KSI::KEYWORD-GARBAGE 10218843}))
                                (SYNTAX/PROGN ((SYNTAX/SETQ-LEXICAL #{Variable X_5} NIL) NIL))
                                NIL)
                     (SYNTAX/IF (EQ #{Variable Y_6} (SYNTAX/SPECIAL-REF #{LITERAL KSI::KEYWORD-GARBAGE 10218896}))
                                (SYNTAX/PROGN ((SYNTAX/SETQ-LEXICAL #{Variable Y_6} 3) NIL))
                                NIL)
                     (SYNTAX/IF (EQ #{Variable Z_7} (SYNTAX/SPECIAL-REF #{LITERAL KSI::KEYWORD-GARBAGE 10218949}))
                                (SYNTAX/PROGN ((SYNTAX/SETQ-LEXICAL #{Variable Z_7} (SYNTAX/QUOTE FOO)) NIL))
                                NIL)
                     (LISTN #{Variable A_2}
                            #{Variable B_3}
                            #{Variable C_4}
                            #{Variable X_5}
                            #{Variable Y_6}
                            #{Variable Z_7})))
     )
    ))))
Substituting: K_8 := K_1
Substituting: V_67 := (QUOTE T)
Substituting: V_65 := (QUOTE NIL)
Substituting: C_61 := ^C_39
Substituting: C_63 := ^C_42
Substituting: V_80 := (QUOTE T)
Substituting: V_78 := (QUOTE NIL)
Substituting: C_74 := ^C_26
Substituting: C_76 := ^C_29
Substituting: V_93 := (QUOTE T)
Substituting: V_91 := (QUOTE NIL)
Substituting: C_87 := ^C_15
Substituting: C_89 := ^C_18
Simplified tree:
10228164    ((FOO_9 REST1411_0 K_1 A_2 B_3 C_4) ($OPEN-FRAME 1 ^C_11 (QUOTE #{CALL-NODE (GET-KEYWORD-ARG-VALUES 1 ^R_55 REST1411_0 (QUOTE (X Y Z)) (QUOTE NIL)) 10228202})))   NIL
10228400     ((C_11)   (GET-KEYWORD-ARG-VALUES 1 ^R_55 REST1411_0 (QUOTE (X Y Z)) (QUOTE NIL)))   NIL
10232367      ((R_55 NIL X_5 Y_6 Z_7) (^P_14 1 ^B_36))   NIL
10228556       ((P_14 NIL J_13) ($SPECIAL-REF 1 ^C_20 (QUOTE KEYWORD-GARBAGE)))   NIL
10229340        ((C_20 NIL V_19) ($CONDITIONAL 2 ^C_15 ^C_18 $EQ X_5 V_19))   NIL
10228615         ((C_15 NIL) ($SETQ-LEXICAL 1 ^B_17 X_5 (QUOTE NIL)))   NIL
10228835          ((B_17 IGNORE_16) (J_13 0 (QUOTE NIL)))   NIL
10228894         ((C_18 NIL) (J_13 0 (QUOTE NIL)))   NIL
10230653       ((B_36 IGNORE_35) (^P_25 1 ^B_49))   NIL
10229605        ((P_25 NIL J_24) ($SPECIAL-REF 1 ^C_31 (QUOTE KEYWORD-GARBAGE)))   NIL
10230389         ((C_31 NIL V_30) ($CONDITIONAL 2 ^C_26 ^C_29 $EQ Y_6 V_30))   NIL
10229664          ((C_26 NIL) ($SETQ-LEXICAL 1 ^B_28 Y_6 (QUOTE 3)))   NIL
10229884           ((B_28 IGNORE_27) (J_24 0 (QUOTE NIL)))   NIL
10229943          ((C_29 NIL) (J_24 0 (QUOTE NIL)))   NIL
10231760        ((B_49 IGNORE_48) (^P_38 1 ^B_53))   NIL
10230712         ((P_38 NIL J_37) ($SPECIAL-REF 1 ^C_44 (QUOTE KEYWORD-GARBAGE)))   NIL
10231496          ((C_44 NIL V_43) ($CONDITIONAL 2 ^C_39 ^C_42 $EQ Z_7 V_43))   NIL
10230771           ((C_39 NIL) ($SETQ-LEXICAL 1 ^B_41 Z_7 (QUOTE FOO)))   NIL
10230991            ((B_41 IGNORE_40) (J_37 0 (QUOTE NIL)))   NIL
10231050           ((C_42 NIL) (J_37 0 (QUOTE NIL)))   NIL
10232196         ((B_53 IGNORE_52) ($OPEN-FRAME 1 ^C_51 (QUOTE #{CALL-NODE (LISTN 1 K_1 A_2 B_3 C_4 X_5 Y_6 Z_7) 10231798})))   NIL
10232104          ((C_51)   (LISTN 1 K_1 A_2 B_3 C_4 X_5 Y_6 Z_7))   NIL

Register preference classes:
(*  V_43)
(IGNORE  J_37)
(*  V_30)
(IGNORE  J_24)
(*  V_19)
(IGNORE  J_13)
(A*  Z_7)
(A*  Y_6)
(A*  X_5)
(2  C_4)
(1  B_3)
(0  A_2)
(A*  REST1411_0)
V_43: 3
J_37: IGNORE
V_30: 4
J_24: IGNORE
V_19: 5
J_13: IGNORE
Z_7: 6
Y_6: 7
X_5: 8
C_4: 2
B_3: 1
A_2: 0
REST1411_0: 9
Generating:
10228164    ((FOO_9 REST1411_0 K_1 A_2 B_3 C_4) ($OPEN-FRAME 1 ^C_11 (QUOTE #{CALL-NODE (GET-KEYWORD-ARG-VALUES 1 ^R_55 REST1411_0 (QUOTE (X Y Z)) (QUOTE NIL)) 10228202})))   STRATEGY/HEAP
10228400     ((C_11)   (GET-KEYWORD-ARG-VALUES 1 ^R_55 REST1411_0 (QUOTE (X Y Z)) (QUOTE NIL)))   STRATEGY/OPEN
10232367      ((R_55 NIL X_5 Y_6 Z_7) (^P_14 1 ^B_36))   STRATEGY/OPEN
10228556       ((P_14 NIL J_13) ($SPECIAL-REF 1 ^C_20 (QUOTE KEYWORD-GARBAGE)))   STRATEGY/OPEN
10229340        ((C_20 NIL V_19) ($CONDITIONAL 2 ^C_15 ^C_18 $EQ X_5 V_19))   STRATEGY/OPEN
10228615         ((C_15 NIL) ($SETQ-LEXICAL 1 ^B_17 X_5 (QUOTE NIL)))   STRATEGY/LABEL
10228835          ((B_17 IGNORE_16) (J_13 0 (QUOTE NIL)))   STRATEGY/OPEN
10228894         ((C_18 NIL) (J_13 0 (QUOTE NIL)))   STRATEGY/LABEL
10230653       ((B_36 IGNORE_35) (^P_25 1 ^B_49))   STRATEGY/LABEL
10229605        ((P_25 NIL J_24) ($SPECIAL-REF 1 ^C_31 (QUOTE KEYWORD-GARBAGE)))   STRATEGY/OPEN
10230389         ((C_31 NIL V_30) ($CONDITIONAL 2 ^C_26 ^C_29 $EQ Y_6 V_30))   STRATEGY/OPEN
10229664          ((C_26 NIL) ($SETQ-LEXICAL 1 ^B_28 Y_6 (QUOTE 3)))   STRATEGY/LABEL
10229884           ((B_28 IGNORE_27) (J_24 0 (QUOTE NIL)))   STRATEGY/OPEN
10229943          ((C_29 NIL) (J_24 0 (QUOTE NIL)))   STRATEGY/LABEL
10231760        ((B_49 IGNORE_48) (^P_38 1 ^B_53))   STRATEGY/LABEL
10230712         ((P_38 NIL J_37) ($SPECIAL-REF 1 ^C_44 (QUOTE KEYWORD-GARBAGE)))   STRATEGY/OPEN
10231496          ((C_44 NIL V_43) ($CONDITIONAL 2 ^C_39 ^C_42 $EQ Z_7 V_43))   STRATEGY/OPEN
10230771           ((C_39 NIL) ($SETQ-LEXICAL 1 ^B_41 Z_7 (QUOTE FOO)))   STRATEGY/LABEL
10230991            ((B_41 IGNORE_40) (J_37 0 (QUOTE NIL)))   STRATEGY/OPEN
10231050           ((C_42 NIL) (J_37 0 (QUOTE NIL)))   STRATEGY/LABEL
10232196         ((B_53 IGNORE_52) ($OPEN-FRAME 1 ^C_51 (QUOTE #{CALL-NODE (LISTN 1 K_1 A_2 B_3 C_4 X_5 Y_6 Z_7) 10231798})))   STRATEGY/LABEL
10232104          ((C_51)   (LISTN 1 K_1 A_2 B_3 C_4 X_5 Y_6 Z_7))   STRATEGY/OPEN
FOO_9
  (MOVE *ARG-1* (QUOTE 2))
  (JUMP CONS-REST (*RETURN-PC-1* OPC))
  (MOVE A9 *REST*)
  (KOPEN)
  (MOVE O0 A9)
  (MOVE O1 (QUOTE (X Y Z)))
  (MOVE O2 (QUOTE NIL))
  (KCALL GET-KEYWORD-ARG-VALUES (QUOTE 3) A8)
  (ALU L-R NOOP-NO-OVERFLOW-TRAP R0 (QUOTE 6))
  (ALU L+R NOOP-NO-OVERFLOW-TRAP R0 OPC BRANCH-GREATER-THAN-OR-EQUAL)
  (BRANCH MVBIND1412)
  (DISPATCH)
  (MOVE A8 (QUOTE NIL))
  (MOVE R1 (QUOTE NIL))
  (MOVE R2 (QUOTE NIL))
MVBIND1412
  (MOVE A7 R1)
  (MOVE A6 R2)
R_55
  (MOVE A5 (%VALUE-CELL KEYWORD-GARBAGE))
  (ALU L-R NOOP-NO-OVERFLOW-TRAP A8 A5 BW-32)
  (TEST BR-NOT-EQUAL)
  (BRANCH C_18)
C_15
  (MOVE A8 (QUOTE NIL))
B_36
  (MOVE A4 (%VALUE-CELL KEYWORD-GARBAGE))
  (ALU L-R NOOP-NO-OVERFLOW-TRAP A7 A4 BW-32)
  (TEST BR-NOT-EQUAL)
  (BRANCH C_29)
C_26
  (MOVE A7 (QUOTE 3))
B_49
  (MOVE A3 (%VALUE-CELL KEYWORD-GARBAGE))
  (ALU L-R NOOP-NO-OVERFLOW-TRAP A6 A3 BW-32)
  (TEST BR-NOT-EQUAL)
  (BRANCH C_42)
C_39
  (MOVE A6 (QUOTE FOO))
B_53
  (TAIL-OPEN)
  (MOVE O0 A0)
  (MOVE O1 A1)
  (MOVE O2 A2)
  (MOVE O3 A8)
  (MOVE O4 A7)
  (MOVE O5 A6)
  (TAIL-CALL LISTN (QUOTE 6))
C_42
  (JUMP B_53)
C_29
  (JUMP B_49)
C_18
  (JUMP B_36)

Post Processed:
FOO_9
   (MOVE *ARG-1* (QUOTE 2))
   (JUMP CONS-REST (*RETURN-PC-1* OPC))
   (MOVE A9 *REST*)
   (MOVE O0 A9 CH-OPEN)
   (MOVE O1 (QUOTE (X Y Z)))
   (KCALL GET-KEYWORD-ARG-VALUES (QUOTE 3) A8 (O2 (QUOTE NIL)))
   (ALU L-R NOOP-NO-OVERFLOW-TRAP R0 (QUOTE 6))
   (ALU L+R NOOP-NO-OVERFLOW-TRAP R0 OPC BRANCH-GREATER-THAN-OR-EQUAL)
   (BRANCH MVBIND1412)
   (DISPATCH)
   (MOVE A8 (QUOTE NIL))
   (MOVE R1 (QUOTE NIL))
   (MOVE R2 (QUOTE NIL))
MVBIND1412
   (MOVE A7 R1)
   (MOVE A6 R2)
R_55
   (MOVE A5 (%VALUE-CELL KEYWORD-GARBAGE))
   (ALU L-R NOOP-NO-OVERFLOW-TRAP A8 A5 BW-32)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_18)
C_15
   (MOVE A8 (QUOTE NIL))
B_36
   (MOVE A4 (%VALUE-CELL KEYWORD-GARBAGE))
   (ALU L-R NOOP-NO-OVERFLOW-TRAP A7 A4 BW-32)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_29)
C_26
   (MOVE A7 (QUOTE 3))
B_49
   (MOVE A3 (%VALUE-CELL KEYWORD-GARBAGE))
   (ALU L-R NOOP-NO-OVERFLOW-TRAP A6 A3 BW-32)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_42)
C_39
   (MOVE A6 (QUOTE FOO))
B_53
   (MOVE O0 A0 CH-TAIL-OPEN)
   (MOVE O1 A1)
   (MOVE O2 A2)
   (MOVE O3 A8)
   (MOVE O4 A7)
   (TAIL-CALL LISTN (QUOTE 6) (O5 A6))
C_42
   (JUMP B_53)
C_29
   (JUMP B_49)
C_18
   (JUMP B_36)






----------------------------------------------------------------

(a b &optinal (x (x-init) x-p) (y (y-init)) (z (z-init) z-p))


(nargs-entry 2)
((lambda (x x-p)
  (tagbody
      (go 4)
      (nargs-entry 3)
      (setq x-p t)
    4
      ((lambda (y)
         (nargs-entry 4)
         ((lambda (z z-p)
            (tagbody
                (go 6)
                (nargs-entry 5)
                (setq z-p t)
             6  <body>))
          (z-init) nil))
       (y-init))))
 (x-init) t)




(tagbody
 2  (nargs-entry 2)
    (optional-setup x (x-init) x-p 3 3x)
    (go 3x)
 3  (nargs-entry 3)
    (setq x-p t)
 3x
    (optional-setup y (y-init) nil 4 nil)
 4  (nargs-entry 4)
    (optional-setup z (z-init) z-p 5 5x)
    (go 5x)
 5  (nargs-entry 5)
    (setq z-p t)
 5x
    <body>)

(optional-setup 3)
(setq x (x-init))
(setq x-p nil)
(go 3x)


(defun alpha-lambda (name lambda-list body syntax shape fshape)
  (multiple-value-bind (args rest op-vars body)
      (hack-lambda-list lambda-list body)
    (let* ((vars (map! #'(lambda (name)
                           (if name (create-variable name)))
                       (list* rest 'k args)))
           (real-vars (cons (car vars) (cddr vars))))
      (bind-variables shape real-vars)
      (let ((exp (list syntax/lambda name (append vars op-vars)
                       (alpha-list body syntax shape fshape))))
        (unbind-variables shape op-vars)
        (unbind-variables shape real-vars)
;      (return-to-freelist real-vars)
        exp))))

(defun hack-lambda-list (lambda-list body)
  (declare (values args optionals rest body))
  (multiple-value-bind (args optionals rest keys allow-other-keys-p auxes)
      (parse-lambda-list lambda-list)
    (when auxes
      (setq body
            `((let ,auxes
                . ,body))))
    (when keys
      (setq body
            (hack-keys (or rest (setq rest (gensym 'rest)))
                       keys
                       allow-other-keys-p
                       body)))
    (let ((optional-vars (mapcar #'create-variable (mapcar #'car optionals))))
      (values args rest
              optional-vars
              (if optionals
                  (hack-optionals optional-vars optionals (length args) body)
                body)))))


(defun hack-optionals (vars optionals nargs body)
  `((tagbody
        ,@(do* ((ops optionals (cdr ops))
                (op (car ops) (car ops))
                (vars vars (cdr vars))
                supplied-p last-supplied-p
                (nargs nargs (1+ nargs))
                (forms '()))
               ((null ops)
                (nreverse `(
                            ,@(when supplied-p
                                `(,(+ 1000 nargs)
                                  (setq ,supplied-p t)))
                            (nargs-entry ,nargs)
                            ,nargs
                            . ,forms)))
            (setq last-supplied-p supplied-p)
            (setq supplied-p (third op))
            (setq forms
                  `(
                    ,@(when supplied-p `((go ,(+ 1001 nargs))))
                    (optional-setup ,(car vars) ,(second op) ,(create-variable supplied-p) ,(1+ nargs) ,(when supplied-p
                                                                                                          (+ 1001 nargs)))
                    ,@(when last-supplied-p
                        `(,(+ 1000 nargs)
                          (setq ,last-supplied-p t)))
                    (nargs-entry ,nargs)
                    ,nargs
                    . ,forms)))
        (progn ,@body))))


(define-special-form OPTIONAL-SETUP (var init-form init-p next-tag next-x-tag) (syntax shape fshape)
  (prog1 `(syntax/progn
            ((syntax/optional-setup ,(cdr (assoc next-tag *tags*)) ,(and next-x-tag (cdr (assoc next-x-tag *tags*))))
             (syntax/setq-lexical ,var
                                  ,(alpha init-form syntax shape fshape))
             . ,(when init-p `((syntax/setq-lexical ,init-p
                                                   ,(create-literal-node nil))))))
         (bind-variables shape (list var init-p))))  ;fix this


(define-compilator (optional-setup next-tag next-x-tag)
  (let* ((call (create-call-node 4 3))
         (top (cps-args call `(,primop/optional-setup ,empty ,next-tag ,next-x-tag))))
    (values top call (call-arg 1))))


(a b &optional (x (x-init) x-p)

(labels ((i.x () (optional-setup x (x-init) x-p 3x))  ;=> (setq x (x-init)) (setq x-p nil) (branch 3x)
         (i.y () (optional-setup y (y-init) nil 4))   ;=> (setq y (y-init))
         (i.z () (optional-setup z (z-init) z-p 5x)))
  (nargs-entries i.x i.y i.z (2 3 4 5) (nil 3x nil 5x) (x-p nil z-p))
  <body>)


(nargs-entries <setups> <start-nargs> <supplied-p>)

(do ((setups (call-exit-args node) (cdr setups))
     (tag (car (call-non-exit-args node)) (1+ tag))

  (generate-tag tag)
  (let ((setup (known-variable (variable-reference (car setups)))))
  (if (hairy? setup)
      (progn (generate-jump setup)
             (lambda-queue-at-end setup))
    (generate-call (lambda-body setup))




(a b &optional x y z)

2  (2x)
2x (setq x (x-init))
   (setq x-p nil)
   (3x)
3  (setq x-p t)
   (3x)
3x (setq y (y-init))
   (4x)
4  (4x)
4x (setq z (z-init))
   (setq z-p nil)
   (5x)
5  (setq z-p t)
   (5x)
5x <body>



(labels ((2  () (2x))
         (2x () (optional-init x (x-init) x-p 3x)) ;=> (setq x (x-init)) (setq x-p nil) (3x)
         (3  () (setq x-p t) (3x))
         (3x () (optional-init y (y-init) nil 4x)) ;=> (setq y (y-init)) (4x)
         (4  () (4x))
         (4x () (optional-init z (z-init) z-p 5x)) ;=> (setq z (z-init)) (setq z-p nil) (5x)
         (5  () (setq z-p t) (5x))
         (5x () <body>))
  (optionals-setup 2 2x 3 3x 4 4x 5 5x 2)
  (5x))

;;; **** the <n>x's get STRATEGY/PROC...


(defun getsym (n &optional (suffix ""))
  (make-symbol (format nil "~D~A" n suffix) nil))


(defun hack-optionals (vars optionals nargs body)
  (let ((label-procs (do ((ops optionals (cdr ops))
                          supplied-p last-supplied-p
                          (vars vars (cdr vars))
                          (nargs nargs (1+ nargs))
                          (entry (getsym nargs) next-entry)
                          (next-entry (getsym (1+ nargs)) (getsym (+ nargs 2)))
                          (setup (getsym nargs "setup") next-setup)
                          (next-setup (getsym (1+ nargs) "setup") (getsym (+ nargs 2) "setup"))
                          (labels '()))
                         ((null ops) (nreverse (list* `(,setup ()
                                                        ,@body)
                                                      `(,entry ()
                                                        ,@(when supplied-p
                                                            `((setq ,supplied-p t)))
                                                        (,setup))
                                                      labels)))
                       (let* ((op (car ops))
                              (init-form (second op)))
                         (setq last-supplied-p supplied-p)
                         (setq supplied-p (if (third op) (create-variable (third op))))
                         (setq labels (list* `(,setup ()
                                               (optional-init ,(car vars) ,init-form ,supplied-p)
                                               (,next-setup))
                                             `(,entry ()
                                               ,@(when last-supplied-p
                                                   `((setq ,last-supplied-p t)))
                                               (,setup))
                                             labels))))))
    `((labels ,label-procs
        (optional-setup ,nargs ,@(mapcar #'car label-procs))))))

(define-special-form OPTIONAL-INIT (var init-form init-p) (syntax shape fshape)
  (prog1 `(syntax/progn
            ((syntax/setq-lexical ,var
                                  ,(alpha init-form syntax shape fshape))
             . ,(when init-p `((syntax/setq-lexical ,init-p
                                                   ,(create-literal-node nil))))))
         (bind-variables shape (list var init-p))))

(define-special-form OPTIONAL-SETUP (nargs . labels) (syntax shape fshape)
  `(syntax/optional-setup ,(mapcar #'(lambda (label) (obtain-variable fshape label))
                                   labels)
                          ,(create-literal-node nargs)))

(define-compilator (optional-setup labels nargs)
  (let* ((exits (1+ (length labels)))
         (call (create-call-node (+ 2 exits) exits))
         (top (cps-args call `(,primop/optional-setup ,empty ,@labels ,nargs))))
    (values top call (call-arg 1))))

(defun ref-known (ref)
  (variable-known (reference-variable ref)))

(defun unknown-function-p (var)
  (and (null (variable-binder var))
       (some #'(lambda (ref)
                 (eq (node-role ref) call-proc))
             (variable-refs var))))

(defun generate-optional-setup (node)
  (let ((labels (cdr (call-exit-args node)))
        (nargs (leaf-value (car (call-non-exit-args node)))))
    (do ((nargs nargs (1+ nargs))
         (labels (cddr labels) (cddr labels))
         (entry (ref-known (first labels)) next-entry)
         (setup (ref-known (second labels)) next-setup)
         (next-entry (ref-known (third labels))
                     (ref-known (first labels)))
         (next-setup (ref-known (fourth labels))
                     (ref-known (second labels))))
        ((null labels) (cerror "foo" "now what"))
      (setf (lambda-strategy setup) STRATEGY/LABEL)     ;gack
      (push (cons nargs entry) *nargs-entries*)
      (generate-lambda entry)
      (if (some #'unknown-function-p
                 (lambda-live setup))
          (progn (emit-tag setup)
                 (setf (lambda-self-var setup)
                       (create-variable (variable-name (lambda-self-var setup))))
                 (cerror "foo" "before gen jump")
                 (generate-jump setup)
                 (lambda-queue-at-end setup))
        (let ((*lambda-queue* (list* next-entry next-setup *lambda-queue*)))    ;crock, fool generate-jump
          (generate-lambda setup))))))



----------------------------------------------------------------
;;; the hairy-init? test doesn't work because
;;; lambda-live of label procs is union of live of all
(defun generate-optional-setup (node)
  (let* ((args (cddr (call-args node)))
         (nargs (leaf-value (car args)))
         (supplied-p-vars (leaf-value (cadr args)))
         (inits (cddr args)))
    (do ((nargs nargs (1+ nargs))
         (vars supplied-p-vars (cdr vars))
         (inits inits (cdr inits)))
        (())
      (let* ((tag (create-variable nargs))
             (var (car vars))
             (init (cont (lambda-body (variable-known (leaf-value (car inits))))))
             (hairy-init? (some #'unknown-function-p
                                (lambda-live init))))
        (push (cons nargs tag) *nargs-entries*)
        (emit-tag tag)
        (if var
            (generate-move 't (write-acc var)))
        (cerror "foo" "gen init")
        (if (null (cdr inits))
            (return nil)
          (if hairy-init?
              (progn (generate-jump init)
                     (lambda-queue-at-end init))
            (generate-lambda init)))))))

----------------------------------------------------------------

(labels ((2x () (optional-init x (x-init) x-p 3x)) ;=> (optional-init) (setq x (x-init)) (setq x-p nil) (3x)
         (3x () (optional-init y (y-init) nil 4x)) ;=> (optional-init) (setq y (y-init)) (4x)
         (4x () (optional-init z (z-init) z-p 5x)) ;=> (optional-init) (setq z (z-init)) (setq z-p nil) (5x)
         (5x () <body>))
  (optionals-setup 2x 2 '(nil x-p nil z-p) '5x '2x '3x '4x))


(defun alpha-lambda (name lambda-list body syntax shape fshape)
  (multiple-value-bind (args rest op-vars body)
      (hack-lambda-list lambda-list body)
    (let* ((vars (map! #'(lambda (name)
                           (if name (if (variable-p name) name (create-variable name))))
                       (list* rest 'k args)))
           (real-vars (cons (car vars) (cddr vars))))
      (bind-variables shape real-vars)
      (let ((exp (list syntax/lambda name (append vars op-vars)
                       (alpha-list body syntax shape fshape))))
        (unbind-variables shape op-vars)
        (unbind-variables shape real-vars)
;      (return-to-freelist real-vars)
        exp))))


(defun hack-lambda-list (lambda-list body)
  (declare (values args optionals rest body))
  (multiple-value-bind (args optionals rest keys allow-other-keys-p auxes)
      (parse-lambda-list lambda-list)
    (when auxes
      (setq body
            `((let ,auxes
                . ,body))))
    (when keys
      (setq body
            (hack-keys (or rest (setq rest (gensym 'rest)))
                       keys
                       allow-other-keys-p
                       body)))
    (let ((optional-vars (mapcar #'create-variable (mapcar #'car optionals))))
      (values args rest
              optional-vars
              (if optionals
                  (hack-optionals optional-vars optionals (length args) body)
                body)))))

(defun hack-optionals (vars optionals nargs body)
  (let* ((supplied-p-vars '())
         body-label
         (first-label (getsym nargs "x"))
         (label-procs (do ((ops optionals (cdr ops))
                          supplied-p last-supplied-p
                          (vars vars (cdr vars))
                          (nargs nargs (1+ nargs))
                          (setup first-label next-setup)
                          (next-setup (getsym (1+ nargs) "x")
                                      (getsym (+ nargs 2) "x"))
                          (labels '()))
                         ((null ops) (push supplied-p supplied-p-vars)
                          (setq body-label setup)
                          (nreverse (cons `(,setup ()
                                            ,@body)
                                          labels)))
                       (let* ((op (car ops))
                              (init-form (second op)))
                         (setq last-supplied-p supplied-p)
                         (setq supplied-p (if (third op) (create-variable (third op))))
                         (push last-supplied-p supplied-p-vars)
                         (setq labels (list* `(,setup ()
                                               (optional-init ,(car vars) ,init-form ,supplied-p)
                                               (,next-setup))
                                             labels))))))
    `(((lambda ,(remove-if-not #'identity supplied-p-vars)
         (labels ,label-procs
           (optional-setup ,first-label ,nargs ,(setq supplied-p-vars (nreverse supplied-p-vars))
                           ,body-label ,@(butlast (mapcar #'car label-procs)))
;       (,body-label)
           ))
       ,@(make-list (count nil supplied-p-vars :test-not #'eq)
           :initial-value `',undefined)))))

(define-special-form OPTIONAL-INIT (var init-form init-p) (syntax shape fshape)
  (prog1 `(syntax/progn
            ((syntax/optional-init)
             (syntax/setq-lexical ,var
                                  ,(alpha init-form syntax shape fshape))
             . ,(when init-p `((syntax/setq-lexical ,init-p
                                                   ,(create-literal-node nil))))))
         (bind-variables shape (list var init-p))))

(define-compilator (optional-init)
  (make-call `(,primop/noop)))

;;; this does nothing
;;; optional inits use it
(define-constant-primop noop
  (primop.generate (self node)
    self node nil)
  (primop.side-effects? t))

(define-special-form OPTIONAL-SETUP (first-label nargs supplied-p-vars . init-labels) (syntax shape fshape)
  `(syntax/optional-setup ,(obtain-variable fshape first-label)
                          ,(create-literal-node nargs)
                          ,(create-literal-node supplied-p-vars)
                          ,(mapcar #'(lambda (label) (create-literal-node (obtain-variable fshape label)))
                                   init-labels)))

(define-compilator (optional-setup first-label nargs supplied-p-vars init-labels)
  (let* ((call (create-call-node (+ 5 (length init-labels)) 2))
         (top (cps-args call `(,primop/optional-setup ,empty ,first-label
                               ,nargs ,supplied-p-vars
                               ,@init-labels))))
    (values top call (call-arg 1))))

;  (make-call `(,primop/optional-setup ,nargs ,supplied-p-vars . ,init-labels)))




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
            (generate-move 't (car vars))))
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

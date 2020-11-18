;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-

;;;; CATCH, THROW and UNWIND-PROTECT

(in-package 'lisp-internals)

(export '(CATCH THROW UNWIND-PROTECT) 'nlisp)

;;; Catch
;;;
;;; (catch ^C <tag> <body>)
;;;
;;; (let* ((.tag. <tag>)
;;;        (*sg-established-tags* (cons .tag. *sg-established-tags*)))
;;;   (catch-open
;;;     'si:unwind-marker
;;;     .tag.
;;;     *special-pdl*
;;;     *stack-pointer*
;;;     <pc>
;;;     <body>))
;;;
(defmacro catch (tag &body body)
  ` ;(let* ((.tag. <tag>)
    ;       (*sg-established-tags* (cons .tag. *sg-established-tags*)))
     (catch-continue
       ;; this is so arg order isn't rearranged
       (setf (hw:O0) 'li:unwind-marker)
       (setf (hw:O1) ,tag)
       (setf (hw:O2) gr:*special-pdl*)
       (setf (hw:O3) gr:*stack-pointer*)
       (%catch-cont)
       (nc:%values (progn ,@body))
       (%catch-label)))

(defun foo (x p)
  (bar (if p x (fff))))

((  ) ($catch-open 1 ^c_1 'c2))
 ((c_1) (body ^c_2))
  ((c_2 v_3) ($catch-continue ^c_4 v_3))

target v_3 to O5

(open)
(move o0
(move o1
(move o2
(move o3
(move o4
<body> => o5
(call catch-continue)

(defun foo (x p)
  (catch 'tag
    (if p x (fff))))

(open)
...
(move nop p)
(test)
(branch l1)
l2
(move o5 a0) ; needs (open-call identity o5 a0)
l3
(call catch-continue ?)

l1
(open-call fff o5)
(uncond-branch l3)

((  ) ($catch-open 1 ^c_1 'c2))
 ((c_1) (body ^c_2))
  ((c_2 v_3) ($catch-continue ^c_4 v_3))

****************

target v_3 to (MV . O5)

(generate-move <from> '(MV . <reg>))
  =>
   (open-call identity <reg> <from>)

(get-call-dest '(MV . <reg>))
   => <reg>

$catch-open needs to duplicate a lot of open-frame?
  (check for tail, hack new open?)

((  ) ($open-frame 1 ^c1 '{call $catch-continue})
 ((c1 nil) ($catch-open 1 ^c2))
  ((c2 nil) ...
       ... body ...
         ((c100 v101) ($catch-continue 1 ^c102 v101))

arg to $catch-continue targeted to (MV . O5)

need primop.needs-open? for $catch-continue
need primop.wants       to target var


---
problem with this is that not all assignments to v
or to other things in v's preference class want
to hack mv bit
---

****************


we must flush the open, either:
   a- .. put value anywhere ..
      (nop ch-call)
      (move <dest> <value> ch-return)

   b- ... put value in O5
      (call pop-catch)

the major problem is the mv bit which needs to get cleared
when catch returns something


(defun foo (n x y)
  (catch 'tag
    (case n
      (0 (bar))
      (1 x)
      (2 y)
      (3 (values x y)))))


=0
 (open-call bar O5 ())
 (uncond-branch catch-cont)
=1
 (open-call (return-sv 1) O5 (O0 x))
 (uncond-branch catch-cont)
=2
 (open-call (return-sv 1) O5 (O0 y))
 (uncond-branch catch-cont)
=3
 (move *return-0* y)
 (movei *num-ret* '2)
 (open-call (return-mv 1) O5 (O0 x))
 (uncond-branch catch-cont)

catch-cont
 (call pop-catch a7)
co


 ((p j) ($open ...))
   .....
     (( ) (j 0 x y))
     (( ) (j 0 x))

  ((c v) ($catch-cont ... v))

now how does (j 0 x y) know to generate
mv stuff rather than just (move v x)?
crock special casing on (MV . <reg>) in parallel-assign?

it doesn't even have to if cont to $catch-cont doesn't need it
v could be 'CATCH-VALUE which knows to look in (cont (variable-binder v))



----------------------------------------------------------------
If we are doing a catch, it is quite possible it will get thrown to
therefore, result forms of the catch should be put in the same place
that throw will put values
(ie O5, *result-n*)


;; this probably won't be real winning:
(defun foo (x y)
  (multiple-value-bind (a b)
      (catch 'foo (if (foo-p) (values x y) (bar)))

unless we can separately flush catch and jump to values-set-up

  (open)
  (move o ...
  (move o ...
  ...
 (move a x)
 (move b y)
 (call flush-catch ignore)
 (branch values-set-up)
else
 (open-call bar o5 ())
catch-continue
 (call catch-continue a)
 (alu-field extract-bit-right ...
 (movei r0 ..
 (branch one-value
 (alu l+r ...
 (branch all-or-more ...
 (dispatch)
 (movei a 'nil)
 (move *return-0* 'nil)
mvbind
 (move b *return-0*)
values-set-up


could work something like this?

(defun foo (x y p)
  (multiple-value-bind (a b)
      (if p (values x y) (bar))
    (fff a b)))


generate-return when the cont was
 ((c2 v1) ($catch-cont ^c3 v1))
could use c3
 parassign to vars of c3
 unwind to c3 (flush catch)
 branch to c3


what if generate-return isn't called??

;; this looks like let
(defun foo (x y)
  (multiple-value-bind (a b)
      (ketch (values (bar x) y))
    (fff a b)))

something would have to be done in generate-let???

----------------------------------------------------------------
mv-prog1

(mv-prog1
  (bar)
  (foo))

   (bar ^c))
 ((c v)




  ($mv-save (bar))
  (progn ...)
  ($mv-restore)





catch must return multiple-values if the body does

clearing the bit beforehand doesn't work       (catch 'foo (progn (mv-hair) (foo)))

we must leave the bit as functions return it   (catch 'foo (bar))

if we may need mv's, some things which normally return in simpler ways (non-calls)
must be more complicated (set or clear mv bit, move to return regs)
somehow something has to know that mv's may be needed
what?
  the continuation is a handy thing to look at.

((c v) ($catch-cont v))

  things that pass a value to $catch-cont will call continuation-expecting
which can arrange to return something which says mv

[this doesn't quite work for things like this:

       ($catch-cont x)

       ($catch-cont '259)

but those things:
   1.  probably can't throw anyway and maybe
       the catch should go away

   2.  can probably be handled by $catch-cont
       (let ((value (call-arg-n 5 node)))
         (unless (and (reference-node value)
                      (eq (variable-binder (reference-variable value))
                          (node-parent node)))
           (generate-move value '(MV . O5))))


]

what about pref classes, alloc of v?  should be O5?


;;; Return the register in which CONT
;;; is expecting its value
(defun continuation-expecting (cont)
  (cond ((lambda-node? cont)
         (or
           (mv-maybe-needed cont)
           (let ((cvar (cont-var cont)))
             (if cvar
                 (acc cvar)
               IGNORED))))                                      ;continuation has no vars?
        (t (let ((proc (variable-known (reference-variable cont))))
             (if proc    ;bound continuation
                 (continuation-expecting proc)
               RETURN)))))



(defun mv-maybe-needed (lambda)
  (let ((call (lambda-body lambda)))
    (let ((proc (call-proc call)))
      (if (and (primop-ref? proc primop/%catch-label)
               ;;  *** but not if cvar is not value arg like in c20 below
               (and (= (acc (car (lambda-variables lambda)))
                       (+ O0 5)))
               ;; don't bother if don't need mv's
               (let ((catch-continuation (cont (lambda-body (cont call)))))
                 (not (and (lambda-node? catch-continuation)
                           (null (cdr (lambda-variables catch-continuation)))))))
             '(MV . O5)))))


(defun bar ()
  (catch 'tag (fff)))

(defun catch-no-mv ()
  (print (catch 'tag (fff))))

;;; why doesn't this work?
;;; continuation-expecting never gets called on the right lambda
;;; because generate-known-return does parallel-assign, not continuation-expecting
(defun foo (x)
  (catch 'tag (if (pred) x 259)))


(defun foo (x)
  (catch 'tag (1+ x)))

(defun foo (x)
  (catch 'tag (bar)))

(defun foo (x)
  (catch 'tag (values 3 4 5)))

(defun foo ()
  (catch 'tag (values)))

(defun foo (x)
  (catch 'tag x))

(defun foo ()
  (catch 'tag 259))

Generating:
51721047  ((FOO_10 NIL K_0) ($OPEN-FRAME 1 ^C_33 '#{CALL-NODE CATCH-CONTINUE_1 51721103}))   STRATEGY/HEAP
51725164   ((C_33)   ($WRITE-FUNCTIONAL-DEST 1 ^C_32 '16 'UNWIND-MARKER))   STRATEGY/OPEN
51725053    ((C_32 NIL V_31) ($WRITE-FUNCTIONAL-DEST 1 ^C_30 '17 'TAG))   STRATEGY/OPEN
51724734     ((C_30 NIL V_29) ($WRITE-FUNCTIONAL-DEST 1 ^C_28 '18 *SPECIAL-PDL*_59))   STRATEGY/OPEN
51724615      ((C_28 NIL V_27) ($WRITE-FUNCTIONAL-DEST 1 ^C_26 '19 *STACK-POINTER*_58))   STRATEGY/OPEN
51724476       ((C_26 NIL V_25) ($%CATCH-CONT 1 ^C_24))   STRATEGY/OPEN
51724357        ((C_24 NIL V_23) ($%CATCH-LABEL 1 ^C_20))   STRATEGY/OPEN
51724121         ((C_20 NIL V_19) (CATCH-CONTINUE_1 1 K_0 V_31 V_29 V_27 V_25 V_23 '259 V_19))   STRATEGY/OPEN


(1 ENTER NC::CONTINUATION-EXPECTING: #{LAMBDA-NODE C_20 46366615})
(1 EXIT NC::CONTINUATION-EXPECTING: 22)
(1 ENTER NC::CONTINUATION-EXPECTING: #{LAMBDA-NODE C_20 46366615})
(1 EXIT NC::CONTINUATION-EXPECTING: 22)

Post Processed:
FOO_10
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_20)
C_20
   (MOVEI O5 (QUOTE 259) BOXED)
   (TAIL-CALL (CATCH-CONTINUE 7) NIL)






   (open)
  ....
   ((c mv v) ($catch-continue ^c2 v mv))

(var-loc mv) = 'MULTIPLE-VALUES ?????

(parallel-assign '(x) '(v) mv)  => (move v x) (clear bit)
(parallel-assign '(x y z) '(v) mv) => (move v x) (move *return-0* y) (move *return-1* z) (set bit)

but...

       (bar ^c))
  ((c mv v) ($catch-continue

(parallel-assign '(v) '(v) mv)  => *not* clear-bit but leave it alone

(but this uses continuation-expecting does'nt it???)

(parallel-assign args (lambda-variables lambda) (get-rest-place lambda))

(defun get-rest-place (lambda)
  (let ((rest-var (lambda-rest-variable lambda)))
    (if (and rest-var
             (eq (acc rest-var) 'MV)
             (body is catch and cont is not non-mv lambda...))
          'MV
      rest-var)))


(%values (bar))

         (bar ^c1)
 ((c1 v3) ($%values ^c2 v3))
  ((c2 v4) (foo k0 v4))

==>

        (bar ^c1)
  ((c1 mv v4) (foo k0 v4))

;;; This works if body returns multiple values
;;; but doesn't work if body returns a single value
;;; without a function return
(defafun catch-continue (marker tag spdl sptr pc body-value ignore)
  (return-tail a5))

(defun foo ()
  (print (catch 'tag
           (bar 3 4))))


(defun generate-catch-open (node)
  (emit-code
    '((K:OPEN)
      (K:MOVEI O0 'LI:UNWIND-MARKER)))
  (generate-move (call-arg-n 2 node) O1)
  (emit-code
      '((K:MOVE O2 GR:*SPECIAL-PDL*)
        (K:MOVE O3 GR:*STACK-POINTER*)
        (K:MOVE-PC O4 ??)
        ...)))

(defun generate-catch-continue (node)
  (nc::emit-call 'li:catch-continue 6) dest)


(open)
(movei o0 'li:unwind-marker)
(move o1 <tag>)
(move o2 *special-pdl*)
(move o3 *stack-pointer*)
(move-pc o4 catch-cont)
... body ...
catch-cont
(call (catch-continue 6) <dest>)


;;; (unwind-protect
;;;     <form>
;;;   <cleanup>)
;;;
;;; (unwind-protect-continue
;;;      'si:unwind-marker
;;;      'si:unwind-protect-tag
;;;      *special-pdl*
;;;      *stack-pointer*
;;;      #'(lambda ()
;;;          <cleanup>)
;;;      <form>)
;;;
(defmacro unwind-protect (protected-form &body cleanup-forms)
  `(unwind-protect-continue
     'si:unwind-marker
     'si:unwind-protect-tag
     *special-pdl*
     *stack-pointer*
     #'(lambda ()
         ,@cleanup-forms)
     ,protected-form))


(defun unwind-protect-continue (marker tag spdl sptr cleanup-closure form-value)
  (funcall cleanup-closure)
  form-value)


;;; Throw
;;;
;;; A catch frame contains:
;;; ----------------------
;;; O0: li:unwind-marker
;;; O1: <tag> li:unwind-protect-tag for unwind protect
;;; O2: *special-pdl*
;;; O3: *stack-pointer*
;;; O4: <pc> if throw <cleanup closure> if unwind protect
;;;
;;; Temporarily used:
;;; O5: <value> of catch or unwind body
;;;
(defun throw (tag value)
;  (if (memq tag *sg-established-tags*)
    (progn
      (setq gr:*throw-tag* tag)
      (setq gr:*throw-value* value)
      (do-forever
        ;; or until we hopefully hit an
        ;; unwind protect at top of stack
        ;; ** scroll stack when hit bottom
        (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
               (hw:ldb hw:%%ch-oar-open (hw:read-open-active-return) 0))
            ;; was a call, pop it
            (hw:ch-return)
          ;; was open or topen, check it
          (when (eq (hw:O0) 'LI:UNWIND-MARKER)
            (unbind-to (hw:O2))
            (setq gr:*stack-pointer* (hw:O3))
            (if (eq (hw:O1) gr:*throw-tag*)
                (progn
                  ;; this calls flush-catch rather than ch-call ch-return
                  ;; so that the mv-return bit gets cleared
                  ;; the value comes back in R0
                  (setf (hw:R0) (hw:call 'flush-catch 0))
                  (hw:dispatch (hw:R4)))
              (when (eq (hw:O1) 'LI:UNWIND-PROTECT-TAG)
                (setf (hw:O0) NIL)               ;don't lose if cleanup throws
                (setf (hw:O1) gr:*throw-tag*)    ;save tag and value
                (setf (hw:O5) gr:*throw-value*)
                (funcall (hw:O4))                ;execute cleanup forms
                (setq gr:*throw-value* (hw:O5))  ;and restore tag and value
                (setq gr:*throw-tag*   (hw:O1)))
            (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
                   (get-CS-A-O))
                (hw:ch-call)
              (hw:ch-tcall))
            (hw:ch-return)))))
;   (error "There was no pending CATCH for the tag ~s" tag)
      ))

(defun flush-catch ()
  gr:*throw-value*)


(defun throw-mv (tag value1)
;  (if (memq tag *sg-established-tags*)
    (progn
      (setq gr:*throw-tag* tag)
      (setq gr:*throw-value* value1)
      (do-forever
        ;; or until we hopefully hit an
        ;; unwind protect at top of stack
        ;; ** scroll stack when hit bottom
        (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
               (hw:ldb hw:%%ch-oar-open (hw:read-open-active-return) 0))
            ;; was a call, pop it
            (hw:ch-return)
          ;; was open or topen, check it
          (when (eq (hw:O0) 'LI:UNWIND-MARKER)
            (unbind-to (hw:O2))
            (setq gr:*stack-pointer* (hw:O3))
            (if (eq (hw:O1) gr:*throw-tag*)
                (progn
                  ;; this calls flush-catch-mv so that the mv-return bit gets set
                  ;; the value comes back in R0
                  (setf (hw:R0) (hw:call 'flush-catch-mv 0))
                  (hw:dispatch (hw:R4)))
              (when (eq (hw:O1) 'LI:UNWIND-PROTECT-TAG)
                (setf (hw:O0) nil)               ;don't lose if cleanup throws
                (setf (hw:O1) gr:*throw-tag*)    ;save tag and value
                (setf (hw:O5) gr:*throw-value*)
                ;; *** save rest of values
                (funcall (hw:O4))                ;execute cleanup forms
                ;; *** restore rest of values
                (setq gr:*throw-value* (hw:O5))  ;and restore tag and value
                (setq gr:*throw-tag*   (hw:O1)))
            (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
                   (hw:ldb hw:%%ch-oar-active (get-CS-OA) 0))
                (hw:ch-call)
              (hw:ch-tcall))
            (hw:ch-return)))))
;   (error "There was no pending CATCH for the tag ~s" tag)
      ))


(defafun flush-catch-mv ()
  (return-mv gr:*throw-value*))

(defun get-CS-OA ()
  "Return the open and active frame numbers
from the top of the call stack."
  (setf (hw:A0) (hw:read-open-active-return))
  (setf (hw:A2) (hw:read-call-sp-hp))
  (hw:ch-return)
  (setf (hw:R1) (hw:read-open-active-return))   ;save active on stack
  (hw:write-open-active-return (hw:R0))         ;put back saved oar
  (hw:write-call-sp-hp (hw:A2))
  (hw:A1))


#||||

(defun foo ()
  (tagbody
      x
         (print 'start)
         (bar #'(lambda () (go x)))
         (print 'done)
      y
         (print 'start)
         (bar #'(lambda () (go y)))
         (print 'done)))


Bel Canto
547-6120


(defun throwup ()
  (progn
    (catch 'foo
      (catch 'bar
        (catch 'foo
          (unwind-protect
              (throw 'bar)
            (throw 'foo)))
        (print 'foo2))
      (print 'bar))
    (print 'foo1)))


||||#



Catch must return multiple values if multiple values are thrown,
or if the body returns multiple values.

Throw of a single value can clear the mv flag, throw of multiple
values can set it.  This is not hard.

CATCH-CONTINUE, the thing which pops the frame, must use RETURN-TAIL
and not mess up the flag.

If no throw happens, the body falls into the call to CATCH-CONTINUE
but it must set the flag appropriately


This will happen if the value of the body is the value of a function
   (catch 'tag (f))

but if the value is a variable,literal,or primop value
   (catch 'tag (case (pred)
                 (0 x)
                 (1 259)
                 (2 (+ x y))
                 (3 (values x y 259))))
the flag will not be set.


  one solution would be to make a closure out of the body
  (STRATEGY/PROC)
    (catch 'tag (funcall #'(lambda () 3)))


The problem is the communication of the information that the flag must be
set.

 The value could be assigned by:
   generation of individual primops (get-destination)
   generate-continuation
   parallel-assign
     gen-label-call
     gen-known-return
     let
     gen-general-call (move to open)






****

mv's returned from catch body
looks very much like mv's returned
to dest RETURN

why can't code be similiar???

*****














;----------------------------------------------------------------

;(%VALUES)


;;; %VALUES gets wrapped around code which needs
;;; to set the MV flag, even though it is not doing
;;; a function return.
;;; This takes a body of code, node converts it, and
;;; then wraps a call to %VALUES1 around every place
;;; where the body can return a value.
;;; %VALUES1 will arrange that the MV flag gets set
;;; correctly.
(define-compilator (%values body)
  (multiple-value-bind (node c-parent c-role) (->node body)
    (cond ;;
          ;; '259 => ($%VALUES1 1 <cont> '259)
          ;;
          ((or (literal-node? node)
               (reference-node? node))
           (make-call `(,primop/%values1 ,node)))
          ;;
          ;; (<cont> 0 '3 V_1 V_2) => ($%VALUES1 1 <cont> '3 V_1 V_2)
          ;;
          ((eq call-proc c-role)
           (let ((args (copy-list (call-args c-parent))))
             (mapc #'detach args)
             (let ((new-call (make-call `(,primop/%values1 . ,args))))
               (values new-call new-call (call-arg 1)))))
          ;;
          ;; (( ... ) (^P_1 0 ^C_2))
          ;;  ((P_2 NIL J_3) ...
          ;;     ...  (J_3 0 X_6)     =>  ($%VALUES1 1 J_3 X_6)
          ;;     ...  (BAR 1 J_3 V_7) =>  ...        (BAR 1 ^C_22 V_7)
          ;;                             ((C_22 V_23) ($%VALUES1 1 J_3 V_23))
          ;;
          ((lambda-node? (call-proc c-parent))
           (dolist (ref (variable-refs (car (lambda-variables (call-proc c-parent)))))
             (let ((ref-role (node-role ref))
                   (ref-parent (node-parent ref)))
               (detach ref)
               (cond ((eq ref-role call-proc)
                      (let ((args (copy-list (call-args ref-parent))))
                        (mapc #'detach args)
                        (replace-node
                          ref-parent
                          (make-call-with-exits 1 `(,primop/%values1 ,ref . ,args)))))
                     ((and (eq ref-role (call-arg 1))
                           (= 1 (call-exits ref-parent)))
                      (relate (call-arg 1)
                              (insert-%values-call-as-cont ref-parent)
                              ref))
                     (t (bug "funny reference to bound cont in %values")))))
             (values node c-parent c-role))
          ;;
          ;; (BAR 1 <cont> X_1) => ...         (BAR 1 ^C_22 X_1)
          ;;                       ((C_22 V_23) ($%VALUES1 1 <cont> V_23))
          ;;
          ((and (eq c-role (call-arg 1))
                (= 1 (call-exits c-parent)))
           (values node (insert-%values-call-as-cont c-parent) (call-arg 1)))
          (t (bug "something funny in %values")))))


(defun insert-%values-call-as-cont (parent)
  (let* ((var (create-variable 'v))
         (lambda (create-lambda-node 'c (list nil var)))
         (call (make-call `(,primop/%values1 ,var))))
    (relate lambda-body lambda call)
    (relate (call-arg 1) parent lambda)
    call))


(define-special-form %VALUES (body) (env)
  (list '%VALUES (alpha body env)))

(define-primop %values1 (&rest values)
  (:generate (node)
     (generate-values node)))


;;; This is the generator for %VALUES1 the primitive
;;; for %VALUES.
;;; The idea is to return multiple values, even though
;;; there may not be a function return.
(defun generate-values (node)
  (let* ((args (cdr (call-args node)))
         (cont (cont node))
         (dest (get-destination cont))
         (nargs (length args)))
    (cond
      ((and (lambda-node? (node-parent node))
            (let ((proc (call-proc (node-parent (node-parent node)))))
              (and (reference-node? proc)
                   (not (variable-known (reference-variable proc))))))
         (generate-move (car args) dest)
         dest)
      ((= 1 nargs)
       (generate-internal-call 'LI:SINGLE-VALUE dest (car args))
       dest)
      ((= 0 nargs)
       (emit 'K:OPEN)
       (generate-move ''0 'GR:*NUMBER-OF-RETURN-VALUES*)
       (generate-move ''NIL O0)
       (generate-general-call 'LI:MULTIPLE-VALUES nil dest 1)
       dest)
      (t (emit 'K:OPEN)
         (parallel-assign `(',(+ nargs *mv-return-nargs-offset*)
                            ,@(cdr args)
                            ,(car args))
                          `(GR:*NUMBER-OF-RETURN-VALUES*
                             ,@(subseq *mv-return-registers* 0 (1- nargs))
                             ,O0))
         (generate-general-call 'LI:MULTIPLE-VALUES nil dest 1)
         dest))))


;----------------------------------------------------------------
;;; 4/29/87

maybe throw should flush catch?

if throw flushes catch, it must return value in some canonical place (R0)
   also inline catch-continue can do mv setting in one fcn call

       (open-call (single-value 1) o5 (o0 a3))
    continue
       (call (catch-continue 6) <dest> nil)

   becomes

       (open-call (catch-cont-sv 1) r0 (o5 a3))
    continue
       (move <dest> r0)

   new opens for result of catch will be harder

if code flushes catch, call can move to anywhere






(defun foo (x)
  (catch 'tag (if (pred) x 259)))



Post Processed:
FOO_12
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_29)
P_22
   (OPEN-CALL (PRED 0) R2 NIL)
   (MOVE NOP R2)
   (TEST BR-ZERO)
   (BRANCH C_24 NIL)
C_23
   (OPEN-CALL (SINGLE-VALUE 1) O5 (O0 A0))    ;(tail-call (catch-continue-sv 6) (o5 a0))
C_31
C_29
   (TAIL-CALL (CATCH-CONTINUE 7) NIL)         ;(return-tail r0)
C_24
   (MOVEI O0 (QUOTE 259) BOXED CH-OPEN)       ;(movei o5 '259 boxed)
   (CALL (SINGLE-VALUE 1) O5 NIL)             ;(tail-call (catch-continue-sv 6) nil)
   (UNCONDITIONAL-BRANCH C_31 NIL)



(defun foo (x)
  (catch 'tag (1+ x)))


Post Processed:
FOO_11
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_23)
   (ALU R+1 R2 A0 A0 BW-24 BOXED DT-BOTH-FIXNUM-WITH-OVERFLOW)
   (OPEN-CALL (SINGLE-VALUE 1) O5 (O0 R2))     ;(tail-call (catch-continue-sv 6) (o5 r2))
C_23
   (TAIL-CALL (CATCH-CONTINUE 7) NIL)          ;(return-tail r0)

(defun foo (x)
  (catch 'tag (bar)))


Post Processed:
FOO_12
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_25)
   (OPEN-CALL (BAR 0) R2 NIL)                  ;(open-call (bar 0) o5 nil)
   (MOVE O5 R2)                                ;(tail-call (catch-continue-tail 6) nil)
C_25
   (TAIL-CALL (CATCH-CONTINUE 7) NIL)          ;(return-tail r0)

(defun foo (x)
  (catch 'tag (values 3 4 5)))



Post Processed:
FOO_11
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_21)
   (MOVEI *NUMBER-OF-RETURN-VALUES* (QUOTE 6) BOXED CH-OPEN)
   (MOVEI *RETURN-0* (QUOTE 4) BOXED)
   (MOVEI *RETURN-1* (QUOTE 5) BOXED)
   (MOVEI O0 (QUOTE 3) BOXED)            ;(movei o5 '3 boxed)
   (CALL (MULTIPLE-VALUES 1) O5 NIL)     ;(tail-call (catch-continue-mv 6) nil)
C_21
   (TAIL-CALL (CATCH-CONTINUE 7) NIL)    ;(return-tail r0)



(defun foo ()
  (print (catch 'tag (bar))))


Post Processed:
FOO_12
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_25)
   (OPEN-CALL (BAR 0) R2 NIL)                       ;   (open-call (bar 0) o5 nil)
   (MOVE O5 R2)                                     ;   (call (catch-continue-tail 6) r0 nil)
C_25                                                ;c_25
   (CALL (CATCH-CONTINUE 7) (NEW-TAIL-OPEN 0) NIL)  ;   (tail-open-call (print 1) (o0 r0))
   (TAIL-CALL (PRINT 1) NIL)


(defun foo ()
  (+ (catch 'tag (bar))
     (foo)))


Post Processed:
FOO_12
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC O4 C_25)
   (OPEN-CALL (BAR 0) R2 NIL)                   ;(open-call (bar 0) o5 nil)
   (MOVE O5 R2)                                 ;(call (catch-continue-tail 6) r0 nil)
C_25
   (CALL (CATCH-CONTINUE 7) A0 NIL)             ;(move a0 r0)                 can be combined with next
   (OPEN-CALL (FOO 0) R2 NIL)                   ;(open-call (foo 0) r2 nil)
   (NOP)                                        ;(nop)
   (ALU L+R RETURN A0 R2 ...)                   ;(alu l+r return a0 r2 ...)


(defun foo ()
  (catch 'tag (values)))

(defun foo (x)
  (catch 'tag x))

(defun foo ()
  (catch 'tag 259))




;----------------------------------------------------------------

a problem here because the body does not ever fall into the catch-continuation

(defun catch-exited ()
  (tagbody
      (li:catch 'tag (bar) (go xxx))
      (print 'caught)
    xxx
      (print 'done)))

Unsimplified node:
  ((CATCH-EXITED_15 NIL K_0) (^TAGBODY_16 1 K_0))
   ((TAGBODY_16 NIL K_1) ($Y 1 ^Y_19))
    ((Y_19 NIL C_17 XXX_2) (C_17 0 ^C_18 ^T_21))
     ((C_18 NIL NIL)   ($OPEN-FRAME 1 ^C_51 '#{CALL-NODE CATCH-CONTINUE_5 51700042}))
      ((C_51)           ($OPEN-FRAME 1 ^C_25 '#{CALL-NODE ^P_24 51700124}))
       ((C_25)           (^P_24 1 ^C_50 'UNWIND-MARKER))
        ((P_24 NIL K_6 EXP0314_7) ($WRITE-FUNCTIONAL-DEST 1 K_6 '16 EXP0314_7))
        ((C_50 NIL V_49)  ($OPEN-FRAME 1 ^C_27 '#{CALL-NODE ^P_26 51700634}))
         ((C_27)           (^P_26 1 ^C_48 'TAG))
          ((P_26 NIL K_8 EXP0315_9) ($WRITE-FUNCTIONAL-DEST 1 K_8 '17 EXP0315_9))
          ((C_48 NIL V_47)  ($OPEN-FRAME 1 ^C_29 '#{CALL-NODE ^P_28 51701337}))
           ((C_29)           (^P_28 1 ^C_46 *SPECIAL-PDL*_55))
            ((P_28 NIL K_10 EXP0316_11) ($WRITE-FUNCTIONAL-DEST 1 K_10 '18 EXP0316_11))
            ((C_46 NIL V_45)  ($OPEN-FRAME 1 ^C_31 '#{CALL-NODE ^P_30 51702045}))
             ((C_31)           (^P_30 1 ^C_44 *STACK-POINTER*_54))
              ((P_30 NIL K_12 EXP0317_13) ($WRITE-FUNCTIONAL-DEST 1 K_12 '19 EXP0317_13))
              ((C_44 NIL V_43)  ($%CATCH-CONT 1 ^C_42))
               ((C_42 NIL V_41)  ($OPEN-FRAME 1 ^C_32 '#{CALL-NODE BAR_14 51702645}))
                ((C_32)           (BAR_14 1 ^B_36))
                 ((B_36 IGNORE_35) (^GO_34 1 ^C_40))
                  ((GO_34 IGNORE_33) ($%GO 2 K_1 XXX_2))
                  ((C_40 NIL V_39)  ($%CATCH-LABEL 1 ^C_38))
                   ((C_38 NIL V_37)  (CATCH-CONTINUE_5 1 ^B_54 V_49 V_47 V_45 V_43 V_41 V_39 V_37))
                    ((B_54 IGNORE_53) ($OPEN-FRAME 1 ^C_52 '#{CALL-NODE PRINT_4 51704746}))
                     ((C_52)           (PRINT_4 1 ^B_58 'CAUGHT))
                      ((B_58 IGNORE_57) (^GO_56 1 K_1))
                       ((GO_56 IGNORE_55) ($%GO 2 K_1 XXX_2))
     ((T_21 NIL C_20)  (C_20 0 ^XXX_22))
      ((XXX_22 NIL K_3) ($OPEN-FRAME 1 ^C_23 '#{CALL-NODE PRINT_4 51677416}))
       ((C_23)           (PRINT_4 1 K_3 'DONE))
Generating:
  ((CATCH-EXITED_15 NIL K_0) ($Y 1 ^Y_19))   STRATEGY/HEAP
   ((Y_19 NIL C_17 XXX_2) (C_17 0 ^C_18 ^XXX_22))   STRATEGY/LABEL
    ((C_18 NIL NIL)   ($OPEN-FRAME 1 ^C_51 '#{CALL-NODE CATCH-CONTINUE_5 51700042}))   STRATEGY/OPEN
     ((C_51)           ($WRITE-FUNCTIONAL-DEST 1 ^C_50 '16 'UNWIND-MARKER))   STRATEGY/OPEN
      ((C_50 NIL V_49)  ($WRITE-FUNCTIONAL-DEST 1 ^C_48 '17 'TAG))   STRATEGY/OPEN
       ((C_48 NIL V_47)  ($WRITE-FUNCTIONAL-DEST 1 ^C_46 '18 *SPECIAL-PDL*_55))   STRATEGY/OPEN
        ((C_46 NIL V_45)  ($WRITE-FUNCTIONAL-DEST 1 ^C_44 '19 *STACK-POINTER*_54))   STRATEGY/OPEN
         ((C_44 NIL V_43)  ($%CATCH-CONT 1 ^C_42))   STRATEGY/OPEN
          ((C_42 NIL V_41)  ($OPEN-FRAME 1 ^C_32 '#{CALL-NODE BAR_14 51702645}))   STRATEGY/OPEN
           ((C_32)           (BAR_14 1 ^B_36))   STRATEGY/OPEN
            ((B_36 IGNORE_35) ($%GO 2 K_0 XXX_2))   STRATEGY/OPEN
    ((XXX_22 NIL K_3) ($OPEN-FRAME 1 ^C_23 '#{CALL-NODE PRINT_4 51677416}))   STRATEGY/LABEL
     ((C_23)           (PRINT_4 1 K_3 'DONE))   STRATEGY/OPEN

Post Processed:
CATCH-EXITED_15
   (MOVEI O0 (QUOTE UNWIND-MARKER) BOXED CH-OPEN)
   (MOVEI O1 (QUOTE TAG) BOXED)
   (MOVE O2 (REGISTER *SPECIAL-PDL* 6 1))
   (MOVE O3 (REGISTER *STACK-POINTER* 6 0))
   (MOVE-PC IGNORE NIL)
   (OPEN-CALL (BAR 0) IGNORE NIL)
   (CALL (FLUSH-OPEN-FRAME 0) IGNORE NIL)
XXX_22
   (MOVEI O0 (QUOTE DONE) BOXED CH-TAIL-OPEN)
   (TAIL-CALL (PRINT 1) NIL)

(defmacro catch1 (tag &body body)
  `(labels ((cont ()
               (hw:call 'catch-continue 6)))
     (%catch-open #'cont ,tag)
     (setf (hw:o5) (nc:%values (progn ,@body)))
     (cont)))



(defmacro CATCH (tag &body body)
  (let ((cont (gensym 'cont)))
    `(PROGN
       (BLOCK ,cont
         (%CATCH-OPEN ,cont ,tag)
         (SETF (HW:O5) (NC:%VALUES (PROGN ,@body))))
       (%CATCH-CONTINUE))))

(define-special-form CATCH (tag . body) (env)
  (let ((cont (create-variable 'cont)))
    `(PROGN
       ((BLOCK ,cont
          ((%CATCH-OPEN ,cont ,(alpha tag env))
           ,(alpha `(SETF (HW:O5) (%VALUES (PROGN ,@body)))
                   env)))
        (,primop/%catch-continue)))))


;(define-special-form %CATCH-OPEN (cont tag) (env)
;  (let ((block (assoc cont *blocks*)))
;    (let ((var (or (cdr block)
;                  (setf (cdr block) (create-variable (car block))))))
;      (list '%CATCH-OPEN var (alpha tag env)))))

(define-compilator (%CATCH-OPEN cont tag)
  (let ((call (make-call-with-exits 2 `(,primop/%catch-open ,empty ,cont ,tag))))
    (values call call (call-arg 1))))

(define-primop %catch-open (cont tag)
  (:generate (node)
     (let ((cont (call-arg-n 2 node)))
       (let ((cont-lambda
               (cond ((reference-node? cont)
                      (variable-known (reference-variable cont)))
                     ((lambda-node? cont)
                       cont)
                     (t (bug "funny catch cont")))))
       (generate-open-1 (tail-continuation-p (call-arg-n 1 (lambda-body cont-lambda))))
       ;; a kluge
       (if (eq cont cont-lambda)
           (lambda-queue cont-lambda)
         (setf (lambda-dynamic-state (variable-binder (reference-variable cont)))
               *dynamic-state*))
       (generate-move ''LI:UNWIND-MARKER  O0)
       (generate-move (call-arg-n 3 node) O1)
       (generate-move 'GR:*SPECIAL-PDL*   O2)
       (generate-move 'GR:*STACK-POINTER* 'K:O3)
       (emit 'K:MOVE-PC 'K:O4 (get-tag cont))
       IGNORED))))

(define-primop %catch-continue ()
  (:side-effects? t)
  (:generate (node)
     (pop *dynamic-state*)
     (let ((dest (get-destination (call-arg-n 1 node))))
       (emit-call 'LI:CATCH-CONTINUE 6 dest)
       dest)))

(defun foo ()
  (catch 'tag (bar)))

(defun foo (x)
  (print (catch 'tag (1+ x))))


(defun catch-exited ()
  (tagbody
      (catch 'tag (bar) (go xxx))
      (print 'caught)
    xxx
      (print 'done)))





;;; ---------

block with dynamic rf needs to be like catch
but we don't know that until after analyze...


can we uncanonicalize catch mv?

(we know all value places though...)

catch
  ... open-catch catch-cont

  ... body ...
  unconditional-branch cont

catch-cont
  (cont ... values ...)
cont
  ...





(defun foo (x)
  (throw 'tag (bar)))


(defun dynamic-rf ()
  (block foo
    (bar #'(lambda (x) (return-from foo (fff)))
         3))
  (print 'done))

(call-mult-value #'fff nil)

(defun mvcall ()
  (multiple-value-bind (x y)
      (fff)
    (foo x y)))

(call-mult-value #'fff 2)


throw of multiple values is not hard
we just put them in *return's and throw with bit set
which does throw-sv or throw-mv

  if we know we have single values we can just call throw-sv

  if we don't know, we can wrap %values


dynamic rf


with rf we know where all the values come from

;;; Generate a catch open for a block which
;;; can have a dynamic return-from.
;;;
;;; BLOCK
;;;   <catch-open>
;;;   (MOVE-PC O4 BLOCK-CONT)
;;;   (UCONDITIONAL-BRANCH BODY)
;;; BLOCK-CONT
;;;   <assign r1 & values to cont-wants>
;;; CONT
;;;   <continuation of block>
;;; BODY
;;;   <body of block>
;;;   (UNCONDITIONAL-BRANCH CONT)
;;;
;;; this assumes that dynamic return-from
;;; calls a version of throw which pops the catch frame
;;;
;;; this doesn't quite work, because if the block continuation
;;; is expecting something in an O reg, the body will move it
;;; there even though catch frame is open...
;;;
(defun generate-return-from-catch (conts)
  ;; if there is more than one called exit
  ;; then this is not a block
  ;; what is it?
  (let* ((cvar (car conts))
         (cont (variable-known cvar)))
    (generate-catch-1 (get-tag cvar) ''block-cont)
    (emit-unconditional-branch 'body)
    (emit-tag 'block-cont)
    ;; first value comes back in R1
    (if cont
        (generate-continuation R1 cont)
      ; ;; generate-continuation does this
      ; ;; but a variable is not a continuation...
      ; (generate-primop-return R1 cvar)
      (progn
        ;; ** unwind dynamic state
        (generate-move R1 RETURN-TAIL)
        (emit-return)))
    (emit-tag 'body)))

(define-compilator (%xCATCH-OPEN cont tag)
  (let ((call (make-call-with-exits 2 `(,primop/%xcatch-open ,empty ,cont ,tag))))
    (values call call (call-arg 1))))

(define-primop %xCATCH-OPEN (catch-cont tag)
  (:side-effects? t)
  (:generate (node)
     (generate-xcatch node)))

(defun generate-xcatch (node)
  (destructure (((body-cont catch-cont tag) (call-args node)))
    (let ((cont (if (lambda-node? catch-cont)
                    catch-cont
                  (variable-known (reference-variable catch-cont)))))
      (generate-catch-1 tag ''throw-cont)
    (emit-unconditional-branch 'body)
    (emit-tag 'throw-cont)
    ;; first value comes back in R1
    (if cont
        (generate-continuation R1 cont)
      ; ;; generate-continuation does this
      ; ;; but a variable is not a continuation...
      ; (generate-primop-return R1 cvar)
      (progn
        ;; ** unwind dynamic state
        (generate-move R1 RETURN-TAIL)
        (emit-return)))
    (emit-tag 'body))
    IGNORED))


(defun dynamic-rf ()
  (block foo
    (bar #'(lambda (x) (return-from foo x))
         3))
  (print 'done))

;;; this doesn't quite work, because if the block continuation
;;; is expecting something in an O reg, the body will move it
;;; there even though catch frame is open...
(defun dynamic-rf ()
  (print (block foo
           (bar #'(lambda (x) (return-from foo x))
                3)))
  (print 'done))

;;; (a b) can be bound to the result of
;;;    - the values (no mv-bind needed)
;;;    - fff        (mv-bind needed)
;;;    - bar        (mv-bind needed)
(defun dynamic-rf-fcn ()
  (multiple-value-bind (a b)
      (block foo
        (if (pred)
            (values 3 4)
          (bar #'(lambda (x) (return-from foo (fff x)))
             3)))
    (done a b)))


(defun dynamic-rf-mv ()
  (multiple-value-bind (a b c)
      (block foo
        (if (pred)
            (values 3 4 5)
          (bar #'(lambda (x) (return-from foo (values 6 7 8)))
               3)))
    (done a b c)))

;;; this doesn't need an mvbind in catch cont
;;; because values passed to foo are always known...
;;; (but it doesn't hurt, because throw will always
;;; set mv stuff)
;;; (however, for return from, it doesn't need to do
;;; that either, could call somve throw variant which
;;; knows how many to return...)
(defun dynamic-rf-mv ()
  (multiple-value-bind (a b c)
      (block foo
        (if (pred)
            (values 3 4 5)
          (progn
            (bar #'(lambda () (return-from foo (values 6 7 8)))
                 3)
            (values 11 12 13))))
    (done a b c)))

;;; needs to return-tail
(defun foo (x)
  (block foo
    (if (pred)
        259
      (bar #'(lambda (x) (return-from foo (fff x)))))))


(defun foo (x)
  (block foo
    (if (pred)
        259
      (bar #'(lambda (x) (if x (return-from foo 3)))))))


???????????????????

no %values !!!!

the body of a catch does not need to set the mv flag
and move to *returns if the continuation pc in the frame
is not the same as the catch forms continuation

catch-open throw-pc

body
  <body>
  ubranch catch-cont

catch-cont
  .....


throw-pc
  move mv's if necc
  unconditional-branch catch-cont


if catch-cont is return
will need to do return-tail


?????????????????








Throw Pops Catch

   - T.  Body must pop catch

   - NIL.


Body Continues at throw-pc

   - T.  Needs canonical place for value
           and for multiple values
           mv-bind/values loses

         Needs body to set MV bit


   - NIL.



;----------------------------------------------------------------

(define-compilator (%catch-body body)
  (multiple-value-bind (node c-parent c-role) (->node body)
    (cond ;;
          ;; '259 => ($%catch-body-values 1 <cont> '259)
          ;;
          ((or (literal-node? node)
               (reference-node? node))
           (make-call `(,primop/%catch-body-values ,node)))
          ;;
          ;; (<cont> 0 '3 V_1 V_2) => ($%CATCH-BODY-VALUES 1 <cont> '3 V_1 V_2)
          ;;
          ((eq call-proc c-role)
           (let ((args (copy-list (call-args c-parent))))
             (mapc #'detach args)
             (let ((new-call (make-call `(,primop/%catch-body-values . ,args))))
               (values new-call new-call (call-arg 1)))))
          ;;
          ;; (( ... ) (^P_1 0 ^C_2))
          ;;  ((P_2 NIL J_3) ...
          ;;     ...  (J_3 0 X_6)     =>  ($%CATCH-BODY-VALUES 1 J_3 X_6)
          ;;     ...  (BAR 1 J_3 V_7) =>  ...        (BAR 1 ^C_22 V_7)
          ;;                             ((C_22 V_23) ($%CATCH-BODY-VALUES 1 J_3 V_23))
          ;;
          ((lambda-node? (call-proc c-parent))
           (let ((jvar (car (lambda-variables (call-proc c-parent)))))
             (when jvar
               (dolist (ref (variable-refs jvar))
                 (let ((ref-role (node-role ref))
                       (ref-parent (node-parent ref)))
                   (detach ref)
                   (cond ((eq ref-role call-proc)
                          (let ((args (copy-list (call-args ref-parent))))
                            (mapc #'detach args)
                            (replace-node
                              ref-parent
                              (make-call-with-exits 1 `(,primop/%catch-body-values ,ref . ,args)))))
                         ((and (eq ref-role (call-arg 1))
                               (= 1 (call-exits ref-parent)))
                          (relate (call-arg 1)
                                  (insert-%catch-body-values-call-as-cont ref-parent)
                                  ref))
                         (t (bug "funny reference to bound cont in %values")))))))
             (values node c-parent c-role))
          ;;
          ;; (BAR 1 <cont> X_1) => ...         (BAR 1 ^C_22 X_1)
          ;;                       ((C_22 V_23) ($%CATCH-BODY-VALUES 1 <cont> V_23))
          ;;
          ((and (eq c-role (call-arg 1))
                (= 1 (call-exits c-parent)))
           (values node (insert-%catch-body-values-call-as-cont c-parent) (call-arg 1)))
          (t (bug "something funny in %values")))))


(defun insert-%catch-body-values-call-as-cont (parent)
  (let* ((var (create-variable 'v))
         (lambda (create-lambda-node 'c (list nil var)))
         (call (make-call `(,primop/%catch-body-values ,var))))
    (relate lambda-body lambda call)
    (relate (call-arg 1) parent lambda)
    call))


(define-special-form CATCH (tag . body) (env)
  (let ((cont (create-variable 'cont)))
    `(PROGN
       ((BLOCK ,cont
          ((%CATCH-OPEN ,cont ,(alpha tag env))
           (%CATCH-BODY ,(alpha `(PROGN ,@body)
                                env))))
        (,primop/%catch-continue)))))


(define-primop %catch-open (cont tag)
  (:generate (node)
     (generate-catch-open node)))

(define-primop %catch-body-values (&rest values)
  (:special? t)
  (:generate (node)
     (generate-catch-body-values node)))


(defun generate-catch-open (node)
  (let ((cont (call-arg-n 2 node)))
    (let ((cont-lambda
            (cond ((reference-node? cont)
                   (variable-known (reference-variable cont)))
                  ((lambda-node? cont)
                   cont)
                  (t (bug "funny catch cont")))))
      (let ((tail-p (tail-continuation-p (call-arg-n 1 (lambda-body cont-lambda)))))
        (generate-open-1 tail-p)
        ;; a kluge
        (unless (eq cont cont-lambda)
          (setf (lambda-dynamic-state (variable-binder (reference-variable cont)))
                *dynamic-state*))
        (if (or tail-p
                (lambda-node? cont))
            (lambda-queue cont-lambda))
        (generate-move ''LI:UNWIND-MARKER  O0)
        (generate-move (call-arg-n 3 node) O1)
        (generate-move 'GR:*SPECIAL-PDL*   O2)
        (generate-move 'GR:*STACK-POINTER* 'K:O3)
        (emit 'K:MOVE-PC 'K:O4 (get-tag cont-lambda))
        IGNORED))))

(defun generate-mv-bind-1 (first-value-in first-value-place rest-values-places)
  (let ((number-of-mvs (length rest-values-places)))
    (let ((mvbind-fcn (nth number-of-mvs '(LI:MVBIND-1 LI:MVBIND-2
                                           LI:MVBIND-3 LI:MVBIND-4
                                           LI:MVBIND-5 LI:MVBIND-6))))
      (if mvbind-fcn
          (generate-internal-call mvbind-fcn first-value-place first-value-in)
        (generate-internal-call 'LI:MVBIND-N first-value-place `',(1+ number-of-mvs) first-value-in)))
    (do ((i 0 (1+ i))
         (places rest-values-places (cdr places)))
        ((= i number-of-mvs))
      (generate-move (nth i *mv-return-registers*)
                     (car places)))))



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
  (pop *dynamic-state*) ;??
  (let ((nvalues (length values)))
    (cond
      ((= 1 nvalues)
       (let ((value (car values)))
         (generate-move value 'K:O5)
         (unwind-dynamic-state (reference-variable cont))
         (emit-call (if (value-of-unknown-call? value)
                        'LI:CATCH-CONTINUE-TAIL
                      'LI:CATCH-CONTINUE-SV)
                    6 RETURN t)))
      ((= 0 nvalues)
       (generate-move ''0 'GR:*NUMBER-OF-RETURN-VALUES*)
       (generate-move ''NIL 'K:O5)
       (emit-call 'LI:CATCH-CONTINUE-MV 6 RETURN t))
      (t
       (generate-move (car values) 'K:O5)
       (parallel-assign (cdr values) *mv-return-registers*)
       (generate-move `',(+ nvalues *mv-return-nargs-offset*)
                      'GR:*NUMBER-OF-RETURN-VALUES*)
       (emit-call 'LI:CATCH-CONTINUE-MV 6 RETURN t)))))


(define-primop %catch-continue ()
  (:side-effects? t)
  (:special? t)
  (:generate (node)
     (generate-catch-continue node)))

(defun generate-catch-continue (node)
     (pop *dynamic-state*)
     (let ((dest (get-destination (call-arg-n 1 node))))
       (emit-call 'LI:CATCH-CONTINUE 6 dest)
       (generate-continuation (if (eq dest RETURN) RETURN-TAIL dest) (cont node) t)))

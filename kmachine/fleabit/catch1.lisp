;;; -*- Mode:LISP; Package:NC; Readtable:CL; Fonts:(CPTFONT TVFONT); Base:10 -*-



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


  1one solution would be to make a closure out of the body*
  1(STRATEGY/PROC)*
   1 (catch 'tag (funcall #'(lambda () 3)))*


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


(define-compilator (%values body)
  (multiple-value-bind (node c-parent c-role) (->node body)
    (cond ((or (literal-node? node)
               (reference-node? node))
           (make-call `(,primop/%values1 ,node)))
          ((eq call-proc c-role)
           (let ((args (copy-list (call-args c-parent))))
             (mapc #'detach args)
             (let ((new-call (make-call `(,primop/%values1 . ,args))))
               (values new-call new-call (call-arg 1)))))
          ((lambda-node? (call-proc c-parent))
           (let ((jvar (car (lambda-variables (call-proc c-parent)))))
             (dolist (ref (variable-refs jvar))
               (cond ((eq (node-role ref) call-proc)
                      (let ((args (copy-list (call-args (node-parent ref)))))
                        (mapc #'detach args)
                        (replace-node
                          (node-parent ref)
                          (make-call-with-exits 1 `(,primop/%values1 ,(detach ref) . ,args)))))
                     (t
                      (cerror "foo" "bound cont passed"))))
             (values node c-parent c-role)))
          ((and (eq c-role (call-arg 1))
                (= 1 (call-exits c-parent)))
           (let* ((var (create-variable 'v))
                    (lambda (create-lambda-node 'c (list nil var)))
                    (call (make-call `(,primop/%values1 ,var))))
               (relate lambda-body lambda call)
               (relate c-role c-parent lambda)
               (values node call (call-arg 1))))
          (t (cerror "foo" "whats this?" )))))


(define-special-form %VALUES (body) (env)
  (list '%VALUES (alpha body env)))

(define-primop %values1 (&rest values)
  (:generate (node)
     (generate-values node)))


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












jrd
253-0314 office
    0316 lispm room
    0360 terminal garden

jrd@media-lab.edu
    oz
    mc

tues 7:30 pamplona

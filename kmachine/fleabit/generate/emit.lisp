;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-

;;;; Emit Code

(defvar *instructions* '())

;;; Control whether to remove branch-point-plus-1 instruction sequences.
(defparameter *remove-branch-point-plus-1* t)

(defvar *tag-package* (or (find-package 'tag)
                               (make-package 'tag)))


(defun get-tag (tag)
  (if (symbolp tag)
      tag
    (let ((var (cond ((lambda-node? tag)
                      (lambda-self-var tag))
                     ((reference-node? tag)
                      (reference-variable tag))
                     ((variable-p tag) tag)     ;??
                     (t (bug "Bad tag")))))
;    (or (debug :tags
      (intern (variable-unique-name var) *tag-package*))
;       (variable-id var)))
  ))

(defun emit-template (n1 n2)
  (let ((tag (get-tag n1)))
    (debug :emit (format t "~&~a" tag))
    (push tag *instructions*)))

(defun gen-tag (arg)
  (gensym arg))

(defun emit-tag (tag)
  (setq tag (get-tag tag))
  (emit-tag-1 tag))

(defun emit-tag-1 (tag)
  (debug :emit (format t "~&~a" tag))
  (push tag *instructions*))

(defmacro emit-comment (format-string &rest args)
  `(debug :comments
     (format *debug-stream* "~&;;; ")           ; $$$ Changed stream from T <11-Nov-88 smh>
     (format *debug-stream* ,format-string ,@args)))

(defun emit-local-jump (node)
  (emit 'K:LOCAL-JUMP (get-tag node)))

(defun emit-jump (fcn nargs)
  (emit 'K:JUMP (list (get-proc-name fcn) nargs)))

(defun emit-return ()
  (emit 'K:RETURN))

(defun emit-call (name nargs &optional (dest IGNORED) (tail-p (eq dest RETURN)))
  (if tail-p
      (emit 'K:TAIL-CALL (list (get-proc-name name) nargs))
    (emit 'K:CALL (list (get-proc-name name) nargs) dest)))

(defun get-proc-name (proc)
  (cond ((reference-node? proc)
         (variable-name (reference-variable proc)))
        ((lambda-node? proc)
         (variable-unique-name (lambda-self-var proc)))
        ((symbolp proc) proc)
        ((and (listp proc)
              (eq (car proc) :INTERNAL))
         proc)
        ((ncompiled-function-p proc) proc)
        (t (bug "Weird proc"))))


(defconstant *inverse-conditions*
             `((K:BR-EQUAL                . K:BR-NOT-EQUAL)
               (K:BR-NOT-EQUAL            . K:BR-EQUAL)
               (K:BR-ZERO                 . K:BR-NOT-ZERO)
               (K:BR-NOT-ZERO             . K:BR-ZERO)
               (K:BR-NOT-NEGATIVE         . K:BR-NEGATIVE)
               (K:BR-POSITIVE             . K:BR-NOT-POSITIVE)
               (K:BR-NOT-POSITIVE         . K:BR-POSITIVE)
               (K:BR-NEGATIVE             . K:BR-NOT-NEGATIVE)
               (K:BR-GREATER-THAN         . K:BR-NOT-GREATER-THAN)
               (K:BR-NOT-GREATER-THAN     . K:BR-GREATER-THAN)
               (K:BR-NOT-LESS-THAN        . K:BR-LESS-THAN)
               (K:BR-LESS-THAN            . K:BR-NOT-LESS-THAN)
               (K:BR-NOT-GREATER-OR-EQUAL . K:BR-GREATER-OR-EQUAL)
               (K:BR-GREATER-OR-EQUAL     . K:BR-NOT-GREATER-OR-EQUAL)
               (K:BR-NOT-LESS-OR-EQUAL    . K:BR-LESS-OR-EQUAL)
               (K:BR-LESS-OR-EQUAL        . K:BR-NOT-LESS-OR-EQUAL)))


(defun inverse-cond (cond)
  (let ((inv (assoc cond *inverse-conditions*)))
    (if inv (cdr inv)
      (bug "No inverse cond for ~a" cond))))


(defun emit-inverse-test (cond)
  (emit 'K:TEST (inverse-cond cond)))

(defun emit-test (cond)
  (emit 'K:TEST cond))

(defun emit-branch (node)
 (emit 'K:BRANCH (get-tag node))
;  (emit 'K:JUMP-CONDITIONAL (get-tag node))
 )

(defun emit-unconditional-branch (node)
  (emit 'K:UNCONDITIONAL-BRANCH (get-tag node))
;  (emit 'K:JUMP (get-tag node))
  )

(defun emit-move (from to &optional options)
  ;; Make EMIT-MOVE be specific about boxedness because the assembler likes
  ;; to randomly put in defaults and sometimes the defaults conflict making
  ;; for erroneous code.  In particular, when a move gets glommed onto a
  ;; unconditional branch, the boxed bits get inherited from the NOP-instruction
  ;; instead of the move instruction.  I fix it by explicitly stating a boxedness
  ;; unless it is already stated, or if the destination is a cons which seems to
  ;; be the case in the VMA and MD instructions.
  ;; This is truly disgusting, but it does patch the problem for now. - JRM 1-May-87 16:48:20
  (if (or (consp to)
          (intersection '(K:BOXED K:UNBOXED K:BOXED-LEFT K:BOXED-RIGHT
                                  K:BOXED-OUTREG0 K:BOXED-MD K:UNBOXED-MD K:BOXED-VMA K:UNBOXED-VMA)
                        options))
      (apply #'emit 'K:MOVE to from options)
      (apply #'emit 'K:MOVE to from (cons 'K:BOXED-RIGHT options))))

(defun emit-movei (from to &optional options)
 (apply #'emit 'K:MOVEI to from
        (let ((val (get-literal-value from)))
         (if (and (consp val)
                  (eq (first val) 'hw:unboxed-constant))
             'K:UNBOXED
           'K:BOXED))
         options))

(defun emit-alu (op dest left right &optional options)
  (apply #'emit 'K:ALU op dest left right options))

(defun emit-alu-field (op dest left right byte-spec pw &rest options)
  (apply #'emit 'K:ALU-FIELD op dest left right byte-spec pw options))

(defun emit-code (code)
  (dolist (codething code)
    (if (consp codething)
        (apply #'emit codething)
      (emit-tag codething))))

(defun emit (op &rest operands)
  (let ((i (cons op (hack-operands operands))))
    (debug :emit (format t "~&  ~a" i))
    (push  i *instructions*)))


(defun hack-operands (ops)
  (let ((new-ops '()))
    (dolist (op ops)
      (push
        (cond ((integerp op)
               (aref *all-regs* op))
              ((literal-node? op)
               (let ((val (leaf-value op)))
                 (if (and (consp val)
                          (or (eq (first val) 'hw:unboxed-constant)
                              (eq (first val) 'hw:boxed-constant)))
                     (second val)
                   (list 'quote (leaf-value op)))))
              ((reference-node? op)
               (variable-name (leaf-value op)))
              ((lambda-node? op)
               (variable-unique-name (lambda-self-var op)))
              ((and (consp op) (integerp (car op)) (integerp (cdr op)))
               (cons (aref *all-regs* (car op)) (cdr op)))
              ((and (consp op) (eq (car op) '/#))
               (cdr op))
              (t op))
        new-ops))
    (nreverse new-ops)))


(defun i-type (i)
  (if (consp i)
      (car i)
    'LABEL))

(defun post-process (instructions)
   ;this is the peephole optimizer!
   ; "holding registers"  -- new feature added 3/11/88
   ;  k:h<n> are really the same as k:r<n> except you are guarenteed the value
   ;  will be used at most once.  So (MOVE K:H<N> FOO) (MOVE BAR K:H<N>) ==> (MOVE BAR FOO), etc.
  (do ((instructions (cdr instructions) (cdr instructions))
       (i (car instructions) (car instructions))
       (new-instructions '()))
      ((null instructions) (push i new-instructions)
       ;; Eliminate any instructions which were declined.
       (remove nil new-instructions))
    (let* ((i-type (i-type i))
           (prev-i (car instructions))
           (prev-i-type (i-type prev-i)))
      (push (case i-type
              (K:CALL
        ;add NOP after CALL if followed by return.
               (let ((next-i (dolist (new-i new-instructions)
                               (when (consp new-i) (return new-i)))))
                 (when (or (member (car next-i)
                                   '(K:RETURN K:RETURNI))
                           (and (or (eq (car next-i) 'K:MOVE)
                                    (eq (car next-i) 'K:MOVEI))
                                (member (second next-i) '(K:RETURN K:RETURN-MV K:RETURN-TAIL)))
                           (and (or (eq (car next-i) 'K:ALU)
                                    (eq (car next-i) 'K:ALU-FIELD))
                                (member (third next-i) '(K:RETURN K:RETURN-MV K:RETURN-TAIL))))
                   (push '(K:NOP) new-instructions)))
               (case prev-i-type
                 (K:MOVE (if (or (different-frame-globals? (third i) (second prev-i))
                                 (different-frame-globals? (third i) (third prev-i))
                                 (member 'k:ch-return prev-i))
                             `(K:CALL ,(second i) ,(third i) () . ,(cdddr i))
                           (progn
                             (pop instructions)
                             (case (i-type (car instructions))
                               (K:OPEN
                                (if (or (member 'k:ch-open prev-i)
                                        (member 'k:ch-tail-open prev-i)
                                        (cdr (car instructions)))
                                    ;; If there's already an OPEN going on in the MOVE, we can't
                                    ;; merge in the OPEN.  Similarly, if there's any hair going on in the OPEN.
                                    `(k:call ,(second i) ,(third i) ,(cdr prev-i) ,@(cdddr i))
                                  (progn ; @#$(*&#@ facists!
                                    (pop instructions)
                                    `(K:OPEN-CALL ,(second i) ,(third i) ,(cdr prev-i) . ,(cdddr i)))))
                               (t
                                `(K:CALL ,(second i) ,(third i) ,(cdr prev-i) . ,(cdddr i)))))))
                 (K:OPEN (pop instructions)
                         `(K:OPEN-CALL ,(second i) ,(third i) () . ,(cdddr i)))
                 (t `(K:CALL ,(second i) ,(third i) () . ,(cdddr i)))))
              ;; gack
              (K:TAIL-CALL
               (case prev-i-type
                 (K:MOVE
                  (cond ((member 'k:ch-return prev-i)
                         `(k:tail-call ,(second i) () ,@(cddr i)))
                        (t
                         (pop instructions)
                         (case (i-type (car instructions))
                           (K:TAIL-OPEN
                            (pop instructions)
                            `(K:OPEN-TAIL-CALL ,(second i) ,(cdr prev-i) . ,(cddr i)))
                           (t
                            `(K:TAIL-CALL ,(second i) ,(cdr prev-i) . ,(cddr i)))))))
                 (K:TAIL-OPEN (pop instructions)
                              `(K:OPEN-TAIL-CALL ,(second i) () . ,(cddr i)))
                 (t `(K:TAIL-CALL ,(second i) () . ,(cddr i)))))
              (K:RETURN
               (cond
;                ((and (eq prev-i-type 'K:MOVE)
;                      (eq (second prev-i) 'K:RETURN))
;                 (pop instructions)
;                 `(K:RETURN ,(third prev-i)))
;                ((and (eq prev-i-type 'K:MOVEI)
;                      (eq (second prev-i) 'K:RETURN))
;                 (pop instructions)
;                 `(K:RETURNI ,(third prev-i)))
                 ((member prev-i-type '(K:MOVE K:MOVEI K:ALU K:ALU-FIELD K:FALU K:FMUL))
                  (pop instructions)
                  (append prev-i (list 'K:CH-RETURN 'K:NEXT-PC-RETURN)))
                 ((member prev-i-type '(K:TAIL-CALL K:OPEN-TAIL-CALL))
                  NIL)
                 (t i)))
              ((K:MOVE K:MOVEI K:ALU K:ALU-FIELD K:ALUI-16)
               (cond ((or (global:memq 'K:CH-OPEN i)            ;until cross compiler knows about new-open call destination
                          (global:memq 'K:CH-TAIL-OPEN i))      ;  it can get screwwed unless this test here.
                      i)
;                    ((and (eql (car i) 'k:move)
;                          (eql (second i) (third i)))          ;flush (move x x)
;                     nil)
                     ((and (eq (first i) 'k:move)
                           (eq (second i) 'k:return)
                           (eq (third i) 'gr:*save-return-crap-0*)
                           (member 'k:boxed-right (cdddr i))
                           (member 'k:ch-return (cdddr i))
                           (member 'k:next-pc-return (cdddr i)))
                      (case prev-i-type
                        ((k:move k:move-i )
                         (cond ((eq (second prev-i) 'gr:*save-return-crap-0*)
                                (pop instructions)
                                `(,(first i) ,(second i) ,(third prev-i)
                                  k:ch-return k:next-pc-return ,@(cdddr prev-i)))
                               (t i)))
                        (otherwise i)))
                     (t
                      (case prev-i-type
                        (K:OPEN (pop instructions)
                                (append i (list 'K:CH-OPEN)))
                        (K:TAIL-OPEN (pop instructions)
                                     (append i (list 'K:CH-TAIL-OPEN)))
                        (t i)))))
              ((K:BRANCH K:UNCONDITIONAL-BRANCH)
               (cond ((and (member prev-i-type '(K:MOVE K:ALU))
                           (not (member 'k:ch-return prev-i)))
                      (pop instructions)
                      `(,(first i) ,(second i) ,prev-i . ,(cddr i)))
                     (t (append i '(())))))
              (K:JUMP (cond ((and (eq prev-i-type 'K:MOVE)
                                  (not (member 'k:ch-return prev-i)))
                             (pop instructions)
                             `(K:JUMP ,(second i)  ,(cdr prev-i) . ,(cddr i)))
                            (t `(K:JUMP ,(second i) () . ,(cddr i)))))
              (K:JUMP-CONDITIONAL
               (cond ((and (eq prev-i-type 'K:MOVE )
                           (member 'k:ch-return prev-i))
                      (pop instructions)
                      `(K:JUMP-CONDITIONAL ,(second i)  ,(cdr prev-i) . ,(cddr i)))
                     (t `(K:JUMP-CONDITIONAL ,(second i) () . ,(cddr i)))))
              ;; Move branch-point-plus-1 stuff.
              (label
               (when *remove-branch-point-plus-1*
                 (case prev-i-type
                   ((k:unconditional-branch)
                    (when (eql i (second prev-i))
                      (pop instructions)))
                   ((k:branch)
                    (when (eql i (second prev-i))
                      (pop instructions)
                      (case (i-type (first instructions))
                        ((k:test)
                         (pop instructions)
                         (case (i-type (first instructions))
                           ((k:move)
                            (case (second (first instructions))
                              ((k:nop)
                               (pop instructions)))))))))))
               i)

              (t i))
            new-instructions))))


(defun print-instructions (insts &optional (stream *debug-stream*) code)
  ;;(format stream "~%")
  (let ((count 0))
    (loop for i in insts
          with k = code
          as num = (or (car k) 0)
          (cond ((consp i)
             (format stream "~&~4x:~:[~4*~; ~4,'0x ~4,'0x ~4,'0x ~4,'0x~]  (~a~~{~^ ~<~%~1,100:;~a~;~>~}~)"
                     count code
                     (logand #xffff (ash num -48)) (logand #xffff (ash num -32))
                     (logand #xffff (ash num  -16)) (logand #xffff num)
                     (car i) (cdr i))
             (incf count)
             (setq k (cdr k)))
            (t (format stream "~&~:[~6t~;~26t~]~a" code i))))))

;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;;; Primitive Operations

;----------------------------------------------------------------
;;;; Compiler Internal Primitives

;;; from primops;base
(define-primop Y (???)
  (:generate (node)
    (generate-labels node))
  (:simplify (node)
    (simplify-y node))
  (:special? t))

(define-primop conditional (test-primop &rest args)
  (:generate (node)
      (primop.generate (primop-value (call-arg-n 3 node)) node))
  (:conditional? t)
  (:simplify (node)
      (primop.simplify (primop-value (call-arg-n 3 node)) node)))

(define-primop test (???)
  (:generate (node)
    (primop.generate (primop-value (call-arg-n 4 node)) node))
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
    (simplify-test node))
  (:conditional? t))

(define-primop true? (value)
  (:generate (node)
     (generate-nil-test node))
  (:presimplify (node)
     (presimplify-predicate node))
  (:type-predicate? (primop) t)
;  (:type (node) '(proc (cont boolean?) top?))
  )

;(define-primop contents-location
;  (:generate (node)
;    (generate-contents-location node)))


(define-primop setq-lexical (var value)
  (:side-effects? t)
  (:generate (node)
    (generate-lexical-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node))))

(define-primop setq-special (symbol value)
  (:side-effects? t)
  (:generate (node)
    (generate-special-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node))))

(define-primop special-ref (symbol)
  (:generate (node)
     (generate-special-ref
       node
       (call-arg-n 2 node))))

(define-primop function-ref (symbol)
  (:generate (node)
     (generate-function-ref
       node
       (call-arg-n 2 node))))


;;; Primop which generates setup code
;;; for optional argument
(define-primop optional-setup (first-label nargs supplied-p-vars &rest init-labels)
  (:generate (node)
    (generate-optional-setup node))
  (:side-effects? t)
  (:special? t))

;;; this does nothing
;;; optional inits use it
(define-primop noop ()
  (:generate (node)
     node nil)
  (:side-effects? t))

;;; Primop which generates an OPEN instruction
;;; this primop is put in by make-user-call
;;; during node conversion
(define-primop open-frame (call)
  (:side-effects? t)
  (:generate (node)
    (generate-open node)))


;---------------------------------------------------------------
;;;; Lisp Primitives

(define-primop eq (x y)
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )


;;;; Arithmatic Comparisons

(define-primop zerop (number)
  (:generate (node)
    (generate-arith-predicate node 'K:BR-NOT-ZERO 'K:BW-24))
  (:presimplify (node)
    (presimplify-predicate node))
  (:conditional? t))

(define-primop minusp (number)
  (:generate (node)
    (generate-arith-predicate node 'K:BR-NOT-NEGATIVE 'K:BW-24))
  (:presimplify (node)
    (presimplify-predicate node))
  (:conditional? t))

(define-primop plusp (number)
  (:generate (node)
    (generate-arith-predicate node 'K:BR-NOT-POSITIVE 'K:BW-24))
  (:presimplify (node)
    (presimplify-predicate node))
  (:conditional? t))



(define-primop = (number1 number2)
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

(define-primop < (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-THAN 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

(define-primop > (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-THAN 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

(define-primop >= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-OR-EQUAL 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

(define-primop <= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-THAN-OR-EQUAL 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )


;;;; Logical Primops

(define-primop 2-arg-logand (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logand))
  (:generate (node)
      (generate-binop node 'K:AND 'K:AND 'K:BW-24)))

(define-primop 2-arg-logxor (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logxor))
  (:generate (node)
      (generate-binop node 'K:XOR 'K:XOR 'K:BW-24)))

(define-primop 2-arg-logior (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logior))
  (:generate (node)
      (generate-binop node 'K:OR 'K:OR 'K:BW-24)))

(define-primop ash (n nbits)
  (:simplify (node)
      (simplify-if-constant-expression node 'ash))
  (:generate (node)
      (destructure (((cont n nbits) (call-args node)))
       (if (literal-node? nbits)
           (let ((amount (leaf-value nbits)))
             (if (< amount 0)
                 (if (> amount -6)
                     (let ((dest (continuation-expecting cont)))
                       (dotimes (i (abs amount))
                         (emit-alu 'K:SHIFT-DN-AR-R dest IGNORED (get-right-operand n) 'K:BW-24)))
                   (cerror "foo" "> 5 bit shifts not yet implemented"))
               (if (< amount 6)
                     (let ((dest (continuation-expecting cont)))
                       (dotimes (i amount)
                         (emit-alu 'K:SHIFT-UP-0F-R dest IGNORED (get-right-operand n) 'K:BW-24)))
                   (cerror "foo" "> 5 bit shifts not yet implemented"))))
         (progn
           (cerror "foo" "call ash"))))))


(define-primop byte (size position)
  (:simplify (node)
      (simplify-if-constant-expression node 'byte))
  (:generate (node)
     (destructure (((cont size position) (call-args node)))
        ;; `(hw:dpb ,size vinc:%%byte-size ,position)
        (generate-dpb cont size `',vinc:%%byte-size position))))




;;;; Arithmetic Primops

(defun value-if-literal (node)
  (when (literal-node? node)
    (literal-value node)))

(define-primop 2-arg-+ (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node '+))
  (:generate (node)
      (generate-binop node 'K:L+R 'K:L+R 'K:BW-24)))

(define-primop 2-arg-- (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node '-))
  (:generate (node)
    (generate-binop node 'K:L-R 'K:R-L 'K:BW-24)))

(define-primop 1+ (n)
  (:simplify (node)
      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-unop node 'K:R+1 'K:BW-24))
;  (:type (node) '(proc (cont fixnum?) fixnum?))
  )

(define-primop 1- (n)
  (:simplify (node)
      (simplify-if-constant-expression node '1-))
  (:generate (node)
    (generate-unop node 'K:R-1 'K:BW-24))
;  (:type (node) '(proc (cont fixnum?) fixnum?))
  )


;----------------------------------------------------------------
;;;; Hardware Primops


(define-primop hw:nop ()
  (:side-effects? t)
  (:generate (node)
    (emit 'K:NOP)
    (generate-move ''nil
                   (continuation-expecting (call-arg-n 1 node)))))

;;; compiler internal
;;; (hw:<fsource> ...)  expands into this
(define-primop read-functional-source (fsource)
  (:side-effects? t)
  (:generate (node)
    (generate-move (leaf-value (call-arg-n 2 node))
                   (continuation-expecting (call-arg-n 1 node)))))

;;; compiler internal
;;; (hw:<fdest> ...) expands into this
(define-primop write-functional-dest (fdest value)
  (:side-effects? t)
  (:generate (node)
    (let ((value (call-arg-n 3 node)))
      (generate-move value
                     (leaf-value (call-arg-n 2 node)))
      (generate-move ''nil
                     (continuation-expecting (call-arg-n 1 node))))))


;;; This does not use write-functional-dest because
;;; it has to be done twice and a temp holding the value
;;; could get allocated to MEMORY-MAP then the second
;;; move would not get generated.
(define-primop hw:write-map (value)
  (:side-effects? t)
  (:generate (node)
    (let ((ref (get-right-operand (call-arg-n 2 node))))
      (emit 'K:MOVE 'K:MEMORY-MAP ref 'K:UNBOXED)
      (emit 'K:MOVE 'K:MEMORY-MAP ref 'K:UNBOXED)
      (generate-move ref (continuation-expecting (call-arg-n 1 node))))))

;;; This does not use write-functional-dest because
;;; it has to be done twice and a temp holding the value
;;; could get allocated to GC-RAM then the second
;;; move would not get generated.
(define-primop hw:write-gc-ram (value)
  (:side-effects? t)
  (:generate (node)
    (let ((ref (get-right-operand (call-arg-n 2 node))))
      (emit 'K:MOVE 'K:GC-RAM ref 'K:UNBOXED)
      (emit 'K:MOVE 'K:GC-RAM ref 'K:UNBOXED)
      (generate-move ref (continuation-expecting (call-arg-n 1 node))))))



;;; This does not use write-functional-dest because
;;; it has to be done twice and a temp holding the value
;;; could get allocated to TRANSPORTER-RAM then the second
;;; move would not get generated.
(define-primop hw:write-transporter-ram (value)
  (:side-effects? t)
  (:generate (node)
    (let ((ref (get-right-operand (call-arg-n 2 node))))
      (emit 'K:MOVE 'K:TRANSPORTER-RAM ref 'K:UNBOXED)
      (emit 'K:MOVE 'K:TRANSPORTER-RAM ref 'K:UNBOXED)
      (generate-move ref (continuation-expecting (call-arg-n 1 node))))))





(define-primop hw:dpb (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word))))

(define-primop hw:dpb-xor (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word 'K:FIELD-XOR))))


(define-primop hw:dpb-unboxed (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
       (generate-dpb cont value byte-spec word 'K:FIELD-PASS 'K:UNBOXED))))


(defun hw-ldb (from byte-spec into)
  (dpb (ldb byte-spec from) (byte (byte-size byte-spec) 0) into))

(define-primop hw:ldb (from byte-spec into)
  (:simplify (node)
    (simplify-if-constant-expression node 'hw-ldb))
  (:generate (node)
      (destructure (((cont from byte-spec into) (call-args node)))
       (if (literal-node? byte-spec)
           (emit-alu-field 'K:FIELD-PASS
                (continuation-expecting cont)
                (get-left-operand from)
                (get-right-operand into)
                (let ((byte-spec (leaf-value byte-spec)))
                  `(BYTE ,(byte-size byte-spec)            ;(ldb vinc:%%byte-size byte-spec)         ;target byte-size
                         ,(- (byte-position byte-spec))))  ;(ldb vinc:%%byte-position byte-spec))))  ;target byte-position
                'K:PW-II)
         (progn
           (emit 'K:MOVE R1 (get-right-operand byte-spec) 'K:BW-16)
           (emit-alu 'K:NEG-R R1 IGNORED R1 'K:BW-8)
           (emit-alu 'K:LOAD-STATUS-R 'K:NOP-NO-OVERFLOW-TRAP
                     IGNORED R1 'K:BW-16)
           (emit-alu-field 'K:FIELD-PASS
                           (continuation-expecting cont)
                           (get-left-operand from)
                           (get-right-operand into)
                           ''0
                           'K:PW-RR))))))


(define-primop hw:32logbitp (index n)
  (:conditional? t)
  (:presimplify (node)
     (presimplify-to-conditional node))
  (:generate (node)
     (let ((index (call-arg-n 4 node))
           (n     (call-arg-n 5 node)))
       (if (literal-node? index)
           (emit-alu-field 'K:EXTRACT-BIT-RIGHT 'K:NOP-NO-OVERFLOW-TRAP
                           IGNORED (get-right-operand n)
                           `(BYTE 0 ,(leaf-value index))
                           'K:PW-II)
         (progn
           (emit-alu 'K:LOAD-STATUS-R 'K:NOP-NO-OVERFLOW-TRAP
                     IGNORED (get-right-operand index) 'K:BW-8)
           (emit-alu-field 'K:EXTRACT-BIT-RIGHT 'K:NOP-NO-OVERFLOW-TRAP
                           IGNORED (get-right-operand n)
                           ''0
                           'K:PW-RR)))
       (generate-conditional node 'K:BR-ZERO))))


;;; Don't ask about the next few.

;;; Do a tail open
(define-primop hw:ch-topen ()
  (:side-effects? t)
  (:generate (node)
     (emit 'K:TAIL-OPEN)))

;;; Have the call hardware do a tail call operation
;;; without really calling
(define-primop hw:ch-tcall ()
 (:generate (node)
    (emit 'K:TAIL-CALL '(0 0) 'K:NEXT-PC-PC+1))
 (:side-effects? t))

;;; Have the call hardware do an open-call operation
;;; without transferring control.
(define-primop hw:ch-open-call ()
  (:generate (node)
    (emit 'K:OPEN-CALL '(0 0) 'K:NEXT-PC-PC+1))
  (:side-effects? t))

(define-primop hw:ch-topen-call ()
  (:generate (node)
    (emit 'K:TAIL-OPEN-CALL '(0 0) 'K:NEXT-PC-PC+1))
  (:side-effects? t))

(define-primop hw:ch-return ()
  (:generate (node)
    (emit 'K:NOP 'K:CH-RETURN 'k:next-pc-pc+1))
  (side-effects? t))

;;; dispatch to a computed value
(define-primop hw:dispatch (pc)
  (:generate (node)
    (generate-move (call-arg-n 2 node) 'K:NOP-NO-OVERFLOW-TRAP)
    (emit 'K:NOP)
    (emit 'K:NOP 'K:NEXT-PC-DISPATCH)
    (generate-move ''NIL (continuation-expecting (call-arg-n 1 node))))
  (:side-effects? t))

(define-primop hw:jump (fcn)
  (:side-effects? t)
  (:special? t)  ;we do our own contination (ignore it)
  (:generate (node)
     (emit 'K:JUMP (leaf-value (call-arg-n 2 node)))))

;(define-primop hw:unboxed-constant (number)
;  (:generate (node)
;    (let ((n (call-arg-n 2 node)))
;      (if (and (literal-node? n)
;              (integerp (leaf-value n)))
;       (emit 'K:MOVEI (continuation-expecting (call-arg-n 1 node)) (cons '/# (leaf-value n)) 'K:UNBOXED)
;       (error "Argument to HW:UNBOXED-CONSTANT: ~s should be an integer." (leaf-value n))))))

;;; Actually this is the same as eq?
(define-primop hw:32= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

(define-primop hw:32>= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-OR-EQUAL 'K:BW-32))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

(define-primop hw:32logand (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logand))
  (:generate (node)
      (generate-binop node 'K:AND 'K:AND 'K:BW-32)))

(define-primop hw:32logxor (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logxor))
  (:generate (node)
      (generate-binop node 'K:XOR 'K:XOR 'K:BW-32)))

(define-primop hw:32logior (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logior))
  (:generate (node)
      (generate-binop node 'K:OR 'K:OR 'K:BW-32)))

(define-primop hw:32-1+ (n)
;  (:simplify (node)
;      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-unop node 'K:R+1 'K:BW-32)))

(define-primop hw:32-2+ (n)
;  (:simplify (node)
;      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-unop node 'K:R+2 'K:BW-32)))

(define-primop hw:32-4+ (n)
;  (:simplify (node)
;      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-unop node 'K:R+4 'K:BW-32)))

(define-primop hw:32+ (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L+R 'K:L+R)))

(define-primop hw:32- (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L-R 'K:R-L)))

;;; This is just like equal but doesn't TRAP-UNLES-BOTH-FIX
(define-primop hw:24= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t)
; (:type (node)
;    node
;    '(proc (cont boolean?) fixnum? fixnum?))
 )

;;; like + but no dtp trap
(define-primop hw:24+ (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L+R 'K:L+R 'K:BW-24)))


;;;; Lisp Subprimitives


(define-primop funcall-internal (f)
  (:side-effects? t)
  (:simplify (node)
    (simplify-funcall node))
  (:generate (node)))



;;; ksi

;;; (KSI:OPEN-FRAME <nargs>)
;;;  (alu l+r+2 *arg-1* *arg-1* )
;;;  (alu l+r+2 *arg-1* *arg-1* )
;(define-primop ksi:open-frame
;  (:side-effects? t)
;  (:generate (node)
;     (emit 'K:KOPEN)
;     ;; bump stack-pointer if nargs>16
;     ))


;;; (move *arg-1* <n>)
;;; (move *arg-2* <value>)
;;; (jump ksi:set-arg-aux (*return-pc-1* opc))
;;;
;;; ksi:set-arg-aux
;;;  (alu l-r ignore *arg-1* '15)
;;;  (alu l+r+4 noop *arg-1* opc br-gt)
;;;  (branch set-stack-arg (r+1 noop ignore *return-pc-1*))
;;;  (dispatch)
;;;  (noop) ;this doesn't get executed
;;;  (dispatch (move O0 *arg-2*))
;;;  (dispatch (move O1 *arg-2*))
;;;  (dispatch (move O2 *arg-2*))
;;;  (dispatch (move O3 *arg-2*))
;;;  (dispatch (move O4 *arg-2*))
;;;  (dispatch (move O5 *arg-2*))
;;;  (dispatch (move O6 *arg-2*))
;;;  (dispatch (move O7 *arg-2*))
;;;  (dispatch (move O8 *arg-2*))
;;;  (dispatch (move O9 *arg-2*))
;;;  (dispatch (move O10 *arg-2*))
;;;  (dispatch (move O11 *arg-2*))
;;;  (dispatch (move O12 *arg-2*))
;;;  (dispatch (move O13 *arg-2*))
;;;  (dispatch (move O14 *arg-2*))
;;;  (dispatch (move O15 *arg-2*))
;;; get-stack-arg
;;;

;;; (KSI:SET-ARG <n> <value>)
;(define-primop ksi:set-arg)

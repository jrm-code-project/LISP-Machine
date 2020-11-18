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

(defvar *primop-table* (make-table))

(defmacro define-primop (id &body clauses)
  (let ((name (intern (format nil "PRIMOP/~s" id))))
   `(progn
      (defun ,name () ,(primop-code id t '() clauses))
      (defconstant ,name (,name))
      (setf (table-entry *primop-table* ',id) ,name))))


;----------------------------------------------------------------
;;;; Compiler Internal Primitives

;;; from primops;base
(define-primop Y
  (primop.generate (self node)
    self
    (generate-labels node))
  (primop.simplify (self node)
    self
    (simplify-y node))
  (primop.special? t))

(define-primop conditional
  (primop.generate (self node)
     self
     (primop.generate (primop-value (call-arg-n 3 node)) node))
  (primop.conditional? t)
  (primop.simplify (self node)
     self
     (primop.simplify (primop-value (call-arg-n 3 node)) node)))

(define-primop test
  (primop.generate (self node)
    self
    (primop.generate (primop-value (call-arg-n 4 node)) node))
  (primop.presimplify (self node)
    self
    (presimplify-to-conditional node))
  (primop.simplify (self node)
    self
    (simplify-test node))
  (primop.conditional? t))

(define-primop true?
  (primop.generate (self node)
     (generate-nil-test node))
  (primop.presimplify (self node)
     (presimplify-predicate node))
  (primop.type-predicate? (primop) t)
  (primop.type (self node) '(proc (cont boolean?) top?)))

;(define-primop contents-location
;  (primop.generate (self node)
;    (generate-contents-location node)))


(define-primop setq-lexical
  (primop.side-effects? t)
  (primop.generate (self node)
    (generate-lexical-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node)))
  (primop.uses-L-value? t)
  (primop.defines-support? t)                   ;???
  (primop.support-variant (self) 'set))

(define-primop setq-special
  (primop.side-effects? t)
  (primop.generate (self node)
    (generate-special-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node)))
  (primop.uses-L-value? t)
  (primop.defines-support? t)                   ;???
  (primop.support-variant (self) 'set))

(define-primop special-ref
  (primop.generate (self node)
     (generate-special-ref
       node
       (call-arg-n 2 node))))

(define-primop function-ref
  (primop.generate (self node)
     (generate-function-ref
       node
       (call-arg-n 2 node))))


;;; Primop which generates setup code
;;; for optional argument
(define-primop optional-setup
  (primop.generate (self node)
    self
    (generate-optional-setup node))
  (primop.side-effects? t)
  (primop.special? t))

;;; this does nothing
;;; optional inits use it
(define-primop noop
  (primop.generate (self node)
    self node nil)
  (primop.side-effects? t))

;;; Primop which generates an OPEN instruction
;;; this primop is put in by make-user-call
;;; during node conversion
(define-primop open-frame
  (primop.side-effects? t)
  (primop.generate (self node)
    self
    (generate-open node)))


;---------------------------------------------------------------
;;;; Lisp Primitives

(define-primop eq
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))


;;;; Arithmatic Comparisons

(define-primop zerop
  (primop.generate (self node)
    (declare (ignore self))
    (generate-arith-predicate node 'K:BR-NOT-ZERO 'K:BW-24))
  (primop.presimplify (self node)
    (declare (ignore self))
    (presimplify-predicate node))
  (primop.conditional? t))

(define-primop minusp
  (primop.generate (self node)
    (declare (ignore self))
    (generate-arith-predicate node 'K:BR-NOT-NEGATIVE 'K:BW-24))
  (primop.presimplify (self node)
    (declare (ignore self))
    (presimplify-predicate node))
  (primop.conditional? t))

(define-primop plusp
  (primop.generate (self node)
    (declare (ignore self))
    (generate-arith-predicate node 'K:BR-NOT-POSITIVE 'K:BW-24))
  (primop.presimplify (self node)
    (declare (ignore self))
    (presimplify-predicate node))
  (primop.conditional? t))



(define-primop =
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-24))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))

(define-primop <
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-LESS-THAN 'K:BW-24))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))

(define-primop >
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-GREATER-THAN 'K:BW-24))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))

(define-primop >=
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-GREATER-OR-EQUAL 'K:BW-24))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))

(define-primop <=
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-LESS-THAN-OR-EQUAL 'K:BW-24))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))


;;;; Logical Primops

(define-primop logand
  (primop.generate (self node)
     self
     (generate-binop node 'K:AND 'K:AND 'K:BW-24)))

(define-primop logxor
  (primop.generate (self node)
     self
     (generate-binop node 'K:XOR 'K:XOR 'K:BW-24)))

(define-primop logior
  (primop.generate (self node)
     self
     (generate-binop node 'K:OR 'K:OR 'K:BW-24)))

(define-primop ash
  (primop.generate (self node)
     (declare (ignore self))
     (destructure (((cont n nbits) (call-args node)))
       (if (literal-node? nbits)
           (let ((amount (leaf-value nbits)))
             (if (< amount 0)
                 (if (> amount -6)
                     (let ((dest (continuation-expecting cont)))
                       (dotimes (i (abs amount))
                         (emit-alu 'K:SHIFT-DN-AR-R dest IGNORE (get-right-operand n) 'K:BW-24)))
                   (cerror "foo" "> 5 bit shifts not yet implemented"))
               (if (< amount 6)
                     (let ((dest (continuation-expecting cont)))
                       (dotimes (i amount)
                         (emit-alu 'K:SHIFT-UP-0F-R dest IGNORE (get-right-operand n) 'K:BW-24)))
                   (cerror "foo" "> 5 bit shifts not yet implemented"))))
         (progn
           (cerror "foo" "call ash"))))))



;;;; Arithmetic Primops

(define-primop %2-arg-+
  (primop.generate (self node)
    self
    (generate-binop node 'K:L+R 'K:L+R 'K:BW-24)))

(define-primop %2-arg--
  (primop.generate (self node)
    self
    (generate-binop node 'K:L-R 'K:R-L 'K:BW-24)))

(define-primop 1-
  (primop.generate (self node)
    self
    (generate-unop node 'K:R-1 'K:BW-24))
  (primop.type (self node) '(proc (cont fixnum?) fixnum?)))

(define-primop 1+
  (primop.generate (self node)
    self
    (generate-unop node 'K:R+1 'K:BW-24))
  (primop.type (self node) '(proc (cont fixnum?) fixnum?)))


;----------------------------------------------------------------
;;;; Hardware Primops


(define-primop hw:nop
  (primop.side-effects? t)
  (primop.generate (self node)
    (declare (ignore self node))
    (emit 'K:NOP)))

;;; compiler internal
;;; (hw:<fsource> ...)  expands into this
(define-primop read-functional-source
  (primop.side-effects? t)
  (primop.generate (self node)
    (declare (ignore self))
    (generate-move (leaf-value (call-arg-n 2 node))
                   (continuation-expecting (call-arg-n 1 node)))))

;;; compiler internal
;;; (hw:<fdest> ...) expands into this
(define-primop write-functional-dest
  (primop.side-effects? t)
  (primop.generate (self node)
    (declare (ignore self))
    (generate-move (call-arg-n 3 node)
                   (leaf-value (call-arg-n 2 node)))
    (generate-move ''nil (continuation-expecting (call-arg-n 1 node)))))


(define-primop hw:dpb
  (primop.generate (self node)
     (declare (ignore self))
     (destructure (((cont value byte-spec word) (call-args node)))
       (if (literal-node? byte-spec)
           (emit-alu-field 'K:FIELD-PASS
               (continuation-expecting cont)
               (get-left-operand value)
               (get-right-operand word)
               `',(leaf-value byte-spec)
               'K:PW-II)
         (progn
           (emit-alu 'K:LOAD-STATUS-R 'K:NOP-NO-OVERFLOW-TRAP
                     IGNORE (get-right-operand byte-spec) 'K:BW-16)
           (emit-alu-field 'K:FIELD-PASS
             (continuation-expecting cont)
             (get-left-operand value)
             (get-right-operand word)
             ''0 'K:PW-RR))))))

(define-primop hw:dpb-unboxed
  (primop.generate (self node)
     (declare (ignore self))
     (destructure (((cont value byte-spec word) (call-args node)))
       (if (literal-node? byte-spec)
           (emit-alu-field 'K:FIELD-PASS
               (continuation-expecting cont)
               (get-left-operand value)
               (get-right-operand word)
               `',(leaf-value byte-spec)
               'K:PW-II 'K:UNBOXED)
         (progn
           (emit-alu 'K:LOAD-STATUS-R 'K:NOP-NO-OVERFLOW-TRAP
                     'K:IGNORE (get-right-operand byte-spec) 'K:BW-16)
           (emit-alu-field 'K:FIELD-PASS
             (continuation-expecting cont)
             (get-left-operand value)
             (get-right-operand word)
             ''0 'K:PW-RR 'K:UNBOXED))))))

(define-primop hw:ldb
  (primop.generate (self node)
     (declare (ignore self))
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
           (emit 'K:MOVE R0 (get-right-operand byte-spec) 'K:BW-16)
           (emit-alu 'K:NEG-R R0 IGNORE R0 'K:BW-8)
           (emit-alu 'K:LOAD-STATUS-R 'K:NOP-NO-OVERFLOW-TRAP
                     IGNORE R0 'K:BW-16)
           (emit-alu-field 'K:FIELD-PASS
                           (continuation-expecting cont)
                           (get-left-operand from)
                           (get-right-operand into)
                           ''0
                           'K:PW-RR))))))


;;; Have the call hardware do a tail call operation
;;; without really calling
(define-primop hw:ch-tcall
 (primop.generate (self node)
    (declare (ignore self node))
    (emit 'K:TAIL-CALL ''0 ''0 R0 () 'K:NEXT-PC-PC+1))
 (primop.side-effects? t))

;;; Actually this is the same as eq?
(define-primop hw:32=
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))

(define-primop hw:32+
  (primop.generate (self node)
    self
    (generate-binop node 'K:L+R 'K:L+R)))


;;;; Lisp Subprimitives


(define-primop li:funcall-internal
  (primop.simplify (self node)
    (declare (ignore self))
    (simplify-funcall node))
  (primop.generate (self node)))



;;; ksi

;;; (KSI:OPEN-FRAME <nargs>)
;;;  (alu l+r+2 *arg-1* *arg-1* )
;;;  (alu l+r+2 *arg-1* *arg-1* )
;(define-primop ksi:open-frame
;  (primop.side-effects? t)
;  (primop.generate (self node)
;     (declare (ignore self))
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

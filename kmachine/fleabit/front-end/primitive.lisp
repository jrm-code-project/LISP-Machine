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

(defmacro define-initial-primop (id &body clauses)
  (let ((name (intern (format nil "PRIMOP/~s" id))))
   `(progn
      (defun ,name () ,(primop-code id t '() clauses))
      (defvar ,name (,name))
      (setf (primop-table-lookup *primitive-primop-table* ',id) ,name)
      (make-support-entry (create-variable ',id)
                          *primitive-support-table*
                          nil
                          'support
                          (node->vector (create-primop-node ,name))
                          nil))))

;;; In T primops are defined by (define-constant foo (primop ...
;;; however actually primop files get created. They get loaded by
;;; load-primop-table which is called by expression->support-table which
;;; gets the expression which mentions the primop file name from a
;;; support file??? I don't see any of those.  who makes the support
;;; entry with support variant of 'constant?????
(defmacro define-constant-primop (id &body clauses)
  (let ((name (intern (format nil "PRIMOP/~s" id))))
   `(progn
      (defun ,name () ,(primop-code id t '() clauses))
      (defvar ,name (,name))
      (setf (primop-table-lookup *primitive-primop-table* ',id) ,name)
      (make-support-entry (create-variable ',id)
                          *primitive-support-table*
                          nil
                          'constant
                          (node->vector (create-primop-node ,name))
                          nil))))


;----------------------------------------------------------------
;;;; Compiler Internal Primitives

;(define-initial-primop undefined)
;(define-initial-primop undefined-effect)
;(define-initial-primop proc+handler)

;(define-initial-primop *primop
;  (primop.simplify (self node)
;   (simplify-*primop self node)))

;(define-initial-primop *define-constant
;  (primop.side-effects?    t)
;  (primop.defines-support? t)
;  (primop.generate (self node)
;     self
;    (generate-define-var node))
;  (primop.uses-L-value?    t)
;  (primop.support-variant (self) 'constant))

;;;; from primops;base
;(define-initial-primop *define
;  (primop.side-effects?    t)
;  (primop.defines-support? t)
;  (primop.generate (self node)
;     self
;    (generate-define-var node))
;  (primop.uses-L-value?    t)
;  (primop.support-variant (self) 'define))

;;; from primops;base
(define-initial-primop Y
  (primop.generate (self node)
    self
    (generate-labels node))
  (primop.simplify (self node)
    self
    (simplify-y node))
  (primop.special? t))

(define-initial-primop conditional
  (primop.generate (self node)
     self
     (primop.generate (primop-value (call-arg-n 3 node)) node))
  (primop.conditional? t)
  (primop.simplify (self node)
     self
     (primop.simplify (primop-value (call-arg-n 3 node)) node)))

(define-initial-primop test
  (primop.generate (self node)
    self
    (destructure (((nil nil nil type arg) (call-args node)))
      (primop.test-code (primop-value type)
                        node
                        (read-acc arg))  ;(access-value node (leaf-value arg)))
      (generate-conditional node
                       (if (primop.jump-on-equal? (primop-value type))
                           'K:BR-EQUAL
                         'K:BR-NOT-EQUAL))))
    (primop.presimplify (self node)
      self
      (presimplify-to-conditional node))
    (primop.simplify (self node)
      self
      (simplify-test node))
    (primop.conditional? t))

(define-initial-primop true?
  (primop.test-code (self node arg)
     (generate-nil-test arg))
  (primop.presimplify (self node)
     (presimplify-predicate node))
  (primop.jump-on-equal?  (primop) t)       ; because we compare with nil
  (primop.type-predicate? (primop) t)
  (primop.type (self node) '(proc (cont boolean?) top?)))

;(define-initial-primop contents-location
;  (primop.generate (self node)
;    (generate-contents-location node)))


(define-initial-primop setq-lexical
  (primop.side-effects? t)
  (primop.generate (self node)
    (generate-lexical-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node)))
  (primop.uses-L-value? t)
  (primop.defines-support? t)                   ;???
  (primop.support-variant (self) 'set))

(define-initial-primop setq-special
  (primop.side-effects? t)
  (primop.generate (self node)
    (generate-special-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node)))
  (primop.uses-L-value? t)
  (primop.defines-support? t)                   ;???
  (primop.support-variant (self) 'set))

(define-initial-primop special-ref
  (primop.generate (self node)
     (generate-special-ref
       node
       (call-arg-n 2 node))))

(define-initial-primop function-ref
  (primop.generate (self node)
     (generate-function-ref
       node
       (call-arg-n 2 node))))


;;; from base
;(define-initial-primop cell-value
;  (primop.location? t)
;  (primop.location-specs (self) 0)    ;(- (* 1 4) 2)) ???
;  (primop.rep-wants (self) self 'rep/pointer)
;  (primop.simplify (self node)
;    self
;    (simplify-location node))
;  (primop.type (self node) '(proc (cont top?) cell?)))

;(define-initial-primop set-location
;  (primop.side-effects? t)
;  (primop.generate (self node)
;     self
;     (generate-set-location node)))

;(define-initial-primop make-cell
;  (primop.generate (self node)
;     (generate-make-cell node))
;  (primop.type (self node) '(proc (cont cell?) top?)))


;;; Primop which generates setup code
;;; for optional argument
(define-constant-primop optional-setup
  (primop.generate (self node)
    self
    (generate-optional-setup node))
  (primop.side-effects? t)
  (primop.special? t))

;;; this does nothing
;;; optional inits use it
(define-constant-primop noop
  (primop.generate (self node)
    self node nil)
  (primop.side-effects? t))

;;; Primop which generates an OPEN instruction
;;; this primop is put in by make-user-call
;;; during node conversion
(define-initial-primop open-frame
  (primop.side-effects? t)
  (primop.generate (self node)
    self
    (generate-open node)))


;---------------------------------------------------------------
;;;; Lisp Primitives

(define-constant-primop eq
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

(define-constant-primop =
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

(define-constant-primop <
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

(define-constant-primop >
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

(define-constant-primop >=
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

(define-constant-primop <=
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

(define-constant-primop logand
  (primop.generate (self node)
     self
     (generate-binop node 'K:ALU-AND 'K:ALU-AND 'K:BW-24)))

;;;; Arithmetic Primops

(define-constant-primop %2-arg-+
  (primop.generate (self node)
    self
    (generate-binop node 'K:L+R 'K:L+R 'K:BW-24)))

(define-constant-primop %2-arg--
  (primop.generate (self node)
    self
    (generate-binop node 'K:L-R 'K:R-L 'K:BW-24)))

(define-constant-primop 1-
  (primop.generate (self node)
    self
    (generate-unop node 'K:R-1 'K:BW-24))
  (primop.type (self node) '(proc (cont fixnum?) fixnum?)))

(define-constant-primop 1+
  (primop.generate (self node)
    self
    (generate-unop node 'K:R+1 'K:BW-24))
  (primop.type (self node) '(proc (cont fixnum?) fixnum?)))


;----------------------------------------------------------------
;;;; Hardware Primops


(define-constant-primop hw:nop
  (primop.side-effects? t)
  (primop.generate (self node)
    (declare (ignore self node))
    (emit 'K:NOP)))

;;; compiler internal
;;; (hw:<fsource> ...)  expands into this
(define-constant-primop read-functional-source
  (primop.side-effects? t)
  (primop.generate (self node)
    (declare (ignore self))
    (generate-move (leaf-value (call-arg-n 2 node))
                   (continuation-expecting (call-arg-n 1 node)))))

;;; compiler internal
;;; (hw:<fdest> ...) expands into this
(define-constant-primop write-functional-dest
  (primop.side-effects? t)
  (primop.generate (self node)
    (declare (ignore self))
    (generate-move (read-acc (call-arg-n 3 node))
                   (leaf-value (call-arg-n 2 node)))
    (generate-move ''nil (continuation-expecting (call-arg-n 1 node)))))



(define-constant-primop hw:dpb
  (primop.generate (self node)
     (declare (ignore self))
     (destructure (((cont value byte-spec word) (call-args node)))
       (if (literal-node? byte-spec)
           (emit-alu-field 'K:FIELD-PASS
             (continuation-expecting cont)
             (read-acc value)
             (read-acc word)
             `',(leaf-value byte-spec)
             'K:PW-II)
         (progn
           (emit-alu 'K:LOAD-STATUS-R 'K:NOOP-NO-OVERFLOW-TRAP
                     'K:IGNORE (read-acc byte-spec) 'K:BW-16)
           (emit-alu-field 'K:FIELD-PASS
             (continuation-expecting cont)
             (read-acc value)
             (read-acc word)
             ''0 'K:PW-RR))))))

(define-constant-primop hw:ldb
  (primop.generate (self node)
     (declare (ignore self))
     (destructure (((cont byte-spec word) (call-args node)))
       (if (literal-node? byte-spec)
           (emit-alu-field 'K:FIELD-PASS
             (continuation-expecting cont)
             (read-acc word)
             'K:R0
             (let ((byte-spec (leaf-value byte-spec)))
               `(BYTE ,(byte-size byte-spec)
                      ,(- (byte-position byte-spec))))
             'K:PW-II)
         (progn
           (emit-alu 'K:LOAD-STATUS-R 'K:NOOP-NO-OVERFLOW-TRAP
                     'K:IGNORE (read-acc byte-spec) 'K:BW-16)
           (emit-alu 'K:NEG-R '*foo* 'K:IGNORE (read-acc byte-spec) 'K:BW-8)
           (emit-alu 'K:LOAD-STATUS-R 'K:NOOP-NO-OVERFLOW-TRAP
                     'K:IGNORE '*foo* 'K:BW-8)
           (emit-alu-field 'K:FIELD-PASS
             (continuation-expecting cont)
             (read-acc word)
             'K:R0
             ''0 'K:PW-RR))))))


;;; Have the call hardware do a tail call operation
;;; without really calling
(define-constant-primop hw:ch-tcall
 (primop.generate (self node)
    (declare (ignore self node))
    (emit 'K:TAIL-CALL ''0 ''0 R0 () 'K:NEXT-PC-PC+1))
 (primop.side-effects? t))

;;; Actually this is the same as eq?
(define-constant-primop hw:32=
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


;;;; Lisp Subprimitives

(define-constant-primop k:%loc+1
  (primop.generate (self node)
    self
    (generate-unop node 'K:L+R+1)))

(define-constant-primop k:%loc-1
  (primop.generate (self node)
    self
    (generate-unop node 'K:L+R-1)))

(define-constant-primop k:%loc-+
  (primop.generate (self node)
    self
    (generate-binop node 'K:L+R)))

(define-constant-primop k:%loc-+-+1
  (primop.generate (self node)
    self
    (generate-binop node 'K:L+R+1)))

(define-constant-primop k:%loc->
 (primop.generate (self node)
   self
   (comparator node 'K:BR-NOT-GREATER-THAN))
 (primop.presimplify (self node)
   self
   (presimplify-to-conditional node))
 (primop.conditional? t)
 (primop.type (self node)
    self node
    '(proc (cont boolean?) fixnum? fixnum?)))

(define-constant-primop k:%loc-store
  (primop.side-effects?    t)
  (primop.generate (self node)
     self
     (generate-move (acc (call-arg-n 3 node))
                    'K:MD)
     (generate-move (acc (call-arg-n 2 node))
                    'K:VMA-START-WRITE)))

;;; this might do different type trapping than %store-car?
;;; (%loc-store-car <loc> <data>)
(define-constant-primop k:%loc-store-car
  (primop.side-effects?    t)
  (primop.generate (self node)
     self
     (generate-move (acc (call-arg-n 3 node))
                    'K:MD)
     (generate-move (acc (call-arg-n 2 node))
                    'K:VMA-START-WRITE)))

(define-constant-primop k:%loc-store-cdr
  (primop.side-effects?    t)
  (primop.generate (self node)
     self
     (generate-move (acc (call-arg-n 3 node))
                    'K:MD)
     (emit-alu 'K:ALU-OR 'K:VMA-START-WRITE
               (acc (call-arg-n 2 node))
               ''1)))



(define-constant-primop k:%k-make-pointer
  (primop.generate (self node)
     self
     (emit 'K:KDPB
           (continuation-expecting (call-arg-n 1 node))
           (acc (call-arg-n 2 node))
           (acc (call-arg-n 3 node))
           'K:%%PTR-DATA-TYPE)))




;;; ksi

;;; (KSI:OPEN-FRAME <nargs>)
;;;  (alu l+r+2 *arg-1* *arg-1* )
;;;  (alu l+r+2 *arg-1* *arg-1* )
;(define-constant-primop ksi:open-frame
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
;(define-constant-primop ksi:set-arg)

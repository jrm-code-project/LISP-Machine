;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Primitive Operations

;;; This file defines the primitive operations known to the compiler.
;;; Various fields and flags may be specified as follows:
;;;
;;;  :CONDITIONAL?  T if this is a predicate, ie, the value
;;;                 is a boolean and this predicate may be
;;;                 an argument to IF
;;;
;;;  :SPECIAL?      T if a continuation should not be generated
;;;                 for this primop, usually used only for
;;;                 more obscure flow of control primops
;;;
;;;  :PRESIMPLIFY   code to do some simplification before the simplification
;;;                 pass.  The major use of this is for predicates, which
;;;                 should call presimplify-to-conditional
;;;
;;;  :SIMPLIFY      code which attempts to simplify this primop
;;;
;;;  :GENERATE      code which generates the code of this primop
;;;                 unless :special? is specified, this code
;;;                 must return and accessor for the return value,
;;;                 generate-continuation will move the return value
;;;                 to the appropriate place.

;----------------------------------------------------------------
;;;; Compiler Internal Primitives

;;; from primops;base
(define-primop Y (???)
  (:generate (node)
    (generate-labels node))
  (:simplify (node)
    (simplify-y node))
  (:special? t))

(define-primop %go (tag tagbody-cont)
  (:generate (node)
    (destructure (((tagbody-cont tag) (call-args node)))
      (generate-go tagbody-cont tag)))
  (:special? t))

(define-primop conditional (test-primop &rest args)
  (:generate (node)
      (primop.generate (primop-value (call-arg-n 3 node)) node))
  (:conditional? t)
  (:simplify (node)
      (primop.simplify (primop-value (call-arg-n 3 node)) node)))

;(define-primop test (???)
;  (:generate (node)
;    (primop.generate (primop-value (call-arg-n 4 node)) node))
;  (:presimplify (node)
;    (presimplify-to-conditional node))
;  (:simplify (node)
;    (simplify-test node))
;  (:conditional? t))

(define-primop true? (value)
  (:generate (node)
     (generate-nil-test node))
  (:presimplify (node)
     (presimplify-to-conditional node))
  (:simplify (node)
     (simplify-test node))
  (:type-predicate? (primop) t)
  (:type (node) '((t) boolean))
  )

(define-primop setq-lexical (var value)
  (:side-effects? t)
  (:generate (node)
    (generate-lexical-setq
      node
      (call-arg-n 2 node)
      (call-arg-n 3 node))))

;(define-primop setq-special (symbol value)
;  (:side-effects? t)
;  (:generate (node)
;    (generate-special-setq
;      node
;      (call-arg-n 2 node)
;      (call-arg-n 3 node))))

;(define-primop special-ref (symbol)
;  (:generate (node)
;     (generate-special-ref
;       node
;       (call-arg-n 2 node))))

;(define-primop function-ref (symbol)
;  (:generate (node)
;     (generate-function-ref
;       node
;       (call-arg-n 2 node))))

(define-primop initialize-global (global value)
  (:side-effects? t)
  (:generate (node)
    (destructure (((nil global value) (call-args node)))
      (if (and (literal-node? global)
               (literal-node? value))
          (emit-movei value
                      (literal-value global))
        (bug "INITIALIZE-GLOBAL called with bad args"))
      ''NIL)))

(define-primop %cons-rest (var)
  (:side-effects? t)
  (:generate (node)
     (generate-cons-rest node)))

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


;;; Primitives used by CATCH and THROW

(define-primop %catch-open (cont tag)
  (:generate (node)
     (generate-catch-open node)))

(define-primop %catch-body-values (&rest values)
  (:special? t)
  (:generate (node)
     (generate-catch-body-values node)))

(define-primop %catch-continue ()
  (:side-effects? t)
  (:special? t)
  (:generate (node)
     (generate-catch-continue node)))

(define-primop %throw-internal (tag result)
  (:side-effects? t)
  (:special? t)
  (:generate (node) (generate-throw-internal node)))


;;; dispatch to labels
(define-primop %dispatch (<procs> <values>)
  ;; just to be careful
  (:side-effects? t)
  ;; no continuation for this luser
  ;; LUSER?  this wins.
  (:special? t)
  (:generate (node)
     (let ((cont (car (call-args node)))
           (otherwise (second (call-exit-args node)))
           (procs (cddr (call-exit-args node))))
       (destructure (((byte-spec word . values)
                      (call-non-exit-args node)))
         (if (literal-node? byte-spec)
           (setq byte-spec (literal-value byte-spec))
           (warn "The byte spec: ~s, is unknown at compile time."))
         (let ((field-size (expt 2 (prims::byte-size byte-spec)))
               (clause-list '()))
           (do ((procs procs (cdr procs)))
               ((null procs))
             (lambda-queue (car procs))
             (let ((keys '()))
               (dotimes (i (literal-value (pop values)))
                 (let ((value (literal-value (pop values))))
                   (when (> value field-size)
                     (warn "The DISPATCH clause test value ~A is not a possible value of the field (BYTE ~a ~a)."
                           value (prims::byte-size byte-spec) (prims::byte-position byte-spec)))
                   (push value keys)))
               (push (cons keys (car procs)) clause-list)))
           (lambda-queue otherwise)
           (emit-alu-field 'K:FIELD-EXTRACT-R R0 IGNORED (get-right-operand word)
                           `(PRIMS::BYTE ,(prims::byte-size byte-spec) ,(- (prims::byte-position byte-spec)))
                           'K:PW-II 'K:UNBOXED)
           (emit-alu 'K:L+R 'K:R1 'GR::*TRAP-DTP-CODE-5* 'K:TRAP-PC+ '(K:BW-32 K:BOXED))
           (emit-alu 'K:L+R 'k:NOP 'K:R0 'K:R1 '(K:BW-32 K:BOXED))
           (emit 'K:NOP)
           (emit 'K:NOP 'K:NEXT-PC-DISPATCH)
           (dotimes (i field-size)
             (let ((clause (assoc i clause-list :test #'member)))
             (emit-unconditional-branch (if clause (cdr clause) otherwise) ))))))))



;---------------------------------------------------------------
;;;; Lisp Primitives

;(deftype boolean () t)
(deftype boolean () '(or t nil))

(define-primop eq (x y)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
   (simplify-if-constant-predicate node 'eq))
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32))
 (:conditional? t)
 (:type (node)
    '((t t) boolean)))

;;; EQL is just like EQ except it traps if both sides are
;;; an extended numbers of the same type.
(define-primop eql (x y)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
   (simplify-if-constant-predicate node 'eql))
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32 'K:DT-HAIRY-NUMBER))
 (:conditional? t)
 (:type (node)
    '((t t) boolean)))


;;;; Arithmatic Comparisons

(define-primop zerop (number)
  (:presimplify (node)
    (presimplify-to-conditional node))  ;predicate
  (:simplify (node)
    (simplify-if-constant-predicate node 'zerop))
  (:generate (node)
    (generate-fixnum-arith-predicate node 'K:BR-NOT-ZERO))
  (:conditional? t)
  (:type (node)
    '((number) boolean)))

(define-primop minusp (number)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
    (simplify-if-constant-predicate node 'minusp))
  (:generate (node)
    (generate-fixnum-arith-predicate node 'K:BR-NOT-NEGATIVE))
  (:conditional? t)
  (:type (node)
    '((number) boolean)))

(define-primop plusp (number)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
    (simplify-if-constant-predicate node 'plusp))
  (:generate (node)
    (generate-fixnum-arith-predicate node 'K:BR-NOT-POSITIVE))
  (:conditional? t)
  (:type (node)
    '((number) boolean)))

(define-primop 2-arg-= (number1 number2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node '=))
 (:generate (node)
   ;; This does not use subtract:
   ;;  (comparator node 'K:BR-NOT-EQUAL
   ;;       'K:BW-24 'K:DT-BOTH-FIXNUM)
   ;; so that we can tell the difference between = and < etc
   ;; for complex numbers
   (let ((left (call-arg-n 4 node))
         (right (call-arg-n 5 node)))
     (multiple-value-bind (l r) (get-operands left right)
       (emit-alu 'K:XOR 'K:NOP l r '(K:BW-24 K:DT-BOTH-FIXNUM)))
     (generate-conditional node 'K:BR-NOT-EQUAL)))
 (:conditional? t)
 (:type (node)
    '((number number) boolean)))

(define-primop 2-arg-< (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node '<))
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-THAN
               'K:BW-24 'K:DT-BOTH-FIXNUM))
 (:conditional? t)
 (:type (node)
    '((number number) boolean)))

(define-primop 2-arg-> (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node '>))
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-THAN
               'K:BW-24 'K:DT-BOTH-FIXNUM))
 (:conditional? t)
 (:type (node)
    '((number number) boolean)))

(define-primop 2-arg->= (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
   (simplify-if-constant-predicate node '>=))
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-OR-EQUAL
               'K:BW-24 'K:DT-BOTH-FIXNUM))
 (:conditional? t)
 (:type (node)
    '((number number) boolean)))

(define-primop 2-arg-<= (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
   (simplify-if-constant-predicate node '<=))
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-OR-EQUAL
               'K:BW-24 'K:DT-BOTH-FIXNUM))
 (:conditional? t)
 (:type (node)
    '((number number) boolean)))


;;;; Logical Primops

(define-primop 2-arg-logand (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logand))
  (:generate (node)
      (generate-binop node 'K:AND 'K:AND
                      'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
  (:type (node)
     '((integer integer) integer)))

(define-primop 2-arg-logxor (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logxor))
  (:generate (node)
      (generate-binop node 'K:XOR 'K:XOR
                      'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
  (:type (node)
     '((integer integer) integer)))

(define-primop 2-arg-logxnor (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logxor))
  (:generate (node)
      (generate-binop node 'K:XNOR 'K:XNOR
                      'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
  (:type (node)
     '((integer integer) integer)))

(define-primop 2-arg-logior (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logior))
  (:generate (node)
      (generate-binop node 'K:OR 'K:OR
                      'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
  (:type (node)
     '((integer integer) integer)))

(define-primop ash (n nbits)
  (:simplify (node)
      (simplify-if-constant-expression node 'ash))
  (:generate (node)
    (destructure (((cont n nbits) (call-args node)))
      (let ((dest (get-destination cont))
            (num (get-right-operand n)))
        (let ((left (get-left-side-for-fixnum-unop num)))
          (if (literal-node? nbits)
              (let ((amount (leaf-value nbits)))
                (cond
                  ((not (integerp amount))
                   (warn "Shift amount to ASH ~a is not an integer." amount))
                  ((= amount 1)
                   (emit-alu 'K:SHIFT-UP-0F-R dest left num
                             '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW)))
                  ((= amount 2)
                   (emit-alu 'K:SHIFT-UP-0F-R R0 left num
                             '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
                   (emit-alu 'K:SHIFT-UP-0F-R dest R0 R0
                             '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW)))
                  ((= amount -1)
                   (emit-alu 'K:SHIFT-DN-AR-R dest left num
                             '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW)))
                  ((= amount -2)
                   (emit-alu 'K:SHIFT-DN-AR-R R0 left num
                             '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW))
                   (emit-alu 'K:SHIFT-DN-AR-R dest R0 R0
                             '(K:BW-24 K:BOXED K:DT-BOTH-FIXNUM-WITH-OVERFLOW)))
                                                ; ((minusp amount) shift-down)
                                                ; ((plusp amount) shift-up)
                  ((zerop amount) (generate-move num dest))
                  (t (generate-internal-call 'NEW-MATH:ASH-GENERIC dest num nbits)))) ;;@@@ Do at compile time instead
            (generate-internal-call 'NEW-MATH:ASH-GENERIC dest num nbits)))
        dest)))
  (:type (node)
     '((integer integer) integer)))

(define-primop dpb (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'nlisp:dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (if (and (literal-node? byte-spec)
                 (let ((bs (literal-value byte-spec)))
                   (< (+ (prims::byte-position bs) (prims::byte-size bs))
                      23.)))
            (generate-dpb cont value byte-spec word 'K:DT-BOTH-FIXNUM)
          (let ((dest (get-destination cont)))
            (generate-internal-call 'NEW-MATH:FIELD-PASS-GENERIC dest value byte-spec word)
            dest)))))



(define-primop prims::byte (size position)
  (:simplify (node)
      (simplify-if-constant-expression node 'prims:byte))
  (:generate (node)
     (destructure (((cont size position) (call-args node)))
       ;; `(hw:dpb ,size vinc:%%byte-size ,position)
       ;; this isn't correct.  It is more like
       ;; `(hw:dpb ,size vinc:%%byte-size (hw:ldb ,position vinc:%%byte-position 0.))
       (emit-alu-field 'K:FIELD-PASS R1 (get-left-operand position) 'GR::*ZERO*
                       `(PRIMS::BYTE ,(prims::byte-size vinc:%%byte-position)
                                     ,(- (prims::byte-position vinc:%%byte-position)))
                       'K::DT-BOTH-FIXNUM 'K:BOXED-RIGHT 'K:PW-II)
       (generate-dpb cont size `',vinc:%%byte-size R1 'K:DT-BOTH-FIXNUM))))

(define-primop prims::byte-size (byte)
  (:simplify (node)
    (simplify-if-constant-expression node 'prims:byte-size))
  (:generate (node)
    (destructure (((cont byte) (call-args node)))
      (generate-ldb cont byte `',vinc:%%byte-size 0 'k:dt-both-fixnum))))

(define-primop prims::byte-position (byte)
  (:simplify (node)
    (simplify-if-constant-expression node 'prims:byte-position))
  (:generate (node)
    (destructure (((cont byte) (call-args node)))
      (generate-ldb cont byte `',vinc:%%byte-position 0 'k:dt-both-fixnum))))


;;;; Arithmetic Primops

(define-primop 2-arg-+ (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node '+))
  (:generate (node)
      (let ((arg1 (call-arg-n 2 node))
            (arg2 (call-arg-n 3 node)))
        (multiple-value-bind (literal non-literal)
            (if (literal-node? arg1) (values arg1 arg2)
              (if (literal-node? arg2) (values arg2 arg1)))
          (if literal
              (case (literal-value literal)
                (0 non-literal)
                (1 (generate-degenerate-fixnum-binop node non-literal 'K:R+1))
                (2 (generate-degenerate-fixnum-binop node non-literal 'K:R+2))
                (4 (generate-degenerate-fixnum-binop node non-literal 'K:R+4))
                (t (generate-binop node 'K:L+R 'K:L+R
                      'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW)))
            (generate-binop node 'K:L+R 'K:L+R
                      'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW)))))
  (:type (node)
      '((number number) number)))

(define-primop 2-arg-- (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node '-))
  (:generate (node)
      (let ((arg2 (call-arg-n 3 node)))
          (if (literal-node? arg2)
              (let ((arg1 (call-arg-n 2 node)))
                (case (literal-value arg2)
                  (0 arg1)
                  (1 (generate-degenerate-fixnum-binop node arg1 'K:R-1))
                  (2 (generate-degenerate-fixnum-binop node arg1 'K:R-2))
                  (4 (generate-degenerate-fixnum-binop node arg1 'K:R-4))
                  (t (generate-binop node 'K:L-R 'K:R-L
                        'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW))))
            (generate-binop node 'K:L-R 'K:R-L
               'K:BW-24 'K:BOXED 'K:DT-BOTH-FIXNUM-WITH-OVERFLOW))))
  (:type (node)
      '((number number) number)))

(define-primop 1+ (n)
  (:simplify (node)
      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-fixnum-unop node 'K:R+1))
  (:type (node) '((number) number)))

(define-primop 1- (n)
  (:simplify (node)
      (simplify-if-constant-expression node '1-))
  (:generate (node)
    (generate-fixnum-unop node 'K:R-1))
  (:type (node) '((number) number)))


;----------------------------------------------------------------

;;;; Lisp Subprimitives


(define-primop funcall-internal (f &rest args)
  (:side-effects? t)
  (:simplify (node)
    (simplify-funcall node))
  (:generate (node)
    (destructure (((cont fcn . args) (call-args node)))
      (generate-move fcn 'GR:*ARG-2*)
      (generate-move `',(length args) 'GR:*ARG-1*)
      (let ((dest (get-destination cont)))
        (apply #'generate-internal-call 'LI:FUNCALL-INTERNAL dest args)
        dest))))

(define-primop apply-internal (f &rest args)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont fcn . args) (call-args node)))
      (generate-move fcn 'GR:*ARG-2*)
      (let ((spread-args (butlast args))
            (consed-args (car (last args))))
        (generate-move `',(length spread-args) 'GR:*ARG-1*)
        (generate-move consed-args 'GR:*VALUE-1*)
        (let ((dest (get-destination cont)))
          (apply #'generate-internal-call 'LI:APPLY-INTERNAL dest spread-args)
          dest)))))





(define-primop cons::%car (list)
  (:generate (node)
     (generate-move (call-arg-n 2 node)
                    '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD)
                    'K:DT-RIGHT-LIST)
     (emit 'K:MEMORY-WAIT)
     'K:MD))

(define-primop cons::%cdr (list)
  (:generate (node)
     (generate-move (call-arg-n 2 node)
                    '(K:VMA-START-READ-CDR K:BOXED-VMA K:BOXED-MD)
                    'K:DT-RIGHT-LIST)
     (emit 'K:MEMORY-WAIT)
     'K:MD))

(NOTE "%set-car and %set-cdr use DT-RIGHT-LIST, but shouldn't allow NIL")

(define-primop cons::%set-car (ptr new-value return-value)
  (:side-effects? t)
  (:generate (node)
     (generate-gc-safe-memory-write (call-arg-n 2 node)
                                    (call-arg-n 3 node)
                                    (call-arg-n 4 node)
                                    '(K:VMA-START-READ-WILL-WRITE K:BOXED-VMA K:BOXED-MD)
                                    'K:DT-RIGHT-LIST)))


(define-primop cons::%set-cdr (ptr new-value return-value)
  (:side-effects? t)
  (:generate (node)
     (generate-gc-safe-memory-write (call-arg-n 2 node)
                                    (call-arg-n 3 node)
                                    (call-arg-n 4 node)
                                    '(K:VMA-START-READ-CDR-WILL-WRITE K:BOXED-VMA K:BOXED-MD)
                                    'K:DT-RIGHT-LIST)))

(define-primop cons::%store-contents (ptr new-value)
  (:side-effects? t)
  (:generate (node)
     (generate-gc-safe-memory-write (call-arg-n 2 node)
                                    (call-arg-n 3 node)
                                    (call-arg-n 3 node)
                                    '(K:VMA-START-READ-WILL-WRITE K:BOXED-VMA K:BOXED-MD)
                                    'K:DT-NONE)))

(define-primop cons::%store-contents-offset (ptr offset new-value)
  (:side-effects? t)
  (:generate (node)
     (generate-gc-safe-memory-write (call-arg-n 2 node)
                                    (call-arg-n 4 node)
                                    (call-arg-n 4 node)
                                    '(K:VMA-START-READ-WILL-WRITE K:UNBOXED-VMA K:BOXED-MD)
                                    'K:DT-NONE
                                    (call-arg-n 3 node))))






(define-primop array::start-array-header-reference (array)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont array) (call-args node)))
      ;; data type traps don't work on immediates because
      ;; they look like unboxed (the box bits are for the result, not the source)
      (if (literal-p array)
          (if (arrayp (literal-value array))
              (generate-move array '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD))
            (progn
              (warn "AREF or ASET of the constant ~S which is not an array." (literal-value array))
              ;; arrange to trap at run time
              (generate-move array R0)
              (emit-alu 'K:SETR '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD)
                        'GR:*RANDOM-STRUCTURE* R0 '(K:DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE))))
        (multiple-value-bind (left right) (get-operands 'gr:*random-structure* array)
          (emit-alu 'K:SETR '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD)
                    left right '(K:DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE))))
      ''NIL)))


(define-primop li:%char= (number1 number2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node 'char=))
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL
               'K:BW-24 'K:DT-BOTH-CHARACTER))
 (:conditional? t)
 (:type (node)
    '((character character) boolean)))

(define-primop li:%char/= (number1 number2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node 'char/=))
 (:generate (node)
   (comparator node 'K:BR-EQUAL
               'K:BW-24 'K:DT-BOTH-CHARACTER))
 (:conditional? t)
 (:type (node)
    '((character character) boolean)))

(define-primop li:%char< (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node 'char<))
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-THAN
               'K:BW-24 'K:DT-BOTH-CHARACTER))
 (:conditional? t)
 (:type (node)
    '((character character) boolean)))

(define-primop li:%char> (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
    (simplify-if-constant-predicate node 'char>))
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-THAN
               'K:BW-24 'K:DT-BOTH-CHARACTER))
 (:conditional? t)
 (:type (node)
    '((character character) boolean)))

(define-primop li:%char>= (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
   (simplify-if-constant-predicate node 'char>=))
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-OR-EQUAL
               'K:BW-24 'K:DT-BOTH-CHARACTER))
 (:conditional? t)
 (:type (node)
    '((character character) boolean)))

(define-primop li:%char<= (n1 n2)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:simplify (node)
   (simplify-if-constant-predicate node 'char<=))
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-OR-EQUAL
               'K:BW-24 'K:DT-BOTH-CHARACTER))
 (:conditional? t)
 (:type (node)
    '((character character) boolean)))

(define-primop li:%char-equal (char1 char2)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
             (simplify-if-constant-predicate node 'char-equal))
  (:generate (node)
    (comparator node 'K:BR-NOT-EQUAL 'K:BW-8 'K:DT-BOTH-CHARACTER)))   ;DT-NOT-BOTH-CHAR

(define-primop li:%char-not-equal (char1 char2)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
             (simplify-if-constant-predicate node 'char-not-equal))
  (:generate (node)
    (comparator node 'K:BR-EQUAL 'K:BW-8 'K:DT-BOTH-CHARACTER)))   ;DT-NOT-BOTH-CHAR

(define-primop li:%char-lessp (char1 char2)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
    (simplify-if-constant-predicate node 'char-lessp))
  (:generate (node)
    (comparator node 'K:BR-NOT-LESS-THAN 'K:BW-8 'K:DT-BOTH-CHARACTER)))   ;DT-NOT-BOTH-CHAR

(define-primop li:%char-not-lessp (char1 char2)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
             (simplify-if-constant-predicate node 'char-not-lessp))
  (:generate (node)
    (comparator node 'K:BR-LESS-THAN 'K:BW-8 'K:DT-BOTH-CHARACTER)))   ;DT-NOT-BOTH-CHAR

(define-primop li:%char-greaterp (char1 char2)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
             (simplify-if-constant-predicate node 'char-greaterp))
  (:generate (node)
    (comparator node 'K:BR-NOT-GREATER-THAN 'K:BW-8 'K:DT-BOTH-CHARACTER)))   ;DT-NOT-BOTH-CHAR

(define-primop li:%char-not-greaterp (char1 char2)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:simplify (node)
             (simplify-if-constant-predicate node 'char-not-greaterp))
  (:generate (node)
    (comparator node 'K:BR-GREATER-THAN 'K:BW-8 'K:DT-BOTH-CHARACTER)))   ;DT-NOT-BOTH-CHAR


(define-primop li:%trap-if-not-character (char)
  (:side-effects? t)
  (:simplify (node)
     (let ((arg (call-arg-n 2 node)))
       (if (and (literal-node? arg)
                (characterp (literal-value arg)))
           (simplify-if-constant-expression node 'characterp))))
  (:generate (node)
      (let ((left (get-left-operand (call-arg-n 2 node))))
        (emit 'K:ALU 'K:PASS-STATUS 'K:NOP left left
              'K:DT-BOTH-CHARACTER 'K:UNBOXED)
        ''NIL)))


(define-primop li:%trap-if-not-both-fixnum (fixnum1 fixnum2)
  (:side-effects? t)
  (:simplify (node)
     (let ((arg1 (call-arg-n 2 node))
           (arg2 (call-arg-n 3 node)))
       (if (or (and (literal-node? arg1)
                    (si:fixnump (literal-value arg1)))
               (and (literal-node? arg2)
                    (si:fixnump (literal-value arg2))))
           (simplify-if-constant-expression node 'si:fixnump))))
  (:generate (node)
      (let ((left (get-left-operand (call-arg-n 2 node)))
            (right (get-right-operand (call-arg-n 3 node))))
        (emit 'K:ALU 'K:SEX-R 'K:NOP left right
              'K:DT-BOTH-FIXNUM 'K:UNBOXED)
        ''NIL)))

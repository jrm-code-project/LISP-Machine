;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Hardware Primops

;;; See the file PRIMITIVES

(define-primop hw:nop ()
  (:side-effects? t)
  (:generate (node)
    (declare (ignore node))
    (emit 'K:NOP)
    ''NIL))

(define-primop hw:memory-wait ()
  (:side-effects? t)
  (:generate (node)
    (declare (ignore node))
    (emit 'K:MEMORY-WAIT)
    ''NIL))

;;; compiler internal
;;; (hw:<fsource> ...)  expands into this
(define-primop read-functional-source (fsource)
  (:generate (node)
    (leaf-value (call-arg-n 2 node))))

;;; Magic functional source
(define-primop hw:read-md ()
  (:side-effects? t)
  (:generate (node)
     (let ((dest (get-destination (call-arg-n 1 node))))
       (if (eql dest IGNORED)
           (progn
             (emit 'K:MOVE dest 'K:MD)
             dest)
           'K:MD))))

(define-primop hw:trap-off ()
  (:side-effects? t)
  (:generate (node)
     (let ((dest (get-destination (call-arg-n 1 node))))
       (if (eql dest IGNORED)
           (progn
             (emit 'K:MOVE dest 'K:TRAP-OFF)
             dest)
           'K:TRAP-OFF))))

;;; compiler internal
;;; (hw:<fdest> ...) expands into this
(define-primop write-functional-dest (fdest value)
  (:side-effects? t)
  (:generate (node)
    (let ((fdest (leaf-value (call-arg-n 2 node)))
          (value (call-arg-n 3 node)))
      (generate-move value fdest)
      (if (register-p fdest)
          fdest
        ''NIL))))


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
      ref)))

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
      ref)))



;;; This does not use write-functional-dest because
;;; it has to be done twice and a temp holding the value
;;; could get allocated to TRANSPORTER-RAM then the second
;;; mofe would not get generated.
(define-primop hw:write-transporter-ram (value)
  (:side-effects? t)
  (:generate (node)
    (let ((ref (get-right-operand (call-arg-n 2 node))))
      (emit 'K:MOVE 'K:TRANSPORTER-RAM ref 'K:UNBOXED)
      (emit 'K:MOVE 'K:TRANSPORTER-RAM ref 'K:UNBOXED)
      ref)))


(define-primop hw:dpb (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'nlisp::dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word))))

(define-primop hw:dpb-xor (value byte-spec word)
;; This is broken.
;  (:simplify (node)
;    (simplify-if-constant-expression node 'nlisp::dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word 'K:DT-NONE 'K:BOXED-RIGHT 'K:FIELD-XOR))))

(define-primop hw:dpb-ior (value byte-spec word)
;; This is broken.
;  (:simplify (node)
;    (simplify-if-constant-expression node 'nlisp::dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word 'K:DT-NONE 'K:BOXED-RIGHT 'K:FIELD-OR))))

(define-primop hw:dpb-aligned (value byte-spec word)
;; This is broken.
;  (:simplify (node)
;    (simplify-if-constant-expression node 'nlisp::dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word 'K:DT-NONE 'K:BOXED-RIGHT 'K:ALIGNED-FIELD-PASS-LEFT))))

(define-primop hw:dpb-and-unboxed (value byte-spec word)
;; This is broken.
;  (:simplify (node)
;    (simplify-if-constant-expression node 'nlisp::dpb))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
        (generate-dpb cont value byte-spec word 'K:DT-NONE 'K:UNBOXED 'K:FIELD-AND))))


;;; This is some stuff to do compile time arithmetic
;;; on unboxed constants

(defun get-integer (n)
  (cond ((integerp n) n)
        ((and (consp n)
              (eq (car n) 'hw:unboxed-constant))
         (cadr n))
        (t (error "Some primitive was given a non integer: ~s" n))))

(defmacro unboxed-fcn (fcn &rest args)
  ``(hw:unboxed-constant
      ,(,fcn . ,(mapcar #'(lambda (arg) `(GET-INTEGER ,arg))
                            args))))

(defun hw-dpb-unboxed (value byte-spec word)
  `(hw:unboxed-constant
     ,(nlisp:dpb (get-integer value) byte-spec (get-integer word))))

(define-primop hw:dpb-unboxed (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'hw-dpb-unboxed))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
       (generate-dpb cont value byte-spec word 'K:DT-NONE 'K:UNBOXED))))

(defun hw-dpb-boxed (from byte-spec into)
  `(hw:boxed-constant
     ,(nlisp::dpb from byte-spec into)))

(define-primop hw::dpb-boxed (value byte-spec word)
  (:simplify (node)
    (simplify-if-constant-expression node 'hw-dpb-boxed))
  (:generate (node)
      (destructure (((cont value byte-spec word) (call-args node)))
       (generate-dpb cont value byte-spec word 'K:DT-NONE 'K:BOXED))))

(defun hw-ldb (from byte-spec into)
  (nlisp::dpb (nlisp::ldb byte-spec from) (prims::byte (prims::byte-size byte-spec) 0) into))

(define-primop hw:ldb (from byte-spec into)
  (:simplify (node)
    (simplify-if-constant-expression node 'hw-ldb))
  (:generate (node)
      (destructure (((cont from byte-spec into) (call-args node)))
        (generate-ldb cont from byte-spec into))))

(defun hw-ldb-boxed (from byte-spec into)
  `(hw:boxed-constant
     ,(hw-ldb from byte-spec into)))

(define-primop hw:ldb-boxed (from byte-spec into)
  (:simplify (node)
    (simplify-if-constant-expression node 'hw-ldb-boxed))
  (:generate (node)
      (destructure (((cont from byte-spec into) (call-args node)))
        (generate-ldb cont from byte-spec into 'K:DT-NONE 'K:BOXED))))

(defun hw-ldb-not (from byte-spec into)
 (nlisp::dpb (lognot (nlisp::ldb byte-spec from)) (prims::byte (prims::byte-size byte-spec) 0) into))

(define-primop hw::ldb-not (from byte-spec into)
  (:simplify (node)
    (simplify-if-constant-expression node 'hw-ldb-not))
  (:generate (node)
      (destructure (((cont from byte-spec into) (call-args node)))
        (generate-ldb cont from byte-spec into 'K:DT-NONE 'K:BOXED-RIGHT 'K:FIELD-NOT))))

(define-primop hw::field-extract-64 (from-hi from-lo byte-spec)
  (:generate (node)
      (destructure (((cont from-hi from-lo byte-spec) (call-args node)))
        (generate-ldb cont from-hi byte-spec from-lo 'K:DT-NONE 'K:UNBOXED 'K:FIELD-EXTRACT-LR))))

(define-primop hw::merge-24 (left-high-byte right-low-24) ;;||| Written by 10/13/88 --wkf
  ;;Useful for making fixnums from unboxed 24 data; Sets Zero and Negative bits of alu-status accordingly.
  (:generate (node)
     (generate-binop node 'K:MERGE-R 'K:MERGE-R 'K:BW-24 'K:BOXED)))

(define-primop hw:32logbitp (index n)
  (:conditional? t)
  (:presimplify (node)
     (presimplify-to-conditional node))
  (:generate (node)
     (let ((index (call-arg-n 4 node))
           (n     (call-arg-n 5 node)))
       (if (literal-node? index)
           (emit-alu-field 'K:EXTRACT-BIT-RIGHT 'K:NOP
                           IGNORED (get-right-operand n)
                           `(PRIMS::BYTE 0 ,(leaf-value index))
                           'K:PW-II)
         (progn
           (generate-load-sr-position index)
           (emit-alu-field 'K:EXTRACT-BIT-RIGHT 'K:NOP
                           IGNORED (get-right-operand n)
                           ''0
                           'K:PW-RR)))
       (generate-conditional node 'K:BR-ZERO))))

(define-primop hw::alu-status-logbitp (index)
  (:conditional? t)
  (:presimplify (node)
     (presimplify-to-conditional node))
  (:generate (node)
     (let ((index (call-arg-n 4 node)))
       (if (literal-node? index)
           (emit-alu-field 'K:EXTRACT-BIT-STATUS 'K:NOP
                           IGNORED 'GR::*ZERO*
                           `(PRIMS::BYTE 0 ,(leaf-value index))
                           'K:PW-II)
         (progn
           (error "Argh, logbitp non constant from status.  Fix me.")))
;          (emit-alu 'K:LOAD-STATUS-R 'K:NOP
;                    IGNORED (get-right-operand index) '(K:BW-8))
;          (emit-alu-field 'K:EXTRACT-BIT-RIGHT 'K:NOP
;                          IGNORED (get-right-operand n)
;                          ''0
;                          'K:PW-RR)))
       (generate-conditional node 'K:BR-ZERO))))

(define-primop hw::32set-bit (index word)
  (:generate (node)
     (destructure (((cont index word) (call-args node)))
       ;; not good enough
       (let ((dest (get-destination cont)))
         (if (literal-node? index)
             (emit-alu-field 'K:SET-BIT-RIGHT dest
                             IGNORED (get-right-operand word)
                             `(PRIMS::BYTE 0 ,(leaf-value index))
                             'K:PW-II)
           (progn
             (generate-load-sr-position index)
             (emit-alu-field 'K:SET-BIT-RIGHT dest
                             IGNORED (get-right-operand word)
                             ''0
                             'K:PW-RR)))
         dest))))

(define-primop hw::32clear-bit (index word)
  (:generate (node)
     (destructure (((cont index word) (call-args node)))
       ;; not good enough
       (let ((dest (get-destination cont)))
         (if (literal-node? index)
             (emit-alu-field 'K:RESET-BIT-RIGHT dest
                             IGNORED (get-right-operand word)
                             `(PRIMS::BYTE 0 ,(leaf-value index))
                             'K:PW-II)
           (progn
             (generate-load-sr-position index)
             (emit-alu-field 'K:RESET-BIT-RIGHT dest
                             IGNORED (get-right-operand word)
                             ''0
                             'K:PW-RR)))
         dest))))

(define-primop hw:field-extract (from byte-spec)
  (:generate (node)
    (destructure (((cont from byte-spec) (call-args node)))
      (multiple-value-bind (dest right) (get-dest-and-right-operand cont from)
        (if (literal-node? byte-spec)
            (emit-alu-field 'K:FIELD-extract-r dest
                            IGNORED right
                            (let ((byte-spec (leaf-value byte-spec)))
                              `(PRIMS:BYTE ,(prims:byte-size byte-spec)
                                           ,(- (prims:byte-position byte-spec))))
                            'K:PW-II)
          (error "non constant field-extract not yet implemented"))
        dest))))

;;; Do a tail open
(define-primop hw:ch-topen ()
  (:side-effects? t)
  (:generate (node)
     (declare (ignore node))
     (emit 'K:TAIL-OPEN)
     ''NIL))

;;; Have the call hardware do a tail call operation
;;; without really calling
(define-primop hw:ch-tcall ()
 (:side-effects? t)
 (:generate (node)
    (declare (ignore node))
    (emit 'K:TAIL-CALL '(0 0) 'K:NEXT-PC-PC+1)
    ''NIL))

(define-primop hw::ch-return ()
  (:side-effects? t)
  (:generate (node)
     (declare (ignore node))
     (emit 'K:NOP 'K:CH-RETURN)
     ''NIL))

(define-primop hw::ch-return-one-value ()
  (:side-effects? t)
  (:generate (node)
    (declare (ignore node))
    (emit 'K:MOVE 'K::RETURN 'K:R0 'K:CH-RETURN)
    ''NIL))

(define-primop hw::ch-return-multiple-values ()
  (:side-effects? t)
  (:generate (node)
    (declare (ignore node))
    (emit 'K:MOVE 'K:RETURN-MV 'K:R0 'K:CH-RETURN)
    ''NIL))

(define-primop hw::ch-topen-call ()
  (:side-effects? t)
  (:generate (node)
    (declare (ignore node))
    (emit 'K:TAIL-OPEN-CALL '(0 0) nil 'K:NEXT-PC-PC+1)
    ''NIL))

;;; Have the call hardware do an open-call operation
;;; without transferring control.
(define-primop hw::ch-open-call (&optional dest)
  (:side-effects? t)
  (:generate (node)
    (let ((call-dest (call-arg-n 2 node)))
    (emit 'K:OPEN-CALL '(0 0) (if call-dest
                                  (get-left-operand call-dest)
                                  IGNORED)
          NIL 'K:NEXT-PC-PC+1)
    ''NIL)))

(define-primop hw::open-frame ()
  (:side-effects? t)
  (:generate (node)
     (declare (ignore node))
     (emit 'K:OPEN)
     ''NIL))

(define-primop hw::call (fcn nargs)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont fcn nargs) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit 'K:CALL (list (literal-value fcn)
                           (literal-value nargs))
          dest)
        dest))))

;;; dispatch to a computed value
(define-primop hw:dispatch (pc)
  (:side-effects? t)
  (:generate (node)
    (generate-move (call-arg-n 2 node) 'K:NOP)
    (emit 'K:NOP)
    (emit 'K:NOP 'K:NEXT-PC-DISPATCH)
    ''NIL))


(define-primop hw:tail-call-dispatch (pc)
  (:side-effects? t)
  (:generate (node)
    (generate-move (call-arg-n 2 node) 'K:NOP)
    (emit 'K:NOP)
    (emit 'K:NOP 'K:NEXT-PC-DISPATCH 'K:CH-TAIL-CALL)
    ''NIL))

(define-primop hw:jump (fcn)
  (:side-effects? t)
  (:special? t) ;no continuation
  (:generate (node)
     (emit 'K:JUMP (leaf-value (call-arg-n 2 node)))))

;;; This is used to implement a simple subroutine call mechanism
;;; without changing the call hardware.
;;; BUG:  The compiler doesn't seem to realize that the code at the target PC
;;; can modify the environment and then return here.  It will optimize code
;;; after the HW:JUMP-SAVING-PC as if this had no side effects.
(define-primop hw:jump-saving-pc (target &optional (pc-place gr:*return-pc-1*))
  (:side-effects? t)
  (:generate (node)
    (let ((pc-place (call-arg-n 3 node)))
      (unless pc-place (setq pc-place 'gr:*return-pc-1*))
      (generate-move 'K:TRAP-PC+ pc-place)
      (emit 'K:JUMP (leaf-value (call-arg-n 2 node))))))


;;; I can't imagine what you might use this for.
(define-primop hw:return (value)
  (:side-effects? t)
  (:special? t)
  (:generate (node)
      (emit 'K:RETURN (get-right-operand (call-arg-n 2 node)))))

;;; Use this if you are setting up the return registers yourself
;;; Don't forget to set *number-of-return-values*
(define-primop hw:return-mv (value)
  (:side-effects? t)
  (:special? t)
  (:generate (node)
      (emit 'K:RETURN-MV (get-right-operand (call-arg-n 2 node)))))

(define-primop hw:return-tail (value)
  (:side-effects? t)
  (:special? t)
  (:generate (node)
      (emit 'K:RETURN-TAIL (get-right-operand (call-arg-n 2 node)))))


;(define-primop hw:unboxed-constant (number)
;  (:generate (node)
;    (let ((n (call-arg-n 2 node)))
;      (if (and (literal-node? n)
;              (integerp (leaf-value n)))
;       (emit 'K:MOVEI (get-destination (call-arg-n 1 node)) (cons '/# (leaf-value n)) 'K:UNBOXED)
;       (error "Argument to HW:UNBOXED-CONSTANT: ~s should be an integer." (leaf-value n))))))

(define-primop hw:32zerop (number)
  (:conditional? t)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:generate (node)
    (generate-arith-predicate node 'K:BR-NOT-ZERO)))

;;; Actually this is the same as eq?
(define-primop hw:32= (n1 n2)
 (:conditional? t)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-32)))

(define-primop hw::32> (n1 n2)
  (:conditional? t)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:generate (node)
    (comparator node 'k:BR-NOT-GREATER-THAN 'K:BW-32)))

(define-primop hw::32< (n1 n2)
  (:conditional? t)
  (:presimplify (node)
    (presimplify-to-conditional node))
  (:generate (node)
    (comparator node 'k:BR-NOT-LESS-THAN 'K:BW-32)))

(define-primop hw:32>= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-OR-EQUAL 'K:BW-32))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t))

(define-primop hw:32<= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-LESS-OR-EQUAL 'K:BW-32))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t))

(defun hw-32logand (n1 n2)
  (unboxed-fcn logand n1 n2))

(define-primop hw:32logand (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'hw-32logand))
  (:generate (node)
      (generate-binop node 'K:AND 'K:AND 'K:BW-32)))

(defun hw-32logxor (n1 n2)
  (unboxed-fcn logxor n1 n2))

(define-primop hw:32logxor (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'logxor))
  (:generate (node)
      (generate-binop node 'K:XOR 'K:XOR 'K:BW-32)))

(defun hw-32logior (n1 n2)
  (unboxed-fcn logior n1 n2))

(define-primop hw:32logior (n1 n2)
  (:simplify (node)
      (simplify-if-constant-expression node 'hw-32logior))
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

(define-primop hw:32-1- (n)
;  (:simplify (node)
;      (simplify-if-constant-expression node '1-))
  (:generate (node)
    (generate-unop node 'K:R-1 'K:BW-32)))

(define-primop hw:32-2- (n)
;  (:simplify (node)
;      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-unop node 'K:R-2 'K:BW-32)))

(define-primop hw:32-4- (n)
;  (:simplify (node)
;      (simplify-if-constant-expression node '1+))
  (:generate (node)
    (generate-unop node 'K:R-4 'K:BW-32)))

(define-primop hw:accumulate-box-bits (n1 n2)
  (:generate (node)
    (let ((dest (get-destination (cont node))))
      ;; the destination of this instruction can't be the MD
      ;; (or VMA which we aren't doing)
      ;; because BOXED-RIGHT needs to be set
      (if (MD-p dest)
          (multiple-value-bind (l r)
              (get-operands (call-arg-n 2 node) (call-arg-n 3 node))
            (emit-alu 'K:SHIFT-UP-LF-L R0 l r '(K:BW-24 K:CARRY-1 K:BOXED-RIGHT))
            (generate-move R0 dest)
            dest)
      (generate-binop node 'K:SHIFT-UP-LF-L 'K:SHIFT-UP-LF-L 'K:BW-24 'K:CARRY-1 'K:BOXED-RIGHT)))))


(define-primop hw:32+ (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L+R 'K:L+R)))

(define-primop hw:32- (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L-R 'K:R-L)))

(define-primop hw:8+ (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L+R 'K:L+R 'K:BW-8 'K:BOXED-RIGHT)))

(define-primop hw:8-1+ (n)
  (:generate (node)
    (generate-unop node 'K:R+1 'K:BW-8 'K:BOXED-RIGHT)))

(define-primop hw:8-1- (n)
  (:generate (node)
    (generate-unop node 'K:R-1 'K:BW-8 'K:BOXED-RIGHT)))

(define-primop hw:24-1+ (n)
  (:generate (node)
    (generate-unop node 'K:R+1 'K:BW-24 'K:BOXED-RIGHT)))

(define-primop hw:24-1- (n)
  (:generate (node)
    (generate-unop node 'K:R-1 'K:BW-24 'K:BOXED-RIGHT)))

(define-primop hw:8- (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L-R 'K:R-L 'K:BW-8 'K:BOXED-RIGHT)))

(define-primop hw:left+right+1-bw24 (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L+R+C 'K:L+R+C 'K:BW-24 'k:CARRY-1 'K:BOXED-RIGHT)))

(define-primop hw:left+right+1-bw32 (n1 n2)
  (:generate (node)
    (generate-binop node 'K:L+R+C 'K:L+R+C 'K:BW-32 'K:CARRY-1 'K:UNBOXED)))

(define-primop hw:32logical-shift-up (n nbits)
  (:generate (node)
      (destructure (((cont n nbits) (call-args node)))
        (let ((dest (get-destination cont)))
          (if (literal-node? nbits)
              (let ((amount (leaf-value nbits)))
                (emit-alu-field 'K:NB-SHIFT-0F-R
                                dest IGNORED (get-right-operand n)
                                `(PRIMS::BYTE 0 ,amount) 'K:PW-II))
            (progn
              (generate-load-sr-position nbits)
              (emit-alu-field 'K:NB-SHIFT-0F-R
                                dest IGNORED (get-right-operand n)
                                ''0 'K:PW-RR)))
          dest))))

(define-primop hw:32logical-shift-down (n nbits)
  (:generate (node)
      (destructure (((cont n nbits) (call-args node)))
        (let ((dest (get-destination cont)))
          (if (literal-node? nbits)
              (let ((amount (leaf-value nbits)))
                (emit-alu-field 'K:NB-SHIFT-0F-R
                                dest IGNORED (get-right-operand n)
                                `(PRIMS::BYTE 0 ,(- amount)) 'K:PW-II))
            (progn
              (generate-load-sr-minus-position nbits)
              (emit-alu-field 'K:NB-SHIFT-0F-R
                                dest IGNORED (get-right-operand n)
                                ''0 'K:PW-RR)))

          dest))))

(define-primop hw:32arithmetic-shift-down (n nbits)
  (:generate (node)
      (destructure (((cont n nbits) (call-args node)))
        (let ((dest (get-destination cont)))
          (if (literal-node? nbits)
              (let ((amount (leaf-value nbits)))
                (emit-alu-field 'K:NB-SHIFT-AR-R
                                dest IGNORED (get-right-operand n)
                                `(PRIMS::BYTE 0 ,(- amount)) 'K:PW-II))
            (progn
              (generate-load-sr-minus-position nbits)
              (emit-alu-field 'K:NB-SHIFT-AR-R
                                dest IGNORED (get-right-operand n)
                                ''0 'K:PW-RR)))
          dest))))

(define-primop hw:32arithmetic-shift-up (n nbits)
  (:generate (node)
      (destructure (((cont n nbits) (call-args node)))
        (let ((dest (get-destination cont)))
          (if (literal-node? nbits)
              (let ((amount (leaf-value nbits)))
                (emit-alu-field 'K:NB-SHIFT-AR-R
                                dest IGNORED (get-right-operand n)
                                `(PRIMS::BYTE 0 ,amount) 'K:PW-II))
            (progn
              (generate-load-sr-position nbits)
              (emit-alu-field 'K:NB-SHIFT-AR-R
                                dest IGNORED (get-right-operand n)
                                ''0 'K:PW-RR)))
          dest))))

(define-primop hw:32-rotate-up (n nbits)
  (:generate (node)
      (destructure (((cont n nbits) (call-args node)))
        (let ((dest (get-destination cont)))
          (if (literal-node? nbits)
              (let ((amount (leaf-value nbits)))
                (emit-alu-field 'K:ROTATE-R
                                dest IGNORED (get-right-operand n)
                                `(PRIMS::BYTE 0 ,amount) 'K:PW-II 'K:UNBOXED))
            (progn
              (generate-load-sr-position nbits)
              (emit-alu-field 'K:ROTATE-R
                                dest IGNORED (get-right-operand n)
                                ''0 'K:PW-RR 'K:UNBOXED)))
          dest))))

;;; This is just like equal but doesn't TRAP-UNLES-BOTH-FIX
(define-primop hw:24= (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-EQUAL 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t))

(define-primop hw:24> (n1 n2)
 (:generate (node)
   (comparator node 'K:BR-NOT-GREATER-THAN 'K:BW-24))
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:conditional? t))

;;; like + but no dtp trap
(define-primop hw:24+ (left right)
  (:generate (node)
    (generate-binop node 'K:L+R 'K:L+R 'K:BW-24 'K:BOXED-RIGHT)))

(define-primop hw:24- (left right)
  (:generate (node)
    (generate-binop node 'K:L-R 'K:L-R 'K:BW-24 'K:BOXED-RIGHT)))

;;; (ALU-FIELD ALIGNED-FIELD-XOR NOP l r dtp)
(define-primop hw::field= (x y byte-spec)
 (:conditional? t)
 (:presimplify (node)
   (presimplify-to-conditional node))
 (:generate (node)
   (let ((byte-spec (call-arg-n 6 node)))
     (multiple-value-bind (l r)
         (get-operands (call-arg-n 4 node) (call-arg-n 5 node))
       (if (literal-node? byte-spec)
           (emit-alu-field 'K:ALIGNED-FIELD-XOR 'K:NOP l r `',(leaf-value byte-spec) 'K:PW-II)
         (error "not yet implemented"))
       (generate-conditional node 'K:BR-NOT-EQUAL)))))

(define-primop hw::load-q-register (value)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont value) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:LOAD-Q-R dest
          (get-left-operand value)
          (get-right-operand value) '(k:BW-32 K:BOXED-RIGHT))
        dest))))

;;This is not currently used 9/22/88 but might be used for optimization where you want the status registers
;; filled with the status of the low 24bits (requires that the high 8 bits are in a known state) --wkf
#+Not-currently-used
(define-primop hw::24-load-q-register (value)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont value) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:LOAD-Q-R dest
          (get-left-operand value)
          (get-right-operand value) '(K:BW-24 K:BOXED-RIGHT))
        dest))))

(define-primop hw::read-q-register ()
  (:generate (node)
    (destructure (((cont) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:PASS-Q dest 'K:IGNORE 'K:IGNORE '(K:BW-32 K:UNBOXED))
        dest))))

(define-primop hw::read-q-register-boxed ()
  (:generate (node)
    (destructure (((cont) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:PASS-Q dest 'K:IGNORE 'K:IGNORE '(K:BW-32 K:BOXED))
        dest))))

(define-primop hw::read-alu-status ()
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:PASS-STATUS dest 'GR::*ZERO* 'GR::*ZERO* '(K:BW-24 K:BOXED))
        dest))))

(define-primop hw::signed-multiply-first-step (a b)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont a b) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:SMUL-FIRST dest (get-left-operand a) (get-right-operand b) '(K:BW-24 K:BOXED-RIGHT))
        dest))))

(define-primop hw::signed-multiply-step (a b)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont a b) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:SMUL-STEP dest (get-left-operand a) (get-right-operand b) '(K:BW-24 K:BOXED-RIGHT))
        dest))))

(define-primop hw::32-sign-fill (a)
  (:generate (node)
    (destructure (((cont a) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:SIGN dest (get-left-operand a) (get-right-operand a) '(K:BW-32 K:UNBOXED))
        dest))))

(define-primop hw::24-sign-fill (a)
  (:generate (node)
    (destructure (((cont a) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:SIGN dest (get-left-operand a) (get-right-operand a) '(K:BW-24 K:BOXED-RIGHT))
        dest))))

(define-primop hw::32-sign-extend-byte (a)
  (:generate (node)
    (destructure (((cont a) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:SEX-R dest (get-left-operand a) (get-right-operand a) '(K:BW-8 K:UNBOXED)) ;;8 Width before extending
        dest))))

(define-primop hw::32-sign-extend (a)
  (:generate (node)
    (destructure (((cont a) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:SEX-R dest (get-left-operand a) (get-right-operand a) '(K:BW-24 K:UNBOXED)) ;;24 Width before extending
        dest))))

(define-primop hw::32-prioritize (a)
  (:generate (node)
    (destructure (((cont a) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:PRIORITIZE-R dest (get-left-operand a) (get-right-operand a) '(K:BW-32 K:UNBOXED))
        dest))))

(define-primop hw::24-prioritize (a)
  (:generate (node)
    (destructure (((cont a) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'K:PRIORITIZE-R dest (get-right-operand a) (get-right-operand a) '(K:BW-24 K:UNBOXED))
        dest))))

(define-primop hw::signed-divide-first-step (x y)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont x y) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'k:sdiv-first dest (get-left-operand x) (get-right-operand y) '(K:bw-24 K:BOXED))
        dest))))


(define-primop hw::signed-divide-step (x y)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont x y) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'k:sdiv-step dest (get-left-operand x) (get-right-operand y) '(K:bw-24 K:BOXED))
        dest))))

(define-primop hw::signed-divide-last1 (x y)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont x y) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'k:sdiv-last1 dest (get-left-operand x) (get-right-operand y) '(K:bw-24 k:boxed))
        dest))))

(define-primop hw::signed-divide-last2 (x y)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont x y) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'k:sdiv-last2 dest (get-left-operand x) (get-right-operand y) '(K:bw-24 k:boxed))
        dest))))

(define-primop hw::quotient-correct (x)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont x) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'k:quo-corr dest (get-left-operand x) (get-right-operand x) '(K:bw-24 k:boxed))
        dest))))

(define-primop hw::remainder-correct (x y)
  (:side-effects? t)
  (:generate (node)
    (destructure (((cont x y) (call-args node)))
      (let ((dest (get-destination cont)))
        (emit-alu 'k:rem-corr dest (get-left-operand x) (get-right-operand y) '(K:bw-24 k:boxed))
        dest))))



;;; Floating point chip primitives

(define-primop hw:float-add-single (x y)
  (:generate (node)
    (generate-float-single-op 'K:FALU 'K:SINGLE-ADD node)))

(define-primop hw:float-subtract-single (x y)
  (:generate (node)
    (generate-float-single-op 'K:FALU 'K:SINGLE-SUBTRACT node)))

(define-primop hw:float-multiply-single (x y)
  (:generate (node)
    (generate-float-single-op 'K:FMUL 'K:SINGLE-MULTIPLY node)))

(define-primop hw:float-divide-single (divisor dividend) ;;||| Args are backwards for some reason 9/29/88 --wkf
  (:generate (node)
    (generate-float-single-op 'K:FMUL 'K:SINGLE-DIVIDE node 9)))

(define-primop hw:float-compare-single (x y)
  (:side-effects? t)
  (:generate (node)
    (generate-float-single-op 'K:FALU 'K:SINGLE-COMPARE node)))

(defun generate-float-single-op (wtl op node &optional delay)
  (destructure (((cont x y) (call-args node)))
    (multiple-value-bind (dest left right) (get-dest-and-operands cont x y)
      (emit wtl op 'K:NOP left right 'K:FPU-LOAD-XY  'K:FPU-UNLOAD-HIGH 'K:UNBOXED)
      (emit wtl op 'K:NOP left right 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-HIGH 'K:UNBOXED)
      (when delay
        (generate-delay delay))
      (emit wtl op 'K:NOP left right 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-HIGH 'K:UNBOXED)
      (emit wtl op 'K:NOP left right 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-HIGH 'K:UNBOXED)
      (emit wtl op dest   left right 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-HIGH 'K:UNBOXED)
      dest)))

(defun generate-delay (ticks &optional (reg R0))
  ;; ||| Changed to include looping delay when possible, fixed bug with loop time. 10/12/88 --wkf
  (multiple-value-bind (quotient remainder)
      (floor (1- ticks) 3)
    (let ((tag (gen-tag 'loop)))
      (dotimes (i remainder)
        (emit 'K:NOP))
      (if (zerop quotient)
          (emit 'K:NOP)
        (emit-code
          `((K:MOVEI ,reg ',quotient)
            ,tag
            (K:ALU K:R-1 ,reg K:IGNORE ,reg K:BW-24)
            (K:TEST K:BR-NOT-ZERO)
            (K:BRANCH ,tag)))))))

(define-primop hw:float-add-double (xhi xlo yhi ylo)
  (:special? t)
  (:generate (node)
     (generate-float-double-op 'K:FALU 'K:DOUBLE-ADD node)))

(define-primop hw:float-subtract-double (xhi xlo yhi ylo)
  (:special? t)
  (:generate (node)
     (generate-float-double-op 'K:FALU 'K:DOUBLE-SUBTRACT node)))

(define-primop hw:float-multiply-double (xhi xlo yhi ylo)
  (:special? t)
  (:generate (node)
     (generate-float-double-op 'K:FMUL 'K:DOUBLE-MULTIPLY node)))

(define-primop hw:float-divide-double (xhi xlo yhi ylo)
  (:special? t)
  (:generate (node)
     (generate-float-double-op 'K:FMUL 'K:DOUBLE-DIVIDE node 21)))

(define-primop hw:float-compare-double (xhi xlo yhi ylo)
  (:special? t)
  (:side-effects? t)
  (:generate (node)
     (generate-float-double-op 'K:FALU 'K:DOUBLE-COMPARE node)))

(defun generate-float-double-op (wtl op node &optional (delay 3))
  ;;Changed to take wtl of either 'K:FMUL or 'K:FALU 9/30/88 --wkf |||
  (destructure (((cont xhi xlo yhi ylo) (call-args node)))
    (multiple-value-bind (y-left y-right) (get-operands yhi ylo)
      ;; Load Y
      (emit wtl op 'K:NOP y-left y-right 'K:FPU-LOAD-Y 'K:FPU-UNLOAD-HIGH 'K:UNBOXED))
    (multiple-value-bind (x-left x-right) (get-operands xhi xlo)
      ;; Load X and start alu
      (emit wtl op 'K:NOP x-left x-right 'K:FPU-LOAD-X 'K:FPU-UNLOAD-HIGH 'K:UNBOXED))
    (generate-delay delay)
    (if (not (lambda-node? cont))
        (progn (error "You should use MULTIPLE-VALUE-BIND with double-float operations")
               (generate-continuation ''NIL cont 1))
      (destructure (((hi lo) (lambda-variables cont)))
        (let ((hi-acc (acc hi))
              (lo-acc (if lo (acc lo) IGNORED)))
          (emit wtl op 'K:NOP IGNORED IGNORED 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-HIGH 'K:UNBOXED)
          (emit wtl op 'K:NOP IGNORED IGNORED 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-LOW 'K:UNBOXED)
          (emit wtl op (if (or (register-p hi-acc)
                                   (global-p hi-acc))
                               hi-acc R0)
                IGNORED IGNORED 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-LOW 'K:UNBOXED)
          (emit wtl op (if (or (register-p lo-acc)
                                   (global-p lo-acc))
                               lo-acc R1)
                IGNORED IGNORED 'K:FPU-LOAD-NOP 'K:FPU-UNLOAD-LOW 'K:UNBOXED)
          (unless (or (register-p hi-acc)
                      (global-p hi-acc))
            (generate-move R0 hi-acc))
          (unless (or (register-p lo-acc)
                      (global-p lo-acc))
            (generate-move R1 lo-acc))
          (generate-continuation hi-acc cont 2))))))

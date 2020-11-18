;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Disassembler

(defun pri (inst)
  (format t "~&~1,'0b ~1,'0b ~1,'0b ~3,'0b ~
               ~2,'0b ~2,'0b ~3,'0b ~3,'0b ~
               ~7,'0b ~4,'0b ~3,'0b ~2,'0b ~
               ~7,'0b ~6,'0b ~7,'0b X ~
               ~6,'0b ~5,'0b"
          (nlisp:ldb hw:%%i-stat-bit inst)
          (nlisp:ldb hw:%%i-spare-bit inst)
          (nlisp:ldb hw:%%i-x16 inst)
          (nlisp:ldb hw:%%i-op-code inst)

          (nlisp:ldb hw:%%i-next-pc inst)
          (nlisp:ldb hw:%%i-boxed inst)
          (nlisp:ldb hw:%%i-dtp-check inst)
          (nlisp:ldb hw:%%i-chop inst)

          (nlisp:ldb hw:%%i-destination inst)
          (nlisp:ldb hw:%%i-global-frame inst)
          (nlisp:ldb hw:%%i-jcond inst)
          (nlisp:ldb hw:%%i-bw inst)

          (nlisp:ldb hw:%%i-right-source inst)
          (nlisp:ldb hw:%%i-left-source inst)
          (nlisp:ldb hw:%%i-alu-op inst)

          (nlisp:ldb hw:%%i-alu-shift inst)
          (nlisp:ldb hw:%%i-alu-mask inst)))



(defmacro idecode (field inst &body clauses)
  (let ((temp (gensym)))
    `(let ((,temp (nlisp:ldb ,field ,inst)))
       (cond ,@(mapcar #'(lambda (clause)
                           `(,(cond ((eq (car clause) t)
                                     t)
                                    ((listp (car clause))
                                     `(or ,@(mapcar #'(lambda (tst)
                                                        `(eq ,temp ,tst))
                                                    (car clause))))
                                    (t
                                     `(eq ,temp ,(car clause))))
                             ,(cadr clause)))
                       clauses)))))



;----------------------------------------------------------------
;;; Instructions

(defun dis-opcode (inst)
  (idecode hw:%%i-op-code inst
    ((hw:$$i-op-code-alu hw:$$i-op-code-alu-with-link)
     (idecode hw:%%i-next-pc inst
       ((hw:$$i-next-pc-pc+1 hw:$$i-next-pc-return)
        (idecode hw:%%i-alu-op inst

;; leave this line out to see MOVEs as what they really are ALU SETR ..  --pfc
;         (hw:$$i-alu-op-zero-ext-right 'K:MOVE)

          (t
            ;; a bit klugey
            (let ((op (nlisp:ldb hw:%%i-alu-op inst)))
              (if (and (>= op hw:$$i-alu-op-not-f-left)
                       (<= op hw:$$i-alu-op-pass-mask))
                  'K:ALU-FIELD
                'K:ALU)))))
       (hw:$$i-next-pc-dispatch
        'K:DISPATCH)
       (hw:$$i-next-pc-ir
        (idecode hw:%%i-chop inst
          ((hw:$$i-chop-nop hw:$$i-chop-open hw:$$i-chop-topen)
           (idecode hw:%%i-cond inst
             (hw:$$i-cond-conditional-branch   'K:BRANCH)
             (hw:$$i-cond-unconditional-branch 'K:UNCONDITIONAL-BRANCH)))
          (hw:$$i-chop-call
           'K:CALLZ)
          (hw:$$i-chop-open-call
           'K:OPEN-CALLZ)
          (hw:$$i-chop-tcall
           'K:TAIL-CALLZ)
          (hw:$$i-chop-topen-call
           'K:TAIL-OPEN-CALLZ)))))
    ((hw:$$i-op-code-alui hw:$$i-op-code-alui-with-link)
      'K:ALUI-16)
    (hw:$$i-op-code-loadi-32
      'K:MOVEI)
    (hw:$$i-op-code-move
      (idecode hw:%%i-chop inst
        ((hw:$$i-chop-nop hw:$$i-chop-open hw:$$i-chop-topen)
         'K:JUMP)
        (hw:$$i-chop-call
          'K:CALL)
        (hw:$$i-chop-open-call
          'K:OPEN-CALL)
        (hw:$$i-chop-tcall
          'K:TAIL-CALL)
        (hw:$$i-chop-topen-call
          'K:TAIL-OPEN-CALL)))
    (hw:$$i-op-code-fp-alu
      'K:FALU)
    (hw:$$i-op-code-fp-mult
      'K:FMUL)
    ))

;----------------------------------------------------------------
;;; Fields

(defmacro def-dis-field (field-name lambda-list &body body)
  (let ((fname (intern (concatenate 'string "DIS-" (string field-name)))))
  `(progn
     (defun ,fname ,lambda-list
       ,@body)
     (setf (get ',field-name 'dis-function) #',fname))))

(defun dis-fields (opcode inst)
  (mapcar #'(lambda (field)
              (funcall (get field 'dis-function) inst))
          (get opcode 'fields)))

(def-dis-field aluop (inst)
  (decode-aluop (nlisp:ldb hw:%%i-alu-op inst)))

(def-dis-field alui-op (inst)
  (decode-aluop (nlisp:ldb hw:%%i-alui-op inst)))

(def-dis-field fpu-op (inst)
  (dis-fpuop (dis-opcode inst)
             (decode-fpuop (nlisp:ldb hw:%%i-fpu-function inst))
             (decode-fpuload (nlisp:ldb hw:%%i-fpu-load inst))))


;; This depends on the order in which the DEF-FPUOPs are defined in ASSEM.LISP
;; Assumes that:
;;    1) K:DEFAULT-MODEx is defined after the arithmetic functions
;;    2) K:SINGLE|DOUBLE-MULTIPLY is defined after K:SINGLE|DOUBLE-SUBTRACT
;; The correct way to do this would be to change the assembler to keep separate tables.
;;       --pfc 6/1/88
(defun dis-fpuop (opcode fpu-function fpu-load)
  (if (eq fpu-load 'K:FPU-LOAD-MODE)
      fpu-function
    (case fpu-function
      (K:FPU-DEFAULT-MODE0 'K:DOUBLE-SUBTRACT)
      (K:FPU-DEFAULT-MODE1 'K:SINGLE-ADD)
      (K:FPU-DEFAULT-MODE2 'K:SINGLE-COMPARE)
      (K:SINGLE-MULTIPLY   (if (eq opcode 'K:FMUL)
                               'K:SINGLE-MULTIPLY
                             'K:SINGLE-SUBTRACT))
      (K:DOUBLE-MULTIPLY   (if (eq opcode 'K:FMUL)
                               'K:DOUBLE-MULTIPLY
                             'K:DOUBLE-SUBTRACT))
      (otherwise fpu-function))))

(def-dis-field fpu-load (inst)
  (decode-fpuload (nlisp:ldb hw:%%i-fpu-load inst)))

(def-dis-field fpu-unload (inst)
  (decode-fpuunload (nlisp:ldb hw:%%i-fpu-unload inst)))

(def-dis-field destination (inst)
  (let ((dest (nlisp:ldb hw:%%i-destination inst)))
    (idecode hw:%%i-reg-funcp dest
      (hw:$$i-reg-func
        (decode-fdest dest))
      (t (dis-dest-or-src dest inst)))))

(def-dis-field right-source (inst)
  (let ((source (nlisp:ldb hw:%%i-right-source inst)))
    (idecode hw:%%i-reg-funcp source
      (hw:$$i-reg-func
        (decode-fsource source))
      (t (dis-dest-or-src source inst)))))


(def-dis-field left-source (inst)
  (dis-dest-or-src (nlisp:ldb hw:%%i-left-source inst) inst))


(defun dis-dest-or-src (dors inst)
  (let ((offset (nlisp:ldb hw:%%i-reg-offset dors)))
    (idecode hw:%%i-reg-base dors
      (hw:$$i-reg-base-active
        (nth offset '(K:A0 K:A1 K:A2 K:A3 K:A4 K:A5 K:A6 K:A7 K:A8 K:A9 K:A10 K:A11 K:A12 K:A13 K:A14 K:A15)))
      (hw:$$i-reg-base-open
        (nth offset '(K:O0 K:O1 K:O2 K:O3 K:O4 K:O5 K:O6 K:O7 K:O8 K:O9 K:O10 K:O11 K:O12 K:O13 K:O14 K:O15)))
      (hw:$$i-reg-base-return
        (nth offset '(K:R0 K:R1 K:R2 K:R3 K:R4 K:R5 K:R6 K:R7 K:R8 K:R9 K:R10 K:R11 K:R12 K:R13 K:R14 K:R15)))
      (hw:$$i-reg-base-global
        (let ((frame-num (nlisp:ldb hw:%%i-global-frame inst)))
          (or (global-name frame-num offset)
              (list 'K:REGISTER 'UNKNOWN frame-num offset)))))))


(def-dis-field alu (inst)
  (cons 'K:ALU (dis-fields 'K:ALU inst))

;; leave these lines out to see MOVEs as what they really are ALU SETR ..  --pfc
; (idecode hw:%%i-alu-op inst
;   (hw:$$i-alu-op-zero-ext-right
;     (cons 'K:MOVE (dis-fields 'K:MOVE inst)))
;   (t (cons 'K:ALU (dis-fields 'K:ALU inst))))

 )


(def-dis-field local-ref (inst)
  (nlisp:ldb hw:%%i-branch-address inst))

(def-dis-field external-ref (inst)
;  (format nil "#x~x"
          (nlisp:ldb hw:%%i-jump-address inst)
;         )
  )

(def-dis-field call-nargs (inst)
  (declare (ignore inst))
  '<nargs>)

(def-dis-field call-return-dest (inst)
  (if (zerop (nlisp:ldb hw:%%i-call-dret-6 inst))
      (dis-dest-or-src
        (set-fields
          (nlisp:byte 5. 1.) (nlisp:ldb hw:%%i-call-dret-5-1 inst)
          (nlisp:byte 1. 0.) (nlisp:ldb hw:%%i-call-dret-0   inst)
          0)
        inst)
    '<new-open-dest>))

(def-dis-field call-move (inst)
  (list (dis-destination inst)
        (dis-right-source inst)))

(def-dis-field immediate-32 (inst)
  ;(nlisp:ldb hw:%%i-imm32-data inst)
;  (format nil "#x~x"
  (logand inst #xFFFFFFFF)
;  )
)

(def-dis-field immediate-16 (inst)
;  (format nil "#x~x"
  (nlisp:ldb hw:%%i-imm16-data inst)
;  )
)

(def-dis-field byte-spec (inst)
  `(BYTE ,(nlisp:ldb hw:%%i-alu-mask inst) ,(nlisp:ldb hw:%%i-alu-shift inst)))

;----------------------------------------------------------------`
;;;; Options

;;; Not all options are valid for all instructions
;;; No jcond or bw in call

(defun dis-options (opcode inst)
  (remove-if-not #'identity
   (list
     (dis-chop inst)
     (if (not (global:memq opcode '(K:CALLZ K:OPEN-CALLZ K:TAIL-CALLZ K:TAIL-OPEN-CALLZ
                                    K:CALL K:OPEN-CALL K:TAIL-CALL K:TAIL-OPEN-CALL)))
         (dis-bw inst))
     (if (not (global:memq opcode '(K:CALLZ K:OPEN-CALLZ K:TAIL-CALLZ K:TAIL-OPEN-CALLZ
                                    K:CALL K:OPEN-CALL K:TAIL-CALL K:TAIL-OPEN-CALL)))
         (dis-jcond inst))
     (dis-boxed inst)
     (dis-boxed-md  inst)
     (dis-boxed-vma inst)
     (if (and (global:memq opcode '(K:CALLZ K:OPEN-CALLZ K:TAIL-CALLZ K:TAIL-OPEN-CALLZ
                                    K:CALL K:OPEN-CALL K:TAIL-CALL K:TAIL-OPEN-CALL
                                    k:branch k:unconditional-branch k:jump k:jump-conditional))
              (= (nlisp:ldb hw:%%i-next-pc inst)
                 hw:$$i-next-pc-pc+1))
         'k:next-pc-pc+1)
     (dis-dt    inst)
     (dis-trap inst)
     (dis-stat inst))))


(defun dis-chop (inst)
  (idecode hw:%%i-chop inst
    (hw:$$i-chop-open
      'K:CH-OPEN)
    (hw:$$i-chop-topen
      'K:CH-TAIL-OPEN)
    (hw:$$i-chop-return
      'K:CH-RETURN)))


(defun dis-bw (inst)
  (idecode hw:%%i-bw inst
    (hw:$$i-bw-8  'K:BW-8)
    (hw:$$i-bw-16 'K:BW-16)
    (hw:$$i-bw-24 'K:BW-24)))

(defun dis-dt (inst)
  (idecode hw:%%i-dtp-check inst
    (vinc:$$dtc-none                            'K:DT-NONE)
    (vinc:$$dtc-hairy-number                    'K:DT-HAIRY-NUMBER)
    (vinc:$$dtc-both-character                  'K:DT-BOTH-CHARACTER)
    (vinc:$$dtc-right-array-and-left-structure  'K:DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE)
    (vinc:$$dtc-right-list                      'K:DT-RIGHT-LIST)
    (vinc:$$dtc-both-fixnum                     'K:DT-BOTH-FIXNUM)
    (vinc:$$dtc-both-fixnum-with-overflow       'K:DT-BOTH-FIXNUM-WITH-OVERFLOW)
    (0.      'K:DT-0)
    (1.      'K:DT-1)
    (2.      'K:DT-2)
    (3.      'K:DT-3)
    (4.      'K:DT-4)
    (5.      'K:DT-5)
    (6.      'K:DT-6)
    (7.      'K:DT-7)))


(defun dis-jcond (inst)
  (idecode hw:%%i-jcond inst
    (hw:$$i-jcond-eq  'K:BR-EQUAL)
    (hw:$$i-jcond-neq 'K:BR-NOT-EQUAL)
    (hw:$$i-jcond-lt  'K:BR-LESS-THAN)
    (hw:$$i-jcond-ge  'K:BR-GREATER-OR-EQUAL)
    (hw:$$i-jcond-gt  'K:BR-GREATER-THAN)
    (hw:$$i-jcond-le  'K:BR-LESS-OR-EQUAL)))

(defun dis-boxed (inst)
  (idecode hw:%%i-boxed inst
    (hw:$$i-boxed-left     'K:BOXED-LEFT)
    (hw:$$i-boxed-right    'K:BOXED-RIGHT)
    (hw:$$i-boxed-unboxed  'K:UNBOXED)
    (hw:$$i-boxed-boxed    'K:BOXED)))

(defun dis-boxed-md (inst)
  (idecode hw:%%i-md-boxed inst
    (hw:$$i-md-boxed       'K:BOXED-MD)
    (hw:$$i-md-unboxed     'K:UNBOXED-MD)))

(defun dis-boxed-vma (inst)
  (idecode hw:%%i-vma-boxed inst
    (hw:$$i-vma-boxed       'K:BOXED-VMA)
    (hw:$$i-vma-unboxed     'K:UNBOXED-VMA)))

(defun dis-stat (inst)
  (idecode hw:%%i-stat-bit inst
    (0  nil)
    (1  'k:stat-1)))

(defun dis-trap (inst)
  (idecode hw::%%i-trap-bit inst
    (0 nil)
    (1  'k:itrap-1)))

;----------------------------------------------------------------
;;; Disassemble

(defun dis (inst)
  (let ((opcode (dis-opcode inst)))
    (cons opcode
          (nconc (dis-fields opcode inst)
                 (dis-options opcode inst)))))

(defun dis-list (instructions)
  (mapcar #'dis instructions))

(defun pr-dis (instructions)
  (map nil #'(lambda (i)
               (format t "~&~a" (dis i)))
       instructions))

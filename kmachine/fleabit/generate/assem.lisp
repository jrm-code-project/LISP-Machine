;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Assembler

;;; Code looks like:
;;; (MOVE O2 A3)
;;; (MOVE RETURN A1 CH-RETURN)
;;; (MOVE O0 A1 CH-OPEN)
;;; (MOVEI A1 '259)
;;; (ALU L+R A0 A2 A3 CH-OPEN BW-24)
;;; (ALU R+1 RETURN R0 A1 CH-RETURN)
;;; (TEST BR-EQUAL)
;;; (BRANCH FOO ())
;;; (BRANCH FOO (MOVE A0 A1))
;;; (BRANCH FOO (MOVE A0 A1) CH-OPEN)
;;; (BRANCH FOO (ALU L+R O0 A1 A2) CH-OPEN)
;;; (CALL FOO A0 (O0 A1))
;;; (CALL (FOO 2) A1 ())
;;; (OPEN)
;;; (OPEN-CALL (CAR 1) O1 (O0 A1))
;;; (TAIL-CALL (FOO 2))
;;; (TAIL-CALL (FOO 2) (O1 A2))
;;; (JUMP FOO ())
;;; (JUMP FOO (A0 A1))

;;; <destination>, <left-source> and <right-source> are:
;;;   On  open registers
;;;   An  active registers
;;;   Rn  return registers
;;;   a functional destination <dest> or source <right-source>
;;;     RETURN
;;;     WRITE-VMA
;;;     (VMA-START-READ UNBOXED-VMA UNBOXED-MD)
;;;     MD-START-WRITE
;;;     READ-MD
;;;   a named register
;;;     *REGION-FREE-POINTER*
;;;     (REGISTER FOO <frame> <offset>)

(defvar *local-symbols* '())
(defvar *local-refs* '())
(defvar *refs* '())
(defvar *immediates* '())
(defvar *assembly-constants* '())
(defvar *location-counter* 0)
(defvar *load-time-evals* '())

(defmacro def-symbol-table (name)
  "Defines a symbol table, accessor, and defining macro.
    *<name>-NAME-TABLE* is a hash table
    (GET-<name> value) looks up value.
    (DEF-<name> symbol value) defines symbol to have value"
  (let ((table-name (intern (format nil "*~A-NAME-TABLE*" name))))
  `(progn
     (defvar ,table-name
             (make-hash-table :test #'eq))
     (defun ,(intern (format nil "GET-~A" name)) (name)
            (gethash name ,table-name))
     (defmacro ,(intern (format nil "DEF-~A" name)) (name value)
       `(progn
          (export ',name ',(intern (package-name (symbol-package name))))
          (setf (gethash ',name ,',table-name)
                ,value))))))


(defmacro def-symbol-tables (name)
  "Like DEF-SYMBOL-TABLE but also defines an inverse table.
    (DECODE-<name> value) returns the symbol"
  (let ((table-name (intern (format nil "*~A-NAME-TABLE*" name)))
        (inv-table-name (intern (format nil "*~A-INV-NAME-TABLE*" name))))
  `(progn
     (defvar ,table-name
             (make-hash-table :test #'eq))
     (defvar ,inv-table-name
             (make-hash-table :test #'=))
     (defun ,(intern (format nil "GET-~A" name)) (name)
            (gethash name ,table-name))
     (defun ,(intern (format nil "DECODE-~A" name)) (value)
            (gethash value ,inv-table-name))
     (defmacro ,(intern (format nil "DEF-~A" name)) (name value)
       `(progn
          (export ',name ',(intern (package-name (symbol-package name))))
          (setf (gethash ',name ,',table-name)
              ,value)
          (setf (gethash ,value ,',inv-table-name)
              ',name))))))


(eval-when (eval load compile)
(defmacro set-fields (&rest fields-and-base)
  (if (null (cdr fields-and-base))
      (car fields-and-base)
    `(nlisp:dpb ,(cadr fields-and-base)
                ,(car fields-and-base)
                (set-fields . ,(cddr fields-and-base)))))
)

;----------------------------------------------------------------
;;; Registers

(def-symbol-table reg)

(defmacro def-register (name base offset)
  `(def-reg ,name
            ,(set-fields hw:%%i-reg-funcp  hw:$$i-reg-reg
                         hw:%%i-reg-base   (eval base)
                         hw:%%i-reg-offset (eval offset)
                         0)))

(defmacro def-frame (name base)
  (setq base (eval base))
  `(progn ,@(do ((i 0 (1+ i))
                 (forms () (cons `(def-register
                                    ,(intern (format nil "~A~D"
                                                     (string name)
                                                     i)
                                             (symbol-package name))
                                    ,base ,i)
                                 forms)))
                ((= i hw:frame-size) forms))))



;;; Register Frames
(def-frame K:O hw:$$i-reg-base-open)
(def-frame K:A hw:$$i-reg-base-active)
(def-frame K:R hw:$$i-reg-base-return)


;;; R0
(defconstant random-source
    (set-fields
      hw:%%i-reg-base   hw:$$i-reg-base-return
      hw:%%i-reg-offset 0
      0))

(def-reg K:IGNORE random-source)

;----------------------------------------------------------------
;;; Functional Sources

(def-symbol-tables fsource)

(def-fsource K:MEMORY-MAP            hw:$$i-fs-memory-map)
(def-fsource K:GC-RAM                hw:$$i-fs-gc-ram)
(def-fsource K:MEMORY-CONTROL        hw:$$i-fs-memory-control-register)
(def-fsource K:MEMORY-STATUS         hw:$$i-fs-memory-status-register)
(def-fsource K:BUS-CONTROL           hw:$$i-fs-bus-control-register)
(def-fsource K:TRAP-REGISTER         hw:$$i-fs-trap-register)
(def-fsource K:STATISTICS-COUNTER    hw:$$i-fs-statistics-counter)
(def-fsource K:MICROSECOND-CLOCK     hw:$$i-fs-microsecond-clock)

(def-fsource K:PROCESSOR-STATUS      hw:$$i-fs-processor-status)
(def-fsource K:PROCESSOR-CONTROL     hw:$$i-fs-processor-control)
(def-fsource K:OPEN-ACTIVE-RETURN    hw:$$i-fs-open-active-return)
(def-fsource K:RETURN-PC-RETURN-DEST hw:$$i-fs-return-pc-return-dest)
(def-fsource K:CALL-HP-SP            hw:$$i-fs-hp-csp)          ;new name
(def-fsource K:CALL-SP-HP            hw:$$i-fs-csp-hp)          ;old name
(def-fsource K:TRAP-PC               hw:$$i-fs-trap-pc)
(def-fsource K:TRAP-PC+              hw:$$i-fs-trap-pc+)

(def-fsource K:VMA                   hw:$$i-fs-vma)
(def-fsource K:MD                    hw:$$i-fs-md)

(def-fsource K:ICACHE-A-LO           hw:$$i-fs-read-cache-a-lo)
(def-fsource K:ICACHE-A-HI           hw:$$i-fs-read-cache-a-hi)
(def-fsource K:ICACHE-B-LO           hw:$$i-fs-read-cache-b-lo)
(def-fsource K:ICACHE-B-HI           hw:$$i-fs-read-cache-b-hi)
(def-fsource K:TRAP-OFF              hw:$$i-fs-read-trap-enable-and-disable)

;----------------------------------------------------------------
;;; Functional Destinations

(def-symbol-tables fdest)

(def-fdest K:RETURN                      hw:$$i-fd-return)
(def-fdest K:RETURN-MV                   hw:$$i-fd-return-mv)
(def-fdest K:RETURN-TAIL                 hw:$$i-fd-return-tail)

(def-fdest K:DATATYPE-RAM-WRITE-PULSE    hw:$$i-fd-datatype-write-pulse)
(def-fdest K:PROCESSOR-CONTROL           hw:$$i-fd-processor-control-register)
(def-fdest K:OPEN-ACTIVE-RETURN          hw:$$i-fd-call-hardware-o-a-r)
(def-fdest K:RETURN-PC-RETURN-DEST       hw:$$i-fd-return-pc-return-dest)
(def-fdest K:CALL-HP-SP                  hw:$$i-fd-call-hardware-hp-sp)         ;new name
(def-fdest K:CALL-SP-HP                  hw:$$i-fd-call-hardware-sp-hp)         ;old name

(def-fdest K:NOP                         hw:$$i-fd-nop)

(def-fdest K:MEMORY-MAP                  hw:$$i-fd-memory-map)
(def-fdest K:GC-RAM                      hw:$$i-fd-gc-ram)
(def-fdest K:MEMORY-CONTROL              hw:$$i-fd-memory-control-register)
(def-fdest K:BUS-CONTROL                 hw:$$i-fd-bus-control-register)
(def-fdest K:MICROSECOND-CLOCK           hw:$$i-fd-microsecond-clock)
(def-fdest K:STATISTICS-COUNTER          hw:$$i-fd-statistics-counter)
(def-fdest K:TRANSPORTER-RAM             hw:$$i-fd-transporter-ram-write-pulse)

(def-fdest K:VMA                         hw:$$i-fd-vma)
(def-fdest K:MD                          hw:$$i-fd-md)
(def-fdest K:VMA-START-WRITE-NO-GC-TRAP  hw:$$i-fd-vma-start-write-no-gc-trap)
(def-fdest K:VMA-START-WRITE             hw:$$i-fd-vma-start-write)
(def-fdest K:MD-START-WRITE-NO-GC-TRAP   hw:$$i-fd-md-start-write-no-gc-trap)
(def-fdest K:MD-START-WRITE              hw:$$i-fd-md-start-write)

(def-fdest K:VMA-START-READ-NO-TRANSPORT     hw:$$i-fd-vma-start-read-no-transport)
(def-fdest K:VMA-START-READ                  hw:$$i-fd-vma-start-read)
(def-fdest K:VMA-START-READ-VISIBLE-EVCP     hw:$$i-fd-vma-start-read-visible-evcp)
(def-fdest K:VMA-START-READ-WILL-WRITE       hw:$$i-fd-vma-start-read-will-write)
(def-fdest K:VMA-START-READ-CDR-NO-TRANSPORT hw:$$i-fd-vma-start-read-cdr-no-transport)
(def-fdest K:VMA-START-READ-CDR              hw:$$i-fd-vma-start-read-cdr)
(def-fdest K:VMA-START-READ-CDR-VISIBLE-EVCP hw:$$i-fd-vma-start-read-cdr-visible-evcp)
(def-fdest K:VMA-START-READ-CDR-WILL-WRITE   hw:$$i-fd-vma-start-read-cdr-will-write)

(def-fdest K:VMA-START-READ-EARLY-NO-TRANSPORT     hw:$$i-fd-vma-start-read-early-no-transport)
(def-fdest K:VMA-START-READ-EARLY                  hw:$$i-fd-vma-start-read-early)
(def-fdest K:VMA-START-READ-EARLY-VISIBLE-EVCP     hw:$$i-fd-vma-start-read-early-visible-evcp)
(def-fdest K:VMA-START-READ-EARLY-WILL-WRITE       hw:$$i-fd-vma-start-read-early-will-write)
(def-fdest K:VMA-START-READ-EARLY-CDR-NO-TRANSPORT hw:$$i-fd-vma-start-read-early-cdr-no-transport)
(def-fdest K:VMA-START-READ-EARLY-CDR              hw:$$i-fd-vma-start-read-early-cdr)
(def-fdest K:VMA-START-READ-EARLY-CDR-VISIBLE-EVCP hw:$$i-fd-vma-start-read-early-cdr-visible-evcp)
(def-fdest K:VMA-START-READ-EARLY-CDR-WILL-WRITE   hw:$$i-fd-vma-start-read-early-cdr-will-write)


;----------------------------------------------------------------
;;; Alu operations

(def-symbol-tables aluop)

(def-aluop K:PASS-STATUS hw:$$i-alu-op-pass-stat)
(def-aluop K:LOAD-STATUS-R hw:$$i-alu-op-ld-stat-right)
(def-aluop K:LOAD-STATUS-L hw:$$i-alu-op-ld-stat-left)
(def-aluop K:PASS-Q   hw:$$i-alu-op-pass-q)
(def-aluop K:LOAD-Q-R hw:$$i-alu-op-load-q-right)
(def-aluop K:LOAD-Q-L hw:$$i-alu-op-load-q-left)

(def-aluop K:NOT-R hw:$$i-alu-op-not-right)
(def-aluop K:NOT-L hw:$$i-alu-op-not-left)
(def-aluop K:OR    hw:$$i-alu-op-or)
(def-aluop K:AND   hw:$$i-alu-op-and)
(def-aluop K:XOR   hw:$$i-alu-op-xor)
(def-aluop K:XNOR  hw:$$i-alu-op-xnor)
(def-aluop K:ZERO  hw:$$i-alu-op-zero)
(def-aluop K:SIGN  hw:$$i-alu-op-sign)

(def-aluop K:SHIFT-DN-AR-RQ  hw:$$i-alu-op-dnl-ar-right-q)
(def-aluop K:SHIFT-DN-AR-R  hw:$$i-alu-op-dnl-ar-right)
(def-aluop K:SHIFT-DN-AR-L  hw:$$i-alu-op-dnl-ar-left)
(def-aluop K:SHIFT-DN-0F-R  hw:$$i-alu-op-dnl-0f-right)
(def-aluop K:SHIFT-DN-0F-L  hw:$$i-alu-op-dnl-0f-left)
(def-aluop K:SHIFT-DN-LF-R  hw:$$i-alu-op-dnl-lf-right)
(def-aluop K:SHIFT-DN-LF-L  hw:$$i-alu-op-dnl-lf-left)
(def-aluop K:SHIFT-UP-0F-R  hw:$$i-alu-op-upl-0f-right)
(def-aluop K:SHIFT-UP-0F-L  hw:$$i-alu-op-upl-0f-left)
(def-aluop K:SHIFT-UP-0F-LQ hw:$$i-alu-op-upl-0f-left-q)
(def-aluop K:SHIFT-UP-0F-RQ hw:$$i-alu-op-upl-0f-right-q)
(def-aluop K:SHIFT-UP-LF-R  hw:$$i-alu-op-upl-lf-right)
(def-aluop K:SHIFT-UP-LF-L  hw:$$i-alu-op-upl-lf-left)

(def-aluop K:NEG-L hw:$$i-alu-op-neg-left)
(def-aluop K:NEG-R hw:$$i-alu-op-neg-right)
(def-aluop K:ADD   hw:$$i-alu-op-add)
(def-aluop K:R+L   hw:$$i-alu-op-add)
(def-aluop K:L+R   hw:$$i-alu-op-add)
(def-aluop K:L+R+C hw:$$i-alu-op-addc)
(def-aluop K:L-R   hw:$$i-alu-op-sub)
(def-aluop K:L-R-C hw:$$i-alu-op-subc)
(def-aluop K:R-L   hw:$$i-alu-op-subr)
(def-aluop K:R-L-C hw:$$i-alu-op-subrc)
(def-aluop K:SETR  hw:$$i-alu-op-zero-ext-right)
(def-aluop K:SETL  hw:$$i-alu-op-zero-ext-left)
(def-aluop K:SEX-R hw:$$i-alu-op-sign-ext-right)
(def-aluop K:SEX-L hw:$$i-alu-op-sign-ext-left)
(def-aluop K:MERGE-L hw:$$i-alu-op-merge-right-left)
(def-aluop K:MERGE-R hw:$$i-alu-op-merge-left-right)

(def-aluop K:R+1  hw:$$i-alu-op-incr1-right)
(def-aluop K:R+2  hw:$$i-alu-op-incr2-right)
(def-aluop K:R+4  hw:$$i-alu-op-incr4-right)

(def-aluop K:R-1  hw:$$i-alu-op-decr1-right)
(def-aluop K:R-2  hw:$$i-alu-op-decr2-right)
(def-aluop K:R-4  hw:$$i-alu-op-decr4-right)

(def-aluop K:L+1  hw:$$i-alu-op-incr1-left)
(def-aluop k:L+2  hw:$$i-alu-op-incr2-left)
(def-aluop k:L+4  hw:$$i-alu-op-incr4-left)

(def-aluop K:L-1  hw:$$i-alu-op-decr1-left)
(def-aluop K:L-2  hw:$$i-alu-op-decr2-left)
(def-aluop K:L-4  hw:$$i-alu-op-decr4-left)

;;; Signed multiplication
(def-aluop K:SMUL-FIRST hw:$$i-alu-op-smul-first)
(def-aluop K:SMUL-STEP  hw:$$i-alu-op-smul-step)

;;; Unsigned multiplication
(def-aluop K:UMUL-FIRST hw:$$i-alu-op-umul-first)
(def-aluop K:UMUL-STEP  hw:$$i-alu-op-umul-step)
(def-aluop K:UMUL-LAST  hw:$$i-alu-op-umul-last)

;;; Division
(def-aluop K:SDIV-FIRST hw:$$i-alu-op-sdiv-first)
(def-aluop K:SDIV-STEP  hw:$$i-alu-op-sdiv-step)
(def-aluop K:SDIV-LAST1 hw:$$i-alu-op-sdiv-last1)
(def-aluop K:SDIV-LAST2 hw:$$i-alu-op-sdiv-last2)
(def-aluop K:REM-CORR   hw:$$i-alu-op-remcorr)
(def-aluop K:QUO-CORR   hw:$$i-alu-op-quocorr)

;;; Multi-precision division
(def-aluop K:MP-DIV-STEP1   hw:$$i-alu-op-mp-div-step1)
(def-aluop K:MP-DIV-STEP2   hw:$$i-alu-op-mp-div-step2)
(def-aluop K:MP-SDIV-STEP3  hw:$$i-alu-op-mp-s-div-step3)
(def-aluop K:MP-UDIV-STEP3  hw:$$i-alu-op-mp-u-div-step3)

(def-aluop K:PRIORITIZE-R hw:$$i-alu-op-prior-right)
(def-aluop K:PRIORITIZE-L hw:$$i-alu-op-prior-left)

(def-aluop K:ROTATE-L hw:$$i-alu-op-nb-rot-left)
(def-aluop K:ROTATE-R hw:$$i-alu-op-nb-rot-right)
(def-aluop K:NB-SHIFT-AR-L  hw:$$i-alu-op-nb-sn-sh-left)
(def-aluop K:NB-SHIFT-AR-R  hw:$$i-alu-op-nb-sn-sh-right)
(def-aluop K:NB-SHIFT-0F-L  hw:$$i-alu-op-nb-0f-sh-left)
(def-aluop K:NB-SHIFT-0F-R  hw:$$i-alu-op-nb-0f-sh-right)

(def-aluop K::SET-BIT-LEFT       hw:$$i-alu-op-set-bit-left)
(def-aluop K::SET-BIT-RIGHT      hw:$$i-alu-op-set-bit-right)
(def-aluop K::RESET-BIT-LEFT     hw:$$i-alu-op-rst-bit-left)
(def-aluop K::RESET-BIT-RIGHT    hw:$$i-alu-op-rst-bit-right)

(def-aluop K::EXTRACT-BIT-RIGHT  hw:$$i-alu-op-ext-bit-from-right)
(def-aluop K::EXTRACT-BIT-LEFT   hw:$$i-alu-op-ext-bit-from-left)

(def-aluop K::SET-BIT-STATUS     hw:$$i-alu-op-set-bit-stat)
(def-aluop K::RESET-BIT-STATUS   hw:$$i-alu-op-rst-bit-stat)
(def-aluop K::EXTRACT-BIT-STATUS hw:$$i-alu-op-ext-bit-stat)

(def-aluop K::ALIGNED-FIELD-NOT-LEFT   hw:$$i-alu-op-not-f-al-left)
(def-aluop K::ALIGNED-FIELD-PASS-LEFT  hw:$$i-alu-op-pass-f-al-left)
(def-aluop K::ALIGNED-FIELD-IOR        hw:$$i-alu-op-or-f-al-left)
(def-aluop K::ALIGNED-FIELD-XOR        hw:$$i-alu-op-xor-f-al-left)
(def-aluop K::ALIGNED-FIELD-AND        hw:$$i-alu-op-and-f-al-left)
(def-aluop K::ALIGNED-FIELD-NOT-RIGHT  hw:$$i-alu-op-not-f-al-right)
(def-aluop K::ALIGNED-FIELD-PASS-RIGHT hw:$$i-alu-op-pass-f-al-right)

(def-aluop K:FIELD-NOT  hw:$$i-alu-op-not-f-left)
(def-aluop K:FIELD-PASS hw:$$i-alu-op-pass-f-left)
(def-aluop K:FIELD-OR   hw:$$i-alu-op-or-f-left)
(def-aluop K:FIELD-XOR  hw:$$i-alu-op-xor-f-left)
(def-aluop K:FIELD-AND  hw:$$i-alu-op-and-f-left)

(def-aluop K:FIELD-EXTRACT-L  hw:$$i-alu-op-ext-f-left)
(def-aluop K:FIELD-EXTRACT-R  hw:$$i-alu-op-ext-f-right)
(def-aluop K:FIELD-EXTRACT-LR hw:$$i-alu-op-ext-f-left-right)
(def-aluop K:FIELD-EXTRACT-RL hw:$$i-alu-op-ext-f-right-left)

;----------------------------------------------------------------
;;; Floating point operations

(def-symbol-tables fpuop)

(def-fpuop K:single-add         hw:$$falu-single-add)
(def-fpuop K:single-subtract    hw:$$falu-single-subtract)
(def-fpuop K:single-multiply    hw:$$fmul-single-multiply)
(def-fpuop k:single-divide      hw:$$fmul-single-divide)
(def-fpuop k:single-compare     hw:$$falu-single-compare)
(def-fpuop k:single-negate      hw:$$falu-single-negate)
(def-fpuop k:single-test        hw:$$falu-single-test)
(def-fpuop k:single-fix         hw:$$falu-single-fix)
(def-fpuop k:single-float       hw:$$falu-single-float)
(def-fpuop k:single-to-double   hw:$$falu-single-to-double)

(def-fpuop K:double-add         hw:$$falu-double-add)
(def-fpuop K:double-subtract    hw:$$falu-double-subtract)
(def-fpuop K:double-multiply    hw:$$fmul-double-multiply)
(def-fpuop k:double-divide      hw:$$fmul-double-divide)
(def-fpuop k:double-compare     hw:$$falu-double-compare)
(def-fpuop k:double-negate      hw:$$falu-double-negate)
(def-fpuop k:double-test        hw:$$falu-double-test)
(def-fpuop k:double-fix         hw:$$falu-double-fix)
(def-fpuop k:double-float       hw:$$falu-double-float)
(def-fpuop k:double-to-single   hw:$$falu-double-to-single)

(def-fpuop k:fpu-default-mode0  hw:$$fpu-mode-0)
(def-fpuop k:fpu-default-mode1  hw:$$fpu-mode-1)
(def-fpuop k:fpu-default-mode2  hw:$$fpu-mode-2)
(def-fpuop k:fpu-default-mode3  hw:$$fpu-mode-3)

(def-symbol-tables fpuload)

(def-fpuload k:fpu-load-mode    hw:$$fpu-load-mode)
(def-fpuload k:fpu-load-xy      hw:$$fpu-load-xy-go)
(def-fpuload k:fpu-load-x       hw:$$fpu-load-x-go)
(def-fpuload k:fpu-load-y       hw:$$fpu-load-y)
(def-fpuload k:fpu-load-nop     hw:$$fpu-load-nop)

(def-symbol-tables fpuunload)

(def-fpuunload k:fpu-unload-high hw:$$fpu-unload-high)
(def-fpuunload k:fpu-unload-low  hw:$$fpu-unload-low)
;----------------------------------------------------------------
;;; Options

(def-symbol-table field-option)

(defmacro def-option (name field value)
  `(def-field-option ,name (cons ,field ,value)))

(def-option K:ITRAP-0       hw:%%i-trap-bit 0)
(def-option K:ITRAP-1       hw:%%i-trap-bit 1)

(def-option K:STAT-0        hw:%%i-stat-bit 0)
(def-option K:STAT-1        hw:%%i-stat-bit 1)

(def-option K:CARRY-0       hw:%%i-macro-carry-bit 0)
(def-option K:CARRY-1       hw:%%i-macro-carry-bit 1)

(def-option K:BOXED-OUTREG0  hw:%%i-boxed hw:$$i-boxed-outreg0)
(def-option K:BOXED-LEFT     hw:%%i-boxed hw:$$i-boxed-left)
(def-option K:BOXED-RIGHT    hw:%%i-boxed hw:$$i-boxed-right)
(def-option K:UNBOXED        hw:%%i-boxed hw:$$i-boxed-unboxed)
(def-option K:BOXED          hw:%%i-boxed hw:$$i-boxed-boxed)

(def-option K:BOXED-MD       hw:%%i-md-boxed  hw:$$i-md-boxed)
(def-option K:UNBOXED-MD     hw:%%i-md-boxed  hw:$$i-md-unboxed)
(def-option K:BOXED-VMA      hw:%%i-vma-boxed hw:$$i-vma-boxed)
(def-option K:UNBOXED-VMA    hw:%%i-vma-boxed hw:$$i-vma-unboxed)

(def-option K:DT-0          hw:%%i-dtp-check 0.)
(def-option K:DT-1          hw:%%i-dtp-check 1.)
(def-option K:DT-2          hw:%%i-dtp-check 2.)
(def-option K:DT-3          hw:%%i-dtp-check 3.)
(def-option K:DT-4          hw:%%i-dtp-check 4.)
(def-option K:DT-5          hw:%%i-dtp-check 5.)
(def-option K:DT-6          hw:%%i-dtp-check 6.)
(def-option K:DT-7          hw:%%i-dtp-check 7.)
(def-option K:DT-NONE                           hw:%%i-dtp-check vinc:$$dtc-none)
(def-option K:DT-HAIRY-NUMBER                   hw:%%i-dtp-check vinc:$$dtc-hairy-number)
(def-option K:DT-BOTH-CHARACTER                 hw:%%i-dtp-check vinc:$$dtc-both-character)
(def-option K:DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE hw:%%i-dtp-check vinc:$$dtc-right-array-and-left-structure)
(def-option K:DT-RIGHT-LIST                     hw:%%i-dtp-check vinc:$$dtc-right-list)
(def-option K:DT-BOTH-FIXNUM                    hw:%%i-dtp-check vinc:$$dtc-both-fixnum)
(def-option K:DT-BOTH-FIXNUM-WITH-OVERFLOW      hw:%%i-dtp-check vinc:$$dtc-both-fixnum-with-overflow)


(def-option K:CH-NOOP           hw:%%i-chop hw:$$i-chop-nop)
(def-option K:CH-OPEN           hw:%%i-chop hw:$$i-chop-open)
(def-option K:CH-CALL           hw:%%i-chop hw:$$i-chop-call)
(def-option K:CH-OPEN-CALL      hw:%%i-chop hw:$$i-chop-open-call)
(def-option K:CH-RETURN         hw:%%i-chop hw:$$i-chop-return)
(def-option K:CH-TAIL-OPEN      hw:%%i-chop hw:$$i-chop-topen)
(def-option K:CH-TAIL-CALL      hw:%%i-chop hw:$$i-chop-tcall)
(def-option K:CH-TAIL-OPEN-CALL hw:%%i-chop hw:$$i-chop-topen-call)

;these apply to byte operations
(def-option K:BW-32 hw:%%i-bw hw:$$i-bw-32)
(def-option K:BW-8  hw:%%i-bw hw:$$i-bw-8)
(def-option K:BW-16 hw:%%i-bw hw:$$i-bw-16)
(def-option K:BW-24 hw:%%i-bw hw:$$i-bw-24)

;these apply to bit operations
(def-option K:PW-II hw:%%i-pw hw:$$i-pw-ii)     ;both width and position from instruction
(def-option K:PW-IR hw:%%i-pw hw:$$i-pw-ir)     ;position from instruction, width from register
(def-option K:PW-RI hw:%%i-pw hw:$$i-pw-ri)     ;position from register, width from instruction
(def-option K:PW-RR hw:%%i-pw hw:$$i-pw-rr)     ;both width and position from register

(def-option K:NEXT-PC-DISPATCH hw:%%i-next-pc hw:$$i-next-pc-dispatch)
(def-option K:NEXT-PC-RETURN   hw:%%i-next-pc hw:$$i-next-pc-return)
(def-option K:NEXT-PC-PC+1     hw:%%i-next-pc hw:$$i-next-pc-pc+1)

(defun options (options inst)
  (dolist (option options)
    (let ((f-v (get-field-option option)))
      (setq inst
            (set-fields
              (car f-v) (cdr f-v)
              inst))))
  inst)



;----------------------------------------------------------------
;;; Conditions

(def-symbol-tables jcond)

;;; A condition can be a field or an option
(defmacro def-jump-cond (name value)
  `(progn
     (def-jcond ,name ,value)
     (def-option ,name hw:%%i-jcond ,value)))


(def-jump-cond K:BR-ALWAYS               hw:$$i-jcond-uncond)
(def-jump-cond K:BR-JINDIR               hw:$$i-jcond-indir)
(def-jump-cond K:BR-EQUAL                hw:$$i-jcond-eq)
(def-jump-cond K:BR-ZERO                 hw:$$i-jcond-eq)
(def-jump-cond K:BR-NOT-EQUAL            hw:$$i-jcond-neq)
(def-jump-cond K:BR-NOT-ZERO             hw:$$i-jcond-neq)
(def-jump-cond K:BR-GREATER-THAN         hw:$$i-jcond-gt)
(def-jump-cond K:BR-NOT-LESS-OR-EQUAL    hw:$$i-jcond-gt)
(def-jump-cond K:BR-POSITIVE             hw:$$i-jcond-gt)
(def-jump-cond K:BR-NOT-GREATER-OR-EQUAL hw:$$i-jcond-lt)
(def-jump-cond K:BR-LESS-THAN            hw:$$i-jcond-lt)
(def-jump-cond K:BR-NEGATIVE             hw:$$i-jcond-lt)
(def-jump-cond K:BR-GREATER-OR-EQUAL     hw:$$i-jcond-ge)
(def-jump-cond K:BR-NOT-LESS-THAN        hw:$$i-jcond-ge)
(def-jump-cond K:BR-NOT-NEGATIVE         hw:$$i-jcond-ge)
(def-jump-cond K:BR-LESS-OR-EQUAL        hw:$$i-jcond-le)
(def-jump-cond K:BR-NOT-GREATER-THAN     hw:$$i-jcond-le)
(def-jump-cond K:BR-NOT-POSITIVE         hw:$$i-jcond-le)



;----------------------------------------------------------------
;;; Fields

(defun destination (dest inst)
  (let ((fd (get-fdest (if (consp dest) (car dest) dest))))
    (if fd
        (let ((i (set-fields
                   hw:%%i-destination fd
                   inst)))
          (if (consp dest)
              (options (cdr dest) i)
            i))
      (get-src-or-dest dest hw:%%i-destination inst))))

(defun right-source (src inst)
  (let ((fs (get-fsource src)))
    (if fs
        (set-fields
          hw:%%i-right-source fs
          inst)
      (get-src-or-dest src hw:%%i-right-source inst))))

(defun left-source (src inst)
  (get-src-or-dest src hw:%%i-left-source inst))

(defvar *last-global-register-frame-number* :unbound)

(defmacro with-global-register-frame-checking (&rest code)
  (declare (zwei:indentation 1 2))
  `(let ((*last-global-register-frame-number* ()))
     ,@code))

(defmacro assure-global-registers-in-same-frame ((frame) &rest body)
  (declare (zwei:indentation 1 2))
  (let ((frame-name (gensym)))
    `(let ((,frame-name ,frame))
       (cond ((null *last-global-register-frame-number*)
              (setq *last-global-register-frame-number* ,frame-name))
             ((not (= ,frame-name *last-global-register-frame-number*))
              (cerror "Assemble instruction bogusly"
                      "Global registers not in the same frame")))
       ,@body)))

(export 'K:REGISTER 'K)

(defun get-src-or-dest (name field inst)
  (cond ((symbolp name)
         (let ((reg (get-reg name)))
           (cond
             (reg
              (nlisp:dpb reg field inst))
             (t (setq reg (get name :register))
                (assure-global-registers-in-same-frame ((second reg))
                   (cond (reg
                          (set-fields
                            hw:%%i-global-frame (second reg)
                            field (set-fields
                                    hw:%%i-reg-base   hw:$$i-reg-base-global
                                    hw:%%i-reg-offset (third reg)
                                    0)
                            inst))
                         (t (cerror "zero field" "The source or destination ~s is unknown" name)
                            (nlisp:dpb 0 field inst))))))))
        ((consp name)
         (case (car name)
           (K:REGISTER
;           (if (or (zerop (nlisp:ldb hw:%%i-global-frame inst))   ;maybe 0 is valid?
;                   (= (nlisp:ldb hw:%%i-global-frame inst)
;                      (third name)))
            (assure-global-registers-in-same-frame ((third name))
               (set-fields
                 hw:%%i-global-frame (third name)
                 field (set-fields
                         hw:%%i-reg-base hw:$$i-reg-base-global
                         (fourth name))                       ;offset
                 inst)))
;             (cerror "xxx" "global registers not in same frame")))
           (t (cerror "zero field" "The source or destination ~s is unknown" name)
              (nlisp:dpb 0 field inst))))
        (t (cerror "zero field" "The source or destination ~s is unknown" name)
           (nlisp:dpb 0 field inst))))


(defun aluop (name inst)
  (let ((aluop (get-aluop name)))
    (if aluop
        (set-fields
          hw:%%i-alu-op aluop
          inst)
      (progn
        (cerror "use zero" "unknown alu operation: ~s" name)
        (set-fields
          hw:%%i-alu-op 0
          inst)))))

(defun alui-op (name inst)
  (let ((aluop (get-aluop name)))
    (if aluop
        (set-fields
          hw:%%i-alui-op aluop
          inst)
      (progn
        (cerror "use zero" "unknown alu operation: ~s" name)
        (set-fields
          hw:%%i-alui-op 0
          inst)))))


(defun alu (alu inst)
  (if alu
      (let ((movep (eq (first alu) 'K:MOVE))
            aluop destination left right options)
        (if movep
            (setq aluop       'K:SETR
                  destination (second alu)
                  left        'K:A0
                  right       (third alu)
                  options     (cdddr alu))
            (setq aluop       (second alu)
                  destination (third alu)
                  left        (fourth alu)
                  right       (fifth alu)
                  options     (nthcdr 5 alu)))
        (options options
           (destination destination
              (aluop aluop
                 (left-source left
                    (right-source right
                                  (if movep
                                      ;; moves default to having boxed right
                                      (set-fields
                                        hw:%%i-boxed hw:$$i-boxed-right
                                        inst)
                                    inst)))))))
    ;; defaults to nop
    inst))


(defun fpu-op (name inst)
  (let ((fpuop (get-fpuop name)))
    (if fpuop
        (set-fields
          hw:%%i-fpu-function fpuop
          inst)
      (progn
        (cerror "use zero" "unknown fpu function: ~s" name)
        (set-fields
          hw:%%i-fpu-function 0
          inst)))))

(defun fpu-load (name inst)
  (let ((fpuload (get-fpuload name)))
    (if fpuload
        (set-fields
          hw:%%i-fpu-load fpuload
          inst)
      (progn
        (cerror "use zero" "unknown fpu load operation: ~s" name)
        (set-fields
          hw:%%i-fpu-load 0
          inst)))))

(defun fpu-unload (name inst)
  (let ((fpuunload (get-fpuunload name)))
    (if fpuunload
        (set-fields
          hw:%%i-fpu-unload fpuunload
          inst)
      (progn
        (cerror "use zero" "unknown fpu unload operation: ~s" name)
        (set-fields
          hw:%%i-fpu-unload 0
          inst)))))

(defun condition (name inst)
  (set-fields
    hw:%%i-jcond (get-jcond name)
    inst))


(defun local-ref (address inst)
 (cond ((numberp address)
        (nlisp:dpb address hw:%%i-branch-address inst))
       (t (push (cons *location-counter* address) *local-refs*)
          inst)))

(defun long-local-ref (address inst)
  (push (list *location-counter* address) *local-refs*)
  inst)

(defun external-ref (address inst)
  (let ((nargs 0))
    (when (consp address)
      (setq nargs (second address))
      (setq address (first address)))
    (cond ((numberp address)
           (nlisp:dpb address hw:%%i-jump-address inst))
          ((or (symbolp address)
               (and (consp address)
                    (eq (car address) :internal))
               ;; klugey
               (ncompiled-function-p address))
           (push (list *location-counter* address nargs) *refs*)
           inst)
          (t (bug "Bad address: ~s" address)))))

(defun load-time-form (form inst)
  (push (cons *location-counter* (cadr form)) *load-time-evals*)
  inst)

(defun dispatch-nargs (nargs inst)
  (declare (ignore nargs))
  ;; save nargs info
  inst)

(export '(K::NEW-OPEN K::NEW-TAIL-OPEN) 'k)

(defun set-call-rdest-global (inst frame offset)
  (set-fields
    hw:%%i-global-frame frame
    hw:%%i-call-dret-6   0
    hw:%%i-call-dret-5-4 hw:$$i-reg-base-global
    hw:%%i-call-dret-3-1 (ldb (byte 3. 1.) offset)
    hw:%%i-call-dret-0   (ldb (byte 1. 0.) offset)
    inst))

(defun call-return-dest (rdest inst &aux reg)
  (cond ((consp rdest)
         (ecase (car rdest)
           ((K:NEW-OPEN K:NEW-TAIL-OPEN)
            (let ((o-reg (cadr rdest)))
              (set-fields
                hw:%%i-call-dret-6   1
                hw:%%i-call-dret-5   (if (eq (car rdest) 'K:NEW-TAIL-OPEN)
                                         1 0)
                hw:%%i-call-dret-3-1 (ldb (byte 3. 1.) o-reg)
                hw:%%i-call-dret-0   (ldb (byte 1. 0.) o-reg)
                inst)))
           (K:REGISTER
            (assure-global-registers-in-same-frame ((third rdest))
               (set-call-rdest-global inst (third rdest) (fourth rdest))))))
        ((setq reg (get-reg rdest))
         (set-fields
           hw:%%i-call-dret-6   0
           hw:%%i-call-dret-5-1 (ldb (byte 5. 1.) reg)
           hw:%%i-call-dret-0   (ldb (byte 1. 0.) reg)
           inst))
        ((setq reg (get rdest :register))
         (assure-global-registers-in-same-frame ((second reg))
            (set-call-rdest-global inst (second reg) (third reg))))
        (t (cerror "foo" "unknown call return destination: ~s" rdest))))



(defun callz-return-dest (rdest inst)
  (if (and (consp rdest)
           (member (car rdest) '(K:NEW-OPEN K:NEW-TAIL-OPEN)))
      (let ((o-reg (cadr rdest)))
        (set-fields
          hw:%%i-callz-dret-6   1
          hw:%%i-callz-dret-5   (if (eq (car rdest) 'K:NEW-TAIL-OPEN)
                                   1 0)
          hw:%%i-callz-dret-3   (ldb (byte 1. 3.) o-reg)
          hw:%%i-callz-dret-2-0 (ldb (byte 2. 0.) o-reg)
          inst))
    (let ((reg (get-reg rdest)))
      (set-fields
        hw:%%i-callz-dret-6   0
        hw:%%i-callz-dret-5-3 (ldb (byte 5. 3.) reg)
        hw:%%i-callz-dret-2-0 (ldb (byte 2. 0.) reg)
        inst))))


(defun call-move (move inst)
  (cond ((null move) inst)
        ((consp move)
         (destination (car move) (right-source (cadr move) (options (cddr move) inst))))
        (t (cerror "foo" "unknown value for call-move: ~a" move))))



(defun immediate-32 (imm inst)
  (let ((value (if (integerp imm)
                   (progn
                     (setq inst
                           (set-fields
                             ;hw:%%i-imm32-data imm
                             ;; this is not right for vma/md
                             ;hw:%%i-boxed      hw:$$i-boxed-unboxed
                             inst))
                     imm)
                 (progn
                   (setq imm (eval imm))
                   (cond ((typep imm '(signed-byte #.(prims:byte-size vinc:%%fixnum-field)))
                          (set-fields
                            vinc:%%data-type    vinc:$$dtp-fixnum
                            vinc:%%fixnum-field imm
                            0))
                         ((characterp imm)
                          (nlisp:dpb vinc:$$dtp-character vinc:%%data-type (char-int imm)))
                         ((eq imm NIL)
                          (nlisp:dpb vinc:$$dtp-nil vinc:%%data-type 0))
                          ;(set-fields
                          ;  vinc:%%data-type vinc:$$dtp-nil
                          ;  ;vinc:%%pointer   0
                          ;  0))
                         ((eq imm T)
                          (nlisp:dpb vinc:$$dtp-symbol vinc:%%data-type 5))
                          ;(set-fields
                          ;  vinc:%%data-type vinc:$$dtp-symbol
                          ;  ;vinc:%%pointer   5
                          ;  5))
                         ((and (consp imm)
                               (eq (car imm) 'hw:unboxed-constant))
                          (second imm))
                         ((eql imm 0.0s0)
                          ;; The bit pattern for 0.0s0 on the K
                          0)
                         ((eql imm -0.0s0)
                          ;; The bit pattern for -0.0s0 on the K.  Can't happen if we're
                          ;; compiling on the Lambda, because the Lambda doesn't have them!
                          (nlisp:dpb 1 hw:%%short-float-sign 0))
                         ((typep imm 'short-float)
                          (multiple-value-bind (fraction exponent sign-flonum)
                              (integer-decode-float imm)
                            (let* ((exponent (+ exponent hw:$$short-float-exponent-excess
                                                (prims:byte-size hw:%%short-float-mantissa) -1))
                                   (mantissa (dpb fraction (byte 16 1) 0))) ;;Make high bit hidden.
                              (check-type mantissa (unsigned-byte #.(1+ (prims:byte-size hw:%%short-float-mantissa))))
                              (check-type exponent (unsigned-byte #.(prims:byte-size hw:%%short-float-exponent)))
                              ;; Construct a K short float, independent of original
                              ;; machine
                              (nlisp:dpb vinc:$$dtp-short-float vinc:%%data-type
                                         (nlisp:dpb (if (< sign-flonum 0) 1 0)
                                                    hw:%%short-float-sign
                                                    (nlisp:dpb exponent
                                                               hw:%%short-float-exponent
                                                               (nlisp:dpb
                                                                 mantissa
                                                                 hw:%%short-float-mantissa
                                                                 0)))))))
                         (t (push (cons *location-counter* imm) *immediates*)
                            0))))))
    (logxor inst (logand #xFFFFFFFF (logxor value inst)))
;   (set-fields
;     hw:%%i-imm32-data value
;     inst)
    ))

(defun immediate-16 (imm inst)
  (set-fields
    hw:%%i-imm16-data (eval imm)
    inst))


(defun byte-spec (byte-spec inst)
  (let ((bs (eval byte-spec)))
    (unless (numberp bs)
      (cerror "0" "Bad byte spec: ~s" byte-spec)
      (setq bs 0))
    (set-fields
      hw:%%i-alu-shift (prims:byte-position bs)
      hw:%%i-alu-mask  (prims:byte-size bs)
      inst)))


;----------------------------------------------------------------
;;; Instructions

(def-symbol-table instruction)

(defmacro def-inst (name fields &rest base-instruction)
  (let ((fname (gensym name)))
    `(progn
       (defun ,fname (,@fields &rest options)
         ,(do ((fields fields (cdr fields))
               (form `(options options
                               (set-fields . ,base-instruction))
                     `(,(car fields) ,(car fields) ,form)))
              ((null fields) form)))
       (setf (get ',name 'fields) ',fields)
       (def-instruction ,name #',fname))))

(defconstant instruction-default
    (set-fields
          hw:%%i-x16         hw:$$i-x16-nil
          hw:%%i-next-pc     hw:$$i-next-pc-pc+1
          hw:%%i-boxed       hw:$$i-boxed-unboxed
          hw:%%i-dtp-check   0  ;vinc:$$i-dcheck-nop
          hw:%%i-chop        hw:$$i-chop-nop
          hw:%%i-destination hw:$$i-fd-nop
          0))

(defconstant nop-instruction
    (set-fields
      hw:%%i-right-source random-source
      hw:%%i-left-source  random-source
      hw:%%i-alu-op       hw:$$i-alu-op-pass-stat   ;doesn't set condition codes

      instruction-default))

;;;; The Instructions

(def-inst K:NOP  ()
          nop-instruction)

(def-inst K:MEMORY-WAIT  ()
          nop-instruction)

(def-inst K:MOVE (destination right-source)
          hw:%%i-op-code hw:$$i-op-code-alu
          hw:%%i-alu-op  hw:$$i-alu-op-zero-ext-right
          hw:%%i-boxed   hw:$$i-boxed-right
          instruction-default)

(def-inst K:ALU (aluop destination left-source right-source)
          hw:%%i-op-code hw:$$i-op-code-alu
          instruction-default)

(def-inst K:ALU-FIELD (aluop destination left-source right-source byte-spec)
          hw:%%i-op-code hw:$$i-op-code-alu
          instruction-default)

(def-inst K:MOVEI (destination immediate-32)
          hw:%%i-op-code hw:$$i-op-code-loadi-32
          instruction-default)

(def-inst K:MOVEA (destination external-ref)
          vinc:%%data-type  vinc:$$dtp-code
          hw:%%i-op-code    hw:$$i-op-code-loadi-32
          instruction-default)

(def-inst K:MOVEI-LOAD-TIME (destination load-time-form)
          vinc:%%data-type    vinc:$$dtp-unbound                ;replaced during fasl
          vinc:%%fixnum-field *location-counter*                ;bugstopper -- value unused -- overwritten later
          hw:%%i-op-code      hw:$$i-op-code-loadi-32
          instruction-default)

(def-inst K:MOVE-PC (destination long-local-ref)
          hw:%%i-boxed      hw:$$i-boxed-boxed
          hw:%%i-op-code    hw:$$i-op-code-loadi-32
          instruction-default)

(def-inst K:ALUI-16 (alui-op destination left-source immediate-16)
          hw:%%i-op-code hw:$$i-op-code-alui
          instruction-default)

(def-inst K:LOADI (destination immediate-32)
          hw:%%i-op-code hw:$$i-op-code-loadi-32
          instruction-default)

(def-inst K:RETURN (right-source)
          hw:%%i-next-pc     hw:$$i-next-pc-return
          hw:%%i-op-code     hw:$$i-op-code-alu
          hw:%%i-chop        hw:$$i-chop-return
          hw:%%i-destination hw:$$i-fd-return
          hw:%%i-alu-op      hw:$$i-alu-op-zero-ext-right
          hw:%%i-boxed       hw:$$i-boxed-right
          instruction-default)

(def-inst K:RETURN-MV (right-source)
          hw:%%i-next-pc     hw:$$i-next-pc-return
          hw:%%i-op-code     hw:$$i-op-code-alu
          hw:%%i-chop        hw:$$i-chop-return
          hw:%%i-destination hw:$$i-fd-return-mv
          hw:%%i-alu-op      hw:$$i-alu-op-zero-ext-right
          hw:%%i-boxed       hw:$$i-boxed-right
          instruction-default)

(def-inst K:RETURN-TAIL (right-source)
          hw:%%i-next-pc     hw:$$i-next-pc-return
          hw:%%i-op-code     hw:$$i-op-code-alu
          hw:%%i-chop        hw:$$i-chop-return
          hw:%%i-destination hw:$$i-fd-return-tail
          hw:%%i-alu-op      hw:$$i-alu-op-zero-ext-right
          hw:%%i-boxed       hw:$$i-boxed-right
          instruction-default)

(def-inst K:RETURNI (immediate-32)
          hw:%%i-next-pc     hw:$$i-next-pc-return
          hw:%%i-op-code     hw:$$i-op-code-loadi-32
          hw:%%i-chop        hw:$$i-chop-return
          hw:%%i-destination hw:$$i-fd-return
          instruction-default)

(def-inst K:RETURNI-MV (immediate-32)
          hw:%%i-next-pc     hw:$$i-next-pc-return
          hw:%%i-op-code     hw:$$i-op-code-loadi-32
          hw:%%i-chop        hw:$$i-chop-return
          hw:%%i-destination hw:$$i-fd-return-mv
          instruction-default)

(def-inst K:RETURNI-TAIL (immediate-32)
          hw:%%i-next-pc     hw:$$i-next-pc-return
          hw:%%i-op-code     hw:$$i-op-code-loadi-32
          hw:%%i-chop        hw:$$i-chop-return
          hw:%%i-destination hw:$$i-fd-return-tail
          instruction-default)

(def-inst K:TEST (condition)
          nop-instruction)

(def-inst K:BRANCH (local-ref alu)
          hw:%%i-cond    hw:$$i-cond-conditional-branch
          hw:%%i-op-code hw:$$i-op-code-alu
          hw:%%i-next-pc hw:$$i-next-pc-ir
          ;; alu operation defaults to NOP
          nop-instruction)

(def-inst K:UNCONDITIONAL-BRANCH (local-ref alu)
          hw:%%i-cond    hw:$$i-cond-unconditional-branch
          hw:%%i-op-code hw:$$i-op-code-alu
          hw:%%i-next-pc hw:$$i-next-pc-ir
          ;; alu operation defaults to NOP
          nop-instruction)


(def-inst K:OPEN ()
          hw:%%i-chop  hw:$$i-chop-open
          nop-instruction)

(def-inst K:TAIL-OPEN ()
          hw:%%i-chop  hw:$$i-chop-topen
          nop-instruction)

(defconstant call-instruction-default
             (set-fields
               hw:%%i-op-code      hw:$$i-op-code-move
               hw:%%i-right-source random-source
               hw:%%i-left-source  random-source
               hw:%%i-next-pc      hw:$$i-next-pc-ir
               hw:%%i-boxed        hw:$$i-boxed-right
               instruction-default))

(def-inst K:KCALL (external-ref call-return-dest call-move)
          hw:%%i-chop        hw:$$i-chop-call
          call-instruction-default)

(def-inst K:CALL (external-ref call-return-dest call-move)
          hw:%%i-chop        hw:$$i-chop-call
          call-instruction-default)

(def-inst K:OPEN-CALL (external-ref call-return-dest call-move)
          hw:%%i-chop        hw:$$i-chop-open-call
          call-instruction-default)

(def-inst K:TAIL-CALL (external-ref call-move)
          hw:%%i-chop        hw:$$i-chop-tcall
          call-instruction-default)

(def-inst K:OPEN-TAIL-CALL (external-ref call-move)
          hw:%%i-chop        hw:$$i-chop-topen-call
          call-instruction-default)

(def-inst K:TAIL-OPEN-CALL (external-ref call-move)
          hw:%%i-chop        hw:$$i-chop-topen-call
          call-instruction-default)


(def-inst K:JUMP (external-ref call-move)
          hw:%%i-cond    hw:$$i-cond-unconditional-branch
          hw:%%i-chop    hw:$$i-chop-nop
          call-instruction-default)

(def-inst K:JUMP-CONDITIONAL (external-ref call-move)
          hw:%%i-cond    hw:$$i-cond-conditional-branch
          hw:%%i-chop    hw:$$i-chop-nop
          call-instruction-default)


;;; the alu operation here can be slightly more because
;;; a shift field is included
(def-inst K:CALL-DISPATCH (dispatch-nargs callz-return-dest alu)
          hw:%%i-next-pc   hw:$$i-next-pc-dispatch
          hw:%%i-chop      hw:$$i-chop-call
          nop-instruction)

(def-inst K:TAIL-CALL-DISPATCH (dispatch-nargs alu)
          hw:%%i-next-pc   hw:$$i-next-pc-dispatch
          hw:%%i-chop      hw:$$i-chop-tcall
          nop-instruction)

(def-inst K:FALU (fpu-op destination left-source right-source fpu-load fpu-unload)
          hw:%%i-op-code hw:$$i-op-code-fp-alu
          instruction-default)

(def-inst K:FMUL (fpu-op destination left-source right-source fpu-load fpu-unload)
          hw:%%i-op-code hw:$$i-op-code-fp-mult
          instruction-default)

;----------------------------------------------------------------
;;; The Assembler


(defun assemble-inst (inst)
  (with-global-register-frame-checking
    (let ((assemble-function (get-instruction (car inst))))
      (if assemble-function
          (apply assemble-function (cdr inst))
        (error "Unknown instruction: ~a" inst)))))


(defun assemble-function-info (finfo)
  "Assemble function info, which is a list of NAME, INSTRUCTIONS
and ENTRY-POINTS, returning an ncompiled-function containing:

     CODE:         a sequence of numeric instructions
     ENTRY-POINTS: a vector of entry points:
                    #(<no-of-args> <offset-of-inst> ...)
                   <no-of-args> is negative for a rest arg
     LOCAL-REFS:   a vector of local references:
                    #(<offset-of-branch-inst> <offset-of-tag> ...)
                   the tag offset is also in the instruction, so
                   this information is redundant
     REFS:         a vector of references
                    #(<offset-of-jump-or-call-inst> <fcn-name> <no-of-args> ...)
     IMMEDIATES:   a vector of constant references
                    #(<offset-of-inst> <constant> ...)

  An Example:

    (assemble-function-info
       '(FOO
         (FOO
            (ALU L-R NOP A0 A1)
            (TEST BR-EQUAL)
            (BRANCH DONE ())
            (ALU R+1 A0 A0 A0)
            (UNCONDITIONAL-BRANCH FOO ())
          DONE
            (RETURN A0))
         ((0 . FOO))))

    =>
    #<NCOMPILED-FUNCTION FOO 259>
      NAME:         FOO
      CODE:         (252375302549028864
                     270361148627357696
                      54188332153835525
                     270251162559655936
                    2360031341367529472
                     199427220559630336)
      ENTRY-POINTS: #(2 0)
      LOCAL-REFS:   #(4 0 2 5)
      REFS:         #()
      IMMEDIATES:   #()"
  (assemble-instruction-list (first finfo) (second finfo) (third finfo)))

(defun assemble-instruction-list (name instructions entry-points)
  (let ((*local-symbols* '())
        (*refs* '())
        (*load-time-evals* '())
        (*local-refs* '())
        (*immediates* '())
        (*assembly-constants* '())
        (*location-counter* 0)
        (code '()))
    (dolist (inst instructions)
       (cond ((consp inst)
              (push (assemble-inst inst) code)
              (incf *location-counter*))
             ((symbolp inst)
              (push (cons inst *location-counter*) *local-symbols*))))
    (setq code (nreverse code))
    ;;; munge local-ref symbols with offsets
    (dolist (local-ref *local-refs*)
      (let ((addr (assoc (if (listp (cdr local-ref))
                             (cadr local-ref)
                           (cdr local-ref)) *local-symbols*)))
        (if addr
          (progn
            (rplacd local-ref (if (listp (cdr local-ref)) (- (cdr addr)) (cdr addr)))
            ;; put addr offset in address field
            (let ((code-pt (nthcdr (car local-ref)
                                   code)))
              (rplaca code-pt
                      (set-fields
                        hw:%%i-branch-address (cdr addr)
                        (car code-pt)))))
          (format nil "~&Warning: unresolved local reference to label ~a" (cdr local-ref)))))
    (make-ncompiled-function
      :name name
      :code code
      :length (length code)
      :local-refs (vectorize-link-info *local-refs*)
      :refs       (vectorize-refs *refs*)
      :immediates (vectorize-link-info *immediates*)
      :load-time-evals (nreverse *load-time-evals*)
      :entry-points (vectorize-link-info
                      (mapcar #'(lambda (entry-pt)
                                  (let ((addr (assoc (cdr entry-pt) *local-symbols*)))
                                    (if addr
                                        (rplacd entry-pt (cdr addr))
                                      (error "~&Unknown entry point: ~s" entry-pt))))
                              entry-points)))))


(defun vectorize-link-info (info)
  (let ((vector (make-array (ash (length info) 1))))
    (do ((i 0 (+ i 2))
         (info info (cdr info)))
        ((null info))
      (setf (svref vector i) (caar info))
      (setf (svref vector (1+ i)) (cdar info)))
    vector))

(defun vectorize-refs (refs)
  (let ((vector (make-array (* (length refs) 3))))
    (do ((i 0 (+ i 3))
         (refs refs (cdr refs)))
        ((null refs))
      (let ((ref (car refs)))
        (setf (svref vector i) (first ref))
        (setf (svref vector (1+ i)) (second ref))
        (setf (svref vector (+ i 2)) (third ref))))
    vector))

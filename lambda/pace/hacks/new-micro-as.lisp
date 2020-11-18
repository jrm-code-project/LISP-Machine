;;; -*- Mode:LISP; Package:(MICRO GLOBAL); Base:8; Readtable:ZL -*-

;;; 2/21/86
;;; Pace Willisson at LMI
;;;

;;; This is an incremental micro assembler for LAMBDA.  I don't have
;;; time to write a lot of documentation about it, but I'll get some
;;; thoughts down before I stop working on it for a while.

;;; This assembler is mostly compatable with the syntax of the old
;;; assembler LAMLP (and before that CADRLP, I think.)  Someday, it may
;;; be expanded enough to replace LAMLP.

;;; At this point, the way to use it is to write a form like:
;;;
;;;    (define-micro-function car-if-list (l)
;;;      ((m-t) pdl-pop)
;;;      (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)))
;;;      ((vma-start-read) m-t)
;;;      (check-page-read)
;;;      (dispatch transport md)
;;;      ((m-t) md)
;;;      (popj))
;;;
;;; DEFINE-MICRO-FUNCTION is a macro that assembles the instructions into
;;; a relocatable format (very similar to the micro-compiler format, but
;;; not compatable) and arranges to install the instructions in control
;;; memory, and define the name as a DTP-U-ENTRY.  Currently, all of this
;;; happens at load time, but soon the assembly will happen at compile
;;; time.

;;; I haven't addressed the problems of warm boots, or of having such
;;; functions in saved bands.  We can depend on the micro-code-paging
;;; area to stay loaded during a warm boot on the lambda (this was not true
;;; for the cadr.)  However, we will need a BEFORE-COLD initialization
;;; to turn all of the incremental DTP-U-ENTRYs into regular functions
;;; (an undefine function or even NIL is OK.), and probably a SYSTEM
;;; initialization to put them back.

;;; By the way, there is a bug in the microcode (at least
;;; up to version 1371) that causes the MICRO-CODE-PAGING-AREA to be reloaded
;;; from the LMC file during a warm boot.  This makes breakpoints go away
;;; in initially loaded pagable microcode, but I guess wont affect
;;; incremental stuff.

;;; The way this thing allocates control and A memories is not compatable
;;; with the micro-compiler, so the pair should not be loaded into the
;;; same band.  We will be able to fix this later.

;;; The function MICRO-PRINT disassembles a MICRO-FUNCTION.  You need to
;;; have lambda diag loaded, and it is more interesting if you have
;;; the running symbol table loaded also.  (Yes, this means that the symbol
;;; table has to be loaded twice, once for IMICRO and once for LAMBDA-DIAG,
;;; that will change someday also.
;;; Here is a handy idiom for the lambda-diag symbol table:
;;;
;;;    (when (null (lam:lam-select-symbols-for-version-if-possible
;;;                  %microcode-version-number))
;;;      (lam:lam-load-ucode-symbols-for-version
;;;        %microcode-version-number))

;;; At the moment, files that use DEFINE-MICRO-FUNCTION should be in the MICRO
;;; package.

;;; The instructions you write are for a MISC instruction.  Look through the
;;; microcode for (misc-inst-entry ...) to see many examples.

;;; Of course, there are many unimplemented instructions.  Among the important
;;; are GC-WRITE-TEST and CHECK-PAGE-WRITE.  They will eventually be available.


;;; The major parts of IMICRO are:
;;;   process-instruction
;;;     uses the definitions in the file LAMBDA-COMPONENTS to build a numeric
;;;     instructions out of a symbolic list
;;;   micro-assemble-list
;;;     calls process-instruction on a whole program, and takes care of inserting
;;;     no-ops for micro-paging
;;;   the macro DEFINE-MICRO-FUNCTION
;;;     assembles the program and stores it on the property list of the symbol,
;;;     attempts to use RECORD-SOURCE-FILE-NAME to do the normal redefinition
;;;     warning thing, and calls INSTALL
;;;   INSTALL/UNINSTALL
;;;     these take care of control memory allocation, and allocation of slots in
;;;     the micro-code-entry-area and micro-code-symbol-area, and storing
;;;     programs in control memory
;;;   I-MEM-BLOCK allocator
;;;     this manages free control memory.  you can ask for blocks of any length,
;;;     and require any alignment.  for example, IMICRO always asks for blocks
;;;     that begin on micro-page boundaries.  the allocator can handle different
;;;     alignment requirements at the same time so that it can eventually be used
;;;     with the micro compiler too.
;;;   instruction macro processor
;;;     if the car of an instruction has a property MICRO:MICRO-MACRO, then it is
;;;     a function that is called with the whole instruction as an argument.
;;;     It returns a list of instructions to be used instead.
;;;   symbol macro processor
;;;     if a symbol in an instruction has the property MICRO:MICRO-EXPAND
;;;     it is a function to call, and it returns a list of symbols to be
;;;     spliced into the instruction instead.
;;;   a-constant allocator
;;;     this is code to evaluate A-CONSTANT forms, and find a location in a
;;;     memory that has that value (allocating one if necessary. - there are
;;;     only about 15 free slots in the current microcodes.)
;;;   micro function disassembler
;;;     this calls the lambda-diag instruction disassembler to print a micro-function.
;;;     if the instruction is installed in control memory, then it is printed
;;;     directly from there, otherwise it tries to find the code on the property
;;;     list.

;TODO:
;  get error if vma-start-read, etc not immediately followed by check-page-read in pagable code

;Major forms of instructions that I handle:
; (jump foo)
; (call foo)
; (jump-xct-next foo)
; (jump-less-than m-a a-b foo)
; (popj-less-than m-a a-b)
; (jump-if-bit-set (byte 1 5) m-a foo)

; ((m-a) dpb m-b (byte 1 0) a-c)
; ((m-a) ldb (byte 1 0) m-a)
; ((m-a) (byte 1 0) m-a)

; ((m-a) m-b)
; ((a-a) m-b)
; ((vma) m-b)
; ((m-a vma) m-b)
; ((m-a) vma)
; ((m-a) add m-b a-c)
; (pdl-pop)

; (dispatch (i-arg 1) d-xxx m-a (byte 1 0))

(defvar *previous-uinst-linked-to-next* nil)

(defstruct (i-mem-block (:type :named-array)
                        (:print "#<~s Start ~s Length ~s Function ~s ~s>"
                                (type-of i-mem-block)
                                (i-mem-block-start i-mem-block)
                                (i-mem-block-length i-mem-block)
                                (i-mem-block-function i-mem-block)
                                (%pointer i-mem-block)))
  i-mem-block-start
  i-mem-block-length
  i-mem-block-next
  i-mem-block-prev
  i-mem-block-function
  )

(defmacro define-micro-component (name options &body body)
  `(define-micro-component-1 ',name ',options ',body))

(defun define-micro-component-1 (name options body)
  (let ((fixed-fields 0)
        (fixed-fields-values 0)
        (required-fields nil))
    (do ((rest body (cddr rest)))
        ((null rest))
      (let ((field (car rest))
            (val (cadr rest)))
        (if (null (get field 'lam:constant))
            (ferror nil "unknown field: ~s" field))
        (cond ((or (integerp val)
                   (get val 'lam:constant))
               (setq fixed-fields (dpb -1 (symeval field) fixed-fields))
               (setq fixed-fields-values (dpb (eval val) (symeval field) fixed-fields-values)))
              ((eq val :required)
               (push field required-fields))
              (t
               (ferror nil "unknown option: ~s" val)))))
    (cond ((null (memq 'destination options))
           (putprop name fixed-fields 'fixed-fields)
           (putprop name fixed-fields-values 'fixed-fields-values)
           (putprop name required-fields 'required-fields))
          (t
           (putprop name fixed-fields 'destination-fixed-fields)
           (putprop name fixed-fields-values 'destination-fixed-fields-values)
           (putprop name required-fields 'destination-required-fields)))
    nil))


(defsignal bad-inst error () "syntax error")


(defun expand (form)
  (cond ((null form) nil)
        ((symbolp form)
         (cond ((get form 'micro-expand)
                (values (expand (funcall (get form 'micro-expand) form)) t))
               (t
                form)))
        ((atom form)
         form)
        (t
         (multiple-value-bind (a splice)
             (expand (car form))
           (cond ((null splice)
                  (cons a (expand (cdr form))))
                 ((consp a)
                  (append a (expand (cdr form))))
                 (t
                  (cons a (expand (cdr form)))))))))


(defun process-instruction (form)
  (let ((inst 0)
        (load-time-stuff nil)
        (fields-filled-in 0)
        (required-list nil))
    (setq form (expand form))
    (dolist (atom form)
      (cond ((consp atom)
             (case (car atom)
               ((byte byte-field)
                (if (and (not (zerop (ldb lam-ir-op fields-filled-in)))
                         (= (ldb lam-ir-op inst) lam-op-alu))
                    (ferror 'bad-inst "(byte ..) in alu instruction"))
                (when (zerop (ldb lam-ir-op fields-filled-in))
                  (setq fields-filled-in (dpb -1 lam-ir-op fields-filled-in))
                  (setq inst (dpb lam-op-byte lam-ir-op inst)))
                (when (and (= (ldb lam-ir-op inst) lam-op-byte)
                           (zerop (ldb lam-ir-byte-func fields-filled-in)))
                  ;;haven't said which kind yet - default to LDB
                  (setq fields-filled-in (dpb -1 lam-ir-byte-func fields-filled-in))
                  (setq inst (dpb lam-byte-func-ldb lam-ir-byte-func inst)))
                (push 'lam-ir-m-src required-list)
                (let ((width (cadr atom))
                      (pos (caddr atom)))
                  (cond ((not (= (ldb lam-ir-op inst) lam-op-jump))
                         (setq fields-filled-in (dpb -1 lam-ir-byte-length-spec fields-filled-in))
                         (setq inst (dpb (if (= (ldb lam-ir-op inst) lam-op-byte)
                                             (1- width)
                                           width)
                                         lam-ir-byte-length-spec
                                         inst))))
                  (cond ((or (= (ldb lam-ir-op inst) lam-op-jump)
                             (= (ldb lam-ir-op inst) lam-op-dispatch)
                             (= (ldb lam-ir-byte-func inst) lam-byte-func-ldb))
                         (setq pos (- 32. pos))))
                  (setq fields-filled-in (dpb -1 lam-ir-m-rotate fields-filled-in))
                  (setq inst (dpb pos lam-ir-m-rotate inst))))
               (a-constant
                (if (not (zerop (ldb lam-ir-a-src fields-filled-in)))
                    (ferror 'bad-inst "overlapping fields - a-mem-adr"))
                (setq fields-filled-in (dpb -1 lam-ir-a-src fields-filled-in))
                (setq load-time-stuff (append load-time-stuff
                                              (list 'lam-ir-a-src (eval-a-constant (cadr atom))))))
               (i-arg
                (if (not (= (ldb lam-ir-op inst) lam-op-dispatch))
                    (ferror 'bad-inst "I-ARG seen but not in dispatch instruction"))
                (setq fields-filled-in (dpb -1 lam-ir-disp-dispatch-constant fields-filled-in))
                (setq inst (dpb (cadr atom) lam-ir-disp-dispatch-constant inst)))
               (t
                ;;must be dest
                (dolist (dest atom)
                  (let ((info (symbol-info dest)))
                    (cond ((null info)
                           (let ((fixed-fields (get dest 'destination-fixed-fields))
                                 (required-fields (get dest 'destination-required-fields)))
                             (when (null fixed-fields)
                               (ferror 'bad-inst "undefined symbol ~s in what is apparently the destination"
                                       dest))
                             (if (not (zerop (logand fields-filled-in fixed-fields)))
                                 (ferror 'bad-inst "overlap - func dest"))
                             (setq fields-filled-in (+ fields-filled-in fixed-fields))
                             (setq inst (+ inst (get dest 'destination-fixed-fields-values)))
                             (when required-fields
                               (setq required-list (append required-list required-fields)))))
                          (t
                           (ecase (car info)
                             (m-mem-adr
                              (if (not (and (zerop (ldb lam-ir-a-mem-dest-flag fields-filled-in))
                                            (zerop (ldb lam-ir-m-mem-dest fields-filled-in))))
                                  (ferror 'bad-inst "overlap - m mem dest"))
                              (setq fields-filled-in (dpb -1 lam-ir-m-mem-dest fields-filled-in))
                              (setq load-time-stuff (append load-time-stuff
                                                            (list 'lam-ir-m-mem-dest dest))))
                             ((a-mem-adr d-mem-adr)
                              (if (not (and (zerop (ldb lam-ir-a-mem-dest fields-filled-in))
                                            (zerop (ldb lam-ir-a-mem-dest-flag fields-filled-in))))
                                  (ferror 'bad-inst "overlap - a dest"))
                              (setq fields-filled-in (dpb -1 lam-ir-a-mem-dest fields-filled-in))
                              (setq fields-filled-in (dpb -1 lam-ir-a-mem-dest-flag fields-filled-in))
                              (setq inst (dpb -1 lam-ir-a-mem-dest-flag inst))
                              (setq load-time-stuff (append load-time-stuff
                                                            (list 'lam-ir-a-mem-dest dest))))
                             (i-mem-adr
                              (ferror 'bad-inst "i-mem tag in dest field ~s" dest))))))))))
            ((get atom 'fixed-fields)
             (let ((fixed-fields (get atom 'fixed-fields))
                   (required-fields (get atom 'required-fields)))
               (if (not (zerop (logand fields-filled-in fixed-fields)))
                   (ferror 'bad-inst "overlapping fields"))
               (setq fields-filled-in (+ fields-filled-in fixed-fields))
               (setq inst (+ inst (get atom 'fixed-fields-values)))
               (when required-fields
                 (setq required-list (append required-list required-fields)))
               ))
            (t
             (let ((info (symbol-info atom)))
               (case (car info)
                 (i-mem-adr
                  (if (zerop (ldb lam-ir-op fields-filled-in))
                      (ferror 'bad-inst "i-mem-adr seen before opcode known"))
                  (if (not (= (ldb lam-ir-op inst) lam-op-jump))
                      (ferror 'bad-inst "i-mem-adr when not in jump inst"))
                  (if (not (zerop (ldb lam-ir-jump-addr fields-filled-in)))
                      (ferror 'bad-inst "overlapping fields"))
                  (setq fields-filled-in (dpb -1 lam-ir-jump-addr fields-filled-in))
                  (setq load-time-stuff (append load-time-stuff
                                                (list 'lam-ir-jump-addr atom))))
                 (a-mem-adr
                  (if (not (zerop (ldb lam-ir-a-src fields-filled-in)))
                      (ferror 'bad-inst "overlapping fields - a-mem-adr"))
                  (setq fields-filled-in (dpb -1 lam-ir-a-src fields-filled-in))
                  (setq load-time-stuff (append load-time-stuff
                                                (list 'lam-ir-a-src atom))))
                 (m-mem-adr
                  (if (not (zerop (ldb lam-ir-m-src fields-filled-in)))
                      (ferror 'bad-inst "overlapping fields - m-mem-adr"))
                  (setq fields-filled-in (dpb -1 lam-ir-m-src fields-filled-in))
                  (setq load-time-stuff (append load-time-stuff
                                                (list 'lam-ir-m-src atom))))
                 (d-mem-adr
                  (if (not (= (ldb lam-ir-op inst) lam-op-dispatch))
                      (ferror 'bad-inst "d-mem-adr seen in non-dispatch instruction"))
                  (if (not (zerop (ldb lam-ir-dispatch-addr fields-filled-in)))
                      (ferror 'bad-inst "overlap - d-mem-adr"))
                  (setq fields-filled-in (dpb -1 lam-ir-dispatch-addr fields-filled-in))
                  (setq load-time-stuff (append load-time-stuff
                                                (list 'lam-ir-dispatch-addr atom))))
                 (t
                  (cond ((null info)
                         (ferror 'bad-inst "~s is an undefined symbol" atom)))))))))
    (let ((all-required-fields 0))
      (dolist (f required-list)
        (setq all-required-fields (dpb -1 (symeval f) all-required-fields)))
      (if (not (= (logand fields-filled-in all-required-fields)
                  all-required-fields))
          (ferror nil "required field not filled in")))
    (cond ((zerop (ldb lam-ir-op fields-filled-in))
           ;;didn't fill in opcode - must be ALU
           (if (zerop (ldb lam-ir-ob fields-filled-in))
               (setq inst (dpb lam-ob-alu lam-ir-ob inst)))
           (cond ((zerop (ldb lam-ir-aluf fields-filled-in))
                  (cond ((not (zerop (ldb lam-ir-m-src fields-filled-in)))
                         (setq inst (dpb lam-alu-setm lam-ir-aluf inst)))
                        ((not (zerop (ldb lam-ir-a-src fields-filled-in)))
                         (setq inst (dpb lam-alu-seta lam-ir-aluf inst)))
                        (t
                         (ferror 'bad-inst "can't figure out what alu function to use")))))))
    ;;if a byte instruction that doesn't specify an a src, default to 2@a (A-ZERO)
    (cond ((and (= (ldb lam-ir-op inst) lam-op-byte)
                (zerop (ldb lam-ir-a-src fields-filled-in)))
           (setq load-time-stuff (append load-time-stuff
                                         (list 'lam-ir-a-src 'a-zero)))))
    (cons inst load-time-stuff)))

(defun eval-a-constant (form)
  (cond ((numberp form) form)
        ((symbolp form)
         (cond ((get form 'micro-symbol-value))
               ((get form 'si:system-constant)
                (symbol-value form))
               (t
                (ferror nil "can't evaluate ~s" form))))
        ((consp form)
         (ecase (car form)
           (eval (eval (cadr form)))
           (byte-value
            (dpb (eval-a-constant (caddr form))
                 (eval-a-constant (cadr form))
                 0))
           (byte
            (byte (eval-a-constant (cadr form))
                  (eval-a-constant (caddr form))))
           ))))

(defun micro-assemble-list (list)
  (let ((adr 0)
        output)
    (labels ((micro-assemble-list-1 (sub-list)
              (dolist (inst sub-list)
                (cond ((symbolp inst)
                       (push inst output))
                      ((get (car inst) 'micro-macro)
                       (micro-assemble-list-1 (funcall (get (car inst) 'micro-macro) inst)))
                      (t
                       (let ((bin-inst (process-instruction inst)))
                         (when (alignment-needed-for-micro-paging adr (car bin-inst))
                           (push (list 0) output)
                           (incf adr))
                         (push bin-inst output)
                         (incf adr)))))))
      (setq *previous-uinst-linked-to-next* nil)
      (micro-assemble-list-1 list)
      (reverse output))))

;(defun micro-assemble-list (list)
;  (do ((input list (cdr input))
;       (adr 0 (1+ adr))
;       output)
;      ((null input)
;       (reverse output))
;    (push (process-instruction (car input)) output)
;    (when (alignment-needed-for-micro-paging adr (caar output))
;      (let ((temp (pop output)))
;       (push (list 0) output)
;       (push temp output))
;      (incf adr))
;    ))

(defconst lam-ir-this-instruction-linked-to-next (byte 1 64.))
(defprop lam-ir-this-instruction-linked-to-next t lam:constant)
(defconst lam-ir-this-instruction-not-linked-to-next (byte 1 65.))
(defprop  lam-ir-this-instruction-not-linked-to-next t lam:constant)
;XCT-NEXT and OA-MOD sequences cannot straddle page boundaries in pagable UCODE.
;(If the second page weren't there, taking a micro-fault to get it would not work.)
;This predicate tries to avoid such lossage, however, it is only able to deal with
;sequences of two "interlocked" uinsts.  Unfortunately, three can be useful, such as where
;an OA-MOD is used on a uinst which is an XCT-NEXT.  This has to be outlawwed in
;pagable ucode for now, altho we could easily add a manual frob which would insert two
;no-ops if necessary, etc.  VMA-START-READ, etc, lose because information needed by the
;following CALL-CONDITIONAL-IF-PAGE-FAULT, etc, could be lost in case of micro-fault.
(defun alignment-needed-for-micro-paging (adr inst)
  (let* ((op (ldb lam-ir-op inst))
         (i-link-to-next
           (and (zerop (ldb lam-ir-this-instruction-not-linked-to-next inst))
                (OR (= 1 (ldb lam-ir-this-instruction-linked-to-next inst))
                    (= 1 (ldb lam-ir-popj-after-next inst))
                    (= op lam-op-dispatch)             ;for now, any dispatch might xct-next
                    (and (= op lam-op-jump)
                         (= 0 (ldb lam-ir-n inst)))
                    (and (or (= op lam-op-alu) (= op lam-op-byte))
                         (= 0 (ldb lam-ir-a-mem-dest-flag inst))
                         (or (= (ldb lam-ir-func-dest inst) lam-func-dest-imod-low)
                             (= (ldb lam-ir-func-dest inst) lam-func-dest-imod-high)
                             (= (ldb lam-ir-func-dest inst) lam-func-dest-vma-start-read)
                             (= (ldb lam-ir-func-dest inst) lam-func-dest-vma-start-write)
                             (= (ldb lam-ir-func-dest inst) lam-func-dest-md-start-write)
                             ))))))
    (cond ((and *previous-uinst-linked-to-next*
                (zerop (logand 17 adr)))
           (ferror 'bad-inst "first inst on micro paged linked to previous")))
    (cond ((and *previous-uinst-linked-to-next*
                i-link-to-next)
           (cond ((= 17 (logand 17 adr))
                  (ferror 'bad-inst "more than 2 linked uinst crossing a micro page"))
                 (t
                  (ferror 'bad-inst "more than 2 linked uinst luckily aligned this time")))))

    (cond (i-link-to-next
           (cond ((= #o17 (logand #o17 adr))
                  (setq *previous-uinst-linked-to-next* nil)
                  t)
                 (t
                  (setq *previous-uinst-linked-to-next* t)
                  nil)))
          (t
           (setq *previous-uinst-linked-to-next* nil)
           nil))))

(defmacro define-micro-function (name &rest form)
  (declare (arglist name arglist &body body))
  `(define-micro-function-1 ',name ',form))

(defun define-micro-function-1 (name form)
  (when (not (symbolp name))
    (ferror nil "micro function name must be a symbol"))
  (let ((lambda-exp (si:process-defun-body name form t))
        documentation
        arglist
        declarations
        body
        macro-compiled-alternative
        )
    ;;now we have (named-lambda (foo (documentation "foobar")) (args) (declare (...) (...)) body)
    ;;the declare may be absent
    (setq documentation (cadr (assq 'si:documentation (si:debugging-info lambda-exp))))
    (setq arglist (third lambda-exp))
    (setq body (cdddr lambda-exp))
    (when (eq (caar body) 'declare)
      (setq declarations (cdr (car body)))
      (pop body))

    (dolist (atom arglist)
      (if (char-equal (aref (string atom) 0) #/&)
          (ferror nil "no lambda list keywords allowed")))

    (setq macro-compiled-alternative
          (or (cadr (assq ':macro-compiled-alternative
                          declarations))
              'micro-function-no-macro-definition))

    (when (null (record-source-file-name name 'defun))
      (return-from define-micro-function-1 nil))

    (uninstall name)
    (putprop name macro-compiled-alternative 'macro-compiled-alternative)
    (putprop name (micro-assemble-list body) 'new-micro-out)
    (putprop name (length arglist) 'new-micro-n-args)
    (putprop name arglist 'new-micro-arglist)

    (install name)
    name))



(defun install (name)
  (uninstall name)

  (let ((block (allocate-i-mem-block (count-instructions (get name 'new-micro-out)) 4))
        (arglist (get name 'new-micro-arglist))
        (aborted t)
        (symbol-area-slot (get name 'micro-symbol-area-slot))
        (entry-area-slot (get name 'micro-entry-area-slot)))
    (unwind-protect
        (progn
          (putprop name block 'i-mem-block)
          (setf (i-mem-block-function block) name)
          (link name (i-mem-block-start block))
          (if (not (= (count-instructions (get name 'new-micro-out))
                      (length (get name 'new-micro-linked))))
              (ferror nil "linking shouldn't change size"))
          (do ((code (get name 'new-micro-linked) (cdr code))
               (adr (i-mem-block-start block) (1+ adr)))
              ((null code))
            (write-c-mem adr (car code)))
          (aset (i-mem-block-start block) #'si:micro-code-symbol-area symbol-area-slot)
          (aset (dpb (length arglist)
                     %%arg-desc-min-args
                     (dpb (length arglist)
                          %%arg-desc-max-args
                          0))
                #'si:micro-code-entry-args-info-area
                entry-area-slot)
          (aset arglist #'si:micro-code-entry-arglist-area entry-area-slot)

          (aset symbol-area-slot #'si:micro-code-entry-area entry-area-slot)
          (setq aborted nil)
          )
      (when aborted
        (aset (get name 'macro-compiled-alternative) #'si:micro-code-entry-area entry-area-slot)
        (free-i-mem-block block)
        (putprop name nil 'i-mem-block))))
  name)

(defun check-micro-consistancy (name)
  (if (not (= (%data-type (symbol-function name)) dtp-u-entry))
      (ferror nil "the function cell of ~s is not DTP-U-ENTRY"))
  (let ((entry-area-slot (get name 'micro-entry-area-slot))
        (symbol-area-slot (get name 'micro-symbol-area-slot))
        (block (get name 'i-mem-block))
        )
    (if (or (not (fixnump entry-area-slot))
            (not (fixnump symbol-area-slot)))
        (ferror nil "bad plist on ~s" name))
    (when (not (= entry-area-slot (%pointer (symbol-function name))))
      (ferror nil "entry-area-slot bad on ~s" name))
    (when (and (fixnump (aref #'si:micro-code-entry-area entry-area-slot))
               (not (= symbol-area-slot (aref #'si:micro-code-entry-area entry-area-slot))))
      (ferror nil "symbol-area-slot bad on ~s" name))
    (cond (block
           (if (not (typep block 'i-mem-block))
               (ferror nil "bad ~s property on ~s" 'I-MEM-BLOCK name))
            (if (not (= (i-mem-block-start block)
                        (aref #'si:micro-code-symbol-area symbol-area-slot)))
                (ferror nil "active address not beginning of i-mem-block for ~s" name)))
          ((not (= (aref #'si:micro-code-symbol-area symbol-area-slot)
                   (symbol-table-get 'lam:micro-code-symbol-table-fill-value)))
           (ferror nil "~s is not loaded in control memory, but its symbol-area entry is not ILLEGAL-INSTRUCTION" name)))
    t))


(defun uninstall (name)
  (when (or (not (fboundp name))
            (not (= (%data-type (symbol-function name)) dtp-u-entry)))
    (make-function-into-u-entry name (if (fboundp name)
                                         (symbol-function name)
                                       'not-micro-defined)))
  (check-micro-consistancy name)
  (let ((symbol-area-slot (get name 'micro-symbol-area-slot))
        (entry-area-slot (get name 'micro-entry-area-slot)))
    (aset (symbol-table-get 'lam:micro-code-symbol-table-fill-value)
          #'si:micro-code-symbol-area
          symbol-area-slot)
    (aset 0 #'si:micro-code-entry-args-info-area entry-area-slot)
    (aset '() #'si:micro-code-entry-arglist-area entry-area-slot)
    (aset (get name 'macro-compiled-alternative) #'si:micro-code-entry-area entry-area-slot)
    (when (get name 'i-mem-block)
      (free-i-mem-block (get name 'i-mem-block))
      (putprop name nil 'i-mem-block))
    ))

(defun make-function-into-u-entry (name macro-function)
  (let (entry-area-slot symbol-area-slot)
    (cond ((get name 'micro-symbol-area-slot)
           (setq entry-area-slot (get name 'micro-entry-area-slot))
           (setq symbol-area-slot (get name 'micro-symbol-area-slot))
           (when (and (fboundp name)
                      (= (%data-type (symbol-function name)) dtp-u-entry)
                      (not (= (%pointer (symbol-function name)) entry-area-slot)))
             (ferror nil "out of phase")))
          (t
           (setq symbol-area-slot (get-slot-in-micro-code-symbol-area))
           (setq entry-area-slot (get-slot-in-micro-code-entry-area))
           (putprop name entry-area-slot 'micro-entry-area-slot)
           (putprop name symbol-area-slot 'micro-symbol-area-slot)))

    (aset macro-function #'si:micro-code-entry-area entry-area-slot)
    (aset (symbol-table-get 'lam:micro-code-symbol-table-fill-value)
          #'si:micro-code-symbol-area
          symbol-area-slot)
    (aset 0 #'si:micro-code-entry-args-info-area entry-area-slot)
    (aset '() #'si:micro-code-entry-arglist-area entry-area-slot)
    (aset name #'si:micro-code-entry-name-area entry-area-slot)
    (setf (symbol-function name) (%make-pointer dtp-u-entry entry-area-slot))))

(defun count-instructions (code)
  (do ((tail code (cdr tail))
       (length 0))
      ((null tail)
       length)
    (when (consp (car tail))
      (incf length))))

(defvar *local-jump-tags*)

(defun link (function beg-adr)
  beg-adr
  (let ((code (get function 'new-micro-out))
        (*local-jump-tags* nil))
    (if (null code) (ferror nil "no code"))
    (do ((adr beg-adr)
         (tail code (cdr tail)))
        ((null tail))
      (cond ((symbolp (car tail))
             (push (list (car tail) adr) *local-jump-tags*))
            (t
             (incf adr))))
    (putprop function
             (loop for word in code
                   when (not (symbolp word))
                   collect (resolve word))
             'new-micro-linked))
  nil)

(defun resolve (word)
  (let ((inst (car word))
        (fields (cdr word)))
    (do ((next fields (cddr next)))
        ((null next))
      (let ((field (car next))
            (val (cadr next)))
        (cond ((numberp val)
               (setq inst (dpb (find-or-make-a-constant val)
                               (symeval field)
                               inst)))
              (t
               (let ((adr (or (cadr (assq val *local-jump-tags*))
                              (cadr (symbol-info val)))))
                 (cond ((null adr)
                        (ferror nil "can't resolve ~s" val))
                       (t
                        (setq inst (dpb adr (symeval field) inst)))))))))
    inst))

(defun micro-print (function &optional from-property-list)
  (let ((code (get function 'new-micro-linked)))
    (cond ((null code)
           (format t "~&No code."))
          ((and (null from-property-list)
                (fboundp function)
                (= (%data-type (symbol-function function)) dtp-u-entry)
                (fixnump (aref #'si:micro-code-entry-area (%pointer (symbol-function function)))))
           (format t "~&From control memory:")
           (do* ((adr (aref #'si:micro-code-symbol-area
                            (aref #'si:micro-code-entry-area (%pointer (symbol-function function)))) (1+ adr))
                 (end (+ adr (length (get function 'new-micro-linked)))))
                ((>= adr end))
             (let ((*package* (find-package "LAMBDA")))
               (format t "~&~5o: " adr)
               (lam:lam-type-out (read-c-mem adr) lam:lam-uinst-desc t t))))
          (t
           (format t "~&From property list:")
           (let ((*package* (find-package "LAMBDA")))
             (do ((inst-list code (cdr inst-list))
                  (adr 0 (1+ adr)))
                 ((null inst-list))
               (format t "~&~5o: " adr)
               (lam:lam-type-out (car inst-list) lam:lam-uinst-desc t t)))))))


(defun get-slot-in-micro-code-symbol-area (&aux x)
  (cond ((< (fill-pointer #'si:micro-code-symbol-area)
            (array-length #'si:micro-code-symbol-area))
         (setq x (fill-pointer #'si:micro-code-symbol-area))
         (incf (fill-pointer #'si:micro-code-symbol-area))
         x)
        (t
         (ferror nil "out of micro-code-symbol-area slots"))))

(defun get-slot-in-micro-code-entry-area (&aux x)
  (when (not (= si:%number-of-micro-entries
                (fill-pointer #'si:micro-code-entry-area)))
    (ferror nil "out of phase"))
  (cond ((< si:%number-of-micro-entries
            (array-length #'si:micro-code-entry-area))
         (setq x si:%number-of-micro-entries)
         (incf (fill-pointer #'si:micro-code-entry-area))
         (incf si:%number-of-micro-entries)
         x)
        (t
         (ferror nil "out of entry area slots"))))

(defun read-c-mem (adr)
  (setq adr (i-mem-lookup adr))
  (let ((origin (si:%region-origin si:micro-code-paging-area))
        (offset (* adr 2)))
    (dpb (%p-ldb (byte 16. 16.) (+ origin offset 1))
         (byte 16. 48.)
         (dpb (%p-ldb (byte 16. 0) (+ origin offset 1))
              (byte 16. 32.)
              (dpb (%p-ldb (byte 16. 16.) (+ origin offset))
                   (byte 16. 16.)
                   (%p-ldb (byte 16. 0) (+ origin offset)))))))

(defun write-c-mem (adr inst)
  (setq adr (i-mem-lookup adr))
  (let ((origin (+ (* adr 2) (si:%region-origin si:micro-code-paging-area))))
    (without-interrupts
      (%p-dpb (ldb (byte 16. 0) inst) (byte 16. 0) origin)
      (%p-dpb (ldb (byte 16. 16.) inst) (byte 16. 16.) origin)
      (%p-dpb (ldb (byte 16. 32.) inst) (byte 16. 0) (+ origin 1))
      (%p-dpb (ldb (byte 16. 48.) inst) (byte 16. 16.) (+ origin 1)))
    (cond ((< adr (* 16. (read-meter 'si:%highest-handcode-ucode-page)))
           (si:%write-internal-processor-memories
             1 adr
             (ash inst -32.)
             inst))
          (t
           (si:%micro-paging 1)))
    inst))




(defvar *i-mem-blocks* nil)

(defun print-i-mem-blocks ()
  (when (not (= (i-mem-block-start *i-mem-blocks*)
                (symbol-table-get 'lam:pagable-i-mem-loc)))
    (format t "~&Warning: doesn't start at PAGABLE-I-MEM-LOC"))
  (do ((block *i-mem-blocks* (i-mem-block-next block)))
      ((null block))
    (print block)
    (when (and (i-mem-block-next block)
               (not (= (+ (i-mem-block-start block)
                          (i-mem-block-length block))
                       (i-mem-block-start (i-mem-block-next block)))))
      (format t "   warning, a hole of ~d. words"
              (- (i-mem-block-start (i-mem-block-next block))
                 (+ (i-mem-block-start block)
                    (i-mem-block-length block)))))))

(defun set-up-i-mem-blocks ()
  (let ((lowest-loc (symbol-table-get 'lam:pagable-i-mem-loc)))
    (setq *i-mem-blocks* (make-i-mem-block i-mem-block-start lowest-loc
                                              i-mem-block-length (- (^ 2 16.) lowest-loc)
                                              i-mem-block-next nil
                                              i-mem-block-prev nil
                                              i-mem-block-function nil
                                              ))))

(defun allocate-i-mem-block (n-words &optional (alignment 0))
  ;;do a best fit thing
  (let (closest)
    (do ((block *i-mem-blocks* (i-mem-block-next block)))
        ((null block))
      (when (and (null (i-mem-block-function block))
                 (<= n-words (- (i-mem-block-length block)
                                (wasted-words-in-block block alignment)))
                 (or (null closest)
                     (< (i-mem-block-length block)
                        (i-mem-block-length closest))))
        (setq closest block)))
    (when closest
      (let ((wasted-words (wasted-words-in-block closest alignment)))
        (when (not (zerop wasted-words))
          (let ((wasted-block (make-i-mem-block i-mem-block-start (i-mem-block-start closest)
                                                i-mem-block-length wasted-words
                                                i-mem-block-next closest
                                                i-mem-block-prev (i-mem-block-prev closest)
                                                i-mem-block-function nil)))
            (setf (i-mem-block-prev closest) wasted-block)
            (cond ((null (i-mem-block-prev wasted-block))
                   (setq *i-mem-blocks* wasted-block))
                  (t
                   (setf (i-mem-block-next (i-mem-block-prev wasted-block)) wasted-block)))
            (incf (i-mem-block-start closest) wasted-words)
            (decf (i-mem-block-length closest) wasted-words))))

      (cond ((= (i-mem-block-length closest) n-words)
             (setf (i-mem-block-function closest) t))
            (t
             (let ((new-block (make-i-mem-block i-mem-block-start (+ (i-mem-block-start closest) n-words)
                                                i-mem-block-length (- (i-mem-block-length closest) n-words)
                                                i-mem-block-next (i-mem-block-next closest)
                                                i-mem-block-prev closest
                                                i-mem-block-function nil)))
               (when (i-mem-block-next closest)
                 (setf (i-mem-block-prev (i-mem-block-next closest)) new-block))
               (setf (i-mem-block-length closest) n-words)
               (setf (i-mem-block-next closest) new-block)
               (setf (i-mem-block-function closest) t))))
      closest)))

(defun wasted-words-in-block (block alignment)
  (let* ((modulus (ash 1 alignment))
         (mask (1- modulus)))
    (logand mask (- modulus (logand (i-mem-block-start block) mask)))))

(defun free-i-mem-block (block)
  (when (null (i-mem-block-function block))
    (ferror nil "block already free"))
  (setf (i-mem-block-function block) nil)
  (when (and (i-mem-block-prev block)
             (null (i-mem-block-function (i-mem-block-prev block)))
             (= (+ (i-mem-block-start (i-mem-block-prev block))
                   (i-mem-block-length (i-mem-block-prev block)))
                (i-mem-block-start block)))
    (incf (i-mem-block-length (i-mem-block-prev block))
          (i-mem-block-length block))
    (setf (i-mem-block-next (i-mem-block-prev block)) (i-mem-block-next block))
    (setf (i-mem-block-prev (i-mem-block-next block)) (i-mem-block-prev block))
    (setq block (i-mem-block-prev block)))
  (when (and (i-mem-block-next block)
             (null (i-mem-block-function (i-mem-block-next block)))
             (= (+ (i-mem-block-start block)
                   (i-mem-block-length block))
                (i-mem-block-start (i-mem-block-next block))))
    (incf (i-mem-block-length block) (i-mem-block-length (i-mem-block-next block)))
    (when (i-mem-block-next (i-mem-block-next block))
      (setf (i-mem-block-prev (i-mem-block-next (i-mem-block-next block))) block))
    (setf (i-mem-block-next block) (i-mem-block-next (i-mem-block-next block))))
  nil)

(defvar *a-mem-free-pointer* nil)
(defvar *new-a-constants* nil)

(defun find-or-make-a-constant (val &aux adr)
  (let ((entry (or (cl:assoc val (symbol-table-get 'lam:a-constant-list))
                   (cl:assoc val *new-a-constants*))))
    (cond ((null entry)
           (when (>= *a-mem-free-pointer* #o1600)
             (ferror nil "out of a constant locations"))
           (setq adr *a-mem-free-pointer*)
           (incf *a-mem-free-pointer*)
           (push (list val adr) *new-a-constants*)
           (write-a-mem adr val)
           adr)
          (t
           (cadr entry)))))



(defun read-a-mem (adr)
  (when (or (< adr 0)
            (>= adr 1024.))
    (ferror nil "out of range"))
  (dpb (%p-ldb (byte 16. 16.) (%pointer-plus si:a-memory-virtual-address adr))
       (byte 16. 16.)
       (%p-ldb (byte 16. 0) (%pointer-plus si:a-memory-virtual-address adr))))


(defun write-a-mem (adr val)
  (when (or (< adr 0)
            (>= adr 1024.))
    (ferror nil "out of range"))
  (%p-dpb (ldb (byte 16. 0) val)
          (byte 16. 0)
          (%pointer-plus si:a-memory-virtual-address
                         adr))
  (%p-dpb (ldb (byte 16. 16.) val)
          (byte 16. 16.)
          (%pointer-plus si:a-memory-virtual-address
                         adr))
  val)


(defun set-up-once ()
  (when (not (= si:%number-of-micro-entries
                (fill-pointer #'si:micro-code-entry-area)))
    (ferror nil "entry area out of phase"))
  (when (= (fill-pointer #'si:micro-code-symbol-area)
           (array-length #'si:micro-code-symbol-area))
    (setf (fill-pointer #'si:micro-code-symbol-area)
          (1+ (- (symbol-table-get 'lam:highest-misc-entry) #o200))))
  (set-up-i-mem-blocks)
  (setq *a-mem-free-pointer* (symbol-table-get 'lam:a-constant-loc))
  )

(add-initialization "set up new micro assembler"
                    '(set-up-once)
                    '(:once))

;;; -*- Mode:LISP; Package:LAMBDA; Readtable:CL; Base:8 -*-

;;;;;;;;;;;;;;;;;;;;;
;;; Does this look ugly? Use a landscape.
;;;;;;;;;;;;;;;;;;;;;

(defconstant *dont-count-these-instructions-in-those-that-have-no-predecessors*
             '(HIGHEST-KERNAL-UCODE-LOCATION)
  "Anything on this list is not included in the *instructions-without-predecessors*.")

(defparameter *default-scanning-distance* 10. "Declarations can be this far ahead of applicable instruction.")

(defvar *i-mem-array*)
(defvar *a-mem-array*)
(defvar *main-mem-array*)
(defvar *mid-array*)
(defvar *lmc-pathname*)
(defvar *trace-lmc-version-number*)
(defvar *trap-upc*)
(defvar *illop-upc*)
(defvar *highest-kernal-ucode-location+1*)
(defvar *i-mem-symbols-array* (make-sequence 'vector 50000.))
(defvar *a-mem-symbols-array* (make-sequence 'vector 1024.))
(defvar *register-names* (make-sequence 'vector *number-of-registers*))
(defvar *registers* (make-hash-table :size *number-of-registers*))
(defvar *register-set-universe* (make-universe *number-of-registers*))
(defvar *m-mem-registers-set*)
(defvar *constant-registers-set*)
(defvar *global-registers-set*)

(defvar *ucode-declarations*)
     ; a list (<ucode-locn> <args to DECLARE at ucode assembly time)
     ; following forms are meaningful:
     ;  (args <reg1> <reg2> ..)    ;this subroutine takes args in these regs.
     ;  (values <reg1> <reg2> ..)  ;this subroutine returns values here
     ;  (clobbers <reg1> <reg2> ..);clobbers or may clobber these.  the server a documentation
     ;                        ; purpose and speeds up the micro-tracer.  However, it is
     ;                        ; capable of figuring this out for itself.
     ;  (local <reg1> ...)            ;holds state in these vars, error if caller depends on them.
     ;                        ; a-fake-microsecond-clock, for example.
     ;  (must-avoid <symbol>)      ;this subroutine, ones it calls, etc must never get to <symbol>.
     ;                        ;  intended mainly for (must-avoid INTR).
     ;  (saves (<reg> <place> ..)  ;place may be register or NIL if too complicated
     ;                        ; error if store it twice in any path and place is NON-NIL.
     ;  (restores (<reg> <place> ..) ;restores from last saved.
     ;  (suspend-flow-tracing)  ;dont look past here, leaving environment of interest never to return

(defvar *subroutine-entries*)
(defvar *instructions-without-predecessors*)

(defvar *basic-block-info*)

;;; This is where everything kicks off.  If you do this by hand, be sure to follow
;;; the order of things.  Many things are not idempotent and most must be done in the
;;; order shown.

;;; Diagnostic:

(defun i-want-to-lose (&key (how-much? :really-big))
  "Run the microcode disassembler until you lose."
  how-much?
  (format t "~%Anything you say...")
  (do ((count 0 (1+ count)))
      ((logic-has-fallen?) "Logic has fallen")
    (format t "~%~%Attempting to lose on pass ~d." count)
    (disassemble-microcode)))

(defun disassemble-microcode (&optional (filename t)) ;will load latest version if t.
  (readin-files filename)
  (initial-setup)
  (compute-instructions)
  (find-subroutine-entries)
  (find-instructions-without-predecessors)
  )

(defun readin-files (filename)
  (multiple-value-setq (*i-mem-array* *a-mem-array*
                        *main-mem-array* *mid-array*
                        *lmc-pathname* *trace-lmc-version-number*)
    (si:load-lmc-file-into-arrays filename)) ;defined in sys:io;disk
  (si:load-i-mem-symbols-if-necessary *trace-lmc-version-number* *lmc-pathname*)
  (readin-declarations *trace-lmc-version-number* *lmc-pathname*))

(defun readin-declarations (&optional (vn *trace-lmc-version-number*)
                                      (filename *lmc-pathname*))
  (setf filename (funcall filename :new-type-and-version "lmc-dcl" vn))
  (with-open-file (f filename)
    (let* ((ibase 8)
           (base 8)
           (s-exp (read f)))
      (setf *ucode-declarations* (second (third s-exp)))))) ;flush (SETQ MICRO-DECLARATION-TABLE (quote ..))

(defun initial-setup ()
  (setup-i-mem-symbols-array)
  (setup-a-mem-symbols-array)
  (setq *illop-upc* (i-mem-symbol->address "ILLOP")
        *trap-upc*  (i-mem-symbol->address "TRAP"))
  (setup-register-info)
  )

(defun compute-instructions ()
  (digest-i-mem-array)
  (digest-declarations)
  (compute-successors)
  (compute-predecessors)
  )

(defun setup-i-mem-symbols-array ()
  (fill *i-mem-symbols-array* '())
  (insert-i-mem-labels)
  (insert-relative-i-mem-labels)
  )

(defun insert-i-mem-labels ()
  (let ((hash-table (cadr (assoc *trace-lmc-version-number* si:*i-mem-symbol-tables*))))
    (maphash #'(lambda (symbol symbol-value)
                 (when (array-in-bounds? *i-mem-symbols-array* symbol-value)
                   (setf (elt *i-mem-symbols-array* symbol-value) symbol)))
             hash-table)))

(defun insert-relative-i-mem-labels ()
  (let (last-symbol last-symbol-value)
    (for-elements-in-vector
      *i-mem-symbols-array*
      #'(lambda (address entry)
          (if entry
              (setq last-symbol       entry
                    last-symbol-value address)
              (setf (elt *i-mem-symbols-array* address)
                    (if (and last-symbol                                ;When last symbol is close enough,
                             (< (- address last-symbol-value) 100.))    ;number relative to it.
                        (list last-symbol (- address last-symbol-value))
                        address)))))))                          ;Otherwise just use the number.

(defun address->i-mem-symbol (address)
  (when address (elt *i-mem-symbols-array* address)))

(defun i-mem-symbol->address (symbol)
  (si:i-mem-symeval symbol *trace-lmc-version-number*))

(defun setup-a-mem-symbols-array ()
  (dolist (symbol-address-pair (third (assoc *trace-lmc-version-number* si:*i-mem-symbol-tables*)))
    (setf (elt *a-mem-symbols-array* (first symbol-address-pair)) (second symbol-address-pair))))

(defun address->a-mem-symbol (address)
  (when (array-in-bounds? *a-mem-symbols-array* address)
    (elt *a-mem-symbols-array* address)))

;;; Registers

(defun setup-register-info ()
  (fill *register-names* '())
  (let* ((symbol-table-list (assoc *trace-lmc-version-number* si:*i-mem-symbol-tables*))
         (register-alist (third symbol-table-list))
         (info-list (fourth symbol-table-list)))
    (setup-register-names register-alist)
    (check-a-mem-consistancy)
    (setup-register-set-universe)
    (setup-constants-set (get-from-alternating-list info-list 'a-constant-list))
    (setup-globals-set)
    (setup-m-mem-set)))

(defun setup-register-names (register-alist)
  (dolist (name-register-pairs register-alist)
    (let ((register-number (first name-register-pairs))
          (register-name   (second name-register-pairs)))
      (when (and (number? register-number)
                 (array-in-bounds? *register-names* register-number))
        (setf (elt *register-names* register-number) register-name)))))

(defun check-a-mem-consistancy ()
  "This just makes a-mem-symbol-array and register-names consistant"
  (for-elements-in-vector
    *a-mem-symbols-array*
    #'(lambda (index a-mem-name)
      (let ((register-name (register-number->symbol index)))
        (if a-mem-name
            (if register-name
                (unless (eq? a-mem-name register-name)
                  (ferror nil "Name mismatch between ~S in a-mem and ~S register at ~D"
                          a-mem-name register-name index))
                (setf (elt *register-names* index) a-mem-name))
            (if register-name
                (setf (elt *a-mem-symbols-array* index) register-name)
                (let ((name (intern (format nil "REGISTER-~D" index))))
                  (setf (elt *register-names* index) name)
                  (setf (elt *a-mem-symbols-array* index) name))))))))

(defun setup-register-set-universe ()
  ;; Setup universe for source/clobbers sets.
  ;; We assume that lower number registers are used more often.
  ;; This assumption is implementation dependant, but will only cause speed lossage
  ;; if it is not true.
  (for-elements-in-vector *register-names*
    (lambda (ignore-index name)
      ignore-index
      (element->number-inserting name *register-set-universe*)))
    (setq *default-universe* *register-set-universe*))

(defun setup-constants-set (assembly-constant-list)
  (setq *constant-registers-set* (list->set *constant-registers*))
  (dolist (constant-register-pair assembly-constant-list)
    (let ((name (register-number->symbol (second constant-register-pair))))
      (unless (null? name)
        (set-push! name *constant-registers-set*))))
  (dolist (register *registers-used-for-M-constants*) ;;See beginning of file.
    (set-push! (register-number->symbol register) *constant-registers-set*)))

(defun setup-globals-set ()
  (setq *global-registers-set* (list->set *global-registers*)))

(defun setup-m-mem-set ()
  (setq *m-mem-registers-set*
        (list->set
          (sequence->list
            (subseq *a-mem-symbols-array* *m-mem-low-bound* *m-mem-high-bound*)))))

(defun symbol->register-number (symbol)
  (si:register-symeval symbol *trace-lmc-version-number*))

(defun register-number->symbol (number)
  (elt *register-names* number))

(defun register-exists? (register-name)
  (not (null? (symbol->register-number register-name))))

(defun constant-register? (register)
  (set-element? register *constant-registers-set*))

;;; Instructions.

(defconstant *instruction-mask* 77777777777777777777) ;;ldb lossage?
(defconstant-now *dispatch-address-mask* 177777)

(defun valid-instruction? (instruction)
  (not (null? instruction)))

(defun no-op? (instruction)
  (zero? (logand instruction *instruction-mask*)))

;;; Slow extractors
;(defun extractor (field)
;  #'(lambda (fixinst)
;      (ldb field fixinst)))

;(defun extract-flag (field)
;  #'(lambda (fixinst)
;      (= 1 (funcall (extractor field) fixinst))))

;(defun extract-symbolic (field symbol-map)
;  (let ((lookup (appropriate-lookup symbol-map)))
;    #'(lambda (fixinst)
;        (funcall lookup (funcall (extractor field) fixinst)))))

;(defun appropriate-lookup (alist-or-vector)
;  (if (list? alist-or-vector)
;      #'(lambda (value) (second (assoc value alist-or-vector)))
;      #'(lambda (value) (aref alist-or-vector value))))

;;; Fast extractors
(defmacro defextractor (name field)
  `(DEFMACRO ,name (FIXINST) `(LDB ,,field ,fixinst)))

(defmacro defflag-extractor (name field)
  `(DEFMACRO ,name (FIXINST) `(= 1 (LDB ,,field ,fixinst))))

(defmacro defsymbolic-extractor (name field symbol-table)
  `(DEFMACRO ,name (FIXINST)
     (IF (LIST? ,symbol-table)
         `(SECOND (ASSOC (LDB ,,field ,fixinst) (QUOTE ,,symbol-table) :TEST #'=))
         `(ELT ,,symbol-table (LDB ,,field ,fixinst)))))

(defconstant-now opcode-map (vector 'alu-op 'byte-op 'jump-op 'dispatch-op))

(defeq-test alu-op?      'alu-op)
(defeq-test byte-op?     'byte-op)
(defeq-test jump-op?     'jump-op)
(defeq-test dispatch-op? 'dispatch-op)

(defsymbolic-extractor raw-opcode-extractor lam-ir-op opcode-map)

(defun extract-opcode (instruction)
  (if (no-op? instruction)
      'no-op
      (raw-opcode-extractor instruction)))

(defflag-extractor extract-popj-after-next lam-ir-popj-after-next)

(defconstant-now jump-condition-map
   `((,lam-jump-cond-bit-set                 BIT-SET)
     (,lam-jump-cond-bit-clear               BIT-CLEAR)
     (,lam-jump-cond-m<a                     M<A)
     (,lam-jump-cond-m<=a                    M<=A)
     (,lam-jump-cond-m=a                     M=A)
     (,lam-jump-cond-page-fault              PAGE-FAULT)
     (,lam-jump-cond-page-fault-or-interrupt PAGE-FAULT-OR-INTERRUPT)
     (,lam-jump-cond-page-fault-or-interrupt-or-sequence-break
                                             PAGE-FAULT-OR-INTERRUPT-OR-SEQUENCE-BREAK)
     (,lam-jump-cond-unc                     UNCONDITIONAL)
     (,lam-jump-cond-m>=a                    M>=A)
     (,lam-jump-cond-m>a                     M>A)
     (,lam-jump-cond-m-neq-a                 M-NEQ-A)
     (,lam-jump-cond-no-page-fault           NO-PAGE-FAULT)
     (,lam-jump-cond-data-type-not-equal     DATA-TYPE-NOT-EQUAL)
     (,lam-jump-cond-data-type-equal         DATA-TYPE-EQUAL)))

(defeq-test unconditional? 'unconditional)

(defconstant-now jump-rpn-map
             (vector 'jump-xct-next
                     'jump
                     'call-xct-next
                     'call
                     'return-xct-next
                     'return
                     'illegal-rpn
                     'illegal-rpn))

(defsymbolic-extractor extract-jump-condition lam-ir-jump-cond jump-condition-map)
(defsymbolic-extractor extract-jump-rpn-bits lam-ir-rpn jump-rpn-map)
(defextractor extract-jump-address lam-ir-jump-addr)

(defconstant-now dispatch-rpn-map
             (vector 'jump-xct-next
                     'jump
                     'call-xct-next
                     'call
                     'return-xct-next
                     'return
                     'fall-through
                     'skip))

(defflag-extractor extract-dispatch-push-own-address lam-ir-disp-lpc)
(defextractor extract-dispatch-base-address lam-ir-disp-addr)
(defextractor extract-dispatch-bits lam-ir-disp-bytl)
(defsymbolic-extractor extract-dispatch-rpn-bits lam-disp-rpn-bits dispatch-rpn-map)
(defmacro extract-dispatch-address-from-entry (entry)
  `(LOGAND ,entry ,*dispatch-address-mask*))

(defextractor extract-a-source lam-ir-a-src)
(defextractor extract-m-source-address lam-ir-m-src-adr)
(defflag-extractor extract-functional-source-flag lam-ir-func-src-flag)

(defflag-extractor extract-a-memory-destination-flag lam-ir-a-mem-dest-flag)
(defextractor extract-a-memory-destination lam-ir-a-mem-dest)
(defextractor extract-m-memory-destination lam-ir-m-mem-dest)

(defun extract-instruction-sources (fixinst)
  (let ((opcode (extract-opcode fixinst))
        (a-source (extract-a-source fixinst))
        (m-source-address (extract-m-source-address fixinst))
        (functional-source-flag (extract-functional-source-flag fixinst))
        (ans (make-empty-set)))
    (unless (dispatch-op? opcode)
      (set-push! (address->a-mem-symbol a-source) ans))
    (when (null? functional-source-flag)
      (set-push! (address->a-mem-symbol m-source-address) ans))
    ans))

(defun extract-instruction-destination (fixinst)
  (let ((opcode (extract-opcode fixinst)))
    (if (or (alu-op? opcode)
            (byte-op? opcode))
        (let ((address (if (extract-a-memory-destination-flag fixinst)
                           (extract-a-memory-destination fixinst)
                           (extract-m-memory-destination fixinst))))
          (make-set (address->a-mem-symbol address)))
        (make-empty-set))))

(defstruct (instruction (:constructor make-instruction-internal)
                        (:callable-constructors nil) ;; This makes the constructor a macro.
                        (:print-function print-instruction)
                        )
  numeric-form    ;; Fresh from the binary file
  address
  (declarations '())
  opcode-type     ;; alu-op, byte-op, jump-op, dispatch-op
  popj-after-next
  sources
  destination
  successors
  predecessors
  calling-subroutine
  trace-info
  trace-warnings
  )

(defsynonym instruction? instruction-p)

(defalike #'instruction? #'eq?)

(defun print-instruction (instruction stream level)
  (if (and *print-level* (> level *print-level*))
      (format stream " # ")
      (format stream "#<Instruction: ~S>"
              `(,(instruction-address instruction)      ;punctuation!
                ; ,(instruction-numeric-form instruction)
                ,(instruction-opcode-type instruction)
                ,@(when (jump-op? (instruction-opcode-type instruction))
                   (list (extract-jump-condition (instruction-numeric-form instruction))))
                ,@(when (instruction-popj-after-next instruction) '(POPJ-AFTER-NEXT))
                (SUCCESSORS ,@(map 'list #'names-of-successors (instruction-successors instruction)))
                ,@(instruction-declarations instruction))
              )))

(defun names-of-successors (successor)
  (labels (
    (maybe-successor-name (instruction)
      (when (instruction? instruction)
        (instruction-address instruction)))
    )
    (if (instruction? successor)
        (instruction-address successor)
        (cons (first successor)
              (map 'list #'maybe-successor-name (rest successor))))))

(defvar *instruction-array* (make-sequence 'vector 50000 :initial-element '()))

(defun instruction-at (address)
  (elt *instruction-array* address))

(defsetf instruction-at (address) (new-value)
  `(SETF (ELT *INSTRUCTION-ARRAY* ,address) ,new-value))

(defun for-each-instruction (operation)
  (for-elements-in-vector
    *instruction-array*
    #'(lambda (address possible-instruction)
        (when possible-instruction
          (funcall operation possible-instruction address)))))

(defun make-instruction (fixinst address)
    (make-instruction-internal
      :numeric-form       fixinst
      :address            address
      :opcode-type       (extract-opcode fixinst)
      :popj-after-next   (extract-popj-after-next fixinst)
      :sources           (extract-instruction-sources fixinst)
      :destination       (extract-instruction-destination fixinst)
      ))

(defun flush-field (field-selector)
  "Takes a symbol specifying a field-selector and removes this field from all
instructions.  Must be passed a symbol because of setf lossage (see this code)."
  ;; *Sigh*  You can't pass a selector to setf!
  (let ((flusher
          (compile-lambda `(LAMBDA (INSTRUCTION)
                             (WHEN INSTRUCTION
                               (SETF (,field-selector INSTRUCTION) '()))))))
    (map '() flusher *instruction-array*)))

(defun digest-i-mem-array ()
  (for-elements-in-vector
    *i-mem-array*
    #'(lambda (index fixinst)
        (when (null? fixinst) (return-from digest-i-mem-array nil))
        (when (valid-instruction? fixinst)
          (setf (instruction-at index)
                (make-instruction fixinst (address->i-mem-symbol index)))))))

;;; Declarations
;;;
;;; We play a little fast and loose with abstraction here, so watch out!

(defconstant *set-declarations* '(args values clobbers local))
(defconstant *saves-tag*    'saves)
(defconstant *restores-tag* 'restores)

(defsynonym declaration-type car)
(defsynonym declaration-info cdr)

(defun declaration-type? (type)
  #'(lambda (declaration)
      (eq? (declaration-type declaration) type)))

(defun spread-declarations (instruction &rest decls-return-map)
  (let ((decls (instruction-declarations instruction)))
    (labels (
      (find-decl (type)
        (or (cdr (assoc type decls))
            (when (member type *set-declarations*)
              (make-empty-set))))
      )
      (values-list
        (map 'list #'find-decl decls-return-map)))))

(deff saves-declaration? (declaration-type? *saves-tag*))
(deff restores-declaration? (declaration-type? *restores-tag*))

(defun suspend-flow-tracing? (instruction)
  (assoc 'suspend-flow-tracing (instruction-declarations instruction)))

(defun instruction-saves? (instruction)
  (assoc *saves-tag* (instruction-declarations instruction)))

(defun instruction-restores? (instruction)
  (assoc *restores-tag* (instruction-declarations instruction)))

(defun digest-declarations ()
  (dolist (declarations *ucode-declarations*)
    (let ((address (first declarations)))
      (if (> address (length *instruction-array*))
          (format t "~&Declaration ~s ignored:  address too high." declarations)
          (dolist (declaration (rest declarations))
            (associate-declaration-with-instruction address declaration))))))

(defun associate-declaration-with-instruction (address declaration)
  (case (declaration-type declaration)
    ((must-avoid suspend-flow-tracing) (push declaration (instruction-declarations (instruction-at address))))
    (saves      (associate-saves-with-instruction address (declaration-info declaration)))
    (restores   (associate-restores-with-instruction address (declaration-info declaration)))
    (otherwise  (push (cons (first declaration) (list->set (rest declaration)))
                      (instruction-declarations (instruction-at address))))))

(defsubst make-test-triplet (predicate win-action lose-action)
  (list predicate win-action lose-action))

(defsynonym test-triplet-predicate  first)
(defsynonym test-triplet-win-action second)
(defsynonym test-triplet-lose-action third)

(defun scan-forward-to-appropriate-instruction
       (start-address test-triplets &optional (how-far-to-scan *default-scanning-distance*))
  (dotimes (scan how-far-to-scan
                 (when test-triplets (dolist (test-t test-triplets) (funcall (test-triplet-lose-action test-t)))))
    (let ((instruction (instruction-at (+ start-address scan)))
          (triplets-left '()))
      (dolist (test-t test-triplets)
        (if (funcall (test-triplet-predicate  test-t) instruction)
            (funcall (test-triplet-win-action test-t) instruction)
            (push test-t triplets-left)))
      (unless triplets-left (return-from scan-forward-to-appropriate-instruction nil))
      (setq test-triplets triplets-left))))

(defun scanned-declaration (tester win-action lose-action)
  #'(lambda (start-address declaration-guts)
      (let (test-triplets)
        (dolist (decl declaration-guts)
          (let ((possible-predicate (apply tester decl)))
            (unless (null? possible-predicate)
              (push (make-test-triplet possible-predicate
                                       (apply win-action decl)
                                       (apply lose-action start-address decl))
                    test-triplets))))
        (scan-forward-to-appropriate-instruction start-address test-triplets))))

(defun tester (source destination barf-string)
  (if (and (register-exists? source)
           (register-exists? destination))
      #'(lambda (instruction)
          (and (set-element? source      (instruction-sources instruction))
               (set-element? destination (instruction-destination instruction))))
      (format t barf-string source destination)
      nil))

(defun tagger (source destination illegality-test tag)
  #'(lambda (instruction)
      (funcall illegality-test instruction)
      (pushnew (list tag) (instruction-declarations instruction))
      (setf (instruction-sources instruction)     (make-set source))
      (setf (instruction-destination instruction) (make-set destination))))

(defun loser (barf-string source destination location)
  #'(lambda ()
      (format t barf-string source destination (instruction-address (instruction-at location)))))

(defun might-instruction-save? (from-register into-place)
  (tester from-register into-place "~&Ignoring save ~S into ~S."))

(defun tag-as-save-instruction (from into)
  (tagger from into
          #'(lambda (instruction)
              (when (instruction-restores? instruction)
                (format t "Invalid declaration: save and restore in ~S")))
          *saves-tag*))

(defun losing-save (where from into)
  (loser "~&Cannot match save ~S into ~S at ~S." from into where))

(deff associate-saves-with-instruction
      (scanned-declaration #'might-instruction-save?
                           #'tag-as-save-instruction
                           #'losing-save))

(defun might-instruction-restore? (register from-register)
  (tester from-register register "~&Ignoring restore from ~S into ~S."))

(defun tag-as-restore-instruction (into from)
  (tagger from into
          #'(lambda (instruction)
              (when (instruction-saves? instruction)
                (format t "Invalid declaration: save and restore in ~S")))
          *restores-tag*))

(defun losing-restore (where into from)
  (loser "~&Cannot match restore from ~S into ~S at ~S." from into where))

(deff associate-restores-with-instruction
      (scanned-declaration #'might-instruction-restore?
                           #'tag-as-restore-instruction
                           #'losing-restore))

(defvar *losing-successors* '() "Holds list of instruction addresses.
Instructions at these addresses have successors that do not exist.")

(defun compute-successors ()
  (for-each-instruction
    #'(lambda (instruction index)
      (setf (instruction-successors instruction)
            (compute-instruction-successors instruction index)))))

;;; Uncomment these (and comment the macros) in order to check for the
;;; validity of successors.

;(defun verified-instruction (address referenced-by)
;  (let ((instruction (instruction-at address))
;       (reference-symbolic-address (instruction-address (instruction-at referenced-by))))
;    #'(lambda (message)
;      (when (null? instruction)
;       (pushnew reference-symbolic-address *losing-successors* :test #'equal?)
;       (fresh-line)
;       (format t message '())
;       (format t " at ~S " address)
;       (format t " referenced by instruction at ~S " reference-symbolic-address))
;      instruction)))

;(defun check-verification (instruction-or-null message)
;  (if (null? instruction-or-null)
;      `()
;      (funcall instruction-or-null message)))

(defmacro verified-instruction (address ignore-referenced-by)
  ignore-referenced-by
  `(INSTRUCTION-AT ,address))

(defmacro check-verification (instruction ignore-message)
  ignore-message
  instruction)

;;; format:
;;; (CALLS <instruction-called> <instruction-xct-along-the-way> <instruction-returned-to>)
;;; (JUMP-XCT-NEXT <instruction-jumped-to> <instruction-xct-along-the-way>)
;;; (RETURNS () <instruction-xct-along-the-way>)

(defsynonym instruction-xfered-to second)
(defsynonym instruction-xct-along-the-way third)
(defsynonym instruction-returned-to fourth)

(defun make-normal-successor (instruction)
  (check-verification instruction "Missing instruction in stream"))

(defun make-jump-successor (instruction)
  (check-verification instruction "Jump to missing instruction"))

(defun make-dispatch-jump-successor (instruction)
  (check-verification instruction "Dispatch jump to missing instruction"))

(defun make-dispatch-fall-through-successor (instruction)
  (check-verification instruction "Dispatch fall through to missing instruction"))

(defun make-dispatch-skip-successor (instruction)
  (check-verification instruction "Dispatch skip to missing instruction"))

(defun make-call-successor (instruction-called instruction-xct-next instruction-returned-to)
  `(CALLS ,(check-verification instruction-called "Call to missing instruction")
          ,(check-verification instruction-xct-next "Missing instruction xct-next'd during call")
          ,(check-verification instruction-returned-to "Return to missing instruction")))

(defun make-return-successor (instruction-returned-to instruction-xct-next)
  `(RETURNS ,(check-verification instruction-returned-to "Return to missing instruction")
            ,(check-verification instruction-xct-next "Missing instruction xct-next'd during return")))

(defun make-jump-xct-next-successor (instruction-xfered-to instruction-xct-next)
  `(JUMP-XCT-NEXT ,(check-verification instruction-xfered-to "Jump to missing instruction")
                  ,(check-verification instruction-xct-next "Missing instruction xct-next'd during jump")))

(defun list-successor-type? (type)
  #'(lambda (successor)
      (and (list? successor)
           (eq? (first successor) type))))

(deff call-successor? (list-successor-type? 'calls))
(deff returns-successor? (list-successor-type? 'returns))
(deff jump-xct-next-successor? (list-successor-type? 'jump-xct-next))
(defsynonym next-instruction-successor? instruction?)

(defun compute-instruction-successors (instruction in-address)
  (let ((numeric-form (instruction-numeric-form instruction))
        (popj-after-next (instruction-popj-after-next instruction))
        (next-instruction (verified-instruction (1+ in-address) in-address))
        (after-next-instruction (verified-instruction (+ 2 in-address) in-address)))
    (case (instruction-opcode-type instruction)
      ((no-op alu-op byte-op) (if popj-after-next
                                  (list (make-return-successor '() next-instruction))
                                  (list (make-normal-successor next-instruction))))
      (jump-op (compute-jump-successors popj-after-next next-instruction after-next-instruction
                                        (verified-instruction (extract-jump-address numeric-form) in-address)
                                        (extract-jump-condition numeric-form)
                                        (extract-jump-rpn-bits numeric-form)))
      (dispatch-op (compute-dispatch-successors in-address popj-after-next
                                                instruction next-instruction after-next-instruction
                                                (extract-dispatch-push-own-address numeric-form)
                                                (extract-dispatch-base-address numeric-form)
                                                (extract-dispatch-bits numeric-form)))
      )))

(defun compute-jump-successors (popj-after-next next-instruction after-next-instruction to-address condition rpn-bits)
  (let ((ans '()))
    (labels (
             (cannot-popj-after-next ()
                (when popj-after-next
                  (ferror nil "Popj-after-next combined with ~S" rpn-bits)))
             (can-fall-through ()
                (unless (unconditional? condition)
                  (can-go-to (make-normal-successor next-instruction))))
             (can-go-to (where)
                (push where ans)))
      (case rpn-bits
        (jump-xct-next
          (cannot-popj-after-next)
          (can-go-to (make-jump-xct-next-successor to-address next-instruction))
          (can-fall-through))
        (jump
          (cannot-popj-after-next)
          (can-go-to (make-jump-successor to-address))
          (can-fall-through))
        (call-xct-next
          (cannot-popj-after-next)
          (can-go-to (make-call-successor to-address next-instruction after-next-instruction))
          (can-fall-through))
        (call
          (cannot-popj-after-next)
          (can-go-to (make-call-successor to-address '() next-instruction))
          (can-fall-through))
        (return-xct-next
          (cannot-popj-after-next)
          (can-go-to (make-return-successor '() next-instruction))
          (can-fall-through))
        (return
         (if popj-after-next
             (can-go-to (make-return-successor '() next-instruction))
             (can-go-to (make-return-successor '() '())))
         (unless (unconditional? condition)
           (can-fall-through)))
        (illegal-rpn
          (ferror nil "Illegal rpn bits in jump"))))
    ans))

(defun compute-dispatch-successors (dispatched-from-location popj-after-next
                                    instruction next-instruction after-next-instruction
                                    push-own-address? base-address bits)
  dispatched-from-location ;;This is not used if verification is off.
  (let ((number-of-dispatch-options (expt 2 bits))
        (return-address (if push-own-address? instruction next-instruction))
        (return-address-if-xct-next (if push-own-address? next-instruction after-next-instruction))
        (ans '()))
    (labels (
             (can-go-to (where)
               (pushnew where ans :test #'equal?)))
      (dotimes (option number-of-dispatch-options)
        (let* ((dispatch-entry (elt *a-mem-array* (+ base-address option)))
               (rpn-bits (extract-dispatch-rpn-bits dispatch-entry))
               (dispatch-address
                 (verified-instruction (extract-dispatch-address-from-entry dispatch-entry)
                                       dispatched-from-location)))
            (can-go-to
              (case rpn-bits
                (jump-xct-next   (make-jump-xct-next-successor dispatch-address next-instruction))
                (jump            (make-dispatch-jump-successor dispatch-address))
                (call-xct-next   (make-call-successor dispatch-address return-address return-address-if-xct-next))
                (call            (make-call-successor dispatch-address '() return-address))
                (return-xct-next (make-return-successor '() next-instruction))
                (return          (make-return-successor '() '()))
                (fall-through    (if popj-after-next
                                     (make-return-successor '() next-instruction)
                                     (make-dispatch-fall-through-successor next-instruction)))
                (skip            (if popj-after-next
                                     (make-return-successor '() '()) ;; Is this right?
                                     (make-dispatch-skip-successor after-next-instruction)))))
            )))
    ans))

(defun compute-predecessors ()
  (for-each-instruction
    #'(lambda (instruction address)
        address ;; is not needed here.
        (dolist (successor (instruction-successors instruction))
          (unless (null? successor)
            (labels (
              (preceeds (predecessor successor)
                (unless (null? successor)
                  (push predecessor (instruction-predecessors successor))))
              )
              (if (instruction? successor)
                  (preceeds instruction successor)
                  (case (first successor)
                    (calls          (preceeds `(CALLED ,instruction
                                                       ,(instruction-xct-along-the-way successor)
                                                       ,(instruction-returned-to successor))
                                              (instruction-xfered-to successor))
                                    (preceeds `(XCT-NEXTED ,instruction) (instruction-xct-along-the-way successor))
                                    (preceeds `(RETURNED-TO ,instruction) (instruction-returned-to successor)))

                    (jump-xct-next  (preceeds `(JUMP-XCT-NEXTED ,instruction
                                                                ,(instruction-xct-along-the-way successor))
                                              (instruction-xfered-to successor))
                                    (preceeds `(XCT-NEXTED ,instruction) (instruction-xct-along-the-way successor)))

                    (returns        (preceeds `(XCT-NEXTED ,instruction) (instruction-xct-along-the-way successor)))))))))))

(defun for-instruction-successors (predicate operation)
  (for-each-instruction
    #'(lambda (instruction address)
        address;; is ignored
        (dolist (successor (instruction-successors instruction))
          (when (funcall predicate successor)
            (funcall operation successor))))))

(defun find-subroutine-entries ()
  (setq *subroutine-entries* '())
  (for-instruction-successors
    #'call-successor?
    #'(lambda (call-successor)
        (pushnew (instruction-xfered-to call-successor) *subroutine-entries*))))

(defun find-instructions-without-predecessors ()
  (setq *instructions-without-predecessors* '())
  (for-each-instruction
    #'(lambda (instruction address)
        address ;; is ignored
        (when (null? (instruction-predecessors instruction))
          (unless (member (instruction-address instruction)
                          *dont-count-these-instructions-in-those-that-have-no-predecessors*)
            (push instruction *instructions-without-predecessors*))))))

(defun find-basic-blocks ()
  (setq *basic-block-info* '())
  (for-each-instruction
    #'(lambda (instruction address)
        (check-basic-blockness instruction address)))
  (setq *basic-block-info* (cons *trace-lmc-version-number* (list *basic-block-info*))))

;;; Doesn't seem to be any reasonable way to do this.

(defun check-basic-blockness (instruction address)
  (let ((predecessors (instruction-predecessors instruction)))
    (labels (
        (is-block ()
          (push address *basic-block-info*))
        (isnt-block () (values)))
      (if (or (null? predecessors)
              (cdr predecessors))
          (is-block)
          (let ((p (car predecessors)))
            (if (list? p)
                (ecase (car p)
                  (called      (is-block))
                  ;; this is wrong, but probably what is wanted.
                  (returned-to (is-block))
                  ((jump-xct-nexted xct-nexted)
                   (if (null? (cdr (instruction-successors (cadr p))))
                       (isnt-block)
                       (is-block))))
                (if (null? (cdr (instruction-successors p)))
                    (isnt-block)
                    (is-block))))))))

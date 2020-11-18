;;; -*- Mode:LISP; Package:K2; Readtable:CL; Base:10. -*-

;;;; Warm Loader

;;; After the cold load is running
;;; the warm loader is called (through the debugger)
;;; to load more stuff

;;; We get input from the COLD K FASL stream.
;;; The FASLOADER reads a binary file, but we are going to
;;; break with tradition here.
;;; The K fasload format is a series of opcodes and data.
;;; The first thing in the file is an opcode and the data is freeform
;;; after the opcodes.

(defun mini-fasl-peek-byte ()
  (%kbug-stream-peek-byte kbug-k-input-fasl-stream (kbug-stream-out-pointer kbug-k-input-fasl-stream)))

(defun mini-fasl-read-byte ()  ;;This is an optimized version  --wkf
  (setq gr:*mini-fasl-byte-counter* (1+ gr:*mini-fasl-byte-counter*))
;;;  (kbug-stream-read-byte kbug-k-input-fasl-stream)  ;;This is the old version.
  (let ((out-ptr (kbug-stream-out-pointer kbug-k-input-fasl-stream)))
    (prog1 (%kbug-stream-peek-byte kbug-k-input-fasl-stream out-ptr)
           (setf (kbug-stream-out-pointer kbug-k-input-fasl-stream)
                 (let ((new-pointer (1+ out-ptr)))
                   (if (>= new-pointer (+ kbug-input-fasl-stream-base kbug-stream-buffer-size))
                       kbug-input-fasl-stream-base
                     new-pointer))))))

(defun mini-fasl-read-opcode ()
  (mini-fasl-read-byte))

(defun mini-fasl-peek-opcode () "Same as mini-fasl-peek-byte"
  (%kbug-stream-peek-byte kbug-k-input-fasl-stream (kbug-stream-out-pointer kbug-k-input-fasl-stream)))

(defun mini-fasl-read-16-bits ()
  (let ((low-bits  (mini-fasl-read-byte))
        (high-bits (mini-fasl-read-byte)))
    (hw:dpb high-bits (byte 8. 8.) low-bits)))

(defun mini-fasl-read-fixnum ()
  (let ((low-bits    (mini-fasl-read-byte))
        (medium-bits (mini-fasl-read-byte))
        (high-bits   (mini-fasl-read-byte)))
    (hw:dpb high-bits
            (byte 8. 16.)
            (hw:dpb medium-bits
                    (byte 8. 8.)
                    low-bits))))

(defun mini-fasl-read-bignum ()
  (let* ((words-needed (mini-fasl-read-fixnum))
         (bignum (new-math:allocate-bignum words-needed)))
    (do ((i 1 (1+ i)))
        ((> i words-needed))
      (hw:write-md-unboxed
        (vinc::dpb-multiple-unboxed
          (mini-fasl-read-byte) (byte 8. 0)
          (mini-fasl-read-byte) (byte 8. 8.)
          (mini-fasl-read-byte) (byte 8. 16.)
          (mini-fasl-read-byte) (byte 8. 24.)
          0))
      (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ i bignum))
      )
    bignum
    )
  )

(defun mini-fasl-read-cons ()
  (cons:cons (mini-fasl-read-object) (mini-fasl-read-object)))

;;; avoid recursion when reading lists
(defun mini-fasl-read-list ()
  (let ((length (mini-fasl-read-fixnum)))
    (let ((l '()))
      (let ((tail l))
        (dotimes (i length)
          (let ((cons (cons:cons (mini-fasl-read-object)
                                    nil)))
            (if tail
              (cons:rplacd tail cons)
              (setq l cons))
            (setq tail cons))))
      l)))

(defun mini-fasl-read-string-character ()
  (cons:make-pointer vinc:$$dtp-character (mini-fasl-read-byte)))

(defun mini-fasl-read-simple-string ()
  (let ((length (mini-fasl-read-fixnum)))
    (let ((string (array::make-string length)))
      (dotimes (i length)
        (array::aset-1 (mini-fasl-read-string-character) string i))
      string)))

(defun mini-fasl-read-simple-vector ()
  (let ((length (mini-fasl-read-fixnum)))
    (let ((vector (array::make-vector length)))
      (dotimes (i length)
        (setf (array:svref vector i) (mini-fasl-read-object)))
      vector)))

;;; Only called by mini-fasl's loop to record things about top-level fasl objects
(defun mini-fasl-read-top-level-object ()
  (let ((opcode (mini-fasl-read-byte)))
    (setq gr:*mini-fasl-top-level-opcode-byte-count* gr:*mini-fasl-byte-counter*)
    (setq gr:*mini-fasl-top-level-opcode* opcode)
    (mini-fasl-read-object-1 opcode)))

(defun mini-fasl-read-object ()
  (let ((opcode (mini-fasl-read-byte)))
    (mini-fasl-read-object-1 opcode)))

(defun mini-fasl-read-object-1 (opcode)
  (dispatch (byte 5 0) opcode
     ($$fasl-op-end-of-file       (li:tail-error "Unexpected EOF in MINI-FASLOAD"))
     ($$fasl-op-string            (mini-fasl-read-simple-string))
     ($$fasl-op-fixnum            (mini-fasl-read-fixnum))
     ($$fasl-op-bignum            (mini-fasl-read-bignum))
     ($$fasl-op-symbol            (mini-fasl-read-symbol))
     ($$fasl-op-defun             (mini-fasl-read-defun))
     ($$fasl-op-defafun           (mini-fasl-read-defun))
     ($$fasl-op-defmacro          (mini-fasl-read-defmacro))
     ($$fasl-op-defsubst          (mini-fasl-read-defsubst))
     ($$fasl-op-compiled-function (mini-fasl-read-function))
     ($$fasl-op-cons              (mini-fasl-read-cons))
     ($$fasl-op-list              (mini-fasl-read-list))
     ($$fasl-op-nil               'nil)
     ($$fasl-op-short-float       (mini-fasl-read-short-float))
     ($$fasl-op-single-float      (mini-fasl-read-single-float))
     ($$fasl-op-double-float      (mini-fasl-read-double-float))
     ($$fasl-op-defvar            (mini-fasl-do-defvar))
     ($$fasl-op-string-character  (mini-fasl-read-string-character))
     ($$fasl-op-defconstant       (mini-fasl-do-defconstant))
     ($$fasl-op-defparameter      (mini-fasl-do-defparameter))
     ($$fasl-op-eval              (mini-fasl-fake-eval))
     ($$fasl-op-unbound           (li:tail-error "MINI-FASL-READ-OBJECT can't cope with FASL-OP-UNBOUND."))
     ($$fasl-op-simple-vector     (mini-fasl-read-simple-vector))
     (t                           (li:tail-error "Mini-fasl-opcode-dispatch is missing!" opcode))))

(defun mini-fasl ()
  (setq gr:*mini-fasl-byte-counter* 0)
  (setq gr:*mini-fasl-top-level-opcode-byte-count* gr:*mini-fasl-byte-counter*)
  (setq gr:*mini-fasl-top-level-opcode* -1)
  (labels ((fasl-loop (last-object)
             (if (= $$fasl-op-end-of-file (%kbug-stream-peek-byte kbug-k-input-fasl-stream
                                                                  (kbug-stream-out-pointer kbug-k-input-fasl-stream)))
                 (progn (mini-fasl-read-byte)
                        (loop)                  ; wait here at eof
                        last-object)
                 (fasl-loop (mini-fasl-read-top-level-object)))))
    (fasl-loop nil)))

(defun kbug-fasl-stream ()                      ;implements KBUG-COMMAND-FASL-STREAM
  (setq gr:*mini-fasl-byte-counter* 0)
  (setq gr:*mini-fasl-top-level-opcode-byte-count* gr:*mini-fasl-byte-counter*)
  (setq gr:*mini-fasl-top-level-opcode* -1)
  (labels ((fasl-loop (last-object)
             (if (= $$fasl-op-end-of-file (%kbug-stream-peek-byte kbug-k-input-fasl-stream
                                                                  (kbug-stream-out-pointer kbug-k-input-fasl-stream)))
                 (progn (mini-fasl-read-byte)   ; consume the EOF opcode
                        last-object)
               (fasl-loop (mini-fasl-read-top-level-object)))))
    (fasl-loop nil)))

(defun mini-fasl-read-symbol ()
  (warm-intern (mini-fasl-read-object)
               (mini-fasl-read-object)))

(defun Cold-load-linkedp () "Returns nil if the cold load is not yet linked."
  "t")

;;; $$$ Removed new scheme for *warm-symbols* <22-Nov-88 wkf>
;(defun make-package-entry (pname package)
;  (let ((symbol (symbol::%make-symbol pname)))
;    (setf (symbol::symbol-package symbol) package)
;    (li:cons package symbol)))

;(defun warm-find-oblist-entry (pname) "Returns '(\"FOO\" . ((\"LISP-INTERNALS\" . li:foo) (\"GLOBAL\" . global:foo)))"
;  (li:do ((rest gr:*warm-symbols* (li:cdr rest)))
;        ((li:null rest)
;         (let ((new (li:cons pname nil)))
;           (li:push new gr:*warm-symbols*)))
;    (when (array::%string= pname (li:caar rest))
;      (return (li:car rest)))))

;; symlist is  '(("CAR" . CONS:CAR) ("CDR" . CONS:CDR))
;;we add ("GLOBAL" . CONS:CAR) , etc to *warm-symbols*
;(defun warm-fix-global-symbols (symlist)
;  (li:dolist (pair symlist)
;    (let ((entry (warm-find-oblist-entry (li:car pair))))
;      (if (or (li:null (li:cdr entry))
;             (li:cdr  (li:cdr entry)))
;         ;;we expect that each symbol has been interned once while fasl built our argument
;         ;;therefore, each symbol should have exactly one package
;         (li:error "warm symbols bad" gr:*warm-symbols*))
;      (setf (li:cddr entry) (li:cons (li:cons "GLOBAL" (li:cdr pair))  ; $$$ LI:CONS <22-Nov-88 wkf>
;                                    (li:cddr entry))))))

;;; $$$ Put back old method. <22-Nov-88 wkf>
(defun warm-intern (pname package)
  (if (eq gr:*warm-symbols* gr:*t*)
      (li:intern pname package)
    (li::dolist (symbol gr:*warm-symbols*
                        (progn (setq symbol (symbol::%make-symbol pname))
                               (setf (symbol::symbol-package symbol) package)
                               (setq gr:*warm-symbols* (cons:cons symbol gr:*warm-symbols*))
                               symbol))
      (when (and (array::%string= pname   (symbol::symbol-name symbol))
                 (array::%string= package (symbol::symbol-package symbol)))
        (return symbol)))))

;;; $$$ Removed new method. <22-Nov-88 wkf>
         ;;Gr:*warm-symbols* is of the form '(("CAR" . (("LISP-INTERNALS" . li:car) ("GLOBAL" . li:car) ("FOO" . foo:car)))
         ;;                                     ("BAR" . (("GLOBAL" . global:bar))))
         ;; $$$ Rewrote to allow refering to the same symbol via different packages (only by explicit setup) <21-Nov-88 wkf>
;        (let ((entry (warm-find-oblist-entry pname)))
;          (li:do ((pkg-sym (li:cdr entry) (li:cdr pkg-sym)))
;                 ((li:null pkg-sym)
;                  (let ((pkg-entry (make-package-entry pname package)))
;                    ;; @@@ Maybe put new ones at the end of list? <22-Nov-88 wkf>
;                    (setf (li:cdr entry) (li:cons pkg-entry (li:cdr entry)))   ;; $$$ Fixed bug. <22-Nov-88 wkf>
;                    (li:cdr pkg-entry)))
;            (when (array::%string= package (li:car (li:car pkg-sym)))
;              (return (li:cdr (li:car pkg-sym))))))



(defun mini-fasl-read-defsubst ()
  (mini-fasl-read-object) ;Throw the source away for now (PLIST eventually).
  (mini-fasl-read-defun))

(defun mini-fasl-read-defmacro ()
  (let ((fname  (mini-fasl-read-defun)))
    (setf (symbol:symbol-function fname)
          (cons:cons 'LISP-INTERNALS:MACRO
                     (symbol:symbol-function fname)))))

(defun mini-fasl-read-defun ()
  (let ((name     (mini-fasl-read-object))
        (function (mini-fasl-read-object)))
    name))


(defun mini-fasl-read-cold-info ()
  (setq gr:*mini-fasl-byte-counter* 0)
  (setq gr:*mini-fasl-top-level-opcode-byte-count* gr:*mini-fasl-byte-counter*)
  (setq gr:*mini-fasl-top-level-opcode* -1)
  (dotimes (nfcns (mini-fasl-read-fixnum))
     (mini-fasl-read-cold-fcn-info))
  (loop))

(defun kbug-load-cold-info ()                   ;implements KBUG-COMMAND-LOAD-COLD-INFO
  (setq gr:*mini-fasl-byte-counter* 0)
  (setq gr:*mini-fasl-top-level-opcode-byte-count* gr:*mini-fasl-byte-counter*)
  (setq gr:*mini-fasl-top-level-opcode* -1)
  (dotimes (nfcns (mini-fasl-read-fixnum))
    (mini-fasl-read-cold-fcn-info)))

(defun read-code-word (addr)
  ;; ??
  (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
  (hw:read-md))

;;; code is in read only regions
(defun write-code-word (addr value)
   (map-fault:call-while-allowing-write-in-read-only
     #'(lambda ()
         (hw:write-md-unboxed value)
         (hw:vma-start-write-no-gc-trap-unboxed addr)
         nil)))

(defun write-call-address (addr value)
  (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
  (hw:write-md-unboxed (hw:dpb value (byte 24. 0.) (hw:read-md)))
   (map-fault:call-while-allowing-write-in-read-only
     #'(lambda ()
         (hw:vma-start-write-no-gc-trap-unboxed addr)
         nil)))

(defun read-call-address (addr)
  (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
  (hw:ldb (hw:read-md) (byte 24. 0.) 0))

(defun write-instruction (address high-half low-half)
    (map-fault:call-while-allowing-write-in-read-only
      #'(lambda ()

          (hw:write-md-unboxed low-half)
          (hw:vma-start-write-no-gc-trap-unboxed address)

          ;; Write the high half
          (hw:write-md-unboxed high-half)
          (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 1 address))
          nil)))

(defun write-boxed-immediate (address immediate)
  (map-fault:call-while-allowing-write-in-read-only
    #'(lambda ()
        (hw:write-md-boxed immediate)
        (hw:vma-start-write-boxed address)
        nil
        )))


(defun warm-read-unboxed-word ()
  (let ((low-half (mini-fasl-read-16-bits))
        (high-half (mini-fasl-read-16-bits)))
    (hw:dpb-unboxed high-half (byte 16. 16.) low-half)))

(defun read-local-refs ()
  (let* ((n (mini-fasl-read-fixnum))
         (len (+ n n))
         (refs (array:make-vector len)))
      (do ((i 0 (+ i 2)))
          ((>= i len))
        (setf (array:svref refs i)      (mini-fasl-read-fixnum))
        (setf (array:svref refs (1+ i)) (mini-fasl-read-fixnum)))
      refs))

(defun read-refs ()
  (let* ((n (mini-fasl-read-fixnum))
         (len (+ n n n))
         (refs (array:make-vector len)))
      (do ((i 0 (+ i 3)))
          ((>= i len))
        (setf (array:svref refs i)      (mini-fasl-read-fixnum))      ;number of args
        (setf (array:svref refs (1+ i)) (mini-fasl-read-object))      ;ref offset
        (setf (array:svref refs (+ i 2)) (mini-fasl-read-fixnum)))    ;function called
      refs))

(defun read-entry-points ()
  (let* ((n (mini-fasl-read-fixnum))
         (len (+ n n))
         (entries (array:make-vector len)))
      (do ((i 0 (+ i 2)))
          ((>= i len))
        (setf (array:svref entries i)      (mini-fasl-read-fixnum))    ;number of args
        (setf (array:svref entries (1+ i)) (mini-fasl-read-fixnum)))   ;entry point offset
      entries))


user::
(prims::defmacro k2::define-accessors (object-var value-var &rest slot-list)
  (do ((slot-tail slot-list (rest slot-tail))
       (number 0         (1+ number))
       (code  '()  (let ((slot (first slot-tail)))
                     (let ((access-name (first slot))
                           (setter-name (second slot)))
                       (append `((K2::DEFSETF ,access-name ,setter-name)
                                 (K2::DEFUN ,setter-name (,object-var ,value-var)
                                   (CONS:STORE-CONTENTS-OFFSET ,object-var ,number ,value-var))
                                 (k2::DEFUN ,access-name (,object-var)
                                   (CONS:CONTENTS-OFFSET ,object-var ,number)))
                               code)))))
      ((null slot-tail) `(k2::PROGN ,@(reverse code)))))

;;;; -starting-address is memory address
;;;; -code is a code pointer (pc)
;;;; other than that there's probably not
;;;; much difference, maybe -s-a should go away.
(define-accessors compiled-function value
  (%compiled-function-header            %set-compiled-function-header)
  (%compiled-function-name              %set-compiled-function-name)
;  (%compiled-function-starting-address         %set-compiled-function-starting-address)
  (%compiled-function-entry-points      %set-compiled-function-entry-points)
        ;a vector with paired entries, <number-of-args> <entry-point-offset>
  (%compiled-function-local-refs        %set-compiled-function-local-refs)
        ;a vector with paired entries, <iaddr> <toffset>
        ; <iaddr> is the relative instruction address to be modified.
        ; <toffset>, if positive, is to be added to start-pc and put into %%i-branch-address
        ;                of that inst.
        ;            if negative, add start-pc and abs(toffset), supply data type dtp-code,
        ;               and smash that into the low word of the instruction.
  (%compiled-function-refs              %set-compiled-function-refs)
        ;a vector with 3-entries. <iaddr> <called function-spec> <nargs>.
        ; <iaddr> is relative instruction address to be modified (i.e. the call instruction).
        ;<called function-spec> can be a symbol, a dtp-compiled-function, or a CONS.
        ;   if a CONS, CAR had better be :INTERNAL. (separate error check for LI:MACRO)
        ;              CADR is <recursively> a <function-spec>.
        ;              CADDR is a <name extension> which is compared with EQ versus other
        ;                <name extension>s.
  (%compiled-function-length            %set-compiled-function-length)
  (%compiled-function-code              %set-compiled-function-code))

;  (%compiled-function-resurrected-code %set-compiled-function-resurrected-code))

(defsubst %compiled-function-p (ptr)
  (hw:field= ptr
             (hw:dpb-unboxed $$dtp-compiled-function vinc:%%data-type (hw:unboxed-constant 0))
             vinc:%%data-type))

(defconstant compiled-function-structure-size 8.)

(defun make-compiled-function (name entry-points
                               local-refs refs
                               length)
  (let ((function
          (cons:allocate-structure compiled-function-structure-size 0
                                   $$dtp-compiled-function
                                   (cons:make-header $$dtp-compiled-function-header length)
                                   )))

    (setf (%compiled-function-name         function)             name)
    (setf (%compiled-function-entry-points function)     entry-points)
    (setf (%compiled-function-local-refs   function)       local-refs)
    (setf (%compiled-function-refs         function)             refs)
    (setf (%compiled-function-length       function)           length)
    function))

;;; this should probably be in vinc
(defun addr->pc (address)
  (hw:ldb-boxed
    address
    (byte 24. 1.)
    gr:*trap-dtp-code-5*))

(defun pc->addr (pc)
  (trap:without-traps
    #'(lambda ()
        (cons:make-pointer
          vinc:$$dtp-unboxed-locative
          (hw:32set-bit (1- (byte-size vinc:%%pointer))
                        (hw:32+ pc pc))))))


(defun mini-fasl-read-cold-fcn-info ()
  (let* ((name         (mini-fasl-read-object))
         (local-refs   (read-local-refs))
         (refs         (read-refs))
         (entry-points (read-entry-points))
         (length       (mini-fasl-read-fixnum))
         (pc           (mini-fasl-read-fixnum))
         (starting-addr (pc->addr pc))
         (function (make-compiled-function name entry-points
                                            local-refs refs
                                            length)))
    (setf (%compiled-function-code function)
          (cons:make-pointer vinc:$$dtp-code pc))
;    (setf (%compiled-function-starting-address function) starting-addr)
    (when (symbolp name)
      (setf (symbol:symbol-function name) function))
    (when (>= pc 64.)
      (map-fault:call-while-allowing-write-in-read-only
        #'(lambda ()
            (hw:write-md-unboxed cons:code-header-instruction-high)
            (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ -1 starting-addr))
            (cons:store-contents-offset starting-addr -2 function))))
    (read-and-link-immediates starting-addr)))



(defun mini-fasl-read-function ()
  (let ((name         (mini-fasl-read-object))
        (local-refs   (read-local-refs))
        (refs         (read-refs))
        (entry-points (read-entry-points))
        (length       (mini-fasl-read-fixnum)))
    (let ((function (make-compiled-function name entry-points
                                            local-refs refs
                                            length)))
      (let ((address (cons:allocate-code-space length function gr:*default-code-area*)))
        (let ((code-addr (hw:24+ 2 address)))
;         (setf (%compiled-function-starting-address function) code-addr)
          (setf (%compiled-function-code function) (addr->pc code-addr))
          (do ((i 0 (1+ i))
               (addr code-addr (hw:24+ 2 addr)))
              ((>= i length))
            (let ((low-half (warm-read-unboxed-word))
                  (high-half (warm-read-unboxed-word)))
              (write-instruction addr high-half low-half)))
          (read-and-link-immediates code-addr)
          (link-function function code-addr)))
      (when (symbolp name)
        (setf (symbol:symbol-function name) function))
      function)))

(defun kill-old-function (function)
  (when (%compiled-function-p function)
  ;; $$$ Temporary for debugging <22-Nov-88 wkf>
  ;; $$$ Remove temporary test. <22-Nov-88 JIM>
  ;;(trap:illop "Tried to kill a function")
    (do ((count  (%compiled-function-length function) (1- count))
         (fptr   (hw:24+ 1 (pc->addr (%compiled-function-code function))) (hw:24+ 2 fptr)))
        ((zerop count))
      (write-code-word fptr (hw:32set-bit (- (byte-position hw:%%i-trap-bit) 32.)
                                          (read-code-word fptr))))))

(defun read-and-link-immediates (base)
  (dotimes (i (mini-fasl-read-fixnum))
    (write-boxed-immediate
      (hw:24+ (ash (mini-fasl-read-fixnum) 1.) base)
      (mini-fasl-read-object))))

(defun link-function (function address)
  (relocate-local-refs function address)
  (link-refs function address))

(defun relocate-local-refs (cfun starting-address)
  (let* ((local-refs (%compiled-function-local-refs cfun))
         (length (array:length local-refs))
         (start-pc (addr->pc starting-address)))
    (do ((i 0 (+ i 2)))
        ((>= i length))
      (let ((iaddr (hw:24+ (ash (array:svref local-refs i) 1.)
                           starting-address))
            (toffset (array:svref local-refs (1+ i))))
        (if (not (minusp toffset))
            (write-code-word
              iaddr
              (hw:dpb-unboxed (hw:24+ toffset start-pc)
                              hw:%%i-branch-address
                              (read-code-word iaddr)))
            ;; negative offset means pc ref (imm32)
            (write-boxed-immediate
              iaddr
              (cons:make-pointer $$dtp-code
                                 (hw:24- start-pc toffset))))))))


;(defun get-entry-point (fcn nargs callee)
;  (let ((entry-points (%compiled-function-entry-points fcn)))
;    (let ((length (array:length entry-points)))
;      (do ((i 0 (+ i 2)))
;         ((>= i length)
;          (li:error "Function call with wrong number of args"
;                  callee
;                  (%compiled-function-name fcn)
;                  nargs)
;          nil)
;       (let ((ep-nargs (array:svref entry-points i)))
;         (when (or (= ep-nargs nargs)
;                   (and (minusp ep-nargs)
;                        (>= nargs (- ep-nargs))))
;           (return (array:svref entry-points (1+ i)))))))))


(defun get-entry-address-for-funcall (fcn nargs)
  (let* ((entry-points (%compiled-function-entry-points fcn))
         (start-pc     (resurrect-function-if-dead fcn))
         (length       (array:length entry-points)))
    (do ((i 0 (+ i 2)))
        ((>= i length)
         (values (hw:24+ (array:svref entry-points (- length 1)) start-pc)
                 (array:svref entry-points (- length 2))))
      (let ((entry-nargs (array:svref entry-points i)))
        (cond ((= nargs entry-nargs)
               (return (values (hw:24+ (array:svref entry-points (1+ i)) start-pc)
                               entry-nargs)))
           ;the following COND clause added by RG 3/6/88.  Doesnt seem to help tho.
           ;problem is, it seems to always go to the last entry if there is a rest arg,
           ;which can do the wrong thing if there are optional args with non-null inits
           ;and no supplied args for them (supplied-p variables also lose.)
              ((and (minusp entry-nargs)        ;if frob is a rest arg, go there if there are enuf
                    (>= nargs (1- (- entry-nargs))))  ;args to cover the spread args.
               (return (values (hw:24+ (array:svref entry-points (1+ i)) start-pc)
                               entry-nargs))))))))


(defun get-entry-address (called-function nargs calling-function call-addr)
  (let* ((entry-points (%compiled-function-entry-points called-function))
         (start-pc     (resurrect-function-if-dead called-function))
         (length (array:length entry-points)))
      (do ((i 0 (+ i 2)))
          ((>= i length)
           (li:error "Function call with wrong number of args: A1 calls A2 with A3 args A4 entry-points"
                   calling-function
                   (%compiled-function-name called-function)
                   nargs
                   entry-points)
           (get-wna-function-reference))
        (let ((ep-nargs (array:svref entry-points i)))
          (cond ((= ep-nargs nargs)
                 (return (hw:24+ (array:svref entry-points (1+ i))
                                 start-pc)))
                ((and (minusp ep-nargs)
                      (>= nargs (1- (- ep-nargs))))
                 (return (create-rest-arg-link
                           nargs
                           (hw:24+ (array:svref entry-points (1+ i))
                                   start-pc)
                           call-addr))))))))


(defun dead-code-p (ptr)
  ;this is really TRAP-BIT-SET-P.
  (hw:32logbitp (- (byte-position hw:%%i-trap-bit) 32.) (array:%vm-read32 ptr 1)))

;(defun function-really-dead-p (fcn)            ;||rg: kludge added 10/23/88 to try to fix things.
;  ;say its dead if ALL instructions in it are marked "dead".
;  (cond ((eq fcn #'undefined-function)
;        nil)
;       (t
;        (let* ((start-pc (%compiled-function-code fcn))
;               (code-ptr (pc->addr start-pc))
;               (length   (%compiled-function-length fcn)))
;          (dotimes (i length t)
;            (if (null (dead-code-p code-ptr))
;                (return nil))
;            (setq code-ptr (hw:24+ 2 code-ptr)))))))


(defun resurrect-function-if-dead (fcn)
  (let* ((start-pc     (%compiled-function-code fcn))
         (code-ptr     (pc->addr start-pc)))
    (cond
      ((eq fcn #'undefined-function)
       start-pc)
      ((dead-code-p code-ptr)
       (let* ((length     (%compiled-function-length fcn))
              (r-code-ptr (hw:24+ 2 (cons:allocate-code-space length fcn gr:*default-code-area*)))
              (r-start-pc (addr->pc r-code-ptr)))
         (let ((to-code-ptr r-code-ptr))
           (dotimes (i length)
             (write-instruction to-code-ptr
                                (hw:32clear-bit (- (byte-position hw:%%i-trap-bit) 32.)
                                                (array:%vm-read32 code-ptr 1))
                                (array:%vm-read32 code-ptr 0))
             (setq code-ptr   (hw:24+ 2 code-ptr))
             (setq to-code-ptr (hw:24+ 2 to-code-ptr))))
         (relocate-local-refs fcn r-code-ptr)
         (setf (%compiled-function-code fcn) r-start-pc)
         r-start-pc))
      (t start-pc))))

;;; This creates the two instruction link to a rest arg function
;;; it looks like this:
;;;
;;;  (MOVEI GR:*ARG-1* <nargs> BOXED)
;;;  (JUMP <fcn> ())
;;;
;;; the header of the link contains a locative to the original call instruction
;;; you can tell it from the code of a real function by the locative data type.
;;;
(defun create-rest-arg-link (nargs callee-pc call-addr)
  ;; maybe a different area for these would be fun
  (let ((addr (hw:24+ 2 (cons:allocate-code-space 2 call-addr gr:*default-code-area*))))
    (write-code-word addr nargs)
    (write-code-word (hw:24+ 1 addr)
                     (hw:unboxed-constant
                       #.(lisp:ash (nc:assemble-inst '(MOVEI GR:*ARG-1* 0 BOXED))
                                   -32)))
    (write-code-word (hw:24+ 2 addr) (hw:dpb-unboxed
                                       callee-pc
                                       hw:%%i-jump-address
                                       (hw:unboxed-constant
                                         #.(lisp:logand (nc:assemble-inst '(JUMP 0 ()))
                                                        #xFFFFFFFF))))

    (write-code-word (hw:24+ 3 addr)
                     (hw:unboxed-constant
                       #.(lisp:ash (nc:assemble-inst '(JUMP 0 ()))
                                   -32)))
    (addr->pc addr)))


(defafun get-wna-function-reference ()
  (movea return wna-function next-pc-return ch-return boxed))

(defun wna-function ()
  (li:error "Function call with wrong number of args"))

(defun link-refs (function starting-address)
  (let* ((refs (%compiled-function-refs function))
         (length (array:length refs)))
    (do ((i 0 (+ i 3)))
        ((>= i length))
      (let ((ref-addr (hw:24+ (ash (array:svref refs i) 1.) starting-address))
            (called-fcn-spec (array:svref refs (+ i 1))))
        (let ((called-fcn (li:find-function called-fcn-spec)))
        (write-code-word
                ref-addr
                (hw:dpb (if called-fcn
                            (get-entry-address  called-fcn
                                                (array:svref refs (+ i 2))
                                                function
                                                ref-addr)
                          (get-undefined-function-reference))
                        hw:%%i-jump-address
                        (read-code-word ref-addr))))))))



(defafun get-undefined-function-reference ()
  (movea return undefined-function ch-return next-pc-return boxed))

;;; References to functions which are undefined at link time
;;; are linked to this function.  It will attempt to find the
;;; function at run time and jump to it.  The link to this
;;; will be snapped so the next time the called function will
;;; be jumped to directly.

(defafun undefined-function ()
  (tail-open-call (undefined-function 0) () itrap-1)) ;cause an instruction trap!!!

;(defafun assure-currently-unsnapped-link (current-jump-pc)
;  (movea a1 undefined-function boxed)
;  (alu l-r nop a0 a1 bw-24 unboxed)
;  (test br-not-equal)
;  (branch lose ())
;  (return a0)
; lose
;  (open-call (trap-bomb 0) a15 ())             ;illop only works conveniently defuns
;  (unconditional-branch lose ())
;  )

(defafun call-to-undefined-function-p (call-address)
  (movea a1 undefined-function boxed)
  (alu l-r nop a0 a1 bw-24 unboxed)
  (test br-not-equal)
  (branch no ())
  (return gr:*t*)
 no
  (return gr:*nil*))

(defun trap-bomb ()
  (trap:illop "False IR62 trap."))      ;got an IR62 trap, but IR62 was not set on inspection.

;(call-to-undefined-function-p (read-call-address (pc->addr call-pc)))

(defun fix-undefined-function (call-pc call-pc-+)
  ;call-pc should have PC of call instruction.
  ;call-pc-+ should have PC of instruction which was called to.  It should have IR-62 on.
  ; It seems this could lose if you were returnning to a "dead" function.  contents(call-pc-+)
  ; would still be dead, but contents (call-pc) would be a return not a call.  O well, this whole scheme is a loss. --rg.
  (cond ((not (dead-code-p (pc->addr call-pc-+)))       ;||rg: crosscheck added 10/23/88.
         (prog2 (trap-bomb) nil)))
  (multiple-value-bind (callee offset)
      (get-function-and-offset-from-pc call-pc)
    (when (null callee)
      (trap:illop "Undef function before cold symbols warm loaded"))
    (resurrect-function-if-dead callee)         ;if it has been redefined
    (multiple-value-bind (fcn-name nargs)
        (get-called-function-and-nargs offset callee)
      ;; symbol?
      (let ((fcn (li:find-function fcn-name)))
        (if (%compiled-function-p fcn)
            (let ((call-addr (pc->addr call-pc)))
              (let ((entry-addr (get-entry-address fcn nargs callee call-addr)))
                ;; snap link
                (write-call-address
                  call-addr
                  entry-addr)
                nil))
          (li:error "Calling undefined function" (symbol:symbol-package fcn-name) (symbol:symbol-name fcn-name)))))))

;;; Given a PC which points into the code of a function,
;;; return the compiled function object.  Do this by
;;; searching back in the code until we get to the illegal
;;; instruction which marks the beginning of the function
;;; and contains a back pointer in its low word.
;;;  This is slightly complicated by the fact that the pc
;;; could point into a rest-link in which case the header will
;;; point back to the real call in the real function.
(defvar *special-trap-functions*
             '(trap:trap trap:non-modifying-exit   trap:modifying-exit
                         trap:diagnostic-trap-exit trap:trap-vector-table))

(defun get-function-and-offset-from-pc (pc)
  (if (< (vinc:make-fixnum pc) 64.) ;;|| 10/13/88 --wkf
      (trap:illop "Don't try to link to TRAP code!!!")
    (do ((addr (hw:24+ -1 (pc->addr pc))
               (hw:24+ -2 addr))
         (offset 0 (1+ offset)))
        ((hw:32= (progn
                   (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
                   (hw:read-md))
                 cons:code-header-instruction-high)
         (let ((back-pointer (cons:contents
                               (hw:24+ -1 addr))))
           (cond
             ;; Normal functions
             ((vinc:type-test back-pointer vinc:$$dtp-compiled-function)
              (values back-pointer offset))
             ;; look back again starting from real call (&REST links)
             ((vinc:type-test back-pointer vinc:$$dtp-unboxed-locative)
              (get-function-and-offset-from-pc (addr->pc back-pointer)))
             ;; this should only happen before cold symbols are warm loaded
             (t nil)))))))

;(defun get-compiled-function-from-pc (pc)
;  (if (< (vinc:make-fixnum pc) 64.)
;      (trap:illop "Don't try to link to TRAP code!!!")
;    (do ((addr (hw:24+ -1 (pc->addr pc))
;              (hw:24+ -2 addr)))
;       ((hw:32= (progn
;                  (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
;                  (hw:read-md))
;                cons:code-header-instruction-high)
;        (let ((back-pointer (cons:contents
;                              (hw:24+ -1 addr))))
;          (cond
;            ;; Normal functions
;            ((vinc:type-test back-pointer vinc:$$dtp-compiled-function)
;             (values back-pointer pc))
;            ;; look back again starting from real call (&REST links)
;            ((vinc:type-test back-pointer vinc:$$dtp-unboxed-locative)
;             (get-compiled-function-from-pc (addr->pc back-pointer)))
;            ;; this should only happen before cold symbols are warm loaded
;            (t nil)))))))



;(defun get-offset-in-fcn (fcn pc)
;  (let* ((offset1 (hw:32- pc (%compiled-function-code fcn)))
;        (r-code  (%compiled-function-resurrected-code fcn))
;        (offset2 (hw:32- pc r-code)))
;    (cond
;      ((null r-code)
;       (hw:ldb offset1 vinc:%%fixnum-field 0))
;      ((hw:32< offset1 (hw:unboxed-constant 0))
;       (hw:ldb offset2 vinc:%%fixnum-field 0))
;      ((hw:32< offset2 (hw:unboxed-constant 0))
;       (hw:ldb offset1 vinc:%%fixnum-field 0))
;      ((hw:32< offset1 offset2)
;       (hw:ldb offset1 vinc:%%fixnum-field 0))
;      (t
;       (hw:ldb offset2 vinc:%%fixnum-field 0)))))

;;;; Given a PC of a call instruction and the function
;;;; it appears in, return two values:
;;;;   1.  The function being called
;;;;   2.  The number of arguments it is being called with

(defun get-called-function-and-nargs (call-offset callee)
;  (declare (values function nargs))
  (let* ((refs (%compiled-function-refs callee))
         (len  (array:length refs)))
    (do ((i 0 (+ i 3)))
        ((>= i len)
         (li:tail-error "Couldn't find ref"))
      (when (= (array:svref refs i)
               call-offset)
        (return (values
                  (array:svref refs (+ i 1))
                  (array:svref refs (+ i 2))))))))

;(defun get-called-function-and-nargs (call-offset callee trap-pc)
;  (declare (values function nargs))
;  ;test to make sure callee + offset represents a legit call instruction.  Otherwise, it could
;  ; be some other random instruction which has managed to cause a trap.
;  (let* ((addr (pc->addr trap-pc))
;        (instr-high
;          (progn
;            (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
;            (hw:read-md))))
;    (let ((op-code (hw:ldb instr-high hw:%%i-op-code-high 0))
;         (ch-op (hw:ldb instr-high hw::%%i-chop-high 0)))
;      (cond ((or (not (= op-code hw:$$i-op-code-move))
;                (not (or (= ch-op hw:$$i-chop-call)
;                         (= ch-op hw:$$i-chop-open-call)
;                         (= ch-op hw:$$i-chop-topen)
;                         (= ch-op hw:$$i-chop-topen-call))))
;            (li:tail-error "Unexpected trap!" callee call-offset trap-pc)))))
;  (let* ((refs (%compiled-function-refs callee))
;        (len  (array:length refs)))
;    (do ((i 0 (+ i 3)))
;       ((>= i len)
;        (li:tail-error "Couldn't find ref" callee call-offset trap-pc))        ;corrupted ref datastructure?
;      (when (= (array:svref refs i)
;              call-offset)
;       (return (values
;                 (array:svref refs (+ i 1))
;                 (array:svref refs (+ i 2))))))))

(defun li:find-function (fcn-specifier)
  (cond
    ((symbolp fcn-specifier)
     (when (symbol:fboundp fcn-specifier)
       (let ((f (symbol:symbol-function fcn-specifier)))
         (if (%compiled-function-p f)
             f
           (li:find-function f)))))
    ((%compiled-function-p fcn-specifier)
     fcn-specifier)
    ((li:consp fcn-specifier)
     (cond
         ((eq (cons:car fcn-specifier) :INTERNAL)
     ;; (:INTERNAL FOO BAR) will be found in the refs of FOO
     ;; This is a little wierd and not true for closures
     (let ((internal-to (li:find-function (cons:cadr fcn-specifier))))
       (when (%compiled-function-p internal-to)
         (let* ((refs (%compiled-function-refs internal-to))
                (len  (array:length refs)))
           (do ((i 0 (+ i 3)))
               ((>= i len))
             (let ((ref (array:svref refs i)))
               (when (and (%compiled-function-p ref)
                          (let ((name (%compiled-function-name ref)))
                            (and (li:consp name)
                                 (eq (cons:car name) :INTERNAL)
                                 (eq (cons:caddr name) (cons:caddr fcn-specifier)))))
                 (return ref))))))))
         ((eq (cons:car fcn-specifier) 'LI:MACRO)
          (li:error "Something is trying to call the macro ~s" (cons:cdr fcn-specifier)))
         (t
          (li:error "Unknown function spec: ~s" fcn-specifier))))
     (t fcn-specifier)))



;;;; Flonums:

(defun mini-fasl-read-short-float ()
  (let ((bits (hw:unboxed-constant 0)))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  0.) bits))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  8.) bits))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 16.) bits))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 2. 24.) bits))
    (cons:make-pointer $$dtp-short-float bits)))

(defun mini-fasl-read-single-float ()
  (let ((bits (hw:unboxed-constant 0)))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  0.) bits))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  8.) bits))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 16.) bits))
    (setq bits (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 24.) bits))
    (array:make-single-float bits)))

(defun mini-fasl-read-double-float ()
  (let ((bits1 (hw:unboxed-constant 0))
        (bits2 (hw:unboxed-constant 0)))
    (setq bits1 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  0.) bits1))
    (setq bits1 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  8.) bits1))
    (setq bits1 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 16.) bits1))
    (setq bits1 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 24.) bits1))
    (setq bits2 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  0.) bits2))
    (setq bits2 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8.  8.) bits2))
    (setq bits2 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 16.) bits2))
    (setq bits2 (hw:dpb-unboxed (mini-fasl-read-byte) (byte 8. 24.) bits2))
    (array:make-double-float bits2 bits1)))


(defun mini-fasl-do-defconstant ()
  (let ((symbol (mini-fasl-read-object))
        (value (mini-fasl-read-object))
        (documentation (mini-fasl-read-object)))
    (unless (symbolp symbol)
      (li:error "DEFCONSTANT of other than a symbol."))
    (symbol:set symbol value)
    (evaluate-or-postpone
      (list-of-four 'LI::DEFCONSTANT symbol value documentation))))

(defun mini-fasl-do-defparameter ()
  (let ((symbol (mini-fasl-read-object))
        (value (mini-fasl-read-object))
        (documentation (mini-fasl-read-object)))
      (unless (symbolp symbol)
        (li:error "DEFPARAMETER of other than a symbol."))
      ;; defparameter always sets the value
      (symbol:set symbol value)
      (evaluate-or-postpone
        (list-of-four 'LI::DEFPARAMETER symbol value documentation))))


(defun mini-fasl-read-someones-value (someone)
  (let ((opcode (mini-fasl-read-byte)))
    (cond ((= opcode $$fasl-op-unbound)
           (cons:make-pointer $$dtp-unbound someone))
          (t (mini-fasl-read-object-1 opcode)))))


(defun mini-fasl-do-defvar ()
  (let ((symbol (mini-fasl-read-object)))
    ;; if the symbol is bound this shouldn't eval (if it's fasl-op-eval)
    (let ((opcode        (%kbug-stream-peek-byte kbug-k-input-fasl-stream
                                                 (kbug-stream-out-pointer kbug-k-input-fasl-stream)))
          (value         (mini-fasl-read-someones-value symbol))
          (documentation (mini-fasl-read-object)))
      (unless (symbolp symbol)
        (li:error "DEFVAR of other than a symbol."))
      (unless (symbol:boundp symbol)
        (symbol:set symbol value))
      (evaluate-or-postpone
        (if (= opcode $$fasl-op-unbound)
            (list-of-two  'LI::DEFVAR symbol)
            (list-of-four 'LI::DEFVAR symbol value documentation))))))



(defun mini-fasl-fake-eval ()
  (evaluate-or-postpone (mini-fasl-read-object)))


;;; This symbol will not actually be in the cold load.
;;; mini-fasl-fake-eval cannot be called until after
;;; downloading cold info.
(eval-when  (compile) (defvar *warm-eval-list*))



;; Let's keept track of what's NOT getting evaled ... 27sept88 pfc

(eval-when (compile) (defvar *censored-forms*))

;;; $$$ Added. <17-Nov-88 wkf>
(defvar li::*evaluator-available?* nil "T when eval works on the K.")

(defun evaluate-or-postpone (form)
  "This function is called by the fasloader whenever it needs to
evaluate a form.  If the evaluator is loaded and available, then
FORM is evaluated.  Otherwise, FORM is pushed onto K2::*WARM-EVAL-LIST*
and is not evaluated until (LI::HOT-BOOT) is run."
  (cond
    ((and (symbol::boundp 'li::*evaluator-available?*)  ; $$$ defvar in cold file ineffective <18-Nov-88 smh>
          li::*evaluator-available?*
          (not (when (will-cause-evaluator-barfage? form)
                 (setq *censored-forms*
                       (cons::cons form
                                   (if (symbol::boundp '*censored-forms*)
                                       *censored-forms*
                                     ())))
                 t)
                 ))
     (li::eval-special-ok form))
    (t
     (setq *warm-eval-list*
           (cons::cons form
                       (if (symbol::boundp '*warm-eval-list*)
                           *warm-eval-list*
                           ()))))))

(defun list-of-two (foo bar)
  "Does the same as (LIST FOO BAR)."
  (cons:cons foo
             (cons:cons bar nil)))

(defun list-of-four (foo bar baz boz)
  "Does the same as (LIST FOO BAR BAZ BOZ).  We need this function
here because LIST isn't defined until the warm load."
  (cons:cons foo
             (cons:cons bar
                        (cons:cons baz
                                   (cons:cons boz nil)))))


;; NC: forms still do not exist
;; EXPORT does exist and is needed by CROSS-SUPPORT
;; DEFMACRO doesn't exist yet
;; MOST-NEGATIVE-FIXNUM is being defined anyway

;; wait on CONTAINS-BIG-NUMBER? for now also
;; one thing at a time

;; || 27sept88 pfc

(defun will-cause-evaluator-barfage? (form)
  "Returns true if (EVAL FORM) will lose."
  (or (bogus-compiler-form? form)
;      (export-form? form)
      (defmacro-form? form)
      (contains-big-number? form)
      ))

(defun bogus-compiler-form? (form)
  (and (li:consp form)
       (symbolp (li:car form))
       (li:string= (li:package-name (li:symbol-package (li:car form)))
                "NC")))

(defun export-form? (form)
  (and (li:consp form)
       (li:eq (li:car form) 'LI::EXPORT)))

(defun defmacro-form? (form)
  (and (li:consp form)
       (li:eq (li:car form) 'LI::DEFMACRO)))

(defun contains-big-number? (form)
  (and (li:consp form)
       (li:eq (li:car form) 'LI::DEFCONSTANT)
       (li:eq (li:cadr form) 'LI::MOST-NEGATIVE-FIXNUM)))

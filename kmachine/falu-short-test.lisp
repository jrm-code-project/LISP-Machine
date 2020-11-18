;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:ZL -*-

(defun trap-on ()
  (let ((old-trap-state (hw:trap-off)))
    (hw:write-memory-control
      (hw:dpb-unboxed hw:$$trap-enable k-hw:%%memory-control-master-trap-enable
                      (hw:read-memory-control)))
    ;; Let mmfio clear out.
    (hw:nop)
    (hw:nop)
    (hw:nop)
    old-trap-state))

(defmacro define-toplevel-form-handler (name pattern &body body)
  (let ((sym (concatenate-symbol 'toplevel/ name))
        (form (gensym 'form)))
    `(progn
       (defun ,sym (,form)
         (destructure ((,pattern (cdr ,form)))
                      ,@body))
       (setf (table-entry *top-level-form-handler-table* ',name)
             #',sym))))

(define-toplevel-form-handler PRIMS:DEFMACRO macrobody
  (dump-to-kenv-file `(PRIMS:DEFMACRO . ,macrobody))
  (let ((macrofun (si::expand-defmacro macrobody nil))) ;** env
    (fasdump:fasd-defmacro (nca `(named-lambda . ,(cdr macrofun)) *env*)
                           *kbin-output-stream*)
    (fbind (car macrobody) (cons 'MACRO macrofun) *env*)))

(prims:defmacro modify-control-registers (thunk)
  `(LET ((OLD-TRAP-STATE (HW:TRAP-OFF))
         (THUNK          ,thunk))
     (LET ((MEMORY-CONTROL    (HW:READ-MEMORY-CONTROL))
           (PROCESSOR-CONTROL (HW:READ-PROCESSOR-CONTROL)))
       (FUNCALL THUNK MEMORY-CONTROL PROCESSOR-CONTROL
                #'(LAMBDA (NEW-MEMORY-CONTROL NEW-PROCESSOR-CONTROL)
                    (HW:WRITE-PROCESSOR-CONTROL NEW-PROCESSOR-CONTROL)
                    (HW:WRITE-MEMORY-CONTROL
                      (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                              NEW-MEMORY-CONTROL))
                    (HW:NOP)
                    (HW:NOP))))))

(prims:defmacro define-trap-modifier (name register byte)
  `(DEFUN ,name (NEW-VALUE)
     (MODIFY-CONTROL-REGISTERS
       #'(LAMBDA (MEMORY-CONTROL PROCESSOR-CONTROL WRITER)
           (LET (,(if (eq register :memory-control)
                      `(NEW-MEMORY-CONTROL (HW:DPB NEW-VALUE ,byte MEMORY-CONTROL))
                      `(NEW-PROCESSOR-CONTROL (HW:DPB NEW-VALUE ,byte PROCESSOR-CONTROL))))
             (DEDUCE-TRAP-MASK ,@(if (eq register :memory-control)
                                       '(NEW-MEMORY-CONTROL PROCESSOR-CONTROL)
                                       '(MEMORY-CONTROL NEW-PROCESSOR-CONTROL)))
             (FUNCALL WRITER ,@(if (eq register :Memory-control)
                                 ` (NEW-MEMORY-CONTROL PROCESSOR-CONTROL)
                                 ` (MEMORY-CONTROL NEW-PROCESSOR-CONTROL)))
             (HW:LDB ,(if (eq register :memory-control)
                          'MEMORY-CONTROL
                          'PROCESSOR-CONTROL) ,byte 0))))))

(define-trap-modifier modify-single-step-trap :memory-control k-hw:%%memory-control-single-step-enable)
(define-trap-modifier modify-asynchronous-traps :memory-control k-hw:%%memory-control-asynchronous-trap-enable)
(define-trap-modifier modify-single-step-trap :memory-control hw:%%memory-control-single-step-enable)
(define-trap-modifier modify-asynchronous-traps :memory-control hw:%%memory-control-asynchronous-trap-enable)

(defun dt-and-ovf-trap-handler-1 ()
  (let* ((save-sstep   (modify-single-step-trap   hw:$$trap-disable))
         (save-async   (modify-asynchronous-traps hw:$$trap-disable))
         (save-pc      gr::*save-trap-pc*)
         (save-pc+     gr::*save-trap-pc+*)
         (save-oreg    gr::*save-oreg*)
         (save-jcond   gr::*save-jcond*)
         (save-status  gr::*save-status*)
         (save-left    gr::*save-left*)
         (save-right   gr::*save-right*)
         (save-q       (hw:read-q-register))
         (memstat      (hw:read-memory-status))
         (vma          (hw:read-vma))
         (md (progn
               (when (= hw:$$wmd-valid (hw:ldb memstat k-hw:%%memory-status-md-written-lately 0))
                 (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
                 (hw:memory-wait)
                 (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed trap:*magic-garbage-location*)
                 (hw:read-md))
               (trap-on)
               (hw:read-md))))
      (progn
        (modify-asynchronous-traps save-async)
        (multiple-value-bind (answer result-status)
            (dt-and-ovf-trap-handler-2 save-pc save-left save-right save-status)
          (hw:read-md)
          (hw:nop)
          (hw:trap-off)
          (modify-single-step-trap save-sstep)
          (setq gr:*kbug-trap* nil)
          (setq gr::*save-right* answer)
          (setq gr::*save-status*  result-status)))
    (setq gr::*save-trap-pc*      save-pc)
    (setq gr::*save-trap-pc+*     save-pc+)
    (setq gr::*save-oreg*    save-oreg)
    (setq gr::*save-jcond*   save-jcond)
    (hw:load-q-register save-q)
    (hw:write-md-unboxed md)
    (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
    (vmem:write-md-generic md
                           (hw:ldb-not memstat k-hw:%%memory-status-md-not-boxed-bit 0))
    (vmem:write-vma-generic vma
                            (hw:ldb-not memstat k-hw:%%memory-status-vma-not-boxed-bit 0))))

(defun dt-and-ovf-trap-handler-2 (trap-pc left right status)
  (let* ((result nil)
         (rstat  nil)
         (pc-loc (pc->unboxed-locative trap-pc))
         (instl (progn (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed pc-loc)
                       (hw:read-md)))
         (insth (progn (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:32-1+ pc-loc))
                       (hw:read-md))))
    (dispatch k-hw:%%i-dtp-check-high insth  ;; dispatch on datatype check code
       ((vinc:$$dtc-both-fixnum vinc:$$dtc-both-fixnum-with-overflow)
        (when (or (zerop (k2:boxed-bit left)) (zerop (k2:boxed-bit right)))
          (li:error "Fixnum datatype trap on unboxed data!" left right))
        (let ((op-info (decode-alu-op insth instl)))
          (multiple-value-setq (result rstat)
            (dispatch (byte 2. 5.) op-info
                      (binary-op
                        (handle-binary-op insth instl op-info left right status))
                      (t (li:error "Unhandled unary/binary"))))))
       (t
         (li:error "Illegal/unhandled datatype check code specified!")))
  (values result (hw:dpb (hw:ldb rstat (byte 4. 16.) 0) (byte 4. 16.) status))))

(defun overflow-trap-handler ()
  (hw:jump 'datatype-trap-handler))

(defun datatype-trap-handler ()
  (HW:CH-OPEN-CALL GR::*TRAP-TEMP1*)
     (HW:CH-TOPEN-CALL)
     (HW:OPEN-FRAME)
     (SETF (HW:O0)  GR::*RETURN-16*)
     (SETF (HW:O1)  GR::*RETURN-17*)
     (SETF (HW:O2)  GR::*RETURN-18*)
     (SETF (HW:O3)  GR::*RETURN-19*)
     (SETF (HW:O4)  GR::*RETURN-20*)
     (SETF (HW:O5)  GR::*RETURN-21*)
     (SETF (HW:O6)  GR::*RETURN-22*)
     (SETF (HW:O7)  GR::*RETURN-23*)
     (SETF (HW:O8)  GR::*RETURN-24*)
     (SETF (HW:O9)  GR::*RETURN-25*)
     (SETF (HW:O10) GR::*RETURN-26*)
     (SETF (HW:O11) GR::*RETURN-27*)
     (SETF (HW:O12) GR::*RETURN-28*)
     (SETF (HW:O13) GR::*RETURN-29*)
     (SETF (HW:O14) (HW:LDB (HW:READ-PROCESSOR-STATUS) K-HW:%%PROCESSOR-STATUS-RETURN-CODE 0.))
     (SETF (HW:O15) GR::*NUMBER-OF-RETURN-VALUES*)

     (HW:OPEN-FRAME)
     (SETF (HW:O0)  GR::*RETURN-0*)
     (SETF (HW:O1)  GR::*RETURN-1*)
     (SETF (HW:O2)  GR::*RETURN-2*)
     (SETF (HW:O3)  GR::*RETURN-3*)
     (SETF (HW:O4)  GR::*RETURN-4*)
     (SETF (HW:O5)  GR::*RETURN-5*)
     (SETF (HW:O6)  GR::*RETURN-6*)
     (SETF (HW:O7)  GR::*RETURN-7*)
     (SETF (HW:O8)  GR::*RETURN-8*)
     (SETF (HW:O9)  GR::*RETURN-9*)
     (SETF (HW:O10) GR::*RETURN-10*)
     (SETF (HW:O11) GR::*RETURN-11*)
     (SETF (HW:O12) GR::*RETURN-12*)
     (SETF (HW:O13) GR::*RETURN-13*)
     (SETF (HW:O14) GR::*RETURN-14*)
     (SETF (HW:O15) GR::*RETURN-15*)

     (dt-and-ovf-trap-handler-1)

     (SETQ GR::*RETURN-0*  (HW:O0))
     (SETQ GR::*RETURN-1*  (HW:O1))
     (SETQ GR::*RETURN-2*  (HW:O2))
     (SETQ GR::*RETURN-3*  (HW:O3))
     (SETQ GR::*RETURN-4*  (HW:O4))
     (SETQ GR::*RETURN-5*  (HW:O5))
     (SETQ GR::*RETURN-6*  (HW:O6))
     (SETQ GR::*RETURN-7*  (HW:O7))
     (SETQ GR::*RETURN-8*  (HW:O8))
     (SETQ GR::*RETURN-9*  (HW:O9))
     (SETQ GR::*RETURN-10* (HW:O10))
     (SETQ GR::*RETURN-11* (HW:O11))
     (SETQ GR::*RETURN-12* (HW:O12))
     (SETQ GR::*RETURN-13* (HW:O13))
     (SETQ GR::*RETURN-14* (HW:O14))
     (SETQ GR::*RETURN-15* (HW:O15))

     (setq gr::*trap-temp1* (hw:call 'flush-open-frame 0))

     (SETQ GR::*RETURN-16*  (HW:O0))
     (SETQ GR::*RETURN-17*  (HW:O1))
     (SETQ GR::*RETURN-18*  (HW:O2))
     (SETQ GR::*RETURN-19*  (HW:O3))
     (SETQ GR::*RETURN-20*  (HW:O4))
     (SETQ GR::*RETURN-21*  (HW:O5))
     (SETQ GR::*RETURN-22*  (HW:O6))
     (SETQ GR::*RETURN-23*  (HW:O7))
     (SETQ GR::*RETURN-24*  (HW:O8))
     (SETQ GR::*RETURN-25*  (HW:O9))
     (SETQ GR::*RETURN-26* (HW:O10))
     (SETQ GR::*RETURN-27* (HW:O11))
     (SETQ GR::*RETURN-28* (HW:O12))
     (SETQ GR::*RETURN-29* (HW:O13))
     (SETQ GR::*TRAP-TEMP1* (HW:O14))
     (SETQ GR::*NUMBER-OF-RETURN-VALUES* (HW:O15))

     (setq gr::*trap-temp2* (hw:call 'flush-open-frame 0))

     (if (zerop gr::*trap-temp1*)
         (hw:ch-return-one-value)
         (hw:ch-return-multiple-values))

     (hw:nop)

  (if gr:*kbug-trap*
      (hw:jump 'non-modifying-exit)
    (hw:jump 'modifying-exit)))


(defafun falu-test ()
k-test-first-trap-vector-loc  ; #o000  "First location reserved for trap vectors."
  ;; trap
loc-0
  (alu setl gr::*save-oreg* r0 r0 bw-32 boxed-left)
  (alu setl gr::*save-left* r0 r0 bw-32 boxed-left)
  (alu setr gr::*save-right* r0 r0 bw-32 boxed-right)
  (alu pass-status gr::*save-status* r0 r0 bw-32 unboxed)
  (alu-field extract-bit-right gr::*save-jcond* r0 processor-status (byte 1. (+ 32. 17.)) unboxed)
  (alu-field field-and gr::*save-trap* gr::*trap-mask* trap-register (byte 31. 0.) unboxed)
  (alu prioritize-r gr::*trap-temp1* r0 gr::*save-trap* bw-32 unboxed)
  (alu-field set-bit-right gr::*trap-temp1* r0 gr::*trap-temp1* (byte 1. 5.) unboxed)
  (alu merge-r gr::*save-trap-pc*  gr::*trap-dtp-code-5* trap-pc bw-24 boxed)
  (alu merge-r gr::*save-trap-pc+* gr::*trap-dtp-code-5* trap-pc+  bw-24 boxed next-pc-dispatch)
  (nop)
  (nop)
  ;; non-modifying-exit
loc-12
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right*  bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right*  bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)
  (nop)
  (nop)
  (nop)
  ;; modifying-exit
loc-20
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right* bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right* bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)
  (nop)
  (nop)
  (nop)
  ;; diagnostic-trap-exit
loc-28
  (alu setl nop gr::*save-trap-pc*  gr::*save-right* bw-32)
  (alu setl nop gr::*save-trap-pc+* gr::*save-right* bw-32)
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch)
  (nop)
  ;; trap-vector-table
loc-32
trap-vector-reset                                       ;Bit 31 - addr 32 - Highest priority
  (unconditional-branch reset-trap-handler ())
trap-vector-trace                                       ;Bit 30 - addr 33
  (unconditional-branch trace-trap-handler ())
trap-vector-icache-parity                               ;Bit 29 - addr 34
  (unconditional-branch icache-parity-trap-handler ())
trap-vector-icache-nubus-err                            ;Bit 28 - addr 35
  (unconditional-branch icache-nubus-error-trap-handler ())
trap-vector-icache-nubus-timeout                        ;Bit 27 - addr 36
  (unconditional-branch icache-nubus-timeout-trap-handler ())
trap-vector-icache-page-fault                           ;Bit 26 - addr 37
  (unconditional-branch icache-map-fault-trap-handler ())
trap-vector-proc-mread-parity                           ;Bit 25 - addr 38
  (unconditional-branch memory-read-parity-trap-handler ())
trap-vector-proc-mread-nubus-err                        ;Bit 24 - addr 39
  (unconditional-branch memory-read-nubus-error-trap-handler ())
trap-vector-proc-mread-nubus-timeout                    ;Bit 23- addr 40
  (unconditional-branch memory-read-nubus-timeout-trap-handler ())
trap-vector-proc-mread-page-fault                       ;Bit 22 - addr 41
  (unconditional-branch memory-read-page-fault-trap-handler ())
trap-vector-proc-mread-transporter                      ;Bit 21 - addr 42
  (unconditional-branch memory-read-transporter-trap-handler ())
trap-vector-proc-mwrite-nubus-err                       ;Bit 20 - addr 43
  (unconditional-branch memory-write-nubus-error-trap-handler ())
trap-vector-proc-mwrite-nubus-timeout                   ;Bit 19-  addr 44
  (unconditional-branch memory-write-nubus-timeout-trap-handler ())
trap-vector-proc-mwrite-page-fault                      ;Bit 18 - addr 45
  (unconditional-branch memory-write-page-fault-trap-handler ())
trap-vector-proc-mwrite-gc                              ;Bit 17 - addr 46
  (unconditional-branch memory-write-gc-trap-handler ())
trap-vector-floating-point                              ;Bit 16 - addr 47
  (unconditional-branch floating-point-trap-handler ())
trap-vector-heap-empty                                  ;Bit 15 - addr 48
  (unconditional-branch heap-empty-trap-handler ())
trap-vector-instruction-bit                             ;Bit 14 - addr 49
  (unconditional-branch instruction-trap-handler ())
trap-vector-datatype                                    ;Bit 13 - addr 50
  (unconditional-branch datatype-trap-handler ())
trap-vector-overflow                                    ;Bit 12 - addr 51
  (jump overflow-trap-handler ())
trap-vector-spare11                                     ;Bit 11 - addr 52
  (unconditional-branch spare11-trap-handler ())
trap-vector-interrupt7                                  ;Bit 10 - addr 53
  (unconditional-branch debugger-trap-handler ())
trap-vector-interrupt6                                  ;Bit 09 - addr 54
  (unconditional-branch interrupt6-trap-handler ())
trap-vector-interrupt5                                  ;Bit 08 - addr 55
  (unconditional-branch interrupt5-trap-handler ())
trap-vector-interrupt4                                  ;Bit 07 - addr 56
  (unconditional-branch iop-trap-handler ())
trap-vector-interrupt3                                  ;Bit 06 - addr 57
  (unconditional-branch interrupt3-trap-handler ())
trap-vector-interrupt2                                  ;Bit 05 - addr 58
  (unconditional-branch interrupt2-trap-handler ())
trap-vector-interrupt1                                  ;Bit 04 - addr 59
  (unconditional-branch interrupt1-trap-handler ())
trap-vector-interrupt0                                  ;Bit 03 - addr 60
  (unconditional-branch interrupt0-trap-handler ())
trap-vector-timer-1024                                  ;Bit 02 - addr 61
  (unconditional-branch timer-1024-trap-handler ())
trap-vector-timer-16384                                 ;Bit 01 - addr 62
  (unconditional-branch timer-16384-trap-handler ())
trap-vector-spurious                                    ;Bit 00 - addr 63
k-test-last-trap-vector-loc ; #o077  "Last location reserved for trap vectors."
  (unconditional-branch spurious-trap-handler ())


;; locations reserved for use by the test code

k-test-entry-loc            ; #o100  "This is normally a jump to the real entry."
  (unconditional-branch k-test-code-loc ())
k-test-exit-loc             ; #o101  "Normally a halt instruction."
  (unconditional-branch k-test-exit-loc ())
k-test-bits-arg-loc         ; #o102  "(low word) 32 bits of switches passed to tests."
k-test-word-arg-loc         ; #o102  "(high word)One word argument to tests."
  (nop)
k-test-internal-result-loc  ; #o103  "(low word)"
k-test-external-result-loc  ; #o103  "(high word"
  (nop)
k-test-expected-value-loc   ; #o104  "(low word)"
k-test-incorrect-value-loc  ; #o104  "(high word"
  (nop)
k-test-address-loc          ; #o105  "(low word)"
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)


k-test-trap-code-loc        ; #o120  "Traps vector to this location."
 ;; these are the labels that traps vector to
 ;; I've kept them around in case we want to handle certain traps specially.
 ;; For now they all refer to the same location which simply loops on itself.
 reset-trap-handler
 trace-trap-handler
 icache-parity-trap-handler
 icache-nubus-error-trap-handler
 icache-nubus-timeout-trap-handler
 icache-map-fault-trap-handler
 memory-read-parity-trap-handler
 memory-read-nubus-error-trap-handler
 memory-read-nubus-timeout-trap-handler
 memory-read-page-fault-trap-handler
 memory-read-transporter-trap-handler
 memory-write-nubus-error-trap-handler
 memory-write-nubus-timeout-trap-handler
 memory-write-page-fault-trap-handler
 memory-write-gc-trap-handler
 floating-point-trap-handler
 heap-empty-trap-handler
 instruction-trap-handler
 datatype-trap-handler
 overflow-trap-handler
 spare11-trap-handler
 debugger-trap-handler
 interrupt6-trap-handler
 interrupt5-trap-handler
 iop-trap-handler
 interrupt3-trap-handler
 interrupt2-trap-handler
 interrupt1-trap-handler
 interrupt0-trap-handler
 timer-1024-trap-handler
 timer-16384-trap-handler
 spurious-trap-handler

  (unconditional-branch k-test-trap-code-loc ())
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)

k-test-code-loc               ;#o140  "Test code lives here."
setup
  (movei processor-control 7)
  (movei bus-control #X79)
  (movei open-active-return #xffffff)
  (movei vma 0)
  (nop)
  (movei memory-map #x0f)
  (movei memory-map #x0f)
  (nop)
first-test
  (movei a0 32.)
 loop
  (alu load-status-r nop a0 a0 bw-8)
  (alu-field set-bit-left a1 gr:*all-zero* ignore 0 pw-ri)
  (move md a1)
  (move vma-start-write gr:*zero* unboxed-md)
  (memory-wait)
  (move vma-start-read gr:*zero* unboxed-md)
  (memory-wait)
  (move a2 md)
  (alu xor nop a1 a2 bw-32 unboxed)
  (test br-not-equal)
  (branch fail ())
  (alu l-r a0 a0 gr:*one* bw-8)
  (test br-not-zero)
  (branch loop ())
 pass
  ;; set up success completion code and jump to exit location
  (movei md k-test-passed)
  (movei vma-start-write k-test-external-result-loc unboxed-vma)
  (unconditional-branch k-test-exit-loc ())
 fail
  ;; set up
  ;;        failure completion code
  ;;        address being tested
  ;;        expected value
  ;;        incorrect value
  ;; and jump to exit location
  (movei md k-test-failed)
  (movei vma-start-write k-test-external-result-loc unboxed-vma)
  (move md gr:*zero*)
  (movei vma-start-write k-test-address-loc unboxed-vma)
  (move md a1)
  (movei vma-start-write k-test-expected-value-loc unboxed-vma)
  (move md a2)
  (movei vma-start-write k-test-incorrect-value-loc unboxed-vma)
  (unconditional-branch k-test-exit-loc ())

  )

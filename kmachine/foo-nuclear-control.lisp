;;; -*- Mode:LISP; Package:VINCULUM; Base:10; Readtable:CL -*-

(export '(
          define-control-register-modifier
          1024-interrupt-off
          16384-interrput-off
          datatype-traps-off
          dram-parity-traps-off
          floating-point-trap-off
          flush-icache
          heap-underflow-trap-off
          icache-traps-off
          overflow-traps-off
          synchronous-traps-off))

(defmacro modify-memory-control (thunk)
  `(LET ((OLD-TRAP-STATE (HW:TRAP-OFF))
         (THUNK          ,thunk))
     (LET ((MEMORY-CONTROL (HW:READ-MEMORY-CONTROL)))
       (HW:WRITE-MEMORY-CONTROL
         (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                 (FUNCALL THUNK MEMORY-CONTROL)))
       (HW:NOP)
       (HW:NOP)
       MEMORY-CONTROL)))

(defmacro modify-processor-control (thunk)
  `(LET ((THUNK ,thunk))
     (TRAP::WITHOUT-TRAPS
       #'(LAMBDA ()
           (LET ((PROCESSOR-CONTROL (HW:READ-PROCESSOR-CONTROL)))
             (HW:WRITE-PROCESSOR-CONTROL
               (FUNCALL THUNK PROCESSOR-CONTROL))
             (HW:NOP)
             (HW:NOP)
             PROCESSOR-CONTROL)))))

(defmacro define-control-register-modifier (name modifier bit-field)
  `(DEFUN ,name (NEW-VALUE)
     (HW:LDB (,modifier
              #'(LAMBDA (REGISTER)
                  (HW:DPB NEW-VALUE ,bit-field REGISTER)))
             ,bit-field 0.)))

(defun flush-icache ()
  (modify-processor-control
    #'(lambda (procctl)
        (hw:write-processor-control
          (hw:dpb hw:$$icache-disable-all-sets hw:%%processor-control-icache-enables procctl))
        (hw:nop)
        procctl)))

(defmacro define-trap-modifier (disable-name restore-name register byte)
  `(PROGN
     (EXPORT '(,disable-name ,restore-name))
     (DEFUN ,disable-name ()
       (LET ((OLD-TRAP-STATE    (HW:TRAP-OFF))
             (MEMORY-CONTROL    (HW:READ-MEMORY-CONTROL))
             (PROCESSOR-CONTROL (HW:READ-PROCESSOR-CONTROL)))
         ,(if (eq register :memory-control)
              `(LET ((NEW-MEMORY-CONTROL (HW:DPB HW:$$TRAP-DISABLE ,byte MEMORY-CONTROL)))
                 (DEDUCE-TRAP-MASK NEW-MEMORY-CONTROL PROCESSOR-CONTROL)
              ;; (HW:WRITE-PROCESSOR-CONTROL PROCESSOR-CONTROL) ;nop?? smh/wkf
                 (HW:WRITE-MEMORY-CONTROL
                   (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                           NEW-MEMORY-CONTROL))
                 (HW:NOP) ;;@@@ Do we need this?
                 (HW:NOP) ;;@@@ Do we need this?
                 (HW:LDB MEMORY-CONTROL ,byte 0))
            `(LET ((NEW-PROCESSOR-CONTROL (HW:DPB HW:$$TRAP-DISABLE ,byte PROCESSOR-CONTROL)))
               (DEDUCE-TRAP-MASK MEMORY-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-PROCESSOR-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-MEMORY-CONTROL
                 (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                         MEMORY-CONTROL))
               (HW:NOP) ;;@@@ Do we need this?
               (HW:NOP) ;;@@@ Do we need this?
               (HW:LDB PROCESSOR-CONTROL ,byte 0)))))
     (DEFUN ,restore-name (NEW-VALUE)
       (LET ((OLD-TRAP-STATE    (HW:TRAP-OFF))
             (MEMORY-CONTROL    (HW:READ-MEMORY-CONTROL))
             (PROCESSOR-CONTROL (HW:READ-PROCESSOR-CONTROL)))
         ,(if (eq register :memory-control)
              `(LET ((NEW-MEMORY-CONTROL (HW:DPB NEW-VALUE ,byte MEMORY-CONTROL)))
                 (DEDUCE-TRAP-MASK NEW-MEMORY-CONTROL PROCESSOR-CONTROL)
              ;; (HW:WRITE-PROCESSOR-CONTROL PROCESSOR-CONTROL) ;nop?? smh/wkf
                 (HW:WRITE-MEMORY-CONTROL
                   (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                           NEW-MEMORY-CONTROL))
                 (HW:NOP)  ;;@@@ Do we need this?
                 (HW:NOP)) ;;@@@ Do we need this?
            `(LET ((NEW-PROCESSOR-CONTROL (HW:DPB NEW-VALUE ,byte PROCESSOR-CONTROL)))
               (DEDUCE-TRAP-MASK MEMORY-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-PROCESSOR-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-MEMORY-CONTROL
                 (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                         MEMORY-CONTROL))
               (HW:NOP)       ;;@@@ Do we need this?
               (HW:NOP))))))) ;;@@@ Do we need this?

(defun deduce-trap-mask (memory-control processor-control)
  (labels ((deduce-traps ()
             (hw:32logior
               (deduce-synchronous)
               (hw:32logior
                 (deduce-asynchronous)
                 (hw:32logior
                   (deduce-random)
                   hw:*initial-trap-mask*))))

           (deduce-synchronous ()
             (if (= hw:$$trap-enable (hw:ldb memory-control hw:%%memory-control-synchronous-trap-enable 0))
                 (hw:32logior hw:*unmaskable-synchronous-trap-mask*
                              (deduce-subsynchronous))
                 (hw:unboxed-constant 0)))

           (deduce-subsynchronous ()
             (hw:32logior
               (deduce-icache)
               (hw:32logior
                 (deduce-dram-parity)
                 (deduce-heap-underflow))))

           (deduce-icache ()
             (if (= hw:$$icache-trap-enable
                    (hw:ldb memory-control hw:%%memory-control-icache-error-enable 0.))
                 hw:*icache-trap-mask*
                 (hw:unboxed-constant 0)))

           (deduce-dram-parity ()
             (if (= hw:$$dram-parity-enable
                    (hw:ldb memory-control hw:%%memory-control-dram-parity-enable 0.))
                  hw:*dram-parity-mask*
                 (hw:unboxed-constant 0)))

           (deduce-heap-underflow ()
             (if (= hw:$$call-heap-underflow-trap-enable
                    (hw:ldb processor-control hw:%%processor-control-heap-underflow-trap-enable 0))
                 hw:*heap-underflow-mask*
                 (hw:unboxed-constant 0)))

           (deduce-asynchronous ()
             (if (= hw:$$trap-enable
                    (hw:ldb memory-control hw:%%memory-control-asynchronous-trap-enable 0))
                 (hw:32logior
                   hw:*unmaskable-asynchronous-trap-mask*
                   (deduce-subasynchronous))
                 (hw:unboxed-constant 0)))

           (deduce-subasynchronous ()
             (hw:32logior
               (deduce-1024-interrupt)
               (deduce-16384-interrupt)))

           (deduce-1024-interrupt ()
             (if (= hw:$$trap-enable
                    (hw:ldb memory-control hw:%%memory-control-1024-interrupt 0))
                 hw:*1024-interrupt-mask*
                 (hw:unboxed-constant 0)))

           (deduce-16384-interrupt ()
             (if (= hw:$$trap-enable
                    (hw:ldb memory-control hw:%%memory-control-16384-interrupt 0))
                  hw:*16384-interrupt-mask*
                 (hw:unboxed-constant 0)))

           (deduce-random ()
             (hw:32logior
               (deduce-single-step-mask)
               (hw:32logior
                 (deduce-floating-point-mask)
                 (hw:32logior
                   (deduce-datatype-mask)
                   (deduce-overflow-mask)))))

           (deduce-single-step-mask ()
             (if (= hw:$$trap-enable
                    (hw:ldb memory-control hw:%%memory-control-single-step-enable 0))
                 hw:*single-step-mask*
                 (hw:unboxed-constant 0)))

           (deduce-floating-point-mask ()
             (if (= hw:$$floating-point-trap-enable
                    (hw:ldb processor-control hw:%%processor-control-floating-point-trap-enable 0))
                 hw:*floating-point-mask*
                 (hw:unboxed-constant 0)))

           (deduce-datatype-mask ()
             (if (= hw:$$trap-enable
                    (hw:ldb memory-control hw:%%memory-control-datatype-trap-enable 0))
                 hw:*datatype-mask*
                 (hw:unboxed-constant 0)))

           (deduce-overflow-mask ()
             (if (= hw:$$trap-enable
                    (hw:ldb memory-control hw:%%memory-control-overflow-trap-enable 0))
                 hw:*overflow-mask*
                 (hw:unboxed-constant 0))))
    (setq gr::*trap-mask* (deduce-traps))))


(define-trap-modifier disable-icache-traps       restore-icache-traps       :memory-control
                      hw:%%memory-control-icache-error-enable)
(define-trap-modifier disable-1024-interrupt     restore-1024-interrupt     :memory-control
                      hw:%%memory-control-1024-interrupt)
(define-trap-modifier disable-16384-interrupt    restore-16384-interrupt    :memory-control
                      hw:%%memory-control-16384-interrupt)
(define-trap-modifier disable-dram-parity-traps  restore-dram-parity-traps  :memory-control
                      hw:%%memory-control-dram-parity-enable)
(define-trap-modifier disable-synchronous-traps  restore-synchronous-traps  :memory-control
                      hw:%%memory-control-synchronous-trap-enable)
(define-trap-modifier disable-datatype-traps     restore-datatype-traps     :memory-control
                      hw:%%memory-control-datatype-trap-enable)
(define-trap-modifier disable-overflow-traps     restore-overflow-traps     :memory-control
                      hw:%%memory-control-overflow-trap-enable)
(define-trap-modifier disable-asynchronous-traps restore-asynchronous-traps :memory-control
                      hw:%%memory-control-asynchronous-trap-enable)
(define-trap-modifier disable-single-step-trap   restore-single-step-trap   :memory-control
                      hw:%%memory-control-single-step-enable)

(define-trap-modifier disable-heap-underflow-traps restore-heap-underflow-traps :processor-control
                      hw:%%processor-control-heap-underflow-trap-enable)
(define-trap-modifier disable-floating-point-traps restore-floating-point-traps :processor-control
                      hw:%%processor-control-floating-point-trap-enable)

;;; This one is special.
;(define-trap-control modify-single-step-trap
;                    single-step-trap-off
;                    restore-single-step-trap
;                    modify-memory-control
;                    hw:%%memory-control-single-step-enable
;                    hw:$$trap-disable)

(define-control-register-modifier modify-leds modify-memory-control
  hw:%%memory-control-leds)

(define-control-register-modifier modify-lowcore-cache-enable modify-processor-control
  hw:%%processor-control-icache-z-enable)

(define-control-register-modifier modify-icache-enables modify-processor-control
  hw:%%processor-control-icache-enables)

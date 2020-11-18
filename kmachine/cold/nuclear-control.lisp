;;; -*- Mode:LISP; Package:VINCULUM; Base:10; Readtable:CL -*-

(export '(define-control-register-modifier))

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
  `(progn
     (EXPORT '(,disable-name ,restore-name))
     (DEFUN ,disable-name ()
       (LET ((OLD-TRAP-STATE    (HW:TRAP-OFF))
             (MEMORY-CONTROL    (HW:READ-MEMORY-CONTROL)))
         ,(if (eq register :memory-control)
              `(LET ((NEW-MEMORY-CONTROL (HW:DPB HW:$$TRAP-DISABLE ,byte MEMORY-CONTROL)))
                 (DISABLE-DEDUCE-TRAP-MASK ,register ,byte NEW-MEMORY-CONTROL)
                 (HW:WRITE-MEMORY-CONTROL
                   (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                           NEW-MEMORY-CONTROL))
                 (HW:NOP)
                 (HW:NOP) ;;@@@ Do we need this?
                 (HW:LDB MEMORY-CONTROL ,byte 0)) ;;Return old value.
            `(LET* ((PROCESSOR-CONTROL     (HW:READ-PROCESSOR-CONTROL))
                    (NEW-PROCESSOR-CONTROL (HW:DPB HW:$$TRAP-DISABLE ,byte PROCESSOR-CONTROL)))
               (DISABLE-DEDUCE-TRAP-MASK ,register ,byte MEMORY-CONTROL)
               (HW:WRITE-PROCESSOR-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-MEMORY-CONTROL
                 (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                         MEMORY-CONTROL))
               (HW:NOP)
               (HW:NOP)
               (HW:LDB PROCESSOR-CONTROL ,byte 0))))) ;;Return old value.
     (DEFUN ,restore-name (OLD-VALUE)
       (LET ((OLD-TRAP-STATE    (HW:TRAP-OFF))
             (MEMORY-CONTROL    (HW:READ-MEMORY-CONTROL))
             (PROCESSOR-CONTROL (HW:READ-PROCESSOR-CONTROL)))
         ,(if (eq register :memory-control)
              `(LET ((NEW-MEMORY-CONTROL (HW:DPB OLD-VALUE ,byte MEMORY-CONTROL)))
                 (UPDATE-TRAP-MASK OLD-VALUE ,register ,byte NEW-MEMORY-CONTROL PROCESSOR-CONTROL)
                 (HW:WRITE-MEMORY-CONTROL
                   (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                           NEW-MEMORY-CONTROL))
                 (HW:NOP)
                 (HW:NOP)
                 NIL)
            `(LET ((NEW-PROCESSOR-CONTROL (HW:DPB OLD-VALUE ,byte PROCESSOR-CONTROL)))
               (UPDATE-TRAP-MASK OLD-VALUE ,register ,byte MEMORY-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-PROCESSOR-CONTROL NEW-PROCESSOR-CONTROL)
               (HW:WRITE-MEMORY-CONTROL
                 (HW:DPB OLD-TRAP-STATE HW:%%MEMORY-CONTROL-MASTER-TRAP-ENABLE
                         MEMORY-CONTROL))
               (HW:NOP)
               (HW:NOP)
               NIL))))))

(defmacro update-trap-mask (new-value register byte memory-control processor-control)
  `(if (= ,new-value hw:$$trap-disable)
      (disable-deduce-trap-mask ,register ,byte ,memory-control)
    (enable-deduce-trap-mask ,register ,byte ,memory-control ,processor-control)))

(defmacro bit-on (byte memory-control)
  `(hw:32logbitp (byte-position ,byte) ,memory-control))

(defmacro synchronous-on  (memory-control)
  `(bit-on hw:%%memory-control-synchronous-trap-enable  ,memory-control))

(defmacro asynchronous-on (memory-control)
  `(bit-on hw:%%memory-control-asynchronous-trap-enable ,memory-control))

(defmacro bit-on-mask (byte memory-control mask)
  `(if (bit-on ,byte ,memory-control)
       ,mask
     gr:*all-zero*))

(defmacro synchronous-on-mask  (memory-control mask)
  `(bit-on-mask hw:%%memory-control-synchronous-trap-enable       ,memory-control    ,mask))

(defmacro asynchronous-on-mask (memory-control mask)
  `(bit-on-mask hw:%%memory-control-asynchronous-trap-enable      ,memory-control    ,mask))

(defmacro icache-on-mask (memory-control)
  `(bit-on-mask hw:%%memory-control-icache-error-enable           ,memory-control    hw:*icache-trap-mask*))

(defmacro dram-on-mask   (memory-control)
  `(bit-on-mask hw:%%memory-control-dram-parity-enable            ,memory-control    hw:*dram-parity-mask*))

(defmacro heap-on-mask   (processor-control)
  `(bit-on-mask hw:%%processor-control-heap-underflow-trap-enable ,processor-control hw:*heap-underflow-mask*))

(defmacro 1024-on-mask   (memory-control)
  `(bit-on-mask hw:%%memory-control-1024-interrupt                ,memory-control    hw:*1024-interrupt-mask*))

(defmacro 16384-on-mask  (memory-control)
  `(bit-on-mask hw:%%memory-control-16384-interrupt               ,memory-control    hw:*16384-interrupt-mask*))

(defmacro enable-deduce-trap-mask (register byte memory-control processor-control)  ;;written by --wkf
  `(setq gr::*trap-mask*
         (hw:32logior (hw:32logior gr::*trap-mask* hw:*initial-trap-mask*)  ;;@@@ Do initial-mask at compile time.
               ,(if (eq register :memory-control)
                    (case byte
                      (hw:%%memory-control-synchronous-trap-enable
                       `(hw:32logior hw:*unmaskable-synchronous-trap-mask*
                                     (hw:32logior (icache-on-mask ,memory-control)
                                                  (hw:32logior (dram-on-mask ,memory-control)
                                                               (heap-on-mask ,processor-control)))))
                      (hw:%%memory-control-asynchronous-trap-enable
                       `(hw:32logior hw:*unmaskable-asynchronous-trap-mask*
                                     (hw:32logior (1024-on-mask  ,memory-control)
                                                  (16384-on-mask ,memory-control))))
                      (hw:%%memory-control-icache-error-enable
                       `(synchronous-on-mask ,memory-control hw:*icache-trap-mask*))
                      (hw:%%memory-control-dram-parity-enable
                       `(synchronous-on-mask ,memory-control hw:*dram-parity-mask*))
                      (hw:%%memory-control-1024-interrupt
                       `(asynchronous-on-mask ,memory-control hw:*1024-interrupt-mask*))
                      (hw:%%memory-control-16384-interrupt
                       `(asynchronous-on-mask ,memory-control hw:*16384-interrupt-mask*))
                      (hw:%%memory-control-single-step-enable   'hw:*single-step-mask*)
                      (hw:%%memory-control-datatype-trap-enable 'hw:*datatype-mask*)
                      (hw:%%memory-control-overflow-trap-enable 'hw:*overflow-mask*)
                      (t (zl:error "Unhandled Memory control byte in enable-deduce-trap-mask" register byte)))
                  (case byte
                    (hw:%%processor-control-heap-underflow-trap-enable
                     `(synchronous-on-mask ,memory-control hw:*heap-underflow-mask*))
                    (hw:%%processor-control-floating-point-trap-enable 'hw:*floating-point-mask*)
                    (t (zl:error "Unhandled Processor control byte in enable-deduce-trap-mask" register byte)))))))

(defmacro 32lognot (32bits)
  `(hw:32logxor ,32bits gr:*all-ones*))

(defmacro disable-deduce-trap-mask (register byte memory-control)  ;;written by --wkf
  `(setq gr::*trap-mask*
         (hw:32logand (hw:32logior gr::*trap-mask* hw:*initial-trap-mask*)  ;;@@@ Do initial-mask at compile time.
               ,(if (eq register :memory-control)
                    (case byte
                      (hw:%%memory-control-synchronous-trap-enable
                                  '(32lognot (hw:32logior hw:*unmaskable-synchronous-trap-mask*
                                                          (hw:32logior hw:*icache-trap-mask*
                                                                       (hw:32logior hw:*dram-parity-mask*
                                                                                    hw:*heap-underflow-mask*)))))
                      (hw:%%memory-control-asynchronous-trap-enable
                                  '(32lognot (hw:32logior hw:*unmaskable-asynchronous-trap-mask*
                                                          (hw:32logior hw:*1024-interrupt-mask*
                                                                       hw:*16384-interrupt-mask*))))
                      (hw:%%memory-control-icache-error-enable
                                  `(hw:32logior (32lognot hw:*icache-trap-mask*)
                                                (synchronous-on-mask ,memory-control (dram-on-mask ,memory-control))))
                      (hw:%%memory-control-dram-parity-enable
                                  `(hw:32logior (32lognot hw:*dram-parity-mask*)
                                                (synchronous-on-mask ,memory-control (icache-on-mask ,memory-control))))
                      (hw:%%memory-control-1024-interrupt              '(32lognot hw:*1024-interrupt-mask*))
                      (hw:%%memory-control-16384-interrupt             '(32lognot hw:*16384-interrupt-mask*))
                      (hw:%%memory-control-single-step-enable          '(32lognot hw:*single-step-mask*))
                      (hw:%%memory-control-datatype-trap-enable        '(32lognot hw:*datatype-mask*))
                      (hw:%%memory-control-overflow-trap-enable        '(32lognot hw:*overflow-mask*))
                      (t (zl:error "Unhandled Memory control byte in disable-deduce-trap-mask" register byte)))
                  (case byte
                    (hw:%%processor-control-heap-underflow-trap-enable '(32lognot hw:*heap-underflow-mask*))
                    (hw:%%processor-control-floating-point-trap-enable '(32lognot hw:*floating-point-mask*))
                    (t (zl:error "Unhandled Processor control byte in disable-deduce-trap-mask" register byte)))))))

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

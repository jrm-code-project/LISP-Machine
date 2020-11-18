;;; -*- Mode:LISP; Package:VINCULUM; Base:10; Readtable:CL -*-

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
  `(DEFSUBST ,name (NEW-VALUE)
     (HW:LDB (,modifier
              #'(LAMBDA (REGISTER)
                  (HW:DPB NEW-VALUE ,bit-field REGISTER)))
             ,bit-field 0.)))

(defmacro define-trap-control (modifier disabler restorer register bit-field disable)
  `(PROGN
     (DEFINE-CONTROL-REGISTER-MODIFIER ,modifier ,register ,bit-field)
     (DEFSUBST ,disabler () (,modifier ,disable))
     (DEFSUBST ,restorer (VALUE) (,modifier VALUE))))

(define-trap-control modify-icache-traps
                     icache-traps-off
                     restore-icache-traps
                     modify-memory-control
                     hw:%%memory-control-icache-error-enable
                     hw:$$icache-trap-disable-reset)

(define-trap-control modify-1024-interrupt
                     1024-interrupt-off
                     restore-1024-interrupt
                     modify-memory-control
                     hw:%%memory-control-1024-interrupt
                     hw:$$timer-interrupt-disable-reset)

(define-trap-control modify-16384-interrupt
                     16384-interrupt-off
                     restore-16384-interrupt
                     modify-memory-control
                     hw:%%memory-control-16384-interrupt
                     hw:$$timer-interrupt-disable-reset)

(define-trap-control modify-dram-parity-trap
                     dram-parity-traps-off
                     restore-dram-parity-traps
                     modify-memory-control
                     hw:%%memory-control-dram-parity-enable
                     hw:$$dram-parity-disable)

;;; This one is special.
;(define-trap-control modify-single-step-trap
;                    single-step-trap-off
;                    restore-single-step-trap
;                    modify-memory-control
;                    hw:%%memory-control-single-step-enable
;                    hw:$$trap-disable)

(define-trap-control modify-synchronous-traps
                     synchronous-traps-off
                     restore-synchronous-traps
                     modify-memory-control
                     hw:%%memory-control-synchronous-trap-enable
                     hw:$$trap-disable)

(define-trap-control modify-datatype-trap
                      datatype-traps-off
                      restore-datatype-traps
                      modify-memory-control
                      hw:%%memory-control-datatype-trap-enable
                      hw:$$trap-disable)

(define-trap-control modify-overflow-traps
                      overflow-traps-off
                      restore-overflow-traps
                      modify-memory-control
                      hw:%%memory-control-overflow-trap-enable
                      hw:$$trap-disable)

(define-trap-control modify-asynchronous-traps
                      asynchronous-traps-off
                      restore-asynchronous-traps
                      modify-memory-control
                      hw:%%memory-control-asynchronous-trap-enable
                      hw:$$trap-disable)

(define-trap-control modify-heap-underflow-trap
                      heap-underflow-trap-off
                      restore-heap-underflow-trap
                      modify-processor-control
                      hw:%%processor-control-heap-underflow-trap-enable
                      hw:$$call-heap-underflow-trap-disable)

(define-trap-control modify-floating-point-trap
                      floating-point-trap-off
                      restore-floating-point-trap
                      modify-processor-control
                      hw:%%processor-control-floating-point-trap-enable
                      hw:$$floating-point-trap-disable)

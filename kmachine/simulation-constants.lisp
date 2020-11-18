;;; -*- Mode:LISP; Package:SIMULATOR; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register bit fields
;;;;;;;;;;;;;;;;;;;;;;;;

;;; These bit fields are wired into the hardware.

;;; Trap register

(defconstant %%k-trap-always-zero                (byte 1.  0.))
(defconstant %%k-trap-16384-microsecond          (byte 1.  1.))
(defconstant %%k-trap-1024-microsecond           (byte 1.  2.))
(defconstant %%k-trap-nubus-interrupt-0          (byte 1.  3.))
(defconstant %%k-trap-nubus-interrupt-1          (byte 1.  4.))
(defconstant %%k-trap-nubus-interrupt-2          (byte 1.  5.))
(defconstant %%k-trap-nubus-interrupt-3          (byte 1.  6.))
(defconstant %%k-trap-nubus-interrupt-4          (byte 1.  7.))
(defconstant %%k-trap-nubus-interrupt-5          (byte 1.  8.))
(defconstant %%k-trap-nubus-interrupt-6          (byte 1.  9.))
(defconstant %%k-trap-nubus-interrupt-7          (byte 1. 10.))
(defconstant %%k-trap-spare-11                   (byte 1. 11.))
(defconstant %%k-trap-29332-overflow             (byte 1. 12.))
(defconstant %%k-trap-datatype                   (byte 1. 13.))
(defconstant %%k-trap-spare-14                   (byte 1. 14.))
(defconstant %%k-trap-call-stack-overflow        (byte 1. 15.))
(defconstant %%k-trap-floating-point             (byte 1. 16.))
(defconstant %%k-trap-memory-trap-bits           (byte 13. 16.))
(defconstant %%k-trap-memory-write-volatility    (byte 1. 17.))
(defconstant %%k-trap-memory-write-map-fault     (byte 1. 18.))
(defconstant %%k-trap-memory-write-nubus-timeout (byte 1. 19.))
(defconstant %%k-trap-memory-write-nubus-error   (byte 1. 20.))
(defconstant %%k-trap-memory-read-transport      (byte 1. 21.))
(defconstant %%k-trap-memory-read-map-fault      (byte 1. 22.))
(defconstant %%k-trap-memory-read-nubus-timeout  (byte 1. 23.))
(defconstant %%k-trap-memory-read-nubus-error    (byte 1. 24.))
(defconstant %%k-trap-memory-read-parity-error   (byte 1. 25.))
(defconstant %%k-trap-icache-map-fault           (byte 1. 26.))
(defconstant %%k-trap-icache-nubus-timeout       (byte 1. 27.))
(defconstant %%k-trap-icache-nubus-error         (byte 1. 28.))
(defconstant %%k-trap-icache-parity-error        (byte 1. 29.))
(defconstant %%k-trap-single-step-trace          (byte 1. 30.))
(defconstant %%k-trap-reset                      (byte 1. 31.))

;;; Processor status register

(defconstant %%k-processor-status-eco-jumper            (byte 4.  0.))
(defconstant %%k-processor-status-floating-point-status (byte 4.  4.))
(defconstant %%k-processor-status-floating-point-ready  (byte 1.  8.))
(defconstant %%k-processor-status-call-stack-immediate-bits (byte 4. 9.))
;; unused (byte 3. 13.)
(defconstant %%k-processor-status-jump-bit              (byte 1. 16.))
(defconstant %%k-processor-status-delayed-jump-bit      (byte 1. 17.))
(defconstant %%k-processor-status-alu-boxed-bit         (byte 1. 18.))
;; unused (byte 13. 19.)

;;; Processor control register

(defconstant %%k-processor-control-icache-a-enable                        (byte 1.  0.))
(defconstant %%k-processor-control-icache-b-enable                        (byte 1.  1.))
(defconstant %%k-processor-control-icache-z-enable                        (byte 1.  2.))
;; unused  (byte 1.  3.)
(defconstant %%k-processor-control-jump-indirect                          (byte 1.  4.))
(defconstant %%k-processor-control-floating-point-status-ram-write-enable (byte 1.  5.))
(defconstant %%k-processor-control-box-mode                               (byte 1.  6.))
(defconstant %%k-processor-control-halt-processor                         (byte 1.  7.))
(defconstant %%k-processor-control-data-bit                               (byte 1.  8.))
(defconstant %%k-processor-control-misc                                   (byte 4.  9.))
(defconstant %%k-processor-control-stack-group-number                     (byte 4. 13.))
(defconstant %%k-processor-control-call-stack-load                        (byte 1. 17.))
(defconstant %%k-processor-control-stack-overflow-trap-enable             (byte 1. 18.))
(defconstant %%k-processor-control-floating-point-trap-enable             (byte 1. 19.))
;;; The unused bytes can be written and read.  The unimplement ones cannot.
; unused        (byte 5. 19.)
; unimplemented (byte 8. 24.)

(defconstant $$icache-set-disable 0)            ;Also flushes the cache.
(defconstant $$icache-set-enable  1)

;;; The jump indirect bit is used to restart traps
;;; on conditional jumps.  (Don't ask).

(defconstant $$floating-point-status-ram-read  0)
(defconstant $$floating-point-status-ram-write 1)

(defconstant $$box-mode-normal 0)               ;For reloading boxed bits on
(defconstant $$box-mode-reload 1)               ;stack group swaping.

;;; The data and misc bits are used to supply extra bits
;;; to certain things when loading (e.g. the call stack).

;;; The call stack has 16 "stack groups"
;;;(i.e. 4 extra address lines into the call hardware.)

(defconstant $$call-stack-normal-mode          0)
(defconstant $$call-stack-special-load-sources 1)

(defconstant $$call-stack-overflow-trap-disable 0)      ;Clears trap also.
(defconstant $$call-stack-overflow-trap-disable 1)

(defconstant $$floating-point-trap-disable 0)
(defconstant $$floating-point-trap-enable  1)


;;; Memory status register

(defconstant %%k-memory-status-nubus-slot-id              (byte 3.  0.))
(defconstant %%k-memory-status-eco-jumper-number          (byte 4.  4.))
(defconstant %%k-memory-status-nubus-bootstrap-mode       (byte 3.  8.))
;unused (byte 1. 11.)
;;; Note, the GC-TRAP enable overlaps with the TRANSPORT TYPE
;;; GC-TRAP is valid on read, TRANSPORT TYPE is valid on write
(defconstant %%k-memory-status-cycle-type                 (byte 1. 12.))
(defconstant %%k-memory-status-gc-trap-enable             (byte 1. 13.))
(defconstant %%k-memory-status-transport-ram-bits         (byte 2. 13.))
(defconstant %%k-memory-status-md-not-boxed-bit           (byte 1. 15.))
(defconstant %%k-memory-status-vma-not-boxed-bit          (byte 1. 16.))
(defconstant %%k-memory-status-read-md-will-fault         (byte 1. 17.))
(defconstant %%k-memory-status-read-md-will-trans-trap    (byte 1. 18.))
;unused (byte 2. 19.)
(defconstant %%k-memory-status-parity-error               (byte 1. 21.))
(defconstant %%k-memory-status-autoboot-jumper-bit        (byte 1. 22.))
(defconstant %%k-memory-status-16meg                      (byte 1. 23.))
;undefined (byte 8. 24.)

(defconstant $$unboxed 0)
(defconstant $$boxed   1)

(defconstant $$memory-eco-0  #b0000)
(defconstant $$memory-eco-1  #b0001)
(defconstant $$memory-eco-2  #b0011)
(defconstant $$memory-eco-3  #b0010)
(defconstant $$memory-eco-4  #b0110)
(defconstant $$memory-eco-5  #b0111)
(defconstant $$memory-eco-6  #b0101)
(defconstant $$memory-eco-7  #b0100)

(defconstant $$memory-eco-8  #b1100)
(defconstant $$memory-eco-9  #b1101)
(defconstant $$memory-eco-10 #b1111)
(defconstant $$memory-eco-11 #b1110)
(defconstant $$memory-eco-12 #b1010)
(defconstant $$memory-eco-13 #b1011)
(defconstant $$memory-eco-14 #b1001)
(defconstant $$memory-eco-15 #b1000)


;;; modes 0 through 5 are software definable
(defconstant $$bootstrap-mode-short-reset 6)    ;NuBUS Bus Reset
(defconstant $$bootstrap-mode-cold        7)    ;NuBUS System Reset

(defconstant $$memory-cycle-type-write 0)
(defconstant $$memory-cycle-type-read  1)

;;; GC trap enable is defined elsewhere.
;;; A bit of the vma-start-write or md-start-write comes
;;; through inverted.

;;; Transporter types are defined elsewhere.
;;; Two bits of the vma-start-read instruction come through
;;; inverted.

;;; Note that the vma and md boxed bits are inverted!

(defconstant $$status-boxed     0)
(defconstant $$status-not-boxed 1)

;;; For trans trap or map faults
(defconstant $$md-will-cause-trap     0)
(defconstant $$md-will-not-cause-trap 1)

(defconstant $$parity-error-has-occurred     0)
(defconstant $$parity-error-has-not-occurred 1)

(defconstant $$autoboot-jumper-master-external 0)       ;NuBUS cycles will hang.
(defconstant $$autoboot-jumper-go-for-it       1)

(defconstant $$16meg-or-less 1)                 ;Single sided strips
(defconstant $$32meg         0)                 ;Double sided strips


;;; Memory control register

(defconstant %%k-memory-control-statistics-mode          (byte 1.  0.))
(defconstant %%k-memory-control-statistics-source        (byte 3.  1.))
(defconstant %%k-memory-control-statistics-polarity      (byte 1.  4.))
(defconstant %%k-memory-control-leds                     (byte 3.  5.))
(defconstant %%k-memory-control-nubus-tm0-bit            (byte 1.  8.))
(defconstant %%k-memory-control-nubus-low-address-bits   (byte 2. 10.))
(defconstant %%k-memory-control-icache-error-enable      (byte 1. 11.))
(defconstant %%k-memory-control-1024-interrupt           (byte 1. 12.))
(defconstant %%k-memory-control-16384-interrupt          (byte 1. 13.))
(defconstant %%k-memory-control-write-wrong-parity       (byte 1. 14.))
(defconstant %%k-memory-control-low-high-map-select      (byte 1. 15.))
(defconstant %%k-memory-control-transporter-mode         (byte 2. 16.))
(defconstant %%k-memory-control-bootprom-disable         (byte 1. 18.))
(defconstant %%k-memory-control-dram-parity-enable       (byte 1. 19.))
; unused (byte 5. 20.)
(defconstant %%k-memory-control-trap-bits                (byte 6. 26.))
(defconstant %%k-memory-control-single-step-enable       (byte 1. 26.))
(defconstant %%k-memory-control-synchronous-trap-enable  (byte 1. 27.))
(defconstant %%k-memory-control-datatype-trap-enable     (byte 1. 28.))
(defconstant %%k-memory-control-overflow-trap-enable     (byte 1. 29.))
(defconstant %%k-memory-control-asynchronous-trap-enable (byte 1. 30.))
(defconstant %%k-memory-control-master-trap-enable       (byte 1. 31.))

(defconstant $$high-trap-bits   #b0)
(defconstant $$low-trap-bits    #b1)

(defconstant $$bootprom-on  #b0)
(defconstant $$bootprom-off #b1)

(defconstant $$trap-disabled #b0)
(defconstant $$trap-enabled  #b1)

(defconstant *maskable-traps*         #b10000000000000000000000000000000)
(defconstant *master-trap-mask*       #b11111111111111111111111111111111)
(defconstant *synchronous-trap-mask*  #b10111111111111101000000000000000)
(defconstant *asynchronous-trap-mask* #b10000000000000000000011111111110)
(defconstant *single-step-mask*       #b11000000000000000000000000000000)
(defconstant *overflow-mask*          #b10000000000000000001000000000000)
(defconstant *floating-point-mask*    #b10000000000000010000000000000000)
(defconstant *datatype-mask*          #b10000000000000000010000000000000)

(defconstant *trap-masks* (make-array (expt 2 6.)))

;;; These bytes determine the characteristics of the address space
;;; of the machine.  The address space is a maximum of 2^26. Qs
;;; and has a mapping granularity of 2^10 Qs.

(defconstant %%k-unmapped-vma-byte (byte 10.  0.))
(defconstant %%k-mapped-vma-byte   (byte 16. 10.))

;;; Fields in the map.
;;; The first 8 bits are connected to various hardware
;;; features.

(defconstant %%k-map-low-trap-bits            (byte  2.  0.))
(defconstant %%k-map-low-valid-bit            (byte  1.  0.))
(defconstant %%k-map-low-write-enable-bit     (byte  1.  1.))

(defconstant %%k-map-high-trap-bits           (byte  2.  2.))
(defconstant %%k-map-high-valid-bit           (byte  1.  2.))
(defconstant %%k-map-high-write-enable-bit    (byte  1.  3.))

(defconstant %%k-map-volatility               (byte  3.  4.))
(defconstant %%k-map-local-memory-bit         (byte  1.  7.))

;;; (byte 4. 8.) is software defined

;;; These bits are used to address the memory.

(defconstant %%k-map-off-board-address        (byte 20. 12.))
(defconstant %%k-map-on-board-address         (byte 13. 12.))
;; unused if on board                         (byte  7. 25.)

(defconstant $$can-read  #b1)

(defconstant $$can-write #b11)

(defconstant $$cluster-non-local-memory #b0)
(defconstant $$cluster-local-memory     #b1)

(defconstant *GC-RAM* (user:make-array 4096. :type :art-4b))

(defconstant %%k-gc-ram-volatility         (byte 3. 0.))
(defconstant %%k-gc-ram-transporter-select (byte 1. 3.))

(defconstant $$transporter-select-trap-on-bit-1 0.)
(defconstant $$transporter-select-trap-on-bit-2 1.)

;;; The gc-ram is addressed by a byte of the MD.  This determines
;;; the granularity of volatile regions and flippable regions.
;;; This granularity is called a quantum, and is used for other
;;; things in the machine.  This is the real physical constraint.

(defconstant %%k-gc-ram-md-byte (byte 12. 14.))

(defconstant *TRANSPORTER-RAM* (user:make-array 4096. :type :art-4b))

;;; Addressing of transporter ram by upper bits of
;;; MD determines the maximum size of the data type field.

(defconstant %%k-transporter-md-byte                (byte 6. 26.))

(defconstant %%k-transporter-ram-gc-trap-enable (byte 1. 0.))
(defconstant %%k-transporter-ram-bit-1          (byte 1. 1.))
(defconstant %%k-transporter-ram-bit-2          (byte 1. 2.))
(defconstant %%k-transporter-ram-always-gc-trap (byte 1. 3.))

(defconstant $$gc-trap-disable #b0)
(defconstant $$gc-trap-enable  #b1)

(defconstant $$transporter-doesnt-trap #b0)
(defconstant $$transporter-traps       #b1)

(defconstant $$dont-always-trap #b0)
(defconstant $$always-trap      #b1)

(defconstant $$read-no-cdr 0.)
(defconstant $$read-cdr    1.)

(defconstant *vma-boxed-options*
             (list (list ""         'K-WRITE-VMA-BOXED)
                   (list "-UNBOXED" 'K-WRITE-VMA-UNBOXED)))

(defconstant *md-boxed-options-for-vma*
             (list (list ""            $$unboxed)
                   (list "-MD-UNBOXED" $$boxed)))

(defconstant *md-boxed-options*
             (list (list ""         'k-write-md-boxed)
                   (list "-UNBOXED" 'k-write-md-unboxed)))

(defconstant *cdr-options*
             (list (list ""     $$read-no-cdr)
                   (list "-CDR" $$read-cdr)))

(defconstant *gc-options*
             (list (list "-OPTION-0" 1)
                   (list "-OPTION-1" 0)))

(defconstant *transporter-options*
             (list (list "-OPTION-0" 0)
                   (list "-OPTION-1" 1)
                   (list "-OPTION-2" 2)
                   (list "-OPTION-3" 3)))

(defconstant %%k-data-type (byte  6. 26.))
(defconstant %%k-pointer   (byte 26.  0.))

;;;;;;;;;;;;;;;;;;
;;; Data type ram
;;;;;;;;;;;;;;;;;;

(defconstant %%k-data-type-ram-byte (byte 6. 26.))

(defconstant *DATA-TYPE-RAM* (user:make-array '(2. 64. 2. 64. 8.) :type :art-1b
                                         :initial-element 0.))

;;;;;;;;
;;; ALU
;;;;;;;;

(defconstant $$boxed-code-pass-left     0)
(defconstant $$boxed-code-pass-right    1)
(defconstant $$boxed-code-force-unboxed 2)
(defconstant $$boxed-code-force-boxed   3)

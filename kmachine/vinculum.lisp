;;; -*- Mode:LISP; Package:HARDWARE; Readtable:CL; Base:10 -*-

;;; This file defines the extreme low level of the interface to the
;;; hardware.  If the hardware changes, this file will, too.  If this
;;; file doesn't change, you will lose big.  There is no way to check
;;; consistancy.  Also, use the symbolic bit definitions where possible.

;;; The meaning of the software definable bits is not here, but in
;;; another file.  This file cannot change without ECO's being done.

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register bit fields
;;;;;;;;;;;;;;;;;;;;;;;;

;;; These registers are read and written with functional sources
;;; and destinations.  They directly change the behavior of the
;;; processor.  I.E.  Don't frob these idly.

;;; Trap register

;;; Bit comes on when trapping.  Note:  If certain traps are
;;; disabled, the bit will still come on, but have no effect.
;;; Be forewarned:  If the bit is still on when these traps
;;; are enabled, the processor will get a trap.

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
(defconstant %%k-trap-memory-bits                (byte 9. 16.))
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

;;; Processor control register

(defconstant %%k-processor-control-icache-a-enable                        (byte 1.  0.))
(defconstant %%k-processor-control-icache-b-enable                        (byte 1.  1.))
(defconstant %%k-processor-control-icache-z-enable                        (byte 1.  2.))
(defconstant %%k-processor-control-opc-select                             (byte 1.  3.))
(defconstant %%k-processor-control-jump-indirect                          (byte 1.  4.))
(defconstant %%k-processor-control-floating-point-status-ram-write-enable (byte 1.  5.))
(defconstant %%k-processor-control-box-mode                               (byte 1.  6.))
(defconstant %%k-processor-control-spare-7                                (byte 1.  7.))
(defconstant %%k-processor-control-data-bit                               (byte 1.  8.))
(defconstant %%k-processor-control-misc                                   (byte 4.  9.))
(defconstant %%k-processor-control-stack-group-number                     (byte 4. 13.))
(defconstant %%k-processor-control-call-stack-load                        (byte 1. 17.))
(defconstant %%k-processor-control-floating-point-trap-enable             (byte 1. 18.))
;;; The unused bytes can be written and read.  The unimplement ones cannot.
; unused        (byte 5. 19.)
; unimplemented (byte 8. 24.)

(defconstant $$icache-set-disable 0)            ;Also flushes the cache.
(defconstant $$icache-set-enable  1)

(defconstant $$opc-2 0)
(defconstant $$opc-1 1)

(defconstant $$floating-point-status-ram-read  0)
(defconstant $$floating-point-status-ram-write 1)

(defconstant $$box-mode-normal 0)               ;For reloading boxed bits or something.
(defconstant $$box-mode-trap   1)

;;; The data and misc bits are used to supply extra bits
;;; to certain things when loading (e.g. the call stack).

;;; The call stack has 16 "stack groups" (i.e. 4 extra address lines)

(defconstant $$call-stack-normal-mode          0)
(defconstant $$call-stack-special-load-sources 1)

(defconstant $$floating-point-trap-disable 0)
(defconstant $$floating-point-trap-enable  1)


;;; Processor status register

(defconstant %%k-processor-status-eco-jumper            (byte 4.  0.))
(defconstant %%k-processor-status-floating-point-status (byte 4.  4.))
;; unused (byte 7. 9.)
(defconstant %%k-processor-status-jump-bit              (byte 1. 16.))
(defconstant %%k-processor-status-delayed-jump-bit      (byte 1. 17.))
(defconstant %%k-processor-status-alu-boxed-bit         (byte 1. 18.))
;; unused (byte 13. 19.)

;;; We number these with Grey code so that stupid technicians
;;; will only have to change one wire each time.

(defconstant $$processor-eco-0  #b0000)
(defconstant $$processor-eco-1  #b0001)
(defconstant $$processor-eco-2  #b0011)
(defconstant $$processor-eco-3  #b0010)
(defconstant $$processor-eco-4  #b0110)
(defconstant $$processor-eco-5  #b0111)
(defconstant $$processor-eco-6  #b0101)
(defconstant $$processor-eco-7  #b0100)

(defconstant $$processor-eco-8  #b1100)
(defconstant $$processor-eco-9  #b1101)
(defconstant $$processor-eco-10 #b1111)
(defconstant $$processor-eco-11 #b1110)
(defconstant $$processor-eco-12 #b1010)
(defconstant $$processor-eco-13 #b1011)
(defconstant $$processor-eco-14 #b1001)
(defconstant $$processor-eco-15 #b1000)

;;; Floating point status goes here.

;;; The three processor status bits are active low.


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
(defconstant %%k-memory-control-l-c-map-select           (byte 1. 15.))
(defconstant %%k-memory-control-transporter-mode         (byte 2. 16.))
(defconstant %%k-memory-control-bootprom-disable         (byte 1. 18.))
(defconstant %%k-memory-control-dram-parity-enable       (byte 1. 19.))
; unused (byte 5. 20.)
(defconstant %%k-memory-control-single-step-enable       (byte 1. 26.))
(defconstant %%k-memory-control-synchronous-trap-enable  (byte 1. 27.))
(defconstant %%k-memory-control-datatype-trap-enable     (byte 1. 28.))
(defconstant %%k-memory-control-overflow-trap-enable     (byte 1. 29.))
(defconstant %%k-memory-control-asynchronous-trap-enable (byte 1. 30.))
(defconstant %%k-memory-control-master-trap-enable       (byte 1. 31.))

;;; The following are synchronous traps:
;;; All icache, read and write memory traps.
;;; The call stack overflow trap.

;;; The following are asynchronous traps:
;;; All nubus interrupts and the two timers. (two-timers!!?)

;;; The master trap enable turns off all traps. (Except reset!)

(defconstant $$statistics-edge-trigger 0)
(defconstant $$statistics-duration     1)

(defconstant $$statistics-source-icache-hit             0)
(defconstant $$statistics-source-processor-memory-cycle 1)
(defconstant $$statistics-source-instruction-status-bit 2)
;;; undefined 3
(defconstant $$statistics-source-pc-in-high-core        3)
;;; undefined 4 through 7

(defconstant $$statistics-parity-true   0)
(defconstant $$statistics-parity-invert 1)

(defconstant $$led-on  0)
(defconstant $$led-off 1)

;;; What do the nubus bits do?

;;; Bring these bits low to reset the condition.
(defconstant $$icache-trap-disable-reset 0)
(defconstant $$icache-trap-enable        1)

;;; Bring these bits low to reset the condition.
(defconstant $$timer-interrupt-disable-reset 0)
(defconstant $$timer-interrupt-enable        1)

(defconstant $$write-normal-parity 0)
(defconstant $$write-wrong-parity  1)

;;; Selects which trap bits in maps to use.
(defconstant $$c-map-bits    0)
(defconstant $$lisp-map-bits 1)

;;; The transporter modes are defined elsewhere.

(defconstant $$bootprom-on  0)
(defconstant $$bootprom-off 1)

(defconstant $$dram-parity-disable 0)
(defconstant $$dram-parity-enable  1)

;;; All traps are disabled and enabled the same way.

(defconstant $$trap-disable 0)
(defconstant $$trap-enable  1)

;;; Memory status register

(defconstant %%k-memory-status-nubus-slot-id              (byte 3.  0.))
(defconstant %%k-memory-status-eco-jumper-number          (byte 3.  4.))
(defconstant %%k-memory-status-nubus-bootstrap-mode       (byte 3.  7.))
;unused (byte 1. 10.)
;;; Note, the GC-TRAP enable overlaps with the TRANSPORT TYPE
;;; GC-TRAP is valid on read, TRANSPORT TYPE is valid on write
(defconstant %%k-memory-status-cycle-type                 (byte 1. 11.))
(defconstant %%k-memory-status-gc-trap-enable             (byte 1. 12.))
(defconstant %%k-memory-status-transport-type             (byte 2. 12.))
(defconstant %%k-memory-status-md-not-boxed-bit           (byte 1. 14.))
(defconstant %%k-memory-status-vma-not-boxed-bit          (byte 1. 15.))
(defconstant %%k-memory-status-autoboot-jumper-bit        (byte 1. 16.))
(defconstant %%k-memory-status-16meg                      (byte 1. 17.))
;undefined (byte 14. 18.)

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


(defconstant $$bootstrap-mode-cold        0)
(defconstant $$bootstrap-mode-short-reset 1)
;;; modes 2 through 7 are software definable

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

(defconstant $$autoboot-jumper-master-external 0)
(defconstant $$autoboot-jumper-go-for-it       1)

(defconstant $$16meg-or-less 1)
(defconstant $$32meg         0)

;;;;;;;;;;;;;;;;;;;;
;;; The memory maps
;;;;;;;;;;;;;;;;;;;;

;;; The mapped byte is used to address the maps.
;;; The unmapped byte is used to supply the low bits
;;; of the physical address.  The machine has a mapping
;;; granularity of 2^10. Qs

(defconstant %%unmapped-vma-byte (byte 10.  0.))
(defconstant %%mapped-vma-byte   (byte 16. 10.))

;;; Fields in the map.
;;; The first 8 bits are connected to various hardware
;;; features.

(defconstant %%k-map-lisp-trap-bits            (byte  2.  0.))
(defconstant %%k-map-lisp-valid-bit            (byte  1.  0.))
(defconstant %%k-map-lisp-write-enable-bit     (byte  1.  1.))

(defconstant %%k-map-c-trap-bits           (byte  2.  2.))
(defconstant %%k-map-c-valid-bit           (byte  1.  2.))
(defconstant %%k-map-c-write-enable-bit    (byte  1.  3.))

(defconstant %%k-map-volatility               (byte  3.  4.))
(defconstant %%k-map-local-memory-bit         (byte  1.  7.))

;;; (byte 4. 8.) is software defined

;;; These bits are used to address the memory.

(defconstant %%k-map-off-board-address        (byte 20. 12.))
(defconstant %%k-map-on-board-address         (byte 13. 12.))
;; unused if on board                         (byte  7. 25.)

(defconstant $$map-invalid 0)
(defconstant $$map-valid   1)

(defconstant $$map-write-disable 0)
(defconstant $$map-write-enable  1)

;;; We have 8 levels of volatility.  The lower the level
;;; the less volatile the storage.

(defconstant $$map-non-local 0)
(defconstant $$map-local     1)

;;;;;;;;;;;
;;; GC Ram
;;;;;;;;;;;

;;; The gc-ram is addressed by a byte of the MD.  This determines
;;; the granularity of volatile regions and flippable regions.
;;; This granularity is called a quantum, and is used for other
;;; things in the machine.

(defconstant %%k-gc-ram-md-byte (byte 12. 14.))

(defconstant %%gc-ram-quantum-volatility (byte 3. 0.))
(defconstant %%gc-ram-quantum-oldspace   (byte 1. 3.))

(defconstant $$not-oldspace 0)
(defconstant $$oldspace     1)

;;;;;;;;;;;;;;;;;;;;
;;; Transporter RAM
;;;;;;;;;;;;;;;;;;;;

;;; The transporter RAM is addressed by several things.
;;; The boxed bit of the VMA, the boxed bit of the MD,
;;; the transporter mode from the memory control register,
;;; the transport type from the instruction that started
;;; the read, and finally a byte from the MD.

(defconstant %%transporter-ram-md-byte (byte 6. 26.))

;;; The transporter ram has 4 bits of output.  These bits
;;; are connected to the MFO bus displaced by 4 bits.

(defconstant %%transporter-ram-trappable-pointer    (byte 1. 4.))
(defconstant %%transporter-ram-trap-if-oldspace     (byte 1. 5.))
(defconstant %%transporter-ram-trap-if-not-oldspace (byte 1. 6.))
(defconstant %%transporter-ram-box-error            (byte 1. 7.))

(defconstant $$non-trappable-pointer 0)
(defconstant $$trappable-pointer     1)

(defconstant $$dont-trap-if-oldspace 0)
(defconstant $$trap-if-oldspace      1)

(defconstant $$dont-trap-if-not-oldspace 0)
(defconstant $$trap-if-not-oldspace      1)

(defconstant $$no-box-error 0)
(defconstant $$box-error    1)

;;; The transporter RAM is connected to the GC ram and volatility
;;; hardware.  It has two functions.
;;;
;;; 1)  On writes, the trappable pointer line causes a volatility
;;;     comparison to take place.  The box error bit will cause
;;;     a write trap regardless of whether the volatility was
;;;     incorrect.
;;;
;;; 2)  On reads, the GC RAM indicates whether the word read into
;;;     the MD points to oldspace.  This selects the trap-if-oldspace
;;;     or trap-if-not-oldspace line of the transporter.  If the
;;;     selected line is asserted, the processor traps.
;;;
;;; The processor must read the transporter RAM to find out why it
;;; trapped, but presumably, trapping is infrequent enough to not
;;; worry about caching this data.

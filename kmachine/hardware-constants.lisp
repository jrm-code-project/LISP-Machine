;;; -*- Mode:LISP; Package:HARDWARE; Readtable:CL; Base:10. -*-

;;; This file defines the extreme low level of the interface to the
;;; hardware.  If the hardware changes, this file will, too.  If this
;;; file doesn't change, you will lose big.  There is no way to check
;;; consistancy.  Also, use the symbolic bit definitions where possible.

;;; The meaning of the software definable bits is not here, but in
;;; another file.  This file cannot change without ECO's being done.

(lisp::import
  (lisp::list
    (lisp::intern "DEFCONSTANT" (lisp::find-package "PRIMITIVES" lisp::*package*))
    (lisp::intern "BYTE"        (lisp::find-package "PRIMITIVES" lisp::*package*))))  ;; WKF into package 'K-HW


(defconstant frame-size              16.)
(defconstant number-of-global-frames 16.)

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

(defconstant %%trap-always-zero                (byte 1.  0.))
(defconstant %%trap-16384-microsecond          (byte 1.  1.))
(defconstant %%trap-1024-microsecond           (byte 1.  2.))
(defconstant %%trap-nubus-interrupt-0          (byte 1.  3.))
(defconstant %%trap-nubus-interrupt-1          (byte 1.  4.))
(defconstant %%trap-nubus-interrupt-2          (byte 1.  5.))
(defconstant %%trap-nubus-interrupt-3          (byte 1.  6.))
(defconstant %%trap-nubus-interrupt-4          (byte 1.  7.))
(defconstant %%trap-nubus-interrupt-5          (byte 1.  8.))
(defconstant %%trap-nubus-interrupt-6          (byte 1.  9.))
(defconstant %%trap-nubus-interrupt-7          (byte 1. 10.))
(defconstant %%trap-spare-11                   (byte 1. 11.))
(defconstant %%trap-29332-overflow             (byte 1. 12.))
(defconstant %%trap-datatype                   (byte 1. 13.))
(defconstant %%trap-instruction-bit            (byte 1. 14.))
(defconstant %%trap-call-stack-overflow        (byte 1. 15.))
(defconstant %%trap-floating-point             (byte 1. 16.))
(defconstant %%trap-memory-bits                (byte 9. 16.))
(defconstant %%trap-memory-write-volatility    (byte 1. 17.))
(defconstant %%trap-memory-write-map-fault     (byte 1. 18.))
(defconstant %%trap-memory-write-nubus-timeout (byte 1. 19.))
(defconstant %%trap-memory-write-nubus-error   (byte 1. 20.))
(defconstant %%trap-memory-read-transport      (byte 1. 21.))
(defconstant %%trap-memory-read-map-fault      (byte 1. 22.))
(defconstant %%trap-memory-read-nubus-timeout  (byte 1. 23.))
(defconstant %%trap-memory-read-nubus-error    (byte 1. 24.))
(defconstant %%trap-memory-read-parity-error   (byte 1. 25.))
(defconstant %%trap-icache-map-fault           (byte 1. 26.))
(defconstant %%trap-icache-nubus-timeout       (byte 1. 27.))
(defconstant %%trap-icache-nubus-error         (byte 1. 28.))
(defconstant %%trap-icache-parity-error        (byte 1. 29.))
(defconstant %%trap-single-step-trace          (byte 1. 30.))
(defconstant %%trap-reset                      (byte 1. 31.))

;;; Processor control register

(defconstant %%processor-control-icache-enables                         (byte 3.  0.))
(defconstant %%processor-control-icache-a-enable                        (byte 1.  0.))
(defconstant %%processor-control-icache-b-enable                        (byte 1.  1.))
(defconstant %%processor-control-icache-z-enable                        (byte 1.  2.))
(defconstant %%processor-control-spare-3                                (byte 1.  3.))
(defconstant %%processor-control-jump-indirect                          (byte 1.  4.))
(defconstant %%processor-control-floating-point-status-ram-write-enable (byte 1.  5.))
(defconstant %%processor-control-box-mode                               (byte 1.  6.))
(defconstant %%processor-control-halt-processor                         (byte 1.  7.))
(defconstant %%processor-control-data-bit                               (byte 1.  8.))
(defconstant %%processor-control-misc                                   (byte 4.  9.))
(defconstant %%processor-control-stack-group-number                     (byte 4. 13.))
(defconstant %%processor-control-spare-17                               (byte 1. 17.))
(defconstant %%processor-control-heap-underflow-trap-enable             (byte 1. 18.))
(defconstant %%processor-control-floating-point-trap-enable             (byte 1. 19.))
(defconstant %%processor-control-spare-20-through-23                    (byte 4. 20.))
;;; The unused bytes can be written and read.  The unimplement ones cannot.
; unimplemented (byte 8. 24.)

(defconstant $$icache-set-disable 0)            ;Also flushes the cache.
(defconstant $$icache-set-enable  1)

(defconstant $$icache-disable-all-sets 0)  ;; It appears that in the event-horizon calling modify-icache-enables with disable
                                           ;;    or set-a-only somehow puts the machine in single step mode and ends up
                                           ;;    in (boot-stack-groups) at the end of mega-boot.    --wkf
(defconstant $$icache-enable-set-a-only 1)              ;A is selected if ICACHE-OSEL is low
(defconstant $$icache-enable-all-sets  7)

;;; The jump indirect bit is used to restart traps
;;; on conditional jumps.  (Don't ask).

(defconstant $$floating-point-status-ram-read  0)
(defconstant $$floating-point-status-ram-write 1)

(defconstant $$box-mode-normal 0)               ;For reloading boxed bits on
(defconstant $$box-mode-reload 1)               ;stack group swaping.

(defconstant $$run  0)
(defconstant $$halt 1)

;;; The data and misc bits are used to supply extra bits
;;; to certain things when loading (e.g. the call stack).

;;; The call stack has 16 "stack groups"
;;;(i.e. 4 extra address lines into the call hardware.)

(defconstant $$call-heap-underflow-trap-disable 0)    ;; Clears trap also.
(defconstant $$call-heap-underflow-trap-enable  1)    ;; Update-trap-mask depends upon enable being the same as $$trap-enable

(defconstant $$floating-point-trap-disable 0)
(defconstant $$floating-point-trap-enable  1)


;;; Processor status register

(defconstant %%processor-status-eco-jumper                (byte 4.  0.))
(defconstant %%processor-status-floating-point-status     (byte 4.  4.))
(defconstant %%processor-status-floating-point-ready      (byte 1.  8.))
(defconstant %%processor-status-call-stack-immediate-bits (byte 4.  9.))        ;this name is obsolete use
(defconstant %%processor-status-global-return-frame       (byte 4.  9.))        ;this one
;; unused (byte 3. 13.)
(defconstant %%processor-status-jump-bit                  (byte 1. 16.))
(defconstant %%processor-status-delayed-jump-bit          (byte 1. 17.))
(defconstant %%processor-status-alu-boxed-bit             (byte 1. 18.))
(defconstant %%processor-status-return-code               (byte 1. 19.))
;; unused (byte 12. 18.)

;;; We number these with Grey code so that technicians
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

;;; Floating point status goes here.  Each field is different
;;; for the instructions.

(defconstant $$floating-point-not-ready 0)
(defconstant $$floating-point-ready     1)

;;; Call stack immediate bits are used for dumping the
;;; call hardware.

;;; The three processor status bits are active low.
;;; Return code is for multiple values.
;;; Jump bit is used for returning from traps (see processor control)
;;; Delayed jump bit may not be needed?
;;; Alu box bit for dumping the registers.  Works only because
;;; we are pipelined.

(defconstant $$return-code-one-value       0)
(defconstant $$return-code-multiple-values 1)

;;; Memory control register

;;If any of these change See code enable-deduce-trap-mask in nuclear-control.  --wkf

(defconstant %%memory-control-statistics-mode          (byte 1.  0.))
(defconstant %%memory-control-statistics-source        (byte 3.  1.))
(defconstant %%memory-control-statistics-polarity      (byte 1.  4.))
(defconstant %%memory-control-leds                     (byte 3.  5.))
(defconstant %%memory-control-led-0                    (byte 1.  5.))
(defconstant %%memory-control-led-1                    (byte 1.  6.))
(defconstant %%memory-control-led-2                    (byte 1.  7.))
(defconstant %%memory-control-nubus-transfer-mode      (byte 3.  8.))
(defconstant %%memory-control-icache-error-enable      (byte 1. 11.))
(defconstant %%memory-control-1024-interrupt           (byte 1. 12.))
(defconstant %%memory-control-16384-interrupt          (byte 1. 13.))
(defconstant %%memory-control-write-wrong-parity       (byte 1. 14.))
(defconstant %%memory-control-l-c-map-select           (byte 1. 15.))
(defconstant %%memory-control-transporter-mode         (byte 2. 16.))
(defconstant %%memory-control-bootprom-disable         (byte 1. 18.))
(defconstant %%memory-control-dram-parity-enable       (byte 1. 19.))
; unused (byte 3. 20.)
(defconstant %%memory-control-reset-trap-bit           (byte 1. 24.))
; unused (byte 1. 25.)
(defconstant %%memory-control-single-step-enable       (byte 1. 26.))
(defconstant %%memory-control-synchronous-trap-enable  (byte 1. 27.))
(defconstant %%memory-control-datatype-trap-enable     (byte 1. 28.))
(defconstant %%memory-control-overflow-trap-enable     (byte 1. 29.))
(defconstant %%memory-control-asynchronous-trap-enable (byte 1. 30.))
(defconstant %%memory-control-master-trap-enable       (byte 1. 31.))

;;; The following are synchronous traps:
;;; All icache, read and write memory traps.
;;; The call stack overflow trap.

;;; The following are asynchronous traps:
;;; All nubus interrupts and the two timers. (two-timers!!?)

;;; The master trap enable turns off all traps. (Except reset!)

;values for statistics-mode
(defconstant $$statistics-edge-trigger 0)
(defconstant $$statistics-duration     1)

;values for statistics-source
(defconstant $$statistics-source-icache-hit             0)
(defconstant $$statistics-source-processor-memory-cycle 1)
(defconstant $$statistics-source-instruction-status-bit 2)
;;; undefined 3
(defconstant $$statistics-source-pc-in-high-core        4)
;;; undefined 5 through 7

(defconstant $$statistics-polarity-true   0)
(defconstant $$statistics-polarity-invert 1)

(defconstant $$led-on  0)
(defconstant $$led-off 1)

;;;         NUBUS only
;;; 31                           0
;;;    high 16         low 16
;;; byte 3  byte 2  byte 1  byte 0

;values for nubus-transfer-mode
(defconstant $$nubus-transfer-32-bits      0)
(defconstant $$nubus-transfer-byte-0       1)
(defconstant $$nubus-transfer-block        2)
(defconstant $$nubus-transfer-byte-1       3)
(defconstant $$nubus-transfer-low-16-bits  4)
(defconstant $$nubus-transfer-byte-2       5)
(defconstant $$nubus-transfer-high-16-bits 6)
(defconstant $$nubus-transfer-byte-3       7)

;;; Bring these bits low to reset the condition.
;values for icache-error-enable
(defconstant $$icache-trap-disable-reset 0)
(defconstant $$icache-trap-enable        1);;; Update-trap-mask depends upon enable being the same as $$trap-enable

;;; Bring these bits low to reset the condition.
(defconstant $$timer-interrupt-disable-reset 0)
(defconstant $$timer-interrupt-enable        1)

;values for write-wrong-parity
(defconstant $$write-normal-parity 0)
(defconstant $$write-wrong-parity  1)

;;; Selects which trap bits in maps to use.
;values for l-c-map-select
(defconstant $$lisp-map-bits 0)
(defconstant $$c-map-bits    1)

;;; The transporter modes are defined elsewhere.  (selects section of transporter ram)

;values for bootprom disable
(defconstant $$bootprom-on  0)
(defconstant $$bootprom-off 1)

;values for dram-parity-enable
(defconstant $$dram-parity-disable 0)
(defconstant $$dram-parity-enable  1);;; Update-trap-mask depends upon enable being the same as $$trap-enable

;values for reset-trap-bit
(defconstant $$reset-trap-bit-on  0)
(defconstant $$reset-trap-bit-off 1)

;;; All traps are disabled and enabled the same way.

(defconstant $$trap-disable 0)
(defconstant $$trap-enable  1)

;;; Memory status register

(defconstant %%memory-status-nubus-slot-id              (byte 4.  0.))
(defconstant %%memory-status-eco-jumper-number          (byte 4.  4.))
(defconstant %%memory-status-nubus-bootstrap-mode       (byte 3.  8.))
(defconstant %%Memory-status-md-written-lately          (byte 1. 11.))
;;; Note, the GC-TRAP enable overlaps with the TRANSPORT TYPE
;;; GC-TRAP is valid on read, TRANSPORT TYPE is valid on write
(defconstant %%memory-status-cycle-type                 (byte 1. 12.))          ;1 -> write.
(defconstant %%memory-status-gc-trap-enable             (byte 1. 13.))
(defconstant %%memory-status-transport-type             (byte 2. 13.))
(defconstant %%memory-status-md-not-boxed-bit           (byte 1. 15.))
(defconstant %%memory-status-vma-not-boxed-bit          (byte 1. 16.))
(defconstant %%memory-status-read-md-will-fault         (byte 1. 17.))
(defconstant %%memory-status-read-md-will-trans-trap    (byte 1. 18.))
(defconstant %%memory-status-spare-19-20                (byte 2. 19.))
(defconstant %%memory-status-parity-error               (byte 1. 21.))
(defconstant %%memory-status-autoboot-jumper-bit        (byte 1. 22.))
(defconstant %%memory-status-16meg                      (byte 1. 23.))
;undefined (byte 8. 24.)

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


;these are the bits you write in bits 3,4,5 in the nubus-mode register.  bits are inverted
;  from those actually written.
;;; modes 0 through 5 are software definable
(defconstant $$bootstrap-mode-short-reset 6)    ;NuBUS Bus Reset
(defconstant $$bootstrap-mode-cold        7)    ;NuBUS System Reset

;values for md-written-lately.
(defconstant $$wmd-valid     0)   ;means md written, but no cycle started, so read-md
                                  ; is garbage.  used by trap handler save correct MD.
(defconstant $$both-md-valid 1)

;values for status-cycle-type
(defconstant $$memory-cycle-type-write 0)
(defconstant $$memory-cycle-type-read  1)

;;; GC trap enable is defined elsewhere.
;;; A bit of the vma-start-write or md-start-write comes
;;; through inverted.

;;; Transporter types are defined elsewhere.
;;; Two bits of the vma-start-read instruction come through
;;; inverted.

;;; Note that the vma and md boxed bits are inverted!

;values for md and vma not-boxed-bit s.
(defconstant $$status-boxed     0)
(defconstant $$status-not-boxed 1)

;;; For trans trap or map faults
;values for read-md-will-fault  and will-trans-trap
(defconstant $$md-will-cause-trap     0)
(defconstant $$md-will-not-cause-trap 1)

;values for status-parity-error.
(defconstant $$parity-error-has-occurred     0)
(defconstant $$parity-error-has-not-occurred 1)

;values for autoboot-jumper-bit.  this is read only.  bit is affected by hardware jumper.
(defconstant $$autoboot-jumper-master-external 0)       ;NuBUS cycles will hang.
(defconstant $$autoboot-jumper-go-for-it       1)

;values for -16meg.  affected only by jumper.
(defconstant $$16meg-or-less 1)                 ;Single sided strips
(defconstant $$32meg         0)                 ;Double sided strips

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

(defconstant %%map-lisp-trap-bits            (byte  2.  0.))
(defconstant %%map-lisp-valid-bit            (byte  1.  0.))
(defconstant %%map-lisp-write-enable-bit     (byte  1.  1.))

(defconstant %%map-c-trap-bits           (byte  2.  2.))
(defconstant %%map-c-valid-bit           (byte  1.  2.))
(defconstant %%map-c-write-enable-bit    (byte  1.  3.))

(defconstant %%map-volatility               (byte  3.  4.))
(defconstant %%map-local-memory-bit         (byte  1.  7.))

;;; (byte 4. 8.) is software defined

;;; These bits are used to address the memory.

(defconstant %%map-off-board-address                (byte 20. 12.))
(defconstant %%map-off-board-address-nubus-slot     (byte  4. 24.))
(defconstant %%map-off-board-address-nubus-constant (byte  4. 28.))
(defconstant %%map-off-board-address-nubus-quad-slot (byte 8 24.))
(defconstant %%map-on-board-address                 (byte 12. 12.))     ;this used to be (byte 13. 12.),  <14-Nov-88 rg>
        ;which potentially allows for 32 meg of on-board memory, using double sided 1meg SIMMS.
        ;however, this is a screw in NUBUS memory mode where a card can have 16 meg max simplemindedly in slot space.
        ;16 meg is really the max for on-board memory anyway, so..
;; unused if on board                               (byte  7. 25.)

(defconstant $$map-invalid 0)
(defconstant $$map-valid   1)

(defconstant $$map-write-disable 0)
(defconstant $$map-write-enable  1);;; Update-trap-mask depends upon enable being the same as $$trap-enable

;;; We have 8 levels of volatility.  The lower the level
;;; the less volatile the storage.

;values for %%map-local-memory-bit in memory map.
(defconstant $$map-non-local 0)
(defconstant $$map-local     1)

;;;;;;;;;;;
;;; GC Ram
;;;;;;;;;;;

;;; The gc-ram is addressed by a byte of the MD.  This determines
;;; the granularity of volatile regions and flippable regions.
;;; This granularity is called a quantum, and is used for other
;;; things in the machine.

(defconstant %%gc-ram-md-byte (byte 12. 14.))

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

(defconstant %%transporter-ram-bus-offset (byte 4. 4.))

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


;;; Call Hardware functional sources and destinations

;;; open/active/return
(defconstant %%ch-oar-return (byte 8.  0.))
(defconstant %%ch-oar-active (byte 8.  8.))
(defconstant %%ch-oar-open   (byte 8. 16.))

;;; return pc, return destination (read only)
(defconstant %%ch-rpcd-return-pc   (byte 24.  0.))
(defconstant %%ch-rpcd-return-dest (byte  7. 24.))

;;; call stack pointer, heap pointer
(defconstant %%ch-csphp-call-stack-pointer (byte 8. 0.))
(defconstant %%ch-csphp-heap-pointer       (byte 8. 8.))


;;;;;;;;;;;;;;;
;;; Boxed bits
;;;;;;;;;;;;;;;

(defconstant $$unboxed 0)
(defconstant $$boxed   1)

;;;;;;;;;;;;;;;
;;; byte specs
;;;;;;;;;;;;;;;

(defconstant %%bytespec-mask (byte 5. 8.))
(defconstant %%bytespec-lrot (byte 6. 0.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instruction boxed codes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $$pass-left-boxed  0)
(defconstant $$pass-right-boxed 1)
(defconstant $$force-unboxed    2)
(defconstant $$force-boxed      3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instruction byte width codes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;these for byte ALU ops.
(defconstant $$32-bit-operation 0)
(defconstant $$8-bit-operation  1)
(defconstant $$16-bit-operation 0)
(defconstant $$24-bit-operation 0)

;these for bit ALU ops.
(defconstant $$external-mask-and-shift 0)
(defconstant $$internal-shift          1)
(defconstant $$internal-mask           2)
(defconstant $$internal-mask-and-shift 3)

;;;;;;;;;;;;;;;;;;;;
;;; Data type traps
;;;;;;;;;;;;;;;;;;;;

(defconstant $$no-data-type-trap       0)
;;;; (defconstant $$trap-if-not-both-fixnum 1) --- look in firm-definitions for these!!!! khh

;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALU STATUS REGISTER
;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %%alu-status-internal-position-register  (byte 8.  0.))
(defconstant %%alu-status-internal-width-register     (byte 5.  8.))
(defconstant %%alu-status-unsigned-less-than-or-equal (byte 1. 13.))    ;(ior (not carry) zero)
(defconstant %%alu-status-less-than                   (byte 1. 14.))    ;(xor negative overflow)
(defconstant %%alu-status-less-than-or-equal          (byte 1. 15.))    ;(ior zero (xor negative overflow))
(defconstant %%alu-status-carry                       (byte 1. 16.))
(defconstant %%alu-status-negative                    (byte 1. 17.))
(defconstant %%alu-status-overflow                    (byte 1. 18.))
(defconstant %%alu-status-zero                        (byte 1. 19.) "same as EQUAL")
(defconstant %%alu-status-jump-conditions             (byte 7. 13.) "Status bits which affect jumping conditions")
(defconstant %%alu-status-link                        (byte 1. 20.))
(defconstant %%alu-status-multiply                    (byte 1. 21.))

;; There is some question of whether the next two bits are actually exchanged.
(defconstant %%alu-status-sign                        (byte 1. 22.))
;(defconstant %%alu-status-low                         (byte 1. 23.) "always zero, unused by ALU")

(defconstant %%alu-status-nibble-carries              (byte 8. 24.))

(defconstant $$alu-status-carry                      #x10000)
(defconstant $$alu-status-zero                       #x80000)
(defconstant $$alu-status-equal                      $$alu-status-zero)
(defconstant $$alu-status-negative-zero              #xA0000)
(defconstant $$alu-status-positive-zero              #x80000)
(defconstant $$alu-status-negative                   #x20000)
(defconstant $$alu-status-less-than                  $$alu-status-negative)
(defconstant $$alu-status-positive                   #x00000)
(defconstant $$alu-status-greater-than               $$alu-status-positive)

(defconstant $$alu-status-zero-and-others            #x8A000 "Bits 13, 15, and 19 on.") ;;||| These Five added 10/12/88 --wkf
(defconstant $$alu-status-equal-and-others           $$alu-status-zero-and-others)
(defconstant $$alu-status-negative-and-others        #x2C000 "Bits 14, 15, and 17 on.")
(defconstant $$alu-status-less-than-and-others       $$alu-status-negative-and-others)
(defconstant $$alu-status-negative-zero-and-others   #xAE000 "Bits 13, 14, 15, 17, and 19 on.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Physical memory layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are 32 blocks each of one megabyte.

(defconstant %%cluster-physical-address-block        (byte 5. 8.))
(defconstant %%physical-address-block-cluster-offset (byte 8. 0.))

;;;;;;;;;;;;;;;;;;;;
;;;; Maskable traps
;;;;;;;;;;;;;;;;;;;;

(defconstant *initial-trap-mask*
                   (unboxed-constant #b00000000000000000100000000000001))  ;two spare trap bits. setting these means
                           ;that if the spares somehow got set, we would get to the spurious trap handler.
(defconstant *synchronous-trap-mask*
                   (unboxed-constant #b00111111111111101000000000000000))  ;instruction-cache(ic) parity, nubus error,
                           ;nubus timeout, map fault, processor-read parity, nubus error, nubus timeout, map fault,
                           ;transporter, processor-write nubus-error, nubus timeout, gc-ram, map-fault, call stack ovflo.
(defconstant *unmaskable-synchronous-trap-mask*
                   (unboxed-constant #b00000001111111100000000000000000))  ;pr nubus error, nubus timeout, map fault,
                           ;transporter, pr nubus error, nubus timeout, gc-ram, map fault.
(defconstant *icache-trap-mask*
                   (unboxed-constant #b00111100000000000000000000000000))
(defconstant *dram-parity-mask*
                   (unboxed-constant #b00100010000000000000000000000000))       ;icache parity, pr parity.
(defconstant *heap-underflow-mask*
                   (unboxed-constant #b00000000000000001000000000000000))       ;call stack overflow!

(defconstant *asynchronous-trap-mask*
                   (unboxed-constant #b00000000000000000000011111111110))       ;nubus 7-0, 1024, 16384 timers.
(defconstant *unmaskable-asynchronous-trap-mask*
                   (unboxed-constant #b00000000000000000000011111111000))       ;nubus 7-0.
(defconstant *1024-interrupt-mask*
                   (unboxed-constant #b00000000000000000000000000000100))
(defconstant *16384-interrupt-mask*
                   (unboxed-constant #b00000000000000000000000000000010))

(defconstant *overflow-mask*
                   (unboxed-constant #b00000000000000000001000000000000))
(defconstant *floating-point-mask*
                   (unboxed-constant #b00000000000000010000000000000000))
(defconstant *datatype-mask*
                   (unboxed-constant #b00000000000000000010000000000000))
(defconstant *single-step-mask*
                   (unboxed-constant #b01000000000000000000000000000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Virtual memory limitation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *first-instruction-cluster* #x8000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot offsets for various K machine registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *slot-offset-memory* 0)
(defconstant *slot-offset-nubus-interrupts*  (hw:unboxed-constant #xFFF780))
(defconstant *cluster-offset-nubus-interrupt-0* #x1e0)
(defconstant *cluster-offset-nubus-interrupt-1* #x1e1)
(defconstant *cluster-offset-nubus-interrupt-2* #x1e2)
(defconstant *cluster-offset-nubus-interrupt-3* #x1e3)
(defconstant *cluster-offset-nubus-interrupt-4* #x1e4)
(defconstant *cluster-offset-nubus-interrupt-5* #x1e5)
(defconstant *cluster-offset-nubus-interrupt-6* #x1e6)
(defconstant *cluster-offset-nubus-interrupt-7* #x1e7)

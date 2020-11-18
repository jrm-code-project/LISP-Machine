;;; -*- Mode:LISP; Package:HARDWARE; Base:10; Readtable:CL -*-

;;;; Instruction fields and values

(defconstant %%i-stat-bit   (byte 1. 63.))

(defconstant %%i-spare-bit  (byte 1. 62.))
(defconstant %%i-trap-bit   (byte 1. 62.))

(defconstant %%i-x16        (byte 1. 61.))
(defconstant   $$i-x16-nil  0.)
(defconstant   $$i-x16-t    1.)
(defconstant %%i-cond       (byte 1. 61.))
(defconstant   $$i-cond-conditional-branch   0.)
(defconstant   $$i-cond-unconditional-branch 1.)

(defconstant %%i-call-dret-6    (byte 1. 61.))
(defconstant %%i-call-dret-5    (byte 1. 36.))
(defconstant %%i-call-dret-5-4  (byte 2. 35.))
(defconstant %%i-call-dret-5-1  (byte 5. 32.))
(defconstant %%i-call-dret-3-1  (byte 3. 32.))
(defconstant %%i-call-dret-0    (byte 1. 24.))

(defconstant %%i-callz-dret-6   (byte 1. 61.))
(defconstant %%i-callz-dret-5   (byte 1. 36.))
(defconstant %%i-callz-dret-5-3 (byte 3. 34.))
(defconstant %%i-callz-dret-3   (byte 1. 34.))
(defconstant %%i-callz-dret-2-0 (byte 3.  0.))

(defconstant %%i-macro-carry-bit (byte 1. 60.))

(defconstant %%i-op-code    (byte 3. 58.))
(defconstant %%i-op-code-high (byte 3. 26.))            ;from 32. bit word containing high half
(defconstant   $$i-op-code-alu      #b000)
(defconstant   $$i-op-code-alui     #b001)
(defconstant   $$i-op-code-move     #b010)
(defconstant   $$i-op-code-alu-with-link #b100)         ;for disassembler only
(defconstant   $$i-op-code-alui-with-link #b101)        ;for disassembler only
(defconstant   $$i-op-code-loadi-32 #b011)
(defconstant   $$i-op-code-fp-alu   #b110)
(defconstant   $$i-op-code-fp-mult  #b111)

(defconstant %%i-next-pc    (byte 2. 56.))
(defconstant   $$i-next-pc-ir       0.)
(defconstant   $$i-next-pc-dispatch 1.)
(defconstant   $$i-next-pc-return   2.)
(defconstant   $$i-next-pc-pc+1     3.)

(defconstant %%i-boxed      (byte 2. 54.))
(defconstant   $$i-boxed-outreg0 0.)
(defconstant   $$i-boxed-left    0.)
(defconstant   $$i-boxed-right   1.)
(defconstant   $$i-boxed-unboxed 2.)
(defconstant   $$i-boxed-boxed   3.)

(defconstant %%i-vma-boxed      (byte 1. 54.))
(defconstant   $$i-vma-unboxed  0.)
(defconstant   $$i-vma-boxed    1.)

(defconstant %%i-md-boxed       (byte 1. 55.))
(defconstant   $$i-md-unboxed  0.)
(defconstant   $$i-md-boxed    1.)


(defconstant %%i-dtp-check  (byte 3. 51.))
(defconstant %%i-dtp-check-high (byte 3. (global:- 51. 32.)))

(defconstant %%i-chop       (byte 3. 48.))
(defconstant %%i-chop-high  (byte 3. 16.))      ;from 32. bit word holding high half.
(defconstant   $$i-chop-nop        0.)
(defconstant   $$i-chop-open       1.)
(defconstant   $$i-chop-call       2.)
(defconstant   $$i-chop-open-call  3.)
(defconstant   $$i-chop-return     4.)
(defconstant   $$i-chop-topen      5.)
(defconstant   $$i-chop-tcall      6.)
(defconstant   $$i-chop-topen-call 7.)

(defconstant %%i-destination (byte 7. 41.))
(defconstant   %%i-reg-funcp  (byte 1. 6.))
(defconstant     $$i-reg-reg   0.)
(defconstant     $$i-reg-func  1.)

;;; Functional Destinations
(defconstant   %%i-reg-fdest  (byte 6. 0.))
;;; These have %%i-reg-funcp bit set
(defconstant $$i-fd-return                      #b1000000)
(defconstant $$i-fd-return-mv                   #b1000001)
(defconstant $$i-fd-return-tail                 #b1000010)

(defconstant $$i-fd-datatype-write-pulse        #b1001000)
(defconstant $$i-fd-processor-control-register  #b1001001)
(defconstant $$i-fd-call-hardware-o-a-r         #b1001010)
(defconstant $$i-fd-return-pc-return-dest       #b1001011)
(defconstant $$i-fd-call-hardware-hp-sp         #b1001100)      ;new name
(defconstant $$i-fd-call-hardware-sp-hp         #b1001100)      ;old name
(defconstant $$i-fd-nop                         #b1001111)

(defconstant $$i-fd-memory-map                  #b1100000)
(defconstant $$i-fd-gc-ram                      #b1100001)
(defconstant $$i-fd-memory-control-register     #b1100010)
(defconstant $$i-fd-microsecond-clock           #b1100011)
(defconstant $$i-fd-statistics-counter          #b1100100)
(defconstant $$i-fd-transporter-ram-write-pulse #b1100101)
(defconstant $$i-fd-bus-control-register        #b1100111)

(defconstant $$i-fd-vma                         #b1101000)
(defconstant $$i-fd-md                          #b1101010)
(defconstant $$i-fd-vma-start-write-no-gc-trap  #b1101100)
(defconstant $$i-fd-vma-start-write             #b1101101)
(defconstant $$i-fd-md-start-write-no-gc-trap   #b1101110)
(defconstant $$i-fd-md-start-write              #b1101111)

(defconstant $$i-fd-vma-start-read-no-transport           #b1110000)
(defconstant $$i-fd-vma-start-read                        #b1110001)
(defconstant $$i-fd-vma-start-read-visible-evcp           #b1110010)
(defconstant $$i-fd-vma-start-read-will-write             #b1110011)
(defconstant $$i-fd-vma-start-read-cdr-no-transport       #b1110100)
(defconstant $$i-fd-vma-start-read-cdr                    #b1110101)
(defconstant $$i-fd-vma-start-read-cdr-visible-evcp       #b1110110)
(defconstant $$i-fd-vma-start-read-cdr-will-write         #b1110111)

(defconstant $$i-fd-vma-start-read-early-no-transport     #b1111000)
(defconstant $$i-fd-vma-start-read-early                  #b1111001)
(defconstant $$i-fd-vma-start-read-early-visible-evcp     #b1111010)
(defconstant $$i-fd-vma-start-read-early-will-write       #b1111011)
(defconstant $$i-fd-vma-start-read-early-cdr-no-transport #b1111100)
(defconstant $$i-fd-vma-start-read-early-cdr              #b1111101)
(defconstant $$i-fd-vma-start-read-early-cdr-visible-evcp #b1111110)
(defconstant $$i-fd-vma-start-read-early-cdr-will-write   #b1111111)

;;; Register Destinations / Sources
(defconstant   %%i-reg-base   (byte 2. 4.))
(defconstant     $$i-reg-base-open   0.)
(defconstant     $$i-reg-base-active 1.)
(defconstant     $$i-reg-base-return 2.)
(defconstant     $$i-reg-base-global 3.)
(defconstant   %%i-reg-offset (byte 4. 0.))

(defconstant %%i-global-frame (byte 4. 37.))

(defconstant %%i-jcond       (byte 3. 34.))
(defconstant   $$i-jcond-uncond 0.)
(defconstant   $$i-jcond-indir  1.)
(defconstant   $$i-jcond-eq     2.)
(defconstant   $$i-jcond-neq    3.)
(defconstant   $$i-jcond-lt     4.)
(defconstant   $$i-jcond-ge     5.)
(defconstant   $$i-jcond-gt     6.)
(defconstant   $$i-jcond-le     7.)

;;; Byte Width field for Alu Operations
(defconstant %%i-bw           (byte 2. 32.))
(defconstant   $$i-bw-32 0.)
(defconstant   $$i-bw-8  1.)
(defconstant   $$i-bw-16 2.)
(defconstant   $$i-bw-24 3.)

;;; Position/Width selector for Alu Field Operations
(defconstant %%i-pw           (byte  2. 32.))
(defconstant   $$i-pw-ii 0.)            ;position and width both come from IR
(defconstant   $$i-pw-ir 1.)            ;position from IR, width from status register.
(defconstant   $$i-pw-ri 2.)            ;position from status register, width from IR
(defconstant   $$i-pw-rr 3.)            ;position and width both from status register.

(defconstant %%i-right-source (byte  7. 25.))
(defconstant   %%i-reg-fsource  (byte 6. 0.))
;;; Functional sources
;;; these have the %%i-reg-fsource bit set

;;; Processor Board Group                  #b100XXXX
(defconstant $$i-fs-read-cache-b-lo        #b1000100)
(defconstant $$i-fs-read-cache-a-lo        #b1000101)
(defconstant $$i-fs-read-cache-b-hi        #b1000110)
(defconstant $$i-fs-read-cache-a-hi        #b1000111)
(defconstant $$i-fs-processor-status       #b1001000)
(defconstant $$i-fs-processor-control      #b1001001)
(defconstant $$i-fs-open-active-return     #b1001010)
(defconstant $$i-fs-return-pc-return-dest  #b1001011)
(defconstant $$i-fs-hp-csp                 #b1001100)   ;new name
(defconstant $$i-fs-csp-hp                 #b1001100)   ;old name
(defconstant $$i-fs-trap-pc+               #b1001101)
(defconstant $$i-fs-trap-pc                #b1001110)
(defconstant $$i-fs-read-trap-enable-and-disable #b1000011)

;;; Memory Board Group                      #b11XXXXX)
(defconstant $$i-fs-memory-map              #b1100000)
(defconstant $$i-fs-gc-ram                  #b1100001)
(defconstant $$i-fs-memory-control-register #b1100010)
(defconstant $$i-fs-microsecond-clock       #b1100011)
(defconstant $$i-fs-statistics-counter      #b1100100)
(defconstant $$i-fs-trap-register           #b1100101)
(defconstant $$i-fs-memory-status-register  #b1100110)
(defconstant $$i-fs-bus-control-register    #b1100111)
(defconstant $$i-fs-vma                     #b1101000)
(defconstant $$i-fs-md                      #b1101010)


(defconstant %%i-imm32-data     (byte 32.  0.))
(defconstant %%i-imm16-data     (byte 16.  0.))
(defconstant %%i-jump-address   (byte 24.  0.))
(defconstant %%i-branch-address (byte 12.  0.))
(defconstant %%i-alui-op        (byte  7. 25.))
(defconstant %%i-left-source    (byte  6. 19.))
(defconstant %%i-alu-op         (byte  7. 12.))
(defconstant %%i-alu-shift      (byte  6.  5.))
(defconstant %%i-alu-mask       (byte  5.  0.))

;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-

;;; (c) Copyright 1984,1985,1986 Lisp Machines Incorporated.

;;; This file is now loaded by the cold load builder, and values on
;;; SYSCONF-CONSTANT-LISTS are built into the cold load.
;;; 21-May-86 11:04:03 -gjc


;;; Loading this with a base of other than 8 can really cause bizarre effects
;global:(unless (= *read-base* 8) (break "*READ-BASE* not octal"))

;; Copied from LAD: RELEASE-3.COLD; SYSCONF.LISP#17 on 26-Mar-87 15:59:50
(defconst system-configuration-qs '(
  %system-configuration-version-number          ;version of this whole header - currently 1
  %system-configuration-size                    ;size of this structure in 32 bit words
  %system-configuration-number-of-processors    ;number of processor configuration blocks
                                                ;after the system configuration block
  %system-configuration-processor-block-size    ;in 32 bit words
  %system-configuration-share-struct-pointer    ;what used to be stored at #xff000080
  %system-configuration-debug-level             ;old debug switch for SDU code

  %system-configuration-lock                    ;lock for hacking sys-conf struct

  ;; an "owner" is either a slot number of owner,
  ;; or -1 if not allocted, or -2 if not present on bus
  %system-configuration-ethernet-owner          ;3com ethernet
  %system-configuration-tapemaster-owner        ;half in tape
  %system-configuration-mti-8-line-owner
  %system-configuration-mti-16-line-owner
  %system-configuration-quarter-inch-tape-owner
  %system-configuration-sdu-serial-a-owner
  %system-configuration-sdu-serial-b-owner

  %system-configuration-share-tty-0             ;nubus addr of first sharetty
  %system-configuration-share-tty-1
  %system-configuration-share-tty-2
  %system-configuration-grey-owner              ;medium-color
  %system-configuration-grey-slot

  %system-configuration-number-of-share-ttys ;number of share ttys UNIX has set up
  %system-configuration-number-of-share-tty-pages ;how many pages might be share ttys

  %system-configuration-global-shared-base
  %system-configuration-global-shared-size ;in bytes

  %system-configuration-excelan-owner

  %system-configuration-excelan-2-owner
  %system-configuration-shared-excelan-pointer  ;NuBus address of shared excelan struct
  %system-configuration-excelan-2-initted
  %system-configuration-sdu-interrupt-map       ;nubus addr of intmap
  %system-configuration-tapemaster-base-multibus-map    ;base 1k map page
  %system-configuration-tapemaster-multibus-map-size    ;number of map pages
  %system-configuration-titn-owner              ;silicon-graphics TITN interface
  %system-configuration-system-console          ;slot for processor that owns SDU console dev
  %system-configuration-sdu-nubus-base          ;nubus mem for newboot to use
  %system-configuration-sdu-nubus-size
  %system-configuration-multibus-tapemaster-parameter-block     ;tape IOPB in multibus ram

  %system-configuration-excelan-base-multibus-map-block ;base 1k map page
  %system-configuration-excelan-multibus-map-size
  %system-configuration-cmos-clock-chip-owner
  %system-configuration-user-base-multibus-map  ;base 1k map page for "user reserved" dev
  %system-configuration-user-multibus-map-size

  ;; these used to be disk-drive status
  %system-configuration-second-grey-owner       ;second medium-color
  %system-configuration-second-grey-slot
  %system-configuration-default-grey-owner      ;"preferred" owners as set by config
  %system-configuration-default-second-grey-owner

  %system-configuration-flavors-bus-link-owner
  %system-configuration-flavors-bus-link-slot
  %system-configuration-second-flavors-bus-link-owner
  %system-configuration-second-flavors-bus-link-slot

  %system-configuration-newboot-version-number  ;boot tape version; MAJOR.MINOR (16/16)
  %system-configuration-sdu-rom-version-number
  %system-configuration-burr-brown-owner
  %system-configuration-second-burr-brown-owner
  %system-configuration-interphase-2181-owner

  %system-configuration-disk-unit-0-initialized ;non-zero if controller is initted for this unit
  %system-configuration-disk-unit-1-initialized
  %system-configuration-disk-unit-2-initialized
  %system-configuration-disk-unit-3-initialized
  %system-configuration-disk-unit-4-initialized
  %system-configuration-disk-unit-5-initialized
  %system-configuration-disk-unit-6-initialized
  %system-configuration-disk-unit-7-initialized

  %system-configuration-nubus-disk-owner
  %system-configuration-newboot-idle-count

  %system-configuration-chaos-sharedev-buffer-size-in-bytes

  %system-configuration-lmi-debug-board-owner
  %system-configuration-lmi-debug-board-slot
  %system-configuration-second-lmi-debug-board-owner
  %system-configuration-second-lmi-debug-board-slot

  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) system-configuration-qs)
(assign-values system-configuration-qs)

;; Copied from LAD: RELEASE-3.COLD; SYSCONF.LISP#17 on 26-Mar-87 15:59:51
(defconst processor-configuration-qs '(
  %processor-conf-sys-conf-ptr                  ;32 bit nubus address of sys conf
  %processor-conf-slot-number                   ;slot number of RG board, etc
  %processor-conf-major-version                 ;version number of processor from conf prom
  %processor-conf-minor-version
  %processor-conf-starting-processor-switches   ;for prom and ulambda - bits below

  ;; this is the actual share-iopb structure for the main user on this processor
  ;; i.e. ulambda for the lispm
  %processor-conf-share-runme                   ;set this when you want the iopb executed
  %processor-conf-share-slot                    ;slot number of processor for this share-iopb
  %processor-conf-share-type                    ;type of thing that set up the share-iopb
                                                ;should be different for each different "driver"
                                                ;   sdu = 1
                                                ;   unix = 2
                                                ;   prom = 3
                                                ;   ulambda = 4
                                                ;   lisp user level = 5

  %processor-conf-share-iopb                    ;8086 pointer to real iopb
  %processor-conf-share-interrupt-addr          ;8086 pointer to interrupt address, or 0
  %processor-conf-share-spare-1
  %processor-conf-share-spare-2
  %processor-conf-share-spare-3
  %processor-conf-share-spare-4

  %processor-conf-chaos-address                 ;chaos address of this processor

  %processor-conf-send-chaos-share-dev          ;(obsolete) for passing ethernet packets
  %processor-conf-rcv-chaos-share-dev

  %processor-conf-memory-base-0                 ;whole 32 bit nubus address
  %processor-conf-memory-base-1                 ;low bit is 1 if 1/2 meg card & you
  %processor-conf-memory-base-2                 ;want to initialize
  %processor-conf-memory-base-3
  %processor-conf-memory-base-4
  %processor-conf-memory-base-5
  %processor-conf-memory-base-6
  %processor-conf-memory-base-7
  %processor-conf-memory-base-8
  %processor-conf-memory-base-9
  %processor-conf-memory-bytes-0                ;number of bytes long - 0 is not used
  %processor-conf-memory-bytes-1
  %processor-conf-memory-bytes-2
  %processor-conf-memory-bytes-3
  %processor-conf-memory-bytes-4
  %processor-conf-memory-bytes-5
  %processor-conf-memory-bytes-6
  %processor-conf-memory-bytes-7
  %processor-conf-memory-bytes-8
  %processor-conf-memory-bytes-9

  %processor-conf-vcmem-slot                    ;4-byte struct for console / screen dev
                                                ;first byte is nubus slot number for board
                                                ;second byte is "board type"
                                                ;   0 = serial port A
                                                ;   1 = vcmem
                                                ;   2 = quad
                                                ;   3 = sharetty
                                                ;   4 = serial port B
                                                ;   5 = color hack from all four screens of quad
                                                ;   6 = serial port on quad board
                                                ;third byte is screen or port number
                                                ;fourth byte is 0 for portrait, 1 for land,
                                                ;   0xff for no device present

  %processor-conf-processor-type                ;1 lambda, 2 68000, 3 SDU, 4 Falcon

  %processor-conf-micro-band                    ;optional 4-byte ascii partition names
  %processor-conf-load-band
  %processor-conf-paging-band
  %processor-conf-file-band

  %processor-conf-base-multibus-mapping-register

  %processor-conf-boot-status
  %processor-conf-chaos-share-0                 ;nubus addrs of chaos sharedevs
  %processor-conf-chaos-share-1
  %processor-conf-chaos-share-2
  %processor-conf-chaos-share-3
  %processor-conf-chaos-share-4

  %processor-conf-parity-enables
  %processor-conf-vcmem-words-per-line          ;scan line table
  %processor-conf-number-of-multibus-maps
  %processor-conf-boot-command                  ;pending newboot command
  %processor-conf-boot-mode                     ;newboot command that booted this processor
  %processor-conf-console                       ;console type for unix, see -vcmem-slot types
  %processor-conf-console-baud-rate             ;%processor-conf-screen-number

  %processor-conf-watchdog
  %processor-conf-intmap-multibus-map

  %processor-conf-n-aux-devs                    ;number of aux-dev-#'s following
  %processor-conf-aux-dev-0                     ;optional I/O devs for this processor,
  %processor-conf-aux-dev-1                     ;structure is same as for -vcmem-slot
; %processor-conf-aux-dev-2
; %processor-conf-aux-dev-3
  %processor-conf-excelan-multibus-map-base     ;base for shared-excelan map
  %processor-conf-excelan-multibus-map-size     ;size for shared-excelan map
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) processor-configuration-qs)
(assign-values processor-configuration-qs)

(defconst processor-conf-console-types '(
                                         "SDU serial port A"
                                         "vcmem"
                                         "quad-video"
                                         "sharetty"
                                         "SDU serial port B"
                                         "quad-video four-screen-color"
                                         "quad-video serial port"))

(defconst lambda-processor-switches-bits '(
  %%processor-switch-use-stat2-for-usec-clock 3701              ;31.
  %%processor-switch-allow-boot-chars 3601                      ;30.
  %%processor-switch-use-multiplier-in-uc-tv 3501               ;29.
  %%processor-switch-use-disk-sharing-protocol 3401             ;28.
  %%processor-switch-prom-jumps-to-cold-boot 3301               ;27.
  %%processor-switch-slot-numbers-set-up 3201                   ;26.
  %%processor-switch-2x2-stuff-valid-in-conf-structure 3101     ;25.
  %%processor-switch-new-sys-conf-mapping 3001                  ;24.
  %%processor-switch-debug-illops-halt    2701                  ;23.
  %%processor-switch-chaos-ucode-enable   2601                  ;22.
  %%processor-switch-fast-boot-enable     0501                  ; 5.
  %%processor-switch-fast-cache-mode 0401                       ; 4.
  %%processor-switch-cache-permit-for-video-buffer 0301         ; 3.
  %%processor-switch-cache-permit 0201                          ; 2.
  %%processor-switch-packet-size-code 0002                      ; 0,1.
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:IF (SI:SYMBOLP x) (SI:PUTPROP x t 'SI:special))) lambda-processor-switches-bits)
(assign-alternate lambda-processor-switches-bits)

(defconst lambda-processor-switches-bits-symbols
          (si::get-alternate lambda-processor-switches-bits))

(defconst proc-conf-boot-commands
          '(%proc-conf-boot-command-herald 1
            %proc-conf-boot-command-menu 2      ;old code uses this name
            %proc-conf-boot-command-command-prompt 2
            %proc-conf-boot-command-boot 3
            %proc-conf-boot-command-warm 4
            %proc-conf-boot-command-connect 5
            %proc-conf-boot-command-halt 6
            %proc-conf-boot-command-diag 7
            %proc-conf-boot-command-continue-lambda 10.
            %proc-conf-boot-command-print-menu 13.
            %proc-conf-boot-command-remote-debug-on 14.
            %proc-conf-boot-command-remote-debug-off 15.
            ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:IF (SI:SYMBOLP x) (SI:PUTPROP x t 'SI:special))) proc-conf-boot-commands)
(assign-alternate proc-conf-boot-commands)

(defconst proc-conf-boot-commands-symbols
          (si::get-alternate proc-conf-boot-commands))


(defconst chaos-share-dev-qs '(
  %chaos-share-csr
  %chaos-share-size                             ;number of words before buffer
  %chaos-share-buf-size                         ;buf size in bytes
  %chaos-share-intr-addr                        ;32 bit nubus address
  %chaos-share-pkt-length                       ;number of bytes in packet
; Buffer follows...
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) chaos-share-dev-qs)
(assign-values chaos-share-dev-qs)

(defconst chaos-share-dev-csr-bits '(
  %%chaos-share-dev-valid-bit 0001
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:IF (SI:SYMBOLP x) (SI:PUTPROP x t 'SI:special))) chaos-share-dev-csr-bits)
(assign-alternate chaos-share-dev-csr-bits)

(defconst chaos-share-dev-csr-bits-symbols (si::get-alternate chaos-share-dev-csr-bits))

(defconst share-tty-qs '(
  %share-tty-lisp-to-unix-buffer        ;word offset from beginng of structure to xmit buffer
  %share-tty-unix-to-lisp-buffer        ;word offset from beginng of structure to rcv buffer
  %share-tty-buf-size                   ;in bytes
  %share-tty-unix-intr
  %share-tty-lam-intr
  %share-tty-lisp-to-unix-out-ptr       ;offset in bytes ...
  %share-tty-lisp-to-unix-in-ptr
  %share-tty-unix-to-lisp-out-ptr
  %share-tty-unix-to-lisp-in-ptr
  %share-tty-lcsr
  %share-tty-ucsr
  %share-tty-owner
  %share-tty-minor
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) share-tty-qs)
(assign-values share-tty-qs)

(defconst share-tty-csr-bits '(
  %%share-tty-csr-carrier 0001
  %%share-tty-csr-raw 0101
  %%share-tty-csr-opened 0201
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:IF (SI:SYMBOLP x) (SI:PUTPROP x t 'SI:special))) share-tty-csr-bits)
(assign-alternate share-tty-csr-bits)

(defconst share-tty-csr-bits-symbols (si::get-alternate share-tty-csr-bits))

(defconst share-struct-qs '(
  %share-struct-lock
  %share-struct-max-iopbs
  %share-struct-current-iopb
  %share-struct-start-of-valid-table            ;the valid table is max-iopbs long, followed
                                                ;by the iopb table which is also max-iopbs
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) share-struct-qs)
(assign-values share-struct-qs)


(defconst intmap-qs '(
  %intmap-type
  %intmap-multibus-addr
  %intmap-sdu-1
  %intmap-size-in-words
  ))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) intmap-qs)
(assign-values intmap-qs)


(defconst intmap-types '(
  %intmap-type-none 0
  %intmap-type-sdu 1
  %intmap-type-nubus 2
  ))

(SI:MAPC #'(SI:LAMBDA (x) (SI:IF (SI:SYMBOLP x) (SI:PUTPROP x t 'SI:special))) intmap-types)
(assign-alternate intmap-types)

(defconst intmap-types-symbols (si::get-alternate intmap-types))


(defconst sdu-interrupt-numbers
          '(%sdu-div-0
             %sdu-trace
             %sdu-nmi
             %sdu-int3
             %sdu-overflow
             %sdu-int5
             %sdu-int6
             %sdu-int7
             %sdu-multibus-timeout
             %sdu-nubus-timeout
             %sdu-quart-exception
             %sdu-quart-ready
             %sdu-power-fail
             %sdu-8087
             %sdu-PIC-2
             %sdu-PIC-1
             %sdu-port-A-rcv
             %sdu-port-A-xmit
             %sdu-port-B-rcv
             %sdu-port-B-xmit
             %sdu-PIT0-unix-clock
             %sdu-PIT1
             %sdu-PIT2-sdu-clock
             %sdu-unused27
             %sdu-m0-3com
             %sdu-m1-iomsg
             %sdu-m2-tapemaster
             %sdu-m3
             %sdu-m4-disk
             %sdu-m5
             %sdu-m6-MTI
             %sdu-m7-share))
(SI:MAPC #'(SI:LAMBDA (x) (SI:PUTPROP x t 'SI:system-constant)) sdu-interrupt-numbers)
(assign-values sdu-interrupt-numbers)

(defconst sysconf-constant-lists
          '(
            system-configuration-qs
            processor-configuration-qs
            lambda-processor-switches-bits-symbols
            proc-conf-boot-commands-symbols
            chaos-share-dev-qs
            chaos-share-dev-csr-bits-symbols
            share-tty-qs
            share-tty-csr-bits-symbols
            share-struct-qs
            intmap-qs
            intmap-types-symbols
            sdu-interrupt-numbers))

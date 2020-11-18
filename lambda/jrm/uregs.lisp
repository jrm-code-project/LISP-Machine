;;; -*- Mode:LISP; Package:LAMBDA; Base:8 -*-

;;; How the registers are configured.

(defconstant *number-of-registers* 10000)
(defconstant *m-mem-low-bound* 0)
(defconstant *m-mem-high-bound* 77)
(defconstant *registers-used-for-M-constants* '(76 77)) ;;Magic numbers!
(defconstant *constant-registers*
       '(a-zero a-minus-one a-v-nil a-v-true a-floating-zero
         a-v-resident-symbol-area a-v-system-communication-area a-v-scratch-pad-init-area
         a-v-micro-code-symbol-area a-v-region-origin a-v-region-length a-v-region-bits
         a-v-region-free-pointer a-v-wired-disk-buffer a-v-page-table-area a-v-physical-page-data
         a-v-address-space-map a-v-virtual-page-volatility a-v-region-maximum-virtual-page-volatility
         a-v-region-gc-pointer a-v-region-list-thread
         a-v-region-allocation-status a-v-region-area-map
         a-v-area-name a-v-area-region-list
         a-v-area-region-bits a-v-area-region-size
         a-v-support-entry-vector a-v-constants-area a-v-extra-pdl-area
         a-v-micro-code-entry-area a-v-micro-code-entry-name-area
         a-v-micro-code-entry-args-info-area a-v-micro-code-entry-max-pdl-usage
         a-v-micro-code-paging-area a-v-virtual-page-data a-v-scavenge-queue
         a-v-micro-code-entry-arglist-area
         a-v-micro-code-symbol-name-area
         a-v-init-list-area a-v-first-unfixed-area a-v-misc-base
         a-pmh-0 a-pmh-1 a-pmh-2 a-pmh-3 a-pmh-4 a-pmh-5
         a-pmh-6 a-pmh-7 a-pmh-8 a-pmh-9
         a-pmo-0 a-pmo-1 a-pmo-2 a-pmo-3 a-pmo-4 a-pmo-5
         a-pmo-6 a-pmo-7 a-pmo-8 a-pmo-9
         a-map-scratch-block
         a-rg-quad-slot a-sdu-quad-slot a-tv-quad-slot a-grey-quad-slot
         a-processor-switches
         a-disk-run-light a-loaded-band a-loaded-ucode
         a-disk-blocks-per-track a-disk-blocks-per-cylinder
         a-background-cons-area
         a-array-index-order a-processor-type-code
         a-pht-index-mask a-pht-index-limit
         a-l2-map-control-bits
         a-proc-conf-local-phys-adr
         a-proc-conf-virtual-adr a-sys-conf-base-phys-page
         a-my-iopb-valid-flag-physical-adr a-disk-page-partition-name
         a-disk-band-partition-name a-disk-ucode-partition-name
         a-disk-ucode-partition-start a-disk-ucode-partition-length
         ))

(defconstant *global-registers*
             '(a-garbage
               a-ap
               a-ipmark
               a-error-substatus
               a-flags
               a-defer-boot-char-mode a-last-l2-map-control a-last-l2-map-physical-page
               a-first-level-map-reloads a-second-level-map-reuse-pointer
               a-second-level-map-reuse-pointer-init
               a-pgf-vma
               a-lowest-direct-virtual-address
               a-counter-block-base
               a-second-level-map-reloads
               a-pdl-buffer-read-faults a-pdl-buffer-write-faults a-pdl-buffer-memory-faults
               a-disk-prepage-used-count
               a-mar-low a-mar-high
               a-pdl-buffer-virtual-address a-pdl-buffer-head
               a-video-buffer-base-phys-page
               )
  "These registers are okay to source and clobber at random.")

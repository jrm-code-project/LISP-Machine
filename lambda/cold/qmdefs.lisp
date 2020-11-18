;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-

;;; definitions for QUANTUM-MAP stuff:

;;; allocations in the wired system tables area (which, out of randomness is named quantum-map)
;;; sizes are in pages
(defconst %quantum-map-offset-in-tables 0)              ; 32 pages
(defconst %partition-table-offset-in-tables 32.)        ;  1 page
(defconst %quantum-swap-buffer-offset-in-tables 33.)    ; 65 pages
    ;;; 24 pages free at 104

;;; We need the quantum map before its area actually exists.  It is temporarily put
;;; at this location, which is at the very end of the direct mapped bootstrap memory
(defconst cold-wired-system-tables-base (* 800. 400))   ;last 100. pages of direct mapped memory

;;; fields of quantum map
(defconst %%pq1-quantum-is-valid (byte 1 31.))
(defconst %%pq1-quantum-is-device (byte 1 30.))

(defconst %%pq1d-quantum-is-special-a-memory (byte 1 29.))
(defconst %%pq1d-page-fault-dispatch-field (byte 3 29.))
(defconst %%pq1d-quantum-nubus-words (byte 15. 0))      ;only 2^14 words in a quantum
(defconst %%pq2d-quantum-l2mc-except-meta-bits (byte 8. 24.))   ;this and next are what
(defconst %%pq2d-quantum-l2mpp (byte 24. 0))            ;to write in L2 map

(defconst %%pq1m-page-out-copy-first (byte 1 29.))
(defconst %%pq1m-disk-swap-dispatch-field (byte 3 29.))
(defconst %%pq1m-page-offset (byte 20. 0))              ;field could occupy whole word but
(defconst %%pq1m-all-but-page-offset (byte 12. 20.))
(defconst %%pq2m-boot-pages-allocated (byte 7 8.))      ;pages quantum occupies in LOD band
(defconst %%pq2m-partition-number (byte 8. 0))          ;index into partition table.  note:
                                                        ;code assumes this starts at bit 0.

;;; other definitions used by quantum map code
;(defconst virtual-address-quantum-number (byte 11. 14.))       ;bits of an address which say which quantum
;;; use VMA-QUANTUM-BYTE instead.
;;; %address-space-quantum-size
(defconst %%virtual-address-offset-in-quantum (byte 14. 0))     ;how many words into the quantum
(defconst %%l2-map-control-all-but-meta-bits (byte 10. 6))
(defconst %%virtual-page-quantum-number (byte 11. 6))
(defconst %number-of-address-quanta (ash 1 12.))        ; 11 bits, 14-24

;;; fields in the partition table
(defconst %%pt1-valid (byte 1 31.))                             ;this entry is valid
(defconst %%pt1-unit-number (byte 3 28.))
;;; eventually will need a bit which tells ALLOCATE-PAGE-SPACE-FOR-QUANTUM that this is a
;;; partition that it is allowed to use.
(defconst %%pt1-paging-enable (byte 1 27.))     ;and here it is
(defconst %%pt1-size (byte 27. 0))
(defconst %%pt2-offset (byte 31. 0))                            ;the whole word for now

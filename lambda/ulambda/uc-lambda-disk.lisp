;-*- mode: lisp; base: 8; readtable: ZL -*-
;       ** (c) Copyright 1983 Lisp Machine Inc **

(DEFCONST UC-LAMBDA-DISK '(

(locality a-mem)
a-active-iopb (0)
(locality i-mem)

;Routines to be reimplemented for each disk controller:
;
; start-disk-op (and start-disk-op-1, start-disk-op-2)
; await-disk
; disk-completion-get-status

;;; some things that are used for the quantum map fast-boot hack:

(locality a-mem)
a-quantum-map-index             (0)
a-disk-swap-saved-ccw-pointer   (0)             ;also used as a temporary storage place during initialization
a-page-partition-to-use         (1)             ;index in partition table
a-new-home-on-disk              (0)
a-disk-swap-saved-m-c           (0)
a-disk-swap-saved-m-a           (0)
a-saved-disk-transfer-size      (0)
a-page-allocation-free-pointer  (0)             ;allocation pointer for page band
                                                ;must be set after cold-swap-in
(locality i-mem)

(locality d-mem)
(start-dispatch 3)
;;; used in DISK-SWAP-PAGIFY-QUANTUM.
d-swap-quantum-map-dispatch
        (n-bit allocate-new-quantum)    ;000 quantum not valid  allocate quantum and back to DISK-SWAP-HANDLER
;       (n-bit p-bit illop)             ;000 quantum not valid
        (n-bit p-bit illop)             ;001 quantum not valid
        (n-bit p-bit illop)             ;010 quantum not valid
        (n-bit p-bit illop)             ;011 quantum not valid
        (n-bit r-bit)                   ;100 normal thing, nothing special      return to DISK-SWAP-HANDLER
        (p-bit r-bit)                   ;101 must copy if a write operation     fall through
        (n-bit p-bit illop)             ;110 map device
        (n-bit p-bit illop)             ;111 special a memory
(end-dispatch)
(locality i-mem)

;;; setup A-DISK-PAGE-UNIT A-DISK-OFFSET and A-DISK-MAXIMUM for index in partition table
;;; enter with the second word of a physical quantum map entry in md.
;;; clobbers VMA, MD and above disk paging variables
lookup-in-partition-table
        ((vma) ldb (lisp-byte %%pq2m-partition-number) md a-zero)
        ((vma) dpb vma (byte-field 31. 1) a-zero)       ;2 words per entry
        ((vma) add vma (a-constant (eval (* page-size %partition-table-offset-in-tables))))
        ((vma-start-read) add vma a-v-quantum-map)      ;look up in partition table
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ;;; pt1 is now in md
        ((a-disk-page-unit) ldb (lisp-byte %%pt1-unit-number) md a-zero)
        ((a-disk-maximum) ldb (lisp-byte %%pt1-size) md a-zero)
        ((vma-start-read) m+a+1 vma a-zero)     ;get pt2 word
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (popj-after-next (a-disk-offset) ldb (lisp-byte %%pt2-offset) md a-zero)
        (no-op)

;;; determine offset in partition described by A-PAGE-PARTITION-TO-USE and return
;;; in M-TEM.  This is allowed to change A-PAGE-PARTITION-TO-USE if it runs
;;; out of space in it.  This routine will eventually allocate page partition space
;;; as needed, but for now just uses the virtual address to compute the index in a single
;;; page partition which is the image of virtual memory.  For this case it is allowed to
;;; use the virtual address in a-disk-swap-saved-m-a.
;(begin-comment)
allocate-page-space-for-quantum
        (popj-after-next (m-tem) dpb m-zero (lisp-byte %%virtual-address-offset-in-quantum) a-disk-swap-saved-m-a)
                ;point to base of quantum
       ((m-tem) ldb vma-page-addr-part m-tem a-zero)    ;turn it into a page number
;(end-comment)

(begin-comment)
allocate-page-space-for-quantum
        (call-xct-next lookup-in-partition-table)       ;set up parameters for current page partition
       ((md) a-page-partition-to-use)
        ((m-tem) a-page-allocation-free-pointer)
        ((m-tem) add m-tem (a-constant (eval (// %address-space-quantum-size page-size))))
        (call-greater-or-equal m-tem a-disk-maximum allocate-new-page-band)
        (popj-after-next (a-page-allocation-free-pointer) m-tem)
       ((m-tem) sub m-tem (a-constant (eval (// %address-space-quantum-size page-size))))
allocate-new-page-band
        (no-op halt-cons)
        ((vma) a-page-partition-to-use)
        ((vma) add vma (a-constant 1))          ;next table entry
        ((vma) dpb vma (byte-field 31. 1) a-zero)       ;two words per partition table entry
        ((vma) add vma a-v-quantum-map)
        ((vma-start-read) add vma (a-constant (eval (* page-size %partition-table-offset-in-tables))))
       (illop-if-page-fault)

        (popj-after-next (a-page-allocation-free-pointer) m-zero)
       ((m-tem) (a-constant (eval (// %address-space-quantum-size page-size))))
        ((m-tem) ldb vma-page-addr-part m-tem a-zero)   ;turn it into a page number
(end-comment)

;;; enter with a virtual address in m-a.  Make sure the quantum in which that virtual address
;;; resides is in the page partition.
;;; may not clobber m-a or m-c.  Should leave with vma and md pointing
;;; to the the first word of the quantum map entry to be copied to page.
;;; m-t gets value of a-disk-write-command on return
;;; This routine exists for the convenience of DISK-SWAP-HANDLER.
disk-swap-pagify-quantum
        ; look up in quantum map and do right thing
        ((vma) ldb vma-quantum-byte m-a a-zero)
        ((vma) dpb vma (byte-field 31. 1) a-zero)       ;we will need this later
        ((vma) add vma (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((vma-start-read) add vma a-v-quantum-map)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((a-quantum-map-index) vma)             ;save quantum map index for later
        ;;; pq1 now in md
        (dispatch (lisp-byte %%pq1m-disk-swap-dispatch-field) md d-swap-quantum-map-dispatch)
     (error-table crash not a memory quantum)
        ;;; if we are here then page-out-copy-first is set
        (popj-not-equal m-t (A-CONSTANT DISK-WRITE-COMMAND))

        ((m-1) ldb (lisp-byte %%pq1m-page-offset) md a-zero) ;get offset in partition for
                                                             ; this quantum
        ((a-disk-swap-saved-m-c) m-c)
        ((a-disk-swap-saved-m-a) m-a)
        ((m-c) a-v-quantum-map) ; ccw list is at beginning of buffer and has entries for 64 pages.
        ((m-c) add m-c (a-constant (eval (* page-size %quantum-swap-buffer-offset-in-tables))))
        ((m-c) add m-c (a-constant 64.))        ; subtract transfer size from this

        ;;; page is going out to disk.  We must copy the entire quantum (or as much
        ;;; of it as exists) from the LOD band to the PAGE band.

        ((m-t) (A-CONSTANT DISK-READ-COMMAND))  ;first read from LOD
        ;;; with pq1 word in md, look up the disk partition info

        ((vma-start-read) m+a+1 m-zero a-quantum-map-index)     ; get pq2 word
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((a-disk-transfer-size) ldb
                (lisp-byte %%pq2m-boot-pages-allocated) md a-zero)      ;better not be zero!!!
        ((m-c) sub m-c a-disk-transfer-size)
        ((a-disk-swap-saved-ccw-pointer) m-c)           ; gets bashed by START-DISK-SWAP.
        ;;; we now have the CCW list set up for both the read and the write

        (call lookup-in-partition-table)

        ((q-r) add m-1 a-disk-transfer-size)
        (call-greater-or-equal q-r a-disk-maximum illop)        ;this test can be removed when
     (error-table crash outside of band) ;we have run-time allocation of page space.
                                         ; ** should add transfer size
        ((m-1) add m-1 a-disk-offset)
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8. 24.) a-1)
        (call START-DISK-SWAP)

        (call allocate-page-space-for-quantum);setup m-a with page offset in PAGE for this quantum
        ((m-a) m-tem)
        ((m-1) a-page-partition-to-use)
        ((md) dpb m-1 (lisp-byte %%pq2m-partition-number) a-zero) ;fake arg to look like quantum
                                                                  ; map entry
        (call lookup-in-partition-table)        ; m-a now contains page offset in paging partition
        ((q-r) add m-a a-disk-transfer-size)
        (call-greater-or-equal q-r a-disk-maximum illop)        ;this test can be removed when
     (error-table crash outside of band) ;we have run-time allocation of page space.
                                         ; ** should add transfer size
        ((m-1) add m-a a-disk-offset)
        ((a-new-home-on-disk) m-a)
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8. 24.) a-1)
        ((m-t) (A-CONSTANT DISK-WRITE-COMMAND))
        ((m-c) a-disk-swap-saved-ccw-pointer)   ;restore what was destroyed by START-DISK-SWAP
        (call START-DISK-SWAP)

        ;;; now fix the quantum map entry to point to the PAGE partition.
        ((vma-start-read) a-quantum-map-index)  ;get pq1 word
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((m-1) md)
        ((m-tem) a-new-home-on-disk)
        ((m-1) dpb m-tem (lisp-byte %%pq1m-page-offset) a-1)
        ((md-start-write) dpb
                (lisp-byte %%pq1m-page-out-copy-first) m-zero a-1)  ;clear the magic bit
        (illop-if-page-fault)
     (error-table crash page fault in wired area)

        ((vma-start-read) m+a+1 vma a-zero)     ;now fix pq2 word
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((m-1) a-page-partition-to-use)
        ((m-2) md)
        ((md-start-write) dpb m-1 (lisp-byte %%pq2m-partition-number) a-2)
        ;;; write new partition in quantum map
        ;;; boot-pages-allocated no longer matters so we won't bother with it
        (illop-if-page-fault)
     (error-table crash page fault in wired area)

        ;;; now put things back in the state that GO-AHEAD-SWAP-FOR-QUANTUM wants
        ((vma-start-read) a-quantum-map-index)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((m-c) a-disk-swap-saved-m-c)
        (popj-after-next (m-a) a-disk-swap-saved-m-a)
       ((m-t) (a-constant disk-write-command))  ;thats what it was when we started this mess

;;; here if the quantum map entry is zero.  If a read operation then ILLOP
;;; Otherwise allocate page space for the quantum, set up the map entry and POPJ.
;;; A-QUANTUM-MAP-INDEX already set up.
allocate-new-quantum
;       (call-not-equal m-t (a-constant disk-write-command) illop)
;it turns out the the page is swapped in even though there is no data on it.
;hopefully this is fixed
;     (error-table crash read quantum not allocated)

        ((a-disk-swap-saved-m-a) m-a)   ;the stupid version of the page space allocator needs this
        (call allocate-page-space-for-quantum)
        ;;; m-tem now contains location in PAGE for this quantum

        ((md) a-page-partition-to-use)          ;assumes %%pq2m-partition-number is at lsb of word
        ((vma-start-write) m+a+1 m-zero a-quantum-map-index)    ;writing second word first
                                                ; saves us a read at the end
        (illop-if-page-fault)                   ;since we must leave with VMA and MD at pq1 entry
     (error-table crash page fault in wired area)

        ((md) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-tem) ;quantum valid,
                                                                        ; memory, don't copy
        ((vma-start-write) a-quantum-map-index)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (popj)
        ((md) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-a)
        ((vma-start-write) a-quantum-map-index)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)

        (popj-after-next (m-a) a-disk-swap-saved-m-a)
       (no-op)
;(end-comment)

;;; Here to perform a disk swapping operation.
;;; M-A has the virtual memory address, M-T has the command.
;;; M-B is no longer an argument at this level.
;;; The CCW is already set up starting at location in M-C.  M-C, M-T bashed.
;;; Returns with operation successfully completed.

;m-tem4 has transfer size in pages. If fast cache swapin on, this is used by hexadec
;ager.
DISK-SWAP-HANDLER
        (CALL-XCT-NEXT DISK-PGF-SAVE)
       ((A-DISK-IDLE-TIME) M-ZERO)              ;I use the disk

        ((m-1) a-processor-switches)
        (jump-if-bit-set (lisp-byte %%processor-switch-fast-boot-enable)
                         m-1 disk-swap-handler-consider-quantum)

        ((M-1) VMA-PAGE-ADDR-PART M-A)          ;Convert virtual address to disk address
        (CALL-GREATER-OR-EQUAL M-1 A-DISK-MAXIMUM ILLOP)        ;Address out of bounds
        ((M-1) ADD M-1 A-DISK-OFFSET)           ;Relocate to appropriate part of disk
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((a-disk-transfer-size) m-tem4)
        (jump disk-swap-handler-start-disk-swap)

disk-swap-handler-consider-quantum
        ((a-saved-disk-transfer-size) m-tem4)
        ((m-tem) ldb vma-page-addr-part m-a a-zero)
        ((m-tem) add m-tem a-tem4)
        ((m-tem) m-a-1 m-tem a-zero)            ;current page counts as one of the pages to swap
        ((m-tem) ldb (lisp-byte %%virtual-page-quantum-number) m-tem a-zero)
        ((m-tem4) ldb vma-quantum-byte m-a a-zero)
        (call-not-equal m-tem a-tem4 illop)
     (error-table crash "disk swap passes quantum boundary")
        (call disk-swap-pagify-quantum)
go-ahead-swap-for-quantum                       ; the simple case, here with pq1- in md.
        ((m-1) ldb (lisp-byte %%virtual-address-offset-in-quantum) m-a)
        ((m-1) ldb vma-page-addr-part m-1)
        ((m-tem) ldb (lisp-byte %%pq1m-page-offset) md a-zero)  ;offset of quantum from partition base
        ((m-1) add m-1 a-tem)
        ((vma-start-read) m+a+1 vma a-zero)
        (illop-if-page-fault)
     (error-table crash "page fault in wired area")
        (call lookup-in-partition-table)
        ((m-1) add m-1 a-disk-offset)
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8. 24.) a-1)
        ((a-disk-transfer-size) a-saved-disk-transfer-size)

disk-swap-handler-start-disk-swap
        (CALL START-DISK-SWAP)                  ;Start the disk operation
     ;; Turn run light off, saving old value on stack.
        ((vma-start-read m-t) add vma (a-constant 2))
        (check-page-read-map-reload-only)
        ((pdl-push) md)
        ((md-start-write) m-zero)
        (check-page-write-map-reload-only)
        (CALL READ-MICROSECOND-CLOCK)           ;Read microsecond clock into M-2
#-lambda(begin-comment)
        ((M-TEM) A-PROCESSOR-SWITCHES)
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 4) M-TEM DS-HEXA-AGER)
#-lambda(end-comment)
        ((M-1) A-AGING-SCAN-POINTER)            ;Run Ager while in disk wait
        (CALL-NOT-EQUAL M-1 A-FINDCORE-SCAN-POINTER AGER)
#+lambda ds-hexa-ager-return
     ;; Scanning pages on swapout to control volatilities, see comment in UC-SCAVENGER.
        ((q-r) a-disk-read-write)
       (call-equal q-r (a-constant disk-write-command) volatility-scan-pages)
        (CALL AWAIT-DISK)                       ;Now wait for operation to complete
        (CALL-XCT-NEXT READ-MICROSECOND-CLOCK)  ;Get current time
       ((M-1) M-2)                              ;but save old time
        ((M-2) SUB M-2 A-1)                     ;Get delta time
        ((A-DISK-WAIT-TIME) ADD M-2 A-DISK-WAIT-TIME)   ;Increment wait time counter
     ;; Restore run light to previous value.
        ((md) pdl-pop)
        ((vma-start-write) m-t)
        (check-page-write-map-reload-only)
        (JUMP DISK-PGF-RESTORE)

#-lambda(begin-comment)
ds-hexa-ager
        ((m-5) ldb (byte-field 4 8) m-a)
ds-ha-0 (call load-scan-for-hexadec)
        ((m-1) a-aging-scan-pointer)
        (call-not-equal m-1 a-findcore-scan-pointer ager)
        (call store-scan-for-hexadec)
        ((m-tem4) sub m-tem4 (a-constant 1))
        (jump-equal m-tem4 a-zero ds-hexa-ager-return)
        ((m-5) add m-5 (a-constant 1))
        (jump-xct-next ds-ha-0)
       ((m-5) ldb (byte-field 4 0) m-5)
#-lambda(end-comment)

volatility-scan-pages
        ((pdl-push) m-a)
        ((pdl-push) a-disk-transfer-size)
     ;; Round down to bottom of page.
        ((m-a) dpb m-zero (byte 8 0) a-a)
volatility-scan-loop
        (jump-equal m-zero a-disk-transfer-size volatility-scan-done)
        (call-xct-next volatility-scan-page)
       ((vma) m-a)
        ((m-a) add m-a (a-constant 400))
        ((a-disk-transfer-size) add m-minus-one a-disk-transfer-size)
        (jump volatility-scan-loop)
volatility-scan-done
        (popj-after-next
          (a-disk-transfer-size) pdl-pop)
       ((m-a) pdl-pop)


;;; Here to start a disk operation, first waiting for the disk to become idle.
;;; M-1 has the disk address, M-T has the command.
;;; The CLP is already built and is in M-C.  M-T, M-C, M-1, M-2 bashed.
;;; Multiple pages can be transfered to consecutive pages on the disk, as
;;; per the CCW list.  Returns with A-DISK-RUN-LIGHT in VMA.
START-DISK-SWAP
        (CALL AWAIT-DISK)                       ;Wait until disk is idle
        ((A-DISK-READ-WRITE) M-T)               ;Then store parameters into A-memory
        ((A-DISK-CLP) M-C)
        ((A-DISK-RETRY-STATE) M-ZERO)

        (call convert-m-1-to-a-disk-address)

        (JUMP-XCT-NEXT START-DISK-OP)
       ((A-DISK-RESERVED-FOR-USER) (A-CONSTANT 0))      ;Not any more, it isn't!

;;; Here to start a disk operation, first waiting for the disk to become idle.
;;; M-1 has the disk address, M-B has the page frame number of the first main
;;; memory page to transfer (if sign bit set, M-B has NUBUS physical page).
;;;  this is only used when flushing the METER buffer
;;; M-T has the command.
;;; The CLP is always 777 .  M-T, M-C, M-1, M-2 bashed.
;;; Returns with A-DISK-RUN-LIGHT in VMA.
START-DISK-1-PAGE
        ((M-2) (A-CONSTANT 1))                  ;Transfer just one page
        ((M-C) (A-CONSTANT #+lambda 777  #+exp 776))            ;CLP is always 777
;;; M-1 starting disk address, M-B starting main memory page frame number (if sign set,
;;; as above, but if more than one page, it will be consecutive PHYSICAL NUBUS pages).
;;; M-2 number of pages to transfer, M-T command, M-C address of CCW list.
;;; Bashes M-T, M-1, M-2.  Returns with A-DISK-RUN-LIGHT in VMA.
START-DISK-N-PAGES
        (CALL AWAIT-DISK)                       ;Wait until disk is idle
        ((A-DISK-READ-WRITE) M-T)               ;Then store parameters into A-memory
        ((A-DISK-CLP) M-C)
        ((A-DISK-RETRY-STATE) M-ZERO)
        ((a-disk-transfer-size) m-2)
        ((M-T) M-2)
        (call convert-m-1-to-a-disk-address)
        ;; Now build the CCW list
#-lambda(begin-comment)
        ((VMA) ADD (M-CONSTANT -1) A-DISK-CLP)
        ((MD) DPB M-B VMA-PHYS-PAGE-ADDR-PART (A-CONSTANT 1))
        (jump-if-bit-clear (byte-field 1 31.) m-b build-ccw-list-1)
        ((md) dpb m-b (byte-field 22. 8) (a-constant (plus (byte-mask (byte-field 1 31.))
                                                           1))) ;NUBUS physical page to
                                                                ;NUBUS word address.
BUILD-CCW-LIST-1
        (JUMP-GREATER-THAN M-T (A-CONSTANT 1) BUILD-CCW-LIST-2)
        ((MD) SUB MD (A-CONSTANT 1))    ;last
BUILD-CCW-LIST-2
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((M-T) SUB M-T (A-CONSTANT 1))
        (JUMP-GREATER-THAN-XCT-NEXT M-T (A-CONSTANT 0) BUILD-CCW-LIST-1)
       ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))
#-lambda(end-comment)

#-exp(begin-comment)
        ((vma) a-disk-clp)

        (jump-if-bit-set (byte-field 1 31.) m-b exp-build-physical-ccw-list)

        ((m-tem1) dpb m-b vma-phys-page-addr-part a-zero) ;convert page frame to cadr-physical
build-ccw-list-1
        ((m-tem2) m-t)
        ((m-t) m-tem1)
        (call translate-cadr-physical-to-nubus)
        ((m-t) m-tem2)
        ((md-start-write) m-lam)
        (illop-if-page-fault)
        ((md) (a-constant 1024.))
        ((vma-start-write) add vma (a-constant 1))
        (illop-if-page-fault)
        ((vma) add vma (a-constant 1))
        ((m-t) sub m-t (a-constant 1))
        ((m-tem1) add m-tem1 (a-constant (eval page-size)))
        (jump-greater-than m-t a-zero build-ccw-list-1)
        (jump ccw-list-done)

exp-build-physical-ccw-list

        ((m-tem1) dpb m-b (byte-field 22. 10.) a-zero) ;nubus page to nubus address
build-phys-ccw-list-1
        ((md-start-write) m-tem1)
        (illop-if-page-fault)
        ((md) (a-constant 1024.))
        ((vma-start-write) add vma (a-constant 1))
        (illop-if-page-fault)
        ((vma) add vma (a-constant 1))
        ((m-t) sub m-t (a-constant 1))
        ((m-tem1) add m-tem1 (a-constant 1024.))
        (jump-greater-than m-t a-zero build-phys-ccw-list-1)

ccw-list-done
#-exp(end-comment)
        ((A-DISK-RESERVED-FOR-USER) (A-CONSTANT 0))     ;Not any more, it isn't!
        (jump start-disk-op)

DISK-COMPLETION-MAIN-PROGRAM  ;simulate interrupt level re register saving, then call
                              ; disk-completion
        ((A-INTR-VMA) VMA)
        ((A-INTR-MD) MD)
        ((A-INTR-A) M-A)
        ((A-INTR-B) M-B)
        ((A-INTR-T) M-T)
        (CALL DISK-COMPLETION)
        ((MD) A-INTR-MD)
        ((VMA) A-INTR-VMA)
        ((M-T) A-INTR-T)
        (POPJ-AFTER-NEXT (M-B) A-INTR-B)
       ((M-A) A-INTR-A)

;;; Disk completion handler - called from XBUS interrupt handler, status in MD on CADR only.
;;; The following registers may be clobbered
;;; M-A, M-B, M-T
;;; M-TEM, A-TEM1, A-TEM2, A-TEM3
;;; DISPATCH-CONSTANT, Q-R, VMA, MD

DISK-COMPLETION
        (CALL DISK-COMPLETION-GET-STATUS)               ;return status in M-A
        (JUMP-NOT-EQUAL M-ZERO A-DISK-DOING-READ-COMPARE DISK-COMPLETION-READ-COMPARE-OVER)
        (JUMP-NOT-EQUAL M-A A-ZERO DISK-COMPLETION-ERROR)
        ;set up to go back to start-disk-op-1 if you want to do read compare's...
DISK-COMPLETION-OK      ;; Here when a disk operation has successfully completed
        ((MD) M-ZERO)   ;Turn off disk run light
        ((VMA-START-WRITE) A-DISK-RUN-LIGHT)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((A-DISK-BUSY) M-ZERO)
        (POPJ)
;       (POPJ-AFTER-NEXT (VMA-START-WRITE) A-DISK-REGS-BASE) ;Clear interrupt enable ***
;      (CHECK-PAGE-WRITE-NO-INTERRUPT)

DISK-COMPLETION-READ-COMPARE-OVER
        (CALL ILLOP)


DISK-COMPLETION-ERROR
        ((A-DISK-ERROR-COUNT) M+A+1 M-ZERO A-DISK-ERROR-COUNT)
        (CALL LOG-DISK-ERROR)
 (call illop) ;fix use of a-disk-transfer-size before removing this

        (JUMP START-DISK-OP-1)

;;; Log a disk error for later analysis by macrocode or console program
LOG-DISK-ERROR
        (declare (clobbers a-tem))
        ((M-TEM) A-DISK-CLP)
        ((MD) DPB M-TEM (BYTE-FIELD 20 20) A-DISK-COMMAND)
        ((VMA-START-WRITE) A-DISK-ERROR-LOG-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((MD) A-DISK-FINAL-ADDRESS)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((MD) A-DISK-STATUS)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((MD) A-DISK-MA)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        (POPJ-LESS-THAN-XCT-NEXT VMA (A-CONSTANT 637))
       ((A-DISK-ERROR-LOG-POINTER) ADD VMA (A-CONSTANT 1))
        (POPJ-AFTER-NEXT (A-DISK-ERROR-LOG-POINTER) (A-CONSTANT 600))
       (NO-OP)

;;; Support for "user" disk I/O
;;; Note that the interrupt-enable bit in the command word controls
;;; whether or not system error recovery features are invoked.

#-lambda(begin-comment)
XDSKOP (MISC-INST-ENTRY %DISK-OP)

#+exp (call illop) ;set up a-disk-transfer-size before removing this

                ;Get disk-rq array, which must be temp-wired
#+exp   ((vma) pdl-top)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA
                   (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE PDL-TOP ARRAY-HEADER-SETUP-DISPATCH)
        ((m-a) invalidate-array-cache c-pdl-buffer-pointer-pop)

        (call store-array-registers-in-accumulators)
        ;; For now, no queueing, just perform request immediately
        ((A-DISK-IDLE-TIME) M-ZERO)             ;I use the disk
        ((A-DISK-RESERVED-FOR-USER) SETO)       ;I want the disk
        (CALL AWAIT-DISK)                       ;Wait for disk control to become available
        ((VMA-START-READ) ADD M-E (A-CONSTANT (EVAL (// %DISK-RQ-COMMAND 2))))
        (CHECK-PAGE-READ)                       ;Copy user's commands into A-memory
        ((A-DISK-COMMAND) READ-MEMORY-DATA)
        ((A-DISK-READ-WRITE) (BYTE-FIELD 4 0) READ-MEMORY-DATA) ;For error recovery
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;CLP
        (CHECK-PAGE-READ)
        ((A-DISK-CLP) READ-MEMORY-DATA)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;Address
        (CHECK-PAGE-READ)
        ((A-DISK-ADDRESS) READ-MEMORY-DATA)

#-lambda(begin-comment)
;offset only unit 0, and then only if the highest bit of the cylinder address is 0
        ((m-1) ldb da-cylinder read-memory-data)
        ((m-j) ldb da-unit read-memory-data)
        (jump-not-equal m-j a-zero xdskop-no-offset)
        (jump-if-bit-set da-cylinder-highest-bit m-1 xdskop-no-offset)
        ((m-1) add m-1 a-disk-cylinder-offset)
xdskop-no-offset
        ((m-1) dpb m-zero da-cylinder-highest-bit a-1)
        ((a-disk-address) dpb m-1 da-cylinder a-disk-address)
#-lambda(end-comment)

        ((A-DISK-RETRY-STATE) M-ZERO)
        ((A-DISK-DOING-READ-COMPARE) M-ZERO)
        (CALL-XCT-NEXT START-DISK-OP-2)         ;Fire it up
       ((M-J) A-DISK-COMMAND)

        ;WAIT FOR OPERATION TO COMPLETE
        ((a-active-iopb) a-iopb-base)
        (call await-disk-hardware)
        (CALL-IF-BIT-SET (BYTE-FIELD 1 11.) M-J AWAIT-DISK)     ;Await retry if enabled
        (CALL-IF-BIT-CLEAR (BYTE-FIELD 1 11.) M-J DISK-COMPLETION-GET-STATUS) ;Get status
                                        ;into A-MEM if havent already.
        (CALL-IF-BIT-CLEAR (BYTE-FIELD 1 11.) M-J DISK-COMPLETION-OK)   ;and finish up if nec.
        ;Return status from A-memory.  Unfortunately not quite the same as at LOG-DISK-ERROR.
        ((VMA) ADD M-E (A-CONSTANT (EVAL (// %DISK-RQ-STATUS-LOW 2))))
        ((MD-START-WRITE) A-DISK-STATUS)
        (CHECK-PAGE-WRITE-unboxed)
        ((VMA) ADD VMA (A-CONSTANT 1))
        ((MD-START-WRITE) A-DISK-MA)
        (CHECK-PAGE-WRITE-unboxed)
        ((VMA) ADD VMA (A-CONSTANT 1))
        ((MD-START-WRITE) A-DISK-FINAL-ADDRESS)
        (CHECK-PAGE-WRITE-unboxed)
        ((VMA) ADD VMA (A-CONSTANT 1))
        ((MD-START-WRITE) A-DISK-ECC)
        (CHECK-PAGE-WRITE-unboxed)

        ((MD) DPB (M-CONSTANT -1) (BYTE-FIELD 17. 15.) A-DISK-RETRY-STATE)
        ((VMA-START-WRITE) ADD M-E (A-CONSTANT (EVAL (// %DISK-RQ-DONE-FLAG 2))))
        (CHECK-PAGE-WRITE-unboxed)                      ;Set completion flag, return retry state
        (CALL-EQUAL M-ZERO A-DISK-RESERVED-FOR-USER ILLOP)      ;Took a page fault??
        (POPJ-AFTER-NEXT (M-T) A-V-NIL)
       ((A-DISK-RESERVED-FOR-USER) M-ZERO)      ;I'm done with it
#-lambda(end-comment)

;;; Support for "user" disk I/O
;;; Note that the interrupt-enable bit in the command word controls
;;; whether or not system error recovery features are invoked.

#-exp(begin-comment)
x-io-cmd-run (misc-inst-entry %io-cmd-run)

;set up a-disk-transfer-size before removing this

        ;Get disk-rq array, which must be temp-wired
#+exp   ((vma) pdl-top)
        (DISPATCH-XCT-NEXT #+lambda DISPATCH-WRITE-VMA
                   (I-ARG DATA-TYPE-INVOKE-OP) Q-DATA-TYPE PDL-TOP ARRAY-HEADER-SETUP-DISPATCH)
        ((m-a) invalidate-array-cache c-pdl-buffer-pointer-pop)

        (call store-array-registers-in-accumulators)

;       ((md) m-e)
;       (no-op)
;       (dispatch l2-map-status-code d-get-map-bits)
        ;get map set up
        ((vma-start-read) m-e)
        (check-page-read-no-interrupt)
        ((md) m-e)
        (no-op)
        ((m-1) dpb l2-map-physical-page (byte-field 22. 10.) a-zero)
        ((m-1) dpb m-e (byte-field 8 2) a-1)

        ;; For now, no queueing, just perform request immediately
        ((A-DISK-IDLE-TIME) M-ZERO)             ;I use the disk
        ((A-DISK-RESERVED-FOR-USER) SETO)       ;I want the disk
        (CALL AWAIT-DISK)                       ;Wait for disk control to become available

        ((md) setz)
        ((vma-start-write) a-nupi-special-event-vadr)
        (illop-if-page-fault)
        ((md) m-1)
        ((vma-start-write-unmapped) (a-constant #xf2e00004))

        ((a-active-iopb) m-e)
        (call await-disk-hardware)

        (CALL-EQUAL M-ZERO A-DISK-RESERVED-FOR-USER ILLOP)      ;Took a page fault??

        (POPJ-AFTER-NEXT (M-T) A-V-NIL)
       ((A-DISK-RESERVED-FOR-USER) M-ZERO)      ;I'm done with it
#-exp(end-comment)

#-lambda(begin-comment)
;This is the LAMBDA INTERPHASE disk driver

convert-m-1-to-a-disk-address
        (declare (args a-1) (clobbers a-tem1 a-2))
        (CALL-XCT-NEXT DIV)                     ;Convert disk address to physical
       ((M-2) DPB M-ZERO Q-ALL-BUT-POINTER A-DISK-BLOCKS-PER-CYLINDER)

        ((Q-R) ADD Q-R A-DISK-CYLINDER-OFFSET)  ;offset entire world!!!

        ((A-DISK-ADDRESS) DPB Q-R DA-CYLINDER A-ZERO)   ;Save cylinder
        (CALL-XCT-NEXT DIV)
       ((M-2) DPB M-ZERO Q-ALL-BUT-POINTER A-DISK-BLOCKS-PER-TRACK)
        (popj-after-next (A-DISK-ADDRESS) DPB Q-R DA-HEAD A-DISK-ADDRESS)       ;Save head
       ((A-DISK-ADDRESS) DPB M-1 DA-BLOCK A-DISK-ADDRESS)       ;Save block


;;; Here to start a disk operation that has been set up in the A-memory variables.
;;; Also called from interrupt level for retries
;;; Returns immediately; call AWAIT-DISK if you want to wait for completion.
;;; Returns with address of disk-run-light in VMA
START-DISK-OP
        ((A-DISK-COMMAND) A-DISK-READ-WRITE)
        ((A-DISK-DOING-READ-COMPARE) M-ZERO)
;;; Here to start some command other than the one we are really supposed to be doing
START-DISK-OP-1
 ;      ((MD) DPB (M-CONSTANT -1)       ;Turn on interrupt enable
 ;              (BYTE-FIELD 1 11.) A-DISK-COMMAND)
;;; Enter here from %DISK-OP. must not clobber M-3 from below here.
START-DISK-OP-2
        (CALL STORE-IOPB-FROM-A-MEM)
        (CALL IP-DISK-START)
        ((A-DISK-BUSY) (M-CONSTANT -1))
        ((MD) (M-CONSTANT -1))
        (POPJ-AFTER-NEXT (VMA-START-WRITE) A-DISK-RUN-LIGHT)
       (CHECK-PAGE-WRITE-NO-INTERRUPT)



;;; Subroutine to wait for a disk operation to complete.  Checks for interrupts,
;;; but doesn't check for interrupts between discovering that it is idle and
;;; returning; hence it is guaranteed still to be idle.
await-disk
        (popj-equal a-disk-busy m-zero)
        ((a-active-iopb) a-iopb-base)
        (call await-disk-hardware)
        (call disk-completion-main-program)
        (jump await-disk)

AWAIT-DISK-DELAY
        ((M-TEM) (A-CONSTANT 100.))     ;give delay of 40 US at full (half) speed.
AW-D-0  (JUMP-GREATER-THAN-XCT-NEXT M-TEM A-ZERO AW-D-0)
       ((M-TEM) SUB M-TEM (A-CONSTANT 1))

await-disk-hardware
        ;WAIT FOR OPERATION TO COMPLETE
        ((VMA-START-READ) a-active-iopb)
        (CHECK-PAGE-READ)       ;allow interrupts. interrupt hacker does not hack disks now.
        ((M-TEM) LDB (BYTE-FIELD 8 16.) READ-MEMORY-DATA)
        (JUMP-EQUAL M-TEM (A-CONSTANT 201) AWAIT-DISK-DELAY)
        (JUMP-EQUAL M-TEM (A-CONSTANT 0) AWAIT-DISK-DELAY)
        (popj)

DISK-COMPLETION-GET-STATUS
        (CALL MAP-INTERPHASE-ERROR)
        ((A-DISK-STATUS) M-A)
        (POPJ)

MAP-INTERPHASE-ERROR
        ((VMA-START-READ) A-IOPB-BASE)
        (CHECK-PAGE-READ-NO-INTERRUPT)
        ((M-TEM) LDB (BYTE-FIELD 8 16.) READ-MEMORY-DATA)       ;status byte
        ((M-A) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 200))  ;no errors on successful reply
        ((M-TEM) LDB (BYTE-FIELD 8 24.) READ-MEMORY-DATA)       ;error code.
    ;map Interphase error code to CADR error bit.
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 5) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 33))      ;1B(hex), unit not selected
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 6) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 36))      ;1E, drive faulted
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 8.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 57))      ;2F, not on cylinder
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 9.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 20))      ;10, drive not ready
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 10.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 22))      ;12, seek error (via header compare)
  ;     ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 11.) A-ZERO)
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 12.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 34))      ;1C, no address mark, header field
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 13.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 47))      ;27, data overrun
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 14.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 61))      ;31, format overrun on data
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 15.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 23))      ;13, ECC code error-data field
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 16.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 43))      ;23, Uncorrectable error
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 17.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 51))      ;29, Sector not found
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 18.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 52))      ;2A, ID field header wrong
  ;     ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 19.) A-ZERO)
        ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 20.) A-ZERO)
        (POPJ-EQUAL M-TEM (A-CONSTANT 30))      ;18, BUS timeout
  ;     ((M-A) DPB M-MINUS-ONE (BYTE-FIELD 1 23.) A-ZERO)
        (CALL ILLOP)            ;disk error not mappable to corresponding CADR disk error
        (POPJ)

IP-DISK-START   ;clobbers M-T, M-LAM plus registers clobbered by map faults.
                ;store multibus->nubus mapping register so as to allow disk control to access
                ; IOPB.
        (CALL-XCT-NEXT TRANSLATE-CADR-PHYSICAL-TO-NUBUS)
       ((M-T) A-IOPB-BASE)
        ((m-lam) xor m-lam a-local-phys-adr-convert)
        ((MD) LDB (BYTE-FIELD 22. 10.) M-LAM (A-CONSTANT 40000000))   ;page number to
                  ;store in SDU map.  Constant is enable bit in map.
        ((VMA) A-MULTIBUS-DISK-MAP-BASE)  ;construct hardware-virtual-address of SDU map.
        ((VMA-START-WRITE) ADD VMA A-MULTIBUS-MAP-HARDWARE-VIRTUAL-ADDRESS)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 2000))   ;write 3 times, 1 byte ea time.
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 2000))
        (CHECK-PAGE-WRITE-NO-INTERRUPT)

        ((m-t) a-processor-switches)
        (jump-if-bit-set (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                         m-t share-start)

                ;writes IOPB base address and gives start command.
        ((MD) A-MULTIBUS-DISK-MAP-BASE)
        ((MD) DPB MD (BYTE-FIELD 10. 10.) A-ZERO)  ;multibus byte adr of IOPB map
        ((M-T) A-IOPB-BASE)
        ((M-T) DPB M-T (BYTE-FIELD 8. 2) A-ZERO) ;byte offset within page.
        ((M-T) ADD MD A-T)      ;M-T now has byte adr where disk should look for IOPB
        ((VMA) A-DISK-REGS-BASE)
        ((vma) add vma (a-constant 2))  ;write IOPB base address
        ((MD-START-WRITE) LDB (BYTE-FIELD 8 16.) M-T A-ZERO)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((VMA) ADD VMA (A-CONSTANT 1))
        ((MD-START-WRITE) LDB (BYTE-FIELD 8 8) M-T A-ZERO)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((VMA) ADD VMA (A-CONSTANT 1))
        ((MD-START-WRITE) LDB (BYTE-FIELD 8 0) M-T A-ZERO)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        ((VMA) A-DISK-REGS-BASE)
        ((MD-START-WRITE) (A-CONSTANT IP-DISK-START-COMMAND))
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        (POPJ)

share-start
        ((md) (a-constant 1))
        ((vma) a-proc-conf-virtual-adr)
        ((vma-start-write) add vma (a-constant (eval %processor-conf-share-runme)))
        (check-page-write-no-interrupt)
        ((vma-start-write) (a-constant (eval (+ 177370000 (ash #x1fc -2))))) ;multibus interrupt 7
        (check-page-write-no-interrupt)
        (popj)

STORE-IOPB-FROM-A-MEM  ;clobbers M-1, M-2, M-LAM, M-TEM, M-C
        ;translates CADR type disk command in A-memory variables to IOPB form and
        ; sets up multibus map.
        ;fill in iopb
         ; wd0: (errors,status,command-options,command) 10400+command
         ; wd1: (cyl lsb, cyl msb, head, unit)
         ; wd2: (sect count lsb, sect count msb, sect lsb, sect msb)
         ; wd3: (buf lsb, buf msb, buf xmb, dma count)
         ; wd4: (rel adr lsb, rel adr msb, io adr lsb, io adr msb)
         ; wd5: (iopb link lsb, iopb link msb, iopb llink xmb, reserved)

        (CALL-XCT-NEXT SETUP-MAP-FROM-CLP)
       ((M-C) A-DISK-CLP)
        ((M-LAM) DPB M-ZERO (BYTE-FIELD 28. 4) A-DISK-COMMAND)   ;get CADR disk command
        (JUMP-EQUAL-XCT-NEXT M-LAM (A-CONSTANT DISK-READ-COMMAND) SIOPB-1)
       ((MD) (A-CONSTANT IP-DISK-READ-COMMAND-WORD))
        (JUMP-EQUAL-XCT-NEXT M-LAM (A-CONSTANT DISK-WRITE-COMMAND) SIOPB-1)
       ((MD) (A-CONSTANT IP-DISK-WRITE-COMMAND-WORD))
        (CALL-NOT-EQUAL M-LAM (A-CONSTANT DISK-READ-COMPARE-COMMAND) ILLOP)
        ((MD) (A-CONSTANT IP-DISK-READ-COMPARE-COMMAND-WORD))
SIOPB-1 ((VMA-START-WRITE) A-IOPB-BASE)
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)         ;store command word.
        ((M-1) A-DISK-ADDRESS)
        ((M-2) LDB M-1 DA-UNIT A-ZERO)
        ((M-TEM) LDB M-1 DA-HEAD A-ZERO)
        ((M-2) DPB M-TEM (BYTE-FIELD 8 8) A-2)
        ((M-TEM) LDB M-1 DA-CYLINDER A-ZERO)
        ((M-2) DPB M-TEM (BYTE-FIELD 8 24.) A-2)    ;m-2, hi byte gets low byte of cyl
        ((M-TEM) LDB M-TEM (BYTE-FIELD 8 8) A-ZERO) ;move hi byte of cyl to low byte of m-tem
        ((M-2 MD) DPB M-TEM (BYTE-FIELD 8 16.) A-2) ;insert hi byte of cyl in m-2 and md
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))      ;wd1
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)        ;store first disk address word

        ((M-TEM) LDB M-1 DA-BLOCK A-ZERO)
        ((M-2) DPB M-TEM (BYTE-FIELD 8 8) A-ZERO)
        ((M-TEM) LDB (BYTE-FIELD 8 8) M-TEM A-ZERO)
        ((M-2) DPB M-TEM (BYTE-FIELD 8 0) A-2)
  ;     ((M-2) ADD M-2 A-2)                     ;double the sector number
  ;     ((M-C) ADD M-C A-C)                     ;double the sector count
        ((M-2) DPB M-C (BYTE-FIELD 8 24.) A-2)
        ((M-C) LDB (BYTE-FIELD 8 8) M-C A-ZERO)
        ((M-2 MD) DPB M-C (BYTE-FIELD 8 16.) A-2)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))      ;wd2
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)      ;store sector count, sector

        ((M-TEM) A-MULTIBUS-DISK-MAP-BASE)  ;first mapping reg used for data.
        ((m-tem) add m-tem (a-constant 2))
        ((M-TEM) DPB M-TEM (BYTE-FIELD 10. 2.) A-ZERO)  ;first multibus byte adr for data.n
                                ;shifted -8. (lsb of address is always 0)
        ((M-2) DPB M-TEM (BYTE-FIELD 8 16.) (A-CONSTANT 200))  ;200 is DMA count, dpb in msb
        ((MD) SELECTIVE-DEPOSIT M-TEM (BYTE-FIELD 8 8) A-2) ;stick in xsb
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)              ;store buffer addr, dma count
        ((M-TEM) A-MULTIBUS-DISK-DEVICE-NUMBER)
        ((M-TEM1) LDB (BYTE-FIELD 8. 8.) M-TEM A-ZERO)
        ((MD) DPB M-TEM (BYTE-FIELD 8. 8.) A-TEM1)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)              ;store rel adr, io adr
        ((MD) (A-CONSTANT 0))
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)              ;linked IOPB address.
        (POPJ)

SETUP-MAP-FROM-CLP      ;set up MULTIBUS to NUBUS map from CADR CLP pointer in M-C.
    ;ACs clobbered:  M-1, M-2, M-T, M-LAM, M-C. M-C returns number of pages
    ; which were set up.  Note well that the CLP is a CADR-HARDWARE-VIRTUAL-ADDRESS.
    ; ie, it would be a physical address in CADR.

        ((M-2) A-MULTIBUS-DISK-MAP-BASE)  ;construct hardware-virtual-address of SDU map.
        ((m-2) add m-2 (a-constant 2))
        ((M-2) add M-2 A-MULTIBUS-MAP-HARDWARE-VIRTUAL-ADDRESS)
        ((M-1) M-C)
SM-CLP-0
        ((m-lam) sub m-1 a-c)
        (call-greater-or-equal m-lam a-number-of-data-mapping-registers-for-disk illop)
        (CALL-XCT-NEXT PHYS-MEM-READ)   ;CLP is a CADR-HARDWARE-VIRTUAL-ADDRESS.
       ((VMA) M-1)
        ((M-T) MD)      ;now have CADR ccw word
        (CALL TRANSLATE-CADR-PHYSICAL-TO-NUBUS)  ;map to nubus physical address, ans in M-LAM
        ((m-lam) xor m-lam a-local-phys-adr-convert)
        ((VMA) M-2)
        ((MD-START-WRITE) LDB (BYTE-FIELD 22. 10.) M-LAM (A-CONSTANT 40000000))
                  ;store in SDU map.  Constant is enable bit in map.
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 2000))   ;write 3 times, 1 byte ea time.
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 2000))
        (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)
        ((M-1) ADD M-1 (A-CONSTANT 1))
        (JUMP-IF-BIT-SET-XCT-NEXT (BYTE-FIELD 1 0) M-T SM-CLP-0)
       ((M-2) ADD M-2 (A-CONSTANT 1))
        ((M-C) SUB M-1 A-C)     ;RETURN NUMBER OF PAGES SET UP IN M-C
        (POPJ)
#-lambda(end-comment)


#-exp
(begin-comment)
;This is the EXPLORER NUPI disk driver

convert-m-1-to-a-disk-address
        ;*** hack unit here
        (popj-after-next (a-disk-address) m-1)
       (no-op)

;;; Here to start a disk operation that has been set up in the A-memory variables.
;;; Also called from interrupt level for retries
;;; Returns immediately; call AWAIT-DISK if you want to wait for completion.
;;; Returns with address of disk-run-light in VMA
START-DISK-OP
        ((a-disk-command) a-disk-read-write)
        ((a-disk-doing-read-compare) m-zero)
;;; Here to start some command other than the one we are really supposed to be doing
start-disk-op-1
 ;      ((md) dpb (m-constant -1)       ;Turn on interrupt enable
 ;              (byte-field 1 11.) a-disk-command)
;;; Enter here from %DISK-OP. must not clobber M-3 from below here.
start-disk-op-2
        (call store-nupi-command-block-from-a-mem)
        (call nupi-disk-start)
        ((a-disk-busy) (m-constant -1))
        ((md) (m-constant -1))
        (popj-after-next (vma-start-write) a-disk-run-light)
       (check-page-write-no-interrupt)



;;; Subroutine to wait for a disk operation to complete.  Checks for interrupts,
;;; but doesn't check for interrupts between discovering that it is idle and
;;; returning; hence it is guaranteed still to be idle.
await-disk
        (popj-equal a-disk-busy m-zero)
        ((a-active-iopb) a-nupi-command-block-base)
        (call await-disk-hardware)
        (call disk-completion-main-program)
        (jump await-disk)

await-disk-delay
        ((m-tem) (a-constant 100.))     ;give delay of 40 US at full (half) speed.
aw-d-0  (jump-greater-than-xct-next m-tem a-zero aw-d-0)
       ((m-tem) sub m-tem (a-constant 1))

;address of base of command block in M-A
await-disk-hardware
        ;check for operation to complete
        ((vma-start-read) m+a+1 m-zero a-active-iopb)
        (check-page-read)       ;allow interrupts. interrupt hacker does not hack disks now.
        (popj-if-bit-set (byte-field 1 30.) md)
        ((vma-start-read) a-nupi-special-event-vadr)
        (check-page-read)
        (popj-not-equal md a-zero)
        (jump await-disk-delay)

;this is called with a-intr-a, etc alive ... therefore must not let interrupt happen
disk-completion-get-status
        ((vma-start-read) m+a+1 m-zero a-nupi-command-block-base)
        (check-page-read-no-interrupt)
        ((a-disk-status) a-zero)
        ((m-a) a-zero)
        (jump-if-bit-clear (byte-field 1 29.) md check-for-special-nupi-event)

        ((m-tem) ldb (byte-field 8 8) md)               ;device error code
        (call map-nupi-error)
        ((a-disk-status) m-a)
        (popj)

check-for-special-nupi-event
        ((vma-start-read) a-nupi-special-event-vadr)
        (illop-if-page-fault)
        (call-not-equal md a-zero get-nupi-extended-status)
        (popj)

;this should do the extended status command, then set a-disk-status to some cadr error code
;actually, we should only get here on complete machine failures, or if we create a grabage
;command block
get-nupi-extended-status
        (call illop)
        (popj)

map-nupi-error
    ;map nupi error code to CADR error bit.
        ((m-a) dpb m-minus-one (byte-field 1 5) a-zero)
        (popj-equal m-tem (a-constant #x41))    ;unit not selected
        ((m-a) dpb m-minus-one (byte-field 1 6) a-zero)
        (popj-equal m-tem (a-constant #x46))    ;drive faulted (temperature)
        (popj-equal m-tem (a-constant #xa3))    ;write fault
        ((m-a) dpb m-minus-one (byte-field 1 8.) a-zero)
        (popj-equal m-tem (a-constant #xa6))    ;not on cylinder
        ((m-a) dpb m-minus-one (byte-field 1 9.) a-zero)
        (popj-equal m-tem (a-constant #x62))    ;drive not ready
        (popj-equal m-tem (a-constant #x44))
        (popj-equal m-tem (a-constant #x65))    ;drive offline
        (popj-equal m-tem (a-constant #x43))    ;write protected ... a little the same
        (popj-equal m-tem (a-constant #x42))    ;no media loaded
        ((m-a) dpb m-minus-one (byte-field 1 12.) a-zero)
        (popj-equal m-tem (a-constant #xc3))    ;no address mark, header field
        (popj-equal m-tem (a-constant #xc4))
        ((m-a) dpb m-minus-one (byte-field 1 13.) a-zero)
        (popj-equal m-tem (a-constant #x63))    ;data overrun
        ((m-a) dpb m-minus-one (byte-field 1 16.) a-zero)
        (popj-equal m-tem (a-constant #xc2))    ;Uncorrectable error
        ((m-a) dpb m-minus-one (byte-field 1 17.) a-zero)
        (popj-equal m-tem (a-constant #xc5))    ;Sector not found
        (call illop)            ;disk error not mappable to corresponding CADR disk error
        (popj)

nupi-DISK-START ;clobbers M-T, M-LAM plus registers clobbered by map faults.
        ((md) setz)
        ((vma-start-write) a-nupi-special-event-vadr)
        (illop-if-page-fault)
        (call-xct-next translate-cadr-physical-to-nubus)
       ((m-t) a-nupi-command-block-base)
        ((md) m-lam)
        ((vma-start-write-unmapped) (a-constant #xf2e00004))
        (popj)

store-nupi-command-block-from-a-mem  ;clobbers M-1, M-LAM, M-TEM, M-C
        ;translates CADR type disk command in A-memory variables to command block form

        ((m-tem) dpb m-zero (byte-field 28. 4) a-disk-command)   ;get CADR disk command
        (jump-equal-xct-next m-tem (a-constant disk-read-command) store-nupi-command-block-1)
       ((m-1) (a-constant #x12)) ;nupi read command
        (jump-equal-xct-next m-tem (a-constant disk-write-command) store-nupi-command-block-1)
       ((m-1) (a-constant #x13))

        (call illop)

store-nupi-command-block-1
;NUPI command block
;
;   off in words  n-bytes
;       0           1       unit-select
;       0           1       spare
;       0           1       options
;       0           1       command
;       1           4       returned status
;       2           4       buffer pointer or param list pointer
;       3           4       transfer count (in bytes) (must be multiple of 4)
;       4           4       device block address
;       5           4       interrupt address
;       6           4       reserved
;       7           4       reserved
        ;command word
        ((m-tem) a-disk-address)
        ((m-tem) ldb (byte-field 8 24.) m-tem)          ;unit select in low byte
        ((m-tem) dpb m-minus-one (byte-field 1 22.) a-tem)      ;scatter bit
        ((md) dpb m-1 (byte-field 8 24.) a-tem)         ;command in high byte
        ((vma-start-write m-1) a-nupi-command-block-base)
        (check-page-write-no-interrupt)

        ;status word
        ((md) setz)
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        ;parameter list pointer
        ((m-t) a-disk-clp)
        (call translate-cadr-physical-to-nubus)
        ((md) xor m-lam a-local-phys-adr-convert)
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        ;transfer count
        ((m-t) a-disk-transfer-size)
        ((md) dpb m-t (byte-field 22. 10.) a-zero) ;convert from pages to bytes
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        ;device block address
        ((md) dpb m-zero (byte-field 8 24.) a-disk-address)     ;zero out the unit number
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        ;interrupt address
        ((md) setz)
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        ;reserved 1
        ((md) setz)
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        ;reserved 2
        ((md) setz)
        ((vma-start-write m-1) m+1 m-1)
        (check-page-write-no-interrupt)

        (popj)


#-exp(end-comment)
))

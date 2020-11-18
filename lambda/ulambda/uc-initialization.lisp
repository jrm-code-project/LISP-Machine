;-*- Mode:LISP; Package:LAMBDA; Base:8; readtable: ZL -*-

;This file is for the LAMBDA and EXPLORER only!!
;       ** (c) Copyright 1983 Lisp Machine Inc **

(DEFCONST UC-INITIALIZATION '(

(LOCALITY A-MEM)
a-band-saved-crc (0)
a-band-crc-accumulator (0)
a-band-compute-crc-flag (0)
a-band-crc-tem (0)
a-address-space-maximum (0)                     ;maximum allowed amount of address space, sum of page partition sizes.
(locality i-mem)

END-WIRED-UCODE
  ;All after here is "paged out" after initialization is complete.
  ;(However, unlike pagable ucode it does get loaded by the PROM because it lives below 36000).
  ;Code after here should not be used at all once LISP is started unless a
  ;TURN-PAGABLE-UCODE-OFF-AND-RESTORE-INITIALIZATION operation is done
  ;  (ie, just before a DISK-SAVE, etc).
;of course, on the explorer, this is always available...


xdisk-save      ;get here from DISK-SAVE
        ((m-4) a-processor-switches)
        (call-if-bit-set (lisp-byte %%processor-switch-fast-boot-enable)
                         m-4 trap)
     (error-table cant-disk-save-quantum-mapped-image)
        (call invalidate-cons-caches)   ;try to preserve freshly consed stuff.
#+lambda((RG-MODE) ANDCA RG-MODE
                         (A-CONSTANT (BYTE-MASK (BYTE-FIELD 1 27.)))) ;disable interrupts
#+exp   ((mcr) andca mcr (a-constant 1_15.))
        ((M-4) PDL-POP)
        ((M-4) DPB PDL-POP (BYTE-FIELD 20 20) A-4)
        ((M-S) Q-POINTER PDL-POP)
        ((MD) (A-CONSTANT 1000))    ;store code so this band known to be in compressed format
        (JUMP-IF-BIT-CLEAR BOXED-SIGN-BIT M-S DISK-SAVE-1)
        (call trap)
    (error-table incremental-disk-save-not-supported)
DISK-SAVE-1
        ((VMA-START-WRITE) (A-CONSTANT (EVAL (+ 400 %SYS-COM-BAND-FORMAT))))  ;before swapout
        (ILLOP-IF-PAGE-FAULT)                   ; so it gets to saved image on disk
        ;;set valid size to 0 for crc computation - will be updated after save is finished
        ((MD) setz)
        ((VMA-START-WRITE) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-VALID-SIZE))))
        (ILLOP-IF-PAGE-FAULT)
        ((md) (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-write) (a-constant (eval (+ 400 %sys-com-band-crc))))
        (illop-if-page-fault)
        (CALL SWAP-OUT-ALL-PAGES)               ;Make sure disk has valid data for all pages.
#+exp   ((field a-destination-multiplier (plus 1_12. 1777)) m-4)
        (CALL COLD-READ-LABEL)                  ;Find the specified partition, and PAGE.
                                                ;cold-read-label no longer clobbers memory
        ((a-band-crc-accumulator) setz)
;Set up args for DISK-SAVE-REGIONWISE in case we go straight there.,
        ((M-K) A-ZERO)
        ((M-AP) M-ZERO)                 ;region to hack.
        ((M-Q) M-I)

        (CALL DISK-SAVE-REGIONWISE-SUBR)
        (JUMP COLD-SWAP-IN-after-disk-save)                     ;Physical core now clobbered, so re-swap-in.

;M-I and M-J have origin and size of band to dump into.
;M-Q has disk address, within band, to start writing at,
; and M-AP has region number of first region to dump.
;M-S has size of phys memory in words.
;M-K used to have size of page bitmap in bits for incremental - now always 0
;A-V-REGION-ORGIN, -FREE-POINTER, and -BITS
;are valid, and those arrays are still in main memory and wont get clobbered by calling
;DISK-COPY-SECTION.
DISK-SAVE-REGIONWISE-SUBR
        ((A-COPY-BAND-TEM) ADD M-I A-J) ;better not try to write above here.
        ((A-COPY-BAND-TEM1) M-I)        ;Starting track for dest. band.
DISK-SR-1
        ((VMA-START-READ) ADD M-AP A-V-REGION-BITS)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) LDB (LISP-BYTE %%REGION-SPACE-TYPE) MD)
        (JUMP-EQUAL M-TEM A-ZERO DISK-SR-2)     ;free region, forget it.
        ((VMA-START-READ) ADD M-AP A-V-REGION-ORIGIN)
        (ILLOP-IF-PAGE-FAULT)
        ((M-I) LDB VMA-PAGE-ADDR-PART MD)
        ((M-I) ADD M-I A-DISK-OFFSET)
        ((VMA-START-READ) ADD M-AP A-V-REGION-FREE-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((MD) ADD MD (A-CONSTANT 377))
        ((M-J) LDB VMA-PAGE-ADDR-PART MD)
        ((M-TEM) ADD M-Q A-J)
        (CALL-GREATER-OR-EQUAL M-TEM A-COPY-BAND-TEM BAND-NOT-BIG-ENOUGH)
        ((M-TEM) ADD M-I A-J)
        ((M-TEM) SUB M-TEM A-DISK-OFFSET)
        (CALL-GREATER-OR-EQUAL M-TEM a-address-space-maximum ILLOP)  ;Band not within paging partition
   (error-table crash band seems to overflow paging partition)
        (CALL DISK-SAVE-REGION)
DISK-SR-2
        ((vma-start-read) (a-constant (eval (+ 400 %sys-com-number-regions))))
        (illop-if-page-fault)
        ((m-tem) q-pointer md)
        ((M-AP) ADD M-AP (A-CONSTANT 1))
        (JUMP-LESS-THAN M-AP A-TEM DISK-SR-1)

        ((m-1) m+a+1 m-zero a-copy-band-tem1)           ;second page of saved band
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-2) (A-CONSTANT 1))                  ;one page
        ((M-B) (A-CONSTANT copy-buffer-origin)) ;to a scratch area - just like disk-restore
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))
        (CALL COLD-DISK-READ)

        ;;done saving regions - now update two words in system-communication-area
        ((M-Q) SUB M-Q A-COPY-BAND-TEM1)
        ((MD) DPB M-Q (BYTE-FIELD 30 10) A-ZERO) ;Record active size of band.
        ((VMA-START-WRITE) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-VALID-SIZE))))
        (ILLOP-IF-PAGE-FAULT)
        ((vma) (a-constant copy-buffer-origin))
        ((vma-start-write) add vma (a-constant (eval %sys-com-valid-size)))
        (illop-if-page-fault)

        ((md) a-band-crc-accumulator)
        ((md) ldb (byte 16. 0) md (a-constant (plus (byte-value q-data-type dtp-fix)
                                                    (byte-value (byte-field 8 16.) 1))))
        ((vma-start-write) (a-constant (eval (plus 400 %sys-com-band-crc))))
        (illop-if-page-fault)
        ((vma) (a-constant copy-buffer-origin))
        ((vma-start-write) add vma (a-constant (eval %sys-com-band-crc)))
        (illop-if-page-fault)


        ((m-1) m+a+1 m-zero a-copy-band-tem1)           ;second page of saved band
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-2) (A-CONSTANT 1))                  ;one page
        ((M-B) (A-CONSTANT copy-buffer-origin)) ;to a scratch area - just like disk-restore
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))
        (call COLD-DISK-WRITE)          ;write it on the band.
        (popj)


;M-I and M-J have origin and size, on disk in the PAGE partition, of a region.
;M-Q has disk address to copy to in band being dumped.
DISK-SAVE-REGION
DISK-SAVE-SECTION
        ((M-TEM) ADD M-Q A-J)
        (CALL-GREATER-OR-EQUAL M-TEM A-COPY-BAND-TEM BAND-NOT-BIG-ENOUGH)
        (JUMP DISK-COPY-SECTION-from-page-to-lod)

BAND-NOT-BIG-ENOUGH     ;Destination band not big enuf.  This should have been detected
        (CALL ILLOP)    ; before now.  If you proceed this, it should swap your band
    (error-table crash "Destination band not big enough")
        (POPJ)          ; back in.

;Make sure all pages are correct on disk.
;Requires that M-S contain the number of words of physical main memory.
;Has the side-effect of destroying the page hash table.
;For %DISK-SAVE, that doesn't matter since we just re-boot anyway.
SWAP-OUT-ALL-PAGES
        ((C-PDL-BUFFER-POINTER-PUSH) M-S)
        ((M-S) LDB (BYTE-FIELD 17. 8) M-S A-ZERO)       ;Number of physical pages.
        ((VMA-START-READ) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-WIRED-SIZE))))
        (ILLOP-IF-PAGE-FAULT)
        ((M-T) (BYTE-FIELD 17. 8) READ-MEMORY-DATA)     ;Number of wired pages.
        ((C-PDL-BUFFER-POINTER-PUSH) M-T)
        ((M-T) SUB M-S (A-CONSTANT 1))          ;First page to do is highest in core
;Swap out all unwired pages first, using %DELETE-PHYSICAL-PAGE and updating the PHT normally.
SWAP-OUT-ALL-PAGES-1
        ((C-PDL-BUFFER-POINTER-PUSH) M-T)       ;Save current page
        ((C-PDL-BUFFER-POINTER-PUSH) DPB M-T VMA-PAGE-ADDR-PART A-ZERO)  ;arg
        (CALL XDPPG)
        ((M-T) SUB C-PDL-BUFFER-POINTER-POP (A-CONSTANT 1))
        (JUMP-GREATER-OR-EQUAL M-T A-ZERO SWAP-OUT-ALL-PAGES-1)
;Now swap out all the wired pages
        ((M-A) (A-CONSTANT WORDS-TO-DIRECT-MAP-DURING-BOOTSTRAP))  ;Direct-map the first 64K
        (CALL INITIAL-MAP-A)
        ((M-1) A-DISK-OFFSET)                   ;Disk address of virtual location 0
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-2) C-PDL-BUFFER-POINTER-POP)        ;Number of wired pages
        ((M-B) M-ZERO)                          ;Physical memory location 0
        ((M-C) DPB M-2 VMA-PAGE-ADDR-PART A-ZERO)       ;Put CCW list in high memory
        ((M-S) C-PDL-BUFFER-POINTER-POP)
        (JUMP COLD-DISK-WRITE)

(begin-comment) (end-comment)

xdisk-restore   ;from DISK-RESTORE-1
#+lambda((RG-MODE) ANDCA RG-MODE
         (A-CONSTANT (PLUS (BYTE-MASK (BYTE-FIELD 1 27.))       ;disable interrupts
                           (BYTE-MASK (BYTE-FIELD 1 30.)))))    ;set 25 bit mode.
;#+exp   ((mcr) andca mcr (a-constant 1_15.)) ;only disable interrupts on explorer
#-exp (begin-comment)
        ;;the explorer bootstrap prom seems to leave some interrupts enabled
        ;;with the vectors pointing at physical memory!
        ;;since we don't have code to reinitialized all of the devices, we just
        ;;make them all "safe" by pointing them to 0

        ;;first clear 16 interrupt vectors on SI board
        ((vma) (a-constant #xf5f00000))
        ((md) setz)

reset-interrupts-for-explorer
        ((vma-start-write-unmapped) vma)
        (no-op)
        ((vma) add vma (a-constant 4))
        (jump-not-equal vma (a-constant #xf5f00040) reset-interrupts-for-explorer)

        ;;nupi special event interrupt
        ((vma-start-write-unmapped) (a-constant #xf2e00004))
        (no-op)

        ;;ethernet event interrupt
        ((vma-start-write-unmapped) (a-constant #xf000A000))
        (no-op)

        ((mcr) (a-constant (eval (+ (dpb 1 (byte 1  8.) 0) ;memory cycle enable
                                    (dpb 0 (byte 1  9.) 0) ;forced access request
                                    (dpb 0 (byte 1 10.) 0) ;bus lock
                                    (dpb 1 (byte 1 11.) 0) ;prom disable
                                    (dpb 0 (byte 1 12.) 0) ;halt on parity errors
                                    (dpb 0 (byte 1 13.) 0) ;abort on bus error
                                    (dpb 0 (byte 1 14.) 0) ;sequence break request
                                    (dpb 0 (byte 1 15.) 0) ;interrupt enable
                                    (dpb 1 (byte 1 20.) 0) ;power fail and warm boot enable
                                    (dpb 0 (byte 1 21.) 0) ;nubus reset
                                    (dpb 0 (byte 1 22.) 0) ;need fetch
                                    (dpb 0 (byte 1 23.) 0) ;loop on self test
                                    (dpb 0 (byte 1 24.) 0) ;enable MISC0
                                    (dpb 0 (byte 1 25.) 0) ;enable MISC1
                                    (dpb 0 (byte 1 26.) 0) ;macro chaining enable
                                    ))))
#-exp (end-comment)

        (call invalidate-cons-caches)   ;try to avoid various screws.

        ;;get-configuration either sets all of the a-pmh-* and a-pmo-* variables,
        (call get-configuration)
#+LAMBDA(call flush-cache)
        (CALL-XCT-NEXT COLD-WIRE-MAP)           ; (more than) 64K to be direct-mapped
       ((WRITE-MEMORY-DATA) (A-CONSTANT WORDS-TO-DIRECT-MAP-DURING-BOOTSTRAP))
        ;; Wire enough for all wired pages + disk mapping registers needed to read them in.

        ((A-DISK-BUSY) A-ZERO)                  ;used to call reset-machine

        ((M-S) A-PMH-0)                         ;Determine size of main memory
        ((M-S) ADD M-S A-PMH-1)
        ((M-S) ADD M-S A-PMH-2)
        ((M-S) ADD M-S A-PMH-3)
        ((M-S) ADD M-S A-PMH-4)
        ((M-S) ADD M-S A-PMH-5)
        ((M-S) ADD M-S A-PMH-6)
        ((M-S) ADD M-S A-PMH-7)
        ((M-S) ADD M-S A-PMH-8)
        ((M-S) ADD M-S A-PMH-9)

;;;Merge from LAD on 9-23-86 (mrc)       Re-hacked since, -naha
;;; old code:
        ((M-S) DPB M-S (BYTE-FIELD 17. 8) A-ZERO)
        ;M-S now has the first non-existent location

        ;M-S now has total memory size allocated to this processor.
        ;This isn't really useful until after the call to get-area-origins, when we can make sure that
        ;the machine has enough PHYSICAL-PAGE-DATA table for the amount of memory we have.

        (call initialize-quantum-map-cold)
#+lambda(CALL COLD-READ-MINI-LABEL)
        (CALL COLD-READ-LABEL)                  ;Find PAGE partition and specified partition.
#+exp   (Call Add-Boot-Info-to-Crash-Record)    ;Add ucode band and load band now that they are known
        ((M-1) M-I)                             ;From start of source band.
        ((m-tem) a-disk-lod-partition-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((a-band-crc-accumulator) setz)
        ((M-2) (A-CONSTANT 3))                  ;Core pages 0, 1, and 2
        ((M-B) (A-CONSTANT copy-buffer-origin)) ;to page 3 of memory, will copy to 0
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN)) ;CCW list after MICRO-CODE-SYMBOL-AREA
        (CALL COLD-DISK-READ)
        ((m-1) setz)
        ((m-2) (a-constant copy-buffer-origin))
copy-first-three-pages-loop
        ((vma-start-read) m-2)
        (illop-if-page-fault)
        ((vma-start-write) m-1)
        (illop-if-page-fault)
        ((m-1) add m-1 (a-constant 1))
        ((m-2) add m-2 (a-constant 1))
        (jump-not-equal m-1 (a-constant (eval (* page-size 3))) copy-first-three-pages-loop)

        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-band-crc))))
        ((a-band-saved-crc) q-pointer md)

        ((md) setz)
        ((vma) (a-constant copy-buffer-origin))
        ((vma-start-write) add vma (a-constant (eval (+ 400 %sys-com-valid-size))))
        (illop-if-page-fault)
        ((md) (a-constant (byte-value q-data-type dtp-fix)))
        ((vma) (a-constant copy-buffer-origin))
        ((vma-start-write) add vma (a-constant (eval (+ 400 %sys-com-band-crc))))
        (illop-if-page-fault)

;       ((m-1) (a-constant copy-buffer-origin))
;       ((m-2) (a-constant (eval (+ page-size (length system-communication-area-qs)))))
;       (call compute-region-crc)
;       ((m-1) (a-constant copy-buffer-origin))
;       ((m-1) add m-1 (a-constant (eval (* 2 page-size))))
;       ((m-2) (a-constant (eval page-size)))
;       (call compute-region-crc)

        ((m-1) (a-constant copy-buffer-origin))
        ((m-2) (a-constant (eval (* 3 page-size))))
        (call compute-region-crc)

        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-BAND-FORMAT))))
        (JUMP-EQUAL MD (A-CONSTANT 1000) DISK-RESTORE-REGIONWISE)  ;compressed partition.
        (JUMP-EQUAL MD (A-CONSTANT 1001) illop) ;incremental partition -no longer supported
;Non-compressed band (must be a cold-load band, I think).
        ;;; first turn of quantum map if this is a cold load band
        ((a-processor-switches) dpb m-zero
                (lisp-byte %%processor-switch-fast-boot-enable) a-processor-switches)
        (CALL-XCT-NEXT PHYS-MEM-READ)           ;Get useful size of partition, in words
       ((VMA) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-VALID-SIZE))))
        ((M-D) VMA-PAGE-ADDR-PART MD)           ;Number of valid pages
        (JUMP-LESS-OR-EQUAL M-J A-D DISK-COPY-PART-1)
        ((M-J) M-D)                             ;M-J is number of pages to copy (min sizes)
DISK-COPY-PART-1
        (CALL-GREATER-THAN M-J A-R ILLOP)       ;Not enough room in destination partition
    (error-table crash "Destination band not big enough")
        (CALL DISK-COPY-SECTION-from-lod-to-page)
        (JUMP COLD-SWAP-IN)

;starting vadr in M-1
;number of words in M-2
;clobbers M-1, M-2, VMA, MD
;registers also available: M-B, M-T
compute-region-crc
        (popj-equal m-2 a-zero)
        ((vma-start-read) m-1)
        (illop-if-page-fault)
        ((md) ldb (byte 16. 0) md)
        (call compute-region-crc-1)
        ((vma-start-read) m-1)
        (illop-if-page-fault)
        ((md) ldb (byte 16. 16.) md)
        (call compute-region-crc-1)

        ((m-1) add m-1 (a-constant 1))
        ((m-2) sub m-2 (a-constant 1))
        (jump compute-region-crc)

;this function will be called with every half-word in the band.
;each time, the low 16 bits of the MD contain the current half-word,
;and the high 16 bits are 0
compute-region-crc-1
        ((a-band-crc-accumulator) xor md a-band-crc-accumulator)
        (popj)

DISK-RESTORE-REGIONWISE
        ((M-K) A-ZERO)
        ((M-I) ADD M-I (A-CONSTANT 3))
        ((M-J) SUB M-J (A-CONSTANT 3))
  ;Micro-code-symbol-area has a free pointer
  ;of zero, so is not copied into band.  Therefore, REGION-ORIGIN, etc. start at 3rd page
  ;of band
        (CALL DISK-RESTORE-REGIONWISE-SUBR)
        (JUMP COLD-SWAP-IN)

;M-I and M-J have origin and size of data to restore,
;omitting the first three pages, and the bitmap and base band pages for an inc band.
;M-K used to have to do with incremental bands - now always 0
DISK-RESTORE-REGIONWISE-SUBR
;low 3 pages already in.
;Read in stuff below CCW buffer.  This had better include REGION-ORIGIN, -LENGTH, -BITS,
; -FREE-POINTER.
        ;;core address
        ((M-B) (A-CONSTANT END-OF-MICRO-CODE-SYMBOL-AREA))

        ;;we need to read in region-origin, length, bits and free-pointer
        ;; therefore, number of words = 3 * number of regions
        ((vma-start-read) (a-constant (eval (+ 400 %sys-com-number-regions))))
        (illop-if-page-fault)
        ((md) q-pointer md)
        ((m-2) dpb md (byte-field 30. 2) a-zero)        ;multiply by 4
        ((m-2) ldb (byte 24. 8) m-2)            ;convert to pages

        ((M-1) M-I)                             ;disk address
        ((m-tem) a-disk-lod-partition-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))
        (CALL COLD-DISK-READ)
        ((PDL-PUSH) M-K)
        (CALL GET-AREA-ORIGINS)         ;set up A-V-REGION-ORIGIN, -LENGTH, -BITS for below
        (call hack-quantum-map-base)

        ; Make sure physical memory size does not overflow PHYSICAL-PAGE-DATA.
        ((m-1) a-v-address-space-map)
        ((m-1) ldb q-pointer m-1)
        ((m-2) a-v-physical-page-data)
        ((m-2) ldb q-pointer m-2)
        ((m-1) sub m-1 a-2)                     ;size of PPD, now divide by two since
        ((m-1) ldb (byte-field 31. 1) m-1)      ;PPD contains 2 words per physical page of memory (so claims QCOM).
        ((m-tem) vma-page-addr-part m-s)

        (jump-less-or-equal m-tem a-1 memory-size-ok)
        ((m-s) dpb m-1 vma-page-addr-part a-zero)       ;memory size no bigger than will fit in PPD
memory-size-ok
        ;M-S now has the first non-existent location.
        ((M-K) PDL-POP)
;At this point, M-S has words physical memory.   M-I, M-J point to band.
; A-V-REGION-ORGIN, -FREE-POINTER, and -BITS are valid,
; and those arrays are still in main memory and wont get clobbered by calling
; DISK-COPY-SECTION.  A-DISK-OFFSET and A-DISK-MAXIMUM (as well as A-ADDRESS-SPACE-MAXIMUM) are set.
        ((M-AP) (A-CONSTANT 3))                 ;region to hack.
        ((A-COPY-BAND-TEM) ADD M-I A-J) ;better not try to read above here.
DISK-RR-1
        ((VMA-START-READ) ADD M-AP A-V-REGION-BITS)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) LDB (LISP-BYTE %%REGION-SPACE-TYPE) MD)
        (JUMP-EQUAL M-TEM A-ZERO DISK-RR-2)     ;free region, forget it.
        ((VMA-START-READ) ADD M-AP A-V-REGION-ORIGIN)
        (ILLOP-IF-PAGE-FAULT)
        ((M-Q) LDB VMA-PAGE-ADDR-PART MD)
;       ((M-Q) ADD M-Q A-DISK-OFFSET)
        ((VMA-START-READ) ADD M-AP A-V-REGION-FREE-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((MD) ADD MD (A-CONSTANT 377))
        ((M-J) LDB VMA-PAGE-ADDR-PART MD)
        (CALL-GREATER-OR-EQUAL M-I A-COPY-BAND-TEM ILLOP) ;bandwise EOF.
    (error-table crash "Source band too small")
        ((M-TEM) ADD M-Q A-J)

;       ((M-TEM) SUB M-TEM A-DISK-OFFSET)
        (CALL-GREATER-OR-EQUAL M-TEM a-address-space-maximum ILLOP)     ;page partition not big enuf
                                                             ; for this band.
    (error-table crash "Page band not big enough")
        ((m-1) a-processor-switches)
        (jump-if-bit-set (lisp-byte %%processor-switch-fast-boot-enable)
                         m-1 disk-rr-1-map-quantum)
        (CALL DISK-RESTORE-REGION)
        (jump disk-rr-1-5)
disk-rr-1-map-quantum
        (call-greater m-ap (a-constant (difference (a-mem-loc a-v-init-list-area)
                                                   (a-mem-loc a-v-resident-symbol-area)))
                      quantum-map-region)       ;just know about it
        (call-less-or-equal m-ap (a-constant (difference (a-mem-loc a-v-init-list-area)
                                                         (a-mem-loc a-v-resident-symbol-area)))
                            disk-restore-region)        ;if it is one of the low memory fixed
          ; regions (areas) then copy it to page band so that COLD-SWAP-IN can find it there.
disk-rr-1-5
        (CALL-GREATER-OR-EQUAL M-I A-COPY-BAND-TEM ILLOP) ;bandwise EOF.
    (error-table crash "Source band too small")
DISK-RR-2
        ((vma-start-read) (a-constant (eval (+ 400 %sys-com-number-regions))))
        (illop-if-page-fault)
        ((m-tem) q-pointer md)
        ((M-AP) ADD M-AP (A-CONSTANT 1))
        (JUMP-LESS-THAN M-AP A-TEM DISK-RR-1)
        (POPJ)

;;; For every quantum up to but not including A-V-FIRST-UNFIXED-AREA, that entry must point to the
;;; PAGE partition.  The fixed areas have been copied there because their boundaries are not quantum aligned.
;;; called from INITIALIZE-QUANTUM-MAP-FOR-REAL.
quantum-map-fixed-area-kludge
        ((vma) a-v-quantum-map)
        ((vma) add vma (a-constant (eval (* page-size %quantum-map-offset-in-tables)))) ;base of quantum map
        ((m-tem) a-zero)                        ;first quantum virtual address
        ((m-tem1) a-v-first-unfixed-area)
        ((m-tem1) ldb vma-quantum-byte m-tem1)
        ((m-tem1) dpb m-tem1 vma-quantum-byte a-zero)
        ((m-tem1) ldb vma-page-addr-part m-tem1)
quantum-map-fixed-area-kludge-loop
        ((md-start-write) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-tem)     ;quantum is valid, not device,
                                   ;already copied to page, offset in page partition is its virtual address.
        (illop-if-page-fault)
     (error-table crash page-fault-in-wired-area)
        ((vma) m+a+1 vma a-zero)
        ((q-r) a-page-partition-to-use)
        ((md-start-write) dpb q-r (lisp-byte %%pq2m-partition-number) a-zero)
        (illop-if-page-fault)
     (error-table crash page-fault-in-wired-area)
        ;;; now increment
        ((m-tem) add m-tem (a-constant 64.))    ;increment by one quantum size
        (popj-greater-or-equal m-tem a-tem1)
        ((vma) m+a+1 vma a-zero)
        (jump quantum-map-fixed-area-kludge-loop)

;;; called from DISK-RR-1 with information about a region that it wants to be copied to PAGE.  I guess
;;; the joke's on him.
;;; see comment at beginning of disk-restore-region to see what we are called with.
;;; NOTE we only allocate quanta from REGION-ORIGIN to REGION-FREE-POINTER since we don't want
;;; to allocate disk space until we need it.  This will work because DISK-SWAP-HANDLER and its friends will
;;; set up the quantum map entry if it does not already exist.  If the quantum map code in MAKE-REGION were
;;; activated instead of that in DISK-SWAP-HANDLER then this code would have to be changed to allocate all
;;; the way to REGION-LENGTH rather than just to REGION-FREE-POINTER.
quantum-map-region
        ((m-tem) dpb m-zero (lisp-byte %%virtual-page-quantum-number) a-q)
;       ((m-tem) ldb (byte-field 6 0) m-q)      ;m-q is virtual page number, what page of this quantum
        (call-not-equal m-tem a-zero illop)
     (error-table crash region does not start on quantum boundary)
        ((vma) (a-constant (eval (* page-size %partition-table-offset-in-tables))))
        ((vma-start-read) m+a+1 vma a-v-quantum-map)
        (illop-if-page-fault)
        ((m-1) ldb (lisp-byte %%pt2-offset) md)
        ((m-1) sub m-i a-1)
quantum-map-region-loop
        (jump-less-than m-j (a-constant 64.) map-last-quantum-of-region)    ;64 pages per quantum
        ((m-tem) ldb (lisp-byte %%virtual-page-quantum-number) m-q)     ;quantum number
        ((m-tem) dpb m-tem (byte-field 31. 1) a-zero)   ;double it
        ((vma) add m-tem (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((vma) add vma a-v-quantum-map)
        ((m-2) dpb m-1 (lisp-byte %%pq1m-page-offset) a-zero)                   ;offset in LOD band
        ((m-2) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-2)
        ((md-start-write) dpb m-minus-one (lisp-byte %%pq1m-page-out-copy-first) a-2)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)    ;first word of quantum map entry now written
        ((m-2) (a-constant 64.))
        ((md) dpb m-2 (lisp-byte %%pq2m-boot-pages-allocated) a-zero)
;       ((md) dpb )      ;must get partition table index of LOD and put in %%pq2m-partition-number;
                                                ;LOD is partition table entry 0.

        ((vma-start-write) m+a+1 vma a-zero)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)

        ((m-q) add m-q (a-constant 64.))        ;next quantum
        ((m-j) sub m-j (a-constant 64.))        ;one less quantum to deal with
        ((m-1) add m-1 (a-constant 64.))
        (jump-xct-next quantum-map-region-loop)
       ((m-i) add m-i (a-constant 64.))         ;past this quantum
map-last-quantum-of-region                      ;like above only we also must put the size in
        (popj-less-or-equal m-j a-zero) ;just return, nothing left
        ((m-2) dpb m-1 (lisp-byte %%pq1m-page-offset) a-zero)
        ((m-2) dpb m-minus-one (lisp-byte %%pq1m-page-out-copy-first) a-2)
        ((md) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-2)
        ((m-tem) ldb (lisp-byte %%virtual-page-quantum-number) m-q)     ;quantum number
        ((m-tem) dpb m-tem (byte-field 31. 1) a-zero)   ;double it to get quantum map index
        ((vma) add m-tem (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((vma-start-write) add vma a-v-quantum-map)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((md) dpb m-j (lisp-byte %%pq2m-boot-pages-allocated) a-zero)
        ;;; now put in partition table index (it's zero).

        ((vma-start-write) m+a+1 vma a-zero)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (popj-after-next (m-q) add m-q (a-constant 64.))
       ((m-i) add m-i a-j)

;Copy one region from compressed LOD band to PAGE band.
;M-I and M-J have origin and size (on disk) of the region, where it lives in the LOD band.
;M-Q has disk address in PAGE band to copy to.
;;; (well, not really, but thats what DISK-COPY-SECTION-FROM-LOD-TO-PAGE expects.  We kludge it here to
;;; avoid a bigger kludge above.)
;M-K used to have bitmap length, now always 0
;M-S assumed to have phys memory size.
;Clobbers M-1, M-2, M-B, M-C, M-D, M-J, M-T and M-R
;On exit, M-I and M-Q are updated past this region.

DISK-RESTORE-REGION
        ((m-q) add m-q a-disk-offset)
        (call DISK-COPY-SECTION-from-lod-to-page)
        (popj-after-next (m-q) sub m-q a-disk-offset)
       (no-op)


;;; Initialize physical memory from its swapped-out image on disk.
;;; Low 3 pages, page zero, the system communication area, and
;;; the scratchpad-init-area, already in.  MICRO-CODE-SYMBOL-AREA also in since it
;;; was loaded by microcode loader.
COLD-SWAP-IN
;;; Read in the rest of wired memory (the sys comm area has its size).
;;; Don't clobber the MICRO-CODE-SYMBOL-AREA

        ((m-1) a-band-saved-crc)
        ((m-1) ldb (byte 8 16.) m-1)
        (jump-equal m-1 a-zero cold-swap-in-1)
        (jump-equal m-1 (a-constant 1) band-check-checksum)
        (call illop)
    (error-table crash "Bad CRC type code")

band-check-checksum
        ((m-1) a-band-saved-crc)
        ((m-1) ldb (byte 16. 0) m-1)
        ((m-2) a-band-crc-accumulator)
        ((m-2) ldb (byte 16. 0) m-2)
        (jump-equal m-1 a-2 cold-swap-in-1)
        ((q-r) a-processor-switches)
        (jump-if-bit-set (lisp-byte %%processor-switch-fast-boot-enable)
                         q-r cold-swap-in-1)    ;ignore checksum if fast boot
        (call illop)
    (error-table crash "BAND HAS BAD CRC")

cold-swap-in-after-disk-save
cold-swap-in-1
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-WIRED-SIZE))))
        ((M-2) VMA-PAGE-ADDR-PART READ-MEMORY-DATA)     ;Number of wired pages
        ((m-1) q-pointer m-2)   ;now is as good a time as any to set up a-page-allocation-free-pointer
        ((m-1) add m-1 (a-constant 63.))        ;set to end of wired areas rounded up to next quantum boundary
        ((a-page-allocation-free-pointer) dpb m-zero (lisp-byte 6 0) a-1)
        ((M-C) Q-POINTER READ-MEMORY-DATA)      ;Save for later, also put CCW list there
        ((M-B) (A-CONSTANT END-OF-MICRO-CODE-SYMBOL-AREA))
        ((M-1) A-DISK-OFFSET)
        ((m-1) add m-1 (a-constant end-of-micro-code-symbol-area-in-pages))
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-2) SUB M-2 (A-CONSTANT END-OF-MICRO-CODE-SYMBOL-AREA-IN-PAGES))
        (CALL COLD-DISK-READ)
;;; Set things up according to actual main memory size
        ((WRITE-MEMORY-DATA) Q-POINTER M-S (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA-START-WRITE) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-MEMORY-SIZE))))
        (ILLOP-IF-PAGE-FAULT)
;;; Now set up the table of area addresses
        (CALL GET-AREA-ORIGINS)

;;; Fancy stuff to free end of PHT and PPD is gone (old code below)
        ((m-tem) a-v-physical-page-data)
        ((m-tem) sub m-tem a-v-page-table-area)
        ((a-pht-index-limit) q-pointer m-tem)   ;Size of page hash table (get from qcom someday)
        ((md) add m-tem (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((VMA-START-WRITE) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-PAGE-TABLE-SIZE))))
        (ILLOP-IF-PAGE-FAULT)
        (call-xct-next set-pht-index-mask)
       ((m-1) a-pht-index-limit)

        ((md) a-zero)           ;Fill PHT with zero.
        ((vma) a-v-physical-page-data)          ;area after page-table-area
COLD-REINIT-PHT-2
        ((VMA-START-WRITE) SUB VMA (A-CONSTANT 1))      ;last word of PAGE-TABLE-AREA
        (ILLOP-IF-PAGE-FAULT)
        (JUMP-GREATER-THAN VMA A-V-PAGE-TABLE-AREA COLD-REINIT-PHT-2)

;;; Initialize physical-page-data.  First make it all completely null.
        ((md) m-minus-one)
        ((VMA) A-V-ADDRESS-SPACE-MAP)   ;area after PHYSICAL-PAGE-DATA
COLD-REINIT-PPD-0
        ((VMA-START-WRITE) SUB VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        (JUMP-GREATER-THAN VMA A-V-PHYSICAL-PAGE-DATA COLD-REINIT-PPD-0)

;;; Make magic PHYSICAL-PAGE-DATA entries for the wired pages and
;;; free entries in PPD and PHT for the available main memory.
;;; M-J has the upper-bound address of the PHT.  M-I gets same for PPD.
        ((M-1) VMA-PAGE-ADDR-PART M-S)          ;Number of pages of main memory
        ((M-R) A-V-RESIDENT-SYMBOL-AREA)        ;Address doing
        ((M-K) ADD M-S A-R)                     ;Size of memory
        ((M-C) a-v-page-table-area)             ;Address for filling in PHT
        ;;(by the way, we don't use the zeroth entry in the PHT now)
COLD-REINIT-PPD-1
        (jump-less-than M-R A-V-REGION-MOBY-BITS-ARRAY COLD-REINIT-PPD-4)       ;jump if wired
        ((VMA M-C) add M-C (A-CONSTANT 4))              ;Put in a PHT entry for free page
        (CALL-XCT-NEXT XCPPG1)                          ;Create physical page
       ((C-PDL-BUFFER-POINTER-PUSH) M-R)                ;At this address
COLD-REINIT-PPD-4
        ((M-R) ADD M-R (A-CONSTANT (EVAL PAGE-SIZE)))
        (JUMP-LESS-THAN M-R A-K COLD-REINIT-PPD-1)
        (call initialize-quantum-map-for-real)  ;yes, the moment you've all been waiting for
        (JUMP BEG0000)


;;;; Reinitialize the page hash table to be completely empty;
;;;; permanently wired pages have no entries.
;;;; Decide the size of the PHT from the size of main memory; it should
;;;; have 4 words in it for each page of main memory (thus will be 1/2 full).
;       ((M-1) VMA-PAGE-ADDR-PART M-S)          ;Number of pages of main memory
;       ((M-1) ADD M-1 A-1 OUTPUT-SELECTOR-LEFTSHIFT-1) ;Times 4
;       ((M-1) ADD M-1 (A-CONSTANT (EVAL (1- PAGE-SIZE))))      ;Round up to multiple of page
;       ((M-1) AND M-1 (A-CONSTANT (EVAL (MINUS PAGE-SIZE))))
;       ((M-TEM) A-V-PHYSICAL-PAGE-DATA)        ;But not bigger than available space
;       ((M-TEM) SUB M-TEM A-V-PAGE-TABLE-AREA)
;       (JUMP-LESS-OR-EQUAL M-1 A-TEM COLD-REINIT-PHT-0)
;       ((M-1) A-TEM)
;COLD-REINIT-PHT-0
;       ((A-PHT-INDEX-LIMIT) M-1)               ;Size of page hash table
;       ((WRITE-MEMORY-DATA) Q-POINTER M-1 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       ((VMA-START-WRITE) (A-CONSTANT (EVAL (PLUS 400 %SYS-COM-PAGE-TABLE-SIZE))))
;       (ILLOP-IF-PAGE-FAULT)
;       ((M-J VMA) ADD M-1 A-V-PAGE-TABLE-AREA) ;Address above PHT
;       (CALL SET-PHT-INDEX-MASK)
;       ((WRITE-MEMORY-DATA) a-zero)            ;Fill PHT with zero.
;COLD-REINIT-PHT-2
;       ((VMA-START-WRITE) SUB VMA (A-CONSTANT 1))
;       (ILLOP-IF-PAGE-FAULT)
;       (JUMP-GREATER-THAN VMA A-V-PAGE-TABLE-AREA COLD-REINIT-PHT-2)
;;;; Initialize physical-page-data.  First make it all completely null.
;       ((WRITE-MEMORY-DATA) (M-CONSTANT -1))
;       ((VMA) A-V-ADDRESS-SPACE-MAP)   ;A-V-PHYSICAL-PAGE-DATA + 1
;COLD-REINIT-PPD-0
;       ((VMA-START-WRITE) SUB VMA (A-CONSTANT 1))
;       (ILLOP-IF-PAGE-FAULT)
;       (JUMP-GREATER-THAN VMA A-V-PHYSICAL-PAGE-DATA COLD-REINIT-PPD-0)
;;;; Make magic PHYSICAL-PAGE-DATA entries for the wired pages and
;;;; free entries in PPD and PHT for the available main memory.
;;;; M-J has the upper-bound address of the PHT.  M-I gets same for PPD.
;       ((M-1) VMA-PAGE-ADDR-PART M-S)                  ;Number of pages of main memory
;       ((M-I) ADD M-1 A-V-PHYSICAL-PAGE-DATA)
;       ((M-R) A-V-RESIDENT-SYMBOL-AREA)                ;Address doing
;       ((M-K) ADD M-S A-R)                             ;Size of memory
;       ((M-C) M-J)                                     ;Address for filling in PHT
;COLD-REINIT-PPD-1
;       (JUMP-GREATER-OR-EQUAL M-R A-V-REGION-MOBY-BITS-ARRAY COLD-REINIT-PPD-3)        ;free
;       (JUMP-GREATER-OR-EQUAL M-R A-V-ADDRESS-SPACE-MAP COLD-REINIT-PPD-2)     ;wired
;       (JUMP-GREATER-OR-EQUAL M-R A-I COLD-REINIT-PPD-3)       ;free part of PPD
;       (JUMP-GREATER-OR-EQUAL M-R A-V-PHYSICAL-PAGE-DATA COLD-REINIT-PPD-2)    ;wired
;       (JUMP-GREATER-OR-EQUAL M-R A-J COLD-REINIT-PPD-3)       ;free part of PHT
;COLD-REINIT-PPD-2
;       ((WRITE-MEMORY-DATA) (A-CONSTANT 177777))       ;Wired page, no PHT entry
;       ((vma) ldb (byte-field 17. 8) m-r)
;       ((VMA-START-WRITE) add vma A-V-PHYSICAL-PAGE-DATA)
;       (ILLOP-IF-PAGE-FAULT)
;       (JUMP COLD-REINIT-PPD-4)

;COLD-REINIT-PPD-3
;       ((VMA M-C) SUB M-C (A-CONSTANT 4))              ;Put in a PHT entry for free page
;       (CALL-XCT-NEXT XCPPG1)                          ;Create physical page
;       ((C-PDL-BUFFER-POINTER-PUSH) M-R)               ;At this address
;COLD-REINIT-PPD-4
;       ((M-R) ADD M-R (A-CONSTANT (EVAL PAGE-SIZE)))
;       (JUMP-LESS-THAN M-R A-K COLD-REINIT-PPD-1)
;       (JUMP BEG0000)

xwarm-boot      ;from warm-boot
        ((a-disk-busy) setz)                    ;otherwise, if we crashed because of a disk
                                                ;error - we will wait for it to finish, and
                                                ;crash again on the first try to use the disk
        (call invalidate-cons-caches)   ;try to preserve freshly consed stuff.
#+lambda((RG-MODE) ANDCA RG-MODE
                         (A-CONSTANT (PLUS (BYTE-MASK (BYTE-FIELD 1 27.)) ;disable interrupts
                                           (BYTE-MASK (BYTE-FIELD 1 30.))))) ;set 25 bit mode.
#+exp   ((mcr) andca mcr (a-constant 1_15.)) ;just disable interrupts
#+LAMBDA(call flush-cache)
#-lambda(begin-comment)
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                           m-tem beg0000)
        ((md) setz)
        ((vma) a-my-iopb-valid-flag-physical-adr)
        (call new-cold-nubus-write)
        (call initialize-share-iopb)
#-lambda(end-comment)

;cold and warm boot merge here
BEG0000
        ((pdl-buffer-pointer m-a) (a-constant 4000)) ;size of pdl buffer
CLEAR-PDL-BUFFER-AGAIN
        ((C-PDL-BUFFER-POINTER-PUSH) M-ZERO)
        ((M-A) ADD M-A A-MINUS-ONE)
        (JUMP-NOT-EQUAL M-A A-ZERO CLEAR-PDL-BUFFER-AGAIN)
        (call store-0-in-pointer-acs)
        ((M-FLAGS) (A-CONSTANT (PLUS            ;RE-INITIALIZE ALL FLAGS
                (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                (BYTE-VALUE M-CAR-SYM-MODE 1)
                (BYTE-VALUE M-CAR-NUM-MODE 0)
                (BYTE-VALUE M-CDR-SYM-MODE 1)
                (BYTE-VALUE M-CDR-NUM-MODE 0)
                (BYTE-VALUE M-DONT-SWAP-IN 0)
                (BYTE-VALUE M-TRAP-ENABLE 0)    ;MACROCODE WILL TURN ON TRAPS WHEN READY
                (BYTE-VALUE M-MAR-MODE 0)
                (BYTE-VALUE M-PGF-WRITE 0)
                (BYTE-VALUE M-INTERRUPT-FLAG 0)
                (BYTE-VALUE M-SCAVENGE-FLAG 0)
                (BYTE-VALUE M-TRANSPORT-FLAG 0)
                (BYTE-VALUE M-STACK-GROUP-SWITCH-FLAG 0)
                (BYTE-VALUE M-DEFERRED-SEQUENCE-BREAK-FLAG 0)
                (BYTE-VALUE M-METER-STACK-GROUP-ENABLE 0)
                ;Presumably, these are what is wanted here, but they cause lossage in a
                ;very big way (will crash both sides of a 2X2 when one is booted.)
                ;(byte-value m-trap-on-calls 0)
                ;(byte-value m-enable-store-unreconciled 0)
                )))
        ((M-SB-SOURCE-ENABLE) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((A-TV-CURRENT-SHEET) A-V-NIL)          ;Forget this cache
        ((A-LEXICAL-ENVIRONMENT) A-V-NIL)       ;At top level wrt lexical bindings.
  ;     ((A-AMEM-EVCP-VECTOR) A-V-NIL)          ;Don't write all over memory
        ((A-MOUSE-CURSOR-STATE) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))  ;Mouse off
        (call-xct-next reset-scavenger)
       ((pdl-push) a-scavenge-region)
        ((A-GC-SWITCHES) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((A-INHIBIT-SCHEDULING-FLAG) A-V-TRUE)  ;DISABLE SEQUENCE BREAKS
        ((A-INHIBIT-SCAVENGING-FLAG) A-V-TRUE)  ;GARBAGE COLLECTOR NOT TURNED ON UNTIL LATER
        ((A-PAGE-TRACE-PTR) SETZ)               ;SHUT OFF PAGE-TRACE
        ((A-METER-GLOBAL-ENABLE) A-V-NIL)       ;Turn off metering
        ((A-METER-DISK-COUNT) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL RESET-MACHINE)                    ;Reset and turn on interrupts
        (CALL INITIAL-MAP)                      ;set up map
        ((VMA-START-READ) (A-CONSTANT 1031))    ;FETCH MISCELLANEOUS SCRATCHPAD LOCS
        (ILLOP-IF-PAGE-FAULT)
        ((A-AMCENT) Q-TYPED-POINTER READ-MEMORY-DATA)
        ((VMA-START-READ) (A-CONSTANT 1021))
        (ILLOP-IF-PAGE-FAULT)
        ((A-CNSADF) Q-TYPED-POINTER READ-MEMORY-DATA)
        ((A-BACKGROUND-CONS-AREA) A-CNSADF)
        ;; Initially don't hack the extra-pdl area.
        ;; The setup of A-FLOATING-ZERO depends on this,
        ;; as well as possibly other things.
        ((A-NUM-CNSADF) Q-TYPED-POINTER READ-MEMORY-DATA)
        (call invalidate-cons-caches)
        (CALL GET-AREA-ORIGINS)
        ((M-K) SUB M-ZERO (A-CONSTANT 200))     ;FIRST 200 MICRO ENTRIES ARE NOT IN TABLE
        ((A-V-MISC-BASE) ADD M-K A-V-MICRO-CODE-SYMBOL-AREA)
        ;; Clear the unused pages of the PHT and PPD out of the map
        ((MD) A-V-PHYSICAL-PAGE-DATA)
        ((MD) ADD MD A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH)
        ((MD) IOR MD (A-CONSTANT 377))
        ((MD) ADD MD (A-CONSTANT 1))            ;First page above PPD
        (JUMP-GREATER-OR-EQUAL MD A-V-REGION-ORIGIN BEGCM2)
BEGCM1
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) A-ZERO)
  ;     (NO-OP)
        ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))
        (JUMP-LESS-THAN MD A-V-REGION-ORIGIN BEGCM1)
BEGCM2  ((MD) A-V-PAGE-TABLE-AREA)
        ((MD) ADD MD A-PHT-INDEX-LIMIT)
        (JUMP-GREATER-OR-EQUAL MD A-V-PHYSICAL-PAGE-DATA BEGCM4)
BEGCM3
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) A-ZERO)
  ;     (NO-OP)
        ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))
        (JUMP-LESS-THAN MD A-V-PHYSICAL-PAGE-DATA BEGCM3)
(begin-comment) (end-comment)
BEGCM4
;set up volatility bits in L1 for all areas that exist,
;and in L2 for all fixed pages.
;(we assume we haven't taken a page fault yet.)

;;; Initialize the l1 map volatility bits to the volatilities of the corresponding
;;; regions.  Note that the l1 map extra-pdl meta bits (and the valid bit) are set up later.
initialize-l1-map-volatility
        ((m-k) m-zero)
initialize-l1-map-volatility-loop
        (call-xct-next xrgn)
       ((pdl-push) m-k)
     ;; XRGN returns NIL for unassigned addresses.  We plow ahead, assuming the low 25 bits is
     ;; the region number, which effectively sets up unused l1 map entries to have the same
     ;; volatility as region 0, which is volatility 0, which is OK.
        ((vma-start-read) add m-t a-v-region-bits)
        (illop-if-page-fault)
        ((m-tem1) ldb (lisp-byte %%region-volatility) md)
        ((md) m-k)
        ((m-tem) l1-map)
        ((#+lambda l1-map
          #+exp vma-write-l1-map) dpb m-tem1 map1-volatility a-tem)
        ((m-k) add m-k (a-constant (eval (* 400 32.))))
        (jump-less-than m-k (a-constant (eval (^ 2 25.))) initialize-l1-map-volatility-loop)

;;; Volatility bits in wired l2 maps don't get set up by the normal mechanisms, so we do it here.
initialize-l2-map-volatility
        ((vma-start-read) (a-constant (plus 400 (eval %sys-com-wired-size))))
        (illop-if-page-fault)
        ((m-a) q-pointer md)
initialize-l2-map-volatility-loop
        ((m-a) add m-a (a-constant -400))
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number m-a)
        ((md) m-a)
     ;; Invert volatility here since it's going into the L2 map.  Wait for maps.
        ((m-k) xor m-tem (a-constant 3))
        ((m-tem) l2-map-control)
#+lambda(jump-greater-than-xct-next m-a a-zero initialize-l2-map-volatility-loop)
#+lambda((l2-map-control) dpb m-k map2c-volatility a-tem)
#+exp   ((m-tem) dpb m-k map2c-volatility a-tem)
#+exp   ((vma-write-l2-map-control) dpb m-tem exp-map2c-volatility a-tem)
#+exp   (jump-greater-than m-a a-zero initialize-l2-map-volatility-loop)

        ;; Get A-INITIAL-FEF, A-QTRSTKG, A-QCSTKG, A-QISTKG
        ((VMA) (A-CONSTANT 777))                ;SCRATCH-PAD-INIT-AREA MINUS ONE
        ((M-K) (A-CONSTANT (A-MEM-LOC A-SCRATCH-PAD-BEG))) ;FIRST A MEM LOC TO BLT INTO
BEG03   ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ((OA-REG-LOW) DPB M-K OAL-A-DEST A-ZERO)        ;DESTINATION
        ((A-GARBAGE) READ-MEMORY-DATA)
        (JUMP-NOT-EQUAL-XCT-NEXT M-K (A-CONSTANT (A-MEM-LOC A-SCRATCH-PAD-END)) BEG03)
       ((M-K) ADD M-K (A-CONSTANT 1))
        ((VMA-START-READ) A-INITIAL-FEF)        ;INDIRECT
        (CHECK-PAGE-READ)
        ;; Don't let garbage pointer leak through DISK-RESTORE
        ;; There are a lot of these, we only get the ones that are known to cause trouble
        ((A-SELF) DPB Q-ALL-BUT-TYPED-POINTER M-ZERO A-V-NIL)
        ((a-self-mapping-table) DPB Q-ALL-BUT-TYPED-POINTER M-ZERO A-V-NIL)
        ((a-method-search-pointer) DPB Q-ALL-BUT-TYPED-POINTER M-ZERO A-V-NIL)
        ((a-sg-calling-args-pointer) DPB Q-ALL-BUT-TYPED-POINTER M-ZERO A-V-NIL)
        ((A-SG-PREVIOUS-STACK-GROUP) A-V-NIL)
        (DISPATCH TRANSPORT READ-MEMORY-DATA)
        ((A-INITIAL-FEF) READ-MEMORY-DATA)

        (CALL-XCT-NEXT SG-LOAD-STATIC-STATE)    ;INITIALIZE PDL LIMITS ETC
       ((A-QCSTKG) A-QISTKG)                    ;FROM INITIAL STACK-GROUP
        ((A-QLBNDP) ADD (M-CONSTANT -1) A-QLBNDO) ;INITIALIZE BINDING PDL POINTER
                        ; POINTS AT VALID LOCATION, OF WHICH THERE ARENT ANY YET.
        ((A-PDL-BUFFER-HEAD) A-ZERO)
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) A-QLPDLO)
        ((PDL-BUFFER-POINTER) A-PDL-BUFFER-HEAD)
        ((A-PDL-BUFFER-HIGH-WARNING) (A-CONSTANT PDL-BUFFER-HIGH-LIMIT))  ;INITAL STACK
                                        ;HAD BETTER AT LEAST BIG ENUF FOR P.B.
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL XFLOAT)
        ((A-FLOATING-ZERO) M-T)
        ((C-PDL-BUFFER-POINTER) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX))) ;THIS GOES
                                        ;INTO 0@P
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((M-A C-PDL-BUFFER-POINTER-PUSH) A-INITIAL-FEF)
        (check-data-type-call-not-equal m-a m-k dtp-fef-pointer illop)
  (error-table crash "Lisp-Top-Level pointer is not a FEF")
        ((M-AP) PDL-BUFFER-POINTER)
        ((m-fef) m-a)
        ((M-PDL-BUFFER-ACTIVE-QS) (A-CONSTANT 4))
        ((VMA-START-READ) M-A)
        (CHECK-PAGE-READ)
        (DISPATCH TRANSPORT-HEADER READ-MEMORY-DATA)
        ((C-PDL-BUFFER-POINTER-PUSH)
                (LISP-BYTE %%FEFH-PC) READ-MEMORY-DATA) ;temporarily save initial PC
BEG06
#+LAMBDA(CALL-NOT-EQUAL MICRO-STACK-PNTR-AND-DATA       ;CLEAR THE MICRO STACK PNTR (TO -1)
                        (A-CONSTANT (PLUS 377_24. 1 (I-MEM-LOC BEG06))) BEG06)
#+EXP   (CALL-NOT-EQUAL MICRO-STACK-PNTR        ;CLEAR THE MICRO STACK PNTR (TO -1)
                        (A-CONSTANT 77) BEG06)
        ((MICRO-STACK-DATA-PUSH) A-MAIN-DISPATCH)       ;PUSH MAGIC RETURN
   ;store 4 in L1-MAP-META-BITS for pages corresponding to EXTRA-PDL-AREA.
        ((MD) A-V-EXTRA-PDL-AREA)
BEG-EX-PDL-0
        ((M-B) L1-MAP)
        ((#+lambda L1-MAP
          #+exp vma-write-l1-map) DPB M-minus-one map1-volatility-invalid A-B)
        ((MD) ADD MD (A-CONSTANT 20000))
        (JUMP-LESS-THAN MD A-V-MICRO-CODE-ENTRY-AREA BEG-EX-PDL-0)  ;follows ex-pdl-area.
        ((M-C) A-V-MICRO-CODE-ENTRY-AREA)
        ((M-C) LDB (BYTE-FIELD 13. 0) M-C A-ZERO)
        (CALL-NOT-EQUAL M-C A-ZERO ILLOP)       ;EXTRA-PDL-AREA MUST END ON A LVL-1 MAP
                                                ;BOUNDARY.
    (error-table crash "Extra-Pdl-Area does not end of map boundary")

#+lambda(call load-constants-page)
#+lambda(call read-ucode-into-ucode-paging-area)
        ((m-array-pointer) invalidate-array-cache m-zero)
        (jump boot-exit-and-turn-pagable-ucode-on)

#-lambda(begin-comment)
load-constants-page  ;load constants page into A-MEM 2200-2277.  This allows us to save time
     ;referencing them.  Note we do not transport.  This is because there can be garbage
     ;left around after the active area.  This should be fixed in cold-load generater.
     ;for now, its OK because there are no "pointer" constants.
        ((m-1) (a-constant 0))
lcp1    ((vma-start-read) add m-1 a-v-constants-area)
        (check-page-read)
        ((oa-reg-low) dpb m-1 oal-a-dest-6-bits (a-constant (byte-value oal-a-dest 2200)))
        ((a-garbage) q-typed-pointer md)
        (jump-less-than-xct-next m-1 (a-constant 77) lcp1)
       ((m-1) add m-1 (a-constant 1))
        (popj)
#-lambda(end-comment)

store-0-in-pointer-acs
        ((m-1) (a-constant (m-mem-loc m-zr)))
sz-0    ((oa-reg-low) dpb m-1 oal-m-dest a-zero)
        ((m-garbage) (a-constant (byte-value q-data-type dtp-fix)))
     (jump-not-equal-xct-next m-1 (a-constant (m-mem-loc m-k)) sz-0)
       ((m-1) add m-1 (a-constant 1))
        (popj)

;;; Lowest level disk routines.
;;; Read or write sequence of blocks from core,
;;; copy contiguous range of blocks from disk to disk.

;The CCW buffer must be above all wired storage in the load because COLD-SWAP-IN
; swaps in all wired storage in disk ops using the CCW buffer.
; A-V-VIRTUAL-PAGE-VOLATILITY is the last wired area.

;WORDS-TO-DIRECT-MAP-DURING-BOOTSTRAP must be above CCW blocks to avoid page
; faults in disk-routines.

(ASSIGN COPY-BUFFER-CCW-PAGE-ORIGIN 600.)
(ASSIGN COPY-BUFFER-CCW-ORIGIN (eval (* 600. 400)))     ;above * page-size
(ASSIGN COPY-BUFFER-CCW-BLOCK-LENGTH 200)  ;on lambda, also limited by available
                ;multibus -> nubus mapping pages.  (1000 on CADR).
(ASSIGN COPY-BUFFER-ORIGIN (eval (* 602. 400)))

;Copy one sequence of disk blocks into another.
;M-I and M-J now have the start and size of the sequence to be copied from.
;M-Q has the start of the sequence to be copied into.
;M-S has the size of main memory (in words)
;On exit, M-I and M-Q are incremented past the block transfered, and M-J is zero.
;Clobbers M-B, M-C, M-D, M-T, M-1, M-2.

;Uses all of memory starting at COPY-BUFFER-ORIGIN.
;The two pages starting at COPY-BUFFER-CCW-PAGE-ORIGIN
;are used for disk CCWs, allowing transfer of up to 512. pages (128k words) at a time.

DISK-COPY-SECTION-from-lod-to-page
;Here M-I, M-Q and M-J are as updated for blocks already transfered.
        (POPJ-EQUAL M-J A-ZERO)                 ;If done.
;M-D gets max # blocks we can transfer at once.
        ((M-D) SUB M-S (A-CONSTANT COPY-BUFFER-ORIGIN)) ;memory not used for buffer
        ((M-D) VMA-PHYS-PAGE-ADDR-PART M-D)
;Copy at most 1000 pages at a time since that is size of 2-page command list
        (JUMP-LESS-THAN M-D (A-CONSTANT COPY-BUFFER-CCW-BLOCK-LENGTH) DISK-COPY-PART-l-to-p-2)
        ((M-D) (A-CONSTANT COPY-BUFFER-CCW-BLOCK-LENGTH))
DISK-COPY-PART-l-to-p-2
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-J A-D DISK-COPY-PART-l-to-p-3)
       ((M-2) M-D)                              ;Number to do this time
        ((M-2) M-J)
DISK-COPY-PART-l-to-p-3
        ((M-D) M-2)
        ((M-B) (A-CONSTANT COPY-BUFFER-ORIGIN)) ;First page to use as buffer
        ((M-1) M-I)                             ;Read some in
        ((m-tem) a-disk-lod-partition-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))     ;CCW list address
        (CALL COLD-DISK-READ)

;       ((m-1) (a-constant copy-buffer-origin))
;       ((m-2) dpb m-d (byte 24. 8) a-zero)
;       (call compute-region-crc)

        ((M-2) M-D)
        ((M-1) M-Q)                             ;Write some out
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))   ;CCW list address (this gets clobbered)
        ((a-band-compute-crc-flag) seto)
        (CALL COLD-DISK-WRITE)
        ((M-I) ADD M-I A-D)                     ;Advance pointers
        ((M-Q) ADD M-Q A-D)
        ((M-J) SUB M-J A-D)
        (JUMP DISK-COPY-SECTION-from-lod-to-page)

DISK-COPY-SECTION-from-page-to-lod
;Here M-I, M-Q and M-J are as updated for blocks already transfered.
        (POPJ-EQUAL M-J A-ZERO)                 ;If done.
;M-D gets max # blocks we can transfer at once.
        ((M-D) SUB M-S (A-CONSTANT COPY-BUFFER-ORIGIN)) ;memory not used for buffer
        ((M-D) VMA-PHYS-PAGE-ADDR-PART M-D)
;Copy at most 1000 pages at a time since that is size of 2-page command list
        (JUMP-LESS-THAN M-D (A-CONSTANT COPY-BUFFER-CCW-BLOCK-LENGTH) DISK-COPY-PART-p-to-l-2)
        ((M-D) (A-CONSTANT COPY-BUFFER-CCW-BLOCK-LENGTH))
DISK-COPY-PART-p-to-l-2
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT M-J A-D DISK-COPY-PART-p-to-l-3)
       ((M-2) M-D)                              ;Number to do this time
        ((M-2) M-J)
DISK-COPY-PART-p-to-l-3
        ((M-D) M-2)
        ((M-B) (A-CONSTANT COPY-BUFFER-ORIGIN)) ;First location to use as buffer
        ((M-1) M-I)                             ;Read some in
        ((m-tem) a-disk-page-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))     ;CCW list address
        (CALL COLD-DISK-READ)
        ((M-2) M-D)
        ((M-1) M-Q)                             ;Write some out
        ((m-tem) a-disk-lod-partition-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-C) (A-CONSTANT COPY-BUFFER-CCW-ORIGIN))   ;CCW list address (this gets clobbered)
        ((a-band-compute-crc-flag) seto)
        (CALL COLD-DISK-WRITE)
        ((M-I) ADD M-I A-D)                     ;Advance pointers
        ((M-Q) ADD M-Q A-D)
        ((M-J) SUB M-J A-D)
        (JUMP DISK-COPY-SECTION-from-page-to-lod)

COLD-DISK-WRITE
        ((VMA) A-DISK-RUN-LIGHT)
        ((WRITE-MEMORY-DATA) Q-POINTER (M-CONSTANT 0))
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 2))      ;Turn off run bar
        (check-page-write)
        (call reset-watchdog)
        ((M-T) (A-CONSTANT DISK-WRITE-COMMAND))
;;; M-B starting main memory ADDRESS.  Must be on page boundary. Used to be page frame number.
;;; M-1 starting disk address
;;; M-2 number of pages to transfer
;;; M-T command
;;; M-C address of CCW list.

COLD-RUN-DISK
        ((a-cold-run-disk-1) m-1) ;save disk address
        ((a-cold-run-disk-2) m-2) ;save n pages
        ((a-band-crc-tem) m-2)
        ((a-cold-run-disk-t) m-t) ;save command
        ((a-cold-run-disk-first-b) m-b) ;original M-B to restore later
        ((a-cold-run-disk-b) ldb (byte-field 24. 8) m-b a-zero)  ;get page frame number.
cold-run-disk-1
        ((m-b) a-cold-run-disk-first-b)
        (popj-equal m-zero a-cold-run-disk-2) ;down to 0 pages, done
        ((m-1) a-cold-run-disk-1)
        ((m-2) a-cold-run-disk-2)
        (jump-less-than m-2 a-number-of-data-mapping-registers-for-disk cold-run-disk-2)
        ((m-2) a-number-of-data-mapping-registers-for-disk)
cold-run-disk-2
        ((m-t) a-cold-run-disk-2)
        ((a-cold-run-disk-2) sub m-t a-2) ;we need to do M-2 less pages next time
        ((a-cold-run-disk-1) add m-2 a-cold-run-disk-1) ; ...and starting M-2 pages farther in
        ((m-b) a-cold-run-disk-b)
        ((a-cold-run-disk-b) add m-b a-2)       ;and bump the core address by M-2
        ((m-t) a-cold-run-disk-t)
        (CALL START-DISK-N-PAGES)
        (jump-equal m-zero a-band-compute-crc-flag cold-run-disk-3)

        ((m-1) a-cold-run-disk-first-b)
        ((m-2) a-band-crc-tem)
        ((m-2) dpb m-2 (byte 24. 8) a-zero)
        (call compute-region-crc)               ;may clobber M-B, M-1, M-2, or M-T
        ((a-band-compute-crc-flag) setz)

cold-run-disk-3
        (call cold-await-disk)
        ((m-c) a-disk-clp)                      ;restore M-C to where it was before
                                                ;start-disk-n-pages
        (jump cold-run-disk-1)

cold-await-disk
        ((A-DISK-SAVE-PGF-A) M-A)
        ((A-DISK-SAVE-PGF-B) M-B)
        (CALL AWAIT-DISK)
        (POPJ-AFTER-NEXT (M-B) A-DISK-SAVE-PGF-B)
       ((M-A) A-DISK-SAVE-PGF-A)

COLD-DISK-READ-1                                ;1 page read
        ((M-2) (A-CONSTANT 1))
        ((M-C) (A-CONSTANT #+lambda 777 #+exp 776))
COLD-DISK-READ
        ((VMA) A-DISK-RUN-LIGHT)
        ((WRITE-MEMORY-DATA) (M-CONSTANT -1))
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 2))      ;Turn on run bar
        (check-page-write)
        (call reset-watchdog)
        ((M-T) (A-CONSTANT DISK-READ-COMMAND))
        (JUMP COLD-RUN-DISK)


#-lambda (begin-comment)
READ-UCODE-INTO-UCODE-PAGING-AREA
;       ;CALLED WHEN JUST READY TO START LISP.
;       ;READ I-MEM SECTION OF LMC FILE INTO MICRO-CODE-PAGING-AREA
;       ;LMC PARITION START IN A-DISK-UCODE-PARTITION-START
        ((m-array-pointer) a-disk-ucode-partition-start)
        ((m-array-header) (a-constant 400))

;This "cut-down" version of the PROM ucode reads only the I-MEM section of the
;LMC file into MICRO-CODE-PAGING-AREA.  It assumes the I-MEM section comes first
;in the LMC file.
;;; Process one section.  Each section starts with three words:
;;; The section type, the initial address, and the number of locations.
;;; These are gotten into M-ARRAY-ORIGIN, M-ARRAY-LENGTH, and M-ARRAY-RANK; then the section type is
;;; "dispatched" on.
;;; Section codes are:
;;; 1 = I-MEM, 2 = D-MEM (obsolete), 3 = MAIN-MEM, 4 = A-M-MEM, 5 MACRO-INSTRUCTION-DECODE

PROCESS-SECTION
        (CALL GET-NEXT-WORD)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-ORIGIN) M-A)                   ;gets section code (first word)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-LENGTH) M-A)                   ;gets starting adr (second word)
        ((M-ARRAY-RANK) M-A)                    ;size (third word)
        (JUMP-EQUAL M-ARRAY-ORIGIN (A-CONSTANT 1) process-i-mem-section)
        (jump-equal m-array-origin (a-constant 3) process-main-mem-section)
        (jump-equal m-array-origin (a-constant 4) process-a-mem-section)
        (jump-equal m-array-origin (a-constant 5) process-mid-section)
        (call illop)
    (error-table crash "Bad section type in microcode partition")

PROCESS-I-MEM-SECTION
        (JUMP-NOT-EQUAL M-ARRAY-LENGTH A-ZERO ILLOP)    ;ERROR-BAD-ADDRESS
    (error-table crash "Bad address for I-MEM in microcode partition")
PROCESS-I-MEM-SECTION-0
        (popj-equal m-array-rank a-zero)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-RANK) SUB M-ARRAY-RANK (A-CONSTANT 1))
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-ORIGIN) M-A)
        ;;; Now the first word of the instruction is in M-ARRAY-ORIGIN, second word is in M-A,
        ;;; and the address in I-MEM is in M-ARRAY-LENGTH.
        ((md) m-array-origin)
        ((vma) add m-array-length a-array-length)
        ((vma-start-write) add vma a-v-micro-code-paging-area)
        (check-page-write-no-interrupt)
        ;store second word
        ((md) m-a)
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write-no-interrupt)
        (JUMP-XCT-NEXT PROCESS-I-MEM-SECTION-0)
       ((M-ARRAY-LENGTH) ADD M-ARRAY-LENGTH (A-CONSTANT 1))

process-main-mem-section
        (call get-next-word)
        ;;; M-array-length (m-c)/ Number of blocks.
        ;;; M-array-rank (m-d)/ Address of first block, relative to beginning of partition.
        ;;; M-A/ Physical memory address of first word.
        ((M-array-origin) ADD M-array-rank a-disk-ucode-partition-start)
MAIN-MEM-LOOP
        (JUMP-EQUAL m-array-length A-ZERO PROCESS-SECTION)
        ((m-1) m-array-origin) ;disk address
        ((m-tem) a-disk-ucode-partition-unit)
        ((m-1) dpb m-tem (byte 8 24.) a-1)
        ((m-b) m-a)
        (CALL cold-DISK-READ-1)

        ((M-array-origin) ADD m-array-origin (a-constant 1))
        ((M-A) ADD M-A (A-constant 400))
        (JUMP-XCT-NEXT MAIN-MEM-LOOP)
       ((M-array-length) SUB M-array-length (A-constant 1))


process-d-mem-section
process-t-mem-section
process-mid-section
        (jump-equal m-array-rank a-zero process-section)
        (call get-next-word)
        ((m-array-rank) sub m-array-rank (a-constant 1))
        (jump process-d-mem-section)

process-a-mem-section
        (popj-equal m-array-rank a-zero)
        (call get-next-word)
        ((m-array-rank) sub m-array-rank (a-constant 1))
        (jump process-a-mem-section)

GET-NEXT-WORD
; Get the next word of the MICR partition into M-A.  M-ARRAY-POINTER
; contains the number of the next page to be read in from it.
; M-ARRAY-HEADER has the relative address of the next word within disk buffer.
; Clobbers M-TEMP-1, M-TEMP-2, Q-R.
        (JUMP-GREATER-OR-EQUAL m-array-header (A-CONSTANT 400) GET-NEXT-PAGE)
        ((VMA-START-READ) add m-array-header a-v-wired-disk-buffer)
        (CALL-IF-PAGE-FAULT ILLOP)
        (POPJ-AFTER-NEXT (M-A) READ-MEMORY-DATA)
       ((M-ARRAY-HEADER) ADD M-ARRAY-HEADER (a-constant 1))

GET-NEXT-PAGE
        ((M-ARRAY-HEADER) SETZ)
        ((M-1) M-ARRAY-POINTER)         ;DISK ADDRESS
        ((m-tem) a-disk-ucode-partition-unit)
        ((m-1) dpb m-tem (byte-field 8 24.) a-1)
        ((M-B) A-V-WIRED-DISK-BUFFER)
        (CALL-XCT-NEXT COLD-DISK-READ-1)
       ((M-A) SETZ)
        (JUMP-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-POINTER) M+A+1 M-ARRAY-POINTER A-ZERO)
#-lambda (end-comment)

#-exp (begin-comment)
Setup-Microcode-Symbol-Area
;;; Called right away to copy the main memory section of the microcode into
;;; the mirocode symbol area before the main memory section gets wiped out.

;;; This "cut-down" version of the PROM ucode reads only the main memory section
;;; of the EMC file into the MICRO-CODE-SYMBOL-AREA

;;; Process one section.  Each section starts with three words:
;;; The section type, the initial address, and the number of locations.
;;; These are gotten into M-ARRAY-ORIGIN, M-ARRAY-LENGTH, and M-ARRAY-RANK; then the section type is
;;; "dispatched" on.
;;; Section codes are:
;;; 1 = I-MEM, 2 = D-MEM (obsolete), 3 = MAIN-MEM, 4 = A-M-MEM, 5 MACRO-INSTRUCTION-DECODE

        ((m-array-pointer) seta (field a-source-multiplier 1772))
                ; Random offset used by the Explorer boot PROM
        ((m-array-pointer) add m-array-pointer (a-constant #x80000))

;first throw out the stupid version number section
        (call get-next-word)
        (call get-next-word)
        (call get-next-word)
        (call get-next-word)
        (call get-next-word)
PROCESS-SECTION
        (CALL GET-NEXT-WORD)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-ORIGIN) M-A)                   ;gets section code (first word)
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-LENGTH) M-A)                   ;gets starting adr (second word)
        ((M-ARRAY-RANK) M-A)                    ;size (third word)
        (JUMP-EQUAL M-ARRAY-ORIGIN (A-CONSTANT 1) process-i-mem-section)
        (jump-equal m-array-origin (a-constant 2) process-d-mem-section)
        (jump-equal m-array-origin (a-constant 3) process-main-mem-section)
        (jump-equal m-array-origin (a-constant 4) process-a-mem-section)
        (jump-equal m-array-origin (a-constant 5) process-t-mem-section)
        (call illop)
   (error-table crash "Bad section type in microcode partition")

PROCESS-I-MEM-SECTION
        (JUMP-NOT-EQUAL M-ARRAY-LENGTH A-ZERO ILLOP)    ;ERROR-BAD-ADDRESS
    (error-table crash "Bad I-MEM address in microcode partition")
PROCESS-I-MEM-SECTION-0
        (jump-EQUAL M-ARRAY-RANK A-ZERO process-section)        ;count exhausted, done
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-RANK) SUB M-ARRAY-RANK (A-CONSTANT 1))
        (CALL-XCT-NEXT GET-NEXT-WORD)
       ((M-ARRAY-ORIGIN) M-A)
        (JUMP-XCT-NEXT PROCESS-I-MEM-SECTION-0)
       ((M-ARRAY-LENGTH) ADD M-ARRAY-LENGTH (A-CONSTANT 1))

process-main-mem-section
        (call get-next-word)
        ;;; M-array-length (m-c)/ Number of blocks.
        ;;; M-array-rank (m-d)/ Address of first block, relative to beginning of partition.
        ;;; M-A/ Physical memory address of first word.
        ((m-array-rank) dpb m-array-rank (byte-field 22. 10.) a-zero)   ; convert from blocks to bytes
        ((m-array-rank) add m-array-rank (field a-source-multiplier 1772))
        ((m-array-rank) add m-array-rank (a-constant #x80000))
        ((m-array-origin) dpb m-a (byte-field 22. 2) (a-constant #xF4000000))
        ((M-Array-Length) dpb m-array-length (byte-field 24. 8.) a-zero)        ; convert from blocks to words
MAIN-MEM-LOOP
        (jump-equal m-array-length a-zero process-section)
                ; read word
        (call-xct-next nubus-read)
       ((vma) m-array-rank)
        ((m-array-rank) add m-array-rank (a-constant 4))
                ; write word
        (call-xct-next nubus-write)
       ((vma) m-array-origin)
        ((m-array-origin) add m-array-origin (a-constant 4))
        (jump-xct-next main-mem-loop)
       ((m-array-length) sub m-array-length (a-constant 1))

process-d-mem-section
process-t-mem-section
process-mid-section
        (jump-equal m-array-rank a-zero process-section)
        (call get-next-word)
        ((m-array-rank) sub m-array-rank (a-constant 1))
        (jump process-d-mem-section)

process-a-mem-section
        (popj-equal m-array-rank a-zero)
        (call get-next-word)
        ((m-array-rank) sub m-array-rank (a-constant 4))
        (jump process-a-mem-section)

GET-NEXT-WORD
        (call-xct-next nubus-read)
       ((vma) m-array-pointer)
        (popj-after-next (m-array-pointer) add m-array-pointer (a-constant 4))
       ((m-a) md)
#-exp (end-comment)

SET-PHT-INDEX-MASK                              ;Given A-PHT-INDEX-SIZE in M-1
        ((M-2) A-ZERO)                          ;Build mask with same haulong
SET-PHT-INDEX-MASK-1
        ((M-2) M+A+1 M-2 A-2)                   ;Shift left bringing in 1
        ((M-1) (BYTE-FIELD 37 1) M-1)           ;Shift right bringing in 0
        (POPJ-AFTER-NEXT (A-PHT-INDEX-MASK) DPB M-ZERO (BYTE-FIELD 1 0) A-2) ;clear low bit
       (CALL-NOT-EQUAL M-1 (A-CONSTANT 0) SET-PHT-INDEX-MASK-1)

#-lambda(begin-comment)
COLD-GET-DISK-GEOMETRY-FROM-CMOS
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                                       360140)))
        (CALL XMULTIBUS-READ-8)
        (CALL-NOT-EQUAL M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                              144)) ILLOP)      ;d
    (error-table crash bad cmos ram in sdu)
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                                       360144)))
        (CALL XMULTIBUS-READ-8)
        (CALL-NOT-EQUAL M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                              151)) ILLOP)      ;i
    (error-table crash bad cmos ram in sdu)
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                                       360150)))
        (CALL XMULTIBUS-READ-8)
        (CALL-NOT-EQUAL M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                              163)) ILLOP)      ;s
    (error-table crash bad cmos ram in sdu)
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                                       360154)))
        (CALL XMULTIBUS-READ-8)
        (CALL-NOT-EQUAL M-T (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                              153)) ILLOP)      ;k
    (error-table crash bad cmos ram in sdu)
        ((C-PDL-BUFFER-POINTER-PUSH) (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                                                       360214))) ;sectors per track
        (CALL XMULTIBUS-READ-8)
        ((A-DISK-BLOCKS-PER-TRACK) Q-POINTER M-T)
        (POPJ)
#-lambda(end-comment)

#-lambda(begin-comment)
COLD-READ-MINI-LABEL
        (CALL COLD-GET-DISK-GEOMETRY-FROM-CMOS)
        ((A-DISK-CYLINDER-OFFSET) A-ZERO)
        ((A-DISK-BLOCKS-PER-CYLINDER) (A-CONSTANT 500.))  ;a large number for the time being
    ;   ((A-DISK-BLOCKS-PER-TRACK) (A-CONSTANT 25.))   ;so as to win..
        ((M-B) A-V-WIRED-DISK-BUFFER)                   ;core address.  0 first time, otherwise
                        ;set up.
        ((M-1) (A-CONSTANT 22.))        ;disk address
        (CALL COLD-DISK-READ-1)
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) A-V-WIRED-DISK-BUFFER)            ;buffer loc 0
        (CALL-NOT-EQUAL MD (A-CONSTANT 10223647506) ILLOP)  ;FOOB = 106 117 117 102
    (error-table crash bad check word in mini label)
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) add vma (A-CONSTANT 1))
        (CALL-NOT-EQUAL MD (A-CONSTANT 12024644514) ILLOP)  ;LISP = 114, 111, 123, 120
    (error-table crash no LISP entry in mini-label)
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) add vma (A-CONSTANT 1))
        ((A-DISK-CYLINDER-OFFSET) MD)
        (POPJ)
#-lambda(end-comment)

;;; Read the disk label and find the main load partition to be used,
;;; and the PAGE partition.  The main load to be used is either the
;;; one whose name is in M-4, or, if M-4 is 0, A-DISK-BAND-PARTITION-NAME if that
;;; is non-zero, or the current one from the label.  The paging partition now
;;; comes from a-disk-page-partition-name, which will normally be PAGE unless
;;; it was loaded differently from the configuration structure.
;;; Also set A-LOADED-BAND for later macrocode use.

;To summerize - this routine sets up
;
;  a-disk-blocks-per-track (on lambda)
;  a-disk-blocks-per-cylinder (on lambda)
;  a-disk-offset (for PAGE) (also put in M-Q)
;  a-disk-maximum (for PAGE) (also put in M-R)
;  a-loaded-ucode with dtp-fix & lower 3 bytes of ucode part name
;  a-disk-ucode-partition-start
;  a-disk-ucode-partition-length
;  a-loaded-band with dtp-fix & lower 3 bytes of LOD part name
;  m-i LOD band start
;  m-j LOD band length

#-lambda(begin-comment)
COLD-READ-LABEL
        ((M-B) a-v-wired-disk-buffer)           ;Core page 0 or wired disk buffer
        ((M-1) A-ZERO)                          ;Disk page 0
        (CALL COLD-DISK-READ-1)

 ;support for multi page labels temporarily removed.
 ; has to worry about clobbering the IOPB in sys com area in page 1.

 ;      ((M-B) (A-CONSTANT 1000))               ;Core page 2
 ;      ((M-1) (a-constant 4))                  ;Disk page 4
 ;      (CALL COLD-DISK-READ-1)
;Cannot use COLD-DISK-READ-1 next since that puts the CCW in 777
 ;      ((M-B) (A-CONSTANT 400))                ;Core page 1
 ;      ((M-1) (a-constant 2))                  ;Disk page 2
 ;      ((M-2) (A-CONSTANT 1))
 ;      ((M-C) (A-CONSTANT 170))        ;Words 170-177 in disk label not used!
 ;      (CALL COLD-DISK-READ)
        ;Location 6 contains the name of the ucode partition.
        ;Location 7 contains the name of the main load partition.
        ;Location 200 contains the partition table.
        ;We must also find the PAGE partition and set up A-DISK-OFFSET and A-DISK-MAXIMUM
        ((m-1) a-v-wired-disk-buffer)
        (CALL-XCT-NEXT PHYS-MEM-READ)           ; Read the number of blocks per track
       ((VMA) add m-1 (A-CONSTANT 4))
        ((A-DISK-BLOCKS-PER-TRACK) Q-POINTER READ-MEMORY-DATA
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-XCT-NEXT PHYS-MEM-READ)           ; Read the number of heads
       ((VMA) add m-1 (A-CONSTANT 3))
        ((Q-R) READ-MEMORY-DATA)                ; Get number of blocks per cylinder
        (CALL-XCT-NEXT MPY)                     ; Blocks/track * tracks/cylinder
       ((M-1) DPB M-ZERO Q-ALL-BUT-POINTER A-DISK-BLOCKS-PER-TRACK)
        ((A-DISK-BLOCKS-PER-CYLINDER) Q-POINTER Q-R
                        (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-XCT-NEXT COLD-FIND-PARTITION)
       ((m-3) a-disk-page-partition-name)
  ;    ((M-3) (A-CONSTANT 10521640520))         ; PAGE = 105 107 101 120 = 10521640520
        ((A-DISK-OFFSET) M-I)
        ((A-DISK-MAXIMUM) M-J)
        ((M-Q) M-I)                             ;M-Q, M-R point to PAGE partition
        ((M-R) M-J)
        ((vma) a-v-wired-disk-buffer)
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) add vma (A-CONSTANT 6))
        ((M-3) READ-MEMORY-DATA)                ;Current ucode
        (jump-equal m-zero a-disk-ucode-partition-name cold-read-label-4)
        ((m-3) a-disk-ucode-partition-name)     ;use this if it has been loaded from conf.
cold-read-label-4
        ((A-LOADED-UCODE) (BYTE-FIELD 30 10) M-3 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL COLD-FIND-PARTITION)              ;Set up M-I, M-J for partition to load.
        ((a-disk-ucode-partition-start) m-i)
        ((a-disk-ucode-partition-length) m-j)
        ((vma) a-v-wired-disk-buffer)
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) add vma (A-CONSTANT 7))
        ((M-3) READ-MEMORY-DATA)                ;Current Band
        (jump-equal m-zero a-disk-band-partition-name cold-read-label-2)
        ((m-3) a-disk-band-partition-name)      ;use this if it has been loaded from conf.
cold-read-label-2
        (JUMP-EQUAL M-4 A-ZERO COLD-READ-LABEL-1)
        ((M-3) M-4)
COLD-READ-LABEL-1
        ((A-LOADED-BAND) (BYTE-FIELD 30 10) M-3 (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL COLD-FIND-PARTITION)              ;Set up M-I, M-J for partition to load.
        (call hack-partition-table)
        (POPJ)

;;; put entries in the partition table for the LOD and PAGE bands we are using.  Eventually
;;; this and maybe cold-read-label will be rewritten to use the partition table for real
;;; for the rest of the system, but for now we just pound this stuff into here for use
;;; by the quantum map code.
;;; not that QUANTUM-MAP-REGION assumes that LOD is entry 0 of partition table
hack-partition-table
        ((a-address-space-maximum) a-disk-maximum)      ;address space size and current page partition size must be decoupled.
        ((vma) a-v-quantum-map)
        ((vma) add vma (a-constant (eval (* page-size %partition-table-offset-in-tables))))
        ;;; first the LOD band, see comment at beginning of COLD-READ-LABEL to find out how
        ;;; we know this stuff
        ((m-3) (a-constant 1))
        ((m-3) dpb m-3 (lisp-byte %%pt1-valid) a-zero)
        ;;; we assume unit number is zero since that is all that the u-code can deal with now.
        ((md-start-write) dpb (lisp-byte %%pt1-size) m-j a-3)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((vma) m+a+1 vma a-zero)
        ((md-start-write) dpb m-i (lisp-byte %%pt2-offset) a-zero)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ;;; now PAGE band
        ((vma) m+a+1 vma a-zero)
        ((md-start-write) dpb m-r (lisp-byte %%pt1-size) a-3)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((vma) m+a+1 vma a-zero)
        ((md-start-write) dpb m-q (lisp-byte %%pt2-offset) a-zero)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((vma) m+a+1 vma a-zero)                ;invalid entry to flag end of table
        ((md-start-write) a-zero)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (POPJ)

;;; With the label in buffer starting at A-V-WIRED-DISK-BUFFER, this routine finds a partition
;;; whose name is in M-3 and returns its start and size (in blocks) in M-I and M-J.
COLD-FIND-PARTITION
        ((vma) a-v-wired-disk-buffer)
        (CALL-XCT-NEXT PHYS-MEM-READ)           ;Get number of partitions
       ((VMA) add vma (A-CONSTANT 200))
        ((M-I) READ-MEMORY-DATA)
        (CALL-XCT-NEXT PHYS-MEM-READ)           ;Get words per partition
       ((VMA) ADD VMA (A-CONSTANT 1))
        ((M-J) READ-MEMORY-DATA)
        ((VMA) ADD VMA (A-CONSTANT 1))
COLD-FIND-PART-LOOP
        (CALL-EQUAL M-I A-ZERO ILLOP)           ;Out of partitions, not found, die
    (error-table crash "Can't find LOD, PAGE, or LMC partition")
        (CALL PHYS-MEM-READ)                    ;Get name of a partition
        ((M-I) SUB M-I (A-CONSTANT 1))
        (JUMP-NOT-EQUAL-XCT-NEXT READ-MEMORY-DATA A-3 COLD-FIND-PART-LOOP)
       ((VMA) ADD VMA A-J)
        ((VMA) SUB VMA A-J)
        (CALL-XCT-NEXT PHYS-MEM-READ)           ;Found it, get start and size
       ((VMA) ADD VMA (A-CONSTANT 1))
        ((M-I) READ-MEMORY-DATA)
        (CALL-XCT-NEXT PHYS-MEM-READ)
       ((VMA) ADD VMA (A-CONSTANT 1))
        (POPJ-AFTER-NEXT (M-J) READ-MEMORY-DATA)
       (NO-OP)
#-lambda(end-comment)

;;; Read the disk label and find the main load partition to be used,
;;; and the PAGE partition.  The main load to be used is either the
;;; one whose name is in M-4, or, if M-4 is 0, A-DISK-BAND-PARTITION-NAME if that
;;; is non-zero, or the current one from the label.  The paging partition now
;;; comes from a-disk-page-partition-name, which will normally be PAGE unless
;;; it was loaded differently from the configuration structure.
;;; Also set A-LOADED-BAND for later macrocode use.

;To summerize - this routine sets up
;
;  a-disk-blocks-per-track (on lambda)
;  a-disk-blocks-per-cylinder (on lambda)
;  a-disk-offset (for PAGE) (also put in M-Q)
;  a-disk-maximum (for PAGE) (also put in M-R)
;  a-loaded-ucode with dtp-fix & lower 3 bytes of ucode part name
;  a-disk-ucode-partition-start
;  a-disk-ucode-partition-length
;  a-loaded-band with dtp-fix & lower 3 bytes of LOD part name
;  m-i LOD band start
;  m-j LOD band length

#-exp
(begin-comment)
;uses page 0 the first time; then uses a-v-wired-disk-buffer,
; therefore, this never trashes any lisp memory

;when we get here, there is enough of a straight map to cover page 0
;or a-v-wired-disk-buffer
COLD-READ-LABEL

        (jump-equal m-4 a-zero cold-read-label-each-drive)
        ;;M-4 is zero the first time, but will be the name of a load
        ;; band to save into if coming from disk-save.  if so, we
        ;; don't want to re-use the old offsets for the LOD band.
        ;; on the other hand, disk-save shouldn't update the offset
        ;; and size of the PAGE band.
        ((a-disk-lod-partition-start) setz)

cold-read-label-each-drive
        ((m-b) a-v-wired-disk-buffer)   ;will be 0 the first time
        ((m-a) seta (field a-source-multiplier 1775))   ; read unit number from PROM
        ((m-a) (byte-field 6 0) m-a)
        ((a-disk-ucode-partition-unit) m-a)
        ((a-disk-lod-partition-unit) m-a)
        ((a-disk-page-unit) m-a)
        ((m-1) dpb m-a (byte-field 8 24.) (a-constant 2))       ;disk page 2 on unit in M-A
        (CALL COLD-DISK-READ-1)
        (call cold-read-label-page)
;;;     ((m-a) add m-a (a-constant 1))  ;this will have to be hairier for multiple formatters
;;;     (jump-less-than m-a (a-constant 2) cold-read-label-each-drive)
;;;     (jump-less-than m-a (a-constant 1) cold-read-label-each-drive)

        (call-equal m-zero a-disk-offset illop)
        (call-equal m-zero a-disk-ucode-partition-start illop)
        (call-equal m-zero a-disk-lod-partition-start illop)

        ((m-i) a-disk-lod-partition-start)
        ((m-j) a-disk-lod-partition-length)
        ((m-q) a-disk-offset)
        ((m-r) a-disk-maximum)
        (popj)

cold-read-label-page
        ((m-1) (a-constant 2))                  ;partition type PAGE
        ((m-b) setz)                            ;M-B = 0 means looking for PAGE band
        (call search-label-block-for-partition-type)
        (call-not-equal m-i a-zero found-page-partition)

        ((m-1) (a-constant 1))                  ;microcode
        ((m-b) (a-constant 1))                  ;M-B = 1 means looking for ucode
                                                ;name is in 1776@a
        (call search-label-block-for-partition-type)
        (call-not-equal m-i a-zero found-ucode-partition)

        ((m-1) (a-constant 0))                  ;load band
        ((m-b) (a-constant 2))                  ;M-B = 2 means looking for band,
                                                ;name is either M-4 or 1777@a
        (call search-label-block-for-partition-type)
        (call-not-equal m-i a-zero found-lod-partition)

        (popj)

found-page-partition
        (popj-not-equal m-zero a-disk-offset)
  ;dont check for partition too big here since true allowable maximum size
  ;depends on A-LOWEST-DIRECT-VIRTUAL-ADDRESS.  Instead, check against that
  ;at MAKE-REGION-CONTINUE.
        ((a-disk-maximum) m-j)
        (popj-after-next
         (a-address-space-maximum) a-disk-maximum) ;address space size and
                                   ; current page partition size must be decoupled.
       ((a-disk-offset) m-i)

found-ucode-partition
        (popj-not-equal m-zero a-disk-ucode-partition-start)
        ((vma-start-read) m-2)
        (illop-if-page-fault)
        ((a-loaded-ucode) ldb (byte-field 24. 8) md
                        (a-constant (byte-value q-data-type dtp-fix)))
        ((a-disk-ucode-partition-start) m-i)
        ((a-disk-ucode-partition-length) m-j)
        (popj)

found-lod-partition
        (popj-not-equal m-zero a-disk-lod-partition-start)
        ((a-loaded-band) ldb (byte-field 24. 8) md (a-constant (byte-value q-data-type dtp-fix)))
        ((a-disk-lod-partition-start) m-i)
        ((a-disk-lod-partition-length) m-j)
        (popj)

search-label-block-for-partition-type
        ((vma) a-v-wired-disk-buffer)
        ((vma-start-read) add vma (a-constant 2))       ;get number of partitions
        (illop-if-page-fault)
        ((m-i) md)
        ((vma) a-v-wired-disk-buffer)
        ((vma-start-read) add vma (a-constant 3))       ;get words per partition
        (illop-if-page-fault)
        ((m-j) md)

        ((m-2) a-v-wired-disk-buffer)
        ((m-2) add m-2 (a-constant 20))

search-label-block-loop
        (jump-equal m-i a-zero search-label-block-not-found)
        ((vma-start-read) add m-2 (a-constant 3))       ;get type word
        (illop-if-page-fault)
        ((m-tem) ldb (byte-field 8 0) md)
        (call-equal m-tem a-1 search-label-block-check-this-one)
        ((m-i) sub m-i (a-constant 1))
        ((m-2) add m-2 a-j)
        (jump search-label-block-loop)

search-label-block-check-this-one
        (jump-equal m-b (a-constant 0) search-label-block-for-page-partition)
        (jump-equal m-b (a-constant 1) search-label-block-for-ucode-partition)
        (jump-equal m-b (a-constant 2) search-label-block-for-band-partition)

search-label-block-for-band-partition
        (jump-equal m-zero (field a-source-multiplier 1777) search-label-check-for-default-lod-band)
        ((vma-start-read) m-2)
        (illop-if-page-fault)
        (jump-equal md (field a-source-multiplier 1777) search-label-block-really-found)
        (popj)

search-label-check-for-default-lod-band
        ((vma-start-read) add m-2 (a-constant 3))
        (illop-if-page-fault)
        (popj-if-bit-clear (byte-field 1 26.) md)
        (jump search-label-block-really-found)

search-label-block-for-ucode-partition
        (jump-equal m-zero (field a-source-multiplier 1776) search-label-check-for-default-mcr-band)
        ((vma-start-read) m-2)
        (illop-if-page-fault)
        (jump-equal md (field a-source-multiplier 1776) search-label-block-really-found)
        (popj)

search-label-check-for-default-mcr-band
        ((vma-start-read) add m-2 (a-constant 3))
        (illop-if-page-fault)
        (popj-if-bit-clear (byte-field 1 26.) md)
        (jump search-label-block-really-found)

search-label-block-for-page-partition
        ;;falls in -- just use first page partition found

search-label-block-really-found
        ((vma-start-read) add m-2 (a-constant 1))       ;partition start
        (illop-if-page-fault)
        ((m-i) md)
        ((vma-start-read) add m-2 (a-constant 2))       ;partition length
        (illop-if-page-fault)
        ((m-j) md)
        ((vma-start-read) m-2)                  ;leave name in MD
        (illop-if-page-fault)
        ((m-garbage) micro-stack-data-pop)
        (popj)

search-label-block-not-found
        ((m-i) setz)
        (popj)

#-exp(end-comment)

#-lambda(begin-comment)
RESET-MACHINE
        ((A-DISK-BUSY) M-ZERO)                  ;Forget pending disk operation
        ((m-tem1) rg-mode)
        ((rg-mode) dpb m-minus-one (byte-field 1 31) a-tem1)  ;turn on enable MISC-MID.
                ;if this isnt on it will get into a loop at MISC-PDL, etc, because the
                ;dispatch address it reads from MACRO-IR-DECODE will be MISC-PDL, etc.
        (call activate-processor-switches) ;e.g. turn on stat2 clock

        (call set-up-video-board)

#+lambda(call lambda-reset-interrupt-ram)

        (popj)

set-up-video-board
        ((m-1) a-tv-quad-slot)
        ((m-1) ldb (byte-field 8 8) m-1)
        (jump-equal m-1 (a-constant 0) init-vcmem-serial)       ;VCMEM
        (jump-equal m-1 (a-constant 1) init-vcmem-serial)       ;VCMEM
        (popj-equal m-1 (a-constant 2))         ;QUAD
        (call illop)
   (error-table crash unknown video board type)

init-vcmem-serial
;;; Set up the baud rates for the serial ports
        ((write-memory-data) (a-constant #x88))
        ((vma-start-write) (a-constant (plus 177370400 4)))
        (check-page-write-map-reload-only)

        (call-xct-next vcmem-serial-set-up-port)        ;VCMEM A port for keyboard
       ((vma) (a-constant (plus 177370400 15)))
        ;redo register 5 to turn on break bit, and keep from heating up speaker
        ((md-start-write) (a-constant #x05))
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #xFA))
        (check-page-write-map-reload-only)

        (call-xct-next vcmem-serial-set-up-port)        ;VCMEM B port for mouse
       ((vma) (a-constant (plus 177370400 17)))

        (popj)


vcmem-serial-set-up-port
        ((md-start-write) (a-constant #x00))    ;Fake out if waiting for data byte
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x18))    ;Reset port
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x01))    ;Address register 1
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x18))    ;Interrupt on all RX characters
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x03))    ;Address register 3
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #xC1))    ;8 bits/RX char, RX enable
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x04))    ;address register 4
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x84))    ;x32 clock, 1 stop bit
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x05))    ;address register 5
        (check-page-write-map-reload-only)
        (popj-after-next (md-start-write) (a-constant #xEA))
                                                ;DTR, 8 bits/TX char, TX enable, RTS
       (check-page-write-map-reload-only)

;Reset interrupt RAM.
lambda-reset-interrupt-ram
        ((RG-MODE) IOR RG-MODE (A-CONSTANT (BYTE-MASK (BYTE-FIELD 1 27.)))) ;enable interrupts
RST0    ((M-1) (A-CONSTANT 400))
RST     (JUMP-CONDITIONAL PG-FAULT-OR-INTERRUPT RESET-I-1);an "interrupt" to "service"
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-ZERO RST)
       ((M-1) SUB M-1 (A-CONSTANT 1))

 ;      ((INTERRUPT-CONTROL) DPB (M-CONSTANT -1)        ;Clear RESET, set halfword-mode,
 ;              (BYTE-FIELD 1 27.) A-ZERO)              ;and enable interrupts
 ;      ((MD) SETZ)
 ;      (CALL-XCT-NEXT PHYS-MEM-WRITE)                  ;Reset bus interface status.
 ;     ((VMA) (A-CONSTANT 17773022))                    ;Unibus loc 766044

        ((RG-MODE) IOR RG-MODE (A-CONSTANT 1_26.))   ;clear to start.
        (POPJ)          ;dont drop into INITIAL-MAP, since SYS-COM area might not be set up.

RESET-I-1
        (JUMP-XCT-NEXT RST0)            ;service interrupt and start delay over.
       ((INTERRUPT-CLEAR) A-ZERO)


serial-set-up-port
        ((md-start-write) (a-constant #x00))    ;Fake out if waiting for data byte
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x18))    ;Reset port
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x01))    ;Address register 1
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x18))    ;Interrupt on all RX characters
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x03))    ;Address register 3
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #xC1))    ;8 bits/RX char, RX enable
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x04))    ;address register 4
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x84))    ;x32 clock, 1 stop bit
        (check-page-write-map-reload-only)
        ((md-start-write) (a-constant #x05))    ;address register 5
        (check-page-write-map-reload-only)
        (popj-after-next (md-start-write) (a-constant #xEA))
                                                ;DTR, 8 bits/TX char, TX enable, RTS
       (check-page-write-map-reload-only)

#-lambda(end-comment)

#-exp (begin-comment)

RESET-MACHINE
        ((A-DISK-BUSY) M-ZERO)                  ;Forget pending disk operation


        ((md) setz)
        ((vma) (a-constant #xf6e00000))
        ((m-tem) setz)

exp-reset-interrupts
        ((vma-start-write-unmapped) vma)
        (no-op)
        ((vma) add vma (a-constant 4))
        ((m-tem) add m-tem (a-constant 1))
        (jump-less-than m-tem (a-constant 16.) exp-reset-interrupts)

        (popj)

#-exp (end-comment)

;;; routines for initializing QUANTUM MAP

;;; set all entries to zero.   there are 2 * 2^25 / (64 * page-size) words in the map
zero-quantum-map
        ((m-tem) a-v-quantum-map)
        ((m-tem) add m-tem (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((vma) add m-tem (a-constant (eval (* 2 %number-of-address-quanta))))
        ((md) a-zero)
zero-quantum-map-loop
        ((vma-start-write) sub vma (a-constant 1))
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (jump-greater-than vma a-tem zero-quantum-map-loop)
        (popj)

;;; build ccw list for disk quantum-swap-buffer.  So who cares if we write it backwards?
;;; The first page of the buffer contains the CCW list and the rest is the actual buffer
;;; which it points to.
initialize-quantum-buffer
        ((vma) a-v-quantum-map)
        ((vma) add vma (a-constant (eval (* page-size %quantum-swap-buffer-offset-in-tables))))
        ((m-tem) vma)                           ;base of the CCW list and the buffer
;       ((md) ldb vma-page-addr-part a-tem)
;       ((md) dpb vma-page-addr-part md a-zero) ;are this and preveous inst necessary
        ((md) vma)
        ((vma) add vma (a-constant 64.))
        ((vma) sub vma (a-constant 1))          ;element 63 of CCW list
        ((md-start-write) add md (a-constant (eval page-size)))  ;write last entry (which points to first
                                                ; page after CCW)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((vma) sub vma (a-constant 1))
        ((md) ior md (a-constant 1))                    ;set the NOT-LAST bit (lsb) for all others
initialize-quantum-buffer-loop
        ((md-start-write) add md (a-constant (eval page-size)))
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        (jump-greater-than-xct-next vma a-tem initialize-quantum-buffer-loop)
       ((vma) sub vma (a-constant 1))
        (popj)

;;; set up the device entries in the quantum map
quantum-map-devices

        (popj)

;;; called during early initialization to set up an interim quantum map to use.
;;; This must be called before any use of devices, the multibus or the SDU but after
;;; memory is straight mapped.
initialize-quantum-map-cold
        ((m-tem) a-processor-switches)
        (popj-if-bit-clear (lisp-byte %%processor-switch-fast-boot-enable) m-tem)
        (call-xct-next zero-quantum-map)
       ((a-v-quantum-map) (a-constant (eval cold-wired-system-tables-base)))
;       (call quantum-map-devices)
        (popj)

;;; put just before call to COLD-READ-MINI-LABEL in XDISK-RESTORE:
;       (call initialize-quantum-map-cold)

;;; called at end of cold-swap-in
initialize-quantum-map-for-real
        ((m-2) a-processor-switches)
        (popj-if-bit-clear (lisp-byte %%processor-switch-fast-boot-enable) m-2)
  ;;; first copy quantum-map and partition-table to their true home now that it's safe there.
        (call unhack-quantum-map-base)
        ((m-2) a-v-quantum-map)
        ((m-1) (a-constant (eval cold-wired-system-tables-base)))
        ((m-tem) a-zero)
move-quantum-map-and-partition-table-loop
        ((vma-start-read) add m-1 a-tem)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((vma-start-write) add m-2 a-tem)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((m-tem) m+a+1 m-tem a-zero)
        (jump-less-than m-tem
                 (a-constant (eval (* page-size %quantum-swap-buffer-offset-in-tables)))
                 move-quantum-map-and-partition-table-loop)
        (call quantum-map-fixed-area-kludge)
        (call initialize-quantum-buffer)
        (popj)

;;; The quantum map is needed very early during initialization.  The device entries will
;;; eventually be needed before the first disk access.  INITIALIZE-QUANTUM-MAP-COLD
;;; causes A-V-QUANTUM-MAP to point to a temporary place at the high end of initial
;;; straight maped memory.  The quantum map continues to live there until after COLD-SWAP-IN,
;;; when INITIALIZE-QUANTUM-MAP-FOR-REAL copies the quantum map from its temporary location
;;; to its permanent home in a wired area.  Unfortunately the value of A-V-QUANTUM-MAP
;;; is caused to refer to the location in wired memory by GET-AREA-ORIGINS,
;;; before it is safe for the quantum map to live there.  We therefore use
;;; A-DISK-SWAP-SAVED-CCW-POINTER to store the permanent value of A-V-QUANTUM-MAP
;;; temporarily until it is safe for the quantum map to relocate to its new home.
hack-quantum-map-base
        (popj-after-next (a-disk-swap-saved-ccw-pointer) a-v-quantum-map)
       ((a-v-quantum-map) (a-constant (eval cold-wired-system-tables-base)))

unhack-quantum-map-base
        (popj-after-next (a-v-quantum-map) a-disk-swap-saved-ccw-pointer)
       ((a-disk-swap-saved-ccw-pointer) a-zero)

;LOADING THE INITIAL MAP.
; THE FIRST STEP IS TO ADDRESS THE SYSTEM COMMUNICATION AREA AND FIND
; OUT MUCH VIRTUAL MEMORY SHOULD BE WIRED AND STRAIGHT-MAPPED (%SYS-COM-WIRED-SIZE).
; THE MAP IS THEN SET UP FOR THOSE PAGES.  THE REMAINDER OF VIRTUAL
; SPACE IS MADE "MAP NOT SET UP."  STUFF WILL THEN BE PICKED
; UP OUT OF THE PAGE HASH TABLE.  IT IS ALSO NECESSARY TO SET UP THE
; LAST BLOCK OF LEVEL 2 MAP TO "MAP NOT SET UP (ZERO)".

INITIAL-MAP
        (CALL-XCT-NEXT PHYS-MEM-READ)           ;ADDRESS SYSTEM COMMUNICATION AREA
       ((VMA) (A-CONSTANT (PLUS 400 (EVAL %SYS-COM-WIRED-SIZE))))
COLD-WIRE-MAP
        ((M-A) Q-POINTER MD)                    ;SAVE NUMBER OF WIRED WORDS
INITIAL-MAP-A   ;Enter here with number of words to map in M-A
        ;FIRST SET ALL LEVEL 1 MAP TO 177
        ((MD) DPB (M-CONSTANT -1) (BYTE-FIELD 1 25.) A-ZERO)
INIMAP1
#+LAMBDA((L1-MAP) (A-CONSTANT 177))
#+exp   ((vma-write-l1-map) (a-constant (plus 177
                                              (byte-value l1-map-valid-exp 1)
                                              (byte-value l1-map-old-exp 1))))
        ((MD) SUB MD (A-CONSTANT 20000))
        (JUMP-NOT-EQUAL MD A-ZERO INIMAP1)
        ;THEN ZERO LAST BLOCK OF LEVEL 2 MAP
        ((MD) A-ZERO)
INIMAP2
        ;ZERO LAST BLOCK OF LVL 2 MAP. (NOTE LVL 1 MAP ENTRY 0 IS 177, TEMPORARILY)
#+exp   (no-op)
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) A-ZERO)
  ;     (NO-OP)
        ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 13.) MD INIMAP2)
        ;NOW SET UP WIRED LEVEL 1 MAP
        ((MD) A-ZERO)
#+lambda((M-C) A-ZERO)
#+exp   ((m-c) (a-constant (plus (byte-value l1-map-valid-exp 1)
                                 (byte-value l1-map-old-exp 1))))
INIMAP7
        ((#+lambda L1-MAP
          #+exp vma-write-l1-map) M-C)
        ((MD) ADD MD (A-CONSTANT 20000))
        (JUMP-LESS-THAN-XCT-NEXT MD A-A INIMAP7)
       ((M-C) ADD M-C (A-CONSTANT 1))
        ((A-SECOND-LEVEL-MAP-REUSE-POINTER-INIT) and M-C (a-constant 177))      ;FIRST NON-WIRED
        ;THEN SET UP WIRED LEVEL 2 MAP
        ((MD) SETZ)
INIMAP3 (CALL-XCT-NEXT LOAD-L2-MAP-FROM-CADR-PHYSICAL)  ;SELF-ADDRESS
       ((M-LAM) VMA-PHYS-PAGE-ADDR-PART MD
           (A-CONSTANT (PLUS (BYTE-VALUE pht2-MAP-ACCESS-CODE 3)   ;RW
                            ;(BYTE-VALUE pht2-MAP-STATUS-CODE 0)  ;4 READ/WRITE
                             (BYTE-VALUE pht2-META-BITS 64) ;NOT OLD, NOT EXTRA-PDL, STRUC
                             )))
        ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))                     ;NEXT PAGE
        (JUMP-LESS-THAN MD A-A INIMAP3)         ;LOOP UNTIL DONE ALL WIRED ADDRESSES
INIM3A  ((M-1) (BYTE-FIELD 5 8) MD)             ;IF NOT AT EVEN 1ST LVL MAP BOUNDARY...
        (JUMP-EQUAL M-1 A-ZERO INIM3B)          ; INITIALIZE REST OF 2ND LVL BLOCK TO
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) (A-CONSTANT 0))       ; MAP NOT SET UP.
        (JUMP-XCT-NEXT INIM3A)
       ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))

INIM3B                                          ;INITIALIZE REVERSE 1ST LVL MAP
        ((A-SECOND-LEVEL-MAP-REUSE-POINTER) A-SECOND-LEVEL-MAP-REUSE-POINTER-INIT)

;store scratch pointer in l1 map. -- never changed
        ((md) a-map-scratch-block)
#+lambda((l1-map) (a-constant 176))
#+exp   ((vma-write-l1-map) (a-constant (plus 176
                                              (byte-value l1-map-valid-exp 1)
                                              (byte-value l1-map-old-exp 1))))

#-lambda(begin-comment)
;reverse 1st LVL MAP in A-MEM locs 2000-2177 on LAMBDA.
; entry is data to put in MD to address 1st level map entry which is currently assigned
;  code which is relative adr of entry.  Unused entries are negative.

        ((MD) SETZ)             ;value to go in wired entries
        ((M-2) (A-CONSTANT 2000))
INIMAP5 ((OA-REG-LOW) DPB M-2 OAL-A-DEST A-ZERO)
        ((A-GARBAGE) MD)
        ((MD) ADD MD (A-CONSTANT 20000))  ;address next 1st lvl entry.  This is the same as
                                          ; virtual address if hard wired.
        (JUMP-LESS-THAN MD A-A INIMAP6)   ;jump if still hard wired.
        ((M-A MD) (M-CONSTANT -1))
INIMAP6 (JUMP-LESS-THAN-XCT-NEXT M-2 (A-CONSTANT 2177) INIMAP5)
       ((M-2) ADD M-2 (A-CONSTANT 1))
#-lambda(end-comment)

#-exp (begin-comment)
        ;REVERSE 1ST LVL MAP LOCS 1200-1377
        ;that is, last half of scratch-pad-init-area page
        ((md) M-ZERO)   ;VALUE TO GO IN WIRED ENTRIES
        ((VMA) (A-CONSTANT 1177))
INIMAP5 ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((md) ADD WRITE-MEMORY-DATA (A-CONSTANT 20000))
        (JUMP-LESS-THAN md A-A INIMAP6) ;JUMP IF STILL WIRED
        ((M-A md) (M-CONSTANT -1))      ;REST OF ENTRYS ARE -1.
INIMAP6 (JUMP-LESS-THAN VMA (A-CONSTANT 1377) INIMAP5)
#-exp (end-comment)

        (POPJ)


;some routines, used below, to help set up the initial configuration
;since none of them can get page faults without calling illop, I'm using
;the page fault variables to save things, and nothing but the page fault
;variables get clobbered
;address are passed in m-lam, and are normal nubus addresses, except shifted
;right by 2.
;data comes in or goes out in MD
;location 0 in the map is used, though it is restored

;NOTE: these can't use the scratch block since they are called from get-configuration

cold-nubus-read
#-LAMBDA (BEGIN-COMMENT)
        ((md) setz)     ;following inst gives maps time to settle.
        (no-op)
        ((a-pgf-t) l1-map)
        ((a-pgf-a) l2-map-control)
        ((a-pgf-b) l2-map-physical-page)
        ((l2-map-control) (a-constant 1464))    ;no caching.
        ((l2-map-physical-page) ldb m-lam (byte-field 22. 8.) a-zero)
        ((vma-start-read) ldb (byte-field 8 0) m-lam a-zero)
        (illop-if-page-fault)
        ((vma) md)
        ((md) setz)
        (no-op)
        ((l1-map) a-pgf-t)
        ((l2-map-control) a-pgf-a)
        (popj-after-next (l2-map-physical-page) a-pgf-b)
       ((md) vma)
#-LAMBDA (END-COMMENT)
#-exp (begin-comment)
        ((vma-start-read-unmapped) dpb m-lam (byte-field 30. 2) a-zero)
        (popj)
#-exp (end-comment)

;vma gets whole 32. bit nubus address - result in MD,  vma is modified
new-cold-nubus-read
#-LAMBDA (BEGIN-COMMENT)
        ((md) setz)
        (no-op)
        ((a-pgf-t) l1-map)
        ((a-pgf-a) l2-map-control)
        ((a-pgf-b) l2-map-physical-page)
        ((l2-map-control) (a-constant 1464))
        ((l2-map-physical-page) ldb vma (byte-field 22. 10.) a-zero)
        ((vma-start-read) ldb (byte-field 8 2) vma a-zero)
        (illop-if-page-fault)
        ((vma) md)
        ((md) setz)
        ((l1-map) a-pgf-t)
        ((l2-map-control) a-pgf-a)
        (popj-after-next (l2-map-physical-page) a-pgf-b)
       ((md) vma)
#-LAMBDA (END-COMMENT)
#-exp (begin-comment)
        ((vma-start-read-unmapped) vma)
        (popj)
#-exp(end-comment)

cold-nubus-write
#-LAMBDA (BEGIN-COMMENT)
        ((m-tem1) md)
        ((md) setz)     ;following inst gives maps time to settle.
        (no-op)
        ((a-pgf-t) l1-map)
        ((a-pgf-a) l2-map-control)
        ((a-pgf-b) l2-map-physical-page)
        ((l2-map-control) (a-constant 1464))    ;no caching.
        ((l2-map-physical-page) ldb m-lam (byte-field 22. 8.) a-zero)
        ((md) a-tem1)
        ((vma-start-write) ldb (byte-field 8 0) m-lam a-zero)
        (illop-if-page-fault)
        ((md) setz)
        (no-op)
        ((l1-map) a-pgf-t)
        (popj-after-next (l2-map-control) a-pgf-a)
       ((l2-map-physical-page) a-pgf-b)

#-LAMBDA (END-COMMENT)
#-exp(begin-comment)
        ((vma-start-write-unmapped) vma)
        (popj)
#-exp(end-comment)

;new-cold-nubus-write moved to uc-lambda-cold-disk until restore thing works

#-LAMBDA (BEGIN-COMMENT)
; for writing SDU mapping registers
cold-nubus-write-3-bytes
        ((m-tem1) md)
        ;byte 0
        ((md) setz)     ;following inst gives maps time to settle.
        ((m-tem3) ldb (byte-field 8 2) vma)
        ((a-pgf-t) l1-map)
        ((a-pgf-a) l2-map-control)
        ((a-pgf-b) l2-map-physical-page)
        ((l2-map-control) (a-constant 5400))    ;no caching.
        ((m-tem2) ldb vma (byte-field 22. 10.) a-zero)
        ((l2-map-physical-page) a-tem2)
        ((md) a-tem1)
        ((vma-start-write) a-tem3)
        (illop-if-page-fault)

        ;byte 1
        ((md) setz)
        ((l2-map-physical-page) m-minus-one dpb (byte-field 1 22.) a-tem2)
        ((md) a-tem1)
        ((vma-start-write) a-tem3)
        (illop-if-page-fault)

        ;byte 2
        ((md) setz)
        ((l2-map-physical-page) m-minus-one dpb (byte-field 1 23.) a-tem2)
        ((md) a-tem1)
        ((vma-start-write) a-tem3)
        (illop-if-page-fault)

        ((md) setz)
        ((l1-map) a-pgf-t)
        (popj-after-next (l2-map-control) a-pgf-a)
       ((l2-map-physical-page) a-pgf-b)
#-LAMBDA (END-COMMENT)

#-lambda (begin-comment)
;;; Configuration and options

; m-a  sys-conf-virtual-adr
; m-b  a-proc-conf-virtual-adr
; m-c  "share struct pointer" a 32. bit nubus address, shifted right by 2 bits
; m-d  slot number for my share-iopb

; m-t  temp , max-iopbs * 4 for a little while

;we only arrive here at cold boot time, so it's ok to clear memories, etc
get-configuration
        (call-equal m-zero a-proc-conf-local-phys-adr illop)

        ;;the following constructurs either 0 or #x10000000 in M-E depending
        ;;on if we are on the same bus as the SDU or not
        ((m-e) a-proc-conf-local-phys-adr) ;this is still an SDU-PHYS-ADR at this point
        ((m-e) and m-e (a-constant #x10000000))
        ((m-e) xor m-e (a-constant #x10000000))
        ((a-local-phys-adr-convert) m-e)

        ((m-t) ldb (byte-field 8 24.) m-e)
        ((a-sdu-quad-slot) xor m-t (a-constant #xff))

        ((vma m-a) a-proc-conf-local-phys-adr)
        (call new-cold-nubus-read) ; get the system configuration pointer
        ((md) xor md a-local-phys-adr-convert)

        ((m-b) md)
        ((m-a) sub m-a a-b) ;number of bytes between proc conf and sys conf
        ((m-a) ldb (byte-field 30. 2) m-a) ; convert to word offset
        ((a-proc-conf-virtual-adr) add m-a (a-constant sys-conf-virtual-adr))


        ((m-a) ldb (byte-field 22. 10.) md)
        ((a-sys-conf-base-phys-page) sub m-a (a-constant (eval (- 80. 2))))

        ;;set up the mapping regs for the sys conf page(s)
        ((md) (a-constant sys-conf-virtual-adr))
        ((l1-map) (a-constant 176))             ;use scratch block
        ((l2-map-control) (a-constant 1464))
        ((m-tem) a-sys-conf-base-phys-page)
        ((l2-map-physical-page m-tem) add m-tem (a-constant (eval (- 80. 2))))

        ((md) add md (a-constant 400))
        ((l2-map-control) (a-constant 1464))
        ((l2-map-physical-page) m+1 m-tem)

        ((m-a) (a-constant sys-conf-virtual-adr))
        ((m-b) a-proc-conf-virtual-adr)

        ;check version number
        ((vma-start-read) add m-a (a-constant (eval %system-configuration-version-number)))
        (illop-if-page-fault)
        (call-not-equal md (a-constant 1) illop)        ;must be version 1
    (error-table crash "System Configuraton Structure is not version 1")

        ((vma-start-read) add m-b (a-constant
                                    (eval %processor-conf-starting-processor-switches)))
        (illop-if-page-fault)
        ((a-processor-switches) ior md
                                (a-constant (eval
                                        (dpb 1 %%processor-switch-new-sys-conf-mapping 0))))
;       ((a-processor-switches) dpb m-minus-one         ;allways fast boot for now
;         (lisp-byte %%processor-switch-fast-boot-enable) a-processor-switches)

;new mem board loop
        ((m-c) (a-constant (a-mem-loc a-pmo-0)))
        ((m-d) (a-constant (a-mem-loc a-pmh-0)))
        ((m-i) add m-b (a-constant (eval %processor-conf-memory-base-0)))
        ((m-j) add m-b (a-constant (eval %processor-conf-memory-bytes-0)))

        ((m-t) a-zero)

get-next-mem-board
        ((vma-start-read) m-i)                  ;base
        (illop-if-page-fault)
        ((md) xor md a-local-phys-adr-convert)
        ((m-r) ldb (byte-field 22. 10.) md)
        ((oa-reg-low) dpb m-c oal-a-dest a-zero)
        ((a-garbage) m-r)

        ((vma-start-read) m-j)                  ;bytes
        (illop-if-page-fault)
        ((m-s) ldb (byte-field 22. 10.) md)
        ((oa-reg-low) dpb m-d oal-a-dest a-zero)
        ((a-garbage) m-s)

        (jump-equal m-t a-zero clear-mem-board-done)    ;prom has cleared first memory board
        (jump-equal m-s a-zero clear-mem-board-done)    ;size of zero means not there

        ((md) a-zero)
        (setz)
        ((m-1) l2-map-physical-page)
        ((m-3) a-zero)                          ;relative page number

clear-mem-board-1
        ((l2-map-physical-page) add m-r a-3)
        ((vma) a-zero)
clear-mem-board-2
        ((md-start-write) a-zero)
        (illop-if-page-fault)
        ((vma) add vma (a-constant 1))
        (jump-less-than vma (a-constant 400) clear-mem-board-2)

        ((m-3) add m-3 (a-constant 1))
        (jump-less-than m-3 a-s clear-mem-board-1)

        ((l2-map-physical-page) m-1)
clear-mem-board-done

        ((m-c) add m-c (a-constant 1))
        ((m-d) add m-d (a-constant 1))
        ((m-i) add m-i (a-constant 1))
        ((m-j) add m-j (a-constant 1))

        ((m-t) add m-t (a-constant 1))
        (jump-less-than m-t (a-constant 10.) get-next-mem-board)

;set up slot numbers

        ((m-tem) a-processor-switches)
        (jump-if-bit-clear (lisp-byte %%processor-switch-slot-numbers-set-up)
                m-tem slots-set-up)

        ((vma-start-read) add m-b (a-constant (eval %processor-conf-slot-number)))
        (illop-if-page-fault)
        ((a-rg-quad-slot) xor md (a-constant #xf0))
        ((m-tem) ldb (byte-field 4 4) md)

        ((vma-start-read) add m-b (a-constant (eval %processor-conf-vcmem-slot)))
        (illop-if-page-fault)
        ((a-tv-quad-slot) xor md (a-constant #xf0))

        ((m-t) ldb (byte-field 4 4) md)
        ((a-tv-quad-slot) dpb m-minus-one (byte-field 4 4) a-tv-quad-slot)
        (jump-equal m-t a-tem rg-and-tv-on-same-bus)
        ((a-tv-quad-slot) dpb m-zero (byte-field 1 4) a-tv-quad-slot)

rg-and-tv-on-same-bus
        ((m-t) a-tv-quad-slot)
        ((m-t) ldb (byte-field 8 8) m-t) ; video board type, 0 or 1 = VCMEM, 2 = QUAD
        (jump-equal m-t (a-constant 0) set-up-for-vcmem)
        (jump-equal m-t (a-constant 1) set-up-for-vcmem)
        (jump-equal m-t (a-constant 2) set-up-for-quad)
        (call illop)
    (error-table crash "Unknown video board type")

set-up-for-quad
        ((m-t) a-tv-quad-slot)
        ((a-video-buffer-base-phys-page) dpb m-t (byte-field 8 14.) a-zero)
        ((m-t) ldb (byte-field 8 16.) m-t) ;screen number
        ((a-video-buffer-base-phys-page) dpb m-t (byte-field 2 11.) a-video-buffer-base-phys-page)
        ((a-disk-run-light) (a-constant (plus (byte-value q-data-type dtp-fix)
                                              177000000
                                              310_5
                                              12.)))
        (jump video-set-up-done)

set-up-for-vcmem
        ((m-t) a-tv-quad-slot)
        ((a-video-buffer-base-phys-page) dpb m-t (byte-field 8 14.) (a-constant 200))
        ;fix up the run lights so they can be seen on either landscape or portrait monitors
        ;310 is line number for run lights to appear on
        ;#x6000 is beginning of VCMEM scan line table
        ((vma) dpb m-t (byte-field 8 24.) (a-constant (eval (+ #x6000 (* 4 310)))))
        (call new-cold-nubus-read)
        ((md) ldb (byte-field 15. 1) md) ;divide by 2, also mask to make sure it
                                         ;doesn't fall outside the screen
                                         ;(you'll probably lose some other way though...)
        ;now MD is word offset in TV buffer for desired line
        ;12. is number of words over on line
        ((a-disk-run-light) add md (a-constant (plus (byte-value q-data-type dtp-fix)
                                                     177000000
                                                     12.)))

        (jump video-set-up-done)

video-set-up-done
        ((vma-start-read) add m-a (a-constant (eval %system-configuration-grey-slot)))
        (illop-if-page-fault)
        ((a-grey-quad-slot) xor md (a-constant #xf0))

        ((m-t) ldb (byte-field 4 4) md)
        ((a-grey-quad-slot) dpb m-minus-one (byte-field 4 4) a-grey-quad-slot)
        (jump-equal m-t a-tem rg-and-grey-on-same-bus)
        ((a-grey-quad-slot) dpb m-zero (byte-field 1 4) a-grey-quad-slot)

rg-and-grey-on-same-bus
        ((a-rg-quad-slot) dpb m-minus-one (byte-field 4 4) a-rg-quad-slot)

slots-set-up
;I think whoever put in the set-up-for-[quad,vcmem] above probably wanted
;to comment this out too...
;       ;fix up the run lights so they can be seen on either landscape or portrait monitors
;       ((m-t) a-tv-quad-slot)
;       ;310 is line number for run lights to appear on
;       ;#x6000 is beginning of VCMEM scan line table
;       ((vma) dpb m-t (byte-field 8 24.) (a-constant (eval (+ #x6000 (* 4 310)))))
;       (call new-cold-nubus-read)
;       ((md) ldb (byte-field 15. 1) md) ;divide by 2, also mask to make sure it
;                                        ;doesn't fall outside the screen
;                                        ;(you'll probably lose some other way though...)
;       ;now MD is word offset in TV buffer for desired line
;       ;12. is number of words over on line
;       ((a-disk-run-light) add md (a-constant (plus (byte-value q-data-type dtp-fix)
;                                                    177000000
;                                                    12.)))

        ((vma-start-read) add m-b (a-constant (eval %processor-conf-number-of-multibus-maps)))
        (illop-if-page-fault)
        (jump-equal md a-zero no-multibus-map-limit)
        ((a-number-of-data-mapping-registers-for-disk) add md (a-constant -2))
no-multibus-map-limit

  ;hack the "2x2" stuff before the disk because of A-MULTIBUS-DISK-MAP-BASE
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear (lisp-byte %%processor-switch-2x2-stuff-valid-in-conf-structure)
                           m-tem get-configuration-2x2-done)
        ((vma-start-read) add m-b (a-constant (eval %processor-conf-load-band)))
        (illop-if-page-fault)
        ((a-disk-band-partition-name) md)
        ((vma-start-read) add m-b (a-constant (eval %processor-conf-micro-band)))
        (illop-if-page-fault)
        ((a-disk-ucode-partition-name) md)
        ((vma-start-read) add m-b (a-constant (eval %processor-conf-paging-band)))
        (illop-if-page-fault)
        ((a-disk-page-partition-name) md)
        ((vma-start-read) add m-b
                (a-constant (eval %processor-conf-base-multibus-mapping-register)))
        (illop-if-page-fault)
        ((a-multibus-disk-map-base) md)

get-configuration-2x2-done
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear (lisp-byte %%processor-switch-use-disk-sharing-protocol)
                           m-tem get-configuration-no-share-disk)

;;get here either on cold boot, or on warm boot to reinitialize the disk stuff
;;therefore m-a, m-b, etc may be trashed
initialize-share-iopb
        ;;set up the mapping regs for the sys conf page(s)
        ((md) (a-constant sys-conf-virtual-adr))
        ((l1-map) (a-constant 176))
        ((l2-map-control) (a-constant 1464))
        ((m-tem) a-sys-conf-base-phys-page)
        ((l2-map-physical-page m-tem) add m-tem (a-constant (eval (- 80. 2))))

        ((md) add md (a-constant 400))
        ((l2-map-control) (a-constant 1464))
        ((l2-map-physical-page) m+1 m-tem)


        ((m-a) (a-constant sys-conf-virtual-adr))
        ((m-b) a-proc-conf-virtual-adr)

        ((md) a-zero)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-runme)))
        (illop-if-page-fault)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-interrupt-addr)))
        (illop-if-page-fault)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-spare-1)))
        (illop-if-page-fault)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-spare-2)))
        (illop-if-page-fault)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-spare-3)))
        (illop-if-page-fault)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-spare-4)))
        (illop-if-page-fault)

        ((md) a-rg-quad-slot)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-slot)))
        (illop-if-page-fault)

        ((md) (a-constant 4))
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-type)))
        (illop-if-page-fault)

        ;;SDU mapping register MAP-BASE + 0 points to the IOPB
        ;;thie iopb is at virtual address 640
        ;;that's an offset of (- 640 400) = 240 words in the page
        ;;therefore, the multibus offset is (+ (ash map-base 10.) (* 240 4.))
        ((m-tem) a-multibus-disk-map-base)
        ((md) dpb m-tem (byte-field 10. 10.) (a-constant (eval (* 240 4.))))

        (call convert-md-to-8086)
        ((vma-start-write) add m-b (a-constant (eval %processor-conf-share-iopb)))
        (illop-if-page-fault)

        ;;set up SDU mapping register MAP-BASE + 1 to point to share-iopb structure
        ;;here we assume that the lambda proc conf is contained in 1 page ***
        ((m-tem) a-proc-conf-local-phys-adr)
        ((m-tem) xor m-tem a-local-phys-adr-convert)
        ((m-tem) ldb m-tem (byte-field 22. 10.) a-zero)
        ((md) dpb m-minus-one (byte-field 1 23.) a-tem) ; enable bit

        ((m-t) m+a+1 m-zero a-multibus-disk-map-base)
        ((m-t) dpb m-t (byte-field 30. 2) a-zero) ;multiply by 4
        ((m-t) add m-t (a-constant #x18000)) ;offset on SDU
        ((vma) a-sdu-quad-slot)
        ((vma) dpb vma (byte-field 8 24.) a-t)
        (call cold-nubus-write-3-bytes)

        ;;do the work to link into the iopb "chain" (either cold or warm boot)
        ((vma-start-read) add m-a
                (a-constant (eval %system-configuration-share-struct-pointer)))
        (illop-if-page-fault)
        ((m-c) xor md a-local-phys-adr-convert)

        ;;below here, memory references are to the "share-struct" which is probably
        ;; somewhere in multibus memory.  in any case, it doesn't seem like it
        ;; will be used enough to "direct map" it

        ((vma) add m-c (a-constant (eval (* 4 %share-struct-max-iopbs))))
        (call new-cold-nubus-read)
        ((m-t) dpb md (byte-field 30. 2) a-zero) ;multiply by 4

        ;m-d is the iopb slot number counter
        ((m-d) setz)
find-empty-entry
        ((vma) add m-c (a-constant (eval (* 4 %share-struct-start-of-valid-table))))
        (call-xct-next new-cold-nubus-read)
       ((vma) add vma a-d)
        (jump-equal md a-zero found-empty-entry)
        ((m-d) add m-d (a-constant 4))
        (call-equal m-d a-t illop)
    (error-table crash "Out of SHARE-IOPB slots")
        (jump find-empty-entry)

found-empty-entry
        ;; now m-d is four times the number of an empty share-iopb slot
        ;; new we have to make an 8086 pointer to the share-iopb in multibus mapping
        ;; register MAP-BASE + 1.  The multibus offset is
        ;; (+ (ash (1- MAP-BASE) 10.) (ldb 0012 (* 4 (+ m-proc-conf-ptr
        ;;                                 a-%processor-conf-share-runme))))
        ((md) add m-b (a-constant (eval %processor-conf-share-runme)))
        ((m-tem) m+a+1 m-zero a-multibus-disk-map-base)
        ((m-tem) dpb m-tem (byte-field 10. 10.) a-zero)
        ((md) dpb md (byte-field 8 2) a-tem)

        (call convert-md-to-8086)

        ;; now md is the approiate pointer, store it in the iopb table
        ((vma) add m-c (a-constant (eval (* 4 %share-struct-start-of-valid-table))))
        ((vma) add vma a-t) ; add in (* 4 max-iopbs)
        ((vma) add vma a-d)
        (call new-cold-nubus-write)

        ;; set the valid flag
        ((md) (a-constant 1))
        ((vma) add m-c (a-constant (eval (* 4 %share-struct-start-of-valid-table))))
        ((a-my-iopb-valid-flag-physical-adr) add vma a-d)
        ((vma) a-my-iopb-valid-flag-physical-adr)
        (call new-cold-nubus-write)
        (popj)

convert-md-to-8086
        ((m-tem1) ldb (byte-field 4 0) md a-zero)
        (popj-after-next (md) ldb (byte-field 16. 4) md a-zero)
       ((md) dpb md (byte-field 16. 16.) a-tem1)

get-configuration-no-share-disk
        (popj)
#-lambda(end-comment)

#-exp
(begin-comment)
get-configuration
;; search slots from slot 4 down to 0 for memory boards

        ;; initialize slot number
        ((m-1) (a-constant 4))
        ;; initialize block pointers
        ((m-a) (a-constant (a-mem-loc a-pmo-0)))
        ((m-b) (a-constant (a-mem-loc a-pmh-0)))
memory-board-loop
        ;; check for memory board
        (call memory-board-in-slot)     ; flag into m-2
        ;; if found, get size and add it
        (jump-equal m-2 a-zero not-memory-board)
        (call get-memory-board-size)    ; size into m-2
        (call add-memory-board)
not-memory-board
        ((m-1) SUB m-1 (a-constant 1))
        (jump-if-bit-clear (byte-field 1 31.) m-1 memory-board-loop)
        ((a-tv-quad-slot) (a-constant #xf5))
        (popj)

;; check for memory board in slot
;; slot number in m-1
;; result non-zero in m-2 if memory board found
;; trashes m-c

memory-board-in-slot
                ;; check for TI memory board
        ((m-2) m-minus-one)     ;maybe memory board here
                ;; build address and check for M
        ((m-c) dpb m-minus-one (byte-field 24. 8.) (a-constant #x84))
        (call-xct-next memory-board-check-M)
       ((m-c vma) dpb m-1 (byte-field 4. 24.) a-c)
                ;; increment pointer and check for E
        (call-xct-next memory-board-check-E)
       ((m-c vma) ADD m-c (a-constant 4))
                ;; increment pointer and check for M
        (call-xct-next memory-board-check-M)
       ((vma) ADD m-c (a-constant 4))
                ;; if memory board found return
        (popj-not-equal m-2 a-zero)
                ;; check for LMI memory board
        ((m-2) m-minus-one)     ;maybe memory board here
                ;; build address and check for M
        ((m-c) dpb m-minus-one (byte-field 20. 12.) (a-constant #xC10))
        (call-xct-next memory-board-check-M)
       ((m-c vma) dpb m-1 (byte-field 4. 24.) a-c)
                ;; increment pointer and check for E
        (call-xct-next memory-board-check-E)
       ((m-c vma) ADD m-c (a-constant 4))
                ;; increment pointer and check for "M"
        (call-xct-next memory-board-check-M)
       ((vma) ADD m-c (a-constant 4))
        (popj)

memory-board-check-M
        (call new-cold-nubus-read)
        ((md) ldb md (byte-field 8 0) a-zero)
        (popj-equal md (a-constant 115))        ; "M"
        (popj-xct-next)
       ((m-2) a-zero)

memory-board-check-E
        (call new-cold-nubus-read)
        ((md) ldb md (byte-field 8 0) a-zero)
        (popj-equal md (a-constant 105))        ; "E"
        (popj-xct-next)
       ((m-2) a-zero)

;; get memory board size
;; slot number in m-1
;; result in m-2 (number of pages)
;; hard code size of 4000 pages (2 meg) for testing
get-memory-board-size
        ;; starting offset for memory board
        ((m-2) dpb m-1 (byte-field 4. 24.) a-zero)
        ((m-2) dpb m-minus-one (byte-field 4. 28.) a-2)
memory-board-size-loop
        ;; try to write to memory location
        ((m-2) add m-2 (a-constant 400000))     ; min memory quantum 128K
        ((md) a-zero)
        ((vma) m-2)
        (call nubus-write)      ; sets vma to m-minus-one on timeout
        (jump-not-equal vma a-minus-one memory-board-size-loop)
        ((m-2) ldb m-2 (byte-field 14. 10.) a-zero)
        (popj)


;; add memory board
;; slot number in m-1
;; size in m-2
;; a-pmo-* pointer in m-a
;; a-pmh-* pointer in m-b
;; trashes m-c
add-memory-board
        ((m-c) dpb m-1 (byte-field 4. 14.) a-zero)
        ((m-c) dpb m-minus-one (byte-field 4. 18.) a-c)
        ((OA-reg-low) dpb m-a oal-a-dest a-zero)
        ((a-garbage) m-c)
        ((m-a) M+1 m-a)
        ((OA-reg-low) dpb m-b oal-a-dest a-zero)
        ((a-garbage) m-2)
        ((m-b) M+1 m-b)
        (popj)

;; claim: (1) there is always a memory board in slot 4.
;;        (2) memory boards are always 2 or 8 megs.
;;        (3) mit people will put things other than memory in slot 3
;;            so we have to be careful in our test of what the board is.
;; just in case i dont fix this correctly by noon, i will do the
;; wired 8-meg board that Soley at MIT wants.

; this is 8 meg mode
;       ((a-pmo-0) (a-constant (eval (ash #xf4000000 -10.))))
;       ((a-pmh-0) (a-constant 20000))

; this is 2 2 meg mode
;;;     ((a-pmo-0) (a-constant (eval (ash #xf4000000 -10.))))
;;;     ((a-pmh-0) (a-constant 4000))
;;;     ((a-pmo-1) (a-constant (eval (ash #xf3000000 -10.))))
;;;     ((a-pmh-1) (a-constant 4000))
;;;     ((a-tv-quad-slot) (a-constant #xf5))
;;;     ((a-number-of-data-mapping-registers-for-disk) (a-constant 10000.))
;;;     (popj)

#-exp(end-comment)

highest-kernal-ucode-location

))

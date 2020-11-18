;-*- Mode:LISP; Package:LAMBDA; Base:8; readtable: ZL -*-

;this file for LAMBDA only!!
;       ** (c) Copyright 1983 Lisp Machine Inc **

(DEFCONST UC-LAMBDA-COLD-DISK '(


;physical memory referencing on LAMBDA works by setting having a straight map set up
; ahead of time.  (also, the page fault routines are relied upon to work for IO devices,
; etc, without much setup).  The PHYS-MEM-READ and PHYS-MEM-WRITE routines are
; kept around for convience.  PHYS-MEM-READ, tho, is used to reference disk CCW lists,
; and therefore must save and restore map.

PHYS-MEM-READ
        (declare (clobbers a-lam a-tem))
        ((MD) VMA)
        (CALL-XCT-NEXT LOAD-L2-MAP-FROM-CADR-PHYSICAL)  ;SELF-ADDRESS
       ((M-LAM) VMA-PHYS-PAGE-ADDR-PART MD
           (A-CONSTANT (PLUS (BYTE-VALUE pht2-MAP-ACCESS-CODE 3)   ;RW
                            ;(BYTE-VALUE pht2-MAP-STATUS-CODE 0)  ;4 READ/WRITE
                             (BYTE-VALUE pht2-META-BITS 64) ;NOT OLD, NOT EXTRA-PDL, STRUC
                             )))
        ((VMA-START-READ) md)
        (ILLOP-IF-PAGE-FAULT)
        ((M-LAM) MD)
        ((MD) VMA)
#+exp   (no-op)
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) A-LAST-L2-MAP-CONTROL)        ;put back map
        ((#+lambda L2-MAP-PHYSICAL-PAGE
          #+exp vma-write-l2-map-physical-page) A-LAST-L2-MAP-PHYSICAL-PAGE)
        ((MD) M-LAM)
        (POPJ)


PHYS-MEM-WRITE
        ((VMA-START-WRITE) VMA)
        (ILLOP-IF-PAGE-FAULT)
        (POPJ)


;;PHYSICAL MEMORY REFERENCING.
;;THIS WORKS BY TEMPORARILY CLOBBERING LOCATION 0 OF THE SECOND-LEVEL MAP.
;;A-TEM1, A-TEM2, AND A-TEM3 ARE USED AS TEMPORARIES.  ARGS ARE IN VMA AND MD.
;PHYS-MEM-READ
;       ((M-TEM1) VMA)                          ;SAVE ADDRESS
;       ((MD) A-ZERO)                           ;ADDRESS MAP LOCATION 0@2
;       ((M-TEM3) l2-map-control)               ;SAVE IT (READ & WRITE THE SAME)
;  ;**map map**
; ;     ((VMA-WRITE-MAP) VMA-PHYS-PAGE-ADDR-PART VMA
; ;                               (A-CONSTANT (BYTE-VALUE MAP-ACCESS-CODE 3)))
;       ((VMA-START-READ) DPB M-ZERO            ;READ, USING LOC WITHIN PAGE ZERO
;               ALL-BUT-VMA-LOW-BITS A-TEM1)
;       (ILLOP-IF-PAGE-FAULT)                   ;FOO, I JUST SET UP THE MAP
;       ((M-TEM2) READ-MEMORY-DATA)             ;GET RESULT TO BE RETURNED
;       ((MD) A-ZERO)                           ;RESTORE THE MAP
;       ((l2-map-control) A-TEM3)
;       (POPJ-AFTER-NEXT (VMA) A-TEM1)          ;RETURN CORRECT VALUES IN VMA AND MD
;      ((MD) A-TEM2)
;
;PHYS-MEM-WRITE
;       ((M-TEM1) VMA)                          ;SAVE ADDRESS
;       ((M-TEM2) MD)                           ;AND DATA
;       ((MD) A-ZERO)                           ;ADDRESS MAP LOCATION 0@2
;       ((M-TEM3) l2-map-control)       ;SAVE IT (READ & WRITE THE SAME)
;  ;**map map**
;  ;    ((VMA-WRITE-MAP) VMA-PHYS-PAGE-ADDR-PART VMA
;  ;            (A-CONSTANT (BYTE-VALUE MAP-ACCESS-CODE 3)))
;       ((MD) A-TEM2)                           ;RESTORE THE DATA TO BE WRITTEN
;       ((VMA-START-WRITE) DPB M-ZERO           ;WRITE, USING LOC WITHIN PAGE ZERO
;               ALL-BUT-VMA-LOW-BITS A-TEM1)
;       (ILLOP-IF-PAGE-FAULT)                   ;FOO, I JUST SET UP THE MAP
;       ((MD) A-ZERO)                           ;RESTORE THE MAP
;       ((l2-map-control) A-TEM3)
;       (POPJ-AFTER-NEXT (VMA) A-TEM1)          ;RETURN CORRECT VALUES IN VMA AND MD
;      ((MD) A-TEM2)

;;; COLD BOOT, %DISK-RESTORE and %DISK-SAVE code

;(%DISK-SAVE main-memory-size high-16-bits-of-partition-name low-16-bits)
;The second and third arguments may be zero to specify the current partition.
;The first arg may also be minus the main-memory-size, to dump an incremental band.
DISK-SAVE (MISC-INST-ENTRY %DISK-SAVE)
#+lambda(call turn-pagable-ucode-off-and-restore-initialization)
        (jump xdisk-save)


COLD-BOOT
        ((q-r) setz)

cold-boot-with-conf-ptr
;halt before starting if default boot
#+exp   (jump-not-equal m-zero (field a-source-multiplier 1777) xdisk-restore-no-halt)
;#+exp   (no-op halt-cons)
#+exp   (no-op)                                 ;leave a spot for some other program
                                                ;to set a halt bit
#+exp xdisk-restore-no-halt
#+exp   (call setup-crash-record-pointer)
        ((a-proc-conf-local-phys-adr) q-r)
cold-boot-with-previous-conf-ptr        ;use same as supplied previously (manual start)
        ((M-4) A-ZERO)                          ;0 => use current band.
#+exp   ((mcr) (a-constant (eval (+ (dpb 1 (byte 1  8.) 0) ;memory cycle enable
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
#+exp   (call Setup-Microcode-Symbol-Area)
        (JUMP DISK-RESTORE-1)                   ;Load world from there

;(%DISK-RESTORE high-16-bits-of-partition-name low-16-bits)
;The first and second arguments may be zero to specify the current partition.
DISK-RESTORE (MISC-INST-ENTRY %DISK-RESTORE)
        ((M-4) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        ((M-4) DPB C-PDL-BUFFER-POINTER-POP (BYTE-FIELD 20 20) A-4)
#+lambda(call turn-pagable-ucode-off-and-restore-initialization)
DISK-RESTORE-1  (declare (suspend-flow-tracing))
        (jump xdisk-restore)


warm-boot  (declare (suspend-flow-tracing))
#+lambda(call turn-pagable-ucode-off-and-restore-initialization)
#+exp   (call setup-crash-record-pointer)
#+exp   (call Add-Boot-Info-to-Crash-Record)
        (jump xwarm-boot)

boot-exit-and-turn-pagable-ucode-on
#+lambda(call turn-pagable-ucode-on-and-evict-initialization)  ;clobbers lots of ACs
        ((m-j) c-pdl-buffer-pointer-pop)        ;restore initial PC.
        ((m-a) m-fef)
#+exp   ((mcr) ior mcr (a-constant 1_15.)) ;enable interrupts
        (JUMP-XCT-NEXT QLENX)                   ;CALL INITIAL FUNCTION, NEVER RETURNS
       ((M-ERROR-SUBSTATUS) M-ZERO)



;
;  177300000 -> 177337777 64 pages
;  177360000 -> 177367777 32 pages

;177367400 sys-conf 2nd page & top of 32. page segment
;177367000 sys-conf 1st page
;177366400
; ...
;177360000 ;bottom of 32. page segment
; >gap<
;177337400 ;top of 64. page segment
; ...
;177300000 ;bottom of 64. page segment - stored in a-sys-conf-base-phys-page

xlambda-sys-conf-phys-to-virtual (misc-inst-entry %lambda-sys-conf-phys-to-virtual)
        (call get-32-bits)
        (call convert-local-phys-to-virtual)
        (popj)

;arg is M-1 as 32 bit local physical adr
;return virtual adr in M-T as fixnum
convert-local-phys-to-virtual
  ;m-2 gets page number desired
  ;m-3 gets offset to word desried
                (error-table restart convert-local-phys-to-virtual)
        ((m-2) ldb (byte-field 22. 10.) m-1)
        ((m-2) sub m-2 a-sys-conf-base-phys-page)
        (call-less-than m-2 a-zero trap)
    (error-table physical-address-not-in-sys-conf m-1 convert-local-phys-to-virtual)
        (jump-less-than m-2 (a-constant 64.) cvt-p-to-v-large-block)
        (call-greater-or-equal m-2 (a-constant 80.) trap)
    (error-table physical-address-not-in-sys-conf m-1 convert-local-phys-to-virtual)

;address is in small block
        ((m-2) sub m-2 (a-constant 64.))
        ((m-3) dpb m-2 (byte-field 22. 8) a-zero)
        ((m-tem) ldb (byte-field 8 2) m-1)
        ((m-3) dpb m-tem (byte-field 8 0) a-3)
        (popj-after-next (m-t) add m-3 (a-constant (plus 177360000
                                                         (byte-value q-data-type dtp-fix))))
       (no-op)

cvt-p-to-v-large-block
        ((m-3) dpb m-2 (byte-field 22. 8) a-zero)
        ((m-tem) ldb (byte-field 8 2) m-1)
        ((m-3) dpb m-tem (byte-field 8 0) a-3)
        (popj-after-next (m-t) add m-3 (a-constant (plus 177300000
                                                         (byte-value q-data-type dtp-fix))))
       (no-op)

xlambda-sys-conf-virtual-to-phys (misc-inst-entry %lambda-sys-conf-virtual-to-phys)
                (error-table restart xlambda-sys-conf-virtual-to-phys)
        (dispatch (i-arg data-type-invoke-op)
                  q-data-type c-pdl-buffer-pointer trap-unless-fixnum)
   (error-table argtyp fixnum pp 0 xlambda-sys-conf-virtual-to-phys)
   (error-table arg-popped 0 pp pp)
        ((m-2) q-pointer c-pdl-buffer-pointer-pop)
        (call convert-virtual-to-local-phys)
        (jump return-m-1-unsigned)

;m-2 is virtual address, without type bits
;returns phys adr in m-1
convert-virtual-to-local-phys
                (error-table restart convert-virtual-to-local-phys)
        (call-less-than m-2 (a-constant 177300000) trap)
    (error-table virtual-address-not-in-sys-conf m-2 convert-virtual-to-local-phys)
        (jump-less-than m-2 (a-constant 177340000) cvt-v-to-p-large)
        (call-less-than m-2 (a-constant 177360000) trap)
    (error-table virtual-address-not-in-sys-conf m-2 convert-virtual-to-local-phys)
        (call-greater-or-equal m-2 (a-constant 177370000) trap)
    (error-table virtual-address-not-in-sys-conf m-2 convert-virtual-to-local-phys)

;small block
        ((m-1) sub m-2 (a-constant 177360000))
        ((m-1) ldb (byte-field 22. 8) m-1)
        ((m-1) add m-1 a-sys-conf-base-phys-page)
        ((m-1) add m-1 (a-constant 64.))
        ((m-1) dpb m-1 (byte-field 22. 10.) a-zero)
        ((m-1) dpb m-2 (byte-field 8 2) a-1)
        (popj)

;large block
cvt-v-to-p-large
        ((m-1) sub m-2 (a-constant 177300000))
        ((m-1) ldb (byte-field 22. 8) m-1)
        ((m-1) add m-1 a-sys-conf-base-phys-page)
        ((m-1) dpb m-1 (byte-field 22. 10.) a-zero)
        ((m-1) dpb m-2 (byte-field 8 2) a-1)
        (popj)

xlambda-sys-conf-virtual-adr (misc-inst-entry %lambda-sys-conf-virtual-adr)
        (popj-after-next (m-1) (a-constant sys-conf-virtual-adr))
       ((m-t) dpb m-1 q-pointer (a-constant (byte-value q-data-type dtp-fix)))

;move back to uc-initialization after restore thing works
new-cold-nubus-write
#-LAMBDA (BEGIN-COMMENT)
        ((m-tem1) md)
        ((md) setz)     ;following inst gives maps time to settle.
        (no-op)
        ((a-pgf-t) l1-map)
        ((a-pgf-a) l2-map-control)
        ((a-pgf-b) l2-map-physical-page)
        ((l2-map-control) (a-constant 1464))    ;no caching.
        ((l2-map-physical-page) ldb vma (byte-field 22. 10.) a-zero)
        ((md) a-tem1)
        ((vma-start-write) ldb (byte-field 8 2) vma a-zero)
        (illop-if-page-fault)
        ((md) setz)
        (no-op)
        ((l1-map) a-pgf-t)
        (popj-after-next (l2-map-control) a-pgf-a)
       ((l2-map-physical-page) a-pgf-b)
#-LAMBDA (END-COMMENT)
#-exp (begin-comment)
        ((vma-start-read-unmapped) vma)
        (popj)
#-exp (end-comment)

))

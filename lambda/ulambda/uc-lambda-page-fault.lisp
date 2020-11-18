;-*- Mode:LISP; Base:8; readtable: ZL -*-
;       ** (c) Copyright 1983 Lisp Machine Inc **

;;; Notes for explorer gc - pace Nov 20, 1985
;;;
;;; The per page volatility bits for the explorer are in L2-MAP-CONTROL<12.:11.>,
;;; but on the lambda its L2-MAP-CONTROL<1:0>.  Therefore, where ever we write
;;; L2-MAP-CONTROL, put in another instruction for the explorer to copy
;;; the bits to the other field.  -done  RG 1/20/86
;;;
;;; Secondly, the OLD-SPACE bit on the explorer (and on the lambda AVP) is in
;;; L1-MAP<10.>  1 means newspace, 0 means oldspace.
;;; This bit needs to be correctly set when regions are allocated or flipped.
;;;
;;; The region volatility bits stored in the L1-MAP are the same place and have the same
;;; meanings.
;;;
;;; There is code at INIMAP1 to set all of the L1-MAP to "newspace" when booting.
;;; This may have to be replaced by a thing to set all of the oldspace bits by running
;;; through the region tables.   -no, would not win much to boot with oldspace in existance.
;;;

;;;Timing restrictions concerning 2nd level map:
;;;  on LAMBDA, a instruction must intervene between the MD being changed and the map being
;;;     used as a SOURCE.  No NO-OP is necessary if map is used as a DESTINATION, however.
;;;  on EXP, instructions must intervene both places (as per "special considerations").

(DEFCONST UC-LAMBDA-PAGE-FAULT '(
;;; page fault handler
;;; THIS FILE FOR LAMBDA ONLY.  However, some cadr stuff is still present in conditionalized
;;;  form for reference.  On LAMBDA as on CADR, there are three complex data structures
;;;  (the level-2 map, the %PHT2- fields and the %%REGION- fields) which share some of the
;;;  same fields.  Eventually, we will want to "decouple" all three.  For now, however,
;;;  we will leave %PHT2- and %%REGION- "coupled" but we must deal with the issue of
;;;  the level 2 map, since it is split into two pieces on LAMBDA.  (Even eventually,
;;;  all three will still have the same 10. bit byte of ACCESS-STATUS-AND-META bits.
;;;  Its just that it may be shifted to different places in the word in the various
;;;  places.)  %%MAP2C-  prefix is used to refer to the bits "in position" for the
;;;  level2 map.

;PAGE FAULTS GENERALLY DO NOT CLOBBER ANYTHING.
;EXCEPTIONS:
;       THE A-PGF-MUMBLE REGISTERS ARE CLOBBERED.  ONLY THE PAGE FAULT
;               ROUTINES SHOULD USE THEM.
;       M-TEM, M-TEM1, M-TEM2, M-TEM3 AND M-TEM4 ARE CLOBBERED.  THEY ARE SUPER-TEMPORARY.
;       THE DISPATCH CONSTANT AND THE Q REGISTER ARE CLOBBERED.
;       THE DATA-TYPE OF VMA -MUST NOT- BE CLOBBERED.
;       THE PDL-BUFFER-INDEX ISN'T CLOBBERED, BUT IT SHOULD BE.
;
;IF AN INTERRUPT OCCURS, IT HAS ALMOST NO SIDE-EFFECTS OTHER THAN WHAT
;PAGE FAULTS HAVE.
;
;IF A SEQUENCE BREAK IS ALLOWED AND OCCURS, IT OCCURS AFTER A WRITE CYCLE
;IS SUCCESSFULLY COMPLETED, BUT EFFECTIVELY BEFORE A READ CYCLE.
;THE MD IS RESET FROM THE VMA.  THE LETTERED M ACS ARE SAVED,
;AND MUST CONTAIN GC MARKABLE STUFF OR DTP TRAP (OR -1).  RANDOM
;MISCELLANEOUS ACS LIKE A-TEM'S ARE CLOBBERED BY SEQUENCE BREAKS.

;Fields in first level map.  Lambda only, since on cadr the 5 bit l2-map-block-index is
; all there is.   The L1 map meta-bits are a function of the region.  Since each L1 map
; entry corresponds to 32. virtual pages, this means regions must be at least 32 pages, etc.
; This is no further restriction, since the address space quantum was already 64 pages.
; For hardware convenience, all three L1 map meta bits are stored in COMPLEMENTED form.
; The L1 map MB codes are: (in parens is the inverted value actually seen in hardware).
;   0 -> static region (3)
;   1 -> dynamic region (2)
;   2 -> active consing region (ie, copy space) (1)
;   3 -> extra pdl region (0)

;gc bits same between lambda and explorer
(DEF-DATA-FIELD L1-MAP-MB-INVALID 1 9)          ;if 1, MB1 and MB0 are INVALID.
(def-data-field map1-volatility-invalid 1 9)
(def-data-field map1-volatility 2 7)
(DEF-DATA-FIELD L1-MAP-MB1-BAR 1 8)
(DEF-DATA-FIELD L1-MAP-MB0-BAR 1 7)
(DEF-DATA-FIELD L1-MAP-META-BITS #+lambda 3 #+exp 5 7)
(DEF-DATA-FIELD L1-MAP-L2-BLOCK-SELECT 7 0)
#+exp (def-data-field l1-map-old-exp 1 10.)
#+exp (def-data-field l1-map-valid-exp 1 11.)
#+exp (def-data-field l1-map-all-but-old-and-valid 9 0)
; DEFINITIONS OF FIELDS IN THE MAP HARDWARE
;  BITS IN MEMORY-MAP-DATA.

;;CADR DEFINITIONS:
;;(DEF-DATA-FIELD MAP-READ-FAULT-BIT 1 30.)     ;this symbol not used
;;(DEF-DATA-FIELD MAP-WRITE-FAULT-BIT 1 31.)    ;this symbol not used
;;(DEF-DATA-FIELD MAP-PHYSICAL-PAGE-NUMBER 14. 0)
;;(DEF-DATA-FIELD MAP-META-BITS 6 14.)          ;THE HIGH TWO OF THESE ARE HACKABLE BY
;;                                              ; DISPATCH INSTRUCTION
;;                                              ;THE REST ARE JUST FOR SOFTWARE TO LOOK AT
;;(DEF-DATA-FIELD MAP-STATUS-CODE 3 20.)
;;(DEF-DATA-FIELD MAP-ACCESS-CODE 2 22.)                ;NOTE BIT 22 IS IN TWO FIELDS
;;(DEF-DATA-FIELD MAP-FIRST-LEVEL-MAP 5 24.)    ;NOTE NOT THE SAME AS WHERE IT WRITES
;;(DEF-DATA-FIELD MAP-SECOND-LEVEL-MAP 24. 0)
;;(DEF-DATA-FIELD MAP-ACCESS-STATUS-AND-META-BITS 10. 14.)
;;(DEF-DATA-FIELD MAP-HARDWARE-READ-ACCESS 1 23.) ;HARDWARE PERMITS (AT LEAST) READ ACCESS
;;                                              ; IF THIS BIT SET.

;(DEF-DATA-FIELD CADR-MAP-ACCESS-CODE 2 22.) ;czrr, inimap, phys-mem-read
;(DEF-DATA-FIELD CADR-MAP-STATUS-CODE 3 20.)
;(DEF-DATA-FIELD CADR-MAP-META-BITS 6 14.) ;used in inimap and phys-mem-read
;(DEF-DATA-FIELD CADR-ACCESS-STATUS-AND-META-BITS 10. 14.) ;load-l2-map-from-cadr-physical
;(DEF-DATA-FIELD CADR-PHYSICAL-PAGE-NUMBER 14. 0)       ;load-l2-map-from-cadr-physical, czrr

(DEF-DATA-FIELD MAP2C-META-BITS 6 0)            ;high two are hackable by dispatch inst.
(DEF-DATA-FIELD MAP2C-STATUS-CODE 3 6)
(DEF-DATA-FIELD MAP2C-ACCESS-CODE 2 8)          ;note one bit overlap with status-code
(DEF-DATA-FIELD MAP2C-ACCESS-STATUS-AND-META-BITS 10. 0)
(DEF-DATA-FIELD MAP2C-HARDWARE-READ-ACCESS 1 9.) ;hardware permits (at least) read access
(DEF-DATA-FIELD MAP2C-READ-ACCESS-IF-FORCE 1 10.);if force, (ie 40 bit in func dest that
                ;starts memory cycle) this bit used by hardware instead of above bit.  This
                ;allows PDL buffer routines to hack without clobbering map, etc.
                ;this bit turns out to be in same place for LAMBDA and EXP!
(def-data-field map2c-volatility 2 0)
;explorer has below fields in hardware.  We keep bits in usual place, frotzing them just
; before loading into hardware in LOAD-L2-MAP-FROM-CADR-PHYSICAL.
#+exp(def-data-field exp-map2c-volatility 2 11.)
;Note forced-bit is same for both, so definition above suffices.

(DEF-DATA-FIELD MAP2C-REPRESENTATION-TYPE 2 2)
(DEF-DATA-FIELD MAP2C-EXTRA-PDL-META-BIT 1 4)  ;on CADR, this was looked at by hardware
    ;by DISPATCH-ON-MAP-18 feature.  On LAMBDA and EXPLORER, the hardware function is
    ;replaced by the GC-VOLATILITY gate, so the bit has no special hardware function.
(DEF-DATA-FIELD MAP2C-OLDSPACE-META-BIT 1 5)   ;on CADR, this was looked at by hardware
    ;DISPATCH-ON-MAP-19 feature.  On LAMBDA, it still is. EXPLORER and AVP, this hardware
    ;function comes instead from the level-1 map.


(DEF-DATA-FIELD MAP2P-PHYSICAL-PAGE-NUMBER 22. 0)
(DEF-DATA-FIELD MAP2P-PHYSICAL-BYTE-CODE 2 22.) ;0 unless trying to hack bytes or 16 bit wds

;these definitions reduce need to conditionalize things per processor as well as being
; more modular.
(assign l2-map-status-code (plus map2c-status-code l2-map-control))
(assign l2-map-access-status-and-meta-bits
        (plus map2c-access-status-and-meta-bits l2-map-control))
(assign l2-map-representation-type (plus map2c-representation-type l2-map-control))
(assign l2-map-extra-pdl-meta-bit (plus map2c-extra-pdl-meta-bit l2-map-control))
(assign l2-map-oldspace-meta-bit (plus map2c-oldspace-meta-bit l2-map-control))
(assign l2-map-physical-page-number (plus map2p-physical-page-number l2-map-physical-page))

;;;FIELDS IN VMA WHEN WRITING MAP.
;;(DEF-DATA-FIELD MAP-WRITE-SECOND-LEVEL-MAP 24. 0)
;;(DEF-DATA-FIELD MAP-WRITE-ENABLE-SECOND-LEVEL-WRITE 1 25.)
;;(DEF-DATA-FIELD MAP-WRITE-ENABLE-FIRST-LEVEL-WRITE 1 26.)
;;(DEF-DATA-FIELD MAP-WRITE-FIRST-LEVEL-MAP 5 27.) ;NOTE NOT THE SAME AS WHERE IT READS

; DEFINITIONS OF FIELDS IN PAGE HASH TABLE

 ;WORD 1
(DEF-DATA-FIELD PHT1-VIRTUAL-PAGE-NUMBER 17. 8) ;ALIGNED SAME AS VMA
(DEF-DATA-FIELD PHT1-SWAP-STATUS-CODE 3 0)
 (DEF-DATA-FIELD PHT1-ALL-BUT-SWAP-STATUS-CODE 29. 3)
(DEF-DATA-FIELD PHT1-AGE 2 3)
 (DEF-DATA-FIELD PHT1-ALL-BUT-AGE-AND-SWAP-STATUS-CODE 27. 5)
(DEF-DATA-FIELD PHT1-MODIFIED-BIT 1 5)          ;SET IF PAGE MODIFIED
(DEF-DATA-FIELD PHT1-VALID-BIT 1 6)
 ;WORD 2  THESE ARE NOW THE SAME BIT POSITIONS AS IN THE SECOND LEVEL MAP on CADR.
(DEF-DATA-FIELD PHT2-META-BITS 6 26)
(def-data-field pht2-extra-pdl-meta-bit 1 32)
(def-data-field pht2-oldspace-meta-bit 1 33)
(def-data-field pht2-map-volatility 2 26)
(DEF-DATA-FIELD PHT2-MAP-STATUS-CODE 3 34)
(DEF-DATA-FIELD PHT2-MAP-ACCESS-CODE 2 36)
(DEF-DATA-FIELD PHT2-MAP-ACCESS-AND-STATUS-CODE 4 34)
(DEF-DATA-FIELD PHT2-ACCESS-STATUS-AND-META-BITS 12 26)
(def-data-field pht2-access-status-and-meta-bits-except-volatility 10 30)
(DEF-DATA-FIELD PHT2-PHYSICAL-PAGE-NUMBER 26 0)

; DEFINITIONS OF FIELDS IN THE ADDRESS

(DEF-DATA-FIELD VMA-MAP-BLOCK-PART 12. 13.)     ;ADDRESS BLOCK OF 32. PAGES
(DEF-DATA-FIELD VMA-PAGE-ADDR-PART 17. 8)       ;VIRTUAL PAGE NUMBER
(DEF-DATA-FIELD VMA-PHYS-PAGE-ADDR-PART 14. 8)  ;PHYSICAL PAGE NUMBER
(DEF-DATA-FIELD VMA-LOW-BITS 8 0)               ;ADDR WITHIN PAGE
(DEF-DATA-FIELD ALL-BUT-VMA-LOW-BITS 24. 8)

;NOTE: PGF-R, ETC CAN BE ENTERED RECURSIVELY IF THE PAGE IS SWAPPED OUT AND THE DISK
;ROUTINES FAULT WHEN REFERENCING THE DISK CONTROL.

;THESE COMMENTS APPLY TO SEQUENCE BREAK
;INTERRUPT MAY BE INSERTED -AFTER- THE READ CYCLE, HOWEVER
;IT IS EFFECTIVELY BEFORE SINCE ON DISMISS READ-MEMORY-DATA RESTORED FROM VMA!!
;NOTE THAT THIS ORDERING ALLOWS AN EFFECTIVE READ-PAUSE-WRITE CYCLE
;TO BE DONE JUST BY DOING A READ THEN A WRITE, EVEN
;THOUGH AFTER EACH CYCLE IS STARTED INTERRUPTS ARE CHECKED.

;To request a sequence-break, do
;       ((LOCATION-COUNTER) LOCATION-COUNTER)   ;Assure PC gets fetched
;       ((RG-MODE) ANDCA RG-MODE (A-CONSTANT 1_26.)) .  (note sense opposite from CADR)

;PUSHJ HERE ON PAGE FAULT, INTERRUPT REQUEST, OR SEQUENCE BREAK DURING READ CYCLE

PGF-R-SB-save-vma-in-t (JUMP-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-R-I)
        (call-xct-next sbser)   ;can clobber VMA, MD.
       ((m-t) q-typed-pointer vma)
        ((vma-start-read) m-t)
        (check-page-read)
        (popj)

  ;must not loop a la  CADR because PGF could be handled w/o mem cycle, so page-fault
  ;may persist even if memory cycle completed ok.
SBSER
#+lambda(POPJ-IF-BIT-SET (BYTE-FIELD 1 26.) RG-MODE)  ;Flush on no SB req
#+exp   (popj-if-bit-clear (byte-field 1 14.) mcr)
        ((M-TEM) M-FLAGS-NO-SEQUENCE-BREAK)     ;TURN INTO ILLOP IF TRAP AT BAD TIME
        (CALL-NOT-EQUAL M-TEM A-ZERO ILLOP)     ;NOTE WOULD PROBABLY DIE LATER ANYWAY
#+lambda((RG-MODE) IOR RG-MODE (A-CONSTANT 1_26.))
#+exp   ((mcr) andca mcr (a-constant 1_14.))

        (jump-not-equal m-zero a-defer-boot-char-mode sbser-process-boot-char)

        ((M-TEM) A-INHIBIT-SCHEDULING-FLAG)
        (JUMP-NOT-EQUAL M-TEM A-V-NIL SB-DEFER)
;---new run light
        ((m-tem3) vma)
        ((md) setz)
        ((vma) a-disk-run-light)
        ((vma-start-write) sub vma (a-constant 12))
        (check-page-write-map-reload-only)
        ((vma) m-tem3)
;---new run light
        ((M-DEFERRED-SEQUENCE-BREAK-FLAG) DPB M-ZERO A-FLAGS)
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-QSSTKG)
        (CALL-EQUAL M-TEM A-QCSTKG ILLOP)       ;SCHEDULER SHOULD HAVE DEFERED INTERRUPTS
        ((A-QLBNDH) A-QLBNDRH)                  ;ENSURE NO SPECIAL PDL OVERFLOW STORING STATUS
                                                ;REGULAR PDL IS PREWITHDRAWN, CAN'T OVERFLOW
        (CALL-XCT-NEXT SGLV)                    ;STORE CURRENT STATUS
       ((M-TEM) (A-CONSTANT (EVAL SG-STATE-RESUMABLE))) ;AND SWAP SPECIAL-PDL
        ((A-SG-TEM) A-V-NIL)                            ;Transmit NIL
        (JUMP-XCT-NEXT SG-ENTER)                        ;"CALL" SCHEDULER STACK GROUP
       ((M-A) A-QSSTKG)

SB-DEFER
;---new run light
        ((m-tem3) vma)
        ((m-tem4) md)
        ((vma) a-disk-run-light)
        ((vma) sub vma (a-constant 12))
        ((md) m-minus-one)
        ((m-tem) dpb m-zero q-all-but-typed-pointer a-qsstkg)
        (jump-equal m-tem a-qcstkg sb-light-off)
        ((vma-start-read) vma)
        (check-page-read-map-reload-only)
sb-light-off
        ((md) xor md a-minus-one)
        ((vma-start-write) vma)
        (check-page-write-map-reload-only)
        ((vma) m-tem3)
        ((md) m-tem4)

;---end of new run light
        (POPJ-AFTER-NEXT
           (M-DEFERRED-SEQUENCE-BREAK-FLAG) DPB (M-CONSTANT -1) A-FLAGS)
       (NO-OP)

sbser-process-boot-char
        ((M-DEFERRED-SEQUENCE-BREAK-FLAG) DPB M-ZERO A-FLAGS)
        ((A-QLBNDH) A-QLBNDRH)                  ;ENSURE NO SPECIAL PDL OVERFLOW STORING STATUS
                                                ;REGULAR PDL IS PREWITHDRAWN, CAN'T OVERFLOW

        ((a-sg-tem) a-qcstkg)                   ;this seems to be preserved by SGLV

        (CALL-XCT-NEXT SGLV)                    ;STORE CURRENT STATUS
       ((M-TEM) (A-CONSTANT (EVAL SG-STATE-RESUMABLE))) ;AND SWAP SPECIAL-PDL
        (call kbd-boot-char-xct-now)
        ;;here is an attempt at returning via an SDU continue command -- I don't know if it will work.
        ((m-a) a-sg-tem)                        ;try to return to prev stack group
                                                ;(can't go to scheduler since we
                                                ; may have interrupted it with
                                                ; inhibit scheduling flag on)
        ((A-SG-TEM) A-V-NIL)                    ;Transmit NIL
        (JUMP SG-ENTER)

;PUSHJ HERE ON PAGE FAULT OR INTERRUPT REQUEST DURING READ CYCLE
PGF-R-I (declare (clobbers a-tem))
        (JUMP-CONDITIONAL NO-PG-FAULT INTR)     ;IF NO PG FAULT, TAKE INTERRUPT

;PUSHJ HERE ON READ CYCLE PAGE FAULT WHEN DESIRE NOT TO TAKE INTERRUPT
;GUARANTEED TO RETURN WITHOUT ANY INTERRUPTS HAPPENING, OR ELSE TO GO TO ILLOP
;BUT SEE THE COMMENTS ON THE DEFINITION OF CHECK-PAGE-READ-NO-INTERRUPT
PGF-R (declare (clobbers a-tem) (must-avoid intr))
        ((MD) VMA)                              ;ADDRESS THE MAP
        (no-op)         ;give map time to set up.
        (DISPATCH-XCT-NEXT L2-MAP-STATUS-CODE D-PGF)
       ((M-PGF-WRITE) DPB M-ZERO A-FLAGS)
;IF IT RETURNS HERE, WE RESTART THE READ REFERENCE
        ((VMA-START-READ) A-PGF-VMA)
        (POPJ-AFTER-NEXT NO-OP)
       (CHECK-PAGE-READ-NO-INTERRUPT)           ;DIDN'T ENTIRELY SUCCEED, TRY AGAIN

PGF-R-MAP-RELOAD-ONLY (declare (clobbers a-tem) (must-avoid intr))
        ((MD) VMA)                              ;ADDRESS THE MAP
        (no-op)         ;give map time to set up.
        (DISPATCH-XCT-NEXT L2-MAP-STATUS-CODE D-PGF-MAP-RELOAD-ONLY)
       ((M-PGF-WRITE) DPB M-ZERO A-FLAGS)
;IF IT RETURNS HERE, WE RESTART THE READ REFERENCE
        ((VMA-START-READ) A-PGF-VMA)
        (POPJ-AFTER-NEXT NO-OP)
       (CHECK-PAGE-READ-MAP-RELOAD-ONLY)                ;DIDN'T ENTIRELY SUCCEED, TRY AGAIN

;PUSHJ HERE ON PAGE FAULT, INTERRUPT, OR SEQUENCE BREAK DURING WRITE CYCLE --not used.
;PGF-W-SB(declare (clobbers a-tem))
;       (JUMP-CONDITIONAL PG-FAULT-OR-INTERRUPT PGF-W-I)
  ;see note above
;       (JUMP SBSER)

PGF-W-UNBOXED   (declare (clobbers a-tem a-pgf-mode) (must-avoid intr))
        (JUMP-XCT-NEXT PGF-W-1)
       ((A-PGF-MODE) (a-constant (plus (byte-value q-data-type dtp-fix)
                                       4)))     ;no binding, no write-force, unboxed.

PGF-W-BIND      (declare (clobbers a-tem a-pgf-mode) (must-avoid intr))
        (JUMP-XCT-NEXT PGF-W-1)
       ((A-PGF-MODE) (a-constant (plus (byte-value q-data-type dtp-fix)
                                       1)))     ;binding, no write-force, no unboxed.

PGF-W-FORCE     (declare (clobbers a-tem a-pgf-mode) (must-avoid intr))
        (JUMP-XCT-NEXT PGF-W-1)
       ((A-PGF-MODE) (a-constant (plus (byte-value q-data-type dtp-fix)
                                       2)))     ;no binding, write-force, no unboxed.

;PUSHJ HERE ON PAGE FAULT OR INTERRUPT REQUEST DURING WRITE CYCLE
PGF-W-I (declare (clobbers a-tem a-pgf-mode))
;       (JUMP-CONDITIONAL NO-PG-FAULT INTR)     ;NO PAGE FAULT, THEN TAKE INTERRUPT
     ;; Interrupts on write cycles have to be sure to do a full vma-start-write on the
     ;; original vma/md before returning, so the volatilities are correctly latched.
        (jump-if-page-fault pgf-w)
        (call intr)
        ((vma-start-write) vma)
        (check-page-write-no-interrupt)
        (popj)

;PUSHJ HERE ON PAGE FAULT WHEN DESIRE NOT TO TAKE INTERRUPT
;GUARANTEED TO RETURN WITH NO INTERRUPT, OR TO GO TO ILLOP
;BUT SEE THE COMMENTS ON THE DEFINITION OF CHECK-PAGE-READ-NO-INTERRUPT
PGF-W (declare (clobbers a-tem a-pgf-mode))
        ((A-PGF-MODE) (a-constant (plus (byte-value q-data-type dtp-fix)
                                        0)))    ;no bind, no force, no unboxed.
PGF-W-1 ((A-PGF-WMD) MD)                        ;SAVE DATA BEING WRITTEN
        ((MD) VMA)                              ;ADDRESS THE MAP
        ((a-pgf-vma) vma)  ;give map time to set up. This instruction substituted for
           ; no-op to make code less marginal, ie, dependant on all paths in d-pgf
           ; getting to pgf-save.
        (DISPATCH-XCT-NEXT L2-MAP-STATUS-CODE D-PGF)
       ((M-PGF-WRITE) DPB (M-CONSTANT -1) A-FLAGS)
;IF IT RETURNS HERE, WE RESTART THE WRITE REFERENCE
        ((WRITE-MEMORY-DATA) A-PGF-WMD)
        ((VMA-START-WRITE) A-PGF-VMA)           ; ASSUMES WE WERE TRYING TO DO A WRITE CYCLE
        (POPJ-AFTER-NEXT NO-OP)
       (CHECK-PAGE-WRITE-RETRY)                 ;DIDN'T ENTIRELY SUCCEED, TRY AGAIN

PGF-W-MAP-RELOAD-ONLY (declare (clobbers a-tem a-pgf-mode))
        ((A-PGF-MODE) (a-constant (plus (byte-value q-data-type dtp-fix)
                                        0)))    ;no bind, no force, no unboxed.
        ((A-PGF-WMD) MD)                        ;SAVE DATA BEING WRITTEN
        ((MD) VMA)                              ;ADDRESS THE MAP
        ((a-pgf-vma) vma)  ;give map time to set up. This instruction substituted for
           ; no-op to make code less marginal, ie, dependant on all paths in d-pgf
           ; getting to pgf-save.
        (DISPATCH-XCT-NEXT L2-MAP-STATUS-CODE D-PGF-MAP-RELOAD-ONLY)
       ((M-PGF-WRITE) DPB (M-CONSTANT -1) A-FLAGS)
;IF IT RETURNS HERE, WE RESTART THE WRITE REFERENCE
        ((WRITE-MEMORY-DATA) A-PGF-WMD)
        ((VMA-START-WRITE) A-PGF-VMA)           ; ASSUMES WE WERE TRYING TO DO A WRITE CYCLE
        (POPJ-AFTER-NEXT NO-OP)
       (CHECK-PAGE-WRITE-MAP-RELOAD-ONLY)       ;DIDN'T ENTIRELY SUCCEED, TRY AGAIN

(LOCALITY D-MEM)
(START-DISPATCH 3 0)            ;DISPATCH ON MAP STATUS
D-PGF   (P-BIT PGF-MAP-MISS)    ;0 LEVEL 1 OR 2 MAP NOT VALID
        (P-BIT PGF-MAP-MISS)    ;1 META BITS ONLY, TAKE AS MAP MISS
        (PGF-RDONLY)            ;2 WRITE IN READ ONLY, note jump not PUSHJ
        (P-BIT PGF-RWF)         ;3 WRITE IN READ/WRITE FIRST
        (P-BIT ILLOP)           ;4 READ/WRITE
        (PGF-PDL)               ;5 MAY BE IN PDL BUFFER, note jump.
        (PGF-MAR)               ;6 POSSIBLE MAR BREAK, note jump.
        (P-BIT ILLOP)           ;7 nubus physical in pht2
(END-DISPATCH)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)            ;DISPATCH ON MAP STATUS
D-PGF-MAP-RELOAD-ONLY
        (P-BIT PGF-MAP-MISS-RELOAD-ONLY)        ;0 LEVEL 1 OR 2 MAP NOT VALID
        (P-BIT PGF-MAP-MISS-RELOAD-ONLY)        ;1 META BITS ONLY, TAKE AS MAP MISS
        (P-BIT ILLOP) ;PGF-RDONLY       ;2 WRITE IN READ ONLY, note jump not PUSHJ
        (P-BIT pgf-rwf) ;PGF-RWF                ;3 WRITE IN READ/WRITE FIRST ;***
        (P-BIT ILLOP)           ;4 READ/WRITE
        (P-BIT ILLOP) ;PGF-PDL          ;5 MAY BE IN PDL BUFFER, note jump.
        (P-BIT ILLOP) ;PGF-MAR          ;6 POSSIBLE MAR BREAK, note jump.
        (P-BIT ILLOP)           ;7 nubus physical in pht2
(END-DISPATCH)
(LOCALITY I-MEM)

;THIS DISPATCH IS FOR GETTING META BITS FROM MAP[MD] WITHOUT SWAPPING IN
;WHAT IT POINTS TO.  SMASHES VMA.
(LOCALITY D-MEM)
(START-DISPATCH 3 0)
D-GET-MAP-BITS
        (P-BIT INHIBIT-XCT-NEXT-BIT GET-MAP-BITS) ;0 LEVEL 1 OR 2 MAP NOT VALID
        (P-BIT R-BIT)                           ;1 GOT MAP BITS ANYWAY
        (P-BIT R-BIT)                           ;2 READ ONLY
        (P-BIT R-BIT)                           ;3 READ/WRITE FIRST
        (P-BIT R-BIT)                           ;4 READ/WRITE
        (P-BIT R-BIT)                           ;5 MAY BE IN PDL BUFFER
        (P-BIT R-BIT)                           ;6 POSSIBLE MAR BREAK
        (P-BIT r-bit)                           ;7 nubus physical in pht2
(END-DISPATCH)
(LOCALITY I-MEM)

;MAP MISS WHEN TRYING TO GET META BITS.  GET FROM REGION TABLE, SET UP META-BITS-ONLY STATUS
;this runs at "page-fault" level and had better not be called by anything already
;within the page-fault handler or variables will be overstored.  It can be called from
;extra-pdl-trap, which can be called from pdl-buffer-dump, but fortunately, none of that
;is within the page fault level.
GET-MAP-BITS
        ((A-META-BITS-MAP-RELOADS) ADD M-ZERO A-META-BITS-MAP-RELOADS ALU-CARRY-IN-ONE)
        (CALL-XCT-NEXT PGF-SAVE-1)      ;Save M-A, M-B, M-T
       ((A-PGF-VMA) MD)                 ;Address of reference, also saves MD
                                        ;LEVEL-1-MAP-MISS also gets it out of here!
        ((M-TEM) L1-MAP-L2-BLOCK-SELECT L1-MAP) ;Check for level 1 map miss
        (CALL-EQUAL M-TEM (A-CONSTANT 177) LEVEL-1-MAP-MISS)
        ((M-A) DPB M-ZERO Q-ALL-BUT-POINTER A-PGF-VMA)  ;address of reference
     ;; This needs to change with new fixed area scheme.  Right now fixed areas are
     ;; legislated to be contain homogeneous storage.
        (JUMP-GREATER-OR-EQUAL-XCT-NEXT         ;Check for A-memory or I/O address
                M-A A-LOWEST-DIRECT-VIRTUAL-ADDRESS
                GET-MAP-BITS-1)
       ((MD) (A-CONSTANT (PLUS (BYTE-MASK %%REGION-OLDSPACE-META-BIT)
                               (BYTE-MASK %%REGION-EXTRA-PDL-META-BIT)
                               (BYTE-VALUE %%REGION-REPRESENTATION-TYPE
                                           %REGION-REPRESENTATION-TYPE-LISP))))
        (CALL-XCT-NEXT XRGN1)                   ;Normal address, get meta bits from region
       ((M-A) Q-POINTER M-A (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL-EQUAL M-T A-V-NIL ILLOP)          ;Region not found
get-map-bits-2
        ((VMA-START-READ) ADD M-T A-V-REGION-BITS)      ;Fetch meta bits
        (ILLOP-IF-PAGE-FAULT)
GET-MAP-BITS-1
        ((m-a) (lisp-byte %%region-map-bits) md)   ;save before gets clobbered. also right adjust
        ((m-lam) a-pgf-vma)
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number m-lam)
        ((m-tem) xor m-minus-one a-tem)                ;Complement volatility.
        ((MD) A-PGF-VMA)        ;address map
        ((m-a) dpb m-a MAP2C-META-BITS
                (A-CONSTANT (BYTE-VALUE MAP2C-STATUS-CODE %PHT-MAP-STATUS-META-BITS-ONLY)))
#+lambda((l2-map-control) dpb m-tem map2c-volatility a-a)
#+exp   ((m-tem) dpb m-tem map2c-volatility a-a)
#+exp   ((vma-write-l2-map-control) dpb m-tem exp-map2c-volatility a-tem)
        ((#+lambda L2-MAP-PHYSICAL-PAGE
          #+exp vma-write-l2-map-physical-page) A-ZERO)         ;CLEAR OUT TO REDUCE CONFUSION

        (JUMP-XCT-NEXT PGF-RESTORE)                     ;Restore M-A, M-B, M-T
       ((VMA) A-V-NIL)          ;Don't leave garbage in VMA.


;PDL BUFFER HANDLING CONVENTIONS:
;  THE LINEAR PUSHDOWN LIST MUST ALWAYS BE COMPOSED OF PAGES FROM AN AREA WHOSE
;REGION-BITS Q HAS %PHT-MAP-STATUS-PDL-BUFFER IN THE MAP STATUS PORTION OF ITS
;%%REGION-MAP-BITS FIELD.  THUS ANY MEMORY CYCLE REF'ING
;SUCH AN AREA WILL TRAP AND COME HERE, WHERE THE CODE CHECKS TO SEE IF IT IS REALLY
;IN THE PDL-BUFFER NOW.  IF NOT, IT TURNS ON R/W ACCESS TEMPORARILY AND PERFORMS THE
;REQUESTED CYCLE, ETC.
;  THESE PAGES ARE TREATED ENTIRELY AS NORMAL PAGES FOR SWAPPING PURPOSES, AND MAY
;EVEN BE SWAPPED OUT WHILE ACTUALLY RESIDENT IN THE PDL-BUFFER! THE ONLY DIFFERENCE
;IS THAT THE PAGE MUST ALWAYS BE WRITTEN TO THE DISK ON SWAP-OUT, SINCE THE R-W-F
;MECHANISM IS NOT AVAILABLE TO KEEP TRACK OF WHETHER IT HAS ACTUALLY BEEN MODIFIED.
;  PDL-BUFFER-POINTER IS TAKEN TO MARK THE HIGHEST PDL-BUFFER LOCN WHICH IS REALLY VALID.

PGF-PDL (JUMP-IF-BIT-SET M-PGF-WRITE PGF-W-PDL)
;READ REFERENCE TO LOCATION THAT MAY BE IN THE PDL BUFFER
PGF-R-PDL  (declare (local a-pdl-buffer-read-faults))
        ((M-PGF-TEM) SUB PDL-BUFFER-POINTER A-PDL-BUFFER-HEAD)
        ((M-PGF-TEM) ADD M-PGF-TEM (A-CONSTANT 1))      ;*** THIS CODE COULD USE BUMMING ***
        ((M-PGF-TEM) PDL-BUFFER-ADDRESS-MASK M-PGF-TEM) ;COMPUTE # ACTIVE WDS IN PDL-BUFFER
        ((A-PGF-B) ADD M-PGF-TEM A-PDL-BUFFER-VIRTUAL-ADDRESS)
        ((M-PGF-TEM) Q-POINTER VMA)     ;GET ADDRESS BEING REFERENCED SANS EXTRA BITS
        (JUMP-LESS-THAN M-PGF-TEM A-PDL-BUFFER-VIRTUAL-ADDRESS PGF-R-NOT-REALLY-IN-PDL-BUFFER)
        (JUMP-GREATER-THAN M-PGF-TEM A-PGF-B PGF-R-NOT-REALLY-IN-PDL-BUFFER) ;GREATER BECAUSE
                                                        ;(PP) IS A VALID WD.
        ;READ REFERENCE TO LOCATION THAT IS IN THE PDL BUFFER
        ((A-PDL-BUFFER-READ-FAULTS) ADD A-PDL-BUFFER-READ-FAULTS M-ZERO ALU-CARRY-IN-ONE)
        ((M-PGF-TEM) SUB M-PGF-TEM
                A-PDL-BUFFER-VIRTUAL-ADDRESS)  ;GET RELATIVE PDL LOC REFERENCED
        ((A-PGF-A) PDL-BUFFER-INDEX)    ;DON'T CLOBBER PDL-BUFFER-INDEX
        ((PDL-BUFFER-INDEX) ADD M-PGF-TEM A-PDL-BUFFER-HEAD)    ;TRUNCATES TO 10 BITS
        (POPJ-AFTER-NEXT (MD) C-PDL-BUFFER-INDEX)
       ((PDL-BUFFER-INDEX) A-PGF-A)

;READ REFERENCE TO LOCATION NOT IN THE PDL BUFFER, BUT IT MIGHT HAVE BEEN.
PGF-R-NOT-REALLY-IN-PDL-BUFFER
#-LAMBDA(begin-comment)
        ((VMA-START-READ-FORCE) VMA)    ;READ OUT THAT LOCATION
        (ILLOP-IF-PAGE-FAULT)           ;VALID-IF-FORCE should be on for may-be-in-pdl-buffer
        (POPJ-AFTER-NEXT
         (A-PDL-BUFFER-MEMORY-FAULTS) ADD A-PDL-BUFFER-MEMORY-FAULTS M-ZERO ALU-CARRY-IN-ONE)
       (NO-OP)
#-lambda (end-comment)

#-exp(begin-comment)  ;PDL-FORCE feature does not work on explorer.. so..
        ((M-PGF-TEM) l2-map-control)    ;SAVE CORRECT MAP CONTENTS
        ((vma-write-l2-map-control) IOR M-PGF-TEM               ;TURN ON ACCESS
                (A-CONSTANT (BYTE-VALUE MAP2C-ACCESS-CODE 3))) ;R/W
;Maybe putting this insn here will avoid hardware lossage??
        ((A-PDL-BUFFER-MEMORY-FAULTS) ADD A-PDL-BUFFER-MEMORY-FAULTS M-ZERO ALU-CARRY-IN-ONE)
        ((VMA-START-READ) MD)                   ;READ OUT THAT LOCATION
        (ILLOP-IF-PAGE-FAULT)                   ;I THOUGHT WE JUST TURNED ON ACCESS
        ((A-PGF-WMD) READ-MEMORY-DATA)          ;SAVE CONTENTS
        ((MD) VMA)                              ;ADDRESS THE MAP
        (no-op)
        ((vma-write-l2-map-control) M-PGF-TEM)  ;RESTORE THE MAP
        (POPJ-AFTER-NEXT                        ;RESTORE REGISTERS AND RETURN
         (VMA) MD)
       ((MD) A-PGF-WMD)
#-exp(end-comment)

;WRITE REFERENCE TO LOCATION THAT MAY BE IN THE PDL BUFFER
PGF-W-PDL  (declare (local a-pdl-buffer-write-faults))
        ((M-PGF-TEM) SUB PDL-BUFFER-POINTER A-PDL-BUFFER-HEAD)
        ((M-PGF-TEM) ADD M-PGF-TEM (A-CONSTANT 1))      ;*** THIS CODE COULD USE BUMMING ***
        ((M-PGF-TEM) PDL-BUFFER-ADDRESS-MASK M-PGF-TEM) ;COMPUTE # ACTIVE WDS IN PDL-BUFFER
        ((A-PGF-B) ADD M-PGF-TEM A-PDL-BUFFER-VIRTUAL-ADDRESS) ;HIGHEST VIRT LOC IN P.B,
        ((M-PGF-TEM) Q-POINTER VMA)     ;GET ADDRESS BEING REFERENCED SANS EXTRA BITS
        (JUMP-LESS-THAN M-PGF-TEM A-PDL-BUFFER-VIRTUAL-ADDRESS PGF-W-NOT-REALLY-IN-PDL-BUFFER)
        (JUMP-GREATER-THAN M-PGF-TEM A-PGF-B PGF-W-NOT-REALLY-IN-PDL-BUFFER) ;GREATER BECAUSE
                                                        ;(PP) IS A VALID WD
        ;WRITE REFERENCE TO LOCATION THAT IS IN THE PDL BUFFER
        ((A-PDL-BUFFER-WRITE-FAULTS) ADD A-PDL-BUFFER-WRITE-FAULTS M-ZERO ALU-CARRY-IN-ONE)
        ((M-PGF-TEM) SUB M-PGF-TEM
                A-PDL-BUFFER-VIRTUAL-ADDRESS)  ;GET RELATIVE PDL LOC REFERENCED
        ((A-PGF-A) PDL-BUFFER-INDEX)    ;DON'T CLOBBER PDL-BUFFER-INDEX
        ((PDL-BUFFER-INDEX) ADD M-PGF-TEM A-PDL-BUFFER-HEAD)    ;TRUNCATES TO 10 BITS
        ((MD) A-PGF-WMD)
;       (POPJ-AFTER-NEXT
;        (C-PDL-BUFFER-INDEX) MD)       ;DO THE WRITE
;       ((PDL-BUFFER-INDEX) A-PGF-A)    ;RESTORE REGS AND RETURN FROM FAULT

        ((c-pdl-buffer-index) md)
        ((pdl-buffer-index) a-pgf-a)

#+exp   ((md) a-pgf-vma)  ;address the map since dropping through
                          ;note that first inst doesn't reference the map
  ;drop through

;WRITE REFERENCE TO LOCATION NOT IN THE PDL BUFFER, BUT IT MIGHT HAVE BEEN
;** note ** volatility not maintained for PDL pages!!
;** not necessarily true:  this can only be reached by
;(check-page-write) which is always followed by a gc-write-test
;in the caller.
PGF-W-NOT-REALLY-IN-PDL-BUFFER  (declare (local a-pdl-buffer-memory-faults))
#-lambda(begin-comment)
        ((WRITE-MEMORY-DATA-START-WRITE-FORCE) A-PGF-WMD) ;WRITE INTO THAT LOCATION
        (ILLOP-IF-PAGE-FAULT)                   ;VALID-IF-FORCE should be on for p.b. pages.
        (POPJ-AFTER-NEXT
         (A-PDL-BUFFER-MEMORY-FAULTS) ADD A-PDL-BUFFER-MEMORY-FAULTS M-ZERO ALU-CARRY-IN-ONE)
       (NO-OP)
#-lambda(end-comment)

#-exp(begin-comment)
        ;don't use l2-map on first inst here
        ((A-PDL-BUFFER-MEMORY-FAULTS) ADD A-PDL-BUFFER-MEMORY-FAULTS M-ZERO ALU-CARRY-IN-ONE)
        ((M-PGF-TEM) l2-map-control)    ;SAVE CORRECT MAP CONTENTS
        ((vma-write-l2-map-control) IOR M-PGF-TEM               ;TURN ON ACCESS
                (A-CONSTANT (BYTE-VALUE MAP2C-ACCESS-CODE 3))) ;R/W
        ((VMA) MD)                              ;WRITE INTO THAT LOCATION
        ((WRITE-MEMORY-DATA-START-WRITE) A-PGF-WMD)
        (ILLOP-IF-PAGE-FAULT)                   ;I THOUGHT WE JUST TURNED ON ACCESS
        ((MD) VMA)                              ;ADDRESS THE MAP
        (no-op)
        ((vma-write-l2-map-control) M-PGF-TEM)  ;RESTORE THE MAP
        (POPJ-AFTER-NEXT                        ;RESTORE REGISTERS AND RETURN
         (VMA) MD)
       ((MD) A-PGF-WMD)
#-exp(end-comment)

pgf-r-pdl-check-immediately
  (call pgf-pdl-check-immediately)
  ((a-pdl-buffer-read-faults) add a-pdl-buffer-read-faults m-zero alu-carry-in-one)
  ((md) c-pdl-buffer-index)
  ((m-garbage) micro-stack-data-pop) ;; return address in pgf-map-miss
  ((m-garbage) micro-stack-data-pop) ;;pop return-address in pgf-r
  (popj-after-next (pdl-buffer-index) a-pgf-a)
  (no-op)

;;; Figure out how to deal with volatility here.
;pgf-w-pdl-check-immediately
;  (call pgf-pdl-check-immediately)
;  ((a-pdl-buffer-write-faults) add a-pdl-buffer-write-faults m-zero alu-carry-in-one)
;  (popj-after-next (c-pdl-buffer-index) md)
; ((pdl-buffer-index) a-pgf-a)

pgf-pdl-check-immediately
  ((m-pgf-tem) sub pdl-buffer-pointer a-pdl-buffer-head)
  ((m-pgf-tem) pdl-buffer-address-mask m-pgf-tem)
  ((a-pgf-b)   m+a+1 m-pgf-tem a-pdl-buffer-virtual-address)
  ((m-pgf-tem) q-pointer vma)
  (jump-less-than m-pgf-tem a-pdl-buffer-virtual-address pgf-pdl-check-not-in-pdl-buffer)
  (jump-greater-than m-pgf-tem a-pgf-b pgf-pdl-check-not-in-pdl-buffer)

  ((m-pgf-tem) sub m-pgf-tem a-pdl-buffer-virtual-address)
  (popj-after-next (a-pgf-a) pdl-buffer-index)
 ((pdl-buffer-index) add m-pgf-tem a-pdl-buffer-head)

pgf-pdl-check-not-in-pdl-buffer
  ((m-garbage) micro-stack-data-pop)
  (popj)


;SAVE REGISTERS UPON ENTERING PAGE FAULT HANDLER
PGF-SAVE (declare (clobbers a-pgf-vma a-pgf-a a-pgf-b a-pgf-t)
                  (saves (vma a-pgf-vma)))
        ((A-PGF-VMA) VMA)
PGF-SAVE-1
        (declare (clobbers a-pgf-a a-pgf-b a-pgf-t)
                 (saves (a-a a-pgf-a) (a-b a-pgf-b) (a-t a-pgf-t)))
        ((A-PGF-A) M-A)
        (POPJ-AFTER-NEXT (A-PGF-B) M-B)
       ((A-PGF-T) M-T)

;RESTORE REGISTERS AND LEAVE PAGE FAULT HANDLER
;DOESN'T RESTORE VMA SINCE CYCLE RESTARTER (POPJED TO) WILL DO THAT
PGF-RESTORE
        (declare (restores (a-a a-pgf-a) (a-b a-pgf-b) (a-t a-pgf-t)))
;; Tempoary map checking code
;CHECK-WIRED-LEVEL-2-MAP-CONTROL
;        ((M-A) DPB (BYTE-FIELD 1 12.) M-MINUS-ONE A-ZERO)
;        (JUMP-EQUAL M-A A-ZERO CHECK-WIRED-LEVEL-2-MAP-CONTROL-2)
;        ((M-T) MD)
;        ((MD) SETZ)
;CHECK-WIRED-LEVEL-2-MAP-CONTROL-1
;        (CALL-EQUAL L2-MAP-CONTROL A-ZERO ILLOP-DEBUG)
;        ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))
;        (JUMP-LESS-THAN MD A-A CHECK-WIRED-LEVEL-2-MAP-CONTROL-1)
;        ((MD) M-T)
;CHECK-WIRED-LEVEL-2-MAP-CONTROL-2
;
; end of tempoary code
        ((M-A) A-PGF-A)
        (POPJ-AFTER-NEXT (M-B) A-PGF-B)
       ((M-T) A-PGF-T)

 ;Page fault level routines call these in order to be able to take map-reload-type page
 ;faults recursively.  Even after saving here, any recursive page fault which
 ;touched the disk would lose since these variables would get stored over by the
 ;call to DISK-PGF-SAVE at DISK-SWAP-HANDLER.
DISK-PGF-SAVE (declare (saves (a-pgf-vma a-disk-save-pgf-vma) (a-pgf-wmd a-disk-save-pgf-wmd)
                (a-pgf-t a-disk-save-pgf-t) (a-pgf-a a-disk-save-pgf-a)
                (a-pgf-b a-disk-save-pgf-b) (a-pgf-mode a-disk-save-mode)
                (a-1 a-disk-save-1) (a-2 a-disk-save-2)))
        ((A-DISK-SAVE-PGF-VMA) A-PGF-VMA)       ;Save page fault handler variables
        ((A-DISK-SAVE-PGF-WMD) A-PGF-WMD)       ;in case of recursive fault
        ((A-DISK-SAVE-PGF-T) A-PGF-T)
        ((A-DISK-SAVE-PGF-A) A-PGF-A)
        ((A-DISK-SAVE-PGF-B) A-PGF-B)
        ((A-DISK-SAVE-MODE) A-PGF-MODE)
        (POPJ-AFTER-NEXT
         (A-DISK-SAVE-1) M-1)                   ;Clobbered by disk routine
       ((A-DISK-SAVE-2) M-2)                    ;..

DISK-PGF-RESTORE (declare (restores (a-pgf-vma a-disk-save-pgf-vma) (a-pgf-wmd a-disk-save-pgf-wmd)
                (a-pgf-t a-disk-save-pgf-t) (a-pgf-a a-disk-save-pgf-a)
                (a-pgf-b a-disk-save-pgf-b) (a-pgf-mode a-disk-save-mode)
                (a-1 a-disk-save-1) (a-2 a-disk-save-2)))
        ((M-2) A-DISK-SAVE-2)           ;Restore registers
        ((M-1) A-DISK-SAVE-1)
        ((A-PGF-MODE) A-DISK-SAVE-MODE)
        ((A-PGF-B) A-DISK-SAVE-PGF-B)
        ((A-PGF-A) A-DISK-SAVE-PGF-A)
        ((A-PGF-T) A-DISK-SAVE-PGF-T)
        (POPJ-AFTER-NEXT
         (A-PGF-WMD) A-DISK-SAVE-PGF-WMD)
       ((A-PGF-VMA) A-DISK-SAVE-PGF-VMA)

moby-pgf-save   ;save stuff on PDL in fully boxed format
        ((pdl-push) a-pgf-a)
        ((pdl-push) a-pgf-b)
        ((pdl-push) a-pgf-t)
        ((m-tem) a-pgf-mode)
        (jump-if-bit-set (byte-field 1 2) m-tem m-pgfs2)  ;unboxed.
        ((pdl-push) a-pgf-vma)
        ((pdl-push) a-pgf-wmd)
m-pgfs1 ((pdl-push) a-pgf-mode)  ;must be last
        (popj)

;unboxed store.  This will do for now, when traps to macrocode are possible,
; it will need to find header and store the vma as pointer to header plus offset.
m-pgfs2 ((m-tem) a-pgf-vma)
        ((pdl-push) ldb m-tem (byte-field 20 20) (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) ldb m-tem (byte-field 20 0) (a-constant (byte-value q-data-type dtp-fix)))
        ((m-tem) a-pgf-wmd)
        ((pdl-push) ldb m-tem (byte-field 20 20) (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) ldb m-tem (byte-field 20 0) (a-constant (byte-value q-data-type dtp-fix)))
        (jump m-pgfs1)

moby-pgf-restore
        ((a-pgf-mode) pdl-pop)
        ((m-tem) a-pgf-mode)
        (jump-if-bit-set (byte-field 1 2) m-tem m-pgfr2)        ;unboxed.
        ((a-pgf-wmd) pdl-pop)
        ((a-pgf-vma) pdl-pop)
m-pgfr1 ((a-pgf-t) pdl-pop)
        ((a-pgf-b) pdl-pop)
        ((a-pgf-a) pdl-pop)
        (popj)

m-pgfr2 ((m-tem) pdl-pop)
        ((a-pgf-wmd) dpb pdl-pop (byte-field 20 20) a-tem)
        ((m-tem) pdl-pop)
        ((a-pgf-vma) dpb pdl-pop (byte-field 20 20) a-tem)
        (jump m-pgfr1)

moby-pgf-retrieve
        ((a-pgf-mode) pdl-pop)
        ((m-tem) a-pgf-mode)
        (jump-if-bit-set (byte-field 1 2) m-tem moby-pgf-ret2)  ;unboxed.
        ((a-pgf-wmd) pdl-pop)
        (popj-after-next (a-pgf-vma) pdl-pop)
       ((pdl-buffer-pointer) add pdl-buffer-pointer (a-constant 3)) ;undo these pops.

moby-pgf-ret2
        ((m-tem) pdl-pop)
        ((a-pgf-wmd) dpb pdl-pop (byte-field 20 20) a-tem)
        ((m-tem) pdl-pop)
        (popj-after-next (a-pgf-vma) dpb pdl-pop (byte-field 20 20) a-tem)
       ((pdl-buffer-pointer) add pdl-buffer-pointer (a-constant 5))   ;undo these pops.

;ROUTINE TO HANDLE LEVEL-1 MAP MISSES.  CALLED FROM PGF-MAP-MISS AND FROM GET-MAP-BITS.
;ADDRESS IN MD ON CALL AND RETURN, VMA CLOBBERED.  PGF-SAVE MUST HAVE BEEN CALLED.
;What we do here is throw away a section of the level 2 map and make the level 1 map point
;to it.  When we go around again, we get a level 2 map miss and set it up properly.
LEVEL-1-MAP-MISS
        (declare (clobbers a-pgf-tem) (values a-a a-t))
        ((A-FIRST-LEVEL-MAP-RELOADS) ADD A-FIRST-LEVEL-MAP-RELOADS M-ZERO ALU-CARRY-IN-ONE)
        ((#+lambda L1-MAP
          #+exp vma-write-l1-map)
                 SELECTIVE-DEPOSIT L1-MAP L1-MAP-META-BITS  ;dont clobber meta bits.
                A-SECOND-LEVEL-MAP-REUSE-POINTER)       ;ALLOCATE A BLOCK OF LVL 2 MAP
        ((M-T) A-SECOND-LEVEL-MAP-REUSE-POINTER)        ;note no meta bits.
        ((MD M-A) SELECTIVE-DEPOSIT MD VMA-MAP-BLOCK-PART A-ZERO) ;-> 1ST ENTRY IN BLOCK

#-exp (begin-comment)
        ((m-pgf-tem) ADD M-T (A-CONSTANT 1200))         ;REVERSE 1ST LVL MAP IN 1200-1377
                                                        ; (see uc-initialization)
        ((VMA-START-READ) M-PGF-TEM)
        (ILLOP-IF-PAGE-FAULT)
#-exp (end-comment)

#-lambda(begin-comment)
        ((M-PGF-TEM) ADD M-T (A-CONSTANT 2000))  ;reverse 1st level map in 2000-2177 of A mem
        ((OA-REG-HIGH) DPB M-PGF-TEM OAH-A-SRC A-ZERO)
        ((MD) A-GARBAGE)
#-lambda(end-comment)

        (JUMP-LESS-THAN md A-ZERO PGF-L1C) ;DON'T WRITE MAP IF NO PREVIOUS
#+exp   ((m-tem) ldb (byte 12. 13.) md)
#+exp   (call-equal m-tem a-zero illop)
        ((#+lambda L1-MAP
          #+exp vma-write-l1-map) SELECTIVE-DEPOSIT L1-MAP L1-MAP-META-BITS
                (A-CONSTANT 177))                       ;AND 177-IFY OLD 1ST LVL MAP
PGF-L1C
        ((md) M-A)

#-exp(begin-comment)
        ((vma-start-write) m-pgf-tem)
        (ILLOP-IF-PAGE-FAULT)
#-exp(end-comment)

#-lambda(begin-comment)
        ((OA-REG-LOW) DPB M-PGF-TEM OAL-A-DEST A-ZERO)
        ((A-GARBAGE) md)
#-lambda(end-comment)

        ((M-T) (M-CONSTANT 40))                         ;DO ALL 32. ENTRIES IN BLOCK
PGF-L1A
        ((#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) (A-CONSTANT 0))
                                ;FILL 2ND-LEVEL MAP WITH MAP-MISS (0)
  ;     (NO-OP)
        ((MD M-A) ADD M-A (A-CONSTANT (BYTE-VALUE VMA-PAGE-ADDR-PART 1)))
        (JUMP-GREATER-THAN-XCT-NEXT M-T (A-CONSTANT 1) PGF-L1A)
       ((M-T) SUB M-T (A-CONSTANT 1))
        ((VMA) A-V-NIL)         ;Don't leave garbage in VMA.

;; Tempoary map checking code
;CHECK-PGF-L1-L2-MAP-CONTROL
;        ((M-A) DPB (BYTE-FIELD 1 12.) M-MINUS-ONE A-ZERO)
;        (JUMP-EQUAL M-A A-ZERO CHECK-PGF-L1-L2-MAP-CONTROL-2)
;        ((MD) SETZ)
;CHECK-PGF-L1-L2-MAP-CONTROL-1
;        (CALL-EQUAL L2-MAP-CONTROL A-ZERO ILLOP-DEBUG)
;        ((MD) ADD MD (A-CONSTANT (EVAL PAGE-SIZE)))
;        (JUMP-LESS-THAN MD A-A CHECK-PGF-L1-L2-MAP-CONTROL-1)
;CHECK-PGF-L1-L2-MAP-CONTROL-2
;
; end of tempoary code

        ((MD) A-PGF-VMA)                                ;RESTORE MD (ADDRESS OF REFERENCE)
        ;DROP THROUGH ADVANCE-SECOND-LEVEL-MAP-REUSE-POINTER, AND RETURN
;ROUTINE TO ADVANCE SECOND LEVEL MAP REUSE POINTER, WITH CARE.  CLOBBERS Q-R
ADVANCE-SECOND-LEVEL-MAP-REUSE-POINTER
        ((Q-R A-SECOND-LEVEL-MAP-REUSE-POINTER)
                ADD M-ZERO A-SECOND-LEVEL-MAP-REUSE-POINTER ALU-CARRY-IN-ONE)
  ;now use whole 2nd level map.
        (POPJ-AFTER-NEXT POPJ-LESS-THAN Q-R (A-CONSTANT 176))
       ((A-SECOND-LEVEL-MAP-REUSE-POINTER)
                A-SECOND-LEVEL-MAP-REUSE-POINTER-INIT)  ;WRAP AROUND TO AFTER THE WIRED ONES

PGF-MAP-MISS-RELOAD-ONLY
        (declare (clobbers a-lam a-tem a-tem3 a-tem1)
                 (local a-pgf-t a-pgf-a a-pgf-b))
        (CALL-XCT-NEXT PGF-SAVE)        ;SAVE A,B,T,VMA
       ((M-TEM) L1-MAP-L2-BLOCK-SELECT L1-MAP)  ;CHECK FOR 1ST-LEVEL MISS
        (CALL-EQUAL M-TEM (A-CONSTANT 177) LEVEL-1-MAP-MISS)
        ;; MD HAS ADDRESS, VMA SAVED AND CLOBBERED.  HANDLE 2ND-LEVEL MISS
        ((A-SECOND-LEVEL-MAP-RELOADS) ADD A-SECOND-LEVEL-MAP-RELOADS M-ZERO ALU-CARRY-IN-ONE)
        ((M-T) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-PGF-VMA) ;ADDRESS SANS EXTRA BITS
        (JUMP-LESS-THAN M-T A-LOWEST-DIRECT-VIRTUAL-ADDRESS PGF-L2A-RELOAD-ONLY)
        (JUMP PGF-MM-RELOAD-ONLY-1)

;MAP MISS COMES HERE.  ADDRESS IN VMA AND MD BOTH.
;SET UP FIRST-LEVEL MAP IF NECESSARY.  THEN DEAL WITH PAGE-FAULT.
PGF-MAP-MISS
;;; Experimental.
;  (call pgf-r-pdl-check-immediately)

        (CALL-XCT-NEXT PGF-SAVE)        ;SAVE A,B,T,VMA
       ((M-TEM) L1-MAP-L2-BLOCK-SELECT L1-MAP)  ;CHECK FOR 1ST-LEVEL MISS
        (CALL-EQUAL M-TEM (A-CONSTANT 177) LEVEL-1-MAP-MISS)
        ;; MD HAS ADDRESS, VMA SAVED AND CLOBBERED.  HANDLE 2ND-LEVEL MISS
        ((A-SECOND-LEVEL-MAP-RELOADS) ADD A-SECOND-LEVEL-MAP-RELOADS M-ZERO ALU-CARRY-IN-ONE)
        ((M-T) SELECTIVE-DEPOSIT M-ZERO Q-ALL-BUT-POINTER A-PGF-VMA) ;ADDRESS SANS EXTRA BITS
        (JUMP-LESS-THAN M-T A-LOWEST-DIRECT-VIRTUAL-ADDRESS PGF-L2A)
PGF-MM-RELOAD-ONLY-1
        (JUMP-LESS-THAN M-T (A-CONSTANT LOWEST-A-MEM-VIRTUAL-ADDRESS) MAP-GREY)
                ;"direct mapping" cant win on LAMBDA.  Someday, maybe we'll have some sort
                ; of main memory tables to allow various things to get mapped in.
                ;However, if A-LOWEST-DIRECT-VIRTUAL-ADDRESS has been moved down,
                ; we stick in the medium resolution color.
        (JUMP-LESS-THAN M-T (A-CONSTANT LOWEST-IO-SPACE-VIRTUAL-ADDRESS)
                PGF-SPECIAL-A-MEMORY-REFERENCE)
;REFERENCE TO UNIBUS OR X-BUS IO VIRTUAL ADDRESS (on CADR).  FAKE UP PAGE HASH TABLE ENTRY
PGF-MM0 (JUMP-LESS-THAN M-T (A-CONSTANT 177100000) MAP-VIDEO-BUFFER)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT LOWEST-UNIBUS-VIRTUAL-ADDRESS) MAP-MULTIBUS)
        (CALL-GREATER-OR-EQUAL M-T (A-CONSTANT 177377400) ILLOP)  ;CADR control registers.
                        ;this page left blank so they will trap.
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177377000) MAP-MULTIBUS-IO)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177371000) MAP-NU-MULTI-MAP-REGS)  ;See below.
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177370400) MAP-TV-CONTROL-REGS)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177370000) MAP-SDU-CONTROL-REGS)
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177360000) MAP-SHARED-PAGES-1)
        (CALL-GREATER-OR-EQUAL M-T (A-CONSTANT 177340000) ILLOP)                  ;Scratch block.
        (JUMP-GREATER-OR-EQUAL M-T (A-CONSTANT 177300000) MAP-SHARED-PAGES-2)
#+lambda(jump-greater-or-equal m-t (a-constant 177277400) map-quad-video-control-regs)
        (CALL ILLOP)

;       ((M-T) VMA-PHYS-PAGE-ADDR-PART M-T
;               (A-CONSTANT (BYTE-MASK MAP-WRITE-ENABLE-SECOND-LEVEL-WRITE)))
;       ((M-A) (A-CONSTANT 1460))       ;RW ACCESS, STATUS=4, NO AREA TRAPS, REP TYPE 0
;       (JUMP-XCT-NEXT PGF-RESTORE)             ;GO RETRY REFERENCE
;      ((VMA-WRITE-MAP) DPB M-A MAP-ACCESS-STATUS-AND-META-BITS A-T)
        ;; Returning to PGF-R or PGF-W, which will reload VMA with good data.

MAP-MULTIBUS  ;virtual address >=177,,400000.
          ;Extract bits 16-8 which are page number within multibus.  These bits go
          ; in 8-0 of L2 map.  Byte mask accounts for F in high 4 bits of NUBUS address.
#-LAMBDA (BEGIN-COMMENT)
        ((M-T) LDB (BYTE-FIELD 9 8) M-T a-zero)
        ((M-A) A-SDU-quad-SLOT)   ;SDU quad slot goes in bits 21-14.
        ((L2-MAP-PHYSICAL-PAGE) DPB M-A (BYTE-FIELD 8 14.) A-T)
        (JUMP-XCT-NEXT PGF-RESTORE)             ;GO RETRY REFERENCE
       ((L2-MAP-CONTROL) (A-CONSTANT 1460)) ;map2c-access-status-and-meta-bits is right adj.
                                        ;RW access, status 4, no area traps.
        ;; Returning to PGF-R or PGF-W, which will reload VMA with good data.
#-LAMBDA (END-COMMENT)
#+EXP   (CALL ILLOP)

(begin-comment) (end-comment)
MAP-VIDEO-BUFFER
#-lambda(begin-comment)
        ((l2-map-physical-page) LDB (BYTE-FIELD 7 8) M-T a-video-buffer-base-phys-page)
        ((m-a) a-processor-switches)
        ((m-a) ldb (lisp-byte %%processor-switch-cache-permit-for-video-buffer) m-a)
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((L2-MAP-CONTROL) dpb m-a (byte-field 1 14.) (A-CONSTANT 1460))
#-lambda(end-comment)
#-exp(begin-comment)
        ((m-t) ldb (byte-field 7 8) m-t (a-constant (eval (ash #xe80000 -10.))))
        ((m-a) a-tv-quad-slot)
        ((vma-write-l2-map-physical-page) dpb m-a (byte-field 8 14.) a-t)
        (jump-xct-next pgf-restore)
       ((vma-write-l2-map-control) (a-constant 1460))
#-exp(end-comment)

MAP-MULTIBUS-IO ;multibus io page.
#-LAMBDA (BEGIN-COMMENT)
        ((M-A) A-SDU-quad-SLOT)
        ((L2-MAP-PHYSICAL-PAGE) DPB M-A (BYTE-FIELD 8 14.)
         (A-CONSTANT (BYTE-MASK (BYTE-FIELD 1 10.))))
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((L2-MAP-CONTROL) (A-CONSTANT 5460))     ;packet size code 1
#-LAMBDA (END-COMMENT)
#+exp(call illop)

MAP-NU-MULTI-MAP-REGS
#-LAMBDA (BEGIN-COMMENT)
        ((M-T) SUB M-T (A-CONSTANT 177371000))    ;relative adr within range
        ((M-T) LDB M-T (BYTE-FIELD 8 8) A-ZERO)  ;relative page number within range.
        ((M-A) A-SDU-quad-SLOT)
        ;;map regs begin at 18000(hex), ie 140 pages
        ((M-A) DPB M-A (BYTE-FIELD 8 14.) (A-CONSTANT 140))
        ((M-A) DPB M-T (BYTE-FIELD 2 0) A-A)    ;four pages of mapping registers.
        ((M-T) LDB M-T (BYTE-FIELD 2 2) A-ZERO) ;high bits determine byte no to select.
        ((L2-MAP-PHYSICAL-PAGE) DPB M-T (BYTE-FIELD 2 22.) A-A)  ;store byte code.
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((L2-MAP-CONTROL) (A-CONSTANT 5460))     ;packet size code 1
#-LAMBDA (END-COMMENT)
#+EXP   (CALL ILLOP)

MAP-TV-CONTROL-REGS     ;map a single page at base of TV slot space.
#-lambda(begin-comment)
        ((M-A) A-TV-quad-SLOT)
        ((L2-MAP-PHYSICAL-PAGE) DPB M-A (BYTE-FIELD 8 14.) a-zero)
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((L2-MAP-CONTROL) (A-CONSTANT 1460))

map-quad-video-control-regs
        ((m-a) a-video-buffer-base-phys-page)
        ((l2-map-physical-page) add m-a (a-constant (eval (ash #x80000 -10.))))
        (jump-xct-next pgf-restore)
       ((l2-map-control) (a-constant 1460))
#-lambda(end-comment)
#+exp (call illop)

;one page, mapped to low byte of #xff01c000 in the SDU
; (ash #x1c000 -10.) => page 160 on the SDU
MAP-sdu-control-regs
#-LAMBDA (BEGIN-COMMENT)
        ((m-a) a-sdu-quad-slot)
        ((l2-map-physical-page) dpb m-a (byte-field 8 14.) (a-constant 160))
        (jump-xct-next pgf-restore)
       ((l2-map-control) (a-constant 5460)) ; packet size code 1 = byte mode
#-LAMBDA (END-COMMENT)
#+EXP   (CALL ILLOP)

;map the 32. page segment
map-shared-pages-1
#-lambda(begin-comment)
        ((m-t) sub m-t (a-constant 177360000)) ;relative adr within range
        ((m-t) ldb m-t (byte-field 8 8) a-zero) ;relative page number within range
        ((M-TEM) a-sys-conf-base-phys-page)
        ((M-TEM) add M-TEM (a-constant 64.))
        ((l2-map-physical-page) add M-TEM a-t)
        (jump-xct-next pgf-restore)
       ((l2-map-control) (a-constant 1460))
#-lambda(end-comment)
#+exp (call illop)

;this is the 64. page segment
map-shared-pages-2
#-lambda (begin-comment)
        ((m-t) sub m-t (a-constant 177300000)) ;relative adr within range
        ((m-t) ldb m-t (byte-field 8 8) a-zero) ;relative page number within range
        ((M-TEM) a-sys-conf-base-phys-page)
        ((l2-map-physical-page) add M-TEM a-t)
        (jump-xct-next pgf-restore)
        ((l2-map-control) (a-constant 1460))
#-lambda(end-comment)
#+exp (call illop)

map-grey
;; ACTUALLY GUYS, STEVE WARD AT MIT WANTS TO USE OUR COLOR BOARD ON THE EXPLORER.
;;#+exp (call illop)
;;#-lambda (begin-comment)
        (call-less-than m-t (a-constant 176300000) illop)   ;below anything reasonable
        ((m-a) a-grey-quad-slot)
        (jump-less-than m-t (a-constant 176500000) map-grey-pixel)
        (jump-less-than m-t (a-constant 176700000) map-grey-plane)
    ;fall thru in case of color-map, registers, and prom.
        ((m-t) sub m-t (a-constant 176700000))
        ((m-t) ldb m-t (byte-field 5 8) (a-constant 37740)) ;base page number within slot space
map-grey-x
        ((#+LAMBDA l2-map-physical-page #+EXPLORER vma-write-l2-map-physical-page) dpb m-a (byte-field 8 14.) a-t)
        (jump-xct-next pgf-restore)
       ((#+LAMBDA l2-map-control #+EXPLORER vma-write-l2-map-control) (a-constant 1460))


map-grey-pixel
        ((m-t) sub m-t (a-constant 176300000))
        (jump-xct-next map-grey-x)
       ((m-t) ldb m-t (byte-field 8 8) a-zero)          ;to page 0 in slot space.

map-grey-plane
        ((m-t) sub m-t (a-constant 176500000))
        ((m-t) ldb m-t (byte-field 8 8) a-zero)
        (jump-xct-next map-grey-x)
       ((m-t) add m-t (a-constant 2000))        ;page number within slot space
;;#-lambda (end-comment)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)
d-pgf-quantum-map-dispatch
        (p-bit r-bit)                           ;000 quantum not valid
        (p-bit r-bit)                           ;001 quantum not valid
        (p-bit r-bit)                           ;010 quantum not valid
        (p-bit r-bit)                           ;011 quantum not valid
        (p-bit r-bit)                           ;100 normal thing, fall through.
        (p-bit r-bit)                           ;101 normal thing, fall through.
        (n-bit pgf-map-device)                  ;110 map device
;       (n-bit pgf-special-a-memory-reference)  ;111 special a memory
        (p-bit n-bit illop)
(END-DISPATCH)
(LOCALITY I-MEM)

;REFERENCE TO ORDINARY VIRTUAL ADDRESS.  LOOK IN PAGE HASH TABLE
;;; first see if it is a quantum mapped device though.
PGF-L2A
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear m-tem (lisp-byte %%processor-switch-fast-boot-enable) pgf-l2a-continue)
        ;;;look up the virtual address in m-t in the quantum map
        ((vma) ldb vma-quantum-byte m-t)
        ((vma) dpb vma (byte-field 31. 1) a-zero)   ;double it, two words
                                                    ;per entry
        ((vma) add vma (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((vma-start-read) add vma a-v-quantum-map)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ;;; pq1 is now in md
        (dispatch (lisp-byte %%pq1d-page-fault-dispatch-field) md d-pgf-quantum-map-dispatch)
pgf-l2a-continue
        (CALL SEARCH-PAGE-HASH-TABLE)
        (DISPATCH PHT1-SWAP-STATUS-CODE READ-MEMORY-DATA D-PGF-PHT) ;FOUND, CHK SW STS

PGF-L2A-RELOAD-ONLY
        (declare (clobbers a-t a-tem a-tem3 a-tem1 a-lam))
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear m-tem (lisp-byte %%processor-switch-fast-boot-enable) pgf-l2a-continue)
        ;;;look up the virtual address in m-t in the quantum map
        ((vma) ldb vma-quantum-byte m-t)
        ((vma) dpb vma (byte-field 31. 1) a-zero)   ;double it, two words
                                                    ;per entry
        ((vma) add vma (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        ((vma-start-read) add vma a-v-quantum-map)
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ;;; pq1 is now in md
        (dispatch (lisp-byte %%pq1d-page-fault-dispatch-field) md d-pgf-quantum-map-dispatch)
pgf-l2a-reload-only-continue
        (CALL SEARCH-PAGE-HASH-TABLE)
        (DISPATCH PHT1-SWAP-STATUS-CODE READ-MEMORY-DATA D-PGF-PHT-RELOAD-ONLY) ;FOUND, CHK SW STS

;;; entered via PGF-QUANTUM-MAP-DISPATCH from PGF-MAP-MISS and PGF-MAP-MISS-RELOAD-ONLY.
;;; vma is pointing to QUANTUM-MAP entry
pgf-map-device
        ;;; check size
        ;;; find an l2 map entry, no, this has been done for us already.
        ;;; set it up and return
        ((vma-start-read) m+a+1 vma a-zero)     ;get pq2 word
        (illop-if-page-fault)
     (error-table crash page fault in wired area)
        ((m-tem) md)
        ((MD) A-PGF-VMA)                                ;ADDRESS THE MAP
        (NO-OP)         ;allow time.
        ((M-T) L1-MAP-L2-BLOCK-SELECT L1-MAP)
        (CALL-EQUAL M-T (A-CONSTANT 177) ILLOP)         ;ABOUT TO CLOBBER
        ((md) a-pgf-vma)                        ;address the map
        ((m-tem1) ldb (lisp-byte %%pq2d-quantum-l2mc-except-meta-bits) m-tem)
        ((#+lambda L2-MAP-PHYSICAL-PAGE
          #+exp vma-write-l2-map-physical-page) ldb (lisp-byte %%pq2d-quantum-l2mpp) m-tem)
#+lambda((l2-map-control) dpb m-tem1 (lisp-byte %%l2-map-control-all-but-meta-bits)
                (a-constant #o60))
#+exp   ((m-tem1) dpb m-tem1 (lisp-byte %%l2-map-control-all-but-meta-bits)
                (a-constant #o60))
#+exp   ((vma-write-l2-map-control) dpb m-tem1 exp-map2c-volatility a-tem1)
                ;meta bits stored inverted, 060 means not extra-pdl, not old-space,
                ;most-volatile, representation-type unstructured, most volatile pointer
                ;(though should never do gc-write-test).
        (popj)

(LOCALITY D-MEM)
(START-DISPATCH 3 INHIBIT-XCT-NEXT-BIT) ;DISPATCH ON SWAP STATUS
D-PGF-PHT
        (SWAPIN)                ;0 PHT ENTRY INVALID, GET PAGE FROM DISK
        (PGF-RL)                ;1 NORMAL, RELOAD PAGE MAP
        (PGF-FL)                ;2 FLUSHABLE, CHANGE BACK TO NORMAL
        (PGF-PRE)               ;3 PREPAGED, CHANGE TO NORMAL, WE WANT IT NOW
        (PGF-AG)                ;4 AGE, CHANGE BACK TO NORMAL
        (PGF-RL)                ;5 WIRED DOWN, RELOAD PAGE MAP
        (P-BIT ILLOP)           ;6 NOT USED
        (P-BIT ILLOP)           ;7 NOT USED
(END-DISPATCH)

(LOCALITY D-MEM)
(START-DISPATCH 3 INHIBIT-XCT-NEXT-BIT) ;DISPATCH ON SWAP STATUS
D-PGF-PHT-RELOAD-ONLY
        (P-BIT ILLOP)           ;0 PHT ENTRY INVALID, GET PAGE FROM DISK
        (PGF-RL)                ;1 NORMAL, RELOAD PAGE MAP
        (PGF-FL)                ;2 FLUSHABLE, CHANGE BACK TO NORMAL
        (PGF-PRE)               ;3 PREPAGED, CHANGE TO NORMAL, WE WANT IT NOW
        (PGF-AG)                ;4 AGE, CHANGE BACK TO NORMAL
        (PGF-RL)                ;5 WIRED DOWN, RELOAD PAGE MAP
        (P-BIT ILLOP)           ;6 NOT USED
        (P-BIT ILLOP)           ;7 NOT USED
(END-DISPATCH)

(START-DISPATCH 3) ;jumps to pgf-mar-{read,write}-trap, or drops through
D-MAR   (P-BIT R-BIT)                                  ;0 READ, MAR DISABLED
        (n-bit pgf-mar-read-TRAP)                      ;1 READ, READ-TRAP
        (P-BIT R-BIT)                                  ;2 READ, WRITE-TRAP
        (n-bit pgf-mar-read-TRAP)                      ;3 READ, READ-WRITE-TRAP
        (P-BIT R-BIT)                                  ;4 WRITE, MAR DISABLED
        (P-BIT R-BIT)                                  ;5 WRITE, READ-TRAP
        (n-bit pgf-mar-write-TRAP)                     ;6 WRITE, WRITE-TRAP
        (n-bit pgf-mar-write-TRAP)                     ;7 WRITE, READ-WRITE-TRAP
(END-DISPATCH)
(LOCALITY I-MEM)

;Here on reference to page containing the MAR'ed location.
;The VMA is still valid, the MD has been clobbered to the VMA but
;if writing is saved in A-PGF-WMD, and M-PGF-WRITE says type of cycle.
;If this traps, VMA and M-PGF-WRITE will still be valid as saved by SGLV.
;In the case of a write, the data to be written will be on the stack.
;A read can be recovered just by returning from PGF-R,
;since the MAR is inhibited during stack-group switching.
;A write is continued by simulation in the error handler, followed
;by same continuation as a read.
PGF-MAR ((M-PGF-TEM) M-FLAGS-NO-SEQUENCE-BREAK) ;If can't take trap now
        (JUMP-NOT-EQUAL M-PGF-TEM A-ZERO PGF-MAR1) ;then don't take one
        ((M-PGF-TEM) Q-POINTER VMA (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (JUMP-LESS-THAN M-PGF-TEM A-MAR-LOW PGF-MAR1)   ;Check address bounds
        (JUMP-GREATER-THAN M-PGF-TEM A-MAR-HIGH PGF-MAR1)
        (DISPATCH M-FLAGS-MAR-DISP D-MAR)       ;Take MAR break if necessary
PGF-MAR1
        ;False alarm, simulate the memory cycle,
        ;but it might be in the PDL buffer, so simulate that trap.
        ;Anyway that code is pretty experienced at simulating memory cycles.
        (JUMP-IF-BIT-CLEAR M-PGF-WRITE PGF-R-PDL)
        (JUMP PGF-W-PDL)

;*** following two should probably be pushing DTP-LOCATIVEs most of the time
pgf-mar-read-trap
        ((pdl-push) dpb vma q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) ldb q-data-type vma (a-constant (byte-value q-data-type dtp-fix)))
        (call trap)
    (ERROR-TABLE MAR-BREAK READ)
        ;to continue, the debugger should pop two, then exit without calling
        ;sg-proceed-micro-pc which will make PGF-R return.

pgf-mar-write-trap
        ((pdl-push) dpb vma q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) ldb q-data-type vma (a-constant (byte-value q-data-type dtp-fix)))
        ((m-tem) a-pgf-wmd)
        ((PDL-PUSH) DPB m-tem Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-PUSH) LDB Q-DATA-TYPE m-tem (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (call trap)
    (ERROR-TABLE MAR-BREAK WRITE)
        ;to continue, optionally do the write in macrocode, pop 4, then exit without calling
        ;sg-proceed-micro-pc, which will make PGF-W return


;HERE ON REFERENCE TO LOCATION MAPPED INTO A/M SCRATCHPAD, ADDRESS IN M-T
PGF-SPECIAL-A-MEMORY-REFERENCE
        (JUMP-IF-BIT-SET-XCT-NEXT M-PGF-WRITE PGF-SA-W) ;JUMP IF CYCLE IS A WRITE
       ((M-GARBAGE) MICRO-STACK-DATA-POP)       ;FLUSH RETRY-CYCLE RETURN
        ((OA-REG-HIGH) DPB M-T OAH-A-SRC-10-BITS A-ZERO)
                                                ;NOTE LOWEST-A-MEM-VIRTUAL-ADDRESS
        ((MD) A-GARBAGE)                        ;MUST BE 0 MODULO A-MEMORY SIZE
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((VMA) A-PGF-VMA)                ;NOBODY ELSE WILL PUT BACK VMA

PGF-SA-W
        ((m-a) a-pgf-mode)
        (JUMP-if-bit-clear M-A (byte-field 1 0) PGF-SA-W-NOT-BINDING)
;       ((M-A) A-V-NIL)
;       (JUMP-EQUAL M-A A-AMEM-EVCP-VECTOR PGF-SA-W-NOT-BINDING)
;       ;; Get a-mem address being bound.  In range for EVCP hacking?
;       ((VMA) DPB M-ZERO (BYTE-FIELD 22. 10.) A-PGF-VMA)       ;Get low 10 bits
;       (JUMP-GREATER-OR-EQUAL VMA (A-CONSTANT (A-MEM-LOC A-END-Q-POINTERS))
;                       PGF-SA-W-NOT-BINDING)
;       ;; We are binding or unbinding and must hack the EVCP vector.
;       ;; "restore" all info saved by PGF-W to its real home
;       ;; or else save it on the stack
;       ;; so we can be in a position to take recursive page faults.
;       ;; Note: A-PGF-WMD can be untyped data, but since we
;       ;; do not allow sequence breaks herein, that can't cause trouble.
;       ;; Also, since this happens only from binding or unbinding,
;       ;; we need not fear that PDL-BUFFER-POINTER doesn't really
;       ;; point at the top of the stack.
;       (CALL PGF-RESTORE)
;       ((C-PDL-BUFFER-POINTER-PUSH) A-PGF-WMD)
;       ((C-PDL-BUFFER-POINTER-PUSH) A-PGF-VMA)
;       ;Now we can take page faults again!
;       ;Get the current EVCP out of the EVCP vector.
;       ((VMA-START-READ) M+A+1 A-AMEM-EVCP-VECTOR VMA)
;       (CHECK-PAGE-READ)
;       (DISPATCH TRANSPORT-NO-EVCP READ-MEMORY-DATA)
;       ((MD) Q-TYPED-POINTER MD)
;       (JUMP-EQUAL MD A-V-NIL PGF-SA-BIND-NO-EVCP)
;       ;Write current contents of a-mem location into the EVCP, if any.
;       ((VMA) MD)
;       ((M-TEM) (BYTE-FIELD 10. 0) C-PDL-BUFFER-POINTER)
;       ((OA-REG-HIGH) DPB M-TEM OAH-A-SRC A-ZERO) ;NOTE LOWEST-A-MEM-VIRTUAL-ADDRESS
;       ((MD-START-WRITE) A-GARBAGE)    ;MUST BE 0 MODULO A-MEMORY SIZE
;       (CHECK-PAGE-WRITE)
;       (GC-WRITE-TEST)
;PGF-SA-BIND-NO-EVCP
;       ;Replace the current EVCP with the old one, or NIL if not an EVCP.
;       ((VMA) C-PDL-BUFFER-POINTER-POP)
;       ((MD) C-PDL-BUFFER-POINTER)
;       ((C-PDL-BUFFER-POINTER-PUSH) VMA)
;       ((M-TEM) Q-DATA-TYPE MD)
;       (JUMP-EQUAL M-TEM (A-CONSTANT (EVAL DTP-EXTERNAL-VALUE-CELL-POINTER))
;                   PGF-SA-BIND-NEW-EVCP)
;       ((MD) A-V-NIL)
;PGF-SA-BIND-NEW-EVCP
;       ((VMA) (BYTE-FIELD 10. 0) VMA)
;       ((VMA-START-WRITE) M+A+1 A-AMEM-EVCP-VECTOR VMA)
;       (CHECK-PAGE-WRITE)
;       (JUMP-EQUAL MD A-V-NIL PGF-SA-BIND-NO-NEW-EVCP)

;; If thing to be stored is an EVCP, store the contents of where it points.
        ((M-TEM) A-PGF-WMD)
;       ((M-TEM) Q-DATA-TYPE M-TEM)
;       (JUMP-NOT-EQUAL M-TEM (A-CONSTANT (EVAL DTP-EXTERNAL-VALUE-CELL-POINTER))
;                       PGF-SA-W-NOT-BINDING)
  ;an attempt to store an EVCP in A-MEM must be part of an attempt to closure bind A-MEM,
  ; which cant work anyway.  Furthermore, the call to the transporter below could lose
  ; due to numerous reasons.
   (call-data-type-equal m-tem
                         (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTERNAL-VALUE-CELL-POINTER))
                         illop)
;       (JUMP-DATA-TYPE-NOT-EQUAL M-TEM
;               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-EXTERNAL-VALUE-CELL-POINTER))
;               PGF-SA-W-NOT-BINDING)
;       (CALL PGF-RESTORE)
;       ((C-PDL-BUFFER-POINTER-PUSH) A-PGF-VMA)
;       ;Now safe to take page faults recursively.
;       ;Get contents of the new EVCP, and put that in a mem instead of the EVCP.
;       ((VMA-START-READ) A-PGF-WMD)
;       (CHECK-PAGE-READ)
;       (DISPATCH TRANSPORT-NO-EVCP)
;       (CALL-XCT-NEXT PGF-SAVE)
;       ((A-PGF-WMD) MD)
;;      (CALL-XCT-NEXT PGF-SA-W-NOT-BINDING)
;       ((A-PGF-VMA) C-PDL-BUFFER-POINTER-POP)
;       ;Now we are inside a page fault again!
;       ;Finish writing new contents into A memory.
;;      ((MD) C-PDL-BUFFER-POINTER-POP)
;;      (POPJ)
;;
;;PGF-SA-BIND-NO-NEW-EVCP
;;      ((A-PGF-VMA) C-PDL-BUFFER-POINTER-POP)
;;      ((A-PGF-WMD) C-PDL-BUFFER-POINTER-POP)
;;      (CALL PGF-SAVE-1)
PGF-SA-W-NOT-BINDING
        ((M-T) DPB M-ZERO (BYTE-FIELD 22. 10.) A-PGF-VMA)
        (JUMP-LESS-THAN M-T (A-CONSTANT 100) PGF-SM-W)  ;LOCN REALLY IN M-MEM.
        ((OA-REG-LOW) DPB M-T OAL-A-DEST-10-BITS A-ZERO)
        ((A-GARBAGE) A-PGF-WMD)
        ((MD) A-PGF-WMD)
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((VMA) A-PGF-VMA)                ;NOBODY ELSE WILL PUT BACK VMA

PGF-SM-W((OA-REG-LOW) DPB M-T OAL-M-DEST A-ZERO)
        ((M-GARBAGE MD) A-PGF-WMD)
        (JUMP-XCT-NEXT PGF-RESTORE)
       ((VMA) A-PGF-VMA)                ;NOBODY ELSE WILL PUT BACK VMA

;Write in read-only.
PGF-RDONLY      ;dispatch to here is a JUMP, not a PUSHJ.
        ;; Should not get here on a read.
        (CALL-IF-BIT-CLEAR M-PGF-WRITE ILLOP)
        ;; If this is a CHECK-PAGE-WRITE-FORCE, do it anyway.
        ((m-tem) a-pgf-mode)
        (jump-if-bit-set m-tem (byte-field 1 1) FORCE-WR-RDONLY)
        ((M-TEM) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-INHIBIT-READ-ONLY)
        (CALL-EQUAL M-TEM A-V-NIL TRAP)
                (ERROR-TABLE WRITE-IN-READ-ONLY VMA)    ;Not continuable!
                ;drop into FORCE-WR-RDONLY
;Forced write in nominally read-only area.
;Second-level map is set-up and grants read-only access.
FORCE-WR-RDONLY
        (CALL PGF-SAVE)
MOBY-FORCE-WR           ;enter here from MOBY-PGF-RWF.
        ;Find PHT entry to mark page as modified
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)
       ((VMA M-T) A-PGF-VMA)
        (CALL-IF-BIT-CLEAR PHT1-VALID-BIT READ-MEMORY-DATA ILLOP) ;not found?
        ((WRITE-MEMORY-DATA-START-WRITE)
                IOR READ-MEMORY-DATA (A-CONSTANT (BYTE-MASK PHT1-MODIFIED-BIT)))
        (ILLOP-IF-PAGE-FAULT)

        ((md) md)               ;wait for cycle to finish
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control
          m-tem) ior l2-map-control     ;Force read/write access
                (A-CONSTANT (BYTE-VALUE MAP2C-ACCESS-CODE 3)))

     ;; Note: this has to be careful to do a write cycle on the original VMA/MD,
     ;; then reset the maps to read-only and return without starting another
     ;; write cycle.
        ((VMA) A-PGF-VMA)               ;Restore original VMA
        ((MD-START-WRITE) A-PGF-WMD)    ;Do the write
        (ILLOP-IF-PAGE-FAULT)
        ((MD) VMA)                      ;Address map again
        ((vma) (a-constant 2))  ;map access code rd-only
                                ;Set read-only access again
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control) dpb vma map2c-access-code a-tem)
        (CALL PGF-RESTORE)
        (POPJ-AFTER-NEXT                ;Memory cycle completed, return
         (VMA) A-PGF-VMA)
       ((MD) A-PGF-WMD)

;HERE FOR READ-WRITE-FIRST TRAP
;FIND PAGE HASH TABLE ENTRY, CHANGE STATUS TO READ/WRITE, AND RELOAD MAP
PGF-RWF (CALL-IF-BIT-CLEAR M-PGF-WRITE ILLOP)
        (CALL PGF-SAVE)

  ;if this a moby region, handle things differently.
        (call-xct-next xrgn1)
       ((m-a) a-pgf-vma)
        ((vma-start-read) add m-t a-v-region-bits)
        (illop-if-page-fault)
        ((m-tem) (lisp-byte %%region-space-type) md)
        (jump-greater-or-equal m-tem (a-constant (eval %region-space-moby-fixed))
                               moby-pgf-rwf)

        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)
       ((M-T) A-PGF-VMA)
        (CALL-IF-BIT-CLEAR PHT1-VALID-BIT READ-MEMORY-DATA ILLOP)       ;NOT IN PHT??
        ((WRITE-MEMORY-DATA-START-WRITE)                ;MARK PAGE MODIFIED
                IOR READ-MEMORY-DATA (A-CONSTANT (BYTE-MASK PHT1-MODIFIED-BIT)))
        (ILLOP-IF-PAGE-FAULT)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;GET SECOND WORD
        (ILLOP-IF-PAGE-FAULT)                           ;TABLE SUPPOSED TO BE WIRED
    (declare (restores (a-a a-pgf-a)))
        ((M-A) A-PGF-A)                                 ;RESTORE A REG DURING MEM CYCLE
        ((M-LAM) (A-CONSTANT 4))                                ;NORMAL STATUS
        ((M-B) READ-MEMORY-DATA)
        ((WRITE-MEMORY-DATA-START-WRITE M-LAM) DPB M-LAM PHT2-MAP-STATUS-CODE A-B)
        (ILLOP-IF-PAGE-FAULT)
    (declare (restores (a-b a-pgf-b)))
        ((M-B) A-PGF-B)
        ((MD) A-PGF-VMA)                ;ADDRESS THE MAP (no xct-next to allow map to set up)
        (CALL LOAD-L2-MAP-FROM-CADR-PHYSICAL) ;PHT2 SAME AS TO 2ND LVL MAP (ON CADR)
        (POPJ-AFTER-NEXT NO-OP)
    (declare (restores (a-t a-pgf-t)))
       ((M-T) A-PGF-T)          ;GO RETRY MEMORY CYCLE  (used to reload VMA randomly, as well)

moby-pgf-rwf            ;region being written in M-T.
        (call moby-pgf-save)
        ;ref virtual page data for this page.  This can take a recursive page fault.
        ((m-b) a-pgf-vma)
        ((m-b) q-page-number m-b)
        ((vma-start-read) add m-b a-v-virtual-page-data)
        (check-page-read-no-interrupt)
        ((md-start-write) andca md (a-constant (byte-value %%virtual-page-clean 1)))
        (check-page-write-no-interrupt)
  ;NOTE!!! Every possible page fault will garbage a-pgf-mode, a-pgf-vma, a-pgf-wmd.
  ; if you need those back, you have to call MOBY-PGF-RETRIEVE, which gets them off
  ; the top of the stack.

  ;An check that its OK to write this data in moby-space.  This is purely for debugging purposes.
  ; OK if:
  ;(1)  an unboxed write.
  ;(2)  not a pointer. (including any type of number).
  ;(3)  dtp-symbol.
  ;(4)  dtp-instance.  (assume it is either in moby space or handles :dereconcile).
  ;(5)  dtp-array-pointer to a named structure. (also assume :dereconcile).
  ;(6)  dtp-instance-header.
  ;(7)  dtp-unreconciled.
  ;(7a) dtp-null.  (value cells in instances initialized.  Assume OK.)
  ;(8)  points to a moby region.
  ;(9)  location is a local cell (as specified by moby-bits).

        (call moby-pgf-retrieve)
        ((m-tem) a-pgf-mode)
        (jump-if-bit-set (byte-field 1 2) m-tem moby-pgf-exit)          ;(1)
        ((md) a-pgf-wmd)
        (dispatch q-data-type md d-skip-on-pointerp)                    ;(2), partly
        (jump moby-pgf-exit)
        (jump-data-type-equal md moby-pgf-exit
            (a-constant (byte-value q-data-type dtp-extended-number)))  ;rest of 2
        (jump-data-type-equal md moby-pgf-exit
            (a-constant (byte-value q-data-type dtp-symbol)))           ;(3)
        (jump-data-type-equal md moby-pgf-exit
            (a-constant (byte-value q-data-type dtp-instance)))         ;(4)
        (jump-data-type-equal md moby-pgf-check-array-write
            (a-constant (byte-value q-data-type dtp-array-pointer)))    ;(5)
        (jump-data-type-equal md moby-pgf-exit
            (a-constant (byte-value q-data-type dtp-instance-header)))  ;(6)
        (jump-data-type-equal md moby-pgf-exit
            (a-constant (byte-value q-data-type dtp-unreconciled)))     ;(7)
        (jump-data-type-equal md moby-pgf-exit
            (a-constant (byte-value q-data-type dtp-null)))             ;(7a)
moby-pgf-check-pointer-write
        (call-xct-next moby-get-moby-bits)
       ((m-a) dpb m-zero q-all-but-pointer a-pgf-vma)
        ((m-tem1) (byte-field 2 2) m-t)         ;%%%moby-boxed
        (jump-equal m-tem1 (a-constant 2) moby-pgf-exit)                ;local-cell, ok (9)

        (call-xct-next xrgn1)
       ((m-a) a-pgf-wmd)
        ((vma-start-read) add m-t a-v-region-bits)
        (illop-if-page-fault)
        ((m-tem) (lisp-byte %%region-space-type) md)
        (jump-less-than m-tem (a-constant (eval %region-space-moby-fixed))
                        moby-pgf-trap)          ;points to not-moby region, LOSE!!!

moby-pgf-exit
        (call moby-pgf-restore)

        ((md) a-pgf-vma)
        (no-op)
  ;target page might have gotten swapped out or map overwritten.  If so, try again.
        (jump-if-bit-clear map2c-hardware-read-access l2-map-control moby-pgf-rwf0)

        ((m-garbage) micro-stack-data-pop)  ;flush return since RWF does PUSHJ in D-PGF
        (jump moby-force-wr)    ;force write to happen and return.

moby-pgf-rwf0
        (call pgf-restore)
        (popj)

moby-pgf-check-array-write
        ((vma-start-read) a-pgf-wmd)
        (check-page-read-no-interrupt)
        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-header))
                moby-pgf-exit)  ;some kind of forwarding.  Too complicated, and might win.
        (jump-if-bit-set (lisp-byte %%array-named-structure-flag)
                md moby-pgf-exit)  ;Named str, might win.
        (call moby-pgf-retrieve)
        (jump moby-pgf-check-pointer-write)

moby-pgf-trap
        ((m-a) a-pgf-vma)
        ((m-b) a-pgf-wmd)
        (call trap)
    (error-table illegal-moby-write m-a m-b)

moby-get-moby-bits
  ;local address in M-A, its region in M-T.
  ;return bits with fixnum data type in M-T.
        ((vma-start-read) add m-t a-v-region-origin)
        (illop-if-page-fault)
        ((m-tem1) md)
        ((m-a) sub m-a a-tem1)          ;region relative address.
        ((vma-start-read) add m-t a-v-region-moby-bits-array)
        (check-page-read-no-interrupt)
  ;region moby-bits-array must be ART-8B, with long length, in a static area.
        ((m-tem1) (byte-field (difference q-pointer-width 2) 2) M-a)
        ((m-tem1) add m-tem1 (a-constant 2))  ;one to space past header, one past long length q.
        ((vma-start-read) add a-tem1 md)
        (check-page-read-no-interrupt)
        (call moby-pgf-retrieve)        ;get back A-PGF-WMD, etc. Dont clobber MD.
        (dispatch-xct-next (byte-field 2 0) m-a d-qbary)
     ;bits return as fixnum in M-T
       (no-op)  ;QBARY guys POPJ.

;REFERENCE TO PAGE THAT WAS PREPAGED AND HASN'T BEEN TOUCHED YET.  GRAB IT.
PGF-PRE (declare (clobbers a-tem a-lam))
        ((A-DISK-PREPAGE-USED-COUNT) M+A+1 M-ZERO A-DISK-PREPAGE-USED-COUNT)
;REFERENCE TO PAGE MARKED FLUSHABLE.  WE WANT THIS PAGE AFTER ALL, CHANGE BACK TO NORMAL
PGF-FL  ;drop through
;REFERENCE TO PAGE WITH AGE TRAP.  CHANGE BACK TO NORMAL TO INDICATE PAGE
;HAS BEEN REFERENCED, AND SHOULDN'T BE SWAPPED OUT OR MADE FLUSHABLE.
PGF-AG  (declare (clobbers a-tem a-lam))
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT READ-MEMORY-DATA
                PHT1-ALL-BUT-SWAP-STATUS-CODE (A-CONSTANT 1))   ;SW STS := NORMAL
        (ILLOP-IF-PAGE-FAULT)                           ;THEN DROP THROUGH

;RELOAD HARDWARE MAP FROM PAGE HASH TABLE
PGF-RL  (declare (clobbers a-tem a-lam))
        ((MD) A-PGF-VMA)                                ;ADDRESS THE MAP
        (NO-OP)         ;allow time.
        ((M-T) L1-MAP-L2-BLOCK-SELECT L1-MAP)
        (CALL-EQUAL M-T (A-CONSTANT 177) ILLOP)         ;ABOUT TO CLOBBER
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;GET SECOND WORD OF PHT ENTRY
        (ILLOP-IF-PAGE-FAULT)                           ;TABLE SUPPOSED TO BE WIRED
     (declare (restores (a-a a-pgf-a) (a-b a-pgf-b)))
        ((M-A) A-PGF-A)                                 ;RESTORE REGS DURING MEM CYCLE
        ((M-B) A-PGF-B)
        (DISPATCH PHT2-MAP-STATUS-CODE READ-MEMORY-DATA D-SWAPAR)       ;VERIFY THE BITS
                ;; This will go to ILLOP if this is a page of a free region
        ((M-LAM) READ-MEMORY-DATA)      ;COMES DIRECTLY FROM PHT2 (on cadr)
        ((MD) A-PGF-VMA)                ;address map (no xct-next to allow map time)
        (CALL LOAD-L2-MAP-FROM-CADR-PHYSICAL)
        (POPJ-AFTER-NEXT NO-OP)
     (declare (restores (a-t a-pgf-t)))
       ((VMA M-T) A-PGF-T)

;ROUTINE TO LOOK FOR PAGE ADDRESSED BY M-T IN THE PAGE HASH TABLE
;RETURNS WITH VMA AND READ-MEMORY-DATA POINTING TO PHT1 WORD,
;OR VMA POINTING TO FIRST HOLE IN HASH TABLE AND PHT1-VALID-BIT
;OF READ-MEMORY-DATA ZERO.  IN THIS CASE, THE SWAP STATUS FIELD
;OF READ-MEMORY-DATA WILL ALSO BE ZERO.  CLOBBERS M-A, M-B, M-T, A-TEM1, A-TEM3

;;; m-tem3  virtual-page-number (m-t)

;;; This version is 7 cycles per iteration, as compared with 10 cycles for the
;;; old version below.
search-page-hash-table
        (declare (args a-t) (values a-tem3 a-t) (clobbers a-tem a-tem1))
        (call-xct-next compute-page-hash)              ;M-T := hash (M-T)
       ((m-tem3) pht1-virtual-page-number m-t)         ;Save for comparison below.
spht1   ((vma-start-read) add a-v-page-table-area m-t) ;Get PHT1 entry.
        (illop-if-page-fault)                          ;Supposed to be wired.
        ((m-t) add m-t (a-constant 2))                 ;Bump index for next iteration.
        (call-greater-or-equal m-t a-pht-index-limit pht-wraparound)
        ((m-tem) pht1-virtual-page-number md)          ;Compare page numbers.
        (jump-not-equal-xct-next m-tem a-tem3 spht1)   ;We usually don't win first time.
       (popj-if-bit-clear pht1-valid-bit md)           ;Page not in PHT.
        (popj)

XCPH (MISC-INST-ENTRY %COMPUTE-PAGE-HASH)
        (CALL-XCT-NEXT COMPUTE-PAGE-HASH)
       ((M-T) Q-POINTER C-PDL-BUFFER-POINTER-POP)
        (POPJ-AFTER-NEXT
          (M-T) Q-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
     ;; Execute one more instruction, clobbering A-TEM1.

;;; I suspected that the old hash function (below) was not performing
;;; so well because physical memory sizes have increased by a factor of 4-8 times
;;; since it was written.  I made some measurements of typical collision rates,
;;; and discovered that the simple "XOR the low bits with the high bits" outperforms
;;; more intricate functions by a significant amount.  This function produces
;;; about 25% fewer collisions than the old one.  KHS 851222.

;;; Foo.  Somewhere in the system someone depends on some property of the old
;;; hash algorithm.  I'm going to wimp out and revert it.

;;; Try using KHS algorithm. -Jrm 07/03/86

;;; This "unknown" property is that the hashing is independent
;;; of the bottom 8 bits.

;compute-page-hash
;     ;; 15 bits assures reasonable hashing for up to 4 megawords of memory.
;        ((m-tem1) (byte 15. 10.) m-t)          ;High 15 bits of page number.
;        ((m-t) xor m-t a-tem1)
;       ((m-t) and m-t a-pht-index-mask)

;;; Using the bit-mirror gives us the best hash if the GC improves
;;; locality.  This makes a noticable improvement.  ~jrm
compute-page-hash
  ((m-tem1) output-selector-bit-mirror m-t)
  (popj-after-next (m-t) ldb (byte 16. 8.) m-tem1)
 ((m-t) and m-t a-pht-index-mask)

pht-wraparound
        (popj-after-next popj-less-than m-t a-pht-index-limit)
       ((m-t) sub m-t a-pht-index-limit)        ;Wrap around

;COMPUTE-PAGE-HASH                              ;New algorithm, 3-DEC-80
;        (declare (args a-t) (values a-t) (clobbers a-tem1))
;       ((M-TEM1) (BYTE-FIELD 10. 14.) M-T)     ;VMA<23:14>
;       ((M-T) (BYTE-FIELD (DIFFERENCE Q-POINTER-WIDTH 4) 4) M-T)       ;VMA<23:8>x16+C
;       ((M-T) ANDCA M-T (A-CONSTANT 17))       ;-C
;       ((M-T) XOR M-T A-TEM1)
;       ((M-T) AND M-T A-PHT-INDEX-MASK)
;pht-wraparound
;       (declare (args a-t) (values a-t))
;       (POPJ-AFTER-NEXT POPJ-LESS-THAN M-T A-PHT-INDEX-LIMIT)
;       ((M-T) SUB M-T A-PHT-INDEX-LIMIT)       ;Wrap around

;SEARCH-PAGE-HASH-TABLE
;       ((M-TEM3) M-T)                          ;SAVE FOR COMPARISON BELOW
;       (CALL COMPUTE-PAGE-HASH)                ;M-T := HASH (M-T)
;SPHT1  ((VMA-START-READ) ADD A-V-PAGE-TABLE-AREA M-T)  ;GET PHT ENTRY
;       (ILLOP-IF-PAGE-FAULT)                   ;SUPPOSED TO BE WIRED
;       ((M-T) ADD M-T (A-CONSTANT 2))          ;BUMP INDEX FOR NEXT ITERATION
;       (JUMP-LESS-THAN M-T A-PHT-INDEX-LIMIT SPHT2)
;       ((M-T) SUB M-T A-PHT-INDEX-LIMIT)       ;WRAP AROUND
;SPHT2  (POPJ-IF-BIT-CLEAR PHT1-VALID-BIT READ-MEMORY-DATA) ;PAGE NOT IN PHT
;       ((M-tem) XOR A-TEM3 READ-MEMORY-DATA)   ;XOR VIRTUAL ADDRESSES
;       (POPJ-AFTER-NEXT                        ;(HOPING WE'LL WIN AND RETURN)
;        (M-B) PHT1-VIRTUAL-PAGE-NUMBER M-tem)  ;ZERO IF MATCH
;       (CALL-NOT-EQUAL M-B A-ZERO SPHT1)       ;IF NOT FOUND, TRY NEXT


;COMES HERE WHEN A PAGE NEEDS TO BE READ IN FROM DISK.
;
;FIRST, FIND SOME MEMORY.  ENTER A LOOP THAT SEARCHES PHYSICAL-PAGE-DATA,
;STARTING FROM LAST PLACE STOPPED, FOR A FLUSHABLE PAGE.  IF NONE
;FOUND, SEARCH INSTEAD FOR ANY NON WIRED PAGE. (THE EMERGENCY CASE.)
;
;HAVING FOUND A PAGE TO REPLACE, WRITE IT TO THE DISK IF NECESSARY.  THEN DELETE
;THAT ENTRY FROM THE PAGE HASH TABLE (HARD), AND FROM THE HARDWARE MAP (EASY).
;
;PERFORM THE DISK READ INTO THE CORE PAGE THUS MADE FREE.
;
;USE A PIPELINED LOOP TO SEARCH THE REGION TABLES AT MEMORY SPEED TO FIND THE
;REGION CONTAINING THE PAGE BEING REFERENCED, AND GET THE META BITS.
;
;NOW RE-HASH THE ADDRESS ORIGINALLY BEING
;REFERENCED TO FIND THE FIRST HOLE (MAY HAVE MOVED DUE TO DELETION) AND PUT
;IN AN ENTRY FOR THAT PAGE.  RESTART THE REFERENCE (SET UP THE MAP FIRST?)

SWAPIN (declare (local a-disk-page-read-count a-disk-page-write-count a-disk-error-count
                       a-disk-fresh-page-count a-disk-swapin-virtual-address
                       a-disk-swap-in-ccw-pointer a-disk-swapin-size
                       a-disk-read-compare-rewrites a-disk-ecc-count
                       a-disk-read-compare-rereads a-disk-page-read-op-count
                       a-disk-page-write-op-count a-disk-page-write-wait-count
                       a-disk-page-write-busy-count
                       a-disk-page-write-appends a-disk-page-read-appends))
        (CALL-IF-BIT-SET M-INTERRUPT-FLAG ILLOP)        ;Uh uh, no paging from interrupts
  ;decide how many pages to bring in with one disk op.  Must not bring in again a page
  ;already in.  Must not cross region boundaries. (There is no reason to believe we
  ;will need pages from another region and complications arise in making the pages known.)
        ((A-DISK-SWAPIN-VIRTUAL-ADDRESS) DPB M-ZERO Q-ALL-BUT-POINTER A-PGF-VMA)
        ((A-DISK-SWAP-IN-CCW-POINTER) (A-CONSTANT DISK-SWAP-IN-CCW-BASE))
        ((A-DISK-SWAPIN-SIZE) (A-CONSTANT 1))
        (JUMP-IF-BIT-SET M-DONT-SWAP-IN SWAPIN-SIZE-X)  ;going to create 0 core, no disk op.
        ((M-TEM) A-DISK-SWITCHES)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 3) M-TEM SWAPIN-SIZE-X)  ;multi-swapin not enabled


        ((C-PDL-BUFFER-POINTER-PUSH) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        (CALL XRGN)                             ;=> region number in M-T.. no XCT-NEXT.
        (CALL-EQUAL M-T A-V-NIL ILLOP)          ;Swapping in a page not in a region
        ((A-DISK-SAVE-PGF-T) M-T)
        ((VMA-START-READ) ADD M-T A-V-REGION-BITS)
        (ILLOP-IF-PAGE-FAULT)
        ((A-DISK-SAVE-PGF-A) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        ((A-DISK-SAVE-1) (LISP-BYTE %%REGION-SWAPIN-QUANTUM) READ-MEMORY-DATA)
SWAPIN-SIZE-LOOP
        (JUMP-GREATER-OR-EQUAL M-ZERO A-DISK-SAVE-1 SWAPIN-SIZE-X)
        ((M-A) (A-CONSTANT (EVAL PAGE-SIZE)))
        ((A-DISK-SAVE-PGF-A) ADD M-A A-DISK-SAVE-PGF-A)
        ((m-tem) a-disk-save-pgf-a)             ;disk swap operation can't cross a quantum boundary
        ((m-tem) ldb (lisp-byte %%virtual-address-offset-in-quantum) m-tem)
        ((m-tem) ldb vma-page-addr-part m-tem)
        (jump-equal m-tem a-zero swapin-size-x)                         ;not same quantum
        ((C-PDL-BUFFER-POINTER-PUSH) A-DISK-SAVE-PGF-A)
        (CALL XRGN)
        (JUMP-NOT-EQUAL M-T A-DISK-SAVE-PGF-T SWAPIN-SIZE-X)            ;not same region
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)
       ((M-T) A-DISK-SAVE-PGF-A)
        (JUMP-IF-BIT-SET PHT1-VALID-BIT READ-MEMORY-DATA SWAPIN-SIZE-X) ;page in core.
        (jump-if-bit-set m-transport-flag swapin-append-page)
        (jump-if-bit-clear m-scavenge-flag swapin-append-page)
     ;; In scavenger -- stop appending if we don't need to look at this page.
        ((m-lam) a-disk-save-pgf-a)
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number m-lam)
        (jump-less-than m-tem a-scavenge-volatility swapin-size-x)
swapin-append-page
          ;append to transfer
        ((A-DISK-PAGE-READ-APPENDS) M+A+1 M-ZERO A-DISK-PAGE-READ-APPENDS)
        ((A-DISK-SWAPIN-SIZE) M+A+1 M-ZERO A-DISK-SWAPIN-SIZE)
        (JUMP-XCT-NEXT SWAPIN-SIZE-LOOP)
       ((A-DISK-SAVE-1) ADD (M-CONSTANT -1) A-DISK-SAVE-1)

(locality a-mem)
a-hexadec-required-alignment (0)
(locality i-mem)

SWAPIN-SIZE-X
#+exp   (jump swapin0-a)
#-lambda(begin-comment)
        ((M-TEM) A-PROCESSOR-SWITCHES)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 4) M-TEM SWAPIN0-a)   ;JUMP ON NO FAST CACHE
        ((m-tem) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        ;LOW 4 BITS OF VIRTUAL PAGE NUMBER.
        ((a-hexadec-required-alignment) LDB (BYTE-FIELD 4 8) M-TEM)
        (CALL LOAD-SCAN-FOR-HEXADEC)
        (CALL XFIND-HEXA0)
        (CALL STORE-SCAN-FOR-HEXADEC)
        (JUMP SWAPIN1)
#-lambda(end-comment)

SWAPIN-LOOP
#+exp   (jump swapin0-a)
#-lambda(begin-comment)
        ((m-tem) a-processor-switches)
        (jump-if-bit-clear (byte-field 1 4) m-tem swapin0-a)   ;JUMP ON NO FAST CACHE
        ;Next page into next hexadec
        ((m-tem) m+a+1 m-zero a-hexadec-required-alignment)
        ((a-hexadec-required-alignment) ldb m-tem (byte-field 4 0) a-zero)
        (CALL LOAD-SCAN-FOR-HEXADEC)
        (CALL XFIND-HEXA0)
        (CALL STORE-SCAN-FOR-HEXADEC)
        (JUMP SWAPIN1)
#-lambda(end-comment)


XFINDCORE-HEXADEC (MISC-INST-ENTRY %FINDCORE-HEXADEC)
#-exp(begin-comment)
        ((m-garbage) pdl-pop)
        (jump xfindcore)
#-exp(end-comment)
#-lambda(begin-comment)
  ;Find memory with specified low 4 bits of physical page number.
        ((a-hexadec-required-alignment) ldb (byte 4 0) C-PDL-BUFFER-POINTER-POP)
        (call load-scan-for-hexadec)
        (call XFIND-HEXA0)
        (call store-scan-for-hexadec)
#-lambda(end-comment)
return-m-b-as-fixnum
        (popj-after-next (m-t) dpb m-b q-pointer (a-constant (byte-value q-data-type dtp-fix)))
       (no-op)

XFINDCORE (MISC-INST-ENTRY %FINDCORE)
        ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC RETURN-M-B-as-fixnum)))
SWAPIN0
XFIND-HEXA0
        ((M-B) A-FINDCORE-SCAN-POINTER)                 ;last page frame to returned
FINDCORE0
        ((m-b) add m-b (a-constant 1))
        (CALL-GREATER-OR-EQUAL M-B A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH FINDCORE2)
        ((VMA-START-READ) ADD M-B A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)                           ;Delayed for fencepost error
        (JUMP-EQUAL M-B A-FINDCORE-SCAN-POINTER FINDCORE3)      ;Did all pages but 1, no luck
        ((m-tem) a-processor-switches)
        (jump-if-bit-set (byte 1 4) m-tem findcore-in-hexadec)
;; Added I-LONG to this ldb, since it seems to lose occasionally.  KHS 840912
#+LAMBDA((M-TEM) (BYTE-FIELD 20 0) READ-MEMORY-DATA I-LONG)     ;PHT entry index
#+EXP   ((M-TEM) (BYTE-FIELD 20 0) READ-MEMORY-DATA)
        (JUMP-EQUAL-XCT-NEXT M-TEM (A-CONSTANT 177777) FINDCORE0)       ;No page here
       ((A-COUNT-FINDCORE-STEPS) M+A+1 M-ZERO A-COUNT-FINDCORE-STEPS)
        ((VMA-START-READ M-T) ADD M-TEM A-V-PAGE-TABLE-AREA)
        (illop-if-page-fault)
        (DISPATCH PHT1-SWAP-STATUS-CODE READ-MEMORY-DATA D-FINDCORE)
       (CALL-GREATER-OR-EQUAL M-B A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH FINDCORE2)

FINDCORE2       ;Reached end of memory.  Wrap around to page zero.  There can be pageable
                ;memory in the middle of the wired pages on machines with small memory.
        (declare (values a-b))
        (POPJ-AFTER-NEXT (M-B) A-ZERO)
       (NO-OP)

FINDCORE-IN-HEXADEC
        ((M-TEM) LDB (BYTE-FIELD 4 0) M-B)
        ;;check for correct physical boundary
        (JUMP-NOT-EQUAL-XCT-NEXT M-TEM a-hexadec-required-alignment FINDCORE0)
       ((A-COUNT-FINDCORE-STEPS) M+A+1 M-ZERO A-COUNT-FINDCORE-STEPS)
;; Added I-LONG to this ldb, since it seems to lose occasionally.  KHS 840912
#+LAMBDA((M-TEM) (BYTE-FIELD 20 0) READ-MEMORY-DATA I-LONG)     ;PHT entry index
#+EXP   ((M-TEM) (BYTE-FIELD 20 0) READ-MEMORY-DATA)
        (JUMP-EQUAL M-TEM (A-CONSTANT 177777) FINDCORE0)        ;No page here
        ((VMA-START-READ M-T) ADD M-TEM A-V-PAGE-TABLE-AREA)
        (illop-if-page-fault)
        (DISPATCH PHT1-SWAP-STATUS-CODE READ-MEMORY-DATA D-FINDCORE)
       (CALL-GREATER-OR-EQUAL M-B A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH FINDCORE2)


;; Searched all of memory (except for the last page brought in), time for emergency measures
;; Age all of the pages in memory, which should make some flushable
FINDCORE3
        ((A-COUNT-FINDCORE-EMERGENCIES) M+A+1 M-ZERO A-COUNT-FINDCORE-EMERGENCIES)
        ((M-T) M-1)                             ;Mustn't clobber M-1
        (CALL-XCT-NEXT AGER)
       ((M-1) A-FINDCORE-SCAN-POINTER)
        (JUMP-XCT-NEXT FINDCORE0)               ;Try again
       ((M-1) M-T)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)            ;DISPATCH TABLE TO LOOK FOR FLUSHABLE PAGES
D-FINDCORE                      ;DISPATCH ON SWAP STATUS
        (FINDCORE0)                     ;0 ILLEGAL
        (FINDCORE0)                     ;1 NORMAL
        (INHIBIT-XCT-NEXT-BIT COREFOUND)        ;2 FLUSHABLE
        (INHIBIT-XCT-NEXT-BIT COREFOUND-PRE)    ;3 PREPAGE
        (FINDCORE0)                     ;4 AGE TRAP
        (FINDCORE0)                     ;5 WIRED DOWN
        (FINDCORE0)                     ;6 NOT USED
        (FINDCORE0)                     ;7 NOT USED
(END-DISPATCH)

;(START-DISPATCH 3 0)           ;FOR SWAP-OUT CANDIDATE FROM SCAV WORKING-SET
;D-SCAV-SWAPOUT                 ;DISPATCH ON SWAP STATUS
;       (INHIBIT-XCT-NEXT-BIT SWAPIN0)  ;0 ILLEGAL
;       (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;1 NORMAL - TAKE
;       (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;2 FLUSHABLE - TAKE
;       (P-BIT R-BIT)                           ;3 PREPAGE - TAKE AND METER
;       (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;4 AGE TRAP - TAKE
;       (INHIBIT-XCT-NEXT-BIT SWAPIN0)  ;5 WIRED DOWN
;       (INHIBIT-XCT-NEXT-BIT SWAPIN0)  ;6 NOT USED
;       (INHIBIT-XCT-NEXT-BIT SWAPIN0)  ;7 NOT USED
;(END-DISPATCH)

(START-DISPATCH 3 0)            ;DISPATCH TABLE TO DROP THROUGH IF PAGE NEEDS WRITING
D-WRITEBACK-NEEDED              ;DISPATCH ON MAP STATUS
        (INHIBIT-XCT-NEXT-BIT P-BIT ILLOP)      ;0 ILLEGAL (LVL 1 MAP)
        (INHIBIT-XCT-NEXT-BIT P-BIT ILLOP)      ;1 ILLEGAL (LVL 2 MAP)
        (INHIBIT-XCT-NEXT-BIT COREFOUND2)       ;2 READ ONLY
        (INHIBIT-XCT-NEXT-BIT COREFOUND2)       ;3 READ/WRITE FIRST
        (P-BIT R-BIT)                           ;4 READ/WRITE - INDICATES PAGE MODIFIED
        (P-BIT R-BIT)                           ;5 PDL BUFFER, ALWAYS WRITE PDL-BUFFER PAGES
                                                ;   SINCE R/W/F MECHANISM NOT AVAILABLE.
        (P-BIT R-BIT)                           ;6 MAR BREAK, ALWAYS WRITE FOR SAME REASON
        (INHIBIT-XCT-NEXT-BIT P-BIT ILLOP)      ;7 nubus physical in PHT2
(END-DISPATCH)

(START-DISPATCH 3 0)            ;DISPATCH TABLE TO DROP THROUGH IF PAGE NEEDS WRITING
D-WRITEBACK-NEEDED-CCW          ;DISPATCH ON MAP STATUS
        (INHIBIT-XCT-NEXT-BIT P-BIT ILLOP)      ;0 ILLEGAL (LVL 1 MAP)
        (INHIBIT-XCT-NEXT-BIT P-BIT ILLOP)      ;1 ILLEGAL (LVL 2 MAP)
        (INHIBIT-XCT-NEXT-BIT COREF-CCW-X)      ;2 READ ONLY
        (INHIBIT-XCT-NEXT-BIT COREF-CCW-X)      ;3 READ/WRITE FIRST
        (P-BIT R-BIT)                           ;4 READ/WRITE - INDICATES PAGE MODIFIED
        (INHIBIT-XCT-NEXT-BIT COREF-CCW-X)      ;5 PDL BUFFER, ALWAYS WRITE PDL-BUFFER PAGES
                                                ;   SINCE R/W/F MECHANISM NOT AVAILABLE.
                                                ;HOWEVER, WE DONT APPEND THESE.
        (INHIBIT-XCT-NEXT-BIT COREF-CCW-X)      ;6 MAR BREAK, ALWAYS WRITE FOR SAME REASON
                                                ;HOWEVER, WE DONT APPEND THESE.
        (INHIBIT-XCT-NEXT-BIT P-BIT ILLOP)      ;7 nubus physical in PHT2
(END-DISPATCH)
(LOCALITY I-MEM)

;Here when we've found a page to evict.  M-B has the PFN+1.
;VMA and MD are for the PHT1.  M-T same as VMA.

;This version for the case where victim was pre-paged in and not used
COREFOUND-PRE
        (JUMP-XCT-NEXT COREFOUND0)
       ((A-DISK-PREPAGE-NOT-USED-COUNT) M+A+1 M-ZERO A-DISK-PREPAGE-NOT-USED-COUNT)

;This version for the normal case
COREFOUND
COREFOUND0
        ((A-FINDCORE-SCAN-POINTER) M-B)         ;Next time, start search with page after this
COREFOUND3                                      ;Enter here on %DELETE-PHYSICAL-PAGE.
        (CALL-IF-BIT-CLEAR PHT1-VALID-BIT READ-MEMORY-DATA ILLOP)
        ((M-A) READ-MEMORY-DATA)                        ;PHT1
COREFOUND1
        (CALL-NOT-EQUAL A-PAGE-TRACE-PTR M-ZERO PAGE-TRACE-OUT) ;Trace page eviction
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-PAGE-FAULT-ENABLE) M-METER-ENABLES
                METER-PAGE-OUT)

        ;;*** When there is background writing, will have to synchronize here
        ;;*** This will require dual modified bits or something.
        ((VMA-START-READ) ADD M-T (A-CONSTANT 1))       ;Get PHT2
        (ILLOP-IF-PAGE-FAULT)                           ;PHT should be addressable
        (JUMP-IF-BIT-SET PHT1-MODIFIED-BIT M-A COREFOUND1A)
        (DISPATCH PHT2-MAP-STATUS-CODE
                 READ-MEMORY-DATA D-WRITEBACK-NEEDED)   ;See if needs writing
COREFOUND1A     ;Page needs to be written back to disk
        ((C-PDL-BUFFER-POINTER-PUSH) M-T)               ;PHT1 address. - randomly used below to restore m-t
                                                        ; original use is at coref-ccw-x
        ((A-DISK-SWAP-OUT-CCW-POINTER) (A-CONSTANT DISK-SWAP-OUT-CCW-BASE))
        ((A-DISK-PAGE-WRITE-COUNT) M+A+1 M-ZERO A-DISK-PAGE-WRITE-COUNT)
     ;add main memory page frame number in M-B to CCW list.
#-lambda(begin-comment)
        ((WRITE-MEMORY-DATA) DPB M-B VMA-PHYS-PAGE-ADDR-PART (A-CONSTANT 1))
        ((VMA-START-WRITE) A-DISK-SWAP-OUT-CCW-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((A-DISK-SWAP-OUT-CCW-POINTER)
             ADD A-DISK-SWAP-OUT-CCW-POINTER M-ZERO ALU-CARRY-IN-ONE)
#-lambda(end-comment)
#-exp(begin-comment)
        ((m-t) dpb m-b vma-phys-page-addr-part a-zero)
        (call translate-cadr-physical-to-nubus)
        ((md) m-lam)
        ((vma-start-write) a-disk-swap-out-ccw-pointer)
        (illop-if-page-fault)
        ((md) (a-constant 1024.))
        ((vma-start-write) add vma (a-constant 1))
        (illop-if-page-fault)
        ((a-disk-swap-out-ccw-pointer) add vma (a-constant 1))
        ((m-t) pdl-top)
#-exp(end-comment)
        ((A-DISK-SAVE-PGF-A) M-A)
        ((A-DISK-SAVE-PGF-B) M-B)
        ((M-TEM) A-DISK-SWITCHES)               ;Multiple page swapouts enabled?
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 2) M-TEM COREF-CCW-X)
        ((A-DISK-SAVE-1) M-A)
COREF-CCW-0
        ((M-T) (A-CONSTANT (EVAL PAGE-SIZE)))
        ((A-DISK-SAVE-1) ADD M-T A-DISK-SAVE-1)
        ;;; see if we pass a quantum boundary
        ((m-tem) dpb m-zero vma-quantum-byte a-disk-save-1)     ;clear quantum number
        ((m-tem) ldb vma-page-addr-part m-tem)
        (jump-equal m-tem a-zero coref-ccw-x)   ;no more pages to write out so go do it
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)   ;Is next higher page in core?
       ((M-T) A-DISK-SAVE-1)                     ; virt adr in M-T.
                ; clobbers m-a m-b m-t a-tem1 a-tem3
        (JUMP-IF-BIT-CLEAR PHT1-VALID-BIT READ-MEMORY-DATA COREF-CCW-X) ;not found.
                ;That page in core, does it need to be written?
        ((M-T) VMA)                             ;Save PHT1 adr.
        ((M-A) MD)                              ;Save PHT1.
        ((VMA-START-READ) ADD M-T (A-CONSTANT 1))       ;get PHT2
        (ILLOP-IF-PAGE-FAULT)
        ((M-B) READ-MEMORY-DATA)
        (JUMP-IF-BIT-SET PHT1-MODIFIED-BIT M-A COREF-CCW-ADD)
        (DISPATCH PHT2-MAP-STATUS-CODE M-B D-WRITEBACK-NEEDED-CCW)  ;See if needs writing
COREF-CCW-ADD
        ((WRITE-MEMORY-DATA M-A) ANDCA M-A
           (A-CONSTANT (BYTE-MASK PHT1-MODIFIED-BIT))) ;clear modified flag
        ((VMA-START-WRITE) M-T)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) PHT2-MAP-STATUS-CODE M-B)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT 4) COREF-CCW-ADD-1)  ;change RW to RWF
        ((M-TEM) (A-CONSTANT 3))
        ((WRITE-MEMORY-DATA M-B) DPB M-TEM PHT2-MAP-STATUS-CODE A-B)
        ((VMA-START-WRITE) ADD M-T (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((MD) M-A)                              ;address the map
        (no-op)         ;allow time
        ((M-TEM) L2-MAP-STATUS-CODE)            ;see if map is set up
        (JUMP-LESS-THAN M-TEM (A-CONSTANT 2) COREF-CCW-ADD-1)
        (CALL-XCT-NEXT LOAD-L2-MAP-FROM-CADR-PHYSICAL)  ;PHT2 same as 2ND LVL MAP(on cadr)
       ((M-LAM) M-B)
COREF-CCW-ADD-1
        ((A-DISK-PAGE-WRITE-APPENDS) M+A+1 M-ZERO A-DISK-PAGE-WRITE-APPENDS)
        ((A-DISK-PAGE-WRITE-COUNT) M+A+1 M-ZERO A-DISK-PAGE-WRITE-COUNT)
   ;add main memory page frame number in M-B to CCW list.
#-lambda(begin-comment)
        ((WRITE-MEMORY-DATA) DPB M-B VMA-PHYS-PAGE-ADDR-PART (A-CONSTANT 1))
        ((VMA-START-WRITE) A-DISK-SWAP-OUT-CCW-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((A-DISK-SWAP-OUT-CCW-POINTER)
           M+A+1 A-DISK-SWAP-OUT-CCW-POINTER M-ZERO)
#-lambda(end-comment)
#-exp (begin-comment)
        ((m-tem1) m-t)
        ((m-t) dpb m-b vma-phys-page-addr-part a-zero)
        (call translate-cadr-physical-to-nubus)
        ((md) m-lam)
        ((vma-start-write) a-disk-swap-out-ccw-pointer)
        (illop-if-page-fault)
        ((md) (a-constant 1024.))
        ((vma-start-write) add vma (a-constant 1))
        (illop-if-page-fault)
        ((a-disk-swap-out-ccw-pointer) add vma (a-constant 1))
        ((m-t) m-tem1)
#-exp(end-comment)
        ((M-TEM) A-DISK-SWAP-OUT-CCW-POINTER)
        (JUMP-LESS-THAN M-TEM (A-CONSTANT DISK-SWAP-OUT-CCW-MAX) COREF-CCW-0)
COREF-CCW-X
#-lambda(begin-comment)
        ((VMA-START-READ) ADD A-DISK-SWAP-OUT-CCW-POINTER (M-CONSTANT -1))
        (ILLOP-IF-PAGE-FAULT)
        ((WRITE-MEMORY-DATA-START-WRITE) SUB READ-MEMORY-DATA (A-CONSTANT 1)) ;last CCW
        (ILLOP-IF-PAGE-FAULT)
#-lambda(end-comment)
        ((A-DISK-PAGE-WRITE-OP-COUNT) M+A+1 M-ZERO A-DISK-PAGE-WRITE-OP-COUNT)
        ((M-A) A-DISK-SAVE-PGF-A)                       ;get back base virt adr.
        ((M-B) A-DISK-SAVE-PGF-B)                       ;get back page frame number of first
                                                        ; page.  It is no longer used by
                                                        ; disk swap handler, but is needed
                                                        ; by COREFOUND2.
        ((C-PDL-BUFFER-POINTER-PUSH) M-C)
        ((M-C) (A-CONSTANT DISK-SWAP-OUT-CCW-BASE))     ;M-C
        ((m-tem4) a-disk-swap-out-ccw-pointer)
        ((m-tem4) sub m-tem4 a-c)       ;length of transfer in pages (for hexadec aging hack).
#+exp   ((m-tem4) ldb (byte-field 31. 1) m-tem4) ;divide by 2
        (CALL-XCT-NEXT DISK-SWAP-HANDLER)               ;Do the write (virt adr in M-A)
       ((M-T) (A-CONSTANT DISK-WRITE-COMMAND))
        ((M-C) C-PDL-BUFFER-POINTER-POP)
        ((A-DISK-PAGE-WRITE-WAIT-COUNT) M+A+1 M-ZERO A-DISK-PAGE-WRITE-WAIT-COUNT)
        ((M-T) C-PDL-BUFFER-POINTER-POP)                ;RESTORE PHT ENTRY ADDRESS
;DROPS THROUGH
;DROPS IN
;AT THIS POINT, M-T HAS ADDR OF PHT ENTRY TO BE DELETED,
;M-A HAS ITS VIRTUAL ADDRESS, M-B HAS ITS PAGE FRAME NUMBER (NOT! PHYSICAL ADDRESS)
;DELETION WORKS BY FINDING PAGES THAT SHOULD HAVE HASHED TO THE
;HOLE WHERE THE THING WAS DELETED, AND EXCHANGING THEM WITH THE HOLE.
;NOTE THAT THE ALGORITHM IN THE PAGING MEMO IS WRONG.
;CONVENTIONS: M-B POINTS AT THE HOLE, VMA POINTS AT THE ITEM SUSPECTED
;OF BEING IN THE WRONG PLACE, M-PGF-TEM POINTS AT THE UPPERMOST ENTRY IN THE PHT,
;M-T POINTS AT WHERE (VMA) SHOULD HAVE HASHED TO. THESE ARE TYPELESS ABSOLUTE ADDRESSES.

COREFOUND2
        ((C-PDL-BUFFER-POINTER-PUSH) M-B)       ;Save page frame number
        ((WRITE-MEMORY-DATA) (M-CONSTANT -1))           ;Remove pointer to PHT entry
        ((VMA-START-WRITE) ADD M-B A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)
        ((M-B) M-T)                                     ;-> PHT entry to delete
        ((M-PGF-TEM) DPB M-ZERO Q-ALL-BUT-POINTER A-V-PAGE-TABLE-AREA)
        ((M-PGF-TEM) ADD M-PGF-TEM A-PHT-INDEX-LIMIT)   ;-> last entry in table +2
PHTDEL1 ((WRITE-MEMORY-DATA) a-zero)                    ;Delete PHT entry.
        ((VMA-START-WRITE M-B) Q-POINTER M-B)
        (ILLOP-IF-PAGE-FAULT)                           ;Supposed to be wired
PHTDEL2 ((VMA-START-READ) ADD VMA (A-CONSTANT 2))       ;Check location following hole
        (JUMP-GREATER-OR-EQUAL VMA A-PGF-TEM PHTDEL5)   ;Jump if wrap around
PHTDEL3 (ILLOP-IF-PAGE-FAULT)
        (JUMP-IF-BIT-CLEAR PHT1-VALID-BIT READ-MEMORY-DATA PHTDELX)
        ((M-T) SELECTIVE-DEPOSIT READ-MEMORY-DATA       ;Check for dummy entry
                PHT1-VIRTUAL-PAGE-NUMBER (A-CONSTANT -1))       ;which has an address of -1
        (JUMP-EQUAL-XCT-NEXT M-T (A-CONSTANT -1) PHTDEL7)       ;Dummy always hashes
       ((M-T) M-B)                                              ; to the hole
        (CALL-XCT-NEXT COMPUTE-PAGE-HASH)               ;Something there, rehash it
       ((M-T) READ-MEMORY-DATA)
        ((M-T) ADD M-T A-V-PAGE-TABLE-AREA)             ;Convert fixnum hash to address
        ((M-T) Q-POINTER M-T)                           ; sans extra bits
PHTDEL7 (JUMP-LESS-THAN VMA A-T PHTDEL4)                ;Jump on funny wrap around case
        (JUMP-GREATER-THAN M-T A-B PHTDEL2)             ;Jump if hole is not between where
        (JUMP-LESS-THAN VMA A-B PHTDEL2)                ; the frob is and where it hashes to
PHTDEL6 ((C-PDL-BUFFER-POINTER-PUSH) READ-MEMORY-DATA)  ;Move the cell into the hole
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((M-T) SUB VMA (A-CONSTANT 1))                  ;Save pointer to moved cell
        ((WRITE-MEMORY-DATA) READ-MEMORY-DATA)          ;Complete the cycle
        ((VMA-START-WRITE) ADD M-B (A-CONSTANT 1))      ;Address the hole, store PHT2
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) PHT2-PHYSICAL-PAGE-NUMBER MD)          ;Fix up physical-page-data
;; Bug was found.
  ;error check for clobbering PPD entry 0
;  (call-equal m-tem a-zero illop)
        ((VMA-START-READ) ADD M-TEM A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) SUB M-B A-V-PAGE-TABLE-AREA)           ;New PHT index
     ;; *** Will need to be changed if PHT gets Bigger ***
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT
                READ-MEMORY-DATA (BYTE-FIELD 20 20) A-TEM)
        (ILLOP-IF-PAGE-FAULT)
        ((VMA) M-B)
        ((WRITE-MEMORY-DATA-START-WRITE) C-PDL-BUFFER-POINTER-POP) ;Store PHT1
        (ILLOP-IF-PAGE-FAULT)
        (JUMP-XCT-NEXT PHTDEL1)                         ;Make the moved cell into new hole
       ((M-B) M-T)

PHTDEL4 (JUMP-LESS-OR-EQUAL M-T A-B PHTDEL6)            ;Jump if hole is between where the
        (JUMP-GREATER-OR-EQUAL VMA A-B PHTDEL6)         ; frob is and where it hashes to
        (JUMP PHTDEL2)                                  ;It's not, loop more

PHTDEL5 (JUMP-XCT-NEXT PHTDEL3)                         ;Wrap around to beg of PHT
       ((VMA-START-READ) DPB M-ZERO Q-ALL-BUT-POINTER A-V-PAGE-TABLE-AREA)

PHTDELX ((M-B) C-PDL-BUFFER-POINTER-POP)                ;Restore found page frame number (?)
        ((MD) M-A)              ;Access map for virt page deleted
#+exp   (no-op)
        (POPJ-AFTER-NEXT
         (#+lambda L2-MAP-CONTROL
          #+exp vma-write-l2-map-control) (A-CONSTANT 0))       ;Flush 2nd lvl map, if any
                        ;Note that if we have a first-level map miss, this does no harm
       ((VMA) A-V-NIL)          ;Don't leave garbage in VMA.

swapin0-a
        (call swapin0)
;We have found one page of core, store it away in the CCW and loop
; until we have got enuf for the transfer we intend.
SWAPIN1    ;add main memory page frame number in M-B to CCW list.
#-lambda(begin-comment)
        ((WRITE-MEMORY-DATA) DPB M-B VMA-PHYS-PAGE-ADDR-PART (A-CONSTANT 1))
        ((VMA-START-WRITE) A-DISK-SWAP-IN-CCW-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((A-DISK-SWAP-IN-CCW-POINTER) M+A+1 A-DISK-SWAP-IN-CCW-POINTER M-ZERO)
#-lambda(end-comment)
#-exp(begin-comment)
        ((m-tem1) m-t)
        ((m-t) dpb m-b vma-phys-page-addr-part a-zero)
        (call translate-cadr-physical-to-nubus)
        ((md) m-lam)
        ((vma-start-write) a-disk-swap-in-ccw-pointer)
        (illop-if-page-fault)
        ((md) (a-constant 1024.))
        ((vma-start-write) add vma (a-constant 1))
        (illop-if-page-fault)
        ((a-disk-swap-in-ccw-pointer) add vma (a-constant 1))
        ((m-t) m-tem1)
#-exp(end-comment)
        ((A-DISK-PAGE-READ-COUNT) ADD M-ZERO A-DISK-PAGE-READ-COUNT ALU-CARRY-IN-ONE)
        ((A-DISK-SWAPIN-SIZE) ADD A-DISK-SWAPIN-SIZE (M-CONSTANT -1))
        (JUMP-NOT-EQUAL A-DISK-SWAPIN-SIZE M-ZERO SWAPIN-LOOP)
#-lambda(begin-comment)
        ((VMA-START-READ) ADD A-DISK-SWAP-IN-CCW-POINTER (M-CONSTANT -1))  ;finish ccw list
        (ILLOP-IF-PAGE-FAULT)
        ((WRITE-MEMORY-DATA-START-WRITE) SUB WRITE-MEMORY-DATA (A-CONSTANT 1)) ;last
        (ILLOP-IF-PAGE-FAULT)
#-lambda(end-comment)
;SWAPIN1-GO
;CONTINUE SWAPPING IN.  NEXT STEP IS TO SEARCH REGION TABLES TO FIND META BITS.
        (CALL PAGE-IN-GET-MAP-BITS)     ;note M-B still holds page frame number in path
                                        ; to CZRR.
        (JUMP-IF-BIT-SET M-DONT-SWAP-IN CZRR)           ;IF FRESH PAGE DON'T REALLY SWAP IN
        ((C-PDL-BUFFER-POINTER-PUSH) M-C)
        ((M-C) (A-CONSTANT DISK-SWAP-IN-CCW-BASE))      ;CCW list pointer (CLP)
        ((m-tem4) a-disk-swap-in-ccw-pointer)
        ((m-tem4) sub m-tem4 a-c)       ;transfer size in pages for hexadec aging.
#+exp   ((m-tem4) ldb (byte-field 31. 1) m-tem4) ;divide by 2
        (CALL-XCT-NEXT DISK-SWAP-HANDLER)               ;Do actual disk transfer
       ((M-T) (A-CONSTANT DISK-READ-COMMAND))
        ((M-C) C-PDL-BUFFER-POINTER-POP)
SWAPIN2
        ;; Now loop through ccw list making the pages known.
        ((M-B) (A-CONSTANT DISK-SWAP-IN-CCW-BASE))
        ;; First page in gets normal swap-status
        ((A-PAGE-IN-PHT1) (A-CONSTANT (PLUS (BYTE-VALUE PHT1-VALID-BIT 1)
                                            (BYTE-VALUE PHT1-SWAP-STATUS-CODE 1))))
        ((A-DISK-PAGE-READ-OP-COUNT) ADD M-ZERO A-DISK-PAGE-READ-OP-COUNT ALU-CARRY-IN-ONE)
SWAPIN2-LOOP
        (CALL-NOT-EQUAL A-PAGE-TRACE-PTR M-ZERO PAGE-TRACE-IN) ;Trace page swapin
        (CALL-IF-BIT-SET (LISP-BYTE %%METER-PAGE-FAULT-ENABLE) M-METER-ENABLES
                METER-PAGE-IN)
        ((VMA-START-READ) M-B)
        (ILLOP-IF-PAGE-FAULT)
#-exp (begin-comment)
        ((m-lam) md)
        (call translate-nubus-to-cadr-physical)
        ((md) m-lam)
#-exp (end-comment)
        ((A-DISK-SWAPIN-PAGE-FRAME) LDB VMA-PHYS-PAGE-ADDR-PART READ-MEMORY-DATA A-ZERO)
        ((C-PDL-BUFFER-POINTER-PUSH) M-B)
        (CALL PAGE-IN-MAKE-KNOWN)
        ((M-B) C-PDL-BUFFER-POINTER-POP)
        ((M-A) (A-CONSTANT (EVAL PAGE-SIZE)))
        ((A-DISK-SWAPIN-VIRTUAL-ADDRESS) ADD M-A A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        ((m-b) add m-b (a-constant #+lambda 1 #+exp 2))
        (JUMP-LESS-THAN-XCT-NEXT M-B A-DISK-SWAP-IN-CCW-POINTER SWAPIN2-LOOP)
        ;; Pages after the first get pre-paged swap-status
       ((A-PAGE-IN-PHT1) (A-CONSTANT (PLUS (BYTE-VALUE PHT1-VALID-BIT 1)
                                           (BYTE-VALUE PHT1-SWAP-STATUS-CODE 3))))
SWAPIN2-X
        (JUMP PGF-RESTORE)      ;TAKE FAULT AGAIN SINCE DISK XFER
                                ;MAY HAVE FAULTED AND FLUSHED SECOND LEVEL MAP BLOCK.


PAGE-IN-GET-MAP-BITS   ;Get PHT2 bits and leave them in A-DISK-SWAPIN-PHT2-BITS.
        ((C-PDL-BUFFER-POINTER-PUSH) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        (CALL XRGN)                                     ;=> region number in M-T
        (CALL-EQUAL M-T A-V-NIL ILLOP)                  ;Swapping in a page not in a region
        ((VMA-START-READ) ADD M-T A-V-REGION-BITS)      ;Get misc bits word
        (ILLOP-IF-PAGE-FAULT)                           ;Should be wired down
        ((M-A) DPB M-ZERO Q-ALL-BUT-POINTER A-DISK-SWAPIN-VIRTUAL-ADDRESS)

        ((m-tem) ldb (lisp-byte %%region-map-bits) md)
        ((a-disk-swapin-pht2-bits) dpb m-tem pht2-access-status-and-meta-bits a-zero)

;       ((m-lam) a-disk-swapin-virtual-address)
;       (call-xct-next read-page-volatility)
;       ((m-lam) q-page-number m-lam)
;       ((a-disk-swapin-pht2-bits) dpb m-tem pht2-map-volatility a-disk-swapin-pht2-bits)

 ;      ((M-TEM) SELECTIVE-DEPOSIT READ-MEMORY-DATA (LISP-BYTE %%REGION-MAP-BITS) a-zero)
 ;      ((A-DISK-SWAPIN-PHT2-BITS) M-TEM)

        ((M-T) A-MAR-LOW)                               ;Check VMA against MAR
        ((M-T) SELECTIVE-DEPOSIT M-T VMA-PAGE-ADDR-PART A-ZERO)
        (POPJ-LESS-THAN M-A A-T)
        ((M-T) A-MAR-HIGH)
        ((M-T) SELECTIVE-DEPOSIT M-T VMA-PAGE-ADDR-PART (A-CONSTANT (EVAL (1- PAGE-SIZE))))
        (POPJ-GREATER-THAN M-A A-T)     ;If MAR to be set, change map status and turn off
        (POPJ-AFTER-NEXT
         (M-T) (A-CONSTANT (EVAL %PHT-MAP-STATUS-MAR))) ; hardware access
       ((A-DISK-SWAPIN-PHT2-BITS) DPB M-T PHT2-MAP-ACCESS-AND-STATUS-CODE A-disk-swapin-pht2-bits)

;;; Second part.  Make physical page frame number A-DISK-SWAPIN-PHYSICAL-PAGE-FRAME
;;;  known at A-DISK-SWAPIN-VIRTUAL-ADDRESS.  PHT2 bits are in
;;;  A-DISK-SWAPIN-PHT2-BITS.
;;; A-PAGE-IN-PHT1 contains the bits desired in the PHT1 (swap status mainly)
;;; Clobbers M-A, M-B, M-T, A-TEM1, A-TEM3

PAGE-IN-MAKE-KNOWN
        ((m-t) a-disk-swapin-virtual-address)
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number m-t)
        ((a-disk-swapin-pht2-bits) dpb m-tem pht2-map-volatility a-disk-swapin-pht2-bits)
        (call search-page-hash-table)
;       (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)          ;Find hole in PHT for it
;      ((M-T) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        (CALL-IF-BIT-SET PHT1-VALID-BIT READ-MEMORY-DATA ILLOP) ;Supposed to be a hole!
        ((M-A) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        ((WRITE-MEMORY-DATA-START-WRITE)  ;Construct and store PHT1 word
            SELECTIVE-DEPOSIT M-A PHT1-VIRTUAL-PAGE-NUMBER A-PAGE-IN-PHT1)
        (ILLOP-IF-PAGE-FAULT)                   ;Should be wired
        ((M-PGF-TEM) A-DISK-SWAPIN-PAGE-FRAME)
        ((WRITE-MEMORY-DATA) SELECTIVE-DEPOSIT M-PGF-TEM
                PHT2-PHYSICAL-PAGE-NUMBER       ;Restore access, status, and meta bits
                A-DISK-SWAPIN-PHT2-BITS)
        (DISPATCH (LISP-BYTE %%PHT2-MAP-STATUS-CODE) MD D-SWAPAR) ;Verify the bits
                ;; This will go to ILLOP if this is a page of a free region
        ((VMA-START-WRITE) ADD VMA (A-CONSTANT 1))      ;Store PHT2
        (ILLOP-IF-PAGE-FAULT)                           ;Should be wired
;error check
  ((md) pht2-physical-page-number md)
  (call-equal md a-zero illop)
        ((WRITE-MEMORY-DATA) M-A-1 VMA A-V-PAGE-TABLE-AREA)     ;0,,Index in PHT
        ((VMA) A-DISK-SWAPIN-PAGE-FRAME)
  ;error check for clobbering PPD entry 0
  (call-equal vma a-zero illop)
        (POPJ-AFTER-NEXT
         (VMA-START-WRITE) ADD VMA A-V-PHYSICAL-PAGE-DATA)
       (ILLOP-IF-PAGE-FAULT)

(LOCALITY D-MEM)
(START-DISPATCH 3 0)                            ;DISPATCH ON MAP-STATUS
D-SWAPAR                                        ;VERIFY MAP STATUS CODE FROM CORE
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;0 MAP NOT SET UP ERRONEOUS
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;1 META BITS ONLY ERRONEOUS
        (P-BIT R-BIT)                           ;2 READ ONLY
        (P-BIT R-BIT)                           ;3 READ WRITE FIRST
        (P-BIT R-BIT)                           ;4 READ WRITE
        (P-BIT R-BIT)                           ;5 PDL BUFFER
        (P-BIT R-BIT)                           ;6 MAR BREAK
        (P-BIT r-bit)                           ;7 nubus physical in PHT2
(END-DISPATCH)
(LOCALITY I-MEM)

;INITIALIZE A FRESH PAGE BY FILLING IT WITH <DTP-TRAP .>
; Virtual adr in A-DISK-SWAPIN-VIRTUAL-ADDRESS (no type bits), M-B/ PAGE FRAME NUMBER
CZRR    ((MD) A-MAP-SCRATCH-BLOCK)
        (CALL-XCT-NEXT LOAD-L2-MAP-FROM-CADR-PHYSICAL)
       ((M-LAM) DPB M-B pht2-PHYSICAL-PAGE-NUMBER
                (A-CONSTANT (BYTE-VALUE pht2-MAP-ACCESS-CODE 3)))   ;R/W
        ((VMA) A-MAP-SCRATCH-BLOCK)     ;COMPUTE PAGE BASE ADDRESS
        ((M-TEM1) SELECTIVE-DEPOSIT M-ZERO VMA-LOW-BITS A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        ((M-LAM) A-ZERO)
CZRR1   ((WRITE-MEMORY-DATA-START-WRITE)        ;STORE TRAPS POINTING TO SELF
                ADD M-LAM A-TEM1)               ;NOTE DTP-TRAP = 0
        (ILLOP-IF-PAGE-FAULT)
        ((VMA) ADD VMA (A-CONSTANT 1))
        (JUMP-LESS-THAN-XCT-NEXT M-LAM (A-CONSTANT 377) CZRR1)
       ((M-LAM) ADD M-LAM (A-CONSTANT 1))
        ((A-FRESH-PAGE-COUNT) ADD M-ZERO A-FRESH-PAGE-COUNT ALU-CARRY-IN-ONE)
        (JUMP-XCT-NEXT SWAPIN2)                 ;RETURN TO MAIN SWAP-IN CODE
       ((VMA) A-V-NIL)

#-lambda(begin-comment)
store-scan-for-hexadec  ;hexadec in a-hexadec-required-alignment
        ((m-tem) a-hexadec-required-alignment)
        ((oa-reg-low) dpb m-tem oal-a-dest-4-bits (a-constant (byte-value oal-a-dest 1740)))
        ((a-garbage) a-findcore-scan-pointer)
        (popj-after-next
         (oa-reg-low) dpb m-tem oal-a-dest-4-bits (a-constant (byte-value oal-a-dest 1760)))
       ((a-garbage) a-aging-scan-pointer)

load-scan-for-hexadec   ;hexadec in a-hexadec-required-alignment
        ((m-tem) a-hexadec-required-alignment)
        ((oa-reg-high) dpb m-tem oah-a-src-4-bits (a-constant (byte-value oah-a-src 1740)))
        ((a-findcore-scan-pointer) seta a-garbage)
        (popj-after-next
         (oa-reg-high) dpb m-tem oah-a-src-4-bits (a-constant (byte-value oah-a-src 1760)))
       ((a-aging-scan-pointer) seta a-garbage)
#-lambda(end-comment)

;Ager. Called from DISK-SWAP-HANDLER, may clobber M-1, A-TEM1, A-TEM2, A-TEM3, M-TEM.
;Must be called with A-AGING-SCAN-POINTER in M-1.
;This advances A-AGING-SCAN-POINTER through main memory until it catches up
;to A-FINDCORE-SCAN-POINTER, skipping over the page which is being read in now.
;If a page is found with normal swap-status, it is changed to age trap.
;If a page is found with age-trap status, it is changed to flushable.
AGER    ((A-AGING-SCAN-POINTER) A-FINDCORE-SCAN-POINTER)        ;Will advance to here
AGER0   ((M-1) ADD M-1 (A-CONSTANT 1))
        (CALL-GREATER-OR-EQUAL M-1 A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH AGER1)  ;If wrap around
        ((VMA-START-READ) ADD M-1 A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)
        (POPJ-EQUAL M-1 A-FINDCORE-SCAN-POINTER)        ;Return if caught up, skipping this one
        ((M-TEM) (BYTE-FIELD 20 0) READ-MEMORY-DATA)    ;PHT entry index
        (JUMP-EQUAL M-TEM (A-CONSTANT 177777) AGER0)    ;No page here
        ((VMA-START-READ) ADD M-TEM A-V-PAGE-TABLE-AREA)
        (ILLOP-IF-PAGE-FAULT)
        (DISPATCH PHT1-SWAP-STATUS-CODE READ-MEMORY-DATA D-AGER)

AGER1   (declare (values a-1))
        (POPJ-AFTER-NEXT (M-1) A-ZERO)                  ;Wrap around to page zero
       (no-op)

(LOCALITY D-MEM)
(START-DISPATCH 3 INHIBIT-XCT-NEXT-BIT)
D-AGER  (AGER0)         ;0 PHT ENTRY INVALID, IGNORE
        (AGER2)         ;1 NORMAL, SET AGE TRAP
        (AGER0)         ;2 FLUSHABLE, IGNORE
        (AGER0)         ;3 PREPAGED, IGNORE
        (AGER3)         ;4 AGE TRAP, CHANGE TO FLUSHABLE IF AGED ENOUGH
        (AGER0)         ;5 WIRED, IGNORE
        (P-BIT ILLOP)   ;6 NOT USED, ERROR
        (P-BIT ILLOP)   ;7 NOT USED, ERROR
(END-DISPATCH)
(LOCALITY I-MEM)

;CHANGE NORMAL TO AGE-TRAP, ALSO TURN OFF HARDWARE MAP ACCESS, SET AGE TO 0
AGER2   ((A-PAGE-AGE-COUNT) ADD M-ZERO A-PAGE-AGE-COUNT ALU-CARRY-IN-ONE)
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT READ-MEMORY-DATA
            PHT1-ALL-BUT-AGE-AND-SWAP-STATUS-CODE
            (A-CONSTANT (EVAL %PHT-SWAP-STATUS-AGE-TRAP)))
        (ILLOP-IF-PAGE-FAULT)
        ((md) md)
         (JUMP-XCT-NEXT AGER0)
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control) (a-constant 0))       ;FLUSH 2ND LVL MAP, IF ANY
                ;; AGER0 will put good data in VMA.

;CHANGE AGE-TRAP TO FLUSHABLE IF HAS BEEN AGED ENOUGH
AGER3   ((M-TEM) PHT1-AGE READ-MEMORY-DATA)
        (JUMP-GREATER-OR-EQUAL M-TEM A-AGING-DEPTH AGER4)       ;AGED ENOUGH
        ((WRITE-MEMORY-DATA-START-WRITE) ADD READ-MEMORY-DATA   ;AGE MORE BEFORE MAKING
                (A-CONSTANT (BYTE-VALUE PHT1-AGE 1)))           ; FLUSHABLE
        (ILLOP-IF-PAGE-FAULT)
        (JUMP AGER0)

AGER4   ((A-PAGE-FLUSH-COUNT) ADD M-ZERO A-PAGE-FLUSH-COUNT ALU-CARRY-IN-ONE)
        ((WRITE-MEMORY-DATA-START-WRITE) SELECTIVE-DEPOSIT READ-MEMORY-DATA
            PHT1-ALL-BUT-SWAP-STATUS-CODE (A-CONSTANT (EVAL %PHT-SWAP-STATUS-FLUSHABLE)))
        (ILLOP-IF-PAGE-FAULT)
        (JUMP AGER0)

;GIVEN AN ADDRESS FIND WHAT AREA IT IS IN.  RETURNS THE AREA NUMBER OR NIL.
;THIS WORKS BY FINDING THE REGION NUMBER, THEN FINDING WHAT AREA THAT REGION LIES IN.
XARN (MISC-INST-ENTRY %AREA-NUMBER)
        (CALL XRGN)                     ;GET REGION NUMBER FROM ARG ON PDL
        (POPJ-EQUAL M-T A-V-NIL)        ;NONE
;GIVEN A REGION NUMBER IN M-T, FIND THE AREA-NUMBER (IN M-T WITH DATA-TYPE)
REGION-TO-AREA
        ((vma-start-read) add m-t a-v-region-area-map)
        (check-page-read)
        (popj-after-next no-op)
       ((m-t) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))

; Winning REGION-AREA-MAP superseded this bagbiting code.
;       ((VMA-START-READ) ADD M-T A-V-REGION-LIST-THREAD)
;       (CHECK-PAGE-READ)
;       (JUMP-IF-BIT-CLEAR-XCT-NEXT BOXED-SIGN-BIT READ-MEMORY-DATA REGION-TO-AREA)
;       ((M-T) BOXED-NUM-EXCEPT-SIGN-BIT READ-MEMORY-DATA       ;GET NEXT IN LIST
;               (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       (POPJ)                          ;END OF LIST, M-T HAS AREA NUMBER

;GIVEN AN ADDRESS FIND WHAT REGION IT IS IN.  RETURNS THE REGION NUMBER OR NIL
;IF NOT IN ANY REGION, IN M-T.  RETURNS (OR TAKES AT XRGN1) THE POINTER IN M-A.
;MUST CLOBBER ONLY M-T, M-TEM, Q-R, A-TEM1, A-TEM2, A-TEM3, M-A
;SINCE IT IS CALLED BY THE PAGE FAULT ROUTINES.
XRGN    (declare (values a-t) (clobbers a-tem a-a))
     (MISC-INST-ENTRY %REGION-NUMBER)
        ((M-A) Q-POINTER C-PDL-BUFFER-POINTER-POP       ;An address in the region
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
  ;** datatype of M-A really should be DTP-LOCATIVE, since it is a pointer.
XRGN1   ;; Get word from ADDRESS-SPACE-MAP.
        (declare (args a-a) (values a-t) (clobbers a-tem))
        ((vma) address-space-map-word-index-byte m-a)
        ((vma-start-read) add vma a-v-address-space-map)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) ADDRESS-SPACE-MAP-BYTE-NUMBER-BYTE M-A)        ;Byte number in that word
        ((M-TEM) DPB M-TEM ADDRESS-SPACE-MAP-BYTE-MROT A-ZERO)
#+lambda((OA-REG-LOW) SUB (M-CONSTANT 40) A-TEM)        ;40 doesn't hurt here, IORed in
#+exp   ((oa-reg-low) add m-tem (a-constant 1_16.)) ;rotate right
       ((M-T) (BYTE-FIELD (EVAL %ADDRESS-SPACE-MAP-BYTE-SIZE) 0) READ-MEMORY-DATA
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (POPJ-NOT-EQUAL M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
     ;Low areas do not start on proper quantuum boundaries, so above code does not work for them.
     ; Scan region origins, etc to get answer if pointer is below FIRST-UNFIXED-AREA.  Note
     ; INITIAL-LIST-AREA is below FIRST-UNFIXED-AREA.
        ;; 0 in table, is either free space or fixed area
        (JUMP-GREATER-OR-EQUAL M-A A-V-FIRST-UNFIXED-AREA XFALSE)       ;Free space
     ;; Search table of area origins.  I guess linear search is fast enough.
     ;; Note: each entry in this table is a fixnum.
        ((M-T) (A-CONSTANT (A-MEM-LOC A-V-INIT-LIST-AREA)))
XRGN2
     ;; This cannot happen under any circumstances.  A-V-RESIDENT-SYMBOL-AREA is fixnum
     ;; zero -- if it were less than that the test above would have indicated it as free.
     ;; (call-less-than m-t (a-constant (a-mem-loc a-v-resident-symbol-area)) illop)
        ((OA-REG-HIGH) DPB M-T OAH-A-SRC A-ZERO)
       (JUMP-LESS-THAN-XCT-NEXT M-A A-GARBAGE XRGN2)
      ((M-T) SUB M-T (A-CONSTANT 1))
        (POPJ-AFTER-NEXT (M-T) SUB M-T
                (A-CONSTANT (DIFFERENCE (A-MEM-LOC A-V-RESIDENT-SYMBOL-AREA) 1)))
       ((M-T) Q-POINTER M-T (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;;; MISCELLANEOUS FUNCTIONS FOR LISP PROGRAMS TO HACK THE PAGE HASH TABLE

XCPGS (MISC-INST-ENTRY %CHANGE-PAGE-STATUS)
        ;ARGS ARE VIRTUAL ADDRESS, SWAP STATUS CODE, ACCESS STATUS AND META BITS
        ;DOESN'T DO ERROR CHECKING, IF YOU DO THE WRONG THING YOU WILL LOSE.
        ((M-E) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Access, status, and meta bits
        ((M-D) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Swap status code
;Here from UPDATE-REGION-PHT.  Must bash only M-A, M-B, M-T, tems.
;Returns address which came in on pdl, in MD.
;;; Bit 24. means disconnect virtual page completely, for %GC-FREE-REGION.
;;; Bit 23. means clear modified bit, for CLEAN-DIRTY-PAGES.
XCPGS0  (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)
       ((M-T) Q-POINTER C-PDL-BUFFER-POINTER)   ;Virtual address
        (JUMP-IF-BIT-CLEAR-XCT-NEXT             ;If not swapped in, return NIL, and make
                PHT1-VALID-BIT READ-MEMORY-DATA XCPGS2) ; sure to clear the map
       ((M-T) A-V-NIL)
        ((M-T) A-V-TRUE)                        ;Get ready to return T
        (JUMP-EQUAL M-D A-V-NIL XCPGS1)         ;See if should change swap-status
        (jump-if-bit-set (byte-field 1 23.) m-d xcpgs4)   ;other special kludge, clear modified
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 24.) M-D XCPGS3) ;If sign bit of M-D set,
        ((M-TEM2) ANDCA MD (A-CONSTANT (BYTE-MASK PHT1-MODIFIED-BIT))) ;clear modified flag
        ((MD) DPB (M-CONSTANT -1) PHT1-VIRTUAL-PAGE-NUMBER A-TEM2) ;and forget virtual page
XCPGS3  ((WRITE-MEMORY-DATA-START-WRITE)
                SELECTIVE-DEPOSIT MD PHT1-ALL-BUT-SWAP-STATUS-CODE A-D)  ;pht1-swap-status is low 3 bits.
        (ILLOP-IF-PAGE-FAULT)
XCPGS1  (JUMP-EQUAL M-E A-V-NIL XCPGS2)
        ((m-tem3) (byte-field (difference q-pointer-width 2) 2) m-e)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM2) READ-MEMORY-DATA)
        ((WRITE-MEMORY-DATA-START-WRITE)
           DPB m-tem3 PHT2-ACCESS-STATUS-AND-META-BITS-except-volatility A-TEM2)
        (ILLOP-IF-PAGE-FAULT)
XCPGS2  ((MD) C-PDL-BUFFER-POINTER-POP)         ;ADDRESS LOCATION BEING HACKED
        (no-op)         ;supposedly necessary only on exp, try it on lambda to be sure.
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control) (a-constant 0))       ;FLUSH 2ND LVL MAP, IF ANY
        ((VMA) A-V-NIL)                         ;INSTRUCTIONS MUST LEAVE VMA NON-GARBAGE
        (POPJ)        ;On popj cycle, MUSTN'T MAP-WRITE or affect VMA.

xcpgs4  (jump-xct-next xcpgs3)
       ((md) andca md (A-CONSTANT (BYTE-MASK PHT1-MODIFIED-BIT)))

XCPPG (MISC-INST-ENTRY %CREATE-PHYSICAL-PAGE)
 ;      (CALL LOAD-PHYSICAL-MEMORY-SETUP-FROM-SYS-COM)
        ;ARG IS PHYSICAL ADDRESS
        ((VMA-START-READ) A-V-PAGE-TABLE-AREA)          ;FIND FIRST HOLE
        ;;; search PHT until we find an entry that is not valid
XCPPG0  (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) SUB VMA A-V-PAGE-TABLE-AREA)
        (CALL-GREATER-OR-EQUAL M-TEM A-PHT-INDEX-LIMIT ILLOP)   ;OUT OF BOUNDS
        (JUMP-IF-BIT-SET-XCT-NEXT PHT1-VALID-BIT READ-MEMORY-DATA XCPPG0)
       ((VMA-START-READ) ADD VMA (A-CONSTANT 2))
        (NO-OP)                                         ;USELESS MEM CYCLE
        ((VMA) SUB VMA (A-CONSTANT 2))                  ;ADDRESS PHT1 OF HOLE
;Enter here from COLD-REINIT-PHT.  May smash only M-T.
XCPPG1  ((WRITE-MEMORY-DATA-START-WRITE) DPB (M-CONSTANT -1)    ;FAKE VIRTUAL ADDRESS
                PHT1-VIRTUAL-PAGE-NUMBER
                (A-CONSTANT (PLUS (BYTE-VALUE PHT1-SWAP-STATUS-CODE 2) ;FLUSHABLE
                                        (BYTE-VALUE PHT1-VALID-BIT 1))))
        (ILLOP-IF-PAGE-FAULT)
        ((M-T) VMA-PHYS-PAGE-ADDR-PART C-PDL-BUFFER-POINTER-POP);PAGE FRAME NUMBER
        ((WRITE-MEMORY-DATA) SUB VMA A-V-PAGE-TABLE-AREA)       ;0,,PHT INDEX
        ((VMA-START-WRITE) ADD M-T A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)
        (JUMP-LESS-THAN M-T A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH
                                 XCPPG2)        ;See if table getting bigger
        (CALL-GREATER-OR-EQUAL VMA A-V-ADDRESS-SPACE-MAP ILLOP) ;Bigger than space allocated
        ((A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH) ADD M-T (A-CONSTANT 1))
XCPPG2  ((VMA) M+A+1 MD A-V-PAGE-TABLE-AREA)            ;Address PHT2
        ((WRITE-MEMORY-DATA-START-WRITE) IOR M-T
                (A-CONSTANT (BYTE-VALUE PHT2-ACCESS-STATUS-AND-META-BITS 1200))) ;RO
        (ILLOP-IF-PAGE-FAULT)
        (JUMP XTRUE)

XDPPG (MISC-INST-ENTRY %DELETE-PHYSICAL-PAGE)
 ;      (CALL LOAD-PHYSICAL-MEMORY-SETUP-FROM-SYS-COM)
        ;ARG is physical address
        ((M-B) VMA-PHYS-PAGE-ADDR-PART C-PDL-BUFFER-POINTER-POP);Page frame number
        (CALL-GREATER-OR-EQUAL M-B A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH ILLOP)   ;PFN too big
        ((VMA-START-READ) ADD M-B A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) (BYTE-FIELD 20 0) READ-MEMORY-DATA)    ;PHT entry index
        (JUMP-EQUAL M-TEM (A-CONSTANT 177777) XFALSE)   ;Already deleted or wired
        ((VMA-START-READ M-T) ADD M-TEM A-V-PAGE-TABLE-AREA)
        (ILLOP-IF-PAGE-FAULT)
        (CALL COREFOUND3)                               ;Swap it out, delete PHT entry
XDPPG1  (POPJ-AFTER-NEXT (M-T) A-V-TRUE)        ;Done, return T
       ((VMA) A-V-NIL)                          ;INSTRUCTIONS MUST LEAVE VMA NON-GARBAGE

(locality a-mem)
a-quantum-being-mapped   (0)
a-virtual-page-to-un-map (0)                    ;really a virtual address
a-pq1-tem                (0)
a-pq2-tem                (0)
(locality i-mem)

;;; args are quantum number, nubus address (quantum aligned?) size and l2-control
;;; (defmic %map-device-quantum 1164 (quantum-number nubus-page nubus-words l2-control) t)
xmap-device-quantum (misc-inst-entry %map-device-quantum)
        ((m-tem) dpb m-minus-one (lisp-byte %%pq1-quantum-is-valid) a-zero)
        ((m-tem) dpb m-minus-one (lisp-byte %%pq1-quantum-is-device) a-tem)
        ((m-tem1) dpb pdl-pop (lisp-byte %%pq2d-quantum-l2mc-except-meta-bits) a-zero)  ;l2-control
        ((a-pq1-tem) dpb pdl-pop (lisp-byte %%pq1d-quantum-nubus-words) a-tem)  ;nubus-words
        ((a-pq2-tem) dpb pdl-pop (lisp-byte %%pq2d-quantum-l2mpp) a-tem1)       ;nubus-address
        ((a-quantum-being-mapped) q-pointer pdl-pop)    ;quantum-number
        (call delete-quantum-pages)
        ;;; now fill in the quantum map entry
        ((md) a-pq1-tem)
        ((vma) a-quantum-being-mapped)
        ((vma) dpb vma (byte-field 31. 1) a-zero)       ;double it
        ((vma) add vma a-v-quantum-map)
        ((vma-start-write) add vma (a-constant (eval (* page-size %quantum-map-offset-in-tables))))
        (illop-if-page-fault)
     (error-table crash "page fault in wired area")
        ((md) a-pq2-tem)
        ((vma-start-write) m+a+1 vma a-zero)
        (illop-if-page-fault)
     (error-table crash "page fault in wired area")
        (popj-after-next (m-t) a-v-true)
       (no-op)

;;; for quantum whos number is in A-QUANTUM-BEING-MAPPED, make the paging system forget about the pages of that quantum
;;; for each virtual page of the quantum, if it has a physical page associated with it then delete
;;; that physical page.  then create that physical page so the memory system can reuse it.
delete-quantum-pages
        ((m-tem) a-quantum-being-mapped)
        ((a-virtual-page-to-un-map) dpb m-tem vma-quantum-byte a-zero)
delete-quantum-pages-loop
        (call-xct-next search-page-hash-table)
       ((m-t) a-virtual-page-to-un-map)
        (jump-if-bit-clear pht1-valid-bit md delete-quantum-pages-continue)
        ;;; we found one
        ((vma-start-read) m+a+1 vma a-zero)     ;read physical page
        (illop-if-page-fault)
     (error-table crash "page fault in wired area")
        ((m-tem) ldb (lisp-byte %%PHT2-PHYSICAL-PAGE-NUMBER) md)
        ((m-tem) dpb m-tem q-pointer
                (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) m-tem)                      ;argument to xdppg
        ((pdl-push) m-tem)                      ;argument to xcppg
        (call xdppg)                            ;delete the physical page
        (call xcppg)                            ;re-create the physical page
delete-quantum-pages-continue
        ((m-tem) a-virtual-page-to-un-map)
        ((a-virtual-page-to-un-map) add m-tem (a-constant (eval page-size)))
        ((m-tem) dpb m-zero vma-quantum-byte a-virtual-page-to-un-map)
        (jump-not-equal m-tem a-zero delete-quantum-pages-loop) ;past quantum boundary when bits
                                                ;below quantum number wrap to zero
        (popj)

XPAGE-IN (MISC-INST-ENTRY %PAGE-IN)
        ((A-DISK-SWAPIN-VIRTUAL-ADDRESS) DPB    ;ARG 2 - VIRTUAL PAGE NUMBER
                 C-PDL-BUFFER-POINTER-POP VMA-PAGE-ADDR-PART A-ZERO)
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)          ;SEE IF ALREADY IN
       ((M-T) A-DISK-SWAPIN-VIRTUAL-ADDRESS)
        (JUMP-IF-BIT-SET-XCT-NEXT PHT1-VALID-BIT READ-MEMORY-DATA XFALSE) ;YES, RETURN NIL
       ((A-DISK-SWAPIN-PAGE-FRAME) Q-POINTER C-PDL-BUFFER-POINTER-POP)  ;ARG 1 - PAGE FRAME
        (CALL PAGE-IN-GET-MAP-BITS)                     ;NO, PUT IT IN
        (CALL-XCT-NEXT PAGE-IN-MAKE-KNOWN)
       ((A-PAGE-IN-PHT1) (A-CONSTANT (PLUS (BYTE-VALUE PHT1-VALID-BIT 1)
                                           (BYTE-VALUE PHT1-SWAP-STATUS-CODE 1))))
        (JUMP XDPPG1)                                   ;RETURN T, FIX VMA


;NIL if not swapped in, else PHT1 value, except:
;the modified bit in our value is always up to date,
;even though that in the PHT1 is not.
XPGSTS (MISC-INST-ENTRY %PAGE-STATUS)
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)
       ((M-T) C-PDL-BUFFER-POINTER-POP)
        (JUMP-IF-BIT-CLEAR PHT1-VALID-BIT MD XFALSE)
        (POPJ-IF-BIT-SET-XCT-NEXT PHT1-MODIFIED-BIT MD)
       ((M-T) DPB MD Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;If modified bit is set in PHT1, that must be accurate, so return the PHT1.
;Otherwise must check the PHT2 to know for sure.
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;Get PHT2
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) PHT2-MAP-STATUS-CODE READ-MEMORY-DATA)
        (POPJ-AFTER-NEXT POPJ-LESS M-TEM
                                (A-CONSTANT (EVAL %PHT-MAP-STATUS-READ-WRITE)))
;If PHT2 implies page is modified, return value with modified-bit set.
       ((M-T) DPB (M-CONSTANT -1) PHT1-MODIFIED-BIT A-T)

XPHYADR (MISC-INST-ENTRY %PHYSICAL-ADDRESS)
;       ((VMA-START-READ) C-PDL-BUFFER-POINTER-POP)     ;ADDRESS THE MAP
;       (CHECK-PAGE-READ-NO-INTERRUPT)          ;BE SURE INTERRUPT DOESN'T DISTURB MAP
;       ((MD) VMA)                              ;ADDRESS MAP (DELAYS UNTIL READ CYCLE OVER)
;       (POPJ-AFTER-NEXT (M-T) DPB l2-map-physical-page
;                VMA-PHYS-PAGE-ADDR-PART (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
;       ((M-T) VMA-LOW-BITS MD A-T)

        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)
       ((M-T) M-C)
        (JUMP-IF-BIT-CLEAR PHT1-VALID-BIT MD XFALSE)
        ((VMA-START-READ) ADD VMA (A-CONSTANT 1))       ;Get PHT2
        (ILLOP-IF-PAGE-FAULT)
        (POPJ-AFTER-NEXT (M-T) DPB MD
                VMA-PHYS-PAGE-ADDR-PART (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((M-T) VMA-LOW-BITS M-C A-T)


;PDL-BUFFER LOADING CONVENTIONS:
;   1. THE CURRENT RUNNING FRAME IS ALWAYS COMPLETELY CONTAINED WITHIN THE PDL-BUFFER.
;   2. SO IS ITS CALLING ADI (LOCATED IMMEDIATELY BEFORE IT ON PDL).
;   3. POINTERS ASSOCIATED WITH ADI (SUCH AS MULTIPLE VALUE STORING POINTERS
;       AND INDIRECT-ADI POINTERS) MAY POINT AT REGIONS OF THE PDL WHICH
;       ARE NOT CONTAINED WITHIN THE PDL-BUFFER.

;CHECKING TO SEE IF PDL-BUFFER NEEDS TO BE REFILLED:
;   SINCE M-AP CHANGES MUCH LESS FREQUENTLY THAN THE PDL-BUFFER POINTER ITSELF,
; ALL TESTING FOR PDL-BUFFER DUMPING/REFILLING IS DONE WITH REFERENCE TO M-AP.
; AS A RESULT, 400 (OCTAL) WORDS (THE MAXIMUM FRAME SIZE) EXTRA SLOP MUST BE LEFT.
; M-PDL-BUFFER-ACTIVE-QS CONTAINS THE NUMBER OF QS BETWEEN A-PDL-BUFFER-HEAD
; AND M-AP (MOMENTARILY, IT MAY BE NEGATIVE).
; WHENEVER M-AP IS CHANGED, M-PDL-BUFFER-ACTIVE-QS MUST LIKEWISE BE ADJUSTED.
;    CLEARLY, M-PDL-BUFFER-ACTIVE-QS MUST BE AT LEAST 4 FOR ANY CODE TO BE RUNNABLE.
; IN ADDITION, THE ADI OF THE RUNNING FRAME, IF ANY, MUST ALSO BE IN THE PDL-BUFFER.
; IF M-PDL-BUFFER-ACTIVE-QS IS GREATER THAN THE CONSTANT PDL-BUFFER-LOW-WARNING
; (SET TO 4 + MAX LENGTH OF ADI), IT MAY SAFELY BE ASSUMED THAT THE ADI, IF ANY,
; IS IN.
;    WHENEVER M-AP IS ADJUSTED DOWNWARD (POPPED), M-AP SHOULD BE ADJUSTED BEFORE
; M-PDL-BUFFER-ACTIVE-QS TESTED, SO THAT M-AP IS AT ITS NEW VALUE IF AND WHEN
; PDL-BUFFER-REFILL IS REACHED.

;ROUTINE TO UNLOAD PDL-BUFFER INTO MAIN MEMORY, MAKING AT LEAST N WDS
; OF ROOM IN PDL BUFFER.  GENERAL IDEA IS START AT PDL-BUFFER INDEX A-PDL-BUFFER-HEAD
; AND VIRTUAL ADDRESS A-PDL-BUFFER-VIRTUAL-ADDRESS, WRITING OUT CRUFT AND INCREMENTING
; BOTH POINTERS.  ONE OPTIMIZATION IS WE FIDDLE MAP TO AVOID GOING THRU
; PAGE FAULT HANDLER ON EVERY CYCLE (WHICH WOULDNT QUITE WORK ANYWAY SINCE IT
; WOULD WRITE THE STUFF BACK IN THE PDL-BUFFER).  THUS, WE HAVE TO KEEP TRACK OF
; WHICH MAP PAGE WE HAVE HACKED AND PUT IT BACK AT END.  ALSO, CHECK IF MOVING TO A
; NEW PAGE, ETC.

PDL-BUFFER-DUMP-RESET-FLAGS
;; Special entrypoint for QMRCL and MMCAL4, which, for convenience and speed, require
;; M-FLAGS to be cleared.
        ((M-FLAGS) SELECTIVE-DEPOSIT M-FLAGS M-FLAGS-EXCEPT-PROCESSOR-FLAGS A-ZERO)
PDL-BUFFER-DUMP
        ((M-2) (A-CONSTANT PDL-BUFFER-HIGH-LIMIT))
        (CALL-NOT-EQUAL M-2 A-PDL-BUFFER-HIGH-WARNING TRAP)     ;PUSH-DOWN CAPACITY EXCEEDED
   (ERROR-TABLE PDL-OVERFLOW REGULAR)   ;I.E. ALREADY NEAR END, THERE IS PROBABLY JUST
                                        ;ENOUGH SPACE LEFT TO DUMP WHAT'S IN THE PDL BUFFER NOW
                        ;HERE I AM ASSUMING THAT A-PDL-BUFFER-HIGH-WARNING IS GUARANTEED
                        ;NOT TO COME OUT NEGATIVE AFTER PDL-BUFFER-MAKE-ROOM RETURNS,
                        ;BECAUSE OF THE CHECK ABOVE.  THIS USED TO BE CHECKED.
;ARG IN M-2 -> HIGHEST "SATISFACTORY" VALUE FOR M-PDL-BUFFER-ACTIVE-QS.
; COMMON VALUES ARE PDL-BUFFER-HIGH-LIMIT TO UNBLOAT PDL-BUFFER OR
;   0 TO COMPLETELY DUMP PDL-BUFFER (THRU M-AP) OR
; - (PP - M-AP) [MINUS SIZE OF ACTIVE FRAME] TO REALLY COMPLETELY DUMP PDL-BUFFER

;;; Clobbers M-1.

#-lambda(begin-comment)
PDL-BUFFER-MAKE-ROOM                    ;ARG IN M-2
        ((A-PDLB-TEM) PDL-BUFFER-INDEX) ;PRESERVE..
P-B-MR0 (JUMP-LESS-OR-EQUAL M-PDL-BUFFER-ACTIVE-QS A-2 P-B-X1)  ;If nothing to do, done
        ((PDL-BUFFER-INDEX) A-PDL-BUFFER-HEAD)  ;Starting pdl-buffer address
        ((VMA m-lam) A-PDL-BUFFER-VIRTUAL-ADDRESS)      ;Starting virtual-memory address
        ((M-TEM) SUB M-PDL-BUFFER-ACTIVE-QS A-2)        ;Number locations to do total
        ((A-PDL-FINAL-VMA) ADD m-lam A-TEM)
P-B-MR1
        ((WRITE-MEMORY-DATA-START-WRITE-FORCE) C-PDL-BUFFER-INDEX) ;Write next Q into memory
        (JUMP-IF-PAGE-FAULT P-B-MR-PF)

;Interpreter now puts forwards into stack frames.  So can lexical scoping in compiled code.
;       (DISPATCH Q-DATA-TYPE WRITE-MEMORY-DATA D-ILLOP-IF-BAD-DATA-TYPE)
                                                ;Error-check stuff being written
        (GC-WRITE-TEST (I-ARG 1))               ;Check for writing ptr to extra-pdl
                                                ;If traps, will clean up & return to P-B-MR0
   ;unfortunately, various paths in gc-write-test can clobber M-LAM, so we have to depend on VMA.
        ((VMA m-lam) ADD VMA (A-CONSTANT 1))            ;Close loop
        (JUMP-LESS-THAN-XCT-NEXT m-lam A-PDL-FINAL-VMA P-B-MR1)
       ((PDL-BUFFER-INDEX) ADD PDL-BUFFER-INDEX (A-CONSTANT 1))
        ((M-TEM) SUB m-lam A-PDL-BUFFER-VIRTUAL-ADDRESS)        ;Number of locations dumped
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) m-lam)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
   ;drops-thru
#-lambda(end-comment)

#-exp(BEGIN-COMMENT)
PDL-BUFFER-MAKE-ROOM                    ;ARG IN M-2
        ((A-PDLB-TEM) PDL-BUFFER-INDEX) ;PRESERVE..
P-B-MR0 (JUMP-LESS-OR-EQUAL M-PDL-BUFFER-ACTIVE-QS A-2 P-B-X1)  ;If nothing to do, done
        ((VMA-START-READ) A-PDL-BUFFER-VIRTUAL-ADDRESS) ;Take a read cycle to
        (CHECK-PAGE-READ-NO-INTERRUPT)                  ;make sure 2nd lvl map set up, etc
                ;Note a reference is guaranteed to set up 2nd level map
                ;even if it turns out to be in the pdl-buffer and no main memory
                ;cycle is made.
  ;*** bug! this is present in CADR too.  Q-R could get clobbered by extra pdl trap!!
  ; gee, maybe not, but boy is it marginal..
        ((MD Q-R) VMA)                          ;Address the map, Q-R saves addr
        (no-op)         ;allow time
        ((M-1) l2-map-control)  ;Save correct map contents
        ((vma-write-L2-MAP-CONTROL) IOR M-1             ;Turn on access
                (A-CONSTANT (BYTE-VALUE MAP2C-ACCESS-CODE 3))) ;R/W
        ((M-TEM) DPB (M-CONSTANT -1) ALL-BUT-VMA-LOW-BITS A-PDL-BUFFER-VIRTUAL-ADDRESS)
        ((A-PDL-FINAL-VMA) SUB M-ZERO A-TEM)    ;Number locations left in page
        ((M-TEM) SUB M-PDL-BUFFER-ACTIVE-QS A-2)        ;Number locations to do total
        (JUMP-GREATER-OR-EQUAL M-TEM A-PDL-FINAL-VMA P-B-MR3)
        ((A-PDL-FINAL-VMA) M-TEM)               ;Don't do a full page
P-B-MR3 ((PDL-BUFFER-INDEX) A-PDL-BUFFER-HEAD)  ;Starting pdl-buffer address
        ((VMA m-lam) A-PDL-BUFFER-VIRTUAL-ADDRESS)      ;Starting virtual-memory address
        ((A-PDL-FINAL-VMA) ADD m-lam A-PDL-FINAL-VMA)   ;Ending virtual-memory address +1
P-B-MR1 ((WRITE-MEMORY-DATA-START-WRITE) C-PDL-BUFFER-INDEX)    ;Write next Q into memory
        (ILLOP-IF-PAGE-FAULT)                   ;Write-access supposedly turned on.
;Interpreter now puts forwards into stack frames.  So can lexical scoping in compiled code.
;       (DISPATCH Q-DATA-TYPE WRITE-MEMORY-DATA D-ILLOP-IF-BAD-DATA-TYPE)
                                                ;Error-check stuff being written
        (GC-WRITE-TEST (I-ARG 1))               ;Check for writing ptr to extra-pdl
                                                ;If traps, will clean up & return to P-B-MR0
        ((VMA m-lam) ADD VMA (A-CONSTANT 1))            ;Close loop
        (JUMP-LESS-THAN-XCT-NEXT m-lam A-PDL-FINAL-VMA P-B-MR1)
       ((PDL-BUFFER-INDEX) ADD PDL-BUFFER-INDEX (A-CONSTANT 1))
;Clean up and restore the map.
        ((M-TEM) SUB m-lam A-PDL-BUFFER-VIRTUAL-ADDRESS)        ;Number of locations dumped
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) m-lam)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
        ((MD) Q-R)                              ;Address the map
        (JUMP-GREATER-THAN-XCT-NEXT             ;Loop back for next page
                M-PDL-BUFFER-ACTIVE-QS A-2 P-B-MR0)
       ((vma-write-l2-map-control) M-1)                 ;Restore the map for this page
  ;drops through
#-exp(END-COMMENT)

  ;drops in.
;Here when we're done.  This exit used by both PDL-BUFFER-MAKE-ROOM and PDL-BUFFER-REFILL.
P-B-X1  ((VMA) A-V-NIL)                         ;Don't leave VMA nil.
        ((m-2) a-pdl-buffer-virtual-address)
        (call-less-than m-2 a-qlpdlo illop)     ;not in range of PDL
        ((M-2) A-QLPDLH)                        ;Recompute A-PDL-BUFFER-HIGH-WARNING
        (call-less-or-equal m-2 a-pdl-buffer-virtual-address illop) ;not in range of PDL
        ((M-2) SUB M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS)
        ((M-2) SUB M-2 (A-CONSTANT PDL-BUFFER-SIZE-IN-WORDS))   ;Result negative if within
                                   ; pdl-buffer size of the end of the regular-pdl in virt mem
        (JUMP-LESS-THAN M-2 A-ZERO P-B-SL-1)
        (POPJ-AFTER-NEXT                        ;Enough room, allow P.B. to fill
         (A-PDL-BUFFER-HIGH-WARNING) (A-CONSTANT PDL-BUFFER-HIGH-LIMIT))
       ((PDL-BUFFER-INDEX) A-PDLB-TEM)          ;Restore

;Getting near the end of the stack.  Set A-PDL-BUFFER-HIGH-WARNING
;so that we will trap to PDL-BUFFER-DUMP before getting more stuff
;into the pdl buffer than there is room to store into virtual memory.
;Note that this result can actually be negative if we are currently
;in the process of taking a pdl-overflow trap.
P-B-SL-1(POPJ-AFTER-NEXT
         (A-PDL-BUFFER-HIGH-WARNING) ADD M-2 (A-CONSTANT PDL-BUFFER-HIGH-LIMIT))
       ((PDL-BUFFER-INDEX) A-PDLB-TEM)          ;Restore

;Attempt to refill pdl-buffer from virtual memory such that
;M-PDL-BUFFER-ACTIVE-QS is at least PDL-BUFFER-LOW-WARNING.
#-lambda(begin-comment)
PDL-BUFFER-REFILL
        ((A-PDLB-TEM) PDL-BUFFER-INDEX)         ;Preserve PI
        ((M-2) A-QLPDLO)                        ;Get base address of pdl into M memory
P-R-0   (JUMP-GREATER-OR-EQUAL M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS
                                P-R-AT-BOTTOM)  ;No more pdl to reload, exit
        (JUMP-GREATER-OR-EQUAL M-PDL-BUFFER-ACTIVE-QS
           (A-CONSTANT PDL-BUFFER-LOW-WARNING) P-R-AT-BOTTOM)   ;Enough in there to win
        ((M-TEM) SUB M-PDL-BUFFER-ACTIVE-QS     ;Negative number of words to do total
                (A-CONSTANT PDL-BUFFER-LOW-WARNING))
        ((M-1) SUB M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS)    ;Negative number of words left in pdl
        (JUMP-GREATER-OR-EQUAL M-TEM A-1 P-R-2)
        ((M-TEM) M-1)
P-R-2   ((A-PDL-LOOP-COUNT) M-A-1 M-ZERO A-TEM) ;Max number of words for those reasons (-1)
        ((VMA m-lam) A-PDL-BUFFER-VIRTUAL-ADDRESS)      ;Initial virtual-memory address +1
        ((PDL-BUFFER-INDEX) A-PDL-BUFFER-HEAD)  ;Initial P.B. address +1
P-R-1
        ((VMA-START-READ-FORCE m-lam) SUB m-lam (A-CONSTANT 1))
        (JUMP-IF-PAGE-FAULT P-R-PF)
        ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-INDEX (A-CONSTANT 1))
        (DISPATCH Q-DATA-TYPE-PLUS-ONE-BIT      ;Transport the data just read from memory
                DISPATCH-ON-MAP-19
                READ-MEMORY-DATA D-PB-TRANS)    ;Running cleanup handler first
        ((C-PDL-BUFFER-INDEX) READ-MEMORY-DATA)
        (JUMP-LESS-THAN-XCT-NEXT M-ZERO A-PDL-LOOP-COUNT P-R-1)
       ((A-PDL-LOOP-COUNT) ADD (M-CONSTANT -1) A-PDL-LOOP-COUNT)
;Now clean up
        (call-not-equal vma a-lam illop)
        ((M-TEM) SUB VMA A-PDL-BUFFER-VIRTUAL-ADDRESS)  ;Minus number of Q's moved
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)     ;Increase this
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) m-lam)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
  ;DROP THRU
#-lambda(end-comment)

#-exp(BEGIN-COMMENT)
PDL-BUFFER-REFILL
        ((A-PDLB-TEM) PDL-BUFFER-INDEX)         ;Preserve PI
        ((M-2) A-QLPDLO)                        ;Get base address of pdl into M memory
    ;this is just left in M-2 as a constant in below code.
P-R-0   (JUMP-GREATER-OR-EQUAL M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS
                                P-R-AT-BOTTOM)  ;No more pdl to reload, exit
        (JUMP-GREATER-OR-EQUAL M-PDL-BUFFER-ACTIVE-QS
           (A-CONSTANT PDL-BUFFER-LOW-WARNING) P-R-AT-BOTTOM)   ;Enough in there to win
        ((VMA-START-READ) ADD (M-CONSTANT -1) A-PDL-BUFFER-VIRTUAL-ADDRESS)
        (CHECK-PAGE-READ-NO-INTERRUPT)          ;Take cycle to assure 2nd lvl map set up
  ;bug!? Can Q-R get clobbered?? ***  well maybe its OK, but...
        ((MD Q-R) VMA)                          ;Address the map
        (no-op)         ;allow time
        ((M-PGF-TEM) l2-map-control)    ;Save correct map contents
        ((vma-write-l2-map-control) IOR M-PGF-TEM    ;Turn on access to mem which shadows pdl buf
                (A-CONSTANT (BYTE-VALUE MAP2C-ACCESS-CODE 3))) ;R/W
        ((M-TEM) SUB M-PDL-BUFFER-ACTIVE-QS     ;Negative number of words to do total
                (A-CONSTANT PDL-BUFFER-LOW-WARNING))
        ((M-1) SUB M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS)    ;Negative number of words left in pdl
        (JUMP-GREATER-OR-EQUAL M-TEM A-1 P-R-2)
        ((M-TEM) M-1)
P-R-2   ((M-TEM) SUB M-ZERO A-TEM)              ;Max number of words for those reasons
        ((A-PDL-LOOP-COUNT) VMA-LOW-BITS Q-R)   ;Number of words on this page -1
        (JUMP-GREATER-THAN M-TEM A-PDL-LOOP-COUNT P-R-3)
        ((A-PDL-LOOP-COUNT) SUB M-TEM (A-CONSTANT 1))   ;Won't be able to do full page
P-R-3   ((VMA m-lam) A-PDL-BUFFER-VIRTUAL-ADDRESS)      ;Initial virtual-memory address +1
        ((PDL-BUFFER-INDEX) A-PDL-BUFFER-HEAD)  ;Initial P.B. address +1
P-R-1   ((VMA-START-READ m-lam) SUB m-lam (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)                   ;Map should be hacked
        ((PDL-BUFFER-INDEX) SUB PDL-BUFFER-INDEX (A-CONSTANT 1))
        (DISPATCH Q-DATA-TYPE-PLUS-ONE-BIT      ;Transport the data just read from memory
                DISPATCH-ON-MAP-19
                READ-MEMORY-DATA D-PB-TRANS)    ;Running cleanup handler first
        ((C-PDL-BUFFER-INDEX) READ-MEMORY-DATA)
        (JUMP-LESS-THAN-XCT-NEXT M-ZERO A-PDL-LOOP-COUNT P-R-1)
       ((A-PDL-LOOP-COUNT) ADD (M-CONSTANT -1) A-PDL-LOOP-COUNT)
;Now clean up
        (call-not-equal vma a-lam illop)
        ((M-TEM) SUB VMA A-PDL-BUFFER-VIRTUAL-ADDRESS)  ;Minus number of Q's moved
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)     ;Increase this
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) m-lam)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
        ((MD) Q-R)                              ;Address the map
        (JUMP-LESS-THAN-XCT-NEXT                ;Loop back for next page
                M-2 A-PDL-BUFFER-VIRTUAL-ADDRESS P-R-0) ; unless at bottom of pdl
       ((vma-write-l2-map-control) M-PGF-TEM)           ;Restore the map
#-exp(END-COMMENT)
  ;DROPS IN.
P-R-AT-BOTTOM
        (JUMP-XCT-NEXT P-B-X1)
       (CALL-LESS-THAN M-PDL-BUFFER-ACTIVE-QS (A-CONSTANT 4) ILLOP)     ;Over pop

;Here if transport required while reloading pdl.  Clean up first.
;Note that the transport happens with the bottom pdl word not stored into
;yet.  This should be all right.
PB-TRANS(call-not-equal vma a-lam illop)
        ((M-TEM) SUB m-lam A-PDL-BUFFER-VIRTUAL-ADDRESS)        ;Minus number of Q's moved
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)     ;Increase this
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) m-lam)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
        ((C-PDL-BUFFER-POINTER-PUSH) A-PDLB-TEM);Save stuff momentarily
#-exp(begin-comment)
        ((C-PDL-BUFFER-POINTER-PUSH) MD)
        ((MD) VMA)                              ;Address the map
        (no-op)
        ((vma-write-l2-map-control) M-PGF-TEM)          ;Restore the map
        ((VMA) MD)                              ;Restore VMA
        ((MD) C-PDL-BUFFER-POINTER-POP)         ;Restore MD
#-exp(end-comment)
  ;This used to be just TRANSPORT.  Changed to allow EVCPs on PDL.  There is some loss of
  ;error checking (for DTP-NULL, etc) involved in this, so we may eventually want another
  ;dispatch table.
        (DISPATCH TRANSPORT-NO-EVCP-FOR-PDL-RELOAD MD)          ;Now invoke the transporter
        ((A-PDLB-TEM) C-PDL-BUFFER-POINTER-POP) ;Restore A-PDLB-TEM, lost by transporter
        ((PDL-BUFFER-INDEX) A-PDL-BUFFER-HEAD)
        (JUMP-XCT-NEXT P-R-0)                   ;Now re-start fast loop for next word
       ((C-PDL-BUFFER-INDEX) MD)                ;Put the transported datum on the pdl

P-B-MR-PF  ;TOOK PAGE FAULT DUMPING PDL BUFFER.  CLEAN UP THEN PROCESS IT.
        (call-not-equal vma a-lam illop)
        ((M-TEM) SUB m-lam A-PDL-BUFFER-VIRTUAL-ADDRESS)        ;Number of locations dumped
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) m-lam)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
        (CHECK-PAGE-WRITE-NO-INTERRUPT)
        (JUMP P-B-MR0)

P-R-PF    ;TOOK PAGE FAULT RELOADING PDL BUFFER, CLEAR UP THEN PROCESS IT.
        (call-not-equal vma a-lam illop)
        ((M-LAM) ADD m-lam (A-CONSTANT 1))     ;LAST ACTUALLY TRANSFERRED.
        ((M-TEM) SUB M-LAM A-PDL-BUFFER-VIRTUAL-ADDRESS)        ;Minus number of Q's moved
        ((M-PDL-BUFFER-ACTIVE-QS) SUB M-PDL-BUFFER-ACTIVE-QS A-TEM)     ;Increase this
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) M-LAM)
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)
        (CHECK-PAGE-READ-NO-INTERRUPT)
        (JUMP P-R-0)

;;;Merge from LAD on 9-23-86 (mrc)
;;; An example of how to use a poor mans mar.  Leave this here.

;;; jrm wrote this to find a nasty explorer bug.  The page hash table
;;; was being corrupted early in the boot.  This code checked the pht
;;; every time something was read.  It turned out that another board
;;; on the nubus was trying to post an interrupt to the middle of the pht.

;poor-mans-mar
;  ((m-lam) vma)
;  ((vma-start-read) (a-constant 500)) ;;magic location
;  (illop-if-page-fault)
;  ((vma-start-read) m-lam)
;  (jump-conditional pg-fault-or-interrupt pgf-r-i)
;  (popj)
))

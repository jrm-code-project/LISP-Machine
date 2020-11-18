;-*- Mode:LISP; Package:LAMBDA; Base:8; readtable: ZL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

(DEFCONST UC-TRANSPORTER '(
;;; THE TRANSPORTER

;               "Energize!"
;                    -- J. T. Kirk
;
;THIS CAN CALL CONS BUT CANNOT SEQUENCE-BREAK.  IT WILL NOT CLOBBER ANY REGISTERS
;EXCEPT WHAT PAGE-FAULTS CLOBBER.  IF IT NEEDS TO SEQUENCE BREAK, THE BREAK WILL
;ACTUALLY BE DEFERRED SO THAT EVERYONE WHO TRANSPORTS DOESN'T HAVE TO WORRY ABOUT
;SEQUENCE BREAKS.

;GET HERE BY SPECIAL DISPATCH, THE RETURN ADDRESS ON THE MICROSTACK
;IS THE ADDRESS OF THE DISPATCH INSTRUCTION ITSELF.
;PRESENTLY, WE HAVE ONE DISPATCH TABLE AND USE I-ARG'S TO DISTINGUISH THE
;CASES.  IF IT TURNS OUT WE OUGHT TO HAVE DROPPED THROUGH, WE RETURN TO
;THE DISPATCH INSTRUCTION, OA-MODIFYING IT TO DISPATCH THROUGH LOC 3777
;WHICH FORCES IT TO DROP THROUGH.  NORMALLY, WE EITHER ERR OUT OR ALTER
;VMA AND MD AND RETURN TO RE-EXECUTE THE DISPATCH.

;Enter here if either the MD is a pointer to old-space or we have a map miss
TRANS-OLD
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 1) READ-I-ARG TRANS-DROP-THROUGH);Ignore if no-transport
TRANS-OLD0      ;Enter here if forwarding-pointer, mustn't ever drop-through
        ((A-TRANS-VMA) VMA)                     ;Save where MD came from
        (DISPATCH L2-MAP-STATUS-CODE D-GET-MAP-BITS) ;Ensure validity of meta bits
     ;; If maps weren't set up, popj.
        (popj-if-bit-set-xct-next l2-map-oldspace-meta-bit)
       ((vma) a-trans-vma)                      ;Restoring VMA which could have been bashed.

        ((vma-start-read) md)                   ;Read word out of old space.
        (check-page-read)
        (dispatch-xct-next q-data-type md d-trans-old)
       ((a-trans-md) vma)                       ;Save pointer to old space.

     ;; Fall through on GC-forwards.
trans-old-gc-fwd
     ;; GC-forward in oldspace, snap out.
        ((md) q-pointer md a-trans-md)          ;Combine new pointer with old tag.
        ((vma-start-write) a-trans-vma)
        (check-page-write-force)
        (gc-write-test-volatility)
        (popj)

(locality d-mem)
;Dispatch on datatype of word fetched from old space when transporting a pointer to old-space
;Usually go to TRANS-OLD-COPY to copy the containing structure.  Check specially for
;GC-FORWARD (already copied), invisibles (snap out).
(start-dispatch 5)
d-trans-old
        (trans-old-copy)        ;trap
        (trans-old-copy)        ;null
        (trans-old-copy)        ;unreconciled
        (trans-old-copy)        ;symbol
        (trans-old-copy)        ;symbol-header
        (trans-old-copy)        ;fix
        (trans-old-copy)        ;extended number
        (trans-old-copy)        ;header
        (p-bit r-bit)           ;gc-forward
        (trans-old-copy)        ;external-value-cell-pointer
        (trans-old-copy)        ;one-q-forward
        (trans-old-hdr-fwd)     ;header-forward
        (trans-old-body-fwd)    ;body-forward
        (trans-old-copy)        ;locative
        (trans-old-copy)        ;list
        (trans-old-copy)        ;u code entry
        (trans-old-copy)        ;fef
        (trans-old-copy)        ;array-pointer
        (trans-old-copy)        ;array-header
        (trans-old-copy)        ;stack-group
        (trans-old-copy)        ;closure
        (trans-old-copy)        ;indexed-forward
        (trans-old-copy)        ;select-method
        (trans-old-copy)        ;instance
        (trans-old-copy)        ;instance-header
        (trans-old-copy)        ;entity
        (trans-old-copy)        ;unused-32
        (trans-old-copy)        ;self-ref-pointer
        (trans-old-copy)        ;character
        (trans-old-copy)        ;rplacd-forward
        (trans-old-copy)        ;spare
        (trans-old-copy)        ;small-flonum
 (repeat nqzusd (trans-old-copy))
(end-dispatch)
(locality i-mem)

;;; DTP-BODY-FORWARD in old space.  Must find header, find new copy, and snap out.
trans-old-body-fwd
        ((vma-start-read) md)                   ;Pick up the DTP-HEADER-FORWARD.
        (check-page-read)
        ((a-trans-tem) sub vma a-trans-md)      ;Offset from particular Q to header.
        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-header-forward)) illop)
      ;; *pace* is this right?
      (error-table crash transporter dtp-body-forward pointer to a word  not of datatype dtp-header-forward)
        ((md) sub md a-trans-tem)               ;MD gets address of new copy of Q.
     ;; Fall through.

;;; DTP-HEADER-FORWARD in oldspace.  Snap out, then transport in case it pointed to oldspace.
trans-old-hdr-fwd
        ((md) q-pointer md a-trans-md)
        ((vma-start-write) a-trans-vma)
        (check-page-write-force)
;;; We are transporting here, so the transport-flag should be set (I think) -JRM
        ((m-transport-flag) dpb (m-constant -1) a-flags)
        (gc-write-test-volatility)
        (popj-after-next (m-transport-flag) dpb m-zero a-flags)
       (no-op)

;Enter here for trapping data type.  If it points to old-space, and
;this is not an inum-type (DTP-NULL), will have already been
;transported.  If going to write, we ignore it, otherwise we trap anyway.
TRANS-TRAP
        (CALL-IF-BIT-CLEAR (BYTE-FIELD 1 4) READ-I-ARG
                TRANS-REALLY-TRAP)      ;BARF IF READING RANDOM DATA

;;; Return to caller, causing dispatch to drop through by OA-modifying it.
;;; Assume that VMA and MD haven't been modified, or have been saved and restored.
trans-drop-through
         (popj-after-next)
#+cadr  ((oa-reg-low) dpb (m-constant -1) oal-disp a-zero)      ;Force dispatch to location 3777.
#+lambda((oa-reg-high) dpb (m-constant -1) oah-disp a-zero)     ;Force dispatch to location 7777.
#+exp   ((oa-reg-low) dpb (m-constant -1) oal-disp a-zero)      ;Force dispatch to location 3777.

;;; Return to caller, causing dispatch to drop through by OA-modifying it.
;;; Assume that VMA and MD haven't been modified, or have been saved and restored.
;;; ILLOP if doing TRANSPORT-HEADER.
transport-header-drop-through
         (call-if-bit-set (byte-field 1 2) read-i-arg illop)    ;Barf if TRANSPORT-HEADER.
         (popj-after-next)
#+cadr  ((oa-reg-low) dpb (m-constant -1) oal-disp a-zero)      ;Force dispatch to location 3777.
#+lambda((oa-reg-high) dpb (m-constant -1) oah-disp a-zero)     ;Force dispatch to location 7777.
#+exp   ((oa-reg-low) dpb (m-constant -1) oal-disp a-zero)      ;Force dispatch to location 3777.

;Since MD is not saved in the stack group state, save it elsewhere (on the stack)
;and then call trap.  It is sort of important to be able to tell what data in MD
;actually caused the trap, in case the contents of the addressed location change.
TRANS-REALLY-TRAP
        ;later change this to store DTP-LOCATIVE if appropriate
        ((pdl-push) dpb vma q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        ((pdl-push) ldb q-data-type vma (a-constant (byte-value q-data-type dtp-fix)))
        ((PDL-PUSH) DPB MD Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        ((PDL-PUSH) LDB Q-DATA-TYPE MD (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
        (CALL TRAP)
    (ERROR-TABLE TRANS-TRAP) ;This is a special entry, which the EH knows all about.
    (ERROR-TABLE RESTART TRANS-TRAP-RESTART)    ;Retart here
    (error-table restart trans-trap-new-restart) ;presence of this tag in the restart-list
                                                       ;tells the error handler that the
                                                       ;vma has been pushed
        ((MD) C-PDL-BUFFER-POINTER-POP)         ;with replacement data on the stack
        (PDL-POP)               ;Discard the MD value we provided for the error handler.
        (PDL-POP)
        (pdl-pop)               ;and the VMA
        (pdl-pop)
        (popj)

;Enter here if external-value-cell-pointer to old-space.
;If supposed to invz, transport first.  Otherwise, transport
;unless don't-transport bit is set.
TRANS-OLDP-EVCP
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 1) READ-I-ARG TRANS-OLD0) ;Transport if supposed to.
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 0) READ-I-ARG TRANS-OLD0) ;Transport first, if must invz
        (JUMP transport-header-drop-through)
        ;Drop into TRANS-EVCP-1 if either going to drop-through and no transp desired,
        ;or if going to ILLOP

;;; Transport external-value-cell-pointer to newspace.  I-ARG<0> clear means do nothing.
trans-evcp
        (jump-if-bit-clear (byte-field 1 0) read-i-arg transport-header-drop-through)
        ((m-tem1) md)
        (popj-after-next
          (vma-start-read) selective-deposit vma q-all-but-pointer a-tem1)
       (check-page-read)

;Enter here for DTP-HEADER-FORWARD pointer, always forwards.
;Already transported if was old-space.
TRANS-HFWD
;Chase forwarding pointer, restart cycle
TRANS-INVZ
        (jump-if-bit-set (byte-field 1 6) read-i-arg trans-drop-through)  ;if inhibit forwards
        ((m-tem1) md)
        (popj-after-next
          (vma-start-read) selective-deposit vma q-all-but-pointer a-tem1)
       (check-page-read)

;Enter here for one-q-forward.  Already transported if was old-space.
TRANS-OQF
        (JUMP-IF-BIT-SET (BYTE-FIELD 1 3) READ-I-ARG TRANS-DROP-THROUGH) ;IGNORE OQF IF JUST
        (JUMP-XCT-NEXT TRANS-INVZ)                                       ;CHECKING CDR CODE
       (CALL-IF-BIT-SET (BYTE-FIELD 1 2) READ-I-ARG ILLOP)      ;BARF IF TRANSPORT-HEADER

trans-indxf
        (jump-if-bit-set (byte-field 1 3) read-i-arg trans-drop-through) ;oqf not invis
        (call-if-bit-set (byte-field 1 2) read-i-arg illop)    ;invis illegal.
        (jump-if-bit-set (byte-field 1 6) read-i-arg trans-drop-through)  ;if inhibit forwards
        ((a-trans-tem) read-i-arg)      ;save for test below and debugging.
        ((a-trans-vma) vma)     ;save for debugging, and to preserve cdr code.
        ((pdl-push) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))
        ((vma-start-read) a-indexed-cell-array)
        (check-page-read)
        (call-equal vma a-v-nil illop)
trans-indxf3
        (jump-data-type-equal md (a-constant (byte-value q-data-type dtp-header-forward))
                              trans-indxf1)
        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-header))
                              illop)
        ((m-tem1) q-pointer vma)
        ((m-tem1) m+a+1 pdl-pop a-tem1)
        (jump-if-bit-clear (lisp-byte %%array-long-length-flag) md trans-indxf2)
        ((m-tem1) m+1 m-tem1)  ;long array, move down one.
trans-indxf2
        ((vma) a-trans-vma)
        ((m-tem) a-trans-tem)
        (jump-if-bit-set (byte-field 1 7) m-tem trans-indxf-convert)
        (popj-after-next
          (vma-start-read) selective-deposit vma q-all-but-pointer a-tem1)
       (check-page-read)

trans-indxf1
        ((vma-start-read) md)
        (check-page-read)
        (jump trans-indxf3)

trans-indxf-convert
;leave a OQF in MD pointing to where we would have ref'ed.  CDR code 0.
        (popj-after-next
         (md) ldb q-pointer m-tem1
                (a-constant (byte-value q-data-type dtp-one-q-forward)))        ;was evcp.
       (no-op)

;Enter here for DTP-BODY-FORWARD, always forwards, but must "go around" through header
TRANS-BFWD
        (jump-if-bit-set (byte-field 1 6) read-i-arg trans-drop-through)
        ((a-trans-vma) vma)
        ((vma-start-read) dpb md q-pointer a-trans-vma)
        (check-page-read)
        (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-header-forward)) illop)
        ((pdl-push) a-trans-vma)                       ;Push address of body-forward.
        ((pdl-push) vma)                               ;Push address of header-forward.
        (dispatch transport-header md)
        ((m-tem1) pdl-pop)
        ((m-tem) sub pdl-pop a-tem1)                   ;Delta between header-forward and body-forward.
        ((m-tem) add vma a-tem)
        (popj-after-next
          (vma-start-read) selective-deposit m-tem1 q-all-but-pointer a-tem)
       (check-page-read)

;Enter here if self-ref-pointer.  Need not transport from old space since Q-POINTER field
;is not really a pointer.
TRANS-SRP
        (JUMP-IF-BIT-SET (LISP-BYTE %%SELF-REF-MONITOR-FLAG) MD TRANS-MONITOR)
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 0) READ-I-ARG transport-header-drop-through) ;JUMP IF SHOULDN'T INVZ
        ((M-TEM) DPB M-ZERO Q-POINTER A-SELF)
        (CALL-NOT-EQUAL M-TEM
         (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-INSTANCE))
         TRAP)
    (ERROR-TABLE SELF-NOT-INSTANCE)
;Jump if this pointer wants no mapping table.
        (JUMP-IF-BIT-CLEAR (LISP-BYTE %%SELF-REF-RELOCATE-FLAG) MD TRANS-SRP-NO-MAP)
;Jump if combined method getting another mapping table.
        (JUMP-IF-BIT-SET-XCT-NEXT (LISP-BYTE %%SELF-REF-MAP-LEADER-FLAG) MD
         TRANS-SRP-MAP-LEADER)
       ((A-TRANS-VMA) VMA)
;Jump if no mapping table now.
        ((M-TEM) A-SELF-MAPPING-TABLE)
        (JUMP-DATA-TYPE-NOT-EQUAL M-TEM (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ARRAY-POINTER))
                                  TRANS-SRP-NO-MAP)
;Map the SELF-REF-INDEX thru the mapping table, an ART-16B array.
        ((M-TEM) (LISP-BYTE %%SELF-REF-WORD-INDEX) MD)
        ((A-TRANS-MD) LDB (BYTE-FIELD 1 0) MD A-ZERO)
        ((VMA-START-READ) M+A+1 A-SELF-MAPPING-TABLE M-TEM)
        (CHECK-PAGE-READ)
        ((VMA) A-TRANS-VMA)
        (JUMP-EQUAL A-TRANS-MD M-ZERO TRANS-SRP-NO-MAP)
        ((MD) (BYTE-FIELD 20 20) MD)
;Make a pointer to the desired slot in SELF and then INVZ to there.
;MD contains the slot number.
;Note: no need to check for oldspace since A-SELF can't point to oldspace.
TRANS-SRP-NO-MAP
        ((M-TEM) (LISP-BYTE %%SELF-REF-INDEX) MD)
;       (CALL-GREATER-THAN M-TEM (A-CONSTANT 7770) TRAP)
;    (ERROR-TABLE NONEXISTENT-INSTANCE-VARIABLE VMA)
        (jump-greater-than m-tem (a-constant 7770) trans-srp-no-map-trap)
        ((MD) M+A+1 A-SELF M-TEM)
        (JUMP-XCT-NEXT TRANS-INVZ)
       ((MD) DPB MD Q-POINTER           ;avoid garbage data type
                (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-ONE-Q-FORWARD)))        ;was evcp.

trans-srp-no-map-trap
        ((pdl-push) dpb md q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (call trap)
       (error-table new-nonexistent-instance-variable)
        ;the comment in the error handler says this happens when compiled code
        ;references an instance variable that is no longer part of that flavor
        ;therefore, we don't worry about restart ...

;Here to get the contents of what an array leader slot points to.
;In this case, we get the mapping table from local slot 1 on the stack.
TRANS-SRP-MAP-LEADER
        ((PDL-BUFFER-INDEX) M+A+1 M-ZERO A-LOCALP)
        ((A-TRANS-MD) (LISP-BYTE %%SELF-REF-INDEX) MD)
        ((M-TEM) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
        (CALL-EQUAL M-TEM A-V-NIL TRAP)
    (ERROR-TABLE NO-MAPPING-TABLE-1)
        ((VMA) SUB C-PDL-BUFFER-INDEX A-TRANS-MD)
        ((VMA-START-READ) SUB VMA (A-CONSTANT 2))
        (CHECK-PAGE-READ)       ;Get the contents of the arrayleader slot.
        ((VMA) DPB VMA Q-POINTER A-TRANS-VMA)   ;Go thru gyrations to preserve data type field
        (DISPATCH TRANSPORT MD)                 ;of the original VMA that pointed to the SRP.
        ;; A-TRANS-VMA has just been clobbered!
        ;; The array-leader slot contains a locative to a cell.
        ;; Put the cell's address in VMA and read the cell's contents.
        ((A-TRANS-VMA) VMA)
        (POPJ-AFTER-NEXT
         (VMA-START-READ) DPB MD Q-POINTER A-TRANS-VMA)
       (CHECK-PAGE-READ)  ;Return, and transport the new contents of VMA/MD.

TRANS-MONITOR
;Here if we encounter a DTP-SELF-REF-POINTER whose %%SELF-REF-MONITOR-FLAG bit is set.
;Forward to the following word.  But first, get an error if about to write.
        (CALL-IF-BIT-SET READ-I-ARG (A-CONSTANT 40) TRANS-REALLY-TRAP)
        (POPJ-AFTER-NEXT (VMA-START-READ) ADD VMA (A-CONSTANT 1))
       (CHECK-PAGE-READ)


;;; Routine to copy what A-TRANS-MD points to into area in M-TEM.
;;; Returns with MD pointing to copy.  Leaves forwarding pointers behind.
;;; Note that cdr-code of the GC-forwarding pointer must be cdr-error,
;;; to avoid faking out XFSHL.
;;; Used by the transporter and the extra-pdl copier.
;;; Can't save registers in the pdl buffer since might be called from XFLIPW
;;; and might decide to clobber the registers with GC-forwarding pointers.

(locality a-mem)
a-transport-new-object (0)
a-transport-old-object-base (0)
a-transport-new-object-base (0)
a-transport-boxed-size (0)
a-transport-object-type (0)
a-transport-saved-gc-light (0)
a-transport-saved-run-light (0)
a-transporter-start-time (0)
a-transporter-start-disk-time (0)
(locality i-mem)

(locality d-mem)
(start-dispatch 1)
d-transporter-allocation
        (p-bit allocate-list-copy-storage)
        (p-bit allocate-structure-copy-storage)
(end-dispatch)
(locality i-mem)

;;; Note: LM-Prolog depends on DTP-RPLACD-FORWARDS not being snapped.  If this assumption
;;; is changed, something different (perhaps a disable flag) will need to be done.

trans-old-copy
        (call-equal m-zero a-scavenge-queue-active turn-on-transporter-run-light)
        (call-xct-next trans-copy-save)
       ((m-transport-flag) dpb (m-constant -1) a-flags)

        (call-xct-next xrgn1)
       ((m-a) a-trans-md)
        (call-equal m-t a-v-nil illop)                     ;Error check.
        ((a-trans-tem) m-t)                                ;Save region-number for volatility computation.
        ((vma-start-read) add m-t a-v-region-area-map)
        (check-page-read)
        (call-xct-next find-structure-leader-region-known)
       ((m-s) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))

        (call-xct-next structure-info)                     ;Find size of structure.
       ((a-transport-old-object-base) m-t)                 ;Save base of old object.
     ;; Boxed size and type are for interacting with scavenge-queue.
        ((a-transport-object-type) (byte-field 1 1) m-k)   ;1 means it's a pdl.
        ((a-transport-boxed-size) m-3)

     ;; Compute volatility of region to copy into, pass (untagged) in M-E.
     ;; If GCing level 0, copy all regions to level 0.
        ((m-e) dpb m-zero (byte 36 2) a-gc-switches)
        (jump-equal m-e a-zero transport-allocate)
        ((m-tem) a-trans-tem)
        ((vma-start-read) add m-tem a-v-region-bits)
        (check-page-read)
        ((m-e) (lisp-byte %%region-volatility) md)
        (jump-equal m-e a-zero transport-allocate)
        ((m-e) sub m-e (a-constant 1))

transport-allocate
        ((m-b) add m-3 a-4)
        ((a-transporter-words-copied) add m-b a-transporter-words-copied)
        (dispatch-xct-next (byte-field 1 0) m-k d-transporter-allocation)
       ((m-a) m-3)

        ((a-transport-new-object-base) q-pointer m-t)
        ((m-e) dpb m-zero q-all-but-pointer a-transport-old-object-base)
        ((m-tem) sub m-t a-e)                                   ;Offset from old to new.
        ((a-transport-new-object) output-selector-mask-25 add m-tem a-trans-md)

     ;; Charge scavenger and scavenge-queue 1 unit for every word copied.
        ((m-tem) sub m-zero a-b)
        ((a-scavenge-work) add m-tem a-scavenge-work)
        ((a-scavenge-queue-work) add m-tem a-scavenge-queue-work)

        (call-xct-next transport-copy-boxed-block)
       ((m-3) m-a)
        (call-not-equal-xct-next m-b a-a transport-copy-unboxed-block)
       ((m-3) sub m-b a-a)

        (call-xct-next trans-copy-restore)
       ((m-transport-flag) dpb m-zero a-flags)
        (call-equal m-zero a-transport-object-type transport-run-scavenge-queue)
        (call-equal m-zero a-scavenge-queue-active turn-off-transporter-run-light)
        ((md) a-transport-new-object)
        ((vma-start-write) a-trans-vma)
        (check-page-write-force)
        (gc-write-test-volatility)
        (popj)

;;; Given source address in M-E, destination address in M-T, number of words in M-3.
;;; Copy the object from source to destination, forwarding the
;;; old locations to the new ones.  M-E and M-T are post-incremented to just after the copied
;;; words.
transport-copy-boxed-block
        ((m-3) add m-e a-3)                                ;1+ last source address.
transport-copy-boxed-loop
        ((vma-start-read) m-e)                             ;Read source word.
        (check-page-read)
        ((m-e) add m-e (a-constant 1))
        ((vma-start-write) m-t)                            ;Copy to destination.
        (check-page-write-force)
        (gc-write-test-volatility)                         ;Boxed, so test volatility.
        ((md) q-pointer m-t (a-constant (plus (byte-value q-data-type dtp-gc-forward)
                                              (byte-value q-cdr-code cdr-error))))
        ((vma-start-write) sub m-e (a-constant 1))         ;Write forwarding pointer back to source.
        (check-page-write-force)
;;;Merge from LAD on 9-23-86 (mrc)
;begin bug trap, make sure all DTP-GC-FORWARDs written in old space.
;       ((md) vma)
;       (no-op)
;       (call-if-bit-set l2-map-oldspace-meta-bit illop)

        (jump-not-equal-xct-next m-e a-3 transport-copy-boxed-loop)
       ((m-t) add m-t (a-constant 1))
        (popj)

;;; Given source address in M-E, destination address in M-T, number of words in M-3.
;;; Copy the object from source to destination.  M-E and M-T
;;; are post-incremented to just after the copied words.
transport-copy-unboxed-block
     ;; Unboxed loop doesn't do GC-WRITE-TEST for destination words, and doesn't
     ;; bother to forward source words.
        ((m-3) add m-e a-3)                                ;1+ last source address.
transport-copy-unboxed-loop
        ((vma-start-read) m-e)                             ;Read source word.
        (check-page-read)
        ((m-e) add m-e (a-constant 1))
        ((vma-start-write) m-t)                            ;Copy to destination.
        (check-page-write-force)                ;**unboxed
        (jump-not-equal-xct-next m-e a-3 transport-copy-unboxed-loop)
       ((m-t) add m-t (a-constant 1))
        (popj)

trans-copy-save
     ;; Save registers clobbered by consing and find-structure-leader.
        ((a-trans-save-a) m-a)
        ((a-trans-save-b) m-b)
        ((a-trans-save-e) m-e)
        ((a-trans-save-k) m-k)
        ((a-trans-save-s) m-s)
        ((a-trans-save-t) m-t)
        ((a-trans-save-3) m-3)
        ((a-trans-save-4) m-4)
        (popj-after-next
          (a-trans-save-5) m-5)
       ((a-trans-save-6) m-6)

trans-copy-restore
        ((m-6) a-trans-save-6)
        ((m-5) a-trans-save-5)
        ((m-4) a-trans-save-4)
        ((m-3) a-trans-save-3)
        ((m-t) a-trans-save-t)
        ((m-s) a-trans-save-s)
        ((m-k) a-trans-save-k)
        ((m-e) a-trans-save-e)
        (popj-after-next
          (m-b) a-trans-save-b)
       ((m-a) a-trans-save-a)

turn-on-transporter-run-light
#-lambda(begin-comment)
        ((a-transporter-start-time) stat-counter-aux)
        ((a-transporter-start-disk-time) a-disk-wait-time)
#-lambda(end-comment)
        ((vma) a-disk-run-light)
     ;; Save old GC light value.
        ((vma-start-read) sub vma (a-constant 2))
        (check-page-read-map-reload-only)
        ((a-transport-saved-gc-light) md)
     ;; Turn on transporter half of GC light.
        ((md-start-write) dpb m-minus-one (byte-field 20 20) a-transport-saved-run-light)
        (check-page-write-map-reload-only)
     ;; Save old user light value.
        ((vma-start-read) add vma (a-constant 4))
        (check-page-read-map-reload-only)
        ((a-transport-saved-run-light) md)
     ;; Turn off user light.
        (popj-after-next
          (md-start-write) m-zero)
       (check-page-write-map-reload-only)

turn-off-transporter-run-light
#-lambda(begin-comment)
        ((m-tem) sub stat-counter-aux a-transporter-start-time)
        ((a-transporter-time) add m-tem a-transporter-time)
        ((m-tem) a-disk-wait-time)
        ((m-tem) sub m-tem a-transporter-start-disk-time)
        ((a-transporter-disk-time) add m-tem a-transporter-disk-time)
#-lambda(end-comment)
     ;; Restore GC light to previous state.
        ((vma) a-disk-run-light)
        ((md) a-transport-saved-gc-light)
        ((vma-start-write) sub vma (a-constant 2))
        (check-page-write-map-reload-only)
     ;; Restore user light to previous state.
        ((vma) add vma (a-constant 4))
        (popj-after-next
          (md-start-write) a-transport-saved-run-light)
       (check-page-write-map-reload-only)


;;; EXTRA-PDL-TRAP
;;; We get here if we just wrote a possible pointer to the extra-pdl
;;; into main memory.  If so, we must copy the object out into a normal
;;; area and do the write again.  Mustn't sequence-break while the
;;; bad thing is in memory, and mustn't clobber anything other than
;;; what page faults clobber.  SMASHES PDL-BUFFER-INDEX.
;;; (I-ARG 1) indicates coming from pdl-buffer dumper, special return
;;; indicated since map has been munged.  (I-ARG 2) means from scavenger,
;;; should not get here.  (I-ARG 4) means from
;;; transporter or otherwise shouldnt be here, ILLOP.
;;;  --ON LAMBDA, PDL-BUFFER-DUMPER DOES NOT AFFECT MAP.  HOWEVER SPECIAL HANDLING IS
;;; STILL REQUIRED TO UPDATE PDL-BUFFER VARIABLES WHICH ARE NOT INCREMENTED EACH TIME
;;; AROUND P-B-MR1 LOOP.
;;; Note that we cannot get here from inside the transporter, which
;;; is fortunate since some variables are shared.

EXTRA-PDL-TRAP
        (call-if-bit-set (byte-field 1 2) read-i-arg illop)  ;should not come
                                        ;from transporter
        (popj-if-bit-set (byte 1 1) read-i-arg)
        ((A-TRANS-MD) MD)                       ;SAVE DUBIOUS OBJECT
        (JUMP-IF-BIT-SET-XCT-NEXT               ;CHECK FOR CALL FROM PDL-BUFFER DUMPER
                (BYTE-FIELD 1 0) READ-I-ARG EXTRA-PDL-TRAP-0)
       ((A-TRANS-VMA) VMA)                      ;SAVE ADDRESS WRITTEN INTO
EXTRA-PDL-TRAP-1
;Only if MD points at the extra pdl area do we need to copy it.
        (DISPATCH L2-MAP-STATUS-CODE D-GET-MAP-BITS) ;ENSURE VALIDITY OF META BITS
                ;RETURN IF FALSE ALARM
  ;on lambda & exp, it does not win to just popj.  Level 1 meta bits have been "captured"
  ;on the last memory cycle, and just making the map valid does affect them
  ;(if fact, it clears them if memory cycles have been taken while reloading the map)
        (POPJ-IF-BIT-SET-XCT-NEXT L2-MAP-EXTRA-PDL-META-BIT)
       ((VMA) A-TRANS-VMA)                      ;RESTORE VMA
;Don't copy if the pointer is being stored in the extra pdl area.
        ((MD) VMA)
        (no-op)                 ;give map time.
        (DISPATCH L2-MAP-STATUS-CODE D-GET-MAP-BITS) ;ENSURE VALIDITY OF META BITS
        (popj-IF-BIT-CLEAR-XCT-NEXT             ;Return if false alarm.
                L2-MAP-EXTRA-PDL-META-BIT)
;               TRANS-DROP-THROUGH)
       ((MD) A-TRANS-MD)                        ;RESTORE MD

;Don't copy if storing into virtual address which will map to PDL buffer.
        ((m-tem) dpb m-zero q-all-but-pointer a-trans-vma)
        (jump-less-than m-tem a-pdl-buffer-virtual-address extra-pdl-trap-2)
        ((m-tem) sub m-tem a-pdl-buffer-active-qs)
        ((pdl-buffer-index) sub pdl-buffer-pointer a-ap)
        ((m-lam) pdl-buffer-index)      ;do modulo arithmetic
        ((m-tem) sub m-tem a-lam)       ;account for Qs in current frame
                                        ;(in case clobbering REST arg).
        (popj-less-than m-tem a-pdl-buffer-virtual-address); trans-drop-through

(begin-comment) Zwei Lossage (end-comment)

extra-pdl-trap-2
        (call-xct-next trans-copy-save)
       ((m-transport-flag) dpb (m-constant -1) a-flags)
        (call-xct-next structure-info)
       ((m-t) a-trans-md)
        ((m-a) m-3)
        ((m-b) add m-3 a-4)
        (call-xct-next allocate-structure-storage)
       ((m-s) dpb m-zero q-all-but-typed-pointer a-background-cons-area)

        ((a-transport-new-object-base) q-pointer m-t a-trans-md)
        ((m-e) dpb m-zero q-all-but-pointer a-trans-md)
        (call-xct-next transport-copy-unboxed-block)
       ((m-3) m-b)

     ;; Push words needed later in the reentrant portion.
        ((pdl-push) a-transport-new-object-base)
        ((pdl-push) a-trans-vma)
        ((pdl-push) m-a)

        (call-xct-next trans-copy-restore)
       ((m-transport-flag) dpb m-zero a-flags)

     ;; Loop through the copied boxed words to see if any point into extra-pdl.
     ;; This code must be reentrant, since it uses GC-WRITE-TEST.  The stack contains the
     ;; number of boxed words to look at, and the vma and md to be restored (pushed above).
        ((vma) a-transport-new-object-base)
        (jump-less-than pdl-top (a-constant 2) extra-pdl-trap-4)
extra-pdl-trap-5
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        ((vma-start-write) vma)                        ;Yes, this is necessary, see GC-WRITE-TEST.
        (check-page-write)
        (gc-write-test)
        (jump-not-equal-xct-next pdl-top (a-constant 2) extra-pdl-trap-5)
       ((pdl-top) sub pdl-top (a-constant 1))
extra-pdl-trap-4
        (pdl-pop)
        ((a-trans-vma) pdl-pop)
        ((md) pdl-pop)
extra-pdl-trap-3
        ((vma-start-write) a-trans-vma)
        (check-page-write-force)
        (gc-write-test-volatility)
        (popj)

;Here for EXTRA-PDL-TRAP while storing pdl-buffer.  Must clean up
;before processing the trap, and must eventually return to P-B-MR0.
;M-1 has the original map word contents.
;Cleanup is different in that VMA and PI haven't been advanced yet.
EXTRA-PDL-TRAP-0
        (call-not-equal vma a-lam illop)
        ((M-TEM) SUB m-lam A-PDL-BUFFER-VIRTUAL-ADDRESS)        ;Number of locations dumped -1
        ((M-PDL-BUFFER-ACTIVE-QS) M-A-1 M-PDL-BUFFER-ACTIVE-QS A-TEM)
        ((A-PDL-BUFFER-VIRTUAL-ADDRESS) ADD m-lam (A-CONSTANT 1))
        ((PDL-BUFFER-INDEX) ADD PDL-BUFFER-INDEX (A-CONSTANT 1))
        ((A-PDL-BUFFER-HEAD) PDL-BUFFER-INDEX)

;traditional map restore code
;       ((MD) Q-R)                              ;Address the map
;#+cadr ((VMA-WRITE-MAP) DPB M-1                ;Restore the map for this page
;               MAP-WRITE-SECOND-LEVEL-MAP
;               (A-CONSTANT (BYTE-MASK MAP-WRITE-ENABLE-SECOND-LEVEL-WRITE)))
;#+lambda((L2-MAP-CONTROL) M-1)
#-exp (begin-comment)
        ((md) q-r) ;address map
        (no-op)    ; this no-op required on EXP, according to manual.
  ;note: volatility not maintained for PDL pages..  (here or elsewhere)
        ((vma-write-l2-map-control) m-1) ;restore entry saved from p-b-mr0
        ((md) a-trans-md)
#-exp (end-comment)

;newest exit, may 1985, no push-own-address in gc-write-test
        ((m-garbage) #+lambda micro-stack-pntr-and-data-pop
                     #+exp micro-stack-data-pop)
        (jump-xct-next extra-pdl-trap-1)
       ((micro-stack-data-push) (a-constant (i-mem-loc p-b-mr0)))

;traditional exit code; pre 1984
;       ;; EXTRA-PDL-TRAP-1 will clean up garbage in VMA.
;#+LAMBDA((MD) SETA A-TRANS-MD                  ;Restore dubious MD
;               MICRO-STACK-PNTR-AND-DATA-POP)  ;and flush useless return address
;#+EXP  ((MD) SETA A-TRANS-MD MICRO-STACK-DATA-POP)
;       (JUMP-XCT-NEXT EXTRA-PDL-TRAP-1)        ;Return to mainline
;       ((MICRO-STACK-DATA-PUSH) (A-CONSTANT (I-MEM-LOC P-B-MR0-HACK))) ;A REAL KLUDGE!!!!
;               ;read the comment below before you think you understand this!!!

;exit code that doesn't deposite in non-dispatch instructions; about april 1984
;p-b-mr0-hack   ;extra-pdl-trap-1 can return through TRANS-DROP-THRU, which will MODIFY the
;              ;dispatch table operand of the following instruction!! Since its a dispatch
;              ;guarenteed to be a noop, it wont hurt.
;       (dispatch (byte-field 0 0) all-ones-in-d-mem)
;       (jump p-b-mr0)

;;; Given page number in M-LAM, return volatility in M-TEM.
read-page-volatility
        (declare (args a-lam) (values a-tem) (clobbers a-tem1 a-tem3))
        ((m-tem1) (byte-field (difference q-pointer-width 4) 4) m-lam)
        ((vma-start-read) add m-tem1 a-v-virtual-page-volatility)
        (illop-if-page-fault)
        ((m-tem3) dpb m-lam (byte-field 4 1) a-zero)
        ((m-tem3) sub (m-constant 40) a-tem3)
        (popj-after-next
          (oa-reg-low) oal-mrot m-tem3)
       ((m-tem) (byte-field 2 0) md)

;;; Given virtual address in A-GC-TRAP-VMA, desired volatility in A-GC-TRAP-MD-VOLATILITY, update the
;;; virtual-page-volatility bits, the level-2 map, and the page-hash-table to reflect the
;;; new volatility.
update-page-volatility
     ;; Update level-2 map.
        ((md) a-gc-trap-vma)
        (no-op)                                        ;Wait for maps.
        (dispatch l2-map-status-code d-get-map-bits)   ;Ensure validity of meta bits.
        ((m-tem) l2-map-control)
        ((m-tem1) xor m-minus-one a-gc-trap-md-volatility)
#+lambda((l2-map-control) dpb m-tem1 map2c-volatility a-tem)
#+exp   ((m-tem1) dpb m-tem1 map2c-volatility a-tem)
#+exp   ((vma-write-l2-map-control) dpb m-tem1 exp-map2c-volatility a-tem1)
update-page-volatility-no-maps
     ;; Update virtual-page-volatility.
        ((m-tem2) a-gc-trap-vma)
        ((m-tem2) q-page-number m-tem2)
        ((m-tem1) (byte-field (difference q-pointer-width 4) 4) m-tem2)        ;Word offset.
        ((vma-start-read) add m-tem1 a-v-virtual-page-volatility)
        (illop-if-page-fault)
        ((m-tem) a-gc-trap-md-volatility)
        ((m-tem1) md)
        ((oa-reg-low) dpb m-tem2 (byte-field 4 1) a-zero)
        ((md-start-write) dpb m-tem (byte-field 2 0) a-tem1)
        (illop-if-page-fault)
     ;; Update PHT entry if the page pointed to by the VMA is in memory.
        ((pdl-push) m-t)
        ((pdl-push) m-b)
        (call-xct-next search-page-hash-table)         ;Clobbers M-T and M-B.
       ((m-t) a-gc-trap-vma)
        ((m-b) pdl-pop)
        ((m-t) pdl-pop)
        (popj-if-bit-clear pht1-valid-bit md)
        ((vma-start-read) m+1 vma)
        (illop-if-page-fault)
        ((m-tem1) a-gc-trap-md-volatility)
        ((m-tem) md)
        (popj-after-next
          (md-start-write) dpb m-tem1 pht2-map-volatility a-tem)
       (illop-if-page-fault)

(locality a-mem)
a-gc-trap-vma (0)
a-gc-trap-md (0)
a-page-volatility                                      ;Scavenger uses this name.
a-gc-trap-md-volatility (0)
(locality i-mem)

;;; No more stack closures. ~jrm
;gc-write-and-stack-closure-trap

;     ;; note: stack-closure-trap needs the i-arg (set by pdl-buffer dumper).
;     ;; also note that stack-closure-trap does a volatility check after all is said and done.
;     ;; stack-closure-trap will illop if i-arg says comming from stack-closure copier.
;        (call stack-closure-trap)
;        (jump gc-write-trap)
;;could this replace above two insns, now that stack-closure-trap ends with volatility check?
;;      (jump stack-closure-trap)

gc-write-and-extra-pdl-trap
        ;;extra-pdl-trap will change the md, re-write it, and then do a gc-write-test
        (jump-if-bit-set map1-volatility-invalid l1-map extra-pdl-trap)

gc-write-trap
        ((a-volatility-traps) m+a+1 m-zero a-volatility-traps)

gc-write-trap-invalid-ok
        ((a-gc-trap-vma) vma)
        ((a-gc-trap-md) md)
        ((a-gc-trap-md-volatility) map1-volatility l1-map)

;;; This didn't catch anything.  Un-comment it if you think macrocode may be
;;; making oldspace pointers.
;     ;;; Bug test, don't write pointers to oldspace
;     ;; theoretically, this can't happen because there should be no oldspace
;     ;; pointers in the machine.  However, something is screwing up and this
;     ;; will rule out an easy case to check.
;#-lambda (begin-comment)
;        (dispatch l2-map-status-code d-get-map-bits)
;        (jump-if-bit-set map2c-oldspace-meta-bit l2-map-control gc-write-trap-invalid-ok-bug-test)
;     ;; The transporter, however, writes pointers to oldspace and it is ok.
;        (call-if-bit-clear m-transport-flag illop)
;#-lambda (end-comment)
;gc-write-trap-invalid-ok-bug-test

     ;; Check level-2 map
        ((md) a-gc-trap-vma)
        (no-op)
        (dispatch l2-map-status-code d-get-map-bits) ;Ensure validity of meta bits
                ;Should be already set up since you just did CHECK-PAGE-WRITE, but just in case.
                ;Make sure you don't do anything to clobber the map before it is
                ;written below
        ((m-tem) map2c-volatility l2-map-control)
;better get it from meta-bits even on explorer since d-get-map-bits sets up meta bits only
; and not necessarily explorer volatility bits field..
;       ((m-tem) exp-map2c-volatility l2-map-control)  ;above should work, but might as well
                ;get bits from same place hardware does. ** see above.
        ((m-tem) xor m-tem (a-constant 3))
        (jump-greater-or-equal m-tem a-gc-trap-md-volatility gc-write-trap-exit)
     ;; Update level-2 map volatility.
        ((m-tem) l2-map-control)
        ((m-tem1) xor m-minus-one a-gc-trap-md-volatility)
#+lambda((l2-map-control) dpb m-tem1 map2c-volatility a-tem)
#+exp   ((m-tem1) dpb m-tem1 map2c-volatility a-tem)
#+exp   ((vma-write-l2-map-control) dpb m-tem1 exp-map2c-volatility a-tem1)
        (call update-page-volatility-no-maps)
gc-write-trap-exit
        ((md) a-gc-trap-md)
        ((vma-start-write) a-gc-trap-vma)
        (check-page-write-force)
     ;; In some places we are tail-recursing back to the main loop, so don't POPJ-AFTER-NEXT.
        (popj)

(begin-pagable-ucode)
;MOBY UNRECONCILED STUFF.

UNRECONCILED-ILLOP
        (CALL ILLOP)
    (ERROR-TABLE UNRECONCILED-ILLOP)

UNRECONCILED-TRAP
        (CALL TRAP)
    (ERROR-TABLE UNRECONCILED-TRAP)

trans-unreconciled
   ;save VMA untyped.  This is OK since moby regions cant
   ;move.  Having pointers to moby stuff can cause it to bomb out in the error handler
   ;and places I dont want to deal with now.
        (call-xct-next save-untyped-on-pdl)
       ((m-tem) vma)
        (call save-state-acs-on-pdl)
        ((m-s) q-pointer vma (a-constant (byte-value q-data-type dtp-fix)))
   ;the pointer part of the DTP-UNRECONCILED is unused for now, but pass it along just
   ; for randomness.
        ((m-r) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))
        (call p3zero)
        ((arg-call ref-support-vector) (i-arg svc-unreconciled) i-dont-chain)
        (dispatch transport md)
        ((pdl-push) md)
        ((pdl-push) dpb m-s q-typed-pointer (a-constant (byte-value q-cdr-code cdr-next)))
        ((pdl-push) dpb m-r q-typed-pointer (a-constant (byte-value q-cdr-code cdr-nil)))
        ((arg-call mmcall) (i-arg 2))
        (call restore-state-acs-from-pdl)
        ((LOCATION-COUNTER) SUB LOCATION-COUNTER (A-CONSTANT #+lambda 2 #+exp 1))
                        ;IT IS NECESSARY THAT
                        ;M-INSTRUCTION-BUFFER ACTUALLY HAVE THE LAST INSTRUCTION
                        ;EXECUTED (IE NOT SUFFICIENT MERELY THAT THE CORRECT INSTRUCTION
                        ;WILL BE FETCHED NEXT TIME AROUND THE MAIN LOOP).  THIS IS BECAUSE
                        ;THE CURRENT MACRO-INSTRUCTION, WHICH MAY BE BEING REENTERED
                        ;IN THE MIDDLE, CAN DISPATCH AGAIN ON M-INSTRUCTION-STREAM
                        ;(TO GET THE DESTINATION IN MISC, FOR EXAMPLE).  THE SIMPLEST
                        ;WAY TO ASSURE THIS IS TO BACK UP THE LOCATION COUNTER AND
                        ;RE-ADVANCE IT.
        (DISPATCH ADVANCE-INSTRUCTION-STREAM i-dont-chain)
        (call restore-untyped-from-pdl)
        ((vma-start-read) m-tem)        ;reconstituted pointer and data.
        (check-page-read-no-interrupt)
   ;unreconciled handler should smash data, so just try again.
        (popj)

save-state-acs-on-pdl
        (declare (clobbers a-tem)
                 (args a-zr a-a a-b a-c a-d a-e a-t a-r a-q a-i a-j a-s a-k a-1 a-2))
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-zr)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-a)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-b)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-c)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-d)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-e)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-t)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-r)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-q)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-i)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-j)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-s)
        (call-xct-next save-typed-on-pdl)
       ((m-tem) m-k)
        (call-xct-next save-untyped-on-pdl)
       ((m-tem) m-1)
        (call-xct-next save-untyped-on-pdl)
       ((m-tem) m-2)
        (popj)

save-typed-on-pdl
        (declare (args a-tem))
        (dispatch q-data-type m-tem d-skip-on-pointerp)
        (jump save-typed-no-pointerp)
save-typed-pointerp
  ;save as 2 qs, a locative and a fixnum.
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-tem q-pointer
                (a-constant (byte-value q-data-type dtp-locative)))
       ((c-pdl-buffer-pointer-push) ldb q-all-but-pointer m-tem
                (a-constant (byte-value q-data-type dtp-fix)))

save-typed-no-pointerp
  ;save as 2 qs, two fixnums.
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-tem q-pointer
                (a-constant (byte-value q-data-type dtp-fix)))
       ((c-pdl-buffer-pointer-push) ldb q-all-but-pointer m-tem
                (a-constant (byte-value q-data-type dtp-fix)))


save-untyped-on-pdl
        (declare (args a-tem))
        (popj-after-next
         (c-pdl-buffer-pointer-push) dpb m-tem (byte-field 16. 0)
                (a-constant (byte-value q-data-type dtp-fix)))
       ((c-pdl-buffer-pointer-push) ldb (byte-field 16. 16.) m-tem
                (a-constant (byte-value q-data-type dtp-fix)))

restore-state-acs-from-pdl
        (declare (clobbers a-tem)
                 (values a-2 a-1 a-k a-s a-j a-i a-q a-r a-t a-e a-d a-c a-b a-a a-zr))
        (call restore-untyped-from-pdl)
        ((m-2) m-tem)
        (call restore-untyped-from-pdl)
        ((m-1) m-tem)
        (call restore-typed-from-pdl)
        ((m-k) m-tem)
        (call restore-typed-from-pdl)
        ((m-s) m-tem)
        (call restore-typed-from-pdl)
        ((m-j) m-tem)
        (call restore-typed-from-pdl)
        ((m-i) m-tem)
        (call restore-typed-from-pdl)
        ((m-q) m-tem)
        (call restore-typed-from-pdl)
        ((m-r) m-tem)
        (call restore-typed-from-pdl)
        ((m-t) m-tem)
        (call restore-typed-from-pdl)
        ((m-e) m-tem)
        (call restore-typed-from-pdl)
        ((m-d) m-tem)
        (call restore-typed-from-pdl)
        ((m-c) m-tem)
        (call restore-typed-from-pdl)
        ((m-b) m-tem)
        (call restore-typed-from-pdl)
        ((m-a) m-tem)
        (call restore-typed-from-pdl)
        ((m-zr) m-tem)
        (popj)

restore-typed-from-pdl
        (declare (values a-tem))
        (popj-after-next (m-tem) dpb c-pdl-buffer-pointer-pop q-all-but-pointer a-zero)
       ((m-tem) dpb c-pdl-buffer-pointer-pop q-pointer a-tem)

restore-untyped-from-pdl
        (declare (values a-tem))
        (popj-after-next (m-tem) dpb c-pdl-buffer-pointer-pop (byte-field 16. 16.) a-zero)
       ((m-tem) dpb c-pdl-buffer-pointer-pop (byte-field 16. 0) a-tem)



(end-pagable-ucode)

(LOCALITY D-MEM)
(START-DISPATCH 5 0)
;DISPATCH ON DATA TYPE.  DROPS THROUGH IN EITHER CASE BUT SKIPS IF pointerp.
D-SKIP-ON-POINTERP
        (P-BIT R-BIT)   ;TRAP  -no pointer-
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;NULL -considered pointer-
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;UNRECONCILED -considered pointer-
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;SYMBOL
        (P-BIT R-BIT)   ;SYMBOL HEADER -no pointer-
        (P-BIT R-BIT)   ;FIX
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;EXTENDED NUMBER
        (P-BIT R-BIT)   ;HEADER
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;GC-FORWARD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;EXTERNAL-VALUE-CELL-POINTER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;ONE-Q-FORWARD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;HEADER-FORWARD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;BODY-FORWARD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;LOCATIVE
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;LIST
        (P-BIT R-BIT)   ;U CODE ENTRY
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;FEF
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;ARRAY-POINTER
        (P-BIT R-BIT)   ;ARRAY-HEADER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;STACK-GROUP
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;CLOSURE
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;indexed-forward  -considered pointer-
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;SELECT-METHOD
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;INSTANCE
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;INSTANCE-HEADER
        (P-BIT R-BIT INHIBIT-XCT-NEXT-BIT)      ;ENTITY
        (P-BIT inhibit-xct-next-bit illop)      ;unused-32
        (P-BIT INHIBIT-XCT-NEXT-BIT ILLOP)      ;SELF-REF-POINTER
        (P-BIT R-BIT)                           ;CHARACTER
        (p-bit R-BIT INHIBIT-XCT-NEXT-BIT)      ;rplacd-forward
        (P-BIT R-BIT)                           ;spare
        (P-BIT R-BIT)                           ;SMALL-FLONUM
 (REPEAT NQZUSD (P-BIT R-BIT))                  ;no pointer.
(END-DISPATCH)
(LOCALITY I-MEM)

))

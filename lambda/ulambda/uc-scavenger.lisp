;;; -*- Mode:LISP; Package:LAMBDA; Readtable:ZL; Base:8; Fonts:(CPTFONT MEDFNT) -*-
;;; (c) Copyright 1985, Lisp Machine Incorporated.

(defconstant uc-scavenger '(

(locality a-mem)
a-scavenge-region (0)
a-scavenge-region-origin (0)
a-scavenge-region-free-pointer (0)
a-scavenge-object (0)
a-scavenge-next-object (0)
a-scavenge-object-bound (0)
a-scavenge-block-bound (0)
a-scavenge-work (0)
a-scavenge-volatility (0)
a-scavenge-state (0)  ;0 "normal", 2 resume scavenging page, 3 resume scavenging carefully
a-scavenge-block-return-pc (0)
a-scavenger-start-time (0)
a-scavenger-start-disk-time (0)
a-scavenger-transporter-start-time (0)
a-scavenger-transporter-start-disk-time (0)
a-scavenge-page-resident (0)
a-scavenge-idle-flag (0)
A-SCAVENGE-SCAN-POINTER (0)
(locality i-mem)

     (misc-inst-entry %gc-scav-reset)
reset-scavenger
        ((m-k) q-pointer pdl-pop)
        (jump-not-equal m-k a-scavenge-region xfalse)
        (jump-equal m-k a-zero xfalse)
        (call invalidate-cons-caches)
     ;; Set scavenge pointer back to object boundary.
        (call-xct-next store-region-gc-pointer)
       ((m-1) a-scavenge-object)
        ((a-scavenge-state) m-zero)
     ;; Check to see if we're beyond free pointer.
        ((vma-start-read) add m-k a-v-region-free-pointer)
        (check-page-read)
        ((m-1) output-selector-mask-25 add md a-scavenge-region-origin)
     ;; If we're still below the free pointer, don't worry about it.
        (jump-greater-or-equal m-1 a-scavenge-next-object xfalse)
     ;; Past free pointer, set scavenge-pointer = free-pointer and reset scavenger.
        (call-return store-region-gc-pointer xtrue)

     (misc-inst-entry %better-gc-scavenge)
better-scavenge
     ;; This should scavenge even if interrupts are disabled, since it's primarily called
     ;; from the scheduler, which runs uninterruptibly.
        ((a-scavenge-work) q-pointer pdl-pop)
        (call-xct-next scavenge-even-without-interrupts)
       ((a-scavenge-idle-flag) pdl-pop)
        (jump xfalse)

     (misc-inst-entry %gc-scavenge)
scavenge
     ;; This should scavenge even if interrupts are disabled, since it's primarily called
     ;; from the scheduler, which runs uninterruptibly.
        ((a-scavenge-work) q-pointer pdl-pop)
        (call-xct-next scavenge-even-without-interrupts)
       ((a-scavenge-idle-flag) a-v-nil)
        (jump xfalse)

scavenge-while-consing
     ;; Don't scavenge if interrupts disabled, to help macrocode that just
     ;; does without-interrupts (and not without-scavenging) during critical periods.
        ((m-tem) dpb m-zero q-all-but-typed-pointer a-inhibit-scheduling-flag)
        (popj-not-equal m-tem a-v-nil)
        ((a-scavenge-idle-flag) a-v-nil)
scavenge-even-without-interrupts
     ;; Extra-PDL trap depends on this test to inhibit scavenge during cons.
        (popj-if-bit-set m-transport-flag)
        ((m-tem) dpb m-zero q-all-but-typed-pointer a-inhibit-scavenging-flag)
        (popj-not-equal m-tem a-v-nil)
        ((m-tem) dpb m-zero q-all-but-typed-pointer a-gc-flip-ready)
        (popj-not-equal m-tem a-v-nil)
        (call scavenge-save)
        (jump-equal-xct-next m-zero a-scavenge-state scavenge-regions)
       ((a-scavenge-volatility) dpb m-zero (byte 36 2) a-gc-switches)
resume-scavenge
        (call load-region-scavenge-parameters)
        ((q-r) a-scavenge-state)
        (jump-equal-xct-next q-r (a-constant 2) scavenge-block)
       ((micro-stack-data-push) a-scavenge-block-return-pc)
     ;; Scavenging a PDL (carefully).  Scavenge limit may have changed since last invocation.
        (call-xct-next structure-info)
       ((m-t) a-scavenge-object)
        (jump-xct-next scavenge-block)
       ((a-scavenge-block-bound) add m-3 a-scavenge-object)

(begin-comment) Zwei Lossage (end-comment)

scavenge-next-region
        (call-xct-next store-region-gc-pointer)
       ((m-1) a-scavenge-region-free-pointer)
scavenge-regions
        ((vma-start-read) (a-constant (eval (+ 400 %sys-com-number-regions))))
        (illop-if-page-fault)
        ((a-scavenge-region) q-pointer md)
scavenge-regions-loop
        ((a-scavenge-region q-r) add m-minus-one a-scavenge-region)
        (jump-less q-r a-zero finish-scavenge)
        ((vma-start-read) add q-r a-v-region-bits)
        (illop-if-page-fault)
        (jump-if-bit-clear (lisp-byte %%region-scavenge-enable) md scavenge-regions-loop)
        (call-xct-next load-region-scavenge-parameters)
       ((pdl-push) md)

        (jump-equal-xct-next m-1 a-scavenge-region-free-pointer scavenge-regions-loop)
       ((m-tem) pdl-pop)
        (call-greater-than m-1 a-scavenge-region-free-pointer illop)
        (jump-if-bit-set (lisp-byte %%region-scavenge-carefully) m-tem scavenge-region-carefully)


scavenge-next-page
     ;; Run scavenge-queue, spending scavenge units.  Doing it this often helps locality.
        (call-not-equal-xct-next m-minus-one a-scavenge-queue-pointer run-scavenge-queue)
       ((a-scavenge-queue-work) a-scavenge-work)
        ((a-scavenge-work) a-scavenge-queue-work)
     ;; If the page we just scavenged was not resident before, flush it.  Now is the best time
     ;; to do it -- the scavenge queue has already run, which might reference the page.
        (jump-not-equal a-scavenge-page-resident m-zero scavenge-dont-flush)
scavenge-flush-page
        (call-xct-next search-page-hash-table)
       ((m-t) sub m-1 (a-constant 400))
     ;; Don't do it if page has no valid PHT entry.
        (jump-if-bit-clear pht1-valid-bit md scavenge-dont-flush)
     ;; Only flush normal pages.  In particular, don't do wired pages.
        ((m-tem) pht1-swap-status-code md)
        (jump-not-equal m-tem (a-constant (eval %pht-swap-status-normal)) scavenge-dont-flush)
        ((md-start-write) selective-deposit md pht1-all-but-age-and-swap-status-code
                          (a-constant (eval %pht-swap-status-flushable)))
        (illop-if-page-fault)
        ((md) md)               ;wait for memory cycle to finish
#+exp   (no-op)
     ;; Flush level-2 map.
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control) (a-constant 0))
scavenge-dont-flush
        (call-greater-or-equal m-1 a-scavenge-region-free-pointer scavenge-check-region-free-pointer)
     ;; Scavenging level 0, don't bother searching for worthwhile pages.  (Code below depends on this.)
        (jump-equal-xct-next m-zero a-scavenge-volatility scavenge-page)
       ((m-1) dpb m-zero (byte 10 0) a-1)

scavenge-page-search
     ;Read an entire word from the a-v-virtual-page-volatility table.
     ;This corresponds to a "sector".
        ((m-tem3) (byte (difference q-pointer-width 14) 14) m-1)
        ((vma-start-read) add m-tem3 a-v-virtual-page-volatility)
        (illop-if-page-fault)
        ((m-tem3) q-page-number m-1)
        ((m-tem3) dpb m-tem3 (byte 4 1) a-zero)        ;Position in human sense.
     ;; If all pages in this sector have zero volatility, skip it.
        (jump-equal-xct-next md a-zero scavenge-skip-sector)
       ((m-3) add m-1 (a-constant (eval (* 20 400))))
        ((m-tem3 q-r) sub (m-constant 40) a-tem3)      ;Complement for shifter.
     ;; The hardware form of the byte-position is also the correct byte-length, since we want the
     ;; upper (- 32. position) bits.  However, the Lambda actually wants 1- this value.
#+lambda((q-r) sub q-r (a-constant 1))
        ((oa-reg-low) dpb q-r oal-bytl-1 a-tem3)
        ((md) byte-inst md a-zero)
scavenge-page-pipeline
        ((m-tem) (byte 2 0) md)
        (jump-greater-or-equal m-tem a-scavenge-volatility scavenge-page-0)
        ((m-tem) (byte 2 2) md)
        (jump-greater-or-equal m-tem a-scavenge-volatility scavenge-page-1)
        ((m-tem) (byte 2 4) md)
        (jump-greater-or-equal m-tem a-scavenge-volatility scavenge-page-2)
        ((m-tem) (byte 2 6) md)
        (jump-greater-or-equal m-tem a-scavenge-volatility scavenge-page-3)
        ((md) (byte 24. 8.) md)
        (jump-not-equal-xct-next md a-zero scavenge-page-pipeline)
       ((m-1) add m-1 (a-constant 2000))
scavenge-skip-sector
     ;; None of the remaining pages in this sector contain volatile pointers, skip to next sector.
        ((m-1) dpb m-zero (byte 14 0) a-3)
        (jump-xct-next scavenge-page-search)
       (call-greater-or-equal m-1 a-scavenge-region-free-pointer scavenge-check-region-free-pointer)

scavenge-page-3
        ((m-1) add m-1 (a-constant 400))
scavenge-page-2
        ((m-1) add m-1 (a-constant 400))
scavenge-page-1
        ((m-1) add m-1 (a-constant 400))
scavenge-page-0
     ;; If page is already resident in physical memory, set a-scavenge-page-resident to 1,
     ;; otherwise set it to 0.  This is used later to decide whether to flush the page.
        (call-xct-next search-page-hash-table)
       ((m-t) q-pointer m-1)
        (jump-if-bit-clear-xct-next pht1-valid-bit md scavenge-page)
       ((a-scavenge-page-resident) (a-constant 0))
        ((a-scavenge-page-resident) (a-constant 1))
scavenge-page
        (call-greater-or-equal m-1 a-scavenge-region-free-pointer scavenge-check-region-free-pointer)
     ;; Reset volatility of this page to zero.
        ((a-gc-trap-vma) m-1)
        (call-xct-next update-page-volatility)
       ((a-gc-trap-md-volatility) m-zero)

     ;; Look up Initial-Qs, First-Header in structure handles.
        (call-xct-next read-structure-handle)
       ((m-3) q-page-number m-1)

        ((a-scavenge-next-object) add m-1 a-5)             ;First-Object.
        ((a-scavenge-object-bound) add m-1 a-6)            ;Initial-Qs.
        ((a-scavenge-object) m-1)                          ;Compatability with careful code.
        ((a-scavenge-block-bound q-r) add m-1 (a-constant 400))
        ((a-scavenge-state) (a-constant 2))
        (jump-less-or-equal-xct-next q-r a-scavenge-region-free-pointer scavenge-block)
       ((micro-stack-data-push) (a-constant (i-mem-loc scavenge-next-page)))
     ;; Close to end, make sure we have the latest and greatest free-pointer for comparisons.
        (call-xct-next load-scavenge-region-free-pointer)   ;transparent to q-r.
       ((m-k) a-scavenge-region)
        (jump-less-or-equal q-r a-scavenge-region-free-pointer scavenge-block)
     ;; Really on last page in region, scavenge the partial page.
        (jump-xct-next scavenge-block)
       ((a-scavenge-block-bound) a-scavenge-region-free-pointer)


;;; Careful scavenging.

scavenge-region-carefully
scavenge-carefully-page-loop
        (call-greater-or-equal m-1 a-scavenge-region-free-pointer scavenge-check-region-free-pointer)
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number m-1)
        (jump-less-than-xct-next m-tem a-scavenge-volatility scavenge-carefully-page-loop)
       ((m-1) add m-1 (a-constant 400))
        ((m-1) sub m-1 (a-constant 400))                   ;Undo superfluous add.

     ;; We found a volatile page.  We have to scavenge the entire containing object, because
     ;; we can't trust the structure handles to indicate boxedness or unboxedness.  Search back
     ;; through pages until we find the start of the object.  Note: objects in pdl areas are
     ;; always allocated on page boundaries, to simplify this code (make-stack-group enforces
     ;; this constraint).
     ;; M-1 is at the first word of a volatile page.
scavenge-carefully-find-header
        (call-xct-next read-structure-handle)
       ((m-3) q-page-number m-1)
     ;; If <first-header> (M-5) = 0, the structure starts on this page.
        (jump-not-equal-xct-next m-5 a-zero scavenge-carefully-find-header)
       ((m-1) sub m-1 (a-constant 400))
        ((m-1) add m-1 (a-constant 400))                   ;Undo superfluous subtract.

     ;; Now compute the size and current boxed/unboxed boundary of the object.
     ;; M-1 points to the header of a structure that contains volatile pointers.
        (call-xct-next structure-info)
       ((m-t) q-pointer m-1)
        ((a-scavenge-object) q-pointer m-1)
        ((a-scavenge-block-bound) add m-1 a-3)
        ((a-scavenge-next-object) add a-scavenge-block-bound m-4)

        ((m-tem) dpb m-zero q-all-but-pointer-within-page a-scavenge-next-object)
        (call-not-equal m-tem a-zero illop)     ;pdl not allocated on block boundary.
                        ;This can also be caused if PDL is forwarded, but BODY-FORWARD
                        ;Qs somehow get stored in.

     ;; Clear volatility of every page in the object (including unboxed words) to zero.
     ;; SCAVENGE-BLOCK will raise the volatilities to their correct values.
scavenge-carefully-update-volatilities
        (jump-greater-or-equal m-1 a-scavenge-next-object scavenge-object-carefully)
        ((a-gc-trap-vma) m-1)
        (call-xct-next update-page-volatility)
       ((a-gc-trap-md-volatility) m-zero)
        (jump-xct-next scavenge-carefully-update-volatilities)
       ((m-1) add m-1 (a-constant 400))

scavenge-object-carefully
     ;; If we suspend scavenging during this invocation of scavenge-block, we need to
     ;; recompute the size of the object before we resume, because the boxed/unboxed
     ;; boundary may have changed (the pdl-pointer may have changed).  This is the
     ;; raison d'etre of "careful scavenging".
        ((a-scavenge-state) (a-constant 3))
        ((a-scavenge-object-bound) m-minus-one)
        (call-xct-next scavenge-block)
       ((m-1) a-scavenge-object)
        (jump-xct-next scavenge-region-carefully)
       ((m-1) a-scavenge-next-object)

;;; Scavenge block.

scavenge-block-next-object
        ((m-1) a-scavenge-next-object)
        ((a-scavenge-object-bound) m-minus-one)
scavenge-block
scavenge-block-loop
     ;; It's important to test for block boundary before object boundary.  Also, note
     ;; that skipping over unboxed words can take M-1 past block boundary, so test for
     ;; greater-or-equal.
        (popj-greater-or-equal m-1 a-scavenge-block-bound)
        (jump-equal m-1 a-scavenge-object-bound scavenge-block-next-object)
        ((vma-start-read) m-1)
        (check-page-read)
        ((a-scavenge-work) add m-minus-one a-scavenge-work)
        ((A-SCAVENGE-SCAN-POINTER) ldb M-1 q-pointer a-zero)
        ((M-1) ADD M-1 (A-CONSTANT 1))
        (dispatch q-data-type d-scavenge-word md)
        (dispatch transport-scav md)
        ((vma-start-write) vma)
        (check-page-write-force)
        (gc-write-test-volatility)
        (jump-equal a-scavenge-idle-flag a-v-true scavenge-maybe-suspend-idle-scavenging)
        (jump-less-than m-zero a-scavenge-work scavenge-block-loop)
;;; Debug this sometime, for now, just go back to the old version.
;;        (jump-less-than-XCT-NEXT m-zero a-scavenge-work scavenge-block-loop)
;;       ((M-1) M+A+1 M-ZERO A-SCAVENGE-SCAN-POINTER)

     ;; Completed scavenge work quantum -- suspend scavenge-block in a resumable way.
        (jump-xct-next suspend-scavenge)
       ((a-scavenge-block-return-pc) micro-stack-data-pop)

scavenge-maybe-suspend-idle-scavenging
     ;; Suspend if we waited for the disk at all.
        (jump-equal a-scavenger-start-disk-time a-disk-wait-time scavenge-block-loop)
        ((a-scavenge-work) a-zero)
        (jump-xct-next suspend-scavenge)
       ((a-scavenge-block-return-pc) micro-stack-data-pop)

scavenge-block-header
     ;; Quickly check for things that are guaranteed to be boxed -- we can simply return and
     ;; work our way through them.
        ((m-tem) (lisp-byte %%header-type-field) md)
        (popj-equal m-tem (a-constant (eval %header-type-list)))           ;Lists are all boxed.
        (popj-equal m-tem (a-constant (eval %header-type-array-leader)))   ;Leaders are boxed to the header.
        (jump scavenge-block-general-object)

scavenge-block-array-header
     ;; Simply popj for ART-Q arrays as above, since they're always all boxed.
        ((m-tem) (lisp-byte %%array-type-field) md)
        (popj-equal m-tem (a-constant (eval (lsh art-q array-type-shift))))
     ;; Fall through for general arrays.

scavenge-block-general-object
     ;; Back up one since we pre-incremented in the loop.
        ((m-1) add m-minus-one a-1)
        (call-xct-next structure-info)
       ((m-t) q-pointer m-1)
     ;; Restart reference, since VMA/MD probably lost in STRUCTURE-INFO.
        ((vma-start-read) m-1)
        (check-page-read)
     ;; If there are no unboxed words, can go until block bound or until next header.
     ;; Remember to increment M-1 before returning.
        (popj-equal-xct-next m-4 a-zero)
       ((m-1) add m-1 (a-constant 1))

     ;; Boxed/unboxed boundary := M-1 + Boxed-words - 1.  -1 is because we just incremented.
        ((m-tem) add m-1 a-3)
        (popj-after-next
          (a-scavenge-object-bound q-r) add m-minus-one a-tem)
       ((a-scavenge-next-object) add q-r a-4)

(locality d-mem)
(start-dispatch 5 0)
d-scavenge-word
        (n-bit scavenge-block-loop)                    ;trap (immediate) (allowed in PDLs and SGs)
        (p-bit r-bit)                                  ;null
        (n-bit scavenge-block-loop)                    ;unreconciled.
        (p-bit r-bit)                                  ;symbol
        (p-bit r-bit)                                  ;symbol-header (all boxed)
        (n-bit scavenge-block-loop)                    ;fix (immediate)
        (p-bit r-bit)                                  ;extended-number
        (p-bit n-bit scavenge-block-header)            ;header
        (p-bit n-bit illop)                            ;gc-forward (never scavenge oldspace)
        (p-bit r-bit)                                  ;external-value-cell-pointer
        (p-bit r-bit)                                  ;one-q-forward
        (p-bit r-bit)                                  ;header-forward
        (p-bit r-bit)                                  ;body-forward
        (p-bit r-bit)                                  ;locative
        (p-bit r-bit)                                  ;list
        (n-bit scavenge-block-loop)                    ;u code entry (immediate)
        (p-bit r-bit)                                  ;fef
        (p-bit r-bit)                                  ;array-pointer
        (p-bit n-bit scavenge-block-array-header)      ;array-header
        (p-bit r-bit)                                  ;stack-group
        (p-bit r-bit)                                  ;closure
        (n-bit scavenge-block-loop)                    ;indexed-forward
        (p-bit r-bit)                                  ;select-method
        (p-bit r-bit)                                  ;instance
        (p-bit r-bit)                                  ;instance-header (all boxed)
        (p-bit r-bit)                                  ;entity
        (p-bit n-bit illop)                            ;unused-32
        (n-bit scavenge-block-loop)                    ;self-ref-pointer (immediate)
        (n-bit scavenge-block-loop)                    ;character (immediate)
        (p-bit r-bit)                                  ;rplacd-forward
        (p-bit n-bit illop)                            ;36
        (n-bit scavenge-block-loop)                    ;small-flonum (immediate)
(end-dispatch)
(locality i-mem)

;;;

finish-scavenge
        (jump-not-equal m-minus-one a-scavenge-queue-pointer finish-scavenge-not-quite-done)
        ((a-scavenge-state) m-zero)
        (jump-xct-next scavenge-restore)
       ((a-gc-flip-ready) a-v-true)

finish-scavenge-not-quite-done
     ;; All regions seem scavenged, but there are still objects on the scavenge queue.
     ;; Run the queue to completion (won't take long), then loop through all the regions
     ;; again, just to make sure.
        (call-not-equal-xct-next m-minus-one a-scavenge-queue-pointer run-scavenge-queue)
       ((a-scavenge-queue-work) dpb m-minus-one q-pointer a-zero)
        (jump scavenge-regions)

;;;

(begin-comment) Zwei Lossage (end-comment)

load-region-scavenge-parameters
        ((m-k) a-scavenge-region)
        ((vma-start-read) add m-k a-v-region-origin)
        (illop-if-page-fault)
        ((a-scavenge-region-origin) q-pointer md)
        ((vma-start-read) add m-k a-v-region-gc-pointer)
        (check-page-read)
        ((m-1) output-selector-mask-25 add md a-scavenge-region-origin)
load-scavenge-region-free-pointer
   ;must be transparent to q-r.  This is OK since no page faults possible
        (popj-equal-xct-next m-k a-copy-cons-cache-region)
       ((a-scavenge-region-free-pointer) dpb m-zero q-all-but-pointer a-copy-cons-cache-free-pointer)
        (popj-equal-xct-next m-k a-active-cons-cache-region)
       ((a-scavenge-region-free-pointer) dpb m-zero q-all-but-pointer a-active-cons-cache-free-pointer)
        ((vma-start-read) add m-k a-v-region-free-pointer)
        (illop-if-page-fault)
        (popj-after-next)
       ((a-scavenge-region-free-pointer) output-selector-mask-25 add md a-scavenge-region-origin)

;;; This is an interesting hack.  During all the various loops in the scavenger, we're continually
;;; checking M-1 (the scavenge pointer) against a-scavenge-region-free-pointer to see if we're done.
;;; Since there is often consing going on in regions (particularly copy regions!) as we scavenge
;;; them, the region-free-pointer is moving, but we don't notice it cause we only set up
;;; a-scavenge-region-free-pointer at the beginning.  This can cause the scavenger to move on to other
;;; regions before truly finishing one, which is OK since we will come back to it, but it is slow.
;;; To remedy this, test for the end of a region with:
;;;    (call-greater-or-equal m-1 a-scavenge-region-free-pointer scavenge-check-region-free-pointer)
;;; which will update a-scavenge-region-free-pointer based on the latest information.  The interesting
;;; part is that it only returns to the caller if there are more words to be scavenged in this region.
;;; Otherwise, it jumps to SCAVENGE-NEXT-REGION, fixing up the microstack appropriately.  Nifty, eh?

scavenge-check-region-free-pointer
        (call-xct-next load-scavenge-region-free-pointer)
       ((m-k) a-scavenge-region)
        (popj-less-than m-1 a-scavenge-region-free-pointer)
        (jump-xct-next scavenge-next-region)
       ((m-garbage) micro-stack-data-pop)

store-region-gc-pointer
        ((vma) a-scavenge-region)
        ((vma) add vma a-v-region-gc-pointer)
  ;     ((md) q-pointer m-1 (a-constant (byte-value q-data-type dtp-fix)))
        (popj-after-next
          (md-start-write) sub m-1 a-scavenge-region-origin)
       (check-page-write-no-interrupt)

(locality a-mem)
a-scavenge-save-a (0)
a-scavenge-save-b (0)
a-scavenge-save-k (0)
a-scavenge-save-t (0)
a-scavenge-save-1 (0)
a-scavenge-save-2 (0)
a-scavenge-save-3 (0)
a-scavenge-save-4 (0)
a-scavenge-save-5 (0)
a-scavenge-save-6 (0)
(locality i-mem)

scavenge-save
#-lambda(begin-comment)
        ((a-scavenger-start-time) stat-counter-aux)
        ((a-scavenger-start-disk-time) a-disk-wait-time)
        ((a-scavenger-transporter-start-time) a-transporter-time)
        ((a-scavenger-transporter-start-disk-time) a-transporter-disk-time)
#-lambda(end-comment)
        (call-xct-next turn-on-scavenger-run-light)
       ((m-scavenge-flag) dpb m-minus-one a-flags)
        ((a-scavenge-save-a) m-a)
        ((a-scavenge-save-b) m-b)
        ((a-scavenge-save-k) m-k)
        ((a-scavenge-save-t) m-t)
        ((a-scavenge-save-1) m-1)
        ((a-scavenge-save-2) m-2)
        ((a-scavenge-save-3) m-3)
        ((a-scavenge-save-4) m-4)
        (popj-after-next
          (a-scavenge-save-5) m-5)
       ((a-scavenge-save-6) m-6)

suspend-scavenge
        (call store-region-gc-pointer)
scavenge-restore
#-lambda(begin-comment)
        ((m-tem) sub stat-counter-aux a-scavenger-start-time)
     ;; Subtract out time spent in transporter.
        ((m-tem) add m-tem a-scavenger-transporter-start-time)
        ((m-tem) sub m-tem a-transporter-time)
        ((a-scavenger-time) add m-tem a-scavenger-time)
        ((m-tem) a-disk-wait-time)
        ((m-tem) sub m-tem a-scavenger-start-disk-time)
     ;; Subtract out time spent in transporter.
        ((m-tem) add m-tem a-scavenger-transporter-start-disk-time)
        ((m-tem) sub m-tem a-transporter-disk-time)
        ((a-scavenger-disk-time) add m-tem a-scavenger-disk-time)
#-lambda(end-comment)
        ((m-scavenge-flag) dpb m-zero a-flags)
        (call turn-off-scavenger-run-light)
        ((m-a) a-scavenge-save-a)
        ((m-b) a-scavenge-save-b)
        ((m-k) a-scavenge-save-k)
        ((m-t) a-scavenge-save-t)
        ((m-1) a-scavenge-save-1)
        ((m-2) a-scavenge-save-2)
        ((m-3) a-scavenge-save-3)
        ((m-4) a-scavenge-save-4)
        (popj-after-next
          (m-5) a-scavenge-save-5)
       ((m-6) a-scavenge-save-6)

turn-on-scavenger-run-light
        ((vma) a-disk-run-light)
        ((vma-start-read) sub vma (a-constant 2))
        (check-page-read-map-reload-only)
        ((m-tem) md)
        ((md-start-write) dpb m-minus-one (byte 20 0) a-tem)
        (check-page-write-map-reload-only)
     ;; Turn off user run light.
        ((vma) add vma (a-constant 4))
        (popj-after-next
          (md-start-write) m-zero)
       (check-page-write-map-reload-only)

turn-off-scavenger-run-light
        ((vma) a-disk-run-light)
        ((md) m-zero)
        ((vma-start-write) sub vma (a-constant 2))
        (check-page-write-map-reload-only)
     ;; Turn on user run light.
        ((vma) add vma (a-constant 4))
        (popj-after-next
          (md-start-write) m-minus-one)
       (check-page-write-map-reload-only)

(begin-comment) Zwei Lossage (end-comment)

;;; Scavenge queue.

;;; The scavenge queue is a small (1024 word) area of memory managed in a stack-like fashion
;;; to do approximately depth-first copying of objects in the transporter (and thus the
;;; scavenger).  Each entry on the queue is one word, consisting of a 25-bit address and a
;;; 7-bit word count.  The address is the address of the next word to be scavenged, the count
;;; is the remaining words in the entry.  When every word is looked at, the entry on the queue
;;; is updated (looking at a word can cause more objects to appear on the queue, so this whole
;;; process must be very reentrant) by incrementing the address and decrementing the word
;;; count.  When the count for an object gets to zero, the object is popped off the queue, and
;;; then that word is looked at (sort of tail-recursively).

;;; Yes, this code is pretty simple.  However, there are some non-trivial interactions with
;;; TRANS-OLD-COPY that you have to be careful about.

;;; This code probably does more scavenging than scavenge block.  If I am reading the code
;;; correctly, every time a q is transported, we come through here via trans-old-copy which
;;; pushes some stuff and calls the scavenge queue.  Unfortunately, the scavenge queue does
;;; quite a bit of memory cycles and hangs on most of them.  This code could use some
;;; pipelining and I think that would have a noticable impact on the cost of scavenging
;;; level 3 and 2.

(locality a-mem)
a-scavenge-queue-pointer (-1)  ;Points to current entry.  Pushing on the queue pre-increments.
a-scavenge-queue-depth (0)     ;How many items are currently active on the queue.
a-scavenge-queue-active (0)    ;Communication with transporter.
a-scavenge-queue-work (0)      ;Number of units remaining in this invocation.
A-SCAVENGE-QUEUE-TOP-ITEM (0)  ;PROVIDED FOR pipelining purposes, work from here rather than from queue.
(locality i-mem)

(begin-pagable-ucode)

;;; Called at the end of TRANS-OLD-COPY to put the copied object on the scavenge-queue and
;;; to do some scavenge-queue work.  This can use no accumulators.


transport-run-scavenge-queue

;;;Merge from LAD on 9-23-86 (mrc)
        ;; Bug trap to catch pointers to oldspace pushed on scavenge queue.
;       ((md) q-pointer a-transport-new-object-base)
;       (no-op)
;       (call-if-bit-clear l2-map-oldspace-meta-bit illop)     ;backwards flag

   ;If the object is bigger than 77 q's, we put only the first 77 on the queue.
   ;The scavenge queue is not so important as to waste time figuring out how to
   ;improve locality of large objects.

        ((m-tem) a-transport-boxed-size)
        (jump-less-than m-tem (a-constant 77) transport-run-scavenge-queue-push-object)
        ((m-tem) (a-constant 77))

transport-run-scavenge-queue-push-object
        ((md) dpb m-tem (byte 6 31) a-transport-new-object-base)       ;Put count into high bits.
        ((m-tem) m+a+1 m-zero a-scavenge-queue-pointer)                ;Compute new pointer.
        (jump-less-than m-tem
                        a-scavenge-queue-maximum-depth dont-reset-to-zero) ;Reset pointer to 0 if too high.
        ((m-tem) a-zero)

dont-reset-to-zero
        ((m-tem1) a-scavenge-queue-depth)
        (jump-greater-or-equal                                          ;-xct-next
              m-tem1 a-scavenge-queue-maximum-depth already-full)   ;Don't increment if already at max-depth.
        ((a-scavenge-queue-depth) m+a+1 m-zero a-scavenge-queue-depth)  ;Save new value.

already-full
        ((a-scavenge-queue-pointer) m-tem)                             ;Save new pointer.

        ;; Swap MD and TOP ITEM.
        ((M-TEM2) MD)                                                  ;HOLD ONTO NEW ENTRY
        ((MD) A-SCAVENGE-QUEUE-TOP-ITEM)                               ;PUT IN MD WHERE IT NEEDS TO BE
        ((A-SCAVENGE-QUEUE-TOP-ITEM) M-TEM2)                           ;STORE CURRENT ENTRY


        ((vma-start-write) add m-tem a-v-scavenge-queue)               ;Save PREVIOUS entry.
        (check-page-write-unboxed)
        (popj-not-equal m-zero a-scavenge-queue-active)                ;Not reentrant from here on.
        ((pdl-push) a-transport-new-object)                            ;Save these for restarting trap.
        ((pdl-push) a-trans-vma)
        (call-xct-next run-scavenge-queue)
       ((a-scavenge-queue-work) a-transporter-scavenge-queue-work-quantum)
        (popj-after-next
          (a-trans-vma) pdl-pop)
       ((a-transport-new-object) pdl-pop)

;;; Set A-SCAVENGE-QUEUE-WORK to the maximum number of words to look at before calling
;;; RUN-SCAVENGE-QUEUE.  When run-scavenge-queue returns, a-scavenge-queue-work will either
;;; be zero (there may still be entries on the queue) or the initial value minus the number
;;; of words scanned (the queue was exhausted).
;;; You shouldn't depend on this (I don't think anyone does).  If the scavenge queue pointer
;;; is -1, then the queue is empty.  That is all that you need to know.

;;; The scavenge queue is now maintained circularly.  That is, if you push too many things on
;;; the queue, it forgets the oldest entry.  This is not a bug, because the queue is just
;;; an optimization on the linear scavenging.

run-scavenge-queue
        ((a-scavenge-queue-active) (a-constant 1))

scavenge-queue-loop
        (jump-equal m-zero a-scavenge-queue-depth scavenge-queue-exit)  ;Is there any work on queue?
        ((M-TEM2) A-SCAVENGE-QUEUE-TOP-ITEM)                          ;MOVE TO M-MEMORY
        ((M-TEM1) Q-ALL-BUT-POINTER M-TEM2)                            ;WORD COUNT FOR THIS OBJECT.
        (jump-less-or-equal-xct-next m-tem1 (a-constant 1) pop-scavenge-queue)
       ((PDL-PUSH) Q-POINTER M-TEM2)                         ;LOCATION TO BE CHECKED, POPPED BELOW.
        ((m-tem1) sub m-tem1 (a-constant 1))
        ((M-TEM) OUTPUT-SELECTOR-MASK-25 ADD M-TEM2 (A-CONSTANT 1))
        ((A-SCAVENGE-QUEUE-TOP-ITEM) DPB M-TEM1 Q-ALL-BUT-POINTER A-TEM)  ;UPDATE QUEUE BEFORE TRANSPORT.

scavenge-queue-transport
        ((vma-start-read) ldb pdl-pop q-pointer a-zero)
        (check-page-read)
        (JUMP-NOT-EQUAL-XCT-NEXT A-SCAVENGE-SCAN-POINTER VMA SCAVENGE-QUEUE-DISPATCH-TRANSPORT)
       ((a-scavenge-queue-work) add m-minus-one a-scavenge-queue-work)
        ((A-SCAVENGE-SCAN-POINTER) M+A+1 M-ZERO A-SCAVENGE-SCAN-POINTER)

SCAVENGE-QUEUE-DISPATCH-TRANSPORT
        (dispatch transport-scav md)                           ;This might push another object.
        ((VMA-START-WRITE) VMA)
        (CHECK-PAGE-WRITE-force)
        (GC-WRITE-TEST)
        (jump-less-or-equal m-zero a-scavenge-queue-work scavenge-queue-loop)
        ;; Finished the quantum, go away.
        (popj-after-next)
       ((a-scavenge-queue-active) m-zero)

scavenge-queue-exit
        (popj-after-next
          (a-scavenge-queue-pointer) (a-constant -1))   ;We are done.
       ((a-scavenge-queue-active) m-zero)

pop-scavenge-queue
        ((a-scavenge-queue-pointer) add m-minus-one a-scavenge-queue-pointer)
        ((m-tem) a-scavenge-queue-pointer)
        ((vma-start-read) add m-tem a-v-scavenge-queue)
        (CHECK-PAGE-READ)
        (jump-less-or-equal m-zero a-scavenge-queue-pointer dont-reset-to-max-depth)    ;If zero, wrap-backwards
        ((a-scavenge-queue-pointer) add m-minus-one a-scavenge-queue-maximum-depth)     ;last element of queue
dont-reset-to-max-depth
        ((a-scavenge-queue-depth) add m-minus-one a-scavenge-queue-depth)       ;Decrement depth.
        (jump-xct-next scavenge-queue-transport)
       ((a-scavenge-queue-top-item) md)                                      ;PUT NEW TOP ITEM INTO VARIABLE

(end-pagable-ucode)


;;; Scanning on swap-out.

;;; All reference volatilities, if left to their own devices, will rise monotonically to
;;; most-volatile and stay there, negating all the benefit of the scheme.  To prevent this from
;;; happening, pages are scanned at various times to determine their correct volatility -- the
;;; volatility of the most-volatile pointer on the page.
;;;
;;; Note that there are two basic ways that more-volatile pointers can be removed from a page.
;;; Lisp program activity can overwrite volatile pointers (I think this is pretty rare).  More
;;; importantly, when the scavenger scans a page that has pointers of volatility N, by definition
;;; when the scavenger is done the maximum possible volatility is N-1.  This principle also
;;; applies in a probabilistic way to the localizer.
;;;
;;; The effect of letting the reference volatility of a page rise higher than it needs to be
;;; is that at some point the scavenger will examine it unnecessarily.  Examining a page that
;;; already resides in primary memory is not too painful (more on that later), but if the page
;;; has been swapped out to the disk, the overhead of swapping it back in, only to find out that
;;; it wasn't really needed, is unacceptable.  Therefore, we try to ensure that non-resident pages
;;; always have their correct volatility.
;;;
;;; The best way to implement this is to scan every modified page as it is being written back to
;;; the disk in preparation for reuse of a page frame.  There is a problem with this: we (theoretically)
;;; can not scan a page without the aid of the structure handles to decipher the object boundaries
;;; on a page.  Unfortunately, the structure handles are not and can not be wired; since the scanning
;;; occurs deep inside the page fault handler, accessing the structure handles would require
;;; arbitrary levels of recursion in the hard-fault handler.
;;;
;;; Without the aid of the structure handles, we cannot avoid looking at untagged storage.  It
;;; turns out that this is actually acceptable -- as long as we're not depending on the actual
;;; meaning of words of memory, we can scan through looking for volatile "pointers" without danger.
;;; Of course, the results are only probabilistic.  However, measurements indicate that we get the
;;; correct answer between 98-99% of the time, which is perfectly reasonable.
;;;
;;; The overall effect of this is to reduce the disk traffic required to scavenge levels 1, 2 and 3
;;; if the pages have had a chance to migrate to the disk.  In practice, almost all storage involved in
;;; level 3 flips is in core anyway, so this scheme really only affects level 1 and 2 flips.  The
;;; overall gain appears to be a 2x - 3x reduction in paging during level 1 and 2 GCs.
;;;
;;; I think it is moderately important to try to keep the volatility of resident pages correct.
;;; Moon thinks it's unnecessary (he says the 3600 has scanning hardware to scan pages in about
;;; 80 microseconds).  The Lambda, however, takes about 500 microseconds to scavenge a page.
;;; Fortunately, a small amount of overhead in the scavenger (setting the volatility of a page
;;; to zero before scanning, then reading and writing every word to take transport and volatility
;;; traps) can guarantee that the page will have the correct volatility.  I think this small overhead
;;; (figure about 20% of the cost of scavenging a page) is worth the effort -- it prevents pages
;;; from being needlessly scavenged 2, 3, 4... times.

;;; VOLATILITY-SCAN-PAGE is called for every page written to the disk by DISK-SWAP-HANDLER.  It
;;; runs in parallel with the disk (primarily with the seek, so I don't think bus bandwidth is
;;; a problem).  It may take map faults and pdl-buffer faults, but obviously can't take any
;;; hard faults.  We do a GC-WRITE-TEST on every word on the page, but the only action we can
;;; take as a result is to update the volatility.  If an untagged word happens to have a pointer
;;; data-type and a pointer field that points to volatile storage, then we will get a too-high
;;; answer.  This doesn't happen too often, but just to help limit the lossage, we don't let the
;;; final volatility rise above the initial volatility.

;;; Post-mortem:
;;; There were some initial problems with this (it made pages be modified even after they
;;; were written to the disk, it changed pages' swap-status to normal) that caused performance
;;; to degrade dramatically when I first installed this.  After fixing all the (known)
;;; problems with it, we ran some benchmarks which showed that there was really no effect
;;; on performance.  I don't know why, but for now I'm deleting it.

;;; Post-post-mortem:
;;; It appears that a bug was introduced in the system that seriously affected the paging
;;; performance of the machine.  This went unfixed for a very long time.  During this time,
;;; the benchmarks for the volatility scanning were run and showed no effect.  Now, the paging
;;; bug is fixed and I'm uncommenting the code to see if it wins.

;;; Page (virtual-address) in vma.
volatility-scan-page
     ;; If bit 4 of disk switches is 1, scanning is disabled.
        ((q-r) a-disk-switches)
        (popj-if-bit-set (byte 1 4) q-r)
        ((pdl-push) m-1)
        ((pdl-push) m-t)
     ;; Save virtual page address for volatility update below.
        ((a-gc-trap-vma) vma)
     ;; Read current page volatility and save on stack.
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number vma)
        (jump-equal m-tem a-zero volatility-scan-page-abort)
        ((pdl-push) m-tem)
     ;; Set reference volatility of this page down to zero.
        (call-xct-next update-page-volatility)
       ((a-gc-trap-md-volatility) m-zero)
     ;; M-1 gets first word of page, VMA gets first word of next page.  Count VMA down
     ;; to M-1, GC-WRITE-TESTING every word.  Read first word to map page in, then change
     ;; map access to read/write.
        (call-xct-next search-page-hash-table)
       ((m-t) a-gc-trap-vma)
        ((pdl-push) vma)
        ((pdl-push) md)
        ((vma-start-read m-1) a-gc-trap-vma)
        (check-page-read)
        ((md) m-1)
        (no-op)
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control
          m-tem) ior l2-map-control     ;Force read/write access
                (a-constant (byte-value map2c-access-code 3)))
        ((vma) add m-1 (a-constant 400))
volatility-scan-page-loop
        ((vma-start-read) sub vma (a-constant 1))
        (illop-if-page-fault)
        ((vma-start-write) vma)
        (illop-if-page-fault)
        (jump-not-equal-xct-next vma a-1 volatility-scan-page-loop)
       (gc-write-test-volatility)
        ((md) pdl-pop)
        ((vma-start-write) pdl-pop)
        (illop-if-page-fault)
        ((#+lambda l2-map-control
          #+exp vma-write-l2-map-control) (a-constant 0))
     ;; Read current volatility of page.  If current volatility is greater than what it
     ;; started with, update volatility to old value.  This helps minimize randomness.
        (call-xct-next read-page-volatility)
       ((m-lam) q-page-number m-1)
        ((a-gc-trap-vma) m-1)
        ((a-gc-trap-md-volatility) pdl-pop)
        (call-greater-than m-tem a-gc-trap-md-volatility update-page-volatility)
volatility-scan-page-abort
        (popj-after-next (m-t) pdl-pop)
       ((m-1) pdl-pop)

))

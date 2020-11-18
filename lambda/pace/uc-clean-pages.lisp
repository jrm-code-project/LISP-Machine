;;; -*- Mode:LISP; Package:MICRO; Base:8; Readtable:ZL -*-

(defun total-processor-utilization (&aux (total 0))
  (without-interrupts
    (dolist (ape si:active-processes)
      (when (car ape)
        (incf total (send (car ape) :percent-utilization)))))
  (values (round total)))

(defun machine-is-idle-p ()
  (and (> (time-difference (time)
                           tv:kbd-last-activity-time)
          (* 5 60.))
       (< (total-processor-utilization)
          20.)))

(defvar *sched-clean-pages-last-time-run* 0)
(defvar *clean-pages-run-light* #o37777777770)

(defun sched-clean-pages ()
  (when (> (time-difference (time)
                            *sched-clean-pages-last-time-run*)
           60.)
    (cond ((machine-is-idle-p)
           (%P-STORE-TAG-AND-POINTER (+ si:%DISK-RUN-LIGHT 16.) -1 -1)
           (gc::idle-scavenge si:gc-idle-scavenge-quantum)
           (cond ((%clean-pages)
                  (setq *clean-pages-run-light* (logand #o37777777777
                                                        (+ (ash *clean-pages-run-light* 3) 7)))
                  (when (= *clean-pages-run-light* #o37777777777)
                    (setq *clean-pages-run-light* #o37777777770))
                  (%p-dpb (ldb (byte 16. 0) *clean-pages-run-light*)
                          (byte 16. 0)
                          (+ si:%disk-run-light 14.))
                  (%p-dpb (ldb (byte 16. 16.) *clean-pages-run-light*)
                          (byte 16. 16.)
                          (+ si:%disk-run-light 14.)))
                 (t
                  (%P-STORE-TAG-AND-POINTER (+ si:%DISK-RUN-LIGHT 14.) 0 0))))
          (t
           (%P-STORE-TAG-AND-POINTER (+ si:%DISK-RUN-LIGHT 16.) 0 0)
           (%P-STORE-TAG-AND-POINTER (+ si:%DISK-RUN-LIGHT 14.) 0 0)
           (setq *sched-clean-pages-last-time-run* (time))))))

(define-micro-function %clean-pages ()
 (locality a-mem)
a-clean-pages-scan-pointer (0)
a-clean-pages-steps (0)
 (locality i-mem)
        ((a-clean-pages-steps) setz)
        ((m-b) a-clean-pages-scan-pointer)
clean-pages-find-dirty-page
        ((m-tem) m+a+1 m-zero a-clean-pages-steps)
        (jump-equal m-tem (a-constant 200) clean-pages-done)
        ((a-clean-pages-steps) m-tem)
        ((m-b) add m-b (a-constant 1))
        (CALL-GREATER-OR-EQUAL M-B A-V-PHYSICAL-PAGE-DATA-VALID-LENGTH clean-pages-ppd-wrap)
        ((a-clean-pages-scan-pointer) m-b)
        ((VMA-START-READ) ADD M-B A-V-PHYSICAL-PAGE-DATA)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) (BYTE-FIELD 20 0) MD I-LONG)   ;PHT entry index
        (JUMP-EQUAL M-TEM (A-CONSTANT 177777) clean-pages-find-dirty-page)      ;No page here
        ((VMA-START-READ M-T) ADD M-TEM A-V-PAGE-TABLE-AREA)
        (illop-if-page-fault)
        ((m-a) selective-deposit md (byte 17. 8) a-zero)
        ((m-tem) ldb (lisp-byte si:%%pht1-swap-status-code) md)
;       (jump-equal m-tem (a-constant (eval si:%pht-swap-status-wired)) clean-pages-find-dirty-page)
        (jump-not-equal m-tem (a-constant (eval si:%pht-swap-status-flushable)) clean-pages-find-dirty-page)
        (jump-if-bit-set (lisp-byte si:%%pht1-modified-bit) md clean-pages-found-dirty-page)
        ((vma-start-read) add m-t (a-constant 1))
        (illop-if-page-fault)
        ((m-tem) ldb (lisp-byte si:%%pht2-map-status-code) md)
        (jump-equal md (a-constant si:%pht-map-status-read-write) clean-pages-found-dirty-page)
        (jump clean-pages-find-dirty-page)

clean-pages-ppd-wrap
        (POPJ-AFTER-NEXT (M-B) A-ZERO)
       (NO-OP)

clean-pages-found-dirty-page
        ((PDL-PUSH) M-T)                ;PHT1 address. - randomly used below to restore m-t
                                                        ; original use is at coref-ccw-x
        ((A-DISK-SWAP-OUT-CCW-POINTER) (A-CONSTANT #o700))      ;DISK-SWAP-OUT-CCW-BASE
     ;add main memory page frame number in M-B to CCW list.
;#-lambda(begin-comment)
        ((MD) DPB M-B (byte 14. 8) (A-CONSTANT 1))      ;VMA-PHYS-PAGE-ADDR-PART
        ((VMA-START-WRITE) A-DISK-SWAP-OUT-CCW-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((A-DISK-SWAP-OUT-CCW-POINTER)
             m+a+1 M-ZERO A-DISK-SWAP-OUT-CCW-POINTER)
;#-lambda(end-comment)
;#-exp(begin-comment)
;       ((m-t) dpb m-b vma-phys-page-addr-part a-zero)
;       (call translate-cadr-physical-to-nubus)
;       ((md) m-lam)
;       ((vma-start-write) a-disk-swap-out-ccw-pointer)
;       (illop-if-page-fault)
;       ((md) (a-constant 1024.))
;       ((vma-start-write) add vma (a-constant 1))
;       (illop-if-page-fault)
;       ((a-disk-swap-out-ccw-pointer) add vma (a-constant 1))
;       ((m-t) pdl-top)
;#-exp(end-comment)
        ((A-DISK-SAVE-PGF-A) M-A)
        ((A-DISK-SAVE-PGF-B) M-B)
        ((A-DISK-SAVE-1) M-A)
clean-pages-collect-pages
        ((M-T) (A-CONSTANT (EVAL si:PAGE-SIZE)))
        ((A-DISK-SAVE-1) ADD M-T A-DISK-SAVE-1)
        (CALL-XCT-NEXT SEARCH-PAGE-HASH-TABLE)   ;Is next higher page in core?
       ((M-T) A-DISK-SAVE-1)                     ; virt adr in M-T.
                ; clobbers m-a m-b m-t a-tem1 a-tem3
        (JUMP-IF-BIT-CLEAR (lisp-byte si:%%PHT1-VALID-BIT) md clean-pages-got-all-pages) ;not found.
                ;That page in core, does it need to be written?
        ((M-T) VMA)                             ;Save PHT1 adr.
        ((M-A) MD)                              ;Save PHT1.
        ((VMA-START-READ) ADD M-T (A-CONSTANT 1))       ;get PHT2
        (ILLOP-IF-PAGE-FAULT)
        ((M-B) MD)
        (JUMP-IF-BIT-SET (lisp-byte si:%%PHT1-MODIFIED-BIT) M-A clean-pages-add)
        ((m-tem) ldb (lisp-byte si:%%pht2-map-status-code) m-b)
        (jump-not-equal m-tem (a-constant (eval si:%pht-map-status-read-write)) clean-pages-got-all-pages)
clean-pages-add
        ((md m-a) dpb m-zero (lisp-byte si:%%pht1-modified-bit) a-a) ;clear modified flag
        ((VMA-START-WRITE) M-T)
        (ILLOP-IF-PAGE-FAULT)
        ((M-TEM) (lisp-byte si:%%PHT2-MAP-STATUS-CODE) M-B)
        (JUMP-NOT-EQUAL M-TEM (A-CONSTANT 4) clean-pages-add-1)  ;change RW to RWF
        ((M-TEM) (A-CONSTANT 3))
        ((md M-B) DPB M-TEM (lisp-byte si:%%PHT2-MAP-STATUS-CODE) A-B)
        ((VMA-START-WRITE) ADD M-T (A-CONSTANT 1))
        (ILLOP-IF-PAGE-FAULT)
        ((MD) M-A)                              ;address the map
        (no-op)         ;allow time
        ((M-TEM) (byte 3 6) l2-map-control)             ;see if map is set up   L2-MAP-STATUS-CODE
        (JUMP-LESS-THAN M-TEM (A-CONSTANT 2) clean-pages-add-1)
        (CALL-XCT-NEXT LOAD-L2-MAP-FROM-CADR-PHYSICAL)  ;PHT2 same as 2ND LVL MAP(on cadr)
       ((M-LAM) M-B)
clean-pages-add-1
   ;add main memory page frame number in M-B to CCW list.
;#-lambda(begin-comment)
        ((MD) DPB M-B (byte 14. 8) (A-CONSTANT 1))      ;VMA-PHYS-PAGE-ADDR-PART
        ((VMA-START-WRITE) A-DISK-SWAP-OUT-CCW-POINTER)
        (ILLOP-IF-PAGE-FAULT)
        ((A-DISK-SWAP-OUT-CCW-POINTER)
           M+A+1 A-DISK-SWAP-OUT-CCW-POINTER M-ZERO)
;#-lambda(end-comment)
;#-exp (begin-comment)
;       ((m-tem1) m-t)
;       ((m-t) dpb m-b vma-phys-page-addr-part a-zero)
;       (call translate-cadr-physical-to-nubus)
;       ((md) m-lam)
;       ((vma-start-write) a-disk-swap-out-ccw-pointer)
;       (illop-if-page-fault)
;       ((md) (a-constant 1024.))
;       ((vma-start-write) add vma (a-constant 1))
;       (illop-if-page-fault)
;       ((a-disk-swap-out-ccw-pointer) add vma (a-constant 1))
;       ((m-t) m-tem1)
;#-exp(end-comment)
        ((M-TEM) A-DISK-SWAP-OUT-CCW-POINTER)
        (JUMP-LESS-THAN M-TEM (A-CONSTANT #o720) clean-pages-collect-pages)     ;DISK-SWAP-OUT-CCW-MAX
clean-pages-got-all-pages
;#-lambda(begin-comment)
        ((VMA-START-READ) ADD m-minus-one A-DISK-SWAP-OUT-CCW-POINTER )
        (ILLOP-IF-PAGE-FAULT)
        ((MD-START-WRITE) SUB MD (A-CONSTANT 1)) ;last CCW
        (ILLOP-IF-PAGE-FAULT)
;#-lambda(end-comment)
        ((M-A) A-DISK-SAVE-PGF-A)                       ;get back base virt adr.
        ((M-B) A-DISK-SAVE-PGF-B)                       ;get back page frame number of first
                                                        ; page.  It is no longer used by
                                                        ; disk swap handler, but is needed
                                                        ; by COREFOUND2.
        ((PDL-PUSH) M-C)
        ((M-C) (A-CONSTANT #o700))              ;DISK-SWAP-OUT-CCW-BASE
        ((m-tem4) a-disk-swap-out-ccw-pointer)
        ((m-tem4) sub m-tem4 a-c)       ;length of transfer in pages (for hexadec aging hack).
;#+exp  ((m-tem4) ldb (byte-field 31. 1) m-tem4) ;divide by 2
        (CALL-XCT-NEXT DISK-SWAP-HANDLER)               ;Do the write (virt adr in M-A)
       ((M-T) (A-CONSTANT #o11))                ;DISK-WRITE-COMMAND
        ((M-C) PDL-POP)
        ((M-T) PDL-POP)         ;RESTORE PHT ENTRY ADDRESS

        (call clear-regs)

        ((m-t) a-v-true)
        (popj)

clear-regs
        ((M-ZR) setz)
        ((M-A) setz)
        ((M-B) setz)
        ((M-C) setz)
        ((M-D) setz)
        ((M-E) setz)
        ((M-R) setz)
        ((M-Q) setz)
        ((M-I) setz)
        ((M-J) setz)
        ((M-S) setz)
        ((M-K) setz)
        (popj)

clean-pages-done
        ((m-t) a-v-nil)
        (popj)
)

;(defun find-next-dirty-page ()
;  (let ((phys-pages (floor (aref #'sys:system-communication-area sys:%sys-com-memory-size) 256.)))
;    (do ((scan-pointer (mod (+ (sim:read-register 'a-clean-pages-scan-pointer) 1) phys-pages)
;                      (mod (+ scan-pointer 1) phys-pages))
;        (original-scan-pointer (sim:read-register 'a-clean-pages-scan-pointer)))
;       ((= scan-pointer original-scan-pointer)
;        "Can't find any dirty pages")
;      (let ((pht-index (%p-ldb (byte 16. 0) (+ (si:%region-origin si:physical-page-data)
;                                              scan-pointer))))
;       (when (not (= pht-index #o177777))
;         (let ((pht-adr (+ pht-index (si:%region-origin si:page-table-area))))
;           (when (and (= (%p-ldb si:%%pht1-valid-bit pht-adr) 1)
;                      (not (= (%p-ldb si:%%pht1-swap-status-code pht-adr) si:%pht-swap-status-wired))
;                    (or (= (%p-ldb si:%%pht1-modified-bit pht-adr) 1)
;                        (= (%p-ldb si:%%pht2-map-status-code (1+ pht-adr)) si:%pht-map-status-read-write)))
;             (return scan-pointer))))))))


(setf (symbol-function 'si:PROCESS-SCHEDULER-IDLE-FUNCTION) 'sched-clean-pages)

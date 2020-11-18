;;; -*- Mode:LISP; Package:MICRO; Readtable:CL; Base:8 -*-

(defconst %%usim-0-l-offset (byte 4 0))
(defconst %%usim-0-l-base (byte 3 4))
(defconst %%usim-0-r-offset (byte 4 7))
(defconst %%usim-0-r-base (byte 3 11.))
(defconst %%usim-0-d-offset (byte 4 14.))
(defconst %%usim-0-d-base (byte 3 18.))
(defconst %%usim-0-d-whole (byte 7 14.))
(defconst %%usim-0-imm (byte 8 21.))

(defconst %%usim-1-adr (byte 26. 0))
(defconst %%usim-1-aluf (byte 26. 0))

(defconst %%usim-2-cont (byte 3 0))
(defconst %%usim-2-cond (byte 4 3))
(defconst %%usim-2-opcode (byte 4 7))
(defconst %%usim-2-stat (byte 4 11.))
(defconst %%usim-2-halt (byte 1 15.))
(defconst %%usim-2-noop-next (byte 1 16.))
(defconst %%usim-2-uses-alu (byte 1 17.))

(progn
 (define-micro-function %sim-step ()
 (locality m-mem)
m-sim-src-1-ptr (0)
m-sim-src-2-ptr (0)
m-sim-dest-ptr (0)
m-sim-last-src-1 (0)
m-sim-last-aluf (0)
m-sim-inst-0 (0)
m-sim-inst-1 (0)
m-sim-inst-2 (0)
m-sim-open-frame (0)
m-sim-active-frame (0)
m-sim-return-frame (0)

 (locality a-mem)
a-sim-main-memory (0)
a-sim-main-memory-size (0)
a-sim-frames (0)
a-sim-pc (0)
a-sim-next-pc (0)
a-sim-noop-next-bit (0)
a-sim-last-src-2 (0)
a-sim-last-result (0)

a-sim-src-ptrs-valid (0)

a-sim-inst-count (0)

a-sim-free-list (0)
a-sim-free-list-ptr (0)
a-sim-hram (0)

a-sim-inst-counter (0)

a-sim-opc-ram (0)
a-sim-opc-ram-size (0)
a-sim-opc-ram-ptr (0)

a-sim-last-memory-op (0)
a-sim-src-is-func (0)
a-sim-memory-cost (0)

 (locality i-mem)
        ((m-t) a-v-true)
        (call sim-fetch-inst)
        (jump sim-first-time)

sim-next-inst
        ;;new inst already fetched when we get here

        (jump-if-bit-set (lisp-byte %%usim-2-halt) m-sim-inst-2 sim-halt)

sim-first-time
        (jump-not-equal m-zero a-sim-src-ptrs-valid sim-have-srcs)
        ;;copy of get-src-ptrs
        ((a-sim-src-is-func) setz)
        (dispatch-xct-next (lisp-byte %%usim-0-l-base) m-sim-inst-0 d-sim-get-src-ptr)
       ((m-1) ldb (lisp-byte %%usim-0-l-offset) m-sim-inst-0)
        ((m-sim-src-1-ptr) m-1)

        (dispatch-xct-next (lisp-byte %%usim-0-r-base) m-sim-inst-0 d-sim-get-src-ptr)
       ((m-1) ldb (lisp-byte %%usim-0-r-offset) m-sim-inst-0)
        ((m-sim-src-2-ptr) m-1)

        (dispatch-xct-next (lisp-byte %%usim-0-d-base) m-sim-inst-0 d-sim-get-dest-ptr)
       ((m-1) ldb (lisp-byte %%usim-0-d-offset) m-sim-inst-0)
        ((m-sim-dest-ptr) m-1)

sim-have-srcs
        ((a-sim-inst-counter) m+a+1 m-zero a-sim-inst-counter)

        ((a-sim-pc) a-sim-next-pc)      ((a-sim-next-pc) m+a+1 m-zero
a-sim-next-pc)

        ((m-tem) a-sim-opc-ram-size)
        ((m-tem1) m+a+1 m-zero a-sim-opc-ram-ptr)
        (call-greater-or-equal m-tem1 a-tem sim-opc-wrap)
        ((a-sim-opc-ram-ptr) m-tem1)
        ((m-tem) a-sim-noop-next-bit)
        ((md) dpb m-tem (byte 1 31.) a-sim-pc)
        ((vma-start-write) add m-tem1 a-sim-opc-ram)
        (check-page-write)

        ((m-a) a-sim-noop-next-bit)
        ((a-sim-noop-next-bit) setz)
        (jump-not-equal m-a a-zero sim-inst-exit)

        (dispatch-xct-next (lisp-byte %%usim-2-opcode) m-sim-inst-2 d-sim-opcode)
       ((a-sim-src-ptrs-valid) setz)
        (call trap)
    (error-table sim-error unknown-opcode)

sim-opc-wrap
        (popj-after-next (m-tem1) setz)
       (no-op)


sim-halt
        (jump-not-equal m-zero a-sim-noop-next-bit sim-first-time)
        ((m-t) (a-constant (byte-value q-data-type dtp-fix)))
        (popj)

 (locality d-mem)
 (start-dispatch 4 0)
d-sim-opcode
        (sim-alu-op)                            ;0
        (sim-jump-op)                           ;1
        (p-bit r-bit)                           ;2
        (sim-execute-open)                      ;3
        (sim-execute-tail-recursive-open)       ;4
        (sim-execute-call)                      ;5
        (sim-execute-tail-recursive-call)       ;6
        (sim-execute-return)                    ;7
        (sim-execute-store-immediate)           ;8
        (sim-execute-tail-recursive-call-indirect)      ;9
        (p-bit r-bit)                           ;10
        (p-bit r-bit)                           ;11
        (p-bit r-bit)                           ;12
        (p-bit r-bit)                           ;13
        (p-bit r-bit)                           ;14
        (p-bit r-bit)                           ;15
 (end-dispatch)
 (locality i-mem)

sim-fetch-inst
        ((vma) a-sim-pc)
        ((vma) dpb vma (byte 30. 2) a-zero)     ;times 4
        ((vma-start-read) add vma a-sim-main-memory)
        (check-page-read)
        ((m-sim-inst-0) md)
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        ((m-sim-inst-1) md)
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        (popj-after-next (m-sim-inst-2) md)
       (no-op)

sim-alu-op
        ((vma-start-read) add m-sim-src-1-ptr a-sim-frames)
        (check-page-read)
        ((m-sim-last-src-1) md)
        ((vma-start-read) add m-sim-src-2-ptr a-sim-frames)
        (check-page-read)
        ((m-sim-last-aluf) ldb (byte 7 0) m-sim-inst-1)
        ((a-sim-last-src-2) md)
        ((oa-reg-low) dpb (byte 7 2) m-sim-last-aluf)
        ((md m-1) m-sim-last-src-1 setz a-sim-last-src-2)
        ((vma-start-write) add m-sim-dest-ptr a-sim-frames)
        (check-page-write)
        (call-not-equal m-zero a-sim-src-is-func sim-check-for-memory-delay)
        (jump-less-than-xct-next m-sim-dest-ptr (a-constant 4096.) sim-inst-exit)
       ((a-sim-last-result) m-1)

        (dispatch (byte 3 0) m-sim-dest-ptr d-sim-func-dest)
        (call trap)
    (error-table sim-error unknown-func-dest)

sim-check-for-memory-delay
        ;;drop-through and xct next on error, otherwise either jump or skip
        (dispatch (byte 3 0) m-sim-src-1-ptr d-sim-func-src)
        (call trap)
    (error-table sim-error bad-func-src-1)
        (dispatch (byte 3 0) m-sim-src-2-ptr d-sim-func-src)
        (call trap)
    (error-table sim-error bad-func-src-2)
        (popj)

 (locality d-mem)
 (start-dispatch 3 0)
d-sim-func-src
        (sim-memory-wait n-bit)                 ;0
        (p-bit r-bit)                           ;1 vma-start-write
        (p-bit r-bit)                           ;2 vma-start-read
        (sim-memory-wait n-bit)                 ;3 md
        (p-bit r-bit)                           ;4 return (dest only)
        (p-bit r-bit n-bit)                     ;5 instruction counter
        (p-bit r-bit)                           ;6
        (p-bit r-bit)                           ;7
 (end-dispatch)
 (locality i-mem)

sim-memory-wait
        ((m-tem) a-sim-inst-counter)
        ((m-tem) sub m-tem a-sim-last-memory-op)
        ((a-sim-last-memory-op) setz)
        ((m-tem1) a-sim-memory-cost)
        ((m-tem) sub m-tem1 a-tem)
        (popj-after-next popj-less-or-equal m-tem a-zero)
       ((a-sim-inst-counter) add m-tem a-sim-inst-counter)

 (locality d-mem)
 (start-dispatch 3 0)
d-sim-func-dest
        (sim-func-dest-vma n-bit)               ;0
        (sim-func-dest-vma-start-write n-bit)   ;1
        (sim-func-dest-vma-start-read n-bit)    ;2
        (sim-func-dest-md n-bit)                ;3
        (p-bit r-bit)                           ;4 return
        (sim-func-dest-instruction-counter n-bit)       ;5
        (p-bit r-bit)                           ;6
        (p-bit r-bit)                           ;7
 (end-dispatch)
 (locality i-mem)

sim-store-vmas
        ((md) a-sim-last-result)
        ((vma) (a-constant (eval (+ 4096. sim:%func-vma))))
        ((vma-start-write) add vma a-sim-frames)
        (check-page-write)
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write)
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write)
        (popj)

sim-func-dest-vma
        (call sim-memory-wait)
        (call sim-store-vmas)
        (jump sim-inst-exit)

sim-func-dest-vma-start-write
        (call sim-memory-wait)
        (call sim-store-vmas)

        ((m-2) a-sim-last-result)
        ((m-2) q-pointer m-2)
        (call-less-than m-2 a-zero trap)
    (error-table sim-error negative-vma)
        (call-greater-or-equal m-2 a-sim-main-memory-size trap)
    (error-table sim-error vma-too-big)

        ((vma) a-sim-frames)
        ((vma-start-read) add vma (a-constant (eval (+ 4096. sim:%func-md))))
        (check-page-read)

        ((m-2) dpb m-2 (byte 30. 2) a-zero)     ;times 4
        ((vma-start-write) add m-2 a-sim-main-memory)
        (check-page-write)

        ((a-sim-last-memory-op) a-sim-inst-counter)
        (jump-if-bit-clear (lisp-byte %%usim-2-uses-alu) m-sim-inst-2 sim-inst-exit)
        ((a-sim-last-memory-op) m+a+1 m-zero a-sim-last-memory-op)
        (jump sim-inst-exit)

sim-func-dest-vma-start-read
        (call sim-memory-wait)
        (call sim-store-vmas)

        ((m-2) a-sim-last-result)
        ((m-2) q-pointer m-2)
        (call-less-than m-2 a-zero trap)
    (error-table sim-error negative-vma)
        (call-greater-or-equal m-2 a-sim-main-memory-size trap)
    (error-table sim-error vma-too-big)

        ((m-2) dpb m-2 (byte 30. 2) a-zero)     ;times 4
        ((vma-start-read) add m-2 a-sim-main-memory)
        (check-page-read)

        ((vma) a-sim-frames)
        ((vma-start-write) add vma (a-constant (eval (+ 4096. sim:%func-md))))
        (check-page-write)

        ((a-sim-last-memory-op) a-sim-inst-counter)
        (jump-if-bit-clear (lisp-byte %%usim-2-uses-alu) m-sim-inst-2 sim-inst-exit)
        ((a-sim-last-memory-op) m+a+1 m-zero a-sim-last-memory-op)
        (jump sim-inst-exit)

        (jump sim-inst-exit)

sim-func-dest-md
        (call sim-memory-wait)
        (jump sim-inst-exit)

sim-func-dest-instruction-counter
        ((a-sim-inst-counter) a-sim-last-result)
        (jump sim-inst-exit)

sim-jump-op
        (dispatch (lisp-byte %%usim-2-cond) m-sim-inst-2 d-sim-jump-cond)
        (call trap)
    (error-table sim-error unknown-jump-condition)

 (locality d-mem)
 (start-dispatch 4 0)
d-sim-jump-cond
        (sim-jump-unc n-bit)                    ;0
        (sim-jump-less-than n-bit)              ;1
        (sim-jump-equal n-bit)                  ;2
        (sim-jump-not-equal n-bit)              ;3
        (sim-jump-greater-than n-bit)           ;4
        (sim-jump-greater-or-equal n-bit)       ;5
        (sim-jump-data-type-equal n-bit)        ;6
        (sim-jump-data-type-not-equal n-bit)    ;7
        (p-bit r-bit)                           ;8
        (p-bit r-bit)                           ;9
        (p-bit r-bit)                           ;10
        (p-bit r-bit)                           ;11
        (p-bit r-bit)                           ;12
        (p-bit r-bit)                           ;13
        (p-bit r-bit)                           ;14
        (p-bit r-bit)                           ;15
 (end-dispatch)
 (locality i-mem)

sim-jump-data-type-equal
        (jump-data-type-equal m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-data-type-not-equal
        (jump-data-type-not-equal m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-not-equal
        (jump-not-equal m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-greater-than
        (jump-greater-than m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-greater-or-equal
        (jump-greater-or-equal m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-less-than
        (jump-less-than m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-equal
        (jump-equal m-sim-last-src-1 a-sim-last-src-2 sim-jump-unc)
        (jump sim-inst-exit)

sim-jump-unc
        ((a-sim-next-pc) dpb m-sim-inst-1 (byte 8 0) a-sim-next-pc)
        (jump-xct-next sim-inst-exit)
       ((a-sim-noop-next-bit) ldb (lisp-byte %%usim-2-noop-next) m-sim-inst-2)

sim-execute-store-immediate
        ((vma) a-sim-pc)
        ((vma) dpb vma (byte 30. 2) a-zero)
        ((vma-start-read) add vma a-sim-main-memory)
        (check-page-read)
        ((vma-start-write) add m-sim-dest-ptr a-sim-frames)
        (check-page-write)
        ((a-sim-last-result) md)
        ((a-sim-pc) a-sim-next-pc)
        ((a-sim-next-pc) m+a+1 m-zero a-sim-next-pc)
        (jump sim-inst-exit)

sim-inst-exit
        (jump-not-equal m-minus-one a-sim-inst-count sim-maybe-stop)
sim-do-next-inst
        (jump-page-fault-or-interrupt-or-sequence-break sim-really-stop)
        ((vma) a-sim-pc)
        ((vma) dpb vma (byte 30. 2) a-zero)     ;times 4
        ((vma-start-read) add vma a-sim-main-memory)
        (check-page-read)
        ((m-sim-inst-0) md)
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        ((m-sim-inst-1) md)
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        (jump-xct-next sim-next-inst)
       ((m-sim-inst-2) md)

sim-maybe-stop
        ((a-sim-inst-count) add m-minus-one a-sim-inst-count)
        (jump-less-than m-zero a-sim-inst-count sim-do-next-inst)

sim-really-stop
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

;;only clobbers M-1
;;this code is copied in the main loop
get-src-ptrs
        ((a-sim-src-is-func) setz)

        (dispatch-xct-next (lisp-byte %%usim-0-l-base) m-sim-inst-0 d-sim-get-src-ptr)
       ((m-1) ldb (lisp-byte %%usim-0-l-offset) m-sim-inst-0)
        ((m-sim-src-1-ptr) m-1)

        (dispatch-xct-next (lisp-byte %%usim-0-r-base) m-sim-inst-0 d-sim-get-src-ptr)
       ((m-1) ldb (lisp-byte %%usim-0-r-offset) m-sim-inst-0)
        ((m-sim-src-2-ptr) m-1)

        (dispatch-xct-next (lisp-byte %%usim-0-d-base) m-sim-inst-0 d-sim-get-dest-ptr)
       ((m-1) ldb (lisp-byte %%usim-0-d-offset) m-sim-inst-0)
        ((m-sim-dest-ptr) m-1)

        ((a-sim-src-ptrs-valid) seto)
        (popj)

 (locality d-mem)
 (start-dispatch 3 0)
d-sim-get-dest-ptr
        (sim-get-src-ptr-open p-bit)
        (sim-get-src-ptr-active p-bit)
        (sim-get-src-ptr-return p-bit)
        (sim-get-src-ptr-global p-bit)
        (sim-get-dest-ptr-func p-bit)
        (sim-src-error p-bit)
        (sim-src-error p-bit)
        (sim-src-error p-bit)
 (end-dispatch)
 (locality i-mem)

        ;;it looks like you can't put 2 dispatch tables next to each other ...
        (no-op)

 (locality d-mem)
 (start-dispatch 3 0)
d-sim-get-src-ptr
        (sim-get-src-ptr-open p-bit)
        (sim-get-src-ptr-active p-bit)
        (sim-get-src-ptr-return p-bit)
        (sim-get-src-ptr-global p-bit)
        (sim-get-src-ptr-func p-bit)
        (sim-src-error p-bit)
        (sim-src-error p-bit)
        (sim-src-error p-bit)
 (end-dispatch)
 (locality i-mem)
sim-src-error
        (call trap)
    (error-table sim-error unknown-register-base)

sim-get-src-ptr-open
        (popj-after-next (m-1) dpb m-sim-open-frame (byte 8 4) a-1)
       (no-op)

sim-get-src-ptr-active
        (popj-after-next (m-1) dpb m-sim-active-frame (byte 8 4) a-1)
       (no-op)

sim-get-src-ptr-return
        (popj-after-next (m-1) dpb m-sim-return-frame (byte 8 4) a-1)
       (no-op)

sim-get-src-ptr-global
        (popj-after-next (m-tem) ldb (lisp-byte %%usim-0-imm) m-sim-inst-0)
       ((m-1) dpb m-tem (byte 8 4) a-1)

sim-get-src-ptr-func
        (popj-after-next (m-1) add m-1 (a-constant 4096.))
       ((a-sim-src-is-func) seto)

sim-get-dest-ptr-func
        (popj-after-next (m-1) add m-1 (a-constant 4096.))
       (no-op)

sim-free-frame-pop
        ((vma m-4) a-sim-free-list-ptr)
        (call-greater-or-equal vma (a-constant 256.) trap)
    (error-table sim-error free-list-exausted)
        ((vma-start-read) add vma a-sim-free-list)
        (check-page-read)
        (popj-after-next (a-sim-free-list-ptr) add m-4 (a-constant 1))
       (no-op)

;;;frame in MD
sim-free-frame-push
        ((vma) add m-minus-one a-sim-free-list-ptr)
        (call-less-than vma a-zero trap)
    (error-table sim-error free-list-overflow)
        ((a-sim-free-list-ptr) vma)
        ((vma-start-write) add vma a-sim-free-list)
        (check-page-write)
        (popj)

sim-execute-open
        (call sim-free-frame-pop)               ;this does the error check
        ((m-1) dpb md (byte 30. 1) a-zero)
        ((m-tem) dpb m-sim-active-frame (byte 8 8) a-sim-open-frame)
        ((m-tem1) ldb (lisp-byte %%usim-0-d-whole) m-sim-inst-0)
        ((md) dpb m-tem1 (byte 7 16.) a-tem)
        ((vma-start-write) add m-1 a-sim-hram)
        (check-page-write)
        (jump-xct-next sim-inst-exit)
       ((m-sim-open-frame) ldb (byte 30. 1) m-1)

sim-execute-tail-recursive-open
        ((vma) dpb m-sim-active-frame (byte 30. 1) a-zero)      ;times 2
        ((vma-start-read) add vma a-sim-hram)
        (check-page-read)
        ((m-1) md)
        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        (call-xct-next sim-free-frame-pop)
       ((m-2) md)
        ((m-3) md)
        ((vma) dpb md (byte 30. 1) a-zero)      ;times 2
        ((md) m-1)
        ((vma-start-write) add vma a-sim-hram)
        (check-page-write)
        ((md) m-2)
        ((vma-start-write) add vma (a-constant 1))
        (check-page-write)
        (jump-xct-next sim-inst-exit)
       ((m-sim-open-frame) m-3)

sim-execute-call
        (call-xct-next sim-fetch-inst)
       ((m-2) m-sim-inst-1)
        (call-xct-next get-src-ptrs)                    ;clobbers m-1
       ((m-3) dpb m-sim-open-frame (byte 30. 1) a-zero)
        ((md) a-sim-next-pc)
        ((vma-start-write) m+a+1 m-3 a-sim-hram)
        (check-page-write)
        ((m-sim-active-frame) m-sim-open-frame)
        (jump-xct-next sim-inst-exit)
       ((a-sim-next-pc) m-2)

sim-execute-tail-recursive-call
        (call-xct-next sim-fetch-inst)
       ((a-sim-next-pc) m-sim-inst-1)
        (call get-src-ptrs)
        (call-xct-next sim-free-frame-push)
       ((md) m-sim-active-frame)
        (jump-xct-next sim-inst-exit)
       ((m-sim-active-frame) m-sim-open-frame)

sim-execute-tail-recursive-call-indirect
        ((vma-start-read) add m-sim-src-1-ptr a-sim-frames)
        (check-page-read)
        ((a-sim-next-pc) md)
        (call sim-fetch-inst)
        (call get-src-ptrs)
        (call-xct-next sim-free-frame-push)
       ((md) m-sim-active-frame)
        (jump-xct-next sim-inst-exit)
       ((m-sim-active-frame) m-sim-open-frame)

sim-execute-return
        (call-xct-next sim-fetch-inst)
       ((m-3) dpb m-sim-active-frame (byte 30. 1) a-zero)
        (call-xct-next get-src-ptrs)
       ((m-sim-return-frame) m-sim-active-frame)
        (call-xct-next sim-free-frame-push)
       ((md) m-sim-active-frame)
        ((vma-start-read) add m-3 a-sim-hram)
        (check-page-read)
        ((m-sim-open-frame) ldb (byte 8 0) md)
        ((m-sim-active-frame) ldb (byte 8 8) md)

        (dispatch-xct-next (byte 3 20.) md d-sim-get-dest-ptr)
       ((m-1) ldb (byte 4 16.) md)
        ((m-sim-dest-ptr) m-1)

        ((vma-start-read) add vma (a-constant 1))
        (check-page-read)
        (jump-xct-next sim-inst-exit)
       ((a-sim-next-pc) md)

;;END
        )
 (when (and (boundp 'sim:*proc*)
            (instancep sim:*proc*))
   (send sim:*proc* :reset))
)

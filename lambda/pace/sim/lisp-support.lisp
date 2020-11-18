;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:ZL -*-
(define-asm %reset-instruction-counter ()
  (alu (func instruction-counter) <- (constant 0) setl (garbage))
  (return-xct-next)
  (alu (func return) <- (constant 'nil) setl (garbage)))

(define-asm %halt ()
  (noop halt)
  (return-xct-next)
  (alu (func return) <- (constant 'nil) setl (garbage)))

(define-asm + ()
  (return-xct-next)
  (alu (func return) <- (active 0) add (active 1)))

;allocate-list-storage-default
;        ((m-s) dpb m-zero q-all-but-typed-pointer a-default-cons-area)
;allocate-list-storage-kernel
;        (jump-not-equal m-s a-active-cons-cache-area allocate-list-storage-uncached)
;        ((a-active-cons-cache-allocation-status q-r) add m-b a-active-cons-cache-allocation-status)
;        (jump-greater-than q-r a-list-allocation-threshold allocate-list-storage-cached-list-header)
;        ((m-3) output-selector-mask-25 add m-b a-active-cons-cache-free-pointer)
;        (jump-greater-than m-3 a-active-cons-cache-free-limit allocate-list-storage-uncached)
;        (popj-after-next
;          (m-t) a-active-cons-cache-free-pointer)
;       ((a-active-cons-cache-free-pointer) m-3)

(defconst *default-cons-area-allocation-status-global-number* #o7000)
(defconst *default-cons-area-cached-free-pointer-global-number* #o7001)
(defconst *default-cons-area-constant-2-global-number* #o7002)
(defconst *default-cons-area-cached-free-pointer-limit-global-number* #o7003)
(defconst *default-cons-area-list-header-global-number* #o7004)
(defconst *default-cons-area-constant-0-global-number* #o7005)
(defconst *default-cons-area-constant-256-global-number* #o7006)
(defconst *default-cons-area-constant-16-global-number* #o7007)
(defconst *default-cons-area-constant-minus-1-global-number* #o7010)

(defun >invalidate-cons-cache ()
  (send *proc* :write-frames *default-cons-area-constant-2-global-number* 2)
  (send *proc* :write-frames *default-cons-area-constant-16-global-number* 16.)
  (send *proc* :write-frames *default-cons-area-constant-0-global-number* 0)
  (send *proc* :write-frames *default-cons-area-constant-256-global-number* 256.)
  (send *proc* :write-frames *default-cons-area-constant-minus-1-global-number* -1)
  (send   *proc* :write-frames *default-cons-area-list-header-global-number*
          (+ (dpb dtp-header %%q-data-type 0)
             (dpb si:%header-type-list si:%%header-type-field 0)))
  (when (not (zerop (send *proc* :read-frames *default-cons-area-cached-free-pointer-limit-global-number*)))
    (send *proc* :write-main-memory *free-pointer*
          (send *proc* :read-frames *default-cons-area-cached-free-pointer-global-number*))
    (send *proc* :write-frames *default-cons-area-cached-free-pointer-limit-global-number* 0)
    t))

(define-asm test-cons ()
  (open (garbage))
  (open (open 0))
  (call-xct-next cons)
  (no-op)

  (open (open 1))
  (call-xct-next cons)
  (no-op)

  (call-xct-next %halt)
  (noop)
  )
#|
(sim-test 'test-cons '())
|#

(define-asm %invalidate-cons-cache ()
  (alu (garbage) <- (global *default-cons-area-cached-free-pointer-limit-global-number*) sub
       (global *default-cons-area-constant-0-global-number*))
  (jump equal done)
  (alu (func md) <- (global *default-cons-area-cached-free-pointer-global-number*) setl (garbage))
  (alu (func vma-start-write) <- (constant *free-pointer*) setl (garbage))
done
  (alu (global *default-cons-area-cached-free-pointer-limit-global-number*) <- setz (garbage) (garbage))
  (return-xct-next)
  (alu (func return) <- setz (garbage) (garbage))
  )

(define-asm cons (car cdr)
cons-restart
  (alu (global *default-cons-area-allocation-status-global-number*) <-
       (global *default-cons-area-allocation-status-global-number*) sub
       (global *default-cons-area-constant-2-global-number*))
  (jump less-than cons-need-list-header)
  (alu (active 2) <- (global *default-cons-area-cached-free-pointer-global-number*) setl (garbage))
check-alignment
  (alu (active 4) <- (active 2) and (constant 1))
  (alu (garbage) <- (active 4) sub (constant 0))
  (jump not-equal cons-need-list-header-already-have-active-2)
  (alu (active 3) <-
       (active 2) add
       (global *default-cons-area-constant-2-global-number*))
  (alu (garbage) <- (active 3) sub (global *default-cons-area-cached-free-pointer-limit-global-number*))
  (jump greater-than cons-uncached)
  (alu (global *default-cons-area-cached-free-pointer-global-number*) <- (active 3) setl (garbage))
store-values
  (alu (func md) <- (active 0) setl (garbage))
  (alu (func vma-start-write) <- (active 2) setl (garbage))
  (alu (func md) <- (active 1) setl (garbage))
  (alu (func vma-start-write) <- (func vma) or (constant 1))
  (return-xct-next)
  (alu (func return) <- (active 2) or (constant #.(dpb dtp-list %%q-data-type 0)))

cons-need-list-header
  (alu (active 2) <- (global *default-cons-area-cached-free-pointer-global-number*) setl (garbage))
cons-need-list-header-already-have-active-2
  (alu (active 3) <- (active 2) l+r+1
       (global *default-cons-area-constant-0-global-number*))
  (alu (garbage) <- (active 3) sub (global *default-cons-area-cached-free-pointer-limit-global-number*))
  (jump greater-or-equal cons-uncached)
  (alu (func md) <- (global *default-cons-area-list-header-global-number*) setl (garbage))
  (alu (func vma-start-write) <- (active 2) setl (garbage))
  (alu (active 2) <- (active 2) add (constant 1))
  (jump-xct-next check-alignment)
 (alu (global *default-cons-area-allocation-status-global-number*) <-
      (global *default-cons-area-constant-16-global-number*) setl
      (garbage))

cons-uncached
  (open (garbage))
  (call-xct-next %invalidate-cons-cache)
  (no-op)
  (alu (func vma-start-read) <- (constant *free-pointer*) setl (garbage))
  (no-op)
  (alu (global *default-cons-area-allocation-status-global-number*) <-
       (global *default-cons-area-constant-minus-1-global-number*) setl (garbage))
  (alu (global *default-cons-area-cached-free-pointer-global-number*) <-
       (func md) setl (garbage))
  (alu (global *default-cons-area-cached-free-pointer-limit-global-number*) <-
       (global *default-cons-area-constant-256-global-number*) setl (garbage))
  (alu (global *default-cons-area-cached-free-pointer-limit-global-number*) <-
       (global *default-cons-area-cached-free-pointer-limit-global-number*) add
       (func md))
  (jump cons-restart)
  (no-op)
  )


;(define-asm cons (car cdr)
;  (alu (func vma-start-read) <- (constant *free-pointer*) setl (garbage))
;  (no-op)
;  (alu (garbage) <- (func md) and (constant 1))
;  (jump not-equal not-aligned)
;  (alu (active 2) <- (func md) setl (garbage))
;  (alu (func md) <- (active 0) setl (garbage))
;  (alu (func vma-start-write) <- (active 2) setl (garbage))
;  (alu (func md) <- (active 1) setl (garbage))
;  (alu (func vma-start-write) <- (func vma) add (constant 1))
;  (alu (func md) <- (func vma) add (constant 1))
;  (alu (func vma-start-write) <- (constant *free-pointer*) setl (garbage))
;  (return-xct-next)
; (alu (func return) <- (active 2) or (constant #.(dpb dtp-list %%q-data-type 0)))
;  )

(define-asm car (cons)
  (alu (func vma-start-read) <- (active 0) setl (garbage))
  (return-xct-next)
 (alu (func return) <- (func md) setl (garbage)))

(define-asm cdr (cons)
  (alu (func vma-start-read) <- (active 0) add (constant 1))
  (return-xct-next)
 (alu (func return) <- (func md) setl (garbage)))

(define-asm = (A B)
  (alu (garbage) <- (active 0) sub (active 1))
  (jump equal yes)
  (return-xct-next)
  (alu (func return) <- (constant 'nil) setl (garbage))
yes
  (return-xct-next)
  (alu (func return) <- (constant 't) setl (garbage))
  )

(define-asm mapcar (func list)
  ;;FUNC      (active 0)
  ;;LIST      (active 1)
  ;;RESULT    (active 2)
  ;;LAST-CONS (active 3)
  (alu (garbage) <- (active 1) sub (constant 'nil))
  (jump equal return-nil)
  ;;open for cons
  (open (active 2))
  ;;open for funcall
  (open (open 0))
  (alu (open 0) <- (active 0) setl (garbage))
  (vma-start-read (active 1))
  (noop)
  (call-xct-next funcall)
  (alu (open 1) <- (func md) setl (garbage))
  (call-xct-next cons)
  (alu (open 1) <- (constant 'nil) setl (garbage))
  (alu (active 3) <- (active 2) setl (garbage))
  (alu (func vma-start-read) <- (active 1) or (constant 1))
  (noop)
  (alu (active 1) <- (func md) (garbage))

  ;;FUNC      (active 0)
  ;;LIST      (active 1)
  ;;RESULT    (active 2)
  ;;LAST-CONS (active 3)
;     again
;       (when (not (consp list))
;         (return result))
;       (rplacd last-cons (cons (funcall func (car list)) nil))
;       (setq last-cons (cdr last-cons))
;       (setq list (cdr list))
;       (go again)
again
  (alu (garbage) <- (active 1) sub (constant #.(dpb dtp-list %%q-data-type 0)))
  (jump data-type-not-equal return-result)
  ;;open for rplacd
  (open (garbage))
  (alu (open 0) <- (active 3) setl (garbage))
  ;;open for CONS
  (open (open 1))
  ;;open for FUNCALL
  (open (open 0))
  (alu (open 0) <- (active 0) setl (garbage))
  (vma-start-read (active 1))
  (noop)
  (call-xct-next funcall)
  (alu (open 1) <- (func md) setl (garbage))
  (call-xct-next cons)
  (alu (open 1) <- (constant 'nil) setl (garbage))
  (call-xct-next rplacd)
  (no-op)
  (alu (func vma-start-read) <- (active 1) or (constant 1))
  (no-op)
  (jump-xct-next always again)
  (alu (active 1) <- (func md) setl (garbage))

return-result
  (return-xct-next)
  (alu (func return) <- (active 2) setl (garbage))

return-nil
  (return-xct-next)
  (alu (func return) <- (constant 'nil) setl (garbage))
  )

(define-asm rplacd (cons new-cdr)
  (alu (garbage) <- (active 0) setz (constant #.(dpb dtp-list %%q-data-type 0)))
  (jump data-type-not-equal not-cons)
  (alu (func md) <- (active 1) setl (garbage))
  (alu (func vma-start-write) <- (active 0) or (constant 1))
  (return-xct-next)
  (alu (func return) <- (active 1) setl (garbage))
not-cons
  (alu halt)
  )

(define-asm funcall (func arg)
  (tail-recursive-open)
  (alu (open 0) <- (active 1) setl (garbage))
  (tail-recursive-call-xct-next-indirect (active 0))
 (no-op)
  )

(define-asm bar (x)
  (open (active 1))
  (alu (open 0) <- (active 0) setl (garbage))
  (alu (open 1) <- (active 0) setl (garbage))
  (call-xct-next cons)
  (no-op)
  (return-xct-next)
  (alu (func return) <- (active 1) setl (garbage)))

(define-asm foo ()
  (open (garbage))
  (open (open 0))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA (function bar))
  (store-immediate boxed (open 1))
  (immediate-data 'test)
  (call-xct-next funcall)
  (no-op)
  (call-xct-next %halt)
  (no-op))

#|
(sim-test 'foo nil)
|#

(define-asm list (args)

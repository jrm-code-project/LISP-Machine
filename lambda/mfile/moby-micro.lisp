;;; -*- Mode: Lisp; Base: 8; Package: (MICRO GLOBAL); Readtable: ZL -*-


;Objects going to or from moby space are transferred uninterruptedly to buffer-area.
;This avoids timing errors if another process comes along and references a partially
;filled in object.

(defvar *buffer-area* (make-area :name 'moby-buffer-area :gc :static))

;assure moby-buffer-area has one region.
(cond ((minusp (si:%area-region-list *buffer-area*))
       (cons-in-area nil nil *buffer-area*)))

(defvar *buffer-region* (si:%area-region-list *buffer-area*))

(define-micro-function mfs:move-object-to-buffer (base boxed-size unboxed-size buffer-region)
        ((m-i) q-pointer pdl-pop)       ;buffer region
        ((m-1) q-pointer pdl-pop)       ;unboxed size
        ((m-2) q-pointer pdl-pop)       ;boxed size
        ((m-a) q-typed-pointer pdl-pop) ;base
        ((vma-start-read) add m-i a-v-region-origin)
        (check-page-read)
        ((m-j) q-pointer md)            ;base of buffer region.
        ((m-k) m-j)                     ;save copy.
   ;doing the entire copy operation before smashing anything to unreconciled
   ; is a bit more conservative..
   ;copy boxed portion
        ((m-3) m-2)             ;copy since will need these for smash phase.
        ((m-s) m-a)
  c-b   ((vma-start-read) m-s)
        (check-page-read)
        (dispatch transport md)
        ((vma-start-write) m-j)
        (check-page-write)
        (gc-write-test)
        ((m-3) sub m-3 (a-constant 1))
        ((m-s) add m-s (a-constant 1))
        ((m-j) add m-j (a-constant 1))
        (jump-not-equal m-3 a-zero c-b)
        (jump-equal m-1 a-zero s-b0)
        ((m-3) m-1)
   ;copy unboxed portion
  c-nb  ((vma-start-read) m-s)
        (check-page-read)
        ((vma-start-write) m-j)
        (check-page-write-unboxed)
        ((m-3) sub m-3 (a-constant 1))
        ((m-s) add m-s (a-constant 1))
        ((m-j) add m-j (a-constant 1))
        (jump-not-equal m-3 a-zero c-nb)
   ;smash boxed portion to unreconciled
  s-b0  ((m-3) m-2)
        ((m-s) m-a)
  s-b   ((md) (a-constant (byte-value q-data-type dtp-unreconciled)))
        ((vma-start-write) m-s)
        (check-page-write)              ;gc-write-test not necessary since no
                                        ; pointer or pdl number involved.
        ((m-3) sub m-3 (a-constant 1))
        ((m-s) add m-s (a-constant 1))
        (jump-not-equal m-3 a-zero s-b)
        (jump-not-equal m-2 a-zero clean-and-exit)
        ((m-3) m-1)
   ;smash unboxed portion to unreconciled
  s-nb  ((vma-start-write) m-s)
        (check-page-write-unboxed)
        ((m-3) sub m-3 (a-constant 1))
        ((m-s) add m-s (a-constant 1))
        (jump-not-equal m-3 a-zero s-nb)
  clean-and-exit
    ;set free pointer of copy region.
        ((md) sub m-s a-k)
        ((vma-start-write) add m-i a-v-region-free-pointer)
        (check-page-write)

        ((md) a-zero)
        ((vma-start-write) add m-i a-v-region-gc-pointer)
        (check-page-write)

    ;if scavenging copy region, reset scavenger.
        (jump-not-equal m-i a-scavenge-region xfalse)
        ((a-scavenge-object) m-zero)
        ((pdl-push) m-i)
        (jump reset-scavenger)
  )

;should check.
; clobber unreconciled with stuff.
; if copy unreconciled, dont change object.
; check if copy and object both not unreconciled and differ.

(define-micro-function mfs:moby-buffer-to-object (base boxed-size unboxed-size buffer-region)
        ((m-i) q-pointer pdl-pop)       ;buffer region
        ((m-1) q-pointer pdl-pop)       ;unboxed size
        ((m-2) q-pointer pdl-pop)       ;boxed size
        ((m-a) q-typed-pointer pdl-pop) ;base
        ((vma-start-read) add m-i a-v-region-origin)
        (check-page-read)
        ((m-j) q-pointer md)            ;base of buffer region.
        ((m-3) m-2)
        ((m-s) m-a)
  c-b   ((vma-start-read) m-j)
        (check-page-read)
        (dispatch transport md)
        ((vma-start-write) m-s)
        (check-page-write)
        (gc-write-test)
        ((m-3) sub m-3 (a-constant 1))
        ((m-s) add m-s (a-constant 1))
        ((m-j) add m-j (a-constant 1))
        (jump-not-equal m-3 a-zero c-b)
        (jump-not-equal m-1 a-zero exit)
        ((m-3) m-1)
  c-nb  ((vma-start-read) m-j)
        (check-page-read)
        ((vma-start-write) m-s)
        (check-page-write-unboxed)
        ((m-3) sub m-3 (a-constant 1))
        ((m-s) add m-s (a-constant 1))
        ((m-j) add m-j (a-constant 1))
        (jump-not-equal m-3 a-zero c-nb)
 exit   (jump xfalse)
  )

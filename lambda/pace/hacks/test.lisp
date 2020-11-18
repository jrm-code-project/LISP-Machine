;;; -*- Mode:LISP; Package:MICRO; Base:8; Readtable:ZL -*-
(define-micro-function increment (a)
  "Increment a."
  ((m-t) m+a+1 pdl-pop a-zero)
  (popj-after-next (m-t) m+a+1 m-t a-zero)
 (no-op)
  )

(define-micro-function add-1-if-odd (num)
  ((m-t) pdl-pop)
  (popj-if-bit-clear (byte 1 0) m-t)
  ((m-t) add m-t (a-constant 1))
  (popj)
  )

(define-micro-function add-1-if-fixnum (num)
  ((m-t) pdl-pop)
  (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix)))
  ((m-t) add m-t (a-constant 1))
  (popj)
  )

(define-micro-function car-if-list (l)
  ((m-t) pdl-pop)
  (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list)))
  ((vma-start-read) m-t)
  (check-page-read)
  (dispatch transport md)
  ((m-t) md)
  (popj))

(define-micro-function stat-counter ()
  ((m-1) stat-counter)
  (jump return-m-1-unsigned))


(define-micro-function reset-and-enable-stat-counters (main-clock main-count aux-clock aux-count)
  ;;stop counters
  ((rg-mode) andca rg-mode (a-constant (eval (+ (dpb -1 (byte 3 0) 0)   ;aux count
                                                (dpb -1 (byte 1 4) 0)   ;aux clock
                                                (dpb -1 (byte 3 20.) 0) ;main count
                                                (dpb -1 (byte 1 24.) 0) ;main clock
                                                ))))
  ((m-1) rg-mode)
  ((m-1) dpb pdl-pop (byte 3 0) a-1)            ;aux count
  ((m-1) dpb pdl-pop (byte 1 4) a-1)            ;aux clock
  ((m-1) dpb pdl-pop (byte 3 20.) a-1)          ;main count
  ((m-1) dpb pdl-pop (byte 1 24.) a-1)
  ((stat-counter) setz)
  ((stat-counter-aux) setz)
  (popj-after-next (m-t) a-v-nil)
 ((rg-mode) m-1)
 )

(define-micro-function stop-and-read-stat-counters ()
  ;;stop counters
  ((rg-mode) andca rg-mode (a-constant (eval (+ (dpb -1 (byte 3 0) 0)   ;aux count
                                                (dpb -1 (byte 1 4) 0)   ;aux clock
                                                (dpb -1 (byte 3 20.) 0) ;main count
                                                (dpb -1 (byte 1 24.) 0) ;main clock
                                                ))))
  ((m-1) stat-counter)
  (call return-m-1-unsigned)
  ((pdl-push) m-t)
  ((m-1) stat-counter-aux)
  (call return-m-1-unsigned)
  ((pdl-push) m-t)
  (call xcons)
  (popj))

;;; -*- Mode:LISP; Package:MICRO; Base:10; Readtable:CL -*-

(defmacro define-micro-memory-functions (memory origin address-lines)
  `(PROGN
     (DEFINE-MICRO-FUNCTION ,(intern (string-append "READ-" memory)) (LOCATION)
       ((M-TEM) LDB ,address-lines PDL-POP A-ZERO)      ;mapped vma byte
       ((VMA-START-READ) ADD M-TEM (A-CONSTANT (EVAL ,origin)))
       (CHECK-PAGE-READ)
       (JUMP-XCT-NEXT RETURN-M-1-UNSIGNED)
      ((M-1) MD))

     (DEFINE-MICRO-FUNCTION ,(intern (string-append "WRITE-" memory)) (LOCATION NEW-VALUE)
       (CALL GET-32-BITS)
       ((MD) M-1)
       ((M-TEM) LDB ,address-lines PDL-POP A-ZERO)
       ((VMA-START-WRITE) ADD M-TEM (A-CONSTANT (EVAL ,origin)))
       (CHECK-PAGE-WRITE)
       (JUMP XFALSE))))

(define-micro-memory-functions bootprom        k::*bootprom-origin*        (byte 13.  0.))
(define-micro-memory-functions memory-map      k::*memory-map-origin*      (byte 16. 10.))
(define-micro-memory-functions physical-memory k::*physical-memory-origin* (byte 22.  0.))
(define-micro-memory-functions register-file   k::*register-frames-origin* (byte 13.  0.))
(define-micro-memory-functions saved-o         k::*saved-o-origin*         (byte 12.  0.))
(define-micro-memory-functions saved-a         k::*saved-a-origin*         (byte 12.  0.))
(define-micro-memory-functions call-heap       k::*call-heap-origin*       (byte 12.  0.))
(define-micro-memory-functions return-pc       k::*return-pc-origin*       (byte 12.  0.))
(define-micro-memory-functions return-dest     k::*return-dest-origin*     (byte 12.  0.))
(define-micro-memory-functions return-global-frame k::*return-global-frame-origin* (byte 12. 0.))

(define-micro-function 32-dpb (value byte destination)
  (declare (:call-as-misc-instruction t))
  (call get-32-bits)
  ((m-2) m-1)

  (call-xct-next get-32-bits)
 ((m-3) pdl-pop)

  ;; Fix up byte spec.
  ((m-tem) ldb m-3 (byte 6. 6.) a-zero)
  ((m-3) sub m-3 (a-constant 1))
  ((m-tem) dpb m-3 (byte 6. 6.) a-tem)

  ((m-tem2) m-2)

  ((oa-reg-low) ldb m-tem (byte 12. 0.) a-zero)
  ((m-1) dpb m-1 (byte 1. 0.) a-tem2)

  (jump return-m-1-unsigned))

(define-micro-function 32-ldb (source byte destination)
  (declare (:call-as-misc-instruction t))
  (call get-32-bits)
  ((m-2) m-1)

  (call-xct-next get-32-bits)
 ((m-3) pdl-pop)

  ;; Fix up byte spec.
  ((m-tem) ldb m-3 (byte 6. 6.) a-zero)         ;m-tem gets byte position
  ((m-tem1) (a-constant #o40))
  ((m-tem) sub m-tem1 a-tem)                    ;Subtract from 40 to get rotation.

  ((m-tem1) ldb m-3 (byte 6. 0.) a-zero)
  ((m-tem1) sub m-tem1 (a-constant 1))

  ((m-tem2) m-2)

  ((oa-reg-low) dpb m-tem1 (byte 6. 6.) a-tem)
  ((m-1) ldb m-1 (byte 1. 0.) a-tem2)

  (jump return-m-1-unsigned))

;; c 16
;; n 17
;; v 18
;; z 19
;; l 20
;; m 21
;; s 22

(define-a-memory-variable a-status-register 0)

(define-micro-function read-status-register ()
  (jump-xct-next return-m-1-unsigned)
 ((m-1) a-status-register))

(define-micro-function write-status-register (bw value)

  (call get-32-bits)

  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-1) and m-1 a-4)
  ((a-status-register) andcm m-4 a-status-register)
  ((a-status-register) ior m-1 a-status-register)
  (jump-xct-next return-m-1-unsigned)
 ((m-1) a-status-register))

(defun show-status-register ()
  (let ((stats (read-status-register)))
    (format t "~&~[ ~;Z~]~
               ~[ ~;V~]~
               ~[ ~;N~]~
               ~[ ~;C~]"
          (ldb (byte 1. 19.) stats)
          (ldb (byte 1. 18.) stats)
          (ldb (byte 1. 17.) stats)
          (ldb (byte 1. 16.) stats))))

(define-micro-function adjust-status-register ()
  ((m-1) a-status-register)
  ((m-tem1) ldb (byte 1. 19.) m-1 a-zero)       ;z
  ((m-tem)  ldb (byte 1. 17.) m-1 a-zero)       ;n
  ((m-tem2) ldb (byte 1. 18.) m-1 a-zero)       ;v
  ((m-tem) xor m-tem2 a-tem)                            ;n xor v
  ((a-status-register) dpb m-tem (byte 1. 14.) a-status-register)

  ((m-tem) ior m-tem a-tem1)                            ;(n xor v)+z
  ((a-status-register) dpb m-tem (byte 1. 15.) a-status-register)

  ((m-tem) ldb (byte 1. 16.) m-1 a-zero)        ;c
  ((m-tem) xor m-minus-one a-tem)               ;c~
  ((m-tem1) ior m-tem a-tem1)                   ;c~+z
  (jump-if-bit-set-xct-next (byte 1. 0.) m-tem2 xtrue)
 ((a-status-register) dpb m-tem1 (byte 1. 13.) a-status-register)
  (jump xfalse))

(define-micro-function zero-ext (bw source)

  (call get-32-bits)

  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-1) and m-4 a-1)

  (jump-equal m-1 a-zero set-z-bit)
  (jump-xct-next compute-n-bit)
 ((a-status-register) dpb m-zero (byte 1. 19.) a-status-register)
 set-z-bit
 ((a-status-register) dpb m-minus-one (byte 1. 19.) a-status-register)

 compute-n-bit
  ((m-tem) dpb pdl-pop (byte 2. 3.) a-zero)
  ((m-tem1) (a-constant 33.))
  ((m-tem) sub m-tem1 a-tem)
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem1) ldb (byte 32. 0.) m-1 a-zero)       ;n bit
  ((a-status-register) dpb m-tem1 (byte 1. 17.) a-status-register)

  (jump return-m-1-unsigned))

(define-micro-function pass-stat (bw right-input)
  (call get-32-bits)

  ((m-tem) dpb pdl-pop (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-2) and m-4 a-status-register)
  ((m-4) xor m-minus-one a-4)
  ((m-1) and m-4 a-1)
  (jump-xct-next return-m-1-unsigned)
 ((m-1) ior m-2 a-1))

(define-micro-function ext-bit (bw-bits position word)

  (call get-32-bits)

  ((m-2) pdl-pop)

  (jump-if-bit-clear (byte 1. 1.) pdl-pop do-ext)
  ((m-tem) a-status-register)
  ((m-2) ldb (byte 6. 0.) m-tem a-zero)

 do-ext
  ((m-tem) dpb m-2 (byte 5. 0.) a-zero)
  ((m-tem1) (a-constant 32.))
  ((m-tem) sub m-tem1 a-tem)
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-1) ldb (byte 1. 0.) m-1 a-zero)

  ((m-tem) ldb (byte 1. 5.) m-2 a-zero)
  ((m-1) xor m-tem a-1)

  ((m-tem) xor m-minus-one a-1)
  ((a-status-register) dpb m-tem (byte 1. 19.) a-status-register)
  (jump-xct-next return-m-1-unsigned)
 ((a-status-register) dpb m-1 (byte 1. 20.) a-status-register))

(define-micro-function set-bit (bw-bits position word)

  (call get-32-bits)

  ((m-2) pdl-pop)

  (jump-if-bit-clear (byte 1. 1.) pdl-pop do-set)
  ((m-tem) a-status-register)
  ((m-2) ldb (byte 6. 0.) m-tem a-zero)

 do-set
  ((m-tem) dpb m-2 (byte 5. 0.) a-zero)
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-1) dpb (byte 1. 0.) m-minus-one a-1)

  ((a-status-register) dpb m-zero (byte 1. 19.) a-status-register)
  ((m-tem) ldb (byte 1. 31.) m-1 a-zero)

  (jump-xct-next return-m-1-unsigned)
 ((a-status-register) dpb m-tem (byte 1. 17.) a-status-register))

(define-micro-function prioritize (bw value)

  (call get-32-bits)

  ((m-tem) dpb pdl-pop (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-1) and m-1 a-4)

  ((a-status-register) dpb m-minus-one (byte 0. 19.) a-status-register)
  (jump-equal m-1 a-zero no-bits-on)

 shift-loop
  (jump-if-bit-set (byte 1. 31.) m-4 count-zeros)
  ((m-4) ldb (byte 32. 31.) m-4 a-zero)
  (jump-xct-next shift-loop)
 ((m-1) ldb (byte 32. 31.) m-1 a-zero)

 count-zeros
  ((m-2) a-zero)                                ;count

 count-loop
  (jump-if-bit-set (byte 1. 31.) m-1 got-result)
  ((m-1) ldb (byte 32. 31.) m-1)                ;rotate left
  (jump-xct-next count-loop)
 ((m-2) add m-2 (a-constant 1.))

 got-result
  ((a-status-register) dpb m-2 (byte 8. 0.) a-status-register)
  ((a-status-register) dpb m-minus-one (byte 1. 19.) a-status-register)
 no-bits-on
  (jump-xct-next return-m-1-unsigned)
 ((m-1) m-2))

(define-micro-function non-aligned-logical-op (operation a b position width bw-bits)

  ((m-t) pdl-pop)                               ;m-t gets bw-bits

  ((m-4) pdl-pop)                               ;m-4 gets width
  (call-xct-next get-32-bits)                   ;m-3 gets position
 ((m-3) pdl-pop)                                ;m-2 gets b
                                                ;m-1 gets a
  (call-xct-next get-32-bits)                   ;leave operation on stack.
 ((m-2) m-1)

  (jump-if-bit-clear (byte 1. 0.) m-t check-shift)
  ;; use internal mask
  ((m-tem) a-status-register)
  ((m-3) ldb (byte 5. 8.) m-tem a-zero)

 check-shift
  (jump-if-bit-clear (byte 1. 1.) m-t do-action)
  ;; use internal shift
  ((m-tem) a-status-register)
  ((m-4) ldb (byte 6. 0.) m-tem a-zero)

 do-action
  ((m-t) a-v-nil)
  (jump-if-bit-set (byte 1. 5.) m-3 pass-f-ldb) ;position is negative

  ;; Do a funny dpb.
  ;;Rotate A by position.
  ((m-tem) sub m-3 (a-constant 1))
  ((oa-reg-low) dpb m-3 (byte 6. 0.) a-zero)
  ((m-1) ldb (byte 32. 0.) m-1 a-zero)

  ;; Generate mask.
  ((m-4) sub m-4 (a-constant 1))
  ((oa-reg-low) dpb m-4 (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((oa-reg-low) dpb m-3 (byte 6. 0.) a-zero)
  ((m-4) ldb (byte 32. 0.) m-4 a-zero)
  (jump do-the-merge)

 pass-f-ldb
  ;; Do a funny ldb
  ;; Rotate A by position.

  ((m-tem) add m-3 (a-constant 31.))
  ((oa-reg-low) dpb m-3 (byte 6. 0.) a-zero)
  ((m-1) ldb (byte 32. 0.) m-1 a-zero)

  ;; Generate mask
  ((m-4) sub m-4 (a-constant 1))
  ((oa-reg-low) dpb m-4 (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

 do-the-merge
  ;; Perform operation on a and b.
  ((oa-reg-low) dpb pdl-pop oal-aluf a-zero)
  ((m-1) setz m-1 a-2)

  ;; Generate the Z bit by anding with the mask.
  ((m-1) and m-1 a-4)
  (jump-equal m-1 a-zero set-z-bit)
  ((a-status-register) dpb m-zero (byte 1. 19.) a-status-register)
  (jump finish-up)
 set-z-bit
  ((a-status-register) dpb m-minus-one (byte 1. 19.) a-status-register)

 finish-up
  ((m-4) xor m-minus-one a-4)                   ;complement the mask
  ((m-2) and m-2 a-4)                           ;Zap bits in B
  ((m-1) ior m-1 a-2)                           ;Merge
  ((m-tem) ldb (byte 1. 31.) m-1 a-zero)
  (jump-xct-next return-m-1-unsigned)
 ((a-status-register) dpb m-tem (byte 1. 17.) a-status-register))


(define-micro-function add (size a b)
  (call get-32-bits)                            ;m-1 gets a
  (call-xct-next get-32-bits)                   ;m-2 gets b
 ((m-2) m-1)

  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-1) and m-4 a-1)                           ;mask the bytes
  ((m-3) and m-4 a-2)

  ((m-3) add m-1 a-3)                           ;do the add, m-3 gets result

  ;; Generate the Z bit by anding with the mask.
  ((m-tem) and m-3 a-4)
  (jump-equal m-tem a-zero set-z-bit)
  ((a-status-register) dpb m-zero (byte 1. 19.) a-status-register)
  (jump set-overflow-and-carry)
 set-z-bit
  ((a-status-register) dpb m-minus-one (byte 1. 19.) a-status-register)

 set-overflow-and-carry
  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)
  ((m-tem1) (a-constant 33.))
  ((m-tem) sub m-tem1 a-tem)
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem1) ldb (byte 32. 0.) m-1 a-zero)       ;a bit
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem2) ldb (byte 1. 0.) m-2 a-zero)        ;b bit
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem3) ldb (byte 1. 0.) m-3 a-zero)        ;y bit
  ((a-status-register) dpb m-tem3 (byte 1. 17.) a-status-register)      ;set n bit

  ((m-tem3) xor m-minus-one a-tem3)             ;y~
  ((m-t) m-tem3)                                ;y~
  ((m-tem)  and m-tem3      a-tem1)             ;ay~
  ((m-tem3) and m-tem3      a-tem2)             ;by~
  ((m-tem)  ior m-tem       a-tem)              ;ay~+by~
  ((m-tem3) and m-tem1      a-tem2)             ;ab
  ((m-tem3) ior m-tem3      a-tem)              ;ay~+by~+ab = Carry
  ((a-status-register) dpb m-tem3 (byte 1. 16.) a-status-register)
  ((m-tem3) xor m-minus-one a-t)
  ((m-t) a-v-nil)                               ;make m-t boxed again.

  ((m-tem2) xor m-tem2      a-tem1)             ;a xor b
  ((m-tem2) xor m-minus-one a-tem2)             ;a xnor b
  ((m-tem3) xor m-tem3      a-tem1)             ;a xor y
  ((m-tem)  and m-tem2      a-tem3)             ;(a xnor b) and (a xor y)
  ((a-status-register) dpb m-tem (byte 1. 18.) a-status-register)

  ((m-1) and m-3 a-4)                           ;mask overflow, etc.
  ((m-4) xor m-minus-one a-4)                   ;complement the mask
  ((m-2) and m-4 a-2)                           ;mask b
  ((m-1) ior m-1 a-2)                           ;merge in b

  (jump-xct-next return-m-1-unsigned)
 ((m-tem) pdl-pop))

(define-micro-function neg (size a b)
  (call get-32-bits)                            ;m-1 gets a
  (call-xct-next get-32-bits)                   ;m-2 gets b
 ((m-2) m-1)

  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-1) andca m-4 a-1)                         ;mask the bytes
  ((m-3) and m-4 a-2)

  ((m-3) add m-1 a-3)                           ;do the add, m-3 gets result

  ;; Generate the Z bit by anding with the mask.
  ((m-tem) and m-3 a-4)
  (jump-equal m-tem a-zero set-z-bit)
  ((a-status-register) dpb m-zero (byte 1. 19.) a-status-register)
  (jump set-overflow-and-carry)
 set-z-bit
  ((a-status-register) dpb m-minus-one (byte 1. 19.) a-status-register)

 set-overflow-and-carry
  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)
  ((m-tem1) (a-constant 33.))
  ((m-tem) sub m-tem1 a-tem)
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem1) ldb (byte 32. 0.) m-1 a-zero)       ;a bit
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem2) ldb (byte 1. 0.) m-2 a-zero)        ;b bit
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem3) ldb (byte 1. 0.) m-3 a-zero)        ;y bit
  ((a-status-register) dpb m-tem3 (byte 1. 17.) a-status-register)      ;set n bit

  ((m-tem3) xor m-minus-one a-tem3)             ;y~
  ((m-t) m-tem3)                                ;y~
  ((m-tem)  and m-tem3      a-tem1)             ;ay~
  ((m-tem3) and m-tem3      a-tem2)             ;by~
  ((m-tem)  ior m-tem       a-tem)              ;ay~+by~
  ((m-tem3) and m-tem1      a-tem2)             ;ab
  ((m-tem3) ior m-tem3      a-tem)              ;ay~+by~+ab = Carry
  ((a-status-register) dpb m-tem3 (byte 1. 16.) a-status-register)
  ((m-tem3) xor m-minus-one a-t)
  ((m-t) a-v-nil)                               ;make m-t boxed again.

  ((m-tem2) xor m-tem2      a-tem1)             ;a xor b
  ((m-tem2) xor m-minus-one a-tem2)             ;a xnor b
  ((m-tem3) xor m-tem3      a-tem1)             ;a xor y
  ((m-tem)  and m-tem2      a-tem3)             ;(a xnor b) and (a xor y)
  ((a-status-register) dpb m-tem (byte 1. 18.) a-status-register)

  ((m-1) and m-3 a-4)                           ;mask overflow, etc.
  ((m-4) xor m-minus-one a-4)                   ;complement the mask
  ((m-2) and m-4 a-2)                           ;mask b
  ((m-1) ior m-1 a-2)                           ;merge in b

  (jump-xct-next return-m-1-unsigned)
 ((m-tem) pdl-pop))

(define-micro-function sub (size a b)
  (call get-32-bits)                            ;m-1 gets a
  (call-xct-next get-32-bits)                   ;m-2 gets b
 ((m-2) m-1)

  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)     ;generate the mask
  ((m-tem) sub m-tem (a-constant 1))
  ((oa-reg-low) dpb m-tem (byte 6. 6.) a-zero)
  ((m-4) ldb (byte 1. 0.) m-minus-one a-zero)

  ((m-1) and m-4 a-1)                           ;mask the bytes
  ((m-tem) xor m-minus-one a-2)                 ;two's complement of m-3
  ((m-tem) add m-tem (a-constant 1))
  ((m-3) and m-tem a-4)

  ((m-4) xor m-minus-one a-4)                   ;invert-mask
  ((m-tem) and m-2 a-4)
  ((m-2) ior m-3 a-2)
  ((m-4) xor m-minus-one a-4)                   ;invert-mask

  ((m-3) add m-1 a-3)                           ;do the add, m-3 gets result

  ;; Generate the Z bit by anding with the mask.
  ((m-tem) and m-3 a-4)
  (jump-equal m-tem a-zero set-z-bit)
  ((a-status-register) dpb m-zero (byte 1. 19.) a-status-register)
  (jump set-overflow-and-carry)
 set-z-bit
  ((a-status-register) dpb m-minus-one (byte 1. 19.) a-status-register)

 set-overflow-and-carry
  ((m-tem) dpb pdl-top (byte 2. 3.) a-zero)
  ((m-tem1) (a-constant 33.))
  ((m-tem) sub m-tem1 a-tem)
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem1) ldb (byte 1. 0.) m-1 a-zero)        ;a bit
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem2) ldb (byte 1. 0.) m-2 a-zero)        ;b bit
  ((oa-reg-low) dpb m-tem (byte 6. 0.) a-zero)
  ((m-tem3) ldb (byte 1. 0.) m-3 a-zero)        ;y bit
  ((a-status-register) dpb m-tem3 (byte 1. 17.) a-status-register)      ;set n bit

  ((m-tem3) xor m-minus-one a-tem3)             ;y~
  ((m-t) m-tem3)                                ;y~
  ((m-tem)  and m-tem3      a-tem1)             ;ay~
  ((m-tem3) and m-tem3      a-tem2)             ;by~
  ((m-tem)  ior m-tem       a-tem)              ;ay~+by~
  ((m-tem3) and m-tem1      a-tem2)             ;ab
  ((m-tem3) ior m-tem3      a-tem)              ;ay~+by~+ab = Carry
  ((a-status-register) dpb m-tem3 (byte 1. 16.) a-status-register)
  ((m-tem3) xor m-minus-one a-t)
  ((m-t) a-v-nil)                               ;make m-t boxed again.

  ((m-tem2) xor m-tem2      a-tem1)             ;a xor b
  ((m-tem2) xor m-minus-one a-tem2)             ;a xnor b
  ((m-tem3) xor m-tem3      a-tem1)             ;a xor y
  ((m-tem)  and m-tem2      a-tem3)             ;(a xnor b) and (a xor y)
  ((a-status-register) dpb m-tem (byte 1. 18.) a-status-register)

  ((m-1) and m-3 a-4)                           ;mask overflow, etc.
  ((m-4) xor m-minus-one a-4)                   ;complement the mask
  ((m-2) and m-4 a-2)                           ;mask b
  ((m-1) ior m-1 a-2)                           ;merge in b

  (jump-xct-next return-m-1-unsigned)
 ((m-tem) pdl-pop))

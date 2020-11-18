;;; -*- Mode:LISP; Package:MICRO; Readtable:ZL; Base:8 -*-

;;; How to read LAMBDA microcode.
;;;
;;; m-<name> is an M memory location.
;;;   Examples:  m-tem, m-a, m-2, m-64
;;;
;;; a-<name> is an A memory location.
;;;   Examples:  a-saved-pdl-pointer
;;;
;;; For each M location, there is a corresponding
;;; A location that is written to when the M side
;;; is written.  This makes the low memory look
;;; dual ported.
;;;   Examples:  m-tem and a-tem   m-1 and a-1
;;;
;;; There are 4 types of instructions.
;;; ALU
;;; BYTE
;;; JUMP
;;; DISPATCH
;;;
;;; The ALU instruction specifies a source from the A-bus
;;; (A memory), the M-bus (M memory and Functional Sources)
;;; an operation and a destination ((M and functional) or A).
;;;
;;; Example:
;;;  ((pdl-buffer-index m-garbage) add m-1 (a-constant 220))
;;;
;;;  The functional destination pdl-buffer-index and
;;;  M register M-garbage are loaded with the output of
;;;  the alu.  The alu operation is Add which adds the M source
;;;  to the a source.
;;;
;;;  In the typical case, we only really want to write either
;;;  a functional source or an M register, so the assembler
;;;  will fill in the empty field with M-garbage or functional-noop
;;;  as needed.  We can abbreviate it as
;;;  ((pdl-index) add m-1 (a-constant 200.))
;;;
;;;  The pseudo-source (a-constant <constant>) designates a register
;;;  dedicated to holding the compile time constant specified.  (Yes,
;;;  you can run out, but we never allocate a new constant if an old
;;;  one has the same value.)
;;;
;;;  If you specify only an M or an A source, with no ALU operation,
;;;  the assembler fills in SETM or SETA as appropriate.
;;;  Examples:
;;;  ((pdl-index) a-saved-pdl-index)
;;;
;;; The BYTE instruction has the about the same syntax as an ALU instruction.
;;; There are 3 byte instructions.
;;;  LDB  (take a byte from the M side and left justify it into the A side)
;;;  DPB  (take a left justified byte from the M side and insert it into the A side)
;;;  SELECTIVE-DEPOSIT
;;;       (take a byte from the M side and insert it into the same place
;;;        in the A side.)
;;;  The BYTE instruction requires a byte-specifier.  (BYTE 3. 2.) is a byte
;;;  specifier which says 3 bits wide, 2 bits over.  The LAMBDA numbers its
;;;  bits with 0 being the least significant, so (byte 3. 2.) means
;;;  bits 4,3,2
;;;
;;;  The byte specifier is used to specify a rotation and a mask.  To rotate
;;;  a field, simply use (byte 32. n)
;;;  If you only specify a byte, a LDB is assumed.
;;;  ((m-tem) q-pointer pdl-pop) ==>
;;;  ((m-tem) ldb (byte 25. 0.) pdl-pop a-zero)
;;;
;;;  The JUMP instruction compares the A and M side and jumps accordingly.
;;;  Because of the pipeline, there is a no-op cycle that happens after
;;;  the jump.  To inhibit this no-op, JUMP-XCT-NEXT is used.  In this case,
;;;  the instruction after the jump is executed whether or not the jump is
;;;  taken.

;;; There are "functional sources" which can be read
;;; from the M side.

;;; Among these are
;;;
;;; PDL-INDEX   (register that addresses 2K of fast local memory (pdl-buffer))
;;; PDL-POINTER (register that addresses 2K of pdl-buffer, used as top of stack)
;;;
;;; C-PDL-BUFFER-INDEX            (the contents of the pdl-buffer at index)
;;; C-PDL-BUFFER-POINTER, PDL-TOP (the contents of the pdl-buffer at pointer)
;;; C-PDL-BUFFER-POINTER-DEC, PDL-POP (pdl-top, then decrement)
;;; C-PDL-BUFFER-INDEX-DEC

;;; There are functional destinations which can be written from the
;;; M side.

;;; Among these are
;;;
;;; PDL-INDEX   (register that addresses 2K of fast local memory (pdl-buffer))
;;; PDL-POINTER (register that addresses 2K of pdl-buffer, used as top of stack)
;;;
;;; C-PDL-BUFFER-INDEX            (the contents of the pdl-buffer at index)
;;; C-PDL-BUFFER-POINTER, PDL-TOP (the contents of the pdl-buffer at pointer)
;;; C-PDL-BUFFER-POINTER-INC, PDL-PUSH (increment, then pdl-top)
;;; C-PDL-BUFFER-INDEX-INC
;;; OA-REG-LOW  (imod feature)
;;; OA-REG-HIGH

;;; The DP-MODE register contains the top bit of the pdl address registers.
;;; The M memory and the PDL buffer are implemented as one 4K by 32bit
;;; static RAM.  The DP-MODE is usually set to one, so the pdl addresses are
;;; in the top half.  If the DP-mode is set to 0, both move down to the
;;; bottom half.  We use this trick to get at a lot of fast memory.
;;; This also introduces the possibility of clobbering M memory.
;;; To avoid this, we set the pdl pointer at #o100 (just above m memory).

;;; The lambda microcode is pageable (!)  This means that a page fault can
;;; occur anywhere.  Page faults are allowed to push on the stack, so it
;;; is not allowed to play with the pdl-pointer in microcode.

;;; Writing to OA-REG-HIGH or LOW will cause the output of the alu to
;;; be inclusive or'ed into the next instruction.  This is how to do
;;; computed shifts and rotates.  The micro instruction is 64 bits, so
;;; you can only modify half of it at any one time.

;;; Layout
;;; m-memory            00 000 000000

;;; pdl-slop            00 001 000000

;;; des-registers       00 010 000000

;;; S boxes             11 000 000000
;;;                     11 111 111111

;;;  200 low-key half
;;;  201 high-key half
;;;  m-60 c0
;;;  m-61 d0
;;;  m-64 R
;;;  m-65 L
;;;  m-66 Low Expanded R
;;;  m-67 High Expanded R
;;;  m-57 salt

;;; 32. key schedule 1
;;; 34. key schedule 2 ,etc

(define-a-memory-variable a-saved-pdl-pointer 0)

(define-micro-function read-hidden-m-memory (location)
  ((pdl-index) pdl-pop)
  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))
  ((m-1) c-pdl-buffer-index)
  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)
  (jump return-m-1-unsigned))

(define-micro-function load-key-char (n char)
  ((m-tem) output-selector-bit-mirror pdl-top)
  ((m-tem) ldb (byte 7. 25.) m-tem (a-constant (byte-value q-data-type dtp-fix)))
  ((m-tem) selective-deposit (byte 1. 7.) pdl-pop a-tem)

  ((m-tem1) pdl-pop)

  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ((pdl-index) ldb (byte 1. 2.) m-tem1 (a-constant 200))
  ((m-tem2) c-pdl-buffer-index)
  ((oa-reg-low) dpb (byte 2. 3.) m-tem1 a-zero)
  ((c-pdl-buffer-index) dpb (byte 8. 0.) m-tem a-tem2)

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)

  (jump xfalse))

(define-micro-function load-salt (s)
  (jump-xct-next xfalse)
 ((m-57) ldb (byte 12. 0.) pdl-pop a-zero))

(define-micro-function generate-c0-and-d0 ()

  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ;; Apply permuted choice 1 to make 56 bit key.

  ((pdl-index) (a-constant 201))

  ((m-tem) ldb (byte 1. 24.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  0.) m-tem a-zero)
  ((m-tem) ldb (byte 1. 16.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  1.) m-tem a-60)
  ((m-tem) ldb (byte 1.  8.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  2.) m-tem a-60)
  ((m-tem) ldb (byte 1.  0.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  3.) m-tem a-60)

  ((m-tem) ldb (byte 1. 25.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  8.) m-tem a-60)
  ((m-tem) ldb (byte 1. 17.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  9.) m-tem a-60)
  ((m-tem) ldb (byte 1.  9.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 10.) m-tem a-60)
  ((m-tem) ldb (byte 1.  1.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 11.) m-tem a-60)

  ((m-tem) ldb (byte 1. 26.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 16.) m-tem a-60)
  ((m-tem) ldb (byte 1. 18.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 17.) m-tem a-60)
  ((m-tem) ldb (byte 1. 10.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 18.) m-tem a-60)
  ((m-tem) ldb (byte 1.  2.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 19.) m-tem a-60)

  ((m-tem) ldb (byte 1. 27.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 24.) m-tem a-60)
  ((m-tem) ldb (byte 1. 19.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 25.) m-tem a-60)
  ((m-tem) ldb (byte 1. 11.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 26.) m-tem a-60)
  ((m-tem) ldb (byte 1.  3.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 27.) m-tem a-60)

  ((pdl-index) (a-constant 200))

  ((m-tem) ldb (byte 1. 24.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  4.) m-tem a-60)
  ((m-tem) ldb (byte 1. 16.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  5.) m-tem a-60)
  ((m-tem) ldb (byte 1.  8.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  6.) m-tem a-60)
  ((m-tem) ldb (byte 1.  0.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1.  7.) m-tem a-60)

  ((m-tem) ldb (byte 1. 25.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 12.) m-tem a-60)
  ((m-tem) ldb (byte 1. 17.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 13.) m-tem a-60)
  ((m-tem) ldb (byte 1.  9.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 14.) m-tem a-60)
  ((m-tem) ldb (byte 1.  1.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 15.) m-tem a-60)

  ((m-tem) ldb (byte 1. 26.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 20.) m-tem a-60)
  ((m-tem) ldb (byte 1. 18.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 21.) m-tem a-60)
  ((m-tem) ldb (byte 1. 10.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 22.) m-tem a-60)
  ((m-tem) ldb (byte 1.  2.) c-pdl-buffer-index a-zero)
  ((m-60)  dpb (byte 1. 23.) m-tem a-60)

  ((pdl-index) (a-constant 201))

  ((m-tem) ldb (byte 1. 30.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  0.) m-tem a-zero)
  ((m-tem) ldb (byte 1. 22.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  1.) m-tem a-61)
  ((m-tem) ldb (byte 1. 14.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  2.) m-tem a-61)
  ((m-tem) ldb (byte 1.  6.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  3.) m-tem a-61)

  ((m-tem) ldb (byte 1. 29.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  8.) m-tem a-61)
  ((m-tem) ldb (byte 1. 21.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  9.) m-tem a-61)
  ((m-tem) ldb (byte 1. 13.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 10.) m-tem a-61)
  ((m-tem) ldb (byte 1.  5.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 11.) m-tem a-61)

  ((m-tem) ldb (byte 1. 28.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 16.) m-tem a-61)
  ((m-tem) ldb (byte 1. 20.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 17.) m-tem a-61)
  ((m-tem) ldb (byte 1. 11.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 18.) m-tem a-61)
  ((m-tem) ldb (byte 1.  4.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 19.) m-tem a-61)

  ((pdl-index) (a-constant 200))

  ((m-tem) ldb (byte 1. 30.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  4.) m-tem a-61)
  ((m-tem) ldb (byte 1. 22.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  5.) m-tem a-61)
  ((m-tem) ldb (byte 1. 14.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  6.) m-tem a-61)
  ((m-tem) ldb (byte 1.  6.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1.  7.) m-tem a-61)

  ((m-tem) ldb (byte 1. 29.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 12.) m-tem a-61)
  ((m-tem) ldb (byte 1. 21.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 13.) m-tem a-61)
  ((m-tem) ldb (byte 1. 13.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 14.) m-tem a-61)
  ((m-tem) ldb (byte 1.  5.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 15.) m-tem a-61)

  ((m-tem) ldb (byte 1. 28.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 20.) m-tem a-61)
  ((m-tem) ldb (byte 1. 20.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 21.) m-tem a-61)
  ((m-tem) ldb (byte 1. 12.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 22.) m-tem a-61)
  ((m-tem) ldb (byte 1.  4.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 23.) m-tem a-61)

  ((m-tem) ldb (byte 1. 27.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 24.) m-tem a-61)
  ((m-tem) ldb (byte 1. 19.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 25.) m-tem a-61)
  ((m-tem) ldb (byte 1. 11.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 26.) m-tem a-61)
  ((m-tem) ldb (byte 1.  3.) c-pdl-buffer-index a-zero)
  ((m-61) dpb (byte 1. 27.) m-tem a-61)

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)

  (jump xfalse))


(define-micro-function generate-scheduled-key (n rotation)

  ((m-tem) q-pointer pdl-top)                   ;m-tem gets rotation

 rotate-c-loop
  (jump-equal m-tem a-zero rotate-d)
  ((m-tem) sub m-tem (a-constant 1.))
  ((m-tem1) ldb (byte 1. 0.) m-60 a-zero)       ;read bottom bit
  ((m-60)   ldb (byte 32. 1.) m-60 a-zero)      ;rotate word
  (jump-xct-next rotate-c-loop)
 ((m-60) dpb m-tem1 (byte 1. 27.) a-60)         ;put bottom bit on top.

 rotate-d
  ((m-tem) q-pointer pdl-pop)                   ;now pop rotation

 rotate-d-loop
  (jump-equal m-tem a-zero apply-pc-2)
  ((m-tem) sub m-tem (a-constant 1.))
  ((m-tem1) ldb (byte 1. 0.) m-61 a-zero)       ;read bottom bit
  ((m-61)   ldb (byte 32. 1.) m-61 a-zero)      ;rotate word
  (jump-xct-next rotate-d-loop)
 ((m-61) dpb m-tem1 (byte 1. 27.) a-61)         ;put bottom bit on top.

 apply-pc-2
  ;; Apply bottom of pc2

  ((m-tem)  ldb (byte 1. 13.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  0.) m-tem a-zero)
  ((m-tem)  ldb (byte 1. 16.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  1.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 10.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  2.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 23.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  3.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  0.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  4.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  4.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  5.) m-tem a-tem1)

  ((m-tem)  ldb (byte 1.  2.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  6.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 27.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  7.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 14.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  8.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  5.) m-60 a-zero)
  ((m-tem1) dpb (byte 1.  9.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 20.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 10.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  9.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 11.) m-tem a-tem1)

  ((m-tem)  ldb (byte 1. 22.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 12.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 18.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 13.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 11.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 14.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  3.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 15.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 25.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 16.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  7.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 17.) m-tem a-tem1)

  ((m-tem)  ldb (byte 1. 15.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 18.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  6.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 19.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 26.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 20.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 19.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 21.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1. 12.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 22.) m-tem a-tem1)
  ((m-tem)  ldb (byte 1.  1.) m-60 a-zero)
  ((m-tem1) dpb (byte 1. 23.) m-tem a-tem1)

  ;; Apply top of pc2

  ((m-tem)  ldb (byte 1. 12.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  0.) m-tem a-zero)
  ((m-tem)  ldb (byte 1. 23.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  1.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  2.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  2.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  8.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  3.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 18.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  4.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 26.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  5.) m-tem a-tem2)

  ((m-tem)  ldb (byte 1.  1.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  6.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 11.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  7.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 22.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  8.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 16.) m-61 a-zero)
  ((m-tem2) dpb (byte 1.  9.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  4.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 10.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 19.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 11.) m-tem a-tem2)

  ((m-tem)  ldb (byte 1. 15.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 12.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 20.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 13.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 10.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 14.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 27.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 15.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  5.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 16.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 24.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 17.) m-tem a-tem2)

  ((m-tem)  ldb (byte 1. 17.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 18.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 13.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 19.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1. 21.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 20.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  7.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 21.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  0.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 22.) m-tem a-tem2)
  ((m-tem)  ldb (byte 1.  3.) m-61 a-zero)
  ((m-tem2) dpb (byte 1. 23.) m-tem a-tem2)

  ((m-tem) q-pointer pdl-pop)                           ;get key number

  ;; Access hidden memory.
  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ((pdl-index) add m-tem (a-constant 220))
  ((c-pdl-buffer-index) m-tem1)
  ((pdl-index) add pdl-index (a-constant 20))
  ((c-pdl-buffer-index) m-tem2)

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)

  (jump-xct-next return-m-1-unsigned)
 ((m-1) m-tem1))

(define-micro-function load-block (left right)
  (call get-32-bits)

  (call-xct-next get-32-bits)
 ((m-64) m-1)

  (jump-xct-next xfalse)
 ((m-65) m-1))

(define-micro-function expand-right ()

  ((m-66)  ldb (byte 1. 31.) m-64 a-zero)
  ((m-tem) ldb (byte 5.  0.) m-64 a-zero)
  ((m-66)  dpb (byte 5.  1.) m-tem a-66)
  ((m-tem) ldb (byte 6.  3.) m-64 a-zero)
  ((m-66)  dpb (byte 6.  6.) m-tem a-66)
  ((m-tem) ldb (byte 6.  7.) m-64 a-zero)
  ((m-66)  dpb (byte 6. 12.) m-tem a-66)
  ((m-tem) ldb (byte 6. 11.) m-64 a-zero)
  ((m-66)  dpb (byte 6. 18.) m-tem a-66)

  ((m-67)  ldb (byte 6. 15.) m-64 a-zero)
  ((m-tem) ldb (byte 6. 19.) m-64 a-zero)
  ((m-67)  dpb (byte 6.  6.) m-tem a-67)
  ((m-tem) ldb (byte 6. 23.) m-64 a-zero)
  ((m-67)  dpb (byte 6. 12.) m-tem a-67)
  ((m-tem) ldb (byte 5. 27.) m-64 a-zero)
  ((m-67)  dpb (byte 5. 18.) m-tem a-67)
  ((m-67)  dpb (byte 1. 23.) m-64 a-67)

  ((m-tem) xor m-66  a-67)                      ;find bits to swap
  ((m-tem) and m-tem a-57)                      ;mask non swapping bits
  ((m-66) xor m-66 a-tem)
  ((m-67) xor m-67 a-tem)

  (jump xfalse))

(define-micro-function load-s-box (s-box-number entry value)

  (call get-32-bits)

  ((m-tem) dpb pdl-pop (byte 6. 0.) (a-constant 3000))
  ((pdl-index) dpb pdl-pop (byte 3. 6.) a-tem)

  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ((c-pdl-buffer-index) m-1)

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)
  (jump xfalse))

(defun load-s-boxes (s-boxes)
  (dotimes (s-box-number 8.)
    (let ((this-s-box (elt s-boxes s-box-number)))
      (dotimes (input (ash 1. 6.))
        (let* ((s-box-output (user::sbox-ref this-s-box (user::fixnum->bit-string input)))
               (shifted-output (ash (user::Bit-string->fixnum s-box-output) (* s-box-number 4)))
               (permuted-output (user::apply-permutation user::p-permutation
                                                         (user::fixnum->bit-string shifted-output 32.))))
          (load-s-box s-box-number input (user::bit-string->fixnum permuted-output)))))))

(defun verify-s-boxes (s-boxes)
  (dotimes (s-box-number 8.)
    (let ((this-s-box (elt s-boxes s-box-number)))
      (dotimes (input (ash 1. 6.))
        (let* ((s-box-output (user::sbox-ref this-s-box (user::fixnum->bit-string input)))
               (shifted-output (ash (user::Bit-string->fixnum s-box-output) (* s-box-number 4)))
               (permuted-output (user::apply-permutation user::p-permutation
                                                         (user::fixnum->bit-string shifted-output 32.)))
               (my-output (s-box-ref s-box-number input)))
          (when (not (= my-output (user::bit-string->fixnum permuted-output)))
            (ferror nil "lose")))))))

(define-micro-function s-box-ref (s-box n)

  ((m-tem) dpb pdl-pop (byte 6. 0.) (a-constant 3000))
  ((pdl-index) dpb pdl-pop (byte 3. 6.) a-tem)

  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ((m-1) c-pdl-buffer-index)

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)
  (jump return-m-1-unsigned))

(define-micro-function xor-key (n)

  ((m-1) ldb (byte 4. 0.) pdl-pop)

  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ((pdl-index) add m-1 (a-constant 220))
  ((m-66) xor c-pdl-buffer-index a-66)
  ((pdl-index) add pdl-index (a-constant 20))
  ((m-67) xor c-pdl-buffer-index a-67)

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)

  (jump xfalse))

(define-micro-function pass-through-s-boxes ()
  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ((pdl-index) ldb (byte 6.  0.) m-66 (a-constant 3000))
  ((m-1) c-pdl-buffer-index)
  ((pdl-index) ldb (byte 6.  6.) m-66 (a-constant 3100))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 12.) m-66 (a-constant 3200))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 18.) m-66 (a-constant 3300))
  ((m-1) ior c-pdl-buffer-index a-1)

  ((pdl-index) ldb (byte 6.  0.) m-67 (a-constant 3400))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6.  6.) m-67 (a-constant 3500))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 12.) m-67 (a-constant 3600))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 18.) m-67 (a-constant 3700))
  ((m-1) ior c-pdl-buffer-index a-1)

  ;; swap the two halves
  ((m-tem) m-65)                                ;m-tem<-L
  ((m-65) m-64)                                 ;L<-R
  ((m-64) xor m-tem a-1)                        ;R<-gunk xor L

  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)
  (jump return-m-1-unsigned))

(define-micro-function swap-halves ()
  ((m-tem) m-65)
  ((m-65) m-64)
  ((m-64) m-tem)
  (jump xfalse))

(define-micro-function des-loop ()
  (declare (:compile-as-misc-instruction t))

  ;; Work in hidden memory.
  ((a-saved-pdl-pointer) pdl-pointer)
  ((dp-mode) m-zero)
  ((pdl-pointer) (a-constant 100))

  ;; Initialize count.
  ((m-a) (a-constant (byte-value q-data-type dtp-fix))) ;m-a holds counter.

 des-round
  ;; Expand the right half.
  ((m-66)  ldb (byte 1. 31.) m-64 a-zero)
  ((m-tem) ldb (byte 5.  0.) m-64 a-zero)
  ((m-66)  dpb (byte 5.  1.) m-tem a-66)
  ((m-tem) ldb (byte 6.  3.) m-64 a-zero)
  ((m-66)  dpb (byte 6.  6.) m-tem a-66)
  ((m-tem) ldb (byte 6.  7.) m-64 a-zero)
  ((m-66)  dpb (byte 6. 12.) m-tem a-66)
  ((m-tem) ldb (byte 6. 11.) m-64 a-zero)
  ((m-66)  dpb (byte 6. 18.) m-tem a-66)

  ((m-67)  ldb (byte 6. 15.) m-64 a-zero)
  ((m-tem) ldb (byte 6. 19.) m-64 a-zero)
  ((m-67)  dpb (byte 6.  6.) m-tem a-67)
  ((m-tem) ldb (byte 6. 23.) m-64 a-zero)
  ((m-67)  dpb (byte 6. 12.) m-tem a-67)
  ((m-tem) ldb (byte 5. 27.) m-64 a-zero)
  ((m-67)  dpb (byte 5. 18.) m-tem a-67)
  ((m-67)  dpb (byte 1. 23.) m-64 a-67)

  ;; Salt the expansion
  ((m-tem) xor m-66  a-67)                      ;find bits to swap
  ((m-tem) and m-tem a-57)                      ;mask non swapping bits
  ((m-66) xor m-66 a-tem)
  ((m-67) xor m-67 a-tem)

  ;; Xor in the key
  ((m-2) ldb (byte 4. 0.) m-a a-zero)           ;get key number.

  ((pdl-index) add m-2 (a-constant 220))        ;read low key half
  ((m-66) xor c-pdl-buffer-index a-66)
  ((pdl-index) add pdl-index (a-constant 20))   ;read high key half
  ((m-67) xor c-pdl-buffer-index a-67)

  ;; Pass through the s-boxes
  ((pdl-index) ldb (byte 6.  0.) m-66 (a-constant 3000))
  ((m-1) c-pdl-buffer-index)
  ((pdl-index) ldb (byte 6.  6.) m-66 (a-constant 3100))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 12.) m-66 (a-constant 3200))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 18.) m-66 (a-constant 3300))
  ((m-1) ior c-pdl-buffer-index a-1)

  ((pdl-index) ldb (byte 6.  0.) m-67 (a-constant 3400))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6.  6.) m-67 (a-constant 3500))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 12.) m-67 (a-constant 3600))
  ((m-1) ior c-pdl-buffer-index a-1)
  ((pdl-index) ldb (byte 6. 18.) m-67 (a-constant 3700))
  ((m-1) ior c-pdl-buffer-index a-1)

  ;; Xor the stuff in and swap halves.
  ((m-tem) m-65)                                ;m-tem<-L
  ((m-65) m-64)                                 ;L<-R
  (jump-not-equal-xct-next m-2 (a-constant 15.) check-if-done)
 ((m-64) xor m-tem a-1)                 ;R<-gunk xor L

  ;; Swap halves every 16 iterations.
  ((m-tem) m-65)
  ((m-65) m-64)
  ((m-64) m-tem)

 check-if-done
  (jump-not-equal-xct-next m-a (a-constant (plus (byte-value q-data-type dtp-fix)
                                                 399.))
                           des-round)
 ((m-a) add m-a (a-constant 1.))

  ;; Go back to lisp.
  ((dp-mode) (a-constant 1.))
  ((pdl-pointer) a-saved-pdl-pointer)
  (jump xfalse))


(defun test1 (times)
  (dotimes (j times)
    (dotimes (i 8.) (load-key-char i (elt "foobar  " i)))
    (generate-c0-and-d0)
    (do ((shift-list user::key-shift-schedule (rest shift-list))
         (n          0                        (1+ n)))
        ((null shift-list) nil)
      (generate-scheduled-key n (first shift-list)))
    (load-block 0 0)
    (des-loop)))

(defun test3 (x)
  (dotimes (j x)
    (dotimes (i 8.) (load-key-char i (elt "foobar  " i)))
    (generate-c0-and-d0)
    (let ((n 0))
      (dolist (shift user::key-shift-schedule)
        (generate-scheduled-key n shift)
        (incf n)))
    (load-block 0 0)
    (des-loop))

(defun des-loop ()
  (dotimes (i 16.)
    (expand-right)
    (xor-key i)
    ;(format t "~&~32,48b" (read-hidden-m-memory #o66))
    (pass-through-s-boxes)
;    (format t "~&~32,48b ~32,48b" (read-hidden-m-memory #o64) (read-hidden-m-memory #o65))
    ))

(defun test4 (x)
  (dotimes (j x)
    (user::generate-key-schedule (user::key->bit-string "foobar  "))))

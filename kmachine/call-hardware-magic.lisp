;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-
;;;
;;; CALL-HARDWARE-MAGIC
;;;
;;; Initial implementation 08-Apr-88, James Rauen
;;;


(defun a0-contents (frame)
  "Foo")

;(defafun foo-add (x y)
;  (hw:without-traps
;    #'(lambda ()
;       )))

(defun foo-add (x y)
  (hw:without-traps
    #'(lambda ()
        (+ x y))))

(defafun bar-add (x y)
  (alu l+r a2 a0 a1 bw-24 boxed-right)
  (return a2 boxed))

(defun baz-add (x y)
  (+ x y))

(defun chm-get-O-A-R-top ()
  (chm-get-O-A-R)
  (hw:nop)
  (hw:nop)
  (hw:nop))

(defafun chm-get-O-A-R ()
  (move a0 open-active-return)
  (movei a1 VINC::$$DTP-FIXNUM)
  (alu-field field-pass a2 a1 a0 (byte 6. 26.) boxed)
  (return a2 boxed))

(defun tryit (x)
  (setq x (chm-register-contents 4 4))
  (loop))

(defun chm-is-register-boxed? (frame register)
  "Return T is the specified register (0..15) in the specified
frame (0..255) contains a boxed value.  Otherwise return nil."
  (trap:without-traps
    #'(lambda ()
        (chm-is-register-boxed?-1 frame register))))

(defun boxed? (frame register)
  (chm-is-register-boxed?-1 frame register))

(defafun chm-is-register-boxed?-1 (frame register)
  (move a2 open-active-return)
  (nop) (nop) (nop) (nop) (nop)
  (movei a3 #b11111111)
  (alu and a0 a0 a3)
  (movei a3 #b1111)
  (alu and a1 a1 a3)
  (alu-field field-pass a4 a0 a2 (byte 8. 0.))
  (move open-active-return a4)
  (nop) (nop) (nop) (nop) (nop)
  (movei a3 1)
  (alu l+r a1 a1 a3)
  foo0 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo1 nil) (move a5 r0)
  foo1 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo2 nil) (move a5 r1)
  foo2 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo3 nil) (move a5 r2)
  foo3 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo4 nil) (move a5 r3)
  foo4 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo5 nil) (move a5 r4)
  foo5 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo6 nil) (move a5 r5)
  foo6 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo7 nil) (move a5 r6)
  foo7 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo8 nil) (move a5 r7)
  foo8 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo9 nil) (move a5 r8)
  foo9 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo10 nil) (move a5 r9)
  foo10 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo11 nil) (move a5 r10)
  foo11 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo12 nil) (move a5 r11)
  foo12 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo13 nil) (move a5 r12)
  foo13 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo14 nil) (move a5 r13)
  foo14 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo15 nil) (move a5 r14)
  foo15 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo16 nil) (move a5 r15)
  foo16
  (move open-active-return a2)
  (nop) (nop) (nop) (nop) (nop)
  (alu l-r nop a5 a5)
  (nop) (nop)
  (move a6 processor-status)
  (nop) (nop) (nop) (nop) (nop)
  (movei a3 0)
  (alu-field field-pass a7 a6 a3 (byte 1. 18.))
  (alu and nop a7 a7)
  (test br-not-zero)
  (branch boxed nil)
  (movei a8 nil)
  (return a8)
  boxed
  (movei a8 t)
  (return a8)
  )

(defun chm-register-contents (frame register)
  "Examine the contents of the specified register (0..15) in the
specified frame (0..255).  If the value in the register is boxed,
return it.  Otherwise raise an error."
  (chm-register-contents-1 frame register))

(defafun chm-register-contents-1 (frame register)

  ; Get the current values (8 bits each) of the O, A, and R frame registers.
  ; Bits 7:0 contain R
  ; Bits 15:8 contain A
  ; Bits 23:16 contain O
  (move a2 open-active-return)

  ; Strip off all but the bottom four bits of the register and the
  ; bottom eight bits of the frame.
  (movei a3 #b11111111)
  (alu and a0 a0 a3)
  (movei a3 #b1111)
  (alu and a1 a1 a3)

  ; Dummy up a new set of O-A-R values by replacing the real R with the
  ; requested register.  Put the new set into a4.
  (alu-field field-pass a4 a0 a2 (byte 8. 0.))
  (nop)
  (nop)

  ; Switch O-A-R register sets.  Only the R frame register should change.
  (move open-active-return a4)

  ; Get the contents of the requested return register; put it into A5.
  (movei a3 1)
  (alu l+r a1 a1 a3)
  foo0 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo1 nil) (move a5 r0)
  foo1 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo2 nil) (move a5 r1)
  foo2 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo3 nil) (move a5 r2)
  foo3 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo4 nil) (move a5 r3)
  foo4 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo5 nil) (move a5 r4)
  foo5 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo6 nil) (move a5 r5)
  foo6 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo7 nil) (move a5 r6)
  foo7 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo8 nil) (move a5 r7)
  foo8 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo9 nil) (move a5 r8)
  foo9 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo10 nil) (move a5 r9)
  foo10 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo11 nil) (move a5 r10)
  foo11 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo12 nil) (move a5 r11)
  foo12 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo13 nil) (move a5 r12)
  foo13 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo14 nil) (move a5 r13)
  foo14 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo15 nil) (move a5 r14)
  foo15 (alu l-r a1 a1 a3) (test br-not-zero) (branch foo16 nil) (move a5 r15)
  foo16

  ; Switch back the O-A-R
  (move open-active-return a2)

  ; Set A5's box bit.
  (move a5 a5 boxed)

  ; Fetch A5's box bit.
  (movei a3 1)
  ; (alu shift-up-lf-l a3 a3 a5 bw-24 carry-1)
  ; (alu-field field-pass a6 a5 a3 (byte 1. 32.))

  ; Error if A5 is unboxed.  Otherwise return A5.
  (alu setr nop a3 a3)
  (test br-zero)
  (branch whoops-its-unboxed nil)
  (return a5)

whoops-its-unboxed
  (movei o0 "Tried to read unboxed register in CHM-REGISTER-CONTENTS" boxed ch-tail-open)
  (tail-call (error 1) nil)
)


(defafun bar ()
  (movei a0 0)
  (movei a1 0 boxed)
  (movei a2 0)
  (alu shift-up-lf-l a2 a2 a0 bw-24 carry-1)
  (movei a2 0)
  (alu shift-up-lf-l a2 a2 a1 bw-24 carry-1)
  (nop)
  (nop)
  (nop)
  )

(defun foo (x)
  (if (zerop x)
      (bar)
    (baz)))

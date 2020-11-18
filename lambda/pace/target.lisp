;;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

;every:
;next pc
;   PC+1 (default)
;   pc-branch
;   pc-dispatch
;   pc-return

;boxed
;   box0 (default)
;   box1
;   boxL
;   boxR

;datatype
;   DT-NONE (default)
;   dt-both-fix
;   dt-same
;   dt-r-list-or-nil
;   dt-r-pointer
;   dt-
;   dt-
;   dt-

;call hardware
;   ch-none (default)
;   ch-open
;   ch-topen
;   ch-call
;   ch-tcall
;   ch-open-call
;   ch-return
;   ch-

;jcond
;   jc-never (default)
;   jc-always
;   jc-zero
;   jc-not-zero
;   jc-carry
;   jc-not-carry
;   jc-indirect
;   jc-

;alu op
;   l+r
;   l-r
;   r-l
;   l+r+1
;   l-r+1
;   ldb
;   dpb

;byte-width
;   bw32 (default)
;   bw24
;   bw16
;   bw8

;(alu aluop dest left right [(byte ss pp)] ...)
;(branch tag aluop dest left right ...)         ;(replaces low 12 bits)
;(alui aluop dest left (constant xxx) ...)   ;constant up to 16 bits - omit left for const to 24 bits
;(loadi dest (constant xxx) ...) ; 32 bit constant
;(jump/call d-foobar dest right ...)
;(jumpz/callz tag d-foobar aluop dest left right ...)

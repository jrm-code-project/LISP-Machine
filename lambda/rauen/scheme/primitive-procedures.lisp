;;; -*- Mode:LISP; Package:SCHEME; Base:10; Readtable:CL -*-
;;;
;;; Primitive procedures

(define-primitive-procedure '+ #'+)
(define-primitive-procedure '- #'-)
(define-primitive-procedure '* #'*)
(define-primitive-procedure '= #'=)
(define-primitive-procedure '< #'<)
(define-primitive-procedure 'cons #'cons)
(define-primitive-procedure 'inspect #'inspect)

(add-binding 'nil NIL *scheme-global-environment*)

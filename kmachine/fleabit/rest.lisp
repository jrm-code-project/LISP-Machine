;;; -*- Mode:LISP; Package:LI; Readtable:CL; Base:10 -*-


;;;; Cons Rest Args
;;;
;;; (defun f (m n o p q r)
;;;   (g r q p o n m) ...)
;;;
;;; (defun g (a b c &rest z)
;;;   ...)
;;;
;;;
;;; f
;;;  (move O0 A5)
;;;  ...
;;;  (move O5 A0)
;;;  (move *arg-2* '5)             ;<n-args>-1
;;;  (call g)
;;;  ...
;;;
;;; g
;;;  (move *arg-1* '3)             ;<n-spread-args>
;;;  (jump cons-rest (*return-pc-1* opc))
;;;  (move A3 *rest*)
;;;  ...
;;;

;;;(defun cons-rest () ;(*arg-1* *arg-2*)
;;;  (tagbody
;;;      (setq *rest* nil)
;;;      (go next)
;;;   loop
;;;      (push (get-arg (incf *arg-1*)) *rest*)
;;;   next
;;;      (unless (= *arg-1* *arg-2*)
;;;        (go loop))
;;;      (hw:dispatch (1+ *return-pc-1*))))


;;; CONS-REST
;;; called with
;;;     <return-address>-1 in *return-pc-1*
;;;     <first-consed>     in *arg-1*
;;;     <last-consed>      in *arg-2*
;;; returns in *rest*
(defafun cons-rest ()
   (unconditional-branch next (alu zero *rest* ignore ignore))   ;(movei *rest* 'nil)
 loop
   (movea r0 get-arg-dispatch br-greater-than)
   (branch get-stack-arg (alu l+r nop-no-overflow *arg-1* r0))
   (alu r+1 *arg-1* ignore *arg-1*)
   (nop ch-open next-pc-dispatch)
 get-arg-dispatch
   (unconditional-branch cons-it (move O0 A0))
   (unconditional-branch cons-it (move O0 A1))
   (unconditional-branch cons-it (move O0 A2))
   (unconditional-branch cons-it (move O0 A3))
   (unconditional-branch cons-it (move O0 A4))
   (unconditional-branch cons-it (move O0 A5))
   (unconditional-branch cons-it (move O0 A6))
   (unconditional-branch cons-it (move O0 A7))
   (unconditional-branch cons-it (move O0 A8))
   (unconditional-branch cons-it (move O0 A9))
   (unconditional-branch cons-it (move O0 A10))
   (unconditional-branch cons-it (move O0 A11))
   (unconditional-branch cons-it (move O0 A12))
   (unconditional-branch cons-it (move O0 A13))
   (unconditional-branch cons-it (move O0 A14))
   (unconditional-branch cons-it (move O0 A15))
 cons-it
   (call (cons 2) *rest* (O1 *rest*))
 next
   (alu l-r nop-no-overflow-trap *arg-1* *arg-2*)
   (movei r0 '15 br-not-greater-than)
   (branch loop (alu l-r nop-no-overflow *arg-1* r0))
 done
   (alu r+1 nop-no-overflow-trap ignore *return-pc-1*)
   (nop)
   (nop next-pc-dispatch)
 get-stack-arg)

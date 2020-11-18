;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;


(zl:defsubst increment-stack ()
  (incf *stack-pos* CELL))

(zl:defsubst n-decrement-stack (n)
  (decf *stack-pos* (* n CELL)))


;;; defsubst
(defun generate-move (from to)
  (unless (or (eql from to)
              (eq to 'IGNORE))
      (if (and (consp from) (null (cdr from)))
          (generate-move-address (car from) to)
          (emit 'K:MOVE to from))))


(defun generate-push (access)
  (increment-stack)
  (if (and (consp access) (null (cdr access)))
      (emit m68/pea (car access))
      (emit lm/push access)))

(zl:defsubst generate-pop (access)
  (emit lm/pop access))


(defun adjust-stack-pointer (n)
  (unless (zerop n)
    (emit  lm/add SP SP `(constant ,n))))


(defun generate-move-address (from to)
  (cond ((register? to)
         (emit lm/move to from))
        ((locked? AN)
         (emit m68/pea from)
         (generate-pop to))
        (t
         (emit m68/lea from AN)
         (emit m68/move .l AN to))))

(defun generate-move-address-hack (from to temp)
  (cond ((register? to)
         (emit m68/lea from to))
        (else
         (emit m68/lea from temp)
         (emit m68/move .l temp to))))


;(zl:defsubst generate-jump (label)
;  (emit-jump label nil))

;;; jump but try not to
(zl:defsubst generate-avoid-jump (label)
  (emit-jump label))
;  (emit-avoid-jump 'jmp label nil))

(defun generate-return (n-args)
;  (emit lm/move NARGS (machine-num (- -1 n-args)))
  (emit 'K:RETURN))

;; move top of stack (pushed continuation i.e. return address) to tp
;; the jump to it leaving it on the stack...
;;  move.l (sp),tp
;;  jmp    (tp)
;  (cond (*open-procedure-calls?*
;         (emit lm/move (@r 15) TP)
;         (emit m68/jmp (@r 13)))
;       (t
;        (emit m68/jmp (reg-offset TASK task/ireturn))))

(defun generate-return-without-nargs ()
  (emit lm/return))

;efh

(defun generate-general-call (name tail-p dest  n-args)
  (if tail-p
      (emit 'K:TAIL-CALL name `',(1+ n-args))   ;name was (reg-offset P -2)
    (emit 'K:KCALL name `',(1+ n-args) dest)))

;  (cond (*open-procedure-calls?*
;         (emit m68/move .l (reg-offset P -2) TP)
;         (emit m68/jmp (@r 13)))
;        (else
;         (emit m68/jmp (reg-offset TASK task/icall)))))


(zl:defsubst generate-push-address (access)
  (increment-stack)
  (emit lm/push access))

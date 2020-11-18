;;; -*- Mode:LISP; Package:MICRO; Base:8 -*-

(define-micro-function fast-eval (form)
  (declare (:support-vector (i-arg-eval 'eval)))

  ((m-t) pdl-pop)
  (popj-data-type-equal m-t (a-constant (byte-value q-data-type dtp-fix)))
  (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-symbol)) a-symbol)
  (call p3zero)
  ((arg-call ref-support-vector) i-arg-eval)
  ((pdl-push) md)
  ((pdl-push) m-t)
  ((arg-call mmcall) (i-arg 1))
  (popj)

a-symbol
  ((vma) q-pointer m-t)
  ((vma-start-read) add vma (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  ((m-t) md)
  (popj)
  )

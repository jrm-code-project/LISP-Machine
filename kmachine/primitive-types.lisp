;;; -*- Mode:LISP; Package:LISP-INTERNALS -*-

;;;; Primitive types

(defsubst vinc:data-type (ptr)
  (hw:ldb ptr vinc:%%data-type 0))

(defrewrite atom (x)
  `(not (hw:field= ,x gr:*dtp-cons* vinc:%%data-type)))

(defrewrite compiled-function-p (x)
  `(hw:field= ,x gr:*dtp-compiled-function* vinc:%%data-type))

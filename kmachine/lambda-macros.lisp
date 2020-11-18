;;; -*- Mode:LISP; Package:SIMULATOR; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;
;;; DEFSYNONYM
;;;;;;;;;;;;;;;

(defmacro defsynonym (new-name old-name)
  "New-name is a subst for old-name.  Uses rest arg so be careful."
  `(DEFSUBST ,new-name (&REST ARGUMENTS)
     (APPLY (FUNCTION ,old-name) ARGUMENTS))
  )

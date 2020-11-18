;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;;
;;; COMMON-LISP-MACROS
;;;


;;;----------------------------------------------------------------------------
;;; Conditionals
;;;----------------------------------------------------------------------------

(defmacro when (test &body forms)
  "WHEN evaluates the TEST form.  If TEST evaluates to NIL, WHEN returns NIL.
Otherwise, WHEN evaluates the list of FORMS and returns the value of the last
one."
  `(IF ,test (PROGN ,@forms) NIL))

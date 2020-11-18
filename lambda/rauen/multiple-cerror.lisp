;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-
;;;
;;; MULTIPLE-CERROR.LISP
;;;
;;; Easy ways to raise proceedable errors with more than one continuation.


(defun mcerror (report-string &rest strings-and-continuations)

  "Raise a proceedable error with any number of continuations.
REPORT-STRING is a string describing the error; STRINGS-AND-
CONTINUATIONS is a list (string-1 cont-1 string-2 cont-2 ...),
where string-n is a string describing the nth continuation and
cont-n is a function of no arguments to call if the user elects
to proceed with the nth option."

  (eval `(MULTIPLE-CERROR 'MC-ERROR ()
                          (,report-string)
           ,@(convert-strings-and-continuations strings-and-continuations))))


(defun convert-strings-and-continuations (s-and-c-list)
  (if s-and-c-list
      (cons `(,(car s-and-c-list) (FUNCALL ,(cadr s-and-c-list)))
            (convert-strings-and-continuations (cddr s-and-c-list)))
      NIL))

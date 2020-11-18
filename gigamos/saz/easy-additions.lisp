;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

;;;
;;; Provide bug reporter with a mechanism for entering the culprit form, if it can be
;;; isolated as "standalone problematic", for maintainer reproducability.
;;; Syntax of bug would be modified to look like: (...   :sexp-for-bug "( ... )"    ... )
;;;


(defun FIND-FORM-AND-EVAL (bug *current-bug*)
;;; Needs to be smarter about ensuring safety in freak cases such as window system lossage,
;;; cold load stream barfage, etc.
;;;
  (let* ((form-string (get bug ':sexp-for-bug))
         (form (read-from-string form-string)))
    (eval form)))

;;;
;;; Just list the short descriptions of the filtered bug set with their numbers,
;;; one pair to a line, mouseable, (a la zmail) as a selection mechanism
;;;
;;; 34.  Mouse doc line incorrect...
;;; 68.  Mouse left on ...
;;; 128. Clicking mouse right ...
;;;      :
;;;      :
;;;

;;;
;;; Add doc strings to pulldown menu choices.
;;;

;;;
;;;
;;;

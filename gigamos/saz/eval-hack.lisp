



(defvar *sexp-for-bug*)                         ;standalone sexp for throwing into the debugger at right place

;;;
;;; Syntax of bug will be: (...   :sexp-for-bug "( ... )"    ... )
;;;
(defun FIND-FORM-AND-EVAL (bug *current-bug*)
  (let* ((form-string (get bug ':sexp-for-bug)))
form (read-from-string (car (assoc ':sexp-for-bug

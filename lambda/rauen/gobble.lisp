;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; GOBBLE.LISP
;;;
;;; Takes two arguments:
;;;    List of forms to gobble
;;;    Flag indicating whether or not to gobble documentation strings too
;;;
;;; Returns three values:
;;;    Body (forms after declarations and documentation strings)
;;;    List of declarations
;;;    Documentation string (if applicable)
;;;
;;; Weird cases, if gobble-doc-strings-too:
;;;    If there is more than one string in the forms, the second string and
;;;       everything following it (including declarations) are part of the body.
;;;    If the body is null, and a documentation string is present, a list
;;;       containing the documentation string is the body.
;;;    These cases follow from a strict interpretation of decl-spec/doc-string
;;;    syntax described in Common Lisp.
;;;
;;; Macroexpand is used to handle the obnoxious case of macros which expand into
;;;    declarations or documentation strings.




(defun gobble-declarations (list-of-forms &optional gobble-doc-strings-too env)
  (let ((declarations '())
        (doc-string nil)
        (body nil))
    (loop
      (let ((form (INTERPRETER::MACROEXPAND (car list-of-forms) env)))
        (cond ((and (listp form) (eq (car form) 'declare))
               (push form declarations))
              ((and (stringp form) gobble-doc-strings-too (not doc-string))
               (setq doc-string form))
              (t
               (setq body list-of-forms)
               (return))))
      (pop list-of-forms))
    (when (and (null body) doc-string) (setq body (list doc-string)))
    (values body (reverse declarations) doc-string)))

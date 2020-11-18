;;; -*- Mode:LISP; Package:(NC LISP); Readtable:CL; Base:10 -*-


;;;; Declarations

;;; Declarations can be in effect in the global environment
;;; or in effect only for the duration of the compilation
;;; of a file.

;;; this may have to take non-symbols for name
;;; (ie function specs, calling function-spec-get)
(defun get-global-declaration (name declaration-specifier)
  (get name declaration-specifier))

(defun put-global-declaration (name declaration-specifier value)
  (setf (get name declaration-specifier) value))

(defsetf get-global-declaration put-global-declaration)

;;; maybe this should be part of the environment structure
(defvar *local-declarations* '())

(defun put-local-declaration (name declaration-specifier value)
  (push (list declaration-specifier name value) *local-declarations*))

(defun get-declaration (name declaration-specifier)
  (or (dolist (decl *local-declarations*)
        (and (eq (first decl) declaration-specifier)
             (equal (second decl) name)
             (return (third decl))))
      (get-global-declaration name declaration-specifier)))

(defmacro def-declaration (name decl-spec value)
  `(PROGN
     (EVAL-WHEN (EVAL LOAD)
       (PUT-GLOBAL-DECLARATION ',name ',decl-spec ,value))
     (EVAL-WHEN (COMPILE)
       (PUT-LOCAL-DECLARATION ',name ',decl-spec ,value))))

;;; a bit of a crock
(eval-when (eval load)
  (setf (nlisp:macro-function 'def-declaration)
        (macro-function 'def-declaration)))

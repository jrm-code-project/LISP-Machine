;;; -*- Mode:LISP; Package:LISP-IO; Readtable:CL; Base:10 -*-
;;;
;;; INDENTATION.LISP
;;;
;;; Common Lisp indentation information for ZWEI (?!) and the grinder.


(defvar *indentation-table* (make-hash-table)
  "Indentation information table.")

(defun get-indentation (key)
  "Look up how to indent a list whose car is KEY.  If KEY is a keyword, look up
how to indent that particular kind of list (e.g. :vertical-indentation).
Return an indent-info structure."
  (let ((indentation (gethash key *indentation-table*)))
    (if indentation
        indentation
        (gethash :default-indentation *indentation-table*))))

(defstruct (indent-info
             (:constructor make-indent-info (top-line-forms
                                             special-indentations
                                             rest-indentation)))
  top-line-forms
  special-indentations
  rest-indentation)

(defun nth-indentation-record (n indent-info)
  (if (< n (length (indent-info-special-indentations indent-info)))
      (nth n (indent-info-special-indentations indent-info))
      (indent-info-rest-indentation indent-info)))

(defun nth-element-indentation (n indent-info)
  (second (nth-indentation-record n indent-info)))

(defun nth-element-indent-relative-to (n indent-info)
  (first (nth-indentation-record n indent-info)))

(defun nth-element-indentation-format (n indent-info)
  (third (nth-indentation-record n indent-info)))

(defmacro defindent (symbols top-line-forms &body element-descriptors)
  `(PROGN
     (DOLIST (SYMBOL ',(if (listp symbols) symbols (list symbols)))
       (PUTHASH
         SYMBOL
         (MAKE-INDENT-INFO ,top-line-forms
                           ',(my-nbutlast element-descriptors)
                           ',(car (my-last element-descriptors)))
         *INDENTATION-TABLE*))
     T))

(defun my-nbutlast (list)
  (unless (null (cdr list))
    (cons (car list) (my-nbutlast (cdr list)))))

(defun my-last (list)
  (if (not (consp (cdr list)))
      list
      (my-last (cdr list))))

;;; name clash (pprint (:vertical-indentation ...))

(defindent :default-indentation 2
  (:car  0 nil)
  (:car  1 nil)
  (:cadr 0 nil))

(defindent :vertical-indentation 1
  (:car  0 nil))

(defindent :flet-binding-list-indentation 1
  (:car  0 :flet-binding-indentation))

(defindent :flet-binding-indentation 2
  (:car  0 nil)
  (:car  2 :vertical-indentation)
  (:car  2 nil))

(defindent :array-indentation 1
  (:car  0 nil)
  (:car  0 :array-indentation))

(defindent block 2
  (:car  0 nil)
  (:car  1 nil))

(defindent eval-when 2
  (:car  0 nil)
  (:car  1 :vertical-indentation))

(defindent (flet labels macrolet) 2
  (:car  0 nil)
  (:car  1 :flet-binding-list-indentation)
  (:car  1 nil))

(defindent (let let* compiler-let) 2
  (:car  0 nil)
  (:car  1 :vertical-indentation)
  (:car  1 nil))

(defindent progn 2
  (:car  0 nil)
  (:car  1 nil)
  (:cadr 0 nil))

(defindent progv 3
  (:car  0 nil)
  (:car  1 :vertical-indentation)
  (:cadr 0 :vertical-indentation)
  (:car  1 nil))

(defindent unwind-protect 2
  (:car  0 nil)
  (:car  3 nil)
  (:car  1 nil))



(defindent cond 2
  (:car  0 nil)
  (:car  1 :vertical-indentation)
  (:cadr 0 :vertical-indentation))

(defindent (defun defmacro named-lambda) 3
  (:car  0 nil)
  (:car  1 nil)
  (:cadr 0 :vertical-indentation)
  (:car  1 nil))

(defindent (defvar defparameter defconstant) 3
  (:car  0 nil)
  (:car  1 nil))

(defindent (do do*) 2
  (:car  0 nil)
  (:car  1 :vertical-indentation)
  (:cadr 0 nil)
  (:car  1 nil))

(defindent multiple-value-bind 2
  (:car  0 nil)
  (:car  1 :vertical-indentation)
  (:car  3 nil)
  (:car  1 nil))

(defindent (when unless) 2
  (:car  0 nil)
  (:car  1 nil))

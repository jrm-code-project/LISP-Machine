;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-
;;;
;;; K-LISP.LISP
;;;
;;; This file contains code to create the KI-LISP and KI-USER packages.  The
;;; KI-LISP package contains and exports the symbols defined in Common Lisp.
;;; It is read-locked, so no more symbols may be interned in it.
;;;
;;; This file must be loaded before a Common Lisp function, macro, or variable
;;; can be implemented.  To implement FOO in a package BAR, put a shadowing
;;; import at the top of the file that implements FOO:
;;;
;;;          -*- Package: BAR -*-
;;;          (shadowing-import KI-LISP:FOO)
;;;
;;; Now, while in package BAR, typing (defun FOO ...) or (defvar FOO ...)
;;; will make the definition of FOO visible in both the KI-LISP and the BAR
;;; packages.


;;; Make the KI-LISP package.

(make-package "KI-LISP" :nicknames NIL
                       :use       NIL)



;;; Now it's time to put all the symbols into KI-LISP.  If the symbols we want
;;; in KI-LISP already exist (for the K cold loader, for instance), we IMPORT
;;; each of these symbols into KI-LISP, then bash their package cells to the
;;; KI-LISP package.  Then export the symbols from KI-LISP.

#+k
(let ((ki-lisp-package (find-package 'ki-lisp)))
  (dolist (symbol ***LIST-OF-SYMBOLS***)
    (import symbol 'ki-lisp)
    (setf (symbol-package symbol) ki-lisp-package)
    (export symbol ki-lisp-package)))



;;; On the other hand, if KI-LISP is being built for an existing machine (say,
;;; the Lambda), we want to make new symbols.  This is done by interning print
;;; names in the virgin-pure KI-LISP package.  The value and function cells of
;;; the new symbols are initialized to their counterparts in the old LISP
;;; package.  Then the symbols are exported from KI-LISP.
;;;
;;; Some of the symbols (CL special forms, defun, t, nil, etc.) must be kept
;;; EQ to their older counterparts.  This is thanks to lossage in the concept
;;; of a package system.  So they are special cased.

#+lambda
(defvar *common-lisp-special-symbols*
        '(block catch compiler-let declare eval-when flet function go if labels
          let let* macrolet multiple-value-call multiple-value-prog1 progn progv
          quote return-from setq tagbody the throw unwind-protect t nil
          lambda named-lambda defun defvar defparameter defconstant
          &optional &rest &aux &key &allow-other-keys &body &whole &environment))

#+lambda
(let ((ki-lisp-package (find-package 'ki-lisp)))
  (dolist (symbol si::initial-lisp-symbols)
    (let ((new-symbol (intern (symbol-name symbol) ki-lisp-package)))
      (when (and (boundp symbol) (not (member symbol '(t nil))))
        (setf (symbol-value new-symbol) (symbol-value symbol)))
      (when (fboundp symbol)
        (if (macro-function symbol)
            (setf (macro-function new-symbol) (macro-function symbol))
            (setf (symbol-function new-symbol) (symbol-function symbol))))
      (export new-symbol ki-lisp-package)))
  (shadowing-import *common-lisp-special-symbols* ki-lisp-package)
  (export *common-lisp-special-symbols* ki-lisp-package))


;;; Lock the KI-LISP package.


;;; Make the KI-USER package.

(make-package "KI-USER" :nicknames NIL
                       :use       'KI-LISP)

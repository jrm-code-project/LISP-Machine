; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;The major mode custiomizations.
;

(define-mode fundamental nil *fundamental-bindings* *fundamental-syntax*)

(define-mode LISP fundamental nil *lisp-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* t
             )


(define-mode text fundamental nil *text-syntax*
             *display-matching-paren* 0
             *function-search* nil
             *paragraph-text-style* t
             )

(defvar *bolio-syntax* (derive-syntax *text-syntax*))

(setf (get-char-syntax-bit *bolio-syntax* #\{ paren-open) 1)
(setf (get-char-syntax-bit *bolio-syntax* #\} paren-close) 1)

(define-mode R text nil *bolio-syntax*
             *display-matching-paren* -1.0
             *function-search* t
             comment-column 0
             comment-start ""
             comment-begin " ")

(define-mode bolio r nil *bolio-syntax*
             *display-matching-paren* -1.0
             *function-search* t
             comment-column 0
             comment-start ".c "
             comment-begin ".c "
             ;; Bolio only runs on PDP-10s currently.
             *line-separator-length* 2)

(defvar *lsb-syntax* (derive-syntax *lisp-syntax*))

(setf (get-char-syntax-bit *lsb-syntax* #\{ paren-open) 1)
(setf (get-char-syntax-bit *lsb-syntax* #\} paren-close) 1)

(defvar *[]-syntax* (derive-syntax *fundamental-syntax*))
(setf (get-char-syntax-bit *[]-syntax* #\[ paren-open) 1)
(setf (get-char-syntax-bit *[]-syntax* #\] paren-close) 1)

(define-mode lsb lisp nil *lsb-syntax*)

(define-mode ll lisp nil nil
             *lisp-listener* t
             *display-matching-paren* 1.2)

;(define-mode test ll nil nil
;            ;;Perhaps this should be a minor mode?
;            *editor-device-mode* nil
;            )

(define-mode dcl fundamental nil nil
             comment-column 0
             comment-start "!"
             comment-begin "! "
             comment-end ""
             *display-matching-paren* 0
             *function-search* nil)

(define-mode macsyma fundamental nil *[]-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             )

(define-mode bliss fundamental nil nil
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             comment-column 0
             comment-start "!"
             comment-begin "! "
             comment-end ""
             )

(defvar *c-syntax* (derive-syntax *fundamental-syntax*))
(setf (get-char-syntax-bit *c-syntax* #\[ paren-open) 1)
(setf (get-char-syntax-bit *c-syntax* #\] paren-close) 1)
(setf (get-char-syntax-bit *c-syntax* #\{ paren-open) 1)
(setf (get-char-syntax-bit *c-syntax* #\} paren-close) 1)

(define-mode c fundamental nil *c-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             comment-column 0
             comment-start "/*"
             comment-begin "/* "
             comment-end "*/"
             )

(define-mode fortran fundamental nil nil
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             comment-column 0
             comment-start "c"
             comment-begin "C "
             comment-end ""
             )

(define-mode macro32 fundamental nil *[]-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             comment-column 40
             comment-start ";"
             comment-begin "; "
             comment-end ""
             )

(define-mode pascal fundamental nil *c-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             comment-column 0
             comment-start "/*"
             comment-begin "/* "
             comment-end "*/"
             *function-search* nil
             )

(define-mode pli fundamental nil *[]-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* nil
             *function-search* nil
             comment-column 0
             comment-start "/*"
             comment-begin "/* "
             comment-end "*/"
             )

(defvar *scribe-syntax* (derive-syntax *fundamental-syntax*))
(setf (get-char-syntax-bit *scribe-syntax* #\[ paren-open) 1)
(setf (get-char-syntax-bit *scribe-syntax* #\] paren-close) 1)
(setf (get-char-syntax-bit *scribe-syntax* #\{ paren-open) 1)
(setf (get-char-syntax-bit *scribe-syntax* #\} paren-close) 1)

(define-mode scribe text nil *scribe-syntax*
             *display-matching-paren* -1.0
             *paragraph-text-style* t
             *function-search* nil
             comment-column 0
             comment-start "@comment["
             comment-begin "@comment["
             comment-end "]"
             )



(editor-bind-key #\tab insert-tab fundamental)
;These are redundant.
;(editor-bind-key #\rubout backward-delete-character fundamental)
;(editor-bind-key #\control-rubout backward-delete-hacking-tabs fundamental)
(editor-bind-key #\tab indent-for-lisp lisp)
;...these too since this is now the default
;(editor-bind-key #\rubout backward-delete-hacking-tabs lisp)
;(editor-bind-key #\control-rubout backward-delete-character lisp)

;
;Font change command for BOLIO and R modes.
;

(defun insert-font-change ()
 (send *editor-cursor* :insert-char #\control-f)
 (send *editor-cursor* :insert-char (make-char *editor-current-key* 0 0))
 nil)

(editor-bind-key #\control-meta-0 insert-font-change r)
(editor-bind-key #\control-meta-1 insert-font-change r)
(editor-bind-key #\control-meta-2 insert-font-change r)
(editor-bind-key #\control-meta-3 insert-font-change r)
(editor-bind-key #\control-meta-4 insert-font-change r)
(editor-bind-key #\control-meta-5 insert-font-change r)
(editor-bind-key #\control-meta-6 insert-font-change r)
(editor-bind-key #\control-meta-7 insert-font-change r)
(editor-bind-key #\control-meta-8 insert-font-change r)
(editor-bind-key #\control-meta-9 insert-font-change r)
(editor-bind-key #\control-meta-* insert-font-change r)

(defparameter default-major-mode-alist
              (append '(("LISP" "LISP")
                        ("VASL" "FUNDAMENTAL")
                        ("UNVASL" "TEXT")
                        ("TEXT" "TEXT")
                        ("TEMPORARY" "LISP")
                        ("LIST" "TEXT")
                        ("INIT" "LISP")
                        ("PDR" "LISP")
                        ("PSD" "LISP")
                        ("PUBDCL" "FUNDAMENTAL")
                        ("PUBDOC" "BOLIO")
                        ("SYSDCL" "FUNDAMENTAL")
                        ("SYSDOC" "BOLIO")
                        ("MODDCL" "FUNDAMENTAL")
                        ("MODDOC" "BOLIO")
                        ("MACROS" "FUNDAMENTAL")
                        ("SYSTEM" "FUNDAMENTAL")
                        ("MAX" "MACRO32")
                        ("MAR" "MACRO32")
                        ("FORTRAN" "FORTRAN")
                        ("MAC" "MACSYMA")
                        ("PLI" "PLI")
                        ("BLI" "BLISS")
                        ("C" "C")
                        ("COM" "DCL")
                        ("INP" "DCL")
                        ("OPT" "DCL")
                        )
                      default-major-mode-alist))

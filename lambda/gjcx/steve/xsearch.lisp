; -*- package:steve; readtable:cl; mode:lisp; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

(defvar *global-search-state* nil)
(defvar *last-global-search-string* nil)

(defun global-string-search (&optional (string *last-global-search-string* rg))
  (unless (null rg) (setq *global-search-state* *all-buffers*))
  (setq *last-global-search-string* string)
  (loop while (not (null *global-search-state*))
        for buffer = (car *global-search-state*)
        for point = (point buffer)
        do (setq *global-search-state* (cdr *global-search-state*))
        do (print buffer echo-area-window)
        do (send point :move (buffer-content buffer) 0)
        if (search-forward string point)
        do (progn (select-point-in-current-window point)
                  (return t))))

(editor-bind-key #\meta-\, search-all-buffers-for-string)

(defun search-all-buffers-for-string ()
  (with-query-line
   (oustr "String: " terminal-io)
   (let ((string (prescan #'readline-with-exit)))
     (when (null (global-string-search string))
       (ed-warn "Search Fails"))))
  nil)

(editor-bind-key #\control-meta-\, continue-global-string-search)

(defun continue-global-string-search ()
  (when (null (global-string-search))
    (ed-warn "Search Fails"))
  nil)

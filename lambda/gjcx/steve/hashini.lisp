; -*-mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;Functions to build command tables for the editor.
;Each of these functions adds the commands which are needed for some
;logically distinct functionality. They are prefixed with CT- for
;"Command Table-".
;Any previous bindings are replaced, so order is important.

(defun ct-self-insert-graphics (table)
  (loop for i from 0 below 256
        for char = (int-char i)
        if (and (graphic-charp char)
                (not (lowercasep char)))
        do (%internal-bindery char 'self-insert table)))

(defun ct-self-insert-alphanumerics (table)
  (loop for i from 0 below 256
        for char = (int-char i)
        if (alphanumericp char)
        do (%internal-bindery char 'self-insert table)))

(defun ct-basic-intra-line-editing (table)
  (%internal-bindery #\control-d 'delete-character table)
  (%internal-bindery #\rubout 'backward-delete-character table)
  (%internal-bindery #\meta-d kill-word table)
  (%internal-bindery #\meta-rubout backward-kill-word table)
  (%internal-bindery #\control-meta-? 'editor-help table)
  (%internal-bindery #\meta-? 'describe-key table)
  )

(defun ct-basic-cursor-movement (table)
  (%internal-bindery #\control-f 'forward-character table)
  (%internal-bindery #\control-b 'backward-character table)
  (%internal-bindery #\control-a 'beginning-of-line table)
  (%internal-bindery #\control-e 'end-of-line table)
  (%internal-bindery #\control-n 'down-real-line table)
  (%internal-bindery #\control-p 'up-real-line table)
  (%internal-bindery #\meta-f 'forward-word table)
  (%internal-bindery #\meta-b 'backward-word table)
  (%internal-bindery #\control-v 'next-screen table)
  (%internal-bindery #\meta-v 'previous-screen table)
  (%internal-bindery #\meta-> 'goto-end table)
  (%internal-bindery #\meta-< 'goto-beginning table)
  (%internal-bindery #\meta-r 'move-to-screen-edge table))

(defun ct-file-and-buffers (table)
  (%internal-bindery '(#\control-x #\control-f) 'find-file table)
  (%internal-bindery '(#\control-x #\control-v) 'visit-file table)
  (%internal-bindery '(#\control-x #\control-r) 'read-file table)
  (%internal-bindery '(#\control-x #\control-w) 'write-file table)
  (%internal-bindery '(#\control-x #\B) 'select-buffer table)
  (%internal-bindery '(#\control-x #\control-b) 'list-buffers table)
  )

(defun ct-arguments (table)
  (%internal-bindery #\control-u '(universal-argument) table)
  (%internal-bindery #\meta-0 '(auto-argument 0) table)
  (%internal-bindery #\meta-1 '(auto-argument 1) table)
  (%internal-bindery #\meta-2 '(auto-argument 2) table)
  (%internal-bindery #\meta-3 '(auto-argument 3) table)
  (%internal-bindery #\meta-4 '(auto-argument 4) table)
  (%internal-bindery #\meta-5 '(auto-argument 5) table)
  (%internal-bindery #\meta-6 '(auto-argument 6) table)
  (%internal-bindery #\meta-7 '(auto-argument 7) table)
  (%internal-bindery #\meta-8 '(auto-argument 8) table)
  (%internal-bindery #\meta-9 '(auto-argument 9) table)
  (%internal-bindery #\control-0 '(argument-digit 0) table)
  (%internal-bindery #\control-1 '(argument-digit 1) table)
  (%internal-bindery #\control-2 '(argument-digit 2) table)
  (%internal-bindery #\control-3 '(argument-digit 3) table)
  (%internal-bindery #\control-4 '(argument-digit 4) table)
  (%internal-bindery #\control-5 '(argument-digit 5) table)
  (%internal-bindery #\control-6 '(argument-digit 6) table)
  (%internal-bindery #\control-7 '(argument-digit 7) table)
  (%internal-bindery #\control-8 '(argument-digit 8) table)
  (%internal-bindery #\control-9 '(argument-digit 9) table)
  (%internal-bindery #\control-meta-0 '(argument-digit 0) table)
  (%internal-bindery #\control-meta-1 '(argument-digit 1) table)
  (%internal-bindery #\control-meta-2 '(argument-digit 2) table)
  (%internal-bindery #\control-meta-3 '(argument-digit 3) table)
  (%internal-bindery #\control-meta-4 '(argument-digit 4) table)
  (%internal-bindery #\control-meta-5 '(argument-digit 5) table)
  (%internal-bindery #\control-meta-6 '(argument-digit 6) table)
  (%internal-bindery #\control-meta-7 '(argument-digit 7) table)
  (%internal-bindery #\control-meta-8 '(argument-digit 8) table)
  (%internal-bindery #\control-meta-9 '(argument-digit 9) table)
  (%internal-bindery #\meta-\- '(auto-negative-digit) table)
  (%internal-bindery #\control-\- '(negative-digit) table)
  (%internal-bindery #\control-meta-\- '(negative-digit) table))

(defun ct-bit-prefixes (table)
  (%internal-bindery #\control-^ '(bit-prefix 1 "C-") table)
  (%internal-bindery #\altmode '(bit-prefix 2 "M-") table)
  (%internal-bindery #\control-z '(bit-prefix 3 "C-M-") table))

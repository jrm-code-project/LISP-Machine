; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Function to set the syntax of chars.
;

(defvar setsyntax-buffer)
(defvar setsyntax-point)
(defvar dired-style-commands)

(defun initialize-dired-commands (&aux table)
  (setq table (make-empty-key-binding-table))
  (ct-self-insert-graphics table)
  (ct-arguments table)
  (ct-basic-cursor-movement table)
  (ct-basic-intra-line-editing table)
  (ct-bit-prefixes table)
  (%internal-bindery #\control-] 'abort-recursive-edit table)
  (%internal-bindery #\control-z '(bit-prefix 3 "C-M-") table)
  (%internal-bindery #\Control-\^ '(bit-prefix 1 "C-") table)
  (%internal-bindery #\altmode '(bit-prefix 2 "M-") table)
  (%internal-bindery #\control-meta-z 'exit-editor table)
  (%internal-bindery #\control-n 'down-real-line table)
  (%internal-bindery #\control-p 'up-real-line table)
  (%internal-bindery #\control-k 'kill-lines table)
;  (%internal-bindery #\Y 'dired-style-self-insert table)
;  (%internal-bindery #\N 'dired-style-self-insert table)
;  (%internal-bindery #\D 'dired-style-self-insert table)
;  (%internal-bindery #\$ 'dired-style-self-insert table)
  (%internal-bindery #\meta-> 'goto-end table)
  (%internal-bindery #\meta-< 'goto-beginning table)
  (%internal-bindery #\meta-x 'extended-command table)
  (%internal-bindery #\control-f 'forward-character table)
  (%internal-bindery #\control-b 'backward-character table)
  (%internal-bindery #\control-d 'delete-character table)
  (%internal-bindery #\rubout 'backward-delete-character table)
  (%internal-bindery #\control-meta-? 'editor-help table)
  (%internal-bindery #\meta-? 'describe-key table)
  (%internal-bindery #\control-a 'beginning-of-line table)
  (%internal-bindery #\control-e 'end-of-line table)
  (%internal-bindery #\control-q '(quoted-insert) table)
  table)

(defun characters-of-syntax (syntax-bit)
  (loop for i from 0 below 256
        if (of-syntax (int-char i) syntax-bit)
        collect (int-char i) into char-list
        finally (return (coerce char-list 'string))))

(defun make-setsyntax-buffer (&aux (width (1-& (window-stream-x-size
                                                (edit-cursor-window
                                                 *editor-cursor*))))
                               line)
  ;;The principle problem left with this is what to do about #\RETURN.
  ;;It cannot be typed into a buffer in any way normally.
  (unless (boundp 'dired-style-commands)
    (setq dired-style-commands
          (initialize-dired-commands)))
  (setq setsyntax-buffer
        (make-instance 'buffer
                       :name "setsyntax Buffer"
                       :content (make-instance 'line
                                               :chars ""
                                               :char-count 0
                                               :bashed? 0
                                               :graphic-length width
                                               :buffer-pointers nil
                                               :previous nil
                                               :next nil)
                       :file-name nil
                       :environment nil
                       :mark-ring (make-vector mark-ring-size :initial-element nil)
                       :mark-ring-index 0
                       :access 0
                       :modified? nil))
  (setq setsyntax-point (create-edit-cursor setsyntax-buffer))
  (bind-in-buffer setsyntax-buffer '*editor-bindings*
                  dired-style-commands)
  (setf (line-buffer (buffer-content setsyntax-buffer)) setsyntax-buffer)
  (setq line
        (make-line setsyntax-buffer (buffer-content setsyntax-buffer) nil
                   " Syntax Types are:"))
  (loop for (bit-num syntax . fuck-me-incompatibly) in syntax-bit-map
        for prev first line then syntax-line
        for syntax-line = (make-line setsyntax-buffer prev nil
                                     (string-append
                                      syntax " "
                                      (characters-of-syntax bit-num))))
  (send setsyntax-point :move (line-next line) 0)
  (setf (edit-cursor-home-line setsyntax-point)
        (buffer-content setsyntax-buffer)))

(defun dired-style-self-insert ()
  (send *editor-cursor* :set-pos 0)
  (cond ((char= (send *editor-cursor* :get-char-forward) #\space)
         (send *editor-cursor* :insert-char (char-upcase *editor-current-key*))
         (send *editor-cursor* :delete-char)
         (unless (null (line-next (bp-line *editor-cursor*)))
           (send *editor-cursor* :to-beginning-of-next-line)))
        (t (send *editor-cursor* :to-beginning-of-next-line)
           (dired-style-self-insert)))
  nil)

(defunmetax syntax-modification ()
  (let ((old-point *editor-cursor*)
        ;;Save old state.
        (*last-buffer-selected* *editor-buffer*))
     (make-setsyntax-buffer)
     (unwind-protect
      (progn (select-point-in-current-window setsyntax-point)
             (cond ((recursive-edit "Set Character Syntax")
                    (parse-and-execute-setsyntax-buffer))
                   (t (ed-warning "Syntax not modified"))))
      (select-point-in-current-window old-point))))

(defun parse-and-execute-setsyntax-buffer ()
  (loop with syntax-type = nil
        with syntax-type-atom = nil
        with syntax-type-bit = nil
        for line first (buffer-content setsyntax-buffer) then (line-next line)
        until (null line)
        if (and (+p (line-char-count line))
                (not (char= (char (line-chars line) 0) #\space)))
        do (progn (setq syntax-type (extract-syntax-type line))
                  (setq syntax-type-atom (intern-soft syntax-type 'steve))
                  (unless (or (null syntax-type-atom)
                              (null (get syntax-type-atom 'syntax-bit)))
                    (setq syntax-type-bit (get syntax-type-atom 'syntax-bit))
                    (loop for i from 0 below 256
                          for char = (int-char i)
                          do (setf (get-char-syntax-bit
                                    syntax-table char syntax-type-bit)
                                   0))
                    (set-to-syntax syntax-table
                                   (substring (line-chars line)
                                              (1+& (string-length syntax-type))
                                              (line-char-count line))
                                   syntax-type-bit)))))

(defun extract-syntax-type (line)
  (extract-word-from-string (line-chars line) 0 (line-char-count line)))

(defun extract-word-from-string (string skip max)
  ;;This is horibbly kluged to allow dash in a word.
  (loop for i from skip below max
        until (or (alphanumericp (char string i))
                  (char= (char string i) #\-))
        finally
        (return
         (loop for j from i below max
               for char = (char string j)
               until (and (not (alphanumericp char)) (not (char= char #\-)))
               finally (return (substring string i (min& max j)))))))

(defun syntax-numeric-mask (syntax-type)
  (loop for (n name mask) in syntax-bit-map
        if (string-equal name syntax-type) return mask
        finally (return 0)))

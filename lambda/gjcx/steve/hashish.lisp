; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;History
;  Added compiler warning for EDITOR-DEFUN-KEY  -- TIM 5/8/84

;
;Key binding tables.
;

(defvar *fundamental-bindings*)

(defvar *editor-bindings*)

;
;Key binding support code.
;

(defun define-bit-prefix (char bits &optional (prompt-string ""))
  (internal-bindery char `(bit-prefix ,bits ,prompt-string)))

(defmacro editor-defun-key (key name &body body)
  `(progn 'compile
          (defcom ,name () ,@body)
          (editor-bind-key ,key ,name)
          ',name))

(defun (editor-defun-key source-code-rewrite) (form)
  (warn "EDITOR-DEFUN-KEY is an obsolete form, use DEFUN and EDITOR-BIND-KEY")
  nil)

(defmacro editor-bind-key (key binding &optional (mode nil))
 `(internal-bindery ,key ',binding
                    ,@(and mode (list (list 'quote mode)))))

;Use this to defun a command. Some day it will make it accessible as
;a Meta-X command, and so on.
(defmacro defcom (name args &body body)
 `(defun ,name ,args ,@body))

(defmacro lookup-key-in-table (key table)
  `(lookup-key-in-superior-tables ,key ,table))

(defmacro ed-read-char ()
  '(let ((char (canonicalize-control-characters (read-char&save terminal-io))))
     (let ((bits (char-bits char)))
       (cond ((NOT (ZEROP bits))
                (make-char (char-upcase (MAKE-CHAR char)) bits))
             (:no-bits
                char)))))

(defun read-canonical-char ()
  (unless (type-ahead-p)
    (send (edit-cursor-window *editor-cursor*) :make-cursor-visible))
  (let ((char (ed-read-char)))
    (when (char= char #\^G) (ed-warn "Aborted"))
    char))

;Returns two values: the char and its binding.
(defun read-key (&optional (stuff-me nil))
 (let* ((char (or stuff-me (read-canonical-char)))
        (binding (or (lookup-key-in-table char *editor-bindings*)
                     (lookup-key-in-table (char-upcase char)
                                          *editor-bindings*))))
  (if (not (consp binding))
      (values char binding) ;A normal function.
      (apply (car binding) char (cdr binding)))))

(defun read-one-key (&optional (table *editor-bindings*)
                               (char nil))
  ;; This reads a key without the ^G escape.
  (when (null char) (setq char (ed-read-char)))
  (values char (or (lookup-key-in-table char table)
                   (lookup-key-in-table (char-upcase char) table))))

;This may be slightly better than the version below, but its not worth
;testing and debugging.
;(defun read-any-key (&optional (chr nil))
;  (multiple-value-bind (char command) (read-one-key *editor-bindings* chr)
;    (if (not (consp command))
;       (values char binding)
;       (apply (car binding) char (cdr binding)))))

(defun read-any-key (&optional (stuff-me nil))
  (let* ((char (or stuff-me (ed-read-char)))
         (binding (or (lookup-key-in-table char *editor-bindings*)
                      (lookup-key-in-table (char-upcase char)
                                           *editor-bindings*))))
    (if (not (consp binding))
        (values char binding) ;A normal function.
        (apply (car binding) char (cdr binding)))))

(defun bit-prefix (command-char bits &optional (echo-string nil))
  (or (null *display-prefixes*)
      (type-ahead-p)
      (null echo-string)
      (with-prefix-echo-line
       (send terminal-io :oustr echo-string)
       (peek-char&save terminal-io)))
  (let ((char (read-canonical-char)))
    ;;We use upper case characters only because lowercase
    ;;characters don't map back to string chars.
    (read-key (make-char (char-upcase char) (logior& bits (char-bits char))))))

(defun quoted-insert (command-char)
  (or (null *display-prefixes*)
      (type-ahead-p)
      (with-prefix-echo-line
       (oustr "<Quoted-" terminal-io)
       (peek-char&save)))
  (values (ed-read-char) 'self-insert))

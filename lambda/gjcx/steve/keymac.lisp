;-*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Keyboard macros.
;

;
(defvar kbd-macro-iterations 0)
(defvar kbd-macro-to-iterate nil)

(editor-bind-key '(#\control-x #\() start-kbd-macro)

(defun start-kbd-macro ()
  (when (memq '|Def| *minor-modes*)
    (ed-lose "Already defining a keyboard macro"))
  (push '|Def| *minor-modes*)
  (setup-mode-area)
  (setq save-chars-for-keyboard-macros t
        keyboard-macro-char-list nil)
  nil)

(editor-bind-key '(#\control-x #\)) end-kbd-macro)

(defun end-kbd-macro ()
  (cond ((not (null save-chars-for-keyboard-macros))
         (setq keyboard-macro-char-list (nreverse keyboard-macro-char-list))
         (setq save-chars-for-keyboard-macros nil)
         (setq *minor-modes* (delq '|Def| *minor-modes*))
         (setup-mode-area))
        ((zerop (setq kbd-macro-iterations (sub1 kbd-macro-iterations))) nil)
        (t (setq executing-keyboard-macro kbd-macro-to-iterate)))
  nil)

(editor-bind-key '(#\control-x #\E) call-last-kbd-macro)

(defun call-last-kbd-macro ()
  (when (not (null keyboard-macro-char-list))
    ;;For safety sake we turn off passall in a keyboard macro.
    ;;it is automatically reset when the KBD macro finishes.
    ;;With-no-passall knows about KBD macros, and leaves passall off when
    ;;executing one.
    (send terminal-io :set-device-mode :passall nil)
    (setq executing-keyboard-macro keyboard-macro-char-list)
    (setq kbd-macro-to-iterate keyboard-macro-char-list)
    (setq kbd-macro-iterations *argument*))
  nil)

(bindmetax name-kbd-macro name-kbd-macro)

(defun name-kbd-macro (&optional (name nil))
  (when (null keyboard-macro-char-list)
    (ed-lose "No Kbd Macro to name"))
  (when (null name)
    (setq name (mx-prompter #'read-symbol "Macro Name: ")))
  (let ((keystrokes (with-query-line
                     (format terminal-io "Key: ")
                     (read-key)))
        (function (eval `(lambda ()
                           (send terminal-io :set-device-mode :passall nil)
                           (setq kbd-macro-iterations *argument*)
                           (setq kbd-macro-to-iterate
                                 (setq executing-keyboard-macro
                                       ',(copy-list keyboard-macro-char-list)))
                           nil))))
    (fset-carefully name function)
    (defcommand (string name) meta-x-command-table name)
    (with-query-line
     (format terminal-io "Bind Kbd macro to key ~s? " keystrokes)
     (when (ed-y-or-n-p "Type Y to bind current macro to key ~s" keystrokes)
       (internal-bindery keystrokes function)))))

(editor-bind-key '(#\control-x #\Q) Kbd-Macro-query)

(defun Kbd-Macro-query ()
  (cond ((not (null save-chars-for-keyboard-macros)))
        ((argument?)
         (cond ((null (recursive-edit "Kbd Macro Query"
                                      '(executing-keyboard-macro) nil))
                (ed-warn "Aborted Kbd Macro"))
               (t (with-notify-line-remaining
                   (format terminal-io "Continuing Kbd Macro ~a"
                           executing-keyboard-macro)))))
        ((with-query-line
          (oustr "Kbd Macro Query (y or n): " terminal-io)
          (ed-y-or-n-p "Type Y to Continue, N to abort Keyboard macro")))
        (t (ed-warn "Aborted Kbd Macro")))
  nil)

(bindmetax view-kbd-macro view-kbd-macro)

(defun view-kbd-macro (&optional (name nil))
  (setq name (if (null name) keyboard-macro-char-list
                 (try-to-extract-kbd-macro-definition name)))
  (if (null name) (ed-warn "Definition Inaccessible")
      (with-notify-line-remaining
       (oustr "Defining Key Sequence: " terminal-io)
       (mapc #'(lambda (x) (princ x terminal-io) (princ " " terminal-io))
             name))))

(defun try-to-extract-kbd-macro-definition (name)
  (when (setq name (intern-soft (string-upcase name) 'steve))
    (when (setq name (si:symbol-function-definition name))
      (when (setq name (fourth name))
        (when (setq name (third name))
          (when (setq name (second name))
            name))))))

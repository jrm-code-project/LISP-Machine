;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:ZL -*-

(DEFMINOR COM-AUTO-SAVE-MODE AUTO-SAVE-MODE "with Auto Save" 2
  "Minor mode in which buffer is saved every so many keystrokes."
          ()
  (COMMAND-HOOK 'AUTO-SAVE-HOOK *POST-COMMAND-HOOK*))

(DEFPROP AUTO-SAVE-HOOK -20 COMMAND-HOOK-PRIORITY)
(defvar *AUTO-SAVE-CHAR-COUNT* 0.)
(defvar *AUTO-SAVE-MAX-CHARS* 1000.)
(defvar *auto-save-generations-to-keep* 3)
(DEFUN AUTO-SAVE-HOOK (ignore)
  (if (and ( *auto-save-char-count* *auto-save-max-chars*)
           (neq *interval* (window-interval *mini-buffer-window*)))
      (progn (save-buffer *interval*)
             (setq *AUTO-SAVE-CHAR-COUNT* 0))
    (incf *auto-save-char-count*)))


(DEFPROP AUTO-SAVE-HOOK DOCUMENT-AUTO-SAVE-HOOK HOOK-DOCUMENTATION-FUNCTION)

(DEFUN DOCUMENT-AUTO-SAVE-HOOK (IGNORE )
  (PRINC "Auto save buffer after *AUTO-SAVE-MAX-CHARS* keystrokes."))

; -*-mode: lisp; package: steve; readtable: cl; -*-
;Wednesday the twenty-seventh of July, 1983; 6:50:18 pm

;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Should I try to write an rmail package now???
;

(defparameter rmail-bindings nil)

(defmacro rmail-bind (key binding documentation &rest doc-args)
  `(progn (setq rmail-bindings
                (nconc (delq (assoc ,key rmail-bindings) rmail-bindings)
                       (list (list ,key ',binding ,documentation ,@doc-args))))
          nil))

(defunmetax rmail-command-help ()
  (overwrite-start)
  (loop for (key binding doc-string . doc-args) in rmail-bindings
        do (format overwrite-stream "~%~%Key ~s is bound to ~s~%"
                   key binding)
        do (apply #'format overwrite-stream doc-string doc-args))
  (more-aborter "*done*")
  nil)

(defvar *rmail-saved-bindings* nil)

(defvar *current-rmail-buffer* nil "Used only by C-M-Y and Q")

(defunmetax rmail ()
  (let ((point (point (pathname "sys$login:mail.mai"))))
    (let ((buffer (edit-cursor-buffer point)))
      (setq *current-rmail-buffer* buffer)
      (when (eq (bp-line point) (buffer-content buffer))
        (send point :to-beginning-of-next-line))
      (narrow-bounds-to-page point)
      (setf (buffer-access buffer) buffer-access-buffer-read-only)
      (with-many-key-bindings-named (*rmail-saved-bindings* rmail-bindings)
         (recursive-editor "Rmail" :point point))
      (widen-bounds-of-buffer buffer))))

(editor-bind-key '(#\control-x #\R) rmail)

(defun rmail-next-page ()
  (widen-bounds-of-buffer *editor-buffer*)
  (unwind-protect (progn (next-page)
                         (send *editor-cursor* :to-beginning-of-next-line))
                  (narrow-bounds-to-page *editor-cursor*))
  nil)

(defun rmail-previous-page ()
  (widen-bounds-of-buffer *editor-buffer*)
  (unwind-protect (progn (previous-page)
                         (send *editor-cursor* :backward-to-char #\page)
                         (send *editor-cursor* :to-beginning-of-next-line))
                  (narrow-bounds-to-page *editor-cursor*))
  nil)

(defun rmail-send-mail ()
  (with-many-key-bindings
   (cons (list #\Control-Meta-Y 'insert-current-message)
         (cdr *rmail-saved-bindings*))
   (send-mail))
  nil)

(defun rmail-continue-mail ()
  (with-many-key-bindings
   (cons (list #\Control-Meta-Y 'insert-current-message)
         (cdr *rmail-saved-bindings*))
   (send-mail T))
  nil)


(defun insert-current-message ()
  (insert-buffer *current-rmail-buffer*)
  (exchange-point-and-mark)
  (unless (not (=& *argument* 1))
    (let ((*argument* 3))
      (indent-rigidly)))
  nil)

(defun rmail-reply ()
  (ed-warning "Not complete yet")
  (rmail-send-mail)
  nil)

(defun exit-rmail ()
  (when (buffer-modified? *editor-buffer*)
    (rmail-save-file))
  (%kill-buffer-primitive *current-rmail-buffer*)
  (exit-editing-level)
  nil)

(defun rmail-delete-page ()
  (widen-bounds-of-buffer *editor-buffer*)
  (unwind-protect
   (let ((bp (copy-bp *editor-cursor*)))
     (setf (buffer-access *editor-buffer*) buffer-access-any)
     (send bp :forward-to-char #\page)
     (send *editor-cursor* :backward-to-char #\page)
     (do-not-append-next-kill)
     (kill-between-marks bp *editor-cursor*)
     (delete-between-marks bp *editor-cursor*)
     (when (char= #\page (send *editor-cursor* :peek-char-backward))
       (send *editor-cursor* :delete-characters -1)))
   (setf (buffer-access *editor-buffer*)
         buffer-access-buffer-read-only)
   (send *editor-cursor* :backward-to-char #\page)
   (send *editor-cursor* :to-beginning-of-next-line)
   (narrow-bounds-to-page *editor-cursor*))
  nil)

(defun rmail-undelete ()
  (widen-bounds-of-buffer *editor-buffer*)
  (setf (buffer-access *editor-buffer*) buffer-access-any)
  (unwind-protect
   (progn (send *editor-cursor* :forward-to-char #\page)
          (unless (buffer-begin? *editor-cursor*)
            (send *editor-cursor* :insert-char #\page))
          (un-kill))
   (setf (buffer-access *editor-buffer*) buffer-access-buffer-read-only)
   (next-page)
   (send *editor-cursor* :to-beginning-of-next-line)
   (narrow-bounds-to-page *editor-cursor*)
   (rmail-previous-page))
  nil)

(defun rmail-save-file ()
  (widen-bounds-of-buffer  *current-rmail-buffer*)
  (save-file)
  (narrow-bounds-to-page)
  nil)

(rmail-bind #\space next-screen "Show rest of current letter")

(rmail-bind #\C rmail-continue-mail "Continue or re-edit previous mail")

(rmail-bind #\D rmail-delete-page "Delete current letter")

(rmail-bind #\M rmail-send-mail "Send mail to someone")

(rmail-bind #\N rmail-next-page "Move to next letter")

(rmail-bind #\Q exit-rmail "Exit rmail")

(rmail-bind #\P rmail-previous-page "Move to previous letter")

(rmail-bind #\R rmail-reply "Reply to sender of current message")

(rmail-bind #\S rmail-save-file "Save current RMAIL file")

(rmail-bind #\U rmail-undelete "Undelete last deleted message")

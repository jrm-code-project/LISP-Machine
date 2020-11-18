; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*- 1983

;Copyright (c) June 1983 by Christopher Eliot, Leigh Klotz
; and Massachusetts Institute of Technology.

;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

(defvar *fill-column* 72.)              ;Should be a buffer local

(defun fill-index-if-applicable (bp)
  (let ((line (line-chars (bp-line bp)))
        (max-index (bp-position bp))
        (column 0))
    (dotimes (index max-index)
      (cond ((> column *fill-column*)
             (return (1-& index)))
            ((= column *fill-column*)
             (return index))
            (:else
              (setq column (+& column (graphic-char-length (char line index)
                                                           column))))))))

(defun com-auto-fill-space (&aux fill-index (fill-failed? nil))
  (when (and *auto-fill-mode*
             (setq fill-index (fill-index-if-applicable *editor-cursor*)))
    (preserving-point
      (send *editor-cursor* :set-pos (1+& fill-index))
      (loop for char = (send *editor-cursor* :get-char-backward)
         do (cond ((send *editor-cursor* :beginning-of-line?)
                   (setq fill-failed? t)
                   (return))
                  ((horizontal-white-space? char)
                   (delete-horizontal-space)
                   (crlf)
                   (return))))))
  (self-insert)
  (when fill-failed?
    (crlf)))

;Just a flag.  Don't touch it.  Should be a local.
(defvar *auto-fill-mode* nil)

;If there's an explicit positive argument, turn auto fill on.
;If auto fill mode is on, or there is a negative argument, turn it off.
;Otherwise, toggle it.

(bindmetax auto-fill-mode auto-fill-mode)

(defun auto-fill-mode (&aux (flag *auto-fill-mode*))
  (cond ((and (argument?) (+p *argument*))
         (setq flag t)
         (editor-bind-key #\space com-auto-fill-space))
        ((or flag (-p *argument*))
         (setq flag nil)
         (editor-bind-key #\space self-insert))
        (t (setq flag t)
           (editor-bind-key #\space com-auto-fill-space)))
  (bind-in-buffer *editor-buffer* '*auto-fill-mode* flag)
  (bind-in-buffer *editor-buffer*
                  '*minor-modes*
                  (if (not (null flag))
                      (cons 'fill (remove 'fill *minor-modes*))
                      (remove 'fill *minor-modes*)))
  nil)

;Use LIST instead of constant list to get around compiler bug on Vulcan.
(editor-bind-key (list #\control-x #\F) set-fill-column)

(defun set-fill-column ()
  (let ((proposed-fill-column (if (argument?)
                                  *argument*
                                (edit-cursor-position-really
                                  *editor-cursor*))))
    (if (<& proposed-fill-column 8)
        (ed-warn "~d is not a reasonable fill column, not changed."
                 proposed-fill-column))
    (setq *fill-column* proposed-fill-column)
    (with-notify-line-remaining
      (format terminal-io "Fill column now ~D." *fill-column*)))
  nil)

;With a positive argument, set fill column to the arg and call
;AUTO-FILL-MODE.  Otherwise just call AUTO-FILL-MODE with the argument.

(editor-bind-key (list #\control-x #\control-a) com-toggle-auto-fill-mode)

(defun com-toggle-auto-fill-mode ()
  (cond ((not (argument?))
         (auto-fill-mode))
        ((-p *argument*)
         (auto-fill-mode))
        (t (set-fill-column)
           (let ((argument-supplied? t))
             (auto-fill-mode))))
  nil)

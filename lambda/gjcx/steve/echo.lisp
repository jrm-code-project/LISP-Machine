; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;

(defun meditate (time difference)
 (do () ((or (type-ahead-p)
             (>$ (-$ (elapsed-time) time) difference)))))

;
;Version of self-insert which will evaluate forms.
;

(defvar *lisp-listener*)
(defvar *overwrite-mode*)

(defvar *display-matching-paren*)
(defvar minimum-display-time)

(defun bp-on-screen (bp)
 (eq (edit-cursor-home-line *editor-cursor*)
     (order-lines (edit-cursor-home-line *editor-cursor*) (bp-line bp))))

(defun self-insert (&optional (char *editor-current-key*) (number *argument*))
  (setq char (coerse-to-string-charp char))
  (dotimes (i number)
    (cond ((or (null *overwrite-mode*)
               (send *editor-cursor* :end-of-line?))
           (send (bp-line *editor-cursor*)
                   :insert-char (bp-position *editor-cursor*) char))
          (t (send (bp-line *editor-cursor*)
                   :replace-char (bp-position *editor-cursor*) char)))
    (when (of-syntax char paren-close)
      (with-bp (bp *editor-cursor*)
         (send bp :advance-pos -1)
         (let ((slashed? (bp-char-slashified? bp))
               (opener (get-paren-match (send bp :get-char-forward))))
           (unless slashed?
             (send bp :backward-over-sexp)
             (unless (or (null opener)
                         (char= (send bp :get-char) opener))
               (send terminal-io :beep))
             (cond ((and *lisp-listener* (send bp :beginning-of-line?))
                    (make-screen-image)
                    (evaluate-sexp-into-buffer bp))
                   ((or (plusp *display-matching-paren*)
                        (and (minusp *display-matching-paren*)
                             (bp-on-screen bp)))
                    (echo-bp bp (abs *display-matching-paren*)))))))))
  nil)

;;; Flushed standout bogosity

(defun echo-bp (bp duration &aux (time (elapsed-time)))
  (let ((mark (send *editor-cursor* :get-mark)))
    (unwind-protect
     (progn (send *editor-cursor* :move (bp-line bp) (bp-position bp))
            (make-screen-image *editor-cursor*)
            (unless (type-ahead-p)
              (send (edit-cursor-window *editor-cursor*) :make-cursor-visible)
;             (set-string-graphic-rendition :standout t *editor-cursor* 1)
              (meditate time (max (plus (difference (elapsed-time) time)
                                        minimum-display-time)
                                  duration))
;             (set-string-graphic-rendition :standout nil *editor-cursor* 1)
              ))
     (send *editor-cursor* :goto-mark mark))))

(defun evaluate-sexp-into-buffer (bp)
  (let ((stream (make-buffer-stream bp))
        (value))
    (let ((standard-output stream)
          (trace-output stream)
          (error-output stream))
      (send terminal-io :send-if-handles :set-device-mode :passall nil)
      (bp-forward-over-sexp bp)
      (let* ((form (read stream))
             (values (multiple-value-list (eval form))))
        (dolist (value values)
          (print value stream))
        (terpri stream)
        (setq *** **
              ** *
              * form)
        (setq +++ ++
              ++ +
              + (car values)))
      (send *editor-cursor* :move (bp-line stream) (bp-position stream))
      (send terminal-io :send-if-handles :set-device-mode :passall t))))

;(defmacro echoin (count pred)
; (let ((var1 (gensym)))
; `(loop repeat ,count
;       for ,var1 = (read-char&save terminal-io)
;       while (cond ((funcall ,pred ,var1))
;                   (t (unread-char&save terminal-io)
;                      nil))
;       do (send terminal-io :write-char ,var1)
;       collect ,var1)))

;This is almost ready to interface with VMS.
;The problem left is to eliminate the use of READ-CHAR&SAVE.
;That is used so that keyboard macros work and for the 60char buffer.
(defmacro echoin (count bit-vector)
  (let ((var1 (gensym)))
    `(loop repeat ,count
           for ,var1 = (read-char&save terminal-io)
           while (cond ((bit1p ,bit-vector (char-int ,var1)))
                       (t (unread-char&save terminal-io)
                          nil))
           do (send terminal-io :write-char ,var1)
           collect ,var1)))


(defmacro echoin-primitive (count bit-vector)
  (let ((var1 (gensym)))
    `(loop repeat ,count
           for ,var1 = (send terminal-io :read-char)
           while (cond ((bit1p ,bit-vector (char-int ,var1)))
                       (t (send terminal-io :unread-char ,var1)
                          nil))
           do (send terminal-io :write-char ,var1)
           collect ,var1)))

(defmacro editor-echoin (count bit-vector)
  (let ((var (gensym)))
    `(if (not (null executing-keyboard-macro))
         nil
         (let ((,var (echoin-primitive ,count ,bit-vector)))
           (string-read&save ,var)
           ,var))))

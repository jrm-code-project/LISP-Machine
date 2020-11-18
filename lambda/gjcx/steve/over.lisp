; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Overwrite streams. Ugh.
;

(defflavor overwrite-stream () (output-stream)
  )

(defvar *overwrite-line*)
(defvar overwrite-stream
        (make-instance 'overwrite-stream))

(defun more-processor (&optional (string "*more*"))
  (with-more-line
   (oustr string terminal-io)
   (case (peek-char&save)
     (#\space (read-char&save) t)
     ((#\rubout #\bell) (read-char&save) nil)
     (#\? (read-char&save)
          (with-notify-line
           (format terminal-io "Space to continue, Rubout to abort, else exit")
           (more-processor string)))
     (t nil))))

(defun more-aborter (&optional (string "*more*"))
  (with-more-line
   (oustr string terminal-io)
   (case (peek-char&save)
     (#\space (read-char&save) t)
     ((#\rubout #\bell) (read-char&save) (ed-warn "Aborted"))
     (#\? (read-char&save)
          (with-notify-line
           (format terminal-io "Space to continue, Rubout to abort, else exit")
           (more-aborter string)))
     (t (if (string-search "ore" string)
            (ed-warn "Aborted")
          t)))))

(defmethod (overwrite-stream :terpri) ()
  (setq *overwrite-line* (1+& *overwrite-line*))
  (cond ((>& *overwrite-line* *last-overwrite-line*)
         (more-aborter "*more*")
         (cursorpos 0 0 self)
         (send terminal-io :clear-eol))
        (t (cursorpos *overwrite-line* 0 terminal-io)
           (send terminal-io :clear-eol)
           (setq creamed-tty-lines-to (max& creamed-tty-lines-to
                                            (1+& *overwrite-line*))))))

(defmethod (overwrite-stream :write-char) (char)
  (when (>=& (send terminal-io :charpos) (1-& *tty-width*))
    (send terminal-io :raw-oustr "!" 0 1)
    (send self :terpri))
  (setq creamed-tty-lines-to (1+& (send terminal-io :linenum)))
  (cond ((graphic-charp char) (send terminal-io :write-char char))
        (t (case char
             (#\return (send self :terpri))
             (#\bell (send terminal-io :beep))
             (t (oustr (format nil "~s" char) self))))))

(defmethod (overwrite-stream :set-cursorpos) (y x)
  (when (null y) (setq y (send terminal-io :linenum)))
  (setq creamed-tty-lines-to (max& creamed-tty-lines-to (1+& y)))
  (setq *overwrite-line* y)
  (send terminal-io :set-cursorpos y x))

(defmethod (overwrite-stream :read-cursorpos) ()
  (send terminal-io :read-cursorpos))

(defmethod (overwrite-stream :clear-eol) ()
  (send terminal-io :clear-eol))

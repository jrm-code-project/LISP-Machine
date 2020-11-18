; -*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;

(defflavor editor-rubout-handling-stream
  (buffer istream ostream chars rereading? pos)
  (input-stream)
  :initable-instance-variables)

(defmethod (editor-rubout-handling-stream :re-init) ()
  (unless (null buffer)
    (setq chars (reverse buffer))
    (setq rereading? t)))

;;With an argument this should behave just as if that char was typed.
(defmethod (editor-rubout-handling-stream :read-char)
  (&optional (char nil char?))
  (when (null char?)
    (cond ((null rereading?)
           (cursorpos (car pos) (cdr pos) ostream)
           (setq char (send istream :read-char)))
          (t (setq char (car (pop chars))))))
  (cond ((char= char #\rubout)
         (send self :rubout)
         (send self :re-init)
         (throw 'rubout-tag nil))
        ((char= char #\^u)
         (loop while (not (null buffer))
               do (send self :rubout))
         (send self :re-init)
         (throw 'rubout-tag nil))
        ((char= char #\^w)
         (loop with flag = nil
               while (not (null buffer))
               while (if (alphanumericp (caar buffer))
                         (setq flag t)
                         (null flag))
               do (send self :rubout))
         (send self :re-init)
         (throw 'rubout-tag nil))
        ((char= char #\bell)
         (ed-warn "Aborted")))
  (unless rereading?
    (push (list char (cursorpos istream)) buffer)
    (send ostream :write-char char)
    (setq pos (cursorpos istream)))
  (when (and rereading? (null chars))
    (setq rereading? nil))
  char)

(defmethod (editor-rubout-handling-stream :unread-char) (char)
  (setq rereading? t)
  (push char chars)
  char)

(defmethod (editor-rubout-handling-stream :peek-char) ()
  (send self :unread-char (send self :read-char)))

(defmethod (editor-rubout-handling-stream :rubout) ()
  (unless rereading?
    (when (null buffer) (throw 'rubout-tag t))
    (setq pos (cadr (pop buffer)))
    (cursorpos (car pos) (cdr pos) ostream)
    (send ostream :clear-eol)
    t))

(defmethod (editor-rubout-handling-stream :buffer-size) ()
  (length buffer))

(defmethod (editor-rubout-handling-stream :beep) ()
  (send ostream :send-if-handles :beep))

(defmethod (editor-rubout-handling-stream :echo-false-char) (char)
  (send ostream :write-char char))

;

;Init with :buffer nil :stream STREAM :index 0 :size 0
(defflavor editor-completion-stream (buffer stream index size) (input-stream)
  :initable-instance-variables)

(defmethod (editor-completion-stream :re-init) ()
  (send stream :re-init)
  (setq buffer nil
        size 0
        index 0))

(defmethod (editor-completion-stream :read-char) ()
  (when (>=& index size)
    (push (send stream :read-char) buffer)
    (setq size (1+& size)))
  (setq index (1+& index))
  (nth (-& size index) buffer))

(defmethod (editor-completion-stream :unread-char) (char)
  (setq index (1-& index))
  char)

(defmethod (editor-completion-stream :peek-char) ()
  (send self :unread-char (send self :read-char)))

(defmethod (editor-completion-stream :buffer-size) ()
  (send stream :buffer-size))

(defmethod (editor-completion-stream :beep) ()
  (send stream :send-if-handles :beep))

(defmethod (editor-completion-stream :rubout) ()
  (send stream :rubout)
  (pop buffer))

(defmethod (editor-completion-stream :echo-false-char) (char)
  (send stream :send-if-handles :echo-false-char char))

(defmethod (editor-completion-stream :completion-string)
  (string replacing &aux (length (string-length string)))
  (when (>=& index size)
    (loop repeat replacing
          do (send stream :rubout)
          do (pop buffer))
    (setq replacing 0)
    (loop for i from 0 below length
          do (send stream :read-char (char string i))))
  (loop with tail = (nthcdr (+& (-& size index) replacing) buffer)
        with head = (loop for i downfrom size above index
                          for x in buffer
                          with value = nil
                          do (setq value (cons x value))        ;Reverse order.
                          finally (return value))
        for i from 0 below length
        do (push (char string i) tail)
        finally (loop for x in head
                      do (push x tail))
        finally (setq buffer tail))
  (setq size (-& (+& size length) replacing)
        index (-& (+& index length) replacing)))

(defun prescan (function &optional (eof nil eof?))
  (with-no-passall
  (let ((stream
         (make-instance 'editor-completion-stream
                        :buffer nil
                        :stream
                        (make-instance 'editor-rubout-handling-stream
                                       :buffer nil
                                       :istream terminal-io
                                       :ostream terminal-io
                                       :rereading? nil
                                       :chars nil
                                       :pos (cursorpos terminal-io))
                        :index 0
                        :size 0)))
    (loop for frob = (catch 'rubout-tag
                       (catch 'completion-reread
                         (list (funcall function stream))))
          if (consp frob) return (car frob)
          do (cond ((null frob) (send stream :re-init))
                   ((0p (or (send stream :send-if-handles :buffer-size)
                            1))
                    (cond ((null eof?) (send stream :send-if-handles :beep))
                          (t (return eof)))))))))

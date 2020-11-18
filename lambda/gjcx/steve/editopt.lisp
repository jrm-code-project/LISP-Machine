; -*- mode:lisp; package:steve; readtable:cl; -*-
;Tuesday the nineteenth of July, 1983; 10:50:09 pm

;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;M-X edit-options.

;(defunmetax list-options ()
;  (overwrite-start)
;  (loop for (var documentation) in *editor-variables*
;       do (format overwrite-stream "~a ~s ~a~%~a~%~%"
;                  var (symeval var) (if (variable-redefined var)
;                                        "(Redefined)" "")
;                  documentation))
;  (more-aborter "*done*"))

(defun setup-edit-options-buffer (buffer)
  (loop with bp = (make-bp buffer (buffer-content buffer) 0)
        with stream = (make-buffer-stream bp)
        for (var documentation . args) in *editor-variables*
        do (format stream "~s ~s ~%" var (symeval var))
        do (apply #'format stream documentation (mapcar #'eval args))
        do (terpri stream)
        do (terpri stream)))


;Thursday the twenty-first of July, 1983; 5:39:13 pm

(bindmetax edit-options edit-options)

(defun edit-options ()
  (let ((buffer)
        (context-buffer *editor-buffer*))
    (unwind-protect
     (progn (setq buffer (buffer "edit options buffer"))
            (setup-edit-options-buffer buffer)
            (unless (recursive-editor "Edit Options" :buffer buffer)
              (ed-warn "Aborted"))
            (read-edit-options-buffer buffer context-buffer))
    (setf (buffer-modified? buffer) nil)
    (kill-one-buffer buffer))))

(defun read-edit-options-buffer (buffer context-buffer)
  (let ((line (buffer-content buffer)))
    (loop do (loop while (and line (empty-line? line))
                   do (setq line (line-next line)))
          while line
          do (with-input-from-string (foo (line-chars line)
                                          :start 0
                                          :end (line-char-count line))
               (let ((variable (read foo))
                     (value (read foo)))
                 (bind-in-buffer context-buffer variable value)))
          do (loop while (and line (not (empty-line? line)))
                   do (setq line (line-next line))))))

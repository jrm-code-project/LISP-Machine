; -*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

(defvar recursive-edit nil)

;;Value will be NIL if aborted otherwise not.
(defun recursive-edit (description &optional (vars nil) &restl values)
  (let ((recursive-edit description)
        (*last-buffer-selected* *editor-buffer*)
        (old-cursor *editor-cursor*)
        (new-cursor (create-edit-cursor *editor-buffer*
                                        (bp-line *editor-cursor*)
                                        (bp-position *editor-cursor*)
                                        nil)))
    (setf (edit-cursor-home-line new-cursor)
          (edit-cursor-home-line *editor-cursor*))
    (setf (edit-cursor-home-pos new-cursor)
          (edit-cursor-home-pos *editor-cursor*))
    (select-point-in-current-window new-cursor)
    (unwind-protect (progv vars values
                           (catch 'exit-editor
                             (loop do (process-in-saved-buffer-environment
                                       (buffer-environment *editor-buffer*)
                                       #'editor-top-level (make-vector 0)))))
                    (select-point-in-current-window old-cursor))))

(defun recursive-editor (description &key
                                     (point *editor-cursor* point?)
                                     (buffer *editor-buffer* buffer?)
                                     (bindings nil)
                                     (mode nil))
  (when (and point? (not buffer?))
    (setq buffer (edit-cursor-buffer point)))
  (let ((recursive-edit description)
        (*last-buffer-selected* *editor-buffer*)
        (old-cursor *editor-cursor*)
        (new-buffer buffer)
        (new-cursor)
        (vars (mapcar #'car bindings))
        (values (mapcar #'cadr bindings)))
    (when (not (null mode))
      (major-mode mode buffer))
    (setq new-cursor (create-edit-cursor new-buffer
                                         (if point?
                                             (bp-line point)
                                             (buffer-content new-buffer))
                                         (if point?
                                             (bp-position point)
                                             0)))
    (when point?
      (setf (edit-cursor-home-line new-cursor)
            (edit-cursor-home-line point))
      (setf (edit-cursor-home-pos new-cursor)
            (edit-cursor-home-pos point)))
    (select-point-in-current-window new-cursor)
    (unwind-protect (progv vars values
                           (catch 'exit-editor
                             (loop do (process-in-saved-buffer-environment
                                       (buffer-environment new-buffer)
                                       #'editor-top-level (make-vector 0)))))
                    (select-point-in-current-window old-cursor))))

(editor-bind-key #\control-] abort-recursive-edit)

(defun abort-recursive-edit ()
  (throw 'exit-editor nil))

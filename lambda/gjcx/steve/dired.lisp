; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-
;
;Sunday the twenty-fourth of July, 1983; 12:00:15 am
;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;DIRED.

(defvar dired-alist nil)

(defvar dired-delete-list nil)

(defvar dired-previous-bindings)

(defparameter dired-bindings nil)

;(editor-bind-key '(#\control-x #\D) dired)

(define-binding '(#\control-x #\D) dired 'DIRED ("Fundamental")
                "Directory editing subsystem")

(defun dired (&optional (path nil)
               &aux (file (buffer-file-name *editor-buffer*)))
  (cond ((null path)
           (cond ((and (argument?) (=& *argument* 1))
                    (setq path (send (send file :new-type :wild)
                                     :new-version :wild)))
                 ((and (argument?) (=& *argument* 4))
                    (setq path (pathname (mx-prompter #'read-file-name
                                                      "Dired: "))))
                 (t (setq path (send (send (send file :new-name :wild)
                                           :new-type :wild)
                                     :new-version :wild)))))
        (t (setq path (pathname path))))
  (unless (send path :device)
    (setq path (send path :new-device (send file :device))))
  (unless (send path :directory)
    (setq path (send path :new-directory (send file :directory))))
  (unless (send path :name) (setq path (send path :new-name :wild)))
  (unless (send path :type) (setq path (send path :new-type :wild)))
  (unless (send path :version) (setq path (send path :new-version :wild)))
  (let ((buffer) (point))
    (when (not (null dired-alist))
      (ed-lose "Dired does not work recursively"))
    (unwind-protect
     (progn (setq buffer (buffer "Dired buffer" :create t))
            (setq point (point buffer))
            (setup-dired-buffer buffer path)
            (with-many-key-bindings-named
              (dired-previous-bindings dired-bindings)
              (loop do (send point :move (line-next (buffer-content buffer)) 0)
                    do (unless (recursive-editor "Dired" :point point)
                         (ed-warn "Aborted Dired"))
                    until (read-dired-buffer buffer))))
     (%kill-buffer-primitive buffer)
     (setq dired-alist nil))))

(defun setup-dired-buffer (buffer path)
  (declare (special bp))
  (let ((bp (make-bp buffer (buffer-content buffer) 0))
        (month-names (time:mode-language-fetch
                       :short time:*month-strings* "month name")))
    (declare (special bp month-names))
    (send bp :insert-string "DIRED ")
    (send bp :insert-string (or (send (send path :host) :name-as-file-computer)
                                ""))
    (send bp :insert-string "::")
    (send bp :insert-string (or (send path :device) ""))
    (send bp :insert-string ":")
    (send bp :insert-string (or (send path :directory-namestring) ""))
    (send bp :insert-char #\return)
    (mapdirectory
     #'(lambda (dirinfo)
         (push (list (bp-line bp) (car dirinfo)) dired-alist)
         (send bp :insert-string
               (directory-display-line-string dirinfo month-names))
         (send bp :insert-char #\return))
     path '(:creation-date :author :protection :blocks))
    (setf (buffer-access buffer) buffer-access-buffer-read-only)))


(defmacro dired-line-as-pathname (line)
  `(cadr (assoc ,line dired-alist)))

(defun read-dired-buffer (buffer)
  (setq dired-delete-list nil)
  (loop for line first (line-next (buffer-content buffer))
        then (line-next line)
        while line
        do (mung-one-dired-line line))
  (if (null dired-delete-list)
      (progn (ed-warning "No Files Deleted") t)
      (dired-delete-files-carefully dired-delete-list)))

(defun dired-delete-files-carefully (files-list)
  (overwrite-start)
  (oustr "Deleting these files:" overwrite-stream)
  (loop for path in files-list
        for i upfrom 0
        if (0p (\\& i 4)) do (terpri overwrite-stream)
        do (format overwrite-stream "~a ~16a "
                   (if (0p (dired-count-later-versions path))
                       #\> #\space)
                   (send path :file-namestring)))
  (terpri overwrite-stream)
  (oustr "OK? " overwrite-stream)
  (cond ((dired-yes-or-no-p)
         (loop with flag = nil
               for file in files-list
               for message = (delete-file file nil)
               unless (null message)
               do (progn (setq flag t)
                         (format overwrite-stream
                                 "~&Error deleting file ~a~%~a"
                                 file message))
               finally (when flag
                         ;;synchronize.
                         (send terminal-io :peek-char)))
         t)
        (t nil)))

(defun dired-yes-or-no-p ()
  (clear-input terminal-io)
  (loop for compare in '(#\Y #\E #\S)
        for char = (read-char terminal-io)
        if (not (char-equal char compare))
        do (case char
             (#\bell (ed-warn "Aborted"))
             (t (return nil)))
        do (write-char compare terminal-io)
        finally (return t)))

(defun mung-one-dired-line (line)
  (unless (empty-line? line)
    (case (char (line-chars line) 0)
      (#\space nil)
      (#\D (push (dired-line-as-pathname line) dired-delete-list))
      (t (ed-warning "Bad Dired Line ~s" (line-chars line))))))

;
;Dired mapping functions which select some set of lines to work on.
;

(defun dired-map-over-lines (function)
  ;; Map over lines as specified by the normal handling of arguments.
  (let ((access (buffer-access *editor-buffer*)))
    (setf (buffer-access *editor-buffer*) buffer-access-any)
    (cond ((-p *argument*)
           (not-first-line)
           (loop for i from 1 to (-& *argument*)
                 do (send *editor-cursor* :to-end-of-previous-line)
                 do (send *editor-cursor* :set-pos 0)
                 while (not (first-line?))
                 if (not (empty-line? (bp-line *editor-cursor*)))
                 do (funcall function (bp-line *editor-cursor*))))
          (t (loop for i from 1 to *argument*
                   if (not (empty-line? (bp-line *editor-cursor*)))
                   do (funcall function (bp-line *editor-cursor*))
                   while (not (last-line?))
                   do (send *editor-cursor* :to-beginning-of-next-line))))
    (setf (buffer-access *editor-buffer*) access)
    (send *editor-cursor* :set-pos 0))
  nil)

(defun dired-map-by-file-name (function)
  (let ((pathname (dired-line-as-pathname (bp-line *editor-cursor*))))
    (when (null pathname) (ed-lose "Current line not a Dired filename"))
    (let ((file-name (send pathname :name))
          (access (buffer-access *editor-buffer*)))
      (setf (buffer-access *editor-buffer*) buffer-access-any)
      (loop for line first (bp-line *editor-cursor*)
            then (line-previous line)
            while (and line
                       (line-previous line) ;First line is a header.
                       (not (empty-line? line))
                       (string-equal (send (dired-line-as-pathname line) :name)
                                     file-name))
            do (funcall function line))
      (loop for line first (line-next (bp-line *editor-cursor*))
            then (line-next line)
            while (and line
                       (not (empty-line? line))
                       (string-equal (send (dired-line-as-pathname line) :name)
                                     file-name))
            do (funcall function line))
      (setf (buffer-access *editor-buffer*) access)))
  (send *editor-cursor* :set-pos 0)
  nil)

(defun dired-map-over-buffer (function)
  (let ((access (buffer-access *editor-buffer*)))
    (setf (buffer-access *editor-buffer*) buffer-access-any)
    (loop for line first (line-next (buffer-content *editor-buffer*))
          ;;Skip the header line.
          then (line-next line)
          while line
          if (not (empty-line? line))
          do (funcall function line))
    (send *editor-cursor* :set-pos 0)
    (setf (buffer-access *editor-buffer*) access))
  nil)

(defun dired-map-possibly-by-file-name (function)
  (if (or argument-supplied? (not (=& 1 *argument*)))
      (dired-map-over-buffer function)
      (dired-map-by-file-name function)))

;
;Now the special dired functions.
;

(defun dired-mark-for-deletion ()
  (dired-map-over-lines #'(lambda (line)
                            (send line :replace-char 0 #\D))))

(defun dired-space ()
  (dired-map-over-lines #'null))

(defun dired-delete ()
  (let ((*argument* (-& *argument*)))
    (dired-map-over-lines #'(lambda (line)
                              (send line :replace-char 0 #\space)))))

(defun dired-undo-mark-for-deletion ()
  (dired-map-over-lines #'(lambda (line)
                            (send line :replace-char 0 #\space))))

(defun dired-up-line ()
  (let ((*argument* (-& *argument*)))
    (dired-map-over-lines #'null)))

(defun dired-help ()
  (dired-map-possibly-by-file-name #'dired-auto-delete-line))

(defun dired-auto-delete-line (line)
  (let ((path nil))
    (when (and (not (empty-line? line))
               (setq path (dired-line-as-pathname line))
               (or (member (send path :type) *temp-file-fn2-list*)
                   (>=& (dired-count-later-versions path)
                        *file-versions-kept*)))
      (send line :replace-char 0 #\D))))

(defun dired-count-later-versions (pathname)
  (loop with name = (send pathname :name)
        with type = (send pathname :type)
        with version = (send pathname :version)
        for tail on dired-alist
        when (string-equal name (send (cadar tail) :name))
        return (loop for ((line path) . end) on tail
                     while (string-equal (send path :name) name)
                     counting (and (>& (send path :version) version)
                                   (string-equal (send path :type) type)))))

(defun dired-view-file ()
  (let ((file (dired-line-as-pathname (bp-line *editor-cursor*))))
    (when (null file)
      (ed-lose "Current line is not a filename"))
    (view-file file))
  nil)

(defun dired-recursive-edit ()
  (let ((file (dired-line-as-pathname (bp-line *editor-cursor*))))
    (when (null file)
      (ed-lose "Current line is not a filename"))
    (with-many-key-bindings dired-previous-bindings
      (recursive-editor "Edit" :buffer (buffer file :create t))))
  nil)

(bindmetax dired-command-help dired-command-help)

(defun dired-command-help ()
  (overwrite-start)
  (loop for (key binding doc-string . doc-args) in dired-bindings
        do (format overwrite-stream "~%~%Key ~s is bound to ~s~%"
                   key binding)
        do (apply #'format overwrite-stream doc-string doc-args))
  (more-aborter "*done*")
  nil)

;The actual bindings.

(defmacro dired-bind (key binding documentation &rest doc-args)
  `(progn (setq dired-bindings
                (nconc (delq (assoc ,key dired-bindings) dired-bindings)
                       (list (list ,key ',binding ,documentation ,@doc-args))))
          nil))

(dired-bind #\H dired-help
            "Automatically select files for deletion.
See variables: *file-versions-kept* (now ~a) and
*temp-file-fn2-list* (now ~a)" *file-versions-kept* *temp-file-fn2-list*)

(dired-bind #\? dired-command-help
            "Print documention on DIRED commands")

(dired-bind #\rubout dired-delete
            "Remove deletion markings from previous line(s)")

(dired-bind #\space dired-space
            "Move down one or more lines")

(dired-bind #\return dired-space
            "Move down one or more lines")

(dired-bind #\control-n dired-space
            "Move down one or more lines")

(dired-bind #\control-p dired-up-line
            "Move up one or more lines")

(dired-bind #\D dired-mark-for-deletion
            "Mark current file (or several) for deletion")

(dired-bind #\control-d dired-mark-for-deletion
            "Mark current file (or several) for deletion")

(dired-bind #\K dired-mark-for-deletion
            "Mark current file (or several) for deletion")

(dired-bind #\U dired-undo-mark-for-deletion
            "Remove deletion mark from current (or several) files")

(dired-bind #\V dired-view-file
            "View current file by screenfulls")

(dired-bind #\Control-R dired-recursive-edit
            "Recursively edit the current file")

(dired-bind #\E dired-recursive-edit
            "Recursively edit the current file")

(dired-bind #\Q exit-editing-level
            "Exit dired and delete marked files after verification
To verify type Y-E-S. Control-G at this point will abort Dired without
doing anything. Any other character will return to Dired.")

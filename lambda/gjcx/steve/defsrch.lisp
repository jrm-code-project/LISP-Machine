;-*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Defun-search and Buffer-search
;

(defvar *function-search*)
(defvar *last-defun-search-string* "")

(defun defun-search (buffer string
                            &key (first-line (buffer-content buffer))
                                 (delimited nil))
  (cond ((null delimited)
         (loop for line first first-line then (line-next line)
               while (not (null line))
               if (and (+p (line-char-count line))
                       (char= (char (line-chars line) 0) left-paren)
                       (string-search-for-substring (line-chars line) string))
               return line))
        (t (loop for line first first-line then (line-next line)
                 while (not (null line))
                 if (and (+p (line-char-count line))
                         (char= (char (line-chars line) 0) left-paren)
                         (delimited-substring-search (line-chars line) string))
                 return line))))

(defun host-specified (string-path)
 (let ((p1 (string-search-char #\: string-path)))
  (and (not (null p1))
       (not (null (string-search-char #\: string-path (1+& p1)))))))

(defun find-module-source-file (module)
  (let* ((vasl-file (si:module-loaded-file module))
         (path)
         (host))
    (setq vasl-file (car (errset (pathname vasl-file) nil)))
    (setq path (cond ((null vasl-file) nil)
                     ((probe-file vasl-file)
                      (setq path (si:vas-source-file vasl-file)))
                     (t (setq path (send (send vasl-file :new-type "lisp")
                                         :new-version nil)))))
    (when (and (stringp path) (host-specified path))
      ;; Si:Vas-Source-File may return a path with a HOST like HTJR.
      ;; If the host is not in the host tables then PATHNAME will
      ;; bomb. Since there is only one host, and we don't want
      ;; to force people to add all NIL-GROUP machines to their
      ;; host tables we just kludge the string to remove the host.

      ;; The next line was missing the zero.  That looks
      ;; like a random mistake someone made with the editor.
      ;; 4/17/84 --CRE
      (setq host (substring path 0 (string-search-char #\: path)))
      ;; strip off the host and only one colon (just to be safe)
      (setq path (substring path (1+& (string-search-char #\: path))))
      ;; if there is another colon, strip it off too
      (when (char= (char path 0) #\:)
        (setq path (substring path 1))))
    (setq path (car (errset (send (pathname path) :new-version nil) nil)))
     ;; Errset is used because some VASL files have bad filenames in them.
    (cond ((null path))
          ((probe-file path))
          ((probe-file (send path :new-device nil))
           (setq path (send path :new-device nil)))
          ((and (string-equal host "HTJR")      ; *** kludge! ***
                (probe-file (send path :new-device "nil$disk")))
           (setq path (send path :new-device "nil$disk"))))
    (and (probe-file path) path)))

;Search for a function. Load the source file if it can be found.
;Search all buffers for it if it is not defined.
;Values are the same as global-defun-search.
;What about packages?
(defun symbol-from-string (string)
  (with-input-from-string
    (foo (string-append (string-upcase string) " "))
    (read foo)))


(defun function-search (function)
  (when (not (symbolp function))
    (setq function (symbol-from-string function)))
  (let ((module)
        (path))
    (cond ((or (null function)
               (not (fboundp function))
               (null (setq module (whereis function)))
               (null (setq path (find-module-source-file module))))
           (global-defun-search (string function)
                                :delimited (and function (fboundp function))))
          (t (let ((line))
               (with-error-line
                (format terminal-io "Pathname: ~a"
                        (send path :string-for-editor))
                (when (null (point-selected path :create (probe-file path)))
                  (ed-warn "Cannot Locate Source File"))
                (setq line (defun-search *editor-buffer*
                                         (string function)
                                         :delimited t)))
               (values *editor-buffer* line))))))

;(defun find-file-from-pathname-soft (pathname)
;  (let ((point (point pathname :create nil)))
;    (cond ((not (null point))
;          (select-point-in-current-window point)
;          t)
;         ((null (probe-file pathname))
;          nil)
;         (t (setq point (make-point (send pathname :name) nil))
;            (setf (buffer-file-name (edit-cursor-buffer point)) pathname)
;            (select-point-in-current-window point)
;            (read-file-into-buffer pathname)
;            t))))
;
;(defun find-file-from-pathname-soft (pathname)
;  (point-selected pathname :create (probe-file pathname)))

(defun global-defun-search (string
                            &key (buffers *all-buffers*)
                            (first-buffer-first-line nil)
                            (delimited nil))
 (loop for buffer in buffers
       for search first (defun-search buffer string
                                      :first-line first-buffer-first-line
                                      :delimited delimited)
       then (defun-search buffer string :delimited delimited)
       if (not (null search))
       return (values buffer search)))

(defun find-defun (function-name &aux (mark (copy-bp *editor-cursor*)))
 (multiple-value-bind (buffer line) (global-defun-search function-name)
  (unless (or (null buffer) (null line))
    (edit-cursor-goto buffer line 0)
    (push-mark-no-copy mark)
    (reposition-window)
    t)))

(defun find-function (function-name &aux (mark (copy-bp *editor-cursor*)))
 (multiple-value-bind (buffer line) (function-search function-name)
  (unless (null buffer)
    (edit-cursor-goto buffer (or line (buffer-content buffer)) 0)
    (push-mark-no-copy mark)
    (cond ((null line)
           (ed-warn "The function seems to have been deleted"))
          (t (reposition-window)))
      t)))

(editor-bind-key #\meta-. defun-search-all-buffers)

(defun defun-search-all-buffers (&aux type)
  (setq type (if (argument?)
                 (not *function-search*)
                 *function-search*))
  (with-query-line
   (cond ((null type)
          (oustr "Global Defun Search: " terminal-io))
         (t (oustr "Find Function: " terminal-io)))
   (let ((string (si:ttyscanner #'readline-with-exit
                                terminal-io terminal-io () T 0))
         (*argument* 1)
         (argument-supplied? nil)) ;For set-file-read-only.
     (when (not (stringp string))
       (ed-warn "Aborted"))
     (setq *last-defun-search-string* string)
     (unless (if (not (null type))
                 (find-function string)
                 (find-defun string))
       (ed-warn "Not Found"))))
  nil)

(editor-bind-key #\control-meta-. defun-search-continue)

(defun defun-search-continue ()
 (let ((buffers (loop for (buffer . rest) on *all-buffers*
                      if (eq buffer *editor-buffer*)
                      return (cons buffer rest)))
       (buffer)
       (line))
  (multiple-value (buffer line)
    (global-defun-search
     *last-defun-search-string*
     :buffer buffers
     :first-buffer-first-line (line-next (bp-line *editor-cursor*))
     :delimited nil))
  (when (or (null buffer) (null line)) (ed-warn "Not Found"))
  (let ((mark (copy-bp *editor-cursor*)))
   (edit-cursor-goto buffer line 0)
   (push-mark-no-copy mark))
  (reposition-window))
 nil)

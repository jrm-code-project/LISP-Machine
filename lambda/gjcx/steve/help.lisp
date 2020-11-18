; -*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Functions for the editor help system.
;

(defvar *editor-command-help-file-pathname*
        (pathname "nil$disk:[nil.steve]comm.inf"))
(defvar *help-buffer* nil)

(defun with-help-file-loaded ()
 ;;Load the help file if not loaded already.
 (cond ((not (null *help-buffer*)))
       ((null (probe-file *editor-command-help-file-pathname*))
        (with-query-line
         (format terminal-io "Help File not In ~a. New pathname: "
                 (send *editor-command-help-file-pathname* :string-for-editor))
         (setq *editor-command-help-file-pathname*
               (pathname (si:ttyscanner #'readline-with-exit
                                        terminal-io terminal-io () () ()))))
        (with-help-file-loaded))
       ((setq *help-buffer*
              (buffer-load "Editor Help"
                           *editor-command-help-file-pathname*)))))

(defun buffer-load (name pathname)
 (let ((point (make-point name nil)))
  (setf (buffer-file-name (edit-cursor-buffer point)) pathname)
  (cond ((probe-file pathname)
         (read-file-into-buffer pathname (edit-cursor-buffer point))
         (edit-cursor-buffer point)))))

(defun buffer-block-search (buffer string found-function)
 (loop with line = (buffer-content buffer)
       for block = nil
       while (not (null line))
       for flag = nil
       do (loop until (null line)
                for chars = (line-chars line)
                do (setq line (line-next line))
                while (not (string-equal chars ""))
                do (push chars block)
                if (string-search-for-substring chars string)
                do (setq flag t))
       if (and flag (funcall found-function (nreverse block)))
       return t))

(defun buffer-block-header-search (buffer string found-function
                                          &aux (flag nil))
 (loop for block = nil
       for line first (buffer-content buffer) then (line-next line)
       while (not (null line))
       for chars = (line-chars line)
       while (null flag)
       if (string-search-for-substring chars string)
       do (loop initially (push chars block)
                do (setq line (line-next line))
                until (null line)
                do (setq chars (line-chars line))
                while (not (string-equal chars ""))
                do (push chars block)
                finally
                (setq flag
                      (funcall found-function (nreverse block))))
       else do (loop until (or (null (setq line (line-next line)))
                               (string-equal (line-chars line) "")))
       while (not (null line)))
   flag)

(defun editor-apropos (&optional (string (mx-prompter #'readline
                                                      "Apropos: ")))

  (overwrite-start)
  (with-no-passall
   (when (with-help-file-loaded)
     (with-notify-line (oustr "Searching..." terminal-io)
                       (buffer-block-search
                        *help-buffer* string
                        #'(lambda (block)
                            (loop for line in block
                                  do (overwrite-terpri)
                                  do (princ line terminal-io))
                            (overwrite-terpri)
                            (overwrite-terpri)
                            nil)))))
  (oustr "Done" terminal-io)
  (overwrite-done)
  (more-aborter "*done*"))

(bindmetax apropos editor-apropos)

(editor-bind-key #\meta-? describe-key)

(defun describe-key ()
  (with-query-line
   (oustr "Describe Key: " terminal-io)
   (let ((key) (command))
     (setq key (ed-read-char)
           command (or (lookup-key-in-table key *editor-bindings*)
                       (lookup-key-in-table (char-upcase key)
                                            *editor-bindings*)))
     (describe-key-command key command)
     (more-aborter "*done*")))
  nil)

(defun keyname-string (key)
  (if (consp key) (format nil "~:@C ~:@C" (car key) (cadr key))
      (format nil "~:@C" key)))

(defun describe-key-command (key command)
  (if (null command)
      (with-notify-line-remaining
        (format terminal-io "Key ~A is not defined" (keyname-string key)))
      (with-query-line-remaining
        (format terminal-io "Documentation for key ~A, ~s"
                (keyname-string key) command)))
  (cond ((null command))
        ((consp command)
         (unless (listen terminal-io) (describe-command (car command)))
         (multiple-value (key command) (read-any-key key))
         (describe-key-command key command))
        ((describe-command command))
        (t (with-notify-line-remaining
            (format terminal-io "No documentation for ~A, ~s"
                    (keyname-string key) command)))))

(defun editor-describe (&optional (string (mx-prompter #'readline
                                                       "Describe Command: ")))
  (with-no-passall
   (cond ((describe-command string)
          (more-aborter "*done*"))
         (t (with-notify-line-remaining
             (oustr "No description for " terminal-io)
             (princ string terminal-io))))))

(bindmetax describe editor-describe)

(defun describe-command (command)
  (cond ((null command) nil)
        ((characterp command)
         (describe-command
          (or (lookup-key-in-table command *editor-bindings*)
              (lookup-key-in-table (char-upcase command) *editor-bindings*))))
        ((consp command)
         (describe-command (car command)))
        ((or (stringp command)
             (symbolp command))
         (unless (fboundp (intern-soft command 'steve))
           (with-notify-line-remaining
            (format terminal-io "Command ~a is not Fbound" command)))
         (with-no-passall
          (when (with-help-file-loaded)
            (with-notify-line (oustr "Searching..." terminal-io)
                              (buffer-block-header-search
                                *help-buffer*
                                (string command)
                                #'(lambda (block)
                                    (overwrite-start)
                                    (loop for line in block
                                          do (overwrite-terpri)
                                          do (princ line terminal-io))
                                    (overwrite-terpri)
                                    (oustr "Done" terminal-io)
                                    (overwrite-done)
                                    t))))))
        (t nil)))

(bindmetax describe-char-syntax describe-char-syntax)

(defun describe-char-syntax (&optional (char (mx-prompter #'read "Char: ")))
  (let ((desc (syntax-description syntax-table (character char))))
    (with-notify-line-remaining
      (if (null desc)
          (format terminal-io "Char ~:@C has no specified syntax." char)
          (format terminal-io "Char ~:@C has syntax ~{~A~^, ~}" char desc)))))

(defun print-last-60chars ()
  (cursorpos 'c echo-area-window)
  (format echo-area-window "Last 60 characters typed:~%")
  (loop for i from 1 to 60
        for j first (\\& (+& 60char-index 59) 60) then (\\& (+& 59 j) 60)
        for char = (char 60char-buffer j)
        for can-char = (canonicalize-control-characters char)
        until (char= char #\null)
        do (format echo-area-window "~a " (case can-char
                                            (#\space "#\\SPACE")
                                            (#\tab "#\\TAB")
                                            (t can-char)))))

(defun editor-help ()
 (with-query-line
  (oustr "Help (type ? for options): " terminal-io)
  (let ((opt (read-char&save terminal-io)))
   (case opt
    (#\? (with-notify-line-remaining
          (oustr
           "? Apropos, Describe, Key(or Character), Last 60chars(or 6), Syntax"
           terminal-io))
         (editor-help))
    ((#\A #\a) (editor-apropos))
    ((#\D #\d) (editor-describe))
    ((#\C #\c #\K #\k) (describe-key))
    ((#\L #\l #\6) (print-last-60chars))
    ((#\S #\s) (describe-char-syntax)))))
 nil)

(editor-bind-key #\control-meta-?  editor-help)

;Counting and information commands.

;This returns the number of lines and buffer characters betwen two marks,
(defun count-between (bp-one bp2 &aux bp1)
  (multiple-value (bp1 bp2) (order-bps bp-one bp2))
  (loop for line first (bp-line bp1) then (line-next line)
        while (and (not (null line)) (neq line (bp-line bp2)))
        for count first (-& (line-char-count line) (bp-position bp1))
        then (+& count (line-char-count line) *line-separator-length*)
        counting line into lines
        finally (return (values (1+& lines)
                                (if (null count)
                                    (-& (bp-position bp2) (bp-position bp1))
                                    (+& count
                                        (bp-position bp2)
                                        *line-separator-length*))
                                (eq bp-one bp1)))))

(editor-bind-key #\meta-= count-lines-region)

(defun count-lines-region ()
  (with-notify-line-remaining
   (multiple-value-bind (lines chars) (count-between *editor-cursor*
                                                     (get-mark))
   (format terminal-io "Lines ~a; Chars ~a" lines chars)))
  nil)

;(defun bp-buffer-length (buffer)
; (loop for line first (buffer-content buffer) then (line-next line)
;       while (not (null line))
;       summing (line-char-count line)))

;These count the number of FILE characters. This means the line
;separator is counted as two characters (CR/LF).
(defun bp-buffer-position (bp &aux (last (bp-line bp)))
 (plus (loop for line first (buffer-content (bp-buffer bp))
             then (line-next line)
             while (and (not (eq line last)) (not (null line)))
             summing (+& (line-char-count line) *line-separator-length*))
       (bp-position bp)))

(defun bp-buffer-remaining (bp)
 (plus (-& (line-char-count (bp-line bp)) (bp-position bp))
       (loop for line first (line-next (bp-line bp)) then (line-next line)
             until (null line)
             summing (+& (line-char-count line) *line-separator-length*))))

(editor-bind-key '(#\control-x #\=) what-cursor-position)

(defun what-cursor-position ()
 (with-notify-line-remaining
  (let ((x) (y) (char (send *editor-cursor* :get-char)))
   (multiple-value (x y) (real-x-y-position *editor-cursor*))
   (format terminal-io "X=~a Y=~a Char=~s (ascii ~s)" x y char (char-int char))
   (let ((pos (bp-buffer-position *editor-cursor*))
         (left (bp-buffer-remaining *editor-cursor*)))
    (format terminal-io " .=~a(~a% of ~a)"
            pos
            (if (and (zerop pos) (zerop left))
                '**
                (fix (times 100 (quotient (float pos) (plus pos left)))))
            (plus pos left))))))

(editor-bind-key #\control-= what-cursor-position)

(defun get-buffer-major-mode (buffer)
  (or (let ((env (buffer-environment buffer)))
        (and (not (null env))
             (loop for name in (load-environment-binding-vars env)
                   for val in (load-environment-new-values env)
                   if (eq name '*major-mode*)
                   return val)))
      :fundamental))

(editor-bind-key '(#\control-x #\control-b) list-buffers)

(defun list-buffers ()
  (overwrite-start)
  (format terminal-io "  Buffer       Mode        Filename")
  (overwrite-terpri)
  (format terminal-io "  (* Indicates buffer needs saving)")
  (loop for buffer in *all-buffers*
        do (overwrite-terpri)
        do (format terminal-io "~a ~12a ~11a ~a "
                   (if (buffer-modified? buffer) "*" " ")
                   (buffer-name buffer)
                   (get-buffer-major-mode buffer)
                   (send (buffer-file-name buffer) :string-for-host))
        do (unless (null (buffer-truename buffer))
             (format terminal-io "(~a)"
                     (send (buffer-truename buffer) :version))))
  (overwrite-done)
  (more-aborter "*done*")
  nil)

(editor-bind-key '(#\control-x #\L) count-lines-page)

(defun count-lines-page ()
  (let ((mark (send *editor-cursor* :get-mark)) (before) (after) (line))
    (setq line (bp-line *editor-cursor*))
    (send *editor-cursor* :backward-to-char #\page)
    (setq before (1-& (count-lines (bp-line *editor-cursor*) line)))
    (send *editor-cursor* :forward-to-char #\page)
    (setq after (count-lines line (bp-line *editor-cursor*)))
    (send *editor-cursor* :goto-mark mark)
    (with-notify-line-remaining
     (format terminal-io "Page has ~D lines (~D+~D)"
             (+& before after) before after)))
  nil)

(bindmetax list-redefinitions list-redefinitions)

(defun list-redefinitions ()
  (overwrite-start)
  (loop for (var documentation . args) in *editor-variables*
        if (variable-redefined var)
        do (progn (format overwrite-stream "~a ~s~%" var (symeval var))
                  (apply #'format overwrite-stream documentation
                         (mapcar #'eval args))
                  (terpri overwrite-stream)
                  (terpri overwrite-stream)))
  (more-aborter "*done*"))

(defun variable-redefined (variable)
  (let ((env (buffer-environment *editor-buffer*)))
    (and (not (null env))
         (not (null (memq variable (load-environment-binding-vars env)))))))

(bindmetax list-options list-options)

(defun list-options ()
  (overwrite-start)
  (loop for (var documentation . args) in *editor-variables*
        do (format overwrite-stream "~a ~s ~a~%"
                   var (symeval var) (if (variable-redefined var)
                                         "(Redefined)" ""))
        do (apply #'format overwrite-stream documentation
                  (mapcar #'eval args))
        do (terpri overwrite-stream)
        do (terpri overwrite-stream))
  (more-aborter "*done*"))

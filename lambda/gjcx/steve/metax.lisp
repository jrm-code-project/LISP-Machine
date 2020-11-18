; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*- 1983

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Meta-x command cruft.
;Started 1/30/83 by CRE.
;

(defun mx-prompter (function format-string &REST args)
 (with-query-line
  (lexpr-funcall #'format terminal-io format-string args)
  (let ((value (prescan function args)))
   ;;Args is the EOF value because it is unique.
   (when (eq value args) (ed-warn "Aborted"))
   value)))


;
;Command tables.
;

(defstruct (command-table (:conc-name command-table-))
 names values sorted? alist)

(defvar meta-x-command-table
      (make-command-table :sorted? nil :alist (list "Meta-x Commands")))

(defun defcommand (command table value)
  (setf (command-table-sorted? table) nil)
  (loop for tail on (command-table-alist table)
        until (or (null (cdr tail)) (string-lessp command (caadr tail)))
        if (string-equal command (caadr tail))
        return (setf (cdr tail) (cons (list command value) (cddr tail)))
        finally (setf (cdr tail) (cons (list command value) (cdr tail))))
  command)

(defun cache-command-table-content (table)
  (setf (command-table-names table)        ;Initial value inconsequential
        (make-vector (length (cdr (command-table-alist table)))))
  (setf (command-table-values table)       ;Initial value inconsequential
        (make-vector (length (cdr (command-table-alist table)))))
  (loop with names = (command-table-names table)
        with values = (command-table-values table)
        for (tag value) in (cdr (command-table-alist table))
        for index from 0
        do (setf (svref names index) tag)
        do (setf (svref values index) value))
  (setf (command-table-sorted? table) t))

;Should this capitalize the command name?
(defmacro bindmetax (command function)
  `(defcommand (string ',command) meta-x-command-table ',function))

(defmacro defunmetax (command args &rest forms)
  (let ((defun-name (if (consp command) (cadr command) command))
        (mx-name (if (consp command) (car command) command)))
    `(progn 'compile
            (defun ,defun-name ,args ,@forms)
            (bindmetax ,mx-name ,defun-name))))

(defun lookup-command (command table)
  (or (command-table-sorted? table)
      (cache-command-table-content table))
  (loop with names = (command-table-names table)
        with bound1 = 0
        with bound2 = (1-& (vector-length names))
        for index = (/& (+& bound1 bound2) 2)
        do (cond ((string-lessp (svref names index) command)
                  (setq bound1 (if (=& bound1 index) (1+& index) index)))
                 (t (setq bound2 index)))
        if (string-equal (svref names bound1) command) return bound1
        if (<& (-& bound2 bound1) 1)
        return (if (and (string-lessp (svref names bound1) command)
                        (string-lessp command (svref names (1+& bound1))))
                   (1+& bound1)
                   bound1)))

(defun complete-command (command table)
 (or (command-table-sorted? table)
     (cache-command-table-content table))
 (loop with names = (command-table-names table)
       with values = (command-table-values table)
       with length = (string-length command)
       for index from (lookup-command command table)
       below (vector-length names)
       for name = (svref names index)
       while (string-equal command name
                           0 0 length (min& (string-length name) length))
       collect (list name (svref values index))))

(defun complete-command? (command table)
 (or (command-table-sorted? table)
     (cache-command-table-content table))
 (loop with names = (command-table-names table)
       with values = (command-table-values table)
       with length = (string-length command)
       for index from (lookup-command command table)
       below (vector-length names)
       for name = (svref names index)
       while (string-equal command name
                           0 0 length (min& (string-length name) length))
       thereis (string-equal command name)))

(defun max-prefix (string-list)
 (if (null string-list)
     ""
     (loop with first = (car string-list)
           with j = -1
           for i from 0 below (loop for string in string-list
                                    minimize (string-length string))
           while (loop with test-char = (char first i)
                       for string in (cdr string-list)
                       always (char-equal (char string i) test-char))
           do (setq j i)
           finally (return (substring first 0 (1+& j))))))

;
;Returns the first position where two strings differ, or nil.
;(defun string-compare (str1 str2)
;  (%string-maxprefix str1 str2 0 0
;                    (min& (string-length str1) (string-length str2))))
;

(defun command-completion (command table)
 ;;This works by finding the first and last entry which match
 ;;the command, and then returning the maximum prefix of these two entries.
 ;;There is a special case when no entry matches the command. If this happens
 ;;then the command is returned unchanged.

 ;;Another algorithm is possible, which might be better. Using LOOKUP-COMMAND
 ;;it is possible to eliminate the case where there is no matching entry.
 ;;Then, start with the first matching string and its LENGTH. Loop
 ;;through the entries, as we do here, and scan to LENGTH. Set
 ;;LENGTH to the first point where they differ. Finally return this prefix.
 (or (command-table-sorted? table)
     (cache-command-table-content table))
 (loop with names = (command-table-names table)
       with values = (command-table-values table)
       with length = (string-length command)
       with index-1 = (lookup-command command table)
       with index-2 = (1-& index-1)
       for index from index-1 below (vector-length names)
       for name = (svref names index)
       while (string-equal command name
                           0 0 length (min& (string-length name) length))
       do (setq index-2 index)
       counting T into possible
       finally (return
                (values (if (>=& index-2 index-1)
                            (max-prefix (list (svref names index-1)
                                              (svref names index-2)))
                            command)
                        possible))))

(defun word-completion (command table)
 (or (command-table-sorted? table) (cache-command-table-content table))
 (loop with names = (command-table-names table)
       with values = (command-table-values table)
       with length = (string-length command)
       with index-1 = (lookup-command command table)
       with index-2 = (1-& index-1)
       for index from index-1 below (vector-length names)
       for name = (svref names index)
       while (string-equal command name
                           0 0 length (min& (string-length name) length))
       do (setq index-2 index)
       counting t into possible
       finally
       ;;Values are: completions-string
       ;;            possible-completions
       ;;            end-of-command-flag
       (cond ((>=& index-2 index-1) ;Is completions possible?
              (let* ((name (svref names index-2))
                     (string1 (svref names index-1))
                     (pos (%string-maxprefix string1 name 0 0
                            (min& (string-length string1)
                                  (string-length name)))))
               (setq pos (1-& (if (null pos) (string-length name) pos)))
               ;;Find word boundary. POS = 1-& the maximum completion length.
               (loop for i from length to pos
                     if (memq (char name i) '(#\space #\-))
                     do (progn (setq pos i) (return nil)))
               (cond ((and (=& possible 1) (=& (1+& pos) (string-length name)))
                      (return (values (substring name 0 (1+& pos))
                                      possible
                                      t)))
                     (t (return (values (substring name 0 (1+& pos))
                                        possible
                                        nil))))))
             (t (return (values command 0 nil))))))

(defun read-meta-x-command (stream)
 (let ((command)
       (complete?))
  (multiple-value (command complete?) (meta-x-read stream))
  (setq command (car (complete-command command meta-x-command-table)))
  (loop with arg
        when (or (not (null complete?))
                 (char= (send stream :peek-char) #\return))
        return (cons command args)
        do (multiple-value (arg complete?) (read-meta-x-arg stream))
        collect arg into args)))

(defun meta-x-read (stream &aux completion choices done-flag)
 (loop for char = (send stream :read-char)
       if (char= char #\space)
       do (progn (send stream :completion-string
                       (multiple-value (completion choices done-flag)
                        (word-completion (coerce chrs 'string)
                                         meta-x-command-table))
                       (1+& (length chrs)))
                 (unless (null done-flag)
                  (send stream :completion-string "" 0)
                  (return (values completion nil)))
                 ;;Add a character to rubout.
                 ;(send stream :completion-string " " 0)
                 (throw 'rubout-tag nil))
       if (or (char= char #\altmode) (char= char #\return))
       do (progn
           (multiple-value (completion choices)
            (command-completion (coerce chrs 'string) meta-x-command-table))
           (send stream :completion-string completion (1+& (length chrs)))
           (cond ((complete-command? completion meta-x-command-table)
                  (cond ((char= char #\return) (return (values completion t)))
                        (t (send stream :completion-string "" 0)
                           (return (values completion
                                           nil)))))
                 (t (send terminal-io :beep)
                    ;;Add a character to rubout.
                    ;(send stream :completion-string " " 0)
                    (throw 'rubout-tag nil))))
       if (or (char= char #\?) (char= char #\page))
       do (if (char= char #\page)
              (progn (send stream :rubout)
                     (make-screen-image))
              (progn (send stream :rubout)
                     (print-possible-completions
                       (coerce chrs 'string) meta-x-command-table)))
       else
       collect char into chrs))

(defun print-possible-completions (command table)
  (catching-editor-losses
   (overwrite-home)
   (or (command-table-sorted? table)
       (cache-command-table-content table))
   (loop with names = (command-table-names table)
         with values = (command-table-values table)
         with length = (string-length command)
         for index from (lookup-command command table)
         below (vector-length names)
         for name = (svref names index)
         while (string-equal command name
                             0 0 length (min& (string-length name) length))
         do (princ name terminal-io)
         do (overwrite-terpri))
   (oustr "Done" terminal-io)
   (overwrite-done)))

(defun read-meta-x-arg (stream)
 (loop for char = (send stream :read-char)
       do (case char
           (#\bell (ed-warn "Aborted"))
           (#\altmode
            (return (values (coerce chrs 'string)
                            (char= (send stream :peek-char) #\return))))
           (#\return (return (values (coerce chrs 'string) t)))
           (t nil))
       collect char into chrs))




;Bind the keys.

(editor-bind-key #\meta-x extended-command)

(defun extended-command ()
 (let ((command))
  (with-query-line
   (cond ((or argument-supplied? (not (=& *argument* 1)))
          (format terminal-io "~d M-X " *argument*))
         (t (oustr "M-X " terminal-io)))
   (setq command (prescan #'read-meta-x-command '(nil))))
   (when (and command (car command))
    (let ((standard-output echo-area-window)
          (msgfiles echo-area-window))
     (apply (cadar command) (cdr command)))
    (setq + (cons (cadar command) (cdr command)))
    (send terminal-io :send-if-handles :set-device-mode
          :passall *editor-device-mode*)))
 nil)

(editor-bind-key #\control-meta-x instant-extended-command)

(defun instant-extended-command ()
 (let ((command))
  (with-query-line
   (cond ((or argument-supplied? (not (=& *argument* 1)))
          (format terminal-io "~d C-M-X " *argument*))
         (t (oustr "C-M-X " terminal-io)))
   (setq command (prescan #'meta-x-read '(nil))))
  (when (stringp command)
    (setq command (car (complete-command command meta-x-command-table)))
    (when (and command (car command))
      (let ((standard-output echo-area-window)
            (msgfiles echo-area-window))
        (funcall (cadr command)))
      (setq + (list (cadr command)))
      (send terminal-io :send-if-handles :set-device-mode
            :passall *editor-device-mode*))))
 nil)

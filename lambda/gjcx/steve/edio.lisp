; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Editor input-output support functions.
;


(defun editor-pathname (pathname)
  (let ((x (errset (pathname pathname) t)))
    (if x (car x)
        (ed-lose "Bad pathname"))))

(defun editor-default (pathname)
  (setq pathname (editor-pathname pathname))
  (let ((defpath (or (and *editor-buffer* (buffer-file-name *editor-buffer*))
                     (merge-pathname-defaults
                      (user-workingdir-pathname) "foo.lsp"))))
    (merge-pathname-defaults pathname defpath (send defpath :type))))

;File/buffer finding.

(defun select-buffer-named (name last-buffer-selected)
  (let ((buffer (point-selected (if (string= name "")
                                    last-buffer-selected
                                    name)
                                :create nil)))
    (when (null buffer)
      (with-query-line
        (format terminal-io "Can't find buffer named ~a.  Create it? "
                name)
        (if (ed-y-or-n-p "Type Y to create it")
            (point-selected name)
            (select-buffer))))))

;File reading.

(eval-when (compile)
  (if (and (fboundp 'frobulate-line-for-read-file)
           (pairp (fsymeval 'frobulate-line-for-read-file))
           (eq 'subst (car (fsymeval 'frobulate-line-for-read-file))))
      (fset 'frobulate-line-for-read-file #'list)))

(defun frobulate-record-for-read-file (buffer line string &aux len p)
  (setq len (simple-string-length string))
  (if (not (setq p (%string-posq #\return string 0 len)))
      (frobulate-line-for-read-file buffer line string)
      (do ((i 0)) (nil)
        (let* ((cnt (-& (or p len) i))
               (new (make-string cnt :initial-element #\space)))
          (%string-replace new string 0 i cnt)
          (setq line (frobulate-line-for-read-file buffer line new))
          (when (null p) (return line))
          (when (and (<& (setq i (1+& p)) len)
                     (char= (schar string i) #\linefeed))
            (setq i (1+& i)))
          (setq p (%string-posq #\return string i (-& len i)))))))

(defun frobulate-line-for-read-file (buffer line string)
  (compiler-let ((compiler:*open-compile-xref-switch t))
    (setf (line-chars line) string)
    (setf (line-char-count line) (simple-string-length string))
    (setf (line-bashed? line) 0)
    (let ((new-line (si:make-extend 8 (get 'line 'si:flavor))))
      (setf (line-buffer new-line) buffer)
      (setf (line-buffer-pointers new-line) nil)
      (setf (line-chars new-line) "")
      (setf (line-char-count new-line) 0)
      (setf (line-previous new-line) line)
      (setf (line-next new-line) nil)
      (setf (line-bashed? new-line) *editor-redisplay-count*)
      (setf (line-graphic-length new-line) #.*:min-fixnum)
      (setf (line-parens new-line) nil)
      (setf (line-string-bits new-line) :reserved-for-future-use)

      (setf (line-next line) new-line)
      new-line)))

;What should this do about BPs in the old buffer?
(defun read-file-into-buffer (pathname &optional (buffer *editor-buffer*)
                              &aux line string eofp)
 (when (not (0p (logand& (buffer-access buffer)
                         buffer-access-buffer-read-only)))
  (ed-warn :read-only))
 (setq line (make-line buffer nil nil))
 (setf (buffer-content buffer) line)
 (loop for point in *all-edit-cursors*
       if (eq (edit-cursor-buffer point) buffer)
       do (progn (setf (edit-cursor-home-line point) line)
                 (setf (edit-cursor-home-pos point) 0)
                 (send point :move line 0)
                 (unless (null (edit-cursor-window point))
                   (setq re-generate-screen-really t))))
 (setf (buffer-modified? buffer) nil)
 (setq pathname (pathname pathname))
 (setf (buffer-access buffer) buffer-access-any)
 (setf (buffer-environment buffer) nil)
 (setq *context-change-flag* t)
 (setf (buffer-mark-ring buffer) (make-vector mark-ring-size :initial-element nil))
 (setf (buffer-mark-ring-index buffer) 0)
 (with-notify-line
  (oustr "Reading File..." terminal-io)
  (send terminal-io :send-if-handles :set-terminal-mode :passall nil)
  (send buffer :set-file-name pathname)
  (if (send pathname :operation-handled-p :open-for-editor)
      (let ((stream nil))
        (unwind-protect
          (progn (unless (streamp (setq stream (send pathname :open-for-editor
                                                     :read)))
                   (ed-lose stream))
                 (loop while (setq string (send stream :next-record))
                       do (setq line (frobulate-record-for-read-file
                                       buffer line string))))
          (when (streamp stream) (close stream))))
      (with-open-file (inf pathname :direction :in :noerror t)
        (when (not (streamp inf)) (ed-lose inf))
        (do () (nil)
          (multiple-value (string eofp) (send inf :readline ()))
          (when (not (null eofp)) (return nil))
          (setq line (frobulate-line-for-read-file buffer line string)))))

  (or (null string)
      (frobulate-line-for-read-file buffer line string))
  (setf (buffer-modified? buffer) nil)
  (when (argument?)
    (set-file-read-only buffer))
  (send terminal-io :send-if-handles :set-terminal-mode
        :passall *editor-device-mode*)))

;File writing.
(defun output-buffer-to-file (filename buffer &optional (access-bypass nil)
                              &aux truename)
  (when (and (not (0p (logand& (buffer-access buffer)
                               buffer-access-file-read-only)))
             (null access-bypass))
    (ed-lose "File Read Only"))
  (with-notify-line
   (oustr "Writing File..." terminal-io)
   (send terminal-io :send-if-handles :set-terminal-mode :passall nil)
   (if (send (setq filename (pathname filename))
             :operation-handled-p :open-for-editor)
       (let ((stream nil))
         (unwind-protect
           (progn
             (unless (streamp (setq stream (send filename :open-for-editor
                                                 :write)))
               (ed-lose stream))
             ;Make sure there is a Newline at the end of the buffer
             (do ((line (buffer-content buffer) next) (next))
                 ((not (setq next (line-next line))) line))
             (do ((line (buffer-content buffer) next) (next) (char-count))
                 ((null line))
               (setq char-count (line-char-count line))
               (when (null (setq next (line-next line)))
                 (when (=& char-count 0) (return nil)))
               (send stream :write-record (line-chars line) char-count))
             (close stream)
             (setq truename (send stream :truename)))
           (when (and (streamp stream) (eq (send stream :status) :open))
             (send stream :close :abort))))
       (with-open-file (outf filename :direction :out :noerror t)
         (unless (streamp outf) (ed-lose outf))
         (do ((line (buffer-content buffer) (line-next line)))
             ((null line))
             (send outf :oustr (line-chars line) 0 (line-char-count line))
             (and (line-next line) (send outf :terpri)))
         (close outf)
         (setq truename (send outf :truename))))
   (send buffer :set-file-name filename)
   (setf (buffer-modified? buffer) nil))
  ;Ditty stuff here.  Compute the string for printing before entering
  ; the with-error-line-remaining construct, so that the pause doesn't
  ; show after the echo-area has been cleared.
  (setq truename (send truename :string-for-printing))
  (with-error-line-remaining (format terminal-io "Written ~a" truename))
  (send terminal-io :send-if-handles :set-terminal-mode
        :passall *editor-device-mode*))

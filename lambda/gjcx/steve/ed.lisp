; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-
;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Note: careful about the HOME-LINE being deleted.
;

;These two variables contain most of the state of the editor.
(defvar *editor-cursor* nil)
(defvar *editor-buffer* nil)
;
(defvar *.* nil) ;Copy of *editor-cursor*
(defvar *b* nil) ;Copy of *editor-buffer*
(defvar *tty-width* 80) ;These are re-set whenever the editor is entered.
(defvar *tty-height* 24) ;But might as well make them reasonable for testing.
(defvar *tty-init-flag* t)
(defvar *last-line*)
(defvar *mode-area-height* 4)
(defvar *window-height*)
(defvar *first-mode-line*)
(defvar *last-mode-line* (1-& *mode-area-height*))
(defvar *mode-area*)
(defvar *argument*) ;#\control-u argument here.
(defvar argument-supplied?)
(defvar *last-buffer-selected* nil) ;Used by #\control-x #\b
(defvar *error-state* nil) ;Set when an error occurs. Makes editor careful.
(defvar *all-edit-cursors* nil)
(defvar *all-buffers* nil)
(defvar *old-screen-image)
(defvar *recentering-fraction* 3/8)
(defvar *query-line* 13)
(defvar *notify-line* 12)
(defvar *feedback-line* 14)
(defvar *double-line*) ;Guaranteed to have a line after it.
(defvar *error-line* 15)
(defvar *prefix-echo-line*)
(defvar *first-echo-line*)
(defvar *last-overwrite-line* 10)
(defvar *more-line* 11) ;Initialized so SAVE-ALL-FILES works outside editor.
(defvar *last-buffer-line*)
(defvar *current-buffer-modified* nil)
(defvar *context-change-flag*)
(defvar *editor-current-key*)
(defvar *editor-last-command*)
(defvar *last-home-line*)
(defvar re-generate-screen-really)
(defvar re-generate-all-windows-really)
(defvar *editor-command-count* 0)
(defvar *editor-device-mode*)
(defvar *editor-running*)
(defvar *major-mode*)
(defvar *last-major-mode* nil)
(defvar *minor-modes* nil)

(defvar *buffer-name-completion-on-return*)
(defconstant buffer-access-any 0)
(defconstant buffer-access-file-read-only 1)
(defconstant buffer-access-buffer-read-only 2)

;
;The top level stuff.
;

(defun error-ed ()
  (oustr ";Attempting to save from editor error" terminal-io)
  (send *editor-cursor* :move (buffer-content *editor-buffer*) 0)
  (when (null (edit-cursor-window *editor-cursor*))
        (setf (edit-cursor-window  *editor-cursor*)
              (create-window 0 0 80 19)))
  (ed))

;;;Do not try to display the buffer inside this function. It will be done
;;;in the wrong environment.

(defun editor-initialize (spec)
  (when (null *editor-cursor*)
    (initial-editor-state))
  (cond ((null spec))
        ((symbolp spec)
           (find-function spec))
        ((and (listp spec)                 ;This is a hack for teach-steve
              (eq (first spec) :load-and-rename))
           (let ((point (point-selected (pathname (second spec)) :create t)))
             (send (bp-buffer point) :set-file-name (pathname (third spec)))))
        ((stringp spec)
           (point-selected (pathname spec) :create t))
        ((point-selected spec :create t))
        (t (format t "%&Invalid argument"))))

(defun initial-editor-state ()
  (let* ((curse (make-point "Main"
                            (create-window 0 0 *tty-width* *window-height*)))
         (buff (edit-cursor-buffer curse)))
    (setq *editor-buffer* buff
          *b* buff
          *last-buffer-selected* buff
          *editor-cursor* curse
          *.* curse)))

(defparameter *steve-tutorial-file* "nil$disk:[nil.steve]teach.txt")

(defun teach-steve ()
  (cond ((null (probe-file *steve-tutorial-file*))
         (format t "%&Sorry, the STEVE tutorial file is nowhere to be found.")
         nil)
        (t (ed (list :load-and-rename
                     *steve-tutorial-file*
                     (fs:merge-pathnames "teach.txt"
                                         (user-homedir-pathname)))))))

;;; Synonyms

(defun teachemacs ()
  (teach-steve))

(defun teach ()
  (teach-steve))

;;; The usual way to invoke STEVE

(defun ed (&optional (spec nil))
  (when (or (null *editor-cursor*)
            (not (=& *tty-width* (send terminal-io :linel)))
            (not (=& *tty-height* (send terminal-io :pagel))))
    (setq *tty-init-flag* t)
    (setq *tty-height* (send terminal-io :pagel))
    (setq *tty-width* (send terminal-io :linel)))
  (let* ((*editor-running* t)
         (re-generate-screen-really t)
         (re-generate-all-windows-really t)
         (*mode-area-height* (/& *tty-height* 6))
         (*last-line* (1-& *tty-height*))
         (*last-overwrite-line* (-& *tty-height* *mode-area-height* 1))
         (*more-line* *last-line*)
         (*last-buffer-line* (1+& (-& *tty-height* *mode-area-height* 2)))
         (*first-mode-line* (-& *tty-height* *mode-area-height*))
         (*last-mode-line* (1-& *mode-area-height*))
         (*window-height* (-& *tty-height* *mode-area-height*))
         (*first-echo-line* (+& 1 *first-mode-line*))
         (*query-line* (+& 0 *first-echo-line*))
         (*notify-line* (+& 1 *first-echo-line*))
         (*feedback-line* (+& 1 *first-echo-line*))
         (*double-line* (+& 1 *first-echo-line*))
         (*error-line* (+& 2 *first-echo-line*))
         (*prefix-echo-line* (+& 2 *first-echo-line*))
         (*editor-current-key* nil)
         (*editor-last-command* nil)
         (*context-change-flag* nil)
         (*argument* 1)
         (argument-supplied? nil)
         (*last-home-line* nil)
         ;;Make sure echo window is set up. Use it for Standard-output.
         (echo-area-window (setup-echo-area-window))
         (standard-output echo-area-window)
         (error-output echo-area-window)
         (trace-output echo-area-window)
         (si:print-gc-messages nil))
    (when (not (null *tty-init-flag*))
      (init-tty-for-editor)
      (setq *tty-init-flag* nil))
    (when (not (boundp '*editor-cursor*)) (setq *editor-cursor* nil))
    (when (not (boundp '*editor-buffer*)) (setq *editor-buffer* nil))
    (editor-guts spec)
    '*))

(deff steve
  #'ed)

(defvar *terminal-passall-on-exit*)
(defvar *terminal-ttsync-on-exit*)

(defun editor-guts (&optional spec)
  (let ((*terminal-passall-on-exit*
         (send terminal-io :get-device-mode :passall))
        ;;We want to restore these on exit.
        (*terminal-ttsync-on-exit*
         (send terminal-io :get-device-mode :ttysync)))
    (unwind-protect
     (catch 'exit-editor
       ;We are trying to prevent the user from typing control-s as
       ;the editor turns on passall mode, (thus getting screwed completely.)
       ;If control-s is typed as the editor is being entered, it should
       ;cause this WRITE-CHAR to hang, until a control-q is typed.
       ;It is possible to type control-q between then and the time ttsync is
       ;turned off. In that case output will hang on the :CLEAR-SCREEN below.
       ;When this happens the terminal is not in passall mode, but
       ;ttsync is off. One may still exit with control-c or control-y.
       (send terminal-io :write-char #\space)
       (send terminal-io :set-device-mode :ttysync nil)
       (send terminal-io :clear-screen) ;See note above.
       (send terminal-io :set-device-mode :passall *editor-device-mode*)
       (clear-one-screen-image old-screen-image)
       (unless (null *terminal-passall-on-exit*)
         (with-notify-line-remaining (oustr "PASSALL mode on" terminal-io)))
       (catching-editor-losses
         (editor-initialize spec))
       (loop do (when (null (buffer-environment *editor-buffer*))
                  (initialize-buffer-environment *editor-buffer*))
             do (process-in-saved-buffer-environment
                 (buffer-environment *editor-buffer*)
                 #'editor-top-level (vector))))
     ;;Restore TTY device modes.
     (unless (null *terminal-passall-on-exit*)
       (send terminal-io :set-cursorpos 0 0)
       (send terminal-io :clear-eol)
       (oustr "Passall mode left on" terminal-io))
     (send terminal-io :set-device-mode :passall
           *terminal-passall-on-exit*)
     (send terminal-io :set-device-mode :ttysync
           *terminal-ttsync-on-exit*)
     ;;Patch mode line.
     (editor-destroy-mode-line-for-exiting)
     )))

(defun editor-destroy-mode-line-for-exiting ()
  (cursorpos *first-mode-line* 0 terminal-io)
  (oustr "     " terminal-io)
  (cursorpos (1+& *first-mode-line*) 0 terminal-io)
  (cursorpos 'end-of-screen terminal-io))

(defun editor-top-level (&optional gunk-pathname)
  (setq *context-change-flag* nil)
  (setup-mode-area)
  (loop while (null *context-change-flag*)
        do (setq executing-keyboard-macro nil)
        do (setq save-chars-for-keyboard-macros nil)
        ;;do (setq keyboard-macro-char-list nil)
        do (when (memq '|Def| *minor-modes*)
             (setq *minor-modes* (delq '|Def| *minor-modes*))
             (setup-mode-area))
        do (catch :editor-loss
            (progn (catch 'si:errset-catch (editor-top-level-loop))
                   (unless *context-change-flag*
                     (setq *last-major-mode* "No mode")
                     (setq creamed-tty-lines-to
                           (max& creamed-tty-lines-to
                                 (1+& (car (cursorpos terminal-io)))))
                     (ed-lose "Quit to Editor"))))))

(defun editor-top-level-loop ()
  (send terminal-io :set-device-mode :passall *editor-device-mode*)
  (loop while (null *context-change-flag*)
        do (ed-command-interpret)))

(defmacro self-insert? (chr)
  `(and (graphic-charp ,chr)
        (case (lookup-key-in-table ,chr *editor-bindings*)
          ('self-insert t)
          ((nil) (lookup-key-in-table (char-upcase ,chr) *editor-bindings*)))))

(defun ed-command-interpret (&aux key cmnd-fun)
  (setq *argument* 1
        auto-digit-arg-save nil
        argument-supplied? nil)
  (when (or (neq *current-buffer-modified* (buffer-modified? *editor-buffer*))
            (neq *last-major-mode* *major-mode*))
    (setup-mode-area))
  (make-screen-image)
  (when (and (null (type-ahead-p))
             (or (send *editor-cursor* :end-of-line?) *overwrite-mode*)
             (buffer-modified? *editor-buffer*)
             (0p (logand& (buffer-access *editor-buffer*)
                          buffer-access-buffer-read-only))
             (null save-chars-for-keyboard-macros))
    (let ((string (editor-echoin (send (edit-cursor-window *editor-cursor*)
                                       :make-cursor-visible)
                                 (send *editor-bindings* :get-echoin-bit-vector)))
          (length) (x) (y))
      (unless (null string)
        (setq *editor-command-count* (1+& *editor-command-count*)
              *editor-last-command* 'echoin-string)
        (setq length (length (setq string (coerce string 'string))))
        (multiple-value (x y) (edit-cursor-position-really *editor-cursor*))
        (send *editor-cursor* :insert-string string)
        (%string-replace (svref old-screen-image
                                 (1-& (send terminal-io :linenum)))
                         string x 0 length)))
    (make-screen-image))
  (or (type-ahead-p)
      (send (edit-cursor-window *editor-cursor*) :make-cursor-visible))
  (multiple-value (key cmnd-fun) (read-key))
  (if (null cmnd-fun) (ed-warn :undefined-key))
  (setq *editor-current-key* key)
  (setq *editor-command-count* (1+& *editor-command-count*))
  (when (not (null (funcall cmnd-fun))) ;Actually execute a command.
    (ed-warn :redisplay-advice-returned))
  (setq *editor-last-command* cmnd-fun))

(defun funcall-key (key)
  (let ((command (internal-editor-lookup-key key)))
    (when (consp command) (ed-lose "Can't funcall-key macro key-binding"))
    (funcall command)))

(defun setup-mode-area ()
  (let ((mode-line (with-output-to-string (mode-line)
                     (if (null recursive-edit)
                         (oustr "STEVE " mode-line)
                       (format mode-line "[STEVE ~a] " recursive-edit))
                     (format mode-line "~a ~a "
                             (buffer-name *editor-buffer*)
                             (cons *major-mode* *minor-modes*))
                     (let ((filename (buffer-file-name *editor-buffer*))
                           (version))
                       (cond ((null filename)
                              (oustr "--No File--" mode-line))
                             (t (oustr (send filename :string-for-editor)
                                       mode-line)
                                (when (and (setq version (buffer-truename
                                                           *editor-buffer*))
                                           (setq version (send version
                                                               :version)))
                                  (format mode-line " (~d)" version)))))
                     (if (buffer-modified? *editor-buffer*)
                         (oustr " *" mode-line)
                       (oustr "  " mode-line)))))
    (setq *current-buffer-modified* (buffer-modified? *editor-buffer*))
    (setq *last-major-mode* *major-mode*)
    (send terminal-io :send-if-handles :standout t)
    (cursorpos *first-mode-line* 0 terminal-io)
    (let ((width (send terminal-io :width)))
      (send terminal-io :oustr mode-line 0 (min width
                                                (string-length mode-line)))
      (send terminal-io :oustr 80spaces 0 (-& width (send terminal-io :hpos)))
      (send terminal-io :clear-eol)
      (send terminal-io :send-if-handles :standout nil))))


;;; The beginnings of competent error handling...

(defmacro def-ed-error (symbol message &optional feep?)
  `(progn (defprop ,symbol ,message error-message)
          (defprop ,symbol ,feep? error-beep?)))

(def-ed-error :move-past-start
  "Attempt to move past beginning of buffer" :beep)

(def-ed-error :move-past-end
  "Attempt to move past end of buffer" :beep)

(def-ed-error :at-start
  "At beginning of buffer")

(def-ed-error :at-end
  "At end of buffer")

(def-ed-error :at-first-line
  "At first line")

(def-ed-error :at-last-line
  "At last line")

(def-ed-error :no-mark
  "No mark" :beep)

(def-ed-error :read-only
  "Buffer is read only" :beep)

(def-ed-error :undefined-key
  "Undefined key" :beep)

(def-ed-error :unbalanced-parens
  "Unbalanced parentheses" :beep)

(def-ed-error :aborted
  "Aborted" :beep)

(def-ed-error :redisplay-adviced-returned
  "Redisplay advice returned")

;;;;;;;;;;

(defmacro catching-editor-losses (&body forms)
  `(catch :editor-loss
     (progn ,@forms)))

(defun ed-warn (error &REST args &aux message)
  (with-error-line-remaining
    (cond ((and (symbolp error)
                (setq message (get error 'error-message)))
             (when (get error 'error-beep?)
               (steve-beep))
             (format terminal-io message))
          (:else
             (steve-beep)
             (lexpr-funcall #'format terminal-io error args))))
  (setq *error-state* t)
  (throw :editor-loss t))

(defun ed-abort (&optional echop (stream standard-output))
  (when echop
    (oustr "^G" stream))
  (steve-beep)
  (throw :editor-loss t))

(defun ed-lose (format-string &restv args)
  ;Temporarily reset passall mode in case of multiple errors.
  (send terminal-io :set-device-mode :passall nil)
  (with-error-line-remaining
   (format terminal-io "Error: ")
   (lexpr-funcall #'format terminal-io format-string args)
   (clear-input terminal-io)
   (setq pushback-flag nil)
   (setq *error-state* t) ;Don't count upon this doing anything.
   (throw :editor-loss t)))

;;; I think this is obsolete

(defun ed-warning (format-string &REST args)
  (with-error-line-remaining
   (lexpr-funcall #'format terminal-io format-string args)))

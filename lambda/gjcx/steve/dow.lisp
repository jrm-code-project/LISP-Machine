; -*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;
;Window code. YET again.
;

(defflavor window-stream (x-pos y-pos x-size y-size x-now y-now fresh?)
 (output-stream)
 :initable-instance-variables
 :outside-accessible-instance-variables
 ;; :gettable-instance-variables
 :ordered-instance-variables)

;Eliminate these.
(defun window-x-pos (x) (window-stream-x-pos x))
(defun window-x-size (x) (window-stream-x-size x))
(defun window-y-pos (x) (window-stream-y-pos x))
(defun window-y-size (x) (window-stream-y-size x))

;;This function replaces one in WIN.LSP
;;Eventually WIN.LSP and its WINDOWs will go away, and these will
;;be used as the only type of editor window.
(defun create-window (x-pos y-pos x-size y-size
                            ;; &optional (edge-init edge-none)
                            )
  (make-instance 'window-stream
                 :x-pos x-pos :y-pos y-pos
                 :x-size x-size :y-size y-size
                 ;; :x-max (+& x-pos x-size -1)
                 ;; :y-max (+& y-pos y-size -1)
                 :x-now x-pos :y-now y-pos
                 ;; :edges edge-init
                 ))

(defmethod (window-stream :make-cursor-visible) ()
  (cursorpos (+& y-pos y-now) (+& x-pos x-now) terminal-io)
  ;;Return chars left on line for ECHOin hack.
  (-& x-size x-now 1))

(defmethod (window-stream :write-char) (char)
 (cond ((graphic-charp char)
        (when (>=& x-now (1-& x-size))
         (send self :continuation-line y-now)
         (send self :fresh-line))
        (cursorpos (+& y-pos y-now) (+& x-pos x-now) terminal-io)
        (setq x-now (1+& x-now))
        (setq fresh? nil)
        (send terminal-io :write-char char))
       ((char= char #\return)
        (send self :fresh-line))
       ((char= char #\line))
       ((char= char #\bell) (send terminal-io :write-char #\bell))
       (t (send self :oustr (stringify-char char x-now)))))

(defmethod (window-stream :continuation-line) (line)
  (cursorpos (+& y-pos line) (+& x-pos x-size -1) terminal-io)
  (send terminal-io :write-raw-char #\!))

(defmethod (window-stream :formfeed) ()
 (loop for i from 1 to y-size
       do (send self :terpri))
 (cursorpos 0 0 self)
 (setq fresh? t))

(defmethod (window-stream :oustr) (string &optional
                                   (start 0)
                                   (count (-& (string-length string) start)))
 (compiler-let ((compiler:*open-compile-char-switch t))
  (loop for i from start below (+& start count)
        do (send self :write-char (char string i)))))

(defmethod (window-stream :fresh-line) ()
 (unless fresh?
  (cond ((<& y-now (1-& y-size)) (setq x-now 0 y-now (1+& y-now)))
        (t (setq x-now 0 y-now 0)))
  (cursorpos (+& y-now y-pos) x-pos terminal-io)
  (unless fresh?
   (setq fresh? t)
   (if (=& (+& x-pos x-size) *tty-width*)
       (send terminal-io :clear-eol)
       (send terminal-io :oustr 80spaces 0 x-size)
       (cursorpos (+& y-now y-pos) x-pos terminal-io)))))

(defmethod (window-stream :terpri) ()
 (cond ((<& y-now (1-& y-size)) (setq x-now 0 y-now (1+& y-now)))
       (t (setq x-now 0 y-now 0)))
 (cursorpos (+& y-now y-pos) x-pos terminal-io)
 (setq fresh? t)
 (if (=& (+& x-pos x-size) *tty-width*)
     (send terminal-io :clear-eol)
     (send terminal-io :oustr 80spaces 0 x-size)
     (cursorpos (+& y-now y-pos) x-pos terminal-io)))

(defmethod (window-stream :clear-eol) ()
 (cursorpos (+& y-now y-pos) (+& x-pos x-now) terminal-io)
 (when (0p x-now) (setq fresh? t))
 (if (=& (+& x-pos x-size) *tty-width*)
     (send terminal-io :clear-eol)
     (send terminal-io :oustr 80spaces 0 (-& x-size x-now))
     (cursorpos (+& y-now y-pos) x-pos terminal-io)))

(defmethod (window-stream :beep) ()
   (send terminal-io :write-char #\bell))

(defmethod (window-stream :rubout) ()
 (cond ((+p x-now)
        (cursorpos (+& y-pos y-now) (+& x-pos x-now) terminal-io)
        (setq x-now (1-& x-now))
        (send terminal-io :rubout))
       ((+p y-now)
        (setq y-now (1-& y-now) x-now (1-& x-size)))
       (t (setq y-now (1-& y-size) x-now (1-& x-size)))))

(defmethod (window-stream :read-cursorpos) ()
  (values y-now x-now))

(defmethod (window-stream :chrpos) ()
  x-now)

(defmethod (window-stream :set-cursorpos) (y* x*)
 (when (not (and x* y*))
  (setq x* (or x* x-now) y* (or y* y-now)))
 (setq x-now x* y-now y*)
 (setq fresh? nil))

(defmethod (window-stream :cursor-up) ()
 (setq fresh? nil)
 (setq y-now (1-& (if (0p y-now) y-size y-now))))

(defmethod (window-stream :cursor-down) ()
 (setq fresh? nil)
 (setq y-now (1+& y-now))
 (when (>=& y-now y-size) (setq y-now 0)))

(defmethod (window-stream :linel) ()
 x-size)

(defmethod (window-stream :width) ()
 (1-& x-size))

(defmethod (window-stream :pagel) ()
 y-size)

(defmethod (window-stream :height) ()
 y-size)

(defmethod (window-stream :size-in-characters) ()
 (values (1-& x-size) y-size))

(defmethod (window-stream :filemode) ()
 (list (list (type-of self)) ':cursorpos))

(defmethod (window-stream :split-window) ()
  (when (<=& y-size 3) (ed-lose "Window too small"))
  (let ((y-half (/& (1+& y-size) 2))
        (new-win))
    (setq y-size (-& y-size y-half)
          )
          ;;y-max (-& y-max y-half))
    (setq new-win
          (create-window x-pos (+& y-pos y-size 1) x-size (1-& y-half)))
    new-win))


;
;Minibuffer.
;

(defvar minibuffer-window)
(defvar echo-area-window)

(defun setup-echo-area-window ()
  (when (or (not (boundp 'echo-area-window))
            (not (=& (window-stream-y-size echo-area-window)
                     *mode-area-height*))
            (not (=& (window-stream-y-pos echo-area-window)
                     *first-mode-line*)))
    (setq echo-area-window
          (make-instance 'window-stream
                         :x-pos 0 :y-pos (1+& *first-mode-line*)
                         :fresh? nil :x-size *tty-width*
                         :y-size (1-& *mode-area-height*) :x-now 0 :y-now 0)))
  echo-area-window)

(defun setup-minibuffer-display ()
 (unless (boundp 'minibuffer-window)
  (setq minibuffer-window
        (make-instance 'window-stream
                       :x-pos 0 :y-pos 0 :fresh? nil
                       :x-size *tty-width* :y-size 7 :x-now 0 :y-now 0)))
 (setq creamed-tty-lines-to (1+& (window-stream-y-size minibuffer-window)))
 (cursorpos (1-& creamed-tty-lines-to) 0 terminal-io)
 (oustr "------------------------------------------------------------------------------" terminal-io)
 (send minibuffer-window :formfeed))

(editor-bind-key #\meta-altmode minibuffer)

(defun minibuffer ()
 (send terminal-io :send-if-handles :set-device-mode :passall nil)
 (with-notify-line
  (oustr "Minibuffer. Type (RETURN) to exit" terminal-io)
  (setup-minibuffer-display)
  (let ((standard-output minibuffer-window)
        (error-output minibuffer-window))
   (loop for - = (si:ttyscanner #'read terminal-io minibuffer-window () () ())
         until (cond ((consp -) (eq (car -) 'return))
                     ((symbolp -) (or (string-equal - '|P|)
                                   (string-equal - '||))))
         do (errset (mapcar #'print (multiple-value-list (setq * (eval -)))))
         do (setq + -))))
 (send terminal-io :send-if-handles :set-device-mode
       :passall *editor-device-mode*)
 nil)

;
;'(defvar minibuffer
;       (make-point "Minibuffer"
;                   (make-instance 'window
;                     :x-pos 0 :y-pos 0
;                     ;;:fresh? nil
;                     :x-size *tty-width*
;                     :y-size 7 :x-now 0 :y-now 0)))
;
;(editor-bind-key #\meta-altmode new-minibuffer)
;
;(defun new-minibuffer (&optional (init nil))
;  (when init
;    (send minibuffer :insert-string init))
;  (let ((point *editor-cursor*))
;    (select-point minibuffer)
;    (cursorpos 7 0 terminal-io)
;    (oustr "------------------------------------------------------------------------------" terminal-io)
;    (let ((*all-edit-cursors* (list minibuffer))
;         (*all-buffers* (list (edit-cursor-buffer minibuffer)))
;         (*lisp-listener* t)
;         (*editor-device-mode* nil))
;      (recursive-editor))
;    (setq creamed-tty-lines-to 8)
;    (select-point point))
;  nil)
;
